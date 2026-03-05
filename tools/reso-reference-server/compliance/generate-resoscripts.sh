#!/bin/sh
# ---------------------------------------------------------------------------
# generate-resoscripts.sh — Generate per-resource RESOScript config files
#
# Queries the running reference server's $metadata and sample records to
# populate field-type parameters required by web-api-commander's
# testWebApiCore task.
#
# The commander requires every REQUIRED parameter to be present and non-empty.
# If a resource cannot fill every required data type, it is skipped with a
# diagnostic message listing the missing types.
#
# Usage: generate-resoscripts.sh <server_url> <auth_token> <output_dir>
# ---------------------------------------------------------------------------
set -e

SERVER_URL="${1:?Usage: generate-resoscripts.sh <server_url> <auth_token> <output_dir>}"
AUTH_TOKEN="${2:?Missing auth_token}"
OUTPUT_DIR="${3:?Missing output_dir}"

mkdir -p "$OUTPUT_DIR"

# Resources and their key fields
RESOURCES="Property:ListingKey Member:MemberKey Office:OfficeKey Media:MediaKey OpenHouse:OpenHouseKey Showing:ShowingKey PropertyGreenVerification:GreenBuildingVerificationKey PropertyPowerProduction:PowerProductionKey PropertyRooms:RoomKey PropertyUnitTypes:UnitTypeKey"

# Fetch $metadata XML once
METADATA=$(wget -qO- --header="Authorization: Bearer $AUTH_TOKEN" "$SERVER_URL/\$metadata" 2>/dev/null || echo "")

if [ -z "$METADATA" ]; then
  echo "ERROR: Could not fetch \$metadata from $SERVER_URL"
  exit 1
fi

# Track results
GENERATED_COUNT=0
SKIPPED_COUNT=0

# ---------------------------------------------------------------------------
# Helper: extract field names of a given Edm type from metadata XML for a
# specific EntityType, using basic text processing (no xmllint dependency).
# ---------------------------------------------------------------------------
extract_fields_by_type() {
  ENTITY_TYPE="$1"
  EDM_TYPE="$2"
  echo "$METADATA" | sed -n "/<EntityType Name=\"${ENTITY_TYPE}\"/,/<\/EntityType>/p" \
    | grep "Property " \
    | grep "Type=\"${EDM_TYPE}\"" \
    | sed 's/.*Name="\([^"]*\)".*/\1/'
}

# ---------------------------------------------------------------------------
# Helper: extract all non-collection enum fields (Type="Edm.String" with
# LookupName annotation) from metadata for a specific EntityType.
# ---------------------------------------------------------------------------
extract_single_lookup_fields() {
  ENTITY_TYPE="$1"
  echo "$METADATA" | sed -n "/<EntityType Name=\"${ENTITY_TYPE}\"/,/<\/EntityType>/p" \
    | sed -n '/<Property .*Type="Edm\.String"/{
      /Collection/!{
        N
        /LookupName/{ s/.*Name="\([^"]*\)".*/\1/; p; }
      }
    }'
}

# ---------------------------------------------------------------------------
# Helper: extract collection enum fields (Type="Collection(Edm.String)")
# from metadata for a specific EntityType.
# ---------------------------------------------------------------------------
extract_multi_lookup_fields() {
  ENTITY_TYPE="$1"
  echo "$METADATA" | sed -n "/<EntityType Name=\"${ENTITY_TYPE}\"/,/<\/EntityType>/p" \
    | grep 'Type="Collection(Edm\.String)"' \
    | sed 's/.*Name="\([^"]*\)".*/\1/'
}

# ---------------------------------------------------------------------------
# Helper: escape XML special characters in values.
# ---------------------------------------------------------------------------
xml_escape() {
  echo "$1" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g'
}

# ---------------------------------------------------------------------------
# For each resource: fetch sample records, resolve field types, validate,
# and either write RESOScript XML or skip with diagnostic.
# ---------------------------------------------------------------------------
for entry in $RESOURCES; do
  RESOURCE=$(echo "$entry" | cut -d: -f1)
  KEY_FIELD=$(echo "$entry" | cut -d: -f2)

  echo "--- Generating RESOScript for $RESOURCE (key=$KEY_FIELD) ---"

  # Fetch sample records (multiple to increase chance of finding populated fields)
  SAMPLE=$(wget -qO- --header="Authorization: Bearer $AUTH_TOKEN" \
    --header="Accept: application/json" \
    "$SERVER_URL/$RESOURCE?\$top=10" 2>/dev/null || echo "{}")

  RECORD_COUNT=$(echo "$SAMPLE" | jq -r '.value | length' 2>/dev/null || echo "0")

  if [ "$RECORD_COUNT" = "0" ] || [ -z "$RECORD_COUNT" ]; then
    echo "  WARNING: No sample records for $RESOURCE — skipping"
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    continue
  fi

  # Key value (from first record)
  KEY_VALUE=$(echo "$SAMPLE" | jq -r --arg k "$KEY_FIELD" '.value[0][$k] // empty')
  if [ -z "$KEY_VALUE" ]; then
    echo "  WARNING: No key value for $KEY_FIELD in $RESOURCE — skipping"
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    continue
  fi

  # --- Find first field with data for each required Edm type ---

  # REQUIRED: Integer fields (Edm.Int16, Edm.Int32, Edm.Int64)
  # Pick a field with 3+ distinct values and use the median as IntegerValueLow
  # so that filter-int-lt (lt IntegerValueLow) finds records below it.
  INTEGER_FIELD=""
  INTEGER_VALUE=""
  for type in "Edm.Int64" "Edm.Int32" "Edm.Int16"; do
    FIELDS=$(extract_fields_by_type "$RESOURCE" "$type")
    for f in $FIELDS; do
      # Collect all non-null values for this field across sample records
      int_vals=$(echo "$SAMPLE" | jq -r --arg f "$f" \
        '[.value[][$f] | select(. != null)] | sort | unique | .[]' 2>/dev/null)
      distinct_count=$(echo "$int_vals" | grep -c .)
      if [ "$distinct_count" -ge 3 ]; then
        INTEGER_FIELD="$f"
        # Pick the median (middle value) so records exist both above and below
        median_idx=$(( distinct_count / 2 ))
        INTEGER_VALUE=$(echo "$int_vals" | sed -n "$((median_idx + 1))p")
        break 2
      elif [ "$distinct_count" -ge 1 ] && [ -z "$INTEGER_FIELD" ]; then
        # Fallback: use any field with data (may fail filter-int-lt)
        INTEGER_FIELD="$f"
        INTEGER_VALUE=$(echo "$int_vals" | head -1)
      fi
    done
  done

  # REQUIRED: Decimal fields (Edm.Decimal)
  DECIMAL_FIELD=""
  DECIMAL_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.Decimal")
  for f in $FIELDS; do
    i=0
    while [ "$i" -lt "$RECORD_COUNT" ]; do
      val=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f] // empty')
      if [ -n "$val" ] && [ "$val" != "null" ]; then
        DECIMAL_FIELD="$f"
        DECIMAL_VALUE="$val"
        break 2
      fi
      i=$((i + 1))
    done
  done

  # REQUIRED: Date fields (Edm.Date)
  DATE_FIELD=""
  DATE_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.Date")
  for f in $FIELDS; do
    i=0
    while [ "$i" -lt "$RECORD_COUNT" ]; do
      val=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f] // empty')
      if [ -n "$val" ] && [ "$val" != "null" ]; then
        DATE_FIELD="$f"
        # Edm.Date values MUST be ISO 8601 date-only (YYYY-MM-DD).
        # Server may return timestamps; truncate to date portion.
        DATE_VALUE=$(echo "$val" | cut -dT -f1)
        break 2
      fi
      i=$((i + 1))
    done
  done

  # REQUIRED: Timestamp fields (Edm.DateTimeOffset)
  # Prefer a field that has data in ALL sampled records (no nulls → no orderby issues).
  # Fall back to ModificationTimestamp which RESO requires to always be populated.
  TIMESTAMP_FIELD=""
  TIMESTAMP_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.DateTimeOffset")
  for f in $FIELDS; do
    all_populated=true
    first_val=""
    i=0
    while [ "$i" -lt "$RECORD_COUNT" ]; do
      val=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f] // empty')
      if [ -z "$val" ] || [ "$val" = "null" ]; then
        all_populated=false
        break
      fi
      [ -z "$first_val" ] && first_val="$val"
      i=$((i + 1))
    done
    if [ "$all_populated" = "true" ] && [ -n "$first_val" ]; then
      TIMESTAMP_FIELD="$f"
      TIMESTAMP_VALUE="$first_val"
      break
    fi
  done
  # Fallback: ModificationTimestamp is always populated per RESO rules
  if [ -z "$TIMESTAMP_FIELD" ]; then
    TIMESTAMP_FIELD="ModificationTimestamp"
    TIMESTAMP_VALUE=$(echo "$SAMPLE" | jq -r '.value[0].ModificationTimestamp // empty')
  fi

  # REQUIRED: Single-value lookup field
  SINGLE_LOOKUP_FIELD=""
  SINGLE_LOOKUP_VALUE=""
  LOOKUP_FIELDS=$(extract_single_lookup_fields "$RESOURCE")
  for f in $LOOKUP_FIELDS; do
    i=0
    while [ "$i" -lt "$RECORD_COUNT" ]; do
      val=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f] // empty')
      if [ -n "$val" ] && [ "$val" != "null" ]; then
        SINGLE_LOOKUP_FIELD="$f"
        SINGLE_LOOKUP_VALUE="$val"
        break 2
      fi
      i=$((i + 1))
    done
  done

  # REQUIRED: Multi-value lookup field — need at least 2 distinct values
  MULTI_LOOKUP_FIELD=""
  MULTI_LOOKUP_VALUE1=""
  MULTI_LOOKUP_VALUE2=""
  MULTI_FIELDS=$(extract_multi_lookup_fields "$RESOURCE")
  for f in $MULTI_FIELDS; do
    i=0
    while [ "$i" -lt "$RECORD_COUNT" ]; do
      arr_len=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" \
        '.value[$i][$f] | if type == "array" then length else 0 end' 2>/dev/null || echo "0")
      if [ "$arr_len" -ge 2 ]; then
        MULTI_LOOKUP_FIELD="$f"
        MULTI_LOOKUP_VALUE1=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f][0]')
        MULTI_LOOKUP_VALUE2=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f][1]')
        break 2
      elif [ "$arr_len" -eq 1 ] && [ -z "$MULTI_LOOKUP_VALUE1" ]; then
        # Save single value; keep looking for a record with 2+
        MULTI_LOOKUP_FIELD="$f"
        MULTI_LOOKUP_VALUE1=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" '.value[$i][$f][0]')
      fi
      i=$((i + 1))
    done
    # If we found a field with one value, try to find a second across other records
    if [ -n "$MULTI_LOOKUP_VALUE1" ] && [ -z "$MULTI_LOOKUP_VALUE2" ]; then
      i=0
      while [ "$i" -lt "$RECORD_COUNT" ]; do
        val=$(echo "$SAMPLE" | jq -r --arg f "$f" --argjson i "$i" --arg v1 "$MULTI_LOOKUP_VALUE1" \
          '.value[$i][$f] // [] | map(select(. != $v1)) | .[0] // empty')
        if [ -n "$val" ] && [ "$val" != "null" ]; then
          MULTI_LOOKUP_VALUE2="$val"
          break
        fi
        i=$((i + 1))
      done
    fi
    [ -n "$MULTI_LOOKUP_VALUE2" ] && break
  done

  # --- Diagnostics ---
  echo "  Key: $KEY_FIELD=$KEY_VALUE"
  echo "  Integer: ${INTEGER_FIELD:-(none)}=${INTEGER_VALUE:-(none)}"
  echo "  Decimal: ${DECIMAL_FIELD:-(none)}=${DECIMAL_VALUE:-(none)}"
  echo "  Date: ${DATE_FIELD:-(none)}=${DATE_VALUE:-(none)}"
  echo "  Timestamp: ${TIMESTAMP_FIELD:-(none)}=${TIMESTAMP_VALUE:-(none)}"
  echo "  SingleLookup: ${SINGLE_LOOKUP_FIELD:-(none)}=${SINGLE_LOOKUP_VALUE:-(none)}"
  echo "  MultiLookup: ${MULTI_LOOKUP_FIELD:-(none)}=${MULTI_LOOKUP_VALUE1:-(none)}, ${MULTI_LOOKUP_VALUE2:-(none)}"

  # --- Validate REQUIRED data types are filled ---
  MISSING=""
  [ -z "$INTEGER_FIELD" ]        && MISSING="${MISSING}  - Integer (Edm.Int16/Int32/Int64)\n"
  [ -z "$DECIMAL_FIELD" ]        && MISSING="${MISSING}  - Decimal (Edm.Decimal)\n"
  [ -z "$DATE_FIELD" ]           && MISSING="${MISSING}  - Date (Edm.Date)\n"
  [ -z "$TIMESTAMP_FIELD" ]      && MISSING="${MISSING}  - Timestamp (Edm.DateTimeOffset)\n"
  [ -z "$SINGLE_LOOKUP_FIELD" ]  && MISSING="${MISSING}  - SingleValueLookup (Edm.String with LookupName)\n"
  [ -z "$MULTI_LOOKUP_FIELD" ] || [ -z "$MULTI_LOOKUP_VALUE2" ] && \
    MISSING="${MISSING}  - MultipleValueLookup (Collection(Edm.String) with 2+ values)\n"

  if [ -n "$MISSING" ]; then
    echo ""
    echo "  Can't find all required data types for Web API Core testing for the $RESOURCE Resource. Skipping its Web API Core tests."
    printf "  Missing types:\n$MISSING"
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    continue
  fi

  # --- Write RESOScript XML ---
  # Mirrors the official sample-web-api-server.core.resoscript structure.
  # REQUIRED params are filled from live server data; non-REQUIRED params
  # use sensible defaults. Computed values use *Parameter_X* references
  # for the commander's recursive parameter resolution.
  cat > "$OUTPUT_DIR/$RESOURCE.resoscript" <<RESOSCRIPT_EOF
<?xml version="1.0" encoding="utf-8" ?>
<OutputScript>
  <RESOScriptVersion>3.1.0</RESOScriptVersion>
  <ClientSettings>
    <WebAPIURI>${SERVER_URL}</WebAPIURI>
    <AuthenticationType>authorization_code</AuthenticationType>
    <BearerToken>${AUTH_TOKEN}</BearerToken>
  </ClientSettings>
  <Parameters>
    <!-- REQUIRED: Resource under test -->
    <Parameter Name="EndpointResource" Value="${RESOURCE}"/>

    <!-- REQUIRED: Key field and sample key value -->
    <Parameter Name="KeyField" Value="$(xml_escape "$KEY_FIELD")"/>
    <Parameter Name="KeyValue" Value="'$(xml_escape "$KEY_VALUE")'"/>

    <!-- REQUIRED: Integer field -->
    <Parameter Name="IntegerField" Value="$(xml_escape "$INTEGER_FIELD")"/>

    <!-- REQUIRED: Decimal field -->
    <Parameter Name="DecimalField" Value="$(xml_escape "$DECIMAL_FIELD")"/>

    <!-- REQUIRED: Single-value lookup (string enum) -->
    <Parameter Name="SingleValueLookupField" Value="$(xml_escape "$SINGLE_LOOKUP_FIELD")"/>
    <Parameter Name="SingleLookupValue" Value="$(xml_escape "$SINGLE_LOOKUP_VALUE")"/>
    <Parameter Name="SingleValueLookupNamespace" Value=""/>

    <!-- REQUIRED: Multi-value lookup (collection enum) -->
    <Parameter Name="MultipleValueLookupField" Value="$(xml_escape "$MULTI_LOOKUP_FIELD")"/>
    <Parameter Name="MultipleValueLookupNamespace" Value=""/>
    <Parameter Name="MultipleLookupValue1" Value="$(xml_escape "$MULTI_LOOKUP_VALUE1")"/>
    <Parameter Name="MultipleLookupValue2" Value="$(xml_escape "$MULTI_LOOKUP_VALUE2")"/>

    <!-- REQUIRED: Date and Timestamp fields -->
    <Parameter Name="DateField" Value="$(xml_escape "$DATE_FIELD")"/>
    <Parameter Name="TimestampField" Value="$(xml_escape "$TIMESTAMP_FIELD")"/>

    <!-- Sample values — use actual data from the server so tests find results -->
    <Parameter Name="IntegerValueLow" Value="$(xml_escape "$INTEGER_VALUE")"/>
    <Parameter Name="IntegerValueHigh" Value="2147483647"/>
    <Parameter Name="IntegerNotFound" Value="-1"/>
    <Parameter Name="DecimalValueLow" Value="$(xml_escape "$DECIMAL_VALUE")"/>
    <Parameter Name="DecimalValueHigh" Value="$(xml_escape "$DECIMAL_VALUE")"/>
    <Parameter Name="FilterNotField" Value="*Parameter_IntegerField*"/>
    <Parameter Name="FilterNotValue" Value="-1"/>
    <Parameter Name="DateTimeValue" Value="$(xml_escape "$TIMESTAMP_VALUE")"/>
    <Parameter Name="DateValue" Value="$(xml_escape "$DATE_VALUE")"/>

    <!-- REQUIRED: HTTP code testing -->
    <Parameter Name="400BadRequest" Value="${RESOURCE}?\$filter=INVALIDFIELD eq 'bad'"/>
    <Parameter Name="404NotFound" Value="ResourceNotFound"/>

    <!-- Constants — do not change -->
    <Parameter Name="TopCount" Value="5"/>
    <Parameter Name="SortCount" Value="20"/>
    <Parameter Name="200_OK" Value="*Parameter_EndpointResource*"/>
    <Parameter Name="WebAPI102_RequiredResourceList" Value="Property,Member,Office,Media"/>
    <Parameter Name="DD17_WellKnownResourceList" Value="Property,Member,Office,Contacts,ContactListings,HistoryTransactional,InternetTracking,Media,OpenHouse,OUID,Prospecting,Queue,Rules,SavedSearch,Showing,Teams,TeamMembers,ContactListingNotes,OtherPhone,PropertyGreenVerification,PropertyPowerProduction,PropertyRooms,PropertyUnitTypes,SocialMedia"/>

    <!-- Computed values — do not change -->
    <Parameter Name="FilterHasValue" Value="*Parameter_SingleValueLookupValue*"/>
    <Parameter Name="FilterHasLookupNamespace" Value="*Parameter_SingleValueLookupNamespace*"/>
    <Parameter Name="FilterHasLookupValue" Value="*Parameter_SingleLookupValue*"/>
    <Parameter Name="SingleValueLookupValue" Value="*Parameter_SingleValueLookupNamespace*'*Parameter_SingleLookupValue*'"/>
    <Parameter Name="FilterHasValueLookupValue" Value="*Parameter_FilterHasLookupNamespace*'*Parameter_FilterHasLookupValue*'"/>
    <Parameter Name="MultipleValueLookupValue1" Value="*Parameter_MultipleValueLookupNamespace*'*Parameter_MultipleLookupValue1*'"/>
    <Parameter Name="MultipleValueLookupValue2" Value="*Parameter_MultipleValueLookupNamespace*'*Parameter_MultipleLookupValue2*'"/>
  </Parameters>
</OutputScript>
RESOSCRIPT_EOF

  GENERATED_COUNT=$((GENERATED_COUNT + 1))
  echo "  Wrote $OUTPUT_DIR/$RESOURCE.resoscript"
done

echo ""
echo "RESOScript generation complete: $GENERATED_COUNT generated, $SKIPPED_COUNT skipped."
ls -la "$OUTPUT_DIR"
