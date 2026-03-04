#!/bin/sh
# ---------------------------------------------------------------------------
# generate-resoscripts.sh — Generate per-resource RESOScript config files
#
# Queries the running reference server's $metadata and sample records to
# populate field-type parameters required by web-api-commander's
# testWebApiCore_2_0_0 task.
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

# ---------------------------------------------------------------------------
# Helper: extract field names of a given Edm type from metadata XML for a
# specific EntityType, using basic text processing (no xmllint dependency).
# ---------------------------------------------------------------------------
extract_fields_by_type() {
  ENTITY_TYPE="$1"
  EDM_TYPE="$2"
  # Find Property elements within the EntityType block matching the Edm type
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
# For each resource: fetch a sample record, resolve field types, write XML.
# ---------------------------------------------------------------------------
for entry in $RESOURCES; do
  RESOURCE=$(echo "$entry" | cut -d: -f1)
  KEY_FIELD=$(echo "$entry" | cut -d: -f2)

  echo "--- Generating RESOScript for $RESOURCE (key=$KEY_FIELD) ---"

  # Fetch one sample record
  SAMPLE=$(wget -qO- --header="Authorization: Bearer $AUTH_TOKEN" \
    "$SERVER_URL/$RESOURCE?\$top=1" 2>/dev/null || echo "{}")

  RECORD=$(echo "$SAMPLE" | jq -r '.value[0] // empty' 2>/dev/null)

  if [ -z "$RECORD" ] || [ "$RECORD" = "null" ]; then
    echo "  WARNING: No sample records for $RESOURCE — skipping"
    continue
  fi

  # Key value
  KEY_VALUE=$(echo "$RECORD" | jq -r --arg k "$KEY_FIELD" '.[$k] // empty')
  if [ -z "$KEY_VALUE" ]; then
    echo "  WARNING: No key value for $KEY_FIELD in $RESOURCE — skipping"
    continue
  fi

  # --- Find first field with data for each Edm type ---

  # Integer fields (Edm.Int32, Edm.Int64)
  INTEGER_FIELD=""
  INTEGER_VALUE=""
  for type in "Edm.Int64" "Edm.Int32"; do
    FIELDS=$(extract_fields_by_type "$RESOURCE" "$type")
    for f in $FIELDS; do
      val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
      if [ -n "$val" ] && [ "$val" != "null" ]; then
        INTEGER_FIELD="$f"
        INTEGER_VALUE="$val"
        break 2
      fi
    done
  done

  # Decimal fields (Edm.Decimal)
  DECIMAL_FIELD=""
  DECIMAL_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.Decimal")
  for f in $FIELDS; do
    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ]; then
      DECIMAL_FIELD="$f"
      DECIMAL_VALUE="$val"
      break
    fi
  done

  # Date fields (Edm.Date)
  DATE_FIELD=""
  DATE_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.Date")
  for f in $FIELDS; do
    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ]; then
      DATE_FIELD="$f"
      DATE_VALUE="$val"
      break
    fi
  done

  # Timestamp fields (Edm.DateTimeOffset)
  TIMESTAMP_FIELD=""
  TIMESTAMP_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.DateTimeOffset")
  for f in $FIELDS; do
    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ]; then
      TIMESTAMP_FIELD="$f"
      TIMESTAMP_VALUE="$val"
      break
    fi
  done

  # Boolean field (Edm.Boolean) — used for FilterHasField
  BOOLEAN_FIELD=""
  BOOLEAN_VALUE=""
  FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.Boolean")
  for f in $FIELDS; do
    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ]; then
      BOOLEAN_FIELD="$f"
      BOOLEAN_VALUE="$val"
      break
    fi
  done

  # String field (Edm.String without lookup annotation)
  STRING_FIELD=""
  STRING_VALUE=""
  ALL_STRING_FIELDS=$(extract_fields_by_type "$RESOURCE" "Edm.String")
  LOOKUP_FIELDS=$(extract_single_lookup_fields "$RESOURCE")
  for f in $ALL_STRING_FIELDS; do
    # Skip if it's a lookup field
    is_lookup=0
    for lf in $LOOKUP_FIELDS; do
      if [ "$f" = "$lf" ]; then
        is_lookup=1
        break
      fi
    done
    if [ "$is_lookup" = "1" ]; then continue; fi

    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ]; then
      STRING_FIELD="$f"
      STRING_VALUE="$val"
      break
    fi
  done

  # Single-value lookup field
  SINGLE_LOOKUP_FIELD=""
  SINGLE_LOOKUP_VALUE=""
  for f in $LOOKUP_FIELDS; do
    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ]; then
      SINGLE_LOOKUP_FIELD="$f"
      SINGLE_LOOKUP_VALUE="$val"
      break
    fi
  done

  # Multi-value lookup field
  MULTI_LOOKUP_FIELD=""
  MULTI_LOOKUP_VALUE=""
  MULTI_FIELDS=$(extract_multi_lookup_fields "$RESOURCE")
  for f in $MULTI_FIELDS; do
    val=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f] // empty')
    if [ -n "$val" ] && [ "$val" != "null" ] && [ "$val" != "[]" ]; then
      MULTI_LOOKUP_FIELD="$f"
      # Extract first value from the array
      MULTI_LOOKUP_VALUE=$(echo "$RECORD" | jq -r --arg f "$f" '.[$f][0] // empty')
      break
    fi
  done

  echo "  Key: $KEY_FIELD=$KEY_VALUE"
  echo "  Integer: $INTEGER_FIELD=$INTEGER_VALUE"
  echo "  Decimal: $DECIMAL_FIELD=$DECIMAL_VALUE"
  echo "  Date: $DATE_FIELD=$DATE_VALUE"
  echo "  Timestamp: $TIMESTAMP_FIELD=$TIMESTAMP_VALUE"
  echo "  Boolean: $BOOLEAN_FIELD=$BOOLEAN_VALUE"
  echo "  String: $STRING_FIELD=$STRING_VALUE"
  echo "  SingleLookup: $SINGLE_LOOKUP_FIELD=$SINGLE_LOOKUP_VALUE"
  echo "  MultiLookup: $MULTI_LOOKUP_FIELD=$MULTI_LOOKUP_VALUE"

  # Escape XML special characters in values
  xml_escape() {
    echo "$1" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g'
  }

  # Write RESOScript XML
  cat > "$OUTPUT_DIR/$RESOURCE.resoscript" <<RESOSCRIPT_EOF
<?xml version="1.0" encoding="utf-8" ?>
<OutputScript>
  <ClientSettings>
    <ServerName>RESO Reference Server</ServerName>
    <WebAPIURI>${SERVER_URL}</WebAPIURI>
    <AuthenticationType>authorization_code</AuthenticationType>
    <BearerToken>${AUTH_TOKEN}</BearerToken>
    <Preauthenticate>true</Preauthenticate>
  </ClientSettings>
  <Parameters>
    <Parameter Name="EndpointResource" Value="${RESOURCE}"/>
    <Parameter Name="TopCount" Value="5"/>
    <Parameter Name="SortCount" Value="5"/>
    <Parameter Name="SelectCount" Value="1"/>
    <Parameter Name="ServiceRoot" Value="${SERVER_URL}"/>

    <Parameter Name="KeyField" Value="$(xml_escape "$KEY_FIELD")"/>
    <Parameter Name="KeyValue" Value="'$(xml_escape "$KEY_VALUE")'"/>

    <Parameter Name="IntegerField" Value="$(xml_escape "$INTEGER_FIELD")"/>
    <Parameter Name="IntegerValueLow" Value="${INTEGER_VALUE:+0}"/>
    <Parameter Name="IntegerValueHigh" Value="${INTEGER_VALUE:+2147483647}"/>

    <Parameter Name="DecimalField" Value="$(xml_escape "$DECIMAL_FIELD")"/>
    <Parameter Name="DecimalValueLow" Value="${DECIMAL_VALUE:+0.00}"/>
    <Parameter Name="DecimalValueHigh" Value="${DECIMAL_VALUE:+99999999.99}"/>

    <Parameter Name="DateField" Value="$(xml_escape "$DATE_FIELD")"/>
    <Parameter Name="DateValueLow" Value="${DATE_VALUE:+2000-01-01}"/>
    <Parameter Name="DateValueHigh" Value="${DATE_VALUE:+2099-12-31}"/>

    <Parameter Name="TimestampField" Value="$(xml_escape "$TIMESTAMP_FIELD")"/>
    <Parameter Name="TimestampValueLow" Value="${TIMESTAMP_VALUE:+2000-01-01T00:00:00Z}"/>
    <Parameter Name="TimestampValueHigh" Value="${TIMESTAMP_VALUE:+2099-12-31T23:59:59Z}"/>

    <Parameter Name="BooleanField" Value="$(xml_escape "$BOOLEAN_FIELD")"/>

    <Parameter Name="StringField" Value="$(xml_escape "$STRING_FIELD")"/>
    <Parameter Name="StringValue" Value="$(xml_escape "$STRING_VALUE")"/>

    <Parameter Name="FilterHasField" Value="$(xml_escape "${BOOLEAN_FIELD:-$STRING_FIELD}")"/>
    <Parameter Name="FilterNotField" Value="$(xml_escape "${BOOLEAN_FIELD:-$STRING_FIELD}")"/>

    <Parameter Name="SingleValueLookupField" Value="$(xml_escape "$SINGLE_LOOKUP_FIELD")"/>
    <Parameter Name="SingleValueLookupValue" Value="$(xml_escape "$SINGLE_LOOKUP_VALUE")"/>
    <Parameter Name="MultipleValueLookupField" Value="$(xml_escape "$MULTI_LOOKUP_FIELD")"/>
    <Parameter Name="MultipleValueLookupValue" Value="$(xml_escape "$MULTI_LOOKUP_VALUE")"/>

    <Parameter Name="ContainsSortableField" Value="$(xml_escape "${STRING_FIELD:-$KEY_FIELD}")"/>
  </Parameters>
</OutputScript>
RESOSCRIPT_EOF

  echo "  Wrote $OUTPUT_DIR/$RESOURCE.resoscript"
done

echo ""
echo "RESOScript generation complete."
ls -la "$OUTPUT_DIR"
