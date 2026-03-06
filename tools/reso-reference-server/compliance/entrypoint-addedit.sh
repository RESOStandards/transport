#!/bin/sh
set -e

# ---------------------------------------------------------------------------
# Add/Edit (RCP-010) compliance entrypoint
#
# 1. Wait for the server to be healthy
# 2. Seed minimal test data via the data generator
# 3. Download EDMX metadata from the server
# 4. Sample real Property records to extract keys
# 5. Generate payload files with sampled data
# 6. Run the add/edit certification tool
# ---------------------------------------------------------------------------

SERVER_URL="${SERVER_URL:-http://server:8080}"
AUTH_TOKEN="${AUTH_TOKEN:-admin-token}"
RESOURCE="${RESOURCE:-Property}"

echo "============================================"
echo " RESO Add/Edit (RCP-010) Compliance Test"
echo "============================================"
echo "Server:   $SERVER_URL"
echo "Resource: $RESOURCE"
echo ""

# --- 1. Wait for server ---
echo "Waiting for server at $SERVER_URL..."
until wget -qO- "$SERVER_URL/health" > /dev/null 2>&1; do sleep 2; done
echo "Server is ready."

# --- 2. Seed minimal data ---
echo "Seeding test data..."
wget -qO- --post-data='{"resource":"Property","count":5,"resolveDependencies":true}' \
  --header='Content-Type: application/json' \
  --header="Authorization: Bearer $AUTH_TOKEN" \
  "$SERVER_URL/admin/data-generator" || true
echo ""
echo "Seed complete."

# --- 3. Download EDMX metadata ---
echo "Downloading metadata..."
wget -qO /tmp/metadata.xml \
  --header="Authorization: Bearer $AUTH_TOKEN" \
  "$SERVER_URL/\$metadata"
echo "Metadata saved ($(wc -c < /tmp/metadata.xml) bytes)."

# --- 4. Sample real records ---
echo "Sampling $RESOURCE records..."
wget -qO /tmp/sample.json --header="Authorization: Bearer $AUTH_TOKEN" \
  "$SERVER_URL/$RESOURCE?\$top=2&\$orderby=ListingKey&\$select=ListingKey,ListPrice,BedroomsTotal,BathroomsTotalInteger,City,StateOrProvince,PostalCode,Country"

# Extract keys using Node (available in the container)
# Run from /tmp to avoid package.json "type":"module" in /app
FIRST_KEY=$(cd /tmp && node -p "JSON.parse(require('fs').readFileSync('/tmp/sample.json','utf-8')).value[0].ListingKey")
SECOND_KEY=$(cd /tmp && node -p "JSON.parse(require('fs').readFileSync('/tmp/sample.json','utf-8')).value[1].ListingKey")

echo "Sampled keys: update=$FIRST_KEY, delete=$SECOND_KEY"

# --- 5. Generate payload files ---
PAYLOADS_DIR="/tmp/payloads"
mkdir -p "$PAYLOADS_DIR"

# create-succeeds: valid Property with required fields
cat > "$PAYLOADS_DIR/create-succeeds.json" << PAYLOAD
{
  "ListPrice": 350000.00,
  "BedroomsTotal": 4,
  "BathroomsTotalInteger": 3,
  "City": "Test City",
  "StateOrProvince": "CA",
  "PostalCode": "90210",
  "Country": "US"
}
PAYLOAD

# create-fails: negative ListPrice triggers validation error
cat > "$PAYLOADS_DIR/create-fails.json" << PAYLOAD
{
  "ListPrice": -99999.00,
  "BedroomsTotal": 3,
  "BathroomsTotalInteger": 2
}
PAYLOAD

# update-succeeds: patch an existing record with a valid price change
cat > "$PAYLOADS_DIR/update-succeeds.json" << PAYLOAD
{
  "ListingKey": "$FIRST_KEY",
  "ListPrice": 375000.00
}
PAYLOAD

# update-fails: patch with invalid negative price
cat > "$PAYLOADS_DIR/update-fails.json" << PAYLOAD
{
  "ListingKey": "$FIRST_KEY",
  "ListPrice": -1.00
}
PAYLOAD

# delete-succeeds: delete the second sampled record
cat > "$PAYLOADS_DIR/delete-succeeds.json" << PAYLOAD
{
  "id": "$SECOND_KEY"
}
PAYLOAD

# delete-fails: non-existent key
cat > "$PAYLOADS_DIR/delete-fails.json" << PAYLOAD
{
  "id": "00000000-0000-0000-0000-000000000000"
}
PAYLOAD

echo "Generated payloads in $PAYLOADS_DIR"
echo ""

# --- 6. Run certification ---
echo "Running Add/Edit compliance tests..."
echo "--------------------------------------------"

REPORT_PATH="/tmp/compliance-report.json"

FAILED=0
node /app/dist/cli/index.js \
  --url "$SERVER_URL" \
  --resource "$RESOURCE" \
  --payloads "$PAYLOADS_DIR" \
  --auth-token "$AUTH_TOKEN" \
  --metadata /tmp/metadata.xml \
  --output console \
  --compliance-report "$REPORT_PATH" \
  --spec-version "${SPEC_VERSION:-2.0.0}" \
  || FAILED=1

echo ""
echo "============================================"
if [ "$FAILED" -eq 0 ]; then
  echo " All Add/Edit tests passed."
else
  echo " Some Add/Edit tests FAILED."
fi
echo "============================================"

if [ -f "$REPORT_PATH" ]; then
  echo ""
  echo "Compliance Report:"
  cat "$REPORT_PATH"
  echo ""
fi

exit $FAILED
