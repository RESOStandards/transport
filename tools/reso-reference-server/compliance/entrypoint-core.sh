#!/bin/sh
# ---------------------------------------------------------------------------
# entrypoint-core.sh — Wait for server, generate RESOScripts, run Web API
# Core 2.0.0 compliance tests for each resource.
# ---------------------------------------------------------------------------
set -e

SERVER_URL="${SERVER_URL:-http://server:8080}"
AUTH_TOKEN="${AUTH_TOKEN:-admin-token}"

echo "Waiting for server at $SERVER_URL..."
until wget -qO- "$SERVER_URL/health" > /dev/null 2>&1; do sleep 2; done
echo "Server ready."

# Generate RESOScripts from live server data
echo "Generating RESOScript configs from live server..."
/config/generate-resoscripts.sh "$SERVER_URL" "$AUTH_TOKEN" /tmp/resoscripts

# Run Web API Core tests for each resource
FAILED=0
for script in /tmp/resoscripts/*.resoscript; do
  RESOURCE=$(basename "$script" .resoscript)
  echo ""
  echo "============================================"
  echo "  Testing: $RESOURCE"
  echo "============================================"
  ./gradlew --no-daemon testWebApiCore \
    -DpathToRESOScript="$script" \
    -DuseStringEnums=true \
    -DuseCollections=true \
    -DshowResponses=true \
    || FAILED=1
done

if [ "$FAILED" = "1" ]; then
  echo ""
  echo "Some Web API Core tests FAILED."
  exit 1
fi

echo ""
echo "All Web API Core tests passed."
