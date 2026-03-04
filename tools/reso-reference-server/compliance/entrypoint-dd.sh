#!/bin/sh
# ---------------------------------------------------------------------------
# entrypoint-dd.sh — Wait for server, run Data Dictionary 2.0 compliance tests
# ---------------------------------------------------------------------------
set -e

SERVER_URL="${SERVER_URL:-http://server:8080}"

echo "Waiting for server at $SERVER_URL..."
until wget -qO- "$SERVER_URL/health" > /dev/null 2>&1; do sleep 2; done
echo "Server ready."

# Substitute server URL into config template
sed "s|SERVER_URL_PLACEHOLDER|$SERVER_URL|g" /config/dd-config.json > /tmp/dd-config.json

echo "Running Data Dictionary 2.0 compliance tests..."
LIMIT_FLAG=""
if [ -n "$RECORD_LIMIT" ]; then
  LIMIT_FLAG="-l $RECORD_LIMIT"
  echo "Record limit: $RECORD_LIMIT"
fi
exec reso-certification-utils runDDTests -v 2.0 -p /tmp/dd-config.json -a $LIMIT_FLAG
