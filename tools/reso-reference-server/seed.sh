#!/usr/bin/env bash
# RESO Reference Server — Seed Data Script
#
# Seeds the reference server with realistic test data using the admin API.
# Uses dependency resolution to create resources in the correct order with valid FK linkages.
# Works with both Docker and locally running server instances.
#
# Usage:
#   ./seed.sh                                    # Defaults: localhost:8080, admin-token
#   ./seed.sh http://localhost:8080               # Custom URL
#   ./seed.sh http://localhost:8080 my-admin-tok  # Custom URL and token
#   ./seed.sh http://server:8080 admin-token      # Docker internal URL

set -e

URL="${1:-http://localhost:8080}"
TOKEN="${2:-admin-token}"

echo "RESO Reference Server — Seed Data"
echo "  Server: $URL"
echo ""

# Wait for server to be ready
echo "Waiting for server..."
until curl -sf "$URL/health" > /dev/null 2>&1; do
  sleep 2
done
echo "Server is ready."
echo ""

# Generate Property records with dependency resolution
# Automatically creates Member, Office, Teams in correct order with valid FK linkages
echo "Generating 50 Property records with dependencies (Member, Office, Teams, etc.)..."
curl -sf -X POST "$URL/admin/data-generator" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "resource": "Property",
    "count": 50,
    "resolveDependencies": true,
    "relatedRecords": {
      "Media": 5,
      "OpenHouse": 2,
      "Showing": 2,
      "PropertyRooms": 3,
      "PropertyGreenVerification": 1,
      "PropertyPowerProduction": 1,
      "PropertyUnitTypes": 2
    }
  }' | tee /dev/stderr | jq -r '"  Property: \(.created) created, \(.failed) failed"' 2>/dev/null || true
echo ""

echo "Seed complete."
