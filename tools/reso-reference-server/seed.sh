#!/usr/bin/env bash
# RESO Reference Server — Seed Data Script
#
# Seeds the reference server with realistic test data using the admin API.
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

# Generate Property records with Media, OpenHouse, and Showing
echo "Generating 50 Property records (with Media x5, OpenHouse x2, Showing x2 each)..."
curl -sf -X POST "$URL/admin/data-generator" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "resource": "Property",
    "count": 50,
    "relatedRecords": {
      "Media": 5,
      "OpenHouse": 2,
      "Showing": 2
    }
  }' | tee /dev/stderr | jq -r '"  Property: \(.created) created, \(.failed) failed"' 2>/dev/null || true
echo ""

# Generate Member records
echo "Generating 20 Member records..."
curl -sf -X POST "$URL/admin/data-generator" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"resource": "Member", "count": 20}' \
  | tee /dev/stderr | jq -r '"  Member: \(.created) created, \(.failed) failed"' 2>/dev/null || true
echo ""

# Generate Office records
echo "Generating 10 Office records..."
curl -sf -X POST "$URL/admin/data-generator" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"resource": "Office", "count": 10}' \
  | tee /dev/stderr | jq -r '"  Office: \(.created) created, \(.failed) failed"' 2>/dev/null || true
echo ""

echo "Seed complete."
