# #24 — Data Generator: Referentially Correct Multi-Resource Seed Data

## Context

The data generator produces test data for RESO OData resources with three output modes (HTTP POST, JSON files, curl scripts). Child collection FK linkages work (Media→Property, OpenHouse→Property), but **all to-one FK relationships are broken**:

- `field-generator.ts:83` blanket-skips ALL `*Key` fields, including FKs like `ListAgentKey`, `OfficeKey`
- No multi-resource orchestration — each resource is generated independently
- Seeding order is wrong (Property first, then Member, then Office)

**Result:** `$expand=ListAgent` always returns null because no Property has a valid `ListAgentKey` pointing to a real Member.

## Dependency Graph (from metadata)

To-one (`parent-fk`) relationships among TARGET_RESOURCES:

| Source | Nav Prop → FK Column | Target |
|--------|---------------------|--------|
| Property | ListAgent → ListAgentKey | Member |
| Property | BuyerAgent → BuyerAgentKey | Member |
| Property | CoListAgent → CoListAgentKey | Member |
| Property | CoBuyerAgent → CoBuyerAgentKey | Member |
| Property | ListOffice → ListOfficeKey | Office |
| Property | BuyerOffice → BuyerOfficeKey | Office |
| Property | CoListOffice → CoListOfficeKey | Office |
| Property | CoBuyerOffice → CoBuyerOfficeKey | Office |
| Property | ListTeam → ListTeamKey | Teams |
| Property | BuyerTeam → BuyerTeamKey | Teams |
| Member | Office → OfficeKey | Office |
| Office | OfficeBroker → OfficeBrokerKey | Member |
| Office | OfficeManager → OfficeManagerKey | Member |
| Office | MainOffice → MainOfficeKey | Office (self) |
| Media | ChangedByMember → ChangedByMemberKey | Member |
| Showing | ShowingAgent → ShowingAgentKey | Member |
| Teams | TeamLead → TeamLeadKey | Member |
| OUID | ChangedByMember → ChangedByMemberKey | Member |
| TeamMembers | Member → MemberKey | Member |
| TeamMembers | (implicit) TeamKey | Teams (no nav prop — FK by convention) |

**Only circular dependency:** Office ↔ Member. Resolved by creating Office first (without broker/manager), then Member, then back-filling Office.

**Note:** TeamMembers.TeamKey is a FK to Teams but has no corresponding navigation property in metadata. The FK resolver needs a supplemental list of known implicit FKs (fields named `{Resource}Key` where the resource exists in TARGET_RESOURCES).

**Creation order:** Office → Member → back-fill Office → OUID → Teams → TeamMembers → Property → child collections

## Implementation Plan

### Phase 1: New file `src/fk-resolver.ts` — FK discovery + dependency graph

Metadata-driven FK resolution (mirrors server's `buildNavigationBindings` logic but standalone):

```
discoverForeignKeys(resource, fields, fieldsByResource, keyFieldMap)
  → scans isExpansion=true, isCollection=false fields
  → checks if parent has {navPropName}Key column
  → returns ForeignKeyBinding[]

buildDependencyGraph(targetResources, fieldsByResource, keyFieldMap)
  → returns ResourceDependency[] (source→target edges with bindings)

topologicalSort(dependencies, targetResources)
  → returns { phases: SeedPhase[], backFillPhases: BackFillPhase[] }
  → detects Office↔Member cycle, breaks by deferring Office broker/manager to back-fill

buildMultiResourcePlan(requestedResource, count, relatedRecords, fieldsByResource, keyFieldMap)
  → walks transitive dependencies, sorts, assigns counts
  → returns MultiResourceSeedPlan
```

**New types** in `src/generators/types.ts`:

```typescript
interface ForeignKeyBinding {
  fkColumn: string;           // e.g. "ListAgentKey"
  targetResource: string;     // e.g. "Member"
  targetKeyField: string;     // e.g. "MemberKey"
  navPropName: string;        // e.g. "ListAgent"
}

interface SeedPhase {
  resource: string;
  count: number;
  fkBindings: ReadonlyArray<ForeignKeyBinding>;
}

interface BackFillPhase {
  resource: string;
  fkBindings: ReadonlyArray<ForeignKeyBinding>;
}

interface MultiResourceSeedPlan {
  phases: ReadonlyArray<SeedPhase>;
  backFillPhases: ReadonlyArray<BackFillPhase>;
  requestedResource: string;
  requestedCount: number;
  relatedRecords?: Record<string, number>;
}
```

Also add `KEY_FIELD_MAP` to `types.ts` (standalone copy for the data-generator package, including TeamMembers: 'TeamMemberKey').

**Dependency count defaults** when user requests N of the primary resource:
- Office: `max(2, ceil(N / 5))`
- Member: `max(4, ceil(N / 2))`
- Teams: `max(1, ceil(N / 10))`
- TeamMembers: `max(2, ceil(N / 5))`
- OUID: 2

**Tests:** FK discovery, dependency graph, topo sort with cycle breaking, plan building.

### Phase 2: Modify `src/index.ts` — Multi-resource orchestrator

Add `resolveDependencies?: boolean` to `SeedOptions`. When true:

1. Build `MultiResourceSeedPlan` from metadata
2. Maintain `keyPool: Record<string, string[]>` (resource → collected keys)
3. For each phase in order:
   - Generate records with existing generator
   - Inject FKs: for each binding, `record[fkColumn] = randomChoice(keyPool[targetResource])`
   - Output records (HTTP/JSON/curl)
   - Collect keys into pool (from server response or synthetic like `Office-0001`)
4. Execute back-fill phases (PATCH Office with Member keys)
5. Generate child collections as before (Media, OpenHouse, etc.)

Self-references (Office.MainOfficeKey → Office): inject from the same pool as it grows. First record may have no MainOfficeKey — that's correct.

### Phase 3: Modify `src/client.ts` — Add PATCH support

- `patchRecordsViaHttp()` — PATCH existing records with FK updates
- `updateJsonRecords()` — read-modify-write JSON files for back-fill
- Extend `generateCurlScript()` to accept patch commands (appended after POSTs)
- Fix key extraction in `postRecord` to use `KEY_FIELD_MAP[resource]` instead of heuristic "first `*Key` field"

### Phase 4: Cleanup generators

- Remove hardcoded `OfficeBrokerKey = 'BRK0001'` from `office.ts`
- No changes to `RecordGenerator` type signature — FK injection happens in the orchestrator, not the generator
- The `*Key` skip rule in `field-generator.ts:83` stays as-is (correct behavior — FKs are injected post-generation)

### Phase 5: Update CLI and Docker seed

**CLI (`src/cli.ts`):**
- Add `--with-dependencies` / `--deps` flag (default true)
- Interactive mode shows plan summary: "Property (10) requires: Office (4), Member (10), OUID (2), Teams (2)"
- Allow override via `--dep-counts Office:5,Member:20`

**Docker seed (`docker-compose.yml`):**
- Single seed call for Property with `resolveDependencies: true` handles everything
- Remove separate Member/Office seed calls

**Server endpoint (`server/src/admin/data-generator.ts`):**
- Add `resolveDependencies` to request body
- When true, use `buildMultiResourcePlan` + multi-phase generation via DAL
- Back-fill uses `dal.update()` directly (no HTTP PATCH needed)

### Phase 6: Tests

**New tests for `fk-resolver.ts`:**
- FK discovery from metadata (to-one only, ignores collections)
- Dependency graph construction
- Topo sort: acyclic graphs, Office↔Member cycle, self-references
- Plan building with correct counts and phases

**New tests for orchestrator:**
- JSON mode: verify dependency files created first, FK values reference real keys
- Back-fill: verify Office records updated with Member keys

**Existing tests (71):** All pass unchanged — generator signatures don't change, PK skip rule unchanged.

## Files Summary

| File | Action |
|------|--------|
| `data-generator/src/fk-resolver.ts` | **Create** — FK discovery, dependency graph, topo sort |
| `data-generator/src/generators/types.ts` | **Modify** — add KEY_FIELD_MAP, ForeignKeyBinding, SeedPhase, etc. |
| `data-generator/src/plan.ts` | **Modify** — add buildMultiResourceSeedPlan |
| `data-generator/src/index.ts` | **Modify** — multi-resource orchestration with FK injection |
| `data-generator/src/client.ts` | **Modify** — PATCH support, fix key extraction |
| `data-generator/src/generators/office.ts` | **Modify** — remove hardcoded OfficeBrokerKey |
| `data-generator/src/cli.ts` | **Modify** — --with-dependencies flag, plan summary |
| `data-generator/tests/fk-resolver.test.ts` | **Create** — FK discovery + topo sort tests |
| `reso-reference-server/docker-compose.yml` | **Modify** — simplify seed to single call |
| `reso-reference-server/server/src/admin/data-generator.ts` | **Modify** — resolveDependencies support |
| `reso-reference-server/server/src/metadata/types.ts` | **Already modified** — Teams, OUID, TeamMembers in TARGET_RESOURCES |
| `reso-reference-server/ui/src/types.ts` | **Already modified** — Teams, OUID, TeamMembers in TARGET_RESOURCES + KEY_FIELD_MAP |

All paths relative to `tools/`.

## Verification

```bash
# 1. Run data-generator tests
cd tools/data-generator && npm test

# 2. PostgreSQL: Reseed with dependencies
cd tools/reso-reference-server
docker compose down -v
docker compose up -d --build --wait
docker compose --profile seed up

# 3. PostgreSQL: Verify FK linkages
curl -s "http://localhost:8080/Property?$top=1&$select=ListingKey,ListAgentKey,ListOfficeKey" \
  -H 'Authorization: Bearer admin-token'
# → ListAgentKey and ListOfficeKey should be real UUIDs

# 4. PostgreSQL: Verify expansions work
curl -s "http://localhost:8080/Property('...')?\$expand=ListAgent,ListOffice,Media" \
  -H 'Authorization: Bearer admin-token'
# → ListAgent should be a full Member object, ListOffice a full Office object

# 5. PostgreSQL: Verify Member→Office expansion
curl -s "http://localhost:8080/Member('...')?\$expand=Office" \
  -H 'Authorization: Bearer admin-token'
# → Office should be a full Office object

# 6. MongoDB: Reseed with dependencies
docker compose down -v
docker compose --profile mongodb up -d --build --wait
docker compose --profile seed-mongo up

# 7. MongoDB: Verify same FK linkages and expansions
curl -s "http://localhost:8081/Property?$top=1&$select=ListingKey,ListAgentKey,ListOfficeKey" \
  -H 'Authorization: Bearer admin-token'
curl -s "http://localhost:8081/Property('...')?\$expand=ListAgent,ListOffice,Media" \
  -H 'Authorization: Bearer admin-token'
curl -s "http://localhost:8081/Member('...')?\$expand=Office" \
  -H 'Authorization: Bearer admin-token'

# 8. Verify JSON output mode
cd tools/data-generator
npx ts-node src/cli.ts -r Property -n 5 -f json -o ./test-output --deps
ls test-output/  # should have office/, member/, property/ directories
```
