#!/usr/bin/env python3
"""
DD reference metadata fitness checks.

Runs the principles from dd-reference-fitness-principles.md against one or more dd-{ver}.json
payloads. Exits non-zero on any failure. Port of the former check-dd-reference-fitness.js.

Usage:
  python3 check-dd-reference-fitness.py <dd-json-path> [<dd-json-path> ...]

Defaults to scanning references/dd/json/ if no paths are supplied.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path


def is_edm(t) -> bool:
    return isinstance(t, str) and t.startswith("Edm.")


def non_empty_string(v) -> bool:
    return isinstance(v, str) and len(v) > 0


def version_from_filename(path: str):
    m = re.match(r"^dd-(.+)\.json$", Path(path).name)
    return m.group(1) if m else None


# ── Per-file checks: each returns {"ok": bool, "detail"?: str, "info"?: str} ──


def check_resource_completeness(d, _ctx):
    resources = set(d.get("resources") or [])
    missing = [r for r in {f["resourceName"] for f in d["fields"]} if r not in resources]
    if not missing:
        return {"ok": True}
    return {"ok": False, "detail": f"missing from resources[]: {', '.join(missing)}"}


def check_expansion_shape(d, _ctx):
    exp = [f for f in d["fields"] if f.get("isExpansion") is True]
    bad = [
        f for f in exp
        if not non_empty_string(f.get("type"))
        or not non_empty_string(f.get("typeName"))
        or not non_empty_string(f.get("sourceResource"))
    ]
    if not bad:
        return {"ok": True, "info": f"{len(exp)} expansions checked"}
    listed = "\n  ".join(
        f"{f['resourceName']}.{f['fieldName']} "
        f"(type={f.get('type')}, typeName={f.get('typeName')}, sourceResource={f.get('sourceResource')})"
        for f in bad[:10]
    )
    return {"ok": False, "detail": f"{len(bad)} malformed:\n  {listed}"}


def check_sourceresource_resolves(d, _ctx):
    resources = set(d.get("resources") or [])
    exp = [f for f in d["fields"] if f.get("isExpansion") is True]
    unresolved = [
        f"{f['resourceName']}.{f['fieldName']} → {f['sourceResource']}"
        for f in exp
        if non_empty_string(f.get("sourceResource")) and f["sourceResource"] not in resources
    ]
    if not unresolved:
        return {"ok": True}
    return {"ok": False, "detail": f"{len(unresolved)} unresolved:\n  " + "\n  ".join(unresolved[:10])}


def check_non_edm_resolves(d, _ctx):
    lookup_names = {l_["lookupName"] for l_ in (d.get("lookups") or [])}
    candidates = [
        f for f in d["fields"]
        if not is_edm(f.get("type")) and not f.get("isExpansion") and f.get("lookupStatus") != "Open"
    ]
    unresolved = [
        f"{f['resourceName']}.{f['fieldName']} → {f['type']} (lookupStatus={f.get('lookupStatus', 'absent')})"
        for f in candidates
        if f.get("type") not in lookup_names
    ]
    if not unresolved:
        return {"ok": True, "info": f"{len(candidates)} non-Edm fields checked"}
    return {"ok": False, "detail": f"{len(unresolved)} unresolved:\n  " + "\n  ".join(unresolved[:10])}


def check_lookup_completeness(d, _ctx):
    lookups = d.get("lookups") or []
    bad = [
        l_ for l_ in lookups
        if not non_empty_string(l_.get("lookupName")) or not non_empty_string(l_.get("lookupValue"))
    ]
    if not bad:
        return {"ok": True, "info": f"{len(lookups)} lookups checked"}
    return {"ok": False, "detail": f"{len(bad)} malformed (first: {json.dumps(bad[0])})"}


def check_version_header_match(d, ctx):
    expected = version_from_filename(ctx["filename"])
    if not expected:
        return {"ok": True, "info": "filename has no version segment; skipping"}
    if d.get("version") == expected:
        return {"ok": True}
    return {"ok": False, "detail": f"filename says {expected}, top-level version is {d.get('version')}"}


CHECKS = [
    ("resource-completeness", "every field.resourceName appears in resources[]", check_resource_completeness),
    ("expansion-shape", "every expansion has type, typeName, sourceResource", check_expansion_shape),
    ("sourceresource-resolves", "every expansion sourceResource resolves to a resource", check_sourceresource_resolves),
    ("non-edm-resolves", 'every non-Edm field type (other than lookupStatus="Open") resolves to a lookup', check_non_edm_resolves),
    ("lookup-completeness", "every lookup has non-empty lookupName and lookupValue", check_lookup_completeness),
    ("version-header-match", "top-level version matches filename", check_version_header_match),
]

DEFAULT_PATHS = [
    "references/dd/json/dd-1.7.json",
    "references/dd/json/dd-2.0.json",
    "references/dd/json/dd-2.1.json",
]


def main() -> None:
    arg_paths = sys.argv[1:]
    candidates = arg_paths if arg_paths else DEFAULT_PATHS
    # absolute(), not resolve(): match Node path.resolve (no symlink following) for identical output.
    paths = [str(Path(p).absolute()) for p in candidates if Path(p).exists()]

    if not paths:
        print("No JSON files found.", file=sys.stderr)
        sys.exit(2)

    total_fail = 0
    files_by_version: dict = {}

    for path in paths:
        data = json.loads(Path(path).read_text(encoding="utf-8"))
        ver = version_from_filename(path)
        files_by_version.setdefault(ver, []).append({"path": path, "data": data})

        print(f"\n── {path} ──")
        for name, description, run in CHECKS:
            result = run(data, {"filename": path})
            if result["ok"]:
                info = f" ({result['info']})" if result.get("info") else ""
                print(f"  ✓ {name}: pass{info}")
            else:
                print(f"  ✗ {name}: {description}")
                print(f"    {result['detail']}")
                total_fail += 1

    # Cross-version FK consistency — runs across versions when >=2 are present.
    print("\n── cross-version checks ──")
    all_versions = [
        {"version": v, "data": files[0]["data"]}
        for v, files in files_by_version.items() if v
    ]
    if len(all_versions) >= 2:
        fk_by_key: dict = {}
        for entry in all_versions:
            for f in entry["data"]["fields"]:
                if f.get("isExpansion") and non_empty_string(f.get("sourceResource")):
                    key = f"{f['resourceName']}.{f['fieldName']}"
                    fk_by_key.setdefault(key, []).append(
                        {"version": entry["version"], "sourceResource": f["sourceResource"]}
                    )
        drift = []
        for key, entries in fk_by_key.items():
            targets = {e["sourceResource"] for e in entries}
            if len(targets) > 1:
                joined = ", ".join(f"{e['version']}→{e['sourceResource']}" for e in entries)
                drift.append(f"{key}: {joined}")
        if not drift:
            print(f"  ✓ cross-version-fk-consistency: pass ({len(fk_by_key)} FK fields shared across versions)")
        else:
            print(f"  ⚠ cross-version-fk-consistency: {len(drift)} fields drift across versions")
            for d_ in drift[:10]:
                print(f"    {d_}")
            print("    (note: cross-version drift is an audit-flag, not a fail — verify against version rename docs)")
    else:
        print("  (skipped — need ≥2 versions for cross-version check)")

    print()
    if total_fail == 0:
        print(f"✓ All checks passed across {len(paths)} file(s).")
        sys.exit(0)
    else:
        print(f"✗ {total_fail} check failure(s) across {len(paths)} file(s).")
        sys.exit(1)


if __name__ == "__main__":
    main()
