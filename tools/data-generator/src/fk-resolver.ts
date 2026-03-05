import type {
  BackFillPhase,
  ForeignKeyBinding,
  MultiResourceSeedPlan,
  ResoField,
  ResourceDependency,
  SeedPhase
} from './generators/types.js';
import { KEY_FIELD_MAP } from './generators/types.js';
import { isChildResource } from './plan.js';

// ---------------------------------------------------------------------------
// FK discovery
// ---------------------------------------------------------------------------

/**
 * Discovers to-one FK bindings for a resource by scanning its expansion fields.
 *
 * For each field with isExpansion=true and isCollection=false, checks whether the
 * resource has a matching FK column named `{navPropName}Key`. Also detects implicit
 * FKs where a field is named `{TargetResource}Key` and the target exists in the
 * key field map (e.g. TeamMembers.TeamKey → Teams).
 */
export const discoverForeignKeys = (
  resource: string,
  fields: ReadonlyArray<ResoField>,
  keyFieldMap: Readonly<Record<string, string>>
): ReadonlyArray<ForeignKeyBinding> => {
  const fieldNames = new Set(fields.filter(f => !f.isExpansion).map(f => f.fieldName));
  const bindings: ForeignKeyBinding[] = [];
  const boundColumns = new Set<string>();

  // 1. Nav-prop-based discovery: isExpansion=true, isCollection=false
  for (const field of fields) {
    if (!field.isExpansion || field.isCollection || !field.typeName) continue;

    const targetResource = field.typeName;
    const targetKeyField = keyFieldMap[targetResource];
    if (!targetKeyField) continue;

    // Convention: FK column = navPropName + 'Key'
    const fkColumn = `${field.fieldName}Key`;
    if (!fieldNames.has(fkColumn)) continue;

    bindings.push({ fkColumn, targetResource, targetKeyField, navPropName: field.fieldName });
    boundColumns.add(fkColumn);
  }

  // 2. Implicit FK discovery: fields whose name matches a PK in the key field map
  //    Uses reverse lookup (keyField → resource) to handle naming conventions
  //    like TeamMembers.TeamKey → Teams (where Teams has PK=TeamKey)
  const reverseKeyMap = new Map<string, string>();
  for (const [targetRes, keyField] of Object.entries(keyFieldMap)) {
    reverseKeyMap.set(keyField, targetRes);
  }

  for (const fieldName of fieldNames) {
    if (boundColumns.has(fieldName)) continue;
    if (!fieldName.endsWith('Key') || fieldName.length <= 3) continue;

    // Skip the resource's own PK
    const ownPk = keyFieldMap[resource];
    if (fieldName === ownPk) continue;

    // Check if this field name is a PK of another resource
    const targetResource = reverseKeyMap.get(fieldName);
    if (!targetResource || targetResource === resource) continue;

    // Don't create implicit FK for child collection resources (they link the other way)
    if (isChildResource(targetResource)) continue;

    const targetKeyField = keyFieldMap[targetResource];
    bindings.push({ fkColumn: fieldName, targetResource, targetKeyField, navPropName: targetResource });
    boundColumns.add(fieldName);
  }

  return bindings;
};

// ---------------------------------------------------------------------------
// Dependency graph
// ---------------------------------------------------------------------------

/**
 * Builds a dependency graph for the given resources.
 * Each edge means: sourceResource depends on targetResource (has FK pointing to it).
 * Only includes edges where both source and target are in the targetResources set.
 */
export const buildDependencyGraph = (
  targetResources: ReadonlyArray<string>,
  fieldsByResource: Readonly<Record<string, ReadonlyArray<ResoField>>>,
  keyFieldMap: Readonly<Record<string, string>>
): ReadonlyArray<ResourceDependency> => {
  const targetSet = new Set(targetResources);
  const dependencies: ResourceDependency[] = [];

  for (const resource of targetResources) {
    const fields = fieldsByResource[resource];
    if (!fields) continue;

    const allBindings = discoverForeignKeys(resource, fields, keyFieldMap);

    // Group bindings by target resource, keeping only targets in our set
    const byTarget = new Map<string, ForeignKeyBinding[]>();
    for (const binding of allBindings) {
      if (!targetSet.has(binding.targetResource)) continue;
      const existing = byTarget.get(binding.targetResource) ?? [];
      existing.push(binding);
      byTarget.set(binding.targetResource, existing);
    }

    for (const [targetResource, bindings] of byTarget) {
      dependencies.push({ sourceResource: resource, targetResource, bindings });
    }
  }

  return dependencies;
};

// ---------------------------------------------------------------------------
// Topological sort with cycle breaking
// ---------------------------------------------------------------------------

/** Result of topological sort: ordered phases + back-fill phases for broken cycles. */
export interface TopologicalSortResult {
  /** Resources in creation order (dependencies first). */
  readonly order: ReadonlyArray<string>;
  /** Edges deferred to back-fill (from cycle breaking). */
  readonly deferredEdges: ReadonlyArray<ResourceDependency>;
}

/**
 * Topological sort with cycle detection and breaking.
 *
 * When a cycle is detected, the edge from the resource with more incoming edges
 * (more depended-upon) is deferred to back-fill. This ensures the more "fundamental"
 * resource is created first.
 *
 * For the known Office ↔ Member cycle: Office is created first (it's depended on
 * by more resources), and its OfficeBroker/OfficeManager FKs are deferred.
 */
export const topologicalSort = (
  dependencies: ReadonlyArray<ResourceDependency>,
  resources: ReadonlyArray<string>
): TopologicalSortResult => {
  // Build adjacency: source → set of targets (source depends on target → target must come first)
  const adj = new Map<string, Set<string>>();
  const depMap = new Map<string, ResourceDependency>();

  for (const resource of resources) {
    adj.set(resource, new Set());
  }

  const deferredEdges: ResourceDependency[] = [];

  // Count how many resources depend on each target (incoming edge count in dependency graph)
  const incomingCount = new Map<string, number>();
  for (const resource of resources) {
    incomingCount.set(resource, 0);
  }
  for (const dep of dependencies) {
    // Skip self-references (e.g. Office.MainOfficeKey → Office)
    if (dep.sourceResource === dep.targetResource) continue;
    incomingCount.set(dep.targetResource, (incomingCount.get(dep.targetResource) ?? 0) + 1);
  }

  // Detect and break cycles before building the final adjacency
  // A cycle exists when A depends on B and B depends on A
  const edgePairs = new Map<string, ResourceDependency>();
  for (const dep of dependencies) {
    if (dep.sourceResource === dep.targetResource) continue;
    const key = `${dep.sourceResource}->${dep.targetResource}`;
    edgePairs.set(key, dep);
  }

  for (const dep of dependencies) {
    if (dep.sourceResource === dep.targetResource) continue;
    const reverseKey = `${dep.targetResource}->${dep.sourceResource}`;
    if (edgePairs.has(reverseKey)) {
      // Cycle detected! Defer the edge FROM the more-depended-upon resource
      const sourceIncoming = incomingCount.get(dep.sourceResource) ?? 0;
      const targetIncoming = incomingCount.get(dep.targetResource) ?? 0;
      if (sourceIncoming >= targetIncoming) {
        // Defer this edge (source is more depended upon, so it should be created first)
        deferredEdges.push(dep);
        continue;
      }
    }

    adj.get(dep.sourceResource)?.add(dep.targetResource);
    depMap.set(`${dep.sourceResource}->${dep.targetResource}`, dep);
  }

  // Kahn's algorithm
  const inDegree = new Map<string, number>();
  for (const resource of resources) {
    inDegree.set(resource, 0);
  }
  for (const [source, targets] of adj) {
    for (const target of targets) {
      inDegree.set(target, (inDegree.get(target) ?? 0) + 1);
    }
  }

  // Note: in our graph, edges go source→target meaning "source depends on target".
  // In Kahn's, we need to reverse this: target must come before source.
  // So we actually want to process nodes with in-degree 0 in the REVERSED graph.
  // Let's rebuild: if source depends on target, then in the ordering graph target→source.
  const orderAdj = new Map<string, Set<string>>();
  const orderInDegree = new Map<string, number>();
  for (const resource of resources) {
    orderAdj.set(resource, new Set());
    orderInDegree.set(resource, 0);
  }
  for (const [source, targets] of adj) {
    for (const target of targets) {
      orderAdj.get(target)?.add(source);
      orderInDegree.set(source, (orderInDegree.get(source) ?? 0) + 1);
    }
  }

  const queue: string[] = [];
  for (const [resource, degree] of orderInDegree) {
    if (degree === 0) queue.push(resource);
  }
  // Sort for deterministic output
  queue.sort();

  const order: string[] = [];
  while (queue.length > 0) {
    const current = queue.shift()!;
    order.push(current);

    for (const neighbor of orderAdj.get(current) ?? []) {
      const newDegree = (orderInDegree.get(neighbor) ?? 1) - 1;
      orderInDegree.set(neighbor, newDegree);
      if (newDegree === 0) {
        // Insert sorted for determinism
        const insertIdx = queue.findIndex(q => q > neighbor);
        if (insertIdx === -1) queue.push(neighbor);
        else queue.splice(insertIdx, 0, neighbor);
      }
    }
  }

  return { order, deferredEdges };
};

// ---------------------------------------------------------------------------
// Multi-resource seed plan
// ---------------------------------------------------------------------------

/** Default counts for dependency resources relative to the requested count N. */
const DEFAULT_DEPENDENCY_RATIOS: Readonly<Record<string, (n: number) => number>> = {
  Office: n => Math.max(2, Math.ceil(n / 5)),
  Member: n => Math.max(4, Math.ceil(n / 2)),
  Teams: n => Math.max(1, Math.ceil(n / 10)),
  TeamMembers: n => Math.max(2, Math.ceil(n / 5)),
  OUID: () => 2
};

/**
 * Builds a multi-resource seed plan for the requested resource.
 *
 * Walks transitive dependencies, topologically sorts them, and assigns
 * record counts for each dependency resource.
 */
export const buildMultiResourcePlan = (
  requestedResource: string,
  requestedCount: number,
  relatedRecords: Readonly<Record<string, number>> | undefined,
  fieldsByResource: Readonly<Record<string, ReadonlyArray<ResoField>>>,
  keyFieldMap: Readonly<Record<string, string>> = KEY_FIELD_MAP
): MultiResourceSeedPlan => {
  // Find all resources that have fields available
  const availableResources = Object.keys(fieldsByResource).filter(r => keyFieldMap[r]);

  // Build full dependency graph
  const allDeps = buildDependencyGraph(availableResources, fieldsByResource, keyFieldMap);

  // Find transitive dependencies of the requested resource
  const needed = new Set<string>([requestedResource]);
  const depsForResource = new Map<string, ReadonlyArray<ForeignKeyBinding>>();

  // Build a lookup: resource → its FK dependencies
  const depsBySource = new Map<string, ResourceDependency[]>();
  for (const dep of allDeps) {
    const existing = depsBySource.get(dep.sourceResource) ?? [];
    existing.push(dep);
    depsBySource.set(dep.sourceResource, existing);
  }

  // BFS to find all transitive dependencies
  const queue = [requestedResource];
  while (queue.length > 0) {
    const current = queue.shift()!;
    for (const dep of depsBySource.get(current) ?? []) {
      if (dep.targetResource === current) continue; // skip self-refs
      if (!needed.has(dep.targetResource)) {
        needed.add(dep.targetResource);
        queue.push(dep.targetResource);
      }
    }
  }

  // Also include child collection resources' transitive dependencies
  // e.g. if Media depends on Member, Member needs to be in the plan
  if (relatedRecords) {
    for (const childResource of Object.keys(relatedRecords)) {
      const childQueue = [childResource];
      while (childQueue.length > 0) {
        const current = childQueue.shift()!;
        for (const dep of depsBySource.get(current) ?? []) {
          if (dep.targetResource === current) continue;
          if (!needed.has(dep.targetResource)) {
            needed.add(dep.targetResource);
            childQueue.push(dep.targetResource);
          }
        }
      }
    }
  }

  // Filter dependencies and resources to only those needed
  const neededResources = availableResources.filter(r => needed.has(r));
  const neededDeps = allDeps.filter(d => needed.has(d.sourceResource) && needed.has(d.targetResource));

  // Topological sort
  const { order, deferredEdges } = topologicalSort(neededDeps, neededResources);

  // Build FK binding map: resource → all its bindings (non-deferred)
  const deferredSet = new Set(deferredEdges.map(d => `${d.sourceResource}->${d.targetResource}`));
  const bindingsByResource = new Map<string, ForeignKeyBinding[]>();
  for (const dep of neededDeps) {
    if (dep.sourceResource === dep.targetResource) continue;
    if (deferredSet.has(`${dep.sourceResource}->${dep.targetResource}`)) continue;
    const existing = bindingsByResource.get(dep.sourceResource) ?? [];
    existing.push(...dep.bindings);
    bindingsByResource.set(dep.sourceResource, existing);
  }

  // Build phases with counts
  const phases: SeedPhase[] = [];
  for (const resource of order) {
    let count: number;
    if (resource === requestedResource) {
      count = requestedCount;
    } else {
      const ratioFn = DEFAULT_DEPENDENCY_RATIOS[resource];
      count = ratioFn ? ratioFn(requestedCount) : Math.max(2, Math.ceil(requestedCount / 5));
    }

    phases.push({
      resource,
      count,
      fkBindings: bindingsByResource.get(resource) ?? []
    });
  }

  // Build back-fill phases
  const backFillPhases: BackFillPhase[] = [];
  for (const dep of deferredEdges) {
    const existing = backFillPhases.find(b => b.resource === dep.sourceResource);
    if (existing) {
      (existing.fkBindings as ForeignKeyBinding[]).push(...dep.bindings);
    } else {
      backFillPhases.push({ resource: dep.sourceResource, fkBindings: [...dep.bindings] });
    }
  }

  return {
    phases,
    backFillPhases,
    requestedResource,
    requestedCount,
    relatedRecords: relatedRecords ? { ...relatedRecords } : undefined
  };
};
