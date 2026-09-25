/**
 * A metadata report generated from the specification's own field tables.
 *
 * This is the keystone. RESO's schema validator takes a metadata report and
 * produces a JSON Schema, so generating the report from Sections 2.4 to 2.7
 * makes the specification itself the validator: change a field's type or its
 * nullability and every payload check follows, with no second copy to keep in
 * step.
 *
 * The alternative — hand-writing a schema — is how the specification and its
 * tests drift apart. Today already proved that risk twice: a generator anchored
 * on prose broke when the prose changed, and a worked example nested a resource
 * no field table declared.
 *
 * The shape matches `dd-{version}.json` as `generate-reference-metadata.js`
 * produces it, so a consumer that already reads Data Dictionary reference
 * metadata reads this without special-casing.
 */

/** One field, in the Data Dictionary reference-metadata shape. */
export interface MetadataField {
  readonly resourceName: string;
  readonly fieldName: string;
  /** `Edm.String`, `Edm.Decimal`, or a fully-qualified enum type. */
  readonly type: string;
  readonly nullable: boolean;
  readonly isEnumeration: boolean;
  readonly isCollection?: boolean;
  /** For an expansion, the resource it targets. The Data Dictionary carries
   *  expansions as fields with this member, so a consumer that already reads
   *  reference metadata reads these the same way. */
  readonly sourceResource?: string;
  readonly maxLength?: number;
  readonly annotations?: ReadonlyArray<{ readonly term: string; readonly value: string }>;
}

export interface MetadataLookup {
  readonly lookupName: string;
  readonly lookupValue: string;
  readonly type: string;
}

export interface MetadataReport {
  readonly description: string;
  readonly generatedOn: string;
  readonly resources: ReadonlyArray<{ readonly resourceName: string }>;
  readonly fields: ReadonlyArray<MetadataField>;
  readonly lookups: ReadonlyArray<MetadataLookup>;
}

const ENUM_NS = 'org.reso.metadata.enums';

/** The three resource sections, by heading. Headings, never prose. */
const RESOURCE_SECTIONS: ReadonlyArray<readonly [string, string]> = [
  ['Offer', '## Section 2.4: The Offer Resource'],
  ['OfferSubmission', '## Section 2.5: The OfferSubmission Resource'],
  ['OfferPropertyGroup', '## Section 2.6: The OfferPropertyGroup Resource'],
];

interface RawRow {
  readonly name: string;
  readonly type: string;
  readonly nullable: string;
  readonly maxLength: string;
  readonly lookup: string;
}

const rowsOf = (spec: string, heading: string): readonly RawRow[] => {
  if (!spec.includes(heading)) throw new Error(`section heading moved or renamed: ${heading}`);
  const after = spec.split(heading, 2)[1];
  const seg = after.split(/\n##? Section /, 1)[0];
  const rows: RawRow[] = [];
  for (const line of seg.split('\n')) {
    if (!line.startsWith('| ') || line.startsWith('| :--') || line.startsWith('| Field |')) continue;
    const c = line.trim().replace(/^\||\|$/g, '').split('|').map((x) => x.trim());
    if (c.length < 6) continue;
    rows.push({ name: c[0], type: c[1], nullable: c[2], maxLength: c[3], lookup: c[4] });
  }
  if (rows.length === 0) throw new Error(`no field rows under ${heading}`);
  return rows;
};

/**
 * Map the specification's type column onto OData.
 *
 * "String List, Single" and "String List, Multi" are the Data Dictionary's way
 * of saying enumeration, single- or multi-valued. The distinction is not
 * cosmetic: a collection accepts an array and a single value does not, which is
 * exactly the mismatch found in three reused elements earlier today.
 */
const typeOf = (
  row: RawRow
): Pick<MetadataField, 'type' | 'isEnumeration' | 'isCollection' | 'sourceResource'> => {
  // An expansion names its target resource in the lookup column, the same column
  // a "String List" row uses to name its enumeration. Declared here because a
  // payload carrying an undeclared member fails RCF validation, which is how the
  // worked examples came to show `PropertyGroup` while no table declared it.
  if (row.type.startsWith('Expansion')) {
    const target = row.lookup.replace(/`/g, '').trim();
    if (target === '') throw new Error(`expansion ${row.name} names no target resource`);
    return {
      type: `${ENUM_NS.replace('.enums', '')}.${target}`,
      isEnumeration: false,
      isCollection: row.type.includes('Multi'),
      sourceResource: target,
    };
  }
  if (row.type.startsWith('String List')) {
    const lookup = row.lookup.replace(/`/g, '').trim();
    return {
      type: `${ENUM_NS}.${lookup === '' ? row.name : lookup}`,
      isEnumeration: true,
      isCollection: row.type.includes('Multi'),
    };
  }
  const plain: Record<string, string> = {
    String: 'Edm.String',
    Number: 'Edm.Int64',
    Decimal: 'Edm.Decimal',
    Timestamp: 'Edm.DateTimeOffset',
    Date: 'Edm.Date',
    Boolean: 'Edm.Boolean',
  };
  const t = plain[row.type];
  if (t === undefined) throw new Error(`unmapped type "${row.type}" on ${row.name}`);
  return { type: t, isEnumeration: false };
};

/** Lookup values, read from the state tables in Section 2.7. */
const lookupsOf = (spec: string): readonly MetadataLookup[] => {
  const out: MetadataLookup[] = [];
  for (const name of ['OfferSubmissionStatus', 'OfferReceivedStatus']) {
    const marker = `**${name}.**`;
    if (!spec.includes(marker)) throw new Error(`lookup section missing: ${name}`);
    const seg = spec.split(marker, 2)[1].split('\n\n**', 1)[0].split('\nThe definitions', 1)[0];
    for (const line of seg.split('\n')) {
      if (!line.startsWith('| ') || line.startsWith('| :--') || line.startsWith('| Lookup Value |')) {
        continue;
      }
      const c = line.trim().replace(/^\||\|$/g, '').split('|').map((x) => x.trim());
      if (c.length < 2 || c[0] === '') continue;
      out.push({ lookupName: `${ENUM_NS}.${name}`, lookupValue: c[0], type: `${ENUM_NS}.${name}` });
    }
  }
  if (out.length === 0) throw new Error('no lookup values found in Section 2.7');
  return out;
};

/**
 * Build the report.
 *
 * `generatedOn` is passed in rather than read from the clock, so the same
 * specification always produces the same bytes. A report that changes on every
 * run cannot be diffed, and diffing it against the last one is how a change to
 * the specification gets noticed.
 */
export const metadataFromSpec = (spec: string, generatedOn: string): MetadataReport => {
  const fields: MetadataField[] = [];
  for (const [resourceName, heading] of RESOURCE_SECTIONS) {
    for (const row of rowsOf(spec, heading)) {
      const max = Number.parseInt(row.maxLength, 10);
      fields.push({
        resourceName,
        fieldName: row.name,
        ...typeOf(row),
        nullable: row.nullable.toLowerCase() !== 'no',
        ...(Number.isFinite(max) ? { maxLength: max } : {}),
        annotations: [{ term: 'RESO.DDWikiUrl', value: `https://dd.reso.org/RCP-57/${resourceName}/${row.name}/` }],
      });
    }
  }
  return {
    description: 'RCP-57 Offer Management, generated from the specification',
    generatedOn,
    resources: RESOURCE_SECTIONS.map(([resourceName]) => ({ resourceName })),
    fields,
    lookups: lookupsOf(spec),
  };
};
