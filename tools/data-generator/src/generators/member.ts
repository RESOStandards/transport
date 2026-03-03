import { generateRecord, randomChoice, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

const FIRST_NAMES = [
  'James',
  'Mary',
  'Robert',
  'Patricia',
  'John',
  'Jennifer',
  'Michael',
  'Linda',
  'David',
  'Elizabeth',
  'William',
  'Barbara',
  'Richard',
  'Susan',
  'Joseph',
  'Jessica',
  'Thomas',
  'Sarah',
  'Christopher',
  'Karen',
  'Daniel',
  'Lisa',
  'Matthew',
  'Nancy',
  'Anthony',
  'Betty',
  'Mark',
  'Margaret',
  'Steven',
  'Sandra',
  'Andrew',
  'Ashley'
];

const LAST_NAMES = [
  'Smith',
  'Johnson',
  'Williams',
  'Brown',
  'Jones',
  'Garcia',
  'Miller',
  'Davis',
  'Rodriguez',
  'Martinez',
  'Hernandez',
  'Lopez',
  'Gonzalez',
  'Wilson',
  'Anderson',
  'Thomas',
  'Taylor',
  'Moore',
  'Jackson',
  'Martin',
  'Lee',
  'Perez',
  'Thompson',
  'White',
  'Harris',
  'Sanchez',
  'Clark',
  'Ramirez',
  'Lewis',
  'Robinson',
  'Walker'
];

const EMAIL_DOMAINS = ['realestate.example.com', 'homes.example.com', 'property.example.com', 'realty.example.com', 'broker.example.com'];

const DESIGNATIONS = ['CRS', 'ABR', 'GRI', 'SRES', 'SRS', 'CIPS'];

/** Generates a realistic phone number. */
const randomPhone = (): string => {
  const area = randomInt(200, 999);
  const prefix = randomInt(200, 999);
  const line = randomInt(1000, 9999);
  return `${area}-${prefix}-${line}`;
};

/** Generates realistic Member records with domain-specific overrides. */
export const generateMemberRecords = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number
): ReadonlyArray<Record<string, unknown>> =>
  Array.from({ length: count }, (_, i) => {
    const record = generateRecord(fields, lookups, i);

    const firstName = randomChoice(FIRST_NAMES);
    const lastName = randomChoice(LAST_NAMES);
    const domain = randomChoice(EMAIL_DOMAINS);

    record.MemberFirstName = firstName;
    record.MemberLastName = lastName;
    record.MemberFullName = `${firstName} ${lastName}`;
    record.MemberEmail = `${firstName.toLowerCase()}.${lastName.toLowerCase()}@${domain}`;
    record.MemberPreferredPhone = randomPhone();
    record.MemberDirectPhone = randomPhone();
    record.MemberOfficePhone = randomPhone();
    record.MemberMobilePhone = randomPhone();

    // Designations
    const numDesignations = randomInt(0, 3);
    if (numDesignations > 0) {
      const shuffled = [...DESIGNATIONS].sort(() => Math.random() - 0.5);
      record.MemberDesignation = shuffled.slice(0, numDesignations);
    }

    // Status — prefer Active
    const statusValues = lookups['org.reso.metadata.enums.MemberStatus'];
    if (statusValues && statusValues.length > 0) {
      const active = statusValues.find(s => s.lookupValue === 'Active');
      record.MemberStatus = active ? 'Active' : randomChoice(statusValues).lookupValue;
    }

    // National association ID
    record.MemberNationalAssociationId = `NAR${String(randomInt(100000, 999999))}`;

    return record;
  });
