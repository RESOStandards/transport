# @reso/validation

Isomorphic TypeScript validation library for RESO Data Dictionary records. Validates field types, lengths, and ranges against metadata, plus resource-specific business rules with cross-field constraints. Zero external dependencies.

## Usage

```typescript
import { validateRecord, validateBusinessRules, getBusinessRules } from '@reso/validation';
import type { ResoField, ValidationFailure } from '@reso/validation';

// Validate a record against field metadata
const failures = validateRecord(record, fields);

// Validate resource-specific business rules
const ruleFailures = validateBusinessRules('Property', record);

// Get the rule definitions for a resource
const rules = getBusinessRules('Property');
```

## API

### `validateRecord(body, fields): ValidationFailure[]`

Validates a record payload against RESO field metadata. Checks:

- **Unknown fields** — fields not in metadata
- **Type mismatches** — Edm.String, Edm.Boolean, Edm.Int32, Edm.Decimal, Edm.Date, Edm.DateTimeOffset, etc.
- **Negative numerics** — rejects negative values for numeric types
- **MaxLength** — strings exceeding `field.maxLength`
- **Integer enforcement** — Int types must be whole numbers
- **Collection fields** — must be arrays
- **Enum fields** — must be string or number
- **Business rules** — delegates to `validateBusinessRules()` for range/relationship checks

Null, undefined, and empty string values are silently skipped unless the field is required. Fields starting with `@` (OData annotations) are ignored.

### `validateBusinessRules(resourceName, body): ValidationFailure[]`

Validates resource-specific constraints:

**Property:**
- Required: City, StateOrProvince, PostalCode, Country
- Price fields (0 to $1B): ListPrice, OriginalListPrice, PreviousListPrice, ClosePrice, ListPriceLow
- Room counts (0 to 100): BedroomsTotal, BathroomsFull, BathroomsHalf, etc.
- Cross-field: `ListPrice >= ListPriceLow`
- Cross-field: `BathroomsTotalInteger = sum(BathroomsFull + BathroomsHalf + ...)`

**Member:** Required MemberCity, MemberStateOrProvince, MemberPostalCode, MemberCountry

**Office:** Required OfficeCity, OfficeStateOrProvince, OfficePostalCode, OfficeCountry

### `getBusinessRules(resourceName): FieldRule[]`

Returns the per-field constraint definitions for a resource.

### Type Helpers

- `isEnumType(type)` — true if type is an enum reference (not a primitive Edm type)
- `isNumericEdmType(type)` — true for Edm.Decimal, Edm.Int32, Edm.Double, etc.
- `isIntegerEdmType(type)` — true for Edm.Int16, Edm.Int32, Edm.Int64, Edm.Byte

## Types

```typescript
interface ValidationFailure {
  readonly field: string;
  readonly reason: string;
}

interface FieldRule {
  readonly fieldName: string;
  readonly required?: boolean;
  readonly min?: number;
  readonly max?: number;
  readonly message?: string;
}

interface CrossFieldRule {
  readonly name: string;
  readonly validate: (body: Record<string, unknown>) => ValidationFailure | null;
}
```

## Integration

Used by both the reference server (request body validation on POST/PATCH) and the React UI (client-side form validation). Also used by `@reso/certification-test-runner` for payload validation in compliance testing.
