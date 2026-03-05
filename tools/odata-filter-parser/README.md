# @reso/odata-filter-parser

Standalone, zero-dependency library for parsing OData 4.01 `$filter` expressions into a typed AST (abstract syntax tree). Used by both [`@reso/odata-client`](../odata-client/) for query validation and [`@reso/reference-server`](../reso-reference-server/) for SQL WHERE clause generation.

## Install

```bash
npm install @reso/odata-filter-parser
```

## Usage

```typescript
import { parseFilter } from "@reso/odata-filter-parser";

const ast = parseFilter("ListPrice gt 200000 and contains(City, 'Austin')");

console.log(ast);
// {
//   type: "logical",
//   operator: "and",
//   left: {
//     type: "comparison",
//     operator: "gt",
//     left: { type: "property", name: "ListPrice" },
//     right: { type: "literal", value: 200000, dataType: "number" }
//   },
//   right: {
//     type: "function",
//     name: "contains",
//     args: [
//       { type: "property", name: "City" },
//       { type: "literal", value: "Austin", dataType: "string" }
//     ]
//   }
// }
```

## Supported Features

### Comparison Operators

`eq`, `ne`, `gt`, `ge`, `lt`, `le`, `has`, `in`

```
ListPrice gt 200000
City eq 'Austin'
StandardStatus has 'Active'
City in ('Austin', 'Dallas', 'Houston')
```

### Logical Operators

`and`, `or`, `not`

```
ListPrice gt 200000 and City eq 'Austin'
not contains(City, 'Test')
(ListPrice gt 100000 or BedroomsTotal ge 3) and City eq 'Austin'
```

### Arithmetic Operators

`add`, `sub`, `mul`, `div`, `mod`, `divby`

```
ListPrice add 1000 gt 300000
ListPrice divby 1000 lt 500
```

### String Functions

`contains`, `startswith`, `endswith`, `length`, `indexof`, `substring`, `tolower`, `toupper`, `trim`, `concat`, `matchesPattern`

```
contains(City, 'Aus')
startswith(PostalCode, '787')
tolower(City) eq 'austin'
length(City) gt 5
concat(City, ', TX')
```

### Date/Time Functions

`year`, `month`, `day`, `hour`, `minute`, `second`, `fractionalseconds`, `totalseconds`, `date`, `time`, `totaloffsetminutes`, `now`, `maxdatetime`, `mindatetime`

```
year(ModificationTimestamp) eq 2024
month(CloseDate) ge 6
```

### Math Functions

`round`, `floor`, `ceiling`

```
round(ListPrice) eq 250000
floor(Latitude) eq 30
```

### Type Functions

`cast`, `isof`

### Lambda Operators

`any`, `all` with variable binding

```
Rooms/any(r: r/Area gt 200)
Tags/all(t: t eq 'luxury')
```

### Literal Types

- Strings: `'Austin'`
- Numbers: `200000`, `3.14`
- Booleans: `true`, `false`
- Null: `null`
- Dates: `2024-01-15`
- DateTimeOffset: `2024-01-15T10:30:00Z`
- TimeOfDay: `10:30:00`
- Duration: `duration'PT12H30M'`
- GUID: `01234567-89ab-cdef-0123-456789abcdef`
- Enum: `Namespace.EnumType'Value'`

## AST Node Types

The parser produces a `FilterExpression` discriminated union:

| Node Type | Description |
|-----------|-------------|
| `ComparisonExpr` | Binary comparison (`eq`, `ne`, `gt`, etc.) |
| `LogicalExpr` | Binary logical (`and`, `or`) |
| `NotExpr` | Unary logical negation |
| `ArithmeticExpr` | Binary arithmetic (`add`, `sub`, etc.) |
| `FunctionCallExpr` | Built-in function call |
| `LambdaExpr` | Lambda expression (`any`/`all`) |
| `LiteralExpr` | Literal value with data type |
| `PropertyExpr` | Property path reference |
| `CollectionExpr` | Collection of expressions (for `in` operator) |

## AST Serializer

Convert an AST back to a canonical OData `$filter` string with `astToFilterString`:

```typescript
import { parseFilter, astToFilterString } from '@reso/odata-filter-parser';

const ast = parseFilter("ListPrice gt 200000 and contains(City, 'Austin')");
const roundTripped = astToFilterString(ast);
// → "ListPrice gt 200000 and contains(City, 'Austin')"
```

Handles all node types: comparison, logical, not, arithmetic, function, lambda, literal, property, and collection.

## Development

```bash
npm install
npm run build
npm test        # 152 tests
```

## License

See [LICENSE](../../License.txt) in the repository root.
