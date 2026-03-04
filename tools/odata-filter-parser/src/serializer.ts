/**
 * OData $filter AST serializer.
 *
 * Converts a FilterExpression AST back into a canonical OData $filter string.
 * This is the inverse of parseFilter() and enables round-trip fidelity.
 */

import type { CollectionExpr, FilterExpression } from './types.js';

/** Serialize a FilterExpression AST back to an OData $filter string. */
export const astToFilterString = (expr: FilterExpression): string => {
  switch (expr.type) {
    case 'comparison': {
      const left = astToFilterString(expr.left);
      if (expr.operator === 'in') {
        const items = (expr.right as CollectionExpr).items.map(astToFilterString).join(', ');
        return `${left} in (${items})`;
      }
      return `${left} ${expr.operator} ${astToFilterString(expr.right)}`;
    }
    case 'logical':
      return `${astToFilterString(expr.left)} ${expr.operator} ${astToFilterString(expr.right)}`;
    case 'not':
      return `not (${astToFilterString(expr.operand)})`;
    case 'arithmetic':
      return `${astToFilterString(expr.left)} ${expr.operator} ${astToFilterString(expr.right)}`;
    case 'function':
      return `${expr.name}(${expr.args.map(astToFilterString).join(',')})`;
    case 'lambda': {
      const source = astToFilterString(expr.source);
      if (!expr.variable) return `${source}/${expr.operator}()`;
      return `${source}/${expr.operator}(${expr.variable}:${astToFilterString(expr.predicate)})`;
    }
    case 'literal': {
      if (expr.value === null) return 'null';
      if (expr.dataType === 'boolean') return String(expr.value);
      if (expr.dataType === 'number') return String(expr.value);
      if (expr.dataType === 'string') return `'${String(expr.value).replace(/'/g, "''")}'`;
      if (expr.dataType === 'duration') return `duration'${expr.value}'`;
      if (expr.dataType === 'enum') return String(expr.value);
      // date, datetimeoffset, timeofday, guid — bare values
      return String(expr.value);
    }
    case 'property':
      return expr.name;
    case 'collection':
      return expr.items.map(astToFilterString).join(', ');
  }
};
