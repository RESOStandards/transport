/**
 * OData URI builder — functional chainable API inspired by Apache Olingo's
 * URIBuilder. Constructs OData URLs with entity key syntax and system query
 * options ($filter, $select, $orderby, $top, $skip, $count, $expand,
 * $search, $compute, $format).
 *
 * Supports single and compound keys per OData 4.01 URL Conventions §4.3.
 *
 * @see https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html
 *
 * @example
 * ```ts
 * const url = buildUri("http://localhost:8080", "Property")
 *   .key("ABC123")
 *   .select("ListPrice", "City")
 *   .filter("ListPrice gt 200000")
 *   .expand("Media")
 *   .orderby("ListPrice desc")
 *   .top(10)
 *   .build();
 * ```
 */

/** Intermediate builder state. */
interface UriBuilderState {
  readonly baseUrl: string;
  readonly resource: string;
  readonly keyValue?: string;
  readonly compoundKey?: Readonly<Record<string, string>>;
  readonly selectFields?: ReadonlyArray<string>;
  readonly filterExpr?: string;
  readonly orderbyExpr?: string;
  readonly topValue?: number;
  readonly skipValue?: number;
  readonly countValue?: boolean;
  readonly expandExpr?: string;
  readonly searchExpr?: string;
  readonly computeExpr?: string;
  readonly formatValue?: string;
}

/** Chainable URI builder object. */
export interface UriBuilder {
  /** Set simple entity key: `/{Resource}('{key}')`. */
  key: (value: string) => UriBuilder;
  /** Set compound entity key: `/{Resource}(Key1='v1',Key2='v2')`. */
  compoundKey: (keys: Readonly<Record<string, string>>) => UriBuilder;
  /** Add $select query option. */
  select: (...fields: ReadonlyArray<string>) => UriBuilder;
  /** Add $filter query option (raw OData filter expression). */
  filter: (expr: string) => UriBuilder;
  /** Add $orderby query option. */
  orderby: (expr: string) => UriBuilder;
  /** Add $top query option. */
  top: (n: number) => UriBuilder;
  /** Add $skip query option. */
  skip: (n: number) => UriBuilder;
  /** Add $count=true query option. */
  count: (value?: boolean) => UriBuilder;
  /** Add $expand query option for navigation properties. */
  expand: (expr: string) => UriBuilder;
  /** Add $search query option (free-text search). */
  search: (expr: string) => UriBuilder;
  /** Add $compute query option (computed properties). */
  compute: (expr: string) => UriBuilder;
  /** Add $format query option (response format). */
  format: (value: string) => UriBuilder;
  /** Build the final URL string. */
  build: () => string;
}

const createBuilder = (state: UriBuilderState): UriBuilder => ({
  key: (value: string) => createBuilder({ ...state, keyValue: value }),

  compoundKey: (keys: Readonly<Record<string, string>>) =>
    createBuilder({ ...state, compoundKey: keys }),

  select: (...fields: ReadonlyArray<string>) =>
    createBuilder({ ...state, selectFields: fields }),

  filter: (expr: string) => createBuilder({ ...state, filterExpr: expr }),

  orderby: (expr: string) => createBuilder({ ...state, orderbyExpr: expr }),

  top: (n: number) => createBuilder({ ...state, topValue: n }),

  skip: (n: number) => createBuilder({ ...state, skipValue: n }),

  count: (value = true) => createBuilder({ ...state, countValue: value }),

  expand: (expr: string) => createBuilder({ ...state, expandExpr: expr }),

  search: (expr: string) => createBuilder({ ...state, searchExpr: expr }),

  compute: (expr: string) => createBuilder({ ...state, computeExpr: expr }),

  format: (value: string) => createBuilder({ ...state, formatValue: value }),

  build: () => {
    const base = state.baseUrl.replace(/\/$/, "");
    let path = `${base}/${state.resource}`;

    if (state.compoundKey !== undefined) {
      const keyParts = Object.entries(state.compoundKey)
        .map(([k, v]) => `${k}='${encodeURIComponent(v)}'`)
        .join(",");
      path += `(${keyParts})`;
    } else if (state.keyValue !== undefined) {
      path += `('${encodeURIComponent(state.keyValue)}')`;
    }

    const params: string[] = [];

    if (state.selectFields && state.selectFields.length > 0) {
      params.push(`$select=${state.selectFields.join(",")}`);
    }
    if (state.filterExpr) {
      params.push(`$filter=${encodeURIComponent(state.filterExpr)}`);
    }
    if (state.orderbyExpr) {
      params.push(`$orderby=${encodeURIComponent(state.orderbyExpr)}`);
    }
    if (state.expandExpr) {
      params.push(`$expand=${encodeURIComponent(state.expandExpr)}`);
    }
    if (state.searchExpr) {
      params.push(`$search=${encodeURIComponent(state.searchExpr)}`);
    }
    if (state.computeExpr) {
      params.push(`$compute=${encodeURIComponent(state.computeExpr)}`);
    }
    if (state.topValue !== undefined) {
      params.push(`$top=${state.topValue}`);
    }
    if (state.skipValue !== undefined) {
      params.push(`$skip=${state.skipValue}`);
    }
    if (state.countValue !== undefined) {
      params.push(`$count=${state.countValue}`);
    }
    if (state.formatValue) {
      params.push(`$format=${encodeURIComponent(state.formatValue)}`);
    }

    return params.length > 0 ? `${path}?${params.join("&")}` : path;
  },
});

/**
 * Create an OData URI builder for the given base URL and resource name.
 */
export const buildUri = (baseUrl: string, resource: string): UriBuilder =>
  createBuilder({ baseUrl, resource });
