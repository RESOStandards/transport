/** A single error detail entry in an OData error response. */
export interface ODataErrorDetail {
  readonly code: string;
  readonly target: string;
  readonly message: string;
}

/** The full OData error response body structure. */
export interface ODataErrorBody {
  readonly error: {
    readonly code: string;
    readonly message: string;
    readonly details: ReadonlyArray<ODataErrorDetail>;
  };
}

/** Builds an OData-compliant error response body. */
export const buildODataError = (
  code: string,
  message: string,
  details: ReadonlyArray<{ readonly target: string; readonly message: string }>,
): ODataErrorBody => ({
  error: {
    code,
    message,
    details: details.map((d) => ({
      code: "30212",
      target: d.target,
      message: d.message,
    })),
  },
});

/** Builds a validation error response for fields that failed validation. */
export const buildValidationError = (
  failures: ReadonlyArray<{ readonly field: string; readonly reason: string }>,
): ODataErrorBody =>
  buildODataError(
    "20100",
    "Validation failed",
    failures.map((f) => ({ target: f.field, message: f.reason })),
  );
