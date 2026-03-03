# Project Core Principles
- **Style:** Functional & Declarative.
- **TypeScript:** ESM (`import`/`export`), strict mode enabled.
- **Goal:** Expressive, testable, immutable code.

# Coding Standards
- **Paradigm:** Use functional programming patterns (`map`, `filter`, `reduce`, `flatMap`).
- **No Classes:** Prefer functions, pure functions, and data objects. Use `interface` and `type` for data shapes.
- **Immutability:** Use `const` always; avoid `let` and `var`. Do not mutate objects/arrays. Use `Object.freeze()` when appropriate — note that it is shallow, so deep-clone nested objects before mutation. Prefer `Readonly<T>` and `ReadonlyArray<T>` in type signatures where practical.
- **Naming:** camelCase for variables/functions, PascalCase for types/interfaces, snake_case for directories.
- **Async:** Use `async/await`.
- **HTTP:** Use the native `fetch` API (Node 22+) for outbound requests. Express for inbound HTTP handling.
- **Database:** Use raw parameterized `pg` queries — no ORM. Query builders return `{ text, values }`.
- **Style**: Always use arrow functions.

# TypeScript
- Compose small, pure functions. Avoid shared mutable state.
- Use explicit return types on exported functions.
- Prefer `unknown` over `any`. Use type narrowing and type guards instead of type assertions.
- Use `.js` extensions in import paths (required for Node16 module resolution with ESM).

# Directory Structure (server/)
- `/src/metadata` — RESO JSON metadata types, loader, EDMX/OpenAPI generators.
- `/src/db` — PostgreSQL pool, schema generator, migration runner, CRUD query builders.
- `/src/odata` — OData route handlers, response headers/annotations/errors, request validation.
- `/src/auth` — Mock OAuth2 token endpoint.
- `/src/docs` — Swagger UI middleware.
- `/tests` — Vitest tests.

# Commands (run from server/)
- Build: `npm run build`
- Test: `npm test`
- Dev: `npm run dev`
- Start: `npm start`
- Seed: `npm run seed`

# Prohibitions
- DO NOT use classes or `this`.
- Prefer `map`, `filter`, `reduce`; `for...of` / `for await...of` is allowed when sequential async processing requires it.
- DO NOT use `any`. Use `unknown` and narrow with type guards.
