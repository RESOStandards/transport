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
- **HTTP:** Use the native `fetch` API (Node 18+). Check `response.ok` before reading the body.
- **Linting and Formatting:** No eslint/prettier config exists yet. If added, follow the project config.
- **Style**: Always use arrow functions.

# TypeScript
- Compose small, pure functions. Avoid shared mutable state.
- Use explicit return types on exported functions.
- Prefer `unknown` over `any`. Use type narrowing and type guards instead of type assertions.
- Use `.js` extensions in import paths (required for Node16 module resolution with ESM).

# Directory Structure
- `/src/lib` — Core SDK: types, metadata parser, HTTP client, validators, test runner, reporter.
- `/src/cli` — CLI entry point using commander.
- `/src/mock` — Express mock OData server for local testing.
- `/tests` — Vitest tests.
- `/sample-payloads` — Example JSON payload files for testing.

# Commands
- Build: `npm run build`
- Test: `npm test`
- Dev: `npm run dev`
- Lint: `npm run lint`

# Prohibitions
- DO NOT use classes or `this`.
- Prefer `map`, `filter`, `reduce`; `for...of` / `for await...of` is allowed when sequential async processing requires it.
- Avoid `console.log` in library code (`src/lib`) — only use it in `src/cli` and `src/mock`.
- DO NOT use `any`. Use `unknown` and narrow with type guards.
