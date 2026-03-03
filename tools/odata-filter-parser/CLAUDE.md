# Project Core Principles
- **Style:** Functional & Declarative.
- **TypeScript:** ESM (`import`/`export`), strict mode enabled.
- **Goal:** Zero-dependency OData $filter parser producing a typed AST.

# Coding Standards
- **Paradigm:** Use functional programming patterns (`map`, `filter`, `reduce`, `flatMap`).
- **No Classes:** Prefer functions, pure functions, and data objects. Use `interface` and `type` for data shapes.
- **Immutability:** Use `const` always; avoid `let` and `var`. Do not mutate objects/arrays. Prefer `Readonly<T>` and `ReadonlyArray<T>` in type signatures where practical.
- **Naming:** camelCase for variables/functions, PascalCase for types/interfaces.
- **Style**: Always use arrow functions.

# TypeScript
- Compose small, pure functions. Avoid shared mutable state.
- Use explicit return types on exported functions.
- Prefer `unknown` over `any`. Use type narrowing and type guards instead of type assertions.
- Use `.js` extensions in import paths (required for Node16 module resolution with ESM).

# Commands
- Build: `npm run build`
- Test: `npm test`

# Prohibitions
- DO NOT use classes or `this`.
- DO NOT use `any`. Use `unknown` and narrow with type guards.
- DO NOT add runtime dependencies. This package must have zero runtime deps.
