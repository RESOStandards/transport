FROM node:22-alpine AS build

WORKDIR /app

# Copy workspace dependencies (same resolution chain as the server Dockerfile)
COPY odata-filter-parser/ /deps/odata-filter-parser/
COPY validation/ /deps/validation/
COPY odata-client/ /deps/odata-client/
COPY certification/test-runner/ /deps/test-runner/

# Build odata-filter-parser
WORKDIR /deps/odata-filter-parser
RUN npm install && npm run build

# Build validation
WORKDIR /deps/validation
RUN npm install && npm run build

# Build odata-client (depends on filter-parser + validation)
WORKDIR /deps/odata-client
RUN rm -f package-lock.json \
 && sed -i 's|file:../odata-filter-parser|file:/deps/odata-filter-parser|' package.json \
 && sed -i 's|file:../validation|file:/deps/validation|' package.json \
 && npm install && npm run build

# Build test-runner (depends on odata-client + validation)
WORKDIR /deps/test-runner
RUN rm -f package-lock.json \
 && sed -i 's|file:../../odata-client|file:/deps/odata-client|' package.json \
 && sed -i 's|file:../../validation|file:/deps/validation|' package.json \
 && npm install && npm run build

# Build add-edit (depends on test-runner + odata-client + validation)
WORKDIR /app
COPY certification/add-edit/package.json ./
RUN sed -i 's|file:../test-runner|file:/deps/test-runner|' package.json \
 && sed -i 's|file:../../odata-client|file:/deps/odata-client|' package.json \
 && sed -i 's|file:../../validation|file:/deps/validation|' package.json \
 && npm install --ignore-scripts

COPY certification/add-edit/tsconfig.json ./
COPY certification/add-edit/src/ ./src/
RUN npm run build

# Copy sample payloads
COPY certification/add-edit/sample-payloads/ ./sample-payloads/

# --- Runtime stage ---
FROM node:22-alpine

WORKDIR /app

COPY --from=build /app/dist ./dist
COPY --from=build /app/node_modules ./node_modules
COPY --from=build /app/package.json ./
COPY --from=build /app/sample-payloads ./sample-payloads
COPY --from=build /deps/odata-filter-parser /deps/odata-filter-parser
COPY --from=build /deps/validation /deps/validation
COPY --from=build /deps/odata-client /deps/odata-client
COPY --from=build /deps/test-runner /deps/test-runner

# Install wget for health checks in entrypoint
RUN apk add --no-cache wget

# Run as non-root user
RUN addgroup -S reso && adduser -S reso -G reso
USER reso

ENTRYPOINT ["node", "dist/cli/index.js"]
