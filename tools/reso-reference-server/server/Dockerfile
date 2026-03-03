# Build stage
#
# NOTE: Build context should be the parent directory (reso-reference-server/)
# or the tools/ directory so that the file: dependency on odata-filter-parser
# can be resolved. When using docker-compose from reso-reference-server/,
# the build context is set to ./server and the filter-parser is copied in.
#
# For standalone Docker builds from the tools/ directory:
#   docker build -f reso-reference-server/server/Dockerfile .

FROM node:22-alpine AS build
WORKDIR /app

# Copy the filter parser dependency first (file: reference)
COPY odata-filter-parser/ /deps/odata-filter-parser/

# Copy server source
COPY reso-reference-server/server/package.json reso-reference-server/server/package-lock.json* ./

# Rewrite the file: dependency path for the Docker build context
RUN sed -i 's|file:../../odata-filter-parser|file:/deps/odata-filter-parser|' package.json

RUN npm ci --ignore-scripts || npm install
COPY reso-reference-server/server/tsconfig.json ./
COPY reso-reference-server/server/src/ ./src/
RUN npm run build

# Runtime stage
FROM node:22-alpine
WORKDIR /app
COPY --from=build /app/dist ./dist
COPY --from=build /app/node_modules ./node_modules
COPY --from=build /deps/odata-filter-parser /deps/odata-filter-parser
COPY reso-reference-server/server/package.json ./
COPY reso-reference-server/server/server-metadata.json ./

ENV PORT=8080
ENV METADATA_PATH=./server-metadata.json

EXPOSE 8080

CMD ["node", "dist/index.js"]
