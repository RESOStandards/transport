# Stage 1: Build
FROM node:22-alpine AS build
WORKDIR /app

# Copy file: dependencies
COPY validation/ /deps/validation/
COPY odata-filter-parser/ /deps/odata-filter-parser/

COPY reso-reference-server/ui/package.json reso-reference-server/ui/package-lock.json* ./

# Rewrite file: dependency paths for the Docker build context
RUN sed -i 's|file:../../validation|file:/deps/validation|' package.json && \
    sed -i 's|file:../../odata-filter-parser|file:/deps/odata-filter-parser|' package.json

RUN npm ci --ignore-scripts || npm install

COPY reso-reference-server/ui/ ./
RUN npm run build

# Stage 2: Serve with nginx
FROM nginx:alpine
COPY --from=build /app/dist /usr/share/nginx/html

# nginx config template — API_UPSTREAM is substituted at container startup
# by the nginx entrypoint's envsubst mechanism (20-envsubst-on-templates.sh).
# Default: "server:8080" (matches the default docker-compose service name).
# Override via environment variable to point at a different backend service.
ENV API_UPSTREAM=server:8080

COPY reso-reference-server/ui/nginx.conf.template /etc/nginx/templates/default.conf.template

EXPOSE 80
