# Stage 1: Build
FROM node:22-alpine AS build
WORKDIR /app

# Copy file: dependency
COPY validation/ /deps/validation/

COPY reso-reference-server/ui/package.json reso-reference-server/ui/package-lock.json* ./

# Rewrite file: dependency path for the Docker build context
RUN sed -i 's|file:../../validation|file:/deps/validation|' package.json

RUN npm ci --ignore-scripts || npm install

COPY reso-reference-server/ui/ ./
RUN npm run build

# Stage 2: Serve with nginx
FROM nginx:alpine
COPY --from=build /app/dist /usr/share/nginx/html

# nginx config to proxy API requests to the server container and handle SPA routing
#
# Routing strategy: the UI client sends "Accept: application/json" on all API
# calls, while browser navigation sends "Accept: text/html". We use this header
# to distinguish OData API requests from SPA page loads on the same paths.
RUN cat > /etc/nginx/conf.d/default.conf << 'EOF'
server {
    listen 80;

    # Named location for proxying to the OData server
    location @api {
        internal;
        proxy_pass http://server:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Non-resource API endpoints — always proxy
    location ~ ^/(ui-config|field-groups|api|admin|health|images|oauth|\$metadata) {
        proxy_pass http://server:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # OData entity requests with key syntax — always proxy
    # Matches: /Property('key'), /Property(%28'key'%29)
    location ~ ^/(Property|Member|Office|Media|OpenHouse|Showing)(\(|%28) {
        proxy_pass http://server:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Bare resource collection paths — proxy if Accept: application/json, SPA otherwise
    # Uses error_page + return trick to safely branch inside nginx (avoids if + proxy_pass)
    location ~ ^/(Property|Member|Office|Media|OpenHouse|Showing)$ {
        error_page 418 = @api;
        if ($http_accept ~* "application/json") {
            return 418;
        }
        root /usr/share/nginx/html;
        try_files /index.html =404;
    }

    # SPA fallback — serve index.html for all other routes
    # Handles: /Property/key, /Property/edit/key, /Property/add, etc.
    location / {
        root /usr/share/nginx/html;
        index index.html;
        try_files $uri $uri/ /index.html;
    }
}
EOF

EXPOSE 80
