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
RUN cat > /etc/nginx/conf.d/default.conf << 'EOF'
server {
    listen 80;

    # API proxy to the reference server
    location ~ ^/(Property|Member|Office|Media|OpenHouse|Showing|ui-config|field-groups|api|admin|health|images|oauth|\$metadata) {
        proxy_pass http://server:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # SPA fallback — serve index.html for all other routes
    location / {
        root /usr/share/nginx/html;
        index index.html;
        try_files $uri $uri/ /index.html;
    }
}
EOF

EXPOSE 80
