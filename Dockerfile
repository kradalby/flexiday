FROM codesimple/elm:0.18 as elm-builder
WORKDIR /app

# RUN apt-get update \
#         && apt-get install zlib1g-dev libncurses-dev -y \
#         && rm -rf /var/lib/apt/lists/*

COPY . .

RUN elm package install -y
RUN elm make src/Main.elm --output elm.js

FROM node:16 as builder
WORKDIR /app

COPY . .
COPY --from=elm-builder /app/elm.js /app/src/elm.js

RUN yarn
RUN yarn parcel build src/index.html

FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
