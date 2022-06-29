from haskell:9.0.2-buster

RUN mkdir /app

WORKDIR /app

COPY stack.yaml /app/
COPY stack.yaml.lock /app/
COPY src /app/src
COPY app /app/app
COPY test-common /app/test-common
COPY test-unit /app/test-unit
COPY test-e2e /app/test-e2e
COPY package.yaml /app/
COPY wow-stream.cabal /app/
RUN env
RUN stack build
COPY Makefile /app/
CMD make test-unit