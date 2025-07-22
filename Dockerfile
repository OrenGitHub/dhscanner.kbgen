FROM haskell:9.4.8
WORKDIR /kbgen
COPY dhscanner.cabal dhscanner.cabal
RUN cabal update
RUN cabal build --only-dependencies
COPY src src
RUN cabal build
CMD ["cabal", "run"]