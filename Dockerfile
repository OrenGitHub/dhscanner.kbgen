FROM haskell:9.8.1
RUN cabal update
RUN apt-get update
RUN apt-get install vim -y
RUN echo "set number" > ~/.vimrc
RUN echo "set incsearch" >> ~/.vimrc
RUN echo "syntax on" >> ~/.vimrc
WORKDIR /kbgen
COPY dhscanner.cabal dhscanner.cabal
RUN cabal build --only-dependencies
COPY dhscanner.ast dhscanner.ast
COPY dhscanner.bitcode dhscanner.bitcode
COPY src src
RUN cabal build
CMD ["cabal", "run"]