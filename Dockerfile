FROM haskell:8.4.4


ADD . .

RUN apt-get update
RUN apt-get install postgresql postgresql-contrib postgresql-server-dev-9.6 -y
RUN cabal new-update
RUN cabal new-build all
