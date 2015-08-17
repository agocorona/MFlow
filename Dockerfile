FROM biscarch/ghc-7.8.3
MAINTAINER Alberto G. Corona "agocorona@gmail.com"
RUN apt-get update
RUN apt-get install -y git
RUN apt-get install -y libbz2-dev zlib1g-dev
RUN cabal update
ENV LANG en_US.UTF8
RUN cabal install cpphs
RUN cabal install monadloc-pp
git clone https://github.com/agocorona/MFlow
RUN   cd MFlow \
      && cabal install

CMD cd MFlow && ./dist/build/demos-blaze/demos-blaze