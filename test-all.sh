#!/bin/bash -ex

cabal clean
cabal configure --enable-tests --constraint 'monad-control < 0.3'
cabal build
cabal test

cabal clean
cabal configure --enable-tests --constraint 'monad-control > 0.3'
cabal build
cabal test
