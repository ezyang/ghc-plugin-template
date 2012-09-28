#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: ./rename.sh NewName"
    exit 1
fi

CAMEL="$1"
LOWER="${1,,}"

sed --in-place -e "s/template/$LOWER/g" -e "s/Template/$CAMEL/g" ghc-plugin-template.cabal
git mv ghc-plugin-template.cabal "ghc-plugin-$LOWER.cabal"
sed --in-place -e "s/Template/$CAMEL/g" GhcPlugins/Template.hs GhcPlugins/Template/*.hs tests/Makefile
git mv GhcPlugins/Template.hs "GhcPlugins/$CAMEL.hs"
git mv GhcPlugins/Template "GhcPlugins/$CAMEL"
