#!/bin/bash
shopt -s globstar
shopt -s nullglob

# work around brittany not recognising pattern synonyms
sed -i -e 's/^pattern /pattern_/g' p4p-*/**/*.hs
brittany "$@" --write-mode=inplace --omit-output-check p4p-*/**/*.hs
# work around brittany not recognising GADTs properly (#261, #79)
sed -i -e 's/::\([A-Z!(]\)/:: \1/g' p4p-*/**/*.hs
# work around brittany weirdly-formatting DataKinds
sed -i -e "s/ ( '/ ('/g" p4p-*/**/*.hs
sed -i -e 's/^pattern_/pattern /g' p4p-*/**/*.hs

stylish-haskell -i p4p-*/**/*.hs

hlint -h hlint.yml p4p-*
