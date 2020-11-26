#!/bin/bash
shopt -s globstar
shopt -s nullglob
targets="${targets-p4p-*/**/*.hs}"
brittany_flags="--write-mode=inplace --omit-output-check"
hs_targets="$(echo $(eval echo $targets | xargs -rn1 | grep '.hs$'))"
test -n "$hs_targets" || exit 0

# work around brittany not recognising pattern synonyms
eval sed -i -e "'s/^pattern /pattern_/g'" "$hs_targets"
eval brittany "\"\$@\"" $brittany_flags "$hs_targets"
# work around brittany not recognising GADTs properly (#261, #79)
eval sed -i -e "'s/::\([A-Z!\(]\)/:: \1/g'" "$hs_targets"
# work around brittany weirdly-formatting DataKinds
eval sed -i -e "\"s/ \([(@]\) '/ \1'/g\"" "$hs_targets"
eval sed -i -e "'s/^pattern_/pattern /g'" "$hs_targets"

eval stylish-haskell -i "$hs_targets"

eval hlint -h hlint.yml "$hs_targets"
