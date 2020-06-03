#!/bin/bash
# Copies the common stanzas from common.cabal.in into the other projects.

header='-- begin common stanzas'
footer='-- end common stanzas'

for f in */*.cabal; do
sed -i "$f" -e "/^$header/,/^$footer/c\\"'
'"COMMON_STANZAS"
sed -i "$f" -e '/^COMMON_STANZAS/{rcommon.cabal.in
;d}'
done
