SRC = $(wildcard *.hp)

all: all-head-3600 all-tail-3600
all-head-3600: $(SRC:%.hp=%-head-3600.svg)
all-tail-3600: $(SRC:%.hp=%-tail-3600.svg)

all-full: $(SRC:%.hp=%.svg)
all-full-png: $(SRC:%.hp=%.png)

# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#viewing-the-heap-profile-of-a-running-program
%-tail-3600.hp: %.hp
	cat "$<" | \
	head -n"$$(($$(fgrep -n BEGIN_SAMPLE "$*.hp" | head -n1    |            cut -d: -f1) - 1))" \
	  >  "$@"
	cat "$<" | \
	head     -n"$$(fgrep -n   END_SAMPLE "$*.hp" | tail -n1    |            cut -d: -f1)" | \
	tail    -n+"$$(fgrep -n BEGIN_SAMPLE "$*.hp" | tail -n3600 | head -n1 | cut -d: -f1)" \
	  >> "$@"

%-head-3600.hp: %.hp
	cat "$<" | \
	head     -n"$$(fgrep -n   END_SAMPLE "$*.hp" | head -n3600 | tail -n1 | cut -d: -f1)" \
	  >> "$@"

%.svg: %.hp
	hp2pretty "$<"

%.png: %.svg
	convert "$<" "$@"
