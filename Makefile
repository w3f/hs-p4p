SRC = $(wildcard *.hp)
DST_SVG = $(SRC:%.hp=%.svg)
DST_PNG = $(SRC:%.hp=%.png)

all: all-300
clean: clean-300

all-full: $(DST_SVG) $(DST_PNG)
clean-full:
	rm -f $(DST_SVG) $(DST_PNG)

# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#viewing-the-heap-profile-of-a-running-program
make_tail_hp = \
cat "$<" | \
head -n"$$(($$(fgrep -n BEGIN_SAMPLE "$*.hp" | head -n1    |            cut -d: -f1) - 1))" \
	>  "$@" && \
cat "$<" | \
head     -n"$$(fgrep -n   END_SAMPLE "$*.hp" | tail -n1    |            cut -d: -f1)" | \
tail    -n+"$$(fgrep -n BEGIN_SAMPLE "$*.hp" | tail -n$(1) | head -n1 | cut -d: -f1)" \
	>> "$@"

make_head_hp = \
cat "$<" | \
head     -n"$$(fgrep -n   END_SAMPLE "$*.hp" | head -n$(1) | tail -n1 | cut -d: -f1)" \
	>> "$@"

DST_3600_PNG = $(SRC:%.hp=%-head-3600.png) $(SRC:%.hp=%-tail-3600.png)
DST_3600_SVG = $(SRC:%.hp=%-head-3600.svg) $(SRC:%.hp=%-tail-3600.svg)
all-3600: $(DST_3600_SVG) $(DST_3600_PNG)
%-tail-3600.hp: %.hp
	$(call make_tail_hp,3600)
%-head-3600.hp: %.hp
	$(call make_head_hp,3600)
clean-3600:
	rm -f $(DST_3600_SVG) $(DST_3600_PNG)

DST_300_PNG = $(SRC:%.hp=%-head-300.png) $(SRC:%.hp=%-tail-300.png)
DST_300_SVG = $(SRC:%.hp=%-head-300.svg) $(SRC:%.hp=%-tail-300.svg)
all-300: $(DST_300_SVG) $(DST_300_PNG)
%-tail-300.hp: %.hp
	$(call make_tail_hp,300)
%-head-300.hp: %.hp
	$(call make_head_hp,300)
clean-300:
	rm -f $(DST_300_SVG) $(DST_300_PNG)

%.svg: %.hp
	hp2pretty "$<"

%.png: %.svg
	convert "$<" "$@"
