BEAMS=$(patsubst %.erl,../ebin/%.beam,$(wildcard *.erl))

.PHONY: all build test clean info

../ebin/%.beam: %.erl
	erlc -W -o ../ebin $<

all build: $(BEAMS)

clean:
	rm -f $(BEAMS) *~

info:
	@echo "BEAMS=$(BEAMS)"
