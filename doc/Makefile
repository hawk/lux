# Copyright 2012-2021 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ../include.mk
include ../vsn.mk

# Generate an HTML file from all markdown files
# and generate a table of contents with links
# to each chapter.
#
# See http://www.daringfireball.net/projects/markdown

LUX_HTML=../lux.html
LUX_MD=lux.md
GEN_TOC=toc
GEN_DBG_CMDS=debug_cmds

CHAPTERS= \
	../README \
	main_concepts \
	../tutorial/README \
	script_syntax \
	cmd_line_opts \
	config_params \
	logs \
	$(GEN_DBG_CMDS) \
	examples \
	hardening \
	warnings \
	../INSTALL \
	../AUTHORS \
	references

MARKDOWNS=$(patsubst %,%.md,$(CHAPTERS))
WILDCARD=$(patsubst %,%.src,$(MARKDOWNS))
SRC_MD=$(wildcard $(WILDCARD))
GEN_MD=$(patsubst %.md.src,%.md,$(SRC_MD))
DEPS_MD=$(patsubst %.src,%.d,$(SRC_MD))

all doc: $(LUX_HTML)

clean:
	rm -f $(LUX_HTML) $(LUX_HTML).tmp $(LUX_MD)
	rm -f $(GEN_TOC).md $(GEN_TOC).md.tmp
	rm -f $(GEN_MD) $(DEPS_MD)

$(LUX_HTML): \
    html.header \
    $(MARKDOWNS) \
    $(GEN_TOC).md \
    html.footer \
    ../include.mk \
    Makefile
	-rm -f $(LUX_MD) $(LUX_HTML).tmp
	cat html.header > $(LUX_HTML).tmp
	cat $(GEN_TOC).md >> $(LUX_MD)
	@for f in $(CHAPTERS); do \
		/bin/echo "<a name=\"$$f\"/>" >> $(LUX_MD); \
		/bin/echo >> $(LUX_MD); \
		cat $$f.md >> $(LUX_MD); \
	done
	$(MARKDOWN) $(LUX_MD) >> $(LUX_HTML).tmp
	cat html.footer >> $(LUX_HTML).tmp
	mv $(LUX_HTML).tmp $(LUX_HTML)
	@/bin/echo
	@/bin/echo "Read the documentation in the browser:"
	@/bin/echo
	@/bin/echo "file://$(PWD)/$(LUX_HTML)"

$(GEN_TOC).md: Makefile $(MARKDOWNS) ../vsn.mk
	rm -f $@ $@.tmp
	/bin/echo "Lux - LUcid eXpect scripting" > $@.tmp
	/bin/echo "============================" >> $@.tmp
	/bin/echo "" >> $@.tmp
	/bin/echo -n "Version $(LUX_VSN) - " >> $@.tmp
	date "+%F" >> $@.tmp
	/bin/echo >> $@.tmp
	/bin/echo $(CHAPTERS)
	@for f in $(CHAPTERS); do \
		/bin/echo -n "* [" >> $@.tmp; \
		head -1 "$$f.md" | tr -d '\n' >> $@.tmp; \
		/bin/echo -n "]" >> $@.tmp; \
		/bin/echo "(#$$f)" >> $@.tmp; \
	done
	/bin/echo >> $@.tmp
	mv $@.tmp $@

$(GEN_DBG_CMDS).md.src: ../ebin/lux_debug.beam
	$(LUX) --gen_markdown=$@

%.md: %.md.src
	$(LUX) --pre_markdown=$@

%.md.d: %.md.src
	grep -IE '#include +<.*>' $< | sed -e 's#.*<#../#g' -e 's/>.*//g' > $@
