# Copyright 2012-2017 Tail-f Systems AB
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

HTML=../lux.html
MD_TMP=lux.md
GEN_TOC=toc
GEN_DBG_CMDS=debug_cmds
APP_DIR=$(shell dirname $(PWD))

CHAPTERS= \
	../README \
	main_concepts \
	script_syntax \
	cmd_line_opts \
	config_params \
	logs \
	$(GEN_DBG_CMDS) \
	examples \
	../INSTALL \
	../AUTHORS \
	references

MARKDOWNS=$(patsubst %,%.md,$(CHAPTERS))

all doc: $(HTML)

clean:
	rm -f $(HTML) $(HTML).tmp $(MD_TMP) $(GEN_TOC).md $(GEN_TOC).md.tmp
	rm -f $(GEN_DBG_CMDS).md $(GEN_DBG_CMDS).md.tmp

$(HTML): \
    html.header \
    $(MARKDOWNS) \
    $(GEN_TOC).md \
    html.footer \
    ../include.mk \
    Makefile
	-rm -f $(MD_TMP) $(HTML).tmp
	cat html.header > $(HTML).tmp
	cat $(GEN_TOC).md >> $(MD_TMP)
	@for f in $(CHAPTERS); do \
		/bin/echo "<a name=\"$$f\"/>" >> $(MD_TMP); \
		/bin/echo >> $(MD_TMP); \
		cat $$f.md >> $(MD_TMP); \
	done
	$(MARKDOWN) $(MD_TMP) >> $(HTML).tmp
	cat html.footer >> $(HTML).tmp
	mv $(HTML).tmp $(HTML)
	@/bin/echo
	@/bin/echo "Read the documentation in the browser:"
	@/bin/echo
	@/bin/echo "file://$(PWD)/$(HTML)"

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

$(GEN_DBG_CMDS).md: ../ebin/lux_debug.beam Makefile
	$(APP_DIR)/bin/lux --markdown > $@.tmp
	mv $@.tmp $@
