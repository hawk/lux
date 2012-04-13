include ../include.mk
include ../vsn.mk

# Generate an HTML file from all markdown files
# and generate a table of contents with links
# to each chapter.
#
# See http://www.daringfireball.net/projects/markdown

HTML=../lux.html
HTML_TMP=$(HTML).tmp
MD_TMP=lux.md
GEN_TOC=toc
GEN_DBG_CMDS=debug_cmds

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
	rm -f $(HTML) $(HTML_TMP) $(MD_TMP) $(GEN_DBG_CMDS).md

$(HTML): \
    html.header \
    $(MARKDOWNS) \
    $(GEN_TOC).md \
    html.footer \
    ../include.mk \
    Makefile
	-rm -f $(MD_TMP) $(HTML_TMP)
	cat html.header > $(HTML_TMP)
	cat $(GEN_TOC).md >> $(MD_TMP)
	@for f in $(CHAPTERS); do \
		echo "<a name=\"$$f\"/>" >> $(MD_TMP); \
		echo >> $(MD_TMP); \
		cat $$f.md >> $(MD_TMP); \
	done
	$(MARKDOWN) $(MD_TMP) >> $(HTML_TMP)
	cat html.footer >> $(HTML_TMP)
	mv $(HTML_TMP) $(HTML)
	echo "\nRead the documentation in the browser:\n\n\tfile://$(PWD)/$(HTML)"

$(GEN_TOC).md: Makefile
	rm -f $(GEN_TOC).md
	echo "Lux - LUcid eXpect scripting" > $(GEN_TOC).md
	echo "============================" >> $(GEN_TOC).md
	echo -n "Version $(LUX_VSN) - " >> $(GEN_TOC).md
	date "+%F" >> $(GEN_TOC).md
	echo >> $(GEN_TOC).md
	@for f in $(CHAPTERS); do \
		echo -n "* [" >> $(GEN_TOC).md; \
		head -1 "$$f.md" | tr -d '\n' >> $(GEN_TOC).md; \
		echo -n "]" >> $(GEN_TOC).md; \
		echo "(#$$f)" >> $(GEN_TOC).md; \
	done
	echo >> $(GEN_TOC).md

$(GEN_DBG_CMDS).md: ../ebin/lux_debug.beam Makefile
	lux --markdown > $@
