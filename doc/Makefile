include ../include.mk
include ../vsn.mk

# Generate an HTML file from all markdown files
# and generate a table of contents with links
# to each chapter.
#
# See http://www.daringfireball.net/projects/markdown

HTML=../lux.html
HTML_TMP=$(HTML).tmp
TMP_MD=tmp.md
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
	rm -f $(HTML)$(HTML_TMP) $(TMP_MD) $(GEN_DBG_CMDS).md

$(HTML): \
    html.header \
    $(MARKDOWNS) \
    html.footer \
    ../include.mk \
    Makefile
	-rm -f $(TMP_MD) $(HTML_TMP)
	cat html.header > $(HTML_TMP)
	echo -n "<h3>Version $(LUX_VSN) - " >> $(HTML_TMP)
	date "+%F" >> $(HTML_TMP)
	echo "</h3>\n" >> $(HTML_TMP)
	echo "<ol>">> $(HTML_TMP)
	@for f in $(CHAPTERS); do \
		echo "<li><a href=\"#$$f\">" >> $(HTML_TMP); \
		head -1 $$f.md >> $(HTML_TMP); \
		echo "</a></li>" >> $(HTML_TMP); \
		echo >> $(HTML_TMP); \
		echo "<a name=\"$$f\"/>" >> $(TMP_MD); \
		echo >> $(TMP_MD); \
		cat $$f.md >> $(TMP_MD); \
	done
	echo "</ol>">> $(HTML_TMP)
	$(MARKDOWN) $(TMP_MD) >> $(HTML_TMP)
	cat html.footer >> $(HTML_TMP)
	mv $(HTML_TMP) $(HTML)
	echo "\nRead the documentation in the browser:\n\n\tfile://$(PWD)/$(HTML)"

$(GEN_DBG_CMDS).md: ../ebin/lux_debug.beam Makefile
	lux --markdown > $@
