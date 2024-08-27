# Copyright 2012-2024 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ./include.mk

ifdef LUX_SELF_TEST
LUX_EXTRAS += test
endif

SUBDIRS = src $(C_SRC_TARGET) $(LUX_EXTRAS)
DIALYZER_PLT = .dialyzer_plt
DIALYZER_LOG = dialyzer.log

LUXCFG=$(DESTDIR)$(TARGETDIR)/priv/luxcfg

### BUILD ###

.PHONY: all debug clean config_clean doc

all debug clean:
	@for d in $(SUBDIRS); do \
	   if test ! -d $$d ; then \
	       echo "=== Skipping subdir $$d" ; \
	   else \
	      $(MAKE) -C $$d $@ || exit $?; \
	   fi ; \
	done

config_clean:
	$(MAKE) clean
	-rm -rf configure include.mk autom4te.cache config.status config.log *~

doc:
	$(MAKE) -C doc all

### TEST ###

.PHONY: test test_clean xref dialyzer dialyzer_clean

test:
	$(MAKE) -C test all

test_clean:
	rm -rf lux_logs ecover_logs html_logs
	$(MAKE) -C test clean

.PHONY: ecover_logs html_logs

cover: ecover_logs html_logs

ecover_logs:
	rm -rf ecover_logs
	bin/ecover \
		-beam ebin/\*.beam \
		-outdir ecover_logs \
		-ecover_var LUX_COVER_NODE \
		-collect $(MAKE) test < /dev/null

html_logs:
	rm -rf html_logs
	bin/ecover \
		-outdir html_logs \
		-datadir ecover_logs \
		-datadir test/lux_logs/latest_runs \
		-html < /dev/null
	echo "open html_logs/ecover.html"

xref:
	bin/lux --xref

dialyzer: $(DIALYZER_PLT)
	dialyzer --src src --src bin --plt $(DIALYZER_PLT) \
		| tee $(DIALYZER_LOG)

$(DIALYZER_PLT):
	dialyzer --build_plt --output_plt $(DIALYZER_PLT) \
		--apps erts kernel stdlib runtime_tools xmerl inets \
		tools reltool et wx

dialyzer_clean:
	rm -f $(DIALYZER_PLT) $(DIALYZER_LOG)

### INSTALL ###

.PHONY: install info

install:
	@### TOP
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)
	$(INSTALL_DATA) \
		LICENSE AUTHORS.md README.md INSTALL.md lux.html \
			$(DESTDIR)$(TARGETDIR)

	@### BIN
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/bin
	$(INSTALL_PGM) bin/lux $(DESTDIR)$(TARGETDIR)/bin
	$(INSTALL_DIR) $(DESTDIR)$(BINDIR)
	ln -s $(TARGETDIR)/bin/lux $(DESTDIR)$(BINDIR)/lux

	@### PRIV
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/priv
	$(INSTALL_DATA) priv/filter_trace  priv/luxcfg \
		$(DESTDIR)$(TARGETDIR)/priv
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/priv/bin
	$(INSTALL_PGM) priv/bin/runpty $(DESTDIR)$(TARGETDIR)/priv/bin

	echo "# Added by Lux installer"          >> $(LUXCFG)
	echo "[config config_dir=$(SYSCONFDIR)]" >> $(LUXCFG)
	$(INSTALL_DIR) $(DESTDIR)$(SYSCONFDIR)

	@### EMACS
	$(call INSTALL_DIR_AND_DATA,emacs)

	@### EBIN
	$(call INSTALL_DIR_AND_DATA,ebin)

	@### EXAMPLES
	$(call INSTALL_DIR_AND_DATA,examples)

	@### TUTORIAL
	$(call INSTALL_DIR_AND_DATA,tutorial)

	@### SUMMARY
	@echo
	@echo "*** Lux doc, examples, Emacs mode etc. \
		is installed at $(DESTDIR)$(TARGETDIR)"
	@echo "*** Lux executable is $(DESTDIR)$(BINDIR)/lux"
	@echo "*** Lux default config_dir is $(DESTDIR)$(SYSCONFDIR)"

info:
	@echo "DESTDIR=$(DESTDIR)"
	@echo "PREFIX=$(PREFIX) -> $(DESTDIR)$(PREFIX)"
	@echo "EXEC_PREFIX=$(EXEC_PREFIX) -> $(DESTDIR)$(EXEC_PREFIX)"
	@echo "BINDIR=$(BINDIR) -> $(DESTDIR)$(BINDIR)"
	@echo "SYSCONFDIR=$(SYSCONFDIR) -> $(DESTDIR)$(SYSCONFDIR)"
	@echo "TARGETDIR=$(TARGETDIR) -> $(DESTDIR)$(TARGETDIR)"
	@echo "LUXCFG=$(LUXCFG)"
	@echo
	@echo "LUX_SELF_TEST=$(LUX_SELF_TEST)"
	@echo "LUX_EXTRAS=$(LUX_EXTRAS)"
	@echo "SUBDIRS=$(SUBDIRS)"
