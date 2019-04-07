# Copyright 2012-2019 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ./include.mk

ifdef LUX_SELF_TEST
LUX_EXTRAS += test
endif

SUBDIRS = src $(C_SRC_TARGET) $(LUX_EXTRAS)

ERL_FILES=$(shell ls src/*erl)
EBIN_FILES=$(ERL_FILES:src/%.erl=ebin/%.$(EMULATOR)) ebin/lux.app

EXAMPLES=$(shell ls examples/*lux)

all debug clean:
	@for d in $(SUBDIRS); do \
	   if test ! -d $$d ; then \
	       echo "=== Skipping subdir $$d" ; \
	   else \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ; \
	done

xref:
	bin/lux --xref

.PHONY: test
test:
	cd test && $(MAKE) all

test_clean:
	cd test && $(MAKE) clean

config_clean:
	$(MAKE) clean
	-rm -rf configure include.mk autom4te.cache config.status config.log *~

install:
	@### TOP
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)
	$(INSTALL_DATA) LICENSE AUTHORS.md README.md INSTALL.md lux.html $(DESTDIR)$(TARGETDIR)

	@### BIN
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/bin
	$(INSTALL_PGM) bin/lux $(DESTDIR)$(TARGETDIR)/bin
	$(INSTALL_DIR) $(DESTDIR)$(BINDIR)
	ln -s $(TARGETDIR)/bin/lux $(DESTDIR)$(BINDIR)/lux

	@### PRIV
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/priv
	$(INSTALL_DATA) priv/filter_trace  priv/luxcfg $(DESTDIR)$(TARGETDIR)/priv
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/priv/bin
	$(INSTALL_PGM) priv/bin/runpty $(DESTDIR)$(TARGETDIR)/priv/bin
	echo "# Added by Lux installer"      >> $(DESTDIR)$(TARGETDIR)/priv/luxcfg
	echo "[config config_dir=$(SYSCONFDIR)]" >> $(DESTDIR)$(TARGETDIR)/priv/luxcfg
	$(INSTALL_DIR) $(DESTDIR)$(SYSCONFDIR)

	@### EMACS
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/emacs
	$(INSTALL_DATA) emacs/lux-mode.el $(DESTDIR)$(TARGETDIR)/emacs

	@### EBIN
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/ebin
	$(INSTALL_DATA) $(EBIN_FILES) $(DESTDIR)$(TARGETDIR)/ebin

	@### EXAMPLES
	$(INSTALL_DIR) $(DESTDIR)$(TARGETDIR)/examples
	$(INSTALL_DATA) $(EXAMPLES) $(DESTDIR)$(TARGETDIR)/examples

	@### SUMMARY
	@echo
	@echo "*** Lux doc, examples, Emacs mode etc. is installed at $(DESTDIR)$(TARGETDIR)"
	@echo "*** Lux executable is $(DESTDIR)$(BINDIR)/lux"
	@echo "*** Lux default config_dir is $(DESTDIR)$(SYSCONFDIR)"

info:
	@echo "DESTDIR=$(DESTDIR)"
	@echo "PREFIX=$(PREFIX) -> $(DESTDIR)$(PREFIX)"
	@echo "EXEC_PREFIX=$(EXEC_PREFIX) -> $(DESTDIR)$(EXEC_PREFIX)"
	@echo "BINDIR=$(BINDIR) -> $(DESTDIR)$(BINDIR)"
	@echo "SYSCONFDIR=$(SYSCONFDIR) -> $(DESTDIR)$(SYSCONFDIR)"
	@echo "TARGETDIR=$(TARGETDIR) -> $(DESTDIR)$(TARGETDIR)"
	@echo
	@echo "LUX_SELF_TEST=$(LUX_SELF_TEST)"
	@echo "LUX_EXTRAS=$(LUX_EXTRAS)"
	@echo "SUBDIRS=$(SUBDIRS)"
	@echo "EXAMPLES=$(EXAMPLES)"
	@echo "ERL_FILES=$(ERL_FILES)"
	@echo "EBIN_FILES=$(EBIN_FILES)"
