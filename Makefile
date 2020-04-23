# Copyright 2012-2020 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ./include.mk

ifdef LUX_SELF_TEST
LUX_EXTRAS += test
endif

SUBDIRS = src $(C_SRC_TARGET) $(LUX_EXTRAS)

LUXCFG=$(DESTDIR)$(TARGETDIR)/priv/luxcfg

.PHONY: all debug clean xref doc test test_clean config_clean install
all debug clean:
	@for d in $(SUBDIRS); do \
	   if test ! -d $$d ; then \
	       echo "=== Skipping subdir $$d" ; \
	   else \
	      $(MAKE) -C $$d $@ || exit $?; \
	   fi ; \
	done

xref:
	bin/lux --xref

doc:
	cd doc && $(MAKE) all

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
