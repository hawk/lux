# Copyright 2012-2019 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ./include.mk

ifdef LUX_SELF_TEST
LUX_EXTRAS += test
endif

SUBDIRS = src $(C_SRC_TARGET) $(LUX_EXTRAS)

all debug clean:
	@for d in $(SUBDIRS); do         \
	   if test ! -d $$d ; then        \
	       echo "=== Skipping subdir $$d" ; \
	   else                   \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ;                        \
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
	$(INSTALL_DIR) $(TARGETDIR)
	$(INSTALL_DATA) LICENSE AUTHORS.md README.md INSTALL.md lux.html $(TARGETDIR)

	@### BIN
	$(INSTALL_DIR) $(TARGETDIR)/bin
	$(INSTALL_PGM) bin/lux $(TARGETDIR)/bin
	$(INSTALL_DIR) $(BINDIR)
	ln -s $(TARGETDIR)/bin/lux $(BINDIR)/lux

	@### PRIV
	$(INSTALL_DIR) $(TARGETDIR)/priv
	$(INSTALL_DATA) priv/filter_trace  priv/luxcfg $(TARGETDIR)/priv
	$(INSTALL_DIR) $(TARGETDIR)/priv/bin
	$(INSTALL_PGM) priv/bin/runpty $(TARGETDIR)/priv/bin
	echo "# Added by Lux installer"      >> $(TARGETDIR)/priv/luxcfg
	echo "[config config_dir=$(SYSCONFDIR)]" >> $(TARGETDIR)/priv/luxcfg

	@### EMACS
	$(INSTALL_DIR) $(TARGETDIR)/emacs
	$(INSTALL_DATA) emacs/lux-mode.el $(TARGETDIR)/emacs

	@### EBIN
	(cd src && $(MAKE) $@)

	@### EXAMPLES
	$(INSTALL_DIR) $(TARGETDIR)/examples
	cd examples && $(INSTALL_DATA) \
		calc.lux fail.lux intro.lux loop.lux loop_fail.lux \
		require.lux skip.lux unstable.lux warning.lux \
			$(TARGETDIR)/examples

	@### SUMMARY
	@echo
	@echo "*** Lux doc, examples, Emacs mode etc. is installed at $(TARGETDIR)"
	@echo "*** Lux executable is $(BINDIR)/lux"
	@echo "*** Lux default config_dir is $(SYSCONFDIR)"

info:
	@echo "DESTDIR=$(DESTDIR)"
	@echo "REALDESTDIR=$(REALDESTDIR)"
	@echo "PREFIX=$(PREFIX)"
	@echo "EXEC_PREFIX=$(EXEC_PREFIX)"
	@echo "BINDIR=$(BINDIR)"
	@echo "SYSCONFDIR=$(SYSCONFDIR)"
	@echo "TARGETDIR=$(TARGETDIR)"
	@echo
	@echo "LUX_SELF_TEST=$(LUX_SELF_TEST)"
	@echo "LUX_EXTRAS=$(LUX_EXTRAS)"
	@echo "SUBDIRS=$(SUBDIRS)"
