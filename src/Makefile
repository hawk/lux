# Copyright 2012-2019 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ../include.mk
include ../vsn.mk
VSN=$(LUX_VSN)

ERL_MODULES = \
	lux \
	lux_case \
	lux_debug \
	lux_diff \
	lux_html_annotate \
	lux_html_history \
	lux_html_parse \
	lux_html_utils \
	lux_interpret \
	lux_junit \
	lux_log \
	lux_suite \
	lux_tap \
	lux_trace \
	lux_parse \
	lux_shell \
	lux_utils

ERL_FILES=$(ERL_MODULES:%=%.erl)
EBIN_FILES=$(ERL_MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/lux.app
APP_DIR=$(shell dirname $(PWD))

all debug: $(EBIN_FILES)

$(EBIN_FILES): lux.hrl

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

# Standalone/local installation
install: $(INSTALL_TARGET)

install_standalone:
	echo "Install standalone"
	mkdir -p $(DESTDIR)
	$(APP_DIR)/bin/lux --install $(DESTDIR)
	if [ "$(ETCDIR)" != "$(DESTDIR)/priv" ]; then \
	  echo "# Added by Lux installer"      >> $(DESTDIR)/lib/lux-$(VSN)/priv/luxcfg ; \
	  echo "[config config_dir=$(ETCDIR)]" >> $(DESTDIR)/lib/lux-$(VSN)/priv/luxcfg ; \
	fi

install_local:
	echo "Install local"
	mkdir -p $(DESTDIR)
	(cd .. && tar cf - *) | (cd $(DESTDIR) && tar xf -)
	if [ "$(ETCDIR)" != "$(DESTDIR)/priv" ]; then \
	  echo "# Added by Lux installer"      >> $(DESTDIR)/priv/luxcfg ; \
	  echo "[config config_dir=$(ETCDIR)]" >> $(DESTDIR)/priv/luxcfg ; \
	fi
