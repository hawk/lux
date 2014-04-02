include ../include.mk
include ../vsn.mk
VSN=$(LUX_VSN)

ERL_MODULES = \
	lux \
	lux_debug \
	lux_html \
	lux_interpret \
	lux_log \
	lux_suite \
	lux_parse \
	lux_shell \
	lux_utils

ERL_FILES=$(ERL_MODULES:%=%.erl)
EBIN_FILES=$(ERL_MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/lux.app
LUXCFG=$(DESTDIR)/lib/lux-$(VSN)/priv/luxcfg
APP_DIR=$(shell dirname $(PWD))

all: $(EBIN_FILES)

$(EBIN_FILES): lux.hrl

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

install:
	mkdir -p $(DESTDIR)
	$(APP_DIR)/bin/lux --install $(DESTDIR)
	if [ "$(ETCDIR)" != "$(DESTDIR)/priv" ]; then \
	  echo "# Added by Lux installer" >> $(LUXCFG); \
	  echo "[config config_dir=$(ETCDIR)]" >> $(LUXCFG); \
	fi
