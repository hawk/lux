include ../include.mk
include ../vsn.mk
VSN=$(LUX_VSN)

ERL_MODULES = \
	lux \
	lux_debug \
	lux_html \
	lux_interpret \
	lux_suite \
	lux_parse \
	lux_shell \
	lux_utils

ERL_FILES=$(ERL_MODULES:%=%.erl)
EBIN_FILES=$(ERL_MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/lux.app
LUXCFG=$(DESTDIR)/lib/lux-$(VSN)/priv/luxcfg

all: $(EBIN_FILES)

$(EBIN_FILES): lux.hrl

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

install:
	mkdir -p $(DESTDIR)
	lux --install $(DESTDIR)
	if [ "$(ETCDIR)" != "$(DESTDIR)/etc" ]; then \
	  echo "# Added by Lux installer" >> $(LUXCFG); \
	  echo "[config config_dir=$(ETCDIR)]" >> $(LUXCFG); \
	fi
