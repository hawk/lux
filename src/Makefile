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


install:
	$(INSTALL_DIR) $(TARGETDIR)/ebin
	$(INSTALL_DATA) $(EBIN_FILES) $(TARGETDIR)/ebin

info:
	@echo "VSN=$(VSN)"
	@echo "ERL_MODULES=$(ERL_MODULES)"
	@echo "ERL_FILES=$(ERL_FILES)"
	@echo "EBIN_FILES=$(EBIN_FILES)"
	@echo "APP_DIR=$(APP_DIR)"
	@echo "TARGETDIR=$(TARGETDIR)"
