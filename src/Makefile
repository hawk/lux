# Copyright 2012-2019 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ../include.mk
include ../vsn.mk

VSN=$(LUX_VSN)

APP_DIR=$(shell dirname $(PWD))

ERL_FILES=$(shell ls *erl)
EBIN_FILES=$(ERL_FILES:%.erl=../ebin/%.$(EMULATOR)) ../ebin/lux.app

all debug: $(EBIN_FILES)

$(EBIN_FILES): lux.hrl

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

info:
	@echo "VSN=$(VSN)"
	@echo "ERL_MODULES=$(ERL_MODULES)"
	@echo "ERL_FILES=$(ERL_FILES)"
	@echo "EBIN_FILES=$(EBIN_FILES)"
	@echo "APP_DIR=$(APP_DIR)"
