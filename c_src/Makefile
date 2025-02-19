# Copyright 2012-2024 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ../include.mk

ifeq ($(DEBUG), true)
CFLAGS = -g -ggdb -Wall -DDEBUG
endif

ifeq ($(TRACE), true)
CFLAGS = -g -ggdb -Wall -DTRACE
endif

PROG = ../priv/bin/runpty

.PHONY: all debug trace clean info

all: $(PROG)

debug:
	$(MAKE) DEBUG=true

trace:
	$(MAKE) TRACE=true

$(PROG): runpty.c
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $<
ifeq ($(STRIP_RUNPTY), true)
	strip $(PROG)
	strip --remove-section=.note.gnu.build-id $(PROG)
endif

clean:
	rm -rf $(PROG) ../priv/bin/runpty.dSYM core* *~

info:
	@echo "DEBUG=$(DEBUG)"
	@echo "TRACE=$(TRACE)"
	@echo "CFLAGS=$(CFLAGS)"
	@echo "PROG=$(PROG)"
