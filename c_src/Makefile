# Copyright 2012-2017 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ../include.mk

ifeq ($(DEBUG), true)
CFLAGS = -g -ggdb -Wall -DDEBUG
endif

PROG = ../priv/bin/runpty

all: $(PROG)

debug:
	$(MAKE) DEBUG=true

$(PROG): runpty.c
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $<

clean:
	rm -rf $(PROG) ../priv/bin/runpty.dSYM core* *~

install:
