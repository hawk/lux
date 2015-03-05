include ../include.mk

PROG = ../priv/bin/runpty

all: $(PROG)

$(PROG): runpty.c
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $<

clean:
	rm -rf $(PROG) ../priv/bin/runpty.dSYM core* *~

install:
