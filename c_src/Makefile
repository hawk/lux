include ../include.mk

PROG = ../priv/bin/runpty

all: $(PROG)

$(PROG): runpty.c
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $<

clean:
	rm -f $(PROG) core* *~

install:

