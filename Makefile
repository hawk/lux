# Copyright 2012-2015 Tail-f Systems AB
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ./include.mk

SUBDIRS = src $(C_SRC_TARGET)

all install clean: Makefile
	@for d in $(SUBDIRS); do         \
	   if test ! -d $$d ; then        \
	       echo "=== Skipping subdir $$d" ; \
	   else                   \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ;                        \
	done

config_clean:
	$(MAKE) clean
	-rm -rf configure include.mk autom4te.cache config.status config.log rm -f *~
