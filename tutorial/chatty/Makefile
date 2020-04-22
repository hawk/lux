SUBDIRS=src

.PHONY: all build test history clean info

all build clean:
	@for d in $(SUBDIRS); do \
	   if [ -f $$d/Makefile ] ; then \
	      $(MAKE) -C $$d $@ || exit $?; \
	   fi; \
	done

test:
	cd test && $(MAKE) build test

history:
	cd test && $(MAKE) history

info:
	@echo "SUBDIRS=$(SUBDIRS)"
