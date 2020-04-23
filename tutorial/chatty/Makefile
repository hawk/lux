SUBDIRS=src test

.PHONY: all build test history clean info

all build test history clean:
	@for d in $(SUBDIRS); do \
	   if [ -f $$d/Makefile ] ; then \
	      $(MAKE) -C $$d $@ || exit $?; \
	   fi; \
	done

info:
	@echo "SUBDIRS=$(SUBDIRS)"
