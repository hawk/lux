LUXDIRS=$(filter-out .,$(shell lux --mode=list_dir *))

.PHONY: all build test history clean info

all build test history:
	@for d in $(LUXDIRS); do \
	   if test -f $$d/Makefile ; then \
	      $(MAKE) -C $$d $@ || exit $?; \
	   fi; \
	done

clean:
	rm -rf lux_logs lux_history* *~
	@for d in $(LUXDIRS); do \
	   if test -f $$d/Makefile ; then \
	      (cd $$d && $(MAKE) $@) ; \
	   fi; \
	done
info:
	@echo "LUXDIRS=$(LUXDIRS)"
