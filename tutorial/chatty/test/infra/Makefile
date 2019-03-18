.PHONY: all build test history clean history_demo info

TIMESTAMP=$(shell date +"%F_%T")
GITHASH=$(shell git rev-parse --verify --short HEAD)
TOPDIR=$(shell pwd | sed -e 's/tutorial.*/tutorial/')
LUXOPTS=\
	--config_dir=$(TOPDIR)/support/luxcfg \
	--revision=$(TIMESTAMP)_$(GITHASH)

all: build test

build:

test:
	lux $(LUX_OPTS) .

history:
	lux --history . .

clean:
	rm -rf lux_logs lux_history* chatty_*.log erl_crash.dump *~

history_demo:
	for i in 0 1 2 3 4 5 6 7 8 9; do \
		rev="2019-05-14_21:5$$i:04_$(GITHASH)$$i" ; \
		opts="--revision=$$rev --suite=demo --config_dir=$(TOPDIR)/support/luxcfg" ; \
		lux $$opts --hostname=sunny --config_name=SunOS-i86pc .; \
		lux $$opts --hostname=netty --config_name=NetBSD-macppc .; \
		sleep 1; \
	done

info:
	@echo "TOPDIR=$(TOPDIR)"
	@echo "TIMESTAMP=$(TIMESTAMP)"
	@echo "GITHASH=$(GITHASH)"
	@echo "LUXOPTS=$(LUXOPTS)"
