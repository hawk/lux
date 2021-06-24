.PHONY: all build test history clean info

TIMESTAMP=$(shell date +"%F_%T")
GITHASH=$(shell git rev-parse --verify --short HEAD)
TOPDIR=$(shell pwd | sed -e 's/tutorial.*/tutorial/')

export PATH:=$(TOPDIR)/../bin:$(PATH)

all: build test

build:

test:
	lux .

history:
	lux --history . lux_logs
	@echo
	@echo open lux_history.html

clean:
	rm -rf lux_logs history_demo*_logs erl_crash.dump *~

info:
	@echo "TOPDIR=$(TOPDIR)"
	@echo "TIMESTAMP=$(TIMESTAMP)"
	@echo "GITHASH=$(GITHASH)"

############################################################
# Internal history demo targets
############################################################

.PHONY: history_demo history_demo_single history_demo_multi_host history_demo_multi_branch history_demo_success history_demo_warning history_demo_empty

history_demo: history_demo_single history_demo_multi_host history_demo_multi_branch history_demo_success history_demo_warning history_demo_empty
	ls -1 history_demo_*/lux_history.html | sed -e 's/^/open /g'

history_demo_single:
	rm -rf ${@}; \
	for i in 1 2 3 4 5 6 7 8 9; do \
	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc  --log_dir=${@}/run_logs/run_sunny_$$i .; \
	done; \
	lux --history ${@} ${@}/run_logs

history_demo_multi_host:
	rm -rf ${@}; \
	for i in 1 2 3 4 5 6 7 8 9; do \
	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc   --log_dir=${@}/run_logs/run_sunny_$$i  .; \
	  lux $$opts --hostname=cloudy --config_name=SunOS-i86pc   --log_dir=${@}/run_logs/run_cloudy_$$i .; \
	  lux $$opts --hostname=netty  --config_name=NetBSD-macppc --log_dir=${@}/run_logs/run_netty_$$i  .; \
	done; \
	lux --history ${@} ${@}/run_logs

history_demo_multi_branch:
	rm -rf ${@}; \
	branches="chatty-1.0 chatty-2.0"; \
	histargs=${@}; \
	for b in $$branches; do \
	  for i in 1 2 3 4 5 6 7 8 9; do \
	    opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
	    lux $$opts --hostname=sunny  --config_name=SunOS-i86pc    --log_dir=${@}/run_logs/$$b/run_sunny_$$i  .; \
	    lux $$opts --hostname=cloudy --config_name=SunOS-i86pc    --log_dir=${@}/run_logs/$$b/run_cloudy_$$i .; \
	    lux $$opts --hostname=netty  --config_name=NetBSD-macppc  --log_dir=${@}/run_logs/$$b/run_netty_$$i  .; \
	  done; \
	  histargs="$$histargs $$b:::${@}/run_logs/$$b"; \
	done; \
	lux --history $$histargs

history_demo_success:
	rm -rf ${@}; \
	for i in 1 2 3 4 5 6 7 8 9; do \
	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc  --log_dir=${@}/run_logs/run_sunny_$$i success.lux; \
	done; \
	lux --history ${@} ${@}/run_logs

history_demo_warning:
	rm -rf ${@}; \
	for i in 1 2 3 4 5 6 7 8 9; do \
	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc  --log_dir=${@}/run_logs/run_sunny_$$i success.lux warning.lux; \
	done; \
	lux --history ${@} ${@}/run_logs

history_demo_empty:
	rm -rf ${@}; \
	lux --history ${@} ${@}/run_logs
