SHELL         := /usr/bin/env bash
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox
HC_PKG        ?= cabal sandbox ghc-pkg
MODULES       := notifier-apns notifier-apns-io notifier-server

default: all

all: clean init install

init:
	cabal sandbox --sandbox=$(CABAL_SANDBOX) init

.PHONY: install
install: init
	cabal install \
	--enable-tests --enable-bench --enable-documentation \
	--avoid-reinstalls -j $(addprefix ./,$(MODULES))

.PHONY: clean
clean: $(addprefix clean-,$(MODULES))

clean-%:
	(cd $*; cabal clean)

.PHONY: compile
compile: configure $(addprefix compile-,$(MODULES))

compile-%:
	(cd $*; cabal build)

.PHONY: configure
configure: $(addprefix configure-,$(MODULES))

configure-%:
	(cd $*; cabal configure)

.PHONY: dist
dist: install $(addprefix dist-,$(MODULES))

dist-%:
	(cd $*; cabal sdist)

.PHONY: doc
doc: install $(addprefix doc-,$(MODULES))
