# ##[cl,pname]            -*- makefile-gmake -*-
# GNUmakefile

DISPLAY = short
DLDCREPO = /data/web/dldc/opam
DUNE = opam exec -- dune $1 --display $(DISPLAY)

build all:
	$(call DUNE, build @@default)
.PHONY: build all

install: build
	$(call DUNE, install)
.PHONY: install

doc:
	$(call DUNE, build @doc)
.PHONY: doc

clean:
	$(call DUNE, clean)
.PHONY: clean

sandbox:
	opam switch create . --deps-only --with-test --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
.PHONY: sandbox

deps:
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
.PHONY: deps

home-install:
	eval $$(opam env)
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam update --yes
	opam upgrade --yes
	opam install spinup
	install -m 555 $(OPAM_SWITCH_PREFIX)/bin/spinup ~/bin
.PHONY: home-install

dune-install: build
	eval $$(opam env)
	$(call DUNE, install)
.PHONY: install

dev-install: dune-install
	install -m 555 $(OPAM_SWITCH_PREFIX)/bin/spinup ~/bin
.PHONY: home-install

dev-uninstall: dune-install
	rm ~/bin/spinup || true
.PHONY: home-install

mounts:
	if mountpoint /data/web 2> /dev/null; then : ; else sudo mkdir -p /data/web && sudo mount voldemort:/export/www-legacy /data/web ; fi
.PHONY: mounts

publish: build mounts
	echo 'url { src:' $$(cat spinup.opam | grep dev-repo | awk '{ print $$2 }') '}' >> spinup.opam
	make -C $(DLDCREPO) add NAME=spinup OPAM=$$PWD/spinup.opam
.PHONY: publish

# Local Variables:
# mode: makefile-gmake
# End:


