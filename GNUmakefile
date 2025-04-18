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

# mounts:
# 	if mountpoint /data/web 2> /dev/null; then echo hi; else echo bye; fi
# .PHONY: mounts

# publish: build
# 	scp spinup.opam $${OPAMFILE_HOSTNAME}:$(REMOTE_OPAMFILE_PATH)/opam
# 	ssh $(OPAMFILE_HOSTNAME) env MAKEFLAGS=$(MAKEFLAGS) gmake -C $(DLDCREPO) update NAME=spinup OPAM=$(REMOTE_OPAMFILE_PATH)/opam
# 	ssh $(OPAMFILE_HOSTNAME) rm opam
# .PHONY: publish

# Local Variables:
# mode: makefile-gmake
# End:


