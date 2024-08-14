# ##[cl,pname]            -*- makefile-gmake -*-
# GNUmakefile

DISPLAY = short
DLDCREPO = /data/web/dldc/opam
DUNE = opam exec -- dune $1 --display $(DISPLAY)

build all::
	$(call DUNE, build @@default)
.PHONY: build all

install: build
	$(call DUNE, install)
.PHONY: install

doc::
	$(call DUNE, build @doc)
.PHONY: doc

clean::
	$(call DUNE, clean)
.PHONY: clean

sandbox::
	opam switch create . --deps-only --with-test --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
PHONY: sandbox

deps::
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
PHONY: deps

publish: build
	scp spinup.opam $${OPAMFILE_HOSTNAME}:$(REMOTE_OPAMFILE_PATH)/opam
	ssh $(OPAMFILE_HOSTNAME) env MAKEFLAGS=$(MAKEFLAGS) gmake -C $(DLDCREPO) update NAME=spinup OPAM=$(REMOTE_OPAMFILE_PATH)/opam
	ssh $(OPAMFILE_HOSTNAME) rm opam

# Local Variables:
# mode: makefile-gmake
# End:


