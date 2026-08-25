# ##[cl,pname]            -*- makefile-gmake -*-
# GNUmakefile

PNAME = ##[cl,pname]
DISPLAY = short
DLDCREPO = /data/web/dldc/opam
DUNE = opam exec -- dune $1 --display $(DISPLAY)

build all:
	$(call DUNE, build @@default)
.PHONY: build all

doc:
	$(call DUNE, build @doc)
.PHONY: doc

clean: musl-clean cgi-clean
	$(call DUNE, clean)
.PHONY: clean

sandbox:
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
PHONY: sandbox

deps:
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
.PHONY: deps

dune-install: build
	eval $$(opam env)
	$(call DUNE, install)
.PHONY: install

dev-install: dune-install
	install -m 555 $(OPAM_SWITCH_PREFIX)/bin/$(PNAME) ~/bin
.PHONY: home-install

dev-uninstall: 
	$(RM) ~/bin/$(PNAME)
.PHONY: home-install

mounts:
	if mountpoint /data/web 2> /dev/null; then : ; else sudo mkdir -p /data/web && sudo mount voldemort:/export/www-legacy /data/web ; fi
.PHONY: mounts

publish: TEMP_FILE := $(shell mktemp)
publish: build mounts
	cat $(PNAME).opam | grep dev-repo | awk '{ print $$2 }' > $(TEMP_FILE)
	echo 'url { src:' `cat $(TEMP_FILE)` '}' >> $(PNAME).opam
	make -C $(DLDCREPO) add NAME=$(PNAME) OPAM=$$PWD/$(PNAME).opam
.PHONY: publish

cgi:
	$(call DUNE, build)
	install -m 555 $(PWD)/_build/default/app/$(PNAME).exe $(PWD)/cgi-bin/$(PNAME)
.PHONY: cgi

cgi-clean:
	rm cgi/$(PNAME) || true
.PHONY: cgi-clean

serve:
	althttpd -root $(PWD)/cgi-bin -port 3000
.PHONY: serve

musl-build-test:
	docker build --progress=plain --target test -t $(PNAME)-test .
.PHONY: musl-build-test

musl-build-exe:
	docker build --progress=plain -t $(PNAME) .
.PHONY: musl-build-exe

musl-build: musl-build-exe musl-build-test

musl-run: 
	docker run --rm $(PNAME)
.PHONY: musl-run

musl-run-fresh: musl-build musl-run

.PHONY: musl-unit-tests-build

musl-clean:
	docker rmi -f $(PNAME) $(PNAME)-test 2> /dev/null || true
	rm musl/$(PNAME) || true
.PHONY: musl-clean

musl: musl-build
	docker create --name $(PNAME)-extract $(PNAME)
	docker cp $(PNAME)-extract:/usr/local/bin/$(PNAME) ./musl/$(PNAME)
	docker rm $(PNAME)-extract
.PHONY: musl

test: 
	docker run --rm $(PNAME)-test
.PHONY: test

test-fresh: musl-build-test test

githooks:
	chmod +x ./githooks/pre-commit
	git config core.hooksPath ./githooks
.PHONY: githooks

# Local Variables:
# mode: makefile-gmake
# End:
