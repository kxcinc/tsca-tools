OPAM=opam
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
TEZOS_PACKAGES:=$(shell $(ROOT_DIR)/scripts/list-tezos-packages.sh)

.PHONY: build-deps build-tezos install-tezos-packages list-tezos-packages

build-deps:
	@echo "initializing local opam switch and install tezos dependencies"
	WORKSPACE_ROOT="$(ROOT_DIR)" vendors/tezos/scripts/install_build_deps.sh

install-tezos-packages:
	(cd vendors/tezos && ./scripts/opam-pin.sh)
	@echo '## Installing these packages: ' $(TEZOS_PACKAGES)
	@echo '##'
	opam depext --yes $(TEZOS_PACKAGES)
	opam reinstall --yes $(TEZOS_PACKAGES)

build-tezos-src:
	(cd vendors/tezos && make all.pkg)

list-tezos-packages:
	@echo $(TEZOS_PACKAGES)
