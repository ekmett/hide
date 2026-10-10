# Documentation is independent of the editor and its optional native backends.
.DEFAULT_GOAL := docs
CABAL ?= cabal
PANDOC ?= pandoc
DOCS_REVISION ?= $(shell git rev-parse HEAD)
DOCS_BUILD_DIR := $(CURDIR)/build/docs/cabal
DOCS_RUN = $(CABAL) run --project-file=tools/docs/cabal.project --builddir="$(DOCS_BUILD_DIR)" exe:hide-docs --

.PHONY: docs docs-check check-pandoc
docs: check-pandoc
	$(DOCS_RUN) build "$(DOCS_REVISION)" "$(PANDOC)"

# The build validates its output; either entry point works in a fresh checkout.
docs-check: docs

check-pandoc:
	@command -v "$(PANDOC)" >/dev/null || { echo "Pandoc is required to build documentation" >&2; exit 1; }

# Install all entry points together so the compatibility launchers can use siblings.
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
.PHONY: install
install:
	$(CABAL) install exe:hide --installdir="$(BINDIR)" --overwrite-policy=always
	install -m 755 bin/thc-edit bin/th "$(BINDIR)/"
