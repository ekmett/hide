# Documentation is independent of the editor and its optional native backends.
.DEFAULT_GOAL := docs
CABAL ?= cabal
PANDOC ?= pandoc
DOCS_REVISION ?= $(shell git rev-parse HEAD)
DOCS_BUILD_DIR := $(CURDIR)/build/docs/cabal
DOCS_RUN = $(CABAL) run --project-file=tools/docs/cabal.project --builddir="$(DOCS_BUILD_DIR)" exe:thc-edit-docs --

.PHONY: docs docs-check check-pandoc
docs: check-pandoc
	$(DOCS_RUN) build "$(DOCS_REVISION)" "$(PANDOC)"

docs-check:
	$(DOCS_RUN) check "$(DOCS_REVISION)"

check-pandoc:
	@command -v "$(PANDOC)" >/dev/null || { echo "Pandoc is required to build documentation" >&2; exit 1; }
