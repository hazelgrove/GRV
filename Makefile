HTML_DIR=_build/default/src/www
HTML_FILE=$(HTML_DIR)/index.html

all: dev

deps:
	opam switch import opam.export

change-deps:
	opam switch export opam.export

dev:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

watch:
	dune build @src/fmt --auto-promote src --profile dev --watch

release:
	dune build src --profile release

test:
	dune build @src/fmt --auto-promote || true
	dune runtest || true

clean:
	dune clean

echo-html:
	@echo "$(HTML_FILE)"

echo-html-dir:
	@echo "$(HTML_DIR)"

.PHONY: all dev release repl test clean echo-html echo-html-dir