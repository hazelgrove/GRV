HTML_DIR=_build/default/src

all: dev

deps:
	opam install \
		dune utop ocamlformat \
		incr_dom js_of_ocaml ppx_let ppx_sexp_conv sexplib

dev:
	dune build @src/fmt --auto-promote || true
	dune build --profile dev

release:
	dune build --profile release

echo-html-dir:
	@echo "$(HTML_DIR)"

echo-html:
	@echo "$(HTML_DIR)/index.html"

win-chrome:
	"/mnt/c/Program Files (x86)/Google/Chrome/Application/chrome.exe" "$(HTML_DIR)/index.html"

win-firefox:
	"/mnt/c/Program Files (x86)/Mozilla Firefox/firefox.exe" "$(HTML_DIR)/index.html"

repl:
	dune utop src/grvcore

clean:
	dune clean

.PHONY: all deps dev release echo-html-dir echo-html win-chrome win-firefox repl clean
