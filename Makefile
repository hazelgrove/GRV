BUILD_DIR=_build/default
HTML_DIR=_build/default/src/grvweb/

all: dev

deps:
	opam install \
		dune utop ocamlformat odoc \
		incr_dom js_of_ocaml ppx_deriving ppx_let ppx_sexp_conv sexplib

dev:
	dune build @src/fmt --auto-promote || true
	dune build --profile dev
	dune build @doc

test:
	dune build @runtest -f

release:
	dune build --profile release

cat-lang:
	@cat "$(BUILD_DIR)/src/grvcore/Lang.ml"

echo-lang:
	@echo "$(BUILD_DIR)/src/grvcore/Lang.ml"

echo-doc:
	@echo "$(BUILD_DIR)/src/grvdoc/master.pdf"

echo-html-doc:
	@echo "$(BUILD_DIR)/_doc/_html/GRV/index.html"

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

.PHONY: all deps dev release echo-html-dir echo-html echo-doc \
	win-chrome win-firefox repl clean
