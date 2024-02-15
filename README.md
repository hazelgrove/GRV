# Grove

### Move statics and update `dune` 
- Move `index.html` and `style.css` and all other `.js` deps to a separate directory
- Create `dune` build, copy [haz3lweb/www/dune](https://github.com/hazelgrove/hazel/blob/0de93aaecff7ef432b37737e11679a28025b4108/src/haz3lweb/www/dune)


### Package Upgrade Process

- Intial attempt (M1 Mac Platform)
  - Create a `4.08` switch using Rosetta emulated terminal
  - Verify by running `arch`, should return `i386`
  - Pin `js_of_ocaml` and `ocamlformat` packages 

- Upgraded to `OCaml 5.0.0`
  - `incr_dom v0.15.1`
  - `ocamlformat 0.26.1`
  - Refactored grv web to compile with latest versions
