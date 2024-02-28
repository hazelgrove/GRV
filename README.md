# Grove

### Package Upgrade Process

- Intial attempt (M1 Mac Platform)
  - Create a `4.08` switch using Rosetta emulated terminal
  - Verify by running `arch`, should return `i386`
  - Pin `js_of_ocaml` and `ocamlformat` packages 

- Upgraded to `OCaml 5.0.0`
  - `incr_dom v0.15.1`
  - `ocamlformat 0.26.1`
  - Refactored grv web to compile with latest versions