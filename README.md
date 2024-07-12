# Grove Workbench

---

## Build Instructions

1. Install OCaml 5.0.0
2. Run `make deps` to install the dependencies
3. Run `make dev` to build Grove locally
4. Run `make open` or open the generated `index.html` file to play around with Grove in your browser

## Exploring Grove Workbench

Upon opening the workbench in your browser, you will see two instances of the editor running side-by-side emulating two collaborators. The workbench also offers a graph based view apart from the regular structure editor to visualize the relationship between the vertices corresponding to their terms.

There are multiple panels that gather information for restoration or any *conflicts* that may occur during collaboration. The actions panels contains a local history of all the actions perfomed by the user to get to that editor which gets flushed upon synchronization. The send to editors pane allows the user to select which user they want to their edits with. The number of editor replicas can be changed by *cloning* or *dropping* editors. The multi-parented panel and unicycle panels store terms that correspond to relocation and unicycle conflicts. The deleted panel contains deleted terms that can be restored by selecting the appropriate term from the panel and having the cursor in the editor where the term is to be restored. Keyboard commands and buttons are available below the panels to interact with the editor and the workbench.  

### Package Upgrade Process

- Upgraded to `OCaml 5.0.0`
  - `incr_dom v0.15.1`
  - `ocamlformat 0.26.1`
