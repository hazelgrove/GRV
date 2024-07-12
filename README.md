# Grove Workbench

---

## Build Instructions

1. Run `make open` to play around with Grove in your browser

## Exploring Grove Workbench

Upon opening the workbench in your browser, you will see two instances of the editor running side-by-side emulating two collaborators. The workbench also offers a graph based view apart from the regular structure editor to visualize the relationship between the vertices corresponding to their terms.

As the graph forms a CmRDT, sharing edits across instances is a matter of record and replay. When the user performs some edits, which correspond to patches formally, the \emph{actions} panel displays the local history of edit actions. These patches can be shared across editor instances, and the local history panel is flushed upon synchronization. The user edits the state of the graph, which is displayed in the graph visualization, and upon these edits, the graph gets \emph{decomposed} into a grove. Under the hood, the model is able to \emph{classify} the vertices as one of $NP$, $MP$, or $U$ roots or not a root. The terms in $NP$, $MP$, and $U$ root cases correspond to the deleted, multiparented, and unicycle panels in the workbench, where the latter two are instances of location-based conflicts. The deleted panel also allows the user to select a deleted term and restore it to the cursor location in the editor, which is attached using a fresh edge, as edge deletion is permanent. In addition to performing edits and sharing them, the user can also \emph{clone} and \emph{drop} editor instances to add more collaborative users into the mix. Keyboard commands and buttons are available below the panels to interact with the editor and the workbench. This is a brief overview of the features of the workbench.

### Package Upgrade Process

- Upgraded to `OCaml 5.0.0`
  - `incr_dom v0.15.1`
  - `ocamlformat 0.26.1`
