# Grove

## Introduction

- Grove is a collaborative structure editor calculus (think of Hazel with multiple online / offline editors). It uses [CRDTs](FIXME) and a target language that forms a join semilattice such that when multiple editors share / synchronize their edits, the actions always commute, and hence all the replicas converge to the same state (independent of the order in which the edits are applied).

- Additional reading


## Building Grove Locally

- Clone the repo locally
- Make sure you're running OCaml `5.0.0`
- Run these following lines

```bash
  make deps
  make dev
  make open
```

- You should now see the `Grove Sandbox` with two editors loaded

## Grove By Example

### Single-User Actions

#### Construction & Deletion

Fig 2. Simple Edits

1. You start with `x * []`
2. Add `y` to right operand
3. Delete `x`

```lisp
(((1
   (1
    ((((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_times))))))
      Created)
     (((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_times))) (position Exp_times_left)))
         (target ((id 5) (value (Exp_var x)))))))
      Created))
    ((vertex ((id 3) (value Exp_times))) (position Exp_times_left))
    (((edge
       ((id 4)
        (value
         ((source
           ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
          (target ((id 3) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 6)
        (value
         ((source
           ((vertex ((id 3) (value Exp_times))) (position Exp_times_left)))
          (target ((id 5) (value (Exp_var x))))))))
      (state Created)))
    (((edge
       ((id 4)
        (value
         ((source
           ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
          (target ((id 3) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 6)
        (value
         ((source
           ((vertex ((id 3) (value Exp_times))) (position Exp_times_left)))
          (target ((id 5) (value (Exp_var x))))))))
      (state Created)))
    false))
  (2
   (2 () ((vertex ((id 0) (value Root_root))) (position Root_root_root)) ()
    () false)))
 (((1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_times))) (position Exp_times_left)))
         (target ((id 5) (value (Exp_var x))))))))
     (state Created)))
   (1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_times)))))))
     (state Created))))))
```

#### Wrapping

Fig.3 Wrapping

NOTE: Contineu with same editor state as before

1. Move cursor `up`
2. Construct `+`

#### Repositioning

This action is for cut/paste as edges are uniquely identifiable, i.e have explicit repositioning actions that preserve identities.
NOTE: For now, we have to explicitly delete old edge and create a fresh one

1. Delete `([] * y)`
2. Restore the same on the right operand of `+`

### Multi-User Actions

TODO: Save dump of starting state of Fig 5

#### Multiple users editing

Fig. 5 Multiple users editing different parts of the code

NOTE: Continue with same editor state as before

1. Send state to Bob  
2. Alice adds `u` to left operand of `*`
3. Bob **changes** `y` to `v`. First he deletes `y`, then constructs `v`.
4. Send actions to each other


#### Multiple users editing nested parts of the code

NOTE: Continue from last stage

1. Ensure both have `[] + u * y`
2. Alice **changes** `u` to `w`.
3. Bob repositions `*` from right to left operand of `+`
4. Share actions you should have `w * v + []`

### Multi-user conflicts

#### Multi-child Conflicts

NOTE: Continue with same edit state as last stage

1. Alice constructs `x` at right operand of `+`
2. Bob constructs 'y` at same spot
3. Share edits with each other

#### Multi-parent references

1. Delete the conflict and `v`
2. You should have `(w * []) + []` on both editors
3. Alice repositions `w` to right operand of `*`
4. Bob repositions `w` to right operand of `+`
5. Share edits
6. `w` will show up in the multiparented panel

NOTE: We do not have a perfect way to resolve this conflict yet, up to the discretion of users
NOTE: For now, we just delete the references

#### Unicycles

NOTE: Start with fresh state, load this into the editor and share edits

```lisp
(((1
   (1
    ((((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus))))))
      Created)
     (((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times))))))
      Created)
     (((id 8)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
         (target ((id 7) (value Exp_times))))))
      Created)
     (((id 10)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
         (target ((id 9) (value Exp_plus))))))
      Created))
    ((vertex ((id 0) (value Root_root))) (position Root_root_root)) ()
    (((edge
       ((id 4)
        (value
         ((source
           ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
          (target ((id 3) (value Exp_plus)))))))
      (state Created))
     ((edge
       ((id 6)
        (value
         ((source
           ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
          (target ((id 5) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 8)
        (value
         ((source
           ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
          (target ((id 7) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 10)
        (value
         ((source
           ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
          (target ((id 9) (value Exp_plus)))))))
      (state Created)))
    false))
  (2
   (2
    ((((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus))))))
      Created)
     (((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times))))))
      Created)
     (((id 8)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
         (target ((id 7) (value Exp_times))))))
      Created)
     (((id 10)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
         (target ((id 9) (value Exp_plus))))))
      Created))
    ((vertex ((id 0) (value Root_root))) (position Root_root_root)) ()
    (((edge
       ((id 4)
        (value
         ((source
           ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
          (target ((id 3) (value Exp_plus)))))))
      (state Created))
     ((edge
       ((id 6)
        (value
         ((source
           ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
          (target ((id 5) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 8)
        (value
         ((source
           ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
          (target ((id 7) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 10)
        (value
         ((source
           ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
          (target ((id 9) (value Exp_plus)))))))
      (state Created)))
    false)))
 (((1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 8)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
         (target ((id 7) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 10)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
         (target ((id 9) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 8)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
         (target ((id 7) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 10)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
         (target ((id 9) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 10)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_right)))
         (target ((id 9) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 8)
       (value
        ((source
          ((vertex ((id 5) (value Exp_times))) (position Exp_times_left)))
         (target ((id 7) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus)))))))
      (state Created))))))
```

1. Alice moves left nested `*` to right operand of `+` and then moves right nested `+` (L) under it.
2. Bob moves right nested `+` to right operand of `+` and then moves left nested `*` (L) under it.
3. Share edits with each other
4. Manually break cycle to resolve conflict

#### Multiple Editors

NOTE: Load this editor stage

```lisp
(((1
   (1
    ((((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus))))))
      Created)
     (((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times))))))
      Created)
     (((id 8)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
         (target ((id 7) (value Exp_times))))))
      Created))
    ((vertex ((id 7) (value Exp_times))) (position Exp_times_left)) ()
    (((edge
       ((id 4)
        (value
         ((source
           ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
          (target ((id 3) (value Exp_plus)))))))
      (state Created))
     ((edge
       ((id 6)
        (value
         ((source
           ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
          (target ((id 5) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 8)
        (value
         ((source
           ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
          (target ((id 7) (value Exp_times)))))))
      (state Created)))
    false))
  (2
   (2
    ((((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus))))))
      Created)
     (((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times))))))
      Created)
     (((id 8)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
         (target ((id 7) (value Exp_times))))))
      Created))
    ((vertex ((id 0) (value Root_root))) (position Root_root_root)) ()
    (((edge
       ((id 4)
        (value
         ((source
           ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
          (target ((id 3) (value Exp_plus)))))))
      (state Created))
     ((edge
       ((id 6)
        (value
         ((source
           ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
          (target ((id 5) (value Exp_times)))))))
      (state Created))
     ((edge
       ((id 8)
        (value
         ((source
           ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
          (target ((id 7) (value Exp_times)))))))
      (state Created)))
    false)))
 (((1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 8)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
         (target ((id 7) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus)))))))
     (state Created)))
   (1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 8)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
         (target ((id 7) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 8)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_right)))
         (target ((id 7) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 6)
       (value
        ((source
          ((vertex ((id 3) (value Exp_plus))) (position Exp_plus_left)))
         (target ((id 5) (value Exp_times)))))))
     (state Created)))
   (1
    ((edge
      ((id 4)
       (value
        ((source
          ((vertex ((id 0) (value Root_root))) (position Root_root_root)))
         (target ((id 3) (value Exp_plus)))))))
      (state Created))))))
```

1. Alice moves right nested `*` to right operand of left nested `*`
2. Bob moves left nested `*` to left operand of right nested `*`
3. Share actions with each other


### Package Upgrade Process

- Intial attempt (M1 Mac Platform)
  - Create a `4.08` switch using Rosetta emulated terminal
  - Verify by running `arch`, should return `i386`
  - Pin `js_of_ocaml` and `ocamlformat` packages

- Upgraded to `OCaml 5.0.0`
  - `incr_dom v0.15.1`
  - `ocamlformat 0.26.1`
  - Refactored grv web to compile with latest versions
