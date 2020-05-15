# `grvcore`

This code is split into the following directories.

## The `app` directory

This folder contains application-level code code such as `Model` and `State`.

## The `ast` directory

This folder contains code specific to the AST representation of the code.

## The `graph` directory

This folder contains graph code independent to the particular language being modeled
(e.g., vertexes and edges).

The exceptions to this are "root" objects like `Vertex.root` and `Graph.Child.root`.
