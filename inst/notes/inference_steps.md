Inference Procedure
================
Nick Ulle
August 12, 2016

The type inference procedure is divided into three steps.

1.  Conversion
2.  Detection
3.  Inference

The details of the conversion step are discussed in the notes for the ast package. The primary purpose is to convert R code to a data structure suitable for storing the results of inference.

The detection step traverses the AST to collect type information available in each statement. The most important actions of this step are taken for Symbol and Assign nodes.

When a Symbol node is encountered, that node is annotated with a type variable. The correct type variable is determined by looking up the Symbol's name in a table of live type variables.

When an Assign node is encountered, a new type variable is assigned to the Symbol being written and the table of live type variables is updated. A new constraint is also added to the constraint set to show that the new type variable must have the same type as the expression being read.

Actions must also be taken for other nodes in the tree. \[TODO\]

The inference step solves the constraint set by unification.
