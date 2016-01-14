Elm Polish Parser
=================

A small toy project writing a parser and recursive evaluator for polish notation. 

It can be ran with `elm-reactor` and is completely interactive. It will also show some debug information detailing the interpretation process. 

Notes
-----

The AST of the expression is stored in a List, but in effect it is a Tree. The list is traversed to find Operator nodes with two evaluated arguments. 
A future improvement may be to actually represent this structure as a Tree. This would also make adding n-ary operators easier. 

Furthermore, this code uses Float to store numbers, instead of the `number` type class. Data types cannot currently be defined over type classes, only over concrete types.  

