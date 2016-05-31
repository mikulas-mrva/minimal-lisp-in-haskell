# minimal-lisp-in-haskell
Implementation of a minimal subset of Lisp in Haskell. Written as a final project for a course in functional and logic programming at the Charles University in Prague, Faculty of Arts, Logic bachelor course.

Based on [this article](https://programmingpraxis.com/2011/11/01/rip-john-mccarthy/).

Implemets constants *t*, *nil* and functions *atom*, *car*, *cdr*, *cons*, *defun*, *eq*, *if*, *floor*, *quote*, *'*, *+*, *-*, \*, */* and *<*.

The greatest limitation of this implementation is the fact that it works as a repl, so line ending triggers evaluation and more complex expressions can not be indented.
