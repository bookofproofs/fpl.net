# Changes in the FPL grammar
## 2.0.0 Amendments to the FPL Grammar, major change of parser   
The original grammar until Grammar version 1.2.1 was the EBNF input for a FPL parser implemented using python and the **tatsu** parser generator.
From the version 2.0 on, there are some major changes to the grammar:
* The FPL parser is being implemented from scratch, based on .NET (concretely the F# FParsec library). 
* The EBNF file in this folder is kept for historical reference only.
* Consequently, there is no separate EBNF input file. 
* Instead, the grammar is coded explicitly in the F# FplParser.fs file in this repositors.

The following documentation describes the amendments and provides a rationale behind each of them.


### General improvements to the AST
* Consequent structuring of the AST using annotations instead of literals.

### More stringent predicate syntax


### Better handling of generic templates

### Keyword 'delegate' or 'del' instead of 'py'

### Simplified Syntax for the 'cases' statement

### Classes 
* AST-Annotation for optional calls of parental constructors in classes (originally also optional but not annotated as such)
* Bugfix: Classes can only be derived from classType that now excludes functional terms and predicates (classes of FPL objects never intended to be derived from predicates or functional terms) 

### Enhancement of Proofs
* Justifying arguments can now contain not only lists of 'primePredicate' but more general of 'predicate'
* Derived arguments can now also reference to the conclusion of the to-be-proven theorem
* A simplified syntax of argumentIdentifiers (referencing via slash '/' is no more necessary)
* Bugfix preventing syntax allowing assumptions followed by a justification (pure math standard: without justification)

### Self-Containment 


