# Changes in the FPL grammar
## 2.0.0 Amendments to the FPL Grammar, major change of parser   
The original grammar until Grammar version 1.2.1 was the EBNF input for a FPL parser implemented using python and the **tatsu** parser generator.

From the version 2.0 on, there are some major changes to the grammar:
* The FPL parser is being implemented from scratch, based on .NET (concretely the F# FParsec library). 
* Consequently, there is no separate EBNF input file. 
* There are amendments to the FPL syntax in the new implementation.

The following documentation describes the syntactical amendments and provides a rationale behind each of them.

### General improvements to the AST
* Consequent structuring of the AST using annotations 
* Skipping rule names not needed in the AST is now done automatically in FParsec
* A more careful placement of whitespace and comments

### Error recovery
* Both, the original tatsu parser generator and the new, FParsec-based parsers do not provide in-built error recovery
* However, the Fparsec-based parser provides more programmatic control. 
* Potentially, error recovery can now be added to the FPL parser more easily.

### More stringent predicate syntax
* Giving up mixing up statements and index variables being not predicative in the choice rule of Prime predicates 
* This mixing is now only possible for the isOperator.

### Better handling of keywords and generic templates
* Better recognition and error reporting of conflicts between variable names and keywords 
* Better recognition and error reporting of conflicts between variable names and template names

### Keyword 'delegate' or 'del' instead of 'py'
* The original python 'py' prefix followed pythonic delegates like 'py.some_delegate_name(x,y,z)' are now replaced by the more general prefix 'del' or 'delegate'
* The name for the delegates are now generalized from the original regex @"[a-z_]*" to @"[a-z_A-Z][a-z_A-Z0-9]+"

### Simplified Syntax for the 'cases' statement
* The keywords 'case' and 'else' are now discontinued and replaced by the literals '|' and ":>"
* Rationale: 
    * Simplified syntax since 'case' is likely to be mistaken for the also existing keyword 'cases'
    * '|' is intuitively similar to the ebnf or regex 'OR' character 
    * Simplified recognition of the end of each 'case' and the 'else' block since the literals '|' and ":>" do not conflict with small-case variable names of predicates that may be in each 'case' and the 'else' block.
    * Improved readability

### Classes 
* AST-Annotation for optional calls of parental constructors in classes (originally also optional but not annotated as such)
* Bugfix: Classes can only be derived from classType that now excludes functional terms and predicates (classes of FPL objects never intended to be derived from predicates or functional terms) 

### Enhancement of Proofs
* Justifying arguments can now contain not only lists of 'primePredicate' but more general of 'predicate'
* Derived arguments can now also reference to the conclusion of the to-be-proven theorem
* A simplified syntax of referencing argumentIdentifiers (referencing via slash '/' is no more necessary). Now simply restate the same identifier
* Bugfix preventing syntax allowing assumptions followed by a justification (pure math standard: without justification)
* Improved readability

### Self-Containment 
* This is not really an amendment to the FPL parser. However, we want to significantly simplify the later recognition of self-containment in the FPL interpreter by the following convention:
    * The order of declarations will now matter. 
    * This is unlike the previous, python-based FPL interpreter (which can be found in the repository [https://github.com/bookofproofs/fpl](https://github.com/bookofproofs/fpl)).
    * In the new FPL interpreter, checking if an FPL identifier was already declared can be done - in principle - already during the parsing process. This could significantly simplified the implementation and performance of the new FPL interpreter.
    * Nevertheless, we stick to the 'must' requirements (see [INTRO.md](https://github.com/bookofproofs/fpl.net/blob/main/))#28 (support of overrides), #38 (support recursive linguistic constructs). and #40 (support of self-reference in definitions) that could still potentially negatively impact how complicated it is to implement the new FPL interpreter.

### Namespaces
* A single *.fpl file can now contain more than one namespace. This will significantly simplify later preprocessing when we FPL parser needs to included namespaces via the 'uses' keyword. 
* Moreover, it provides more flexibility to end-users
* Since 'order of declarations now matter' (see Self-Containment above), we have to discontinue the possibility of including FPL namespaces using wildcards like in 'Fpl.Commons.*') since it may be undecidable in which order they have to be included.
