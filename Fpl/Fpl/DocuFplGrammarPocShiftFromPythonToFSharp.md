# Changes in FPL 
## Amendments to the FPL grammar, major change of the FPL parser   
The original grammar until version 1.2.1 was the EBNF input for an FPL parser implemented using Python and the **tatsu** parser generator.

From the version 2.0 on, there are some major changes to the grammar:
* The FPL parser was implemented from scratch, based on .NET (concretely the F# FParsec library). 
* Consequently, there is no separate EBNF input file, since the grammar results from FParsec parsers building upon each other.
* There are amendments to the FPL syntax in the new implementation.

The following documentation describes the syntax amendments and provides a rationale behind each.

### General improvements of the Abstract Syntax Tree (AST)
* Consequent structuring of the AST using F# types 
* Skipping rule names not needed in the AST as specified in the FParsec parser
* A stricter syntax  
* A more careful placement of whitespace and comments.

### Error recovery
* Both the original tatsu parser and the new, FParsec-based parser do not provide in-built error recovery.
* However, the Fparsec-based parser provides more programmatic control. 
* An experimental approach to error recovery was added to the FPL parser.

### Changes to the Grammar (Details)

#### 1) No more extra blocks for uses, inference, theories and localizations

In the original FPL version, a namespace contained four types of sections: the optional `uses` clause, the `inference` and `localization` blocks and a mandatory `theory` block. Inside the blocks, there were syntactical differences with respect to whether a building block, like a theorem or an inference rule required a prefix keyword or not. For instance, inference rules had no prefix inside their `inference` block while theorems, axioms and proof building blocks required the corresponding keyword inside the `theory` block. Moreover, the `uses` clause did not contain a block but was a comma-separated list 

In the current version, the extra sub-blocks  `inference`, `localization`, and `theory` as well as comma-separation in the `uses` clause were abandoned. Instead, each building block has a unique prefix keyword:

* Every namespace that could be referred to in a comma-separated list in the `uses` clause of previous FPL grammar versions now requires a separate `uses` keyword,
* Every inference rules starts with the keyword `inference` (short-form `inf`),
* Every localization starts with the keyword `localization` (short-form `loc`),
* Every class definition starts the keyword `definition` (short-form `def`), followed as previously by the keyword `class` (short-form `cl`), 
* Every predicate definition starts the keyword `definition` (short-form `def`), followed as previously by the keyword `function` (short-form `func`), 
* Every functional term definition starts the keyword `definition` (short-form `def`), followed as previously by the keyword `predicate` (short-form `pred`), 
* All other building blocks keywords remain unchanged, e.g. axioms are started by the keyword `axiom` (short-form `ax`) or `postulate` (short-form `post`), theorems are started by the keyword `theorem` (short-form `thm`), etc.
    
#### 2) In-block Variable Type Declarations and Statements
In FPL, you can declare variable types of building blocks both in their signatures or in their body. Moreover, in the body also statements using variables are possible.
  The body type declarations and statements must be started by a new keyword `declaration` (or `dec`) and ended by a semicolon `;`. 

*Before*
``` 
{
    // some type declarations
    myField: Field 
    addInField: BinOp
    mulInField: BinOp
    // some statements
    myField := field
    addInField := myField.AddOp()
    mulInField := myField.MulOp()
}
``` 
*Now*
``` 
{
    dec
        // some type declarations
        ~myField: Field 
        ~addInField: BinOp
        ~mulInField: BinOp
    
        // some statements
        myField := field
        addInField := myField.AddOp()
        mulInField := myField.MulOp()
    ; 
}
``` 
This is a trade-off between simplicity of syntax and a stricter syntax with respect to the distinction of type declarations and statements, and with respect to what comes after them in the building block. In FPL, this depends on the kind of a building block. For instance, functional terms require a return statement while predicates require a predicate. With the new enclosing annotations `dec` and `;` it becomes much more easier to recognize missing linguistic components, because this recognition can be done already in the parser on the syntactical level and have not be dealt with by the interpreter. These additional syntax changes will be discussed below. 

The `~` at the beginning of type declarations is necessary to distinguish between them and statements starting with a variable (like the assignment statement). The FParsec parser would allow to get rid of this additional syntactical sugar using the `attempt` parser, but at the cost of a less intuitive error recovery messaging: If the syntax error occurred somewhere deep in the type declaration, the position of the reported error would point not to the error but to the beginning of the attempted type declaration. Thus, the `~` at the beginning of each type declaration is a trade-off between to-be-avoided syntax complexity and to-be-avoided `attempt` parsers for better error recovery messages.

#### 3) Classes allow multiple inheritance

In previous version of the grammar, polymorphism of mathematical object could only be achieved by using the `is` operator and asserting that the object is of some additional type than the type of the single parent class. Now, this is simplified because we can add as many types to the class as necessary.

The AST-Annotation for optional calls of parental constructors in classes (originally also optional but not annotated as such)

Bugfix: Classes can only be derived from classType that now excludes functional terms and predicates (classes of FPL objects never intended to be derived from predicates or functional terms) 



*Before*
``` 
class A :B
{
    A() 
    {
        assert is(self,C)
    }
    ...
}
``` 
*Now*
``` 
class A :B, :C
{
...
}
``` 

#### 4) Syntax of class constructors

The syntax of constructors allows calls of parental constructors in the `specification`, their syntax starts with `base." followed by the parent class identifier and related parameters like in `base.ParentClass(first, second)` . Moreover, the keyword `self` has as the last expression in the constructor. These two additional components disambiguate the representation of the class object after the constructor has been executed. 

*Before*
``` 
class SomeClass: ParentClass
{
	// private variables of the class
    myField: Field

    // constructor
    SomeClass(field : Field)
    {
        myField := field
    }
}
``` 
*Now*
``` 
class SomeClass: ParentClass
{
	// private variables of the class
    dec:
        myField: Field
    ;

    // constructor
    SomeClass(field : Field)
    {
    	dec
            myField := field
            base.ParentClass()
        ;
        self
    }
}
``` 

#### 5) Explicit intrinsic definitions

*Before*
``` 
class Set: obj
{
}
``` 
*Now*
``` 
class Set: obj
{
    intrinsic
}
``` 
In intrinsic definitions of classes, predicates, functional terms, and related properties of this kind, the new keyword `intrinsic` (short form `intr`) has to be used explicitly. This prevents the definition of being empty just because the end-user left the body of a definition unintentionally.

#### 6) `assert` becomes a statement (not a predicate)

*Before*
``` 
all x ( assert p(x) )
``` 
*Now*
``` 
all x ( p(x) )
``` 
The `assert` statement cannot be used, where FPL expects a predicate. Using `assert` is still possible in the context where statements are expected, in particular in the `specification` section. 


#### 7) `for` loop replaces `loop` and `range`

In the original version of FPL grammar, loops could be constructed using the `loop` and the `range` keywords with pretty the same syntax. Now, this is unified using the `for` keyword. Moreover, the `in` keyword makes the statement more readable.

*Before*
``` 
    range i [1~n]
    (
        // do something
    )

    loop i [1~n]
    (
        // do something
    )

``` 
*Now*
``` 
    for i in [1~n]
    (
        // do something
    )
``` 

The for statement comes with three flavors:
* iterating through all i in some type: `for i in SomeType (...)`, 
* iterating through all i in some range: `for i in [1~n] (...)`,
* iterating through all i in some variadic variable: `for i in y (...)`.


#### 8) New `exn` keyword  

The existence quantor accepting a number of allowed occurrences gets an own keyword `exn`, disambiguating it from the general quantor `ex`.

*Before*
``` 
    ex$1 x ( p (x) ) // there exists exactly one x ...

``` 
*Now*
``` 
    exn$1 x ( p (x) ) // there exists exactly one x ...
``` 

This disambiguation helps to formulate the FPL grammar without `attempt` parsers which would otherwise distort error positions shown during error recovery that occur inside the predicate.

#### 9) In-built equality predicate

The original FPL language had not inbuilt equality predicate, relying on the second-order logic definition of equality that would have otherwise be provided in the language itself

``` 
    pred Equal(a,b: tpl)
    {
        dec: p: pred ;

		all p
		(
			iif
			(
				p(a),
				p(b)
			)
		)
    }
``` 

However, this approach seems not feasible from the implementation point of view because such a predicate would be hard if not impossible to interpret. Moreover, the equality sign `=` in the infix notation is so common in proof-based mathematics that it is preferable to FPL expressions using the user-defined `Equal` predicate.

In the new FPL syntax version, we introduce an inbuilt equality sign and allow infix notation for equality. Nevertheless, infix notation together with the error recovery mechanism in the new FPL parser
  requires disambiguation in the form that equality comparison have to be enclosed by some other characters that cannot be mixed up with other literals or predicates in FPL. We chose the enclosing characters `(` and `)` for this purpose:

*Before*
``` 
    Equals (x,y) 

``` 
*Now*
``` 
    (x = y)
``` 

Of course, the equality predicate can be placed everywhere any predicate can be used in FPL, for instance, it can be nested within other predicates. 

The equality comparison supports multiple equalities at once, for instance `( x = y = z )` is a shorter form for  `and ((x = y), (y = z))`

#### 10) Domains are now allowed in the quantors `all`, `ex`, and `exn`.

In the original version of FPL grammar, free variables used in quantors had to be first declared with a specific type and could then be only listed after the quantor and before the predicate of the quantor. In the new version of the FPL grammar, type declarations can be made implicit by using allowing the `in` keyword. 

*Before*
``` 
    dec: 
        x: Nat 
    ;
    all x ( p(x) ) // for all p of type Nat, the predicate p(x) holds

``` 
*Now*
``` 
    all x in Nat ( p (x) ) 
``` 

 The syntax is similar to the `for` statement, i.e. it also allows variadic variables and ranges, but it is more flexible since it not only also allows types but also can be enumerated, for instance this the free variables `x` and `n` do not have to be declared. Their type will be inferred from how they are used in the `all` quantor:

``` 
    all x in Nat, n in [1~m] 
    ( 
        p (x,n) 
    ) 

``` 

Also, expressions in second-order logic predicates are allowed:

``` 
    axiom SchemaSeparation()
    {
        all p in pred, x,y in Set
        (
            ex y in Set
            (
                (y = SetBuilder(x,p))
            )
        )
    }
``` 

#### 11) Disambiguation of coordinate lists and ranges

In previous versions, the square brackets `[`, `]` were used for both, coordinate lists and ranges. 
In the new version, coordinates have to be placed in square brackets `[` and `]`, while ranges have left and right bounds 
that can be either open like in `[(`, `)]` or closed like in `[[`, `]]`
This disambiguates the grammar for to improve the inbuilt error recovery mechanism.

*Before*
``` 
    for i in [1~n]  // <- range
    (
        a[i,j]:= b // <- coordinates
    )

``` 
*Now*
``` 
    for i in [[1~n]]  // <- range
    (
        a[i,j]:= b // <- coordinates
    )
``` 

#### 12) Delegates 

* The original Python `py` prefix followed pythonic delegates like `py.some_delegate_name(x,y,z)` are now replaced by the more general prefix `del` or `delegate`
* The names for the delegates are now generalized from the original regex `@"[a-z_]*"` to `@"[a-z_A-Z][a-z_A-Z0-9]+"`

*Before*
``` 
    py.decrement(x)

``` 
*Now*
``` 
    delegate.decrement(x)
``` 

#### 13) Syntax of extensions vs. syntax of indexed variables

Extensions can be used in FPL (among others) to introduce symbols "0,1,2" ... etc for the purpose of identifying them with concrete mathematical objects, like natural numbers. 
FPL has no in-built type "natural number". Instead, we can define natural numbers for instance using the Peano axioms and then identifying the class of objects "Natural number" with the (otherwise meaningless) symbols "0,1,2,..." Nevertheless, FPL has in-built index type using the same symbols. 

To disambiguate the two usages, the following changes in the FPL syntax have been performed:

* Simplification of the extension syntax:
*Before*
``` 
    :ext
        extDigits : /\d+/
    :end

    // other stuff ...
    
    // constructor identifying the introduce digit symbols with natural numbers
    Nat(x: @extDigits)
    {
        ...
    }

``` 

*Now*
``` 
    :ext
        Digits : /\d+/
    :end

    Nat(x: @Digits)
    {
        ...
    }
``` 

* Indexing is done via `!` instead of `$`
*Before*
``` 
    n$1 // (n subindex 1)
    n$m$k // (n subindex m subindex k)
``` 

*Now*
``` 
    n!1
    n!m!k
``` 
* The symbols `!1`, `!2`, ... have always the predefined type `index` and never the type of the extension.
* In proof-based mathematics, there are use cases where extensions symbols `1,2,3,...` (for instance those identified with natural numbers) have to be used as index variables. In those cases, FPL allows the coordinate notation.

``` 
    n[1]
    n[m,k]
``` 
* Left- and Right-open ranges contain both indexes and extensions
*Before*
``` 
    // boundaries cannot be disambiguated between index type and extension type
    [1~5] // (closed range with boundaries 1 and 5)
    [!1~5] // (left-open range with boundaries 1 and 5)
    [1~5!] // (right-open range with boundaries 1 and 5)
    [!1~5!] // (left- and right-open range with boundaries 1 and 5)

``` 

*Now*
``` 
    // boundaries can be disambiguated 
    // extension type 
    [[1~5]] // (closed range with boundaries 1 and 5 )
    [(1~5]] // (left-open range with boundaries 1 and 5)
    [[1~5)] // (right-open range with boundaries 1 and 5)
    [(1~5)] // (left- and right-open range with boundaries 1 and 5)

    // boundaries can be disambiguated 
    // index type 
    [[!1~!5]] // (closed range with boundaries 1 and 5 )
    [(!1~!5]] // (left-open range with boundaries 1 and 5)
    [[!1~!5)] // (right-open range with boundaries 1 and 5)
    [(!1~!5)] // (left- and right-open range with boundaries 1 and 5)

``` 

#### 14) Simplified syntax of proofs
The following changes have been made:
* Justifying arguments can now contain not only lists of 'primePredicate' but more general of 'predicate'
* Derived arguments can now also reference the conclusion of the to-be-proven theorem
* A simplified syntax of referencing argumentIdentifiers (referencing via slash `/` is no longer necessary). Now, restating the same identifier is enough.
* Bugfix preventing syntax allowing assumptions followed by a justification (pure math standard: without justification)
* The referencing identifier using dollar digits (e.g. `$1` at the end of `SomeTheorem$1`) was extended to look like the indexed predicate (`SomeTheorem!$1`) so there is no difference between the two any more.
* Improved readability, for instance, instead of 

*Before*
```
    proof SomeTheorem$1
    {
        a:A
        b:B
        c:C
        x,y,z: obj

        1. /GreaterAB |- Greater(a,b)
        2. /GreaterBC |- Greater(b,c)
        3. /ProceedingResults(/1,/2) |- and (Greater(a,b), Greater(b,c))
        4. /3, /GreaterTransitive |- impl ( and (Greater(a,b), Greater(b,c)), Greater(a,c) )
        5. /4, /ModusPonens |- Greater(a,c)
        6. /ProceedingResults(/5,/1) |- and (Greater(a,c), Greater(a,b))
        7. /6, /ExistsByExample(and(Greater(a,c), Greater(a,b))) |- ex x ( and (Greater(x,y), Greater(x,z)) )
	8. |- qed
    }
```
*Now*
```        
    proof SomeTheorem!$1
    {
        dec
            ~a:A
            ~b:B
            ~c:C
            ~x,y,z: obj
        ;
        1. GreaterAB |- Greater(a,b) 
        2. GreaterBC |- Greater(b,c) 
        3. ProceedingResults(1.,2.) |- and (Greater(a,b), Greater(b,c)) 
        4. 3., GreaterTransitive |- impl ( and (Greater(a,b), Greater(b,c)), Greater(a,c) ) 
        5. 4., ModusPonens |- Greater(a,c)
        6. ProceedingResults(5.,1.) |- and (Greater(a,c), Greater(a,b)) 
        7. 6., ExistsByExample(and(Greater(a,c), Greater(a,b))) |- ex x ( and (Greater(x,y), Greater(x,z)) ) 
        8. |- qed
    }
```

#### 15) Simplified syntax of the `cases` statement
* The keyword `case` is now discontinued and replaced by the literal `|`
* The sequence of cases starting by the literal `|` must end by a `?` which denotes the default case.
* Thus, we use `?` instead of `else:`.
* Rationale: 
    * Simplified syntax since `case` is likely to be mistaken for the existing keyword `cases`
    * `|` is intuitively similar to the BNF or regex 'OR' character 
    * Improved readability

*Before*
```
    cases
    (
        case Equal(x,0) : self := Zero()
        case Equal(x,1) : self := Succ(Zero())
        case Equal(x,2) : self := Succ(Succ(Zero()))
        else: self := Succ(delegate.decrement(x))
    )
```
*Now*
```        
    cases
    (
        | (x = 0) : self := Zero()
        | (x = 1) : self := Succ(Zero())
        | (x = 2) : self := Succ(Succ(Zero()))
        ? self := Succ(delegate.decrement(x))
    )
```

#### 16) New keyword `constructor` (short-form `ctor`)
In order to simplify the error recovery process, we introduce an additional keyword `constructor` (short-form `ctor`) that now proceeds the constructors of classes.

*Before*
```
    def class ZeroVectorN: Tuple
    {
        ZeroVectorN(n: Nat, field: Field)
        {
            dec
                ~i: Nat 
                base.Tuple()
                for i in [1~n] 
                (
                    self[i]:=field.AdditiveGroup().NeutralElement()
                )
            ;
            self
        }
    }
```
*Now*
```        
    def class ZeroVectorN: Tuple
    {
        ctor ZeroVectorN(n: Nat, field: Field)
        {
            dec
                ~i: Nat 
                base.Tuple()
                for i in [[1,n]] 
                (
                    self[i]:=field.AdditiveGroup().NeutralElement()
                )
            ;
            self
        }
    }
```

#### 17) Recognition of misplaced keywords and generic types
The recognition and error reporting of misplaced keywords (for instance, conflicts between variable names and keywords or variable and templates has been improved.

#### 18) Changes in localizations
* The separator between choices of localization strings was changed from `|` to `,` to prevent false positives when emitting error recovery diagnostics for case conditions (see 15).
* The separator between translations was changed from `~` to `!` to prevent false positives when 
  emitting error recovery diagnostics for variable declarations (see 2).

#### 19) changes in the properties

The property marker keyword `mandatory` (short form `mand`) were abandoned and there is a marker keyword `property` (short form `prty`). A property becomes mandatory per default unless the modifier `optional` (short form `opt`) is used. 

*Before*
```
    mandatory predicate SomePredicate() 
    {
        ...
    }

    optional predicate SomeOtherPredicate() 
    {
        ...
    }
```
*Now*
```        
    property predicate SomePredicate() 
    {
        ...
    }

    property optional predicate SomePredicate() 
    {
        ...
    }
```

#### 20) Simplification of the syntax of theorem-like statements, conjectures, and corollaries

In previous versions of FPL, the syntax of theorem-like statements, conjectures and corollaries was similar to that of inference rules, i.e., their blocks consisted of a separate premise and a separate conclusion specification. 
 This choice had the unnecessary consequence that if a theorem-like statement, conjecture, or corollary had no premise (which is sometimes the case in proof-based mathematics), 
 it had still be declared with `premise: undefined`.

 In the new version of FPL, the syntax of blocks inside theorem-like statements, conjectures and corollaries are exactly the same as in axioms and consist only of a predicate expression.

*Before*
```
    theorem SomeTheorem() 
    {
        premise: undefined
        conclusion: 
            all x,y in N
            (
                impl
                (
                    not ( x = y )
                    ,
                    not ( Successor(x) = Successor(y) )
                )
            )
    }
```
*Now*
```        
    theorem SomeTheorem() 
    {
        all x,y in N
        (
            impl
            (
                not ( x = y )
                ,
                not ( Successor(x) = Successor(y) )
            )
        )
    }
```

#### 21) Simplified syntax of namespaces and *.fpl file names
In the original FPL parser, every FPL file contained a namespaces with a block inside curly brackets. The name of the file could deviate from the name of the namespace.

The current FPL parser simply uses filename as the name of the namespce (without the extension `.fpl` ). The basename of the FPL file itself has to follow the rules of namespaces (i.e. be a concatenated dotted sequence of pascal-case ids). The need of writing curly brackets inside the FPL file and keeping track of which namespaces are inside which file names becomes so obsolete.

As a result, a syntax sugar from this simplification is that every FPL file has to end with a semicolon `;` to flag the parser that it does not has to look for any other building blocks in the file. 


#### 22) Additional inbuilt-predicate `bydef`
The additional predicate `bydef <variable>` is an abbreviation to of what had to be formulated in a more complicated way in second-order logic on a case-by-case basis. In principle, the predicate means to check if the asserted predicates used to define a variable justify an argument in an FPL proof.

#### 23) A more stringent usage of qualifiers, coordinates, and ranges

FPL supports the following qualifiers: dotted notation `x.something`, with arguments `x(something)`, with coordinates `x[something]`, with range `x[[something,something]]`, with subscript `x!something`. In general, all of them can be chained, for instance, a dotted notation can be chained with a subscripted one like this: `x.something!somethingelse`

There is a connection between qualifiers and identifiers, that are variables, the self keyword, pascal-cased FPL identifiers < PascalCasId >, index-typed digits< $digits >, and extension digits < extensionDigits >. This connection depends on whether identifiers can be used "with" qualifiers, "as" qualifiers, or both- The following table shows which identifiers can be used how with these qualifiers:

| Qualifier   | Variables |  self keyword | < PascalCasId > | < $digits > | < extensionDigits >
| :----:    | :----: | :----: |:----: |:----: |:----: |
| Dotted      |   both  | both    |   both  | -      |   -      |
| Arguments   |   both  | both    |   both  | as     |   as     |
| Coordinates |   both  | both    |   both  | as     |   as     |
| Ranges      |   both  | both    |   both  | as     |   as     |
| Subscripts  |   both  | both    |   both  | as     |   as     |

#### 24) Self-Containment 
This is not an amendment to the FPL parser. However, we want to significantly simplify the later recognition of self-containment in the FPL interpreter by the following convention:

* The order of declarations will now matter. 
* This is unlike the previous, python-based FPL interpreter (which can be found in the repository [https://github.com/bookofproofs/fpl](https://github.com/bookofproofs/fpl)).
* In the new FPL interpreter, checking if an FPL identifier was already declared, can be done - in principle - during the parsing process. This could significantly simplify the implementation and performance of the new FPL interpreter.
* Nevertheless, we stick to the 'must' requirements (see [INTRO.md](https://github.com/bookofproofs/fpl.net/blob/main/INTRO.md)) 28 (support of overrides), 38 (support recursive linguistic constructs), and 40 (support of self-reference in definitions) that could still potentially negatively impact how complicated it is to implement the new FPL interpreter.


## Amendments to the FPL interpreter 
### Amendments resulting from the FPL parser
* See Self-Containment
### More to come...
