﻿# Changes in FPL 
## Amendments to the FPL grammar, major change of the FPL parser   
The original grammar until version 1.2.1 was the EBNF input for an FPL parser implemented using Python and the **tatsu** parser generator.

From the version 2.0 on, there are some major changes to the grammar:
* The FPL parser was implemented from scratch, based on .NET (concretely the F# FParsec library). 
* Consequently, there is no separate EBNF input file since the grammar results from FParsec parsers build upon each other.
* There are amendments to the FPL syntax in the new implementation.

The following documentation describes the syntax amendments and provides a rationale behind each.

### General improvements of the Abstract Syntax Tree (AST)
* Consequent structuring of the AST using F# types 
* Skipping rule names not needed in the AST as specified in the FParsec parser
* A stricter syntax  
* A more careful placement of whitespace and comments.

### Error recovery
* Both the original Tatsu parser and the new FParsec-based parser do not provide in-built error recovery.
* However, the Fparsec-based parser provides more programmatic control. 
* An experimental approach to error recovery was added to the FPL parser.

### Changes to the Grammar (Details)

#### 1) No more extra blocks for uses clause, inference, theories, and localizations

In the original FPL version, a namespace contained four sections: the optional `uses` clause, the `inference` and `localization` blocks, and a mandatory `theory` block. There were syntactical differences in the blocks concerning whether a building block, like a theorem or an inference rule, required a prefix keyword or not. For instance, inference rules had no prefix inside their `inference` block, while theorems, axioms, and proof building blocks required the corresponding keyword inside the `theory` block. Moreover, the `uses` clause did not contain a block but was a comma-separated list. 

In the current version, the extra sub-blocks  `inference`, `localization`, `theory`, and comma separation in the `uses` clause were abandoned. Instead, each building block has a unique prefix keyword:

* Every namespace that could be referred to in a comma-separated list in the `uses` clause of previous FPL grammar versions now requires a separate `uses` keyword,
* Every inference rule starts with the keyword `inference` (short-form `inf`),
* Every localization starts with the keyword `localization` (short-form `loc`),
* Every class definition starts with the keyword `definition` (short-form `def`), followed as previously by the keyword `class` (short-form `cl`),
* Every predicate definition starts the keyword `definition` (short-form `def`), followed as previously by the keyword `predicate` (short-form `pred`), 
* Every functional term definition starts the keyword `definition` (short-form `def`), followed as previously by the keyword `function` (short-form `func`), 
* All other building block keywords remain unchanged. For example, axioms are started by the keyword `axiom` (short-form `ax`) or `postulate` (short-form `post`), theorems are started by the keyword `theorem` (short-form `them`), etc.
    
#### 2) In-block Variable Type Declarations and Statements
In FPL, you can declare variable types of building blocks in their signatures or body. Moreover, in the body also statements using variables are possible.
  The body type declarations and statements must start with the new keyword `declaration` (or `dec`) and end with a semicolon `;`. 

*Before*
``` 
{
    // Some types declarations
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
        // Some types declarations
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
This is a trade-off between simplicity of syntax and a stricter syntax concerning the distinction of type declarations and statements and what comes after them in the building block. In FPL, this depends on the kind of building block. For instance, functional terms require a return statement, while predicates require a predicate. With the new enclosing annotations `dec` and `;`, it becomes much easier to recognize missing linguistic components because this recognition can already be done in the parser on the syntactical level and has not been dealt with by the interpreter. These additional syntax changes will be discussed below.

The `~` at the beginning of type declarations is a syntax sugar necessary to distinguish between them and statements starting with a variable (like the assignment statement). The FParsec parser would allow us to eliminate this additional syntactical sugar using the `attempt` parser. Still, at the cost of a less intuitive error recovery messaging, if the syntax error occurred somewhere deep in the type declaration, the position of the reported error would point not to the error but to the beginning of the attempted type declaration. Thus, the `~` at the beginning of each type declaration is a trade-off between to-be-avoided syntax complexity and to-be-avoided `attempt` parsers for better error recovery messages.

#### 3) Classes allow multiple inheritance

In previous versions of the grammar, polymorphism of mathematical objects could only be achieved by using the `is` operator and asserting that the object is of some additional type than the type of the single parent class. In current versions, this is simplified because we can add as many types to the class as necessary.

The AST-Annotation for optional calls of parental constructors in classes (originally also optional but not annotated as such)

Bugfix: Classes can only be derived from a class type that now excludes functional terms and predicates (classes of FPL objects never intended to be derived from predicates or functional terms) 


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
class A: B, C
{
...
}
``` 

Moreover, inheritance is only possible from other class identifiers or the object keyword. Templates or extensions (see 13 below) are not possible. 

#### 4) Syntax of class constructors

The syntax of constructors allows calls of parental constructors in the `specification`; their syntax starts with `base.` followed by the parent class identifier and related parameters like in `base.ParentClass(first, second)` . Moreover, the keyword `self` has the last expression in the constructor. These two additional components disambiguate the representation of the class object after the constructor has been executed. 

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
In intrinsic definitions of classes, predicates, functional terms, and related properties, the new keyword `intrinsic` (short form `intr`) has to be used explicitly. It prevents the definition from being empty just because the end-user unintentionally left the body of a definition.

#### 6) `assert` becomes a statement (not a predicate)

*Before*
``` 
all x ( assert p(x) )
``` 
*Now*
``` 
all x ( p(x) )
``` 
The `assert` statement cannot be used where FPL expects a predicate. However, it is still possible to use `assert` in the context where statements are expected, particularly in the `declaration` section.  


#### 7) `for` loop replaces `loop` and `range`

In the original version of FPL grammar, loops could be constructed using the `loop` and the `range` keywords with the same syntax. Now, this is unified using the `for` keyword. Moreover, the `in` keyword makes the statement more readable.

*Before*
``` 
    range i x
    (
        // do something
    )

    loop i x
    (
        // do something
    )

``` 
*Now*
``` 
    for i in x
    (
        // do something
    )
``` 

The for statement comes in two flavors:
* iterating through all i in some type: `for i is <variableType> (...)`, 
* iterating through all i in some entity: `for i in <predicateWithQualification> (...)`,

Please note that the keyword `is` indicates that we want to loop over all entities of with a specific variable type, while the keyword `in` indicates that we want to loop over all entities as elements of some other entity. 

#### 8) New `exn` keyword  

The existence quantor accepting several allowed occurrences gets its own keyword `exn`, disambiguating it from the general quantor `ex`.

*Before*
``` 
    ex$1 x ( p (x) ) // there exists exactly one x ...

``` 
*Now*
``` 
    exn$1 x:obj ( p (x) ) // there exists exactly one x ...
``` 

This disambiguation helps formulate the FPL grammar without `attempt` parsers, which would otherwise distort error positions shown during error recovery inside the predicate.

#### 9) In-built equality predicate

The original FPL language had no inbuilt equality predicate, requiring a user-defined, second-order logic definition of equality, like this:
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

However, this approach seems not feasible from the implementation point of view because such a predicate would be hard, if not impossible, to interpret. Moreover, the equality sign `=` in the infix notation is so common in proof-based mathematics that it is preferable to FPL expressions using the user-defined `Equal` predicate.

In the new FPL syntax version, we introduce an inbuilt equality sign and allow infix notation for equality. Nevertheless, infix notation, together with the error recovery mechanism in the new FPL parser,
  requires disambiguation in the form that equality comparison has to be enclosed by some other characters that cannot be mixed up with other literals or predicates in FPL. We chose the enclosing characters `(` and `)` for this purpose:

*Before*
``` 
    Equals (x,y) 

``` 
*Now*
``` 
    (x = y)
``` 

Of course, the equality predicate can be placed anywhere a predicate can be used in FPL; for instance, it can be nested within other predicates. 

The equality comparison supports multiple equalities at once; for instance `( x = y = z )` is a shorter form for `and ((x = y), (y = z))`.

See also 24) to see how to extend FPL with other symbols.

#### 10) Named variable declarations are now allowed and even required in the quantors `all`, `ex`, and `exn`.

In the original version of FPL grammar, free variables used in quantors had to be first declared with a specific type and could then be only listed after the quantor and before the predicate of the quantor. In the new version of the FPL grammar, type declarations can be implicit by allowing the `in` keyword. 

*Before*
``` 
    dec: 
        x: Nat 
    ;
    all x ( p(x) ) // for all p of type Nat, the predicate p(x) holds

``` 
*Now*
``` 
    all x:Nat ( p (x) ) 
``` 

The syntax is similar to the `for` statement, i.e., it also allows the two flavors 
* iterating through all i in some type: `for i is <variableType> (...)`, 
* iterating through all i in some entity: `for i in <predicateWithQualification> (...)`,

Still, the syntax is s more flexible since it not only allows types but also can be enumerated, for instance, this the free variables `x` and `n` do not have to be declared. Their type will be inferred from how they are used in the `all` quantor:

``` 
    all x:Nat, n:func(obj)->obj
    ( 
        p (x,n) 
    ) 

``` 

Also, expressions in second-order logic predicates are allowed:

``` 
    axiom SchemaSeparation()
    {
        all p:pred, x,y:Set
        (
            ex y:Set
            (
                (y = SetBuilder(x,p))
            )
        )
    }
``` 

#### 11) Disambiguation of coordinate lists, ranges removed from FPL syntax

In previous versions, the square brackets `[`, `]` were used for both, coordinate lists and ranges. 
In the new version, ranges have been abandoned from the parser since dealing with them requires the concept of ordered entities that is in mathematics a semantic one. Implementing such structures in FPL, however, still remains possible using (possibly a combination of) definitions of predicates, classes, or functional terms.

#### 12) Delegates 

* The original prefix delegates `py` like in `py.some_delegate_name(x,y,z)` was replaced by the more general prefix `del` or `delegate`.
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

Extensions can be used in FPL (among other things) to introduce infinitely many new symbols, such as "0,1,2", etc., to identify them with concrete mathematical objects 
defined in FPL. For instance, if you defin natural numbers in FPL, you can map the symbols "0,1,2" with the semantical representations of natural numbers
as you have defined it in FPL.
Such literals can be injected into FPL using the at @ operater. For instance, we can write `@1`, and the parser will except it, while `1` will produce a syntax error.
In order to tell the FPL interpreter what `@1` means, you have to define a named extension that will map the (potentially infinitely many symbols) with concreate 
objects defined in FPL.

The @ at the beginning is used to change the parsing mode into a regex-based mode. All characters that follow after the @
except whitespace, the brackets `[]`, the parentheses `()`, the braces `{}`, and the comma `,` will be consumed by the parser.
The FPL interpreter will then try to apply the user-defined, named extension definition. If the string matches the regex-pattern declared in some named 
extension definition, it will assign the matched literal the user-defined name and apply the mapping to the FPL definition type.

* * Simplification of the extension syntax:
*Before*
``` 
    :ext
        extDigits : /\d+/
    :end

    // other stuff ...
    
    // constructor identifying the introduced digit symbols with natural numbers
    Nat(x: @extDigits)
    {
		...
    }

``` 

*Now*
``` 
    ext Digits x @ /\d+/ -> Nat
    {
        // map te regex pattern to FPL representations of natural numbers (example)
		dec ~n:Nat
		cases
		(
			| (x = @0) : n := Zero() 
			| (x = @1) : n := Succ(Zero()) 
			| (x = @2) : n := Succ(Succ(Zero())) 
			? n := Succ(delegate.Decrement(x))  
		)
		return n
    }

    // will be now interpreted as as a function taking a `Digits` type (as input) returning a `Nat` type (as output)
    @42 
``` 

* Indexing is done via brackets `[`, `]` instead of `$`
*Before*
``` 
    n$1 // (n subindex 1)
    n$m$k // (n subindex m subindex k)
``` 

*Now*
``` 
    n[@1]
    n[m,k]
``` 
* The symbols `m`, `k`, can be of any type. If n is variadic, only indexes $1, $2 are allowed. But this is something the FPL interpreter (and not the FPL parser) can ensure.

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
    [[1,5]] // (closed range with boundaries 1 and 5 )
    [(1,5]] // (left-open range with boundaries 1 and 5)
    [[1,5)] // (right-open range with boundaries 1 and 5)
    [(1,5)] // (left- and right-open range with boundaries 1 and 5)

    // boundaries can be disambiguated 
    // index type 
    [[$1,$5]] // (closed range with boundaries 1 and 5 )
    [($1,$5]] // (left-open range with boundaries 1 and 5)
    [[$1,$5)] // (right-open range with boundaries 1 and 5)
    [($1,$5)] // (left- and right-open range with boundaries 1 and 5)

``` 
Moreover, the boundaries can be of any type, not only of index and extension digits.


#### 14) Simplified syntax of proofs
The following changes have been made:
* Justifying arguments can now contain not only lists of 'primePredicate' but more general of 'predicate'.
* Derived arguments can now also reference the conclusion of the to-be-proven theorem
* A simplified syntax of referencing argumentIdentifiers (referencing via slash `/` is no longer necessary). Now, restating the same identifier is enough.
* Bugfix preventing syntax allowing assumptions followed by a justification (pure math standard: without justification)
* The referencing identifier using dollar digits (e.g., `$1` at the end of `SomeTheorem$1`) was extended to look like the indexed predicate (`SomeTheorem$1`), so there is no longer any difference between the two.
* `qed` is optional and not a derived argument. 
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
    proof SomeTheorem$1
    {
        dec
            ~a:A
            ~b:B
            ~c:C
            ~y,z: obj
        ;
        1. GreaterAB |- (a > b) 
        2. GreaterBC |- (b > c) 
        3. 1., 2. |- and ((a > b), (b > c)) 
        4. 3., GreaterTransitive |- ( and ((a > b), (b > c)) => (a > c) ) 
        5. 4., ModusPonens |- (a > c)
        6. 5., 1. |- and ((a > c), (a > b)) 
        7. 6., ExistsByExample(and((a > c), (a > b))) |- 
            ex x:obj  
                and ((x > y), (x > z)) 
        qed
    }
```

#### 15) Simplified syntax of the `cases` statement
* The keyword `case` is now discontinued and replaced by the literal `|`
* The sequence of cases starting with the literal `|` must end with a `?`, which denotes the default case.
* Thus, we use `?` instead of `else:.`
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
To simplify the error recovery process, we introduce an additional keyword, `constructor` (short-form `ctor`), that now proceeds the constructors of classes.

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
The recognition and error reporting of misplaced keywords (for instance, conflicts between variable names and keywords or variables and templates has been improved.

#### 18) Changes in localizations
* The separator between choices of localization strings was changed from `|` to `,` to prevent false positives when emitting error recovery diagnostics for case conditions (see 15).
* The separator between translations was changed from `~` to `!` to prevent false positives when 
  emitting error recovery diagnostics for variable declarations (see 2).

#### 19) Changes in the properties

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

    property predicate optional SomePredicate() 
    {
        ...
    }
```

Moreover, in previous versions, properties could have one of three types: they could be predicate instances, functional terms instances and class instances. The latter are abandoned because they can be expressed using functional term instances with the arity zero.

*Before*
```
    property optional tplSetElem NeutralElement()
    {
        dec assert IsNeutralElement(self);
        self
    }

    property CommutativeGroup AdditiveGroup()
    {
        dec
            self:= CommutativeGroup(myX, myAdd)
        ;
        self
    }
```
*Now*
```        
    property func optional NeutralElement() -> tplSetElem
    {
        dec 
            ~result:tplSetElem 
            assert IsNeutralElement(result)
        ;
        return result
    }

    property func AdditiveGroup() -> CommutativeGroup
    {
        return CommutativeGroup(myX, myAdd)
    }
```

This approach also significantly simplifies the recognition of duplicate signatures of properties in the same scope.

#### 20) Simplification of the syntax of theorem-like statements, conjectures, and corollaries

In previous versions of FPL, the syntax of theorem-like statements, conjectures, and corollaries was similar to that of inference rules; their blocks consisted of a separate premise and a separate conclusion specification. 
 This choice had the unnecessary consequence that if a theorem-like statement, conjecture, or corollary had no premise (which is sometimes the case in proof-based mathematics), 
 it had still been declared with `premise: undefined.`

 In the new version of FPL, the syntax of blocks inside theorem-like statements, conjectures, and corollaries is exactly the same as in axioms, and they consist only of a predicate expression.

*Before*
```
    theorem SomeTheorem() 
    {
        premise: undefined
        conclusion: 
            all x,y : N
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
        all x,y : N
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
In the original FPL parser, every FPL file contained namespaces with a block inside curly brackets. The name of the file could deviate from the name of the namespace.

The current FPL parser uses the filename as the namespace's name (without the extension `.fpl` ). The basename of the FPL file itself has to follow the rules of namespaces (i.e., be a concatenated dotted sequence of Pascal-case IDs). The need to write curly brackets inside the FPL file and keep track of which namespaces are inside which file names becomes so obsolete.

As a result, a syntax sugar from this simplification is that every FPL file has to end with a semicolon `;` to tell the parser not to look for any other building blocks in the file. 


#### 22) Additional inbuilt-predicate `bydef`
The additional predicate `bydef <variable>` is an abbreviation for what had to be formulated more complicatedly in second-order logic on a case-by-case basis. In principle, the predicate means to check if the asserted predicates used to define a variable justify an argument in an FPL proof.

#### 23) A more stringent usage of qualifiers, coordinates, and ranges

FPL supports the following qualifiers: dotted notation `x.something`, with arguments `x(something)`, with coordinates `x[something]`, and with a range `x[[something,something]]`. 

Note: The subscripted notation `x$something` available in the original version of FPL was removed because it was equivalent to coordinate notation. However, it is still available in the signatures of corollaries and proofs. 

In general, all of the above-mentioned qualifiers can be chained; for instance, a dotted notation can be chained with a coordinate one like this: `x.something[somethingelse]`.

There is a connection between qualifiers and identifiers, that are variables, the self keyword, pascal-cased FPL identifiers < PascalCasId >, index-typed digits< $digits >, and extension digits < extensionDigits >. This connection depends on whether identifiers can be used "with" qualifiers, "as" qualifiers, or both- The following table shows which identifiers can be used how with these qualifiers:

| Qualifier   | Variables |  self keyword | < PascalCasId > | < $digits > | < extensionDigits >
| :----:    | :----: | :----: |:----: |:----: |:----: |
| Dotted      |   both  | both    |   both  | -      |   both     |
| Arguments   |   both  | both    |   both  | as     |   both     |
| Coordinates |   both  | both    |   both  | as     |   both     |
| Ranges      |   both  | both    |   both  | as     |   both     |


#### 24) User-Defined Symbols for Predicates and Functional terms, and Object Symbols

FPL's standard syntax for calling user-defined functional terms or predicates requires their Pascal-case identifier, followed by a comma-separated list of parameters in parentheses, like in 
`SomeIdentifier(x,y)`. 

Besides the inbuilt equality operator `=`, many new, user-defined infix symbols like `+`, `-`, `∈`, etc., can be injected into FPL and used instead of the standard syntax described above. For instance, instead of writing `Add(x,y)`, we can no tell the FPL to interpret the input `(x + y)` to mean `Add(x,y)`.

The same holds for symbols representing mathematical objects rather than operators like infinity `∞` of angle `∠`. 

The FPL parser will accept all these symbols when placed properly in the FPL code. However, we must identify them within the FPL definitions of functional terms, predicates, and classes to disambiguate them. Then, the FPL interpreter must check if a symbol was declared and is being used properly. 

To accomplish this behavior for the FPL interpreter, we have to do two things: 
1. Declare the prefix, postfix, or infix symbols within the definition of the functional terms or predicates you want to abbreviate like this; similarly, declare the object symbol within the definition of a class.
1. Use these symbols instead of the long notation in your FPL code.  

##### Example of Prefix-Notation definition

```
    // definition of a prefix notation
    def func Minus prefix "-" (x: Int) -> Int
    {
       // ...
    }

    // Example usages
    -a
    -b
    -(x + y)

```
Whitespace characters are insignificant in the infix notation: 
```
    (x + -y) // correct
    (x + - y) // still correct, the "+" will be parsed as infix operation, the "-" as a prefix operation

```

##### Example of Infix Definition and Usage

```
    // definition of an infix notation
    def func Add infix "-" (x,y: Int) -> Int
    {
        // ...
    }

    // Example usages
    -(x + y)
    (x + y + z) // equivalent to ((x + y) + z)
    (x + (y + z))

```
The infix notation is, by default, left-associative. If you want another 

##### Example of Postfix Definition and Usage

```
    // definition of a postfix notation
    def func Successor postfix "'" (x: Nat) -> Nat
    {
        // ...
    }
    def func Factorial postfix "!" (x: Nat) -> Nat
    {
        // ...
    }

    // example usage
    x'
    x!

```
Whitespace characters are significant in postfix notation:
```
    x' // correct
    x ' // wrong, (syntax error)
```

Postfix notation can be composed by applying parentheses:
```
    (x')' // correct
    x'' // wrong, since '' is a different operator as '.
```
In cases you want to use the shorter notation `x''` instead of `(x')'`, you will have to define it separately: 
```
    // definition of a postfix notation
    def func SuccessorOfSuccessor postfix "''" (x: Nat) -> Nat
    {
        return (x')'
    }

    // Example usage
    x'' // now, this is equivalent to (x')'

```

##### Example of Object Symbol Definition and Usage

```
def class EmptySet symbol "∅": Set
{
    // ...
}

// Example usage
x := ∅ // this is equivalent to x := EmptySet()
```

#### 25) Self-Containment 
It is not an amendment to the FPL parser. However, we want to significantly simplify the later recognition of self-containment in the FPL interpreter by the following convention:

* The order of declarations will now matter. 
* This is unlike the previous, python-based FPL interpreter (which can be found in the repository [https://github.com/bookofproofs/fpl](https://github.com/bookofproofs/fpl)).
* In the new FPL interpreter, checking if an FPL identifier was already declared can - in principle - be done during the parsing process. It could significantly simplify the implementation and performance of the new FPL interpreter.
* Nevertheless, we stick to the 'must' requirements (see [INTRO.md](https://github.com/bookofproofs/fpl.net/blob/main/INTRO.md)) 28 (support of overrides), 38 (support recursive linguistic constructs), and 40 (support of self-reference in definitions) that could still potentially negatively impact how complicated it is to implement the new FPL interpreter.


#### 26) `self` vs. `parent` reference
* The keyword `self` had in previous version be proceeded with the at sign `@` like in `@self` in order to refer to the parent definition when used in a property of this definition.
* Now, we use the new keyword `parent` instead of `@self`.
* Rationale: 
    * The literal `@` is now free for syntax extensions (see 13).
    * Syntactically, previous versions of FPL allowed using multiple ats like in `@@self`. However, only one `@` is sufficient.

*Before*
```
    @self
```
*Now*
```         
parent
```

#### 26) Changing `and`, `or`, `xor` from n-ary to binary 
* The predicates `and`, `or`, `xor` could in previous versions be written as n-ary operations.
* Now, they require the standard binary syntax. More than two arguments have to be nested
* Rationale: 
    * It was unclear how to interpret only one argument.
    * As a remedy, FPL allows introducing infix operations to get rid of nesting.

*Before* (example for `and`, analogouly for `xor` and `or`)
```
    and (true, false, false)
    and (true) // allowed 
```
*Now*
```         
    and (and(true, false), false)
    // and (true) causes now a syntax error
```