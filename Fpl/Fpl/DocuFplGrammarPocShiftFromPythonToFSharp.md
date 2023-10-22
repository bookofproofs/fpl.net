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

#### 1) In-block Variable Declarations and Statements
In FPL, you can declare variables of building blocks both in their signatures or in their body. The declarations in the body must be started by a new keyword `declaration` (or `dec`) and ended by a semicolon `;`. The statements in the body must be started by a new keyword `specification` (or `spec` and ended by a semicolon `;`.

*Before*
``` 
{
    // some declarations
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
    dec: 
        myField: Field 
        addInField: BinOp
        mulInField: BinOp
    ; 

    spec: 
        myField := field
        addInField := myField.AddOp()
        mulInField := myField.MulOp()
    ; 
}
``` 
This is a trade-off between simplicity of syntax and a stricter syntax with respect to what comes after variable declarations and specifications. In FPL, this depends on the kind of a building block. For instance, functional terms require a return statement while predicates require a predicate. With the new annotations `dec` and `spec` it becomes much more easier to recognize missing linguistic components, because this recognition can be done already in the parser on the syntactical level and have not be dealt with by the interpreter. These additional syntax changes will be discussed below. 

#### 2) Classes allow multiple inheritance

In previous version of the grammar, polymorphism of mathematical object could only be achieved by using the `is` operator and asserting that the object is of some additional type than the type of the single parent class. Now, this is simplified because we can add as many types to the class as necessary.

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

#### 3) Syntax of class constructors

The syntax of constructors allows calls of parental constructors in the `specification`, their syntax starts with `self!" followed by the parent class identifier and related parameters like in `self!ParentClass(first, second)` . Moreover, the keyword `self` has as the last expression in the constructor. These two additional components disambiguate the representation of the class object after the constructor has been executed. 

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
    	spec:
            myField := field
            self!ParentClass()
        ;
        self
    }
}
``` 

#### 4) Explicit intrinsic definitions

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

#### 5) `assert` becomes a statement (not a predicate)

*Before*
``` 
all x ( assert p(x) )
``` 
*Now*
``` 
all x ( p(x) )
``` 
The `assert` statement cannot be used, where FPL expects a predicate. Using `assert` is still possible in the context where statements are expected, in particular in the `specification` section. 


#### 6) `for` loop replaces `loop` and `range`

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

#### 7) New `exn` keyword  

The existence quantor accepting a number of allowed occurrences gets an own keyword `exn`, disambiguating it from the general quantor `ex`.

*Before*
``` 
    ex$1 x ( p (x) ) // there exists exactly one x ...

``` 
*Now*
``` 
    exn!1 x ( p (x) ) // there exists exactly one x ...
``` 

This disambiguation helps to formulate the FPL grammar without `attempt` parsers which would otherwise distort error positions shown during error recovery that occur inside the predicate.

#### 8) In-built equality predicate

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
  requires disambiguation in the form that equality comparison have to be enclosed by some other characters that cannot be mixed up with other literals or predicates in FPL. We chose the enclosing characters `<` and `>` for this purpose:

*Before*
``` 
    Equals (x,y) 

``` 
*Now*
``` 
    <x = y>
``` 

Of course, the equality predicate can be placed everywhere any predicate can be used in FPL, for instance, it can be nested within other predicates. 

#### 9) Domains are now allowed in the quantors `all`, `ex`, and `exn`.

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
                <y = SetBuilder(x,p)>
            )
        )
    }
``` 

#### 10) Disambiguation of coordinate lists and ranges

In previous versions, the square brackets `[`, `]` were used for both, coordinate lists and ranges. In the new version, coordinates have to be placed in angle brackets `<` and `>`. This disambiguates the grammar for to improve the inbuilt error recovery mechanism.

*Before*
``` 
    for i in [1~n]  // <- range
    (
        a[i,j]:= b // <- coordinates
    )

``` 
*Now*
``` 
    for i in [1~n]  // <- range
    (
        a<i,j>:= b // <- coordinates
    )
``` 


### More stringent predicate syntax
* Giving up mixing up statements and index variables being not predicative in the choice rule of Prime predicates 
* This mixing is now only possible for the isOperator.
* The `all` compound predicate now allows iterating through contents variadic variables containing predicates. For instance, for the types `a: pred b: +pred`,   instead of writing a loop - assert statement construct like `loop a b (assert a)` we can also write `all a b (a)`. This way, the expression becomes a predicate, not a statement in the AST, allowing to interpret it appropriately.

### Better handling of keywords and generic templates
* Better recognition and error reporting of conflicts between variable names and keywords 
* Better recognition and error reporting of conflicts between variable names and template names

### Keyword `delegate` or `del` instead of `py`
* The original Python `py` prefix followed pythonic delegates like `py.some_delegate_name(x,y,z)` are now replaced by the more general prefix `del` or `delegate`
* The names for the delegates are now generalized from the original regex `@"[a-z_]*"` to `@"[a-z_A-Z][a-z_A-Z0-9]+"`

### Simplified Syntax for the `cases` statement
* The keyword `case` is now discontinued and replaced by the literal `|`
* The sequence of cases starting by the literal `|` must end by a semicolon
* After this semicolon, the else case does not require a colon, instead of `else:` we simply write `else`.
* Rationale: 
    * Simplified syntax since `case` is likely to be mistaken for the existing keyword `cases`
    * `|` is intuitively similar to the BNF or regex 'OR' character 
    * Improved readability

### Classes 
* AST-Annotation for optional calls of parental constructors in classes (originally also optional but not annotated as such)
* Bugfix: Classes can only be derived from classType that now excludes functional terms and predicates (classes of FPL objects never intended to be derived from predicates or functional terms) 

### Enhancement of Proofs
* Justifying arguments can now contain not only lists of 'primePredicate' but more general of 'predicate'
* Derived arguments can now also reference the conclusion of the to-be-proven theorem
* A simplified syntax of referencing argumentIdentifiers (referencing via slash `/` is no longer necessary). Now, restating the same identifier is enough.
* Bugfix preventing syntax allowing assumptions followed by a justification (pure math standard: without justification)
* Improved readability, for instance, instead of 
```
    proof Example4$1
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
* we can write 
```        
    proof Example4$1
    {
        a:A
        b:B
        c:C
        x,y,z: obj

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

### Self-Containment 
* This is not an amendment to the FPL parser. However, we want to significantly simplify the later recognition of self-containment in the FPL interpreter by the following convention:
    * The order of declarations will now matter. 
    * This is unlike the previous, python-based FPL interpreter (which can be found in the repository [https://github.com/bookofproofs/fpl](https://github.com/bookofproofs/fpl)).
    * In the new FPL interpreter, checking if an FPL identifier was already declared can be done - in principle - during the parsing process. This could significantly simplify the implementation and performance of the new FPL interpreter.
    * Nevertheless, we stick to the 'must' requirements (see [INTRO.md](https://github.com/bookofproofs/fpl.net/blob/main/INTRO.md)) 28 (support of overrides), 38 (support recursive linguistic constructs), and 40 (support of self-reference in definitions) that could still potentially negatively impact how complicated it is to implement the new FPL interpreter.

### Namespaces
* A single *.fpl file can now contain more than one namespace. This will significantly simplify later preprocessing when the FPL parser needs to include namespaces via the `uses` keyword. 
* Moreover, it provides more flexibility to end-users
* Since 'order of declarations now matter' (see Self-Containment above), we have to discontinue the possibility of including FPL namespaces using wildcards like in `Fpl.Commons.*`) since it may be undecidable in which order they have to be included.
* The `uses` clause has a separate block enclosed by `{` and `}` so it is not necessary to repeate the `uses` keyword in each line.
* The `uses` clause has to be made explicitly, even if it is empty. The same holds for the inference block and the localization block. Thus, an empty, syntactically correct namespace in FPL looks now like this: 

```
    TestNamescpace {
        uses {}
        inference {}
        theory {}
        localization {}
    }
```

### Extensions of FPL  
* In the Proof of Concept for the syntax of FPL, we have only one extension for digit literals to identify them in FPL with a mathematical definition of natural numbers. These literals are 0, 1, 2, ...
* In previous versions of the FPL grammar (before 2.4.2), there was a conflict in the syntax between these symbols and the symbols for using inbuilt index values. Now, the latter start with a dollar: $0, $1, $2, ....
* This difference will make it easier in the FPL interpreter to disambiguate the two.

## Amendments to the FPL interpreter 
### Amendments resulting from the FPL parser
* See Self-Containment
### More to come...
