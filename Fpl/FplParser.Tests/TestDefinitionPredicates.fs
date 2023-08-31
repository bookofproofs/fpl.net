namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestDefinitionPredicates () =

    [<TestMethod>]
    member this.TestDefinitionPredicate01 () =
        let result = run definitionPredicate """pred IsGreaterOrEqual(n,m: Nat)
        {
            k: Nat
            ex k ( Equal(n,Add(m,k)) )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["IsGreaterOrEqual"],
      [([Var "n"; Var "m"],
        VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))]),
   (([BlockVariableDeclaration
        ([Var "k"], VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))],
     Some
       (Exists
          ([Var "k"],
           PredicateWithArgs
             (AliasedId ["Equal"],
              [PredicateWithoutArgs (Var "n");
               PredicateWithArgs
                 (AliasedId ["Add"],
                  [PredicateWithoutArgs (Var "m");
                   PredicateWithoutArgs (Var "k")])])))), None))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate02 () =
        let result = run definitionPredicate """pred IsBounded(x: Real)
        {
            upperBound, lowerBound: Real
            ex upperBound, lowerBound
            (
                and (LowerEqual(x,upperBound), LowerEqual(lowerBound,x))
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["IsBounded"],
      [([Var "x"], VariableTypeWithModifier (None, ClassHeaderType ["Real"]))]),
   (([BlockVariableDeclaration
        ([Var "upperBound"; Var "lowerBound"],
         VariableTypeWithModifier (None, ClassHeaderType ["Real"]))],
     Some
       (Exists
          ([Var "upperBound"; Var "lowerBound"],
           And
             [PredicateWithArgs
                (AliasedId ["LowerEqual"],
                 [PredicateWithoutArgs (Var "x");
                  PredicateWithoutArgs (Var "upperBound")]);
              PredicateWithArgs
                (AliasedId ["LowerEqual"],
                 [PredicateWithoutArgs (Var "lowerBound");
                  PredicateWithoutArgs (Var "x")])]))), None))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate03 () =
        let result = run definitionPredicate """pred IsBounded(f: RealValuedFunction)
        {
            x: Real
            all x
            (
                IsBounded(f(x))
            )
        }
"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["IsBounded"],
      [([Var "f"],
        VariableTypeWithModifier (None, ClassHeaderType ["RealValuedFunction"]))]),
   (([BlockVariableDeclaration
        ([Var "x"], VariableTypeWithModifier (None, ClassHeaderType ["Real"]))],
     Some
       (All
          ([Var "x"],
           PredicateWithArgs
             (AliasedId ["IsBounded"],
              [PredicateWithArgs (Var "f", [PredicateWithoutArgs (Var "x")])])))),
    None))"""
        let exp1 = expected.Replace("\r","")
        Assert.AreEqual(exp1, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate04 () =
        let result = run definitionPredicate """pred Equal(a,b: tpl)
        {
            p: pred 

			all p
			(
				iif
				(
					p(a),
					p(b)
				)
			)
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["Equal"],
      [([Var "a"; Var "b"], VariableTypeWithModifier (None, TemplateType "tpl"))]),
   (([BlockVariableDeclaration
        ([Var "p"], VariableTypeWithModifier (None, PredicateType))],
     Some
       (All
          ([Var "p"],
           Iif
             (PredicateWithArgs (Var "p", [PredicateWithoutArgs (Var "a")]),
              PredicateWithArgs (Var "p", [PredicateWithoutArgs (Var "b")]))))),
    None))"""
        let exp1 = expected.Replace("\r","")
        Assert.AreEqual(exp1, actual);


    [<TestMethod>]
    member this.TestDefinitionPredicate05 () =
        let result = run definitionPredicate """pred NotEqual(x,y: tpl)
        {
            not
            (
                Equal(x,y)
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["NotEqual"],
      [([Var "x"; Var "y"], VariableTypeWithModifier (None, TemplateType "tpl"))]),
   (([],
     Some
       (Not
          (PredicateWithArgs
             (AliasedId ["Equal"],
              [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (Var "y")])))),
    None))"""
        let exp1 = expected.Replace("\r","")
        Assert.AreEqual(exp1, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate06 () =
        let result = run definitionPredicate """pred AreRelated(u,v: Set, r: BinaryRelation)
        {
            one, two:Nat
            one := Nat(1)
            two := Nat(2)
            tuple: Tuple[one~two]
            tuple:=Tuple(u,v)
            assert
                and
                (
                    In(tuple,r),
                    In(u,r.Domain()),
                    In(v,r.Codomain())
                )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["AreRelated"],
      [([Var "u"; Var "v"],
        VariableTypeWithModifier (None, ClassHeaderType ["Set"]));
       ([Var "r"],
        VariableTypeWithModifier (None, ClassHeaderType ["BinaryRelation"]))]),
   (([BlockVariableDeclaration
        ([Var "one"; Var "two"],
         VariableTypeWithModifier (None, ClassHeaderType ["Nat"]));
      BlockStatement
        (Assignment
           (Var "one",
            PredicateWithArgs
              (AliasedId ["Nat"], [PredicateWithoutArgs (ExtDigits "1")])));
      BlockStatement
        (Assignment
           (Var "two",
            PredicateWithArgs
              (AliasedId ["Nat"], [PredicateWithoutArgs (ExtDigits "2")])));
      BlockVariableDeclaration
        ([Var "tuple"],
         VariableTypeWithModifier
           (None,
            FplTypeWithRange
              ((ClassHeaderType ["Tuple"], LeftClosed),
               (RangeInType (Some (Var "one"), Some (Var "two")), RightClosed))));
      BlockStatement
        (Assignment
           (Var "tuple",
            PredicateWithArgs
              (AliasedId ["Tuple"],
               [PredicateWithoutArgs (Var "u"); PredicateWithoutArgs (Var "v")])));
      BlockStatement
        (Assertion
           (And
              [PredicateWithArgs
                 (AliasedId ["In"],
                  [PredicateWithoutArgs (Var "tuple");
                   PredicateWithoutArgs (Var "r")]);
               PredicateWithArgs
                 (AliasedId ["In"],
                  [PredicateWithoutArgs (Var "u");
                   QualifiedIdentifier
                     (Var "r", [PredicateWithArgs (AliasedId ["Domain"], [])])]);
               PredicateWithArgs
                 (AliasedId ["In"],
                  [PredicateWithoutArgs (Var "v");
                   QualifiedIdentifier
                     (Var "r", [PredicateWithArgs (AliasedId ["Codomain"], [])])])]))],
     None), None))"""
        let exp1 = expected.Replace("\r","")
        Assert.AreEqual(exp1, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate07 () =
        let result = run definitionPredicate """pred Greater(x,y: obj)
        {

        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["Greater"],
      [([Var "x"; Var "y"], VariableTypeWithModifier (None, ObjectType))]),
   (([], None), None))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate08 () =
        let result = run definitionPredicate """pred IsPowerSet(ofSet, potentialPowerSet: Set)
        {
            z: Set
            all z
            (
                impl (Subset(z,ofSet), In(z, potentialPowerSet))
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["IsPowerSet"],
      [([Var "ofSet"; Var "potentialPowerSet"],
        VariableTypeWithModifier (None, ClassHeaderType ["Set"]))]),
   (([BlockVariableDeclaration
        ([Var "z"], VariableTypeWithModifier (None, ClassHeaderType ["Set"]))],
     Some
       (All
          ([Var "z"],
           Impl
             (PredicateWithArgs
                (AliasedId ["Subset"],
                 [PredicateWithoutArgs (Var "z");
                  PredicateWithoutArgs (Var "ofSet")]),
              PredicateWithArgs
                (AliasedId ["In"],
                 [PredicateWithoutArgs (Var "z");
                  PredicateWithoutArgs (Var "potentialPowerSet")]))))), None))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDefinitionPredicate09 () =
        let result = run definitionPredicate """pred Union(x,superSet: Set)
        {
            u: Set
            all u
            (
                impl (In(u, x), In(u, superSet))
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: DefinitionPredicate
  (Signature
     (AliasedId ["Union"],
      [([Var "x"; Var "superSet"],
        VariableTypeWithModifier (None, ClassHeaderType ["Set"]))]),
   (([BlockVariableDeclaration
        ([Var "u"], VariableTypeWithModifier (None, ClassHeaderType ["Set"]))],
     Some
       (All
          ([Var "u"],
           Impl
             (PredicateWithArgs
                (AliasedId ["In"],
                 [PredicateWithoutArgs (Var "u"); PredicateWithoutArgs (Var "x")]),
              PredicateWithArgs
                (AliasedId ["In"],
                 [PredicateWithoutArgs (Var "u");
                  PredicateWithoutArgs (Var "superSet")]))))), None))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

        