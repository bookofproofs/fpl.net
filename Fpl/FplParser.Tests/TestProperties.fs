namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type TestProperties () =

    [<TestMethod>]
    member this.TestProperty01 () =
        let result = run property """mand func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to]) -> tplFieldElem[from ~ to]
	        {
	            self[from ~ to]:=addInField(v[from ~ to],w[from ~ to])
	        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Mandatory,
   FunctionalTermInstance
     ((Signature
         (AliasedId ["VecAdd"],
          [([Var "from"; Var "to"],
            VariableTypeWithModifier (None, ClassHeaderType ["Nat"]));
           ([Var "v"; Var "w"],
            VariableTypeWithModifier
              (None,
               FplTypeWithRange
                 ((TemplateType "tplFieldElem", LeftClosed),
                  (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed))))]),
       VariableTypeWithModifier
         (None,
          FplTypeWithRange
            ((TemplateType "tplFieldElem", LeftClosed),
             (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed)))),
      [BlockStatement
         (Assignment
            (EntityWithCoord
               (Self [],
                ClosedOrOpenRange
                  ((LeftClosed, (Some (Var "from"), Some (Var "to"))),
                   RightClosed)),
             PredicateWithArgs
               (Var "addInField",
                [PredicateWithoutArgs
                   (EntityWithCoord
                      (Var "v",
                       ClosedOrOpenRange
                         ((LeftClosed, (Some (Var "from"), Some (Var "to"))),
                          RightClosed)));
                 PredicateWithoutArgs
                   (EntityWithCoord
                      (Var "w",
                       ClosedOrOpenRange
                         ((LeftClosed, (Some (Var "from"), Some (Var "to"))),
                          RightClosed)))])))]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty02 () =
        let result = run property """mandatory Nat Length()
            {
                return myLength
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Mandatory,
   ClassInstance
     ((VariableTypeWithModifier (None, ClassHeaderType ["Nat"]),
       Signature (AliasedId ["Length"], [])),
      [BlockStatement (Return (PredicateWithoutArgs (Var "myLength")))]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty03 () =
        let result = run property """mandatory tpl Coord(i: Nat)
            {
                return self(i)
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Mandatory,
   ClassInstance
     ((VariableTypeWithModifier (None, TemplateType "tpl"),
       Signature
         (AliasedId ["Coord"],
          [([Var "i"], VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))])),
      [BlockStatement
         (Return (PredicateWithArgs (Self [], [PredicateWithoutArgs (Var "i")])))]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty04 () =
        let result = run property """mandatory tplSetElem NeutralElem()
            {
                self:=myOp.NeutralElement()
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Mandatory,
   ClassInstance
     ((VariableTypeWithModifier (None, TemplateType "tplSetElem"),
       Signature (AliasedId ["NeutralElem"], [])),
      [BlockStatement
         (Assignment
            (Self [],
             QualifiedIdentifier
               (Var "myOp",
                [PredicateWithArgs (AliasedId ["NeutralElement"], [])])))]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty05 () =
        let result = run property """mandatory func InverseOf(x: tplSetElem) -> tplSetElem
            {
                val: tplSetElem
                assert
                    and
                    (
                        Equal( myOp(x,val), self.NeutralElem()),
                        Equal( myOp(val,x), self.NeutralElem())
                    )
                ret val
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Mandatory,
   FunctionalTermInstance
     ((Signature
         (AliasedId ["InverseOf"],
          [([Var "x"],
            VariableTypeWithModifier (None, TemplateType "tplSetElem"))]),
       VariableTypeWithModifier (None, TemplateType "tplSetElem")),
      [BlockVariableDeclaration
         ([Var "val"],
          VariableTypeWithModifier (None, TemplateType "tplSetElem"));
       BlockStatement
         (Assertion
            (And
               [PredicateWithArgs
                  (AliasedId ["Equal"],
                   [PredicateWithArgs
                      (Var "myOp",
                       [PredicateWithoutArgs (Var "x");
                        PredicateWithoutArgs (Var "val")]);
                    QualifiedIdentifier
                      (Self [],
                       [PredicateWithArgs (AliasedId ["NeutralElem"], [])])]);
                PredicateWithArgs
                  (AliasedId ["Equal"],
                   [PredicateWithArgs
                      (Var "myOp",
                       [PredicateWithoutArgs (Var "val");
                        PredicateWithoutArgs (Var "x")]);
                    QualifiedIdentifier
                      (Self [],
                       [PredicateWithArgs (AliasedId ["NeutralElem"], [])])])]));
       BlockStatement (Return (PredicateWithoutArgs (Var "val")))]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty06 () =
        let result = run property """optional pred IsAssociative()
            {
                a,b,c: tplSetElem
                all a,b,c
                (
                    Equal
                    (
                        @self(a,@self(b,c)),
                        @self(@self(a,b),c)
                    )
                )
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Optional,
   PredicateInstance
     (Signature (AliasedId ["IsAssociative"], []),
      ([BlockVariableDeclaration
          ([Var "a"; Var "b"; Var "c"],
           VariableTypeWithModifier (None, TemplateType "tplSetElem"))],
       All
         ([Var "a"; Var "b"; Var "c"],
          PredicateWithArgs
            (AliasedId ["Equal"],
             [PredicateWithArgs
                (Self ['@'],
                 [PredicateWithoutArgs (Var "a");
                  PredicateWithArgs
                    (Self ['@'],
                     [PredicateWithoutArgs (Var "b");
                      PredicateWithoutArgs (Var "c")])]);
              PredicateWithArgs
                (Self ['@'],
                 [PredicateWithArgs
                    (Self ['@'],
                     [PredicateWithoutArgs (Var "a");
                      PredicateWithoutArgs (Var "b")]);
                  PredicateWithoutArgs (Var "c")])])))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty07 () =
        let result = run property """optional pred IsLeftNeutralElement(e: tplSetElem)
            {
                Equal(@self(e,x), x)
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Optional,
   PredicateInstance
     (Signature
        (AliasedId ["IsLeftNeutralElement"],
         [([Var "e"], VariableTypeWithModifier (None, TemplateType "tplSetElem"))]),
      ([],
       PredicateWithArgs
         (AliasedId ["Equal"],
          [PredicateWithArgs
             (Self ['@'],
              [PredicateWithoutArgs (Var "e"); PredicateWithoutArgs (Var "x")]);
           PredicateWithoutArgs (Var "x")]))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty08 () =
        let result = run property """optional pred IsNeutralElement(e: tplSetElem)
            {
                and (IsLeftNeutralElement(e), IsRightNeutralElement(e))
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Optional,
   PredicateInstance
     (Signature
        (AliasedId ["IsNeutralElement"],
         [([Var "e"], VariableTypeWithModifier (None, TemplateType "tplSetElem"))]),
      ([],
       And
         [PredicateWithArgs
            (AliasedId ["IsLeftNeutralElement"],
             [PredicateWithoutArgs (Var "e")]);
          PredicateWithArgs
            (AliasedId ["IsRightNeutralElement"],
             [PredicateWithoutArgs (Var "e")])])))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty09 () =
        let result = run property """optional pred HasLeftNeutralElement()
            {
                e: tplSetElem
                ex e
                (
                    IsLeftNeutralElement(e)
                )
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Optional,
   PredicateInstance
     (Signature (AliasedId ["HasLeftNeutralElement"], []),
      ([BlockVariableDeclaration
          ([Var "e"], VariableTypeWithModifier (None, TemplateType "tplSetElem"))],
       Exists
         ([Var "e"],
          PredicateWithArgs
            (AliasedId ["IsLeftNeutralElement"],
             [PredicateWithoutArgs (Var "e")])))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestProperty10 () =
        let result = run property """optional pred HasNeutralElement()
            {
                e: tplSetElem
                ex e
                (
                    IsNeutralElement(e)
                )
            }"""
        let actual = sprintf "%O" result
        let expected = """Success: Property
  (Optional,
   PredicateInstance
     (Signature (AliasedId ["HasNeutralElement"], []),
      ([BlockVariableDeclaration
          ([Var "e"], VariableTypeWithModifier (None, TemplateType "tplSetElem"))],
       Exists
         ([Var "e"],
          PredicateWithArgs
            (AliasedId ["IsNeutralElement"], [PredicateWithoutArgs (Var "e")])))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);
