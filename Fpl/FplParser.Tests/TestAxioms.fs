namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestAxioms () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestAxiom01 () =
        let result = run axiom """axiom ZeroIsNat()
        {
            is(Zero,Nat)
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
  (((Ln: 1, Col: 1), (Ln: 4, Col: 10)),
   (Signature
      (((Ln: 1, Col: 7), (Ln: 2, Col: 9)),
       (PredicateIdentifier
          (((Ln: 1, Col: 7), (Ln: 1, Col: 16)),
           [PascalCaseId (((Ln: 1, Col: 7), (Ln: 1, Col: 16)), "ZeroIsNat")]),
        [])),
    ([],
     IsOperator
       (((Ln: 3, Col: 13), (Ln: 4, Col: 9)),
        (PredicateIdentifier
           (((Ln: 3, Col: 16), (Ln: 3, Col: 20)),
            [PascalCaseId (((Ln: 3, Col: 16), (Ln: 3, Col: 20)), "Zero")]),
         VariableTypeWithModifier
           (((Ln: 3, Col: 21), (Ln: 3, Col: 24)),
            (None,
             ClassHeaderType
               (((Ln: 3, Col: 21), (Ln: 3, Col: 24)),
                [PascalCaseId (((Ln: 3, Col: 21), (Ln: 3, Col: 24)), "Nat")]))))))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);

    [<TestMethod>]
    member this.TestAxiom02 () =
        let result = run axiom """axiom SuccessorExistsAndIsUnique()
        {
            n, successor: Nat
            all n
            (
                ex$1 successor
                (
                    and
                    (
                        NotEqual(successor,n),
                        Equal(successor,Succ(n))
                    )
                )
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
      (Signature (AliasedId ["SuccessorExistsAndIsUnique"], []),
       ([BlockVariableDeclaration
           ([Var "n"; Var "successor"],
            VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))],
        All
          ([Var "n"],
           ExistsN
             ((DollarDigits "1", [Var "successor"]),
              And
                [PredicateWithArgs
                   (AliasedId ["NotEqual"],
                    [PredicateWithoutArgs (Var "successor");
                     PredicateWithoutArgs (Var "n")]);
                 PredicateWithArgs
                   (AliasedId ["Equal"],
                    [PredicateWithoutArgs (Var "successor");
                     PredicateWithArgs
                       (AliasedId ["Succ"], [PredicateWithoutArgs (Var "n")])])]))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);

    [<TestMethod>]
    member this.TestAxiom03 () =
        let result = run axiom """axiom ZeroIsNotSuccessor()
        {
            n: Nat
            all n
            (
                NotEqual(Zero(), Succ(n))
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
  (Signature (AliasedId ["ZeroIsNotSuccessor"], []),
   ([BlockVariableDeclaration
       ([Var "n"], VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))],
    All
      ([Var "n"],
       PredicateWithArgs
         (AliasedId ["NotEqual"],
          [PredicateWithArgs (AliasedId ["Zero"], []);
           PredicateWithArgs
             (AliasedId ["Succ"], [PredicateWithoutArgs (Var "n")])]))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);

    [<TestMethod>]
    member this.TestAxiom04 () =
        let result = run axiom """axiom SuccessorIsInjective()
        {
            n,m: Nat
            all n,m
            (
                impl
                (
                    Equal(Succ(n),Succ(m)),
                    Equal(n,m)
                )
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
  (Signature (AliasedId ["SuccessorIsInjective"], []),
   ([BlockVariableDeclaration
       ([Var "n"; Var "m"],
        VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))],
    All
      ([Var "n"; Var "m"],
       Impl
         (PredicateWithArgs
            (AliasedId ["Equal"],
             [PredicateWithArgs
                (AliasedId ["Succ"], [PredicateWithoutArgs (Var "n")]);
              PredicateWithArgs
                (AliasedId ["Succ"], [PredicateWithoutArgs (Var "m")])]),
          PredicateWithArgs
            (AliasedId ["Equal"],
             [PredicateWithoutArgs (Var "n"); PredicateWithoutArgs (Var "m")])))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);

    [<TestMethod>]
    member this.TestAxiom05 () =
        let result = run axiom """axiom CompleteInduction()
        {
            n: Nat
            p: pred
            all p
            (
                impl
                (
                    and ( p(0), all n ( impl ( p(n), p(Succ(n)) ) ) ),
                    all n ( p(n) )
                )
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
  (Signature (AliasedId ["CompleteInduction"], []),
   ([BlockVariableDeclaration
       ([Var "n"], VariableTypeWithModifier (None, ClassHeaderType ["Nat"]));
     BlockVariableDeclaration
       ([Var "p"], VariableTypeWithModifier (None, PredicateType))],
    All
      ([Var "p"],
       Impl
         (And
            [PredicateWithArgs (Var "p", [PredicateWithoutArgs (ExtDigits "0")]);
             All
               ([Var "n"],
                Impl
                  (PredicateWithArgs (Var "p", [PredicateWithoutArgs (Var "n")]),
                   PredicateWithArgs
                     (Var "p",
                      [PredicateWithArgs
                         (AliasedId ["Succ"], [PredicateWithoutArgs (Var "n")])])))],
          All
            ([Var "n"],
             PredicateWithArgs (Var "p", [PredicateWithoutArgs (Var "n")]))))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);

    [<TestMethod>]
    member this.TestAxiom06 () =
        let result = run axiom """axiom EmptySetExists()
        {
            x: Set
            ex x
            (
                IsEmpty(x)
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
  (Signature (AliasedId ["EmptySetExists"], []),
   ([BlockVariableDeclaration
       ([Var "x"], VariableTypeWithModifier (None, ClassHeaderType ["Set"]))],
    Exists
      ([Var "x"],
       PredicateWithArgs
         (AliasedId ["IsEmpty"], [PredicateWithoutArgs (Var "x")]))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);

    [<TestMethod>]
    member this.TestAxiom07 () =
        let result = run axiom """axiom Extensionality()
        {
            x,y: Set
            all x,y
            (
                impl
                (
                    and
                    (
                        IsSubset(x,y),
                        IsSubset(y,x)
                    ),
                    Equal(x,y)
                )
            )
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: Axiom
  (Signature (AliasedId ["Extensionality"], []),
   ([BlockVariableDeclaration
       ([Var "x"; Var "y"],
        VariableTypeWithModifier (None, ClassHeaderType ["Set"]))],
    All
      ([Var "x"; Var "y"],
       Impl
         (And
            [PredicateWithArgs
               (AliasedId ["IsSubset"],
                [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (Var "y")]);
             PredicateWithArgs
               (AliasedId ["IsSubset"],
                [PredicateWithoutArgs (Var "y"); PredicateWithoutArgs (Var "x")])],
          PredicateWithArgs
            (AliasedId ["Equal"],
             [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (Var "y")])))))"""
        Assert.AreEqual(replaceWhiteSpace expected,  actual);
