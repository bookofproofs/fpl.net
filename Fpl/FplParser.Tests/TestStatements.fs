namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestStatements () =

    [<TestMethod>]
    member this.TestRange01 () =
        let result = run rangeStatement """range proceedingResult p$
                (
                    assert proceedingResult
                    a:=1
                    b:=1
                )"""
        let actual = sprintf "%O" result
        let expected = """Success: Range
  ((Var "proceedingResult", Var "p"),
   [Assertion (PredicateWithoutArgs (Var "proceedingResult"));
    Assignment (Var "a", PredicateWithoutArgs (ExtDigits "1"));
    Assignment (Var "b", PredicateWithoutArgs (ExtDigits "1"))])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestLoop01 () =
        let result = run loopStatement """loop proceedingResult p$
                (
                    assert proceedingResult
                    a:=1
                    b:=1
                )"""
        let actual = sprintf "%O" result
        let expected = """Success: Loop
  ((Var "proceedingResult", Var "p"),
   [Assertion (PredicateWithoutArgs (Var "proceedingResult"));
    Assignment (Var "a", PredicateWithoutArgs (ExtDigits "1"));
    Assignment (Var "b", PredicateWithoutArgs (ExtDigits "1"))])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAssignment01 () =
        let result = run assignmentStatement """a:= 1"""
        let actual = sprintf "%O" result
        let expected = """Success: Assignment (Var "a", PredicateWithoutArgs (ExtDigits "1"))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAssignment02 () =
        let result = run assignmentStatement """self := Zero()"""
        let actual = sprintf "%O" result
        let expected = """Success: Assignment (Self [], PredicateWithArgs (AliasedId ["Zero"], []))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDelegate01 () =
        let result = run fplDelegate """del.test(1,2)"""
        let actual = sprintf "%O" result
        let expected = """Success: Delegate
  (DelegateId "test",
   [PredicateWithoutArgs (ExtDigits "1"); PredicateWithoutArgs (ExtDigits "2")])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDelegate02 () =
        let result = run fplDelegate """del.decrement(x)"""
        let actual = sprintf "%O" result
        let expected = """Success: Delegate (DelegateId "decrement", [PredicateWithoutArgs (Var "x")])""".Replace("\r","")
        Assert.AreEqual(expected, actual);
        
    [<TestMethod>]
    member this.TestReturn01 () =
        let result = run returnStatement """return result"""
        let actual = sprintf "%O" result
        let expected = """Success: Return (PredicateWithoutArgs (Var "result"))""".Replace("\r","")
        Assert.AreEqual(expected, actual);
    
    [<TestMethod>]
    member this.TestAssertion01 () =
        let result = run assertionStatement """assert
                    all n
                    (
                        and
                        (
                            is(n, Set),
                            In(n, self)
                        )
                    )"""
        let actual = sprintf "%O" result
        let expected = """Success: Assertion
  (All
     ([Var "n"],
      And
        [IsOperator
           (Var "n", VariableTypeWithModifier (None, ClassHeaderType ["Set"]));
         PredicateWithArgs
           (AliasedId ["In"],
            [PredicateWithoutArgs (Var "n"); PredicateWithoutArgs (Self [])])]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestCases01 () =
        let result = run casesStatement """cases
                (
                    |Equal(x,0) :
                        self := Zero()
                    | Equal(x,1) :
                        self := Succ(Zero())
                    | Equal(x,2) :
                        self := Succ(Succ(Zero()))
                    :>
                        // else case addressed using a python delegate
                        self := Succ(del.decrement(x))
                )"""
        let actual = sprintf "%O" result
        let expected = """Success: Cases
  ([ConditionFollowedByResult
      (PredicateWithArgs
         (AliasedId ["Equal"],
          [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (ExtDigits "0")]),
       [Assignment (Self [], PredicateWithArgs (AliasedId ["Zero"], []))]);
    ConditionFollowedByResult
      (PredicateWithArgs
         (AliasedId ["Equal"],
          [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (ExtDigits "1")]),
       [Assignment
          (Self [],
           PredicateWithArgs
             (AliasedId ["Succ"], [PredicateWithArgs (AliasedId ["Zero"], [])]))]);
    ConditionFollowedByResult
      (PredicateWithArgs
         (AliasedId ["Equal"],
          [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (ExtDigits "2")]),
       [Assignment
          (Self [],
           PredicateWithArgs
             (AliasedId ["Succ"],
              [PredicateWithArgs
                 (AliasedId ["Succ"],
                  [PredicateWithArgs (AliasedId ["Zero"], [])])]))])],
   DefaultResult
     [Assignment
        (Self [],
         PredicateWithArgs
           (AliasedId ["Succ"],
            [PredicateWithArgs
               (DelegateId "decrement", [PredicateWithoutArgs (Var "x")])]))])""".Replace("\r","")
        Assert.AreEqual(expected, actual);
