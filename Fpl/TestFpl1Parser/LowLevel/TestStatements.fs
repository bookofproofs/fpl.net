namespace TestFpl1Parser

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestStatements () =
    [<DataRow("01", """for precedingResult in    p { assert precedingResult a:=1 b:=1 }""")>]
    [<DataRow("02", """for    n in Range(1,4) { assert Equal(f(n),n) }""")>]
    [<DataRow("03", """for n in Range($1,$4) { assert Equal(f(n),n) }""")>]
    [<DataRow("04", """for n in SomeType { x[n] := 1 }""")>]
    [<TestMethod>]
    member this.TestForStatementSuccess (no:string, fplCode:string) =
        let result = run (forStatement .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", @"@1 :=true")>]
    [<DataRow("02", @"@1:=true")>]
    [<DataRow("03", """result:=Zero()""")>]
    [<DataRow("04", """a:= 1""")>]
    [<DataRow("05", """self := Zero()""")>]
    [<DataRow("06", """n:=mcases ( | (x = $1): false | (x = $2): true | (x = $3): false ? undef )""")>]
    [<TestMethod>]
    member this.TestAssignmentStatementSuccess (no:string, fplCode:string) =
        let result = run (assignmentStatement .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """del.Test(1,2)""")>]
    [<DataRow("02", """del.Decrement(x)""")>]
    [<TestMethod>]
    member this.TestFplDelegateSuccess (no:string, fplCode:string) =
        let result = run (fplDelegate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """assert all n:Set { In(n, self) }""")>]
    [<TestMethod>]
    member this.TestAssertionStatementSuccess (no:string, fplCode:string) =
        let result = run (assertionStatement .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """cases ( | Equal(x,0): self := Zero() | Equal(x,1): self := Succ(Zero()) | Equal(x,2): self := Succ(Succ(Zero())) ? self := Succ(del.Decrement(x)) )""")>]
    [<DataRow("02", """cases ( | Equal(n,0): result := m.NeutralElem() ? result := op( y, Exp( m(y,op), y, Sub(n,1)) ) )""")>]
    [<DataRow("03", """cases ( | (x = 0): self := Zero() | (x = 1): self := Succ(Zero()) | (x = 2): self := Succ(Succ(Zero())) ? self := Succ(delegate.Decrement(x)) )""")>]
    [<DataRow("04", """cases ( | IsGreaterOrEqual(x.RightMember(), x.LeftMember()): self:=x.RightMember() ? self:=undefined )""")>]
    [<DataRow("05", """cases ( | (m = 0): result:= n | (Succ(m) = k): result:= Succ(Add(n,k)) ? result:= undef )""")>]
    [<TestMethod>]
    member this.TestCasesStatementSuccess (no:string, fplCode:string) =
        let result = run (casesStatement .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """mcases ( | (x = $1): false | (x = $2): true | (x = $3): false ? undef )""")>]
    [<TestMethod>]
    member this.TestMapCasesSuccess (no:string, fplCode:string) =
        let result = run (mapCases .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
