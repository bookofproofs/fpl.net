namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestStatements () =

    [<DataRow("01", """for precedingResult in    p { assert precedingResult a:=1 b:=1 }""")>]
    [<DataRow("02", """for    n in Range(1,4) { assert Equal(f(n),n) }""")>]
    [<DataRow("03", """for n in Range($1,$4) { assert Equal(f(n),n) }""")>]
    [<DataRow("04", """for n in SomeType { x[n] := 1 }""")>]
    [<TestMethod>]
    member _.TestForStatementForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments forStatement fplCode

    [<DataRow("01", @"@1 :=true")>]
    [<DataRow("02", @"@1:=true")>]
    [<DataRow("03", """result:=Zero()""")>]
    [<DataRow("04", """a:= 1""")>]
    [<DataRow("05", """self := Zero()""")>]
    [<DataRow("06", """n:=mcases ( | (x = $1): false | (x = $2): true | (x = $3): false ? undef )""")>]
    [<TestMethod>]
    member _.TestAssignmentStatementForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments assignmentStatement fplCode

    [<DataRow("01", """del.Test(1,2)""")>]
    [<DataRow("02", """del.Decrement(x)""")>]
    [<TestMethod>]
    member _.TestFplDelegateForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments fplDelegate fplCode

    [<DataRow("01", """assert all n:Set { In(n, self) }""")>]
    [<TestMethod>]
    member _.TestAssertionStatementForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments assertionStatement fplCode

    [<DataRow("01", """cases ( | Equal(x,0): self := Zero() | Equal(x,1): self := Succ(Zero()) | Equal(x,2): self := Succ(Succ(Zero())) ? self := Succ(del.Decrement(x)) )""")>]
    [<DataRow("02", """cases ( | Equal(n,0): result := m.NeutralElem() ? result := op( y, Exp( m(y,op), y, Sub(n,1)) ) )""")>]
    [<DataRow("03", """cases ( | (x = 0): self := Zero() | (x = 1): self := Succ(Zero()) | (x = 2): self := Succ(Succ(Zero())) ? self := Succ(delegate.Decrement(x)) )""")>]
    [<DataRow("04", """cases ( | IsGreaterOrEqual(x.RightMember(), x.LeftMember()): self:=x.RightMember() ? self:=undefined )""")>]
    [<DataRow("05", """cases ( | (m = 0): result:= n | (Succ(m) = k): result:= Succ(Add(n,k)) ? result:= undef )""")>]
    [<TestMethod>]
    member _.TestCasesStatementForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments casesStatement fplCode

    [<DataRow("01", """mcases ( | (x = $1): false | (x = $2): true | (x = $3): false ? undef )""")>]
    [<TestMethod>]
    member _.TestMapCasesForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments mapCases fplCode

    [<DataRow("01", """in TestClass""")>]
    [<DataRow("02", """in someVar""")>]
    [<DataRow("03", """in self""")>]
    [<DataRow("04", """in ClosedRange(from,to)""")>]
    [<DataRow("05", """in T[x]""")>]
    [<TestMethod>]
    member _.TestInEntityForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments inEntity fplCode
