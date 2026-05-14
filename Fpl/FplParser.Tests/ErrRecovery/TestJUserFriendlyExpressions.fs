namespace FplParser.Tests.ErrRecovery
open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestRecovery() =



    [<DataRow("ax00", "ax T true};")>]
    [<DataRow("thm00", "thm T true};")>]
    [<DataRow("lem00", "lem T true};")>]
    [<DataRow("prop00", "prop T true};")>]
    [<DataRow("conj00", "conj T true};")>]
    [<DataRow("cor00", "cor T$1 true};")>]
    [<DataRow("ax00", "ax T true};")>]
    [<DataRow("inf00", "inf T  pre: true con:true};")>]
    [<DataRow("all00", "ax T { all x:obj  true } };")>]
    [<DataRow("ex00", "ax T { ex x:obj  true } };")>]
    [<DataRow("ex00", "ax T { exn$1 x:obj  true } };")>]
    [<TestMethod>]
    member this.TestMissingOpeningBrace(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("ax00", "ax T {true;")>]
    [<DataRow("thm00", "thm T {true;")>]
    [<DataRow("lem00", "lem T {true;")>]
    [<DataRow("prop00", "prop T {true;")>]
    [<DataRow("conj00", "conj T {true;")>]
    [<DataRow("cor00", "cor T$1 {true;")>]
    [<DataRow("inf00", "inf T { pre: true con:true;")>]
    [<DataRow("all00", "ax T { all x:obj  { true  };")>]
    [<DataRow("ex00", "ax T { ex x:obj { true  };")>]
    [<DataRow("exn00", "ax T { exn$1 x:obj { true  };")>]
    [<TestMethod>]
    member this.TestMissingClosingBrace(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
