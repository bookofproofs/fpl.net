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
    [<DataRow("ext00", "ext Digits x@/\d+/ -> X ret x};")>]
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
    [<DataRow("ext00", "ext Digits x@/\d+/ -> X {ret x;")>]
    [<TestMethod>]
    member this.TestMissingClosingBrace(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("uses00", """uses Fpl.Test.;""")>]
    [<DataRow("uses01", """uses Fpl.Test alias ;""")>]
    [<DataRow("cl01", """def cl ;""")>]
    [<DataRow("cl01a", """def cl {intr};""")>]
    [<DataRow("thm01", """thm {true};""")>]
    [<DataRow("lem01", """lem {true};""")>]
    [<DataRow("prop01", """prop {true};""")>]
    [<DataRow("conj01", """conj {true};""")>]
    [<DataRow("ax01", """ax {true};""")>]
    [<DataRow("pred01", """def pred ();""")>]
    [<DataRow("pred01a", """def pred () {intr};""")>]
    [<DataRow("func01", """def func ()->obj ;""")>]
    [<DataRow("func01a", """def func ()->obj {intr};""")>]
    [<DataRow("func01b", """def func T ()-> {intr};""")>]
    [<DataRow("inf01", """inf {pre:true con:true};""")>]
    [<DataRow("cor01", """cor {true};""")>]
    [<DataRow("proof01", """proof {1. |- trivial};""")>]
    [<DataRow("ext01", """ext  x@/\d+/ -> X {ret x};""")>]
    [<DataRow("del01", """def pred T() {del. ()};""")>]
    [<TestMethod>]
    member this.TestMissingPascalCaseId(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
