namespace FplParser.Tests.ErrRecovery
open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestRecovery() =



    [<DataRow("ax01", "ax T true};")>]
    [<DataRow("thm01", "thm T true};")>]
    [<DataRow("lem01", "lem T true};")>]
    [<DataRow("prop01", "prop T true};")>]
    [<DataRow("conj01", "conj T true};")>]
    [<DataRow("cor01", "cor T$1 true};")>]
    [<DataRow("ax01", "ax T true};")>]
    [<DataRow("inf01", "inf T  pre: true con:true};")>]
    [<DataRow("all01", "ax T { all x:obj  true } };")>]
    [<DataRow("ext01", "ext Digits x@/\d+/ -> X ret x};")>]
    [<DataRow("for01", "def pred T() {dec for x in y assert z};true};")>]
    [<DataRow("ctor01", "def cl T {ctor T() }};")>]
    [<DataRow("prf01", "prf T$1 1. |- trivial };")>]
    [<TestMethod>]
    member this.TestMissingOpeningBrace(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("ax01", "ax T {true;")>]
    [<DataRow("thm01", "thm T {true;")>]
    [<DataRow("lem01", "lem T {true;")>]
    [<DataRow("prop01", "prop T {true;")>]
    [<DataRow("conj01", "conj T {true;")>]
    [<DataRow("cor01", "cor T$1 {true;")>]
    [<DataRow("inf01", "inf T { pre: true con:true;")>]
    [<DataRow("all01", "ax T { all x:obj  { true  };")>]
    [<DataRow("ex01", "ax T { ex x:obj { true  };")>]
    [<DataRow("exn01", "ax T { exn$1 x:obj { true  };")>]
    [<DataRow("ext01", "ext Digits x@/\d+/ -> X {ret x;")>]
    [<DataRow("for01", "def pred T() {dec for x in y {assert z;true};")>]
    [<DataRow("ctor01", "def cl T {ctor T() {};")>]
    [<DataRow("propPred01", "def cl T {intr prty pred S() {intr };")>]
    [<DataRow("propFunc01", "def cl T {intr prty func S()->obj {intr };")>]
    [<DataRow("prf01", "prf T$1 {1. |- trivial ;")>]
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
    [<DataRow("cl02", """def cl T:;""")>]
    [<DataRow("cl03", """def cl T: ,;""")>]
    [<DataRow("cl04", """def cl T:,,;""")>]
    [<DataRow("cl05", """def cl T:, ,;""")>]
    [<DataRow("cl06", """def cl T: , , ;""")>]
    [<DataRow("thm01", """thm {true};""")>]
    [<DataRow("lem01", """lem {true};""")>]
    [<DataRow("prop01", """prop {true};""")>]
    [<DataRow("conj01", """conj {true};""")>]
    [<DataRow("ax01", """ax {true};""")>]
    [<DataRow("pred01", """def pred ();""")>]
    [<DataRow("pred01a", """def pred () {intr};""")>]
    [<DataRow("pred02", """def pred T:();""")>]
    [<DataRow("pred03", """def pred T:,();""")>]
    [<DataRow("func01", """def func ()->obj ;""")>]
    [<DataRow("func01a", """def func ()->obj {intr};""")>]
    [<DataRow("func01b", """def func T ()-> {intr};""")>]
    [<DataRow("func02", """def func T:()->obj ;""")>]
    [<DataRow("func03", """def func T:,()->obj ;""")>]
    [<DataRow("inf01", """inf {pre:true con:true};""")>]
    [<DataRow("cor01", """cor $1 {true};""")>]
    [<DataRow("proof01", """proof $1 {1. |- trivial};""")>]
    [<DataRow("ext01", """ext  x@/\d+/ -> X {ret x};""")>]
    [<DataRow("del01", """def pred T() {del. ()};""")>]
    [<DataRow("del02", """def pred T() {del.()};""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base. (); }};""")>]
    [<DataRow("base02", """def cl S def cl T {ctor T() {dec base. (); }};""")>]
    [<TestMethod>]
    member this.TestMissingPascalCaseId(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("pred01", """def pred T);""")>]
    [<DataRow("func01", """def func T)->obj ;""")>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T) {dec base.S(); }};""")>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T) };""")>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T)->obj };""")>]
    [<DataRow("del01", """def pred T() {del.T)};""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T); }};""")>]
    [<TestMethod>]
    member this.TestMissingOpeningParen(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("pred01", """def pred T(;""")>]
    [<DataRow("func01", """def func T(->obj ;""")>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T( {dec base.S(); }};""")>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T( };""")>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T(->obj };""")>]
    [<DataRow("del01", """def pred T() {del.T(};""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T(; }};""")>]
    [<DataRow("ref01", """def pred T() {S(};""")>]
    [<TestMethod>]
    member this.TestMissingClosingParen(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("del01", """def pred T() {del T()};""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base T(); }};""")>]
    [<TestMethod>]
    member this.TestMissingDot(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("endOfFile01", """""")>]
    [<DataRow("endOfFile02", """ """)>]
    [<DataRow("loc01", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x ;""")>]
    [<DataRow("loc02", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x def cl A""")>]
    [<DataRow("dec01", """def pred T() {dec ~x:obj true};""")>]
    [<DataRow("dec02", """def pred T() {dec ~x:obj true}""")>]
    [<TestMethod>]
    member this.TestMissingSemicolon(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
