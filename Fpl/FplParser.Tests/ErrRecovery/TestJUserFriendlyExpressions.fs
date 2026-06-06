namespace FplParser.Tests.ErrRecovery
open System
open FplGrammarTypes
open FplParsing.Combinators
open FplParsing.Main
open ErrDiagnostics
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestRecovery() =


    [<DataRow("del01", """def pred T() {del T()}""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base T(); }}""")>]
    [<DataRow("bb01", """loc  """)>]
    [<DataRow("bb02", """def pred T() loc  def cl T""")>]
    [<DataRow("bb03", """def pred T )""")>]
    [<DataRow("bb04", """def cl TestId {ctor TestId() {} ctor TestId(x:obj) {} ctor TestId(x:pre d) {} }""")>]
    [<DataRow("bb05", """def cl TestId { s }""")>]
    [<DataRow("bb06", """ def cl TestId { s }""")>]
    [<DataRow("bb07", """def pred T() { .(.∀ x:obj {.x is N} ∧ ¬∃ y:obj {y is M}) ∨ (.¬∀ x:obj {.x is N} ∧ ∃ y:obj {.y is M}) }""")>]
    [<DataRow("bb08", """def pred T() { .(.¬∃ x:obj {.x is N} ∨ (.true ⇔ false)) ∧ .(.∃ x:obj {.x is N} ∨ ¬.true ⇔ false) }""")>]
    [<DataRow("bb09", """def pred T() { .(.¬∃ x:obj {.x is N} ∨ (.true ⇔ false)) ∧ .(.∃ x:obj {.x is N} ∨ ¬.true ⇔ false) } def cl A""")>]
    [<DataRow("bb09", """ext  x@/\d+/ -> A {return x}""")>]
    [<DataRow("bb10", """proof T$1 {100. assume true}""")>]
    [<DataRow("bb11", """def pred A() {dec n:ind cases (|($2 = $1) : n:=$42 ? n:=); true}""")>]
    [<DataRow("bb12", """def pred T() {}""")>]
    [<TestMethod>]
    member this.TestErrorRecoveryBuildingBlock(no:string, fplCode:string) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)


    [<DataRow("01", """def pred T() {}""", 1L, 14L)>]
    [<DataRow("01a", """ def pred T() {}""", 1L, 15L)>]
    [<DataRow("01b", """  def pred T() {}""", 1L, 16L)>]
    [<DataRow("01c", """   def pred T() {}""", 1L, 17L)>]
    [<DataRow("02a", """
def pred T() {}""", 2L, 14L)>]
    [<DataRow("02a", """
 def pred T() {}""", 2L, 15L)>]
    [<DataRow("02b", """

  def pred T() {}""", 3L, 16L)>]
    [<TestMethod>]
    member this.TestErrorRecoveryPositions(no:string, fplCode:string, errLin: int64, errCol: int64) =
        ad.Clear()
        let result, _ = fplParser fplCode
        let errBlock = result.Head
        match errBlock with
        | Ast.ErrorSyntax((pos1,_),_) ->
            Assert.AreEqual<uint>((uint)errLin, (uint)pos1.Line)
            Assert.AreEqual<uint>((uint)errCol, (uint)pos1.Column)
        | _ ->
            Assert.Fail("No error block found")

    [<DataRow("01", """def pred T() {}""", 1)>]
    [<DataRow("02", """xxxx def pred T() {}""", 2)>]
    [<DataRow("02a", """ xxxx def pred T() {}""", 2)>]
    [<DataRow("03", """xxxx def pred T() {true} yyyyy""", 3)>]
    [<DataRow("03a", """ xxxx def pred T() {true} yyyyy""", 3)>]
    [<DataRow("03b", """xxxx def pred T() {true} yyyyy """, 3)>]
    [<DataRow("03c", """ xxxx def pred T() {true} yyyyy """, 3)>]
    [<DataRow("04", """def pred T() {true} yyyyy""", 2)>]
    [<DataRow("04a", """ def pred T() {true} yyyyy""", 2)>]
    [<TestMethod>]
    member this.TestErrorRecoveryBeforeOrAfterCode(no:string, fplCode:string, numbOfBlocks: int) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<int>(numbOfBlocks, result.Length)



    [<DataRow("uses00", """uses Fpl.Test.""")>]
    [<DataRow("uses01", """uses Fpl.Test alias """)>]
    [<DataRow("cl01", """def cl """)>]
    [<DataRow("cl01a", """def cl {intr}""")>]
    [<DataRow("cl02", """def cl T:""")>]
    [<DataRow("cl03", """def cl T: A,""")>]
    [<DataRow("cl04", """def cl T:A,B,""")>]
    [<DataRow("thm01", """thm {true}""")>]
    [<DataRow("lem01", """lem {true}""")>]
    [<DataRow("prop01", """prop {true}""")>]
    [<DataRow("conj01", """conj {true}""")>]
    [<DataRow("ax01", """ax {true}""")>]
    [<DataRow("pred01", """def pred ()""")>]
    [<DataRow("pred01a", """def pred () {intr}""")>]
    [<DataRow("pred02", """def pred T:()""")>]
    [<DataRow("pred03", """def pred T:,()""")>]
    [<DataRow("func01", """def func ()->obj """)>]
    [<DataRow("func01a", """def func ()->obj {intr}""")>]
    [<DataRow("func01b", """def func T ()-> {intr}""")>]
    [<DataRow("func02", """def func T:()->obj """)>]
    [<DataRow("func03", """def func T:,()->obj """)>]
    [<DataRow("inf01", """inf {pre:true con:true}""")>]
    [<DataRow("cor01", """cor $1 {true}""")>]
    [<DataRow("proof01", """proof $1 {1: trivial}""")>]
    [<DataRow("ext01", """ext  x@/\d+/ -> X {ret x}""")>]
    [<DataRow("del01", """def pred T() {del. ()}""")>]
    [<DataRow("del02", """def pred T() {del.()}""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base. (); }}""")>]
    [<DataRow("base02", """def cl S def cl T {ctor T() {dec base. (); }}""")>]
    [<DataRow("base03", """def cl S                       def cl T {ctor T() {dec base. (); }}""")>]
    [<TestMethod>]
    member this.TestMissingPascalCaseId(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("ax01", "ax T true}")>]
    [<DataRow("thm01", "thm T true}")>]
    [<DataRow("lem01", "lem T true}")>]
    [<DataRow("prop01", "prop T true}")>]
    [<DataRow("conj01", "conj T true}")>]
    [<DataRow("cor01", "cor T$1 true}")>]
    [<DataRow("ax01", "ax T true}")>]
    [<DataRow("inf01", "inf T  pre: true con:true}")>]
    [<DataRow("all01", "ax T { all x:obj  true } }")>]
    [<DataRow("ext01", "ext Digits x@/\d+/ -> X ret x}")>]
    [<DataRow("for01", "def pred T() {dec for x in y assert z};true}")>]
    [<DataRow("ctor01", "def cl T {ctor T() }}")>]
    [<DataRow("prf01", "prf T$1 1: trivial }")>]
    [<TestMethod>]
    member this.TestMissingOpeningBrace(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("ax01", "ax T {true")>]
    [<DataRow("thm01", "thm T {true")>]
    [<DataRow("lem01", "lem T {true")>]
    [<DataRow("prop01", "prop T {true")>]
    [<DataRow("conj01", "conj T {true")>]
    [<DataRow("cor01", "cor T$1 {true")>]
    [<DataRow("inf01", "inf T { pre: true con:true")>]
    [<DataRow("all01", "ax T { all x:obj  { true  }")>]
    [<DataRow("ex01", "ax T { ex x:obj { true  }")>]
    [<DataRow("exn01", "ax T { exn$1 x:obj { true  }")>]
    [<DataRow("ext01", "ext Digits x@/\d+/ -> X {ret x")>]
    [<DataRow("for01", "def pred T() {dec for x in y {assert z;true}")>]
    [<DataRow("ctor01", "def cl T {ctor T() {}")>]
    [<DataRow("propPred01", "def cl T {intr prty pred S() {intr }")>]
    [<DataRow("propFunc01", "def cl T {intr prty func S()->obj {intr }")>]
    [<DataRow("prf01", "prf T$1 {1: trivial ")>]
    [<TestMethod>]
    member this.TestMissingClosingBrace(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("pred01", """def pred T)""")>]
    [<DataRow("func01", """def func T)->obj """)>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T) {dec base.S(); }}""")>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T) }""")>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T)->obj }""")>]
    [<DataRow("del01", """def pred T() {del.T)}""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T); }}""")>]
    [<TestMethod>]
    member this.TestMissingOpeningParen(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("pred01", """def pred T(""")>]
    [<DataRow("func01", """def func T(->obj """)>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T( {dec base.S(); }}""")>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T( }""")>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T(->obj }""")>]
    [<DataRow("del01", """def pred T() {del.T(}""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T(; }}""")>]
    [<DataRow("ref01", """def pred T() {S(}""")>]
    [<TestMethod>]
    member this.TestMissingClosingParen(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("arrType01", """def pred T(a:*ind obj])""")>]
    [<DataRow("arrType02", """def pred T(a:*ind obj)""")>]
    [<DataRow("arrType03", """def pred T() {dec a:*ind obj]; true}""")>]
    [<DataRow("arrType04", """def pred T() {dec a:*ind obj; true}""")>]
    [<TestMethod>]
    member this.TestMissingOpeningBracket(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("arrType01", """def pred T(a:*ind[obj)""")>]
    [<DataRow("arrType02", """def pred T(a:*ind obj)""")>]
    [<DataRow("arrType03", """def pred T() {dec a:*ind[obj; true}""")>]
    [<DataRow("arrType04", """def pred T() {dec a:*ind obj; true}""")>]
    [<DataRow("arrUsage01", """def pred T() {dec a:=x[b; true}""")>]
    [<TestMethod>]
    member this.TestMissingClosingBracket(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)

    [<DataRow("loc01", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x """)>]
    [<DataRow("loc02", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x def cl A""")>]
    [<DataRow("dec01", """def pred T() {dec x:obj true}""")>]
    [<DataRow("dec02", """def pred T() {dec x:obj true}""")>]
    [<TestMethod>]
    member this.TestMissingSemicolon(no:string, fplCode) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)
