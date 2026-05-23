namespace FplParser.Tests.ErrRecovery
open FParsec
open FplParsing.Combinators
open FplParsing.Main
open ErrDiagnostics
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestRecovery() =

    [<DataRow("del01", """def pred T() {del T()}""")>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base T(); }}""")>]
    [<TestMethod>]
    member this.TestMissingDot(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("endOfFile01", """""")>]
    [<DataRow("endOfFile02", """ """)>]
    [<DataRow("loc01", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x """)>]
    [<DataRow("loc02", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x def cl A""")>]
    [<DataRow("dec01", """def pred T() {dec x:obj true}""")>]
    [<DataRow("dec02", """def pred T() {dec x:obj true}""")>]
    [<TestMethod>]
    member this.TestMissingSemicolon(no:string, fplCode) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("bb01", """loc  ;""", 1)>]
    [<DataRow("bb02", """def pred T() loc  def cl T;""", 1)>]
    [<DataRow("bb03", """def pred T();""", 0)>]
    [<DataRow("bb04", """def cl TestId {ctor TestId() {} ctor TestId(x:obj) {} ctor TestId(x:pred) {} };""", 0)>]
    [<DataRow("bb05", """def cl TestId { s };""", 1)>]
    [<DataRow("bb06", """ def cl TestId { s };""", 1)>]
    [<DataRow("bb07", """def pred T() { .(.∀ x:obj {.x is N} ∧ ¬∃ y:obj {y is M}) ∨ (.¬∀ x:obj {.x is N} ∧ ∃ y:obj {.y is M}) };""", 2)>]
    [<DataRow("bb08", """def pred T() { .(.¬∃ x:obj {.x is N} ∨ (.true ⇔ false)) ∧ .(.∃ x:obj {.x is N} ∨ ¬.true ⇔ false) };""", 4)>]
    [<DataRow("bb09", """def pred T() { .(.¬∃ x:obj {.x is N} ∨ (.true ⇔ false)) ∧ .(.∃ x:obj {.x is N} ∨ ¬.true ⇔ false) } def cl A;""", 4)>]
    
    [<TestMethod>]
    member this.TestErrorRecoveryBuildingBlock(no:string, fplCode:string, numbErr:int) =
        ad.Clear()
        let result, success = fplParser fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual<bool>(false, success)
