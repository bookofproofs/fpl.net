namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplPrimitives
open ErrDiagnostics
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Root
open FplInterpreter.Globals.Heap
open FplInterpreterVariables
open FplInterpreterAssignments
open CommonTestHelpers

[<TestClass>]
type TestFixTypeExpressionNames() =


    [<DataRow("01", """def pred T() {x ∧ y}""", "x ∧ y")>]
    [<DataRow("01a", """def pred T() {(x ∧ y)}""", "(x ∧ y)")>]
    [<DataRow("01b", """def pred T() {((x ∧ y))}""", "((x ∧ y))")>]
    [<DataRow("02", """def pred T() {x}""", "x")>]
    [<DataRow("02a", """def pred T() {(x)}""", "(x)")>]
    [<DataRow("02n", """def pred T() {((x))}""", "((x))")>]
    [<DataRow("03", """def pred T() {1}""", "1")>]
    [<DataRow("03a", """def pred T() {(1)}""", "(1)")>]
    [<DataRow("03n", """def pred T() {((1))}""", "((1))")>]
    [<DataRow("04", """def pred T() {-x}""", "-x")>]
    [<DataRow("04a", """def pred T() {(-x)}""", "(-x)")>]
    [<DataRow("04b", """def pred T() {((-x))}""", "((-x))")>]
    [<DataRow("05", """def pred T() {x'}""", "x'")>]
    [<DataRow("05a", """def pred T() {(x')}""", "(x')")>]
    [<DataRow("05b", """def pred T() {((x'))}""", "((x'))")>]
    [<DataRow("06", """def pred T() {-x'}""", "-x'")>]
    [<DataRow("06a", """def pred T() {(-x')}""", "(-x')")>]
    [<DataRow("06b", """def pred T() {((-x'))}""", "((-x'))")>]
    [<DataRow("07", """def pred T() {-(x)'}""", "-(x)'")>]
    [<DataRow("07a", """def pred T() {(-(x)')}""", "(-(x)')")>]
    [<DataRow("07b", """def pred T() {((-(x)'))}""", "((-(x)'))")>]
    [<TestMethod>]
    member this.TestExpressionParen(var, fplCode:string, expectedFormula:string) =
        
        let filename = "TestExpressionParen"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr1 = theory.Scope["T()"] 
        let base1 = pr1.ArgList[0]
        Assert.AreEqual<string>(expectedFormula, base1.Type SignatureType.Name)
        prepareFplCode(filename, "", false) |> ignore

