namespace TestFpl3LanguageServer.ServiceGoToDefinition

open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl3LanguageServer.Helpers.SampleFpl
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl2Interpreter.Helpers.Common



[<TestClass>]
type TestGoToDefinitionSameFile () =
    [<DataRow("01", "byax A1", 5, "axiom A1", 0)>]
    [<DataRow("02", "byax A2", 5, "axiom A2", 0)>]
    [<DataRow("03", "3. 1,2,", 3, "1: ass f", 0)>]

    [<DataRow("01", "axiom A1", 0, "axiom A1", 0)>]
    [<DataRow("01a", "byax A1", 5, "axiom A1", 0)>]
    [<DataRow("02", "theorem T1", 0,"theorem T1", 0)>]
    [<DataRow("02a", "proof T1$1", 0, "theorem T1", 0)>]
    [<DataRow("02b", "cor T1$2", 0, "39, 45, 63", 0)>]
    [<DataRow("03", "inf ModusPonens", 0, "30, 54, 58, 75, 77", 0)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "30, 54, 58, 75, 77", 0)>]
    // find declarations of arguments that have justifications
    [<DataRow("04", "6 |-", 0, "78, 80", 0)>]
    [<DataRow("04a", "6.", 0, "78, 80", 0)>]
    [<DataRow("05", "4. byax A1 |-", 0, "56, 58, 81", 0)>]
    [<DataRow("05a", "T1$1:4", 0, "56, 58, 81", 0)>]
    // find references of arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "73, 75, 86", 0)>]
    [<DataRow("06a", "1:", 0, "73, 75, 86", 0)>]



    [<TestMethod>]
    member this.TestGoToDefinitionDeclaration(no:string, cursorAnchor: string, cursorOffset: int, declAnchor: string, declOffset: int) =
        prepareFplCode("TestGoToDefinition.fpl", fplCode, false) |> ignore

        let path = normalizePath @"C:\Dev\fpl.net\Fpl\TestFpl3LanguageServer\bin\Debug\net8.0\TestGoToDefinition.fpl"
        let cursorLine, cursorColumn = locate fplCode cursorAnchor cursorOffset
        let cursorNode = heap.Eval.CursorNode path cursorLine cursorColumn

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn}.")

        let declaration = resolveDeclaration cursorNode.Value
        Assert.IsTrue(declaration.IsSome, $"node at '{cursorAnchor}' does not resolve to a declaration.")

        let expectedLine, expectedColumn = locate fplCode declAnchor declOffset
        Assert.AreEqual<int64>(expectedLine, declaration.Value.StartPos.Line, $"Line mismatch for '{declAnchor}'.")
        Assert.AreEqual<int64>(
            expectedColumn,
            declaration.Value.StartPos.Column,
            $"Column mismatch for '{declAnchor}': declaration {declaration.Value.GetType().Name} starts at column {declaration.Value.StartPos.Column}.")

