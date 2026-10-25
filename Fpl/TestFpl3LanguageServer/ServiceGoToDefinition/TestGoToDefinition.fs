namespace TestFpl3LanguageServer.ServiceGoToDefinition

open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl3LanguageServer.Helpers.SampleFpl
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl2Interpreter.Helpers.Common



[<TestClass>]
type TestGoToDefinitionSameFile () =
    [<DataRow("01", "byax A1", 5, "axiom A1")>]
    [<DataRow("02", "byax A2", 5, "axiom A2")>]
    [<DataRow("03", "3. 1,2,", 3, "1: ass f")>]

    [<DataRow("01", "axiom A1", 0, "axiom A1")>]
    [<DataRow("01a", "byax A1", 5, "axiom A1")>]
    [<DataRow("02", "theorem T1", 0,"theorem T1")>]
    [<DataRow("02a", "proof T1$1", 0, "theorem T1")>]
    [<DataRow("02b", "cor T1$2", 0, "theorem T1")>]
    [<DataRow("03", "inf ModusPonens", 0, "inf ModusPonens")>]
    [<DataRow("03a", "byinf ModusPonens", 0, "inf ModusPonens")>]
    // find declarations of arguments that have justifications
    [<DataRow("04", "6 |-", 0, "6. 2,4")>]
    [<DataRow("04a", "6.", 0, "6. 2,4")>]
    [<DataRow("05", "4. byax A1 |-", 0, "4. byax A1 ")>]
    [<DataRow("05a", "T1$1:4", 0, "4. byax A1 ")>]
    // find references of arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "1: ass f")>]
    [<DataRow("06a", "1:", 0, "1: ass f")>]

    [<TestMethod>]
    member this.TestGoToDefinitionDeclaration(no:string, cursorAnchor: string, cursorOffset: int, expectedPos: string) =

        let path = normalizePath (writeCode fplFirstCode "TestFirstCode.fpl")
        prepareFplCode("TestSecondCode.fpl", fplSecondCode, false) |> ignore

        let cursorLine, cursorColumn = locate fplFirstCode cursorAnchor cursorOffset
        let cursorNode = heap.SymbolTable.CursorNode path cursorLine cursorColumn

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn}.")

        let declaration = resolveDeclaration (cursorTargetNode cursorNode.Value)
        Assert.IsTrue(declaration.IsSome, $"node at '{cursorAnchor}' is {cursorNode.Value.Name} and does not resolve to a declaration.")

        let actualPos = fplFirstCode.Substring((int)declaration.Value.StartPos.Index,expectedPos.Length)
        Assert.AreEqual<string>(expectedPos, actualPos)
