namespace TestFpl3LanguageServer.ServiceGoToDefinition

open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl3LanguageServer.Helpers.SampleFpl
open Fpl2Interpreter.SymbolTable.GoToDefinition


[<TestClass>]
type TestGoToDefinitionSameFile () =
    [<DataRow("01", "byax A1", 5, "axiom A1", 1)>]
    [<DataRow("02", "byax A2", 5, "axiom A2", 1)>]
    [<DataRow("03", "3. 1,2,", 3, "1: ass f", 1)>]

    [<DataRow("01", "axiom A1", 0, "axiom A1", 1)>]
    [<DataRow("01a", "byax A1", 5, "axiom A1", 1)>]
    [<DataRow("02", "theorem T1", 0,"theorem T1", 1)>]
    [<DataRow("02a", "proof T1$1", 0, "theorem T1", 1)>]
    [<DataRow("02b", "cor T1$2", 0, "theorem T1", 1)>]
    [<DataRow("03", "inf ModusPonens", 0, "inf ModusPonens", 1)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "inf ModusPonens", 1)>]
    // find declarations of arguments that have justifications
    [<DataRow("04", "6 |-", 0, "6. 2,4", 1)>]
    [<DataRow("04a", "6.", 0, "6. 2,4", 1)>]
    [<DataRow("05", "4. byax A1 |-", 0, "4. byax A1 ", 1)>]
    [<DataRow("05a", "T1$1:4", 0, "4. byax A1 ", 1)>]
    // find references of arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "1: ass f", 1)>]
    [<DataRow("06a", "1:", 0, "1: ass f", 1)>]
    // cross-file referencing
    [<DataRow("07", "lem Le", 0, "lem Le", 1)>]
    [<DataRow("07a", "Le, Pr", 0, "lem Le", 2)>]
    [<DataRow("08", "conj Co", 0, "conj Co", 1)>]
    [<DataRow("08a", "byconj Co", 0, "conj Co", 2)>]
    [<DataRow("09", "cor Le$1", 0, "lem Le", 2)>] // go-to-definition of corollary reveals its parent node (here lemma)
    [<DataRow("09a", "bycor Le$1", 0, "cor Le$1 {", 2)>] // bycor corollary goes to its declaration
    [<DataRow("10", "cor Le$1$2", 0, "cor Le$1 {", 2)>] // go-to-definition of corollary reveals its parent node (here corollary)
    [<DataRow("10a", "bycor Le$1$2", 0, "cor Le$1$2 {", 2)>] // bycor corollary goes to its declaration

    [<TestMethod>]
    member this.TestGoToDefinitionDeclaration(no:string, cursorAnchor: string, cursorOffset: int, expectedPos: string, where:int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where 

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        let declaration = resolveDeclaration (cursorTargetNode cursorNode.Value)
        Assert.IsTrue(declaration.IsSome, $"node at '{cursorAnchor}' is {cursorNode.Value.Name} and does not resolve to a declaration.")

        let actualPos =
            if declaration.Value.FilePath.Contains("TestFirstCode") then 
                fplFirstCode.Substring((int)declaration.Value.StartPos.Index,expectedPos.Length)
            elif declaration.Value.FilePath.Contains("TestSecondCode") then 
                fplSecondCode.Substring((int)declaration.Value.StartPos.Index,expectedPos.Length)
            else
                failwith $"unknown file input {declaration.Value.FilePath}"
        Assert.AreEqual<string>(expectedPos, actualPos)
