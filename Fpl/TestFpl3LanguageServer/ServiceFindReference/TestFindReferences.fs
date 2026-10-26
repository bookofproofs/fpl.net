namespace TestFpl3LanguageServer.ServiceFindReference

open System
open System.IO
open System.Collections.Generic
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl2Interpreter.Helpers.Common
open TestFpl3LanguageServer.Helpers.SampleFpl

[<TestClass>]
type TestFindReferences() =

    [<DataRow("01", "axiom A1", 0, "10, 52, 56", 1)>]
    [<DataRow("01a", "byax A1", 5, "10, 52, 56", 1)>]
    [<DataRow("02", "theorem T1", 0, "39, 45, 63", 1)>]
    [<DataRow("02a", "proof T1$1", 0, "39, 45, 63", 1)>]
    [<DataRow("02b", "cor T1$2", 0, "39, 45, 63", 1)>]
    [<DataRow("03", "inf ModusPonens", 0, "30, 54, 58, 75, 77", 1)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "30, 54, 58, 75, 77", 1)>]
    // find references to arguments that have justifications
    [<DataRow("04", "6 |-", 0, "78, 80", 1)>]
    [<DataRow("04a", "6.", 0, "78, 80", 1)>]
    [<DataRow("05", "4. byax A1 |-", 0, "56, 58, 81", 1)>]
    [<DataRow("05a", "T1$1:4", 0, "56, 58, 81", 1)>]
    // find references to arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "73, 75, 86", 1)>]
    [<DataRow("06a", "1:", 0, "73, 75, 86", 1)>]
    [<TestMethod>]
    member _.TestFindReferencesLines(no:string, cursorAnchor: string, cursorOffset: int, expectedLines: string, where:int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where 

        let references =
            findReferenceNodes cursorNode.Value heap.SymbolTable.Nodes

        let actual =
            references
            |> List.map (fun node -> node.StartPos.Line)
            |> List.sort
            |> List.map (fun f -> $"{f}")
            |> String.concat ", "

        Assert.AreEqual<string>(expectedLines, actual, $"Reference lines differ for '{cursorAnchor}'.")
