namespace TestFpl3LanguageServer.ServiceRename

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl2Interpreter.SymbolTable.Storage.Heap
open Fpl3LanguageServer.ServiceRename.RenameCore
open TestFpl3LanguageServer.Helpers.SampleFpl

[<TestClass>]
type TestRename() =

    /// <summary>
    /// Converts a 0-based LSP position into an offset inside the text.
    /// </summary>
    let offsetOf (text: string) (position: Position) : int =
        let mutable offset = 0
        for _ in 1 .. position.Line do
            offset <- text.IndexOf('\n', offset) + 1
        offset + position.Character

    /// <summary>
    /// Returns the text covered by an LSP range.
    /// </summary>
    let rangeText (text: string) (range: Range) : string =
        let start = offsetOf text range.Start
        let finish = offsetOf text range.End
        text.Substring(start, finish - start)

    // group 1 and 2 (variables and axioms): the first version of the rename service
    [<DataRow("01", "axiom A1", 0, "A1", 1)>]
    [<DataRow("01a", "byax A1", 5, "A1", 1)>]
    [<DataRow("V01", "x, y:", 0, "x", 2)>]
    [<DataRow("V01a", "(x ⇒ (y ⇒ x))", 1, "x", 2)>]
    [<DataRow("V01b", "y ⇒ x", 4, "x", 2)>]
    [<DataRow("V01c", "x ⇒ x", 0, "x", 2)>]
    [<DataRow("V01d", "x ⇒ x", 4, "x", 2)>]
    [<TestMethod>]
    member _.TestPrepareRename(no: string, cursorAnchor: string, cursorOffset: int, expectedName: string, where: int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        match prepareRenameAt File.ReadAllText cursorNode.Value with
        | Result.Error message ->
            Assert.Fail($"prepareRename rejected '{cursorAnchor}' (case {no}): {message}")
        | Result.Ok(path, range) ->
            let text = File.ReadAllText path
            Assert.AreEqual<string>(expectedName, rangeText text range, $"Identifier range differs for case {no}.")

    // rows mirror TestFindReferences.fs and TestGoToDefinition.fs
    // columns: no, cursorAnchor, cursorOffset, newName, expectedEditCount, where
    // expectedEditCount is the number of nodes that find-references returns for the same cursor
    [<DataRow("01", "axiom A1", 0, "Ax1", 3, 1)>]
    [<DataRow("01a", "byax A1", 5, "Ax1", 3, 1)>]
    [<DataRow("02", "theorem T1", 0, "Renamed", 3, 1)>]
    [<DataRow("02a", "cor T1$2", 0, "Renamed", 2, 1)>]
    [<DataRow("03", "inf ModusPonens", 0, "Renamed", 5, 1)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "Renamed", 5, 1)>]
    // find references to arguments that have justifications
    [<DataRow("04", "6 |-", 0, "Renamed", 2, 1)>]
    [<DataRow("04a", "6.", 0, "Renamed", 2, 1)>]
    [<DataRow("05", "4. byax A1 |-", 0, "Renamed", 3, 1)>]
    [<DataRow("05a", "T1$1:4", 0, "Renamed", 3, 1)>]
    // find references to arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "Renamed", 3, 1)>]
    [<DataRow("06a", "1:", 0, "Renamed", 3, 1)>]
    // cross-file referencing
    [<DataRow("07", "lem Le", 0, "Renamed", 3, 1)>]
    [<DataRow("07a", "Le, Pr", 0, "Renamed", 2, 2)>]
    [<DataRow("08", "conj Co", 0, "Renamed", 2, 1)>]
    [<DataRow("08a", "byconj Co", 0, "Renamed", 2, 2)>]
    [<DataRow("09", "cor Le$1", 0, "Renamed", 5, 2)>]
    [<DataRow("09a", "bycor Le$1", 0, "Renamed", 3, 2)>]
    [<DataRow("10", "cor Le$1$2", 0, "Renamed", 5, 2)>]
    [<DataRow("10a", "bycor Le$1$2", 0, "Renamed", 3, 2)>]
    // group 1: variables
    [<DataRow("V01", "x, y:", 0, "z", 5, 2)>]
    [<DataRow("V01a", "(x ⇒ (y ⇒ x))", 1, "z", 5, 2)>]
    [<DataRow("V01b", "y ⇒ x", 4, "z", 5, 2)>]
    [<DataRow("V01c", "x ⇒ x", 0, "z", 5, 2)>]
    [<DataRow("V01d", "x ⇒ x", 4, "z", 5, 2)>]
    // group 2: predicate and byDef
    [<DataRow("P01", "pred Q", 5, "Renamed", 3, 2)>]
    [<DataRow("P01a", "Q(f)", 0, "Renamed", 3, 2)>]
    [<DataRow("P01b", "bydef Q", 6, "Renamed", 3, 2)>]
    // group 3: functional term
    [<DataRow("F01", "def func Id", 4, "Renamed", 2, 2)>]
    [<DataRow("F01a", "Id(true)", 1, "Renamed", 2, 2)>]
    // classes
    [<DataRow("C01", "class Cls", 0, "Renamed", 4, 2)>]
    [<DataRow("C01a", "Sub : Cls", 6, "Renamed", 4, 2)>]
    [<DataRow("C02", "cl Cl1", 0, "Renamed", 2, 2)>]
    [<DataRow("C02a", "z:Cl1", 3, "Renamed", 2, 2)>]
    // group 9: properties
    [<DataRow("Pp01", "prty pred PropP", 10, "Renamed", 2, 2)>]
    [<DataRow("Pp01a", "x.PropP()", 2, "Renamed", 2, 2)>]
    [<DataRow("Pp02", "prty func PropF", 10, "Renamed", 2, 2)>]
    [<DataRow("Pp02a", "x.PropF()", 2, "Renamed", 2, 2)>]
    // group 10: extensions
    [<DataRow("Ext01", "ext Digits", 4, "Renamed", 2, 2)>]
    [<DataRow("Ext01a", "@1}", 0, "Renamed", 2, 2)>]
    // group 11: array type occurrences
    [<DataRow("Arr01", "cl ArrCl", 3, "Renamed", 2, 2)>]
    // group 12: array type occurrences with index references
    [<DataRow("Arr02", "cl ArrIdx", 3, "Renamed", 2, 2)>]
    // group 13: array variable references
    [<DataRow("Arr03", "x:*pred[ind,ind]", 0, "z", 3, 2)>]
    // constructors
    [<DataRow("Co1", "ctor Cls()", 6, "Renamed", 3, 2)>]
    [<DataRow("Co1a", "x:=Cls()", 4, "Renamed", 3, 2)>]
    [<DataRow("Co1b", "base.Cls()", 6, "Renamed", 3, 2)>]
    // group 6: proposition
    [<DataRow("PR01", "prop Pr", 5, "Renamed", 2, 1)>]
    [<DataRow("PR01a", "Le, Pr |-", 4, "Renamed", 2, 2)>]
    // infix operation
    [<DataRow("I01", "pred Inf", 0, "Renamed", 3, 2)>]
    [<DataRow("I01a", "x => y", 2, "Renamed", 3, 2)>]
    [<DataRow("I01b", "y => z", 3, "Renamed", 3, 2)>]
    // postfix operation
    [<DataRow("Pos01", "func Post", 0, "Renamed", 2, 2)>]
    [<DataRow("Pos01a", "x'", 1, "Renamed", 2, 2)>]
    // prefix operation
    [<DataRow("Pre01", "func Pre", 0, "Renamed", 2, 2)>]
    [<DataRow("Pre01a", "-x", 0, "Renamed", 2, 2)>]
    // symbol operation
    [<DataRow("Sym01", "cl Cl2", 0, "Renamed", 3, 2)>]
    [<DataRow("Sym01a", "∅ = ∅", 0, "Renamed", 3, 2)>]
    [<DataRow("Sym02a", "∅ = ∅", 4, "Renamed", 3, 2)>]
    // group 14: is operator
    [<DataRow("IS01", "cl IsCl", 3, "Renamed", 2, 2)>]
    [<DataRow("IS01a", "is(x,IsCl)", 5, "Renamed", 2, 2)>]
    [<TestMethod>]
    member _.TestRename(no: string, cursorAnchor: string, cursorOffset: int, newName: string, expectedCount: int, where: int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        // the identifier that the rename must replace, as reported by prepareRename, along with its
        // path and range so that we can identify the one edit that corresponds to the cursor itself.
        let cursorPath, cursorRange, oldName =
            match prepareRenameAt File.ReadAllText cursorNode.Value with
            | Result.Ok(path, range) -> path, range, rangeText (File.ReadAllText path) range
            | Result.Error message -> failwith $"prepareRename rejected '{cursorAnchor}' (case {no}): {message}"
        let myEdits = renameEdits File.ReadAllText cursorNode.Value heap.SymbolTable.Nodes newName
        match myEdits with
        | Result.Error message ->
            Assert.Fail($"Rename to '{newName}' rejected (case {no}): {message}")
        | Result.Ok edits ->
            Assert.AreEqual(expectedCount, edits.Length, $"Edit count differs for case {no}.")
            let cursorEdit =
                edits
                |> List.tryFind (fun (path, range, _) -> path = cursorPath && range = cursorRange)
            match cursorEdit with
            | None ->
                Assert.Fail($"No edit found matching the cursor position for case {no}.")
            | Some(path, range, newText) ->
                Assert.AreEqual<string>(newName, newText, $"New text differs for case {no}.")
                Assert.AreEqual<string>(oldName, rangeText (File.ReadAllText path) range, $"Edited range differs for case {no}.")


    // columns: no, cursorAnchor, cursorOffset, newName, where, isValid, substring of error 
    [<DataRow("01",  "axiom A1", 0, "Ren", 1, true, "")>]
    [<DataRow("01a", "axiom A1", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01b", "axiom A1", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01c", "axiom A1", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01d", "axiom A1", 0, "T1", 1, false, "already exists for theorem")>]
    [<DataRow("01e", "axiom A1", 0, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01f", "axiom A1", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("01_1",  "byax A1", 5, "Ren", 1, true, "")>]
    [<DataRow("01_1a", "byax A1", 5, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01_1b", "byax A1", 5, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01_1c", "byax A1", 5, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01_1d", "byax A1", 5, "T1", 1, false, "already exists for theorem")>]
    [<DataRow("01_1e", "byax A1", 5, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("01_1f", "byax A1", 5, "xor", 1, false, "is a keyword")>]
    [<DataRow("02",  "theorem T1", 0, "Ren", 1, true, "")>]
    [<DataRow("02a", "theorem T1", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02b", "theorem T1", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02c", "theorem T1", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02d", "theorem T1", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("02e", "theorem T1", 0, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02f", "theorem T1", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("02_1",  "1. T1 |-", 3, "Ren", 2, true, "")>]
    [<DataRow("02_1a", "1. T1 |-", 3, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02_1b", "1. T1 |-", 3, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02_1c", "1. T1 |-", 3, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02_1d", "1. T1 |-", 3, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("02_1e", "1. T1 |-", 3, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("02_1f", "1. T1 |-", 3, "xor", 2, false, "is a keyword")>]
    [<DataRow("03",  "cor T1$2", 0, "Ren$1", 1, true, "")>]
    [<DataRow("03a", "cor T1$2", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03b", "cor T1$2", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03c", "cor T1$2", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03d", "cor T1$2", 0, "A1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03e", "cor T1$2", 0, "Ren", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03f", "cor T1$2", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("03_1",  "bycor T1$2", 6, "Ren$1", 2, true, "")>]
    [<DataRow("03_1a", "bycor T1$2", 6, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03_1b", "bycor T1$2", 6, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03_1c", "bycor T1$2", 6, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03_1d", "bycor T1$2", 6, "A1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03_1e", "bycor T1$2", 6, "Ren", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("03_1f", "bycor T1$2", 6, "xor", 2, false, "is a keyword")>]
    [<DataRow("04",  "lem Le", 0, "Ren", 1, true, "")>]
    [<DataRow("04a", "lem Le", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04b", "lem Le", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04c", "lem Le", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04d", "lem Le", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("04e", "lem Le", 0, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04f", "lem Le", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("04_1",  "Le, Pr |-", 0, "Ren", 2, true, "")>]
    [<DataRow("04_1a", "Le, Pr |-", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04_1b", "Le, Pr |-", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04_1c", "Le, Pr |-", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04_1d", "Le, Pr |-", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("04_1e", "Le, Pr |-", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("04_1f", "Le, Pr |-", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("05",  "prop Pr", 0, "Ren", 1, true, "")>]
    [<DataRow("05a", "prop Pr", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05b", "prop Pr", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05c", "prop Pr", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05d", "prop Pr", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("05e", "prop Pr", 0, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05f", "prop Pr", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("05_1",  "Le, Pr |-", 4, "Ren", 2, true, "")>]
    [<DataRow("05_1a", "Le, Pr |-", 4, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05_1b", "Le, Pr |-", 4, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05_1c", "Le, Pr |-", 4, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05_1d", "Le, Pr |-", 4, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("05_1e", "Le, Pr |-", 4, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("05_1f", "Le, Pr |-", 4, "xor", 2, false, "is a keyword")>]
    [<DataRow("06",  "conj Co", 0, "Ren", 1, true, "")>]
    [<DataRow("06a", "conj Co", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06b", "conj Co", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06c", "conj Co", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06d", "conj Co", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("06e", "conj Co", 0, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06f", "conj Co", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("06_1",  "byconj Co", 7, "Ren", 2, true, "")>]
    [<DataRow("06_1a", "byconj Co", 7, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06_1b", "byconj Co", 7, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06_1c", "byconj Co", 7, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06_1d", "byconj Co", 7, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("06_1e", "byconj Co", 7, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("06_1f", "byconj Co", 7, "xor", 2, false, "is a keyword")>]
    [<DataRow("07",  "inf ModusPonens", 0, "Ren", 1, true, "")>]
    [<DataRow("07a", "inf ModusPonens", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07b", "inf ModusPonens", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07c", "inf ModusPonens", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07d", "inf ModusPonens", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("07e", "inf ModusPonens", 0, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07f", "inf ModusPonens", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("07_1",  "byinf ModusPonens", 6, "Ren", 1, true, "")>]
    [<DataRow("07_1a", "byinf ModusPonens", 6, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07_1b", "byinf ModusPonens", 6, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07_1c", "byinf ModusPonens", 6, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07_1d", "byinf ModusPonens", 6, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("07_1e", "byinf ModusPonens", 6, "Ren$1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("07_1f", "byinf ModusPonens", 6, "xor", 1, false, "is a keyword")>]
    [<DataRow("08",  "pred Q", 0, "Ren", 2, true, "")>]
    [<DataRow("08a", "pred Q", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08b", "pred Q", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08c", "pred Q", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08d", "pred Q", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("08e", "pred Q", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08f", "pred Q", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("08_1",  "Q(f)", 0, "Ren", 2, true, "")>]
    [<DataRow("08_1a", "Q(f)", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_1b", "Q(f)", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_1c", "Q(f)", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_1d", "Q(f)", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("08_1e", "Q(f)", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_1f", "Q(f)", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("08_2",  "bydef Q", 6, "Ren", 2, true, "")>]
    [<DataRow("08_2a", "bydef Q", 6, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_2b", "bydef Q", 6, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_2c", "bydef Q", 6, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_2d", "bydef Q", 6, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("08_2e", "bydef Q", 6, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("08_2f", "bydef Q", 6, "xor", 2, false, "is a keyword")>]
    [<DataRow("09",  "func Id", 0, "Ren", 2, true, "")>]
    [<DataRow("09a", "func Id", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09b", "func Id", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09c", "func Id", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09d", "func Id", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("09e", "func Id", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09f", "func Id", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("09_1",  "Id(true)", 0, "Ren", 2, true, "")>]
    [<DataRow("09_1a", "Id(true)", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09_1b", "Id(true)", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09_1c", "Id(true)", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09_1d", "Id(true)", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("09_1e", "Id(true)", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("09_1f", "Id(true)", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("10",  "prf T2$1", 0, "Ren$1", 1, true, "")>]
    [<DataRow("10a", "prf T2$1", 0, "1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("10b", "prf T2$1", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("10c", "prf T2$1", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("10d", "prf T2$1", 0, "A1", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("10e", "prf T2$1", 0, "Ren", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+`")>]
    [<DataRow("10f", "prf T2$1", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("10g", "prf T2$1", 0, "T2$2", 1, false, "already exists for proof")>]
    [<DataRow("11",  "class Cls", 0, "Ren", 2, true, "")>]
    [<DataRow("11a", "class Cls", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("11b", "class Cls", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("11c", "class Cls", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("11d", "class Cls", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("11e", "class Cls", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("11f", "class Cls", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("12",  "ext Digits", 0, "Ren", 2, true, "")>]
    [<DataRow("12a", "ext Digits", 0, "1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("12b", "ext Digits", 0, "", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("12c", "ext Digits", 0, "x", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("12d", "ext Digits", 0, "A1", 2, false, "already exists for axiom")>]
    [<DataRow("12e", "ext Digits", 0, "Ren$1", 2, false, "must match pattern `[A-Z][a-zA-Z0-9_]*`")>]
    [<DataRow("12f", "ext Digits", 0, "xor", 2, false, "is a keyword")>]
    [<DataRow("13",  "6. 2,4 |-", 0, "Ren", 1, true, "")>]
    [<DataRow("13a", "6. 2,4 |-", 0, "1", 1, false, "already exists for argument")>]
    [<DataRow("13b", "6. 2,4 |-", 0, "", 1, false, "must match pattern `[a-zA-Z0-9_]+`")>]
    [<DataRow("13c", "6. 2,4 |-", 0, "x", 1, true, "")>]
    [<DataRow("13d", "6. 2,4 |-", 0, "A1", 1, true, "")>]
    [<DataRow("13e", "6. 2,4 |-", 0, "Ren$1", 1, false, "must match pattern `[a-zA-Z0-9_]+`")>]
    [<DataRow("13f", "6. 2,4 |-", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("13g", "6. 2,4 |-", 0, "8", 1, false, "")>]
    [<DataRow("13_1",  "6 |-", 0, "Ren", 1, true, "")>]
    [<DataRow("13_1a", "6 |-", 0, "1", 1, false, "already exists for argument")>]
    [<DataRow("13_1b", "6 |-", 0, "", 1, false, "must match pattern `[a-zA-Z0-9_]+`")>]
    [<DataRow("13_1c", "6 |-", 0, "x", 1, true, "")>]
    [<DataRow("13_1d", "6 |-", 0, "A1", 1, true, "")>]
    [<DataRow("13_1e", "6 |-", 0, "Ren$1", 1, false, "must match pattern `[a-zA-Z0-9_]+`")>]
    [<DataRow("13_1f", "6 |-", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("13_1g", "6 |-", 0, "8", 1, false, "")>]
    [<DataRow("14",  "T1$1:4", 0, "Ren", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+\:[a-zA-Z0-9_]+`")>]
    [<DataRow("14a", "T1$1:4", 0, "T1$1:5", 1, false, "already exists for argument")>]
    [<DataRow("14b", "T1$1:4", 0, "", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+\:[a-zA-Z0-9_]+`")>]
    [<DataRow("14c", "T1$1:4", 0, "x", 1, false, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+\:[a-zA-Z0-9_]+`")>]
    [<DataRow("14d", "T1$1:4", 0, "A1", 1, true, "must match pattern `[A-Z][a-zA-Z0-9_]*(?:\$\d+)+\:[a-zA-Z0-9_]+`")>]
    [<DataRow("14e", "T1$1:4", 0, "Ren$1", 1, false, "must match pattern `[a-zA-Z0-9_]+`")>]
    [<DataRow("14f", "T1$1:4", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("14g", "T1$1:4", 0, "T1$1:x", 1, true, "")>]
    [<DataRow("14h", "T1$1:4", 0, "T1$1:Ren", 1, true, "")>]
    [<DataRow("14i", "T1$1:4", 0, "T1$1:A1", 1, true, "")>]
    [<DataRow("14j", "T1$1:4", 0, "T1$1:9", 1, true, "")>]
    [<TestMethod>]
    member _.TestValidateNewName(no: string, cursorAnchor: string, cursorOffset: int, newName: string, where: int, isValid:bool, sError:string) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")
        let validated = validateNewName cursorNode.Value newName heap.SymbolTable.Nodes
   
        match isValid, validated with 
        | true, Result.Error error ->
            Assert.Fail($"Failed - produced error for a valid rename to '{newName}' at '{cursorAnchor}'{Environment.NewLine}{error}")
        | true, Result.Ok _ ->
            printfn "%s" $"Succeeded validation to '{newName}' at '{cursorAnchor}'"
        | false, Result.Error error when error.Contains(sError) ->
            printfn "%s" $"Succeeded check for invalid rename to '{newName}' at '{cursorAnchor}' with error {Environment.NewLine}{error}"
        | false, Result.Error error when not (error.Contains(sError)) ->
            Assert.Fail($"Failed - produced wrong validation error for an invalid rename to '{newName}' at '{cursorAnchor}'{Environment.NewLine}{error}{Environment.NewLine}does not contain '{sError}'")
        | _, _ ->
            Assert.Fail($"Failed - produced no error for an invalid rename to '{newName}' at '{cursorAnchor}'")
