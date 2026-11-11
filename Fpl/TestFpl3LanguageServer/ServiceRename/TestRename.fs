namespace TestFpl3LanguageServer.ServiceRename

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

        match prepareRenameAt cursorNode.Value with
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

        // the identifier that the rename must replace, as reported by prepareRename
        let oldName =
            match prepareRenameAt cursorNode.Value with
            | Result.Ok(path, range) -> rangeText (File.ReadAllText path) range
            | Result.Error message -> failwith $"prepareRename rejected '{cursorAnchor}' (case {no}): {message}"

        match renameEdits cursorNode.Value heap.SymbolTable.Nodes newName with
        | Result.Error message ->
            Assert.Fail($"Rename to '{newName}' rejected (case {no}): {message}")
        | Result.Ok edits ->
            Assert.AreEqual(expectedCount, edits.Length, $"Edit count differs for case {no}.")
            for (path, range, newText) in edits do
                Assert.AreEqual<string>(newName, newText, $"New text differs for case {no}.")
                Assert.AreEqual<string>(oldName, rangeText (File.ReadAllText path) range, $"Edited range differs for case {no}.")

    // columns: no, cursorAnchor, cursorOffset, newName, where
    // invalid names (E01 to E05) and an unresolved reference (R01) must be rejected
    [<DataRow("E01", "axiom A1", 0, "ax", 1)>]        // keyword
    [<DataRow("E02", "axiom A1", 0, "a1", 1)>]        // axioms use PascalCase
    [<DataRow("E03", "x, y:", 0, "1x", 2)>]           // variables start with a lowercase letter
    [<DataRow("E04", "x, y:", 0, "X", 2)>]            // variables start with a lowercase letter
    [<DataRow("E05", "x, y:", 0, "", 2)>]             // empty name
    [<DataRow("E06", "x, y:", 0, "y", 2)>]            // collision with dec y in the same axiom A4
    [<DataRow("R01", "byax ExtAx", 5, "Renamed", 2)>] // unresolved reference: nothing to rename
    [<TestMethod>]
    member _.TestRenameRejected(no: string, cursorAnchor: string, cursorOffset: int, newName: string, where: int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        match renameEdits cursorNode.Value heap.SymbolTable.Nodes newName with
        | Result.Error _ -> ()
        | Result.Ok _ ->
            Assert.Fail($"Rename to '{newName}' at '{cursorAnchor}' (case {no}) should be rejected.")
