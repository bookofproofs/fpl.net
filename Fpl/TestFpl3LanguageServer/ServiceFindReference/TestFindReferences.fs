namespace TestFpl3LanguageServer.ServiceFindReference

open System
open System.Collections.Generic
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl2Interpreter.Helpers.Common

[<TestClass>]
type TestFindReferences() =

    let fplCode = """
    // auxiliary definitions
    def pred Impl(f, g: pred) infix "⇒" 0
    {
        impl(f,g)
    }

 
    // weakening rule
    axiom A1
    {
        dec f, g: pred;
        f ⇒ (g ⇒ f)
    }

    // impl distributive
    axiom A2 
    {
        dec f, g, h: pred;
        (f ⇒ (g ⇒ h)) ⇒ ((f ⇒ g) ⇒ (f ⇒ h))
    }

    // contraposition
    axiom A3 
    {
        dec f, g: pred;
        (¬f ⇒ ¬g) ⇒ (g ⇒ f)
    }

    inf ModusPonens
    {
        dec f, g: pred;
        pre:
            f, f ⇒ g 
        con:
            g
    }

    theorem T1
    {
        dec f:pred;
        f ⇒ f
    }

    proof T1$1 
    {
        // Direct Proof
        // Strategy: Assume that the premise is true, then prove that the conclusion also must be true.
    
        1. byax A2 |- 
        (f ⇒ ((f ⇒ f) ⇒ f)) ⇒ ((f ⇒ (f ⇒ f)) ⇒ (f ⇒ f))
        2. byax A1 |- 
        f ⇒ ((f ⇒ f) ⇒ f)
        3. 2, 1, byinf ModusPonens |- 
        (f ⇒ (f ⇒ f)) ⇒ (f ⇒ f)
        4. byax A1 |- 
        f ⇒ (f ⇒ f)
        5. 4, 3, byinf ModusPonens |- 
        f ⇒ f
        qed
    }

    thm T2 
    {
        dec f,g,h:pred;
        (f ⇒ g) ⇒ ((g ⇒ h) ⇒ (f ⇒ h)) 
    }

    prf T2$1 
    {
        1: ass f
        2: ass f ⇒ g
        3. 1,2, byinf ModusPonens |- g
        4: ass g ⇒ h
        5. 3,4, byinf ModusPonens |- h
        6. 2,4 |- f ⇒ h
        7. 2 |- (g ⇒ h) ⇒ (f ⇒ h)
        8. 6 |- (f ⇒ g) ⇒ ((g ⇒ h) ⇒ (f ⇒ h)) 
    }
    """

    /// <summary>
    /// Converts a unique anchor text plus an offset inside it into a 0-based (line, column) position.
    /// </summary>
    let locate (code: string) (anchor: string) (offset: int) : int64 * int64 =
        let index = code.IndexOf(anchor, StringComparison.Ordinal)
        Assert.IsTrue(index >= 0, $"Anchor '{anchor}' not found.")
        let target = index + offset
        let before = code.Substring(0, target)
        let line = before.Split('\n').Length
        let column = target - (before.LastIndexOf('\n') + 1)
        (int64 line, int64 column)

    /// <summary>
    /// Collects every node reachable from the root via Scope and ArgList, each node once.
    /// </summary>
    let collectNodes (root: FplGenericNode) =
        let visited = HashSet<FplGenericNode>(HashIdentity.Reference)
        let result = List<FplGenericNode>()
        let rec walk (node: FplGenericNode) =
            if visited.Add node then
                result.Add node
                for child in node.Scope.Values do
                    walk child
                for child in node.ArgList do
                    walk child
        walk root
        result

    /// <summary>
    /// Finds the innermost node of the given type name that contains the cursor position.
    /// </summary>
    let findCursorNode (line: int64) (column: int64) : FplGenericNode option =
        collectNodes heap.Root
        |> Seq.filter (fun node ->
            (node.StartPos.Line, node.StartPos.Column) <= (line, column)
            && (line, column) <= (node.EndPos.Line, node.EndPos.Column))
        |> Seq.sortBy (fun node -> (node.StartPos.Line, node.StartPos.Column, -node.EndPos.Line, -node.EndPos.Column))
        |> Seq.tryLast

    /// <summary>
    /// Compares reference nodes by their start line only. Columns of block nodes are not reliable (see GoToDefinition).
    /// </summary>
    [<DataRow("01", "axiom A1", 0, "10, 52, 56")>]
    [<DataRow("02", "byax A1", 5, "10, 52, 56")>]
    [<DataRow("03", "theorem T1", 0, "39, 45")>]
    [<TestMethod>]
    member _.TestFindReferencesLines(no:string, cursorAnchor: string, cursorOffset: int, expectedLines: string) =

        prepareFplCode("TestFindReferences.fpl", fplCode, false) |> ignore

        let cursorLine, cursorColumn = locate fplCode cursorAnchor cursorOffset
        let cursorNode = findCursorNode cursorLine cursorColumn
        Assert.IsTrue(cursorNode.IsSome, $"No node found at '{cursorAnchor}'.")

        let references =
            findReferenceNodes cursorNode.Value heap.Eval.FlattenedNodes

        let actual =
            references
            |> List.map (fun node -> node.StartPos.Line)
            |> List.sort
            |> List.map (fun f -> $"{f}")
            |> String.concat ", "

        Assert.AreEqual<string>(expectedLines, actual, $"Reference lines differ for '{cursorAnchor}'.")
