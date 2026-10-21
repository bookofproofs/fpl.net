namespace TestFpl3LanguageServer.ServiceGoToDefinition

open System
open System.Collections.Generic
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl2Interpreter.Helpers.Common



[<TestClass>]
type TestGoToDefinitionSameFile () =

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
    /// Finds the innermost node of the given type name that contains the 1-based cursor position.
    /// </summary>
    let findCursorNode (typeName: string) (line: int64) (column: int64) : FplGenericNode option =
        collectNodes heap.Root
        |> Seq.filter (fun node ->
            node.GetType().Name = typeName
            && (node.StartPos.Line, node.StartPos.Column) <= (line, column)
            && (line, column) <= (node.EndPos.Line, node.EndPos.Column))
        |> Seq.sortBy (fun node -> (node.StartPos.Line, node.StartPos.Column, -node.EndPos.Line, -node.EndPos.Column))
        |> Seq.tryLast

    [<DataRow("FplJustificationItemByAx", "byax A1", 5, "axiom A1", 0)>]
    [<DataRow("FplJustificationItemByAx", "byax A2", 5, "axiom A2", 0)>]
    [<DataRow("FplJustificationItemByRefArgument", "3. 1,2,", 3, "1: ass f", 0)>]
    [<TestMethod>]
    member this.TestGoToDefinitionDeclaration(nodeType: string, cursorAnchor: string, cursorOffset: int, declAnchor: string, declOffset: int) =
        prepareFplCode("TestGoToDefinition.fpl", fplCode, false) |> ignore

        let cursorLine, cursorColumn = locate fplCode cursorAnchor cursorOffset
        let cursorNode = findCursorNode nodeType cursorLine cursorColumn
        Assert.IsTrue(cursorNode.IsSome, $"No {nodeType} found at {cursorLine}:{cursorColumn}.")

        let declaration = resolveDeclaration cursorNode.Value
        Assert.IsTrue(declaration.IsSome, $"{nodeType} at '{cursorAnchor}' does not resolve to a declaration.")

        let expectedLine, expectedColumn = locate fplCode declAnchor declOffset
        Assert.AreEqual<int64>(expectedLine, declaration.Value.StartPos.Line, $"Line mismatch for '{declAnchor}'.")
        Assert.AreEqual<int64>(
            expectedColumn,
            declaration.Value.StartPos.Column,
            $"Column mismatch for '{declAnchor}': declaration {declaration.Value.GetType().Name} starts at column {declaration.Value.StartPos.Column}.")
