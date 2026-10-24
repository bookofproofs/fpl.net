module TestFpl3LanguageServer.Helpers.SampleFpl

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Storage.Heap

/// <summary>
/// Shared FPL sample code for the find-references and go-to-definition tests.
/// Append new cases at the end so that existing line numbers in the data rows stay valid.
/// </summary>
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

    cor T1$2 {true}

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
        9. T1$1:4 |- trivial
    }

    prf T2$2
    {
        1. T2$1:1 |- trivial
    }
    """

/// <summary>
/// Converts a unique anchor text plus an offset inside it into a 1-based (line, column) position.
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
/// Returns the 1-based line of the first occurrence of an anchor text.
/// </summary>
let lineOf (code: string) (anchor: string) : int64 =
    fst (locate code anchor 0)

/// <summary>
/// Normalizes a path the same way the language server handler does.
/// </summary>
let normalizePath (path: string) =
    Path.GetFullPath(path).Replace('\\', '/').TrimEnd('/')

/// <summary>
/// Returns the normalized path of a test file, in the same directory that <c>prepareFplCode</c> writes to.
/// </summary>
let testFilePath (fileName: string) =
    normalizePath (Path.Combine(Directory.GetCurrentDirectory(), fileName))

/// <summary>
/// Finds the innermost symbol-table node at the cursor position, using the same lookup as the language server.
/// </summary>
/// <param name="fileName">File name passed to <c>prepareFplCode</c>.</param>
/// <param name="code">The FPL code that was prepared.</param>
/// <param name="cursorAnchor">Unique text that marks the cursor position.</param>
/// <param name="cursorOffset">Offset inside the anchor.</param>
let cursorNodeAt (fileName: string) (code: string) (cursorAnchor: string) (cursorOffset: int) : FplGenericNode option =
    let line, column = locate code cursorAnchor cursorOffset
    heap.Eval.CursorNode (testFilePath fileName) line column
