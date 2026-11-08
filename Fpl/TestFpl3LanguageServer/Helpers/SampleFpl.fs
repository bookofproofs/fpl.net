module TestFpl3LanguageServer.Helpers.SampleFpl


open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl2Interpreter.Helpers.Common

/// <summary>
/// Shared FPL sample code for the find-references and go-to-definition tests.
/// Append new cases at the end so that existing line numbers in the data rows stay valid.
/// </summary>
let fplFirstCode = """
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

    prop Pr {true}

    lem Le {true}

    conj Co {true}
    """

let fplSecondCode = """
    uses TestFirstCode

    cor Le$1 {true}
    cor Le$1$2 {true}

    prf Le$1$2$1 
    {
        1. bycor Le$1 |- true
        2. byconj Co |- true
        3. Le, Pr |- true
        4. bycor Le$1$2 |- true
        5. bycor Le$1 |- true
        6. bycor Le$1$2 |- true
    }

    // group 1: variables used several times
    axiom A4
    {
        dec x, y: pred;
        (x ⇒ (y ⇒ x)) ⇒ (x ⇒ x)
    }

    // group 2: predicate used by byDef
    def pred Q(f: pred) 
    {
        f
    }

    thm T3
    {
        dec f: pred;
        Q(f)
    }

    prf T3$1
    {
        1. bydef Q |- f
    }

    // group 8: justification referring to a declaration in the second file (invalid by design)
    prf T3$2
    {
        1. byax ExtAx |- true
    }

    // group 3: functional term (grammar unverified)
    def func Id(f: pred) -> pred
    {
        return f
    }

    // groups 4 and 5: classes with inheritance and a property (grammar unverified)
    def class Cls
    {
        ctor Cls() { }
        prty pred Pp() { Id(true) }
    }

    def class Sub : Cls
    {
        dec
            x:Cls
            x:=Cls()
        ;
        ctor Sub() { dec base.Cls(); }
        prty func T()->Cls
    }

    def cl Cl1
    def pred Cl1Test(x,y,z:Cl1)

    def cl Cl2 symbol "∅" 
    def pred Cl2Test() { ∅ = ∅ }

    def pred Inf(a, b: obj) infix "=>" 1
    def pred InfTest() { dec x,y,z:obj; x => y => z}

    def func Post(a: obj) -> obj postfix "'" 
    def pred PostTest() { dec x:obj; x' }

    def func Pref(a: obj) -> obj prefix "-" 
    def pred PrefTest() { dec x:obj; -x }

    // group 9: properties (FplMandatoryPredicate and FplMandatoryFunctionalTerm)
    def cl PropCl {intr prty pred PropP() }
    def pred PropTest() { dec x:PropCl; x.PropP() }
    def cl PropFuncCl {intr prty func PropF()->obj }
    def pred PropFuncTest() { dec x:PropFuncCl; x.PropF() }

    // group 10: extensions (FplExtension and FplExtensionObj)
    ext Digits x @ /\d+/ -> N {return x}
    def pred ExtTest() {@1}

    // group 11: array type occurrences (main types, involve FplVariableArray)
    def cl ArrCl {intr}
    def pred ArrTypeTest() {dec arr:*ind[ArrCl]; true}

    // group 12: array type occurrences (including index references)
    def cl ArrIdx {intr}
    def pred ArrIdxTest() {dec i:ArrIdx arr:*ind[obj] arr[i]:=undef; true}

    // group 13: array variable references (involve FplVariableArray)
    def pred ArrVarTest() {dec i,j:ind x:*pred[ind,ind] x[i,j]:=x[j,i]; true}

    // group 14: is operator (the second argument is a type reference)
    def cl IsCl {intr}
    def pred IsTest() {dec x:obj; is(x,IsCl)}
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
    heap.SymbolTable.CursorNode (testFilePath fileName) line column

/// <summary>
/// Writes an file with some content the innermost symbol-table node at the cursor position, using the same lookup as the language server.
/// </summary>
let writeCode (content:string) (fileName:string) =
    let path = Path.Combine(Directory.GetCurrentDirectory(), fileName)
    File.WriteAllText(path, content)
    path 

let assertPrepareWorked cursorAnchor cursorOffset where =
    let pathFirstCode = normalizePath (writeCode fplFirstCode "TestFirstCode.fpl")
    let pathSecondCode = normalizePath (writeCode fplSecondCode "TestSecondCode.fpl")
    // second code is main theory
    prepareFplCode("TestSecondCode.fpl", fplSecondCode, false) |> ignore

    let codeToLookForAnchors =
        match where with
        | 1 -> fplFirstCode
        | 2 -> fplSecondCode
        | _ -> failwith $"unknown code number to look for {where}"

    let pathToLookForAnchors =
        match where with
        | 1 -> pathFirstCode
        | 2 -> pathSecondCode
        | _ -> failwith $"unknown code number to look for {where}"

    let cursorLine, cursorColumn = locate codeToLookForAnchors cursorAnchor cursorOffset
    let cursorNode = heap.SymbolTable.CursorNode pathToLookForAnchors cursorLine cursorColumn
    (cursorLine, cursorColumn, cursorNode)
