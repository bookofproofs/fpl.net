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

    // group 1: variables
    [<DataRow("V01", "x, y:", 0, "x, y: pred", 2)>]
    [<DataRow("V01a", "(x ⇒ (y ⇒ x))", 1, "x, y: pred", 2)>]
    [<DataRow("V01b", "y ⇒ x", 4, "x, y: pred", 2)>]
    [<DataRow("V01c", "x ⇒ x", 0, "x, y: pred", 2)>]
    [<DataRow("V01d", "x ⇒ x", 4, "x, y: pred", 2)>]

    // group 2: predicate and byDef
    [<DataRow("P01", "pred Q", 5, "pred Q", 2)>]
    [<DataRow("P01a", "Q(f)", 0, "pred Q", 2)>]
    [<DataRow("P01b", "bydef Q", 6, "pred Q", 2)>]

    // group 3: functional term 
    [<DataRow("F01", "def func Id", 4, "func Id", 2)>]
    [<DataRow("F01a", "Id(true)", 1, "func Id", 2)>]

    // classes 
    [<DataRow("C01", "class Cls", 0, "class Cls", 2)>]
    [<DataRow("C01a", "Sub : Cls", 6, "class Cls", 2)>]
    [<DataRow("C02", "cl Cl1", 0, "cl Cl1", 2)>]
    [<DataRow("C02a", "z:Cl1", 3, "cl Cl1", 2)>]

    // group 9: properties (FplMandatoryPredicate and FplMandatoryFunctionalTerm)
    [<DataRow("Pp01", "prty pred PropP", 10, "prty pred PropP", 2)>]
    [<DataRow("Pp01a", "x.PropP()", 2, "prty pred PropP", 2)>]
    [<DataRow("Pp02", "prty func PropF", 10, "prty func PropF", 2)>]
    [<DataRow("Pp02a", "x.PropF()", 2, "prty func PropF", 2)>]

    // group 10: extensions (FplExtension and FplExtensionObj)
    [<DataRow("Ext01", "ext Digits", 4, "ext Digits", 2)>]
    [<DataRow("Ext01a", "@1}", 0, "ext Digits", 2)>]

    // group 11: array type occurrences (main types, involve FplVariableArray)
    [<DataRow("Arr01", "cl ArrCl", 3, "cl ArrCl", 2)>]

    // group 12: array type occurrences (including index references)
    [<DataRow("Arr02", "cl ArrIdx", 3, "cl ArrIdx", 2)>]

    // group 13: array variable references (involve FplVariableArray)
    [<DataRow("Arr03", "x:*pred[ind,ind]", 0, "x:*pred[ind,ind]", 2)>]

    // constructors
    [<DataRow("Co1", "ctor Cls()", 6, "ctor Cls()", 2)>]
    [<DataRow("Co1a", "x:=Cls()", 4, "ctor Cls()", 2)>]
    [<DataRow("Co1b", "base.Cls()", 6, "ctor Cls()", 2)>]

    // group 6: proposition as cursor target 
    [<DataRow("PR01", "prop Pr", 5, "prop Pr", 1)>]
    [<DataRow("PR01a", "Le, Pr |-", 4, "prop Pr", 2)>]

    // infix operation
    [<DataRow("I01", "pred Inf", 0, "pred Inf", 2)>]
    [<DataRow("I01a", "x => y", 2, "pred Inf", 2)>]
    [<DataRow("I01b", "y => z", 3, "pred Inf", 2)>]

    // postfix operation
    [<DataRow("Pos01", "func Post", 0, "func Post", 2)>]
    [<DataRow("Pos01a", "x'", 1, "func Post", 2)>]

    // prefix operation
    [<DataRow("Pre01", "func Pre", 0, "func Pref", 2)>]
    [<DataRow("Pre01a", "-x", 0, "func Pref", 2)>]

    // symbol operation
    [<DataRow("Sym01", "cl Cl2", 0, "cl Cl2", 2)>]
    [<DataRow("Sym01a", "∅ = ∅", 0, "cl Cl2", 2)>]
    [<DataRow("Sym02a", "∅ = ∅", 4, "cl Cl2", 2)>]

    // group 14: is operator (the second argument is a type reference)
    [<DataRow("IS01", "cl IsCl", 3, "cl IsCl", 2)>]
    [<DataRow("IS01a", "is(x,IsCl)", 5, "cl IsCl", 2)>]

    // TODO: Alias
    [<TestMethod>]
    member this.TestGoToDeclaration(no:string, cursorAnchor: string, cursorOffset: int, expectedPos: string, where:int) =

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

    // group 8: non existing reference
    [<DataRow("R01", "byax ExtAx", 5, "2~44~byax ExtAx", 2)>]
    [<TestMethod>]
    member this.TestGoToNonExistingDeclaration(no:string, cursorAnchor: string, cursorOffset: int, expectedPos: string, where:int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where 

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        let declaration = resolveDeclaration (cursorTargetNode cursorNode.Value)
        Assert.IsTrue(declaration.IsNone, $"node at '{cursorAnchor}' is {cursorNode.Value.Name} and resolve to a declaration, but was ment to be non-existing, please use a non-existing identifier for this test.")

