namespace TestFpl3LanguageServer.ServiceFindReference

open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Text.RegularExpressions
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open TestFpl3LanguageServer.Helpers.SampleFpl

[<TestClass>]
type TestFindReferences() =


    let firstRelevantTokens (input: string) =
        let matches = Regex.Matches(input, @"\S+")

        match matches.Count with
        | 0 ->
            ""
        | 1 ->
            matches[0].Value
        | _ ->
            let first = matches[0]
            let second = matches[1]
            let whitespaceBetween =
                input.Substring(
                    first.Index + first.Length,
                    second.Index - (first.Index + first.Length)
                )

            if whitespaceBetween |> Seq.exists (fun c -> c = '\n' || c = '\r') then
                first.Value
            else
                first.Value + " " + second.Value


    [<DataRow("01", "axiom A1", 0, "1~10~axiom A1, 1~52~byax A1, 1~56~byax A1", 1)>]
    [<DataRow("01a", "byax A1", 5, "1~10~axiom A1, 1~52~byax A1, 1~56~byax A1", 1)>]
    [<DataRow("02", "theorem T1", 0, "1~39~theorem T1, 1~45~proof T1$1, 1~63~cor T1$2", 1)>]
    [<DataRow("02a", "cor T1$2", 0, "1~39~theorem T1, 1~63~cor T1$2", 1)>]
    [<DataRow("03", "inf ModusPonens", 0, "1~30~inf ModusPonens, 1~54~byinf ModusPonens, 1~58~byinf ModusPonens, 1~75~byinf ModusPonens, 1~77~byinf ModusPonens", 1)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "1~30~inf ModusPonens, 1~54~byinf ModusPonens, 1~58~byinf ModusPonens, 1~75~byinf ModusPonens, 1~77~byinf ModusPonens", 1)>]
    // find references to arguments that have justifications
    [<DataRow("04", "6 |-", 0, "1~78~6. 2,4, 1~80~6 |-", 1)>]
    [<DataRow("04a", "6.", 0, "1~78~6. 2,4, 1~80~6 |-", 1)>]
    [<DataRow("05", "4. byax A1 |-", 0, "1~56~4. byax, 1~58~4, 3,, 1~81~T1$1:4 |-", 1)>]
    [<DataRow("05a", "T1$1:4", 0, "1~56~4. byax, 1~58~4, 3,, 1~81~T1$1:4 |-", 1)>]
    // find references to arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "1~73~1: ass, 1~75~1,2, byinf, 1~86~T2$1:1 |-", 1)>]
    [<DataRow("06a", "1:", 0, "1~73~1: ass, 1~75~1,2, byinf, 1~86~T2$1:1 |-", 1)>]
    // cross-file referencing
    [<DataRow("07", "lem Le", 0, "1~91~lem Le, 2~4~cor Le$1, 2~11~Le, Pr", 1)>]
    [<DataRow("07a", "Le, Pr", 0, "1~91~lem Le, 2~11~Le, Pr", 2)>]
    [<DataRow("08", "conj Co", 0, "1~93~conj Co, 2~10~byconj Co", 1)>]
    [<DataRow("08a", "byconj Co", 0, "1~93~conj Co, 2~10~byconj Co", 2)>]
    [<DataRow("09", "cor Le$1", 0, "1~91~lem Le, 2~4~cor Le$1, 2~5~cor Le$1$2, 2~9~bycor Le$1, 2~13~bycor Le$1", 2)>] 
    [<DataRow("09a", "bycor Le$1", 0, "2~4~cor Le$1, 2~9~bycor Le$1, 2~13~bycor Le$1", 2)>] 
    [<DataRow("10", "cor Le$1$2", 0, "2~4~cor Le$1, 2~5~cor Le$1$2, 2~7~prf Le$1$2$1, 2~12~bycor Le$1$2, 2~14~bycor Le$1$2", 2)>] 
    [<DataRow("10a", "bycor Le$1$2", 0, "2~5~cor Le$1$2, 2~12~bycor Le$1$2, 2~14~bycor Le$1$2", 2)>] 

    // group 1: variables
    [<DataRow("V01", "x, y:", 0, "2~20~x, y:, 2~21~x ⇒, 2~21~x)) ⇒, 2~21~x ⇒, 2~21~x)", 2)>]
    [<DataRow("V01a", "(x ⇒ (y ⇒ x))", 1, "2~20~x, y:, 2~21~x ⇒, 2~21~x)) ⇒, 2~21~x ⇒, 2~21~x)", 2)>]
    [<DataRow("V01b", "y ⇒ x", 4, "2~20~x, y:, 2~21~x ⇒, 2~21~x)) ⇒, 2~21~x ⇒, 2~21~x)", 2)>]
    [<DataRow("V01c", "x ⇒ x", 0, "2~20~x, y:, 2~21~x ⇒, 2~21~x)) ⇒, 2~21~x ⇒, 2~21~x)", 2)>]
    [<DataRow("V01d", "x ⇒ x", 4, "2~20~x, y:, 2~21~x ⇒, 2~21~x)) ⇒, 2~21~x ⇒, 2~21~x)", 2)>]

    // group 2: predicate and byDef
    [<DataRow("P01", "pred Q", 5, "2~25~pred Q(f:, 2~33~Q(f), 2~38~bydef Q", 2)>]
    [<DataRow("P01a", "Q(f)", 0, "2~25~pred Q(f:, 2~33~Q(f), 2~38~bydef Q", 2)>]
    [<DataRow("P01b", "bydef Q", 6, "2~25~pred Q(f:, 2~33~Q(f), 2~38~bydef Q", 2)>]

    // group 3: functional term 
    [<DataRow("F01", "def func Id", 4, "2~48~func Id(f:, 2~57~Id(true) }", 2)>]
    [<DataRow("F01a", "Id(true)", 1, "2~48~func Id(f:, 2~57~Id(true) }", 2)>]

    // classes 
    [<DataRow("C01", "class Cls", 0, "2~54~class Cls, 2~60~Cls, 2~63~Cls, 2~67~Cls", 2)>]
    [<DataRow("C01a", "Sub : Cls", 6, "2~54~class Cls, 2~60~Cls, 2~63~Cls, 2~67~Cls", 2)>]
    [<DataRow("C02", "cl Cl1", 0, "2~70~cl Cl1, 2~71~Cl1)", 2)>]
    [<DataRow("C02a", "z:Cl1", 3, "2~70~cl Cl1, 2~71~Cl1)", 2)>]

    // group 9: properties (FplMandatoryPredicate and FplMandatoryFunctionalTerm)
    [<DataRow("Pp01", "prty pred PropP", 10, "2~86~prty pred, 2~87~.PropP() }", 2)>]
    [<DataRow("Pp01a", "x.PropP()", 2, "2~86~prty pred, 2~87~.PropP() }", 2)>]
    [<DataRow("Pp02", "prty func PropF", 10, "2~88~prty func, 2~89~.PropF() }", 2)>]
    [<DataRow("Pp02a", "x.PropF()", 2, "2~88~prty func, 2~89~.PropF() }", 2)>]

    // group 10: extensions (FplExtension and FplExtensionObj)
    [<DataRow("Ext01", "ext Digits", 4, "2~92~ext Digits, 2~93~@1}", 2)>]
    [<DataRow("Ext01a", "@1}", 0, "2~92~ext Digits, 2~93~@1}", 2)>]

    // group 11: array type occurrences (main types, involve FplVariableArray)
    [<DataRow("Arr01", "cl ArrCl", 3, "2~96~cl ArrCl, 2~97~ArrCl]; true}", 2)>]

    // group 12: array type occurrences (including index references)
    [<DataRow("Arr02", "cl ArrIdx", 3, "2~100~cl ArrIdx, 2~101~ArrIdx arr:*ind[obj]", 2)>]

    // group 13: array variable references (involve FplVariableArray)
    [<DataRow("Arr03", "x:*pred[ind,ind]", 0, "2~104~x:*pred[ind,ind] x[i,j]:=x[j,i];, 2~104~x[i,j]:=x[j,i]; true}, 2~104~x[j,i]; true}", 2)>]

    // constructors
    [<DataRow("Co1", "ctor Cls()", 6, "2~56~ctor Cls(), 2~64~Cls(), 2~66~base.Cls(); }", 2)>]
    [<DataRow("Co1a", "x:=Cls()", 4, "2~56~ctor Cls(), 2~64~Cls(), 2~66~base.Cls(); }", 2)>]
    [<DataRow("Co1b", "base.Cls()", 6, "2~56~ctor Cls(), 2~64~Cls(), 2~66~base.Cls(); }", 2)>]

    // group 6: proposition as cursor target 
    [<DataRow("PR01", "prop Pr", 5, "1~89~prop Pr, 2~11~Pr |-", 1)>]
    [<DataRow("PR01a", "Le, Pr |-", 4, "1~89~prop Pr, 2~11~Pr |-", 2)>]

    // group 8: non existing reference
    [<DataRow("R01", "byax ExtAx", 5, "2~44~byax ExtAx", 2)>]

    // infix operation
    [<DataRow("I01", "pred Inf", 0, "2~76~pred Inf(a,, 2~77~=> y, 2~77~=> z}", 2)>]
    [<DataRow("I01a", "x => y", 2, "2~76~pred Inf(a,, 2~77~=> y, 2~77~=> z}", 2)>]
    [<DataRow("I01b", "y => z", 3, "2~76~pred Inf(a,, 2~77~=> y, 2~77~=> z}", 2)>]

    // postfix operation
    [<DataRow("Pos01", "func Post", 0, "2~79~func Post(a:, 2~80~' }", 2)>]
    [<DataRow("Pos01a", "x'", 1, "2~79~func Post(a:, 2~80~' }", 2)>]

    // prefix operation
    [<DataRow("Pre01", "func Pre", 0, "2~82~func Pref(a:, 2~83~-x }", 2)>]
    [<DataRow("Pre01a", "-x", 0, "2~82~func Pref(a:, 2~83~-x }", 2)>]

    // symbol operation
    [<DataRow("Sym01", "cl Cl2", 0, "2~73~cl Cl2, 2~74~∅ =, 2~74~∅ }", 2)>]
    [<DataRow("Sym01a", "∅ = ∅", 0, "2~73~cl Cl2, 2~74~∅ =, 2~74~∅ }", 2)>]
    [<DataRow("Sym02a", "∅ = ∅", 4, "2~73~cl Cl2, 2~74~∅ =, 2~74~∅ }", 2)>]

    // group 14: is operator (the second argument is a type reference)
    [<DataRow("IS01", "cl IsCl", 3, "2~107~cl IsCl, 2~108~IsCl)}", 2)>]
    [<DataRow("IS01a", "is(x,IsCl)", 5, "2~107~cl IsCl, 2~108~IsCl)}", 2)>]

    [<TestMethod>]
    member _.TestFindReferences(no:string, cursorAnchor: string, cursorOffset: int, expectedLines: string, where:int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where 

        let references =
            findReferenceNodes cursorNode.Value heap.SymbolTable.Nodes

        let actual =
            references
            |> List.sortBy (fun node -> (node.FilePath.Contains("TestSecondCode"), node.StartPos.Line))
            |> List.map (fun node ->
                if node.FilePath.Contains("TestFirstCode") then
                    let minCutOff = min 100 (fplFirstCode.Length - (int)node.StartPos.Index)
                    $"1~{node.StartPos.Line}~{firstRelevantTokens( fplFirstCode.Substring((int)node.StartPos.Index, minCutOff) )}"
                elif node.FilePath.Contains("TestSecondCode") then
                    let minCutOff = min 100 (fplSecondCode.Length - (int)node.StartPos.Index)
                    $"2~{node.StartPos.Line}~{firstRelevantTokens( fplSecondCode.Substring((int)node.StartPos.Index, minCutOff) )}"
                else
                    failwith $"Unspecified theory {node.FilePath}"
            )
            |> List.map (fun f -> $"{f}")
            |> String.concat ", "

        let info =
            references
            |> Seq.map (fun n ->
                $"{n.GetType().Name}:({n.StartPos.Line},{n.StartPos.Column})-({n.EndPos.Line},{n.EndPos.Column})"    
            )
            |> String.concat Environment.NewLine


        printfn "%s" info
        let rec ancestors (n: FplGenericNode) =
            match n.Parent with
            | Some p -> $"{p.GetType().Name}@{p.StartPos.Line}:{p.StartPos.Column}-{p.EndPos.Line}:{p.EndPos.Column}" :: ancestors p
            | None -> []

        Assert.AreEqual<string>(expectedLines, actual, $"Reference lines differ for '{cursorAnchor}'.")
