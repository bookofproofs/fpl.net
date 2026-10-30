namespace TestFpl3LanguageServer.ServiceFindReference

open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Text.RegularExpressions
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


    [<DataRow("01", "axiom A1", 0, "1~axiom A1, 1~byax A1, 1~byax A1", 1)>]
    [<DataRow("01a", "byax A1", 5, "1~axiom A1, 1~byax A1, 1~byax A1", 1)>]
    [<DataRow("02", "theorem T1", 0, "1~cor T1$2, 1~proof T1$1, 1~theorem T1", 1)>]
    [<DataRow("02a", "cor T1$2", 0, "1~cor T1$2, 1~proof T1$1, 1~theorem T1", 1)>]
    [<DataRow("02b", "cor T1$2", 0, "1~cor T1$2, 1~proof T1$1, 1~theorem T1", 1)>]
    [<DataRow("03", "inf ModusPonens", 0, "1~byinf ModusPonens, 1~byinf ModusPonens, 1~byinf ModusPonens, 1~byinf ModusPonens, 1~inf ModusPonens", 1)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "1~byinf ModusPonens, 1~byinf ModusPonens, 1~byinf ModusPonens, 1~byinf ModusPonens, 1~inf ModusPonens", 1)>]
    // find references to arguments that have justifications
    [<DataRow("04", "6 |-", 0, "1~6 |-, 1~6. 2,4", 1)>]
    [<DataRow("04a", "6.", 0, "1~6 |-, 1~6. 2,4", 1)>]
    [<DataRow("05", "4. byax A1 |-", 0, "1~4, 3,, 1~4. byax, 1~T1$1:4 |-", 1)>]
    [<DataRow("05a", "T1$1:4", 0, "1~4, 3,, 1~4. byax, 1~T1$1:4 |-", 1)>]
    // find references to arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "1~1,2, byinf, 1~1: ass, 1~T2$1:1 |-", 1)>]
    [<DataRow("06a", "1:", 0, "1~1,2, byinf, 1~1: ass, 1~T2$1:1 |-", 1)>]
    // cross-file referencing
    [<DataRow("07", "lem Le", 0, "1~lem Le, 2~Le, Pr, 2~cor Le$1", 1)>]
    [<DataRow("07a", "Le, Pr", 0, "1~lem Le, 2~Le, Pr, 2~cor Le$1", 2)>]
    [<DataRow("08", "conj Co", 0, "1~conj Co, 2~byconj Co", 1)>]
    [<DataRow("08a", "byconj Co", 0, "1~conj Co, 2~byconj Co", 2)>]
    [<DataRow("09", "cor Le$1", 0, "1~lem Le, 2~bycor Le$1, 2~cor Le$1", 2)>] // find references of a corollary includes its parent (here: lemma), bycor Le$1, and the corollary itself
    [<DataRow("09a", "bycor Le$1", 0, "2~bycor Le$1, 2~cor Le$1", 2)>] // find references of bycor includes its parent (here: corollary), and itself
    [<DataRow("10", "cor Le$1$2", 0, "2~cor Le$1, 2~cor Le$1$2, 2~bycor Le$1$2", 2)>] // find references of a subcorollary includes its parent (here: corollary), itself, and bycor Le$1$2
    [<DataRow("10a", "bycor Le$1$2", 0, "2~cor Le$1$2, 2~bycor Le$1$2", 2)>] // find references of bycor subcorollary finds the subcorollary and itself
    
    [<TestMethod>]
    member _.TestFindReferencesLines(no:string, cursorAnchor: string, cursorOffset: int, expectedLines: string, where:int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where 

        let references =
            findReferenceNodes cursorNode.Value heap.SymbolTable.Nodes

        let actual =
            references
            |> List.map (fun node ->
                if node.FilePath.Contains("TestFirstCode") then
                    let minCutOff = min 100 (fplFirstCode.Length - (int)node.StartPos.Index)
                    $"1~{firstRelevantTokens( fplFirstCode.Substring((int)node.StartPos.Index, minCutOff) )}"
                elif node.FilePath.Contains("TestSecondCode") then
                    let minCutOff = min 100 (fplSecondCode.Length - (int)node.StartPos.Index)
                    $"2~{firstRelevantTokens( fplSecondCode.Substring((int)node.StartPos.Index, minCutOff) )}"
                else 
                    failwith $"Unspecified theory {node.FilePath}"
            )
            |> List.sort
            |> List.map (fun f -> $"{f}")
            |> String.concat ", "

        Assert.AreEqual<string>(expectedLines, actual, $"Reference lines differ for '{cursorAnchor}'.")
