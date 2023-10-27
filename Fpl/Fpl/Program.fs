open ErrRecovery
open FplParser
open FParsec
open System.Text.RegularExpressions

let input = "TestNamespace {
    theory {   
        def cl T:obj
        {
            ctor T() { self; }
        }
    }
}"

(*
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics


let i = "all proceedingResult in p
                (
                    assert proceedingResult
                )"
let r = run predicate i
printf "%O" r
*)

let stringMatches (inputString: string) =
    let pattern = "(definition|def|mandatory|mand|optional|opt|axiom|ax|postulate|post|theorem|thm|proposition|prop|lemma|lem|corollary|cor|conjecture|conj|declaration|dec|constructor|ctor|proof|prf|inference|inf|localization|loc)\s+"
    let regex = new Regex(pattern)
    let matchList = regex.Matches(inputString) |> Seq.cast<Match> |> Seq.toList
    let rec collectMatches matchList index =
        match (matchList:Match list) with
        | [] -> 
            (index, inputString.Substring(index)) :: []
        | m::ms ->
            (index, inputString.Substring(index, m.Index - index)) :: collectMatches ms m.Index
    collectMatches matchList 0 

let result = stringMatches " def Hello, axiom World! " 
printfn "%A" result

(*
let         
    for m in matchList do
        let toBeParsed = input.Substring(m.Index)
        match m.Value with
        | v when v.StartsWith("definition") || v.StartsWith("def") 
            -> fplParserDef toBeParsed 
        | v when v.StartsWith("mand") || v.StartsWith("opt") 
            -> fplParserProperty toBeParsed
        | v when v.StartsWith("ax") || v.StartsWith("post") 
            -> fplParserAxiom toBeParsed
        | v when v.StartsWith("theorem") || v.StartsWith("thm") 
            -> fplParserTheorem toBeParsed
        | v when v.StartsWith("prop")
            -> fplParserProposition toBeParsed
        | v when v.StartsWith("lem") 
            -> fplParserLemma toBeParsed
        | v when v.StartsWith("cor") 
            -> fplParserCorollary toBeParsed
        | v when v.StartsWith("conj") 
            -> fplParserConjecture toBeParsed
        | v when v.StartsWith("dec") 
            -> fplParserDeclaration toBeParsed
        | v when v.StartsWith("constructor") || v.StartsWith("ctor") 
            -> fplParserConstructor toBeParsed
        | v when v.StartsWith("proof") || v.StartsWith("prf") 
            -> fplParserProof toBeParsed
        | v when v.StartsWith("inf") 
            -> fplParserInference toBeParsed
        | v when v.StartsWith("loc") 
            -> fplParserLocalization toBeParsed
        | _ -> fplParser toBeParsed
        |> ignore



ad.Clear()
parseMatches input
ad.PrintDiagnostics
*)