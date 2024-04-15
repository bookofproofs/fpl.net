open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FplInterpreterTypes
open FParsec
open System.Text.RegularExpressions
open System.Collections.Generic




let input = """inf ModusPonens()
{
    dec ~p,q: pred;

    premise:
        and (p, impl (p,q) )
    conclusion:
        q
}
"""

let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
let parsedAsts = System.Collections.Generic.List<ParsedAst>()
FplInterpreter.fplInterpreter input (System.Uri("file:///d%3A/Forschung/fpl.net/theories/Landau/Test.fpl")) fplLibUrl parsedAsts true |> ignore
printf "%A" parsedAsts

printf "\n--------------------------------\n"
ad.PrintDiagnostics

printf "\n--------------------------------\n"


let mutable test = []
test <- test @ [""]