open FplGrammarTypes
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamespace {
    theory {   
    ax T () 
    { true }
    pred T ( ) { }     
    } loc { true := ~ tex : \"\\operatorname{true}\" ; "

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics


let strings = ["@"; "}"; "<PascalCase"]

printf "%A" (List.sort strings)
