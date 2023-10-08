open FplGrammarTypes
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamescpace { theory { 
    
    theorem T() {
  s
}


  } }"

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics

