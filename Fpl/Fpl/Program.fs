open FplGrammarTypes
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open FParsec


let input = "T { inf { D() { pre : true con : true     }     theory { y} }"
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics


ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics


