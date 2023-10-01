open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = "result:=Add(result,arr[i])"

let result = run (assignmentStatement .>> eof) input
printf "%O" result
