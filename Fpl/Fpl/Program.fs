open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = "P["

let result = run (classType .>> eof) input
printf "%O" result
