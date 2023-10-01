open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = "[! "

let result = run (leftBound .>> eof) input
printf "%O" result
