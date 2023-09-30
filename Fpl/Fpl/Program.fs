open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = """ """

let result = run predicate input
printf "%O" result