﻿open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamescpace {
    d
    theory {
        P
    }
}"
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics
