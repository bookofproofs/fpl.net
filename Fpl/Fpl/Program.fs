﻿open FplGrammarTypes
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamespace {
    theory {   
        ax T () 
        {
    }
    y
}"
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics


ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics


