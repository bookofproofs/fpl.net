﻿open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamespace {
    inf {
        D(x:tpl[< )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics

