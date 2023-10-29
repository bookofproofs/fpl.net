open ErrRecovery
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = "T  uses Fpl.Commons s
    uses Fpl.Commons.Structures 
    uses Fpl.SetTheory.ZermeloFraenkel
    uses Fpl.Algebra.Structures 
    }

}


"

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"
