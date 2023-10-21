open ErrRecovery
open FplParser
open FParsec


let input = "TestNamespace {
    theory {   
        ax T () 
        {
            intr
        }
    }
    y
}"

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics


let i = "cl A:obj 
        {
    }"
let r = run definitionClass i
printf "%O" r

