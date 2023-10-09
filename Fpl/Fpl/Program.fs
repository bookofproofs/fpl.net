open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamespace {
    theory {   
            func pdddddd
            (
            )  tplSass
        {
        }
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

