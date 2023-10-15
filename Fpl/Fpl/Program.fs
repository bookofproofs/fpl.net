open ErrRecovery
open FplParser
open FParsec


let input = "TestNamespace {
    theory {   
        func
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


let i = "pred T()
        {
            x,
            true
        }"
let r = run definitionPredicate i
printf "%O" r

let i1 = "func T() ->S
        {
            x,
            true
        }"
let r1 = run definitionFunctionalTerm i1
printf "%O" r1
