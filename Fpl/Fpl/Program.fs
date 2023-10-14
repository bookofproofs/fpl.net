open ErrRecovery
open FplGrammar
open FParsec


let input = "TestNamespace {
    theory {   
        pred T()
        {
            x:* 
            true
        }
        y
    }
}"

let input1 = "TestNamespace {
    theory {   
        cl T:obj
        {
            x:* true
        }
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

ad.Clear()
let origResult1 = tryParse' ast "recovery failed;" ad input1
printf "%O" origResult1
ad.PrintDiagnostics


let input3 = "P"
let res = run variableType input3
printf "%O" res
