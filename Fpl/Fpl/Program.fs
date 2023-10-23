open ErrRecovery
open FplParser
open FParsec


let input = "TestNamespace {
    theory {   
        cl T:obj
        {
            T() { self }
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


let i = "all proceedingResult in p
                (
                    assert proceedingResult
                )"
let r = run predicate i
printf "%O" r


