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

(*
let i = "cases
            (
                | Equal(n,0):
                    return m.NeutralElem();
                else
                    return op(
                        y,
                        Exp( m(y,op), y, Sub(n,1))
                    )
            )"
let r = run casesStatement i
printf "%O" r
*)