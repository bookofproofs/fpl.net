open FParsec
open FplGrammar


let res = run loopStatement """loop n [Nat(1)~limit]
            (
            assert Equal(f(n),n)
            )"""
printfn "%O" res

let res1 = run variableRange """[Nat(1)~limit]"""
printfn "%O" res1

let res2 = run primePredicate """@@self"""
printfn "%O" res2
