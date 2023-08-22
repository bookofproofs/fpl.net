open FParsec
open FplGrammar


let result = run predicateWithArguments """ex$0 x,y,z(true)"""
let result1 = run existsTimesN """ex$1 x,y,z (not (iif ( true, not(false))))"""
let result2 = run existsTimesN """ex$2 x,y,z (not (iif ( iif( true, false), true)))"""
let result3 = run existsTimesN """ex$3 x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result2
printfn "%O" result3
