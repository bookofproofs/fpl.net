open FParsec
open FplGrammar


let result = run entityWithCoord """myField[1 ~ n]"""
let result1 = run entityWithCoord """ex$1 x,y,z (not (iif ( true, not(false))))"""
let result2 = run entityWithCoord """ex$2 x,y,z (not (iif ( iif( true, false), true)))"""
let result3 = run entityWithCoord """ex$3 x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result2
printfn "%O" result3
