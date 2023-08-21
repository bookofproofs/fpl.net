open FParsec
open FplGrammar


let result = run all """all x,y,z(true)"""
let result1 = run negation """all x,y,z (not (iif ( true, not(false))))"""
let result2 = run negation """all x,y,znot (iif ( iif( true, false), true))"""
let result3 = run negation """not(iif ( iif ( true, iif( true, false)), not(true) ))"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result2
printfn "%O" result3
