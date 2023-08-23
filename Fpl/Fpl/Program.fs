open FParsec
open FplGrammar


let result = run predicate """is ( x,Nat)"""
let result1 = run variableType """*func"""
let result1a = run variableType """*predicate"""
let result2 = run variableType """object[self]"""
let result3 = run variableType """tpl[from~]"""
let result3a = run variableType """tpl[~to]"""
let result3b = run variableType """tpl[~]"""
let result4 = run variableType """Set[from ~ to]"""
let result5 = run variableType """index"""
let result6 = run variableType """@extNat"""
let result7 = run variableType """bla"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result1a
printfn "%O" result2
printfn "%O" result3
printfn "%O" result3a
printfn "%O" result3b
printfn "%O" result4
printfn "%O" result5
printfn "%O" result6
printfn "%O" result7
