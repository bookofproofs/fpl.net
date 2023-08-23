open FParsec
open FplGrammar


let result = run generalType """object[self]"""
let result1 = run generalType """*function"""
let result2 = run generalType """object[self]"""
let result3 = run generalType """tpl[from~]"""
let result3a = run generalType """tpl[~to]"""
let result3b = run generalType """tpl[~]"""
let result4 = run generalType """Set[from ~ to]"""
let result5 = run generalType """index"""
let result6 = run generalType """@extNat"""
let result7 = run generalType """bla"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result2
printfn "%O" result3
printfn "%O" result3a
printfn "%O" result3b
printfn "%O" result4
printfn "%O" result5
printfn "%O" result6
printfn "%O" result7
