open FParsec
open FplGrammar


let result = run generalType """+predicate"""
let result1 = run generalType """*function"""
let result2 = run generalType """object"""
let result3 = run generalType """tpl"""
let result4 = run generalType """tplSetElem"""
let result5 = run generalType """index"""
let result6 = run generalType """@extNat"""
let result7 = run generalType """bla"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result2
printfn "%O" result3
printfn "%O" result4
printfn "%O" result5
printfn "%O" result6
printfn "%O" result7
