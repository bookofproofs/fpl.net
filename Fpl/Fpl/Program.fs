open FParsec
open FplGrammar


let result = run specificType """predicate"""
let result1 = run specificType """function"""
let result2 = run specificType """object"""
let result3 = run specificType """tpl"""
let result4 = run specificType """tplSetElem"""
let result5 = run specificType """index"""
let result6 = run specificType """bla"""
printfn "%O" result
printfn "%O" result1
printfn "%O" result2
printfn "%O" result3
printfn "%O" result4
printfn "%O" result5
printfn "%O" result6
