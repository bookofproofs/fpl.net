open FParsec
open FplGrammar


let res = run primePredicate """myOp.NeutralElement()"""
printfn "%O" res

