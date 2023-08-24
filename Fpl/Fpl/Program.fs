open FParsec
open FplGrammar


let result = run axiom """axiom ZeroIsNat()
        {
            is(Zero,Nat)
        }"""
printfn "%O" result

let result1 = run axiomBlock """{
            is(Zero,Nat)
        }"""
printfn "%O" result1

let result2 = run (leftBrace >>. many CW >>. variableSpecificationList) """{
            is(Zero,Nat)
        """
printfn "%O" result2
