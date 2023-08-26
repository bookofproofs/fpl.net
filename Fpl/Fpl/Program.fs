open FParsec
open FplGrammar


let res = run proof """proof Example4$1
        {
            a:A
            b:B
            c:C
            x,y,z: obj

            1. GreaterAB() |- Greater(a,b)
            2. GreaterBC() |- Greater(b,c)
            3. ProceedingResults(1.,2.) |- and (Greater(a,b), Greater(b,c))
            4. 3., GreaterTransitive |- impl ( and (Greater(a,b), Greater(b,c)), Greater(a,c) )
            5. 4., ModusPonens |- Greater(a,c)
            6. ProceedingResults(5.,1.) |- and (Greater(a,c), Greater(a,b))
            7. 6., ExistsByExample(and(Greater(a,c), Greater(a,b))) |- ex x ( and (Greater(x,y), Greater(x,z)) )
			8. |- qed
        }"""
printfn "%O" res

let res1 = run proofArgument """1. GreaterAB |- Greater(a,b)"""
printfn "%O" res1

let res2 = run justification """GreaterAB |- Greater(a,b)"""
printfn "%O" res2
