open ErrRecovery
open FplParser
open FParsec


let input = "TestNamespace {
    theory {   
        ax
    }
    y
}"

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics

let i = "class AlgebraicStructure: obj
        {

            dec:
            myCarrierSet: tplSet
            myOps:+ Composition(myElem:* tplSetElem)
            ;
            
            AlgebraicStructure(x: tplSet, ops:+ Composition(args:* tplSetElem))
            {

                a: obj
                arg: index
				myOps := ops
                myCarrierSet := x

				assert 
                    and (
                        is (x,Set),
                        all arg args (is (arg, Set)),
                        all a (impl ( is(a,tplSetElem), In(a,myCarrierSet) ))
                    )

            }"
let r = run definitionClass i
printf "%O" r
