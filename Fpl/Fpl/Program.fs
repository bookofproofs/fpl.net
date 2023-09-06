open FParsec
open FplGrammar


let res1 = run casesStatement """	cases
            (
                case Equal(n,0):
                    return m.NeutralElem()
                else:
                    return
                        op(
                            y,
                            Exp( m(y,op), y, Sub(n,1))
                          )
            )"""
printfn "%O" res1

let res = run inlineComment """// foo bar 
"""
printfn "%O" res



let res2 = run primePredicate """@@self"""
printfn "%O" res2
