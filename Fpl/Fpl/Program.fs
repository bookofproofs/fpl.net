open FParsec
open FplGrammar


let result = run conditionFollowedByResultList """case Equal(x,0) :
                            self := Zero()
                        case Equal(x,1) :
                            self := Succ(Zero())
                        case Equal(x,2) :
                            self := Succ(Succ(Zero()))
    """
printfn "%O" result

