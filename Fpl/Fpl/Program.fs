open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec



let p = pchar 'a'

let input = "abba"

let q = positions p

let result = run q input
printf "%O" result

type UserState = { mutable Depth: int }

let expr, exprRef = createParserForwardedToRef()

let p1 = 
    (pstring "a")
    .>> (fun stream -> stream.UserState.Depth <- stream.UserState.Depth + 1; Reply())
<|>
    (fun stream -> stream.UserState.Depth <- stream.UserState.Depth - 1; Reply())
    >>. pstring "b"
<|>
    pstring "c"

do exprRef := p1

let initialState = { Depth = 0 }
let result1 = runParserOnString expr initialState "source" "accb"
printf "%O" result1
