open FParsec
open FplGrammar


let res1 = run translation """~tex: x "\Leftrightarrow" y """
printfn "%O" res1

let res = run ebnfTransl """"\neg(" x ")" | x "\Rightarrow" y """
printfn "%O" res

let res4 = run ebnfTransl """x "\Rightarrow" y """
printfn "%O" res4

let res4a = run ebnfTransl """x "\Leftrightarrow" y """
printfn "%O" res4a


let res2 = run ebnfTransl """"\neg(" x ")" | x "\Rightarrow" y """
printfn "%O" res2

let replaceWhiteSpace (input: string) =
    let whiteSpaceChars = [|' '; '\t'; '\n'|]
    input.Split(whiteSpaceChars)
        |> String.concat ""

let result = replaceWhiteSpace """He llo\tWorld
     _"""

printfn "%s" result // prints "Hello_World_"