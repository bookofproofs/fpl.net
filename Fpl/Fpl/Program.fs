open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions






let input = """definition function DoubleSuccessor postfix "''" (x: N) -> N
{
    returtttt
}"""
//let input = """PascalId()()"""


(*
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics
*)


let result = run (definition .>> eof) input
printf "%O" result

printf "\n--------------------------------\n"

