open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions






let input = """-(x + -y)'"""
//let input = """PascalId()()"""


(*
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics
*)


let result = run (predicate .>> eof) input
printf "%O" result

printf "\n--------------------------------\n"

