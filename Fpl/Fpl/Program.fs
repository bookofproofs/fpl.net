open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = """PascalId().PascalId()"""
//let input = """PascalId()()"""


(*
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics
*)


let result = run predicateWithQualification input
printf "%O" result

printf "\n--------------------------------\n"

