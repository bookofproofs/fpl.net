open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions






let input = """(0+1)"""



let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

let result1 = run (predicate .>> eof) input
printf "%O" result1

printf "\n--------------------------------\n"

