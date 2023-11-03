open FplGrammarCommons
open ErrRecovery
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = """Fpl. {}

dec
selfs

dec
field

"""

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

