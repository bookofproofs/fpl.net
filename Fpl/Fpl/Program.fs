open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FParsec
open System.Text.RegularExpressions
open System.Collections.Generic




let input = """;"""

let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
let interpret = FplInterpreter.fplInterpreter input (System.Uri("file:///d%3A/Forschung/fpl.net/theories/Landau/Test.fpl")) fplLibUrl
printf "%A" interpret

printf "\n--------------------------------\n"
ad.PrintDiagnostics

printf "\n--------------------------------\n"


