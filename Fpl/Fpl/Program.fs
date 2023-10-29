open FplGrammarCommons
open ErrRecovery
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = """    loc NotEqual(x,y) :=
        ~tex: x "\neq" y 
        ~eng: x "is unequal" y 
        ~ger: x "ist ungleich" y
        ~pol: x ( "nie równa się" | "nie równe" ) y
        ;
"""

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

printf "%s" (removeStrings input)