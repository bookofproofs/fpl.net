open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions






let input = """uses Fpl.Commons

def class Set: obj
{
    intr
}

def pred In infix "in" (x,y: Set)
{
    intr
}

def pred IsEmpty(x: Set)
{
    all y in Set
    (
        not In(y, x) 
    )
}
;"""



let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

let result1 = run (ast .>> eof) input
printf "%O" result1

printf "\n--------------------------------\n"

