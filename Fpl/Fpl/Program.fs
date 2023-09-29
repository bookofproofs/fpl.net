open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = """~pol: x ( "nie równa się" | "nie równe" ) y"""
//let input = """~ger: x "ist ungleich" y"""

let result = run translation input
printf "%O" result