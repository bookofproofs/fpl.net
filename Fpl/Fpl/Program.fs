open FplGrammarTypes
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open FParsec


let input = """		input	"TestNamespace {
:ext T : /d/ :end     
theory {
cl S : obj {    // control error
}
}"	string
"""
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics



let tryKeyword s =
    let result = run variableX s
    match result with
    | Success(a,b,c) ->
        true
    | Failure(a,b,c) ->
        false   
