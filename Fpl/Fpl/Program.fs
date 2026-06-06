// This console "main" program is for test/debugging purposes only.
// It is not really needed because the necessary FPL modules are run 
// as an FPL Language Server (see FplLS C# Project in the same solution).
open FParsec
open FplParsing.Basic
open FplParsing.Combinators


let par = attempt ((predicateWithQualification .>> SW .>> keywordIs) .>>. (SW >>. variableType))
        
let result = run (par .>> eof) "y is M"
printfn "%O" result
