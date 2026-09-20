// This console "main" program is for test/debugging purposes only.
// It is not really needed because the necessary FPL modules are run 
// as an FPL Language Server (see FplLS C# Project in the same solution).
open FParsec
open Fpl1Parser.Grammar

let result = run (axiom .>> eof) """axiom ZeroIsNat {true}"""
printfn "%O" result
