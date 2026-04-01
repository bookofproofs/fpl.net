/// This module contains classes used as Singletons during the interpretation
/// by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module FplInterpreter.Globals.Debug
open System
open System.IO
open FplInterpreterBasicTypes


type Recursion() =
    let mutable _recursionLevel = 0

    /// Recursion level
    member this.RecursionLevel = _recursionLevel

    /// Increment recursion level
    member this.RecursionInc() = 
        _recursionLevel <- _recursionLevel + 1

    /// Decrement recursion level
    member this.RecursionDec() = 
        _recursionLevel <- _recursionLevel - 1


let debugRec = Recursion()
type Debug =
    | Start
    | Stop

let debug (fv:FplGenericNode) (debugMode:Debug) =
    let bars n = String.replicate n "| "
    let rec getPath (fv1:FplGenericNode) =
        match fv1.Parent with 
        | Some parent -> $"{getPath parent} # {fv1.ShortName} {fv1.Type SignatureType.Name}"
        | None -> $"{fv1.ShortName}"
    let vars =
        fv.GetVariables()
        |> List.map (fun var -> $"{var.FplId}={var.Represent()}")
        |> String.concat ", "
    if TestSharedConfig.TestConfig.DebugMode then 
        let indent = bars (debugRec.RecursionLevel)
        let logLine =
            match debugMode with
            | Debug.Start ->
                debugRec.RecursionInc()
                $"Start:{indent}{getPath fv}:[{fv.Represent()}][{vars}]{Environment.NewLine}"
            | Debug.Stop ->
                debugRec.RecursionDec()
                $"Stop :{indent.Substring(2)}{getPath fv}:[{fv.Represent()}][{vars}]{Environment.NewLine}"
        let currDir = Directory.GetCurrentDirectory()
        File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine)


