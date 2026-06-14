/// This module contains all nodes of the symbol table used by the FplInterpreter
/// to interpret quantors

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterQuantors
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open FplInterpreterChecks
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables


[<AbstractClass>]
type FplGenericQuantor(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    override this.ShortName = PrimQuantor

    override this.Type signatureType =
        match signatureType with
        | SignatureType.Type ->
            let head = LiteralPred
            let paramT =
                this.GetVariables()
                |> List.filter (fun var -> box var :? FplVariable)
                |> List.map (fun var -> box var :?> FplVariable)
                |> List.filter (fun var -> not var.IsBound)
                |> List.map (fun var -> $"{var.Type SignatureType.Type}")
                |> String.concat ", "

            match paramT with
            | "" -> head
            | _ -> sprintf "%s(%s)" head paramT
        | _ ->
            let head =
                match this.Name with
                | PrimQuantorAll -> "∀"
                | PrimQuantorExists -> "∃"
                | PrimQuantorExistsN -> $"{this.FplId}".Replace("exn$1","∃!").Replace("exn$","∃!")
                | _ -> ""
            let boundVars =
                this.GetVariables()
                |> List.choose (function
                    | :? FplVariable as v when v.IsBound -> Some v
                    | _ -> None)
                |> List.groupBy (fun v -> v.Type SignatureType.Type)
                |> List.sortBy fst   // sort groups by type
                |> List.map (fun (typ, vars) ->
                    let names =
                        vars
                        |> List.sortBy (fun v -> v.FplId)   // sort names inside each group
                        |> List.map (fun v -> v.FplId)
                        |> String.concat ", "
                    $"{names}:{typ}")
                |> String.concat ", "

            let body =
                if this.ArgList.Count>0 then
                    this.ArgList[0].Type signatureType
                else
                    ""
            $"{head} {boundVars} " + "{" + body + "}"

    override this.CheckConsistency () = 
        base.CheckConsistency()
        this.GetVariables()
        |> List.map(fun var -> var :?> FplGenericVariable)
        |> List.filter(fun var -> not var.IsUsed)
        |> List.iter (fun var -> 
            var.ErrorOccurred <- emitVAR05diagnostics var.FplId var.StartPos var.EndPos
        )
        checkArgPred this (this.ArgList[0])
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this
    
    override this.Run() = 
        debug this Debug.Start
        this.ArgList[0].Run()
        this.SetDefaultValue()
        debug this Debug.Stop

type FplQuantorAll(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralAll

    override this.Name = PrimQuantorAll

    override this.Clone () =
            let ret = new FplQuantorAll((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

type FplQuantorExists(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralEx

    override this.Name = PrimQuantorExists

    override this.Clone () =
            let ret = new FplQuantorExists((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

type FplQuantorExistsN(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralExN
        this.Arity <- 1


    override this.Name = PrimQuantorExistsN

    override this.Clone () =
            let ret = new FplQuantorExistsN((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

