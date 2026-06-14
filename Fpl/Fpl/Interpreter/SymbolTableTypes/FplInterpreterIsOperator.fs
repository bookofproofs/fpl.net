/// This module contains all symbol table nodes used by the FplInterpreter
/// to model the is operator.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterIsOperator
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open FplInterpreterReferences
open FplInterpreterIntrinsicTypes
open FplInterpreterFplTypeMatching


/// Implements the semantics of the FPL is operator.
type FplIsOperator(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIs

    override this.Name = PrimIsOperator
    override this.ShortName = LiteralIs

    override this.Clone () =
        let ret = new FplIsOperator((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getNotationTwoArgs this "is" signatureType LiteralPred
        
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        // FPL truth-table
        match operand with 
        | :? FplReference as op ->
            match FplTypeMatcher.MatchPwA [operand] [typeOfOperand] with
            | Some errMsg -> 
                let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
                this.SetValue newValue
            | None -> 
                let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
                this.SetValue newValue
        | _ -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        
        StaticDebug.Debug(this,Debug.Stop)

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this
