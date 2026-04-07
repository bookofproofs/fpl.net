/// This module contains all symbol table nodes used by the FplInterpreter
/// to model the assert statement.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterAssertStmt
open FplPrimitives
open FplGrammarTypes
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.Heap
open FplInterpreterIntrinsicTypes


type FplAssertion(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimAssertion
    override this.ShortName = LiteralAss

    member this.InferrableExpression =
        let validityReason, exprStr = 
            let exprOpt = this.ArgList |> Seq.tryLast
            match exprOpt with
            | Some expr -> ValidityReason.IsAxiomAssertion expr, expr.Type SignatureType.Name
            | _ -> ValidityReason.Error, "" // fallback if axiom node is empty

        { ValidStatement.Node = this
          ValidStatement.ValidityReason = validityReason
          ValidStatement.StatementExpression = exprStr }

    interface IInferrable with
        member this.InferrableExpression
            with get () = this.InferrableExpression

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        debug this Debug.Start
        heap.ValidStmtStore.RegisterExpression this |> ignore
        debug this Debug.Stop

    override this.RunOrder = None

