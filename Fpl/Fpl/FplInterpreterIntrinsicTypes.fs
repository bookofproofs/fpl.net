/// This module contains all intrinsic types used by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterIntrinsicTypes
open System
open FplGrammarTypes
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterSTEmbedding
open FplInterpreterUtils
open FplInterpreterBasicTypes

type FplIntrinsicTpl(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do
        this.TypeId <- name
        this.FplId <- name

    override this.Name = PrimIntrinsicTpl
    override this.ShortName = LiteralTpl

    override this.Clone () =
        let ret = new FplIntrinsicTpl(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        match this.RefersTo with
        | Some fv -> 
            // if the template was used, its representation is the 
            // type signature of how it was used
            fv.Type SignatureType.Type  
        | None -> 
            // otherwise, the representation defaults to the user-defined name of the template
            getFplHead this signatureType

    override this.Run() = 
        // A template has no value
        ()

    /// Sets a template usage and issues a specific diagnostics if a type conflict occurs.
    /// The caller decides with specific diagnostics to issue. If the diagnostics is unhandled, 
    /// GEN00 diagnostic will be issued as default.
    member this.TrySetTemplateUsage (fv:FplGenericNode) diagnostic = 
        match this.RefersTo with 
        | None -> 
            this.RefersTo <- Some fv // if this template was not used, use it
        | Some templateUsage ->
            match templateUsage.UltimateBlockNode, fv.UltimateBlockNode with
            | Some block1, Some block2 when Object.ReferenceEquals(block1, block2) ->
                // test only usages within the scope of the same UltimateBlockNode
                // (since all other usages are not usages are out ouf scope)
                // otherwise calculate the type signatures of the very first usage  
                let firstUsage = templateUsage.Type SignatureType.Type
                // and the current one
                let currentUsage = fv.Type SignatureType.Type
                // compare both
                match currentUsage with 
                | LiteralUndef -> () // Usage "undef" is always accepted
                | _  when firstUsage <> currentUsage ->
                    // issue diagnostics, if inconsistent usage
                    match diagnostic with 
                    | "SIG12" -> this.ErrorOccurred <- emitSIG12diagnostics this.FplId firstUsage currentUsage (fv.QualifiedStartPos) templateUsage.StartPos templateUsage.EndPos
                    | "SIG13" -> this.ErrorOccurred <- emitSIG13diagnostics this.FplId currentUsage firstUsage (templateUsage.QualifiedStartPos) fv.StartPos fv.EndPos
                    | _ -> emitUnexpectedErrorDiagnostics $"Unhandled diagnostic `{diagnostic}` in FplIntrinsicTpl.TrySetTemplateUsage."
                | _ -> () // equal usage is accepted
            | _, _ -> () // equal usage is accepted

    override this.EmbedInSymbolTable _ = tryAddTemplateToParent this 

    override this.RunOrder = None

type FplIntrinsicUndef(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.TypeId <- LiteralUndef
        this.FplId <- LiteralUndef

    override this.Name = PrimIntrinsicUndef
    override this.ShortName = LiteralUndef

    override this.Clone () =
        let ret = new FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent() = // done
        LiteralUndef 

    override this.Run() = 
        // FplIntrinsicUndef is a value of not defined FPL objects and has no value on its own
        ()

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// Implements the semantics of an FPL predicate prime predicate that is intrinsic.
/// It serves as a value for everything in FPL that is "predicative in nature". 
/// These can be predicates, theorem-like-statements, proofs or predicative expressions. 
/// The semantical representation can have one of two values in FPL: "true" and "false". 
type FplIntrinsicPred(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.FplId <- LiteralTrue
        this.TypeId <- LiteralPred

    override this.Name = PrimIntrinsicPred
    override this.ShortName = LiteralPred

    override this.Clone () =
        let ret = new FplIntrinsicPred((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = getFplHead this signatureType
                    
    override this.Represent() = // done
        this.FplId 

    override this.Run() = 
        // FplIntrinsicPred is a value of predicate closures and has no value on its own
        ()

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

type FplIntrinsicInd(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do 
        this.TypeId <- LiteralInd
        this.FplId <- LiteralInd

    override this.Name = PrimIntrinsicInd
    override this.ShortName = LiteralInd

    override this.Clone () =
        let ret = new FplIntrinsicInd((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent() = // done
        match this.FplId with
        | LiteralInd -> $"dec {this.TypeId}"
        | _ -> this.FplId

    override this.Run() = 
        // no Run needed for FplIntrinsicInd
        ()

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

type FplInstance(typeId:string, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do
        this.FplId <- LiteralObj
        this.TypeId <- typeId
 
    override this.Name = PrimInstanceL
    override this.ShortName = PrimInstance

    override this.Clone () =
        let ret = new FplInstance(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        //match signatureType with 
        //| SignatureType.Type -> this.TypeId
        //| _ -> this.FplId
        let head = getFplHead this signatureType 
        head

    override this.Represent() = // done
        this.FplId

    override this.Run() = 
        // run is not neccessary, since this node is are never referenced in the FPL syntax
        // Instead, we use them internally as default value of FplGenericHasValue
        // FplInstance is a value representation and has no value on its own
        ()

    override this.EmbedInSymbolTable _ = 
        //// the embedding is not neccessary, since this node is are never referenced in the FPL syntax
        //// Instead, we use them internally as default value of FplGenericHasValue
        //() 
        addExpressionToParentArgList this 

    override this.RunOrder = None
