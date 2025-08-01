/// This modulde contains all side-effect functions necessary to emit diagnostics for the FPL language server.
module FplInterpreterDiagnosticsEmitter

open System.Collections.Generic
open System.Text.RegularExpressions
open FParsec
open ErrDiagnostics
open FplDelegates
open FplInterpreterTypes
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
let emitUnexpectedErrorDiagnostics errMsg =
    let diagnostic =
        {
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position("", 0, 1, 1)
            Diagnostic.EndPos = Position("", 0, 1, 1)
            Diagnostic.Code = GEN00 errMsg
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic(diagnostic)

let emitID001diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID001(fplValue.Type(SignatureType.Type), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitID014diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID014(fplValue.Type(SignatureType.Mixed), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitID015diagnostics name (self:FplValue) =
    let c = ID015 name
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = self.StartPos
            Diagnostic.EndPos = self.EndPos
            Diagnostic.Code = c
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitID016diagnostics name (self:FplValue) =
    let c = ID016 name
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = self.StartPos
            Diagnostic.EndPos = self.EndPos
            Diagnostic.Code = c
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR003diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = PR003(fplValue.Type(SignatureType.Mixed), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitVAR01diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR01 name
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitVAR03diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = VAR03(fplValue.Type(SignatureType.Mixed), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = Some "Remove this variable declaration or rename the variable." 
        }

    ad.AddDiagnostic diagnostic

let emitVAR03diagnosticsForCorollaryOrProofVariable (fplValue: FplValue) =
    match fplValue.FplBlockType with 
    | FplBlockType.Proof 
    | FplBlockType.Corollary ->
        fplValue.Scope
        |> Seq.filter (fun kv -> FplValue.IsVariable(kv.Value))
        |> Seq.iter (fun kv -> 
            let res = FplValue.VariableInBlockScopeByName (kv.Value) (kv.Value.Type(SignatureType.Mixed)) false
            match res with
            | ScopeSearchResult.Found conflict -> emitVAR03diagnostics kv.Value conflict
            | _ -> ()
        )
    | _ -> ()

let getVAR04diagnostic (fv:FplValue) name = 
    { 
        Diagnostic.Uri = ad.CurrentUri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fv.StartPos
        Diagnostic.EndPos = fv.EndPos
        Diagnostic.Code = VAR04 name
        Diagnostic.Alternatives = None 
    }

let emitVAR04diagnostics (fv:FplValue) =
    fv.GetVariables()
    |> List.filter(fun var -> var.AuxiliaryInfo = 0)
    |> List.map (fun var -> 
        ad.AddDiagnostic (getVAR04diagnostic var var.FplId)
    )
    |> ignore

let emitVAR05diagnostics (fv:FplValue) =
    fv.GetVariables()
    |> List.filter(fun var -> var.AuxiliaryInfo = 0)
    |> List.map (fun var -> 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = var.StartPos
                Diagnostic.EndPos = var.EndPos
                Diagnostic.Code = VAR05 var.FplId
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic
    )
    |> ignore

let emitVAR06iagnostic name parentClass pos = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos
            Diagnostic.EndPos = pos
            Diagnostic.Code = VAR06(name,parentClass)
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitID000Diagnostics astType =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position("", 0, 1, 1)
            Diagnostic.EndPos = Position("", 0, 1, 1)
            Diagnostic.Code = ID000 astType
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitID002diagnostics (fplValue: FplValue) incorrectBlockType =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID002(fplValue.Type(SignatureType.Type), incorrectBlockType)
            Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
        }

    ad.AddDiagnostic diagnostic

let emitID005diagnostics (fplValue: FplValue) incorrectBlockType =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID005(fplValue.Type(SignatureType.Type), incorrectBlockType)
            Diagnostic.Alternatives =
                Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary), a conjecture, or an axiom." 
         }

    ad.AddDiagnostic diagnostic

let emitID003diagnostics (fplValue: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID003 (fplValue.Type(SignatureType.Type))
            Diagnostic.Alternatives = 
                Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
        }

    ad.AddDiagnostic diagnostic

let emitID006diagnostics (fplValue: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID006 (fplValue.Type(SignatureType.Type))
            Diagnostic.Alternatives =
                Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary), a conjecture, or an axiom." 
        }

    ad.AddDiagnostic diagnostic

let emitID004diagnostics (fplValue: FplValue) listOfCandidates =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID004(fplValue.Type(SignatureType.Type), listOfCandidates)
            Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
        }
    ad.AddDiagnostic diagnostic

let emitID007diagnostics (fplValue: FplValue) listOfCandidates =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = ID007(fplValue.Type(SignatureType.Type), listOfCandidates)
            Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
        }
    ad.AddDiagnostic diagnostic

let checkVAR00Diagnostics numberOfVariadicVars startPos endPos =
    if numberOfVariadicVars > 1 then
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = startPos
                Diagnostic.EndPos = endPos
                Diagnostic.Code = VAR00
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let checkID008Diagnostics (fv: FplValue) pos1 pos2 =
    let nameStart = fv.FplId
    let className = fv.Parent.Value.FplId

    if nameStart <> className then
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = ID008(nameStart, className) // misspelled constructor name
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

/// Given the class node fplValue and the identifier `name`, this function checks the 
/// semantical consistency of a parent class with this name, covering ID009, ID010 and ID011 diagnostics.
/// It will return None or Some reference to the parent class node with this name, if it could be found.
let checkID009_ID010_ID011_Diagnostics (st: SymbolTable) (fplValue: FplValue) name pos1 pos2 =
    let rightContext = st.EvalPath()
    let classInheritanceChain = findClassInheritanceChain fplValue name

    if rightContext.EndsWith("InheritedClassType.PredicateIdentifier") then
        if fplValue.Type(SignatureType.Type) = name then
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID009 name // circular base dependency
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic
            None
        else
            match classInheritanceChain with
            | Some chain ->
                let diagnostic =
                    { 
                        Diagnostic.Uri = ad.CurrentUri
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = pos1
                        Diagnostic.EndPos = pos2
                        Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                        Diagnostic.Alternatives = None 
                    }
                ad.AddDiagnostic diagnostic
                None
            | _ ->
                match FplValue.InScopeOfParent (fplValue) name with
                | ScopeSearchResult.Found classCandidate ->
                    let mutable duplicateInheritanceChainFound = false

                    fplValue.ArgList
                    |> Seq.iter (fun child ->
                        let childType = child.Type(SignatureType.Type)
                        let classInheritanceChain = findClassInheritanceChain classCandidate childType

                        match classInheritanceChain with
                        | Some chain ->
                            let diagnostic =
                                { 
                                    Diagnostic.Uri = ad.CurrentUri
                                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                                    Diagnostic.Severity = DiagnosticSeverity.Error
                                    Diagnostic.StartPos = pos1
                                    Diagnostic.EndPos = pos2
                                    Diagnostic.Code = ID011(childType, chain) // inheritance chain duplicate
                                    Diagnostic.Alternatives = None 
                                }
                            ad.AddDiagnostic diagnostic
                            duplicateInheritanceChainFound <- true
                        | _ -> ())

                    if not duplicateInheritanceChainFound then
                        Some classCandidate
                    else
                        None
                | _ ->
                    let diagnostic =
                        { 
                            Diagnostic.Uri = ad.CurrentUri
                            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                            Diagnostic.Severity = DiagnosticSeverity.Error
                            Diagnostic.StartPos = pos1
                            Diagnostic.EndPos = pos2
                            Diagnostic.Code = ID010 name // class not found
                            Diagnostic.Alternatives = None 
                        }
                    ad.AddDiagnostic diagnostic
                    None

    elif rightContext.EndsWith("InheritedClassType.ObjectType") then
        match classInheritanceChain with
        | Some chain ->
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic
            None
        | _ ->
            let mutable duplicateInheritanceChainFound = false

            fplValue.ArgList
            |> Seq.iter (fun child ->
                let classInheritanceChain = findClassInheritanceChain child name

                match classInheritanceChain with
                | Some chain ->
                    let diagnostic =
                        { 
                            Diagnostic.Uri = ad.CurrentUri
                            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                            Diagnostic.Severity = DiagnosticSeverity.Error
                            Diagnostic.StartPos = pos1
                            Diagnostic.EndPos = pos2
                            Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                            Diagnostic.Alternatives = None 
                        }
                    ad.AddDiagnostic diagnostic
                    duplicateInheritanceChainFound <- true
                | _ -> ())

            if not duplicateInheritanceChainFound then
                let obJ = FplValue.CreateFplValue((pos1, pos2),FplBlockType.IntrinsicObject,fplValue)
                Some obJ
            else
                None
    else    
        None

let emitID020Diagnostics identifier pos1 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos1
            Diagnostic.Code = ID020 identifier
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitID021Diagnostics identifier pos1 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos1
            Diagnostic.Code = ID021 identifier
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let checkID012Diagnostics (st: SymbolTable) (parentConstructorCall: FplValue) identifier (pos1: Position) pos2 =
    let context = st.EvalPath()

    if
        context.EndsWith("ParentConstructorCall.InheritedClassType.PredicateIdentifier")
        || context.EndsWith("ParentConstructorCall.InheritedClassType.ObjectType")
    then
        let stmt = parentConstructorCall.Parent.Value
        let constructor = stmt.Parent.Value
        let classOfConstructor = constructor.Parent.Value
        let mutable foundInheritanceClass = false

        let candidates =
            classOfConstructor.ArgList
            |> Seq.map (fun inheritanceClass ->
                let inheritanceClassType = inheritanceClass.Type(SignatureType.Type)
                if inheritanceClassType = identifier then
                    foundInheritanceClass <- true
                inheritanceClassType)
            |> String.concat ", "

        if not foundInheritanceClass then
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID012(identifier, candidates) // call of parent class does not match the class id
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic

let checkID018Diagnostics (st: SymbolTable) (fv:FplValue) (identifier:string) pos1 pos2 =
    let matchReprId (fv1:FplValue) (identifier:string) = 
        let regex = Regex(fv1.ReprId)
        regex.IsMatch(identifier)
        
    let candidatesFromScope =
        st.Root.Scope
        |> Seq.map (fun theory ->
            theory.Value.Scope
            |> Seq.filter (fun kvp -> kvp.Value.FplBlockType = FplBlockType.Extension)
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ext -> 
                if matchReprId ext identifier && not (fv.Scope.ContainsKey(fv.FplId)) then 
                    // assign the reference FplValue fv only the first found match 
                    // even, if there are multiple extensions that would match it 
                    // (thus, the additional check for Scope.ContainsKey...)
                    fv.Scope.Add(fv.FplId, ext)
                    true
                else
                    false
            )
        )
        |> Seq.concat
        |> Seq.toList

    let candidates = 
        let parentExtension = filterTreePathByBlockType fv FplBlockType.Extension
        match parentExtension with 
        | Some ext -> 
            if matchReprId ext identifier then
                // if fv is inside an extension block, we add this block to the candidates
                // so we can match patterns inside this extension block's definition referring to 
                // its own pattern even if it is not yet fully parsed and analysed
                candidatesFromScope @ [ext]
            else 
                candidatesFromScope
        | _ -> candidatesFromScope

    if candidates.Length = 0 then 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = ID018 identifier
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic
     


let checkID019Diagnostics (st: SymbolTable) (name:string) pos1 pos2 =
    if name.StartsWith("@") then 
        match searchExtensionByName st.Root name with
        | ScopeSearchResult.NotFound ->          
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID019 name
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic
        | _ -> ()
    
let emitID013Diagnostics (fv: FplValue) pos1 pos2 =
    let d = Delegates()

    try
        d.CallExternalDelegate(fv.FplId.Substring(4), fv.ArgList |> Seq.toList)
    with ex ->
        if ex.Message.StartsWith("OK:") then
            let result = ex.Message.Substring(3)
            fv.ReprId <- result
            ""
        else
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID013 ex.Message
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic
            ""

let emitID017Diagnostics name (candidates:FplValue list) pos1 pos2 =
    let candidatesName =
        candidates
        |> Seq.map (fun fv -> fv.QualifiedName)
        |> String.concat ", "

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID017(name, candidatesName) 
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR004Diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.StartPos
            Diagnostic.EndPos = fplValue.EndPos
            Diagnostic.Code = PR004(fplValue.Type(SignatureType.Type), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitPR005Diagnostics (fv:FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fv.StartPos
            Diagnostic.EndPos = fv.EndPos
            Diagnostic.Code = PR005 (fv.Type(SignatureType.Mixed)) // argument reference not defined
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitSIG00Diagnostics (fplValue: FplValue) pos1 pos2 =
    let detailed (exprType: FixType) expectedArity actualArity pos1 pos2 =
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = SIG00(exprType.Type, actualArity)
                Diagnostic.Alternatives = Some $"Arity of {expectedArity} expected." 
            }
        ad.AddDiagnostic diagnostic

    match fplValue.ExpressionType with
    | FixType.Infix _ when fplValue.Arity <> 2 ->
        let fplValueType = fplValue.Type(SignatureType.Type)
        if fplValueType.Length > 2
            && (fplValueType.Substring(5).StartsWith("+") 
                || fplValueType.Substring(5).StartsWith("*")
            )
        then
            () // avoid false positives for variadic variables
        else
            detailed fplValue.ExpressionType 2 fplValue.Arity pos1 pos2
    | FixType.Prefix _ when fplValue.Arity <> 1 -> detailed fplValue.ExpressionType 1 fplValue.Arity pos1 pos2
    | FixType.Postfix _ when fplValue.Arity <> 1 -> detailed fplValue.ExpressionType 1 fplValue.Arity pos1 pos2
    | _ -> ()

let emitSIG01Diagnostics (st: SymbolTable) (fv: FplValue) pos1 pos2 =
    if fv.FplBlockType = FplBlockType.Reference then
        // collect candidates to match this reference from all theories and
        // add them to fplValues's scope
        let expressionId = fv.FplId

        st.Root.Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.iter (fun block ->
                if expressionId = block.FplId then
                    let blockType = block.Type(SignatureType.Mixed)
                    fv.Scope.Add(blockType, block)
                    fv.TypeId <- block.TypeId
                else
                    let blockType = block.Type(SignatureType.Mixed)
                    match block.ExpressionType with
                    | FixType.Prefix symbol
                    | FixType.Symbol symbol
                    | FixType.Postfix symbol ->
                        if expressionId = symbol then
                            fv.Scope.Add(blockType, block)
                            fv.TypeId <- block.TypeId
                    | FixType.Infix(symbol, precedence) ->
                        if expressionId = symbol then
                            fv.Scope.Add(blockType, block)
                            fv.TypeId <- block.TypeId
                    | _ -> ()))

        if fv.Scope.Count = 0 then
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG01 expressionId
                    Diagnostic.Alternatives = Some "Declare a functional term, predicate, or class with this symbol." 
                }
            ad.AddDiagnostic diagnostic

let emitSIG02Diagnostics (st: SymbolTable) (fplValue: FplValue) pos1 pos2 =
    let precedences = Dictionary<int, FplValue>()

    match fplValue.ExpressionType with
    | FixType.Infix(symbol, precedence) ->
        let precedenceWasAlreadyThere precedence fv =
            if not (precedences.ContainsKey(precedence)) then
                precedences.Add(precedence, fv)
                false
            else
                true

        st.Root.Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv1 -> kv1.Value)
            |> Seq.iter (fun block ->
                match block.ExpressionType with
                | FixType.Infix(_, precedence) -> precedenceWasAlreadyThere precedence block |> ignore
                | _ -> ()))

        if precedences.ContainsKey(precedence) then
            let conflict = precedences[precedence].QualifiedStartPos 

            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Information
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG02(symbol, precedence, conflict)
                    Diagnostic.Alternatives = Some "Consider disambiguating the precedence to avoid unexpected results." 
                }
            ad.AddDiagnostic diagnostic
    | _ -> ()

let emitSIG04DiagnosticsForTypes identifier pos1 pos2 =
    let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = SIG04(identifier, 0, [""])
                Diagnostic.Alternatives = None 
            }
    ad.AddDiagnostic diagnostic

let emitSIG03Diagnostics (retType:FplValue) (mapType:FplValue) = 
    match matchWithMapping retType mapType with
    | Some errMsg -> 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = retType.StartPos
                Diagnostic.EndPos = retType.EndPos
                Diagnostic.Code = SIG03(errMsg, mapType.Type(SignatureType.Type))
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic
    | _ -> ()

let checkSIG04Diagnostics (calling:FplValue) (candidates: FplValue list) = 
    match checkCandidates calling candidates [] with
    | (Some candidate,_) -> Some candidate // no error occured
    | (None, errList) -> 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = calling.StartPos
                Diagnostic.EndPos = calling.EndPos
                Diagnostic.Code = SIG04(calling.Type(SignatureType.Mixed), candidates.Length, errList)
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic
        None

let checkSIG05Diagnostics (assignee:FplValue) (toBeAssignedValue: FplValue) = 
    let valueOpt = toBeAssignedValue.GetArgument
    match valueOpt with
    | Some value when value.FplBlockType = FplBlockType.Class ->
        let chainOpt = findClassInheritanceChain value assignee.TypeId
        match chainOpt with
        | None ->
            // issue SIG05 diagnostics if either no inheritance chain found 
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = toBeAssignedValue.StartPos
                    Diagnostic.EndPos = toBeAssignedValue.EndPos
                    Diagnostic.Code = SIG05(assignee.Type(SignatureType.Type), value.Type(SignatureType.Type))
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic
        | _ -> () // inheritance chain found (no SIG05 diagnostics)
    | Some value when value.FplBlockType = FplBlockType.Constructor ->
        // find a class inheritance chain for the constructor's class (which is stored in its parent value)
        let chainOpt = findClassInheritanceChain value.Parent.Value assignee.TypeId
        match chainOpt with
        | None ->
            // issue SIG05 diagnostics if either no inheritance chain found 
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = toBeAssignedValue.StartPos
                    Diagnostic.EndPos = toBeAssignedValue.EndPos
                    Diagnostic.Code = SIG05(assignee.Type(SignatureType.Type), value.Type(SignatureType.Type))
                    Diagnostic.Alternatives = None 
                }
            ad.AddDiagnostic diagnostic
        | _ -> () // inheritance chain found (no SIG05 diagnostics)
    | Some value when assignee.TypeId <> value.TypeId ->
        // Issue SIG05 diagnostics if value is not a constructor and not a class and still the types are not the same 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = toBeAssignedValue.StartPos
                Diagnostic.EndPos = toBeAssignedValue.EndPos
                Diagnostic.Code = SIG05(assignee.Type(SignatureType.Type), value.Type(SignatureType.Type))
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic
    | Some value when assignee.TypeId = value.TypeId ->
        // Issue no SIG05 diagnostics if value is not a constructor and not a class but the types match
        ()
    | None ->
        // Issue SIG05 diagnostics if there is (for some reason) no value of the toBeAssignedValue 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = toBeAssignedValue.StartPos
                Diagnostic.EndPos = toBeAssignedValue.EndPos
                Diagnostic.Code = SIG05(assignee.Type(SignatureType.Type), toBeAssignedValue.Type(SignatureType.Type))
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let rec blocktIsProof (fplValue: FplValue) =
    if FplValue.IsProof(fplValue) then
        true
    else
        match fplValue.Parent with
        | Some parent ->
            if FplValue.IsTheory(parent) then
                false
            elif FplValue.IsFplBlock(parent) then
                FplValue.IsProof(parent)
            else
                blocktIsProof parent
        | None -> false

let emitPR000Diagnostics (fplValue: FplValue) =
    if not (blocktIsProof fplValue) then
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = fplValue.StartPos
                Diagnostic.EndPos = fplValue.EndPos
                Diagnostic.Code = PR000 (fplValue.Type(SignatureType.Type))
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let emitPR001Diagnostics (fplValue: FplValue) pos1 pos2 =
    if not (blocktIsProof fplValue) then
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = PR001
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let emitPR002Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR002
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitLG000orLG001Diagnostics (fplValue: FplValue) typeOfPredicate =
    let filterByErrorCode (input: Diagnostics) errCode =
        input.Collection |> List.filter (fun d -> d.Code.Code = errCode)

    let diags = Diagnostics()

    let emitLG000Diagnostics (arg: FplValue) =
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = arg.StartPos
                Diagnostic.EndPos = arg.EndPos
                Diagnostic.Code = LG000(typeOfPredicate, arg.Type(SignatureType.Name))
                Diagnostic.Alternatives = None 
            }
        diags.AddDiagnostic diagnostic

    let emitLG001Diagnostics pos1 pos2 (arg: FplValue) =
        let argType = arg.Type(SignatureType.Type)
        let argName = arg.Type(SignatureType.Name)
        let whatWeGot = 
            if argType = argName then
                "undef"
            else
                argType

        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = LG001(typeOfPredicate, argName, whatWeGot)
                Diagnostic.Alternatives = None 
            }
        diags.AddDiagnostic diagnostic

    fplValue.ArgList
    |> Seq.iter (fun argument ->
        let repr = argument.Type(SignatureType.Repr)
        match repr with
        | "true"
        | "false" -> ()
        | "undetermined" -> emitLG000Diagnostics argument 
        | _ -> emitLG001Diagnostics argument.StartPos argument.EndPos argument
    )

    let code = LG000("", "")
    let numbLG000 = filterByErrorCode diags code.Code

    if numbLG000.Length = fplValue.ArgList.Count then
        () // we have no reason to emit any diagnostics since the are as many undetermined predicates as arguments
    else
        diags.Collection
        |> List.iter (fun d -> ad.AddDiagnostic d)
