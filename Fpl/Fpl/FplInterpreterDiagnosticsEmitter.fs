module FplInterpreterDiagnosticsEmitter

open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplDelegates
open FplParser
open FplInterpreterTypes

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
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID001(fplValue.Type(true), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitID014diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID014(fplValue.Type(false), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitPR003diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = PR003(fplValue.Type(false), conflict.QualifiedStartPos)
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
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = VAR03(fplValue.Type(false), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = Some "Remove this variable declaration or rename the variable." 
        }

    ad.AddDiagnostic diagnostic

let emitVAR03diagnosticsForCorollaryOrProofVariable (fplValue: FplValue) =
    match fplValue.BlockType with 
    | FplValueType.Proof 
    | FplValueType.Corollary ->
        fplValue.Scope
        |> Seq.filter (fun kv -> FplValue.IsVariable(kv.Value))
        |> Seq.iter (fun kv -> 
            let res = FplValue.VariableInBlockScopeByName (kv.Value) (kv.Value.Type(false))

            match res with
            | ScopeSearchResult.Found conflict -> emitVAR03diagnostics kv.Value conflict
            | _ -> ()
        )
    | _ -> ()

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
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID002(fplValue.Type(true), incorrectBlockType)
            Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
        }

    ad.AddDiagnostic diagnostic

let emitID005diagnostics (fplValue: FplValue) incorrectBlockType =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID005(fplValue.Type(true), incorrectBlockType)
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
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID003 (fplValue.Type(true))
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
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID006 (fplValue.Type(true))
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
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID004(fplValue.Type(true), listOfCandidates)
            Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
        }
    ad.AddDiagnostic diagnostic

let emitID007diagnostics (fplValue: FplValue) listOfCandidates =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = ID007(fplValue.Type(true), listOfCandidates)
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

let checkID008Diagnostics (fplValue: FplValue) pos1 pos2 =
    if FplValue.IsConstructor(fplValue) && fplValue.Type(true).Length = 1 then
        let nameStart = fplValue.FplId
        let className = fplValue.Parent.Value.Type(true)

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

let checkID009_ID010_ID011_Diagnostics (st: SymbolTable) (fplValue: FplValue) name pos1 pos2 =
    let rec findPath (root: FplValue) (candidateName: string) =
        if root.Type(true) = candidateName then
            Some(root.Type(true))
        else
            root.ValueList
            |> Seq.collect (fun child ->
                match findPath child candidateName with
                | Some path -> [ root.Type(true) + ":" + path ]
                | None -> [])
            |> Seq.tryLast

    let rightContext = st.EvalPath()
    let classInheritanceChain = findPath fplValue name

    if rightContext.EndsWith("InheritedClassType.PredicateIdentifier") then
        if fplValue.Type(true) = name then
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
            | _ ->
                match FplValue.InScopeOfParent (fplValue) name with
                | ScopeSearchResult.Found classCandidate ->
                    let mutable duplicateInheritanceChainFound = false

                    fplValue.ValueList
                    |> Seq.iter (fun child ->
                        let classInheritanceChain = findPath classCandidate (child.Type(true))

                        match classInheritanceChain with
                        | Some chain ->
                            let diagnostic =
                                { 
                                    Diagnostic.Uri = ad.CurrentUri
                                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                                    Diagnostic.Severity = DiagnosticSeverity.Error
                                    Diagnostic.StartPos = pos1
                                    Diagnostic.EndPos = pos2
                                    Diagnostic.Code = ID011(child.Type(true), chain) // inheritance chain duplicate
                                    Diagnostic.Alternatives = None 
                                }
                            ad.AddDiagnostic diagnostic
                            duplicateInheritanceChainFound <- true
                        | _ -> ())

                    if not duplicateInheritanceChainFound then
                        fplValue.ValueList.Add classCandidate
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
        | _ ->
            let mutable duplicateInheritanceChainFound = false

            fplValue.ValueList
            |> Seq.iter (fun child ->
                let classInheritanceChain = findPath child name

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
                let obj = FplValue.CreateObject((pos1, pos2))
                fplValue.ValueList.Add obj

let checkID012Diagnostics (st: SymbolTable) (parentConstructorCall: FplValue) identifier (pos1: Position) pos2 =
    let context = st.EvalPath()

    if
        context.EndsWith("ParentConstructorCall.InheritedClassType.PredicateIdentifier")
        || context.EndsWith("ParentConstructorCall.InheritedClassType.ObjectType")
    then
        let constructor = parentConstructorCall.Parent.Value
        constructor.ValueList.Add(parentConstructorCall)
        let classOfConstructor = constructor.Parent.Value
        let mutable foundInheritanceClass = false

        let candidates =
            classOfConstructor.ValueList
            |> Seq.map (fun inheritanceClass ->
                if inheritanceClass.Type(true) = identifier then
                    foundInheritanceClass <- true

                inheritanceClass.Type(true))
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

let emitID013Diagnostics (fplValue: FplValue) pos1 pos2 =
    let d = Delegates()

    try
        d.CallExternalDelegate(fplValue.FplId.Substring(4), fplValue.ValueList |> Seq.toList)
    with ex ->
        if ex.Message.StartsWith("OK:") then
            match ex.Message.Substring(3) with
            | _ -> () // todo
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


let emitPR004Diagnostics (fplValue: FplValue) (conflict: FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = PR004(fplValue.Type(true), conflict.QualifiedStartPos)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitPR005Diagnostics (fv:FplValue) =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fv.NameStartPos
            Diagnostic.EndPos = fv.NameEndPos
            Diagnostic.Code = PR005 (fv.Type(false)) // argument reference not defined
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
        if
            fplValue.Type(true).Length > 2
            && (fplValue.Type(true).Substring(2).StartsWith("+") || fplValue.Type(true).Substring(2).StartsWith("*"))
        then
            () // avoid false positives for variadic variables
        else
            detailed fplValue.ExpressionType 2 fplValue.Arity pos1 pos2
    | FixType.Prefix _ when fplValue.Arity <> 1 -> detailed fplValue.ExpressionType 1 fplValue.Arity pos1 pos2
    | FixType.Postfix _ when fplValue.Arity <> 1 -> detailed fplValue.ExpressionType 1 fplValue.Arity pos1 pos2
    | _ -> ()

let emitSIG01Diagnostics (st: SymbolTable) (fplValue: FplValue) pos1 pos2 =
    if
        (fplValue.BlockType = FplValueType.Reference)
    then
        // collect candidates to match this reference from all theories and
        // add them to fplValues's scope
        let expressionId = fplValue.FplId

        st.Root.Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.iter (fun block ->
                if expressionId = block.FplId then
                    fplValue.Scope.Add(block.Type(true), block)
                else
                    match block.ExpressionType with
                    | FixType.Prefix symbol
                    | FixType.Postfix symbol ->
                        if expressionId = symbol then
                            fplValue.Scope.Add(block.Type(true), block)
                    | FixType.Infix(symbol, precedence) ->
                        if expressionId = symbol then
                            fplValue.Scope.Add(block.Type(true), block)
                            fplValue.AuxiliaryInfo <- precedence
                    | _ -> ()))

        if fplValue.Scope.Count = 0 then
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG01 expressionId
                    Diagnostic.Alternatives = Some "Declare a functional term or predicate with this symbol." 
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

let emitSIG04Diagnostics (fplValue: FplValue) (candidates: FplValue list) firstFailingArgument pos1 pos2 =
    let candidateNames =
        candidates |> List.map (fun fv -> fv.QualifiedName) |> String.concat ", "

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG04(fplValue.Type(true), candidateNames, firstFailingArgument)
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

/// Emits SIG04 diagnostics. If a single candidate was found, sets the fplValue's pointer representation to this candidate.
let emitSIG04TypeDiagnostics (st:SymbolTable) name (fplValue:FplValue) pos1 pos2 =

    let rightContext = st.EvalPath()
    if rightContext.EndsWith(".VariableType.ClassType.PredicateIdentifier") 
        || rightContext.EndsWith(".Expression.PredicateWithQualification.PredicateWithOptSpecification.PredicateIdentifier") then
        match tryMatchTypes st fplValue name with
        | (_, candidates, Some matchedFplValue) -> 
            fplValue.FplRepresentation <- FplRepresentation.Pointer matchedFplValue
        | (firstFailingArgument, candidates, None) -> 
            let candidateNames =
                candidates |> List.map (fun fv -> fv.QualifiedName) |> String.concat ", "
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG04(name, candidateNames, firstFailingArgument)
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
                Diagnostic.StartPos = fplValue.NameStartPos
                Diagnostic.EndPos = fplValue.NameEndPos
                Diagnostic.Code = PR000 (fplValue.Type(true))
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
        match arg.FplRepresentation with
        | FplRepresentation.PredRepr FplPredicate.True
        | FplRepresentation.PredRepr FplPredicate.False -> ()
        | FplRepresentation.PredRepr FplPredicate.Undetermined ->
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = arg.NameStartPos
                    Diagnostic.EndPos = arg.NameEndPos
                    Diagnostic.Code = LG000(typeOfPredicate, arg.Type(true))
                    Diagnostic.Alternatives = None 
                }
            diags.AddDiagnostic diagnostic
        | _ -> ()

    let emitLG001Diagnostics pos1 pos2 (arg: FplValue) =
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = LG001(typeOfPredicate, arg.Type(true), arg.FplRepresentation.String())
                Diagnostic.Alternatives = None 
            }
        diags.AddDiagnostic diagnostic

    fplValue.ValueList
    |> Seq.iter (fun arg ->
        match arg.FplRepresentation with
        | FplRepresentation.PredRepr _ -> emitLG000Diagnostics arg
        | FplRepresentation.Pointer variable ->
            match variable.FplRepresentation with
            | FplRepresentation.PredRepr _ -> emitLG000Diagnostics variable
            | _ -> emitLG001Diagnostics arg.NameStartPos arg.NameEndPos variable
        | _ -> emitLG001Diagnostics arg.NameStartPos arg.NameEndPos arg)

    let code = LG000("", "")
    let numbLG000 = filterByErrorCode diags code.Code

    if numbLG000.Length = fplValue.ValueList.Count then
        () // we have no reason to emit any diagnostics since the are as many undetermined predicates as arguments
    else
        diags.Collection
        |> List.iter (fun d -> ad.AddDiagnostic d)
