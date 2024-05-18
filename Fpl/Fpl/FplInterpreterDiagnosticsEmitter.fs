module FplInterpreterDiagnosticsEmitter
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplParser
open FplInterpreterTypes

let emitUnexpectedErrorDiagnostics streamName errMsg = 
        let diagnostic = { 
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position(streamName,0,1,1)
            Diagnostic.EndPos = Position(streamName,0,1,1)
            Diagnostic.Code = GEN00 errMsg
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic(diagnostic)

let emitVAR01orID001diagnostics (fplValue:FplValue) (conflict:FplValue) = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = 
            if (FplValue.IsVariable(fplValue)) then 
                VAR01 (fplValue.Name, conflict.QualifiedStartPos)
            else
                ID001 (fplValue.Name, conflict.QualifiedStartPos)
        Diagnostic.Alternatives = None 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitVAR02diagnostics (fplValue:FplValue) (conflict:FplValue) = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = VAR02 (fplValue.Name, conflict.QualifiedStartPos)
        Diagnostic.Alternatives = None 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitVAR03diagnostics (fplValue:FplValue) (conflict:FplValue) = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = VAR03 (fplValue.Name, conflict.QualifiedStartPos)
        Diagnostic.Alternatives = Some "Remove this variable declaration or rename the variable." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitVAR03diagnosticsForCorollarysSignatureVariale (fplValue:FplValue) = 
    if FplValue.IsCorollary(fplValue) then
        for kv in fplValue.Scope do
            let res = FplValue.CorollaryVariableInOuterScope(kv.Value) 
            match res with
            | ScopeSearchResult.Found conflict ->
                emitVAR03diagnostics kv.Value conflict 
            | _ -> ()

let emitID002diagnostics (fplValue:FplValue) incorrectBlockType = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID002 (fplValue.Name, incorrectBlockType)
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID005diagnostics (fplValue:FplValue) incorrectBlockType = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID005 (fplValue.Name, incorrectBlockType)
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary), a conjecture, or an axiom." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID003diagnostics (fplValue:FplValue) = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID003 fplValue.Name
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID006diagnostics (fplValue:FplValue) = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID006 fplValue.Name
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary), a conjecture, or an axiom." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID004diagnostics (fplValue:FplValue) listOfCandidates = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID004 (fplValue.Name, listOfCandidates)
        Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID007diagnostics (fplValue:FplValue) listOfCandidates = 
    let diagnostic = { 
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID007 (fplValue.Name, listOfCandidates)
        Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let tryAddVariadicVariables numberOfVariadicVars startPos endPos =
    if numberOfVariadicVars > 1 then
        let diagnostic = { 
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = startPos
            Diagnostic.EndPos = endPos
            Diagnostic.Code = VAR00 
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic

let checkID008Diagnostics (fplValue:FplValue) pos1 pos2 =
    if FplValue.IsConstructor(fplValue) && fplValue.TypeSignature.Length = 1 then 
        let nameStart = fplValue.TypeSignature.Head
        let className = fplValue.Parent.Value.Name
        if nameStart <> className then
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID008(nameStart, className) // misspelled constructor name 
                    Diagnostic.Alternatives = None 
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic

let checkID009_ID010_ID011_Diagnostics (st:SymbolTable) (fplValue:FplValue) name pos1 pos2 =
    let rec findPath (root: FplValue) (candidateName: string) =
        if root.Name = candidateName then
            Some(root.Name)
        else
            root.ValueList
            |> Seq.collect (fun child -> 
                match findPath child candidateName with
                | Some path -> [root.Name + ":" + path]
                | None -> [])
            |> Seq.tryLast
    let rightContext = st.EvalPath() 
    let classInheritanceChain = findPath fplValue name 
    if rightContext.EndsWith("InheritedClassType.PredicateIdentifier") then
        if fplValue.Name = name then 
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID009 name // circular base dependency
                    Diagnostic.Alternatives = None 
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic
        else
            match classInheritanceChain with
            | Some chain ->
                let diagnostic =
                    { 
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = pos1
                        Diagnostic.EndPos = pos2
                        Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                        Diagnostic.Alternatives = None 
                    }
                FplParser.parserDiagnostics.AddDiagnostic diagnostic
            | _ -> 
            match FplValue.InScopeOfParent(fplValue) name with
            | ScopeSearchResult.Found classCandidate ->
                let mutable duplicateInheritanceChainFound = false
                fplValue.ValueList
                |> Seq.iter (fun child -> 
                    let classInheritanceChain = findPath classCandidate child.Name 
                    match classInheritanceChain with
                    | Some chain ->
                        let diagnostic =
                            { 
                                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                                Diagnostic.Severity = DiagnosticSeverity.Error
                                Diagnostic.StartPos = pos1
                                Diagnostic.EndPos = pos2
                                Diagnostic.Code = ID011(child.Name, chain) // inheritance chain duplicate
                                Diagnostic.Alternatives = None 
                            }
                        FplParser.parserDiagnostics.AddDiagnostic diagnostic
                        duplicateInheritanceChainFound <- true
                    | _ -> ()
                )
                if not duplicateInheritanceChainFound then 
                    fplValue.ValueList.Add classCandidate
            | _ -> 
                let diagnostic =
                    { 
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = pos1
                        Diagnostic.EndPos = pos2
                        Diagnostic.Code = ID010 name // class not found
                        Diagnostic.Alternatives = None 
                    }
                FplParser.parserDiagnostics.AddDiagnostic diagnostic
    elif rightContext.EndsWith("InheritedClassType.ObjectType") then
        match classInheritanceChain with
        | Some chain ->
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                    Diagnostic.Alternatives = None 
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic
        | _ -> 
            let mutable duplicateInheritanceChainFound = false
            fplValue.ValueList
            |> Seq.iter (fun child -> 
                let classInheritanceChain = findPath child name 
                match classInheritanceChain with
                | Some chain ->
                    let diagnostic =
                        { 
                            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                            Diagnostic.Severity = DiagnosticSeverity.Error
                            Diagnostic.StartPos = pos1
                            Diagnostic.EndPos = pos2
                            Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                            Diagnostic.Alternatives = None 
                        }
                    FplParser.parserDiagnostics.AddDiagnostic diagnostic
                    duplicateInheritanceChainFound <- true
                | _ -> ()
            )
            if not duplicateInheritanceChainFound then 
                let obj = FplValue.CreateObject((pos1,pos2))
                fplValue.ValueList.Add obj

let checkID012Diagnostics (st:SymbolTable) (parentConstructorCall:FplValue) identifier (pos1:Position) pos2 =
    let context = st.EvalPath()
    if context.EndsWith("ParentConstructorCall.InheritedClassType.PredicateIdentifier") || context.EndsWith("ParentConstructorCall.InheritedClassType.ObjectType") then
        let constructor = parentConstructorCall.Parent.Value
        constructor.ValueList.Add(parentConstructorCall)
        let classOfConstructor = constructor.Parent.Value
        let mutable foundInheritanceClass = false
        let candidates = 
            classOfConstructor.ValueList
            |> Seq.map (fun inheritanceClass ->
                if inheritanceClass.Name = identifier then 
                    foundInheritanceClass <- true
                inheritanceClass.Name
            )
            |> String.concat ", "
        if not foundInheritanceClass then 
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID012 (identifier, candidates) // call of parent class does not match the class id
                    Diagnostic.Alternatives = None 
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID000Diagnostics streamName astType = 
    let diagnostic =
        { 
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position(streamName, 0, 1, 1)
            Diagnostic.EndPos = Position(streamName, 0, 1, 1)
            Diagnostic.Code = ID000 astType
            Diagnostic.Alternatives = None 
        }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitSIG00Diagnostics (fplValue:FplValue) pos1 pos2 = 
    let detailed (exprType:ExprType) expectedArity actualArity pos1 pos2 = 
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = SIG00 (exprType.Type, actualArity)
                Diagnostic.Alternatives = Some $"Arity of {expectedArity} expected."
            }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic
    match fplValue.ExpressionType with 
    | ExprType.Infix _ when fplValue.AuxiliaryInfo <> 2 -> 
        detailed fplValue.ExpressionType 2 fplValue.AuxiliaryInfo pos1 pos2
    | ExprType.Prefix _ when fplValue.AuxiliaryInfo <> 1 -> 
        detailed fplValue.ExpressionType 1 fplValue.AuxiliaryInfo pos1 pos2
    | ExprType.Postfix _ when fplValue.AuxiliaryInfo <> 1 -> 
        detailed fplValue.ExpressionType 1 fplValue.AuxiliaryInfo pos1 pos2
    | _ -> ()


let emitSIG02Diagnostics (st:SymbolTable) (fplValue:FplValue) pos1 pos2 = 
    let precedences = Dictionary<int,FplValue>()
    match fplValue.ExpressionType with
    | ExprType.Infix (symbol, precedence) -> 
        let precedenceWasAlreadyThere precedence fv = 
            if not (precedences.ContainsKey(precedence)) then
                precedences.Add(precedence,fv)
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
                | ExprType.Infix(_, precedence) -> 
                    precedenceWasAlreadyThere precedence block |> ignore
                | _ -> ()
            )
        )

        if precedenceWasAlreadyThere precedence fplValue then
            let conflictList = 
                precedences.Values
                |> Seq.toList
                |> List.map (fun fv -> fv.QualifiedStartPos)
                |> String.concat ", "
            match fplValue.ExpressionType with
            | ExprType.Infix (symbol, precedence) ->
                let diagnostic =
                    { 
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                        Diagnostic.Severity = DiagnosticSeverity.Information
                        Diagnostic.StartPos = pos1
                        Diagnostic.EndPos = pos2
                        Diagnostic.Code = SIG02 (symbol,precedence,conflictList)
                        Diagnostic.Alternatives = Some "Consider disambiguating the precedence to avoid unexpected results."
                    }
                FplParser.parserDiagnostics.AddDiagnostic diagnostic
            | _ -> ()
    | _ -> ()

let emitSIG01Diagnostics (st:SymbolTable) (fplValue:FplValue) pos1 pos2 = 
    if fplValue.BlockType = FplValueType.Reference || fplValue.BlockType = FplValueType.Expression then 
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
                    fplValue.Scope.Add(block.Name, block)
                else 
                    match block.ExpressionType with 
                    | ExprType.Prefix symbol 
                    | ExprType.Postfix symbol -> 
                        if expressionId = symbol then 
                            fplValue.Scope.Add(block.Name, block)
                    | ExprType.Infix (symbol, precedence) ->
                        if expressionId = symbol then 
                            fplValue.Scope.Add(block.Name, block)
                            fplValue.AuxiliaryInfo <- precedence
                    | _ -> ()
            )
        )
        if fplValue.Scope.Count = 0 then
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG01 expressionId
                    Diagnostic.Alternatives = Some "Declare a functional term or predicate with this symbol."
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic
            
        