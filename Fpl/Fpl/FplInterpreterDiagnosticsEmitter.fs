module FplInterpreterDiagnosticsEmitter
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplParser
open FplInterpreterTypes


let emitVAR01orID001diagnostics (fplValue:FplValue) (conflict:FplValue) uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
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

let emitVAR02diagnostics (fplValue:FplValue) (conflict:FplValue) uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = VAR02 (fplValue.Name, conflict.QualifiedStartPos)
        Diagnostic.Alternatives = None 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitVAR03diagnostics (fplValue:FplValue) (conflict:FplValue) uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = VAR03 (fplValue.Name, conflict.QualifiedStartPos)
        Diagnostic.Alternatives = Some "Remove this variable declaration or rename the variable." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitVAR03diagnosticsForCorollarysSignatureVariale (fplValue:FplValue) uri = 
    if FplValue.IsCorollary(fplValue) then
        for kv in fplValue.Scope do
            let res = FplValue.CorollaryVariableInOuterScope(kv.Value) 
            match res with
            | ScopeSearchResult.Found conflict ->
                emitVAR03diagnostics kv.Value conflict uri
            | _ -> ()

let emitID002diagnostics (fplValue:FplValue) incorrectBlockType uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID002 (fplValue.Name, incorrectBlockType)
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID005diagnostics (fplValue:FplValue) incorrectBlockType uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID005 (fplValue.Name, incorrectBlockType)
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary), a conjecture, or an axiom." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID003diagnostics (fplValue:FplValue) uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID003 fplValue.Name
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID006diagnostics (fplValue:FplValue) uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID006 fplValue.Name
        Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary), a conjecture, or an axiom." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID004diagnostics (fplValue:FplValue) listOfCandidates uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID004 (fplValue.Name, listOfCandidates)
        Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let emitID007diagnostics (fplValue:FplValue) listOfCandidates uri = 
    let diagnostic = { 
        Diagnostic.Uri = uri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = fplValue.StartPos
        Diagnostic.EndPos = fplValue.NameEndPos
        Diagnostic.Code = ID007 (fplValue.Name, listOfCandidates)
        Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
    }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic

let tryAddVariadicVariables (uri:System.Uri) numberOfVariadicVars startPos endPos =
    if numberOfVariadicVars > 1 then
        let diagnostic = { 
            Diagnostic.Uri = uri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = startPos
            Diagnostic.EndPos = endPos
            Diagnostic.Code = VAR00 
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic

let checkID008Diagnostics (fplValue:FplValue) uri pos1 pos2 =
    if FplValue.IsConstructor(fplValue) && fplValue.TypeSignature.Length = 1 then 
        let nameStart = fplValue.TypeSignature.Head
        let className = fplValue.Parent.Value.Name
        if nameStart <> className then
            let diagnostic =
                { 
                    Diagnostic.Uri = uri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID008(nameStart, className) // misspelled constructor name 
                    Diagnostic.Alternatives = None 
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic

let checkID009_ID010_ID011_Diagnostics (st:SymbolTable) (fplValue:FplValue) name uri pos1 pos2 =
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
                    Diagnostic.Uri = uri
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
                        Diagnostic.Uri = uri
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
                let mutable found = false
                fplValue.ValueList
                |> Seq.iter (fun child -> 
                    let classInheritanceChain = findPath classCandidate child.Name 
                    match classInheritanceChain with
                    | Some chain ->
                        let diagnostic =
                            { 
                                Diagnostic.Uri = uri
                                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                                Diagnostic.Severity = DiagnosticSeverity.Error
                                Diagnostic.StartPos = pos1
                                Diagnostic.EndPos = pos2
                                Diagnostic.Code = ID011(child.Name, chain) // inheritance chain duplicate
                                Diagnostic.Alternatives = None 
                            }
                        FplParser.parserDiagnostics.AddDiagnostic diagnostic
                        found <- true
                    | _ -> ()
                )
                if not found then 
                    fplValue.ValueList.Add classCandidate
            | _ -> 
                let diagnostic =
                    { 
                        Diagnostic.Uri = uri
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
                    Diagnostic.Uri = uri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                    Diagnostic.Alternatives = None 
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic
        | _ -> 
            let mutable found = false
            fplValue.ValueList
            |> Seq.iter (fun child -> 
                let classInheritanceChain = findPath child name 
                match classInheritanceChain with
                | Some chain ->
                    let diagnostic =
                        { 
                            Diagnostic.Uri = uri
                            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                            Diagnostic.Severity = DiagnosticSeverity.Error
                            Diagnostic.StartPos = pos1
                            Diagnostic.EndPos = pos2
                            Diagnostic.Code = ID011(name, chain) // inheritance chain duplicate
                            Diagnostic.Alternatives = None 
                        }
                    FplParser.parserDiagnostics.AddDiagnostic diagnostic
                    found <- true
                | _ -> ()
            )
            if not found then 
                let obj = FplValue.CreateObject((pos1,pos2))
                fplValue.ValueList.Add obj


let emitID000Diagnostics uri astType = 
    let diagnostic =
        { 
            Diagnostic.Uri = uri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position("", 0, 1, 1)
            Diagnostic.EndPos = Position("", 0, 1, 1)
            Diagnostic.Code = ID000 astType
            Diagnostic.Alternatives = None 
        }
    FplParser.parserDiagnostics.AddDiagnostic diagnostic
