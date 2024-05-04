module FplInterpreterDiagnosticsEmitter
open System
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


