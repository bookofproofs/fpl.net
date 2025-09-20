﻿/// This modulde contains all side-effect functions necessary to emit diagnostics for the FPL language server.
module FplInterpreterDiagnosticsEmitterPre

open FParsec
open FplPrimitives
open ErrDiagnostics
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

let emitID001Diagnostics alreadyDeclaredTypeStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID001(alreadyDeclaredTypeStr, qualifiedStartPosConflictStr)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitID002Diagnostics nodeTypeName incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID002(nodeTypeName, incorrectBlockType)
            Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
        }
    ad.AddDiagnostic diagnostic

let emitID004Diagnostics nodeTypeName listOfCandidates pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID004(nodeTypeName, listOfCandidates)
            Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
        }
    ad.AddDiagnostic diagnostic

let emitID007Diagnostics pos1 pos2 fplValueTypeStr listOfCandidates =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID007(fplValueTypeStr, listOfCandidates)
            Diagnostic.Alternatives = Some "Disambiguate the candidates by naming them differently." 
        }
    ad.AddDiagnostic diagnostic

let emitID010Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID010 identifier // identifier not found
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitID012Diagnostics identifier candidates pos1 pos2 =
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

let emitID013Diagnostics pos1 pos2 message =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID013 message
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitID014Diagnostics alreadyDeclaredMixedStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID014(alreadyDeclaredMixedStr, qualifiedStartPosConflictStr)
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitID017Diagnostics name candidatesNames pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID017(name, candidatesNames) 
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

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

let emitID023Diagnostics multipleCandidates pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID023 multipleCandidates
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitLG002diagnostic nodeTypeName times pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG002(nodeTypeName,times)
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitLG003diagnostic nodeTypeName nodeName nodeRepr pos1 pos2 = 
    if nodeRepr = LiteralFalse then
        let code = 
            if isEnglishAn nodeName then
                LG003(nodeTypeName, $"an {nodeName}")
            else
                LG003(nodeTypeName, $"a {nodeName}")
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = code
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let emitLG004diagnostic nodeName arity pos1 pos2 = 
    if arity > 0 then
        let code = 
            if isEnglishAn nodeName then
                LG004 $"an {nodeName}"
            else
                LG004 $"a {nodeName}"
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = code
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let emitLG005Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG005 name
            Diagnostic.Alternatives = Some "Expected a theorem-like statement (theorem, lemma, proposition, corollary)." 
        }

    ad.AddDiagnostic diagnostic

let emitPR000Diagnostics incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR000 incorrectBlockType
            Diagnostic.Alternatives = Some "Expected a definition (def class, def predicate, def function)."
        }
    ad.AddDiagnostic diagnostic

let emitPR001Diagnostics incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR001 incorrectBlockType
            Diagnostic.Alternatives = Some "Expected another proof, followed by its argument." 
        }
    ad.AddDiagnostic diagnostic

let emitPR002Diagnostics incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR002 incorrectBlockType
            Diagnostic.Alternatives = Some "Expected another proof, followed by its argument." 
        }
    ad.AddDiagnostic diagnostic


let emitPR003Diagnostics alreadyDeclaredMixedStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR003(alreadyDeclaredMixedStr, qualifiedStartPosConflictStr)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitPR004Diagnostics alreadyDeclaredTypeStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR004(alreadyDeclaredTypeStr, qualifiedStartPosConflictStr)
            Diagnostic.Alternatives = None 
        }

    ad.AddDiagnostic diagnostic

let emitPR005Diagnostics argumentName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR005 argumentName // argument reference not defined
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR006Diagnostics proofName argumentName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR006 (proofName, argumentName) // argument in proof not defined
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR007Diagnostics nodeTypeName nodeName pos1 pos2 = 
        let code = 
            if isEnglishAn nodeName then
                PR007 (nodeTypeName, $"an {nodeName}")
            else
                PR007 (nodeTypeName, $"a {nodeName}")
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Warning
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = code
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let emitPR008Diagnostics nodeName expectedInput actualInput pos1 pos2 = 
        let code = PR008 (nodeName, expectedInput, actualInput)
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Warning
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = code
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

let emitPR009Diagnostics pos1 pos2 =

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR009 // not all arguments verifiable
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic
    
let emitPR010Diagnostics keyword exptectedRef pos1 pos2 =
    let code = 
        if isEnglishAn exptectedRef then
            PR010 (keyword, $"an {exptectedRef}")
        else
            PR010 (keyword, $"a {exptectedRef}")
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = code 
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR011Diagnostics keyword exptectedRef pos1 pos2 =
    let code = 
        if isEnglishAn exptectedRef then
            PR010 (keyword, $"an {exptectedRef}")
        else
            PR010 (keyword, $"a {exptectedRef}")
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = code
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR012Diagnostics pos1 pos2 =

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR012 
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR013Diagnostics pos1 pos2 =

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR013 
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitPR014Diagnostics pos1 pos2 =

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR014 
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitSIG03Diagnostics errMsg mapTypeStr pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG03(errMsg, mapTypeStr)
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic

let emitSIG05Diagnostics assigneeTypeStr assignedTypeStr pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG05(assigneeTypeStr, assignedTypeStr)
            Diagnostic.Alternatives = None 
        }
    ad.AddDiagnostic diagnostic
        
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