/// This module contains all side-effect functions necessary to emit diagnostics for the FPL language server.
module Fpl.Errors.Emitter

open FParsec
open Fpl.Primitives
open Fpl.Errors.Messages
open Fpl.Errors.Diagnostics
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
        }
    ad.AddDiagnostic(diagnostic)
    // do not aggregate GEN00 and return unit instead of Some (diagnostic.Code.Code)

let emitID001Diagnostics alreadyDeclaredTypeStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID001(alreadyDeclaredTypeStr, qualifiedStartPosConflictStr)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID002Diagnostics nodeTypeName incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID002(nodeTypeName, incorrectBlockType)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID003Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID003 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID005Diagnostics name incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID005(name, incorrectBlockType)
         }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID006Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID006 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID007Diagnostics nodeType signatureNode baseType signatureBase pos1 pos2 =

    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID007 (capitalize nodeType, signatureNode, baseType, signatureBase)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID008Diagnostics constructorId classId pos1 pos2 =
    if constructorId <> classId then
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = ID008(constructorId, classId) // misspelled constructor name
            }
        ad.AddDiagnostic diagnostic
        Some (diagnostic.Code.Code)
    else
        None

let emitID009Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID009 name // circular base dependency
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)


let emitID010Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID010 identifier // identifier not found
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)


let emitID011Diagnostics chain errorMsg pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID011(chain, errorMsg) // inheritance chain-related error
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID012Diagnostics prtyName varName varType candidates pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID012 (prtyName, varName, varType, candidates)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID013Diagnostics message pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID013 message
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID014Diagnostics alreadyDeclaredMixedStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID014(alreadyDeclaredMixedStr, qualifiedStartPosConflictStr)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID015Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID015 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID016Diagnostics name pos1 pos2  =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID016 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID017Diagnostics name candidatesNames incompatible pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID017(name, candidatesNames, incompatible) 
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID018Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID018 identifier
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID020Diagnostics identifier pos1 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos1
            Diagnostic.Code = ID020 identifier
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID021Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID021 identifier
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID022Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID022 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID023Diagnostics multipleCandidates pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID023 multipleCandidates
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitID024Diagnostics alreadyLocalizedExpr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID024(alreadyLocalizedExpr, qualifiedStartPosConflictStr)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// Emits ID025 diagnostics if the blockName cannot be referred from qualifiedNameCandidate
let checkID025Diagnostics qualifiedNameCandidate blockName pos1 pos2 =
    match blockName with 
    | LiteralAxL
    | LiteralThmL
    | LiteralLemL
    | LiteralPropL
    | LiteralConjL
    | LiteralConjL
    | LiteralCorL
    | LiteralPrfL
    | LiteralLocL ->
        let blockEnglishName = getEnglishName blockName false
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = ID025(qualifiedNameCandidate, blockEnglishName)
            }
        ad.AddDiagnostic diagnostic
        Some (diagnostic.Code.Code)
    | _ -> None


let emitID027Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID027 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitLG001Diagnostics argType argName typeOfPredicate pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG001(typeOfPredicate, argName, argType)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitLG002Diagnostics nodeTypeName times pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG002(nodeTypeName,times)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitLG003Diagnostics nodeTypeName nodeName nodeRepr pos1 pos2 = 
    if nodeRepr = LiteralFalse then
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = LG003(nodeTypeName, getEnglishName nodeName false)
            }
        ad.AddDiagnostic diagnostic
        Some (diagnostic.Code.Code)
    else
        None

let emitLG004Diagnostics nodeName pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG004 (getEnglishName nodeName false)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitLG005Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG005 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitNSP00Diagnostics fileNamePattern pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP00 fileNamePattern
        }
    ad.AddDiagnostic diagnostic

let emitNSP01Diagnostics filename message pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP01 (filename, message)
        }
    ad.AddDiagnostic diagnostic 

let emitNSP02Diagnostics url message pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP02 (url, message)
        }
    ad.AddDiagnostic diagnostic 

let emitNSP03Diagnostics aliasOrStar pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP03 aliasOrStar
        }
    ad.AddDiagnostic diagnostic

let emitNSP04Diagnostics path pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP04 path
        }
    ad.AddDiagnostic diagnostic


let emitNSP05Diagnostics pathTypes theoryName chosenPathType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP05 (pathTypes, theoryName, chosenPathType)
        }
    ad.AddDiagnostic diagnostic

let emitPR001Diagnostics incorrectBlockType justificationItemName pos1 pos2 alternative =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR001 (incorrectBlockType, justificationItemName, alternative)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR003Diagnostics alreadyDeclaredMixedStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR003(alreadyDeclaredMixedStr, qualifiedStartPosConflictStr)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR004Diagnostics alreadyDeclaredTypeStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR004 alreadyDeclaredTypeStr 
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR005Diagnostics argumentName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR005 argumentName // argument reference not defined
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR006Diagnostics proofName argumentName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR006 (proofName, argumentName) // argument in proof not defined
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR007Diagnostics nodeName nodeTypeName pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR007 (nodeName, capitalize nodeTypeName)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR008Diagnostics byInfName numbPrem expectedPremise mismatchingCandidates pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR008 (byInfName, numbPrem, expectedPremise, mismatchingCandidates)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR009Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR009 // not all arguments verifiable
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)
    
let emitPR010Diagnostics keyword exptectedRef pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR010 (keyword, getEnglishName exptectedRef false)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR011Diagnostics keyword exptectedRef pos1 pos2 =            
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR011 (keyword, getEnglishName exptectedRef false)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR012Diagnostics providedIdentifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR012 providedIdentifier
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR013Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR013 
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR014Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR014 
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR015Diagnostics argumentID pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR015 argumentID
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR016Diagnostics argumentID lastAssumedArgumentId pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR016(argumentID, lastAssumedArgumentId)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR017Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR017 
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR019Diagnostics justificationType1 justificationType2 pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR019 (justificationType1, justificationType2)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR020Diagnostics expectedNum actualNum pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR020 (expectedNum, actualNum)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR021Diagnostics mismatchingCandidates inferredFormula justificationName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR021 (mismatchingCandidates, inferredFormula, justificationName)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitPR022Diagnostics reason pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR022 reason
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG00Diagnostics exprType expectedArity actualArity pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG00(exprType, actualArity, expectedArity)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG01Diagnostics expressionId pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG01 expressionId
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG02Diagnostics symbol precedence conflict pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG02(symbol, precedence, conflict)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG03Diagnostics errMsg pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG03 errMsg
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)


/// Occurs in the following cases:
/// 1: A variable type declaration uses a non-existing type reference.
/// 2: A mapping type declaration uses a non-existing type reference.
/// 3: A dotted reference uses a signature for call-by-value that doesn't match. 
/// 4: A reference uses a signature for call-by-value that doesn't match. 
/// 5: A reference uses a type reference that doesn't exist.
let emitSIG04Diagnostics mixedName errList pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG04(mixedName, errList)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG05Diagnostics errMsg pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG05 errMsg
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG06Diagnostics name oldFromNode newFromNode typeName pos1 pos2  = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG06(name, oldFromNode, newFromNode, typeName)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG07diagnostics assigneeName assigneeType pos1 pos2  = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG07(assigneeName, assigneeType)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG08Diagnostics arrName indexVarName indexVarType dimType dimNumber pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG08(arrName, indexVarName, indexVarType, dimType, dimNumber)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG09Diagnostics arrName dimType dimNumber pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG09(arrName, dimType, dimNumber)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG10Diagnostics arrName indexVarName indexNumber pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG10(arrName, indexVarName, indexNumber)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG11Diagnostics qualifiedWrongCandidate typeOfCandidate pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG11 (qualifiedWrongCandidate, typeOfCandidate)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG12Diagnostics templateName secondUsage firstUsage firstUsagePos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG12 (templateName, secondUsage, firstUsage, firstUsagePos)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG13Diagnostics stmtName secondUsage firstUsage firstUsagePos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG13 (stmtName, secondUsage, firstUsage, firstUsagePos)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSIG14Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG14
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitST001Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST001 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitST002Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST002 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitST004Diagnostics languageCode pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST004 languageCode
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitST005Diagnostics domain nodeType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST005 (domain, getEnglishName nodeType false)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSY000Diagnostics errMsg pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplParser
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY000 errMsg
        }
    ad.AddDiagnostic diagnostic

let emitSY001Diagnostics errMsg pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplParser
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY001 errMsg
        }
    ad.AddDiagnostic diagnostic

let emitSY002Diagnostics errMsg chain pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplParser
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY002(errMsg, chain)
        }
    ad.AddDiagnostic diagnostic

let emitSY010Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY010
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSY011Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY011
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSY012Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY012
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSY013Diagnostics innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY013 (innerInfixSymbol, innerPrecedence, outerInfixSymbol, outerPrecedence)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitSY014Diagnostics infixSymbol1 infixSymbol2 precedence pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY014 (infixSymbol1, infixSymbol2, precedence)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR00Diagnostics startPos endPos =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = startPos
            Diagnostic.EndPos = endPos
            Diagnostic.Code = VAR00
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR01Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR01 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR02Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR02 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR03Diagnostics varName conflictStartPos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR03(varName, conflictStartPos)
        }

    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR04Diagnostics name pos1 pos2 = 
    let diagnostic = { 
        Diagnostic.Uri = ad.CurrentUri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Warning
        Diagnostic.StartPos = pos1
        Diagnostic.EndPos = pos2
        Diagnostic.Code = VAR04 name
    }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR05Diagnostics name pos1 pos2 = 
    let diagnostic = { 
        Diagnostic.Uri = ad.CurrentUri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = pos1
        Diagnostic.EndPos = pos2
        Diagnostic.Code = VAR05 name
    }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR06iagnostic name oldFromNode newFromNode typeName pos1 pos2  = 
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR06(name, oldFromNode, newFromNode, typeName)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR07Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR07 name
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR08Diagnostics varName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR08 varName
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR09Diagnostics varName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR09 varName
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR10Diagnostics varName formulaName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR10(varName, formulaName)
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

let emitVAR11Diagnostics varName conflictStartPos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = ad.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR11(varName, conflictStartPos)
                    
        }
    ad.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)
