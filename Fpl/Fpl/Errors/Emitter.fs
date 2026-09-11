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
/// <summary>
/// Emit a general unexpected interpreter diagnostic.
/// </summary>
/// <param name="errMsg">Human-readable error message to emit.</param>
/// <returns>Unit.</returns>
let emitUnexpectedErrorDiagnostics errMsg =
    let diagnostic =
        {
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position("", 0, 1, 1)
            Diagnostic.EndPos = Position("", 0, 1, 1)
            Diagnostic.Code = GEN00 errMsg
        }
    diagnosticsContainer.AddDiagnostic(diagnostic)
    // do not aggregate GEN00 and return unit instead of Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID001 diagnostic for a duplicated declaration conflict.
/// </summary>
/// <param name="alreadyDeclaredTypeStr">The already-declared type signature string.</param>
/// <param name="qualifiedStartPosConflictStr">Qualified start position representation for the conflict.</param>
/// <param name="pos1">Start position for the diagnostic.</param>
/// <param name="pos2">End position for the diagnostic.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID001Diagnostics alreadyDeclaredTypeStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID001(alreadyDeclaredTypeStr, qualifiedStartPosConflictStr)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID002 diagnostic for an incorrect block type usage.
/// </summary>
/// <param name="nodeTypeName">Name of the node type.</param>
/// <param name="incorrectBlockType">The incorrect block type string.</param>
/// <param name="pos1">Start position for the diagnostic.</param>
/// <param name="pos2">End position for the diagnostic.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID002Diagnostics nodeTypeName incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID002(nodeTypeName, incorrectBlockType)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID003 diagnostic for identifier-related errors.
/// </summary>
/// <param name="name">Identifier name involved.</param>
/// <param name="pos1">Start position for the diagnostic.</param>
/// <param name="pos2">End position for the diagnostic.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID003Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID003 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID005 diagnostic for block type conflicts.
/// </summary>
/// <param name="name">Identifier or signature.</param>
/// <param name="incorrectBlockType">The incorrect block type string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID005Diagnostics name incorrectBlockType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID005(name, incorrectBlockType)
         }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID006 diagnostic for identifier-specific errors.
/// </summary>
/// <param name="name">Identifier name involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID006Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID006 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID007 diagnostic when a node cannot inherit from a specified base.
/// </summary>
/// <param name="nodeType">Human-readable node type.</param>
/// <param name="signatureNode">Signature of the node.</param>
/// <param name="baseType">Human-readable base type.</param>
/// <param name="signatureBase">Signature of the base.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID007Diagnostics nodeType signatureNode baseType signatureBase pos1 pos2 =

    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID007 (capitalize nodeType, signatureNode, baseType, signatureBase)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID008 diagnostic when a constructor identifier is misspelled relative to a class id.
/// </summary>
/// <param name="constructorId">Constructor identifier found.</param>
/// <param name="classId">Expected class identifier.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted, or None if no diagnostic is needed.</returns>
let emitID008Diagnostics constructorId classId pos1 pos2 =
    if constructorId <> classId then
        let diagnostic =
            { 
                Diagnostic.Uri = diagnosticsContainer.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = ID008(constructorId, classId) // misspelled constructor name
            }
        diagnosticsContainer.AddDiagnostic diagnostic
        Some (diagnostic.Code.Code)
    else
        None

/// <summary>
/// Emit ID009 diagnostic (e.g. circular base dependency).
/// </summary>
/// <param name="name">Relevant name involved in the error.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID009Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID009 name // circular base dependency
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)


/// <summary>
/// Emit ID010 diagnostic when an identifier cannot be found.
/// </summary>
/// <param name="identifier">Identifier that was not found.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID010Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID010 identifier // identifier not found
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)


/// <summary>
/// Emit ID011 diagnostic related to inheritance-chain errors.
/// </summary>
/// <param name="chain">Inheritance chain representation.</param>
/// <param name="errorMsg">Inner error message to include.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID011Diagnostics chain errorMsg pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID011(chain, errorMsg) // inheritance chain-related error
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID012 diagnostic for property/variable type resolution failures.
/// </summary>
/// <param name="prtyName">Property name.</param>
/// <param name="varName">Variable name.</param>
/// <param name="varType">Variable type string.</param>
/// <param name="candidates">Candidate list for resolution.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID012Diagnostics prtyName varName varType candidates pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID012 (prtyName, varName, varType, candidates)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID013 diagnostic with a custom message.
/// </summary>
/// <param name="message">Diagnostic message payload.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID013Diagnostics message pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID013 message
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID014 diagnostic for re-declaration in mixed contexts.
/// </summary>
/// <param name="alreadyDeclaredMixedStr">Previously declared mixed identifier string.</param>
/// <param name="qualifiedStartPosConflictStr">Qualified start position of conflict.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID014Diagnostics alreadyDeclaredMixedStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID014(alreadyDeclaredMixedStr, qualifiedStartPosConflictStr)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID015 diagnostic for identifier collision or misuse.
/// </summary>
/// <param name="name">Identifier name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID015Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID015 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID016 diagnostic for identifier-related warnings/errors.
/// </summary>
/// <param name="name">Identifier involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID016Diagnostics name pos1 pos2  =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID016 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID017 diagnostic when multiple candidates are incompatible.
/// </summary>
/// <param name="name">Name involved in the conflict.</param>
/// <param name="candidatesNames">Comma-separated candidate names.</param>
/// <param name="incompatible">Bool indicating incompatibility details.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID017Diagnostics name candidatesNames incompatible pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID017(name, candidatesNames, incompatible) 
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID018 diagnostic for identifier-specific errors.
/// </summary>
/// <param name="identifier">Identifier involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID018Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID018 identifier
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID020 diagnostic at a single position (start=end).
/// </summary>
/// <param name="identifier">Identifier involved.</param>
/// <param name="pos1">Position for the diagnostic (used as both start and end).</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID020Diagnostics identifier pos1 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos1
            Diagnostic.Code = ID020 identifier
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID021 diagnostic for identifier-specific errors (with range).
/// </summary>
/// <param name="identifier">Identifier involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID021Diagnostics identifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID021 identifier
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID022 diagnostic for name-resolution problems.
/// </summary>
/// <param name="name">The name involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID022Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID022 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID023 diagnostic when multiple matching candidates were found.
/// </summary>
/// <param name="multipleCandidates">List of candidate names.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID023Diagnostics multipleCandidates pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID023 multipleCandidates
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ID024 diagnostic for already-localized expressions conflicts.
/// </summary>
/// <param name="alreadyLocalizedExpr">Localized expression identifier.</param>
/// <param name="qualifiedStartPosConflictStr">Qualified position string for conflict.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID024Diagnostics alreadyLocalizedExpr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID024(alreadyLocalizedExpr, qualifiedStartPosConflictStr)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Conditionally emit ID025 diagnostic if a block cannot be referred from a qualified candidate.
/// </summary>
/// <param name="qualifiedNameCandidate">Qualified name candidate used for reference.</param>
/// <param name="blockName">Block name token to check.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted; None when no diagnostic applies.</returns>
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
                Diagnostic.Uri = diagnosticsContainer.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = ID025(qualifiedNameCandidate, blockEnglishName)
            }
        diagnosticsContainer.AddDiagnostic diagnostic
        Some (diagnostic.Code.Code)
    | _ -> None


/// <summary>
/// Emit ID027 diagnostic for name-related errors.
/// </summary>
/// <param name="name">Identifier name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitID027Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ID027 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit LG001 diagnostic when a predicate is applied to an argument of the wrong type.
/// </summary>
/// <param name="argType">Type of the argument expression.</param>
/// <param name="argName">Argument name or representation.</param>
/// <param name="typeOfPredicate">Type expected by the predicate.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitLG001Diagnostics argType argName typeOfPredicate pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG001(typeOfPredicate, argName, argType)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit LG002 diagnostic for repeated node occurrences errors.
/// </summary>
/// <param name="nodeTypeName">Node type name.</param>
/// <param name="times">Number of occurrences.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitLG002Diagnostics nodeTypeName times pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG002(nodeTypeName,times)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit LG003 diagnostic for logic-level representation problems.
/// </summary>
/// <param name="nodeTypeName">Node type name.</param>
/// <param name="nodeName">Node name token.</param>
/// <param name="nodeRepr">Node representation token.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted or None when not applicable.</returns>
let emitLG003Diagnostics nodeTypeName nodeName nodeRepr pos1 pos2 = 
    if nodeRepr = LiteralFalse then
        let diagnostic =
            { 
                Diagnostic.Uri = diagnosticsContainer.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = pos1
                Diagnostic.EndPos = pos2
                Diagnostic.Code = LG003(nodeTypeName, getEnglishName nodeName false)
            }
        diagnosticsContainer.AddDiagnostic diagnostic
        Some (diagnostic.Code.Code)
    else
        None

/// <summary>
/// Emit LG004 warning diagnostics.
/// </summary>
/// <param name="nodeName">Node name involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitLG004Diagnostics nodeName pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG004 (getEnglishName nodeName false)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit LG005 warning diagnostics.
/// </summary>
/// <param name="name">Name referenced in the warning.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitLG005Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = LG005 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit NSP00 diagnostic for namespace / path pattern errors.
/// </summary>
/// <param name="fileNamePattern">Filename pattern that failed to match.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitNSP00Diagnostics fileNamePattern pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP00 fileNamePattern
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit NSP01 diagnostic for namespace resolution errors with an inner message.
/// </summary>
/// <param name="filename">Filename that triggered the error.</param>
/// <param name="message">Inner diagnostic message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitNSP01Diagnostics filename message pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP01 (filename, message)
        }
    diagnosticsContainer.AddDiagnostic diagnostic 

/// <summary>
/// Emit NSP02 diagnostic for remote URL import errors.
/// </summary>
/// <param name="url">URL that caused the problem.</param>
/// <param name="message">Inner error message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitNSP02Diagnostics url message pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP02 (url, message)
        }
    diagnosticsContainer.AddDiagnostic diagnostic 

/// <summary>
/// Emit NSP03 diagnostic when an unrecognised alias or wildcard import is used.
/// </summary>
/// <param name="aliasOrStar">Alias or '*' token used.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitNSP03Diagnostics aliasOrStar pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP03 aliasOrStar
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit NSP04 diagnostic when a problematic path is encountered.
/// </summary>
/// <param name="path">Path string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitNSP04Diagnostics path pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP04 path
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit NSP05 diagnostic describing chosen path resolution from a list of candidates.
/// </summary>
/// <param name="pathTypes">Available path type list.</param>
/// <param name="theoryName">Theory name.</param>
/// <param name="chosenPathType">Chosen path type string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitNSP05Diagnostics pathTypes theoryName chosenPathType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = NSP05 (pathTypes, theoryName, chosenPathType)
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit PR001 diagnostic used by proof-related validation.
/// </summary>
/// <param name="incorrectBlockType">Incorrect block type string.</param>
/// <param name="justificationItemName">Justification item name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <param name="alternative">Alternative suggestion text.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR001Diagnostics incorrectBlockType justificationItemName pos1 pos2 alternative =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR001 (incorrectBlockType, justificationItemName, alternative)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR003 diagnostic for proof-related mixed declarations errors.
/// </summary>
/// <param name="alreadyDeclaredMixedStr">Previously declared mixed signature.</param>
/// <param name="qualifiedStartPosConflictStr">Qualified conflict location.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR003Diagnostics alreadyDeclaredMixedStr qualifiedStartPosConflictStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR003(alreadyDeclaredMixedStr, qualifiedStartPosConflictStr)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR004 warning when a justification item is already declared.
/// </summary>
/// <param name="alreadyDeclaredTypeStr">Previously declared type string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR004Diagnostics alreadyDeclaredTypeStr pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR004 alreadyDeclaredTypeStr 
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR005 diagnostic for undefined argument references in proofs.
/// </summary>
/// <param name="argumentName">Argument identifier.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR005Diagnostics argumentName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR005 argumentName // argument reference not defined
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR006 diagnostic for proof argument-not-defined errors.
/// </summary>
/// <param name="proofName">Name of the proof.</param>
/// <param name="argumentName">Argument name referenced.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR006Diagnostics proofName argumentName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR006 (proofName, argumentName) // argument in proof not defined
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR007 warning diagnostics related to node names and types.
/// </summary>
/// <param name="nodeName">Node name.</param>
/// <param name="nodeTypeName">Node type name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR007Diagnostics nodeName nodeTypeName pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR007 (nodeName, capitalize nodeTypeName)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR008 diagnostic showing which premise pattern mismatched and why.
/// </summary>
/// <param name="byInfName">Name of the inference rule.</param>
/// <param name="numbPrem">Number of premises expected.</param>
/// <param name="expectedPremise">Text describing the expected premise.</param>
/// <param name="mismatchingCandidates">Concatenated mismatch diagnostics for tried candidates.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR008Diagnostics byInfName numbPrem expectedPremise mismatchingCandidates pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR008 (byInfName, numbPrem, expectedPremise, mismatchingCandidates)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR009 warning diagnostics (e.g. "not all arguments verifiable").
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR009Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR009 // not all arguments verifiable
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)
    
/// <summary>
/// Emit PR010 diagnostic for an expected reference keyword usage error.
/// </summary>
/// <param name="keyword">Keyword that was expected.</param>
/// <param name="exptectedRef">Expected reference token.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR010Diagnostics keyword exptectedRef pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR010 (keyword, getEnglishName exptectedRef false)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR011 diagnostic for another expected reference error variant.
/// </summary>
/// <param name="keyword">Keyword involved.</param>
/// <param name="exptectedRef">Expected reference token.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR011Diagnostics keyword exptectedRef pos1 pos2 =            
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR011 (keyword, getEnglishName exptectedRef false)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR012 diagnostic for invalid provided identifiers in proofs.
/// </summary>
/// <param name="providedIdentifier">The provided identifier string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR012Diagnostics providedIdentifier pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR012 providedIdentifier
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR013 informational diagnostic.
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR013Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR013 
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR014 diagnostic (error) without parameters.
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR014Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR014 
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR015 diagnostic for missing argument ids in proofs.
/// </summary>
/// <param name="argumentID">Argument identifier string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR015Diagnostics argumentID pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR015 argumentID
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR016 diagnostic for argument ordering/assumption issues.
/// </summary>
/// <param name="argumentID">Argument identifier.</param>
/// <param name="lastAssumedArgumentId">Last assumed argument id used for context.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR016Diagnostics argumentID lastAssumedArgumentId pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR016(argumentID, lastAssumedArgumentId)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR017 diagnostic (error) without parameters.
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR017Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR017 
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR019 diagnostic when two justification types mismatch.
/// </summary>
/// <param name="justificationType1">First justification type string.</param>
/// <param name="justificationType2">Second justification type string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR019Diagnostics justificationType1 justificationType2 pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR019 (justificationType1, justificationType2)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR020 diagnostic when expected/actual premise counts differ.
/// </summary>
/// <param name="expectedNum">Expected number of premises.</param>
/// <param name="actualNum">Actual number provided.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR020Diagnostics expectedNum actualNum pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR020 (expectedNum, actualNum)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR021 diagnostic for mismatching candidates vs inferred formula for a justification.
/// </summary>
/// <param name="mismatchingCandidates">Concatenated mismatch diagnostics.</param>
/// <param name="inferredFormula">The inferred formula representation.</param>
/// <param name="justificationName">Name of the justification.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR021Diagnostics mismatchingCandidates inferredFormula justificationName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR021 (mismatchingCandidates, inferredFormula, justificationName)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit PR022 diagnostic with an explanatory reason.
/// </summary>
/// <param name="reason">Textual reason for the failure to collect preceding results.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitPR022Diagnostics reason pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = PR022 reason
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG00 diagnostic for signature arity mismatches.
/// </summary>
/// <param name="exprType">Expression/fix type name.</param>
/// <param name="expectedArity">Expected arity for the fix.</param>
/// <param name="actualArity">Actual arity found.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG00Diagnostics exprType expectedArity actualArity pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG00(exprType, actualArity, expectedArity)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG01 diagnostic for missing expression identifiers in signatures.
/// </summary>
/// <param name="expressionId">Expression identifier.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG01Diagnostics expressionId pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG01 expressionId
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG02 informational diagnostic for symbol precedence conflicts.
/// </summary>
/// <param name="symbol">Symbol name.</param>
/// <param name="precedence">Precedence value.</param>
/// <param name="conflict">Conflict description.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG02Diagnostics symbol precedence conflict pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG02(symbol, precedence, conflict)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG03 diagnostic with custom error message.
/// </summary>
/// <param name="errMsg">Error message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG03Diagnostics errMsg pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG03 errMsg
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)


/// <summary>
/// Emit SIG04 diagnostic for type/reference resolution failures inside signatures.
/// </summary>
/// <param name="mixedName">Mixed name causing the problem.</param>
/// <param name="errList">List of error details.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG04Diagnostics mixedName errList pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG04(mixedName, errList)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG05 diagnostic for generic signature errors.
/// </summary>
/// <param name="errMsg">Error message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG05Diagnostics errMsg pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG05 errMsg
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG06 warning when a signature reassignment or change is detected.
/// </summary>
/// <param name="name">Name being reassigned.</param>
/// <param name="oldFromNode">Old source node.</param>
/// <param name="newFromNode">New source node.</param>
/// <param name="typeName">Type name involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG06Diagnostics name oldFromNode newFromNode typeName pos1 pos2  = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG06(name, oldFromNode, newFromNode, typeName)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG07 warning diagnostics about assignment/typing issues.
/// </summary>
/// <param name="assigneeName">Assignee name.</param>
/// <param name="assigneeType">Assignee type string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG07diagnostics assigneeName assigneeType pos1 pos2  = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG07(assigneeName, assigneeType)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG08 diagnostic for array/index declaration problems.
/// </summary>
/// <param name="arrName">Array name.</param>
/// <param name="indexVarName">Index variable name.</param>
/// <param name="indexVarType">Index variable type string.</param>
/// <param name="dimType">Dimension type.</param>
/// <param name="dimNumber">Dimension number.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG08Diagnostics arrName indexVarName indexVarType dimType dimNumber pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG08(arrName, indexVarName, indexVarType, dimType, dimNumber)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG09 diagnostic for array/dimension mismatches.
/// </summary>
/// <param name="arrName">Array name.</param>
/// <param name="dimType">Dimension type.</param>
/// <param name="dimNumber">Dimension number.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG09Diagnostics arrName dimType dimNumber pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG09(arrName, dimType, dimNumber)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG10 diagnostic for array index out-of-range or index misuse.
/// </summary>
/// <param name="arrName">Array name.</param>
/// <param name="indexVarName">Index variable name.</param>
/// <param name="indexNumber">Index number used.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG10Diagnostics arrName indexVarName indexNumber pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG10(arrName, indexVarName, indexNumber)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG11 diagnostic for wrong qualified candidate type usage.
/// </summary>
/// <param name="qualifiedWrongCandidate">Qualified candidate string.</param>
/// <param name="typeOfCandidate">Type of the candidate.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG11Diagnostics qualifiedWrongCandidate typeOfCandidate pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG11 (qualifiedWrongCandidate, typeOfCandidate)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG12 diagnostic when a template is used multiple times (warning).
/// </summary>
/// <param name="templateName">Template name.</param>
/// <param name="secondUsage">Second usage information.</param>
/// <param name="firstUsage">First usage information.</param>
/// <param name="firstUsagePos">Position of the first usage.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG12Diagnostics templateName secondUsage firstUsage firstUsagePos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG12 (templateName, secondUsage, firstUsage, firstUsagePos)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG13 diagnostic for statement double-usage errors.
/// </summary>
/// <param name="stmtName">Statement name.</param>
/// <param name="secondUsage">Second usage information.</param>
/// <param name="firstUsage">First usage information.</param>
/// <param name="firstUsagePos">Position of the first usage.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG13Diagnostics stmtName secondUsage firstUsage firstUsagePos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG13 (stmtName, secondUsage, firstUsage, firstUsagePos)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SIG14 warning diagnostic (generic signature warning).
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSIG14Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SIG14
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ST001 informational diagnostic.
/// </summary>
/// <param name="name">Node name used in the information message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitST001Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST001 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ST002 informational diagnostic.
/// </summary>
/// <param name="name">Node name used in the information message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitST002Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST002 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ST004 warning for unsupported language codes.
/// </summary>
/// <param name="languageCode">Language code string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitST004Diagnostics languageCode pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST004 languageCode
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit ST005 informational diagnostic related to domain and node type.
/// </summary>
/// <param name="domain">Domain string.</param>
/// <param name="nodeType">Node type token.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitST005Diagnostics domain nodeType pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Information
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = ST005 (domain, getEnglishName nodeType false)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SY000 parser diagnostic (error) with a raw parser message.
/// </summary>
/// <param name="errMsg">Raw parser error message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitSY000Diagnostics errMsg pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplParser
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY000 errMsg
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit SY001 parser diagnostic (error) with a raw parser message.
/// </summary>
/// <param name="errMsg">Raw parser error message.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitSY001Diagnostics errMsg pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplParser
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY001 errMsg
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit SY002 parser diagnostic that includes a backtracking chain string.
/// </summary>
/// <param name="errMsg">Raw parser error message.</param>
/// <param name="chain">Backtracking chain or context string.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Unit.</returns>
let emitSY002Diagnostics errMsg chain pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplParser
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY002(errMsg, chain)
        }
    diagnosticsContainer.AddDiagnostic diagnostic

/// <summary>
/// Emit SY010 warning diagnostic produced by the interpreter for syntax-tolerant productions.
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSY010Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY010
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SY011 diagnostic produced by the interpreter for syntax-tolerant productions.
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSY011Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY011
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SY012 warning diagnostic produced by the interpreter for syntax-tolerant productions.
/// </summary>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSY012Diagnostics pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY012
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SY013 warning about nested infix precedence relationships.
/// </summary>
/// <param name="innerInfixSymbol">Inner infix symbol string.</param>
/// <param name="innerPrecedence">Inner precedence integer.</param>
/// <param name="outerInfixSymbol">Outer infix symbol string.</param>
/// <param name="outerPrecedence">Outer precedence integer.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSY013Diagnostics innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY013 (innerInfixSymbol, innerPrecedence, outerInfixSymbol, outerPrecedence)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit SY014 warning about infix symbol precedence conflicts.
/// </summary>
/// <param name="infixSymbol1">First infix symbol.</param>
/// <param name="infixSymbol2">Second infix symbol.</param>
/// <param name="precedence">Precedence value.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitSY014Diagnostics infixSymbol1 infixSymbol2 precedence pos1 pos2 = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = SY014 (infixSymbol1, infixSymbol2, precedence)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR00 diagnostic for variable-related errors across a span.
/// </summary>
/// <param name="startPos">Start position.</param>
/// <param name="endPos">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR00Diagnostics startPos endPos =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = startPos
            Diagnostic.EndPos = endPos
            Diagnostic.Code = VAR00
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR01 diagnostic for variable-related errors.
/// </summary>
/// <param name="name">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR01Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR01 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR02 diagnostic for variable-related errors.
/// </summary>
/// <param name="name">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR02Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR02 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR03 diagnostic when a variable conflicts with a prior declaration.
/// </summary>
/// <param name="varName">Variable identifier.</param>
/// <param name="conflictStartPos">Position where the conflicting declaration starts.</param>
/// <param name="pos1">Start position of this diagnostic.</param>
/// <param name="pos2">End position of this diagnostic.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR03Diagnostics varName conflictStartPos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR03(varName, conflictStartPos)
        }

    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR04 warning about variable usage.
/// </summary>
/// <param name="name">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR04Diagnostics name pos1 pos2 = 
    let diagnostic = { 
        Diagnostic.Uri = diagnosticsContainer.CurrentUri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Warning
        Diagnostic.StartPos = pos1
        Diagnostic.EndPos = pos2
        Diagnostic.Code = VAR04 name
    }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR05 diagnostic for variable-specific errors.
/// </summary>
/// <param name="name">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR05Diagnostics name pos1 pos2 = 
    let diagnostic = { 
        Diagnostic.Uri = diagnosticsContainer.CurrentUri
        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
        Diagnostic.Severity = DiagnosticSeverity.Error
        Diagnostic.StartPos = pos1
        Diagnostic.EndPos = pos2
        Diagnostic.Code = VAR05 name
    }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR06 warning when a variable's origin/source changed.
/// </summary>
/// <param name="name">Variable name.</param>
/// <param name="oldFromNode">Old source node identifier.</param>
/// <param name="newFromNode">New source node identifier.</param>
/// <param name="typeName">Type name involved.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR06iagnostic name oldFromNode newFromNode typeName pos1 pos2  = 
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Warning
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR06(name, oldFromNode, newFromNode, typeName)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR07 diagnostic for variable errors.
/// </summary>
/// <param name="name">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR07Diagnostics name pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR07 name
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR08 diagnostic for variable name not expected in the current scope.
/// </summary>
/// <param name="varName">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR08Diagnostics varName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR08 varName
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR09 diagnostic for variable misuse errors.
/// </summary>
/// <param name="varName">Variable name.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR09Diagnostics varName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR09 varName
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR10 diagnostic when a variable is referenced in the wrong formula context.
/// </summary>
/// <param name="varName">Variable name.</param>
/// <param name="formulaName">Formula name where the issue occurred.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR10Diagnostics varName formulaName pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR10(varName, formulaName)
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)

/// <summary>
/// Emit VAR11 diagnostic when a variable conflicts with a prior declaration, includes conflict start position.
/// </summary>
/// <param name="varName">Variable name.</param>
/// <param name="conflictStartPos">Position of the conflicting declaration.</param>
/// <param name="pos1">Start position.</param>
/// <param name="pos2">End position.</param>
/// <returns>Some diagnostic code string when emitted.</returns>
let emitVAR11Diagnostics varName conflictStartPos pos1 pos2 =
    let diagnostic =
        { 
            Diagnostic.Uri = diagnosticsContainer.CurrentUri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = pos1
            Diagnostic.EndPos = pos2
            Diagnostic.Code = VAR11(varName, conflictStartPos)
                    
        }
    diagnosticsContainer.AddDiagnostic diagnostic
    Some (diagnostic.Code.Code)
