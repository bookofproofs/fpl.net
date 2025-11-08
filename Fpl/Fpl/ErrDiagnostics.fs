module ErrDiagnostics
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Security.Cryptography
open System.Text
open FParsec
open FplPrimitives
open FplGrammarTypes



(* MIT License

Copyright (c) 2023 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

(*
This module provides some general functionality to emit error diagnostics in a custom language.

We call it `ErrDiagnostics` and not `ErrRecovery`, because it does not change the original parser's grammar that would 
be otherwise the case. Instead, it depends on synchronizing tokens defined in the particular language. In this repository,
the synchronizing tokens are defined in the module FplParser `errRecPattern` constant. Moreover, there are some
standard error messages depending on which synchronizing token was found defined in the module FplParser in `errInformation`.

We tried to keep the functions in this module as independent of the language and grammar in use as possible.
Nevertheless, some of the functions contained in this module at least partly depend on the grammar of the language 
and the way Visual Studio Code displays error diagnostics in its "Problems" window.

If you decide to reuse this code for your language and other IDEs, please consider customizing (in particular) the following functions for your environment: 
`type DiagnosticCode`, `replaceFParsecErrMsgForFplParser`, `inputStringManipulator`, `preParsePreProcess`, and `stringMatches`.

*)

type PathEquivalentUri(uriString: string) =
    inherit Uri(PathEquivalentUri.UnescapeDataString(uriString.Replace("\\","/")))

    interface IEquatable<PathEquivalentUri> with
        member this.Equals(other: PathEquivalentUri) =
            this.AbsoluteUri = other.AbsoluteUri

    override this.Equals(obJ: obj) =
        match obJ with
        | :? PathEquivalentUri as other -> (this :> IEquatable<PathEquivalentUri>).Equals(other)
        | _ -> false

    override this.GetHashCode() =
        this.AbsoluteUri.GetHashCode()

    /// An auxiliary static method for changing windows-like Uris with "\" into "/" formatted ones.
    static member EscapedUri(path:string) = 
        let pathNew = PathEquivalentUri.UnescapeDataString(path.Replace("\\","/"))
        PathEquivalentUri($"{pathNew}")

    member this.TheoryName = Path.GetFileNameWithoutExtension(this.AbsolutePath)

type DiagnosticCode = 
    // parser error codes
    | SYN000
    | SYN001
    | DEF000
    | PRP000
    | AXI000
    | THM000
    | COR000
    | LEM000
    | PPS000
    | CNJ000
    | VAR000
    | CTR000
    | PRF000
    | INF000
    | LOC000
    | EXT000
    | USE000
    | PRD000
    | AGI000
    | STMASE
    | STMASU
    | STMCAL
    | STMCAS
    | STMDEL
    | STMFOI
    | STMFOR
    | STMMAP
    | STMREV
    | STMRET
    | PRE000
    | CON000
    | TYD000
    // interpreter error codes
    | GEN00 of string
    | NSP00 of string
    | NSP01 of string * string
    | NSP02 of string * string
    | NSP03 of string
    | NSP04 of string 
    | NSP05 of string list * string * string
    // identifier-related error codes
    | ID001 of string * string
    | ID002 of string * string
    | ID003 of string
    | ID004 of string
    | ID005 of string * string
    | ID006 of string
    /// (nodeType, signatureNode, baseType, signatureBase) -> $"The {nodeType} `{signatureNode}` cannot inherit from {baseType} `{signatureBase}`."  
    | ID007 of string * string * string * string
    | ID008 of string * string
    | ID009 of string
    | ID010 of string
    | ID011 of string * string
    | ID013 of string
    | ID014 of string * string
    | ID015 of string 
    | ID016 of string 
    | ID017 of string * string 
    | ID018 of string 
    | ID019 of string 
    | ID020 of string 
    | ID021 of string 
    | ID022 of string
    | ID023 of string 
    | ID024 of string * string
    | ID025 of string * string * string
    | ID027 of string
    // logic-related error codes
    | LG001 of string * string * string * string
    | LG002 of string * int
    | LG003 of string * string
    | LG004 of string
    | LG005 of string
    // proof-related error codes
    | PR001 of string * string 
    | PR003 of string * string
    | PR004 of string * string
    | PR005 of string
    | PR006 of string * string
    | PR007 of string * string
    | PR008 of string * string * string
    | PR009 
    | PR010 of string * string 
    | PR011 of string * string
    | PR012 
    | PR013
    | PR014
    | PR015 of string
    | PR016 of string
    // signature-related error codes
    | SIG00 of string * int
    | SIG01 of string 
    | SIG02 of string * int * string
    | SIG03 of string * string 
    | SIG04 of string * int * string list
    | SIG05 of string * string
    | SIG06 of string * string * string * bool
    | SIG07 of string * string * string 
    // structure-related error codes
    | ST001 of string 
    | ST002 of string 
    // variable-related error codes
    | VAR00 
    | VAR01 of string 
    | VAR02 of string 
    | VAR03 of string * string
    | VAR04 of string 
    | VAR05 of string 
    | VAR06 of string * string * string * bool
    | VAR07 of string 
    | VAR08 
    | VAR09 of string * string 
    member this.Code = 
        match this with
            // parser error messages
            | SYN000 -> "SYN000"
            | SYN001 -> "SYN001"
            | DEF000 -> "DEF000"
            | PRP000 -> "PRP000"
            | AXI000 -> "AXI000"
            | THM000 -> "THM000"
            | COR000 -> "COR000"
            | LEM000 -> "LEM000"
            | PPS000 -> "PPS000"
            | CNJ000 -> "CNJ000"
            | VAR000 -> "VAR000"
            | CTR000 -> "CTR000"
            | PRF000 -> "PRF000"
            | INF000 -> "INF000"
            | LOC000 -> "LOC000"
            | EXT000 -> "EXT000"
            | USE000 -> "USE000"
            | PRD000 -> "PRD000"
            | STMASE -> "STMASE"
            | STMASU -> "STMASU"
            | STMCAL -> "STMCAL"
            | STMCAS -> "STMCAS"
            | STMDEL -> "STMDEL"
            | STMFOI -> "STMFOI"
            | STMFOR -> "STMFOR"
            | STMMAP -> "STMMAP"
            | STMREV -> "STMREV"
            | STMRET -> "STMRET"
            | AGI000 -> "AGI000"
            | PRE000 -> "PRE000"
            | CON000 -> "CON000"
            | TYD000 -> "TYD000"
            // interpreter error messages
            | GEN00 _ -> "GEN00"
            | NSP00 _ -> "NSP00"
            | NSP01 _ -> "NSP01"
            | NSP02 _ -> "NSP02"
            | NSP03 _ -> "NSP03"
            | NSP04 _ -> "NSP04"
            | NSP05 _ -> "NSP05"
            // identifier-related error codes 
            | ID001 _ -> "ID001"
            | ID002 _ -> "ID002"
            | ID003 _ -> "ID003"
            | ID004 _ -> "ID004"
            | ID005 _ -> "ID005"
            | ID006 _ -> "ID006"
            | ID007 _ -> "ID007"
            | ID008 _ -> "ID008"
            | ID009 _ -> "ID009"
            | ID010 _ -> "ID010"
            | ID011 _ -> "ID011"
            | ID013 _ -> "ID013"
            | ID014 _ -> "ID014"
            | ID015 _ -> "ID015"
            | ID016 _ -> "ID016"
            | ID017 _ -> "ID017"
            | ID018 _ -> "ID018"
            | ID019 _ -> "ID019"
            | ID020 _ -> "ID020"
            | ID021 _ -> "ID021"
            | ID022 _ -> "ID022"
            | ID023 _ -> "ID023"
            | ID024 _ -> "ID024"
            | ID025 _ -> "ID025"
            | ID027 _ -> "ID027"
            // logic-related error codes
            | LG001 _ -> "LG001"
            | LG002 _ -> "LG002"
            | LG003 _ -> "LG003"
            | LG004 _ -> "LG004"
            | LG005 _ -> "LG005"
            // proof-related error codes
            | PR001 _ -> "PR001"
            | PR003 _ -> "PR003"
            | PR004 _ -> "PR004"
            | PR005 _ -> "PR005"
            | PR006 _ -> "PR006"
            | PR007 _ -> "PR007"
            | PR008 _ -> "PR008"
            | PR009 -> "PR009"
            | PR010 _ -> "PR010"
            | PR011 _ -> "PR011"
            | PR012 -> "PR012"
            | PR013 -> "PR013"
            | PR014 -> "PR014"
            | PR015 _ -> "PR015"
            | PR016 _ -> "PR016"
            // signature-related error codes
            | SIG00 _ -> "SIG00"
            | SIG01 _ -> "SIG01"
            | SIG02 _ -> "SIG02"
            | SIG03 _ -> "SIG03"
            | SIG04 _ -> "SIG04"
            | SIG05 _ -> "SIG05"
            | SIG06 _ -> "SIG06"
            | SIG07 _ -> "SIG07"
            // structure-related error codes
            | ST001 _ -> "ST001"
            | ST002 _ -> "ST002"
            // variable-related error codes
            | VAR00 -> "VAR00"
            | VAR01 _  -> "VAR01"
            | VAR02 _  -> "VAR02"
            | VAR03 _  -> "VAR03"
            | VAR04 _  -> "VAR04"
            | VAR05 _  -> "VAR05"
            | VAR06 _  -> "VAR06"
            | VAR07 _  -> "VAR07"
            | VAR08 -> "VAR08"
            | VAR09 _ -> "VAR09"
    member this.Message = 
        match this with
            // parser error messages
            | SYN000 -> "Other syntax error"
            | SYN001 -> "Characters found after namespace"
            | DEF000 -> "Syntax error in definition"
            | PRP000 -> "Syntax error in property"
            | AXI000 -> "Syntax error in axiom"
            | THM000 -> "Syntax error in theorem"
            | COR000 -> "Syntax error in corollary"
            | LEM000 -> "Syntax error in lemma"
            | PPS000 -> "Syntax error in proposition"
            | CNJ000 -> "Syntax error in conjecture"
            | VAR000 -> "Syntax error in variable declaration and/or specification"
            | CTR000 -> "Syntax error in constructor"
            | PRF000 -> "Syntax error in proof"
            | INF000 -> "Syntax error in rule of inference"
            | LOC000 -> "Syntax error in localization"
            | EXT000 -> "Syntax error in extension"
            | USE000 -> "Syntax error in uses clause"
            | PRD000 -> "Syntax error in predicate definition"
            | STMASE -> "Syntax error in assertion"
            | STMASU -> "Syntax error in assumption"
            | STMCAL -> "Syntax error in call to a constructor"
            | STMCAS -> "Syntax error in cases statement"
            | STMDEL -> "Syntax error in delegate"
            | STMFOI -> "Syntax error in domain"
            | STMFOR -> "Syntax error in for statement"
            | STMMAP -> "Syntax error in mapcases statement"
            | STMREV -> "Syntax error in revocation"
            | STMRET -> "Syntax error in return statement"
            | AGI000 -> "Syntax error in proof argument"
            | PRE000 -> "Syntax error in premise"
            | CON000 -> "Syntax error in conclusion"
            | TYD000 -> "Syntax error in type declaration"
            // interpreter error messages
            | GEN00 message -> sprintf "Unexpected error occurred: %s" message
            | NSP00 fileNamePattern -> sprintf "The theory `%s` could not be found" fileNamePattern
            | NSP01 (fileName, innerErrMsg) -> sprintf "The theory `%s` was found but could not be loaded: %s" fileName innerErrMsg
            | NSP02 (url, innerErrMsg) -> sprintf "The theory `%s` was found but could not be downloaded: %s" url innerErrMsg
            | NSP03 alias -> sprintf "Alias `%s` appeared previously in this namespace" alias
            | NSP04 path -> sprintf "Circular theory reference detected: `%s`" path
            | NSP05 (pathTypes, theory, chosenSource) -> sprintf "Multiple sources %A for theory %s detected (%s was chosen)." pathTypes theory chosenSource
             // identifier-related error codes 
            | ID001 (signature, conflict) -> $"Signature `{signature}` was already declared at {conflict}."  
            | ID002 (signature, incorrectBlockType) -> $"Cannot find a block to be associated with the proof `{signature}`, found only {incorrectBlockType}."  
            | ID003 signature -> $"The proof `{signature}` is missing a block to be associated with."  
            | ID004 name -> $"Cannot assign a class `{name}` directly; use a constructor `{name}()` instead."  
            | ID005 (signature, incorrectBlockType) -> $"Cannot find a block to be associated with the corollary `{signature}`, found only {incorrectBlockType}."  
            | ID006 signature -> $"The corollary `{signature}` is missing a block to be associated with."  
            | ID007 (nodeType, signatureNode, baseType, signatureBase) -> $"The {nodeType} `{signatureNode}` cannot inherit from {baseType} `{signatureBase}`."  
            | ID008 (constructorId, classId)  -> $"Misspelled constructor name `{constructorId}`, expecting `{classId}`."  
            | ID009 name -> $"Circular base type dependency involving `{name}`." 
            | ID010 name -> $"The type `{name}` could not be found. Are you missing a uses clause?" 
            | ID011 (chain, errorMsg) -> $"The inheritance chain `{chain}` causes the following error: {errorMsg}."  
            | ID013 delegateDiagnostic -> sprintf "%s" delegateDiagnostic // just emit the delegate's diagnostic
            | ID014 (signature, conflict) -> sprintf "Language code `%s` was already declared at %s." signature conflict
            | ID015 signature -> $"`parent` cannot be referenced from {signature}." 
            | ID016 signature -> $"`self` cannot be referenced from {signature}." 
            | ID017 (name, candidates) -> 
                if candidates.Length > 0 then
                   $"The type `{name}` could not be determined, candidates were {candidates}."  
                else
                   $"The type `{name}` not found, no candidates found."  
            | ID018 name -> sprintf "The extension `%s` could not be matched. Declare an extension with this pattern." name
            | ID019 name -> sprintf "The extension `%s` could not be found. Are you missing a uses clause?" name
            | ID020 name -> $"Missing call of base constructor `{name}`." 
            | ID021 name -> $"Duplicate call of base constructor `{name}`."
            | ID022 name -> $"`{name}` is intrinsic, it has no parameterized constructors. This call uses parameters."
            | ID023 candidates  -> $"Cannot associate a justification with a single block. Found more candidates: {candidates}." 
            | ID024 (signature, conflict) -> sprintf "Expression `%s` was already localized at %s." signature conflict
            | ID025 (candidate, candidateType, nodeType)  -> $"Cannot reference to `{candidate}` which is {candidateType} inside {nodeType}." 
            | ID027 name -> $"Illegal recursion in for statement. The entity `{name}` cannot be used as its own domain." 
            // logic-related error codes
            | LG001 (typeOfPredicate,argument,typeOfExpression, evaluatedTo) -> $"Cannot evaluate `{typeOfPredicate}` because `{argument}` typed `{typeOfExpression}` was evaluated to `{evaluatedTo}`."
            | LG002 (nodeTypeName, times) -> $"Possible infinite recursion detected, `{nodeTypeName}` was called for more than {times} times.`."
            | LG003 (nodeTypeName, nodeName) -> $"`{nodeTypeName}` evaluates to `false` and cannot be {nodeName}."
            | LG004 nodeType -> $"`Parameters not allowed for {nodeType}."
            | LG005 name -> $"Unnecessary assignment of `{name}` detected (will be implicitely ignored)."
            // proof-related error codes
            | PR001 (incorrectBlockType, justificatinItemName) -> $"Cannot find a `{justificatinItemName}`, found {incorrectBlockType} instead."
            | PR003 (name, conflict) -> $"Argument identifier `{name}` was already declared at {conflict}."  
            | PR004 (name, conflict)  -> $"Justification `{name}` was already declared at {conflict}." 
            | PR005 name ->  $"Argument identifier `{name}` not declared in this proof."
            | PR006 (proofName, argumentName)->  $"A proof {proofName} was found, but there ít has no argument with the name `{argumentName}`."
            | PR007 (nodeTypeName, nodeName) ->  $"{nodeTypeName} is {nodeName} and is missing a proof."
            | PR008 (nodeName, expectedInputArgInference, actualInputArgInference) ->  $"This {nodeName} expects `{expectedInputArgInference}` and could not be applied to the proceeding argument inference which was `{actualInputArgInference}`."
            | PR009 -> "Not all arguments of the proof could be verified."
            | PR010 (keyword, expectedRef) -> $"Justification `{keyword}` expects a reference to {expectedRef}, not to a proof or corollary."
            | PR011 (keyword, expectedRef) -> $"Justification `{keyword}` expects a reference to {expectedRef}, not to an argument in some proof."
            | PR012 -> $"Justification `{LiteralByCor}` expects a reference to a corollary."
            | PR013 -> $"Add the keyword `{LiteralByCor}` when referencing to corollaries to increase readability."
            | PR014 -> "Justification expects a reference to a theorem-like statement without any more specific references."
            | PR015 argId -> $"Cannot revoke the argument `{argId}` because it wasn't assumed."
            | PR016 argId -> $"Cannot revoke the argument `{argId}` because it wasn't the last assumed one."
            // signature-related error codes
            | SIG00 (fixType, arity) -> sprintf $"Illegal arity `{arity}` using `{fixType}` notation."
            | SIG01 symbol -> $"The symbol `{symbol}` was not declared." 
            | SIG02 (symbol, precedence, conflict) -> $"The symbol `{symbol}` was declared with the same precedence of `{precedence}` in {conflict}." 
            | SIG03 (retType, mapType) -> $"The return type `{retType}` does not match the expected functional type `{mapType}`."
            | SIG04 (signature, numbOfcandidates, errorList) -> 
                if numbOfcandidates = 0 then 
                    $"No overload matching `{signature}`, no candidates were found. Are you missing a uses clause?" 
                elif numbOfcandidates = 1 then
                    let errMsg = errorList |> String.concat ", "
                    $"No overload matching `{signature}`. {errMsg}." 
                else 
                    let errMsg = errorList |> List.mapi (fun i s -> sprintf "%d. %s" (i + 1) s) |> String.concat ", "
                    $"No overload matching `{signature}`. Checked candidates: {errorList}." 
            | SIG05 (assigneeType, assignedType) -> $"Cannot assign type `{assignedType}` to type `{assigneeType}`."
            | SIG06 (name, first, second, isClass) -> 
                if isClass then
                    $"Property name `{name}` of base class `{first} will be overshadowed by `{second}`."
                else
                    $"Property name `{name}` of base functional term `{first} will be overshadowed by `{second}`."
            | SIG07 (assigneeName, assigneeType, nodeType) -> $"`{assigneeName}` is {nodeType} ({assigneeType}) and is not assignable."
            // structure-related error codes
            | ST001 nodeName -> sprintf $"The {nodeName} does nothing. Simplify the code by the block."
            | ST002 nodeName -> sprintf $"The {nodeName} does nothing. Simplify the code by removing it entirely."
            // variable-related error codes
            | VAR00 ->  sprintf "Declaring multiple variadic variables at once may cause ambiguities."
            | VAR01 name -> $"Variable `{name}` not declared in this scope."
            | VAR02 name -> $"Variable `{name}` was already bound in this quantor."
            | VAR03 (identifier, conflict) -> $"Variable `{identifier}` was already declared at {conflict}."  
            | VAR04 name -> $"Declared variable `{name}` not used in this scope."
            | VAR05 name -> $"Bound variable `{name}` was not used in this quantor."
            | VAR06 (name, first, second, isClass) -> 
                if isClass then
                    $"Variable name `{name}` of base class `{first} will be overshadowed by `{second}`."
                else
                    $"Variable name `{name}` of base functional term `{first} will be overshadowed by `{second}`."
            | VAR07 name -> $"The {PrimQuantorExistsN} accepts only one bound variable `{name}`."
            | VAR08 -> "Variadic variables cannot be bound in a quantor."
            | VAR09 (varName,varType) -> $"The variable {varName}:{varType} is free and cannot be used to evaluate this expresssion."

/// Computes an MD5 checksum of a string
let computeMD5Checksum (input: string) =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes(input)
    let hash = md5.ComputeHash(inputBytes)
    hash |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""

type DiagnosticEmitter =
    // replace your language-specific emitters here
    | FplParser
    | FplInterpreter

type DiagnosticSeverity =
    | Error
    | Warning
    | Hint
    | Information

type Diagnostic =
    {
        Uri: PathEquivalentUri
        Code: DiagnosticCode
        Emitter: DiagnosticEmitter
        Severity: DiagnosticSeverity
        StartPos: Position
        EndPos: Position
        Alternatives: string option
    }
    member this.Message = 
        let alternatives = 
            match this.Alternatives with
            | Some s ->
                s
            | None -> 
                ""
        let tranlatedMsg = 
            // improves the error message for a special case in FPL, return this.StdMsg in your case
            match this.Code with 
            | DEF000 -> 
                if alternatives.StartsWith("':'") then 
                    (this.Code.Message + " " + alternatives).Replace("Expecting:", "Missing '~' before the variable(s) or expecting:")
                else
                    this.Code.Message 
            | _ -> this.Code.Message
        if alternatives = "" then   
            tranlatedMsg
        else
            tranlatedMsg + " " + alternatives 

    member this.DiagnosticID = 
        computeMD5Checksum (sprintf "%07d" this.StartPos.Index + this.Emitter.ToString() + this.Code.Code + this.Message)

    member this.ShortForm = 
        this.Emitter.ToString() + ":" +
        this.Code.Code + ":" +
        this.Message



type Diagnostics() =
    let mutable _currentUri = new PathEquivalentUri("about:blank")
    let mutable _diagnosticsStopped = false
    let _diagnosticStorageTotal = new Dictionary<PathEquivalentUri,Dictionary<string, Diagnostic>>()
    member this.Collection = 
        _diagnosticStorageTotal
        |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
        |> Seq.sortBy (fun (uri:PathEquivalentUri,_) -> uri.AbsoluteUri)
        |> Seq.map snd
        |> Seq.concat
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.toList

    member this.DiagnosticsStopped
        with get() = _diagnosticsStopped
        and set (value) = _diagnosticsStopped <- value

    member this.CurrentUri 
        with get() = _currentUri
        and set (value) = _currentUri <- value


    member this.AddDiagnostic (d:Diagnostic) =
        if not _diagnosticsStopped then
            let keyOfd = d.DiagnosticID
            if not (_diagnosticStorageTotal.ContainsKey(d.Uri)) then
                _diagnosticStorageTotal.Add(d.Uri, new Dictionary<string, Diagnostic>())
            if not (_diagnosticStorageTotal[d.Uri].ContainsKey(keyOfd)) then
                _diagnosticStorageTotal[d.Uri].Add(keyOfd, d) |> ignore

    member this.CountDiagnostics  =
        this.Collection.Length

    member this.DiagnosticsToString = 
        _diagnosticStorageTotal
        |> Seq.collect (fun kvpOuter ->
            let a = kvpOuter.Key.AbsolutePath
            kvpOuter.Value
            |> Seq.map (fun kvpInner ->
                $"{a}: {kvpInner.Value.ShortForm}{Environment.NewLine}"
            )
        )
        |> String.concat ""

    member this.PrintDiagnostics =
        printfn "%s" this.DiagnosticsToString
        printfn "%s" "\n^------------------------^\n"

    member this.ResetStream(uri:PathEquivalentUri) =
        this.CurrentUri <- uri
        if (_diagnosticStorageTotal.ContainsKey(uri)) then
            _diagnosticStorageTotal[uri].Clear() |> ignore

    member this.Clear() = 
        _diagnosticStorageTotal.Values
        |> Seq.iter (fun dict -> dict.Clear())
        _diagnosticStorageTotal.Clear()

    member this.GetStreamDiagnostics(uri:PathEquivalentUri) =
        if (_diagnosticStorageTotal.ContainsKey(uri)) then
            _diagnosticStorageTotal[uri]
        else
            Dictionary<string, Diagnostic>()

let ad = Diagnostics()

type Interval = 
    | Interval of int * int 

    member this.Start =
        match this with
        | Interval(startIndex, _) -> startIndex    

    member this.End =
        match this with
        | Interval(_, endIndex) -> endIndex

let private _position: Parser<_, _> = fun stream -> Reply stream.Position

/// A helper replacing the FParsec error string by a string that can be better displayed in the VSCode problem window
/// For other IDEs, a change of this function might be required
let replaceFParsecErrMsgForFplParser (errMsg: string) (choices:string) (pos: Position)=
    let lines = errMsg.Split(Environment.NewLine)
    let firstLine = lines.[1]
    let caretLine = lines.[2]

    let carretPos = caretLine.IndexOf('^')
    if carretPos>=0 then
        // Extract the significant characters
        let significantCharacters =
            let search = Regex.Match(firstLine.Substring(carretPos), @"\S+").Value
            if search = "" then 
                firstLine.Trim()
            else
                search

        // Replace the significant characters with quoted version in the first line
        let quotedFirstLine = sprintf "'%s'" significantCharacters

        quotedFirstLine + Environment.NewLine + "Expecting: " + choices
    else
        errMsg

let split = [|" or "; LiteralOr + Environment.NewLine ; "or\r" ; "or "; " Other error"; Environment.NewLine + "Other error"; ", "; "," + Environment.NewLine; Environment.NewLine + Environment.NewLine; Environment.NewLine|]
let groupRegex = "(?<=Expecting: )(.+?)(?=(Expecting|(\n.+)+|$))"
let retrieveExpectedParserChoices (errMsg:string) =
    // replace accidental new lines injected by FParsec into FPL parser labels that start by "<" and end by ">"
    // to avoid them from being split apart later
    let replacement (m:Match) = m.Value.Replace(Environment.NewLine, " ")
    let errMsgMod = Regex.Replace(errMsg, "(?<=<)[^>]*", MatchEvaluator(replacement))
    // now, look in this manipulated error FParsec message by looking for groups 
    // starting with "Expecting: " and followed by an enumeration of choices
    let regex = Regex(groupRegex)
    let matches = regex.Matches(errMsgMod)
    // now, we extract all choices so they contain only those parts of the matched group that
    // are choice enumerations relevant for us.
    let hashSet = System.Collections.Generic.HashSet<string>()
    for m in matches do
        for g in m.Groups do
            let s = g.Value.Split(split, System.StringSplitOptions.RemoveEmptyEntries)
            let trimmedS = 
                s 
                |> Array.map (fun str -> str.Trim() ) // trim all strings
                |> Array.filter (fun str -> str.Length > 0) // ignore all empty strings
                |> Array.map (fun str -> if str.EndsWith(",") then str.Substring(0, str.Length - 1) else str) // remove incidental trailing commas 
                |> Array.filter (fun str -> (str.StartsWith("<") && str.EndsWith(">")) || (str.StartsWith("'") && str.EndsWith("'"))) // filter out all split elements that are not quoted or start with "<"
            let hs = System.Collections.Generic.HashSet<string>(trimmedS)
            hashSet.UnionWith(hs) |> ignore
    // return the choices as a comma-separated list of alphabetically sorted choices
    let choices = 
        hashSet
        |> Seq.toList
        |> List.sort
    choices


let getLineOffset (input: string) (line:int)=
    let lines = input.Split(Environment.NewLine)
    let lengthLineSep = Environment.NewLine.Length
    let mutable offset = 0
    for i in 0..(line - 2) do
        if i<lines.Length then
            offset <- offset + lines.[i].Length + lengthLineSep
    offset

let getLineAndColumn (input: string) =
    let regex = System.Text.RegularExpressions.Regex("Error in Ln: (\\d+) Col: (\\d+)")
    let m = regex.Match(input)
    if m.Success then
        let line = int m.Groups.[1].Value
        let column = int m.Groups.[2].Value
        Some(line, column)
    else
        None

let getLastSubstringAfterSeparator (input:string) (sep:string) =
    let substrings = input.Split(sep)
    if substrings.Length > 0 then
        substrings.[substrings.Length - 1].Trim()
    else
        ""

/// If the parser's error message contains a FParsec backtracking message, this function will 
/// correct the error position of the error to that of the backtracking error and also extract the 
/// backtracking error message, ignoring the more global FParsec's error message.
/// We need this function to make the error diagnostics more intuitive.
let extractBacktrackingFreeErrMsgAndPos (input: string) (errMsg: string) (pos:Position) =
    let backtrackingFreeErrMsg = getLastSubstringAfterSeparator errMsg "backtracked after:"
    let lineColumn = getLineAndColumn backtrackingFreeErrMsg
    match lineColumn with
    | Some(line, column) -> 
        let index = column + getLineOffset input line - 1
        let backtrackingFreePos = Position(
                                    "",
                                    index,
                                    line,
                                    column 
                                )
        (backtrackingFreeErrMsg, backtrackingFreePos)
    | None ->
        (errMsg, pos)

/// A low-level error recovery function mapping error messages to tuples
/// consisting of some parser choices, a pick from this choices and a replace
/// error message that is formatted suitable for the VS Code Problems window.
let mapErrMsgToRecText (input: string) (errMsg: string) (pos:Position) =
    // extract from errMsg only this part that is contained in the last 
    // part of the FParsec error message after any before FParsec started any 
    // backtracking to find another syntax error. We consider those
    // "other syntax errors" irrelevant for a correct error recovery process 
    // because we want to report only those errors that the user 
    // is most likely to handle while typing FPL source code.
    let backtrackingFreeErrMsg,  backtrackingFreePos = extractBacktrackingFreeErrMsgAndPos input errMsg pos 

    let choices = retrieveExpectedParserChoices backtrackingFreeErrMsg
    let newErrMsg = replaceFParsecErrMsgForFplParser backtrackingFreeErrMsg (String.concat ", " choices) backtrackingFreePos
    newErrMsg, choices

/// Replaces in the `input` all starting non-whitespace characters by as many spaces 
let replaceFirstNonWhitespace str =
    let regex = new Regex("\\S+")
    let replacement = MatchEvaluator(fun m -> String.replicate m.Value.Length " ")
    regex.Replace(str, replacement, 1)


let inputStringManipulator (input:string) errorIndex = 
    if errorIndex > 0 then 
        let characterBeforeError = input.Substring(errorIndex-1,1)
        if characterBeforeError = "." then
            let preErrorString = input.Substring(0, errorIndex - 1) // strip the point at the end
            let postErrorString = input.Substring(errorIndex) // and do not change the remainder of the string
            let newInput = preErrorString + postErrorString
            newInput
        else
            let preErrorString = input.Substring(0, errorIndex)
            let postErrorString = replaceFirstNonWhitespace (input.Substring(errorIndex)) // strip the starting non-whitespace characters from the string
            let newInput = preErrorString + postErrorString
            newInput
    else
        replaceFirstNonWhitespace (input)

/// If the source code is not syntax-error-free, this function will find the first error and emit it.
let tryParseFirstError someParser input (code:DiagnosticCode) =
   
    match run someParser input with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        result, (int userState.Index)
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, _ = mapErrMsgToRecText input errorMsg restInput.Position
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplParser 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = restInput.Position
                Diagnostic.EndPos = restInput.Position
                Diagnostic.Code = code
                Diagnostic.Alternatives = Some newErrMsg
            }
        ad.AddDiagnostic diagnostic

        Ast.Error, int restInput.Position.Index

/// This function emits diagnostics for someParser if it fails and tries to do so consuming more and more input by replacing the error
/// position with spaces as long as the parser reaches a position where another parser should be 
/// applied according to the applied synchronizing tokens. The recursive call stop also in rare cases, 
/// in which this strategy would fail because the error occurs at the same position in consecutive recursive calls of the function.
let rec tryParse someParser input startIndexOfInput nextIndex (code:DiagnosticCode) lastCorrectedIndex lastChoices =
   
    match run someParser input with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        result, (int userState.Index + startIndexOfInput), true
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
        let previousChoices = String.concat ", " choices
        // calculate the index in the original input because the error index points to the input that might be a 
        // substring of the original input
        let correctedIndex = int restInput.Position.Index + startIndexOfInput
        // since we call the function with lastCorrectedIndex=-1 that is impossible, the following condition checks if 
        // the recursive call have had altered the corrected position, if not, we have to break the recursion
        // since in this case, the error cannot be changed by removing first non-white characters from the remaining input
        let cond0 = lastCorrectedIndex <> correctedIndex 
        // the index of the error must not exceed the next index. If it does, we have to break the recursion
        let cond1 = correctedIndex < nextIndex
        let cond2 = previousChoices <> lastChoices
        let cond = cond0 && cond1 && cond2 
        if cond then
            let correctedPosition = 
                Position(
                    "",
                    correctedIndex,
                    restInput.Position.Line,
                    restInput.Position.Column 
                )
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplParser 
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = correctedPosition
                    Diagnostic.EndPos = correctedPosition
                    Diagnostic.Code = code                    
                    Diagnostic.Alternatives = Some newErrMsg
                }
            ad.AddDiagnostic diagnostic
            
            // replace the input by manipulating the input string depending on the error position
            let newInput = inputStringManipulator input (int restInput.Position.Index)
            // emit further diagnostics recursively for this manipulated input, as long as the recursion breaking conditions cond0 
            // and cond1 are still met.
            tryParse someParser newInput startIndexOfInput nextIndex code correctedIndex previousChoices
        else
            // We return -1 if in the first recursive call the error position did not met the conditions cond0 and cond1
            // Otherwise, we return an error position of the previous error in the recursive call that still 
            // did satisfy the conditions cond0 and cond1. These conditions don't have to be met by the current error position.
            Ast.Error, lastCorrectedIndex, false


/// This function emits diagnostics for chunks of input between the end of parsing result of someParser and the starting index
/// where another parser should be applied according to the applied synchronizing tokens.
let rec tryParseRemainingChunk someParser (input:string) startIndexOfInput nextIndex (code:DiagnosticCode) lastCorrectedIndex lastChoices =
    
    if input.Trim() <> "" then
         match run someParser input with
            | Success(result, restInput, userState) -> 
                let correctedIndex = userState.Index + startIndexOfInput
                // replace the input by manipulating the input string depending on the parser position
                let newInput = inputStringManipulator input (int userState.Index)
                if input = newInput then
                    () // end the recursion, since we have probably an infinite loop
                else
                    if userState.Index < input.Length then
                        tryParseRemainingChunk someParser newInput startIndexOfInput nextIndex code correctedIndex ""
            | Failure(errorMsg, restInput, userState) ->
                let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
                let previousChoices = String.concat ", " choices
                // calculate the index in the original input because the error index points to the input that might be a 
                // substring of the original input
                let correctedIndex = restInput.Position.Index + startIndexOfInput
                if correctedIndex < nextIndex && previousChoices<>lastChoices then 
                    let correctedPosition = 
                        Position(
                            "",
                            correctedIndex,
                            restInput.Position.Line,
                            restInput.Position.Column 
                        )
                    let diagnostic =
                        { 
                            Diagnostic.Uri = ad.CurrentUri
                            Diagnostic.Emitter = DiagnosticEmitter.FplParser 
                            Diagnostic.Severity = DiagnosticSeverity.Error
                            Diagnostic.StartPos = correctedPosition
                            Diagnostic.EndPos = correctedPosition
                            Diagnostic.Code = code 
                            Diagnostic.Alternatives = Some newErrMsg
                        }
                    ad.AddDiagnostic diagnostic
            
                    // replace the input by manipulating the input string depending on the error position
                    let newInput = inputStringManipulator input (int restInput.Position.Index)
                    // emit further diagnostics recursively for this manipulated input, as long as the recursion breaking conditions cond0 
                    // and cond1 are still met.
                    if restInput.Position.Index < input.Length && lastCorrectedIndex <> correctedIndex then
                        tryParseRemainingChunk someParser newInput startIndexOfInput nextIndex code correctedIndex previousChoices
        

let isNotInAnyInterval (intervals: System.Collections.Generic.List<Interval>) num = 
    intervals.TrueForAll(
        fun interval -> 
            match interval with
            | Interval(s,e) -> num < s || num >= e
        )


/// Emits diagnostics for the error positions (if any) that are not overlapped by the intervals
let rec tryParseRemainingOnly someParser input (code:DiagnosticCode) (intervals:System.Collections.Generic.List<Interval>) lastCorrectedIndex lastChoices =
   
    match run someParser input with
    | Success(result, restInput, userState) -> 
        // In the success case, we still try to parse further, if the string is longer than the successfully parsed string
        if userState.Index < input.Length then
            // replace the input by manipulating the input string depending on the parser position
            let newInput = inputStringManipulator input (int userState.Index)
            // emit further diagnostics recursively for this manipulated input, as long as the recursion breaking conditions 
            // are still met.
            tryParseRemainingOnly someParser newInput code intervals userState.Index ""
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
        let previousChoices = (String.concat ", " choices)
        let stringBetweenRecursiveCalls = 
            if restInput.Position.Index> lastCorrectedIndex && lastCorrectedIndex >= 0  then
                input.Substring(int lastCorrectedIndex, int restInput.Position.Index - int lastCorrectedIndex)
            else
                "#"
        let cond = previousChoices<>lastChoices || stringBetweenRecursiveCalls.Contains(Environment.NewLine)
        if isNotInAnyInterval intervals (int restInput.Position.Index) then
            if cond then
                let diagnostic =
                    { 
                        Diagnostic.Uri = ad.CurrentUri
                        Diagnostic.Emitter = DiagnosticEmitter.FplParser 
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = restInput.Position
                        Diagnostic.EndPos = restInput.Position
                        Diagnostic.Code = code 
                        Diagnostic.Alternatives = Some newErrMsg
                    }
                ad.AddDiagnostic diagnostic
            
        if restInput.Position.Index < input.Length && restInput.Position.Index <> lastCorrectedIndex && cond then
            // replace the input by manipulating the input string depending on the error position
            let newInput = inputStringManipulator input (int restInput.Position.Index)
            // emit further diagnostics recursively for this manipulated input, as long as the recursion breaking conditions are still met.
            tryParseRemainingOnly someParser newInput code intervals restInput.Position.Index previousChoices

/// Generate an ast on a best-effort basis, no matter if there are syntax errors, without emitting any diagnostics
let rec tryGetAst someParser input lastCorrectedIndex =
    match run someParser input with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        result
    | Failure(errorMsg, restInput, userState) ->
        // replace the input by manipulating the input string depending on the error position
        let newInput = inputStringManipulator input (int restInput.Position.Index)
        if restInput.Position.Index < input.Length && restInput.Position.Index <> lastCorrectedIndex then 
            tryGetAst someParser newInput restInput.Position.Index
        else
            // only if the error occurs at the end of the input, the ast generation fails
            Ast.Error 


/// A simple helper function for printing trace information to the console (taken from FParsec Docs)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply


/// Taken from https://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html#parser-predicates
let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
      let state = stream.State
      let reply = p stream
      if reply.Status <> Ok || predicate reply.Result then reply
      else
          stream.BacktrackTo(state) // backtrack to beginning
          Reply(Primitives.Error, error)

/// Replaces all comments and strings in a FPL source code by spaces with as many line breaks as in the replacements.
/// This significantly simplifies the grammar because we do not have to parse comments and inject them everywhere in the grammar the could be possible.
/// The string replacement, on the other hand, prevents false positives in the error recovery mechanism in user-defined 
/// localization strings that would otherwise emit diagnostics for regex matches inside those user-defined strings.
let preParsePreProcess (input:string) = 
    input
    |> removeFplComments 


/// Returns a list of tuples with (position,regexMmatch) of string matches based on an error-recovery regex pattern 
/// The pattern has to start with non-whitespace characters (e.g. keywords or other strings that are distinctive for the language)
/// we want to provide with emitting error recovery messages
/// The list will be filtered to include only those matches that really start with that pattern, i.e. are proceeded some whitespace character 
/// in the remaining source code. For instance, if LiteralFor is the pattern than " for" will match, but "_for" will not.

let stringMatches (input: string) (pattern: string) =
    let regex = new Regex(pattern)
    let matches = regex.Matches(input)
    let list = new List<int>()
    list.Add(0)
    for m in matches do
        let character = input.Substring(m.Index, 1)
        if Regex.IsMatch(character, "\w") then
            // If the matched pattern starts with a word-character, 
            // filter only those matches that have a non-word character before the matched pattern.
            // E.g., the match 'lem' will be matched, if found in ' lem' or ',lem' but not if found in 'tplFieldElem' or 'elem'
            let preCharacter = 
                if m.Index > 0 then
                    input.Substring(m.Index-1, 1)
                else
                    "#"
            if Regex.IsMatch(preCharacter, "\W") then
                list.Add(m.Index)
        else
            // If the matched pattern starts with a non-word-character, 
            // filter only those matches that have a whitespace character before the matched pattern.
            // E.g., the match '|' will added, if found in ' |' or '\n|' but not if found in 'n|' or ',|'
            let preCharacter = 
                if m.Index > 0 then
                    input.Substring(m.Index-1, 1)
                else
                    " "
            if Regex.IsMatch(preCharacter, "\s") then
                list.Add(m.Index)
    list.Add(input.Length)
    list

/// A helper function for checking if a string starts with any of some string prefixes.
let startsWithAny (prefixes:string list) (input:string) = 
    prefixes |> List.exists input.StartsWith

/// A helper function adding an English article to a string 
let getEnglishName someString = 
    let isEnglishAn = startsWithAny ["a"; "e"; "i"; "o"; "u"] someString
    if isEnglishAn then 
        $"an {someString}"
    else
        $"a {someString}"
