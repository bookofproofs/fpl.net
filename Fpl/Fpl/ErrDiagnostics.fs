(*
This module provides some functionality to emit error diagnostics in a custom language.

*)
(* MIT License

Copyright (c) 2023 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

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
open ErrMessages

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

/// A helper function for checking if a string starts with any of some string prefixes.
let startsWithAny (prefixes:string list) (input:string) = 
    prefixes |> List.exists input.StartsWith

/// A helper function adding an English article to a string 
let getEnglishName someString determined = 
    let isEnglishAn = startsWithAny ["a"; "e"; "i"; "o"; "u"] someString
    if determined then 
        $"the {someString}"
    elif isEnglishAn then 
        $"an {someString}"
    else
        $"a {someString}"

type DiagnosticCode = 
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
    | ID005 of string * string
    | ID006 of string
    /// (nodeType, signatureNode, baseType, signatureBase) -> $"The {nodeType} `{signatureNode}` cannot inherit from {baseType} `{signatureBase}`."  
    | ID007 of string * string * string * string
    | ID008 of string * string
    | ID009 of string
    | ID010 of string
    | ID011 of string * string
    | ID012 of string * string * string * string
    | ID013 of string
    | ID014 of string * string
    | ID015 of string 
    | ID016 of string 
    | ID017 of string * string 
    | ID018 of string 
    | ID020 of string 
    | ID021 of string 
    | ID022 of string
    | ID023 of string 
    | ID024 of string * string
    | ID025 of string * string
    | ID027 of string
    // logic-related error codes
    | LG001 of string * string * string
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
    | PR008 of string * int * string * string
    | PR009 
    | PR010 of string * string 
    | PR011 of string * string
    | PR012 
    | PR013
    | PR014
    | PR015 of string
    | PR016 of string
    | PR017
    | PR018
    | PR019 of string * string
    | PR020 of int * int
    | PR021 of string * string
    // signature-related error codes
    | SIG00 of string * int
    | SIG01 of string 
    | SIG02 of string * int * string
    | SIG03 of string 
    | SIG04 of string * string
    | SIG05 of string 
    | SIG06 of string * string * string * string
    | SIG07 of string * string * string 
    | SIG08 of string * string * string * string * int
    | SIG09 of string * string * int
    | SIG10 of string * string * int 
    | SIG11 of string 
    | SIG12 of string * string * string * string
    | SIG13 of string * string * string * string 
    | SIG14 
    // structure-related error codes
    | ST001 of string 
    | ST002 of string 
    | ST004 of string
    | ST005 of string * string
    // interpreter syntax-related error codes for error-tolerant parser productions
    | SY000 of string
    | SY001 
    | SY002 
    | SY003 
    | SY004
    | SY005
    | SY006
    | SY007
    | SY008
    | SY009
    | SY010
    | SY011
    | SY999 of string
    // variable-related error codes
    | VAR00 
    | VAR01 of string 
    | VAR02 of string 
    | VAR03 of string * string
    | VAR04 of string 
    | VAR05 of string 
    | VAR06 of string * string * string * string
    | VAR07 of string 
    | VAR08 
    | VAR09 of string * string 
    | VAR10 of string * string
    | VAR11 of string * string
    member this.Code = 
        match this with
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
            | ID005 _ -> "ID005"
            | ID006 _ -> "ID006"
            | ID007 _ -> "ID007"
            | ID008 _ -> "ID008"
            | ID009 _ -> "ID009"
            | ID010 _ -> "ID010"
            | ID011 _ -> "ID011"
            | ID012 _ -> "ID012"
            | ID013 _ -> "ID013"
            | ID014 _ -> "ID014"
            | ID015 _ -> "ID015"
            | ID016 _ -> "ID016"
            | ID017 _ -> "ID017"
            | ID018 _ -> "ID018"
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
            | PR017 -> "PR017"
            | PR018 -> "PR018"
            | PR019 _ -> "PR019"
            | PR020 _ -> "PR020"
            | PR021 _ -> "PR021"
            // signature-related error codes
            | SIG00 _ -> "SIG00"
            | SIG01 _ -> "SIG01"
            | SIG02 _ -> "SIG02"
            | SIG03 _ -> "SIG03"
            | SIG04 _ -> "SIG04"
            | SIG05 _ -> "SIG05"
            | SIG06 _ -> "SIG06"
            | SIG07 _ -> "SIG07"
            | SIG08 _ -> "SIG08"
            | SIG09 _ -> "SIG09"
            | SIG10 _ -> "SIG10"
            | SIG11 _ -> "SIG11"
            | SIG12 _ -> "SIG12"
            | SIG13 _ -> "SIG13"
            | SIG14 -> "SIG14"
            // structure-related error codes
            | ST001 _ -> "ST001"
            | ST002 _ -> "ST002"
            | ST004 _ -> "ST004"
            | ST005 _ -> "ST005"
            // interpreter syntax-related error codes for error-tolerant parser
            | SY000 _ -> "SY000"
            | SY001 -> "SY001"
            | SY002 -> "SY002"
            | SY003 -> "SY003"
            | SY004 -> "SY004"
            | SY005 -> "SY005"
            | SY006 -> "SY006"
            | SY007 -> "SY007"
            | SY008 -> "SY008"
            | SY009 -> "SY009"
            | SY010 -> "SY010"
            | SY011 -> "SY011"
            | SY999 _ -> "SY999"
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
            | VAR10 _ -> "VAR10"
            | VAR11 _ -> "VAR11"
    member this.Message = 
        match this with
            // interpreter error messages
            | GEN00 message -> errGEN00 message
            | NSP00 fileNamePattern -> errNSP00 fileNamePattern
            | NSP01 (fileName, innerErrMsg) -> errNSP01 fileName innerErrMsg
            | NSP02 (url, innerErrMsg) -> errNSP02 url innerErrMsg
            | NSP03 alias -> errNSP03 alias
            | NSP04 path -> errNSP04 path
            | NSP05 (pathTypes, theory, chosenSource) -> errNSP05 pathTypes theory chosenSource
             // identifier-related error codes 
            | ID001 (signature, conflict) -> errID001 signature conflict 
            | ID002 (signature, incorrectBlockType) -> errID002 signature incorrectBlockType 
            | ID003 signature -> errID003 signature
            | ID005 (signature, incorrectBlockType) -> errID005 signature incorrectBlockType
            | ID006 signature -> errID006 signature  
            | ID007 (nodeType, signatureNode, baseType, signatureBase) -> errID007 nodeType signatureNode baseType signatureBase 
            | ID008 (constructorId, classId)  -> errID008 constructorId classId
            | ID009 name -> errID009 name 
            | ID010 name -> errID010 name 
            | ID011 (chain, errorMsg) -> errID011 chain errorMsg
            | ID012 (prtyName, varName, varType, candidates) -> errID012 prtyName varName varType candidates
            | ID013 delegateDiagnostic -> errID013 delegateDiagnostic
            | ID014 (signature, conflict) -> errID014 signature conflict
            | ID015 signature -> errID015 signature
            | ID016 signature -> errID016 signature
            | ID017 (name, candidates) -> errID017 name candidates
            | ID018 name -> errID018 name
            | ID020 name -> errID020 name 
            | ID021 name -> errID021 name
            | ID022 name -> errID022 name
            | ID023 candidates  -> errID023 candidates
            | ID024 (signature, conflict) -> errID024 signature conflict
            | ID025 (candidate, nodeType)  -> errID025 candidate nodeType
            | ID027 name -> errID027 name
            // logic-related error codes
            | LG001 (typeOfPredicate,argument,typeOfExpression) -> errLG001 typeOfPredicate argument typeOfExpression
            | LG002 (nodeTypeName, times) -> errLG002 nodeTypeName times
            | LG003 (nodeTypeName, nodeName) -> errLG003 nodeTypeName nodeName 
            | LG004 nodeType -> errLG004 nodeType 
            | LG005 name -> errLG005 name
            // proof-related error codes
            | PR001 (incorrectBlockType, justificatinItemName) -> errPR001 incorrectBlockType justificatinItemName
            | PR003 (name, conflict) -> errPR003 name conflict
            | PR004 (name, conflict)  -> errPR004 name conflict
            | PR005 name ->  errPR005 name
            | PR006 (proofName, argumentName) -> errPR006 proofName argumentName
            | PR007 (nodeTypeName, nodeName) -> errPR007 nodeTypeName nodeName
            | PR008 (byInfName, numbPrem, expectedPremise, mismatchingCandidates) -> errPR008 byInfName numbPrem expectedPremise mismatchingCandidates
            | PR009 -> errPR009 
            | PR010 (keyword, expectedRef) -> errPR010 keyword expectedRef
            | PR011 (keyword, expectedRef) -> errPR011 keyword expectedRef
            | PR012 -> errPR012
            | PR013 -> errPR013
            | PR014 -> errPR014
            | PR015 argId -> errPR015 argId
            | PR016 argId -> errPR016 argId
            | PR017 -> errPR017
            | PR018 -> errPR018
            | PR019 (justificationType1, justificationType2) -> errPR019 justificationType1 justificationType2
            | PR020 (expectedNum, actualNum) -> errPR020 expectedNum actualNum
            | PR021 (expectedFormula, foundFormula) -> errPR021 expectedFormula foundFormula
            // signature-related error codes
            | SIG00 (fixType, arity) -> errSIG00 fixType arity
            | SIG01 symbol -> errSIG01 symbol
            | SIG02 (symbol, precedence, conflict) -> errSIG02 symbol precedence conflict
            | SIG03 errMsg -> errSIG03 errMsg
            | SIG04 (signature, candidates) -> errSIG04 signature candidates
            | SIG05 errMsg -> errSIG05 errMsg
            | SIG06 (name, oldFromNode, newFromNode, typeName) -> errSIG06 name oldFromNode newFromNode typeName
            | SIG07 (assigneeName, assigneeType, nodeType) -> errSIG07 assigneeName assigneeType nodeType
            | SIG08 (arrName, indexVarName, indexVarType, dimType, dimNumber) -> errSIG08 arrName indexVarName indexVarType dimType dimNumber
            | SIG09 (arrName, dimType, dimNumber) -> errSIG09 arrName dimType dimNumber
            | SIG10 (arrName, indexVarName, indexNumber) -> errSIG10 arrName indexVarName indexNumber
            | SIG11 qualifiedWrongCandidate -> errSIG11 qualifiedWrongCandidate
            | SIG12 (templateName, secondUsage, firstUsage, firstUsagePos) -> errSIG12 templateName secondUsage firstUsage firstUsagePos
            | SIG13 (stmtName, secondUsage, firstUsage, firstUsagePos) -> errSIG13 stmtName secondUsage firstUsage firstUsagePos
            | SIG14 -> errSIG14
            // structure-related error codes
            | ST001 nodeName -> errST001 nodeName
            | ST002 nodeName -> errST002 nodeName
            | ST004 langCode -> errST004 langCode
            | ST005 (domain, nodeType) -> errST005 domain nodeType
            // interpreter syntax-related error codes for error-tolerant parser
            | SY000 infixOp -> errSY000 infixOp
            | SY001 -> errSY001
            | SY002 -> errSY002
            | SY003 -> errSY003
            | SY004 -> errSY004
            | SY005 -> errSY005
            | SY006 -> errSY006
            | SY007 -> errSY007
            | SY008 -> errSY008
            | SY009 -> errSY009
            | SY010 -> errSY010
            | SY011 -> errSY011
            | SY999 errMsg -> errSY999 errMsg
            // variable-related error codes
            | VAR00 -> errVAR00
            | VAR01 name -> errVAR01 name
            | VAR02 name -> errVAR02 name
            | VAR03 (identifier, conflict) -> errVAR03 identifier conflict
            | VAR04 name -> errVAR04 name
            | VAR05 name -> errVAR05 name
            | VAR06 (name, oldFromNode, newFromNode, typeName) -> errVAR06 name oldFromNode newFromNode typeName
            | VAR07 name -> errVAR07 name
            | VAR08 -> errVAR08
            | VAR09 (varName,varType) -> errVAR09 varName varType
            | VAR10 (identifier, formulaName) -> errVAR10 identifier formulaName
            | VAR11 (identifier, conflict) -> errVAR11 identifier conflict

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
        if alternatives = "" then   
            this.Code.Message
        else
            this.Code.Message + " " + alternatives 

    member this.DiagnosticID = 
        computeMD5Checksum (sprintf "%07d" this.StartPos.Index + this.Emitter.ToString() + this.Code.Code + this.Message)

    member this.ShortForm = 
        this.Emitter.ToString() + ":" +
        this.Code.Code + ":" +
        this.Message



type Diagnostics() =
    let mutable _currentUri = new PathEquivalentUri("about:blank")
    let _diagnosticStorageTotal = new Dictionary<PathEquivalentUri,Dictionary<string, Diagnostic>>()
    member this.Collection = 
        _diagnosticStorageTotal
        |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
        |> Seq.sortBy (fun (uri:PathEquivalentUri,_) -> uri.AbsoluteUri)
        |> Seq.map snd
        |> Seq.concat
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.toList

    member this.CurrentUri 
        with get() = _currentUri
        and set (value) = _currentUri <- value

    member this.AddDiagnostic (d:Diagnostic) =
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


let private getLineOffset (input: string) (line:int)=
    let lines = input.Split(Environment.NewLine)
    let lengthLineSep = Environment.NewLine.Length
    let mutable offset = 0
    for i in 0..(line - 2) do
        if i<lines.Length then
            offset <- offset + lines.[i].Length + lengthLineSep
    offset

let private getLineAndColumn (input: string) =
    let regex = System.Text.RegularExpressions.Regex("Error in Ln: (\\d+) Col: (\\d+)")
    let m = regex.Match(input)
    if m.Success then
        let line = int m.Groups.[1].Value
        let column = int m.Groups.[2].Value
        Some(line, column)
    else
        None

let private getLastSubstringAfterSeparator (input:string) (sep:string) =
    let substrings = input.Split(sep)
    if substrings.Length > 0 then
        substrings.[substrings.Length - 1].Trim()
    else
        ""

/// If the parser's error message contains a FParsec backtracking message, this function will 
/// correct the error position of the error to that of the backtracking error and also extract the 
/// backtracking error message, ignoring the more global FParsec's error message.
/// We need this function to make the error diagnostics more intuitive.
let private extractBacktrackingFreeErrMsgAndPos (input: string) (errMsg: string) (pos:Position) =
    let backtrackingFreeErrMsg = getLastSubstringAfterSeparator errMsg "backtracked after:"
    let lineColumn = getLineAndColumn backtrackingFreeErrMsg
    match lineColumn with
    | Some(line, column) -> 
        let index = column + getLineOffset input line - 1
        let backtrackingFreePos = Position("",index,line,column)
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






