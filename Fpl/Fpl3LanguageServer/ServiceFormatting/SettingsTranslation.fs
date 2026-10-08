/// <summary>
/// Translates raw client configuration payloads (<c>JToken</c>/<c>JObject</c>, as received via
/// <c>workspace/didChangeConfiguration</c> or a <c>workspace/configuration</c> response) into a
/// <see cref="Fpl1Parser.LSRelated.FormattingOptions.FormattingOptions"/> value.
/// </summary>
/// <remarks>
/// This is the only module allowed to reference both <c>Newtonsoft.Json.Linq</c> and
/// <c>Fpl1Parser.LSRelated.FormattingOptions</c> together. <c>Fpl1Parser</c> itself must never
/// gain a dependency on JSON types, so all enum-string/int parsing (with graceful fallback to
/// defaults on missing/malformed values) lives here.
/// </remarks>
module Fpl3LanguageServer.ServiceFormatting.SettingsTranslation

open Newtonsoft.Json.Linq
open Fpl1Parser.LSRelated.FormattingOptions

/// <summary>
/// Reads the string value of a given key from a <see cref="JToken"/>, if the token is a
/// <see cref="JObject"/> and the key is present and holds a string.
/// </summary>
let private tryGetString (json: JToken) (key: string) : string option =
    match json with
    | :? JObject as obj ->
        match obj.TryGetValue(key) with
        | true, value when value.Type = JTokenType.String -> Some(value.Value<string>())
        | _ -> None
    | _ -> None

/// <summary>
/// Reads the integer value of a given key from a <see cref="JToken"/>, if the token is a
/// <see cref="JObject"/> and the key is present and holds a number.
/// </summary>
let private tryGetInt (json: JToken) (key: string) : int option =
    match json with
    | :? JObject as obj ->
        match obj.TryGetValue(key) with
        | true, value when value.Type = JTokenType.Integer || value.Type = JTokenType.Float ->
            Some(value.Value<int>())
        | _ -> None
    | _ -> None

/// <summary>
/// Reads the boolean value of a given key from a <see cref="JToken"/>, if the token is a
/// <see cref="JObject"/> and the key is present and holds a boolean.
/// </summary>
let private tryGetBool (json: JToken) (key: string) : bool option =
    match json with
    | :? JObject as obj ->
        match obj.TryGetValue(key) with
        | true, value when value.Type = JTokenType.Boolean -> Some(value.Value<bool>())
        | _ -> None
    | _ -> None

let private parseOpeningStyle (defaultValue: OpeningStyle) (json: JToken) (key: string) : OpeningStyle =
    match tryGetString json key with
    | Some "auto" -> OpeningStyle.Auto
    | Some "oneLiner" -> OpeningStyle.OneLiner
    | Some "egyptian" -> OpeningStyle.Egyptian
    | Some "allman" -> OpeningStyle.Allman
    | _ -> defaultValue

let private parseCommaStyle (defaultValue: CommaStyle) (json: JToken) (key: string) : CommaStyle =
    match tryGetString json key with
    | Some "auto" -> CommaStyle.Auto
    | Some "oneLiner" -> CommaStyle.OneLiner
    | Some "trailing" -> CommaStyle.Trailing
    | Some "leading" -> CommaStyle.Leading
    | _ -> defaultValue

let private parseOptionYesNo (defaultValue: OptionYesNo) (json: JToken) (key: string) : OptionYesNo =
    match tryGetBool json key with
    | Some true -> OptionYesNo.Yes
    | Some false -> OptionYesNo.No
    | None -> defaultValue

let private parseKeywordLength (defaultValue: KeywordLength) (json: JToken) (key: string) : KeywordLength =
    match tryGetString json key with
    | Some "long" -> KeywordLength.Long
    | Some "short" -> KeywordLength.Short
    | _ -> defaultValue

let private parseNotation (defaultValue: Notation) (json: JToken) (key: string) : Notation =
    match tryGetString json key with
    | Some "keyword" -> Notation.Keyword
    | Some "symbol" -> Notation.Symbol
    | _ -> defaultValue

let private parseIsOpStyle (defaultValue: IsOpStyle) (json: JToken) (key: string) : IsOpStyle =
    match tryGetString json key with
    | Some "polish" -> IsOpStyle.Polish
    | Some "infix" -> IsOpStyle.Infix
    | _ -> defaultValue

let private parseBlockStyle (defaultValue: BlockStyle) (json: JToken) (key: string) : BlockStyle =
    match tryGetString json key with
    | Some "compact" -> BlockStyle.Compact
    | Some "enclosing" -> BlockStyle.Enclosing
    | _ -> defaultValue

let private parseInt (defaultValue: int) (json: JToken) (key: string) : int =
    match tryGetInt json key with
    | Some value -> value
    | None -> defaultValue

/// <summary>
/// Translates a raw client configuration payload into a <see cref="FormattingOptions"/> value,
/// falling back to the corresponding field of <paramref name="defaults"/> whenever a key is
/// missing or holds an unrecognized/malformed value. Never throws.
/// </summary>
let translate (defaults: FormattingOptions) (json: JToken) : FormattingOptions =
    { IndentSize = parseInt defaults.IndentSize json "indentSize"
      BraceStyle = parseOpeningStyle defaults.BraceStyle json "braceStyle"
      MaxLineLength = parseInt defaults.MaxLineLength json "maxLineLength"
      ParenthesesStyle = parseOpeningStyle defaults.ParenthesesStyle json "parenthesesStyle"
      ParameterStyle = parseCommaStyle defaults.ParameterStyle json "parameterStyle"
      ArgumentStyle = parseCommaStyle defaults.ArgumentStyle json "argumentStyle"
      SpacingInsideBrackets = parseOptionYesNo defaults.SpacingInsideBrackets json "spacingInsideBrackets"
      SpacingInsideParentheses = parseOptionYesNo defaults.SpacingInsideParentheses json "spacingInsideParentheses"
      SpacingAfterCommas = parseOptionYesNo defaults.SpacingAfterCommas json "spacingAfterCommas"
      SpacingBeforeBrackets = parseOptionYesNo defaults.SpacingBeforeBrackets json "spacingBeforeBrackets"
      SpacingBeforeParentheses = parseOptionYesNo defaults.SpacingBeforeParentheses json "spacingBeforeParentheses"
      KeywordStyle = parseKeywordLength defaults.KeywordStyle json "keywordStyle"
      CompoundPredicateStyle = parseNotation defaults.CompoundPredicateStyle json "compoundPredicateStyle"
      EmptyLinesAfterBlocks = parseInt defaults.EmptyLinesAfterBlocks json "emptyLinesAfterBlocks"
      MaxConsecutiveBlankLines = parseInt defaults.MaxConsecutiveBlankLines json "maxConsecutiveBlankLines"
      OperatorStyle = parseNotation defaults.OperatorStyle json "operatorStyle"
      IsOperator = parseIsOpStyle defaults.IsOperator json "isOperator"
      DeclSemicolon = parseBlockStyle defaults.DeclSemicolon json "declSemicolon" }
