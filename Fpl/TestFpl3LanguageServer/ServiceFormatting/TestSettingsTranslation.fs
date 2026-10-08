namespace TestFpl3LanguageServer.ServiceFormatting

open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json.Linq
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl3LanguageServer.ServiceFormatting.SettingsTranslation

[<TestClass>]
type TestSettingsTranslation () =

    /// <summary>
    /// A fully-populated JSON payload, with every field set to a non-default value where
    /// possible, used to verify the full round-trip into a <see cref="FormattingOptions"/>.
    /// </summary>
    let fullPayload () : JObject =
        JObject(
            JProperty("indentSize", 4),
            JProperty("braceStyle", "egyptian"),
            JProperty("maxLineLength", 120),
            JProperty("parenthesesStyle", "oneLiner"),
            JProperty("parameterStyle", "trailing"),
            JProperty("argumentStyle", "leading"),
            JProperty("spacingInsideBrackets", true),
            JProperty("spacingInsideParentheses", true),
            JProperty("spacingAfterCommas", false),
            JProperty("spacingBeforeBrackets", true),
            JProperty("spacingBeforeParentheses", true),
            JProperty("keywordStyle", "long"),
            JProperty("compoundPredicateStyle", "keyword"),
            JProperty("emptyLinesAfterBlocks", 2),
            JProperty("maxConsecutiveBlankLines", 3),
            JProperty("operatorStyle", "keyword"),
            JProperty("isOperator", "polish"),
            JProperty("declSemicolon", "enclosing")
        )

    [<TestMethod>]
    member _.TestFullPayloadRoundTrips() =
        let json = fullPayload () :> JToken
        let actual = translate fplFormatDefaults json

        Assert.AreEqual<int>(4, actual.IndentSize)
        Assert.AreEqual<OpeningStyle>(OpeningStyle.Egyptian, actual.BraceStyle)
        Assert.AreEqual<int>(120, actual.MaxLineLength)
        Assert.AreEqual<OpeningStyle>(OpeningStyle.OneLiner, actual.ParenthesesStyle)
        Assert.AreEqual<CommaStyle>(CommaStyle.Trailing, actual.ParameterStyle)
        Assert.AreEqual<CommaStyle>(CommaStyle.Leading, actual.ArgumentStyle)
        Assert.AreEqual<OptionYesNo>(OptionYesNo.Yes, actual.SpacingInsideBrackets)
        Assert.AreEqual<OptionYesNo>(OptionYesNo.Yes, actual.SpacingInsideParentheses)
        Assert.AreEqual<OptionYesNo>(OptionYesNo.No, actual.SpacingAfterCommas)
        Assert.AreEqual<OptionYesNo>(OptionYesNo.Yes, actual.SpacingBeforeBrackets)
        Assert.AreEqual<OptionYesNo>(OptionYesNo.Yes, actual.SpacingBeforeParentheses)
        Assert.AreEqual<KeywordLength>(KeywordLength.Long, actual.KeywordStyle)
        Assert.AreEqual<Notation>(Notation.Keyword, actual.CompoundPredicateStyle)
        Assert.AreEqual<int>(2, actual.EmptyLinesAfterBlocks)
        Assert.AreEqual<int>(3, actual.MaxConsecutiveBlankLines)
        Assert.AreEqual<Notation>(Notation.Keyword, actual.OperatorStyle)
        Assert.AreEqual<IsOpStyle>(IsOpStyle.Polish, actual.IsOperator)
        Assert.AreEqual<BlockStyle>(BlockStyle.Enclosing, actual.DeclSemicolon)

    [<DataRow("indentSize")>]
    [<DataRow("braceStyle")>]
    [<DataRow("maxLineLength")>]
    [<DataRow("parenthesesStyle")>]
    [<DataRow("parameterStyle")>]
    [<DataRow("argumentStyle")>]
    [<DataRow("spacingInsideBrackets")>]
    [<DataRow("spacingInsideParentheses")>]
    [<DataRow("spacingAfterCommas")>]
    [<DataRow("spacingBeforeBrackets")>]
    [<DataRow("spacingBeforeParentheses")>]
    [<DataRow("keywordStyle")>]
    [<DataRow("compoundPredicateStyle")>]
    [<DataRow("emptyLinesAfterBlocks")>]
    [<DataRow("maxConsecutiveBlankLines")>]
    [<DataRow("operatorStyle")>]
    [<DataRow("isOperator")>]
    [<DataRow("declSemicolon")>]
    [<TestMethod>]
    member _.TestMissingFieldFallsBackToDefault(missingKey: string) =
        let json = fullPayload ()
        json.Remove(missingKey) |> ignore

        let actual = translate fplFormatDefaults (json :> JToken)

        // Reconstruct what the result would be with every field populated except the removed one,
        // then compare just the fallback field against fplFormatDefaults.
        match missingKey with
        | "indentSize" -> Assert.AreEqual<int>(fplFormatDefaults.IndentSize, actual.IndentSize)
        | "braceStyle" -> Assert.AreEqual<OpeningStyle>(fplFormatDefaults.BraceStyle, actual.BraceStyle)
        | "maxLineLength" -> Assert.AreEqual<int>(fplFormatDefaults.MaxLineLength, actual.MaxLineLength)
        | "parenthesesStyle" -> Assert.AreEqual<OpeningStyle>(fplFormatDefaults.ParenthesesStyle, actual.ParenthesesStyle)
        | "parameterStyle" -> Assert.AreEqual<CommaStyle>(fplFormatDefaults.ParameterStyle, actual.ParameterStyle)
        | "argumentStyle" -> Assert.AreEqual<CommaStyle>(fplFormatDefaults.ArgumentStyle, actual.ArgumentStyle)
        | "spacingInsideBrackets" -> Assert.AreEqual<OptionYesNo>(fplFormatDefaults.SpacingInsideBrackets, actual.SpacingInsideBrackets)
        | "spacingInsideParentheses" -> Assert.AreEqual<OptionYesNo>(fplFormatDefaults.SpacingInsideParentheses, actual.SpacingInsideParentheses)
        | "spacingAfterCommas" -> Assert.AreEqual<OptionYesNo>(fplFormatDefaults.SpacingAfterCommas, actual.SpacingAfterCommas)
        | "spacingBeforeBrackets" -> Assert.AreEqual<OptionYesNo>(fplFormatDefaults.SpacingBeforeBrackets, actual.SpacingBeforeBrackets)
        | "spacingBeforeParentheses" -> Assert.AreEqual<OptionYesNo>(fplFormatDefaults.SpacingBeforeParentheses, actual.SpacingBeforeParentheses)
        | "keywordStyle" -> Assert.AreEqual<KeywordLength>(fplFormatDefaults.KeywordStyle, actual.KeywordStyle)
        | "compoundPredicateStyle" -> Assert.AreEqual<Notation>(fplFormatDefaults.CompoundPredicateStyle, actual.CompoundPredicateStyle)
        | "emptyLinesAfterBlocks" -> Assert.AreEqual<int>(fplFormatDefaults.EmptyLinesAfterBlocks, actual.EmptyLinesAfterBlocks)
        | "maxConsecutiveBlankLines" -> Assert.AreEqual<int>(fplFormatDefaults.MaxConsecutiveBlankLines, actual.MaxConsecutiveBlankLines)
        | "operatorStyle" -> Assert.AreEqual<Notation>(fplFormatDefaults.OperatorStyle, actual.OperatorStyle)
        | "isOperator" -> Assert.AreEqual<IsOpStyle>(fplFormatDefaults.IsOperator, actual.IsOperator)
        | "declSemicolon" -> Assert.AreEqual<BlockStyle>(fplFormatDefaults.DeclSemicolon, actual.DeclSemicolon)
        | _ -> Assert.Fail($"Unhandled key: {missingKey}")

    [<TestMethod>]
    member _.TestEmptyPayloadFallsBackToAllDefaults() =
        let actual = translate fplFormatDefaults (JObject() :> JToken)
        Assert.AreEqual<FormattingOptions>(fplFormatDefaults, actual)

    [<DataRow("braceStyle")>]
    [<DataRow("parenthesesStyle")>]
    [<TestMethod>]
    member _.TestUnrecognizedOpeningStyleFallsBackToDefault(key: string) =
        let json = JObject(JProperty(key, "not-a-real-style")) :> JToken
        let actual = translate fplFormatDefaults json
        match key with
        | "braceStyle" -> Assert.AreEqual<OpeningStyle>(fplFormatDefaults.BraceStyle, actual.BraceStyle)
        | "parenthesesStyle" -> Assert.AreEqual<OpeningStyle>(fplFormatDefaults.ParenthesesStyle, actual.ParenthesesStyle)
        | _ -> Assert.Fail($"Unhandled key: {key}")

    [<TestMethod>]
    member _.TestNonNumericIndentSizeFallsBackToDefault() =
        let json = JObject(JProperty("indentSize", "not-a-number")) :> JToken
        let actual = translate fplFormatDefaults json
        Assert.AreEqual<int>(fplFormatDefaults.IndentSize, actual.IndentSize)
