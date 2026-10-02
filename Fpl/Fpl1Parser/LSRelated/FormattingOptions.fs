/// <summary>
/// User-configurable formatting preferences consumed by <c>Fpl1Parser.LSRelated.PrettyPrint</c>.
/// </summary>
/// <remarks>
/// Deliberately free of any LSP/OmniSharp/JSON dependency, so this module stays usable from plain
/// unit tests as well as from <c>Fpl3LanguageServer</c>, which is solely responsible for
/// translating raw client configuration (<c>workspace/configuration</c> /
/// <c>workspace/didChangeConfiguration</c> payloads) into a <see cref="FormattingOptions"/> value.
/// </remarks>
module Fpl1Parser.LSRelated.FormattingOptions

/// <summary>
/// Layout style for an opening delimiter (brace, parenthesis or bracket).
/// </summary>
type OpeningStyle =
    /// Width-driven: behaves as <see cref="OneLiner"/> if the enclosing construct fits within
    /// <c>MaxLineLength</c> at its current column, or as <see cref="Egyptian"/> otherwise.
    | Auto
    /// Opening and closing delimiters and content all on one line, e.g. <c>class X { ... }</c>.
    | OneLiner
    /// Opening delimiter stays on the same line as the preceding construct, e.g. <c>theorem X {</c>
    /// followed by indented content and the closing delimiter on its own line.
    | Egyptian
    /// Opening delimiter is placed on its own line, e.g. a <c>{</c> directly below <c>class X</c>.
    | Allman

/// <summary>
/// Layout style for comma-separated lists (e.g. parameters, arguments).
/// </summary>
type CommaStyle =
    /// Width-driven: behaves as <see cref="OneLiner"/> if the list fits within
    /// <c>MaxLineLength</c> at its current column, or as <see cref="Leading"/> otherwise.
    | Auto
    /// All items on a single line, e.g. <c>a, b, c</c>.
    | OneLiner
    /// One item per line, comma placed after each item (except the last), e.g.
    /// <c>a,{newline}b,{newline}c</c>.
    | Trailing
    /// One item per line, comma placed before each item (except the first), e.g.
    /// <c>a{newline},b{newline},c</c>.
    | Leading

/// <summary>A binary on/off formatting toggle.</summary>
type OptionYesNo =
    | Yes
    | No

/// <summary>Whether a keyword with both a short and long spelling should use the short or long form.</summary>
type KeywordLength =
    | Long
    | Short

/// <summary>Whether a construct with both a keyword and symbolic spelling should use which form.</summary>
type Notation =
    | Keyword
    | Symbol

/// <summary>Layout style for the <c>is</c>-operator.</summary>
type IsOpStyle =
    /// Function-call-like form, e.g. <c>is(x, y)</c>.
    | Polish
    /// Infix form, e.g. <c>x is y</c>.
    | Infix

/// <summary>
/// Layout style for a declaration block's trailing semicolon and its variable declarations.
/// </summary>
type BlockStyle =
    /// Declarations and the closing semicolon all on one line, e.g. <c>dec x, y: obj a: pred;</c>.
    | Compact
    /// Each declaration on its own (indented) line, with the semicolon on its own line, e.g.
    /// <c>dec{newline}  x, y: obj{newline}  a: pred{newline};</c>.
    | Enclosing

/// <summary>
/// The complete set of user-configurable FPL formatting preferences.
/// </summary>
type FormattingOptions =
    { IndentSize: int
      BraceStyle: OpeningStyle
      MaxLineLength: int
      ParenthesesStyle: OpeningStyle
      ParameterStyle: CommaStyle
      ArgumentStyle: CommaStyle
      SpacingInsideBrackets: OptionYesNo
      SpacingInsideParentheses: OptionYesNo
      SpacingAfterCommas: OptionYesNo
      SpacingBeforeBrackets: OptionYesNo
      SpacingBeforeParentheses: OptionYesNo
      KeywordStyle: KeywordLength
      CompoundPredicateStyle: Notation
      EmptyLinesAfterBlocks: int
      MaxConsecutiveBlankLines: int
      OperatorStyle: Notation
      IsOperator: IsOpStyle
      DeclSemicolon: BlockStyle }

/// <summary>The default FPL formatting preferences.</summary>
let fplFormatDefaults : FormattingOptions =
    { IndentSize = 2
      BraceStyle = OpeningStyle.Allman
      MaxLineLength = 100
      ParenthesesStyle = OpeningStyle.Auto
      ParameterStyle = CommaStyle.Auto
      ArgumentStyle = CommaStyle.Auto
      SpacingInsideBrackets = OptionYesNo.No
      SpacingInsideParentheses = OptionYesNo.No
      SpacingAfterCommas = OptionYesNo.Yes
      SpacingBeforeBrackets = OptionYesNo.No
      SpacingBeforeParentheses = OptionYesNo.No
      KeywordStyle = KeywordLength.Short
      CompoundPredicateStyle = Notation.Symbol
      EmptyLinesAfterBlocks = 1
      MaxConsecutiveBlankLines = 1
      OperatorStyle = Notation.Symbol
      IsOperator = IsOpStyle.Infix
      DeclSemicolon = BlockStyle.Compact }
