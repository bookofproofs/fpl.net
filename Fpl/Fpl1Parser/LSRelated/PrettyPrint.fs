/// <summary>
/// Pretty-prints an FPL AST back to canonically formatted source text, re-inserting comments
/// captured in a <c>TriviaMap</c> as leading/trailing trivia of their attached nodes, and honoring
/// user-configurable <see cref="FormattingOptions.FormattingOptions"/>.
/// </summary>
module Fpl1Parser.LSRelated.PrettyPrint
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.Doc
open Fpl1Parser.LSRelated.FormattingOptions

/// <summary>Renders a single comment, honoring its <see cref="CommentLexer.CommentKind"/>.</summary>
/// <param name="c">The comment to render.</param>
/// <returns>
/// A <see cref="Doc"/> containing the comment's text: for <see cref="CommentLexer.LineComment"/>,
/// followed by a mandatory <see cref="line"/> break (since nothing may follow a <c>//</c> comment
/// on the same physical line); for <see cref="CommentLexer.BlockComment"/>, with no forced break,
/// since <c>/* ... */</c> comments are self-terminating and may remain inline.
/// </returns>
/// <remarks>
/// Used specifically for the <em>trailing</em> comment slot of <see cref="TriviaMap.Trivia"/>,
/// where whether a line break is required depends on the comment's kind. Leading comments always
/// occupy their own line regardless of kind and are instead rendered via
/// <see cref="renderLeadingComment"/>.
/// </remarks>
let private renderComment (c: CommentLexer.Comment) : Doc =
    match c.Kind with
    | CommentLexer.LineComment -> concat [ text c.Text; line ]   // must end the line
    | CommentLexer.BlockComment -> text c.Text                   // can stay inline

/// <summary>
/// Renders a single leading comment as its own line: the comment text followed by a line break,
/// regardless of <c>Kind</c>, since leading comments always occupy their own source line.
/// </summary>
/// <param name="c">The leading comment to render.</param>
/// <returns>A <see cref="Doc"/> containing the comment's text followed by a <see cref="line"/> break.</returns>
let private renderLeadingComment (c: Comment) : Doc =
    concat [ text c.Text; line ]

/// <summary>
/// Wraps <paramref name="body"/> with any leading-comment lines before it and any trailing
/// comment appended after it on the same line, as recorded for <paramref name="pos"/> in <paramref name="map"/>.
/// </summary>
/// <param name="map">The <see cref="TriviaMap"/> built from the parsed AST and discovered comments.</param>
/// <param name="pos">The <see cref="Positions"/> of the AST node whose trivia should be consulted.</param>
/// <param name="body">The already-rendered <see cref="Doc"/> for the node itself, to be wrapped with its trivia.</param>
/// <returns>
/// <paramref name="body"/> unchanged if <paramref name="pos"/> has no attached trivia; otherwise
/// <paramref name="body"/> preceded by each leading comment (each on its own line, via
/// <see cref="renderLeadingComment"/>) and followed by its trailing comment, if any (via
/// <see cref="renderComment"/>, preceded by a single space).
/// </returns>
let private withTrivia (map: TriviaMap) (pos: Positions) (body: Doc) : Doc =
    match tryGetTrivia map pos with
    | None -> body
    | Some trivia ->
        let leading =
            trivia.Leading
            |> List.map renderLeadingComment
            |> concat
        // a trailing LineComment gets its mandatory line break via renderComment's LineComment branch,
        // while a trailing BlockComment stays inline exactly as before
        let trailing =
            match trivia.Trailing with
            | Some c -> concat [ text " "; renderComment c ]
            | None -> concat []
        concat [ leading; body; trailing ]

// ============================================================================
// FormattingOptions-aware layout helpers.
// ============================================================================
// These small combinators are the only places where a FormattingOptions value
// actually influences the shape of the emitted Doc tree; every case in `print`
// below composes bodies purely out of calls to these helpers (plus `text`/
// `concat` for fixed FPL syntax such as literal keywords/punctuation that no
// option controls), so that adding/adjusting an option never requires touching
// more than one helper.

/// <summary>Renders <paramref name="yn"/> as either a single space or nothing.</summary>
let private optSpace (yn: OptionYesNo) : Doc =
    match yn with
    | Yes -> text " "
    | No -> concat []

/// <summary>
/// Renders a keyword that has both a short and a long spelling, choosing the spelling per
/// <paramref name="opts"/>.<c>KeywordStyle</c>.
/// </summary>
/// <param name="opts">The active formatting options.</param>
/// <param name="short">The short spelling, e.g. <c>"def"</c>.</param>
/// <param name="long">The long spelling, e.g. <c>"definition"</c>.</param>
let private keyword (opts: FormattingOptions) (short: string) (long: string) : Doc =
    match opts.KeywordStyle with
    | KeywordLength.Short -> text short
    | KeywordLength.Long -> text long

/// <summary>
/// Renders one of two spellings for a construct that has both a keyword form and a symbolic
/// form, choosing per <paramref name="opts"/>.<c>CompoundPredicateStyle</c> (used for
/// and/or/impl/iif/xor/not/all/exists).
/// </summary>
let private compoundNotation (opts: FormattingOptions) (kw: string) (sym: string) : Doc =
    match opts.CompoundPredicateStyle with
    | Notation.Keyword -> text kw
    | Notation.Symbol -> text sym

/// <summary>
/// Renders one of two spellings for a user-defined operator, choosing per
/// <paramref name="opts"/>.<c>OperatorStyle</c>.
/// </summary>
let private operatorNotation (opts: FormattingOptions) (kw: string) (sym: string) : Doc =
    match opts.OperatorStyle with
    | Notation.Keyword -> text kw
    | Notation.Symbol -> text sym

/// <summary>
/// Renders an opening delimiter pair (<paramref name="openD"/>/<paramref name="closeD"/>, e.g.
/// <c>"{"</c>/<c>"}"</c>) around <paramref name="bodyDoc"/>, honoring <paramref name="style"/>:
/// <see cref="OpeningStyle.OneLiner"/> keeps everything inline, <see cref="OpeningStyle.Egyptian"/>
/// keeps the opening delimiter on the preceding line with the body indented on following lines,
/// <see cref="OpeningStyle.Allman"/> additionally puts the opening delimiter on its own line, and
/// <see cref="OpeningStyle.Auto"/> defers the OneLiner-vs-Egyptian choice to <see cref="Doc.render"/>
/// via <see cref="Doc.group"/>/<see cref="Doc.softline"/>.
/// </summary>
/// <param name="style">The requested <see cref="OpeningStyle"/>.</param>
/// <param name="openD">The opening delimiter text, e.g. <c>"{"</c>.</param>
/// <param name="closeD">The closing delimiter text, e.g. <c>"}"</c>.</param>
/// <param name="bodyDoc">The already-rendered content to place between the delimiters.</param>
let private delimited (style: OpeningStyle) (openD: string) (closeD: string) (bodyDoc: Doc) : Doc =
    match style with
    | OpeningStyle.OneLiner ->
        concat [ text " "; text openD; text " "; bodyDoc; text " "; text closeD ]
    | OpeningStyle.Egyptian ->
        concat [ text " "; text openD; line; indent bodyDoc; line; text closeD ]
    | OpeningStyle.Allman ->
        concat [ line; text openD; line; indent bodyDoc; line; text closeD ]
    | OpeningStyle.Auto ->
        group (concat [ text " "; text openD; softline; indent bodyDoc; softline; text closeD ])

/// <summary>
/// Renders a comma-separated list of already-printed item <see cref="Doc"/>s, honoring
/// <paramref name="style"/>: <see cref="CommaStyle.OneLiner"/> places all items on one line
/// separated by <c>", "</c> (or <c>","</c>, depending on <paramref name="spaceAfterComma"/>);
/// <see cref="CommaStyle.Trailing"/>/<see cref="CommaStyle.Leading"/> place one item per line with
/// the comma trailing or leading each item respectively; <see cref="CommaStyle.Auto"/> defers the
/// OneLiner-vs-Leading choice to <see cref="Doc.render"/> via <see cref="Doc.group"/>.
/// </summary>
/// <param name="style">The requested <see cref="CommaStyle"/>.</param>
/// <param name="spaceAfterComma">Whether a one-liner separates items with <c>", "</c> or <c>","</c>.</param>
/// <param name="items">The already-rendered item documents, in order.</param>
let private commaList (style: CommaStyle) (spaceAfterComma: OptionYesNo) (items: Doc list) : Doc =
    let sep = match spaceAfterComma with Yes -> text ", " | No -> text ","
    let oneLiner () =
        match items with
        | [] -> concat []
        | d :: rest -> concat (d :: (rest |> List.collect (fun d -> [ sep; d ])))
    let trailing () =
        match items with
        | [] -> concat []
        | _ ->
            let n = List.length items
            items
            |> List.mapi (fun i d -> if i < n - 1 then concat [ d; text ","; line ] else d)
            |> concat
    let leading () =
        match items with
        | [] -> concat []
        | d :: rest -> concat (d :: (rest |> List.collect (fun d -> [ line; text ","; d ])))
    match style with
    | CommaStyle.OneLiner -> oneLiner ()
    | CommaStyle.Trailing -> trailing ()
    | CommaStyle.Leading -> leading ()
    | CommaStyle.Auto ->
        match items with
        | [] -> concat []
        | [ d ] -> d
        | d :: rest ->
            group (concat (d :: (rest |> List.collect (fun d -> [ text ","; softline; d ]))))

/// <summary>
/// Wraps <paramref name="bodyDoc"/> in parentheses, honoring <paramref name="opts"/>'s
/// <c>ParenthesesStyle</c>, <c>SpacingBeforeParentheses</c> and <c>SpacingInsideParentheses</c>.
/// </summary>
let private parens (opts: FormattingOptions) (bodyDoc: Doc) : Doc =
    let inside = match opts.SpacingInsideParentheses with Yes -> text " " | No -> concat []
    let beforeOpen = optSpace opts.SpacingBeforeParentheses
    match opts.ParenthesesStyle with
    | OpeningStyle.OneLiner | OpeningStyle.Auto ->
        // Parentheses are always single-construct wrappers (argument/param tuples, grouping) —
        // Egyptian/Allman-style line breaks don't apply to them the way they do to braces; Auto
        // therefore behaves like OneLiner here, and any width overflow is instead handled by the
        // comma-list inside choosing to break (see commaList/Auto).
        concat [ beforeOpen; text "("; inside; bodyDoc; inside; text ")" ]
    | OpeningStyle.Egyptian | OpeningStyle.Allman ->
        concat [ beforeOpen; text "("; inside; bodyDoc; inside; text ")" ]

/// <summary>
/// Wraps <paramref name="bodyDoc"/> in square brackets, honoring <paramref name="opts"/>'s
/// <c>SpacingBeforeBrackets</c> and <c>SpacingInsideBrackets</c>.
/// </summary>
let private brackets (opts: FormattingOptions) (bodyDoc: Doc) : Doc =
    let inside = match opts.SpacingInsideBrackets with Yes -> text " " | No -> concat []
    let beforeOpen = optSpace opts.SpacingBeforeBrackets
    concat [ beforeOpen; text "["; inside; bodyDoc; inside; text "]" ]

/// <summary>Renders a brace-delimited block using <paramref name="opts"/>'s <c>BraceStyle</c>.</summary>
let private braces (opts: FormattingOptions) (bodyDoc: Doc) : Doc =
    delimited opts.BraceStyle "{" "}" bodyDoc

/// <summary>
/// Renders a parameter/argument tuple: <c>"(" + commaList(items) + ")"</c>, honoring
/// <paramref name="style"/> (the caller supplies <c>opts.ParameterStyle</c> or
/// <c>opts.ArgumentStyle</c> as appropriate) plus the shared parenthesis/spacing options.
/// </summary>
let private tuple (opts: FormattingOptions) (style: CommaStyle) (items: Doc list) : Doc =
    parens opts (commaList style opts.SpacingAfterCommas items)

/// <summary>
/// Renders a declaration block's content and trailing semicolon, honoring
/// <paramref name="opts"/>.<c>DeclSemicolon</c>: <see cref="BlockStyle.Compact"/> keeps every
/// declaration and the semicolon on one line; <see cref="BlockStyle.Enclosing"/> places each
/// declaration on its own indented line with the semicolon on its own trailing line.
/// </summary>
let private declBlock (opts: FormattingOptions) (declDocs: Doc list) : Doc =
    match opts.DeclSemicolon with
    | BlockStyle.Compact ->
        concat [ text "dec "; concat (declDocs |> List.collect (fun d -> [ d; text " " ])); text ";" ]
    | BlockStyle.Enclosing ->
        concat [ text "dec"; line
                 indent (concat (declDocs |> List.map (fun d -> concat [ d; line ])))
                 text ";" ]

/// <summary>Renders the <c>is</c>-operator honoring <paramref name="opts"/>.<c>IsOperator</c>.</summary>
let private isOperator (opts: FormattingOptions) (subjectDoc: Doc) (typeDoc: Doc) : Doc =
    match opts.IsOperator with
    | IsOpStyle.Infix -> concat [ subjectDoc; text " is "; typeDoc ]
    | IsOpStyle.Polish -> concat [ text "is("; subjectDoc; text ", "; typeDoc; text ")" ]

/// <summary>Joins a list of already-rendered <see cref="Doc"/>s with a separator in between each pair.</summary>
let private join (sep: Doc) (docs: Doc list) : Doc =
    match docs with
    | [] -> concat []
    | [ d ] -> d
    | d :: rest -> concat (d :: (rest |> List.collect (fun d -> [ sep; d ])))

/// <summary>
/// Renders a block of raw, already-formatted source text verbatim, splitting on line breaks so
/// each physical line becomes its own <see cref="Doc.Text"/> node joined by <see cref="line"/>.
/// </summary>
/// <param name="raw">The verbatim text to emit (may be empty, in which case nothing is rendered).</param>
/// <returns>
/// <see cref="concat"/> of per-line <see cref="text"/> nodes, or <c>concat []</c> when
/// <paramref name="raw"/> is empty.
/// </returns>
/// <remarks>
/// Used exclusively for <c>Ast.ErrorSyntax</c>/<c>ErrorSyntaxBacktracking</c>/<c>ErrorSyntaxChain</c>
/// verbatim-reprint text: unlike every other case in <see cref="print"/>, this text is raw source
/// copied byte-for-byte from the original input rather than built up from sub-<c>Doc</c>s, so a
/// single <see cref="text"/> call would corrupt <see cref="Doc.render"/>'s line/indentation
/// tracking if the text itself contains embedded newlines. Splitting here keeps each resulting
/// <see cref="Doc.Text"/> single-line, as every other call site in this module already assumes.
/// </remarks>
let private verbatimBlock (raw: string) : Doc =
    if System.String.IsNullOrEmpty raw then
        concat []
    else
        raw.Split([| "\r\n"; "\n" |], System.StringSplitOptions.None)
        |> Array.map text
        |> Array.toList
        |> join line

// ============================================================================
// Recursive per-node printer.
// ============================================================================

/// <summary>
/// The recursive per-node printer: renders a single <see cref="Ast"/> node to a <see cref="Doc"/>,
/// consulting <paramref name="map"/> via <see cref="withTrivia"/> at each node so leading/trailing
/// comments are spliced in at the correct position, consulting <paramref name="opts"/> via the
/// layout helpers above for every stylistic choice, and recursing into child nodes in source order.
/// </summary>
/// <param name="opts">The active <see cref="FormattingOptions"/> controlling all stylistic choices.</param>
/// <param name="map">The <see cref="TriviaMap"/> used to attach comments to the nodes being printed.</param>
/// <param name="ast">The AST node to render.</param>
/// <returns>The rendered <see cref="Doc"/> for <paramref name="ast"/>, including any attached trivia.</returns>
/// <remarks>
/// Has no notion of "this is the whole document" — each case renders exactly one construct and
/// delegates to itself for child nodes. <see cref="printAll"/> is the module's public entry point
/// and is the only function that should be called from outside this module; <c>print</c> is kept
/// <c>private</c> so callers cannot bypass <see cref="printAll"/>'s top-level layout policy
/// (blank-line separation and final rendering) by invoking it directly on an isolated sub-tree.
/// The case grouping/order below mirrors <c>Trivia.collectPositions</c> exactly, so gaps or
/// mismatches are easy to spot by diffing the two files. No case below embeds a literal brace,
/// parenthesis, bracket, comma-list or short/long keyword spelling directly — all such choices are
/// routed through the <paramref name="opts"/>-aware helpers above, so that adding or adjusting a
/// <see cref="FormattingOptions"/> field never requires touching more than one helper plus this match.
/// </remarks>
let rec print (opts: FormattingOptions) (map: TriviaMap) (ast: Ast) : Doc =
    let p = print opts map
    let opt f = function Some x -> f x | None -> concat []
    let list sep xs = xs |> List.map p |> join sep
    match ast with
    // Lexical / Leaf tokens
    | Alias(pos, name) -> withTrivia map pos (concat [ text "as "; text name ])
    | Dot () -> text "."
    | Star(pos, _) -> withTrivia map pos (text "*")
    | Digits s -> text s
    | DollarDigits(pos, n) -> withTrivia map pos (text ($"${n}"))
    | ObjectSymbolWithPos(pos, s) -> withTrivia map pos (text s)
    | InfixSymbolWithPos(pos, s) -> withTrivia map pos (text s)
    | PostFixSymbolWithPos(pos, s) -> withTrivia map pos (text s)
    | PrefixSymbolWithPos(pos, s) -> withTrivia map pos (text s)

    // Identifiers & identifier dispatchers
    | PascalCaseId(pos, name) -> withTrivia map pos (text name)
    | BaseClassName(pos, name) -> withTrivia map pos (text name)
    | PredicateIdentifier(pos, name) -> withTrivia map pos (text name)
    | NamespaceIdentifier(pos, asts) ->
        withTrivia map pos (list (text ".") asts)
    | ClassIdentifier(pos, a) -> withTrivia map pos (p a)
    | AliasedNamespaceIdentifier(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ p a; opt (fun a2 -> concat [ text " "; p a2 ]) aOpt ])
    | ArgumentIdentifier(pos, s) -> withTrivia map pos (text s)
    | RefArgumentIdentifier(pos, s) -> withTrivia map pos (text s)
    | DelegateName(pos, s) -> withTrivia map pos (text s)
    | ReferencingIdentifier(pos, (a, asts)) ->
        withTrivia map pos (concat [ p a; concat (asts |> List.map p) ])

    // Types & type related constructs
    | IndexType(pos, _) -> withTrivia map pos (keyword opts "ind" "index")
    | FunctionalTermType(pos, _) -> withTrivia map pos (keyword opts "func" "function")
    | ObjectType(pos, _) -> withTrivia map pos (keyword opts "obj" "object")
    | PredicateType(pos, _) -> withTrivia map pos (keyword opts "pred" "predicate")
    | TemplateType(pos, s) -> withTrivia map pos (text s)
    | ArrayType(pos, (a, asts)) ->
        withTrivia map pos (concat [ text "*"; p a; brackets opts (commaList (if opts.ArgumentStyle = CommaStyle.Auto then CommaStyle.Auto else opts.ArgumentStyle) opts.SpacingAfterCommas (asts |> List.map p)) ])
    | SimpleVariableType(pos, a) -> withTrivia map pos (p a)
    | IndexAllowedType(pos, a) -> withTrivia map pos (p a)
    | InheritedType(pos, s) -> withTrivia map pos (text s)
    | InheritedTypeList asts -> commaList opts.ArgumentStyle opts.SpacingAfterCommas (asts |> List.map p)
    | CompoundPredicateType(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ p a; opt p aOpt ])
    | CompoundFunctionalTermType(pos, (a, tupOpt)) ->
        withTrivia map pos (concat [ p a; opt (fun (a1, a2) -> concat [ p a1; p a2 ]) tupOpt ])

    // Variables
    | VarDeclBlock astsOpt ->
        opt (fun asts -> declBlock opts (asts |> List.map p)) astsOpt
    | NamedVarDecl(pos, (asts, a)) ->
        withTrivia map pos (concat [ commaList opts.ArgumentStyle opts.SpacingAfterCommas (asts |> List.map p); text ": "; p a ])
    | Var(pos, name) -> withTrivia map pos (text name)

    // Predicates
    | True(pos, _) -> withTrivia map pos (text "true")
    | False(pos, _) -> withTrivia map pos (text "false")
    | And(pos, (a1, a2)) ->
        withTrivia map pos (concat [ compoundNotation opts "and" "∧"; tuple opts opts.ArgumentStyle [ p a1; p a2 ] ])
    | Or(pos, (a1, a2)) ->
        withTrivia map pos (concat [ compoundNotation opts "or" "∨"; tuple opts opts.ArgumentStyle [ p a1; p a2 ] ])
    | Xor(pos, (a1, a2)) ->
        withTrivia map pos (concat [ compoundNotation opts "xor" "⩡"; tuple opts opts.ArgumentStyle [ p a1; p a2 ] ])
    | Impl(pos, (a1, a2)) ->
        withTrivia map pos (concat [ compoundNotation opts "impl" "⇒"; tuple opts opts.ArgumentStyle [ p a1; p a2 ] ])
    | Iif(pos, (a1, a2)) ->
        withTrivia map pos (concat [ compoundNotation opts "iif" "⇔"; tuple opts opts.ArgumentStyle [ p a1; p a2 ] ])
    | Not(pos, a) ->
        withTrivia map pos (concat [ compoundNotation opts "not " "¬"; p a ])
    | All(pos, (asts, a)) ->
        withTrivia map pos
            (concat [ compoundNotation opts "all " "∀"; commaList opts.ParameterStyle opts.SpacingAfterCommas (asts |> List.map p); braces opts (p a) ])
    | Exists(pos, (asts, a)) ->
        withTrivia map pos
            (concat [ compoundNotation opts "ex " "∃"; commaList opts.ParameterStyle opts.SpacingAfterCommas (asts |> List.map p); braces opts (p a) ])
    | Exists1 () -> text "∃!"
    | ExistsN(pos, ((a1, asts), a2)) ->
        withTrivia map pos
            (concat [ text "exn"; p a1; text " "; commaList opts.ParameterStyle opts.SpacingAfterCommas (asts |> List.map p); braces opts (p a2) ])
    | IsOperator(pos, (a1, a2)) ->
        withTrivia map pos (isOperator opts (p a1) (p a2))

    // Expressions
    | PredicateWithQualification(a1, a2) ->
        concat [ p a1; p a2 ]
    | PredicateWithOptSpecification(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ p a; opt p aOpt ])
    | PrefixOp(a1, a2) ->
        concat [ p a1; p a2 ]
    | PostfixOp(a1, a2) ->
        concat [ p a2; p a1 ]
    | InfixOp(pos, items) ->
        withTrivia map pos
            (concat (items |> List.map (fun (a, aOpt) ->
                concat [ p a; opt (fun op -> concat [ text " "; p op; text " " ]) aOpt ])))
    | Parens(pos, a) ->
        withTrivia map pos (parens opts (p a))

    // Tuple-like constructs and qualifies
    | BrackedCoordList(pos, asts) ->
        withTrivia map pos (brackets opts (commaList opts.ArgumentStyle opts.SpacingAfterCommas (asts |> List.map p)))
    | ArgumentTuple(pos, asts) ->
        withTrivia map pos (tuple opts opts.ArgumentStyle (asts |> List.map p))
    | DottedPredicate(pos, a) ->
        withTrivia map pos (concat [ text "."; p a ])
    | QualificationList(pos, asts) ->
        withTrivia map pos (concat (asts |> List.map p))
    | ParamTuple asts ->
        tuple opts opts.ParameterStyle (asts |> List.map p)

    // Commands
    | Delegate(a1, a2) ->
        concat [ text "del."; p a1; p a2 ]
    | Assertion(pos, a) ->
        withTrivia map pos (concat [ text "assert "; p a ])
    | Cases(pos, (asts, a)) ->
        withTrivia map pos
            (concat [ text "cases"; parens opts (concat [ line; concat (asts |> List.map p); p a ]) ])
    | CaseSingle(pos, (a, asts)) ->
        withTrivia map pos (concat [ text "| "; p a; text ": "; concat (asts |> List.map p); line ])
    | CaseElse(pos, asts) ->
        withTrivia map pos (concat [ text "? "; concat (asts |> List.map p) ])
    | MapCases(pos, (asts, a)) ->
        withTrivia map pos
            (concat [ text "mcases"; parens opts (concat [ line; concat (asts |> List.map p); p a ]) ])
    | MapCaseSingle(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "| "; p a1; text ": "; p a2; line ])
    | MapCaseElse(pos, a) ->
        withTrivia map pos (concat [ text "? "; p a ])
    | Assignment(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " := "; p a2 ])
    | ForIn(pos, ((a1, a2), asts)) ->
        withTrivia map pos
            (concat [ text "for "; p a1; text " "; p a2; braces opts (concat (asts |> List.map p)) ])
    | InEntity(pos, a) ->
        withTrivia map pos (concat [ text "in "; p a ])
    | Return(pos, a) ->
        withTrivia map pos (concat [ keyword opts "ret" "return"; text " "; p a ])

    // Symbol extensions
    | SymbolDecl(pos, s) -> withTrivia map pos (concat [ text "symbol \""; text s; text "\"" ])
    | PrefixDecl(pos, s) -> withTrivia map pos (concat [ text "prefix \""; text s; text "\"" ])
    | PostfixDecl(pos, s) -> withTrivia map pos (concat [ text "postfix \""; text s; text "\"" ])
    | InfixDeclWithPrecedence(pos, (s, a)) ->
        withTrivia map pos (concat [ text "infix \""; text s; text "\" "; p a ])
    | Precedence(pos, n) -> withTrivia map pos (text (string n))
    | DefinitionExtension(pos, ((a1, a2), a3)) ->
        withTrivia map pos (concat [ keyword opts "ext" "extension"; text " "; p a1; p a2; braces opts (p a3) ])
    | ExtensionSignature(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; p a2 ])
    | ExtensionAssignment(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " @ /"; p a2; text "/" ])
    | ExtensionRegex s -> text s
    | ExtensionName(pos, s) -> withTrivia map pos (text s)

    // Definitions
    | DefinitionClass(pos, (((a1, a1Opt), a2Opt), a3)) ->
        withTrivia map pos
            (concat [ keyword opts "def" "definition"; text " "; p a1
                      opt (fun a -> concat [ text ": "; p a ]) a1Opt
                      opt (fun a -> concat [ text " "; p a ]) a2Opt
                      p a3 ])
    | ClassSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "cl" "class"; text " "; p a ])
    | ClassDefinitionBlock(pos, tupOpt) ->
        withTrivia map pos
            (opt (fun (a, astsOpt) ->
                braces opts (concat [ p a; opt (fun asts -> concat (asts |> List.map (fun a -> concat [ line; p a ]))) astsOpt ])) tupOpt)
    | DefClassCompleteContent(a, asts) ->
        concat [ p a; concat (asts |> List.map p) ]
    | Constructor(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | ConstructorSignature(pos, (a1, a2)) ->
        withTrivia map pos (concat [ keyword opts "ctor" "constructor"; text " "; p a1; p a2 ])
    | ConstructorBlock a ->
        braces opts (p a)
    | BaseConstructorCall(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "base."; p a1; p a2 ])

    | DefinitionPredicate(pos, (a, tupOpt)) ->
        withTrivia map pos
            (concat [ keyword opts "def" "definition"; text " "; p a
                      opt (fun (a1, astsOpt) ->
                          braces opts (concat [ p a1; opt (fun asts -> concat (asts |> List.map (fun a -> concat [ line; p a ]))) astsOpt ])) tupOpt ])
    | PredicateSignature((pos, ((a1, a1Opt), a2)), a3Opt) ->
        withTrivia map pos
            (concat [ keyword opts "pred" "predicate"; text " "; p a1
                      opt (fun a -> concat [ text ": "; p a ]) a1Opt
                      p a2
                      opt p a3Opt ])
    | DefPredicateContent(a1, a2) ->
        concat [ p a1; p a2 ]

    | DefinitionFunctionalTerm(pos, (a1, a2)) ->
        withTrivia map pos (concat [ keyword opts "def" "definition"; text " "; p a1; text " "; p a2 ])
    | FunctionalTermSignature((pos, (((a1, a1Opt), a2), a3)), a4Opt) ->
        withTrivia map pos
            (concat [ keyword opts "func" "function"; text " "; p a1
                      opt (fun a -> concat [ text ": "; p a ]) a1Opt
                      p a2; p a3
                      opt p a4Opt ])
    | Mapping(pos, a) ->
        withTrivia map pos (concat [ text "-> "; p a ])
    | FunctionalTermDefinitionBlock(pos, tupOpt) ->
        withTrivia map pos
            (opt (fun (a, astsOpt) ->
                braces opts (concat [ p a; opt (fun asts -> concat (asts |> List.map (fun a -> concat [ line; p a ]))) astsOpt ])) tupOpt)
    | DefFunctionContent(a1, a2) ->
        concat [ p a1; p a2 ]

    | PredicateInstance(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ keyword opts "prty" "property"; text " "; p a; opt (fun a2 -> braces opts (p a2)) aOpt ])
    | PredicateInstanceSignature(pos, (a1, a2)) ->
        withTrivia map pos (concat [ keyword opts "pred" "predicate"; text " "; p a1; p a2 ])
    | FunctionalTermInstance(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ keyword opts "prty" "property"; text " "; p a; opt (fun a2 -> braces opts (p a2)) aOpt ])
    | FunctionalTermInstanceSignature(pos, ((a1, a2), a3)) ->
        withTrivia map pos (concat [ keyword opts "func" "function"; text " "; p a1; p a2; p a3 ])

    // Rules of inference
    | RuleOfInference(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | RuleOfInferenceSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "inf" "inference"; text " "; p a ])
    | PremiseConclusionBlock(a1, (a2, a3)) ->
        braces opts (concat [ p a1; p a2; p a3 ])
    | PremiseList(pos, asts) ->
        withTrivia map pos (concat [ keyword opts "pre" "premise"; text ": "; commaList opts.ArgumentStyle opts.SpacingAfterCommas (asts |> List.map p) ])

    // Statements
    | Axiom(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; braces opts (concat [ p a2; p a3 ]) ])
    | AxiomSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "ax" "axiom"; text " "; p a ])
    | Conjecture(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; braces opts (concat [ p a2; p a3 ]) ])
    | ConjectureSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "conj" "conjecture"; text " "; p a ])
    | Theorem(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; braces opts (concat [ p a2; p a3 ]) ])
    | TheoremSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "thm" "theorem"; text " "; p a ])
    | Lemma(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; braces opts (concat [ p a2; p a3 ]) ])
    | LemmaSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "lem" "lemma"; text " "; p a ])
    | Proposition(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; braces opts (concat [ p a2; p a3 ]) ])
    | PropositionSignature(pos, a) ->
        withTrivia map pos (concat [ keyword opts "prop" "proposition"; text " "; p a ])
    | Corollary(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; braces opts (concat [ p a2; p a3 ]) ])
    | CorollarySignature(pos, (a, asts)) ->
        withTrivia map pos (concat [ keyword opts "cor" "corollary"; text " "; p a; concat (asts |> List.map p) ])

    // Proofs
    | Proof(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | ProofSignature(pos, (a, asts)) ->
        withTrivia map pos (concat [ keyword opts "prf" "proof"; text " "; p a; concat (asts |> List.map p) ])
    | ProofBlock a ->
        braces opts (p a)
    | ProofContent((a1, asts), a2Opt) ->
        concat [ p a1; concat (asts |> List.map p); opt p a2Opt ]
    | Argument(pos, a) ->
        withTrivia map pos (p a)
    | JustArgInf(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | StartArgument a ->
        p a
    | StartArgumentStictly(a, asts) ->
        concat [ p a; commaList opts.ArgumentStyle opts.SpacingAfterCommas (asts |> List.map p) ]
    | Justification(pos, a) ->
        withTrivia map pos (p a)
    | JustificationItem(pos, a) ->
        withTrivia map pos (p a)
    | ReferenceToProofOrCorollary(pos, a) ->
        withTrivia map pos (p a)
    | ByDef(pos, a) ->
        withTrivia map pos (concat [ text "bydef "; p a ])
    | JustificationIdentifier(pos, (((modOpt, a1), astsOpt), a2Opt)) ->
        withTrivia map pos
            (concat [ (match modOpt with Some m -> concat [ text m; text " " ] | None -> concat [])
                      p a1
                      opt (fun asts -> concat (asts |> List.map p)) astsOpt
                      opt (fun a -> concat [ text ": "; p a ]) a2Opt ])
    | TrivialArgument(pos, _) -> withTrivia map pos (text "trivial")
    | DeriveArgument(pos, a) ->
        withTrivia map pos (p a)
    | AssumeArgument(pos, a) ->
        withTrivia map pos (concat [ keyword opts "ass" "assume"; text " "; p a ])
    | RevokeArgument(pos, a) ->
        withTrivia map pos (concat [ keyword opts "rev" "revoke"; text " "; p a ])
    | Qed(pos, _) -> withTrivia map pos (text "qed")

    // Special references
    | Intrinsic(pos, _) -> withTrivia map pos (keyword opts "intr" "intrinsic")
    | Undefined(pos, _) -> withTrivia map pos (keyword opts "undef" "undefined")
    | SelfOrParent(pos, a) -> withTrivia map pos (p a)
    | Self(pos, _) -> withTrivia map pos (text "self")
    | Parent(pos, _) -> withTrivia map pos (text "parent")
    | Extension(pos, s) -> withTrivia map pos (concat [ text "@"; text s ])

    // Localizations
    | Localization((pos, a), asts) ->
        withTrivia map pos (concat [ keyword opts "loc" "localization"; text " "; p a; braces opts (concat (asts |> List.map p)) ])
    | TranslationTermList(pos, asts) ->
        withTrivia map pos (concat (asts |> List.map p))
    | TranslationTerm(pos, asts) ->
        withTrivia map pos (concat (asts |> List.map p))
    | Language(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text ": "; p a2 ])
    | LanguageCode(pos, s) -> withTrivia map pos (text s)
    | LocalizationString(pos, s) -> withTrivia map pos (concat [ text "\""; text s; text "\"" ])

    // TopLevel
    | AST(pos, a) -> withTrivia map pos (p a)
    | Namespace asts -> concat (asts |> List.map p)
    | UsesClause(pos, a) ->
        withTrivia map pos (concat [ keyword opts "uses" "uses"; text " "; p a ])
    | BuildingBlock(pos, a) -> withTrivia map pos (p a)
    // Verbatim reprint: unlike every other case above, these three carry raw, already-formatted
    // source text captured from the faulty building block by Fpl1Parser.Main's error recovery, so
    // withTrivia (which would consult the TriviaMap by the *error* position, not the block span,
    // and re-inject comments already present verbatim in the captured text) is deliberately not
    // applied here to avoid duplicating comments. Chain links after the first carry "" (see
    // Fpl1Parser.Formatting.getErrorNodes) and render as nothing, so a multi-link SY002 chain
    // reprints its enclosing block exactly once.
    | ErrorSyntax(_, _, _, verbatim) -> verbatimBlock verbatim
    | ErrorSyntaxBacktracking(_, _, _, verbatim) -> verbatimBlock verbatim
    | ErrorSyntaxChain(_, _, _, verbatim) -> verbatimBlock verbatim
/// <summary>
/// The entry point for the formatting service: pretty-prints a list of top-level building-block
/// ASTs (as returned by <c>Fpl1Parser.Main.fplParser</c>), honoring trivia recorded in
/// <paramref name="map"/> and the user's <paramref name="opts"/>.
/// </summary>
/// <param name="opts">The active <see cref="FormattingOptions"/>; pass <see cref="FormattingOptions.defaults"/> if the caller has no user-specific configuration.</param>
/// <param name="map">The <see cref="TriviaMap"/> built from the parsed AST and discovered comments.</param>
/// <param name="asts">The top-level building-block AST nodes to render, in source order.</param>
/// <returns>
/// The fully rendered, canonically formatted FPL source text, with
/// <paramref name="opts"/>.<c>EmptyLinesAfterBlocks</c> blank lines separating each top-level
/// building block, and no run of blank lines anywhere exceeding
/// <paramref name="opts"/>.<c>MaxConsecutiveBlankLines</c>.
/// </returns>
/// <remarks>
/// This is the only function in the module intended to be called by <c>Fpl3LanguageServer</c>'s
/// <c>FormattingHandler</c>. It maps <see cref="print"/> over each top-level node, inserts blank
/// lines between building blocks per <c>EmptyLinesAfterBlocks</c>, delegates text composition to
/// <see cref="Doc.render"/> (which honors <c>opts.IndentSize</c> and <c>opts.MaxLineLength</c>, the
/// latter driving every <see cref="OpeningStyle.Auto"/>/<see cref="CommaStyle.Auto"/> decision made
/// while printing), and finally runs <see cref="Doc.collapseBlankLines"/> over the rendered text so
/// that no run of blank lines — regardless of whether it came from source comments, consecutive
/// hard breaks, or the <c>EmptyLinesAfterBlocks</c> separator itself — exceeds
/// <c>opts.MaxConsecutiveBlankLines</c>.
/// </remarks>
let printAll (opts: FormattingOptions) (map: TriviaMap) (asts: Ast list) : string =
    let blankLines = concat (List.replicate opts.EmptyLinesAfterBlocks line)
    asts
    |> List.map (print opts map)
    |> List.collect (fun d -> [ d; line; blankLines ])
    |> concat
    |> render opts.IndentSize opts.MaxLineLength
    |> collapseBlankLines opts.MaxConsecutiveBlankLines
