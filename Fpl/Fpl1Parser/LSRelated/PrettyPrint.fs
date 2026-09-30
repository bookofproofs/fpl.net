/// <summary>
/// Pretty-prints an FPL AST back to canonically formatted source text, re-inserting comments
/// captured in a <c>TriviaMap</c> as leading/trailing trivia of their attached nodes.
/// </summary>
module Fpl1Parser.LSRelated.PrettyPrint
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.Doc


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

/// <summary>Joins a list of already-rendered <see cref="Doc"/>s with a separator in between each pair.</summary>
let private join (sep: Doc) (docs: Doc list) : Doc =
    match docs with
    | [] -> concat []
    | [ d ] -> d
    | d :: rest -> concat (d :: (rest |> List.collect (fun d -> [ sep; d ])))

/// <summary>
/// The recursive per-node printer: renders a single <see cref="Ast"/> node to a <see cref="Doc"/>,
/// consulting <paramref name="map"/> via <see cref="withTrivia"/> at each node so leading/trailing
/// comments are spliced in at the correct position, and recursing into child nodes in source order.
/// </summary>
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
/// mismatches are easy to spot by diffing the two files.
/// </remarks>
let rec private print (map: TriviaMap) (ast: Ast) : Doc =
    let p = print map
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
    | IndexType(pos, _) -> withTrivia map pos (text "index")
    | FunctionalTermType(pos, _) -> withTrivia map pos (text "function")
    | ObjectType(pos, _) -> withTrivia map pos (text "object")
    | PredicateType(pos, _) -> withTrivia map pos (text "predicate")
    | TemplateType(pos, s) -> withTrivia map pos (text s)
    | ArrayType(pos, (a, asts)) ->
        withTrivia map pos (concat [ text "*"; p a; text "["; list (text ", ") asts; text "]" ])
    | SimpleVariableType(pos, a) -> withTrivia map pos (p a)
    | IndexAllowedType(pos, a) -> withTrivia map pos (p a)
    | InheritedType(pos, s) -> withTrivia map pos (text s)
    | InheritedTypeList asts -> list (text ", ") asts
    | CompoundPredicateType(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ p a; opt p aOpt ])
    | CompoundFunctionalTermType(pos, (a, tupOpt)) ->
        withTrivia map pos (concat [ p a; opt (fun (a1, a2) -> concat [ p a1; p a2 ]) tupOpt ])

    // Variables
    | VarDeclBlock astsOpt ->
        opt (fun asts -> concat [ text "dec"; line; indent (concat (asts |> List.map (fun a -> concat [ p a; text ";"; line ]))) ]) astsOpt
    | NamedVarDecl(pos, (asts, a)) ->
        withTrivia map pos (concat [ list (text ", ") asts; text ": "; p a ])
    | Var(pos, name) -> withTrivia map pos (text name)

    // Predicates
    | True(pos, _) -> withTrivia map pos (text "true")
    | False(pos, _) -> withTrivia map pos (text "false")
    | And(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "and("; p a1; text ", "; p a2; text ")" ])
    | Or(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "or("; p a1; text ", "; p a2; text ")" ])
    | Xor(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "xor("; p a1; text ", "; p a2; text ")" ])
    | Impl(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "impl("; p a1; text ", "; p a2; text ")" ])
    | Iif(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "iif("; p a1; text ", "; p a2; text ")" ])
    | Not(pos, a) ->
        withTrivia map pos (concat [ text "not "; p a ])
    | All(pos, (asts, a)) ->
        withTrivia map pos (concat [ text "all "; list (text ", ") asts; text " { "; p a; text " }" ])
    | Exists(pos, (asts, a)) ->
        withTrivia map pos (concat [ text "ex "; list (text ", ") asts; text " { "; p a; text " }" ])
    | Exists1 () -> text "ex!"
    | ExistsN(pos, ((a1, asts), a2)) ->
        withTrivia map pos (concat [ text "exn"; p a1; text " "; list (text ", ") asts; text " { "; p a2; text " }" ])
    | IsOperator(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " is "; p a2 ])

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
        withTrivia map pos (concat [ text "("; p a; text ")" ])

    // Tuple-like constructs and qualifies
    | BrackedCoordList(pos, asts) ->
        withTrivia map pos (concat [ text "["; list (text ", ") asts; text "]" ])
    | ArgumentTuple(pos, asts) ->
        withTrivia map pos (concat [ text "("; list (text ", ") asts; text ")" ])
    | DottedPredicate(pos, a) ->
        withTrivia map pos (concat [ text "."; p a ])
    | QualificationList(pos, asts) ->
        withTrivia map pos (concat (asts |> List.map p))
    | ParamTuple asts ->
        concat [ text "("; list (text ", ") asts; text ")" ]

    // Commands
    | Delegate(a1, a2) ->
        concat [ text "del."; p a1; p a2 ]
    | Assertion(pos, a) ->
        withTrivia map pos (concat [ text "assert "; p a ])
    | Cases(pos, (asts, a)) ->
        withTrivia map pos
            (concat [ text "cases ("; line
                      indent (concat (asts |> List.map p))
                      p a; text ")" ])
    | CaseSingle(pos, (a, asts)) ->
        withTrivia map pos (concat [ text "| "; p a; text ": "; concat (asts |> List.map p); line ])
    | CaseElse(pos, asts) ->
        withTrivia map pos (concat [ text "? "; concat (asts |> List.map p) ])
    | MapCases(pos, (asts, a)) ->
        withTrivia map pos
            (concat [ text "mcases ("; line
                      indent (concat (asts |> List.map p))
                      p a; text ")" ])
    | MapCaseSingle(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "| "; p a1; text ": "; p a2; line ])
    | MapCaseElse(pos, a) ->
        withTrivia map pos (concat [ text "? "; p a ])
    | Assignment(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " := "; p a2 ])
    | ForIn(pos, ((a1, a2), asts)) ->
        withTrivia map pos
            (concat [ text "for "; p a1; text " "; p a2; text " {"; line
                      indent (concat (asts |> List.map p))
                      text "}" ])
    | InEntity(pos, a) ->
        withTrivia map pos (concat [ text "in "; p a ])
    | Return(pos, a) ->
        withTrivia map pos (concat [ text "return "; p a ])

    // Symbol extensions
    | SymbolDecl(pos, s) -> withTrivia map pos (concat [ text "symbol \""; text s; text "\"" ])
    | PrefixDecl(pos, s) -> withTrivia map pos (concat [ text "prefix \""; text s; text "\"" ])
    | PostfixDecl(pos, s) -> withTrivia map pos (concat [ text "postfix \""; text s; text "\"" ])
    | InfixDeclWithPrecedence(pos, (s, a)) ->
        withTrivia map pos (concat [ text "infix \""; text s; text "\" "; p a ])
    | Precedence(pos, n) -> withTrivia map pos (text (string n))
    | DefinitionExtension(pos, ((a1, a2), a3)) ->
        withTrivia map pos (concat [ text "ext "; p a1; p a2; text " {"; p a3; text "}" ])
    | ExtensionSignature(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; p a2 ])
    | ExtensionAssignment(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " @ /"; p a2; text "/" ])
    | ExtensionRegex s -> text s
    | ExtensionName(pos, s) -> withTrivia map pos (text s)

    // Definitions
    | DefinitionClass(pos, (((a1, a1Opt), a2Opt), a3)) ->
        withTrivia map pos
            (concat [ text "class "; p a1
                      opt (fun a -> concat [ text ": "; p a ]) a1Opt
                      opt (fun a -> concat [ text " "; p a ]) a2Opt
                      text " "; p a3 ])
    | ClassSignature(pos, a) ->
        withTrivia map pos (concat [ text "class "; p a ])
    | ClassDefinitionBlock(pos, tupOpt) ->
        withTrivia map pos
            (opt (fun (a, astsOpt) ->
                concat [ text "{"; line
                         indent (concat [ p a; opt (fun asts -> concat (asts |> List.map p)) astsOpt ])
                         text "}" ]) tupOpt)
    | DefClassCompleteContent(a, asts) ->
        concat [ p a; concat (asts |> List.map p) ]
    | Constructor(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | ConstructorSignature(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "constructor "; p a1; p a2 ])
    | ConstructorBlock a ->
        concat [ text "{"; p a; text "}" ]
    | BaseConstructorCall(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "base."; p a1; p a2 ])

    | DefinitionPredicate(pos, (a, tupOpt)) ->
        withTrivia map pos
            (concat [ p a
                      opt (fun (a1, astsOpt) ->
                          concat [ text " {"; line
                                   indent (concat [ p a1; opt (fun asts -> concat (asts |> List.map p)) astsOpt ])
                                   text "}" ]) tupOpt ])
    | PredicateSignature((pos, ((a1, a1Opt), a2)), a3Opt) ->
        withTrivia map pos
            (concat [ text "predicate "; p a1
                      opt (fun a -> concat [ text ": "; p a ]) a1Opt
                      p a2
                      opt p a3Opt ])
    | DefPredicateContent(a1, a2) ->
        concat [ p a1; p a2 ]

    | DefinitionFunctionalTerm(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | FunctionalTermSignature((pos, (((a1, a1Opt), a2), a3)), a4Opt) ->
        withTrivia map pos
            (concat [ text "function "; p a1
                      opt (fun a -> concat [ text ": "; p a ]) a1Opt
                      p a2; p a3
                      opt p a4Opt ])
    | Mapping(pos, a) ->
        withTrivia map pos (concat [ text "-> "; p a ])
    | FunctionalTermDefinitionBlock(pos, tupOpt) ->
        withTrivia map pos
            (opt (fun (a, astsOpt) ->
                concat [ text "{"; line
                         indent (concat [ p a; opt (fun asts -> concat (asts |> List.map p)) astsOpt ])
                         text "}" ]) tupOpt)
    | DefFunctionContent(a1, a2) ->
        concat [ p a1; p a2 ]

    | PredicateInstance(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ text "property "; p a; opt (fun a2 -> concat [ text " {"; p a2; text "}" ]) aOpt ])
    | PredicateInstanceSignature(pos, (a1, a2)) ->
        withTrivia map pos (concat [ text "predicate "; p a1; p a2 ])
    | FunctionalTermInstance(pos, (a, aOpt)) ->
        withTrivia map pos (concat [ text "property "; p a; opt (fun a2 -> concat [ text " {"; p a2; text "}" ]) aOpt ])
    | FunctionalTermInstanceSignature(pos, ((a1, a2), a3)) ->
        withTrivia map pos (concat [ text "function "; p a1; p a2; p a3 ])

    // Rules of inference
    | RuleOfInference(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | RuleOfInferenceSignature(pos, a) ->
        withTrivia map pos (concat [ text "inf "; p a ])
    | PremiseConclusionBlock(a1, (a2, a3)) ->
        concat [ text "{"; line
                 indent (concat [ p a1; p a2; p a3 ])
                 text "}" ]
    | PremiseList(pos, asts) ->
        withTrivia map pos (concat [ text "premise: "; list (text ", ") asts ])

    // Statements
    | Axiom(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; text " {"; line; indent (concat [ p a2; p a3 ]); text "}" ])
    | AxiomSignature(pos, a) ->
        withTrivia map pos (concat [ text "axiom "; p a ])
    | Conjecture(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; text " {"; line; indent (concat [ p a2; p a3 ]); text "}" ])
    | ConjectureSignature(pos, a) ->
        withTrivia map pos (concat [ text "conjecture "; p a ])
    | Theorem(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; text " {"; line; indent (concat [ p a2; p a3 ]); text "}" ])
    | TheoremSignature(pos, a) ->
        withTrivia map pos (concat [ text "theorem "; p a ])
    | Lemma(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; text " {"; line; indent (concat [ p a2; p a3 ]); text "}" ])
    | LemmaSignature(pos, a) ->
        withTrivia map pos (concat [ text "lemma "; p a ])
    | Proposition(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; text " {"; line; indent (concat [ p a2; p a3 ]); text "}" ])
    | PropositionSignature(pos, a) ->
        withTrivia map pos (concat [ text "proposition "; p a ])
    | Corollary(pos, (a1, (a2, a3))) ->
        withTrivia map pos (concat [ p a1; text " {"; line; indent (concat [ p a2; p a3 ]); text "}" ])
    | CorollarySignature(pos, (a, asts)) ->
        withTrivia map pos (concat [ text "corollary "; p a; concat (asts |> List.map p) ])

    // Proofs
    | Proof(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | ProofSignature(pos, (a, asts)) ->
        withTrivia map pos (concat [ text "proof "; p a; concat (asts |> List.map p) ])
    | ProofBlock a ->
        concat [ text "{"; line; indent (p a); text "}" ]
    | ProofContent((a1, asts), a2Opt) ->
        concat [ p a1; concat (asts |> List.map p); opt p a2Opt ]
    | Argument(pos, a) ->
        withTrivia map pos (p a)
    | JustArgInf(pos, (a1, a2)) ->
        withTrivia map pos (concat [ p a1; text " "; p a2 ])
    | StartArgument a ->
        p a
    | StartArgumentStictly(a, asts) ->
        concat [ p a; list (text ", ") asts ]
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
        withTrivia map pos (concat [ text "assume "; p a ])
    | RevokeArgument(pos, a) ->
        withTrivia map pos (concat [ text "revoke "; p a ])
    | Qed(pos, _) -> withTrivia map pos (text "qed")

    // Special references
    | Intrinsic(pos, _) -> withTrivia map pos (text "intrinsic")
    | Undefined(pos, _) -> withTrivia map pos (text "undefined")
    | SelfOrParent(pos, a) -> withTrivia map pos (p a)
    | Self(pos, _) -> withTrivia map pos (text "self")
    | Parent(pos, _) -> withTrivia map pos (text "parent")
    | Extension(pos, s) -> withTrivia map pos (concat [ text "@"; text s ])

    // Localizations
    | Localization((pos, a), asts) ->
        withTrivia map pos (concat [ text "loc "; p a; text " {"; line; indent (concat (asts |> List.map p)); text "}" ])
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
        withTrivia map pos (concat [ text "uses "; p a ])
    | BuildingBlock(pos, a) -> withTrivia map pos (p a)
    | ErrorSyntax(pos, s) -> withTrivia map pos (text s)
    | ErrorSyntaxBacktracking(pos, s) -> withTrivia map pos (text s)
    | ErrorSyntaxChain((pos, _), (s, _)) -> withTrivia map pos (text s)

/// <summary>
/// The entry point for the formatting service: pretty-prints a list of top-level building-block
/// ASTs (as returned by <c>Fpl1Parser.Main.fplParser</c>), honoring trivia recorded in <paramref name="map"/>.
/// </summary>
/// <param name="indentSize">The number of spaces to use per indentation level when rendering.</param>
/// <param name="map">The <see cref="TriviaMap"/> built from the parsed AST and discovered comments.</param>
/// <param name="asts">The top-level building-block AST nodes to render, in source order.</param>
/// <returns>
/// The fully rendered, canonically formatted FPL source text, with a blank line separating each
/// top-level building block.
/// </returns>
/// <remarks>
/// This is the only function in the module intended to be called by <c>Fpl3LanguageServer</c>'s
/// <c>FormattingHandler</c>. It maps <see cref="print"/> over each top-level node, inserts a blank
/// line between building blocks, and delegates final text composition to <see cref="Doc.render"/>.
/// </remarks>
let printAll (indentSize: int) (map: TriviaMap) (asts: Ast list) : string =
    asts
    |> List.map (print map)
    |> List.collect (fun d -> [ d; line; line ])   // blank line between top-level blocks
    |> concat
    |> render indentSize
