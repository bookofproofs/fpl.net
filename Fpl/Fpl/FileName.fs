module Flop
// Flop - Sometimes you fail, and that's OK
//
// This is a small investigation into error handling and recovery in FParsec
// based on the ideas discussed by @ebkalderon in [Error recovery with parser
// combinators (using nom)](https://www.eyalkalderon.com/nom-error-recovery/).
//
// For a discussion behind this code check out the blog post [Error Recovery in
// FParsec](https://willspeak.me/2020/09/10/error-recovery-in-fparsec.html).

open FParsec

/// Sructured error information to be emitted by our parser.
type Diagnostic = Diagnostic of Position * string

/// The output of our parse.
type SyntaxNode =
    | ValueInt of int64 option
    | Variable of float option
    | Number of SyntaxNode option
    | Product of SyntaxNode * SyntaxNode
    | Sum of SyntaxNode * SyntaxNode
    | Error

/// Binary Node Constructor. Converts the head::tail list from a binary parser
/// into a `SyntaxNode` by recursively applying `cons`. This has the property of
/// not wrapping the inner nodes if no `cdr` is at this level for a simpler
/// syntax tree.
let rec private binNode cons (car, cdr) =
    match cdr with
    | [] -> car
    | [single] -> cons(car, single)
    | head::tail -> binNode cons (cons(car, head), tail)

/// Our Parser State. Used to keep track of the diagnostics we encountered while
/// parsing the source text so far.
type State =
    { mutable Diagnostics: Diagnostic list }

    /// Emit Diagnostic
    member s.EmitDiagnostic pos err =
        let diag = Diagnostic(pos, err)
        s.Diagnostics <- diag::s.Diagnostics

    /// Initial parser state
    static member Initial = { Diagnostics = [] }

// ~~~~~~~~~~~~~ Error Handling Constructs ~~~~~~~~~~~~~~~~~~
//
// These parsers are responsible for handling and syncrhonising after errors are
// encountered.

/// Expect a given parser to match in the current location. If the parser fails
/// the given `err` is emitted as a diagnostic at the current location.
let expect p err =
    let raiseErr (stream: CharStream<State>) =
        stream.UserState.EmitDiagnostic stream.Position err
        Reply(None)
    attempt (p |>> Some) <|> raiseErr

/// A variant of `expect` that resolves parser failures with the `Error` syntax
/// node rather than returning an `option`al value.
let expectSyn (p: Parser<SyntaxNode, State>) err =
    expect p err |>> Option.defaultValue Error

/// Error syncrhonisation. used to parse any characters and emit them as
/// diagnostics. This is `<|>`ed into the standard combinator chain right at the
/// root to try and ensure the parser _always_ has a way to succeed.
let error (stream: CharStream<State>) =
    match stream.ReadCharOrNewline() with
    | EOS -> Reply(ReplyStatus.Error, expected "valid expression character")
    | ch ->
        sprintf "Unexpected character %c" ch
        |> stream.UserState.EmitDiagnostic stream.Position
        Reply(Error)

// ~~~~~~~~~~~~~ The Parser ~~~~~~~~~~~~~~~~~~

let expr, exprRef = createParserForwardedToRef()

let variable = expect pfloat "expecting variable" |>> SyntaxNode.Variable
let intValue = expect pint64 "expecting int" |>> SyntaxNode.ValueInt
let number = expect (attempt variable <|> intValue) "int or float expected" |>> SyntaxNode.Number 

let value =
     number <|> between (pchar '(') (expect (pchar ')') "missing closing ')'") (expectSyn expr "expected expression after opening (")

let product =
    let op = pchar '*' <|> pchar '/'
    (number .>>. (many (op >>. expectSyn number "expected expression after operator"))) |>> (binNode Product)

let sum =
    let op = pchar '+' <|> pchar '-'
    (product .>>. (many (op >>. expectSyn product "expected expression after operator"))) |>> (binNode Sum)

exprRef := sum

let parser = (expr <|> error) .>> eof

/// Runs the parser on the input string and throws an exception if the parser
/// fails. We expect the parser should _always_ succeed. For malformed source
/// text an `SyntaxNode.Error` should be returned and a `Diagnostic` emitted.
let private parseExpr input =
    match runParserOnString parser State.Initial "test" input with
    | ParserResult.Failure(_) as f -> failwithf "Parser failed! %A" f
    | ParserResult.Success(r, s, _) -> (r, s.Diagnostics)

let private test input =
    parseExpr input
    |> printfn "'%s' ~> %A" input

let main argv =

    test "123"
    test "1*2"
    test "1+3*4"
    test "(1+2)*3"
    test "1+"
    test "(1"
    test "()"
    test "("
    test "(*1)+2)"

    0 // return an integer exit code
