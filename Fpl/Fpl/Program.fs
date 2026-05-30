// This console "main" program is for test/debugging purposes only.
// It is not really needed because the necessary FPL modules are run 
// as an FPL Language Server (see FplLS C# Project in the same solution).
open FParsec
open FplPrimitives

// ============================================================================
// AST
// ============================================================================

type Expr =
    | A
    | B
    | Parens of Expr
    | Prefix of string * Expr
    | Postfix of Expr * string
    | Infix of Expr * string * Expr
    | Call of Expr * Expr list      // a(...)
    | Coord of Expr * Expr list     // a[...]

// ============================================================================
// Whitespace control
// ============================================================================

let whiteSpaces = anyOf " \t\r\n"

// No whitespace allowed at this point
let NW : Parser<unit,unit> =
    notFollowedBy (skipMany1 whiteSpaces) <?> "<no whitespace>"

// At least one whitespace required
let SW : Parser<unit,unit> =
    skipMany1 whiteSpaces >>% () <?> "<significant whitespace>"

// Optional whitespace (for comma lists etc.)
let IW : Parser<unit,unit> =
    skipMany whiteSpaces >>% () <?> "<whitespace>"

// ============================================================================
// Operator sets
// ============================================================================

// Prefix operators: - ~ #
let prefixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "-~#".Contains(c))

// Postfix operators: ! ' /
let postfixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "!'/".Contains(c))

// Infix operators: + - * /
let infixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "+-*/".Contains(c))

// ============================================================================
// Forward declaration
// ============================================================================

let pExpr, pExprRef = createParserForwardedToRef<Expr,unit>()

// ============================================================================
// Literals and base atoms
// ============================================================================

let leftPar = skipChar '(' .>> IW
let rightPar = skipChar ')'
let com = skipChar ','
let leftBra = skipChar '[' .>> IW
let rightBra = skipChar ']'


let pLiteral : Parser<Expr,unit> =
    (pchar 'a' >>% A)
    <|>
    (pchar 'b' >>% B)

// Parenthesized expression
let pParens : Parser<Expr,unit> =
    between leftPar rightPar pExpr |>> Parens

// ============================================================================
// Expr list for arguments / coordinates
// ============================================================================

let pExprList : Parser<Expr list,unit> =
    sepBy pExpr (IW >>. com >>. IW)

// ============================================================================
// Call / Coord suffixes on literals (no space allowed before '(' or '[')
// ============================================================================

let pCallOrCoordSuffix : Parser<(Expr -> Expr),unit> =
    NW >>.
    choice [
        // a(expr, ...)
        leftPar >>. pExprList .>> rightPar 
        |>> fun args -> fun basis -> Call(basis, args)

        // a[expr, ...]
        leftBra >>. pExprList .>> rightBra 
        |>> fun coords -> fun basis -> Coord(basis, coords)
    ]

// Literal extended by optional call/coord chains
let pOperandAtom : Parser<Expr,unit> =
    pipe2
        pLiteral
        (many (attempt pCallOrCoordSuffix))
        (fun lit suffixes ->
            List.fold (fun acc f -> f acc) lit suffixes
        )

// Atom: either extended literal or parenthesized expression
let pAtom : Parser<Expr,unit> =
    pOperandAtom
    <|> pParens

// ============================================================================
// Precedence: prefix > postfix > infix
// (calls/coords are part of the atom, i.e. tighter than prefix/postfix)
// ============================================================================

// PREFIX: prefix* atom
let pPrefixExpr : Parser<Expr,unit> =
    pipe2
        (many (attempt (
            prefixMathSymbols .>> NW
            )) <?> "<prefix operator>" // suppress low-level backtracking errors with a label
        )
        pAtom
        (fun prefixes atom ->
            List.foldBack (fun op acc -> Prefix(op, acc)) prefixes atom
        )

// POSTFIX: prefixExpr postfix*
let pPostfixExpr : Parser<Expr,unit> =
    pipe2
        pPrefixExpr
        (many (attempt (
            NW >>. postfixMathSymbols
            )) <?> "<postfix operator>" // suppress low-level backtracking errors with a label
        )
        (fun expr postfixes ->
            List.fold (fun acc op -> Postfix(acc, op)) expr postfixes
        )

// INFIX: postfixExpr (space infixOp space postfixExpr)*
let pInfixExpr : Parser<Expr,unit> =
    pipe2
        pPostfixExpr
        // We wrap the infix tail in attempt so that if we see a space
        // that is not followed by a valid infix operator,
        // we roll back and let the comma separator handle that space (allowing productions like "a(b ,a)"):
        (many (attempt (
            SW >>. infixMathSymbols .>> SW .>>. pPostfixExpr
            )) <?> "<infix operator>" // suppress low-level backtracking errors with a label
        )
        (fun first rest ->
            List.fold (fun acc (op, rhs) -> Infix(acc, op, rhs)) first rest
        )

pExprRef.Value <- pInfixExpr

// ============================================================================
// Entry point
// ============================================================================

let parse input =
    run (pExpr .>> eof) input

// ============================================================================
// Some example inputs you can try:
//
//parse "a"
//parse "a(b,a)"
//parse "a[b,a]"
//parse "b[(a + b!)   ,   -b',a(b) ,b, a]"
//parse "a(-b ,~a)"
//parse "~-a(b)[a, b!]/ b"
// ============================================================================```

let res = parse "(a + b)! - -(a - b)"
printfn "%O" res

let resa = parse "(a + b)! --(a - b)"
printfn "%O" resa

let resb = parse "(a + b)!! -- (a - b)"
printfn "%O" resb

let res1 = parse "a! + ~b - a + b"
printfn "%O" res1

let res2 = parse "-(a + b - a + b)'"
printfn "%O" res2

let res2a = parse "a"
printfn "%O" res2a

let res2b = parse "a(b,a)"
printfn "%O" res2b

let res2c = parse "a[b,a]"
printfn "%O" res2c

let res2d = parse "b[(a + b!)   ,   -b',a(b) ,b, a]"
printfn "%O" res2d

let res2e = parse "a(-b ,~a)"
printfn "%O" res2e

let res2e_ = parse "a(-b, ~a)"
printfn "%O" res2e_

let res2f = parse "~-a(b)[a, b!]/ b"
printfn "%O" res2f

let res2f_ = parse "~-a(b)[a, b!]/ + b"
printfn "%O" res2f_

let res2g = parse "a/ b"
printfn "%O" res2g

let res3 = parse "a[]"
printfn "%O" res3

let res3a = parse "a[ ]"
printfn "%O" res3a

let res4 = parse "a()"
printfn "%O" res4

let res4a = parse "a( )"
printfn "%O" res4a

