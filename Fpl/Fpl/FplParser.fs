module FplParser
open FplGrammarTypes
open ErrorHandling
open FParsec

(* IMPORTANT NOTE: This is the unstable and incomplete new version of the FPL parser,
where we are trying to add error recovery. 


For the current stable version see FplGrammar.fs that has no error recovery in it.

This version is unstable because some of the unit tests in Fpl/FplParserTests/TestComments.fs fail.
This version is incomplete because it only covers a tiny bit of the FPL grammar.
*)

(* Literals *)
let leftBrace = expect (skipString "{") "missing opening '{'" |>> SyntaxNode.LeftBrace
let rightBrace = expect (skipString "}") "missing closing '}'" |>> SyntaxNode.RightBrace
let leftParen = expect (skipString "(") "missing opening '('" |>> SyntaxNode.LeftParen
let rightParen = expect (skipString ")") "missing closing ')'" |>> SyntaxNode.RightParen
let leftBracket = expect (skipString "[") "missing opening '['" |>> SyntaxNode.LeftBracket
let rightBracket = expect (skipString "]") "missing closing ']'" |>> SyntaxNode.RightBracket
let comma = expect (skipChar ',') "expecting ','" |>> SyntaxNode.Comma
let star = expect (skipChar '*') "zero or more qualifier'*'" |>> SyntaxNode.Star
let plus = expect (skipChar '+') "one or more qualifier '+'" |>> SyntaxNode.Plus
let dot = expect (skipChar '.') "expecting point '.'" |>> SyntaxNode.Dot
let colon = expect (skipChar ':') "expecting colon ':'" |>> SyntaxNode.Colon
let colonEqual = expect (skipString ":=") "expecting assignment ':='" |>> SyntaxNode.ColonEqual
let at = expect (skipChar '@') "expecting '@'" |>> SyntaxNode.At
let exclamationMark = expect (skipChar '!') "expecting '!'" |>> SyntaxNode.ExclamationMark
let case = expect (skipChar '|') "expecting '|'" |>> SyntaxNode.Case
let tilde = expect (skipChar '~') "expecting '~'" |>> SyntaxNode.Tilde
let semiColon = expect (skipChar ';') "expecting ';'" |>> SyntaxNode.Semicolon
let dollar = expect (skipChar '$') "expecting '$'" |>> SyntaxNode.Dollar
let map = expect (skipString "->") "expecting map '->'" |>> SyntaxNode.Map
let vDash = expect (skipString "|-") "expecting '|-'" |>> SyntaxNode.VDash
let inlineCommentStart = expect (skipString "//") "expecting '//'" |>> SyntaxNode.InlineCommentStart
let blockCommentStart = expect (skipString "/*") "expecting '/*'" |>> SyntaxNode.BlockCommentStart
let blockCommentEnd = skipString "*/"  

(* Whitespaces and Comments *)
let IW = spaces
let SW = expect spaces1 "whitespace" |>> SyntaxNode.SignificantWS
let inlineComment = expect (inlineCommentStart >>. skipManyTill anyChar (skipNewline <|> eof)) "inlineComment" |>> SyntaxNode.InlineComment
let blockComment = expect (blockCommentStart >>. (skipManyTill anyChar blockCommentEnd)) "missing closing '*/'" |>> SyntaxNode.BlockComment
let CW = expect (attempt ((attempt SW) <|> blockComment) <|> inlineComment ) "expecting whitespace, inlineComment, or blockComment" |>> SyntaxNode.CW