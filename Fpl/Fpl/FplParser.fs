module FplParser
open FplGrammarTypes
open ErrorHandling
open FParsec

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

(* Whitespaces and Comments *)
let IW = spaces
let SW = spaces1
let inlineComment = pstring "//" >>. skipManyTill anyChar (skipNewline <|> eof) |>> ignore <?> "<line-comment>"
let blockComment = (pstring "/*" >>. (skipManyTill anyChar (pstring "*/"))) |>> ignore <?> "<multiline-comment>"
let CW = expect (choice [ 
            SW 
            blockComment 
            inlineComment 
         ]) "expecting whitespace, inline comment, or block comment" |>> SyntaxNode.CW