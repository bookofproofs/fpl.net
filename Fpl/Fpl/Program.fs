open FplGrammarTypes
open ErrorHandling
open FplParser
open FParsec




// ~~~~~~~~~~~~~ The Parser ~~~~~~~~~~~~~~~~~~





let result = tryParse vDash "s|-"

printfn "%O" result