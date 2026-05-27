// This console "main" program is for test/debugging purposes only.
// It is not really needed because the necessary FPL modules are run 
// as an FPL Language Server (see FplLS C# Project in the same solution).

open FParsec
open FplParsing.Combinators
//open ErrDiagnostics
//open FplParsing.Main
//open FplInterpreter.Globals.Heap
//open FplInterpreter.Main
//open System.IO

//let deleteFilesWithExtension dir extension =
//    if Directory.Exists(dir) then
//        Directory.GetFiles(dir, "*." + extension)
//        |> Array.iter File.Delete
//    else
//        printfn "Directory %s does not exist." dir


//let prepareFplCode(fplCode:string, delete:bool) =
//    let currDir = Directory.GetCurrentDirectory()

//    File.WriteAllText(Path.Combine(currDir, "Test.fpl"), fplCode)
//    let uri = PathEquivalentUri(Path.Combine(currDir, "Test.fpl"))
//    let fplLibUrl =
//        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
//    if delete then 
//        deleteFilesWithExtension currDir "fpl"
//    else
//        fplInterpreter fplCode uri fplLibUrl

//let loadFplFile(path:string) = 
//    let uri = PathEquivalentUri(path)
//    let fplLibUrl =
//        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
//    let fplCode = File.ReadAllText(path)
//    fplInterpreter fplCode uri fplLibUrl

//let input = """def pred T() { intr prty pred T1() {is(parent,pred)} };"""


//let result = fplParser input

//printf "%O" result

//ad.PrintDiagnostics

//prepareFplCode(input,false) |> ignore


//// loadFplFile(@"C:\Users\Peaq\source\repos\bookofproofs\fpl.net\theories\FoundationsOfAnalysisLandau\Landau.1.1.Axioms.fpl")
//// loadFplFile(@"D:\Forschung\fpl.net\theories\FoundationsOfAnalysisLandau\Landau.1.1.Axioms.fpl")

//printf "\n--------------------------------\n"
//ad.PrintDiagnostics
//printf "%s" (heap.SymbolTable.ToJson())

// run (expression .>> eof) "false ∧ true"

// expr   ::= term ( "+" term )*
// term   ::= "a" | "(" expr ")"
// whitespace is ignored

type Expr = 
    | A
    | B
    | Add of Expr list
    | Sub of Expr list
    | Parens of Expr

let ws = spaces

// --- literals -------------------------------------------------------------

let pLiteral : Parser<Expr, unit> =
    ws >>. (
        (pchar 'a' >>% A)
        <|>
        (pchar 'b' >>% B)
    ) .>> ws

// --- forward declaration --------------------------------------------------

let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

// --- parentheses ----------------------------------------------------------

let pParens : Parser<Expr, unit> =
    between
        (ws >>. pchar '(' >>. ws)
        (ws >>. pchar ')' >>. ws)
        pExpr
    |>> Parens

// --- term -----------------------------------------------------------------

let pTerm : Parser<Expr, unit> =
    pLiteral <|> pParens

// --- operators ------------------------------------------------------------

let pPlus  = ws >>. pchar '+' .>> ws
let pMinus = ws >>. pchar '-' .>> ws

// --- full expression ------------------------------------------------------

let pFullExpr : Parser<Expr, unit> =
    pipe2
        pTerm
        (many ( (pPlus <|> pMinus) .>>. pTerm ))
        (fun first rest ->
            match rest with
            | [] -> first
            | _ ->
                // Partition by operator
                let plusTerms  = rest |> List.choose (function ('+', t) -> Some t | _ -> None)
                let minusTerms = rest |> List.choose (function ('-', t) -> Some t | _ -> None)

                match plusTerms, minusTerms with
                | [], [] ->
                    first

                | _ , [] ->
                    // Only additions
                    Add (first :: plusTerms)

                | [], _ ->
                    // Only subtractions
                    Sub (first :: minusTerms)

                | _ , _ ->
                    // Mixed + and -: preserve exact sequence
                    // Example: a + b - c + d
                    // becomes: Add [a; b] and Sub [c; d]
                    // but we must preserve order, so we build a combined AST:
                    let rec buildSeq acc currentOp currentList = function
                        | [] ->
                            match currentOp with
                            | '+' -> Add (List.rev currentList) :: acc |> List.rev
                            | '-' -> Sub (List.rev currentList) :: acc |> List.rev
                            | _   -> failwith "Unexpected operator"

                        | (op, term)::xs ->
                            if op = currentOp then
                                buildSeq acc currentOp (term :: currentList) xs
                            else
                                let node =
                                    match currentOp with
                                    | '+' -> Add (List.rev currentList)
                                    | '-' -> Sub (List.rev currentList)
                                    | _   -> failwith "Unexpected operator"

                                buildSeq (node :: acc) op [term] xs

                    let seq = buildSeq [] '+' [first] rest

                    // If the sequence has only one node, return it directly
                    match seq with
                    | [single] -> single
                    | many     -> Add many // top-level grouping
        )

do pExprRef := pFullExpr

// --- entry point ----------------------------------------------------------

let parse input =
    run (pExpr .>> eof) input

let res = parse "(a + b) - (a - b)"

printfn "%O" res

let res1 = parse "a + b - a + b"
printfn "%O" res1

let res2 = parse "(a + b - a + b)"
printfn "%O" res2
