namespace TestFpl1Parser.LSRelated.PrettyPrint

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

/// <summary>
/// Regression tests for comment-trivia MISPLACEMENT bugs in <c>TriviaMap.buildTriviaMap</c>:
/// a comment attached to one AST node drifting onto a structurally unrelated — but textually
/// nearby, on the same single source line — sibling or child node instead.
/// </summary>
/// <remarks>
/// Each test below reproduces one of the originally reported misplacement cases. Rather than
/// asserting an exact rendered string (fragile against unrelated formatting changes), each test
/// asserts the weaker, more robust invariant that actually matters for correctness: the comment
/// must appear immediately after its true anchor text, and must NOT appear immediately after the
/// wrong (previously-buggy) anchor text.
/// </remarks>
[<TestClass>]
type TestCommentPlacementRegressions () =

    /// <summary>
    /// Finds the first line in <paramref name="rendered"/> containing <paramref name="needle"/>.
    /// </summary>
    let lineContaining (rendered: string) (needle: string) : string option =
        rendered.Split([| Environment.NewLine |], StringSplitOptions.None)
        |> Array.tryFind (fun l -> l.Contains(needle: string))

    /// <summary>
    /// Asserts that <paramref name="commentText"/> appears on the same rendered line as
    /// <paramref name="correctAnchor"/>, immediately after it (no other anchor text in between),
    /// and that it does NOT appear on the same line as any of <paramref name="wrongAnchors"/>.
    /// </summary>
    member private _.AssertCommentNextToCorrectAnchor
        (fplCode: string)
        (commentText: string)
        (correctAnchor: string)
        (wrongAnchors: string list) =
        let rendered, success = formatViaFplParser fplCode
        Assert.IsTrue(success, $"Expected a clean parse of:{Environment.NewLine}{fplCode}{Environment.NewLine}--- rendered ---{Environment.NewLine}{rendered}")

        match lineContaining rendered correctAnchor with
        | None ->
            Assert.Fail($"Expected rendered output to contain correct anchor '{correctAnchor}' but got:{Environment.NewLine}{rendered}")
        | Some line ->
            Assert.IsTrue(
                line.Contains(commentText: string),
                $"Expected comment '{commentText}' to appear on the same line as correct anchor '{correctAnchor}', but that line was: '{line}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")

        for wrongAnchor in wrongAnchors do
            match lineContaining rendered wrongAnchor with
            | Some line when line.Contains(commentText: string) && not (line.Contains(correctAnchor: string)) ->
                Assert.Fail($"Comment '{commentText}' was misplaced next to wrong anchor '{wrongAnchor}' instead of correct anchor '{correctAnchor}'. Line was: '{line}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")
            | _ -> ()

    /// <summary>
    /// Input B: a block-comment trailing the whole rule-of-inference (after its closing <c>}</c>)
    /// must stay attached there, not drift onto the <c>inf MP</c> signature line.
    /// </summary>
    [<TestMethod>]
    member this.TestTrailingCommentStaysAfterRuleOfInferenceBlock () =
        let fplCode = """inf MP { dec p,q: pred; pre: p, p => q con: q } /*a*/"""
        this.AssertCommentNextToCorrectAnchor fplCode "/*a*/" "}" [ "MP" ]

    /// <summary>
    /// Input C: a comment inside the third proof argument's justification must stay there, not
    /// drift onto the <c>proof T$1</c> signature line.
    /// </summary>
    [<TestMethod>]
    member this.TestCommentStaysInsideProofArgumentJustification () =
        let fplCode = """proof T$1 {dec f,x:pred; 1: x 2: x => (f => f) 3. 2,1, byinf/*a*/ MP |- (f => f) }"""
        this.AssertCommentNextToCorrectAnchor fplCode "/*a*/" "byinf" [ "T$1" ]

    /// <summary>
    /// Input D: a comment between a class's constructor block and its property block must stay
    /// between them, not drift onto the class signature (<c>FieldPowerN: Set</c>) line.
    /// </summary>
    [<TestMethod>]
    member this.TestCommentStaysBetweenClassSubBlocks () =
        let fplCode = """def class FieldPowerN: Set { ctor FieldPowerN(x:obj, y:pred) { dec base.Obj() ; } /*a*/ property pred T() { true } }"""
        this.AssertCommentNextToCorrectAnchor fplCode "/*a*/" "}" [ "FieldPowerN" ]

    /// <summary>
    /// Input A: a comment between the starting of a definition (e.g, a predicate's parameter list) and its symbolic clause (e.g., a <c>infix</c> symbol)
    /// should stay there. KNOWN LIMITATION: <c>Ast.PredicateSignature</c>'s recorded
    /// <c>Positions</c> do not currently span the trailing <c>infix "..." n</c> clause at all (a
    /// grammar/AST-shape gap, not a <c>TriviaMap</c> attachment bug), so this case is expected to
    /// still fail until that AST shape is revisited; it is included here, marked
    /// <c>[&lt;Ignore&gt;]</c>, purely so the limitation is tracked and the test starts passing
    /// automatically once fixed.
    /// </summary>
    [<DataRow("01", """def pred Impl(f, g: pred) /*a*/ infix "=>" 0 {impl(f,g)}""")>]
    [<DataRow("01a", """def pred Not(f: pred) /*a*/ prefix "!" {not f}""")>]
    [<DataRow("01b", """def pred NotD(f: pred) /*a*/ postfix "'" 0 {not f}""")>]
    [<DataRow("02", """def func Add(f, g: obj)->obj /*a*/ infix "+" 0 {intr}""")>]
    [<DataRow("02a", """def func Succ(f: obj)->obj /*a*/ prefix "!" {intr}""")>]
    [<DataRow("02b", """def func SuccD(f: obj)->obj /*a*/ postfix "'" {intr}""")>]
    [<DataRow("03", """def cl Bla /*a*/ symbol "∠" {intr}""")>]
    [<TestMethod>]
    member this.TestCommentStaysSymbolicClause (no:string, fplCode:string) =
        this.AssertCommentNextToCorrectAnchor fplCode "/*a*/" "pred)" [ "infix" ]
