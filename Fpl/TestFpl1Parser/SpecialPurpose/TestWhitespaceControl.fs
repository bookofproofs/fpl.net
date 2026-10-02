namespace TestFpl1Parser.SpecialPurpose

open FParsec
open Fpl0Base.Primitives
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestWhitespaceControl() =

    // ---- Group 1: pure Success/Failure checks (no other condition), grouped by parser ----

    [<DataRow("1", """cases(|true:x:=1?x:=0)""")>]
    [<DataRow("2", """cases (|true:x:=1?x:=0)""")>]
    [<TestMethod>]
    member this.TestCasesStatementSuccess (id:string, input:string) =
        let result = run (casesStatement .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("1", """declaration a:obj ;""")>]
    [<DataRow("2", """declaration a:obj ;""")>]
    [<DataRow("3", """dec a:obj ;""")>]
    [<DataRow("4", """dec a:obj ;""")>]
    [<TestMethod>]
    member this.TestVarDeclOrSpecListSuccess (id:string, input:string) =
        let result = run (varDeclOrSpecList .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("1", """delegate.Test()""")>]
    [<DataRow("2", """del.Test()""")>]
    [<TestMethod>]
    member this.TestFplDelegateSuccess (id:string, input:string) =
        let result = run (fplDelegate .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("1", """delegate .Test()""")>]
    [<DataRow("2", """delegate. Test()""")>]
    [<DataRow("3", """del .Test()""")>]
    [<DataRow("4", """del. Test()""")>]
    [<TestMethod>]
    member this.TestFplDelegateFailure (id:string, input:string) =
        let result = run (fplDelegate .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("1", """ext """)>]
    [<DataRow("2", """extension """)>]
    [<TestMethod>]
    member this.TestKeywordExtensionSuccess (id:string, input:string) =
        let result = run (keywordExtension .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("1", LiteralExt)>]
    [<DataRow("2", LiteralExtL)>]
    [<TestMethod>]
    member this.TestKeywordExtensionFailure (id:string, input:string) =
        let result = run (keywordExtension .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("1", """is(x,obj)""")>]
    [<DataRow("2", """is (x,obj)""")>]
    [<TestMethod>]
    member this.TestIsOperatorSuccess (id:string, input:string) =
        let result = run (isOperator .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("1", "proof T$1{1: trivial qed}")>]
    [<DataRow("2", "proof T$1{1: trivial qed}")>]
    [<TestMethod>]
    member this.TestProofSuccess (id:string, input:string) =
        let result = run (proof .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSelfOrParentSuccess () =
        let result = run (selfOrParent .>> eof) LiteralSelf
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReturnStatementFailure () =
        let result = run (returnStatement .>> eof) """retx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    // ---- Group 2: Failure checks with an additional condition, grouped by parser AND condition ----

    [<DataRow("1", """for n inxomeType ( x := 1)""")>]
    [<DataRow("2", """forx in SomeType(x := 1)""")>]
    [<TestMethod>]
    member this.TestStatementFailureSignificantWhitespace (id:string, input:string) =
        let result = run (statement .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestReturnStatementFailureSignificantWhitespace () =
        let result = run (returnStatement .>> eof) """returnx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestAllFailureSignificantWhitespace () =
        let result = run (all .>> eof) """allx p"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestAssertionStatementFailureSignificantWhitespace () =
        let result = run (assertionStatement .>> eof) """assertp"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow("1", """assumex""")>]
    [<DataRow("2", """assx""")>]
    [<TestMethod>]
    member this.TestAssumeArgumentFailureSignificantWhitespace (id:string, input:string) =
        let result = run (assumeArgument .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralByCor)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralByAx)>]
    [<DataRow(LiteralByInf)>]
    [<TestMethod>]
    member this.TestSpacesBydef (keyword:string) =
        let result = run (byModifier .>> eof) $"{keyword}p"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralCon)>]
    [<TestMethod>]
    member this.TestSpacesConclusion (word:string) =
        let result = run (conclusion .>> eof) $"""{word}:true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralCon)>]
    [<TestMethod>]
    member this.TestSpacesConclusionA (word:string) =
        let result = run (conclusion .>> eof) $"""{word} :true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralCon)>]
    [<TestMethod>]
    member this.TestSpacesConclusionB (word:string) =
        let result = run (conclusion .>> eof) $"""{word}: true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExistsTimesNFailureSignificantWhitespace () =
        let result = run (existsTimesN .>> eof) """exn$1x p"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestExistsFailureSignificantWhitespace () =
        let result = run (exists .>> eof) """exx p"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralUndef)>]
    [<TestMethod>]
    member this.TestSpacesFalseTrueUndef (word:string) =
        let result = run (predicate .>> eof) $"""and({word},true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralUndef)>]
    [<TestMethod>]
    member this.TestSpacesFalseTrueUndefA (word:string) =
        let result = run (predicate .>> eof) $"""and({word}, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralUndef)>]
    [<TestMethod>]
    member this.TestSpacesFalseTrueUndefB (word:string) =
        let result = run (predicate .>> eof) $"""and({word}A)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow(LiteralIif)>]
    [<TestMethod>]
    member this.TestSpacesParenthesizedPredicate (word:string) =
        let result = run (predicate .>> eof) $"""{word}(false,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow(LiteralIif)>]
    [<TestMethod>]
    member this.TestSpacesParenthesizedPredicateA (word:string) =
        let result = run (predicate .>> eof) $"""{word} (false,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow(LiteralIif)>]
    [<TestMethod>]
    member this.TestSpacesParenthesizedPredicateB (word:string) =
        let result = run (predicate .>> eof) $"""{word}A(false,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("whitespace>"))

    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIntr)>]
    [<TestMethod>]
    member this.TestSpacesIntrinsic (word:string) =
        let result = run (keywordIntrinsic .>> eof) $"""{word}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIntr)>]
    [<TestMethod>]
    member this.TestSpacesIntrinsicA (word:string) =
        let result = run (keywordIntrinsic .>> eof) $"""{word} """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestNegationFailureSignificantWhitespace () =
        let result = run (negation .>> eof) """notx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralPre)>]
    [<TestMethod>]
    member this.TestSpacesPremise (word:string) =
        let result = run (premiseList .>> eof) $"""{word}:true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralPre)>]
    [<TestMethod>]
    member this.TestSpacesPremiseA (word:string) =
        let result = run (premiseList .>> eof) $"""{word} :true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralPre)>]
    [<TestMethod>]
    member this.TestSpacesPremiseB (word:string) =
        let result = run (premiseList .>> eof) $"""{word}: true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRet)>]
    [<TestMethod>]
    member this.TestSpacesReturn (word:string) =
        let result = run (returnStatement .>> eof) $"""{word}x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralRev)>]
    [<TestMethod>]
    member this.TestSpacesRevoke (word:string) =
        let result = run (revokeArgument .>> eof) $"""{word}100."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralThmL)>]
    [<DataRow(LiteralThm)>]
    [<DataRow(LiteralLemL)>]
    [<DataRow(LiteralLem)>]
    [<DataRow(LiteralPropL)>]
    [<DataRow(LiteralProp)>]
    [<DataRow(LiteralConjL)>]
    [<DataRow(LiteralConj)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralPostL)>]
    [<DataRow(LiteralPost)>]
    [<TestMethod>]
    member this.TestSpacesBuildingBlock (word:string) =
        let result = run (buildingBlock .>> eof) $"""{word}X(){true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralDefL)>]
    [<DataRow(LiteralDef)>]
    [<TestMethod>]
    member this.TestSpacesDefinition (word:string) =
        let result = run (definition .>> eof) (word + """class:obj{intr}""")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCl)>]
    [<TestMethod>]
    member this.TestSpacesClass (word:string) =
        let result = run (definition .>> eof) ("def " + word + "T{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCl)>]
    [<TestMethod>]
    member this.TestSpacesClassWithSpace (word:string) =
        let result = run (definition .>> eof) ("def " + word + " T{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPred)>]
    [<TestMethod>]
    member this.TestSpacesPredicate (word:string) =
        let result = run (definition .>> eof) ("def " + word + "X(){intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPred)>]
    [<TestMethod>]
    member this.TestSpacesPredicateWithSpace (word:string) =
        let result = run (definition .>> eof) ("def " + word + " X(){intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralFunc)>]
    [<TestMethod>]
    member this.TestSpacesFunctionalTerm (word:string) =
        let result = run (definition .>> eof) ("def " + word + "X()->obj{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralFunc)>]
    [<TestMethod>]
    member this.TestSpacesFunctionalTermWithSpace (word:string) =
        let result = run (definition .>> eof) ("def " + word + " X()->obj{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralInd)>]
    [<TestMethod>]
    member this.TestSpacesSimpleType (word:string) =
        let result = run (varDecl .>> eof) ("a:" + word + "x")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<DataRow("1", "proof T$1{1: trivial qedx}")>]
    [<DataRow("2", "proof T$1{1: trivialx qed}")>]
    [<TestMethod>]
    member this.TestProofFailureWhitespace (id:string, input:string) =
        let result = run (proof .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<TestMethod>]
    member this.TestSelfOrParentFailureWhitespace () =
        let result = run (selfOrParent .>> eof) "selfx}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<DataRow("1", "usesx A}")>]
    [<DataRow("2", "uses A aliasx B}")>]
    [<TestMethod>]
    member this.TestUsesClauseFailureSignificantWhitespace (id:string, input:string) =
        let result = run (usesClause .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestSpacesCorollary (word:string) =
        let result = run (corollary .>> eof) ($"{word}x" + " T$1() { true }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralPrf)>]
    [<DataRow(LiteralPrf)>]
    [<TestMethod>]
    member this.TestSpacesProof (word:string) =
        let result = run (proof .>> eof) ($"{word}x" + " T$1 {1: true }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralCtorL)>]
    [<DataRow(LiteralCtor)>]
    [<TestMethod>]
    member this.TestSpacesConstructor (word:string) =
        let result = run (constructor .>> eof) ($"{word}x" + " T() { self }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralInfL)>]
    [<DataRow(LiteralInf)>]
    [<TestMethod>]
    member this.TestSpacesInference (word:string) =
        let result = run (ruleOfInference .>> eof) ($"{word}x" + " T() { pre:true con:true }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestSpacesLocalization (word:string) =
        let result = run (localization .>> eof) ($"{word}x" + " T() := !tex: x")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralPrtyL)>]
    [<DataRow(LiteralPrty)>]
    [<TestMethod>]
    member this.TestSpacesProperty (word:string) =
        let result = run (definitionProperty .>> eof) ($"{word}x" + " pred T() {true}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(LiteralPrefix)>]
    [<DataRow(LiteralPostFix)>]
    [<TestMethod>]
    member this.TestSpacesSomeFixNotation (word:string) =
        let result = run (definition .>> eof) ($"def pred T(){word}\"-\"" + "{true}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(LiteralInfix)>]
    [<TestMethod>]
    member this.TestSpacesInfixNotation (word:string) =
        let result = run (definition .>> eof) ($"def pred T(){word}\"-\" 2" + "{true}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
