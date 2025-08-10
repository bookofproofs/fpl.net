namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestKeywordSpaces() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestSpacesIn () =
        let result = run (statement .>> eof) """for n inxomeType
            (
                x := 1
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesFor () =
        let result = run (statement .>> eof) """forx in SomeType
            (
                x := 1
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))


    [<TestMethod>]
    member this.TestSpacesReturn () =
        let result = run (returnStatement .>> eof) """returnx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesRet () =
        let result = run (returnStatement .>> eof) """retx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))



    [<TestMethod>]
    member this.TestSpacesAll () =
        let result = run (all .>> eof) """allx p"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesAssert () =
        let result = run (assertionStatement .>> eof) """assertp"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesAssume () =
        let result = run (assumeArgument .>> eof) """assumex"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesAss () =
        let result = run (assumeArgument .>> eof) """assx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesBydef () =
        let result = run (byDefinition .>> eof) """bydefp"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesCases () =
        let result = run (casesStatement .>> eof) """cases(|true:x:=1?x:=0)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesCasesA () =
        let result = run (casesStatement .>> eof) """cases (|true:x:=1?x:=0)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalConL)>]
    [<DataRow(literalCon)>]
    [<TestMethod>]
    member this.TestSpacesConclusion (word:string) =
        let result = run (conclusion .>> eof) $"""{word}:true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalConL)>]
    [<DataRow(literalCon)>]
    [<TestMethod>]
    member this.TestSpacesConclusionA (word:string) =
        let result = run (conclusion .>> eof) $"""{word} :true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalConL)>]
    [<DataRow(literalCon)>]
    [<TestMethod>]
    member this.TestSpacesConclusionB (word:string) =
        let result = run (conclusion .>> eof) $"""{word}: true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesDeclaration () =
        let result = run (varDeclBlock .>> eof) """declaration ~a:obj ;"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesDeclarationA () =
        let result = run (varDeclBlock .>> eof) """declaration~a:obj ;"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesDec () =
        let result = run (varDeclBlock .>> eof) """dec ~a:obj ;"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesDecA () =
        let result = run (varDeclBlock .>> eof) """dec~a:obj ;"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesDelegate () =
        let result = run (fplDelegate .>> eof) """delegate.Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesDelegateA () =
        let result = run (fplDelegate .>> eof) """delegate .Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesDelegateB () =
        let result = run (fplDelegate .>> eof) """delegate. Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesDel () =
        let result = run (fplDelegate .>> eof) """del.Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesDelA () =
        let result = run (fplDelegate .>> eof) """del .Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesDelB () =
        let result = run (fplDelegate .>> eof) """del. Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesExt () =
        let result = run (keywordExtension .>> eof) literalExt
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesExtA () =
        let result = run (keywordExtension .>> eof) """ext """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesExtB () =
        let result = run (keywordExtension .>> eof) """extension """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesExtC () =
        let result = run (keywordExtension .>> eof) literalExtL
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpacesExistsTimesN () =
        let result = run (existsTimesN .>> eof) """exn$1x p"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesEx () =
        let result = run (exists .>> eof) """exx p"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(literalFalse)>]
    [<DataRow("true")>]
    [<DataRow("undefined")>]
    [<DataRow("undef")>]
    [<TestMethod>]
    member this.TestSpacesFalseTrueUndef (word:string) =
        let result = run (predicate .>> eof) $"""and({word},true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalFalse)>]
    [<DataRow("true")>]
    [<DataRow("undefined")>]
    [<DataRow("undef")>]
    [<TestMethod>]
    member this.TestSpacesFalseTrueUndefA (word:string) =
        let result = run (predicate .>> eof) $"""and({word}, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalFalse)>]
    [<DataRow("true")>]
    [<DataRow("undefined")>]
    [<DataRow("undef")>]
    [<TestMethod>]
    member this.TestSpacesFalseTrueUndefB (word:string) =
        let result = run (predicate .>> eof) $"""and({word}A)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<DataRow(literalImpl)>]
    [<DataRow("xor")>]
    [<DataRow(literalAnd)>]
    [<DataRow("or")>]
    [<DataRow(literalIif)>]
    [<TestMethod>]
    member this.TestSpacesParenthesizedPredicate (word:string) =
        let result = run (predicate .>> eof) $"""{word}(false,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalImpl)>]
    [<DataRow("xor")>]
    [<DataRow(literalAnd)>]
    [<DataRow("or")>]
    [<DataRow(literalIif)>]
    [<TestMethod>]
    member this.TestSpacesParenthesizedPredicateA (word:string) =
        let result = run (predicate .>> eof) $"""{word} (false,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalImpl)>]
    [<DataRow("xor")>]
    [<DataRow(literalAnd)>]
    [<DataRow("or")>]
    [<DataRow(literalIif)>]
    [<TestMethod>]
    member this.TestSpacesParenthesizedPredicateB (word:string) =
        let result = run (predicate .>> eof) $"""{word}A(false,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("whitespace>"))

    [<DataRow(literalIntrL)>]
    [<DataRow(literalIntr)>]
    [<TestMethod>]
    member this.TestSpacesIntrinsic (word:string) =
        let result = run (keywordIntrinsic .>> eof) $"""{word}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalIntrL)>]
    [<DataRow(literalIntr)>]
    [<TestMethod>]
    member this.TestSpacesIntrinsicA (word:string) =
        let result = run (keywordIntrinsic .>> eof) $"""{word} """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesIs () =
        let result = run (isOperator .>> eof) $"""is(x,obj)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesIsA () =
        let result = run (isOperator .>> eof) $"""is (x,obj)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesNot () =
        let result = run (negation .>> eof) """notx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow("premise")>]
    [<DataRow("pre")>]
    [<TestMethod>]
    member this.TestSpacesPremise (word:string) =
        let result = run (premise .>> eof) $"""{word}:true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("premise")>]
    [<DataRow("pre")>]
    [<TestMethod>]
    member this.TestSpacesPremiseA (word:string) =
        let result = run (premise .>> eof) $"""{word} :true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("premise")>]
    [<DataRow("pre")>]
    [<TestMethod>]
    member this.TestSpacesPremiseB (word:string) =
        let result = run (premise .>> eof) $"""{word}: true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("return")>]
    [<DataRow("ret")>]
    [<TestMethod>]
    member this.TestSpacesReturn (word:string) =
        let result = run (returnStatement .>> eof) $"""{word}x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow("revoke")>]
    [<DataRow("rev")>]
    [<TestMethod>]
    member this.TestSpacesRevoke (word:string) =
        let result = run (revokeArgument .>> eof) $"""{word}100."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow("theorem")>]
    [<DataRow("thm")>]
    [<DataRow("lemma")>]
    [<DataRow("lem")>]
    [<DataRow("proposition")>]
    [<DataRow("prop")>]
    [<DataRow(literalConjL)>]
    [<DataRow(literalConj)>]
    [<DataRow(literalAxL)>]
    [<DataRow(literalAx)>]
    [<DataRow("postulate")>]
    [<DataRow("post")>]
    [<TestMethod>]
    member this.TestSpacesBuildingBlock (word:string) =
        let result = run (buildingBlock .>> eof) $"""{word}X(){true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(literalDefL)>]
    [<DataRow(literalDef)>]
    [<TestMethod>]
    member this.TestSpacesDefinition (word:string) =
        let result = run (definition .>> eof) (word + """class:obj{intr}""")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(literalClL)>]
    [<DataRow(literalCl)>]
    [<TestMethod>]
    member this.TestSpacesClass (word:string) =
        let result = run (definition .>> eof) ("def " + word + "T:obj{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(literalClL)>]
    [<DataRow(literalCl)>]
    [<TestMethod>]
    member this.TestSpacesClassWithSpace (word:string) =
        let result = run (definition .>> eof) ("def " + word + " T:obj{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("predicate")>]
    [<DataRow("pred")>]
    [<TestMethod>]
    member this.TestSpacesPredicate (word:string) =
        let result = run (definition .>> eof) ("def " + word + "X(){intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow("predicate")>]
    [<DataRow("pred")>]
    [<TestMethod>]
    member this.TestSpacesPredicateWithSpace (word:string) =
        let result = run (definition .>> eof) ("def " + word + " X(){intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalFuncL)>]
    [<DataRow(literalFunc)>]
    [<TestMethod>]
    member this.TestSpacesFunctionalTerm (word:string) =
        let result = run (definition .>> eof) ("def " + word + "X()->obj{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("Expecting: <significant whitespace>"))

    [<DataRow(literalFuncL)>]
    [<DataRow(literalFunc)>]
    [<TestMethod>]
    member this.TestSpacesFunctionalTermWithSpace (word:string) =
        let result = run (definition .>> eof) ("def " + word + " X()->obj{intr}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(literalFuncL)>]
    [<DataRow(literalFunc)>]
    [<DataRow("object")>]
    [<DataRow("obj")>]
    [<DataRow("predicate")>]
    [<DataRow("pred")>]
    [<DataRow(literalIndL)>]
    [<DataRow(literalInd)>]
    [<TestMethod>]
    member this.TestSpacesSimpleType (word:string) =
        let result = run (varDecl .>> eof) ("~a:" + word + "x")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<TestMethod>]
    member this.TestSpacesQed () =
        let result = run (proof .>> eof) "proof T$1{1.|- trivial qed}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesQedA () =
        let result = run (proof .>> eof) "proof T$1{1.|- trivial qedx}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<TestMethod>]
    member this.TestSpacesTrivial () =
        let result = run (proof .>> eof) "proof T$1{1.|- trivial qed}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesTrivialA () =
        let result = run (proof .>> eof) "proof T$1{1.|- trivialx qed}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<TestMethod>]
    member this.TestSpacesSelf () =
        let result = run (selfOrParent .>> eof) "self"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpacesSelfA () =
        let result = run (selfOrParent .>> eof) "selfx}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))

    [<TestMethod>]
    member this.TestSpacesUses() =
        let result = run (usesClause .>> eof) "usesx A}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<TestMethod>]
    member this.TestSpacesAlias() =
        let result = run (usesClause .>> eof) "uses A aliasx B}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(literalCorL)>]
    [<DataRow(literalCor)>]
    [<TestMethod>]
    member this.TestSpacesCorollary (word:string) =
        let result = run (corollary .>> eof) ($"{word}x" + " T$1() { true }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow("proof")>]
    [<DataRow("prf")>]
    [<TestMethod>]
    member this.TestSpacesProof (word:string) =
        let result = run (proof .>> eof) ($"{word}x" + " T$1 {1. |- true }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(literalCtorL)>]
    [<DataRow(literalCtor)>]
    [<TestMethod>]
    member this.TestSpacesConstructor (word:string) =
        let result = run (constructor .>> eof) ($"{word}x" + " T() { self }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow(literalInfL)>]
    [<DataRow(literalInf)>]
    [<TestMethod>]
    member this.TestSpacesInference (word:string) =
        let result = run (ruleOfInference .>> eof) ($"{word}x" + " T() { pre:true con:true }")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow("localization")>]
    [<DataRow("loc")>]
    [<TestMethod>]
    member this.TestSpacesLocalization (word:string) =
        let result = run (localization .>> eof) ($"{word}x" + " T() := !tex: x;")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow("property")>]
    [<DataRow("prty")>]
    [<TestMethod>]
    member this.TestSpacesProperty (word:string) =
        let result = run (property .>> eof) ($"{word}x" + " pred T() {true}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow("optional")>]
    [<DataRow("opt")>]
    [<TestMethod>]
    member this.TestSpacesOptional (word:string) =
        let result = run (property .>> eof) ($"property pred {word}x" + " T() {true}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<significant whitespace>"))

    [<DataRow("prefix")>]
    [<DataRow(literalInfix)>]
    [<DataRow("postfix")>]
    [<TestMethod>]
    member this.TestSpacesSomeFixNotation (word:string) =
        let result = run (definition .>> eof) ($"def pred T {word}x \"~\"" + " () {true}")
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:") && actual.Contains("<whitespace>"))
