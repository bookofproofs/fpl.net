namespace FplParser.Tests

open FParsec
open FplParser
open FplGrammarCommons
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestVariableTypes () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestVariableType001 () =
        let result = run (variableType .>> eof) """object"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType002 () =
        let result = run (variableType .>> eof) """obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType003 () =
        let result = run (variableType .>> eof) literalFuncL
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType004 () =
        let result = run (variableType .>> eof) literalFunc
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType005 () =
        let result = run (variableType .>> eof) """predicate"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType006 () =
        let result = run (variableType .>> eof) """pred"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType007 () =
        let result = run (variableType .>> eof) """index"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType008 () =
        let result = run (variableType .>> eof) """ind"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType009 () =
        let result = run (variableType .>> eof) """SomeClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType010 () =
        let result = run (variableType .>> eof) """@Nat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType011 () =
        let result = run (variableType .>> eof) """template"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType012 () =
        let result = run (variableType .>> eof) """tpl"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType013 () =
        let result = run (variableType .>> eof) """templateTest"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType014 () =
        let result = run (variableType .>> eof) """tplTest"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType001a() =
        let result = run (variableType .>> eof) """object """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType002a() =
        let result = run (variableType .>> eof) """obj """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType003a() =
        let result = run (variableType .>> eof) """function """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType004a() =
        let result = run (variableType .>> eof) """func """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType005a() =
        let result = run (variableType .>> eof) """predicate """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType006a() =
        let result = run (variableType .>> eof) """pred """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType007a() =
        let result = run (variableType .>> eof) """index """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType008a() =
        let result = run (variableType .>> eof) """ind """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType009a () =
        let result = run (variableType .>> eof) """SomeClass """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType010a () =
        let result = run (variableType .>> eof) """@Nat """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType011a () =
        let result = run (variableType .>> eof) """template """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType012a () =
        let result = run (variableType .>> eof) """tpl """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType013a () =
        let result = run (variableType .>> eof) """templateTest """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType014a () =
        let result = run (variableType .>> eof) """tplTest """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType015 () =
        let result = run (variableType .>> eof) """object[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType015a () =
        let result = run (variableType .>> eof) """object[x:ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestVariableType015b () =
        let result = run (variableType .>> eof) """func[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType015c () =
        let result = run (variableType .>> eof) """pred[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType016 () =
        let result = run (variableType .>> eof) """obj[x,y:pred]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType017 () =
        let result = run (variableType .>> eof) """function(a,b:obj)->obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType017a () =
        let result = run (variableType .>> eof) """function(a,b:obj)->pred"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType018 () =
        let result = run (variableType .>> eof) """func(a:ind,b:pred)->func"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType018a () =
        let result = run (variableType .>> eof) """func(a:ind,b:pred)->func(x,y:obj)->obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType019 () =
        let result = run (variableType .>> eof) """predicate()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType020 () =
        let result = run (variableType .>> eof) """pred"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType021 () =
        let result = run (variableType .>> eof) """index[obj]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType022 () =
        let result = run (variableType .>> eof) """ind"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType023 () =
        let result = run (variableType .>> eof) """SomeClass[x:ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType023a () =
        let result = run (variableType .>> eof) """SomeClass[1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType024 () =
        let result = run (variableType .>> eof) """@Nat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType024a () =
        let result = run (variableType .>> eof) """@Nat()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType024b () =
        let result = run (variableType .>> eof) """@Nat[y:ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType025 () =
        let result = run (variableType .>> eof) """template[x:ind,y:ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType025a () =
        let result = run (variableType .>> eof) """template[x:ind,y:2]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType026 () =
        let result = run (variableType .>> eof) """tpl[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType027 () =
        let result = run (variableType .>> eof) """templateTest()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType028 () =
        let result = run (variableType .>> eof) """tplTest[()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType029 () =
        let result = run (variableType .>> eof) """object[a:ind,b:pred,c:obj,d:@T,e:T,f:tpl]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType030 () =
        let result = run (variableType .>> eof) """pred[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType031 () =
        let result = run (variableType .>> eof) """function()->obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType032 () =
        let result = run (variableType .>> eof) """func[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType033 () =
        let result = run (variableType .>> eof) """predicate []"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType034 () =
        let result = run (variableType .>> eof) """pred(x:ind)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType035 () =
        let result = run (variableType .>> eof) """index[ind,ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType036 () =
        let result = run (variableType .>> eof) """ind[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType037 () =
        let result = run (variableType .>> eof) """SomeClass[y:ind,z:ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType038 () =
        let result = run (variableType .>> eof) """@Nat[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType039 () =
        let result = run (variableType .>> eof) """template[x:func()->func()->func()->func()->ind]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType040 () =
        let result = run (variableType .>> eof) """tpl[x,y,z:pred()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType041 () =
        let result = run (variableType .>> eof) """templateTest(x,y,z:pred())"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType042 () =
        let result = run (variableType .>> eof) """tplTest  (ind)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType043 () =
        let result = run (variableType .>> eof) """object [x,y,z]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType044 () =
        let result = run (variableType .>> eof) """obj[x,y:obj]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType045 () =
        let result = run (variableType .>> eof) """function[x:obj]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType046 () =
        let result = run (variableType .>> eof) literalFunc
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType047 () =
        let result = run (variableType .>> eof) """predicate"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType048 () =
        let result = run (variableType .>> eof) """pred(x , y, z:obj)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType049 () =
        let result = run (variableType .>> eof) """index()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType050 () =
        let result = run (variableType .>> eof) """ind[SomeClass]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType051 () =
        let result = run (variableType .>> eof) """SomeClass[x:SomeOtherClass]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"));

    [<TestMethod>]
    member this.TestVariableType052 () =
        let result = run (variableType .>> eof) """@Nat[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType053 () =
        let result = run (variableType .>> eof) """@Nat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType054 () =
        let result = run (variableType .>> eof) """tpl[x:index   ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType055 () =
        let result = run (variableType .>> eof) """templateTest[x:ind ,y:pred]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType056 () =
        let result = run (variableType .>> eof) """tplTest[]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariableType057 () =
        let result = run (variableType .>> eof) """Nat()()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType058 () =
        let result = run (variableType .>> eof) """pred()()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariableType059 () =
        let result = run (variableType .>> eof) """Set(y:Nat)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

module TestModule1 =
    let main () = printfn "This is a Testmodule"
    main ()

        