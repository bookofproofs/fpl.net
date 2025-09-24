namespace FplParser.Tests

open FParsec
open FplParser
open FplPrimitives
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestVariableTypes () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow("SomeClass")>]
    [<DataRow("@Nat")>]
    [<DataRow("template")>]
    [<DataRow(LiteralTpl)>]
    [<DataRow("templateTest")>]
    [<DataRow("tplTest")>]
    [<DataRow("object[]")>]
    [<DataRow("object[x:ind]")>]
    [<DataRow("obj[x,y:pred]")>]
    [<DataRow("function(a,b:obj)->obj")>]
    [<DataRow("function(a,b:obj)->pred")>]
    [<DataRow("func(a:ind,b:pred)->func")>]
    [<DataRow("func(a:ind,b:pred)->func(x,y:obj)->obj")>]
    [<DataRow("predicate()")>]
    [<DataRow("SomeClass[x:ind]")>]
    [<DataRow("template(x:ind,y:ind)")>]
    [<DataRow("template(x:ind,y:T)->obj")>]
    [<DataRow("tpl()")>]
    [<DataRow("templateTest()")>]
    [<DataRow("tplTest(x:obj())")>]
    [<DataRow("tpl(a:ind,b:pred,c:obj,d:@T,e:T,f:tpl)")>]
    [<DataRow("function()->obj")>]
    [<DataRow("predicate ()")>]
    [<DataRow("pred(x:ind)")>]
    [<DataRow("template(x:func()->func()->func()->func()->ind)")>]
    [<DataRow("tpl(x,y,z:pred())")>]
    [<DataRow("templateTest(x,y,z:pred())")>]
    [<DataRow("pred(x:ind)")>]
    [<DataRow("tplTest  (x:ind)")>]
    [<DataRow("@Nat")>]
    [<DataRow("tplTest  (x:ind)")>]
    [<DataRow("Set(y:Nat)")>]
    [<DataRow("pred(x , y, z:obj)")>]
    [<DataRow("object()")>]
    [<DataRow("obj()")>]
    [<DataRow("function ")>]
    [<DataRow("func ")>]
    [<DataRow("predicate ")>]
    [<DataRow("pred ")>]
    [<DataRow("index")>]
    [<DataRow("ind")>]
    [<DataRow("template")>]
    [<DataRow("tpl")>]
    [<DataRow("SomeClass")>]
    [<DataRow("templateTest")>]
    [<DataRow("tplTest")>]
    
    [<TestMethod>]
    member this.TestVariableTypeSuccess (input:string) =
        let result = run (variableType .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("func[]")>]
    [<DataRow("pred[]")>]
    [<DataRow("index[obj]")>]
    [<DataRow("index(x:obj)")>]
    [<DataRow("SomeClass[1]")>]
    [<DataRow("@Nat()")>]
    [<DataRow("@Nat[y:ind]")>]
    [<DataRow("index[x:ind,y:ind]")>]
    [<DataRow("SomeClass[y:ind,z:ind]")>]
    [<DataRow("object [x,y,z]")>]
    [<DataRow("obj[x,y:obj]")>]
    [<DataRow("function[x:obj]")>]
    [<DataRow("SomeClass[y:ind,z:ind]")>]
    [<DataRow("index()")>]
    [<DataRow("ind[SomeClass]")>]
    [<DataRow("index()")>]
    [<DataRow("SomeClass[x:SomeOtherClass]")>]
    [<DataRow("@Nat[]")>]
    [<DataRow("Nat()()")>]
    [<DataRow("index()")>]
    [<DataRow("pred()()")>]
    [<DataRow("tpl[x:index   ]")>]
    [<DataRow("templateTest[x:ind ,y:pred]")>]
    [<DataRow("tplTest[]")>]
    [<TestMethod>]
    member this.TestVariableTypeFailure (input:string) =
        let result = run (variableType .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))




        