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
    [<DataRow("function(a,b:Obj)->obj")>]
    [<DataRow("function(a,b:Obj)->pred")>]
    [<DataRow("func(a:ind,b:pred)->func")>]
    [<DataRow("func(a:ind,b:pred)->func(x,y:Obj)->obj")>]
    [<DataRow("predicate()")>]
    [<DataRow("function()->obj")>]
    [<DataRow("predicate()")>]
    [<DataRow("pred(x:ind)")>]
    [<DataRow("pred(x:ind)")>]
    [<DataRow("@Nat")>]
    [<DataRow("pred(x , y, z:Obj)")>]
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


    [<DataRow("template(x:ind,y:ind)")>]
    [<DataRow("template(x:ind,y:T)->obj")>]
    [<DataRow("tpl()")>]
    [<DataRow("template(x:func()->func()->func()->func()->ind)")>]
    [<DataRow("tpl(x,y,z:pred())")>]
    [<DataRow("templateTest(x,y,z:pred())")>]
    [<DataRow("tplTest(x:ind)")>]
    [<DataRow("tplTest(x:ind)")>]
    [<DataRow("templateTest()")>]
    [<DataRow("tplTest(x:Obj)")>]
    [<DataRow("tpl(a:ind,b:pred,c:Obj,d:@T,e:T,f:tpl)")>]
    [<DataRow("tplTest  (x:ind)")>]
    [<DataRow("tplTest(x:Obj())")>]
    [<DataRow("Set(y:Nat)")>]
    [<DataRow("SomeClass[x:ind]")>]
    [<DataRow("object[]")>]
    [<DataRow("object[x:ind]")>]
    [<DataRow("obj[x,y:pred]")>]
    [<DataRow("object()")>]
    [<DataRow("obj()")>]
    [<DataRow("function ")>]
    [<DataRow("func ")>]
    [<DataRow("predicate ")>]
    [<DataRow("pred ")>]
    [<DataRow("func[]")>]
    [<DataRow("pred[]")>]
    [<DataRow("index[obj]")>]
    [<DataRow("index(x:Obj)")>]
    [<DataRow("SomeClass[1]")>]
    [<DataRow("@Nat()")>]
    [<DataRow("predicate ()")>]
    [<DataRow("@Nat[y:ind]")>]
    [<DataRow("index[x:ind,y:ind]")>]
    [<DataRow("SomeClass[y:ind,z:ind]")>]
    [<DataRow("object [x,y,z]")>]
    [<DataRow("obj[x,y:Obj]")>]
    [<DataRow("function[x:Obj]")>]
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
    [<DataRow("func()->obj()")>]
    [<DataRow("func()->tpl()")>]
    [<TestMethod>]
    member this.TestVariableTypeFailure (input:string) =
        let result = run (variableType .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))




        