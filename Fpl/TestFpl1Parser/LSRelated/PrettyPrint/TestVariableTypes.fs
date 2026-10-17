namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestVariableTypes () =

    [<DataRow("01", LiteralObjL)>]
    [<DataRow("02", LiteralObj)>]
    [<DataRow("03", LiteralFuncL)>]
    [<DataRow("04", LiteralFunc)>]
    [<DataRow("05", LiteralPredL)>]
    [<DataRow("06", LiteralPred)>]
    [<DataRow("07", LiteralIndL)>]
    [<DataRow("08", LiteralInd)>]
    [<DataRow("09", "SomeClass")>]
    [<DataRow("10", "Nat")>]
    [<DataRow("11", "template")>]
    [<DataRow("12", LiteralTpl)>]
    [<DataRow("13", "templateTest")>]
    [<DataRow("14", "tplTest")>]
    [<DataRow("15", "function(a,b:obj)->obj")>]
    [<DataRow("16", "function(a,b:obj)->pred")>]
    [<DataRow("17", "*function(a,b:obj)->* pred [ind] [obj]")>]
    [<DataRow("18", "*function(a,b:obj)->pred[tpl]")>]
    [<DataRow("19", "func(a:ind,b:pred)->func")>]
    [<DataRow("20", "func(a:ind,b:pred)->func(x,y:obj)->obj")>]
    [<DataRow("21", "predicate()")>]
    [<DataRow("22", "function()->obj")>]
    [<DataRow("23", "*predicate()[Nat]")>]
    [<DataRow("24", "pred(x:ind)")>]
    [<DataRow("25", "*pred(x:ind)[ ind ,ind]")>]
    [<DataRow("26", "pred(x , y, z:obj)")>]
    [<DataRow("27", "*pred(x , y, z:obj) [ obj]")>]
    [<DataRow("28", "index")>]
    [<DataRow("29", "ind")>]
    [<DataRow("30", "*ind[ind]")>]
    [<DataRow("31", "template")>]
    [<DataRow("32", "tpl")>]
    [<DataRow("33", "*tpl[Nat]")>]
    [<DataRow("34", "SomeClass")>]
    [<DataRow("35", "*SomeClass[obj]")>]
    [<DataRow("36", "templateTest")>]
    [<DataRow("37", "tplTest")>]
    [<DataRow("38", "*tplTest[ind,ind,ind]")>]
    [<TestMethod>]
    member _.TestVariableTypeSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments variableType fplCode

