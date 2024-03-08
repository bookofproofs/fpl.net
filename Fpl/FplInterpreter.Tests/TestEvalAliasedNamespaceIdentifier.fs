namespace FplInterpreter.Tests

open FParsec
open FplInterpreter
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestEvalAliasedNamespaceIdentifier() = 

    [<DataRow("*", "Test1", "Test1*.fpl")>]
    [<DataRow("*", "Test1.Test2", "Test1.Test2*.fpl")>]
    [<DataRow("T1", "Test1", "Test1.fpl")>]
    [<DataRow("T2", "Test1.Test2", "Test1.Test2.fpl")>]
    [<DataRow("", "Test1", "Test1.fpl")>]
    [<DataRow("", "Test1.Test2", "Test1.Test2.fpl")>]
    [<TestMethod>]
    member this.TestFileNamePattern(aliasOrStar:string, pascelCaseId:string, expected:string) = 
        let eval = 
            if aliasOrStar="" then
                { 
                    StartPos = Position("",1,1,1)
                    EndPos = Position("",1,1,1)
                    AliasOrStar = None
                    PascalCaseIdList = [pascelCaseId] 
                }
            else
                { 
                    StartPos = Position("",1,1,1)
                    EndPos = Position("",1,1,1)
                    AliasOrStar = Some aliasOrStar
                    PascalCaseIdList = [pascelCaseId] 
                }
        Assert.AreEqual(expected, eval.FileNamePattern)

    [<DataRow("*", "Test1", "Test1")>]
    [<DataRow("*", "Test1.Test2", "Test1.Test2")>]
    [<DataRow("T1", "Test1", "T1")>]
    [<DataRow("T2", "Test1.Test2", "T2")>]
    [<DataRow("", "Test1", "Test1")>]
    [<DataRow("", "Test1.Test2", "Test1.Test2")>]
    [<TestMethod>]
    member this.TestName(aliasOrStar:string, pascelCaseId:string, expected:string) = 
        let eval = 
            if aliasOrStar="" then
                { 
                    StartPos = Position("",1,1,1)
                    EndPos = Position("",1,1,1)
                    AliasOrStar = None
                    PascalCaseIdList = [pascelCaseId] 
                }
            else
                { 
                    StartPos = Position("",1,1,1)
                    EndPos = Position("",1,1,1)
                    AliasOrStar = Some aliasOrStar
                    PascalCaseIdList = [pascelCaseId] 
                }
        Assert.AreEqual(expected, eval.Name)