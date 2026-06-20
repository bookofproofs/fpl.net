namespace Diagnostics.LogicRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestLG005() =

    [<DataRow("00", """def pred T() { dec x:obj x:=x; true }""", 1)>]
    [<DataRow("01", """def pred T() { dec x,y:pred x(x):=x(x); true }""", 1)>]
    [<DataRow("02", """def pred T() { dec x,y:pred x(y):=x(y); true }""", 1)>]
    [<DataRow("03", """def pred T() { dec x:obj Succ(x):=Succ(x); true }""", 1)>]
    [<DataRow("04", """def pred T() { dec x,y:pred Succ(x,x):=Succ(x,y); true }""", 0)>]
    [<DataRow("05", """def pred T() { dec x,y:pred Succ(y,y):=Succ(y,y); true }""", 1)>]
    [<DataRow("arr0", """def pred T() { dec i:ind x:*pred[ind] x[i]:=x[i]; true }""", 1)>]
    [<DataRow("arr1", """def pred T() { dec i,j:ind x:*pred[ind] x[i]:=x[j]; true }""", 0)>]
    [<DataRow("arr2", """def pred T() { dec i,j:ind x:*pred[ind,ind] x[i,j]:=x[i , j]; true }""", 1)>]
    [<DataRow("arr3", """def pred T() { dec i,j:ind x:*pred[ind,ind] x[i,j]:=x[j , i]; true }""", 0)>]
    [<DataRow("arr4", """def pred T() { dec i:ind j:obj x:*pred[ind,obj] x[i,j]:=x[j , i]; true }""", 0)>]
    [<DataRow("arr5", """def pred T() { dec i:ind j:obj x:*pred[ind,obj] x[i,j]:=x[ i ,j]; true }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestLG005(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG005 "" 
            runTestHelper "TestLG005.fpl" fplCode code expected
