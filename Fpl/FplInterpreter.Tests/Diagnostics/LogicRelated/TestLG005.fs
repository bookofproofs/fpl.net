namespace Diagnostics.LogicRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* LG005
   Purpose: Warn when an assignment is redundant and will be ignored.
   What it indicates: The code contains an assignment to an identifier that has no effect (the assignment will be implicitly ignored by the interpreter).
   Use: Helps authors find and remove or adjust redundant assignments to avoid confusion or unintended behavior.
   Action / Treat: Remove or correct the unnecessary assignment so the code's intent is explicit; LG005 is emitted as a warning. *)

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
