namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG02() =

    [<DataRow("""def pred T (x,y:obj) infix "+" 1 {intr}""", 0)>]
    [<DataRow("""def pred T1 (x,y:obj) infix "+" 1 {intr} def pred T2 (x,y:obj) infix "+" 1 {intr}""", 1)>]
    [<DataRow("""def pred T1  (x,y: obj) infix "+" 2 {intr} def pred T2 (x,y: obj) infix "*" 1 {intr}""", 0)>]
    [<DataRow("""def func T (x,y:obj)->obj infix "+" 1 {intr}""", 0)>]
    [<DataRow("""def func T1 (x,y:obj)->obj infix "+" 1 {intr} def pred T2 (x,y:obj) infix "+" 1 {intr}""", 1)>]
    [<DataRow("""def func T1  (x,y: obj)->obj infix "+" 2 {intr} def pred T2 (x,y: obj) infix "*" 1 {intr}""", 0)>]
    [<DataRow("""def pred T1 (x,y:obj) infix "+" 1 {intr} def func T2 (x,y:obj)->obj infix "+" 1 {intr}""", 1)>]
    [<DataRow("""def pred T1  (x,y: obj) infix "+" 2 {intr} def func T2 (x,y: obj)->obj infix "*" 1 {intr}""", 0)>]
    [<DataRow("""def func T1 (x,y:obj)->obj infix "+" 1 {intr} def func T2 (x,y:obj)->obj infix "+" 1 {intr}""", 1)>]
    [<DataRow("""def func T1  (x,y: obj)->obj infix "+" 2 {intr} def func T2 (x,y: obj)->obj infix "*" 1 {intr}""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG02(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG02 ("",0, "")
            runTestHelper "TestSIG02.fpl" fplCode code expected
