namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG00() =

    [<DataRow("""def pred Or (x,y:*pred[obj]) infix "∨" 0 {true}""", 0)>]
    [<DataRow("""def pred Or (x:* pred[ind]) infix "∨" 0 {true}""", 1)>]
    [<DataRow("""def pred Or (x,y,z:* pred[ind]) infix "∨" 0 {true}""", 1)>]
    [<DataRow("""def pred T() {true}""", 0)>]
    [<DataRow("""def pred T(x:obj) infix "+" 0 {true}""", 1)>]
    [<DataRow("""def pred T(x,y:obj) infix "+" 0{true}""", 0)>]
    [<DataRow("""def pred T(x,y,z:obj) infix "+" 0{true}""", 1)>]
    [<DataRow("""def pred T () prefix "+" {true}""", 1)>]
    [<DataRow("""def pred T(x:obj) prefix "+"  {true}""", 0)>]
    [<DataRow("""def pred T(x,y:obj) prefix "+" {true}""", 1)>]
    [<DataRow("""def pred T() postfix "+" {true}""", 1)>]
    [<DataRow("""def pred T(x:obj) postfix "+" {true}""", 0)>]
    [<DataRow("""def pred T(x,y:obj) postfix "+" {true}""", 1)>]
    [<DataRow("""def func T()->obj infix "+" 0{intr}""", 1)>]
    [<DataRow("""def func T(x:obj)->obj infix "+" 0{intr}""", 1)>]
    [<DataRow("""def func T(x,y:obj)->obj infix "+" 0{intr}""", 0)>]
    [<DataRow("""def func T(x,y,z:obj)->obj infix "+" 0{intr}""", 1)>]
    [<DataRow("""def func T()->obj prefix "+" {intr}""", 1)>]
    [<DataRow("""def func T(x:obj)->obj prefix "+" {intr}""", 0)>]
    [<DataRow("""def func T(x,y:obj)->obj prefix "+" {intr}""", 1)>]
    [<DataRow("""def func T()->obj postfix "+" {intr}""", 1)>]
    [<DataRow("""def func T(x:obj)->obj postfix "+" {intr}""", 0)>]
    [<DataRow("""def func T(x,y:obj)->obj postfix "+" {intr}""", 1)>]
    [<DataRow("""def func T(x,y:*pred[obj])->obj  infix "∨" 0 {intr}""", 0)>]
    [<DataRow("""def func T(x:* pred[ind])->obj  infix "∨" 0 {intr}""", 1)>]
    [<DataRow("""def func T(x,y,z:* pred[ind])->obj infix "∨" 0 {intr}""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG00(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG00 ("",0)
            runTestHelper "TestSIG00.fpl" fplCode code expected
