namespace Diagnostics.VariableRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestVAR10() =

    [<DataRow("ex00", """def pred T() { ex x:obj { (x = x) } }""", 0)>]
    [<DataRow("ex00a", """def pred T() { ex x:obj { not In(y, x) } }""", 0)>]
    [<DataRow("ex00b", """def pred In(a,b:tpl) def pred T() { ex x:obj { not In(y, x) } }""", 0)>]
    [<DataRow("ex01", """def pred T() { and(x, ex x:obj {true}) }""", 1)>]
    [<DataRow("ex01a", """def pred T() { and(y, ex x:obj {true}) }""", 0)>]
    [<DataRow("ex02", """def pred T() { or(x, ex x:obj {true}) }""", 1)>]
    [<DataRow("ex02a", """def pred T() { or(y, ex x:obj {true}) }""", 0)>]
    [<DataRow("ex03", """def pred T() { impl(x, ex x:obj {true}) }""", 1)>]
    [<DataRow("ex03a", """def pred T() { impl(y, ex x:obj {true}) }""", 0)>]
    [<DataRow("ex04", """def pred T() { iif(x, ex x:obj {true}) }""", 1)>]
    [<DataRow("ex04a", """def pred T() { iif(y, ex x:obj {true}) }""", 0)>]
    [<DataRow("ex05", """def pred T() { xor(x, ex x:obj {true}) }""", 1)>]
    [<DataRow("ex05a", """def pred T() { xor(y, ex x:obj {true}) }""", 0)>]
    [<DataRow("ex06", """def pred T() { not and(x, ex x:obj {true}) }""", 1)>]
    [<DataRow("ex06a", """def pred T() { not and(y, ex x:obj {true}) }""", 0)>]
    [<DataRow("ex07", """def pred T() { ex z:obj {and(x, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex07a", """def pred T() { ex z:obj {and(y, ex x:obj {true})} }""", 0)>]
    [<DataRow("ex07b", """def pred T() { ex x:obj {and(y, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex08", """def pred T() { all z:obj {and(x, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex08a", """def pred T() { all z:obj {and(y, ex x:obj {true})} }""", 0)>]
    [<DataRow("ex08b", """def pred T() { all x:obj {and(y, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex09", """def pred T() { ex z:obj {and(x, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex09a", """def pred T() { ex z:obj {and(y, ex x:obj {true})} }""", 0)>]
    [<DataRow("ex09b", """def pred T() { ex x:obj {and(y, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex10", """def pred T() { exn$1 z:obj {and(x, ex x:obj {true})} }""", 1)>]
    [<DataRow("ex10a", """def pred T() { exn$1 z:obj {and(y, ex x:obj {true})} }""", 0)>]
    [<DataRow("ex10b", """def pred T() { exn$1 x:obj {and(y, ex x:obj {true})} }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR10(no: string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR10 ("", "")
            runTestHelper "TestVAR10.fpl" fplCode code expected
