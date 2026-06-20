namespace Diagnostics.VariableRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestVAR09() =

    [<DataRow("00", """def pred T() { not true }""", 0)>]
    [<DataRow("01", """def pred T() { dec x:pred; not x }""", 1)>] 
    [<DataRow("01a", """def pred T(x:pred) { not x }""", 0)>] 
    [<DataRow("01b", """def func T()->obj { dec x:pred assert not x; return x }""", 1)>] 
    [<DataRow("01c", """def func T()->pred(x:obj) { dec assert not x; return x }""", 0)>] 
    [<DataRow("01d", """def cl T {dec x:pred assert not x; ctor T(){}}""", 1)>] 
    [<DataRow("01e", """def cl T {ctor T(){dec x:pred assert not x; }}""", 1)>] 
    [<DataRow("01f", """def cl T {ctor T(x:pred){dec assert not x;}}""", 0)>] 
    [<DataRow("01g", """def cl T {intr prty pred T() {dec x:pred; not x}}""", 1)>] 
    [<DataRow("01h", """def cl T {intr prty pred T(x:pred) {not x}}""", 0)>] 
    [<DataRow("01i", """def cl T {intr prty func T()->obj {dec x:pred assert not x; return x}}""", 1)>] 
    [<DataRow("01j", """def cl T {intr prty func T(x:pred)->obj {dec assert not x; return x}}""", 0)>] 
    [<DataRow("01k", """def cl T {intr prty func T()->pred(x:pred) {dec assert not x; return x }}""", 0)>] 
    [<DataRow("02", """def pred T() { dec x:ind; not x }""", 1)>]
    [<DataRow("03", """def pred T() { dec x:pred; not (x) }""", 1)>]
    [<DataRow("04", """def pred T() { dec x:pred; not ((x)) }""", 1)>]
    [<DataRow("05", """def pred T() { dec x:pred; not (((x))) }""", 1)>]
    [<DataRow("06", """def pred T() { all x:obj {true} }""", 0)>]
    [<DataRow("07", """def pred T() { dec x:pred; and(x,true) }""", 1)>]
    [<DataRow("08", """def pred T() { dec x:pred; all y:obj {and(x,true)} }""", 1)>]
    [<DataRow("09", """def pred T() { dec x:pred; or(x,false) }""", 1)>]
    [<DataRow("10", """def pred T() { dec x,y:pred; or(x,y) }""", 2)>]
    [<DataRow("11a", """def pred T() { all y:obj {and(x,y)} }""", 1)>] // x is undefined, but still a variable
    [<DataRow("11a_", """def pred T() { dec x:obj; all y:obj {and(x,y)} }""", 1)>] // VAR09, since x is free 
    [<DataRow("11b", """def pred T() { ex y:obj {and(x,y)} }""", 1)>] // x is undefined, but still a variable 
    [<DataRow("11b_", """def pred T() { dec x:obj; ex y:obj {and(x,y)} }""", 1)>]  // VAR09, since x is free 
    [<DataRow("11c", """def pred T() { exn$1 y:obj {and(x,y)} }""", 1)>] // x is undefined, but still a variable 
    [<DataRow("11c_", """def pred T() { dec x:obj; exn$1 y:obj {and(x,y)} }""", 1)>]  // VAR09, since x is free 
    [<DataRow("11d", """def pred T() { all x,y:obj {and(x,y)} }""", 0)>]
    [<DataRow("11e", """def pred T() { ex x,y:obj {and(x,y)} }""", 0)>]
    [<DataRow("11f", """def pred T() { exn$1 y:obj {and(true,y)} }""", 0)>]
    [<DataRow("12", """inf ModusTollens {dec p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)}""", 0)>]
    [<DataRow("13", """inf ModusTollens {dec p,q: pred; pre: and (not q, impl(p,q) ) con: not p}""", 0)>]
    [<DataRow("14", """def pred T() { dec x:pred; impl(true,x) }""", 1)>]
    [<DataRow("15", """def pred T() { dec x,y:pred; impl(x,y) }""", 2)>]
    [<DataRow("16", """def pred T() { impl(true,true) }""", 0)>]
    [<DataRow("17", """def pred T() { dec x:pred; iif(true,x) }""", 1)>]
    [<DataRow("18", """def pred T() { dec x,y:pred; iif(x,y) }""", 2)>]
    [<DataRow("19", """def pred T() { iif(true,true) }""", 0)>]
    [<DataRow("20", """def pred T() { xor(xor(true,true),true) }""", 0)>]
    [<DataRow("21", """def pred T() { all x,y:pred { xor(xor(y,x),true) } }""", 0)>]
    [<DataRow("22", """def pred T() { all i:Nat {true} }""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR09(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR09 ("","")
            runTestHelper "TestVAR09.fpl" fplCode code expected
