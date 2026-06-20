namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID012() =

    // class properties
    [<DataRow("C1", "def cl A {intr prty pred L() } def pred T() {dec x:A; x.L()}", 0)>]
    [<DataRow("C1a", "def cl A {intr prty pred L() } def pred T() {dec x:A; x.LTypo()}", 1)>]
    [<DataRow("C2", "def cl A {intr prty pred L() } def pred T(x:A) {x.L()}", 0)>]
    [<DataRow("C2a", "def cl A {intr prty pred L() } def pred T(x:A) {x.LTypo()}", 1)>]
    [<DataRow("C3", "def cl A {intr prty func L()->ind } def pred T() {dec x:A; x.L()}", 0)>]
    [<DataRow("C3a", "def cl A {intr prty func L()->ind } def pred T() {dec x:A; x.LTypo()}", 1)>]
    [<DataRow("C3", "def cl A {intr prty func L()->ind } def pred T(x:A) {x.L()}", 0)>]
    [<DataRow("C3a", "def cl A {intr prty func L()->ind } def pred T(x:A) {x.LTypo()}", 1)>]

    // inherited class properties
    [<DataRow("IC1", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec x:B; x.L()}", 0)>]
    [<DataRow("IC1a", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec x:B; x.LTypo()}", 1)>]
    [<DataRow("IC2", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {x.L()}", 0)>]
    [<DataRow("IC2a", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {x.LTypo()}", 1)>]
    [<DataRow("IC3", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec x:B; x.L()}", 0)>]
    [<DataRow("IC3a", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec x:B; x.LTypo()}", 1)>]
    [<DataRow("IC4", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {x.L()}", 0)>]
    [<DataRow("IC4a", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {x.LTypo()}", 1)>]

    // class instance properties
    [<DataRow("I1", "def cl A {intr prty pred L() } def pred T() {dec x:A x:=A(); x.L()}", 0)>]
    [<DataRow("I1a", "def cl A {intr prty pred L() } def pred T() {dec x:A x:=A(); x.LTypo()}", 1)>]
    [<DataRow("I2", "def cl A {intr prty pred L() } def pred T(x:A) {dec x:=A(); x.L()}", 0)>]
    [<DataRow("I2a", "def cl A {intr prty pred L() } def pred T(x:A) {dec x:=A(); x.LTypo()}", 1)>]
    [<DataRow("I3", "def cl A {intr prty func L()->ind } def pred T() {dec x:A x:=A(); x.L()}", 0)>]
    [<DataRow("I3a", "def cl A {intr prty func L()->ind } def pred T() {dec x:A x:=A(); x.LTypo()}", 1)>]
    [<DataRow("I4", "def cl A {intr prty func L()->ind } def pred T(x:A) {dec x:=A(); x.L()}", 0)>]
    [<DataRow("I4a", "def cl A {intr prty func L()->ind } def pred T(x:A) {dec x:=A(); x.LTypo()}", 1)>]

    // inherited class instance properties
    [<DataRow("II1", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec x:B x:=B(); x.L()}", 0)>]
    [<DataRow("II1a", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec x:A x:=B(); x.LTypo()}", 1)>]
    [<DataRow("II2", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {dec x:=B(); x.L()}", 0)>]
    [<DataRow("II2a", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {dec x:=B(); x.LTypo()}", 1)>]
    [<DataRow("II3", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec x:B x:=B(); x.L()}", 0)>]
    [<DataRow("II3a", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec x:A x:=B(); x.LTypo()}", 1)>]
    [<DataRow("II4", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {dec x:=B(); x.L()}", 0)>]
    [<DataRow("II4a", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {dec x:=B(); x.LTypo()}", 1)>]

    // predicate properties
    [<DataRow("P1", "def pred A() {intr prty pred L() } def pred T() {dec x:A; x.L()}", 0)>]
    [<DataRow("P1a", "def pred A() {intr prty pred L() } def pred T() {dec x:A; x.LTypo()}", 1)>]
    [<DataRow("P2", "def pred A() {intr prty pred L() } def pred T(x:A) {x.L()}", 0)>]
    [<DataRow("P2a", "def pred A() {intr prty pred L() } def pred T(x:A) {x.LTypo()}", 1)>]
    [<DataRow("P3", "def pred A() {intr prty func L()->ind } def pred T() {dec x:A; x.L()}", 0)>]
    [<DataRow("P3a", "def pred A() {intr prty func L()->ind } def pred T() {dec x:A; x.LTypo()}", 1)>]
    [<DataRow("P4", "def pred A() {intr prty func L()->ind } def pred T(x:A) {x.L()}", 0)>]
    [<DataRow("P4a", "def pred A() {intr prty func L()->ind } def pred T(x:A) {x.LTypo()}", 1)>]

    // inherited predicate properties
    [<DataRow("IP1", "def pred A() {intr prty pred L() } def pred B:A() def pred T() {dec x:B; x.L()}", 0)>]
    [<DataRow("IP1a", "def pred A() {intr prty pred L() } def pred B:A() def pred T() {dec x:B; x.LTypo()}", 1)>]
    [<DataRow("IP2", "def pred A() {intr prty pred L() } def pred B:A() def pred T(x:B) {x.L()}", 0)>]
    [<DataRow("IP2a", "def pred A() {intr prty pred L() } def pred B:A() def pred T(x:B) {x.LTypo()}", 1)>]
    [<DataRow("IP3", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T() {dec x:B; x.L()}", 0)>]
    [<DataRow("IP3a", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T() {dec x:B; x.LTypo()}", 1)>]
    [<DataRow("IP4", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T(x:B) {x.L()}", 0)>]
    [<DataRow("IP4a", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T(x:B) {x.LTypo()}", 1)>]

    // functional term properties
    [<DataRow("F1", "def func A()->ind {intr prty pred L() } def pred T() {dec x:A; x.L()}", 0)>]
    [<DataRow("F1a", "def func A()->ind {intr prty pred L() } def pred T() {dec x:A; x.LTypo()}", 1)>]
    [<DataRow("F2", "def func A()->ind {intr prty pred L() } def pred T(x:A) {x.L()}", 0)>]
    [<DataRow("F2a", "def func A()->ind {intr prty pred L() } def pred T(x:A) {x.LTypo()}", 1)>]
    [<DataRow("F3", "def func A()->ind {intr prty func L()->ind } def pred T() {dec x:A; x.L()}", 0)>]
    [<DataRow("F3a", "def func A()->ind {intr prty func L()->ind } def pred T() {dec x:A; x.LTypo()}", 1)>]
    [<DataRow("F4", "def func A()->ind {intr prty func L()->ind } def pred T(x:A) {x.L()}", 0)>]
    [<DataRow("F4a", "def func A()->ind {intr prty func L()->ind } def pred T(x:A) {x.LTypo()}", 1)>]

    // inherited functional term properties
    [<DataRow("IF1", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T() {dec x:B; x.L()}", 0)>]
    [<DataRow("IF1a", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T() {dec x:B; x.LTypo()}", 1)>]
    [<DataRow("IF2", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T(x:B) {x.L()}", 0)>]
    [<DataRow("IF2a", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T(x:B) {x.LTypo()}", 1)>]
    [<DataRow("IF3", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T() {dec x:B; x.L()}", 0)>]
    [<DataRow("IF3a", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T() {dec x:B; x.LTypo()}", 1)>]
    [<DataRow("IF4", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T(x:B) {x.L()}", 0)>]
    [<DataRow("IF4a", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T(x:B) {x.LTypo()}", 1)>]

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID012Properties(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID012 ("", "", "", "")
            runTestHelper "TestID012Properties.fpl" fplCode code expected

    // class variables
    [<DataRow("C1", "def cl A {dec a:obj; ctor A() {} } def pred T() {dec x:A; x.a}", 0)>]
    [<DataRow("C1a", "def cl A {dec a:obj; ctor A() {} } def pred T() {dec x:A; x.aTypo}", 1)>]
    [<DataRow("C2", "def cl A {dec a:obj; ctor A() {} } def pred T(x:A) {x.a}", 0)>]
    [<DataRow("C2a", "def cl A {dec a:obj; ctor A() {} } def pred T(x:A) {x.aTypo}", 1)>]

    // inherited class variables
    [<DataRow("IC1", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T() {dec x:B; x.a}", 0)>]
    [<DataRow("IC1a", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T() {dec x:B; x.aTypo}", 1)>]
    [<DataRow("IC2", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T(x:B) {x.a}", 0)>]
    [<DataRow("IC2a", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T(x:B) {x.aTypo}", 1)>]

    // class instance variables
    [<DataRow("I1", "def cl A {dec a:obj; ctor A() {} } def pred T() {dec x:A x:=A(); x.a}", 0)>]
    [<DataRow("I1a", "def cl A {dec a:obj; ctor A() {} } def pred T() {dec x:A x:=A(); x.aTypo}", 1)>]
    [<DataRow("I2", "def cl A {dec a:obj; ctor A() {} } def pred T(x:A) {dec x:=A(); x.a}", 0)>]
    [<DataRow("I2a", "def cl A {dec a:obj; ctor A() {} } def pred T(x:A) {dec x:=A(); x.aTypo}", 1)>]

    // inherited class instance variables
    [<DataRow("II1", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T() {dec x:B x:=B(); x.a}", 0)>]
    [<DataRow("II1a", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T() {dec x:A x:=B(); x.aTypo}", 1)>]
    [<DataRow("II2", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T(x:B) {dec x:=B(); x.a}", 0)>]
    [<DataRow("II2a", "def cl A {dec a:obj; ctor A() {} } def cl B:A def pred T(x:B) {dec x:=B(); x.aTypo}", 1)>]

    // predicate variables
    [<DataRow("P1", "def pred A() {dec a:obj; true} def pred T() {dec x:A; x.a}", 0)>]
    [<DataRow("P1a", "def pred A() {dec a:obj; true} def pred T() {dec x:A; x.aTypo}", 1)>]
    [<DataRow("P2", "def pred A() {dec a:obj; true} def pred T(x:A) {x.a}", 0)>]
    [<DataRow("P2a", "def pred A() {dec a:obj; true} def pred T(x:A) {x.aTypo}", 1)>]

    // inherited predicate variables
    [<DataRow("IP1", "def pred A() {dec a:obj; true} def pred B:A() def pred T() {dec x:B; x.a}", 0)>]
    [<DataRow("IP1a", "def pred A() {dec a:obj; true} def pred B:A() def pred T() {dec x:B; x.aTypo}", 1)>]
    [<DataRow("IP2", "def pred A() {dec a:obj; true} def pred B:A() def pred T(x:B) {x.a}", 0)>]
    [<DataRow("IP2a", "def pred A() {dec a:obj; true} def pred B:A() def pred T(x:B) {x.aTypo}", 1)>]

    // functional term variables
    [<DataRow("F1", "def func A()->ind {dec a:obj; return $1} def pred T() {dec x:A; x.a}", 0)>]
    [<DataRow("F1a", "def func A()->ind {dec a:obj; return $1} def pred T() {dec x:A; x.aTypo}", 1)>]
    [<DataRow("F2", "def func A()->ind {dec a:obj; return $1} def pred T(x:A) {x.a}", 0)>]
    [<DataRow("F2a", "def func A()->ind {dec a:obj; return $1} def pred T(x:A) {x.aTypo}", 1)>]

    // inherited functional term variables
    [<DataRow("IF1", "def func A()->ind {dec a:obj; return $1} def func B:A()->ind def pred T() {dec x:B; x.a}", 0)>]
    [<DataRow("IF1a", "def func A()->ind {dec a:obj; return $1} def func B:A()->ind def pred T() {dec x:B; x.aTypo}", 1)>]
    [<DataRow("IF2", "def func A()->ind {dec a:obj; return $1} def func B:A()->ind def pred T(x:B) {x.a}", 0)>]
    [<DataRow("IF2a", "def func A()->ind {dec a:obj; return $1} def func B:A()->ind def pred T(x:B) {x.aTypo}", 1)>]

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID012Variables(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID012 ("", "", "", "")
            runTestHelper "TestID012Variables.fpl" fplCode code expected
