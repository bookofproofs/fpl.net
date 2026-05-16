namespace FplInterpreter.Tests.Diagnostics.ErrRecovery

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open FplInterpreter.Main
open CommonTestHelpers
open TestSharedConfig

[<TestClass>]
type TestInterpreterErrors() =

 
    [<DataRow("00", "def pred T() { (1 = x) } ;", 0)>] // parser does infix operator, no operand missing
    [<DataRow("01", "def pred T() { (1 = ) } ;", 1)>] // parser does infix operator, missing second operand
    [<DataRow("02", "def pred T() { (1 =) } ;", 1)>] // parser does infix operator, missing second operand
    [<DataRow("03", "def pred T() { (1+) } ;", 0)>] // parser does postfix operator, no infix operation check
    [<DataRow("03", "def pred T() { (1=) } ;", 1)>] // parser does infix operator, missing second operand
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY000(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY000 ""
            runTestHelper "TestSY000.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!0 x:obj{true} } ;", 1)>] 
    [<DataRow("01", "def pred T() { ∃!1 x:obj{true} } ;", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY001(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY001 
            runTestHelper "TestSY001.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!1 x:obj{true} } ;", 1)>] 
    [<DataRow("01", "def pred T() { ∃!2 x:obj{true} } ;", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002
            runTestHelper "TestSY002.fpl" fplCode code expected

    [<DataRow("all00", "ax T { ∀ x:obj {true} } ;", 0)>]
    [<DataRow("all01", "ax T { ∀ x:obj true} } ;", 1)>]
    [<DataRow("ex00", "ax T { ∃ x:obj {true} } ;", 0)>]
    [<DataRow("ex01", "ax T { ∃ x:obj true} } ;", 1)>]
    [<DataRow("exn01", "ax T { ∃! x:obj true} } ;", 1)>]
    [<DataRow("ax00", "ax T { true } ;", 0)>]
    [<DataRow("ax01", "ax T  true } ;", 1)>]
    [<DataRow("thm00", "thm T { true } ;", 0)>]
    [<DataRow("thm01", "thm T  true } ;", 1)>]
    [<DataRow("lem00", "lem T { true } ;", 0)>]
    [<DataRow("lem01", "lem T  true } ;", 1)>]
    [<DataRow("prop00", "prop T { true } ;", 0)>]
    [<DataRow("prop01", "prop T  true } ;", 1)>]
    [<DataRow("conj00", "conj T { true } ;", 0)>]
    [<DataRow("conj01", "conj T  true } ;", 1)>]
    [<DataRow("cor00", "cor T$1 { true } ;", 0)>]
    [<DataRow("cor01", "cor T$1  true } ;", 1)>]
    [<DataRow("inf00", "inf T { pre: true con: true} ;", 0)>]
    [<DataRow("inf01", "inf T  pre: true con: true} ;", 1)>]
    [<DataRow("inf00", "inf T { pre: true con: true} ;", 0)>]
    [<DataRow("ext00", "ext Digits x@/\d+/ -> X {ret x};", 0)>]
    [<DataRow("ext01", "ext Digits x@/\d+/ -> X ret x};", 1)>]
    [<DataRow("for00", "def pred T() {dec for x in y {assert z};true};", 0)>]
    [<DataRow("for01", "def pred T() {dec for x in y assert z};true};", 1)>]
    [<DataRow("ctor00", "def cl T {ctor T() {}};", 0)>]
    [<DataRow("ctor01", "def cl T {ctor T() }};", 1)>]
    [<DataRow("prf00", "prf T$1 {1. |- trivial };", 0)>]
    [<DataRow("prf01", "prf T$1 1. |- trivial };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY003(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY003
            runTestHelper "TestSY003.fpl" fplCode code expected

    [<DataRow("all00", "ax T { ∀ x:obj {true} } ;", 0)>]
    [<DataRow("all01", "ax T { ∀ x:obj {true } ;", 1)>]
    [<DataRow("ex00", "ax T { ∃ x:obj {true } };", 0)>]
    [<DataRow("ex01", "ax T { ∃ x:obj {true  };", 1)>]
    [<DataRow("exn01", "ax T { ∃! x:obj {true  };", 1)>]
    [<DataRow("ax00", "ax T { true } ;", 0)>]
    [<DataRow("ax01", "ax T { true  ;", 1)>]
    [<DataRow("thm00", "thm T { true } ;", 0)>]
    [<DataRow("thm01", "thm T { true  ;", 1)>]
    [<DataRow("lem00", "lem T { true } ;", 0)>]
    [<DataRow("lem01", "lem T { true  ;", 1)>]
    [<DataRow("prop00", "prop T { true } ;", 0)>]
    [<DataRow("prop01", "prop T { true  ;", 1)>]
    [<DataRow("conj00", "conj T { true } ;", 0)>]
    [<DataRow("conj01", "conj T { true  ;", 1)>]
    [<DataRow("cor00", "cor T$1 { true } ;", 0)>]
    [<DataRow("cor01", "cor T$1 { true  ;", 1)>]
    [<DataRow("inf00", "inf T { pre: true con: true} ;", 0)>]
    [<DataRow("inf01", "inf T {pre: true con: true ;", 1)>]
    [<DataRow("ext00", "ext Digits x@/\d+/ -> X {ret x};", 0)>]
    [<DataRow("ext01", "ext Digits x@/\d+/ -> X {ret x;", 1)>]
    [<DataRow("for00", "def pred T() {dec for x in y {assert z};true};", 0)>]
    [<DataRow("for01", "def pred T() {dec for x in y {assert z;true};", 1)>]
    [<DataRow("ctor00", "def cl T {ctor T() {}};", 0)>]
    [<DataRow("ctor01", "def cl T {ctor T() {};", 1)>]
    [<DataRow("cl00", "def cl T {intr};", 0)>]
    [<DataRow("cl01", "def cl T {intr;", 1)>]
    [<DataRow("pred00", "def pred T() {intr};", 0)>]
    [<DataRow("pred01", "def pred T() {intr;", 1)>]
    [<DataRow("func00", "def func T()->obj {intr};", 0)>]
    [<DataRow("func01", "def func T()->obj {intr;", 1)>]
    [<DataRow("prf00", "prf T$1 {1. |- trivial };", 0)>]
    [<DataRow("prf01", "prf T$1 {1. |- trivial ;", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY004(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY004
            runTestHelper "TestSY004.fpl" fplCode code expected


    [<DataRow("uses00", """uses Fpl.Test;""", 0)>]
    [<DataRow("uses01", """uses Fpl.Test.;""", 1)>]
    [<DataRow("uses02", """uses Fpl.Test alias A;""", 0)>]
    [<DataRow("uses03", """uses Fpl.Test alias ;""", 1)>]
    [<DataRow("cl00", """def cl T ;""", 0)>]
    [<DataRow("cl01", """def cl ;""", 1)>]
    [<DataRow("cl02", """def cl T {intr};""", 0)>]
    [<DataRow("cl03", """def cl {intr};""", 1)>]
    [<DataRow("cl04", """def cl T:S ;""", 0)>]
    [<DataRow("cl05", """def cl T:;""", 1)>]
    [<DataRow("cl06", """def cl T: ;""", 1)>]
    [<DataRow("cl07", """def cl T:,,;""", 3)>]
    [<DataRow("thm00", """thm T {true};""", 0)>]
    [<DataRow("thm01", """thm {true};""", 1)>]
    [<DataRow("lem00", """lem T{true};""", 0)>]
    [<DataRow("lem01", """lem {true};""", 1)>]
    [<DataRow("prop00", """prop T{true};""", 0)>]
    [<DataRow("prop01", """prop {true};""", 1)>]
    [<DataRow("conj00", """conj T{true};""", 0)>]
    [<DataRow("conj01", """conj {true};""", 1)>]
    [<DataRow("ax00", """ax T{true};""", 0)>]
    [<DataRow("ax01", """ax {true};""", 1)>]
    [<DataRow("pred00", """def pred T();""", 0)>]
    [<DataRow("pred01", """def pred ();""", 1)>]
    [<DataRow("pred02", """def pred T:S();""", 0)>]
    [<DataRow("pred03", """def pred T:();""", 1)>]
    [<DataRow("pred03", """def pred T:,, ();""", 3)>]
    [<DataRow("func00", """def func T()->obj ;""", 0)>]
    [<DataRow("func01", """def func ()->obj ;""", 1)>]
    [<DataRow("func02", """def func T:S()->obj ;""", 0)>]
    [<DataRow("func03", """def func T:()->obj ;""", 1)>]
    [<DataRow("func03", """def func T: , , ()->obj ;""", 3)>]
    [<DataRow("inf00", """inf T{pre:true con:true};""", 0)>]
    [<DataRow("inf01", """inf {pre:true con:true};""", 1)>]
    [<DataRow("ext01", """ext T x@/\d+/ -> X {ret x};""", 0)>]
    [<DataRow("ext02", """ext  x@/\d+/ -> X {ret x};""", 1)>]
    [<DataRow("del01", """def pred T() {del.T()};""", 0)>]
    [<DataRow("del02", """def pred T() {del. ()};""", 1)>]
    [<DataRow("del03", """def pred T() {del.()};""", 1)>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T();}};""", 0)>]
    [<DataRow("base02", """def cl S def cl T {ctor T() {dec base. ();}};""", 1)>]
    [<DataRow("base03", """def cl S def cl T {ctor T() {dec base.();}};""", 1)>]
    [<DataRow("cor00", """cor T$1 {true};""", 0)>]
    [<DataRow("cor01", """cor $1 {true};""", 1)>]
    [<DataRow("prf00", """prf T$1 {1. |- trivial};""", 0)>]
    [<DataRow("prf01", """prf $1 {1. |- trivial};""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY005(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY005
            runTestHelper "TestSY005.fpl" fplCode code expected

    [<DataRow("pred00", """def pred T();""", 0)>]
    [<DataRow("pred01", """def pred T);""", 1)>]
    [<DataRow("func00", """def func T()->obj ;""", 0)>]
    [<DataRow("func01", """def func T)->obj ;""", 1)>]
    [<DataRow("ctor00", """def cl S def cl T {ctor T() {dec base.S(); }};""", 0)>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T) {dec base.S(); }};""", 1)>]
    [<DataRow("propPred00", """def cl S def cl T {intr prty pred T() };""", 0)>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T) };""", 1)>]
    [<DataRow("propFunc00", """def cl S def cl T {intr prty func T()->obj };""", 0)>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T)->obj };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY006(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY006
            runTestHelper "TestSY007.fpl" fplCode code expected


    [<DataRow("pred00", """def pred T();""", 0)>]
    [<DataRow("pred01", """def pred T(;""", 1)>]
    [<DataRow("func00", """def func T()->obj ;""", 0)>]
    [<DataRow("func01", """def func T(->obj ;""", 1)>]
    [<DataRow("ctor00", """def cl S def cl T {ctor T() {dec base.S(); }};""", 0)>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T( {dec base.S(); }};""", 1)>]
    [<DataRow("propPred00", """def cl S def cl T {intr prty pred T() };""", 0)>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T( };""", 1)>]
    [<DataRow("propFunc00", """def cl S def cl T {intr prty func T()->obj };""", 0)>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T(->obj };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY007(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY007
            runTestHelper "TestSY007.fpl" fplCode code expected

    [<DataRow("del00", """def pred T() {del.T()};""", 0)>]
    [<DataRow("del01", """def pred T() {del T()};""", 1)>]
    [<DataRow("base00", """def cl S def cl T {ctor T() {dec base.T(); }};""", 0)>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base T(); }};""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY008(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY008
            runTestHelper "TestSY008.fpl" fplCode code expected
