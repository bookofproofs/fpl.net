namespace Diagnostics.General

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl0Base.Errors.Diagnostics
open Fpl2Interpreter.Helpers.Debug
open TestFpl2Interpreter.Helpers.Common


[<TestClass>]
type TestGEN00() =

    [<TestMethod>]
    member this.TestGEN00() =
        let code = GEN00 ""
        runTestHelperWithoutSyntaxChecking "TestGEN00.fpl" """~testGEN00~""" code 1
