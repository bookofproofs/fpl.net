namespace Diagnostics.General

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


[<TestClass>]
type TestGEN00() =

    [<TestMethod>]
    member this.TestGEN00() =
        let code = GEN00 ""
        runTestHelperWithoutSyntaxChecking "TestGEN00.fpl" """~testGEN00~""" code 1
