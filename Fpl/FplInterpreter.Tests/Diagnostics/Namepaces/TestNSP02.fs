namespace Diagnostics.Namespaces

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestNSP02() =

    [<TestMethod>]
    member this.Test(fplCode:string, expected:int) = ()
