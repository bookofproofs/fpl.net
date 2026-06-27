namespace Diagnostics.SignatureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG00
   Purpose: Report illegal arity when using fixity notation (prefix/infix/postfix) in a signature.
   What it indicates: A declaration used a fixity form with the wrong arity (for example, `infix` must have arity 2, `prefix`/`postfix` arity 1). The emitted message is: `Illegal arity `{arity}` using `{fixType}` notation.`
   Use: Emitted during signature checks to locate declarations whose declared arity does not match the expected arity for the chosen fixity.
   Action / Treat: Fix the declaration by supplying the correct arity for the chosen fixity (or change the fixity to match the intended arity). Treat SIG00 as an error that must be corrected for correct parsing/semantics. *)

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
