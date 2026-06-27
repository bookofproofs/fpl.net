namespace Diagnostics.SignatureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG04
   Purpose: Report that no overload or signature matches a requested usage (call/application/reference).
   What it indicates: The argument(s) or value provided do not match any declared signature for the target (function, predicate, property, constructor, mapping, etc.). The diagnostic includes the attempted signature and the candidate signatures that were tried.
   Use: Help authors locate mismatches between a use-site and available declarations (wrong argument types/arity, calling a non-callable node, or missing overload).
   Action / Treat: Correct the call site (adjust argument types/arity), declare a matching overload/signature, or change the target to one with a compatible signature; treat SIG04 as a signature-resolution error that must be resolved for successful type matching and invocation. *)

[<TestClass>]
type TestSIG04() =

    // match with simple types
    [<DataRow("ST0", "def pred Test(v:obj) def pred T() {dec x:obj; Test(x)}", 0)>]
    [<DataRow("ST1", "def pred Test(v:ind) def pred T() {dec x:ind; Test(x)}", 0)>]
    [<DataRow("ST1a", "def pred Test(v:ind) def pred T() {Test($1)}", 0)>]
    [<DataRow("ST2", "def pred Test(v:func) def pred T() {dec x:func; Test(x)}", 0)>]
    [<DataRow("ST2a", "def pred Test(v:func) def pred T() {dec x:func()->ind; Test(x)}", 0)>]
    [<DataRow("ST2b", "def pred Test(v:func) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 0)>]
    [<DataRow("ST2c", "def pred Test(v:func) def pred T() {dec x:func(y:obj)->func; Test(x)}", 0)>]
    [<DataRow("ST2d", "def pred Test(v:func) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 0)>]
    [<DataRow("ST3", "def pred Test(v:pred) def pred T() {dec x:pred; Test(x)}", 0)>]
    [<DataRow("ST3a", "def pred Test(v:pred) def pred T() {dec x:pred(); Test(x)}", 0)>]
    [<DataRow("ST3b", "def pred Test(v:pred) def pred T() {dec x:pred; Test(x)}", 0)>]
    [<DataRow("ST3c", "def pred Test(v:pred) def pred T() {dec x:pred(y:obj); Test(x)}", 0)>]
    [<DataRow("ST3d", "def pred Test(v:pred) def pred T() {Test(true)}", 0)>]
    [<DataRow("ST3e", "def pred Test(v:pred) def pred T() {Test(false)}", 0)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj", "def pred Test(v:obj) def pred T() {dec x:obj; Test(x)}", 0)>]
    [<DataRow("ST1_obj", "def pred Test(v:obj) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("ST2_obj", "def pred Test(v:obj) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("ST2a_obj", "def pred Test(v:obj) def pred T() {dec x:func()->ind; Test(x)}", 1)>]
    [<DataRow("ST2b_obj", "def pred Test(v:obj) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 1)>]
    [<DataRow("ST2c_obj", "def pred Test(v:obj) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("ST2d_obj", "def pred Test(v:obj) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("ST3_obj", "def pred Test(v:obj) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("ST3a_obj", "def pred Test(v:obj) def pred T() {dec x:pred(); Test(x)}", 1)>]
    [<DataRow("ST3b_obj", "def pred Test(v:obj) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("ST3c_obj", "def pred Test(v:obj) def pred T() {dec x:pred(y:obj); Test(x)}", 1)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def pred Test(v:ind) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("ST1_ind", "def pred Test(v:ind) def pred T() {dec x:ind; Test(x)}", 0)>]
    [<DataRow("ST2_ind", "def pred Test(v:ind) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("ST2a_ind", "def pred Test(v:ind) def pred T() {dec x:func()->ind; Test(x)}", 1)>]
    [<DataRow("ST2b_ind", "def pred Test(v:ind) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 1)>]
    [<DataRow("ST2c_ind", "def pred Test(v:ind) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("ST2d_ind", "def pred Test(v:ind) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("ST3_ind", "def pred Test(v:ind) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("ST3a_ind", "def pred Test(v:ind) def pred T() {dec x:pred(); Test(x)}", 1)>]
    [<DataRow("ST3b_ind", "def pred Test(v:ind) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("ST3c_ind", "def pred Test(v:ind) def pred T() {dec x:pred(y:obj); Test(x)}", 1)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def pred Test(v:pred) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("ST1_pred", "def pred Test(v:pred) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("ST2_pred", "def pred Test(v:pred) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("ST2a_pred", "def pred Test(v:pred) def pred T() {dec x:func()->ind; Test(x)}", 1)>]
    [<DataRow("ST2b_pred", "def pred Test(v:pred) def pred T() {dec x:func(y:obj)->pred; Test(x)}", 1)>]
    [<DataRow("ST2c_pred", "def pred Test(v:pred) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("ST2d_pred", "def pred Test(v:pred) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("ST3_pred", "def pred Test(v:pred) def pred T() {dec x:pred; Test(x)}", 0)>]
    [<DataRow("ST3a_pred", "def pred Test(v:pred) def pred T() {dec x:pred(); Test(x)}", 0)>]
    [<DataRow("ST3b_pred", "def pred Test(v:pred) def pred T() {dec x:pred; Test(x)}", 0)>]
    [<DataRow("ST3c_pred", "def pred Test(v:pred) def pred T() {dec x:pred(y:obj); Test(x)}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def pred Test(v:func) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("ST1_func", "def pred Test(v:func) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("ST2_func", "def pred Test(v:func) def pred T() {dec x:func; Test(x)}", 0)>]
    [<DataRow("ST2a_func", "def pred Test(v:func) def pred T() {dec x:func()->ind; Test(x)}", 0)>]
    [<DataRow("ST2b_func", "def pred Test(v:func) def pred T() {dec x:func(y:obj)->pred; Test(x)}", 0)>]
    [<DataRow("ST2c_func", "def pred Test(v:func) def pred T() {dec x:func(y:obj)->func; Test(x)}", 0)>]
    [<DataRow("ST2d_func", "def pred Test(v:func) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 0)>]
    [<DataRow("ST3_func", "def pred Test(v:func) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("ST3a_func", "def pred Test(v:func) def pred T() {dec x:pred(); Test(x)}", 1)>]
    [<DataRow("ST3b_func", "def pred Test(v:func) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("ST3c_func", "def pred Test(v:func) def pred T() {dec x:pred(y:obj); Test(x)}", 1)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def pred Test(v:pred()) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("NP1", "def pred Test(v:pred()) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("NP2", "def pred Test(v:pred()) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("NP2a", "def pred Test(v:pred()) def pred T() {dec x:func()->ind; Test(x)}", 1)>]
    [<DataRow("NP2b", "def pred Test(v:pred()) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 1)>]
    [<DataRow("NP2c", "def pred Test(v:pred()) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("NP2d", "def pred Test(v:pred()) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("NP3", "def pred Test(v:pred()) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NP3a", "def pred Test(v:pred()) def pred T() {dec x:pred(); Test(x)}", 0)>]
    [<DataRow("NP3b", "def pred Test(v:pred()) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NP3c", "def pred Test(v:pred()) def pred T() {dec x:pred(y:obj); Test(x)}", 1)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def pred Test(v:pred(a:obj)) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("NP_1", "def pred Test(v:pred(a:obj)) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("NP_2", "def pred Test(v:pred(a:obj)) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("NP_2a", "def pred Test(v:pred(a:obj)) def pred T() {dec x:func()->ind; Test(x)}", 1)>]
    [<DataRow("NP_2b", "def pred Test(v:pred(a:obj)) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 1)>]
    [<DataRow("NP_2c", "def pred Test(v:pred(a:obj)) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("NP_2d", "def pred Test(v:pred(a:obj)) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("NP_3", "def pred Test(v:pred(a:obj)) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NP_3a", "def pred Test(v:pred(a:obj)) def pred T() {dec x:pred(); Test(x)}", 1)>]
    [<DataRow("NP_3b", "def pred Test(v:pred(a:obj)) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NP_3c", "def pred Test(v:pred(a:obj)) def pred T() {dec x:pred(y:obj); Test(x)}", 0)>]
    [<DataRow("NP_3c", "def pred Test(v:pred(a:obj)) def pred T() {dec x:pred(y:ind); Test(x)}", 1)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def pred Test(v:func()->ind) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("NF1", "def pred Test(v:func()->ind) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("NF2", "def pred Test(v:func()->ind) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("NF2a", "def pred Test(v:func()->ind) def pred T() {dec x:func()->ind; Test(x)}", 0)>]
    [<DataRow("NF2b", "def pred Test(v:func()->ind) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 1)>]
    [<DataRow("NF2c", "def pred Test(v:func()->ind) def pred T() {dec x:func(y:obj)->obj; Test(x)}", 1)>]
    [<DataRow("NF2d", "def pred Test(v:func()->ind) def pred T() {dec x:func(y:ind)->ind; Test(x)}", 1)>]
    [<DataRow("NF2e", "def pred Test(v:func()->ind) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("NF2f", "def pred Test(v:func()->ind) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("NF3", "def pred Test(v:func()->ind) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NF3a", "def pred Test(v:func()->ind) def pred T() {dec x:pred(); Test(x)}", 1)>]
    [<DataRow("NF3b", "def pred Test(v:func()->ind) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NF3c", "def pred Test(v:func()->ind) def pred T() {dec x:pred(y:obj); Test(x)}", 1)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:obj; Test(x)}", 1)>]
    [<DataRow("NF_1", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:ind; Test(x)}", 1)>]
    [<DataRow("NF_2", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func; Test(x)}", 1)>]
    [<DataRow("NF_2a", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func()->ind; Test(x)}", 1)>]
    [<DataRow("NF_2b", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func(y:obj)->ind; Test(x)}", 0)>]
    [<DataRow("NF_2c", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func(y:obj)->obj; Test(x)}", 1)>]
    [<DataRow("NF_2d", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func(y:ind)->ind; Test(x)}", 1)>]
    [<DataRow("NF_2e", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func(y:obj)->func; Test(x)}", 1)>]
    [<DataRow("NF_2f", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:func(y:obj)->func(z:pred)->pred; Test(x)}", 1)>]
    [<DataRow("NF_3", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NF_3a", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:pred(); Test(x)}", 1)>]
    [<DataRow("NF_3b", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:pred; Test(x)}", 1)>]
    [<DataRow("NF_3c", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:pred(y:obj); Test(x)}", 1)>]
    [<DataRow("NF_3d", "def pred Test(v:func(a:obj)->ind) def pred T() {dec x:pred(y:ind); Test(x)}", 1)>]

    // match with class type
    [<DataRow("CT1", "def cl A def pred Test(v:obj) def pred T() {dec x:A; Test(x)}", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A def pred Test(v:A) def pred T() {dec x:A; Test(x)}", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A def cl B:A def pred Test(v:A) def pred T() {dec x:B; Test(x)}", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec x:B; Test(x)}", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A def cl B:A def pred Test(v:obj) def pred T() {dec x:B; Test(x)}", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A def pred Test(v:obj) def pred T() {dec x:A x:=A(); Test(x)}", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A def pred Test(v:A) def pred T() {dec x:A x:=A(); Test(x)}", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A def cl B:A def pred Test(v:A) def pred T() {dec x:B x:=B(); Test(x)}", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec x:B x:=B(); Test(x)}", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A def cl B:A def pred Test(v:obj) def pred T() {dec x:B x:=B(); Test(x)}", 0)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_", "def pred Test(v:obj) def pred T() {dec x:A; Test(x)}", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A def pred Test(v:A) def pred T() {dec x:obj; Test(x)}", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec a:A; Test(a)}", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "def pred Test(v:obj) def pred T() {dec x:A x:=A; Test(x)}", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec a:A a:=A; Test(a)}", 1)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec a:B a:=B; Test(a)}", 1)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A def pred Test(v:obj) def pred T() {dec x:A x:=A; Test(x)}", 1)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec x:B x:=B; Test(x)}", 1)>] // B is B, but x is class reference, error
    [<DataRow("CI6_", "def cl A def pred Test(v:A) def pred T() {dec x:A x:=A; Test(x)}", 1)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A def pred Test(v:A) def pred T() {dec x:B x:=B; Test(x)}", 1)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec x:B x:=B; Test(x)}", 1)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A def pred Test(v:obj) def pred T() {dec x:B x:=B; Test(x)}", 1)>] // B is obj but x is class reference, error

    // match with the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 0)>] // OK: ->pred(y:obj) matches signature A(obj), whole node would be returned
    [<DataRow("MS1a", "def pred A(z:obj) def pred Test(v:pred(y:obj)) def pred T() {dec x:obj; Test(A(x))}", 1)>] // SIG04: pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) def pred Test(v:pred(y:obj)) def pred T() {dec x:ind; Test(A(x))}", 2)>] // SIG04: pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) def pred Test(v:pred(y:obj)) def pred T() {dec x:ind; Test(A(x))}", 1)>] // SIG04: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A$1)}", 1)>] // SIG04: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1: trivial}def pred Test(v:pred(y:obj)) def pred T() {Test(A$1)}", 1)>] // SIG04: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A def pred Test(v:pred(y:obj)) def pred T() {Test(A)}", 1)>] // SIG04: pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec a:obj o:A o:=A(); Test(o.X(a))}", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_", "def pred A() def pred Test(v:pred()) def pred T() {Test(A)}", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "def pred A() def pred Test(v:pred()) def pred T() {dec x:obj; Test(A(x))}", 2)>] // SIG04: pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() def pred Test(v:pred()) def pred T() {dec x:ind; Test(A(x))}", 2)>] // SIG04: pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() def pred Test(v:pred()) def pred T() {dec x:ind; Test(A(x))}", 2)>] // SIG04: pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} def pred Test(v:pred()) def pred T() {Test(A$1)}", 1)>] // SIG04: pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1: trivial}def pred Test(v:pred()) def pred T() {Test(A$1)}", 1)>] // SIG04: pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A def pred Test(v:pred()) def pred T() {Test(A)}", 1)>] // SIG04: pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } def pred Test(v:pred()) def pred T() {Test(A.X)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } def pred Test(v:pred()) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec a:obj o:A o:=A(); Test(o.X(a))}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } def pred Test(v:pred()) def pred T() {Test(A.X)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } def pred Test(v:pred()) def pred T() {Test(A.X)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2", "def pred A(z:obj) def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) def pred Test(v:pred) def pred T() {dec x:obj; Test(A(x))}", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) def pred Test(v:pred) def pred T() {dec x:ind; Test(A(x))}", 2)>] // SIG04: pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) def pred Test(v:pred) def pred T() {dec x:ind; Test(A(x))}", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} def pred Test(v:pred) def pred T() {Test(A)}", 0)>] // OK: ->pred matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} def pred Test(v:pred) def pred T() {Test(A$1)}", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1: trivial}def pred Test(v:pred) def pred T() {Test(A$1)}", 0)>] // OK: pred matches signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} def pred Test(v:pred) def pred T() {Test(A)}", 1)>] // SIG04: pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj def pred Test(v:pred) def pred T() {Test(A)}", 1)>] // SIG04: pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred Test(v:pred) def pred T() {Test(A)}", 1)>] // SIG04: pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A def pred Test(v:pred) def pred T() {Test(A)}", 1)>] // SIG04: pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec a:obj; Test(A.X(a))}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } def pred Test(v:pred) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } def pred Test(v:pred) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec a:obj o:A o:=A(); Test(o.X(a))}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec a:obj; T(A.X(a))}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } def pred Test(v:pred) def pred T() {Test(A.X)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {Test(A.X)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec a:obj; Test(A.X(a))}", 0)>] // OK: pred matches by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind def pred Test(v:func(y:obj)->ind) def pred T() {dec x:obj; Test(A(x))}", 1)>] // SIG04: func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind def pred Test(v:func(y:obj)->ind) def pred T() {dec x:obj; Test(A(x))}", 1)>] // SIG04: func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind def pred Test(v:func(y:obj)->ind) def pred T() {dec x:obj; Test(A(x))}", 2)>] // SIG04: func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A$1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1: trivial}def pred Test(v:func(y:obj)->ind) def pred T() {Test(A$1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec a:obj o:A o:=A(); Test(o.X(a))}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {Test(A)}", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {dec h:obj; Test(A(h))}", 2)>] // SIG04: func()->ind does not match value A(obj) not matching A()
    [<DataRow("MS3b_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {Test(A())}", 1)>] // SIG04: func()->ind does not match value A(ind) not matching A()
    [<DataRow("MS3c_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {dec h:ind; Test(A(h))}", 2)>] // SIG04: func()->ind does not match value A(ind) not matching A()
    [<DataRow("MS3d_", "def func A(z:ind)->ind def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} def pred Test(v:func()->ind) def pred T() {Test(A$1)}", 1)>] // SIG04: func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1: trivial}def pred Test(v:func()->ind) def pred T() {Test(A$1)}", 1)>] // SIG04: func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A def pred Test(v:func()->ind) def pred T() {Test(A)}", 1)>] // SIG04: func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec a:obj o:A o:=A(); Test(o.X(a))}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4", "def func A(z:obj)->ind def pred Test(v:func) def pred T() {Test(A)}", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind def pred Test(v:func) def pred T() {dec x:obj; Test(A(x))}", 1)>] // SIG04: func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind def pred Test(v:func) def pred T() {dec x:obj; Test(A(x))}", 1)>] // SIG04: func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind def pred Test(v:func) def pred T() {dec x:obj; Test(A(x))}", 2)>] // SIG04: func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind def pred Test(v:func) def pred T() {Test(A)}", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (axiom)
    [<DataRow("MS4f", "thm A {true} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (theorem)
    [<DataRow("MS4g", "lem A {true} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (lemma)
    [<DataRow("MS4h", "prop A {true} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (proposition)
    [<DataRow("MS4i", "conj A {true} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} def pred Test(v:func) def pred T() {Test(A$1)}", 1)>] // SIG04: func does not match signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1: trivial}def pred Test(v:func) def pred T() {Test(A$1)}", 1)>] // SIG04: func does not match signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj def pred Test(v:func) def pred T() {Test(A)}", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A def pred Test(v:func) def pred T() {Test(A)}", 1)>] // SIG04: func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } def pred Test(v:func) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } def pred Test(v:func) def pred T() {dec o:A o:=A(); Test(o.X)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } def pred Test(v:func) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {dec o:A o:=A(); Test(o.X)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec a:obj o:A o:=A(); Test(o.X(a))}", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } def pred Test(v:func) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } def pred Test(v:func) def pred T() {Test(A.X)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {Test(A.X)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec a:obj; Test(A.X(a))}", 1)>] // SIG04: func does not match by value A.X(obj) 

    [<DataRow("00", "def cl Test {intr}", 0)>]
    [<DataRow("01", "def cl Test:Set {intr}", 0)>] // this should cause the ID010 error only and not SIG04
    [<DataRow("02", "def class Set def cl Test:Set {intr}", 0)>]
    [<DataRow("03", "def cl Set {intr} def cl Test:Set {intr}", 0)>]
    [<DataRow("04", "def cl Set {intr} def cl Test:SetTypo {intr}", 0)>] // this should cause the ID010 error only and not SIG04
    [<DataRow("06", "def cl Set {intr} def pred Test(x:Set) {intr}", 0)>]
    [<DataRow("09", "def cl Set {intr} axiom Test {dec x:Set; true}", 0)>]
    [<DataRow("10", "def cl Set {intr} def func PowerSer(x:Set) -> Set {dec y:Set; return y}", 0)>]
    [<DataRow("11", "def cl Set {intr} axiom Test {dec x:Set; true}", 0)>]
    [<DataRow("15a", "def cl Set def pred Test() {dec x:object; is(x,Set)}", 0)>]
    [<DataRow("16", "def cl Set def pred Test() {dec x:object; is(x,Set)}", 0)>]
    [<DataRow("16a", "def cl C {ctor C(x:ind) {}} def cl A:C { ctor A() {dec x:obj base.C(x); } }", 1)>]
    [<DataRow("17", """def pred T1() {true} def pred Test() { dec x:obj; T1(x) }""", 1)>]
    [<DataRow("19", """def pred T (x:obj) {true} def pred Caller() {dec x:obj; T(x)} """, 0)>]
    [<DataRow("20", """def pred T (x:obj) {true} def pred Caller() {dec x:ind; T(x)} """, 1)>]
    [<DataRow("21", "inf ExistsByExample {dec p: pred(c: obj) x: obj; pre: p(c) con: ex y:obj {p(y)}}", 0)>]
    [<DataRow("22", """loc NotEqual(x,y) := !tex: x "\neq" y; """, 0)>]
    [<DataRow("23", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec x,y:obj; (x = y) }""", 0)>]
    [<DataRow("24", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec x,y:obj; Eq(x,y) }""", 0)>]
    [<DataRow("25", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec x:ind y:obj; (x = y) }""", 1)>]
    [<DataRow("26", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec x:ind y:obj; (x = y) }""", 1)>]
    [<DataRow("27", """def cl Nat def pred Eq(x,y: Nat) infix "=" 1000 {intr} axiom A {dec x:ind y:obj; (x = y) }""", 2)>]
    [<DataRow("28", """def pred Eq(x,y: ind) infix "=" 1000 {intr} axiom A {dec x:ind y:obj; (x = y) }""", 1)>]
    [<DataRow("29", """def pred Mul(x,y: pred) infix "*" 2 {intr} def pred Add(x,y: ind) infix "+" 1 {intr} def pred Eq (x,y: obj) infix "=" -100 {intr} def pred T1() { (x = y * z + 1) }""", 5)>]
    [<DataRow("30", """def pred T (x:tpl) {true} def pred Caller() {dec x:ind; T(x)} """, 0)>]
    [<DataRow("31", """def pred T (x:tplTest) {true} def pred Caller() {dec x:ind; T(x)} """, 0)>]
    [<DataRow("32", """def pred T (x,y,z:obj) {true} def pred Caller() {dec x:obj; T(x)} """, 1)>]
    [<DataRow("33", """def pred T (x,y,z:obj) {true} def pred Caller() {dec x:obj; T(x,x)} """, 1)>]
    [<DataRow("34", """def pred T (x,y,z:obj) {true} def pred Caller() {dec x:obj; T(x,x,x)} """, 0)>]
    [<DataRow("35", """def pred T (x,y,z:obj) {true} def pred Caller() {dec x,y:obj z:ind; T(x,y,z)} """, 1)>]
    [<DataRow("36", """def pred T (x,y:obj,z:ind) {true} def pred Caller() {dec x,y:obj z:ind; T(x,y,z)} """, 0)>]
    [<DataRow("37", """def pred T (x,y:obj) {true} def pred Caller() {dec x,y:obj z:ind; T(x,y,z)} """, 1)>]
    [<DataRow("38", """def class Nat  {ctor Nat(){dec self:=x.R(); }}""", 0)>] // this would cause SIG03 error
    [<DataRow("40", """def func T()->obj { dec x:obj; return x}""", 0)>]
    [<DataRow("40a", """def func S(n:obj) -> obj {intr} def func T()->obj { dec x:obj; return S(x)} """, 0)>]
    [<DataRow("40b", """def func S(n:obj) -> obj {intr} def func T()->obj { dec x:obj; return (S(x)) } """, 0)>]
    [<DataRow("40c", """def func S(n:obj) -> obj {intr} def func T()->obj { dec x:obj; return S(S(x))} """, 0)>]
    [<DataRow("41", """def func T(y:obj)->obj { return self(y)} """, 0)>]
    [<DataRow("42", """def func T(y:obj)->obj def func S()->obj {dec x:obj; return T(x)} """, 0)>]
    [<DataRow("43", """axiom T { dec x:obj; all p:pred(y:obj) {p(x)}}""", 0)>]
    [<DataRow("44", """def cl A {intr} def func Add(n,m:A)->A {return self(n,m)}""", 0)>]
    [<DataRow("45", """def cl A {intr} def func Add(n,m:A)->A {dec x:A; return x}""", 0)>]
    [<DataRow("46", """def cl A {intr} def func Add(n,m:A)->A {dec x:A; return x} prop P {dec op:Add; true}""", 0)>]
    [<DataRow("47", """def cl A {intr property pred T() {true} property pred S() {T()}}""", 0)>]
    [<DataRow("48", """def cl Obj def cl A:Obj {dec x:obj; ctor A(y:obj) {dec base.Obj() x:=y; } property func P()->obj {return x}} def pred T(r:A) {r.P()}""", 0)>]
    [<DataRow("49", """def cl A:Obj {ctor A(y:*obj[ind]) {}} def class B {ctor B(z:*obj[ind]) {dec a:A base.Obj() a := A(z); }}""", 0)>]
    [<DataRow("50", """def cl A {intr property pred T() {true}} def cl B:A {ctor B() {dec base.A() assert self.T(); }}""", 0)>]
    [<DataRow("51", """def func A(n,m:obj)->obj {intr} prop T {dec op:A x,y:obj; (op(x,y) = x)}""", 0)>]
    [<DataRow("52", """def cl T { dec x:*tpl[ind]; ctor T(y:*tpl[ind]) {dec x:=y; } property func C(i:ind) -> tpl {return x[i]}}""", 0)>]
    [<DataRow("53", """def cl Nat ext D x@/\d+/ -> Nat {dec n,m:Nat cases ( | (x = @0): n:=m ? m:=n ); return n } def func Add(x,y:Nat)->obj {intr} prop K {dec op:Add n:Nat; ( op(n,@0) = n ) } """, 0)>]
    [<DataRow("54", """def cl C {ctor C() {}} def pred T() {dec cI1:C cI1:=C; true } """, 1)>]
    [<DataRow("54a", """def cl C {ctor C() {}} def pred T() {dec cI1:C cI1:=C(); true } """, 0)>]
    [<DataRow("54b", """def cl C {ctor C(x:obj) {}} def pred T() {dec cI1:C cI1:=C(); true } """, 1)>]
    [<DataRow("54c", """def cl C {ctor C(x:obj) {}} def pred T() {dec x:obj cI1:C cI1:=C(x); true } """, 0)>]
    [<DataRow("54b_", """def cl C1 {ctor C1(i1:ind) {dec o:ind o:=i1; }} """, 0)>]
    [<DataRow("54c_", """def cl C1 {ctor C1(i1:ind) {dec o:ind o:=i1; }} """, 0)>]
    [<DataRow("55", """def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec dI1:D dI1:=D; true } """, 1)>] // D mismatches D()
    [<DataRow("55a", """def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec dI1:D dI1:=D(); true } """, 0)>]
    [<DataRow("55b", """def cl B: A {intr} def cl D: B {ctor D(x:obj) {dec base.B();  }} def pred T() {dec dI1:D dI1:=D; true } """, 1)>]
    [<DataRow("55c", """def cl B: A {intr} def cl D: B {ctor D(x:obj) {dec base.B();  }} def pred T() {dec dI1:D dI1:=D(); true } """, 1)>]
    [<DataRow("56", """def pred T() {dec x:ind x:=$1; true } """, 0)>]
    [<DataRow("57", """def pred T() {dec x:pred x:=true; true } """, 0)>]
    [<DataRow("57a", """def pred T() {dec x:pred x:=not true; true } """, 0)>]
    [<DataRow("66", """def cl Set def pred In(x,y: Set) def cl SetRoster:Set { ctor SetRoster(list:* Set[ind]) { dec e:Set base.Set() for e in list {assert In(e, parent)}; } }""", 0)>]
    [<DataRow("67", """def class Set def pred In(x,y: Set) def pred IsEmpty(x: Set) { all y:Set { not In(y, x) } }""", 0)>]
    [<DataRow("68", """def class Set def pred In(x,y: Set) def cl SetBuilder: Set { ctor SetBuilder(x: Set, p: pred(u1: Set, o:* obj[ind])) { dec base.Set() assert all u2:Set { iif (In(u2,parent), and ( In(u2,x), p(u2,o) ) ) }; } }""", 0)>]
    [<DataRow("69", """ext Digits x@/\d+/ -> Digits {ret x} def cl A {dec myX:Digits; ctor A(x:Digits) {dec myX:=x;}} def cl B:A { ctor B(x:Digits) {dec base.A(del.Decrement(x)); } } def pred T() { dec v:B v:=B(@2); false}""", 0)>]
    [<DataRow("70", """def cl Nat def func Succ(n: Nat) -> Nat ext Digits x@/\d+/ -> Nat {ret mcases (| true: Nat() ? Succ(self(delegate.Decrement(x))) ) }""", 0)>]    
    [<DataRow("71", """def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y) } def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def pred T() {all x,y:Nat {(x = Succ(y))}}""", 0)>]    
    [<DataRow("72", "def pred S() {dec parent():=true; true}", 0)>] // SIG04 won't be issued due to proceeding errors (ID015)
    [<DataRow("72a", "thm S {dec self():=true; true}", 0)>] // SIG04 won't be issued due to proceeding errors (ID015)
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG04(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("", "")
            
            runTestHelper "TestSIG04.fpl" fplCode code expected

    // -----------------------------

    // match with simple types
    [<DataRow("ST0","ext U x@/\d+/ -> obj { dec a:obj; ret a } def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST1","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST1a","ext U x@/\d+/ -> ind {ret $1} def pred T(v:ind) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2a","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2b","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2c","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2d","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3a","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3b","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3c","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3d","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3e","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST1_obj","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2_obj","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2a_obj","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2b_obj","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2c_obj","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2d_obj","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3_obj","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3a_obj","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3b_obj","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3c_obj","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST1_ind","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2_ind","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2a_ind","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2b_ind","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2c_ind","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2d_ind","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3_ind","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3a_ind","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3b_ind","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3c_ind","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:ind) {true} def pred Test() {T(@1)}", 1)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST1_pred","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2_pred","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2a_pred","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2b_pred","ext U x@/\d+/ -> func(y:obj)->pred {dec a:func(y:obj)->pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2c_pred","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2d_pred","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3_pred","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3a_pred","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3b_pred","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3c_pred","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST1_func","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST2_func","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2a_func","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2b_func","ext U x@/\d+/ -> func(y:obj)->pred {dec a:func(y:obj)->pred; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2c_func","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST2d_func","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("ST3_func","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3a_func","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3b_func","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("ST3c_func","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>]

    // (mis)match with pred() types
    [<DataRow("NP0","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP1","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP2","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP2a","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP2b","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP2c","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP2d","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP3","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP3a","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("NP3b","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP3c","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_1","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_2","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_2a","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_2b","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_2c","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_2d","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_3","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_3a","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_3b","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NP_3c","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("NP_3c","ext U x@/\d+/ -> pred(y:ind) {dec a:pred(y:ind); ret a} def pred T(v:pred(a:obj)) {true} def pred Test() {T(@1)}", 1)>]

    // (mis)match with func() types
    [<DataRow("NF0","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF1","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF2","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF2a","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("NF2b","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF2c","ext U x@/\d+/ -> func(y:obj)->obj {dec a:func(y:obj)->obj; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF2d","ext U x@/\d+/ -> func(y:ind)->ind {dec a:func(y:ind)->ind; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF2e","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF2f","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF3","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF3a","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF3b","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF3c","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0","ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_1","ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_2","ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_2a","ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_2b","ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("NF_2c","ext U x@/\d+/ -> func(y:obj)->obj {dec a:func(y:obj)->obj; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_2d","ext U x@/\d+/ -> func(y:ind)->ind {dec a:func(y:ind)->ind; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_2e","ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_2f","ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_3","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_3a","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_3b","ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_3c","ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]
    [<DataRow("NF_3d","ext U x@/\d+/ -> pred(y:ind) {dec a:pred(y:ind); ret a} def pred T(v:func(a:obj)->ind) {true} def pred Test() {T(@1)}", 1)>]

    // match with class type
    [<DataRow("CT1","def cl A ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>] // A is obj, no error
    [<DataRow("CT2","def cl A ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:A) {true} def pred Test() {T(@1)}", 0)>] // A is A, no error
    [<DataRow("CT3","def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B; ret a} def pred T(v:A) {true} def pred Test() {T(@1)}", 0)>] // x is also B:A, no error
    [<DataRow("CT4","def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B; ret a} def pred T(v:B) {true} def pred Test() {T(@1)}", 0)>] // x is B, no error
    [<DataRow("CT5","def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1","def cl A ext U x@/\d+/ -> A {dec a:A a:=A(); ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>] // A is obj, no error
    [<DataRow("CI2","def cl A ext U x@/\d+/ -> A {dec a:A a:=A(); ret a} def pred T(v:A) {true} def pred Test() {T(@1)}", 0)>] // A is A, no error
    [<DataRow("CI3","def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B a:=B(); ret a} def pred T(v:A) {true} def pred Test() {T(@1)}", 0)>] // x is also B:A, no error
    [<DataRow("CI4","def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B a:=B(); ret a} def pred T(v:B) {true} def pred Test() {T(@1)}", 0)>] // x is B, no error
    [<DataRow("CI5","def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B a:=B(); ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_","ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>] // A is undefined, error
    [<DataRow("CT2_","def cl A ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:A) {true} def pred Test() {T(@1)}", 1)>] // obj is not A, error
    [<DataRow("CT3_","def cl A def cl B:A ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:B) {true} def pred Test() {T(@1)}", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_","ext U x@/\d+/ -> A {ret A} def pred T(v:obj) {true} def pred Test() {T(@1)}", 1)>] // A is undefined, error
    [<DataRow("CI2_","def cl A def cl B:A ext U x@/\d+/ -> A {ret A} def pred T(v:B) {true} def pred Test() {T(@1)}", 1)>] // A is not B; error
    [<DataRow("CI3_","def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:B) {true} def pred Test() {T(@1)}", 0)>] // B() is B
    [<DataRow("CI4_","def cl A ext U x@/\d+/ -> A {ret A} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>] // A() is obj
    [<DataRow("CI5_","def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:B) {true} def pred Test() {T(@1)}", 0)>] // B() is B
    [<DataRow("CI6_","def cl A ext U x@/\d+/ -> A {ret A()} def pred T(v:A) {true} def pred Test() {T(@1)}", 0)>] // A() is A
    [<DataRow("CI7_","def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:A) {true} def pred Test() {T(@1)}", 0)>] // B() is A 
    [<DataRow("CI8_","def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:B) {true} def pred Test() {T(@1)}", 0)>] //  B() is B
    [<DataRow("CI9_","def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:obj) {true} def pred Test() {T(@1)}", 0)>] // B() is obj 

    // match with the type pred(...) 
    [<DataRow("MS1","ext U x@/\d+/ -> pred(z:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred(y:obj) matches signature pred(obj), whole node would be returned
    [<DataRow("MS1a","ext U x@/\d+/ -> pred(z:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: pred(y:obj) matches signature pred(y:obj) 
    [<DataRow("MS1b","ext U x@/\d+/ -> pred(z:obj) {dec a:pred(y:ind); ret a} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match value pred(ind) not matching A(obj)
    [<DataRow("MS1c","ext U x@/\d+/ -> pred(z:ind) {dec a:pred(y:ind); ret a} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d","ext U x@/\d+/ -> pred(z:ind) {dec a:pred(y:ind); ret a} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e","ax A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued:  pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f","thm A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g","lem A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h","prop A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i","conj A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j","cor A$1 {true} ext U x@/\d+/ -> pred(z:obj) {ret A$1} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k","proof A$1 {1: trivial}ext U x@/\d+/ -> pred(z:obj) {ret A$1} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l","inf A {pre:true con:true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m","def func A()->obj ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n","ext U x@/\d+/ -> pred(z:ind) {dec u:pred(a:ind); ret u} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:ind) does not match pred(y:obj)
    [<DataRow("MS1o","def cl A ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec g:obj; ret A.X(g)} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> pred {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec p:obj o:A o:=A(); ret o.X(p)} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2","def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3","def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> pred {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5","def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6","def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec p:obj; ret A.X(p)} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK, only SIG03 instead of SIG04 would be issued: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2","def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3","def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred()  {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> pred {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5","def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6","def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec p:obj; ret A.X(p)} def pred T(v:pred(y:obj)) {true} def pred Test() {T(@1)}", 0)>]  // OK, only SIG03 instead of SIG04: pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_","ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_","ext U x@/\d+/ -> pred(z:obj) {dec a:pred(x:obj); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match value A(obj) 
    [<DataRow("MS1b_","ext U x@/\d+/ -> pred(z:obj) {dec a:pred(x:obj); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_","ext U x@/\d+/ -> pred(z:obj) {dec a:pred(x:obj); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match value A(ind) 
    [<DataRow("MS1d_","ext U x@/\d+/ -> pred(z:ind) {dec a:pred(x:ind); ret a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A(ind)
    [<DataRow("MS1e_","ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (axiom)
    [<DataRow("MS1f_","thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (theorem)
    [<DataRow("MS1g_","lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (lemma)
    [<DataRow("MS1h_","prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (proposition)
    [<DataRow("MS1i_","conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (conjecture)
    [<DataRow("MS1j_","cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (corollary)
    [<DataRow("MS1k_","proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (proof)
    [<DataRow("MS1l_","inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_","def func A()->obj ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (functional term)
    [<DataRow("MS1n_","ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ -> pred {ret @a} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (extension)
    [<DataRow("MS1o_","def cl A ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec y:obj; ret A.X(y)} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec a:obj o:A o:=A(); ret o.X(a)} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2","def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3","def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X()
    [<DataRow("MS1r_5","def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6","def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec a:obj; ret A.X(a)} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2","def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3","def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5","def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6","def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec a:obj; ret A.X(a)} def pred T(v:pred()) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2","def pred A(z:obj) ext U x@/\d+/ -> pred(y:obj) {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a","def pred A(z:obj) ext U x@/\d+/ -> pred(y:obj) {dec h:obj; ret A(h)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b","def pred A(z:obj) ext U x@/\d+/ -> pred(y:obj) {dec h:ind; ret A(h)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c","def pred A(z:ind) ext U x@/\d+/ -> pred(y:ind) {dec h:ind; ret A(h)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d","def pred A(z:ind) ext U x@/\d+/ -> pred(y:ind) {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e","ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f","thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g","lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h","prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i","conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A (conjecture)
    [<DataRow("MS2j","cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k","proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: pred matches signature A$1 (proof)
    [<DataRow("MS2l","inf A {pre:true con:true} ext U x@/\d+/ -> undef {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A (rule of inference)
    [<DataRow("MS2m","def func A()->obj ext U x@/\d+/ -> func()->obj {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A (functional term)
    [<DataRow("MS2n","ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ -> obj {ret @a} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A (extension)
    [<DataRow("MS2o","def cl A ext U x@/\d+/ -> A {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2p5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec a:obj; ret A.X(a)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec o:A o:=A(); ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {dec o:A o:=A(); ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2q5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec y:obj o:A o:=A(); ret o.X(y)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2","def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3","def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2r5","def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6","def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec y:obj; ret o.X(y)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2","def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3","def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2s5","def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6","def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret o.X} def pred T(v:pred) {true} def pred Test() {T(@1)}", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec y:obj; ret o.X(y)} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>] // OK: pred matches by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3",  "def func A(z:obj)->ind ext U x@/\d+/ -> func(h:obj)->ind {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a","def func A(z:obj)->ind ext Test x@/\d+/->ind {dec h:obj; return A(h)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b","def func A(z:obj)->ind ext Test x@/\d+/->ind {dec h:ind; return A(h)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 2)>] // SIG04: func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c","def func A(z:ind)->ind ext Test x@/\d+/->ind {dec h:ind; return A(h)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d","def func A(z:ind)->ind ext U x@/\d+/ -> func(h:ind)->ind {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e","ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f","thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g","lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h","prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i","conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j","cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k","proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l","inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m","def func A(z:obj)->func()->obj ext U x@/\d+/ -> func(z:obj)->func()->obj {dec a:func(h:obj)->func()->obj; ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n","ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ ->obj {ret @a} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o","def cl A ext U x@/\d+/ ->A {ret A} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.A(obj) 
    [<DataRow("MS3p2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {ret A.X} def pred T(v:func(y:obj)->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7","def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5","def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7","def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec o:A a:obj o:=A(); ret o.X(a)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2","def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3","def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5","def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {ret A.X} def pred T(v:func(y:obj)->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6","def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7","def pred A() {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2","def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3","def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5","def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {ret A.X} def pred T(v:func(y:obj)->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6","def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7","def func A()->obj {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_","def func A()->ind ext U x@/\d+/ -> func()->ind {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_","def func A()->ind ext U x@/\d+/ -> ind {dec h:obj; ret A(h)} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 2)>] // SIG04: func()->ind does not match value A(obj) (2x)
    [<DataRow("MS3b_","def func A()->ind ext U x@/\d+/ -> ind {ret A()} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_","def func A()->ind ext U x@/\d+/ -> ind {dec h:ind; ret A(h)} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 2)>] // SIG04: func()->ind does not match value A(ind) (2x)
    [<DataRow("MS3d_","def func A(z:ind)->ind ext U x@/\d+/ -> func(h:ind)->ind {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_","ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_","thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_","lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_","prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_","conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_","cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_","proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_","inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_","def func A()->func()->obj ext U x@/\d+/ -> func()->func()->obj {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_","ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ ->obj {ret @a} def pred T(v:func(y:obj)->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_","def cl A ext U x@/\d+/ ->A {ret A} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3p_4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7","def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:ind)->obj {dec a:obj; ret A.X(a)} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3q_4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7","def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:obj)->ind {dec o:A a:obj o:=A(); ret o.X(a)} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2","def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3","def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3r_4","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6","def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7","def pred A() {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:obj)->ind {dec a:obj; ret A.X(a)} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2","def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3","def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3s_4","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6","def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7","def func A()->obj {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:obj)->ind {dec a:obj; ret A.X(a)} def pred T(v:func()->ind) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4","def func A(z:obj)->ind ext U x@/\d+/ -> func(z:obj)->ind {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a","def func A(z:obj)->ind ext U x@/\d+/ -> ind {dec y:obj; ret A(y)} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match value A(obj) 
    [<DataRow("MS4b","def func A(z:obj)->ind ext U x@/\d+/ -> ind {dec y:obj; ret A(y)} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match value A(obj) 
    [<DataRow("MS4c","def func A(z:ind)->ind ext U x@/\d+/ -> ind {dec y:obj; ret A(y)} def pred T(v:func) {true} def pred Test() {T(@1)}", 2)>] // SIG04: func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4d","def func A(z:ind)->func(a:obj)->ind ext U x@/\d+/ -> func(h:ind)->func(g:obj)->ind {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e","ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (axiom)
    [<DataRow("MS4f","thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (theorem)
    [<DataRow("MS4g","lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (lemma)
    [<DataRow("MS4h","prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (proposition)
    [<DataRow("MS4i","conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (conjecture)
    [<DataRow("MS4j","cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A$1 (corollary)
    [<DataRow("MS4k","proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A$1 (proof)
    [<DataRow("MS4l","inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (rule of inference)
    [<DataRow("MS4m","def func A(z:obj)->func()->obj ext U x@/\d+/ -> func(d:obj)->func()->obj {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n","ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ ->obj {ret @a} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (extension)
    [<DataRow("MS4o","def cl A ext U x@/\d+/ ->A {ret A} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4p2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4p3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4p4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(d:ind)->obj {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7","def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> obj {dec a:obj; ret A.X(a)} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1","def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {dec o:A o:=A(); ret o.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4q2","def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {dec o:A o:=A(); ret o.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4q3","def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4q4","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5","def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6","def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7","def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec o:A a:obj o:=A(); ret o.X(a)} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1","def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4r2","def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4r3","def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4r4","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5","def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6","def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7","def pred A() {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1","def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4s2","def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4s3","def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4s4","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5","def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6","def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func) {true} def pred Test() {T(@1)}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7","def func A()->obj {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func) {true} def pred Test() {T(@1)}", 1)>] // SIG04: func does not match by value A.X(obj) 

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG04Extensions(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("", "")
            
            runTestHelper "TestSIG04Extensions.fpl" fplCode code expected


    // match with simple types
    [<DataRow("ST0", "def pred Test(v:obj) def pred T() {Test(undef)}", 0)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def pred Test(v:ind) def pred T() {Test(undef)}", 0)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def pred Test(v:pred) def pred T() {Test(undef)}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def pred Test(v:func) def pred T() {Test(undef)}", 0)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def pred Test(v:pred()) def pred T() {Test(undef)}", 0)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def pred Test(v:pred(a:obj)) def pred T() {Test(undef)}", 0)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def pred Test(v:func()->ind) def pred T() {Test(undef)}", 0)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "def pred Test(v:func(a:obj)->ind) def pred T() {Test(undef)}", 0)>]
    [<DataRow("NF_0", "def pred Test(v:func(a:obj)->obj) def pred T() {Test(undef)}", 0)>]
    [<DataRow("NF_0", "def pred Test(v:func(a:ind)->ind) def pred T() {Test(undef)}", 0)>]
    [<DataRow("NF_0", "def pred Test(v:func(a:ind)->func(b:obj)->pred) def pred T() {Test(undef)}", 0)>]

    // match with class type
    [<DataRow("CT2", "def cl A def pred Test(v:A) def pred T() {Test(undef)}", 0)>] // A is A, no error
    [<DataRow("CT4", "def cl A def cl B:A def pred Test(v:B) def pred T() {Test(undef)}", 0)>] // x is B, no error

    [<TestMethod>]
    member this.TestSIG04Undef(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("", "")
            
            runTestHelper "TestSIG04Undef.fpl" fplCode code expected
