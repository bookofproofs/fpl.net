namespace Diagnostics.SignatureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG03
   Purpose: Report that a returned expression does not match the expected mapping/signature of the enclosing functional term.
   What it indicates: The `return` expression's type or signature failed the type/signature matcher (the matcher produced a concrete error message which is forwarded as the diagnostic text).
   Use: Pinpoint return statements whose value or form is incompatible with the function's declared/mapped result type (wrong arity, wrong argument/value shape, or mismatched mapping target).
   Action / Treat: Fix the return so it matches the function's declared or mapped signature (adjust the returned expression, change the function/mapping signature, or provide a compatible mapping). Treat SIG03 as an error that must be resolved for correct function/mapping semantics. *)

[<TestClass>]
type TestSIG03() =

    // match with simple types
    [<DataRow("ST0", "def func Test()->obj {dec x:obj; return x}", 0)>]
    [<DataRow("ST1", "def func Test()->ind {dec x:ind; return x}", 0)>]
    [<DataRow("ST1a", "def func Test()->ind {return $1}", 0)>]
    [<DataRow("ST2", "def func Test()->func {dec x:func; return x}", 0)>]
    [<DataRow("ST2a", "def func Test()->func {dec x:func()->ind; return x}", 0)>]
    [<DataRow("ST2b", "def func Test()->func {dec x:func(y:obj)->ind; return x}", 0)>]
    [<DataRow("ST2c", "def func Test()->func {dec x:func(y:obj)->func; return x}", 0)>]
    [<DataRow("ST2d", "def func Test()->func {dec x:func(y:obj)->func(z:pred)->pred; return x}", 0)>]
    [<DataRow("ST3", "def func Test()->pred {dec x:pred; return x}", 0)>]
    [<DataRow("ST3a", "def func Test()->pred {dec x:pred(); return x}", 0)>]
    [<DataRow("ST3b", "def func Test()->pred {dec x:pred; return x}", 0)>]
    [<DataRow("ST3c", "def func Test()->pred {dec x:pred(y:obj); return x}", 0)>]
    [<DataRow("ST3d", "def func Test()->pred {return true}", 0)>]
    [<DataRow("ST3e", "def func Test()->pred {return false}", 0)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj", "def func Test()->obj {dec x:obj; return x}", 0)>]
    [<DataRow("ST1_obj", "def func Test()->obj {dec x:ind; return x}", 1)>]
    [<DataRow("ST2_obj", "def func Test()->obj {dec x:func; return x}", 1)>]
    [<DataRow("ST2a_obj", "def func Test()->obj {dec x:func()->ind; return x}", 1)>]
    [<DataRow("ST2b_obj", "def func Test()->obj {dec x:func(y:obj)->ind; return x}", 1)>]
    [<DataRow("ST2c_obj", "def func Test()->obj {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("ST2d_obj", "def func Test()->obj {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("ST3_obj", "def func Test()->obj {dec x:pred; return x}", 1)>]
    [<DataRow("ST3a_obj", "def func Test()->obj {dec x:pred(); return x}", 1)>]
    [<DataRow("ST3b_obj", "def func Test()->obj {dec x:pred; return x}", 1)>]
    [<DataRow("ST3c_obj", "def func Test()->obj {dec x:pred(y:obj); return x}", 1)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def func Test()->ind {dec x:obj; return x}", 1)>]
    [<DataRow("ST1_ind", "def func Test()->ind {dec x:ind; return x}", 0)>]
    [<DataRow("ST2_ind", "def func Test()->ind {dec x:func; return x}", 1)>]
    [<DataRow("ST2a_ind", "def func Test()->ind {dec x:func()->ind; return x}", 1)>]
    [<DataRow("ST2b_ind", "def func Test()->ind {dec x:func(y:obj)->ind; return x}", 1)>]
    [<DataRow("ST2c_ind", "def func Test()->ind {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("ST2d_ind", "def func Test()->ind {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("ST3_ind", "def func Test()->ind {dec x:pred; return x}", 1)>]
    [<DataRow("ST3a_ind", "def func Test()->ind {dec x:pred(); return x}", 1)>]
    [<DataRow("ST3b_ind", "def func Test()->ind {dec x:pred; return x}", 1)>]
    [<DataRow("ST3c_ind", "def func Test()->ind {dec x:pred(y:obj); return x}", 1)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def func Test()->pred {dec x:obj; return x}", 1)>]
    [<DataRow("ST1_pred", "def func Test()->pred {dec x:ind; return x}", 1)>]
    [<DataRow("ST2_pred", "def func Test()->pred {dec x:func; return x}", 1)>]
    [<DataRow("ST2a_pred", "def func Test()->pred {dec x:func()->ind; return x}", 1)>]
    [<DataRow("ST2b_pred", "def func Test()->pred {dec x:func(y:obj)->pred; return x}", 1)>]
    [<DataRow("ST2c_pred", "def func Test()->pred {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("ST2d_pred", "def func Test()->pred {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("ST3_pred", "def func Test()->pred {dec x:pred; return x}", 0)>]
    [<DataRow("ST3a_pred", "def func Test()->pred {dec x:pred(); return x}", 0)>]
    [<DataRow("ST3b_pred", "def func Test()->pred {dec x:pred; return x}", 0)>]
    [<DataRow("ST3c_pred", "def func Test()->pred {dec x:pred(y:obj); return x}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def func Test()->func {dec x:obj; return x}", 1)>]
    [<DataRow("ST1_func", "def func Test()->func {dec x:ind; return x}", 1)>]
    [<DataRow("ST2_func", "def func Test()->func {dec x:func; return x}", 0)>]
    [<DataRow("ST2a_func", "def func Test()->func {dec x:func()->ind; return x}", 0)>]
    [<DataRow("ST2b_func", "def func Test()->func {dec x:func(y:obj)->pred; return x}", 0)>]
    [<DataRow("ST2c_func", "def func Test()->func {dec x:func(y:obj)->func; return x}", 0)>]
    [<DataRow("ST2d_func", "def func Test()->func {dec x:func(y:obj)->func(z:pred)->pred; return x}", 0)>]
    [<DataRow("ST3_func", "def func Test()->func {dec x:pred; return x}", 1)>]
    [<DataRow("ST3a_func", "def func Test()->func {dec x:pred(); return x}", 1)>]
    [<DataRow("ST3b_func", "def func Test()->func {dec x:pred; return x}", 1)>]
    [<DataRow("ST3c_func", "def func Test()->func {dec x:pred(y:obj); return x}", 1)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def func Test()->pred() {dec x:obj; return x}", 1)>]
    [<DataRow("NP1", "def func Test()->pred() {dec x:ind; return x}", 1)>]
    [<DataRow("NP2", "def func Test()->pred() {dec x:func; return x}", 1)>]
    [<DataRow("NP2a", "def func Test()->pred() {dec x:func()->ind; return x}", 1)>]
    [<DataRow("NP2b", "def func Test()->pred() {dec x:func(y:obj)->ind; return x}", 1)>]
    [<DataRow("NP2c", "def func Test()->pred() {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("NP2d", "def func Test()->pred() {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("NP3", "def func Test()->pred() {dec x:pred; return x}", 1)>]
    [<DataRow("NP3a", "def func Test()->pred() {dec x:pred(); return x}", 0)>]
    [<DataRow("NP3b", "def func Test()->pred() {dec x:pred; return x}", 1)>]
    [<DataRow("NP3c", "def func Test()->pred() {dec x:pred(y:obj); return x}", 1)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def func Test()->pred(a:obj) {dec x:obj; return x}", 1)>]
    [<DataRow("NP_1", "def func Test()->pred(a:obj) {dec x:ind; return x}", 1)>]
    [<DataRow("NP_2", "def func Test()->pred(a:obj) {dec x:func; return x}", 1)>]
    [<DataRow("NP_2a", "def func Test()->pred(a:obj) {dec x:func()->ind; return x}", 1)>]
    [<DataRow("NP_2b", "def func Test()->pred(a:obj) {dec x:func(y:obj)->ind; return x}", 1)>]
    [<DataRow("NP_2c", "def func Test()->pred(a:obj) {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("NP_2d", "def func Test()->pred(a:obj) {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("NP_3", "def func Test()->pred(a:obj) {dec x:pred; return x}", 1)>]
    [<DataRow("NP_3a", "def func Test()->pred(a:obj) {dec x:pred(); return x}", 1)>]
    [<DataRow("NP_3b", "def func Test()->pred(a:obj) {dec x:pred; return x}", 1)>]
    [<DataRow("NP_3c", "def func Test()->pred(a:obj) {dec x:pred(y:obj); return x}", 0)>]
    [<DataRow("NP_3c", "def func Test()->pred(a:obj) {dec x:pred(y:ind); return x}", 1)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def func Test()->func()->ind {dec x:obj; return x}", 1)>]
    [<DataRow("NF1", "def func Test()->func()->ind {dec x:ind; return x}", 1)>]
    [<DataRow("NF2", "def func Test()->func()->ind {dec x:func; return x}", 1)>]
    [<DataRow("NF2a", "def func Test()->func()->ind {dec x:func()->ind; return x}", 0)>]
    [<DataRow("NF2b", "def func Test()->func()->ind {dec x:func(y:obj)->ind; return x}", 1)>]
    [<DataRow("NF2c", "def func Test()->func()->ind {dec x:func(y:obj)->obj; return x}", 1)>]
    [<DataRow("NF2d", "def func Test()->func()->ind {dec x:func(y:ind)->ind; return x}", 1)>]
    [<DataRow("NF2e", "def func Test()->func()->ind {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("NF2f", "def func Test()->func()->ind {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("NF3", "def func Test()->func()->ind {dec x:pred; return x}", 1)>]
    [<DataRow("NF3a", "def func Test()->func()->ind {dec x:pred(); return x}", 1)>]
    [<DataRow("NF3b", "def func Test()->func()->ind {dec x:pred; return x}", 1)>]
    [<DataRow("NF3c", "def func Test()->func()->ind {dec x:pred(y:obj); return x}", 1)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "def func Test()->func(a:obj)->ind {dec x:obj; return x}", 1)>]
    [<DataRow("NF_1", "def func Test()->func(a:obj)->ind {dec x:ind; return x}", 1)>]
    [<DataRow("NF_2", "def func Test()->func(a:obj)->ind {dec x:func; return x}", 1)>]
    [<DataRow("NF_2a", "def func Test()->func(a:obj)->ind {dec x:func()->ind; return x}", 1)>]
    [<DataRow("NF_2b", "def func Test()->func(a:obj)->ind {dec x:func(y:obj)->ind; return x}", 0)>]
    [<DataRow("NF_2c", "def func Test()->func(a:obj)->ind {dec x:func(y:obj)->obj; return x}", 1)>]
    [<DataRow("NF_2d", "def func Test()->func(a:obj)->ind {dec x:func(y:ind)->ind; return x}", 1)>]
    [<DataRow("NF_2e", "def func Test()->func(a:obj)->ind {dec x:func(y:obj)->func; return x}", 1)>]
    [<DataRow("NF_2f", "def func Test()->func(a:obj)->ind {dec x:func(y:obj)->func(z:pred)->pred; return x}", 1)>]
    [<DataRow("NF_3", "def func Test()->func(a:obj)->ind {dec x:pred; return x}", 1)>]
    [<DataRow("NF_3a", "def func Test()->func(a:obj)->ind {dec x:pred(); return x}", 1)>]
    [<DataRow("NF_3b", "def func Test()->func(a:obj)->ind {dec x:pred; return x}", 1)>]
    [<DataRow("NF_3c", "def func Test()->func(a:obj)->ind {dec x:pred(y:obj); return x}", 1)>]
    [<DataRow("NF_3d", "def func Test()->func(a:obj)->ind {dec x:pred(y:ind); return x}", 1)>]

    // match with class type
    [<DataRow("CT1", "def cl A {intr} def func Test()->obj {dec x:A; return x}", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A {intr} def func Test()->A {dec x:A; return x}", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A {intr} def cl B:A {intr} def func Test()->A {dec x:B; return x}", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A {intr} def cl B:A {intr} def func Test()->B {dec x:B; return x}", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A {intr} def cl B:A {intr} def func Test()->obj {dec x:B; return x}", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A {intr} def func Test()->obj {dec x:A x:=A(); return x}", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A {intr} def func Test()->A {dec x:A x:=A(); return x}", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A {intr} def cl B:A {intr} def func Test()->A {dec x:B x:=B(); return x}", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A {intr} def cl B:A {intr} def func Test()->B {dec x:B x:=B(); return x}", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A {intr} def cl B:A {intr} def func Test()->obj {dec x:B x:=B(); return x}", 0)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_", "def func Test()->obj {dec x:A; return x}", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A def func Test()->A {dec x:obj; return x}", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A def func Test()->B {dec a:A; return a}", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "def func Test()->obj {ret A}", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A def func Test()->B {ret A}", 1)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A def func Test()->B {dec a:B a:=B; return a}", 1)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A def func Test()->obj {ret A}", 1)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A def func Test()->B {ret B}", 1)>] // B is B, but x is class reference, error
    [<DataRow("CI6_", "def cl A def func Test()->A {ret A}", 1)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A def func Test()->A {ret B}", 1)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A def func Test()->B {ret B}", 1)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A def func Test()->obj {ret B}", 1)>] // B is obj but x is class reference, error

    // match with the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) def func Test()->pred(y:obj) {return A}", 0)>] // OK: ->pred(y:obj) matches signature A(obj), whole node would be returned
    [<DataRow("MS1a", "def pred A(z:obj) def func Test()->pred(y:obj) {dec x:obj; return A(x)}", 1)>] // SIG03: ->pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) def func Test()->pred(y:obj) {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) def func Test()->pred(y:obj) {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} def func Test()->pred(y:obj) {return A$1}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1: trivial}def func Test()->pred(y:obj) {return A$1}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A def func Test()->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {return A.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } def func Test()->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } def func Test()->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {return A.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {return A.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_", "def pred A() def func Test()->pred() {return A}", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "def pred A() def func Test()->pred() {dec x:obj; return A(x)}", 1)>] // SIG03: ->pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() def func Test()->pred() {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() def func Test()->pred() {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} def func Test()->pred() {return A$1}", 1)>] // SIG03: ->pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1: trivial}def func Test()->pred() {return A$1}", 1)>] // SIG03: ->pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A def func Test()->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } def func Test()->pred() {return A.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } def func Test()->pred() {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } def func Test()->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } def func Test()->pred() {return A.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred() {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } def func Test()->pred() {return A.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred() {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2", "def pred A(z:obj) def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) def func Test()->pred {dec x:obj; return A(x)}", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) def func Test()->pred {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) def func Test()->pred {dec x:ind; return A(x)}", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} def func Test()->pred {return A}", 0)>] // OK: ->pred matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} def func Test()->pred {return A$1}", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1: trivial}def func Test()->pred {return A$1}", 0)>] // OK: ->pred match signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} def func Test()->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj def func Test()->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def func Test()->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A def func Test()->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {return A.X}", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred {return A.X}", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } def func Test()->pred {return A.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {dec a:obj; return A.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } def func Test()->pred {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } def func Test()->pred {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {dec a:obj o:A o:=A(); return o.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } def func Test()->pred {return A.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred {dec a:obj; return A.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } def func Test()->pred {return A.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred {dec a:obj; return A.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind def func Test()->func(y:obj)->ind {return A}", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind def func Test()->func(y:obj)->ind {dec x:obj; return A(x)}", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind def func Test()->func(y:obj)->ind {dec x:ind; return A(x)}", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind def func Test()->func(y:obj)->ind {dec x:ind; return A(x)}", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} def func Test()->func(y:obj)->ind {return A$1}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1: trivial}def func Test()->func(y:obj)->ind {return A$1}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {return A.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } def func Test()->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } def func Test()->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {return A.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {return A.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_", "def func A()->ind def func Test()->func()->ind {return A}", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind def func Test()->func()->ind {dec h:obj; return A(h)}", 1)>] // SIG03: ->func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind def func Test()->func()->ind {return A()}", 1)>] // SIG03: ->func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind def func Test()->func()->ind {dec h:ind; return A(h)}", 1)>] // SIG03: ->func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} def func Test()->func()->ind {return A$1}", 1)>] // SIG03: ->func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1: trivial}def func Test()->func()->ind {return A$1}", 1)>] // SIG03: ->func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} def func Test()->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A def func Test()->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {return A.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } def func Test()->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } def func Test()->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } def func Test()->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } def func Test()->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } def func Test()->func()->obj {return A.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } def func Test()->func()->obj {return A.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4", "def func A(z:obj)->ind def func Test()->func {return A}", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind def func Test()->func {dec x:obj; return A(x)}", 1)>] // SIG03: ->func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind def func Test()->func {dec x:ind; return A(x)}", 1)>] // SIG03: ->func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind def func Test()->func {dec x:ind; return A(x)}", 1)>] // SIG03: ->func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind def func Test()->func {return A}", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (axiom)
    [<DataRow("MS4f", "thm A {true} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (theorem)
    [<DataRow("MS4g", "lem A {true} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (lemma)
    [<DataRow("MS4h", "prop A {true} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (proposition)
    [<DataRow("MS4i", "conj A {true} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} def func Test()->func {return A$1}", 1)>] // SIG03: ->func does not matches signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1: trivial}def func Test()->func {return A$1}", 1)>] // SIG03: ->func does not match signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj def func Test()->func {return A}", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A def func Test()->func {return A}", 1)>] // SIG03: ->func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } def func Test()->func {return A.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } def func Test()->func {return A.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } def func Test()->func {return A.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } def func Test()->func {return A.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {return A.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func {return A.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } def func Test()->func {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } def func Test()->func {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } def func Test()->func {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } def func Test()->func {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->func {return A.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->func {return A.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } def func Test()->func {return A.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } def func Test()->func {return A.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } def func Test()->func()->obj {return A.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->func {return A.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } def func Test()->func {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->func {return A.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->func {return A.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } def func Test()->func {return A.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } def func Test()->func {return A.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } def func Test()->func()->obj {return A.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->func {return A.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } def func Test()->func {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 

    // ... other
    [<DataRow("01a", "def cl A def cl B:A def func T()->A {ret A}", 1)>]
    [<DataRow("01b", "def cl A def cl B:A def func T()->B {ret B}", 1)>]
    [<DataRow("02a", "def cl A def cl B:A ext U x@/\d+/ -> B {ret A}", 1)>]
    [<DataRow("02b", "def cl A def cl B:A ext U x@/\d+/ -> B {ret B}", 1)>]
    [<DataRow("03a", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("03b", "thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("03c", "prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("03d", "lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("03e", "conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("03g", "cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred) {true} def pred Test() {T(@1)}", 0)>]
    [<DataRow("04", """def cl Nat def func Zero() -> Nat def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y) } ext Digits x@/\d+/ -> Nat { ret mcases ( | (x = @0): Zero() ? Nat() ) }""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG03(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG03 ""
            
            runTestHelper "TestSIG03.fpl" fplCode code expected


    // match with simple types
    [<DataRow("ST0", "ext Test x@/\d+/->obj {dec a:obj; return a}", 0)>]
    [<DataRow("ST1", "ext Test x@/\d+/->ind {dec a:ind; return a}", 0)>]
    [<DataRow("ST1a", "ext Test x@/\d+/->ind {return $1}", 0)>]
    [<DataRow("ST2", "ext Test x@/\d+/->func {dec f:func; return f}", 0)>]
    [<DataRow("ST2a", "ext Test x@/\d+/->func {dec f:func()->ind; return f}", 0)>]
    [<DataRow("ST2b", "ext Test x@/\d+/->func {dec f:func(y:obj)->ind; return f}", 0)>]
    [<DataRow("ST2c", "ext Test x@/\d+/->func {dec f:func(y:obj)->func; return f}", 0)>]
    [<DataRow("ST2d", "ext Test x@/\d+/->func {dec f:func(y:obj)->func(z:pred)->pred; return f}", 0)>]
    [<DataRow("ST3", "ext Test x@/\d+/->pred {dec f:pred; return f}", 0)>]
    [<DataRow("ST3a", "ext Test x@/\d+/->pred {dec f:pred(); return f}", 0)>]
    [<DataRow("ST3b", "ext Test x@/\d+/->pred {dec f:pred; return f}", 0)>]
    [<DataRow("ST3c", "ext Test x@/\d+/->pred {dec f:pred(y:obj); return f}", 0)>]
    [<DataRow("ST3d", "ext Test x@/\d+/->pred {return true}", 0)>]
    [<DataRow("ST3e", "ext Test x@/\d+/->pred {return false}", 0)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj", "ext Test x@/\d+/->obj {dec f:obj; return f}", 0)>]
    [<DataRow("ST1_obj", "ext Test x@/\d+/->obj {dec f:ind; return f}", 1)>]
    [<DataRow("ST2_obj", "ext Test x@/\d+/->obj {dec f:func; return f}", 1)>]
    [<DataRow("ST2a_obj", "ext Test x@/\d+/->obj {dec f:func()->ind; return f}", 1)>]
    [<DataRow("ST2b_obj", "ext Test x@/\d+/->obj {dec f:func(y:obj)->ind; return f}", 1)>]
    [<DataRow("ST2c_obj", "ext Test x@/\d+/->obj {dec f:func(y:obj)->func; return f}", 1)>]
    [<DataRow("ST2d_obj", "ext Test x@/\d+/->obj {dec f:func(y:obj)->func(z:pred)->pred; return f}", 1)>]
    [<DataRow("ST3_obj", "ext Test x@/\d+/->obj {dec f:pred; return f}", 1)>]
    [<DataRow("ST3a_obj", "ext Test x@/\d+/->obj {dec f:pred(); return f}", 1)>]
    [<DataRow("ST3b_obj", "ext Test x@/\d+/->obj {dec f:pred; return f}", 1)>]
    [<DataRow("ST3c_obj", "ext Test x@/\d+/->obj {dec f:pred(y:obj); return f}", 1)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "ext Test x@/\d+/->ind {dec f:obj; return f}", 1)>]
    [<DataRow("ST1_ind", "ext Test x@/\d+/->ind {dec f:ind; return f}", 0)>]
    [<DataRow("ST2_ind", "ext Test x@/\d+/->ind {dec f:func; return f}", 1)>]
    [<DataRow("ST2a_ind", "ext Test x@/\d+/->ind {dec f:func()->ind; return f}", 1)>]
    [<DataRow("ST2b_ind", "ext Test x@/\d+/->ind {dec f:func(y:obj)->ind; return f}", 1)>]
    [<DataRow("ST2c_ind", "ext Test x@/\d+/->ind {dec f:func(y:obj)->func; return f}", 1)>]
    [<DataRow("ST2d_ind", "ext Test x@/\d+/->ind {dec f:func(y:obj)->func(z:pred)->pred; return f}", 1)>]
    [<DataRow("ST3_ind", "ext Test x@/\d+/->ind {dec f:pred; return f}", 1)>]
    [<DataRow("ST3a_ind", "ext Test x@/\d+/->ind {dec f:pred(); return f}", 1)>]
    [<DataRow("ST3b_ind", "ext Test x@/\d+/->ind {dec f:pred; return f}", 1)>]
    [<DataRow("ST3c_ind", "ext Test x@/\d+/->ind {dec f:pred(y:obj); return f}", 1)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "ext Test x@/\d+/->pred {dec f:obj; return f}", 1)>]
    [<DataRow("ST1_pred", "ext Test x@/\d+/->pred {dec f:ind; return f}", 1)>]
    [<DataRow("ST2_pred", "ext Test x@/\d+/->pred {dec f:func; return f}", 1)>]
    [<DataRow("ST2a_pred", "ext Test x@/\d+/->pred {dec f:func()->ind; return f}", 1)>]
    [<DataRow("ST2b_pred", "ext Test x@/\d+/->pred {dec f:func(y:obj)->pred; return f}", 1)>]
    [<DataRow("ST2c_pred", "ext Test x@/\d+/->pred {dec f:func(y:obj)->func; return f}", 1)>]
    [<DataRow("ST2d_pred", "ext Test x@/\d+/->pred {dec f:func(y:obj)->func(z:pred)->pred; return f}", 1)>]
    [<DataRow("ST3_pred", "ext Test x@/\d+/->pred {dec f:pred; return f}", 0)>]
    [<DataRow("ST3a_pred", "ext Test x@/\d+/->pred {dec f:pred(); return f}", 0)>]
    [<DataRow("ST3b_pred", "ext Test x@/\d+/->pred {dec f:pred; return f}", 0)>]
    [<DataRow("ST3c_pred", "ext Test x@/\d+/->pred {dec f:pred(y:obj); return f}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "ext Test x@/\d+/->func {dec f:obj; return f}", 1)>]
    [<DataRow("ST1_func", "ext Test x@/\d+/->func {dec f:ind; return f}", 1)>]
    [<DataRow("ST2_func", "ext Test x@/\d+/->func {dec f:func; return f}", 0)>]
    [<DataRow("ST2a_func", "ext Test x@/\d+/->func {dec f:func()->ind; return f}", 0)>]
    [<DataRow("ST2b_func", "ext Test x@/\d+/->func {dec f:func(y:obj)->pred; return f}", 0)>]
    [<DataRow("ST2c_func", "ext Test x@/\d+/->func {dec f:func(y:obj)->func; return f}", 0)>]
    [<DataRow("ST2d_func", "ext Test x@/\d+/->func {dec f:func(y:obj)->func(z:pred)->pred; return f}", 0)>]
    [<DataRow("ST3_func", "ext Test x@/\d+/->func {dec f:pred; return f}", 1)>]
    [<DataRow("ST3a_func", "ext Test x@/\d+/->func {dec f:pred(); return f}", 1)>]
    [<DataRow("ST3b_func", "ext Test x@/\d+/->func {dec f:pred; return f}", 1)>]
    [<DataRow("ST3c_func", "ext Test x@/\d+/->func {dec f:pred(y:obj); return f}", 1)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "ext Test x@/\d+/->pred() {dec a:obj; return a}", 1)>]
    [<DataRow("NP1", "ext Test x@/\d+/->pred() {dec a:ind; return a}", 1)>]
    [<DataRow("NP2", "ext Test x@/\d+/->pred() {dec a:func; return a}", 1)>]
    [<DataRow("NP2a", "ext Test x@/\d+/->pred() {dec a:func()->ind; return a}", 1)>]
    [<DataRow("NP2b", "ext Test x@/\d+/->pred() {dec a:func(y:obj)->ind; return a}", 1)>]
    [<DataRow("NP2c", "ext Test x@/\d+/->pred() {dec a:func(y:obj)->func; return a}", 1)>]
    [<DataRow("NP2d", "ext Test x@/\d+/->pred() {dec a:func(y:obj)->func(z:pred)->pred; return a}", 1)>]
    [<DataRow("NP3", "ext Test x@/\d+/->pred() {dec a:pred; return a}", 1)>]
    [<DataRow("NP3a", "ext Test x@/\d+/->pred() {dec a:pred(); return a}", 0)>]
    [<DataRow("NP3b", "ext Test x@/\d+/->pred() {dec a:pred; return a}", 1)>]
    [<DataRow("NP3c", "ext Test x@/\d+/->pred() {dec a:pred(y:obj); return a}", 1)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "ext Test x@/\d+/->pred(b:obj) {dec a:obj; return a}", 1)>]
    [<DataRow("NP_1", "ext Test x@/\d+/->pred(b:obj) {dec a:ind; return a}", 1)>]
    [<DataRow("NP_2", "ext Test x@/\d+/->pred(b:obj) {dec a:func; return a}", 1)>]
    [<DataRow("NP_2a", "ext Test x@/\d+/->pred(b:obj) {dec a:func()->ind; return a}", 1)>]
    [<DataRow("NP_2b", "ext Test x@/\d+/->pred(b:obj) {dec a:func(y:obj)->ind; return a}", 1)>]
    [<DataRow("NP_2c", "ext Test x@/\d+/->pred(b:obj) {dec a:func(y:obj)->func; return a}", 1)>]
    [<DataRow("NP_2d", "ext Test x@/\d+/->pred(b:obj) {dec a:func(y:obj)->func(z:pred)->pred; return a}", 1)>]
    [<DataRow("NP_3", "ext Test x@/\d+/->pred(b:obj) {dec a:pred; return a}", 1)>]
    [<DataRow("NP_3a", "ext Test x@/\d+/->pred(b:obj) {dec a:pred(); return a}", 1)>]
    [<DataRow("NP_3b", "ext Test x@/\d+/->pred(b:obj) {dec a:pred; return a}", 1)>]
    [<DataRow("NP_3c", "ext Test x@/\d+/->pred(b:obj) {dec a:pred(y:obj); return a}", 0)>]
    [<DataRow("NP_3c", "ext Test x@/\d+/->pred(b:obj) {dec a:pred(y:ind); return a}", 1)>]

    // (mis)match with func() types
    [<DataRow("NF0", "ext Test x@/\d+/->func()->ind {dec a:obj; return a}", 1)>]
    [<DataRow("NF1", "ext Test x@/\d+/->func()->ind {dec a:ind; return a}", 1)>]
    [<DataRow("NF2", "ext Test x@/\d+/->func()->ind {dec a:func; return a}", 1)>]
    [<DataRow("NF2a", "ext Test x@/\d+/->func()->ind {dec a:func()->ind; return a}", 0)>]
    [<DataRow("NF2b", "ext Test x@/\d+/->func()->ind {dec a:func(y:obj)->ind; return a}", 1)>]
    [<DataRow("NF2c", "ext Test x@/\d+/->func()->ind {dec a:func(y:obj)->obj; return a}", 1)>]
    [<DataRow("NF2d", "ext Test x@/\d+/->func()->ind {dec a:func(y:ind)->ind; return a}", 1)>]
    [<DataRow("NF2e", "ext Test x@/\d+/->func()->ind {dec a:func(y:obj)->func; return a}", 1)>]
    [<DataRow("NF2f", "ext Test x@/\d+/->func()->ind {dec a:func(y:obj)->func(z:pred)->pred; return a}", 1)>]
    [<DataRow("NF3", "ext Test x@/\d+/->func()->ind {dec a:pred; return a}", 1)>]
    [<DataRow("NF3a", "ext Test x@/\d+/->func()->ind {dec a:pred(); return a}", 1)>]
    [<DataRow("NF3b", "ext Test x@/\d+/->func()->ind {dec a:pred; return a}", 1)>]
    [<DataRow("NF3c", "ext Test x@/\d+/->func()->ind {dec a:pred(y:obj); return a}", 1)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "ext Test x@/\d+/->func(h:obj)->ind {dec a:obj; return a}", 1)>]
    [<DataRow("NF_1", "ext Test x@/\d+/->func(h:obj)->ind {dec a:ind; return a}", 1)>]
    [<DataRow("NF_2", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func; return a}", 1)>]
    [<DataRow("NF_2a", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func()->ind; return a}", 1)>]
    [<DataRow("NF_2b", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func(y:obj)->ind; return a}", 0)>]
    [<DataRow("NF_2c", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func(y:obj)->obj; return a}", 1)>]
    [<DataRow("NF_2d", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func(y:ind)->ind; return a}", 1)>]
    [<DataRow("NF_2e", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func(y:obj)->func; return a}", 1)>]
    [<DataRow("NF_2f", "ext Test x@/\d+/->func(h:obj)->ind {dec a:func(y:obj)->func(z:pred)->pred; return a}", 1)>]
    [<DataRow("NF_3", "ext Test x@/\d+/->func(h:obj)->ind {dec a:pred; return a}", 1)>]
    [<DataRow("NF_3a", "ext Test x@/\d+/->func(h:obj)->ind {dec a:pred(); return a}", 1)>]
    [<DataRow("NF_3b", "ext Test x@/\d+/->func(h:obj)->ind {dec a:pred; return a}", 1)>]
    [<DataRow("NF_3c", "ext Test x@/\d+/->func(h:obj)->ind {dec a:pred(y:obj); return a}", 1)>]
    [<DataRow("NF_3d", "ext Test x@/\d+/->func(h:obj)->ind {dec a:pred(y:ind); return a}", 1)>]

    // match with class type
    [<DataRow("CT1", "def cl A {intr} ext Test x@/\d+/->obj {dec a:A; return a}", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A {intr} ext Test x@/\d+/->A {dec a:A; return a}", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A {intr} def cl B:A {intr} ext Test x@/\d+/->A {dec a:B; return a}", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A {intr} def cl B:A {intr} ext Test x@/\d+/->B {dec a:B; return a}", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A {intr} def cl B:A {intr} ext Test x@/\d+/->obj {dec a:B; return a}", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A {intr} ext Test x@/\d+/->obj {dec a:A a:=A(); return a}", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A {intr} ext Test x@/\d+/->A {dec a:A a:=A(); return a}", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A {intr} def cl B:A {intr} ext Test x@/\d+/->A {dec a:B a:=B(); return a}", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A {intr} def cl B:A {intr} ext Test x@/\d+/->B {dec a:B a:=B(); return a}", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A {intr} def cl B:A {intr} ext Test x@/\d+/->obj {dec a:B a:=B(); return a}", 0)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_", "ext Test x@/\d+/->obj {dec a:A; return a}", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A ext Test x@/\d+/->A {dec a:obj; return a}", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A ext Test x@/\d+/->B {dec a:A; return a}", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "ext Test x@/\d+/->obj {ret A}", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A ext Test x@/\d+/->B {ret A}", 1)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A ext Test x@/\d+/->B {ret B}", 1)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A ext Test x@/\d+/->obj {ret A}", 1)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A ext Test x@/\d+/->B {ret B}", 1)>] // B is B, but x is class reference, error
    [<DataRow("CI6_", "def cl A ext Test x@/\d+/->A {ret A}", 1)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A ext Test x@/\d+/->A {ret B}", 1)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A ext Test x@/\d+/->B {ret B}", 1)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A ext Test x@/\d+/->obj {ret B}", 1)>] // B is obj but x is class reference, error

    // match with the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) ext Test x@/\d+/->pred(y:obj) {return A}", 0)>] // OK: ->pred(y:obj) matches signature A(obj), whole node would be returned
    [<DataRow("MS1a", "def pred A(z:obj) ext Test x@/\d+/->pred(y:obj) {dec x:obj; return A(x)}", 1)>] // SIG03: ->pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) ext Test x@/\d+/->pred(y:obj) {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) ext Test x@/\d+/->pred(y:obj) {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} ext Test x@/\d+/->pred(y:obj) {return A$1}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1: trivial}ext Test x@/\d+/->pred(y:obj) {return A$1}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec y:obj; return y} ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A ext Test x@/\d+/->pred(y:obj) {return A}", 1)>] // SIG03: ->pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {return A.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } ext Test x@/\d+/->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred(y:obj) {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {return A.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {return A.X}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred(y:obj) {return A.X}", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->pred(y:obj) {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_", "def pred A() ext Test x@/\d+/->pred() {return A}", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "def pred A() ext Test x@/\d+/->pred() {dec x:obj; return A(x)}", 1)>] // SIG03: ->pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() ext Test x@/\d+/->pred() {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() ext Test x@/\d+/->pred() {dec x:ind; return A(x)}", 1)>] // SIG03: ->pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} ext Test x@/\d+/->pred() {return A$1}", 1)>] // SIG03: ->pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1: trivial}ext Test x@/\d+/->pred() {return A$1}", 1)>] // SIG03: ->pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A ext Test x@/\d+/->pred() {return A}", 1)>] // SIG03: ->pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } ext Test x@/\d+/->pred() {return A.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } ext Test x@/\d+/->pred() {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred() {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } ext Test x@/\d+/->pred() {return A.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } ext Test x@/\d+/->pred() {return A.X}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred() {return A.X}", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->pred() {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2", "def pred A(z:obj) ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) ext Test x@/\d+/->pred {dec a:obj; return A(a)}", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) ext Test x@/\d+/->pred {dec a:ind; return A(a)}", 1)>] // SIG03: ->pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) ext Test x@/\d+/->pred {dec a:ind; return A(a)}", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} ext Test x@/\d+/->pred {return A}", 0)>] // OK: ->pred matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} ext Test x@/\d+/->pred {return A$1}", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1: trivial}ext Test x@/\d+/->pred {return A$1}", 0)>] // OK: ->pred matches signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} ext Test x@/\d+/->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj ext Test x@/\d+/->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec y:obj; return y} ext Test x@/\d+/->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A ext Test x@/\d+/->pred {return A}", 1)>] // SIG03: ->pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {dec a:obj; return A.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->pred {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } ext Test x@/\d+/->pred {dec o:A o:=A(); return o.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->pred {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {dec a:obj o:A o:=A(); return o.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {dec a:obj; return A.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } ext Test x@/\d+/->pred {return A.X}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext Test x@/\d+/->pred {return A.X}", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->pred {dec a:obj; return A.X(a)}", 0)>] // OK: ->pred matches by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind ext Test x@/\d+/->func(y:obj)->ind {return A}", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind ext Test x@/\d+/->func(y:obj)->ind {dec h:obj; return A(h)}", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind ext Test x@/\d+/->func(y:obj)->ind {dec h:ind; return A(h)}", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind ext Test x@/\d+/->func(y:obj)->ind {dec h:ind; return A(h)}", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} ext Test x@/\d+/->func(y:obj)->ind {return A$1}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1: trivial}ext Test x@/\d+/->func(y:obj)->ind {return A$1}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec y:obj; return y} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->func(y:obj)->obj {return A.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func(y:obj)->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } ext Test x@/\d+/->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } ext Test x@/\d+/->func(y:obj)->obj {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func(y:obj)->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func(y:obj)->ind {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } ext Test x@/\d+/->func(y:obj)->obj {return A.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func(y:obj)->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } ext Test x@/\d+/->func(y:obj)->obj {return A.X}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func(y:obj)->ind {return A.X}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func(y:obj)->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_", "def func A()->ind ext Test x@/\d+/->func()->ind {return A}", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind ext Test x@/\d+/->func()->ind {dec h:obj; return A(h)}", 1)>] // SIG03: ->func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind ext Test x@/\d+/->func()->ind {return A()}", 1)>] // SIG03: ->func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind ext Test x@/\d+/->func()->ind {dec h:ind; return A(h)}", 1)>] // SIG03: ->func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} ext Test x@/\d+/->func()->ind {return A$1}", 1)>] // SIG03: ->func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1: trivial}ext Test x@/\d+/->func()->ind {return A$1}", 1)>] // SIG03: ->func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} ext Test x@/\d+/->func(y:obj)->ind {return A}", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A ext Test x@/\d+/->func()->ind {return A}", 1)>] // SIG03: ->func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {return A.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func()->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } ext Test x@/\d+/->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func()->ind {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func()->ind {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {return A.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func()->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {return A.X}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func()->ind {return A.X}", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func()->ind {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4", "def func A(z:obj)->ind ext Test x@/\d+/->func {return A}", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind ext Test x@/\d+/->func {dec x:obj; return A(x)}", 1)>] // SIG03: ->func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind ext Test x@/\d+/->func {dec x:ind; return A(x)}", 1)>] // SIG03: ->func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind ext Test x@/\d+/->func {dec x:ind; return A(x)}", 1)>] // SIG03: ->func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind ext Test x@/\d+/->func {return A}", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (axiom)
    [<DataRow("MS4f", "thm A {true} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (theorem)
    [<DataRow("MS4g", "lem A {true} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (lemma)
    [<DataRow("MS4h", "prop A {true} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (proposition)
    [<DataRow("MS4i", "conj A {true} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} ext Test x@/\d+/->func {return A$1}", 1)>] // SIG03: ->func does not matches signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1: trivial}ext Test x@/\d+/->func {return A$1}", 1)>] // SIG03: ->func does not match signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj ext Test x@/\d+/->func {return A}", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec y:obj; return y} ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A ext Test x@/\d+/->func {return A}", 1)>] // SIG03: ->func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func {return A.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {return A.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func {return A.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } ext Test x@/\d+/->func {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } ext Test x@/\d+/->func {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } ext Test x@/\d+/->func {dec o:A o:=A(); return o.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func {dec o:A o:=A(); return o.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func {dec a:obj o:A o:=A(); return o.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->func {return A.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {return A.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func {return A.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } ext Test x@/\d+/->func {return A.X}", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->func {return A.X}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } ext Test x@/\d+/->func()->obj {return A.X}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext Test x@/\d+/->func {return A.X}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } ext Test x@/\d+/->func {dec a:obj; return A.X(a)}", 1)>] // SIG03: ->func does not match by value A.X(obj) 

    // ... other
    [<DataRow("00", "ext Digits x@/\d+/ -> Digits { ret x }", 0)>] 
    [<DataRow("00a", "ext Digits x@/\d+/ -> Digits { ret x } def pred T() { dec a:Digits a:=@0; true }", 0)>] 
    [<DataRow("01a", "def cl A ext Digits x@/\d+/ -> A { ret A() } def func T() -> A { ret @9 }", 0)>] 
    [<DataRow("01b", "ext Digits x@/\d+/ -> ind { ret $42 } def func T() -> ind { ret @9 }", 0)>] 
    [<DataRow("01c", "ext Digits x@/\d+/ -> pred { ret true } def func T() -> pred { ret @9 }", 0)>] 
    [<DataRow("01d", "def func A()->obj ext Digits x@/\d+/ -> func { ret A } def func T() -> func { ret @9 }", 0)>] 
    [<DataRow("01e", "def cl A ext Digits x@/\d+/ -> obj { ret A() } def func T() -> obj { ret @9 }", 0)>] 
    [<DataRow("02", "ext Digits x@/\d+/ -> Digits {ret x} def func Decr(x:Digits)->Digits { ret del.Decrement(x) }", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG03Extensions(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG03 ""
            
            runTestHelper "TestSIG03Extensions.fpl" fplCode code expected

    // match with simple types
    [<DataRow("ST0", "def func Test()->obj {return undef}", 0)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def func Test()->ind {return undef}", 0)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def func Test()->pred {return undef}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def func Test()->func {return undef}", 0)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def func Test()->pred() {return undef}", 0)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def func Test()->pred(a:obj) {return undef}", 0)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def func Test()->func()->ind {return undef}", 0)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "def func Test()->func(a:obj)->ind {return undef}", 0)>]

    // match with class type
    [<DataRow("CT2", "def cl A {intr} def func Test()->A {return undef}", 0)>] // undef is A, no error
    [<DataRow("CT4", "def cl A {intr} def cl B:A {intr} def func Test()->B {return undef}", 0)>] // undef is B, no error

    // match with the type func(...)->...
    [<DataRow("MS3", "def func Test()->func(y:obj)->ind {return undef}", 0)>] // OK: ->func(y:obj)->ind matches undef
    // ... using properties of classes
    [<DataRow("MS3n_", "def func Test()->func(y:obj)->ind {return undef}", 0)>] // OK: ->func(y:obj)->ind matches undef 
    [<DataRow("MS3p_5", "def func Test()->func()->obj {return undef}", 0)>] // OK: ->func()->ind matches undef

    [<TestMethod>]
    member this.TestSIG03Undef(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG03 ""
            
            runTestHelper "TestSIG03Undef.fpl" fplCode code expected
