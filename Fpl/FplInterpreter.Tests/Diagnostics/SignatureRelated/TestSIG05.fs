namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG05() =

    [<DataRow("inh", """def cl A def pred T() {dec n:A n:=A(); true}""", 0)>]
    [<DataRow("inh_a", """def cl A def pred T() {dec n:obj n:=A(); true}""", 0)>]
    [<DataRow("inh_b", """def cl A def cl B:A def pred T() {dec n:A n:=B(); true}""", 0)>]
    [<DataRow("inh_c", """def cl A def cl B:A def pred T() {dec n:B n:=A(); true}""", 1)>]
    [<DataRow("inh_d", """def cl A def cl B:A def pred T() {dec n:obj n:=B(); true}""", 0)>]
    [<DataRow("inh_e", """def cl A def cl B:A def pred T() {dec n:obj n:=A(); true}""", 0)>]
    [<DataRow("inh_f", """def cl A def cl B def pred T() {dec n:B n:=A(); true}""", 1)>]
    [<DataRow("inh_g", """def cl A def cl B def pred T() {dec n:A n:=B(); true}""", 1)>]
    [<DataRow("inh_type_a", """def cl A  def pred T() {dec n:ind n:=A(); true}""", 1)>]
    [<DataRow("inh_type_b", """def cl A def pred T() {dec n:pred n:=A(); true}""", 1)>]
    [<DataRow("inh_type_c", """def cl A def pred T() {dec n:func n:=A(); true}""", 1)>]
    [<DataRow("constr_a", """def cl A def cl B: A def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec dI2:D dI2:=D(); true}""", 0)>]
    [<DataRow("constr_b", """def cl A def cl B:A def cl D:B def pred T() {dec dI2:B dI2:=D(); true}""", 0)>]
    [<DataRow("constr_b1", """def cl A def cl B:A def cl D:B def pred T() {dec dI2:B dI2:=D(); true}""", 0)>]
    [<DataRow("constr_c", """def cl B { ctor B(x:obj) {dec y:obj x:=y; } } def pred T() {dec n,y:obj n:=B(y); true}""", 0)>]
    [<DataRow("constr_d", """def cl A def cl B: A def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec dI2:obj dI2:=D(); true}""", 0)>]
    [<DataRow("constr_inh_a", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec n:A n:=B(); true}""", 0)>] // SIG05 won't be issued due to proceedinng SIG04
    [<DataRow("constr_inh_b", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec n:obj n:=B(); true}""", 0)>] // SIG05 won't be issued due to proceedinng SIG04
    [<DataRow("constr_inh_c", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec n:obj n:=A(); true}""", 0)>] // SIG05 won't be issued due to proceedinng SIG04
    [<DataRow("constr_inh_d", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec n:B x:obj n:=A(x); true}""", 1)>]
    [<DataRow("constr_inh_e", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B { ctor B(x:pred) {dec base.Obj(); } } def pred T() {dec n:B x:obj n:=A(x); true}""", 1)>]
    [<DataRow("constr_inh_f", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B { ctor B(x:pred) {dec base.Obj(); } } def pred T() {dec n:A x:pred n:=B(x); true}""", 1)>]
    [<DataRow("ass_ind_ind", """def cl C1 {ctor C1(i1:ind) {dec o:ind o:=i1; }} """, 0)>]
    [<DataRow("ass_ind_pred1", """def cl A {dec myX:ind; ctor A(x:pred) {dec myX:=x;}}""", 1)>]
    [<DataRow("ass_ind_pred", """def cl C1 {ctor C1(i1:pred) {dec o:ind o:=i1; }}""", 1)>]
    [<DataRow("ass_ind_func", """def cl C1 {ctor C1(i1:func) {dec o:ind o:=i1; }}""", 1)>]
    [<DataRow("ass_ind_obj", """def cl C1 {ctor C1(i1:obj) {dec o:ind o:=i1; }} """, 1)>]
    [<DataRow("ass_pred_ind", """def cl C1 {ctor C1(i1:ind) {dec o:pred o:=i1; }} """, 1)>]
    [<DataRow("ass_pred_pred", """def cl C1 {ctor C1(i1:pred) {dec o:pred o:=i1; }}""", 0)>]
    [<DataRow("ass_pred_func", """def cl C1 {ctor C1(i1:func) {dec o:pred o:=i1; }}""", 1)>]
    [<DataRow("ass_pred_obj", """def cl C1 {ctor C1(i1:obj) {dec o:pred o:=i1; }}""", 1)>]
    [<DataRow("ass_func_ind", """def cl C1 {ctor C1(i1:ind) {dec o:func o:=i1; }} """, 1)>]
    [<DataRow("ass_func_pred", """def cl C1 {ctor C1(i1:pred) {dec o:func o:=i1; }}""", 1)>]
    [<DataRow("ass_func_func", """def cl C1 {ctor C1(i1:func) {dec o:func o:=i1; }}""", 0)>]
    [<DataRow("ass_func_obj", """def cl C1 {ctor C1(i1:obj) {dec o:func o:=i1; }}""", 1)>]
    [<DataRow("ass_obj_ind", """def cl C1 {ctor C1(i1:ind) {dec o:obj o:=i1; }} """, 1)>]
    [<DataRow("ass_obj_pred", """def cl C1 {ctor C1(i1:pred) {dec o:obj o:=i1; }}""", 1)>]
    [<DataRow("ass_obj_func", """def cl C1 {ctor C1(i1:func) {dec o:obj o:=i1; }}""", 1)>]
    [<DataRow("ass_obj_obj", """def cl C1 {ctor C1(i1:obj) {dec o:obj o:=i1; }}""", 0)>]

    // -----------------------------

    // match with simple types
    [<DataRow("ST0", "def pred T(v:obj) {dec x:obj v:=x; true}", 0)>]
    [<DataRow("ST1", "def pred T(v:ind) {dec x:ind v:=x; true}", 0)>]
    [<DataRow("ST1a", "def pred T(v:ind) {dec v:=$1; true}", 0)>]
    [<DataRow("ST2", "def pred T(v:func) {dec x:func v:=x; true}", 0)>]
    [<DataRow("ST2a", "def pred T(v:func) {dec x:func()->ind v:=x; true}", 0)>]
    [<DataRow("ST2b", "def pred T(v:func) {dec x:func(y:obj)->ind v:=x; true}", 0)>]
    [<DataRow("ST2c", "def pred T(v:func) {dec x:func(y:obj)->func v:=x; true}", 0)>]
    [<DataRow("ST2d", "def pred T(v:func) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 0)>]
    [<DataRow("ST3", "def pred T(v:pred) {dec x:pred v:=x; true}", 0)>]
    [<DataRow("ST3a", "def pred T(v:pred) {dec x:pred() v:=x; true}", 0)>]
    [<DataRow("ST3b", "def pred T(v:pred) {dec x:pred v:=x; true}", 0)>]
    [<DataRow("ST3c", "def pred T(v:pred) {dec x:pred(y:obj) v:=x; true}", 0)>]
    [<DataRow("ST3d", "def pred T(v:pred) {dec x:pred v:=true; true}", 0)>]
    [<DataRow("ST3e", "def pred T(v:pred) {dec x:pred v:=false; true}", 0)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj", "def pred T(v:obj) {dec x:obj v:=x; true}", 0)>]
    [<DataRow("ST1_obj", "def pred T(v:obj) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("ST2_obj", "def pred T(v:obj) {dec x:func v:=x; true}", 1)>]
    [<DataRow("ST2a_obj", "def pred T(v:obj) {dec x:func()->ind v:=x; true}", 1)>]
    [<DataRow("ST2b_obj", "def pred T(v:obj) {dec x:func(y:obj)->ind v:=x; true}", 1)>]
    [<DataRow("ST2c_obj", "def pred T(v:obj) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("ST2d_obj", "def pred T(v:obj) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("ST3_obj", "def pred T(v:obj) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("ST3a_obj", "def pred T(v:obj) {dec x:pred() v:=x; true}", 1)>]
    [<DataRow("ST3b_obj", "def pred T(v:obj) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("ST3c_obj", "def pred T(v:obj) {dec x:pred(y:obj) v:=x; true}", 1)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def pred T(v:ind) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("ST1_ind", "def pred T(v:ind) {dec x:ind v:=x; true}", 0)>]
    [<DataRow("ST2_ind", "def pred T(v:ind) {dec x:func v:=x; true}", 1)>]
    [<DataRow("ST2a_ind", "def pred T(v:ind) {dec x:func()->ind v:=x; true}", 1)>]
    [<DataRow("ST2b_ind", "def pred T(v:ind) {dec x:func(y:obj)->ind v:=x; true}", 1)>]
    [<DataRow("ST2c_ind", "def pred T(v:ind) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("ST2d_ind", "def pred T(v:ind) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("ST3_ind", "def pred T(v:ind) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("ST3a_ind", "def pred T(v:ind) {dec x:pred() v:=x; true}", 1)>]
    [<DataRow("ST3b_ind", "def pred T(v:ind) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("ST3c_ind", "def pred T(v:ind) {dec x:pred(y:obj) v:=x; true}", 1)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def pred T(v:pred) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("ST1_pred", "def pred T(v:pred) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("ST2_pred", "def pred T(v:pred) {dec x:func v:=x; true}", 1)>]
    [<DataRow("ST2a_pred", "def pred T(v:pred) {dec x:func()->ind v:=x; true}", 1)>]
    [<DataRow("ST2b_pred", "def pred T(v:pred) {dec x:func(y:obj)->pred v:=x; true}", 1)>]
    [<DataRow("ST2c_pred", "def pred T(v:pred) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("ST2d_pred", "def pred T(v:pred) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("ST3_pred", "def pred T(v:pred) {dec x:pred v:=x; true}", 0)>]
    [<DataRow("ST3a_pred", "def pred T(v:pred) {dec x:pred() v:=x; true}", 0)>]
    [<DataRow("ST3b_pred", "def pred T(v:pred) {dec x:pred v:=x; true}", 0)>]
    [<DataRow("ST3c_pred", "def pred T(v:pred) {dec x:pred(y:obj) v:=x; true}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def pred T(v:func) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("ST1_func", "def pred T(v:func) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("ST2_func", "def pred T(v:func) {dec x:func v:=x; true}", 0)>]
    [<DataRow("ST2a_func", "def pred T(v:func) {dec x:func()->ind v:=x; true}", 0)>]
    [<DataRow("ST2b_func", "def pred T(v:func) {dec x:func(y:obj)->pred v:=x; true}", 0)>]
    [<DataRow("ST2c_func", "def pred T(v:func) {dec x:func(y:obj)->func v:=x; true}", 0)>]
    [<DataRow("ST2d_func", "def pred T(v:func) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 0)>]
    [<DataRow("ST3_func", "def pred T(v:func) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("ST3a_func", "def pred T(v:func) {dec x:pred() v:=x; true}", 1)>]
    [<DataRow("ST3b_func", "def pred T(v:func) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("ST3c_func", "def pred T(v:func) {dec x:pred(y:obj) v:=x; true}", 1)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def pred T(v:pred()) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("NP1", "def pred T(v:pred()) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("NP2", "def pred T(v:pred()) {dec x:func v:=x; true}", 1)>]
    [<DataRow("NP2a", "def pred T(v:pred()) {dec x:func()->ind v:=x; true}", 1)>]
    [<DataRow("NP2b", "def pred T(v:pred()) {dec x:func(y:obj)->ind v:=x; true}", 1)>]
    [<DataRow("NP2c", "def pred T(v:pred()) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("NP2d", "def pred T(v:pred()) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("NP3", "def pred T(v:pred()) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NP3a", "def pred T(v:pred()) {dec x:pred() v:=x; true}", 0)>]
    [<DataRow("NP3b", "def pred T(v:pred()) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NP3c", "def pred T(v:pred()) {dec x:pred(y:obj) v:=x; true}", 1)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def pred T(v:pred(a:obj)) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("NP_1", "def pred T(v:pred(a:obj)) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("NP_2", "def pred T(v:pred(a:obj)) {dec x:func v:=x; true}", 1)>]
    [<DataRow("NP_2a", "def pred T(v:pred(a:obj)) {dec x:func()->ind v:=x; true}", 1)>]
    [<DataRow("NP_2b", "def pred T(v:pred(a:obj)) {dec x:func(y:obj)->ind v:=x; true}", 1)>]
    [<DataRow("NP_2c", "def pred T(v:pred(a:obj)) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("NP_2d", "def pred T(v:pred(a:obj)) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("NP_3", "def pred T(v:pred(a:obj)) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NP_3a", "def pred T(v:pred(a:obj)) {dec x:pred() v:=x; true}", 1)>]
    [<DataRow("NP_3b", "def pred T(v:pred(a:obj)) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NP_3c", "def pred T(v:pred(a:obj)) {dec x:pred(y:obj) v:=x; true}", 0)>]
    [<DataRow("NP_3c", "def pred T(v:pred(a:obj)) {dec x:pred(y:ind) v:=x; true}", 1)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def pred T(v:func()->ind) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("NF1", "def pred T(v:func()->ind) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("NF2", "def pred T(v:func()->ind) {dec x:func v:=x; true}", 1)>]
    [<DataRow("NF2a", "def pred T(v:func()->ind) {dec x:func()->ind v:=x; true}", 0)>]
    [<DataRow("NF2b", "def pred T(v:func()->ind) {dec x:func(y:obj)->ind v:=x; true}", 1)>]
    [<DataRow("NF2c", "def pred T(v:func()->ind) {dec x:func(y:obj)->obj v:=x; true}", 1)>]
    [<DataRow("NF2d", "def pred T(v:func()->ind) {dec x:func(y:ind)->ind v:=x; true}", 1)>]
    [<DataRow("NF2e", "def pred T(v:func()->ind) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("NF2f", "def pred T(v:func()->ind) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("NF3", "def pred T(v:func()->ind) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NF3a", "def pred T(v:func()->ind) {dec x:pred() v:=x; true}", 1)>]
    [<DataRow("NF3b", "def pred T(v:func()->ind) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NF3c", "def pred T(v:func()->ind) {dec x:pred(y:obj) v:=x; true}", 1)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "def pred T(v:func(a:obj)->ind) {dec x:obj v:=x; true}", 1)>]
    [<DataRow("NF_1", "def pred T(v:func(a:obj)->ind) {dec x:ind v:=x; true}", 1)>]
    [<DataRow("NF_2", "def pred T(v:func(a:obj)->ind) {dec x:func v:=x; true}", 1)>]
    [<DataRow("NF_2a", "def pred T(v:func(a:obj)->ind) {dec x:func()->ind v:=x; true}", 1)>]
    [<DataRow("NF_2b", "def pred T(v:func(a:obj)->ind) {dec x:func(y:obj)->ind v:=x; true}", 0)>]
    [<DataRow("NF_2c", "def pred T(v:func(a:obj)->ind) {dec x:func(y:obj)->obj v:=x; true}", 1)>]
    [<DataRow("NF_2d", "def pred T(v:func(a:obj)->ind) {dec x:func(y:ind)->ind v:=x; true}", 1)>]
    [<DataRow("NF_2e", "def pred T(v:func(a:obj)->ind) {dec x:func(y:obj)->func v:=x; true}", 1)>]
    [<DataRow("NF_2f", "def pred T(v:func(a:obj)->ind) {dec x:func(y:obj)->func(z:pred)->pred v:=x; true}", 1)>]
    [<DataRow("NF_3", "def pred T(v:func(a:obj)->ind) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NF_3a", "def pred T(v:func(a:obj)->ind) {dec x:pred() v:=x; true}", 1)>]
    [<DataRow("NF_3b", "def pred T(v:func(a:obj)->ind) {dec x:pred v:=x; true}", 1)>]
    [<DataRow("NF_3c", "def pred T(v:func(a:obj)->ind) {dec x:pred(y:obj) v:=x; true}", 1)>]
    [<DataRow("NF_3d", "def pred T(v:func(a:obj)->ind) {dec x:pred(y:ind) v:=x; true}", 1)>]

    // match with class type
    [<DataRow("CT1", "def cl A def pred T(v:obj) {dec x:A v:=x; true}", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A def pred T(v:A) {dec x:A v:=x; true}", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A def cl B:A def pred T(v:A) {dec x:B v:=x; true}", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A def cl B:A def pred T(v:B) {dec x:B v:=x; true}", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A def cl B:A def pred T(v:obj) {dec x:B v:=x; true}", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A def pred T(v:obj) {dec x:A x:=A() v:=x; true}", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A def pred T(v:A) {dec x:A x:=A() v:=x; true}", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A def cl B:A def pred T(v:A) {dec x:B x:=B() v:=x; true}", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A def cl B:A def pred T(v:B) {dec x:B x:=B() v:=x; true}", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A def cl B:A def pred T(v:obj) {dec x:B x:=B() v:=x; true}", 0)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_", "def pred T(v:obj) {dec x:A v:=x; true}", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A def pred T(v:A) {dec x:obj v:=x; true}", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A def pred T(v:B) {dec a:A v:=a; true}", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "def pred T(v:obj) {dec x:A x:=A v:=x; true}", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A def pred T(v:B) {dec a:A a:=A v:=a; true}", 2)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A def pred T(v:B) {dec a:B a:=B v:=a; true}", 2)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A def pred T(v:obj) {dec x:A x:=A v:=x; true}", 2)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A def pred T(v:B) {dec x:B x:=B v:=x; true}", 2)>] // B is B, but x is class reference, error
    [<DataRow("CI6_", "def cl A def pred T(v:A) {dec x:A x:=A v:=x; true}", 2)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A def pred T(v:A) {dec x:B x:=B v:=x; true}", 2)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A def pred T(v:B) {dec x:B x:=B v:=x; true}", 2)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A def pred T(v:obj) {dec x:B x:=B v:=x; true}", 2)>] // B is obj but x is class reference, error

    // match with the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) def pred T(v:pred(y:obj)) {dec v:=A; true}", 0)>] // OK: ->pred(y:obj) matches signature A(obj), whole node would be returned
    [<DataRow("MS1a", "def pred A(z:obj) def pred T(v:pred(y:obj)) {dec x:obj v:=A(x); true}", 1)>] // SIG05: pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) def pred T(v:pred(y:obj)) {dec x:ind v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) def pred T(v:pred(y:obj)) {dec x:ind v:=A(x); true}", 1)>] // SIG05: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} def pred T(v:pred(y:obj)) {dec v:=A$1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1: trivial}def pred T(v:pred(y:obj)) {dec v:=A$1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A def pred T(v:pred(y:obj)) {dec v:=A; true}", 1)>] // SIG05: pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } def pred T(v:pred(y:obj)) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } def pred T(v:pred(y:obj)) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } def pred T(v:pred(y:obj)) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:pred(y:obj)) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:pred(y:obj)) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec a:obj o:A o:=A() v:=o.X(a); true}", 1)>] // SIG05: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred T(v:pred(y:obj)) {dec v:=A.X; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:pred(y:obj)) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_", "def pred A() def pred T(v:pred()) {dec v:=A; true}", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "def pred A() def pred T(v:pred()) {dec x:obj v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() def pred T(v:pred()) {dec x:ind v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() def pred T(v:pred()) {dec x:ind v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} def pred T(v:pred()) {dec v:=A$1; true}", 1)>] // SIG05: pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1: trivial}def pred T(v:pred()) {dec v:=A$1; true}", 1)>] // SIG05: pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A def pred T(v:pred()) {dec v:=A; true}", 1)>] // SIG05: pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } def pred T(v:pred()) {dec v:=A.X; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred()) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred()) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } def pred T(v:pred()) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } def pred T(v:pred()) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } def pred T(v:pred()) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:pred()) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:pred()) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred()) {dec a:obj o:A o:=A() v:=o.X(a); true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } def pred T(v:pred()) {dec v:=A.X; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } def pred T(v:pred()) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } def pred T(v:pred()) {dec v:=A.X; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred T(v:pred()) {dec v:=A.X; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:pred()) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2", "def pred A(z:obj) def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) def pred T(v:pred) {dec x:obj v:=A(x); true}", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) def pred T(v:pred) {dec x:ind v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) def pred T(v:pred) {dec x:ind v:=A(x); true}", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} def pred T(v:pred) {dec v:=A; true}", 0)>] // OK: ->pred matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} def pred T(v:pred) {dec v:=A$1; true}", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1: trivial}def pred T(v:pred) {dec v:=A$1; true}", 0)>] // OK: pred matches signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} def pred T(v:pred) {dec v:=A; true}", 1)>] // SIG05: pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj def pred T(v:pred) {dec v:=A; true}", 1)>] // SIG05: pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred T(v:pred) {dec v:=A; true}", 1)>] // SIG05: pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A def pred T(v:pred) {dec v:=A; true}", 1)>] // SIG05: pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred) {dec a:obj v:=A.X(a); true}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } def pred T(v:pred) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } def pred T(v:pred) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } def pred T(v:pred) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:pred) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:pred) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } def pred T(v:pred) {dec a:obj o:A o:=A() v:=o.X(a); true}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } def pred T(v:pred) {dec a:obj v:=A.X(a); true}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } def pred T(v:pred) {dec v:=A.X; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred T(v:pred) {dec v:=A.X; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:pred) {dec a:obj v:=A.X(a); true}", 0)>] // OK: pred matches by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind def pred T(v:func(y:obj)->ind) {dec x:obj v:=A(x); true}", 1)>] // SIG05: func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind def pred T(v:func(y:obj)->ind) {dec x:obj v:=A(x); true}", 1)>] // SIG05: func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind def pred T(v:func(y:obj)->ind) {dec x:obj v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} def pred T(v:func(y:obj)->ind) {dec v:=A$1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1: trivial}def pred T(v:func(y:obj)->ind) {dec v:=A$1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:func(y:obj)->obj) {dec v:=A.X; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } def pred T(v:func(y:obj)->ind) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } def pred T(v:func(y:obj)->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } def pred T(v:func(y:obj)->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } def pred T(v:func(y:obj)->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } def pred T(v:func(y:obj)->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } def pred T(v:func(y:obj)->obj) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:func(y:obj)->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } def pred T(v:func(y:obj)->ind) {dec a:obj o:A o:=A() v:=o.X(a); true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } def pred T(v:func(y:obj)->obj) {dec v:=A.X; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } def pred T(v:func(y:obj)->ind) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred T(v:func(y:obj)->obj) {dec v:=A.X; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred T(v:func(y:obj)->ind) {dec v:=A.X; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred T(v:func(y:obj)->ind) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_", "def func A()->ind def pred T(v:func()->ind) {dec v:=A; true}", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind def pred T(v:func()->ind) {dec x:obj v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind def pred T(v:func()->ind) {dec v:=A(); true}", 1)>] // SIG05: func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind def pred T(v:func()->ind) {dec x:ind v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} def pred T(v:func()->ind) {dec v:=A$1; true}", 1)>] // SIG05: func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1: trivial}def pred T(v:func()->ind) {dec v:=A$1; true}", 1)>] // SIG05: func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred T(v:func(y:obj)->ind) {dec v:=A; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A def pred T(v:func()->ind) {dec v:=A; true}", 1)>] // SIG05: func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } def pred T(v:func()->obj) {dec v:=A.X; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } def pred T(v:func()->ind) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } def pred T(v:func()->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } def pred T(v:func()->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } def pred T(v:func()->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } def pred T(v:func()->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } def pred T(v:func()->obj) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:func()->ind) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } def pred T(v:func()->ind) {dec a:obj o:A o:=A() v:=o.X(a); true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } def pred T(v:func()->obj) {dec v:=A.X; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } def pred T(v:func()->ind) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } def pred T(v:func()->obj) {dec v:=A.X; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred T(v:func()->ind) {dec v:=A.X; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred T(v:func()->ind) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4", "def func A(z:obj)->ind def pred T(v:func) {dec v:=A; true}", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind def pred T(v:func) {dec x:obj v:=A(x); true}", 1)>] // SIG05: func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind def pred T(v:func) {dec x:obj v:=A(x); true}", 1)>] // SIG05: func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind def pred T(v:func) {dec x:obj v:=A(x); true}", 0)>] // SIG05 won't be issued due to proceedinng SIG04: func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind def pred T(v:func) {dec v:=A; true}", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (axiom)
    [<DataRow("MS4f", "thm A {true} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (theorem)
    [<DataRow("MS4g", "lem A {true} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (lemma)
    [<DataRow("MS4h", "prop A {true} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (proposition)
    [<DataRow("MS4i", "conj A {true} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} def pred T(v:func) {dec v:=A$1; true}", 1)>] // SIG05: func does not match signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1: trivial}def pred T(v:func) {dec v:=A$1; true}", 1)>] // SIG05: func does not match signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj def pred T(v:func) {dec v:=A; true}", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec y:obj; return y} def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A def pred T(v:func) {dec v:=A; true}", 1)>] // SIG05: func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } def pred T(v:func) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } def pred T(v:func()->obj) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:func) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } def pred T(v:func) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } def pred T(v:func) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } def pred T(v:func) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } def pred T(v:func) {dec o:A o:=A() v:=o.X; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } def pred T(v:func) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } def pred T(v:func()->obj) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } def pred T(v:func) {dec o:A o:=A() v:=o.X; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } def pred T(v:func) {dec a:obj o:A o:=A() v:=o.X(a); true}", 1)>] // SIG05: func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } def pred T(v:func) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } def pred T(v:func()->obj) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } def pred T(v:func) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } def pred T(v:func) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } def pred T(v:func) {dec v:=A.X; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } def pred T(v:func) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } def pred T(v:func()->obj) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred T(v:func) {dec v:=A.X; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred T(v:func) {dec a:obj v:=A.X(a); true}", 1)>] // SIG05: func does not match by value A.X(obj) 

    [<DataRow("24a", "def cl A {dec myX:obj; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec v:B v:=B(@2); false}", 0)>]    
    [<DataRow("24b", "def cl A {dec myX:ind; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec v:B v:=B(@2); false}", 1)>]    
    [<DataRow("25", "def cl Nat def func Succ(x:Nat)->Nat def cl A {dec myX:Nat; ctor A(i:Nat) {dec myX:=Succ(i);}}", 0)>]    
    [<DataRow("26", "def cl Nat def cl A {dec arr:*Nat[Nat]; ctor A(i:Nat) {dec arr[i]:=i;}}", 0)>]    
    [<DataRow("27", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def pred T() {dec n:Nat n:=Succ(Zero()); true}", 0)>]    
    [<DataRow("28", "def cl Nat def func Succ(n:Nat)->Nat ext Digits x@/\d+/ -> Nat {dec n:Nat n:=Succ(self(delegate.Decrement(x))); return n}", 0)>]      
    [<DataRow("29", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def func Add(x,y: Nat)->Nat {dec r:Nat r := Succ(self(x,y)); return r }", 0)>]    
    [<DataRow("29a", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def func Add(n,m: Nat)->Nat {return Succ(self(n,m))}", 0)>]    
    [<DataRow("30", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def pred T() {dec r:Nat r:= undef; true }", 0)>]    
    [<DataRow("31", "def cl Nat def cl Tuple {ctor Tuple(l:*tpl[Nat]) {} } def pred T(x,y:Nat) {dec tuple:Tuple tuple:=Tuple(x,y); tuple}", 0)>]    
    [<DataRow("32", "def cl Nat def cl C {dec myLength: Nat; ctor C(x:Nat) {dec myLength:=x;} property func Length() -> Nat {return myLength} } def pred T() {dec l:Nat c:C c:=C(l) l:=c.Length(); l}", 0)>]    
    [<DataRow("32", "def pred T(x:ind) {dec v:*ind[ind] v[$1]:=x; true}", 0)>]    
    [<DataRow("32a", "def pred T(x:obj) {dec v:*ind[ind] v[$1]:=x; true}", 1)>]    
    [<DataRow("33a", "def cl A def cl B:A def pred T(v:B) {dec v:=A; true}", 1)>] // A is A but references to a class, error
    [<DataRow("33b", "def cl A def cl B:A def pred T(v:B) {dec v:=B; true}", 1)>] // B is B, but a class reference, error
    [<DataRow("34", "def cl A def func S()->A def func T()->A {dec x:A x:=S(); return x}", 0)>] // mapping matching class
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]

    member this.TestSIG05(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG05 ""
            
            runTestHelper "TestSIG05.fpl" fplCode code expected


    // ... matching based on open formulas
    [<DataRow("is_01", "def pred T(v:pred(a:obj)) {dec y:obj v:=is(y,obj); true}", 0)>] 
    [<DataRow("is_01a", "def pred T(v:pred()) {dec y:obj v:=is(y,obj); true}", 1)>] 
    [<DataRow("ex_01", "def pred T(v:pred()) {dec v:=ex x:obj { is(x,obj) } ; true}", 0)>] 
    [<DataRow("ex_01a", "def pred T(v:pred(a:obj)) {dec v:=ex x:obj { is(x,obj) } ; true}", 1)>] 
    [<DataRow("ex_02", "def pred T(v:pred()) {dec y:obj v:=ex x:obj { and(is(y,obj), is(x,obj)) } ; true}", 1)>] 
    [<DataRow("ex_02a", "def pred T(v:pred(a:obj)) {dec y:obj v:=ex x:obj { or(is(y,obj), is(x,obj)) } ; true}", 0)>] 
    [<DataRow("ex_03", "def pred T(v:pred(a:obj)) {dec y:obj v:=ex x:obj { and(is(y,obj), is(x,obj)) } ; true}", 0)>] 
    [<DataRow("ex_03a", "def pred T(v:pred()) {dec v:=ex x:obj { and(is(x,obj), is(x,obj)) } ; true}", 0)>] 
    [<DataRow("ex_04", "def pred T(v:pred(a:obj)) {dec y:obj v:=ex x:obj { and(is(y,obj), is(x,obj)) } ; true}", 0)>] 
    [<DataRow("ex_04a", "def pred T(v:pred()) {dec y:obj v:=ex x:obj { and(is(y,obj), is(x,obj)) } ; true}", 1)>] 
    [<DataRow("exn_01", "def pred T(v:pred(a:obj)) {dec y:obj v:=exn$1 x:obj { impl(is(y,obj), (x = y)) }; true}", 0)>] 
    [<DataRow("exn_01a", "def pred T(v:pred()) {dec v:=exn$1 x:obj { impl(is(x,obj), (x = x)) }; true}", 0)>] 
    [<DataRow("all_06", "def pred T(v:pred()) {dec v:=all x:obj { not true } ; true}", 0)>] 
    [<DataRow("all_07", "def pred T(v:pred(a:obj)) {dec y:obj v:=all x:obj { not (x = @1) } ; true}", 1)>] 
    [<DataRow("all_07a", "def pred T(v:pred(a:obj)) {dec y:obj v:=all x:obj { not (x = @1) } ; true}", 1)>] 
    [<DataRow("and_01", "def pred T(v:pred(a:obj)) {dec x,y:obj v:=and(is(y,obj), is(x,obj)) ; true}", 1)>] 
    [<DataRow("and_01a", "def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=and(is(y,obj), is(x,obj)) ; true}", 0)>] 
    [<DataRow("and_01b", "def pred T(v:pred(a:obj)) {dec y:obj v:=and(is(y,obj), ex x:obj { is(x,obj) }) ; true}", 0)>] 
    [<DataRow("or_01", "def pred T(v:pred(a:obj)) {dec x,y:obj v:=or(is(y,obj), is(x,obj)) ; true}", 1)>] 
    [<DataRow("or_01a", "def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=or(is(y,obj), is(x,obj)) ; true}", 0)>] 
    [<DataRow("or_01b", "def pred T(v:pred(a:obj)) {dec y:obj v:=or(is(y,obj), ex x:obj { is(x,obj) }) ; true}", 0)>] 
    [<DataRow("xor_01", "def pred T(v:pred(a:obj)) {dec x,y:obj v:=xor(is(y,obj), is(x,obj)) ; true}", 1)>] 
    [<DataRow("xor_01a", "def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=xor(is(y,obj), is(x,obj)) ; true}", 0)>] 
    [<DataRow("xor_01b", "def pred T(v:pred(a:obj)) {dec y:obj v:=xor(is(y,obj), ex x:obj { is(x,obj) }) ; true}", 0)>] 
    [<DataRow("impl_01", "def pred T(v:pred(a:obj)) {dec x,y:obj v:=impl(is(y,obj), is(x,obj)) ; true}", 1)>] 
    [<DataRow("impl_01a", "def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=impl(is(y,obj), is(x,obj)) ; true}", 0)>] 
    [<DataRow("impl_01b", "def pred T(v:pred(a:obj)) {dec y:obj v:=impl(is(y,obj), ex x:obj { is(x,obj) }) ; true}", 0)>] 
    [<DataRow("iif_01", "def pred T(v:pred(a:obj)) {dec x,y:obj v:=iif(is(y,obj), is(x,obj)) ; true}", 1)>] 
    [<DataRow("iif_01a", "def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=iif(is(y,obj), is(x,obj)) ; true}", 0)>] 
    [<DataRow("iif_01b", "def pred T(v:pred(a:obj)) {dec y:obj v:=iif(is(y,obj), ex x:obj { is(x,obj) }) ; true}", 0)>] 
    [<DataRow("not_01", "def pred T(v:pred(a:obj)) {dec x,y:obj v:=not iif(is(y,obj), is(x,obj)) ; true}", 1)>] 
    [<DataRow("not_01a", "def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=not iif(is(y,obj), is(x,obj)) ; true}", 0)>] 
    [<DataRow("not_01b", "def pred T(v:pred(a:obj)) {dec y:obj v:=not iif(is(y,obj), ex x:obj { is(x,obj) }) ; true}", 0)>] 
    [<DataRow("=01", """def pred Equal(x,y: obj) infix "=" 50 { del.Equal(x,y)} def pred T(v:pred(a:obj)) {dec x,y:obj v:=(x = y) ; true}""", 1)>] 
    [<DataRow("=02", """def pred Equal(x,y: obj) infix "=" 50 { del.Equal(x,y)} def pred T(v:pred(a,b:obj)) {dec x,y:obj v:=(x = y) ; true}""", 0)>] 
    [<DataRow("=03", """def pred Equal(x,y: obj) infix "=" 50 { del.Equal(x,y)} def pred T(v:pred(a,b:obj)) {dec y:obj v:=(x = y) ; true}""", 0)>] 
    [<TestMethod>]
    member this.TestSIG05OpenFormulas(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG05 ""
            
            runTestHelper "TestSIG05OpenFormulas.fpl" fplCode code expected


    // -----------------------------

    // match with simple types
    [<DataRow("ST0", "ext U x@/\d+/ -> obj { dec a:obj; ret a } def pred T(v:obj) {dec v:=@1; true}", 0)>]
    [<DataRow("ST1", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:ind) {dec v:=@1; true}", 0)>]
    [<DataRow("ST1a", "ext U x@/\d+/ -> ind {ret $1} def pred T(v:ind) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2a", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2b", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2c", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2d", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3a", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3b", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3c", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3d", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3e", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:obj) {dec v:=@1; true}", 0)>]
    [<DataRow("ST1_obj", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2_obj", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2a_obj", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2b_obj", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2c_obj", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2d_obj", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3_obj", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3a_obj", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3b_obj", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3c_obj", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST1_ind", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:ind) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2_ind", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2a_ind", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2b_ind", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2c_ind", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2d_ind", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3_ind", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3a_ind", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3b_ind", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3c_ind", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:ind) {dec v:=@1; true}", 1)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST1_pred", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2_pred", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2a_pred", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2b_pred", "ext U x@/\d+/ -> func(y:obj)->pred {dec a:func(y:obj)->pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2c_pred", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2d_pred", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3_pred", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3a_pred", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3b_pred", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3c_pred", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred) {dec v:=@1; true}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:func) {dec v:=@1; true}", 1)>]
    [<DataRow("ST1_func", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:func) {dec v:=@1; true}", 1)>]
    [<DataRow("ST2_func", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2a_func", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2b_func", "ext U x@/\d+/ -> func(y:obj)->pred {dec a:func(y:obj)->pred; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2c_func", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST2d_func", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func) {dec v:=@1; true}", 0)>]
    [<DataRow("ST3_func", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3a_func", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:func) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3b_func", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func) {dec v:=@1; true}", 1)>]
    [<DataRow("ST3c_func", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:func) {dec v:=@1; true}", 1)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP1", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP2", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP2a", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP2b", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP2c", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP2d", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP3", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP3a", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred()) {dec v:=@1; true}", 0)>]
    [<DataRow("NP3b", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    [<DataRow("NP3c", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_1", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_2", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_2a", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_2b", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_2c", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_2d", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_3", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_3a", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_3b", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]
    [<DataRow("NP_3c", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 0)>]
    [<DataRow("NP_3c", "ext U x@/\d+/ -> pred(y:ind) {dec a:pred(y:ind); ret a} def pred T(v:pred(a:obj)) {dec v:=@1; true}", 1)>]

    // (mis)match with func() types
    [<DataRow("NF0", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF1", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF2", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF2a", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 0)>]
    [<DataRow("NF2b", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF2c", "ext U x@/\d+/ -> func(y:obj)->obj {dec a:func(y:obj)->obj; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF2d", "ext U x@/\d+/ -> func(y:ind)->ind {dec a:func(y:ind)->ind; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF2e", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF2f", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF3", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF3a", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF3b", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF3c", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_1", "ext U x@/\d+/ -> ind {dec a:ind; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_2", "ext U x@/\d+/ -> func {dec a:func; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_2a", "ext U x@/\d+/ -> func()->ind {dec a:func()->ind; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_2b", "ext U x@/\d+/ -> func(y:obj)->ind {dec a:func(y:obj)->ind; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 0)>]
    [<DataRow("NF_2c", "ext U x@/\d+/ -> func(y:obj)->obj {dec a:func(y:obj)->obj; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_2d", "ext U x@/\d+/ -> func(y:ind)->ind {dec a:func(y:ind)->ind; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_2e", "ext U x@/\d+/ -> func(y:obj)->func {dec a:func(y:obj)->func; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_2f", "ext U x@/\d+/ -> func(y:obj)->func(z:pred)->pred {dec a:func(y:obj)->func(z:pred)->pred; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_3", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_3a", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_3b", "ext U x@/\d+/ -> pred {dec a:pred; ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_3c", "ext U x@/\d+/ -> pred(y:obj) {dec a:pred(y:obj); ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]
    [<DataRow("NF_3d", "ext U x@/\d+/ -> pred(y:ind) {dec a:pred(y:ind); ret a} def pred T(v:func(a:obj)->ind) {dec v:=@1; true}", 1)>]

    // match with class type
    [<DataRow("CT1", "def cl A ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:obj) {dec v:=@1; true}", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:A) {dec v:=@1; true}", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B; ret a} def pred T(v:A) {dec v:=@1; true}", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B; ret a} def pred T(v:B) {dec v:=@1; true}", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B; ret a} def pred T(v:obj) {dec v:=@1; true}", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A ext U x@/\d+/ -> A {dec a:A a:=A(); ret a} def pred T(v:obj) {dec v:=@1; true}", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A ext U x@/\d+/ -> A {dec a:A a:=A(); ret a} def pred T(v:A) {dec v:=@1; true}", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B a:=B(); ret a} def pred T(v:A) {dec v:=@1; true}", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B a:=B(); ret a} def pred T(v:B) {dec v:=@1; true}", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A def cl B:A ext U x@/\d+/ -> B {dec a:B a:=B(); ret a} def pred T(v:obj) {dec v:=@1; true}", 0)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_", "ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:obj) {dec v:=@1; true}", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A ext U x@/\d+/ -> obj {dec a:obj; ret a} def pred T(v:A) {dec v:=@1; true}", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A ext U x@/\d+/ -> A {dec a:A; ret a} def pred T(v:B) {dec v:=@1; true}", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "ext U x@/\d+/ -> A {ret A} def pred T(v:obj) {dec v:=@1; true}", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A ext U x@/\d+/ -> A {ret A} def pred T(v:B) {dec v:=@1; true}", 1)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A ext U x@/\d+/ -> B {ret B} def pred T(v:B) {dec v:=@1; true}", 0)>] // B() is B
    [<DataRow("CI4_", "def cl A ext U x@/\d+/ -> A {ret A} def pred T(v:obj) {dec v:=@1; true}", 0)>] // A() is obj
    [<DataRow("CI5_", "def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:B) {dec v:=@1; true}", 0)>] // B() is B
    [<DataRow("CI6_", "def cl A ext U x@/\d+/ -> A {ret A()} def pred T(v:A) {dec v:=@1; true}", 0)>] // A() is A
    [<DataRow("CI7_", "def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:A) {dec v:=@1; true}", 0)>] // B() is A 
    [<DataRow("CI8_", "def cl A def cl B:A ext U x@/\d+/ -> B {ret B()} def pred T(v:B) {dec v:=@1; true}", 0)>] // B() ia B
    [<DataRow("CI9_", "def cl A def cl B:A ext U x@/\d+/ -> B {ret B} def pred T(v:obj) {dec v:=@1; true}", 0)>] // B() is obj 

    // match with the type pred(...) 
    [<DataRow("MS1",  "ext U x@/\d+/ -> pred(z:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK: ->pred(y:obj) matches signature pred(obj)
    [<DataRow("MS1a", "ext U x@/\d+/ -> pred(z:obj) {dec a:pred(y:obj); ret a} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK: pred(y:obj) does matches signature pred(obj) 
    [<DataRow("MS1b", "ext U x@/\d+/ -> pred(z:obj) {dec a:pred(y:ind); ret a} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:ind) does not match pred(obj)
    [<DataRow("MS1c", "ext U x@/\d+/ -> pred(z:ind) {dec a:pred(y:ind); ret a} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "ext U x@/\d+/ -> pred(z:ind) {dec a:pred(y:ind); ret a} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} ext U x@/\d+/ -> pred(z:obj) {ret A$1} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred(z:obj) {ret A$1} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext U x@/\d+/ -> pred(z:obj) {dec u:pred(a:obj); ret u} def pred T(v:pred(y:ind)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature pred(y:ind)
    [<DataRow("MS1o", "def cl A ext U x@/\d+/ -> pred(z:obj) {ret A} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec g:obj; ret A.X(g)} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {dec p:obj o:A o:=A(); ret o.X(p)} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec p:obj; ret A.X(p)} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(h:obj) {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(h:ind) {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(h:obj)->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(h:ind)->obj {ret A.X} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 1)>] // SIG05: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec p:obj; ret A.X(p)} def pred T(v:pred(y:obj)) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05: pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_", "ext U x@/\d+/ -> pred() {dec a:pred(); ret a} def pred T(v:pred()) {dec v:=@1; true}", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "ext U x@/\d+/ -> pred(z:obj) {dec a:pred(x:obj); ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match value A(obj) 
    [<DataRow("MS1b_", "ext U x@/\d+/ -> pred(z:obj) {dec a:pred(x:obj); ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "ext U x@/\d+/ -> pred(z:obj) {dec a:pred(x:obj); ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match value A(ind) 
    [<DataRow("MS1d_", "ext U x@/\d+/ -> pred(z:ind) {dec a:pred(x:ind); ret a} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ -> pred {ret @a} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A ext U x@/\d+/ -> pred {ret A} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec y:obj; ret A.X(y)} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {dec v:=@1; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec a:obj o:A o:=A(); ret o.X(a)} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec a:obj; ret A.X(a)} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {ret A.X} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec a:obj; ret A.X(a)} def pred T(v:pred()) {dec v:=@1; true}", 1)>] // SIG05: pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2", "def pred A(z:obj) ext U x@/\d+/ -> pred(y:obj) {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) ext U x@/\d+/ -> pred(y:obj) {dec h:obj; ret A(h)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) ext U x@/\d+/ -> pred(y:obj) {dec h:ind; ret A(h)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK, only SIG03 instead of SIG05 would be issued: pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) ext U x@/\d+/ -> pred(y:ind) {dec h:ind; ret A(h)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) ext U x@/\d+/ -> pred(y:ind) {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: pred matches signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} ext U x@/\d+/ -> undef {ret A} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj ext U x@/\d+/ -> func()->obj {ret A} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ -> obj {ret @a} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A ext U x@/\d+/ -> A {ret A} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {ret A.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {ret A.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {ret A.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {ret A.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec a:obj; ret A.X(a)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec o:A o:=A(); ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {dec o:A o:=A(); ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec y:obj o:A o:=A(); ret o.X(y)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec y:obj; ret o.X(y)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: pred matches by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(x:ind) {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(x:obj)->obj {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(x:ind)->obj {ret o.X} def pred T(v:pred) {dec v:=@1; true}", 1)>] // SIG05: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(x:obj) {dec y:obj; ret o.X(y)} def pred T(v:pred) {dec v:=@1; true}", 0)>] // OK: pred matches by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3",  "def func A(z:obj)->ind ext U x@/\d+/ -> func(x:obj)->ind {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind ext Test x@/\d+/->ind {dec h:obj; return A(h)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind ext Test x@/\d+/->ind {dec h:ind; return A(h)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind ext Test x@/\d+/->ind {dec h:ind; return A(h)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind ext U x@/\d+/ -> func(x:ind)->ind {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj ext U x@/\d+/ -> func(z:obj)->func()->obj {dec a:func(h:obj)->func()->obj; ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ ->obj {ret @a} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A ext U x@/\d+/ ->A {ret A} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {ret A.X} def pred T(v:func(y:obj)->obj) {dec v:=@1; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->obj) {dec v:=@1; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec o:A a:obj o:=A(); ret o.X(a)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {ret A.X} def pred T(v:func(y:obj)->obj) {dec v:=@1; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } ext U x@/\d+/ -> func(z:obj)->obj {ret A.X} def pred T(v:func(y:obj)->obj) {dec v:=@1; true}", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_", "def func A()->ind ext U x@/\d+/ -> func()->ind {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind ext U x@/\d+/ -> ind {dec h:obj; ret A(h)} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG04: func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind ext U x@/\d+/ -> ind {ret A()} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind ext U x@/\d+/ -> ind {dec h:ind; ret A(h)} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind ext U x@/\d+/ -> func(h:ind)->ind {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj ext U x@/\d+/ -> func()->func()->obj {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ ->obj {ret @a} def pred T(v:func(y:obj)->ind) {dec v:=@1; true}", 1)>] // SIG05: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A ext U x@/\d+/ ->A {ret A} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:ind)->obj {dec a:obj; ret A.X(a)} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:obj)->ind {dec o:A a:obj o:=A(); ret o.X(a)} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:obj)->ind {dec a:obj; ret A.X(a)} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> func(z:obj)->ind {dec a:obj; ret A.X(a)} def pred T(v:func()->ind) {dec v:=@1; true}", 1)>] // SIG05: func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4", "def func A(z:obj)->ind ext U x@/\d+/ -> func(z:obj)->ind {ret A} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind ext U x@/\d+/ -> ind {dec y:obj; ret A(y)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind ext U x@/\d+/ -> ind {dec y:obj; ret A(y)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind ext U x@/\d+/ -> ind {dec y:obj; ret A(y)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind ext U x@/\d+/ -> func(h:ind)->func(g:obj)->ind {ret A} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (axiom)
    [<DataRow("MS4f", "thm A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (theorem)
    [<DataRow("MS4g", "lem A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (lemma)
    [<DataRow("MS4h", "prop A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (proposition)
    [<DataRow("MS4i", "conj A {true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} ext U x@/\d+/ -> pred {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj ext U x@/\d+/ -> func(d:obj)->func()->obj {ret A} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\w/ -> obj {dec y:obj; return y} ext U x@/\d+/ ->obj {ret @a} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A ext U x@/\d+/ ->A {ret A} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(d:ind)->obj {ret A.X} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> obj {dec a:obj; ret A.X(a)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {dec o:A o:=A(); ret o.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {dec o:A o:=A(); ret o.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } ext U x@/\d+/ -> pred() {dec o:A o:=A(); ret o.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {dec o:A o:=A(); ret o.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {dec o:A o:=A(); ret o.X} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec o:A a:obj o:=A(); ret o.X(a)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func)  {dec v:=@1; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func)  {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func)  {dec v:=@1; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func)  {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj)  {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func)  {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } ext U x@/\d+/ -> pred(z:obj) {ret A.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } ext U x@/\d+/ -> pred(z:ind) {ret A.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } ext U x@/\d+/ -> pred() {ret A.X} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } ext U x@/\d+/ -> func()->obj {ret A.X} def pred T(v:func()->obj) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } ext U x@/\d+/ -> func(z:ind)->obj {ret A.X} def pred T(v:func) {dec v:=@1; true}", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } ext U x@/\d+/ -> ind {dec a:obj; ret A.X(a)} def pred T(v:func) {dec v:=@1; true}", 1)>] // SIG05: func does not match by value A.X(obj) 

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG05Extensions(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG05 ""
            
            runTestHelper "TestSIG05Extensions.fpl" fplCode code expected

    // -----------------------------

    // match with simple types
    [<DataRow("ST0", "def pred T(v:obj) {dec v:=undef; true}", 0)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def pred T(v:ind) {dec v:=undef; true}", 0)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def pred T(v:pred) {dec v:=undef; true}", 0)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def pred T(v:func) {dec v:=undef; true}", 0)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def pred T(v:pred()) {dec v:=undef; true}", 0)>]

    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def pred T(v:pred(a:obj)) {dec v:=undef; true}", 0)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def pred T(v:func()->ind) {dec v:=undef; true}", 0)>]
    [<DataRow("NF1", "def pred T(v:func()->obj) {dec v:=undef; true}", 0)>]

    // (mis)match with func(...) types
    [<DataRow("NF_0", "def pred T(v:func(a:obj)->ind) {dec v:=undef; true}", 0)>]
    [<DataRow("NF_1", "def pred T(v:func(a:obj)->obj) {dec v:=undef; true}", 0)>]
    [<DataRow("NF_2", "def pred T(v:func(a:ind)->ind) {dec v:=undef; true}", 0)>]
    [<DataRow("NF_2a", "def pred T(v:func(a:ind)->obj) {dec v:=undef; true}", 0)>]

    // match with class type
    [<DataRow("CT2", "def cl A def pred T(v:A) {dec v:=undef; true}", 0)>] // A is A, no error
    [<DataRow("CT4", "def cl A def cl B:A def pred T(v:B) {dec v:=undef; true}", 0)>] // x is B, no error

    [<DataRow("MS3m", "def pred T(v:func(y:obj)->ind) {dec v:=undef; true}", 0)>] // SIG05: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)

    [<TestMethod>]
    member this.TestSIG05Undef(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG05 ""
            
            runTestHelper "TestSIG05Undef.fpl" fplCode code expected
