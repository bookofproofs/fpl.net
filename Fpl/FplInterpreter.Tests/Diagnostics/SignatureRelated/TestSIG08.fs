namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG08() =

    [<DataRow("00", "def pred T() {dec i:ind arr:*ind[ind] arr[i]:=undef; true}", 0)>]    
    [<DataRow("00a", "def pred T() {dec i:Nat arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("00b", "def pred T() {dec i:pred arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("00c", "def pred T() {dec i:func arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("00d", "def pred T() {dec i:*ind[ind] arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("00e", "def pred T() {dec i:*Nat[ind] arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("00f", "def pred T() {dec i:*pred[ind] arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("00g", "def pred T() {dec i:*func[ind] arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("01", "def pred T() {dec i:ind i:=$1 arr:*ind[ind] arr[i]:=undef; true}", 0)>]    
    [<DataRow("02", "def pred T(i:ind) {dec arr:*ind[ind] arr[i]:=undef; true}", 0)>]    
    [<DataRow("03", "def pred T() {dec i:ind i:=$1 arr:*ind[ind] arr[i]:=undef; true}", 0)>]    
    [<DataRow("04", "def pred T() {dec arr:*ind[ind] arr[i]:=undef; true}", 1)>]    
    [<DataRow("05", "def pred T() {dec i:obj arr:*ind[ind] arr[i]:=undef; true}", 1)>]    // index typed `obj` — invalid
    [<DataRow("06", "def pred T() {dec i:ind arr:*obj[ind] arr[i]:=undef; true}", 0)>]    // array element type `obj` incompatible with expected `ind`
    [<DataRow("07", "def pred T(i:obj) {dec arr:*ind[ind] arr[i]:=undef; true}", 1)>]        // parameter `i` typed `obj` — invalid
    [<DataRow("08", "def pred T() {dec i:ind arr:*ind[ind] arr[i]:=undef arr[i]:=undef; true}", 0)>] // repeated valid indexing, should succeed
    [<DataRow("08a", "def pred T() {dec i:ind arr1:*ind[ind] arr2:*ind[ind] arr2[arr1[i]]:=undef; true}", 0)>]   // arr indexed by array type *ind[ind], i has that type -> valid
    [<DataRow("08b", "def pred T() {dec i:*ind[ind] arr:*ind[ind] arr[i]:=undef; true}", 1)>]         // i is array, arr expects ind -> invalid
    [<DataRow("08c", "def pred T() {dec i:ind arr:*ind[ind] arr[i]:=undef; true}", 0)>]       // arr expects array index, provided ind -> invalid
    [<DataRow("08d", "def pred T() {dec i:*ind[ind] j:ind arr:*ind[ind] arr[i[j]]:=undef; true}", 0)>] // i[j] is ind, arr indexed by ind -> valid
    [<DataRow("08e", "def pred T() {dec i:*ind[ind] j:ind arr:*ind[obj] arr[i[j]]:=undef; true}", 1)>] // i[j] is ind but arr expects obj -> invalid
    [<DataRow("08f", "def pred T() {dec irr:*pred[ind] krr:*ind[ind] arr:*ind[pred,ind] arr[irr[$1],krr[$1]]:=undef; true}", 0)>] // irr[$1] is pred, *krr[$1] is ind, arr accepts that -> valid
    
    // arrays indexed by nested arrays 
    [<DataRow("N01", "def pred T() {dec i:*ind[ind] j:ind arr:*ind[ind] arr[i[j]]:=undef; true}", 0)>]  // i[j] is ind, arr expects ind
    [<DataRow("N02", "def pred T() {dec i:*obj[ind] j:ind arr:*ind[ind] arr[i[j]]:=undef; true}", 1)>]   // i[j] is obj, arr expects ind
    [<DataRow("N03", "def pred T() {dec i:*ind[ind] j:obj arr:*ind[ind] arr[i[j]]:=undef; true}", 1)>]    // j has wrong type to index i
    [<DataRow("N04", "def pred T() {dec i:*ind[ind] j:ind arr:*obj[ind] arr[i[j]]:=undef; true}", 0)>]   // arr element type obj is irrelevant for indexing, valid
    [<DataRow("N06", "def pred T() {dec idx:*ind[ind] k:ind arr:*ind[ind] arr[idx[k]]:=undef; true}", 0)>]   // idx[k] yields ind, valid as arr index
    [<DataRow("N07", "def pred T() {dec j:*ind[ind] l:ind i:*ind[ind] arr:*ind[ind] arr[i[j[l]]]:=undef; true}", 0)>] // j[l] -> ind, i[...] -> ind, arr[...] valid
    [<DataRow("N08", "def pred T() {dec j:*obj[ind] l:ind i:*ind[ind] arr:*ind[ind] arr[i[j[l]]]:=undef; true}", 1)>] // j[l] -> obj, breaks inner indexing chain

    // functional terms as indexes of arrays
    [<DataRow("F01", "def func T()->obj def pred T() {dec arr:*ind[obj] arr[T()]:=undef; true}", 0)>]  // T() is obj, arr accepts it
    [<DataRow("F02", "def func T(x:ind)->obj def pred T() {dec x:ind arr:*ind[obj] arr[T(x)]:=undef; true}", 0)>]  // T() is obj, arr accepts it
    [<DataRow("F03", "def func T(x:ind)->pred def pred T() {dec x:ind arr:*ind[obj] arr[T(x)]:=undef; true}", 1)>]  // T() is pred, arr does not accept it
    [<DataRow("F04", "def func T()->ind def pred S() {dec arr:*obj[ind] arr[T()]:=undef; true}", 0)>]   // T() -> ind, arr indexed by ind -> valid
    [<DataRow("F05", "def func T()->ind def pred S() {dec arr:*obj[obj] arr[T()]:=undef; true}", 1)>]   // T() -> ind, arr expects obj -> invalid
    [<DataRow("F06", "def func T()->func def pred S() {dec arr:*obj[func] arr[T()]:=undef; true}", 0)>] // T() -> func, arr indexed by func -> valid
    [<DataRow("F07", "def func T()->pred def pred S() {dec arr:*obj[pred] arr[T()]:=undef; true}", 0)>] // T() -> pred, arr indexed by pred -> valid
    [<DataRow("F08", "def func U()->ind def func T(x:ind)->obj def pred S() {dec arr:*ind[obj] arr[T(U())]:=undef; true}", 0)>] // nested call T(U()) -> obj, arr expects obj -> valid
    [<DataRow("F09", "def func U()->obj def func T(x:obj)->ind def pred S() {dec arr:*obj[obj] arr[T(U())]:=undef; true}", 1)>] // T(U()) -> ind, arr expects obj -> invalid
    [<DataRow("F10", "def func T(x:ind)->obj def pred S(x:ind) {dec arr:*ind[obj] arr[T(x)]:=undef; true}", 0)>] // function with param, return obj used as index -> valid
    [<DataRow("F11", "def func T()->obj def pred S() {dec arr:*ind[func] arr[T()]:=undef; true}", 1)>] // T() -> obj, arr expects func -> invalid
    [<DataRow("F12", "def func T()->ind def pred S() {dec arr:*ind[ind] arr[T()]:=undef; true}", 0)>] // simple match, T() -> ind and arr indexed by ind -> valid

    // classes as indexes of arrays
    [<DataRow("C01", "def cl A def pred T() {dec i:A arr:*ind[obj] arr[i]:=undef; true}", 0)>]  // i is A, obj accepts any class, arr expects an obj index -> valid
    [<DataRow("C02", "def cl A def cl B def pred T() {dec i:B arr:*ind[A] arr[i]:=undef; true}", 1)>]  // i is B, B is not derived from A, arr expects an A index -> invalid
    [<DataRow("C03", "def cl A def cl B:A def pred T() {dec i:B arr:*ind[A] arr[i]:=undef; true}", 0)>]  // i is B, B is derived from A, arr expects an A index -> valid
    [<DataRow("C04", "def cl A def pred T() {dec i:A arr:*ind[A] arr[i]:=undef; true}", 0)>]   // i:A, arr indexed by A -> valid
    [<DataRow("C05", "def cl A def pred T() {dec i:obj arr:*ind[A] arr[i]:=undef; true}", 1)>] // i:obj is supertype, arr expects A, cannot index arr[obj] -> invalid
    [<DataRow("C06", "def cl A def cl D:A def pred T() {dec i:D arr:*ind[A] arr[i]:=undef; true}", 0)>] // D derives A -> valid
    [<DataRow("C07", "def cl A def cl B def cl C def pred T() {dec i:C arr:*ind[A] arr[i]:=undef; true}", 1)>] // C unrelated to A -> invalid
    [<DataRow("C08", "def cl X def cl Y def cl M:X,Y def pred T() {dec i:M arr:*ind[X] arr[i]:=undef; true}", 0)>] // M derives X and Y -> valid for X-index
    [<DataRow("C09", "def cl Base def cl Mid:Base def cl Leaf:Mid def pred T() {dec i:Leaf arr:*ind[Base] arr[i]:=undef; true}", 0)>] // Leaf->Mid->Base chain -> valid
    [<DataRow("C10", "def cl A def cl B def cl M:A,B def pred T() {dec i:B arr:*ind[M] arr[i]:=undef; true}", 1)>] // B is a base of M? here M derives from A,B so B is base but arr expects M -> invalid (B is not M)
    [<DataRow("C11", "def cl A def cl B:A def pred T() {dec i:obj arr:*ind[obj] arr[i]:=undef; true}", 0)>] // i:obj and arr indexed by obj -> valid

    // mixed - functional terms yielding class types used as indexes
    [<DataRow("M01", "def cl A def func F()->A def pred T() {dec arr:*ind[obj] arr[F()]:=undef; true}", 0)>]  // F() is A, obj accepts any class, arr expects an obj index -> valid
    [<DataRow("M02", "def cl A def func F()->A def pred T() {dec arr:*ind[A] arr[F()]:=undef; true}", 0)>]   // F() -> A, arr expects A -> valid
    [<DataRow("M03", "def cl A def cl B:A def func F()->B def pred T() {dec arr:*ind[A] arr[F()]:=undef; true}", 0)>] // F() -> B (inherits A), arr expects A -> valid
    [<DataRow("M04", "def cl A def func F()->A def pred T() {dec arr:*ind[B] arr[F()]:=undef; true}", 1)>]   // F() -> A, arr expects B (unrelated) -> invalid
    [<DataRow("M05", "def cl A def func F()->obj def pred T() {dec arr:*ind[A] arr[F()]:=undef; true}", 1)>] // F() -> obj, arr expects A -> invalid (obj is supertype)
    [<DataRow("M06", "def cl Base def cl Derived:Base def func F()->Derived def pred T() {dec arr:*ind[Base] arr[F()]:=undef; true}", 0)>] // Derived->Base chain -> valid
    [<DataRow("M07", "def cl X def cl Y def cl M:X,Y def func F()->M def pred T() {dec arr:*ind[X] arr[F()]:=undef; true}", 0)>] // M derives X,Y -> valid for X-index
    [<DataRow("M08", "def cl B def cl A def func U()->B def func F(x:B)->A def pred T() {dec arr:*ind[A] arr[F(U())]:=undef; true}", 0)>] // nested call F(U()) -> A -> valid
    [<DataRow("M09", "def cl A def func F()->A def pred T() {dec arr:*ind[obj] arr[F()]:=undef; true}", 0)>] // arr indexed by obj accepts class A -> valid
    [<DataRow("M10", "def cl A def cl B:A def func F()->A def pred T() {dec arr:*ind[B] arr[F()]:=undef; true}", 1)>] // F() -> A, arr expects B (derived) -> invalid

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG08(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG08 ("", "", "", "", 0)
            runTestHelper "TestSIG08.fpl" fplCode code expected
