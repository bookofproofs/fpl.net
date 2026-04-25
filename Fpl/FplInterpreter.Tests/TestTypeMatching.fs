namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreter.Globals.Heap
open FplInterpreterFplTypeMatching
open CommonTestHelpers

[<TestClass>]
type TestTypeMatching() =


    [<DataRow("pred_obj_true", "def pred T(a:pred(b:obj)) {true};", 1)>]
    [<DataRow("pred_pred_true", "def pred T(a:pred(b:pred)) {true};", 1)>]
    [<DataRow("pred__true", "def pred T(a:pred()) {true};", 1)>]
    [<DataRow("pred_pred_obj_true", "def pred T(a:pred(b:pred, c:obj)) {true};", 1)>]
    [<DataRow("pred_obj_$1", "def pred T(a:pred(b:obj)) {$1};", 1)>]
    [<DataRow("pred_pred_$1", "def pred T(a:pred(b:pred)) {$1};", 1)>]
    [<DataRow("pred__$1", "def pred T(a:pred()) {$1};", 1)>]
    [<DataRow("pred_pred_obj_$1", "def pred T(a:pred(b:pred, c:obj)) {$1};", 1)>]
    [<DataRow("func_obj_true", "def pred T(a:func(b:obj)->ind) {true};", 1)>]
    [<DataRow("func_pred_true", "def pred T(a:func(b:pred)->ind) {true};", 1)>]
    [<DataRow("func__true", "def pred T(a:func()->ind) {true};", 1)>]
    [<DataRow("func_pred_obj_true", "def pred T(a:func(b:pred, c:obj)->ind) {true};", 1)>]
    [<DataRow("func_obj_$1", "def pred T(a:func(b:obj)->ind) {$1};", 1)>]
    [<DataRow("func_pred_$1", "def pred T(a:func(b:pred)->ind) {$1};", 1)>]
    [<DataRow("func__$1", "def pred T(a:func()->ind) {$1};", 1)>]
    [<DataRow("func_pred_obj_$1", "def pred T(a:func(b:pred, c:obj)->ind) {$1};", 1)>]
    [<TestMethod>]
    member this.TestMatchingParameterizedVariablesWithValues(no:string, fplCode, errNo:int) =
        let filename = "TestMatchingParameterizedVariablesWithValues.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope.Values |> Seq.toList |> List.head
        let par = pred.Scope["a"]
        let arg = pred.ArgList |> Seq.last
        match matchArgumentsWithParameters arg par, errNo with
        | None, 0 -> ()
        | Some err, 1 ->
            printf "%s" err
            Assert.IsTrue(err.Length>0)
        | Some err, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for error:{err}")
        | None, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for no error")
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("pred_obj_00", "def pred T(a:pred(b:obj)) {dec ~x:pred(y:obj); x};", 0)>]
    [<DataRow("pred_ind_00", "def pred T(a:pred(b:ind)) {dec ~x:pred(y:obj); x};", 1)>]
    [<DataRow("pred_ind_01", "def pred T(a:pred(b:ind)) {dec ~x:pred(y:ind); x};", 0)>]
    [<DataRow("pred_ind_02", "def pred T(a:pred(b:ind)) {dec ~x:pred(); x};", 1)>]
    [<DataRow("pred_ind_02", "def pred T(a:pred()) {dec ~x:pred(y:ind); x};", 1)>]
    [<TestMethod>]
    member this.TestMatchingParameterizedVariablesWithOthers(no:string, fplCode, errNo:int) =
        let filename = "TestMatchingParameterizedVariables.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope.Values |> Seq.toList |> List.head
        let par = pred.Scope["a"]
        let arg = pred.ArgList |> Seq.last
        match matchArgumentsWithParameters arg par, errNo with
        | None, 0 -> ()
        | Some err, 1 ->
            printf "%s" err
            Assert.IsTrue(err.Length>0)
        | Some err, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for error:{err}")
        | None, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for no error")
        prepareFplCode(filename, "", false) |> ignore
