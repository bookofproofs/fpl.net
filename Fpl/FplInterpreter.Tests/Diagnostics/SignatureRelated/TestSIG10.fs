namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG10() =

    [<DataRow("01", "def pred T() {dec i,j:ind arr:*obj[ind] arr[i,j]:=undef; true}", 1)>]   // 1D array, 2 indices -> 1 extra (SIG10)
    [<DataRow("01b", "def pred T() {dec i:ind arr:*obj[ind] arr[i]:=undef; true}", 0)>]   // 1D array, 1 indices -> valid
    [<DataRow("02", "def pred T() {dec i,j,k:ind arr:*obj[ind] arr[i,j,k]:=undef; true}", 2)>] // 1D array, 3 indices -> 2 extra (SIG10 x2)
    [<DataRow("02a", "def pred T() {dec i,j,k:ind arr:*obj[ind,ind,ind] arr[i,j,k]:=undef; true}", 0)>] // 3D array, 3 indices -> valid
    [<DataRow("02b", "def pred T() {dec i,j,k:ind arr:*obj[ind,ind,ind] arr[i,j]:=undef; true}", 0)>] // 3D array, 2 indices -> not relevant for SIG10
    [<DataRow("03", "def pred T() {dec i,j,k:ind arr:*obj[ind,ind] arr[i,j,k]:=undef; true}", 1)>] // 2D array, 3 indices -> 1 extra
    [<DataRow("04", "def pred T() {dec i,j,k,l:ind arr:*obj[ind,ind,ind] arr[i,j,k,l]:=undef; true}", 1)>] // 3D array, 4 indices -> 1 extra
    [<DataRow("04a", "def pred T() {dec i,j,k,l:ind arr:*obj[ind,ind,ind,ind] arr[i,j,k,l]:=undef; true}", 0)>] // 4D array, 4 indices -> valid
    [<DataRow("05", "def func A()->ind def func B()->ind def pred T() {dec arr:*obj[ind] arr[A(),B()]:=undef; true}", 1)>] // functional indices: 2 supplied for 1D array -> 1 extra
    [<DataRow("06", "def func A()->ind def func B()->ind def func C()->ind def pred T() {dec arr:*obj[ind,ind] arr[A(),B(),C()]:=undef; true}", 1)>] // 3 functional indices for 2D array -> 1 extra
    [<DataRow("07", "def pred T() {dec arr:*obj[ind,ind] arr[$1,$2,$3]:=undef; true}", 1)>] // explicit $-indices: 3 provided for 2D array -> 1 extra
    // nested indices
    [<DataRow("N01", "def pred T() {dec i:*ind[ind] j:ind arr:*ind[ind] arr[i[j]]:=undef; true}", 0)>]    // i is 1D, i[j] is 1D -> ok
    [<DataRow("N02", "def pred T() {dec i:*ind[obj,ind] k:obj j:ind arr:*ind[ind] arr[i[k,k,j,j], i[k,j]]:=undef; true}", 3)>]    // i is 2D, i[k,k,j,j] is 4D -> 2x SIG10; arr is 1D, arr[i[k,k,j,j], i[k,j]] is 2D -> 1x SIG10 
    [<DataRow("N02a", "def pred T() {dec i:*ind[obj,ind] k:obj j:ind arr:*ind[ind] arr[i[k,k,j], i[k,j]]:=undef; true}", 2)>]    // i is 2D, i[k,k,j] is 3D -> 1x SIG10; arr is 1D, arr[i[k,k,j], i[k,j]] is 2D -> 2x SIG10 
    [<DataRow("N02b", "def pred T() {dec i:*ind[obj,ind] k:obj j:ind arr:*ind[ind] arr[i[k,k], i[k,j]]:=undef; true}", 1)>]    // i is 2D, i[k,k] is 2D -> valid; arr is 1D, arr[i[k,k], i[k,j]] is 2D -> 1x SIG10 
    [<DataRow("N02c", "def pred T() {dec i:*ind[obj,ind] k:obj j:ind arr:*ind[ind] arr[i[k,k]]:=undef; true}", 0)>]    // i is 2D, i[k,k] is 2D -> valid, arr is 1D; arr[i[k,k]] is 1D -> valid
    [<DataRow("N03", "def pred T() {dec i:*ind[obj,obj] k:obj j:ind arr:*ind[ind] arr[i[k,j,k,k,j]]:=undef; true}", 3)>]    // i is 2D, i[k,j,k,k,j] is 5D -> 3x SIG10; arr is 1D arr[i[k,j,k,k,j]]] is 1D -> valid
    [<DataRow("N03a", "def pred T() {dec i:*ind[obj,obj] k:obj j:ind arr:*ind[ind] arr[i[k,j,k,k]]:=undef; true}", 2)>]    // i is 2D, i[k,j,k,k] is 4D -> 2x SIG10; arr is 1D arr[i[k,j,k,k]] is 1D -> valid
    [<DataRow("N03b", "def pred T() {dec i:*ind[obj,obj] k:obj j:ind arr:*ind[ind] arr[i[k,j,k]]:=undef; true}", 1)>]    // i is 2D, i[k,j,k] is 3D -> 1x SIG10; arr is 1D arr[i[k,j,k]] is 1D -> valid
    [<DataRow("N03c", "def pred T() {dec i:*ind[obj,obj] k:obj j:ind arr:*ind[ind] arr[i[k,j]]:=undef; true}", 0)>]    // i is 2D, i[k,j] is 2D -> valid; arr is 1D arr[i[k,j]] is 1D -> valid
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG10(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG10 ("", "", 0)
            runTestHelper "TestSIG10.fpl" fplCode code expected
