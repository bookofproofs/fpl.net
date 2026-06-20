namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG09() =

    [<DataRow("01", "def pred T() {dec i:ind arr:*obj[ind,ind] arr[i]:=undef; true}", 1)>] // array two-dimensional, index one-dimensional -> invalid
    [<DataRow("02", "def pred T() {dec i,j:obj arr:*Base[obj,obj] arr[i,j]:=undef; true}", 0)>] // array two-dimensional, index two-dimensional -> valid
    [<DataRow("03", "def pred T() {dec i:ind arr:*obj[ind,ind,ind] arr[i]:=undef; true}", 2)>]   // 3D array, single index -> 2x SIG09
    [<DataRow("04", "def pred T() {dec i,j:ind arr:*obj[ind,ind,ind] arr[i,j]:=undef; true}", 1)>] // 3D array, two indices -> SIG09
    [<DataRow("05", "def pred T() {dec i,j,k:ind arr:*obj[ind,ind,ind] arr[i,j,k]:=undef; true}", 0)>] // 3D array, three indices -> valid
    [<DataRow("06", "def func X()->ind def pred T() {dec arr:*obj[ind,ind] arr[X()]:=undef; true}", 1)>] // 2D array, one index via functional term -> SIG09
    [<DataRow("06a", "def func X()->ind def pred T() {dec arr:*obj[ind,ind] arr[X(),X()]:=undef; true}", 0)>] // 2D array, two indexes via functional term -> valid
    [<DataRow("07", "def func A()->ind def func B()->ind def pred T() {dec arr:*obj[ind,ind,ind] arr[A(),B()]:=undef; true}", 1)>] // 3D array, two functional indices -> SIG09
    [<DataRow("07a", "def func A()->ind def func B()->ind def pred T() {dec arr:*obj[ind,ind,ind] arr[A(),B(),$1]:=undef; true}", 0)>] // 3D array, indices -> valid
    [<DataRow("08", "def pred T() {dec arr:*obj[ind,ind] arr[$1,$2]:=undef; true}", 0)>] // 2D array, two indices -> valid
    // nested indices
    [<DataRow("N01", "def pred T() {dec i:*ind[ind,ind] j:ind arr:*ind[ind] arr[i[j]]:=undef; true}", 1)>]    // i is 2D, i[j] is 1D -> SIG09
    [<DataRow("N02", "def pred T() {dec i:*ind[obj,ind,ind] k:obj j:ind arr:*ind[ind] arr[i[k,j]]:=undef; true}", 1)>]    // i is 3D, i[k,j] is 2D -> SIG09
    [<DataRow("N03", "def pred T() {dec i:*ind[obj,ind,obj] k:obj j:ind arr:*ind[ind] arr[i[k,j,k]]:=undef; true}", 0)>]    // i is 3D, i[k,j,k] is 3D -> ok
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG09(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG09 ("", "", 0)
            runTestHelper "TestSIG09.fpl" fplCode code expected
