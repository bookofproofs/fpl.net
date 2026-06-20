namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR019() =

    [<DataRow("byax_nomix", "prf P$1 {1. byax A1, byax A2 |- true}", 0)>]
    [<DataRow("byax_byconj", "prf P$1 {1. byax A1, byconj C1 |- true}", 1)>]
    [<DataRow("byax_bythm", "prf P$1 {1. byax A1, T1 |- true}", 1)>]
    [<DataRow("byax_bycor", "prf P$1 {1. byax A1, bycor T$1 |- true}", 1)>]
    [<DataRow("byax_bydef_pred", "prf P$1 {1. byax A1, bydef D1 |- true}", 1)>]
    [<DataRow("byax_bydef_func", "prf P$1 {1. byax A1, bydef D2 |- true}", 1)>]
    [<DataRow("byax_bydef_cl", "prf P$1 {1. byax A1, bydef D3 |- true}", 1)>]
    [<DataRow("byax_bydef_var", "prf P$1 {1. byax A1, bydef x |- true}", 1)>]
    [<DataRow("byax_byinf", "prf P$1 {1. byax A1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("byax_byprfarg", "prf P$1 {1. byax A1, T1$3:1  |- true}", 1)>]
    [<DataRow("byax_byarg", "prf P$1 {1: true 2. byax A1, 1 |- true}", 1)>]

    [<DataRow("byconj_nomix", "prf P$1 {1. byconj C1, byconj C2 |- true}", 0)>]
    [<DataRow("byconj_byax", "prf P$1 {1. byconj C1, byax A1 |- true}", 1)>]
    [<DataRow("byconj_bythm", "prf P$1 {1. byconj C1, T1 |- true}", 1)>]
    [<DataRow("byconj_bycor", "prf P$1 {1. byconj C1, bycor T$1 |- true}", 1)>]
    [<DataRow("byconj_bydef_pred", "prf P$1 {1. byconj C1, bydef D1 |- true}", 1)>]
    [<DataRow("byconj_bydef_func", "prf P$1 {1. byconj C1, bydef D2 |- true}", 1)>]
    [<DataRow("byconj_bydef_cl", "prf P$1 {1. byconj C1, bydef D3 |- true}", 1)>]
    [<DataRow("byconj_bydef_var", "prf P$1 {1. byconj C1, bydef x |- true}", 1)>]
    [<DataRow("byconj_byinf", "prf P$1 {1. byconj C1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("byconj_byprfarg", "prf P$1 {1. byconj C1, T1$3:1  |- true}", 1)>]
    [<DataRow("byconj_byarg", "prf P$1 {1: true 2. byconj C1, 1 |- true}", 1)>]

    [<DataRow("bythm_nomix", "prf P$1 {1. T1, T2 |- true}", 0)>]
    [<DataRow("bythm_byax", "prf P$1 {1. T1, byax A1 |- true}", 1)>]
    [<DataRow("bythm_byconj", "prf P$1 {1. T1, byconj C1 |- true}", 1)>]
    [<DataRow("bythm_bycor", "prf P$1 {1. T1, bycor T$1 |- true}", 1)>]
    [<DataRow("bythm_bydef_pred", "prf P$1 {1. T1, bydef D1 |- true}", 1)>]
    [<DataRow("bythm_bydef_func", "prf P$1 {1. T1, bydef D2 |- true}", 1)>]
    [<DataRow("bythm_bydef_cl", "prf P$1 {1. T1, bydef D3 |- true}", 1)>]
    [<DataRow("bythm_bydef_var", "prf P$1 {1. T1, bydef x |- true}", 1)>]
    [<DataRow("bythm_byinf", "prf P$1 {1. T1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("bythm_byprfarg", "prf P$1 {1. T1, T1$3:1  |- true}", 1)>]
    [<DataRow("bythm_byarg", "prf P$1 {1: true 2. T1, 1 |- true}", 1)>]

    [<DataRow("bycor_nomix", "prf P$1 {1. bycor T$1, bycor T$2 |- true}", 0)>]
    [<DataRow("bycor_byax", "prf P$1 {1. bycor T$1, byax A1 |- true}", 1)>]
    [<DataRow("bycor_byconj", "prf P$1 {1. bycor T$1, byconj C1 |- true}", 1)>]
    [<DataRow("bycor_bythm", "prf P$1 {1. bycor T$1, T1 |- true}", 1)>]
    [<DataRow("bycor_bydef_pred", "prf P$1 {1. bycor T$1, bydef D1 |- true}", 1)>]
    [<DataRow("bycor_bydef_func", "prf P$1 {1. bycor T$1, bydef D2 |- true}", 1)>]
    [<DataRow("bycor_bydef_cl", "prf P$1 {1. bycor T$1, bydef D3 |- true}", 1)>]
    [<DataRow("bycor_bydef_var", "prf P$1 {1. bycor T$1, bydef x |- true}", 1)>]
    [<DataRow("bycor_byinf", "prf P$1 {1. bycor T$1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("bycor_byprfarg", "prf P$1 {1. bycor T$1, T1$3:1  |- true}", 1)>]
    [<DataRow("bycor_byarg", "prf P$1 {1: true 2. bycor T$1, 1 |- true}", 1)>]

    [<DataRow("bydef_pred_nomix", "prf P$1 {1. bydef D1, bydef D2 |- true}", 0)>]
    [<DataRow("bydef_pred_byax", "prf P$1 {1. bydef D1, byax A1 |- true}", 1)>]
    [<DataRow("bydef_pred_byconj", "prf P$1 {1. bydef D1, T1 |- true}", 1)>]
    [<DataRow("bydef_pred_bythm", "prf P$1 {1. bydef D1, T1 |- true}", 1)>]
    [<DataRow("bydef_pred_bydef_pred", "prf P$1 {1. bydef D1, bydef D1 |- true}", 0)>]
    [<DataRow("bydef_pred_bydef_func", "prf P$1 {1. bydef D1, bydef D2 |- true}", 0)>]
    [<DataRow("bydef_pred_bydef_cl", "prf P$1 {1. bydef D1, bydef D3 |- true}", 0)>]
    [<DataRow("bydef_pred_bydef_var", "prf P$1 {1. bydef D1, bydef x |- true}", 1)>]
    [<DataRow("bydef_pred_byinf", "prf P$1 {1. bydef D1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("bydef_pred_byprfarg", "prf P$1 {1. bydef D1, T1$3:1  |- true}", 1)>]
    [<DataRow("bydef_pred_byarg", "prf P$1 {1: true 2. bydef D1, 1 |- true}", 1)>]

    [<DataRow("bydef_func_nomix", "prf P$1 {1. bydef D2, bydef D3 |- true}", 0)>]
    [<DataRow("bydef_func_byax", "prf P$1 {1. bydef D2, byax A1 |- true}", 1)>]
    [<DataRow("bydef_func_byconj", "prf P$1 {1. bydef D2, T1 |- true}", 1)>]
    [<DataRow("bydef_func_bythm", "prf P$1 {1. bydef D2, T1 |- true}", 1)>]
    [<DataRow("bydef_func_bydef_pred", "prf P$1 {1. bydef D2, bydef D1 |- true}", 0)>]
    [<DataRow("bydef_func_bydef_func", "prf P$1 {1. bydef D2, bydef D2 |- true}", 0)>]
    [<DataRow("bydef_func_bydef_cl", "prf P$1 {1. bydef D2, bydef D3 |- true}", 0)>]
    [<DataRow("bydef_func_bydef_var", "prf P$1 {1. bydef D2, bydef x |- true}", 1)>]
    [<DataRow("bydef_func_byinf", "prf P$1 {1. bydef D2, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("bydef_func_byprfarg", "prf P$1 {1. bydef D2, T1$3:1  |- true}", 1)>]
    [<DataRow("bydef_func_byarg", "prf P$1 {1: true 2. bydef D2, 1 |- true}", 1)>]

    [<DataRow("bydef_cl_nomix", "prf P$1 {1. bydef D3, bydef D3 |- true}", 0)>]
    [<DataRow("bydef_cl_byax", "prf P$1 {1. bydef D3, byax A1 |- true}", 1)>]
    [<DataRow("bydef_cl_byconj", "prf P$1 {1. bydef D3, T1 |- true}", 1)>]
    [<DataRow("bydef_cl_bythm", "prf P$1 {1. bydef D3, T1 |- true}", 1)>]
    [<DataRow("bydef_cl_bydef_pred", "prf P$1 {1. bydef D3, bydef D1 |- true}", 0)>]
    [<DataRow("bydef_cl_bydef_func", "prf P$1 {1. bydef D3, bydef D2 |- true}", 0)>]
    [<DataRow("bydef_cl_bydef_cl", "prf P$1 {1. bydef D3, bydef D3 |- true}", 0)>]
    [<DataRow("bydef_cl_bydef_var", "prf P$1 {1. bydef D3, bydef x |- true}", 1)>]
    [<DataRow("bydef_cl_byinf", "prf P$1 {1. bydef D3, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("bydef_cl_byprfarg", "prf P$1 {1. bydef D3, T1$3:1  |- true}", 1)>]
    [<DataRow("bydef_cl_byarg", "prf P$1 {1: true 2. bydef D3, 1 |- true}", 1)>]

    [<DataRow("bydef_var_nomix", "prf P$1 {1. bydef x, bydef y |- true}", 0)>]
    [<DataRow("bydef_var_byax", "prf P$1 {1. bydef x, byax A1 |- true}", 1)>]
    [<DataRow("bydef_var_byconj", "prf P$1 {1. bydef x, T1 |- true}", 1)>]
    [<DataRow("bydef_var_bythm", "prf P$1 {1. bydef x, T1 |- true}", 1)>]
    [<DataRow("bydef_var_bydef_pred", "prf P$1 {1. bydef x, bydef D1 |- true}", 1)>]
    [<DataRow("bydef_var_bydef_func", "prf P$1 {1. bydef x, bydef D2 |- true}", 1)>]
    [<DataRow("bydef_var_bydef_cl", "prf P$1 {1. bydef x, bydef D3 |- true}", 1)>]
    [<DataRow("bydef_var_bydef_var", "prf P$1 {1. bydef x, bydef x |- true}", 0)>]
    [<DataRow("bydef_var_byinf", "prf P$1 {1. bydef x, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("bydef_var_byprfarg", "prf P$1 {1. bydef x, T1$3:1  |- true}", 1)>]
    [<DataRow("bydef_var_byarg", "prf P$1 {1: true 2. bydef x, 1 |- true}", 1)>]

    [<DataRow("byinf_nomix", "prf P$1 {1. byinf I1, byinf I2 |- true}", 0)>]
    [<DataRow("byinf_byax", "prf P$1 {1. byinf I1, byax A1 |- true}", 1)>]
    [<DataRow("byinf_byconj", "prf P$1 {1. byinf I1, T1 |- true}", 1)>]
    [<DataRow("byinf_bythm", "prf P$1 {1. byinf I1, T1 |- true}", 1)>]
    [<DataRow("byinf_bydef_pred", "prf P$1 {1. byinf I1, bydef D1 |- true}", 1)>]
    [<DataRow("byinf_bydef_func", "prf P$1 {1. byinf I1, bydef D2 |- true}", 1)>]
    [<DataRow("byinf_bydef_cl", "prf P$1 {1. byinf I1, bydef D3 |- true}", 1)>]
    [<DataRow("byinf_bydef_var", "prf P$1 {1. byinf I1, bydef x |- true}", 1)>]
    [<DataRow("byinf_byinf", "prf P$1 {1. byinf I1, byinf I1 |- true}", 0)>]
    [<DataRow("byinf_byprfarg", "prf P$1 {1. byinf I1, T1$3:1  |- true}", 1)>]
    [<DataRow("byinf_byarg", "prf P$1 {1: true 2. byinf I1, 1 |- true}", 1)>]

    [<DataRow("byprfarg_nomix", "prf P$1 {1. T1$3:1, T1$4:1 |- true}", 0)>]
    [<DataRow("byprfarg_byax", "prf P$1 {1. T1$3:1, byax A1 |- true}", 1)>]
    [<DataRow("byprfarg_byconj", "prf P$1 {1. T1$3:1, T1 |- true}", 1)>]
    [<DataRow("byprfarg_bythm", "prf P$1 {1. T1$3:1, T1 |- true}", 1)>]
    [<DataRow("byprfarg_bydef_pred", "prf P$1 {1. T1$3:1, bydef D1 |- true}", 1)>]
    [<DataRow("byprfarg_bydef_func", "prf P$1 {1. T1$3:1, bydef D2 |- true}", 1)>]
    [<DataRow("byprfarg_bydef_cl", "prf P$1 {1. T1$3:1, bydef D3 |- true}", 1)>]
    [<DataRow("byprfarg_bydef_var", "prf P$1 {1. T1$3:1, bydef x |- true}", 1)>]
    [<DataRow("byprfarg_byinf", "prf P$1 {1. T1$3:1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("byprfarg_byprfarg", "prf P$1 {1. T1$3:1, T1$3:1  |- true}", 0)>]
    [<DataRow("byprfarg_byarg", "prf P$1 {1: true 2. T1$3:1, 1 |- true}", 1)>]

    [<DataRow("byarg_nomix", "prf P$1 {1: true 2: true 3. 1, 2 |- true}", 0)>]
    [<DataRow("byarg_byax", "prf P$1 {1: true 2. 1, byax A1 |- true}", 1)>]
    [<DataRow("byarg_byconj", "prf P$1 {1: true 2. 1, T1 |- true}", 1)>]
    [<DataRow("byarg_bythm", "prf P$1 {1: true 2. 1, T1 |- true}", 1)>]
    [<DataRow("byarg_bydef_pred", "prf P$1 {1: true 2. 1, bydef D1 |- true}", 1)>]
    [<DataRow("byarg_bydef_func", "prf P$1 {1: true 2. 1, bydef D2 |- true}", 1)>]
    [<DataRow("byarg_bydef_cl", "prf P$1 {1: true 2. 1, bydef D3 |- true}", 1)>]
    [<DataRow("byarg_bydef_var", "prf P$1 {1: true 2. 1, bydef x |- true}", 1)>]
    [<DataRow("byarg_byinf", "prf P$1 {1: true 2. 1, byinf I1 |- true}", 0)>] // last byinf allows mixing
    [<DataRow("byarg_byprfarg", "prf P$1 {1: true 2. 1, T1$3:1  |- true}", 1)>]
    [<DataRow("byarg_byarg", "prf P$1 {1: true 2: true 3. 2, 1 |- true}", 0)>]

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR019(no:string, fplCodePrf:string, expected) =
        if offlineWatcher.OfflineMode && fplCodePrf.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR019("", "")
            let fplCodePre = """
                ax A1 {true}
                ax A2 {true}
                conj C1 {true}
                conj C2 {true}
                thm T1 {true}
                thm T2 {true}
                cor T1$1 {true}
                cor T2$1 {true}
                def pred D1() 
                def func D2()->obj
                def cl D3
                inf I1 {pre:true con:true}
                inf I2 {pre:true con:true}
                prf T1$3 {1: true}
                prf T2$4 {1: true}
                thm P {dec x:obj y:ind; true} 
            """
            runTestHelper "TestPR019.fpl" (fplCodePre + fplCodePrf) code expected
