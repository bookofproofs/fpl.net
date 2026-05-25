namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreter.Globals.Heap
open FplInterpreterBasicTypes
open FplInterpreterFplTypeMatching
open FplInterpreterProofs
open CommonTestHelpers
open FplPrimitives


[<TestClass>]
type TestTypeMatching() =


    [<DataRow("pred_obj_true", "def pred T(a:pred(b:obj)) {true}", 1)>]
    [<DataRow("pred_pred_true", "def pred T(a:pred(b:pred)) {true}", 1)>]
    [<DataRow("pred__true", "def pred T(a:pred()) {true}", 1)>]
    [<DataRow("pred_pred_obj_true", "def pred T(a:pred(b:pred, c:obj)) {true}", 1)>]
    [<DataRow("pred_obj_$1", "def pred T(a:pred(b:obj)) {$1}", 1)>]
    [<DataRow("pred_pred_$1", "def pred T(a:pred(b:pred)) {$1}", 1)>]
    [<DataRow("pred__$1", "def pred T(a:pred()) {$1}", 1)>]
    [<DataRow("pred_pred_obj_$1", "def pred T(a:pred(b:pred, c:obj)) {$1}", 1)>]
    [<DataRow("func_obj_true", "def pred T(a:func(b:obj)->ind) {true}", 1)>]
    [<DataRow("func_pred_true", "def pred T(a:func(b:pred)->ind) {true}", 1)>]
    [<DataRow("func__true", "def pred T(a:func()->ind) {true}", 1)>]
    [<DataRow("func_pred_obj_true", "def pred T(a:func(b:pred, c:obj)->ind) {true}", 1)>]
    [<DataRow("func_obj_$1", "def pred T(a:func(b:obj)->ind) {$1}", 1)>]
    [<DataRow("func_pred_$1", "def pred T(a:func(b:pred)->ind) {$1}", 1)>]
    [<DataRow("func__$1", "def pred T(a:func()->ind) {$1}", 1)>]
    [<DataRow("func_pred_obj_$1", "def pred T(a:func(b:pred, c:obj)->ind) {$1}", 1)>]
    [<TestMethod>]
    member this.TestMatchingParameterizedVariablesWithValues(no:string, fplCode, errNo:int) =
        let filename = "TestMatchingParameterizedVariablesWithValues.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope.Values |> Seq.toList |> List.head
        let par = pred.Scope["a"]
        let arg = pred.ArgList |> Seq.last
        match FplTypeMatcher.MatchArgumentsWithParameters arg par, errNo with
        | None, 0 -> ()
        | Some err, 1 ->
            printf "%s" err
            Assert.IsTrue(err.Length>0)
        | Some err, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for error:{err}")
        | None, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for no error")
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("pred_obj_00", "def pred T(a:pred(b:obj)) {dec x:pred(y:obj); x}", 0)>]
    [<DataRow("pred_ind_00", "def pred T(a:pred(b:ind)) {dec x:pred(y:obj); x}", 1)>]
    [<DataRow("pred_ind_01", "def pred T(a:pred(b:ind)) {dec x:pred(y:ind); x}", 0)>]
    [<DataRow("pred_ind_02", "def pred T(a:pred(b:ind)) {dec x:pred(); x}", 1)>]
    [<DataRow("pred_ind_02", "def pred T(a:pred()) {dec x:pred(y:ind); x}", 1)>]
    [<TestMethod>]
    member this.TestMatchingParameterizedVariablesWithOthers(no:string, fplCode, errNo:int) =
        let filename = "TestMatchingParameterizedVariables.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope.Values |> Seq.toList |> List.head
        let par = pred.Scope["a"]
        let arg = pred.ArgList |> Seq.last
        match FplTypeMatcher.MatchArgumentsWithParameters arg par, errNo with
        | None, 0 -> ()
        | Some err, 1 ->
            printf "%s" err
            Assert.IsTrue(err.Length>0)
        | Some err, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for error:{err}")
        | None, _ ->
            Assert.IsFalse(true, $"wrong test parameters errNo:{errNo} for no error")
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("pred_vars_01", "def pred T() {dec x:pred(y:ind); x}", "pred(ind)")>]
    [<DataRow("pred_vars_02", "def pred T(a:pred()) {a}", "pred()")>]
    [<DataRow("pred_vars_03", "def pred T(a:pred(b:ind, c:pred(d:func))) {a}", "pred(ind, pred(func))")>]
    [<DataRow("pred_vars_04", "def pred T() {dec a:ind x:pred(y:ind); x(a)}", "pred(ind)")>]
    [<DataRow("func_vars_01", "def pred T() {dec x:func(y:ind)->obj; x}", "func(ind) -> obj")>]
    [<DataRow("func_vars_02", "def pred T(a:func()->ind) {a}", "func() -> ind")>]
    [<DataRow("func_vars_03", "def pred T(a:func(b:ind, c:pred(d:func))->func) {a}", "func(ind, pred(func)) -> func")>]
    [<DataRow("isop_01", "def pred T(a:obj) {is(a,ind)}", "pred(obj)")>]
    [<DataRow("isop_02", "def cl A def pred T(a:obj) {is(a,A)}", "pred(obj)")>]
    [<DataRow("isop_03", "def cl A def pred T(a:A) {is(a,ind)}", "pred(A)")>]
    [<DataRow("pred_val_01", "def pred T() {true}", "pred()")>]
    [<DataRow("pred_val_02", "def pred T() {false}", "pred()")>]
    [<DataRow("pred_val_03", "def pred T() {dec x:pred x:=false; x}", "pred()")>]
    [<DataRow("pred_val_04", "def pred T() {dec x:pred x:=true; x}", "pred()")>]
    [<DataRow("conjunction_01", "def pred T() {and(true, impl(true, false))}", "pred()")>]
    [<DataRow("conjunction_02", "def pred T(x:obj) {and(true, impl(x, false))}", "pred(obj)")>]
    [<DataRow("conjunction_03", "def pred T(x:ind, y:obj) {and(y, iif(true, x))}", "pred(obj, ind)")>]
    [<DataRow("conjunction_04", "def cl A def pred T(x:A) {and(x, xor(true, false))}", "pred(A)")>]
    [<DataRow("conjunction_05", "def cl A def pred T(x:A, y:ind) {and(x, xor(y, x))}", "pred(A, ind)")>]
    [<DataRow("disjunction_01", "def pred T() {and(true, impl(true, false))}", "pred()")>]
    [<DataRow("disjunction_02", "def pred T() {or(true,false)}", "pred()")>]
    [<DataRow("disjunction_03", "def pred T() {or(x,or(y,z))}", "pred(undef, undef, undef)")>]
    [<DataRow("disjunction_04", "def pred T(x:ind, y:obj) {or(x,y)}", "pred(ind, obj)")>]
    [<DataRow("ex_01", "def pred T() { ex x:obj { is(x,M) } }", "pred()")>]
    [<DataRow("ex_02", "def pred T(a:pred) { ex x:obj { and (a, is(x,M)) } }", "pred()")>]
    [<DataRow("ex_03", "def pred T(a:func) { ex x:obj { and (a, is(x,M)) } }", "pred()")>]
    [<DataRow("ex_04", "def pred T(a:obj) { ex x:obj { and (a, is(x,M)) } }", "pred(obj)")>]
    [<DataRow("exn_01", "def pred T() { exn$1 x:obj { is(x,M) } }", "pred()")>]
    [<DataRow("exn_02", "def pred T(a:pred) { exn$1 x:obj { and (a, is(x,M)) } }", "pred()")>]
    [<DataRow("exn_03", "def pred T(a:func) { exn$1 x:obj { and (a, is(x,M)) } }", "pred()")>]
    [<DataRow("exn_04", "def pred T(a:obj) { exn$1 x:obj { and (a, is(x,M)) } }", "pred(obj)")>]
    [<DataRow("all_01", "def pred T() { all x:obj { is(x,M) } }", "pred()")>]
    [<DataRow("all_02", "def pred T(a:pred) { all x:obj { and (a, is(x,M)) } }", "pred()")>]
    [<DataRow("all_03", "def pred T(a:pred) { all x:obj { and (a, is(x,M)) } }", "pred()")>]
    [<DataRow("all_04", "def pred T(a:obj) { all x:obj { and (a, is(x,M)) } }", "pred(obj)")>]
    
    [<TestMethod>]
    member this.TestGetOpenFormulaOfExpression(no:string, fplCode, errStr:string) =
        let filename = "TestGetOpenFormulaOfExpression.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope.Values |> Seq.filter (fun fv -> fv.Name = PrimPredicateL) |> Seq.head
        let arg = pred.ArgList |> Seq.last
        match FplTypeMatcher.GetOpenFormulaOfExpression arg with
        | Some result ->
            Assert.AreEqual<string>(errStr, result.Type SignatureType.Type)
        | None ->
            Assert.IsFalse(true, $"wrong test parameters:{arg.Type SignatureType.Mixed} is neither a predicate, nor a functional term")

    // ModusPonens and (p, impl (p,q) )
    [<DataRow("MP_01", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(true, impl(true, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01a", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: or(true, impl(true, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01b", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(true, xor(true, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01c", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(true, impl(ex x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01d", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(ex x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01e", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, impl(all x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01f", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(exn$1 x:obj {is(x,N)}, impl(exn$1 x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01g", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(and(exn$1 x:obj {is(x,N)}, true), impl(and(exn$1 x:obj {is(x,N)}, true), false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01h", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(not (ex x:obj {is(x,N)}), impl(not (ex x:obj {is(x,N)}), false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01i", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(true, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01j", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(ex x:obj {is(x,N)}, xor(true,false))) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01k", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(iif(true, ex x:obj {is(x,N)}), impl(iif(true, ex x:obj {is(x,N)}), false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01l", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(all x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01m", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(is(A,N), impl(is(A,N), false)) 2. 1, byinf M |- false }""")>]
    [<DataRow("MP_01n", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(is(A,N), impl(is(N,A), false)) 2. 1, byinf M |- false }""")>]

    // AndCummutative and(p,q) 
    [<DataRow("AndC_01", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- and(false,true) }""")>]
    [<DataRow("AndC_02", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf AndCummutative |- and(false,true) }""")>]
    [<DataRow("AndC_03", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- and(not(all x:obj {is(x,N)}), all x:obj {is(x,N)}) }""")>]

    // OrCummutative or(p,q) 
    [<DataRow("OrC_01", """inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf OrCummutative |- or(false,true) }""")>]
    [<DataRow("OrC_02", """inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf OrCummutative |- or(false,true) }""")>]
    [<DataRow("OrC_03", """inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: or(ex x:obj {is(x,N)}, iif(true,false)) 2. 1, byinf OrCummutative |- or(iif(true,false), ex x:obj {is(x,N)}) }""")>]

    // XorCummutative xor(p,q) 
    [<DataRow("XorC_01", """inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(true,false) 2. 1, byinf XorCummutative |- xor(false,true) }""")>]
    [<DataRow("XorC_02", """inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(true,true) 2. 1, byinf XorCummutative |- xor(true,true) }""")>]
    [<DataRow("XorC_02a", """inf XorX{dec q,s:pred; pre:xor(q,s) con:true} thm T {true} proof T$1 {1: true 2. 1, byinf XorX |- false }""")>]
    [<DataRow("XorC_03", """inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(and(ex x:obj {is(x,N)}, true), impl(true,false)) 2. 1, byinf XorCummutative |- xor(impl(true,false), and(ex x:obj {is(x,N)}, true)) }""")>]

    // IifCummutative iif(p,q)
    [<DataRow("IifC_01", """inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: iif(true,false) 2. 1, byinf IifCummutative |- iif(false,true) }""")>]
    [<DataRow("IifC_02", """inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf IifCummutative |- iif(false,true) }""")>]
    [<DataRow("IifC_03", """inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: iif(iif(true,ex x:obj {is(x,N)}), xor(true,false)) 2. 1, byinf IifCummutative |- iif(xor(true,false), iif(true,ex x:obj {is(x,N)})) }""")>]

    // AndAssociative and(p,and(q,s)) 
    [<DataRow("AndA_01", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true, and(false, true)) 2. 1, byinf AndAssociative |- and(and(true,false), true) }""")>]
    [<DataRow("AndA_02", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(and(true,false), true) 2. 1, byinf AndAssociative |- and(and(true,false), true) }""")>]
    [<DataRow("AndA_02a", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true,and(true,false)) 2. 1, byinf AndAssociative |- and(and(true,false), true) }""")>]
    [<DataRow("AndA_03", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, and(ex y:obj {is(y,M)}, true)) 2. 1, byinf AndAssociative |- and(and(all x:obj {is(x,N)}, ex y:obj {is(y,M)}), true) }""")>]

    // OrAssociative or(p,or(q,s))
    [<DataRow("OrA_01", """inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(true, or(false, true)) 2. 1, byinf OrAssociative |- or(or(true,false), true) }""")>]
    [<DataRow("OrA_02", """inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(or(true,false), true) 2. 1, byinf OrAssociative |- or(or(true,false), true) }""")>]
    [<DataRow("OrA_03", """inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(ex x:obj {is(x,N)}, or(iif(true,false), true)) 2. 1, byinf OrAssociative |- or(or(ex x:obj {is(x,N)}, iif(true,false)), true) }""")>]

    // XorAssociative xor(p,xor(q,s))
    [<DataRow("XorA_01", """inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(true, xor(false, true)) 2. 1, byinf XorAssociative |- xor(xor(true,false), true) }""")>]
    [<DataRow("XorA_02", """inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(xor(true,false), true) 2. 1, byinf XorAssociative |- xor(xor(true,false), true) }""")>]
    [<DataRow("XorA_03", """inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(iif(true,ex x:obj {is(x,N)}), xor(xor(true,false), true)) 2. 1, byinf XorAssociative |- xor(xor(iif(true,ex x:obj {is(x,N)}), xor(true,false)), true) }""")>]

    // IifAssociative iif(p,iif(q,s))
    [<DataRow("IifA_01", """inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(true, iif(false, true)) 2. 1, byinf IifAssociative |- iif(iif(true,false), true) }""")>]
    [<DataRow("IifA_02", """inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(iif(true,false), true) 2. 1, byinf IifAssociative |- iif(iif(true,false), true) }""")>]
    [<DataRow("IifA_03", """inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(ex x:obj {is(x,N)}, iif(iif(true,false), ex y:obj {is(y,M)})) 2. 1, byinf IifAssociative |- iif(iif(ex x:obj {is(x,N)}, iif(true,false)), ex y:obj {is(y,M)}) }""")>]

    // FalseAndAbsorbing and(false,p)
    [<DataRow("FAbs_01", """inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(false,true) 2. 1, byinf FalseAndAbsorbing |- false }""")>]
    [<DataRow("FAbs_02", """inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf FalseAndAbsorbing |- false }""")>]
    [<DataRow("FAbs_03", """inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(false, ex x:obj {is(x,N)}) 2. 1, byinf FalseAndAbsorbing |- false }""")>]

    // TrueOrAbsorbing or(true,p)
    [<DataRow("TOrAbs_01", """inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf TrueOrAbsorbing |- true }""")>]
    [<DataRow("TOrAbs_02", """inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(false,true) 2. 1, byinf TrueOrAbsorbing |- true }""")>]
    [<DataRow("TOrAbs_03", """inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(true, ex x:obj {is(x,N)}) 2. 1, byinf TrueOrAbsorbing |- true }""")>]

    // OrAndAbsorbing: pre: or(p, and(p, q))
    [<DataRow("OrAndAbsorbing_01", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(and(true, not(false)), and(and(true, not(false)), xor(false, true))) 2. 1, byinf OrAndAbsorbing |- true }")>]
    [<DataRow("OrAndAbsorbing_02", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(iif(true, not(false)), and(iif(true, not(false)), ex n:obj { is(n, N) })) 2. 1, byinf OrAndAbsorbing |- true }")>]
    [<DataRow("OrAndAbsorbing_03", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(all x:obj { is(x, N) }, and(all x:obj { is(x, N) }, impl(true, false))) 2. 1, byinf OrAndAbsorbing |- true }")>]

    // AndOrAbsorbing: pre: and(p, or(p, q))
    [<DataRow("AndOrAbsorbing_01", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(xor(true, false), or(xor(true, false), not(false))) 2. 1, byinf AndOrAbsorbing |- true }")>]
    [<DataRow("AndOrAbsorbing_02", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(not(and(true, false)), or(not(and(true, false)), all y:obj { is(y, N) })) 2. 1, byinf AndOrAbsorbing |- true }")>]
    [<DataRow("AndOrAbsorbing_03", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(ex n:obj { is(n, N) }, or(ex n:obj { is(n, N) }, iif(true, false))) 2. 1, byinf AndOrAbsorbing |- true }")>]

    // AndTrueNeutral: pre: and(true, p)
    [<DataRow("AndTrueNeutral_01", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, and(xor(true, false), not(false))) 2. 1, byinf AndTrueNeutral |- true }")>]
    [<DataRow("AndTrueNeutral_02", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, ex n:obj { is(n, N) }) 2. 1, byinf AndTrueNeutral |- true }")>]
    [<DataRow("AndTrueNeutral_03", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, iif(or(true, false), not(false))) 2. 1, byinf AndTrueNeutral |- true }")>]

    // OrFalseNeutral: pre: or(false, p)
    [<DataRow("OrFalseNeutral_01", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, and(true, not(false))) 2. 1, byinf OrFalseNeutral |- true }")>]
    [<DataRow("OrFalseNeutral_02", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, ex n:obj { is(n, N) }) 2. 1, byinf OrFalseNeutral |- true }")>]
    [<DataRow("OrFalseNeutral_03", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, iif(true, xor(false, true))) 2. 1, byinf OrFalseNeutral |- true }")>]

    // AndInversion: pre: and(p, not p)
    [<DataRow("AndInversion_01", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(or(true, false), not(or(true, false))) 2. 1, byinf AndInversion |- true }")>]
    [<DataRow("AndInversion_02", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, not(all x:obj { is(x, N) })) 2. 1, byinf AndInversion |- true }")>]
    [<DataRow("AndInversion_03", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(ex n:obj { is(n, N) }, not(ex n:obj { is(n, N) })) 2. 1, byinf AndInversion |- true }")>]

    // OrInversion: pre: or(p, not p)
    [<DataRow("OrInversion_01", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(impl(true, false), not(impl(true, false))) 2. 1, byinf OrInversion |- true }")>]
    [<DataRow("OrInversion_02", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(xor(true, false), not(xor(true, false))) 2. 1, byinf OrInversion |- true }")>]
    [<DataRow("OrInversion_03", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(all x:obj { is(x, N) }, not(all x:obj { is(x, N) })) 2. 1, byinf OrInversion |- true }")>]

    // AndIdempotence: pre: and(p, p)
    [<DataRow("AndIdempotence_01", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(or(true, false), or(true, false)) 2. 1, byinf AndIdempotence |- true }")>]
    [<DataRow("AndIdempotence_02", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, all x:obj { is(x, N) }) 2. 1, byinf AndIdempotence |- true }")>]
    [<DataRow("AndIdempotence_03", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(iif(true, false), iif(true, false)) 2. 1, byinf AndIdempotence |- true }")>]

    // OrIdempotence: pre: or(p, p)
    [<DataRow("OrIdempotence_01", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(and(true, false), and(true, false)) 2. 1, byinf OrIdempotence |- true }")>]
    [<DataRow("OrIdempotence_02", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(ex n:obj { is(n, N) }, ex n:obj { is(n, N) }) 2. 1, byinf OrIdempotence |- true }")>]
    [<DataRow("OrIdempotence_03", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(xor(true, false), xor(true, false)) 2. 1, byinf OrIdempotence |- true }")>]

    // OrAndDistributiveUnpack: pre: or(p, and(q, s))
    [<DataRow("OrAndDistributiveUnpack_01", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(iif(true, false), and(and(true, not(false)), xor(false, true))) 2. 1, byinf OrAndDistributiveUnpack |- true }")>]
    [<DataRow("OrAndDistributiveUnpack_02", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(all x:obj { is(x, N) }, and(ex y:obj { is(y, M) }, or(true, false))) 2. 1, byinf OrAndDistributiveUnpack |- true }")>]
    [<DataRow("OrAndDistributiveUnpack_03", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(not(xor(true, false)), and(iif(true, true), all z:obj { is(z, N) })) 2. 1, byinf OrAndDistributiveUnpack |- true }")>]

    // AndOrDistributivePack: pre: and(or(p, q), or(p, s))
    [<DataRow("AndOrDistributivePack_01", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(iif(true, false), xor(true, false)), or(iif(true, false), and(ex n:obj { is(n, N) }, true))) 2. 1, byinf AndOrDistributivePack |- true }")>]
    [<DataRow("AndOrDistributivePack_02", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(all x:obj { is(x, N) }, not(false)), or(all x:obj { is(x, N) }, xor(true, false))) 2. 1, byinf AndOrDistributivePack |- true }")>]
    [<DataRow("AndOrDistributivePack_03", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(not(true), impl(true, false)), or(not(true), ex y:obj { is(y, N) })) 2. 1, byinf AndOrDistributivePack |- true }")>]

    // AndOrDistributiveUnpack: pre: and(p, or(q, s))
    [<DataRow("AndOrDistributiveUnpack_01", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(iif(true, ex x:obj { is(x, N) }), or(and(true, not(false)), xor(false, true))) 2. 1, byinf AndOrDistributiveUnpack |- true }")>]
    [<DataRow("AndOrDistributiveUnpack_02", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, or(ex y:obj { is(y, M) }, impl(true, false))) 2. 1, byinf AndOrDistributiveUnpack |- true }")>]
    [<DataRow("AndOrDistributiveUnpack_03", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(not(xor(true, false)), or(iif(true, false), and(exn$1 z:obj { is(z, N) }, true))) 2. 1, byinf AndOrDistributiveUnpack |- true }")>]

    // OrAndDistributivePack: pre: or(and(p, q), and(p, s))
    [<DataRow("OrAndDistributivePack_01", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(all x:obj { is(x, N) }, not(false)), and(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf OrAndDistributivePack |- true }")>]
    [<DataRow("OrAndDistributivePack_02", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(iif(true, false), and(true, not(false))), and(iif(true, false), impl(true, false))) 2. 1, byinf OrAndDistributivePack |- true }")>]
    [<DataRow("OrAndDistributivePack_03", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(ex x:obj { is(x, N) }, or(true, false)), and(ex x:obj { is(x, N) }, xor(true, false))) 2. 1, byinf OrAndDistributivePack |- true }")>]

    // DeMorganAndUnpack: pre: not and(p, q)
    [<DataRow("DeMorganAndUnpack_01", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(not(true), xor(true, false)) 2. 1, byinf DeMorganAndUnpack |- true }")>]
    [<DataRow("DeMorganAndUnpack_02", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(all x:obj { is(x, N) }, ex y:obj { not is(y, M) }) 2. 1, byinf DeMorganAndUnpack |- true }")>]
    [<DataRow("DeMorganAndUnpack_03", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(iif(true, false), impl(true, false)) 2. 1, byinf DeMorganAndUnpack |- true }")>]

    // DeMorganOrPack: pre: or(not p, not q)
    [<DataRow("DeMorganOrPack_01", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (and(true, false)), not (ex x:obj { is(x, N) })) 2. 1, byinf DeMorganOrPack |- true }")>]
    [<DataRow("DeMorganOrPack_02", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (iif(true, false)), not (all x:obj { is(x, N) })) 2. 1, byinf DeMorganOrPack |- true }")>]
    [<DataRow("DeMorganOrPack_03", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (xor(true, false)), not (impl(true, false))) 2. 1, byinf DeMorganOrPack |- true }")>]

    // DeMorganOrUnpack: pre: not or(p, q)
    [<DataRow("DeMorganOrUnpack_01", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(iif(true, false), ex x:obj { is(x, N) }) 2. 1, byinf DeMorganOrUnpack |- true }")>]
    [<DataRow("DeMorganOrUnpack_02", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(all x:obj { is(x, N) }, xor(true, false)) 2. 1, byinf DeMorganOrUnpack |- true }")>]
    [<DataRow("DeMorganOrUnpack_03", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(impl(true, false), and(true, false)) 2. 1, byinf DeMorganOrUnpack |- true }")>]

    // DeMorganAndPack: pre: and(not p, not q)
    [<DataRow("DeMorganAndPack_01", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (iif(true, false)), not (ex x:obj { is(x, N) })) 2. 1, byinf DeMorganAndPack |- true }")>]
    [<DataRow("DeMorganAndPack_02", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (all x:obj { is(x, N) }), not (xor(true, false))) 2. 1, byinf DeMorganAndPack |- true }")>]
    [<DataRow("DeMorganAndPack_03", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (impl(true, false)), not (and(true, false))) 2. 1, byinf DeMorganAndPack |- true }")>]

    // NotDouble: pre: not not p
    [<DataRow("NotDouble_01", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (and(true, xor(false, true))) 2. 1, byinf NotDouble |- true }")>]
    [<DataRow("NotDouble_02", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (ex x:obj { not is(x, N) }) 2. 1, byinf NotDouble |- true }")>]
    [<DataRow("NotDouble_03", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (iif(true, ex y:obj { is(y, M) })) 2. 1, byinf NotDouble |- true }")>]

    // ImplUnpack2Or: pre: impl(p, q)
    [<DataRow("ImplUnpack2Or_01", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf ImplUnpack2Or |- true }")>]
    [<DataRow("ImplUnpack2Or_02", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(not (xor(true, false)), iif(true, false)) 2. 1, byinf ImplUnpack2Or |- true }")>]
    [<DataRow("ImplUnpack2Or_03", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(and(true, not(false)), or(ex x:obj { is(x, N) }, true)) 2. 1, byinf ImplUnpack2Or |- true }")>]

    // OrPack2Impl: pre: or(not p, q)
    [<DataRow("OrPack2Impl_01", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf OrPack2Impl |- true }")>]
    [<DataRow("OrPack2Impl_02", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not (iif(true, false)), and(true, false)) 2. 1, byinf OrPack2Impl |- true }")>]
    [<DataRow("OrPack2Impl_03", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not (xor(true, false)), impl(true, false)) 2. 1, byinf OrPack2Impl |- true }")>]

    // IifUnpack2Or: pre: iif(p, q)
    [<DataRow("IifUnpack2Or_01", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf IifUnpack2Or |- true }")>]
    [<DataRow("IifUnpack2Or_02", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(not (xor(true, false)), and(true, false)) 2. 1, byinf IifUnpack2Or |- true }")>]
    [<DataRow("IifUnpack2Or_03", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(iif(true, false), xor(true, false)) 2. 1, byinf IifUnpack2Or |- true }")>]

    // IifUnpack2And: pre: iif(p, q)
    [<DataRow("IifUnpack2And_01", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(ex x:obj { is(x, N) }, iif(true, false)) 2. 1, byinf IifUnpack2And |- true }")>]
    [<DataRow("IifUnpack2And_02", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(all x:obj { is(x, N) }, xor(true, false)) 2. 1, byinf IifUnpack2And |- true }")>]
    [<DataRow("IifUnpack2And_03", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(not (and(true, false)), or(true, false)) 2. 1, byinf IifUnpack2And |- true }")>]

    // OrPack2Iif: pre: or(and(not p, not q), and(p, q))
    [<DataRow("OrPack2Iif_01", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) }), and(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf OrPack2Iif |- true }")>]
    [<DataRow("OrPack2Iif_02", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not iif(true, false), not xor(true, false)), and(iif(true, false), xor(true, false))) 2. 1, byinf OrPack2Iif |- true }")>]
    [<DataRow("OrPack2Iif_03", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not (impl(true, false)), not (and(true, false))), and(impl(true, false), and(true, false))) 2. 1, byinf OrPack2Iif |- true }")>]

    // AndPack2Iif: pre: and(or(not p, q), or(p, not q))
    [<DataRow("AndPack2Iif_01", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not all x:obj { is(x, N) }, ex y:obj { is(y, M) }), or(all x:obj { is(x, N) }, not ex z:obj { is(z, K) })) 2. 1, byinf AndPack2Iif |- true }")>]
    [<DataRow("AndPack2Iif_02", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not iif(true, false), xor(true, false)), or(iif(true, false), not xor(true, false))) 2. 1, byinf AndPack2Iif |- true }")>]
    [<DataRow("AndPack2Iif_03", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not (impl(true, false)), and(true, false)), or(impl(true, false), not and(true, false))) 2. 1, byinf AndPack2Iif |- true }")>]

    // XorUnpack2Or: pre: xor(p, q)
    [<DataRow("XorUnpack2Or_01", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf XorUnpack2Or |- true }")>]
    [<DataRow("XorUnpack2Or_02", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(not iif(true, false), and(true, false)) 2. 1, byinf XorUnpack2Or |- true }")>]
    [<DataRow("XorUnpack2Or_03", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(iif(true, false), xor(true, false)) 2. 1, byinf XorUnpack2Or |- true }")>]

    // XorUnpack2And: pre: xor(p, q)
    [<DataRow("XorUnpack2And_01", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(and(ex x:obj { is(x, N) }, true), iif(true, false)) 2. 1, byinf XorUnpack2And |- true }")>]
    [<DataRow("XorUnpack2And_02", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(not (all x:obj { is(x, N) }), or(true, false)) 2. 1, byinf XorUnpack2And |- true }")>]
    [<DataRow("XorUnpack2And_03", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(iif(true, true), not (xor(false, true))) 2. 1, byinf XorUnpack2And |- true }")>]

    // AndPack2Xor: pre: and(or(not p, not q), or(p, q))
    [<DataRow("AndPack2Xor_01", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) }), or(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf AndPack2Xor |- true }")>]
    [<DataRow("AndPack2Xor_02", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not iif(true, false), not xor(true, false)), or(iif(true, false), xor(true, false))) 2. 1, byinf AndPack2Xor |- true }")>]
    [<DataRow("AndPack2Xor_03", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not (impl(true, false)), not (and(true, false))), or(impl(true, false), and(true, false))) 2. 1, byinf AndPack2Xor |- true }")>]

    // OrPack2Xor: pre: or(and(not p, q), and(p, not q))
    [<DataRow("OrPack2Xor_01", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not all x:obj { is(x, N) }, ex y:obj { is(y, M) }), and(all x:obj { is(x, N) }, not ex z:obj { is(z, K) })) 2. 1, byinf OrPack2Xor |- true }")>]
    [<DataRow("OrPack2Xor_02", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not iif(true, false), xor(true, false)), and(iif(true, false), not xor(true, false))) 2. 1, byinf OrPack2Xor |- true }")>]
    [<DataRow("OrPack2Xor_03", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not (impl(true, false)), and(true, false)), and(impl(true, false), not and(true, false))) 2. 1, byinf OrPack2Xor |- true }")>]

    // Proceeding2Results: pre: p, q
    [<DataRow("Proceeding2Results_01", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: all x:obj { is(x, N) } 2: ex y:obj { is(y, M) } 3. 1, 2, byinf Proceeding2Results |- true }")>]
    [<DataRow("Proceeding2Results_02", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: iif(true, false) 2: not (xor(true, false)) 3. 1, 2, byinf Proceeding2Results |- true }")>]
    [<DataRow("Proceeding2Results_03", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: and(true, false) 2: impl(true, false) 3. 1, 2, byinf Proceeding2Results |- true }")>]

    // Proceeding3Results: pre: p, q, s
    [<DataRow("Proceeding3Results_01", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: all x:obj { is(x, N) } 2: ex y:obj { is(y, M) } 3: iif(true, false) 4. 1, 2, 3, byinf Proceeding3Results |- true }")>]
    [<DataRow("Proceeding3Results_02", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true, not(false)) 2: xor(true, false) 3: all z:obj { is(z, K) } 4. 1, 2, 3, byinf Proceeding3Results |- true }")>]
    [<DataRow("Proceeding3Results_03", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: not (xor(true, false)) 2: impl(true, false) 3: exn$1 u:obj { is(u, L) } 4. 1, 2, 3, byinf Proceeding3Results |- true }")>]

    // AndUnpack2NotOr: pre: and(p, q)
    [<DataRow("AndUnpack2NotOr_01", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf AndUnpack2NotOr |- true }")>]
    [<DataRow("AndUnpack2NotOr_02", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(iif(true, false), xor(true, false)) 2. 1, byinf AndUnpack2NotOr |- true }")>]
    [<DataRow("AndUnpack2NotOr_03", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(not (ex x:obj { is(x, N) }), impl(true, false)) 2. 1, byinf AndUnpack2NotOr |- true }")>]

    // NotOrPack2And: pre: not (or(not p, not q))
    [<DataRow("NotOrPack2And_01", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) })) 2. 1, byinf NotOrPack2And |- true }")>]
    [<DataRow("NotOrPack2And_02", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not iif(true, false), not xor(true, false))) 2. 1, byinf NotOrPack2And |- true }")>]
    [<DataRow("NotOrPack2And_03", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not impl(true, false), not and(true, false))) 2. 1, byinf NotOrPack2And |- true }")>]

    // AndUnpack2NotImpl: pre: and(p, q)
    [<DataRow("AndUnpack2NotImpl_01", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, iif(true, false)) 2. 1, byinf AndUnpack2NotImpl |- true }")>]
    [<DataRow("AndUnpack2NotImpl_02", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(ex x:obj { is(x, M) }, not (xor(true, false))) 2. 1, byinf AndUnpack2NotImpl |- true }")>]
    [<DataRow("AndUnpack2NotImpl_03", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(not (impl(true, false)), all z:obj { is(z, K) }) 2. 1, byinf AndUnpack2NotImpl |- true }")>]

    // NotImplPack2And: pre: not (impl(p, not q))
    [<DataRow("NotImplPack2And_01", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(all x:obj { is(x, N) }, not ex y:obj { is(y, M) })) 2. 1, byinf NotImplPack2And |- true }")>]
    [<DataRow("NotImplPack2And_02", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(iif(true, false), not xor(true, false))) 2. 1, byinf NotImplPack2And |- true }")>]
    [<DataRow("NotImplPack2And_03", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(and(true, false), not iif(true, false))) 2. 1, byinf NotImplPack2And |- true }")>]

    // NotImpl2And: pre: not (impl(p, q))
    [<DataRow("NotImpl2And_01", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf NotImpl2And |- true }")>]
    [<DataRow("NotImpl2And_02", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(iif(true, false), xor(true, false))) 2. 1, byinf NotImpl2And |- true }")>]
    [<DataRow("NotImpl2And_03", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(and(true, false), not (ex x:obj { is(x, N) }))) 2. 1, byinf NotImpl2And |- true }")>]

    // And2NotImpl: pre: and(p, not q)
    [<DataRow("And2NotImpl_01", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, not ex y:obj { is(y, M) }) 2. 1, byinf And2NotImpl |- true }")>]
    [<DataRow("And2NotImpl_02", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(iif(true, false), not xor(true, false)) 2. 1, byinf And2NotImpl |- true }")>]
    [<DataRow("And2NotImpl_03", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(not (impl(true, false)), not all z:obj { is(z, K) }) 2. 1, byinf And2NotImpl |- true }")>]

    // NotIif2Or: pre: not (iif(p, q))
    [<DataRow("NotIif2Or_01", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf NotIif2Or |- true }")>]
    [<DataRow("NotIif2Or_02", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(iif(true, false), xor(true, false))) 2. 1, byinf NotIif2Or |- true }")>]
    [<DataRow("NotIif2Or_03", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(not (and(true, false)), impl(true, false))) 2. 1, byinf NotIif2Or |- true }")>]

    // Or2NotIif: pre: or(and(p, not q), and(not p, q))
    [<DataRow("Or2NotIif_01", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(all x:obj { is(x, N) }, not ex y:obj { is(y, M) }), and(not all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf Or2NotIif |- true }")>]
    [<DataRow("Or2NotIif_02", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(iif(true, false), not xor(true, false)), and(not iif(true, false), xor(true, false))) 2. 1, byinf Or2NotIif |- true }")>]
    [<DataRow("Or2NotIif_03", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(impl(true, false), not all z:obj { is(z, K) }), and(not (impl(true, false)), all z:obj { is(z, K) })) 2. 1, byinf Or2NotIif |- true }")>]

    // NotXor2Or: pre: not (xor(p, q))
    [<DataRow("NotXor2Or_01", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf NotXor2Or |- true }")>]
    [<DataRow("NotXor2Or_02", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(iif(true, false), and(true, false))) 2. 1, byinf NotXor2Or |- true }")>]
    [<DataRow("NotXor2Or_03", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(not (impl(true, false)), xor(true, false))) 2. 1, byinf NotXor2Or |- true }")>]

    // Or2NotXor: pre: or(and(p, q), and(not p, not q))
    [<DataRow("Or2NotXor_01", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(all x:obj { is(x, N) }, ex y:obj { is(y, M) }), and(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) })) 2. 1, byinf Or2NotXor |- true }")>]
    [<DataRow("Or2NotXor_02", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(iif(true, false), xor(true, false)), and(not iif(true, false), not xor(true, false))) 2. 1, byinf Or2NotXor |- true }")>]
    [<DataRow("Or2NotXor_03", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(impl(true, false), and(true, false)), and(not (impl(true, false)), not (and(true, false)))) 2. 1, byinf Or2NotXor |- true }")>]

    // NotAll2ExNot: pre: not all x1:tpl { p(x1) }
    [<DataRow("NotAll2ExNot_01", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all x1:obj { is(x1, N) } 2. 1, byinf NotAll2ExNot |- true }")>]
    [<DataRow("NotAll2ExNot_02", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all n:Nat { impl(is(n, N), xor(true, false)) } 2. 1, byinf NotAll2ExNot |- true }")>]
    [<DataRow("NotAll2ExNot_03", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all y:obj { not is(y, M) } 2. 1, byinf NotAll2ExNot |- true }")>]

    // ExNot2NotAll: pre: ex x:tpl{not p(x)}
    [<DataRow("ExNot2NotAll_01", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex x:obj { not iif(is(x,N), true) } 2. 1, byinf ExNot2NotAll |- true }")>]
    [<DataRow("ExNot2NotAll_02", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex n:Nat { not and(is(n,N), xor(true,false)) } 2. 1, byinf ExNot2NotAll |- true }")>]
    [<DataRow("ExNot2NotAll_03", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex y:obj { not (all z:obj { impl(is(z,N), false) }) } 2. 1, byinf ExNot2NotAll |- true }")>]

    // NotEx2AllNot: pre: not ex x:tpl{p(x)}
    [<DataRow("NotEx2AllNot_01", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex x:obj { iif(is(x,N), false) } 2. 1, byinf NotEx2AllNot |- true }")>]
    [<DataRow("NotEx2AllNot_02", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex n:Nat { and(not is(n,N), xor(true,false)) } 2. 1, byinf NotEx2AllNot |- true }")>]
    [<DataRow("NotEx2AllNot_03", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex y:obj { not (ex z:obj { is(z,M) }) } 2. 1, byinf NotEx2AllNot |- true }")>]

    // AllNot2ExNot: pre: all x:tpl{not p(x)}
    [<DataRow("AllNot2ExNot_01", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all x:obj { not iif(is(x,N), true) } 2. 1, byinf AllNot2ExNot |- true }")>]
    [<DataRow("AllNot2ExNot_02", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all n:Nat { not (and(is(n,N), xor(true,false))) } 2. 1, byinf AllNot2ExNot |- true }")>]
    [<DataRow("AllNot2ExNot_03", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all y:obj { not (ex z:obj { is(z,M) }) } 2. 1, byinf AllNot2ExNot |- true }")>]

    // OrUnpack2Impl: pre: or(p,q)
    [<DataRow("OrUnpack2Impl_01", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(iif(is(A,N), true), ex x:obj { is(x,M) }) 2. 1, byinf OrUnpack2Impl |- true }")>]
    [<DataRow("OrUnpack2Impl_02", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(all x:obj { is(x,N) }, and(true, not false)) 2. 1, byinf OrUnpack2Impl |- true }")>]
    [<DataRow("OrUnpack2Impl_03", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(xor(true,false), impl(all z:obj { is(z,N) }, false)) 2. 1, byinf OrUnpack2Impl |- true }")>]

    // ImplPack2Or: pre: impl(not p,q)
    [<DataRow("ImplPack2Or_01", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (and(is(A,N), true)), ex x:obj { is(x,M) }) 2. 1, byinf ImplPack2Or |- true }")>]
    [<DataRow("ImplPack2Or_02", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (xor(true,false)), iif(all x:obj { is(x,N) }, false)) 2. 1, byinf ImplPack2Or |- true }")>]
    [<DataRow("ImplPack2Or_03", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (iif(true,false)), or(ex y:obj { is(y,M) }, true)) 2. 1, byinf ImplPack2Or |- true }")>]

    // ModusTollens: pre: not q, impl(p,q)
    [<DataRow("ModusTollens_01", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (iif(true,false)) 2: impl(all x:obj { is(x,N) }, iif(true,false)) 3. 1, 2, byinf ModusTollens |- true }")>]
    [<DataRow("ModusTollens_02", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not ex x:obj { is(x,N) } 2: impl(xor(true,false), all y:obj { is(y,M) }) 3. 1, 2, byinf ModusTollens |- true }")>]
    [<DataRow("ModusTollens_03", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: impl(iif(true,false), xor(true,false)) 3. 1, 2, byinf ModusTollens |- true }")>]

    // HypotheticalSyllogism: pre: impl(p,q), impl(q,s)
    [<DataRow("HypotheticalSyllogism_01", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(all x:obj { is(x,N) }, ex y:obj { is(y,M) }) 2: impl(ex z:obj { is(z,M) }, xor(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }")>]
    [<DataRow("HypotheticalSyllogism_02", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), iif(iif(true,false), true)) 2: impl(all u:obj { is(u,K) }, iif(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }")>]
    [<DataRow("HypotheticalSyllogism_03", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), iif(iif(true,false), true)) 2: impl(all u:obj { is(u,K) }, iif(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }")>]

    // DisjunctiveSyllogism: pre: not p, or(p,q)
    [<DataRow("DisjunctiveSyllogism_01", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (iif(true,false)) 2: or(all x:obj { is(x,N) }, ex y:obj { is(y,M) }) 3. 1, 2, byinf DisjunctiveSyllogism |- true }")>]
    [<DataRow("DisjunctiveSyllogism_02", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not ex x:obj { is(x,N) } 2: or(iif(true,false), xor(true,false)) 2. 1, 3, byinf DisjunctiveSyllogism |- true }")>]
    [<DataRow("DisjunctiveSyllogism_03", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: or(not (iif(true,false)), impl(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }")>]

    // ExistsByExample: pre: p(c)
    [<DataRow("ExistsByExample_01", "inf ExistsByExample{dec p:pred(c:obj); pre:p(c) con:ex x:tpl{p(x)}} thm T {dec a:obj; true} proof T$1 {1: iif(is(a,N), true) 2. 1, byinf ExistsByExample |- true }")>]
    [<DataRow("ExistsByExample_02", "inf ExistsByExample{dec p:pred(); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: and(ex x:obj { is(x,M) }, iif(true,false)) 2. 1, byinf ExistsByExample |- true }")>]
    [<DataRow("ExistsByExample_02a", "inf ExistsByExample{dec p:pred(c:tpl); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {dec a:tpl; 1: and(is(a,M) , (a = $1)) 2. 1, byinf ExistsByExample |- true }")>]
    [<DataRow("ExistsByExample_02b", """def pred Equal(x,y:tpl) infix "=" 0 { delegate.Equal(x,y) } inf ExistsByExample{dec p:pred(d:obj, c:tpl); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {dec a:tpl x:obj; 1: and(is(x,M) , (a = $1)) 2. 1, byinf ExistsByExample |- true }""")>]
    [<DataRow("ExistsByExample_03", "inf ExistsByExample{dec p:pred(); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: xor(all z:obj { is(z,K) }, not (xor(true,false))) 2. 1, byinf ExistsByExample |- true }")>]

    // Contraposition: pre: impl(not p, not q)
    [<DataRow("Contraposition_01", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not all x:obj { is(x,N) }, not (ex y:obj { is(y,M) })) 2. 1, byinf Contraposition |- true }")>]
    [<DataRow("Contraposition_02", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not (iif(true,false)), not (xor(true,false))) 2. 1, byinf Contraposition |- true }")>]
    [<DataRow("Contraposition_03", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not (and(is(A,N), true)), not (iif(true,false))) 2. 1, byinf Contraposition |- true }")>]

    // WeakeningRule: pre: p
    [<DataRow("WeakeningRule_01", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: iif(true, ex x:obj { is(x,N) }) 2. 1, byinf WeakeningRule |- true }")>]
    [<DataRow("WeakeningRule_02", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: all x:obj { is(x,N) } 2. 1, byinf WeakeningRule |- true }")>]
    [<DataRow("WeakeningRule_03", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: and(not (xor(true,false)), iif(true,false)) 2. 1, byinf WeakeningRule |- true }")>]

    // PrenexUnpackAndEx: pre: and(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndEx_01", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackAndEx |- true }")>]
    [<DataRow("PrenexUnpackAndEx_02", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(all z:obj { is(z,K) }, ex y:obj { xor(is(y,M), true) }) 2. 1, byinf PrenexUnpackAndEx |- true }")>]
    [<DataRow("PrenexUnpackAndEx_03", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackAndEx |- true }")>]

    // PrenexPackExAnd: pre: ex x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackExAnd_01", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { and(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExAnd |- true }")>]
    [<DataRow("PrenexPackExAnd_02", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { and(all y:obj { is(y,K) }, xor(is(n,M), true)) } 2. 1, byinf PrenexPackExAnd |- true }")>]
    [<DataRow("PrenexPackExAnd_03", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { and(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExAnd |- true }")>]

    // PrenexUnpackAndAll: pre: and(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndAll_01", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackAndAll |- true }")>]
    [<DataRow("PrenexUnpackAndAll_02", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(ex y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackAndAll |- true }")>]
    [<DataRow("PrenexUnpackAndAll_03", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackAndAll |- true }")>]

    // PrenexPackAllAnd: pre: all x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackAllAnd_01", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { and(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllAnd |- true }")>]
    [<DataRow("PrenexPackAllAnd_02", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { and(ex y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllAnd |- true }")>]
    [<DataRow("PrenexPackAllAnd_03", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { and(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllAnd |- true }")>]

    // PrenexUnpackOrEx: pre: or(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrEx_01", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackOrEx |- true }")>]
    [<DataRow("PrenexUnpackOrEx_02", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackOrEx |- true }")>]
    [<DataRow("PrenexUnpackOrEx_03", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackOrEx |- true }")>]

    // PrenexPackExOr: pre: ex x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackExOr_01", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { or(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExOr |- true }")>]
    [<DataRow("PrenexPackExOr_02", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { or(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExOr |- true }")>]
    [<DataRow("PrenexPackExOr_03", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { or(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExOr |- true }")>]

    // PrenexUnpackOrAll: pre: or(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrAll_01", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackOrAll |- true }")>]
    [<DataRow("PrenexUnpackOrAll_02", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(ex y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackOrAll |- true }")>]
    [<DataRow("PrenexUnpackOrAll_03", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackOrAll |- true }")>]

    // PrenexPackAllOr: pre: all x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackAllOr_01", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { or(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllOr |- true }")>]
    [<DataRow("PrenexPackAllOr_02", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { or(ex y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllOr |- true }")>]
    [<DataRow("PrenexPackAllOr_03", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { or(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllOr |- true }")>]

    // PrenexUnpackImplEx: pre: impl(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplEx_01", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackImplEx |- true }")>]
    [<DataRow("PrenexUnpackImplEx_02", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackImplEx |- true }")>]
    [<DataRow("PrenexUnpackImplEx_03", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackImplEx |- true }")>]

    // PrenexPackExImpl: pre: ex x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackExImpl_01", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { impl(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExImpl |- true }")>]
    [<DataRow("PrenexPackExImpl_02", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { impl(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExImpl |- true }")>]
    [<DataRow("PrenexPackExImpl_03", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { impl(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExImpl |- true }")>]

    // PrenexUnpackImplAll: pre: impl(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplAll_01", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackImplAll |- true }")>]
    [<DataRow("PrenexUnpackImplAll_02", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(all y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackImplAll |- true }")>]
    [<DataRow("PrenexUnpackImplAll_03", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackImplAll |- true }")>]

    // PrenexPackAllImpl: pre: all x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackAllImpl_01", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { impl(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllImpl |- true }")>]
    [<DataRow("PrenexPackAllImpl_02", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { impl(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllImpl |- true }")>]
    [<DataRow("PrenexPackAllImpl_03", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { impl(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllImpl |- true }")>]

    // PrenexUnpackIifEx: pre: iif(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifEx_01", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackIifEx |- true }")>]
    [<DataRow("PrenexUnpackIifEx_02", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackIifEx |- true }")>]
    [<DataRow("PrenexUnpackIifEx_03", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackIifEx |- true }")>]

    // PrenexPackExIif: pre: ex x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackExIif_01", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { iif(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExIif |- true }")>]
    [<DataRow("PrenexPackExIif_02", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { iif(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExIif |- true }")>]
    [<DataRow("PrenexPackExIif_03", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { iif(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExIif |- true }")>]

    // PrenexUnpackIifAll: pre: iif(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifAll_01", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackIifAll |- true }")>]
    [<DataRow("PrenexUnpackIifAll_02", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(all y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackIifAll |- true }")>]
    [<DataRow("PrenexUnpackIifAll_03", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackIifAll |- true }")>]

    // PrenexPackAllIif: pre: all x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackAllIif_01", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { iif(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllIif |- true }")>]
    [<DataRow("PrenexPackAllIif_02", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { iif(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllIif |- true }")>]
    [<DataRow("PrenexPackAllIif_03", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { iif(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllIif |- true }")>]

    // PrenexUnpackXorEx: pre: xor(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorEx_01", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackXorEx |- true }")>]
    [<DataRow("PrenexUnpackXorEx_02", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackXorEx |- true }")>]
    [<DataRow("PrenexUnpackXorEx_03", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackXorEx |- true }")>]

    // PrenexPackExXor: pre: ex x:tpl{xor(p, q(x))}
    [<DataRow("PrenexPackExXor_01", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { xor(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExXor |- true }")>]
    [<DataRow("PrenexPackExXor_02", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { xor(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExXor |- true }")>]
    [<DataRow("PrenexPackExXor_03", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { xor(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExXor |- true }")>]

    // PrenexUnpackXorAll: pre: xor(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorAll_01", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackXorAll |- true }")>]
    [<DataRow("PrenexUnpackXorAll_02", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(all y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackXorAll |- true }")>]
    [<DataRow("PrenexUnpackXorAll_03", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackXorAll |- true }")>]
    [<TestMethod>]
    member this.TestGetOpenFormulaOfExpressionProofs(no:string, fplCode) =
        let filename = "TestGetOpenFormulaOfExpressionProofs"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        // expected value
        let inf = theory.Scope.Values |> Seq.filter (fun fv -> fv.Name = PrimRuleOfInference) |> Seq.head
        let expectedFormula = inf.ArgList[0].ArgList[0]
        // actual value
        let thm = theory.Scope.Values |> Seq.filter (fun fv -> fv.Name = LiteralThmL) |> Seq.head
        let prf = thm.Scope.Values |> Seq.filter (fun fv -> fv.Name = LiteralPrfL) |> Seq.head :?> FplProof 
        let firstArg = prf.OrderedArguments |> List.head
        let argInfOpt = firstArg.ArgumentInference
        match argInfOpt with
        | Some argInference ->
            let actuaLFormula = argInference.ArgList |> Seq.head
            match FplTypeMatcher.GetOpenFormulaOfExpression actuaLFormula with
            | Some actual ->
                let actualStr = actual.Type SignatureType.Type
                match FplTypeMatcher.GetOpenFormulaOfExpression expectedFormula with
                | Some expected ->
                    let expectedStr = expected.Type SignatureType.Type
                    Assert.AreEqual<string>(expectedStr, actualStr)
                | _ ->
                    Assert.IsFalse(true, $"wrong test parameters:{actualStr} is missing expected")
            | None ->
                Assert.IsFalse(true, $"wrong test parameters: missing actual formula")
        | _ ->
            Assert.IsFalse(true, $"wrong test parameters: missing proof justification")
