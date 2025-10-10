namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplPrimitives
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestReferencesTypeOfSignature() =


    [<DataRow("base1", LiteralTrue)>]
    [<DataRow("base2", LiteralFalse)>]
    [<DataRow("base3", LiteralUndef)>]
    [<DataRow("base4", "-1")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "Test$1(x)")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test$1()")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base11v1", "dec ~v:obj; v")>]
    [<DataRow("base11v2", "dec ~v:ind; v")>]
    [<DataRow("base11v3", "dec ~v:Nat; v")>]
    [<DataRow("base12", LiteralParent)>]
    [<DataRow("base13", "@1")>]
    [<DataRow("base11a", "v.x")>]
    [<DataRow("base12a", "parent.x")>]
    [<DataRow("base10b", "Test()")>]
    [<DataRow("base11b", "v()")>]
    [<DataRow("base12b", "parent()")>]
    [<DataRow("base13b", "@1()")>]
    [<DataRow("base10c", "dec ~x,y:Nat; Test(x, y)")>]
    [<DataRow("base11c", "v(x, y)")>]
    [<DataRow("base12c", "parent(x, y)")>]
    [<DataRow("base13c", "@1(x, y)")>]
    [<DataRow("base10d", "Test[x, y]")>]
    [<DataRow("base11d", "v[x, y]")>]
    [<DataRow("base12d", "parent[x, y]")>]
    [<DataRow("base13d", "@1[x.y]")>]
    [<DataRow("base10e", "Test(x, y).parent[a, b]")>]
    [<DataRow("base11e", "v(x, y).x[a, b]")>]
    [<DataRow("base12e", "parent(x, y).3[a, b]")>]
    [<DataRow("base13e", "@1(x, y).T[a, b]")>]
    [<DataRow("base10f", "Test[x, y].x(a, b)")>]
    [<DataRow("base11f", "v[x, y].x(a, b)")>]
    [<DataRow("base12f", "parent[x, y].parent(a, b)")>]
    [<DataRow("base13f", "@1[x.y].T(a, b)")>]
    [<DataRow("base14", "∅")>]
    [<DataRow("base15", "-x")>]
    [<DataRow("base15a", "x'")>]
    [<DataRow("base15b", "-x'")>]
    [<DataRow("base16", "-(y + x = @2 * x)")>]
    [<DataRow("base17", "(y + x' = @2 * x)'")>]
    [<DataRow("base18", "ex x:pred(a:T), y:C, z:obj {and (a,and(b,c))}")>]
    [<DataRow("base19", "exn$1 x:obj {all y:N {true}}")>]
    [<DataRow("base20", "all x:obj {not x}")>]
    [<DataRow("base21", "and (x, and(y, z))")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor (x, xor(y, z))")>]
    [<DataRow("base23", "or (x, or(y, z))")>]
    [<DataRow("base24", "iif (x, y)")>]
    [<DataRow("base25", "impl (x, y)")>]
    [<DataRow("base26", "is (x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a,b,c,d)")>]
    [<DataRow("base29", "D(parent,b,c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base32", "E(true, undef, false)")>]
    [<DataRow("base33", "dec ~p: pred(c: obj); p(c)")>]
    [<DataRow("base34", "is(x, Set)")>]
    
    [<TestMethod>]
    member this.TestPredicateReference(var, fplCode) =
        ad.Clear()
        let filename = "TestPredicateReferenceTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", "def pred T() { " + fplCode + " };", false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"] 
            let base1 = pr1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>(LiteralPred, base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>(LiteralPred, base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>(LiteralUndef, base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>(LiteralPred, base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("undef()", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>(LiteralInd, base1.Type(SignatureType.Type))
            | "base7" -> Assert.AreEqual<string>("Test$1(undef)", base1.Type(SignatureType.Type))
            | "base8" -> Assert.AreEqual<string>("Test$1", base1.Type(SignatureType.Type))
            | "base9" -> Assert.AreEqual<string>("Test$1()", base1.Type(SignatureType.Type))
            | "base10" -> Assert.AreEqual<string>("Test", base1.Type(SignatureType.Type))
            | "base11" -> Assert.AreEqual<string>(LiteralUndef, base1.Type(SignatureType.Type))
            | "base11v1" -> Assert.AreEqual<string>(LiteralObj, base1.Type(SignatureType.Type))
            | "base11v2" -> Assert.AreEqual<string>(LiteralInd, base1.Type(SignatureType.Type))
            | "base11v3" -> Assert.AreEqual<string>("Nat", base1.Type(SignatureType.Type))
            | "base12" -> Assert.AreEqual<string>(LiteralParent, base1.Type(SignatureType.Type))
            | "base13" -> Assert.AreEqual<string>("obj", base1.Type(SignatureType.Type))
            | "base11a" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12a" -> Assert.AreEqual<string>("parent.undef", base1.Type(SignatureType.Type))
            | "base10b" -> Assert.AreEqual<string>("Test()", base1.Type(SignatureType.Type))
            | "base11b" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12b" -> Assert.AreEqual<string>("parent()", base1.Type(SignatureType.Type))
            | "base13b" -> Assert.AreEqual<string>("obj()", base1.Type(SignatureType.Type))
            | "base10c" -> Assert.AreEqual<string>("Test(Nat, Nat)", base1.Type(SignatureType.Type))
            | "base11c" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12c" -> Assert.AreEqual<string>("parent(undef, undef)", base1.Type(SignatureType.Type))
            | "base13c" -> Assert.AreEqual<string>("obj(undef, undef)", base1.Type(SignatureType.Type))
            | "base10d" -> Assert.AreEqual<string>("Test[undef, undef]", base1.Type(SignatureType.Type))
            | "base11d" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12d" -> Assert.AreEqual<string>("parent[undef, undef]", base1.Type(SignatureType.Type))
            | "base13d" -> Assert.AreEqual<string>("obj[undef]", base1.Type(SignatureType.Type))
            | "base10e" -> Assert.AreEqual<string>("Test(undef, undef).parent[undef, undef]", base1.Type(SignatureType.Type))
            | "base11e" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12e" -> Assert.AreEqual<string>("parent(undef, undef).3[undef, undef]", base1.Type(SignatureType.Type))
            | "base13e" -> Assert.AreEqual<string>("obj(undef, undef).T[undef, undef]", base1.Type(SignatureType.Type))
            | "base10f" -> Assert.AreEqual<string>("Test[undef, undef].undef", base1.Type(SignatureType.Type))
            | "base11f" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12f" -> Assert.AreEqual<string>("parent[undef, undef].parent(undef, undef)", base1.Type(SignatureType.Type))
            | "base13f" -> Assert.AreEqual<string>("obj[undef].T(undef, undef)", base1.Type(SignatureType.Type))
            | "base14" -> Assert.AreEqual<string>("∅", base1.Type(SignatureType.Type))
            | "base15" -> Assert.AreEqual<string>("-(undef)", base1.Type(SignatureType.Type))
            | "base15a" -> Assert.AreEqual<string>("'(undef)", base1.Type(SignatureType.Type))
            | "base15b" -> Assert.AreEqual<string>("'(-(undef))", base1.Type(SignatureType.Type))
            | "base16" -> Assert.AreEqual<string>("-(*(=(+(undef, undef), obj), undef))", base1.Type(SignatureType.Type))
            | "base17" -> Assert.AreEqual<string>("'(*(=(+(undef, '(undef)), obj), undef))", base1.Type(SignatureType.Type))
            | "base18" -> Assert.AreEqual<string>("pred(pred(T), C, obj)", base1.Type(SignatureType.Type))
            | "base19" -> Assert.AreEqual<string>("pred$1(obj)", base1.Type(SignatureType.Type))
            | "base20" -> Assert.AreEqual<string>("pred(obj)", base1.Type(SignatureType.Type))
            | "base21" -> Assert.AreEqual<string>("pred(undef, pred(undef, undef))", base1.Type(SignatureType.Type))
            | "base21a" -> Assert.AreEqual<string>("pred(undef)", base1.Type(SignatureType.Type))
            | "base21b" -> Assert.AreEqual<string>("pred(undef)", base1.Type(SignatureType.Type))
            | "base22" -> Assert.AreEqual<string>("pred(undef, pred(undef, undef))", base1.Type(SignatureType.Type))
            | "base23" -> Assert.AreEqual<string>("pred(undef, pred(undef, undef))", base1.Type(SignatureType.Type))
            | "base24" -> Assert.AreEqual<string>("pred(undef, undef)", base1.Type(SignatureType.Type))
            | "base25" -> Assert.AreEqual<string>("pred(undef, undef)", base1.Type(SignatureType.Type))
            | "base26" -> Assert.AreEqual<string>("pred(undef, Nat)", base1.Type(SignatureType.Type))
            | "base27" -> Assert.AreEqual<string>("B()", base1.Type(SignatureType.Type))
            | "base28" -> Assert.AreEqual<string>("C(undef, undef, undef, undef)", base1.Type(SignatureType.Type))
            | "base29" -> Assert.AreEqual<string>("D(parent, undef, undef)", base1.Type(SignatureType.Type))
            | "base30" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(SignatureType.Type))
            | "base31" -> Assert.AreEqual<string>("C(Test1(undef), Test2(undef, undef, undef))", base1.Type(SignatureType.Type))
            | "base32" -> Assert.AreEqual<string>("E(pred, undef, pred)", base1.Type(SignatureType.Type))
            | "base33" -> Assert.AreEqual<string>("pred(obj)", base1.Type(SignatureType.Type))
            | "base34" -> Assert.AreEqual<string>("pred(undef, Set)", base1.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("base1", "base.B()")>]
    [<DataRow("base2", "base.C(a, b, c, d)")>]
    [<DataRow("base3", "base.D(parent, a, b)")>]
    [<DataRow("base4", "base.B(In(x))")>]
    [<DataRow("base5", "base.C(Test1(a), Test2(b, c, d))")>]
    [<DataRow("base6", "base.E(true, undef, false)")>]
    [<TestMethod>]
    member this.TestBaseConstructorCall(var, varVal) =
        ad.Clear()
        let fplCode = sprintf """
                        def cl B {intr}
                        def cl C {intr}
                        def cl D {intr}

                        def cl A:B,C,D,E
                        {
                            ctor A(a:T1, b:func, c:ind, d:pred) 
                            {
                                dec
                                    %s
                                ;
                                
                            }
                        }
                        ;""" varVal
        let filename = "TestBaseConstructorCallTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("B()", base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("C(T1, func, ind, pred)", base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("D(parent, T1, func)", base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("C(Test1(T1), Test2(func, ind, pred))", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("E(pred, undef, pred)", base1.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "del.B()")>]
    [<DataRow("base2", "del.C(a,b,c,d)")>]
    [<DataRow("base3", "del.D(parent,b,c)")>]
    [<DataRow("base4", "del.B(In(x))")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "del.C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base7", "del.E(true, undef, false)")>] 
    [<TestMethod>]
    member this.TestDelegate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestDelegateTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("undef()", base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("undef(undef, undef, undef, undef)", base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("undef(parent, undef, undef)", base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("undef(In(undef))", base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("undef()", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("undef(Test1(undef), Test2(undef, undef, undef))", base1.Type(SignatureType.Type))
            | "base7" -> Assert.AreEqual<string>("undef(pred, undef, pred)", base1.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""loc and(p,q) := !tex: x "=" y;;""", "pred(undef, undef)")>]
    [<DataRow("""thm T {true};""","pred")>]
    [<DataRow("""ax T {true};""", "pred")>]
    [<DataRow("""lem T {true};""", "pred")>]
    [<DataRow("""prop T {true};""", "pred")>]
    [<DataRow("""conj T {true};""", "pred")>]
    [<DataRow("""cor T$1 {true};""", "pred$1")>]
    [<DataRow("""proof T$1 {1. |- trivial};""","pred$1")>]
    [<DataRow("""def pred T(p:obj) {true};""","pred(obj)")>]
    [<DataRow("""def pred T() {true};""", "pred()")>]
    [<DataRow("""inf T {pre: true con:true};""", "pred")>]
    [<TestMethod>]
    member this.TestBlockTypeSignature(varVal, name:string) =
        ad.Clear()
        let fplCode = sprintf "%s" varVal
        let filename = "TestBlockTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let loc = theory.Scope.Values |> Seq.toList |> List.head 
            Assert.AreEqual<string>(name, loc.Type(SignatureType.Type))
        | None -> 
            Assert.IsTrue(false)
