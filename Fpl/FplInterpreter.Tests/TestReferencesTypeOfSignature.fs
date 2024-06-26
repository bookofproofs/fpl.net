namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestReferencesTypeOfSignature() =


    [<DataRow("base1", "def pred T() { true };")>]
    [<DataRow("base2", "def pred T() { false };")>]
    [<DataRow("base3", "def pred T() { undef };")>]
    [<DataRow("base4", "def pred T() { 1. };")>]
    // [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "def pred T() { $1 };")>]
   
    //[<DataRow("base7", "bydef Test()")>] 
    //[<DataRow("base8", "Test$1")>]
    //[<DataRow("base9", "Test$1()")>]
    //[<DataRow("base10", "Test")>]
    [<DataRow("base11", "def pred T() { v };")>]
    [<DataRow("base11v1", "def pred T() { dec ~v:obj; v };")>]
    [<DataRow("base11v2", "def pred T() { dec ~v:ind; v };")>]
    [<DataRow("base11v3", "def pred T() { dec ~v:Nat; v };")>]
    //[<DataRow("base12", "self")>]
    //[<DataRow("base13", "1")>]
    //[<DataRow("base11a", "v.x")>]
    //[<DataRow("base12a", "self.x")>]
    //[<DataRow("base10b", "Test()")>]
    //[<DataRow("base11b", "v()")>]
    //[<DataRow("base12b", "self()")>]
    //[<DataRow("base13b", "1()")>]
    [<DataRow("base10c", "def pred Test(x,y:Nat) {true} def pred T() { dec ~x,y:Nat; Test(x, y) };")>]
    //[<DataRow("base11c", "v(x, y)")>]
    //[<DataRow("base12c", "self(x, y)")>]
    //[<DataRow("base13c", "1(x, y)")>]
    //[<DataRow("base10d", "Test[x, y]")>]
    //[<DataRow("base11d", "v[x, y]")>]
    //[<DataRow("base12d", "self[x, y]")>]
    //[<DataRow("base13d", "1[x.y]")>]
    //[<DataRow("base10e", "Test(x, y).@self[a, b]")>]
    //[<DataRow("base11e", "v(x, y).x[a, b]")>]
    //[<DataRow("base12e", "self(x, y).3[a, b]")>]
    //[<DataRow("base13e", "1(x, y).T[a, b]")>]
    //[<DataRow("base10f", "Test[x, y].x(a, b)")>]
    //[<DataRow("base11f", "v[x, y].x(a, b)")>]
    //[<DataRow("base12f", "self[x, y].self(a, b)")>]
    //[<DataRow("base13f", "1[x.y].T(a, b)")>]
    //[<DataRow("base14", "∅")>]
    //[<DataRow("base15", "-x")>]
    //[<DataRow("base15a", "x'")>]
    //[<DataRow("base15b", "-x'")>]
    //[<DataRow("base16", "-(y + x = 2 * x)")>]
    //[<DataRow("base17", "(y + x' = 2 * x)'")>]
    //[<DataRow("base18", "ex x in Range(a, b), y in c, z {and (a, b, c)}")>]
    //[<DataRow("base19", "exn$1 x {all y {true}}")>]
    //[<DataRow("base20", "all x {not x}")>]
    //[<DataRow("base21", "and (x, y, z)")>]
    //[<DataRow("base21a", "not x")>]
    //[<DataRow("base21b", "not (x)")>]
    //[<DataRow("base22", "xor (x, y, z)")>]
    //[<DataRow("base23", "or (x, y, z)")>]
    //[<DataRow("base24", "iif (x, y)")>]
    //[<DataRow("base25", "impl (x, y)")>]
    //[<DataRow("base26", "is (x, Nat)")>]
    //[<DataRow("base27", "B()")>]
    //[<DataRow("base28", "C(a,b,c,d)")>]
    //[<DataRow("base29", "D(self,b,c)")>]
    //[<DataRow("base30", "B(In(x))")>]
    //[<DataRow("base31", "C(Test1(a),Test2(b,c,d))")>]
    //[<DataRow("base32", "E(true, undef, false)")>]
    
    [<TestMethod>]
    member this.TestPredicateReference(var, fplCode) =
        FplParser.parserDiagnostics.Clear()
        let filename = "TestPredicateReference"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"] 
            let base1 = pr1.ValueList[0]
            match var with
            | "base1" -> Assert.AreEqual<string list>(["pred"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["pred"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["undef"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["1."], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["del."; "Test"; "("; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["ind"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual<string list>(["bydef."; "Test"; "("; ")"], base1.TypeSignature)
            | "base8" -> Assert.AreEqual<string list>(["Test"; "ind"], base1.TypeSignature)
            | "base9" -> Assert.AreEqual<string list>(["Test"; "ind"; "("; ")"], base1.TypeSignature)
            | "base10" -> Assert.AreEqual<string list>(["Test"], base1.TypeSignature)
            | "base11" -> Assert.AreEqual<string list>(["undef"], base1.TypeSignature)
            | "base11v1" -> Assert.AreEqual<string list>(["obj"], base1.TypeSignature)
            | "base11v2" -> Assert.AreEqual<string list>(["ind"], base1.TypeSignature)
            | "base11v3" -> Assert.AreEqual<string list>(["Nat"], base1.TypeSignature)
            | "base12" -> Assert.AreEqual<string list>(["self"], base1.TypeSignature)
            | "base13" -> Assert.AreEqual<string list>(["1"], base1.TypeSignature)
            | "base11a" -> Assert.AreEqual<string list>(["undef"; "."; "undef"], base1.TypeSignature)
            | "base12a" -> Assert.AreEqual<string list>(["self"; "."; "undef"], base1.TypeSignature)
            | "base10b" -> Assert.AreEqual<string list>(["Test"; "("; ")"], base1.TypeSignature)
            | "base11b" -> Assert.AreEqual<string list>(["undef"; "("; ")"], base1.TypeSignature)
            | "base12b" -> Assert.AreEqual<string list>(["self"; "("; ")"], base1.TypeSignature)
            | "base13b" -> Assert.AreEqual<string list>(["1"; "("; ")"], base1.TypeSignature)
            | "base10c" -> Assert.AreEqual<string list>(["Test"; "("; "Nat"; "Nat"; ")"], base1.TypeSignature)
            | "base11c" -> Assert.AreEqual<string list>(["undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base12c" -> Assert.AreEqual<string list>(["self"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base13c" -> Assert.AreEqual<string list>(["1"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base10d" -> Assert.AreEqual<string list>(["Test"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base11d" -> Assert.AreEqual<string list>(["undef"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base12d" -> Assert.AreEqual<string list>(["self"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base13d" -> Assert.AreEqual<string list>(["1"; "["; "undef"; "."; "undef"; "]"], base1.TypeSignature)
            | "base10e" -> Assert.AreEqual<string list>(["Test"; "("; "undef"; "undef"; ")"; "."; "@self"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base11e" -> Assert.AreEqual<string list>(["undef"; "("; "undef"; "undef"; ")"; "."; "undef"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base12e" -> Assert.AreEqual<string list>(["self"; "("; "undef"; "undef"; ")"; "."; "3"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base13e" -> Assert.AreEqual<string list>(["1"; "("; "undef"; "undef"; ")"; "."; "T"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base10f" -> Assert.AreEqual<string list>(["Test"; "["; "undef"; "undef"; "]"; "."; "undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base11f" -> Assert.AreEqual<string list>(["undef"; "["; "undef"; "undef"; "]"; "."; "undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base12f" -> Assert.AreEqual<string list>(["self"; "["; "undef"; "undef"; "]"; "."; "self"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base13f" -> Assert.AreEqual<string list>(["1"; "["; "undef"; "."; "undef"; "]"; "."; "T"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base14" -> Assert.AreEqual<string list>(["∅"], base1.TypeSignature)
            | "base15" -> Assert.AreEqual<string list>(["-"; "("; "undef"; ")"], base1.TypeSignature)
            | "base15a" -> Assert.AreEqual<string list>(["'"; "("; "undef"; ")"], base1.TypeSignature)
            | "base15b" -> Assert.AreEqual<string list>(["'"; "("; "-"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base16" -> Assert.AreEqual<string list>(["-"; "("; "+"; "("; "undef"; "="; "("; "undef"; "*"; "("; "2"; "undef"; ")"; ")"; ")"; ")"], base1.TypeSignature)
            | "base17" -> Assert.AreEqual<string list>(["'"; "("; "+"; "("; "undef"; "="; "("; "'"; "("; "undef"; ")"; "*"; "("; "2"; "undef"; ")"; ")"; ")"; ")"], base1.TypeSignature)
            | "base18" -> Assert.AreEqual<string list>([], base1.TypeSignature)
            | "base19" -> Assert.AreEqual<string list>([], base1.TypeSignature)
            | "base20" -> Assert.AreEqual<string list>([], base1.TypeSignature)
            | "base21" -> Assert.AreEqual<string list>(["and"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base21a" -> Assert.AreEqual<string list>(["not"; "undef"], base1.TypeSignature)
            | "base21b" -> Assert.AreEqual<string list>(["not"; "undef"], base1.TypeSignature)
            | "base22" -> Assert.AreEqual<string list>(["xor"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base23" -> Assert.AreEqual<string list>(["or"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base24" -> Assert.AreEqual<string list>(["iif"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base25" -> Assert.AreEqual<string list>(["impl"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base26" -> Assert.AreEqual<string list>(["is"; "("; "undef"; "Nat"; ")"], base1.TypeSignature)
            | "base27" -> Assert.AreEqual<string list>(["B"; "("; ")"], base1.TypeSignature)
            | "base28" -> Assert.AreEqual<string list>(["C"; "("; "undef"; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base29" -> Assert.AreEqual<string list>(["D"; "("; "self"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base30" -> Assert.AreEqual<string list>(["B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base31" -> Assert.AreEqual<string list>(["C"; "("; "Test1"; "("; "undef"; ")"; "Test2"; "("; "undef"; "undef"; "undef"; ")"; ")"], base1.TypeSignature)
            | "base32" -> Assert.AreEqual<string list>(["E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    (*
    [<DataRow("base1", "base.B()")>]
    [<DataRow("base2", "base.C(a, b, c, d)")>]
    [<DataRow("base3", "base.D(self, a, b)")>]
    [<DataRow("base4", "base.B(In(x))")>]
    [<DataRow("base5", "base.C(Test1(a), Test2(b, c, d))")>]
    [<DataRow("base6", "base.E(true, undef, false)")>]
    [<TestMethod>]
    member this.TestCallConstructorParentClass(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf """
                        def cl B:obj {intr}
                        def cl C:obj {intr}
                        def cl D:obj {intr}

                        def cl A:B,C,D,E
                        {
                            ctor A(a:T1, b:func, c:ind, d:pred) 
                            {
                                dec
                                    %s
                                ;
                                self
                            }
                        }
                        ;""" varVal
        let filename = "TestCallConstructorParentClassTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string list>(["B"; "("; ")"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["C"; "("; "undef"; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["D"; "("; "self"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["C"; "("; "Test1"; "("; "undef"; ")"; "Test2"; "("; "undef"; "undef"; "undef"; ")"; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "del.B()")>]
    [<DataRow("base2", "del.C(a,b,c,d)")>]
    [<DataRow("base3", "del.D(self,b,c)")>]
    [<DataRow("base4", "del.B(In(x))")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "del.C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base7", "del.E(true, undef, false)")>] 
    [<TestMethod>]
    member this.TestDelegate(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestDelegateTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string list>(["del."; "B"; "("; ")"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["del."; "C"; "("; "undef"; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["del."; "D"; "("; "self"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["del."; "B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["del."; "Test"; "("; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["del."; "C"; "("; "Test1"; "("; "undef"; ")"; "Test2"; "("; "undef"; "undef"; "undef"; ")"; ")"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual<string list>(["del."; "E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def pred T1() {intr};""")>]
    [<DataRow("base2", """def pred infix ">" -1 T1() {intr};""")>]
    [<DataRow("base3", """def pred postfix "'" T1() {intr};""")>]
    [<DataRow("base4", """def pred prefix "-" T1() {intr};""")>]
    [<DataRow("base5", """def cl symbol "∅" T1:obj {intr};""")>]
    [<DataRow("base5a", """def cl T1:obj {intr};""")>]
    [<DataRow("base6", """def func T1()->obj {intr};""")>]
    [<DataRow("base7", """def func infix ">" -1 T1()->obj {intr};""")>]
    [<DataRow("base8", """def func postfix "'" T1()->obj {intr};""")>]
    [<DataRow("base9", """def func prefix "-" T1()->obj {intr};""")>]
    [<TestMethod>]
    member this.TestFixNotation(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestFixNotationTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = 
                if varVal.Contains "cl" then 
                    theory.Scope["T1"]
                elif varVal.Contains "func" then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["T1"], base1.TypeSignature)
            | "base5a" -> Assert.AreEqual<string list>(["T1"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | "base8" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | "base9" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
    *)