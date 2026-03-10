namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestRepresentation() =


    [<DataRow("00",LiteralFalse, LiteralFalse)>]
    [<DataRow("01",LiteralTrue, LiteralTrue)>]
    [<DataRow("02","(x = x)", LiteralTrue)>]
    [<DataRow("02a","not(x = x)", LiteralFalse)>]
    [<DataRow("03","(x = y)", LiteralFalse)>]
    [<DataRow("04","not (x = y)", LiteralTrue)>]
    [<DataRow("05","iif ((x = y),(x = y))", LiteralTrue)>]
    [<DataRow("06","iif ((x = y),not (x = y))", LiteralFalse)>]
    [<TestMethod>]
    member this.TestRepresentationPredicate(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
            def pred Equal(x,y: tpl) infix "=" 50 
            {
                del.Equal(x,y)
            }           
            def cl A  {intr} 
            def cl B  {intr} 
            def pred T() { dec ~x,y:obj x:=A() y:=B(); %s };""" varVal
        let filename = "TestRepresentationPredicate.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","n:=Zero()", """Zero()""")>]
    [<TestMethod>]
    member this.TestRepresentationAssignment(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.PeanoArithmetics def pred T() { dec ~n:Nat %s; true };""" varVal
        let filename = "TestRepresentationAssignment.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let fn = theory.Scope["T()"] 
            let assignmentStmt = fn.ArgList[0]
            let assignee = assignmentStmt.ArgList[0]
            Assert.AreEqual<string>(expected, assignee.Represent())
        | None -> 
            Assert.IsTrue(false)
            
    [<DataRow("00","uses Fpl.PeanoArithmetics", "n:=Zero()", """Zero()""")>]
    [<DataRow("00a","def cl Nat def cl Zero:Nat", "n:=Zero()", """Zero()""")>]
    [<DataRow("01","def cl Nat", "n:=Nat()", """Nat()""")>]
    [<DataRow("02","def cl Nat def func Zero() -> Nat", "n:=Zero()", """Zero()""")>]
    [<TestMethod>]
    member this.TestRepresentationReturn(var:string, uses:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """%s def func T()->Nat { dec ~n:Nat %s; return n };""" uses varVal
        let filename = "TestRepresentationReturn.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let fn = theory.Scope["T() -> Nat"] 
            let retStmt = fn.ArgList |> Seq.rev |> Seq.head
            Assert.AreEqual<string>(expected, retStmt.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","T() -> A", "def cl A def func T() -> A;", """T()""", "A")>] // intrinsic function using Skolem representation
    [<TestMethod>]
    member this.TestRepresentationFunctionalTerms(var:string, funcTermSignature:string, fplCode, expectedRepr:string, expectedType:string) =
        ad.Clear()
        let filename = "TestRepresentationFunctionalTerms.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let fn = theory.Scope[funcTermSignature] :?> FplGenericNodeWithValue
            Assert.AreEqual<string>(expectedRepr, fn.Represent())
            match fn.Value with 
            | Some v -> Assert.AreEqual<string>(expectedType, v.Type SignatureType.Type)
            | None -> Assert.IsTrue(false, "The functional term has no value")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","(@0 = Zero())", LiteralTrue)>]
    [<TestMethod>]
    member this.TestRepresentationCases(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.PeanoArithmetics def pred T() { %s };""" varVal
        let filename = "TestRepresentationCases.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            Assert.AreEqual<string>(expected, pr.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("i01", "($0 = $0)", LiteralTrue)>]
    [<DataRow("i02", "($1 = $2)", LiteralFalse)>]
    [<DataRow("i03", "($3 = $3)", LiteralTrue)>]
    [<DataRow("e01", "(@0 = @0)", LiteralTrue)>]
    [<DataRow("e02", "(@1 = @0)", LiteralFalse)>]
    [<DataRow("e03", "(@0 = @1)", LiteralFalse)>]
    [<DataRow("e04", "(@2 = @2)", LiteralTrue)>]
    [<DataRow("e05", "(@3 = @3)", LiteralTrue)>]
    [<DataRow("e06", "(@5 = @5)", LiteralTrue)>]
    [<DataRow("e07", "(@5 = @3)", LiteralFalse)>]
    [<DataRow("pr01", "(true = false)", LiteralFalse)>]
    [<DataRow("pr03", "(undef = undef)", PrimUndetermined)>]
    [<DataRow("pr04", "(true = true)", LiteralTrue)>]
    [<DataRow("pr05", "(true = undef)", PrimUndetermined)>]
    [<DataRow("pr06", "(undef = true)", PrimUndetermined)>]
    [<DataRow("m01", "(@3 = $3)", LiteralFalse)>]
    [<DataRow("m02", "(a = true)", PrimUndetermined)>]
    [<DataRow("m03", "(i = $3)", PrimUndetermined)>]
    [<DataRow("m04", "(j = $2)", LiteralTrue)>]
    [<DataRow("m05", "(k = $2)", LiteralFalse)>]
    [<DataRow("m06", "(true = a)", PrimUndetermined)>]
    [<DataRow("m07", "($3 = i)", PrimUndetermined)>]
    [<DataRow("m08", "($2 = j)", LiteralTrue)>]
    [<DataRow("m09", "($2 = k)", LiteralFalse)>]
    [<TestMethod>]
    member this.TestRepresentationEqualityWithCases(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        def cl Nat
        def cl Zero: Nat
        def func Succ(n: Nat) -> Nat
        ext Digits x@/\d+/ -> Nat 
        {
            dec
            ~n:Nat
            cases
            (
                | (x = @0) : n := Zero() 
                | (x = @1) : n := Succ(Zero()) 
                | (x = @2) : n := Succ(Succ(Zero())) 
                ? n := Succ(delegate.Decrement(x))  
            )
            ;
            return n
        }

        def pred Equal(x,y: tpl) infix "=" 50 
        {
            del.Equal(x,y)
        } 
        def pred T() { dec ~i,j,k:ind j:=$2 k:=$3; %s };""" varVal
        let filename = "TestRepresentationEqualityWithCases.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            Assert.AreEqual<string>(expected, pr.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("i01", "($0 = $0)", LiteralTrue)>]
    [<DataRow("i02", "($1 = $2)", LiteralFalse)>]
    [<DataRow("i03", "($3 = $3)", LiteralTrue)>]
    [<DataRow("e01", "(@0 = @0)", LiteralTrue)>]
    [<DataRow("e02", "(@1 = @0)", LiteralFalse)>]
    [<DataRow("e03", "(@0 = @1)", LiteralFalse)>]
    [<DataRow("e04", "(@2 = @2)", LiteralTrue)>]
    [<DataRow("e05", "(@3 = @3)", LiteralTrue)>]
    [<DataRow("e06", "(@5 = @5)", LiteralTrue)>]
    [<DataRow("e07", "(@5 = @3)", LiteralFalse)>]
    [<DataRow("pr01", "(true = false)", LiteralFalse)>]
    [<DataRow("pr03", "(undef = undef)", PrimUndetermined)>]
    [<DataRow("pr04", "(true = true)", LiteralTrue)>]
    [<DataRow("pr05", "(true = undef)", PrimUndetermined)>]
    [<DataRow("pr06", "(undef = true)", PrimUndetermined)>]
    [<DataRow("m01", "(@3 = $3)", LiteralFalse)>]
    [<DataRow("m02", "(a = true)", PrimUndetermined)>]
    [<DataRow("m03", "(i = $3)", PrimUndetermined)>]
    [<DataRow("m04", "(j = $2)", LiteralTrue)>]
    [<DataRow("m05", "(k = $2)", LiteralFalse)>]
    [<DataRow("m06", "(true = a)", PrimUndetermined)>]
    [<DataRow("m07", "($3 = i)", PrimUndetermined)>]
    [<DataRow("m08", "($2 = j)", LiteralTrue)>]
    [<DataRow("m09", "($2 = k)", LiteralFalse)>]
    [<TestMethod>]
    member this.TestRepresentationEqualityWithMCases(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        def cl Nat
        def cl Zero: Nat
        def func Succ(n: Nat) -> Nat
        ext Digits x@/\d+/ -> Nat 
        {
            return mcases
            (
                | (x = @0) : Zero() 
                | (x = @1) : Succ(Zero()) 
                | (x = @2) : Succ(Succ(Zero())) 
                ? Succ(delegate.Decrement(x))  
            )
        }

        def pred Equal(x,y: tpl) infix "=" 50 
        {
            del.Equal(x,y)
        } 
        def pred T() { dec ~i,j,k:ind j:=$2 k:=$3; %s };""" varVal
        let filename = "TestRepresentationEqualityWithMCases.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            Assert.AreEqual<string>(expected, pr.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "@0", "Zero()")>]
    [<DataRow("01", "@1", "Succ(Zero())")>]
    [<DataRow("02", "@2", "Succ(Succ(Zero()))")>]
    [<DataRow("03", "@3", "Succ(Succ(Succ(Zero())))")>]
    [<DataRow("04", "@6", "Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))")>]
    [<TestMethod>]
    member this.TestRepresentationMCases(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        def cl Nat
        def func Zero()-> Nat
        def func Succ(n: Nat) -> Nat
        def pred Equal(x,y: tpl) infix "=" 50 
        {
            del.Equal(x,y)
        }
        
        ext Digits x@/\d+/ -> Nat 
        {
            return mcases
            (
                | (x = @0) : Zero() 
                ? Succ(self(del.Decrement(x)))  
            )
        }

 
        def func T()->Nat { return %s };""" varVal
        let filename = "TestRepresentationMCases.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let func = theory.Scope["T() -> Nat"] 
            Assert.AreEqual<string>(expected, func.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "@0", "Zero()")>]
    [<DataRow("01", "@1", "Succ(Zero())")>]
    [<DataRow("02", "@2", "Succ(Succ(Zero()))")>]
    [<DataRow("03", "@3", "Succ(Succ(Succ(Zero())))")>]
    [<DataRow("04", "@6", "Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))")>]
    [<TestMethod>]
    member this.TestRepresentationMCasesWrapper(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        def cl Nat
        def func Zero()-> Nat
        def func Succ(n: Nat) -> Nat
        def pred Equal(x,y: tpl) infix "=" 50 
        {
            del.Equal(x,y)
        }
        def func Decr(x:obj)->obj { ret del.Decrement(x) } 

        ext Digits x@/\d+/ -> Nat 
        {
            return mcases
            (
                | (x = @0) : Zero() 
                ? Succ(self(Decr(x)))  
            )
        }

 
        def func T()->Nat { return %s };""" varVal
        let filename = "TestRepresentationMCases.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let func = theory.Scope["T() -> Nat"] 
            Assert.AreEqual<string>(expected, func.Represent())
        | None -> 
            Assert.IsTrue(false)



    [<DataRow("00", "@0", """Zero()""")>]
    [<DataRow("00", "@1", "One()")>]
    [<DataRow("00", "@2", "One()")>]
    [<DataRow("00", "@4", "One()")>]
    [<DataRow("00", "@100", "One()")>]
    [<TestMethod>]
    member this.TestRepresentationMCasesSimple(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        def cl Nat
        def func Zero() -> Nat
        def func One() -> Nat
        def pred Equal(x,y: tpl) infix "=" 50 
        {
            del.Equal(x,y)
        }
        
        ext Digits x@/\d+/ -> Nat 
        {
            return mcases
            (
                | (x = @0) : Zero() 
                ? One()  
            )
        }
 
        def func T()->Nat { return %s };""" varVal
        let filename = "TestRepresentationMCases.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let func = theory.Scope["T() -> Nat"] 
            Assert.AreEqual<string>(expected, func.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","""def pred T() { dec ~v:ind; true };""", "dec ind")>]
    [<DataRow("00a","""def pred T() { dec ~v:pred; true };""", PrimUndetermined)>]
    [<DataRow("00b","""def pred T() { dec ~v:obj; true };""", "dec obj")>]
    [<DataRow("00c","""def pred T() { dec ~v:func; true };""", "dec func")>]
    [<DataRow("00d","""def pred T() { dec ~v:tpl; true };""", "dec tpl")>]
    [<DataRow("01a","""def pred T() { dec ~v:pred(d:ind); true };""", PrimUndetermined)>]
    [<DataRow("01b","""def pred T() { dec ~v:pred(d:pred(e,f:obj)); true };""", PrimUndetermined)>]
    [<DataRow("01c","""def pred T() { dec ~v:func(x:pred(y:obj,d,e:ind)) ->pred(i:pred(j,k:obj)); true };""", "dec func(pred(obj, ind, ind)) -> pred(pred(obj, obj))")>]
    [<DataRow("02","""def pred T() { dec ~v:A; true };""", "dec A")>]
    [<DataRow("02a","""def cl A {intr} def pred T() { dec ~v:A; true };""", "dec A")>]
    [<TestMethod>]
    member this.TestRepresentationUnitializedVars(var:string, fplCode, expected:string) =
        ad.Clear()
        let filename = "TestRepresentationUnitializedVars.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let v = pr.Scope["v"]
            Assert.AreEqual<string>(expected, v.Represent())
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("00","""def pred T() { dec ~v:pred v:=true; false};""", LiteralTrue)>]
    [<DataRow("01","""def pred T() { dec ~v:pred v:=false; false};""", LiteralFalse)>]
    [<DataRow("02","""def cl A {dec ~myX:obj; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec ~v:B v:=B(@2); false};""", """{"name":"B","base":[{"name":"A","base":[],"vars":[],"prtys":[]}],"vars":[],"prtys":[]}""")>]
    [<DataRow("03","""def cl A {dec ~myX:pred; ctor A(x:pred) {dec myX:=x;}} def cl B:A { ctor B(x:pred) {dec base.A(not x); } } def pred T() { dec ~v:B v:=B(true); false};""", """{"name":"B","base":[{"name":"A","base":[],"vars":[],"prtys":[]}],"vars":[],"prtys":[]}""")>]
    [<TestMethod>]
    member this.TestRepresentationItializedVars(var:string, fplCode, expected:string) =
        ad.Clear()
        let filename = "TestRepresentationUnitializedVars.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let v = pr.Scope["v"]
            Assert.AreEqual<string>(expected, v.Represent())
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("00","(@0 = A())", LiteralFalse)>]
    [<DataRow("00a","(@1 = A())", LiteralFalse)>]
    [<DataRow("00b","(@2 = A())", LiteralTrue)>]
    [<DataRow("01","(@0 = B())", LiteralTrue)>]
    [<DataRow("01a","(@1 = B())", LiteralFalse)>]
    [<DataRow("01b","(@2 = A())", LiteralFalse)>]
    [<DataRow("02","(@0 = C())", LiteralFalse)>]
    [<DataRow("02a","(@1 = C())", LiteralTrue)>]
    [<DataRow("02b","(@2 = C())", LiteralFalse)>]
    [<TestMethod>]
    member this.TestRepresentationCases2(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
            
            def cl X 
            def cl A 
            def func B() -> A 
            def func C() -> A 

            def pred Equal(x,y: tpl) infix "=" 50 
            {
                del.Equal(x,y)
            }

            ext Digits x@/\d+/ -> A 
            {
                dec
                ~n:A
                cases
                (
                    | (x = @0) : n := B() 
                    | (x = @1) : n := C() 
                    ? n := A()  
                )
                ;
                return n
            }        
        
        
        def pred T() { %s };""" varVal
        let filename = "TestRepresentationCases2.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let base1 = pr.ArgList[0]
            let result = base1.Represent()
            printf "Representation: %s\n" (result)
            Assert.AreEqual<string>(expected, result)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "@0", "0")>]
    [<DataRow("01", "@42", "42")>]
    [<TestMethod>]
    member this.TestRepresentationExtensionObj(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        ext Digits x@/\d+/ -> obj
        {
            return x
        }
        def pred T() { dec ~a:obj a:=%s; true };""" varVal
        let filename = "TestRepresentationExtensionObj.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["T()"] 
            let a = pred.Scope["a"] 
            Assert.AreEqual<string>(expected, a.Represent())
        | None -> 
            Assert.IsTrue(false)
