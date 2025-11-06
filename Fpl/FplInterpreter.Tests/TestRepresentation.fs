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
    [<TestMethod>]
    member this.TestRepresentationPredicate(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons 
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

    [<DataRow("00","n:=Zero()", """{"name":"Zero","base":[],"vars":[],"prtys":[]}""")>]
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
            
    [<DataRow("00","n:=Zero()", "Zero()")>]
    [<TestMethod>]
    member this.TestRepresentationReturn(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.PeanoArithmetics def func T()->Nat { dec ~n:Nat %s; return n };""" varVal
        let filename = "TestRepresentationReturn.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let fn = theory.Scope["T() -> Nat"] 
            let retStmt = fn.ArgList[0]
            Assert.AreEqual<string>(expected, retStmt.Represent())
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

    [<DataRow("($0 = $0)", LiteralTrue)>]
    [<DataRow("($1 = $2)", LiteralFalse)>]
    [<DataRow("($3 = $3)", LiteralTrue)>]
    [<DataRow("(@3 = $3)", LiteralFalse)>]
    [<DataRow("(true = false)", LiteralFalse)>]
    [<DataRow("(undef = undef)", LiteralUndef)>]
    [<DataRow("(true = true)", LiteralTrue)>]
    [<DataRow("(true = undef)", LiteralUndef)>]
    [<DataRow("(undef = true)", LiteralUndef)>]
    [<DataRow("(a = true)", LiteralUndef)>]
    [<DataRow("(i = $3)", PrimUndetermined)>]
    [<DataRow("(j = $2)", LiteralTrue)>]
    [<DataRow("(k = $2)", LiteralFalse)>]
    [<DataRow("(true = a)", LiteralUndef)>]
    [<DataRow("($3 = i)", PrimUndetermined)>]
    [<DataRow("($2 = j)", LiteralTrue)>]
    [<DataRow("($2 = k)", LiteralFalse)>]
    [<TestMethod>]
    member this.TestRepresentationEquality(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
        def pred Equal(x,y: tpl) infix "=" 50 
        {
            del.Equal(x,y)
        } 
        def pred T() { dec ~i,j,k:ind j:=$2 k:=$3; %s };""" varVal
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

    [<DataRow("00","""def pred T() { dec ~v:ind; true };""", "dec ind")>]
    [<DataRow("00a","""def pred T() { dec ~v:pred; true };""", "dec pred")>]
    [<DataRow("00b","""def pred T() { dec ~v:obj; true };""", "dec obj")>]
    [<DataRow("00c","""def pred T() { dec ~v:func; true };""", "dec func")>]
    [<DataRow("00d","""def pred T() { dec ~v:tpl; true };""", "dec tpl")>]
    [<DataRow("01a","""def pred T() { dec ~v:pred(d:ind); true };""", "dec pred(ind)")>]
    [<DataRow("01b","""def pred T() { dec ~v:pred(d:pred(e,f:obj)); true };""", "dec pred(pred(obj, obj))")>]
    [<DataRow("01c","""def pred T() { dec ~v:func(d:pred(e:obj,d,e:ind)) ->pred(d:pred(e,f:obj)); true };""", "dec func(pred(obj, ind, ind)) -> pred(pred(obj, obj))")>]
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
            uses Fpl.Commons
            
            def cl X { intr }
            
            def cl A { intr }
            def cl B: A { intr }
            def cl C: A { intr }

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
