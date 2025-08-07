namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestRepresentation() =


    [<DataRow("00","false", "false")>]
    [<DataRow("01","true", "true")>]
    [<DataRow("02","(x = x)", "true")>]
    [<DataRow("02a","not(x = x)", "false")>]
    [<DataRow("03","(x = y)", "false")>]
    [<DataRow("04","not (x = y)", "true")>]
    [<TestMethod>]
    member this.TestRepresentationPredicate(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons 
            def cl A: obj  {intr} 
            def cl B: obj  {intr} 
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
            Assert.AreEqual<string>(expected, getType SignatureType.Repr predicateValue)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","n:=Zero()", "intr Zero:intr Nat:intr obj")>]
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
            let assignedValue = assignee.GetArgument
            match assignedValue with 
            | Some value -> Assert.AreEqual<string>(expected, getType SignatureType.Repr value)
            | None -> Assert.AreEqual<string>(expected, "no value was assigned")
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
            Assert.AreEqual<string>(expected, getType SignatureType.Repr retStmt)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","(@0 = Zero())", "true")>]
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
            Assert.AreEqual<string>(expected, getType SignatureType.Repr pr)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","""def pred T() { dec ~v:ind; true };""", "dec ind")>]
    [<DataRow("00a","""def pred T() { dec ~v:pred; true };""", "dec pred")>]
    [<DataRow("00b","""def pred T() { dec ~v:obj; true };""", "dec obj")>]
    [<DataRow("00c","""def pred T() { dec ~v:func; true };""", "dec func")>]
    [<DataRow("00d","""def pred T() { dec ~v:tpl; true };""", "dec tpl")>]
    [<DataRow("01a","""def pred T() { dec ~v:tpl(d:ind); true };""", "dec tpl(ind)")>]
    [<DataRow("01b","""def pred T() { dec ~v:tpl(d:pred(e,f:obj)); true };""", "dec tpl(pred(obj, obj))")>]
    [<DataRow("01c","""def pred T() { dec ~v:func(d:tpl(e:obj,d,e:ind)) ->tpl1(d:pred(e,f:obj)); true };""", "dec func(tpl(obj, ind, ind)) -> tpl1(pred(obj, obj))")>]
    [<DataRow("02","""def pred T() { dec ~v:A; true };""", "dec A")>]
    [<DataRow("02a","""def cl A:obj {intr} def pred T() { dec ~v:A; true };""", "dec A")>]
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
            Assert.AreEqual<string>(expected, getType SignatureType.Repr v)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","""def pred T() { dec ~v:pred v:=true; false};""", "true")>]
    [<DataRow("01","""def pred T() { dec ~v:pred v:=false; false};""", "false")>]
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
            Assert.AreEqual<string>(expected, getType SignatureType.Repr v)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("00","(@0 = A())", "false")>]
    [<DataRow("00a","(@1 = A())", "false")>]
    [<DataRow("00b","(@2 = A())", "true")>]
    [<DataRow("01","(@0 = B())", "true")>]
    [<DataRow("01a","(@1 = B())", "false")>]
    [<DataRow("01b","(@2 = A())", "false")>]
    [<DataRow("02","(@0 = C())", "false")>]
    [<DataRow("02a","(@1 = C())", "true")>]
    [<DataRow("02b","(@2 = C())", "false")>]
    [<TestMethod>]
    member this.TestRepresentationCases2(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
            uses Fpl.Commons
            
            def cl X: obj { intr }
            
            def cl A: obj { intr }
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
            let result = getType SignatureType.Repr base1
            printf "Representation: %s\n" (result)
            Assert.AreEqual<string>(expected, result)
        | None -> 
            Assert.IsTrue(false)
