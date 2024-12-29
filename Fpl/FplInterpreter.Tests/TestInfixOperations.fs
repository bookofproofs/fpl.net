namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestInfixOperations() =

    [<DataRow("""def pred T1() { dec ~x,y:obj; (x = y) }""", "true")>]
    [<DataRow("""def pred T1() { (@1 = @2) }""", "false")>]
    [<DataRow("""def pred T1() { (@1 = @1) }""", "true")>]
    [<TestMethod>]
    member this.TestEqualityPredicate(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred Equal infix "=" 0 (x,y: tpl) { del.Equal(x,y) } %s;""" varVal
        let filename = "TestEqualityPredicate"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Type(SignatureType.Repr))
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00a", """def pred T1() { (true => false) };""", "false")>]
    [<DataRow("00b", """def pred T1() { (false => false) };""", "true")>]
    [<DataRow("00c", """def pred T1() { (false => true) };""", "true")>]
    [<DataRow("00d", """def pred T1() { (true => true) };""", "true")>]
    [<TestMethod>]
    member this.TestImplicationCallsFplCommons(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons %s""" varVal
        let filename = "TestImplicationCallsFplCommons"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.ReprId)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("01a", """def pred T1() { (true <=> false) };""", "false")>]
    [<DataRow("01b", """def pred T1() { (false <=> false) };""", "true")>]
    [<DataRow("01c", """def pred T1() { (false <=> true) };""", "false")>]
    [<DataRow("01d", """def pred T1() { (true <=> true) };""", "true")>]
    [<TestMethod>]
    member this.TestEquivalenceCallsFplCommons(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons %s""" varVal
        let filename = "TestEquivalenceCallsFplCommons"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.ReprId)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(true) ) };""", "false")>]
    [<DataRow("00a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(true) ) };""", "false")>]
    [<DataRow("01", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(false) ) };""", "true")>]
    [<DataRow("01a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(false) ) };""", "true")>]
    [<TestMethod>]
    member this.TestNegationCalls(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestNegationCalls"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.ReprId)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false and false and false) };""", "false")>]
    [<DataRow("01", """def pred T1() { (true and false and false) };""", "false")>]
    [<DataRow("02", """def pred T1() { (false and true and false) };""", "false")>]
    [<DataRow("03", """def pred T1() { (true and true and false) };""", "false")>]
    [<DataRow("04", """def pred T1() { (false and false and true) };""", "false")>]
    [<DataRow("05", """def pred T1() { (true and false and true) };""", "false")>]
    [<DataRow("06", """def pred T1() { (false and true and true) };""", "false")>]
    [<DataRow("07", """def pred T1() { (true and true and true) };""", "true")>]
    [<TestMethod>]
    member this.TestConjunctionCallsFplCommons(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons %s""" varVal
        let filename = "TestConjunctionCallsFplCommons"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.ReprId)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false and false and false) };""", "false")>]
    [<DataRow("01", """def pred T1() { (true and false and false) };""", "false")>]
    [<DataRow("02", """def pred T1() { (false and true and false) };""", "false")>]
    [<DataRow("03", """def pred T1() { (true and true and false) };""", "false")>]
    [<DataRow("04", """def pred T1() { (false and false and true) };""", "false")>]
    [<DataRow("05", """def pred T1() { (true and false and true) };""", "false")>]
    [<DataRow("06", """def pred T1() { (false and true and true) };""", "false")>]
    [<DataRow("07", """def pred T1() { (true and true and true) };""", "true")>]
    [<TestMethod>]
    member this.TestConjunctionCalls(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred And infix "and" 7 (x:+ pred) { and (x) } %s""" varVal
        let filename = "TestConjunctionCalls"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.ReprId)
        | None -> 
            Assert.IsTrue(false)
