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
    [<DataRow("01a", """def pred T1() { (true <=> false) };""", "false")>]
    [<DataRow("01b", """def pred T1() { (false <=> false) };""", "true")>]
    [<DataRow("01c", """def pred T1() { (false <=> true) };""", "false")>]
    [<DataRow("01d", """def pred T1() { (true <=> true) };""", "true")>]
    [<TestMethod>]
    member this.TestInfixPrecedenceFplCommons(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons %s""" varVal
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
