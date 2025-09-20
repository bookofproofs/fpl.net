namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestInfixOperations() =

    [<DataRow("""def pred T1() { dec ~x,y:obj; (x = y) }""", LiteralTrue)>]
    [<DataRow("""def pred T1() { (@1 = @2) }""", LiteralFalse)>]
    [<DataRow("""def pred T1() { (@1 = @1) }""", LiteralTrue)>]
    [<TestMethod>]
    member this.TestEqualityPredicate(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred Equal (x,y: tpl) infix "=" 0 { del.Equal(x,y) } %s;""" varVal
        let filename = "TestEqualityPredicate"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let variableStack = new FplVariableStack()
            pr1.Run variableStack

            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00a", """def pred T1() { (true => false) };""", LiteralFalse)>]
    [<DataRow("00b", """def pred T1() { (false => false) };""", LiteralTrue)>]
    [<DataRow("00c", """def pred T1() { (false => true) };""", LiteralTrue)>]
    [<DataRow("00d", """def pred T1() { (true => true) };""", LiteralTrue)>]
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
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("01a", """def pred T1() { (true <=> false) };""", LiteralFalse)>]
    [<DataRow("01b", """def pred T1() { (false <=> false) };""", LiteralTrue)>]
    [<DataRow("01c", """def pred T1() { (false <=> true) };""", LiteralFalse)>]
    [<DataRow("01d", """def pred T1() { (true <=> true) };""", LiteralTrue)>]
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
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(true) ) };""", LiteralFalse)>]
    [<DataRow("00a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(true) ) };""", LiteralFalse)>]
    [<DataRow("01", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(false) ) };""", LiteralTrue)>]
    [<DataRow("01a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(false) ) };""", LiteralTrue)>]
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
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false and false and false) };""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true and false and false) };""", LiteralFalse)>]
    [<DataRow("02", """def pred T1() { (false and true and false) };""", LiteralFalse)>]
    [<DataRow("03", """def pred T1() { (true and true and false) };""", LiteralFalse)>]
    [<DataRow("04", """def pred T1() { (false and false and true) };""", LiteralFalse)>]
    [<DataRow("05", """def pred T1() { (true and false and true) };""", LiteralFalse)>]
    [<DataRow("06", """def pred T1() { (false and true and true) };""", LiteralFalse)>]
    [<DataRow("07", """def pred T1() { (true and true and true) };""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true and true) };""", LiteralTrue)>]
    [<DataRow("08b", """def pred T1() { (true and false) };""", LiteralFalse)>]
    [<DataRow("08c", """def pred T1() { (false and true) };""", LiteralFalse)>]
    [<DataRow("08d", """def pred T1() { (false and false) };""", LiteralFalse)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack


            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false and false and false) };""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true and false and false) };""", LiteralFalse)>]
    [<DataRow("02", """def pred T1() { (false and true and false) };""", LiteralFalse)>]
    [<DataRow("03", """def pred T1() { (true and true and false) };""", LiteralFalse)>]
    [<DataRow("04", """def pred T1() { (false and false and true) };""", LiteralFalse)>]
    [<DataRow("05", """def pred T1() { (true and false and true) };""", LiteralFalse)>]
    [<DataRow("06", """def pred T1() { (false and true and true) };""", LiteralFalse)>]
    [<DataRow("07", """def pred T1() { (true and true and true) };""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true and true) };""", LiteralTrue)>]
    [<DataRow("08b", """def pred T1() { (true and false) };""", LiteralFalse)>]
    [<DataRow("08c", """def pred T1() { (false and true) };""", LiteralFalse)>]
    [<DataRow("08d", """def pred T1() { (false and false) };""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestConjunctionCalls(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred And infix "and" 7 (x,y: pred) { and (x,y) } %s""" varVal
        let filename = "TestConjunctionCalls"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let variableStack = new FplVariableStack()
            pr1.Run variableStack


            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false or false or false) };""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true or false or false) };""", LiteralTrue)>]
    [<DataRow("02", """def pred T1() { (false or true or false) };""", LiteralTrue)>]
    [<DataRow("03", """def pred T1() { (true or true or false) };""", LiteralTrue)>]
    [<DataRow("04", """def pred T1() { (false or false or true) };""", LiteralTrue)>]
    [<DataRow("05", """def pred T1() { (true or false or true) };""", LiteralTrue)>]
    [<DataRow("06", """def pred T1() { (false or true or true) };""", LiteralTrue)>]
    [<DataRow("07", """def pred T1() { (true or true or true) };""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true or true) };""", LiteralTrue)>]
    [<DataRow("08b", """def pred T1() { (true or false) };""", LiteralTrue)>]
    [<DataRow("08c", """def pred T1() { (false or true) };""", LiteralTrue)>]
    [<DataRow("08d", """def pred T1() { (false or false) };""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestDisjunctionCallsFplCommons(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons %s""" varVal
        let filename = "TestDisjunctionCallsFplCommons"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false xor false xor false) };""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true xor false xor false) };""", LiteralTrue)>]
    [<DataRow("02", """def pred T1() { (false xor true xor false) };""", LiteralTrue)>]
    [<DataRow("03", """def pred T1() { (true xor true xor false) };""", LiteralFalse)>]
    [<DataRow("04", """def pred T1() { (false xor false xor true) };""", LiteralTrue)>]
    [<DataRow("05", """def pred T1() { (true xor false xor true) };""", LiteralFalse)>]
    [<DataRow("06", """def pred T1() { (false xor true xor true) };""", LiteralFalse)>]
    [<DataRow("07", """def pred T1() { (true xor true xor true) };""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true xor true) };""", LiteralFalse)>]
    [<DataRow("08b", """def pred T1() { (true xor false) };""", LiteralTrue)>]
    [<DataRow("08c", """def pred T1() { (false xor true) };""", LiteralTrue)>]
    [<DataRow("08d", """def pred T1() { (false xor false) };""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestExDisjunctionCallsFplCommons(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons %s""" varVal
        let filename = "TestDisjunctionCallsFplCommons"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors VAR00
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)
