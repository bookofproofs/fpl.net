namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestInfixOperations() =

    [<DataRow("""def pred T1() { dec ~x,y:obj; (x = y) }""", literalTrue)>]
    [<DataRow("""def pred T1() { (@1 = @2) }""", literalFalse)>]
    [<DataRow("""def pred T1() { (@1 = @1) }""", literalTrue)>]
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
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00a", """def pred T1() { (true => false) };""", literalFalse)>]
    [<DataRow("00b", """def pred T1() { (false => false) };""", literalTrue)>]
    [<DataRow("00c", """def pred T1() { (false => true) };""", literalTrue)>]
    [<DataRow("00d", """def pred T1() { (true => true) };""", literalTrue)>]
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

    [<DataRow("01a", """def pred T1() { (true <=> false) };""", literalFalse)>]
    [<DataRow("01b", """def pred T1() { (false <=> false) };""", literalTrue)>]
    [<DataRow("01c", """def pred T1() { (false <=> true) };""", literalFalse)>]
    [<DataRow("01d", """def pred T1() { (true <=> true) };""", literalTrue)>]
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

    [<DataRow("00", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(true) ) };""", literalFalse)>]
    [<DataRow("00a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(true) ) };""", literalFalse)>]
    [<DataRow("01", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(false) ) };""", literalTrue)>]
    [<DataRow("01a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(false) ) };""", literalTrue)>]
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

    [<DataRow("00", """def pred T1() { (false and false and false) };""", literalFalse)>]
    [<DataRow("01", """def pred T1() { (true and false and false) };""", literalFalse)>]
    [<DataRow("02", """def pred T1() { (false and true and false) };""", literalFalse)>]
    [<DataRow("03", """def pred T1() { (true and true and false) };""", literalFalse)>]
    [<DataRow("04", """def pred T1() { (false and false and true) };""", literalFalse)>]
    [<DataRow("05", """def pred T1() { (true and false and true) };""", literalFalse)>]
    [<DataRow("06", """def pred T1() { (false and true and true) };""", literalFalse)>]
    [<DataRow("07", """def pred T1() { (true and true and true) };""", literalTrue)>]
    [<DataRow("08a", """def pred T1() { (true and true) };""", literalTrue)>]
    [<DataRow("08b", """def pred T1() { (true and false) };""", literalFalse)>]
    [<DataRow("08c", """def pred T1() { (false and true) };""", literalFalse)>]
    [<DataRow("08d", """def pred T1() { (false and false) };""", literalFalse)>]
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
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false and false and false) };""", literalFalse)>]
    [<DataRow("01", """def pred T1() { (true and false and false) };""", literalFalse)>]
    [<DataRow("02", """def pred T1() { (false and true and false) };""", literalFalse)>]
    [<DataRow("03", """def pred T1() { (true and true and false) };""", literalFalse)>]
    [<DataRow("04", """def pred T1() { (false and false and true) };""", literalFalse)>]
    [<DataRow("05", """def pred T1() { (true and false and true) };""", literalFalse)>]
    [<DataRow("06", """def pred T1() { (false and true and true) };""", literalFalse)>]
    [<DataRow("07", """def pred T1() { (true and true and true) };""", literalTrue)>]
    [<DataRow("08a", """def pred T1() { (true and true) };""", literalTrue)>]
    [<DataRow("08b", """def pred T1() { (true and false) };""", literalFalse)>]
    [<DataRow("08c", """def pred T1() { (false and true) };""", literalFalse)>]
    [<DataRow("08d", """def pred T1() { (false and false) };""", literalFalse)>]
    [<TestMethod>]
    member this.TestConjunctionCalls(no:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred And infix literalAnd 7 (x,y: pred) { and (x,y) } %s""" varVal
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
            Assert.AreEqual<string>(expected, base1.Represent())
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T1() { (false or false or false) };""", literalFalse)>]
    [<DataRow("01", """def pred T1() { (true or false or false) };""", literalTrue)>]
    [<DataRow("02", """def pred T1() { (false or true or false) };""", literalTrue)>]
    [<DataRow("03", """def pred T1() { (true or true or false) };""", literalTrue)>]
    [<DataRow("04", """def pred T1() { (false or false or true) };""", literalTrue)>]
    [<DataRow("05", """def pred T1() { (true or false or true) };""", literalTrue)>]
    [<DataRow("06", """def pred T1() { (false or true or true) };""", literalTrue)>]
    [<DataRow("07", """def pred T1() { (true or true or true) };""", literalTrue)>]
    [<DataRow("08a", """def pred T1() { (true or true) };""", literalTrue)>]
    [<DataRow("08b", """def pred T1() { (true or false) };""", literalTrue)>]
    [<DataRow("08c", """def pred T1() { (false or true) };""", literalTrue)>]
    [<DataRow("08d", """def pred T1() { (false or false) };""", literalFalse)>]
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

    [<DataRow("00", """def pred T1() { (false xor false xor false) };""", literalFalse)>]
    [<DataRow("01", """def pred T1() { (true xor false xor false) };""", literalTrue)>]
    [<DataRow("02", """def pred T1() { (false xor true xor false) };""", literalTrue)>]
    [<DataRow("03", """def pred T1() { (true xor true xor false) };""", literalFalse)>]
    [<DataRow("04", """def pred T1() { (false xor false xor true) };""", literalTrue)>]
    [<DataRow("05", """def pred T1() { (true xor false xor true) };""", literalFalse)>]
    [<DataRow("06", """def pred T1() { (false xor true xor true) };""", literalFalse)>]
    [<DataRow("07", """def pred T1() { (true xor true xor true) };""", literalTrue)>]
    [<DataRow("08a", """def pred T1() { (true xor true) };""", literalFalse)>]
    [<DataRow("08b", """def pred T1() { (true xor false) };""", literalTrue)>]
    [<DataRow("08c", """def pred T1() { (false xor true) };""", literalTrue)>]
    [<DataRow("08d", """def pred T1() { (false xor false) };""", literalFalse)>]
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
