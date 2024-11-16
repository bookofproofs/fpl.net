namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestPrecedence() =

    [<DataRow("b01","(x + y * z = 1)")>]
    [<DataRow("b02","(x * y + z = 1)")>]
    [<DataRow("b03","(x + y = z * 1)")>]
    [<DataRow("b04","(x * y = z + 1)")>]
    [<DataRow("b05","(x = y + z * 1)")>]
    [<DataRow("b06","(x = y * z + 1)")>]
    [<TestMethod>]
    member this.TestPrecedenceInfix(var, varVal) =
        ad.Clear()
        let fplCode = sprintf """ 
                 def pred Mul infix "*" 1 (x,y: obj) {intr}
                 def pred Add infix "+" 2 (x,y: obj) {intr} 
                 def pred Eq infix "=" 1000 (x,y: obj) {intr} 
                 def pred T1() { %s };""" varVal
        let filename = "TestPrecedenceInfix.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            match var with
            | "b01" -> Assert.AreEqual<string>("=(+(x, *(y, z)), 1)", base1.Type(SignatureType.Name))
            | "b02" -> Assert.AreEqual<string>("=(+(*(x, y), z), 1)", base1.Type(SignatureType.Name))
            | "b03" -> Assert.AreEqual<string>("=(+(x, y), *(z, 1))", base1.Type(SignatureType.Name))
            | "b04" -> Assert.AreEqual<string>("=(*(x, y), +(z, 1))", base1.Type(SignatureType.Name))
            | "b05" -> Assert.AreEqual<string>("=(x, +(y, *(z, 1)))", base1.Type(SignatureType.Name))
            | "b06" -> Assert.AreEqual<string>("=(x, +(*(y, z), 1))", base1.Type(SignatureType.Name))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

