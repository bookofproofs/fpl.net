namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestParentsInSymbolTable() =


    [<DataRow("00", """;""", PrimTheory)>]
    [<DataRow("01", """def pred T() {dec ~x:pred x:=false; true};""", PrimStmtAssign)>]
    [<TestMethod>]
    member this.TestParentNodeST(varVal, fplCode, firstTypeNode) =
        ad.Clear()
        let filename = "TestParentST.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let rec findNamedItem (root:FplValue) = 
                if root.Name = firstTypeNode then 
                    Some root
                else
                    match root.Scope.Values |> Seq.tryPick findNamedItem with 
                    | Some found -> Some found
                    | _ -> root.ArgList |> Seq.tryPick findNamedItem 

            let testNode = findNamedItem st.Root
            let node = testNode.Value
            let parent = node.Parent.Value 
            match varVal, node, parent with
            | "00", :? FplTheory, :? FplRoot -> ()
            | "01", :? FplAssignment, :? FplPredicate -> ()
            | _ -> failwith($"unmatched test / node / parent combination {varVal} {node} {parent}")
        | None -> 
            Assert.IsTrue(false)


