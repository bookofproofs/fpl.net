namespace FplInterpreter.Tests
open System
open System.Collections.Generic
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open FParsec
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type SymbolTableStructure() =
    let positions = (Position("",0,0,0), Position("",0,0,0))
    let parent = new FplRoot()

    let isValidJson (input: string) =
        try
            JToken.Parse(input) |> ignore
            true
        with
        | :? JsonReaderException -> false
        | _ -> false

    let rec findNamedItem firstTypeNode identifier (infiniteLoop:HashSet<obj>) (root:FplValue) = 
        if infiniteLoop.Contains(root) then
            None
        else
            infiniteLoop.Add(root) |> ignore
            if identifier = "" then 
                if root.Name = firstTypeNode then 
                    Some root
                else
                    match root.Scope.Values |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) with 
                    | Some found -> Some found
                    | _ -> 
                        match root.ArgList |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) with 
                        | Some found -> Some found
                        | _ -> root.ValueList |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) 
            else
                let searchItem = root.Type(SignatureType.Name)
                if root.Name = firstTypeNode && searchItem = identifier then 
                    Some root
                else
                    match root.Scope.Values |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) with 
                    | Some found -> Some found
                    | _ -> 
                        match root.ArgList |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) with 
                        | Some found -> Some found
                        | _ -> root.ValueList |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) 
    let getName nodeType = 
        match nodeType with
        | "FplArgInferenceAssume" ->
            let x = new FplArgInferenceAssume(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplBase" ->
            let x = new FplBase(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplBaseConstructorCall" ->
            let x = new FplBaseConstructorCall(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplConstructor" ->
            parent.FplId <- "obj"
            let x = new FplConstructor(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplDecrement" ->
            let x = new FplDecrement(PrimDelegateDecrementL, positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplDefaultConstructor" ->
            let x = new FplDefaultConstructor(LiteralObj, positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplEquality" ->
            let x = new FplEquality(PrimDelegateEqualL, positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            let mapping = new FplMapping(positions, x)
            mapping.TypeId <- LiteralObj
            mapping.FplId <- LiteralObj
            x.ArgList.Add mapping
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByConj" ->
            let x = new FplJustificationItemByConj(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByDefVar" ->
            let x = new FplJustificationItemByDefVar(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByRefArgument" ->
            let x = new FplJustificationItemByRefArgument(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            let mapping = new FplMapping(positions, x)
            mapping.TypeId <- LiteralObj
            mapping.FplId <- LiteralObj
            x.ArgList.Add mapping
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            x.TypeId <- LiteralObj
            x.FplId <- LiteralObj
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplParent" ->
            let x = new FplParent(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplPredicateList" ->
            let x = new FplPredicateList(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplRoot" ->
            let x = new FplRoot()
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplSelf" ->
            let x = new FplSelf(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplTheory" ->
            let x = new FplTheory("", parent, "", 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplVariable" -> 
            let x = new FplVariable("x", positions, parent) 
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | "FplVariableArray" ->
            let x = new FplVariableArray("x", positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""; x.Represent(); x.Type SignatureType.Mixed]
        | _ -> 
            failwith $"Unknown node type {nodeType}"

    let testSkeleton nodeType filename fplCode identifier = 
        ad.Clear()
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let nodeName = (getName nodeType).[0]
            let infiniteLoop = new HashSet<obj>()
            let testNodeOpt = findNamedItem nodeName identifier infiniteLoop st.Root
            match testNodeOpt with 
            | Some (node:FplValue) when node.Parent.IsSome ->
                let parent = node.Parent.Value 
                (parent, node)
            | Some (node:FplValue) ->
                if node.Name = PrimRoot then 
                    Assert.IsInstanceOfType<FplRoot>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                    (node, node)
                else
                    failwith($"Nodetype {nodeType} has unexpectedly no parent.")
            | None ->
                failwith($"Nodetype {nodeType} not found in the symbol table. Test is not implemented correctly.")
        | None -> 
            failwith($"FPL code could not be interpreted due to errors {Environment.NewLine}{ad.DiagnosticsToString}")



    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBase")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDefaultConstructor")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByConj")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplParent")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplSelf")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariableArray")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        let index = 0 // name 
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(PrimArgInfAssume, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(PrimArgInfDerive, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(PrimArgInfRevoke, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(PrimArgInfTrivial, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(PrimArgL, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>(PrimAssertion, (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>(PrimAssignment, (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralAxL, (getName var).[index])
        | "FplBase" ->
            Assert.AreEqual<string>(LiteralBase, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(PrimBaseConstructorCall, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>(PrimCases, (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>(PrimCaseElse, (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>(PrimCaseSingle, (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(PrimClassL, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralConjL, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(PrimConjunction, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralCtorL, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralCorL, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(PrimDelegateDecrementL, (getName var).[index])        
        | "FplDefaultConstructor" ->
            Assert.AreEqual<string>(PrimDefaultConstructor, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(PrimDisjunction, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(PrimDelegateEqualL, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(PrimEquivalence, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(PrimExclusiveOr, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>(PrimExtensionL, (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(PrimExtensionObj, (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>(PrimForInStmt, (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>(PrimForInStmtDomain, (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>(PrimForInStmtEntity, (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(PrimFuncionalTermL, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(PrimImplication, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(PrimInstanceL, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(PrimIntrinsicInd, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(PrimIntrinsicPred, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(PrimIntrinsicTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(PrimIntrinsicUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(PrimIsOperator, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(PrimJustificationL, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>(PrimJIByAx, (getName var).[index])
        | "FplJustificationItemByConj" ->
            Assert.AreEqual<string>(PrimJIByConj, (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>(PrimJIByCor, (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>(PrimJIByDef, (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>(PrimJIByDefVar, (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>(PrimJIByInf, (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>(PrimJIByProofArgument, (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>(PrimJIByRefArgument, (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>(PrimJIByTheoremLikeStmt, (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>(PrimLanguageL, (getName var).[index])        
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralLemL, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>(LiteralLocL, (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(PrimMandatoryFunctionalTermL, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(PrimMandatoryPredicateL, (getName var).[index])        
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>(PrimMapCaseElse, (getName var).[index])        
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>(PrimMapCaseSingle, (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>(PrimMapCases, (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>(PrimMappingL, (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(PrimNegation, (getName var).[index])        
        | "FplParent" ->
            Assert.AreEqual<string>(LiteralParent, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(PrimPredicateL, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>(LiteralPreL, (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralPrfL, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralPropL, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(PrimQuantorAll, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(PrimQuantorExists, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(PrimQuantorExistsN, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>(PrimRefL, (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(PrimReturn, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>(PrimRoot, (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(PrimRuleOfInference, (getName var).[index])
        | "FplSelf" ->
            Assert.AreEqual<string>(LiteralSelf, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralThmL, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>(PrimTheoryL, (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>(PrimTranslationL, (getName var).[index])
        | "FplVariable" -> 
            Assert.AreEqual<string>(PrimVariableL, (getName var).[index])
        | "FplVariableArray" ->
            Assert.AreEqual<string>(PrimVariableArrayL, (getName var).[index])
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBase")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDefaultConstructor")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByConj")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplParent")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplSelf")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariableArray")>]
    [<TestMethod>]
    member this.TestFplBlockTypeShortName(var) =
        let index = 1 // short name 
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(PrimArg, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>(LiteralAss, (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralAx, (getName var).[index])
        | "FplBase" ->
            Assert.AreEqual<string>(LiteralBase, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(PrimClass, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralConj, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(LiteralAnd, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralCtor, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralCor, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(PrimDelegateDecrement, (getName var).[index])        
        | "FplDefaultConstructor" ->
            Assert.AreEqual<string>(LiteralCtor, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(LiteralOr, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(PrimDelegateEqual, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(LiteralIif, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(LiteralXor, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>(PrimExtension, (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(PrimFuncionalTerm, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(LiteralImpl, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(PrimInstance, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(LiteralIs, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByConj" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>(PrimLanguage, (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralLem, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>(LiteralLoc, (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(PrimMandatoryFunctionalTerm, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(PrimMandatoryPredicate, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])        
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])        
        | "FplMapCases" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>(PrimMapping, (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(LiteralNot, (getName var).[index])
        | "FplParent" ->
            Assert.AreEqual<string>(LiteralParent, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(PrimPredicate, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>(LiteralInf, (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralPrf, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralProp, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(PrimQuantor, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(PrimQuantor, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(PrimQuantor, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>(PrimRef, (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>(PrimRoot, (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(LiteralInf, (getName var).[index])
        | "FplSelf" ->
            Assert.AreEqual<string>(LiteralSelf, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralThm, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>(PrimTheory, (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>(PrimTranslation, (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>(PrimVariable, (getName var).[index])
        | "FplVariableArray" ->
            Assert.AreEqual<string>(PrimVariableArray, (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)

    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBase")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDefaultConstructor")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByConj")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplParent")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplSelf")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariableArray")>]
    [<TestMethod>]
    member this.TestFplBlockTypeFplId(var) =
        let index = 2 // FplId
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>("assign (ln 0)", (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplBase" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(LiteralAnd, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(PrimDelegateDecrementL, (getName var).[index])        
        | "FplDefaultConstructor" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(LiteralOr, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(PrimDelegateEqualL, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(LiteralIif, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(LiteralXor, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(LiteralImpl, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(LiteralIs, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByConj" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>("", (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(LiteralNot, (getName var).[index])
        | "FplParent" ->
            Assert.AreEqual<string>(LiteralParent, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(LiteralAll, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(LiteralEx, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(LiteralExN, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplSelf" ->
            Assert.AreEqual<string>(LiteralSelf, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>("x", (getName var).[index])
        | "FplVariableArray" ->
            Assert.AreEqual<string>("x", (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)


    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBase")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDefaultConstructor")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByConj")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplParent")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplSelf")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariableArray")>]
    [<TestMethod>]
    member this.TestFplBlockTypeId(var) =
        let index = 3 // TypeId
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplBase" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])        
        | "FplDefaultConstructor" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByConj" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>("", (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplParent" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplSelf" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplVariableArray" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)

    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBase")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDefaultConstructor")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByConj")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplParent")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplSelf")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariableArray")>]
    [<TestMethod>]
    member this.TestFplBlockTypeRunOrder(var) =
        let index = 4 // RunOrder
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplBase" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplConstructor" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplDefaultConstructor" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplDisjunction" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplEquality" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplEquivalence" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplExtension" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplInstance" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustification" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByConj" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapping" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplParent" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplSelf" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplVariableArray" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)


    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBase")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDefaultConstructor")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByConj")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplParent")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplSelf")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariableArray")>]
    [<TestMethod>]
    member this.TestFplBlockTypeRepresent(var) =


        let index = 5 // Represent
        match var with
        | "FplArgInferenceAssume" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplArgument" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplAssertion" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplAssignment" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplAxiom" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplBase" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplCases" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplCaseElse" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplCaseSingle" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplClass" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("dec cl obj", (getName var).[index])
        | "FplConjecture" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplConjunction" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplConstructor" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>($"{LiteralObj}()", (getName var).[index])
        | "FplCorollary" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplDecrement" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplDefaultConstructor" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>($"{LiteralObj}()", (getName var).[index])
        | "FplDisjunction" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplEquality" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplEquivalence" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplExclusiveOr" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplExtension" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmt" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>($"{LiteralDec} {LiteralObj}", (getName var).[index])
        | "FplImplication" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplInstance" ->
            Assert.IsTrue(isValidJson (getName var).[index])
            Assert.AreEqual<string>("""{"name":"obj"}""", (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplJustification" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByConj" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLanguage" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLemma" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplLocalization" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>($"{LiteralDec} {LiteralObj}", (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCases" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapping" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>($"{LiteralDec} {LiteralObj}", (getName var).[index])
        | "FplNegation" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplParent" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplPredicate" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplPredicateList" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplProof" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplProposition" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplReference" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplReturn" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplRoot" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplSelf" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplTheorem" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(PrimUndetermined, (getName var).[index])
        | "FplTheory" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplTranslation" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariable" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplVariableArray" ->
            Assert.IsFalse(isValidJson (getName var).[index])
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, $"Unimplemented test {var}")


    // todo: issue diagnostics if assuming referenced arguments
    // todo: issue diagnostics restricting assumtions only to non-compound predicates and to references to definitions of predicates
    // todo: issue diagnostics if proving an implication impl(a,b) and the first argument is not the assumption of a, and the last derived argument is not b.
    // todo: issue diagnostics if proving an equivalence iif(a,b) and the proof does not consist of two blocks, each starting with the assumption of a (resp. b) and ending with the derivation of b (resp. a)
    [<DataRow("FplArgInferenceAssume", "00", """proof T$1 {1. |- assume and(x,y) };""", "")>]
    [<DataRow("FplArgInferenceAssume", "01", """proof T$1 {1. |- assume is(x,Nat) };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplArgInferenceAssume(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplArgInferenceAssume.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplArgInferenceAssume", "00" -> 
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgInferenceAssume>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplArgInferenceAssume", "01" -> 
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgInferenceAssume>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // todo: issue diagnostics if the first argument of the proof starts with a derived argument without providing a justification
    // todo: issue diagnostics if the derived predicate is neither one of the prime predicates 'true' and 'false', nor a compound predicate, nor a reference to a definition of a predicate
    [<DataRow("FplArgInferenceDerived", "00", """proof T$1 {1. byax A |- and(x,y)};""", "")>]
    [<DataRow("FplArgInferenceDerived", "01", """proof T$1 {1. |- and(x,y)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplArgInferenceDerived(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplArgInferenceDerived.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplArgInferenceDerived", "00" -> 
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgInferenceDerived>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplArgInferenceDerived", "01" -> 
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgInferenceDerived>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplArgInferenceRevoke", "00", """proof T$1 {1. |- revoke 1};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplArgInferenceRevoke(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplArgInferenceRevoke.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplArgInferenceRevoke", "00" -> 
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgInferenceRevoke>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplArgInferenceTrivial", "00", """proof T$1 {1. |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplArgInferenceTrivial(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplArgInferenceTrivial.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplArgInferenceTrivial", "00" ->
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // justification / arginference pair
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgInferenceTrivial>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplArgument", "00", """proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplArgument", "01", """proof T$1 {1. A |- trivial};""", "")>]
    [<DataRow("FplArgument", "02", """proof T$1 {1. byax A, 2 |- trivial};""", "")>]
    [<DataRow("FplArgument", "03", """proof T$1 {1. bycor A$1 |- trivial};""", "")>]
    [<DataRow("FplArgument", "04", """proof T$1 {1. bydef A |- trivial};""", "")>]
    [<DataRow("FplArgument", "05", """proof T$1 {1. A$1:2, bydef y |- trivial};""", "")>]
    [<DataRow("FplArgument", "06", """proof T$1 {1. byinf A |- revoke 2};""", "")>]
    [<DataRow("FplArgument", "07", """proof T$1 {1. 2 |- trivial};""", "")>]
    [<DataRow("FplArgument", "08", """proof T$1 {1. bydef x |- assume and(x,y)};""", "")>]
    [<DataRow("FplArgument", "09", """proof T$1 {1.  A, byax B, bycor C$1, bydef D, E$1:2, byinf F, 2, bydef x |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplArgument(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplArgument.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplArgument", "00" 
        | "FplArgument", "01" 
        | "FplArgument", "02" 
        | "FplArgument", "03" 
        | "FplArgument", "04" 
        | "FplArgument", "05" 
        | "FplArgument", "06" 
        | "FplArgument", "07" 
        | "FplArgument", "08" 
        | "FplArgument", "09" 
            ->
            Assert.IsInstanceOfType<FplProof>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplArgument>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // todo: issue diagnostics if assertions are made outside the definitions of functional terms and definitions of classes
    // todo: issue diagnostics if assertions are made inside the definitions of functional terms and definitions of classes but do not involve the self keyword
    // todo: issue diagnostics if assertions involve the is opeperator
    [<DataRow("FplAssertion", "00", """def cl A {ctor A(){dec assert all x:A,y:obj {In(y,x)};}};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplAssertion(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplAssertion.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplAssertion", "00" ->
            Assert.IsInstanceOfType<FplConstructor>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplAssertion>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplAssignment", "00", """def pred T() {dec ~x:pred x:=false; true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplAssignment(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplAssignment.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplAssignment", "00" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplAssignment>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // reference to variable := reference to false
            Assert.AreEqual<int>(0, node.Scope.Count)
            let assignment = node :?> FplAssignment
            Assert.IsInstanceOfType<FplVariable>(assignment.Assignee.Value)
            Assert.IsInstanceOfType<FplIntrinsicPred>(assignment.AssignedValue.Value)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplAxiom", "00", """ax T {true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplAxiom(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplAxiom.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with

        | "FplAxiom", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplAxiom>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplBase", "00", """def cl A:B,C {intr};""", "")>] 
    [<DataRow("FplBase", "01", """def cl A {intr} def cl B:A {intr} def cl C:B {intr} def cl D:A,B,C {intr};""", "C")>]
    [<DataRow("FplBase", "02", """def cl A {intr} def cl B:A {intr} def cl C:B {intr} def cl D:C,E {intr};""", "E")>]
    [<TestMethod>]
    member this.TestStructureFplBase(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplBase.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplBase", "00" ->
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplBase>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplBase", "01" ->
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplBase>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplBase", "02" ->
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplBase>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplBaseConstructorCall", "00", """def cl A { ctor A() {dec base.Obj(); } };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplBaseConstructorCall(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplBaseConstructorCall.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplBaseConstructorCall", "00" -> 
            Assert.IsInstanceOfType<FplConstructor>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) // constructor
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplBaseConstructorCall>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // call to base.Obj()
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplCaseElse", "00", """def pred T(x:ind) { dec ~n:pred cases (| (x = $1) : n:=false | (x = $2) : n:=true | (x = $3) : n:=false ? n:=undef ); n };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplCaseElse(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplCaseElse.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplCaseElse", "00" ->
            Assert.IsInstanceOfType<FplCases>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplCaseElse>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplCases", "00", """def pred T(x:ind) { dec ~n:pred cases (| (x = $1) : n:=false | (x = $2) : n:=true | (x = $3) : n:=false ? n:=undef ); n };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplCases(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplCases.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplCases", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplCases>(node)
            Assert.AreEqual<int>(4, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    
    [<DataRow("FplCaseSingle", "00", """def pred T(x:ind) { dec ~n:pred cases (| (x = $1) : n:=false | (x = $2) : n:=true | (x = $3) : n:=false ? n:=undef ); n };""", "")>]
    [<DataRow("FplCaseSingle", "01", """def pred T(x:ind) { dec ~n:pred cases (| (x = $1) : n:=true n:=true n:=false | (x = $2) : n:=true | (x = $3) : n:=false ? n:=undef ); n };""", "")>]
    [<DataRow("FplCaseSingle", "02", """def pred T(x:ind) { dec ~n:pred cases (| (x = $1) : n:=true n:=false | (x = $2) : n:=true | (x = $3) : n:=false ? n:=undef ); n };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplCaseSingle(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplCaseSingle.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplCaseSingle", "00" ->
            Assert.IsInstanceOfType<FplCases>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplCaseSingle>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCaseSingle", "01" ->
            Assert.IsInstanceOfType<FplCases>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplCaseSingle>(node)
            Assert.AreEqual<int>(4, node.ArgList.Count) // 1 case + 3 statements
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCaseSingle", "02" ->
            Assert.IsInstanceOfType<FplCases>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplCaseSingle>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // 1 case + 2 statements
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplClass", "00", """def cl A {intr};""", "")>]
    // base classed not declared
    [<DataRow("FplClass", "00a", """def cl A:B,C {intr};""", "")>] 
    // base classed declared
    [<DataRow("FplClass", "01", """def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {intr};""", "D")>]
    // one constructor
    [<DataRow("FplClass", "02", """def cl A { ctor A() {} };""", "")>]
    // two constructors
    [<DataRow("FplClass", "02a", """def cl A { ctor A() {} ctor A(x,y,z:obj) {} };""", "")>]
    // intrinsic (without constructor), but with properties
    [<DataRow("FplClass", "03", """def cl A { intr prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    // with constructor and properties
    [<DataRow("FplClass", "04", """def cl A { ctor A() {} prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    // with variables constructor and properties
    [<DataRow("FplClass", "05", """def cl A { dec ~x,y:obj; ctor A() {} prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplClass(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplClass.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with

        | "FplClass", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplClass", "00a" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // base classes are added only if they were previously declared
            Assert.AreEqual<int>(1, node.Scope.Count) // 1 default constructor
        | "FplClass", "01" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(4, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // three base classes 
            Assert.AreEqual<int>(1, node.Scope.Count) // 1 default constructor
        | "FplClass", "02" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // constructor
        | "FplClass", "02a" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(2, node.Scope.Count) // 2 constructors
        | "FplClass", "03" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(5, node.Scope.Count) // 4 properties + 1 default constructor
        | "FplClass", "04" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(5, node.Scope.Count) // 1 constructor + 4 properties
        | "FplClass", "05" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(7, node.Scope.Count) // 2 variables + 1 constructor + 4 properties
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // conjecture
    [<DataRow("FplConjecture", "00", """conj T {true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplConjecture(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplConjecture.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplConjecture", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConjecture>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplConjunction", "00", """def pred T() {and (x,y)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplConjunction(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplConjunction.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplConjunction", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConjunction>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // one constructor
    [<DataRow("FplConstructor", "00", """def cl A { ctor A() {} };""", "")>]
    [<DataRow("FplConstructor", "00a", """def cl B def cl A:B { ctor A() {} };""", "")>]
    [<DataRow("FplConstructor", "00b", """def cl A:B,C { ctor A() {} };""", "")>]
    // two constructors
    [<DataRow("FplConstructor", "01", """def cl A { ctor A() {} ctor A(x,y,z:obj) {} };""", "A(x, y, z)")>]
    // one constructor with variables 
    [<DataRow("FplConstructor", "02", """def cl A { ctor A(x:obj) {dec ~y:obj; } };""", "")>]
    // with with shared variables and stmts
    [<DataRow("FplConstructor", "03", """def cl A { dec ~x,y:obj; ctor A(z:obj) {dec z:=x; } };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplConstructor(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplConstructor.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        // todo: issue diagnostics if the constructor does nothing
        | "FplConstructor", "00" -> 
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count) // no base 
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplConstructor", "00a" -> 
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) // one base 
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplConstructor", "00b" -> 
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // two bases
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplConstructor", "01" -> 
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count) // no base
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
            Assert.AreEqual<int>(3, node.Scope.Count) // three variables
        | "FplConstructor", "02" -> 
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count) // no base
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
            Assert.AreEqual<int>(2, node.Scope.Count) // two variables
        | "FplConstructor", "03" -> 
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count) // no base
            Assert.AreEqual<int>(3, parent.Scope.Count) // two variables
            Assert.IsInstanceOfType<FplConstructor>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // assignment
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // corollary with wrong parent
    [<DataRow("FplCorollary", "00a", """cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00b", """inf T {pre:true con:true} cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00c", """def cl T {intr} cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00d", """def pred T() {true} cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00e", """def func T()->obj {intr} cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00f", """loc T := !tex: "T"; cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00g", """ext T x@/\d+/->obj {ret x} cor T$1 {true};""", "")>]
    [<DataRow("FplCorollary", "00h", """proof T$1 {1. |- trivial} cor T$1$1 {true};""", "")>]
    // corollary with conjecture
    [<DataRow("FplCorollary", "01a", """conj T {true} cor T$1 {true};""", "")>]
    // corollary with axiom
    [<DataRow("FplCorollary", "01b", """ax T {true} cor T$1 {true};""", "")>]
    // corollary with theorem
    [<DataRow("FplCorollary", "01c", """thm T {true} cor T$1 {true};""", "")>]
    // corollary with lemma
    [<DataRow("FplCorollary", "01d", """lem T {true} cor T$1 {true};""", "")>]
    // corollary with proposition
    [<DataRow("FplCorollary", "01e", """prop T {true} cor T$1 {true};""", "")>]
    // corollary with corollary
    [<DataRow("FplCorollary", "01f", """thm T {true} cor T$1 {true} cor T$1$1 {true};""", "T$1$1")>]
    [<TestMethod>]
    member this.TestStructureFplCorollary(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplCorollary.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        // corollary without parent
        | "FplCorollary", "00a" ->
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node) 
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        // corollary with wrong parent
        | "FplCorollary", "00b" 
        | "FplCorollary", "00c" 
        | "FplCorollary", "00d" 
        | "FplCorollary", "00e" 
        | "FplCorollary", "00f" 
        | "FplCorollary", "00g" 
        | "FplCorollary", "00h" 
        | "FplCorollary", "00i" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node) 
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCorollary", "01a" ->
            Assert.IsInstanceOfType<FplConjecture>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node)  
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCorollary", "01b" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node)  
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCorollary", "01c" -> 
            Assert.IsInstanceOfType<FplTheorem>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node)  
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCorollary", "01d" -> 
            Assert.IsInstanceOfType<FplLemma>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node)  
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCorollary", "01e" -> 
            Assert.IsInstanceOfType<FplProposition>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node)  
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplCorollary", "01f" -> 
            Assert.IsInstanceOfType<FplCorollary>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplCorollary>(node)  
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplDecrement", "00", """def func T(x:ind)->ind {ret delegate.Decrement(x)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplDecrement(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplDecrement.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplDecrement", "00" ->
            Assert.IsInstanceOfType<FplReturn>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplDecrement>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplDefaultConstructor", "00", """def cl A def pred T() {dec ~x:A x:=A(); true};""", "")>]
    [<DataRow("FplDefaultConstructor", "01", """def cl A def func T()->A { return A() };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplDefaultConstructor(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplDefaultConstructor.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplDefaultConstructor", "00" ->
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplDefaultConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
            Assert.AreEqual<int>(1, node.ValueList.Count) // instance
        | "FplDefaultConstructor", "01" ->
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplDefaultConstructor>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
            Assert.AreEqual<int>(1, node.ValueList.Count) // instance
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplDisjunction", "00", """def pred T() {or (x,y)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplDisjunction(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplDisjunction.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplDisjunction", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplDisjunction>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplEquality", "00", """def pred T() {del.Equal(x, y)};""", "")>]
    [<DataRow("FplEquality", "01", """def pred Equal (x,y: tpl) infix "=" 50 { del.Equal(x,y) };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplEquality(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplEquality.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplEquality", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplEquality>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplEquality", "01" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplEquality>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplEquivalence", "00", """def pred T() {iif (x,y)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplEquivalence(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplEquivalence.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplEquivalence", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplEquivalence>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplExclusiveOr", "00", """def pred T() {xor(x,y)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplExclusiveOr(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplExclusiveOr.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplExclusiveOr", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExclusiveOr>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplExtension", "00", """ext Digits x@/\d+/ -> obj {ret x};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplExtension(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplExtension.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplExtension", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExtension>(parent.Scope["@Digits"])
            Assert.IsInstanceOfType<FplExtension>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplExtensionObj", "00", """ax A {@1};""", "")>]
    [<DataRow("FplExtensionObj", "00a", """def cl Nat ext D x@/\d+/ -> Nat {dec ~n:Nat; return n } ax A {@1};""", "")>]
    [<DataRow("FplExtensionObj", "00b", """def cl Nat ext D x@/\d+/ -> Nat {dec ~n:Nat; return n } ax A {and(@1,@0)};""", "")>]
    [<DataRow("FplExtensionObj", "00c", """def cl Nat ext D x@/\d+/ -> Nat {dec ~n:Nat; return n } def func Add(x,y:Nat)->obj prop K {dec ~op:Add ~n:Nat; ( op(n,@0) = n ) } ;""", "")>]
    [<DataRow("FplExtensionObj", "00d", """ax A {and(@1,@0)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplExtensionObj(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplExtensionObj.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplExtensionObj", "00" ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExtensionObj>(node)
            Assert.AreEqual<string>("obj", node.TypeId)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplExtensionObj", "00a" ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExtensionObj>(node)
            Assert.AreEqual<string>("Nat", node.TypeId)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplExtensionObj", "00b" ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExtensionObj>(node)
            Assert.AreEqual<string>("Nat", node.TypeId)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplExtensionObj", "00c" ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExtensionObj>(node)
            Assert.AreEqual<string>("Nat", node.TypeId)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplExtensionObj", "00d" ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplExtensionObj>(node)
            Assert.AreEqual<string>("obj", node.TypeId)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplForInStmt", "01", """def func Sum(list:* Nat[ind])->Nat { dec ~a:obj ~result, addend: Nat result:=Zero() for addend in list { result:=Add(result,addend) }; return result };""", "")>]
    [<DataRow("FplForInStmt", "02", """def func Sum(from, to: Nat, arr:*Nat[Nat]) -> Nat { dec ~a:obj ~i, result: Nat result:=Zero() for i in ClosedRange(from,to) { result:=Add(result,arr[i]) }; return result };""", "")>]
    [<DataRow("FplForInStmt", "03", """def func Sum() -> Nat { dec ~addend, result: Nat result:=Zero() for addend in Nat { result:=Add(result,addend) }; return result };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplForInStmt(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplForInStmt.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplForInStmt", "01" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(4, parent.Scope.Count)
            Assert.IsInstanceOfType<FplForInStmt>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // entity, domain, for body consisting of 1 stmt
            Assert.AreEqual<string>("addend", node.ArgList[0].Type SignatureType.Name)
            Assert.AreEqual<string>("list", node.ArgList[1].Type SignatureType.Name)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplForInStmt", "02" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(6, parent.Scope.Count)
            Assert.IsInstanceOfType<FplForInStmt>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // entity, domain, for body consisting of 1 stmt
            Assert.AreEqual<string>("i", node.ArgList[0].Type SignatureType.Name)
            Assert.AreEqual<string>("ClosedRange(from, to)", node.ArgList[1].Type SignatureType.Name)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplForInStmt", "03" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplForInStmt>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // entity, domain, for body consisting of 1 stmt
            Assert.AreEqual<string>("addend", node.ArgList[0].Type SignatureType.Name)
            Assert.AreEqual<string>("Nat", node.ArgList[1].Type SignatureType.Name)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplForInStmtDomain", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplForInStmtDomain(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplForInStmtDomain.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplForInStmtEntity", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplForInStmtEntity(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplForInStmtEntity.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    
    // intrinsic functional term
    [<DataRow("FplFunctionalTerm", "00", """def func T()->obj {intr};""", "")>]
    [<DataRow("FplFunctionalTerm", "00a", """def func T()->obj;""", "")>]
    [<DataRow("FplFunctionalTerm", "00b", """def func T:A()->obj;""", "")>]
    [<DataRow("FplFunctionalTerm", "00c", """def func T:A,B,C()->obj;""", "")>]
    // intrinsic functional term with variables
    [<DataRow("FplFunctionalTerm", "01", """def func T(x,y:obj)->obj {intr};""", "")>]
    // intrinsic functional term with variables and properties
    [<DataRow("FplFunctionalTerm", "02", """def func T(x,y:obj)->obj {intr prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    // non-intrinsic functional term with variables and properties
    [<DataRow("FplFunctionalTerm", "03", """def func T(x,y:obj)->obj {dec ~z:obj; return z prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    // non-intrinsic functional term with some statements 
    [<DataRow("FplFunctionalTerm", "04", """def func T(x,y:obj)->obj {dec ~z:obj z:=x y:=z; return z };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplFunctionalTerm(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplFunctionalTerm.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplFunctionalTerm", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplFunctionalTerm", "00a" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplFunctionalTerm", "00b" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // intrinsic with mapping and 1 base
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplFunctionalTerm", "00c" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(4, node.ArgList.Count) // intrinsic with mapping and 3 bases
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplFunctionalTerm", "01" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(2, node.Scope.Count) // two variables
        | "FplFunctionalTerm", "02" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(6, node.Scope.Count) // 2 variables, 4 properties
        | "FplFunctionalTerm", "03" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return statemenet
            Assert.AreEqual<int>(7, node.Scope.Count) // 3 variables, 4 properties
        | "FplFunctionalTerm", "04" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node)
            Assert.AreEqual<int>(4, node.ArgList.Count) // non-intrinsic with mapping, 2 statements, and return statement
            Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables                
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    
    [<DataRow("FplImplication", "00", """def pred T() {impl(x,y)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplImplication(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplImplication.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplImplication", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplImplication>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplInstance", "00", """def cl A {intr} def pred T() {dec ~x:A x:=A(); x};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplInstance(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplInstance.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplInstance", "00" 
        | "FplInstance", "01" ->
            Assert.IsInstanceOfType<FplVariable>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.AreEqual<int>(1, parent.ValueList.Count)
            Assert.AreEqual<FplValue>(parent.ValueList[0], node)
            Assert.IsInstanceOfType<FplInstance>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            Assert.IsTrue(isValidJson (node.Represent()), "json not valid")
            Assert.AreEqual<string>("""{"name":"A","base":[],"vars":[],"prtys":[]}""", node.Represent())
        | _ -> failwith($"unmatched test {nodeType} {varVal}")
    
    [<DataRow("FplIntrinsicInd", "00", """ax A {$1};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplIntrinsicInd(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplIntrinsicInd.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplIntrinsicInd", "00" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplIntrinsicInd>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplIntrinsicPred", "00", """ax A {true};""", "")>]
    [<DataRow("FplIntrinsicPred", "01", """ax A {false};""", "")>]
    // reference to intrinsic pred
    [<DataRow("FplIntrinsicPred", "03a", """def pred A() {dec ~x:pred x:=true; x};""", "true")>]
    [<DataRow("FplIntrinsicPred", "03b", """def pred A() {D(true)};""", "true")>]
    [<DataRow("FplIntrinsicPred", "03c", """def pred A() {dec ~x:pred x:=false; x};""", "false")>]
    [<DataRow("FplIntrinsicPred", "03d", """def pred A() {D(false)};""", "false")>]
    [<TestMethod>]
    member this.TestStructureFplIntrinsicPred(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplIntrinsicPred.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplIntrinsicPred", "00" 
        | "FplIntrinsicPred", "01" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplIntrinsicPred>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplIntrinsicPred", "03a" 
        | "FplIntrinsicPred", "03c" ->
            Assert.IsInstanceOfType<FplAssignment>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplIntrinsicPred>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplIntrinsicPred", "03b" 
        | "FplIntrinsicPred", "03d" ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplIntrinsicPred>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplIntrinsicTpl", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplIntrinsicTpl(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplIntrinsicTpl.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplIntrinsicUndef", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplIntrinsicUndef(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplIntrinsicUndef.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplIsOperator", "00", """def pred T() {is(x,pred)};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplIsOperator(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplIsOperator.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplIsOperator", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplIsOperator>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // no justification    
    [<DataRow("FplJustification", "00", """proof T$1 {1. |- trivial};""", "")>]
    // justification by theorem-like stmt  
    [<DataRow("FplJustification", "01", """proof T$1 {1. A |- trivial};""", "")>]
    // justification byax   
    [<DataRow("FplJustification", "02", """proof T$1 {1. byax A |- trivial};""", "")>]
    // justification bycor   
    [<DataRow("FplJustification", "03", """proof T$1 {1. bycor A$1 |- trivial};""", "")>]
    // justification bydef   
    [<DataRow("FplJustification", "04", """proof T$1 {1. bydef A |- trivial};""", "")>]
    // justificattion by proof argument (other proof)
    [<DataRow("FplJustification", "05", """proof T$1 {1. A$1:2 |- trivial};""", "")>]
    // justificattion byinf
    [<DataRow("FplJustification", "06", """proof T$1 {1. byinf A |- trivial};""", "")>]
    // justificattion by proof argument (this proof)
    [<DataRow("FplJustification", "07", """proof T$1 {1. 2 |- trivial};""", "")>]
    // justification bydef var  
    [<DataRow("FplJustification", "08", """proof T$1 {1. bydef x |- trivial};""", "")>]
    // all kinds
    [<DataRow("FplJustification", "09", """proof T$1 {1.  A, byax B, bycor C$1, bydef D, E$1:2, byinf F, 2, bydef x |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustification(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustification.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplJustification", "00" ->
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustification>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // no justification 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplJustification", "01" 
        | "FplJustification", "02" 
        | "FplJustification", "03" 
        | "FplJustification", "04" 
        | "FplJustification", "05" 
        | "FplJustification", "06" 
        | "FplJustification", "07" 
        | "FplJustification", "08" ->
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustification>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // contains only a single type of FplJustificationItem
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplJustification", "09" ->
            Assert.IsInstanceOfType<FplArgument>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustification>(node)
            Assert.AreEqual<int>(8, node.ArgList.Count)  // contains all possible types of FplJustificationItem
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // byax without reference
    [<DataRow("FplJustificationItemByAx", "00", """proof T$1 {1. byax A |- trivial};""", "")>]
    // byax with reference to an axiom
    [<DataRow("FplJustificationItemByAx", "00a", """ax A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    // byax with reference to a non-axiom
    [<DataRow("FplJustificationItemByAx", "00b", """inf A {pre:true con:true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00c", """thm A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00d", """prop A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00e", """lem A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00f", """conj A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00g", """def pred A() {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00h", """def func A()->pred {intr} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00i", """def cl A {intr} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00j", """ext A x@/\d+/->pred(a:obj) {ret x} proof T$1 {1. byax A |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByAx(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByAx.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplJustificationItemByAx", "00" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByAx>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)  
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplJustificationItemByAx", "00a" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByAx>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)  
            Assert.AreEqual<int>(1, node.Scope.Count) // referenced axiom
        | "FplJustificationItemByAx", "00b" 
        | "FplJustificationItemByAx", "00c" 
        | "FplJustificationItemByAx", "00d" 
        | "FplJustificationItemByAx", "00e" 
        | "FplJustificationItemByAx", "00f" 
        | "FplJustificationItemByAx", "00g" 
        | "FplJustificationItemByAx", "00h" 
        | "FplJustificationItemByAx", "00i" 
        | "FplJustificationItemByAx", "00j" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByAx>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count) // referenced to a wrong block with matching name
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

        
    [<DataRow("FplJustificationItemByConj", "00a", """conj A {true} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00b", """thm A {true} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00c", """prop A {true} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00d", """lem A {true} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00e", """inf A {pre:true con:true} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00f", """ax A {true} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00g", """def cl A {intr} proof T$1 { 100. byconj A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByConj", "00h", """def pred A() {true} proof T$1 {1. byconj A, 1 |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByConj", "00i", """def func A()->pred {intr} proof T$1 {1. byconj A, 1 |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByConj(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByConj.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplJustificationItemByConj", "00a" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByConj>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplConjecture>(node.Scope[node.FplId])
        | "FplJustificationItemByConj", "00b" 
        | "FplJustificationItemByConj", "00c" 
        | "FplJustificationItemByConj", "00d" 
        | "FplJustificationItemByConj", "00e" 
        | "FplJustificationItemByConj", "00f" 
        | "FplJustificationItemByConj", "00g"
        | "FplJustificationItemByConj", "00h"
        | "FplJustificationItemByConj", "00i" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByConj>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplJustificationItemByCor", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByCor(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByCor.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplJustificationItemByDef", "00a", """conj A {true} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00b", """thm A {true} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00c", """prop A {true} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00d", """lem A {true} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00e", """inf A {pre:true con:true} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00f", """ax A {true} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00g", """def cl A {intr} proof T$1 { 100. bydef A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByDef", "00h", """def pred A() {true} proof T$1 {1. bydef A, 1 |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByDef", "00i", """def func A()->pred {intr} proof T$1 {1. bydef A, 1 |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByDef(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByDef.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplJustificationItemByDef", "00a" 
        | "FplJustificationItemByDef", "00b" 
        | "FplJustificationItemByDef", "00c" 
        | "FplJustificationItemByDef", "00d" 
        | "FplJustificationItemByDef", "00e" 
        | "FplJustificationItemByDef", "00f" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByDef>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplJustificationItemByDef", "00g" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByDef>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplClass>(node.Scope[node.FplId])
        | "FplJustificationItemByDef", "00h" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByDef>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplPredicate>(node.Scope[node.FplId])
        | "FplJustificationItemByDef", "00i" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByDef>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplFunctionalTerm>(node.Scope[node.FplId])
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplJustificationItemByDefVar", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByDefVar(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByDefVar.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplJustificationItemByInf", "00a", """inf A {pre:true con:true} proof T$1 { 100. byinf A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByInf", "00b", """conj A {true} proof T$1 { 100. byinf A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByInf", "00c", """thm A {true} proof T$1 { 100. byinf A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByInf", "00d", """prop A {true} proof T$1 { 100. byinf A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByInf", "00e", """lem A {true} proof T$1 { 100. byinf A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByInf", "00f", """ax A {true} proof T$1 { 100. byinf A, 1 |- false };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByInf(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByInf.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplJustificationItemByInf", "00a" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByInf>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplRuleOfInference>(node.Scope[node.FplId])
        | "FplJustificationItemByInf", "00b" 
        | "FplJustificationItemByInf", "00c" 
        | "FplJustificationItemByInf", "00d" 
        | "FplJustificationItemByInf", "00e" 
        | "FplJustificationItemByInf", "00f" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByInf>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplJustificationItemByProofArgument", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByProofArgument(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByProofArgument.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplJustificationItemByRefArgument", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByRefArgument(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByRefArgument.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00a", """thm A {true} proof T$1 { 100. A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00b", """prop A {true} proof T$1 { 100. A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00c", """lem A {true} proof T$1 { 100. A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00d", """conj A {true} proof T$1 { 100. A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00e", """ax A {true} proof T$1 { 100. A, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00f", """inf A {pre:true con:true} proof T$1 { 100. A, 1 |- false };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplJustificationItemByTheoremLikeStmt(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplJustificationItemByTheoremLikeStmt.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplJustificationItemByTheoremLikeStmt", "00a" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByTheoremLikeStmt>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplTheorem>(node.Scope[node.FplId])
        | "FplJustificationItemByTheoremLikeStmt", "00b" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByTheoremLikeStmt>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplProposition>(node.Scope[node.FplId])
        | "FplJustificationItemByTheoremLikeStmt", "00c" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByTheoremLikeStmt>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.IsInstanceOfType<FplLemma>(node.Scope[node.FplId])
        | "FplJustificationItemByTheoremLikeStmt", "00d" 
        | "FplJustificationItemByTheoremLikeStmt", "00e" 
        | "FplJustificationItemByTheoremLikeStmt", "00f" ->
            Assert.IsInstanceOfType<FplJustification>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplJustificationItemByTheoremLikeStmt>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplLanguage", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplLanguage(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplLanguage.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // lemma 
    [<DataRow("FplLemma", "00", """lem T {true};""", "")>]
    // lemma with two variables
    [<DataRow("FplLemma", "01", """lem T {dec ~x,y:pred; true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplLemma(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplLemma.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplLemma", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplLemma>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplLemma", "01" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplLemma>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(2, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplLocalization", "01", """loc not x := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "")>]
    [<DataRow("FplLocalization", "02", """loc not(x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "")>]
    [<DataRow("FplLocalization", "03", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;;""", "")>]
    [<DataRow("FplLocalization", "04", """loc and(p,q) := !tex: p "\wedge" q !eng: p " and" q !ger: p " und " q;;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplLocalization(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplLocalization.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplLocalization", "01" ->
            Assert.IsInstanceOfType<FplTheory>(parent) 
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplLocalization>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(4, node.Scope.Count) // a variable + 3 languages
        | "FplLocalization", "02" ->
            Assert.IsInstanceOfType<FplTheory>(parent) 
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplLocalization>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(4, node.Scope.Count) // a variable + 3 languages
        | "FplLocalization", "03" ->
            Assert.IsInstanceOfType<FplTheory>(parent) 
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplLocalization>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(7, node.Scope.Count) // 2 variables + 5 languages
        | "FplLocalization", "04" ->
            Assert.IsInstanceOfType<FplTheory>(parent) 
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplLocalization>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(5, node.Scope.Count) // 2 variables + 3 languages

        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // intrinsic mandatory functional term with predicate parent
    [<DataRow("FplMandatoryFunctionalTerm", "00",  """def pred T() {intr prty func MandF(x:obj)->obj {intr} };""", "")>]
    // intrinsic mandatory functional term with functional term parent
    [<DataRow("FplMandatoryFunctionalTerm", "01",  """def func T()->obj {intr prty func MandF(x:obj)->obj {intr} };""", "")>]
    // intrinsic mandatory functional term with class parent
    [<DataRow("FplMandatoryFunctionalTerm", "02",  """def cl T {intr prty func MandF(x:obj)->obj {intr} };""", "")>]
    // non-intrinsic mandatory functional term with predicate parent
    [<DataRow("FplMandatoryFunctionalTerm", "03",  """def pred T() {intr prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic mandatory functional term with functional term parent
    [<DataRow("FplMandatoryFunctionalTerm", "04",  """def func T()->obj {intr prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic mandatory functional term with class parent
    [<DataRow("FplMandatoryFunctionalTerm", "05",  """def cl T {intr prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic mandatory functional term with predicate parent, shared variables and one statement
    [<DataRow("FplMandatoryFunctionalTerm", "06",  """def pred T() {dec ~y:obj; true prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    // non-intrinsic mandatory functional term with functional term parent, shared variables and one statement
    [<DataRow("FplMandatoryFunctionalTerm", "07",  """def func T()->obj {dec ~y:obj; return y prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    // non-intrinsic mandatory functional term with class parent, shared variables and one statement
    [<DataRow("FplMandatoryFunctionalTerm", "08",  """def cl T {dec ~y:obj; ctor T() {} prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplMandatoryFunctionalTerm(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplMandatoryFunctionalTerm.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplMandatoryFunctionalTerm", "00" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "01" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
            Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "02" -> 
            Assert.IsInstanceOfType<FplClass>(parent) // class parent
            Assert.AreEqual<int>(0, parent.ArgList.Count) 
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "03" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "04" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
            Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "05" -> 
            Assert.IsInstanceOfType<FplClass>(parent) // class parent
            Assert.AreEqual<int>(0, parent.ArgList.Count) // class's base class
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "06" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
            Assert.AreEqual<int>(1, parent.ArgList.Count) // predicate's value
            Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "07" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
            Assert.AreEqual<int>(2, parent.ArgList.Count) // parent's mapping, return stmt
            Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryFunctionalTerm", "08" -> 
            Assert.IsInstanceOfType<FplClass>(parent) // class parent
            Assert.AreEqual<int>(0, parent.ArgList.Count) // class's base class
            Assert.AreEqual<int>(3, parent.Scope.Count) // variable, property and constructor
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // intrinsic predicate with predicate parent
    [<DataRow("FplMandatoryPredicate", "00",  """def pred T() {intr prty pred MandF(x:obj) {intr} };""", "")>]
    // intrinsic predicate with functional term parent
    [<DataRow("FplMandatoryPredicate", "01",  """def func T()->obj {intr prty pred MandF(x:obj) {intr} };""", "")>]
    // intrinsic predicate with class parent
    [<DataRow("FplMandatoryPredicate", "02",  """def cl T {intr prty pred MandF(x:obj) {intr} };""", "")>]
    // non-intrinsic predicate with predicate parent
    [<DataRow("FplMandatoryPredicate", "03",  """def pred T() {intr prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic predicate with functional term parent
    [<DataRow("FplMandatoryPredicate", "04",  """def func T()->obj {intr prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic predicate with class parent
    [<DataRow("FplMandatoryPredicate", "05",  """def cl T {intr prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic predicate with predicate parent, shared variables and one statement
    [<DataRow("FplMandatoryPredicate", "06",  """def pred T() {dec ~y:obj; true prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    // non-intrinsic predicate with functional term parent, shared variables and one statement
    [<DataRow("FplMandatoryPredicate", "07",  """def func T()->obj {dec ~y:obj; return y prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    // non-intrinsic predicate with class parent, shared variables and one statement
    [<DataRow("FplMandatoryPredicate", "08",  """def cl T {dec ~y:obj; ctor T() {} prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplMandatoryPredicate(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplMandatoryPredicate.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplMandatoryPredicate", "00" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic  
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "01" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
            Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "02" -> 
            Assert.IsInstanceOfType<FplClass>(parent) // class parent
            Assert.AreEqual<int>(0, parent.ArgList.Count) // class's base class
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "03" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic  
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "04" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
            Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "05" -> 
            Assert.IsInstanceOfType<FplClass>(parent) // class parent
            Assert.AreEqual<int>(0, parent.ArgList.Count) // class's base class
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "06" -> 
            Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
            Assert.AreEqual<int>(1, parent.ArgList.Count) // predicate's value
            Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with assignment 
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "07" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
            Assert.AreEqual<int>(2, parent.ArgList.Count) // parent's mapping, return stmt
            Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic assignment
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | "FplMandatoryPredicate", "08" -> 
            Assert.IsInstanceOfType<FplClass>(parent) // class parent
            Assert.AreEqual<int>(0, parent.ArgList.Count) // class's base class
            Assert.AreEqual<int>(3, parent.Scope.Count) // variable, property and constructor
            Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic withassignment
            Assert.AreEqual<int>(1, node.Scope.Count) // one variable
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplMapCaseElse", "00", """def pred T(x:ind) { dec ~n:pred n:= mcases (| (x = $1) : false | (x = $2) : true | (x = $3) : false ? undef ); n };""", "")>]
    [<DataRow("FplMapCaseElse", "01", """def pred T() { mcases (| true : false | false : true ? undef )};""", "")>]
    [<DataRow("FplMapCaseElse", "02", """def func T()->pred { return mcases (| true : false | false : true ? undef )};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplMapCaseElse(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplMapCaseElse.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplMapCaseElse", "00" ->
            Assert.IsInstanceOfType<FplMapCases>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCaseElse>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplMapCaseElse", "01" ->
            Assert.IsInstanceOfType<FplMapCases>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCaseElse>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplMapCaseElse", "02" ->
            Assert.IsInstanceOfType<FplMapCases>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCaseElse>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplMapCases", "00", """def pred T(x:ind) { dec ~n:pred n:= mcases (| (x = $1) : false | (x = $2) : true | (x = $3) : false ? undef ); n };""", "")>]
    [<DataRow("FplMapCases", "01", """def pred T() { mcases (| true : false | false : true ? undef )};""", "")>]
    [<DataRow("FplMapCases", "02", """def func T()->pred { return mcases (| true : false | false : true ? undef )};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplMapCases(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplMapCases.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplMapCases", "00" ->
            Assert.IsInstanceOfType<FplAssignment>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // n := mcases
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCases>(node)
            Assert.AreEqual<int>(4, node.ArgList.Count) // 3 cases + 1 else
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplMapCases", "01" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCases>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // 2 cases + 1 else
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplMapCases", "02" ->
            Assert.IsInstanceOfType<FplReturn>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCases>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // 2 cases + 1 else
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplMapCaseSingle", "00", """def pred T(x:ind) { dec ~n:pred n:= mcases (| (x = $1) : false | (x = $2) : true | (x = $3) : false ? undef ); n };""", "")>]
    [<DataRow("FplMapCaseSingle", "01", """def pred T() { mcases (| true : false | false : true ? undef )};""", "")>]
    [<DataRow("FplMapCaseSingle", "02", """def func T()->pred { return mcases (| true : false | false : true ? undef )};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplMapCaseSingle(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplMapCaseSingle.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplMapCaseSingle", "00" ->
            Assert.IsInstanceOfType<FplMapCases>(parent)
            Assert.AreEqual<int>(4, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCaseSingle>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplMapCaseSingle", "01" ->
            Assert.IsInstanceOfType<FplMapCases>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCaseSingle>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplMapCaseSingle", "02" ->
            Assert.IsInstanceOfType<FplMapCases>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplMapCaseSingle>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // mapping of functional terms
    [<DataRow("FplMapping", "00a", """def func T()->pred(a:obj) {intr};""", "")>]
    [<DataRow("FplMapping", "00b", """def func T()->func()->pred(a,b:obj) {intr};""", "")>]
    [<DataRow("FplMapping", "00c", """def func T()->func()->pred(a,b:obj) {intr};""", "pred(a, b)")>]
    [<DataRow("FplMapping", "00d", """def func T()->func(x,y,z:obj)->obj {intr};""", "")>]
    // mapping of mandatory properties
    [<DataRow("FplMapping", "01a", """def pred T1() {intr prty func T()->pred(a:obj) {intr}};""", "")>]
    [<DataRow("FplMapping", "01b", """def pred T1() {intr prty func T()->func()->pred(a,b:obj) {intr}};""", "")>]
    [<DataRow("FplMapping", "01c", """def pred T1() {intr prty func T()->func()->pred(a,b:obj) {intr}};""", "pred(a, b)")>]
    [<DataRow("FplMapping", "01d", """def pred T1() {intr prty func T()->func(a,b,c:obj)->obj {intr}};""", "")>]
    // mapping of extensions
    [<DataRow("FplMapping", "03a", """ext Digits x@/\d+/->pred(a:obj) {ret x};""", "")>]
    [<DataRow("FplMapping", "03b", """ext Digits x@/\d+/->func()->pred(a,b:obj) {ret x};""", "")>]
    [<DataRow("FplMapping", "03c", """ext Digits x@/\d+/->func()->pred(a,b:obj) {ret x};""", "pred(a, b)")>]
    [<DataRow("FplMapping", "03d", """ext Digits x@/\d+/->func(a,b,c:obj)->obj {ret x};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplMapping(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplMapping.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplMapping", "00a" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // mapping
            Assert.AreEqual<int>(1, parent.Scope.Count) // mapping's variable(s) in node's scope
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) 
        | "FplMapping", "00b" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // mapping
            Assert.AreEqual<int>(2, parent.Scope.Count) // mapping's variable(s) in node's scope
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(0, node.Scope.Count) // 0 variables
        | "FplMapping", "00c" 
        | "FplMapping", "01c"  
        | "FplMapping", "02c"  
        | "FplMapping", "03c" -> 
            Assert.IsInstanceOfType<FplMapping>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // nested mapping 
            Assert.AreEqual<int>(0, parent.Scope.Count) // 0 variables
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // no nested mapping
            Assert.AreEqual<int>(2, node.Scope.Count) // 2 variables
        | "FplMapping", "00d" -> 
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // mapping
            Assert.AreEqual<int>(3, parent.Scope.Count) // mapping's variable(s) in node's scope
            Assert.IsInstanceOfType<FplMapping>(node) 
            Assert.AreEqual<int>(1, node.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables
        | "FplMapping", "01a" -> 
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // mapping
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) 
        | "FplMapping", "01b" -> 
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // mapping
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(0, node.Scope.Count) // 0 variables
        | "FplMapping", "01d" -> 
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(parent) 
            Assert.AreEqual<int>(1, parent.ArgList.Count) // mapping
            Assert.AreEqual<int>(0, parent.Scope.Count) // 0 variables
            Assert.IsInstanceOfType<FplMapping>(node) 
            Assert.AreEqual<int>(1, node.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables
        | "FplMapping", "03a" -> 
            Assert.IsInstanceOfType<FplExtension>(parent) 
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(2, parent.Scope.Count) // mapping's variable(s) in node's scope
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) 
        | "FplMapping", "03b" -> 
            Assert.IsInstanceOfType<FplExtension>(parent) 
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return
            Assert.AreEqual<int>(3, parent.Scope.Count) // mapping's variable(s) in node's scope
            Assert.IsInstanceOfType<FplMapping>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(0, node.Scope.Count) // 0 variables
        | "FplMapping", "03d" -> 
            Assert.IsInstanceOfType<FplExtension>(parent) 
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return
            Assert.AreEqual<int>(4, parent.Scope.Count) // mapping's variable(s) in node's scope
            Assert.IsInstanceOfType<FplMapping>(node) 
            Assert.AreEqual<int>(1, node.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplNegation", "00", """def pred T() {not x};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplNegation(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplNegation.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplNegation", "00" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplNegation>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplParent", "00a", """def cl A {dec ~x:obj x:=parent; ctor A() {}};""", "")>]
    [<DataRow("FplParent", "00b", """def cl A {ctor A() {dec ~x:obj x:=parent;}};""", "")>]
    [<DataRow("FplParent", "00c", """def cl A {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "00d", """def cl A {intr property func T()->obj {dec ~x:obj x:=parent; return x } };""", "")>]
    [<DataRow("FplParent", "00e", """def cl A {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "00f", """def cl A {intr property func T()->obj {dec ~x:obj x:=parent; return x } };""", "")>]
    [<DataRow("FplParent", "01a", """def pred A() {dec assert is(parent,A); true };""", "")>]
    [<DataRow("FplParent", "01b", """def pred A() {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "01c", """def pred A() {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "01d", """def pred A() {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "01e", """def pred A() {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "02a", """def func A()->obj {dec ~x:obj assert is(parent,A); return x};""", "")>]
    [<DataRow("FplParent", "02b", """def func A()->obj {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "02c", """def func A()->obj {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "02d", """def func A()->obj {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent", "02e", """def func A()->obj {intr property pred T() { is(parent,A) } };""", "")>]
    [<DataRow("FplParent",  "03", """axiom A {parent};""", "")>]
    [<DataRow("FplParent",  "04", """theorem A {parent};""", "")>]
    [<DataRow("FplParent",  "05", """lemma A {parent};""", "")>]
    [<DataRow("FplParent",  "06", """prop A {parent};""", "")>]
    [<DataRow("FplParent",  "08", """conj A {parent};""", "")>]
    [<DataRow("FplParent",  "09", """cor A$1 {parent};""", "")>]
    [<DataRow("FplParent",  "10", """prf A$1 {1. |- parent qed};""", "")>]
    [<DataRow("FplParent",  "11", """inf A {pre: true con: parent};""", "")>]
    [<DataRow("FplParent",  "12", """inf A {pre: parent con: true};""", "")>]
    [<DataRow("FplParent",  "13", """loc not(parent) := !tex: "\neg(" x ")";;""", "")>]
    [<DataRow("FplParent",  "14", """ext A x@/\d+/ -> obj {dec assert is(parent,A); ret x};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplParent(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplParent.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
            // nodes that can be referrenced as parent 
            | "FplParent", "00b"
            | "FplParent", "00c"
            | "FplParent", "00d"
            | "FplParent", "00e"
            | "FplParent", "00f"
            | "FplParent", "01b"
            | "FplParent", "01c"
            | "FplParent", "01d"
            | "FplParent", "01e"
            | "FplParent", "02b"
            | "FplParent", "02c"
            | "FplParent", "02d"
            | "FplParent", "02e"
                ->
                Assert.IsInstanceOfType<FplReference>(parent)
                Assert.AreEqual<int>(0, parent.ArgList.Count)
                Assert.AreEqual<int>(1, parent.Scope.Count)
                Assert.IsInstanceOfType<FplParent>(node)
                Assert.AreEqual<int>(0, node.ArgList.Count)
                Assert.AreEqual<int>(1, node.Scope.Count)
            // nodes that cannot be referrenced as parent 
            | "FplParent", "00a"
            | "FplParent", "01a"
            | "FplParent", "02a"
            | "FplParent",  "03"
            | "FplParent",  "04"
            | "FplParent",  "05"
            | "FplParent",  "06"
            | "FplParent",  "08"
            | "FplParent",  "09"
            | "FplParent",  "10"
            | "FplParent",  "11"
            | "FplParent",  "12"
            | "FplParent",  "13"
            | "FplParent",  "14"
                ->
                Assert.IsInstanceOfType<FplReference>(parent)
                Assert.AreEqual<int>(0, parent.ArgList.Count)
                Assert.AreEqual<int>(1, parent.Scope.Count)
                Assert.IsInstanceOfType<FplParent>(node)
                Assert.AreEqual<int>(0, node.ArgList.Count)
                Assert.AreEqual<int>(0, node.Scope.Count)
            | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // intrinsic predicate
    [<DataRow("FplPredicate", "00", """def pred T() {intr};""", "")>]
    // intrinsic predicate with variables
    [<DataRow("FplPredicate", "01", """def pred T(x,y:obj) {intr};""", "")>]
    // intrinsic predicate with variables and properties
    [<DataRow("FplPredicate", "02", """def pred T(x,y:obj) {intr prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    // non-intrinsic predicate with variables and properties
    [<DataRow("FplPredicate", "03", """def pred T(x,y:obj) {dec ~z:obj; true prty func MandF()->obj {intr} prty func OptF()->obj {intr} prty pred MandP() {true} prty pred OptP() {true} };""", "")>]
    // non-intrinsic predicate with some statements 
    [<DataRow("FplPredicate", "04", """def pred T(x,y:obj) {dec ~z:obj z:=x y:=z; false };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplPredicate(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplPredicate.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplPredicate", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicate>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            Assert.AreEqual<string>(PrimUndetermined, node.Represent())
        | "FplPredicate", "01" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicate>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
            Assert.AreEqual<int>(2, node.Scope.Count) // two variables
            Assert.AreEqual<string>(PrimUndetermined, node.Represent())
        | "FplPredicate", "02" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicate>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
            Assert.AreEqual<int>(6, node.Scope.Count) // 2 variables, 4 properties
            Assert.AreEqual<string>(PrimUndetermined, node.Represent())
        | "FplPredicate", "03" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicate>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic
            Assert.AreEqual<int>(7, node.Scope.Count) // 3 variables, 4 properties
            Assert.AreEqual<string>(LiteralTrue, node.Represent())
        | "FplPredicate", "04" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicate>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with 2 statements
            Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables
            Assert.AreEqual<string>(LiteralFalse, node.Represent())
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplPredicateList", "00", """inf T {pre: true con: true};""", "")>]
    [<DataRow("FplPredicateList", "01", """inf T {pre: true, true, true con: true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplPredicateList(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplPredicateList.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplPredicateList", "00" -> 
            Assert.IsInstanceOfType<FplRuleOfInference>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicateList>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // one predicate
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplPredicateList", "01" -> 
            Assert.IsInstanceOfType<FplRuleOfInference>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplPredicateList>(node)
            Assert.AreEqual<int>(3, node.ArgList.Count) // three predicates
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // proof 
    [<DataRow("FplProof", "00", """proof T$1 {1. |- trivial};""", "")>]
    // proof with qed
    [<DataRow("FplProof", "00x", """proof T$1 {1. |- trivial qed};""", "")>]
    // proof with wrong parent
    [<DataRow("FplProof", "00a", """ax T {true} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00b", """conj T {true} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00c", """inf T {pre:true con:true} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00d", """def cl T {intr} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00e", """def pred T() {true} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00f", """def func T()->obj {intr} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00g", """loc T := !tex: "T"; proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00h", """ext T x@/\d+/->obj {ret x} proof T$1 {1. |- trivial};""", "")>]
    [<DataRow("FplProof", "00i", """proof T$1 {1. |- trivial} proof T$1$1 {1. |- trivial};""", "T$1$1")>]
    // proof with theorem
    [<DataRow("FplProof", "01a", """thm T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with lem
    [<DataRow("FplProof", "01b", """lem T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with proposition
    [<DataRow("FplProof", "01c", """prop T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with corollary
    [<DataRow("FplProof", "02", """thm T {true} cor T$1 {true} proof T$1$1 {1. |- trivial};""", "")>]
    // proof with two arguments
    [<DataRow("FplProof", "03", """proof T$1$1 {1. byax A |- trivial 2. |- trivial};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplProof(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplProof.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplProof", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // single proof
            Assert.IsInstanceOfType<FplProof>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "00x" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // single proof with qed
            Assert.IsInstanceOfType<FplProof>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "00a" 
        | "FplProof", "00b" 
        | "FplProof", "00c" 
        | "FplProof", "00d" 
        | "FplProof", "00e" 
        | "FplProof", "00f" 
        | "FplProof", "00g" 
        | "FplProof", "00h" 
        | "FplProof", "00i" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count) // proof with wrong parent (e.g. axiom or conjecture)
            Assert.IsInstanceOfType<FplProof>(node) 
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "01a" ->
            Assert.IsInstanceOfType<FplTheorem>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // proof with theorem
            Assert.IsInstanceOfType<FplProof>(node)  
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "01b" ->
            Assert.IsInstanceOfType<FplLemma>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // proof with lemma
            Assert.IsInstanceOfType<FplProof>(node)  
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "01c" -> 
            Assert.IsInstanceOfType<FplProposition>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // proof with proposition
            Assert.IsInstanceOfType<FplProof>(node)  
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "02" -> 
            Assert.IsInstanceOfType<FplCorollary>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // proof with proposition
            Assert.IsInstanceOfType<FplProof>(node)  
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        | "FplProof", "03" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // proof with proposition
            Assert.IsInstanceOfType<FplProof>(node)  
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(2, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplProposition", "00", """prop T {true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplProposition(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplProposition.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplProposition", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplProposition>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplQuantorAll", "00", """inf T {pre: true con: all x:pred {and (x,x)}};""", "")>]
    [<DataRow("FplQuantorAll", "01", """inf T {dec ~p: pred(c: obj); pre: p(c) con: all x:obj {p(x)}};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplQuantorAll(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplQuantorAll.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplQuantorAll", "00" -> 
            Assert.IsInstanceOfType<FplRuleOfInference>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable x
            Assert.IsInstanceOfType<FplQuantorAll>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // conjunction
            Assert.AreEqual<int>(1, node.Scope.Count) // x variable
        | "FplQuantorAll", "01" -> 
            Assert.IsInstanceOfType<FplRuleOfInference>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(3, parent.Scope.Count) // variable p, c, x
            Assert.IsInstanceOfType<FplQuantorAll>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
            Assert.AreEqual<int>(1, node.Scope.Count) // x variable
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplQuantorExists", "00", """inf T {pre: ex x:tpl {or(p, q(x))} con: true};""", "")>]
    [<DataRow("FplQuantorExists", "01", """inf T {dec ~p: pred ~q:pred(z:tpl); pre: ex x:pred { p(z) } con: true};""", "")>]
    [<DataRow("FplQuantorExists", "02", """inf T {pre: ex x,y:pred { and(x,y) }, true con: true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplQuantorExists(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplQuantorExists.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplQuantorExists", "00" -> 
            Assert.IsInstanceOfType<FplPredicateList>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplQuantorExists>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // disjunction
            Assert.AreEqual<int>(1, node.Scope.Count) // x variable
        | "FplQuantorExists", "01" -> 
            Assert.IsInstanceOfType<FplPredicateList>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) // variable p
            Assert.IsInstanceOfType<FplQuantorExists>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
            Assert.AreEqual<int>(1, node.Scope.Count) // x variable
        | "FplQuantorExists", "02" -> 
            Assert.IsInstanceOfType<FplPredicateList>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // two predicates in list
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplQuantorExists>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // conjunction
            Assert.AreEqual<int>(2, node.Scope.Count) // 2 variables
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplQuantorExistsN", "00", """ax T {exn$1 x:tpl {and(p, q(x))} };""", "")>]
    [<DataRow("FplQuantorExistsN", "01", """conj T {dec ~p: pred ~q:pred(z:tpl); exn$1 x:pred { p(z) } };""", "")>]
    [<DataRow("FplQuantorExistsN", "02", """lem T {exn$1 x:pred { x } };""", "")>]
    [<TestMethod>]
    member this.TestStructureFplQuantorExistsN(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplQuantorExistsN.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplQuantorExistsN", "00" -> 
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count) // 1 variable
            Assert.IsInstanceOfType<FplQuantorExistsN>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // conjunction
            Assert.AreEqual<int>(1, node.Scope.Count) // 1 variable
        | "FplQuantorExistsN", "01" -> 
            Assert.IsInstanceOfType<FplConjecture>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(4, parent.Scope.Count) // 4 variables
            Assert.IsInstanceOfType<FplQuantorExistsN>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
            Assert.AreEqual<int>(1, node.Scope.Count) // 1 variable
        | "FplQuantorExistsN", "02" -> 
            Assert.IsInstanceOfType<FplLemma>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) // inner predicate
            Assert.AreEqual<int>(1, parent.Scope.Count) // 1 variable
            Assert.IsInstanceOfType<FplQuantorExistsN>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
            Assert.AreEqual<int>(1, node.Scope.Count) // z variable
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    // references to variables
    [<DataRow("FplReference", "00a", """ax T {x};""", "")>]
    [<DataRow("FplReference", "00b", """ax T {dec ~x:pred; x};""", "")>]
    // references to blocks
    [<DataRow("FplReference", "01a", """ax T {dec ~x,y:obj; A(x,y) };""", "A(x, y)")>]
    [<DataRow("FplReference", "01b", """def pred A(x,y:obj) {intr} ax T {dec ~x,y:obj; A(x,y) };""", "A(x, y)")>]
    [<DataRow("FplReference", "01c", """def func A(x,y:obj)->obj {intr} ax T {dec ~x,y:obj; A(x,y) };""", "A(x, y)")>]
    [<DataRow("FplReference", "01d", """ax T { A };""", "A")>]
    [<DataRow("FplReference", "01e", """def cl A {intr} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01f", """inf A {pre:true con:true} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01g", """ax A {true} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01h", """thm A {true} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01i", """lem A {true} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01j", """prop A {true} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01k", """conj A {true} ax T { A };""", "A")>]
    [<DataRow("FplReference", "01l", """cor A$1 {true} ax T { A$1 };""", "A$1")>]
    [<DataRow("FplReference", "01m", """proof A$1 {1. |- trivial} ax T { A$1 };""", "")>]
    [<DataRow("FplReference", "01n", """ext A x@/\d+/ -> obj {ret x} ax T { A };""", "@A")>]
    [<DataRow("FplReference", "01o", """loc A := !tex: "\alpha" ; ax T { A };""", "")>]
    // return reference
    [<DataRow("FplReference", "02a", """def func A(x:obj)->obj {ret x};""", "")>]
    // reference to named predicate
    [<DataRow("FplReference", "04a", """def pred A() {D()};""", "D()")>]
    [<DataRow("FplReference", "04b", """def pred D() {true} def pred A() {D()};""", "D()")>]
    [<TestMethod>]
    member this.TestStructureFplReference(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplReference.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        // references to variables
        | "FplReference", "00a" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.AreEqual<string>("x", node.FplId) // name of the referenced element
        | "FplReference", "00b" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
            Assert.AreEqual<string>("x", node.FplId) // name of the referenced element
        // references to predicates
        | "FplReference", "01a" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count) // 2 variables
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // 2 arguments
            Assert.AreEqual<int>(0, node.Scope.Count) // no referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
        | "FplReference", "01b" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count) // 2 variables
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // 2 arguments
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplPredicate>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01c" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count) // 2 variables
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // 2 arguments
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplFunctionalTerm>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01d" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count) // no referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
        | "FplReference", "01e" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplClass>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01f" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplRuleOfInference>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01g" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplAxiom>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01h" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplTheorem>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01i" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplLemma>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01j" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplProposition>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01k" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplConjecture>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01l" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A$1", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplCorollary>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01m" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A$1", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplProof>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01n" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("@A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplExtension>(node.Scope[node.FplId]) // name of the referenced element
        | "FplReference", "01o" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("A", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplLocalization>(node.Scope[node.FplId]) // name of the referenced element
        
        // return reference
        | "FplReference", "02a" ->
            Assert.IsInstanceOfType<FplReturn>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) // reference
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) // one referenced element
            Assert.AreEqual<string>("x", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplVariable>(node.Scope[node.FplId]) // name of the referenced element

        // reference to named predicate
        | "FplReference", "04a" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(0, node.Scope.Count) 
            Assert.AreEqual<string>("D", node.FplId) // name of the referenced element
        | "FplReference", "04b" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReference>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count) 
            Assert.AreEqual<int>(1, node.Scope.Count) 
            Assert.AreEqual<string>("D", node.FplId) // name of the referenced element
            Assert.IsInstanceOfType<FplPredicate>(node.Scope[node.FplId]) // name of the referenced element

        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplReturn", "00", """def func A(x:obj)->obj {ret x};""", "")>]
    [<DataRow("FplReturn", "00a", """def func T()->obj {dec ~v:obj v:=v; return v};""", "")>]
    [<DataRow("FplReturn", "00b", """def func T()->tpl {dec ~v:tpl v:=v; return v};""", "")>]
    [<DataRow("FplReturn", "00c", """def func T()->SomeClass1 {dec ~v:SomeClass1; return v};""", "")>]
    [<DataRow("FplReturn", "00d", """def func T()->SomeClass1 {dec ~v:SomeClass1 v:=SomeClass1; return v};""", "")>]
    [<DataRow("FplReturn", "00e", """def func T()->SomeClass1 {dec ~v:SomeClass1 v:=SomeClass1(); return v};""", "")>]
    [<DataRow("FplReturn", "00f", """def func T()->ind {return $112};""", "")>]
    [<DataRow("FplReturn", "00g", """def func T()->ind {dec ~v:ind v:=$13; return v};""", "")>]
    [<DataRow("FplReturn", "01", """ext Digits x@/\d+/ -> obj {ret x};""", "")>]
    [<DataRow("FplReturn", "02", """def cl A {intr prty func A(x:obj)->obj {ret x}};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplReturn(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplReturn.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplReturn", "00" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplReturn", "00a" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count) // mapping + assignment + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplReturn", "00b" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count) // mapping + assignment + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplReturn", "00c" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplReturn", "00d" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count) // mapping + assignment + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplReturn", "00e" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplReturn", "00f" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(0, parent.Scope.Count) 
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic index
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplReturn", "00g" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(3, parent.ArgList.Count) // mapping + assignment + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplReturn", "01" ->
            Assert.IsInstanceOfType<FplExtension>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | "FplReturn", "02" ->
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count) // mapping + return 
            Assert.AreEqual<int>(1, parent.Scope.Count) // variable
            Assert.IsInstanceOfType<FplReturn>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count) // reference
            Assert.AreEqual<int>(0, node.Scope.Count) 
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplRoot", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplRoot(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplRoot.fpl"
        match nodeType, varVal with
        | "FplRoot", "00" ->
            testSkeleton nodeType filename fplCode identifier |> ignore
        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    // rule of inference with one premise
    [<DataRow("FplRuleOfInference", "00", """inf T {pre:true con:false};""", "")>]
    // rule of inference with three premises
    [<DataRow("FplRuleOfInference", "01", """inf T {pre:true, true, true con:false};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplRuleOfInference(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplRuleOfInference.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplRuleOfInference", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplRuleOfInference>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplRuleOfInference", "01" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplRuleOfInference>(node)
            Assert.AreEqual<int>(2, node.ArgList.Count) // still 2, because we have FplPremiseList object 
            Assert.AreEqual<int>(0, node.Scope.Count)

        | _ -> failwith($"unmatched test {nodeType} {varVal}")


    [<DataRow("FplSelf", "00", """def pred T() {self()};""", "")>]
    [<DataRow("FplSelf", "00a", """def cl A {dec ~x:obj x:=self; ctor A() {}};""", "")>]
    [<DataRow("FplSelf", "00b", """def cl A {ctor A() {dec ~x:obj x:=self;}};""", "")>]
    [<DataRow("FplSelf", "00c", """def cl A {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "00d", """def cl A {intr property func T()->obj {dec ~x:obj x:=self; return x } };""", "")>]
    [<DataRow("FplSelf", "00e", """def cl A {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "00f", """def cl A {intr property func T()->obj {dec ~x:obj x:=self; return x } };""", "")>]
    [<DataRow("FplSelf", "01a", """def pred A() {dec assert is(self,A); true };""", "")>]
    [<DataRow("FplSelf", "01b", """def pred A() {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "01c", """def pred A() {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "01d", """def pred A() {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "01e", """def pred A() {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "02a", """def func A()->obj {dec ~x:obj assert is(self,A); return x};""", "")>]
    [<DataRow("FplSelf", "02b", """def func A()->obj {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "02c", """def func A()->obj {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "02d", """def func A()->obj {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "02e", """def func A()->obj {intr property pred T() { is(self,A) } };""", "")>]
    [<DataRow("FplSelf", "03", """axiom A {self};""", "")>]
    [<DataRow("FplSelf", "04", """theorem A {self};""", "")>]
    [<DataRow("FplSelf", "05", """lemma A {self};""", "")>]
    [<DataRow("FplSelf", "06", """prop A {self};""", "")>]
    [<DataRow("FplSelf", "08", """conj A {self};""", "")>]
    [<DataRow("FplSelf", "09", """cor A$1 {self};""", "")>]
    [<DataRow("FplSelf", "10", """prf A$1 {1. |- self qed};""", "")>]
    [<DataRow("FplSelf", "11", """inf A {pre: true con: self};""", "")>]
    [<DataRow("FplSelf", "12", """inf A {pre: self con: true};""", "")>]
    [<DataRow("FplSelf", "13", """loc not(self) := !tex: "\neg(" x ")";;""", "")>]
    [<DataRow("FplSelf", "14", """ext A x@/\d+/ -> obj {dec assert is(self,A); ret x};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplSelf(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplSelf.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        // nodes that can be referred to as 'self'
        | "FplSelf", "00" 
        | "FplSelf", "00a" 
        | "FplSelf", "00c"
        | "FplSelf", "00d"
        | "FplSelf", "00e"
        | "FplSelf", "00f"
        | "FplSelf", "01a"
        | "FplSelf", "01b"
        | "FplSelf", "01c"
        | "FplSelf", "01d"
        | "FplSelf", "01e"
        | "FplSelf", "02a"
        | "FplSelf", "02b"
        | "FplSelf", "02c"
        | "FplSelf", "02d"
        | "FplSelf", "02e"
            ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplSelf>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(1, node.Scope.Count)
        // nodes that cannot be referred to as 'self'
        | "FplSelf", "00b"
        | "FplSelf", "03"
        | "FplSelf", "04"
        | "FplSelf", "05"
        | "FplSelf", "06"
        | "FplSelf", "08"
        | "FplSelf", "09"
        | "FplSelf", "10"
        | "FplSelf", "11"
        | "FplSelf", "12"
        | "FplSelf", "13"
        | "FplSelf", "14"
            ->
            Assert.IsInstanceOfType<FplReference>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplSelf>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplTheorem", "00", """thm T {true};""", "")>]
    [<TestMethod>]
    member this.TestStructureFplTheorem(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplTheorem.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplTheorem", "00" -> 
            Assert.IsInstanceOfType<FplTheory>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplTheorem>(node)
            Assert.AreEqual<int>(1, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)

        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplTheory", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplTheory(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplTheory.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplTheory", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplTheory>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)

        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplTranslation", "01", """loc not x := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "")>]
    [<DataRow("FplTranslation", "02", """loc not x := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "x")>]
    [<DataRow("FplTranslation", "03", """loc not x := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", ")")>]
    [<DataRow("FplTranslation", "04", """loc not x := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "nicht ")>]
    [<TestMethod>]
    member this.TestStructureFplTranslation(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplTranslation.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplTranslation", "01" ->
            Assert.IsInstanceOfType<FplLanguage>(parent) 
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplTranslation>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplTranslation", "02" ->
            Assert.IsInstanceOfType<FplLanguage>(parent) 
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplTranslation>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplTranslation", "03" ->
            Assert.IsInstanceOfType<FplLanguage>(parent) 
            Assert.AreEqual<int>(3, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplTranslation>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | "FplTranslation", "04" ->
            Assert.IsInstanceOfType<FplLanguage>(parent) 
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplTranslation>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")




    // variable simple blocks
    [<DataRow("FplVariable", "00a", """ax T {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00b", """thm T {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00c", """lem T {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00d", """prop T {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00e", """conj T {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00f", """def cl T {dec ~x:obj; ctor T(){}};""", "")>]
    [<DataRow("FplVariable", "00g", """def pred T() {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00h", """def pred T(x:obj) {true};""", "")>]
    [<DataRow("FplVariable", "00i", """def func T()->obj {dec ~x:obj; ret x};""", "")>]
    [<DataRow("FplVariable", "00j", """def func T(x:obj)->obj {intr};""", "")>]
    [<DataRow("FplVariable", "00k", """inf T {dec ~x:obj; pre:true con:true};""", "")>]
    [<DataRow("FplVariable", "00l", """ext Digits x@/\d+/ -> obj {ret x};""", "")>]
    [<DataRow("FplVariable", "00m", """cor T$1 {dec ~x:obj; true};""", "")>]
    [<DataRow("FplVariable", "00n", """prf T$1 {dec ~x:obj; 1. |- trivial};""", "")>]
    [<DataRow("FplVariable", "00o", """loc not x := !tex: "\neg(" y ")";;""", "")>]
    // variable sub blocks
    [<DataRow("FplVariable", "01a", """def cl T {ctor T() {dec ~x:obj;}};""", "")>]
    [<DataRow("FplVariable", "01b", """def cl T {ctor T(x:obj) {}};""", "")>]
    [<DataRow("FplVariable", "01c", """def cl T {intr prty pred T() {dec ~x:obj; true}};""", "")>]
    [<DataRow("FplVariable", "01d", """def cl T {intr prty pred T(x:obj) {true}};""", "")>]
    [<DataRow("FplVariable", "01g", """def cl T {intr prty func T()->obj {dec ~x:obj; ret x}};""", "")>]
    [<DataRow("FplVariable", "01h", """def cl T {intr prty func T(x:obj)->obj {intr}};""", "")>]
    // variable to variable 
    [<DataRow("FplVariable", "02a", """def pred T() {dec ~p:pred(x:obj); true};""", "x")>]
    [<DataRow("FplVariable", "02b", """def pred T(p:pred(x:obj)) {true};""", "x")>]
    [<DataRow("FplVariable", "02c", """def pred T() {dec ~p:*pred(x:obj)[ind]; true};""", "x")>]
    [<DataRow("FplVariable", "02d", """def pred T(p:*pred(x:obj)[ind]) {true};""", "x")>]
    [<DataRow("FplVariable", "02e", """def pred T() {dec ~p:*pred(x:obj)[tpl,Nat]; true};""", "x")>]
    [<DataRow("FplVariable", "02f", """def pred T(p:*pred(x:obj)[ind,obj]) {true};""", "x")>]
    // variable in mapping
    [<DataRow("FplVariable", "03a", """def func T()->pred(x:obj) {intr};""", "")>]
    [<DataRow("FplVariable", "03b", """def func T()->func()->pred(a,b:obj) {intr};""", "b")>]
    [<DataRow("FplVariable", "03c", """def func T()->func(a,b,c:obj)->obj {intr};""", "c")>]
    // variable in localization
    [<DataRow("FplVariable", "04a", """loc not x := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "")>]
    [<DataRow("FplVariable", "04b", """loc not(x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "")>]
    [<DataRow("FplVariable", "04c", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;;""", "")>]
    [<DataRow("FplVariable", "04d", """loc and(p,q) := !tex: p "\wedge" q !eng: p " and" q !ger: p " und " q;;""", "")>]
    // variable in translation
    [<DataRow("FplVariable", "05a", """loc not x := !tex: "\neg(" y ")";;""", "y")>]
    [<TestMethod>]
    member this.TestStructureFplVariable(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplVariable.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        // Variables, simple blocks
        | "FplVariable", "00a" ->
            Assert.IsInstanceOfType<FplAxiom>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00b" ->
            Assert.IsInstanceOfType<FplTheorem>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00c" ->
            Assert.IsInstanceOfType<FplLemma>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00d" ->
            Assert.IsInstanceOfType<FplProposition>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00e" ->
            Assert.IsInstanceOfType<FplConjecture>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00f" ->
            Assert.IsInstanceOfType<FplClass>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00g" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00h" ->
            Assert.IsInstanceOfType<FplPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsTrue(x.IsSignatureVariable)
        | "FplVariable", "00i" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00j" ->
            Assert.IsInstanceOfType<FplFunctionalTerm>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsTrue(x.IsSignatureVariable)
        | "FplVariable", "00k" ->
            Assert.IsInstanceOfType<FplRuleOfInference>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00l" ->
            Assert.IsInstanceOfType<FplExtension>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00m" ->
            Assert.IsInstanceOfType<FplCorollary>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00n" ->
            Assert.IsInstanceOfType<FplProof>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "00o" ->
            Assert.IsInstanceOfType<FplLocalization>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) // negation
            Assert.AreEqual<int>(2, parent.Scope.Count) // var + language
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)

        // Variables, sub blocks
        | "FplVariable", "01a" ->
            Assert.IsInstanceOfType<FplConstructor>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "01b" ->
            Assert.IsInstanceOfType<FplConstructor>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsTrue(x.IsSignatureVariable)
        | "FplVariable", "01c" ->
            Assert.IsInstanceOfType<FplMandatoryPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "01d" ->
            Assert.IsInstanceOfType<FplMandatoryPredicate>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsTrue(x.IsSignatureVariable)
        | "FplVariable", "01g" ->
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(parent)
            Assert.AreEqual<int>(2, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "01h" ->
            Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsTrue(x.IsSignatureVariable)
        // variable to variable
        | "FplVariable", "02a" ->
            Assert.IsInstanceOfType<FplVariable>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            let p = (parent:?>FplGenericVariable)
            Assert.IsFalse(p.IsInitializedVariable)
            Assert.IsFalse(p.IsSignatureVariable)                    
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "02b" ->
            Assert.IsInstanceOfType<FplVariable>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            let p = (parent:?>FplGenericVariable)
            Assert.IsFalse(p.IsInitializedVariable)
            Assert.IsTrue(p.IsSignatureVariable)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        // variable to variable
        | "FplVariable", "02c" ->
            Assert.IsInstanceOfType<FplVariableArray>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            let p = (parent:?>FplGenericVariable)
            Assert.IsFalse(p.IsInitializedVariable)
            Assert.IsFalse(p.IsSignatureVariable)                    
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "02d" ->
            Assert.IsInstanceOfType<FplVariableArray>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            let p = (parent:?>FplGenericVariable)
            Assert.IsFalse(p.IsInitializedVariable)
            Assert.IsTrue(p.IsSignatureVariable)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        // variable to variable
        | "FplVariable", "02e" ->
            Assert.IsInstanceOfType<FplVariableArray>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            let p = (parent:?>FplGenericVariable)
            Assert.IsFalse(p.IsInitializedVariable)
            Assert.IsFalse(p.IsSignatureVariable)                    
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "02f" ->
            Assert.IsInstanceOfType<FplVariableArray>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            let p = (parent:?>FplGenericVariable)
            Assert.IsFalse(p.IsInitializedVariable)
            Assert.IsTrue(p.IsSignatureVariable)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        // variable to mappping
        | "FplVariable", "03a" ->
            Assert.IsInstanceOfType<FplMapping>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(1, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "03b" ->
            Assert.IsInstanceOfType<FplMapping>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(2, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "03c" ->
            Assert.IsInstanceOfType<FplMapping>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) // nested mapping
            Assert.AreEqual<int>(3, parent.Scope.Count) // 3 variables
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)

        // localizations 
        | "FplVariable", "04a" 
        | "FplVariable", "04b" ->
            Assert.IsInstanceOfType<FplLocalization>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(4, parent.Scope.Count) // a variable and 3 languages
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "04c" ->
            Assert.IsInstanceOfType<FplLocalization>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(4, parent.Scope.Count) // two variables and 3 languages
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)
        | "FplVariable", "04d" ->
            Assert.IsInstanceOfType<FplLocalization>(parent)
            Assert.AreEqual<int>(1, parent.ArgList.Count) 
            Assert.AreEqual<int>(5, parent.Scope.Count) // two variables and 3 languages
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)

        // translations 
        | "FplVariable", "05a" ->
            Assert.IsInstanceOfType<FplTranslation>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count) 
            Assert.AreEqual<int>(1, parent.Scope.Count) // a variable 
            Assert.IsInstanceOfType<FplVariable>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
            let x = (node:?>FplGenericVariable)
            Assert.IsFalse(x.IsInitializedVariable)
            Assert.IsFalse(x.IsSignatureVariable)

        | _ -> failwith($"unmatched test {nodeType} {varVal}")

    [<DataRow("FplVariableArray", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructureFplVariableArray(nodeType, varVal, fplCode, identifier) =
        let filename = "TestStructureFplVariableArray.fpl"
        let parent, node = testSkeleton nodeType filename fplCode identifier
        
        match nodeType, varVal with
        | "FplVariableArray", "00" ->
            Assert.IsInstanceOfType<FplRoot>(parent)
            Assert.AreEqual<int>(0, parent.ArgList.Count)
            Assert.AreEqual<int>(0, parent.Scope.Count)
            Assert.IsInstanceOfType<FplVariableArray>(node)
            Assert.AreEqual<int>(0, node.ArgList.Count)
            Assert.AreEqual<int>(0, node.Scope.Count)
        | _ -> failwith($"unmatched test {nodeType} {varVal}")

