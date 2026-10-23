namespace TestFpl3LanguageServer.ServiceFindReference

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open Fpl0Base.Primitives
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Types1.TopLevel
open Fpl2Interpreter.SymbolTable.Types2.References
open Fpl2Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl2Interpreter.SymbolTable.GoToDefinition

[<TestClass>]
type TestReferenceIndex() =
    let positions = (Position("", 0, 0, 0), Position("", 0, 0, 0))
    let parent = new FplRoot()

    let makeReference (declaration: FplGenericNode) =
        let reference = new FplReference(positions, parent)
        reference.RefersTo <- Some declaration
        reference

    [<TestMethod>]
    member _.TestFindReferencesGroupsByDeclaration() =
        let declA = new FplAxiom(positions, parent, 0)
        let declB = new FplAxiom(positions, parent, 0)
        let refA1 = makeReference declA
        let refA2 = makeReference declA
        let refB = makeReference declB

        let index = buildReferenceIndex [ refA1; refA2; refB ]

        CollectionAssert.AreEquivalent([| refA1; refA2 |], findReferences index declA |> List.toArray)
        CollectionAssert.AreEquivalent([| refB |], findReferences index declB |> List.toArray)

    [<TestMethod>]
    member _.TestUnknownDeclarationHasNoReferences() =
        let declA = new FplAxiom(positions, parent, 0)
        let declOther = new FplAxiom(positions, parent, 0)
        let index = buildReferenceIndex [ makeReference declA ]

        Assert.AreEqual(0, findReferences index declOther |> List.length)

