namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestPlacingComments () =

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm00 () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionFunctionalTerm00a () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        // comment
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionFunctionalTerm00b () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            // comment
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionFunctionalTerm00c () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            intrinsic
            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionFunctionalTerm00d () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            intrinsic
            optional func X() -> Y { intr }
            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionFunctionalTerm00e () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            // comment
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionFunctionalTerm00f () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            // comment

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01 () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01a () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        // comment
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01b () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            // comment
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01c () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                // comment
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01d () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                // comment
                x:=1
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01e () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                // comment
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01f () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                // comment
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01g () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
                // comment
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01h () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            // comment
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01i () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            return e

            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionFunctionalTerm01j () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            return e

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate00 () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionPredicate00a () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        // comment
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionPredicate00b () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            // comment
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionPredicate00c () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            intrinsic
            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionPredicate00d () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            intrinsic
            optional func X() -> Y { intr }
            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionPredicate00e () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            // comment
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionPredicate00f () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            // comment

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01 () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01a () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        // comment
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01b () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            // comment
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01c () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                // comment
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01d () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                // comment
                x:=1
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01e () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                // comment
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01f () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                // comment
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01g () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
                // comment
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01h () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            // comment
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01i () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            true

            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionPredicate01j () =
        let result = run definitionPredicate """pred LeftNeutralElement()
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            true

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass00 () =
        let result = run definitionClass """cl T:obj
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionClass00a () =
        let result = run definitionClass """cl T:obj
        // comment
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionClass00b () =
        let result = run definitionClass """cl T:obj
        {
            // comment
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionClass00c () =
        let result = run definitionClass """cl T:obj
        {
            intrinsic
            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionClass00d () =
        let result = run definitionClass """cl T:obj
        {
            intrinsic
            optional func X() -> Y { intr }
            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionClass00e () =
        let result = run definitionClass """cl T:obj
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            // comment
            optional func X() -> Y { intr }

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    member this.TestPlacingCommentsDefinitionClass00f () =
        let result = run definitionClass """cl T:obj
        {
            intrinsic
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            // comment

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01 () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01a () =
        let result = run definitionClass """cl T:obj
        // comment
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01b () =
        let result = run definitionClass """cl T:obj
        {
            // comment
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01c () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                // comment
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01d () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                // comment
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01e () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                // comment
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01f () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                // comment
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01g () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
                // comment
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01h () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            // comment
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01i () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            // comment
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPlacingCommentsDefinitionClass01j () =
        let result = run definitionClass """cl T:obj
        {
            dec 
                ~a:obj ~ e: tplSetElem 
                x:=1
                y:=1
                z:=1
            ;
            ctor T() { self}

            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
            optional func X() -> Y { intr }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
