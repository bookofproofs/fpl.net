namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClassInheritanceTypes () =


    [<TestMethod>]
    member this.TestSpecificType3 () =
        let result = run specificClassType """object"""
        let actual = sprintf "%O" result
        let expected = """Success: ObjectType""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType4 () =
        let result = run specificClassType """tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: TemplateType "tpl" """.Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType5 () =
        let result = run specificClassType """tplSetElem"""
        let actual = sprintf "%O" result
        let expected = """Success: TemplateType "tplSetElem" """.Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);


    member this.TestSpecificType7 () =
        let result = run specificClassType """@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: ExtensionType (Extensionname "Nat")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType8 () =
        let result = run specificClassType """SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: ClassHeaderType ["SomeClass"]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType9 () =
        let result = run specificClassType """bla"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 1\nbla\n^\nExpecting: <PascalCaseId>, @ext<PascalCaseId>, 'obj', 'object', 'template' or\n'tpl'\n"
        Assert.AreEqual(expected, actual2);

    [<TestMethod>]
    member this.TestClassType3 () =
        let result = run classType """object[self]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithCoords (ObjectType, [Self []])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType3a () =
        let result = run classType """object[SomeObject1, SomeObject2,SomeObject3]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithCoords
  (ObjectType,
   [AliasedId ["SomeObject1"]; AliasedId ["SomeObject2"];
    AliasedId ["SomeObject3"]])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType4 () =
        let result = run classType """tpl[from~]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((TemplateType "tpl", LeftClosed),
   (RangeInType (Some (Var "from"), None), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType4a () =
        let result = run classType """tpl[~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((TemplateType "tpl", LeftClosed),
   (RangeInType (None, Some (Var "to")), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType4b () =
        let result = run classType """tpl[~]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((TemplateType "tpl", LeftClosed), (RangeInType (None, None), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType5 () =
        let result = run classType """Set[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((ClassHeaderType ["Set"], LeftClosed),
   (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType5a () =
        let result = run classType """Set[!from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((ClassHeaderType ["Set"], LeftOpen),
   (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType5b () =
        let result = run classType """Set[from ~ to!]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((ClassHeaderType ["Set"], LeftClosed),
   (RangeInType (Some (Var "from"), Some (Var "to")), RightOpen))""".Replace("\r","")
        Assert.AreEqual(expected, actual);


    [<TestMethod>]
    member this.TestClassType7 () =
        let result = run classType """@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: ExtensionType (Extensionname "Nat")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType8 () =
        let result = run classType """SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: ClassHeaderType ["SomeClass"]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType9 () =
        let result = run classType """bla"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 1\nbla\n^\nExpecting: <PascalCaseId>, @ext<PascalCaseId>, 'obj', 'object', 'template' or\n'tpl'\n"
        Assert.AreEqual(expected, actual2);
