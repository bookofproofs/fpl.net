namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestVariableTypes () =

    [<TestMethod>]
    member this.TestVariableType001 () =
        let result = run variableType """object"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ObjectType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType002 () =
        let result = run variableType """obj"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ObjectType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType003 () =
        let result = run variableType """function"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType004 () =
        let result = run variableType """func"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType005 () =
        let result = run variableType """predicate"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType006 () =
        let result = run variableType """pred"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType007 () =
        let result = run variableType """index"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType008 () =
        let result = run variableType """ind"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType009 () =
        let result = run variableType """SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ClassHeaderType ["SomeClass"])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType010 () =
        let result = run variableType """@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ExtensionType (Extensionname "Nat"))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType011 () =
        let result = run variableType """template"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "template")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType012 () =
        let result = run variableType """tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "tpl")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType013 () =
        let result = run variableType """templateTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "templateTest")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType014 () =
        let result = run variableType """tplTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "tplTest")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType015 () =
        let result = run variableType """+object"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ObjectType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType016 () =
        let result = run variableType """+obj"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ObjectType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType017 () =
        let result = run variableType """+function"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType018 () =
        let result = run variableType """+func"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType019 () =
        let result = run variableType """+predicate"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType020 () =
        let result = run variableType """+pred"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType021 () =
        let result = run variableType """+index"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType022 () =
        let result = run variableType """+ind"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType023 () =
        let result = run variableType """+SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ClassHeaderType ["SomeClass"])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType024 () =
        let result = run variableType """+@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ExtensionType (Extensionname "Nat"))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType025 () =
        let result = run variableType """+template"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "template")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType026 () =
        let result = run variableType """+tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "tpl")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType027 () =
        let result = run variableType """+templateTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "templateTest")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType028 () =
        let result = run variableType """+tplTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "tplTest")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType029 () =
        let result = run variableType """*object"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ObjectType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType030 () =
        let result = run variableType """*obj"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ObjectType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType031 () =
        let result = run variableType """*function"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType032 () =
        let result = run variableType """*func"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType033 () =
        let result = run variableType """*predicate"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType034 () =
        let result = run variableType """*pred"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType035 () =
        let result = run variableType """*index"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType036 () =
        let result = run variableType """*ind"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType037 () =
        let result = run variableType """*SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ClassHeaderType ["SomeClass"])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType038 () =
        let result = run variableType """*@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ExtensionType (Extensionname "Nat"))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType039 () =
        let result = run variableType """*template"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "template")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType040 () =
        let result = run variableType """*tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "tpl")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType041 () =
        let result = run variableType """*templateTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "templateTest")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType042 () =
        let result = run variableType """*tplTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "tplTest")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType043 () =
        let result = run variableType """*object[x,y,z]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many, FplTypeWithCoords (ObjectType, [Var "x"; Var "y"; Var "z"]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType044 () =
        let result = run variableType """*obj[x~y]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((ObjectType, LeftClosed),
      (RangeInType (Some (Var "x"), Some (Var "y")), RightClosed)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType045 () =
        let result = run variableType """*function[~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType046 () =
        let result = run variableType """*func[1~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType047 () =
        let result = run variableType """*predicate[~2]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType048 () =
        let result = run variableType """*pred[x,y,z]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType049 () =
        let result = run variableType """*index[~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType050 () =
        let result = run variableType """*ind[SomeClass]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType051 () =
        let result = run variableType """*SomeClass[SomeOtherClass]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithCoords
     (ClassHeaderType ["SomeClass"], [AliasedId ["SomeOtherClass"]]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType052 () =
        let result = run variableType """*@extNat[1~2]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((ExtensionType (Extensionname "Nat"), LeftClosed),
      (RangeInType (Some (ExtDigits "1"), Some (ExtDigits "2")), RightClosed)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType053 () =
        let result = run variableType """*template[1~n]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((TemplateType "template", LeftClosed),
      (RangeInType (Some (ExtDigits "1"), Some (Var "n")), RightClosed)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType054 () =
        let result = run variableType """*tpl[33]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many, FplTypeWithCoords (TemplateType "tpl", [ExtDigits "33"]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType055 () =
        let result = run variableType """*templateTest[0~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((TemplateType "templateTest", LeftClosed),
      (RangeInType (Some (ExtDigits "0"), None), RightClosed)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType056 () =
        let result = run variableType """*tplTest[~10]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((TemplateType "tplTest", LeftClosed),
      (RangeInType (None, Some (ExtDigits "10")), RightClosed)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);
