namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestFplBlockComponentes () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestSignature01 () =
        let result = run signature """AreRelated(u,v: Set, r: BinaryRelation)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["AreRelated"],
   [([Var "u"; Var "v"],
     VariableTypeWithModifier (None, ClassHeaderType ["Set"]));
    ([Var "r"],
     VariableTypeWithModifier (None, ClassHeaderType ["BinaryRelation"]))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature02 () =
        let result = run signature """ExistsByExample(p: pred(c: obj))"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["ExistsByExample"],
   [([Var "p"],
     VariableType [([Var "c"], VariableTypeWithModifier (None, ObjectType))])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature03 () =
        let result = run signature """Zero()"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature (AliasedId ["Zero"], [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature04 () =
        let result = run signature """Test(a,b: tpl)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["Test"],
   [([Var "a"; Var "b"], VariableTypeWithModifier (None, TemplateType "tpl"))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature05 () =
        let result = run signature """TestPredicate(a,b:obj)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["TestPredicate"],
   [([Var "a"; Var "b"], VariableTypeWithModifier (None, ObjectType))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature06 () =
        let result = run signature """BinOp(x,y: tplSetElem)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["BinOp"],
   [([Var "x"; Var "y"],
     VariableTypeWithModifier (None, TemplateType "tplSetElem"))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature07 () =
        let result = run signature """IsSubset(subset,superset: Set)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["IsSubset"],
   [([Var "subset"; Var "superset"],
     VariableTypeWithModifier (None, ClassHeaderType ["Set"]))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature08 () =
        let result = run signature """SetRoster(listOfSets: *Set)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["SetRoster"],
   [([Var "listOfSets"],
     VariableTypeWithModifier (Some Many, ClassHeaderType ["Set"]))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature09 () =
        let result = run signature """VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to])"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["VecAdd"],
   [([Var "from"; Var "to"],
     VariableTypeWithModifier (None, ClassHeaderType ["Nat"]));
    ([Var "v"; Var "w"],
     VariableTypeWithModifier
       (None,
        FplTypeWithRange
          ((TemplateType "tplFieldElem", LeftClosed),
           (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed))))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature10 () =
        let result = run signature """ZeroVectorN(n: Nat, field: Field)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["ZeroVectorN"],
   [([Var "n"], VariableTypeWithModifier (None, ClassHeaderType ["Nat"]));
    ([Var "field"], VariableTypeWithModifier (None, ClassHeaderType ["Field"]))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature11 () =
        let result = run signature """ProceedingResults(p: +pred)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["ProceedingResults"],
   [([Var "p"], VariableTypeWithModifier (Some Many1, PredicateType))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature12 () =
        let result = run signature """Nat(x: @extDecimal)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["Nat"],
   [([Var "x"],
     VariableTypeWithModifier (None, ExtensionType (Extensionname "Decimal")))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature13 () =
        let result = run signature """Add(n,m: Nat)"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["Add"],
   [([Var "n"; Var "m"],
     VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignature14 () =
        let result = run signature """AlgebraicStructure(x: tplSet, ops: +Composition(args: *tplSetElem))"""
        let actual = sprintf "%O" result
        let expected = """Success: Signature
  (AliasedId ["AlgebraicStructure"],
   [([Var "x"], VariableTypeWithModifier (None, TemplateType "tplSet"));
    ([Var "ops"],
     VariableType
       [([Var "args"],
         VariableTypeWithModifier (Some Many, TemplateType "tplSetElem"))])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

