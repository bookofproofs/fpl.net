namespace FplInterpreter.Tests.Proofs
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Collections.Generic
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.ExpressionMatching
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types4.Proofs
open TestFplInterpreter.Helpers.Common

[<TestClass>]
type TestExpressionMatching() =

    let assertExpressionMatchesPattern (no: string) (fplCode: string) (candidateBlockName: string) (patternBlockName: string) =
        let filename = $"TestExpressionMatching{no}"
        prepareFplCode(filename + ".fpl", fplCode, false)

        try
            checkForUnexpectedErrors filename fplCode

            let theory = heap.Root.Scope[filename]
            let candidateBlock = theory.Scope[candidateBlockName]
            let patternBlock = theory.Scope[patternBlockName]
            let candidateExpr = extractPredicateDefinitionExpressions candidateBlock |> List.head
            let patternExpr = extractPredicateDefinitionExpressions patternBlock |> List.head
            let dictParameterUsage = Dictionary<string, FplGenericNode>()

            let matchResult =
                matchExpressionAgainstPattern candidateExpr patternExpr dictParameterUsage

            match matchResult with
            | Some err ->
                Assert.Fail($"Unexpected mismatch between `{candidateBlockName}` and `{patternBlockName}`: {err}")
            | None -> ()
        finally
            prepareFplCode(filename, "", true) |> ignore

    // The following precedences are copying those defined in the Fpl.Commons library for test purposes.
    // lowest precedence for equality (1-19 is reserved for user-defined)
    let infixEqual = """def pred Equal(x,y: obj) infix "=" 0 {del.Equal(x,y)}"""
    // non-equality higher than equality
    let infixNotEqual = """def pred NotEqual(x,y: obj) infix "≠" 20 { ¬(x = y) }"""
    // comparison higher than non-equality
    let infixGreater = """def pred Greater(x,y: obj) infix ">" 25"""
    // equivalence is weaker than implication
    let infixIif = """def pred Iif(f, g: pred) infix "⇔" 100 {iif(f,g)}"""
    // implication has structural precedence
    let infixImpl = """def pred Impl(f, g: pred) infix "⇒" 110 {impl(f,g)}"""
    // XOR is between implication and OR
    let infixor = """def pred Xor(f, g: pred) infix "⩡" 120 {xor(f,g)}"""
    // OR is weaker than AND
    let infixOr = """def pred Or(f, g: pred) infix "∨" 130 {or(f,g)}"""
    // AND is the most binging among all logic symbols
    let infixAnd = """def pred And(f, g: pred) infix "∧" 140 {and(f,g)}"""
    // Arithmetic + has higher precedence than logical AND
    let infixAdd = """def func Add(f, g: obj)->obj infix "+" 150 """
    // Arithmetic * has higher precedence than arithmetic + 
    let infixMul = """def func Mul(f, g: obj)->obj infix "*" 160"""

    let prefixTilde = """def func Tilde(f: obj)->obj prefix "~" """
    let prefixNabla = """def func Nabla(f: obj)->obj prefix "∇" """
    let postfixPrime = """def func Prime(f: obj)->obj postfix "'" """
    let postfixFact = """def func Fact(f: obj)->obj postfix "!" """

    let allOperators = $"{infixImpl} {infixIif} {infixAnd} {infixOr} {infixor} {infixEqual} {infixNotEqual} {infixGreater} {infixAdd} {infixMul} {prefixTilde} {prefixNabla} {postfixPrime} {postfixFact}"

    [<DataRow("01", """def pred C() {dec x:pred; not x} def pred P() {dec y:pred; not y}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {dec x:pred; ¬x} def pred P() {dec y:pred; not y}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {dec x:pred; ¬x} def pred P() {dec y:pred; ¬y}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {dec x:pred; not x} def pred P() {dec y:pred; ¬y}""", "C()", "P()")>]

    [<DataRow("02", """def pred C() {dec x:pred; not not x} def pred P() {dec y:pred; not not y}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {dec x:pred; ¬¬x} def pred P() {dec y:pred; not not y}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x:pred; ¬¬x} def pred P() {dec y:pred; ¬¬y}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {dec x:pred; not not x} def pred P() {dec y:pred; ¬¬y}""", "C()", "P()")>]

    [<DataRow("03", """def pred C() {dec x:pred; not (x)} def pred P() {dec y:pred; not (y)}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {dec x:pred; ¬(x)} def pred P() {dec y:pred; not (y)}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x:pred; ¬(x)} def pred P() {dec y:pred; ¬(y)}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {dec x:pred; not (x)} def pred P() {dec y:pred; ¬(y)}""", "C()", "P()")>]

    [<DataRow("04", """def pred C() {dec x:pred; not not (x)} def pred P() {dec y:pred; not not (y)}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {dec x:pred; ¬¬(x)} def pred P() {dec y:pred; not not (y)}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x:pred; ¬¬(x)} def pred P() {dec y:pred; ¬¬(y)}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {dec x:pred; not not (x)} def pred P() {dec y:pred; ¬¬(y)}""", "C()", "P()")>]

    [<DataRow("05", """def pred C() {dec x:pred; not (not (x))} def pred P() {dec y:pred; not (not (y))}""", "C()", "P()")>]
    [<DataRow("05a", """def pred C() {dec x:pred; ¬(¬(x))} def pred P() {dec y:pred; not (not (y))}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {dec x:pred; ¬(¬(x))} def pred P() {dec y:pred; ¬(¬(y))}""", "C()", "P()")>]
    [<DataRow("05c", """def pred C() {dec x:pred; not (not (x))} def pred P() {dec y:pred; ¬(¬(y))}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionPrefixNegation(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no fplCode candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:pred; and(x,y)} def pred P() {dec a,b:pred; and(a,b)}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {dec x,y:pred; ∧(x,y)} def pred P() {dec a,b:pred; and(a,b)}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {dec x,y:pred; ∧(x,y)} def pred P() {dec a,b:pred; ∧(a,b)}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {dec x,y:pred; and(x,y)} def pred P() {dec a,b:pred; ∧(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:pred; and(x,y)} def pred P() {dec a,b:pred; a ∧ b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:pred; x ∧ y} def pred P() {dec a,b:pred; and(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:pred; x ∧ y} def pred P() {dec a,b:pred; a ∧ b}""", "C()", "P()")>]
    [<DataRow("01g", """def pred C() {dec x,y:pred; ∧(x,y)} def pred P() {dec a,b:pred; a ∧ b}""", "C()", "P()")>]
    [<DataRow("01h", """def pred C() {dec x,y:pred; x ∧ y} def pred P() {dec a,b:pred; ∧(a,b)}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:pred; and(and(x, y), z)} def pred P() {dec a,b,c:pred; and(and(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {dec x,y,z:pred; ∧(∧(x, y), z)} def pred P() {dec a,b,c:pred; and(and(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:pred; (x ∧ y) ∧ z} def pred P() {dec a,b,c:pred; and(and(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {dec x,y,z:pred; and(and(x, y), z)} def pred P() {dec a,b,c:pred; ∧(∧(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:pred; and(and(x, y), z)} def pred P() {dec a,b,c:pred; (a ∧ b) ∧ c}""", "C()", "P()")>]
    [<DataRow("02e", """def pred C() {dec x,y,z:pred; ∧(∧(x, y), z)} def pred P() {dec a,b,c:pred; ∧(∧(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02f", """def pred C() {dec x,y,z:pred; ∧(∧(x, y), z)} def pred P() {dec a,b,c:pred; (a ∧ b) ∧ c}""", "C()", "P()")>]
    [<DataRow("02g", """def pred C() {dec x,y,z:pred; (x ∧ y) ∧ z} def pred P() {dec a,b,c:pred; ∧(∧(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:pred; (x ∧ y) ∧ z} def pred P() {dec a,b,c:pred; (a ∧ b) ∧ c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:pred; and(x, and(y, z))} def pred P() {dec a,b,c:pred; and(a, and(b, c))}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {dec x,y,z:pred; ∧(x, ∧(y, z))} def pred P() {dec a,b,c:pred; and(a, and(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:pred; x ∧ (y ∧ z)} def pred P() {dec a,b,c:pred; and(a, and(b, c))}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {dec x,y,z:pred; and(x, and(y, z))} def pred P() {dec a,b,c:pred; ∧(a, ∧(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:pred; and(x, and(y, z))} def pred P() {dec a,b,c:pred; a ∧ (b ∧ c)}""", "C()", "P()")>]
    [<DataRow("03e", """def pred C() {dec x,y,z:pred; ∧(x, ∧(y, z))} def pred P() {dec a,b,c:pred; ∧(a, ∧(b, c))}""", "C()", "P()")>]
    [<DataRow("03f", """def pred C() {dec x,y,z:pred; ∧(x, ∧(y, z))} def pred P() {dec a,b,c:pred; a ∧ (b ∧ c)}""", "C()", "P()")>]
    [<DataRow("03g", """def pred C() {dec x,y,z:pred; x ∧ (y ∧ z)} def pred P() {dec a,b,c:pred; ∧(a, ∧(b, c))}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:pred; x ∧ (y ∧ z)} def pred P() {dec a,b,c:pred; a ∧ (b ∧ c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:pred; and(and(x, y), and(z, w))} def pred P() {dec a,b,c,d:pred; and(and(a, b), and(c, d))}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {dec x,y,z,w:pred; ∧(∧(x, y), ∧(z, w))} def pred P() {dec a,b,c,d:pred; and(and(a, b), and(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:pred; (x ∧ y) ∧ (z ∧ w)} def pred P() {dec a,b,c,d:pred; and(and(a, b), and(c, d))}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {dec x,y,z,w:pred; and(and(x, y), and(z, w))} def pred P() {dec a,b,c,d:pred; ∧(∧(a, b), ∧(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:pred; and(and(x, y), and(z, w))} def pred P() {dec a,b,c,d:pred; (a ∧ b) ∧ (c ∧ d)}""", "C()", "P()")>]
    [<DataRow("04e", """def pred C() {dec x,y,z,w:pred; ∧(∧(x, y), ∧(z, w))} def pred P() {dec a,b,c,d:pred; ∧(∧(a, b), ∧(c, d))}""", "C()", "P()")>]
    [<DataRow("04f", """def pred C() {dec x,y,z,w:pred; ∧(∧(x, y), ∧(z, w))} def pred P() {dec a,b,c,d:pred; (a ∧ b) ∧ (c ∧ d)}""", "C()", "P()")>]
    [<DataRow("04g", """def pred C() {dec x,y,z,w:pred; (x ∧ y) ∧ (z ∧ w)} def pred P() {dec a,b,c,d:pred; ∧(∧(a, b), ∧(c, d))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:pred; (x ∧ y) ∧ (z ∧ w)} def pred P() {dec a,b,c,d:pred; (a ∧ b) ∧ (c ∧ d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixConjunction(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixAnd} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:pred; or(x,y)} def pred P() {dec a,b:pred; or(a,b)}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {dec x,y:pred; ∨(x,y)} def pred P() {dec a,b:pred; or(a,b)}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {dec x,y:pred; ∨(x,y)} def pred P() {dec a,b:pred; ∨(a,b)}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {dec x,y:pred; or(x,y)} def pred P() {dec a,b:pred; ∨(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:pred; or(x,y)} def pred P() {dec a,b:pred; a ∨ b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:pred; x ∨ y} def pred P() {dec a,b:pred; or(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:pred; x ∨ y} def pred P() {dec a,b:pred; a ∨ b}""", "C()", "P()")>]
    [<DataRow("01g", """def pred C() {dec x,y:pred; ∨(x,y)} def pred P() {dec a,b:pred; a ∨ b}""", "C()", "P()")>]
    [<DataRow("01h", """def pred C() {dec x,y:pred; x ∨ y} def pred P() {dec a,b:pred; ∨(a,b)}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:pred; or(or(x, y), z)} def pred P() {dec a,b,c:pred; or(or(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {dec x,y,z:pred; ∨(∨(x, y), z)} def pred P() {dec a,b,c:pred; or(or(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:pred; (x ∨ y) ∨ z} def pred P() {dec a,b,c:pred; or(or(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {dec x,y,z:pred; or(or(x, y), z)} def pred P() {dec a,b,c:pred; ∨(∨(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:pred; or(or(x, y), z)} def pred P() {dec a,b,c:pred; (a ∨ b) ∨ c}""", "C()", "P()")>]
    [<DataRow("02e", """def pred C() {dec x,y,z:pred; ∨(∨(x, y), z)} def pred P() {dec a,b,c:pred; ∨(∨(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02f", """def pred C() {dec x,y,z:pred; ∨(∨(x, y), z)} def pred P() {dec a,b,c:pred; (a ∨ b) ∨ c}""", "C()", "P()")>]
    [<DataRow("02g", """def pred C() {dec x,y,z:pred; (x ∨ y) ∨ z} def pred P() {dec a,b,c:pred; ∨(∨(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:pred; (x ∨ y) ∨ z} def pred P() {dec a,b,c:pred; (a ∨ b) ∨ c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:pred; or(x, or(y, z))} def pred P() {dec a,b,c:pred; or(a, or(b, c))}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {dec x,y,z:pred; ∨(x, ∨(y, z))} def pred P() {dec a,b,c:pred; or(a, or(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:pred; x ∨ (y ∨ z)} def pred P() {dec a,b,c:pred; or(a, or(b, c))}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {dec x,y,z:pred; or(x, or(y, z))} def pred P() {dec a,b,c:pred; ∨(a, ∨(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:pred; or(x, or(y, z))} def pred P() {dec a,b,c:pred; a ∨ (b ∨ c)}""", "C()", "P()")>]
    [<DataRow("03e", """def pred C() {dec x,y,z:pred; ∨(x, ∨(y, z))} def pred P() {dec a,b,c:pred; ∨(a, ∨(b, c))}""", "C()", "P()")>]
    [<DataRow("03f", """def pred C() {dec x,y,z:pred; ∨(x, ∨(y, z))} def pred P() {dec a,b,c:pred; a ∨ (b ∨ c)}""", "C()", "P()")>]
    [<DataRow("03g", """def pred C() {dec x,y,z:pred; x ∨ (y ∨ z)} def pred P() {dec a,b,c:pred; ∨(a, ∨(b, c))}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:pred; x ∨ (y ∨ z)} def pred P() {dec a,b,c:pred; a ∨ (b ∨ c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:pred; or(or(x, y), or(z, w))} def pred P() {dec a,b,c,d:pred; or(or(a, b), or(c, d))}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {dec x,y,z,w:pred; ∨(∨(x, y), ∨(z, w))} def pred P() {dec a,b,c,d:pred; or(or(a, b), or(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:pred; (x ∨ y) ∨ (z ∨ w)} def pred P() {dec a,b,c,d:pred; or(or(a, b), or(c, d))}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {dec x,y,z,w:pred; or(or(x, y), or(z, w))} def pred P() {dec a,b,c,d:pred; ∨(∨(a, b), ∨(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:pred; or(or(x, y), or(z, w))} def pred P() {dec a,b,c,d:pred; (a ∨ b) ∨ (c ∨ d)}""", "C()", "P()")>]
    [<DataRow("04e", """def pred C() {dec x,y,z,w:pred; ∨(∨(x, y), ∨(z, w))} def pred P() {dec a,b,c,d:pred; ∨(∨(a, b), ∨(c, d))}""", "C()", "P()")>]
    [<DataRow("04f", """def pred C() {dec x,y,z,w:pred; ∨(∨(x, y), ∨(z, w))} def pred P() {dec a,b,c,d:pred; (a ∨ b) ∨ (c ∨ d)}""", "C()", "P()")>]
    [<DataRow("04g", """def pred C() {dec x,y,z,w:pred; (x ∨ y) ∨ (z ∨ w)} def pred P() {dec a,b,c,d:pred; ∨(∨(a, b), ∨(c, d))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:pred; (x ∨ y) ∨ (z ∨ w)} def pred P() {dec a,b,c,d:pred; (a ∨ b) ∨ (c ∨ d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixDisjunction(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixOr} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:pred; xor(x,y)} def pred P() {dec a,b:pred; xor(a,b)}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {dec x,y:pred; ⩡(x,y)} def pred P() {dec a,b:pred; xor(a,b)}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {dec x,y:pred; ⩡(x,y)} def pred P() {dec a,b:pred; ⩡(a,b)}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {dec x,y:pred; xor(x,y)} def pred P() {dec a,b:pred; ⩡(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:pred; xor(x,y)} def pred P() {dec a,b:pred; a ⩡ b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:pred; x ⩡ y} def pred P() {dec a,b:pred; xor(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:pred; x ⩡ y} def pred P() {dec a,b:pred; a ⩡ b}""", "C()", "P()")>]
    [<DataRow("01g", """def pred C() {dec x,y:pred; ⩡(x,y)} def pred P() {dec a,b:pred; a ⩡ b}""", "C()", "P()")>]
    [<DataRow("01h", """def pred C() {dec x,y:pred; x ⩡ y} def pred P() {dec a,b:pred; ⩡(a,b)}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:pred; xor(xor(x, y), z)} def pred P() {dec a,b,c:pred; xor(xor(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {dec x,y,z:pred; ⩡(⩡(x, y), z)} def pred P() {dec a,b,c:pred; xor(xor(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:pred; (x ⩡ y) ⩡ z} def pred P() {dec a,b,c:pred; xor(xor(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {dec x,y,z:pred; xor(xor(x, y), z)} def pred P() {dec a,b,c:pred; ⩡(⩡(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:pred; xor(xor(x, y), z)} def pred P() {dec a,b,c:pred; (a ⩡ b) ⩡ c}""", "C()", "P()")>]
    [<DataRow("02e", """def pred C() {dec x,y,z:pred; ⩡(⩡(x, y), z)} def pred P() {dec a,b,c:pred; ⩡(⩡(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02f", """def pred C() {dec x,y,z:pred; ⩡(⩡(x, y), z)} def pred P() {dec a,b,c:pred; (a ⩡ b) ⩡ c}""", "C()", "P()")>]
    [<DataRow("02g", """def pred C() {dec x,y,z:pred; (x ⩡ y) ⩡ z} def pred P() {dec a,b,c:pred; ⩡(⩡(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:pred; (x ⩡ y) ⩡ z} def pred P() {dec a,b,c:pred; (a ⩡ b) ⩡ c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:pred; xor(x, xor(y, z))} def pred P() {dec a,b,c:pred; xor(a, xor(b, c))}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {dec x,y,z:pred; ⩡(x, ⩡(y, z))} def pred P() {dec a,b,c:pred; xor(a, xor(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:pred; x ⩡ (y ⩡ z)} def pred P() {dec a,b,c:pred; xor(a, xor(b, c))}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {dec x,y,z:pred; xor(x, xor(y, z))} def pred P() {dec a,b,c:pred; ⩡(a, ⩡(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:pred; xor(x, xor(y, z))} def pred P() {dec a,b,c:pred; a ⩡ (b ⩡ c)}""", "C()", "P()")>]
    [<DataRow("03e", """def pred C() {dec x,y,z:pred; ⩡(x, ⩡(y, z))} def pred P() {dec a,b,c:pred; ⩡(a, ⩡(b, c))}""", "C()", "P()")>]
    [<DataRow("03f", """def pred C() {dec x,y,z:pred; ⩡(x, ⩡(y, z))} def pred P() {dec a,b,c:pred; a ⩡ (b ⩡ c)}""", "C()", "P()")>]
    [<DataRow("03g", """def pred C() {dec x,y,z:pred; x ⩡ (y ⩡ z)} def pred P() {dec a,b,c:pred; ⩡(a, ⩡(b, c))}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:pred; x ⩡ (y ⩡ z)} def pred P() {dec a,b,c:pred; a ⩡ (b ⩡ c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:pred; xor(xor(x, y), xor(z, w))} def pred P() {dec a,b,c,d:pred; xor(xor(a, b), xor(c, d))}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {dec x,y,z,w:pred; ⩡(⩡(x, y), ⩡(z, w))} def pred P() {dec a,b,c,d:pred; xor(xor(a, b), xor(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:pred; (x ⩡ y) ⩡ (z ⩡ w)} def pred P() {dec a,b,c,d:pred; xor(xor(a, b), xor(c, d))}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {dec x,y,z,w:pred; xor(xor(x, y), xor(z, w))} def pred P() {dec a,b,c,d:pred; ⩡(⩡(a, b), ⩡(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:pred; xor(xor(x, y), xor(z, w))} def pred P() {dec a,b,c,d:pred; (a ⩡ b) ⩡ (c ⩡ d)}""", "C()", "P()")>]
    [<DataRow("04e", """def pred C() {dec x,y,z,w:pred; ⩡(⩡(x, y), ⩡(z, w))} def pred P() {dec a,b,c,d:pred; ⩡(⩡(a, b), ⩡(c, d))}""", "C()", "P()")>]
    [<DataRow("04f", """def pred C() {dec x,y,z,w:pred; ⩡(⩡(x, y), ⩡(z, w))} def pred P() {dec a,b,c,d:pred; (a ⩡ b) ⩡ (c ⩡ d)}""", "C()", "P()")>]
    [<DataRow("04g", """def pred C() {dec x,y,z,w:pred; (x ⩡ y) ⩡ (z ⩡ w)} def pred P() {dec a,b,c,d:pred; ⩡(⩡(a, b), ⩡(c, d))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:pred; (x ⩡ y) ⩡ (z ⩡ w)} def pred P() {dec a,b,c,d:pred; (a ⩡ b) ⩡ (c ⩡ d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixExclusiveOr(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixor} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:pred; impl(x,y)} def pred P() {dec a,b:pred; impl(a,b)}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {dec x,y:pred; ⇒(x,y)} def pred P() {dec a,b:pred; impl(a,b)}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {dec x,y:pred; ⇒(x,y)} def pred P() {dec a,b:pred; ⇒(a,b)}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {dec x,y:pred; impl(x,y)} def pred P() {dec a,b:pred; ⇒(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:pred; impl(x,y)} def pred P() {dec a,b:pred; a ⇒ b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:pred; x ⇒ y} def pred P() {dec a,b:pred; impl(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:pred; x ⇒ y} def pred P() {dec a,b:pred; a ⇒ b}""", "C()", "P()")>]
    [<DataRow("01g", """def pred C() {dec x,y:pred; ⇒(x,y)} def pred P() {dec a,b:pred; a ⇒ b}""", "C()", "P()")>]
    [<DataRow("01h", """def pred C() {dec x,y:pred; x ⇒ y} def pred P() {dec a,b:pred; ⇒(a,b)}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:pred; impl(impl(x, y), z)} def pred P() {dec a,b,c:pred; impl(impl(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {dec x,y,z:pred; ⇒(⇒(x, y), z)} def pred P() {dec a,b,c:pred; impl(impl(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:pred; (x ⇒ y) ⇒ z} def pred P() {dec a,b,c:pred; impl(impl(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {dec x,y,z:pred; impl(impl(x, y), z)} def pred P() {dec a,b,c:pred; ⇒(⇒(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:pred; impl(impl(x, y), z)} def pred P() {dec a,b,c:pred; (a ⇒ b) ⇒ c}""", "C()", "P()")>]
    [<DataRow("02e", """def pred C() {dec x,y,z:pred; ⇒(⇒(x, y), z)} def pred P() {dec a,b,c:pred; ⇒(⇒(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02f", """def pred C() {dec x,y,z:pred; ⇒(⇒(x, y), z)} def pred P() {dec a,b,c:pred; (a ⇒ b) ⇒ c}""", "C()", "P()")>]
    [<DataRow("02g", """def pred C() {dec x,y,z:pred; (x ⇒ y) ⇒ z} def pred P() {dec a,b,c:pred; ⇒(⇒(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:pred; (x ⇒ y) ⇒ z} def pred P() {dec a,b,c:pred; (a ⇒ b) ⇒ c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:pred; impl(x, impl(y, z))} def pred P() {dec a,b,c:pred; impl(a, impl(b, c))}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {dec x,y,z:pred; ⇒(x, ⇒(y, z))} def pred P() {dec a,b,c:pred; impl(a, impl(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:pred; x ⇒ (y ⇒ z)} def pred P() {dec a,b,c:pred; impl(a, impl(b, c))}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {dec x,y,z:pred; impl(x, impl(y, z))} def pred P() {dec a,b,c:pred; ⇒(a, ⇒(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:pred; impl(x, impl(y, z))} def pred P() {dec a,b,c:pred; a ⇒ (b ⇒ c)}""", "C()", "P()")>]
    [<DataRow("03e", """def pred C() {dec x,y,z:pred; ⇒(x, ⇒(y, z))} def pred P() {dec a,b,c:pred; ⇒(a, ⇒(b, c))}""", "C()", "P()")>]
    [<DataRow("03f", """def pred C() {dec x,y,z:pred; ⇒(x, ⇒(y, z))} def pred P() {dec a,b,c:pred; a ⇒ (b ⇒ c)}""", "C()", "P()")>]
    [<DataRow("03g", """def pred C() {dec x,y,z:pred; x ⇒ (y ⇒ z)} def pred P() {dec a,b,c:pred; ⇒(a, ⇒(b, c))}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:pred; x ⇒ (y ⇒ z)} def pred P() {dec a,b,c:pred; a ⇒ (b ⇒ c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:pred; impl(impl(x, y), impl(z, w))} def pred P() {dec a,b,c,d:pred; impl(impl(a, b), impl(c, d))}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {dec x,y,z,w:pred; ⇒(⇒(x, y), ⇒(z, w))} def pred P() {dec a,b,c,d:pred; impl(impl(a, b), impl(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:pred; (x ⇒ y) ⇒ (z ⇒ w)} def pred P() {dec a,b,c,d:pred; impl(impl(a, b), impl(c, d))}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {dec x,y,z,w:pred; impl(impl(x, y), impl(z, w))} def pred P() {dec a,b,c,d:pred; ⇒(⇒(a, b), ⇒(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:pred; impl(impl(x, y), impl(z, w))} def pred P() {dec a,b,c,d:pred; (a ⇒ b) ⇒ (c ⇒ d)}""", "C()", "P()")>]
    [<DataRow("04e", """def pred C() {dec x,y,z,w:pred; ⇒(⇒(x, y), ⇒(z, w))} def pred P() {dec a,b,c,d:pred; ⇒(⇒(a, b), ⇒(c, d))}""", "C()", "P()")>]
    [<DataRow("04f", """def pred C() {dec x,y,z,w:pred; ⇒(⇒(x, y), ⇒(z, w))} def pred P() {dec a,b,c,d:pred; (a ⇒ b) ⇒ (c ⇒ d)}""", "C()", "P()")>]
    [<DataRow("04g", """def pred C() {dec x,y,z,w:pred; (x ⇒ y) ⇒ (z ⇒ w)} def pred P() {dec a,b,c,d:pred; ⇒(⇒(a, b), ⇒(c, d))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:pred; (x ⇒ y) ⇒ (z ⇒ w)} def pred P() {dec a,b,c,d:pred; (a ⇒ b) ⇒ (c ⇒ d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixImplication(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixImpl} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:pred; iif(x,y)} def pred P() {dec a,b:pred; iif(a,b)}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {dec x,y:pred; ⇔(x,y)} def pred P() {dec a,b:pred; iif(a,b)}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {dec x,y:pred; ⇔(x,y)} def pred P() {dec a,b:pred; ⇔(a,b)}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {dec x,y:pred; iif(x,y)} def pred P() {dec a,b:pred; ⇔(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:pred; iif(x,y)} def pred P() {dec a,b:pred; a ⇔ b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:pred; x ⇔ y} def pred P() {dec a,b:pred; iif(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:pred; x ⇔ y} def pred P() {dec a,b:pred; a ⇔ b}""", "C()", "P()")>]
    [<DataRow("01g", """def pred C() {dec x,y:pred; ⇔(x,y)} def pred P() {dec a,b:pred; a ⇔ b}""", "C()", "P()")>]
    [<DataRow("01h", """def pred C() {dec x,y:pred; x ⇔ y} def pred P() {dec a,b:pred; ⇔(a,b)}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:pred; iif(iif(x, y), z)} def pred P() {dec a,b,c:pred; iif(iif(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {dec x,y,z:pred; ⇔(⇔(x, y), z)} def pred P() {dec a,b,c:pred; iif(iif(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:pred; (x ⇔ y) ⇔ z} def pred P() {dec a,b,c:pred; iif(iif(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {dec x,y,z:pred; iif(iif(x, y), z)} def pred P() {dec a,b,c:pred; ⇔(⇔(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:pred; iif(iif(x, y), z)} def pred P() {dec a,b,c:pred; (a ⇔ b) ⇔ c}""", "C()", "P()")>]
    [<DataRow("02e", """def pred C() {dec x,y,z:pred; ⇔(⇔(x, y), z)} def pred P() {dec a,b,c:pred; ⇔(⇔(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02f", """def pred C() {dec x,y,z:pred; ⇔(⇔(x, y), z)} def pred P() {dec a,b,c:pred; (a ⇔ b) ⇔ c}""", "C()", "P()")>]
    [<DataRow("02g", """def pred C() {dec x,y,z:pred; (x ⇔ y) ⇔ z} def pred P() {dec a,b,c:pred; ⇔(⇔(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:pred; (x ⇔ y) ⇔ z} def pred P() {dec a,b,c:pred; (a ⇔ b) ⇔ c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:pred; iif(x, iif(y, z))} def pred P() {dec a,b,c:pred; iif(a, iif(b, c))}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {dec x,y,z:pred; ⇔(x, ⇔(y, z))} def pred P() {dec a,b,c:pred; iif(a, iif(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:pred; x ⇔ (y ⇔ z)} def pred P() {dec a,b,c:pred; iif(a, iif(b, c))}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {dec x,y,z:pred; iif(x, iif(y, z))} def pred P() {dec a,b,c:pred; ⇔(a, ⇔(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:pred; iif(x, iif(y, z))} def pred P() {dec a,b,c:pred; a ⇔ (b ⇔ c)}""", "C()", "P()")>]
    [<DataRow("03e", """def pred C() {dec x,y,z:pred; ⇔(x, ⇔(y, z))} def pred P() {dec a,b,c:pred; ⇔(a, ⇔(b, c))}""", "C()", "P()")>]
    [<DataRow("03f", """def pred C() {dec x,y,z:pred; ⇔(x, ⇔(y, z))} def pred P() {dec a,b,c:pred; a ⇔ (b ⇔ c)}""", "C()", "P()")>]
    [<DataRow("03g", """def pred C() {dec x,y,z:pred; x ⇔ (y ⇔ z)} def pred P() {dec a,b,c:pred; ⇔(a, ⇔(b, c))}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:pred; x ⇔ (y ⇔ z)} def pred P() {dec a,b,c:pred; a ⇔ (b ⇔ c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:pred; iif(iif(x, y), iif(z, w))} def pred P() {dec a,b,c,d:pred; iif(iif(a, b), iif(c, d))}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {dec x,y,z,w:pred; ⇔(⇔(x, y), ⇔(z, w))} def pred P() {dec a,b,c,d:pred; iif(iif(a, b), iif(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:pred; (x ⇔ y) ⇔ (z ⇔ w)} def pred P() {dec a,b,c,d:pred; iif(iif(a, b), iif(c, d))}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {dec x,y,z,w:pred; iif(iif(x, y), iif(z, w))} def pred P() {dec a,b,c,d:pred; ⇔(⇔(a, b), ⇔(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:pred; iif(iif(x, y), iif(z, w))} def pred P() {dec a,b,c,d:pred; (a ⇔ b) ⇔ (c ⇔ d)}""", "C()", "P()")>]
    [<DataRow("04e", """def pred C() {dec x,y,z,w:pred; ⇔(⇔(x, y), ⇔(z, w))} def pred P() {dec a,b,c,d:pred; ⇔(⇔(a, b), ⇔(c, d))}""", "C()", "P()")>]
    [<DataRow("04f", """def pred C() {dec x,y,z,w:pred; ⇔(⇔(x, y), ⇔(z, w))} def pred P() {dec a,b,c,d:pred; (a ⇔ b) ⇔ (c ⇔ d)}""", "C()", "P()")>]
    [<DataRow("04g", """def pred C() {dec x,y,z,w:pred; (x ⇔ y) ⇔ (z ⇔ w)} def pred P() {dec a,b,c,d:pred; ⇔(⇔(a, b), ⇔(c, d))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:pred; (x ⇔ y) ⇔ (z ⇔ w)} def pred P() {dec a,b,c,d:pred; (a ⇔ b) ⇔ (c ⇔ d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixEquivalence(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixIif} {fplCode}" candidateBlockName patternBlockName


    [<DataRow("01", """def pred C() {dec x,y:obj; del.Equal(x,y)} def pred P() {dec a,b:obj; del.Equal(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:obj; del.Equal(x,y)} def pred P() {dec a,b:obj; a = b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:obj; x = y} def pred P() {dec a,b:obj; del.Equal(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:obj; x = y} def pred P() {dec a,b:obj; a = b}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:obj; del.Equal(del.Equal(x, y), z)} def pred P() {dec a,b,c:obj; del.Equal(del.Equal(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:obj; (x = y) = z} def pred P() {dec a,b,c:obj; del.Equal(del.Equal(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:obj; del.Equal(del.Equal(x, y), z)} def pred P() {dec a,b,c:obj; (a = b) = c}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:obj; (x = y) = z} def pred P() {dec a,b,c:obj; (a = b) = c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:obj; del.Equal(x, del.Equal(y, z))} def pred P() {dec a,b,c:obj; del.Equal(a, del.Equal(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:obj; x = (y = z)} def pred P() {dec a,b,c:obj; del.Equal(a, del.Equal(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:obj; del.Equal(x, del.Equal(y, z))} def pred P() {dec a,b,c:obj; a = (b = c)}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:obj; x = (y = z)} def pred P() {dec a,b,c:obj; a = (b = c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:obj; del.Equal(del.Equal(x, y), del.Equal(z, w))} def pred P() {dec a,b,c,d:obj; del.Equal(del.Equal(a, b), del.Equal(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:obj; (x = y) = (z = w)} def pred P() {dec a,b,c,d:obj; del.Equal(del.Equal(a, b), del.Equal(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:obj; del.Equal(del.Equal(x, y), del.Equal(z, w))} def pred P() {dec a,b,c,d:obj; (a = b) = (c = d)}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:obj; (x = y) = (z = w)} def pred P() {dec a,b,c,d:obj; (a = b) = (c = d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixEquality(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixEqual} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:obj; ¬del.Equal(x,y)} def pred P() {dec a,b:obj; ¬del.Equal(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:obj; x ≠ y} def pred P() {dec a,b:obj; a ≠ b}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:obj; ¬del.Equal(¬del.Equal(x, y), z)} def pred P() {dec a,b,c:obj; ¬del.Equal(¬del.Equal(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:obj; (x ≠ y) ≠ z} def pred P() {dec a,b,c:obj; (a ≠ b) ≠ c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:obj; ¬del.Equal(x, ¬del.Equal(y, z))} def pred P() {dec a,b,c:obj; ¬del.Equal(a, ¬del.Equal(b, c))}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:obj; x ≠ (y ≠ z)} def pred P() {dec a,b,c:obj; a ≠ (b ≠ c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:obj; ¬del.Equal(¬del.Equal(x, y), ¬del.Equal(z, w))} def pred P() {dec a,b,c,d:obj; ¬del.Equal(¬del.Equal(a, b), ¬del.Equal(c, d))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:obj; (x ≠ y) ≠ (z ≠ w)} def pred P() {dec a,b,c,d:obj; (a ≠ b) ≠ (c ≠ d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixNotEquality(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixEqual} {infixNotEqual} {fplCode}" candidateBlockName patternBlockName


    [<DataRow("01", """def pred C() {dec x,y:obj; Greater(x,y)} def pred P() {dec a,b:obj; Greater(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:obj; Greater(x,y)} def pred P() {dec a,b:obj; a > b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:obj; x > y} def pred P() {dec a,b:obj; Greater(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:obj; x > y} def pred P() {dec a,b:obj; a > b}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:obj; Greater(Greater(x, y), z)} def pred P() {dec a,b,c:obj; Greater(Greater(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:obj; (x > y) > z} def pred P() {dec a,b,c:obj; Greater(Greater(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:obj; Greater(Greater(x, y), z)} def pred P() {dec a,b,c:obj; (a > b) > c}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:obj; (x > y) > z} def pred P() {dec a,b,c:obj; (a > b) > c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:obj; Greater(x, Greater(y, z))} def pred P() {dec a,b,c:obj; Greater(a, Greater(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:obj; x > (y > z)} def pred P() {dec a,b,c:obj; Greater(a, Greater(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:obj; Greater(x, Greater(y, z))} def pred P() {dec a,b,c:obj; a > (b > c)}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:obj; x > (y > z)} def pred P() {dec a,b,c:obj; a > (b > c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:obj; Greater(Greater(x, y), Greater(z, w))} def pred P() {dec a,b,c,d:obj; Greater(Greater(a, b), Greater(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:obj; (x > y) > (z > w)} def pred P() {dec a,b,c,d:obj; Greater(Greater(a, b), Greater(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:obj; Greater(Greater(x, y), Greater(z, w))} def pred P() {dec a,b,c,d:obj; (a > b) > (c > d)}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:obj; (x > y) > (z > w)} def pred P() {dec a,b,c,d:obj; (a > b) > (c > d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixGreater(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixMul} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:obj; Add(x,y)} def pred P() {dec a,b:obj; Add(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:obj; Add(x,y)} def pred P() {dec a,b:obj; a + b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:obj; x + y} def pred P() {dec a,b:obj; Add(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:obj; x + y} def pred P() {dec a,b:obj; a + b}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:obj; Add(Add(x, y), z)} def pred P() {dec a,b,c:obj; Add(Add(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:obj; (x + y) + z} def pred P() {dec a,b,c:obj; Add(Add(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:obj; Add(Add(x, y), z)} def pred P() {dec a,b,c:obj; (a + b) + c}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:obj; (x + y) + z} def pred P() {dec a,b,c:obj; (a + b) + c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:obj; Add(x, Add(y, z))} def pred P() {dec a,b,c:obj; Add(a, Add(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:obj; x + (y + z)} def pred P() {dec a,b,c:obj; Add(a, Add(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:obj; Add(x, Add(y, z))} def pred P() {dec a,b,c:obj; a + (b + c)}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:obj; x + (y + z)} def pred P() {dec a,b,c:obj; a + (b + c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:obj; Add(Add(x, y), Add(z, w))} def pred P() {dec a,b,c,d:obj; Add(Add(a, b), Add(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:obj; (x + y) + (z + w)} def pred P() {dec a,b,c,d:obj; Add(Add(a, b), Add(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:obj; Add(Add(x, y), Add(z, w))} def pred P() {dec a,b,c,d:obj; (a + b) + (c + d)}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:obj; (x + y) + (z + w)} def pred P() {dec a,b,c,d:obj; (a + b) + (c + d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixAdd(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixAdd} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x,y:obj; Mul(x,y)} def pred P() {dec a,b:obj; Mul(a,b)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x,y:obj; Mul(x,y)} def pred P() {dec a,b:obj; a * b}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x,y:obj; x * y} def pred P() {dec a,b:obj; Mul(a,b)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x,y:obj; x * y} def pred P() {dec a,b:obj; a * b}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x,y,z:obj; Mul(Mul(x, y), z)} def pred P() {dec a,b,c:obj; Mul(Mul(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x,y,z:obj; (x * y) * z} def pred P() {dec a,b,c:obj; Mul(Mul(a, b), c)}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x,y,z:obj; Mul(Mul(x, y), z)} def pred P() {dec a,b,c:obj; (a * b) * c}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x,y,z:obj; (x * y) * z} def pred P() {dec a,b,c:obj; (a * b) * c}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x,y,z:obj; Mul(x, Mul(y, z))} def pred P() {dec a,b,c:obj; Mul(a, Mul(b, c))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x,y,z:obj; x * (y * z)} def pred P() {dec a,b,c:obj; Mul(a, Mul(b, c))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x,y,z:obj; Mul(x, Mul(y, z))} def pred P() {dec a,b,c:obj; a * (b * c)}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x,y,z:obj; x * (y * z)} def pred P() {dec a,b,c:obj; a * (b * c)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x,y,z,w:obj; Mul(Mul(x, y), Mul(z, w))} def pred P() {dec a,b,c,d:obj; Mul(Mul(a, b), Mul(c, d))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x,y,z,w:obj; (x * y) * (z * w)} def pred P() {dec a,b,c,d:obj; Mul(Mul(a, b), Mul(c, d))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x,y,z,w:obj; Mul(Mul(x, y), Mul(z, w))} def pred P() {dec a,b,c,d:obj; (a * b) * (c * d)}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x,y,z,w:obj; (x * y) * (z * w)} def pred P() {dec a,b,c,d:obj; (a * b) * (c * d)}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionInfixMul(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{infixMul} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x:obj; Tilde(x)} def pred P() {dec a:obj; Tilde(a)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x:obj; Tilde(x)} def pred P() {dec a:obj; ~a}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x:obj; ~x} def pred P() {dec a:obj; Tilde(a)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x:obj; ~x} def pred P() {dec a:obj; ~a}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x:obj; Tilde(Tilde(x))} def pred P() {dec a:obj; Tilde(Tilde(a))}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x:obj; ~~x} def pred P() {dec a:obj; Tilde(Tilde(a))}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x:obj; Tilde(Tilde(x))} def pred P() {dec a:obj; ~~a}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x:obj; ~~x} def pred P() {dec a:obj; ~~a}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x:obj; Tilde((Tilde(y)))} def pred P() {dec a:obj; Tilde((Tilde(a)))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x:obj; ~(~x)} def pred P() {dec a:obj; Tilde((Tilde(a)))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x:obj; Tilde((Tilde(y)))} def pred P() {dec a:obj; ~(~a)}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x:obj; ~(~x)} def pred P() {dec a:obj; ~(~a)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x:obj; (Tilde((Tilde(y))))} def pred P() {dec a:obj; (Tilde((Tilde(a))))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x:obj; (~(~x))} def pred P() {dec a:obj; (Tilde((Tilde(a))))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x:obj; (Tilde((Tilde(y))))} def pred P() {dec a:obj; (~(~a))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x:obj; (~(~x))} def pred P() {dec a:obj; (~(~a))}""", "C()", "P()")>]

    [<DataRow("05",  """def pred C() {dec x:obj; (Tilde((Tilde(y))))} def pred P() {dec a:obj; (Tilde((Tilde(a))))}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {dec x:obj; (~(~x))} def pred P() {dec a:obj; (Tilde((Tilde(a))))}""", "C()", "P()")>]
    [<DataRow("05d", """def pred C() {dec x:obj; (Tilde((Tilde(y))))} def pred P() {dec a:obj; (~(~a))}""", "C()", "P()")>]
    [<DataRow("05h", """def pred C() {dec x:obj; (~(~x))} def pred P() {dec a:obj; (~(~a))}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionPrefixTilde(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{prefixTilde} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x:obj; Nabla(x)} def pred P() {dec a:obj; Nabla(a)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x:obj; Nabla(x)} def pred P() {dec a:obj; ∇a}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x:obj; ∇x} def pred P() {dec a:obj; Nabla(a)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x:obj; ∇x} def pred P() {dec a:obj; ∇a}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x:obj; Nabla(Nabla(x))} def pred P() {dec a:obj; Nabla(Nabla(a))}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x:obj; ∇∇x} def pred P() {dec a:obj; Nabla(Nabla(a))}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x:obj; Nabla(Nabla(x))} def pred P() {dec a:obj; ∇∇a}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x:obj; ∇∇x} def pred P() {dec a:obj; ∇∇a}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x:obj; Nabla((Nabla(y)))} def pred P() {dec a:obj; Nabla((Nabla(a)))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x:obj; ∇(∇x)} def pred P() {dec a:obj; Nabla((Nabla(a)))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x:obj; Nabla((Nabla(y)))} def pred P() {dec a:obj; ∇(∇a)}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x:obj; ∇(∇x)} def pred P() {dec a:obj; ∇(∇a)}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x:obj; (Nabla((Nabla(y))))} def pred P() {dec a:obj; (Nabla((Nabla(a))))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x:obj; (∇(∇x))} def pred P() {dec a:obj; (Nabla((Nabla(a))))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x:obj; (Nabla((Nabla(y))))} def pred P() {dec a:obj; (∇(∇a))}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x:obj; (∇(∇x))} def pred P() {dec a:obj; (∇(∇a))}""", "C()", "P()")>]

    [<DataRow("05",  """def pred C() {dec x:obj; Nabla(Nabla((Nabla(y))))} def pred P() {dec a:obj; Nabla(Nabla((Nabla(a))))}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {dec x:obj; ∇(∇(∇x))} def pred P() {dec a:obj; Nabla(Nabla((Nabla(a))))}""", "C()", "P()")>]
    [<DataRow("05d", """def pred C() {dec x:obj; Nabla(Nabla((Nabla(y))))} def pred P() {dec a:obj; ∇(∇(∇a))}""", "C()", "P()")>]
    [<DataRow("05h", """def pred C() {dec x:obj; ∇(∇(∇x))} def pred P() {dec a:obj; ∇(∇(∇a))}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionPrefixNabla(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{prefixNabla} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("01", """def pred C() {dec x:obj; Prime(x)} def pred P() {dec a:obj; Prime(a)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x:obj; Prime(x)} def pred P() {dec a:obj; a'}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x:obj; x'} def pred P() {dec a:obj; Prime(a)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x:obj; x'} def pred P() {dec a:obj; a'}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x:obj; Prime(Prime(x))} def pred P() {dec a:obj; Prime(Prime(a))}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x:obj; x''} def pred P() {dec a:obj; Prime(Prime(a))}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x:obj; Prime(Prime(x))} def pred P() {dec a:obj; a''}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x:obj; x''} def pred P() {dec a:obj; a''}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x:obj; Prime((Prime(y)))} def pred P() {dec a:obj; Prime((Prime(a)))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x:obj; (x')'} def pred P() {dec a:obj; Prime((Prime(a)))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x:obj; Prime((Prime(y)))} def pred P() {dec a:obj; (a')'}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x:obj; (x')'} def pred P() {dec a:obj; (a')'}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x:obj; (Prime((Prime(y))))} def pred P() {dec a:obj; (Prime((Prime(a))))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x:obj; ((x')')} def pred P() {dec a:obj; (Prime((Prime(a))))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x:obj; (Prime((Prime(y))))} def pred P() {dec a:obj; ((a')')}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x:obj; ((x')')} def pred P() {dec a:obj; ((a')')}""", "C()", "P()")>]

    [<DataRow("05",  """def pred C() {dec x:obj; Prime(Prime((Prime(y))))} def pred P() {dec a:obj; Prime(Prime((Prime(a))))}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {dec x:obj; ((x')')'} def pred P() {dec a:obj; Prime(Prime((Prime(a))))}""", "C()", "P()")>]
    [<DataRow("05d", """def pred C() {dec x:obj; Prime(Prime((Prime(y))))} def pred P() {dec a:obj; ((a')')'}""", "C()", "P()")>]
    [<DataRow("05h", """def pred C() {dec x:obj; ((x')')'} def pred P() {dec a:obj; ((a')')'}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionPostfixPrime(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{postfixPrime} {fplCode}" candidateBlockName patternBlockName


    [<DataRow("01", """def pred C() {dec x:obj; Fact(x)} def pred P() {dec a:obj; Fact(a)}""", "C()", "P()")>]
    [<DataRow("01d", """def pred C() {dec x:obj; Fact(x)} def pred P() {dec a:obj; a!}""", "C()", "P()")>]
    [<DataRow("01e", """def pred C() {dec x:obj; x!} def pred P() {dec a:obj; Fact(a)}""", "C()", "P()")>]
    [<DataRow("01f", """def pred C() {dec x:obj; x!} def pred P() {dec a:obj; a!}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {dec x:obj; Fact(Fact(x))} def pred P() {dec a:obj; Fact(Fact(a))}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {dec x:obj; x!!} def pred P() {dec a:obj; Fact(Fact(a))}""", "C()", "P()")>]
    [<DataRow("02d", """def pred C() {dec x:obj; Fact(Fact(x))} def pred P() {dec a:obj; a!!}""", "C()", "P()")>]
    [<DataRow("02h", """def pred C() {dec x:obj; x!!} def pred P() {dec a:obj; a!!}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {dec x:obj; Fact((Fact(y)))} def pred P() {dec a:obj; Fact((Fact(a)))}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {dec x:obj; (x!)!} def pred P() {dec a:obj; Fact((Fact(a)))}""", "C()", "P()")>]
    [<DataRow("03b_", """def pred C() {dec x:obj; (x!)} def pred P() {dec a:obj; (Fact(a))}""", "C()", "P()")>]
    [<DataRow("03d", """def pred C() {dec x:obj; Fact((Fact(y)))} def pred P() {dec a:obj; (a!)!}""", "C()", "P()")>]
    [<DataRow("03h", """def pred C() {dec x:obj; (x!)!} def pred P() {dec a:obj; (a!)!}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {dec x:obj; (Fact((Fact(y))))} def pred P() {dec a:obj; (Fact((Fact(a))))}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {dec x:obj; ((x!)!)} def pred P() {dec a:obj; (Fact((Fact(a))))}""", "C()", "P()")>]
    [<DataRow("04d", """def pred C() {dec x:obj; (Fact((Fact(y))))} def pred P() {dec a:obj; ((a!)!)}""", "C()", "P()")>]
    [<DataRow("04h", """def pred C() {dec x:obj; ((x!)!)} def pred P() {dec a:obj; ((a!)!)}""", "C()", "P()")>]

    [<DataRow("05",  """def pred C() {dec x:obj; Fact(Fact((Fact(y))))} def pred P() {dec a:obj; Fact(Fact((Fact(a))))}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {dec x:obj; ((x!)!)!} def pred P() {dec a:obj; Fact(Fact((Fact(a))))}""", "C()", "P()")>]
    [<DataRow("05d", """def pred C() {dec x:obj; Fact(Fact((Fact(y))))} def pred P() {dec a:obj; ((a!)!)!}""", "C()", "P()")>]
    [<DataRow("05h", """def pred C() {dec x:obj; ((x!)!)!} def pred P() {dec a:obj; ((a!)!)!}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionPostfixFact(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{postfixFact} {fplCode}" candidateBlockName patternBlockName

    [<DataRow("00", """def pred C() {all x:pred {true}} def pred P() {all a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00a", """def pred C() {all x:pred {true}} def pred P() {∀ a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00b", """def pred C() {∀ x:pred {true}} def pred P() {all a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00c", """def pred C() {∀ x:pred {true}} def pred P() {∀ a:pred {true}}""", "C()", "P()")>]

    [<DataRow("01", """def pred C() {all x:obj {true}} def pred P() {all a:obj {true}}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {all x:obj {true}} def pred P() {∀ a:obj {true}}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {∀ x:obj {true}} def pred P() {all a:obj {true}}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {∀ x:obj {true}} def pred P() {∀ a:obj {true}}""", "C()", "P()")>]

    [<DataRow("02", """def pred C() {all x,y:obj {true}} def pred P() {all a,b:obj {true}}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {all x,y:obj {true}} def pred P() {∀ a,b:obj {true}}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {∀ x,y:obj {true}} def pred P() {all a,b:obj {true}}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {∀ x,y:obj {true}} def pred P() {∀ a,b:obj {true}}""", "C()", "P()")>]

    [<DataRow("03", """def pred C() {all x,y,z:obj {true}} def pred P() {all a,b,c:obj {true}}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {all x,y,z:obj {true}} def pred P() {∀ a,b,c:obj {true}}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {∀ x,y,z:obj {true}} def pred P() {all a,b,c:obj {true}}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {∀ x,y,z:obj {true}} def pred P() {∀ a,b,c:obj {true}}""", "C()", "P()")>]

    [<DataRow("04", """def pred C() {all x,y,z:ind {true}} def pred P() {all a,b,c:ind {true}}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {all x,y,z:ind {true}} def pred P() {∀ a,b,c:ind {true}}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {∀ x,y,z:ind {true}} def pred P() {all a,b,c:ind {true}}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {∀ x,y,z:ind {true}} def pred P() {∀ a,b,c:ind {true}}""", "C()", "P()")>]

    [<DataRow("05", """def pred C() {all x:obj {all y:obj {true}}} def pred P() {all a:obj {all b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("05a", """def pred C() {all x:obj {all y:obj {true}}} def pred P() {∀ a:obj {∀ b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {∀ x:obj {∀ y:obj {true}}} def pred P() {all a:obj {all b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("05c", """def pred C() {∀ x:obj {∀ y:obj {true}}} def pred P() {∀ a:obj {∀ b:obj {true}}}""", "C()", "P()")>]

    [<DataRow("06", """def pred C() {all x:obj {(all y:obj {true})}} def pred P() {all a:obj {(all b:obj {true})}}""", "C()", "P()")>]
    [<DataRow("06a", """def pred C() {all x:obj {(all y:obj {true})}} def pred P() {∀ a:obj {(∀ b:obj {true})}}""", "C()", "P()")>]
    [<DataRow("06b", """def pred C() {∀ x:obj {(∀ y:obj {true})}} def pred P() {all a:obj {(all b:obj {true})}}""", "C()", "P()")>]
    [<DataRow("06c", """def pred C() {∀ x:obj {(∀ y:obj {true})}} def pred P() {∀ a:obj {(∀ b:obj {true})}}""", "C()", "P()")>]

    [<DataRow("07", """def pred C() {all x:obj {all y:obj {all z:obj {true}}}} def pred P() {all a:obj {all b:obj {all c:obj {true}}}}""", "C()", "P()")>]
    [<DataRow("07a", """def pred C() {all x:obj {all y:obj {all z:obj {true}}}} def pred P() {∀ a:obj {∀ b:obj {∀ c:obj {true}}}}""", "C()", "P()")>]
    [<DataRow("07b", """def pred C() {∀ x:obj {∀ y:obj {∀ z:obj {true}}}} def pred P() {all a:obj {all b:obj {all c:obj {true}}}}""", "C()", "P()")>]
    [<DataRow("07c", """def pred C() {∀ x:obj {∀ y:obj {∀ z:obj {true}}}} def pred P() {∀ a:obj {∀ b:obj {∀ c:obj {true}}}}""", "C()", "P()")>]

    [<DataRow("08", """def pred C() {all x:obj {(all y:obj {(all z:obj {true})})}} def pred P() {all a:obj {(all b:obj {(all c:obj {true})})}}""", "C()", "P()")>]
    [<DataRow("08a", """def pred C() {all x:obj {(all y:obj {(all z:obj {true})})}} def pred P() {∀ a:obj {(∀ b:obj {(∀ c:obj {true})})}}""", "C()", "P()")>]
    [<DataRow("08b", """def pred C() {∀ x:obj {(∀ y:obj {(∀ z:obj {true})})}} def pred P() {∀ a:obj {(∀ b:obj {(∀ c:obj {true})})}}""", "C()", "P()")>]
    [<DataRow("08c", """def pred C() {∀ x:obj {(∀ y:obj {(∀ z:obj {true})})}} def pred P() {∀ a:obj {(∀ b:obj {(∀ c:obj {true})})}}""", "C()", "P()")>]

    [<DataRow("09", """def pred C() {all x:obj, y:pred {true}} def pred P() {all a:obj, b:pred {true}}""", "C()", "P()")>]
    [<DataRow("09a", """def pred C() {all x:obj, y:pred {true}} def pred P() {∀ a:obj, b:pred {true}}""", "C()", "P()")>]
    [<DataRow("09b", """def pred C() {∀ x:obj, y:pred {true}} def pred P() {all a:obj, b:pred {true}}""", "C()", "P()")>]
    [<DataRow("09c", """def pred C() {∀ x:obj, y:pred {true}} def pred P() {∀ a:obj, b:pred {true}}""", "C()", "P()")>]

    [<DataRow("10", """def pred C() {all y:pred, x:obj {true}} def pred P() {all b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("10a", """def pred C() {all y:pred, x:obj {true}} def pred P() {∀ b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("10b", """def pred C() {∀ y:pred, x:obj {true}} def pred P() {all b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("10c", """def pred C() {∀ y:pred, x:obj {true}} def pred P() {∀ b:pred, a:obj {true}}""", "C()", "P()")>]

    [<DataRow("11", """def pred C() {all x:obj, y:pred, z:ind {true}} def pred P() {all a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]
    [<DataRow("11a", """def pred C() {all x:obj, y:pred, z:ind {true}} def pred P() {∀ a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]
    [<DataRow("11b", """def pred C() {∀ x:obj, y:pred, z:ind {true}} def pred P() {all a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]
    [<DataRow("11c", """def pred C() {∀ x:obj, y:pred, z:ind {true}} def pred P() {∀ a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]

    [<DataRow("12", """def pred C() {all z:ind, y:pred, x:obj {true}} def pred P() {all c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("12a", """def pred C() {all z:ind, y:pred, x:obj {true}} def pred P() {∀ c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("12b", """def pred C() {∀ z:ind, y:pred, x:obj {true}} def pred P() {all c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("12c", """def pred C() {∀ z:ind, y:pred, x:obj {true}} def pred P() {∀ c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionQuantifierAll(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no fplCode candidateBlockName patternBlockName

    [<DataRow("00", """def pred C() {ex x:pred {true}} def pred P() {ex a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00a", """def pred C() {ex x:pred {true}} def pred P() {∃ a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00b", """def pred C() {∃ x:pred {true}} def pred P() {ex a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00c", """def pred C() {∃ x:pred {true}} def pred P() {∃ a:pred {true}}""", "C()", "P()")>]

    [<DataRow("01", """def pred C() {ex x:obj {true}} def pred P() {ex a:obj {true}}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {ex x:obj {true}} def pred P() {∃ a:obj {true}}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {∃ x:obj {true}} def pred P() {ex a:obj {true}}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {∃ x:obj {true}} def pred P() {∃ a:obj {true}}""", "C()", "P()")>]

    [<DataRow("02", """def pred C() {ex x,y:obj {true}} def pred P() {ex a,b:obj {true}}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {ex x,y:obj {true}} def pred P() {∃ a,b:obj {true}}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {∃ x,y:obj {true}} def pred P() {ex a,b:obj {true}}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {∃ x,y:obj {true}} def pred P() {∃ a,b:obj {true}}""", "C()", "P()")>]

    [<DataRow("03", """def pred C() {ex x,y,z:obj {true}} def pred P() {ex a,b,c:obj {true}}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {ex x,y,z:obj {true}} def pred P() {∃ a,b,c:obj {true}}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {∃ x,y,z:obj {true}} def pred P() {ex a,b,c:obj {true}}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {∃ x,y,z:obj {true}} def pred P() {∃ a,b,c:obj {true}}""", "C()", "P()")>]

    [<DataRow("04", """def pred C() {ex x,y,z:ind {true}} def pred P() {ex a,b,c:ind {true}}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {ex x,y,z:ind {true}} def pred P() {∃ a,b,c:ind {true}}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {∃ x,y,z:ind {true}} def pred P() {ex a,b,c:ind {true}}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {∃ x,y,z:ind {true}} def pred P() {∃ a,b,c:ind {true}}""", "C()", "P()")>]

    [<DataRow("05", """def pred C() {ex x:obj {ex y:obj {true}}} def pred P() {ex a:obj {ex b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("05a", """def pred C() {ex x:obj {ex y:obj {true}}} def pred P() {∃ a:obj {∃ b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {∃ x:obj {∃ y:obj {true}}} def pred P() {ex a:obj {ex b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("05c", """def pred C() {∃ x:obj {∃ y:obj {true}}} def pred P() {∃ a:obj {∃ b:obj {true}}}""", "C()", "P()")>]

    [<DataRow("06", """def pred C() {ex x:obj {(ex y:obj {true})}} def pred P() {ex a:obj {(ex b:obj {true})}}""", "C()", "P()")>]
    [<DataRow("06a", """def pred C() {ex x:obj {(ex y:obj {true})}} def pred P() {∃ a:obj {(∃ b:obj {true})}}""", "C()", "P()")>]
    [<DataRow("06b", """def pred C() {∃ x:obj {(∃ y:obj {true})}} def pred P() {ex a:obj {(ex b:obj {true})}}""", "C()", "P()")>]
    [<DataRow("06c", """def pred C() {∃ x:obj {(∃ y:obj {true})}} def pred P() {∃ a:obj {(∃ b:obj {true})}}""", "C()", "P()")>]

    [<DataRow("07", """def pred C() {ex x:obj {ex y:obj {ex z:obj {true}}}} def pred P() {ex a:obj {ex b:obj {ex c:obj {true}}}}""", "C()", "P()")>]
    [<DataRow("07a", """def pred C() {ex x:obj {ex y:obj {ex z:obj {true}}}} def pred P() {∃ a:obj {∃ b:obj {∃ c:obj {true}}}}""", "C()", "P()")>]
    [<DataRow("07b", """def pred C() {∃ x:obj {∃ y:obj {∃ z:obj {true}}}} def pred P() {ex a:obj {ex b:obj {ex c:obj {true}}}}""", "C()", "P()")>]
    [<DataRow("07c", """def pred C() {∃ x:obj {∃ y:obj {∃ z:obj {true}}}} def pred P() {∃ a:obj {∃ b:obj {∃ c:obj {true}}}}""", "C()", "P()")>]

    [<DataRow("08", """def pred C() {ex x:obj {(ex y:obj {(ex z:obj {true})})}} def pred P() {ex a:obj {(ex b:obj {(ex c:obj {true})})}}""", "C()", "P()")>]
    [<DataRow("08a", """def pred C() {ex x:obj {(ex y:obj {(ex z:obj {true})})}} def pred P() {∃ a:obj {(∃ b:obj {(∃ c:obj {true})})}}""", "C()", "P()")>]
    [<DataRow("08b", """def pred C() {∃ x:obj {(∃ y:obj {(∃ z:obj {true})})}} def pred P() {∃ a:obj {(∃ b:obj {(∃ c:obj {true})})}}""", "C()", "P()")>]
    [<DataRow("08c", """def pred C() {∃ x:obj {(∃ y:obj {(∃ z:obj {true})})}} def pred P() {∃ a:obj {(∃ b:obj {(∃ c:obj {true})})}}""", "C()", "P()")>]

    [<DataRow("09", """def pred C() {ex x:obj, y:pred {true}} def pred P() {ex a:obj, b:pred {true}}""", "C()", "P()")>]
    [<DataRow("09a", """def pred C() {ex x:obj, y:pred {true}} def pred P() {∃ a:obj, b:pred {true}}""", "C()", "P()")>]
    [<DataRow("09b", """def pred C() {∃ x:obj, y:pred {true}} def pred P() {ex a:obj, b:pred {true}}""", "C()", "P()")>]
    [<DataRow("09c", """def pred C() {∃ x:obj, y:pred {true}} def pred P() {∃ a:obj, b:pred {true}}""", "C()", "P()")>]

    [<DataRow("10", """def pred C() {ex y:pred, x:obj {true}} def pred P() {ex b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("10a", """def pred C() {ex y:pred, x:obj {true}} def pred P() {∃ b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("10b", """def pred C() {∃ y:pred, x:obj {true}} def pred P() {ex b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("10c", """def pred C() {∃ y:pred, x:obj {true}} def pred P() {∃ b:pred, a:obj {true}}""", "C()", "P()")>]

    [<DataRow("11", """def pred C() {ex x:obj, y:pred, z:ind {true}} def pred P() {ex a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]
    [<DataRow("11a", """def pred C() {ex x:obj, y:pred, z:ind {true}} def pred P() {∃ a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]
    [<DataRow("11b", """def pred C() {∃ x:obj, y:pred, z:ind {true}} def pred P() {ex a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]
    [<DataRow("11c", """def pred C() {∃ x:obj, y:pred, z:ind {true}} def pred P() {∃ a:obj, b:pred, c:ind {true}}""", "C()", "P()")>]

    [<DataRow("12", """def pred C() {ex z:ind, y:pred, x:obj {true}} def pred P() {ex c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("12a", """def pred C() {ex z:ind, y:pred, x:obj {true}} def pred P() {∃ c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("12b", """def pred C() {∃ z:ind, y:pred, x:obj {true}} def pred P() {ex c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<DataRow("12c", """def pred C() {∃ z:ind, y:pred, x:obj {true}} def pred P() {∃ c:ind, b:pred, a:obj {true}}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionQuantifierExists(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no fplCode candidateBlockName patternBlockName

    [<DataRow("00", """def pred C() {exn$1 x:pred {true}} def pred P() {exn$1 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00a", """def pred C() {exn$1 x:pred {true}} def pred P() {∃! a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00b", """def pred C() {∃! x:pred {true}} def pred P() {exn$1 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("00c", """def pred C() {∃! x:pred {true}} def pred P() {∃! a:pred {true}}""", "C()", "P()")>]

    [<DataRow("01",  """def pred C() {exn$1 x:pred {true}} def pred P() {exn$1 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("01a", """def pred C() {exn$1 x:pred {true}} def pred P() {∃! a:pred {true}}""", "C()", "P()")>]
    [<DataRow("01b", """def pred C() {∃! x:pred {true}} def pred P() {exn$1 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("01c", """def pred C() {∃! x:pred {true}} def pred P() {∃! a:pred {true}}""", "C()", "P()")>]

    [<DataRow("02",  """def pred C() {exn$2 x:pred {true}} def pred P() {exn$2 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {exn$2 x:pred {true}} def pred P() {∃!2 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {∃!2 x:pred {true}} def pred P() {exn$2 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {∃!2 x:pred {true}} def pred P() {∃!2 a:pred {true}}""", "C()", "P()")>]

    [<DataRow("03",  """def pred C() {exn$1 x:obj {exn$2 y:pred {true}}} def pred P() {exn$1 a:obj {exn$2 b:pred {true}}}""", "C()", "P()")>]
    [<DataRow("03a", """def pred C() {exn$1 x:obj {exn$2 y:pred {true}}} def pred P() {∃! a:obj {∃!2 b:pred {true}}}""", "C()", "P()")>]
    [<DataRow("03b", """def pred C() {∃! x:obj {∃!2 y:pred {true}}} def pred P() {exn$1 a:obj {exn$2 b:pred {true}}}""", "C()", "P()")>]
    [<DataRow("03c", """def pred C() {∃! x:obj {(∃!2 y:pred {true})}} def pred P() {∃! a:obj {(∃!2 b:pred {true})}}""", "C()", "P()")>]

    [<DataRow("04",  """def pred C() {exn$2 x:pred {exn$1 y:obj {true}}} def pred P() {exn$2 a:pred {exn$1 b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("04a", """def pred C() {exn$2 x:pred {exn$1 y:obj {true}}} def pred P() {∃!2 a:pred {∃! b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("04b", """def pred C() {∃!2 x:pred {∃! y:obj {true}}} def pred P() {exn$2 a:pred {exn$1 b:obj {true}}}""", "C()", "P()")>]
    [<DataRow("04c", """def pred C() {∃!2 x:pred {(∃! y:obj {true})}} def pred P() {∃!2 a:pred {(∃! b:obj {true})}}""", "C()", "P()")>]

    [<DataRow("05",  """def pred C() {exn$1 x:obj {exn$1 y:pred {exn$2 z:ind {true}}}} def pred P() {exn$1 a:obj {exn$1 b:pred {exn$2 c:ind {true}}}}""", "C()", "P()")>]
    [<DataRow("05a", """def pred C() {exn$1 x:obj {exn$1 y:pred {exn$2 z:ind {true}}}} def pred P() {∃! a:obj {∃! b:pred {∃!2 c:ind {true}}}}""", "C()", "P()")>]
    [<DataRow("05b", """def pred C() {∃! x:obj {∃! y:pred {∃!2 z:ind {true}}}} def pred P() {exn$1 a:obj {exn$1 b:pred {exn$2 c:ind {true}}}}""", "C()", "P()")>]
    [<DataRow("05c", """def pred C() {∃! x:obj {(∃! y:pred {(∃!2 z:ind {true})})}} def pred P() {∃! a:obj {(∃! b:pred {(∃!2 c:ind {true})})}}""", "C()", "P()")>]

    [<DataRow("06",  """def pred C() {exn$0 x:pred {true}} def pred P() {exn$0 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("06a", """def pred C() {exn$0 x:pred {true}} def pred P() {∃!0 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("06b",  """def pred C() {∃!0 x:pred {true}} def pred P() {exn$0 a:pred {true}}""", "C()", "P()")>]
    [<DataRow("06c", """def pred C() {∃!0 x:pred {true}} def pred P() {∃!0 a:pred {true}}""", "C()", "P()")>]

    [<TestMethod>]
    member this.TestExpressionQuantifierExistsN(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no fplCode candidateBlockName patternBlockName

    [<DataRow("00", """def cl N def cl M def cl K def pred C() {xor(iif(all x:obj {is(x,N)}, ex y:obj {is(y,M)}), and(or(true, false), not all z:obj {is(z,N)})) } def pred P() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ ((true ∨ false) ∧ ¬∀ z:obj {z is N})}""", "C()", "P()")>]
    [<DataRow("00a", """def cl N def cl M def cl K def pred C() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ ((true ∨ false) ∧ ¬∀ x:obj {x is N})} def pred P() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ ((true ∨ false) ∧ ¬∀ z:obj {z is N})}""", "C()", "P()")>]
    [<DataRow("00b", """def cl N def cl M def cl K def pred C() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ ((true ∨ false) ∧ ¬∀ x:obj {x is N})} def pred P() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ ((true ∨ false) ∧ ¬∀ x:obj {x is N})}""", "C()", "P()")>]
    [<DataRow("01", """def pred C() {dec f:pred; (f ⇒ ((f ⇒ f) ⇒ f)) ⇒ ((f ⇒ (f ⇒ f)) ⇒ (f ⇒ f))} def pred P() {dec f, g, h: pred; (f ⇒ (g ⇒ h)) ⇒ ((f ⇒ g) ⇒ (f ⇒ h))}""", "C()", "P()")>]
    [<DataRow("02",  """def pred C() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ (true ∨ false) ∧ ¬∀ z:obj {z is N}} def pred P() {dec p, q, r: pred; (p ⇔ q) ⩡ r ∧ ¬p}""", "C()", "P()")>]
    [<DataRow("02a", """def pred C() {(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}) ⩡ true ∧ ¬∀ z:obj {z is N}} def pred P() {dec p, q, r: pred; (p ⇔ q) ⩡ r ∧ ¬p}""", "C()", "P()")>]
    [<DataRow("02b", """def pred C() {(false ⇔ ∃ y:obj {y is M}) ⩡ true ∧ ¬false} def pred P() {dec p, q, r: pred; (p ⇔ q) ⩡ r ∧ ¬p}""", "C()", "P()")>]
    [<DataRow("02c", """def pred C() {(false ⇔ false) ⩡ true ∧ ¬false} def pred P() {dec p, q, r: pred; (p ⇔ q) ⩡ r ∧ ¬p}""", "C()", "P()")>]
    [<TestMethod>]
    member this.TestExpressionMixed(no: string, fplCode: string, candidateBlockName: string, patternBlockName: string) =
        assertExpressionMatchesPattern no $"{allOperators} {fplCode}" candidateBlockName patternBlockName

