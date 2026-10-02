namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestExpression () =

    [<DataRow("00", "(x = y * z + 1)")>]
    [<DataRow("01", "(x + 1)")>]
    [<DataRow("01a", "( x + 1)")>]
    [<DataRow("01b", "(x + 1 )")>]
    [<DataRow("02", "(1 + x)")>]
    [<TestMethod>]
    member this.TestExpressionSuccess (no:string, expr:string) =
        let result = run (expression .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00a", "(1 + x)")>]
    [<DataRow("00b", "(1)")>]
    [<DataRow("00d", "(1+ )")>]
    [<DataRow("00f", "(1+)")>]
    [<DataRow("01a", "(@1 + x)")>]
    [<DataRow("01b", "(@1)")>]
    [<DataRow("01d", "(@1+ )")>]
    [<DataRow("01f", "(@1+)")>]
    [<DataRow("02a", "(x + 1)")>]
    [<DataRow("02a", "(x + @1)")>]
    [<DataRow("02b", "(x)")>]
    [<DataRow("02d", "(x+ )")>]
    [<DataRow("02f", "(x+)")>]

    [<DataRow("03a", "(1 = x)")>]
    [<DataRow("03b", "(1)")>]
    [<DataRow("04a", "(@1 = x)")>]
    [<DataRow("04b", "(@1)")>]
    [<DataRow("04d", "(@1= )")>]
    [<DataRow("04f", "(@1=)")>]
    [<DataRow("05a", "(x = 1)")>]
    [<DataRow("05a", "(x = @1)")>]
    [<DataRow("05b", "(x)")>]
    [<TestMethod>]
    member this.TestInfixOperationSuccess (no:string, expr:string) =
        let result = run (pInfixExpr .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("a01", "1")>]
    [<DataRow("a01", "f()")>]
    [<DataRow("a02", "x ∧ y")>]
    [<DataRow("a03", "false ∧ true")>]
    [<DataRow("a01d", "-false")>]
    [<DataRow("a01e", "false'")>]
    [<DataRow("a02d", "-and(a,b)")>]
    [<DataRow("a02e", "and(a,b)'")>]
    [<DataRow("a03d", "-mcases ( | true: true ? false )")>]
    [<DataRow("a03e", "mcases ( | true: true ? false )'")>]
    [<DataRow("00", """PrecedingResults1(x,y)""")>]
    [<DataRow("01", """x + y""")>]
    [<DataRow("02", """(x) + y""")>]
    [<DataRow("03", """(x) + (PrecedingResults1(x,y))""")>]
    [<DataRow("04", """z * (x) + y""")>]
    [<DataRow("04a", """z * (x + y)""")>]
    [<DataRow("05", """z / (x) + y""")>]
    [<DataRow("06", """z / (x - x) + y""")>]
    [<DataRow("07", """¬f ⇒ ¬g""")>]
    [<DataRow("07a", """(¬f ⇒ ¬g)""")>]
    [<DataRow("07b", """(¬f) ⇒ (¬g)""")>]
    [<DataRow("07c", """¬f ⇒ a""")>]
    [<DataRow("07d", """f ⇒ a""")>]
    [<DataRow("07e", """¬(f ⇒ a)""")>]
    [<DataRow("08", """all x:obj {not x}""")>]
    [<DataRow("09", """not x""")>]
    [<DataRow("09a", """¬ x""")>]
    [<DataRow("10a", """x!""")>]
    [<DataRow("10b", """x!!""")>]
    [<DataRow("10c", """(x!)!""")>]
    [<DataRow("11a", """~x""")>]
    [<DataRow("11b", """~~x""")>]
    [<DataRow("11c", """~(~x)""")>]
    [<DataRow("12", """Fact((Fact(a)))""")>]
    [<DataRow("13", """Zero()""")>]
    [<DataRow("14", """self(i)""")>]
    [<DataRow("15", """xor ( xor ( true, xor( true, false)), true )""")>]
    [<DataRow("16", """xor ( true, xor( true, false))""")>]
    [<DataRow("17", """xor ( true, true )""")>]
    [<DataRow("18", """iif ( iif ( true, iif( true, false)), true )""")>]
    [<DataRow("19", """iif ( true, iif( true, false))""")>]
    [<DataRow("20", """iif ( true, true )""")>]
    [<DataRow("21", """iif(true,false)""")>]
    [<DataRow("22", """impl ( impl ( true, impl( true, false)), true )""")>]
    [<DataRow("23", """impl ( true, impl( true, false))""")>]
    [<DataRow("24", """impl ( true, true )""")>]
    [<DataRow("25", """impl(true,false)""")>]
    [<DataRow("26", """or(x.z,y)""")>]
    [<DataRow("27", """or ( or ( true, or( true, false)), true )""")>]
    [<DataRow("28", """or ( true, or( true, false))""")>]
    [<DataRow("29", """or ( true, true )""")>]
    [<DataRow("30", """or(true,false)""")>]
    [<DataRow("31", """and ( and ( true, and( true, false)), true )""")>]
    [<DataRow("32", """and ( true, and( true, false))""")>]
    [<DataRow("33", """and ( true, true )""")>]
    [<DataRow("34", """and(true,false)""")>]
    [<DataRow("35", LiteralUndef)>]
    [<DataRow("36", LiteralFalse)>]
    [<DataRow("37", LiteralTrue)>]
    [<DataRow("38", """myOp.NeutralElement()""")>]
    [<DataRow("39", """myOp.NeutralElement().SomeProperty()""")>]
    [<DataRow("40", """not true""")>]
    [<DataRow("41", """not (iif ( true, not false))""")>]
    [<DataRow("42", """not (iif ( iif( true, false), true))""")>]
    [<DataRow("43", """not iif ( iif ( true, iif( true, false)), not true )""")>]
    [<DataRow("44", """is(x, Nat)""")>]
    [<DataRow("45", """all x,y,z:obj{true}""")>]
    [<DataRow("46", """all x,y,z:obj {not (iif ( true, not false))}""")>]
    [<DataRow("47", """all x,y,z:obj {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("48", """all x:obj {not (iif ( iif ( true, iif( true, false)), not true ))}""")>]
    [<DataRow("49", """ex x,y,z:obj {true }""")>]
    [<DataRow("50", """ex x,y,z:obj { not (iif ( true, not false))}""")>]
    [<DataRow("51", """ex x,y,z:N {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("52", """ex x:G {not (iif ( iif ( true, iif( true, false)), not true ))}""")>]
    [<DataRow("53", """exn$1 x:Nat {not (iif ( true, not (false)))}""")>]
    [<DataRow("54", """exn$2 x: Nat,y:B {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("55", """exn$3 x:Is {not (iif ( iif ( true, iif( true, false)), not true ))}""")>]
    [<DataRow("56", """all arg:Args { is(arg,Set) }""")>]
    [<DataRow("57", """delegate.Abc(x,y,z)""")>]
    [<DataRow("58", """(z = y)""")>]
    [<DataRow("59", """(z @= true)""")>]
    [<DataRow("60", """(z @ true @= and(x,y))""")>]
    [<DataRow("61", """all x:Range, y:C, z:obj {and (and(a,b),c)}""")>]
    [<DataRow("62", """all x:Real, y:pred, z:func {and (and (a,b),c)}""")>]
    [<DataRow("63", """ex x:Range, y:C, z:obj {and (a,and( b,c))}""")>]
    [<DataRow("64", """ex x:Real, y:pred, z:func {and (a,and(b,c))}""")>]
    [<DataRow("65", """not (((x + y)))""")>]
    [<DataRow("66", """impl(T,true)""")>]
    [<DataRow("67", """(x = 0)""")>]
    [<DataRow("68", """(x = 12)""")>]
    [<DataRow("69", """(x = @0)""")>]
    [<DataRow("70", """(x = @12)""")>]
    [<DataRow("71", """parent(x, y).3[a, b]""")>]
    [<DataRow("72", """not x""")>]
    [<TestMethod>]
    member this.TestPredicateSyntaxSuccess (no:string, expr:string) =
        let result = run (predicate .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00c", "(1 + )")>]
    [<DataRow("00e", "(1 +)")>]
    [<DataRow("01", "false()")>]
    [<DataRow("01_", "false ()")>]
    [<DataRow("01a", "false().x[2]")>]
    [<DataRow("01b", "false[2]")>]
    [<DataRow("01c", "false.F().x[2]")>]
    [<DataRow("01c_", "(@1 + )")>]
    [<DataRow("01e", "(@1 +)")>]
    [<DataRow("02", "and(a,b)()")>]
    [<DataRow("02a", "and(a,b)().x[2]")>]
    [<DataRow("02b", "and(a,b).x[2]")>]
    [<DataRow("02c", "and(a,b).F().x[2]")>]
    [<DataRow("02c_", "(x + )")>]
    [<DataRow("02e", "(x +)")>]
    [<DataRow("03", "mcases ( | true: true ? false )()")>]
    [<DataRow("03a", "mcases ( | true: true ? false )().x[2]")>]
    [<DataRow("03b", "mcases ( | true: true ? false )[2]")>]
    [<DataRow("03c", "mcases ( | true: true ? false ).F().x[2]")>]
    [<DataRow("03c_", "(1 = )")>]
    [<DataRow("03d", "(1= )")>]
    [<DataRow("03e", "(1 =)")>]
    [<DataRow("03f", "(1=)")>]
    [<DataRow("04c", "(@1 = )")>]
    [<DataRow("04e", "(@1 =)")>]
    [<DataRow("05c", "(x = )")>]
    [<DataRow("05e", "(x =)")>]
    [<DataRow("05d", "(x= )")>]
    [<DataRow("05f", "(x=)")>]
    [<DataRow("06", "a * b + (c d)")>]
    [<TestMethod>]
    member this.TestPredicateSyntaxFailure (no:string, expr:string) =
        let result = run (predicate .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", "false()")>]
    [<DataRow("01a", "false().x[2]")>]
    [<DataRow("01b", "false.x[2]")>]
    [<DataRow("01c", "false.F().x[2]")>]
    [<TestMethod>]
    member this.TestPredicateWithQualificationSyntaxFailure (no:string, expr:string) =
        let result = run (predicateWithQualification .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", "a * b + (c d)")>]
    [<DataRow("02", "a ∧ ∀ x:obj {x  N}")>]
    [<TestMethod>]
    member this.TestPredicateContentFailure (no:string, expr:string) =
        let result = run (predContent .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", "{a * b + (c d)}")>]
    [<DataRow("02", "{a ∧ ∀ x:obj {x  N}}")>]
    [<DataRow("03", """PrecedingResults$1(x,y)""")>]
    [<DataRow("04", "undet")>]
    [<DataRow("05", """x! !""")>]
    [<DataRow("06", """~ ~x""")>]
    [<DataRow("07", """exn$0 x,y,z:obj(true)""")>]
    [<TestMethod>]
    member this.TestPredicateInstanceBlocktFailure (no:string, expr:string) =
        let result = run (predicateInstanceBlock .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", "{a * b + (c d)}")>]
    [<DataRow("02", "{a ∧ ∀ x:obj {x  N}}")>]
    [<TestMethod>]
    member this.TestPredicateDefinitionBlocktFailure (no:string, expr:string) =
        let result = run (predicateDefinitionBlock .>> eof) expr
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


