namespace FplParser.Tests

open FParsec
open Fpl.Parser.Grammar
open Fpl.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestPredicates () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<DataRow("00", """PrecedingResults1(x,y)""")>]
    [<DataRow("01", """x + y""")>]
    [<DataRow("02", """(x) + y""")>]
    [<DataRow("03", """(x) + (PrecedingResults1(x,y))""")>]
    [<DataRow("04", """z * (x) + y""")>]
    [<DataRow("04", """z * (x + y)""")>]
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
    
    [<TestMethod>]
    member this.TestPredicateSuccess (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", """PrecedingResults$1(x,y)""")>]
    [<DataRow("01", "undet")>]
    [<DataRow("02", """x! !""")>]
    [<DataRow("03", """~ ~x""")>]
    [<TestMethod>]
    member this.TestPredicateFailure (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicate28 () =
        let result = run (predicate .>> eof) """myOp.NeutralElement()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate29 () =
        let result = run (predicate .>> eof) """myOp.NeutralElement().SomeProperty()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate30 () =
        let result = run (predicate .>> eof) """not true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate31 () =
        let result = run (predicate .>> eof) """not (iif ( true, not false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestPredicate32 () =
        let result = run (predicate .>> eof) """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate33 () =
        let result = run (predicate .>> eof) """not iif ( iif ( true, iif( true, false)), not true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate34 () =
        let result = run (predicate .>> eof) """is(x, Nat)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate35 () =
        let result = run (predicate .>> eof) """all x,y,z:obj{true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate36 () =
        let result = run (predicate .>> eof) """all x,y,z:obj {not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate37 () =
        let result = run (predicate .>> eof) """all x,y,z:obj {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate38 () =
        let result = run (predicate .>> eof) """all x:obj {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate39 () =
        let result = run (predicate .>> eof) """ex x,y,z:obj {true }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate40 () =
        let result = run (predicate .>> eof) """ex x,y,z:obj { not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate41 () =
        let result = run (predicate .>> eof) """ex x,y,z:N {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate42 () =
        let result = run (predicate .>> eof) """ex x:G {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate43 () =
        let result = run (predicate .>> eof) """exn$0 x,y,z:obj(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicate44 () =
        let result = run (predicate .>> eof) """exn$1 x:Nat {not (iif ( true, not (false)))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate45 () =
        let result = run (predicate .>> eof) """exn$2 x: Nat,y:B {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate46 () =
        let result = run (predicate .>> eof) """exn$3 x:Is {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate48 () =
        let result = run (predicate .>> eof) """all arg:Args 
				{
					is(arg,Set)
				}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate49 () =
        let result = run (predicate .>> eof) """delegate.Abc(x,y,z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestPredicate50 () =
        let result = run (predicate .>> eof) """(z = y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate51 () =
        let result = run (predicate .>> eof) """(z @= true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate52 () =
        let result = run (predicate .>> eof) """(z @ true @= and(x,y))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate53 () =
        let result = run (predicate .>> eof) """all x:Range, y:C, z:obj {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate54 () =
        let result = run (predicate .>> eof) """all x:Real, y:pred, z:func {and (and (a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate55 () =
        let result = run (predicate .>> eof) """ex x:Range, y:C, z:obj {and (a,and( b,c))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate56 () =
        let result = run (predicate .>> eof) """ex x:Real, y:pred, z:func {and (a,and(b,c))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate57 () =
        let result = run (predicate .>> eof) """not (((x + y)))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestPredicate58 () =
        let result = run (predicate .>> eof) """impl(T,true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestPredicate59 () =
        let result = run (predicate .>> eof) """(x = 0)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate59a () =
        let result = run (predicate .>> eof) """(x = 12)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate60 () =
        let result = run (predicate .>> eof) """(x = @0)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate60a () =
        let result = run (predicate .>> eof) """(x = @12)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
           
    [<TestMethod>]
    member this.TestPredicate61 () =
        let result = run (predicate .>> eof) """parent(x, y).3[a, b]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate62 () =
        let result = run (predicate .>> eof) """not x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
