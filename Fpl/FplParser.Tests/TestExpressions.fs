namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestExpressions () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestExpressions () =
        let result = run (ast .>> eof) """
            def pred T1() 
            {
                true
            }

            def pred T1a() 
            {
                not x
            }

            def pred T1b() 
            {
                not (x)
            }

            def pred T2() 
            {
                false
            }

            def pred T3() 
            {
                undef
            }

            def pred T4() 
            {
                1.x
            }

            def pred T5() 
            {
                del.Test()
            }

            def pred T6() 
            {
                $1
            }

            def pred T7() 
            {
                true
            } 

            def pred T8() 
            {
                Test$2$1()
            }
            
            def pred T9() 
            {
                Test$1()
            }
            
            def pred T10() 
            {
                Test
            }

            def pred T11() 
            {
                v
            }

            def pred T12() 
            {
                self
            }

            def pred T13() 
            {
                1
            }


            def pred T11a() 
            {
                v.x
            }

            def pred T12a() 
            {
                self.x
            }

            def pred T10b() 
            {
                Test()
            }

            def pred T11b() 
            {
                v()
            }

            def pred T12b() 
            {
                self()
            }

            def pred T13b() 
            {
                @1()  
            }

            def pred T10c() 
            {
                Test(x,y)
            }

            def pred T11c() 
            {
                v(x,y)
            }

            def pred T12c() 
            {
                self(x,y)
            }

            def pred T13c() 
            {
                @1(x,y)  
            }

            def pred T10d() 
            {
                Test[x,y]
            }

            def pred T11d() 
            {
                v[x,y]
            }

            def pred T12d() 
            {
                self[x,y]
            }

            def pred T13d() 
            {
                @1[x.y]  
            }

            def pred T10e() 
            {
                Test(x,y).parent[a,b]
            }

            def pred T11e() 
            {
                v(x,y).x[a,b]
            }

            def pred T12e() 
            {
                self(x,y).@3[a,b]
            }

            def pred T13e() 
            {
                @1(x,y).T[a,b]
            }

            def pred T10f() 
            {
                Test[x,y].x(a,b)
            }

            def pred T11f() 
            {
                v[x,y].x(a,b)
            }

            def pred T12f() 
            {
                self[x,y].self(a,b)
            }

            def pred T13f() 
            {
                @1[x.y].T(a,b)  
            }

            def pred T14()
            {
                ∅
            }

            def pred T15()
            {
                -x
            }

            def pred T16()
            {
                -(y + x = @2 * x)
            }

            def pred T17()
            {
                (y + x' = @2 * x)'
            }

            def pred T18()
            {
                ex x:Range(a:T), y:C, z:obj {and (and(a,b),c)}
            }

            def pred T19()
            {
                exn$1 x:obj {all y:N {true}}
            }

            def pred T20()
            {
                all x:obj {not x}
            }

            def pred T21()
            {
                and (and(x,y),z)
            }

            def pred T21a()
            {
                not x
            }

            def pred T21b()
            {
                not (x)
            }

            def pred T22()
            {
                xor (xor(x,y),z)
            }

            def pred T23()
            {
                or (or(x,y),z)
            }

            def pred T24()
            {
                iif (x,y)
            }

            def pred T25()
            {
                impl (x,y)
            }

            def pred T26()
            {
                is (x,Nat)
            }

            def cl T27:obj {ctor T27() {dec base.C(a, b, c, d); } }
            ; 
        """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

           
    [<TestMethod>]
    member this.TestPredecence () =
        let result = run (ast .>> eof) """def pred T1() { (x = y * z + 1) };"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))