namespace FplParser.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open FplParser
open ErrDiagnostics

[<TestClass>]
type TestParserErrors() =
    let filterByErrorCode (input: Diagnostics) errCode =
        input.Collection
        |> List.filter (fun d -> d.Code = errCode)

    [<TestMethod>]
    member this.TestSYN000() =
        let code = SYN000
        printf "Trying %s" code.Message
        let input = """
        def pred T()
        {
            dec ~x:tpl[! ;
            true
        }
        y
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestSYN001() =
        let code = SYN001
        printf "Trying %s" code.Message
        let input = """
        ;
        c"""
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestDEF000() =
        let code = DEF000
        printf "Trying %s" code.Message
        let input = """def cl A
    y
;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestPRP000() =
        let code = PRP000
        printf "Trying %s" code.Message
        let input = """def pred T() {true mand prop pred T(){#}}
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestAXI000() =
        let code = AXI000
        printf "Trying %s" code.Message
        let input = """
        axiom x() { true }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestTHM000() =
        let code = THM000
        printf "Trying %s" code.Message
        let input = """
        thm x() { true }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestCOR000() =
        let code = COR000
        printf "Trying %s" code.Message
        let input = """
        cor x() { true }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestLEM000() =
        let code = LEM000
        printf "Trying %s" code.Message
        let input = """
        lem x() { true }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestPPS000() =
        let code = PPS000
        printf "Trying %s" code.Message
        let input = """
        prop x() { true }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestCNJ000() =
        let code = CNJ000
        printf "Trying %s" code.Message
        let input = """
        conj x() { true }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestVAR000() =
        let code = VAR000
        printf "Trying %s" code.Message
        let input = """
        def pred T()
        {
            dec~x:ind: ;
            true
        }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestCTR000() =
        let code = CTR000
        printf "Trying %s" code.Message
        let input = """def cl T:obj {ctor x() {self}}
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestPRF000() =
        let code = PRF000
        printf "Trying %s" code.Message
        let input = """ prf #
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestINF000() =
        let code = INF000
        printf "Trying %s" code.Message
        let input = """inf D(x) {pre:true con:true};"""
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestLOC000() =
        let code = LOC000
        printf "Trying %s" code.Message
        let input = """loc X#
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestUSE000() =
        let code = USE000
        printf "Trying %s" code.Message
        let input = """uses   R alias %
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestPRD000() =
        let code = PRD000
        printf "Trying %s" code.Message
        let input = """
        def pred T() { and(x,:) }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestSMT000() =
        let code = SMT000
        printf "Trying %s" code.Message
        let input = """
        def pred T() { del.x }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestAGI000() =
        ad.Clear()
        let code = AGI000
        printf "Trying %s" code.Message
        let input = """def pred T() { 1. |- :: };"""
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestCAS000() =
        let code = CAS000
        printf "Trying %s" code.Message
        let input = """def pred T() 
            { 
                dec
                    cases
                    (
                        | dddd
                            self := Ze
                        ? self := Succ(del.decrement(x))
                    )
                ;
                true
             };
            """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestDCS000() =
        let code = DCS000
        printf "Trying %s" code.Message
        let input = """def pred T() 
            { 
                dec
                    cases
                    (
                    | Equal(x,0) :
                        self := Zero()
                    | Equal(x,1) :
                        self := Succ(Zero())
                    | Equal(x,2) :
                        self := Succ(Succ(Zero()))
                    ? self := 
                )
                ;
                true
             };
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestASS000() =
        ad.Clear()
        let code = ASS000
        printf "Trying %s" code.Message
        let input = """proof T$1 { 1. |- assume :: };"""
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestREV000() =
        let code = REV000
        printf "Trying %s" code.Message
        let input = """
            proof T$1
            {
                1. revoke :
            }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestRET000() =
        let code = RET000
        printf "Trying %s" code.Message
        let input = """def func T()->obj {return #}
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestPRE000() =
        let code = PRE000
        printf "Trying %s" code.Message
        let input = """ inf Test() {pre:# con:true}
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

    [<TestMethod>]
    member this.TestCON000() =
        let code = CON000
        printf "Trying %s" code.Message
        let input = """ inf Test() {pre:true con:#}
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.AreEqual<int>(3, result.Length)

    [<TestMethod>]
    member this.TestTYD000() =
        let code = TYD000
        printf "Trying %s" code.Message
        let input = """
        inf D()
        {
            dec ~x ;
            pre:true
            con:true
        }
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode ad code
        Assert.IsTrue(result.Length>0)

