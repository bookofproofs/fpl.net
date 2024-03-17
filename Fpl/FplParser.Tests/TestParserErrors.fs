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
        xxx
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestSYN001() =
        let code = SYN001
        printf "Trying %s" code.Message
        let input = """
        ;
        c"""
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestDEF000() =
        let code = DEF000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestPRP000() =
        let code = PRP000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestAXI000() =
        let code = AXI000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestTHM000() =
        let code = THM000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestCOR000() =
        let code = COR000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestLEM000() =
        let code = LEM000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestPPS000() =
        let code = PPS000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestCNJ000() =
        let code = CNJ000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestVAR000() =
        let code = VAR000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestCTR000() =
        let code = CTR000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestPRF000() =
        let code = PRF000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestINF000() =
        let code = INF000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestLOC000() =
        let code = LOC000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestUSE000() =
        let code = USE000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestPRD000() =
        let code = PRD000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestSMT000() =
        let code = SMT000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestAGI000() =
        let code = AGI000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestCAS000() =
        let code = CAS000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestDCS000() =
        let code = DCS000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestASS000() =
        let code = ASS000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestREV000() =
        let code = REV000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestRET000() =
        let code = RET000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestPRE000() =
        let code = PRE000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestCON000() =
        let code = CON000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestTRL000() =
        let code = TRL000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestTYD000() =
        let code = TYD000
        printf "Trying %s" code.Message
        let input = """
        ;
        """
        let ast = fplParser input
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
