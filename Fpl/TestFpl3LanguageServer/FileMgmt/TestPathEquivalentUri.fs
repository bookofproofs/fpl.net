namespace TestFpl3LanguageServer.FileMgmt

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl0Base.Errors.Diagnostics

[<TestClass>]
type TestPathEquivalentUri() =

    [<DataRow("https://example.com/test.fpl")>]
    [<DataRow(@"c:\temp\test.fpl")>]
    [<DataRow(@"d:/temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")>]
    [<DataRow(@"d:\temp\fpl.net\theories\FoundationsOfAnalysisLandau\repo\Fpl.Commons.fpl")>]
    [<DataRow(@"file://temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")>]
    [<TestMethod>]
    member this.TestIdempotent(path: string) =
        let uri = PathEquivalentUri.EscapedUri(path)
        let nextUri = PathEquivalentUri.EscapedUri(uri.AbsoluteUri)
        Assert.AreEqual<PathEquivalentUri>(uri, nextUri)
        Assert.AreEqual<string>(uri.AbsoluteUri, nextUri.AbsoluteUri)

    [<DataRow("https://example.com/test.fpl")>]
    [<DataRow(@"c:\temp\test.fpl")>]
    [<DataRow(@"d:/temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")>]
    [<DataRow(@"d:\temp\fpl.net\theories\FoundationsOfAnalysisLandau\repo\Fpl.Commons.fpl")>]
    [<DataRow(@"file://temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")>]
    [<TestMethod>]
    member this.TestIdempotentUri(path: string) =
        let uri = Uri(path)
        let nextUri = Uri(uri.AbsoluteUri)
        Assert.AreEqual<Uri>(uri, nextUri)
        Assert.AreEqual<string>(uri.AbsoluteUri, nextUri.AbsoluteUri)
