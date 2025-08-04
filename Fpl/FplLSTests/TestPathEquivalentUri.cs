using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Net.Http;
using System.Threading.Tasks;
using System.IO;
using static ErrDiagnostics;

namespace FplLSTests
{
    [TestClass]
    public class TestPathEquivalentUri
    {
        [TestMethod]
        [DataRow("https://example.com/test.fpl")]
        [DataRow(@"c:\temp\test.fpl")]
        [DataRow(@"d:/temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")]
        [DataRow(@"d:\temp\fpl.net\theories\FoundationsOfAnalysisLandau\repo\Fpl.Commons.fpl")]
        [DataRow(@"file://temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")]
#pragma warning disable CS1998 // Async method lacks 'await' operators and will run synchronously
        public async Task TestIdempotent(string path)
#pragma warning restore CS1998 // Async method lacks 'await' operators and will run synchronously
        {
            var uri = PathEquivalentUri.EscapedUri(path);
            var nextUri = PathEquivalentUri.EscapedUri(uri.AbsoluteUri);
            Assert.AreEqual<PathEquivalentUri>(uri, nextUri);
            Assert.AreEqual<string>(uri.AbsoluteUri, nextUri.AbsoluteUri);
        }

        [TestMethod]
        [DataRow("https://example.com/test.fpl")]
        [DataRow(@"c:\temp\test.fpl")]
        [DataRow(@"d:/temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")]
        [DataRow(@"d:\temp\fpl.net\theories\FoundationsOfAnalysisLandau\repo\Fpl.Commons.fpl")]
        [DataRow(@"file://temp/fpl.net/theories/FoundationsOfAnalysisLandau/repo/Fpl.Commons.fpl")]
#pragma warning disable CS1998 // Async method lacks 'await' operators and will run synchronously
        public async Task TestIdempotentUri(string path)
#pragma warning restore CS1998 // Async method lacks 'await' operators and will run synchronously
        {
            var uri = new Uri(path);
            var nextUri = new Uri(uri.AbsoluteUri);
            Assert.AreEqual<Uri>(uri, nextUri);
            Assert.AreEqual<string>(uri.AbsoluteUri, nextUri.AbsoluteUri);
        }
    }
}
