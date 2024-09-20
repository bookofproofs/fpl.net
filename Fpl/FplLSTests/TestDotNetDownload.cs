using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Net.Http;
using System.Threading.Tasks;
using System.IO;

namespace FplLSTests
{
    [TestClass]
    public class TestDotNetDownload
    {
        [TestMethod]
        [DataRow("https://download.visualstudio.microsoft.com/download/pr/d9d43c59-b9f4-47b7-a520-da3a7fa255dc/95b26e342a1ecfa29c527faebdc272e4/dotnet-runtime-8.0.8-win-x64.zip", "localfile.zip")]
        [DataRow("https://download.visualstudio.microsoft.com/download/pr/68c87f8a-862c-4870-a792-9c89b3c8aa2d/2319ebfb46d3a903341966586e8b0898/dotnet-runtime-8.0.8-linux-x64.tar.gz", "localfile.gz")]
        [DataRow("https://download.visualstudio.microsoft.com/download/pr/0159972b-a4d6-4683-b32a-9da824d5689e/ffb0784119abf49015be375b5a016413/dotnet-runtime-8.0.8-osx-x64.tar.gz", "localfile.gz")]
        public async Task TestFileDownload(string url, string localFilePath)
        {
            using (HttpClient client = new HttpClient())
            {
                client.Timeout = TimeSpan.FromSeconds(10);
                try
                {
                    var response = await client.GetAsync(url);

                    if (response.IsSuccessStatusCode)
                    {
                        var fileStream = File.Create(localFilePath);
                        await response.Content.CopyToAsync(fileStream);
                        fileStream.Close();

                        // If the file download is successful, the file should exist at the local file path
                        Assert.IsTrue(File.Exists(localFilePath));

                        // Clean up the downloaded file
                        File.Delete(localFilePath);
                    }
                    else
                    {
                        Assert.Fail("File could not be downloaded. Status code: " + response.StatusCode);
                    }
                }
                catch (HttpRequestException)
                {
                    // If a HttpRequestException is thrown, the file could not be downloaded
                    Assert.Fail("File could not be downloaded.");
                }
            }
        }
    }
}
