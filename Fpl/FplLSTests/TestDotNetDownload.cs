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
        [DataRow("https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main/dotnet-runtime-8.0.8-win-x64.zip", "winfile.zip")]
        [DataRow("https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main/dotnet-runtime-8.0.8-linux-x64.tar.gz", "linuxfile.gz")]
        [DataRow("https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main/dotnet-runtime-8.0.8-osx-x64.tar.gz", "osxfile.gz")]
        public async Task TestFileDownload(string url, string localFilePath)
        {
            using (HttpClient client = new HttpClient())
            {
                client.Timeout = TimeSpan.FromSeconds(60);
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
                        //File.Delete(localFilePath);
                    }
                    else
                    {
                        Assert.Fail("File could not be downloaded. Status code: " + response.StatusCode);
                    }
                }
                catch (HttpRequestException ex)
                {
                    // If a HttpRequestException is thrown, the file could not be downloaded
                    Assert.Fail($"File could not be downloaded. {ex.Message}");
                }
            }
        }
    }
}
