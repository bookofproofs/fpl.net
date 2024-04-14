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
        // [DataRow("https://download.visualstudio.microsoft.com/download/pr/d8c23e2d-3942-4fb0-8497-04b9f3d9dd8d/46f2d0088b249ca0f5e3b21e710cab97/dotnet-runtime-6.0.21-win-x64.zip", "localfile.zip")]
        // [DataRow("https://download.visualstudio.microsoft.com/download/pr/25fc0412-b2ff-4868-9920-c087b8a75c55/a95292a725fc37c909c4432c74ecdb43/dotnet-runtime-6.0.21-linux-x64.tar.gz", "localfile.gz")]
        // [DataRow("https://download.visualstudio.microsoft.com/download/pr/af927c74-8c04-4aac-9597-3b56902a812a/47139a25bbc5e58b24fff42f6af0da7c/dotnet-runtime-6.0.21-osx-x64.tar.gz", "localfile.gz")]

        [DataRow("https://download.visualstudio.microsoft.com/download/pr/420ca01f-4528-43c0-893b-321ed0f9087a/c340930ab3e48da2abe868244415c846/dotnet-runtime-8.0.3-win-x64.zip", "localfile.zip")]
        [DataRow("https://download.visualstudio.microsoft.com/download/pr/ed0c9129-950a-48db-80be-e770daf2db41/53879e5802bc6e76bac55c1b8154ed06/dotnet-runtime-8.0.3-linux-x64.tar.gz", "localfile.gz")]
        [DataRow("https://download.visualstudio.microsoft.com/download/pr/564a929b-4f15-490b-895e-5260338cbae1/1db7fd97d0907d3911ac3e4dda32fbb2/dotnet-runtime-8.0.3-osx-x64.tar.gz", "localfile.gz")]
        public async Task TestFileDownload(string url, string localFilePath)
        {
            using (HttpClient client = new HttpClient())
            {
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
