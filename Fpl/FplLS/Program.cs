using FplLS;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Server;
using static FplInterpreterTypes;

namespace FplLS
{
    class Program
    {
        /*
        MIT License

        Copyright (c) 2018 Martin Björkström

        Permission is hereby granted, free of charge, to any person obtaining a copy
        of this software and associated documentation files (the "Software"), to deal
        in the Software without restriction, including without limitation the rights
        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
        copies of the Software, and to permit persons to whom the Software is
        furnished to do so, subject to the following conditions:

        The above copyright notice and this permission notice shall be included in all
        copies or substantial portions of the Software.

        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
        SOFTWARE.  
        */

        static async Task Main()
        {
            var server = await LanguageServer.From(options =>
                options
                    .WithInput(Console.OpenStandardInput())
                    .WithOutput(Console.OpenStandardOutput())
                    .WithLoggerFactory(new LoggerFactory())
                    .AddDefaultLoggingProvider()
                    .WithMinimumLogLevel(LogLevel.Trace)
                    .WithServices(ConfigureServices)
                    .WithHandler<TextDocumentSyncHandler>()
                    .WithHandler<CompletionHandler>()
                    .OnInitialize((s, _) =>
                    {
                        if (s is LanguageServer languageServer)
                        {
                            var serviceProvider = languageServer.Services;
                            var bufferManager = serviceProvider.GetService<BufferManager>();
                            var diagnosticsHandler = serviceProvider.GetService<DiagnosticsHandler>();

                            // Hook up diagnostics
                            bufferManager.BufferUpdated += (__, x) => diagnosticsHandler.PublishDiagnostics(FplSources.EscapedUri(x.Uri.AbsoluteUri), bufferManager.GetBuffer(x.Uri));

                            return Task.CompletedTask;
                        }
                        else
                        {
                            throw new Exception("Failed to cast s to LanguageServer");
                        }
                    })
                );

            await server.WaitForExit;
        }

        static void ConfigureServices(IServiceCollection services)
        {
            services.AddSingleton<BufferManager>();
            services.AddSingleton<DiagnosticsHandler>();
            services.AddSingleton<FplAutoCompleteService>();
        }

    }
}