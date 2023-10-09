﻿using FplLS;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Server;

namespace FplLS
{
    class Program
    {
        /*
        static public void Main(string[] args)
        {
            var result = FplGrammar.fplParser("foo");
            Console.WriteLine(result);
        }
        */

        static async Task Main(string[] args)
        {
            var server = await LanguageServer.From(options =>
                options
                    .WithInput(Console.OpenStandardInput())
                    .WithOutput(Console.OpenStandardOutput())
                    .WithLoggerFactory(new LoggerFactory())
                    .AddDefaultLoggingProvider()
                    .WithMinimumLogLevel(LogLevel.Error)
                    .WithServices(ConfigureServices)
                    .WithHandler<TextDocumentSyncHandler>()
                    .OnInitialize((s, _) =>
                    {
                        if (s is LanguageServer languageServer)
                        {
                            var serviceProvider = languageServer.Services;
                            var bufferManager = serviceProvider.GetService<BufferManager>();
                            var diagnosticsHandler = serviceProvider.GetService<DiagnosticsHandler>();

                            // Hook up diagnostics
                            bufferManager.BufferUpdated += (__, x) => diagnosticsHandler.PublishDiagnostics(x.Uri, bufferManager.GetBuffer(x.Uri));

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
        }

    }
}