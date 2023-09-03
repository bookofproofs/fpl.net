using System.Text;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using static FplGrammar;

namespace FplLS
{
    public class DiagnosticsHandler
    {
        private readonly ILanguageServer _languageServer;
        private readonly BufferManager _bufferManager;

        public DiagnosticsHandler(ILanguageServer languageServer, BufferManager bufferManager)
        {
            _languageServer = languageServer;
            _bufferManager = bufferManager;
        }


        public void PublishDiagnostics(Uri uri, StringBuilder buffer)
        {
            var text = buffer.ToString();
            var result = FplGrammar.fplParser(text);
            var textPosition = new TextPositions(text);
            var diagnostics = new List<Diagnostic>();

            if (result.IsSuccess)
            {
                var ast = result;
            } 
            else if (result.IsFailure)
            {
                var failure = result as FParsec.CharParsers.ParserResult<FplGrammarTypes.FplParserResult, Microsoft.FSharp.Core.Unit>.Failure;
                var range = textPosition.GetRange(failure.Item2.Position.Index, failure.Item2.Position.Index);
                var diagnostic = new Diagnostic
                {
                    Message = failure.Item1,
                    Severity = DiagnosticSeverity.Error,
                    Range = range,
                };
                diagnostics.Add(diagnostic);    
            }

            _languageServer.Document.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = diagnostics
            });
        }
        
    }
}
