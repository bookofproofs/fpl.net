using Microsoft.Language.Xml;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using UsingMsLangXml = Microsoft.Language.Xml;

namespace FplLS
{
    public class DiagnosticsHandler
    {
        private readonly ILanguageServer _languageServer;
        private readonly BufferManager _bufferManager;
        private static readonly IReadOnlyCollection<string> TemplatedValues = new[]
        {
            "__replace",
            "space_separated",
            "tag1",
        };

        public DiagnosticsHandler(ILanguageServer languageServer, BufferManager bufferManager)
        {
            _languageServer = languageServer;
            _bufferManager = bufferManager;
        }

        public void PublishDiagnostics(Uri uri, UsingMsLangXml.Buffer buffer)
        {
            var text = buffer.GetText(0, buffer.Length);
            var abstractSyntaxTree = Parser.Parse(buffer);
            var textPosition = new TextPositions(text);
            var diagnostics = new List<Diagnostic>();

            diagnostics.AddRange(NuspecDoesNotContainTemplatedValuesRequirement(abstractSyntaxTree, textPosition));

            _languageServer.Document.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = diagnostics
            });
        }

        private IEnumerable<Diagnostic> NuspecDoesNotContainTemplatedValuesRequirement(XmlDocumentSyntax abstractSyntaxTree, TextPositions textPositions)
        {
            foreach (var node in abstractSyntaxTree.DescendantNodesAndSelf().OfType<XmlTextSyntax>())
            {
                if (!TemplatedValues.Any(x => node.Value.Contains(x, StringComparison.OrdinalIgnoreCase)))
                {
                    continue;
                }
                var range = textPositions.GetRange(node.Start, node.End);
                yield return new Diagnostic
                {
                    Message = "Templated value which should be removed",
                    Severity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity.Error,
                    Range = range,
                };
            }
        }
    }
}
