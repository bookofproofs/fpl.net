using System.Text;
using Microsoft.FSharp.Collections;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using static FplParser;

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


        public void PublishDiagnostics(Uri uri, StringBuilder? buffer)
        {
            if (buffer != null)
            {
                var sourceCode = buffer.ToString();
                var parserDiagnostics = FplParser.parserDiagnostics;
                parserDiagnostics.Clear(); // clear last diagnostics before parsing again 
                var ast = FplParser.fplParser(sourceCode);
                var diagnostics = CastDiagnostics(parserDiagnostics.Collection, new TextPositions(sourceCode));
                _languageServer.Document.PublishDiagnostics(new PublishDiagnosticsParams
                {
                    Uri = uri,
                    Diagnostics = diagnostics
                });
            }
            else
            {
                throw new NullReferenceException("buffer parameter was unexpectedly null");
            }
        }

        /// <summary>
        /// Casts a list of F# ErrReccovery module diagnostics into a list of OmniSharp's Diagnostics
        /// </summary>
        /// <param name="diagnostics">Input list</param>
        /// <param name="tp">TextPositions object to handle ranges in the input stream</param>
        /// <returns>Casted list</returns>
        public List<Diagnostic> CastDiagnostics(FSharpList<ErrRecovery.Diagnostic> listDiagnostics, TextPositions tp)
        {
            var sb = new StringBuilder();
            var castedListDiagnostics = new List<Diagnostic>();
            foreach (ErrRecovery.Diagnostic diagnostic in listDiagnostics)
            {
                castedListDiagnostics.Add(CastDiagnostic(diagnostic, tp, sb));
            }
            return castedListDiagnostics;
        }

        /// <summary>
        /// Casts an F# ErrReccovery module diagnostic into the OmniSharp's Diagnostic
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <param name="tp">TextPositions object to handle ranges in the input stream</param>
        /// <returns>Casted diagnostic</returns>
        public Diagnostic CastDiagnostic(ErrRecovery.Diagnostic diagnostic, TextPositions tp, StringBuilder sb)
        {
            var castedDiagnostic = new Diagnostic();
            castedDiagnostic.Source = diagnostic.Emitter.ToString();
            castedDiagnostic.Severity = CastSeverity(diagnostic.Severity);
            castedDiagnostic.Message = CastMessage(diagnostic, sb);
            castedDiagnostic.Range = tp.GetRange(diagnostic.Position.Index, diagnostic.Position.Index);
            castedDiagnostic.Code = CastCode(diagnostic.Code);
            return castedDiagnostic;
        }

        /// <summary>
        /// Casts the error message depending on the emitter and severity
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <param name="sb">A reference to a string builder object that we do not want to recreate all the time for performance reasons.</param>
        /// <returns>A custom diagnostic message.</returns>
        private string CastMessage(ErrRecovery.Diagnostic diagnostic, StringBuilder sb)
        {
            sb.Clear();
            sb.Append(CastDiagnosticCodeMessage(diagnostic));
            return sb.ToString();
        }
        /// <summary>
        /// Returns a message depending on the code of the diagnostic.
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <returns>"semantics " if emitter was Interpreter, "syntax " if emitter was Parser</returns>
        /// <exception cref="NotImplementedException"></exception>
        private string CastDiagnosticCodeMessage(ErrRecovery.Diagnostic diagnostic)
        {
            return diagnostic.Code.CodeMessage;
        }
        /// <summary>
        /// Casts an F# ErrReccovery module severity into the OmniSharp's DiagnosticSeverity
        /// </summary>
        /// <param name="severity">Input severity</param>
        /// <returns>Casted severity</returns>
        private DiagnosticSeverity CastSeverity(ErrRecovery.DiagnosticSeverity severity)
        {
            DiagnosticSeverity castedSeverity = new DiagnosticSeverity();
            if (severity.IsError)
            {
                castedSeverity = DiagnosticSeverity.Error;
            }
            else if (severity.IsWarning)
            {
                castedSeverity = DiagnosticSeverity.Warning;
            }
            else if (severity.IsHint)
            {
                castedSeverity = DiagnosticSeverity.Hint;
            }
            else if (severity.IsInformation)
            {
                castedSeverity = DiagnosticSeverity.Information;
            }
            else
            {
                throw new NotImplementedException(severity.ToString());
            }
            return castedSeverity;
        }

        private DiagnosticCode CastCode(ErrRecovery.DiagnosticCode code)
        {
            return new DiagnosticCode(code.Code.ToString());
        }
    }
}
