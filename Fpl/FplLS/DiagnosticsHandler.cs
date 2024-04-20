using System.Text;
using FParsec;
using Microsoft.FSharp.Collections;
using System.Collections.Generic;
using Model = OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using static FplParser;
using static FplInterpreterTypes;
using static FplInterpreter;
using static FplInterpreterUsesClause;
using static ErrDiagnostics;

namespace FplLS
{
    public class UriDiagnostics
    {
        private readonly Dictionary<Uri, List<Model.Diagnostic>> _diagnostics;

        public UriDiagnostics()
        {
            _diagnostics = new Dictionary<Uri, List<Model.Diagnostic>>();
        }


        public static Uri EscapedUri(Uri oldUri)
        {
            string decodedUriString = Uri.UnescapeDataString(oldUri.AbsoluteUri);
            return new Uri(decodedUriString);
        }
        
        public void AddDiagnostics(Uri uri, Model.Diagnostic diagnostic)
        {
            var key = EscapedUri(uri);
            if (!_diagnostics.ContainsKey(key))
            {
                _diagnostics.Add(key, new List<Model.Diagnostic>());
            }
            _diagnostics[key].Add(diagnostic);
        }

        public Dictionary<Uri, List<Model.Diagnostic>> Enumerator()
        {
            return _diagnostics;
        }

    }


    public class DiagnosticsHandler
    {
        private readonly ILanguageServer _languageServer;
        private readonly BufferManager _bufferManager;
        private readonly ParsedAstList _parsedAstsList;

        public DiagnosticsHandler(ILanguageServer languageServer, BufferManager bufferManager)
        {
            _languageServer = languageServer;
            _bufferManager = bufferManager;
            _parsedAstsList = new ParsedAstList();
        }


        public void PublishDiagnostics(Uri uri, StringBuilder? buffer)
        {
            if (buffer != null)
            {
                try
                {
                    var sourceCode = buffer.ToString();
                    var parserDiagnostics = FplParser.parserDiagnostics;
                    parserDiagnostics.Clear(); // clear last diagnostics before parsing again 
                    var fplLibUri = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib";
                    FplInterpreter.fplInterpreter(sourceCode, uri, fplLibUri, _parsedAstsList, false);
                    var diagnostics = CastDiagnostics(uri, parserDiagnostics.Collection, new TextPositions(sourceCode));
                    foreach (var diagnostic in diagnostics.Enumerator())
                    {
                        _languageServer.Document.PublishDiagnostics(new Model.PublishDiagnosticsParams
                        {
                            Uri = diagnostic.Key,
                            Diagnostics = diagnostic.Value
                        });
                    }
                }
                catch (Exception ex)
                {
                    FplLsTraceLogger.LogException(_languageServer, ex, "PublishDiagnostics");
                }
            }
            else
            {
                FplLsTraceLogger.LogMsg(_languageServer, "buffer was unexpectedly null", "PublishDiagnostics");
            }
        }

        /// <summary>
        /// Casts a list of F# ErrReccovery module diagnostics into a list of OmniSharp's Diagnostics
        /// </summary>
        /// <param name="diagnostics">Input list</param>
        /// <param name="tp">TextPositions object to handle ranges in the input stream</param>
        /// <returns>Casted list</returns>
        public UriDiagnostics CastDiagnostics(Uri uri, FSharpList<ErrDiagnostics.Diagnostic> listDiagnostics, TextPositions tp)
        {
            var sb = new StringBuilder();
            var castedListDiagnostics = new UriDiagnostics();
            foreach (ErrDiagnostics.Diagnostic diagnostic in listDiagnostics)
            {
                castedListDiagnostics.AddDiagnostics(diagnostic.Uri, CastDiagnostic(diagnostic, tp, sb));
            }
            return castedListDiagnostics;
        }

        /// <summary>
        /// Casts an F# ErrReccovery module diagnostic into the OmniSharp's Diagnostic
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <param name="tp">TextPositions object to handle ranges in the input stream</param>
        /// <returns>Casted diagnostic</returns>
        public Model.Diagnostic CastDiagnostic(ErrDiagnostics.Diagnostic diagnostic, TextPositions tp, StringBuilder sb)
        {
            var castedDiagnostic = new Model.Diagnostic();
            castedDiagnostic.Source = diagnostic.Emitter.ToString();
            castedDiagnostic.Severity = CastSeverity(diagnostic.Severity);
            castedDiagnostic.Message = CastMessage(diagnostic, sb);
            castedDiagnostic.Range = tp.GetRange(diagnostic.StartPos.Index, diagnostic.EndPos.Index);
            castedDiagnostic.Code = CastCode(diagnostic.Code.Code);
            return castedDiagnostic;
        }

        /// <summary>
        /// Casts the error message depending on the emitter and severity
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <param name="sb">A reference to a string builder object that we do not want to recreate all the time for performance reasons.</param>
        /// <returns>A custom diagnostic message.</returns>
        private string CastMessage(ErrDiagnostics.Diagnostic diagnostic, StringBuilder sb)
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
        private string CastDiagnosticCodeMessage(ErrDiagnostics.Diagnostic diagnostic)
        {
            return diagnostic.Message;
        }
        /// <summary>
        /// Casts an F# ErrReccovery module severity into the OmniSharp's DiagnosticSeverity
        /// </summary>
        /// <param name="severity">Input severity</param>
        /// <returns>Casted severity</returns>
        private Model.DiagnosticSeverity CastSeverity(ErrDiagnostics.DiagnosticSeverity severity)
        {
            var castedSeverity = new Model.DiagnosticSeverity();
            if (severity.IsError)
            {
                castedSeverity = Model.DiagnosticSeverity.Error;
            }
            else if (severity.IsWarning)
            {
                castedSeverity = Model.DiagnosticSeverity.Warning;
            }
            else if (severity.IsHint)
            {
                castedSeverity = Model.DiagnosticSeverity.Hint;
            }
            else if (severity.IsInformation)
            {
                castedSeverity = Model.DiagnosticSeverity.Information;
            }
            else
            {
                throw new NotImplementedException(severity.ToString());
            }
            return castedSeverity;
        }

        private Model.DiagnosticCode CastCode(string code)
        {
            return new Model.DiagnosticCode(code);
        }
    }
}
