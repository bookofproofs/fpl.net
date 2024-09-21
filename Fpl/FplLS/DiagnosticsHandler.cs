using Microsoft.Testing.Platform.Extensions.TestHostControllers;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using System;
using System.Linq;
using System.Text;
using static ErrDiagnostics;
using static FplInterpreterTypes;
using Model = OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class UriDiagnostics
    {
        private readonly Dictionary<PathEquivalentUri, List<Model.Diagnostic>> _diagnostics;

        public UriDiagnostics()
        {
            _diagnostics = [];
        }


        public void AddDiagnostics(PathEquivalentUri uri, Model.Diagnostic diagnostic)
        {
            var key = PathEquivalentUri.EscapedUri(uri.AbsoluteUri);
            if (!_diagnostics.TryGetValue(key, out List<Model.Diagnostic>? value))
            {
                value = ([]);
                _diagnostics.Add(key, value);
            }
            value.Add(diagnostic);
        }

        public Dictionary<PathEquivalentUri, List<Model.Diagnostic>> Enumerator()
        {
            return _diagnostics;
        }

    }


    public class DiagnosticsHandler(ILanguageServer languageServer)
    {
        private readonly ILanguageServer _languageServer = languageServer;

        public void PublishDiagnostics(SymbolTable st, PathEquivalentUri uri, StringBuilder? buffer)
        {
            if (buffer != null)
            {
                try
                {
                    var allUris = st.ParsedAsts.Select(pa => pa.Parsing.Uri).ToHashSet();
                    UriDiagnostics diagnostics = RefreshFplDiagnosticsStorage(st, uri, buffer);
                    foreach (var diagnosticsPerUri in diagnostics.Enumerator())
                    {
                        _languageServer.Document.PublishDiagnostics(new Model.PublishDiagnosticsParams
                        {
                            Uri = diagnosticsPerUri.Key,
                            Diagnostics = diagnosticsPerUri.Value
                        });
                        // remove uri path from allUris because we have published them 
                        allUris.Remove(diagnosticsPerUri.Key);
                    }
                    if (diagnostics.Enumerator().Count == 0)
                    {
                        _languageServer.Document.PublishDiagnostics(new Model.PublishDiagnosticsParams
                        {
                            Uri = uri,
                            Diagnostics = new List<Model.Diagnostic>()
                        });
                    }
                    // if they are still remaining allUris then publish empty diagnostics for them
                    // this might happen if the user corrects the last error in the source code, 
                    // leaving it not emitting any more diagnostics. In this case we have to 
                    // delete the last language server diagnostics by explicitly publishing an empty diagnostics model list
                    foreach (var uriPath in allUris)
                    {
                        _languageServer.Document.PublishDiagnostics(new Model.PublishDiagnosticsParams
                        {
                            Uri = uriPath, // reset remaining files
                            Diagnostics = new List<Model.Diagnostic>()
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

        private UriDiagnostics RefreshFplDiagnosticsStorage(SymbolTable st, PathEquivalentUri uri, StringBuilder? buffer)
        {
            FplLsTraceLogger.LogMsg(_languageServer, uri.AbsoluteUri, "Uri in RefreshFplDiagnosticsStorage");
            string sourceCode;
            ArgumentNullException.ThrowIfNull(buffer);
            string bufferSourceCode = buffer.ToString();
            var pa = st.ParsedAsts.FirstOrDefault<ParsedAst>();
            if (pa == null)
            {
                sourceCode = bufferSourceCode;
                FplLsTraceLogger.LogMsg(_languageServer, $"buffer initialized with {uri.AbsoluteUri}", "RefreshFplDiagnosticsStorage");
            }
            else
            {
                FplLsTraceLogger.LogMsg(_languageServer, string.Join(", ", st.ParsedAsts.Select(pa => pa.Parsing.Uri.AbsolutePath)), "st ids in PublishDiagnostics");
                sourceCode = bufferSourceCode;
            }

            var fplLibUri = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib";
            ad.CurrentUri = uri;

            var name = Path.GetFileNameWithoutExtension(uri.AbsoluteUri);
            var idAlreadyFound = st.ParsedAsts.TryFindAstById(name);
            FplInterpreter.fplInterpreter(st, sourceCode, uri, fplLibUri);
            var diagnostics = CastDiagnostics(st);
            return diagnostics;
        }

        /// <summary>
        /// Casts a list of F# ErrReccovery module diagnostics into a list of OmniSharp's Diagnostics
        /// </summary>
        /// <param name="st">Symbol table from the FPL interpreter</param>
        /// <param name="listDiagnostics">List of diagnostics</param>
        /// <param name="origSourceCode">source code of the current file</param>
        /// <returns>Casted list</returns>
        public UriDiagnostics CastDiagnostics(FplInterpreterTypes.SymbolTable st)
        {
            var castedListDiagnostics = new UriDiagnostics();
            var sourceCodes = GetTextPositionsByUri(st);
            FplLsTraceLogger.LogMsg(_languageServer, ad.DiagnosticsToString, "~~~~~Diagnostics Count Orig");
            foreach (ErrDiagnostics.Diagnostic diagnostic in ad.Collection)
            {
                var tpByUri = sourceCodes[diagnostic.Uri];
                castedListDiagnostics.AddDiagnostics(diagnostic.Uri, CastDiagnostic(diagnostic, tpByUri));
            }
            FplLsTraceLogger.LogMsg(_languageServer, ad.Collection.Length.ToString(), "~~~~~Diagnostics Count Orig");
            FplLsTraceLogger.LogMsg(_languageServer, st.TraceStatistics, "~~~~~Statistics");
            foreach (var kvp in castedListDiagnostics.Enumerator())
            {
                FplLsTraceLogger.LogMsg(_languageServer, $"{kvp.Value.Count} diagnostics in {kvp.Key.AbsolutePath}", "~~~~~Diagnostics Count VS Code");
            }
            return castedListDiagnostics;

        }

        private static Dictionary<PathEquivalentUri, TextPositions> GetTextPositionsByUri(SymbolTable st)
        {
            var sourceCodes = st.ParsedAsts.DictionaryOfSUri2FplSourceCode();
            return sourceCodes.Select(x => new KeyValuePair<PathEquivalentUri, TextPositions>(x.Key, new TextPositions(x.Value))).ToDictionary<PathEquivalentUri, TextPositions>();
        }

        /// <summary>
        /// Casts an F# ErrReccovery module diagnostic into the OmniSharp's Diagnostic
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <param name="tp">TextPositions object to handle ranges in the input stream</param>
        /// <returns>Casted diagnostic</returns>
        public static Model.Diagnostic CastDiagnostic(ErrDiagnostics.Diagnostic diagnostic, TextPositions tp)
        {
            var castedDiagnostic = new Model.Diagnostic
            {
                Source = diagnostic.Emitter.ToString(),
                Severity = CastSeverity(diagnostic.Severity),
                Message = CastMessage(diagnostic),
                Range = tp.GetRange(diagnostic.StartPos.Index, diagnostic.EndPos.Index),
                Code = CastCode(diagnostic.Code.Code)
            };
            return castedDiagnostic;
        }

        /// <summary>
        /// Casts the error message depending on the emitter and severity
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <returns>A custom diagnostic message.</returns>
        private static string CastMessage(ErrDiagnostics.Diagnostic diagnostic)
        {
            return CastDiagnosticCodeMessage(diagnostic);
        }
        /// <summary>
        /// Returns a message depending on the code of the diagnostic.
        /// </summary>
        /// <param name="diagnostic">Input diagnostic</param>
        /// <returns>"semantics " if emitter was Interpreter, "syntax " if emitter was Parser</returns>
        /// <exception cref="NotImplementedException"></exception>
        private static string CastDiagnosticCodeMessage(ErrDiagnostics.Diagnostic diagnostic)
        {
            return diagnostic.Message;
        }
        /// <summary>
        /// Casts an F# ErrReccovery module severity into the OmniSharp's DiagnosticSeverity
        /// </summary>
        /// <param name="severity">Input severity</param>
        /// <returns>Casted severity</returns>
        private static Model.DiagnosticSeverity CastSeverity(ErrDiagnostics.DiagnosticSeverity severity)
        {
            Model.DiagnosticSeverity castedSeverity;
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

        private static Model.DiagnosticCode CastCode(string code)
        {
            return new Model.DiagnosticCode(code);
        }
    }
}
