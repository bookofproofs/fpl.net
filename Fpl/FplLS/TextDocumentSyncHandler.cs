using System.Text;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.Embedded.MediatR;
using System;
using static ErrDiagnostics;
using static FplInterpreterTypes;


namespace FplLS
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

    public class TextDocumentSyncHandler(ILanguageServer router, BufferManager bufferManager) : ITextDocumentSyncHandler
    {
        private readonly ILanguageServer _languageServer = router;
        private readonly BufferManager _bufferManager = bufferManager;

        private readonly DocumentSelector _documentSelector = new(
            new DocumentFilter()
            {
                Pattern = "**/*.fpl"
            }
        );
        private SynchronizationCapability? _capability;

        public TextDocumentSyncKind Change { get; } = TextDocumentSyncKind.Full;

        public TextDocumentChangeRegistrationOptions GetRegistrationOptions()
        {
            return new TextDocumentChangeRegistrationOptions()
            {
                DocumentSelector = _documentSelector,
                SyncKind = Change
            };
        }

        public TextDocumentAttributes GetTextDocumentAttributes(Uri uri)
        {
            FplLsTraceLogger.LogMsg(_languageServer, $"{uri.AbsolutePath}", "TextDocumentSyncHandler.GetTextDocumentAttributes");
            return new TextDocumentAttributes(uri, "fpl");
        }

        public Task<Unit> Handle(DidChangeTextDocumentParams request, CancellationToken cancellationToken)
        {

            FplLsTraceLogger.LogMsg(_languageServer, $"{cancellationToken}", "TextDocumentSyncHandler.Handle");
            try
            {
                var uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.AbsoluteUri);
                var text = request.ContentChanges.FirstOrDefault()?.Text;

                FplLsTraceLogger.LogMsg(_languageServer, $"updating buffer", $"TextDocumentSyncHandler.Handle {uri}");
                _bufferManager.UpdateBuffer(uri, new StringBuilder(text));
                FplLsTraceLogger.LogMsg(_languageServer, $"buffer updated", "TextDocumentSyncHandler.Handle");
            }
            catch (Exception ex)
            {
                FplLsTraceLogger.LogException(_languageServer, ex, "TextDocumentSyncHandler.Handle (DidChangeTextDocumentParams)");
            }


            return Unit.Task;
        }

        public Task<Unit> Handle(DidOpenTextDocumentParams request, CancellationToken cancellationToken)
        {
            FplLsTraceLogger.LogMsg(_languageServer, "x(DidOpenTextDocumentParams)", "TextDocumentSyncHandler.Handle");
            try
            {
                var uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.AbsoluteUri);
                FplLsTraceLogger.LogMsg(_languageServer, $"updating buffer (DidOpenTextDocumentParams)", $"TextDocumentSyncHandler.Handle {uri}");
                _bufferManager.UpdateBuffer(uri, new StringBuilder(request.TextDocument.Text));
                FplLsTraceLogger.LogMsg(_languageServer, $"buffer updated (DidOpenTextDocumentParams)", "TextDocumentSyncHandler.Handle");
            }
            catch (Exception ex)
            {
                FplLsTraceLogger.LogException(_languageServer, ex, "TextDocumentSyncHandler.Handle (DidOpenTextDocumentParams)");
            }
            return Unit.Task;
        }

        public Task<Unit> Handle(DidCloseTextDocumentParams request, CancellationToken cancellationToken)
        {
            FplLsTraceLogger.LogMsg(_languageServer, $"y(DidCloseTextDocumentParams)", "TextDocumentSyncHandler.Handle");
            return Unit.Task;
        }

        public Task<Unit> Handle(DidSaveTextDocumentParams request, CancellationToken cancellationToken)
        {
            FplLsTraceLogger.LogMsg(_languageServer, $"z(DidSaveTextDocumentParams)", "TextDocumentSyncHandler.Handle");
            return Unit.Task;
        }

        public void SetCapability(SynchronizationCapability capability)
        {
            FplLsTraceLogger.LogMsg(_languageServer, $"", "TextDocumentSyncHandler.SetCapability");
            _capability = capability;
        }

        TextDocumentRegistrationOptions IRegistration<TextDocumentRegistrationOptions>.GetRegistrationOptions()
        {
            return new TextDocumentChangeRegistrationOptions()
            {
                DocumentSelector = _documentSelector,
                SyncKind = Change
            };
        }

        TextDocumentSaveRegistrationOptions IRegistration<TextDocumentSaveRegistrationOptions>.GetRegistrationOptions()
        {
            return new TextDocumentSaveRegistrationOptions()
            {
                DocumentSelector = _documentSelector
            };
        }
    }
}
