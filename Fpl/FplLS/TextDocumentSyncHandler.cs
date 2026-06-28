// Ignore Spelling: uri

using System.Text;
using MediatR;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;
using static Fpl.Errors.Diagnostics;


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

    using System.Text;
    using MediatR;
    using OmniSharp.Extensions.LanguageServer.Protocol;
    using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
    using OmniSharp.Extensions.LanguageServer.Protocol.Document;
    using OmniSharp.Extensions.LanguageServer.Protocol.Models;
    using OmniSharp.Extensions.LanguageServer.Protocol.Server;
    using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;

    public class TextDocumentSyncHandler : ITextDocumentSyncHandler
    {
        private readonly ILanguageServer _languageServer;
        private readonly BufferManager _bufferManager;

        private readonly TextDocumentSelector _documentSelector = new(
            new TextDocumentFilter { Pattern = "**/*.fpl" }
        );

        private TextSynchronizationCapability? _capability;

        public TextDocumentSyncHandler(
            ILanguageServer languageServer,
            BufferManager bufferManager)
        {
            _languageServer = languageServer;
            _bufferManager = bufferManager;
        }

        public static TextDocumentSyncKind Change => TextDocumentSyncKind.Full;

        // OPEN
        TextDocumentOpenRegistrationOptions
            IRegistration<TextDocumentOpenRegistrationOptions, TextSynchronizationCapability>
            .GetRegistrationOptions(TextSynchronizationCapability capability, ClientCapabilities clientCapabilities)
        {
            return new TextDocumentOpenRegistrationOptions
            {
                DocumentSelector = _documentSelector
            };
        }

        // CHANGE
        TextDocumentChangeRegistrationOptions
            IRegistration<TextDocumentChangeRegistrationOptions, TextSynchronizationCapability>
            .GetRegistrationOptions(TextSynchronizationCapability capability, ClientCapabilities clientCapabilities)
        {
            return new TextDocumentChangeRegistrationOptions
            {
                DocumentSelector = _documentSelector,
                SyncKind = Change
            };
        }

        // CLOSE
        TextDocumentCloseRegistrationOptions
            IRegistration<TextDocumentCloseRegistrationOptions, TextSynchronizationCapability>
            .GetRegistrationOptions(TextSynchronizationCapability capability, ClientCapabilities clientCapabilities)
        {
            return new TextDocumentCloseRegistrationOptions
            {
                DocumentSelector = _documentSelector
            };
        }

        // SAVE
        TextDocumentSaveRegistrationOptions
            IRegistration<TextDocumentSaveRegistrationOptions, TextSynchronizationCapability>
            .GetRegistrationOptions(TextSynchronizationCapability capability, ClientCapabilities clientCapabilities)
        {
            return new TextDocumentSaveRegistrationOptions
            {
                DocumentSelector = _documentSelector
            };
        }

        public void SetCapability(TextSynchronizationCapability capability)
        {
            _capability = capability;
        }

        public TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri)
        {
            return new TextDocumentAttributes(uri, "fpl");
        }

        public Task<Unit> Handle(DidOpenTextDocumentParams request, CancellationToken cancellationToken)
        {
            var uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.GetFileSystemPath());
            _bufferManager.UpdateBuffer(uri, new StringBuilder(request.TextDocument.Text));
            return Unit.Task;
        }

        public Task<Unit> Handle(DidChangeTextDocumentParams request, CancellationToken cancellationToken)
        {
            var uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.GetFileSystemPath());
            var text = request.ContentChanges.FirstOrDefault()?.Text;
            _bufferManager.UpdateBuffer(uri, new StringBuilder(text));
            return Unit.Task;
        }

        public Task<Unit> Handle(DidCloseTextDocumentParams request, CancellationToken cancellationToken)
        {
            return Unit.Task;
        }

        public Task<Unit> Handle(DidSaveTextDocumentParams request, CancellationToken cancellationToken)
        {
            return Unit.Task;
        }
    }

}
