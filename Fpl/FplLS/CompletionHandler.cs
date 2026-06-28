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

using FplLS;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using static Fpl.Errors.Diagnostics;

public class CompletionHandler : ICompletionHandler
{
    private readonly ILanguageServer _languageServer;
    private readonly BufferManager _bufferManager;
    private readonly FplAutoCompleteService _fplAutoComplService;

    // Updated for OmniSharp 19.9
    private readonly TextDocumentSelector _documentSelector = new(
        new TextDocumentFilter { Pattern = "**/*.fpl" }
    );

    private CompletionCapability _capability = new();

    public CompletionHandler(
        ILanguageServer languageServer,
        BufferManager bufferManager,
        FplAutoCompleteService fplAutoCompletionService)
    {
        _languageServer = languageServer;
        _bufferManager = bufferManager;
        _fplAutoComplService = fplAutoCompletionService;
    }

    public CompletionCapability Capability
    {
        get => _capability;
        set => _capability = value;
    }

    // ---------------------------------------------------------
    // EXPLICIT INTERFACE IMPLEMENTATION (required in OmniSharp 19.9)
    // ---------------------------------------------------------
    CompletionRegistrationOptions
        IRegistration<CompletionRegistrationOptions, CompletionCapability>
        .GetRegistrationOptions(CompletionCapability capability, ClientCapabilities clientCapabilities)
    {
        return new CompletionRegistrationOptions
        {
            DocumentSelector = _documentSelector,
            ResolveProvider = false
        };
    }

    // ---------------------------------------------------------
    // CAPABILITY
    // ---------------------------------------------------------
    public void SetCapability(CompletionCapability capability)
    {
        Capability = capability;
    }

    // ---------------------------------------------------------
    // COMPLETION HANDLER
    // ---------------------------------------------------------
    public async Task<CompletionList> Handle(CompletionParams request, CancellationToken cancellationToken)
    {
        var uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.GetFileSystemPath());
        var buffer = _bufferManager.GetBuffer(uri);

        if (buffer == null)
        {
            return new CompletionList();
        }

        var position = GetPosition(
            buffer.ToString()[..buffer.Length],
            (int)request.Position.Line,
            (int)request.Position.Character
        );

        return await FplAutoCompleteService.GetParserChoices(buffer, position, _languageServer);
    }

    private static int GetPosition(string buffer, int line, int col)
    {
        var pos = 0;
        for (var i = 0; i < line; i++)
        {
            pos = buffer.IndexOf('\n', pos) + 1;
        }
        return pos + col;
    }
}

