using FplLS;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using static FplInterpreterTypes;

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

class CompletionHandler : ICompletionHandler
{

    private readonly ILanguageServer _languageServer;
    private readonly BufferManager _bufferManager;
    private readonly FplAutoCompleteService _fplAutoComplService;

    private readonly DocumentSelector _documentSelector = new DocumentSelector(
        new DocumentFilter()
        {
            Pattern = "**/*.fpl"
        }
    );

    private CompletionCapability _capability;

    public CompletionHandler(ILanguageServer languageServer, BufferManager bufferManager, FplAutoCompleteService fplAutoCompletionService)
    {
        _languageServer = languageServer;
        _bufferManager = bufferManager;
        _fplAutoComplService = fplAutoCompletionService;
        _capability = new CompletionCapability();
    }

    public CompletionRegistrationOptions GetRegistrationOptions()
    {
        return new CompletionRegistrationOptions
        {
            DocumentSelector = _documentSelector,
            ResolveProvider = false
        };
    }

    public async Task<CompletionList> Handle(CompletionParams request, CancellationToken cancellationToken)
    {
        FplLsTraceLogger.LogMsg(_languageServer, "Task<CompletionList>", "CompletionHandler.Handle");
        var uri = FplSources.EscapedUri(request.TextDocument.Uri.AbsoluteUri);
        var buffer = _bufferManager.GetBuffer(uri);
        if (buffer == null)
        {
            return new CompletionList();
        }
        var position = GetPosition(buffer.ToString().Substring(0, buffer.Length),
        (int)request.Position.Line,
        (int)request.Position.Character);

        var complList = await _fplAutoComplService.GetParserChoices(buffer, position, _languageServer);

        return complList;

    }

    private static int GetPosition(string buffer, int line, int col)
    {
        var position = 0;
        for (var i = 0; i < line; i++)
        {
            position = buffer.IndexOf('\n', position) + 1;
        }
        return position + col;
    }

    public void SetCapability(CompletionCapability capability)
    {
        _capability = capability;
    }
}