using FplLS;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;

class CompletionHandler : ICompletionHandler
{
    private const string PackageReferenceElement = "PackageReference";
    private const string IncludeAttribute = "Include";
    private const string VersionAttribute = "Version";

    private readonly ILanguageServer _router;
    private readonly BufferManager _bufferManager;
    private readonly FplAutoCompleteService _fplAutoComplService;

    private readonly DocumentSelector _documentSelector = new DocumentSelector(
        new DocumentFilter()
        {
            Pattern = "**/*.fpl"
        }
    );

    private CompletionCapability _capability;

    public CompletionHandler(ILanguageServer router, BufferManager bufferManager, FplAutoCompleteService fplAutoCompletionService)
    {
        _router = router;
        _bufferManager = bufferManager;
        _fplAutoComplService = fplAutoCompletionService;
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
        var uri = request.TextDocument.Uri;
        var buffer = _bufferManager.GetBuffer(uri);

        if (buffer == null)
        {
            return new CompletionList();
        }


        var choices = "aaa,abc,acd,bbb,bac,bca,ccc,cba,cab";
        _router.Window.LogInfo("Trying :" + choices);
        var complList = await _fplAutoComplService.GetParserChoices(choices);

        var ret = new CompletionList(complList.Select(x => new CompletionItem
        {
            Label = x,
            Kind = CompletionItemKind.Reference,
            TextEdit = new TextEdit
            {
                NewText = x,
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
            new Position
            {
                Line = request.Position.Line,
                Character = request.Position.Character - 1
            }, new Position
            {
                Line = request.Position.Line,
                Character = request.Position.Character + 1
            })
            }
        }));

        _router.Window.LogInfo("Receiving :" + ret.Count());
        return ret;

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