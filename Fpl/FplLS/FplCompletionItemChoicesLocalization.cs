using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesLocalization : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippet
            var insert =
                $"""{defaultCi.Word} iif(x,y) :={Environment.NewLine}!tex: x "\\Leftrightarrow" y{Environment.NewLine}!eng: x " if and only if " y{Environment.NewLine}!ger: x " dann und nur dann " y{Environment.NewLine};""" + Environment.NewLine;
            var ci = defaultCi.WithInsertText(insert)
                              .WithLabel(defaultCi.Label + " ...");
            ret.Add(ci);
            // keywords (immutable)
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;

        }
    }
}
