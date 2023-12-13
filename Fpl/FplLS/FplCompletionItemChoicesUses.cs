using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesUses: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone();
            ci.InsertText = $"uses SomeFplNamespace{Environment.NewLine}";
            ci.Detail = "uses namespace";
            ci.SortText = "uses01";
            ci.Label = defaultCi.Label + " ...";
            ret.Add(ci);
            var ci1 = defaultCi.Clone();
            ci1.InsertText = $"uses SomeFplNamespace alias Sfn{Environment.NewLine}";
            ci1.Detail = "uses namespace with alias";
            ci1.SortText = "uses02";
            ci1.Label = defaultCi.Label + " ... alias";
            ret.Add(ci1);
            var ci2 = defaultCi.Clone();
            ci2.InsertText = $"uses SomeFplNamespace *{Environment.NewLine}";
            ci2.Detail = "uses namespace (all subspaces)";
            ci2.SortText = "uses02";
            ci2.Label = defaultCi.Label + " (all) ... ";
            ret.Add(ci2);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            defaultCi.SortText = "uses03";
            ret.Add(defaultCi);
            return ret;

        }
    }
}
