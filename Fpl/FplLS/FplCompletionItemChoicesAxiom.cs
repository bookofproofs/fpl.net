using OmniSharp.Extensions.LanguageServer.Protocol.Models;
namespace FplLS
{
    public class FplCompletionItemChoicesAxiom: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone();
            if (ci.Word.StartsWith("ax"))
            {
                ci.InsertText = $"{ci.Word} SomeFplAxiom (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + "}" + Environment.NewLine + Environment.NewLine;
            }
            else
            {
                ci.InsertText = $"{ci.Word} SomeFplPostulate (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + "}" + Environment.NewLine + Environment.NewLine;
            }
            ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

    }
}
