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
                ci.InsertText = GetBody(ci.Word, "Axiom");
                ci.Label += " ...";
            }
            else
            {
                ci.InsertText = GetBody(ci.Word, "Postulate");
                ci.Label += " ...";
            }
            ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        public static string GetBody(string word, string label)
        {
            return 
                $"{word} SomeFpl{label} (){Environment.NewLine}" + 
                "{" + Environment.NewLine +
                $"\ttrue{Environment.NewLine}" + 
                "}" + Environment.NewLine;

        }
    }
}
