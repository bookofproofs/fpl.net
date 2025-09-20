using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLS
{
    public class FplCompletionItemChoicesAxiom: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone();
            if (ci.Word.StartsWith(LiteralAx))
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
            if (word == LiteralInf)
            {
                return
                    $"{word} SomeFpl{label}{Environment.NewLine}" +
                    "{" + Environment.NewLine +
                    $"\tpre: true{Environment.NewLine}" +
                    $"\tcon: true{Environment.NewLine}" +
                    "}" + Environment.NewLine;
            }
            if (word == LiteralInfL)
            {
                return
                    $"{word} SomeFpl{label}{Environment.NewLine}" +
                    "{" + Environment.NewLine +
                    $"\tpremise: true{Environment.NewLine}" +
                    $"\tconclusion: true{Environment.NewLine}" +
                    "}" + Environment.NewLine;
            }
            else
            {
                return
                    $"{word} SomeFpl{label}{Environment.NewLine}" +
                    "{" + Environment.NewLine +
                    $"\ttrue{Environment.NewLine}" +
                    "}" + Environment.NewLine;
            }
        }
    }
}
