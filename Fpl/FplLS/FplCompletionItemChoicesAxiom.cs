using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesAxiom : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi;
            if (ci.Word.StartsWith(LiteralAx))
            {
                ci = defaultCi
                    .WithInsertText(GetBody(ci.Word, "Axiom"))
                    .WithLabel(defaultCi.Label + " ...");
            }
            else
            {
                ci = defaultCi
                    .WithInsertText(GetBody(ci.Word, "Postulate"))
                    .WithLabel(defaultCi.Label + " ...");
            }
            ret.Add(ci);
            // keywords
            var keyword = defaultCi.WithKeyword();
            ret.Add(keyword);
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
