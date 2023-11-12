using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.ComponentModel.Design;
using System.Data;
using System.Diagnostics.Tracing;
using System.Text;


namespace FplLS
{

    public class FplCompletionItemChoicesProof : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetDirectProof(ci); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetEquivalenceProof(ci1); ret.Add(ci1);

            // keywords
            ret.Add(defaultCi);
            return ret;
        }

        private void SetDirectProof(FplCompletionItem ci)
        {
            ci.SortText = "proof01";
            ci.Detail = "direct proof";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                TokenPremise = "pre";
                TokenConclusion = "con";
                ci.Detail = "direct proof (short)";
                ci.SortText = "z" + ci.SortText;
            }
            ci.InsertText =
                $"// direct proof{Environment.NewLine}" +
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t100. {TokenAssume} {TokenPremise}{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- {TokenConclusion}{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t400. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

        private void SetEquivalenceProof(FplCompletionItem ci)
        {
            ci.SortText = "proof02";
            ci.Detail = "equivalence proof";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                ci.Detail = "equivalence proof (short)";
                ci.SortText = "z" + ci.SortText;
            }
            ci.InsertText =
                $"// \"=>\"{Environment.NewLine}" +
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t100. {TokenAssume} true{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- trivial // \"=>\" follows{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t500. {TokenAssume} true{Environment.NewLine}" +
                $"\t600. |- trivial{Environment.NewLine}" +
                $"\t700. |- trivial{Environment.NewLine}" +
                $"\t800. |- trivial // \"<=\" follows{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t900. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

    }
}
