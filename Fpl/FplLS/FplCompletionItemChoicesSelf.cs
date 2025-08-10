// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

namespace FplLS
{
    public class FplCompletionItemChoicesSelf: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            if (defaultCi.Word == "self")
            {
                var ci = defaultCi.Clone();
                ci.Detail = "self reference";
                ci.InsertText = "self ";
                ci.Label = TokenPrefix + ci.InsertText;
                ci.Kind = CompletionItemKind.Reference;
                ci.SortText = "self01";
                ret.Add(ci);
            }
            if (defaultCi.Word == literalParent)
            {
                var ci = defaultCi.Clone();
                ci.Detail = "parent self reference";
                ci.InsertText = "parent ";
                ci.Label = TokenPrefix + ci.InsertText;
                ci.Kind = CompletionItemKind.Reference;
                ci.SortText = "parent02";
                ret.Add(ci);
            }
            if (defaultCi.Word == literalBase)
            {
                var ci = defaultCi.Clone();
                ci.Detail = "ctor call (parent class)";
                ci.InsertText = literalBase;
                ci.Label = TokenPrefix + ci.InsertText;
                ci.Kind = CompletionItemKind.Reference;
                ci.SortText = "self03";
                ret.Add(ci);
            }
            return ret;
        }
    }
}
