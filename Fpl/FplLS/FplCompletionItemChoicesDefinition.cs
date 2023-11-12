using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesDefinition : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci, "Class", false); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetBody(ci1, "Predicate", false); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetBody(ci2, "Function", false); ret.Add(ci2);

            // keyword
            defaultCi.Kind = CompletionItemKind.Keyword;
            var ciK = defaultCi.Clone(); SetBody(ciK, "Class", false); ret.Add(ciK);
            var ciK1 = defaultCi.Clone(); SetBody(ciK1, "Predicate", false); ret.Add(ciK1);
            var ciK2 = defaultCi.Clone(); SetBody(ciK2, "Function", false); ret.Add(ciK2);
            return ret;
        }

        private void SetBody(FplCompletionItem ci, string definitionType, bool forKeyword)
        {
            if (ci.IsShort)
            {
                TokenIntrinsic = "intr";
                TokenObject = "obj";
                TokenFunction = "func";
                TokenPredicate = "pred";
                if (forKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, definitionType);
                    ci.Detail = $"keywords '{ci.Label}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, definitionType) + " ...";
                    ci.Detail = $"{definitionType.ToLower()} definition (short)";
                    ci.SortText = "z" + ci.SortText;
                    ci.InsertText = GetBody(ci, definitionType);
                }
            }
            else
            {
                if (forKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, definitionType);
                    ci.Detail = $"keywords '{ci.Label}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, definitionType) + " ...";
                    ci.Detail = $"{definitionType.ToLower()} definition";
                    ci.SortText = "z" + ci.SortText;
                    ci.InsertText = GetBody(ci, definitionType);
                }
            }
        }

        public string GetLabelKeyword(FplCompletionItem ci, string definitionType)
        {
            switch (definitionType)
            {
                case "Class":
                    ci.SortText = "definition01";
                    return $"{TokenPrefix}{ci.Word} {TokenClass}'";
                case "Function":
                    ci.SortText = "definition03";
                    return $"{TokenPrefix}{ci.Word} {TokenFunction}'";
                case "Predicate":
                default:
                    ci.SortText = "definition02";
                    return $"{TokenPrefix}{ci.Word} {TokenPredicate}'";
            }
        }

        public string GetBody(FplCompletionItem ci, string definitionType)
        {
            string ret;
            switch (definitionType)
            {
                case "Class":
                    ret = $"{GetLabelKeyword(ci, definitionType)} SomeFpl{definitionType}: {TokenObject}{Environment.NewLine}"; 
                    break;
                case "Function":
                    ret = $"{GetLabelKeyword(ci, definitionType)} SomeFpl{definitionType} -> {TokenObject}";
                    break;
                case "Predicate":
                default:
                    ret = $"{ GetLabelKeyword(ci, definitionType)} SomeFpl{definitionType}()";
                    break;

            }
            return ret +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenIntrinsic}{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

    }
}
