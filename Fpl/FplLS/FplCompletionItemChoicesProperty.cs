using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesProperty : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci, "Class", false, false); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetBody(ci1, "Predicate", false, false); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetBody(ci2, "Function", false, false); ret.Add(ci2);
            var ci3 = defaultCi.Clone(); SetBody(ci3, "Class", false, true); ret.Add(ci3);
            var ci4 = defaultCi.Clone(); SetBody(ci4, "Predicate", false, true); ret.Add(ci4);
            var ci5 = defaultCi.Clone(); SetBody(ci5, "Function", false, true); ret.Add(ci5);

            // keyword
            var ciK = defaultCi.Clone(); SetBody(ciK, "Class", true, false); ret.Add(ciK);
            var ciK1 = defaultCi.Clone(); SetBody(ciK1, "Predicate", true, false); ret.Add(ciK1);
            var ciK2 = defaultCi.Clone(); SetBody(ciK2, "Function", true, false); ret.Add(ciK2);
            var ciK3 = defaultCi.Clone(); SetBody(ciK3, "Class", true, true); ret.Add(ciK3);
            var ciK4 = defaultCi.Clone(); SetBody(ciK4, "Predicate", true, true); ret.Add(ciK4);
            var ciK5 = defaultCi.Clone(); SetBody(ciK5, "Function", true, true); ret.Add(ciK5);
            return ret;
        }

        private void SetBody(FplCompletionItem ci, string propertyType, bool isKeyword, bool isOptional)
        {
            if (ci.IsShort)
            {
                TokenIntrinsic = "intr";
                TokenObject = "obj";
                TokenFunction = "func";
                TokenPredicate = "pred";
                TokenOptional = "opt";
                if (isKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, propertyType, isOptional);
                    ci.Detail = $"keywords '{ci.Label}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label;
                    ci.Kind = CompletionItemKind.Keyword;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, propertyType, isOptional) + " ...";
                    ci.Detail = $"{propertyType.ToLower()} definition (short)";
                    ci.SortText = "z" + ci.SortText;
                    ci.InsertText = GetBody(ci, propertyType, isOptional);
                }
            }
            else
            {
                if (isKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, propertyType, isOptional);
                    ci.Detail = $"keywords '{ci.Label}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label;
                    ci.Kind = CompletionItemKind.Keyword;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, propertyType, isOptional) + " ...";
                    ci.Detail = $"{propertyType.ToLower()} definition";
                    ci.SortText = "z" + ci.SortText;
                    ci.InsertText = GetBody(ci, propertyType, isOptional);
                }
            }
        }

        public string GetLabelKeyword(FplCompletionItem ci, string propertyType, bool isOptional)
        {
            if (isOptional)
            {
                switch (propertyType)
                {
                    case "Class":
                        ci.SortText = "property04";
                        return $"{ci.Word} {TokenOptional} {TokenClass}'";
                    case "Function":
                        ci.SortText = "property06";
                        return $"{ci.Word} {TokenFunction}'";
                    case "Predicate":
                    default:
                        ci.SortText = "property05";
                        return $"{ci.Word} {TokenPredicate}'";
                }
            }
            else
            {
                switch (propertyType)
                {
                    case "Class":
                        ci.SortText = "property01";
                        return $"{ci.Word} {TokenClass}'";
                    case "Function":
                        ci.SortText = "property03";
                        return $"{ci.Word} {TokenFunction}'";
                    case "Predicate":
                    default:
                        ci.SortText = "property02";
                        return $"{ci.Word} {TokenPredicate}'";
                }
            }
        }

        public string GetBody(FplCompletionItem ci, string definitionType, bool isOptional)
        {
            string ret;
            switch (definitionType)
            {
                case "Class":
                    ret = $"{GetLabelKeyword(ci, definitionType, isOptional)} SomeFpl{definitionType}Property: {TokenObject}{Environment.NewLine}"; 
                    break;
                case "Function":
                    ret = $"{GetLabelKeyword(ci, definitionType, isOptional)} SomeFpl{definitionType}Property -> {TokenObject}";
                    break;
                case "Predicate":
                default:
                    ret = $"{ GetLabelKeyword(ci, definitionType, isOptional)} SomeFpl{definitionType}Property()";
                    break;

            }
            return ret +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenIntrinsic}{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

    }
}