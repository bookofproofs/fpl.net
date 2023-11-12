using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesQuantor : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var postfix = "";
            if (defaultCi.Word.Contains("exn"))
            {
                postfix = "!1";
            }
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci, "none", postfix); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetBody(ci1, "inType", postfix); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetBody(ci1, "inList", postfix); ret.Add(ci2);
            var ci3 = defaultCi.Clone(); SetBody(ci1, "inRange", postfix); ret.Add(ci3);
            if (postfix == "")
            {
                var ci4 = defaultCi.Clone(); SetBody(ci4, "inRange", postfix); ret.Add(ci4);
            }
            // keyword
            defaultCi.Kind = CompletionItemKind.Keyword;
            ret.Add(defaultCi);
            return ret;
        }


        private void SetBody(FplCompletionItem ci, string type, string postfix)
        {
            switch (type)
            {
                case "none":
                    ci.SortText = $"{ci.Word}01";
                    ci.Label = $"{TokenPrefix}{ci.Word}{postfix} (..)";
                    ci.InsertText = $"{ci.Word} x" + GetBody();
                    switch (ci.Word)
                    {
                        case "all":
                            ci.Detail = $"all quantor";
                            break;
                        case "ex":
                            ci.Detail = $"exists quantor";
                            break;
                        case "exn":
                            ci.Detail = $"exists n-times quantor";
                            break;
                    }
                    break;
                case "inType":
                    ci.SortText = $"{ci.Word}02";
                    ci.Label = $"{TokenPrefix}{ci.Word}{postfix} in FplType (..)";
                    ci.InsertText = $"{ci.Word} x in FplType" + GetBody();
                    switch (ci.Word)
                    {
                        case "all":
                            ci.Detail = $"all quantor (in type)";
                            break;
                        case "ex":
                            ci.Detail = $"exists quantor (in type)";
                            break;
                        case "exn":
                            ci.Detail = $"exists n-times quantor (in type)";
                            break;
                    }
                    break;
                case "inList":
                    ci.SortText = $"{ci.Word}03";
                    ci.Label = $"{TokenPrefix}{ci.Word}{postfix} in listVar (..)";
                    ci.InsertText = $"{ci.Word} x in listVar" + GetBody();
                    switch (ci.Word)
                    {
                        case "all":
                            ci.Detail = $"all quantor (in list)";
                            break;
                        case "ex":
                            ci.Detail = $"exists quantor (in list)";
                            break;
                        case "exn":
                            ci.Detail = $"exists n-times quantor (in list)";
                            break;
                    }
                    break;
                case "inRange":
                    ci.SortText = $"{ci.Word}04";
                    ci.Label = $"{TokenPrefix}{ci.Word}{postfix} in [a,b] (..)";
                    ci.InsertText = $"{ci.Word} x in [a,b]" + GetBody();
                    switch (ci.Word)
                    {
                        case "all":
                            ci.Detail = $"all quantor (range)";
                            break;
                        case "ex":
                            ci.Detail = $"exists quantor (range)";
                            break;
                        case "exn":
                            ci.Detail = $"exists n-times quantor (range)";
                            break;
                    }
                    break;
                case "combined":
                default:
                    ci.SortText = $"{ci.Word}05";
                    ci.Label = $"{TokenPrefix}{ci.Word}{postfix} <combined> (..)";
                    ci.InsertText = $"{ci.Word} x in [a,b], y in listVar, z in FplType" + GetLongBody();
                    switch (ci.Word)
                    {
                        case "all":
                            ci.Detail = $"all quantor (combined)";
                            break;
                        case "ex":
                            ci.Detail = $"exists quantor (combined)";
                            break;
                        case "exn":
                            ci.Detail = $"exists n-times quantor (combined)";
                            break;
                    }
                    break;

            }

            
        }
        private string GetBody()
        {
            return
                  $"{Environment.NewLine}" +
                  $"({Environment.NewLine}" +
                  $"\tp(x){Environment.NewLine}" +
                  $"){Environment.NewLine}";

        }

        private string GetLongBody()
        {
            return
                  $"{Environment.NewLine}" +
                  $"({Environment.NewLine}" +
                  $"\tand( p(x), q(y), u(z) ){Environment.NewLine}" +
                  $"){Environment.NewLine}";

        }
    }
}
