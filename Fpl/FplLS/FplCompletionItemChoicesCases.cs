namespace FplLS
{
    public class FplCompletionItemChoicesCases : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var insert =
                $"cases{Environment.NewLine}" +
                $"({Environment.NewLine}" +
                $"\t| p(x): y := a{Environment.NewLine}" +
                $"\t| q(x): y := b{Environment.NewLine}" +
                $"\t? y := c{Environment.NewLine}" +
                $"){Environment.NewLine}";
            var ci = defaultCi
                .WithInsertText(insert)
                .WithLabel(defaultCi.Label + " ...");
            ret.Add(ci);
            // keywords
            var keyword = defaultCi.WithKeyword();
            ret.Add(keyword);
            return ret;

        }
    }
}
