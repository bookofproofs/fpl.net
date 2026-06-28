namespace FplLS
{
    public class FplCompletionItemChoicesIsOperator : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippet
            var ci = defaultCi
                .WithInsertText($"is (x, SomeFplType)")
                .WithLabel(TokenPrefix + defaultCi.InsertText + " ...");
            ret.Add(ci);

            // keywords
            var keyword = defaultCi.WithKeyword();
            ret.Add(keyword);
            return ret;
        }

    }
}
