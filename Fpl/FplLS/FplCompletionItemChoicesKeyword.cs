namespace FplLS
{
    public class FplCompletionItemChoicesKeyword: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // keywords
            ret.Add(defaultCi);
            return ret;

        }
    }
}
