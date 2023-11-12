using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;

namespace FplLS
{
    public abstract class FplCompletionItemChoices
    {
        public string TokenPrefix = "_ ";
        public string TokenLeftBrace = "{";
        public string TokenRightBrace = "}";
        public string TokenAssume { get; set; }
        public string TokenClass { get; set; }
        public string TokenConclusion { get; set; }
        public string TokenDeclaration { get; set; }
        public string TokenFunction { get; set; }
        public string TokenIntrinsic { get; set; }
        public string TokenObject { get; set; }
        public string TokenOptional { get; set; }
        public string TokenPredicate { get; set; }
        public string TokenPremise { get; set; }
        public FplCompletionItemChoices()
        {
            TokenAssume = "assume";
            TokenClass = "class";
            TokenConclusion = "conclusion";
            TokenDeclaration = "declaration";
            TokenFunction = "function";
            TokenIntrinsic = "intrinsic";
            TokenObject = "object";
            TokenOptional = "optional";
            TokenPredicate = "predicate";
            TokenPremise = "premise";
        }

        public abstract List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi);



    }
}
