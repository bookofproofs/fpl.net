using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using static FplGrammarCommons;

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
        public string TokenRevoke { get; set; }
        public FplCompletionItemChoices()
        {
            TokenAssume = literalAssume;
            TokenClass = literalClL;
            TokenConclusion = literalConL;
            TokenDeclaration = literalDecL;
            TokenFunction = literalFuncL;
            TokenIntrinsic = literalIntrL;
            TokenObject = literalObjL;
            TokenOptional = literalOptL;
            TokenPredicate = literalPredL;
            TokenPremise = literalPreL;
            TokenRevoke = literalRevL;
        }

        public void AdjustToShort()
        {
            TokenAssume = literalAss;
            TokenClass = literalCl;
            TokenConclusion = literalCon;
            TokenDeclaration = literalDec;
            TokenFunction = literalFunc;
            TokenIntrinsic = literalIntr;
            TokenObject = literalObj;
            TokenOptional = literalOpt;
            TokenPredicate = literalPred;
            TokenPremise = literalPre;
            TokenRevoke = literalRev;
        }

        public abstract List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi);



    }
}
