using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using static FplPrimitives;

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
            TokenAssume = LiteralAssume;
            TokenClass = LiteralClL;
            TokenConclusion = LiteralConL;
            TokenDeclaration = LiteralDecL;
            TokenFunction = LiteralFuncL;
            TokenIntrinsic = LiteralIntrL;
            TokenObject = LiteralObjL;
            TokenOptional = LiteralOptL;
            TokenPredicate = LiteralPredL;
            TokenPremise = LiteralPreL;
            TokenRevoke = LiteralRevL;
        }

        public void AdjustToShort()
        {
            TokenAssume = LiteralAss;
            TokenClass = LiteralCl;
            TokenConclusion = LiteralCon;
            TokenDeclaration = LiteralDec;
            TokenFunction = LiteralFunc;
            TokenIntrinsic = LiteralIntr;
            TokenObject = LiteralObj;
            TokenOptional = LiteralOpt;
            TokenPredicate = LiteralPred;
            TokenPremise = LiteralPre;
            TokenRevoke = LiteralRev;
        }

        public abstract List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi);



    }
}
