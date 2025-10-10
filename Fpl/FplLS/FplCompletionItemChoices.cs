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
        public string TokenPredicate { get; set; }
        public string TokenPremise { get; set; }
        public string TokenRevoke { get; set; }
        public FplCompletionItemChoices()
        {
            TokenAssume = LiteralAssL;
            TokenClass = LiteralObj;
            TokenConclusion = LiteralConL;
            TokenDeclaration = LiteralDecL;
            TokenFunction = LiteralFuncL;
            TokenIntrinsic = LiteralIntrL;
            TokenObject = LiteralObjL;
            TokenPredicate = LiteralPredL;
            TokenPremise = LiteralPreL;
            TokenRevoke = LiteralRevL;
        }

        public void AdjustToShort()
        {
            TokenAssume = LiteralAss;
            TokenClass = LiteralObj;
            TokenConclusion = LiteralCon;
            TokenDeclaration = LiteralDec;
            TokenFunction = LiteralFunc;
            TokenIntrinsic = LiteralIntr;
            TokenObject = LiteralObj;
            TokenPredicate = LiteralPred;
            TokenPremise = LiteralPre;
            TokenRevoke = LiteralRev;
        }

        public abstract List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi);



    }
}
