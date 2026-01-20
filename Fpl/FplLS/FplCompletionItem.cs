using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLS
{
    public class FplCompletionItem : CompletionItem
    {
        private readonly string _prefix = "_ ";

        public bool IsShort { get; set; }
        public string Word { get; set; }

        public FplCompletionItem(string word)
        {
            Word = word;
            SetDetails();
        }

        public FplCompletionItem(string word, string insertText = "")
        {
            Word = word;
            SetDetails();
            if (insertText != "")
            {
                this.InsertText = insertText.Replace("<replace>", word);
            }
            else
            {
                this.Kind = CompletionItemKind.Keyword;
                this.Detail = $"keyword '{word}'";
                this.SortText = "zzz" + this.SortText; // display keywords later as non-keywords
            }
        }

        public void AdjustToShort()
        {
            if (IsShort)
            {
                this.SortText = "z" + this.SortText;
            }
        }

        public void AdjustToKeyword()
        {
            AdjustToShort();
            if (Kind == CompletionItemKind.Keyword)
            {
                this.SortText = "zzz" + this.SortText;
                this.InsertText = this.Label.Substring(2);
                if (this.InsertText.Split(' ').Length > 1)
                {
                    this.Detail = $"keywords '{this.InsertText}'";
                }
                else
                {
                    this.Detail = $"keyword '{this.InsertText}'";
                }
            }
        }

        public List<FplCompletionItem> GetChoices()
        {

            return Word switch
            {
                "ISO 639 language code" => new FplCompletionItemChoicesIso639().GetChoices(this),
                "whitespace" or "significant whitespace" => new FplCompletionItemChoicesWhitespace().GetChoices(this),
                "dollarDigits" => new FplCompletionItemChoicesDigits().GetChoices(this),
                "argument identifier" => new FplCompletionItemChoicesArgumentIdentifier().GetChoices(this),
                "language-specific string" => new FplCompletionItemChoicesString().GetChoices(this),
                "extensionString" or "extension regex" => new FplCompletionItemChoicesRegex().GetChoices(this),
                "word" => new FplCompletionItemChoicesWord().GetChoices(this),
                PrimVariableL or "variable (got keyword)" or "variable (got template)" => new FplCompletionItemChoicesVariable().GetChoices(this),
                PrimPascalCaseId => new FplCompletionItemChoicesPascalCaseId().GetChoices(this),
                LiteralDel or LiteralDelL => new FplCompletionItemChoicesDelegate().GetChoices(this),
                LiteralIs => new FplCompletionItemChoicesIsOperator().GetChoices(this),
                LiteralAlias or LiteralAssL or LiteralAss or LiteralAssert or LiteralByDef or LiteralCl or LiteralClL or LiteralCon or LiteralConL or LiteralExt or LiteralExtL or LiteralFunc or LiteralFuncL or LiteralInd or LiteralIndL or LiteralIntr or LiteralIntrL or LiteralIn or LiteralObj or LiteralObjL or LiteralPred or LiteralPredL or LiteralPre or LiteralPreL or LiteralQed or LiteralRet or LiteralRetL or LiteralRev or LiteralRevL or LiteralTrivial => new FplCompletionItemChoicesKeyword().GetChoices(this),
                LiteralSelf or LiteralBase or LiteralParent => new FplCompletionItemChoicesSelf().GetChoices(this),
                LiteralAll or LiteralEx or LiteralExN => new FplCompletionItemChoicesQuantor().GetChoices(this),
                LiteralTrue or LiteralFalse or LiteralUndef or LiteralUndefL or LiteralNot or LiteralXor or LiteralIif or LiteralImpl or LiteralAnd or LiteralOr or "(" => new FplCompletionItemChoicesPredicate().GetChoices(this),
                LiteralCtor or LiteralCtorL => new FplCompletionItemChoicesConstructor().GetChoices(this),
                LiteralDec or LiteralDecL => new FplCompletionItemChoicesDeclaration().GetChoices(this),
                LiteralCases => new FplCompletionItemChoicesCases().GetChoices(this),
                LiteralFor => new FplCompletionItemChoicesFor().GetChoices(this),
                LiteralPrty or LiteralPrtyL => new FplCompletionItemChoicesProperty().GetChoices(this),
                LiteralAx or LiteralAxL or LiteralPost or LiteralPostL => new FplCompletionItemChoicesAxiom().GetChoices(this),
                LiteralDef or LiteralDefL => new FplCompletionItemChoicesDefinition().GetChoices(this),
                LiteralThm or LiteralThmL => new FplCompletionItemChoicesTheoremLikeStmt("Theorem").GetChoices(this),
                LiteralLem or LiteralLemL => new FplCompletionItemChoicesTheoremLikeStmt("Lemma").GetChoices(this),
                LiteralProp or LiteralPropL => new FplCompletionItemChoicesTheoremLikeStmt("Proposition").GetChoices(this),
                LiteralConj or LiteralConjL => new FplCompletionItemChoicesTheoremLikeStmt("Conjecture").GetChoices(this),
                LiteralInf or LiteralInfL => new FplCompletionItemChoicesRuleOfInference().GetChoices(this),
                LiteralCor or LiteralCorL => new FplCompletionItemChoicesCorollary().GetChoices(this),
                LiteralPrf or LiteralPrfL => new FplCompletionItemChoicesProof().GetChoices(this),
                LiteralLoc or LiteralLocL => new FplCompletionItemChoicesLocalization().GetChoices(this),
                LiteralUses => new FplCompletionItemChoicesUses().GetChoices(this),
                LiteralPrefix or LiteralPostFix or LiteralSymbol or LiteralInfix => new FplCompletionItemChoicesSymbol(Word, true).GetChoices(this),
                "prefix symbol" or "postfix symbol" or "object symbol" or "infix symbol" => new FplCompletionItemChoicesSymbol(Word, false).GetChoices(this),
                _ => new FplCompletionItemChoicesDefault().GetChoices(this),
            };
        }

        public FplCompletionItem Clone()
        {
            var ret = new FplCompletionItem(this.Word)
            {
                AdditionalTextEdits = this.AdditionalTextEdits,
                Command = this.Command,
                CommitCharacters = this.CommitCharacters,
                Detail = this.Detail,
                Documentation = this.Documentation,
                FilterText = this.FilterText,
                InsertText = this.InsertText,
                Kind = this.Kind,
                Label = this.Label,
                Preselect = this.Preselect,
                SortText = this.SortText,
                TextEdit = this.TextEdit
            };
            return ret;
        }

        public static string StripQuotesOrBrackets(string str)
        {
            if (str.StartsWith('\'') && str.EndsWith('\'') || str.StartsWith('<') && str.EndsWith('>'))
            {
                // strip quotes or brackets from label
                return str.Substring(1, str.Length - 2);
            }
            else
            {
                return str;
            }
        }

        private void SetDetails()
        {
            Word = StripQuotesOrBrackets(Word);
            this.InsertText = Word + " ";
            this.Label = _prefix + Word;
            switch (Word)
            {
                case LiteralAlias:
                    this.Detail = LiteralAlias;
                    this.SortText = LiteralAlias;
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case LiteralAll:
                    this.Detail = "predicate (all quantor)";
                    this.SortText = LiteralAll;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralAnd:
                    this.Detail = "predicate (conjunction)";
                    this.SortText = LiteralAnd;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralAss:
                    this.Detail = "argument (assume, short form)";
                    this.SortText = "assume02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case LiteralAssL:
                    this.Detail = "argument (assume)";
                    this.SortText = "assume01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralAssert:
                    this.Detail = "statement (assert)";
                    this.SortText = LiteralAssert;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralAx:
                    this.Detail = "axiom (short form)";
                    this.SortText = "axiom02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralAxL:
                    this.Detail = LiteralAxL;
                    this.SortText = "axiom01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralCases:
                    this.Detail = "statement (cases)";
                    this.SortText = LiteralCases;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralCl:
                    this.Detail = "class (short form)";
                    this.SortText = "class02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case LiteralClL:
                    this.Detail = LiteralClL;
                    this.SortText = "class01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case LiteralCon:
                    this.Detail = "conclusion (short form)";
                    this.SortText = "conclusion02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case LiteralConL:
                    this.Detail = LiteralConL;
                    this.SortText = "conclusion01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case LiteralCor:
                    this.Detail = "corollary (short form)";
                    this.SortText = "corollary02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralCorL:
                    this.Detail = LiteralCorL;
                    this.SortText = "corollary01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralConj:
                    this.Detail = "conjecture (short form)";
                    this.SortText = "conjecture02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralConjL:
                    this.Detail = LiteralConjL;
                    this.SortText = "conjecture01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralCtor:
                    this.Detail = "constructor (short form)";
                    this.SortText = "constructor02";
                    this.Kind = CompletionItemKind.Constructor;
                    this.IsShort = true;
                    break;
                case LiteralCtorL:
                    this.Detail = LiteralCtorL;
                    this.SortText = "constructor01";
                    this.Kind = CompletionItemKind.Constructor;
                    this.IsShort = false;
                    break;
                case LiteralDec:
                    this.Detail = "declaration (short form)";
                    this.SortText = "declaration02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case LiteralDecL:
                    this.Detail = LiteralDecL;
                    this.SortText = "declaration01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralDel:
                    this.Detail = "delegate (short form)";
                    this.SortText = "delegate02";
                    this.Kind = CompletionItemKind.Event;
                    this.IsShort = true;
                    break;
                case LiteralDelL:
                    this.Detail = LiteralDelL;
                    this.SortText = "delegate01";
                    this.Kind = CompletionItemKind.Event;
                    this.IsShort = false;
                    break;
                case LiteralDef:
                    this.Detail = "definition (short form)";
                    this.SortText = "definition02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralDefL:
                    this.Detail = LiteralDefL;
                    this.SortText = "definition01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralEx:
                    this.Detail = "predicate (exists quantor)";
                    this.SortText = LiteralEx;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralExN:
                    this.Detail = "predicate (exists n-times quantor)";
                    this.SortText = LiteralExN;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "exn!":
                    this.Detail = "predicate (exists n-times quantor)";
                    this.SortText = "exn!";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralExt:
                    this.Detail = "extension (short form)";
                    this.SortText = "extension02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralExtL:
                    this.Detail = LiteralExtL;
                    this.SortText = "extension01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralFalse:
                    this.Detail = "predicate (false)";
                    this.SortText = LiteralFalse;
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case LiteralFor:
                    this.Detail = "statement (for loop)";
                    this.SortText = LiteralFor;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralFunc:
                    this.Detail = "type (functional term, short form)";
                    this.SortText = "function02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case LiteralFuncL:
                    this.Detail = "type (functional term)";
                    this.SortText = "function01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case LiteralIif:
                    this.Detail = "predicate (equivalence, <=>)";
                    this.SortText = LiteralIif;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralImpl:
                    this.Detail = "predicate (implication, =>)";
                    this.SortText = LiteralImpl;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralIn:
                    this.Detail = "clause (in type or in range)";
                    this.SortText = LiteralIn;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralInd:
                    this.Detail = "type (index, short form)";
                    this.SortText = "index02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case LiteralIndL:
                    this.Detail = "type (index)";
                    this.SortText = "index01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case LiteralInf:
                    this.Detail = "rule of inference (short form)";
                    this.SortText = "inference02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralInfL:
                    this.Detail = PrimRuleOfInference;
                    this.SortText = "inference01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralInfix:
                    this.Detail = "infix operator";
                    this.SortText = LiteralInfix;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralIntr:
                    this.Detail = "intrinsic (short form)";
                    this.SortText = "intrinsic02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case LiteralIntrL:
                    this.Detail = LiteralIntrL;
                    this.SortText = "intrinsic01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case LiteralIs:
                    this.Detail = "predicate (is of type)";
                    this.SortText = LiteralIs;
                    this.Kind = CompletionItemKind.Interface;
                    this.IsShort = false;
                    break;
                case LiteralLem:
                    this.Detail = "lemma (short form)";
                    this.SortText = "lemma02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralLemL:
                    this.Detail = LiteralLemL;
                    this.SortText = "lemma01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralLoc:
                    this.Detail = "localization (short form)";
                    this.SortText = "localization02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralLocL:
                    this.Detail = LiteralLocL;
                    this.SortText = "localization01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralNot:
                    this.Detail = "predicate (negation)";
                    this.SortText = LiteralNot;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralObj:
                    this.Detail = "type (object, short form)";
                    this.SortText = "object02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case LiteralObjL:
                    this.Detail = "type (object)";
                    this.SortText = "object01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case LiteralOr:
                    this.Detail = "predicate (disjunction)";
                    this.SortText = LiteralOr;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralParent:
                    this.Detail = "reference (to parent)";
                    this.SortText = LiteralParent;
                    this.Kind = CompletionItemKind.Reference;
                    this.IsShort = false;
                    break;
                case LiteralPost:
                    this.Detail = "postulate (short form)";
                    this.SortText = "postulate02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralPostL:
                    this.Detail = LiteralPostL;
                    this.SortText = "postulate01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralPre:
                    this.Detail = "premise (short form)";
                    this.SortText = "premise02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case LiteralPred:
                    this.Detail = "type (predicate, short form)";
                    this.SortText = "predicate02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case LiteralPredL:
                    this.Detail = "type (predicate)";
                    this.SortText = "predicate01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case LiteralPrefix:
                    this.Detail = "prefix operator";
                    this.SortText = LiteralPrefix;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralPostFix:
                    this.Detail = "postfix operator";
                    this.SortText = LiteralPostFix;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case LiteralPreL:
                    this.Detail = LiteralPreL;
                    this.SortText = "premise01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case LiteralProp:
                    this.Detail = "proposition (short form)";
                    this.SortText = "proposition02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralPropL:
                    this.Detail = LiteralPropL;
                    this.SortText = "proposition01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralPrty:
                    this.Detail = "property (short form)";
                    this.SortText = "property02";
                    this.Kind = CompletionItemKind.Value;
                    this.IsShort = true;
                    break;
                case LiteralPrtyL:
                    this.Detail = LiteralPrtyL;
                    this.SortText = "property01";
                    this.Kind = CompletionItemKind.Value;
                    this.IsShort = false;
                    break;
                case LiteralPrf:
                    this.Detail = "proof (short form)";
                    this.SortText = "proof02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralPrfL:
                    this.Detail = LiteralPrfL;
                    this.SortText = "proof01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralQed:
                    this.Detail = "(quod erat demonstrandum)";
                    this.SortText = LiteralQed;
                    this.Kind = CompletionItemKind.Text;
                    this.IsShort = false;
                    break;
                case LiteralRet:
                    this.Detail = "statement (return, short form)";
                    this.SortText = "return02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case LiteralRetL:
                    this.Detail = "statement (return)";
                    this.SortText = "return01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralRev:
                    this.Detail = "argument (revoke, short form)";
                    this.SortText = "revoke02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case LiteralRevL:
                    this.Detail = "argument (revoke)";
                    this.SortText = "revoke01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case LiteralSelf:
                    this.Detail = "reference (to self)";
                    this.SortText = LiteralSelf;
                    this.Kind = CompletionItemKind.Reference;
                    this.IsShort = false;
                    break;
                case LiteralSymbol:
                    this.Detail = "object symbol";
                    this.SortText = LiteralSymbol;
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case LiteralThm:
                    this.Detail = "theorem (short form)";
                    this.SortText = "theorem02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case LiteralThmL:
                    this.Detail = LiteralThmL;
                    this.SortText = "theorem01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case LiteralTrue:
                    this.Detail = "predicate (true)";
                    this.SortText = LiteralTrue;
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case LiteralTrivial:
                    this.Detail = "argument (trivial)";
                    this.SortText = LiteralTrivial;
                    this.Kind = CompletionItemKind.Text;
                    this.IsShort = false;
                    break;
                case LiteralUndef:
                    this.Detail = "undefined (short form)";
                    this.SortText = "undefined02";
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = true;
                    break;
                case LiteralUndefL:
                    this.Detail = LiteralUndefL;
                    this.SortText = "undefined01";
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case LiteralUses:
                    this.Detail = "clause (uses)";
                    this.SortText = LiteralUses;
                    this.Kind = CompletionItemKind.Module;
                    this.IsShort = false;
                    break;
                case LiteralXor:
                    this.Detail = "predicate (exclusive or)";
                    this.SortText = LiteralXor;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                default:
                    this.Detail = Word;
                    this.SortText = Word;
                    this.Kind = CompletionItemKind.Text;
                    this.IsShort = false;
                    break;
            }

        }
    }
}
