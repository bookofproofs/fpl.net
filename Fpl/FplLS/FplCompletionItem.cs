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

            switch (Word)
            {
                case "ISO 639 language code":
                    return new FplCompletionItemChoicesIso639().GetChoices(this);
                case "whitespace":
                case "significant whitespace":
                    return new FplCompletionItemChoicesWhitespace().GetChoices(this);
                case "dollarDigits":
                    return new FplCompletionItemChoicesDigits().GetChoices(this);
                case "argument identifier":
                    return new FplCompletionItemChoicesArgumentIdentifier().GetChoices(this);
                case "language-specific string":
                    return new FplCompletionItemChoicesString().GetChoices(this);
                case "extensionString":
                case "extension regex":
                    return new FplCompletionItemChoicesRegex().GetChoices(this);
                case "word":
                    return new FplCompletionItemChoicesWord().GetChoices(this);
                case PrimVariableL:
                case "variable (got keyword)":
                case "variable (got template)":
                    return new FplCompletionItemChoicesVariable().GetChoices(this);
                case PrimPascalCaseId:
                    return new FplCompletionItemChoicesPascalCaseId().GetChoices(this);
                case LiteralDel:
                case LiteralDelL:
                    return new FplCompletionItemChoicesDelegate().GetChoices(this);
                case LiteralIs:
                    return new FplCompletionItemChoicesIsOperator().GetChoices(this);
                case LiteralAlias:
                case LiteralAssL:
                case LiteralAss:
                case LiteralAssert:
                case LiteralByDef:
                case LiteralCon:
                case LiteralConL:
                case LiteralExt:
                case LiteralExtL:
                case LiteralFunc:
                case LiteralFuncL:
                case LiteralInd:
                case LiteralIndL:
                case LiteralIntr:
                case LiteralIntrL:
                case LiteralIn:
                case LiteralObj:
                case LiteralObjL:
                case LiteralPred:
                case LiteralPredL:
                case LiteralPre:
                case LiteralPreL:
                case LiteralQed:
                case LiteralRet:
                case LiteralRetL:
                case LiteralRev:
                case LiteralRevL:
                case LiteralTrivial:
                    return new FplCompletionItemChoicesKeyword().GetChoices(this);
                case LiteralSelf:
                case LiteralBase:
                case LiteralParent:
                    return new FplCompletionItemChoicesSelf().GetChoices(this);
                case LiteralAll:
                case LiteralEx:
                case LiteralExN:
                    return new FplCompletionItemChoicesQuantor().GetChoices(this);
                case LiteralTrue:
                case LiteralFalse:
                case LiteralUndef:
                case LiteralUndefL:
                case LiteralNot:
                case LiteralXor:
                case LiteralIif:
                case LiteralImpl:
                case LiteralAnd:
                case LiteralOr:
                case "(":
                    return new FplCompletionItemChoicesPredicate().GetChoices(this);
                case LiteralCtor:
                case LiteralCtorL:
                    return new FplCompletionItemChoicesConstructor().GetChoices(this);
                case LiteralDec:
                case LiteralDecL:
                    return new FplCompletionItemChoicesDeclaration().GetChoices(this);
                case LiteralCases:
                    return new FplCompletionItemChoicesCases().GetChoices(this);
                case LiteralFor:
                    return new FplCompletionItemChoicesFor().GetChoices(this);
                case LiteralPrty:
                case LiteralPrtyL:
                    return new FplCompletionItemChoicesProperty().GetChoices(this);
                case LiteralAx:
                case LiteralAxL:
                case LiteralPost:
                case LiteralPostL:
                    return new FplCompletionItemChoicesAxiom().GetChoices(this);
                case LiteralDef:
                case LiteralDefL:
                    return new FplCompletionItemChoicesDefinition().GetChoices(this);
                case LiteralThm:
                case LiteralThmL:
                    return new FplCompletionItemChoicesTheoremLikeStmt("Theorem").GetChoices(this);
                case LiteralLem:
                case LiteralLemL:
                    return new FplCompletionItemChoicesTheoremLikeStmt("Lemma").GetChoices(this);
                case LiteralProp:
                case LiteralPropL:
                    return new FplCompletionItemChoicesTheoremLikeStmt("Proposition").GetChoices(this);
                case LiteralConj:
                case LiteralConjL:
                    return new FplCompletionItemChoicesTheoremLikeStmt("Conjecture").GetChoices(this);
                case LiteralInf:
                case LiteralInfL:
                    return new FplCompletionItemChoicesRuleOfInference().GetChoices(this);
                case LiteralCor:
                case LiteralCorL:
                    return new FplCompletionItemChoicesCorollary().GetChoices(this);
                case LiteralPrf:
                case LiteralPrfL:
                    return new FplCompletionItemChoicesProof().GetChoices(this);
                case LiteralLoc:
                case LiteralLocL:
                    return new FplCompletionItemChoicesLocalization().GetChoices(this);
                case LiteralUses:
                    return new FplCompletionItemChoicesUses().GetChoices(this);
                case LiteralPrefix:
                case LiteralPostFix:
                case LiteralSymbol:
                case LiteralInfix:
                    return new FplCompletionItemChoicesSymbol(Word, true).GetChoices(this);
                case "prefix symbol":
                case "postfix symbol":
                case "object symbol":
                case "infix symbol":
                    return new FplCompletionItemChoicesSymbol(Word, false).GetChoices(this);
                default:
                    return new FplCompletionItemChoicesDefault().GetChoices(this);
            }

        }

        public FplCompletionItem Clone()
        {
            var ret = new FplCompletionItem(this.Word);
            ret.AdditionalTextEdits = this.AdditionalTextEdits;
            ret.Command = this.Command;
            ret.CommitCharacters = this.CommitCharacters;
            ret.Detail = this.Detail;
            ret.Documentation = this.Documentation;
            ret.FilterText = this.FilterText;
            ret.InsertText = this.InsertText;
            ret.Kind = this.Kind;
            ret.Label = this.Label;
            ret.Preselect = this.Preselect;
            ret.SortText = this.SortText;
            ret.TextEdit = this.TextEdit;
            return ret;
        }

        public static string StripQuotesOrBrackets(string str)
        {
            if (str.StartsWith("'") && str.EndsWith("'") || str.StartsWith("<") && str.EndsWith(">"))
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
                    this.Detail = "object class (short form)";
                    this.SortText = "class02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case LiteralObjL:
                    this.Detail = LiteralObjL;
                    this.SortText = "class01";
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
