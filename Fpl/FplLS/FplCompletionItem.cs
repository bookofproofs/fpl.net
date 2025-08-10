using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

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
                case "variable":
                case "variable (got keyword)":
                case "variable (got template)":
                    return new FplCompletionItemChoicesVariable().GetChoices(this);
                case "PascalCaseId":
                    return new FplCompletionItemChoicesPascalCaseId().GetChoices(this);
                case literalDel:
                case literalDelL:
                    return new FplCompletionItemChoicesDelegate().GetChoices(this);
                case literalIs:
                    return new FplCompletionItemChoicesIsOperator().GetChoices(this);
                case literalAlias:
                case literalAssL:
                case literalAss:
                case literalAssume:
                case literalByDef:
                case literalCl:
                case literalClL:
                case literalCon:
                case literalConL:
                case literalExt:
                case literalExtL:
                case literalFunc:
                case literalFuncL:
                case literalInd:
                case literalIndL:
                case literalIntr:
                case literalIntrL:
                case literalIn:
                case "obj":
                case "object":
                case "opt":
                case "optional":
                case "pred":
                case "predicate":
                case "pre":
                case "premise":
                case "qed":
                case "ret":
                case "return":
                case "rev":
                case "revoke":
                case "trivial":
                    return new FplCompletionItemChoicesKeyword().GetChoices(this);
                case "self":
                case literalBase:
                case "parent":
                    return new FplCompletionItemChoicesSelf().GetChoices(this);
                case literalAll:
                case literalEx:
                case literalExN:
                    return new FplCompletionItemChoicesQuantor().GetChoices(this);
                case "true":
                case literalFalse:
                case "undef":
                case "undefined":
                case "not":
                case "xor":
                case literalIif:
                case literalImpl:
                case literalAnd:
                case "or":
                case "(":
                    return new FplCompletionItemChoicesPredicate().GetChoices(this);
                case literalCtor:
                case literalCtorL:
                    return new FplCompletionItemChoicesConstructor().GetChoices(this);
                case literalDec:
                case literalDecL:
                    return new FplCompletionItemChoicesDeclaration().GetChoices(this);
                case literalCases:
                    return new FplCompletionItemChoicesCases().GetChoices(this);
                case literalFor:
                    return new FplCompletionItemChoicesFor().GetChoices(this);
                case "prty":
                case "property":
                    return new FplCompletionItemChoicesProperty().GetChoices(this);
                case literalAx:
                case literalAxL:
                case "post":
                case "postulate":
                    return new FplCompletionItemChoicesAxiom().GetChoices(this);
                case literalDef:
                case literalDefL:
                    return new FplCompletionItemChoicesDefinition().GetChoices(this);
                case "thm":
                case "theorem":
                    return new FplCompletionItemChoicesTheoremLikeStmt("Theorem").GetChoices(this);
                case "lem":
                case "lemma":
                    return new FplCompletionItemChoicesTheoremLikeStmt("Lemma").GetChoices(this);
                case "prop":
                case "proposition":
                    return new FplCompletionItemChoicesTheoremLikeStmt("Proposition").GetChoices(this);
                case literalConj:
                case literalConjL:
                    return new FplCompletionItemChoicesTheoremLikeStmt("Conjecture").GetChoices(this);
                case literalInf:
                case literalInfL:
                    return new FplCompletionItemChoicesRuleOfInference().GetChoices(this);
                case literalCor:
                case literalCorL:
                    return new FplCompletionItemChoicesCorollary().GetChoices(this);
                case "prf":
                case "proof":
                    return new FplCompletionItemChoicesProof().GetChoices(this);
                case "loc":
                case "localization":
                    return new FplCompletionItemChoicesLocalization().GetChoices(this);
                case "uses":
                    return new FplCompletionItemChoicesUses().GetChoices(this);
                case "prefix":
                case "postfix":
                case "symbol":
                case literalInfix:
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
                case literalAlias:
                    this.Detail = literalAlias;
                    this.SortText = literalAlias;
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case literalAll:
                    this.Detail = "predicate (all quantor)";
                    this.SortText = literalAll;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalAnd:
                    this.Detail = "predicate (conjunction)";
                    this.SortText = literalAnd;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalAss:
                    this.Detail = "argument (assume, short form)";
                    this.SortText = "assume02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case literalAssL:
                    this.Detail = "statement (assert)";
                    this.SortText = literalAssL;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case literalAssume:
                    this.Detail = "argument (assume)";
                    this.SortText = "assume01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case literalAx:
                    this.Detail = "axiom (short form)";
                    this.SortText = "axiom02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case literalAxL:
                    this.Detail = literalAxL;
                    this.SortText = "axiom01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case literalCases:
                    this.Detail = "statement (cases)";
                    this.SortText = literalCases;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case literalCl:
                    this.Detail = "class (short form)";
                    this.SortText = "class02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case literalClL:
                    this.Detail = literalClL;
                    this.SortText = "class01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case literalCon:
                    this.Detail = "conclusion (short form)";
                    this.SortText = "conclusion02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case literalConL:
                    this.Detail = literalConL;
                    this.SortText = "conclusion01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case literalCor:
                    this.Detail = "corollary (short form)";
                    this.SortText = "corollary02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case literalCorL:
                    this.Detail = literalCorL;
                    this.SortText = "corollary01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case literalConj:
                    this.Detail = "conjecture (short form)";
                    this.SortText = "conjecture02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case literalConjL:
                    this.Detail = literalConjL;
                    this.SortText = "conjecture01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case literalCtor:
                    this.Detail = "constructor (short form)";
                    this.SortText = "constructor02";
                    this.Kind = CompletionItemKind.Constructor;
                    this.IsShort = true;
                    break;
                case literalCtorL:
                    this.Detail = literalCtorL;
                    this.SortText = "constructor01";
                    this.Kind = CompletionItemKind.Constructor;
                    this.IsShort = false;
                    break;
                case literalDec:
                    this.Detail = "declaration (short form)";
                    this.SortText = "declaration02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case literalDecL:
                    this.Detail = literalDecL;
                    this.SortText = "declaration01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case literalDel:
                    this.Detail = "delegate (short form)";
                    this.SortText = "delegate02";
                    this.Kind = CompletionItemKind.Event;
                    this.IsShort = true;
                    break;
                case literalDelL:
                    this.Detail = literalDelL;
                    this.SortText = "delegate01";
                    this.Kind = CompletionItemKind.Event;
                    this.IsShort = false;
                    break;
                case literalDef:
                    this.Detail = "definition (short form)";
                    this.SortText = "definition02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case literalDefL:
                    this.Detail = literalDefL;
                    this.SortText = "definition01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case literalEx:
                    this.Detail = "predicate (exists quantor)";
                    this.SortText = literalEx;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalExN:
                    this.Detail = "predicate (exists n-times quantor)";
                    this.SortText = literalExN;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "exn!":
                    this.Detail = "predicate (exists n-times quantor)";
                    this.SortText = "exn!";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalExt:
                    this.Detail = "extension (short form)";
                    this.SortText = "extension02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case literalExtL:
                    this.Detail = literalExtL;
                    this.SortText = "extension01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case literalFalse:
                    this.Detail = "predicate (false)";
                    this.SortText = literalFalse;
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case literalFor:
                    this.Detail = "statement (for loop)";
                    this.SortText = literalFor;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case literalFunc:
                    this.Detail = "type (functional term, short form)";
                    this.SortText = "function02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case literalFuncL:
                    this.Detail = "type (functional term)";
                    this.SortText = "function01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case literalIif:
                    this.Detail = "predicate (equivalence, <=>)";
                    this.SortText = literalIif;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalImpl:
                    this.Detail = "predicate (implication, =>)";
                    this.SortText = literalImpl;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalIn:
                    this.Detail = "clause (in type or in range)";
                    this.SortText = literalIn;
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case literalInd:
                    this.Detail = "type (index, short form)";
                    this.SortText = "index02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case literalIndL:
                    this.Detail = "type (index)";
                    this.SortText = "index01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case literalInf:
                    this.Detail = "rule of inference (short form)";
                    this.SortText = "inference02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case literalInfL:
                    this.Detail = "rule of inference";
                    this.SortText = "inference01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case literalInfix:
                    this.Detail = "infix operator";
                    this.SortText = literalInfix;
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case literalIntr:
                    this.Detail = "intrinsic (short form)";
                    this.SortText = "intrinsic02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case literalIntrL:
                    this.Detail = literalIntrL;
                    this.SortText = "intrinsic01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case literalIs:
                    this.Detail = "predicate (is of type)";
                    this.SortText = literalIs;
                    this.Kind = CompletionItemKind.Interface;
                    this.IsShort = false;
                    break;
                case "lem":
                    this.Detail = "lemma (short form)";
                    this.SortText = "lemma02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "lemma":
                    this.Detail = "lemma";
                    this.SortText = "lemma01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "loc":
                    this.Detail = "localization (short form)";
                    this.SortText = "localization02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "localization":
                    this.Detail = "localization";
                    this.SortText = "localization01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "not":
                    this.Detail = "predicate (negation)";
                    this.SortText = "not";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "obj":
                    this.Detail = "type (object, short form)";
                    this.SortText = "object02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case "object":
                    this.Detail = "type (object)";
                    this.SortText = "object01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case "opt":
                    this.Detail = "optional (short form)";
                    this.SortText = "optional02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case "optional":
                    this.Detail = "optional";
                    this.SortText = "optional01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "or":
                    this.Detail = "predicate (disjunction)";
                    this.SortText = "or";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "parent":
                    this.Detail = "reference (to parent)";
                    this.SortText = "parent";
                    this.Kind = CompletionItemKind.Reference;
                    this.IsShort = false;
                    break;
                case "post":
                    this.Detail = "postulate (short form)";
                    this.SortText = "postulate02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "postulate":
                    this.Detail = "postulate";
                    this.SortText = "postulate01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "pre":
                    this.Detail = "premise (short form)";
                    this.SortText = "premise02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case "pred":
                    this.Detail = "type (predicate, short form)";
                    this.SortText = "predicate02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case "predicate":
                    this.Detail = "type (predicate)";
                    this.SortText = "predicate01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case "prefix":
                    this.Detail = "prefix operator";
                    this.SortText = "prefix";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "postfix":
                    this.Detail = "postfix operator";
                    this.SortText = "postfix";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "premise":
                    this.Detail = "premise";
                    this.SortText = "premise01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case "prop":
                    this.Detail = "proposition (short form)";
                    this.SortText = "proposition02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "proposition":
                    this.Detail = "proposition";
                    this.SortText = "proposition01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "prty":
                    this.Detail = "property (short form)";
                    this.SortText = "property02";
                    this.Kind = CompletionItemKind.Value;
                    this.IsShort = true;
                    break;
                case "property":
                    this.Detail = "property";
                    this.SortText = "property01";
                    this.Kind = CompletionItemKind.Value;
                    this.IsShort = false;
                    break;
                case "prf":
                    this.Detail = "proof (short form)";
                    this.SortText = "proof02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "proof":
                    this.Detail = "proof";
                    this.SortText = "proof01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "qed":
                    this.Detail = "(quod erat demonstrandum)";
                    this.SortText = "qed";
                    this.Kind = CompletionItemKind.Text;
                    this.IsShort = false;
                    break;
                case "ret":
                    this.Detail = "statement (return, short form)";
                    this.SortText = "return02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case "return":
                    this.Detail = "statement (return)";
                    this.SortText = "return01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "rev":
                    this.Detail = "argument (revoke, short form)";
                    this.SortText = "revoke02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case "revoke":
                    this.Detail = "argument (revoke)";
                    this.SortText = "revoke01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "self":
                    this.Detail = "reference (to self)";
                    this.SortText = "self";
                    this.Kind = CompletionItemKind.Reference;
                    this.IsShort = false;
                    break;
                case "symbol":
                    this.Detail = "object symbol";
                    this.SortText = "symbol";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case "thm":
                    this.Detail = "theorem (short form)";
                    this.SortText = "theorem02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "theorem":
                    this.Detail = "theorem";
                    this.SortText = "theorem01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "true":
                    this.Detail = "predicate (true)";
                    this.SortText = "true";
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case "trivial":
                    this.Detail = "argument (trivial)";
                    this.SortText = "trivial";
                    this.Kind = CompletionItemKind.Text;
                    this.IsShort = false;
                    break;
                case "undef":
                    this.Detail = "undefined (short form)";
                    this.SortText = "undefined02";
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = true;
                    break;
                case "undefined":
                    this.Detail = "undefined";
                    this.SortText = "undefined01";
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case "uses":
                    this.Detail = "clause (uses)";
                    this.SortText = "uses";
                    this.Kind = CompletionItemKind.Module;
                    this.IsShort = false;
                    break;
                case "xor":
                    this.Detail = "predicate (exclusive or)";
                    this.SortText = "xor";
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
