using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public record FplCompletionItem : CompletionItem
    {
        private readonly string _prefix = "_ ";

        public bool IsShort { get; set; }
        public string Word { get; set; }

        public FplCompletionItem(string word, string insertText = "")
        {
            // Normalize word early so all derived values use the same string.
            Word = StripQuotesOrBrackets(word);

            // Default textual representation
            InsertText = Word + " ";
            Label = _prefix + Word;

            // Initialize semantic metadata that used to be set in SetDetails()
            switch (Word)
            {
                case LiteralAlias:
                    Detail = LiteralAlias;
                    SortText = LiteralAlias;
                    Kind = CompletionItemKind.Struct;
                    IsShort = false;
                    break;
                case LiteralAll:
                    Detail = "predicate (all quantor)";
                    SortText = LiteralAll;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralAnd:
                    Detail = "predicate (conjunction)";
                    SortText = LiteralAnd;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralAss:
                    Detail = "argument (assume, short form)";
                    SortText = "assume02";
                    Kind = CompletionItemKind.Property;
                    IsShort = true;
                    break;
                case LiteralAssL:
                    Detail = "argument (assume)";
                    SortText = "assume01";
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralAssert:
                    Detail = "statement (assert)";
                    SortText = LiteralAssert;
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralAx:
                    Detail = "axiom (short form)";
                    SortText = "axiom02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralAxL:
                    Detail = LiteralAxL;
                    SortText = "axiom01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralCases:
                    Detail = "statement (cases)";
                    SortText = LiteralCases;
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralCl:
                    Detail = "class (short form)";
                    SortText = "class02";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = true;
                    break;
                case LiteralClL:
                    Detail = LiteralClL;
                    SortText = "class01";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = false;
                    break;
                case LiteralCon:
                    Detail = "conclusion (short form)";
                    SortText = "conclusion02";
                    Kind = CompletionItemKind.Struct;
                    IsShort = true;
                    break;
                case LiteralConL:
                    Detail = LiteralConL;
                    SortText = "conclusion01";
                    Kind = CompletionItemKind.Struct;
                    IsShort = false;
                    break;
                case LiteralCor:
                    Detail = "corollary (short form)";
                    SortText = "corollary02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralCorL:
                    Detail = LiteralCorL;
                    SortText = "corollary01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralConj:
                    Detail = "conjecture (short form)";
                    SortText = "conjecture02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralConjL:
                    Detail = LiteralConjL;
                    SortText = "conjecture01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralCtor:
                    Detail = "constructor (short form)";
                    SortText = "constructor02";
                    Kind = CompletionItemKind.Constructor;
                    IsShort = true;
                    break;
                case LiteralCtorL:
                    Detail = LiteralCtorL;
                    SortText = "constructor01";
                    Kind = CompletionItemKind.Constructor;
                    IsShort = false;
                    break;
                case LiteralDec:
                    Detail = "declaration (short form)";
                    SortText = "declaration02";
                    Kind = CompletionItemKind.Property;
                    IsShort = true;
                    break;
                case LiteralDecL:
                    Detail = LiteralDecL;
                    SortText = "declaration01";
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralDel:
                    Detail = "delegate (short form)";
                    SortText = "delegate02";
                    Kind = CompletionItemKind.Event;
                    IsShort = true;
                    break;
                case LiteralDelL:
                    Detail = LiteralDelL;
                    SortText = "delegate01";
                    Kind = CompletionItemKind.Event;
                    IsShort = false;
                    break;
                case LiteralDef:
                    Detail = "definition (short form)";
                    SortText = "definition02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralDefL:
                    Detail = LiteralDefL;
                    SortText = "definition01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralEx:
                    Detail = "predicate (exists quantor)";
                    SortText = LiteralEx;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralExN:
                    Detail = "predicate (exists n-times quantor)";
                    SortText = LiteralExN;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case "exn!":
                    Detail = "predicate (exists n-times quantor)";
                    SortText = "exn!";
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralExt:
                    Detail = "extension (short form)";
                    SortText = "extension02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralExtL:
                    Detail = LiteralExtL;
                    SortText = "extension01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralFalse:
                    Detail = "predicate (false)";
                    SortText = LiteralFalse;
                    Kind = CompletionItemKind.Constant;
                    IsShort = false;
                    break;
                case LiteralFor:
                    Detail = "statement (for loop)";
                    SortText = LiteralFor;
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralFunc:
                    Detail = "type (functional term, short form)";
                    SortText = "function02";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = true;
                    break;
                case LiteralFuncL:
                    Detail = "type (functional term)";
                    SortText = "function01";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = false;
                    break;
                case LiteralIif:
                    Detail = "predicate (equivalence, <=>)";
                    SortText = LiteralIif;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralImpl:
                    Detail = "predicate (implication, =>)";
                    SortText = LiteralImpl;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralIn:
                    Detail = "clause (in type or in range)";
                    SortText = LiteralIn;
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralInd:
                    Detail = "type (index, short form)";
                    SortText = "index02";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = true;
                    break;
                case LiteralIndL:
                    Detail = "type (index)";
                    SortText = "index01";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = false;
                    break;
                case LiteralInf:
                    Detail = "rule of inference (short form)";
                    SortText = "inference02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralInfL:
                    Detail = PrimRuleOfInference;
                    SortText = "inference01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralInfix:
                    Detail = "infix operator";
                    SortText = LiteralInfix;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralIntr:
                    Detail = "intrinsic (short form)";
                    SortText = "intrinsic02";
                    Kind = CompletionItemKind.Struct;
                    IsShort = true;
                    break;
                case LiteralIntrL:
                    Detail = LiteralIntrL;
                    SortText = "intrinsic01";
                    Kind = CompletionItemKind.Struct;
                    IsShort = false;
                    break;
                case LiteralIs:
                    Detail = "predicate (is of type)";
                    SortText = LiteralIs;
                    Kind = CompletionItemKind.Interface;
                    IsShort = false;
                    break;
                case LiteralLem:
                    Detail = "lemma (short form)";
                    SortText = "lemma02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralLemL:
                    Detail = LiteralLemL;
                    SortText = "lemma01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralLoc:
                    Detail = "localization (short form)";
                    SortText = "localization02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralLocL:
                    Detail = LiteralLocL;
                    SortText = "localization01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralNot:
                    Detail = "predicate (negation)";
                    SortText = LiteralNot;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralObj:
                    Detail = "type (object, short form)";
                    SortText = "object02";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = true;
                    break;
                case LiteralObjL:
                    Detail = LiteralObjL;
                    SortText = "object01";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = false;
                    break;
                case LiteralOr:
                    Detail = "predicate (disjunction)";
                    SortText = LiteralOr;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralParent:
                    Detail = "reference (to parent)";
                    SortText = LiteralParent;
                    Kind = CompletionItemKind.Reference;
                    IsShort = false;
                    break;
                case LiteralPost:
                    Detail = "postulate (short form)";
                    SortText = "postulate02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralPostL:
                    Detail = LiteralPostL;
                    SortText = "postulate01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralPre:
                    Detail = "premise (short form)";
                    SortText = "premise02";
                    Kind = CompletionItemKind.Struct;
                    IsShort = true;
                    break;
                case LiteralPred:
                    Detail = "type (predicate, short form)";
                    SortText = "predicate02";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = true;
                    break;
                case LiteralPredL:
                    Detail = "type (predicate)";
                    SortText = "predicate01";
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = false;
                    break;
                case LiteralPrefix:
                    Detail = "prefix operator";
                    SortText = LiteralPrefix;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralPostFix:
                    Detail = "postfix operator";
                    SortText = LiteralPostFix;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                case LiteralPreL:
                    Detail = LiteralPreL;
                    SortText = "premise01";
                    Kind = CompletionItemKind.Struct;
                    IsShort = false;
                    break;
                case LiteralProp:
                    Detail = "proposition (short form)";
                    SortText = "proposition02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralPropL:
                    Detail = LiteralPropL;
                    SortText = "proposition01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralPrty:
                    Detail = "property (short form)";
                    SortText = "property02";
                    Kind = CompletionItemKind.Value;
                    IsShort = true;
                    break;
                case LiteralPrtyL:
                    Detail = LiteralPrtyL;
                    SortText = "property01";
                    Kind = CompletionItemKind.Value;
                    IsShort = false;
                    break;
                case LiteralPrf:
                    Detail = "proof (short form)";
                    SortText = "proof02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralPrfL:
                    Detail = LiteralPrfL;
                    SortText = "proof01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralQed:
                    Detail = "(quod erat demonstrandum)";
                    SortText = LiteralQed;
                    Kind = CompletionItemKind.Text;
                    IsShort = false;
                    break;
                case LiteralRet:
                    Detail = "statement (return, short form)";
                    SortText = "return02";
                    Kind = CompletionItemKind.Property;
                    IsShort = true;
                    break;
                case LiteralRetL:
                    Detail = "statement (return)";
                    SortText = "return01";
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralRev:
                    Detail = "argument (revoke, short form)";
                    SortText = "revoke02";
                    Kind = CompletionItemKind.Property;
                    IsShort = true;
                    break;
                case LiteralRevL:
                    Detail = "argument (revoke)";
                    SortText = "revoke01";
                    Kind = CompletionItemKind.Property;
                    IsShort = false;
                    break;
                case LiteralSelf:
                    Detail = "reference (to self)";
                    SortText = LiteralSelf;
                    Kind = CompletionItemKind.Reference;
                    IsShort = false;
                    break;
                case LiteralSymbol:
                    Detail = "object symbol";
                    SortText = LiteralSymbol;
                    Kind = CompletionItemKind.TypeParameter;
                    IsShort = false;
                    break;
                case LiteralThm:
                    Detail = "theorem (short form)";
                    SortText = "theorem02";
                    Kind = CompletionItemKind.Class;
                    IsShort = true;
                    break;
                case LiteralThmL:
                    Detail = LiteralThmL;
                    SortText = "theorem01";
                    Kind = CompletionItemKind.Class;
                    IsShort = false;
                    break;
                case LiteralTrue:
                    Detail = "predicate (true)";
                    SortText = LiteralTrue;
                    Kind = CompletionItemKind.Constant;
                    IsShort = false;
                    break;
                case LiteralTrivial:
                    Detail = "argument (trivial)";
                    SortText = LiteralTrivial;
                    Kind = CompletionItemKind.Text;
                    IsShort = false;
                    break;
                case LiteralUndef:
                    Detail = "undefined (short form)";
                    SortText = "undefined02";
                    Kind = CompletionItemKind.Constant;
                    IsShort = true;
                    break;
                case LiteralUndefL:
                    Detail = LiteralUndefL;
                    SortText = "undefined01";
                    Kind = CompletionItemKind.Constant;
                    IsShort = false;
                    break;
                case LiteralUses:
                    Detail = "clause (uses)";
                    SortText = LiteralUses;
                    Kind = CompletionItemKind.Module;
                    IsShort = false;
                    break;
                case LiteralXor:
                    Detail = "predicate (exclusive or)";
                    SortText = LiteralXor;
                    Kind = CompletionItemKind.Operator;
                    IsShort = false;
                    break;
                default:
                    Detail = Word;
                    SortText = Word;
                    Kind = CompletionItemKind.Text;
                    IsShort = false;
                    break;
            }

            // post-construction override: if caller supplied explicit insertText, use it
            if (!string.IsNullOrEmpty(insertText))
            {
                InsertText = insertText.Replace("<replace>", word);
            }
            else
            {
                // previous behavior: when insertText wasn't supplied the item was treated as a keyword
                // keep the original Kind (do not force Keyword) but mark sort-order for keywords
                Detail = $"keyword '{word}'";
                SortText = "zzz" + SortText;
            }
        }

        // Public immutable-style helpers ------------------------------------------------

        private FplCompletionItem Copy(
            string? label = null,
            string? detail = null,
            string? insertText = null,
            string? sortText = null,
            CompletionItemKind? kind = null,
            bool? isShort = null)
        {
            var finalSort = sortText ?? this.SortText;
            var finalIsShort = isShort ?? this.IsShort;

            var copy = new FplCompletionItem(this.Word, insertText: insertText ?? this.InsertText)
            {
                AdditionalTextEdits = this.AdditionalTextEdits,
                Command = this.Command,
                CommitCharacters = this.CommitCharacters,
                Detail = detail ?? this.Detail,
                Documentation = this.Documentation,
                FilterText = this.FilterText,
                InsertText = insertText ?? this.InsertText,
                Kind = kind ?? this.Kind,
                Label = label ?? this.Label,
                Preselect = this.Preselect,
                SortText = finalSort,
                TextEdit = this.TextEdit,
                IsShort = finalIsShort
            };

            return copy;
        }

        public FplCompletionItem WithSortText(string sortText) => Copy(sortText: sortText);

        public FplCompletionItem WithKind(CompletionItemKind kind) => Copy(kind: kind);

        public FplCompletionItem WithLabel(string label) => Copy(label: label);

        public FplCompletionItem WithInsertText(string insertText) => Copy(insertText: insertText);

        public FplCompletionItem WithDetail(string detail) => Copy(detail: detail);

        public FplCompletionItem WithIsShort(bool isShort)
        {
            var newSort = isShort ? "z" + this.SortText : this.SortText;
            return Copy(sortText: newSort, isShort: isShort);
        }

        public FplCompletionItem WithKeyword()
        {
            var insert = Label.Length >= 2 ? Label[2..] : Label;
            var detail = insert.Split(' ').Length > 1 ? $"keywords '{insert}'" : $"keyword '{insert}'";
            var newSort = "zzz" + SortText;
            return Copy(insertText: insert, detail: detail, sortText: newSort, kind: CompletionItemKind.Keyword);
        }

        public FplCompletionItem Duplicate() => Copy();

        public FplCompletionItem WithShortAdjusted()
        {
            if (!IsShort)
            {
                return Copy(isShort: true, sortText: "z" + SortText);
            }
            return Copy();
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
                _ => new FplCompletionItemChoicesDefault().GetChoices(this),
            };
        }

        public static string StripQuotesOrBrackets(string str)
        {
            if (str.StartsWith('\'') && str.EndsWith('\'') || str.StartsWith('<') && str.EndsWith('>'))
            {
                return str[1..^1];
            }
            else
            {
                return str;
            }
        }
    }
}
