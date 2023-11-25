using OmniSharp.Extensions.LanguageServer.Protocol.Models;

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
                case "(closed) left bound":
                case "(open) left bound":
                case "(open) right bound":
                case "(closed) right bound":
                    return new FplCompletionItemChoicesBound().GetChoices(this);
                case "digits":
                case "dollarDigits":
                    return new FplCompletionItemChoicesDigits().GetChoices(this);
                case "argument identifier":
                    return new FplCompletionItemChoicesArgumentIdentifier().GetChoices(this);
                case "language-specific string":
                    return new FplCompletionItemChoicesString().GetChoices(this);
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
                case "del":
                case "delegate":
                    return new FplCompletionItemChoicesDelegate().GetChoices(this);
                case "is":
                    return new FplCompletionItemChoicesIsOperator().GetChoices(this);
                case "alias":
                case "assert":
                case "ass":
                case "assume":
                case "bydef":
                case "cl":
                case "class":
                case "con":
                case "conclusion":
                case "end":
                case "ext":
                case "func":
                case "function":
                case "ind":
                case "index":
                case "intr":
                case "intrinsic":
                case "in":
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
                case "base":
                case "@":
                    return new FplCompletionItemChoicesSelf().GetChoices(this);
                case "all":
                case "ex":
                case "exn":
                    return new FplCompletionItemChoicesQuantor().GetChoices(this);
                case "true":
                case "false":
                case "undef":
                case "undefined":
                case "not":
                case "xor":
                case "iif":
                case "impl":
                case "and":
                case "or":
                case "<":
                    return new FplCompletionItemChoicesPredicate().GetChoices(this);
                case "ctor":
                case "constructor":
                    return new FplCompletionItemChoicesConstructor().GetChoices(this);
                case "dec":
                case "declaration":
                    return new FplCompletionItemChoicesDeclaration().GetChoices(this);
                case "cases":
                    return new FplCompletionItemChoicesCases().GetChoices(this);
                case "for":
                    return new FplCompletionItemChoicesFor().GetChoices(this);
                case "prty":
                case "property":
                    return new FplCompletionItemChoicesProperty().GetChoices(this);
                case "ax":
                case "axiom":
                case "post":
                case "postulate":
                    return new FplCompletionItemChoicesAxiom().GetChoices(this);
                case "def":
                case "definition":
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
                case "conj":
                case "conjecture":
                    return new FplCompletionItemChoicesTheoremLikeStmt("Conjecture").GetChoices(this);
                case "inf":
                case "inference":
                    return new FplCompletionItemChoicesRuleOfInference().GetChoices(this);
                case "cor":
                case "corollary":
                    return new FplCompletionItemChoicesCorollary().GetChoices(this);
                case "prf":
                case "proof":
                    return new FplCompletionItemChoicesProof().GetChoices(this);
                case "loc":
                case "localization":
                    return new FplCompletionItemChoicesLocalization().GetChoices(this);
                case "uses":
                    return new FplCompletionItemChoicesUses().GetChoices(this);
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
                case "alias":
                    this.Detail = "alias";
                    this.SortText = "alias";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case "all":
                    this.Detail = "predicate (all quantor)";
                    this.SortText = "all";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "and":
                    this.Detail = "predicate (conjunction)";
                    this.SortText = "and";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "ass":
                    this.Detail = "argument (assume, short form)";
                    this.SortText = "assume02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case "assert":
                    this.Detail = "statement (assert)";
                    this.SortText = "assert";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "assume":
                    this.Detail = "argument (assume)";
                    this.SortText = "assume01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "ax":
                    this.Detail = "axiom (short form)";
                    this.SortText = "axiom02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "axiom":
                    this.Detail = "axiom";
                    this.SortText = "axiom01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "cases":
                    this.Detail = "statement (cases)";
                    this.SortText = "cases";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "cl":
                    this.Detail = "class (short form)";
                    this.SortText = "class02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case "class":
                    this.Detail = "class";
                    this.SortText = "class01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case "con":
                    this.Detail = "conclusion (short form)";
                    this.SortText = "conclusion02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case "conclusion":
                    this.Detail = "conclusion";
                    this.SortText = "conclusion01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case "cor":
                    this.Detail = "corollary (short form)";
                    this.SortText = "corollary02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "corollary":
                    this.Detail = "corollary";
                    this.SortText = "corollary01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "conj":
                    this.Detail = "conjecture (short form)";
                    this.SortText = "conjecture02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "conjecture":
                    this.Detail = "conjecture";
                    this.SortText = "conjecture01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "ctor":
                    this.Detail = "constructor (short form)";
                    this.SortText = "constructor02";
                    this.Kind = CompletionItemKind.Constructor;
                    this.IsShort = true;
                    break;
                case "constructor":
                    this.Detail = "constructor";
                    this.SortText = "constructor01";
                    this.Kind = CompletionItemKind.Constructor;
                    this.IsShort = false;
                    break;
                case "dec":
                    this.Detail = "declaration (short form)";
                    this.SortText = "declaration02";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = true;
                    break;
                case "declaration":
                    this.Detail = "declaration";
                    this.SortText = "declaration01";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "del":
                    this.Detail = "delegate (short form)";
                    this.SortText = "delegate02";
                    this.Kind = CompletionItemKind.Event;
                    this.IsShort = true;
                    break;
                case "delegate":
                    this.Detail = "delegate";
                    this.SortText = "delegate01";
                    this.Kind = CompletionItemKind.Event;
                    this.IsShort = false;
                    break;
                case "def":
                    this.Detail = "definition (short form)";
                    this.SortText = "definition02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "definition":
                    this.Detail = "definition";
                    this.SortText = "definition01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "end":
                    this.Detail = "extension (end of)";
                    this.SortText = "end";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case "ex":
                    this.Detail = "predicate (exists quantor)";
                    this.SortText = "ex";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "exn":
                    this.Detail = "predicate (exists n-times quantor)";
                    this.SortText = "exn";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "exn!":
                    this.Detail = "predicate (exists n-times quantor)";
                    this.SortText = "exn!";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "ext":
                    this.Detail = "extension (beginning of)";
                    this.SortText = "ext";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case "false":
                    this.Detail = "predicate (false)";
                    this.SortText = "false";
                    this.Kind = CompletionItemKind.Constant;
                    this.IsShort = false;
                    break;
                case "for":
                    this.Detail = "statement (for loop)";
                    this.SortText = "for";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "func":
                    this.Detail = "type (functional term, short form)";
                    this.SortText = "function02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case "function":
                    this.Detail = "type (functional term)";
                    this.SortText = "function01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case "iif":
                    this.Detail = "predicate (equivalence, <=>)";
                    this.SortText = "iif";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "impl":
                    this.Detail = "predicate (implication, =>)";
                    this.SortText = "impl";
                    this.Kind = CompletionItemKind.Operator;
                    this.IsShort = false;
                    break;
                case "in":
                    this.Detail = "clause (in type or in range)";
                    this.SortText = "in";
                    this.Kind = CompletionItemKind.Property;
                    this.IsShort = false;
                    break;
                case "ind":
                    this.Detail = "type (index, short form)";
                    this.SortText = "index02";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = true;
                    break;
                case "index":
                    this.Detail = "type (index)";
                    this.SortText = "index01";
                    this.Kind = CompletionItemKind.TypeParameter;
                    this.IsShort = false;
                    break;
                case "inf":
                    this.Detail = "rule of inference (short form)";
                    this.SortText = "inference02";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = true;
                    break;
                case "inference":
                    this.Detail = "rule of inference";
                    this.SortText = "inference01";
                    this.Kind = CompletionItemKind.Class;
                    this.IsShort = false;
                    break;
                case "intr":
                    this.Detail = "intrinsic (short form)";
                    this.SortText = "intrinsic02";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = true;
                    break;
                case "intrinsic":
                    this.Detail = "intrinsic";
                    this.SortText = "intrinsic01";
                    this.Kind = CompletionItemKind.Struct;
                    this.IsShort = false;
                    break;
                case "is":
                    this.Detail = "predicate (is of type)";
                    this.SortText = "is";
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
