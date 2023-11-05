using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Text;
using System;
using static FplParser;
using System.Reflection;

namespace FplLS
{
    class FplAutoCompleteService
    {
        /*
"alias"
           "all"
           "and"
           "assert"
           "ass"
           "assume"
           "cases"
           "cl"
           "class"
           "conj"
           "conjecture"
           "con"
           "conclusion"
           "constructor"
           "cor"
           "corollary"
           "ctor"
           "dec"
           "declaration"
           "del"
           "delegate"
           "else"
           "end"
           "ext"
           "ex"
           "exn"
           "false"
           "for"
           "func"
           "function"
           "iif"
           "impl"
           "ind"
           "index"
           "intr"
           "intrinsic"
           "inf"
           "inference"
           "in"
           "is"
           "lem"
           "lemma"
           "loc"
           "localization"
           "mand"
           "mandatory"
           "not"
           "obj"
           "object"
           "opt"
           "optional"
           "or"
           "pred"
           "predicate"
           "pre"
           "premise"
           "prf"
           "proof"
           "prop"
           "proposition"
           "qed"
           "ret"
           "return"
           "rev"
           "revoke"
           "self"
           "thm"
           "theorem"
           "trivial"
           "true"
           "undef"
           "undefined"
           "uses"
           "xor"        */

        public async Task<CompletionList> GetParserChoices(StringBuilder builder, int index, int line, int col)
        {
            var choicesTuple = FplParser.getParserChoicesAtPosition(builder.ToString(), index);
            var choices = choicesTuple.Item1;
            var firstIndex = choicesTuple.Item2;
            var modChoices = new List<CompletionItem>();
            var nl = Environment.NewLine;
            foreach (var choice in choices)
            {
                switch (choice)
                {
                    case "<whitespace>":
                    case "<significant whitespace>":
                        modChoices.AddRange(AddWhitespaceChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'ax'":
                    case "'axiom'":
                    case "'post'":
                    case "'postulate'":
                        modChoices.AddRange(AddAxiomChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'def'":
                    case "'definition'":
                    default:
                        modChoices.AddRange(AddDefaultChoices(choice, index, line, col, firstIndex));
                        break;
                }
            }
            return new CompletionList(modChoices);
        }

        private void GetTextEdidit(CompletionItem ci, int index, int line, int col, long firstIndex)
        {
            var mumberOfUserCharsFromParserChoices = index - (int)firstIndex;
            ci.TextEdit = new TextEdit
            {
                NewText = ci.Label,
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position
                {
                    Line = line,
                    Character = col - mumberOfUserCharsFromParserChoices
                }, new Position
                {
                    Line = line,
                    Character = col - mumberOfUserCharsFromParserChoices + 1
                })
            };
        }
        private List<CompletionItem> AddAxiomChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplIdentifier(){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + "}" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Keyword;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDefaultChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = choice;
            ci.Label = choice.Substring(1, choice.Length - 2);
            ci.Kind = CompletionItemKind.Value;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddWhitespaceChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = choice;
            ci.Label = " ";
            ci.Kind = CompletionItemKind.Text;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

    }
}
