using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Text;
using System;
using static FplParser;

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
           "def"
           "definition"
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
                var ci = new CompletionItem();
                switch (choice)
                {
                    case "<significant whitespace>":
                        ci.Detail = choice;
                        ci.Label = " ";
                        ci.Kind = CompletionItemKind.Text;
                        break;
                    case "'ax'":
                    case "'axiom'":
                    case "'post'":
                    case "'postulate'":
                        ci.Label = choice.Substring(1, choice.Length - 2) + $" SomeFplIdentifier(){nl}" + "{" + $"{nl}\ttrue{nl}" + "}" + nl;
                        ci.Kind = CompletionItemKind.Keyword;
                        break;
                    default:
                        ci.Detail = choice;
                        ci.Label = choice.Substring(1, choice.Length - 2);
                        ci.Kind = CompletionItemKind.Value;
                        break;
                }
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
                modChoices.Add(ci);

            }
            return new CompletionList(modChoices);
        }




    }
}
