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
           "con"
           "conclusion"
           "constructor"
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
           "in"
           "is"
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
           "qed"
           "ret"
           "return"
           "rev"
           "revoke"
           "self"
           "trivial"
           "true"
           "undef"
           "undefined"
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
                        modChoices.AddRange(AddDefinitionChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'thm'":
                    case "'theorem'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Theorem"));
                        break;
                    case "'lem'":
                    case "'lemma'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Lemma"));
                        break;
                    case "'prop'":
                    case "'proposition'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Proposition"));
                        break;
                    case "'inf'":
                    case "'inference'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Inference"));
                        break;
                    case "'conj'":
                    case "'conjecture'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Conjecture"));
                        break;
                    case "'cor'":
                    case "'corollary'":
                        modChoices.AddRange(AddCorollaryChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'uses'":
                        modChoices.AddRange(AddUsesChoices(choice, index, line, col, firstIndex));
                        break;
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
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplIdentifier (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + "}" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDefinitionChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            // default class definition
            var ciClass = new CompletionItem();
            ciClass.Label = $"{choice.Substring(1, choice.Length - 2)} class SomeFplClass:obj{Environment.NewLine}" + "{" + $"{Environment.NewLine}\tintrinsic{Environment.NewLine}" + "}" + Environment.NewLine;
            ciClass.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ciClass, index, line, col, firstIndex);
            modChoices.Add(ciClass);
            // default predicate definition
            var ciPredicate = new CompletionItem();
            ciPredicate.Label = $"{choice.Substring(1, choice.Length - 2)} predicate SomeFplPredicate (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tintrinsic{Environment.NewLine}" + "}" + Environment.NewLine;
            ciPredicate.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ciPredicate, index, line, col, firstIndex);
            modChoices.Add(ciPredicate);
            // default functionalTerm definition
            var ciFunctionalTerm = new CompletionItem();
            ciFunctionalTerm.Label = $"{choice.Substring(1, choice.Length - 2)} function SomeFplFunctionalTerm () -> obj{Environment.NewLine}" + "{" + $"{Environment.NewLine}\tintrinsic{Environment.NewLine}" + "}" + Environment.NewLine;
            ciFunctionalTerm.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ciFunctionalTerm, index, line, col, firstIndex);
            modChoices.Add(ciFunctionalTerm);
            return modChoices;
        }

        private List<CompletionItem> AddTheoremLikeStatementChoices(string choice, int index, int line, int col, long firstIndex, string example)
        {
            var modChoices = new List<CompletionItem>();
            // default theorem-like statement 
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFpl{example} (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + "}" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddCorollaryChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            // default corollary
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplCorollary!1 (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + "}" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
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
            ci.Kind = CompletionItemKind.Text;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddUsesChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplNamespace{Environment.NewLine}";
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            var ci1 = new CompletionItem();
            ci1.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplNamespace alias Sfn{Environment.NewLine}";
            ci1.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci1, index, line, col, firstIndex);
            modChoices.Add(ci1);
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
