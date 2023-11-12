﻿using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.ComponentModel.Design;
using System.Data;
using System.Diagnostics.Tracing;
using System.Globalization;


namespace FplLS
{

    public class FplCompletionItemChoicesTheoremLikeStmt : FplCompletionItemChoices
    {
        private string _statementType;
        public FplCompletionItemChoicesTheoremLikeStmt(string statementType)
        {
            _statementType = statementType.Trim();
        }

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci); ret.Add(ci);

            // keywords
            ret.Add(defaultCi);
            return ret;
        }

        private void SetBody(FplCompletionItem ci)
        {
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                TokenPremise = "pre";
                TokenConclusion = "con";
                ci.Detail = $"{_statementType.ToLower()} (short)";
                ci.SortText = "z" + ci.SortText;
            }
            ci.InsertText =
                $"{ci.Word} SomeFpl{_statementType}(){Environment.NewLine}" +
                GetBody(TokenLeftBrace, TokenPremise, TokenConclusion, TokenRightBrace);
        }

        public static string GetBody(string lb, string pre, string con, string rb)
        {
            return 
                $"{lb}{Environment.NewLine}" +
                $"\t{pre}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"\t{con}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"{rb}{Environment.NewLine}";
        }

    }
}