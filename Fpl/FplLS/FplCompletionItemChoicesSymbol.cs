using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

namespace FplLS
{
    public class FplCompletionItemChoicesSymbol: FplCompletionItemChoices
    {
        private string _symbolType;
        private bool _declarative;
        private int _bitPattern;
        public FplCompletionItemChoicesSymbol(string symbolType, bool declarative)
        {
            _symbolType = symbolType;
            if (symbolType.StartsWith("postfix"))
            {
                _bitPattern = 1;
            }
            else if (symbolType.StartsWith("infix"))
            {
                _bitPattern = 2;
            }
            else if (symbolType.StartsWith("prefix"))
            {
                _bitPattern = 4;
            }
            else if (symbolType == "symbol" || symbolType == "object symbol")
            {
                _bitPattern = 8;
            }
            else
            {
                throw new ArgumentException($"Unknown symbol type {symbolType}");
            }

            _declarative = declarative;
        }

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            var st = _symbolType;
 
            foreach (KeyValuePair<char, System.Tuple<int,string,string>> item in FplGrammarCommons.mathSymbols)
            {
                if ((item.Value.Item1 & _bitPattern) == _bitPattern) 
                {
                    var ci = defaultCi.Clone();
                    var op = item.Key.ToString();
                    SetDetail(ci, item.Value.Item1);
                    // snippet
                    if (_declarative)
                    {
                        ci.Label = $"{TokenPrefix} {_symbolType} {op} {item.Value.Item2} ({item.Value.Item3})";
                        ci.InsertText = $"{_symbolType} \"{op}\"";
                    }
                    else
                    {
                        ci.Label = $"{TokenPrefix} {op} {item.Value.Item2} ({item.Value.Item3})";
                        ci.InsertText = $"{op}";
                    }
                    ci.Kind = CompletionItemKind.Operator;
                    ci.SortText = $"symbol {ci.InsertText}";
                    ret.Add(ci);
                }
            }
            return ret;

        }

        private void SetDetail(FplCompletionItem ci, int bits)
        {
            List<string> usableAs = new List<string>();
            if ((bits & 8) == 8 && 8 != _bitPattern)
            {
                usableAs.Add("object symbol");
            }
            if ((bits & 4) == 4 && 4 != _bitPattern)
            {
                usableAs.Add("prefix");
            }
            if ((bits & 2) == 2 && 2 != _bitPattern)
            {
                usableAs.Add("infix");
            }
            if ((bits & 8) == 1 && 1 != _bitPattern)
            {
                usableAs.Add("postfix");
            }
            if (usableAs.Count > 0)
            {
                ci.Detail = $"also usable as {string.Join(",", usableAs)}";
            }
            else
            {
                ci.Detail = $"usable as {_symbolType} only";
            }
        }
    }
}


