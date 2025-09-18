using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using static FplPrimitives;
using System.Text;


namespace FplLS
{
    public partial class FplAutoCompleteService
    {

        public static async Task<CompletionList> GetParserChoices(StringBuilder builder, int index, ILanguageServer languageServer)
        {
            // make sure we get the parser choices from the position before the typed character, not after it
            string s;
            if (index > 0)
            {
                s = builder.ToString()[..(index - 1)] + "§";
            }
            else
            {
                s = builder.ToString()[..index];
            }
            var modChoices = new List<FplCompletionItem>();
            try
            {
                var choicesTuple = FplParser.getParserChoicesAtPosition(s, index);
                var choices = choicesTuple.Item1;
                HashSet<string> uniqueSymbols = [];
                foreach (var choice in choices)
                {
                    var defaultCi = new FplCompletionItem(choice);
                    var completionItemChoices = defaultCi.GetChoices();
                    // prevent adding duplicate symbols if they can be used as infix postfix or prefix notation
                    foreach (var ci in completionItemChoices)
                    {
                        if (!uniqueSymbols.Contains(ci.Label))
                        {
                            modChoices.Add(ci);
                            uniqueSymbols.Add(ci.Label);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                FplLsTraceLogger.LogException(languageServer, ex, "GetParserChoices");
            }

            return new CompletionList(modChoices);
        }



    }


}
