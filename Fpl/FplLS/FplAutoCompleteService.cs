using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.ComponentModel.Design;
using System.Data;
using System.Diagnostics.Tracing;
using System.Text;


namespace FplLS
{
    public partial class FplAutoCompleteService
    {
        const string prefix = "_ ";

        public async Task<CompletionList> GetParserChoices(StringBuilder builder, int index)
        {
            // make sure we get the parser choices from the position before the typed character, not after it
            string s;
            if (index > 0)
            {
                s = builder.ToString().Substring(0, index - 1) + "§";
            }
            else
            {
                s = builder.ToString().Substring(0, index);
            }
            var choicesTuple = FplParser.getParserChoicesAtPosition(s, index);
            var choices = choicesTuple.Item1;
            var modChoices = new List<FplCompletionItem>();
            foreach (var choice in choices)
            {
                var defaultCi = new FplCompletionItem(choice);
                modChoices.AddRange(defaultCi.GetChoices());
            }
            return new CompletionList(modChoices);
        }

        private List<CompletionItem> AddWordChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();

            modChoices.Add(ci);
            return modChoices;
        }




    }


}
