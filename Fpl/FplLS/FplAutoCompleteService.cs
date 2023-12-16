using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using System.ComponentModel.Design;
using System.Data;
using System.Diagnostics.Tracing;
using System.Text;


namespace FplLS
{
    public partial class FplAutoCompleteService
    {
        const string prefix = "_ ";

        public async Task<CompletionList> GetParserChoices(StringBuilder builder, int index, ILanguageServer languageServer)
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
            var modChoices = new List<FplCompletionItem>();
            try
            {
                var choicesTuple = FplParser.getParserChoicesAtPosition(s, index);
                var choices = choicesTuple.Item1;
                foreach (var choice in choices)
                {
                    var defaultCi = new FplCompletionItem(choice);
                    modChoices.AddRange(defaultCi.GetChoices());
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
