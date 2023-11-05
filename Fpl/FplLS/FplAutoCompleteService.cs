using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Text;

namespace FplLS
{
    class FplAutoCompleteService
    {
        public async Task<IReadOnlyCollection<string>> GetParserChoices(StringBuilder builder, CompletionParams request)
        {
            var input = builder.ToString().Substring(0, (int)request.Position.Character);

            var choices = "aaa,abc,acd,bbb,bac,bca,ccc,cba,cab";
            return choices.Split(',').ToList();
        }
    }
}
