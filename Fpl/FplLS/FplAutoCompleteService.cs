using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Text;

namespace FplLS
{
    class FplAutoCompleteService
    {
        public async Task<IReadOnlyCollection<string>> GetParserChoices(StringBuilder builder, CompletionParams request)
        {

            var choices = "aaa,abc,acd,bbb,bac,bca,ccc,cba,cab";
            return choices.Split(',').ToList();
        }
    }
}
