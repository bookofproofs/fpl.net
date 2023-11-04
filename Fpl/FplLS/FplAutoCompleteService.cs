namespace FplLS
{
    class FplAutoCompleteService
    {
        public async Task<IReadOnlyCollection<string>> GetParserChoices(string choices)
        {
            return choices.Split(',').ToList();
        }
    }
}
