using OmniSharp.Extensions.LanguageServer.Protocol.Server;

namespace FplLS
{
    internal class FplLsTraceLogger
    {
        public static void LogException(ILanguageServer languageServer, Exception ex, string context)
        {
            languageServer.Window.LogInfo(Pre + context + ": " + ExtractErrorMsg(ex).Substring(2));
        }

        public static void LogMsg(ILanguageServer languageServer, string message, string context) 
        {
            languageServer.Window.LogInfo(Pre + context + ": " + message);
        }

        private static string ExtractErrorMsg(Exception ex)
        {
            if (ex.InnerException != null)
            {
                return ExtractErrorMsg(ex.InnerException);
            }
            else
            {
                return "; " + ex.Message;
            }
        }

        public static string Pre => "######### ";
    }
}
