/// <summary>
/// Pulls the client's current <c>fplExtension.format</c> settings on demand by issuing a raw LSP
/// <c>workspace/configuration</c> request through the generic JSON-RPC client proxy.
/// </summary>
/// <remarks>
/// This bypasses <c>OmniSharp.Extensions.LanguageServer</c>'s higher-level
/// <see cref="Microsoft.Extensions.Configuration.IConfiguration"/> abstraction (exposed via
/// <c>ILanguageServer.Configuration</c>), which in this package version does not reliably perform
/// a live <c>workspace/configuration</c> round-trip — it instead reads from whatever has already
/// been pushed via <c>workspace/didChangeConfiguration</c> or <c>initializationOptions</c>, which
/// not all clients (notably <c>vscode-languageclient</c> without an explicit dynamic capability
/// registration) reliably send. Sending the request directly guarantees an up-to-date value
/// regardless of push-notification support.
/// </remarks>
module Fpl3LanguageServer.ServiceFormatting.SettingsPull

open System.Threading
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl3LanguageServer.ServiceFormatting.SettingsTranslation

/// <summary>
/// Issues a <c>workspace/configuration</c> request scoped to the <c>fplExtension.format</c>
/// section and translates the result into a <see cref="FormattingOptions"/> value, falling back
/// to <see cref="fplFormatDefaults"/> for any missing/malformed field. Never throws; on any
/// request/parsing failure, returns <paramref name="fallback"/> unchanged.
/// </summary>
let pullCurrentOptionsAsync (languageServer: ILanguageServer) (fallback: FormattingOptions) : Task<FormattingOptions> =
    task {
        try
            let configParams =
                ConfigurationParams(
                    Items = Container<ConfigurationItem>([| ConfigurationItem(Section = "fplExtension.format") |]))

            let! (result : JArray) =
                languageServer
                    .SendRequest("workspace/configuration", configParams)
                    .Returning<JArray>(CancellationToken.None)

            return
                result
                |> Seq.tryHead
                |> Option.map (fun (section: JToken) -> translate fallback section)
                |> Option.defaultValue fallback
        with _ ->
            return fallback
    }
