/// <summary>
/// Pulls the client's current <c>fplExtension.format</c> settings on demand via the OmniSharp
/// <see cref="Microsoft.Extensions.Configuration.IConfiguration"/> abstraction (backed by LSP's
/// <c>workspace/configuration</c> request).
/// </summary>
/// <remarks>
/// This is used instead of relying solely on the client pushing
/// <c>workspace/didChangeConfiguration</c> notifications, since not all language clients (notably
/// recent <c>vscode-languageclient</c> versions without an explicit dynamic capability
/// registration) reliably send that notification. Pulling fresh settings on every formatting
/// request guarantees up-to-date behavior regardless of push-notification support.
/// </remarks>
module Fpl3LanguageServer.ServiceFormatting.SettingsPull

open Microsoft.Extensions.Configuration
open Newtonsoft.Json.Linq
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl3LanguageServer.ServiceFormatting.SettingsTranslation

/// <summary>
/// Converts the <c>fplExtension.format</c> section of the client-provided
/// <see cref="IConfiguration"/> into a flat <see cref="JObject"/> (one property per leaf key),
/// suitable for <see cref="SettingsTranslation.translate"/>.
/// </summary>
let private formatSectionToJObject (configuration: IConfiguration) : JToken =
    let section = configuration.GetSection("fplExtension").GetSection("format")
    let result = JObject()
    for child in section.GetChildren() do
        if not (isNull child.Value) then
            result.[child.Key] <- JValue(child.Value :> obj)
    result :> JToken

/// <summary>
/// Pulls and translates the client's current FPL formatting preferences, falling back to
/// <see cref="fplFormatDefaults"/> for any missing/malformed field. Never throws.
/// </summary>
let pullCurrentOptions (configuration: IConfiguration) : FormattingOptions =
    let formatSection = formatSectionToJObject configuration
    translate fplFormatDefaults formatSection
