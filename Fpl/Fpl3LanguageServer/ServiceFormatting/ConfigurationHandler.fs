/// Handles the client-pushed <c>workspace/didChangeConfiguration</c> notification, translating
/// the <c>fplExtension.format</c> section of the payload into a
/// <see cref="Fpl1Parser.LSRelated.FormattingOptions.FormattingOptions"/> value and storing it in
/// the shared <see cref="SettingsStore"/>.
/// </summary>
module Fpl3LanguageServer.ServiceFormatting.ConfigurationHandler

open System.Threading
open System.Threading.Tasks
open MediatR
open Newtonsoft.Json.Linq
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Workspace
open Fpl3LanguageServer.Buffers.Logging
open Fpl3LanguageServer.Buffers.SettingsStore
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl3LanguageServer.ServiceFormatting.SettingsTranslation

/// <summary>
/// Extracts the <c>fplExtension.format</c> sub-object from a raw settings payload, regardless of
/// whether the client nested it under <c>fplExtension</c> or sent it already scoped.
/// </summary>
let private extractFormatSection (settings: JToken) : JToken =
    match settings with
    | :? JObject as root ->
        match root.TryGetValue("fplExtension") with
        | true, (:? JObject as fplExtension) ->
            match fplExtension.TryGetValue("format") with
            | true, formatSection -> formatSection
            | false, _ -> JObject() :> JToken
        | _ ->
            match root.TryGetValue("format") with
            | true, formatSection -> formatSection
            | false, _ -> settings
    | _ -> settings

/// <summary>
/// Handles <c>workspace/didChangeConfiguration</c> notifications, updating the shared
/// <see cref="SettingsStore"/> with the client's current FPL formatting preferences.
/// </summary>
type FormattingConfigurationHandler(languageServer: ILanguageServer, settingsStore: SettingsStore) =

    let mutable capability = DidChangeConfigurationCapability()

    interface IDidChangeConfigurationHandler with

        member _.Handle(request: DidChangeConfigurationParams, cancellationToken: CancellationToken) : Task<Unit> =
            try
                logMsg languageServer "Task<Unit>" "FormattingConfigurationHandler.Handle"
                let formatSection = extractFormatSection request.Settings
                let options = translate fplFormatDefaults formatSection
                settingsStore.UpdateOptions(options)
            with ex ->
                logException languageServer ex "FormattingConfigurationHandler.Handle"

            Unit.Task

        member _.GetRegistrationOptions() : obj =
            null

        member _.SetCapability(cap: DidChangeConfigurationCapability) =
            capability <- cap
