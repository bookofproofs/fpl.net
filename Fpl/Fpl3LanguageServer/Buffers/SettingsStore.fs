/// <summary>
/// Module holds a mutable storage of the current, user-configurable FPL formatting preferences, as pushed/pulled from the
/// language client's <c>fplExtension.format</c> configuration section.
/// </summary>
module Fpl3LanguageServer.Buffers.SettingsStore

open Fpl1Parser.LSRelated.FormattingOptions

/// <summary>
/// Holds the current, user-configurable FPL formatting preferences, as pushed/pulled from the
/// language client's <c>fplExtension.format</c> configuration section.
/// </summary>
/// <remarks>
/// A simple mutable-field store; no change-notification event is fired, since no other service
/// currently needs to react to settings changes.
/// </remarks>
type SettingsStore() =

    let mutable current : FormattingOptions = fplFormatDefaults

    /// <summary>The currently active formatting preferences.</summary>
    member _.Current = current

    /// <summary>Replaces the currently active formatting preferences.</summary>
    member _.UpdateOptions(opts: FormattingOptions) =
        current <- opts
