module Fpl3LanguageServer.Buffers.Logging

open System
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Window

/// <summary>
/// Recursively extracts the innermost exception message, prefixed with "; ".
/// </summary>
let rec private extractErrorMsg (ex: Exception) : string =
    match ex.InnerException with
    | null -> "; " + ex.Message
    | inner -> extractErrorMsg inner

/// <summary>
/// Logs an exception (innermost message only) to the language client's Window channel.
/// </summary>
let logException (languageServer: ILanguageServer) (ex: Exception) (context: string) =
    languageServer.Window.LogInfo(context + ": " + (extractErrorMsg ex).Substring(2))

/// <summary>
/// Logs an informational trace message to the language client's Window channel.
/// </summary>
let logMsg (languageServer: ILanguageServer) (message: string) (context: string) =
    languageServer.Window.LogInfo(context + ": " + message)
