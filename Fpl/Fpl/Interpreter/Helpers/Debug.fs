(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Lightweight debug utilities used by the interpreter. Intended for development and testing only.
/// </summary>
module Fpl.Interpreter.Helpers.Debug
open System
open System.IO
open TestSharedConfig
open Fpl.Interpreter.BasicTypes

/// <summary>
/// Tracks a small recursion-depth counter used to indent debug traces.
/// </summary>
type Recursion() =
    let mutable _recursionLevel = 0

    /// <summary>
    /// Current recursion level.
    /// </summary>
    member this.RecursionLevel = _recursionLevel

    /// <summary>
    /// Increment the recursion level by one.
    /// </summary>
    member this.RecursionInc() = 
        _recursionLevel <- _recursionLevel + 1

    /// <summary>
    /// Decrement the recursion level by one.
    /// </summary>
    member this.RecursionDec() = 
        _recursionLevel <- _recursionLevel - 1


/// <summary>
/// Global recursion tracker used by debug helpers.
/// </summary>
let debugRec = Recursion()

/// <summary>
/// Mode selector for debug tracing.
/// </summary>
type Debug =
    /// <summary>Start of a traced operation.</summary>
    | Start
    /// <summary>End of a traced operation.</summary>
    | Stop

/// <summary>
/// Static debug helpers.
/// </summary>
type StaticDebug =
    /// <summary>
    /// Emit a debug record for a given node and debug mode. Calls are conditional on the DEBUG compilation symbol.
    /// </summary>
    /// <param name="fv">Node being traced.</param>
    /// <param name="debugMode">Whether this is the start or stop of a traced operation.</param>
    /// <returns>Unit.</returns>
    /// <remarks>
    /// This member is marked with <c>System.Diagnostics.Conditional("DEBUG")</c> so invocations are omitted in release builds.
    /// It also respects the test configuration flag that enables interpreter debug logging.
    /// </remarks>
    [<System.Diagnostics.Conditional("DEBUG")>]
    static member Debug(fv: FplGenericNode, debugMode: Debug) : unit =
        if TestSharedConfig.TestConfig.DebugModeInterpreter then
            let bars n = String.replicate n "| "
            let rec getPath (fv1:FplGenericNode) =
                match fv1.Parent with
                | Some parent -> $"{getPath parent} # {fv1.ShortName} {fv1.Type SignatureType.Name}"
                | None -> $"{fv1.ShortName}"
            let vars =
                fv.GetVariables()
                |> List.map (fun var -> $"{var.FplId}={var.Represent()}")
                |> String.concat ", "
            let indent = bars (debugRec.RecursionLevel)
            let logLine =
                match debugMode with
                | Debug.Start ->
                    debugRec.RecursionInc()
                    $"Start:{indent}{getPath fv}:[{fv.Represent()}][{vars}]{Environment.NewLine}"
                | Debug.Stop ->
                    debugRec.RecursionDec()
                    $"Stop :{indent.Substring(2)}{getPath fv}:[{fv.Represent()}][{vars}]{Environment.NewLine}"
            let currDir = Directory.GetCurrentDirectory()
            File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine)

/// <summary>
/// File-system watcher stub used for offline testing of debug flows.
/// </summary>
let offlineWatcher = TestConfig.OfflineWatcher()
