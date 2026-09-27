module Fpl0Base.TestConfig

#if FPL_IS_OFFLINE
let IsOffline = true
#else
let IsOffline = false
#endif

#if FPL_DEBUG_PARSER
let DebugModeParser = true
#else
let DebugModeParser = false
#endif

#if FPL_DEBUG_INTERPRETER
let DebugModeInterpreter = true
#else
let DebugModeInterpreter = false
#endif


// Sets or gets the current OfflineMode.
// OfflineMode=True cannot be used in production.
// If true, the unit tests will try to get a local copy
// of Fpl libraries instead of trying to download them from the Internet.
// Even if TestConfig.IsOffline = false, the flag can be set to avoid
// downloading standard libraries if the test FPL code does not require them
type OfflineWatcher() =
    let mutable _flag = false

    member this.OfflineMode
        with get () = IsOffline || _flag
        and set (value) = _flag <- value


