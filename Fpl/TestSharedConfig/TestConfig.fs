namespace TestSharedConfig

module TestConfig =
    let IsOffline = false
    let DebugMode = false


    /// Sets or gets the current OfflineMode.
    /// OfflineMode=True cannot be used in production.
    /// If true, the unit tests will try to get a local copy
    /// of Fpl libraries instead of trying to download them from the Internet.
    /// Even if TestConfig.IsOffline = false, the flag can be set to avoid
    /// downloading standard libraries if the test FPL code does not require them
    type OfflineWatcher() =
        let mutable _flag = false

        member this.OfflineMode
            with get () = IsOffline || _flag
            and set (value) = _flag <- value


