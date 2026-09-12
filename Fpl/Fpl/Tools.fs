module Fpl.Tools

open System
open System.IO
open System.Text.RegularExpressions
open System.Text

module GrammarToGml =

    /// Read an F# source file into a string.
    let readGrammarFile (path: string) : string =
        if String.IsNullOrWhiteSpace(path) then
            invalidArg "path" "path must not be empty"
        File.ReadAllText(path)

    /// Produce a GML graph from an FParsec grammar source.
    /// - `source` : entire source text of the grammar file
    /// - `excludeNodes` : identifiers to omit entirely from the graph (node names)
    /// Returns the GML file content as a string.
    let grammarToGml (source: string) (excludeNodes: string list) : string =
        if isNull source then invalidArg "source" "source must not be null"

        // 1) Find all top-level `let <name>` bindings (simple heuristic).
        //    We capture identifiers that follow `let` at the start of a line (allowing whitespace).
        let letPattern = Regex(@"(?m)^\s*let\s+([a-zA-Z_]\w*)", RegexOptions.Compiled)
        let letMatches = letPattern.Matches(source)

        // collect names in order of appearance
        let names =
            [ for m in letMatches -> m.Groups.[1].Value ]
            |> Seq.distinct
            |> Seq.toList

        // build map from name to the region of its RHS (from end of its declaration line to before next let)
        // First gather match indices
        let matchInfos =
            [ for m in letMatches ->
                  let name = m.Groups.[1].Value
                  let startIndex = m.Index
                  let endIndex = m.Index + m.Length
                  (startIndex, endIndex, name) ]
            |> List.sortBy (fun (s, _, _) -> s)

        // For each match, RHS goes from endIndex to next startIndex (or to end of file)
        let rhsMap =
            matchInfos
            |> List.mapi (fun i (_, endIndex, name) ->
                let nextStart =
                    if i + 1 < matchInfos.Length then
                        let (ns, _, _) = matchInfos.[i + 1]
                        ns
                    else
                        source.Length
                let length = nextStart - endIndex
                let rhs = if length > 0 then source.Substring(endIndex, length) else String.Empty
                (name, rhs))
            |> Map.ofList

        // 2) Determine edges: if RHS of A contains identifier B (whole-word) then A -> B
        //    Only consider B values that are in the names list (so we don't link to unrelated words).
        let nameSet = names |> Set.ofList
        let excludedSet = excludeNodes |> Set.ofList

        let edges =
            names
            |> List.collect (fun a ->
                if excludedSet.Contains(a) then
                    [] // omit node entirely (and thus its outgoing edges)
                else
                    let rhs =
                        match rhsMap.TryFind a with
                        | Some v -> v
                        | None -> String.Empty
                    // search for each candidate name B != A
                    names
                    |> List.filter (fun b -> b <> a && not (excludedSet.Contains(b)))
                    |> List.choose (fun b ->
                        // use whole-word match to avoid partial matches
                        let pattern = sprintf @"\b%s\b" (Regex.Escape b)
                        if Regex.IsMatch(rhs, pattern) then Some(a, b) else None
                    )
            )
            

        // 3) Build list of nodes (names minus excluded)
        let nodes = names |> List.filter (fun n -> not (excludedSet.Contains(n)))

        // map nodes to integer ids for GML
        let idMap =
            nodes
            |> List.mapi (fun i n -> (n, i))
            |> Map.ofList

        // 4) Emit GML text. Use a compact, compatible format for yEd.
        let sb = StringBuilder()
        sb.AppendLine("graph [") |> ignore
        sb.AppendLine("  directed 1") |> ignore

        // nodes
        for name in nodes do
            let id = idMap.[name]
            // label with the exact identifier
            sb.AppendLine("  node [") |> ignore
            sb.AppendLine(sprintf "    id %d" id) |> ignore
            // quote label properly
            let safeLabel = name.Replace("\"", "\\\"")
            sb.AppendLine(sprintf "    label \"%s\"" safeLabel) |> ignore
            sb.AppendLine("  ]") |> ignore

        // edges
        for (sourceName, targetName) in edges do
            // if either node missing (shouldn't happen) skip
            match idMap.TryFind sourceName, idMap.TryFind targetName with
            | Some sId, Some tId ->
                sb.AppendLine("  edge [") |> ignore
                sb.AppendLine(sprintf "    source %d" sId) |> ignore
                sb.AppendLine(sprintf "    target %d" tId) |> ignore
                sb.AppendLine("  ]") |> ignore
            | _ -> () // skip

        sb.AppendLine("]") |> ignore

        sb.ToString()

    /// Write GML string to file (creates directories if needed).
    let writeGmlToFile (gml: string) (outPath: string) : unit =
        if String.IsNullOrWhiteSpace(outPath) then invalidArg "outPath" "outPath must not be empty"
        let dir = Path.GetDirectoryName(outPath)
        if not (String.IsNullOrWhiteSpace(dir)) && not (Directory.Exists(dir)) then
            Directory.CreateDirectory(dir) |> ignore
        File.WriteAllText(outPath, gml)

    /// Convenience function that reads a grammar F# file, converts to gml and writes it out.
    /// - `inputPath` : path to the F# grammar source file
    /// - `outPath` : path to write the .gml file
    /// - `exclude` : optional list of names to omit from the graph (defaults include common FParsec combinators)
    let processGrammarFile (inputPath: string) (outPath: string) (exclude: string list option) : unit =
        let defaultExclude =
            [ "choice"; "attempt"; "many"; "opt"; "sepBy1"; "sepBy"; "sepEndBy1"
              "regex"; "pstring"; "pchar"; "pint32"; "puint32"
              "createParserForwardedToRef"; "pipe2"; "pipe3"; "resultSatisfies"
              "positions" ]
        let excludeList =
            match exclude with
            | Some l -> (defaultExclude @ l) |> List.distinct
            | None -> defaultExclude

        let src = readGrammarFile inputPath
        let gml = grammarToGml src excludeList
        writeGmlToFile gml outPath
