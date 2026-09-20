module FplLsLib.ServiceAutoCompletion.Service

//open System
//open System.Collections.Generic
//open System.Text
//open OmniSharp.Extensions.LanguageServer.Protocol.Models
//open OmniSharp.Extensions.LanguageServer.Protocol.Server
//open FplLsLib.Buffers.Logging
//open FplLsLib.ServiceAutoCompletion.Item
//open FplLsLib.ServiceAutoCompletion.ItemChoices

///// <summary>
///// Returns the list of parser-driven completion suggestions applicable at the given position
///// within the provided buffer contents.
///// </summary>
//let getParserChoices (builder: StringBuilder) (index: int) (languageServer: ILanguageServer) : Async<CompletionList> =
//    async {
//        // make sure we get the parser choices from the position before the typed character, not after it
//        let s =
//            if index > 0 then
//                builder.ToString().Substring(0, index - 1) + "§"
//            else
//                builder.ToString().Substring(0, index)

//        let modChoices = List<FplCompletionItem>()
//        try
//            let choicesTuple = Fpl.Parser.Main.getParserChoicesAtPosition s index
//            let choices = fst choicesTuple
//            let uniqueSymbols = HashSet<string>()
//            for choice in choices do
//                let defaultCi = FplCompletionItem(choice)
//                let completionItemChoices = defaultCi.GetChoices()
//                // prevent adding duplicate symbols if they can be used as infix postfix or prefix notation
//                for ci in completionItemChoices do
//                    if not (uniqueSymbols.Contains(ci.Label)) then
//                        modChoices.Add(ci)
//                        uniqueSymbols.Add(ci.Label) |> ignore
//        with
//        | ex -> logException languageServer ex "GetParserChoices"

//        return CompletionList(modChoices)
//    }
