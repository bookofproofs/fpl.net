module FplDelegates
open FplInterpreterTypes
open System.Collections.ObjectModel

(* MIT License

Copyright (c) 2024 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

(*
This module implements the delegates your FPL code is using. 

Please do not remove the delegate $equals since this is the inbuilt-equality
operator of the FPL language. Instead, if needed, try to rewrite it according to your needs.

*)

type Delegates() = 

    let _equal (a:FplValue) (b:FplValue) =
        match a.FplRepresentation with
        | FplRepresentation.PredRepr FplPredicate.Undetermined -> 
            failwithf "Predicate `=` cannot be evaluated because the first argument is undertermined."
        | _ -> 
            match b.FplRepresentation with
            | FplRepresentation.PredRepr FplPredicate.Undetermined -> 
                failwithf "Predicate `=` cannot be evaluated because the second argument is undertermined."
            | _ -> 
                failwithf "OK:%b" (a.FplRepresentation = b.FplRepresentation)

    let _externalDelegates = 
        Map.ofList [
            (
                "Equal", fun values -> 
                    match values with
                    | x::y::[] -> _equal x y
                    | _ -> failwithf "Predicate `=` takes 2 arguments, got %i." values.Length
            )
        ]

    member this.CallExternalDelegate(name: string, args: FplValue list) =
        let nameParams = name + ":" + args.Length.ToString()
        
        match _externalDelegates.TryFind(nameParams) with
        | Some func -> func args
        | None -> failwithf "Unknown delegate `%s`" name 


