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
        let getActual (x:FplValue) = 
            if x.ValueList.Count > 0 then
                x.ValueList[0]
            else 
                x

        let a1 = getActual(a)
        let b1 = getActual(b)
        let a1Repr = a1.Type(SignatureType.Repr)
        let b1Repr = b1.Type(SignatureType.Repr)

        match a1Repr with
        | "undef" -> 
            failwithf "Predicate `=` cannot be evaluated because the left argument is undefined." 
        | _ -> ()

        match b1Repr with
        | "undef" -> 
            failwithf "Predicate `=` cannot be evaluated because the right argument is undefined." 
        | _ -> ()

        match a1Repr with
        | "undetermined" -> 
            failwithf "Predicate `=` cannot be evaluated because the left argument is undetermined." 
        | _ -> 
            match b1Repr with
            | "undetermined" -> 
                failwithf "Predicate `=` cannot be evaluated because the right argument is undetermined." 
            | _ -> 
                failwithf "OK:%b" (a1Repr = b1Repr)

    let _decrement (a:FplValue) =
        $"todo {a.Type(SignatureType.Repr)}" 

    let _externalDelegates = 
        Map.ofList [
            (
                "Equal", fun values -> 
                    match values with
                    | x::y::[] -> _equal x y
                    | _ -> failwithf "Predicate `=` takes 2 arguments, got %i." values.Length
            )
            (
                "Decrement", fun values -> 
                    match values with
                    | x::[] -> _decrement x
                    | _ -> failwithf "decrement takes 1 argument, got %i." values.Length
            )
        ]

    member this.CallExternalDelegate(name: string, args: FplValue list) =
        match _externalDelegates.TryFind(name) with
        | Some func -> func args
        | None -> failwithf "Unknown delegate `%s`" name 


