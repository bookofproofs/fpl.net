/// This module contains all utility types utils used by the FplInterpreter
/// as well as all interfaces used by its classes

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterUtils
open FParsec

type FixType =
    | Infix of string * int
    | Postfix of string
    | Prefix of string
    | Symbol of string
    | NoFix

    member this.Type =
        match this with
        | Infix(symbol, precedence) -> sprintf "infix `%s` (with precedence `%i`)" symbol precedence
        | Postfix symbol -> sprintf "postfix `%s` " symbol
        | Prefix symbol -> sprintf "prefix `%s` " symbol
        | Symbol symbol -> sprintf "symbol `%s`" symbol
        | NoFix -> "no fix"

    member this.GetUserDefinedLiteral defaultSymbol =
        match this with
        | Infix(symbol, _) -> symbol 
        | Postfix symbol -> symbol
        | Prefix symbol -> symbol
        | Symbol symbol -> symbol
        | NoFix -> defaultSymbol

type SignatureType =
    | Name
    | Type
    | Mixed

/// Maximum number of calls allowed for an Fpl Node
let maxRecursion = 15

/// Checks if a string starts with a lower case character string
let checkStartsWithLowerCase (s:string) =
    if s.Length > 0 then 
        System.Char.IsLower(s[0])
    else
        false

type IVariable =
    abstract member IsSignatureVariable : bool with get, set
    abstract member IsInitialized : bool with get, set

/// The interface IConstant is used to implement fixed but unknown objects of some type.
/// In general, the equality of two fixed but unknown objects of the same type cannot be determined, 
/// unless it is explicitly asserted (or explicitly negated) in the corresponding FPL theory.
/// However, once a ConstantName is established, the value will be treated like a fixed constant.
type IConstant =
    abstract member ConstantName : string with get
    abstract member SetConstantName: unit -> unit

type IHasSignature =
    abstract member SignStartPos : Position with get, set
    abstract member SignEndPos : Position with get, set

type ICanBeCalledRecusively =
    abstract member CallCounter : int

type IReady =
    abstract member IsReady : bool

type IHasProof =
    abstract member HasProof : bool with get, set

let stripLastDollarDigit (s: string) =
    let lastIndex = s.LastIndexOf('$')
    if lastIndex <> -1 then s.Substring(0, lastIndex) else s
