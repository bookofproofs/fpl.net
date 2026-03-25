/// This module contains all utility types utils used by the FplInterpreter
/// as well as all interfaces used by its classes

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterUtils

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text
open System.IO
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplParser
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterAstPreprocessing

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
(*
    TODO: 1) implement a function ToPL0Form transforming a predicative expression into a PL0 formula by replacing predicates with free pred variables
             possible applications: see 1a) 
    TODO: 1a) implement a function ToTrueTable generating a true table out of ToPL0Form
             possible applications: see 2), 2a) 3) 4)
    TODO: 2) implement a satisfiability check to the Output of ToTrueTable
             possible applications: 
                issue error, if a formula of a theorem / axiom / conjecture is not satisfiable
                issue warning, if a sub formula is not satisfiable to replace it by false
    TODO: 2a) implement a tautology check to the output of ToTrueTable
             possible applications: 
                issue warning, if a formula of a theorem / axiom / conjecture is a tautology, because it could be replaced by a trivial true
                issue warning, if a sub formula is a tautology to replace it by true
    TODO: 3) implement a CanonicalDNF (disjunctive normal form) based on ToTrueTable with a sorted representation.
             possible applications:
                issue error, if in a proof there are two consecutive arguments aprev, anext whose outputs have the same ToTrueTables 
                    in terms of variables (its columns) that are not equivalent (have different rows)
    TODO: 4) implement unit tests for all inference rules defined in Fpl.Commons checking if the respective premises and conclusions produce the same outputs of ToTrueTable.
             In this case, it is ensured that each inference rule in this library is a tautology. This is a required for 
             FPL to use inference rules as a Hilbert Calculus (see definition D.Hoffmann "Theoretische Informatik" 3rd. ed., p. 98)
             respectively inference rules with a premise being a predicate list: Here it is sufficient to check, if each rule 
             conserves the tautology property: If each predicate in a list is a tautology, so is the conclusion (see D.Hoffmann, "Theoretische Informatik", p. 104)
    TODO: 5) ensure cleaned-up expressions by renaming variable with the same names in independent parts of the same formula expression.
             (see D.H. "Theoretische Informatik", 3rd. p. 119) 
             Implementation idea: This can be accomplished by moving the scope of variables declared in quantors to the containing FPL block, forcing renaming the variables by the end-user at coding-time of the formula. 
    TODO: 6) issue error if arity-0 predicates are intrinsically defined, enforcing true or false (see D.H. "Theoretische Informatik", 3rd. p. 120) 
    TODO: 7) write functions for normalizing predicative formulas (see D.H. "Th. Inf", 3rd. p. 122-123):
                NormalizeNeg - (uses cleaned-up expressions - see 5) replace impl, iif, xor, by and/or/not and move all negations from the beginning of non-atomic formula to its atomic sub formulas 
                NormalizePrenex - (uses the output of NormalizeNeg): move quantors from all sub formulas the most outer quantor using fixed rules (see figure 3.35, p. 122)
                NormalizeSkolem - (uses the output of NormalizePrenex): eliminated all exists-quantors from the formula
                    there are two use cases: 
                        exists quantor is not proceeded by all quantors: - then just remove the ex quantor by replacing x <- u() with some intrinsic 0-ary function u()->tpl {intr} (some constant u fulfilling p)
                            for instance, 
                                ex x:tpl { p(x) } 
                            will be transformed to 
                                p(u())
                        exists quantor is proceeded by some all quantors: then remove the ex quantor by replacing x<-g_p(x1,x2) with some intrinsic n-ary function g(tpl1,tpl2)->tpl {intr} (some function fulfilling p, depending on proceeding variables bound by all quantors) 
                            for instance, 
                                all x1:tpl1, x2:tpl2, x:tpl {p(x1,x2,x)} 
                            will be transformed to  
                                all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
    TODO: 8) write unit-test checking if FplValue.Type(SignatureType.Type) of expressions like p(u()) or all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
        includes full signatures of the functions u() and g(,), .i.e., including their mappings. This will later become necessary 
        to be able to recognize the satisfiability-equivalence of two NormalizeSkolem outputs (see 7)
        For the term "satisfiability-equivalence" see D.H. "Th. Inf", 3rd. p. 124
*)


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

type Debug =
    | Start
    | Stop

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
