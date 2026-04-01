/// This module contains helper variables used by the FplInterpreter.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module FplInterpreter.Globals.Helpers
open FParsec


/// A type with helper variables storing some context during the creation process of the symbol table
type Helper() =
    let mutable _inSignatureEvaluation = false
    let mutable _inReferenceToProofOrCorollary = false
    // positions of the caller to prevent some diagnostics of being shown at the wrong position 
    let mutable _callerStartPos = Position("", 0,0,0)
    let mutable _callerEndPos = Position("", 0,0,0)
    let mutable _language = "tex" // the default language is tex, otherwise, it should be set in the FPL IDE extension

    let mutable _nextRunOrder = 0
    
    /// Indicates if a signature on a FPL building block is being evaluted
    member this.InSignatureEvaluation
        with get () = _inSignatureEvaluation
        and set (value) = _inSignatureEvaluation <- value


    /// Current language choice of all localizations
    member this.CurrentLanguage
        with get () = _language
        and set (value) = _language <- value

    /// Starting position of the caller
    member this.CallerStartPos
        with get () = _callerStartPos
        and set (value) = _callerStartPos <- value

    /// End position of the caller 
    member this.CallerEndPos
        with get () = _callerEndPos
        and set (value) = _callerEndPos <- value


    /// Returns the next available RunOrder to be stored, when inserting an FplValue into its parent.
    /// The need for this functionality is that sometimes, the block is inserted into the parent's scope, which is a dictionary.
    /// When running the nodes in the dictionary, their run order will ensure that they are being run in the order they have bin inserted.
    /// This order is incremented and stored when specific FplValue when they are created.
    /// All FplValues can have either Some or None RunOrder.
    /// Those with Some RunOrder include e.g. the following building blocks: axioms, theorems, lemmas, propositions, proofs, corollaries, arguments in proofs.
    /// Those with None include all other types of FplValues. They do not run by their own. They are "called" by those with Some RunOrder.
    member this.GetNextAvailableFplBlockRunOrder = 
        _nextRunOrder <- _nextRunOrder + 1
        _nextRunOrder

    /// Indicates if this EvalStack is evaluating a ReferenceToProofOrCorollary
    member this.InReferenceToProofOrCorollary
        with get () = _inReferenceToProofOrCorollary
        and set (value) = _inReferenceToProofOrCorollary <- value
