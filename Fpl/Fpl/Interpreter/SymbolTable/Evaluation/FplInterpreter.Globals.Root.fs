/// This module contains top-level classes of the symbol table,
/// including root and theories used by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Root
open System
open FParsec
open FplPrimitives
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug

type FplTheory(theoryName, parent: FplGenericNode, filePath: string, runOrder) as this =
    inherit FplGenericNode((Position("",0,1,1), Position("",0,1,1)), Some parent)
    let _runOrder = runOrder

    do
        this.FilePath <- Some filePath
        this.FplId <- theoryName
        this.TypeId <- theoryName

    override this.Name = PrimTheoryL
    override this.ShortName = PrimTheory

    override this.Clone () =
        let ret = new FplTheory(this.FplId, this.Parent.Value, this.FilePath.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getFplHead this signatureType

    /// The RunOrder in which this theory is to be executed.
    override this.RunOrder = Some _runOrder

    override this.EmbedInSymbolTable _ = 
        let next = this.Parent.Value
        // name conflicts of theories do not occur because of *.fpl file management 
        // and file-names being namespace names
        next.Scope.TryAdd(this.FplId, this) |> ignore

    /// Returns all Fpl Building Blocks that run on their own in this theory ordered by their RunOrder ascending.
    /// Only some of the building block run on their own in the theory, including axioms, theorems, lemmas, propositions, and conjectures.
    /// All other building blocks (e.g. rules of inferences, definitions of classes, etc.) are run when called by the first type of blocks.
    /// The RunOrder is set when creating the FplTheory during the parsing of the AST.
    member private this.OrderedBlocksRunningByThemselves =
        this.Scope.Values
        |> Seq.choose (fun block ->
            match block.RunOrder with
            | Some _ -> Some block
            | _ -> None)
        |> Seq.sortBy (fun block -> block.RunOrder.Value) 
        |> Seq.toList

    override this.Run() = 
        debug this Debug.Start 
        let blocks = this.OrderedBlocksRunningByThemselves
        blocks
        |> Seq.iter (fun block -> block.Run())        
        debug this Debug.Stop 

type FplRoot() =
    inherit FplGenericNode((Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
    override this.Name = PrimRoot
    override this.ShortName = PrimRoot

    override this.Clone () = this

    override this.Type _ = String.Empty

    override this.EmbedInSymbolTable _ = () 

    /// Returns all theories in the scope of this root ordered by their discovery time (parsing of the AST).
    /// This means that the theory with the lowest RunOrder comes first.
    member this.OrderedTheories =
        this.Scope.Values
        |> Seq.choose (fun item ->
            match item with
            | :? FplTheory as theory -> Some theory
            | _ -> None)
        |> Seq.sortBy (fun th -> th.RunOrder.Value) 

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        this.OrderedTheories
        |> Seq.iter (fun theory -> theory.Run())        
        debug this Debug.Stop

    member this.Clear() =
        this.ArgList.Clear()
        this.Scope.Clear()

