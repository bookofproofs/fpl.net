(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module providing localization symbol-table nodes for FPL. Localization nodes in the symbol table are used to translate FPL code into a natural-language or LaTeX-style text containing mathematical statements.
/// </summary>
/// <remarks>
/// This module implements nodes that model localization blocks, translations and
/// language entries. Localization nodes integrate with the heap helper to determine
/// the active language at runtime and emit diagnostics for missing translations or
/// duplicate language entries.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.Localization
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util


/// <summary>
/// Represents a localization block that stores translations for multiple languages.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <param name="runOrder">Execution order for running localization blocks.</param>
/// <returns>An instance of <c>FplLocalization</c>.</returns>
/// <remarks>
/// - The node is a block and may contain language entries as scoped variables.
/// - At runtime the active language is read from <c>heap.Helper.CurrentLanguage</c>.
/// - If the active language has no translation defined, an <c>ST004</c> diagnostic is emitted.
/// </remarks>
type FplLocalization(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericNode(positions, Some parent)
    let _runOrder = runOrder
    let mutable _currentLanguage = ""

    override this.Name = LiteralLocL
    override this.ShortName = LiteralLoc

    /// <summary>
    /// Create a shallow clone of the localization block preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplLocalization</c> instance.</returns>
    override this.Clone () =
        let ret = new FplLocalization((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Produce the textual type representation of this localization block.
    /// </summary>
    /// <param name="signatureType">Requested signature type format.</param>
    /// <returns>String describing the block head and parameter types, if any.</returns>
    /// <remarks>
    /// The resulting string lists types of language entry variables, if present.
    /// </remarks>
    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let paramT =
            this.Scope
            |> Seq.filter (fun (kvp: KeyValuePair<string, FplGenericNode>) -> isVar kvp.Value)
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.map (fun fv -> fv.Type signatureType)
            |> String.concat ", "

        match paramT with
        | "" -> head
        | _ -> sprintf "%s(%s)" head paramT

    /// <summary>
    /// Represent the current localization value as the translation for the active language.
    /// </summary>
    /// <returns>String representation of the translation for the current language, or the type name if none.</returns>
    /// <remarks>
    /// Uses the remembered <c>_currentLanguage</c> key to select the language entry from the block scope.
    /// </remarks>
    override this.Represent() = // done
        if this.Scope.ContainsKey(_currentLanguage) then
            let language = this.Scope[_currentLanguage]
            language.Represent() // represent the current language
        else
            this.Type(SignatureType.Name) 
        
    /// <summary>
    /// Mark this node as a block node.
    /// </summary>
    /// <returns>True indicating the node contains nested entries.</returns>
    override this.IsBlock() = true

    /// <summary>
    /// Initialize runtime state for localization and validate the active language.
    /// </summary>
    /// <remarks>
    /// - Sets <c>_currentLanguage</c> from <c>heap.Helper.CurrentLanguage</c>.
    /// - If the active language is not present in the block, emits an <c>ST004</c> diagnostic.
    /// </remarks>
    /// <exceptions>
    /// <exception>
    /// Emits <c>ST004</c> diagnostic when the current language is missing and the first argument
    /// (presumed language expression) is available.
    /// </exception>
    /// </exceptions>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        _currentLanguage <- heap.Helper.CurrentLanguage // remember current language for Represent()
        if not (this.Scope.ContainsKey(_currentLanguage)) then
            let expression = this.ArgList[0]
            this.ErrorOccurred <- emitST004Diagnostics _currentLanguage expression.StartPos expression.EndPos
        StaticDebug.Debug(this,Debug.Stop)

    override this.RunOrder = Some _runOrder

    /// <summary>
    /// Embed the localization block into its parent using typed-signature helpers.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = tryAddToParentUsingTypedSignature this

/// <summary>
/// Represents a single translation term used inside a language entry.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent node (language entry) in the symbol table.</param>
/// <returns>An instance of <c>FplTranslation</c>.</returns>
/// <remarks>
/// The node's representation is provided by its <c>FplId</c> and it does not execute at runtime.
/// </remarks>
type FplTranslation(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Name = PrimTranslationL
    override this.ShortName = PrimTranslation

    /// <summary>
    /// Create a shallow clone of the translation node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplTranslation</c> instance.</returns>
    override this.Clone () =
        let ret = new FplTranslation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Return the type representation of the translation term according to requested signature.
    /// </summary>
    /// <param name="signatureType">Requested signature type.</param>
    /// <returns>Head plus space-separated argument representations.</returns>
    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = signatureSep " " this.ArgList SignatureType.Name
        sprintf "%s%s" head args

    /// <summary>
    /// Represent the translation according to the stored identifier.
    /// </summary>
    /// <returns>String stored in <c>FplId</c> representing the translation text.</returns>
    override this.Represent() = // done
        this.FplId // represent according to string in the FplId of the translation term

    /// <summary>
    /// No runtime execution is required for translation terms.
    /// </summary>
    override this.Run() = 
        // no run necessary 
        ()

    /// <summary>
    /// Attach the translation expression to its parent's argument list.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

/// <summary>
/// Represents a language entry that aggregates multiple translations for a single language.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent localization block node.</param>
/// <returns>An instance of <c>FplLanguage</c>.</returns>
/// <remarks>
/// The language node's representation concatenates its child translation representations
/// separated by spaces. When embedded, it registers itself in the parent scope and emits
/// ID014 on duplicate language identifiers.
/// </remarks>
type FplLanguage(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Name = PrimLanguageL
    override this.ShortName = PrimLanguage

    /// <summary>
    /// Create a shallow clone of the language node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplLanguage</c> instance.</returns>
    override this.Clone () =
        let ret = new FplLanguage((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Return the textual head for this language node.
    /// </summary>
    /// <param name="signatureType">Requested signature type.</param>
    /// <returns>Head string for the language node.</returns>
    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    /// <summary>
    /// Represent the language by concatenating all contained translations.
    /// </summary>
    /// <returns>Concatenated representation of child translations.</returns>
    override this.Represent() = // done
        // concatenate all translations of the language
        representationSep " " this.ArgList 

    /// <summary>
    /// No runtime execution is required for language entries.
    /// </summary>
    override this.Run() = 
        // no run necessary 
        ()

    /// <summary>
    /// Embed this language entry into its parent localization block scope.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    /// <remarks>
    /// If the parent already contains an entry with the same language identifier,
    /// an <c>ID014</c> diagnostic is emitted and the collision is recorded in <c>ErrorOccurred</c>.
    /// </remarks>
    /// <exceptions>
    /// <exception>Emits <c>ID014</c> when a duplicate language identifier is detected in the parent scope.</exception>
    /// </exceptions>
    override this.EmbedInSymbolTable _ = 
        let parent = this.Parent.Value
        if parent.Scope.ContainsKey(this.FplId) then 
            let conflict = parent.Scope[this.FplId]
            this.ErrorOccurred <- emitID014Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
        else
            parent.Scope.Add(this.FplId, this)

    override this.RunOrder = None
