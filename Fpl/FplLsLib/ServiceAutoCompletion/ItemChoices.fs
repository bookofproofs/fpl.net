module FplLsLib.ServiceAutoCompletion.ItemChoices

open System.Collections.Generic
open Fpl.Primitives
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.ArgumentIdentifier
open FplLsLib.ServiceAutoCompletion.Axiom
open FplLsLib.ServiceAutoCompletion.Cases
open FplLsLib.ServiceAutoCompletion.Constructor
open FplLsLib.ServiceAutoCompletion.Corollary
open FplLsLib.ServiceAutoCompletion.Declaration
open FplLsLib.ServiceAutoCompletion.Default
open FplLsLib.ServiceAutoCompletion.Definition
open FplLsLib.ServiceAutoCompletion.Delegate
open FplLsLib.ServiceAutoCompletion.Digits
open FplLsLib.ServiceAutoCompletion.Extension
open FplLsLib.ServiceAutoCompletion.ExtensionString
open FplLsLib.ServiceAutoCompletion.For
open FplLsLib.ServiceAutoCompletion.Iso639
open FplLsLib.ServiceAutoCompletion.IsOperator
open FplLsLib.ServiceAutoCompletion.Keyword
open FplLsLib.ServiceAutoCompletion.Localization
open FplLsLib.ServiceAutoCompletion.MapCases
open FplLsLib.ServiceAutoCompletion.PascalCaseId
open FplLsLib.ServiceAutoCompletion.Predicate
open FplLsLib.ServiceAutoCompletion.Proof
open FplLsLib.ServiceAutoCompletion.Property
open FplLsLib.ServiceAutoCompletion.Quantifier
open FplLsLib.ServiceAutoCompletion.Regex
open FplLsLib.ServiceAutoCompletion.RuleOfInference
open FplLsLib.ServiceAutoCompletion.Self
open FplLsLib.ServiceAutoCompletion.String
open FplLsLib.ServiceAutoCompletion.TheoremLikeStmt
open FplLsLib.ServiceAutoCompletion.Uses
open FplLsLib.ServiceAutoCompletion.Variable
open FplLsLib.ServiceAutoCompletion.Whitespace
open FplLsLib.ServiceAutoCompletion.Word


/// <summary>
/// Returns the list of concrete completion suggestions applicable to the given item's word.
/// </summary>
type FplCompletionItem with


    /// <summary>
    /// Returns the list of concrete completion suggestions applicable to this item's word.
    /// </summary>
    member this.GetChoices() : List<FplCompletionItem> =
        match this.Word with
        | "ISO 639 language code" -> FplCompletionItemChoicesIso639().GetChoices(this)
        | "whitespace" | "significant whitespace" -> FplCompletionItemChoicesWhitespace().GetChoices(this)
        | "dollarDigits" -> FplCompletionItemChoicesDigits().GetChoices(this)
        | "argument identifier" -> FplCompletionItemChoicesArgumentIdentifier().GetChoices(this)
        | "language-specific string" -> FplCompletionItemChoicesString().GetChoices(this)
        | "extensionString" | "extension regex" -> FplCompletionItemChoicesRegex().GetChoices(this)
        | "word" -> FplCompletionItemChoicesWord().GetChoices(this)
        | PrimVariableL | "variable (got keyword)" | "variable (got template)" ->
            FplCompletionItemChoicesVariable().GetChoices(this)
        | PrimPascalCaseId -> FplCompletionItemChoicesPascalCaseId().GetChoices(this)
        | LiteralDel | LiteralDelL -> FplCompletionItemChoicesDelegate().GetChoices(this)
        | LiteralIs -> FplCompletionItemChoicesIsOperator().GetChoices(this)
        | LiteralAlias | LiteralAssL | LiteralAss | LiteralAssert | LiteralByDef | LiteralCl | LiteralClL
        | LiteralCon | LiteralConL | LiteralExt | LiteralExtL | LiteralFunc | LiteralFuncL | LiteralInd
        | LiteralIndL | LiteralIntr | LiteralIntrL | LiteralIn | LiteralObj | LiteralObjL | LiteralPred
        | LiteralPredL | LiteralPre | LiteralPreL | LiteralQed | LiteralRet | LiteralRetL | LiteralRev
        | LiteralRevL | LiteralTrivial -> FplCompletionItemChoicesKeyword().GetChoices(this)
        | LiteralSelf | LiteralBase | LiteralParent -> FplCompletionItemChoicesSelf().GetChoices(this)
        | LiteralAll | LiteralEx | LiteralExN -> FplCompletionItemChoicesQuantifier().GetChoices(this)
        | LiteralTrue | LiteralFalse | LiteralUndef | LiteralUndefL | LiteralNot | LiteralXor | LiteralIif
        | LiteralImpl | LiteralAnd | LiteralOr | "(" -> FplCompletionItemChoicesPredicate().GetChoices(this)
        | LiteralCtor | LiteralCtorL -> FplCompletionItemChoicesConstructor().GetChoices(this)
        | LiteralDec | LiteralDecL -> FplCompletionItemChoicesDeclaration().GetChoices(this)
        | LiteralCases -> FplCompletionItemChoicesCases().GetChoices(this)
        | LiteralFor -> FplCompletionItemChoicesFor().GetChoices(this)
        | LiteralPrty | LiteralPrtyL -> FplCompletionItemChoicesProperty().GetChoices(this)
        | LiteralAx | LiteralAxL | LiteralPost | LiteralPostL -> FplCompletionItemChoicesAxiom().GetChoices(this)
        | LiteralDef | LiteralDefL -> FplCompletionItemChoicesDefinition().GetChoices(this)
        | LiteralThm | LiteralThmL -> FplCompletionItemChoicesTheoremLikeStmt("Theorem").GetChoices(this)
        | LiteralLem | LiteralLemL -> FplCompletionItemChoicesTheoremLikeStmt("Lemma").GetChoices(this)
        | LiteralProp | LiteralPropL -> FplCompletionItemChoicesTheoremLikeStmt("Proposition").GetChoices(this)
        | LiteralConj | LiteralConjL -> FplCompletionItemChoicesTheoremLikeStmt("Conjecture").GetChoices(this)
        | LiteralInf | LiteralInfL -> FplCompletionItemChoicesRuleOfInference().GetChoices(this)
        | LiteralCor | LiteralCorL -> FplCompletionItemChoicesCorollary().GetChoices(this)
        | LiteralPrf | LiteralPrfL -> FplCompletionItemChoicesProof().GetChoices(this)
        | LiteralLoc | LiteralLocL -> FplCompletionItemChoicesLocalization().GetChoices(this)
        | LiteralUses -> FplCompletionItemChoicesUses().GetChoices(this)
        | _ -> FplCompletionItemChoicesDefault().GetChoices(this)
