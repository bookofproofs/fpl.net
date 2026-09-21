module Fpl3LanguageServer.ServiceAutoCompletion.ItemChoices

open System.Collections.Generic
open Fpl0Base.Primitives
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.ArgumentIdentifier
open Fpl3LanguageServer.ServiceAutoCompletion.Axiom
open Fpl3LanguageServer.ServiceAutoCompletion.Cases
open Fpl3LanguageServer.ServiceAutoCompletion.Constructor
open Fpl3LanguageServer.ServiceAutoCompletion.Corollary
open Fpl3LanguageServer.ServiceAutoCompletion.Declaration
open Fpl3LanguageServer.ServiceAutoCompletion.Default
open Fpl3LanguageServer.ServiceAutoCompletion.Definition
open Fpl3LanguageServer.ServiceAutoCompletion.Delegate
open Fpl3LanguageServer.ServiceAutoCompletion.Digits
open Fpl3LanguageServer.ServiceAutoCompletion.Extension
open Fpl3LanguageServer.ServiceAutoCompletion.ExtensionString
open Fpl3LanguageServer.ServiceAutoCompletion.For
open Fpl3LanguageServer.ServiceAutoCompletion.Iso639
open Fpl3LanguageServer.ServiceAutoCompletion.IsOperator
open Fpl3LanguageServer.ServiceAutoCompletion.Keyword
open Fpl3LanguageServer.ServiceAutoCompletion.Localization
open Fpl3LanguageServer.ServiceAutoCompletion.MapCases
open Fpl3LanguageServer.ServiceAutoCompletion.PascalCaseId
open Fpl3LanguageServer.ServiceAutoCompletion.Predicate
open Fpl3LanguageServer.ServiceAutoCompletion.Proof
open Fpl3LanguageServer.ServiceAutoCompletion.Property
open Fpl3LanguageServer.ServiceAutoCompletion.Quantifier
open Fpl3LanguageServer.ServiceAutoCompletion.Regex
open Fpl3LanguageServer.ServiceAutoCompletion.RuleOfInference
open Fpl3LanguageServer.ServiceAutoCompletion.Self
open Fpl3LanguageServer.ServiceAutoCompletion.String
open Fpl3LanguageServer.ServiceAutoCompletion.TheoremLikeStmt
open Fpl3LanguageServer.ServiceAutoCompletion.Uses
open Fpl3LanguageServer.ServiceAutoCompletion.Variable
open Fpl3LanguageServer.ServiceAutoCompletion.Whitespace
open Fpl3LanguageServer.ServiceAutoCompletion.Word


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
