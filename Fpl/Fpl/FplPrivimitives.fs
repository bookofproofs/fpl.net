module FplPrimitives

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FParsec

(* This module contains information needed by both, the error recovery module and the parser *)

(* Keyword constants *)
[<Literal>]
let LiteralAlias = "alias"
[<Literal>]
let LiteralAll = "all"
[<Literal>]
let LiteralAllSymbol = "∀"
[<Literal>]
let LiteralAnd = "and"
[<Literal>]
let LiteralAndSymbol = "∧"
[<Literal>]
let LiteralAss = "ass"
[<Literal>]
let LiteralAssL = "assume"
[<Literal>]
let LiteralAssert = "assert"
[<Literal>]
let LiteralAx = "ax"
[<Literal>]
let LiteralAxL = "axiom"
[<Literal>]
let LiteralBase = "base"
[<Literal>]
let LiteralByAx = "byax"
[<Literal>]
let LiteralByConj = "byconj"
[<Literal>]
let LiteralByCor = "bycor"
[<Literal>]
let LiteralByDef = "bydef"
[<Literal>]
let LiteralByInf = "byinf"
[<Literal>]
let LiteralCases = "cases"
[<Literal>]
let LiteralCl = "cl"
[<Literal>]
let LiteralClL = "class"
[<Literal>]
let LiteralCon = "con"
[<Literal>]
let LiteralConj = "conj"
[<Literal>]
let LiteralConjL = "conjecture"
[<Literal>]
let LiteralConL = "conclusion"
[<Literal>]
let LiteralCor = "cor"
[<Literal>]
let LiteralCorL = "corollary"
[<Literal>]
let LiteralCtor = "ctor"
[<Literal>]
let LiteralCtorL = "constructor"
[<Literal>]
let LiteralDec = "dec"
[<Literal>]
let LiteralDecL = "declaration"
[<Literal>]
let LiteralDef = "def"
[<Literal>]
let LiteralDefL = "definition"
[<Literal>]
let LiteralDel = "del"
[<Literal>]
let LiteralDelL = "delegate"
[<Literal>]
let LiteralEx = "ex"
[<Literal>]
let LiteralExSymbol = "∃"
[<Literal>]
let LiteralExN = "exn"
[<Literal>]
let LiteralExNSymbol = "∃!"
[<Literal>]
let LiteralExt = "ext"
[<Literal>]
let LiteralExtL = "extension"
[<Literal>]
let LiteralFalse = "false"
[<Literal>]
let LiteralFor = "for"
[<Literal>]
let LiteralFunc = "func"
[<Literal>]
let LiteralFuncL = "function"
[<Literal>]
let LiteralIif = "iif"
[<Literal>]
let LiteralIifSymbol = "⇔"
[<Literal>]
let LiteralImpl = "impl"
[<Literal>]
let LiteralImplSymbol = "⇒"
[<Literal>]
let LiteralIn = "in"
[<Literal>]
let LiteralInd = "ind"
[<Literal>]
let LiteralIndL = "index"
[<Literal>]
let LiteralInf = "inf"
[<Literal>]
let LiteralInfix = "infix"
[<Literal>]
let LiteralInfL = "inference"
[<Literal>]
let LiteralIntr = "intr"
[<Literal>]
let LiteralIntrL = "intrinsic"
[<Literal>]
let LiteralIs = "is"
[<Literal>]
let LiteralLem = "lem"
[<Literal>]
let LiteralLemL = "lemma"
[<Literal>]
let LiteralLoc = "loc"
[<Literal>]
let LiteralLocL = "localization"
[<Literal>]
let LiteralMapCases = "mcases"
[<Literal>]
let LiteralNot = "not"
[<Literal>]
let LiteralNotSymbol = "¬"
[<Literal>]
let LiteralObj = "obj"
[<Literal>]
let LiteralObjL = "object"
[<Literal>]
let LiteralOr = "or"
[<Literal>]
let LiteralOrSymbol = "∨"
[<Literal>]
let LiteralParent = "parent"
[<Literal>]
let LiteralPost = "post"
[<Literal>]
let LiteralPostFix = "postfix"
[<Literal>]
let LiteralPostL = "postulate"
[<Literal>]
let LiteralPre = "pre"
[<Literal>]
let LiteralPred = "pred"
[<Literal>]
let LiteralPredL = "predicate"
[<Literal>]
let LiteralPrefix = "prefix"
[<Literal>]
let LiteralPreL = "premise"
[<Literal>]
let LiteralPrf = "prf"
[<Literal>]
let LiteralPrfL = "proof"
[<Literal>]
let LiteralProp = "prop"
[<Literal>]
let LiteralPropL = "proposition"
[<Literal>]
let LiteralPrty = "prty"
[<Literal>]
let LiteralPrtyL = "property"
[<Literal>]
let LiteralQed = "qed"
[<Literal>]
let LiteralRet = "ret"
[<Literal>]
let LiteralRetL = "return"
[<Literal>]
let LiteralRev = "rev"
[<Literal>]
let LiteralRevL = "revoke"
[<Literal>]
let LiteralSelf = "self"
[<Literal>]
let LiteralSymbol = "symbol"
[<Literal>]
let LiteralThm = "thm"
[<Literal>]
let LiteralThmL = "theorem"
[<Literal>]
let LiteralTpl = "tpl"
[<Literal>]
let LiteralTplL = "template"
[<Literal>]
let LiteralTrivial = "trivial"
[<Literal>]
let LiteralTrue = "true"
[<Literal>]
let LiteralUndef = "undef"
[<Literal>]
let LiteralUndefL = "undefined"
[<Literal>]
let LiteralUses = "uses"
[<Literal>]
let LiteralXor = "xor"
[<Literal>]
let LiteralXorSymbol = "⩡"

let keyWordSet =
    HashSet<_>(
        [| LiteralAlias
           LiteralAll
           LiteralAnd
           LiteralAssert
           LiteralAss
           LiteralAssL
           LiteralAx
           LiteralAxL
           LiteralBase
           LiteralByAx
           LiteralByCor
           LiteralByDef
           LiteralByInf
           LiteralCases
           LiteralCl
           LiteralClL
           LiteralConj
           LiteralConjL
           LiteralCon
           LiteralConL
           LiteralCtorL
           LiteralCor
           LiteralCorL
           LiteralCtor
           LiteralDec
           LiteralDecL
           LiteralDef
           LiteralDefL
           LiteralDel
           LiteralDelL
           LiteralExt
           LiteralExtL
           LiteralEx
           LiteralExN
           LiteralFalse
           LiteralFor
           LiteralFunc
           LiteralFuncL
           LiteralIif
           LiteralImpl
           LiteralInd
           LiteralIndL
           LiteralInfix
           LiteralIntr
           LiteralIntrL
           LiteralInf
           LiteralInfL
           LiteralIn
           LiteralIs
           LiteralLem
           LiteralLemL
           LiteralLoc
           LiteralLocL
           LiteralMapCases
           LiteralNot
           LiteralObj
           LiteralObjL
           LiteralOr
           LiteralParent
           LiteralPost
           LiteralPostFix
           LiteralPostL
           LiteralPred
           LiteralPredL
           LiteralPre
           LiteralPrefix
           LiteralPreL
           LiteralPrf
           LiteralPrf
           LiteralProp
           LiteralPropL
           LiteralPrty
           LiteralPrtyL
           LiteralQed
           LiteralRet
           LiteralRetL
           LiteralRev
           LiteralRevL
           LiteralSelf
           LiteralSymbol
           LiteralThm
           LiteralThmL
           LiteralTrivial
           LiteralTrue
           LiteralUndef
           LiteralUndefL
           LiteralUses
           LiteralXor |]
    )

(* String primitives *)
[<Literal>]
let PrimArgInf = "ainf"
[<Literal>]
let PrimArg = "arg"
[<Literal>]
let PrimArgL = "argument"
[<Literal>]
let PrimAssertion = "assertion"
[<Literal>]
let PrimAssignment = "assign"
[<Literal>]
let PrimAssignmentL = "assignment statement"
[<Literal>]
let PrimArgInfAssume = "assumption"
[<Literal>]
let PrimArgInfDerive = "derived argument"
[<Literal>]
let PrimArgInfRevoke = "revocation"
[<Literal>]
let PrimArgInfTrivial = "trivial argument"
[<Literal>]
let PrimBaseConstructorCall = "base constructor call"
[<Literal>]
let PrimCasesL = "cases statement"
[<Literal>]
let PrimCaseElse = "else"
[<Literal>]
let PrimCaseElseL = "else case statement"
[<Literal>]
let PrimCaseSingle = "case"
[<Literal>]
let PrimCaseSingleL = "single case statement"
[<Literal>]
let PrimClass = "def cl"
[<Literal>]
let PrimClassL = "class definition"
[<Literal>]
let PrimConjunction = "conjunction"
[<Literal>]
let PrimDefaultConstructor = "default constructor"
[<Literal>]
let PrimDelegateEqual = "="
[<Literal>]
let PrimDelegateEqualL = "Equal"
[<Literal>]
let PrimDelegateDecrement = "decr"
[<Literal>]
let PrimDelegateDecrementL = "Decrement"
[<Literal>]
let PrimDigits = "Digits"
[<Literal>]
let PrimDisjunction = "disjunction"
[<Literal>]
let PrimEquivalence = "equivalence"
[<Literal>]
let PrimExclusiveOr = "exclusive disjunction"
[<Literal>]
let PrimExtension = "def ext"
[<Literal>]
let PrimExtensionL = "extension definition"
[<Literal>]
let PrimExtensionObj = "extension object"
[<Literal>]
let PrimFalse = "false value"
[<Literal>]
let PrimForInStmtL = "for in statement"
[<Literal>]
let PrimForInStmtDomain = "fordomain"
[<Literal>]
let PrimForInStmtDomainL = "for in statement's domain"
[<Literal>]
let PrimForInStmtEntity = "forentity"
[<Literal>]
let PrimForInStmtEntityL = "for in statement's entity"
[<Literal>]
let PrimFunctionalTerm = "def func"
[<Literal>]
let PrimFunctionalTermL = "functional term definition"
[<Literal>]
let PrimInstance = "inst" 
[<Literal>]
let PrimInstanceL = "instance"
[<Literal>]
let PrimIntrinsicInd = "intrinsic index"
[<Literal>]
let PrimIntrinsicTpl = "intrinsic template"
[<Literal>]
let PrimIntrinsicUndef = "intrinsic undefined"
[<Literal>]
let PrimIsOperator = "is operator"
[<Literal>]
let PrimImplication = "implication"
[<Literal>]
let PrimJIByAx = "justification by axiom"
[<Literal>]
let PrimJIByConj = "justification by conjecture"
[<Literal>]
let PrimJIByCor = "justification by corollary"
[<Literal>]
let PrimJIByDef = "justification by definition"
[<Literal>]
let PrimJIByDefVar = "justification by variable definition"
[<Literal>]
let PrimJIByInf = "justification by rule of inference"
[<Literal>]
let PrimJIByProofArgument = "justification by argument in another proof"
[<Literal>]
let PrimJIByRefArgument = "justification by argument reference"
[<Literal>]
let PrimJIByTheoremLikeStmt = "justification by theorem-like statement"
[<Literal>]
let PrimJustification = "just"
[<Literal>]
let PrimJustificationL = "justification"
[<Literal>]
let PrimLanguage = "lang"
[<Literal>]
let PrimLanguageL = "language"
[<Literal>]
let PrimMandatoryFunctionalTerm = "mfunc"
[<Literal>]
let PrimMandatoryFunctionalTermL = "functional term property"
[<Literal>]
let PrimMandatoryPredicate = "mpred"
[<Literal>]
let PrimMandatoryPredicateL = "predicate property"
[<Literal>]
let PrimMapCasesL = "mapcases statement"
[<Literal>]
let PrimMapCaseElse = "melse"
[<Literal>]
let PrimMapCaseElseL = "else mapcase statement"
[<Literal>]
let PrimMapCaseSingle = "mcase"
[<Literal>]
let PrimMapCaseSingleL = "single mapcase statement"
[<Literal>]
let PrimMapping = "map"
[<Literal>]
let PrimMappingL = "mapping"
[<Literal>]
let PrimNegation = "negation"
[<Literal>]
let PrimNone = "None"
[<Literal>]
let PrimPascalCaseId = "PascalCaseId"
[<Literal>]
let PrimPredicate = "def pred"
[<Literal>]
let PrimPredicateL = "predicate definition"
[<Literal>]
let PrimQuantor = "qtr"
[<Literal>]
let PrimQuantorAll = "all quantor"
[<Literal>]
let PrimQuantorExists = "exists quantor"
[<Literal>]
let PrimQuantorExistsN = "exists n times quantor"
[<Literal>]
let PrimReturn = "return statement"
[<Literal>]
let PrimRef = "ref"
[<Literal>]
let PrimRefL = "reference"
[<Literal>]
let PrimRuleOfInference = "rule of inference"
[<Literal>]
let PrimRoot = "root"
[<Literal>]
let PrimStmt = "stmt"
[<Literal>]
let PrimStmtL = "statement"
[<Literal>]
let PrimTitleAxioms = "Axioms"
[<Literal>]
let PrimTitleDerived = "Derived Arguments"
[<Literal>]
let PrimTitleRuleOfInference = "Inference Rules"
[<Literal>]
let PrimTitleTheorems = "Theorems"
[<Literal>]
let PrimTrue = "true value"
[<Literal>]
let PrimTheory = "th"
[<Literal>]
let PrimTheoryL = "theory"
[<Literal>]
let PrimTheoremLike = "thmlike"
[<Literal>]
let PrimTranslation = "trsl"
[<Literal>]
let PrimTranslationL = "translation"
[<Literal>]
let PrimUndetermined = "undet"
[<Literal>]
let PrimUndeterminedL = "undetermined value"
[<Literal>]
let PrimVariable = "var"
[<Literal>]
let PrimVariableL = "variable"
[<Literal>]
let PrimVariableArray = "*var"
[<Literal>]
let PrimVariableArrayL = "variable array"
