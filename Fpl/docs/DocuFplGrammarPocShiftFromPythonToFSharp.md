# Changes in FPL

## Amendments to the FPL grammar, major change of the FPL parser
The original grammar until version 1.2.1 was the EBNF input for an FPL parser implemented using Python and the **tatsu** parser generator.

From the version 2.0 on, there are some major changes to the grammar:
* The FPL parser was implemented from scratch on .NET using F# and the **FParsec** library.
* There is no separate EBNF input file — the grammar is encoded as composable FParsec parsers.
* The syntax was tightened and some constructs were reworked to improve parseability and error diagnostics.

---

## Summary of major language changes (user-visible)

1. No more extra blocks for `uses`, `inference`, `theory`, `localization`; each building block uses a unique prefix keyword.
2. In-block variable type declarations and statements are wrapped with `dec` / `;`.
3. Classes allow multiple inheritance (from class/object types only).
4. Class constructors may call parental constructors using `base.ParentClass(...)`.
5. Explicit `intrinsic` keyword for intrinsic/empty definitions. Intrisic defintions may be simplified further by omitting a block `{...}` entirely.
6. `assert` is a statement (not a predicate in places where a predicate is expected).
7. `for` replaces `loop`/`range` with `for i in x` and `for i is <type>`.
8. New quantifier keyword `exn` for 'exists N' multi-occurrence forms (disambiguation).
9. In-built equality predicate using infix `=` enclosed by parentheses: `(x = y)` and chained equalities.
10. Named variable declarations in quantifiers `all x:Type (...)`.
11. Ranges removed from generic square-bracket usage; coordinate lists remain.
12. `delegate` prefix replaces `py`; delegate names regex expanded.
13. Simplified extension syntax using `ext` definitions and `@` tokens; indices now use `[...]`.
14. Simplified proofs and justification syntaxes.
15. `cases` branch syntax simplified to use `|` and default `?`.
16. New `constructor` (short `ctor`) keyword for constructors.
17. Better recognition of misplaced keywords and generics.
18. Localizations separators adjusted to avoid parse ambiguities.
19. `property` keyword replaces `mandatory`, no more `optional` properties. Optionality can be configured via inheritance chains.
20. Theorem-like statements use predicate blocks (no separate premise/conclusion blocks if unnecessary).
21. File-name driven namespaces; each `.fpl` file acts as a namespace and files must end with `;`.
22. Extended justification keywords, e.g. `bydef`, `byax`, `byinf`, `bycor`.
23. Stricter usage of qualifiers, coordinates and ranges.
24. User-defined infix/prefix/postfix/object symbols supported.
25. Self-containment: declaration order now matters (implementation- and parser-level facilitation).
26. `parent` replaces earlier `@self` semantics; `self` remains positional.
27. `and`, `or`, `xor` changed from n-ary to binary.
28. Error recovery capabilities added.

---

## Implementation mapping (Grammar.fs references)
The following maps user-visible changes to the concrete parsers and helpers in `Fpl/Parser/Grammar.fs`. Line numbers refer to the currently inspected `Grammar.fs`.

- Identifiers and naming:
  - Variable id regex: `IdStartsWithSmallCase` (line ~20)
  - PascalCase id: `idStartsWithCap` / `pascalCaseId` (lines ~25-31)
  - Namespace identifier: `namespaceIdentifier` (line ~36)

- Extensions and `@` syntax:
  - Extension body regex: `extensionString` (line ~48)
  - `extension` token: `extension` parser (line ~54)
  - Extension definition parsing: `keywordExtension`, `extensionName`, `definitionExtension`, `extensionSignature`, `extensionTerm` (lines ~59, ~64, ~1272, ~1262, ~1267)

- Variables and templates:
  - Variable parser with keyword/template exclusion: `variableX` and `variable` (lines ~71-81)
  - Template support: `keywordTemplate`, `templateWithTail`, `templateType` (lines ~280-296)

- Main keywords:
  - `self`, `parent`, `base`: `keywordSelf`, `keywordParent`, `keywordBaseClassReference` (lines ~94-104)
  - `for`, `in`: `keywordFor`, `keywordIn`, `forStatement`, `forInBody` (lines ~123-131, ~631-639)
  - `dec` (declaration block): `keywordDeclaration`, `varDeclOrSpecList` (lines ~1010-1025)
  - `intrinsic`: `keywordIntrinsic` (lines ~1169-1171)
  - `assert`: `keywordAssert` and `assertionStatement` (lines ~145-147, ~641-647)
  - `constructor` / `ctor`: `keywordConstructor`, constructor signature & block `constructorSignature`, `constructorBlock` (lines ~1179-1196)

- Quantifiers and `exn`:
  - `all`, `ex`, `exn` forms: `keywordAll`, `keywordEx`, `keywordExN`, `exists`, `existsTimesN` (lines ~224-236, ~852-880)
  - Named variable declarations used by quantifiers: `namedVariableDeclaration`, `namedVariableDeclarationList` (lines ~458-466, ~409)

- Equality and infix notation:
  - Infix symbol parser: `infixSymbolWithPos` (line ~897)
  - Infix expression chain and top-level infix handling: `pInfixExpr`, `pInfixExpr` post-processing, `expression` and `predicateRef` (lines ~965-998)
  - Equality is a normal infix symbol parsed by the infix parser and represented in AST as `InfixOp`.

- Cases, mapcases and `|`/`?` syntax:
  - `cases` keyword and `casesStatement`, `caseSingle`, `caseElse` (lines ~135-137, ~576-594)
  - The grammar implements the `case` token and the `case`-separated EBNF; updated `|`/`?` semantics are enforced in parsing of `caseSingle`/`caseElse`.

- Delegates:
  - Delegate parser recognizes new `delegate` prefix: `keywordDel`, `delegateName`, `fplDelegate` (lines ~118-120, ~563-569)

- Classes, inheritance, constructors and `base` calls:
  - Class keyword and signatures: `keywordClass`, `classSignature`, `classSignatureExtended` (lines ~1454-1479)
  - Multiple inheritance supported via `inheritedTypeList` in `classSignatureExtended` (lines ~1420-1422, ~1479)
  - Constructor parsing and `base` calls: `baseConstructorCall`, `baseClassName`, `constructor`, `constructorBlock` (lines ~650-657, ~1186-1196)

- Properties, functional term instances and `property` keyword:
  - `keywordProperty`, `predicateInstance`, `functionalTermInstance`, `predicateInstanceBlock`, `functionalTermInstanceBlock` (lines ~1200-1247)
  - `keywordIntrinsic` applied in these blocks to mark intrinsic definitions (line ~1169)

- Proofs and justifications:
  - Proof tokens and components: `keywordProof`, `proofSignature`, `proofBlock`, `proofContent`, justification/argument parsing `justificationIdentifier`, `justificationItem`, `justifiedArgument`, `proofArgumentList` (lines ~1381-1404, ~786-800, ~1350-1378)
  - `bydef`, `byax`, `byinf`, `bycor` handled via `byModifier` and `justificationIdentifier` (lines ~164-191, ~789)

- Localizations:
  - `keywordLocalization`, language/translation tuple and `ebnfTransl` parsing, change of separators handled in `ebnfTranslRef`/`ebnfTerm`/`language` (lines ~1505-1545)

- Building blocks and file-level grammar:
  - Generic building block list and top-level namespace parser: `buildingBlock`, `fplNamespace`, `stdParser` (lines ~1557-1569)
  - `uses` clause parsing: `keywordUses`, `usesClause` (lines ~324-330)

- Error recovery and parser hints:
  - The code uses `positions`, `attempt`, `opt`, `choice`, and many `attempt` wrappers to refine error reporting and to avoid pointing to the wrong position inside complex constructs (many occurrences; see `attempt` around name-parsers and in choice combinators throughout the file).
  - `positions` is used widely to capture source locations for AST nodes.
