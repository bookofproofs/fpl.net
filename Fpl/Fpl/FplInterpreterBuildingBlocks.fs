module FplInterpreterBuildingBlocks

open System
open System.Linq
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterPredicateEvaluator


let private addWithComma (name:string) str = 
    if str <> "" then
        if str = "(" || str = ")" 
            || str = "[" || str = "]" 
            || str = "->"
            || name.EndsWith "(" 
            || name.EndsWith "[" 
            || name.Length = 0 
            || name.EndsWith " " 
            || name.EndsWith "." 
            || str.StartsWith "$" then
                if str = "->" then 
                    name + " " + str + " "
                else
                    name + str
        else
            name + ", " + str
    else name

let rec adjustSignature (st:SymbolTable) (fplValue:FplValue) str = 
    let readyCheck = (FplValue.HasSignature(fplValue), fplValue.NameIsFinal) 
    
    if str <> "" then
        match readyCheck with
        | (true, SignatureIsFinal.Yes _) -> () //  for definitions with final name stop changing the TypeSignature
        | (false, SignatureIsFinal.Yes _) -> () //  for definitions with final name stop changing the TypeSignature
        | _ ->
            // note: the manipulation of the TypeSignature is necessary for all kinds of fplValue
            if str.StartsWith("*") && str <> "*" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["*"; str.Substring(1)]
            elif str.StartsWith("+") && str <> "+" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["+"; str.Substring(1)]
            elif str.StartsWith("$") && str <> "$" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["ind"]
            elif str = "true" || str = "false" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["pred"]
            else
                fplValue.TypeSignature <- fplValue.TypeSignature @ [str]

        match fplValue.Parent with
        | Some parent -> 
                if parent.BlockType <> FplValueType.Theory then 
                    adjustSignature st parent str
        | None -> ()

    if str <> "" && not (FplValue.IsVariable(fplValue)) then
        match readyCheck with
        | (true, SignatureIsFinal.Yes _) ->  () //  for definitions with final name stop changing the TypeSignature
        | (false, SignatureIsFinal.Yes _) ->  () //  for definitions with final name stop changing the TypeSignature
        | _ ->
            // note: the Name attribute of variables are set in Ast.Var directly
            // and we do not want to append the type to the names of variables.
            fplValue.Name <- addWithComma fplValue.Name str 

    if str ="(" || str = "[" then 
        fplValue.AuxiliaryInfo <- fplValue.AuxiliaryInfo + 1
    elif str =")" || str = "]" then 
        fplValue.AuxiliaryInfo <- fplValue.AuxiliaryInfo - 1

let setRepresentation (st: SymbolTable) representation = 
    let fv = st.ValueStack.Peek()
    fv.FplRepresentation <- representation

let eval_units (st: SymbolTable) unitType pos1 pos2 = 
    if unitType <> "" then 
        let fv = st.ValueStack.Peek()
        if FplValue.HasSignature(fv) then
            if (FplValue.IsVariadicVariableMany(fv)) then 
                adjustSignature st fv $"*{unitType}"
            elif (FplValue.IsVariadicVariableMany1(fv)) then 
                adjustSignature st fv $"+{unitType}"
            else
                adjustSignature st fv unitType
                checkID009_ID010_ID011_Diagnostics st fv unitType pos1 pos2
        elif (FplValue.IsVariadicVariableMany(fv)) then 
            adjustSignature st fv $"*{unitType}"
        elif (FplValue.IsVariadicVariableMany1(fv)) then 
            adjustSignature st fv $"+{unitType}"
        elif (FplValue.IsReference(fv)) then 
            checkID012Diagnostics st fv unitType pos1 pos2
        else
            adjustSignature st fv unitType
            checkID009_ID010_ID011_Diagnostics st fv unitType pos1 pos2


let eval_string (st: SymbolTable) s = ()

let eval_pos_string (st: SymbolTable) (startpos: Position) (endpos: Position) ast = ()

let eval_pos_unit (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast_ast_opt (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

let eval_pos_string_ast (st: SymbolTable) str = ()

let tryAddBlock (fplValue:FplValue) =

    match FplValue.TryFindAssociatedBlockForProof fplValue with
    | ScopeSearchResult.FoundAssociate parentsName -> 
        // everything is ok, change the parent of the provable from theory to the found parent 
        fplValue.Parent <- Some fplValue.Parent.Value.Scope[parentsName]
    | ScopeSearchResult.FoundIncorrectBlock block ->
        emitID002diagnostics fplValue block  
    | ScopeSearchResult.NotFound ->
        emitID003diagnostics fplValue  
    | ScopeSearchResult.FoundMultiple listOfKandidates ->
        emitID004diagnostics fplValue listOfKandidates  
    | _ -> ()

    match FplValue.TryFindAssociatedBlockForCorollary fplValue with
    | ScopeSearchResult.FoundAssociate parentsName -> 
        // everything is ok, change the parent of the provable from theory to the found parent 
        fplValue.Parent <- Some fplValue.Parent.Value.Scope[parentsName]
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
        emitVAR03diagnosticsForCorollarysSignatureVariable fplValue  
    | ScopeSearchResult.FoundIncorrectBlock block ->
        emitID005diagnostics fplValue block  
    | ScopeSearchResult.NotFound ->
        emitID006diagnostics fplValue  
    | ScopeSearchResult.FoundMultiple listOfKandidates ->
        emitID007diagnostics fplValue listOfKandidates  
    | _ -> ()

    match FplValue.InScopeOfParent(fplValue) fplValue.Name with
    | ScopeSearchResult.Found conflict -> 
        emitID001diagnostics fplValue conflict 
    | _ -> 
        fplValue.Parent.Value.Scope.Add(fplValue.Name,fplValue)
            (* let rec addAlsoToTheScopeUpToNextBlock (p:FplValue) =
                match p.Parent with
                | Some nextParent when FplValue.IsDeclaration(nextParent) || FplValue.IsVariable(nextParent) ->
                    nextParent.Scope.Add(fplValue.QualifiedName,fplValue)
                    addAlsoToTheScopeUpToNextBlock (nextParent) 
                | Some nextParent when FplValue.IsFplBlock(nextParent) || FplValue.IsConstructorOrProperty(nextParent) -> 
                    nextParent.Scope.Add(fplValue.QualifiedName,fplValue)
                | _ -> ()
            addAlsoToTheScopeUpToNextBlock(fplValue) 
            *)
            (* This code will fix a single VAR03 test but break ~1400 other unit tests!!!
               we leave the code for future inspiration of correctly fixing VAR03 
            fplValue.Parent.Value.Scope.Add(fplValue.Name,fplValue)
            match fplValue.Parent with
            | Some parent when FplValue.IsVariable(parent) -> 
                parent.Scope.Add(fplValue.Name,fplValue)
                let rec addAlsoToTheScopeToWhichTheLastParentVariableBelongsTo (p:FplValue) =
                    match p.Parent with
                    | Some otherParent when FplValue.IsVariable(otherParent) -> addAlsoToTheScopeToWhichTheLastParentVariableBelongsTo (otherParent)
                    | Some otherParent -> otherParent.Scope.Add(fplValue.Name,fplValue)
                    | _ -> ()
                addAlsoToTheScopeToWhichTheLastParentVariableBelongsTo(parent)
            | Some parent -> 
                parent.Scope.Add(fplValue.Name,fplValue)
            | _ -> ()
            *)

let propagateReference (refBlock:FplValue) withAdding = 
    let fplValue = refBlock.Parent.Value
    if fplValue.BlockType = FplValueType.Reference then
        if refBlock.AuxiliaryInfo = 0 then
            // propagate references only if refblock has all opened brackets closed
            // and the name of its reference-typed parent is not yet ready
            if not (fplValue.ValueList.Contains(refBlock)) then 
                if withAdding then 
                    fplValue.ValueList.Add(refBlock)
                match refBlock.FplRepresentation with
                | FplRepresentation.Pointer variable ->
                    fplValue.Name <- addWithComma fplValue.Name variable.Name 
                    fplValue.TypeSignature <- fplValue.TypeSignature @ variable.TypeSignature
                | _ -> 
                    fplValue.Name <- addWithComma fplValue.Name refBlock.Name 
                    fplValue.TypeSignature <- fplValue.TypeSignature @ refBlock.TypeSignature
    else
        if withAdding then 
            if not (fplValue.ValueList.Contains(refBlock)) then 
                fplValue.ValueList.Add(refBlock)



/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let evalMany (st:SymbolTable) blockType pos1 pos2 = 
        let fv = st.ValueStack.Peek()
        checkVAR00Diagnostics fv.AuxiliaryInfo pos1 pos2
        fv.BlockType <- blockType

    match ast with
    // units: | Star
    | Ast.IndexType((pos1, pos2),()) -> 
        st.EvalPush("IndexType")
        eval_units st "ind" pos1 pos2 
        setRepresentation st (FplRepresentation.Index ((uint)0))
        st.EvalPop() |> ignore
    | Ast.ObjectType((pos1, pos2),()) -> 
        st.EvalPush("ObjectType")
        eval_units st "obj" pos1 pos2 
        setRepresentation st (FplRepresentation.ObjRepr "obj")
        st.EvalPop()
    | Ast.PredicateType((pos1, pos2),()) -> 
        st.EvalPush("PredicateType")
        eval_units st "pred" pos1 pos2 
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        st.EvalPop()
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        st.EvalPush("FunctionalTermType")
        eval_units st "func" pos1 pos2  
        setRepresentation st (FplRepresentation.LangRepr FplLanguageConstruct.Function)
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        evalMany st FplValueType.VariadicVariableMany pos1 pos2
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        evalMany st FplValueType.VariadicVariableMany1 pos1 pos2
        st.EvalPop()
    | Ast.One((pos1, pos2),()) ->
        st.EvalPush("One")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Star((pos1, pos2),()) ->
        st.EvalPush("Star")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Dot((pos1, pos2),()) ->
        st.EvalPush("Dot")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Intrinsic((pos1, pos2),()) -> 
        st.EvalPush("Intrinsic")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Property((pos1, pos2),()) -> 
        st.EvalPush("Property")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Optional((pos1, pos2),()) -> 
        st.EvalPush("Optional")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Error  ->   
        st.EvalPush("Error")
        let pos = Position("",0,1,1)
        eval_units st "" pos pos
        st.EvalPop()
    // strings: | Digits of string
    | Ast.Digits s -> 
        st.EvalPush("Digits")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv s
        st.EvalPop()
    | Ast.PascalCaseId s -> 
        st.EvalPush("PascalCaseId")
        eval_string st s
        st.EvalPop() 
    | Ast.ExtensionRegex s -> 
        st.EvalPush("ExtensionRegex")
        eval_string st s
        st.EvalPop() 
    // | DollarDigits of Positions * int
    | Ast.DollarDigits((pos1, pos2), s) -> 
        st.EvalPush("DollarDigits")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv ("$"+s.ToString())
        fv.NameEndPos <- pos2
        st.EvalPop() 
    | Ast.Extensionname((pos1, pos2), s) ->
        st.EvalPush("Extensionname")
        let fv = st.ValueStack.Peek()
        if (FplValue.IsVariadicVariableMany(fv)) then 
            adjustSignature st fv ("*@" + s)
        elif (FplValue.IsVariadicVariableMany1(fv)) then 
            adjustSignature st fv ("+@" + s)
        else
            adjustSignature st fv ("@" + s)
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        let fv = st.ValueStack.Peek()
        if (FplValue.IsVariadicVariableMany(fv)) then 
            adjustSignature st fv ("*" + s)
        elif (FplValue.IsVariadicVariableMany1(fv)) then 
            adjustSignature st fv ("+" + s)
        else
            adjustSignature st fv s
        st.EvalPop() 
    | Ast.Var((pos1, pos2), name) ->
        st.EvalPush("Var")
        let fv = st.ValueStack.Peek()
        if FplValue.IsReference(fv) then
            match FplValue.VariableInBlockScopeByName fv name with 
            | ScopeSearchResult.Found variableInScope ->
                // replace the reference by a pointer to an existing declared variable
                fv.FplRepresentation <- FplRepresentation.Pointer variableInScope
            | _ -> 
                // otherwise, the variable is not declared, emit VAR01 diagnostics 
                fv.Name <- addWithComma fv.Name name
                fv.TypeSignature <- fv.TypeSignature @ ["undef"]
                emitVAR01diagnostics name pos1 pos2
        else
            let varValue = FplValue.CreateFplValue((pos1,pos2), FplValueType.Variable, fv)
            st.ValueStack.Push(varValue)
            match FplValue.VariableInBlockScopeByName(fv) name with
            | ScopeSearchResult.Found other ->
                // if found, the emit error that the variable was already declared.
                emitVAR03diagnostics fv other 
                st.ValueStack.Pop() |> ignore
                st.ValueStack.Push(other)
            | _ -> 
                varValue.Name <- name
                varValue.NameEndPos <- pos2
                varValue.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
                fv.Scope.Add(varValue.Name,varValue)
        st.EvalPop() 
    | Ast.DelegateId((pos1, pos2), s) -> 
        st.EvalPush("DelegateId")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv s
        st.EvalPop() 
    | Ast.Alias((pos1, pos2), s) -> 
        st.EvalPush("Alias")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.LocalizationString((pos1, pos2), s) -> 
        st.EvalPush("LocalizationString")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.ObjectSymbol((pos1, pos2), s) -> 
        st.EvalPush("ObjectSymbol")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv s
        st.EvalPop() 
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv s
        emitPR000Diagnostics fv s pos1 pos2
        st.EvalPop() 
    | Ast.Prefix((pos1, pos2), symbol) -> 
        st.EvalPush("Prefix")
        let fv = st.ValueStack.Peek()
        fv.ExpressionType <- FixType.Prefix symbol
        st.EvalPop() 
    | Ast.Infix((pos1, pos2), (symbol, precedenceAsts)) -> 
        st.EvalPush("Infix")
        let fv = st.ValueStack.Peek()
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
        emitSIG02Diagnostics st fv pos1 pos2 
        eval st precedenceAsts
        st.EvalPop() 
    | Ast.Postfix((pos1, pos2), symbol) -> 
        st.EvalPush("Postfix")
        let fv = st.ValueStack.Peek()
        fv.ExpressionType <- FixType.Postfix symbol
        st.EvalPop() 
    | Ast.Symbol((pos1, pos2), s) -> 
        st.EvalPush("Symbol")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.InfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("InfixOperator")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv symbol
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PostfixOperator")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv symbol
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PrefixOperator")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv symbol
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.True((pos1, pos2), _) -> 
        st.EvalPush("True")
        let fv = st.ValueStack.Peek()
        if FplValue.IsReference(fv) then
            fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
        adjustSignature st fv "true"
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        let fv = st.ValueStack.Peek()
        if FplValue.IsReference(fv) then
            fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
        adjustSignature st fv "false"
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv "undef"
        st.EvalPop() 
    | Ast.Trivial((pos1, pos2), _) -> 
        st.EvalPush("Trivial")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.Qed((pos1, pos2), _) -> 
        st.EvalPush("Qed")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    // | ExtDigits of Positions * Ast
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        st.EvalPush("RuleOfInference")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.RuleOfInference, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        eval st premiseConclusionBlockAst
        tryAddBlock fplValue 
        st.ValueStack.Pop() |> ignore
        st.EvalPop() 
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
        let fv = st.ValueStack.Peek()
        fv.NameEndPos <- pos2
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ExtDigits((pos1, pos2), ast1) ->
        st.EvalPush("ExtDigits")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ExtensionType((pos1, pos2), ast1) ->
        st.EvalPush("ExtensionType")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.UsesClause((pos1, pos2), ast1) ->
        st.EvalPush("UsesClause")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Not((pos1, pos2), predicateAst) ->
        st.EvalPush("Not")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv "not"
        adjustSignature st fv "("
        eval st predicateAst
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        evaluateNegation fv
        emitLG000orLG001Diagnostics fv "negation"
        st.EvalPop()
    | Ast.InEntity((pos1, pos2), ast1) ->
        st.EvalPush("InEntity")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.IsType((pos1, pos2), ast1) ->
        st.EvalPush("IsType")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Assertion((pos1, pos2), ast1) ->
        st.EvalPush("Assertion")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ByDef((pos1, pos2), predicateWithQualificationAst) ->
        st.EvalPush("ByDef")
        let fv = st.ValueStack.Peek()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature st fv "bydef."
        eval st predicateWithQualificationAst
        emitPR001Diagnostics fv pos1 pos2
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        st.EvalPush("DottedPredicate")
        let fv = st.ValueStack.Peek()
        fv.Name <- fv.Name + "."
        fv.TypeSignature <- fv.TypeSignature @ ["."]
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        st.ValueStack.Push(refBlock)
        eval st predicateWithOptSpecificationAst
        propagateReference refBlock true
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.Return((pos1, pos2), ast1) ->
        st.EvalPush("Return")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.AssumeArgument((pos1, pos2), ast1) ->
        st.EvalPush("AssumeArgument")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.RevokeArgument((pos1, pos2), ast1) ->
        st.EvalPush("RevokeArgument")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.VariableType((pos1, pos2), compoundVariableTypeAst) ->
        st.EvalPush("VariableType")
        eval st compoundVariableTypeAst
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.AST((pos1, pos2), ast1) ->
        st.EvalPush("AST")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.PredicateIdentifier((pos1, pos2), asts) ->
        st.EvalPush("PredicateIdentifier")

        let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        
        let fv = st.ValueStack.Peek()
        if FplValue.HasSignature(fv) then
            if (FplValue.IsVariadicVariableMany(fv)) then 
                adjustSignature st fv ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fv)) then 
                adjustSignature st fv ("+" + identifier)
            else
                adjustSignature st fv identifier
            checkID008Diagnostics fv pos1 pos2
            checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
            emitSIG04TypeDiagnostics st identifier fv pos1 pos2
        elif FplValue.IsVariable(fv) then
            if (FplValue.IsVariadicVariableMany(fv)) then 
                adjustSignature st fv ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fv)) then 
                adjustSignature st fv ("+" + identifier)
            else
                adjustSignature st fv identifier
            emitSIG04TypeDiagnostics st identifier fv pos1 pos2 
        elif FplValue.IsReference(fv) then
            adjustSignature st fv identifier
            checkID012Diagnostics st fv identifier pos1 pos2
            checkID012Diagnostics st fv identifier pos1 pos2
            emitSIG04TypeDiagnostics st identifier fv pos1 pos2

        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), namedVariableDeclarationListAsts) ->
        st.EvalPush("ParamTuple")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv "("
        namedVariableDeclarationListAsts |> List.map (
            fun child ->
            match child with 
            | Ast.NamedVarDecl(_,((varList,_),_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            eval st child
        ) |> ignore
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv "["
        asts 
        |> List.map (fun ast1 ->
            eval st ast1
        ) |> ignore
        adjustSignature st fv "]"
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        st.EvalPush("NamespaceIdentifier")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTerm((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTerm")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTermList((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTermList")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.BrackedCoordList((pos1, pos2), asts) ->
        st.EvalPush("BrackedCoordList")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv "["
        asts |> List.map (eval st) |> ignore
        adjustSignature st fv "]"
        st.EvalPop()
    | Ast.And((pos1, pos2), predicateAsts) ->
        st.EvalPush("And")
        let fv = st.ValueStack.Peek()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature st fv "and"
        adjustSignature st fv "("
        predicateAsts |> List.map (eval st) |> ignore
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        evaluateConjunction fv
        emitLG000orLG001Diagnostics fv "conjunction"
        st.EvalPop()
    | Ast.Or((pos1, pos2), predicateAsts) ->
        st.EvalPush("Or")
        let fv = st.ValueStack.Peek()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature st fv "or"
        adjustSignature st fv "("
        predicateAsts |> List.map (eval st) |> ignore
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        evaluateDisjunction fv
        emitLG000orLG001Diagnostics fv "disjunction"
        st.EvalPop()
    | Ast.Xor((pos1, pos2), predicateAsts) ->
        st.EvalPush("Xor")
        let fv = st.ValueStack.Peek()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature st fv "xor"
        adjustSignature st fv "("
        predicateAsts |> List.map (eval st) |> ignore
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        evaluateExclusiveOr fv
        emitLG000orLG001Diagnostics fv "exclusive-or"
        st.EvalPop()
    | Ast.VarDeclBlock((pos1, pos2), asts) ->
        st.EvalPush("VarDeclBlock")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.StatementList((pos1, pos2), asts) ->
        st.EvalPush("StatementList")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.DefaultResult((pos1, pos2), asts) ->
        st.EvalPush("DefaultResult")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Justification((pos1, pos2), asts) ->
        st.EvalPush("Justification")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.ArgumentTuple((pos1, pos2), asts) ->
        st.EvalPush("ArgumentTuple")
        let fv = st.ValueStack.Peek()
        adjustSignature st fv "("
        asts |> List.map (eval st) |> ignore
        adjustSignature st fv ")"
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        if asts.Length > 0 then
            let fv = st.ValueStack.Peek()
            asts |> List.map (eval st) |> ignore
            propagateReference fv true
        st.EvalPop()
    // | Namespace of Ast option * Ast list
    | Ast.Namespace(optAst, asts) ->
        st.EvalPush("Namespace")
        optAst |> Option.map (eval st) |> ignore
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // CompoundFunctionalTermType of Positions * ((Ast * Ast) option)
    | Ast.CompoundFunctionalTermType((pos1, pos2), (ast1, astTupleOption)) ->
        st.EvalPush("CompoundFunctionalTermType")
        eval st ast1
        match astTupleOption with 
        | Some (ast2, _) -> eval st ast2 |> ignore
        | _ -> ()
        match astTupleOption with 
        | Some (_, ast3) -> 
            let fv = st.ValueStack.Peek()
            adjustSignature st fv "->"
            fv.NameEndPos <- pos2
            eval st ast3 |> ignore
        | _ -> ()
        st.EvalPop()
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("AliasedNamespaceIdentifier")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.ClassType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("ClassType")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("CompoundPredicateType")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (referencingIdentifierAst, optArgumentTuple)) ->
        if optArgumentTuple.IsNone then
            st.EvalPush("ReferenceToProof")
        else
            st.EvalPush("ReferenceToCorollary")
        eval st referencingIdentifierAst
        optArgumentTuple |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.PredicateWithOptSpecification((pos1, pos2), (fplIdentifierAst, optionalSpecificationAst)) ->
        st.EvalPush("PredicateWithOptSpecification")
        let fv = st.ValueStack.Peek()
        match optionalSpecificationAst with
        | Some specificationAst -> 
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
            st.ValueStack.Push(refBlock)
            eval st fplIdentifierAst
            eval st specificationAst |> ignore
            // forget refBlock but propagate its name and typesignature into its parent
            propagateReference refBlock false
            match tryMatchSignatures st refBlock with
            | (_, _, Some matchedFplValue) -> ()
            | (firstFailingArgument, candidates, None) -> 
                emitSIG04Diagnostics refBlock candidates firstFailingArgument pos1 pos2 
            st.ValueStack.Pop() |> ignore
        | None -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst
        st.EvalPop()
    // | SelfAts of Positions * char list
    | Ast.SelfAts((pos1, pos2), chars) -> 
        st.EvalPush("SelfAts")
        let identifier = (chars |> List.map (fun c -> c.ToString()) |>  String.concat "") + "self"
        let fv = st.ValueStack.Peek()
        adjustSignature st fv identifier
        st.EvalPop()
    // | Translation of string * Ast
    | Ast.Translation(s, ast1) ->
        st.EvalPush("Translation")
        eval st ast1
        eval_pos_string_ast st s
        st.EvalPop()
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.InheritedClassType((pos1, pos2), ast1) -> 
        st.EvalPush("InheritedClassType")
        eval st ast1
        st.EvalPop()
    | Ast.ExtensionBlock((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("ExtensionBlock")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Impl")
        let fv = st.ValueStack.Peek()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature st fv "impl"
        adjustSignature st fv "("
        eval st predicateAst1
        eval st predicateAst2
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        evaluateImplication fv
        emitLG000orLG001Diagnostics fv "implication"
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Iif")
        let fv = st.ValueStack.Peek()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature st fv "iif"
        adjustSignature st fv "("
        eval st predicateAst1
        eval st predicateAst2
        adjustSignature st fv ")"
        fv.NameEndPos <- pos2
        evaluateEquivalence fv
        emitLG000orLG001Diagnostics fv "equivalence"
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        st.EvalPush("IsOperator")
        let fv = st.ValueStack.Peek()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature st fv "is"
        adjustSignature st fv "("

        let operand = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        st.ValueStack.Push(operand)
        eval st isOpArgAst
        st.ValueStack.Pop() |> ignore

        let typeOfoperand = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        st.ValueStack.Push(typeOfoperand)
        eval st variableTypeAst
        st.ValueStack.Pop() |> ignore

        adjustSignature st fv ")"
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (fplDelegateIdentifierAst, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let fv = st.ValueStack.Peek()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        adjustSignature st refBlock "del."
        st.ValueStack.Push(refBlock)
        eval st fplDelegateIdentifierAst
        eval st argumentTupleAst
        emitID013Diagnostics refBlock pos1 pos2 |> ignore
        propagateReference refBlock false 
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.SignatureWithUserDefinedString((pos1, pos2),
                                         ((predicateIdentifierAst, optUserDefinedSymbolAst), paramTupleAst)) ->
        st.EvalPush("SignatureWithUserDefinedString")
        eval st predicateIdentifierAst
        optUserDefinedSymbolAst
        |> Option.map (eval st)
        |> Option.defaultValue ()
        |> ignore
        eval st paramTupleAst
        let fv = st.ValueStack.Peek()
        emitSIG00Diagnostics fv pos1 pos2
        st.EvalPop()
    | Ast.PropertyBlock((pos1, pos2), (keywordPropertyAst, definitionPropertyAst)) ->
        st.EvalPush("PropertyBlock")
        eval st keywordPropertyAst
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.MandatoryPredicate, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st definitionPropertyAst
        tryAddBlock fplValue 
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (ast1, asts)) ->
        let parentEvalPath = st.EvalPath()
        st.EvalPush("ReferencingIdentifier")
        eval st ast1
        asts |> List.map (eval st) |> ignore
        if parentEvalPath.EndsWith(".ReferenceToProof") then
            emitPR002Diagnostics pos1 pos2 // avoid referencing to proofs in general considering it not best practice in mathematics
        st.EvalPop()
    | Ast.ConditionFollowedByResult((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("ConditionFollowedByResult")
        eval st ast1
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Localization((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("Localization")
        eval st ast1
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        eval st functionalTermSignatureAst
        eval st functionalTermInstanceBlockAst
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (variableListInOptDomainListAst, predicateAst)) ->
        st.EvalPush("All")
        let fv = st.ValueStack.Peek()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature st fv "all"
        variableListInOptDomainListAst
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore
        eval st predicateAst
        emitLG000orLG001Diagnostics fv "all quantor"
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (variableListInOptDomainListAst, predicateAst)) ->
        st.EvalPush("Exists")
        let fv = st.ValueStack.Peek()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature st fv "ex"
        variableListInOptDomainListAst
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore
        eval st predicateAst
        emitLG000orLG001Diagnostics fv "exists quantor"
        st.EvalPop()
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, (variableAst, inOptDomainAst)), predicateAst)) ->
        st.EvalPush("ExistsN")
        let fv = st.ValueStack.Peek()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature st fv "exn"
        eval st dollarDigitsAst
        eval st variableAst
        inOptDomainAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st predicateAst
        emitLG000orLG001Diagnostics fv "exists n times quantor"
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature((pos1, pos2), ((optAst, signatureWithUserDefinedStringAst), mappingAst)) -> 
        st.EvalPush("FunctionalTermSignature")
        eval st signatureWithUserDefinedStringAst
        let fv = st.ValueStack.Peek()
        match optAst with
        | Some ast1 -> 
            eval st ast1
            if FplValue.IsFplBlock(fv) then
                fv.BlockType <- FplValueType.FunctionalTerm
            else
                fv.BlockType <- FplValueType.OptionalFunctionalTerm
        | None -> 
                fv.BlockType <- FplValueType.MandatoryFunctionalTerm
        adjustSignature st fv "->"
        fv.NameEndPos <- pos2
        eval st mappingAst
        fv.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        st.EvalPop()
    | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, qualificationListAst) ->
        st.EvalPush("PredicateWithQualification")
        eval st predicateWithOptSpecificationAst
        eval st qualificationListAst
        st.EvalPop()
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), separatedPredicateListAst) ->
        st.EvalPush("InfixOperation")
        let fv = st.ValueStack.Peek()
        let dictOfOperators = Dictionary<string,FplValue>()
        separatedPredicateListAst
        |> List.map (fun (_, optSeparatorAst) -> 
            let infixOperator = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,fv)
            st.ValueStack.Push(infixOperator)
            optSeparatorAst |> Option.map (eval st) |> Option.defaultValue ()
            st.ValueStack.Pop() |> ignore
            dictOfOperators.Add(infixOperator.Name, infixOperator)
        )
        |> ignore

        let sortedSeparatedPredicateListAst = 
            separatedPredicateListAst 
            |> List.sortBy (fun (_,opOpt) -> 
                if opOpt.IsSome then 
                    match opOpt.Value with
                    | Ast.InfixOperator ((p1,p2),symbol) -> 
                        if dictOfOperators.ContainsKey(symbol) then 
                            dictOfOperators[symbol].AuxiliaryInfo
                        else
                            Int32.MaxValue
                    | _ -> Int32.MaxValue
                else
                    Int32.MaxValue
            )

        /// Transforms a precedence-sorted list infix-operations into a nested binary infix operations in reversed Polish notation
        let rec createReversedPolishNotation (sortedSeparatedPredicateList:(Ast * Ast option) list) (fv:FplValue) =
            match sortedSeparatedPredicateList with
            | (predicateAst,optOp) :: xs -> 
                if optOp.IsSome then
                    match optOp.Value with 
                    | Ast.InfixOperator ((p1,p2),symbol) ->
                        // add any found candidate FPL blocks matching this symbol to the newly created nested infix operation
                        dictOfOperators[symbol].Scope 
                        |> Seq.iter (fun kv -> fv.Scope.Add(kv.Key,kv.Value))
                        if dictOfOperators.ContainsKey(symbol) then 
                            adjustSignature st fv symbol
                            adjustSignature st fv "("
                            eval st predicateAst 
                            if xs.Length > 0 then
                                let nextInfixOperation = FplValue.CreateFplValue((p1,p2),FplValueType.Reference,fv)
                                st.ValueStack.Push(nextInfixOperation)
                                createReversedPolishNotation xs nextInfixOperation
                                st.ValueStack.Pop() |> ignore                                
                            adjustSignature st fv ")"
                            propagateReference fv true
                    | _ -> ()
                else
                    eval st predicateAst 
                    propagateReference fv true
            | [] -> ()

        createReversedPolishNotation sortedSeparatedPredicateListAst fv
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((prefixOpAst, predicateAst), postfixOpAst), optionalSpecificationAst), qualificationListAst)) ->
        st.EvalPush("Expression")
        let fv = st.ValueStack.Peek()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        st.ValueStack.Push(refBlock)
        let ensureReversedPolishNotation = 
            if prefixOpAst.IsSome && postfixOpAst.IsSome then 
                // for heuristic reasons, we choose a precedence of postfix ...
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue () 
                adjustSignature st refBlock "("
                // ... over prefix notation in mathematics
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                adjustSignature st refBlock "("
                eval st predicateAst
                adjustSignature st refBlock ")"
                adjustSignature st refBlock ")"
            elif prefixOpAst.IsSome then 
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                adjustSignature st refBlock "("
                eval st predicateAst
                adjustSignature st refBlock ")"
            elif postfixOpAst.IsSome then 
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                adjustSignature st refBlock "("
                eval st predicateAst
                adjustSignature st refBlock ")"
            else
                eval st predicateAst
        ensureReversedPolishNotation
        optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st qualificationListAst
        propagateReference refBlock true
        match (fv.BlockType, fv.FplRepresentation,refBlock.FplRepresentation,fv.ValueList.Count) with
        | (FplValueType.Reference, FplRepresentation.Undef, FplRepresentation.Pointer var, 1) ->
            fv.FplRepresentation <- refBlock.FplRepresentation
        | _ -> ()
        refBlock.NameEndPos <- pos2
        refBlock.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        if FplValue.IsFplBlock(fv) || FplValue.IsConstructorOrProperty(fv) then
            fv.ValueList.Add(refBlock)
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (asts, ast1)) ->
        st.EvalPush("Cases")
        asts |> List.map (eval st) |> ignore
        eval st ast1
        st.EvalPop()
    // | Assignment of Positions * (Ast * Ast)
    | Ast.Signature((pos1, pos2), (predicateIdentifierAst, paramTupleAst)) ->
        st.EvalPush("Signature")
        eval st predicateIdentifierAst
        eval st paramTupleAst
        let fv = st.ValueStack.Peek()
        fv.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        st.EvalPop()
    | Ast.Assignment((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Assignment")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), ((optAst, signatureAst), predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        eval st signatureAst
        let fv = st.ValueStack.Peek()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        match optAst with
        | Some ast1 -> 
            eval st ast1
            fv.BlockType <- FplValueType.OptionalPredicate
        | None -> 
            fv.BlockType <- FplValueType.MandatoryPredicate
        eval st predInstanceBlockAst
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("ParentConstructorCall")
        let fv = st.ValueStack.Peek()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        st.ValueStack.Push(refBlock)
        refBlock.Name <- "bas."
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.JustifiedArgument((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("JustifiedArgument")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.Argument((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Argument")
        eval st ast1
        eval st ast2
        st.EvalPop()
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((ast1, ast2), asts)) ->
        st.EvalPush("ForIn")
        eval st ast1
        eval st ast2
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.PremiseConclusionBlock((pos1, pos2), ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        st.EvalPush("PremiseConclusionBlock")
        optVarDeclOrSpecList |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        eval st premiseAst
        eval st conclusionAst
        st.EvalPop()
    // | Theorem of Positions * (Ast * (Ast list option * Ast))
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Theorem")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Theorem, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        tryAddBlock fplValue 
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Lemma, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        tryAddBlock fplValue 
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proposition, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        tryAddBlock fplValue 
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Conjecture, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        tryAddBlock fplValue 
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Axiom, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        tryAddBlock fplValue 
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        st.EvalPush("CorollarySignature")
        eval st referencingIdentifierAst
        eval st paramTupleAst
        let fv = st.ValueStack.Peek()
        fv.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        st.EvalPop()
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Corollary")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Corollary, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st corollarySignatureAst
        tryAddBlock fplValue 
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let fplValue = st.ValueStack.Peek()
        fplValue.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            eval st varAst // here, each new variable is put on the ValueStack
            eval st varDeclModifierAst
            eval st variableTypeAst
            // after evaluating the created var, remove it from ValueStack
            st.ValueStack.Pop() |> ignore 
        ) |> ignore 
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (signatureAst, (optVarDeclOrSpecListAst, keywordSelfAst))) ->
        st.EvalPush("Constructor")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Constructor, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st signatureAst
        tryAddBlock fplValue
        fplValue.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        match optVarDeclOrSpecListAst with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st keywordSelfAst
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent(optAsts, ast1) ->
        st.EvalPush("DefPredicateContent")
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval st ast1
        st.EvalPop()
    | Ast.DefFunctionContent(optAsts, ast1) ->
        st.EvalPush("DefFunctionContent")
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval st ast1
        st.EvalPop()
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent(optAsts, asts) ->
        st.EvalPush("DefClassCompleteContent")
        optAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate((pos1, pos2), (signatureWithUserDefinedStringAst, (predicateContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionPredicate")
        let fplTheory = st.ValueStack.Peek()
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Predicate, fplTheory)
        st.ValueStack.Push(fplValue)
        eval st signatureWithUserDefinedStringAst
        tryAddBlock fplValue 
        fplValue.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        eval st predicateContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.FunctionalTerm, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st functionalTermSignatureAst
        tryAddBlock fplValue 
        eval st funcContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Class, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st predicateIdentifierAst
        tryAddBlock fplValue 
        fplValue.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
        classTypeListAsts |> List.map (eval st) |> ignore
        eval st classContentAst
        optPropertyListAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast1 -> 
        st.EvalPush("DefinitionClass")
        eval st ast1
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proof, st.ValueStack.Peek())
        st.ValueStack.Push(fplValue)
        eval st referencingIdentifierAst
        tryAddBlock fplValue 
        fplValue.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
        proofArgumentListAst |> List.map (eval st) |> ignore
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        st.ValueStack.Pop() |> ignore
        st.EvalPop()
    | Ast.Precedence((pos1, pos2), precedence) ->
        st.EvalPush("Precedence")
        let fv = st.ValueStack.Peek()
        fv.AuxiliaryInfo <- precedence
        st.EvalPop()
    | ast1 ->
        let astType = ast1.GetType().Name
        emitID000Diagnostics astType


let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evaluateSymbolTable (st: SymbolTable) =
    st.OrderAsts()

    let mutable found = true

    while found do
        let usesClausesEvaluatedParsedAst =
            tryFindParsedAstUsesClausesEvaluated st.ParsedAsts

        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            st.ValueStack.Clear()
            // evaluate the ParsedAst of a theory
            let theoryValue = FplValue.CreateFplValue((Position("",0,1,1), Position("",0,1,1)), FplValueType.Theory, st.Root)
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            else
                st.Root.Scope[pa.Id].Reset()
                st.Root.Scope[pa.Id] <- theoryValue
            theoryValue.Name <- pa.Id
            theoryValue.NameIsFinal <- SignatureIsFinal.Yes (st.EvalPath())
            st.ValueStack.Push(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            if st.ValueStack.Count <> 1 then 
                raise (NotSupportedException($"The valuestack was out of sync after evaluation ({st.ValueStack.Count} elements)."))
            st.ValueStack.Pop() |> ignore
        | None -> found <- false
