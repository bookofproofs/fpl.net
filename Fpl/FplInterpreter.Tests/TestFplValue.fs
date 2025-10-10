namespace FplInterpreter.Tests
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValue() =

    [<DataRow("inference TestId {pre: true con: true};", "TestId", "pred")>]
    [<DataRow("axiom TestId {true};", "TestId", "pred")>]
    [<DataRow("postulate TestId {true};", "TestId", "pred")>]
    [<DataRow("theorem TestId {true};", "TestId", "pred")>]
    [<DataRow("lemma TestId {true};", "TestId", "pred")>]
    [<DataRow("proposition TestId {true};", "TestId", "pred")>]
    [<DataRow("conjecture TestId {true};", "TestId", "pred")>]
    [<DataRow("corollary TestId$1 {true};", "TestId$1", "pred")>]
    [<DataRow("corollary TestId$1$2 {true};", "TestId$1$2", "pred")>]
    [<DataRow("corollary TestId$1$2$3 {true};", "TestId$1$2$3", "pred")>]

    [<DataRow("proof TestId$1 {1. |- trivial} ;", "TestId$1", "pred")>]
    [<DataRow("proof TestId$1$2 {1. |- trivial} ;", "TestId$1$2", "pred")>]
    [<DataRow("proof TestId$1$2$3 {1. |- trivial} ;", "TestId$1$2$3", "pred")>]

    
    [<DataRow("def object Test:Obj {intr} proof Test$1 {1. |- trivial};", "Test", "Test")>]
    [<DataRow("def object TestId:Obj {intrinsic} ;", "TestId", "TestId")>]
    [<DataRow("def object TestId:Nat1, Nat2, Nat3, Nat4 {intrinsic} ;", "TestId", "TestId")>]
    [<DataRow("def object TestId:Obj, Nat3 {intrinsic} ;", "TestId", "TestId")>]

    [<DataRow("def pred TestId() {true};", "TestId()", "pred()")>]
    [<DataRow("def pred TestId(x:ind) {true};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def pred TestId(x:pred) {true};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def pred TestId(x:func) {true};", "TestId(func)", "pred(func)")>]
    [<DataRow("def pred TestId(x:Obj) {true};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def pred TestId(x:index) {true};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def pred TestId(x:predicate) {true};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def pred TestId(x:function) {true};", "TestId(func)", "pred(func)")>]
    [<DataRow("def pred TestId(x:Object) {true};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def pred TestId(x:Nat) {true};", "TestId(Nat)", "pred(Nat)")>]
    [<DataRow("def pred TestId(x:@Nat) {true};", "TestId(@Nat)", "pred(@Nat)")>]
    [<DataRow("def pred TestId(x:tpl) {true};", "TestId(tpl)", "pred(tpl)")>]
    [<DataRow("def pred TestId(x:template) {true};", "TestId(template)", "pred(template)")>]
    [<DataRow("def pred TestId(x:tplTest) {true};", "TestId(tplTest)", "pred(tplTest)")>]
    [<DataRow("def pred TestId(x:templateTest) {true};", "TestId(templateTest)", "pred(templateTest)")>]
    [<DataRow("def pred TestId(x,y,z:Obj) {true};", "TestId(obj, obj, obj)", "pred(obj, obj, obj)")>]
    [<DataRow("def pred TestId(x,y:pred(z:Obj)) {true};", "TestId(pred(obj), pred(obj))", "pred(pred(obj), pred(obj))")>]
    [<DataRow("def pred TestId(x,y:pred(u,v,w:Obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "pred(pred(obj, obj, obj), pred(obj, obj, obj))")>]
    [<DataRow("def pred TestId(x:func(u:Obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "pred(func(obj) -> Nat)")>]

    [<DataRow("def pred TestId(x:*ind) {true};", "TestId(*ind)", "pred(*ind)")>]
    [<DataRow("def pred TestId(x:+pred) {true};", "TestId(+pred)", "pred(+pred)")>]
    [<DataRow("def pred TestId(x:*func) {true};", "TestId(*func)", "pred(*func)")>]
    [<DataRow("def pred TestId(x:+Obj) {true};", "TestId(+Obj)", "pred(+Obj)")>]
    [<DataRow("def pred TestId(x:+index) {true};", "TestId(+ind)", "pred(+ind)")>]
    [<DataRow("def pred TestId(x:*predicate) {true};", "TestId(*pred)", "pred(*pred)")>]
    [<DataRow("def pred TestId(x:+function) {true};", "TestId(+func)", "pred(+func)")>]
    [<DataRow("def pred TestId(x:*object) {true};", "TestId(*Obj)", "pred(*Obj)")>]
    [<DataRow("def pred TestId(x:+Nat) {true};", "TestId(+Nat)", "pred(+Nat)")>]
    [<DataRow("def pred TestId(x:*@Nat) {true};", "TestId(*@Nat)", "pred(*@Nat)")>]
    [<DataRow("def pred TestId(x:*tpl) {true};", "TestId(*tpl)", "pred(*tpl)")>]
    [<DataRow("def pred TestId(x:+template) {true};", "TestId(+template)", "pred(+template)")>]
    [<DataRow("def pred TestId(x:*tplTest) {true};", "TestId(*tplTest)", "pred(*tplTest)")>]
    [<DataRow("def pred TestId(x:+templateTest) {true};", "TestId(+templateTest)", "pred(+templateTest)")>]
    [<DataRow("def pred TestId(x,y,z:+Obj) {true};", "TestId(+Obj, +Obj, +Obj)", "pred(+Obj, +Obj, +Obj)")>]
    [<DataRow("def pred TestId(x,y:+pred(z:Obj)) {true};", "TestId(+pred(obj), +pred(obj))", "pred(+pred(obj), +pred(obj))")>]
    [<DataRow("def pred TestId(x,y:pred(u,v,w:*Obj)) {true};", "TestId(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj))", "pred(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj))")>]
    [<DataRow("def pred TestId(x:func(u:+Obj)->Nat) {true};", "TestId(func(+Obj) -> Nat)", "pred(func(+Obj) -> Nat)")>]
 
    [<DataRow("def func TestId() -> obj {intrinsic};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def func TestId(x:ind) -> obj {intrinsic};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def func TestId(x:pred) -> obj {intrinsic};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def func TestId(x:func) -> obj {intrinsic};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def func TestId(x:Obj) -> obj {intrinsic};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def func TestId(x:index) -> obj {intrinsic};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def func TestId(x:predicate) -> obj {intrinsic};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def func TestId(x:function) -> obj {intrinsic};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def func TestId(x:Object) -> obj {intrinsic};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def func TestId(x:Nat) -> obj {intrinsic};", "TestId(Nat) -> obj", "func(Nat) -> obj")>]
    [<DataRow("def func TestId(x:@Nat) -> obj {intrinsic};", "TestId(@Nat) -> obj", "func(@Nat) -> obj")>]
    [<DataRow("def func TestId(x:tpl) -> obj {intrinsic};", "TestId(tpl) -> obj", "func(tpl) -> obj")>]
    [<DataRow("def func TestId(x:template) -> obj {intrinsic};", "TestId(template) -> obj", "func(template) -> obj")>]
    [<DataRow("def func TestId(x:tplTest) -> obj {intrinsic};", "TestId(tplTest) -> obj", "func(tplTest) -> obj")>]
    [<DataRow("def func TestId(x:templateTest) -> obj {intrinsic};", "TestId(templateTest) -> obj", "func(templateTest) -> obj")>]
    [<DataRow("def func TestId(x,y,z:Obj) -> obj {intrinsic};", "TestId(obj, obj, obj) -> obj", "func(obj, obj, obj) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(z:Obj)) -> obj {intrinsic};", "TestId(pred(obj), pred(obj)) -> obj", "func(pred(obj), pred(obj)) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(u,v,w:Obj)) -> obj {intrinsic};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "func(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj")>]
    [<DataRow("def func TestId(x:func(u:Obj)->Nat) -> obj {intrinsic};", "TestId(func(obj) -> Nat) -> obj", "func(func(obj) -> Nat) -> obj")>]
 
    [<DataRow("def func TestId(x:*ind) -> obj {intrinsic};", "TestId(*ind) -> obj", "func(*ind) -> obj")>]
    [<DataRow("def func TestId(x:+pred) -> obj {intrinsic};", "TestId(+pred) -> obj", "func(+pred) -> obj")>]
    [<DataRow("def func TestId(x:*func) -> obj {intrinsic};", "TestId(*func) -> obj", "func(*func) -> obj")>]
    [<DataRow("def func TestId(x:+Obj) -> obj {intrinsic};", "TestId(+Obj) -> obj", "func(+Obj) -> obj")>]
    [<DataRow("def func TestId(x:+index) -> obj {intrinsic};", "TestId(+ind) -> obj", "func(+ind) -> obj")>]
    [<DataRow("def func TestId(x:*predicate) -> obj {intrinsic};", "TestId(*pred) -> obj", "func(*pred) -> obj")>]
    [<DataRow("def func TestId(x:+function) -> obj {intrinsic};", "TestId(+func) -> obj", "func(+func) -> obj")>]
    [<DataRow("def func TestId(x:*object) -> obj {intrinsic};", "TestId(*Obj) -> obj", "func(*Obj) -> obj")>]
    [<DataRow("def func TestId(x:+Nat) -> obj {intrinsic};", "TestId(+Nat) -> obj", "func(+Nat) -> obj")>]
    [<DataRow("def func TestId(x:*@Nat) -> obj {intrinsic};", "TestId(*@Nat) -> obj", "func(*@Nat) -> obj")>]
    [<DataRow("def func TestId(x:*tpl) -> obj {intrinsic};", "TestId(*tpl) -> obj", "func(*tpl) -> obj")>]
    [<DataRow("def func TestId(x:+template) -> obj {intrinsic};", "TestId(+template) -> obj", "func(+template) -> obj")>]
    [<DataRow("def func TestId(x:*tplTest) -> obj {intrinsic};", "TestId(*tplTest) -> obj", "func(*tplTest) -> obj")>]
    [<DataRow("def func TestId(x:+templateTest) -> obj {intrinsic};", "TestId(+templateTest) -> obj", "func(+templateTest) -> obj")>]
    [<DataRow("def func TestId(x,y,z:+Obj) -> obj {intrinsic};", "TestId(+Obj, +Obj, +Obj) -> obj", "func(+Obj, +Obj, +Obj) -> obj")>]
    [<DataRow("def func TestId(x,y:+pred(z:Obj)) -> obj {intrinsic};", "TestId(+pred(obj), +pred(obj)) -> obj", "func(+pred(obj), +pred(obj)) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(u,v,w:*Obj)) -> obj {intrinsic};", "TestId(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj)) -> obj", "func(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj)) -> obj")>]
    [<DataRow("def func TestId(x:func(u:+Obj)->Nat) -> obj {intrinsic};", "TestId(func(+Obj) -> Nat) -> obj", "func(func(+Obj) -> Nat) -> obj")>]
 
    [<DataRow("def func TestId() -> ind {intrinsic};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def func TestId() -> pred {intrinsic};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def func TestId() -> func {intrinsic};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def func TestId() -> obj {intrinsic};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def func TestId() -> index {intrinsic};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def func TestId() -> predicate {intrinsic};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def func TestId() -> function {intrinsic};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def func TestId() -> object {intrinsic};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def func TestId() -> Nat {intrinsic};", "TestId() -> Nat", "func() -> Nat")>]
    [<DataRow("def func TestId() -> @Nat {intrinsic};", "TestId() -> @Nat", "func() -> @Nat")>]
    [<DataRow("def func TestId() -> tpl {intrinsic};", "TestId() -> tpl", "func() -> tpl")>]
    [<DataRow("def func TestId() -> template {intrinsic};", "TestId() -> template", "func() -> template")>]
    [<DataRow("def func TestId() -> tplTest {intrinsic};", "TestId() -> tplTest", "func() -> tplTest")>]
    [<DataRow("def func TestId() -> templateTest {intrinsic};", "TestId() -> templateTest", "func() -> templateTest")>]
    [<DataRow("def func TestId() -> pred(x,y,z:Obj) {intrinsic};", "TestId() -> pred(obj, obj, obj)", "func() -> pred(obj, obj, obj)")>]
    [<DataRow("def func TestId() -> pred(x,y:pred(u,v,w:Obj)) {intrinsic};", "TestId() -> pred(pred(obj, obj, obj), pred(obj, obj, obj))", "func() -> pred(pred(obj, obj, obj), pred(obj, obj, obj))")>]

    [<DataRow("def func TestId() -> pred(x:func(u:Obj)->Nat) {intrinsic};", "TestId() -> pred(func(obj) -> Nat)", "func() -> pred(func(obj) -> Nat)")>]

 
    [<TestMethod>]
    member this.TestTypeSignatureOfFplBlocks(fplCode:string, expectedName:string, expectedType:string) =
        let filename = "TestTypeSignatureOfFplBlocks"
        let result = prepareFplCode(filename + ".fpl", fplCode, false) 
        let block = result.Value.Root.Scope[filename]
        let fplValue = block.Scope[expectedName]
        let actualTypeSignature = fplValue.Type(SignatureType.Type)
        Assert.AreEqual<string>(expectedType, actualTypeSignature)
        match box fplValue with
        | :? IHasSignature as withSignature ->
            let expectedStart =
                if fplCode.StartsWith("def ") then 
                    (int64)4
                else
                    (int64)0            
            Assert.AreEqual<int64>(expectedStart, withSignature.SignStartPos.Index)
            let expectedEnd = 
                if fplCode.Contains("def cl") then
                    (int64)(fplCode.IndexOf(":", System.StringComparison.OrdinalIgnoreCase))
                else
                    (int64)(fplCode.IndexOf(" {", System.StringComparison.OrdinalIgnoreCase))
            Assert.AreEqual<int64>(expectedEnd, withSignature.SignEndPos.Index)
        | _ -> failwith($"{expectedName} does not implement IHasSignature")


        prepareFplCode(filename, "", true) |> ignore

    [<DataRow("""loc not(x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;;""", "not(undef)")>]
    [<DataRow("""loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y;;""","Equal(undef, undef)")>]

    [<TestMethod>]
    member this.TestTypeSignatureOfFplLocalizations(fplCode:string, expectedName:string) =
        let filename = "TestTypeSignatureOfFplBlocks"
        let result = prepareFplCode(filename + ".fpl", fplCode, false) 
        let block = result.Value.Root.Scope[filename]
        let fplValue = block.Scope[expectedName]
        let actualTypeSignature = fplValue.Type(SignatureType.Type)
        Assert.AreEqual<string>(expectedName, actualTypeSignature)
        prepareFplCode(filename, "", true) |> ignore

    [<DataRow("def obj T:Obj {intr prty pred TestId() {intrinsic}};", "TestId()", "pred()")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:ind) {intrinsic}};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:pred) {intrinsic}};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:func) {intrinsic}};", "TestId(func)", "pred(func)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:Obj) {intrinsic}};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:index) {intrinsic}};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:predicate) {intrinsic}};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:function) {intrinsic}};", "TestId(func)", "pred(func)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:Object) {intrinsic}};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:Nat) {intrinsic}};", "TestId(Nat)", "pred(Nat)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:@Nat) {intrinsic}};", "TestId(@Nat)", "pred(@Nat)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:tpl) {intrinsic}};", "TestId(tpl)", "pred(tpl)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:template) {intrinsic}};", "TestId(template)", "pred(template)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:tplTest) {intrinsic}};", "TestId(tplTest)", "pred(tplTest)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:templateTest) {intrinsic}};", "TestId(templateTest)", "pred(templateTest)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x,y,z:Obj) {intrinsic}};", "TestId(obj, obj, obj)", "pred(obj, obj, obj)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x,y:pred(z:Obj)) {intrinsic}};", "TestId(pred(obj), pred(obj))", "pred(pred(obj), pred(obj))")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x,y:pred(u,v,w:Obj)) {intrinsic}};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "pred(pred(obj, obj, obj), pred(obj, obj, obj))")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:func(u:Obj)->Nat) {intrinsic}};", "TestId(func(obj) -> Nat)", "pred(func(obj) -> Nat)")>]

    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*ind) {intrinsic}};", "TestId(*ind)", "pred(*ind)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+pred) {intrinsic}};", "TestId(+pred)", "pred(+pred)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*func) {intrinsic}};", "TestId(*func)", "pred(*func)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+Obj) {intrinsic}};", "TestId(+Obj)", "pred(+Obj)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+index) {intrinsic}};", "TestId(+ind)", "pred(+ind)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*predicate) {intrinsic}};", "TestId(*pred)", "pred(*pred)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+function) {intrinsic}};", "TestId(+func)", "pred(+func)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*object) {intrinsic}};", "TestId(*Obj)", "pred(*Obj)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+Nat) {intrinsic}};", "TestId(+Nat)", "pred(+Nat)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*@Nat) {intrinsic}};", "TestId(*@Nat)", "pred(*@Nat)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*tpl) {intrinsic}};", "TestId(*tpl)", "pred(*tpl)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+template) {intrinsic}};", "TestId(+template)", "pred(+template)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:*tplTest) {intrinsic}};", "TestId(*tplTest)", "pred(*tplTest)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:+templateTest) {intrinsic}};", "TestId(+templateTest)", "pred(+templateTest)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x,y,z:+Obj) {intrinsic}};", "TestId(+Obj, +Obj, +Obj)", "pred(+Obj, +Obj, +Obj)")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x,y:+pred(z:Obj)) {intrinsic}};", "TestId(+pred(obj), +pred(obj))", "pred(+pred(obj), +pred(obj))")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x,y:pred(u,v,w:*Obj)) {intrinsic}};", "TestId(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj))", "pred(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj))")>]
    [<DataRow("def obj T:Obj {intr prty pred TestId(x:func(u:+Obj)->Nat) {intrinsic}};", "TestId(func(+Obj) -> Nat)", "pred(func(+Obj) -> Nat)")>]
 
    [<DataRow("def obj T:Obj {intr prty func TestId() -> obj {intrinsic}};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:ind) -> obj {intrinsic}};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:pred) -> obj {intrinsic}};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:func) -> obj {intrinsic}};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:Obj) -> obj {intrinsic}};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:index) -> obj {intrinsic}};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:predicate) -> obj {intrinsic}};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:function) -> obj {intrinsic}};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:Object) -> obj {intrinsic}};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:Nat) -> obj {intrinsic}};", "TestId(Nat) -> obj", "func(Nat) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:@Nat) -> obj {intrinsic}};", "TestId(@Nat) -> obj", "func(@Nat) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:tpl) -> obj {intrinsic}};", "TestId(tpl) -> obj", "func(tpl) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:template) -> obj {intrinsic}};", "TestId(template) -> obj", "func(template) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:tplTest) -> obj {intrinsic}};", "TestId(tplTest) -> obj", "func(tplTest) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:templateTest) -> obj {intrinsic}};", "TestId(templateTest) -> obj", "func(templateTest) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x,y,z:Obj) -> obj {intrinsic}};", "TestId(obj, obj, obj) -> obj", "func(obj, obj, obj) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x,y:pred(z:Obj)) -> obj {intrinsic}};", "TestId(pred(obj), pred(obj)) -> obj", "func(pred(obj), pred(obj)) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x,y:pred(u,v,w:Obj)) -> obj {intrinsic}};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "func(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:func(u:Obj)->Nat) -> obj {intrinsic}};", "TestId(func(obj) -> Nat) -> obj", "func(func(obj) -> Nat) -> obj")>]
 
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*ind) -> obj {intrinsic}};", "TestId(*ind) -> obj", "func(*ind) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+pred) -> obj {intrinsic}};", "TestId(+pred) -> obj", "func(+pred) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*func) -> obj {intrinsic}};", "TestId(*func) -> obj", "func(*func) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+Obj) -> obj {intrinsic}};", "TestId(+Obj) -> obj", "func(+Obj) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+index) -> obj {intrinsic}};", "TestId(+ind) -> obj", "func(+ind) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*predicate) -> obj {intrinsic}};", "TestId(*pred) -> obj", "func(*pred) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+function) -> obj {intrinsic}};", "TestId(+func) -> obj", "func(+func) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*object) -> obj {intrinsic}};", "TestId(*Obj) -> obj", "func(*Obj) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+Nat) -> obj {intrinsic}};", "TestId(+Nat) -> obj", "func(+Nat) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*@Nat) -> obj {intrinsic}};", "TestId(*@Nat) -> obj", "func(*@Nat) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*tpl) -> obj {intrinsic}};", "TestId(*tpl) -> obj", "func(*tpl) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+template) -> obj {intrinsic}};", "TestId(+template) -> obj", "func(+template) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:*tplTest) -> obj {intrinsic}};", "TestId(*tplTest) -> obj", "func(*tplTest) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:+templateTest) -> obj {intrinsic}};", "TestId(+templateTest) -> obj", "func(+templateTest) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x,y,z:+Obj) -> obj {intrinsic}};", "TestId(+Obj, +Obj, +Obj) -> obj", "func(+Obj, +Obj, +Obj) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x,y:+pred(z:Obj)) -> obj {intrinsic}};", "TestId(+pred(obj), +pred(obj)) -> obj", "func(+pred(obj), +pred(obj)) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x,y:pred(u,v,w:*Obj)) -> obj {intrinsic}};", "TestId(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj)) -> obj", "func(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj)) -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId(x:func(u:+Obj)->Nat) -> obj {intrinsic}};", "TestId(func(+Obj) -> Nat) -> obj", "func(func(+Obj) -> Nat) -> obj")>]
 
    [<DataRow("def obj T:Obj {intr prty func TestId() -> ind {intrinsic}};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> pred {intrinsic}};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> func {intrinsic}};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> obj {intrinsic}};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> index {intrinsic}};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> predicate {intrinsic}};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> function {intrinsic}};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> object {intrinsic}};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> Nat {intrinsic}};", "TestId() -> Nat", "func() -> Nat")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> @Nat {intrinsic}};", "TestId() -> @Nat", "func() -> @Nat")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> tpl {intrinsic}};", "TestId() -> tpl", "func() -> tpl")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> template {intrinsic}};", "TestId() -> template", "func() -> template")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> tplTest {intrinsic}};", "TestId() -> tplTest", "func() -> tplTest")>]
    [<DataRow("def obj T:Obj {intr prty func TestId() -> templateTest {intrinsic}};", "TestId() -> templateTest", "func() -> templateTest")>]

    [<DataRow("def obj T:Obj {intr prty func TestId() -> pred(x:func(u:Obj)->Nat) {intrinsic}};", "TestId() -> pred(func(obj) -> Nat)", "func() -> pred(func(obj) -> Nat)")>]


    [<TestMethod>]
    member this.TestTypeSignatureOfFplProperties(fplCode:string, expectedName:string, expectedType:string) =
        let filename = "TestTypeSignatureOfFplProperties"
        let result = prepareFplCode(filename + ".fpl", fplCode, false) 
        let block = result.Value.Root.Scope[filename].Scope["T"]
        let fplValue = block.Scope[expectedName]
        let actualTypeSignature = fplValue.Type(SignatureType.Type)
        Assert.AreEqual<string>(expectedType, actualTypeSignature)
        prepareFplCode(filename, "", true) |> ignore

    [<DataRow("def obj T:Obj {ctor T() {}};", "T()", "T()")>]
    [<DataRow("def obj T:Obj {ctor T(x:ind) {}};", "T(ind)", "T(ind)")>]
    [<DataRow("def obj T:Obj {ctor T(x:pred) {}};", "T(pred)", "T(pred)")>]
    [<DataRow("def obj T:Obj {ctor T(x:func) {}};", "T(func)", "T(func)")>]
    [<DataRow("def obj T:Obj {ctor T(x:Obj) {}};", "T(obj)", "T(obj)")>]
    [<DataRow("def obj T:Obj {ctor T(x:index) {}};", "T(ind)", "T(ind)")>]
    [<DataRow("def obj T:Obj {ctor T(x:predicate) {}};", "T(pred)", "T(pred)")>]
    [<DataRow("def obj T:Obj {ctor T(x:function) {}};", "T(func)", "T(func)")>]
    [<DataRow("def obj T:Obj {ctor T(x:Object) {}};", "T(obj)", "T(obj)")>]
    [<DataRow("def obj T:Obj {ctor T(x:Nat) {}};", "T(Nat)", "T(Nat)")>]
    [<DataRow("def obj T:Obj {ctor T(x:@Nat) {}};", "T(@Nat)", "T(@Nat)")>]
    [<DataRow("def obj T:Obj {ctor T(x:tpl) {}};", "T(tpl)", "T(tpl)")>]
    [<DataRow("def obj T:Obj {ctor T(x:template) {}};", "T(template)", "T(template)")>]
    [<DataRow("def obj T:Obj {ctor T(x:tplTest) {}};", "T(tplTest)", "T(tplTest)")>]
    [<DataRow("def obj T:Obj {ctor T(x:templateTest) {}};", "T(templateTest)", "T(templateTest)")>]
    [<DataRow("def obj T:Obj {ctor T(x,y,z:Obj) {}};", "T(obj, obj, obj)", "T(obj, obj, obj)")>]
    [<DataRow("def obj T:Obj {ctor T(x,y:pred(z:Obj)) {}};", "T(pred(obj), pred(obj))", "T(pred(obj), pred(obj))")>]
    [<DataRow("def obj T:Obj {ctor T(x,y:pred(u,v,w:Obj)) {}};", "T(pred(obj, obj, obj), pred(obj, obj, obj))", "T(pred(obj, obj, obj), pred(obj, obj, obj))")>]
    [<DataRow("def obj T:Obj {ctor T(x:func(u:Obj)->Nat) {}};", "T(func(obj) -> Nat)", "T(func(obj) -> Nat)")>]
 
    [<DataRow("def obj T:Obj {ctor T(x:*ind) {}};", "T(*ind)", "T(*ind)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+pred) {}};", "T(+pred)", "T(+pred)")>]
    [<DataRow("def obj T:Obj {ctor T(x:*func) {}};", "T(*func)", "T(*func)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+Obj) {}};", "T(+Obj)", "T(+Obj)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+index) {}};", "T(+ind)", "T(+ind)")>]
    [<DataRow("def obj T:Obj {ctor T(x:*predicate) {}};", "T(*pred)", "T(*pred)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+function) {}};", "T(+func)", "T(+func)")>]
    [<DataRow("def obj T:Obj {ctor T(x:*object) {}};", "T(*Obj)", "T(*Obj)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+Nat) {}};", "T(+Nat)", "T(+Nat)")>]
    [<DataRow("def obj T:Obj {ctor T(x:*@Nat) {}};", "T(*@Nat)", "T(*@Nat)")>]
    [<DataRow("def obj T:Obj {ctor T(x:*tpl) {}};", "T(*tpl)", "T(*tpl)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+template) {}};", "T(+template)", "T(+template)")>]
    [<DataRow("def obj T:Obj {ctor T(x:*tplTest) {}};", "T(*tplTest)", "T(*tplTest)")>]
    [<DataRow("def obj T:Obj {ctor T(x:+templateTest) {}};", "T(+templateTest)", "T(+templateTest)")>]
    [<DataRow("def obj T:Obj {ctor T(x,y,z:+Obj) {}};", "T(+Obj, +Obj, +Obj)", "T(+Obj, +Obj, +Obj)")>]
    [<DataRow("def obj T:Obj {ctor T(x,y:+pred(z:Obj)) {}};", "T(+pred(obj), +pred(obj))", "T(+pred(obj), +pred(obj))")>]
    [<DataRow("def obj T:Obj {ctor T(x,y:pred(u,v,w:*Obj)) {}};", "T(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj))", "T(pred(*Obj, *Obj, *Obj), pred(*Obj, *Obj, *Obj))")>]
    [<DataRow("def obj T:Obj {ctor T(x:func(u:+Obj)->Nat) {}};", "T(func(+Obj) -> Nat)", "T(func(+Obj) -> Nat)")>]

    [<TestMethod>]
    member this.TestTypeSignatureOfConstructors(fplCode:string, expectedName:string, expectedType:string) =
        let filename = "TestTypeSignatureOfConstructors"
        let result = prepareFplCode(filename + ".fpl", fplCode, false) 
        let cl = result.Value.Root.Scope[filename].Scope["T"]
        let fplValue = cl.Scope[expectedName]
        let actualTypeSignature = fplValue.Type(SignatureType.Type)
        match box fplValue with
        | :? IHasSignature as withSignature ->
            Assert.AreEqual<int64>((int64)14, withSignature.SignStartPos.Index)
            let expectedEnd = (int64)(fplCode.IndexOf(" {}", System.StringComparison.OrdinalIgnoreCase))
            Assert.AreEqual<int64>(expectedEnd, withSignature.SignEndPos.Index)
        | _ -> failwith($"{expectedName} does not implement IHasSignature")
        Assert.AreEqual<string>(expectedType, actualTypeSignature)
        prepareFplCode(filename, "", true) |> ignore