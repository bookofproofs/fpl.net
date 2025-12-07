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

    
    [<DataRow("def class Test:Obj proof Test$1 {1. |- trivial};", "Test", "Test")>]
    [<DataRow("def class TestId:Obj;", "TestId", "TestId")>]
    [<DataRow("def class TestId:Nat1, Nat2, Nat3, Nat4 {intrinsic} ;", "TestId", "TestId")>]
    [<DataRow("def class TestId:Nat3 {intrinsic} ;", "TestId", "TestId")>]

    [<DataRow("def pred TestId() {true};", "TestId()", "pred()")>]
    [<DataRow("def pred TestId(x:ind) {true};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def pred TestId(x:pred) {true};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def pred TestId(x:func) {true};", "TestId(func)", "pred(func)")>]
    [<DataRow("def pred TestId(x:obj) {true};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def pred TestId(x:index) {true};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def pred TestId(x:predicate) {true};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def pred TestId(x:function) {true};", "TestId(func)", "pred(func)")>]
    [<DataRow("def pred TestId(x:object) {true};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def pred TestId(x:Nat) {true};", "TestId(Nat)", "pred(Nat)")>]
    [<DataRow("def pred TestId(x:tpl) {true};", "TestId(tpl)", "pred(tpl)")>]
    [<DataRow("def pred TestId(x:template) {true};", "TestId(template)", "pred(template)")>]
    [<DataRow("def pred TestId(x:tplTest) {true};", "TestId(tplTest)", "pred(tplTest)")>]
    [<DataRow("def pred TestId(x:templateTest) {true};", "TestId(templateTest)", "pred(templateTest)")>]
    [<DataRow("def pred TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "pred(obj, obj, obj)")>]
    [<DataRow("def pred TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "pred(pred(obj), pred(obj))")>]
    [<DataRow("def pred TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "pred(pred(obj, obj, obj), pred(obj, obj, obj))")>]
    [<DataRow("def pred TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "pred(func(obj) -> Nat)")>]

    [<DataRow("def pred TestId(x:*ind[ind]) {true};", "TestId(*ind[ind])", "pred(*ind[ind])")>]
    [<DataRow("def pred TestId(x:*pred[ind]) {true};", "TestId(*pred[ind])", "pred(*pred[ind])")>]
    [<DataRow("def pred TestId(x:*func[ind]) {true};", "TestId(*func[ind])", "pred(*func[ind])")>]
    [<DataRow("def pred TestId(x:*obj[ind]) {true};", "TestId(*obj[ind])", "pred(*obj[ind])")>]
    [<DataRow("def pred TestId(x:*index[ind]) {true};", "TestId(*ind[ind])", "pred(*ind[ind])")>]
    [<DataRow("def pred TestId(x:*predicate[ind]) {true};", "TestId(*pred[ind])", "pred(*pred[ind])")>]
    [<DataRow("def pred TestId(x:*function[ind]) {true};", "TestId(*func[ind])", "pred(*func[ind])")>]
    [<DataRow("def pred TestId(x:*object[ind]) {true};", "TestId(*obj[ind])", "pred(*obj[ind])")>]
    [<DataRow("def pred TestId(x:*Nat[ind]) {true};", "TestId(*Nat[ind])", "pred(*Nat[ind])")>]
    [<DataRow("def pred TestId(x:*tpl[ind]) {true};", "TestId(*tpl[ind])", "pred(*tpl[ind])")>]
    [<DataRow("def pred TestId(x:*template[ind]) {true};", "TestId(*template[ind])", "pred(*template[ind])")>]
    [<DataRow("def pred TestId(x:*tplTest[ind]) {true};", "TestId(*tplTest[ind])", "pred(*tplTest[ind])")>]
    [<DataRow("def pred TestId(x:*templateTest[ind]) {true};", "TestId(*templateTest[ind])", "pred(*templateTest[ind])")>]
    [<DataRow("def pred TestId(x,y,z:*obj[ind]) {true};", "TestId(*obj[ind], *obj[ind], *obj[ind])", "pred(*obj[ind], *obj[ind], *obj[ind])")>]
    [<DataRow("def pred TestId(x,y:*pred(z:obj)[ind]) {true};", "TestId(*pred(obj)[ind], *pred(obj)[ind])", "pred(*pred(obj)[ind], *pred(obj)[ind])")>]
    [<DataRow("def pred TestId(x,y:pred(u,v,w:*obj[ind])) {true};", "TestId(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind]))", "pred(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind]))")>]
    [<DataRow("def pred TestId(x:func(u:*obj[ind])->Nat) {true};", "TestId(func(*obj[ind]) -> Nat)", "pred(func(*obj[ind]) -> Nat)")>]
 
    [<DataRow("def func TestId() -> obj {intrinsic};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def func TestId(x:ind) -> obj {intrinsic};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def func TestId(x:pred) -> obj {intrinsic};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def func TestId(x:func) -> obj {intrinsic};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def func TestId(x:obj) -> obj {intrinsic};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def func TestId(x:index) -> obj {intrinsic};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def func TestId(x:predicate) -> obj {intrinsic};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def func TestId(x:function) -> obj {intrinsic};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def func TestId(x:object) -> obj {intrinsic};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def func TestId(x:Nat) -> obj {intrinsic};", "TestId(Nat) -> obj", "func(Nat) -> obj")>]
    [<DataRow("def func TestId(x:tpl) -> obj {intrinsic};", "TestId(tpl) -> obj", "func(tpl) -> obj")>]
    [<DataRow("def func TestId(x:template) -> obj {intrinsic};", "TestId(template) -> obj", "func(template) -> obj")>]
    [<DataRow("def func TestId(x:tplTest) -> obj {intrinsic};", "TestId(tplTest) -> obj", "func(tplTest) -> obj")>]
    [<DataRow("def func TestId(x:templateTest) -> obj {intrinsic};", "TestId(templateTest) -> obj", "func(templateTest) -> obj")>]
    [<DataRow("def func TestId(x,y,z:obj) -> obj {intrinsic};", "TestId(obj, obj, obj) -> obj", "func(obj, obj, obj) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(z:obj)) -> obj {intrinsic};", "TestId(pred(obj), pred(obj)) -> obj", "func(pred(obj), pred(obj)) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(u,v,w:obj)) -> obj {intrinsic};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "func(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj")>]
    [<DataRow("def func TestId(x:func(u:obj)->Nat) -> obj {intrinsic};", "TestId(func(obj) -> Nat) -> obj", "func(func(obj) -> Nat) -> obj")>]
 
    [<DataRow("def func TestId(x:*ind[ind]) -> obj {intrinsic};", "TestId(*ind[ind]) -> obj", "func(*ind[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*pred[ind]) -> obj {intrinsic};", "TestId(*pred[ind]) -> obj", "func(*pred[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*func[ind]) -> obj {intrinsic};", "TestId(*func[ind]) -> obj", "func(*func[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*obj[ind]) -> obj {intrinsic};", "TestId(*obj[ind]) -> obj", "func(*obj[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*index[ind]) -> obj {intrinsic};", "TestId(*ind[ind]) -> obj", "func(*ind[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*predicate[ind]) -> obj {intrinsic};", "TestId(*pred[ind]) -> obj", "func(*pred[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*function[ind]) -> obj {intrinsic};", "TestId(*func[ind]) -> obj", "func(*func[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*object[ind]) -> obj {intrinsic};", "TestId(*obj[ind]) -> obj", "func(*obj[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*Nat[ind]) -> obj {intrinsic};", "TestId(*Nat[ind]) -> obj", "func(*Nat[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*Nat[obj]) -> obj {intrinsic};", "TestId(*Nat[obj]) -> obj", "func(*Nat[obj]) -> obj")>]
    [<DataRow("def func TestId(x:*tpl[ind]) -> obj {intrinsic};", "TestId(*tpl[ind]) -> obj", "func(*tpl[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*template[ind]) -> obj {intrinsic};", "TestId(*template[ind]) -> obj", "func(*template[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*tplTest[ind]) -> obj {intrinsic};", "TestId(*tplTest[ind]) -> obj", "func(*tplTest[ind]) -> obj")>]
    [<DataRow("def func TestId(x:*templateTest[ind]) -> obj {intrinsic};", "TestId(*templateTest[ind]) -> obj", "func(*templateTest[ind]) -> obj")>]
    [<DataRow("def func TestId(x,y,z:*obj[ind]) -> obj {intrinsic};", "TestId(*obj[ind], *obj[ind], *obj[ind]) -> obj", "func(*obj[ind], *obj[ind], *obj[ind]) -> obj")>]
    [<DataRow("def func TestId(x,y:*pred(z:obj)[ind]) -> obj {intrinsic};", "TestId(*pred(obj)[ind], *pred(obj)[ind]) -> obj", "func(*pred(obj)[ind], *pred(obj)[ind]) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(u,v,w:*obj[ind])) -> obj {intrinsic};", "TestId(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind])) -> obj", "func(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind])) -> obj")>]
    [<DataRow("def func TestId(x:func(u:*obj[ind])->Nat) -> obj {intrinsic};", "TestId(func(*obj[ind]) -> Nat) -> obj", "func(func(*obj[ind]) -> Nat) -> obj")>]
 
    [<DataRow("def func TestId() -> ind {intrinsic};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def func TestId() -> pred {intrinsic};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def func TestId() -> func {intrinsic};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def func TestId() -> obj {intrinsic};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def func TestId() -> index {intrinsic};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def func TestId() -> predicate {intrinsic};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def func TestId() -> function {intrinsic};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def func TestId() -> object {intrinsic};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def func TestId() -> Nat {intrinsic};", "TestId() -> Nat", "func() -> Nat")>]
    [<DataRow("def func TestId() -> *Nat[ind] {intrinsic};", "TestId() -> *Nat[ind]", "func() -> *Nat[ind]")>]
    [<DataRow("def func TestId() -> tpl {intrinsic};", "TestId() -> tpl", "func() -> tpl")>]
    [<DataRow("def func TestId() -> template {intrinsic};", "TestId() -> template", "func() -> template")>]
    [<DataRow("def func TestId() -> tplTest {intrinsic};", "TestId() -> tplTest", "func() -> tplTest")>]
    [<DataRow("def func TestId() -> templateTest {intrinsic};", "TestId() -> templateTest", "func() -> templateTest")>]
    [<DataRow("def func TestId() -> pred(x,y,z:obj) {intrinsic};", "TestId() -> pred(obj, obj, obj)", "func() -> pred(obj, obj, obj)")>]
    [<DataRow("def func TestId() -> pred(x,y:pred(u,v,w:obj)) {intrinsic};", "TestId() -> pred(pred(obj, obj, obj), pred(obj, obj, obj))", "func() -> pred(pred(obj, obj, obj), pred(obj, obj, obj))")>]

    [<DataRow("def func TestId() -> pred(x:func(u:obj)->Nat) {intrinsic};", "TestId() -> pred(func(obj) -> Nat)", "func() -> pred(func(obj) -> Nat)")>]

 
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

    [<DataRow("def cl T {intr prty pred TestId() {intrinsic}};", "TestId()", "pred()")>]
    [<DataRow("def cl T {intr prty pred TestId(x:ind) {intrinsic}};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:pred) {intrinsic}};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:func) {intrinsic}};", "TestId(func)", "pred(func)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:obj) {intrinsic}};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:index) {intrinsic}};", "TestId(ind)", "pred(ind)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:predicate) {intrinsic}};", "TestId(pred)", "pred(pred)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:function) {intrinsic}};", "TestId(func)", "pred(func)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:object) {intrinsic}};", "TestId(obj)", "pred(obj)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:Nat) {intrinsic}};", "TestId(Nat)", "pred(Nat)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:tpl) {intrinsic}};", "TestId(tpl)", "pred(tpl)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:template) {intrinsic}};", "TestId(template)", "pred(template)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:tplTest) {intrinsic}};", "TestId(tplTest)", "pred(tplTest)")>]
    [<DataRow("def cl T {intr prty pred TestId(x:templateTest) {intrinsic}};", "TestId(templateTest)", "pred(templateTest)")>]
    [<DataRow("def cl T {intr prty pred TestId(x,y,z:obj) {intrinsic}};", "TestId(obj, obj, obj)", "pred(obj, obj, obj)")>]
    [<DataRow("def cl T {intr prty pred TestId(x,y:pred(z:obj)) {intrinsic}};", "TestId(pred(obj), pred(obj))", "pred(pred(obj), pred(obj))")>]
    [<DataRow("def cl T {intr prty pred TestId(x,y:pred(u,v,w:obj)) {intrinsic}};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "pred(pred(obj, obj, obj), pred(obj, obj, obj))")>]
    [<DataRow("def cl T {intr prty pred TestId(x:func(u:obj)->Nat) {intrinsic}};", "TestId(func(obj) -> Nat)", "pred(func(obj) -> Nat)")>]

    [<DataRow("def cl T {intr prty pred TestId(x:*ind[ind]) {intrinsic}};", "TestId(*ind[ind])", "pred(*ind[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*pred[ind]) {intrinsic}};", "TestId(*pred[ind])", "pred(*pred[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*func[ind]) {intrinsic}};", "TestId(*func[ind])", "pred(*func[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*obj[ind]) {intrinsic}};", "TestId(*obj[ind])", "pred(*obj[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*index[ind]) {intrinsic}};", "TestId(*ind[ind])", "pred(*ind[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*predicate[ind]) {intrinsic}};", "TestId(*pred[ind])", "pred(*pred[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*function[ind]) {intrinsic}};", "TestId(*func[ind])", "pred(*func[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*object[ind]) {intrinsic}};", "TestId(*obj[ind])", "pred(*obj[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*Nat[ind]) {intrinsic}};", "TestId(*Nat[ind])", "pred(*Nat[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*Nat[ind]) {intrinsic}};", "TestId(*Nat[ind])", "pred(*Nat[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*tpl[ind]) {intrinsic}};", "TestId(*tpl[ind])", "pred(*tpl[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*template[ind]) {intrinsic}};", "TestId(*template[ind])", "pred(*template[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*tplTest[ind]) {intrinsic}};", "TestId(*tplTest[ind])", "pred(*tplTest[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x:*templateTest[ind]) {intrinsic}};", "TestId(*templateTest[ind])", "pred(*templateTest[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x,y,z:*obj[ind]) {intrinsic}};", "TestId(*obj[ind], *obj[ind], *obj[ind])", "pred(*obj[ind], *obj[ind], *obj[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x,y:*pred(z:obj)[ind]) {intrinsic}};", "TestId(*pred(obj)[ind], *pred(obj)[ind])", "pred(*pred(obj)[ind], *pred(obj)[ind])")>]
    [<DataRow("def cl T {intr prty pred TestId(x,y:pred(u,v,w:*obj[ind])) {intrinsic}};", "TestId(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind]))", "pred(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind]))")>]
    [<DataRow("def cl T {intr prty pred TestId(x:func(u:*obj[ind])->Nat) {intrinsic}};", "TestId(func(*obj[ind]) -> Nat)", "pred(func(*obj[ind]) -> Nat)")>]
 
    [<DataRow("def cl T {intr prty func TestId() -> obj {intrinsic}};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:ind) -> obj {intrinsic}};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:pred) -> obj {intrinsic}};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:func) -> obj {intrinsic}};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:obj) -> obj {intrinsic}};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:index) -> obj {intrinsic}};", "TestId(ind) -> obj", "func(ind) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:predicate) -> obj {intrinsic}};", "TestId(pred) -> obj", "func(pred) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:function) -> obj {intrinsic}};", "TestId(func) -> obj", "func(func) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:object) -> obj {intrinsic}};", "TestId(obj) -> obj", "func(obj) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:Nat) -> obj {intrinsic}};", "TestId(Nat) -> obj", "func(Nat) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:tpl) -> obj {intrinsic}};", "TestId(tpl) -> obj", "func(tpl) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:template) -> obj {intrinsic}};", "TestId(template) -> obj", "func(template) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:tplTest) -> obj {intrinsic}};", "TestId(tplTest) -> obj", "func(tplTest) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:templateTest) -> obj {intrinsic}};", "TestId(templateTest) -> obj", "func(templateTest) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x,y,z:obj) -> obj {intrinsic}};", "TestId(obj, obj, obj) -> obj", "func(obj, obj, obj) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x,y:pred(z:obj)) -> obj {intrinsic}};", "TestId(pred(obj), pred(obj)) -> obj", "func(pred(obj), pred(obj)) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x,y:pred(u,v,w:obj)) -> obj {intrinsic}};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "func(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:func(u:obj)->Nat) -> obj {intrinsic}};", "TestId(func(obj) -> Nat) -> obj", "func(func(obj) -> Nat) -> obj")>]
 
    [<DataRow("def cl T {intr prty func TestId(x:*ind[ind]) -> obj {intrinsic}};", "TestId(*ind[ind]) -> obj", "func(*ind[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*pred[ind]) -> obj {intrinsic}};", "TestId(*pred[ind]) -> obj", "func(*pred[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*func[ind]) -> obj {intrinsic}};", "TestId(*func[ind]) -> obj", "func(*func[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*obj[ind]) -> obj {intrinsic}};", "TestId(*obj[ind]) -> obj", "func(*obj[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*index[ind]) -> obj {intrinsic}};", "TestId(*ind[ind]) -> obj", "func(*ind[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*predicate[ind]) -> obj {intrinsic}};", "TestId(*pred[ind]) -> obj", "func(*pred[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*function[ind]) -> obj {intrinsic}};", "TestId(*func[ind]) -> obj", "func(*func[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*object[ind]) -> obj {intrinsic}};", "TestId(*obj[ind]) -> obj", "func(*obj[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*Nat[ind]) -> obj {intrinsic}};", "TestId(*Nat[ind]) -> obj", "func(*Nat[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*Nat[obj]) -> obj {intrinsic}};", "TestId(*Nat[obj]) -> obj", "func(*Nat[obj]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*tpl[ind]) -> obj {intrinsic}};", "TestId(*tpl[ind]) -> obj", "func(*tpl[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*template[ind]) -> obj {intrinsic}};", "TestId(*template[ind]) -> obj", "func(*template[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*tplTest[ind]) -> obj {intrinsic}};", "TestId(*tplTest[ind]) -> obj", "func(*tplTest[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:*templateTest[ind]) -> obj {intrinsic}};", "TestId(*templateTest[ind]) -> obj", "func(*templateTest[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x,y,z:*obj[ind]) -> obj {intrinsic}};", "TestId(*obj[ind], *obj[ind], *obj[ind]) -> obj", "func(*obj[ind], *obj[ind], *obj[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x,y:*pred(z:obj)[ind]) -> obj {intrinsic}};", "TestId(*pred(obj)[ind], *pred(obj)[ind]) -> obj", "func(*pred(obj)[ind], *pred(obj)[ind]) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x,y:pred(u,v,w:*obj[ind])) -> obj {intrinsic}};", "TestId(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind])) -> obj", "func(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind])) -> obj")>]
    [<DataRow("def cl T {intr prty func TestId(x:func(u:*obj[ind])->Nat) -> obj {intrinsic}};", "TestId(func(*obj[ind]) -> Nat) -> obj", "func(func(*obj[ind]) -> Nat) -> obj")>]
 
    [<DataRow("def cl T {intr prty func TestId() -> ind {intrinsic}};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def cl T {intr prty func TestId() -> pred {intrinsic}};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def cl T {intr prty func TestId() -> func {intrinsic}};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def cl T {intr prty func TestId() -> obj {intrinsic}};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def cl T {intr prty func TestId() -> index {intrinsic}};", "TestId() -> ind", "func() -> ind")>]
    [<DataRow("def cl T {intr prty func TestId() -> predicate {intrinsic}};", "TestId() -> pred", "func() -> pred")>]
    [<DataRow("def cl T {intr prty func TestId() -> function {intrinsic}};", "TestId() -> func", "func() -> func")>]
    [<DataRow("def cl T {intr prty func TestId() -> object {intrinsic}};", "TestId() -> obj", "func() -> obj")>]
    [<DataRow("def cl T {intr prty func TestId() -> Nat {intrinsic}};", "TestId() -> Nat", "func() -> Nat")>]
    [<DataRow("def cl T {intr prty func TestId() -> *Nat[ind] {intrinsic}};", "TestId() -> *Nat", "func() -> *Nat")>]
    [<DataRow("def cl T {intr prty func TestId() -> tpl {intrinsic}};", "TestId() -> tpl", "func() -> tpl")>]
    [<DataRow("def cl T {intr prty func TestId() -> template {intrinsic}};", "TestId() -> template", "func() -> template")>]
    [<DataRow("def cl T {intr prty func TestId() -> tplTest {intrinsic}};", "TestId() -> tplTest", "func() -> tplTest")>]
    [<DataRow("def cl T {intr prty func TestId() -> templateTest {intrinsic}};", "TestId() -> templateTest", "func() -> templateTest")>]

    [<DataRow("def cl T {intr prty func TestId() -> pred(x:func(u:obj)->Nat) {intrinsic}};", "TestId() -> pred(func(obj) -> Nat)", "func() -> pred(func(obj) -> Nat)")>]


    [<TestMethod>]
    member this.TestTypeSignatureOfFplProperties(fplCode:string, expectedName:string, expectedType:string) =
        let filename = "TestTypeSignatureOfFplProperties"
        let result = prepareFplCode(filename + ".fpl", fplCode, false) 
        let block = result.Value.Root.Scope[filename].Scope["T"]
        let fplValue = block.Scope[expectedName]
        let actualTypeSignature = fplValue.Type(SignatureType.Type)
        Assert.AreEqual<string>(expectedType, actualTypeSignature)
        prepareFplCode(filename, "", true) |> ignore

    [<DataRow("def cl T {ctor T() {}};", "T()", "T")>]
    [<DataRow("def cl T {ctor T(x:ind) {}};", "T(ind)", "T")>]
    [<DataRow("def cl T {ctor T(x:pred) {}};", "T(pred)", "T")>]
    [<DataRow("def cl T {ctor T(x:func) {}};", "T(func)", "T")>]
    [<DataRow("def cl T {ctor T(x:obj) {}};", "T(obj)", "T")>]
    [<DataRow("def cl T {ctor T(x:index) {}};", "T(ind)", "T")>]
    [<DataRow("def cl T {ctor T(x:predicate) {}};", "T(pred)", "T")>]
    [<DataRow("def cl T {ctor T(x:function) {}};", "T(func)", "T")>]
    [<DataRow("def cl T {ctor T(x:object) {}};", "T(obj)", "T")>]
    [<DataRow("def cl T {ctor T(x:Nat) {}};", "T(Nat)", "T")>]
    [<DataRow("def cl T {ctor T(x:tpl) {}};", "T(tpl)", "T")>]
    [<DataRow("def cl T {ctor T(x:template) {}};", "T(template)", "T")>]
    [<DataRow("def cl T {ctor T(x:tplTest) {}};", "T(tplTest)", "T")>]
    [<DataRow("def cl T {ctor T(x:templateTest) {}};", "T(templateTest)", "T")>]
    [<DataRow("def cl T {ctor T(x,y,z:obj) {}};", "T(obj, obj, obj)", "T")>]
    [<DataRow("def cl T {ctor T(x,y:pred(z:obj)) {}};", "T(pred(obj), pred(obj))", "T")>]
    [<DataRow("def cl T {ctor T(x,y:pred(u,v,w:obj)) {}};", "T(pred(obj, obj, obj), pred(obj, obj, obj))", "T")>]
    [<DataRow("def cl T {ctor T(x:func(u:obj)->Nat) {}};", "T(func(obj) -> Nat)", "T")>]
 
    [<DataRow("def cl T {ctor T(x:*ind[ind]) {}};", "T(*ind[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*pred[ind]) {}};", "T(*pred[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*func[ind]) {}};", "T(*func[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*obj[ind]) {}};", "T(*obj[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*index[ind]) {}};", "T(*ind[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*predicate[ind]) {}};", "T(*pred[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*function[ind]) {}};", "T(*func[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*object[ind]) {}};", "T(*obj[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*Nat[ind]) {}};", "T(*Nat[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*Nat[ind]) {}};", "T(*Nat[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*tpl[ind]) {}};", "T(*tpl[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*template[ind]) {}};", "T(*template[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*tplTest[ind]) {}};", "T(*tplTest[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x:*templateTest[ind]) {}};", "T(*templateTest[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x,y,z:*obj[ind]) {}};", "T(*obj[ind], *obj[ind], *obj[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x,y:*pred(z:obj)[ind]) {}};", "T(*pred(obj)[ind], *pred(obj)[ind])", "T")>]
    [<DataRow("def cl T {ctor T(x,y:pred(u,v,w:*obj[ind])) {}};", "T(pred(*obj[ind], *obj[ind], *obj[ind]), pred(*obj[ind], *obj[ind], *obj[ind]))", "T")>]
    [<DataRow("def cl T {ctor T(x:func(u:*obj[ind])->Nat) {}};", "T(func(*obj[ind]) -> Nat)", "T")>]

    [<TestMethod>]
    member this.TestTypeSignatureOfConstructors(fplCode:string, expectedName:string, expectedType:string) =
        let filename = "TestTypeSignatureOfConstructors"
        let result = prepareFplCode(filename + ".fpl", fplCode, false) 
        let cl = result.Value.Root.Scope[filename].Scope["T"]
        let fplValue = cl.Scope[expectedName]
        let actualTypeSignature = fplValue.Type(SignatureType.Type)
        match box fplValue with
        | :? IHasSignature as withSignature ->
            Assert.AreEqual<int64>((int64)10, withSignature.SignStartPos.Index)
            let expectedEnd = (int64)(fplCode.IndexOf(" {}", System.StringComparison.OrdinalIgnoreCase))
            Assert.AreEqual<int64>(expectedEnd, withSignature.SignEndPos.Index)
        | _ -> failwith($"{expectedName} does not implement IHasSignature")
        Assert.AreEqual<string>(expectedType, actualTypeSignature)
        prepareFplCode(filename, "", true) |> ignore