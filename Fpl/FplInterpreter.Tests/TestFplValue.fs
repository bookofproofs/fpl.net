namespace FplInterpreter.Tests
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValue() =



    [<TestMethod>]
    member this.TestInitialFactory() =
        FplParser.parserDiagnostics.Clear()
        let r = FplValue.CreateRoot()
        Assert.AreEqual (FplBlockType.Root, r.BlockType)
        let testFactory fplBlockType fplType =
            let fv = FplValue.CreateFplValue((Position("",0,1,1),Position("",0,1,1)), fplBlockType, r)
            Assert.AreEqual (fplBlockType, fv.BlockType)
            Assert.AreEqual ("", fv.Name)
            Assert.AreEqual ("", fv.FplRepresentation)
            Assert.AreEqual ([], fv.TypeSignature)
            Assert.AreEqual (fplType, fv.EvaluationType)
        testFactory FplBlockType.VariadicVariable FplType.Object
        testFactory FplBlockType.Axiom FplType.Predicate
        testFactory FplBlockType.Theorem FplType.Predicate
        testFactory FplBlockType.Lemma FplType.Predicate
        testFactory FplBlockType.Proposition FplType.Predicate
        testFactory FplBlockType.Corollary FplType.Predicate
        testFactory FplBlockType.Conjecture FplType.Predicate
        testFactory FplBlockType.Premise FplType.Predicate
        testFactory FplBlockType.Conclusion FplType.Predicate
        testFactory FplBlockType.Proof FplType.Predicate
        testFactory FplBlockType.RuleOfInference FplType.Predicate
        testFactory FplBlockType.Expression FplType.Predicate
        testFactory FplBlockType.Theory FplType.Predicate
        testFactory FplBlockType.Predicate FplType.Predicate 
        testFactory FplBlockType.Constructor FplType.Object
        testFactory FplBlockType.FunctionalTerm FplType.Object
        testFactory FplBlockType.Variable FplType.Object
        testFactory FplBlockType.VariadicVariable FplType.Object
        testFactory FplBlockType.MandatoryProperty FplType.Object
        testFactory FplBlockType.OptionalProperty FplType.Object
        testFactory FplBlockType.Class FplType.Object 


    member this.ScopeVariablesInSignature() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:pred(u,v,w:func(a,b,c:obj))) 
            {true}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate(pred(func(obj, obj, obj), func(obj, obj, obj), func(obj, obj, obj)), pred(func(obj, obj, obj), func(obj, obj, obj), func(obj, obj, obj)))"
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let block = theory.Scope[name]
                            let x = block.Scope["x"]
                            let y = block.Scope["y"]
                            let xw = x.Scope["w"]
                            let xu = x.Scope["u"]
                            let xv = x.Scope["v"]
                            let yw = y.Scope["w"]
                            let yu = y.Scope["u"]
                            let yv = y.Scope["v"]
                            let xwa = xw.Scope["a"]
                            let xwb = xw.Scope["b"]
                            let xwc = xw.Scope["c"]
                            let xua = xu.Scope["a"]
                            let xub = xu.Scope["b"]
                            let xuc = xu.Scope["c"]
                            let xva = xv.Scope["a"]
                            let xvb = xv.Scope["b"]
                            let xvc = xv.Scope["c"]
                            let ywa = yw.Scope["a"]
                            let ywb = yw.Scope["b"]
                            let ywc = yw.Scope["c"]
                            let yua = yu.Scope["a"]
                            let yub = yu.Scope["b"]
                            let yuc = yu.Scope["c"]
                            let yva = yv.Scope["a"]
                            let yvb = yv.Scope["b"]
                            let yvc = yv.Scope["c"]
                            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    [<TestMethod>]
    member this.TestScopeVariablesInSignatureIsComplete() =
        try
            this.ScopeVariablesInSignature() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestScopeVariablesInSignatureTypeSignature() =
        let result = this.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->

            Assert.AreEqual(["obj"], ywc.TypeSignature)
            Assert.AreEqual(["obj"], ywb.TypeSignature)
            Assert.AreEqual(["obj"], ywa.TypeSignature)
            Assert.AreEqual(["obj"], yvc.TypeSignature)
            Assert.AreEqual(["obj"], yvb.TypeSignature)
            Assert.AreEqual(["obj"], yva.TypeSignature)
            Assert.AreEqual(["obj"], yuc.TypeSignature)
            Assert.AreEqual(["obj"], yub.TypeSignature)
            Assert.AreEqual(["obj"], yua.TypeSignature)
            Assert.AreEqual(["obj"], xwc.TypeSignature)
            Assert.AreEqual(["obj"], xwb.TypeSignature)
            Assert.AreEqual(["obj"], xwa.TypeSignature)
            Assert.AreEqual(["obj"], xvc.TypeSignature)
            Assert.AreEqual(["obj"], xvb.TypeSignature)
            Assert.AreEqual(["obj"], xva.TypeSignature)
            Assert.AreEqual(["obj"], xuc.TypeSignature)
            Assert.AreEqual(["obj"], xub.TypeSignature)
            Assert.AreEqual(["obj"], xua.TypeSignature)

            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], yw.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], yv.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], yu.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], xw.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], xv.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], xu.TypeSignature)
            Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"], y.TypeSignature)
            Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"], x.TypeSignature)
            Assert.AreEqual(["TestPredicate"; "("; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"; ")"], block.TypeSignature)
            Assert.AreEqual([], theory.TypeSignature)
            Assert.AreEqual([], r.TypeSignature)
        | None -> ()


    [<DataRow("inference TestId() {pre: true con: true};", "TestId()", "TestId ( )")>]
    [<DataRow("inference TestId(x:ind) {pre: true con: true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("inference TestId(x:pred) {pre: true con: true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("inference TestId(x:func) {pre: true con: true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("inference TestId(x:obj) {pre: true con: true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("inference TestId(x:index) {pre: true con: true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("inference TestId(x:predicate) {pre: true con: true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("inference TestId(x:function) {pre: true con: true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("inference TestId(x:object) {pre: true con: true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("inference TestId(x:Nat) {pre: true con: true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("inference TestId(x:@Nat) {pre: true con: true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("inference TestId(x:tpl) {pre: true con: true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("inference TestId(x:template) {pre: true con: true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("inference TestId(x:tplTest) {pre: true con: true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("inference TestId(x:templateTest) {pre: true con: true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("inference TestId(x,y,z:obj) {pre: true con: true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("inference TestId(x,y:pred(z:obj)) {pre: true con: true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("inference TestId(x,y:pred(u,v,w:obj)) {pre: true con: true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("inference TestId(x:func(u:obj)) {pre: true con: true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("inference TestId(x:obj[@Nat]) {pre: true con: true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("inference TestId(x:obj[Nat]) {pre: true con: true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("inference TestId(x:obj[Test.Nat]) {pre: true con: true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("inference TestId(x:obj[@Nat]) {pre: true con: true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("inference TestId(x:obj[index]) {pre: true con: true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("inference TestId(x:obj[ind]) {pre: true con: true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("inference TestId(x:obj[tpl]) {pre: true con: true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("inference TestId(x:obj[template]) {pre: true con: true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("inference TestId(x:obj[tplTest]) {pre: true con: true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("inference TestId(x:obj[templateTest]) {pre: true con: true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("inference TestId(x:obj[(Nat,templateTest)]) {pre: true con: true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("inference TestId(x:obj[(index,Nat]]) {pre: true con: true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("inference TestId(x:obj[[obj,@Nat]]) {pre: true con: true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("inference TestId(x:obj[[tpl,index)]) {pre: true con: true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]


    [<DataRow("axiom TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("axiom TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("axiom TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("axiom TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("axiom TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("axiom TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("axiom TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("axiom TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("axiom TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("axiom TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("axiom TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("axiom TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("axiom TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("axiom TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("axiom TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("axiom TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("axiom TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("axiom TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("axiom TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("axiom TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("axiom TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("axiom TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("axiom TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("axiom TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("axiom TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("axiom TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("axiom TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("axiom TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("axiom TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("postulate TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("postulate TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("postulate TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("postulate TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("postulate TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("postulate TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("postulate TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("postulate TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("postulate TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("postulate TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("postulate TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("postulate TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("postulate TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("postulate TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("postulate TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("postulate TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("postulate TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("postulate TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("postulate TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("postulate TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("postulate TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("postulate TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("postulate TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("postulate TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("postulate TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("postulate TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("postulate TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("postulate TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("postulate TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("theorem TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("theorem TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("theorem TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("theorem TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("theorem TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("theorem TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("theorem TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("theorem TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("theorem TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("theorem TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("theorem TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("theorem TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("theorem TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("theorem TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("theorem TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("theorem TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("theorem TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("theorem TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("theorem TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("theorem TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("theorem TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("theorem TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("theorem TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("theorem TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("theorem TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("theorem TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("theorem TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("theorem TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("theorem TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("lemma TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("lemma TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("lemma TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("lemma TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("lemma TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("lemma TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("lemma TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("lemma TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("lemma TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("lemma TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("lemma TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("lemma TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("lemma TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("lemma TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("lemma TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("lemma TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("lemma TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("lemma TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("lemma TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("lemma TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("lemma TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("lemma TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("lemma TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("lemma TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("lemma TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("lemma TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("lemma TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("lemma TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("lemma TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("proposition TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("proposition TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("proposition TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("proposition TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("proposition TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("proposition TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("proposition TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("proposition TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("proposition TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("proposition TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("proposition TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("proposition TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("proposition TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("proposition TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("proposition TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("proposition TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("proposition TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("proposition TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("proposition TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("proposition TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("proposition TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("proposition TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("proposition TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("proposition TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("proposition TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("proposition TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("proposition TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("proposition TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("proposition TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("conjecture TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("conjecture TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("conjecture TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("conjecture TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("conjecture TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("conjecture TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("conjecture TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("conjecture TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("conjecture TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("conjecture TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("conjecture TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("conjecture TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("conjecture TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("conjecture TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("conjecture TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("conjecture TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("conjecture TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("conjecture TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("conjecture TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("conjecture TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("conjecture TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("conjecture TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("conjecture TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("conjecture TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("conjecture TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("conjecture TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("conjecture TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("conjecture TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("conjecture TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("corollary TestId$1() {true};", "TestId$1()", "TestId $1 ( )")>]
    [<DataRow("corollary TestId$1(x:ind) {true};", "TestId$1(ind)", "TestId $1 ( ind )")>]
    [<DataRow("corollary TestId$1(x:pred) {true};", "TestId$1(pred)", "TestId $1 ( pred )")>]
    [<DataRow("corollary TestId$1(x:func) {true};", "TestId$1(func)", "TestId $1 ( func )")>]
    [<DataRow("corollary TestId$1(x:obj) {true};", "TestId$1(obj)", "TestId $1 ( obj )")>]
    [<DataRow("corollary TestId$1(x:index) {true};", "TestId$1(ind)", "TestId $1 ( ind )")>]
    [<DataRow("corollary TestId$1(x:predicate) {true};", "TestId$1(pred)", "TestId $1 ( pred )")>]
    [<DataRow("corollary TestId$1(x:function) {true};", "TestId$1(func)", "TestId $1 ( func )")>]
    [<DataRow("corollary TestId$1(x:object) {true};", "TestId$1(obj)", "TestId $1 ( obj )")>]
    [<DataRow("corollary TestId$1(x:Nat) {true};", "TestId$1(Nat)", "TestId $1 ( Nat )")>]
    [<DataRow("corollary TestId$1(x:@Nat) {true};", "TestId$1(@Nat)", "TestId $1 ( @Nat )")>]
    [<DataRow("corollary TestId$1(x:tpl) {true};", "TestId$1(tpl)", "TestId $1 ( tpl )")>]
    [<DataRow("corollary TestId$1(x:template) {true};", "TestId$1(template)", "TestId $1 ( template )")>]
    [<DataRow("corollary TestId$1(x:tplTest) {true};", "TestId$1(tplTest)", "TestId $1 ( tplTest )")>]
    [<DataRow("corollary TestId$1(x:templateTest) {true};", "TestId$1(templateTest)", "TestId $1 ( templateTest )")>]
    [<DataRow("corollary TestId$1(x,y,z:obj) {true};", "TestId$1(obj, obj, obj)", "TestId $1 ( obj obj obj )")>]
    [<DataRow("corollary TestId$1(x,y:pred(z:obj)) {true};", "TestId$1(pred(obj), pred(obj))", "TestId $1 ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("corollary TestId$1(x,y:pred(u,v,w:obj)) {true};", "TestId$1(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId $1 ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("corollary TestId$1(x:func(u:obj)) {true};", "TestId$1(func(obj))", "TestId $1 ( func ( obj ) )")>]
    [<DataRow("corollary TestId$1(x:obj[@Nat]) {true};", "TestId$1(obj[@Nat])", "TestId $1 ( obj [ @Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[Nat]) {true};", "TestId$1(obj[Nat])", "TestId $1 ( obj [ Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[Test.Nat]) {true};", "TestId$1(obj[Test.Nat])", "TestId $1 ( obj [ Test.Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[index]) {true};", "TestId$1(obj[ind])", "TestId $1 ( obj [ ind ] )")>]
    [<DataRow("corollary TestId$1(x:obj[ind]) {true};", "TestId$1(obj[ind])", "TestId $1 ( obj [ ind ] )")>]
    [<DataRow("corollary TestId$1(x:obj[tpl]) {true};", "TestId$1(obj[tpl])", "TestId $1 ( obj [ tpl ] )")>]
    [<DataRow("corollary TestId$1(x:obj[template]) {true};", "TestId$1(obj[template])", "TestId $1 ( obj [ template ] )")>]
    [<DataRow("corollary TestId$1(x:obj[tplTest]) {true};", "TestId$1(obj[tplTest])", "TestId $1 ( obj [ tplTest ] )")>]
    [<DataRow("corollary TestId$1(x:obj[templateTest]) {true};", "TestId$1(obj[templateTest])", "TestId $1 ( obj [ templateTest ] )")>]
    [<DataRow("corollary TestId$1(x:obj[(Nat,templateTest)]) {true};", "TestId$1(obj[(Nat, templateTest)])", "TestId $1 ( obj [( Nat templateTest )] )")>]
    [<DataRow("corollary TestId$1(x:obj[(index,Nat]]) {true};", "TestId$1(obj[(ind, Nat]])", "TestId $1 ( obj [( ind Nat ]] )")>]
    [<DataRow("corollary TestId$1(x:obj[[obj,@Nat]]) {true};", "TestId$1(obj[[obj, @Nat]])", "TestId $1 ( obj [[ obj @Nat ]] )")>]
    [<DataRow("corollary TestId$1(x:obj[[tpl,index)]) {true};", "TestId$1(obj[[tpl, ind)])", "TestId $1 ( obj [[ tpl ind )] )")>]

    [<DataRow("proof TestId$1 {1. |- trivial} ;", "TestId$1", "TestId $1")>]

    [<DataRow("def class TestId:obj {intrinsic} ;", "TestId", "TestId obj")>]
    [<DataRow("def class TestId:Nat1, :Nat2, :* Nat3, :+ Nat4 {intrinsic} ;", "TestId", "TestId Nat1 Nat2 Many: Nat3 Many1: Nat4")>]
    [<DataRow("def class TestId:obj, :Nat3 {intrinsic} ;", "TestId", "TestId obj Nat3")>]

    [<DataRow("def pred TestId() {true};", "TestId()", "TestId ( )")>]
    [<DataRow("def pred TestId(x:ind) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("def pred TestId(x:pred) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("def pred TestId(x:func) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("def pred TestId(x:obj) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("def pred TestId(x:index) {true};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("def pred TestId(x:predicate) {true};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("def pred TestId(x:function) {true};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("def pred TestId(x:object) {true};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("def pred TestId(x:Nat) {true};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("def pred TestId(x:@Nat) {true};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("def pred TestId(x:tpl) {true};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("def pred TestId(x:template) {true};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("def pred TestId(x:tplTest) {true};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("def pred TestId(x:templateTest) {true};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("def pred TestId(x,y,z:obj) {true};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("def pred TestId(x,y:pred(z:obj)) {true};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("def pred TestId(x,y:pred(u,v,w:obj)) {true};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("def pred TestId(x:func(u:obj)) {true};", "TestId(func(obj))", "TestId ( func ( obj ) )")>]
    [<DataRow("def pred TestId(x:obj[@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("def pred TestId(x:obj[ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("def pred TestId(x:obj[tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("def pred TestId(x:obj[template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("def pred TestId(x:obj[tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("def pred TestId(x:obj[templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("def pred TestId(x:obj[(Nat,templateTest)]) {true};", "TestId(obj[(Nat, templateTest)])", "TestId ( obj [( Nat templateTest )] )")>]
    [<DataRow("def pred TestId(x:obj[(index,Nat]]) {true};", "TestId(obj[(ind, Nat]])", "TestId ( obj [( ind Nat ]] )")>]
    [<DataRow("def pred TestId(x:obj[[obj,@Nat]]) {true};", "TestId(obj[[obj, @Nat]])", "TestId ( obj [[ obj @Nat ]] )")>]
    [<DataRow("def pred TestId(x:obj[[tpl,index)]) {true};", "TestId(obj[[tpl, ind)])", "TestId ( obj [[ tpl ind )] )")>]

    [<DataRow("def func TestId() -> obj {intrinsic};", "TestId() -> obj", "TestId ( ) -> obj")>]
    [<DataRow("def func TestId(x:ind) -> obj {intrinsic};", "TestId(ind) -> obj", "TestId ( ind ) -> obj")>]
    [<DataRow("def func TestId(x:pred) -> obj {intrinsic};", "TestId(pred) -> obj", "TestId ( pred ) -> obj")>]
    [<DataRow("def func TestId(x:func) -> obj {intrinsic};", "TestId(func) -> obj", "TestId ( func ) -> obj")>]
    [<DataRow("def func TestId(x:obj) -> obj {intrinsic};", "TestId(obj) -> obj", "TestId ( obj ) -> obj")>]
    [<DataRow("def func TestId(x:index) -> obj {intrinsic};", "TestId(ind) -> obj", "TestId ( ind ) -> obj")>]
    [<DataRow("def func TestId(x:predicate) -> obj {intrinsic};", "TestId(pred) -> obj", "TestId ( pred ) -> obj")>]
    [<DataRow("def func TestId(x:function) -> obj {intrinsic};", "TestId(func) -> obj", "TestId ( func ) -> obj")>]
    [<DataRow("def func TestId(x:object) -> obj {intrinsic};", "TestId(obj) -> obj", "TestId ( obj ) -> obj")>]
    [<DataRow("def func TestId(x:Nat) -> obj {intrinsic};", "TestId(Nat) -> obj", "TestId ( Nat ) -> obj")>]
    [<DataRow("def func TestId(x:@Nat) -> obj {intrinsic};", "TestId(@Nat) -> obj", "TestId ( @Nat ) -> obj")>]
    [<DataRow("def func TestId(x:tpl) -> obj {intrinsic};", "TestId(tpl) -> obj", "TestId ( tpl ) -> obj")>]
    [<DataRow("def func TestId(x:template) -> obj {intrinsic};", "TestId(template) -> obj", "TestId ( template ) -> obj")>]
    [<DataRow("def func TestId(x:tplTest) -> obj {intrinsic};", "TestId(tplTest) -> obj", "TestId ( tplTest ) -> obj")>]
    [<DataRow("def func TestId(x:templateTest) -> obj {intrinsic};", "TestId(templateTest) -> obj", "TestId ( templateTest ) -> obj")>]
    [<DataRow("def func TestId(x,y,z:obj) -> obj {intrinsic};", "TestId(obj, obj, obj) -> obj", "TestId ( obj obj obj ) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(z:obj)) -> obj {intrinsic};", "TestId(pred(obj), pred(obj)) -> obj", "TestId ( pred ( obj ) pred ( obj ) ) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(u,v,w:obj)) -> obj {intrinsic};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) ) -> obj")>]
    [<DataRow("def func TestId(x:func(u:obj)) -> obj {intrinsic};", "TestId(func(obj)) -> obj", "TestId ( func ( obj ) ) -> obj")>]
    [<DataRow("def func TestId(x:obj[@Nat]) -> obj {intrinsic};", "TestId(obj[@Nat]) -> obj", "TestId ( obj [ @Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[Nat]) -> obj {intrinsic};", "TestId(obj[Nat]) -> obj", "TestId ( obj [ Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[Test.Nat]) -> obj {intrinsic};", "TestId(obj[Test.Nat]) -> obj", "TestId ( obj [ Test.Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[index]) -> obj {intrinsic};", "TestId(obj[ind]) -> obj", "TestId ( obj [ ind ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[ind]) -> obj {intrinsic};", "TestId(obj[ind]) -> obj", "TestId ( obj [ ind ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[tpl]) -> obj {intrinsic};", "TestId(obj[tpl]) -> obj", "TestId ( obj [ tpl ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[template]) -> obj {intrinsic};", "TestId(obj[template]) -> obj", "TestId ( obj [ template ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[tplTest]) -> obj {intrinsic};", "TestId(obj[tplTest]) -> obj", "TestId ( obj [ tplTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[templateTest]) -> obj {intrinsic};", "TestId(obj[templateTest]) -> obj", "TestId ( obj [ templateTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[(Nat,templateTest)]) -> obj {intrinsic};", "TestId(obj[(Nat, templateTest)]) -> obj", "TestId ( obj [( Nat templateTest )] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[(index,Nat]]) -> obj {intrinsic};", "TestId(obj[(ind, Nat]]) -> obj", "TestId ( obj [( ind Nat ]] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[[obj,@Nat]]) -> obj {intrinsic};", "TestId(obj[[obj, @Nat]]) -> obj", "TestId ( obj [[ obj @Nat ]] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[[tpl,index)]) -> obj {intrinsic};", "TestId(obj[[tpl, ind)]) -> obj", "TestId ( obj [[ tpl ind )] ) -> obj")>]
    [<TestMethod>]
    member this.TestTypeSignatureOfFplBlocks(fplCode:string, expectedName:string, expectedTypeSignatureStr:string) =
        let expectedTypeSignature = expectedTypeSignatureStr.Split(' ') |> List.ofArray
        let result = prepareFplCode(fplCode, false) 
        let actual = result.Value.Root.Scope["Test"].Scope[expectedName].TypeSignature
        Assert.AreEqual(expectedTypeSignature, actual)
        prepareFplCode("", true) |> ignore