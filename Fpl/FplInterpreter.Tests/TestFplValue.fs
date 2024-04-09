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


    [<DataRow("axiom SomeAxiom() {true};", "SomeAxiom()", "SomeAxiom ( )")>]
    [<DataRow("axiom SomeAxiom(x:ind) {true};", "SomeAxiom(ind)", "SomeAxiom ( ind )")>]
    [<DataRow("axiom SomeAxiom(x:pred) {true};", "SomeAxiom(pred)", "SomeAxiom ( pred )")>]
    [<DataRow("axiom SomeAxiom(x:func) {true};", "SomeAxiom(func)", "SomeAxiom ( func )")>]
    [<DataRow("axiom SomeAxiom(x:obj) {true};", "SomeAxiom(obj)", "SomeAxiom ( obj )")>]
    [<DataRow("axiom SomeAxiom(x:index) {true};", "SomeAxiom(ind)", "SomeAxiom ( ind )")>]
    [<DataRow("axiom SomeAxiom(x:predicate) {true};", "SomeAxiom(pred)", "SomeAxiom ( pred )")>]
    [<DataRow("axiom SomeAxiom(x:function) {true};", "SomeAxiom(func)", "SomeAxiom ( func )")>]
    [<DataRow("axiom SomeAxiom(x:object) {true};", "SomeAxiom(obj)", "SomeAxiom ( obj )")>]
    [<DataRow("axiom SomeAxiom(x:Nat) {true};", "SomeAxiom(Nat)", "SomeAxiom ( Nat )")>]
    [<DataRow("axiom SomeAxiom(x:@Nat) {true};", "SomeAxiom(@Nat)", "SomeAxiom ( @Nat )")>]
    [<DataRow("axiom SomeAxiom(x:tpl) {true};", "SomeAxiom(tpl)", "SomeAxiom ( tpl )")>]
    [<DataRow("axiom SomeAxiom(x:template) {true};", "SomeAxiom(template)", "SomeAxiom ( template )")>]
    [<DataRow("axiom SomeAxiom(x:tplTest) {true};", "SomeAxiom(tplTest)", "SomeAxiom ( tplTest )")>]
    [<DataRow("axiom SomeAxiom(x:templateTest) {true};", "SomeAxiom(templateTest)", "SomeAxiom ( templateTest )")>]
    [<DataRow("axiom SomeAxiom(x,y,z:obj) {true};", "SomeAxiom(obj, obj, obj)", "SomeAxiom ( obj obj obj )")>]
    [<DataRow("axiom SomeAxiom(x,y:pred(z:obj)) {true};", "SomeAxiom(pred(obj), pred(obj))", "SomeAxiom ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("axiom SomeAxiom(x,y:pred(u,v,w:obj)) {true};", "SomeAxiom(pred(obj, obj, obj), pred(obj, obj, obj))", "SomeAxiom ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("postulate SomePostulate() {true};", "SomePostulate()", "SomePostulate ( )")>]
    [<DataRow("postulate SomePostulate(x:ind) {true};", "SomePostulate(ind)", "SomePostulate ( ind )")>]
    [<DataRow("postulate SomePostulate(x:pred) {true};", "SomePostulate(pred)", "SomePostulate ( pred )")>]
    [<DataRow("postulate SomePostulate(x:func) {true};", "SomePostulate(func)", "SomePostulate ( func )")>]
    [<DataRow("postulate SomePostulate(x:obj) {true};", "SomePostulate(obj)", "SomePostulate ( obj )")>]
    [<DataRow("postulate SomePostulate(x:index) {true};", "SomePostulate(ind)", "SomePostulate ( ind )")>]
    [<DataRow("postulate SomePostulate(x:predicate) {true};", "SomePostulate(pred)", "SomePostulate ( pred )")>]
    [<DataRow("postulate SomePostulate(x:function) {true};", "SomePostulate(func)", "SomePostulate ( func )")>]
    [<DataRow("postulate SomePostulate(x:object) {true};", "SomePostulate(obj)", "SomePostulate ( obj )")>]
    [<DataRow("postulate SomePostulate(x:Nat) {true};", "SomePostulate(Nat)", "SomePostulate ( Nat )")>]
    [<DataRow("postulate SomePostulate(x:@Nat) {true};", "SomePostulate(@Nat)", "SomePostulate ( @Nat )")>]
    [<DataRow("postulate SomePostulate(x:tpl) {true};", "SomePostulate(tpl)", "SomePostulate ( tpl )")>]
    [<DataRow("postulate SomePostulate(x:template) {true};", "SomePostulate(template)", "SomePostulate ( template )")>]
    [<DataRow("postulate SomePostulate(x:tplTest) {true};", "SomePostulate(tplTest)", "SomePostulate ( tplTest )")>]
    [<DataRow("postulate SomePostulate(x:templateTest) {true};", "SomePostulate(templateTest)", "SomePostulate ( templateTest )")>]
    [<DataRow("postulate SomePostulate(x,y,z:obj) {true};", "SomePostulate(obj, obj, obj)", "SomePostulate ( obj obj obj )")>]
    [<DataRow("postulate SomePostulate(x,y:pred(z:obj)) {true};", "SomePostulate(pred(obj), pred(obj))", "SomePostulate ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("postulate SomePostulate(x,y:pred(u,v,w:obj)) {true};", "SomePostulate(pred(obj, obj, obj), pred(obj, obj, obj))", "SomePostulate ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("theorem SomeTheorem() {true};", "SomeTheorem()", "SomeTheorem ( )")>]
    [<DataRow("theorem SomeTheorem(x:ind) {true};", "SomeTheorem(ind)", "SomeTheorem ( ind )")>]
    [<DataRow("theorem SomeTheorem(x:pred) {true};", "SomeTheorem(pred)", "SomeTheorem ( pred )")>]
    [<DataRow("theorem SomeTheorem(x:func) {true};", "SomeTheorem(func)", "SomeTheorem ( func )")>]
    [<DataRow("theorem SomeTheorem(x:obj) {true};", "SomeTheorem(obj)", "SomeTheorem ( obj )")>]
    [<DataRow("theorem SomeTheorem(x:index) {true};", "SomeTheorem(ind)", "SomeTheorem ( ind )")>]
    [<DataRow("theorem SomeTheorem(x:predicate) {true};", "SomeTheorem(pred)", "SomeTheorem ( pred )")>]
    [<DataRow("theorem SomeTheorem(x:function) {true};", "SomeTheorem(func)", "SomeTheorem ( func )")>]
    [<DataRow("theorem SomeTheorem(x:object) {true};", "SomeTheorem(obj)", "SomeTheorem ( obj )")>]
    [<DataRow("theorem SomeTheorem(x:Nat) {true};", "SomeTheorem(Nat)", "SomeTheorem ( Nat )")>]
    [<DataRow("theorem SomeTheorem(x:@Nat) {true};", "SomeTheorem(@Nat)", "SomeTheorem ( @Nat )")>]
    [<DataRow("theorem SomeTheorem(x:tpl) {true};", "SomeTheorem(tpl)", "SomeTheorem ( tpl )")>]
    [<DataRow("theorem SomeTheorem(x:template) {true};", "SomeTheorem(template)", "SomeTheorem ( template )")>]
    [<DataRow("theorem SomeTheorem(x:tplTest) {true};", "SomeTheorem(tplTest)", "SomeTheorem ( tplTest )")>]
    [<DataRow("theorem SomeTheorem(x:templateTest) {true};", "SomeTheorem(templateTest)", "SomeTheorem ( templateTest )")>]
    [<DataRow("theorem SomeTheorem(x,y,z:obj) {true};", "SomeTheorem(obj, obj, obj)", "SomeTheorem ( obj obj obj )")>]
    [<DataRow("theorem SomeTheorem(x,y:pred(z:obj)) {true};", "SomeTheorem(pred(obj), pred(obj))", "SomeTheorem ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("theorem SomeTheorem(x,y:pred(u,v,w:obj)) {true};", "SomeTheorem(pred(obj, obj, obj), pred(obj, obj, obj))", "SomeTheorem ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("lemma SomeLemma() {true};", "SomeLemma()", "SomeLemma ( )")>]
    [<DataRow("lemma SomeLemma(x:ind) {true};", "SomeLemma(ind)", "SomeLemma ( ind )")>]
    [<DataRow("lemma SomeLemma(x:pred) {true};", "SomeLemma(pred)", "SomeLemma ( pred )")>]
    [<DataRow("lemma SomeLemma(x:func) {true};", "SomeLemma(func)", "SomeLemma ( func )")>]
    [<DataRow("lemma SomeLemma(x:obj) {true};", "SomeLemma(obj)", "SomeLemma ( obj )")>]
    [<DataRow("lemma SomeLemma(x:index) {true};", "SomeLemma(ind)", "SomeLemma ( ind )")>]
    [<DataRow("lemma SomeLemma(x:predicate) {true};", "SomeLemma(pred)", "SomeLemma ( pred )")>]
    [<DataRow("lemma SomeLemma(x:function) {true};", "SomeLemma(func)", "SomeLemma ( func )")>]
    [<DataRow("lemma SomeLemma(x:object) {true};", "SomeLemma(obj)", "SomeLemma ( obj )")>]
    [<DataRow("lemma SomeLemma(x:Nat) {true};", "SomeLemma(Nat)", "SomeLemma ( Nat )")>]
    [<DataRow("lemma SomeLemma(x:@Nat) {true};", "SomeLemma(@Nat)", "SomeLemma ( @Nat )")>]
    [<DataRow("lemma SomeLemma(x:tpl) {true};", "SomeLemma(tpl)", "SomeLemma ( tpl )")>]
    [<DataRow("lemma SomeLemma(x:template) {true};", "SomeLemma(template)", "SomeLemma ( template )")>]
    [<DataRow("lemma SomeLemma(x:tplTest) {true};", "SomeLemma(tplTest)", "SomeLemma ( tplTest )")>]
    [<DataRow("lemma SomeLemma(x:templateTest) {true};", "SomeLemma(templateTest)", "SomeLemma ( templateTest )")>]
    [<DataRow("lemma SomeLemma(x,y,z:obj) {true};", "SomeLemma(obj, obj, obj)", "SomeLemma ( obj obj obj )")>]
    [<DataRow("lemma SomeLemma(x,y:pred(z:obj)) {true};", "SomeLemma(pred(obj), pred(obj))", "SomeLemma ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("lemma SomeLemma(x,y:pred(u,v,w:obj)) {true};", "SomeLemma(pred(obj, obj, obj), pred(obj, obj, obj))", "SomeLemma ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("proposition SomeProposition() {true};", "SomeProposition()", "SomeProposition ( )")>]
    [<DataRow("proposition SomeProposition(x:ind) {true};", "SomeProposition(ind)", "SomeProposition ( ind )")>]
    [<DataRow("proposition SomeProposition(x:pred) {true};", "SomeProposition(pred)", "SomeProposition ( pred )")>]
    [<DataRow("proposition SomeProposition(x:func) {true};", "SomeProposition(func)", "SomeProposition ( func )")>]
    [<DataRow("proposition SomeProposition(x:obj) {true};", "SomeProposition(obj)", "SomeProposition ( obj )")>]
    [<DataRow("proposition SomeProposition(x:index) {true};", "SomeProposition(ind)", "SomeProposition ( ind )")>]
    [<DataRow("proposition SomeProposition(x:predicate) {true};", "SomeProposition(pred)", "SomeProposition ( pred )")>]
    [<DataRow("proposition SomeProposition(x:function) {true};", "SomeProposition(func)", "SomeProposition ( func )")>]
    [<DataRow("proposition SomeProposition(x:object) {true};", "SomeProposition(obj)", "SomeProposition ( obj )")>]
    [<DataRow("proposition SomeProposition(x:Nat) {true};", "SomeProposition(Nat)", "SomeProposition ( Nat )")>]
    [<DataRow("proposition SomeProposition(x:@Nat) {true};", "SomeProposition(@Nat)", "SomeProposition ( @Nat )")>]
    [<DataRow("proposition SomeProposition(x:tpl) {true};", "SomeProposition(tpl)", "SomeProposition ( tpl )")>]
    [<DataRow("proposition SomeProposition(x:template) {true};", "SomeProposition(template)", "SomeProposition ( template )")>]
    [<DataRow("proposition SomeProposition(x:tplTest) {true};", "SomeProposition(tplTest)", "SomeProposition ( tplTest )")>]
    [<DataRow("proposition SomeProposition(x:templateTest) {true};", "SomeProposition(templateTest)", "SomeProposition ( templateTest )")>]
    [<DataRow("proposition SomeProposition(x,y,z:obj) {true};", "SomeProposition(obj, obj, obj)", "SomeProposition ( obj obj obj )")>]
    [<DataRow("proposition SomeProposition(x,y:pred(z:obj)) {true};", "SomeProposition(pred(obj), pred(obj))", "SomeProposition ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("proposition SomeProposition(x,y:pred(u,v,w:obj)) {true};", "SomeProposition(pred(obj, obj, obj), pred(obj, obj, obj))", "SomeProposition ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("conjecture SomeConjecture() {true};", "SomeConjecture()", "SomeConjecture ( )")>]
    [<DataRow("conjecture SomeConjecture(x:ind) {true};", "SomeConjecture(ind)", "SomeConjecture ( ind )")>]
    [<DataRow("conjecture SomeConjecture(x:pred) {true};", "SomeConjecture(pred)", "SomeConjecture ( pred )")>]
    [<DataRow("conjecture SomeConjecture(x:func) {true};", "SomeConjecture(func)", "SomeConjecture ( func )")>]
    [<DataRow("conjecture SomeConjecture(x:obj) {true};", "SomeConjecture(obj)", "SomeConjecture ( obj )")>]
    [<DataRow("conjecture SomeConjecture(x:index) {true};", "SomeConjecture(ind)", "SomeConjecture ( ind )")>]
    [<DataRow("conjecture SomeConjecture(x:predicate) {true};", "SomeConjecture(pred)", "SomeConjecture ( pred )")>]
    [<DataRow("conjecture SomeConjecture(x:function) {true};", "SomeConjecture(func)", "SomeConjecture ( func )")>]
    [<DataRow("conjecture SomeConjecture(x:object) {true};", "SomeConjecture(obj)", "SomeConjecture ( obj )")>]
    [<DataRow("conjecture SomeConjecture(x:Nat) {true};", "SomeConjecture(Nat)", "SomeConjecture ( Nat )")>]
    [<DataRow("conjecture SomeConjecture(x:@Nat) {true};", "SomeConjecture(@Nat)", "SomeConjecture ( @Nat )")>]
    [<DataRow("conjecture SomeConjecture(x:tpl) {true};", "SomeConjecture(tpl)", "SomeConjecture ( tpl )")>]
    [<DataRow("conjecture SomeConjecture(x:template) {true};", "SomeConjecture(template)", "SomeConjecture ( template )")>]
    [<DataRow("conjecture SomeConjecture(x:tplTest) {true};", "SomeConjecture(tplTest)", "SomeConjecture ( tplTest )")>]
    [<DataRow("conjecture SomeConjecture(x:templateTest) {true};", "SomeConjecture(templateTest)", "SomeConjecture ( templateTest )")>]
    [<DataRow("conjecture SomeConjecture(x,y,z:obj) {true};", "SomeConjecture(obj, obj, obj)", "SomeConjecture ( obj obj obj )")>]
    [<DataRow("conjecture SomeConjecture(x,y:pred(z:obj)) {true};", "SomeConjecture(pred(obj), pred(obj))", "SomeConjecture ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("conjecture SomeConjecture(x,y:pred(u,v,w:obj)) {true};", "SomeConjecture(pred(obj, obj, obj), pred(obj, obj, obj))", "SomeConjecture ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("corollary SomeCorollary$1() {true};", "SomeCorollary$1()", "SomeCorollary $1 ( )")>]
    [<DataRow("corollary SomeCorollary$1(x:ind) {true};", "SomeCorollary$1(ind)", "SomeCorollary $1 ( ind )")>]
    [<DataRow("corollary SomeCorollary$1(x:pred) {true};", "SomeCorollary$1(pred)", "SomeCorollary $1 ( pred )")>]
    [<DataRow("corollary SomeCorollary$1(x:func) {true};", "SomeCorollary$1(func)", "SomeCorollary $1 ( func )")>]
    [<DataRow("corollary SomeCorollary$1(x:obj) {true};", "SomeCorollary$1(obj)", "SomeCorollary $1 ( obj )")>]
    [<DataRow("corollary SomeCorollary$1(x:index) {true};", "SomeCorollary$1(ind)", "SomeCorollary $1 ( ind )")>]
    [<DataRow("corollary SomeCorollary$1(x:predicate) {true};", "SomeCorollary$1(pred)", "SomeCorollary $1 ( pred )")>]
    [<DataRow("corollary SomeCorollary$1(x:function) {true};", "SomeCorollary$1(func)", "SomeCorollary $1 ( func )")>]
    [<DataRow("corollary SomeCorollary$1(x:object) {true};", "SomeCorollary$1(obj)", "SomeCorollary $1 ( obj )")>]
    [<DataRow("corollary SomeCorollary$1(x:Nat) {true};", "SomeCorollary$1(Nat)", "SomeCorollary $1 ( Nat )")>]
    [<DataRow("corollary SomeCorollary$1(x:@Nat) {true};", "SomeCorollary$1(@Nat)", "SomeCorollary $1 ( @Nat )")>]
    [<DataRow("corollary SomeCorollary$1(x:tpl) {true};", "SomeCorollary$1(tpl)", "SomeCorollary $1 ( tpl )")>]
    [<DataRow("corollary SomeCorollary$1(x:template) {true};", "SomeCorollary$1(template)", "SomeCorollary $1 ( template )")>]
    [<DataRow("corollary SomeCorollary$1(x:tplTest) {true};", "SomeCorollary$1(tplTest)", "SomeCorollary $1 ( tplTest )")>]
    [<DataRow("corollary SomeCorollary$1(x:templateTest) {true};", "SomeCorollary$1(templateTest)", "SomeCorollary $1 ( templateTest )")>]
    [<DataRow("corollary SomeCorollary$1(x,y,z:obj) {true};", "SomeCorollary$1(obj, obj, obj)", "SomeCorollary $1 ( obj obj obj )")>]
    [<DataRow("corollary SomeCorollary$1(x,y:pred(z:obj)) {true};", "SomeCorollary$1(pred(obj), pred(obj))", "SomeCorollary $1 ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("corollary SomeCorollary$1(x,y:pred(u,v,w:obj)) {true};", "SomeCorollary$1(pred(obj, obj, obj), pred(obj, obj, obj))", "SomeCorollary $1 ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("proof SomeProof$1 {1. |- trivial} ;", "SomeProof$1", "SomeProof $1")>]

    [<DataRow("def class SomeClass:obj {intrinsic} ;", "SomeClass", "SomeClass obj")>]
    [<DataRow("def class SomeClass:Nat1, :Nat2, :* Nat3, :+ Nat4 {intrinsic} ;", "SomeClass", "SomeClass Nat1 Nat2 Many: Nat3 Many1: Nat4")>]
    [<DataRow("def class SomeClass:obj, :Nat3 {intrinsic} ;", "SomeClass", "SomeClass obj Nat3")>]

    [<DataRow("def pred SomePredicate() {true};", "SomePredicate()", "SomePredicate ( )")>]
    [<DataRow("def pred SomePredicate(x:ind) {true};", "SomePredicate(ind)", "SomePredicate ( ind )")>]
    [<DataRow("def pred SomePredicate(x:pred) {true};", "SomePredicate(pred)", "SomePredicate ( pred )")>]
    [<DataRow("def pred SomePredicate(x:func) {true};", "SomePredicate(func)", "SomePredicate ( func )")>]
    [<DataRow("def pred SomePredicate(x:obj) {true};", "SomePredicate(obj)", "SomePredicate ( obj )")>]
    [<DataRow("def pred SomePredicate(x:index) {true};", "SomePredicate(ind)", "SomePredicate ( ind )")>]
    [<DataRow("def pred SomePredicate(x:predicate) {true};", "SomePredicate(pred)", "SomePredicate ( pred )")>]
    [<DataRow("def pred SomePredicate(x:function) {true};", "SomePredicate(func)", "SomePredicate ( func )")>]
    [<DataRow("def pred SomePredicate(x:object) {true};", "SomePredicate(obj)", "SomePredicate ( obj )")>]
    [<DataRow("def pred SomePredicate(x:Nat) {true};", "SomePredicate(Nat)", "SomePredicate ( Nat )")>]
    [<DataRow("def pred SomePredicate(x:@Nat) {true};", "SomePredicate(@Nat)", "SomePredicate ( @Nat )")>]
    [<DataRow("def pred SomePredicate(x:tpl) {true};", "SomePredicate(tpl)", "SomePredicate ( tpl )")>]
    [<DataRow("def pred SomePredicate(x:template) {true};", "SomePredicate(template)", "SomePredicate ( template )")>]
    [<DataRow("def pred SomePredicate(x:tplTest) {true};", "SomePredicate(tplTest)", "SomePredicate ( tplTest )")>]
    [<DataRow("def pred SomePredicate(x:templateTest) {true};", "SomePredicate(templateTest)", "SomePredicate ( templateTest )")>]
    [<DataRow("def pred SomePredicate(x,y,z:obj) {true};", "SomePredicate(obj, obj, obj)", "SomePredicate ( obj obj obj )")>]
    [<DataRow("def pred SomePredicate(x,y:pred(z:obj)) {true};", "SomePredicate(pred(obj), pred(obj))", "SomePredicate ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("def pred SomePredicate(x,y:pred(u,v,w:obj)) {true};", "SomePredicate(pred(obj, obj, obj), pred(obj, obj, obj))", "SomePredicate ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("def func SomeFunctionalTerm() -> obj {intrinsic};", "SomeFunctionalTerm() -> obj", "SomeFunctionalTerm ( ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:ind) -> obj {intrinsic};", "SomeFunctionalTerm(ind) -> obj", "SomeFunctionalTerm ( ind ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:pred) -> obj {intrinsic};", "SomeFunctionalTerm(pred) -> obj", "SomeFunctionalTerm ( pred ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:func) -> obj {intrinsic};", "SomeFunctionalTerm(func) -> obj", "SomeFunctionalTerm ( func ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:obj) -> obj {intrinsic};", "SomeFunctionalTerm(obj) -> obj", "SomeFunctionalTerm ( obj ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:index) -> obj {intrinsic};", "SomeFunctionalTerm(ind) -> obj", "SomeFunctionalTerm ( ind ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:predicate) -> obj {intrinsic};", "SomeFunctionalTerm(pred) -> obj", "SomeFunctionalTerm ( pred ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:function) -> obj {intrinsic};", "SomeFunctionalTerm(func) -> obj", "SomeFunctionalTerm ( func ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:object) -> obj {intrinsic};", "SomeFunctionalTerm(obj) -> obj", "SomeFunctionalTerm ( obj ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:Nat) -> obj {intrinsic};", "SomeFunctionalTerm(Nat) -> obj", "SomeFunctionalTerm ( Nat ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:@Nat) -> obj {intrinsic};", "SomeFunctionalTerm(@Nat) -> obj", "SomeFunctionalTerm ( @Nat ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:tpl) -> obj {intrinsic};", "SomeFunctionalTerm(tpl) -> obj", "SomeFunctionalTerm ( tpl ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:template) -> obj {intrinsic};", "SomeFunctionalTerm(template) -> obj", "SomeFunctionalTerm ( template ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:tplTest) -> obj {intrinsic};", "SomeFunctionalTerm(tplTest) -> obj", "SomeFunctionalTerm ( tplTest ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x:templateTest) -> obj {intrinsic};", "SomeFunctionalTerm(templateTest) -> obj", "SomeFunctionalTerm ( templateTest ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x,y,z:obj) -> obj {intrinsic};", "SomeFunctionalTerm(obj, obj, obj) -> obj", "SomeFunctionalTerm ( obj obj obj ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x,y:pred(z:obj)) -> obj {intrinsic};", "SomeFunctionalTerm(pred(obj), pred(obj)) -> obj", "SomeFunctionalTerm ( pred ( obj ) pred ( obj ) ) -> obj")>]
    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic};", "SomeFunctionalTerm(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "SomeFunctionalTerm ( pred ( obj obj obj ) pred ( obj obj obj ) ) -> obj")>]
    [<TestMethod>]
    member this.TestTypeSignatureOfFplBlocks(fplCode:string, expectedName:string, expectedTypeSignatureStr:string) =
        let expectedTypeSignature = expectedTypeSignatureStr.Split(' ') |> List.ofArray
        let result = prepareFplCode(fplCode, false) 
        let actual = result.Value.Root.Scope["Test"].Scope[expectedName].TypeSignature
        Assert.AreEqual(expectedTypeSignature, actual)
        prepareFplCode("", true) |> ignore