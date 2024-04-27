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
        testFactory FplBlockType.VariadicVariableMany FplType.Object
        testFactory FplBlockType.VariadicVariableMany1 FplType.Object
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
        testFactory FplBlockType.MandatoryProperty FplType.Object
        testFactory FplBlockType.OptionalProperty FplType.Object
        testFactory FplBlockType.Class FplType.Object 

    member this.ScopeVariablesInSignature() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:pred(u,v,w:func(a,b,c:obj)->obj)) 
            {true}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))"
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


    member this.ScopeVariablesInSignatureWithVariadic() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:+pred(u,v,w:func(a,b,c:*obj)->obj)) 
            {true}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))"
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
    member this.TestScopeVariablesInSignatureWithVariadicIsComplete() =
        try
            this.ScopeVariablesInSignatureWithVariadic() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    member this.ScopeVariablesInBlock() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate() 
            {dec ~x,y:pred(u,v,w:func(a,b,c:obj)->obj); true}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate()"
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
    member this.TestScopeVariablesInBlockIsComplete() =
        try
            this.ScopeVariablesInBlock() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    member this.ScopeVariablesInBlockWithVariadic() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate() 
            {dec ~x,y:+pred(u,v,w:func(a,b,c:*obj)->obj); true}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate()"
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
    member this.TestScopeVariablesInBlockWithVariadicIsComplete() =
        try
            this.ScopeVariablesInBlockWithVariadic() |> ignore
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

            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            Assert.AreEqual(["TestPredicate"; "("; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; ")"], block.TypeSignature)
            Assert.AreEqual([], theory.TypeSignature)
            Assert.AreEqual([], r.TypeSignature)
        | None -> ()

    [<DataRow("", "r")>]
    [<DataRow("Test at (Ln: 1, Col: 1)", "theory")>]
    [<DataRow("Test at (Ln: 2, Col: 13)", "block")>]
    [<DataRow("Test at (Ln: 2, Col: 32)", "x")>]
    [<DataRow("Test at (Ln: 2, Col: 34)", "y")>]
    [<DataRow("Test at (Ln: 2, Col: 45)", "xw")>]
    [<DataRow("Test at (Ln: 2, Col: 41)", "xu")>]
    [<DataRow("Test at (Ln: 2, Col: 43)", "xv")>]
    [<DataRow("Test at (Ln: 2, Col: 45)", "yw")>]
    [<DataRow("Test at (Ln: 2, Col: 41)", "yu")>]
    [<DataRow("Test at (Ln: 2, Col: 43)", "yv")>]
    [<DataRow("Test at (Ln: 2, Col: 52)", "xwa")>]
    [<DataRow("Test at (Ln: 2, Col: 54)", "xwb")>]
    [<DataRow("Test at (Ln: 2, Col: 56)", "xwc")>]
    [<DataRow("Test at (Ln: 2, Col: 52)", "xua")>]
    [<DataRow("Test at (Ln: 2, Col: 54)", "xub")>]
    [<DataRow("Test at (Ln: 2, Col: 56)", "xuc")>]
    [<DataRow("Test at (Ln: 2, Col: 52)", "xva")>]
    [<DataRow("Test at (Ln: 2, Col: 54)", "xvb")>]
    [<DataRow("Test at (Ln: 2, Col: 56)", "xvc")>]
    [<DataRow("Test at (Ln: 2, Col: 52)", "ywa")>]
    [<DataRow("Test at (Ln: 2, Col: 54)", "ywb")>]
    [<DataRow("Test at (Ln: 2, Col: 56)", "ywc")>]
    [<DataRow("Test at (Ln: 2, Col: 52)", "yua")>]
    [<DataRow("Test at (Ln: 2, Col: 54)", "yub")>]
    [<DataRow("Test at (Ln: 2, Col: 56)", "yuc")>]
    [<DataRow("Test at (Ln: 2, Col: 52)", "yva")>]
    [<DataRow("Test at (Ln: 2, Col: 54)", "yvb")>]
    [<DataRow("Test at (Ln: 2, Col: 56)", "yvc")>]
    [<TestMethod>]
    member this.TestScopeVariablesInSignatureQualifiedStartingPos(expected, var) =
        let result = this.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with 
            | "ywc" -> Assert.AreEqual(expected, ywc.QualifiedStartPos)
            | "ywb" -> Assert.AreEqual(expected, ywb.QualifiedStartPos)
            | "ywa" -> Assert.AreEqual(expected, ywa.QualifiedStartPos)
            | "yvc" -> Assert.AreEqual(expected, yvc.QualifiedStartPos)
            | "yvb" -> Assert.AreEqual(expected, yvb.QualifiedStartPos)
            | "yva" -> Assert.AreEqual(expected, yva.QualifiedStartPos)
            | "yuc" -> Assert.AreEqual(expected, yuc.QualifiedStartPos)
            | "yub" -> Assert.AreEqual(expected, yub.QualifiedStartPos)
            | "yua" -> Assert.AreEqual(expected, yua.QualifiedStartPos)
            | "xwc" -> Assert.AreEqual(expected, xwc.QualifiedStartPos)
            | "xwb" -> Assert.AreEqual(expected, xwb.QualifiedStartPos)
            | "xwa" -> Assert.AreEqual(expected, xwa.QualifiedStartPos)
            | "xvc" -> Assert.AreEqual(expected, xvc.QualifiedStartPos)
            | "xvb" -> Assert.AreEqual(expected, xvb.QualifiedStartPos)
            | "xva" -> Assert.AreEqual(expected, xva.QualifiedStartPos)
            | "xuc" -> Assert.AreEqual(expected, xuc.QualifiedStartPos)
            | "xub" -> Assert.AreEqual(expected, xub.QualifiedStartPos)
            | "xua" -> Assert.AreEqual(expected, xua.QualifiedStartPos)

            | "yw" -> Assert.AreEqual(expected, yw.QualifiedStartPos)
            | "yv" -> Assert.AreEqual(expected, yv.QualifiedStartPos)
            | "yu" -> Assert.AreEqual(expected, yu.QualifiedStartPos)
            | "xw" -> Assert.AreEqual(expected, xw.QualifiedStartPos)
            | "xv" -> Assert.AreEqual(expected, xv.QualifiedStartPos)
            | "xu" -> Assert.AreEqual(expected,  xu.QualifiedStartPos)
            | "y" -> Assert.AreEqual(expected, y.QualifiedStartPos)
            | "x" -> Assert.AreEqual(expected, x.QualifiedStartPos)
            | "block" -> Assert.AreEqual(expected, block.QualifiedStartPos)
            | "theory" -> Assert.AreEqual(expected, theory.QualifiedStartPos)
            | "r" -> Assert.AreEqual(expected, r.QualifiedStartPos)
            | _ -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

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
    [<DataRow("inference TestId(x:func(u:obj)->Nat) {pre: true con: true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("inference TestId(x:obj[y:@Nat]) {pre: true con: true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("inference TestId(x:obj[y:Nat]) {pre: true con: true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("inference TestId(x:obj[y:Test.Nat]) {pre: true con: true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("inference TestId(x:obj[y:index]) {pre: true con: true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("inference TestId(x:obj[y:ind]) {pre: true con: true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("inference TestId(x:obj[y:tpl]) {pre: true con: true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("inference TestId(x:obj[y:template]) {pre: true con: true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("inference TestId(x:obj[y:tplTest]) {pre: true con: true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("inference TestId(x:obj[y:templateTest]) {pre: true con: true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("inference TestId(x:obj[y:Nat,z:templateTest]) {pre: true con: true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("inference TestId(x:obj[y:index,z:Nat]) {pre: true con: true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("inference TestId(x:obj[y:obj,z:@Nat]) {pre: true con: true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("inference TestId(x:obj[y:tpl,z:index]) {pre: true con: true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("inference TestId(x:*ind) {pre: true con: true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("inference TestId(x:+pred) {pre: true con: true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("inference TestId(x:*func) {pre: true con: true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("inference TestId(x:+obj) {pre: true con: true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("inference TestId(x:+index) {pre: true con: true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("inference TestId(x:*predicate) {pre: true con: true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("inference TestId(x:+function) {pre: true con: true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("inference TestId(x:*object) {pre: true con: true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("inference TestId(x:+Nat) {pre: true con: true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("inference TestId(x:*@Nat) {pre: true con: true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("inference TestId(x:*tpl) {pre: true con: true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("inference TestId(x:+template) {pre: true con: true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("inference TestId(x:*tplTest) {pre: true con: true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("inference TestId(x:+templateTest) {pre: true con: true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("inference TestId(x,y,z:+obj) {pre: true con: true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("inference TestId(x,y:+pred(z:obj)) {pre: true con: true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("inference TestId(x,y:pred(u,v,w:*obj)) {pre: true con: true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("inference TestId(x:func(u:+obj)->Nat) {pre: true con: true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("inference TestId(x:obj[y:*@Nat]) {pre: true con: true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("inference TestId(x:obj[y:+Nat]) {pre: true con: true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("inference TestId(x:+obj[y:+Test.Nat]) {pre: true con: true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("inference TestId(x:+obj[y:*index]) {pre: true con: true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("inference TestId(x:*obj[y:+ind]) {pre: true con: true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("inference TestId(x:+obj[y:*tpl]) {pre: true con: true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("inference TestId(x:+obj[y:*template]) {pre: true con: true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("inference TestId(x:*obj[y:+tplTest]) {pre: true con: true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("inference TestId(x:*obj[y:*templateTest]) {pre: true con: true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("inference TestId(x:+obj[y:Nat,z:+templateTest]) {pre: true con: true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("inference TestId(x:+obj[y:index,z:*Nat]) {pre: true con: true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("inference TestId(x:*obj[y:*obj,z:+@Nat]) {pre: true con: true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("inference TestId(x:*obj[y:+tpl,z:index]) {pre: true con: true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("axiom TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("axiom TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("axiom TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("axiom TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("axiom TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("axiom TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("axiom TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("axiom TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("axiom TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("axiom TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("axiom TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("axiom TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("axiom TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("axiom TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("axiom TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("axiom TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("axiom TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("axiom TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("axiom TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("axiom TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("axiom TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("axiom TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("axiom TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("axiom TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("axiom TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("axiom TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("axiom TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("axiom TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("axiom TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("axiom TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("axiom TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("axiom TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("axiom TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("axiom TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("axiom TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("axiom TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("axiom TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("axiom TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("axiom TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("axiom TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("postulate TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("postulate TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("postulate TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("postulate TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("postulate TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("postulate TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("postulate TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("postulate TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("postulate TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("postulate TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("postulate TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("postulate TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("postulate TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("postulate TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("postulate TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("postulate TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("postulate TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("postulate TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("postulate TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("postulate TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("postulate TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("postulate TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("postulate TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("postulate TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("postulate TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("postulate TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("postulate TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("postulate TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("postulate TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("postulate TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("postulate TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("postulate TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("postulate TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("postulate TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("postulate TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("postulate TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("postulate TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("postulate TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("postulate TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("postulate TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("theorem TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("theorem TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("theorem TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("theorem TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("theorem TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("theorem TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("theorem TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("theorem TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("theorem TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("theorem TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("theorem TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("theorem TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("theorem TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("theorem TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("theorem TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("theorem TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("theorem TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("theorem TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("theorem TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("theorem TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("theorem TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("theorem TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("theorem TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("theorem TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("theorem TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("theorem TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("theorem TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("theorem TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("theorem TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("theorem TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("theorem TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("theorem TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("theorem TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("theorem TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("theorem TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("theorem TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("theorem TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("theorem TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("theorem TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("theorem TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("lemma TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("lemma TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("lemma TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("lemma TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("lemma TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("lemma TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("lemma TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("lemma TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("lemma TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("lemma TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("lemma TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("lemma TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("lemma TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("lemma TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("lemma TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("lemma TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("lemma TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("lemma TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("lemma TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("lemma TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("lemma TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("lemma TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("lemma TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("lemma TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("lemma TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("lemma TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("lemma TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("lemma TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("lemma TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("lemma TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("lemma TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("lemma TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("lemma TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("lemma TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("lemma TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("lemma TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("lemma TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("lemma TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("lemma TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("lemma TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("proposition TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("proposition TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("proposition TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("proposition TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("proposition TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("proposition TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("proposition TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("proposition TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("proposition TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("proposition TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("proposition TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("proposition TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("proposition TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("proposition TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("proposition TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("proposition TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("proposition TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("proposition TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("proposition TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("proposition TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("proposition TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("proposition TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("proposition TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("proposition TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("proposition TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("proposition TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("proposition TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("proposition TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("proposition TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("proposition TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("proposition TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("proposition TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("proposition TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("proposition TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("proposition TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("proposition TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("proposition TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("proposition TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("proposition TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("proposition TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("conjecture TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("conjecture TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("conjecture TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("conjecture TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("conjecture TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("conjecture TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("conjecture TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("conjecture TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("conjecture TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("conjecture TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("conjecture TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("conjecture TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("conjecture TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("conjecture TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("conjecture TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("conjecture TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("conjecture TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("conjecture TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("conjecture TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("conjecture TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("conjecture TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("conjecture TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("conjecture TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("conjecture TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("conjecture TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("conjecture TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("conjecture TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("conjecture TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("conjecture TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("conjecture TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("conjecture TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("conjecture TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("conjecture TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

    [<DataRow("corollary TestId$1() {true};", "TestId$1()", "TestId $1 ( )")>]
    [<DataRow("corollary TestId$1$2() {true};", "TestId$1$2()", "TestId $1 $2 ( )")>]
    [<DataRow("corollary TestId$1$2$3() {true};", "TestId$1$2$3()", "TestId $1 $2 $3 ( )")>]
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
    [<DataRow("corollary TestId$1(x:func(u:obj)->Nat) {true};", "TestId$1(func(obj) -> Nat)", "TestId $1 ( func ( obj ) -> Nat )")>]
    [<DataRow("corollary TestId$1(x:obj[y:@Nat]) {true};", "TestId$1(obj[@Nat])", "TestId $1 ( obj [ @Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:Nat]) {true};", "TestId$1(obj[Nat])", "TestId $1 ( obj [ Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:Test.Nat]) {true};", "TestId$1(obj[Test.Nat])", "TestId $1 ( obj [ Test.Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:index]) {true};", "TestId$1(obj[ind])", "TestId $1 ( obj [ ind ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:ind]) {true};", "TestId$1(obj[ind])", "TestId $1 ( obj [ ind ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:tpl]) {true};", "TestId$1(obj[tpl])", "TestId $1 ( obj [ tpl ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:template]) {true};", "TestId$1(obj[template])", "TestId $1 ( obj [ template ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:tplTest]) {true};", "TestId$1(obj[tplTest])", "TestId $1 ( obj [ tplTest ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:templateTest]) {true};", "TestId$1(obj[templateTest])", "TestId $1 ( obj [ templateTest ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:Nat,z:templateTest]) {true};", "TestId$1(obj[Nat, templateTest])", "TestId $1 ( obj [ Nat templateTest ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:index,z:Nat]) {true};", "TestId$1(obj[ind, Nat])", "TestId $1 ( obj [ ind Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:obj,z:@Nat]) {true};", "TestId$1(obj[obj, @Nat])", "TestId $1 ( obj [ obj @Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:tpl,z:index]) {true};", "TestId$1(obj[tpl, ind])", "TestId $1 ( obj [ tpl ind ] )")>]

    [<DataRow("corollary TestId$1(x:*ind) {true};", "TestId$1(*ind)", "TestId $1 ( * ind )")>]
    [<DataRow("corollary TestId$1(x:+pred) {true};", "TestId$1(+pred)", "TestId $1 ( + pred )")>]
    [<DataRow("corollary TestId$1(x:*func) {true};", "TestId$1(*func)", "TestId $1 ( * func )")>]
    [<DataRow("corollary TestId$1(x:+obj) {true};", "TestId$1(+obj)", "TestId $1 ( + obj )")>]
    [<DataRow("corollary TestId$1(x:+index) {true};", "TestId$1(+ind)", "TestId $1 ( + ind )")>]
    [<DataRow("corollary TestId$1(x:*predicate) {true};", "TestId$1(*pred)", "TestId $1 ( * pred )")>]
    [<DataRow("corollary TestId$1(x:+function) {true};", "TestId$1(+func)", "TestId $1 ( + func )")>]
    [<DataRow("corollary TestId$1(x:*object) {true};", "TestId$1(*obj)", "TestId $1 ( * obj )")>]
    [<DataRow("corollary TestId$1(x:+Nat) {true};", "TestId$1(+Nat)", "TestId $1 ( + Nat )")>]
    [<DataRow("corollary TestId$1(x:*@Nat) {true};", "TestId$1(*@Nat)", "TestId $1 ( * @Nat )")>]
    [<DataRow("corollary TestId$1(x:*tpl) {true};", "TestId$1(*tpl)", "TestId $1 ( * tpl )")>]
    [<DataRow("corollary TestId$1(x:+template) {true};", "TestId$1(+template)", "TestId $1 ( + template )")>]
    [<DataRow("corollary TestId$1(x:*tplTest) {true};", "TestId$1(*tplTest)", "TestId $1 ( * tplTest )")>]
    [<DataRow("corollary TestId$1(x:+templateTest) {true};", "TestId$1(+templateTest)", "TestId $1 ( + templateTest )")>]
    [<DataRow("corollary TestId$1(x,y,z:+obj) {true};", "TestId$1(+obj, +obj, +obj)", "TestId $1 ( + obj + obj + obj )")>]
    [<DataRow("corollary TestId$1(x,y:+pred(z:obj)) {true};", "TestId$1(+pred(obj), +pred(obj))", "TestId $1 ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("corollary TestId$1(x,y:pred(u,v,w:*obj)) {true};", "TestId$1(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId $1 ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("corollary TestId$1(x:func(u:+obj)->Nat) {true};", "TestId$1(func(+obj) -> Nat)", "TestId $1 ( func ( + obj ) -> Nat )")>]
    [<DataRow("corollary TestId$1(x:obj[y:*@Nat]) {true};", "TestId$1(obj[*@Nat])", "TestId $1 ( obj [ * @Nat ] )")>]
    [<DataRow("corollary TestId$1(x:obj[y:+Nat]) {true};", "TestId$1(obj[+Nat])", "TestId $1 ( obj [ + Nat ] )")>]
    [<DataRow("corollary TestId$1(x:+obj[y:+Test.Nat]) {true};", "TestId$1(+obj[+Test.Nat])", "TestId $1 ( + obj [ + Test.Nat ] )")>]
    [<DataRow("corollary TestId$1(x:+obj[y:*index]) {true};", "TestId$1(+obj[*ind])", "TestId $1 ( + obj [ * ind ] )")>]
    [<DataRow("corollary TestId$1(x:*obj[y:+ind]) {true};", "TestId$1(*obj[+ind])", "TestId $1 ( * obj [ + ind ] )")>]
    [<DataRow("corollary TestId$1(x:+obj[y:*tpl]) {true};", "TestId$1(+obj[*tpl])", "TestId $1 ( + obj [ * tpl ] )")>]
    [<DataRow("corollary TestId$1(x:+obj[y:*template]) {true};", "TestId$1(+obj[*template])", "TestId $1 ( + obj [ * template ] )")>]
    [<DataRow("corollary TestId$1(x:*obj[y:+tplTest]) {true};", "TestId$1(*obj[+tplTest])", "TestId $1 ( * obj [ + tplTest ] )")>]
    [<DataRow("corollary TestId$1(x:*obj[y:*templateTest]) {true};", "TestId$1(*obj[*templateTest])", "TestId $1 ( * obj [ * templateTest ] )")>]
    [<DataRow("corollary TestId$1(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId$1(+obj[Nat, +templateTest])", "TestId $1 ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("corollary TestId$1(x:+obj[y:index,z:*Nat]) {true};", "TestId$1(+obj[ind, *Nat])", "TestId $1 ( + obj [ ind * Nat ] )")>]
    [<DataRow("corollary TestId$1(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId$1(*obj[*obj, +@Nat])", "TestId $1 ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("corollary TestId$1(x:*obj[y:+tpl,z:index]) {true};", "TestId$1(*obj[+tpl, ind])", "TestId $1 ( * obj [ + tpl ind ] )")>]

    [<DataRow("proof TestId$1 {1. |- trivial} ;", "TestId$1", "TestId $1")>]
    [<DataRow("proof TestId$1$2 {1. |- trivial} ;", "TestId$1$2", "TestId $1 $2")>]
    [<DataRow("proof TestId$1$2$3 {1. |- trivial} ;", "TestId$1$2$3", "TestId $1 $2 $3")>]

    
    [<DataRow("def class Test:obj {intr} proof Test$1 {1. |- trivial};", "Test", "Test")>]
    [<DataRow("def class TestId:obj {intrinsic} ;", "TestId", "TestId")>]
    [<DataRow("def class TestId:Nat1, Nat2, Nat3, Nat4 {intrinsic} ;", "TestId", "TestId")>]
    [<DataRow("def class TestId:obj, Nat3 {intrinsic} ;", "TestId", "TestId")>]

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
    [<DataRow("def pred TestId(x:func(u:obj)->Nat) {true};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("def pred TestId(x:obj[y:@Nat]) {true};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[y:Nat]) {true};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[y:Test.Nat]) {true};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[y:index]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("def pred TestId(x:obj[y:ind]) {true};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("def pred TestId(x:obj[y:tpl]) {true};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("def pred TestId(x:obj[y:template]) {true};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("def pred TestId(x:obj[y:tplTest]) {true};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("def pred TestId(x:obj[y:templateTest]) {true};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("def pred TestId(x:obj[y:Nat,z:templateTest]) {true};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("def pred TestId(x:obj[y:index,z:Nat]) {true};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[y:obj,z:@Nat]) {true};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[y:tpl,z:index]) {true};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("def pred TestId(x:*ind) {true};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("def pred TestId(x:+pred) {true};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("def pred TestId(x:*func) {true};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("def pred TestId(x:+obj) {true};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("def pred TestId(x:+index) {true};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("def pred TestId(x:*predicate) {true};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("def pred TestId(x:+function) {true};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("def pred TestId(x:*object) {true};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("def pred TestId(x:+Nat) {true};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("def pred TestId(x:*@Nat) {true};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("def pred TestId(x:*tpl) {true};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("def pred TestId(x:+template) {true};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("def pred TestId(x:*tplTest) {true};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("def pred TestId(x:+templateTest) {true};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("def pred TestId(x,y,z:+obj) {true};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("def pred TestId(x,y:+pred(z:obj)) {true};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("def pred TestId(x,y:pred(u,v,w:*obj)) {true};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("def pred TestId(x:func(u:+obj)->Nat) {true};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("def pred TestId(x:obj[y:*@Nat]) {true};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("def pred TestId(x:obj[y:+Nat]) {true};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("def pred TestId(x:+obj[y:+Test.Nat]) {true};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("def pred TestId(x:+obj[y:*index]) {true};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("def pred TestId(x:*obj[y:+ind]) {true};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("def pred TestId(x:+obj[y:*tpl]) {true};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("def pred TestId(x:+obj[y:*template]) {true};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("def pred TestId(x:*obj[y:+tplTest]) {true};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("def pred TestId(x:*obj[y:*templateTest]) {true};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("def pred TestId(x:+obj[y:Nat,z:+templateTest]) {true};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("def pred TestId(x:+obj[y:index,z:*Nat]) {true};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("def pred TestId(x:*obj[y:*obj,z:+@Nat]) {true};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("def pred TestId(x:*obj[y:+tpl,z:index]) {true};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

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
    [<DataRow("def func TestId(x:func(u:obj)->Nat) -> obj {intrinsic};", "TestId(func(obj) -> Nat) -> obj", "TestId ( func ( obj ) -> Nat ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:@Nat]) -> obj {intrinsic};", "TestId(obj[@Nat]) -> obj", "TestId ( obj [ @Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:Nat]) -> obj {intrinsic};", "TestId(obj[Nat]) -> obj", "TestId ( obj [ Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:Test.Nat]) -> obj {intrinsic};", "TestId(obj[Test.Nat]) -> obj", "TestId ( obj [ Test.Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:index]) -> obj {intrinsic};", "TestId(obj[ind]) -> obj", "TestId ( obj [ ind ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:ind]) -> obj {intrinsic};", "TestId(obj[ind]) -> obj", "TestId ( obj [ ind ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:tpl]) -> obj {intrinsic};", "TestId(obj[tpl]) -> obj", "TestId ( obj [ tpl ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:template]) -> obj {intrinsic};", "TestId(obj[template]) -> obj", "TestId ( obj [ template ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:tplTest]) -> obj {intrinsic};", "TestId(obj[tplTest]) -> obj", "TestId ( obj [ tplTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:templateTest]) -> obj {intrinsic};", "TestId(obj[templateTest]) -> obj", "TestId ( obj [ templateTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:Nat,z:templateTest]) -> obj {intrinsic};", "TestId(obj[Nat, templateTest]) -> obj", "TestId ( obj [ Nat templateTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:index,z:Nat]) -> obj {intrinsic};", "TestId(obj[ind, Nat]) -> obj", "TestId ( obj [ ind Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:obj,z:@Nat]) -> obj {intrinsic};", "TestId(obj[obj, @Nat]) -> obj", "TestId ( obj [ obj @Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:tpl,z:index]) -> obj {intrinsic};", "TestId(obj[tpl, ind]) -> obj", "TestId ( obj [ tpl ind ] ) -> obj")>]

    [<DataRow("def func TestId(x:*ind) -> obj {intrinsic};", "TestId(*ind) -> obj", "TestId ( * ind ) -> obj")>]
    [<DataRow("def func TestId(x:+pred) -> obj {intrinsic};", "TestId(+pred) -> obj", "TestId ( + pred ) -> obj")>]
    [<DataRow("def func TestId(x:*func) -> obj {intrinsic};", "TestId(*func) -> obj", "TestId ( * func ) -> obj")>]
    [<DataRow("def func TestId(x:+obj) -> obj {intrinsic};", "TestId(+obj) -> obj", "TestId ( + obj ) -> obj")>]
    [<DataRow("def func TestId(x:+index) -> obj {intrinsic};", "TestId(+ind) -> obj", "TestId ( + ind ) -> obj")>]
    [<DataRow("def func TestId(x:*predicate) -> obj {intrinsic};", "TestId(*pred) -> obj", "TestId ( * pred ) -> obj")>]
    [<DataRow("def func TestId(x:+function) -> obj {intrinsic};", "TestId(+func) -> obj", "TestId ( + func ) -> obj")>]
    [<DataRow("def func TestId(x:*object) -> obj {intrinsic};", "TestId(*obj) -> obj", "TestId ( * obj ) -> obj")>]
    [<DataRow("def func TestId(x:+Nat) -> obj {intrinsic};", "TestId(+Nat) -> obj", "TestId ( + Nat ) -> obj")>]
    [<DataRow("def func TestId(x:*@Nat) -> obj {intrinsic};", "TestId(*@Nat) -> obj", "TestId ( * @Nat ) -> obj")>]
    [<DataRow("def func TestId(x:*tpl) -> obj {intrinsic};", "TestId(*tpl) -> obj", "TestId ( * tpl ) -> obj")>]
    [<DataRow("def func TestId(x:+template) -> obj {intrinsic};", "TestId(+template) -> obj", "TestId ( + template ) -> obj")>]
    [<DataRow("def func TestId(x:*tplTest) -> obj {intrinsic};", "TestId(*tplTest) -> obj", "TestId ( * tplTest ) -> obj")>]
    [<DataRow("def func TestId(x:+templateTest) -> obj {intrinsic};", "TestId(+templateTest) -> obj", "TestId ( + templateTest ) -> obj")>]
    [<DataRow("def func TestId(x,y,z:+obj) -> obj {intrinsic};", "TestId(+obj, +obj, +obj) -> obj", "TestId ( + obj + obj + obj ) -> obj")>]
    [<DataRow("def func TestId(x,y:+pred(z:obj)) -> obj {intrinsic};", "TestId(+pred(obj), +pred(obj)) -> obj", "TestId ( + pred ( obj ) + pred ( obj ) ) -> obj")>]
    [<DataRow("def func TestId(x,y:pred(u,v,w:*obj)) -> obj {intrinsic};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj)) -> obj", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) ) -> obj")>]
    [<DataRow("def func TestId(x:func(u:+obj)->Nat) -> obj {intrinsic};", "TestId(func(+obj) -> Nat) -> obj", "TestId ( func ( + obj ) -> Nat ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:*@Nat]) -> obj {intrinsic};", "TestId(obj[*@Nat]) -> obj", "TestId ( obj [ * @Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:obj[y:+Nat]) -> obj {intrinsic};", "TestId(obj[+Nat]) -> obj", "TestId ( obj [ + Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:+obj[y:+Test.Nat]) -> obj {intrinsic};", "TestId(+obj[+Test.Nat]) -> obj", "TestId ( + obj [ + Test.Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:+obj[y:*index]) -> obj {intrinsic};", "TestId(+obj[*ind]) -> obj", "TestId ( + obj [ * ind ] ) -> obj")>]
    [<DataRow("def func TestId(x:*obj[y:+ind]) -> obj {intrinsic};", "TestId(*obj[+ind]) -> obj", "TestId ( * obj [ + ind ] ) -> obj")>]
    [<DataRow("def func TestId(x:+obj[y:*tpl]) -> obj {intrinsic};", "TestId(+obj[*tpl]) -> obj", "TestId ( + obj [ * tpl ] ) -> obj")>]
    [<DataRow("def func TestId(x:+obj[y:*template]) -> obj {intrinsic};", "TestId(+obj[*template]) -> obj", "TestId ( + obj [ * template ] ) -> obj")>]
    [<DataRow("def func TestId(x:*obj[y:+tplTest]) -> obj {intrinsic};", "TestId(*obj[+tplTest]) -> obj", "TestId ( * obj [ + tplTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:*obj[y:*templateTest]) -> obj {intrinsic};", "TestId(*obj[*templateTest]) -> obj", "TestId ( * obj [ * templateTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:+obj[y:Nat,z:+templateTest]) -> obj {intrinsic};", "TestId(+obj[Nat, +templateTest]) -> obj", "TestId ( + obj [ Nat + templateTest ] ) -> obj")>]
    [<DataRow("def func TestId(x:+obj[y:index,z:*Nat]) -> obj {intrinsic};", "TestId(+obj[ind, *Nat]) -> obj", "TestId ( + obj [ ind * Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:*obj[y:*obj,z:+@Nat]) -> obj {intrinsic};", "TestId(*obj[*obj, +@Nat]) -> obj", "TestId ( * obj [ * obj + @Nat ] ) -> obj")>]
    [<DataRow("def func TestId(x:*obj[y:+tpl,z:index]) -> obj {intrinsic};", "TestId(*obj[+tpl, ind]) -> obj", "TestId ( * obj [ + tpl ind ] ) -> obj")>]

    [<DataRow("def func TestId() -> ind {intrinsic};", "TestId() -> ind", "TestId ( ) -> ind")>]
    [<DataRow("def func TestId() -> pred {intrinsic};", "TestId() -> pred", "TestId ( ) -> pred")>]
    [<DataRow("def func TestId() -> func {intrinsic};", "TestId() -> func", "TestId ( ) -> func")>]
    [<DataRow("def func TestId() -> obj {intrinsic};", "TestId() -> obj", "TestId ( ) -> obj")>]
    [<DataRow("def func TestId() -> index {intrinsic};", "TestId() -> ind", "TestId ( ) -> ind")>]
    [<DataRow("def func TestId() -> predicate {intrinsic};", "TestId() -> pred", "TestId ( ) -> pred")>]
    [<DataRow("def func TestId() -> function {intrinsic};", "TestId() -> func", "TestId ( ) -> func")>]
    [<DataRow("def func TestId() -> object {intrinsic};", "TestId() -> obj", "TestId ( ) -> obj")>]
    [<DataRow("def func TestId() -> Nat {intrinsic};", "TestId() -> Nat", "TestId ( ) -> Nat")>]
    [<DataRow("def func TestId() -> @Nat {intrinsic};", "TestId() -> @Nat", "TestId ( ) -> @Nat")>]
    [<DataRow("def func TestId() -> tpl {intrinsic};", "TestId() -> tpl", "TestId ( ) -> tpl")>]
    [<DataRow("def func TestId() -> template {intrinsic};", "TestId() -> template", "TestId ( ) -> template")>]
    [<DataRow("def func TestId() -> tplTest {intrinsic};", "TestId() -> tplTest", "TestId ( ) -> tplTest")>]
    [<DataRow("def func TestId() -> templateTest {intrinsic};", "TestId() -> templateTest", "TestId ( ) -> templateTest")>]
    [<DataRow("def func TestId() -> Nat(x,y,z:obj) {intrinsic};", "TestId() -> Nat(obj, obj, obj)", "TestId ( ) -> Nat ( obj obj obj )")>]
    [<DataRow("def func TestId() -> template(x,y:pred(z:obj)) {intrinsic};", "TestId() -> template(pred(obj), pred(obj))", "TestId ( ) -> template ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("def func TestId() -> obj(x,y:pred(u,v,w:obj)) {intrinsic};", "TestId() -> obj(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( ) -> obj ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("def func TestId() -> pred(x:func(u:obj)->Nat) {intrinsic};", "TestId() -> pred(func(obj) -> Nat)", "TestId ( ) -> pred ( func ( obj ) -> Nat )")>]
    [<DataRow("def func TestId() -> func(x:obj[y:@Nat])->obj {intrinsic};", "TestId() -> func(obj[@Nat]) -> obj", "TestId ( ) -> func ( obj [ @Nat ] ) -> obj")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:Test.Nat]) {intrinsic};", "TestId() -> obj(obj[Test.Nat])", "TestId ( ) -> obj ( obj [ Test.Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:index]) {intrinsic};", "TestId() -> obj(obj[ind])", "TestId ( ) -> obj ( obj [ ind ] )")>]
    [<DataRow("def func TestId() -> templateTest(x:obj[y:ind]) {intrinsic};", "TestId() -> templateTest(obj[ind])", "TestId ( ) -> templateTest ( obj [ ind ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:tpl]) {intrinsic};", "TestId() -> obj(obj[tpl])", "TestId ( ) -> obj ( obj [ tpl ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:template]) {intrinsic};", "TestId() -> obj(obj[template])", "TestId ( ) -> obj ( obj [ template ] )")>]
    [<DataRow("def func TestId() -> templateTest(x:obj[y:tplTest]) {intrinsic};", "TestId() -> templateTest(obj[tplTest])", "TestId ( ) -> templateTest ( obj [ tplTest ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:templateTest]) {intrinsic};", "TestId() -> obj(obj[templateTest])", "TestId ( ) -> obj ( obj [ templateTest ] )")>]

    [<DataRow("def func TestId() -> obj(x:obj[y:Nat,z:templateTest]) {intrinsic};", "TestId() -> obj(obj[Nat, templateTest])", "TestId ( ) -> obj ( obj [ Nat templateTest ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:index,z:Nat]) {intrinsic};", "TestId() -> obj(obj[ind, Nat])", "TestId ( ) -> obj ( obj [ ind Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:obj,z:@Nat]) {intrinsic};", "TestId() -> obj(obj[obj, @Nat])", "TestId ( ) -> obj ( obj [ obj @Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:tpl,z:index]) {intrinsic};", "TestId() -> obj(obj[tpl, ind])", "TestId ( ) -> obj ( obj [ tpl ind ] )")>]
    [<DataRow("def func TestId() -> obj(x:*ind) {intrinsic};", "TestId() -> obj(*ind)", "TestId ( ) -> obj ( * ind )")>]
    [<DataRow("def func TestId() -> obj(x:+pred) {intrinsic};", "TestId() -> obj(+pred)", "TestId ( ) -> obj ( + pred )")>]
    [<DataRow("def func TestId() -> obj(x:*func) {intrinsic};", "TestId() -> obj(*func)", "TestId ( ) -> obj ( * func )")>]
    [<DataRow("def func TestId() -> obj(x:+obj) {intrinsic};", "TestId() -> obj(+obj)", "TestId ( ) -> obj ( + obj )")>]

    [<DataRow("def func TestId() -> obj(x:+index) {intrinsic};", "TestId() -> obj(+ind)", "TestId ( ) -> obj ( + ind )")>]
    [<DataRow("def func TestId() -> obj(x:*predicate) {intrinsic};", "TestId() -> obj(*pred)", "TestId ( ) -> obj ( * pred )")>]
    [<DataRow("def func TestId() -> obj(x:+function) {intrinsic};", "TestId() -> obj(+func)", "TestId ( ) -> obj ( + func )")>]
    [<DataRow("def func TestId() -> obj(x:*object) {intrinsic};", "TestId() -> obj(*obj)", "TestId ( ) -> obj ( * obj )")>]
    [<DataRow("def func TestId() -> obj(x:+Nat) {intrinsic};", "TestId() -> obj(+Nat)", "TestId ( ) -> obj ( + Nat )")>]
    [<DataRow("def func TestId() -> obj(x:*@Nat) {intrinsic};", "TestId() -> obj(*@Nat)", "TestId ( ) -> obj ( * @Nat )")>]
    [<DataRow("def func TestId() -> obj(x:*tpl) {intrinsic};", "TestId() -> obj(*tpl)", "TestId ( ) -> obj ( * tpl )")>]
    [<DataRow("def func TestId() -> obj(x:+template) {intrinsic};", "TestId() -> obj(+template)", "TestId ( ) -> obj ( + template )")>]
    [<DataRow("def func TestId() -> obj(x:*tplTest) {intrinsic};", "TestId() -> obj(*tplTest)", "TestId ( ) -> obj ( * tplTest )")>]
    [<DataRow("def func TestId() -> obj(x:+templateTest) {intrinsic};", "TestId() -> obj(+templateTest)", "TestId ( ) -> obj ( + templateTest )")>]
    [<DataRow("def func TestId() -> obj(x,y,z:+obj) {intrinsic};", "TestId() -> obj(+obj, +obj, +obj)", "TestId ( ) -> obj ( + obj + obj + obj )")>]
    [<DataRow("def func TestId() -> obj(x,y:+pred(z:obj)) {intrinsic};", "TestId() -> obj(+pred(obj), +pred(obj))", "TestId ( ) -> obj ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("def func TestId() -> obj(x,y:pred(u,v,w:*obj)) {intrinsic};", "TestId() -> obj(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( ) -> obj ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("def func TestId() -> obj(x:func(u:+obj)->Nat) {intrinsic};", "TestId() -> obj(func(+obj) -> Nat)", "TestId ( ) -> obj ( func ( + obj ) -> Nat )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:*@Nat]) {intrinsic};", "TestId() -> obj(obj[*@Nat])", "TestId ( ) -> obj ( obj [ * @Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:obj[y:+Nat]) {intrinsic};", "TestId() -> obj(obj[+Nat])", "TestId ( ) -> obj ( obj [ + Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:+obj[y:+Test.Nat]) {intrinsic};", "TestId() -> obj(+obj[+Test.Nat])", "TestId ( ) -> obj ( + obj [ + Test.Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:+obj[y:*index]) {intrinsic};", "TestId() -> obj(+obj[*ind])", "TestId ( ) -> obj ( + obj [ * ind ] )")>]
    [<DataRow("def func TestId() -> obj(x:*obj[y:+ind]) {intrinsic};", "TestId() -> obj(*obj[+ind])", "TestId ( ) -> obj ( * obj [ + ind ] )")>]
    [<DataRow("def func TestId() -> obj(x:+obj[y:*tpl]) {intrinsic};", "TestId() -> obj(+obj[*tpl])", "TestId ( ) -> obj ( + obj [ * tpl ] )")>]
    [<DataRow("def func TestId() -> template(x:+obj[y:*template]) {intrinsic};", "TestId() -> template(+obj[*template])", "TestId ( ) -> template ( + obj [ * template ] )")>]
    [<DataRow("def func TestId() -> obj(x:*obj[y:+tplTest]) {intrinsic};", "TestId() -> obj(*obj[+tplTest])", "TestId ( ) -> obj ( * obj [ + tplTest ] )")>]
    [<DataRow("def func TestId() -> obj(x:*obj[y:*templateTest]) {intrinsic};", "TestId() -> obj(*obj[*templateTest])", "TestId ( ) -> obj ( * obj [ * templateTest ] )")>]
    [<DataRow("def func TestId() -> obj(x:+obj[y:Nat,z:+templateTest]) {intrinsic};", "TestId() -> obj(+obj[Nat, +templateTest])", "TestId ( ) -> obj ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("def func TestId() -> obj(x:+obj[y:index,z:*Nat]) {intrinsic};", "TestId() -> obj(+obj[ind, *Nat])", "TestId ( ) -> obj ( + obj [ ind * Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:*obj[y:*obj,z:+@Nat]) {intrinsic};", "TestId() -> obj(*obj[*obj, +@Nat])", "TestId ( ) -> obj ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("def func TestId() -> obj(x:*obj[y:+tpl,z:index]) {intrinsic};", "TestId() -> obj(*obj[+tpl, ind])", "TestId ( ) -> obj ( * obj [ + tpl ind ] )")>]

    [<TestMethod>]
    member this.TestTypeSignatureOfFplBlocks(fplCode:string, expectedName:string, expectedTypeSignatureStr:string) =
        let expectedTypeSignature = expectedTypeSignatureStr.Split(' ') |> List.ofArray
        let result = prepareFplCode(fplCode, false) 
        let fplValue = result.Value.Root.Scope["Test"].Scope[expectedName]
        Assert.AreEqual(expectedName, fplValue.Name)
        let actualTypeSignature = fplValue.TypeSignature
        let actualSignatureStart = fplValue.StartPos.Index
        let actualSignatureEnd = fplValue.NameEndPos.Index
        Assert.AreEqual(expectedTypeSignature, actualTypeSignature)
        let expectedStart =
            if fplCode.StartsWith("def ") then 
                (int64)4
            else
                (int64)0
        Assert.AreEqual(expectedStart, actualSignatureStart)
        let expectedEnd =
            if fplCode.StartsWith("def class") then 
                (int64)(fplCode.IndexOf(":", System.StringComparison.OrdinalIgnoreCase))
            else
                (int64)(fplCode.IndexOf(" {", System.StringComparison.OrdinalIgnoreCase))
        Assert.AreEqual(expectedEnd, actualSignatureEnd)
        prepareFplCode("", true) |> ignore

    member this.ScopeProperties() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestId() 
        {
            intr 
            prty pred T1() {intr}
            prty opt pred T2() {intr}
            prty func T3()->obj {intr}
            prty func opt func T4()->obj {intr}
        }
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestId()"
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let block = theory.Scope[name]
                            let t1 = block.Scope["T1()"]
                            let t2 = block.Scope["T2()"]
                            let t3 = block.Scope["T3() -> obj"]
                            let t4 = block.Scope["T4() -> obj"]
                            Some (r,theory,block,t1,t2,t3,t4)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    [<TestMethod>]
    member this.TestScopePropertiesIsComplete() =
        try
            this.ScopeProperties() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<DataRow("", "r")>]
    [<DataRow("Test at (Ln: 1, Col: 1)", "theory")>]
    [<DataRow("Test at (Ln: 2, Col: 13)", "block")>]
    [<DataRow("Test at (Ln: 5, Col: 13)", "t1")>]
    [<DataRow("Test at (Ln: 6, Col: 13)", "t2")>]
    [<DataRow("Test at (Ln: 7, Col: 13)", "t3")>]
    [<DataRow("Test at (Ln: 8, Col: 13)", "t4")>]
    [<TestMethod>]
    member this.TestScopePropertiesQualifiedStartPos(expected, var) =
        let res = this.ScopeProperties() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(expected, r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual(expected, theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual(expected, block.QualifiedStartPos)
            | "t1" -> Assert.AreEqual(expected, t1.QualifiedStartPos)
            | "t2" -> Assert.AreEqual(expected, t2.QualifiedStartPos)
            | "t3" -> Assert.AreEqual(expected, t3.QualifiedStartPos)
            | "t4" -> Assert.AreEqual(expected, t4.QualifiedStartPos)
            | _ -> ()
        | _ -> 
            Assert.IsTrue(false)


    member this.ScopeConstructors() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def cl TestId:obj 
        {
            ctor TestId() {self} 
            ctor TestId(x:obj) {self} 
            ctor TestId(x:pred) {self} 
            ctor TestId(x:ind) {self} 
        }
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestId"
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let block = theory.Scope[name]
                            let t1 = block.Scope["TestId()"]
                            let t2 = block.Scope["TestId(obj)"]
                            let t3 = block.Scope["TestId(pred)"]
                            let t4 = block.Scope["TestId(ind)"]
                            Some (r,theory,block,t1,t2,t3,t4)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    [<TestMethod>]
    member this.TestScopeConstructorsIsComplete() =
        try
            this.ScopeConstructors() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)


    [<DataRow("a root  at (Ln: 1, Col: 1)", "r")>]
    [<DataRow("a theory Test at (Ln: 1, Col: 1)", "theory")>]
    [<DataRow("a class definition TestId at (Ln: 2, Col: 13)", "block")>]
    [<DataRow("a constructor TestId() at (Ln: 4, Col: 13)", "t1")>]
    [<DataRow("a constructor TestId(obj) at (Ln: 5, Col: 13)", "t2")>]
    [<DataRow("a constructor TestId(pred) at (Ln: 6, Col: 13)", "t3")>]
    [<DataRow("a constructor TestId(ind) at (Ln: 7, Col: 13)", "t4")>]
    [<TestMethod>]
    member this.TestScopeConstructorsQualifiedName(expected, var) =
        let res = this.ScopeConstructors() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(expected, r.QualifiedName)
            | "theory" -> Assert.AreEqual(expected, theory.QualifiedName)
            | "block" -> Assert.AreEqual(expected, block.QualifiedName)
            | "t1" -> Assert.AreEqual(expected, t1.QualifiedName)
            | "t2" -> Assert.AreEqual(expected, t2.QualifiedName)
            | "t3" -> Assert.AreEqual(expected, t3.QualifiedName)
            | "t4" -> Assert.AreEqual(expected, t4.QualifiedName)
            | _ -> ()
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("", "r")>]
    [<DataRow("Test at (Ln: 1, Col: 1)", "theory")>]
    [<DataRow("Test at (Ln: 2, Col: 13)", "block")>]
    [<DataRow("Test at (Ln: 4, Col: 13)", "t1")>]
    [<DataRow("Test at (Ln: 5, Col: 13)", "t2")>]
    [<DataRow("Test at (Ln: 6, Col: 13)", "t3")>]
    [<DataRow("Test at (Ln: 7, Col: 13)", "t4")>]
    [<TestMethod>]
    member this.TestScopeConstructorsQualifiedStartPos(expected, var) =
        let res = this.ScopeConstructors() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(expected, r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual(expected, theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual(expected, block.QualifiedStartPos)
            | "t1" -> Assert.AreEqual(expected, t1.QualifiedStartPos)
            | "t2" -> Assert.AreEqual(expected, t2.QualifiedStartPos)
            | "t3" -> Assert.AreEqual(expected, t3.QualifiedStartPos)
            | "t4" -> Assert.AreEqual(expected, t4.QualifiedStartPos)
            | _ -> ()
        | _ -> 
            Assert.IsTrue(false)


    member this.ScopeProofsAndCorollaries(fplCode, name, subName) =
        FplParser.parserDiagnostics.Clear()
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let block = theory.Scope[name]
                            let subclock = block.Scope[subName]
                            Some (r,theory,block,subclock)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result


    [<DataRow("theorem TestId() {true} proof TestId$1 {1. |- trivial} ;", "TestId()", "TestId$1")>]
    [<DataRow("lemma TestId() {true} proof TestId$1 {1. |- trivial} ;", "TestId()", "TestId$1")>]
    [<DataRow("proposition TestId() {true} proof TestId$1 {1. |- trivial} ;", "TestId()", "TestId$1")>]
    [<DataRow("corollary TestId$2() {true} proof TestId$2$1 {1. |- trivial} ;", "TestId$2()", "TestId$2$1")>]
    [<DataRow("theorem TestId() {true} corollary TestId$1() {true}  ;", "TestId()", "TestId$1()")>]
    [<DataRow("lemma TestId() {true} corollary TestId$1() {true}  ;", "TestId()", "TestId$1()")>]
    [<DataRow("proposition TestId() {true} corollary TestId$1() {true}  ;", "TestId()", "TestId$1()")>]
    [<TestMethod>]
    member this.TestScopeProofsAndCorollariesComplete(fplCode, name, subName) =
        try
            this.ScopeProofsAndCorollaries(fplCode, name, subName) |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<DataRow("theorem TestId() {true} proof TestId$1 {1. |- trivial} ;", "TestId()", "TestId$1", "Test at (Ln: 1, Col: 25)")>]
    [<DataRow("lemma TestId() {true} proof TestId$1 {1. |- trivial} ;", "TestId()", "TestId$1", "Test at (Ln: 1, Col: 23)")>]
    [<DataRow("proposition TestId() {true} proof TestId$1 {1. |- trivial} ;", "TestId()", "TestId$1", "Test at (Ln: 1, Col: 29)")>]
    [<DataRow("corollary TestId$2() {true} proof TestId$2$1 {1. |- trivial} ;", "TestId$2()", "TestId$2$1", "Test at (Ln: 1, Col: 29)")>]
    [<DataRow("theorem TestId() {true} corollary TestId$1() {true}  ;", "TestId()", "TestId$1()", "Test at (Ln: 1, Col: 25)")>]
    [<DataRow("lemma TestId() {true} corollary TestId$1() {true}  ;", "TestId()", "TestId$1()", "Test at (Ln: 1, Col: 23)")>]
    [<DataRow("proposition TestId() {true} corollary TestId$1() {true}  ;", "TestId()", "TestId$1()", "Test at (Ln: 1, Col: 29)")>]
    [<TestMethod>]
    member this.TestScopeProofsAndCorollariesCompleteQualifiedStartPos(fplCode, name, subName, expected) =
        let res = this.ScopeProofsAndCorollaries(fplCode, name, subName) 
        match res with
        | Some (_,_,_,subBlock:FplValue) -> 
                Assert.AreEqual(expected, subBlock.QualifiedStartPos)
            | _ -> ()
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("def pred TestId() { intr prty pred T() {intr} };", "T()", true)>]
    [<DataRow("def pred TestId() { intr prty opt pred T() {intr} };", "T()", false)>]
    [<DataRow("def pred TestId() { intr prty func T()->obj {intr} };", "T() -> obj", true)>]
    [<DataRow("def pred TestId() { intr prty opt func T()->obj {intr} };", "T() -> obj", false)>]
    [<TestMethod>]
    member this.TestMandatoryAndOptionalProperties(fplCode:string, expectedPropertyName:string, isMandatory) =
        let result = prepareFplCode(fplCode, false) 
        let fplBlock = result.Value.Root.Scope["Test"].Scope["TestId()"]
        let propertyValue = fplBlock.Scope[expectedPropertyName]
        if isMandatory then
            Assert.AreEqual(FplBlockType.MandatoryProperty, propertyValue.BlockType)
        else
            Assert.AreEqual(FplBlockType.OptionalProperty, propertyValue.BlockType)
        prepareFplCode("", true) |> ignore

    [<DataRow("def cl TestId:obj {ctor TestId() {self} };", "TestId()")>]
    [<DataRow("def cl TestId:obj {ctor TestId(x:obj) {self} };", "TestId(obj)")>]
    [<DataRow("def cl TestId:obj {ctor TestId(x:ind) {self} };", "TestId(ind)")>]
    [<DataRow("def cl TestId:obj {ctor TestId(x:pred) {self} };", "TestId(pred)")>]
    [<TestMethod>]
    member this.TestConstructor(fplCode:string, expectedConstructorName:string) =
        let result = prepareFplCode(fplCode, false) 
        let fplBlock = result.Value.Root.Scope["Test"].Scope["TestId"]
        let constructorName = fplBlock.Scope[expectedConstructorName]
        Assert.AreEqual(FplBlockType.Constructor, constructorName.BlockType)
        prepareFplCode("", true) |> ignore

    [<DataRow("def cl T:obj {intr prty pred TestId() {intrinsic}};", "TestId()", "TestId ( )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:ind) {intrinsic}};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:pred) {intrinsic}};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:func) {intrinsic}};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj) {intrinsic}};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:index) {intrinsic}};", "TestId(ind)", "TestId ( ind )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:predicate) {intrinsic}};", "TestId(pred)", "TestId ( pred )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:function) {intrinsic}};", "TestId(func)", "TestId ( func )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:object) {intrinsic}};", "TestId(obj)", "TestId ( obj )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:Nat) {intrinsic}};", "TestId(Nat)", "TestId ( Nat )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:@Nat) {intrinsic}};", "TestId(@Nat)", "TestId ( @Nat )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:tpl) {intrinsic}};", "TestId(tpl)", "TestId ( tpl )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:template) {intrinsic}};", "TestId(template)", "TestId ( template )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:tplTest) {intrinsic}};", "TestId(tplTest)", "TestId ( tplTest )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:templateTest) {intrinsic}};", "TestId(templateTest)", "TestId ( templateTest )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x,y,z:obj) {intrinsic}};", "TestId(obj, obj, obj)", "TestId ( obj obj obj )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x,y:pred(z:obj)) {intrinsic}};", "TestId(pred(obj), pred(obj))", "TestId ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x,y:pred(u,v,w:obj)) {intrinsic}};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:func(u:obj)->Nat) {intrinsic}};", "TestId(func(obj) -> Nat)", "TestId ( func ( obj ) -> Nat )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:@Nat]) {intrinsic}};", "TestId(obj[@Nat])", "TestId ( obj [ @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:Nat]) {intrinsic}};", "TestId(obj[Nat])", "TestId ( obj [ Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:Test.Nat]) {intrinsic}};", "TestId(obj[Test.Nat])", "TestId ( obj [ Test.Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:index]) {intrinsic}};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:ind]) {intrinsic}};", "TestId(obj[ind])", "TestId ( obj [ ind ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:tpl]) {intrinsic}};", "TestId(obj[tpl])", "TestId ( obj [ tpl ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:template]) {intrinsic}};", "TestId(obj[template])", "TestId ( obj [ template ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:tplTest]) {intrinsic}};", "TestId(obj[tplTest])", "TestId ( obj [ tplTest ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:templateTest]) {intrinsic}};", "TestId(obj[templateTest])", "TestId ( obj [ templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:Nat,z:templateTest]) {intrinsic}};", "TestId(obj[Nat, templateTest])", "TestId ( obj [ Nat templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:index,z:Nat]) {intrinsic}};", "TestId(obj[ind, Nat])", "TestId ( obj [ ind Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:obj,z:@Nat]) {intrinsic}};", "TestId(obj[obj, @Nat])", "TestId ( obj [ obj @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:tpl,z:index]) {intrinsic}};", "TestId(obj[tpl, ind])", "TestId ( obj [ tpl ind ] )")>]

    [<DataRow("def cl T:obj {intr prty pred TestId(x:*ind) {intrinsic}};", "TestId(*ind)", "TestId ( * ind )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+pred) {intrinsic}};", "TestId(+pred)", "TestId ( + pred )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*func) {intrinsic}};", "TestId(*func)", "TestId ( * func )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj) {intrinsic}};", "TestId(+obj)", "TestId ( + obj )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+index) {intrinsic}};", "TestId(+ind)", "TestId ( + ind )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*predicate) {intrinsic}};", "TestId(*pred)", "TestId ( * pred )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+function) {intrinsic}};", "TestId(+func)", "TestId ( + func )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*object) {intrinsic}};", "TestId(*obj)", "TestId ( * obj )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+Nat) {intrinsic}};", "TestId(+Nat)", "TestId ( + Nat )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*@Nat) {intrinsic}};", "TestId(*@Nat)", "TestId ( * @Nat )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*tpl) {intrinsic}};", "TestId(*tpl)", "TestId ( * tpl )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+template) {intrinsic}};", "TestId(+template)", "TestId ( + template )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*tplTest) {intrinsic}};", "TestId(*tplTest)", "TestId ( * tplTest )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+templateTest) {intrinsic}};", "TestId(+templateTest)", "TestId ( + templateTest )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x,y,z:+obj) {intrinsic}};", "TestId(+obj, +obj, +obj)", "TestId ( + obj + obj + obj )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x,y:+pred(z:obj)) {intrinsic}};", "TestId(+pred(obj), +pred(obj))", "TestId ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x,y:pred(u,v,w:*obj)) {intrinsic}};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:func(u:+obj)->Nat) {intrinsic}};", "TestId(func(+obj) -> Nat)", "TestId ( func ( + obj ) -> Nat )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:*@Nat]) {intrinsic}};", "TestId(obj[*@Nat])", "TestId ( obj [ * @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:obj[y:+Nat]) {intrinsic}};", "TestId(obj[+Nat])", "TestId ( obj [ + Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj[y:+Test.Nat]) {intrinsic}};", "TestId(+obj[+Test.Nat])", "TestId ( + obj [ + Test.Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj[y:*index]) {intrinsic}};", "TestId(+obj[*ind])", "TestId ( + obj [ * ind ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*obj[y:+ind]) {intrinsic}};", "TestId(*obj[+ind])", "TestId ( * obj [ + ind ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj[y:*tpl]) {intrinsic}};", "TestId(+obj[*tpl])", "TestId ( + obj [ * tpl ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj[y:*template]) {intrinsic}};", "TestId(+obj[*template])", "TestId ( + obj [ * template ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*obj[y:+tplTest]) {intrinsic}};", "TestId(*obj[+tplTest])", "TestId ( * obj [ + tplTest ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*obj[y:*templateTest]) {intrinsic}};", "TestId(*obj[*templateTest])", "TestId ( * obj [ * templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj[y:Nat,z:+templateTest]) {intrinsic}};", "TestId(+obj[Nat, +templateTest])", "TestId ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:+obj[y:index,z:*Nat]) {intrinsic}};", "TestId(+obj[ind, *Nat])", "TestId ( + obj [ ind * Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*obj[y:*obj,z:+@Nat]) {intrinsic}};", "TestId(*obj[*obj, +@Nat])", "TestId ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty pred TestId(x:*obj[y:+tpl,z:index]) {intrinsic}};", "TestId(*obj[+tpl, ind])", "TestId ( * obj [ + tpl ind ] )")>]

    [<DataRow("def cl T:obj {intr prty func TestId() -> obj {intrinsic}};", "TestId() -> obj", "TestId ( ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:ind) -> obj {intrinsic}};", "TestId(ind) -> obj", "TestId ( ind ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:pred) -> obj {intrinsic}};", "TestId(pred) -> obj", "TestId ( pred ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:func) -> obj {intrinsic}};", "TestId(func) -> obj", "TestId ( func ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj) -> obj {intrinsic}};", "TestId(obj) -> obj", "TestId ( obj ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:index) -> obj {intrinsic}};", "TestId(ind) -> obj", "TestId ( ind ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:predicate) -> obj {intrinsic}};", "TestId(pred) -> obj", "TestId ( pred ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:function) -> obj {intrinsic}};", "TestId(func) -> obj", "TestId ( func ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:object) -> obj {intrinsic}};", "TestId(obj) -> obj", "TestId ( obj ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:Nat) -> obj {intrinsic}};", "TestId(Nat) -> obj", "TestId ( Nat ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:@Nat) -> obj {intrinsic}};", "TestId(@Nat) -> obj", "TestId ( @Nat ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:tpl) -> obj {intrinsic}};", "TestId(tpl) -> obj", "TestId ( tpl ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:template) -> obj {intrinsic}};", "TestId(template) -> obj", "TestId ( template ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:tplTest) -> obj {intrinsic}};", "TestId(tplTest) -> obj", "TestId ( tplTest ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:templateTest) -> obj {intrinsic}};", "TestId(templateTest) -> obj", "TestId ( templateTest ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x,y,z:obj) -> obj {intrinsic}};", "TestId(obj, obj, obj) -> obj", "TestId ( obj obj obj ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x,y:pred(z:obj)) -> obj {intrinsic}};", "TestId(pred(obj), pred(obj)) -> obj", "TestId ( pred ( obj ) pred ( obj ) ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x,y:pred(u,v,w:obj)) -> obj {intrinsic}};", "TestId(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj", "TestId ( pred ( obj obj obj ) pred ( obj obj obj ) ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:func(u:obj)->Nat) -> obj {intrinsic}};", "TestId(func(obj) -> Nat) -> obj", "TestId ( func ( obj ) -> Nat ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:@Nat]) -> obj {intrinsic}};", "TestId(obj[@Nat]) -> obj", "TestId ( obj [ @Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:Nat]) -> obj {intrinsic}};", "TestId(obj[Nat]) -> obj", "TestId ( obj [ Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:Test.Nat]) -> obj {intrinsic}};", "TestId(obj[Test.Nat]) -> obj", "TestId ( obj [ Test.Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:index]) -> obj {intrinsic}};", "TestId(obj[ind]) -> obj", "TestId ( obj [ ind ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:ind]) -> obj {intrinsic}};", "TestId(obj[ind]) -> obj", "TestId ( obj [ ind ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:tpl]) -> obj {intrinsic}};", "TestId(obj[tpl]) -> obj", "TestId ( obj [ tpl ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:template]) -> obj {intrinsic}};", "TestId(obj[template]) -> obj", "TestId ( obj [ template ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:tplTest]) -> obj {intrinsic}};", "TestId(obj[tplTest]) -> obj", "TestId ( obj [ tplTest ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:templateTest]) -> obj {intrinsic}};", "TestId(obj[templateTest]) -> obj", "TestId ( obj [ templateTest ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:Nat,z:templateTest]) -> obj {intrinsic}};", "TestId(obj[Nat, templateTest]) -> obj", "TestId ( obj [ Nat templateTest ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:index,z:Nat]) -> obj {intrinsic}};", "TestId(obj[ind, Nat]) -> obj", "TestId ( obj [ ind Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:obj,z:@Nat]) -> obj {intrinsic}};", "TestId(obj[obj, @Nat]) -> obj", "TestId ( obj [ obj @Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:tpl,z:index]) -> obj {intrinsic}};", "TestId(obj[tpl, ind]) -> obj", "TestId ( obj [ tpl ind ] ) -> obj")>]

    [<DataRow("def cl T:obj {intr prty func TestId(x:*ind) -> obj {intrinsic}};", "TestId(*ind) -> obj", "TestId ( * ind ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+pred) -> obj {intrinsic}};", "TestId(+pred) -> obj", "TestId ( + pred ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*func) -> obj {intrinsic}};", "TestId(*func) -> obj", "TestId ( * func ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj) -> obj {intrinsic}};", "TestId(+obj) -> obj", "TestId ( + obj ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+index) -> obj {intrinsic}};", "TestId(+ind) -> obj", "TestId ( + ind ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*predicate) -> obj {intrinsic}};", "TestId(*pred) -> obj", "TestId ( * pred ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+function) -> obj {intrinsic}};", "TestId(+func) -> obj", "TestId ( + func ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*object) -> obj {intrinsic}};", "TestId(*obj) -> obj", "TestId ( * obj ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+Nat) -> obj {intrinsic}};", "TestId(+Nat) -> obj", "TestId ( + Nat ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*@Nat) -> obj {intrinsic}};", "TestId(*@Nat) -> obj", "TestId ( * @Nat ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*tpl) -> obj {intrinsic}};", "TestId(*tpl) -> obj", "TestId ( * tpl ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+template) -> obj {intrinsic}};", "TestId(+template) -> obj", "TestId ( + template ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*tplTest) -> obj {intrinsic}};", "TestId(*tplTest) -> obj", "TestId ( * tplTest ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+templateTest) -> obj {intrinsic}};", "TestId(+templateTest) -> obj", "TestId ( + templateTest ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x,y,z:+obj) -> obj {intrinsic}};", "TestId(+obj, +obj, +obj) -> obj", "TestId ( + obj + obj + obj ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x,y:+pred(z:obj)) -> obj {intrinsic}};", "TestId(+pred(obj), +pred(obj)) -> obj", "TestId ( + pred ( obj ) + pred ( obj ) ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x,y:pred(u,v,w:*obj)) -> obj {intrinsic}};", "TestId(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj)) -> obj", "TestId ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:func(u:+obj)->Nat) -> obj {intrinsic}};", "TestId(func(+obj) -> Nat) -> obj", "TestId ( func ( + obj ) -> Nat ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:*@Nat]) -> obj {intrinsic}};", "TestId(obj[*@Nat]) -> obj", "TestId ( obj [ * @Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:obj[y:+Nat]) -> obj {intrinsic}};", "TestId(obj[+Nat]) -> obj", "TestId ( obj [ + Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj[y:+Test.Nat]) -> obj {intrinsic}};", "TestId(+obj[+Test.Nat]) -> obj", "TestId ( + obj [ + Test.Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj[y:*index]) -> obj {intrinsic}};", "TestId(+obj[*ind]) -> obj", "TestId ( + obj [ * ind ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*obj[y:+ind]) -> obj {intrinsic}};", "TestId(*obj[+ind]) -> obj", "TestId ( * obj [ + ind ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj[y:*tpl]) -> obj {intrinsic}};", "TestId(+obj[*tpl]) -> obj", "TestId ( + obj [ * tpl ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj[y:*template]) -> obj {intrinsic}};", "TestId(+obj[*template]) -> obj", "TestId ( + obj [ * template ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*obj[y:+tplTest]) -> obj {intrinsic}};", "TestId(*obj[+tplTest]) -> obj", "TestId ( * obj [ + tplTest ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*obj[y:*templateTest]) -> obj {intrinsic}};", "TestId(*obj[*templateTest]) -> obj", "TestId ( * obj [ * templateTest ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj[y:Nat,z:+templateTest]) -> obj {intrinsic}};", "TestId(+obj[Nat, +templateTest]) -> obj", "TestId ( + obj [ Nat + templateTest ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:+obj[y:index,z:*Nat]) -> obj {intrinsic}};", "TestId(+obj[ind, *Nat]) -> obj", "TestId ( + obj [ ind * Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*obj[y:*obj,z:+@Nat]) -> obj {intrinsic}};", "TestId(*obj[*obj, +@Nat]) -> obj", "TestId ( * obj [ * obj + @Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId(x:*obj[y:+tpl,z:index]) -> obj {intrinsic}};", "TestId(*obj[+tpl, ind]) -> obj", "TestId ( * obj [ + tpl ind ] ) -> obj")>]

    [<DataRow("def cl T:obj {intr prty func TestId() -> ind {intrinsic}};", "TestId() -> ind", "TestId ( ) -> ind")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> pred {intrinsic}};", "TestId() -> pred", "TestId ( ) -> pred")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> func {intrinsic}};", "TestId() -> func", "TestId ( ) -> func")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj {intrinsic}};", "TestId() -> obj", "TestId ( ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> index {intrinsic}};", "TestId() -> ind", "TestId ( ) -> ind")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> predicate {intrinsic}};", "TestId() -> pred", "TestId ( ) -> pred")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> function {intrinsic}};", "TestId() -> func", "TestId ( ) -> func")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> object {intrinsic}};", "TestId() -> obj", "TestId ( ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> Nat {intrinsic}};", "TestId() -> Nat", "TestId ( ) -> Nat")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> @Nat {intrinsic}};", "TestId() -> @Nat", "TestId ( ) -> @Nat")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> tpl {intrinsic}};", "TestId() -> tpl", "TestId ( ) -> tpl")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> template {intrinsic}};", "TestId() -> template", "TestId ( ) -> template")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> tplTest {intrinsic}};", "TestId() -> tplTest", "TestId ( ) -> tplTest")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> templateTest {intrinsic}};", "TestId() -> templateTest", "TestId ( ) -> templateTest")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> Nat(x,y,z:obj) {intrinsic}};", "TestId() -> Nat(obj, obj, obj)", "TestId ( ) -> Nat ( obj obj obj )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> template(x,y:pred(z:obj)) {intrinsic}};", "TestId() -> template(pred(obj), pred(obj))", "TestId ( ) -> template ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x,y:pred(u,v,w:obj)) {intrinsic}};", "TestId() -> obj(pred(obj, obj, obj), pred(obj, obj, obj))", "TestId ( ) -> obj ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]

    [<DataRow("def cl T:obj {intr prty func TestId() -> pred(x:func(u:obj)->Nat) {intrinsic}};", "TestId() -> pred(func(obj) -> Nat)", "TestId ( ) -> pred ( func ( obj ) -> Nat )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> func(x:obj[y:@Nat])->obj {intrinsic}};", "TestId() -> func(obj[@Nat]) -> obj", "TestId ( ) -> func ( obj [ @Nat ] ) -> obj")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:Test.Nat]) {intrinsic}};", "TestId() -> obj(obj[Test.Nat])", "TestId ( ) -> obj ( obj [ Test.Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:index]) {intrinsic}};", "TestId() -> obj(obj[ind])", "TestId ( ) -> obj ( obj [ ind ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> templateTest(x:obj[y:ind]) {intrinsic}};", "TestId() -> templateTest(obj[ind])", "TestId ( ) -> templateTest ( obj [ ind ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:tpl]) {intrinsic}};", "TestId() -> obj(obj[tpl])", "TestId ( ) -> obj ( obj [ tpl ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:template]) {intrinsic}};", "TestId() -> obj(obj[template])", "TestId ( ) -> obj ( obj [ template ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> templateTest(x:obj[y:tplTest]) {intrinsic}};", "TestId() -> templateTest(obj[tplTest])", "TestId ( ) -> templateTest ( obj [ tplTest ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:templateTest]) {intrinsic}};", "TestId() -> obj(obj[templateTest])", "TestId ( ) -> obj ( obj [ templateTest ] )")>]

    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:Nat,z:templateTest]) {intrinsic}};", "TestId() -> obj(obj[Nat, templateTest])", "TestId ( ) -> obj ( obj [ Nat templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:index,z:Nat]) {intrinsic}};", "TestId() -> obj(obj[ind, Nat])", "TestId ( ) -> obj ( obj [ ind Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:obj,z:@Nat]) {intrinsic}};", "TestId() -> obj(obj[obj, @Nat])", "TestId ( ) -> obj ( obj [ obj @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:tpl,z:index]) {intrinsic}};", "TestId() -> obj(obj[tpl, ind])", "TestId ( ) -> obj ( obj [ tpl ind ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*ind) {intrinsic}};", "TestId() -> obj(*ind)", "TestId ( ) -> obj ( * ind )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+pred) {intrinsic}};", "TestId() -> obj(+pred)", "TestId ( ) -> obj ( + pred )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*func) {intrinsic}};", "TestId() -> obj(*func)", "TestId ( ) -> obj ( * func )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+obj) {intrinsic}};", "TestId() -> obj(+obj)", "TestId ( ) -> obj ( + obj )")>]

    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+index) {intrinsic}};", "TestId() -> obj(+ind)", "TestId ( ) -> obj ( + ind )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*predicate) {intrinsic}};", "TestId() -> obj(*pred)", "TestId ( ) -> obj ( * pred )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+function) {intrinsic}};", "TestId() -> obj(+func)", "TestId ( ) -> obj ( + func )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*object) {intrinsic}};", "TestId() -> obj(*obj)", "TestId ( ) -> obj ( * obj )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+Nat) {intrinsic}};", "TestId() -> obj(+Nat)", "TestId ( ) -> obj ( + Nat )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*@Nat) {intrinsic}};", "TestId() -> obj(*@Nat)", "TestId ( ) -> obj ( * @Nat )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*tpl) {intrinsic}};", "TestId() -> obj(*tpl)", "TestId ( ) -> obj ( * tpl )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+template) {intrinsic}};", "TestId() -> obj(+template)", "TestId ( ) -> obj ( + template )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*tplTest) {intrinsic}};", "TestId() -> obj(*tplTest)", "TestId ( ) -> obj ( * tplTest )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+templateTest) {intrinsic}};", "TestId() -> obj(+templateTest)", "TestId ( ) -> obj ( + templateTest )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x,y,z:+obj) {intrinsic}};", "TestId() -> obj(+obj, +obj, +obj)", "TestId ( ) -> obj ( + obj + obj + obj )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x,y:+pred(z:obj)) {intrinsic}};", "TestId() -> obj(+pred(obj), +pred(obj))", "TestId ( ) -> obj ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x,y:pred(u,v,w:*obj)) {intrinsic}};", "TestId() -> obj(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "TestId ( ) -> obj ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:func(u:+obj)->Nat) {intrinsic}};", "TestId() -> obj(func(+obj) -> Nat)", "TestId ( ) -> obj ( func ( + obj ) -> Nat )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:*@Nat]) {intrinsic}};", "TestId() -> obj(obj[*@Nat])", "TestId ( ) -> obj ( obj [ * @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:obj[y:+Nat]) {intrinsic}};", "TestId() -> obj(obj[+Nat])", "TestId ( ) -> obj ( obj [ + Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+obj[y:+Test.Nat]) {intrinsic}};", "TestId() -> obj(+obj[+Test.Nat])", "TestId ( ) -> obj ( + obj [ + Test.Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+obj[y:*index]) {intrinsic}};", "TestId() -> obj(+obj[*ind])", "TestId ( ) -> obj ( + obj [ * ind ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*obj[y:+ind]) {intrinsic}};", "TestId() -> obj(*obj[+ind])", "TestId ( ) -> obj ( * obj [ + ind ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+obj[y:*tpl]) {intrinsic}};", "TestId() -> obj(+obj[*tpl])", "TestId ( ) -> obj ( + obj [ * tpl ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> template(x:+obj[y:*template]) {intrinsic}};", "TestId() -> template(+obj[*template])", "TestId ( ) -> template ( + obj [ * template ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*obj[y:+tplTest]) {intrinsic}};", "TestId() -> obj(*obj[+tplTest])", "TestId ( ) -> obj ( * obj [ + tplTest ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*obj[y:*templateTest]) {intrinsic}};", "TestId() -> obj(*obj[*templateTest])", "TestId ( ) -> obj ( * obj [ * templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+obj[y:Nat,z:+templateTest]) {intrinsic}};", "TestId() -> obj(+obj[Nat, +templateTest])", "TestId ( ) -> obj ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:+obj[y:index,z:*Nat]) {intrinsic}};", "TestId() -> obj(+obj[ind, *Nat])", "TestId ( ) -> obj ( + obj [ ind * Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*obj[y:*obj,z:+@Nat]) {intrinsic}};", "TestId() -> obj(*obj[*obj, +@Nat])", "TestId ( ) -> obj ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("def cl T:obj {intr prty func TestId() -> obj(x:*obj[y:+tpl,z:index]) {intrinsic}};", "TestId() -> obj(*obj[+tpl, ind])", "TestId ( ) -> obj ( * obj [ + tpl ind ] )")>]

    [<TestMethod>]
    member this.TestTypeSignatureOfFplProperties(fplCode:string, expectedName:string, expectedTypeSignatureStr:string) =
        let expectedTypeSignature = expectedTypeSignatureStr.Split(' ') |> List.ofArray
        let result = prepareFplCode(fplCode, false) 
        let fplValue = result.Value.Root.Scope["Test"].Scope["T"].Scope[expectedName]
        let actualTypeSignature = fplValue.TypeSignature
        let actualSignatureStart = fplValue.StartPos.Index
        let actualSignatureEnd = fplValue.NameEndPos.Index
        Assert.AreEqual(expectedTypeSignature, actualTypeSignature)
        let expectedStart =
                (int64)19
        Assert.AreEqual(expectedStart, actualSignatureStart)
        let expectedEnd =
                (int64)(fplCode.IndexOf(" {intrinsic", System.StringComparison.OrdinalIgnoreCase))
        Assert.AreEqual(expectedEnd, actualSignatureEnd)
        prepareFplCode("", true) |> ignore


    [<DataRow("def cl T:obj {ctor T() {self}};", "T()", "T ( )")>]
    [<DataRow("def cl T:obj {ctor T(x:ind) {self}};", "T(ind)", "T ( ind )")>]
    [<DataRow("def cl T:obj {ctor T(x:pred) {self}};", "T(pred)", "T ( pred )")>]
    [<DataRow("def cl T:obj {ctor T(x:func) {self}};", "T(func)", "T ( func )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj) {self}};", "T(obj)", "T ( obj )")>]
    [<DataRow("def cl T:obj {ctor T(x:index) {self}};", "T(ind)", "T ( ind )")>]
    [<DataRow("def cl T:obj {ctor T(x:predicate) {self}};", "T(pred)", "T ( pred )")>]
    [<DataRow("def cl T:obj {ctor T(x:function) {self}};", "T(func)", "T ( func )")>]
    [<DataRow("def cl T:obj {ctor T(x:object) {self}};", "T(obj)", "T ( obj )")>]
    [<DataRow("def cl T:obj {ctor T(x:Nat) {self}};", "T(Nat)", "T ( Nat )")>]
    [<DataRow("def cl T:obj {ctor T(x:@Nat) {self}};", "T(@Nat)", "T ( @Nat )")>]
    [<DataRow("def cl T:obj {ctor T(x:tpl) {self}};", "T(tpl)", "T ( tpl )")>]
    [<DataRow("def cl T:obj {ctor T(x:template) {self}};", "T(template)", "T ( template )")>]
    [<DataRow("def cl T:obj {ctor T(x:tplTest) {self}};", "T(tplTest)", "T ( tplTest )")>]
    [<DataRow("def cl T:obj {ctor T(x:templateTest) {self}};", "T(templateTest)", "T ( templateTest )")>]
    [<DataRow("def cl T:obj {ctor T(x,y,z:obj) {self}};", "T(obj, obj, obj)", "T ( obj obj obj )")>]
    [<DataRow("def cl T:obj {ctor T(x,y:pred(z:obj)) {self}};", "T(pred(obj), pred(obj))", "T ( pred ( obj ) pred ( obj ) )")>]
    [<DataRow("def cl T:obj {ctor T(x,y:pred(u,v,w:obj)) {self}};", "T(pred(obj, obj, obj), pred(obj, obj, obj))", "T ( pred ( obj obj obj ) pred ( obj obj obj ) )")>]
    [<DataRow("def cl T:obj {ctor T(x:func(u:obj)->Nat) {self}};", "T(func(obj) -> Nat)", "T ( func ( obj ) -> Nat )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:@Nat]) {self}};", "T(obj[@Nat])", "T ( obj [ @Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:Nat]) {self}};", "T(obj[Nat])", "T ( obj [ Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:Test.Nat]) {self}};", "T(obj[Test.Nat])", "T ( obj [ Test.Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:index]) {self}};", "T(obj[ind])", "T ( obj [ ind ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:ind]) {self}};", "T(obj[ind])", "T ( obj [ ind ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:tpl]) {self}};", "T(obj[tpl])", "T ( obj [ tpl ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:template]) {self}};", "T(obj[template])", "T ( obj [ template ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:tplTest]) {self}};", "T(obj[tplTest])", "T ( obj [ tplTest ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:templateTest]) {self}};", "T(obj[templateTest])", "T ( obj [ templateTest ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:Nat,z:templateTest]) {self}};", "T(obj[Nat, templateTest])", "T ( obj [ Nat templateTest ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:index,z:Nat]) {self}};", "T(obj[ind, Nat])", "T ( obj [ ind Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:obj,z:@Nat]) {self}};", "T(obj[obj, @Nat])", "T ( obj [ obj @Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:tpl,z:index]) {self}};", "T(obj[tpl, ind])", "T ( obj [ tpl ind ] )")>]

    [<DataRow("def cl T:obj {ctor T(x:*ind) {self}};", "T(*ind)", "T ( * ind )")>]
    [<DataRow("def cl T:obj {ctor T(x:+pred) {self}};", "T(+pred)", "T ( + pred )")>]
    [<DataRow("def cl T:obj {ctor T(x:*func) {self}};", "T(*func)", "T ( * func )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj) {self}};", "T(+obj)", "T ( + obj )")>]
    [<DataRow("def cl T:obj {ctor T(x:+index) {self}};", "T(+ind)", "T ( + ind )")>]
    [<DataRow("def cl T:obj {ctor T(x:*predicate) {self}};", "T(*pred)", "T ( * pred )")>]
    [<DataRow("def cl T:obj {ctor T(x:+function) {self}};", "T(+func)", "T ( + func )")>]
    [<DataRow("def cl T:obj {ctor T(x:*object) {self}};", "T(*obj)", "T ( * obj )")>]
    [<DataRow("def cl T:obj {ctor T(x:+Nat) {self}};", "T(+Nat)", "T ( + Nat )")>]
    [<DataRow("def cl T:obj {ctor T(x:*@Nat) {self}};", "T(*@Nat)", "T ( * @Nat )")>]
    [<DataRow("def cl T:obj {ctor T(x:*tpl) {self}};", "T(*tpl)", "T ( * tpl )")>]
    [<DataRow("def cl T:obj {ctor T(x:+template) {self}};", "T(+template)", "T ( + template )")>]
    [<DataRow("def cl T:obj {ctor T(x:*tplTest) {self}};", "T(*tplTest)", "T ( * tplTest )")>]
    [<DataRow("def cl T:obj {ctor T(x:+templateTest) {self}};", "T(+templateTest)", "T ( + templateTest )")>]
    [<DataRow("def cl T:obj {ctor T(x,y,z:+obj) {self}};", "T(+obj, +obj, +obj)", "T ( + obj + obj + obj )")>]
    [<DataRow("def cl T:obj {ctor T(x,y:+pred(z:obj)) {self}};", "T(+pred(obj), +pred(obj))", "T ( + pred ( obj ) + pred ( obj ) )")>]
    [<DataRow("def cl T:obj {ctor T(x,y:pred(u,v,w:*obj)) {self}};", "T(pred(*obj, *obj, *obj), pred(*obj, *obj, *obj))", "T ( pred ( * obj * obj * obj ) pred ( * obj * obj * obj ) )")>]
    [<DataRow("def cl T:obj {ctor T(x:func(u:+obj)->Nat) {self}};", "T(func(+obj) -> Nat)", "T ( func ( + obj ) -> Nat )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:*@Nat]) {self}};", "T(obj[*@Nat])", "T ( obj [ * @Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:obj[y:+Nat]) {self}};", "T(obj[+Nat])", "T ( obj [ + Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj[y:+Test.Nat]) {self}};", "T(+obj[+Test.Nat])", "T ( + obj [ + Test.Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj[y:*index]) {self}};", "T(+obj[*ind])", "T ( + obj [ * ind ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:*obj[y:+ind]) {self}};", "T(*obj[+ind])", "T ( * obj [ + ind ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj[y:*tpl]) {self}};", "T(+obj[*tpl])", "T ( + obj [ * tpl ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj[y:*template]) {self}};", "T(+obj[*template])", "T ( + obj [ * template ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:*obj[y:+tplTest]) {self}};", "T(*obj[+tplTest])", "T ( * obj [ + tplTest ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:*obj[y:*templateTest]) {self}};", "T(*obj[*templateTest])", "T ( * obj [ * templateTest ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj[y:Nat,z:+templateTest]) {self}};", "T(+obj[Nat, +templateTest])", "T ( + obj [ Nat + templateTest ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:+obj[y:index,z:*Nat]) {self}};", "T(+obj[ind, *Nat])", "T ( + obj [ ind * Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:*obj[y:*obj,z:+@Nat]) {self}};", "T(*obj[*obj, +@Nat])", "T ( * obj [ * obj + @Nat ] )")>]
    [<DataRow("def cl T:obj {ctor T(x:*obj[y:+tpl,z:index]) {self}};", "T(*obj[+tpl, ind])", "T ( * obj [ + tpl ind ] )")>]

    [<TestMethod>]
    member this.TestTypeSignatureOfConstructors(fplCode:string, expectedName:string, expectedTypeSignatureStr:string) =
        let expectedTypeSignature = expectedTypeSignatureStr.Split(' ') |> List.ofArray
        let result = prepareFplCode(fplCode, false) 
        let fplValue = result.Value.Root.Scope["Test"].Scope["T"].Scope[expectedName]
        let actualTypeSignature = fplValue.TypeSignature
        let actualSignatureStart = fplValue.StartPos.Index
        let actualSignatureEnd = fplValue.NameEndPos.Index
        Assert.AreEqual(expectedTypeSignature, actualTypeSignature)
        let expectedStart =
                (int64)14
        Assert.AreEqual(expectedStart, actualSignatureStart)
        let expectedEnd =
                (int64)(fplCode.IndexOf(" {self", System.StringComparison.OrdinalIgnoreCase))
        Assert.AreEqual(expectedEnd, actualSignatureEnd)
        prepareFplCode("", true) |> ignore