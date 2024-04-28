namespace FplInterpreter.Tests

open CommonTestHelpers

type CommonFplValueTestCases =

    static member ScopeVariablesInSignature() =
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

    static member ScopeVariablesInSignatureWithVariadic() =
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

    static member ScopeVariablesInBlock() =
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

    static member ScopeVariablesInBlockWithVariadic() =
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

    static member ScopeProperties() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestId() 
        {
            intr 
            prty pred T1() {intr}
            prty opt pred T2() {intr}
            prty func T3()->obj {intr}
            prty opt func T4()->obj {intr}
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

    static member ScopeConstructors() =
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

    static member ScopeBlocks() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
            inf SomeInference1() {pre:true con:true}
            inf SomeInference2() {pre:true con:true}
            axiom SomeAxiom1() {true}
            axiom SomeAxiom2() {true}
            postulate SomePostulate1() {true}
            postulate SomePostulate2() {true}
            theorem SomeTheorem1() {true}
            theorem SomeTheorem2() {true}
            proposition SomeProposition1() {true}
            proposition SomeProposition2() {true}
            lemma SomeLemma1() {true}
            lemma SomeLemma2() {true}
            corollary SomeLemma1$1() {true}
            corollary SomeLemma2$1() {true}
            conjecture SomeConjecture1() {true}
            conjecture SomeConjecture2() {true}
            def cl SomeClass1:obj {intr}
            def cl SomeClass2:obj {intr}
            def pred SomePredicate1() {intr}
            def pred SomePredicate2() {intr}
            def func SomeFunctionalTerm1()->obj {intr}
            def func SomeFunctionalTerm2()->obj {intr}
            proof SomeTheorem1$1 {1. |- trivial}
            proof SomeTheorem2$1 {1. |- trivial}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let inf1 = theory.Scope["SomeInference1()"]
                            let inf2 = theory.Scope["SomeInference2()"]
                            let axi1 = theory.Scope["SomeAxiom1()"]
                            let axi2 = theory.Scope["SomeAxiom2()"]
                            let pst1 = theory.Scope["SomePostulate1()"]
                            let pst2 = theory.Scope["SomePostulate2()"]
                            let thm1 = theory.Scope["SomeTheorem1()"]
                            let thm2 = theory.Scope["SomeTheorem2()"]
                            let pro1 = theory.Scope["SomeProposition1()"]
                            let pro2 = theory.Scope["SomeProposition2()"]
                            let lem1 = theory.Scope["SomeLemma1()"]
                            let lem2 = theory.Scope["SomeLemma2()"]
                            let cor1 = lem1.Scope["SomeLemma1$1()"]
                            let cor2 = lem2.Scope["SomeLemma2$1()"]
                            let con1 = theory.Scope["SomeConjecture1()"]
                            let con2 = theory.Scope["SomeConjecture2()"]
                            let cla1 = theory.Scope["SomeClass1"]
                            let cla2 = theory.Scope["SomeClass2"]
                            let pre1 = theory.Scope["SomePredicate1()"]
                            let pre2 = theory.Scope["SomePredicate2()"]
                            let fun1 = theory.Scope["SomeFunctionalTerm1() -> obj"]
                            let fun2 = theory.Scope["SomeFunctionalTerm2() -> obj"]
                            let prf1 = thm1.Scope["SomeTheorem1$1"]
                            let prf2 = thm2.Scope["SomeTheorem2$1"]
                            Some (r,theory,inf1,inf2,axi1,axi2,pst1,pst2,thm1,thm2,pro1,pro2,lem1,lem2,cor1,cor2,con1,con2,cla1,cla2,pre1,pre2,fun1,fun2,prf1,prf2)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    static member ScopeProofsAndCorollaries() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """

            theorem TestTheorem1() {true} 
            proof TestTheorem1$1 {1. |- trivial} 
            
            lemma TestLemma1() {true} 
            proof TestLemma1$1 {1. |- trivial} 
            
            proposition TestProposition1() {true} 
            proof TestProposition1$1 {1. |- trivial} 
            
            corollary TestCorollary1$2() {true} 
            proof TestCorollary1$2$1 {1. |- trivial} 

            theorem TestTheorem2() {true} 
            corollary TestTheorem2$1() {true}  
            
            lemma TestLemma2() {true} 
            corollary TestLemma2$1() {true}  
            
            proposition TestPropositions2() {true} 
            corollary TestPropositions2$1() {true}  

            corollary TestCorollary2$2() {true} 
            corollary TestCorollary2$2$1() {true}  

            conjecture TestConjecture() {true} 
            corollary TestConjecture$1() {true}  

            axiom TestAxiom() {true} 
            corollary TestAxiom$1() {true}  
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let thm1 = theory.Scope["TestTheorem1()"]
                            let proofThm1 = thm1.Scope["TestTheorem1$1"]
                            let lem1 = theory.Scope["TestLemma1()"]
                            let proofLem1 = lem1.Scope["TestLemma1$1"]
                            let prp1 = theory.Scope["TestProposition1()"]
                            let proofPrp1 = prp1.Scope["TestProposition1$1"]
                            let cor1 = theory.Scope["TestCorollary1$2()"]
                            let proofCor1 = cor1.Scope["TestCorollary1$2$1"]
                            let thm2 = theory.Scope["TestTheorem2()"]
                            let corThm2 = thm2.Scope["TestTheorem2$1()"]
                            let lem2 = theory.Scope["TestLemma2()"]
                            let corLem2 = lem2.Scope["TestLemma2$1()"]
                            let prp2 = theory.Scope["TestPropositions2()"]
                            let corPrp2 = prp2.Scope["TestPropositions2$1()"]
                            let cor2 = theory.Scope["TestCorollary2$2()"]
                            let corCor2 = cor2.Scope["TestCorollary2$2$1()"]
                            let con1 = theory.Scope["TestConjecture()"]
                            let corCon1 = con1.Scope["TestConjecture$1()"]
                            let axi1 = theory.Scope["TestAxiom()"]
                            let corAxi1 = axi1.Scope["TestAxiom$1()"]
                            Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result
