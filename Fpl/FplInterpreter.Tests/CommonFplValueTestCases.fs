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

    static member ScopeVariablesInSignatureVariadic() =
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

    static member ScopeVariablesInBlockVariadic() =
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
            prty pred opt T2() {intr}
            prty func T3()->obj {intr}
            prty func opt T4()->obj {intr}
            prty func T5()->ind {intr}
            prty func opt T6()->ind {intr}
            prty func T7()->pred {intr}
            prty func opt T8()->pred {intr}
            prty func T9()->tpl {intr}
            prty func opt T10()->tpl {intr}
            prty func T11()->Nat {intr}
            prty func opt T12()->Nat {intr}
            prty func T13()->func {intr}
            prty func opt T14()->func {intr}
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
                            let t5 = block.Scope["T5() -> ind"]
                            let t6 = block.Scope["T6() -> ind"]
                            let t7 = block.Scope["T7() -> pred"]
                            let t8 = block.Scope["T8() -> pred"]
                            let t9 = block.Scope["T9() -> tpl"]
                            let t10 = block.Scope["T10() -> tpl"]
                            let t11 = block.Scope["T11() -> Nat"]
                            let t12 = block.Scope["T12() -> Nat"]
                            let t13 = block.Scope["T13() -> func"]
                            let t14 = block.Scope["T14() -> func"]
                            Some (r,theory,block,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14)
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
            
            proposition TestProposition2() {true} 
            corollary TestProposition2$1() {true}  

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
                            let prp2 = theory.Scope["TestProposition2()"]
                            let corPrp2 = prp2.Scope["TestProposition2$1()"]
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

    static member ScopeCallConstructorParentClass() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """

            def cl B:obj {intr}
            def cl C:obj {intr}
            def cl D:obj {intr}

            def cl A:B,C,D,E
            {
                ctor A(a:T1, b:func, c:ind, d:pred) 
                {
                    dec
                        base.B()
                        base.C(a,b,c,d)
                        base.D(self,b,c)
                        base.B(In(x))
                        base.C(Test1(a),Test2(b,c,d))
                        base.E(true, undef, false)
                    ;
                    self
                }
            }
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let cl = theory.Scope["A"]
                            let ctor = cl.Scope["A(T1, func, ind, pred)"]
                            let base1 = ctor.Scope["__bas.B()"]
                            let base2 = ctor.Scope["__bas.C(a, b, c, d)"]
                            let base3 = ctor.Scope["__bas.D(self, a, b)"]
                            let base4 = ctor.Scope["__bas.B(In(x))"]
                            let base5 = ctor.Scope["__bas.C(Test1(a), Test2(b, c, d))"]
                            let base6 = ctor.Scope["__bas.E(true, undef, false)"]
                            Some (base1,base2,base3,base4,base5,base6)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    static member ScopeDelegate() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """

            def pred TestPredicate(a:T1, b:func, c:ind, d:pred) 
            {
                true

                property pred T1() 
                {
                    delegate.B()
                }
                property pred T2() 
                {
                    delegate.C(a,b,c,d)
                }
                property pred T3() 
                {
                    delegate.D(self,b,c)
                }
                property pred T4() 
                {
                    delegate.B(In(x))
                }
                property pred T5() 
                {
                    delegate.C(Test1(a),Test2(b,c,d))
                }
                property pred T6() 
                {
                    delegate.E(true, undef, false)
                }

            }
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let pr = theory.Scope["TestPredicate(T1, func, ind, pred)"]
                            let pr1 = pr.Scope["T1()"]
                            let pr2 = pr.Scope["T2()"]
                            let pr3 = pr.Scope["T3()"]
                            let pr4 = pr.Scope["T4()"]
                            let pr5 = pr.Scope["T5()"]
                            let pr6 = pr.Scope["T6()"]
                            let base1 = pr1.Scope["__del.B()"]
                            let base2 = pr2.Scope["__del.C(a, b, c, d)"]
                            let base3 = pr3.Scope["__del.D(self, a, b)"]
                            let base4 = pr4.Scope["__del.B(In(x))"]
                            let base5 = pr5.Scope["__del.C(Test1(a), Test2(b, c, d))"]
                            let base6 = pr6.Scope["__del.E(true, undef, false)"]
                            Some (base1,base2,base3,base4,base5,base6)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    static member ScopePredicateWithArguments() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """

            def pred TestPredicate(a:T1, b:func, c:ind, d:pred) 
            {
                true

                property pred T1() 
                {
                    B()
                }
                property pred T2() 
                {
                    C(a,b,c,d)
                }
                property pred T3() 
                {
                    D(self,b,c)
                }
                property pred T4() 
                {
                    B(In(x))
                }
                property pred T5() 
                {
                    C(Test1(a),Test2(b,c,d))
                }
                property pred T6() 
                {
                    E(true, undef, false)
                }

            }
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let pr = theory.Scope["TestPredicate(T1, func, ind, pred)"]
                            let pr1 = pr.Scope["T1()"]
                            let pr2 = pr.Scope["T2()"]
                            let pr3 = pr.Scope["T3()"]
                            let pr4 = pr.Scope["T4()"]
                            let pr5 = pr.Scope["T5()"]
                            let pr6 = pr.Scope["T6()"]
                            let base1 = pr.Scope["B()"]
                            let base2 = pr.Scope["C(a, b, c, d)"]
                            let base3 = pr.Scope["D(self, a, b)"]
                            let base4 = pr.Scope["B(In(x))"]
                            let base5 = pr.Scope["C(Test1(a), Test2(b, c, d))"]
                            let base6 = pr.Scope["E(true, undef, false)"]
                            Some (base1,base2,base3,base4,base5,base6)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    static member ScopeFixNotation() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """

            def pred infix ">" -1 T1() 
            {
                intr
            }

            def pred postfix "'" T2() 
            {
                intr
            }

            def pred prefix "-" T3() 
            {
                intr
            }

            def cl symbol "∅" T4:obj 
            {
                intr
            }

            def pred T5() 
            {
                intr
            }

        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let base1 = theory.Scope["T1()"]
                            let base2 = theory.Scope["T2()"]
                            let base3 = theory.Scope["T3()"]
                            let base4 = theory.Scope["T4"]
                            let base5 = theory.Scope["T5()"]
                            Some (base1,base2,base3,base4,base5)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result


    static member ScopePredicate() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """

            def pred T1() 
            {
                true
            }

            def pred T2() 
            {
                false
            }

            def pred T3() 
            {
                undef
            }

            def pred T4() 
            {
                1.
            }

            def pred T5() 
            {
                del.Test()
            }

            def pred T6() 
            {
                $1
            }

            def pred T7() 
            {
                bydef Test()
            } 

            def pred T8() 
            {
                Test$1
            }
            
            def pred T9() 
            {
                Test$1()
            }
            
            def pred T10() 
            {
                Test
            }

            def pred T11() 
            {
                v
            }

            def pred T12() 
            {
                self
            }

            def pred T13() 
            {
                1
            }


            def pred T11a() 
            {
                v.x
            }

            def pred T12a() 
            {
                self.x
            }

            def pred T10b() 
            {
                Test()
            }

            def pred T11b() 
            {
                v()
            }

            def pred T12b() 
            {
                self()
            }

            def pred T13b() 
            {
                1()  
            }

            def pred T10c() 
            {
                Test(x,y)
            }

            def pred T11c() 
            {
                v(x,y)
            }

            def pred T12c() 
            {
                self(x,y)
            }

            def pred T13c() 
            {
                1(x,y)  
            }

            def pred T10d() 
            {
                Test[x,y]
            }

            def pred T11d() 
            {
                v[x,y]
            }

            def pred T12d() 
            {
                self[x,y]
            }

            def pred T13d() 
            {
                1[x.y]  
            }

            def pred T10e() 
            {
                Test(x,y).@self[a,b]
            }

            def pred T11e() 
            {
                v(x,y).x[a,b]
            }

            def pred T12e() 
            {
                self(x,y).3[a,b]
            }

            def pred T13e() 
            {
                1(x,y).T[a,b]
            }

            def pred T10f() 
            {
                Test[x,y].x(a,b)
            }

            def pred T11f() 
            {
                v[x,y].x(a,b)
            }

            def pred T12f() 
            {
                self[x,y].self(a,b)
            }

            def pred T13f() 
            {
                1[x.y].T(a,b)  
            }

            def pred T14()
            {
                ∅
            }

            def pred T15()
            {
                -x
            }

            def pred T16()
            {
                -(y + x = 2 * x)
            }

            def pred T17()
            {
                (y + x' = 2 * x)'
            }

            def pred T18()
            {
                ex x in Range(a,b), y in c, z {and (a,b,c)}
            }

            def pred T19()
            {
                exn$1 x {all y {true}}
            }

            def pred T20()
            {
                all x {not x}
            }

            def pred T21()
            {
                and (x,y,z)
            }

            def pred T22()
            {
                xor (x,y,z)
            }

            def pred T23()
            {
                or (x,y,z)
            }

            def pred T24()
            {
                iif (x,y)
            }

            def pred T25()
            {
                impl (x,y)
            }

            def pred T26()
            {
                is (x,Nat)
            }

        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = r.Scope["Test"]

                            let pr1 = theory.Scope["T1()"] 
                            let base1 = pr1.Scope["true"]

                            let pr2 = theory.Scope["T2() "]
                            let base2 = pr2.Scope["false"]

                            let pr3 = theory.Scope["T3()"]
                            let base3 = pr3.Scope["undef"]

                            let pr4 = theory.Scope["T4()"] 
                            let base4 = pr4.Scope["1."]

                            let pr5 = theory.Scope["T5()"] 
                            let base5 = pr5.Scope["del.Test()"]

                            let pr6 = theory.Scope["T6()"] 
                            let base6 = pr6.Scope["$1"]

                            let pr7 = theory.Scope["T7()"] 
                            let base7 = pr7.Scope["bydef Test()"] 

                            let pr8 = theory.Scope["T8()"] 
                            let base8 = pr8.Scope["Test$1"]
            
                            let pr9 = theory.Scope["T9()"] 
                            let base9 = pr9.Scope["Test$1()"]
            
                            let pr10 = theory.Scope["T10()"] 
                            let base10 = pr10.Scope["Test"]

                            let pr11 = theory.Scope["T11()"] 
                            let base11 = pr11.Scope["v"]

                            let pr12 = theory.Scope["T12()"] 
                            let base12 = pr12.Scope["self"]

                            let pr13 = theory.Scope["T13()"] 
                            let base13 = pr13.Scope["1"]

                            let pr11a = theory.Scope["T11a()"] 
                            let base11a = pr11a.Scope["v.x"]

                            let pr12a = theory.Scope["T12a()"] 
                            let base12a = pr12a.Scope["self.x"]

                            let pr10b = theory.Scope["T10b()"] 
                            let base10b = pr10b.Scope["Test()"]

                            let pr11b = theory.Scope["T11b()"] 
                            let base11b = pr11b.Scope["v()"]

                            let pr12b = theory.Scope["T12b()"] 
                            let base12b = pr12b.Scope["self()"]

                            let pr13b = theory.Scope["T13b()"] 
                            let base13b = pr13b.Scope["1()"]

                            let pr10c = theory.Scope["T10c()"] 
                            let base10c = pr10c.Scope["Test(x,y)"]

                            let pr11c = theory.Scope["T11c()"] 
                            let base11c = pr11c.Scope["v(x,y)"]

                            let pr12c = theory.Scope["T12c()"] 
                            let base12c = pr12c.Scope["self(x,y)"]

                            let pr13c = theory.Scope["T13c()"] 
                            let base13c = pr13c.Scope["1(x,y)"]

                            let pr10d = theory.Scope["T10d()"] 
                            let base10d = pr10d.Scope["Test[x,y]"]

                            let pr11d = theory.Scope["T11d()"] 
                            let base11d = pr11d.Scope["v[x,y]"]

                            let pr12d = theory.Scope["T12d()"] 
                            let base12d = pr12d.Scope["self[x,y]"]

                            let pr13d = theory.Scope["T13d()"] 
                            let base13d = pr13d.Scope["1[x.y]"]

                            let pr10e = theory.Scope["T10e()"] 
                            let base10e = pr10e.Scope["Test(x,y).@self[a,b]"]

                            let pr11e = theory.Scope["T11e()"] 
                            let base11e = pr11e.Scope["v(x,y).x[a,b]"]

                            let pr12e = theory.Scope["T12e()"] 
                            let base12e = pr12e.Scope["self(x,y).3[a,b]"]

                            let pr13e = theory.Scope["T13e()"] 
                            let base13e = pr13e.Scope["1(x,y).T[a,b]"]

                            let pr10f = theory.Scope["T10f()"] 
                            let base10f = pr10f.Scope["Test[x,y].x(a,b)"]

                            let pr11f = theory.Scope["T11f()"] 
                            let base11f = pr11f.Scope["v[x,y].x(a,b)"]

                            let pr12f = theory.Scope["T12f()"] 
                            let base12f = pr12f.Scope["self[x,y].self(a,b)"]

                            let pr13f = theory.Scope["T13f()"] 
                            let base13f = pr13f.Scope["1[x.y].T(a,b)"]

                            let pr14 = theory.Scope["T14()"]
                            let base14 = pr14.Scope["∅"]

                            let pr15 = theory.Scope["T15()"]
                            let base15 = pr15.Scope["-x"]

                            let pr16 = theory.Scope["T16()"]
                            let base16 = pr16.Scope["-(y + x = 2 * x)"]

                            let pr17 = theory.Scope["T17()"]
                            let base17 = pr17.Scope["(y + x' = 2 * x)'"]

                            let pr18 = theory.Scope["T18()"]
                            let base18 = pr18.Scope["ex x in Range(a,b), y in c, z {and (a,b,c)}"]

                            let pr19 = theory.Scope["T19()"]
                            let base19 = pr19.Scope["exn$1 x {all y {true}}"]

                            let pr20 = theory.Scope["T20()"]
                            let base20 = pr20.Scope["all x {not x}"]

                            let pr21 = theory.Scope["T21()"]
                            let base21 = pr21.Scope["and (x,y,z)"]

                            let pr22 = theory.Scope["T22()"]
                            let base22 = pr22.Scope["xor (x,y,z)"]

                            let pr23 = theory.Scope["T23()"]
                            let base23 = pr23.Scope["or (x,y,z)"]

                            let pr24 = theory.Scope["T24()"]
                            let base24 = pr24.Scope["iif (x,y)"]

                            let pr25 = theory.Scope["T25()"]
                            let base25 = pr25.Scope["impl (x,y)"]

                            let pr26 = theory.Scope["T26()"]
                            let base26 = pr26.Scope["is (x,Nat)"]

                            Some (theory, base1,base2,base3,base4,base5, base6, base7, 
                                    base8, base9, base10, base11, base12, base13,
                                    base11a, base12a, base10b, base11b, base12b, base13b,
                                    base10c, base11c, base12c, base13c, base10d, base11d,
                                    base12d, base10e, base11e, base12e, base13d, base13e,
                                    base10f, base11f, base12f, base13f, base14, base15,
                                    base16, base17, base18, base19, base20, base21, base22,
                                    base23, base24, base25, base26)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result
