namespace FplInterpreter.Tests

open CommonTestHelpers
open ErrDiagnostics
open FParsec

type CommonFplValueTestCases =

    static member getScopedElement (fv:FplInterpreterTypes.FplValue) name =
        if fv.Scope.ContainsKey(name) then 
            fv.Scope[name]
        elif fv.Scope.Count>0 then
            let kv = fv.Scope |> Seq.head
            kv.Value
        else
            let pos = Position("",(int64)1,(int64)1,(int64)0)
            FplInterpreterTypes.FplValue.CreateRoot()

    static member ScopeVariablesInSignature(subtype) =
        ad.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:pred(u,v,w:func(a,b,c:obj)->obj)) 
            {true}
        ;
        """
        let filename = "TestScopeVariablesInSignature" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let block = CommonFplValueTestCases.getScopedElement theory name
                            let x = CommonFplValueTestCases.getScopedElement block "x"
                            let y = CommonFplValueTestCases.getScopedElement block "y"
                            let xw = CommonFplValueTestCases.getScopedElement x "w"
                            let xu = CommonFplValueTestCases.getScopedElement x "u"
                            let xv = CommonFplValueTestCases.getScopedElement x "v"
                            let yw = CommonFplValueTestCases.getScopedElement y "w"
                            let yu = CommonFplValueTestCases.getScopedElement y "u"
                            let yv = CommonFplValueTestCases.getScopedElement y "v"
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a"
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b"
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c"
                            let xua = CommonFplValueTestCases.getScopedElement xu "a"
                            let xub = CommonFplValueTestCases.getScopedElement xu "b"
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c"
                            let xva = CommonFplValueTestCases.getScopedElement xv "a"
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b"
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c"
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a"
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b"
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c"
                            let yua = CommonFplValueTestCases.getScopedElement yu "a"
                            let yub = CommonFplValueTestCases.getScopedElement yu "b"
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c"
                            let yva = CommonFplValueTestCases.getScopedElement yv "a"
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b"
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c"
                            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeVariablesInSignatureVariadic(subtype) =
        ad.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:+pred(u,v,w:func(a,b,c:*obj)->obj)) 
            {true}
        ;
        """
        let filename = "TestScopeVariablesInSignatureVariadic" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let block = CommonFplValueTestCases.getScopedElement theory name
                            let x = CommonFplValueTestCases.getScopedElement block "x"
                            let y = CommonFplValueTestCases.getScopedElement block "y"
                            let xw = CommonFplValueTestCases.getScopedElement x "w"
                            let xu = CommonFplValueTestCases.getScopedElement x "u"
                            let xv = CommonFplValueTestCases.getScopedElement x "v"
                            let yw = CommonFplValueTestCases.getScopedElement y "w"
                            let yu = CommonFplValueTestCases.getScopedElement y "u"
                            let yv = CommonFplValueTestCases.getScopedElement y "v"
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a"
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b"
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c"
                            let xua = CommonFplValueTestCases.getScopedElement xu "a"
                            let xub = CommonFplValueTestCases.getScopedElement xu "b"
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c"
                            let xva = CommonFplValueTestCases.getScopedElement xv "a"
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b"
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c"
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a"
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b"
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c"
                            let yua = CommonFplValueTestCases.getScopedElement yu "a"
                            let yub = CommonFplValueTestCases.getScopedElement yu "b"
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c"
                            let yva = CommonFplValueTestCases.getScopedElement yv "a"
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b"
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c"
                            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeVariablesInBlock(subtype) =
        ad.Clear()
        let fplCode = """
        def pred TestPredicate() 
        {   dec 
                ~x,y:pred(u,v,w:func(a,b,c:obj)->obj)
                ~s:Set
            ; 
            true
        }
        ;
        """
        let filename = "TestScopeVariablesInBlock" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate()"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let block = CommonFplValueTestCases.getScopedElement theory name
                            let x = CommonFplValueTestCases.getScopedElement block "x"
                            let y = CommonFplValueTestCases.getScopedElement block "y"
                            let s = CommonFplValueTestCases.getScopedElement block "s"
                            let xw = CommonFplValueTestCases.getScopedElement x "w"
                            let xu = CommonFplValueTestCases.getScopedElement x "u"
                            let xv = CommonFplValueTestCases.getScopedElement x "v"
                            let yw = CommonFplValueTestCases.getScopedElement y "w"
                            let yu = CommonFplValueTestCases.getScopedElement y "u"
                            let yv = CommonFplValueTestCases.getScopedElement y "v"
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a"
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b"
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c"
                            let xua = CommonFplValueTestCases.getScopedElement xu "a"
                            let xub = CommonFplValueTestCases.getScopedElement xu "b"
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c"
                            let xva = CommonFplValueTestCases.getScopedElement xv "a"
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b"
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c"
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a"
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b"
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c"
                            let yua = CommonFplValueTestCases.getScopedElement yu "a"
                            let yub = CommonFplValueTestCases.getScopedElement yu "b"
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c"
                            let yva = CommonFplValueTestCases.getScopedElement yv "a"
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b"
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c"
                            Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeVariablesInBlockVariadic(subtype) =
        ad.Clear()
        let fplCode = """
        def pred TestPredicate() 
            {dec ~x,y:+pred(u,v,w:func(a,b,c:*obj)->obj); true}
        ;
        """
        let filename = "TestScopeVariablesInBlockVariadic" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate()"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let block = CommonFplValueTestCases.getScopedElement theory name
                            let x = CommonFplValueTestCases.getScopedElement block "x"
                            let y = CommonFplValueTestCases.getScopedElement block "y"
                            let xw = CommonFplValueTestCases.getScopedElement x "w"
                            let xu = CommonFplValueTestCases.getScopedElement x "u"
                            let xv = CommonFplValueTestCases.getScopedElement x "v"
                            let yw = CommonFplValueTestCases.getScopedElement y "w"
                            let yu = CommonFplValueTestCases.getScopedElement y "u"
                            let yv = CommonFplValueTestCases.getScopedElement y "v"
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a"
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b"
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c"
                            let xua = CommonFplValueTestCases.getScopedElement xu "a"
                            let xub = CommonFplValueTestCases.getScopedElement xu "b"
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c"
                            let xva = CommonFplValueTestCases.getScopedElement xv "a"
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b"
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c"
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a"
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b"
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c"
                            let yua = CommonFplValueTestCases.getScopedElement yu "a"
                            let yub = CommonFplValueTestCases.getScopedElement yu "b"
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c"
                            let yva = CommonFplValueTestCases.getScopedElement yv "a"
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b"
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c"
                            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeProperties(subtype) =
        ad.Clear()
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
        let filename = "TestScopeProperties" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestId()"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let block = CommonFplValueTestCases.getScopedElement theory name
                            let t1 = CommonFplValueTestCases.getScopedElement block "T1()"
                            let t2 = CommonFplValueTestCases.getScopedElement block "T2()"
                            let t3 = CommonFplValueTestCases.getScopedElement block "T3() -> obj"
                            let t4 = CommonFplValueTestCases.getScopedElement block "T4() -> obj"
                            let t5 = CommonFplValueTestCases.getScopedElement block "T5() -> ind"
                            let t6 = CommonFplValueTestCases.getScopedElement block "T6() -> ind"
                            let t7 = CommonFplValueTestCases.getScopedElement block "T7() -> pred"
                            let t8 = CommonFplValueTestCases.getScopedElement block "T8() -> pred"
                            let t9 = CommonFplValueTestCases.getScopedElement block "T9() -> tpl"
                            let t10 = CommonFplValueTestCases.getScopedElement block "T10() -> tpl"
                            let t11 = CommonFplValueTestCases.getScopedElement block "T11() -> Nat"
                            let t12 = CommonFplValueTestCases.getScopedElement block "T12() -> Nat"
                            let t13 = CommonFplValueTestCases.getScopedElement block "T13() -> func"
                            let t14 = CommonFplValueTestCases.getScopedElement block "T14() -> func"
                            Some (r,theory,block,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeConstructors(subtype) =
        ad.Clear()
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
        let filename = "TestScopeConstructors" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestId"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let block = CommonFplValueTestCases.getScopedElement theory name
                            let t1 = CommonFplValueTestCases.getScopedElement block "TestId()"
                            let t2 = CommonFplValueTestCases.getScopedElement block "TestId(obj)"
                            let t3 = CommonFplValueTestCases.getScopedElement block "TestId(pred)"
                            let t4 = CommonFplValueTestCases.getScopedElement block "TestId(ind)"
                            Some (r,theory,block,t1,t2,t3,t4)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeBlocks(subtype) =
        ad.Clear()
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
            loc not(x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;
            loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;
        ;
        """
        let filename = "TestScopeBlocks" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 

        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let inf1 = CommonFplValueTestCases.getScopedElement theory "SomeInference1()"
                            let inf2 = CommonFplValueTestCases.getScopedElement theory "SomeInference2()"
                            let axi1 = CommonFplValueTestCases.getScopedElement theory "SomeAxiom1()"
                            let axi2 = CommonFplValueTestCases.getScopedElement theory "SomeAxiom2()"
                            let pst1 = CommonFplValueTestCases.getScopedElement theory "SomePostulate1()"
                            let pst2 = CommonFplValueTestCases.getScopedElement theory "SomePostulate2()"
                            let thm1 = CommonFplValueTestCases.getScopedElement theory "SomeTheorem1()"
                            let thm2 = CommonFplValueTestCases.getScopedElement theory "SomeTheorem2()"
                            let pro1 = CommonFplValueTestCases.getScopedElement theory "SomeProposition1()"
                            let pro2 = CommonFplValueTestCases.getScopedElement theory "SomeProposition2()"
                            let lem1 = CommonFplValueTestCases.getScopedElement theory "SomeLemma1()"
                            let lem2 = CommonFplValueTestCases.getScopedElement theory "SomeLemma2()"
                            let cor1 = CommonFplValueTestCases.getScopedElement lem1 "SomeLemma1$1()"
                            let cor2 = CommonFplValueTestCases.getScopedElement lem2 "SomeLemma2$1()"
                            let con1 = CommonFplValueTestCases.getScopedElement theory "SomeConjecture1()"
                            let con2 = CommonFplValueTestCases.getScopedElement theory "SomeConjecture2()"
                            let cla1 = CommonFplValueTestCases.getScopedElement theory "SomeClass1"
                            let cla2 = CommonFplValueTestCases.getScopedElement theory "SomeClass2"
                            let pre1 = CommonFplValueTestCases.getScopedElement theory "SomePredicate1()"
                            let pre2 = CommonFplValueTestCases.getScopedElement theory "SomePredicate2()"
                            let fun1 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm1() -> obj"
                            let fun2 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm2() -> obj"
                            let prf1 = CommonFplValueTestCases.getScopedElement thm1 "SomeTheorem1$1"
                            let prf2 = CommonFplValueTestCases.getScopedElement thm2 "SomeTheorem2$1"
                            let loc1 = CommonFplValueTestCases.getScopedElement theory "not(x)"
                            let loc2 = CommonFplValueTestCases.getScopedElement theory "Equal(x, y)"
                            Some (r,theory,inf1,inf2,axi1,axi2,pst1,pst2,thm1,thm2,pro1,pro2,lem1,lem2,cor1,cor2,con1,con2,cla1,cla2,pre1,pre2,fun1,fun2,prf1,prf2,loc1,loc2)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeProofsAndCorollaries(subtype) =
        ad.Clear()
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
        let filename = "TestScopeProofsAndCorollaries" + subtype

        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename
                            let thm1 = CommonFplValueTestCases.getScopedElement theory "TestTheorem1()"
                            let proofThm1 = CommonFplValueTestCases.getScopedElement thm1 "TestTheorem1$1"
                            let lem1 = CommonFplValueTestCases.getScopedElement theory "TestLemma1()"
                            let proofLem1 = CommonFplValueTestCases.getScopedElement lem1 "TestLemma1$1"
                            let prp1 = CommonFplValueTestCases.getScopedElement theory "TestProposition1()"
                            let proofPrp1 = CommonFplValueTestCases.getScopedElement prp1 "TestProposition1$1"
                            let cor1 = CommonFplValueTestCases.getScopedElement theory "TestCorollary1$2()"
                            let proofCor1 = CommonFplValueTestCases.getScopedElement cor1 "TestCorollary1$2$1"
                            let thm2 = CommonFplValueTestCases.getScopedElement theory "TestTheorem2()"
                            let corThm2 = CommonFplValueTestCases.getScopedElement thm2 "TestTheorem2$1()"
                            let lem2 = CommonFplValueTestCases.getScopedElement theory "TestLemma2()"
                            let corLem2 = CommonFplValueTestCases.getScopedElement lem2 "TestLemma2$1()"
                            let prp2 = CommonFplValueTestCases.getScopedElement theory "TestProposition2()"
                            let corPrp2 = CommonFplValueTestCases.getScopedElement prp2 "TestProposition2$1()"
                            let cor2 = CommonFplValueTestCases.getScopedElement theory "TestCorollary2$2()"
                            let corCor2 = CommonFplValueTestCases.getScopedElement cor2 "TestCorollary2$2$1()"
                            let con1 = CommonFplValueTestCases.getScopedElement theory "TestConjecture()"
                            let corCon1 = CommonFplValueTestCases.getScopedElement con1 "TestConjecture$1()"
                            let axi1 = CommonFplValueTestCases.getScopedElement theory "TestAxiom()"
                            let corAxi1 = CommonFplValueTestCases.getScopedElement axi1 "TestAxiom$1()"
                            Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result
