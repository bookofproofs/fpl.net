namespace FplInterpreter.Tests

open CommonTestHelpers
open ErrDiagnostics
open FplInterpreterTypes
open FParsec


type CommonFplValueTestCases =

    static member getScopedElement (fv:FplInterpreterTypes.FplValue) name subtype =
        if subtype <> "" then
            if fv.Scope.ContainsKey(name) then 
                fv.Scope[name]
            elif fv.Scope.Count>0 then
                let kv = fv.Scope |> Seq.head
                kv.Value
            else
                new FplInterpreterTypes.FplRoot()
        else
            fv.Scope[name]

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
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let block = CommonFplValueTestCases.getScopedElement theory name subtype
                            let x = CommonFplValueTestCases.getScopedElement block "x" subtype
                            let y = CommonFplValueTestCases.getScopedElement block "y" subtype
                            let xw = CommonFplValueTestCases.getScopedElement x "w" subtype
                            let xu = CommonFplValueTestCases.getScopedElement x "u" subtype 
                            let xv = CommonFplValueTestCases.getScopedElement x "v" subtype
                            let yw = CommonFplValueTestCases.getScopedElement y "w" subtype
                            let yu = CommonFplValueTestCases.getScopedElement y "u" subtype 
                            let yv = CommonFplValueTestCases.getScopedElement y "v" subtype
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a" subtype
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b" subtype
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c" subtype
                            let xua = CommonFplValueTestCases.getScopedElement xu "a" subtype
                            let xub = CommonFplValueTestCases.getScopedElement xu "b" subtype
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c" subtype
                            let xva = CommonFplValueTestCases.getScopedElement xv "a" subtype
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b" subtype
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c" subtype
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a" subtype
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b" subtype
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c" subtype
                            let yua = CommonFplValueTestCases.getScopedElement yu "a" subtype
                            let yub = CommonFplValueTestCases.getScopedElement yu "b" subtype
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c" subtype
                            let yva = CommonFplValueTestCases.getScopedElement yv "a" subtype
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b" subtype
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c" subtype
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
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let block = CommonFplValueTestCases.getScopedElement theory name subtype
                            let x = CommonFplValueTestCases.getScopedElement block "x" subtype
                            let y = CommonFplValueTestCases.getScopedElement block "y" subtype
                            let xw = CommonFplValueTestCases.getScopedElement x "w" subtype
                            let xu = CommonFplValueTestCases.getScopedElement x "u" subtype
                            let xv = CommonFplValueTestCases.getScopedElement x "v" subtype
                            let yw = CommonFplValueTestCases.getScopedElement y "w" subtype
                            let yu = CommonFplValueTestCases.getScopedElement y "u" subtype
                            let yv = CommonFplValueTestCases.getScopedElement y "v" subtype
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a" subtype
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b" subtype
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c" subtype
                            let xua = CommonFplValueTestCases.getScopedElement xu "a" subtype
                            let xub = CommonFplValueTestCases.getScopedElement xu "b" subtype
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c" subtype
                            let xva = CommonFplValueTestCases.getScopedElement xv "a" subtype
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b" subtype
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c" subtype
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a" subtype
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b" subtype
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c" subtype
                            let yua = CommonFplValueTestCases.getScopedElement yu "a" subtype
                            let yub = CommonFplValueTestCases.getScopedElement yu "b" subtype
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c" subtype
                            let yva = CommonFplValueTestCases.getScopedElement yv "a" subtype
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b" subtype
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c" subtype
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
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let block = CommonFplValueTestCases.getScopedElement theory name subtype
                            let x = CommonFplValueTestCases.getScopedElement block "x" subtype
                            let y = CommonFplValueTestCases.getScopedElement block "y" subtype
                            let s = CommonFplValueTestCases.getScopedElement block "s" subtype
                            let xw = CommonFplValueTestCases.getScopedElement x "w" subtype
                            let xu = CommonFplValueTestCases.getScopedElement x "u" subtype
                            let xv = CommonFplValueTestCases.getScopedElement x "v" subtype
                            let yw = CommonFplValueTestCases.getScopedElement y "w" subtype
                            let yu = CommonFplValueTestCases.getScopedElement y "u" subtype
                            let yv = CommonFplValueTestCases.getScopedElement y "v" subtype
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a" subtype
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b" subtype
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c" subtype
                            let xua = CommonFplValueTestCases.getScopedElement xu "a" subtype
                            let xub = CommonFplValueTestCases.getScopedElement xu "b" subtype
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c" subtype
                            let xva = CommonFplValueTestCases.getScopedElement xv "a" subtype
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b" subtype
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c" subtype
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a" subtype
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b" subtype
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c" subtype
                            let yua = CommonFplValueTestCases.getScopedElement yu "a" subtype
                            let yub = CommonFplValueTestCases.getScopedElement yu "b" subtype
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c" subtype
                            let yva = CommonFplValueTestCases.getScopedElement yv "a" subtype
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b" subtype
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c" subtype
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
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let block = CommonFplValueTestCases.getScopedElement theory name subtype
                            let x = CommonFplValueTestCases.getScopedElement block "x" subtype
                            let y = CommonFplValueTestCases.getScopedElement block "y" subtype
                            let xw = CommonFplValueTestCases.getScopedElement x "w" subtype
                            let xu = CommonFplValueTestCases.getScopedElement x "u" subtype
                            let xv = CommonFplValueTestCases.getScopedElement x "v" subtype
                            let yw = CommonFplValueTestCases.getScopedElement y "w" subtype
                            let yu = CommonFplValueTestCases.getScopedElement y "u" subtype
                            let yv = CommonFplValueTestCases.getScopedElement y "v" subtype
                            let xwa = CommonFplValueTestCases.getScopedElement xw "a" subtype
                            let xwb = CommonFplValueTestCases.getScopedElement xw "b" subtype
                            let xwc = CommonFplValueTestCases.getScopedElement xw "c" subtype
                            let xua = CommonFplValueTestCases.getScopedElement xu "a" subtype
                            let xub = CommonFplValueTestCases.getScopedElement xu "b" subtype
                            let xuc = CommonFplValueTestCases.getScopedElement xu "c" subtype
                            let xva = CommonFplValueTestCases.getScopedElement xv "a" subtype
                            let xvb = CommonFplValueTestCases.getScopedElement xv "b" subtype
                            let xvc = CommonFplValueTestCases.getScopedElement xv "c" subtype
                            let ywa = CommonFplValueTestCases.getScopedElement yw "a" subtype
                            let ywb = CommonFplValueTestCases.getScopedElement yw "b" subtype
                            let ywc = CommonFplValueTestCases.getScopedElement yw "c" subtype
                            let yua = CommonFplValueTestCases.getScopedElement yu "a" subtype
                            let yub = CommonFplValueTestCases.getScopedElement yu "b" subtype
                            let yuc = CommonFplValueTestCases.getScopedElement yu "c" subtype
                            let yva = CommonFplValueTestCases.getScopedElement yv "a" subtype
                            let yvb = CommonFplValueTestCases.getScopedElement yv "b" subtype
                            let yvc = CommonFplValueTestCases.getScopedElement yv "c" subtype
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
            prty pred T1() {true}
            prty pred opt T2() {true}
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
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let block = CommonFplValueTestCases.getScopedElement theory name subtype
                            let t1 = CommonFplValueTestCases.getScopedElement block "T1()" subtype
                            let t2 = CommonFplValueTestCases.getScopedElement block "T2()" subtype
                            let t3 = CommonFplValueTestCases.getScopedElement block "T3() -> obj" subtype
                            let t4 = CommonFplValueTestCases.getScopedElement block "T4() -> obj" subtype
                            let t5 = CommonFplValueTestCases.getScopedElement block "T5() -> ind" subtype
                            let t6 = CommonFplValueTestCases.getScopedElement block "T6() -> ind" subtype
                            let t7 = CommonFplValueTestCases.getScopedElement block "T7() -> pred" subtype
                            let t8 = CommonFplValueTestCases.getScopedElement block "T8() -> pred" subtype
                            let t9 = CommonFplValueTestCases.getScopedElement block "T9() -> tpl" subtype
                            let t10 = CommonFplValueTestCases.getScopedElement block "T10() -> tpl" subtype
                            let t11 = CommonFplValueTestCases.getScopedElement block "T11() -> Nat" subtype
                            let t12 = CommonFplValueTestCases.getScopedElement block "T12() -> Nat" subtype
                            let t13 = CommonFplValueTestCases.getScopedElement block "T13() -> func" subtype
                            let t14 = CommonFplValueTestCases.getScopedElement block "T14() -> func" subtype
                            Some (r,theory,block,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeConstructors(subtype) =
        ad.Clear()
        let fplCode = """
        def cl TestId:obj 
        {
            ctor TestId() {} 
            ctor TestId(x:obj) {} 
            ctor TestId(x:pred) {} 
            ctor TestId(x:ind) {} 
        }
        ;
        """
        let filename = "TestScopeConstructors" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestId"
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let block = CommonFplValueTestCases.getScopedElement theory name subtype
                            let t1 = CommonFplValueTestCases.getScopedElement block "TestId()" subtype
                            let t2 = CommonFplValueTestCases.getScopedElement block "TestId(obj)" subtype
                            let t3 = CommonFplValueTestCases.getScopedElement block "TestId(pred)" subtype
                            let t4 = CommonFplValueTestCases.getScopedElement block "TestId(ind)" subtype
                            Some (r,theory,block,t1,t2,t3,t4)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeBlocks(subtype) =
        ad.Clear()
        let fplCode = """
            inf SomeInference1 {pre:true con:true}
            inf SomeInference2 {pre:true con:true}
            axiom SomeAxiom1 {true}
            axiom SomeAxiom2 {true}
            postulate SomePostulate1 {true}
            postulate SomePostulate2 {true}
            theorem SomeTheorem1 {true}
            theorem SomeTheorem2 {true}
            proposition SomeProposition1 {true}
            proposition SomeProposition2 {true}
            lemma SomeLemma1 {true}
            lemma SomeLemma2 {true}
            corollary SomeLemma1$1 {true}
            corollary SomeLemma2$1 {true}
            conjecture SomeConjecture1 {true}
            conjecture SomeConjecture2 {true}
            def cl SomeClass1:obj {intr}
            def cl SomeClass2:obj {intr}
            def pred SomePredicate1() {true}
            def pred SomePredicate2() {true}
            def func SomeFunctionalTerm1()->obj {intr}
            def func SomeFunctionalTerm2()->obj {intr}
            def func SomeFunctionalTerm3()->obj {dec ~v:obj v:=v; return v}
            def func SomeFunctionalTerm4()->obj(c:pred) {dec ~v:obj(c:pred) v:=v; return v}
            def func SomeFunctionalTerm5()->SomeClass1 {dec ~v:SomeClass1; return v}
            def func SomeFunctionalTerm6()->SomeClass1 {dec ~v:SomeClass1 v:=SomeClass; return v}
            def func SomeFunctionalTerm7()->SomeClass1 {dec ~v:SomeClass1 v:=SomeClass(); return v}
            def func SomeFunctionalTerm8()->ind {return $112}
            def func SomeFunctionalTerm9()->ind {dec ~v:ind v:=$13; return v}
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
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let inf1 = CommonFplValueTestCases.getScopedElement theory "SomeInference1" subtype
                            let inf2 = CommonFplValueTestCases.getScopedElement theory "SomeInference2" subtype
                            let axi1 = CommonFplValueTestCases.getScopedElement theory "SomeAxiom1" subtype
                            let axi2 = CommonFplValueTestCases.getScopedElement theory "SomeAxiom2" subtype
                            let pst1 = CommonFplValueTestCases.getScopedElement theory "SomePostulate1" subtype
                            let pst2 = CommonFplValueTestCases.getScopedElement theory "SomePostulate2" subtype
                            let thm1 = CommonFplValueTestCases.getScopedElement theory "SomeTheorem1" subtype
                            let thm2 = CommonFplValueTestCases.getScopedElement theory "SomeTheorem2" subtype
                            let pro1 = CommonFplValueTestCases.getScopedElement theory "SomeProposition1" subtype
                            let pro2 = CommonFplValueTestCases.getScopedElement theory "SomeProposition2" subtype
                            let lem1 = CommonFplValueTestCases.getScopedElement theory "SomeLemma1" subtype
                            let lem2 = CommonFplValueTestCases.getScopedElement theory "SomeLemma2" subtype
                            let cor1 = CommonFplValueTestCases.getScopedElement lem1 "SomeLemma1$1" subtype
                            let cor2 = CommonFplValueTestCases.getScopedElement lem2 "SomeLemma2$1" subtype
                            let con1 = CommonFplValueTestCases.getScopedElement theory "SomeConjecture1" subtype
                            let con2 = CommonFplValueTestCases.getScopedElement theory "SomeConjecture2" subtype
                            let cla1 = CommonFplValueTestCases.getScopedElement theory "SomeClass1" subtype
                            let cla2 = CommonFplValueTestCases.getScopedElement theory "SomeClass2" subtype
                            let pre1 = CommonFplValueTestCases.getScopedElement theory "SomePredicate1()" subtype
                            let pre2 = CommonFplValueTestCases.getScopedElement theory "SomePredicate2()" subtype
                            let fun1 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm1() -> obj" subtype
                            let fun2 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm2() -> obj" subtype
                            let fun3 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm3() -> obj" subtype
                            let fun4 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm4() -> obj(pred)" subtype
                            let fun5 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm5() -> SomeClass1" subtype
                            let fun6 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm6() -> SomeClass1" subtype
                            let fun7 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm7() -> SomeClass1" subtype
                            let fun8 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm8() -> ind" subtype
                            let fun9 = CommonFplValueTestCases.getScopedElement theory "SomeFunctionalTerm9() -> ind" subtype
                            let prf1 = CommonFplValueTestCases.getScopedElement thm1 "SomeTheorem1$1" subtype
                            let prf2 = CommonFplValueTestCases.getScopedElement thm2 "SomeTheorem2$1" subtype
                            let loc1 = CommonFplValueTestCases.getScopedElement theory "not(x)" subtype
                            let loc2 = CommonFplValueTestCases.getScopedElement theory "Equal(x, y)" subtype
                            Some (r,theory,inf1,inf2,axi1,axi2,pst1,pst2,thm1,thm2,pro1,pro2,lem1,lem2,cor1,cor2,con1,con2,cla1,cla2,pre1,pre2,fun1,fun2,fun3,fun4,fun5,fun6,fun7,fun8,fun9,prf1,prf2,loc1,loc2)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeProofsAndCorollaries(subtype) =
        ad.Clear()
        let fplCode = """

            theorem TestTheorem1 {true} 
            proof TestTheorem1$1 {1. |- trivial} 
            
            lemma TestLemma1 {true} 
            proof TestLemma1$1 {1. |- trivial} 
            
            proposition TestProposition1 {true} 
            proof TestProposition1$1 {1. |- trivial} 
            
            corollary TestCorollary1$2 {true} 
            proof TestCorollary1$2$1 {1. |- trivial} 

            theorem TestTheorem2 {true} 
            corollary TestTheorem2$1 {true}  
            
            lemma TestLemma2 {true} 
            corollary TestLemma2$1 {true}  
            
            proposition TestProposition2 {true} 
            corollary TestProposition2$1 {true}  

            corollary TestCorollary2$2 {true} 
            corollary TestCorollary2$2$1 {true}  

            conjecture TestConjecture {true} 
            corollary TestConjecture$1 {true}  

            axiom TestAxiom {true} 
            corollary TestAxiom$1 {true}  
        ;
        """
        let filename = "TestScopeProofsAndCorollaries" + subtype

        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let thm1 = CommonFplValueTestCases.getScopedElement theory "TestTheorem1" subtype
                            let proofThm1 = CommonFplValueTestCases.getScopedElement thm1 "TestTheorem1$1" subtype
                            let lem1 = CommonFplValueTestCases.getScopedElement theory "TestLemma1" subtype
                            let proofLem1 = CommonFplValueTestCases.getScopedElement lem1 "TestLemma1$1" subtype
                            let prp1 = CommonFplValueTestCases.getScopedElement theory "TestProposition1" subtype
                            let proofPrp1 = CommonFplValueTestCases.getScopedElement prp1 "TestProposition1$1" subtype
                            let cor1 = CommonFplValueTestCases.getScopedElement theory "TestCorollary1$2" subtype
                            let proofCor1 = CommonFplValueTestCases.getScopedElement cor1 "TestCorollary1$2$1" subtype
                            let thm2 = CommonFplValueTestCases.getScopedElement theory "TestTheorem2" subtype
                            let corThm2 = CommonFplValueTestCases.getScopedElement thm2 "TestTheorem2$1" subtype
                            let lem2 = CommonFplValueTestCases.getScopedElement theory "TestLemma2" subtype
                            let corLem2 = CommonFplValueTestCases.getScopedElement lem2 "TestLemma2$1" subtype
                            let prp2 = CommonFplValueTestCases.getScopedElement theory "TestProposition2" subtype
                            let corPrp2 = CommonFplValueTestCases.getScopedElement prp2 "TestProposition2$1" subtype
                            let cor2 = CommonFplValueTestCases.getScopedElement theory "TestCorollary2$2" subtype
                            let corCor2 = CommonFplValueTestCases.getScopedElement cor2 "TestCorollary2$2$1" subtype
                            let con1 = CommonFplValueTestCases.getScopedElement theory "TestConjecture" subtype
                            let corCon1 = CommonFplValueTestCases.getScopedElement con1 "TestConjecture$1" subtype
                            let axi1 = CommonFplValueTestCases.getScopedElement theory "TestAxiom" subtype
                            let corAxi1 = CommonFplValueTestCases.getScopedElement axi1 "TestAxiom$1" subtype
                            Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeIntrinsicPrimitives(subtype) =
        ad.Clear()
        let fplCode = """
            def cl A:obj {intr}
            def func B()->obj {intr}
            def pred T() {
                dec 
                    ~i:ind i:=$1 
                    ~b:func b:=B()  
                    ~p:pred p:=true 
                    ~o:obj o:=A()
                    ~u:obj u:=undef
                    ~t:tpl t:=$2
                ;
                true 
            };
        """
        let filename = "TestScopeIntrinsicPrimitives" + subtype
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        let getValue (varObj:FplValue) =
            match varObj with
            | :? FplVariable as var -> var.ValueList[0]
            | _ -> new FplIntrinsicUndef((Position("",0,0,0), Position("",0,0,0) ), varObj)

        let result = match stOption with
                        | Some st -> 
                            let r = st.Root
                            let theory = CommonFplValueTestCases.getScopedElement r filename subtype
                            let pred = CommonFplValueTestCases.getScopedElement theory "T()" subtype
                            let iVar = CommonFplValueTestCases.getScopedElement pred "i" subtype
                            let i = getValue iVar
                            let bVar = CommonFplValueTestCases.getScopedElement pred "b" subtype
                            let b = getValue bVar
                            let pVar = CommonFplValueTestCases.getScopedElement pred "p" subtype
                            let p = getValue pVar
                            let oVar = CommonFplValueTestCases.getScopedElement pred "o" subtype
                            let o = getValue oVar
                            let uVar = CommonFplValueTestCases.getScopedElement pred "u" subtype
                            let u = getValue uVar
                            let tVar = CommonFplValueTestCases.getScopedElement pred "t" subtype
                            let t = getValue tVar
                            Some (i, b, p, o, u, t)
                        | None -> None
        prepareFplCode(filename, "", true) |> ignore
        result