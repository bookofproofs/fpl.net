namespace Scope

open FParsec
open TestFplInterpreter.Helpers.Common
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Types1.TopLevel
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables


type TestCases =

    static member getScopedElement (fv:FplGenericNode) name subtype =
        if subtype <> "" then
            if fv.Scope.ContainsKey(name) then 
                fv.Scope[name]
            elif fv.Scope.Count>0 then
                let kv = fv.Scope |> Seq.head
                kv.Value
            else
                new FplRoot()
        else
            fv.Scope[name]

    static member ScopeVariablesInSignature(subtype) =
        let fplCode = """
        def pred TestPredicate(x,y:pred(u,v,w:func(a,b,c:obj)->obj)) 
            {true}
        
        """
        let filename = "TestScopeVariablesInSignature" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let name = "TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))"
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let block = TestCases.getScopedElement theory name subtype
            let x = TestCases.getScopedElement block "x" subtype
            let y = TestCases.getScopedElement block "y" subtype
            let xw = TestCases.getScopedElement x "w" subtype
            let xu = TestCases.getScopedElement x "u" subtype 
            let xv = TestCases.getScopedElement x "v" subtype
            let yw = TestCases.getScopedElement y "w" subtype
            let yu = TestCases.getScopedElement y "u" subtype 
            let yv = TestCases.getScopedElement y "v" subtype
            let xwa = TestCases.getScopedElement xw "a" subtype
            let xwb = TestCases.getScopedElement xw "b" subtype
            let xwc = TestCases.getScopedElement xw "c" subtype
            let xua = TestCases.getScopedElement xu "a" subtype
            let xub = TestCases.getScopedElement xu "b" subtype
            let xuc = TestCases.getScopedElement xu "c" subtype
            let xva = TestCases.getScopedElement xv "a" subtype
            let xvb = TestCases.getScopedElement xv "b" subtype
            let xvc = TestCases.getScopedElement xv "c" subtype
            let ywa = TestCases.getScopedElement yw "a" subtype
            let ywb = TestCases.getScopedElement yw "b" subtype
            let ywc = TestCases.getScopedElement yw "c" subtype
            let yua = TestCases.getScopedElement yu "a" subtype
            let yub = TestCases.getScopedElement yu "b" subtype
            let yuc = TestCases.getScopedElement yu "c" subtype
            let yva = TestCases.getScopedElement yv "a" subtype
            let yvb = TestCases.getScopedElement yv "b" subtype
            let yvc = TestCases.getScopedElement yv "c" subtype
            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeVariablesInSignatureVariadic(subtype) =
        let fplCode = """
        def pred TestPredicate(x,y:*pred(u,v,w:func(a,b,c:*obj[ind])->obj)[obj]) 
            {true}
        
        """
        let filename = "TestScopeVariablesInSignatureVariadic" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let name = "TestPredicate(*pred(func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj)[obj], *pred(func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj)[obj])"
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let block = TestCases.getScopedElement theory name subtype
            let x = TestCases.getScopedElement block "x" subtype
            let y = TestCases.getScopedElement block "y" subtype
            let xw = TestCases.getScopedElement x "w" subtype
            let xu = TestCases.getScopedElement x "u" subtype
            let xv = TestCases.getScopedElement x "v" subtype
            let yw = TestCases.getScopedElement y "w" subtype
            let yu = TestCases.getScopedElement y "u" subtype
            let yv = TestCases.getScopedElement y "v" subtype
            let xwa = TestCases.getScopedElement xw "a" subtype
            let xwb = TestCases.getScopedElement xw "b" subtype
            let xwc = TestCases.getScopedElement xw "c" subtype
            let xua = TestCases.getScopedElement xu "a" subtype
            let xub = TestCases.getScopedElement xu "b" subtype
            let xuc = TestCases.getScopedElement xu "c" subtype
            let xva = TestCases.getScopedElement xv "a" subtype
            let xvb = TestCases.getScopedElement xv "b" subtype
            let xvc = TestCases.getScopedElement xv "c" subtype
            let ywa = TestCases.getScopedElement yw "a" subtype
            let ywb = TestCases.getScopedElement yw "b" subtype
            let ywc = TestCases.getScopedElement yw "c" subtype
            let yua = TestCases.getScopedElement yu "a" subtype
            let yub = TestCases.getScopedElement yu "b" subtype
            let yuc = TestCases.getScopedElement yu "c" subtype
            let yva = TestCases.getScopedElement yv "a" subtype
            let yvb = TestCases.getScopedElement yv "b" subtype
            let yvc = TestCases.getScopedElement yv "c" subtype
            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeVariablesInBlock(subtype) =
        let fplCode = """
        def pred TestPredicate() 
        {   dec 
                x,y:pred(u,v,w:func(a,b,c:obj)->obj)
                s:Set
            ; 
            true
        }
        
        """
        let filename = "TestScopeVariablesInBlock" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let name = "TestPredicate()"
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let block = TestCases.getScopedElement theory name subtype
            let x = TestCases.getScopedElement block "x" subtype
            let y = TestCases.getScopedElement block "y" subtype
            let s = TestCases.getScopedElement block "s" subtype
            let xw = TestCases.getScopedElement x "w" subtype
            let xu = TestCases.getScopedElement x "u" subtype
            let xv = TestCases.getScopedElement x "v" subtype
            let yw = TestCases.getScopedElement y "w" subtype
            let yu = TestCases.getScopedElement y "u" subtype
            let yv = TestCases.getScopedElement y "v" subtype
            let xwa = TestCases.getScopedElement xw "a" subtype
            let xwb = TestCases.getScopedElement xw "b" subtype
            let xwc = TestCases.getScopedElement xw "c" subtype
            let xua = TestCases.getScopedElement xu "a" subtype
            let xub = TestCases.getScopedElement xu "b" subtype
            let xuc = TestCases.getScopedElement xu "c" subtype
            let xva = TestCases.getScopedElement xv "a" subtype
            let xvb = TestCases.getScopedElement xv "b" subtype
            let xvc = TestCases.getScopedElement xv "c" subtype
            let ywa = TestCases.getScopedElement yw "a" subtype
            let ywb = TestCases.getScopedElement yw "b" subtype
            let ywc = TestCases.getScopedElement yw "c" subtype
            let yua = TestCases.getScopedElement yu "a" subtype
            let yub = TestCases.getScopedElement yu "b" subtype
            let yuc = TestCases.getScopedElement yu "c" subtype
            let yva = TestCases.getScopedElement yv "a" subtype
            let yvb = TestCases.getScopedElement yv "b" subtype
            let yvc = TestCases.getScopedElement yv "c" subtype
            Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeVariablesInBlockVariadic(subtype) =
        let fplCode = """
        def pred TestPredicate() 
            {dec x,y:*pred(u,v,w:func(a,b,c:*obj[tpl])->obj)[ind]; true}
        
        """
        let filename = "TestScopeVariablesInBlockVariadic" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let name = "TestPredicate()"
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let block = TestCases.getScopedElement theory name subtype
            let x = TestCases.getScopedElement block "x" subtype
            let y = TestCases.getScopedElement block "y" subtype
            let xw = TestCases.getScopedElement x "w" subtype
            let xu = TestCases.getScopedElement x "u" subtype
            let xv = TestCases.getScopedElement x "v" subtype
            let yw = TestCases.getScopedElement y "w" subtype
            let yu = TestCases.getScopedElement y "u" subtype
            let yv = TestCases.getScopedElement y "v" subtype
            let xwa = TestCases.getScopedElement xw "a" subtype
            let xwb = TestCases.getScopedElement xw "b" subtype
            let xwc = TestCases.getScopedElement xw "c" subtype
            let xua = TestCases.getScopedElement xu "a" subtype
            let xub = TestCases.getScopedElement xu "b" subtype
            let xuc = TestCases.getScopedElement xu "c" subtype
            let xva = TestCases.getScopedElement xv "a" subtype
            let xvb = TestCases.getScopedElement xv "b" subtype
            let xvc = TestCases.getScopedElement xv "c" subtype
            let ywa = TestCases.getScopedElement yw "a" subtype
            let ywb = TestCases.getScopedElement yw "b" subtype
            let ywc = TestCases.getScopedElement yw "c" subtype
            let yua = TestCases.getScopedElement yu "a" subtype
            let yub = TestCases.getScopedElement yu "b" subtype
            let yuc = TestCases.getScopedElement yu "c" subtype
            let yva = TestCases.getScopedElement yv "a" subtype
            let yvb = TestCases.getScopedElement yv "b" subtype
            let yvc = TestCases.getScopedElement yv "c" subtype
            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeProperties(subtype) =
        let fplCode = """
        def cl Nat
        def pred TestId() 
        {
            intr 
            prty pred T1() {true}
            prty func T3()->obj {intr}
            prty func T5()->ind {intr}
            prty func T7()->pred {intr}
            prty func T9()->tpl {intr}
            prty func T11()->Nat {intr}
            prty func T13()->func {intr}
        }
        
        """
        let filename = "TestScopeProperties" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let name = "TestId()"
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let block = TestCases.getScopedElement theory name subtype
            let t1 = TestCases.getScopedElement block "T1()" subtype
            let t3 = TestCases.getScopedElement block "T3() -> obj" subtype
            let t5 = TestCases.getScopedElement block "T5() -> ind" subtype
            let t7 = TestCases.getScopedElement block "T7() -> pred" subtype
            let t9 = TestCases.getScopedElement block "T9() -> tpl" subtype
            let t11 = TestCases.getScopedElement block "T11() -> Nat" subtype
            let t13 = TestCases.getScopedElement block "T13() -> func" subtype
            Some (r,theory,block,t1,t3,t5,t7,t9,t11,t13)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeConstructors(subtype) =
        let fplCode = """
        def cl TestId 
        {
            ctor TestId() {} 
            ctor TestId(x:obj) {} 
            ctor TestId(x:pred) {} 
            ctor TestId(x:ind) {} 
        }
        
        """
        let filename = "TestScopeConstructors" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let name = "TestId"
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let block = TestCases.getScopedElement theory name subtype
            let t1 = TestCases.getScopedElement block "TestId()" subtype
            let t2 = TestCases.getScopedElement block "TestId(obj)" subtype
            let t3 = TestCases.getScopedElement block "TestId(pred)" subtype
            let t4 = TestCases.getScopedElement block "TestId(ind)" subtype
            Some (r,theory,block,t1,t2,t3,t4)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeBlocks(subtype) =
        let fplCode = """
            inf SomeInference1 {pre:true con:true}
            inf SomeInference2 {pre:true con:true}
            axiom SomeAxiom1 {true}
            axiom SomeAxiom2 {true}
            postulate  SomePostulate1 {true}
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
            def cl SomeClass1 {intr}
            def cl SomeClass2 {intr}
            def pred SomePredicate1() {true}
            def pred SomePredicate2() {true}
            def func SomeFunctionalTerm1()->obj {intr}
            def func SomeFunctionalTerm2()->obj {intr}
            def func SomeFunctionalTerm3()->obj {dec v:obj v:=v; return v}
            def func SomeFunctionalTerm4()->tpl {dec v:tpl v:=v; return v}
            def func SomeFunctionalTerm5()->SomeClass1 {dec v:SomeClass1; return v}
            def func SomeFunctionalTerm6()->SomeClass1 {dec v:SomeClass1 v:=SomeClass1; return v}
            def func SomeFunctionalTerm7()->SomeClass1 {dec v:SomeClass1 v:=SomeClass1(); return v}
            def func SomeFunctionalTerm8()->ind {return $112}
            def func SomeFunctionalTerm9()->ind {dec v:ind v:=$13; return v}
            proof SomeTheorem1$1 {1: trivial}
            proof SomeTheorem2$1 {1: trivial}
            loc not(x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;
            loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;
        
        """
        let filename = "TestScopeBlocks" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 

        let result =  
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let inf1 = TestCases.getScopedElement theory "SomeInference1" subtype
            let inf2 = TestCases.getScopedElement theory "SomeInference2" subtype
            let axi1 = TestCases.getScopedElement theory "SomeAxiom1" subtype
            let axi2 = TestCases.getScopedElement theory "SomeAxiom2" subtype
            let pst1 = TestCases.getScopedElement theory "SomePostulate1" subtype
            let pst2 = TestCases.getScopedElement theory "SomePostulate2" subtype
            let thm1 = TestCases.getScopedElement theory "SomeTheorem1" subtype
            let thm2 = TestCases.getScopedElement theory "SomeTheorem2" subtype
            let pro1 = TestCases.getScopedElement theory "SomeProposition1" subtype
            let pro2 = TestCases.getScopedElement theory "SomeProposition2" subtype
            let lem1 = TestCases.getScopedElement theory "SomeLemma1" subtype
            let lem2 = TestCases.getScopedElement theory "SomeLemma2" subtype
            let cor1 = TestCases.getScopedElement lem1 "SomeLemma1$1" subtype
            let cor2 = TestCases.getScopedElement lem2 "SomeLemma2$1" subtype
            let con1 = TestCases.getScopedElement theory "SomeConjecture1" subtype
            let con2 = TestCases.getScopedElement theory "SomeConjecture2" subtype
            let cla1 = TestCases.getScopedElement theory "SomeClass1" subtype
            let cla2 = TestCases.getScopedElement theory "SomeClass2" subtype
            let pre1 = TestCases.getScopedElement theory "SomePredicate1()" subtype
            let pre2 = TestCases.getScopedElement theory "SomePredicate2()" subtype
            let fun1 = TestCases.getScopedElement theory "SomeFunctionalTerm1() -> obj" subtype
            let fun2 = TestCases.getScopedElement theory "SomeFunctionalTerm2() -> obj" subtype
            let fun3 = TestCases.getScopedElement theory "SomeFunctionalTerm3() -> obj" subtype
            let fun4 = TestCases.getScopedElement theory "SomeFunctionalTerm4() -> tpl" subtype
            let fun5 = TestCases.getScopedElement theory "SomeFunctionalTerm5() -> SomeClass1" subtype
            let fun6 = TestCases.getScopedElement theory "SomeFunctionalTerm6() -> SomeClass1" subtype
            let fun7 = TestCases.getScopedElement theory "SomeFunctionalTerm7() -> SomeClass1" subtype
            let fun8 = TestCases.getScopedElement theory "SomeFunctionalTerm8() -> ind" subtype
            let fun9 = TestCases.getScopedElement theory "SomeFunctionalTerm9() -> ind" subtype
            let prf1 = TestCases.getScopedElement thm1 "SomeTheorem1$1" subtype
            let prf2 = TestCases.getScopedElement thm2 "SomeTheorem2$1" subtype
            let loc1 = TestCases.getScopedElement theory "not(undef)" subtype
            let loc2 = TestCases.getScopedElement theory "Equal(undef, undef)" subtype
            Some (r,theory,inf1,inf2,axi1,axi2,pst1,pst2,thm1,thm2,pro1,pro2,lem1,lem2,cor1,cor2,con1,con2,cla1,cla2,pre1,pre2,fun1,fun2,fun3,fun4,fun5,fun6,fun7,fun8,fun9,prf1,prf2,loc1,loc2)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeProofsAndCorollaries(subtype) =
        let fplCode = """

            theorem TestTheorem1 {true} 
            proof TestTheorem1$1 {1: trivial}
            
            lemma TestLemma1 {true} 
            proof TestLemma1$1 {1: trivial}
            
            proposition TestProposition1 {true} 
            proof TestProposition1$1 {1: trivial}
            
            corollary TestCorollary1$2 {true} 
            proof TestCorollary1$2$1 {1: trivial}

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
        
        """
        let filename = "TestScopeProofsAndCorollaries" + subtype

        prepareFplCode(filename + ".fpl", fplCode, false) 
        let result = 
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let thm1 = TestCases.getScopedElement theory "TestTheorem1" subtype
            let proofThm1 = TestCases.getScopedElement thm1 "TestTheorem1$1" subtype
            let lem1 = TestCases.getScopedElement theory "TestLemma1" subtype
            let proofLem1 = TestCases.getScopedElement lem1 "TestLemma1$1" subtype
            let prp1 = TestCases.getScopedElement theory "TestProposition1" subtype
            let proofPrp1 = TestCases.getScopedElement prp1 "TestProposition1$1" subtype
            let cor1 = TestCases.getScopedElement theory "TestCorollary1$2" subtype
            let proofCor1 = TestCases.getScopedElement cor1 "TestCorollary1$2$1" subtype
            let thm2 = TestCases.getScopedElement theory "TestTheorem2" subtype
            let corThm2 = TestCases.getScopedElement thm2 "TestTheorem2$1" subtype
            let lem2 = TestCases.getScopedElement theory "TestLemma2" subtype
            let corLem2 = TestCases.getScopedElement lem2 "TestLemma2$1" subtype
            let prp2 = TestCases.getScopedElement theory "TestProposition2" subtype
            let corPrp2 = TestCases.getScopedElement prp2 "TestProposition2$1" subtype
            let cor2 = TestCases.getScopedElement theory "TestCorollary2$2" subtype
            let corCor2 = TestCases.getScopedElement cor2 "TestCorollary2$2$1" subtype
            let con1 = TestCases.getScopedElement theory "TestConjecture" subtype
            let corCon1 = TestCases.getScopedElement con1 "TestConjecture$1" subtype
            let axi1 = TestCases.getScopedElement theory "TestAxiom" subtype
            let corAxi1 = TestCases.getScopedElement axi1 "TestAxiom$1" subtype
            Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                axi1,corAxi1)
        prepareFplCode(filename, "", true) |> ignore
        result

    static member ScopeIntrinsicPrimitives(subtype) =
        let fplCode = """
            def cl A {intr}
            def func B()->func {intr}
            def pred T() {
                dec 
                    i:ind i:=$1 
                    b:func b:=B()  
                    p:pred p:=true 
                    o:obj o:=A()
                    u:obj u:=undef
                    t:tpl t:=$2
                ;
                true 
            }
        """
        let filename = "TestScopeIntrinsicPrimitives" + subtype
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let getValue (varObj:FplGenericNode) =
            match varObj with
            | :? FplVariable as var when var.Value.IsSome -> var.Value.Value
            | _ -> new FplIntrinsicUndef((Position("",0,0,0), Position("",0,0,0) ), varObj)

        let result = 
            let r = heap.Root
            let theory = TestCases.getScopedElement r filename subtype
            let pred = TestCases.getScopedElement theory "T()" subtype
            let iVar = TestCases.getScopedElement pred "i" subtype
            let i = getValue iVar
            let bVar = TestCases.getScopedElement pred "b" subtype
            let b = getValue bVar
            let pVar = TestCases.getScopedElement pred "p" subtype
            let p = getValue pVar
            let oVar = TestCases.getScopedElement pred "o" subtype
            let o = getValue oVar
            let uVar = TestCases.getScopedElement pred "u" subtype
            let u = getValue uVar
            let tVar = TestCases.getScopedElement pred "t" subtype
            let t = getValue tVar
            Some (i, b, p, o, u, t)
        prepareFplCode(filename, "", true) |> ignore
        result
