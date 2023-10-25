﻿open ErrRecovery
open FplParser
open FParsec


let input = "TestNamespace {
    theory {   
        cl T:obj
        {
            T() { self }
        }
    }
}"

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

ad.Clear()
let origResult = tryParse' ast "recovery failed;" ad input
printf "%O" origResult
ad.PrintDiagnostics


let i = "all proceedingResult in p
                (
                    assert proceedingResult
                )"
let r = run predicate i
printf "%O" r


open System.Text.RegularExpressions

let replaceWithSpaces (inputString: string) (pattern: string) =
    let regex = new Regex(pattern)
    let evaluator = MatchEvaluator(fun (m: Match) -> String.replicate m.Value.Length " ")
    regex.Replace(inputString, evaluator)


let replaceLinesWithSpaces (inputString: string) (pattern: string) =
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let evaluator = MatchEvaluator(fun (m: Match) -> 
        m.Value.Split(System.Environment.NewLine)
        |> Array.map (fun line -> String.replicate line.Length " ")
        |> String.concat System.Environment.NewLine
    )
    regex.Replace(inputString, evaluator)

// Usage:
let s1 = """class Zero: obj
        {
            intr
        }

        // definition of a functional term denoting the successor of a natural number
        func Succ(n: Nat) -> Nat
        {
            intr
        }

        axiom ZeroIsNat()
        {
            is(Zero,Nat)
        }

        axiom SuccessorExistsAndIsUnique()
        {
            all n in Nat
            (
                exn!1 successor in Nat
                (
                    and
                    (
                        NotEqual(successor,n),
                        <successor = Succ(n)>
                    )
                )
            )
        }

        axiom ZeroIsNotSuccessor()
        {
            all n in Nat
            (
                NotEqual(Zero(), Succ(n))
            )
        }

        axiom SuccessorIsInjective()
        {
            all n,m in Nat
            (
                impl
                (
                    < Succ(n) = Succ(m) >, 
                    < n = m >
                )
            )
        }

        axiom CompleteInduction()
        {
            dec 
                ~n: Nat
            ;
            all p in pred
            (
                impl
                (
                    and ( p(0), all n ( impl ( p(n), p(Succ(n)) ) ) ),
                    all n ( p(n) )
                )
            )
        }


        // definition of a new mathematical object (natural number)
        class Nat: obj
        {
            Nat(x: @Digits)
            {
                dec
                    cases
                    (
                        | <x = 0> : self := Zero() 
                        | <x = 1> : self := Succ(Zero())
                        | <x = 2> : self := Succ(Succ(Zero()))
                        ? self := Succ(delegate.decrement(x))  
                    )
                ;
                self
            }

            Nat(x: Int)
            {
                dec
                    cases
                    (
                        | IsGreaterOrEqual(x.RightMember(), x.LeftMember()): self:=x.RightMember()
                        ? self:=undefined
                    )
                    self!obj()
                ;
                self
            }
        }

        pred IsGreaterOrEqual(n,m: Nat)
        {
            ex k in Nat ( <n = Add(m,k)> )
        }

        // besides the class "Nat", we can formulate definition of the set of all natural numbers
        class SetNat: Set
        {
            SetNat()
            {
                dec 
                    ~n: Nat
                    // Assert that elements of class "Nat" can be collected to a bigger object of class "SetNat"
                    // This requires that we can apply the "In" predicate defined in Fpl.Set.ZermeloFraenkel
                    // to object of the class "Nat". This becomes possible when we assert that every variable of the class
                    // "Nat" is a also Set.
                    // This is comparable to implementing an interface (or comparable to multiple inheritance).
                    assert
                        all n
                        (
                            and
                            (
                                is(n, Set),
                                In(n, self)
                            )
                        )
                    self!Set()
                ;
                self
            }
        }

        // Addition of natural numbers
        func Add(n,m: Nat)->Nat
        {
            dec
                ~result, k: Nat
                cases
                (
                    | <m = 0>: result:= n
                    | <Succ(m) = k>: result:= Succ(Add(n,k))
                    ? result:= undef
                )
            ;
            return result
        }

        func Add(n,m: @Decimal)->Nat
        {
            return delegate.add(n,m)
        }


        prop AddIsUnique(op: Add)
        {
            dec
                ~n,m: Nat
                ~anotherAdd: Add
            ;
            pre:
                and
                    (
                        < op(n,0) = n >,
                        < anotherAdd(n,0) = n >,
                        < op(n,Succ(m)) = Succ(op(n,m)) >,
                        < anotherAdd(n,Succ(m)) = Succ(anotherAdd(n,m)) >
                    )
            con:
                all n
                (
                    all m
                    (
                        < op(n,m) = anotherAdd(n,m) >
                    )
                )
        }

        proof AddIsUnique!1
        {
            1. assume pre
            2. |- trivial
            3. |- and
                    (
                        < Add(n,0) = n >,
                        < anotherAdd(n,0) = n >,
                        < Add(n,Succ(m)) = Succ(Add(n,m)) >,
                        < anotherAdd(n,Succ(m)) = Succ(anotherAdd(n,m)) >
                    )
        }


        func Sum(list:* Nat)->Nat
        {
            dec 
                ~result, addend: Nat
                result:=Zero()
                for addend in list
                (
                    result:=Add(result,addend)
                )
            ;
            return result
        }

        func Sum(list:* Nat)->Nat
        {
            dec
                ~i: index
                result:=Zero()
                for i in list
                (
                    result:=Add(result,list<i>)
                )
            ; 
            return result
        }

        func Sum(from,to:Nat, arr: Nat[from~to]) -> Nat
        {
            dec
                ~i, result: Nat
                result:=Zero()

                for i in [from~to]
                (
                    result:=Add(result,arr<i>)
                )
            ;
            return result
        }

        func Sum(arr: Nat[~]) -> Nat
        {
            dec
                ~addend, result: Nat
                result:=Zero()
                for addend in arr
                (
                    result:=Add(result,addend)
                )
            ;
            return result
        }

        func Addend(a: Nat)->Nat
        {
            intr
        }

        func Sum(from,to:Nat, addend: Addend) -> Nat
        {
            dec
                ~i, result: Nat
                result:=Zero()
                for i in [from~to]
                (
                    result:=Add(result,addend(i))
                )
            ;
            return result
        }

        proposition SumOfConsecutiveNumbers()
        {
            dec
                ~n, limit: Nat
                ~f: Addend
            ;
            premise:
                all n in [1~limit]
                (
                    <f(n) = n>
                )
            conclusion:
                all n
                (
                    <Sum(1,n,f) = Div(Mul(n,Succ(n)),2)>
                )

        }

        func RealValuedFunction(x:Real) -> Real
        {
            dec
                assert is(self,Function)
            ;
            return self
        }

        pred IsBounded(x: Real)
        {
            ex upperBound, lowerBound in Real
            (
                and (LowerEqual(x,upperBound), LowerEqual(lowerBound,x))
            )
        }

        pred IsBounded(f: RealValuedFunction)
        {
            all x in Real
            (
                IsBounded(f(x))
            )
        }

        func RiemannIntegral(from,to:Real, f: RealValuedFunction ) -> Real
        {
            dec
                ~x, result: Real
                assert IsBounded(f)
                result:=ZeroReal()
                for x in [from~to]
                (
                    result:=Add(result, Mult(f(x), D(x)))
                )
            ;
            return result
        }

        func PowerSeries(k:Nat, arr: Real[k~] ) -> Real
        {
            dec
                ~result: Real
                ~i: Nat
                result:=ZeroReal()
                for i in [k~]
                (
                    result:=Add(result, Exp(arr<i>,i))
                )
            ;
            return result
        }
        /* This is 
        a 

        test
        */

        /* This is 
        another

        test
        */
        function ContinuedFraction(k:Nat, arr: Int[k~]) -> Real
        {
            dec
                ~i: Nat
                ~cf: Real
                cf:=Real(arr<k>)
                i:=Succ(k)
            ;
            return Add( cf, Div(1 , ContinuedFraction(i, arr[i~])))
        }

        // Example of defining a constant for the natural number 100 using the
        class N100:Set{N100(){dec ~n:Nat  self!Set() self:=SetBuilder(SetNat(),IsGreater(n,100)); self }}""" 

let r1 = replaceLinesWithSpaces s1 "\/\/[^\n]*"
printfn "%i, %i, %i, %i" s1.Length r1.Length (r1.Split('\n').Length) (s1.Split('\n').Length)

let r2 = replaceLinesWithSpaces r1 "\/\*((?:.|\n)*?)\*\/"
printfn "%i, %i, %i, %i" r1.Length r2.Length (r2.Split('\n').Length) (r1.Split('\n').Length)

printfn "%s" r2