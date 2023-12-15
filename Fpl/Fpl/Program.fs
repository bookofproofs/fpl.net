﻿open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions






let input = """uses Fpl.SetTheory
uses Landau *

// If x <> y then succesor of x <> successor of y.
theorem Theorem_001()
{
    dec ~x,y: Nat; // PoC's note: FPL makes the distinction between the set N and the class Nat of a natural number explicit!
    // if x != y then x' != y'
    (( x <> y ) => ( x' <> y' ))
}


proof Theorem_001$1
{
    // PoC note: Proof by Contradiction (Type 2)
    // Strategy: Assume that the premise is true and the conclusion is false.
    // Then derive a contradiction.

    // Otherwise, we would have x' = y' and hence, by axiom 4, x = y
    100. |- assume ((x <> y) ~and (x' = y'))
    200. Axiom4() |- ( x = y )
    300. |- revoke 100.
    qed
}

// The successor (x) never equals x 
theorem Theorem_002 ()
{
    dec ~x: Nat;
    ( x' <>  x )
}

proof Theorem_002$1
{
    // PoC note: Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( AddNat(n,1) )` is true.
    
    // "base case" 
    100. and (Axiom1(), Axiom3()) |- ( 1' <> 1 )
    
    // "inductive step" 
    200. |- assume ex n in N ( n' <> n )
    300. Theorem_001() |- ( n''<> n' ) 
    
    400. and(100., 400.) 
        |- all x in N ( x' <> x )
    qed
}
    
// If x <> 1, then there exists one (hence by Axiom 4, exactly one) predecessor u of x, i.e. successor (u) = x.
theorem Theorem_003 ()
{
    dec ~x,u: Nat;
    (
        ( x <> 1 ) => exn$1 u in N ( u' = x )
    )
}

proof Theorem_003$1
{
    // PoC note: Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( AddNat(n,1) )` is true.
    
    dec 
         ~x : object
         ~m : Set // let m be a set 
         assert (1 ~in m) // such that 1 belongs to m
         assert 
            // and of all those x for which there exists such u.
            // PoC note: We formalize the notion "for which there exists such u" by the following implication
            impl
            (
                // if for some x the premise holds
                (
                    ( x <> 1 ) => exn$1 u in N ( u' = x )
                ),
                // then it belongs to the set m
                (x ~in m)
            )
    ;

    // "base case" 
    100. |- (1 ~in m)
    200. |- trivial
    
    // "inductive step" 
    300. 200. |- assume (x ~in m)
    400. |- exn$1 u in N ( u' = x )
    500. Axiom3(), Axiom2() |- ( u'' = x' )
    600. bydef m |- (x' ~in m)
    700. |- ( m = N )
    
    qed
}

// PoC note: In Landau's original, Theorem 4 is formulated as "at the same time Definition 1". In FPL, we deal with theorems and definitions separately,
// making it more transparent, what is part of a constructive definition and what is the predicative expression about it in the theorem.  

// Theorem 3, and at the same time definition 1: To every pair of number x,y, we can assign in exactly one way a natural number, called x + y such that 
// (1) x + 1 = x' for every x
// (2) x + y = (x + y)'
// x + y is called the sum of x and y, or the number obtained by addition of y to x.

definition function Sum infix "+" (x,y: Nat) -> Nat
{
    declaration
        ~res: Nat
        cases
        (
            | ( y = 1 ) : res := x' 
            ? Sum(x,y') := Sum(x,y)'
        )
    ;
    return res
}

// PoC note: Landau 'proves' the definition in one proof splitting it into the parts A and B. In fact, he proves that the way he defines Sum is 'well-defined'
// i.e. it exists and is unique
// In FPL, we formulate for these parts lemmas and them prove them. Finally, we prove the Theorem_004 using the lemmas
// This makes the prove longer on one hand but more clear on the other hand.
// (PoC note: In modern terms, Landau shows the uniqueness Sum is actually a function, i.e. that it produces at most one result, is unique)
lemma Theorem_004_A ()
{
    // (A) We first show that for fixed x there is at most one possibility of defining x + y for all y such that x + 1 = successor(x) and x + successor(y) = successor (x, y)
    declaration 
         ~x : N // for a fixed x
    ;

    and (
        ( Sum(x, 1) = Successor(x) )
        , 
        all y in N 
        ( 
            ( Sum(x,Successor(y)) = Successor(x,y) )
        ) 
    )
}

proof Theorem_004_A$1
{
    // PoC note: Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( Sum(n,1) )` is true.
    
    declaration 
         ~x : N // for a fixed x
        // and for all y we define 
        for y in N 
        (
            // such that
            a[1] := Successor(x) 
            b[1] := Successor(x)
            a[Successor(y)] := Successor(a[y])
            b[Successor(y)] := Successor(b[y])
        )
    ;
    declaration
         ~m : Set // let m be a set 
         assert 
            // Let m be the set of all y 
            // for which a!y = b!y and a!1 = successor(x) = b!1
            iif
            (
                ( a[y] = b[y] )
                ,
                // PoC note: We formulate it predicatively using the implication that if the conjugated premise is fulfilled for some x than x is element of m
                (y ~in m)
            )
    ;
    
    // "base case" 
    100. bydef a[1], bydef b[1] |- ( a[1] = Successor(x) = b[1] )
    100. |- ( a[1] = b[1] )
    200. bydef m |- In(1, m)
    
    // "inductive step" 
    300. 200. |- assume ex y ( In(y,m) ) // if y belongs to m
    400. bydef m |- ( a[y] = b[y] )
    500. Axiom2 |- 
        ( Successor(a[y]) = Successor(b[y]) )
    600. bydef a[Successor(y)], bydef b[Successor(b)] |- 
        ( a[Successor(y)] = Successor(a[y]) = Successor(b[y]) = b[Successor(y)] )
    650. |- In(Successor(y), m)
    
    700. |- all y in N ( ( a[y] = b[y] ) )
    qed
}

// PoC note: In modern terms, Landau shows the existence of the function Sum
lemma Theorem_004_B ()
{
    // (B) Now we will prove that for each x it is actually possible to define x + y for all y in such a way that 
    // x + 1 = successor(x) and x + successor(y) = successor (x + y) for every y.
    // PoC note: We reformulate it predicatively
    all x,y in N
    (
        and 
        (
            ( Sum(x, 1) = Successor (x) )
            ,
            ( Sum(x, Successor(y)) = Successor (x, y) )
        )
    )
}



proof Theorem_004_B$1
{
    // PoC note: Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( AddNat(n,1) )` is true.
    
    // Let m be the set all x for which this is possible.

    declaration
         ~m : Set 
         assert 
            impl
            (
                all x,y in N
                (
                    and 
                    (
                        ( Sum(x, 1) = Successor (x) )
                        ,
                        ( Sum(x, Successor(y)) = Successor (x, y) )
                    )
                )
                ,
                In (x, m)
            )
    ;

    // "base case" 
    // For x = 1 the number x + y = successor(y) is as required
    100. |- assume (x = 1) 
    110. 100., bydef Sum |- 
        ( Sum(x, 1) = Successor (x) ) // since x + 1 = successor(1) = successor (x) and 
    150. 100., bydef Sum |- 
        // (x+y') = (y')' = (x+y)'
        ( Sum(x, Successor(y)) = Successor(Successor(y)) = Successor (x,y) )
    200. |- In (1,m) // hence 1 belongs to m
    
    // "inductive step" 
    300. 200. |- assume ex x ( In (x, m) ) // let x belong to m, so that there exists an x + y for all y. Then the number x'+ y = (x + y)' is the required number for x'  
    400. bydef Sum |- 
        ( Sum(Successor (x), 1) = Successor(Sum(x,1)) = Successor(Successor(x)) ) // since x' + 1 = (x+1)' = (x')'

    500. bydef Sum |- 
        // and x' + y' = (x+y')' = ((x+y)')' = (x'+y)' 
        ( Sum(Successor(x), Successor(y)) = Successor(Sum(x, Successor(y))) = Successor(Successor(x,y)) = Successor(Sum (Successor(x), y)) ) 
    600. |- In (Successor(x), m) // hence, x' belongs to m
    qed
}

// (Associative Law of Addition)
theorem Theorem_005_SumIsAssoziative ()
{
    ( Sum(Sum(x,y),z) = Sum(x,Sum(y,z)) )
}

proof Theorem_005_SumIsAssoziative$1
{
    // Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( Sum(n,1) )` is true.
    
    dec 
         ~x,y,z : N // Fix x and y.
         ~m : Set 
         // Denote by m the set of all z for which the assertion of the theorem holds
         assert 
            impl
            (
                ( Sum(Sum(x,y),z) = Sum(x,Sum(y,z)) )
                ,
                In (z, m)
            )
    ;

    // "base case" 
    // (x + y) + 1 = (x + y)' = x + y' = x + (y + 1)
    100. bydef Sum |- ( Sum(Sum(x,y),1) = Successor(Sum(x,y)) = Sum(x, Successor(y)) = Sum(x, Sum(y,1)) )
    200. |- In (1, m)
    
    // "inductive step" 
    300. 200. |- assume ex x ( ( Sum(Sum(x,y),z) = Sum(x,Sum(y,z)) ) )
    400. |- ( 
              Sum( Sum(x,y), Successor (z) )    //   (x+y) + z' 
            = Successor( Sum(Sum(x,y), z) )     // = ((x+y) + z)'
            = Successor( Sum(x, Sum(y,z)) )     // = (x + (y + z))'
            = Sum( x, Successor(Sum(y,z)) )     // = x + (y+z)'
            = Sum( x, Sum(y, Successor (y)))    // = x + (y + z')
            )
    600. |- In (Successor(z), m)
    qed
}

// (Commutative Law of Addition)
theorem Theorem_006_SumIsCommutatitve ()
{
    ( Sum(x,y) = Sum(y,x) )
}

proof Theorem_006_SumIsCommutatitve$1
{
    // Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( Sum(n,1) )` is true.
    
    dec 
         ~x, y : N // Fix x.
         ~m : Set 
         // Let m be the set of all y for which the assertion of the theorem holds
         assert 
            impl
            (
                ( Sum(x,y) = Sum(y,x) )
                ,
                In (z, m)
            )
    ;

    // "base case" 
    100. bydef Sum |- ( Sum(y,1) = Successor(Sum(y)) ) // We have y + 1 = y', 
    110. Theorem_004_B$1 |- (Sum(1,y) = Successor(Sum(y)) )  // by the construction in the proof of Theorem 4, 1 + y = y'
    150. 100., 110. |- ( Sum(y,1) = Sum(1,y) ) // so that 1 + y = y + 1
    200. |- In (1, m) // and 1 belongs to m.
    
    // "inductive step" 
    300. 200. |- assume ex x ( In (x, m) ) // If x belongs to m,
    400. |- ( Sum(x,y) = Sum(y,x) )
    450. |- (    //   then x + y = y + x
            Successor( Sum(x,y) )     // therefore (x + y)' = (y+x)' = y + x'
            = Successor( Sum(y,x) )  
            = Sum( x, Successor(y) )     
            )
    470. Theorem_004_B$1 |- (Sum(Successor(x),y) = Successor(Sum(x,y)) )  // by the construction in the proof of theorem 4, we have x' + y = (x + y)'.
    500. |- ( Sum(Successor(x),y) = Sum(y, Successor(x)) ) // hense x' + y = y + x'
    600. |- In (Successor(x), m) // so that x' belongs to m
    qed
}

// y != x + y
theorem Theorem_007 ()
{
    not ( ( y = Sum(x,y) ) )
}

proof Theorem_007$1
{
    // Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( Sum(n,1) )` is true.
    
    dec 
         ~x : N // Fix x
         ~m : Set // and let m be the set of all y for which the assertion is true
         assert
            impl
            (
                not ( ( y = Sum(x,y) ) )
                ,
                In (y, m)
            )
            
    ;

    // "base case" 
    100. Axiom3 |- not ( ( 1 = Successor(x) ) ) // 1 != x'
    200. bydef Sum |- not ( ( 1 = Sum(x,1) ) ) // 1 != x + 1
    200. |- In (1, m) // 1 belongs to m
    
    // "inductive step" 
    300. 200. |- assume ex y in N ( In (y, m) ) // if y belongs to m
    400. |- not ( ( y = Sum(x,y) ) ) // then y != x + y
    500. |- not ( ( Successor(y) = Successor(Sum(x,y)) ) ) // hence y' != (x + y)'
    510. bydef Sum |- not ( ( Successor(y) = Sum(x,Successor(y)) ) ) // y' != x + y'
    600. |- In (Successor(y), m) // so that x' belongs to m
    
    700. |- all y in N (  not ( ( y = Sum(x,y) ) ) ) // therefore, the assertion holds for all y.
    qed
}

// if y != z then x + y != x + z
theorem Theorem_008()
{
    impl ( not( (y = z )) , not ( (Sum(x,y) = Sum(x,z)) )  )
}

prf Theorem_008$1
{
    // Proof by Induction (to prove: `all n in N ( p(n) )`)
    // Strategy: Prove the "base case": p(1)
    // Then do the "inductive step": Prove that if `p(n)` is true, then also `p( Sum(n,1) )` is true.
    
    dec 
         ~y,z : N // consider a fixed y and a fixed z such that y != z
         assert not ( ( y = z ) )
         ~m : Set // and let m be the set of all x for which x + y != x + z
         ~x : N
         assert impl ( not( (y = z) ) , not ( ( Sum(x,y) = Sum(x,z) ) )  )
    ;

    // "base case" 
    100. Axiom3 |- not ( ( Successor(y) = Successor(z) ) ) // y' != z'
    150. |- not ( (Sum(1,y) = Sum(1,z)) ) // 1 + y != 1 + z
    200. |- In(1,m) // hence, 1 belongs to m
    
    // "inductive step" 
    300. 200. |- ass ex x in N ( In (x,m) ) // if x belongs to m
    400. |- not ((Sum(x,y) = Sum(x,z))) // then x + y != x + z
    500. Axiom3 |- not ( (Successor(Sum(x,y)) = Successor(Sum(x,z)) ) ) // hence (x+y)' != (x+z)'
    550. |- not ((Sum(Successor(x),y) = Sum(Successor(x),z))) // x' + y != x' + z
    600. |- In (Successor(y), m) // so that x' belongs to m
    
    700. |- all x,y,z in N ( impl ( not( (y = z) ) , not ( (Sum(x,y) = Sum(x,z)) )  ) ) // therefore, the assertion holds always.
    qed
}

theorem Theorem_009()
{
    declaration
        ~x,y: N // for given x and y, exactly one of the following must be the case:
        ~case1,case2,case3:predicate
        case1 := (x = y) // x = y
        case2 := ex u in N (x = y + u) ) // there exists a u (exactly one, by theorem 8) such that x = y + u
        case3 := ex v in N (y = x + v) ) // there exists a v (exactly one, by theorem 8) such that y = x + v
    ;
    // PoC note: a predicative expression that becomes true "if and only if exactly one of the cases is true" 
    // PoC note: FPL allows abbreviation of complex predicates while using them as boolean variables in boolean expressions (reducing PL1 to PL0)
    and
    (
        // PoC note: the cases are mutually exclusive
         not (and (case1, case2) ) 
        ,not (and (case1, case3) ) 
        ,not (and (case2, case3) ) 
        // and at least one of them is true
        ,or (case1, case2, case3)
    ) 
    
}

// PoC note: Landau now splits his proof into two parts A and B. In part A, he shows that the three cases are mutually exclusive. 
// In part B, he shows that at least one of the cases takes place.   

proof Theorem_009$1
{

    // Part A
    100. |- assume and (case1 , case2) 
    110. Theorem_007 |- false // By theorem 7, cases (1) and (2) are incompatible
    120. |- revoke 100.
    130. |- assume and (case1 , case3) 
    140. Theorem_007 |- false // Similarly, cases (1) and (3) are incompatible
    150. |- revoke 130.
    160. |- assume and (case2, case3)
    170. Theorem_007 |- 
            (
            x = Sum(y,u)     // The incompatibility of (2) and (3) also fallows from Theorem 7; for otherwise, we would have x = y + u
            = Sum(Sum(x,v),u) // = (x+v) + u
            = Sum(x,Sum(v,u)) // = x + (v+u)
            = Sum(Sum(v,u),x) // = (v+u) + x
            )
    180. |- revoke 170. 
    200. 120., 150., 180. |- and (not (and (case1 , case2)), not (and (case1 , case3)), not (and (case2 , case3))) // Therefore, we can have at most one of the cases (1), (2), (3).
    
    // Part B (by induction)
    declaration
        ~x,y: N // let x be fixed
        ~m: Set // and let m b the set of all y 
        assert
            impl
            (
                or(case1, case2, case3) // for which one (hence by (A), exactly one) of the cases (1), (2), and (3) obtains.
                ,
                In (y,m)
            )
    ;
    300. Theorem_003 |- xor ( (x = 1), (x = Successor(u) = Sum(1,u)) ) // For y = 1, we have by theorem 3 that either x = 1 = y or x = u' = 1 + u = y + u.
    400. |- In(1,m) // Hence, 1 belongs to m.
    
    500a. 400. |- assume ex y (In (y,m))
    510a. |- assume case1 // either (case (1) for y)
    520a. |- (Successor(y) = Sum(y,1) = Sum(x,1) ) // hence y' = y + 1 = x + 1
    530a. |- In (Successor(y), m)

    510b. |- assume case2 // or (case (2) for y)
    520b. |- impl ( (u=1), (x = Sum(y,1) = Succesor(y)) ) // hence if u = 1 then x = y + 1 = y'
    530b. Theorem_003 |- 
        impl 
        ( 
            not ( (u=1) ), // but if u != 1 then, by theorem 3, 
            and
            (
                (u = Successor(w) = Sum(1,w)) // u = w' = 1 + w
                ,
                (x = Sum(y,Sum(1,w)) = Sum(Sum(y,1),w) = Sum(Successor(y), w)) // x = y + (1+w) = (y+1) + w = y' + w
            )
        ) 
    540c. 520b., 530b. |- In (Successor(y), m)

    510c. |- assume case3 // or (case (3) for y)
    520c. |- ( Successor(y) = Successor(Sum(x,v)) = Sum(x,Successor(v)) ) // y' = (x+v)' = x+v' (case 3 for y')
    530c. |- In (Successor(y), m)
    
    600. 530a., 540b., 530c. |- or (case1, case2, case3) // In any case, y' belongs to m. Therefore, we always have one of the cases (1), (2), and (3).
    
}


;"""



let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

let result1 = run (ast .>> eof) input
printf "%O" result1

printf "\n--------------------------------\n"

