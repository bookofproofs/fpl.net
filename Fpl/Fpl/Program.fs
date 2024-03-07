﻿open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FParsec
open System.Text.RegularExpressions






let input = """
inf ModusPonens()
{
    dec ~p,q: pred;

    premise:
        and (p, impl (p,q) )
    conclusion:
        q
}

inf ModusTollens()
{
    dec ~p,q: pred;

    premise:
        and (not q, impl(p,q) )
    conclusion:
        not (p)
}

inf HypotheticalSyllogism()
{
    dec ~p,q,r: pred;
    premise:
        and (impl(p,q), impl(q,r))
    conclusion:
        impl(p,r)
}

inf DisjunctiveSyllogism()
{
    dec ~p,q: pred;
    premise:
        and (not(p), or(p,q))
    conclusion:
        q
}

inf ExistsByExample(p: pred(c: obj))
{
    dec ~x: obj;
    premise:  
        p(c)
    conclusion: 
        ex x(p(x))
}


def pred NotEqual infix "<>" (x,y: tpl)
{
    not (x = y) 
}

def pred Implies infix "=>" (x,y: pred)
{
    impl
    (
        x
        ,
        y
    )
}

def pred IfAndOnlyIf infix "<=>" (x,y: pred)
{
    iif
    (
        x
        ,
        y
    )
}

def pred And infix "and" (x:+ pred)
{
    and (x)
}

def pred Or infix "or" (x:+ pred)
{
    or (x)
}

def pred Xor infix "xor" (x:+ pred)
{
    xor (x)
}

localization iif(x,y) :=
    !tex: x "\Leftrightarrow" y
    !eng: x " if and only if " y
    !ger: x " dann und nur dann wenn " y
    ;

loc not(x) :=
    !tex: "\neg(" x ")"
    !eng: "not " x
    !ger: "nicht " x
    ;

loc and(p,q) :=
    !tex: p "\wedge" q
    !eng: p " and " q
    !ger: p " und " q
    ;

loc Equal(x,y) :=
    !tex: x "=" y
    !eng: x " equals " y
    !ger: x " ist gleich " y
    !ita: x " è uguale a " y
    !pol: x " równa się " y
    ;

loc NotEqual(x,y) :=
    !tex: x "\neq" y 
    !eng: x "is unequal" y 
    !ger: x "ist ungleich" y 
    !pol: x ( "nie równa się" | "nie równe" ) y 
    ;

;"""

let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

let interpret = eval result
printf "%s" interpret

printf "\n--------------------------------\n"

