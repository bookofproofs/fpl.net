open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FParsec
open System.Text.RegularExpressions






let input = """uses TestNamespace
def class Set: obj
{
    intr
}
uses Fpl.SetTheory

// "in relation" ("is element of") relation
def pred In infix "in" (x,y: Set)
{
    intr
}
uses TestNamespace *
def pred IsEmpty(x: Set)
{
    all y in Set
    (
        not In(y, x) 
    )
}
uses TestNamespace1.TestNamespace2 *
// existence of an empty set
axiom EmptySetExists()
{
    ex x in Set
    (
        IsEmpty(x)
    )
}
uses TestNamespace alias T1
// introduction of a new mathematical object
def class EmptySet: Set
{
    ctor EmptySet()
    {
        dec 
            base.obj()
            assert IsEmpty(self) 
        ;
        self
    }
}
uses TestNamespace1.TestNamespace2 alias T2
// relation between a subset and a superset
def pred IsSubset(subset,superset: Set)
{
    all u in Set
    (
        impl (In(u, subset), In(u, superset))
    )
}
;"""

let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

let interpret = FplInterpreter.fplInterpreter result (System.Uri("."))
printf "%A" interpret

printf "\n--------------------------------\n"
ad.PrintDiagnostics

printf "\n--------------------------------\n"

