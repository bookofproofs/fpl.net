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

let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
let interpret = FplInterpreter.fplInterpreter input (System.Uri("file:///d%3A/Forschung/fpl.net/theories/Landau/Test.fpl")) fplLibUrl
printf "%A" interpret

printf "\n--------------------------------\n"
ad.PrintDiagnostics

printf "\n--------------------------------\n"

open System
open System.IO

let uri = new Uri("file:///d%3A/Forschung/fpl.net/theories/Landau/Test.fpl")
let localPath = uri.LocalPath
let p = Uri.UnescapeDataString(uri.LocalPath)
let pattern = @"^[\/\\][a-zA-Z]:"
let result1 = 
    if Regex.IsMatch(p, pattern) then
            p.Substring(1)
        else
            p
let directoryName = Path.GetDirectoryName(result1)
printfn "Directory Name: %s" directoryName