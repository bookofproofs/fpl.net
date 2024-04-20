open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FplInterpreterTypes
open System.IO
open FParsec
open System.Text.RegularExpressions
open System.Collections.Generic

let deleteFilesWithExtension dir extension =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*." + extension)
        |> Array.iter File.Delete
    else
        printfn "Directory %s does not exist." dir


let prepareFplCode(fplCode:string, delete:bool) =
    FplParser.parserDiagnostics.Clear()
    let currDir = Directory.GetCurrentDirectory()

    File.WriteAllText(Path.Combine(currDir, "Test.fpl"), fplCode)
    let uri = System.Uri(Path.Combine(currDir, "Test.fpl"))
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    if delete then 
        deleteFilesWithExtension currDir "fpl"
        None
    else
        let parsedAsts = ParsedAstList()
        Some (FplInterpreter.fplInterpreter fplCode uri fplLibUrl parsedAsts true)

let loadFplFile(path:string) = 
    let uri = System.Uri(path)
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    let parsedAsts = ParsedAstList()
    let fplCode = File.ReadAllText(path)
    Some (FplInterpreter.fplInterpreter fplCode uri fplLibUrl parsedAsts false)


let input = """inference ModusPonens()
{
    dec
        ~p,q: pred
    ;
    premise:
        and (p, impl (p,q) )
    conclusion:
        q
}

inference ProceedingResults(p:+ pred)
{
    dec
        ~proceedingResult: pred
    ;
    premise: 
        all proceedingResult in p
        {
            proceedingResult
        }

    conclusion:
        and (p)
}

inference ExistsByExample(p: pred(c: obj, other:* obj))
{
    dec
        ~x: obj
    ;
    premise:
        p(c, other)
    conclusion:
        ex x {p(x, other)}
}

def class A: obj
{
    intr
}

def class B: obj
{
    intr
}

def class C: obj
{
    intr
}

def pred Greater infix ">" (x,y: obj)
{
    intr
}

axiom GreaterAB()
{
    dec
        ~a: A
        ~b: B
    ;
    (a > b)
}

axiom GreaterBC()
{
    dec
        ~b: B
        ~c: C
    ;
    (b > c)
}

axiom GreaterTransitive()
{
    dec
        ~x,y,z: obj
    ;
    impl
    (
        and
        (
            (x > y), (y > z)
        ),
        (x > z)
    )
}

lemma Example4()
{
    dec
        ~x,y,z: obj
    ;
    ex x 
    { 
        and ((x > y), (x > z)) 
    }
}

proof Example4$1
{
    dec
        ~a:A
        ~b:B
        ~c:C
        ~x,y,z: obj
    ;
    1. GreaterAB |- (a > b) 
    2. GreaterBC |- (b > c) 
    3. 1., 2. |- and ((a > b), (b > c)) 
    4. 3., GreaterTransitive |- impl ( and ((a > b), (b > c)), (a > c) ) 
    5. 4., ModusPonens |- (a > c)
    6. 5., 1. |- and ((a > c), (a > b)) 
    7. 6., ExistsByExample(and((a > c), (a > b))) |- 
        ex x 
        { 
            and ((x > y), (x > z)) 
        }
    qed
}

;"""

(*let result = fplParser input

printf "%O" result

ad.PrintDiagnostics
*)

let st = loadFplFile(@"D:\Forschung\fpl.net\theories\lib\Fpl.SetTheory.fpl")
(*
prepareFplCode("",true) |> ignore
let st = prepareFplCode(input,true) 
*)

printf "\n--------------------------------\n"
ad.PrintDiagnostics

printf "\n--------------------------------\n"



