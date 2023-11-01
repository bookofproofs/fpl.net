open FplGrammarCommons
open ErrRecovery
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = """Fpl.LinAlg 
{
    uses Fpl.Commons 
    uses Fpl.Commons.Structures 
    uses Fpl.SetTheory.ZermeloFraenkel
    uses Fpl.Algebra.Structures 

s s s
    def class FieldPowerN: Sets 
    {
        dec
            ~myField: Field   
            ~addInField: BinOp 
            ~mulInField: BinOp 
        ;

        ctor FieldPowerN
        (
            field : Field, 
            n: Nat 
        )
        {
            dec 
                myField := field 
                addInField := myField.AddOp() 
                mulInField := myField.MulOp() 
                assert NotEqual(n, Zero()) 
                self:=SetBuilder( myField[1 ~ n], true)
                self!Set()
            ;
            self 

        }

        mand func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to])   -> tplFieldElem[from ~ to]
        {
            dec 
                ~result: tplFieldElem[from ~ to]  
                result := addInField(v[from ~ to],w[from ~ to])
            ;
            return result
        }

    }
  

     
}



"""

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

