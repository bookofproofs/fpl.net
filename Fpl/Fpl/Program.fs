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


    def class FieldPowerN: Sets 
    {
        dec
            ~myField: Field  
            ~addInField: BinOp 
            ~mulInField: BinOp 
        ;
s
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
  

    lemma VecAddIsCommutative()
    {
        dec
            ~to: Nat
            ~x,y: tplFieldElem[1~to]
            ~fieldPowerN: FieldPowerN
                (
                field: Field(f: tplFieldSet, opAdd, opMul: BinOp(a,b: tplFieldElem)),
                n:Nat
                )
            ~vecAdd: VecAdd(v,w: tplFieldElem[from~to])
        ;
        pre:
            and
            (
                In(x, fieldPowerN),
                In(y, fieldPowerN)
            )
        con:
            vecAdd.IsCommutative()
    }

    def class ZeroVectorN: Tuple
    {
        ctor ZeroVectorN(n: Nat, field: Field)
        {
            dec
                ~i: Nat 
                for i in [1~n] 
                (
                    self<i>:=field.AdditiveGroup().NeutralElement()
                )
                self!Tuple()
            ;
            self
        }
    }
     
}

"""

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

