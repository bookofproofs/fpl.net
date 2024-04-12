open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FplInterpreterTypes
open FParsec
open System.Text.RegularExpressions
open System.Collections.Generic




let input = """

// defines a fixed length tuple template using a generic type tplIndex for an index in a range
definition cl Tuple: obj
{
    dec 
        ~myLength: Nat
    ;
    ctor Tuple(listOfTpl:+ tpl)
    {
        dec 
            ~elem: tpl
            ~i: Nat 
            i:= Zero() 
            for elem in listOfTpl
            {
                i:=Succ(i)
                self[i]:=elem
            }
            myLength:=i
            base.obj()
        ;
        self
    }

    ctor Tuple(arr: tpl[from, length:Nat])
    {
        
        dec 
            self:=arr
            myLength:=length
            base.obj()
        ;
        self
    }

    property Nat Length()
    {
        dec
            self:= myLength
        ;
        self
    }

}

// defines a tuple template starting from a specific index with an arbitrary length
definition class UnlimitedTupleFrom: obj
{
    dec 
        ~myX:+ tpl
    ;

    constructor UnlimitedTupleFrom(from:index, x:+ tpl)
    {
        dec
            base.obj()
            myX:=x
        ;
        self
    }

    property tpl Coord(i: Nat)
    {
        dec
            self:=x[i]
        ;
        self
    }
}

def class CartesianProduct: Set
{

    dec
        ~myLength: Nat
        ~myFrom: Nat
    ;

    constructor CartesianProduct(setList:+ Set)
    {
        dec
            ~setItem: Set
            ~i: Nat
            base.Set()
            i:= Zero()
            for setItem in setList
            {
                i:=Succ(i)
                self[i]:=setItem
            }
            myFrom := Nat(1)
            myLength:=i
        ;
        self
    }

    ctor CartesianProduct(setArray: Set[from , to:Nat])
    {
        dec
            self:=setArray
            myLength:=to
            base.Set()
        ;
        self
    }

    property Nat Length()
    {
        dec
            self:= myLength 
        ;
        self
    }

    property pred AllTuplesIn()
    {
        dec 
            ~tupleElem: Set
            ~i: Nat
        ;
        and 
        (
            is(@self,Set),
            all tupleElem
            {
                impl 
                (
                    In(tupleElem,@self),
                    and (
                        is(tupleElem,Tuple[myFrom,myLength:Nat]),
                        all i in Rang(1,myLength)
                        {
                            In(tupleElem[i],@self[i]) 
                        }
                    )
                )
            }
        )
    }

}

def class Relation: Set
{
    dec 
        ~myArity: Nat
    ;

    ctor Relation(setList:+ Set)
    {
        dec
            ~cartProd : CartesianProduct
            base.Set()
            cartProd := CartesianProduct(setList)
            myArity := cartProd.Length()
            self := Subset(cartProd)
        ;
        self
    }

    property Nat Arity()
    {
        dec self:= myArity;
        self
    }
}

def class BinaryRelation: Relation
{
    dec 
        ~myDomain, myCodomain: Set
    ;


    ctor BinaryRelation(x,y: Set)
    {
        dec
            base.Relation(2,x,y)
            myDomain:=x
            myCodomain:=y
        ;
        self
    }

    property Set Domain()
    {
        dec
            self:=myDomain
        ;
        self
    }

    property Set CoDomain()
    {
        dec
            self:=myCodomain
        ;
        self
    }

    property pred RightUnique()
    {
        all v,w1,w2 in Set
        {
            impl
            (
                and
                (
                    In(v,Domain()),
                    In(w1,CoDomain()),
                    In(w2,CoDomain()),
                    AreRelated(v,w1,@self),
                    AreRelated(v,w2,@self)
                ),
                ( w1 = w2 )
            )
        }
    }

    property pred LeftUnique()
    {
        all v1,v2,w in Set
        {
            impl
            (
                and
                (
                    In(v1,Domain()),
                    In(v2,Domain()),
                    In(w,CoDomain()),
                    AreRelated(v1,w,@self),
                    AreRelated(v2,w,@self)
                ),
                ( v1 = v2 )
            )
        }
    }

    property pred Injective()
    {
        LeftUnique()
    }

    property pred Unique()
    {
        and (LeftUnique(), RightUnique())
    }

    property pred RightTotal()
    {
        all w in Set
        {
            ex v in Set
            {
                and
                (
                    In(v,Domain()),
                    In(w,CoDomain()),
                    AreRelated(v,w,@self)
                )
            }
        }
    }

    property pred Surjective()
    {
        RightTotal()
    }

    property pred LeftTotal()
    {
        all v in Set
        {
            ex w in Set
            {
                and
                (
                    In(v,Domain()),
                    In(w,CoDomain()),
                    AreRelated(v,w,@self)
                )
            }
        }
    }

    property pred Total()
    {
        and (LeftTotal(), RightTotal())
    }
}

def pred AreRelated(u,v: Set, r: BinaryRelation)
{
    dec
        ~tuple: Tuple[i,j:ind]
        tuple:=Tuple($1,$2)
    ;
    and
    (
        In(tuple,r),
        In(u,r.Domain()),
        In(v,r.Codomain())
    )
}

definition class Function: BinaryRelation
{
    ctor Function(x,y: Set)
    {
        dec
            base.BinaryRelation(x,y)
            assert self.LeftTotal()
            assert self.RightUnique()
        ;
        self
    }
}

definition class EquivalenceRelation: BinaryRelation
{
    ctor EquivalenceRelation(x,y: Set)
    {
        dec
            self:=BinaryRelation(x,y)
            assert self.Reflexive()
            assert self.Symmetric()
            assert self.Transitive()
        ;
        self
    }
}

;"""

let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
let parsedAsts = System.Collections.Generic.List<ParsedAst>()
FplInterpreter.fplInterpreter input (System.Uri("file:///d%3A/Forschung/fpl.net/theories/Landau/Test.fpl")) fplLibUrl parsedAsts
printf "%A" parsedAsts

printf "\n--------------------------------\n"
ad.PrintDiagnostics

printf "\n--------------------------------\n"


let mutable test = []
test <- test @ [""]