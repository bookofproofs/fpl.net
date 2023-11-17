﻿open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = """Fpl.Arithmetics 
{
    /* This is a formulation of Edmund Landau's "Foundations of Analysis" (1930 Leipzig) in FPL (the Formal Proving Language).
    The namespace serves as a "proof of concept" for the language. */

    definition class N: object
    {
        intrinsic
    }
    
    
    definition class One: object
    {
        intrinsic
    }
    
    
    // 1 is a natural number
    axiom Axiom1 ()
    {
        is (One, N) 
    }
    


    // For each x there exists exactly one natural number, called the successor of x, the will be denoted x
    axiom Axiom2 ()
    {
        all x in N
        (
            exn x
            (
                p(x)
            )
            
            
        )
        
    }
    
    
    
}
"""

let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

printf "\n--------------------------------\n"

