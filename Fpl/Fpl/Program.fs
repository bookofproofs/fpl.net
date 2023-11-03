open FplGrammarCommons
open ErrRecovery
open FplParser
open FParsec
open System.Text.RegularExpressions


let input = """Fpl 
{
s     def class FieldPowerN: Sets 
    {
        ctor FieldPowerN ()
        {
            dec 
                myField := field 
                self:=SetBuilder( myField[1 ~ n], true) 
                self!Set() 
            ;
            self 

        }
    }
s
    def class FieldPowerN: Sets 
    {
        ctor FieldPowerN ()
        {
            dec 
                myField := field 
                self:=SetBuilder( myField[1 ~ n], true) 
                self!Set() 
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

