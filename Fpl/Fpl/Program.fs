open FplGrammarTypes
open ErrRecovery
open FplGrammar
open FParsec


let input = """TestNamescpace {   
    theory {       
        pred I() {
            x: pred
        }
    } 
}"""
let result = fplParser input

printf "%O" result
ad.PrintDiagnostics

