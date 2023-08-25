# Amendments to the FPL Grammar 

The original FPL parser was implemented using python and the tatsu compiler compiler that used an EBNF dialect as an input and 


We port the parser now to FParsec and re-implent it from scrach. These gives us the oportunity to slighly redesign the grammar, impacting the Proof of Concept of imlementing a working interpreter for FPL.

The following documentation describes the amendments and provides a rationale behind each of them.


## More stringent predicate syntax

## Classes
Constructors have now an anotated option (which original was also optional but not anotated) to include calls for parent class constructors.

## Self-Containment 

