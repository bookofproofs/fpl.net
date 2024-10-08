﻿# Documentation of the FPL Symbol Table
## What is the FPL Symbol Table
The symbol table (ST) is a pivotal data structure that the FPL interpreter constructs while traversing the abstract syntax tree (AST) generated by the FPL parser. It is a dynamic structure that undergoes state changes during the traversal. The FPL interpreter leverages the current state of the ST to emit diagnostics as it traverses the AST, making it an integral part of the interpreter's operation.

## Goal of this document
This document focuses on the ST's dynamic aspects and state changes that are not apparent from the code.

## Global Structure of the ST 
The F# type `SymbolTable` is defined in the `FplInterpreterTypes` module. It is a recursive tree structure of the F# type `FplValue` defined in the same module. Each node of the ST is of the `FplValue` type but can have different purposes and attributions. One such key attribution is the `FplValueType` of each node. For instance, the ST starts with a `FplValueType.Root` node. Each of the `FplValue` nodes of the ST tree has two sequences - `Scope` and `ValueList`, which may contain further `FplValue` nodes with a different `FplValuType`. This structure is essential to understand the organization and functionality of the ST.

## How to read this Description of the ST
Since the ST is constructed during the AST traversal, it depends on the AST types being traversed. The titles of the following sections will be titled by concatenating the `FplValueType`s of the nodes of the ST, that are constructed 

### ST.Root
**Description**: The root of the ST

**Created in**: In `FplInterpreterTypes.SymbolTable` constructor

**Output**: 
```
	Root
	|-Name: ""
	|-Parent: None
	|-Scope: <empty>
	|-ValueList: <empty>
```

### ST.Root.Theory
**Created in**: In `FplInterpreterBuildingBlocks.evaluateSymbolTable` loop over all theories 

**Output (parent)**: 
```
	Root
	|-Name: ""
	|-Parent: None
	|-Scope: Contains a non-empty <string,FplValue> dictionary of all here created Theory nodes
	|-ValueList: <empty>
```

**Output**: 
```
	Theory
	|-Name: <Name>
	|-Parent: Some Root
	|-Scope: Contains a (possibly) empty <string,FplValue> dictionary of all FPL blocks inside this theory
	|-ValueList: <empty>
```

### ST.Root.Theory.Class
**Created in**: In `AST.Namespace.DefinitionClass`

**Output (parent)**: 
```
	Class
	|-Name: "<Name>"
	|-Parent: Theory
	|-Scope: Contains a non-empty <string,FplValue> dictionary of nodes of 
		all variables, constructors and properties of the class 
	|-ValueList: Contains (a possibly empty) list of nodes, from which the class
		inherits. These can be class nodes or primitive objects. The values are
		only added if they were previously declared in the code.
```

* *Context*: `InSignature` - the ValueList and the Name get constructed 
* *Context*: `InBlock` - the Scope gets constructed 

### ST.Root.Theory.Class.Constructor
**Created in**: In `AST.Namespace.DefinitionClass.DefClassCompleteContent.Constructor`

**Output (parent)**: 
```
	Constructor
	|-Name: "<Name>"
	|-Parent: Class
	|-Scope: Contains a non-empty <string,FplValue> dictionary of all scope 
		nodes (like variables) declared in the constructor.
	|-ValueList: Contains (a possibly empty) list of reference nodes that
		represent the calls to some base classes constructors. Due to semantical
		errors in the code, the latter do not necessarily have to match the
		signatures of the actual constructors of the base classes of this constructor class.
		The latter can be retrieved from the parent of the constructor - its class node (see ST.Root.Theory.Class).
```

* *Context*: `InConstructorSignature` - the Scope and the Name get constructed 
* *Context*: `InConstructorBlock` - the ValueList gets constructed 

