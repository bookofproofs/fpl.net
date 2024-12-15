module FplInterpreterInterpreterRunner
open FplInterpreterTypes
open System.Collections.Generic
open System

type FplRunner() =
    let _stack = Stack<FplValue>()

    // The stack memory of the runner collecting the variables of the current Runner
    member this.Stack = _stack

    member this.Run(fv:FplValue) = 
        match fv.BlockType with 
        | FplValueType.Reference ->
            if fv.Scope.ContainsKey(fv.FplId) then 
                _stack.Push(fv)
                
                _stack.Pop() |> ignore
        | _ -> ()
        