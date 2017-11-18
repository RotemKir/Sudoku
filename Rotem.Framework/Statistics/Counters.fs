namespace Rotem.Framework

module Counters =
    open System

    // Types

    type CounterValue = 
        NumberCounter of int 
        | TimeCounter of prevTime : DateTime option * TimeSpan 
        | DiffCounter of prevAmount : int option * int

    type CounterName = string * string

    type Counters = Map<CounterName, CounterValue>

    // Private functions
    
    let private setCounterValue counterName (counters : Counters) value =
        Map.add counterName value counters

    let private setCounterStrategy initialValue getNextValue counterName counters : Counters =
       let setCounter = setCounterValue counterName counters
       match Map.containsKey counterName counters with
       | true -> 
            match Map.find counterName counters |> getNextValue with 
            | Some value ->  value |> setCounter  
            | _ -> counters
       | false ->  initialValue() |> setCounter
    
    let private initialNumberCounterValue amount () = NumberCounter amount
    let private initialDiffCounterValue amount () = DiffCounter (Some amount, 0)
    let private initialTimeCounterValue () = TimeCounter (Some DateTime.Now, TimeSpan.Zero)

    let private getNextNumberCounterValue amount counter =
        match counter with
        | NumberCounter prevAmount -> Some <| NumberCounter (prevAmount + amount) 
        | _ -> None

    let private getNextDiffCounterValue amount counter =
        match counter with 
        | DiffCounter (Some prevAmount, total) ->  Some <| DiffCounter (None, total + amount - prevAmount)
        | DiffCounter (None, total) -> Some <| DiffCounter (Some amount, total)
        | _ -> None

    let private getNextTimeCounterValue counter =
        match counter with 
        | TimeCounter (Some prevTime, total) ->  Some <| TimeCounter (None, total + (DateTime.Now - prevTime))
        | TimeCounter (None, total) -> Some <| TimeCounter (Some DateTime.Now, total)
        | _ -> None

    // Public functions

    let incrementCounterByAmount amount = setCounterStrategy <| initialNumberCounterValue amount <| getNextNumberCounterValue amount
    
    let incrementCounter = incrementCounterByAmount 1

    let incrementDiffCounter amount = setCounterStrategy <| initialDiffCounterValue amount <| getNextDiffCounterValue amount

    let incrementTimeCounter = setCounterStrategy <| initialTimeCounterValue <| getNextTimeCounterValue
    
    let createStatistics : Counters = Map.empty<CounterName, CounterValue>