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

    let private setCounterStrategy initialValue getNextValue counters counterName : Counters =
       let setCounter = setCounterValue counterName counters
       match Map.containsKey counterName counters with
       | true -> 
            match Map.find counterName counters |> getNextValue with 
            | Some value ->  value |> setCounter  
            | _ -> counters
       | false ->  initialValue() |> setCounter
    
    let private initialNumberCounterValue () = NumberCounter 1
    let private initialDiffCounterValue value () = DiffCounter (Some value, 0)
    let private initialTimeCounterValue () = TimeCounter (Some DateTime.Now, TimeSpan.Zero)

    let private getNextNumberCounterValue counter =
        match counter with
        | NumberCounter i -> Some <| NumberCounter (i + 1) 
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

    let incrementCounter = setCounterStrategy initialNumberCounterValue getNextNumberCounterValue

    let incrementDiffCounter value = setCounterStrategy <| initialDiffCounterValue value <| getNextDiffCounterValue value

    let incrementTimeCounter = setCounterStrategy <| initialTimeCounterValue <| getNextTimeCounterValue
    
    let createStatistics : Counters = Map.empty<CounterName, CounterValue>