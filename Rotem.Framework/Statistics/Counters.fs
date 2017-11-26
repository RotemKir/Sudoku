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

    type TargetCounters = seq<CounterName * CounterValue>

    // Private functions
    
    let private setCounterValue counterName (counters : Counters) value =
        Map.add counterName value counters

    let private incrementCounterStrategy initialValue getNextValue counterName counters : Counters =
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

    let incrementCounterByAmount amount = incrementCounterStrategy <| initialNumberCounterValue amount <| getNextNumberCounterValue amount
    
    let incrementCounter = incrementCounterByAmount 1

    let incrementDiffCounter amount = incrementCounterStrategy <| initialDiffCounterValue amount <| getNextDiffCounterValue amount

    let incrementTimeCounter = incrementCounterStrategy <| initialTimeCounterValue <| getNextTimeCounterValue
    
    let createStatistics : Counters = Map.empty<CounterName, CounterValue>

    let getCounterValue counterValue =
            match counterValue with
            | NumberCounter value -> value
            | DiffCounter (_, value) -> value
            | TimeCounter (_, value) -> value.TotalMilliseconds |> int
        
    module Export =
        open System.IO

        // Types
        
        type StatCellValue = NumberCell of int | StringCell of string

        type StatCell = 
            {
                CounterName : string
                CellValue : StatCellValue
            }

        // Private functions 

        let private writeToFile fileName contents = File.WriteAllText(fileName, contents)

        let private getTargetCells counters =
            Seq.map (fun getter -> getter counters)

        let private getCountersByTarget (counters : Counters) =
            counters
            |> Map.toSeq
            |> Seq.groupBy (fun (counterName, _) -> snd counterName)
            
        let private export exporter cellGetters counters : unit =
            counters
            |> getCountersByTarget 
            |> Seq.map (fun (_, targetCounters) ->  getTargetCells targetCounters cellGetters)
            |> exporter
                   
        let private getCsvRow getValues (cells : seq<StatCell>) = 
            String.Join(",", Seq.map getValues cells)

        let private getCsvHeader cells = 
            cells
            |> Seq.head
            |> getCsvRow (fun cell -> cell.CounterName)

        let private getCellDisplayValue cell =
            match cell with
            | {CounterName = _ ; CellValue = StringCell s} -> s
            | {CounterName = _ ; CellValue = NumberCell n} -> string n

        let private getCsvLine = getCsvRow getCellDisplayValue 

        let private getCsvLines = Seq.map getCsvLine
        
        let private csvExporter fileName cells = 
            Seq.fold (fun s line -> s + "\n" + line ) 
            <| getCsvHeader cells
            <| getCsvLines cells
            |> writeToFile fileName

        // Public functions 

        let exportToCsv fileName = csvExporter fileName |> export 

        let getTargetNameCell (counters : TargetCounters) =
            let counterName = counters |> Seq.head |> fst
            { 
                CounterName = "Target"
                CellValue = counterName |> snd |> StringCell
            }

        let getCounterValueCell counterName (counters: TargetCounters) =
            let counterValue = 
                counters
                |> Seq.find (fun (name, _) -> fst name = counterName)
                |> snd
            { 
                CounterName = counterName
                CellValue = counterValue |> getCounterValue |> NumberCell 
            }
