// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Sudoku.Library
open Rotem.Framework.Logging
open System
open Rotem.Framework.Counters.Export

let createLogger fileName = Logger.create [ConsoleLogger.Log ; FileLogger.Create <| fileName + ".log"]

let loadBoard fileName = fileName |> BoardParser.createFromFile 

let printSolveResult board =
    if Board.isSolved board
    then sprintf "\nSolved board:\n%s" <| Board.print board
    else sprintf "\nFailed to solve board:\n%s" <| Board.print board

let init =
    printfn "Please enter file to solve:"
    let fileName = Console.ReadLine()
    let board = loadBoard fileName
    let logger = createLogger fileName
    sprintf "Solving board:\n%s\n" <| Board.print board |> logger
    (board, logger)

let exportValues = 
    [
        getTargetNameCell
        getCounterValueCell "Runs"
        getCounterValueCell "Time" 
        getCounterValueCell "Cells Found"
    ]

let export = exportToCsv "Statistics.csv" exportValues

[<EntryPoint>]
let main _ = 
    let (board, logger) = init

    let (solvedBoard, statistics) = board |> Solver.solve logger
   
    solvedBoard |> printSolveResult |> logger
    statistics |> export  
    
    Console.ReadLine() |> ignore

    0 // return an integer exit code
