// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Sudoku.Library
open Rotem.Framework.Logging
open System

let createLogger fileName = Logger.create [ConsoleLogger.Log ; FileLogger.Create <| fileName + ".log"]

[<EntryPoint>]
let main _ = 
    printfn "Please enter file to solve:"
    let fileName = Console.ReadLine()
    let logger = createLogger fileName

    fileName
    |> BoardParser.createFromFile 
    |> Solver.solve logger
    |> Board.print
    |> logger

    Console.ReadLine() |> ignore

    0 // return an integer exit code
