// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Sudoku.Library
open Rotem.Framework.Logging
open System

[<EntryPoint>]
let main _ = 

    BoardParser.createFromFile "C:\Dev\3.txt"
    |> Solver.solve
    |> Board.print
    |> ConsoleLogger.Log 

    Console.ReadLine()

    0 // return an integer exit code
