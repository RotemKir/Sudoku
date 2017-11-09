// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Sudoku.Library

[<EntryPoint>]
let main _ = 

    let board = BoardParser.createFromFile "C:\Dev\3.txt"
    Solver.solve board

    0 // return an integer exit code
