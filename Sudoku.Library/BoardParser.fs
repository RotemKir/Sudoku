namespace Sudoku.Library

module BoardParser =
    open System
    open System.IO
    open Rotem.Framework.Common
    open BoardModel

    // Private functions

    let private parseCell boardSize (flattenedLines:string[]) rowIndex colIndex =
        let position = { RowIndex = rowIndex ; ColIndex = colIndex}
        let flatPosition = rowIndex * boardSize + colIndex
        match flattenedLines.[flatPosition] with
        | Integer value -> FullCell (position, value)
        | _ -> EmptyCell (position, new CellValues ([1..boardSize]))

    let private flattenLines = 
        Array.collect (fun (line:string) -> line.Split(',')) 

    let private parseCells boardSize boardLines =
        flattenLines boardLines 
        |> parseCell boardSize 
        |> Array2D.init boardSize boardSize 
        
    let private parseBoard boardSize boardLines =
        {
            Size = boardSize
            SquareSize = Math.Sqrt(float boardSize) |> int
            Cells = parseCells boardSize boardLines
        }

    let private readBoardFile fileName = 
        let lines = File.ReadAllLines fileName
        (
            lines |> Array.head |> int, 
            lines |> Array.tail
        )
    
    // Public functions

    let createFromFile fileName = readBoardFile fileName ||> parseBoard

