namespace Sudoku.Library

[<RequireQualifiedAccess>]
module Solver =
    open Rotem.Framework.Common
    open BoardModel

    // Types

    type SolverAction = RemoveValueFromCell | SetValueInCell of cells : Cell List

    // Private functions
    
    let (|?) a b =
        match a with
        | Some (SetValueInCell cells) -> 
            match b with 
            | Some (SetValueInCell otherCells) -> Some (SetValueInCell <| cells @ otherCells) 
            | _ -> a
        | Some RemoveValueFromCell -> a
        | None -> b

    let private cellsFold fullCellAction emptyCellAction board =
        Board.getAllCells board 
        |> Seq.fold (fun solverAction cell -> 
            match cell with 
            | FullCell (position, value) -> fullCellAction board position value |? solverAction
            | EmptyCell (position, cellValues) -> emptyCellAction board position cellValues |? solverAction) 
            None
    
    let private fullCellsFold action = cellsFold action (fun _ _ _ -> None)

    let private emptyCellsFold action = cellsFold (fun _ _ _ -> None) action

    let private squaresFold action board =
        Board.getAllSquares board
        |> Seq.fold (fun solverAction square -> action board square |? solverAction) None

    let private getCellsWithValue value =
        Seq.filter (fun cell ->
            match cell with
            | EmptyCell (_, cellValues) -> Set.contains value cellValues
            | _ -> false)
    
    let private getCellsAffectedByCell board position =
        Seq.concat [
            Board.getRow board position.RowIndex 
            Board.getColumn board position.ColIndex
            Board.getSquareByPosition board position
        ]

    let private removeCellValue board position value =
        position
        |> getCellsAffectedByCell board
        |> getCellsWithValue value 
        |> Seq.fold (fun _ cell -> 
            Board.removeCellValue board value cell 
            Some RemoveValueFromCell) 
            None

    let private setValueIfCellHasOneValue board position cellValues =
        match cellValues with
        | value when Set.count value = 1 -> 
            Some <| SetValueInCell [Seq.head value |> Board.setCellValue board position ]
        | _ -> None

    let private cellHasValue value cell =
        match cell with
        | EmptyCell (_, cellValues) -> cellValues |> Seq.contains value
        | _ -> false
        
    let private mapValuesToCells board cells =
        [1..board.Size]
        |> Seq.map (fun value -> (value, Seq.filter <| cellHasValue value <| cells))

    let private getCellsWithOnlyValueInGroup board cells =
        mapValuesToCells board cells
        |> Seq.filter (fun (_, cells) -> Seq.length cells = 1)
        |> Seq.map (fun (value, cells) -> (value, cells |> Seq.head))

    let private setFullCellIfOnlyValueInGroup board cells =
        getCellsWithOnlyValueInGroup board cells 
        |> Seq.fold (fun _ (value, cell) ->
            Some <| SetValueInCell [Board.setCellValue board <| Board.getPosition cell <| value])
            None
            
    let private setFullCellIfOnlyValueInGroups getCells board =
        Seq.map <| getCells board <| [0..board.Size - 1]
        |> Seq.fold (fun result cells -> setFullCellIfOnlyValueInGroup board cells |? result) None

    let private getCellsIndices getIndex cells =
        Seq.map Board.getPosition cells
        |> Seq.map (fun position -> getIndex position)
        |> Seq.distinct

    let private getCellsRowIndices = getCellsIndices (fun position -> position.RowIndex)
    
    let private getCellsColIndices = getCellsIndices (fun position -> position.ColIndex)
    
    let private removeValuesFromGroupBySquare getGroupIndices getGroupByIndex board square =
        mapValuesToCells board square
        |> Seq.map (fun (value, cells) -> (value, getGroupIndices cells))
        |> Seq.filter (fun (_, indices) -> Seq.length indices = 1)
        |> Seq.map (fun (value, indices) -> (value, Seq.head indices))
        |> Seq.filter (fun (value, index) -> 
            getGroupByIndex board index
            |> Seq.except square
            |> Seq.exists (cellHasValue value))
        |> Seq.fold (fun _ (value, index) ->
            getGroupByIndex board index
            |> Seq.except square
            |> Seq.iter (Board.removeCellValue board value)
            Some RemoveValueFromCell)
            None

    let private removeValueByRowInSquare = removeValuesFromGroupBySquare getCellsRowIndices Board.getRow
     
    let private removeValueByColumnInSquare = removeValuesFromGroupBySquare getCellsColIndices Board.getColumn

    let private removeValueByGroupsInSquare board square = 
        removeValueByRowInSquare board square 
        |? removeValueByColumnInSquare board square 

    // Public functions
    
    let removeCellValues = fullCellsFold removeCellValue

    let setFullCells = emptyCellsFold setValueIfCellHasOneValue

    let setFullCellIfOnlyValueInRows = setFullCellIfOnlyValueInGroups Board.getRow    
    
    let setFullCellIfOnlyValueInColumns = setFullCellIfOnlyValueInGroups Board.getColumn

    let setFullCellIfOnlyValueInSquares = setFullCellIfOnlyValueInGroups Board.getSquareByIndex
    
    let removeValueByGroupsInSquares = squaresFold removeValueByGroupsInSquare