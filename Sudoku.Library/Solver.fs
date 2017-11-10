namespace Sudoku.Library

[<RequireQualifiedAccess>]
module Solver =
    open Rotem.Framework.StateMachine
    open Rotem.Framework.Common
    open BoardModel
    open System

    // Types

    type SolverAction = RemoveValueFromCell | SetValueInCell of cells : Cell List

    // Private functions
    
    let private (|?) a b =
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
    
    let private removeCellValuesState = { Name = "Remove Cell Values"; Action = fullCellsFold removeCellValue}
    let private setFullCellsState = {Name = "Set Full Cells"; Action = emptyCellsFold setValueIfCellHasOneValue}
    let private setFullCellIfOnlyValueInRowsState = {Name = "Set Full Cell If Only Value In Rows"; Action = setFullCellIfOnlyValueInGroups Board.getRow}
    let private setFullCellIfOnlyValueInColumnsState = {Name = "Set Full Cell If Only Value In Columns"; Action = setFullCellIfOnlyValueInGroups Board.getColumn}
    let private setFullCellIfOnlyValueInSquaresState = {Name = "Set Full Cell If Only Value In Squares"; Action = setFullCellIfOnlyValueInGroups Board.getSquareByIndex}
    let private removeValueByGroupsInSquaresState = {Name = "Remove Value By Groups In Squares"; Action = squaresFold removeValueByGroupsInSquare}

    let private (|IsSetValueInCell|IsRemoveValueFromCell|IsNone|) (_, action)=
        match action with
        | Some (SetValueInCell _) -> IsSetValueInCell 
        | Some RemoveValueFromCell -> IsRemoveValueFromCell 
        | None -> IsNone
        
    let private stateMachine =
        {
            TransitionFunction = (fun state  -> 
                match state with
                | IsState removeCellValuesState true -> Some setFullCellsState
                | IsState setFullCellsState true & IsSetValueInCell -> Some removeCellValuesState 
                | IsState setFullCellsState true -> Some setFullCellIfOnlyValueInRowsState
                | IsState setFullCellIfOnlyValueInRowsState true & IsSetValueInCell -> Some removeCellValuesState 
                | IsState setFullCellIfOnlyValueInRowsState true -> Some setFullCellIfOnlyValueInColumnsState
                | IsState setFullCellIfOnlyValueInColumnsState true & IsSetValueInCell -> Some removeCellValuesState 
                | IsState setFullCellIfOnlyValueInColumnsState true -> Some setFullCellIfOnlyValueInSquaresState
                | IsState setFullCellIfOnlyValueInSquaresState true & IsSetValueInCell -> Some removeCellValuesState 
                | IsState setFullCellIfOnlyValueInSquaresState true -> Some removeValueByGroupsInSquaresState
                | IsState removeValueByGroupsInSquaresState true & IsRemoveValueFromCell -> Some setFullCellsState
                | _ -> None
                )
            Start = removeCellValuesState
        }
    
    let private printCellOverride cellsToMark cell text =
        if Seq.contains cell cellsToMark 
        then String.collect (fun c -> match c with | ' ' -> "*" | _ -> c.ToString()) text 
        else text

    let private printSetValuesInCells board cells =
       (cells |> Seq.map (sprintf "Set cell value %O") |> String.concat "\n")
        + "\n" 
        + (Board.printWithOverride board <| printCellOverride cells) 
        + "\n" 

    let private postState logger board action =
        match action with
        | Some (SetValueInCell cells) -> logger <| printSetValuesInCells board cells
        | _ -> ignore()
    
    // Public functions
    
    let solve logger board = run stateMachine <| logRunningState logger <| postState logger board <| board