namespace Sudoku.Library

[<RequireQualifiedAccess>]
module Board =
    open System
    open BoardModel

    // Private functions
    
    let private setCell board position = 
        Array2D.set board.Cells position.RowIndex position.ColIndex 
    
    let private getSquare board squareRow squareCol =
        Seq.cast<Cell> board.Cells.[squareRow..squareRow + board.SquareSize - 1, squareCol..squareCol + board.SquareSize - 1]
        
    let private printCell formatter cell : string =
        match cell with
        | FullCell (_, value) ->  string value
        | EmptyCell (_, _) -> "_"
        |> formatter
    
    let private getPrintConfiguration board =
        let maxNumberWidth = string board.Size |> String.length
        let cellSize = maxNumberWidth + 2 // Add space padding to cell size
        let lastIndex = board.Size - 1
        let seperatorLine = String.replicate (board.Size * cellSize + board.Size / board.SquareSize + 1) "-" // # of cells + # of separators (size + 1)
        let printCellWithFormat = printCell (fun value -> " " +  value.PadLeft(maxNumberWidth) + " ")
        (lastIndex, seperatorLine, printCellWithFormat)
 
    // Public functions
    let getPosition cell =
        match cell with
        | FullCell (position, _) -> position
        | EmptyCell (position, _) -> position
    
    let setCellValue board position value = 
        let fullCell = FullCell (position, value)
        setCell board position fullCell
        fullCell 

    let removeCellValue board value cell =
        match cell with
        | EmptyCell (position, cellValues) -> setCell board position <| EmptyCell (position, Set.remove value cellValues)
        | _ -> ignore()

    let getAllCells board = 
        Seq.cast<Cell> board.Cells

    let getRow board rowIndex =
        Seq.ofArray board.Cells.[rowIndex , 0..]

    let getColumn board colIndex =
        Seq.ofArray board.Cells.[0.. , colIndex]

    let getSquareByPosition board position =
        let squareRow = position.RowIndex - position.RowIndex % board.SquareSize
        let squareCol = position.ColIndex - position.ColIndex % board.SquareSize
        getSquare board squareRow squareCol

    let getSquareByIndex board index =
        let squareRow = index / board.SquareSize |> (*) board.SquareSize
        let squareCol = index % board.SquareSize |> (*) board.SquareSize
        getSquare board squareRow squareCol

    let getAllSquares board =
        Seq.map <| getSquareByIndex board <| [0..board.Size - 1]

    let print board =
        let (lastIndex, seperatorLine, printCellWithFormat) = getPrintConfiguration board
    
        let (|FirstColumn|VerticalSeperator|LastColumn|Regular|) position =
            match position with
            | {RowIndex = _ ; ColIndex = 0} -> FirstColumn
            | {RowIndex = _ ; ColIndex = col} when col % board.SquareSize = 0 -> VerticalSeperator
            | {RowIndex = _ ; ColIndex = col} when col = lastIndex -> LastColumn
            | _ -> Regular
        
        let (|FirstRow|HorizontalSeperator|LastRow|Regular|) position =
            match position with
            | {RowIndex = 0 ; ColIndex = _} -> FirstRow
            | {RowIndex = row ; ColIndex = _} when row % board.SquareSize = 0 -> HorizontalSeperator
            | {RowIndex = row ; ColIndex = _} when row = lastIndex -> LastRow
            | _ -> Regular

        getAllCells board |>
        Seq.fold (fun result cell -> 
            match getPosition cell with
            | FirstColumn & (FirstRow | HorizontalSeperator) -> result + seperatorLine + "\n|" + printCellWithFormat cell 
            | LastRow & LastColumn -> result + printCellWithFormat cell + "|\n" + seperatorLine
            | FirstColumn | VerticalSeperator -> result + "|" + printCellWithFormat cell
            | LastColumn -> result + printCellWithFormat cell + "|\n"
            | _ -> result + printCellWithFormat cell)
            "\n"