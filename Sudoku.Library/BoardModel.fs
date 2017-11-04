namespace Sudoku.Library

module BoardModel =
    type CellValues = Set<int>

    type Position = 
        {
            RowIndex : int
            ColIndex : int
        }

    type Cell =
        | FullCell of position : Position * value : int
        | EmptyCell of position : Position * values : CellValues

    type Board =
        {
            Size : int
            SquareSize : int
            Cells : Cell[,]
        }