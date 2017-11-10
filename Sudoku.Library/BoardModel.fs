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
        override c.ToString() = 
            match c with
            | FullCell (position, value) -> sprintf "{%i : %i} - %i" position.RowIndex position.ColIndex value
            | EmptyCell (position, _) -> sprintf "{%i : %i}" position.RowIndex position.ColIndex 
        
    type Board =
        {
            Size : int
            SquareSize : int
            Cells : Cell[,]
        }