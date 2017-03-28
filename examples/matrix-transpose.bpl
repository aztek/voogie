var matrix: [int,int] int;
var transposed: [int,int] int;
var size: int;
var row, col: int;
var bad: bool;

procedure main()
  modifies row, col, transposed, bad;
  requires size > 0;
  requires 0 <= row && row <= col;
  requires (forall i, j: int :: 0 <= i && i <= j && j < size &&
    ((i == row && j < col) || i < row) ==> transposed[i,j] == matrix[j,i] && transposed[j,i] == matrix[i,j]);

  ensures bad || (0 <= row && row <= col);
  ensures bad || (forall i, j: int :: 0 <= i && i <= j && j < size &&
    ((i == row && j < col) || i < row) ==> transposed[i,j] == matrix[j,i] && transposed[j,i] == matrix[i,j]);
{
  if (!(row != size)) {
    bad := true;
  }
  if (col < size) {
    transposed[row,col] := matrix[col,row];
    transposed[col,row] := matrix[row,col];
    col := col + 1;
  } else {
    row := row + 1;
    col := row + 1;
    transposed[row,row] := matrix[row,row];
  }

  if (row != size) {
    bad := true;
  }
}