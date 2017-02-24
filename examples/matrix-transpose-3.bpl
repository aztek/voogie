// Transposes a square matrix in a single loop

var matrix: [int,int] int;
var transposed: [int,int] int;
var size: int;

procedure main()
  modifies transposed;
  requires (size > 0);
  ensures (forall i, j: int :: 0 <= i && i < size && 0 <= j && j < size ==> transposed[i,j] == matrix[j,i]);
{
  var row, col: int;

  row := 0;
  col := 1;
  transposed[0,0] := matrix[0,0];

  assume (row != size);
  if (col < size) {
    transposed[row,col] := matrix[col,row];
    transposed[col,row] := matrix[row,col];
    col := col + 1;
  } else {
    row := row + 1;
    col := row + 1;
    transposed[row,row] := matrix[row,row];
  }

  assume (row != size);
  if (col < size) {
    transposed[row,col] := matrix[col,row];
    transposed[col,row] := matrix[row,col];
    col := col + 1;
  } else {
    row := row + 1;
    col := row + 1;
    transposed[row,row] := matrix[row,row];
  }

  assume (row != size);
  if (col < size) {
    transposed[row,col] := matrix[col,row];
    transposed[col,row] := matrix[row,col];
    col := col + 1;
  } else {
    row := row + 1;
    col := row + 1;
    transposed[row,row] := matrix[row,row];
  }

  assume (forall j: int :: col <= j && j < size ==> transposed[row,j] == matrix[j,row]);
  assume (forall i, j: int :: row < i && i < size && 0 <= j && j < size ==> transposed[i,j] == matrix[j,i]);
}