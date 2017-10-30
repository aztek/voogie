var a: [int] int;
var x, y: int;

procedure main()
  modifies x, y;
  ensures x + y == 2;
 {
  x := 0;
  y := 0;
  if (a[0] > 0) x := x + 1; else y := y + 1;
  if (a[1] > 0) x := x + 1; else y := y + 1;
}