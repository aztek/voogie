var a: [int] int;
var n: int;

procedure main() returns (x: int)
  requires n > 0;
  ensures 0 <= x && x < n;
  ensures (forall i: int :: 0 <= i && i < n ==> a[i] <= a[x]);
{
  var y: int;
  /*ghost*/ var m: int;

  x := 0;
  y := n - 1;
  m := y;

  assume (x != y);
  if (a[x] <= a[y]) {
    x := x + 1; m := y;
  } else {
    y := y - 1; m := x;
  }

  assume (x != y);
  if (a[x] <= a[y]) {
    x := x + 1; m := y;
  } else {
    y := y - 1; m := x;
  }

  assume (x != y);
  if (a[x] <= a[y]) {
    x := x + 1; m := y;
  } else {
    y := y - 1; m := x;
  }

  assume (forall i: int :: x < i && i <= y ==> a[i] <= a[x]);
}
