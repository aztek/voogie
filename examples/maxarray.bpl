var a: [int] int;
var n: int;

var y: int;
/*ghost*/ var m: int;
var x: int;
var bad: bool;

procedure main()
  modifies x, y, m, bad;

  requires n > 0;

  requires 0 <= x && x <= y && y < n;
  requires m == x || m == y;
  requires (forall i: int :: 0 <= i && i < x ==> a[i] <= a[m]);
  requires (forall i: int :: y <  i && i < n ==> a[i] <= a[m]);

  ensures bad || (0 <= x && x <= y && y < n);
  ensures bad || (m == x || m == y);
  ensures bad || (forall i: int :: 0 <= i && i < x ==> a[i] <= a[m]);
  ensures bad || (forall i: int :: y <  i && i < n ==> a[i] <= a[m]);
{
  if (!(x != y)) {
    bad := true;
  }
  if (a[x] <= a[y]) {
    x := x + 1; m := y;
  } else {
    y := y - 1; m := x;
  }

  if (x != y) {
    bad := true;
  }
}