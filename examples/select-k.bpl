var a: [int] int;
var n: int;
var k: int;

var t: int;
var i, j: int;
var minValue: int;
var bad: bool;

procedure main()
  modifies a, t, i, j, minValue, bad;
  requires n > 0;
  requires 0 <= k && k < n;

  requires 0 <= i && i <= k + 1;
  requires i > 0 ==> (forall v: int :: 0     <= v && v < i - 1 ==> a[v]     <= a[i - 1]);
  requires i > 0 ==> (forall w: int :: i - 1 <  w && w < n     ==> a[i - 1] <= a[w]);

  requires i + 1 <= j && j <= n;
  requires a[i] == minValue;
  requires (forall w: int :: i < w && w < j ==> a[i] <= a[w]);
  requires (forall v, w: int :: 0 <= v && v < i && i <= w && w < n ==> a[v] <= a[w]);

  ensures bad || (i + 1 <= j && j <= n);
  ensures bad || (a[i] == minValue);
  ensures bad || (forall w: int :: i < w && w < j ==> a[i] <= a[w]);
  ensures bad || (forall v, w: int :: 0 <= v && v < i && i <= w && w < n ==> a[v] <= a[w]);
{
  if (!(j < n)) {
    bad := true;
  }
  if (a[j] < minValue) {
    minValue := a[j];
    t := a[i];
    a[i] := a[j];
    a[j] := t;
  }
  j := j + 1;

  if (j < n) {
    bad := true;
  }
}