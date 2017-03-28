var a: [int] bool;
var n: int;

var i, j: int;
var t: bool;
var bad: bool;

procedure main()
  modifies a, i, j, t, bad;
  requires n > 0;

  requires 0 <= i && i <= j + 1 && j + 1 <= n;
  requires (forall m: int :: 0 <= m && m < i ==> !a[m]);
  requires (forall k: int :: j <  k && k < n ==> a[k]);

  ensures bad || (0 <= i && i <= j + 1 && j + 1 <= n);
  ensures bad || (forall m: int :: 0 <= m && m < i ==> !a[m]);
  ensures bad || (forall k: int :: j <  k && k < n ==> a[k]);
{
  if (!(i <= j)) {
    bad := true;
  }
  if (!a[i]) {
    i := i + 1;
  } else if (a[j]) {
    j := j - 1;
  } else {
    t := a[i];
    a[i] := a[j];
    a[j] := t;
    i := i + 1;
    j := j - 1;
  }

  if (i <= j) {
    bad := true;
  }
}