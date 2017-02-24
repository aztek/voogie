// Sorts an array of booleans

var a: [int] bool;
var n: int;

procedure main()
  modifies a;
  requires (n > 0);
  ensures (forall m, k: int :: 0 <= m && m < k && k < n ==> (!a[m] || a[k]));
{
  var i, j: int;
  var t: bool;

  i := 0;
  j := n - 1;

  assume (i <= j);
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

  assume (i <= j);
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

  assume (i <= j);
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

  assume (forall m, k: int :: i <= m && m < k && k < j ==> (!a[m] || a[k]));
}