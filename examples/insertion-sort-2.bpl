// Insertion sort

var a: [int] int;
var n: int;

procedure main()
  modifies a;
  requires (n > 0);
  ensures (forall k, m: int :: 0 <= k && k < m && m < n ==> a[k] <= a[m]);
{
  var t: int;
  var i, j: int;
  j := 1;

  assume (j < n);
  i := j - 1;

  if (a[i] > a[i + 1]) {
    t := a[i];
    a[i] := a[i + 1];
    a[i + 1] := t;
  }
  i := i - 1;

  j := j + 1;

  assume (j < n);
  i := j - 1;

  if (a[i] > a[i + 1]) {
    t := a[i];
    a[i] := a[i + 1];
    a[i + 1] := t;
  }
  i := i - 1;

  if (a[i] > a[i + 1]) {
    t := a[i];
    a[i] := a[i + 1];
    a[i + 1] := t;
  }
  i := i - 1;

  j := j + 1;

  assume (forall k, m: int :: 0 <= k && k < j && j <= m && m < n ==> a[k] <= a[m]);
  assume (forall k, m: int :: j <= k && k < m && m < n ==> a[k] <= a[m]);
}