// Bubble sort

var a: [int] int;
var n: int;

procedure main()
  modifies a;
  requires (n >= 0);
  ensures (forall j, k: int :: 0 <= j && j < k && k < n ==> a[j] <= a[k]);
{
  var sortedUntil, i, t: int;

  sortedUntil := 0;

  assume (sortedUntil < n);
  i := n - 1;

  assume (i > sortedUntil);
  if (a[i] <= a[i - 1])
  {
    t := a[i];
    a[i] := a[i - 1];
    a[i - 1] := t;
  }
  i := i - 1;

  assume (i > sortedUntil);
  if (a[i] <= a[i - 1])
  {
    t := a[i];
    a[i] := a[i - 1];
    a[i - 1] := t;
  }
  i := i - 1;

  assume (forall j: int :: sortedUntil <= j && j < i ==> a[i] <= a[j]);

  sortedUntil := sortedUntil + 1;

  assume (sortedUntil < n);
  i := n - 1;

  assume (i > sortedUntil);
  if (a[i] <= a[i - 1])
  {
    t := a[i];
    a[i] := a[i - 1];
    a[i - 1] := t;
  }
  i := i - 1;

  assume (i > sortedUntil);
  if (a[i] <= a[i - 1])
  {
    t := a[i];
    a[i] := a[i - 1];
    a[i - 1] := t;
  }
  i := i - 1;

  assume (forall j: int :: sortedUntil <= j && j < i ==> a[i] <= a[j]);

  sortedUntil := sortedUntil + 1;

  assume (forall j, k: int :: 0 <= j && j < sortedUntil && sortedUntil <= k && k < n ==> a[j] <= a[k]);
  assume (forall j, k: int :: sortedUntil <= j && j < k && k < n ==> a[j] <= a[k]);
}