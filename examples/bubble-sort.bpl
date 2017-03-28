var a: [int] int;
var n: int;
var sortedUntil, i, t: int;
var bad: bool;

procedure main()
  modifies a, sortedUntil, i, t, bad;
  requires n >= 0;

  requires 0 <= sortedUntil && sortedUntil <= n;
  requires (forall j, k: int :: 0 <= j && j < sortedUntil && sortedUntil <= k && k < n ==> a[j] <= a[k]);
  requires (forall j, k: int :: 0 <= j && j < k && k < sortedUntil ==> a[j] <= a[k]);

  requires sortedUntil <= i && i < n;
  requires (forall j, k: int :: 0 <= j && j < sortedUntil && sortedUntil <= k && k < n ==> a[j] <= a[k]);
  requires (forall j: int :: i <= j && j < n ==> a[i] <= a[j]);
  requires (forall j, k: int :: 0 <= j && j < k && k < sortedUntil ==> a[j] <= a[k]);

  ensures bad || (sortedUntil <= i && i < n);
  ensures bad || (forall j, k: int :: 0 <= j && j < sortedUntil && sortedUntil <= k && k < n ==> a[j] <= a[k]);
  ensures bad || (forall j: int :: i <= j && j < n ==> a[i] <= a[j]);
  ensures bad || (forall j, k: int :: 0 <= j && j < k && k < sortedUntil ==> a[j] <= a[k]);
{
  if (!(i > sortedUntil)) {
    bad := true;
  }
  if (a[i] <= a[i - 1])
  {
    t := a[i];
    a[i] := a[i - 1];
    a[i - 1] := t;
  }
  i := i - 1;

  if (i > sortedUntil) {
    bad := true;
  }
}