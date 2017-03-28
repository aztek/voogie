// Insertion sort

// Source: "Introduction to Algorithms" by Cormen, Leiserson, Rivest and Stein; Third edition, page 208

var a: [int] int;
var n: int;

var t: int;
var sortedFrom, sortedUntil: int;
var bad: bool;

procedure main()
  modifies a, t, sortedFrom, sortedUntil, bad;
  requires n > 0;
  requires 0 < sortedUntil && sortedUntil <= n;
  requires (forall k, m: int :: 0 <= k && k < m && m < sortedUntil ==> a[k] <= a[m]);
  requires (forall k: int :: 0 <= k && k < sortedUntil - 1 ==> a[k] <= a[sortedUntil - 1]);

  requires -1 <= sortedFrom && sortedFrom <= sortedUntil - 1;
  requires (forall k, m: int :: 0 <= k && k < sortedFrom + 1 && sortedFrom + 1 < m && m <= sortedUntil ==> a[k] <= a[m]);
  requires (forall k, m: int :: 0 <= k && k < m && m < sortedFrom + 1 ==> a[k] <= a[m]);
  requires (forall k, m: int :: sortedFrom + 1 <= k && k < m && m <= sortedUntil ==> a[k] <= a[m]);

  requires bad || (-1 <= sortedFrom && sortedFrom <= sortedUntil - 1);
  requires bad || (forall k, m: int :: 0 <= k && k < sortedFrom + 1 && sortedFrom + 1 < m && m <= sortedUntil ==> a[k] <= a[m]);
  requires bad || (forall k, m: int :: 0 <= k && k < m && m < sortedFrom + 1 ==> a[k] <= a[m]);
  requires bad || (forall k, m: int :: sortedFrom + 1 <= k && k < m && m <= sortedUntil ==> a[k] <= a[m]);
{
  if (!(sortedFrom >= 0)) {
    bad := true;
  }
  if (a[sortedFrom] > a[sortedFrom + 1]) {
    t := a[sortedFrom];
    a[sortedFrom] := a[sortedFrom + 1];
    a[sortedFrom + 1] := t;
  }
  sortedFrom := sortedFrom - 1;

  if (sortedFrom >= 0) {
    bad := true;
  }
}
