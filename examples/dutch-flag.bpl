// The National Dutch flag problem

var a: [int] int;
var n: int;
var pivot: int;

var low, high: int;
var i, t: int;
var bad: bool;

procedure main()
  modifies i, t, a, low, high, bad;
  requires n >= 0;
  requires 0 <= low && low <= i && i <= high + 1 && high < n;
  requires (forall k: int :: 0    <= k && k < low  ==> a[k] <  pivot);
  requires (forall k: int :: low  <= k && k < i    ==> a[k] == pivot);
  requires (forall k: int :: high <  k && k < n    ==> a[k] >  pivot);
  ensures bad || (0 <= low && low <= i && i <= high + 1 && high < n);
  ensures bad || (forall k: int :: 0    <= k && k < low  ==> a[k] <  pivot);
  ensures bad || (forall k: int :: low  <= k && k < i    ==> a[k] == pivot);
  ensures bad || (forall k: int :: high <  k && k < n    ==> a[k] >  pivot);
{
  if (!(i <= high)) {
    bad := true;
  }
  if (a[i] < pivot) {
    t := a[low];
    a[low] := a[i];
    a[i] := t;

    low := low + 1;
    i := i + 1;
  } else if (a[i] > pivot) {
    t := a[i];
    a[i] := a[high];
    a[high] := t;

    high := high - 1;
  } else {
    i := i + 1;
  }

  if (i <= high) {
    bad := true;
  }
}