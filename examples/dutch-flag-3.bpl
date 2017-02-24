// The National Dutch flag problem

var a: [int] int;
var n: int;
var pivot: int;

var low, high: int;

procedure main()
  modifies a, low, high;
  ensures (forall k: int :: 0    <= k && k <  low  ==> a[k] <  pivot);
  ensures (forall k: int :: low  <= k && k <= high ==> a[k] == pivot);
  ensures (forall k: int :: high <  k && k <  n    ==> a[k] >  pivot);
{
  var i, t: int;

  low := 0;
  high := n - 1;

  i := 0;

  assume (i <= high);
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

  assume (i <= high);
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

  assume (i <= high);
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

  assume (forall k: int :: k >= i && i < high ==> a[k] == pivot);
}