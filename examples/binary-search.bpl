// Binary search

var a: [int] int;
var n: int;
var e: int;

var high, low, mid: int;
var i: int;
var bad: bool;

procedure main()
  modifies high, low, mid, i, bad;
  requires n > 0;
  requires (forall j, k: int :: 0 <= j && j <= k && k < n ==> a[j] <= a[k]);

  requires 0 <= low && low <= high + 1 && high < n;
  requires i >= 0 ==> a[i] == e;
  requires (forall k: int :: k >= 0 && k < low ==> a[k] < e); 
  requires (forall k: int :: k > high && k < n ==> a[k] > e); 

  ensures bad || (0 <= low && low <= high + 1 && high < n);
  ensures bad || (i >= 0 ==> a[i] == e);
  ensures bad || (forall k: int :: k >= 0 && k < low ==> a[k] < e); 
  ensures bad || (forall k: int :: k > high && k < n ==> a[k] > e); 
{
  if (!(low <= high && i < 0)) {
    bad := true;
  }
  mid := (low + high) div 2;
  if (a[mid] == e) {
    i := mid;
  } else {
    if (a[mid] < e) {
      low  := mid + 1;
    } else {
      high := mid - 1;
    }
  }

  if (low <= high && i < 0) {
    bad := true;
  }
}
