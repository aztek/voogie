// Binary search

var a: [int] int;
var n: int;
var e: int;

procedure main() returns (i: int)
  requires (n > 0);
  requires (forall j, k: int :: j >= 0 && j <= k && k < n ==> a[k] >= a[j]);
  ensures  (i == -1 ==> (forall j: int :: j >= 0 && j < n ==> a[j] != e));
  ensures  (i != -1 ==> a[i] == e);
{
  var high, low, mid: int;
  var r: bool;
  r := false;

  i := -1;

  low  := 0;
  high := n - 1;
  
  assume (low <= high);
  if (!r) {
    mid := (low + high) div 2;
    if (a[mid] == e) {
      i := mid;
      r := true;
    } else {
      if (a[mid] < e) {
        low  := mid + 1;
      } else {
        high := mid - 1;
      }
    }
  }

  assume (low <= high);
  if (!r) {
    mid := (low + high) div 2;
    if (a[mid] == e) {
      i := mid;
      r := true;
    } else {
      if (a[mid] < e) {
        low  := mid + 1;
      } else {
        high := mid - 1;
      }
    }
  }

  assume (low <= high);
  if (!r) {
    mid := (low + high) div 2;
    if (a[mid] == e) {
      i := mid;
      r := true;
    } else {
      if (a[mid] < e) {
        low  := mid + 1;
      } else {
        high := mid - 1;
      }
    }
  }

  assume (!r ==> (forall j: int :: j >= low && j <= high ==> a[j] != e));
}
