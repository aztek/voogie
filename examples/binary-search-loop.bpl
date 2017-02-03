var a: [int] int;
var n: int;
var e: int;

procedure main() {
  var high, low, mid: int;

  low  := 0;
  high := n - 1;

  while (low <= high) {
    mid := (low + high) / 2;
    if (a[mid] == e) {
      return mid;
    } else {
      if (a[mid] < e) {
        low  := mid + 1;
      } else {
        high := mid - 1;
      }
    }
  }

  return -1;
}
