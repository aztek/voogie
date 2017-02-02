var a: [int] int;
var e: int;

procedure main()
  returns (i: int)
  requires (forall j: int :: j >= 0 && j < 5 ==> a[j + 1] >= a[j]);
  ensures (i == -1 ==> (forall j: int :: j >= 0 && j < 6 ==> a[j] != i));
  ensures (i != -1 ==> a[i] == e);
{
  var r: bool;
  var high, low, mid: int;

  r := false;

  low  := 0;
  high := 6;

  mid := (low + high) / 2;
  if (a[mid] == e) {
    if (!r) {
      r := true;
      i := mid;
    }
  } else {
    if (a[mid] < e) {
      low  := mid + 1;
    } else {
      high := mid - 1;
    }
  }

  mid := (low + high) / 2;
  if (a[mid] == e) {
    if (!r) {
      r := true;
      i := mid;
    }
  } else {
    if (a[mid] < e) {
      low  := mid + 1;
    } else {
      high := mid - 1;
    }
  }

  mid := (low + high) / 2;
  if (a[mid] == e) {
    if (!r) {
      r := true;
      i := mid;
    }
  } else {
    if (a[mid] < e) {
      low  := mid + 1;
    } else {
      high := mid - 1;
    }
  }

  if (!r) {
    r := true;
    i := -1;
  }
}
