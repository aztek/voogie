var a: [int] int;
var n: int;

procedure main() returns (p: int)
  requires (n > 3);
  requires (forall i: int :: 0 <= i && i < n ==> a[i] >= 0);
  ensures (p != -1 ==>  (exists i, j: int :: 0 <= i && i < j && j < n && a[i] == a[j] && a[j] == p));
  ensures (p == -1 ==> !(exists i, j, d: int :: 0 <= i && i < j && j < n && a[i] == a[j] && a[j] == d));
{
  var occs: [int] bool;
  var i: int;
  var r: bool;
  r := false;

  assume (forall k: int :: !occs[k]);

  p := -1;

  i := 0;

  if (!occs[a[i]]) {
    occs[a[i]] := true;
  } else {
    p := a[i];
    r := true;
  }

  if (!r) {
    i := i + 1;

    if (!occs[a[i]]) {
      occs[a[i]] := true;
    } else {
      p := a[i];
      r := true;
    }
  }

  if (!r) {
    i := i + 1;

    if (!occs[a[i]]) {
      occs[a[i]] := true;
    } else {
      p := a[i];
      r := true;
    }
  }

  if (!r) {
    i := i + 1;

    if (!occs[a[i]]) {
      occs[a[i]] := true;
    } else {
      p := a[i];
      r := true;
    }
  }

  if (!r) {
    i := i + 1;
  }

  assume (!r ==> !(exists d, k, j: int :: i <= k && k < j && j < n && a[k] == a[j] && a[j] == d));
  assume (!r ==> (forall j: int :: 0 <= j && j < i ==> !(exists k: int :: i <= k && k < n ==> a[j] == a[k])));
}
