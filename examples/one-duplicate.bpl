var a: [int] int;
var n: int;
var occs: [int] bool;
var i: int;
var d: int;
var bad: bool;

procedure main()
  modifies occs, i, d, bad;
  requires n > 0;
  requires (forall i: int :: 0 <= i && i < n ==> a[i] >= 0);
  requires 0 <= i && i <= n;
  requires d >= 0 ==> occs[a[i]];
  requires (forall k: int :: 0 <= k && k < i ==> occs[a[k]]);
  requires (forall j, k: int :: 0 <= j && j < k && k < i ==> a[j] != a[k]);
  ensures bad || (0 <= i && i <= n);
  ensures bad || (d >= 0 ==> occs[a[i]]);
  ensures bad || (forall k: int :: 0 <= k && k < i ==> occs[a[k]]);
  ensures bad || (forall j, k: int :: 0 <= j && j < k && k < i ==> a[j] != a[k]);
{
  if (!(i < n && d < 0)) {
    bad := true;
  }
  if (!occs[a[i]]) {
    occs[a[i]] := true;
    i := i + 1;
  } else {
    d := a[i];
  }

  if (i < n && d < 0) {
    bad := true;
  }
}
