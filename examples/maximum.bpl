// Searching for the greatest element in an array

// Source: "Introduction to Algorithms" by Cormen, Leiserson, Rivest and Stein; Third edition, page 214

var a: [int] int;
var n: int;

var i: int;
var max: int;
var bad: bool;

procedure main()
  modifies i, max, bad;
  requires n > 0;
  requires 1 <= i && i <= n;
  requires (forall j: int :: j >= 0 && j < i ==> max >= a[j]);
  ensures bad || (1 <= i && i <= n);
  ensures bad || (forall j: int :: j >= 0 && j < i ==> max >= a[j]);
{
  if (!(i < n)) {
    bad := true;
  }
  if (max < a[i])
  {
    max := a[i];
  }
  i := i + 1;

  if (i < n) {
    bad := true;
  }
}
