// Searching for the greatest element in an array

var a: [int] int;
var n: int;

procedure main() returns (max: int)
  requires (n > 0);
  ensures  (forall j: int :: j >= 0 && j < n ==> max >= a[j]);
{
  var i: int;

  max := a[0];
  i := 1;

  assume (i < n);
  if (max < a[i])
  {
    max := a[i];
  }
  i := i + 1;

  assume (i < n);
  if (max < a[i])
  {
    max := a[i];
  }
  i := i + 1;

  assume (i < n);
  if (max < a[i])
  {
    max := a[i];
  }
  i := i + 1;

  assume (forall j: int :: j >= i && j < n ==> max >= a[j]);
}
