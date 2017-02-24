// Finds the k smallest elements of an array and moves them up front

var a: [int] int;
var n: int;

procedure main()
  modifies a;
  requires (n >= 2);
  ensures (forall w: int :: w >= 0 && w < 1 ==> a[w] <= a[1]);
  ensures (forall w: int :: w >= 1 && w < n ==> a[w] >= a[1]);
{
  var t: int;
  var j: int;
  var minValue: int;

  minValue := a[0];

  j := 1;

  assume (j < n);
  if (a[j] < minValue) {
    minValue := a[j];
    t := a[0];
    a[0] := a[j];
    a[j] := t;
  }
  j := j + 1;

  assume (j < n);
  if (a[j] < minValue) {
    minValue := a[j];
    t := a[0];
    a[0] := a[j];
    a[j] := t;
  }
  j := j + 1;

  assume (forall w: int :: w >= j && w < n ==> minValue <= a[w]);

  minValue := a[1];

  j := 2;

  assume (j < n);
  if (a[j] < minValue) {
    minValue := a[j];
    t := a[1];
    a[1] := a[j];
    a[j] := t;
  }
  j := j + 1;

  assume (j < n);
  if (a[j] < minValue) {
    minValue := a[j];
    t := a[1];
    a[1] := a[j];
    a[j] := t;
  }
  j := j + 1;

  assume (forall w: int :: w >= j && w < n ==> minValue <= a[w]);
}
