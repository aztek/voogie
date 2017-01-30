// Selection sort

// Source: "Algorithms. Fourth edition" by Sedgewick and Wayne, page 249

var a: [int] int;

procedure main()
  modifies a;
  ensures (forall i: int, j: int :: 0 <= i && i < j && j < 4 ==> a[i] <= a[j]);
{
  var min: int;
  var t: int;

  min := 0;

  if (a[1] < a[min]) {
    min := 1;
  }

  if (a[2] < a[min]) {
    min := 2;
  }

  if (a[3] < a[min]) {
    min := 3;
  }

  t := a[0];
  a[0] := a[min];
  a[min] := t;

  min := 1;

  if (a[2] < a[min]) {
    min := 2;
  }

  if (a[3] < a[min]) {
    min := 3;
  }

  t := a[1];
  a[1] := a[min];
  a[min] := t;

  min := 2;

  if (a[3] < a[min]) {
    min := 3;
  }

  t := a[2];
  a[2] := a[min];
  a[min] := t;

  min := 3;

  t := a[3];
  a[3] := a[min];
  a[min] := t;
}
