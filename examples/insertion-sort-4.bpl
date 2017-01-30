// Insertion sort

// Source: "Introduction to Algorithms" by Cormen, Leiserson, Rivest and Stein; Third edition, page 208

var a: [int] int;

procedure main()
  modifies a;
  ensures (forall i: int, j: int :: 0 <= i && i < j && j < 4 ==> a[i] <= a[j]);
{
  var t: int;

  if (a[0] > a[1]) {
    t := a[0];
    a[0] := a[1];
    a[1] := t;
  }

  if (a[1] > a[2]) {
    t := a[1];
    a[1] := a[2];
    a[2] := t;
  }

  if (a[0] > a[1]) {
    t := a[0];
    a[0] := a[1];
    a[1] := t;
  }

  if (a[2] > a[3]) {
    t := a[2];
    a[2] := a[3];
    a[3] := t;
  }

  if (a[1] > a[2]) {
    t := a[1];
    a[1] := a[2];
    a[2] := t;
  }

  if (a[0] > a[1]) {
    t := a[0];
    a[0] := a[1];
    a[1] := t;
  }
}
