// Bubble sort

// Source: "Introduction to Algorithms" by Cormen, Leiserson, Rivest and Stein; Third edition, page 40

var a: [int] int;

procedure main()
  modifies a;
  ensures (forall i: int, j: int :: 0 <= i && i < j && j < 5 ==> a[i] <= a[j]);
{
  var t: int;

  if (a[4] < a[3]) {
    t := a[4];
    a[4] := a[3];
    a[3] := t;
  }

  if (a[3] < a[2]) {
    t := a[3];
    a[3] := a[2];
    a[2] := t;
  }

  if (a[2] < a[1]) {
    t := a[2];
    a[2] := a[1];
    a[1] := t;
  }

  if (a[1] < a[0]) {
    t := a[1];
    a[1] := a[0];
    a[0] := t;
  }

  if (a[4] < a[3]) {
    t := a[4];
    a[4] := a[3];
    a[3] := t;
  }

  if (a[3] < a[2]) {
    t := a[3];
    a[3] := a[2];
    a[2] := t;
  }

  if (a[2] < a[1]) {
    t := a[2];
    a[2] := a[1];
    a[1] := t;
  }

  if (a[4] < a[3]) {
    t := a[4];
    a[4] := a[3];
    a[3] := t;
  }

  if (a[3] < a[2]) {
    t := a[3];
    a[3] := a[2];
    a[2] := t;
  }

  if (a[4] < a[3]) {
    t := a[4];
    a[4] := a[3];
    a[3] := t;
  }
}