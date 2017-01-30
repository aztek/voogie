var a: [int] int;

procedure main()
  modifies a;
  ensures (a[0] <= a[2] && a[1] <= a[2]);
  ensures (a[2] <= a[3] && a[2] <= a[4]);
{
  var t: int;
  var minValue: int;

  minValue := a[0];

  if (a[1] < minValue) {
    minValue := a[1];
    t := a[0];
    a[0] := a[1];
    a[1] := t;
  }

  if (a[2] < minValue) {
    minValue := a[2];
    t := a[0];
    a[0] := a[2];
    a[2] := t;
  }

  if (a[3] < minValue) {
    minValue := a[3];
    t := a[0];
    a[0] := a[3];
    a[3] := t;
  }

  if (a[4] < minValue) {
    minValue := a[4];
    t := a[0];
    a[0] := a[4];
    a[4] := t;
  }

  minValue := a[1];

  if (a[2] < minValue) {
    minValue := a[2];
    t := a[1];
    a[1] := a[2];
    a[2] := t;
  }

  if (a[3] < minValue) {
    minValue := a[3];
    t := a[1];
    a[1] := a[3];
    a[3] := t;
  }

  if (a[4] < minValue) {
    minValue := a[4];
    t := a[1];
    a[1] := a[4];
    a[4] := t;
  }

  minValue := a[2];

  if (a[3] < minValue) {
    minValue := a[3];
    t := a[2];
    a[2] := a[3];
    a[3] := t;
  }

  if (a[4] < minValue) {
    minValue := a[4];
    t := a[2];
    a[2] := a[4];
    a[4] := t;
  }
}
