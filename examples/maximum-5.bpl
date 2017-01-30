// Searching for the minimal element in an array

// Source: "Introduction to Algorithms" by Cormen, Leiserson, Rivest and Stein; Third edition, page 208

procedure main(a: [int] int) returns (min: int)
  ensures (forall i: int :: i >= 0 && i < 5 ==> min <= a[i]);
{
  min := a[0];

  if (min > a[1]) {
    min := a[1];
  }

  if (min > a[2]) {
    min := a[2];
  }

  if (min > a[3]) {
    min := a[3];
  }

  if (min > a[4]) {
    min := a[4];
  }
}
