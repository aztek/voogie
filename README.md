# Voogie

[Vampire](http://vprover.org) meets [Boogie](https://www.microsoft.com/en-us/research/project/boogie-an-intermediate-verification-language/)!

Voogie reads simple Boogie programs and generates their verification conditions as formulas in the [FOOL logic](https://link.springer.com/chapter/10.1007/978-3-319-20615-8_5). These formulas are written in the [TPTP language](http://www.cs.miami.edu/~tptp/) and can be checked by automated first-order theorem provers. As of now, only Vampire supports all the features of FOOL.

Run `cabal install` to compile and install Voogie.

```
$ cat examples/simple.bpl
```
```
var a: [int] int;
var x, y: int;

procedure main()
  modifies x, y;
  ensures x + y == 2;
 {
  x := 0;
  y := 0;
  if (a[0] > 0) x := x + 1; else y := y + 1;
  if (a[1] > 0) x := x + 1; else y := y + 1;
}
```
```
$ voogie examples/simple.bpl
```
```
thf(a, type, (a:$array($int, $int))).
thf(x, type, (x:$int)).
thf(y, type, (y:$int)).
thf(voogie_conjecture, conjecture,
    $let(x := 0,
    $let(y := 0,
    $let([y, x] := $ite($greater($select(a, 0), 0),
                        $let(x := $sum(x, 1),
                             [y, x]),
                        $let(y := $sum(y, 1),
                             [y, x])),
    $let([y, x] := $ite($greater($select(a, 1), 0),
                        $let(x := $sum(x, 1),
                             [y, x]),
                        $let(y := $sum(y, 1),
                             [y, x])),
         ($sum(x, y) = 2)))))).
```