# voogie

A translator of a subset of the [Boogie](https://github.com/boogie-org/boogie) intermediate verification language to [FOOL](http://link.springer.com/chapter/10.1007/978-3-319-20615-8_5) [TPTP](http://www.cs.miami.edu/~tptp/). At the time this translator is being implemented, only the [**V**ampire](http://vprover.org/) theorem prover fully supports FOOL.

The input is a program in a restricted subset of C. It includes
- definitions of variables
- `int`, `bool`, and (multi-dimentional) arrays (`int[]`, `int[][]`, `bool[][][]`, etc.)
- expressions with
  - arithmetical operators `+`, `-`, `*`
  - comparison operators `==`, `!=`, `>`, `<`, `>=`, `<=`
  - logical operators `!`, `&&`, `||` 
  - ternary operator `:?`
- assignments for variables and elements of arrays
- shortcuts `++`, `--`, `+=`, `-=`, `*=`
- `if` statements
- definitions of terminating non-recursive functions

The sequence of program statements must be followed by one or more assertions. See [examples](https://github.com/aztek/kyckling/tree/master/examples).

Run `make` to compile.

```bash
$ cat examples/count_two2.ky
```
```c
int[] a;
int x = 0, y = 0;
if (a[0] > 0) x++; else y++;
if (a[1] > 0) x++; else y++;
assert x + y == 2;
```

```bash
$ ./bin/kyckling examples/count_two2.ky
```
```
thf(a, type, a: $array($int, $int)).
thf(asserts, conjecture,
    $let(x := 0,
    $let(y := 0,
    $let([x, y] := $ite($greater($select(a, 0), 0),
                        $let(x := $sum(x, 1),
                             [x, y]),
                        $let(y := $sum(y, 1),
                             [x, y])),
    $let([x, y] := $ite($greater($select(a, 1), 0),
                        $let(x := $sum(x, 1),
                             [x, y]),
                        $let(y := $sum(y, 1),
                             [x, y])),
         $sum(x, y) = 2))))).
```

The resulting TPTP problem can be checked by a first-order theorem prover such as [Vampire](http://vprover.org/).

```bash
$ cat examples/max.ky
```
```c
int max(int x, int y) {
  if (x > y) {
    return x;
  }
  return y;
}

assert forall (int x, int y) (max(x, y) >= x && max(x, y) >= y);
```

```bash
$ ./bin/kyckling examples/max.ky
```
```
thf(asserts, conjecture,
    $let(max(X:$int, Y:$int) := $let([x, y] := [X, Y],
                                $let(i := $ite($greater(x, y),
                                               $some(x),
                                               $none($int)),
                                     $ite($issome(i),
                                          $fromsome(i),
                                          y))),
         ! [X:$int, Y:$int]: ($greatereq(max(X, Y), X) & $greatereq(max(X, Y), Y)))).
```
