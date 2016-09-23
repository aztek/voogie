# kyckling

A translator of code fragments with assertions to TPTP problems.

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

``$ ./bin/kyckling examples/count_two2.ky``
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
