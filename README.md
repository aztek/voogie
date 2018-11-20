# Voogie

[![Build Status](https://travis-ci.org/aztek/voogie.svg?branch=master)](https://travis-ci.org/aztek/voogie)

[Vampire](https://vprover.github.io/) meets [Boogie](https://www.microsoft.com/en-us/research/project/boogie-an-intermediate-verification-language/)!

Voogie reads simple Boogie programs and generates their verification conditions as formulas in the [FOOL logic](https://link.springer.com/chapter/10.1007/978-3-319-20615-8_5). These formulas are written in the [TPTP language](http://www.cs.miami.edu/~tptp/) and can be checked by automated first-order theorem provers. Currently, only the Vampire theorem prover fully supports FOOL.

> E. Kotelnikov, L. Kovacs, and A. Voronkov. [*A FOOLish Encoding of the Next State Relations of Imperative Programs.*](https://easychair.org/publications/preprint/rpvg) In Proceedings of the 9th International Joint Conference on Automated Reasoning, pages 405–421, 2018.

Voogie is not a full-blown program verifier. Its goal is to explore which extensions of first-order logic are adequate for naturally representing fragments of imperative programs.

The current version of Voogie supports the following features of Boogie.
- Types `int`, `bool`, nested and multi-dimentional arrays (e.g. `[int] int`, `[int,int] int`, `[int][int] int`).
- Top level variable definitions.
- Expressions, built with
  * arithmetic operators `+`, `-`, `*`, `div`,
  * comparison operators `==`, `!=`, `>`, `<`, `>=`, `<=`, and
  * logical connectives `!`, `&&`, `||`.
- Assignments to variables (e.g. `x := 0`), parallel assignments to variables (e.g. `x, y := y, x`), and assignments to array elements (e.g. `a[0] := 1`, `b[0][0] := 1`, `c[0,1] := 1`).
- `if` statements
- A single procedure `main`, possibly annotated with pre-conditions using the keyword `requires` and post-conditions using the keyword `ensures`.

Voogie is written in Haskell and must be built with [GHC and Cabal](https://www.haskell.org/platform/). Run `cabal install` to compile and install Voogie.

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

See the [examples](https://github.com/aztek/voogie/tree/master/examples) dir for more examples.

```
$ voogie examples/simple.bpl
```
```
tff(a, type, a : $array($int, $int)).
tff(x, type, x : $int).
tff(y, type, y : $int).
tff(voogie_conjecture, conjecture,
    $let(x : $int,
         x := 0,
    $let(y : $int,
         y := 0,
    $let([y : $int, x : $int],
         [y, x] := $ite($greater($select(a, 0), 0),
                        $let(x : $int,
                             x := $sum(x, 1),
                             [y, x]),
                        $let(y : $int,
                             y := $sum(y, 1),
                             [y, x])),
    $let([y : $int, x : $int],
         [y, x] := $ite($greater($select(a, 1), 0),
                        $let(x : $int,
                             x := $sum(x, 1),
                             [y, x]),
                        $let(y : $int,
                             y := $sum(y, 1),
                             [y, x])),
         ($sum(x, y) = 2)))))).
```

This is a TPTP encoding of the partial correctness property of `examples/simple.bpl`. You can use Vampire to verify this property.

```
$ voogie examples/simple.bpl | vampire -newcnf on -p off
```
```
% Refutation found. Thanks to Tanya!
% SZS status Theorem for
% ------------------------------
% Version: Vampire 4.0 (commit 47e86a8 on 2017-10-30 07:14:35 -0500)
% Termination reason: Refutation

% Memory used [KB]: 5628
% Time elapsed: 0.036 s
% ------------------------------
% ------------------------------
```
Voogie was developed by [Evgenii Kotelnikov](http://www.cse.chalmers.se/~evgenyk/).
