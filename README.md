# Voogie

[Vampire](http://vprover.org) meets [Boogie](https://www.microsoft.com/en-us/research/project/boogie-an-intermediate-verification-language/)!

Voogie reads simple Boogie programs and generates their verification conditions as formulas in the [FOOL logic](https://link.springer.com/chapter/10.1007/978-3-319-20615-8_5). These formulas are written in the [TPTP language](http://www.cs.miami.edu/~tptp/) and can be checked by automated first-order theorem provers. As of now, only Vampire supports all the features of FOOL.
