# computor-v2

A calculator/programming language

## Building

### Nix/NixOS

You can use `nix build` to build if you're on nix.

### Other

This project is a `stack` project. Use `stack build` to build it.

## Documentation

### Syntax

*Assignment*
```
almostPi = 3.14
```

*Variable query*
```
500 + 300 = ?
```
Shorthand:
```
500 + 300
```

*Function definition*
```
foo(x) = x * 10
```

*Lambda*
As a function definition
```
foo = \x -> x * 10
```
Multiple arguments in Lambda
```
addSquares = \x y -> x ^ 2 + y ^ 2
```

*Function application*
```
addSquares(3, 5)
```

Maybe in the future: Alternative syntax
```
addSquares 3 5
```

*Matrices*
```
identity3x3 = [[1,0,0];[0,1,0];[0,0,1]]
```

*Special case `i`*

```
myComplex = 2i + 5
```
