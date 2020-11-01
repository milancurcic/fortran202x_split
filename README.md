# fortran202x_split

A Fortran implementation of the Fortran 202X `split` intrinsic subroutine.
Candidate for inclusion in fortran-lang/stdlib.

## Getting started

```
git clone https://github.com/milancurcic/fortran202x_split
cd fortran202x_split
```

### With fpm

You can get fpm [here](https://github.com/fortran-lang/fpm).

```
fpm build
fpm test
```

### Without fpm

```
gfortran src/fortran202x_split.f90 test/main.f90 -o test_split
./test_split
``` 
