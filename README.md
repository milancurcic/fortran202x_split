# fortran202x_split

A Fortran implementation of the Fortran 202X `split` intrinsic subroutine.
Candidate for inclusion in fortran-lang/stdlib.

`split` is defined in Section 16.1.194 of
[20-007](https://j3-fortran.org/doc/year/20/20-007.pdf) with corrections in
[20-139](https://j3-fortran.org/doc/year/20/20-139.txt).

Tested with:

* gfortran 8.3.0, 9.2.0
* ifort 2021.1 beta
* xlf 16.1.1

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
