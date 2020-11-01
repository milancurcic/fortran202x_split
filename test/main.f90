program test_split
  !! Examples from https://j3-fortran.org/doc/year/20/20-007.pdf,
  !! Section 16.9.194. 
  use fortran202x_split, only: split
  implicit none

  integer, allocatable :: first(:), last(:)
  character(:), allocatable :: string, set, tokens(:)
  character, allocatable :: separator(:)
  integer :: istart, iend, n, p

  print *, 'Example 1:'
  string = 'first,second,third'
  set = ',;'
  call split(string, set, tokens)
  do n = 1, size(tokens)
    print *, 'n, tokens(n) =', n, tokens(n)
  end do

  call split(string, set, tokens, separator)
  do n = 1, size(separator)
    print *, 'n, separator(n) =', n, separator(n)
  end do

  print *
  print *, 'Example 2:'
  string = 'first,second,,forth'
  set = ',;'
  call split(string, set, first, last)
  print *, 'first =', first
  print *, 'last =', last

  print *
  print *, 'Example 3:'
  string = 'one,last example'
  set = ', '
  p = 0
  do
    if (p > len(string)) exit
    istart = p + 1
    call split(string, set, p)
    iend = p - 1
    print '(t7, a)', string(istart:iend)
  end do

end program test_split
