program bench
  ! Compares the run-time of the split subroutine and a string_tokens function
  ! when applied on a long string.
  use fortran202x_split, only: split, string_tokens
  implicit none
  character(6000000) :: string
  character(80) :: line
  character(:), allocatable :: tokens(:)
  integer :: n, p1, p2, u
  real :: t1, t2
  real, allocatable :: elapsed(:)

  string = ''
  p1 = 1

  open(newunit=u, file='war_and_peace.txt', status='old', action='read')
  do
    read(u, fmt='(a)', end=1) line
    p2 = p1 + len_trim(line) - 1
    string(p1:p2) = trim(line)
    p1 = p2 + 2
  end do
  1 close(u)

  print *, 'Running split subroutine...'
  elapsed = [real ::]
  do n = 1, 10
    call cpu_time(t1)
    call split(string, ' ', tokens)
    call cpu_time(t2)
    print '(a,i2,a,f9.7,a)', '  Run ', n, ': ', t2 - t1, ' seconds elapsed'
    elapsed = [elapsed, t2 - t1]
    deallocate(tokens)
  end do
  print '(80("-"))'
  print '(2(a,f9.7))', 'split subroutine, seconds elapsed: ', mean(elapsed), ' +/- ', std(elapsed)

  print *
  print *, 'Running string_tokens function...'
  elapsed = [real ::]
  do n = 1, 10
    call cpu_time(t1)
    tokens = string_tokens(string, ' ')
    call cpu_time(t2)
    print '(a,i2,a,f9.7,a)', '  Run ', n, ': ', t2 - t1, ' seconds elapsed'
    elapsed = [elapsed, t2 - t1]
    deallocate(tokens)
  end do
  print '(80("-"))'
  print '(2(a,f9.7))', 'string_tokens function, seconds elapsed: ', mean(elapsed), ' +/- ', std(elapsed)

contains

  pure real function mean(x)
    real, intent(in) :: x(:)
    mean = sum(x) / size(x)
  end function mean

  pure real function std(x)
    real, intent(in) :: x(:)
    std = sqrt(mean((x - mean(x))**2))
  end function std

end program bench
