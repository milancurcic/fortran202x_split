module fortran202x_split
  !! An implementation of the Fortran 202X split intrinsic subroutine.
  implicit none

  private
  public :: split, string_tokens

  interface split
    module procedure :: split_tokens, split_first_last, split_pos
  end interface split

contains

  pure subroutine split_tokens(string, set, tokens, separator)
    !! Splits a string into tokens using characters in set as token delimiters.
    !! If present, separator contains the array of token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable, intent(out) :: tokens(:)
    character, allocatable, intent(out), optional :: separator(:)

    integer, allocatable :: first(:), last(:)
    integer :: n

    call split(string, set, first, last)
    allocate(character(len=maxval(last - first) + 1) :: tokens(size(first)))

    do concurrent (n = 1:size(tokens))
      tokens(n) = string(first(n):last(n))
    end do

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do concurrent (n = 1:size(tokens) - 1)
        separator(n) = string(first(n+1)-1:first(n+1)-1)
      end do
    end if

  end subroutine split_tokens


  pure subroutine split_first_last(string, set, first, last)
    !! Computes the first and last indices of tokens in input string, delimited
    !! by the characters in set, and stores them into first and last output
    !! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    character :: set_array(len(set))
    logical, dimension(len(string)) :: is_first, is_last, is_separator
    integer :: n, slen

    slen = len(string)

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    end do

    do concurrent (n = 1:slen)
      is_separator(n) = any(string(n:n) == set_array)
    end do

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        end if
      end if
    end do

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last


  pure subroutine split_pos(string, set, pos, back)
    !! If back is absent, computes the leftmost token delimiter in string whose
    !! position is > pos. If back is present and true, computes the rightmost
    !! token delimiter in string whose position is < pos. The result is stored
    !! in pos.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, intent(in out) :: pos
    logical, intent(in), optional :: back

    logical :: backward
    character :: set_array(len(set))
    integer :: n, result_pos

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    end do

    if (backward) then
      result_pos = 0
      do n = pos - 1, 1, -1
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        end if
      end do
    else
      result_pos = len(string) + 1
      do n = pos + 1, len(string)
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        end if
      end do
    end if

    pos = result_pos

  end subroutine split_pos


  pure function string_tokens(string, set) result(tokens)
    !! Splits a string into tokens using characters in set as token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable :: tokens(:)
    call split_tokens(string, set, tokens)
  end function string_tokens

end module fortran202x_split
