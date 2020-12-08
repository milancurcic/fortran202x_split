module fortran202x_split
  !! An implementation of the Fortran 202X split intrinsic subroutine.
  implicit none

  private
  public :: split, string_tokens

  interface split
    module procedure :: split_tokens, split_first_last_v1, split_pos
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


  pure subroutine split_first_last_v1(string, set, first, last)
    !! Computes the first and last indices of tokens in input string, delimited
    !! by the characters in set, and stores them into first and last output
    !! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    logical, dimension(0:len(string)+1) :: is_first, is_last, is_separator
    integer :: n, slen

    slen = len(string)

    is_separator(0) = .true.
    do concurrent (n = 1:slen)
      is_separator(n) = index(set,string(n:n)) > 0
    end do
    is_separator(slen+1) = .true.

    is_separator = [.true., (index(set,string(n:n)) > 0, n=1,slen), .true.]

    is_first = cshift(is_separator,-1)
    is_last = cshift(is_separator,1)

    first = pack([(n, n = 1, slen+1)], is_first(1:slen+1))
    last = pack([(n, n = 0, slen)], is_last(0:slen))

  end subroutine split_first_last_v1


  pure subroutine split_first_last_v2(string, set, first, last)
    !! Computes the first and last indices of tokens in input string, delimited
    !! by the characters in set, and stores them into first and last output
    !! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    logical, dimension(0:len(string)+1) :: is_separator
    integer :: n, slen

    slen = len(string)
    is_separator = [.true., (index(set,string(n:n)) > 0, n=1,slen), .true.]
    first = pack([(n, n = 1, slen+1)], is_separator(0:slen))
    last = pack([(n, n = 0, slen)], is_separator(1:slen+1))
  end subroutine split_first_last_v2


  pure subroutine split_first_last_v3(string, set, first, last)
    !! Computes the first and last indices of tokens in input string, delimited
    !! by the characters in set, and stores them into first and last output
    !! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    logical, dimension(0:len(string)+1) :: is_separator
    integer :: n, slen, i, p, q, pos

    slen = len(string)

    is_separator = .false.
    is_separator(0) = .true.
    is_separator(slen+1) = .true.

    n = 0
    if (slen > 0) then
      p = 1
      do while(p < slen)
        n = n+1
        pos = scan(string(p:),set)
        if (pos == 0) exit
        is_separator(p + pos - 1) = .true.
        p = p + pos
      end do
    end if

    allocate(first(n),last(n))
    p = 0
    q = 0
    outer: do i = 1, n
      inner: do
        p = p + 1
        if (is_separator(p-1)) exit inner
      end do inner
      first(i) = p
      inner2: do
        q = q + 1
        if (is_separator(q)) exit inner2
      end do inner2
      last(i) = q-1      
    end do outer

  end subroutine split_first_last_v3

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
    integer :: result_pos, bound

    if (len(string) == 0) then
      pos = 1
      return
    end if

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    if (backward) then
      bound = min(len(string), max(pos-1, 0))
      result_pos = scan(string(:bound), set, back=.true.)
    else
      result_pos = scan(string(min(pos+1, len(string)):), set) + pos
      if (result_pos < pos+1) result_pos = len(string) + 1
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
