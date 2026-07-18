module display
      use math_constants
      use arithmetic
      use string
      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none
      !
      ! Standard output
      !
      integer, parameter :: STDOUNIT = OUTPUT_UNIT
      ! -------------------------------------
      ! MESSAGE PRIORITIES
      ! -------------------------------------
      !
      integer, parameter :: MSG_DEBUG  = 0
      integer, parameter :: MSG_NORMAL = 10
      integer, parameter :: MSG_WARNING = 80
      integer, parameter :: MSG_ERROR  = 100
      !
      ! Messages whose priority is strictly
      ! below MSG_PRIORITY_THRESH will not
      ! be displayed
      !
      integer, save :: MSG_PRIORITY_THRESH = MSG_NORMAL

contains

      subroutine syprn(a)
            real(F64), dimension(:, :), intent(inout) :: a

            integer :: k, l
            integer :: n

            n = size(a, dim=1)

            do k = 1, n
                  do l = k + 1, n
                        a(k, l) = a(l, k)
                  end do
            end do

            do k = 1, n
                  write(STDOUNIT, "(100F12.8)") (a(k, l), l = 1, n)
            end do
            flush(STDOUNIT)
      end subroutine syprn


      subroutine print_array_1d(a, ncols)
            real(F64), dimension(:), intent(in) :: a
            integer, intent(in)                 :: ncols
            
            integer :: n, nrows, k, l, l0, l1
            character(:), allocatable :: s

            n = size(a)
            if (modulo(n, ncols) > 0) then
                  nrows = n / ncols + 1
            else
                  nrows = n / ncols
            end if

            do k = 1, nrows
                  l0 = (k - 1) * ncols + 1
                  l1 = min(n, k * ncols)
                  if (l0 == 1) then
                        s = "[" // str(a(l0)) // "_F64"
                  else
                        s = str(a(l0)) // "_F64"
                  end if
                  do l = l0 + 1, l1
                        s = s // ", " // str(a(l)) // "_F64"
                  end do
                  if (l1 < n) then 
                        s = s // ", &"
                  else
                        s = s // "]"
                  end if
                  call msg(s)
            end do
      end subroutine print_array_1d

      
      subroutine geprn(a)
            real(F64), dimension(:, :), intent(in) :: a

            integer :: k, l, m, n

            m = size(a, dim=1)
            n = size(a, dim=2)
            
            do k = 1, m
                  write(STDOUNIT, "(999F20.10)") (a(k, l), l = 1, n)
            end do
            flush(STDOUNIT)
      end subroutine geprn

      
      subroutine imsg(s, i, priority, width)
            ! --------------------------------------------------------
            ! Display a label-number message in the two-column format:
            ! DESCRIPTION_STRING  INTEGER_NUMBER
            ! call imsg("NUBER OF A", 20)
            ! > NUBER OF A: 20
            !
            character(len=*), intent(in) :: s
            integer, intent(in)          :: i
            integer, optional            :: priority
            integer, optional, intent(in):: width
           
            integer :: max_len
            character(:), allocatable :: label, label0
            integer :: p

            if (present(width)) then
                  max_len = width
            else
                  max_len = 40
            end if

            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        label = s
                        if (len_trim(label) <= max_len) then
                              allocate(character(max_len) :: label0)
                              label0 = label
                              write(STDOUNIT, "(1X,A,I0)") label0, i
                              deallocate(label0)
                        else
                              write(STDOUNIT, "(1X,A)") trim(label) // " ..."
                              write(STDOUNIT, "(1X,I0)") i
                        end if
                        flush(STDOUNIT)
                  end if
            end if
      end subroutine imsg


      subroutine dmsg(s, d, fmt, priority, width)
            ! ------------------------------------------
            ! Display "nubmer of ..." message:
            ! call imsg("NUBER OF A", 20.d+0)
            ! > NUBER OF A: 20.000... (format dependent)
            !
            character(len=*), intent(in)           :: s
            real(F64), intent(in)                  :: d
            character(len=*), optional, intent(in) :: fmt
            integer, optional                      :: priority
            integer, optional, intent(in)          :: width

            character(:), allocatable :: title
            character(len=20) :: strd
            integer :: p, w
            character(len=20) :: outfmt

            if (present(width)) then
                  w = width
            else
                  w = 40
            end if

            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        allocate(character(w) :: title)
                        title = s
                        write(outfmt, '("(1X,A",I0,",A)")') w
                        if (present(fmt)) then
                              write(strd, "("//trim(fmt)//")") d
                              write(STDOUNIT, outfmt) title, adjustl(strd)
                        else
                              write(outfmt, '("(1X,A",I0,",F20.12)")') w
                              write(STDOUNIT, outfmt) title, d
                        end if
                        deallocate(title)
                        flush(STDOUNIT)
                  end if
            end if
      end subroutine dmsg 


      subroutine smsg(s1, s2, priority, width)
            ! ------------------------------------------
            ! Display "nubmer of ..." message:
            ! call imsg("NUBER OF A", 20.d+0)
            ! > NUBER OF A: 20.000... (format dependent)
            !
            character(len=*), intent(in) :: s1, s2
            integer, optional            :: priority
            integer, optional, intent(in):: width

            character(:), allocatable :: title
            integer :: p, w
            character(len=20) :: fmt

            if (present(width)) then
                  w = width
            else
                  w = 40
            end if

            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        allocate(character(w) :: title)
                        title = s1
                        write(fmt, '("(1X,A",I0,",A)")') w
                        write(STDOUNIT, fmt) title, adjustl(s2)
                        deallocate(title)
                        flush(STDOUNIT)
                  end if
            end if
      end subroutine smsg 


      subroutine msg(s, priority, underline)
            !
            ! Output character string to the standard output
            !
            character(len=*), intent(in)  :: s
            logical, optional, intent(in) :: underline
            integer, optional, intent(in) :: priority

            integer :: p
            logical :: u

            if (present(underline)) then
                  u = underline
            else
                  u = .false.
            end if
            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        write(STDOUNIT, "(1X,A)") s
                        if (u) then
                              call midrule(width=len_trim(s), priority=p)
                        end if
                        flush(STDOUNIT)
                  end if
            end if
      end subroutine msg


      subroutine toprule(priority, width)
            integer, optional, intent(in) :: priority
            integer, optional, intent(in) :: width

            integer :: p, w

            if (present(width)) then
                  w = width
            else
                  w = 76
            end if
            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        write(STDOUNIT, "(1X,A)") repeat("-", w)
                  end if
            end if
      end subroutine toprule


      subroutine toprule_double(priority, width)
            integer, optional, intent(in) :: priority
            integer, optional, intent(in) :: width

            integer :: p, w

            if (present(width)) then
                  w = width
            else
                  w = 76
            end if
            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        write(STDOUNIT, "(1X,A)") repeat("=", w)
                  end if
            end if
      end subroutine toprule_double


      subroutine midrule(priority, width)
            integer, optional :: priority
            integer, optional :: width

            integer :: p, w

            if (present(width)) then
                  w = width
            else
                  w = 76
            end if
            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        write(STDOUNIT, "(1X,A)") repeat("-", w)
                  end if
            end if
      end subroutine midrule


      subroutine blankline(priority)
            integer, optional, intent(in) :: priority
            integer :: p

            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        write(STDOUNIT, "(1X)")
                  end if
            end if
      end subroutine blankline


      subroutine dotted_separator(n, priority)
            integer, intent(in) :: n
            integer, optional, intent(in) :: priority

            integer :: p
            character(:), allocatable :: s

            if (present(priority)) then
                  p = priority
            else
                  p = MSG_NORMAL
            end if
            if (this_image() == 1 .or. p > MSG_NORMAL) then
                  if (p >= MSG_PRIORITY_THRESH) then
                        s = repeat(". ", n / 2 + 1)
                        write(STDOUNIT, "(1X,A)") s(1:n)
                  end if
            end if
      end subroutine dotted_separator
end module display
