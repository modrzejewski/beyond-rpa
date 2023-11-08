module report
      use gparam
      use images
      use display
      use string
      use io

      implicit none
      save
      !
      ! Make sure that the subroutines of this
      ! module should be are called only
      ! master image/thread
      !
      integer                     :: REPORTUNIT = -1
      logical, private            :: ISENABLED = .false.
      character(len=1), parameter :: CELLSEP = ";"

contains

      subroutine rep_setpath(p)
            character(len=*), intent(in) :: p

            if (IMG_ISMASTER) then
                  if (.not. ISENABLED) then
                        REPORTUNIT = io_text_open(p, "replace")
                        ISENABLED = .true.
                  else
                        call msg("CURRENT REPORT NOT FINALIZED", priority=MSG_ERROR)
                        stop
                  end if
            else
                  call msg("SLAVE ACCESSING REPORT", priority=MSG_ERROR)
                  stop
            end if
      end subroutine rep_setpath


      subroutine rep_update(cells, ncells)
            character(*), dimension(:), intent(in) :: cells
            integer, intent(in)                    :: ncells

            integer :: k
            character(:), allocatable :: fmt

            fmt = "(" // str(ncells) // "(A,:,'" // CELLSEP // "'))"
            if (IMG_ISMASTER) then
                  if (ISENABLED) then
                        write(REPORTUNIT, fmt) (trim(cells(k)), k = 1, ncells)
                        flush(REPORTUNIT)
                  else
                        call msg("REPORT NOT INITIALIZED", priority=MSG_ERROR)
                        stop
                  end if
            else
                  call msg("SLAVE CANNOT ACCESS REPORT FILE", &
                        priority=MSG_ERROR)
                  stop
            end if
      end subroutine rep_update


      subroutine rep_finalize()
            if (IMG_ISMASTER) then
                  if (ISENABLED) then
                        close(REPORTUNIT)
                        ISENABLED = .false.
                  else
                        call msg("REPORT NOT INITIALIZED", priority=MSG_ERROR)
                        stop
                  end if
            else
                  call msg("SLAVE CANNOT ACCESS REPORT FILE", priority=MSG_ERROR)
                  stop
            end if
      end subroutine rep_finalize
end module report
