! --------------------------------------------------
!           SCF guess density (AO basis)
! --------------------------------------------------
module guess
      use math_constants
      use periodic
      use gparam
      use string
      use io

      implicit none

contains

      function atomic_guess_filename(znum)
            character(len=:), allocatable   :: atomic_guess_filename
            integer, intent(in)             :: znum

            character(:), allocatable :: elname
            !
            ! Two-letter name of the element
            !
            elname = lowercase(elname_short(znum))
            atomic_guess_filename = ATOMIC_GUESS_DIR // trim(elname) // ".txt"
      end function atomic_guess_filename


      function guess_inquire_atomic(znum)
            !
            ! Test if atomic-density guess is available for 
            ! the given value of ZNUM
            !
            logical             :: guess_inquire_atomic
            integer, intent(in) :: znum

            character(len=:), allocatable :: s
            
            s = atomic_guess_filename(znum)  
            guess_inquire_atomic = io_exists(s)
      end function guess_inquire_atomic
      

      subroutine guess_atomic(rho_ao)
            !
            ! Generate guess density matrix as a superposition
            ! of atomic densities
            !
            double precision, dimension(:, :), intent(out) :: rho_ao

            integer :: k, l, s
            integer :: i0, j0
            integer :: n
            integer :: z
            integer :: p, q
            integer :: p0, q0
            character(len=:), allocatable :: readpath
            logical :: rholoaded

            rho_ao = ZERO
            do k = 1, NELEMENT
                  z = ELEMENT(k)
                  if (guess_inquire_atomic(z)) then
                        readpath = atomic_guess_filename(z)
                        !
                        ! Read atomic blocks of the density matrix
                        !
                        rholoaded = .false.
                        do s = 1, 2
                              do l = REAL_ATOMS(1, s), REAL_ATOMS(2, s)
                                    if (INUCLZ(l) .eq. z) then
                                          if (.not. rholoaded) then
                                                n = IDX(l+1) - IDX(l)
                                                i0 = IDX(l)
                                                j0 = i0
                                                call io_text_read(rho_ao(i0:i0+n-1, j0:j0+n-1), readpath)
                                                rholoaded = .true.
                                          else
                                                p0 = IDX(l)
                                                q0 = p0
                                                do q = 0, n - 1
                                                      do p = 0, n - 1
                                                            rho_ao(p0+p, q0+q) = rho_ao(i0+p, j0+q)
                                                      end do
                                                end do
                                          end if
                                    end if
                              end do
                        end do
                  else
                        call msg("Note: Incomplete SCF guess. Atomic density missing for " // elname_short(z))
                  end if
            end do
      end subroutine guess_atomic
end module guess
