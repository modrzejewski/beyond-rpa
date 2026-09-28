module ecp_definitions
      use arithmetic
      use math_constants
      use periodic
      use display
      use io
      use string

      implicit none

      type TECPConfig
            !
            ! Pseudopotential configuration for an atom or an element
            ! ---
            !
            ! Atomic number
            !
            integer :: Z
            !
            ! Path to the text file with pseudopotential parameters
            !
            character(:), allocatable :: PathToParams
            !
            ! Number of core electrons represented by the pseudopotential
            !
            integer :: NCoreEl
            !
            ! Maximum angular momentum of the pseudopotential projector
            !
            integer :: Lmax
            !
            ! Total number of Gaussian functions
            !
            integer :: NGauss
            !
            ! True if spin-orbit pseudopotential parameters are available
            !
            logical :: SpinOrbit
            !
            ! Literature citation for the pseudopotential
            !
            character(:), allocatable :: Citation
      end type TECPConfig

contains
      
      subroutine pp_queryecp(basis_path, element, lmax, ngauss, ncoreel, spin_orbit, citation)
            character(*), intent(in)               :: basis_path
            integer, intent(in)                    :: element
            integer, intent(out)                   :: lmax
            integer, intent(out)                   :: ngauss
            integer, intent(out)                   :: ncoreel
            logical, intent(out)                   :: spin_orbit
            character(:), allocatable, intent(out) :: citation
            
            character(:), allocatable :: line
            character(:), allocatable :: key, val
            character(:), allocatable :: keyup
            character(:), allocatable :: targetkey1, targetkey2
            character(:), allocatable :: s1, s23, s2, s3
            integer :: u
            logical :: eof, foundelement
            integer :: l, ngaussl
            integer :: k
            logical :: comment, blank

            lmax = -1
            ngauss = 0
            ncoreel = 0
            spin_orbit = .false.
            citation = ""
            if (len_trim(basis_path) == 0) then
                  call msg("Pseudopotential parameter file path is empty for element " // &
                        trim(ELNAME_SHORT(element)) // &
                        ". This is an internal error and should not happen.", MSG_ERROR)
                  error stop
            end if
            u = io_text_open(basis_path, "OLD")
            !
            ! Scroll through the text file until one of the target keys if found
            !
            targetkey1 = trim(ELNAME_SHORT(element)) // "-ECP"
            targetkey2 = trim(ELNAME_SHORT(element)) // "-SPIN-ORBIT-ECP"
            !
            ! Scroll to the header of the ECP section
            !
            call io_text_readline(line, u, eof)
            scroll1: do while (.not. eof)
                  comment = iscomment(line)
                  blank = isblank(line)
                  if (.not. (comment .or. blank)) then
                        call split(line, key, val)
                        if (uppercase(key) == "$ECP") then
                              exit scroll1
                        end if
                  end if
                  call io_text_readline(line, u, eof)
            end do scroll1

            if (eof) then
                  !
                  ! No ECP section found
                  !
                  close(u)
                  return
            end if
            !
            ! Search for the requested elment in the ECP section
            !
            foundelement = .false.
            call io_text_readline(line, u, eof)
            scroll2: do while (.not. eof)
                  comment = iscomment(line)
                  blank = isblank(line)
                  if (.not. (comment .or. blank)) then
                        call split(line, key, val)
                        keyup = uppercase(key)
                        if (keyup == targetkey1) then
                              foundelement = .true.
                              exit scroll2
                        else if (keyup == targetkey2) then
                              foundelement = .true.
                              spin_orbit = .true.
                              exit scroll2
                        end if

                        if (keyup == "$END") then
                              exit scroll2
                        end if
                  end if
                  call io_text_readline(line, u, eof)
            end do scroll2
            
            if (.not. foundelement) then
                  close(u)
                  return
            else
                  !
                  ! Read the number of electrons represented by the PP and
                  ! lmax. The line might contain an optional comment string.
                  !
                  ! --optional-string-- NCoreElectrons LMAX+1
                  !
                  call split(val, s1, s23)
                  call split(s23, s2, s3)
                  if (s3 == "") then
                        read(s1, *) ncoreel
                        read(s2, *) lmax
                  else
                        read(s2, *) ncoreel
                        read(s3, *) lmax
                  end if
            end if
            lmax = lmax - 1

            if (lmax < 0) then
                  call msg("Invalid ECP parameters for " // ELNAME_LONG(element), MSG_ERROR)
                  call imsg("Invalid max angular momentum: " // str(lmax), MSG_ERROR)
                  stop
            end if

            if (ncoreel < 0) then
                  call msg("Invalid ECP parameters for " // ELNAME_LONG(element), MSG_ERROR)
                  call msg("Invalid number of core electrons: " // str(ncoreel), MSG_ERROR)
                  error stop
            end if

            if (spin_orbit) then
                  !
                  ! The parameters of the local part of the PP are not a part of the input format
                  ! for spin-orbit ECPs. However, for compatibility with the scalar PP subroutines,
                  ! we will keep in memory an extra entry for 0.0000 * Exp(-1.0000 * r^2).
                  !
                  ngauss = 1
                  l = 1
            else
                  ngauss = 0
                  l = 0
            end if
            call io_text_readline(line, u, eof)
            do while (l < lmax+2 .and. .not. eof)
                  call split(line, key, val)
                  if (.not. iscomment(line) .and. .not. isblank(line)) then
                        if (isinteger(key)) then
                              l = l + 1
                              read(key, *) ngaussl
                              ngauss = ngauss + ngaussl
                              !
                              ! Scroll to the next angular momentum projector
                              !
                              do k = 1, ngaussl
                                    call io_text_readline(line, u, eof)
                              end do
                        end if
                  else if (uppercase(key) == "!@CITATION") then
                        citation = val
                  end if
                  call io_text_readline(line, u, eof)
            end do

            if (l .ne. lmax+2) then
                  call msg("Invalid ECP parameters for " // ELNAME_LONG(element), priority=MSG_ERROR)
                  error stop
            end if

            close(u)
      end subroutine pp_queryecp


      subroutine pp_getecp(basis_path, element, lmax, ngauss, ncoreel, coeff, so_coeff, expn, nkl)
            !
            ! Read pseudopotential parameters from a text file.
            !
            character(*), intent(in)             :: basis_path
            integer, intent(in)                  :: element
            integer, intent(out)                 :: lmax
            integer, dimension(:), intent(out)   :: ngauss
            integer, intent(out)                 :: ncoreel
            real(F64), dimension(:), intent(out) :: coeff
            real(F64), dimension(:), intent(out) :: so_coeff
            real(F64), dimension(:), intent(out) :: expn
            integer, dimension(:), intent(out)   :: nkl
            
            character(:), allocatable :: line
            character(:), allocatable :: key, val
            character(:), allocatable :: s23, s1, s2, s3
            character(:), allocatable :: targetkey1, targetkey2
            integer :: u
            logical :: eof
            integer :: l, i
            integer :: k
            integer :: n_angular_parts
            logical :: comment
            logical :: blank
            logical :: spin_orbit

            u = io_text_open(basis_path, "OLD")
            spin_orbit = .false.
            !
            ! Scroll through the text file until one of the target keys appears
            !
            targetkey1 = trim(ELNAME_SHORT(element)) // "-ECP"
            targetkey2 = trim(ELNAME_SHORT(element)) // "-SPIN-ORBIT-ECP"
            !
            ! Scroll to ECP section
            !
            call io_text_readline(line, u, eof)
            scroll1: do while (.not. eof)
                  comment = iscomment(line)
                  blank = isblank(line)
                  if (.not. (comment .or. blank)) then
                        call split(line, key, val)
                        if (uppercase(key) == "$ECP") then
                              exit scroll1
                        end if
                  end if
                  call io_text_readline(line, u, eof)
            end do scroll1
            !
            ! Search for the target element within the ECP section
            !
            call io_text_readline(line, u, eof)
            scroll2: do while (.not. eof)
                  comment = iscomment(line)
                  blank = isblank(line)
                  if (.not. (comment .or. blank)) then
                        call split(line, key, val)
                        if (uppercase(key) == targetkey1) then
                              exit scroll2
                        else if (uppercase(key) == targetkey2) then
                              spin_orbit = .true.
                              exit scroll2
                        end if
                  end if
                  call io_text_readline(line, u, eof)
            end do scroll2
            !
            ! Read the number of electrons represented by the PP and
            ! lmax. The line may contain an optional comment string.
            !
            ! --optional-string-- NCoreElectrons LMAX+1
            !
            call split(val, s1, s23)
            call split(s23, s2, s3)
            if (s3 == "") then
                  read(s1, *) ncoreel
                  read(s2, *) lmax
            else
                  read(s2, *) ncoreel
                  read(s3, *) lmax
            end if
            lmax = lmax - 1
            !
            ! Read the linear coeffs of scalar and spin-orbit PPs, n_{kl} exponents, 
            ! and Alpha_{kl} exponents
            !
            if (spin_orbit) then
                  !
                  ! The parameters of the local part of the PP are not a part of the input format
                  ! for spin-orbit ECPs. However, for compatibility with the scalar PP subroutines,
                  ! we will keep in memory an extra entry for 0.0000 * Exp(-1.0000 * r^2).
                  !
                  l = 1
                  i = 1
                  coeff(i) = ZERO
                  so_coeff(i) = ZERO
                  nkl(i) = 2
                  expn(i) = ONE
                  ngauss(l) = 1
            else
                  l = 0
                  i = 0
            end if
            n_angular_parts = lmax + 2
            call io_text_readline(line, u, eof)
            do while (l < n_angular_parts .and. .not. eof)
                  comment = iscomment(line)
                  blank = isblank(line)
                  if (.not. (comment .or. blank)) then
                        call split(line, key, val)
                        if (isinteger(key)) then
                              l = l + 1
                              read(key, *) ngauss(l)
                              do k = 1, ngauss(l)
                                    i = i + 1
                                    call io_text_readline(line, u, eof)
                                    if (spin_orbit) then
                                          if (l > 2) then
                                                read(line, *) coeff(i), so_coeff(i), nkl(i), expn(i)
                                          else
                                                !
                                                ! The input format does not include SO coefficients for the angular
                                                ! momentum S
                                                !
                                                read(line, *) coeff(i), nkl(i), expn(i)
                                                so_coeff(i) = ZERO
                                          end if
                                    else
                                          read(line, *) coeff(i), nkl(i), expn(i)
                                          so_coeff(i) = ZERO
                                    end if
                              end do
                        end if
                  end if
                  call io_text_readline(line, u, eof)
            end do
            
            if (maxval(nkl(1:i)) > 2 .or. minval(nkl(1:i)) < 0) then
                  call msg("Invalid ECP parameter: R^N exponent outside of the allowed range 0..2", MSG_ERROR)
                  error stop
            end if
            
            if (minval(expn(1:i)) < ZERO) then
                  call msg("Invalid ECP parameter: negative exponent", MSG_ERROR)
                  error stop
            end if
            
            close(u)
      end subroutine pp_getecp
end module ecp_definitions
