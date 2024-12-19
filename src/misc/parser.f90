module parser
      use gparam
      use iso_fortran_env
      use arithmetic
      use math_constants
      use string
      use periodic
      use display
      use images
      use report
      use io
      use h_xcfunc
      use sys_definitions
      use scf_definitions
      use rpa_definitions
      use thc_definitions
      use TwoStepCholesky_definitions
      use Pseudopotential, only: pp_ZNumbers
      use grid_definitions

      implicit none

contains

      pure function basename(s)
            !
            ! Strip off the directory and extension parts from the full file path:
            ! /dir1/dir2/NAME.ext -> NAME
            ! See the comments below for a list of corner cases.
            !
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: basename
            
            integer :: l, m
            integer :: pos_dirsep, pos_dot
            character(len=:), allocatable :: w

            w = adjustl(s)
            l = len_trim(w)

            if (l > 0) then
                  pos_dot = -1
                  pos_dirsep = -1
                  mloop: do m = l, 1, -1
                        if (w(m:m) == "." .and. pos_dot == -1) then
                              pos_dot = m
                        end if
                  
                        if (w(m:m) == DIRSEP .and. pos_dirsep == -1) then
                              pos_dirsep = m
                        end if
                  end do mloop
                  
                  if (pos_dot > 0 .and. pos_dirsep > 0) then
                        if (pos_dot > pos_dirsep+1) then
                              !
                              ! /dir1/dir2/NAME.ext
                              !
                              basename = w(pos_dirsep+1:pos_dot-1)
                        else if (pos_dot == pos_dirsep+1) then
                              !
                              ! /dir1/dir2/.NAME
                              !
                              basename = w(pos_dot+1:l)
                        else
                              !
                              ! /dir1/dir.2/NAME
                              !
                              basename = w(pos_dirsep+1:l)
                        end if
                  else if (pos_dot > 0) then
                        if (pos_dot > 1) then
                              !
                              ! NAME.ext
                              !
                              basename = w(1:pos_dot-1)
                        else
                              !
                              ! .NAME
                              !
                              basename = w(pos_dot+1:l)
                        end if
                  else if (pos_dirsep > 0) then
                        !
                        ! /dir1/dir2/NAME
                        !
                        basename = w(pos_dirsep+1:l)
                  else
                        !
                        ! NAME
                        !
                        basename = w(1:l)
                  end if
            else
                  basename = ""
            end if
      end function basename


      pure function dirname(s)
            !
            ! Remove file name and return only the directory part from 
            ! the full path of a file,
            ! /dir1/dir2/file -> /dir1/dir2
            ! Note that the trailing directory separator is removed.
            !
            character(len=:), allocatable :: dirname
            character(len=*), intent(in)  :: s
            
            integer :: k, l, m

            m = 0
            l = len_trim(s)

            do k = l, 1, -1
                  if (s(k:k) .eq. DIRSEP) then
                        m = k - 1
                        exit
                  end if
            end do

            if (m .gt. 0) then
                  dirname = s(1:m)
            else
                  dirname = ""
            end if
      end function dirname


      function readbytes(s)
            integer(I64) :: readbytes
            character(*), intent(in) :: s

            real(F64) :: n
            integer(I64) :: unit
            character(3) :: sunit

            read(s, *) n, sunit

            select case (uppercase(sunit))
                  case ("MB")
                        unit = MEGABYTE
                  case ("MIB")
                        unit = MEBIBYTE
                  case ("GB")
                        unit = GIGABYTE
                  case ("GIB")
                        unit = GIBIBYTE
                  case ("TB")
                        unit = TERABYTE
                  case ("TIB")
                        unit = TEBIBYTE
                  case ("PB")
                        unit = PETABYTE
                  case ("PIB")
                        unit = PEBIBYTE
                  case default
                        unit = -1
            end select

            readbytes = tobyte(n, unit)
      end function readbytes
      

      subroutine readline(u, line, eof, expecteof)
            integer, intent(in)                :: u
            character(len=DEFLEN), intent(out) :: line
            logical, intent(out)               :: eof
            logical, optional, intent(in)      :: expecteof

            logical :: expecteof0
            integer :: stat

            if (present(expecteof)) then
                  expecteof0 = expecteof
            else
                  expecteof0 = .false.
            end if

            read(u, "(A)", iostat=stat) line

            if (stat .eq. iostat_end) then
                  eof = .true.
            else
                  eof = .false.
            end if

            if (eof .and. .not. expecteof) then
                  call msg("PARSER ERROR: UNEXPECTED END OF FILE")
                  stop
            end if
      end subroutine readline


      subroutine echo(line)
            character(*), intent(in) :: line

            if (.not. (isblank(line) .or. iscomment(line))) then
                  call msg("> " // adjustl(line))
            end if
      end subroutine echo


      subroutine scroll(u, lineidx)
            !
            ! Scroll textfile so that the next line
            ! being read has index LINEIDX
            !
            integer, intent(in) :: u
            integer, intent(in) :: lineidx

            character(len=DEFLEN) :: line
            integer :: stat, k

            lines: do k = 1, lineidx - 1
                  read(u, "(A)", iostat=stat) line
                  if (stat .eq. iostat_end) then
                        call msg("PARSER ERROR: TEXTFILE ENDED UNEXPECTEDLY", &
                              priority=MSG_ERROR)
                        stop
                  end if
            end do lines
      end subroutine scroll


      subroutine skipblock(u, linenumber)
            integer, intent(in)    :: u
            integer, intent(inout) :: linenumber

            character(len=DEFLEN) :: line
            character(:), allocatable :: lup
            integer :: stat

            lines: do
                  linenumber = linenumber + 1
                  read(u, "(A)", iostat=stat) line
                  if (stat .eq. iostat_end) then
                        call msg("PARSER ERROR: TEXTFILE ENDED UNEXPECTEDLY", &
                              priority=MSG_ERROR)
                        stop
                  end if
                  call echo(line)
                  lup = adjustl(uppercase(line))
                  if (lup == "END") then
                        exit lines
                  end if
            end do lines
      end subroutine skipblock


      subroutine queryxyz(path, lineno, natom, nelement, zcount, zlist)
            ! -------------------------------------------------------------
            ! Determine the number of atoms specified in the geometry file.
            ! Also determine the number of unique elements and the number
            ! of representatives of each element.
            ! -------------------------------------------------------------
            ! PATH     
            !         Path to the XYZ file
            ! LINENO  
            !         Line number at which reading the file should start
            ! NATOM   
            !         Number of atoms (real + dummy)
            ! NELEMENT       
            !         Number of elements present in the XYZ file
            ! ZCOUNT
            !         ZCOUNT(K) is the number of representatives
            !         of the K-th element
            ! ZLIST
            !         ZLIST(K) is the Z number of the K-th element
            !
            character(len=*), intent(in)                    :: path
            integer, intent(in)                             :: lineno
            integer, intent(out)                            :: natom
            integer, intent(out)                            :: nelement
            integer, dimension(:), allocatable, intent(out) :: zcount
            integer, dimension(:), allocatable, intent(out) :: zlist

            character(len=DEFLEN) :: line
            character(:), allocatable :: keyword
            character(:), allocatable :: val
            character(:), allocatable :: keyup
            integer, dimension(KNOWN_ELEMENTS) :: num_elements
            integer :: u
            integer :: natom_declared
            logical :: lnatom_declared
            logical :: eof
            integer :: i, j
            integer :: znum
            integer :: stat

            open(newunit=u, file=path, status="old", &
                  access="sequential", position="rewind")
            !
            ! Scroll to the beginning of XYZ data
            !
            call scroll(u, lineno)
            nelement = 0
            num_elements = 0
            natom = 0
            lnatom_declared = .false.
            read(u, "(A)", iostat=stat) line
            eof = (stat .eq. iostat_end)
            xyzdata: do while (.not. eof)
                  if (.not. (isblank(line) .or. iscomment(line))) then
                        call split(line, keyword, val)
                        keyup = uppercase(keyword)
                        if (keyup .eq. "CHARGE" .or. &
                              keyup .eq. "UNITS" .or. &
                              keyup .eq. "REAL" .or. &
                              keyup .eq. "NOPENORB" .or. &
                              keyup .eq. "NOPENELA" .or. &
                              keyup .eq. "NOPENELB" .or. &
                              keyup .eq. "MULT" .or. &
                              keyup .eq. "MULTIPLICITY" .or. &
                              keyup .eq. "MONOMER_A" .or. &
                              keyup .eq. "MONOMER_B") then
                              read(u, "(A)", iostat=stat) line
                              eof = (stat .eq. iostat_end)
                              cycle xyzdata
                        else if (keyup .eq. "END") then
                              exit xyzdata
                        else if (isinteger(keyup)) then
                              lnatom_declared = .true.
                              read(keyup, *) natom_declared
                        else
                              !
                              ! Line contains atom coordinates
                              !
                              natom = natom + 1
                              znum = znumber_short(keyup)
                              if (znum .eq. 0) then
                                    call msg("PARSER ERROR: UNKNOWN ELEMENT " // keyup, &
                                          priority=MSG_ERROR)
                                    stop
                              end if
                              if (num_elements(znum) == 0) then
                                    nelement = nelement + 1
                              end if
                              num_elements(znum) = num_elements(znum) + 1
                        end if
                  end if
                  read(u, "(A)", iostat=stat) line
                  eof = (stat .eq. iostat_end)
            end do xyzdata

            if (nelement == 0) then
                  call msg("PARSER ERROR: NO DATA FOUND", priority=MSG_ERROR)
                  stop
            end if

            if (lnatom_declared) then
                  if (natom .ne. natom_declared) then
                        call msg("PARSER ERROR: WRONG NUMBER OF ATOMS DECLARED:", &
                              priority=MSG_ERROR)
                        call imsg("DECLARED NUMBER OF ATOMS", natom_declared, &
                              priority=MSG_ERROR)
                        call imsg("ACTUAL NUMBER OF ATOMS", natom, priority=MSG_ERROR)
                        stop
                  end if
            end if

            allocate(zcount(nelement))
            allocate(zlist(nelement))

            j = 1
            iloop: do i = 1, KNOWN_ELEMENTS
                  if (num_elements(i) > 0) then
                        zcount(j) = num_elements(i)
                        zlist(j) = i
                        if (j == nelement) exit iloop
                        j = j + 1
                  end if
            end do iloop

            close(u)
      end subroutine queryxyz


      function isint(x)
            !
            ! Test if double precision number X 
            ! represents an integer value. This function
            ! should not be used for general purposes
            ! in unaltered form due to the specific choice
            ! of EPS parameter (see below).
            !
            logical :: isint
            double precision, intent(in) :: x
            
            double precision, parameter :: eps = 1.d-12

            if (abs(dble(nint(x)) - x) <= eps) then
                  isint = .true.
            else
                  isint = .false.
            end if
      end function isint

      
      subroutine loadxyz(path, lineno, units, xyz, xyz_a, xyz_b)
            character(len=*), intent(in)                    :: path
            integer, intent(in)                             :: lineno
            integer, intent(in)                             :: units
            type(tmolecule), intent(inout)                  :: xyz
            type(tmolecule), intent(out)                    :: xyz_a
            type(tmolecule), intent(out)                    :: xyz_b

            integer :: units_local
            character(len=DEFLEN) :: line
            character(:), allocatable :: keyword, val
            character(:), allocatable :: keyup
            logical :: eof
            integer :: stat
            integer :: mult
            logical :: lmult, lnopenorb, lreal
            logical :: lnopenela, lnopenelb
            integer :: natom, i, znum, zsum
            double precision :: x, y, z
            double precision :: dnopenela
            double precision :: dnopenelb
            double precision :: dcharge
            integer :: t1, t2, t3
            integer :: u, s
            integer :: a1, a2, b1, b2, ca, cb
            logical :: monomer_input

            associate (inuclz => xyz%inuclz, atomr => xyz%atomr, real_atoms => xyz%real_atoms, &
                  charge => xyz%charge, multiplicity => xyz%multiplicity, ne => xyz%ne, &
                  nopenorb => xyz%nopenorb, nopenela => xyz%nopenela, nopenelb => xyz%nopenelb, &
                  charge_frac => xyz%charge_frac, ne_frac => xyz%ne_frac, &
                  nopenela_frac => xyz%nopenela_frac, nopenelb_frac => xyz%nopenelb_frac)

                  open(newunit=u, file=path, status="old", access="sequential", position="rewind")
                  !
                  ! Scroll the text file to the beginning of the coordinate data
                  !
                  call scroll(u, lineno)

                  units_local = units
                  lreal = .false.
                  mult = 1
                  lmult = .false.
                  lnopenorb = .false.
                  lnopenela = .false.
                  lnopenelb = .false.
                  dcharge = ZERO
                  natom = 0
                  monomer_input = .false.
                  ca = 0
                  cb = 0
                  a1 = -1
                  a2 = -1
                  b1 = -1
                  b2 = -1

                  read(u, "(A)", iostat=stat) line
                  eof = (stat .eq. iostat_end)
                  xyzdata: do while (.not. eof)
                        call split(line, keyword, val)
                        keyup = uppercase(keyword)
                        if (.not. (isblank(keyup) .or. iscomment(keyup) .or. &
                              isinteger(keyup))) then
                              select case (keyup)
                              case ("REAL")
                                    read(val, *) a1, a2
                                    lreal = .true.
                              case ("MONOMER_A")
                                    read(val, *) a1, a2
                                    monomer_input = .true.
                              case ("MONOMER_B")
                                    read(val, *) b1, b2
                                    monomer_input = .true.
                              case ("CHARGE_A")
                                    read(val, *) ca
                              case ("CHARGE_B")
                                    read(val, *) cb
                              case ("CHARGE")
                                    read(val, *) dcharge
                              case ("NOPENORB")
                                    read(val, *) nopenorb
                                    lnopenorb = .true.
                              case ("NOPENELA")
                                    read(val, *) dnopenela
                                    lnopenela = .true.
                              case ("NOPENELB")
                                    read(val, *) dnopenelb
                                    lnopenelb = .true.
                              case ("MULT", "MULTIPLICITY")
                                    read(val, *) mult
                                    lmult = .true.
                              case ("UNITS")
                                    select case(uppercase(val))
                                    case ("BOHR")
                                          units_local = UNITS_BOHR
                                    case ("ANGS")
                                          units_local = UNITS_ANGS
                                    case default
                                          call msg("INVALID VALUE OF KEYWORD UNITS", MSG_ERROR)
                                          stop
                                    end select
                              case ("END")
                                    exit xyzdata
                              case default
                                    natom = natom + 1
                                    !
                                    ! Read atomic coordinates
                                    ! KEYWORD contains two-letter
                                    ! element's symbol
                                    !
                                    read(val, *) x, y, z
                                    znum = znumber_short(keyword)
                                    inuclz(natom) = znum
                                    if (units_local .eq. UNITS_BOHR) then
                                          atomr(1, natom) = x
                                          atomr(2, natom) = y
                                          atomr(3, natom) = z
                                    else
                                          atomr(1, natom) = tobohr(x)
                                          atomr(2, natom) = tobohr(y)
                                          atomr(3, natom) = tobohr(z)
                                    end if
                              end select
                        end if
                        !
                        ! Read next line
                        !
                        read(u, "(A)", iostat=stat) line
                        eof = (stat .eq. iostat_end)
                  end do xyzdata

                  if (lreal) then
                        if (a1 < 1 .or. a2 > natom .or. a1 > a2) then
                              call msg("INVALID INDICES OF NON-DUMMY ATOMS", MSG_ERROR)
                              stop
                        end if
                        real_atoms(:, 1) = [a1, a2]
                        real_atoms(:, 2) = [1, 0]
                  else
                        real_atoms(:, 1) = [1, natom]
                        real_atoms(:, 2) = [1, 0]
                  end if

                  zsum = 0
                  do s = 1, 2
                        do i = real_atoms(1, s), real_atoms(2, s)
                              zsum = zsum + inuclz(i)
                        end do
                  end do

                  if (lmult) then
                        nopenela = mult - 1
                        nopenorb = nopenela
                        nopenelb = 0
                        nopenela_frac = ROKS_NEUNIT * nopenela
                        nopenelb_frac = 0
                        charge = nint(dcharge)
                        charge_frac = ROKS_NEUNIT * charge
                        multiplicity = mult
                        ne = zsum - nint(dcharge)
                        ne_frac = ROKS_NEUNIT * ne
                  else
                        if (.not. lnopenela) then
                              nopenela = 0
                              nopenela_frac = 0
                        else
                              if (isint(dnopenela)) then
                                    nopenela = nint(dnopenela)
                                    nopenela_frac = ROKS_NEUNIT * nopenela
                              else
                                    nopenela = int(dnopenela)
                                    nopenela_frac = int(dnopenela * dble(ROKS_NEUNIT))
                              end if
                        end if

                        if (.not. lnopenelb) then
                              nopenelb = 0
                              nopenelb_frac = 0
                        else
                              if (isint(dnopenelb)) then
                                    nopenelb = nint(dnopenelb)
                                    nopenelb_frac = ROKS_NEUNIT * nopenelb
                              else
                                    nopenelb = int(dnopenelb)
                                    nopenelb_frac = int(dnopenelb * dble(ROKS_NEUNIT))
                              end if
                        end if

                        if (.not. lnopenorb) then
                              nopenorb = nopenela_frac / ROKS_NEUNIT
                              if (modulo(nopenela_frac, ROKS_NEUNIT) > 0) then
                                    nopenorb = nopenorb + 1
                              end if
                        end if

                        multiplicity = abs(nopenela - nopenelb) + 1
                  end if

                  if (isint(dcharge)) then
                        charge = nint(dcharge)
                        charge_frac = ROKS_NEUNIT * charge
                        ne = zsum - nint(dcharge)
                        ne_frac = ROKS_NEUNIT * ne
                  else
                        charge = int(dcharge)
                        charge_frac = int(dcharge * dble(ROKS_NEUNIT))
                        ne = int(dble(zsum) - dcharge)
                        ne_frac = int((dble(zsum) - dcharge) * dble(ROKS_NEUNIT))
                  end if
                  
                  if (monomer_input) then
                        if (a1 > a2 .or. a1 < 1 .or. a2 > natom) then
                              call msg("INVALID ATOM INDEX IN MONOMER_A", MSG_ERROR)
                              stop
                        end if

                        if (b1 > b2 .or. b1 < 1 .or. b2 > natom) then
                              call msg("INVALID ATOM INDEX IN MONOMER_B", MSG_ERROR)
                              stop
                        end if

                        if ((a1 < b1 .and. b1 <= a2) .or. (a1 > b1 .and. a1 <= b2)) then
                              call msg("OVERLAPPING RANGES OF MONOMER_A AND MONOMER_B", MSG_ERROR)
                              stop
                        end if

                        if (.not. (b1 == a2+1 .or. a1 == b2+1)) then
                              call msg("MONOMER_A AND MONOMER_B MUST FORM A CONTINUOUS RANGE", MSG_ERROR)
                              stop
                        end if

                        call copymolecule(xyz_a, xyz)
                        call copymolecule(xyz_b, xyz)
                        xyz_a%real_atoms(:, 1) = [a1, a2]
                        xyz_a%real_atoms(:, 2) = [1, 0]
                        xyz_b%real_atoms(:, 1) = [b1, b2]
                        xyz_b%real_atoms(:, 2) = [1, 0]
                        !
                        ! Number of electrons of monomer A
                        !
                        zsum = 0
                        do i = a1, a2
                              zsum = zsum + inuclz(i)
                        end do
                        xyz_a%ne = zsum - xyz_a%charge
                        xyz_a%ne_frac = xyz_a%ne * ROKS_NEUNIT
                        xyz_a%charge = ca
                        xyz_a%charge_frac = xyz_a%charge * ROKS_NEUNIT
                        !
                        ! Number of electrons of monomer B
                        !
                        zsum = 0
                        do i = b1, b2
                              zsum = zsum + inuclz(i)
                        end do
                        xyz_b%ne = zsum - xyz_b%charge
                        xyz_b%ne_frac = xyz_b%ne * ROKS_NEUNIT
                        xyz_b%charge = cb
                        xyz_b%charge_frac = xyz_b%charge * ROKS_NEUNIT

                        if (charge .ne. xyz_a%charge + xyz_b%charge) then
                              call msg("INVALID MONOMER CHARGES", MSG_ERROR)
                              stop
                        end if
                  end if

                  if (.not. isint(dcharge)) then
                        t1 = modulo(ne_frac, ROKS_NEUNIT)
                        t2 = modulo(nopenela_frac+nopenelb_frac, ROKS_NEUNIT)
                        t3 = modulo(ne_frac-nopenela_frac-nopenelb_frac, 2)
                        if (abs(t1-t2) .ne. 0 .or. t3 .ne. 0) then
                              call msg("PARSER ERROR: INCONSISTENT NUMBER OF ELECTRONS SPECIFIED")
                              call dmsg("CHARGE", dble(charge_frac)/dble(ROKS_NEUNIT))
                              call dmsg("ALPHA ELECTRONS (OPEN SHELL)", dble(nopenela_frac)/dble(ROKS_NEUNIT))
                              call dmsg("BETA ELECTRONS (OPEN SHELL)", dble(nopenelb_frac)/dble(ROKS_NEUNIT))
                              stop
                        end if
                  end if

                  if (lnopenelb .and. .not. lnopenela) then
                        call msg("PARSER ERROR: NUMBER OF ALPHA ELECTRONS IN "// &
                              "THE OPEN SHELL MUST BE SPECIFIED", priority=MSG_ERROR)
                        stop
                  end if

                  if ((modulo(ne, 2) > 0) .and. nopenela_frac == 0 .and. nopenelb_frac == 0) then
                        call msg("ERROR: MULTIPLICITY/OCCUPATION NUMBERS", &
                              priority=MSG_ERROR)
                        call msg("MUST BE PROVIDED FOR OPEN-SHELL SYSTEMS", priority=MSG_ERROR)
                        call msg("XYZ FILE")
                        call msg(path)
                        call imsg("COORDS START AT LINE", lineno)
                        stop
                  end if

                  if (lmult .and. (lnopenela .or. lnopenelb .or. lnopenorb .or. &
                        .not. isint(dcharge))) then
                        call msg("PARSER ERROR: MULT KEYWORD CANNOT BE USED", &
                              priority=MSG_ERROR)
                        stop
                  end if

                  if (mult < 1 .or. mult > ne + 1) then
                        call msg("ERROR: UNPHYSICAL VALUE OF MULTIPLICITY SPECIFIED", &
                              priority=MSG_ERROR)
                        stop
                  end if

                  close(u)
            end associate
      end subroutine loadxyz


      subroutine newmolecule(xyz, path, lineno, units)
            type(tmolecule), intent(out)        :: xyz
            character(len=*), intent(in)        :: path
            integer, intent(in)                 :: lineno
            integer, intent(in)                 :: units

            type(tmolecule) :: monomer_a, monomer_b

            xyz%path = path
            xyz%start = lineno
            call queryxyz(path, lineno, xyz%natom, xyz%nelement, xyz%zcount, xyz%zlist)
            allocate(xyz%atomr(3, xyz%natom))
            allocate(xyz%inuclz(xyz%natom))
            call loadxyz(path, lineno, units, xyz, monomer_a, monomer_b)
      end subroutine newmolecule


      subroutine newdimer(xyz_a, xyz_b, xyz_ab, path, lineno, units)
            type(tmolecule), intent(out)        :: xyz_a
            type(tmolecule), intent(out)        :: xyz_b
            type(tmolecule), intent(out)        :: xyz_ab
            character(len=*), intent(in)        :: path
            integer, intent(in)                 :: lineno
            integer, intent(in)                 :: units

            xyz_ab%path = path
            xyz_ab%start = lineno
            call queryxyz(path, lineno, xyz_ab%natom, xyz_ab%nelement, xyz_ab%zcount, xyz_ab%zlist)
            allocate(xyz_ab%atomr(3, xyz_ab%natom))
            allocate(xyz_ab%inuclz(xyz_ab%natom))
            call loadxyz(path, lineno, units, xyz_ab, xyz_a, xyz_b)
      end subroutine newdimer


      subroutine querymolecule(xyz, natom, nelement, zcount, zlist)
            type(tmolecule), intent(in)        :: xyz
            integer, intent(out)               :: natom
            integer, intent(out)               :: nelement
            integer, dimension(:), intent(out) :: zcount
            integer, dimension(:), intent(out) :: zlist
            
            natom = xyz%natom
            nelement = xyz%nelement
            zcount(1:nelement) = xyz%zcount(1:nelement)
            zlist(1:nelement) = xyz%zlist(1:nelement)
      end subroutine querymolecule

      
      subroutine loadmolecule(xyz, inuclz, atomr, real_atoms, charge, multiplicity, &
            ne, nopenorb, nopenela, nopenelb, ne_frac, nopenela_frac, nopenelb_frac)
            
            type(tmolecule), intent(in)                    :: xyz
            integer, dimension(:), intent(out)             :: inuclz
            double precision, dimension(:, :), intent(out) :: atomr
            integer, dimension(:, :), intent(out)          :: real_atoms
            double precision, intent(out)                  :: charge
            integer, intent(out)                           :: multiplicity
            integer, intent(out)                           :: ne
            integer, intent(out)                           :: nopenorb
            integer, intent(out)                           :: nopenela
            integer, intent(out)                           :: nopenelb
            integer, intent(out)                           :: ne_frac
            integer, intent(out)                           :: nopenela_frac
            integer, intent(out)                           :: nopenelb_frac

            integer :: nelement, natom
            
            nelement = xyz%nelement
            natom = xyz%natom

            inuclz(1:natom) = xyz%inuclz(1:natom)
            atomr(1:3, 1:natom) = xyz%atomr(1:3, 1:natom)

            real_atoms = xyz%real_atoms
            if (modulo(xyz%charge_frac, ROKS_NEUNIT) == 0) then
                  charge = dble(xyz%charge)
            else
                  charge = dble(xyz%charge_frac) / dble(ROKS_NEUNIT)
            end if
            multiplicity = xyz%multiplicity
            ne = xyz%ne
            nopenorb = xyz%nopenorb
            nopenela = xyz%nopenela
            nopenelb = xyz%nopenelb
            ne_frac = xyz%ne_frac
            nopenela_frac = xyz%nopenela_frac
            nopenelb_frac = xyz%nopenelb_frac
      end subroutine loadmolecule


      function ang2int(ang)
            ! ----------------------------------------------------------
            ! Convert a single-letter symbol of angular momentum into
            ! an integer numerical value,
            !
            ! L -> -1
            ! S -> 0
            ! P -> 1
            ! ...
            !
            ! The error code -2 is returned if the single letter symbol
            ! does not correspond to any angular function.
            ! -----------------------------------------------------------
            integer                  :: ang2int
            character(*), intent(in) :: ang
            
            select case (uppercase(ang))
            case ("S")
                  ang2int = 0
            case ("P")
                  ang2int = 1
            case ("D")
                  ang2int = 2
            case ("F")
                  ang2int = 3
            case ("G")
                  ang2int = 4
            case ("H")
                  ang2int = 5
            case ("I")
                  ang2int = 6
            case ("K")
                  ang2int = 7
            case ("L")
                  ang2int = -1
            case default
                  ang2int = -2
            end select
      end function ang2int
      

      subroutine querybasis(basis_path, element, nshell, maxl)
            character(*), intent(in) :: basis_path
            integer, intent(in) :: element
            integer, intent(out) :: nshell, maxl
            
            character(len=DEFLEN) :: line
            character(len=1) :: momentum
            integer :: u = 10
            integer :: shtyp, nprm
            integer :: i

            open(newunit=u, file=basis_path, status="old", access="sequential", &
                  position="rewind")

            call find(element, u)
            maxl = 0
            nshell = 0
            shells: do
                  read(u, "(A)") line

                  if (iscomment(line)) cycle shells
                  if (isblank(line)) exit shells
                  if (isend(line)) exit shells

                  read(line, *) momentum, nprm

                  if (nprm > MAX_NPRM) then
                        call msg("NUMBER OF PRIMITIVES GREATER THAN MAX_NPRM", &
                              priority=MSG_ERROR)
                        call msg("CHANGE THE MAX_NPRM PARAMETER AND RECOMPILE THE CODE", &
                              priority=MSG_ERROR)
                        stop
                  end if

                  shtyp = ang2int(momentum)

                  if (shtyp .eq. -2) then
                        call smsg("UNKNOWN ANGULAR FUNCTION SYMBOL", momentum, MSG_ERROR)
                        stop
                  end if

                  if (shtyp > MAX_L) then
                        call msg("ANGULAR MOMENTUM GREATER THAN MAX_L", MSG_ERROR)
                        call msg("CHANGE THE MAX_L PARAMETER AND RECOMPILE THE CODE", MSG_ERROR)
                        stop
                  end if
                  !
                  ! SHTYP == -1 corresponds to L basis functions,
                  ! which is a shorthand notation for S + P functions.
                  !
                  if (.not. shtyp .eq. -1) then
                        if (shtyp .gt. maxl) maxl = shtyp
                        nshell = nshell + 1
                  else
                        if (1 .gt. maxl) maxl = 1
                        nshell = nshell + 2
                  end if

                  do i = 1, nprm
                        read(u, "(A)") line
                  end do
            end do shells

            close(u)
      end subroutine querybasis

      
      subroutine getbasis(basis_path, element, cntr, expn, shelltype, nprimitives, nshell)
            character(*), intent(in) :: basis_path
            integer, intent(in) :: element
            double precision, dimension(:, :), intent(inout) :: cntr
            double precision, dimension(:, :), intent(inout) :: expn
            integer, dimension(:), intent(inout) :: shelltype
            integer, dimension(:), intent(inout) :: nprimitives
            integer, intent(out) :: nshell

            integer :: u

            open(newunit=u, file=basis_path, status="old", access="sequential", &
                  position="rewind")
            
            call find(element, u)
            call fetch(u, cntr, expn, shelltype, nprimitives, nshell)

            close(u)
      end subroutine getbasis
      
      
      subroutine fetch(u, contraction, expn, shelltype, nprimitives, nbasis)
            integer, intent(in) :: u
            double precision, dimension(:, :), intent(inout) :: contraction
            double precision, dimension(:, :), intent(inout) :: expn
            integer, dimension(:), intent(inout) :: shelltype
            integer, dimension(:), intent(inout) :: nprimitives
            integer, intent(out) :: nbasis
            
            character(len=DEFLEN) :: line
            character(len=1) :: momentum
            integer :: nprm
            integer :: shtyp
            integer :: p
            integer :: n
            double precision :: e, c, c1, c2

            nbasis = 1
            shells: do
                  read(u, "(A)") line

                  if (iscomment(line)) cycle shells
                  if (isblank(line)) exit shells
                  if (isend(line)) exit shells

                  read(line, *) momentum, nprm

                  shtyp = ang2int(momentum)

                  if (shtyp .eq. -2) then
                        call smsg("UNKNOWN ANGULAR FUNCTION SYMBOL", momentum, MSG_ERROR)
                        stop
                  end if

                  if (shtyp > MAX_L) then
                        call imsg("BASIS SET CONTAINS FUNCTIONS WIGHT ANGULAR MOMENTUM", shtyp)
                        call msg("CHANGE MAX_L PARAMETER AND RECOMPILE THE PROGRAM")
                        stop
                  end if

                  if (.not. shtyp .eq. -1) then
                        shelltype(nbasis) = shtyp
                        nprimitives(nbasis) = nprm

                        do p = 1, nprm
                              read(u, "(A)") line
                              read(line, *) n, e, c
                              contraction(n, nbasis) = c
                              expn(n, nbasis) = e
                        end do

                        nbasis = nbasis + 1
                  else
                        shelltype(nbasis) = 0
                        shelltype(nbasis + 1) = 1
                        nprimitives(nbasis) = nprm
                        nprimitives(nbasis + 1) = nprm

                        do p = 1, nprm
                              read(u, "(A)") line
                              read(line, *) n, e, c1, c2
                              contraction(n, nbasis) = c1
                              contraction(n, nbasis + 1) = c2
                              expn(n, nbasis) = e
                              expn(n, nbasis + 1) = e
                        end do

                        nbasis = nbasis + 2
                  end if
            end do shells
            
            nbasis = nbasis - 1
            if (nbasis .eq. 0) then
                  call msg("PARSER ERROR: NO SHELL DATA FOUND", &
                        priority=MSG_ERROR)
            end if
      end subroutine fetch

      
      subroutine find(element, u)
            integer, intent(in) :: element
            integer, intent(in) :: u

            character(len=DEFLEN) :: line
            logical :: blank, comment, eof
            logical :: success
            integer :: z

            eof = .false.
            lines: do while (.not. eof)
                  read(u, "(A)") line
                  comment = iscomment(line)
                  blank = isblank(line)
                  if (.not. (comment .or. blank)) then
                        eof = isend(line)
                        if (.not. eof) then
                              z = isnew_element(line)
                              if (z .eq. element) then
                                    success = .true.
                                    exit lines
                              end if
                        else
                              success = .false.
                              exit lines
                        end if
                  end if
            end do lines

            if (.not. success) then
                  call msg("PARSER ERROR: ELEMENT NOT AVAILABLE IN THE BASIS SET", &
                        priority=MSG_ERROR)
                  call smsg("ELEMENT", ELNAME_LONG(element), priority=MSG_ERROR)
                  stop
            end if
      end subroutine find


      function isbegin(l)
            logical                           :: isbegin
            character(len=DEFLEN), intent(in) :: l

            character(len=DEFLEN) :: a

            isbegin = .false.

            if (.not. isblank(l)) then
                  read(l, *) a
                  if (a .eq. "$DATA") isbegin = .true.
            end if
      end function isbegin
      

      function isend(l)
            logical                           :: isend
            character(len=DEFLEN), intent(in) :: l

            character(len=DEFLEN) :: a

            isend = .false.

            if (.not. isblank(l)) then
                  read(l, *) a
                  if (a .eq. "$END") isend = .true.
            end if
      end function isend


      function isnew_element(l)
            integer                           :: isnew_element
            character(len=DEFLEN), intent(in) :: l

            character(len=DEFLEN) :: a

            read(l, *) a
            isnew_element = znumber_long(a)
      end function isnew_element


      subroutine read_inputfile(System, SCFParams, RPAParams, Chol2Params, THCParams, filename)
            type(TSystem), intent(out)      :: System
            type(TSCFParams), intent(out)   :: SCFParams
            type(TRPAParams), intent(out)   :: RPAParams
            type(TChol2Params), intent(out) :: Chol2Params
            type(TTHCParams), intent(out)   :: THCParams
            character(len=*), intent(in)    :: filename

            integer :: OldXYZFormat
            character(len=DEFLEN) :: line
            character(:), allocatable :: key, val
            character(:), allocatable :: ecp_path
            integer :: u, linenumber, stat
            integer :: current_block
            integer :: k, z
            integer :: AtomIdx, ChargeIdx
            logical :: RPADefined, XYZDefined
            integer, parameter :: block_none = 0
            integer, parameter :: block_auxint = 1
            integer, parameter :: block_ECP = 2
            integer, parameter :: block_RhoSpher = 3
            integer, parameter :: block_RhoDiff = 4
            integer, parameter :: block_NonSCF = 5
            integer, parameter :: block_PointCharges = 6
            integer, parameter :: block_RPA = 7
            integer, parameter :: block_XYZ = 8
            integer, parameter :: block_SCF = 9

            RPADefined = .false.
            XYZDefined = .false.
            
            open(newunit=u, file=filename, status="old", &
                  access="sequential", position="rewind")

            call toprule()
            call msg("LOADED INPUT FILE")
            call msg(filename)
            call midrule()
            OldXYZFormat = -1
            AtomIdx = -1
            ChargeIdx = -1
            linenumber = 0
            current_block = block_none
            lines: do
                  linenumber = linenumber + 1
                  read(u, "(A)", iostat=stat) line
                  if (stat .eq. iostat_end) then
                        exit lines
                  end if
                  call echo(line)
                  if (isblank(line) .or. iscomment(line)) then
                        cycle lines
                  end if

                  call split(line, key, val)
                  key = uppercase(key)
                  select case (key)
                  case ("AUXINT")
                        if (val == "") then
                              current_block = block_auxint
                              cycle lines
                        end if
                  case ("ECP")
                        current_block = block_ECP
                        cycle lines
                  case ("RHOSPHER")
                        current_block = block_RhoSpher
                        AUXINT_TYPE2 = AUX_RHO_SPHER
                        cycle lines
                  case ("RHODIFF")
                        current_block = block_RhoDiff
                        AUXINT_TYPE2 = AUX_RHO_DIFF
                        cycle lines
                  case ("NONSCF")
                        current_block = block_NonSCF
                        cycle lines
                  case ("POINT-CHARGES", "POINT_CHARGES")
                        current_block = block_PointCharges
                        cycle lines
                  case ("RPA")
                        if (JOBTYPE /= JOB_REAL_UKS_RPA) then
                              call msg("Cannot define RPA parameters for non-RPA job types", MSG_ERROR)
                              error stop
                        end if
                        RPADefined = .true.
                        current_block = block_RPA
                        cycle lines
                  case ("SCF")
                        current_block = block_SCF
                        cycle lines
                  case ("XYZ")
                        if (JOBTYPE == JOB_REAL_UKS_RPA .or. JOBTYPE == JOB_REAL_UKS_SP &
                              .or. JOBTYPE == JOB_REAL_UKS_INT) then
                              XYZDefined = .true.
                              current_block = block_XYZ
                              cycle lines
                        end if
                  case ("END")
                        if (current_block /= block_none) then
                              if (current_block == block_XYZ) then
                                    call sys_Init(System, SYS_TOTAL)
                              end if
                              current_block = block_none
                              cycle lines
                        end if
                  end select

                  if (current_block == block_ECP) then
                        if (key == "*") then
                              call read_ecp_path(ecp_path, val)
                              call ECP_PARAMS_PATH%set_default(ecp_path)
                              call SCFParams%ECPFile%set_default(ecp_path)
                        else
                              z = znumber_short(key)
                              if (z > 0) then
                                    call read_ecp_path(ecp_path, val)
                                    call ECP_PARAMS_PATH%update(ecp_path, z)
                                    call SCFParams%ECPFile%update(ecp_path, z)
                              else
                                    call msg("Unknown element specified in the ECP block", MSG_ERROR)
                                    error stop
                              end if
                        end if
                  else if (current_block == block_auxint) then
                        call read_block_auxint(SCFParams, line)
                  else if (current_block == block_RhoSpher) then
                        call read_block_RhoSpher(line)
                  else if (current_block == block_RhoDiff) then
                        call read_block_RhoDiff(line)
                  else if (current_block == block_NonSCF) then
                        call read_block_NonSCF(SCFParams, line)
                  else if (current_block == block_PointCharges) then
                        if (key == "NCHARGES") then
                              ChargeIdx = 0
                              if (allocated(POINT_CHARGES_Q)) then
                                    deallocate(POINT_CHARGES_Q)
                                    deallocate(POINT_CHARGES_R)
                              end if
                              read(val, *) POINT_CHARGES_N
                              allocate(POINT_CHARGES_Q(POINT_CHARGES_N))
                              allocate(POINT_CHARGES_R(3, POINT_CHARGES_N))
                        else
                              if (ChargeIdx > -1) then
                                    ChargeIdx = ChargeIdx + 1
                                    if (ChargeIdx <= POINT_CHARGES_N) then
                                          read(line, *) POINT_CHARGES_Q(ChargeIdx), (POINT_CHARGES_R(k, ChargeIdx), k=1,3)
                                    else
                                          call msg("Inconsistent number of charges specified", MSG_ERROR)
                                          stop
                                    end if
                              else
                                    call msg("Unspecified number of charges", MSG_ERROR)
                                    stop
                              end if
                        end if
                  else if (current_block == block_RPA) then
                        call read_block_RPA(RPAParams, SCFParams, Chol2Params, THCParams, line)
                  else if (current_block == block_SCF) then
                        call read_block_SCF(SCFParams, line)
                  else if (current_block == block_XYZ) then
                        call read_block_XYZ(System, AtomIdx, line)
                  else
                        select case (uppercase(key))
                        case ("BASIS")
                              call read_basis_set(line)
                              call read_AOBasisPath(line, SCFParams)
                        case ("F12BASIS")
                              call read_F12BasisPath(line, SCFParams)
                        case ("LINDEP_THRESH")
                              call read_lindep_thresh(line)
                        case ("JOBTYPE")
                              call read_jobtype(line)
                        case ("XCFUNC")
                              call read_xcmodel(line)
                        case ("AUXINT")
                              call read_auxint_name(line)
                        case ("XYZ")
                              !
                              ! Compatibility mode
                              !
                              OldXYZFormat = linenumber + 1
                              call read_geomfile(u, line, linenumber)
                        case ("REPORT")
                              call read_report(line)
                        case ("UNITS")
                              call read_units(line)
                        case ("SPHERBASIS")
                              call read_spherbasis(line)
                        case ("DFT_DISP")
                              call read_dft_disp(SCFParams, line)
                        case ("DFTD3_BJ_A1")
                              call read_dftd3_bj_a1(line)
                        case ("DFTD3_BJ_A2")
                              call read_dftd3_bj_a2(line)
                        case ("DFTD3_BJ_S8")
                              call read_dftd3_bj_s8(line)
                        case ("DFTD3_R6")
                              call read_dftd3_r6(line)
                        case ("DFTD3_S8")
                              call read_dftd3_s8(line)
                        case ("DFTD3_3BODY")
                              call read_dftd3_3body(line)
                        case ("MBD_RSSCS_BETA")
                              call read_mbd_rsscs_beta(line)
                        case ("SCF_THRESH")
                              call read_scf_thresh(line)
                        case ("SCF_MAXIT")
                              call read_scf_maxit(line)
                        case ("SCF_GUESS")
                              call read_scf_guess(line)
                        case ("SCF_SAVERHO")
                              call read_scf_saverho(line)
                        case("ONLYRIGHT")
                              call read_onlyright(line)
                        case("PROP_EXC_EXC_DIP")
                              call read_prop_exc_exc_dip(line)
                        case("PROP_GR_EXC")
                              call read_prop_gr_exc(line)
                        case("PROP_EXC_EXC_SO")
                              call read_prop_exc_exc_so(line)
                        case("SLATER_BASIS")
                              call read_slater_basis(line)
                        case("SLATER_FILE_SCF")
                              call read_slater_scf(line)
                        case("SLATER_FILE_AUX")
                              call read_slater_aux(line)
                        case("SLATER_FILE_1E")
                              call read_slater_1e(line)
                        case("SLATER_FILE_2E")
                              call read_slater_2e(line)
                        case("SCF_READ")
                              call read_scf_read(line)
                        case("CCSP_READ")
                              call read_ccsp_read(line)
                        case("EOM_MEM")
                              call read_eom_mem(line)
                        case("EOM_READ")
                              call read_eom_read(line)
                        case("SCF_WRITE")
                              call read_scf_write(line)
                        case("CCSP_WRITE")
                              call read_ccsp_write(line)
                        case("EOM_WRITE")
                              call read_eom_write(line)
                        case ("CC_SAVERHO")
                              call read_cc_saverho(line)
                        case ("MP2_MWORD")
                              call read_mp2_mword(line)
                        case ("ATOMIC_MASS")
                              call read_atomic_mass(line)
                        case ("ORIG")
                              call read_origin(line)
                        case ("ECP_GRAD")
                              call read_ecp_grad(line)
                        case ("CISD_GUESS_S") 
                              call read_cisd_gs(line)
                        case ("CISD_GUESS_D")
                              call read_cisd_gd(line)
                        case ("CISD_GUESS_SD")
                              call read_cisd_gsd(line)
                        case ("POINT_GROUP")
                              call read_point_group(line)
                        case ("CC_BINTV")
                              call read_cc_bintv(line)
                        case ("CC_BINTO")
                              call read_cc_binto(line)
                        case ("CC_KINTV")
                              call read_cc_kintv(line)
                        case ("CC_KINTO")
                              call read_cc_kinto(line)
                        case ("CC_FROZEN")
                              call read_cc_frozen(line)
                        case ("CC_NON")
                              call read_cc_non(line)
                        case ("CC_DIIS_NMAX")
                              call read_cc_diis_nmax(line)
                        case("CC_DIIS_NSTART")
                              call read_cc_diis_nstart(line)
                        case("CC_DIIS_NRELAX")
                              call read_cc_diis_nrelax(line)
                        case ("MBPT_ORDER")
                              call read_mbpt_order(line)
                        case ("S_ORDER")
                              call read_s_order(line)
                        case("MAXPT")
                              call read_maxpt(line)
                        case("DAV_QCONVTHRSH")
                              call read_dav_qconvthresh(line)
                        case("CC_AMP_THRESH")
                              call read_cc_amp_thresh(line)
                        case("CC_E_THRESH")
                              call read_cc_e_thresh(line)
                        case("CC_MIN_HLGAP")
                              call read_cc_min_hlgap(line)
                        case ("CC_NOSYMMETRY")
                              call read_cc_nosymmetry(line)
                        case ("CC_IEXCI")
                              call read_cc_irrepexci(line)
                        case ("CC_IEXCI_S")
                              call read_cc_irrepexci_s(line)
                        case ("CC_IEXCI_D")
                              call read_cc_irrepexci_d(line)
                        case ("CC_LEXCI_S")
                              call read_cc_lexci_s(line)
                        case ("CC_LEXCI_D")
                              call read_cc_lexci_d(line)
                        case ("CC_LEXCI_SD")
                              call read_cc_lexci_sd(line)
                        case ("CC_UEXCI_S")
                              call read_cc_uexci_s(line)
                        case ("CC_UEXCI_D")
                              call read_cc_uexci_d(line)
                        case ("CC_UEXCI_SD")
                              call read_cc_uexci_sd(line)
                        case ("CC_SING_LEXCI_S")
                              call read_cc_sing_lexci_s(line)
                        case ("CC_SING_LEXCI_D")
                              call read_cc_sing_lexci_d(line)
                        case ("CC_SING_UEXCI_S")
                              call read_cc_sing_uexci_s(line)
                        case ("CC_SING_UEXCI_D")
                              call read_cc_sing_uexci_d(line)
                        case ("CC_TRIP_LEXCI_S")
                              call read_cc_trip_lexci_s(line)
                        case ("CC_TRIP_LEXCI_D")
                              call read_cc_trip_lexci_d(line)
                        case ("CC_TRIP_UEXCI_S")
                              call read_cc_trip_uexci_s(line)
                        case ("CC_TRIP_UEXCI_D")
                              call read_cc_trip_uexci_d(line)                              
                        case ("CC_MULTIP")
                              call read_cc_multip(line)                         
                        case ("CC_EOM_DISKSPACE")
                              call read_cc_eom_diskspace(line)
                        case ("CC_EOM_MEMSPACE")
                              call read_cc_eom_memspace(line)
                        case ("OMEGA")
                              call read_lcomega(line)
                        case ("SREXX")
                              call read_lcsrexx(line)
                        case ("MISQUITTA_CT")
                              call read_misquitta_ct(line)
                        case ("OPTOMEGA_J2TYPE")
                              call read_optomega_j2type(line)
                        case ("OPTOMEGA_MIN")
                              call read_optomega_min(line)
                        case ("OPTOMEGA_MAX")
                              call read_optomega_max(line)
                        case ("GDD_NOUT")
                              call read_gdd_nout(line)
                        case ("GDD_MUMIN")
                              call read_gdd_mumin(line)
                        case ("MLRCS12_GPARAM")
                              call read_mlrcs12_gparam(line)
                        case ("MCSV2_GOPP")
                              call read_mcsv2_gopp(line)
                        case ("MCSV2_GPAR")
                              call read_mcsv2_gpar(line)
                        case ("WPBESOL_MLRCS_GPARAM")
                              call read_wpbesol_mlrcs_gparam(line)
                        case ("WPBE_MLRCS_GPARAM")
                              call read_wpbe_mlrcs_gparam(line)
                        case ("TAG_DFT_TOTAL_ENERGY")
                              call read_tag_dft_total_energy(line)
                        case ("TAG_DFT_DISP")
                              call read_tag_dft_disp(line)
                        case ("VIS_CUBEFILE")
                              call read_vis_cubefile(line)
                        case ("VIS_RHOA")
                              call read_vis_rhoa(line)
                        case ("VIS_RHOB")
                              call read_vis_rhob(line)
                        case ("CUBEFILE_SPACING")
                              call read_cubefile_spacing(line)
                        case ("CUBEFILE_RHO")
                              CUBEFILE_FUNC = ior(CUBEFILE_FUNC, VOL_FUNC_RHO)
                        case ("CUBEFILE_LAPL")
                              CUBEFILE_FUNC = ior(CUBEFILE_FUNC, VOL_FUNC_LAPL)
                        case ("CUBEFILE_TAU_UEG_TAU")
                              CUBEFILE_FUNC = ior(CUBEFILE_FUNC, VOL_FUNC_TAU_UEG_TAU)
                        case ("CUBEFILE_DX")
                              CUBEFILE_FUNC = ior(CUBEFILE_FUNC, VOL_FUNC_DX)
                        case ("CUBEFILE_MO")
                              call read_cubefile_mo(line)
                        case ("CUBEFILE_AO")
                              call read_cubefile_ao(line)
                        case ("RTTDDFT_TIMESTEP")
                              call read_rttddft_timestep(line)
                        case ("RTTDDFT_POLAR_FIELD")
                              call read_rttddft_polar_field(line)
                        case ("RTTDDFT_POLAR_NCYCLES")
                              call read_rttddft_polar_ncycles(line)
                        case ("RTTDDFT_POLAR_OMEGA")
                              call read_rttddft_polar_omega(line)
                        case ("RTTDDFT_POLAR_LAMBDA")
                              call read_rttddft_polar_lambda(line)
                        case default
                              call msg("Invalid keyword: " // key, MSG_ERROR)
                              stop
                        end select
                  end if
            end do lines
            close(u)
            !
            ! Check for errors in user's input
            !
            if (JOBTYPE .eq. JOB_UNKNOWN) then
                  call msg("PARSER ERROR: NO JOB TYPE SPECIFIED", &
                        priority=MSG_ERROR)
                  error stop
            end if

            if (JOBTYPE == JOB_REAL_UKS_RPA .and. .not. RPADefined) then
                  call msg("RPA parameters not defined", MSG_ERROR)
                  error stop
            end if

            if (NJOB .eq. 0 .and. JOBTYPE /= JOB_REAL_UKS_RPA .and. JOBTYPE /= JOB_REAL_UKS_SP &
                  .and. JOBTYPE /= JOB_REAL_UKS_INT) then
                  call msg("PARSER ERROR: NO GEOMETRY FILE SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            if (.not. XYZDefined) then
                  if (OldXYZFormat > -1) then
                        call msg("Reading xyz data using compatibility mode with the old SCF module")
                        ! --------------------------------------------------------------------------------
                        ! define system in case old scf is requested (for compatibility with old versions)
                        ! --------------------------------------------------------------------------------
                        open(newunit=u, file=filename, status="old", access="sequential")
                        call msg("OLDXYZFORMAT="//str(OldXYZFormat))
                        call scroll(u, OldXYZFormat)
                        do
                              read(u, "(A)", iostat=stat) line
                              call msg(">>>>>" // line)
                              if (stat .eq. iostat_end) then
                                    exit
                              end if
                              if (isblank(line) .or. iscomment(line)) then
                                    cycle
                              end if
                              
                              call split(line, key, val)
                              key = uppercase(key)

                              if (key == "END") then
                                    !
                                    ! End of xyz block
                                    !
                                    call sys_Init(System, SYS_TOTAL)
                                    exit
                              else
                                    call read_block_XYZ(System, AtomIdx, line)
                              end if
                        end do
                        close(u)
                        XYZDefined = .true.
                  end if
            end if

            if (.not. allocated(BASIS_SET_PATH)) then
                  call msg("NO BASIS SET SPECIFIED", MSG_ERROR)
                  stop
            end if

            if (JOBTYPE == JOB_RTTDDFT_POLAR) then
                  if (.not. allocated(RTTDDFT_POLAR_OMEGA)) then
                        call msg("NO ANGULAR FREQUENCIES SPECIFIED", MSG_ERROR)
                        stop
                  end if
            end if
            !
            ! Default path for ECP parameters will be the file of the basis set parameters
            !
            if (ECP_PARAMS_PATH%get_default() == "") then
                  call ECP_PARAMS_PATH%set_default(BASIS_SET_PATH)
            end if
            !
            ! Default path for ECP parameters will be the file of the basis set parameters
            !
            if (SCFParams%ECPFile%get_default() == "") then
                  call SCFParams%ECPFile%set_default(SCFParams%AOBasisPath)
            end if
            !
            ! Choose the set of orbitals applied in the RPA calculations.
            ! This adjustment can only be done once the level of beyond-RPA
            ! corrections is defined (TheoryLevel).
            !
            if (RPADefined) then
                  call rpa_Params_ChooseOrbitals(RPAParams, SCFParams)
            end if
            !
            ! Update effective nuclear charges if a pseudopotential is defined
            ! The ECP nuclear charges will be the same as physical charges if there's no pseudopotential
            ! on any of the atoms
            !
            if (XYZDefined) then
                  call pp_ZNumbers(System, SCFParams%ECPFile)
                  call sys_SortDistances(System)
            end if
      end subroutine read_inputfile


      subroutine read_ecp_path(path, s)
            character(:), allocatable, intent(out) :: path
            character(*), intent(in) :: s
            character(:), allocatable :: a

            a = uppercase(s)
            select case (a)
            case ("SMALL-CORE-RELATIVISTIC")
                  path = ECPDIR // "small-core-relativistic.txt"
            case ("SMALL-CORE-SR")
                  path = ECPDIR // "sr-small.txt"
            case default
                  call msg("Unknown pseudopotential requested", MSG_ERROR)
                  error stop
            end select
      end subroutine read_ecp_path
      

      subroutine read_basis_set(line)
            character(*), intent(in) :: line
            
            character(:), allocatable :: keyword
            character(:), allocatable :: a, a1, a2
            logical :: CustomFile
            
            call split(line, keyword, a)
            call split(a, a1, a2)
            CustomFile = .false.
            if (uppercase(a2) .ne. "") then
                  if (uppercase(a1) == "FILE") then
                        CustomFile = .true.
                        if (io_exists(a2)) then
                              BASIS_SET_PATH = a2
                              BASIS_SET_NAME = a2
                        else
                              call msg("BASIS SET FILE DOES NOT EXIST:", MSG_ERROR)
                              call msg(a2, MSG_ERROR)
                              stop
                        end if
                  else
                        call msg("INVALID KEYWORD: " // a1, MSG_ERROR)
                        stop
                  end if
            else
                  !
                  ! Standard basis set requested by its EMSL Basis Set Exchange name
                  !
                  select case(uppercase(a))
                  case ("AHLRICHS-VDZ")
                        BASIS_SET_PATH = "ahlrichs-vdz"
                        BASIS_SET_NAME = "Ahlrichs VDZ"
                  case ("3-21G")
                        BASIS_SET_PATH = "3-21g"
                        BASIS_SET_NAME = "3-21G"
                  case ("3-21++G")
                        BASIS_SET_PATH = "3-21g_diff_all"
                        BASIS_SET_NAME = "3-21++G"
                  case ("6-31G")
                        BASIS_SET_PATH = "6-31g"
                        BASIS_SET_NAME = "6-31G"
                  case ("6-31G**")
                        BASIS_SET_PATH = "6-31g_pol_all"
                        BASIS_SET_NAME = "6-31G**"
                  case ("6-31++G")
                        BASIS_SET_PATH = "6-31g_diff_all"
                        BASIS_SET_NAME = "6-31++G"
                  case ("6-31+G**")
                        BASIS_SET_PATH = "6-31g_diff_pol_all"
                        BASIS_SET_NAME = "6-31+G**"
                  case ("6-31++G**")
                        BASIS_SET_PATH = "6-31g_diff_all_pol_all"
                        BASIS_SET_NAME = "6-31++G**"
                  case ("6-31G(3DF,3PD)", "6-31G(3DF, 3PD)")
                        BASIS_SET_PATH = "6-31g_3df_3pd"
                        BASIS_SET_NAME = "6-31G(3df,3dp)"
                  case ("6-311G")
                        BASIS_SET_PATH = "6-311g"
                        BASIS_SET_NAME = "6-311G"
                  case ("6-311G**")
                        BASIS_SET_PATH = "6-311g_pol_all"
                        BASIS_SET_NAME = "6-311G**"
                  case ("6-311+G*")
                        BASIS_SET_PATH = "6-311g_diff_pol"
                        BASIS_SET_NAME = "6-311+G*"
                  case ("6-311++G**")
                        BASIS_SET_PATH = "6-311g_diff_all_pol_all"
                        BASIS_SET_NAME = "6-311++G**"
                  case ("6-311++G(2D,2P)", "6-311++G(2D, 2P)")
                        BASIS_SET_PATH = "6-311g_diff_all_2d_2p"
                        BASIS_SET_NAME = "6-311++G(2d,2p)"
                  case ("6-311++G(3DF,3PD)", "6-311++G(3DF, 3PD)")
                        BASIS_SET_PATH = "6-311g_diff_all_3df_3pd"
                        BASIS_SET_NAME = "6-311++G(3df,3pd)"
                  case ("6-311(3+,3+)G**", "6-311(3+, 3+)G**", "6-311(3+,3+)G(d,p)", &
                        "6-311(3+, 3+)G(d, p)")
                        BASIS_SET_PATH = "6-311g_triple_diff_all_pol_all"
                        BASIS_SET_NAME = "6-311(3+,3+)G**"
                  case ("CC-PVDZ", "CC-PV(D+D)Z")
                        BASIS_SET_PATH = "cc-pvddz"
                        BASIS_SET_NAME = "cc-pVDZ"
                  case ("CC-PVDZ_OLD", "CC-PVDZ-OLD")
                        BASIS_SET_PATH = "cc-pvdz"
                        BASIS_SET_NAME = "cc-pVDZ (old)"
                  case ("CC-PVDZ-PP")
                        BASIS_SET_PATH = "cc-pvdz-pp"
                        BASIS_SET_NAME = "cc-pVDZ-PP"
                  case ("AUG-CC-PVDZ", "AUG-CC-PV(D+D)Z")
                        BASIS_SET_PATH = "aug-cc-pvddz"
                        BASIS_SET_NAME = "aug-cc-pVDZ"
                  case ("AUG-CC-PVDZ_OLD", "AUG-CC-PVDZ-OLD")
                        BASIS_SET_PATH = "aug-cc-pvdz"
                        BASIS_SET_NAME = "aug-cc-pVDZ (old)"
                  case ("AUG-CC-PVDZ-PP")
                        BASIS_SET_PATH = "aug-cc-pvdz-pp"
                        BASIS_SET_NAME = "aug-cc-pVDZ-PP"
                  case ("D-AUG-CC-PVDZ")
                        BASIS_SET_PATH = "d-aug-cc-pvdz"
                        BASIS_SET_NAME = "d-aug-cc-pVDZ"
                  case ("CC-PVTZ", "CC-PV(T+D)Z")
                        BASIS_SET_PATH = "cc-pvtdz"
                        BASIS_SET_NAME = "cc-pVTZ"
                  case ("CC-PVTZ_OLD", "CC-PVTZ-OLD")
                        BASIS_SET_PATH = "cc-pvtz"
                        BASIS_SET_NAME = "cc-pVTZ (old)"
                  case ("CC-PVTZ-PP")
                        BASIS_SET_PATH = "cc-pvtz-pp"
                        BASIS_SET_NAME = "cc-pVTZ-PP"
                  case ("AUG-CC-PVTZ", "AUG-CC-PV(T+D)Z")
                        BASIS_SET_PATH = "aug-cc-pvtdz"
                        BASIS_SET_NAME = "aug-cc-pVTZ"
                  case ("AUG-CC-PVTZ_OLD", "AUG-CC-PVTZ-OLD")
                        BASIS_SET_PATH = "aug-cc-pvtz"
                        BASIS_SET_NAME = "aug-cc-pVTZ (old)"
                  case ("AUG-CC-PVTZ-PP")
                        BASIS_SET_PATH = "aug-cc-pvtz-pp"
                        BASIS_SET_NAME = "aug-cc-pVTZ-PP"
                  case ("D-AUG-CC-PVTZ")
                        BASIS_SET_PATH = "d-aug-cc-pvtz"
                        BASIS_SET_NAME = "d-aug-cc-pVTZ"
                  case ("CC-PVQZ", "CC-PV(Q+D)Z")
                        BASIS_SET_PATH = "cc-pvqdz"
                        BASIS_SET_NAME = "cc-pVQZ"
                  case ("CC-PVQZ_OLD", "CC-PVQZ-OLD")
                        BASIS_SET_PATH = "cc-pvqz"
                        BASIS_SET_NAME = "cc-pVQZ (old)"
                  case ("AUG-CC-PVQZ", "AUG-CC-PV(Q+D)Z")
                        BASIS_SET_PATH = "aug-cc-pvqdz"
                        BASIS_SET_NAME = "aug-cc-pVQZ"
                  case ("AUG-CC-PVQZ_OLD", "AUG-CC-PVQZ-OLD")
                        BASIS_SET_PATH = "aug-cc-pvqz"
                        BASIS_SET_NAME = "aug-cc-pVQZ (old)"
                  case ("D-AUG-CC-PVQZ")
                        BASIS_SET_PATH = "d-aug-cc-pvqz"
                        BASIS_SET_NAME = "d-aug-cc-pVQZ"
                  case ("CC-PV5Z", "CC-PV(5+D)Z")
                        BASIS_SET_PATH = "cc-pv5dz"
                        BASIS_SET_NAME = "cc-pV5Z"
                  case ("CC-PV5Z_OLD", "CC-PV5Z-OLD")
                        BASIS_SET_PATH = "cc-pv5z"
                        BASIS_SET_NAME = "cc-pV5Z (old)"
                  case ("AUG-CC-PV5Z", "AUG-CC-PV(5+D)Z")
                        BASIS_SET_PATH = "aug-cc-pv5dz"
                        BASIS_SET_NAME = "aug-cc-pV5Z"
                  case ("AUG-CC-PV5Z_OLD", "AUG-CC-PV5Z-OLD")
                        BASIS_SET_PATH = "aug-cc-pv5z"
                        BASIS_SET_NAME = "aug-cc-pV5Z (old)"
                  case ("D-AUG-CC-PV5Z")
                        BASIS_SET_PATH = "d-aug-cc-pv5z"
                        BASIS_SET_NAME = "d-aug-cc-pV5Z"
                  case ("CC-PCVTZ")
                        BASIS_SET_PATH = "cc-pcvtz"
                        BASIS_SET_NAME = "cc-pCVTZ"
                  case ("AUG-CC-PCVTZ")
                        BASIS_SET_PATH = "aug-cc-pcvtz"
                        BASIS_SET_NAME = "aug-cc-pCVTZ"
                  case ("CC-PCVQZ")
                        BASIS_SET_PATH = "cc-pcvqz"
                        BASIS_SET_NAME = "cc-pCVQZ"
                  case ("AUG-CC-PCVQZ")
                        BASIS_SET_PATH = "aug-cc-pcvqz"
                        BASIS_SET_NAME = "aug-cc-pCVQZ"
                        ! ----------------------------------------------
                        !               (aug-)cc-pwCXZ
                        ! ----------------------------------------------
                  case ("CC-PWCVQZ")
                        BASIS_SET_PATH = "cc-pwcvqz"
                        BASIS_SET_NAME = "cc-pwCVQZ"
                  case ("AUG-CC-PWCVQZ")
                        BASIS_SET_PATH = "aug-cc-pwcvqz"
                        BASIS_SET_NAME = "aug-cc-pwCVQZ"
                  case ("CC-PWCV5Z")
                        BASIS_SET_PATH = "cc-pwcv5z"
                        BASIS_SET_NAME = "cc-pwCV5Z"
                  case ("AUG-CC-PWCV5Z")
                        BASIS_SET_PATH = "aug-cc-pwcv5z"
                        BASIS_SET_NAME = "aug-cc-pwCV5Z"
                  case ("DEF2-QZVP")
                        BASIS_SET_PATH = "def2-qzvp"
                        BASIS_SET_NAME = "Def2-QZVP"
                  case ("DEF2-QZVPD")
                        BASIS_SET_PATH = "def2-qzvpd"
                        BASIS_SET_NAME = "Def2-QZVPD"
                  case ("DEF2-QZVPP")
                        BASIS_SET_PATH = "def2-qzvpp"
                        BASIS_SET_NAME = "Def2-QZVPP"
                  case ("DEF2-QZVPPD")
                        BASIS_SET_PATH = "def2-qzvppd"
                        BASIS_SET_NAME = "Def2-QZVPPD"
                  case ("DEF2-SV(P)")
                        BASIS_SET_PATH = "def2-sv_p"
                        BASIS_SET_NAME = "Def2-SV(P)"
                  case ("DEF2-SVP")
                        BASIS_SET_PATH = "def2-svp"
                        BASIS_SET_NAME = "Def2-SVP"
                  case ("DEF2-SVPD")
                        BASIS_SET_PATH = "def2-svpd"
                        BASIS_SET_NAME = "Def2-SVPD"
                  case ("DEF2-TZVP")
                        BASIS_SET_PATH = "def2-tzvp"
                        BASIS_SET_NAME = "Def2-TZVP"
                  case ("DEF2-TZVPD")
                        BASIS_SET_PATH = "def2-tzvpd"
                        BASIS_SET_NAME = "Def2-TZVPD"
                  case ("DEF2-TZVPP")
                        BASIS_SET_PATH = "def2-tzvpp"
                        BASIS_SET_NAME = "Def2-TZVPP"
                  case ("DEF2-TZVPPD")
                        BASIS_SET_PATH = "def2-tzvppd"
                        BASIS_SET_NAME = "Def2-TZVPPD"
                  case ("SADLEJ-PVTZ")
                        BASIS_SET_PATH = "sadlej-pvtz"
                        BASIS_SET_NAME = "Sadlej-pVTZ"
                  case ("SAPPORO-QZP-2012")
                        BASIS_SET_PATH = "sapporo-qzp-2012"
                        BASIS_SET_NAME = "Sapporo-QZP-2012"
                  case ("CRENBL")
                        BASIS_SET_PATH = "crenbl"
                        BASIS_SET_NAME = "CRENBL"
                  case default
                        call msg("ERROR: UNKNOWN BASIS SET", MSG_ERROR)
                        call smsg("REQUESTED BASIS SET:", a, MSG_ERROR)
                        call msg("USE A NAME DEFINED IN THE EMSL REPOSITORY", MSG_ERROR)
                        call msg("OR SPECIFY A TEXT FILE IN THE BASIS SET DIRECTORY:", MSG_ERROR)
                        call msg(trim(BASISDIR))
                        stop
                  end select
                  ATOMIC_GUESS_DIR = GUESSDIR // "electron-densities" // DIRSEP // "rohf" &
                        // DIRSEP // trim(BASIS_SET_PATH) // DIRSEP
                  if (.not. CustomFile) then
                        BASIS_SET_PATH = BASISDIR // BASIS_SET_PATH // ".txt"
                  end if
            end if
      end subroutine read_basis_set


      subroutine read_AOBasisPath(line, SCFParams)
            character(*), intent(in) :: line
            type(TSCFParams), intent(inout) :: SCFParams
            
            character(:), allocatable :: keyword
            character(:), allocatable :: a, a1, a2
            character(:), allocatable :: FilePath, BasisName
            
            call split(line, keyword, a)
            call split(a, a1, a2)

            if (uppercase(a2) .ne. "") then
                  if (uppercase(a1) == "FILE") then
                        if (io_exists(a2)) then
                              FilePath = a2
                              BasisName = a2
                        else
                              call msg("Basis set coefficients file is inaccessible", MSG_ERROR)
                              error stop
                        end if
                  else
                        call msg("Invalid keyword: " // a1, MSG_ERROR)
                        error stop
                  end if
                  SCFParams%AOBasisPath = FilePath
                  SCFParams%AOBasisName = BasisName
            else
                  !
                  ! Standard basis set requested by its EMSL Basis Set Exchange name
                  !
                  select case(uppercase(a))
                  case ("AHLRICHS-VDZ")
                        FilePath = "ahlrichs-vdz"
                        BasisName = "Ahlrichs VDZ"
                  case ("3-21G")
                        FilePath = "3-21g"
                        BasisName = "3-21G"
                  case ("3-21++G")
                        FilePath = "3-21g_diff_all"
                        BasisName = "3-21++G"
                  case ("6-31G")
                        FilePath = "6-31g"
                        BasisName = "6-31G"
                  case ("6-31G**")
                        FilePath = "6-31g_pol_all"
                        BasisName = "6-31G**"
                  case ("6-31++G")
                        FilePath = "6-31g_diff_all"
                        BasisName = "6-31++G"
                  case ("6-31+G**")
                        FilePath = "6-31g_diff_pol_all"
                        BasisName = "6-31+G**"
                  case ("6-31++G**")
                        FilePath = "6-31g_diff_all_pol_all"
                        BasisName = "6-31++G**"
                  case ("6-31G(3DF,3PD)", "6-31G(3DF, 3PD)")
                        FilePath = "6-31g_3df_3pd"
                        BasisName = "6-31G(3df,3dp)"
                  case ("6-311G")
                        FilePath = "6-311g"
                        BasisName = "6-311G"
                  case ("6-311G**")
                        FilePath = "6-311g_pol_all"
                        BasisName = "6-311G**"
                  case ("6-311+G*")
                        FilePath = "6-311g_diff_pol"
                        BasisName = "6-311+G*"
                  case ("6-311++G**")
                        FilePath = "6-311g_diff_all_pol_all"
                        BasisName = "6-311++G**"
                  case ("6-311++G(2D,2P)", "6-311++G(2D, 2P)")
                        FilePath = "6-311g_diff_all_2d_2p"
                        BasisName = "6-311++G(2d,2p)"
                  case ("6-311++G(3DF,3PD)", "6-311++G(3DF, 3PD)")
                        FilePath = "6-311g_diff_all_3df_3pd"
                        BasisName = "6-311++G(3df,3pd)"
                  case ("6-311(3+,3+)G**", "6-311(3+, 3+)G**", "6-311(3+,3+)G(d,p)", &
                        "6-311(3+, 3+)G(d, p)")
                        FilePath = "6-311g_triple_diff_all_pol_all"
                        BasisName = "6-311(3+,3+)G**"
                  case ("CC-PVDZ", "CC-PV(D+D)Z")
                        FilePath = "cc-pvddz"
                        BasisName = "cc-pVDZ"
                  case ("CC-PVDZ_OLD", "CC-PVDZ-OLD")
                        FilePath = "cc-pvdz"
                        BasisName = "cc-pVDZ (old)"
                  case ("CC-PVDZ-PP")
                        FilePath = "cc-pvdz-pp"
                        BasisName = "cc-pVDZ-PP"
                  case ("AUG-CC-PVDZ", "AUG-CC-PV(D+D)Z")
                        FilePath = "aug-cc-pvddz"
                        BasisName = "aug-cc-pVDZ"
                  case ("AUG-CC-PVDZ_OLD", "AUG-CC-PVDZ-OLD")
                        FilePath = "aug-cc-pvdz"
                        BasisName = "aug-cc-pVDZ (old)"
                  case ("AUG-CC-PVDZ-PP")
                        FilePath = "aug-cc-pvdz-pp"
                        BasisName = "aug-cc-pVDZ-PP"
                  case ("D-AUG-CC-PVDZ")
                        FilePath = "d-aug-cc-pvdz"
                        BasisName = "d-aug-cc-pVDZ"
                  case ("CC-PVTZ", "CC-PV(T+D)Z")
                        FilePath = "cc-pvtdz"
                        BasisName = "cc-pVTZ"
                  case ("CC-PVTZ_OLD", "CC-PVTZ-OLD")
                        FilePath = "cc-pvtz"
                        BasisName = "cc-pVTZ (old)"
                  case ("CC-PVTZ-PP")
                        FilePath = "cc-pvtz-pp"
                        BasisName = "cc-pVTZ-PP"
                  case ("AUG-CC-PVTZ", "AUG-CC-PV(T+D)Z")
                        FilePath = "aug-cc-pvtdz"
                        BasisName = "aug-cc-pVTZ"
                  case ("AUG-CC-PVTZ_OLD", "AUG-CC-PVTZ-OLD")
                        FilePath = "aug-cc-pvtz"
                        BasisName = "aug-cc-pVTZ (old)"
                  case ("AUG-CC-PVTZ-PP")
                        FilePath = "aug-cc-pvtz-pp"
                        BasisName = "aug-cc-pVTZ-PP"
                  case ("D-AUG-CC-PVTZ")
                        FilePath = "d-aug-cc-pvtz"
                        BasisName = "d-aug-cc-pVTZ"
                  case ("CC-PVQZ", "CC-PV(Q+D)Z")
                        FilePath = "cc-pvqdz"
                        BasisName = "cc-pVQZ"
                  case ("CC-PVQZ_OLD", "CC-PVQZ-OLD")
                        FilePath = "cc-pvqz"
                        BasisName = "cc-pVQZ (old)"
                  case ("AUG-CC-PVQZ", "AUG-CC-PV(Q+D)Z")
                        FilePath = "aug-cc-pvqdz"
                        BasisName = "aug-cc-pVQZ"
                  case ("AUG-CC-PVQZ_OLD", "AUG-CC-PVQZ-OLD")
                        FilePath = "aug-cc-pvqz"
                        BasisName = "aug-cc-pVQZ (old)"
                  case ("D-AUG-CC-PVQZ")
                        FilePath = "d-aug-cc-pvqz"
                        BasisName = "d-aug-cc-pVQZ"
                  case ("CC-PV5Z", "CC-PV(5+D)Z")
                        FilePath = "cc-pv5dz"
                        BasisName = "cc-pV5Z"
                  case ("CC-PV5Z_OLD", "CC-PV5Z-OLD")
                        FilePath = "cc-pv5z"
                        BasisName = "cc-pV5Z (old)"
                  case ("AUG-CC-PV5Z", "AUG-CC-PV(5+D)Z")
                        FilePath = "aug-cc-pv5dz"
                        BasisName = "aug-cc-pV5Z"
                  case ("AUG-CC-PV5Z_OLD", "AUG-CC-PV5Z-OLD")
                        FilePath = "aug-cc-pv5z"
                        BasisName = "aug-cc-pV5Z (old)"
                  case ("D-AUG-CC-PV5Z")
                        FilePath = "d-aug-cc-pv5z"
                        BasisName = "d-aug-cc-pV5Z"
                  case ("CC-PCVTZ")
                        FilePath = "cc-pcvtz"
                        BasisName = "cc-pCVTZ"
                  case ("AUG-CC-PCVTZ")
                        FilePath = "aug-cc-pcvtz"
                        BasisName = "aug-cc-pCVTZ"
                  case ("CC-PCVQZ")
                        FilePath = "cc-pcvqz"
                        BasisName = "cc-pCVQZ"
                  case ("AUG-CC-PCVQZ")
                        FilePath = "aug-cc-pcvqz"
                        BasisName = "aug-cc-pCVQZ"
                        ! ----------------------------------------------
                        !               (aug-)cc-pwCXZ
                        ! ----------------------------------------------
                  case ("CC-PWCVQZ")
                        FilePath = "cc-pwcvqz"
                        BasisName = "cc-pwCVQZ"
                  case ("AUG-CC-PWCVQZ")
                        FilePath = "aug-cc-pwcvqz"
                        BasisName = "aug-cc-pwCVQZ"
                  case ("CC-PWCV5Z")
                        FilePath = "cc-pwcv5z"
                        BasisName = "cc-pwCV5Z"
                  case ("AUG-CC-PWCV5Z")
                        FilePath = "aug-cc-pwcv5z"
                        BasisName = "aug-cc-pwCV5Z"
                  case ("DEF2-QZVP")
                        FilePath = "def2-qzvp"
                        BasisName = "Def2-QZVP"
                  case ("DEF2-QZVPD")
                        FilePath = "def2-qzvpd"
                        BasisName = "Def2-QZVPD"
                  case ("DEF2-QZVPP")
                        FilePath = "def2-qzvpp"
                        BasisName = "Def2-QZVPP"
                  case ("DEF2-QZVPPD")
                        FilePath = "def2-qzvppd"
                        BasisName = "Def2-QZVPPD"
                  case ("DEF2-SV(P)")
                        FilePath = "def2-sv_p"
                        BasisName = "Def2-SV(P)"
                  case ("DEF2-SVP")
                        FilePath = "def2-svp"
                        BasisName = "Def2-SVP"
                  case ("DEF2-SVPD")
                        FilePath = "def2-svpd"
                        BasisName = "Def2-SVPD"
                  case ("DEF2-TZVP")
                        FilePath = "def2-tzvp"
                        BasisName = "Def2-TZVP"
                  case ("DEF2-TZVPD")
                        FilePath = "def2-tzvpd"
                        BasisName = "Def2-TZVPD"
                  case ("DEF2-TZVPP")
                        FilePath = "def2-tzvpp"
                        BasisName = "Def2-TZVPP"
                  case ("DEF2-TZVPPD")
                        FilePath = "def2-tzvppd"
                        BasisName = "Def2-TZVPPD"
                  case ("SADLEJ-PVTZ")
                        FilePath = "sadlej-pvtz"
                        BasisName = "Sadlej-pVTZ"
                  case ("SAPPORO-QZP-2012")
                        FilePath = "sapporo-qzp-2012"
                        BasisName = "Sapporo-QZP-2012"
                  case ("CRENBL")
                        FilePath = "crenbl"
                        BasisName = "CRENBL"
                  case default
                        call msg("Unknown basis set", MSG_ERROR)
                        error stop
                  end select
                  SCFParams%AtomicGuessDir = GUESSDIR // "electron-densities" // DIRSEP // "rohf" &
                        // DIRSEP // trim(FilePath) // DIRSEP
                  SCFParams%AOBasisPath = BASISDIR // FilePath // ".txt"
                  SCFParams%AOBasisName = BasisName
            end if
      end subroutine read_AOBasisPath


      subroutine read_F12BasisPath(line, SCFParams)
            character(*), intent(in) :: line
            type(TSCFParams), intent(inout) :: SCFParams
            
            character(:), allocatable :: keyword
            character(:), allocatable :: a, a1, a2
            character(:), allocatable :: FilePath, BasisName
            
            call split(line, keyword, a)
            call split(a, a1, a2)

            if (uppercase(a2) .ne. "") then
                  if (uppercase(a1) == "FILE") then
                        if (io_exists(a2)) then
                              FilePath = a2
                              BasisName = a2
                        else
                              call msg("Basis set coefficients file is inaccessible", MSG_ERROR)
                              error stop
                        end if
                  else
                        call msg("Invalid keyword: " // a1, MSG_ERROR)
                        error stop
                  end if
                  SCFParams%F12BasisPath = BASISDIR // FilePath // ".txt"
                  SCFParams%F12BasisName = BasisName
            else
                  select case(uppercase(a))
                  case ("DEBUG")
                        FilePath = "f12" // DIRSEP // "debug"
                        BasisName = "DEBUG"
                  case ("CC-PVDZ-F12")
                        FilePath = "f12" // DIRSEP // "cc-pvdz-f12"
                        BasisName = "cc-pVDZ-F12"
                  case ("CC-PVTZ-F12")
                        FilePath = "f12" // DIRSEP // "cc-pvtz-f12"
                        BasisName = "cc-pVTZ-F12"
                  case ("CC-PVQZ-F12")
                        FilePath = "f12" // DIRSEP // "cc-pvqz-f12"
                        BasisName = "cc-pVQZ-F12"
                  end select
                  SCFParams%F12BasisPath = BASISDIR // FilePath // ".txt"
                  SCFParams%F12BasisName = BasisName
            end if
      end subroutine read_F12BasisPath
      
      
      subroutine read_jobtype(line)
            character(*), intent(in) :: line
            character(:), allocatable :: aup, bup, a, b, ab, keyword

            call split(line, keyword, ab)
            call split(ab, a, b)
            aup = uppercase(a)
            bup = uppercase(b)
            select case (aup)
                  case ("VISUALIZE")
                        select case (bup)
                              case ("RHO_DIFF")
                                    JOBTYPE = JOB_VIS_RHO_DIFF
                              case default
                                    call msg("ERROR: UNKNOWN JOB TYPE SPECIFIED", &
                                          priority=MSG_ERROR)
                                    stop 
                        end select
                  case ("DFT_DISP", "DFT-DISP")
                        if (bup == "OPTIM") then
                              JOBTYPE = JOB_DFT_DISP_OPTIM
                        else
                              call msg("INVALID JOB TYPE SPECIFIED", &
                                    priority=MSG_ERROR)
                              stop 
                        end if

                  case ("DFT-D3", "DFTD3")
                        select case (bup)
                              case ("SP", "SINGLEPOINT")
                                    JOBTYPE = JOB_DFTD3_SP
                              case ("INT", "INTERACTION")
                                    JOBTYPE = JOB_DFTD3_INT
                              case default
                                    call msg("INVALID JOB TYPE SPECIFIED", &
                                          priority=MSG_ERROR)
                                    stop 
                        end select
                  case ("RTTDDFT")
                        select case (bup)
                        case ("POLAR")
                              JOBTYPE = JOB_RTTDDFT_POLAR
                        case default
                              call msg("ERROR: UNKNOWN JOB TYPE SPECIFIED", &
                                    priority=MSG_ERROR)
                              stop
                        end select
                  case ("HF", "RHF", "DFT", "KS", "RKS")
                        select case (bup)
                        case ("SP", "SINGLEPOINT")
                              JOBTYPE = JOB_DFT_SP
                        case ("INT", "INTERACTION")
                              JOBTYPE = JOB_DFT_INT
                        case ("FROKS", "FRAGMENT-ROKS")
                              JOBTYPE = JOB_DFT_FROKS
                        case ("OPTOMEGA")
                              JOBTYPE = JOB_DFT_OPTOMEGA
                        case ("OPT_MODRZEJ2015")
                              JOBTYPE = JOB_DFT_OPT_MODRZEJ2015
                        case default
                              call msg("Cannot select job type. Unknown keyword specified.", priority=MSG_ERROR)
                              stop
                        end select

                  case ("UKS", "UHF")
                        select case (bup)
                        case ("SP", "SINGLEPOINT")
                              JOBTYPE = JOB_REAL_UKS_SP
                        case ("INT", "INTERACTION")
                              JOBTYPE = JOB_REAL_UKS_INT
                        case ("RPA")
                              JOBTYPE = JOB_REAL_UKS_RPA
                        case default
                              call msg("Cannot select job type. Unknown keyword specified.", priority=MSG_ERROR)
                              stop
                              
                        end select
                        
                  case ("CMPLX_RKS")
                        select case (bup)
                        case ("SP", "SINGLEPOINT")
                              JOBTYPE = JOB_CMPLX_RKS_SP
                        case default
                              call msg("Cannot select job type. Unknown keyword specified.", priority=MSG_ERROR)
                              stop
                        end select
                        
                  case ("MP2")
                        select case(bup)
                              case ("SP", "SINGLEPOINT")
                                    JOBTYPE = JOB_MP2_SP
                              case default
                                    call msg("INVALID JOB TYPE SPECIFIED", &
                                          priority=MSG_ERROR)
                                    stop
                        end select
                  case ("CCSD")
                        select case(bup)
                        case ("PROP", "PROPERTIES")
                              JOBTYPE = JOB_CCSD_PROP
                        case ("DENSITY", "DENS")
                              JOBTYPE = JOB_CCSD_DENSITY
                        case default
                              call msg("INVALID JOB TYPE SPECIFIED", &
                                    priority=MSG_ERROR)
                              stop
                        end select
                  case ("CC3")
                        select case(bup)
                        case ("PROP", "PROPERTIES")
                              JOBTYPE = JOB_CC3_PROP
                        case ("DENSITY", "DENS")
                              JOBTYPE = JOB_CC3_DENSITY
                        case default
                              call msg("INVALID JOB TYPE SPECIFIED", &
                                    priority=MSG_ERROR)
                              stop
                        end select
                  case default
                        call msg("INVALID METHOD SPECIFIED", &
                              priority=MSG_ERROR)
                        stop
            end select
      end subroutine read_jobtype


      subroutine read_xcmodel(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            XCMODEL = xcf_str2id(uppercase(a))
      end subroutine read_xcmodel


      subroutine read_auxint_name(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a
            integer :: iaux

            call split(line, keyword, a)
            call aux_str2id(iaux, uppercase(a))
            if (DFT_DISP == DISP_MBD_RSSCS) then
                  call msg("NO ADDITIONAL AUXILIARY INTEGRALS CAN BE SPECIFIED", MSG_ERROR)
                  call msg("AUX_HIRSHFELD_VOLUME REQUIRED BY THE MBD DISPERSION", MSG_ERROR)
                  stop
            end if

            if (iaux == AUX_HIRSHFELD_POPULATION .or. &
                  iaux == AUX_HIRSHFELD_VOLUME) then
                  HIRSH = .true.
            end if
            AUXINTEGRAL = iaux
      end subroutine read_auxint_name


      subroutine read_units(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            select case (uppercase(a))
            case ("ANGS", "ANGSTROM")
                  UNITS = UNITS_ANGS
            case ("BOHR")
                  UNITS = UNITS_BOHR
            case default
                  call msg("PARSER ERROR: UNKNOWN UNITS SPECIFIED", &
                        priority=MSG_ERROR)
            end select
      end subroutine read_units


      subroutine read_geomfile(u, line, linenumber)
            integer, intent(inout)    :: u
            character(*), intent(in)  :: line
            integer, intent(inout)    :: linenumber

            character(:), allocatable :: keyword, a, val1, val2
            character(:), allocatable :: xyz_path
            type(tmolecule) :: xyz, xyz_a, xyz_b
            type(tmolecule) :: ion
            type(tsystemdep_params) :: par
            integer :: id
            logical :: inline_xyz
            logical :: generate_ions
            logical :: generate_dimer

            if (JOBTYPE .eq. JOB_UNKNOWN) then
                  call msg("PARSER ERROR: JOB TYPE MUST BE SET BEFORE LOADING GEOMETRY")
                  stop
            end if
            
            generate_dimer = .false.
            generate_ions = .false.
            inline_xyz = .false.

            call split(line, keyword, a)
            call split(a, val1, val2)
            
            select case (JOBTYPE)
            case (JOB_DFT_INT, JOB_REAL_UKS_INT, JOB_DFTD3_INT, JOB_DFT_DISP_OPTIM, JOB_DFT_FROKS)
                  if (isblank(a)) then
                        !
                        ! Single geometry for A, B, and AB.
                        ! The coordinates are defined in the input file.
                        !
                        id = GEOM_MONOMER
                        inline_xyz = .true.
                        generate_dimer = .true.
                  else if (isblank(val2)) then
                        id = GEOM_MONOMER
                        select case (uppercase(val1))
                        case ("MONOMER")
                              !
                              ! Separate geometries for A, B, and AB.
                              ! The coordinates are defined in the input file.
                              !                             
                              inline_xyz = .true.
                              id = GEOM_MONOMER
                        case ("COMPLEX")
                              !
                              ! Separate geometries for A, B, and AB.
                              ! The coordinates are defined in the input file.
                              !                             
                              inline_xyz = .true.
                              id = GEOM_COMPLEX
                        case default
                              !
                              ! Single geometry for A, B, and AB.
                              ! The coordinates are defined in a separate file.
                              !
                              xyz_path = val1
                              generate_dimer = .true.
                              id = GEOM_MONOMER
                        end select
                  else
                        !
                        ! Separate geometries for A, B, and AB.
                        ! The coordinates are defined in a separate file.
                        !
                        id = GEOM_MONOMER
                        select case (uppercase(val1))
                        case ("MONOMER")
                              id = GEOM_MONOMER
                              xyz_path = val2
                        case ("COMPLEX")
                              id = GEOM_COMPLEX
                              xyz_path = val2
                        case default
                              call msg("INVALID DEFINITION OF MONOMER/COMPLEX COORDINATES", &
                                    priority=MSG_ERROR)
                              stop
                        end select
                  end if
            case (JOB_DFT_OPTOMEGA)
                  id = GEOM_NEUTRAL
                  generate_ions = .true.
                  if (isblank(a)) then
                        inline_xyz = .true.
                  else
                        xyz_path = a
                  end if
                  
            case default
                  id = GEOM_MONOMER
                  if (isblank(a)) then
                        inline_xyz = .true.
                  else
                        xyz_path = a
                  end if
            end select

            call pack_systemdep_params(par, .true.)

            if (generate_dimer) then
                  if (inline_xyz) then
                        !
                        ! Close input file. It will be re-opened inside NEWMOLECULE
                        ! subroutine. Fortran standard does not allow the same file
                        ! to be connected simultaneously to more than one unit.
                        !
                        close(u)
                        call newdimer(xyz_a, xyz_b, xyz, INPUTFILE, linenumber+1, UNITS)
                        open(newunit=u, file=INPUTFILE, status="old", &
                              access="sequential", position="rewind")
                        call scroll(u, linenumber+1)
                        call skipblock(u, linenumber)
                        call enqueue_job(xyz_a, par, GEOM_MONOMER)
                        call enqueue_job(xyz_b, par, GEOM_MONOMER)
                        call enqueue_job(xyz, par, GEOM_COMPLEX)
                  else
                        call newdimer(xyz_a, xyz_b, xyz, xyz_path, 1, UNITS)
                        call enqueue_job(xyz_a, par, GEOM_MONOMER)
                        call enqueue_job(xyz_b, par, GEOM_MONOMER)
                        call enqueue_job(xyz, par, GEOM_COMPLEX)
                  end if
            else
                  if (inline_xyz) then
                        !
                        ! Close input file. It will be re-opened inside NEWMOLECULE
                        ! subroutine. Fortran standard does not allow the same file
                        ! to be connected simultaneously to more than one unit.
                        !
                        close(u)
                        call newmolecule(xyz, INPUTFILE, linenumber+1, UNITS)
                        open(newunit=u, file=INPUTFILE, status="old", &
                              access="sequential", position="rewind")
                        call scroll(u, linenumber+1)
                        call enqueue_job(xyz, par, id)
                        call skipblock(u, linenumber)
                  else
                        call newmolecule(xyz, xyz_path, 1, UNITS)
                        call enqueue_job(xyz, par, id)
                  end if
            end if

            if (JOBTYPE == JOB_DFT_OPTOMEGA) then
                  if (generate_ions) then
                        select case (OPTOMEGA_J2TYPE)
                              case (OPTOMEGA_J2_CN)
                                    call gencation(ion, xyz)
                                    call enqueue_job(ion, par, GEOM_CATION)
                              case (OPTOMEGA_J2_NA)
                                    call genanion(ion, xyz)
                                    call enqueue_job(ion, par, GEOM_ANION)
                              case (OPTOMEGA_J2_CNA)
                                    call gencation(ion, xyz)
                                    call enqueue_job(ion, par, GEOM_CATION)
                                    call genanion(ion, xyz)
                                    call enqueue_job(ion, par, GEOM_ANION)
                        end select
                  end if
            end if
      end subroutine read_geomfile


      subroutine read_scf_guess(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, keyword2
            character(:), allocatable :: key2up
            character(:), allocatable :: s
            character(:), allocatable :: a

            call split(line, keyword, a)
            call split(a, keyword2, s)
            key2up = uppercase(keyword2)
            select case (key2up)
            case ("HBARE")
                  SCF_GUESS = SCF_GUESS_HBARE
            case ("ATOMIC")
                  SCF_GUESS = SCF_GUESS_ATOMIC
            case ("TEXT")
                  SCF_GUESS = SCF_GUESS_TEXT_FILE
                  SCF_GUESS_RHOA_PATH = s
                  SCF_GUESS_RHOB_PATH = ""
            case ("RHOA-TEXT")
                  SCF_GUESS = SCF_GUESS_TEXT_FILE
                  SCF_GUESS_RHOA_PATH = s
            case ("RHOB-TEXT")
                  SCF_GUESS = SCF_GUESS_TEXT_FILE
                  SCF_GUESS_RHOB_PATH = s
            case ("BINARY")
                  SCF_GUESS = SCF_GUESS_BINARY_FILE
                  SCF_GUESS_RHOA_PATH = s
                  SCF_GUESS_RHOB_PATH = ""
            case ("RHOA-BINARY")
                  SCF_GUESS = SCF_GUESS_BINARY_FILE
                  SCF_GUESS_RHOA_PATH = s
            case ("RHOB-BINARY")
                  SCF_GUESS = SCF_GUESS_BINARY_FILE
                  SCF_GUESS_RHOB_PATH = s
            case default
                  call msg("PARSER ERROR: UNKNOWN METHOD REQUESTED FOR SCF GUESS", &
                        priority=MSG_ERROR)
                  call smsg("SCFGUESS", key2up)
                  stop
            end select
      end subroutine read_scf_guess


      subroutine read_scf_saverho(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, s12, s1, s2

            call split(line, keyword, s12)
            call split(s12, s1, s2)
            !
            ! scf_saverho binary/text file_path
            !
            select case (uppercase(s1))
            case ("TEXT", "RHOA-TEXT")
                  SCF_SAVE_RHO_MODE = FILEMODE_TEXT
                  SCF_SAVE_RHOA_PATH = s2
            case ("BINARY", "RHOA-BINARY")
                  SCF_SAVE_RHO_MODE = FILEMODE_BINARY
                  SCF_SAVE_RHOA_PATH = s2
            case ("RHOB-TEXT")
                  SCF_SAVE_RHO_MODE = FILEMODE_TEXT
                  SCF_SAVE_RHOB_PATH = s2
            case ("RHOB-BINARY")
                  SCF_SAVE_RHO_MODE = FILEMODE_BINARY
                  SCF_SAVE_RHOB_PATH = s2
            case default
                  call smsg("Unknown mode for writing a converged SCF density", s1, MSG_ERROR)
                  stop
            end select
      end subroutine read_scf_saverho


      subroutine read_cc_saverho(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, s12, s1, s2

            call split(line, keyword, s12)
            call split(s12, s1, s2)

            if (.not. isblank(s2)) then
                  !
                  ! cc_saverho binary/text file_path
                  !
                  select case (uppercase(s1))
                  case ("TEXT")
                        CC_SAVERHO_MODE = FILEMODE_TEXT
                  case ("BINARY")
                        CC_SAVERHO_MODE = FILEMODE_BINARY
                  case default
                        call smsg("UNKNOWN MODE FOR SAVING FILES", s1, MSG_ERROR)
                        stop
                  end select
                  
                  CC_SAVERHO = .true.
                  CC_SAVERHO_PATH = s2
            else
                  !
                  ! cc_saverho file_path
                  !
                  CC_SAVERHO = .true.
                  if (.not. isblank(s1)) then
                        CC_SAVERHO_PATH = s1
                  else
                        call msg("FILE PATH NOT PROVIDED FOR CC_SAVERHO", MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine read_cc_saverho


      subroutine read_report(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, repfile

            if (IMG_ISMASTER) then
                  call split(line, keyword, repfile)
                  call rep_setpath(repfile)
                  DOREPORT = .true.
            end if
      end subroutine read_report


      subroutine read_spherbasis(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            if (len(val) == 0) then
                  SPHERBASIS = .true.
            else
                  select case (uppercase(val))
                  case ("TRUE")
                        SPHERBASIS = .true.
                  case ("FALSE")
                        SPHERBASIS = .false.
                  case default
                        call msg("INVALID VALUE OF SPHERBASIS", MSG_ERROR)
                        stop
                  end select
            end if
      end subroutine read_spherbasis


      subroutine read_ecp_grad(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            select case(uppercase(a))
            case ("TRUE")
                  ECP_GRAD = .true.
            case ("FALSE")
                  ECP_GRAD = .false.
            case default
                  call msg("PARSER ERROR: WRONG VALUE OF ECP_GRAD", &
                        priority=MSG_ERROR)
                  stop
            end select
      end subroutine read_ecp_grad
      
      
      subroutine read_point_group(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a
            
            call split(line, keyword, a)
            select case(uppercase(a))
            case ("C2V")
                  POINT_GROUP = C2v
            case("D2H")
                  POINT_GROUP = D2H
            case default
                  call msg("NO SYMMETRY DEFINED")
            end select
                  
      end subroutine read_point_group


      subroutine read_atomic_mass(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            select case(uppercase(a))
            case ("HI")
                  ATOMIC_MASS = HI
            case("AV")
                  ATOMIC_MASS = AV
            case default
                  call msg("NO SYMMETRY DEFINED")
            end select

      end subroutine read_atomic_mass


      subroutine read_origin(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, s12, s1, s2, s3, s4
            
            call split(line, keyword, s12)
            call split(s12, s1, s2)
            if(s2 == "")then
                  select case(uppercase(s1))
                  case ("CHCENTER")
                        ORIG = CHCTR
                  case("MCENTER")
                        ORIG = MCTR
                  end select
            else
                  ORIG = POINT
                  call split(s2, s3, s4)
                  read(s1, '(F30.20)') origin(1)
                  read(s3, '(F30.20)') origin(2)
                  read(s4, '(F30.20)') origin(3)
            end if
      end subroutine read_origin

      subroutine read_onlyright(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  ONLYRIGHT = .true.
            case ("FALSE")
                  ONLYRIGHT = .false.
            case default
                  call msg("INVALID VALUE OF ONLYRIGHT", MSG_ERROR)
                  stop
            end select

      end subroutine read_onlyright

      subroutine read_prop_gr_exc(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  PROP_GR_EXC = .true.
            case ("FALSE")
                  PROP_GR_EXC = .false.
            case default
                    call msg("INVALID VALUE OF PROP_GR_EXC", MSG_ERROR)
                  stop
            end select

      end subroutine read_prop_gr_exc


      subroutine read_prop_exc_exc_dip(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  PROP_EXC_EXC_DIP = .true.
            case ("FALSE")
                  PROP_EXC_EXC_DIP = .false.
            case default
                  call msg("INVALID VALUE OF PROP_EXC_EXC_DIP", MSG_ERROR)
                  stop
            end select

      end subroutine read_prop_exc_exc_dip


      subroutine read_prop_exc_exc_so(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  PROP_EXC_EXC_SO = .true.
            case ("FALSE")
                  PROP_EXC_EXC_SO = .false.
            case default
                  call msg("INVALID VALUE OF PROP_EXC_EXC_SO", MSG_ERROR)
                  stop
            end select

      end subroutine read_prop_exc_exc_so

      
      subroutine read_slater_basis(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  SLATER_BASIS = .true.
            case ("FALSE")
                  SLATER_BASIS = .false.
            case default
                  call msg("INVALID VALUE OF SLATER_BASIS", MSG_ERROR)
                  stop
            end select

      end subroutine read_slater_basis

      
      subroutine read_scf_read(line)
            character(*), intent(in) :: line
            
            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  SCF_READ = .true.
            case ("FALSE")
                  SCF_READ = .false.
            case default
                  call msg("INVALID VALUE OF SCF READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_scf_read

      
      subroutine read_scf_write(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  SCF_WRITE = .true.
            case ("FALSE")
                  SCF_WRITE = .false.
            case default
                  call msg("INVALID VALUE OF SCF READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_scf_write


      subroutine read_ccsp_read(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  CCSP_READ = .true.
            case ("FALSE")
                  CCSP_READ = .false.
            case default
                  call msg("INVALID VALUE OF CCSD READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_ccsp_read

      
      subroutine read_ccsp_write(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  CCSP_WRITE = .true.
            case ("FALSE")
                  CCSP_WRITE = .false.
            case default
                  call msg("INVALID VALUE OF CCSD READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_ccsp_write

     subroutine read_eom_mem(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
                select case (uppercase(val))
            case ("TRUE")
                  EOM_MEM = .true.
            case ("FALSE")
                  EOM_MEM = .false.
            case default
                  call msg("INVALID VALUE OF EOM READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_eom_mem

      subroutine read_eom_read(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  EOM_READ = .true.
            case ("FALSE")
                  EOM_READ = .false.
            case default
                  call msg("INVALID VALUE OF EOM READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_eom_read


      subroutine read_eom_write(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            select case (uppercase(val))
            case ("TRUE")
                  EOM_WRITE = .true.
            case ("FALSE")
                  EOM_WRITE = .false.
            case default
                  call msg("INVALID VALUE OF EOM READ", MSG_ERROR)
                  stop
            end select

      end subroutine read_eom_write



      subroutine read_slater_scf(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val

            call split(line, keyword, val)            
            SLATER_FILE_SCF = ROOTDIR//'slater-basis'//DIRSEP//val
      end subroutine read_slater_scf
      

      subroutine read_slater_aux(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            SLATER_FILE_AUX = ROOTDIR//'slater-basis'//DIRSEP//val
      end subroutine read_slater_aux

      
      subroutine read_slater_1e(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            
            call split(line, keyword, val)
            SLATER_FILE_1e = ROOTDIR//'slater-basis'//DIRSEP//val
      end subroutine read_slater_1e

      
      subroutine read_slater_2e(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            SLATER_FILE_2E = ROOTDIR//'slater-basis'//DIRSEP//val
      end subroutine read_slater_2e
      
      
      subroutine read_cc_nosymmetry(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            select case (uppercase(a))
            case ("TRUE")
                  CC_NOSYMMETRY = .true.
            case ("FALSE")
                  CC_NOSYMMETRY = .false.
            case default
                  call msg("PARSER ERROR: WRONG VALUE OF CC_NOSYMMETRY", &
                        priority=MSG_ERROR)
                  stop
            end select
      end subroutine read_cc_nosymmetry


      subroutine read_cc_irrepexci(line)
            character(len=DEFLEN), intent(in) :: line

            CC_IEXCI = line
      end subroutine read_cc_irrepexci


      subroutine read_cc_irrepexci_d(line)
            character(len=DEFLEN), intent(in) :: line
            
            CC_IEXCI_D = line
      end subroutine read_cc_irrepexci_d

      
      subroutine read_cc_irrepexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_IEXCI_S = line
      end subroutine read_cc_irrepexci_s

      
      subroutine read_cc_lexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_LEXCI_S = line
      end subroutine read_cc_lexci_s

      
      subroutine read_cc_lexci_d(line)
            character(len=DEFLEN), intent(in) :: line

            CC_LEXCI_D = line
      end subroutine read_cc_lexci_d

      
      subroutine read_cc_lexci_sd(line)
            character(len=DEFLEN), intent(in) :: line

            CC_LEXCI_SD = line
      end subroutine read_cc_lexci_sd


      subroutine read_cc_uexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_UEXCI_S = line
      end subroutine read_cc_uexci_s

      
     subroutine read_cc_uexci_d(line)
            character(len=DEFLEN), intent(in) :: line

            CC_UEXCI_D = line
      end subroutine read_cc_uexci_d

      
      subroutine read_cc_uexci_sd(line)
            character(len=DEFLEN), intent(in) :: line

            CC_UEXCI_SD = line
      end subroutine read_cc_uexci_sd

      subroutine read_cc_sing_lexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_SING_LEXCI_S = line
      end subroutine read_cc_sing_lexci_s

      subroutine read_cc_sing_lexci_d(line)
            character(len=DEFLEN), intent(in) :: line

            CC_SING_LEXCI_D = line
      end subroutine read_cc_sing_lexci_d

      subroutine read_cc_trip_lexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_TRIP_LEXCI_S = line
      end subroutine read_cc_trip_lexci_s

      subroutine read_cc_trip_lexci_d(line)
            character(len=DEFLEN), intent(in) :: line

            CC_TRIP_LEXCI_D = line
      end subroutine read_cc_trip_lexci_d

     subroutine read_cc_sing_uexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_SING_UEXCI_S = line
      end subroutine read_cc_sing_uexci_s

      subroutine read_cc_sing_uexci_d(line)
            character(len=DEFLEN), intent(in) :: line

            CC_SING_UEXCI_D = line
      end subroutine read_cc_sing_uexci_d

      subroutine read_cc_trip_uexci_s(line)
            character(len=DEFLEN), intent(in) :: line

            CC_TRIP_UEXCI_S = line
      end subroutine read_cc_trip_uexci_s

      subroutine read_cc_trip_uexci_d(line)
            character(len=DEFLEN), intent(in) :: line

            CC_TRIP_UEXCI_D = line
      end subroutine read_cc_trip_uexci_d






      subroutine read_cc_multip(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i
            integer, parameter :: one = 1
            integer, parameter :: three = 3
            integer, parameter :: eight = 8

            read(line, *) keyword, i
            if (i .ne. one) then
                  if (i .ne. three) then
                        if (i .ne. eight) then
                              if (i .ne. nine) then
                                    call msg("PARSER ERROR: STATES OF THIS MULTIPLICITY NOT AVAILABLE", &
                                          priority=MSG_ERROR)
                                    stop
                              end if
                        end if
                  end if
            end if

            CC_MULTIP = i
      end subroutine read_cc_multip

      
      subroutine read_multip(line)
            character(len=DEFLEN), intent(in) :: line
            character(len=DEFLEN) :: keyword
            character(len=DEFLEN) :: a
            
            read(line, *) keyword, a
            select case(uppercase(a))
            case ("1")
                  CC_MULTIP = cc_singlet
            case("TRIPLET")
                  CC_MULTIP = cc_triplet
            case default
                  call msg("SINGLET MULTIPLICITY")
            end select                  
      end subroutine read_multip

      
      subroutine read_dft_disp(SCFParams, line)
            type(TSCFParams), intent(inout) :: SCFParams
            character(*), intent(in)        :: line
            
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            select case (uppercase(a))
            case ("DFT-D3", "DFT_D3", "DFTD3", "D3")
                  DFT_DISP = DISP_DFTD3
            case ("DFT-D3(BJ)", "DFTD3(BJ)", "DFT_D3(BJ)", "D3(BJ)", "D3BJ")
                  DFT_DISP = DISP_DFTD3_BJ
            case ("MBD_RSSCS", "MBD-RSSCS", "MBD")
                  DFT_DISP = DISP_MBD_RSSCS
                  SCFParams%AUXInt_Type1 = AUX_HIRSHFELD_VOLUME
                  SCFParams%Hirsh = .true.
            case ("MULTIPOLERPA")
                  DFT_DISP = DISP_MULTIPOLE_RPA
            case default
                  call msg("UNKNOWN DISPERSION MODEL REQUESTED", MSG_ERROR)
                  stop
            end select
      end subroutine read_dft_disp


      subroutine read_dftd3_bj_a1(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            real(F64) :: a1
            
            call split(line, keyword, val)
            read(val, *) a1
            DFTD3_BJ_A1 = a1
      end subroutine read_dftd3_bj_a1


      subroutine read_dftd3_bj_a2(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            real(F64) :: a2
            
            call split(line, keyword, val)
            read(val, *) a2
            DFTD3_BJ_A2 = a2
      end subroutine read_dftd3_bj_a2


      subroutine read_dftd3_bj_s8(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            real(F64) :: s8
            
            call split(line, keyword, val)
            read(val, *) s8
            DFTD3_BJ_S8 = s8
      end subroutine read_dftd3_bj_s8

      
      subroutine read_dftd3_r6(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a
            real(F64) :: r6

            call split(line, keyword, a)
            read(a, *) r6
            if (r6 <= ZERO) then
                  call msg("R6 MUST BE POSITIVE", MSG_ERROR)
                  stop
            else
                  DFTD3_R6 = r6
            end if
      end subroutine read_dftd3_r6


      subroutine read_dftd3_s8(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a
            real(F64) :: s8

            call split(line, keyword, a)
            read(a, *) s8
            if (s8 < ZERO) then
                  call msg("S8 MUST BE NONNEGATIVE", MSG_ERROR)
                  stop
            else
                  DFTD3_S8 = s8
            end if
      end subroutine read_dftd3_s8


      subroutine read_dftd3_3body(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val

            call split(line, keyword, val)
            
            if (len(val) == 0) then
                  DFTD3_3BODY = .true.
            else
                  select case (uppercase(val))
                  case ("TRUE")
                        DFTD3_3BODY = .true.
                  case ("FALSE")
                        DFTD3_3BODY = .false.
                  case default
                        call msg("INVALID VALUE OF DFTD3_3BODY", MSG_ERROR)
                        stop
                  end select
            end if
      end subroutine read_dftd3_3body
      

      subroutine read_mbd_rsscs_beta(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a
            real(F64) :: beta

            call split(line, keyword, a)
            read(a, *) beta
            if (beta <= ZERO) then
                  call msg("BETA MUST BE POSITIVE", MSG_ERROR)
                  stop
            else
                  MBD_RSSCS_BETA = beta
            end if
      end subroutine read_mbd_rsscs_beta


      subroutine read_lcomega(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a
            real(F64) :: omega

            call split(line, keyword, a)
            read(a, *) omega
            if (omega <= ZERO) then
                  call msg("PARSER ERROR: LCOMEGA VALUE SHOULD BE GREATER THAN ZERO", &
                        priority=MSG_ERROR)
                  stop
            else
                  LCOMEGA = omega
            end if
      end subroutine read_lcomega


      subroutine read_rttddft_timestep(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            real(F64) :: a
            
            call split(line, keyword, val)
            read(val, *) a

            if (a < ZERO) then
                  call msg("INVALID VALUE OF TIMESTEP", MSG_ERROR)
                  stop
            else
                  RTTDDFT_TIMESTEP = a
            end if
      end subroutine read_rttddft_timestep


      subroutine read_rttddft_polar_field(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            real(F64) :: a

            call split(line, keyword, val)
            read(val, *) a

            if (a < ZERO) then
                  call msg("INVALID VALUE OF FIELD STRENGTH", MSG_ERROR)
                  stop
            else
                  RTTDDFT_POLAR_FIELD = a
            end if
      end subroutine read_rttddft_polar_field


      subroutine read_rttddft_polar_ncycles(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            integer :: i

            call split(line, keyword, val)
            read(val, *) i

            if (i < i) then
                  call msg("NUMBER OF CYCLES MUST BE GREATER THAN ONE", MSG_ERROR)
                  stop
            else
                  RTTDDFT_POLAR_NCYCLES = i
            end if
      end subroutine read_rttddft_polar_ncycles


      subroutine read_rttddft_polar_omega(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, val
            integer :: i

            call split(line, keyword, val)
            call readreal(RTTDDFT_POLAR_OMEGA, val)

            do i = 1, size(RTTDDFT_POLAR_OMEGA)
                  if (.not. RTTDDFT_POLAR_OMEGA(i) > ZERO) then
                        call msg("ANGULAR FREQUENCY MUST BE GREATER THAN ZERO", MSG_ERROR)
                        stop
                  end if
            end do
      end subroutine read_rttddft_polar_omega


      subroutine read_rttddft_polar_lambda(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, val
            integer :: i

            call split(line, keyword, val)
            call readreal(RTTDDFT_POLAR_OMEGA, val)

            do i = 1, size(RTTDDFT_POLAR_OMEGA)
                  if (.not. RTTDDFT_POLAR_OMEGA(i) > ZERO) then
                        call msg("WAVELENGTH MUST BE GREATER THAN ZERO", MSG_ERROR)
                        stop
                  end if
            end do

            do i = 1, size(RTTDDFT_POLAR_OMEGA)
                  !
                  ! Convert wavelengths in nanometers into angular frequencies (omega)
                  ! in atomic units
                  !
                  RTTDDFT_POLAR_OMEGA(i) = omega_to_lambda_nm(RTTDDFT_POLAR_OMEGA(i))
            end do
      end subroutine read_rttddft_polar_lambda


      subroutine read_optomega_j2type(line)
            character(*), intent(in) :: line
            character(:), allocatable :: keyword, a

            call split(line, keyword, a)
            select case(uppercase(a))
                  case ("CNA", "CATION-NEUTRAL-ANION", "CATION-ANION-NEUTRAL", "ANION-NEUTRAL-CATION", &
                        "NEUTRAL-CATION-ANION", "NEUTRAL-ANION-CATION", "ANION-CATION-NEUTRAL")
                        OPTOMEGA_J2TYPE = OPTOMEGA_J2_CNA
                  case ("CN", "CATION-NEUTRAL", "NEUTRAL-CATION")
                        OPTOMEGA_J2TYPE = OPTOMEGA_J2_CN
                  case ("NA", "NEUTRAL-ANION", "ANION-NEUTRAL")
                        OPTOMEGA_J2TYPE = OPTOMEGA_J2_NA
                  case default
                        call msg("INVALID VALUE OF J2TYPE", priority=MSG_ERROR)
                        stop
            end select
      end subroutine read_optomega_j2type


      subroutine read_optomega_min(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, a
            real(F64) :: omega_min

            call split(line, keyword, a)
            read(a, *) omega_min
            
            if (omega_min < ZERO) then
                  call msg("OPTOMEGA_MIN CANNOT BE NEGATIVE", &
                        MSG_ERROR)
                  stop
            else
                  OPTOMEGA_MIN = omega_min
            end if
      end subroutine read_optomega_min


      subroutine read_optomega_max(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, a
            real(F64) :: omega_max

            call split(line, keyword, a)
            read(a, *) omega_max
            
            if (omega_max < ZERO) then
                  call msg("OPTOMEGA_MAX CANNOT BE NEGATIVE", &
                        MSG_ERROR)
                  stop
            else
                  OPTOMEGA_MAX = omega_max
            end if
      end subroutine read_optomega_max


      subroutine read_lcsrexx(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if ((a .lt. ZERO) .or. (a .gt. ONE)) then
                  call msg("PARSER ERROR: LCSREXX VALUE SHOULD BE IN RANGE (0, 1)", &
                        priority=MSG_ERROR)
                  stop
            else
                  LCSREXX = a
            end if
      end subroutine read_lcsrexx


      subroutine read_gdd_nout(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a .le. zero) then
                  call msg("GDD_NOUT SHOULD BE GREATER THAN ZERO", &
                        priority=MSG_ERROR)
                  stop
            else
                  GDD_NOUT_ALPHA = a
                  GDD_NOUT_BETA = GDD_NOUT_ALPHA
            end if
      end subroutine read_gdd_nout


      subroutine read_gdd_mumin(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a .le. zero) then
                  call msg("GDD_MUMIN SHOULD BE GREATER THAN ZERO", &
                        priority=MSG_ERROR)
                  stop
            else
                  GDD_MUMIN = a
            end if
      end subroutine read_gdd_mumin


      subroutine read_mlrcs12_gparam(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a .le. zero) then
                  call msg("MLRCS12_GPARAM SHOULD BE GREATER THAN ZERO", &
                        priority=MSG_ERROR)
                  stop
            else
                  MLRCS12_GPARAM_USER = a
            end if
      end subroutine read_mlrcs12_gparam


      subroutine read_mcsv2_gopp(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a < ZERO) then
                  call msg("MCSv2_GOPP cannot be negative", MSG_ERROR)
                  stop
            else
                  MCSv2_GOPP = a
            end if
      end subroutine read_mcsv2_gopp


      subroutine read_mcsv2_gpar(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a < ZERO) then
                  call msg("MCSv2_GPAR cannot be negative", MSG_ERROR)
                  stop
            else
                  MCSv2_GPAR = a
            end if
      end subroutine read_mcsv2_gpar


      subroutine read_wpbesol_mlrcs_gparam(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a .le. zero) then
                  call msg("WPBESOL_MLRCS_GPARAM SHOULD BE GREATER THAN ZERO", &
                        priority=MSG_ERROR)
                  stop
            else
                  WPBESOL_MLRCS_GPARAM_USER = a
            end if
      end subroutine read_wpbesol_mlrcs_gparam


      subroutine read_wpbe_mlrcs_gparam(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a

            read(line, *) keyword, a
            if (a .le. zero) then
                  call msg("WPBE_MLRCS_GPARAM SHOULD BE GREATER THAN ZERO", &
                        priority=MSG_ERROR)
                  stop
            else
                  WPBE_MLRCS_GPARAM_USER = a
            end if
      end subroutine read_wpbe_mlrcs_gparam


      subroutine read_scf_thresh(line)
            character(*), intent(in) :: line

            character(:), allocatable :: s1, s2, s12, keyword
            real(F64) :: a

            call split(line, keyword, s12)
            call split(s12, s1, s2)
            read(s2, *) a

            if (a <= ZERO) then
                  call msg("SCF convergence threshold must be positive", MSG_ERROR)
                  stop
            end if

            if (uppercase(s1) == "DENSITY") then
                  SCF_THRESH_DENSITY = a
            else if (uppercase(s1) == "GRADIENT") then
                  SCF_THRESH_GRADIENT = a
            else
                  call msg("Invalid value of SCF convergence threshold", MSG_ERROR)
                  stop
            end if
      end subroutine read_scf_thresh

      
      subroutine read_lindep_thresh(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: a
            
            read(line, *) keyword, a
            if (a .lt. zero) then
                  call msg("PARSER ERROR: BAD VALUE OF LINDEP_THRESH THRESHOLD SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            LINDEP_THRESH = a
      end subroutine read_lindep_thresh


      subroutine read_misquitta_ct(line)
            character(*), intent(in) :: line
            
            character(DEFLEN) :: keyword
            real(F64) :: a

            read(line, *) keyword, a
            if (a <= ZERO) then
                  call msg("ETA value must be positive", MSG_ERROR)
                  stop
            else
                  MISQUITTA_CT = .true.
                  MISQUITTA_CT_ETA = a
            end if
      end subroutine read_misquitta_ct


      subroutine read_block_RhoSpher(line)
            character(*), intent(in) :: line
            character(:), allocatable :: key, val, val1, val2
            
            call split(line, key, val)
            select case (uppercase(key))
            case ("REFERENCE")
                  call split(val, val1, val2)
                  if (uppercase(val1) == "BINARY") then
                        RHO_SPHER_FILEMODE = FILEMODE_BINARY
                  else if (uppercase(val1) == "TEXT") then
                        RHO_SPHER_FILEMODE = FILEMODE_TEXT
                  else
                        call msg("Inavlid file type specified for DENSITY_COMPARE", MSG_ERROR)
                        stop
                  end if
                  RHO_SPHER_REF = VAL2
            case ("CSV")
                  RHO_SPHER_CSV = VAL
            case ("RSTART")
                  read(val, *) RHO_SPHER_R0
                  if (RHO_SPHER_R0 < ZERO) then
                        call msg("Invalid value of RStart", MSG_ERROR)
                        stop
                  end if
            case ("NPOINTS")
                  read(val, *) RHO_SPHER_N
                  if (RHO_SPHER_N < 1) then
                        call msg("Invalid value of NPoints", MSG_ERROR)
                        stop
                  end if
            case ("INTERVAL")
                  read(val, *) RHO_SPHER_DR
                  if (RHO_SPHER_DR <= ZERO) then
                        call msg("Invalid value of interval", MSG_ERROR)
                        stop
                  end if
            end select
      end subroutine read_block_RhoSpher


      subroutine read_block_RPA(RPAParams, SCFParams, Chol2Params, THCParams, line)
            type(TRPAParams), intent(inout)   :: RPAParams
            type(TSCFParams), intent(inout)   :: SCFParams
            type(TChol2Params), intent(inout) :: Chol2Params
            type(TTHCParams), intent(inout)   :: THCParams
            character(*), intent(in)          :: Line
            
            character(:), allocatable :: key, val
            real(F64) :: m
            integer :: i
            
            call split(line, key, val)
            select case (uppercase(key))
            case ("ALGORITHM")
                  select case (uppercase(val))
                  case ("AO")
                        RPAParams%MOAlgorithm = .false.
                  case ("MO")
                        RPAParams%MOAlgorithm = .true.
                  case default
                        call msg("Invalid value of Algorithm", MSG_ERROR)
                        error stop
                  end select
            case ("ACCURACY")
                  select case (uppercase(val))
                  case ("DEFAULT")
                        call rpa_Params_Default(RPAParams, SCFParams, Chol2Params)
                  case ("TIGHT")
                        call rpa_Params_Tight(RPAParams, SCFParams, Chol2Params)
                  case ("LUDICROUS")
                        call rpa_Params_Ludicrous(RPAParams, SCFParams, Chol2Params)
                  case default
                        call msg("Invalid RPA accuracy level", MSG_ERROR)
                        error stop
                  end select
            case ("VECTORS")
                  select case (uppercase(val))
                  case ("RANDOM")
                        RPAParams%RWRBasisType = RPA_BASIS_RANDOM
                  case ("EIGENVECTORS")
                        RPAParams%RWRBasisType = RPA_BASIS_EIGEN
                  case ("FULL-CHOLESKY", "FULLCHOLESKY")
                        RPAParams%RWRBasisType = RPA_BASIS_FULL_CHOLESKY
                  case default
                        call msg("Invalid value for RPA vectors", MSG_ERROR)
                        error stop
                  end select
            case ("MAXNAOMULT")
                  read(val, *) m
                  RPAParams%MaxNAOMult = m
            case ("RPA+MBPT3", "MBPT3", "RPA+MBPT-3")
                  RPAParams%TensorHypercontraction = .true.
                  RPAParams%CoupledClusters = .true.
                  RPAParams%TheoryLevel = RPA_THEORY_JCTC2023
            case ("RPT2")
                  RPAParams%TensorHypercontraction = .false.
                  RPAParams%CoupledClusters = .true.
                  RPAParams%T1Approx = RPA_T1_MEAN_FIELD
                  RPAParams%MeanField = RPA_MEAN_FIELD_HF_TYPE
                  RPAParams%ChiOrbitals = RPA_ORBITALS_CANONICAL
                  RPAParams%ExchangeApprox = RPA_EXCHANGE_SOSEX
                  RPAParams%Ec1RDMApprox = RPA_Ec1RDM_LINEAR
                  RPAParams%DensityApprox = RPA_RHO_T1_LINEAR
                  RPAParams%TheoryLevel = RPA_THEORY_RPT2
            case ("TENSORHYPERCONTRACTION", "THC", "TENSOR-HYPERCONTRACTION")
                  RPAParams%TensorHypercontraction = .true.
            case ("THC_QRTHRESH", "THCQRTHRESH")
                  read(val, *) m
                  RPAParams%THC_QRThresh = m
            case ("THC_PHISQUAREDTHRESH")
                  read(val, *) m
                  THCParams%PhiSquaredThresh = m
            case ("THC_BLOCKDIM")
                  read(val, *) i
                  RPAParams%THC_BlockDim = i
                  THCParams%THC_BlockDim = i
            case ("THC_BECKEGRIDKIND")
                  select case (uppercase(val))
                  case ("SG1", "SG-1")
                        RPAParams%THC_BeckeGridKind = BECKE_PARAMS_SG1
                        THCParams%THC_BeckeGridKind = BECKE_PARAMS_SG1
                  case ("MEDIUM")
                        RPAParams%THC_BeckeGridKind = BECKE_PARAMS_MEDIUM
                        THCParams%THC_BeckeGridKind = BECKE_PARAMS_MEDIUM
                  case ("FINE")
                        RPAParams%THC_BeckeGridKind = BECKE_PARAMS_FINE
                        THCParams%THC_BeckeGridKind = BECKE_PARAMS_FINE
                  case ("XFINE")
                        RPAParams%THC_BeckeGridKind = BECKE_PARAMS_XFINE
                        THCParams%THC_BeckeGridKind = BECKE_PARAMS_XFINE
                  case ("THC")
                        RPAParams%THC_BeckeGridKind = BECKE_PARAMS_THC
                        THCParams%THC_BeckeGridKind = BECKE_PARAMS_THC
                  case default
                        call msg("Unknown grid kind", priority=MSG_ERROR)
                        error stop
                  end select
            case ("T2AUXORBITALS", "T2-AUXILIARY-ORBITALS", "T2AUXILIARYORBITALS", &
                  "T2AUXBASIS", "T2-AUXILIARY-BASIS", "T2AUXILIARYBASIS")
                  
                  select case (uppercase(val))
                  case ("MOLECULAR-ORBITALS", "MOLECULARORBITALS", "MO")
                        RPAParams%T2AuxOrbitals = RPA_AUX_MOLECULAR_ORBITALS
                  case ("NATURAL-ORBITALS", "NATURALORBITALS", "NO")
                        RPAParams%T2AuxOrbitals = RPA_AUX_NATURAL_ORBITALS
                  case ("LOCALIZED-ORBITALS", "LOCALIZEDORBITALS", "LO")
                        RPAParams%T2AuxOrbitals = RPA_AUX_NATURAL_ORBITALS
                  case default
                        call msg("Invalid value of T2AuxOrbitals", MSG_ERROR)
                        error stop
                  end select
            case ("SVDALGO", "SVDALGORITHM", "SVD")
                  select case (uppercase(val))
                  case ("FULL")
                        RPAParams%SVDAlgorithm = RPA_SVD_FULL
                  case ("RANDOM", "RANDOMIZED")
                        RPAParams%SVDAlgorithm = RPA_SVD_RANDOMIZED
                  case default
                        call msg("Invalid value of SVDAlgorithm", MSG_ERROR)
                        error stop
                  end select
            case ("SVDOVERSAMPLING")
                  read(val, *) i
                  if (i > 0) then
                        RPAParams%SVDOversampling = i
                  else
                        call msg("Invalid value of SVDOversampling", MSG_ERROR)
                        error stop
                  end if
            case ("SVDNSUBSPACEITERS")
                  read(val, *) i
                  if (i > 0) then
                        RPAParams%SVDNSubspaceIters = i
                  else
                        call msg("Invalid value of SVDNSubspaceIters", MSG_ERROR)
                        error stop
                  end if
            case ("SVDNGUESSVECS")
                  read(val, *) i
                  if (i > 0) then
                        RPAParams%SVDNGuessVecs = i
                  else
                        call msg("Invalid value of SVDNGuessVecs", MSG_ERROR)
                        error stop
                  end if
            case ("SVDSWITCHOVERRATIO")
                  read(val, *) m
                  if (m >= ZERO) then
                        RPAParams%SVDSwitchoverRatio = m
                  else
                        call msg("Invalid value of SVDSwitchoverRatio", MSG_ERROR)
                        error stop
                  end if
            case ("T2AUXNOCUTOFFTHRESH")
                  read(val, *) m
                  if (m >= ZERO) then
                        RPAParams%T2AuxNOCutoffThresh = m
                  else
                        call msg("Invalid value of T2AuxNOCutoffThresh", MSG_ERROR)
                        error stop
                  end if
            case ("T2AUXLOCUTOFFTHRESH")
                  read(val, *) m
                  if (m >= ZERO) then
                        RPAParams%T2AuxLOCutoffThresh = m
                  else
                        call msg("Invalid value of T2AuxLOCutoffThresh", MSG_ERROR)
                        error stop
                  end if
            case ("LOCALIZEDORBITALS", "LOCALIZED-ORBITALS", "LOCALIZED_ORBITALS")
                  select case (uppercase(val))
                  case ("CHOLESKY")
                        RPAParams%LocalizedOrbitals = RPA_LOCALIZED_ORBITALS_CHOLESKY
                  case ("BOYS")
                        RPAParams%LocalizedOrbitals = RPA_LOCALIZED_ORBITALS_BOYS
                  case default
                        call msg("Invalid value of LocalizedOrbitals", MSG_ERROR)
                        error stop
                  end select
            case ("CUTOFFTHRESHVABIJ")
                  read(val, *) m
                  if (m >= ZERO) then
                        RPAParams%CutoffThreshVabij = m
                  else
                        call msg("Invalid value of CutoffThreshVabij", MSG_ERROR)
                        error stop
                  end if
            case ("CUTOFFTHRESHPNO", "TCUTPNO")
                  read(val, *) m
                  if (m >= ZERO) then
                        RPAParams%CutoffThreshPNO = m
                  else
                        call msg("Invalid value of CutoffThreshPNO", MSG_ERROR)
                        error stop
                  end if
            case ("ADIABATIC-CONNECTION", "ADIABATICCONNECTION", "ADIABATIC_CONNECTION", "COUPLEDCLUSTERS", "COUPLED-CLUSTERS")
                  RPAParams%CoupledClusters = .true.
            case ("ACQUADPOINTS")
                  read(val, *) i
                  RPAParams%ACQuadPoints = i
            case ("AC_1RDMQUAD")
                  select case (uppercase(val))
                  case ("ENABLE", "ENABLED", "TRUE", "")
                        RPAParams%AC_1RDMQuad = .true.
                  case ("DISABLE", "DISABLED", "FALSE")
                        RPAParams%AC_1RDMQuad = .false.
                  case default 
                        call msg("Invalid value of AC_1RDMQuad", MSG_ERROR)
                        error stop
                  end select
            case ("T1APPROX", "T1APPROXIMATION", "T1-APPROX", "T1-APPROXIMATION")
                  select case (uppercase(val))
                  case ("MEANFIELD", "MEAN-FIELD", "MEAN_FIELD")
                        RPAParams%T1Approx = RPA_T1_MEAN_FIELD
                  case ("DIRECT-RING-CCSD", "DRCCSD")
                        RPAParams%T1Approx = RPA_T1_DIRECT_RING_CCSD
                  case ("DIRECT-RING-CCSD-EXCHANGE", "DRCCSD+EXCHANGE")
                        RPAParams%T1Approx = RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE
                  case ("DRCCSD+EXCHANGE+LADDER")
                        RPAParams%T1Approx = RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE_PLUS_LADDER
                  case default
                        call msg("Invalid label of the T1 approximation", MSG_ERROR)
                        error stop
                  end select
            case ("CCD-CORRECTIONS", "CCDCORRECTIONS", "THEORY-LEVEL", "THEORYLEVEL")
                  RPAParams%TensorHypercontraction = .true.
                  RPAParams%CoupledClusters = .true.
                  select case (uppercase(val))
                  case ("RPA", "DIRECT-RING")
                        RPAParams%TheoryLevel = RPA_THEORY_DIRECT_RING
                  case ("RPT2")
                        RPAParams%TensorHypercontraction = .false.
                        RPAParams%CoupledClusters = .true.
                        RPAParams%T1Approx = RPA_T1_MEAN_FIELD
                        RPAParams%MeanField = RPA_MEAN_FIELD_HF_TYPE
                        RPAParams%ChiOrbitals = RPA_ORBITALS_CANONICAL
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_SOSEX
                        RPAParams%Ec1RDMApprox = RPA_Ec1RDM_LINEAR
                        RPAParams%DensityApprox = RPA_RHO_T1_LINEAR
                        RPAParams%TheoryLevel = RPA_THEORY_RPT2
                  case ("JCTC2023", "DEFAULT")
                        RPAParams%TheoryLevel = RPA_THEORY_JCTC2023
                  case ("JCTC2024")
                        RPAParams%TheoryLevel = RPA_THEORY_JCTC2024
                  case ("ALL")
                        RPAParams%TheoryLevel = RPA_THEORY_ALL
                  case default
                        call msg("Invalid value of TheoryLevel", MSG_ERROR)
                        error stop
                  end select
            case ("PT2", "PT_ORDER2", "PT-ORDER2", "PT-ORDER-2")
                  RPAParams%PT_Order2 = .true.
            case ("PT3", "PT_ORDER3", "PT-ORDER3", "PT-ORDER-3")
                  RPAParams%PT_Order3 = .true.
            case ("T2_EIGENVALUETHRESH", "T2_EIGENVALUE_THRESH", "T2-EIGENVALUE-THRESH", "T2EIGENVALUETHRESH", &
                  "T2_CUTOFFTHRESH", "T2_CUTOFF_THRESH", "T2-CUTOFF-THRESH", "T2CUTOFFTHRESH")
                  read(val, *) m
                  RPAParams%T2CutoffThresh = m
            case ("T2CUTOFFTYPE", "T2_CUTOFF_TYPE", "T2-CUTOFF-TYPE")
                  select case (uppercase(val))
                  case ("EIG")
                        RPAParams%T2CutoffType = RPA_T2_CUTOFF_EIG
                  case ("EIG/MAXEIG")
                        RPAParams%T2CutoffType = RPA_T2_CUTOFF_EIG_DIV_MAXEIG
                  case ("EIG/NELECTRON", "EIG/NOCC")
                        RPAParams%T2CutoffType = RPA_T2_CUTOFF_EIG_DIV_NELECTRON
                  case ("SUM-REJECTED", "SUM_REJECTED")
                        RPAParams%T2CutoffType = RPA_T2_CUTOFF_SUM_REJECTED
                  case default
                        call msg("Invalid value of T2CutoffThresh", MSG_ERROR)
                        error stop
                  end select
            case ("T2CUTOFFSMOOTHSTEP", "T2-CUTOFF-SMOOTH-STEP")
                  select case (uppercase(val))
                  case ("", "TRUE", "ENABLED")
                        RPAParams%T2CutoffSmoothStep = .true.
                  case ("FALSE", "DISABLED")
                        RPAParams%T2CutoffSmoothStep = .false.
                  case default
                        call msg("Invalid value of T2CutoffSmoothStep", MSG_ERROR)
                        error stop
                  end select
            case ("T2CUTOFFSTEEPNESS")
                  read(val, *) m
                  if (m >= ZERO) then 
                        RPAParams%T2CutoffSteepness = m
                  else
                        call msg("Invalid value of T2CutoffSteepness", MSG_ERROR)
                        error stop
                  end if
            case ("T2COUPLINGSTRENGTH")
                  read(val, *) m
                  RPAParams%T2CouplingStrength = m
            case ("MEANFIELDPARTITIONING")
                  select case (uppercase(val))
                  case ("KS-TYPE", "KS", "KOHN-SHAM", "LINEAR-SWITCHING")
                        RPAParams%MeanField = RPA_MEAN_FIELD_KS_TYPE
                  case ("HF-TYPE", "HF", "HARTREE-FOCK", "BARTLETT")
                        RPAParams%MeanField = RPA_MEAN_FIELD_HF_TYPE
                  case default
                        call msg("Invalid partitioning of the mean-field hamiltonian", MSG_ERROR)
                        error stop
                  end select
            case ("CHIORBITALS", "CHI-ORBITALS")
                  select case (uppercase(val))
                  case ("SEMI", "SEMICANONICAL")
                        RPAParams%ChiOrbitals = RPA_ORBITALS_SEMICANONICAL
                  case ("CANONICAL")
                        RPAParams%ChiOrbitals = RPA_ORBITALS_CANONICAL
                  case default
                        call msg("Invalid RPA orbitals type", MSG_ERROR)
                        error stop
                  end select
            case ("EXCHANGEAPPROX", "EXCHANGE-APPROX", "EXCHANGEAPPROXIMATION", "EXCHANGE-APPROXIMATION")
                  select case (uppercase(val))
                  case ("CUMULANT_LINEAR", "CUMULANT-LINEAR")
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_CUMULANT_LINEAR
                  case ("SOSEX")
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_SOSEX
                  case ("DTDLAMBDA", "MBPT3-1")
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_MBPT3_1
                  case ("MBPT3-1-NUMERICAL")
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_MBPT3_1_NUMERICAL
                  case ("MBPT3-2")
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_MBPT3_2
                  case ("NONE", "DISABLE")
                        RPAParams%ExchangeApprox = RPA_EXCHANGE_NONE
                  case default
                        call msg("Invalid label of the EcExchange approximation", MSG_ERROR)
                        error stop
                  end select
            case ("T2INTERP", "T2INTERPOLATION")
                  select case (uppercase(val))
                  case ("TRUE", "ENABLE", "")
                        RPAParams%T2Interp = .true.
                  case ("FALSE", "DISABLE")
                        RPAParams%T2Interp = .false.                        
                  case default
                        call msg("Invalid value of T2Interp", MSG_ERROR)
                        error stop
                  end select
            case ("T2ADAPTIVECUTOFF")
                  select case (uppercase(val))
                  case ("TRUE", "ENABLE", "ENABLED", "")
                        RPAParams%T2AdaptiveCutoff = .true.
                  case ("FALSE", "DISABLE", "DISABLED")
                        RPAParams%T2AdaptiveCutoff = .false.                        
                  case default
                        call msg("Invalid value of T2AdaptiveCutoff", MSG_ERROR)
                        error stop
                  end select
            case ("T2ADAPTIVECUTOFFTARGETKCAL")
                  read(val, *) m
                  if (m >= ZERO) then 
                        RPAParams%T2AdaptiveCutoffTargetKcal = m
                  else
                        call msg("Invalid value of T2AdaptiveCutoffTargetKcal", MSG_ERROR)
                        error stop
                  end if
            case ("EC1RDMAPPROX", "EC1RDM-APPROX", "EC1RDM-APPROXIMATION")
                  select case (uppercase(val))
                  case ("LINEAR")
                        RPAParams%Ec1RDMApprox = RPA_Ec1RDM_LINEAR
                  case ("QUADRATIC")
                        RPAParams%Ec1RDMApprox = RPA_Ec1RDM_QUADRATIC
                  case default
                        call msg("Invalid label of the Ec1RDM approximation", MSG_ERROR)
                        error stop
                  end select
            case ("DENSITYAPPROX")
                  select case (uppercase(val))                        
                  case ("T1(LINEAR)")
                        RPAParams%DensityApprox = RPA_RHO_T1_LINEAR
                  case ("T1(QUADRATIC)")
                        RPAParams%DensityApprox = RPA_RHO_T1_QUADRATIC
                  case ("T1(EXPONENTIAL)")
                        RPAParams%DensityApprox = RPA_RHO_T1_EXPONENTIAL
                  case ("S1(EXPONENTIAL)")
                        RPAParams%DensityApprox = RPA_RHO_S1_EXPONENTIAL
                  case ("OFF-DIAGONAL-DRCCSD")
                        RPAParams%DensityApprox = RPA_RHO_OFF_DIAGONAL_drCCSD
                  case ("DRCCSD")
                        RPAParams%DensityApprox = RPA_RHO_drCCSD
                  case ("DRCCSD+EXCHANGE", "DRCCSD+EXCH")
                        RPAParams%DensityApprox = RPA_RHO_drCCSD_PLUS_EXCHANGE
                  case default
                        call msg("Invalid value of DensityApprox", MSG_ERROR)
                        error stop
                  end select
            case ("PURIFY1RDM")
                  select case (uppercase(val))                        
                  case ("NONE", "DISABLE")
                        RPAParams%Purify1RDM = RPA_PURIFY_RHO_NONE
                  case ("CANCES", "PERNAL", "CANCES-PERNAL", "CANCES-PERNAL2008")
                        RPAParams%Purify1RDM = RPA_PURIFY_RHO_CANCES_PERNAL2008
                  case ("KLIMES", "KLIMES2015")
                        RPAParams%Purify1RDM = RPA_PURIFY_RHO_KLIMES2015
                  case default
                        call msg("Invalid method of restoring 1-RDM N-representability", MSG_ERROR)
                        error stop
                  end select
            case ("SINGLESCORRECTION", "SINGLES")
                  select case (uppercase(val))                        
                  case ("NONE", "DISABLE")
                        RPAParams%SinglesCorrection = RPA_SINGLES_NONE
                  case ("KLIMES")
                        RPAParams%SinglesCorrection = RPA_SINGLES_KLIMES
                  case ("REN")
                        RPAParams%SinglesCorrection = RPA_SINGLES_REN
                  case default
                        call msg("Invalid value of SinglesCorrection", MSG_ERROR)
                        error stop
                  end select
            case ("GRIDLIMITDAI")
                  select case (uppercase(val))
                  case ("TRUE", "")
                        RPAParams%GridLimitDai = .true.
                  case ("FALSE")
                        RPAParams%GridLimitDai = .false.
                  case default
                        call msg("Invalid value of GridLimitDai", MSG_ERROR)
                        error stop
                  end select
            case ("DISABLECORRELATION")
                  RPAParams%DisableCorrelation = .true.
            case ("TARGETERRORRANDOM")
                  read(val, *) m
                  RPAParams%TargetErrorRandom = m                  
            case ("TARGETERRORLAPLACE")
                  read(val, *) m
                  RPAParams%TargetErrorLaplace = m
            case ("TARGETRELERRORLAPLACE")
                  read(val, *) m
                  RPAParams%TargetRelErrorLaplace = m
            case ("CHOLESKYTAUTHRESH", "CHOLESKYTAUTHRESHOLD")
                  read(val, *) m
                  RPAParams%CholeskyTauThresh = m
                  Chol2Params%CholeskyTauThresh = m
            case ("TARGETERRORFREQ")
                  read(val, *) m
                  RPAParams%TargetErrorFreq = m
            case ("TARGETRELERRORFREQ")
                  read(val, *) m
                  RPAParams%TargetRelErrorFreq = m
            case ("COREORBTHRESH")
                  read(val, *) m
                  RPAParams%CoreOrbThresh = m
            case ("SMALLEIGENVALCUTOFFT2", "SMALLEIGENVALSCUTOFFT2")
                  read(val, *) m
                  RPAParams%SmallEigenvalCutoffT2 = m
            case ("GUESSNVECST2", "NGUESSVECST2")
                  read(val, *) m
                  RPAParams%GuessNVecsT2 = m
            case ("MAXBLOCKDIM")
                  read(val, *) i
                  RPAParams%MaxBlockDim = i
                  Chol2Params%MaxBlockDim = i
            case default
                  call msg("Unknown keyword in RPA block: " // key, MSG_ERROR)
                  stop
            end select
      end subroutine read_block_RPA


      subroutine read_block_SCF(SCFParams, line)
            type(TSCFParams), intent(inout) :: SCFParams
            character(*), intent(in)        :: Line
            
            character(:), allocatable :: key, val, s1, s2
            real(F64) :: a
            integer :: i
            
            call split(line, key, val)
            select case (uppercase(key))
            case ("XCFUNC")
                  SCFParams%XCFunc = xcf_str2id(uppercase(val))
            case ("SREXX")
                  read(val, *) a
                  if ((a < ZERO) .or. (a > ONE)) then
                        call msg("Invalid value of srexx", MSG_ERROR)
                        error stop
                  else
                        SCFParams%SRExx = a
                  end if
            case ("OMEGA")
                  read(val, *) a
                  if (a <= ZERO) then
                        call msg("Invalid value of omega", MSG_ERROR)
                        error stop
                  else
                        SCFParams%Omega = a
                  end if
            case ("LINDEPTHRESH")
                  read(val, *) a
                  if (a < ZERO) then
                        call msg("Invalid value of LinDepThresh", MSG_ERROR)
                        error stop
                  else
                        SCFParams%LinDepThresh = a
                  end if
            case ("SAVERHO")
                  call split(val, s1, s2)
                  !
                  ! saverho binary/text file_path
                  !
                  select case (uppercase(s1))
                  case ("TEXT", "RHOA-TEXT")
                        SCFParams%save_rho_mode = FILEMODE_TEXT
                        SCFParams%save_rhoa_path = s2
                  case ("BINARY", "RHOA-BINARY")
                        SCFParams%save_rho_mode = FILEMODE_BINARY
                        SCFParams%save_rhoa_path = s2
                  case ("RHOB-TEXT")
                        SCFParams%save_rho_mode = FILEMODE_TEXT
                        SCFParams%save_rhob_path = s2
                  case ("RHOB-BINARY")
                        SCFParams%save_rho_mode = FILEMODE_BINARY
                        SCFParams%save_rhob_path = s2
                  case default
                        call smsg("Unknown mode for writing a converged SCF density", s1, MSG_ERROR)
                        error stop
                  end select
            case ("MAXNITERS")
                  read(val, *) i
                  if (i < 0) then
                        call msg("Invalid value of maxiters", priority=MSG_ERROR)
                        error stop
                  end if
                  SCFParams%MaxNIters = i
            case ("GUESS")
                  call split(val, s1, s2)
                  select case (uppercase(s1))
                  case ("HBARE")
                        SCFParams%guess_type = SCF_GUESS_HBARE
                  case ("ATOMIC")
                        SCFParams%guess_type = SCF_GUESS_ATOMIC
                  case ("TEXT")
                        SCFParams%guess_type = SCF_GUESS_TEXT_FILE
                        SCFParams%guess_rhoa_path = s2
                        SCFParams%guess_rhob_path = ""
                  case ("RHOA-TEXT")
                        SCFParams%guess_type = SCF_GUESS_TEXT_FILE
                        SCFParams%guess_rhoa_path = s2
                  case ("RHOB-TEXT")
                        SCFParams%guess_type = SCF_GUESS_TEXT_FILE
                        SCFParams%guess_rhob_path = s2
                  case ("BINARY")
                        SCFParams%guess_type = SCF_GUESS_BINARY_FILE
                        SCFParams%guess_rhoa_path = s2
                        SCFParams%guess_rhob_path = ""
                  case ("RHOA-BINARY")
                        SCFParams%guess_type = SCF_GUESS_BINARY_FILE
                        SCFParams%guess_rhoa_path = s2
                  case ("RHOB-BINARY")
                        SCFParams%guess_type = SCF_GUESS_BINARY_FILE
                        SCFParams%guess_rhob_path = s2
                  case default
                        call msg("Unknown guess requested", MSG_ERROR)
                        error stop
                  end select
            case ("THRESH", "THRESHOLD")
                  call split(val, s1, s2)
                  read(s2, *) a
                  if (a <= ZERO) then
                        call msg("SCF convergence threshold must be positive", MSG_ERROR)
                        error stop
                  end if
                  select case (uppercase(s1))
                  case ("DENSITY")
                        SCFParams%ConvThreshRho = a
                  case ("GRADIENT", "GRAD")
                        SCFParams%ConvThreshGrad = a
                  case default
                        call msg("Invalid value of SCF convergence threshold", MSG_ERROR)
                        error stop
                  end select
            case ("ASYMPVXC")
                  select case (uppercase(val))
                  case ("LOCALIZED-FERMI-AMALDI", "LFA", "LFAS")
                        SCFParams%AsympVxc = AC_LFAS
                        SCFParams%Hirsh = .true.
                  case default
                        call msg("Invalid asymptotic correction", MSG_ERROR)
                        error stop
                  end select
            case ("ALGORITHM", "ERI_ALGORITHM", "ERI-ALGORITHM")
                  select case (uppercase(val))
                  case ("THC", "TENSORHYPERCONTRACTION", "TENSOR-HYPERCONTRACTION", "TENSOR_HYPERCONTRACTION")
                        SCFParams%ERI_Algorithm = SCF_ERI_THC
                  case ("CHOLESKY")
                        SCFParams%ERI_Algorithm = SCF_ERI_CHOLESKY
                  case ("EXACT")
                        SCFParams%ERI_Algorithm = SCF_ERI_EXACT
                  end select
            case ("THC_QRTHRESH", "THCQRTHRESH")
                  read(val, *) a
                  SCFParams%THC_QRThresh = a
            case ("ASYMPVXCOMEGA")
                  read(val, *) a
                  if (a > ZERO) then
                        SCFParams%AsympVxcOmega = a
                  else
                        call msg("Invalid value of AsympVxcOmega", MSG_ERROR)
                        error stop
                  end if
            case ("GRIDKIND")
                  select case (uppercase(val))
                  case ("SG1", "SG-1")
                        SCFParams%GridKind = GRD_SG1
                  case ("MEDIUM")
                        SCFParams%GridKind = GRD_MEDIUM
                  case ("FINE")
                        SCFParams%GridKind = GRD_FINE
                  case ("XFINE")
                        SCFParams%GridKind = GRD_XFINE
                  case default
                        call msg("Unknown grid kind", priority=MSG_ERROR)
                        error stop
                  end select
            case ("GRIDPRUNING")
                  select case (uppercase(val))
                  case ("TRUE")
                        SCFParams%GridPruning = .true.
                  case ("FALSE")
                        SCFParams%GridPruning = .false.
                  case default
                        call msg("Invalid GridPruning", priority=MSG_ERROR)
                        error stop
                  end select
            case ("SPHERAO")
                  select case (uppercase(val))
                  case ("TRUE", "")
                        SCFParams%SpherAO = .true.
                  case ("FALSE")
                        SCFParams%SpherAO = .false.
                  case default
                        call msg("Invalid value of SpherAO", MSG_ERROR)
                        error stop
                  end select
            case default
                  call msg("Invalid keyword: " // key)
            end select
      end subroutine read_block_SCF
      

      subroutine read_block_XYZ(System, AtomIdx, line)
            type(TSystem), intent(inout) :: System
            integer, intent(inout)       :: AtomIdx
            character(*), intent(in)     :: line

            character(:), allocatable :: key, val
            character(:), allocatable :: element, coords
            integer :: k, z
            integer :: NSubsystems

            call split(line, key, val)
            key = uppercase(key)
            if (System%SystemKind == SYS_NONE) then
                  NSubsystems = IntListLength(line)
                  select case (NSubsystems)
                  case (1)
                        System%SystemKind = SYS_MOLECULE
                  case (2)
                        System%SystemKind = SYS_DIMER
                  case (3)
                        System%SystemKind = SYS_TRIMER
                  case (4)
                        System%SystemKind = SYS_TETRAMER
                  case default
                        call msg("First line of the XYZ block has an invalid format", MSG_ERROR)
                        stop
                  end select
                  AtomIdx = 0
                  read(line, *) (System%SubsystemAtoms(k), k=1,System%SystemKind)
                  System%NAtoms = sum(System%SubsystemAtoms)
                  System%RealAtoms(:, 1) = [1, System%NAtoms]
                  System%RealAtoms(:, 2) = [1, 0]
                  allocate(System%AtomCoords(3, System%NAtoms))
                  allocate(System%ZNumbers(System%NAtoms))
            else if (key == "CHARGE" .or. key == "CHARGES") then
                  read(val, *) (System%SubsystemCharges(k), k=1,System%SystemKind)
                  System%Charge = sum(System%SubsystemCharges)
            else if (key == "MULT" .or. key == "MULTIPLICITY") then
                  if (System%SystemKind==SYS_MOLECULE) then
                        read(val, *) System%SubsystemMult(1)
                  else if (System%SystemKind==SYS_DIMER) then
                        read(val, *) (System%SubsystemMult(k), k=1,3)
                  else if (System%SystemKind==SYS_TRIMER) then
                        read(val, *) (System%SubsystemMult(k), k=1,7)
                  else ! Multiplicities of subsystems in a tetramer
                        read(val, *) (System%SubsystemMult(k), k=1,15)
                  end if
                  System%Mult = System%SubsystemMult(1)
            else
                  if (AtomIdx > -1) then
                        AtomIdx = AtomIdx + 1
                        if (AtomIdx <= System%NAtoms) then
                              element = key
                              coords = val
                              z = znumber_short(element)
                              if (z > 0) then
                                    System%ZNumbers(AtomIdx) = z
                                    read(coords, *) (System%AtomCoords(k, AtomIdx), k=1,3)
                                    System%AtomCoords(:, AtomIdx) = tobohr(System%AtomCoords(:, AtomIdx))
                              else
                                    call msg("Unknown element: " // element, MSG_ERROR)
                                    stop
                              end if
                        else
                              call msg("Inconsistent number of atoms specified", MSG_ERROR)
                              stop
                        end if
                  else
                        call msg("Unspecified number of atoms", MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine read_block_XYZ

      
      subroutine read_block_NonSCF(SCFParams, line)
            type(TSCFParams), intent(inout) :: SCFParams
            character(*), intent(in)        :: line
            
            character(:), allocatable :: key, val
            real(F64) :: a
            
            call split(line, key, val)
            select case (uppercase(key))
            case ("XCFUNC")
                  SCFParams%non_scf_xcfunc = xcf_str2id(uppercase(val))
            case ("SREXX")
                  read(val, *) a
                  if (a < ZERO .or. a > ONE) then
                        call msg("Invalid value of srexx", MSG_ERROR)
                        error stop
                  else
                        SCFParams%non_scf_srexx = a
                  end if
            case ("OMEGA")
                  read(val, *) a
                  if (a <= ZERO) then
                        call msg("Invalid value of omega", MSG_ERROR)
                        error stop
                  else
                        SCFParams%non_scf_omega = a
                  end if
            case ("SLATERVXC")
                  SCFParams%SlaterVxc = .true.
            end select
      end subroutine read_block_NonSCF

      
      subroutine read_block_RhoDiff(line)
            character(*), intent(in) :: line
            character(:), allocatable :: key, val, val1, val2
            integer :: k
            real(F64), dimension(3) :: vector
            
            call split(line, key, val)
            select case (uppercase(key))
            case ("REF")
                  RHO_DIFF_Ref = val
            case ("REFA")
                  RHO_DIFF_RefA = val
            case ("REFB")
                  RHO_DIFF_RefB = val
            case ("REFAB")
                  RHO_DIFF_RefAB = val
            case ("A")
                  RHO_DIFF_A = val
            case ("B")
                  RHO_DIFF_B = val
            case ("CSV")
                  RHO_DIFF_CSV = val
            case ("PROBESTART")
                  call split(val, val1, val2)
                  select case (uppercase(val1))
                  case ("BOHR")
                        read(val2, *) (vector(k), k=1,3)
                        RHO_DIFF_ProbeStart = vector
                  case ("ANGS")
                        read(val2, *) (vector(k), k=1,3)
                        RHO_DIFF_ProbeStart = tobohr(vector)
                  case default
                        call msg("RhoDiff: invalid unit of length", MSG_ERROR)
                        stop
                  end select
            case ("PROBESTOP")
                  call split(val, val1, val2)
                  select case (uppercase(val1))
                  case ("BOHR")
                        read(val2, *) (vector(k), k=1,3)
                        RHO_DIFF_ProbeStop = vector
                  case ("ANGS")
                        read(val2, *) (vector(k), k=1,3)
                        RHO_DIFF_ProbeStop = tobohr(vector)
                  case default
                        call msg("RhoDiff: invalid unit of length", MSG_ERROR)
                        stop
                  end select
            case ("PROBEN")
                  read(val, *) RHO_DIFF_ProbeN
            case ("MONOMER_A")
                  read(val, *) (RHO_DIFF_MonoA(k), k=1,2)
            case ("MONOMER_B")
                  read(val, *) (RHO_DIFF_MonoB(k), k=1,2)
            case ("TYPE")
                  select case (uppercase(val))
                  case ("SIMPLE")
                        RHO_DIFF_TYPE = RHO_DIFF_SIMPLE
                  case("DIMER")
                        RHO_DIFF_TYPE = RHO_DIFF_DIMER
                  case default
                        call msg("Unknown type of RhoDiff procedure", MSG_ERROR)
                        stop
                  end select
            end select
      end subroutine read_block_RhoDiff
      
            
      subroutine read_block_auxint(SCFParams, line)
            type(TSCFParams), intent(inout) :: SCFParams
            character(*), intent(in)        :: line
            
            character(:), allocatable :: key, val, val1, val2

            call split(line, key, val)
            select case (uppercase(key))
            case ("DENSITY_COMPARE", "DENSITY-COMPARE")
                  SCFParams%AuxInt_Type1 = AUX_DENSITY_COMPARE
                  call split(val, val1, val2)
                  SCFParams%aux_density_compare_path = val2
                  if (uppercase(val1) == "BINARY") then
                        SCFParams%aux_density_compare_mode = FILEMODE_BINARY
                  else if (uppercase(val1) == "TEXT") then
                        SCFParams%aux_density_compare_mode = FILEMODE_TEXT
                  else
                        call msg("Inavlid file type specified for DENSITY_COMPARE", MSG_ERROR)
                        error stop
                  end if
            case ("XC_HOLE")
                  select case (uppercase(val))
                  case ("BR_X")
                        SCFParams%AuxInt_Type1 = AUX_BR_X_HOLE
                  case ("PBE_X")
                        SCFParams%AuxInt_Type1 = AUX_PBE_X_HOLE
                  case ("TPSS_X")
                        SCFParams%AuxInt_Type1 = AUX_TPSS_X_HOLE
                  case default 
                        call msg("Invalid keyword: " // val, MSG_ERROR)
                        error stop
                  end select
            case ("HIRSHFELD-POPULATION", "HIRSHFELD_POPULATION")
                  SCFParams%AuxInt_Type1 = AUX_HIRSHFELD_POPULATION
                  SCFParams%Hirsh = .true.
                  HIRSH = .true.
            case ("HIRSHFELD-VOLUME", "HIRSHFELD_VOLUME")
                  SCFParams%AuxInt_Type1 = AUX_HIRSHFELD_VOLUME
                  SCFParams%Hirsh = .true.
                  HIRSH = .true.
            case ("GDD-OMEGA", "GDD_OMEGA")
                  SCFParams%AuxInt_Type1 = AUX_GDD_OMEGA
            case ("XC_HOLE_SPACING")
                  read(val, *) SCFParams%aux_xc_hole_spacing
            case ("XC_HOLE_NPOINTS")
                  read(val, *) SCFParams%aux_xc_hole_npoints
            case default
                  call msg("Invalid keyword: " // key, MSG_ERROR)
                  error stop
            end select
      end subroutine read_block_auxint


      subroutine read_scf_maxit(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: a

            read(line, *) keyword, a
            if (a .lt. 0) then
                  call msg("PARSER ERROR: BAD VALUE OF SCF_MAXIT THRESHOLD SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            SCF_MAXIT = a
      end subroutine read_scf_maxit


      subroutine read_mp2_mword(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

            read(line, *) keyword, i
            if (i .le. 0) then
                  call msg("PARSER ERROR: BAD VALUE OF MP2_MWORD SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            MP2_MWORD = i
      end subroutine read_mp2_mword

      subroutine read_cisd_gs(line)
            character(len=DEFLEN), intent(in) :: line
            
            character(len=DEFLEN) :: keyword
            integer :: i
            integer, parameter :: zero = 0

            read(line, *) keyword, i
            if (i .lt. zero) then
                  call msg("PARSER ERROR: BAD VALUE OF CISD_GUESS SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if
            
            CISD_GUESS_S = i
      end subroutine read_cisd_gs
  

      subroutine read_cisd_gd(line)
            character(len=DEFLEN), intent(in) :: line
            
            character(len=DEFLEN) :: keyword
            integer :: i
            integer, parameter :: zero = 0
            
            read(line, *) keyword, i
            if (i .lt. zero) then
                  call msg("PARSER ERROR: BAD VALUE OF CISD_GUESS SPECIFIED", &
                priority=MSG_ERROR)
                  stop
            end if
        
            CISD_GUESS_D = i
      end subroutine read_cisd_gd

      subroutine read_cisd_gsd(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i
            integer, parameter :: zero = 0

            read(line, *) keyword, i
            if (i .lt. zero) then
                  call msg("PARSER ERROR: BAD VALUE OF CISD_GUESS SPECIFIED", &
                priority=MSG_ERROR)
                  stop
            end if

            CISD_GUESS_SD = i
      end subroutine read_cisd_gsd



      subroutine read_cc_diis_nmax(line)
        character(len=DEFLEN), intent(in) :: line

        character(len=DEFLEN) :: keyword
        integer :: i
        integer, parameter :: zero = 0

        read(line, *) keyword, i
        if (i .le. zero) then
           call msg("PARSER ERROR: BAD VALUE OF CC_DIIS_NMAX SPECIFIED", &
                priority=MSG_ERROR)
           stop
        end if

        CC_DIIS_NMAX = i
      end subroutine read_cc_diis_nmax


      subroutine read_cc_diis_nstart(line)
        character(len=DEFLEN), intent(in) :: line

        character(len=DEFLEN) :: keyword
        integer :: i
        integer, parameter :: zero = 0

        read(line, *) keyword, i
        if (i .le. zero) then
           call msg("PARSER ERROR: BAD VALUE OF CC_DIIS_NSTART SPECIFIED", &
                priority=MSG_ERROR)
           stop
        end if

        CC_DIIS_NSTART = i
      end subroutine read_cc_diis_nstart


      subroutine read_cc_diis_nrelax(line)
        character(len=DEFLEN), intent(in) :: line

        character(len=DEFLEN) :: keyword
        integer :: i
        integer, parameter :: zero = 0

        read(line, *) keyword, i
        if (i .le. zero) then
           call msg("PARSER ERROR: BAD VALUE OF CC_DIIS_NRELAX SPECIFIED", &
                priority=MSG_ERROR)
           stop
        end if

        CC_DIIS_NRELAX = i
      end subroutine read_cc_diis_nrelax

      subroutine read_dav_qconvthresh(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            double precision :: i
            
            read(line, *) keyword, i
            if (i .le. zero) then
                  call msg("PARSER ERROR: BAD VALUE OF DAV_QCONVTHRESH SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if
            
            DAV_QCONVTHRSH = i
      end subroutine read_dav_qconvthresh
      

      subroutine read_cc_amp_thresh(line)
        character(len=DEFLEN), intent(in) :: line

        character(len=DEFLEN) :: keyword
        double precision :: i

        read(line, *) keyword, i
        if (i .le. zero) then
           call msg("PARSER ERROR: BAD VALUE OF CC_AMP_THRESH SPECIFIED", &
                priority=MSG_ERROR)
           stop
        end if

        CC_AMP_THRESH = i
      end subroutine read_cc_amp_thresh


      subroutine read_cc_e_thresh(line)
        character(len=DEFLEN), intent(in) :: line

        character(len=DEFLEN) :: keyword
        double precision :: i

        read(line, *) keyword, i
        if (i .le. zero) then
           call msg("PARSER ERROR: BAD VALUE OF CC_E_THRESH SPECIFIED", &
                priority=MSG_ERROR)
           stop
        end if

        CC_E_THRESH = i
      end subroutine read_cc_e_thresh


      subroutine read_cc_min_hlgap(line)
        character(len=DEFLEN), intent(in) :: line

        character(len=DEFLEN) :: keyword
        double precision :: i

        read(line, *) keyword, i
        if (i .le. zero) then
           call msg("PARSER ERROR: BAD VALUE OF CC_MIN_HLGAP SPECIFIED", &
                priority=MSG_ERROR)
           stop
        end if

        CC_MIN_HLGAP = i
      end subroutine read_cc_min_hlgap

     subroutine read_cc_nlevels(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
                integer :: i
            integer, parameter :: zero = 0

            read(line, *) keyword, i
            if (i .le. zero) then
                  call msg("PARSER ERROR: BAD VALUE OF CC_NLEVELS SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            CC_NLEVELS = i
      end subroutine read_cc_nlevels

     subroutine read_cc_frozen(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
                integer :: i

            read(line, *) keyword, i
                if (i .lt. 0 ) then
                  call msg("PARSER ERROR: BAD VALUE OF FROZEN ORBITALS SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            CC_FROZEN = i

      end subroutine read_cc_frozen

      subroutine read_cc_binto(line)
            character(len=DEFLEN), intent(in) :: line
            
            character(len=DEFLEN) :: keyword
            integer :: i
            
            read(line, *) keyword, i
            if (i .lt. 0 ) then
                  call msg("PARSER ERROR: BAD VALUE OF CC_BINTO SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if
            
            CC_BINTO = i
            
      end subroutine read_cc_binto

      subroutine read_cc_bintv(line)
                character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

                read(line, *) keyword, i
            if (i .lt. 0 ) then
                    call msg("PARSER ERROR: BAD VALUE OF CC_BINTV SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            CC_BINTV = i

      end subroutine read_cc_bintv

      subroutine read_cc_kinto(line)
                character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

                read(line, *) keyword, i
            if (i .lt. 0 ) then
                    call msg("PARSER ERROR: BAD VALUE OF CC_KINTO SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            CC_KINTO = i

      end subroutine read_cc_kinto


      subroutine read_cc_kintv(line)
                    character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

                    read(line, *) keyword, i
            if (i .lt. 0 ) then
                      call msg("PARSER ERROR: BAD VALUE OF CC_KINTV SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            CC_KINTV = i

      end subroutine read_cc_kintv

      


      subroutine read_cc_non(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

            read(line, *) keyword, i
                if (i .lt. 0 ) then
                        call msg("PARSER ERROR: BAD VALUE OF FROZEN ORBITALS SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

                CC_NON = i

          end subroutine read_cc_non



     subroutine read_mbpt_order(line)
            character(len=DEFLEN), intent(in) :: line
            
            character(len=DEFLEN) :: keyword
                integer :: i

            read(line, *) keyword, i
            if (i .gt. 8) then
                  call msg("PARSER ERROR: BAD VALUE OF MBPT_ORDER SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            MBPT_ORDER = i
      end subroutine read_mbpt_order

     subroutine read_s_order(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

            read(line, *) keyword, i
            if (i .ne. 2) then
                  if (i .ne. 3) then
                        if (i .ne. 4) then
                              if (i .ne. 234) then
                                    call msg("PARSER ERROR: BAD VALUE OF S_ORDER SPECIFIED", &
                                          priority=MSG_ERROR)
                                    stop
                              end if
                        end if
                  end if
            end if

            S_ORDER = i

      end subroutine read_s_order


      subroutine read_maxpt(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            integer :: i

            read(line, *) keyword, i
            if (i .lt. 0 .or. i .gt. 4) then
                  call msg("PARSER ERROR: BAD VALUE OF MAXPT SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if

            MAXPT = i
      end subroutine read_maxpt


      subroutine read_tag_dft_total_energy(line)
            character(*), intent(in) :: line
            
            character(:), allocatable :: key, val
            
            call split(line, key, val)
            if (len(val) > 0) then
                  TAG_DFT_TOTAL_ENERGY = val
            else
                  call msg("INVALID VALUE OF TAG_DFT_TOTAL_ENERGY", MSG_ERROR)
                  stop
            end if
      end subroutine read_tag_dft_total_energy


      subroutine read_tag_dft_disp(line)
            character(*), intent(in) :: line
            
            character(:), allocatable :: key, val
            
            call split(line, key, val)
            if (len(val) > 0) then
                  TAG_DFT_DISP = val
            else
                  call msg("INVALID VALUE OF TAG_DFT_TOTAL_ENERGY", MSG_ERROR)
                  stop
            end if
      end subroutine read_tag_dft_disp


      subroutine read_cubefile_spacing(line)
            character(len=DEFLEN), intent(in) :: line

            character(len=DEFLEN) :: keyword
            character(len=DEFLEN) :: a

            read(line, *) keyword, a
            select case (uppercase(a))
            case ("COARSE")
                  CUBEFILE_SPACING = VOL_SPACING_COARSE
            case ("MEDIUM")
                  CUBEFILE_SPACING = VOL_SPACING_MEDIUM
            case ("FINE")
                  CUBEFILE_SPACING = VOL_SPACING_FINE
            case default
                  call msg("INVALID CUBE SPACING", MSG_ERROR)
                  call msg("AVAILABLE PRESETS: COARSE, MEDIUM, FINE", MSG_ERROR)
                  error stop                  
            end select
      end subroutine read_cubefile_spacing

      
      subroutine read_vis_cubefile(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, s12, s1, s2

            call split(line, keyword, s12)
            call split(s12, s1, s2)

            if (isblank(s2)) then
                  if (isblank(s1)) then
                        call msg("FILE PATH MUST BE PROVIDED FOR VIS_CUBFILE", MSG_ERROR)
                        stop
                  else
                        VIS_CUBEFILE_PATH = s1
                  end if
            else
                  select case (uppercase(s1))
                  case ("COARSE")
                        VIS_CUBEFILE_SPACING = VOL_SPACING_COARSE
                  case ("MEDIUM")
                        VIS_CUBEFILE_SPACING = VOL_SPACING_MEDIUM
                  case ("FINE")
                        VIS_CUBEFILE_SPACING = VOL_SPACING_FINE
                  case default
                        call msg("INVALID CUBE SPACING", MSG_ERROR)
                        call msg("AVAILABLE PRESETS: COARSE, MEDIUM, FINE", MSG_ERROR)
                        stop
                  end select

                  VIS_CUBEFILE_PATH = s2
            end if
      end subroutine read_vis_cubefile


      subroutine read_vis_rhoa(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, s12, s1, s2

            call split(line, keyword, s12)
            call split(s12, s1, s2)
            
            if (.not. isblank(s2)) then
                  !
                  ! vis_rhoa binary/text file_path
                  !
                  select case (uppercase(s1))
                  case ("TEXT")
                        VIS_RHOA_MODE = FILEMODE_TEXT
                  case ("BINARY")
                        VIS_RHOA_MODE = FILEMODE_BINARY
                  case default
                        call smsg("UNKNOWN MODE FOR READING FILES", s1, MSG_ERROR)
                        stop
                  end select
                  
                  VIS_RHOA_PATH = s2
            else
                  !
                  ! vis_rhoa file_path
                  !
                  if (.not. isblank(s1)) then
                        VIS_RHOA_PATH = s1
                  else
                        call msg("FILE PATH NOT PROVIDED FOR VIS_RHOA", MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine read_vis_rhoa


      subroutine read_vis_rhob(line)
            character(*), intent(in) :: line

            character(:), allocatable :: keyword, s12, s1, s2

            call split(line, keyword, s12)
            call split(s12, s1, s2)
            
            if (.not. isblank(s2)) then
                  !
                  ! vis_rhoa binary/text file_path
                  !
                  select case (uppercase(s1))
                  case ("TEXT")
                        VIS_RHOB_MODE = FILEMODE_TEXT
                  case ("BINARY")
                        VIS_RHOB_MODE = FILEMODE_BINARY
                  case default
                        call smsg("UNKNOWN MODE FOR READING FILES", s1, MSG_ERROR)
                        stop
                  end select
                  
                  VIS_RHOB_PATH = s2
            else
                  !
                  ! vis_rhoa file_path
                  !
                  if (.not. isblank(s1)) then
                        VIS_RHOB_PATH = s1
                  else
                        call msg("FILE PATH NOT PROVIDED FOR VIS_RHOB", MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine read_vis_rhob


      subroutine read_cubefile_mo(line)
            character(len=DEFLEN), intent(in) :: line
            character(:), allocatable :: key, val

            integer :: nmo

            call split(line, key, val)
            nmo = IntListLength(val)
            
            if (nmo == 0) then
                  call msg("INDICES OF MOLECULAR ORBITALS NOT PROVIDED FOR CUBEFILE_MO", &
                        MSG_ERROR)
                  stop
            end if
            
            allocate(CUBEFILE_MO_IDX(nmo))
            read(val, *) CUBEFILE_MO_IDX
            CUBEFILE_FUNC = ior(CUBEFILE_FUNC, VOL_FUNC_MO)

            if (minval(CUBEFILE_MO_IDX) <= 0) then
                  call msg("INVALID ORBITAL INDEX PROVIDED FOR CUBEFILE_MO", &
                        MSG_ERROR)
                  stop
            end if
      end subroutine read_cubefile_mo


      subroutine read_cubefile_ao(line)
            character(len=DEFLEN), intent(in) :: line

            character(:), allocatable :: key, val
            integer :: nao

            call split(line, key, val)
            nao = IntListLength(val)
            
            if (nao == 0) then
                  call msg("INDICES OF ATOMS MUST BE PROVIDED FOR CUBEFILE_AO", &
                        MSG_ERROR)
                  stop
            end if
            
            allocate(CUBEFILE_AO_CENTERS(nao))
            read(val, *) CUBEFILE_AO_CENTERS
            CUBEFILE_FUNC = ior(CUBEFILE_FUNC, VOL_FUNC_AO)

            if (minval(CUBEFILE_AO_CENTERS) <= 0) then
                  call msg("INVALID ATOM INDEX PROVIDED FOR CUBEFILE_AO", &
                        MSG_ERROR)
                  stop
            end if
      end subroutine read_cubefile_ao

      
      subroutine read_rootdir(cmd)
            character(len=*), intent(in) :: cmd

            character(len=:), allocatable :: runpath
            !
            ! Strip the executable name and ./bin directory
            ! from the COMMAND_NAME string
            !
            runpath = dirname(dirname(cmd))

            if (len(runpath) .gt. 0) then
                  ROOTDIR = runpath // DIRSEP
            else
                  ROOTDIR = ".." // DIRSEP
            end if

            BASISDIR = ROOTDIR // "basis-sets" // DIRSEP
            GUESSDIR = ROOTDIR // "guess" // DIRSEP
            ECPDIR = ROOTDIR // "pseudopotentials" // DIRSEP
      end subroutine read_rootdir


      subroutine read_cc_eom_memspace(line)
            character(DEFLEN), intent(in) :: line
            
            character(:), allocatable :: keyword, val
            integer(I64) :: i

            call split(line, keyword, val)
            i = readbytes(val)

            if (i < 0) then
                  call msg("INVALID VALUE OF CC_EOM_MEMSPACE", MSG_ERROR)
                  stop
            else
                  CC_EOM_MEMSPACE = i
            end if
      end subroutine read_cc_eom_memspace

      
      subroutine read_cc_eom_diskspace(line)
            character(DEFLEN), intent(in) :: line
            
            character(:), allocatable :: keyword, val
            integer(I64) :: i

            call split(line, keyword, val)
            i = readbytes(val)

            if (i < 0) then
                  call msg("INVALID VALUE OF CC_EOM_DISKSPACE", MSG_ERROR)
                  stop
            else
                  CC_EOM_DISKSPACE = i
            end if
      end subroutine read_cc_eom_diskspace
end module parser
