! dftd3 program for computing the dispersion energy and forces from cartesian atomic coordinates
! and atomic numbers as described in
! S. Grimme, J. Antony, S. Ehrlich and H. Krieg
! A consistent and accurate ab initio parameterization of density functional dispersion correction
! (DFT-D) for the 94 elements H-Pu
! J. Chem. Phys, 132 (2010), 154104
! if BJ-damping is used
! S. Grimme, S. Ehrlich and L. Goerigk, J. Comput. Chem, 32 (2011), 1456-1465.
! should be cited as well.
! Copyright (C) 2009 - 2011 Stefan Grimme, University of Muenster, Germany
! This program is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 1, or (at your option)
! any later version.
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
module dftd3
      use math_constants
      use arithmetic
      use gparam
      use display
      use periodic
      use dftd3par
      use h_xcfunc

      implicit none

      integer, parameter :: DFTD3_IS6 = 1
      integer, parameter :: DFTD3_IR6 = 2
      integer, parameter :: DFTD3_IS8 = 3
      integer, parameter :: DFTD3_IR8 = 4
      integer, parameter :: DFTD3_IAL = 5

      integer, parameter :: DFTD3_BJ_IS6 = 1
      integer, parameter :: DFTD3_BJ_IA1 = 2
      integer, parameter :: DFTD3_BJ_IS8 = 3
      integer, parameter :: DFTD3_BJ_IA2 = 4
      !
      ! Global parameters
      !
      real(F64), parameter :: k1=16.0d+0
      real(F64), parameter :: k2=4.d+0/3.d+0
      !
      ! Reasonable choices are between 3 and 5
      ! this gives smoth curves with maxima around the integer values
      ! k3=3 give for CN=0 a slightly smaller value than computed
      ! for the free atom. This also yields to larger CN for atoms
      ! in larger molecules but with the same chem. environment
      ! which is physically not right
      ! values >5 might lead to bumps in the potential
      !
      real(F64), parameter :: k3=-4.d+0

contains

      subroutine compdftd3(ixc, edisp_dftd3, compute_3body, bj_damping, usrpar)
            !
            ! USRPAR array contains user-defined DFT-D3 parameters:
            ! S6  = USRPAR(DFTD3_IS6)
            ! RS6 = USRPAR(DFTD3_IR6)
            ! ALP = USRPAR(DFTD3_IAL)
            ! ...
            !
            type(txcdef), intent(in)                      :: ixc
            real(F64), intent(out)                        :: edisp_dftd3
            logical, intent(in)                           :: compute_3body
            logical, intent(in)                           :: bj_damping
            real(F64), dimension(:), optional, intent(in) :: usrpar

            integer :: max_elem,maxc
            ! conversion factors
            real(F64) :: autoang,autokcal
            parameter (max_elem=94)
            ! maximum coordination number references per element
            parameter (maxc    =5)
            ! DFT-D version
            integer :: version
            ! number of atoms
            integer :: n
            !
            ! Coordinates in au
            !
            real(F64), dimension(:, :), allocatable :: xyz
            !
            ! Gradient
            !
            real(F64), dimension(:, :), allocatable :: g
            !
            ! Cardinal numbers of elements
            !
            integer, dimension(:), allocatable ::   iz
            ! cut-off radii for all element pairs
            real(F64), dimension(max_elem,max_elem) :: r0ab
            ! C6 for all element pairs
            real(F64), dimension(max_elem,max_elem,maxc,maxc,3) :: c6ab
            ! how many different C6 for one element
            integer, dimension(max_elem) :: mxc
            ! C6
            real(F64) :: c6
            ! coordination numbers of the atoms
            real(F64), dimension(:), allocatable :: cn
            !
            ! Covalent radii
            !
            real(F64), dimension(max_elem) :: rcov
            ! atomic <r^2>/<r^4> values
            real(F64), dimension(max_elem) :: r2r4
            ! energies
            real(F64) :: e6, e8, e10, e12, disp, e6abc
            ! THE PARAMETERS OF THE METHOD (not all are independent)
            real(F64) :: rs6, rs8, s6, s18, alp6, alp8, rs18, alp
            ! printout option
            logical :: echo
            ! grad ?
            logical :: grad
            ! third-order term?
            logical :: noabc
            ! gradient calctype
            logical :: numgrad
            ! special parameters
            logical :: tz
            ! R^2 distance neglect threshold (important for speed in case of large systems)
            real(F64) :: rthr,rthr2
            integer :: ifunc

            integer :: i,iat,jat
            real(F64) ::  gdsp,dum
            character(len=DEFLEN) :: line
            !
            ! Use globally defined conversion factors
            !
            autoang = toang(1.d+0)
            autokcal = tokcal(1.d+0)
            !
            ! PBE0/def2-QZVP atomic values
            !
            r2r4 = (/ &
                  8.0589d+0,  3.4698d+0, 29.0974d+0, 14.8517d+0, 11.8799d+0,  7.8715d+0,  5.5588d+0, &
                  4.7566d+0,  3.8025d+0,  3.1036d+0, 26.1552d+0, 17.2304d+0, 17.7210d+0, 12.7442d+0, &
                  9.5361d+0,  8.1652d+0,  6.7463d+0,  5.6004d+0, 29.2012d+0, 22.3934d+0, 19.0598d+0, &
                  16.8590d+0, 15.4023d+0, 12.5589d+0, 13.4788d+0, 12.2309d+0, 11.2809d+0, 10.5569d+0, &
                  10.1428d+0,  9.4907d+0, 13.4606d+0, 10.8544d+0,  8.9386d+0,  8.1350d+0,  7.1251d+0, &
                  6.1971d+0, 30.0162d+0, 24.4103d+0, 20.3537d+0, 17.4780d+0, 13.5528d+0, 11.8451d+0, &
                  11.0355d+0, 10.1997d+0,  9.5414d+0,  9.0061d+0,  8.6417d+0,  8.9975d+0, 14.0834d+0, &
                  11.8333d+0, 10.0179d+0,  9.3844d+0,  8.4110d+0,  7.5152d+0, 32.7622d+0, 27.5708d+0, &
                  23.1671d+0, 21.6003d+0, 20.9615d+0, 20.4562d+0, 20.1010d+0, 19.7475d+0, 19.4828d+0, &
                  15.6013d+0, 19.2362d+0, 17.4717d+0, 17.8321d+0, 17.4237d+0, 17.1954d+0, 17.1631d+0, &
                  14.5716d+0, 15.8758d+0, 13.8989d+0, 12.4834d+0, 11.4421d+0, 10.2671d+0,  8.3549d+0, &
                  7.8496d+0,  7.3278d+0,  7.4820d+0, 13.5124d+0, 11.6554d+0, 10.0959d+0,  9.7340d+0, &
                  8.8584d+0,  8.0125d+0, 29.8135d+0, 26.3157d+0, 19.1885d+0, 15.8542d+0, 16.1305d+0, &
                  15.6161d+0, 15.1226d+0, 16.1576d+0 /)
            !
            ! Covalent radii (taken from Pyykko and Atsumi, Chem. Eur. J. 15, 2009, 188-197)
            ! values for metals decreased by 10%
            !
            rcov = (/ &
                  0.32d+0, 0.46d+0, 1.20d+0, 0.94d+0, 0.77d+0, 0.75d+0, 0.71d+0, 0.63d+0, 0.64d+0, 0.67d+0, &
                  1.40d+0, 1.25d+0, 1.13d+0, 1.04d+0, 1.10d+0, 1.02d+0, 0.99d+0, 0.96d+0, 1.76d+0, 1.54d+0, &
                  1.33d+0, 1.22d+0, 1.21d+0, 1.10d+0, 1.07d+0, 1.04d+0, 1.00d+0, 0.99d+0, 1.01d+0, 1.09d+0, &
                  1.12d+0, 1.09d+0, 1.15d+0, 1.10d+0, 1.14d+0, 1.17d+0, 1.89d+0, 1.67d+0, 1.47d+0, 1.39d+0, &
                  1.32d+0, 1.24d+0, 1.15d+0, 1.13d+0, 1.13d+0, 1.08d+0, 1.15d+0, 1.23d+0, 1.28d+0, 1.26d+0, &
                  1.26d+0, 1.23d+0, 1.32d+0, 1.31d+0, 2.09d+0, 1.76d+0, 1.62d+0, 1.47d+0, 1.58d+0, 1.57d+0, &
                  1.56d+0, 1.55d+0, 1.51d+0, 1.52d+0, 1.51d+0, 1.50d+0, 1.49d+0, 1.49d+0, 1.48d+0, 1.53d+0, &
                  1.46d+0, 1.37d+0, 1.31d+0, 1.23d+0, 1.18d+0, 1.16d+0, 1.11d+0, 1.12d+0, 1.13d+0, 1.32d+0, &
                  1.30d+0, 1.30d+0, 1.36d+0, 1.31d+0, 1.38d+0, 1.42d+0, 2.01d+0, 1.81d+0, 1.67d+0, 1.58d+0, &
                  1.52d+0, 1.53d+0, 1.54d+0, 1.55d+0 /)
            !
            ! DFT-D3 preamble
            !
            call toprule()
            call msg("DFT-D3 DISPERSION CORRECTION")
            call midrule()
            call msg("COEFFICIENT FILE V2.1 REV 6 (AUG 26 2011)")
            call msg("AUTHOR: S. GRIMME, UNIVERSITY OF MUENSTER")
            call blankline()
            call msg("Cite DFT-D3 as")
            call msg("1. S. Grimme, J. Antony, S. Ehrlich, and H. Krieg,")
            call msg("   J. Chem. phys. 132, 154104 (2010); doi: 10.1063/1.3382344")
            call msg("2. S. Grimme, S. Ehrlich, L. Goerigk,")
            call msg("   J. Comput. Chem. 32, 1456 (2011); doi: 10.1002/jcc.21759")
            call blankline()
            !
            ! Scale and convert RCOV to au
            !
            rcov = k2 * tobohr(1.d+0) * rcov

            echo    = .true.
            grad    = .false.
            noabc   = (.not. compute_3body)
            numgrad = .false.
            tz      = .false.
            ifunc    = xcf_get_id(ixc)
            if (bj_damping) then
                  version = 4
            else
                  version = 3
            end if
            !
            ! set parameters for functionals
            !
            if (.not. present(usrpar)) then
                  call setfuncpar(ifunc,version,tz,s6,rs6,s18,rs18,alp)
            else
                  if (bj_damping) then
                        s6 = usrpar(DFTD3_BJ_IS6)
                        rs6 = usrpar(DFTD3_BJ_IA1)
                        s18 = usrpar(DFTD3_BJ_IS8)
                        rs18 = usrpar(DFTD3_BJ_IA2)
                        alp = 14.0d0
                  else
                        !
                        ! Original damping function f(Rab=0)=0
                        !
                        s6   = usrpar(DFTD3_IS6)
                        rs6  = usrpar(DFTD3_IR6)
                        s18  = usrpar(DFTD3_IS8)
                        rs18 = usrpar(DFTD3_IR8)
                        alp  = usrpar(DFTD3_IAL)
                  end if
            end if
            !
            ! Number of non-dummy atoms
            !
            n = NRealAtoms()
            allocate(iz(n))
            allocate(cn(n))
            allocate(xyz(3, n))
            allocate(g(3, n))
            !
            ! Cutoff r^2 thresholds for the gradient in bohr^2.
            ! rthr influences N^2 part of the gradient.
            ! rthr2 influences the N^3 part of the gradient. When using
            ! dftd3 in combination with semi-empirical methods or FFs, and large
            ! (>1000 atoms) systems, rthr2 is crucial for speed:
            ! Recommended values are 20^2 to 25^2 bohr.
            !
            rthr=20000.0d0
            rthr2=1600.0d0

            call setr0ab(max_elem,autoang,r0ab)
            !
            ! C6 hard-coded (c6ab.dat not used)
            ! this is alternative to loadc6
            !
            call copyc6(c6ab, mxc)
            !
            ! Get Cartesian coordinates of the non-dummy nuclei
            ! and atomic numbers
            !
            call rdcoord(xyz, iz)

            ! the analytical E(3) grad is not available yet
            if(grad .AND. ( .NOT. noabc))numgrad= .TRUE. 

            ! norm rthr2 to r0HH to achieve more reasonable cutoffs
            !       write(*,*)rthr,rthr2
            rthr2=rthr2/r0ab(1,1)
            !
            ! CNs for output
            !
            call ncoord(n, rcov, iz, xyz, cn)
            !
            ! scale r4/r2 values of the atoms by sqrt(Z)
            ! sqrt is also globally close to optimum
            ! together with the factor 1/2 this yield reasonable
            ! c8 for he, ne and ar. for larger Z, C8 becomes too large
            ! which effectively mimics higher R^n terms neglected due
            ! to stability reasons
            !
            do i=1,max_elem
                  dum    =0.5d+0*r2r4(i)*real(i, F64)**0.5d+0
                  ! store it as sqrt because the geom. av. is taken
                  r2r4(i)=sqrt(dum)
            enddo
            !
            ! for global ad hoc parameters see
            ! k3 in subroutine getc6, k1 and k2 in subroutine ncoord*
            ! fixed or dependent ones:
            !
            rs8  = rs18
            alp6 = alp
            alp8 = alp+2.d+0
            !
            ! Note: if version=4 (Becke-Johnson), a1=rs6 and a2=rs18
            !       and alp* have no meaning
            ! ---
            !
            ! Check if all parameters have been loaded and are resonable
            !
            do iat=1,n-1
                  do jat=iat+1,n
                        if(r0ab(iz(jat),iz(iat)) < 0.1d+0) then
                              call msg("DFTD3 ERROR: RADIUS MISSING", priority=MSG_ERROR)
                              stop
                        endif

                        call getc6(maxc,max_elem,c6ab,mxc,iz(iat),iz(jat), &
                              cn(iat),cn(jat),c6)

                        if(c6 < 1.d-6) then
                              call msg("DFTD3 ERROR: C6 COEFFICIENT MISSING", priority=MSG_ERROR)
                              stop
                        endif
                  enddo
            enddo
            !
            ! Calculate energy
            !
            call edisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
                  rs6,rs8,alp6,alp8,version,noabc,rthr, &
                  e6,e8,e10,e12,e6abc)
            e6   = e6   *s6
            e6abc= e6abc*s6
            e8   = e8   *s18
            disp =-e6-e8-e6abc
            edisp_dftd3 = disp

            if (.not. bj_damping) then
                  call msg("Damping function: Ref. 1 (vanishes at Rab=0)")
            else
                  call msg("Damping function: Ref. 2 (Becke-Johnson rational damping)")
            endif
            call msg("Damping function parameters:")
            if (bj_damping) then
                  !
                  ! Becke-Johnson rational damping function
                  !
                  write(line, "(A3,F10.4)") "S6 ", s6
                  call msg(line)
                  write(line, "(A3,F10.4)") "S8 ", s18
                  call msg(line)
                  write(line, "(A3,F10.4)") "A1 ", rs6
                  call msg(line)
                  write(line, "(A3,F10.4)") "A2 ", rs18
                  call msg(line)
            else
                  !
                  ! Original DFT-D3 daping function f(Rab=0)=0
                  !
                  write(line, "(A7,F10.4)") "S6      ", s6
                  call msg(line)
                  write(line, "(A7,F10.4)") "S8      ", s18
                  call msg(line)
                  write(line, "(A7,F10.4)") "R6      ", rs6
                  call msg(line)
                  write(line, "(A7,F10.4)") "R8      ", rs18
                  call msg(line)
                  write(line, "(A7,F10.4)") "ALPHA6  ", alp6
                  call msg(line)
                  write(line, "(A7,F10.4)") "ALPHA8  ", alp8
                  call msg(line)
            endif
            call blankline()
            call dmsg("DFT-D3 DISPERSION ENERGY [KCAL/MOL]", tokcal(disp))
            call dmsg("1/R**6 CONTRIBUTION [KCAL/MOL]", tokcal(-e6))
            call dmsg("1/R**8 CONTRIBUTION [KCAL/MOL]", tokcal(-e8))
            if (.not. noabc) then
                  call dmsg("THREE-BODY TERM [KCAL/MOL]", tokcal(-e6abc))
            end if
            !
            ! Gradient
            !
            if(grad)then
                  g=0.d+0
                  call gdisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
                        s6,s18,rs6,rs8,alp6,alp8,noabc,rthr, &
                        numgrad,version,echo,g,gdsp,rthr2)
                  !
                  ! Check if gdisp yields same energy as edisp
                  !
                  if(abs((disp-gdsp)/disp) > 1.d-8) then
                        call msg("DFTD3 ERROR: INTERNAL ERROR")
                        stop
                  endif
            endif

            deallocate(iz)
            deallocate(cn)
            deallocate(xyz)
            deallocate(g)
      end subroutine compdftd3


      subroutine setfuncpar(ifunc,version,TZ,s6,rs6,s18,rs18,alp)
            !
            ! Detrmine parameters for requested functional
            !
            integer, intent(in)           :: ifunc
            integer, intent(in)           :: version
            logical, intent(in)           :: TZ
            real(F64), intent(out) :: s6,rs6,s18,alp,rs18

            ! double hybrid values revised according to procedure in the GMTKN30 paper

            ! DFT-D3 with Becke-Johnson finite-damping, variant 2 with their radii
            ! SE: Alp is only used in 3-body calculations
            if(version == 4)then
                  s6=1.0d0
                  alp =14.0d0
                  select case (ifunc)
                  case (XCF_XC_SCAN)
                        !
                        ! Parameters for SCAN taken from Table I in Brandenburg, J.G., Bates,
                        ! J.E., Sun, J., and Perdew, J.P. Phys. Rev. B 94,  115144 (2016);
                        ! doi: 10.1103/PhysRevB.94.115144
                        !
                        rs6 = 0.5380_F64  ! a1
                        s18 = 0.0000_F64  ! s8
                        rs18 = 5.4200_F64 ! a2

                        ! case ("b-p")
                        !       rs6 =0.3946
                        !       s18 =3.2822
                        !       rs18=4.8516
                        ! case ("b-lyp")
                        !       rs6 =0.4298
                        !       s18 =2.6996
                        !       rs18=4.2359
                        ! case ("revpbe")
                        !       rs6 =0.5238
                        !       s18 =2.3550
                        !       rs18=3.5016
                        ! case ("b97-d")
                        !       rs6 =0.5545
                        !       s18 =2.2609
                        !       rs18=3.2297
                  case (XCF_XC_PBE)
                        rs6 =0.4289_F64
                        s18 =0.7875_F64
                        rs18=4.4407_F64
                        ! case ("rpw86-pbe")
                        !       rs6 =0.4613
                        !       s18 =1.3845
                        !       rs18=4.5062
                  case (XCF_XC_B3LYP)
                        rs6 =0.3981_F64
                        s18 =1.9889_F64
                        rs18=4.4211_F64
                  case (XCF_XC_TPSS)
                        rs6 =0.4535_F64
                        s18 =1.9435_F64
                        rs18=4.4752_F64
                  case (XCF_HF)
                        rs6 =0.3385_F64
                        s18 =0.9171_F64
                        rs18=2.8830_F64
                        ! case ("tpss0")
                        !       rs6 =0.3768
                        !       s18 =1.2576
                        !       rs18=4.5865
                  case (XCF_XC_PBE0)
                        rs6 =0.4145_F64
                        s18 =1.2177_F64
                        rs18=4.8593_F64
                        ! case ("revpbe38")
                        !       rs6 =0.4309
                        !       s18 =1.4760
                        !       rs18=3.9446
                        ! case ("pw6b95")
                        !       rs6 =0.2076
                        !       s18 =0.7257
                        !       rs18=6.3750
                        ! case ("b2-plyp")
                        !       rs6 =0.3065
                        !       s18 =0.9147
                        !       rs18=5.0570
                        !       s6=0.64d0
                        ! case ("dsd-blyp")
                        !       rs6 =0.0000
                        !       s18 =0.2130
                        !       rs18=6.0519
                        !       s6=0.50d0
                        ! case ("dsd-blyp-fc")
                        !       rs6 =0.0009
                        !       s18 =0.2112
                        !       rs18=5.9807
                        !       s6=0.50d0
                        ! case ("bop")
                        !       rs6 =0.4870
                        !       s18 =3.2950
                        !       rs18=3.5043
                        ! case ("mpwlyp")
                        !       rs6 =0.4831
                        !       s18 =2.0077
                        !       rs18=4.5323
                        ! case ("o-lyp")
                        !       rs6 =0.5299
                        !       s18 =2.6205
                        !       rs18=2.8065
                        ! case ("pbesol")
                        !       rs6 =0.4466
                        !       s18 =2.9491
                        !       rs18=6.1742
                        ! case ("bpbe")
                        !       rs6 =0.4567
                        !       s18 =4.0728
                        !       rs18=4.3908
                        ! case ("opbe")
                        !       rs6 =0.5512
                        !       s18 =3.3816
                        !       rs18=2.9444
                        ! case ("ssb")
                        !       rs6 =-0.0952
                        !       s18 =-0.1744
                        !       rs18=5.2170
                        ! case ("revssb")
                        !       rs6 =0.4720
                        !       s18 =0.4389
                        !       rs18=4.0986
                        ! case ("otpss")
                        !       rs6 =0.4634
                        !       s18 =2.7495
                        !       rs18=4.3153
                  ! case (XCF_XC_B3PW91) !("b3pw91")
                  !       rs6 =0.4312_F64
                  !       s18 =2.8524_F64
                  !       rs18=4.4693_F64
                        ! case ("bh-lyp")
                        !       rs6 =0.2793
                        !       s18 =1.0354
                        !       rs18=4.9615
                        ! case ("revpbe0")
                        !       rs6 =0.4679
                        !       s18 =1.7588
                        !       rs18=3.7619
                  ! case (XCF_XC_TPSSH) !("tpssh")
                  !       rs6 =0.0000_F64
                  !       s18 =0.4243_F64
                  !       rs18=5.5253_F64
                        ! case ("mpw1b95")
                        !       rs6 =0.1955
                        !       s18 =1.0508
                        !       rs18=6.4177
                        ! case ("pwb6k")
                        !       rs6 =0.1805
                        !       s18 =0.9383
                        !       rs18=7.7627
                        ! case ("b1b95")
                        !       rs6 =0.2092
                        !       s18 =1.4507
                        !       rs18=5.5545
                        ! case ("bmk")
                        !       rs6 =0.1940
                        !       s18 =2.0860
                        !       rs18=5.9197
                        ! case ("cam-b3lyp")
                        !       rs6 =0.3708
                        !       s18 =2.0674
                        !       rs18=5.4743
                  ! case ("lc-wpbe")
                  !       rs6 =0.3919_F64
                  !       s18 =1.8541_F64
                  !       rs18=5.0897_F64
                        ! case ("b2gp-plyp")
                        !       rs6 =0.0000
                        !       s18 =0.2597
                        !       rs18=6.3332
                        !       s6=0.560
                        ! case ("ptpss")
                        !       rs6 =0.0000
                        !       s18 =0.2804
                        !       rs18=6.5745
                        !       s6=0.750
                        ! case ("pwpb95")
                        !       rs6 =0.0000
                        !       s18 =0.2904
                        !       rs18=7.3141
                        !       s6=0.820
                        !       ! special HF/DFT with eBSSE correction
                        ! case ("hf/mixed")
                        !       rs6 =0.5607
                        !       s18 =3.9027
                        !       rs18=4.5622
                        ! case ("hf/sv")
                        !       rs6 =0.4249
                        !       s18 =2.1849
                        !       rs18=4.2783
                        ! case ("hf/minis")
                        !       rs6 =0.1702
                        !       s18 =0.9841
                        !       rs18=3.8506
                        ! case ("b3-lyp/6-31gd")
                        !       rs6 =0.5014
                        !       s18 =4.0672
                        !       rs18=4.8409
                  ! case (XCF_XC_HCTH_120) !("hcth120")
                  !       rs6=0.3563_F64
                  !       s18=1.0821_F64
                  !       rs18=4.3359_F64
                  case DEFAULT
                        call msg("DFTD3 ERROR: NO PARAMETERS EXIST FOR REQUESTED FUNCTIONAL", &
                              priority=MSG_ERROR)
                        stop
                  end select
            endif

            ! DFT-D3
            if(version == 3)then
                  s6  =1.0d0
                  alp =14.0d0
                  rs18=1.0d0
                  ! default def2-QZVP (almost basis set limit)
                        select case (ifunc)
                              ! case ("slater-dirac-exchange")
                              !       rs6 =0.999
                              !       s18 =-1.957
                              !       rs18=0.697
                        case (XCF_XC_SCAN)
                              !
                              ! Parameters for SCAN taken from Table I in Brandenburg, J.G., Bates,
                              ! J.E., Sun, J., and Perdew, J.P. Phys. Rev. B 94,  115144 (2016);
                              ! doi: 10.1103/PhysRevB.94.115144
                              !
                              rs6 = 1.324_F64
                              s18 = 0.000_F64
                              
                        case (XCF_XC_BLYP)
                              rs6=1.094_F64
                              s18=1.682_F64
                              ! case ("b-p")
                              !       rs6=1.139
                              !       s18=1.683
                              ! case ("b97-d")
                              !       rs6=0.892
                              !       s18=0.909
                              ! case ("revpbe")
                              !       rs6=0.923
                              !       s18=1.010
                        case (XCF_XC_PBE) !("pbe")
                              rs6=1.217_F64
                              s18=0.722_F64
                              ! case ("pbesol")
                              !       rs6=1.345
                              !       s18=0.612
                              ! case ("rpw86-pbe")
                              !       rs6=1.224
                              !       s18=0.901
                              ! case ("rpbe")
                              !       rs6=0.872
                              !       s18=0.514
                        case (XCF_XC_TPSS) !("tpss")
                              rs6=1.166_F64
                              s18=1.105_F64
                        case (XCF_XC_B3LYP) !("b3-lyp")
                              rs6=1.261_F64
                              s18=1.703_F64
                        case (XCF_XC_PBE0) !("pbe0")
                              rs6=1.287_F64
                              s18=0.928_F64
                              ! case ("revpbe38")
                              !       rs6=1.021
                              !       s18=0.862
                              ! case ("pw6b95")
                              !       rs6=1.532
                              !       s18=0.862
                              ! case ("tpss0")
                              !       rs6=1.252
                              !       s18=1.242
                              ! case ("b2-plyp")
                              !       rs6=1.427
                              !       s18=1.022
                              !       s6=0.64
                              ! case ("pwpb95")
                              !       rs6=1.557
                              !       s18=0.705
                              !       s6=0.82
                              ! case ("b2gp-plyp")
                              !       rs6=1.586
                              !       s18=0.760
                              !       s6=0.56
                              ! case ("ptpss")
                              !       rs6=1.541
                              !       s18=0.879
                              !       s6=0.75
                        case (XCF_HF) !("hf")
                              rs6=1.158_F64
                              s18=1.746_F64
                              ! case ("mpwlyp")
                              !       rs6=1.239
                              !       s18=1.098
                              ! case ("bpbe")
                              !       rs6=1.087
                              !       s18=2.033
                              ! case ("bh-lyp")
                              !       rs6=1.370
                              !       s18=1.442
                        ! case (XCF_XC_TPSSH) !("tpssh")
                        !       rs6=1.223_F64
                        !       s18=1.219_F64
                              ! case ("pwb6k")
                              !       rs6=1.660
                              !       s18=0.550
                              ! case ("b1b95")
                              !       rs6=1.613
                              !       s18=1.868
                              ! case ("bop")
                              !       rs6=0.929
                              !       s18=1.975
                              ! case ("o-lyp")
                              !       rs6=0.806
                              !       s18=1.764
                              ! case ("o-pbe")
                              !       rs6=0.837
                              !       s18=2.055
                              ! case ("ssb")
                              !       rs6=1.215
                              !       s18=0.663
                              ! case ("revssb")
                              !       rs6=1.221
                              !       s18=0.560
                              ! case ("otpss")
                              !       rs6=1.128
                              !       s18=1.494
                        ! case (XCF_XC_B3PW91) !("b3pw91")
                        !       rs6=1.176_F64
                        !       s18=1.775_F64
                              ! case ("revpbe0")
                              !       rs6=0.949
                              !       s18=0.792
                              ! case ("pbe38")
                              !       rs6=1.333
                              !       s18=0.998
                              ! case ("mpw1b95")
                              !       rs6=1.605
                              !       s18=1.118
                              ! case ("mpwb1k")
                              !       rs6=1.671
                              !       s18=1.061
                              ! case ("bmk")
                              !       rs6=1.931
                              !       s18=2.168
                              ! case ("cam-b3lyp")
                              !       rs6=1.378
                              !       s18=1.217
                        case (XCF_XC_HJS_PBE)
                              !
                              ! Parameters for LC-wPBE of Vydrov et al.
                              !
                              rs6=1.355_F64
                              s18=1.279_F64
                        case (XCF_XC_MCS)
                              rs6 = 1.18219038_F64
                              s18 = 0.77403585_F64
                        case (XCF_XC_MCSH)
                              rs6 = 1.28996028_F64
                              s18 = 1.39963285_F64
                        case (XCF_XC_EC_PBE_TPSS)
                              !
                              ! Date: 23.12.2015
                              ! omega=0.350
                              ! srexx=0.000
                              ! Objective function: MAD on the S22 set
                              ! s8 constrained to zero during optimization
                              ! (unconstrained optimization resulted in s8 on the order of 10**(-5))
                              ! RMSD = 0.42 kcal/mol
                              ! MAD = 0.30 kcal/mol
                              ! MAPD = 6.89% 
                              ! MSD = -0.05 kcal/mol
                              ! 
                              rs6 = 0.88971_F64
                              s18 = 0.00000_F64
                        case (XCF_XC_M05) !("m05")
                              rs6=1.373_F64
                              s18=0.595_F64
                        case (XCF_XC_M052X) !("m052x")
                              rs6=1.417_F64
                              s18=0.000_F64
                        case (XCF_XC_M06L) !("m06l")
                              rs6=1.581_F64
                              s18=0.000_F64
                        case (XCF_XC_M06) !("m06")
                              rs6=1.325_F64
                              s18=0.000_F64
                        case (XCF_XC_M062X) !("m062x")
                              rs6=1.619_F64
                              s18=0.000_F64
                        case (XCF_XC_M06HF) !("m06hf")
                              rs6=1.446_F64
                              s18=0.000_F64
                              ! case ("dftb")
                              !       rs6=1.699
                              !       s18=1.504
                        ! case (XCF_XC_HCTH_120) !("hcth120")
                        !       rs6=1.221_F64
                        !       s18=1.206_F64
                        case DEFAULT
                              call msg("DFTD3 ERROR: NO PARAMETERS EXIST FOR REQUESTED FUNCTIONAL", &
                                    priority=MSG_ERROR)
                              stop
                        end select
            endif
      end subroutine setfuncpar


      subroutine edisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
            rs6,rs8,alp6,alp8,version,noabc,rthr, &
            e6,e8,e10,e12,e63)
            !
            ! Compute dispersion energy correction
            !
            integer, intent(in)                                    :: n
            real(F64), dimension(:, :), intent(in)          :: xyz
            integer, dimension(:), intent(in)                      :: iz
            real(F64), dimension(:, :, :, :, :), intent(in) :: c6ab
            real(F64), dimension(:), intent(in)             :: r2r4

            real(F64), dimension(:), allocatable :: cn
            integer :: max_elem,maxc,version,mxc(max_elem)
            real(F64) :: r0ab(max_elem,max_elem)
            real(F64) :: rs6,rs8,alp6,alp8,rcov(max_elem)
            real(F64) :: e6, e8, e10, e12, e63
            logical :: noabc

            integer :: iat,jat,kat
            real(F64) :: r,r2,r6,r8,tmp,dx,dy,dz,c6,c8,ang,rav
            real(F64) :: damp6,damp8,rr,c9,rthr
            real(F64), dimension(:), allocatable :: r2ab
            real(F64), dimension(:), allocatable :: cc6ab
            real(F64), dimension(:), allocatable :: dmp
            real(F64) :: d2(3),t1,t2,t3,a1,a2
            integer, dimension(:), allocatable :: icomp
            integer :: ij,ik,jk

            allocate(cn(n))
            allocate(r2ab(n * n))
            allocate(cc6ab(n * n))
            allocate(dmp(n * n))
            allocate(icomp(n * n))

            e6 =0.0_F64
            e8 =0.0_F64
            e10=0.0_F64
            e12=0.0_F64
            e63=0.0_F64

            !     Becke-Johnson parameters
            a1=rs6
            a2=rs8
            !
            ! DFT-D3
            !
            call ncoord(n, rcov, iz, xyz, cn)

            icomp=0
            do iat=1,n-1
                  do jat=iat+1,n
                        dx=xyz(1,iat)-xyz(1,jat)
                        dy=xyz(2,iat)-xyz(2,jat)
                        dz=xyz(3,iat)-xyz(3,jat)
                        r2=dx*dx+dy*dy+dz*dz
                        ! HR
                        if(r2 > rthr) cycle
                        r =sqrt(r2)
                        rr=r0ab(iz(jat),iz(iat))/r
                        ! damping
                        tmp=rs6*rr
                        damp6 =1.d0/( 1.d0+6.d0*tmp**alp6 )
                        tmp=rs8*rr
                        damp8 =1.d0/( 1.d0+6.d0*tmp**alp8 )
                        ! get C6
                        call getc6(maxc,max_elem,c6ab,mxc,iz(iat),iz(jat), &
                              cn(iat),cn(jat),c6)
                        if( .NOT. noabc)then
                              ij=lin(jat,iat)
                              icomp(ij)=1
                              ! store C6 for C9, calc as sqrt
                              cc6ab(ij)=sqrt(c6)
                              ! store R^2 for abc
                              r2ab(ij)=r2
                              ! store for abc damping
                              dmp(ij)=(1.d+0/rr)**(1.d+0/3.d+0)
                        endif

                        r6=r2**3
                        r8=r6*r2
                        ! r2r4 stored in main as sqrt
                        c8 =3.0d0*c6*r2r4(iz(iat))*r2r4(iz(jat))

                        ! DFT-D3 zero-damp
                        if(version == 3)then
                              e6=e6+c6*damp6/r6
                              e8=e8+c8*damp8/r8
                        endif
                        ! DFT-D3(BJ)
                        if(version == 4)then
                              ! use BJ radius
                              tmp=sqrt(c8/c6)
                              e6=e6+  c6/(r6+(a1*tmp+a2)**6)
                              e8=e8+  c8/(r8+(a1*tmp+a2)**8)
                              !              c10=(49.0d0/40.0d0)*c8**2/c6
                              !             e10=e10+c10/(r8*r2+(a1*tmp+a2)**10)
                        endif
                  enddo
            enddo

            if(noabc)return

            ! compute non-additive third-order energy using averaged C6
            do iat=1,n-1
                  do jat=iat+1,n
                        ij=lin(jat,iat)
                        if(icomp(ij) == 1)then
                              do kat=jat+1,n
                                    ik=lin(kat,iat)
                                    jk=lin(kat,jat)
                                    if(icomp(ik) == 0 .OR. icomp(jk) == 0) cycle
                                    ! damping func product
                                    !           tmp=dmp(ik)*dmp(jk)*dmp(ij)
                                    rav=(4./3.)/(dmp(ik)*dmp(jk)*dmp(ij))
                                    tmp=1.d0/( 1.d0+6.d0*rav**alp6 )
                                    ! triple C6 coefficient (stored as sqrt)
                                    c9=cc6ab(ij)*cc6ab(ik)*cc6ab(jk)
                                    !           write(*,*) 'C9', c9
                                    ! angular terms
                                    ! d is r^2
                                    d2(1)=r2ab(ij)
                                    d2(2)=r2ab(jk)
                                    d2(3)=r2ab(ik)
                                    t1 = (d2(1)+d2(2)-d2(3))/sqrt(d2(1)*d2(2))
                                    t2 = (d2(1)+d2(3)-d2(2))/sqrt(d2(1)*d2(3))
                                    t3 = (d2(3)+d2(2)-d2(1))/sqrt(d2(2)*d2(3))
                                    ang=0.375d0*t1*t2*t3+1.0d0

                                    ! C9 has negative sign
                                    e63=e63-tmp*c9*ang/(d2(1)*d2(2)*d2(3))**1.50d0

                              enddo
                        endif
                  enddo
            enddo

            deallocate(cn)
            deallocate(r2ab)
            deallocate(cc6ab)
            deallocate(dmp)
            deallocate(icomp)
      end subroutine edisp

      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ! compute gradient
      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine gdisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
            s6,s18,rs6,rs8,alp6,alp8,noabc,rthr, &
            num,version,echo,g,disp,rthr2)

            integer, intent(in)                                    :: n
            real(F64), dimension(:, :), intent(inout)       :: xyz
            integer, dimension(:), intent(in)                      :: iz
            real(F64), dimension(:, :, :, :, :), intent(in) :: c6ab
            real(F64), dimension(:), intent(in)             :: r2r4
            real(F64), dimension(:, :), intent(out)         :: g

            real(F64), dimension(:), allocatable :: cn
            integer :: max_elem,maxc,version,mxc(max_elem)
            real(F64) :: r0ab(max_elem,max_elem)
            real(F64) :: s6,s18,rcov(max_elem)
            real(F64) :: rs6,rs8,alp8,alp6,a1,a2
            logical :: noabc,num,echo

            integer :: iat,jat,i,j,kat
            real(F64) :: R0,C6,R42,disp,x1,y1,z1,x2,y2,z2,rr,e6abc
            real(F64) :: dx,dy,dz,r2,r,r4,r6,r8,r10,t6,t8
            real(F64) :: damp6,damp8,e6,e8,e10,e12
            real(F64) :: s10,s8,gC6(3),term,step,dispr,displ
            real(F64) :: rthr, rthr2, rthr3
            real(F64), dimension(:, :), allocatable :: dcn2
            real(F64), dimension(:, :, :), allocatable :: dcn3

            allocate(cn(n))
            allocate(dcn2(3, n))
            allocate(dcn3(3, n, n))

            ! this is the crucial threshold to reduce the N^3 to an
            ! effective N^2.

            rthr3=rthr2

            if(num) then
                  if(echo)write(*,*) 'doing numerical gradient O(N^3) ...'

                  call edisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
                        rs6,rs8,alp6,alp8,version,noabc,rthr, &
                        e6,e8,e10,e12,e6abc)
                  disp=-s6*e6-s18*e8-s6*e6abc

                  step=2.d-5

                  do i=1,n
                        do j=1,3
                              xyz(j,i)=xyz(j,i)+step
                              call edisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
                                    rs6,rs8,alp6,alp8,version,noabc,rthr, &
                                    e6,e8,e10,e12,e6abc)
                              dispr=-s6*e6-s18*e8-s6*e6abc
                              xyz(j,i)=xyz(j,i)-2*step
                              call edisp(max_elem,maxc,n,xyz,iz,c6ab,mxc,r2r4,r0ab,rcov, &
                                    rs6,rs8,alp6,alp8,version,noabc,rthr, &
                                    e6,e8,e10,e12,e6abc)
                              displ=-s6*e6-s18*e8-s6*e6abc
                              g(j,i)=0.5d+0*(dispr-displ)/step
                              xyz(j,i)=xyz(j,i)+step
                        enddo
                  enddo

            else

                  if(echo)write(*,*) 'doing analytical gradient O(N^3) ...'
                  ! precompute for analytical part
                  call ncoorda(n,rcov,iz,xyz,cn,dcn2,dcn3)

                  ! 333333333333333333333333333333333333333333333333333333333333333333333333333
                  ! standard correction
                  if (version == 3) then
                        s8 =s18
                        s10=s18

                        disp=0

                        do iat=1,n
                              x1=xyz(1,iat)
                              y1=xyz(2,iat)
                              z1=xyz(3,iat)
                              do jat=1,n
                                    if(iat == jat) cycle
                                    x2=xyz(1,jat)
                                    y2=xyz(2,jat)
                                    z2=xyz(3,jat)
                                    dx = (x1-x2)**2
                                    dy = (y1-y2)**2
                                    dz = (z1-z2)**2
                                    r2 = dx+dy+dz
                                    ! HR
                                    if(r2 > rthr) cycle

                                    R0=r0ab(iz(jat),iz(iat))
                                    ! stored as sqrt
                                    r42=r2r4(iz(iat))*r2r4(iz(jat))
                                    call getc6(maxc,max_elem,c6ab,mxc,iz(iat),iz(jat), &
                                          cn(iat),cn(jat),C6)

                                    ! analytically
                                    call anagrdc6(max_elem,maxc,n,cn,dcn2,dcn3,iz,c6ab,mxc, &
                                          iat,jat,iat,gC6)

                                    r = dsqrt(r2)
                                    t6 = (r/(rs6*R0))**(-alp6)
                                    damp6 =1.d0/( 1.d0+6.d0*t6 )
                                    t8 = (r/(rs8*R0))**(-alp8)
                                    damp8 =1.d0/( 1.d0+6.d0*t8 )

                                    r4 = r2**2
                                    r6 = r2*r4
                                    r8 = r4**2
                                    r10 = r4*r6
                                    !     r12 = r6**2

                                    dx = 2.D0*x1-2.D0*x2

                                    term = -3.D0*damp6*damp6*s6*C6/r8*t6*alp6*dx &
                                          -1.D0*damp6*s6*gC6(1)/r6 &
                                          +3.D0*damp6*s6*C6/r8*dx &
                                          -9.D0*damp8*damp8*s8*C6*R42/r10*t8*alp8*dx &
                                          -3.D0*damp8*s8*gC6(1)*R42/r8 &
                                          +12.D0*damp8*s8*C6*R42/r10*dx
                                    !    &       -11.025D0*gC6(1)*R42**2*damp10*s10/r10
                                    !    &       -33.075D0*C6*R42**2*damp10*damp10*s10/r12*t10*alp10*dx
                                    !    &       +55.125D0*C6*R42**2*damp10*s10/r12*dx
                                    g(1,iat)=g(1,iat)+term

                                    dy = 2.D0*y1-2.D0*y2

                                    term = -3.D0*damp6*damp6*s6*C6/r8*t6*alp6*dy &
                                          -1.D0*damp6*s6*gC6(2)/r6 &
                                          +3.D0*damp6*s6*C6/r8*dy &
                                          -9.D0*damp8*damp8*s8*C6*R42/r10*t8*alp8*dy &
                                          -3.D0*damp8*s8*gC6(2)*R42/r8 &
                                          +12.D0*damp8*s8*C6*R42/r10*dy
                                    !    &       -11.025D0*gC6(2)*R42**2*damp10*s10/r10
                                    !    &       -33.075D0*C6*R42**2*damp10*damp10*s10/r12*t10*alp10*dy
                                    !    &       +55.125D0*C6*R42**2*damp10*s10/r12*dy
                                    g(2,iat)=g(2,iat)+term

                                    dz = 2.D0*z1-2.D0*z2

                                    term = -3.D0*damp6*damp6*s6*C6/r8*t6*alp6*dz &
                                          -1.D0*damp6*s6*gC6(3)/r6 &
                                          +3.D0*damp6*s6*C6/r8*dz &
                                          -9.D0*damp8*damp8*s8*C6*R42/r10*t8*alp8*dz &
                                          -3.D0*damp8*s8*gC6(3)*R42/r8 &
                                          +12.D0*damp8*s8*C6*R42/r10*dz
                                    !    &       -11.025D0*gC6(3)*R42**2*damp10*s10/r10
                                    !    &       -33.075D0*C6*R42**2*damp10*damp10*s10/r12*t10*alp10*dz
                                    !    &       +55.125D0*C6*R42**2*damp10*s10/r12*dz
                                    g(3,iat)=g(3,iat)+term

                                    term = -1.D0/(1.D0+6.D0*t6)*s6*C6/r6 &
                                          -3.D0/(1.D0+6.D0*t8)*s8*C6*R42/r8
                                    !    &       -11.025D0*C6*R42**2/(1.D0+6.D0*t10)*s10/r10
                                    if(iat < jat)then
                                          disp=disp+term
                                    endif

                              enddo

                              do jat=2,n
                                    if(iat == jat) cycle
                                    x1=xyz(1,jat)
                                    y1=xyz(2,jat)
                                    z1=xyz(3,jat)
                                    ! HR IJ MOST IMPORTANT
                                    if( (xyz(1,iat)-x1)**2+ &
                                          (xyz(2,iat)-y1)**2+ &
                                          (xyz(3,iat)-z1)**2 > rthr2* &
                                          r0ab(iz(iat),iz(jat))) cycle


                                    do kat=1,jat-1
                                          if(iat == kat) cycle
                                          x2=xyz(1,kat)
                                          y2=xyz(2,kat)
                                          z2=xyz(3,kat)
                                          dx = (x1-x2)**2
                                          dy = (y1-y2)**2
                                          dz = (z1-z2)**2
                                          r2 = dx+dy+dz

                                          ! HR IK
                                          if( (xyz(1,kat)-xyz(1,iat))**2+ &
                                                (xyz(2,kat)-xyz(2,iat))**2+ &
                                                (xyz(3,kat)-xyz(3,iat))**2 &
                                                > rthr2*r0ab(iz(iat),iz(kat))) cycle
                                          ! HR JK
                                          if(r2 > rthr3*r0ab(iz(jat),iz(kat))) cycle


                                          R0=r0ab(iz(kat),iz(jat))
                                          R42=r2r4(iz(jat))*r2r4(iz(kat))
                                          ! analytically
                                          call anagrdc6(max_elem,maxc,n,cn,dcn2,dcn3,iz,c6ab,mxc, &
                                                jat,kat,iat,gC6)

                                          r = dsqrt(r2)
                                          rr=R0/r
                                          t6 = (rr*rs6)**alp6
                                          damp6 =1.d0/( 1.d0+6.d0*t6 )
                                          t8 = (rr*rs8)**alp8
                                          damp8 =1.d0/( 1.d0+6.d0*t8 )
                                          !     t10 = (rr*rs10)**alp10
                                          !     damp10=1.d0/( 1.d0+6.d0*t10 )

                                          r4 = r2**2
                                          r6 = r2*r4
                                          r8 = r4**2
                                          !     r10 = r4*r6
                                          !     r12 = r6**2

                                          term = -1.D0*damp6*s6*gC6(1)/r6 &
                                                -3.D0*damp8*s8*gC6(1)*R42/r8
                                          !    &       -11.025D0*gC6(1)*R42**2*damp10*s10/r10
                                          g(1,iat)=g(1,iat)+term

                                          term = -1.D0*damp6*s6*gC6(2)/r6 &
                                                -3.D0*damp8*s8*gC6(2)*R42/r8
                                          !    &       -11.025D0*gC6(2)*R42**2*damp10*s10/r10
                                          g(2,iat)=g(2,iat)+term

                                          term = -1.D0*damp6*s6*gC6(3)/r6 &
                                                -3.D0*damp8*s8*gC6(3)*R42/r8
                                          !    &       -11.025D0*gC6(3)*R42**2*damp10*s10/r10
                                          g(3,iat)=g(3,iat)+term

                                    enddo
                              enddo

                        enddo
                  endif

                  ! BJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJ
                  ! Becke-Johnson finite damping
                  if (version == 4) then
                        a1 =rs6
                        a2 =rs8
                        s8 =s18

                        disp=0

                        do iat=1,n
                              x1=xyz(1,iat)
                              y1=xyz(2,iat)
                              z1=xyz(3,iat)
                              do jat=1,n
                                    if(iat == jat) cycle
                                    x2=xyz(1,jat)
                                    y2=xyz(2,jat)
                                    z2=xyz(3,jat)
                                    dx = (x1-x2)**2
                                    dy = (y1-y2)**2
                                    dz = (z1-z2)**2
                                    r2 = dx+dy+dz
                                    ! HR
                                    if(r2 > rthr) cycle
                                    call getc6(maxc,max_elem,c6ab,mxc,iz(iat),iz(jat), &
                                          cn(iat),cn(jat),C6)
                                    ! stored as sqrt
                                    r42=r2r4(iz(iat))*r2r4(iz(jat))
                                    ! use BJ radius
                                    R0=a1*sqrt(3.0d0*r42)+a2
                                    ! analytically
                                    call anagrdc6(max_elem,maxc,n,cn,dcn2,dcn3,iz,c6ab,mxc, &
                                          iat,jat,iat,gC6)

                                    r = dsqrt(r2)

                                    r4 = r2**2
                                    r6 = r2*r4
                                    r8 = r4**2
                                    r10 = r4*r6
                                    !     r12 = r6**2
                                    t6=(r6+R0**6)
                                    t8=(r8+R0**8)

                                    dx = 2.D0*x1-2.D0*x2

                                    term = 3.D0*s6*C6*r4*dx/t6**2 &
                                          -1.D0*s6*gC6(1)/t6 &
                                          +12.D0*C6*R42*s8*r6*dx/t8**2 &
                                          -3.D0*gC6(1)*R42*s8/t8

                                    g(1,iat)=g(1,iat)+term

                                    dy = 2.D0*y1-2.D0*y2

                                    term = 3.D0*s6*C6*r4*dy/t6**2 &
                                          -1.D0*s6*gC6(2)/t6 &
                                          +12.D0*C6*R42*s8*r6*dy/t8**2 &
                                          -3.D0*gC6(2)*R42*s8/t8

                                    g(2,iat)=g(2,iat)+term

                                    dz = 2.D0*z1-2.D0*z2

                                    term = 3.D0*s6*C6*r4*dz/t6**2 &
                                          -1.D0*s6*gC6(3)/t6 &
                                          +12.D0*C6*R42*s8*r6*dz/t8**2 &
                                          -3.D0*gC6(3)*R42*s8/t8

                                    g(3,iat)=g(3,iat)+term

                                    term = -1.D0*s6*C6/t6 &
                                          -3.D0*s8*C6*R42/t8
                                    if(iat < jat)then
                                          disp=disp+term
                                    endif
                              enddo

                              do jat=2,n
                                    if(iat == jat) cycle
                                    x1=xyz(1,jat)
                                    y1=xyz(2,jat)
                                    z1=xyz(3,jat)
                                    ! HR IJ MOST IMPORTANT
                                    if( (xyz(1,iat)-x1)**2+ &
                                          (xyz(2,iat)-y1)**2+ &
                                          (xyz(3,iat)-z1)**2 > rthr2* &
                                          r0ab(iz(iat),iz(jat))) cycle


                                    do kat=1,jat-1
                                          if(iat == kat) cycle
                                          x2=xyz(1,kat)
                                          y2=xyz(2,kat)
                                          z2=xyz(3,kat)
                                          dx = (x1-x2)**2
                                          dy = (y1-y2)**2
                                          dz = (z1-z2)**2
                                          r2 = dx+dy+dz
                                          ! HR JK
                                          if(r2 > rthr3*r0ab(iz(iat),iz(jat))) cycle
                                          ! HR IK
                                          if( (xyz(1,kat)-xyz(1,iat))**2+ &
                                                (xyz(2,kat)-xyz(2,iat))**2+ &
                                                (xyz(3,kat)-xyz(3,iat))**2 &
                                                > rthr2*r0ab(iz(iat),iz(kat))) cycle


                                          R42=r2r4(iz(jat))*r2r4(iz(kat))
                                          ! use BJ radius
                                          R0=a1*sqrt(3.0d0*R42)+a2
                                          ! analytically
                                          call anagrdc6(max_elem,maxc,n,cn,dcn2,dcn3,iz,c6ab,mxc, &
                                                jat,kat,iat,gC6)
                                          !           if(sum(abs(gC6)).lt.thr3)cycle

                                          r = dsqrt(r2)

                                          r4 = r2**2
                                          r6 = r2*r4
                                          r8 = r4**2
                                          !     r10 = r4*r6
                                          !     r12 = r6**2

                                          t6=(r6+R0**6)
                                          t8=(r8+R0**8)
                                          s8=s18
                                          term =-1.D0*s6*gC6(1)/t6 &
                                                -3.D0*gC6(1)*R42*s8/t8

                                          g(1,iat)=g(1,iat)+term

                                          term =-1.D0*s6*gC6(2)/t6 &
                                                -3.D0*gC6(2)*R42*s8/t8

                                          g(2,iat)=g(2,iat)+term

                                          term =-1.D0*s6*gC6(3)/t6 &
                                                -3.D0*gC6(3)*R42*s8/t8

                                          g(3,iat)=g(3,iat)+term
                                    enddo
                              enddo
                        enddo
                  endif
            endif

            deallocate(cn)
            deallocate(dcn2)
            deallocate(dcn3)
      end subroutine gdisp

      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ! calculate dC6/dr analytically
      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine anagrdc6(max_elem,maxc,n,cn,dcn2,dcn3, &
            iz,c6ab,mxc,iat,jat,kat,anag)
            integer ::    n,iz(*),max_elem,maxc,iat,jat,kat,mxc(max_elem)
            real(F64) ::     cn(*),dcn2(3,n),anag(3)
            real(F64) ::     c6ab(max_elem,max_elem,maxc,maxc,3)
            real(F64) ::     term1,term2,term3,term4
            real(F64) ::     dterm2(3),dterm3(3),dcn3(3,n,n)
            real(F64) ::     zaehler,nenner,dzaehler(3),dnenner(3)
            integer ::    i,j,k

            if (iat == kat) then
                  dterm2=dcn2(:,iat)
                  dterm3=dcn3(:,iat,jat)
            else
                  dterm2=dcn3(:,kat,iat)
                  dterm3=dcn3(:,kat,jat)
            endif
            zaehler=0.0d0
            nenner=0.0d0
            dzaehler=0.0d0
            dnenner=0.0d0
            do i=1,mxc(iz(iat))
                  do j=1,mxc(iz(jat))
                        term3=c6ab(iz(iat),iz(jat),i,j,3)-cn(jat)
                        term2=c6ab(iz(iat),iz(jat),i,j,2)-cn(iat)
                        term1=exp(k3*(term2*term2+term3*term3))
                        zaehler=zaehler+c6ab(iz(iat),iz(jat),i,j,1)*term1
                        nenner=nenner+term1
                        term4=term1*k3*2.0d0
                        do k=1,3
                              dzaehler(k)=dzaehler(k)+c6ab(iz(iat),iz(jat),i,j,1)*term4 &
                                    *(term2*dterm2(k)+term3*dterm3(k))
                              dnenner(k)=dnenner(k)+term4 &
                                    *(term2*dterm2(k)+term3*dterm3(k))
                        enddo
                  enddo
            enddo
            if (nenner > 1.0d-99) then
                  term4=1.0d0/(nenner*nenner)
                  do k=1,3
                        anag(k)=(dzaehler(k)*nenner-dnenner(k)*zaehler)*term4
                  enddo
            else
                  anag=0.0d0
            endif
      end subroutine anagrdc6


      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ! ncoord derivative
      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine ncoorda(natoms,rcov,iz,xyz,cn,dcn2,dcn3)
            integer, dimension(:), intent(in)                      :: iz
            real(F64), dimension(:, :), intent(in)          :: xyz
            real(F64), dimension(:), intent(out)             :: cn

            integer :: natoms,i
            real(F64) :: dcn2(3,natoms),rcov(94)
            real(F64) :: dcn3(3,natoms,natoms)
            integer :: iat
            real(F64) :: dx,dy,dz,r,xn,rr,rrr,rco,tmp1,tmp2,tmp3

            dcn2=0.0d0
            dcn3=0.0d0
            do i=1,natoms
                  xn=0.0d0
                  do iat=1,natoms
                        if(iat /= i)then
                              dx=xyz(1,iat)-xyz(1,i)
                              dy=xyz(2,iat)-xyz(2,i)
                              dz=xyz(3,iat)-xyz(3,i)
                              r=sqrt(dx*dx+dy*dy+dz*dz)
                              rco=rcov(iz(i))+rcov(iz(iat))
                              rr=rco/r
                              rrr=1.0d0/(r*r*r)

                              tmp1=exp(-k1*(rr-1.0d0))
                              tmp2=1.0d0/(tmp1+1.0d0)
                              tmp3=tmp1*tmp2*tmp2*k1*rco*rrr

                              xn=xn+tmp2
                              dcn3(1,iat,i)=tmp3*dx
                              dcn3(2,iat,i)=tmp3*dy
                              dcn3(3,iat,i)=tmp3*dz
                              dcn2(1,i)=dcn2(1,i)+tmp3*dx
                              dcn2(2,i)=dcn2(2,i)+tmp3*dy
                              dcn2(3,i)=dcn2(3,i)+tmp3*dz
                        endif
                  enddo
                  cn(i)=xn
            enddo
            dcn2=-1.0d0*dcn2
      end subroutine ncoorda

      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ! interpolate c6
      ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine getc6(maxc,max_elem,c6ab,mxc,iat,jat,nci,ncj,c6)
            integer :: maxc,max_elem
            integer :: iat,jat,i,j,mxc(max_elem)
            real(F64) ::  nci,ncj,c6,c6mem
            real(F64) ::  c6ab(max_elem,max_elem,maxc,maxc,3)
            ! the exponential is sensitive to numerics
            ! when nci or ncj is much larger than cn1/cn2
            real(F64) ::  cn1,cn2,r,rsum,csum,tmp1

            c6mem=-1.d+99
            rsum=0.0
            csum=0.0
            c6  =0.0
            do i=1,mxc(iat)
                  do j=1,mxc(jat)
                        c6=c6ab(iat,jat,i,j,1)
                        if(c6 > 0)then
                              c6mem=c6
                              cn1=c6ab(iat,jat,i,j,2)
                              cn2=c6ab(iat,jat,i,j,3)
                              ! distance
                              r=(cn1-nci)**2+(cn2-ncj)**2
                              tmp1=exp(k3*r)
                              rsum=rsum+tmp1
                              csum=csum+tmp1*c6
                        endif
                  enddo
            enddo

            if(rsum > 0)then
                  c6=csum/rsum
            else
                  c6=c6mem
            endif

      end subroutine getc6


      subroutine ncoord(natoms, rcov, iz, xyz, cn)
            !
            ! Compute coordination numbers by adding 
            ! an inverse damping function
            !
            integer, intent(in)                           :: natoms
            real(F64), dimension(:), intent(in)    :: rcov
            integer, dimension(:), intent(in)             :: iz
            real(F64), dimension(:, :), intent(in) :: xyz
            real(F64), dimension(:), intent(out)   :: cn

            integer :: i, iat
            real(F64) :: dx,dy,dz,r,damp,xn,rr,rco,r2

            do i=1,natoms
                  xn=0.0d0
                  do iat=1,natoms
                        if(iat /= i)then
                              dx=xyz(1,iat)-xyz(1,i)
                              dy=xyz(2,iat)-xyz(2,i)
                              dz=xyz(3,iat)-xyz(3,i)
                              r2=dx*dx+dy*dy+dz*dz
                              r=sqrt(r2)
                              !
                              ! Covalent distance in Bohr
                              !
                              rco=rcov(iz(i))+rcov(iz(iat))
                              rr=rco/r
                              !
                              ! Exponential function has better long-range
                              ! behavior than MHGs inverse damping
                              !
                              damp=1.d0/(1.d0+exp(-k1*(rr-1.0d0)))
                              xn=xn+damp
                        endif
                  enddo
                  cn(i)=xn
            enddo
      end subroutine ncoord


      function lin(i1,i2)
            integer :: lin
            integer :: i1,i2,idum1,idum2

            idum1=max(i1,i2)
            idum2=min(i1,i2)
            lin=idum2+idum1*(idum1-1)/2
      end function lin


      subroutine setr0ab(max_elem,autoang,r)
            !
            ! Cut-off radii
            ! Array divided into chunks due to Intel compiler bug
            !
            integer :: max_elem,i,j,k
            real(F64) :: r(max_elem,max_elem),autoang
            real(F64) :: r0ab(4465)
            r0ab(   1:  70)=(/ &
                  2.1823d+0,  1.8547d+0,  1.7347d+0,  2.9086d+0,  2.5732d+0,  3.4956d+0,  2.3550d+0, &
                  2.5095d+0,  2.9802d+0,  3.0982d+0,  2.5141d+0,  2.3917d+0,  2.9977d+0,  2.9484d+0, &
                  3.2160d+0,  2.4492d+0,  2.2527d+0,  3.1933d+0,  3.0214d+0,  2.9531d+0,  2.9103d+0, &
                  2.3667d+0,  2.1328d+0,  2.8784d+0,  2.7660d+0,  2.7776d+0,  2.7063d+0,  2.6225d+0, &
                  2.1768d+0,  2.0625d+0,  2.6395d+0,  2.6648d+0,  2.6482d+0,  2.5697d+0,  2.4846d+0, &
                  2.4817d+0,  2.0646d+0,  1.9891d+0,  2.5086d+0,  2.6908d+0,  2.6233d+0,  2.4770d+0, &
                  2.3885d+0,  2.3511d+0,  2.2996d+0,  1.9892d+0,  1.9251d+0,  2.4190d+0,  2.5473d+0, &
                  2.4994d+0,  2.4091d+0,  2.3176d+0,  2.2571d+0,  2.1946d+0,  2.1374d+0,  2.9898d+0, &
                  2.6397d+0,  3.6031d+0,  3.1219d+0,  3.7620d+0,  3.2485d+0,  2.9357d+0,  2.7093d+0, &
                  2.5781d+0,  2.4839d+0,  3.7082d+0,  2.5129d+0,  2.7321d+0,  3.1052d+0,  3.2962d+0 &
                  /)
            r0ab(  71: 140)=(/ &
                  3.1331d+0,  3.2000d+0,  2.9586d+0,  3.0822d+0,  2.8582d+0,  2.7120d+0,  3.2570d+0, &
                  3.4839d+0,  2.8766d+0,  2.7427d+0,  3.2776d+0,  3.2363d+0,  3.5929d+0,  3.2826d+0, &
                  3.0911d+0,  2.9369d+0,  2.9030d+0,  2.7789d+0,  3.3921d+0,  3.3970d+0,  4.0106d+0, &
                  2.8884d+0,  2.6605d+0,  3.7513d+0,  3.1613d+0,  3.3605d+0,  3.3325d+0,  3.0991d+0, &
                  2.9297d+0,  2.8674d+0,  2.7571d+0,  3.8129d+0,  3.3266d+0,  3.7105d+0,  3.7917d+0, &
                  2.8304d+0,  2.5538d+0,  3.3932d+0,  3.1193d+0,  3.1866d+0,  3.1245d+0,  3.0465d+0, &
                  2.8727d+0,  2.7664d+0,  2.6926d+0,  3.4608d+0,  3.2984d+0,  3.5142d+0,  3.5418d+0, &
                  3.5017d+0,  2.6190d+0,  2.4797d+0,  3.1331d+0,  3.0540d+0,  3.0651d+0,  2.9879d+0, &
                  2.9054d+0,  2.8805d+0,  2.7330d+0,  2.6331d+0,  3.2096d+0,  3.5668d+0,  3.3684d+0, &
                  3.3686d+0,  3.3180d+0,  3.3107d+0,  2.4757d+0,  2.4019d+0,  2.9789d+0,  3.1468d+0 &
                  /)
            r0ab( 141: 210)=(/ &
                  2.9768d+0,  2.8848d+0,  2.7952d+0,  2.7457d+0,  2.6881d+0,  2.5728d+0,  3.0574d+0, &
                  3.3264d+0,  3.3562d+0,  3.2529d+0,  3.1916d+0,  3.1523d+0,  3.1046d+0,  2.3725d+0, &
                  2.3289d+0,  2.8760d+0,  2.9804d+0,  2.9093d+0,  2.8040d+0,  2.7071d+0,  2.6386d+0, &
                  2.5720d+0,  2.5139d+0,  2.9517d+0,  3.1606d+0,  3.2085d+0,  3.1692d+0,  3.0982d+0, &
                  3.0352d+0,  2.9730d+0,  2.9148d+0,  3.2147d+0,  2.8315d+0,  3.8724d+0,  3.4621d+0, &
                  3.8823d+0,  3.3760d+0,  3.0746d+0,  2.8817d+0,  2.7552d+0,  2.6605d+0,  3.9740d+0, &
                  3.6192d+0,  3.6569d+0,  3.9586d+0,  3.6188d+0,  3.3917d+0,  3.2479d+0,  3.1434d+0, &
                  4.2411d+0,  2.7597d+0,  3.0588d+0,  3.3474d+0,  3.6214d+0,  3.4353d+0,  3.4729d+0, &
                  3.2487d+0,  3.3200d+0,  3.0914d+0,  2.9403d+0,  3.4972d+0,  3.7993d+0,  3.6773d+0, &
                  3.8678d+0,  3.5808d+0,  3.8243d+0,  3.5826d+0,  3.4156d+0,  3.8765d+0,  4.1035d+0 &
                  /)
            r0ab( 211: 280)=(/ &
                  2.7361d+0,  2.9765d+0,  3.2475d+0,  3.5004d+0,  3.4185d+0,  3.4378d+0,  3.2084d+0, &
                  3.2787d+0,  3.0604d+0,  2.9187d+0,  3.4037d+0,  3.6759d+0,  3.6586d+0,  3.8327d+0, &
                  3.5372d+0,  3.7665d+0,  3.5310d+0,  3.3700d+0,  3.7788d+0,  3.9804d+0,  3.8903d+0, &
                  2.6832d+0,  2.9060d+0,  3.2613d+0,  3.4359d+0,  3.3538d+0,  3.3860d+0,  3.1550d+0, &
                  3.2300d+0,  3.0133d+0,  2.8736d+0,  3.4024d+0,  3.6142d+0,  3.5979d+0,  3.5295d+0, &
                  3.4834d+0,  3.7140d+0,  3.4782d+0,  3.3170d+0,  3.7434d+0,  3.9623d+0,  3.8181d+0, &
                  3.7642d+0,  2.6379d+0,  2.8494d+0,  3.1840d+0,  3.4225d+0,  3.2771d+0,  3.3401d+0, &
                  3.1072d+0,  3.1885d+0,  2.9714d+0,  2.8319d+0,  3.3315d+0,  3.5979d+0,  3.5256d+0, &
                  3.4980d+0,  3.4376d+0,  3.6714d+0,  3.4346d+0,  3.2723d+0,  3.6859d+0,  3.8985d+0, &
                  3.7918d+0,  3.7372d+0,  3.7211d+0,  2.9230d+0,  2.6223d+0,  3.4161d+0,  2.8999d+0 &
                  /)
            r0ab( 281: 350)=(/ &
                  3.0557d+0,  3.3308d+0,  3.0555d+0,  2.8508d+0,  2.7385d+0,  2.6640d+0,  3.5263d+0, &
                  3.0277d+0,  3.2990d+0,  3.7721d+0,  3.5017d+0,  3.2751d+0,  3.1368d+0,  3.0435d+0, &
                  3.7873d+0,  3.2858d+0,  3.2140d+0,  3.1727d+0,  3.2178d+0,  3.4414d+0,  2.5490d+0, &
                  2.7623d+0,  3.0991d+0,  3.3252d+0,  3.1836d+0,  3.2428d+0,  3.0259d+0,  3.1225d+0, &
                  2.9032d+0,  2.7621d+0,  3.2490d+0,  3.5110d+0,  3.4429d+0,  3.3845d+0,  3.3574d+0, &
                  3.6045d+0,  3.3658d+0,  3.2013d+0,  3.6110d+0,  3.8241d+0,  3.7090d+0,  3.6496d+0, &
                  3.6333d+0,  3.0896d+0,  3.5462d+0,  2.4926d+0,  2.7136d+0,  3.0693d+0,  3.2699d+0, &
                  3.1272d+0,  3.1893d+0,  2.9658d+0,  3.0972d+0,  2.8778d+0,  2.7358d+0,  3.2206d+0, &
                  3.4566d+0,  3.3896d+0,  3.3257d+0,  3.2946d+0,  3.5693d+0,  3.3312d+0,  3.1670d+0, &
                  3.5805d+0,  3.7711d+0,  3.6536d+0,  3.5927d+0,  3.5775d+0,  3.0411d+0,  3.4885d+0 &
                  /)
            r0ab( 351: 420)=(/ &
                  3.4421d+0,  2.4667d+0,  2.6709d+0,  3.0575d+0,  3.2357d+0,  3.0908d+0,  3.1537d+0, &
                  2.9235d+0,  3.0669d+0,  2.8476d+0,  2.7054d+0,  3.2064d+0,  3.4519d+0,  3.3593d+0, &
                  3.2921d+0,  3.2577d+0,  3.2161d+0,  3.2982d+0,  3.1339d+0,  3.5606d+0,  3.7582d+0, &
                  3.6432d+0,  3.5833d+0,  3.5691d+0,  3.0161d+0,  3.4812d+0,  3.4339d+0,  3.4327d+0, &
                  2.4515d+0,  2.6338d+0,  3.0511d+0,  3.2229d+0,  3.0630d+0,  3.1265d+0,  2.8909d+0, &
                  3.0253d+0,  2.8184d+0,  2.6764d+0,  3.1968d+0,  3.4114d+0,  3.3492d+0,  3.2691d+0, &
                  3.2320d+0,  3.1786d+0,  3.2680d+0,  3.1036d+0,  3.5453d+0,  3.7259d+0,  3.6090d+0, &
                  3.5473d+0,  3.5327d+0,  3.0018d+0,  3.4413d+0,  3.3907d+0,  3.3593d+0,  3.3462d+0, &
                  2.4413d+0,  2.6006d+0,  3.0540d+0,  3.1987d+0,  3.0490d+0,  3.1058d+0,  2.8643d+0, &
                  2.9948d+0,  2.7908d+0,  2.6491d+0,  3.1950d+0,  3.3922d+0,  3.3316d+0,  3.2585d+0 &
                  /)
            r0ab( 421: 490)=(/ &
                  3.2136d+0,  3.1516d+0,  3.2364d+0,  3.0752d+0,  3.5368d+0,  3.7117d+0,  3.5941d+0, &
                  3.5313d+0,  3.5164d+0,  2.9962d+0,  3.4225d+0,  3.3699d+0,  3.3370d+0,  3.3234d+0, &
                  3.3008d+0,  2.4318d+0,  2.5729d+0,  3.0416d+0,  3.1639d+0,  3.0196d+0,  3.0843d+0, &
                  2.8413d+0,  2.7436d+0,  2.7608d+0,  2.6271d+0,  3.1811d+0,  3.3591d+0,  3.3045d+0, &
                  3.2349d+0,  3.1942d+0,  3.1291d+0,  3.2111d+0,  3.0534d+0,  3.5189d+0,  3.6809d+0, &
                  3.5635d+0,  3.5001d+0,  3.4854d+0,  2.9857d+0,  3.3897d+0,  3.3363d+0,  3.3027d+0, &
                  3.2890d+0,  3.2655d+0,  3.2309d+0,  2.8502d+0,  2.6934d+0,  3.2467d+0,  3.1921d+0, &
                  3.5663d+0,  3.2541d+0,  3.0571d+0,  2.9048d+0,  2.8657d+0,  2.7438d+0,  3.3547d+0, &
                  3.3510d+0,  3.9837d+0,  3.6871d+0,  3.4862d+0,  3.3389d+0,  3.2413d+0,  3.1708d+0, &
                  3.6096d+0,  3.6280d+0,  3.6860d+0,  3.5568d+0,  3.4836d+0,  3.2868d+0,  3.3994d+0 &
                  /)
            r0ab( 491: 560)=(/ &
                  3.3476d+0,  3.3170d+0,  3.2950d+0,  3.2874d+0,  3.2606d+0,  3.9579d+0,  2.9226d+0, &
                  2.6838d+0,  3.7867d+0,  3.1732d+0,  3.3872d+0,  3.3643d+0,  3.1267d+0,  2.9541d+0, &
                  2.8505d+0,  2.7781d+0,  3.8475d+0,  3.3336d+0,  3.7359d+0,  3.8266d+0,  3.5733d+0, &
                  3.3959d+0,  3.2775d+0,  3.1915d+0,  3.9878d+0,  3.8816d+0,  3.5810d+0,  3.5364d+0, &
                  3.5060d+0,  3.8097d+0,  3.3925d+0,  3.3348d+0,  3.3019d+0,  3.2796d+0,  3.2662d+0, &
                  3.2464d+0,  3.7136d+0,  3.8619d+0,  2.9140d+0,  2.6271d+0,  3.4771d+0,  3.1774d+0, &
                  3.2560d+0,  3.1970d+0,  3.1207d+0,  2.9406d+0,  2.8322d+0,  2.7571d+0,  3.5455d+0, &
                  3.3514d+0,  3.5837d+0,  3.6177d+0,  3.5816d+0,  3.3902d+0,  3.2604d+0,  3.1652d+0, &
                  3.7037d+0,  3.6283d+0,  3.5858d+0,  3.5330d+0,  3.4884d+0,  3.5789d+0,  3.4094d+0, &
                  3.3473d+0,  3.3118d+0,  3.2876d+0,  3.2707d+0,  3.2521d+0,  3.5570d+0,  3.6496d+0 &
                  /)
            r0ab( 561: 630)=(/ &
                  3.6625d+0,  2.7300d+0,  2.5870d+0,  3.2471d+0,  3.1487d+0,  3.1667d+0,  3.0914d+0, &
                  3.0107d+0,  2.9812d+0,  2.8300d+0,  2.7284d+0,  3.3259d+0,  3.3182d+0,  3.4707d+0, &
                  3.4748d+0,  3.4279d+0,  3.4182d+0,  3.2547d+0,  3.1353d+0,  3.5116d+0,  3.9432d+0, &
                  3.8828d+0,  3.8303d+0,  3.7880d+0,  3.3760d+0,  3.7218d+0,  3.3408d+0,  3.3059d+0, &
                  3.2698d+0,  3.2446d+0,  3.2229d+0,  3.4422d+0,  3.5023d+0,  3.5009d+0,  3.5268d+0, &
                  2.6026d+0,  2.5355d+0,  3.1129d+0,  3.2863d+0,  3.1029d+0,  3.0108d+0,  2.9227d+0, &
                  2.8694d+0,  2.8109d+0,  2.6929d+0,  3.1958d+0,  3.4670d+0,  3.4018d+0,  3.3805d+0, &
                  3.3218d+0,  3.2815d+0,  3.2346d+0,  3.0994d+0,  3.3937d+0,  3.7266d+0,  3.6697d+0, &
                  3.6164d+0,  3.5730d+0,  3.2522d+0,  3.5051d+0,  3.4686d+0,  3.4355d+0,  3.4084d+0, &
                  3.3748d+0,  3.3496d+0,  3.3692d+0,  3.4052d+0,  3.3910d+0,  3.3849d+0,  3.3662d+0 &
                  /)
            r0ab( 631: 700)=(/ &
                  2.5087d+0,  2.4814d+0,  3.0239d+0,  3.1312d+0,  3.0535d+0,  2.9457d+0,  2.8496d+0, &
                  2.7780d+0,  2.7828d+0,  2.6532d+0,  3.1063d+0,  3.3143d+0,  3.3549d+0,  3.3120d+0, &
                  3.2421d+0,  3.1787d+0,  3.1176d+0,  3.0613d+0,  3.3082d+0,  3.5755d+0,  3.5222d+0, &
                  3.4678d+0,  3.4231d+0,  3.1684d+0,  3.3528d+0,  3.3162d+0,  3.2827d+0,  3.2527d+0, &
                  3.2308d+0,  3.2029d+0,  3.3173d+0,  3.3343d+0,  3.3092d+0,  3.2795d+0,  3.2452d+0, &
                  3.2096d+0,  3.2893d+0,  2.8991d+0,  4.0388d+0,  3.6100d+0,  3.9388d+0,  3.4475d+0, &
                  3.1590d+0,  2.9812d+0,  2.8586d+0,  2.7683d+0,  4.1428d+0,  3.7911d+0,  3.8225d+0, &
                  4.0372d+0,  3.7059d+0,  3.4935d+0,  3.3529d+0,  3.2492d+0,  4.4352d+0,  4.0826d+0, &
                  3.9733d+0,  3.9254d+0,  3.8646d+0,  3.9315d+0,  3.7837d+0,  3.7465d+0,  3.7211d+0, &
                  3.7012d+0,  3.6893d+0,  3.6676d+0,  3.7736d+0,  4.0660d+0,  3.7926d+0,  3.6158d+0 &
                  /)
            r0ab( 701: 770)=(/ &
                  3.5017d+0,  3.4166d+0,  4.6176d+0,  2.8786d+0,  3.1658d+0,  3.5823d+0,  3.7689d+0, &
                  3.5762d+0,  3.5789d+0,  3.3552d+0,  3.4004d+0,  3.1722d+0,  3.0212d+0,  3.7241d+0, &
                  3.9604d+0,  3.8500d+0,  3.9844d+0,  3.7035d+0,  3.9161d+0,  3.6751d+0,  3.5075d+0, &
                  4.1151d+0,  4.2877d+0,  4.1579d+0,  4.1247d+0,  4.0617d+0,  3.4874d+0,  3.9848d+0, &
                  3.9280d+0,  3.9079d+0,  3.8751d+0,  3.8604d+0,  3.8277d+0,  3.8002d+0,  3.9981d+0, &
                  3.7544d+0,  4.0371d+0,  3.8225d+0,  3.6718d+0,  4.3092d+0,  4.4764d+0,  2.8997d+0, &
                  3.0953d+0,  3.4524d+0,  3.6107d+0,  3.6062d+0,  3.5783d+0,  3.3463d+0,  3.3855d+0, &
                  3.1746d+0,  3.0381d+0,  3.6019d+0,  3.7938d+0,  3.8697d+0,  3.9781d+0,  3.6877d+0, &
                  3.8736d+0,  3.6451d+0,  3.4890d+0,  3.9858d+0,  4.1179d+0,  4.0430d+0,  3.9563d+0, &
                  3.9182d+0,  3.4002d+0,  3.8310d+0,  3.7716d+0,  3.7543d+0,  3.7203d+0,  3.7053d+0 &
                  /)
            r0ab( 771: 840)=(/ &
                  3.6742d+0,  3.8318d+0,  3.7631d+0,  3.7392d+0,  3.9892d+0,  3.7832d+0,  3.6406d+0, &
                  4.1701d+0,  4.3016d+0,  4.2196d+0,  2.8535d+0,  3.0167d+0,  3.3978d+0,  3.5363d+0, &
                  3.5393d+0,  3.5301d+0,  3.2960d+0,  3.3352d+0,  3.1287d+0,  2.9967d+0,  3.6659d+0, &
                  3.7239d+0,  3.8070d+0,  3.7165d+0,  3.6368d+0,  3.8162d+0,  3.5885d+0,  3.4336d+0, &
                  3.9829d+0,  4.0529d+0,  3.9584d+0,  3.9025d+0,  3.8607d+0,  3.3673d+0,  3.7658d+0, &
                  3.7035d+0,  3.6866d+0,  3.6504d+0,  3.6339d+0,  3.6024d+0,  3.7708d+0,  3.7283d+0, &
                  3.6896d+0,  3.9315d+0,  3.7250d+0,  3.5819d+0,  4.1457d+0,  4.2280d+0,  4.1130d+0, &
                  4.0597d+0,  3.0905d+0,  2.7998d+0,  3.6448d+0,  3.0739d+0,  3.2996d+0,  3.5262d+0, &
                  3.2559d+0,  3.0518d+0,  2.9394d+0,  2.8658d+0,  3.7514d+0,  3.2295d+0,  3.5643d+0, &
                  3.7808d+0,  3.6931d+0,  3.4723d+0,  3.3357d+0,  3.2429d+0,  4.0280d+0,  3.5589d+0 &
                  /)
            r0ab( 841: 910)=(/ &
                  3.4636d+0,  3.4994d+0,  3.4309d+0,  3.6177d+0,  3.2946d+0,  3.2376d+0,  3.2050d+0, &
                  3.1847d+0,  3.1715d+0,  3.1599d+0,  3.5555d+0,  3.8111d+0,  3.7693d+0,  3.5718d+0, &
                  3.4498d+0,  3.3662d+0,  4.1608d+0,  3.7417d+0,  3.6536d+0,  3.6154d+0,  3.8596d+0, &
                  3.0301d+0,  2.7312d+0,  3.5821d+0,  3.0473d+0,  3.2137d+0,  3.4679d+0,  3.1975d+0, &
                  2.9969d+0,  2.8847d+0,  2.8110d+0,  3.6931d+0,  3.2076d+0,  3.4943d+0,  3.5956d+0, &
                  3.6379d+0,  3.4190d+0,  3.2808d+0,  3.1860d+0,  3.9850d+0,  3.5105d+0,  3.4330d+0, &
                  3.3797d+0,  3.4155d+0,  3.6033d+0,  3.2737d+0,  3.2145d+0,  3.1807d+0,  3.1596d+0, &
                  3.1461d+0,  3.1337d+0,  3.4812d+0,  3.6251d+0,  3.7152d+0,  3.5201d+0,  3.3966d+0, &
                  3.3107d+0,  4.1128d+0,  3.6899d+0,  3.6082d+0,  3.5604d+0,  3.7834d+0,  3.7543d+0, &
                  2.9189d+0,  2.6777d+0,  3.4925d+0,  2.9648d+0,  3.1216d+0,  3.2940d+0,  3.0975d+0 &
                  /)
            r0ab( 911: 980)=(/ &
                  2.9757d+0,  2.8493d+0,  2.7638d+0,  3.6085d+0,  3.1214d+0,  3.4006d+0,  3.4793d+0, &
                  3.5147d+0,  3.3806d+0,  3.2356d+0,  3.1335d+0,  3.9144d+0,  3.4183d+0,  3.3369d+0, &
                  3.2803d+0,  3.2679d+0,  3.4871d+0,  3.1714d+0,  3.1521d+0,  3.1101d+0,  3.0843d+0, &
                  3.0670d+0,  3.0539d+0,  3.3890d+0,  3.5086d+0,  3.5895d+0,  3.4783d+0,  3.3484d+0, &
                  3.2559d+0,  4.0422d+0,  3.5967d+0,  3.5113d+0,  3.4576d+0,  3.6594d+0,  3.6313d+0, &
                  3.5690d+0,  2.8578d+0,  2.6334d+0,  3.4673d+0,  2.9245d+0,  3.0732d+0,  3.2435d+0, &
                  3.0338d+0,  2.9462d+0,  2.8143d+0,  2.7240d+0,  3.5832d+0,  3.0789d+0,  3.3617d+0, &
                  3.4246d+0,  3.4505d+0,  3.3443d+0,  3.1964d+0,  3.0913d+0,  3.8921d+0,  3.3713d+0, &
                  3.2873d+0,  3.2281d+0,  3.2165d+0,  3.4386d+0,  3.1164d+0,  3.1220d+0,  3.0761d+0, &
                  3.0480d+0,  3.0295d+0,  3.0155d+0,  3.3495d+0,  3.4543d+0,  3.5260d+0,  3.4413d+0 &
                  /)
            r0ab( 981:1050)=(/ &
                  3.3085d+0,  3.2134d+0,  4.0170d+0,  3.5464d+0,  3.4587d+0,  3.4006d+0,  3.6027d+0, &
                  3.5730d+0,  3.4945d+0,  3.4623d+0,  2.8240d+0,  2.5960d+0,  3.4635d+0,  2.9032d+0, &
                  3.0431d+0,  3.2115d+0,  2.9892d+0,  2.9148d+0,  2.7801d+0,  2.6873d+0,  3.5776d+0, &
                  3.0568d+0,  3.3433d+0,  3.3949d+0,  3.4132d+0,  3.3116d+0,  3.1616d+0,  3.0548d+0, &
                  3.8859d+0,  3.3719d+0,  3.2917d+0,  3.2345d+0,  3.2274d+0,  3.4171d+0,  3.1293d+0, &
                  3.0567d+0,  3.0565d+0,  3.0274d+0,  3.0087d+0,  2.9939d+0,  3.3293d+0,  3.4249d+0, &
                  3.4902d+0,  3.4091d+0,  3.2744d+0,  3.1776d+0,  4.0078d+0,  3.5374d+0,  3.4537d+0, &
                  3.3956d+0,  3.5747d+0,  3.5430d+0,  3.4522d+0,  3.4160d+0,  3.3975d+0,  2.8004d+0, &
                  2.5621d+0,  3.4617d+0,  2.9154d+0,  3.0203d+0,  3.1875d+0,  2.9548d+0,  2.8038d+0, &
                  2.7472d+0,  2.6530d+0,  3.5736d+0,  3.0584d+0,  3.3304d+0,  3.3748d+0,  3.3871d+0 &
                  /)
            r0ab(1051:1120)=(/ &
                  3.2028d+0,  3.1296d+0,  3.0214d+0,  3.8796d+0,  3.3337d+0,  3.2492d+0,  3.1883d+0, &
                  3.1802d+0,  3.4050d+0,  3.0756d+0,  3.0478d+0,  3.0322d+0,  3.0323d+0,  3.0163d+0, &
                  3.0019d+0,  3.3145d+0,  3.4050d+0,  3.4656d+0,  3.3021d+0,  3.2433d+0,  3.1453d+0, &
                  3.9991d+0,  3.5017d+0,  3.4141d+0,  3.3520d+0,  3.5583d+0,  3.5251d+0,  3.4243d+0, &
                  3.3851d+0,  3.3662d+0,  3.3525d+0,  2.7846d+0,  2.5324d+0,  3.4652d+0,  2.8759d+0, &
                  3.0051d+0,  3.1692d+0,  2.9273d+0,  2.7615d+0,  2.7164d+0,  2.6212d+0,  3.5744d+0, &
                  3.0275d+0,  3.3249d+0,  3.3627d+0,  3.3686d+0,  3.1669d+0,  3.0584d+0,  2.9915d+0, &
                  3.8773d+0,  3.3099d+0,  3.2231d+0,  3.1600d+0,  3.1520d+0,  3.4023d+0,  3.0426d+0, &
                  3.0099d+0,  2.9920d+0,  2.9809d+0,  2.9800d+0,  2.9646d+0,  3.3068d+0,  3.3930d+0, &
                  3.4486d+0,  3.2682d+0,  3.1729d+0,  3.1168d+0,  3.9952d+0,  3.4796d+0,  3.3901d+0 &
                  /)
            r0ab(1121:1190)=(/ &
                  3.3255d+0,  3.5530d+0,  3.5183d+0,  3.4097d+0,  3.3683d+0,  3.3492d+0,  3.3360d+0, &
                  3.3308d+0,  2.5424d+0,  2.6601d+0,  3.2555d+0,  3.2807d+0,  3.1384d+0,  3.1737d+0, &
                  2.9397d+0,  2.8429d+0,  2.8492d+0,  2.7225d+0,  3.3875d+0,  3.4910d+0,  3.4520d+0, &
                  3.3608d+0,  3.3036d+0,  3.2345d+0,  3.2999d+0,  3.1487d+0,  3.7409d+0,  3.8392d+0, &
                  3.7148d+0,  3.6439d+0,  3.6182d+0,  3.1753d+0,  3.5210d+0,  3.4639d+0,  3.4265d+0, &
                  3.4075d+0,  3.3828d+0,  3.3474d+0,  3.4071d+0,  3.3754d+0,  3.3646d+0,  3.3308d+0, &
                  3.4393d+0,  3.2993d+0,  3.8768d+0,  3.9891d+0,  3.8310d+0,  3.7483d+0,  3.3417d+0, &
                  3.3019d+0,  3.2250d+0,  3.1832d+0,  3.1578d+0,  3.1564d+0,  3.1224d+0,  3.4620d+0, &
                  2.9743d+0,  2.8058d+0,  3.4830d+0,  3.3474d+0,  3.6863d+0,  3.3617d+0,  3.1608d+0, &
                  3.0069d+0,  2.9640d+0,  2.8427d+0,  3.5885d+0,  3.5219d+0,  4.1314d+0,  3.8120d+0 &
                  /)
            r0ab(1191:1260)=(/ &
                  3.6015d+0,  3.4502d+0,  3.3498d+0,  3.2777d+0,  3.8635d+0,  3.8232d+0,  3.8486d+0, &
                  3.7215d+0,  3.6487d+0,  3.4724d+0,  3.5627d+0,  3.5087d+0,  3.4757d+0,  3.4517d+0, &
                  3.4423d+0,  3.4139d+0,  4.1028d+0,  3.8388d+0,  3.6745d+0,  3.5562d+0,  3.4806d+0, &
                  3.4272d+0,  4.0182d+0,  3.9991d+0,  4.0007d+0,  3.9282d+0,  3.7238d+0,  3.6498d+0, &
                  3.5605d+0,  3.5211d+0,  3.5009d+0,  3.4859d+0,  3.4785d+0,  3.5621d+0,  4.2623d+0, &
                  3.0775d+0,  2.8275d+0,  4.0181d+0,  3.3385d+0,  3.5379d+0,  3.5036d+0,  3.2589d+0, &
                  3.0804d+0,  3.0094d+0,  2.9003d+0,  4.0869d+0,  3.5088d+0,  3.9105d+0,  3.9833d+0, &
                  3.7176d+0,  3.5323d+0,  3.4102d+0,  3.3227d+0,  4.2702d+0,  4.0888d+0,  3.7560d+0, &
                  3.7687d+0,  3.6681d+0,  3.6405d+0,  3.5569d+0,  3.4990d+0,  3.4659d+0,  3.4433d+0, &
                  3.4330d+0,  3.4092d+0,  3.8867d+0,  4.0190d+0,  3.7961d+0,  3.6412d+0,  3.5405d+0 &
                  /)
            r0ab(1261:1330)=(/ &
                  3.4681d+0,  4.3538d+0,  4.2136d+0,  3.9381d+0,  3.8912d+0,  3.9681d+0,  3.7909d+0, &
                  3.6774d+0,  3.6262d+0,  3.5999d+0,  3.5823d+0,  3.5727d+0,  3.5419d+0,  4.0245d+0, &
                  4.1874d+0,  3.0893d+0,  2.7917d+0,  3.7262d+0,  3.3518d+0,  3.4241d+0,  3.5433d+0, &
                  3.2773d+0,  3.0890d+0,  2.9775d+0,  2.9010d+0,  3.8048d+0,  3.5362d+0,  3.7746d+0, &
                  3.7911d+0,  3.7511d+0,  3.5495d+0,  3.4149d+0,  3.3177d+0,  4.0129d+0,  3.8370d+0, &
                  3.7739d+0,  3.7125d+0,  3.7152d+0,  3.7701d+0,  3.5813d+0,  3.5187d+0,  3.4835d+0, &
                  3.4595d+0,  3.4439d+0,  3.4242d+0,  3.7476d+0,  3.8239d+0,  3.8346d+0,  3.6627d+0, &
                  3.5479d+0,  3.4639d+0,  4.1026d+0,  3.9733d+0,  3.9292d+0,  3.8667d+0,  3.9513d+0, &
                  3.8959d+0,  3.7698d+0,  3.7089d+0,  3.6765d+0,  3.6548d+0,  3.6409d+0,  3.5398d+0, &
                  3.8759d+0,  3.9804d+0,  4.0150d+0,  2.9091d+0,  2.7638d+0,  3.5066d+0,  3.3377d+0 &
                  /)
            r0ab(1331:1400)=(/ &
                  3.3481d+0,  3.2633d+0,  3.1810d+0,  3.1428d+0,  2.9872d+0,  2.8837d+0,  3.5929d+0, &
                  3.5183d+0,  3.6729d+0,  3.6596d+0,  3.6082d+0,  3.5927d+0,  3.4224d+0,  3.2997d+0, &
                  3.8190d+0,  4.1865d+0,  4.1114d+0,  4.0540d+0,  3.6325d+0,  3.5697d+0,  3.5561d+0, &
                  3.5259d+0,  3.4901d+0,  3.4552d+0,  3.4315d+0,  3.4091d+0,  3.6438d+0,  3.6879d+0, &
                  3.6832d+0,  3.7043d+0,  3.5557d+0,  3.4466d+0,  3.9203d+0,  4.2919d+0,  4.2196d+0, &
                  4.1542d+0,  3.7573d+0,  3.7039d+0,  3.6546d+0,  3.6151d+0,  3.5293d+0,  3.4849d+0, &
                  3.4552d+0,  3.5192d+0,  3.7673d+0,  3.8359d+0,  3.8525d+0,  3.8901d+0,  2.7806d+0, &
                  2.7209d+0,  3.3812d+0,  3.4958d+0,  3.2913d+0,  3.1888d+0,  3.0990d+0,  3.0394d+0, &
                  2.9789d+0,  2.8582d+0,  3.4716d+0,  3.6883d+0,  3.6105d+0,  3.5704d+0,  3.5059d+0, &
                  3.4619d+0,  3.4138d+0,  3.2742d+0,  3.7080d+0,  3.9773d+0,  3.9010d+0,  3.8409d+0 &
                  /)
            r0ab(1401:1470)=(/ &
                  3.7944d+0,  3.4465d+0,  3.7235d+0,  3.6808d+0,  3.6453d+0,  3.6168d+0,  3.5844d+0, &
                  3.5576d+0,  3.5772d+0,  3.5959d+0,  3.5768d+0,  3.5678d+0,  3.5486d+0,  3.4228d+0, &
                  3.8107d+0,  4.0866d+0,  4.0169d+0,  3.9476d+0,  3.6358d+0,  3.5800d+0,  3.5260d+0, &
                  3.4838d+0,  3.4501d+0,  3.4204d+0,  3.3553d+0,  3.6487d+0,  3.6973d+0,  3.7398d+0, &
                  3.7405d+0,  3.7459d+0,  3.7380d+0,  2.6848d+0,  2.6740d+0,  3.2925d+0,  3.3386d+0, &
                  3.2473d+0,  3.1284d+0,  3.0301d+0,  2.9531d+0,  2.9602d+0,  2.8272d+0,  3.3830d+0, &
                  3.5358d+0,  3.5672d+0,  3.5049d+0,  3.4284d+0,  3.3621d+0,  3.3001d+0,  3.2451d+0, &
                  3.6209d+0,  3.8299d+0,  3.7543d+0,  3.6920d+0,  3.6436d+0,  3.3598d+0,  3.5701d+0, &
                  3.5266d+0,  3.4904d+0,  3.4590d+0,  3.4364d+0,  3.4077d+0,  3.5287d+0,  3.5280d+0, &
                  3.4969d+0,  3.4650d+0,  3.4304d+0,  3.3963d+0,  3.7229d+0,  3.9402d+0,  3.8753d+0 &
                  /)
            r0ab(1471:1540)=(/ &
                  3.8035d+0,  3.5499d+0,  3.4913d+0,  3.4319d+0,  3.3873d+0,  3.3520d+0,  3.3209d+0, &
                  3.2948d+0,  3.5052d+0,  3.6465d+0,  3.6696d+0,  3.6577d+0,  3.6388d+0,  3.6142d+0, &
                  3.5889d+0,  3.3968d+0,  3.0122d+0,  4.2241d+0,  3.7887d+0,  4.0049d+0,  3.5384d+0, &
                  3.2698d+0,  3.1083d+0,  2.9917d+0,  2.9057d+0,  4.3340d+0,  3.9900d+0,  4.6588d+0, &
                  4.1278d+0,  3.8125d+0,  3.6189d+0,  3.4851d+0,  3.3859d+0,  4.6531d+0,  4.3134d+0, &
                  4.2258d+0,  4.1309d+0,  4.0692d+0,  4.0944d+0,  3.9850d+0,  3.9416d+0,  3.9112d+0, &
                  3.8873d+0,  3.8736d+0,  3.8473d+0,  4.6027d+0,  4.1538d+0,  3.8994d+0,  3.7419d+0, &
                  3.6356d+0,  3.5548d+0,  4.8353d+0,  4.5413d+0,  4.3891d+0,  4.3416d+0,  4.3243d+0, &
                  4.2753d+0,  4.2053d+0,  4.1790d+0,  4.1685d+0,  4.1585d+0,  4.1536d+0,  4.0579d+0, &
                  4.1980d+0,  4.4564d+0,  4.2192d+0,  4.0528d+0,  3.9489d+0,  3.8642d+0,  5.0567d+0 &
                  /)
            r0ab(1541:1610)=(/ &
                  3.0630d+0,  3.3271d+0,  4.0432d+0,  4.0046d+0,  4.1555d+0,  3.7426d+0,  3.5130d+0, &
                  3.5174d+0,  3.2884d+0,  3.1378d+0,  4.1894d+0,  4.2321d+0,  4.1725d+0,  4.1833d+0, &
                  3.8929d+0,  4.0544d+0,  3.8118d+0,  3.6414d+0,  4.6373d+0,  4.6268d+0,  4.4750d+0, &
                  4.4134d+0,  4.3458d+0,  3.8582d+0,  4.2583d+0,  4.1898d+0,  4.1562d+0,  4.1191d+0, &
                  4.1069d+0,  4.0639d+0,  4.1257d+0,  4.1974d+0,  3.9532d+0,  4.1794d+0,  3.9660d+0, &
                  3.8130d+0,  4.8160d+0,  4.8272d+0,  4.6294d+0,  4.5840d+0,  4.0770d+0,  4.0088d+0, &
                  3.9103d+0,  3.8536d+0,  3.8324d+0,  3.7995d+0,  3.7826d+0,  4.2294d+0,  4.3380d+0, &
                  4.4352d+0,  4.1933d+0,  4.4580d+0,  4.2554d+0,  4.1072d+0,  5.0454d+0,  5.1814d+0, &
                  3.0632d+0,  3.2662d+0,  3.6432d+0,  3.8088d+0,  3.7910d+0,  3.7381d+0,  3.5093d+0, &
                  3.5155d+0,  3.3047d+0,  3.1681d+0,  3.7871d+0,  3.9924d+0,  4.0637d+0,  4.1382d+0 &
                  /)
            r0ab(1611:1680)=(/ &
                  3.8591d+0,  4.0164d+0,  3.7878d+0,  3.6316d+0,  4.1741d+0,  4.3166d+0,  4.2395d+0, &
                  4.1831d+0,  4.1107d+0,  3.5857d+0,  4.0270d+0,  3.9676d+0,  3.9463d+0,  3.9150d+0, &
                  3.9021d+0,  3.8708d+0,  4.0240d+0,  4.1551d+0,  3.9108d+0,  4.1337d+0,  3.9289d+0, &
                  3.7873d+0,  4.3666d+0,  4.5080d+0,  4.4232d+0,  4.3155d+0,  3.8461d+0,  3.8007d+0, &
                  3.6991d+0,  3.6447d+0,  3.6308d+0,  3.5959d+0,  3.5749d+0,  4.0359d+0,  4.3124d+0, &
                  4.3539d+0,  4.1122d+0,  4.3772d+0,  4.1785d+0,  4.0386d+0,  4.7004d+0,  4.8604d+0, &
                  4.6261d+0,  2.9455d+0,  3.2470d+0,  3.6108d+0,  3.8522d+0,  3.6625d+0,  3.6598d+0, &
                  3.4411d+0,  3.4660d+0,  3.2415d+0,  3.0944d+0,  3.7514d+0,  4.0397d+0,  3.9231d+0, &
                  4.0561d+0,  3.7860d+0,  3.9845d+0,  3.7454d+0,  3.5802d+0,  4.1366d+0,  4.3581d+0, &
                  4.2351d+0,  4.2011d+0,  4.1402d+0,  3.5381d+0,  4.0653d+0,  4.0093d+0,  3.9883d+0 &
                  /)
            r0ab(1681:1750)=(/ &
                  3.9570d+0,  3.9429d+0,  3.9112d+0,  3.8728d+0,  4.0682d+0,  3.8351d+0,  4.1054d+0, &
                  3.8928d+0,  3.7445d+0,  4.3415d+0,  4.5497d+0,  4.3833d+0,  4.3122d+0,  3.8051d+0, &
                  3.7583d+0,  3.6622d+0,  3.6108d+0,  3.5971d+0,  3.5628d+0,  3.5408d+0,  4.0780d+0, &
                  4.0727d+0,  4.2836d+0,  4.0553d+0,  4.3647d+0,  4.1622d+0,  4.0178d+0,  4.5802d+0, &
                  4.9125d+0,  4.5861d+0,  4.6201d+0,  2.9244d+0,  3.2241d+0,  3.5848d+0,  3.8293d+0, &
                  3.6395d+0,  3.6400d+0,  3.4204d+0,  3.4499d+0,  3.2253d+0,  3.0779d+0,  3.7257d+0, &
                  4.0170d+0,  3.9003d+0,  4.0372d+0,  3.7653d+0,  3.9672d+0,  3.7283d+0,  3.5630d+0, &
                  4.1092d+0,  4.3347d+0,  4.2117d+0,  4.1793d+0,  4.1179d+0,  3.5139d+0,  4.0426d+0, &
                  3.9867d+0,  3.9661d+0,  3.9345d+0,  3.9200d+0,  3.8883d+0,  3.8498d+0,  4.0496d+0, &
                  3.8145d+0,  4.0881d+0,  3.8756d+0,  3.7271d+0,  4.3128d+0,  4.5242d+0,  4.3578d+0 &
                  /)
            r0ab(1751:1820)=(/ &
                  4.2870d+0,  3.7796d+0,  3.7318d+0,  3.6364d+0,  3.5854d+0,  3.5726d+0,  3.5378d+0, &
                  3.5155d+0,  4.0527d+0,  4.0478d+0,  4.2630d+0,  4.0322d+0,  4.3449d+0,  4.1421d+0, &
                  3.9975d+0,  4.5499d+0,  4.8825d+0,  4.5601d+0,  4.5950d+0,  4.5702d+0,  2.9046d+0, &
                  3.2044d+0,  3.5621d+0,  3.8078d+0,  3.6185d+0,  3.6220d+0,  3.4019d+0,  3.4359d+0, &
                  3.2110d+0,  3.0635d+0,  3.7037d+0,  3.9958d+0,  3.8792d+0,  4.0194d+0,  3.7460d+0, &
                  3.9517d+0,  3.7128d+0,  3.5474d+0,  4.0872d+0,  4.3138d+0,  4.1906d+0,  4.1593d+0, &
                  4.0973d+0,  3.4919d+0,  4.0216d+0,  3.9657d+0,  3.9454d+0,  3.9134d+0,  3.8986d+0, &
                  3.8669d+0,  3.8289d+0,  4.0323d+0,  3.7954d+0,  4.0725d+0,  3.8598d+0,  3.7113d+0, &
                  4.2896d+0,  4.5021d+0,  4.3325d+0,  4.2645d+0,  3.7571d+0,  3.7083d+0,  3.6136d+0, &
                  3.5628d+0,  3.5507d+0,  3.5155d+0,  3.4929d+0,  4.0297d+0,  4.0234d+0,  4.2442d+0 &
                  /)
            r0ab(1821:1890)=(/ &
                  4.0112d+0,  4.3274d+0,  4.1240d+0,  3.9793d+0,  4.5257d+0,  4.8568d+0,  4.5353d+0, &
                  4.5733d+0,  4.5485d+0,  4.5271d+0,  2.8878d+0,  3.1890d+0,  3.5412d+0,  3.7908d+0, &
                  3.5974d+0,  3.6078d+0,  3.3871d+0,  3.4243d+0,  3.1992d+0,  3.0513d+0,  3.6831d+0, &
                  3.9784d+0,  3.8579d+0,  4.0049d+0,  3.7304d+0,  3.9392d+0,  3.7002d+0,  3.5347d+0, &
                  4.0657d+0,  4.2955d+0,  4.1705d+0,  4.1424d+0,  4.0800d+0,  3.4717d+0,  4.0043d+0, &
                  3.9485d+0,  3.9286d+0,  3.8965d+0,  3.8815d+0,  3.8500d+0,  3.8073d+0,  4.0180d+0, &
                  3.7796d+0,  4.0598d+0,  3.8470d+0,  3.6983d+0,  4.2678d+0,  4.4830d+0,  4.3132d+0, &
                  4.2444d+0,  3.7370d+0,  3.6876d+0,  3.5935d+0,  3.5428d+0,  3.5314d+0,  3.4958d+0, &
                  3.4730d+0,  4.0117d+0,  4.0043d+0,  4.2287d+0,  3.9939d+0,  4.3134d+0,  4.1096d+0, &
                  3.9646d+0,  4.5032d+0,  4.8356d+0,  4.5156d+0,  4.5544d+0,  4.5297d+0,  4.5083d+0 &
                  /)
            r0ab(1891:1960)=(/ &
                  4.4896d+0,  2.8709d+0,  3.1737d+0,  3.5199d+0,  3.7734d+0,  3.5802d+0,  3.5934d+0, &
                  3.3724d+0,  3.4128d+0,  3.1877d+0,  3.0396d+0,  3.6624d+0,  3.9608d+0,  3.8397d+0, &
                  3.9893d+0,  3.7145d+0,  3.9266d+0,  3.6877d+0,  3.5222d+0,  4.0448d+0,  4.2771d+0, &
                  4.1523d+0,  4.1247d+0,  4.0626d+0,  3.4530d+0,  3.9866d+0,  3.9310d+0,  3.9115d+0, &
                  3.8792d+0,  3.8641d+0,  3.8326d+0,  3.7892d+0,  4.0025d+0,  3.7636d+0,  4.0471d+0, &
                  3.8343d+0,  3.6854d+0,  4.2464d+0,  4.4635d+0,  4.2939d+0,  4.2252d+0,  3.7169d+0, &
                  3.6675d+0,  3.5739d+0,  3.5235d+0,  3.5126d+0,  3.4768d+0,  3.4537d+0,  3.9932d+0, &
                  3.9854d+0,  4.2123d+0,  3.9765d+0,  4.2992d+0,  4.0951d+0,  3.9500d+0,  4.4811d+0, &
                  4.8135d+0,  4.4959d+0,  4.5351d+0,  4.5105d+0,  4.4891d+0,  4.4705d+0,  4.4515d+0, &
                  2.8568d+0,  3.1608d+0,  3.5050d+0,  3.7598d+0,  3.5665d+0,  3.5803d+0,  3.3601d+0 &
                  /)
            r0ab(1961:2030)=(/ &
                  3.4031d+0,  3.1779d+0,  3.0296d+0,  3.6479d+0,  3.9471d+0,  3.8262d+0,  3.9773d+0, &
                  3.7015d+0,  3.9162d+0,  3.6771d+0,  3.5115d+0,  4.0306d+0,  4.2634d+0,  4.1385d+0, &
                  4.1116d+0,  4.0489d+0,  3.4366d+0,  3.9732d+0,  3.9176d+0,  3.8983d+0,  3.8659d+0, &
                  3.8507d+0,  3.8191d+0,  3.7757d+0,  3.9907d+0,  3.7506d+0,  4.0365d+0,  3.8235d+0, &
                  3.6745d+0,  4.2314d+0,  4.4490d+0,  4.2792d+0,  4.2105d+0,  3.7003d+0,  3.6510d+0, &
                  3.5578d+0,  3.5075d+0,  3.4971d+0,  3.4609d+0,  3.4377d+0,  3.9788d+0,  3.9712d+0, &
                  4.1997d+0,  3.9624d+0,  4.2877d+0,  4.0831d+0,  3.9378d+0,  4.4655d+0,  4.7974d+0, &
                  4.4813d+0,  4.5209d+0,  4.4964d+0,  4.4750d+0,  4.4565d+0,  4.4375d+0,  4.4234d+0, &
                  2.6798d+0,  3.0151d+0,  3.2586d+0,  3.5292d+0,  3.5391d+0,  3.4902d+0,  3.2887d+0, &
                  3.3322d+0,  3.1228d+0,  2.9888d+0,  3.4012d+0,  3.7145d+0,  3.7830d+0,  3.6665d+0 &
                  /)
            r0ab(2031:2100)=(/ &
                  3.5898d+0,  3.8077d+0,  3.5810d+0,  3.4265d+0,  3.7726d+0,  4.0307d+0,  3.9763d+0, &
                  3.8890d+0,  3.8489d+0,  3.2706d+0,  3.7595d+0,  3.6984d+0,  3.6772d+0,  3.6428d+0, &
                  3.6243d+0,  3.5951d+0,  3.7497d+0,  3.6775d+0,  3.6364d+0,  3.9203d+0,  3.7157d+0, &
                  3.5746d+0,  3.9494d+0,  4.2076d+0,  4.1563d+0,  4.0508d+0,  3.5329d+0,  3.4780d+0, &
                  3.3731d+0,  3.3126d+0,  3.2846d+0,  3.2426d+0,  3.2135d+0,  3.7491d+0,  3.9006d+0, &
                  3.8332d+0,  3.8029d+0,  4.1436d+0,  3.9407d+0,  3.7998d+0,  4.1663d+0,  4.5309d+0, &
                  4.3481d+0,  4.2911d+0,  4.2671d+0,  4.2415d+0,  4.2230d+0,  4.2047d+0,  4.1908d+0, &
                  4.1243d+0,  2.5189d+0,  2.9703d+0,  3.3063d+0,  3.6235d+0,  3.4517d+0,  3.3989d+0, &
                  3.2107d+0,  3.2434d+0,  3.0094d+0,  2.8580d+0,  3.4253d+0,  3.8157d+0,  3.7258d+0, &
                  3.6132d+0,  3.5297d+0,  3.7566d+0,  3.5095d+0,  3.3368d+0,  3.7890d+0,  4.1298d+0 &
                  /)
            r0ab(2101:2170)=(/ &
                  4.0190d+0,  3.9573d+0,  3.9237d+0,  3.2677d+0,  3.8480d+0,  3.8157d+0,  3.7656d+0, &
                  3.7317d+0,  3.7126d+0,  3.6814d+0,  3.6793d+0,  3.6218d+0,  3.5788d+0,  3.8763d+0, &
                  3.6572d+0,  3.5022d+0,  3.9737d+0,  4.3255d+0,  4.1828d+0,  4.1158d+0,  3.5078d+0, &
                  3.4595d+0,  3.3600d+0,  3.3088d+0,  3.2575d+0,  3.2164d+0,  3.1856d+0,  3.8522d+0, &
                  3.8665d+0,  3.8075d+0,  3.7772d+0,  4.1391d+0,  3.9296d+0,  3.7772d+0,  4.2134d+0, &
                  4.7308d+0,  4.3787d+0,  4.3894d+0,  4.3649d+0,  4.3441d+0,  4.3257d+0,  4.3073d+0, &
                  4.2941d+0,  4.1252d+0,  4.2427d+0,  3.0481d+0,  2.9584d+0,  3.6919d+0,  3.5990d+0, &
                  3.8881d+0,  3.4209d+0,  3.1606d+0,  3.1938d+0,  2.9975d+0,  2.8646d+0,  3.8138d+0, &
                  3.7935d+0,  3.7081d+0,  3.9155d+0,  3.5910d+0,  3.4808d+0,  3.4886d+0,  3.3397d+0, &
                  4.1336d+0,  4.1122d+0,  3.9888d+0,  3.9543d+0,  3.8917d+0,  3.5894d+0,  3.8131d+0 &
                  /)
            r0ab(2171:2240)=(/ &
                  3.7635d+0,  3.7419d+0,  3.7071d+0,  3.6880d+0,  3.6574d+0,  3.6546d+0,  3.9375d+0, &
                  3.6579d+0,  3.5870d+0,  3.6361d+0,  3.5039d+0,  4.3149d+0,  4.2978d+0,  4.1321d+0, &
                  4.1298d+0,  3.8164d+0,  3.7680d+0,  3.7154d+0,  3.6858d+0,  3.6709d+0,  3.6666d+0, &
                  3.6517d+0,  3.8174d+0,  3.8608d+0,  4.1805d+0,  3.9102d+0,  3.8394d+0,  3.8968d+0, &
                  3.7673d+0,  4.5274d+0,  4.6682d+0,  4.3344d+0,  4.3639d+0,  4.3384d+0,  4.3162d+0, &
                  4.2972d+0,  4.2779d+0,  4.2636d+0,  4.0253d+0,  4.1168d+0,  4.1541d+0,  2.8136d+0, &
                  3.0951d+0,  3.4635d+0,  3.6875d+0,  3.4987d+0,  3.5183d+0,  3.2937d+0,  3.3580d+0, &
                  3.1325d+0,  2.9832d+0,  3.6078d+0,  3.8757d+0,  3.7616d+0,  3.9222d+0,  3.6370d+0, &
                  3.8647d+0,  3.6256d+0,  3.4595d+0,  3.9874d+0,  4.1938d+0,  4.0679d+0,  4.0430d+0, &
                  3.9781d+0,  3.3886d+0,  3.9008d+0,  3.8463d+0,  3.8288d+0,  3.7950d+0,  3.7790d+0 &
                  /)
            r0ab(2241:2310)=(/ &
                  3.7472d+0,  3.7117d+0,  3.9371d+0,  3.6873d+0,  3.9846d+0,  3.7709d+0,  3.6210d+0, &
                  4.1812d+0,  4.3750d+0,  4.2044d+0,  4.1340d+0,  3.6459d+0,  3.5929d+0,  3.5036d+0, &
                  3.4577d+0,  3.4528d+0,  3.4146d+0,  3.3904d+0,  3.9014d+0,  3.9031d+0,  4.1443d+0, &
                  3.8961d+0,  4.2295d+0,  4.0227d+0,  3.8763d+0,  4.4086d+0,  4.7097d+0,  4.4064d+0, &
                  4.4488d+0,  4.4243d+0,  4.4029d+0,  4.3842d+0,  4.3655d+0,  4.3514d+0,  4.1162d+0, &
                  4.2205d+0,  4.1953d+0,  4.2794d+0,  2.8032d+0,  3.0805d+0,  3.4519d+0,  3.6700d+0, &
                  3.4827d+0,  3.5050d+0,  3.2799d+0,  3.3482d+0,  3.1233d+0,  2.9747d+0,  3.5971d+0, &
                  3.8586d+0,  3.7461d+0,  3.9100d+0,  3.6228d+0,  3.8535d+0,  3.6147d+0,  3.4490d+0, &
                  3.9764d+0,  4.1773d+0,  4.0511d+0,  4.0270d+0,  3.9614d+0,  3.3754d+0,  3.8836d+0, &
                  3.8291d+0,  3.8121d+0,  3.7780d+0,  3.7619d+0,  3.7300d+0,  3.6965d+0,  3.9253d+0 &
                  /)
            r0ab(2311:2380)=(/ &
                  3.6734d+0,  3.9733d+0,  3.7597d+0,  3.6099d+0,  4.1683d+0,  4.3572d+0,  4.1862d+0, &
                  4.1153d+0,  3.6312d+0,  3.5772d+0,  3.4881d+0,  3.4429d+0,  3.4395d+0,  3.4009d+0, &
                  3.3766d+0,  3.8827d+0,  3.8868d+0,  4.1316d+0,  3.8807d+0,  4.2164d+0,  4.0092d+0, &
                  3.8627d+0,  4.3936d+0,  4.6871d+0,  4.3882d+0,  4.4316d+0,  4.4073d+0,  4.3858d+0, &
                  4.3672d+0,  4.3485d+0,  4.3344d+0,  4.0984d+0,  4.2036d+0,  4.1791d+0,  4.2622d+0, &
                  4.2450d+0,  2.7967d+0,  3.0689d+0,  3.4445d+0,  3.6581d+0,  3.4717d+0,  3.4951d+0, &
                  3.2694d+0,  3.3397d+0,  3.1147d+0,  2.9661d+0,  3.5898d+0,  3.8468d+0,  3.7358d+0, &
                  3.9014d+0,  3.6129d+0,  3.8443d+0,  3.6054d+0,  3.4396d+0,  3.9683d+0,  4.1656d+0, &
                  4.0394d+0,  4.0158d+0,  3.9498d+0,  3.3677d+0,  3.8718d+0,  3.8164d+0,  3.8005d+0, &
                  3.7662d+0,  3.7500d+0,  3.7181d+0,  3.6863d+0,  3.9170d+0,  3.6637d+0,  3.9641d+0 &
                  /)
            r0ab(2381:2450)=(/ &
                  3.7503d+0,  3.6004d+0,  4.1590d+0,  4.3448d+0,  4.1739d+0,  4.1029d+0,  3.6224d+0, &
                  3.5677d+0,  3.4785d+0,  3.4314d+0,  3.4313d+0,  3.3923d+0,  3.3680d+0,  3.8698d+0, &
                  3.8758d+0,  4.1229d+0,  3.8704d+0,  4.2063d+0,  3.9987d+0,  3.8519d+0,  4.3832d+0, &
                  4.6728d+0,  4.3759d+0,  4.4195d+0,  4.3952d+0,  4.3737d+0,  4.3551d+0,  4.3364d+0, &
                  4.3223d+0,  4.0861d+0,  4.1911d+0,  4.1676d+0,  4.2501d+0,  4.2329d+0,  4.2208d+0, &
                  2.7897d+0,  3.0636d+0,  3.4344d+0,  3.6480d+0,  3.4626d+0,  3.4892d+0,  3.2626d+0, &
                  3.3344d+0,  3.1088d+0,  2.9597d+0,  3.5804d+0,  3.8359d+0,  3.7251d+0,  3.8940d+0, &
                  3.6047d+0,  3.8375d+0,  3.5990d+0,  3.4329d+0,  3.9597d+0,  4.1542d+0,  4.0278d+0, &
                  4.0048d+0,  3.9390d+0,  3.3571d+0,  3.8608d+0,  3.8056d+0,  3.7899d+0,  3.7560d+0, &
                  3.7400d+0,  3.7081d+0,  3.6758d+0,  3.9095d+0,  3.6552d+0,  3.9572d+0,  3.7436d+0 &
                  /)
            r0ab(2451:2520)=(/ &
                  3.5933d+0,  4.1508d+0,  4.3337d+0,  4.1624d+0,  4.0916d+0,  3.6126d+0,  3.5582d+0, &
                  3.4684d+0,  3.4212d+0,  3.4207d+0,  3.3829d+0,  3.3586d+0,  3.8604d+0,  3.8658d+0, &
                  4.1156d+0,  3.8620d+0,  4.1994d+0,  3.9917d+0,  3.8446d+0,  4.3750d+0,  4.6617d+0, &
                  4.3644d+0,  4.4083d+0,  4.3840d+0,  4.3625d+0,  4.3439d+0,  4.3253d+0,  4.3112d+0, &
                  4.0745d+0,  4.1807d+0,  4.1578d+0,  4.2390d+0,  4.2218d+0,  4.2097d+0,  4.1986d+0, &
                  2.8395d+0,  3.0081d+0,  3.3171d+0,  3.4878d+0,  3.5360d+0,  3.5145d+0,  3.2809d+0, &
                  3.3307d+0,  3.1260d+0,  2.9940d+0,  3.4741d+0,  3.6675d+0,  3.7832d+0,  3.6787d+0, &
                  3.6156d+0,  3.8041d+0,  3.5813d+0,  3.4301d+0,  3.8480d+0,  3.9849d+0,  3.9314d+0, &
                  3.8405d+0,  3.8029d+0,  3.2962d+0,  3.7104d+0,  3.6515d+0,  3.6378d+0,  3.6020d+0, &
                  3.5849d+0,  3.5550d+0,  3.7494d+0,  3.6893d+0,  3.6666d+0,  3.9170d+0,  3.7150d+0 &
                  /)
            r0ab(2521:2590)=(/ &
                  3.5760d+0,  4.0268d+0,  4.1596d+0,  4.1107d+0,  3.9995d+0,  3.5574d+0,  3.5103d+0, &
                  3.4163d+0,  3.3655d+0,  3.3677d+0,  3.3243d+0,  3.2975d+0,  3.7071d+0,  3.9047d+0, &
                  3.8514d+0,  3.8422d+0,  3.8022d+0,  3.9323d+0,  3.7932d+0,  4.2343d+0,  4.4583d+0, &
                  4.3115d+0,  4.2457d+0,  4.2213d+0,  4.1945d+0,  4.1756d+0,  4.1569d+0,  4.1424d+0, &
                  4.0620d+0,  4.0494d+0,  3.9953d+0,  4.0694d+0,  4.0516d+0,  4.0396d+0,  4.0280d+0, &
                  4.0130d+0,  2.9007d+0,  2.9674d+0,  3.8174d+0,  3.5856d+0,  3.6486d+0,  3.5339d+0, &
                  3.2832d+0,  3.3154d+0,  3.1144d+0,  2.9866d+0,  3.9618d+0,  3.8430d+0,  3.9980d+0, &
                  3.8134d+0,  3.6652d+0,  3.7985d+0,  3.5756d+0,  3.4207d+0,  4.4061d+0,  4.2817d+0, &
                  4.1477d+0,  4.0616d+0,  3.9979d+0,  3.6492d+0,  3.8833d+0,  3.8027d+0,  3.7660d+0, &
                  3.7183d+0,  3.6954d+0,  3.6525d+0,  3.9669d+0,  3.8371d+0,  3.7325d+0,  3.9160d+0 &
                  /)
            r0ab(2591:2660)=(/ &
                  3.7156d+0,  3.5714d+0,  4.6036d+0,  4.4620d+0,  4.3092d+0,  4.2122d+0,  3.8478d+0, &
                  3.7572d+0,  3.6597d+0,  3.5969d+0,  3.5575d+0,  3.5386d+0,  3.5153d+0,  3.7818d+0, &
                  4.1335d+0,  4.0153d+0,  3.9177d+0,  3.8603d+0,  3.9365d+0,  3.7906d+0,  4.7936d+0, &
                  4.7410d+0,  4.5461d+0,  4.5662d+0,  4.5340d+0,  4.5059d+0,  4.4832d+0,  4.4604d+0, &
                  4.4429d+0,  4.2346d+0,  4.4204d+0,  4.3119d+0,  4.3450d+0,  4.3193d+0,  4.3035d+0, &
                  4.2933d+0,  4.1582d+0,  4.2450d+0,  2.8559d+0,  2.9050d+0,  3.8325d+0,  3.5442d+0, &
                  3.5077d+0,  3.4905d+0,  3.2396d+0,  3.2720d+0,  3.0726d+0,  2.9467d+0,  3.9644d+0, &
                  3.8050d+0,  3.8981d+0,  3.7762d+0,  3.6216d+0,  3.7531d+0,  3.5297d+0,  3.3742d+0, &
                  4.3814d+0,  4.2818d+0,  4.1026d+0,  4.0294d+0,  3.9640d+0,  3.6208d+0,  3.8464d+0, &
                  3.7648d+0,  3.7281d+0,  3.6790d+0,  3.6542d+0,  3.6117d+0,  3.8650d+0,  3.8010d+0 &
                  /)
            r0ab(2661:2730)=(/ &
                  3.6894d+0,  3.8713d+0,  3.6699d+0,  3.5244d+0,  4.5151d+0,  4.4517d+0,  4.2538d+0, &
                  4.1483d+0,  3.8641d+0,  3.7244d+0,  3.6243d+0,  3.5589d+0,  3.5172d+0,  3.4973d+0, &
                  3.4715d+0,  3.7340d+0,  4.0316d+0,  3.9958d+0,  3.8687d+0,  3.8115d+0,  3.8862d+0, &
                  3.7379d+0,  4.7091d+0,  4.7156d+0,  4.5199d+0,  4.5542d+0,  4.5230d+0,  4.4959d+0, &
                  4.4750d+0,  4.4529d+0,  4.4361d+0,  4.1774d+0,  4.3774d+0,  4.2963d+0,  4.3406d+0, &
                  4.3159d+0,  4.3006d+0,  4.2910d+0,  4.1008d+0,  4.1568d+0,  4.0980d+0,  2.8110d+0, &
                  2.8520d+0,  3.7480d+0,  3.5105d+0,  3.4346d+0,  3.3461d+0,  3.1971d+0,  3.2326d+0, &
                  3.0329d+0,  2.9070d+0,  3.8823d+0,  3.7928d+0,  3.8264d+0,  3.7006d+0,  3.5797d+0, &
                  3.7141d+0,  3.4894d+0,  3.3326d+0,  4.3048d+0,  4.2217d+0,  4.0786d+0,  3.9900d+0, &
                  3.9357d+0,  3.6331d+0,  3.8333d+0,  3.7317d+0,  3.6957d+0,  3.6460d+0,  3.6197d+0 &
                  /)
            r0ab(2731:2800)=(/ &
                  3.5779d+0,  3.7909d+0,  3.7257d+0,  3.6476d+0,  3.5729d+0,  3.6304d+0,  3.4834d+0, &
                  4.4368d+0,  4.3921d+0,  4.2207d+0,  4.1133d+0,  3.8067d+0,  3.7421d+0,  3.6140d+0, &
                  3.5491d+0,  3.5077d+0,  3.4887d+0,  3.4623d+0,  3.6956d+0,  3.9568d+0,  3.8976d+0, &
                  3.8240d+0,  3.7684d+0,  3.8451d+0,  3.6949d+0,  4.6318d+0,  4.6559d+0,  4.4533d+0, &
                  4.4956d+0,  4.4641d+0,  4.4366d+0,  4.4155d+0,  4.3936d+0,  4.3764d+0,  4.1302d+0, &
                  4.3398d+0,  4.2283d+0,  4.2796d+0,  4.2547d+0,  4.2391d+0,  4.2296d+0,  4.0699d+0, &
                  4.1083d+0,  4.0319d+0,  3.9855d+0,  2.7676d+0,  2.8078d+0,  3.6725d+0,  3.4804d+0, &
                  3.3775d+0,  3.2411d+0,  3.1581d+0,  3.1983d+0,  2.9973d+0,  2.8705d+0,  3.8070d+0, &
                  3.7392d+0,  3.7668d+0,  3.6263d+0,  3.5402d+0,  3.6807d+0,  3.4545d+0,  3.2962d+0, &
                  4.2283d+0,  4.1698d+0,  4.0240d+0,  3.9341d+0,  3.8711d+0,  3.5489d+0,  3.7798d+0 &
                  /)
            r0ab(2801:2870)=(/ &
                  3.7000d+0,  3.6654d+0,  3.6154d+0,  3.5882d+0,  3.5472d+0,  3.7289d+0,  3.6510d+0, &
                  3.6078d+0,  3.5355d+0,  3.5963d+0,  3.4480d+0,  4.3587d+0,  4.3390d+0,  4.1635d+0, &
                  4.0536d+0,  3.7193d+0,  3.6529d+0,  3.5512d+0,  3.4837d+0,  3.4400d+0,  3.4191d+0, &
                  3.3891d+0,  3.6622d+0,  3.8934d+0,  3.8235d+0,  3.7823d+0,  3.7292d+0,  3.8106d+0, &
                  3.6589d+0,  4.5535d+0,  4.6013d+0,  4.3961d+0,  4.4423d+0,  4.4109d+0,  4.3835d+0, &
                  4.3625d+0,  4.3407d+0,  4.3237d+0,  4.0863d+0,  4.2835d+0,  4.1675d+0,  4.2272d+0, &
                  4.2025d+0,  4.1869d+0,  4.1774d+0,  4.0126d+0,  4.0460d+0,  3.9815d+0,  3.9340d+0, &
                  3.8955d+0,  2.6912d+0,  2.7604d+0,  3.6037d+0,  3.4194d+0,  3.3094d+0,  3.1710d+0, &
                  3.0862d+0,  3.1789d+0,  2.9738d+0,  2.8427d+0,  3.7378d+0,  3.6742d+0,  3.6928d+0, &
                  3.5512d+0,  3.4614d+0,  3.4087d+0,  3.4201d+0,  3.2607d+0,  4.1527d+0,  4.0977d+0 &
                  /)
            r0ab(2871:2940)=(/ &
                  3.9523d+0,  3.8628d+0,  3.8002d+0,  3.4759d+0,  3.7102d+0,  3.6466d+0,  3.6106d+0, &
                  3.5580d+0,  3.5282d+0,  3.4878d+0,  3.6547d+0,  3.5763d+0,  3.5289d+0,  3.5086d+0, &
                  3.5593d+0,  3.4099d+0,  4.2788d+0,  4.2624d+0,  4.0873d+0,  3.9770d+0,  3.6407d+0, &
                  3.5743d+0,  3.5178d+0,  3.4753d+0,  3.3931d+0,  3.3694d+0,  3.3339d+0,  3.6002d+0, &
                  3.8164d+0,  3.7478d+0,  3.7028d+0,  3.6952d+0,  3.7669d+0,  3.6137d+0,  4.4698d+0, &
                  4.5488d+0,  4.3168d+0,  4.3646d+0,  4.3338d+0,  4.3067d+0,  4.2860d+0,  4.2645d+0, &
                  4.2478d+0,  4.0067d+0,  4.2349d+0,  4.0958d+0,  4.1543d+0,  4.1302d+0,  4.1141d+0, &
                  4.1048d+0,  3.9410d+0,  3.9595d+0,  3.8941d+0,  3.8465d+0,  3.8089d+0,  3.7490d+0, &
                  2.7895d+0,  2.5849d+0,  3.6484d+0,  3.0162d+0,  3.1267d+0,  3.2125d+0,  3.0043d+0, &
                  2.9572d+0,  2.8197d+0,  2.7261d+0,  3.7701d+0,  3.2446d+0,  3.5239d+0,  3.4696d+0 &
                  /)
            r0ab(2941:3010)=(/ &
                  3.4261d+0,  3.3508d+0,  3.1968d+0,  3.0848d+0,  4.1496d+0,  3.6598d+0,  3.5111d+0, &
                  3.4199d+0,  3.3809d+0,  3.5382d+0,  3.2572d+0,  3.2100d+0,  3.1917d+0,  3.1519d+0, &
                  3.1198d+0,  3.1005d+0,  3.5071d+0,  3.5086d+0,  3.5073d+0,  3.4509d+0,  3.3120d+0, &
                  3.2082d+0,  4.2611d+0,  3.8117d+0,  3.6988d+0,  3.5646d+0,  3.6925d+0,  3.6295d+0, &
                  3.5383d+0,  3.4910d+0,  3.4625d+0,  3.4233d+0,  3.4007d+0,  3.2329d+0,  3.6723d+0, &
                  3.6845d+0,  3.6876d+0,  3.6197d+0,  3.4799d+0,  3.3737d+0,  4.4341d+0,  4.0525d+0, &
                  3.9011d+0,  3.8945d+0,  3.8635d+0,  3.8368d+0,  3.8153d+0,  3.7936d+0,  3.7758d+0, &
                  3.4944d+0,  3.4873d+0,  3.9040d+0,  3.7110d+0,  3.6922d+0,  3.6799d+0,  3.6724d+0, &
                  3.5622d+0,  3.6081d+0,  3.5426d+0,  3.4922d+0,  3.4498d+0,  3.3984d+0,  3.4456d+0, &
                  2.7522d+0,  2.5524d+0,  3.5742d+0,  2.9508d+0,  3.0751d+0,  3.0158d+0,  2.9644d+0 &
                  /)
            r0ab(3011:3080)=(/ &
                  2.8338d+0,  2.7891d+0,  2.6933d+0,  3.6926d+0,  3.1814d+0,  3.4528d+0,  3.4186d+0, &
                  3.3836d+0,  3.2213d+0,  3.1626d+0,  3.0507d+0,  4.0548d+0,  3.5312d+0,  3.4244d+0, &
                  3.3409d+0,  3.2810d+0,  3.4782d+0,  3.1905d+0,  3.1494d+0,  3.1221d+0,  3.1128d+0, &
                  3.0853d+0,  3.0384d+0,  3.4366d+0,  3.4562d+0,  3.4638d+0,  3.3211d+0,  3.2762d+0, &
                  3.1730d+0,  4.1632d+0,  3.6825d+0,  3.5822d+0,  3.4870d+0,  3.6325d+0,  3.5740d+0, &
                  3.4733d+0,  3.4247d+0,  3.3969d+0,  3.3764d+0,  3.3525d+0,  3.1984d+0,  3.5989d+0, &
                  3.6299d+0,  3.6433d+0,  3.4937d+0,  3.4417d+0,  3.3365d+0,  4.3304d+0,  3.9242d+0, &
                  3.7793d+0,  3.7623d+0,  3.7327d+0,  3.7071d+0,  3.6860d+0,  3.6650d+0,  3.6476d+0, &
                  3.3849d+0,  3.3534d+0,  3.8216d+0,  3.5870d+0,  3.5695d+0,  3.5584d+0,  3.5508d+0, &
                  3.4856d+0,  3.5523d+0,  3.4934d+0,  3.4464d+0,  3.4055d+0,  3.3551d+0,  3.3888d+0 &
                  /)
            r0ab(3081:3150)=(/ &
                  3.3525d+0,  2.7202d+0,  2.5183d+0,  3.4947d+0,  2.8731d+0,  3.0198d+0,  3.1457d+0, &
                  2.9276d+0,  2.7826d+0,  2.7574d+0,  2.6606d+0,  3.6090d+0,  3.0581d+0,  3.3747d+0, &
                  3.3677d+0,  3.3450d+0,  3.1651d+0,  3.1259d+0,  3.0147d+0,  3.9498d+0,  3.3857d+0, &
                  3.2917d+0,  3.2154d+0,  3.1604d+0,  3.4174d+0,  3.0735d+0,  3.0342d+0,  3.0096d+0, &
                  3.0136d+0,  2.9855d+0,  2.9680d+0,  3.3604d+0,  3.4037d+0,  3.4243d+0,  3.2633d+0, &
                  3.1810d+0,  3.1351d+0,  4.0557d+0,  3.5368d+0,  3.4526d+0,  3.3699d+0,  3.5707d+0, &
                  3.5184d+0,  3.4085d+0,  3.3595d+0,  3.3333d+0,  3.3143d+0,  3.3041d+0,  3.1094d+0, &
                  3.5193d+0,  3.5745d+0,  3.6025d+0,  3.4338d+0,  3.3448d+0,  3.2952d+0,  4.2158d+0, &
                  3.7802d+0,  3.6431d+0,  3.6129d+0,  3.5853d+0,  3.5610d+0,  3.5406d+0,  3.5204d+0, &
                  3.5036d+0,  3.2679d+0,  3.2162d+0,  3.7068d+0,  3.4483d+0,  3.4323d+0,  3.4221d+0 &
                  /)
            r0ab(3151:3220)=(/ &
                  3.4138d+0,  3.3652d+0,  3.4576d+0,  3.4053d+0,  3.3618d+0,  3.3224d+0,  3.2711d+0, &
                  3.3326d+0,  3.2950d+0,  3.2564d+0,  2.5315d+0,  2.6104d+0,  3.2734d+0,  3.2299d+0, &
                  3.1090d+0,  2.9942d+0,  2.9159d+0,  2.8324d+0,  2.8350d+0,  2.7216d+0,  3.3994d+0, &
                  3.4475d+0,  3.4354d+0,  3.3438d+0,  3.2807d+0,  3.2169d+0,  3.2677d+0,  3.1296d+0, &
                  3.7493d+0,  3.8075d+0,  3.6846d+0,  3.6104d+0,  3.5577d+0,  3.2052d+0,  3.4803d+0, &
                  3.4236d+0,  3.3845d+0,  3.3640d+0,  3.3365d+0,  3.3010d+0,  3.3938d+0,  3.3624d+0, &
                  3.3440d+0,  3.3132d+0,  3.4035d+0,  3.2754d+0,  3.8701d+0,  3.9523d+0,  3.8018d+0, &
                  3.7149d+0,  3.3673d+0,  3.3199d+0,  3.2483d+0,  3.2069d+0,  3.1793d+0,  3.1558d+0, &
                  3.1395d+0,  3.4097d+0,  3.5410d+0,  3.5228d+0,  3.5116d+0,  3.4921d+0,  3.4781d+0, &
                  3.4690d+0,  4.0420d+0,  4.1759d+0,  4.0078d+0,  4.0450d+0,  4.0189d+0,  3.9952d+0 &
                  /)
            r0ab(3221:3290)=(/ &
                  3.9770d+0,  3.9583d+0,  3.9434d+0,  3.7217d+0,  3.8228d+0,  3.7826d+0,  3.8640d+0, &
                  3.8446d+0,  3.8314d+0,  3.8225d+0,  3.6817d+0,  3.7068d+0,  3.6555d+0,  3.6159d+0, &
                  3.5831d+0,  3.5257d+0,  3.2133d+0,  3.1689d+0,  3.1196d+0,  3.3599d+0,  2.9852d+0, &
                  2.7881d+0,  3.5284d+0,  3.3493d+0,  3.6958d+0,  3.3642d+0,  3.1568d+0,  3.0055d+0, &
                  2.9558d+0,  2.8393d+0,  3.6287d+0,  3.5283d+0,  4.1511d+0,  3.8259d+0,  3.6066d+0, &
                  3.4527d+0,  3.3480d+0,  3.2713d+0,  3.9037d+0,  3.8361d+0,  3.8579d+0,  3.7311d+0, &
                  3.6575d+0,  3.5176d+0,  3.5693d+0,  3.5157d+0,  3.4814d+0,  3.4559d+0,  3.4445d+0, &
                  3.4160d+0,  4.1231d+0,  3.8543d+0,  3.6816d+0,  3.5602d+0,  3.4798d+0,  3.4208d+0, &
                  4.0542d+0,  4.0139d+0,  4.0165d+0,  3.9412d+0,  3.7698d+0,  3.6915d+0,  3.6043d+0, &
                  3.5639d+0,  3.5416d+0,  3.5247d+0,  3.5153d+0,  3.5654d+0,  4.2862d+0,  4.0437d+0 &
                  /)
            r0ab(3291:3360)=(/ &
                  3.8871d+0,  3.7741d+0,  3.6985d+0,  3.6413d+0,  4.2345d+0,  4.3663d+0,  4.3257d+0, &
                  4.0869d+0,  4.0612d+0,  4.0364d+0,  4.0170d+0,  3.9978d+0,  3.9834d+0,  3.9137d+0, &
                  3.8825d+0,  3.8758d+0,  3.9143d+0,  3.8976d+0,  3.8864d+0,  3.8768d+0,  3.9190d+0, &
                  4.1613d+0,  4.0566d+0,  3.9784d+0,  3.9116d+0,  3.8326d+0,  3.7122d+0,  3.6378d+0, &
                  3.5576d+0,  3.5457d+0,  4.3127d+0,  3.1160d+0,  2.8482d+0,  4.0739d+0,  3.3599d+0, &
                  3.5698d+0,  3.5366d+0,  3.2854d+0,  3.1039d+0,  2.9953d+0,  2.9192d+0,  4.1432d+0, &
                  3.5320d+0,  3.9478d+0,  4.0231d+0,  3.7509d+0,  3.5604d+0,  3.4340d+0,  3.3426d+0, &
                  4.3328d+0,  3.8288d+0,  3.7822d+0,  3.7909d+0,  3.6907d+0,  3.6864d+0,  3.5793d+0, &
                  3.5221d+0,  3.4883d+0,  3.4649d+0,  3.4514d+0,  3.4301d+0,  3.9256d+0,  4.0596d+0, &
                  3.8307d+0,  3.6702d+0,  3.5651d+0,  3.4884d+0,  4.4182d+0,  4.2516d+0,  3.9687d+0 &
                  /)
            r0ab(3361:3430)=(/ &
                  3.9186d+0,  3.9485d+0,  3.8370d+0,  3.7255d+0,  3.6744d+0,  3.6476d+0,  3.6295d+0, &
                  3.6193d+0,  3.5659d+0,  4.0663d+0,  4.2309d+0,  4.0183d+0,  3.8680d+0,  3.7672d+0, &
                  3.6923d+0,  4.5240d+0,  4.4834d+0,  4.1570d+0,  4.3204d+0,  4.2993d+0,  4.2804d+0, &
                  4.2647d+0,  4.2481d+0,  4.2354d+0,  3.8626d+0,  3.8448d+0,  4.2267d+0,  4.1799d+0, &
                  4.1670d+0,  3.8738d+0,  3.8643d+0,  3.8796d+0,  4.0575d+0,  4.0354d+0,  3.9365d+0, &
                  3.8611d+0,  3.7847d+0,  3.7388d+0,  3.6826d+0,  3.6251d+0,  3.5492d+0,  4.0889d+0, &
                  4.2764d+0,  3.1416d+0,  2.8325d+0,  3.7735d+0,  3.3787d+0,  3.4632d+0,  3.5923d+0, &
                  3.3214d+0,  3.1285d+0,  3.0147d+0,  2.9366d+0,  3.8527d+0,  3.5602d+0,  3.8131d+0, &
                  3.8349d+0,  3.7995d+0,  3.5919d+0,  3.4539d+0,  3.3540d+0,  4.0654d+0,  3.8603d+0, &
                  3.7972d+0,  3.7358d+0,  3.7392d+0,  3.8157d+0,  3.6055d+0,  3.5438d+0,  3.5089d+0 &
                  /)
            r0ab(3431:3500)=(/ &
                  3.4853d+0,  3.4698d+0,  3.4508d+0,  3.7882d+0,  3.8682d+0,  3.8837d+0,  3.7055d+0, &
                  3.5870d+0,  3.5000d+0,  4.1573d+0,  4.0005d+0,  3.9568d+0,  3.8936d+0,  3.9990d+0, &
                  3.9433d+0,  3.8172d+0,  3.7566d+0,  3.7246d+0,  3.7033d+0,  3.6900d+0,  3.5697d+0, &
                  3.9183d+0,  4.0262d+0,  4.0659d+0,  3.8969d+0,  3.7809d+0,  3.6949d+0,  4.2765d+0, &
                  4.2312d+0,  4.1401d+0,  4.0815d+0,  4.0580d+0,  4.0369d+0,  4.0194d+0,  4.0017d+0, &
                  3.9874d+0,  3.8312d+0,  3.8120d+0,  3.9454d+0,  3.9210d+0,  3.9055d+0,  3.8951d+0, &
                  3.8866d+0,  3.8689d+0,  3.9603d+0,  3.9109d+0,  3.9122d+0,  3.8233d+0,  3.7438d+0, &
                  3.7436d+0,  3.6981d+0,  3.6555d+0,  3.5452d+0,  3.9327d+0,  4.0658d+0,  4.1175d+0, &
                  2.9664d+0,  2.8209d+0,  3.5547d+0,  3.3796d+0,  3.3985d+0,  3.3164d+0,  3.2364d+0, &
                  3.1956d+0,  3.0370d+0,  2.9313d+0,  3.6425d+0,  3.5565d+0,  3.7209d+0,  3.7108d+0 &
                  /)
            r0ab(3501:3570)=(/ &
                  3.6639d+0,  3.6484d+0,  3.4745d+0,  3.3492d+0,  3.8755d+0,  4.2457d+0,  3.7758d+0, &
                  3.7161d+0,  3.6693d+0,  3.6155d+0,  3.5941d+0,  3.5643d+0,  3.5292d+0,  3.4950d+0, &
                  3.4720d+0,  3.4503d+0,  3.6936d+0,  3.7392d+0,  3.7388d+0,  3.7602d+0,  3.6078d+0, &
                  3.4960d+0,  3.9800d+0,  4.3518d+0,  4.2802d+0,  3.8580d+0,  3.8056d+0,  3.7527d+0, &
                  3.7019d+0,  3.6615d+0,  3.5768d+0,  3.5330d+0,  3.5038d+0,  3.5639d+0,  3.8192d+0, &
                  3.8883d+0,  3.9092d+0,  3.9478d+0,  3.7995d+0,  3.6896d+0,  4.1165d+0,  4.5232d+0, &
                  4.4357d+0,  4.4226d+0,  4.4031d+0,  4.3860d+0,  4.3721d+0,  4.3580d+0,  4.3466d+0, &
                  4.2036d+0,  4.2037d+0,  3.8867d+0,  4.2895d+0,  4.2766d+0,  4.2662d+0,  4.2598d+0, &
                  3.8408d+0,  3.9169d+0,  3.8681d+0,  3.8250d+0,  3.7855d+0,  3.7501d+0,  3.6753d+0, &
                  3.5499d+0,  3.4872d+0,  3.5401d+0,  3.8288d+0,  3.9217d+0,  3.9538d+0,  4.0054d+0 &
                  /)
            r0ab(3571:3640)=(/ &
                  2.8388d+0,  2.7890d+0,  3.4329d+0,  3.5593d+0,  3.3488d+0,  3.2486d+0,  3.1615d+0, &
                  3.1000d+0,  3.0394d+0,  2.9165d+0,  3.5267d+0,  3.7479d+0,  3.6650d+0,  3.6263d+0, &
                  3.5658d+0,  3.5224d+0,  3.4762d+0,  3.3342d+0,  3.7738d+0,  4.0333d+0,  3.9568d+0, &
                  3.8975d+0,  3.8521d+0,  3.4929d+0,  3.7830d+0,  3.7409d+0,  3.7062d+0,  3.6786d+0, &
                  3.6471d+0,  3.6208d+0,  3.6337d+0,  3.6519d+0,  3.6363d+0,  3.6278d+0,  3.6110d+0, &
                  3.4825d+0,  3.8795d+0,  4.1448d+0,  4.0736d+0,  4.0045d+0,  3.6843d+0,  3.6291d+0, &
                  3.5741d+0,  3.5312d+0,  3.4974d+0,  3.4472d+0,  3.4034d+0,  3.7131d+0,  3.7557d+0, &
                  3.7966d+0,  3.8005d+0,  3.8068d+0,  3.8015d+0,  3.6747d+0,  4.0222d+0,  4.3207d+0, &
                  4.2347d+0,  4.2191d+0,  4.1990d+0,  4.1811d+0,  4.1666d+0,  4.1521d+0,  4.1401d+0, &
                  3.9970d+0,  3.9943d+0,  3.9592d+0,  4.0800d+0,  4.0664d+0,  4.0559d+0,  4.0488d+0 &
                  /)
            r0ab(3641:3710)=(/ &
                  3.9882d+0,  4.0035d+0,  3.9539d+0,  3.9138d+0,  3.8798d+0,  3.8355d+0,  3.5359d+0, &
                  3.4954d+0,  3.3962d+0,  3.5339d+0,  3.7595d+0,  3.8250d+0,  3.8408d+0,  3.8600d+0, &
                  3.8644d+0,  2.7412d+0,  2.7489d+0,  3.3374d+0,  3.3950d+0,  3.3076d+0,  3.1910d+0, &
                  3.0961d+0,  3.0175d+0,  3.0280d+0,  2.8929d+0,  3.4328d+0,  3.5883d+0,  3.6227d+0, &
                  3.5616d+0,  3.4894d+0,  3.4241d+0,  3.3641d+0,  3.3120d+0,  3.6815d+0,  3.8789d+0, &
                  3.8031d+0,  3.7413d+0,  3.6939d+0,  3.4010d+0,  3.6225d+0,  3.5797d+0,  3.5443d+0, &
                  3.5139d+0,  3.4923d+0,  3.4642d+0,  3.5860d+0,  3.5849d+0,  3.5570d+0,  3.5257d+0, &
                  3.4936d+0,  3.4628d+0,  3.7874d+0,  3.9916d+0,  3.9249d+0,  3.8530d+0,  3.5932d+0, &
                  3.5355d+0,  3.4757d+0,  3.4306d+0,  3.3953d+0,  3.3646d+0,  3.3390d+0,  3.5637d+0, &
                  3.7053d+0,  3.7266d+0,  3.7177d+0,  3.6996d+0,  3.6775d+0,  3.6558d+0,  3.9331d+0 &
                  /)
            r0ab(3711:3780)=(/ &
                  4.1655d+0,  4.0879d+0,  4.0681d+0,  4.0479d+0,  4.0299d+0,  4.0152d+0,  4.0006d+0, &
                  3.9883d+0,  3.8500d+0,  3.8359d+0,  3.8249d+0,  3.9269d+0,  3.9133d+0,  3.9025d+0, &
                  3.8948d+0,  3.8422d+0,  3.8509d+0,  3.7990d+0,  3.7570d+0,  3.7219d+0,  3.6762d+0, &
                  3.4260d+0,  3.3866d+0,  3.3425d+0,  3.5294d+0,  3.7022d+0,  3.7497d+0,  3.7542d+0, &
                  3.7494d+0,  3.7370d+0,  3.7216d+0,  3.4155d+0,  3.0522d+0,  4.2541d+0,  3.8218d+0, &
                  4.0438d+0,  3.5875d+0,  3.3286d+0,  3.1682d+0,  3.0566d+0,  2.9746d+0,  4.3627d+0, &
                  4.0249d+0,  4.6947d+0,  4.1718d+0,  3.8639d+0,  3.6735d+0,  3.5435d+0,  3.4479d+0, &
                  4.6806d+0,  4.3485d+0,  4.2668d+0,  4.1690d+0,  4.1061d+0,  4.1245d+0,  4.0206d+0, &
                  3.9765d+0,  3.9458d+0,  3.9217d+0,  3.9075d+0,  3.8813d+0,  3.9947d+0,  4.1989d+0, &
                  3.9507d+0,  3.7960d+0,  3.6925d+0,  3.6150d+0,  4.8535d+0,  4.5642d+0,  4.4134d+0 &
                  /)
            r0ab(3781:3850)=(/ &
                  4.3688d+0,  4.3396d+0,  4.2879d+0,  4.2166d+0,  4.1888d+0,  4.1768d+0,  4.1660d+0, &
                  4.1608d+0,  4.0745d+0,  4.2289d+0,  4.4863d+0,  4.2513d+0,  4.0897d+0,  3.9876d+0, &
                  3.9061d+0,  5.0690d+0,  5.0446d+0,  4.6186d+0,  4.6078d+0,  4.5780d+0,  4.5538d+0, &
                  4.5319d+0,  4.5101d+0,  4.4945d+0,  4.1912d+0,  4.2315d+0,  4.5534d+0,  4.4373d+0, &
                  4.4224d+0,  4.4120d+0,  4.4040d+0,  4.2634d+0,  4.7770d+0,  4.6890d+0,  4.6107d+0, &
                  4.5331d+0,  4.4496d+0,  4.4082d+0,  4.3095d+0,  4.2023d+0,  4.0501d+0,  4.2595d+0, &
                  4.5497d+0,  4.3056d+0,  4.1506d+0,  4.0574d+0,  3.9725d+0,  5.0796d+0,  3.0548d+0, &
                  3.3206d+0,  3.8132d+0,  3.9720d+0,  3.7675d+0,  3.7351d+0,  3.5167d+0,  3.5274d+0, &
                  3.3085d+0,  3.1653d+0,  3.9500d+0,  4.1730d+0,  4.0613d+0,  4.1493d+0,  3.8823d+0, &
                  4.0537d+0,  3.8200d+0,  3.6582d+0,  4.3422d+0,  4.5111d+0,  4.3795d+0,  4.3362d+0 &
                  /)
            r0ab(3851:3920)=(/ &
                  4.2751d+0,  3.7103d+0,  4.1973d+0,  4.1385d+0,  4.1129d+0,  4.0800d+0,  4.0647d+0, &
                  4.0308d+0,  4.0096d+0,  4.1619d+0,  3.9360d+0,  4.1766d+0,  3.9705d+0,  3.8262d+0, &
                  4.5348d+0,  4.7025d+0,  4.5268d+0,  4.5076d+0,  3.9562d+0,  3.9065d+0,  3.8119d+0, &
                  3.7605d+0,  3.7447d+0,  3.7119d+0,  3.6916d+0,  4.1950d+0,  4.2110d+0,  4.3843d+0, &
                  4.1631d+0,  4.4427d+0,  4.2463d+0,  4.1054d+0,  4.7693d+0,  5.0649d+0,  4.7365d+0, &
                  4.7761d+0,  4.7498d+0,  4.7272d+0,  4.7076d+0,  4.6877d+0,  4.6730d+0,  4.4274d+0, &
                  4.5473d+0,  4.5169d+0,  4.5975d+0,  4.5793d+0,  4.5667d+0,  4.5559d+0,  4.3804d+0, &
                  4.6920d+0,  4.6731d+0,  4.6142d+0,  4.5600d+0,  4.4801d+0,  4.0149d+0,  3.8856d+0, &
                  3.7407d+0,  4.1545d+0,  4.2253d+0,  4.4229d+0,  4.1923d+0,  4.5022d+0,  4.3059d+0, &
                  4.1591d+0,  4.7883d+0,  4.9294d+0,  3.3850d+0,  3.4208d+0,  3.7004d+0,  3.8800d+0 &
                  /)
            r0ab(3921:3990)=(/ &
                  3.9886d+0,  3.9040d+0,  3.6719d+0,  3.6547d+0,  3.4625d+0,  3.3370d+0,  3.8394d+0, &
                  4.0335d+0,  4.2373d+0,  4.3023d+0,  4.0306d+0,  4.1408d+0,  3.9297d+0,  3.7857d+0, &
                  4.1907d+0,  4.3230d+0,  4.2664d+0,  4.2173d+0,  4.1482d+0,  3.6823d+0,  4.0711d+0, &
                  4.0180d+0,  4.0017d+0,  3.9747d+0,  3.9634d+0,  3.9383d+0,  4.1993d+0,  4.3205d+0, &
                  4.0821d+0,  4.2547d+0,  4.0659d+0,  3.9359d+0,  4.3952d+0,  4.5176d+0,  4.3888d+0, &
                  4.3607d+0,  3.9583d+0,  3.9280d+0,  3.8390d+0,  3.7971d+0,  3.7955d+0,  3.7674d+0, &
                  3.7521d+0,  4.1062d+0,  4.3633d+0,  4.2991d+0,  4.2767d+0,  4.4857d+0,  4.3039d+0, &
                  4.1762d+0,  4.6197d+0,  4.8654d+0,  4.6633d+0,  4.5878d+0,  4.5640d+0,  4.5422d+0, &
                  4.5231d+0,  4.5042d+0,  4.4901d+0,  4.3282d+0,  4.3978d+0,  4.3483d+0,  4.4202d+0, &
                  4.4039d+0,  4.3926d+0,  4.3807d+0,  4.2649d+0,  4.6135d+0,  4.5605d+0,  4.5232d+0 &
                  /)
            r0ab(3991:4060)=(/ &
                  4.4676d+0,  4.3948d+0,  4.0989d+0,  3.9864d+0,  3.8596d+0,  4.0942d+0,  4.2720d+0, &
                  4.3270d+0,  4.3022d+0,  4.5410d+0,  4.3576d+0,  4.2235d+0,  4.6545d+0,  4.7447d+0, &
                  4.7043d+0,  3.0942d+0,  3.2075d+0,  3.5152d+0,  3.6659d+0,  3.8289d+0,  3.7459d+0, &
                  3.5156d+0,  3.5197d+0,  3.3290d+0,  3.2069d+0,  3.6702d+0,  3.8448d+0,  4.0340d+0, &
                  3.9509d+0,  3.8585d+0,  3.9894d+0,  3.7787d+0,  3.6365d+0,  4.1425d+0,  4.1618d+0, &
                  4.0940d+0,  4.0466d+0,  3.9941d+0,  3.5426d+0,  3.8952d+0,  3.8327d+0,  3.8126d+0, &
                  3.7796d+0,  3.7635d+0,  3.7356d+0,  4.0047d+0,  3.9655d+0,  3.9116d+0,  4.1010d+0, &
                  3.9102d+0,  3.7800d+0,  4.2964d+0,  4.3330d+0,  4.2622d+0,  4.2254d+0,  3.8195d+0, &
                  3.7560d+0,  3.6513d+0,  3.5941d+0,  3.5810d+0,  3.5420d+0,  3.5178d+0,  3.8861d+0, &
                  4.1459d+0,  4.1147d+0,  4.0772d+0,  4.3120d+0,  4.1207d+0,  3.9900d+0,  4.4733d+0 &
                  /)
            r0ab(4061:4130)=(/ &
                  4.6157d+0,  4.4580d+0,  4.4194d+0,  4.3954d+0,  4.3739d+0,  4.3531d+0,  4.3343d+0, &
                  4.3196d+0,  4.2140d+0,  4.2339d+0,  4.1738d+0,  4.2458d+0,  4.2278d+0,  4.2158d+0, &
                  4.2039d+0,  4.1658d+0,  4.3595d+0,  4.2857d+0,  4.2444d+0,  4.1855d+0,  4.1122d+0, &
                  3.7839d+0,  3.6879d+0,  3.5816d+0,  3.8633d+0,  4.1585d+0,  4.1402d+0,  4.1036d+0, &
                  4.3694d+0,  4.1735d+0,  4.0368d+0,  4.5095d+0,  4.5538d+0,  4.5240d+0,  4.4252d+0, &
                  3.0187d+0,  3.1918d+0,  3.5127d+0,  3.6875d+0,  3.7404d+0,  3.6943d+0,  3.4702d+0, &
                  3.4888d+0,  3.2914d+0,  3.1643d+0,  3.6669d+0,  3.8724d+0,  3.9940d+0,  4.0816d+0, &
                  3.8054d+0,  3.9661d+0,  3.7492d+0,  3.6024d+0,  4.0428d+0,  4.1951d+0,  4.1466d+0, &
                  4.0515d+0,  4.0075d+0,  3.5020d+0,  3.9158d+0,  3.8546d+0,  3.8342d+0,  3.8008d+0, &
                  3.7845d+0,  3.7549d+0,  3.9602d+0,  3.8872d+0,  3.8564d+0,  4.0793d+0,  3.8835d+0 &
                  /)
            r0ab(4131:4200)=(/ &
                  3.7495d+0,  4.2213d+0,  4.3704d+0,  4.3300d+0,  4.2121d+0,  3.7643d+0,  3.7130d+0, &
                  3.6144d+0,  3.5599d+0,  3.5474d+0,  3.5093d+0,  3.4853d+0,  3.9075d+0,  4.1115d+0, &
                  4.0473d+0,  4.0318d+0,  4.2999d+0,  4.1050d+0,  3.9710d+0,  4.4320d+0,  4.6706d+0, &
                  4.5273d+0,  4.4581d+0,  4.4332d+0,  4.4064d+0,  4.3873d+0,  4.3684d+0,  4.3537d+0, &
                  4.2728d+0,  4.2549d+0,  4.2032d+0,  4.2794d+0,  4.2613d+0,  4.2491d+0,  4.2375d+0, &
                  4.2322d+0,  4.3665d+0,  4.3061d+0,  4.2714d+0,  4.2155d+0,  4.1416d+0,  3.7660d+0, &
                  3.6628d+0,  3.5476d+0,  3.8790d+0,  4.1233d+0,  4.0738d+0,  4.0575d+0,  4.3575d+0, &
                  4.1586d+0,  4.0183d+0,  4.4593d+0,  4.5927d+0,  4.4865d+0,  4.3813d+0,  4.4594d+0, &
                  2.9875d+0,  3.1674d+0,  3.4971d+0,  3.6715d+0,  3.7114d+0,  3.6692d+0,  3.4446d+0, &
                  3.4676d+0,  3.2685d+0,  3.1405d+0,  3.6546d+0,  3.8579d+0,  3.9637d+0,  4.0581d+0 &
                  /)
            r0ab(4201:4270)=(/ &
                  3.7796d+0,  3.9463d+0,  3.7275d+0,  3.5792d+0,  4.0295d+0,  4.1824d+0,  4.1247d+0, &
                  4.0357d+0,  3.9926d+0,  3.4827d+0,  3.9007d+0,  3.8392d+0,  3.8191d+0,  3.7851d+0, &
                  3.7687d+0,  3.7387d+0,  3.9290d+0,  3.8606d+0,  3.8306d+0,  4.0601d+0,  3.8625d+0, &
                  3.7269d+0,  4.2062d+0,  4.3566d+0,  4.3022d+0,  4.1929d+0,  3.7401d+0,  3.6888d+0, &
                  3.5900d+0,  3.5350d+0,  3.5226d+0,  3.4838d+0,  3.4594d+0,  3.8888d+0,  4.0813d+0, &
                  4.0209d+0,  4.0059d+0,  4.2810d+0,  4.0843d+0,  3.9486d+0,  4.4162d+0,  4.6542d+0, &
                  4.5005d+0,  4.4444d+0,  4.4196d+0,  4.3933d+0,  4.3741d+0,  4.3552d+0,  4.3406d+0, &
                  4.2484d+0,  4.2413d+0,  4.1907d+0,  4.2656d+0,  4.2474d+0,  4.2352d+0,  4.2236d+0, &
                  4.2068d+0,  4.3410d+0,  4.2817d+0,  4.2479d+0,  4.1921d+0,  4.1182d+0,  3.7346d+0, &
                  3.6314d+0,  3.5168d+0,  3.8582d+0,  4.0927d+0,  4.0469d+0,  4.0313d+0,  4.3391d+0 &
                  /)
            r0ab(4271:4340)=(/ &
                  4.1381d+0,  3.9962d+0,  4.4429d+0,  4.5787d+0,  4.4731d+0,  4.3588d+0,  4.4270d+0, &
                  4.3957d+0,  2.9659d+0,  3.1442d+0,  3.4795d+0,  3.6503d+0,  3.6814d+0,  3.6476d+0, &
                  3.4222d+0,  3.4491d+0,  3.2494d+0,  3.1209d+0,  3.6324d+0,  3.8375d+0,  3.9397d+0, &
                  3.8311d+0,  3.7581d+0,  3.9274d+0,  3.7085d+0,  3.5598d+0,  4.0080d+0,  4.1641d+0, &
                  4.1057d+0,  4.0158d+0,  3.9726d+0,  3.4667d+0,  3.8802d+0,  3.8188d+0,  3.7989d+0, &
                  3.7644d+0,  3.7474d+0,  3.7173d+0,  3.9049d+0,  3.8424d+0,  3.8095d+0,  4.0412d+0, &
                  3.8436d+0,  3.7077d+0,  4.1837d+0,  4.3366d+0,  4.2816d+0,  4.1686d+0,  3.7293d+0, &
                  3.6709d+0,  3.5700d+0,  3.5153d+0,  3.5039d+0,  3.4684d+0,  3.4437d+0,  3.8663d+0, &
                  4.0575d+0,  4.0020d+0,  3.9842d+0,  4.2612d+0,  4.0643d+0,  3.9285d+0,  4.3928d+0, &
                  4.6308d+0,  4.4799d+0,  4.4244d+0,  4.3996d+0,  4.3737d+0,  4.3547d+0,  4.3358d+0 &
                  /)
            r0ab(4341:4410)=(/ &
                  4.3212d+0,  4.2275d+0,  4.2216d+0,  4.1676d+0,  4.2465d+0,  4.2283d+0,  4.2161d+0, &
                  4.2045d+0,  4.1841d+0,  4.3135d+0,  4.2562d+0,  4.2226d+0,  4.1667d+0,  4.0932d+0, &
                  3.7134d+0,  3.6109d+0,  3.4962d+0,  3.8352d+0,  4.0688d+0,  4.0281d+0,  4.0099d+0, &
                  4.3199d+0,  4.1188d+0,  3.9768d+0,  4.4192d+0,  4.5577d+0,  4.4516d+0,  4.3365d+0, &
                  4.4058d+0,  4.3745d+0,  4.3539d+0,  2.8763d+0,  3.1294d+0,  3.5598d+0,  3.7465d+0, &
                  3.5659d+0,  3.5816d+0,  3.3599d+0,  3.4024d+0,  3.1877d+0,  3.0484d+0,  3.7009d+0, &
                  3.9451d+0,  3.8465d+0,  3.9873d+0,  3.7079d+0,  3.9083d+0,  3.6756d+0,  3.5150d+0, &
                  4.0829d+0,  4.2780d+0,  4.1511d+0,  4.1260d+0,  4.0571d+0,  3.4865d+0,  3.9744d+0, &
                  3.9150d+0,  3.8930d+0,  3.8578d+0,  3.8402d+0,  3.8073d+0,  3.7977d+0,  4.0036d+0, &
                  3.7604d+0,  4.0288d+0,  3.8210d+0,  3.6757d+0,  4.2646d+0,  4.4558d+0,  4.2862d+0 &
                  /)
            r0ab(4411:4465)=(/ &
                  4.2122d+0,  3.7088d+0,  3.6729d+0,  3.5800d+0,  3.5276d+0,  3.5165d+0,  3.4783d+0, &
                  3.4539d+0,  3.9553d+0,  3.9818d+0,  4.2040d+0,  3.9604d+0,  4.2718d+0,  4.0689d+0, &
                  3.9253d+0,  4.4869d+0,  4.7792d+0,  4.4918d+0,  4.5342d+0,  4.5090d+0,  4.4868d+0, &
                  4.4680d+0,  4.4486d+0,  4.4341d+0,  4.2023d+0,  4.3122d+0,  4.2710d+0,  4.3587d+0, &
                  4.3407d+0,  4.3281d+0,  4.3174d+0,  4.1499d+0,  4.3940d+0,  4.3895d+0,  4.3260d+0, &
                  4.2725d+0,  4.1961d+0,  3.7361d+0,  3.6193d+0,  3.4916d+0,  3.9115d+0,  3.9914d+0, &
                  3.9809d+0,  3.9866d+0,  4.3329d+0,  4.1276d+0,  3.9782d+0,  4.5097d+0,  4.6769d+0, &
                  4.5158d+0,  4.3291d+0,  4.3609d+0,  4.3462d+0,  4.3265d+0,  4.4341d+0 &
                  /)

            k=0
            do i=1,max_elem
                  do j=1,i
                        k=k+1
                        r(i,j)=r0ab(k)/autoang
                        r(j,i)=r0ab(k)/autoang
                  enddo
            enddo

      end subroutine setr0ab

      
      subroutine rdcoord(xyz, iat)
            real(F64), dimension(:, :), intent(out) :: xyz
            integer, dimension(:), intent(out)             :: iat

            integer :: k, n

            n = 0
            do k = 1, NATOM
                  if (.not. isdummy(k)) then
                        n = n + 1
                        xyz(:, n) = ATOMR(:, k)
                        iat(n) = INUCLZ(k)
                  end if
            end do
      end subroutine rdcoord
end module dftd3
