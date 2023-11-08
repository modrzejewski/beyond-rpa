! ----------------------------------------------------------------------
!       ONE-ELECTRON INTEGRALS OF AN EFFECTIVE CORE POTENTIAL
! ----------------------------------------------------------------------
! This code employs the algorithms presented in Refs. [1] and [2]. 
! The implementation is verified against GAMESS release 1 May 2012,
! DALTON 2011, and Q-Chem 4.
!
!                           DEFINITIONS
!
! Pseudopotential
! ---------------
! U(r) = U_{local}(r) + Sum_{l=0}^{lmax}Sum_{m=-l}^l |S_{lm}>U_l(r)<S_{lm}|
!
! U(r) 
!       The value of the pseudopotential at the radius r. The
!       distance r is measured from the center A of the pseudopotential.
!       The total pseudopotential of a many-atom system is a
!       superposition of pseudopotentials located on each atom.
!
! U_l(r)
!       The projective part of U(r). U_l(r) acts only on functions that
!       have the angular momentum l with respect to the center A. An
!       orbital which is centered on an atom different from A can be 
!       decomposed into a sum of functions with different l. The 
!       projection operator, Sum_{m=-l}^l |S_{lm}> <S_{lm|, selects
!       from this sum the contributions with the angular momentum l.
!
! U_{local}(r)
!       The local (multiplicative) part of the pseudopotential. U_L(r)
!       acts on any orbital, regardless of its angular momentum with
!       respect to the center A.
!
! Mathematical form of the U_l(r) and U_{local}(r) functions
! ----------------------------------------------------------
! U_l(r) = Sum_{k=1}^{ngauss} C_{kl} r^{n_{kl}} Exp(-Alpha_{kl} r^2)
! (U_local(r) has the same form.)
!
! C_{KL}
!       The linear expansion coefficient of U_l(r). C_{KL} is stored in
!       the global array ECP_COEFF.
!
! r^{n_{kl}}, 0 <= n_{kl} <= 2
!       Radial prefactor. The exponent n_{kl} is stored in the global
!       array ECP_NKL. r^{n_{kl}} includes the r^2 part of the Jacobian
!       of Cartesian->spherical transformation.
!
! Alpha_{kl}
!       Gaussian exponent stored in the global array ECP_EXPN.
!
! Pseudopotential input format
! ----------------------------
! A textfile containing the pseudopotential parameters should 
! contain the following lines.
!
! $ecp
! X-ecp ncoreel lmax+1
! g_local comment
!   C_{1, local}  n_{1, local} Alpha_{1, local}
!   C_{2, local}  n_{1, local} Alpha_{2, local}
!                      .
!                      .
!        (Definition of the local component of the pseudopotential)
!                      .
!   C_{g_local, local} n_{n_local, local} Alpha_{n_local, local}
! g_0 comment
!   C_{1, 0}  n_{1, 0} Alpha_{1, 0}
!   C_{2, 0}  n_{1, 0} Alpha_{2, 0}
!                      .
!                      .
!        (Definition of the S-component of the pseudopotential)
!                      .
!   C_{g_0, 0} n_{g_0, 0} Alpha_{g_0, 0}
!       .
!       .
!       .
! g_{lmax} comment
!   C_{1, 0}  n_{1, 0} Alpha_{1, 0}
!   C_{2, 0}  n_{1, 0} Alpha_{2, 0}
!                      .
!         (Definition of the lmax-component of the pseudopotential)
!                      .
!                      .
!   C_{g_{lmax}, 0} n_{g_{lmax}, 0} Alpha_{g_{lmax}, 0}
! $end
! 
! X
!          A two-letter symbol of the chemical element for which the
!          pseudopotential is defined.
! ncoreel
!          Number of core electrons represented by the pseudopotential.
!
! If the pseudopotential does not contain a local component, the local
! part of the definition should contain a single Gaussian function
! with zero linear coefficient, e.g.,
!  $ECP 
!  AU-ECP   60     4
!  1      ----- local potential     -----
!      0.000000000      2      1.000000000 
!                    .
!                    .
!                    .
!  (angular s, p, d, f components)
!  $END
! ----------------------------------------------------------------------
! 1. Moreno-Flores, R., Alvarez-Mendez, R., Vela, A., and
!    Koster, A.M., Half-Numerical Evaluation of Pseudopotential
!    Integrals, J. Comput. Chem. 27, 1009 (2006);
!    doi: 10.1002/jcc.20410
! 2. McMurchie, L. E., and Davidson, E. R., Calculation of Integrals
!    over ab initio Pseudopotentials, J. Comp. Phys. 44, 289 (1981);
!    doi: 10.1016/0021-9991(81)90053-X
!
module Pseudopotential
      use math_constants
      use arithmetic
      use spherh
      use periodic
      use display
      use gto
      use io
      use string
      use chebinterp
      use basis_sets
      use sys_definitions
      use PseudopotentialData
      
      implicit none

contains

      subroutine pp_ZNumbers(System, ECPFile)
            type(TSystem), intent(inout)  :: System
            type(TStringList), intent(in) :: ECPFile

            character(:), allocatable :: param_file
            integer, dimension(:), allocatable :: ZList, AtomElementMap, ZCount            
            integer :: NElements
            integer :: i, Z
            integer :: ngauss, lmax, ncoreel
            logical :: spin_orbit
            character(:), allocatable :: citation
            integer :: NGaussSum
            integer, dimension(KNOWN_ELEMENTS) :: CoreElectrons

            if (.not. allocated(System%ZNumbersECP)) allocate(System%ZNumbersECP(System%NAtoms))
            CoreElectrons = 0
            NGaussSum = 0
            allocate(AtomElementMap(System%NAtoms))
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_ALL_ATOMS)
            do i = 1, NElements
                  Z = ZList(i)
                  param_file = ECPFile%get(Z)
                  call pp_queryecp(param_file, Z, lmax, ngauss, ncoreel, spin_orbit, citation)
                  if (ngauss > 0) then
                        CoreElectrons(Z) = ncoreel
                        NGaussSum = NGaussSum + ngauss
                  end if
            end do
            System%ECPCharges = (NGaussSum > 0)
            do i = 1, System%NAtoms
                  Z = System%ZNumbers(i)
                  System%ZNumbersECP(i) = Z - CoreElectrons(Z)
            end do
      end subroutine pp_ZNumbers
      

      subroutine pp_Init(AOBasis, System, ecp_path, calcgrad, PrintOutParams)
            ! --------------------------------------------------------------
            ! Initialize ECPINT module
            ! --------------------------------------------------------------
            ! 1. Moreno-Flores, R., Alvarez-Mendez, R., Vela, A., and
            !    Koster, A.M., Half-Numerical Evaluation of Pseudopotential
            !    Integrals, J. Comput. Chem. 27, 1009 (2006)
            ! 2. McMurchie, L. E., and Davidson, E. R., Calculation of Integrals
            !    over ab initio Pseudopotentials, J. Comp. Phys. 44, 289 (1981)
            !
            type(TAOBasis), intent(in)        :: AOBasis
            type(TSystem), intent(in)         :: System
            type(tstringlist), intent(in)     :: ecp_path
            logical, intent(in)               :: calcgrad
            logical, intent(in)               :: PrintOutParams

            integer :: alpha, alpx, alpy, alpz
            integer :: incl1, lambda, mu, i1, i2, i3
            integer :: idx, k0
            integer :: pp_lmax
            integer :: ngauss, ngausstot
            integer :: ncoreel
            integer :: lmax
            integer :: lambdamax
            integer :: l, m, mm
            integer :: pos
            integer :: n, i, j, j0, znum
            integer :: lambda0, lambda1
            real(F64) :: c, s, jac, x, w, r
            integer :: gto_lmax
            integer :: nf
            integer :: lx, ly, lz
            integer :: gto_max_nfunc
            real(F64), dimension(:), allocatable :: xyzwork
            logical :: spin_orbit
            character(:), allocatable :: param_file
            character(:), allocatable :: citation
            type(tstringlist) :: citations
            integer, dimension(:), allocatable :: ZList, AtomElementMap, ZCount
            integer :: NElements

            allocate(AtomElementMap(System%NAtoms))
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_REAL_ATOMS)
            if (calcgrad) then
                  !
                  ! The subroutine for calculating gradient of the pseudopotential
                  ! requires higher angular momentum of the GTO orbitals
                  !
                  gto_lmax = AOBasis%LmaxGTO + 1
                  ECP_CALCGRAD = .true.
            else
                  gto_lmax = AOBasis%LmaxGTO
                  ECP_CALCGRAD = .false.
            end if

            gto_max_nfunc = ((gto_lmax + 1) * (gto_lmax + 2)) / 2

            allocate(ECP_LL(gto_max_nfunc, 0:gto_lmax))
            allocate(ECP_MM(gto_max_nfunc, 0:gto_lmax))
            allocate(ECP_NN(gto_max_nfunc, 0:gto_lmax))

            do L = 0, gto_lmax
                  i = 1
                  do lx = L, 0, -1
                        do ly = L - lx, 0, -1
                              ECP_LL(i, L) = lx
                              ECP_MM(i, L) = ly
                              ECP_NN(i, L) = L - lx - ly
                              i = i + 1
                        end do
                  end do
            end do            
            ECP_SPIN_ORBIT = .false.
            ECP_IELEMENT = 0
            pp_lmax = -1
            ECP_NELEMENTS = 0
            ngausstot = 0
            do i = 1, NElements
                  znum = ZList(i)
                  param_file = ecp_path%get(znum)
                  call pp_queryecp(param_file, znum, lmax, ngauss, ncoreel, spin_orbit, citation)
                  if (citation .ne. "") call citations%update(citation, znum)
                  if (ngauss > 0) then
                        ECP_NELEMENTS = ECP_NELEMENTS + 1
                        pp_lmax = max(pp_lmax, lmax)
                        ECP_IELEMENT(znum) = ECP_NELEMENTS
                        ngausstot = ngausstot + ngauss
                        if (ECP_SPIN_ORBIT .and. .not. spin_orbit) then
                              call msg("Spin-orbit ECP parameters not provided for Z=" // str(znum), MSG_ERROR)
                              error stop
                        else
                              ECP_SPIN_ORBIT = spin_orbit
                        end if
                  end if
            end do

            allocate(ECP_INUCLZ(System%NAtoms))
            ECP_INUCLZ(1:System%NAtoms) = System%ZNumbers(1:System%NAtoms)

            if (ngausstot > 0) then
                  ECP_ENABLED = .true.
            else
                  ECP_ENABLED = .false.
                  ECP_NATOM = 0
                  return
            end if

            allocate(ECP_K0(ECP_NELEMENTS))
            allocate(ECP_LMAX(ECP_NELEMENTS))
            allocate(ECP_NGAUSS(pp_lmax+2, ECP_NELEMENTS))
            allocate(ECP_NKL(ngausstot))
            allocate(ECP_NCORE(ECP_NELEMENTS))
            allocate(ECP_COEFF(ngausstot))
            allocate(ECP_SO_COEFF(ngausstot))
            allocate(ECP_EXPN(ngausstot))
            allocate(ECP_ZNUM(ECP_NELEMENTS))
            allocate(ECP_LOCALPP(ECP_NELEMENTS))

            k0 = 1
            do i = 1, NElements
                  znum = ZList(i)
                  if (pp_isecp(znum)) then
                        idx = ECP_IELEMENT(znum)
                        ECP_ZNUM(idx) = znum
                        ECP_K0(idx) = k0
                        param_file = ecp_path%get(znum)
                        call pp_getecp(param_file, znum, ECP_LMAX(idx), ECP_NGAUSS(:, idx), &
                              ECP_NCORE(idx), ECP_COEFF(k0:), ECP_SO_COEFF(k0:), &
                              ECP_EXPN(k0:), ECP_NKL(k0:))
                        k0 = k0 + sum(ECP_NGAUSS(1:ECP_LMAX(idx)+2,idx))
                  end if
            end do
            !
            ! Check if the local part of the scalar pseudopotential is present
            !
            do i = 1, ECP_NELEMENTS
                  idx = i
                  ECP_LOCALPP(idx) = .true.
                  if (ECP_NGAUSS(1, idx) == 1) then
                        k0 = ECP_K0(idx)
                        if (abs(ECP_COEFF(k0)) < 1.0E-12_F64) then
                              ECP_LOCALPP(idx) = .false.
                        end if
                  end if
            end do
            if (PrintOutParams) then
                  !
                  ! Display ECP parameters
                  !
                  call pp_displayheader(ECP_SPIN_ORBIT)
                  do i = 1, ECP_NELEMENTS
                        citation = citations%get(ECP_ZNUM(i))
                        call pp_displayparams(ECP_ZNUM(i), ECP_SPIN_ORBIT, ecp_path%get(ECP_ZNUM(i)), citation)
                  end do
            end if
            !
            ! Count the atoms on which pseudopotentials reside.
            ! Do not count in the ghost atoms which otherwise would
            ! have a pseudopotential centered on them.
            !
            ECP_NATOM = 0
            do j = 1, 2
                  do i = System%RealAtoms(1, j), System%RealAtoms(2, j)
                        znum = System%ZNumbers(i)
                        if (pp_isecp(znum)) then
                              ECP_NATOM = ECP_NATOM + 1
                        end if
                  end do
            end do
            allocate(ECP_ATOM(ECP_NATOM))
            n = 1
            do j = 1, 2
                  do i = System%RealAtoms(1, j), System%RealAtoms(2, j)
                        znum = System%ZNumbers(i)
                        if (pp_isecp(znum)) then
                              ECP_ATOM(n) = i
                              ECP_INUCLZ(i) = znum - ECP_NCORE(ECP_IELEMENT(znum))
                              n = n + 1
                        end if
                  end do
            end do
            ECP_TCC = pp_lmax + 2
            ECP_TAB = [pp_lmax+gto_lmax, pp_lmax+gto_lmax, pp_lmax, 2*gto_lmax]
            ECP_TAC = [pp_lmax+gto_lmax, pp_lmax, 2*gto_lmax]
            ECP_TCHIAC = [2*gto_lmax, gto_lmax]
            ECP_TCHIAB = [2*gto_lmax, 2*gto_lmax]
            !
            ! Matrix of the angular momentum operator <mm| 1/i L |m> in the basis
            ! of real spherical harmonics. Used to compute the spin-orbit part
            ! of the PP.
            !
            allocate(ECP_LVECTOR(3, -pp_lmax:pp_lmax, -pp_lmax:pp_lmax, 0:pp_lmax))
            ECP_LVECTOR = ZERO
            do l = 0, pp_lmax
                  do m = -l, l
                        do mm = -l, l
                              call AngularMomentum(ECP_LVECTOR(:, mm, m, l), l, mm, m)
                        end do
                  end do
            end do
            
            ECP_ULMAX = max(pp_lmax + gto_lmax, 2*gto_lmax)
            allocate(ECP_U(numulm(ECP_ULMAX)))
            !
            ! Tabulate U^{LM}_{LX,LY,LZ} coefficients:
            ! Eq. 33 in [1]
            ! ---
            ! Maximum L in U^{LM}_{LX,LY,LZ}
            !
            lambdamax = max(pp_lmax + gto_lmax, 2*gto_lmax)
            ECP_SLMA = numslm(lambdamax)
            ECP_SLMB = numslm(lambdamax)
            ECP_SLMK = numslm(2*gto_lmax)
            ECP_XYZWORK = numxyz(lambdamax)
            do l = 0, lambdamax
                  do m = -l, l
                        pos = ulmpos(l, m)
                        call rshu(ECP_U(pos:), l, m)
                  end do
            end do
            !
            ! Generate quadrature points and weights
            ! ---
            ! Smallest quadrature
            !
            allocate(CHEB_W1(CHEB_N1))
            allocate(CHEB_X1(CHEB_N1))
            n = CHEB_N1
            do i = 1, n
                  s = sin(real(i, F64)/real(n+1, F64)*PI)
                  c = cos(real(i, F64)/real(n+1, F64)*PI)
                  x = ONE + TWO/PI*(ONE + TWO/THREE * s**2) * c * s - TWO * real(i, F64)/(real(n+1, F64))
                  w = s**4
                  call pp_cheb_r(r, jac, x)
                  CHEB_X1(i) = r
                  CHEB_W1(i) = w * jac
            end do
            !
            ! Second smallest quadrature
            !
            allocate(CHEB_W2(n+1))
            allocate(CHEB_X2(n+1))
            n = 2 * n + 1
            call pp_cheb_precalc(CHEB_X2, CHEB_W2, n)
            !
            ! Third smallest quadrature
            !
            allocate(CHEB_W3(n+1))
            allocate(CHEB_X3(n+1))
            n = 2 * n + 1
            call pp_cheb_precalc(CHEB_X3, CHEB_W3, n)
            !
            ! Precalculate OMEGA angular integrals
            !
            allocate(ECP_OMEGA(numslm(gto_lmax+pp_lmax), numslm(pp_lmax), pp_numaxayaz(gto_lmax)))
            allocate(xyzwork(ECP_XYZWORK))
            do alpha = 0, gto_lmax
                  do alpx = 0, alpha
                        do alpy = 0, alpha - alpx
                              alpz = alpha - alpx - alpy
                              do l = 0, pp_lmax
                                    !
                                    ! LAMBDA is constrained by Eq. 26 in [2]
                                    ! and the constraint that L+ALPHA-LAMBDA be even
                                    !
                                    lambda0 = max(l-alpha, 0)
                                    lambda1 = l + alpha
                                    incl1 = modulo(lambda1-lambda0, 2)
                                    do m = -l, l
                                          do lambda = lambda0+incl1, lambda1, 2
                                                do mu = -lambda, lambda
                                                      i1 = slmpos(lambda, mu)
                                                      i2 = slmpos(l, m)
                                                      i3 = pp_axayazpos(alpx, alpy, alpz)
                                                      ECP_OMEGA(i1, i2, i3) = pp_omega(alpx, alpy, alpz, &
                                                            l, m, lambda, mu, xyzwork)
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            deallocate(xyzwork)
            !
            ! Precalculate binomial coefficients
            !
            allocate(ECP_BINOM(0:gto_lmax, 0:gto_lmax))
            do l = 0, gto_lmax
                  do m = 0, l
                        ECP_BINOM(l, m) = binom(l, m)
                  end do
            end do
            if (ECP_CALCGRAD) then
                  !
                  ! Precalculate indices of angular functions for each angular momentum
                  !
                  n = pp_numaxayaz(gto_lmax)
                  allocate(ECP_ANGFIDX(n))
                  do l = 0, gto_lmax
                        nf = nfunc(l)
                        j0 = pp_numaxayaz(l-1)
                        do i = 1, nf
                              lx = ECP_LL(i, l)
                              ly = ECP_MM(i, l)
                              lz = ECP_NN(i, l)
                              j = j0 + lxlylzpos(lx, ly, lz)
                              ECP_ANGFIDX(j) = i
                        end do
                  end do
            end if

            call pp_spherbessel_tabulate(gto_lmax, pp_lmax)
            call citations%free()
      end subroutine pp_Init


      subroutine pp_spherbessel_tabulate(gto_lmax, pp_lmax)
            ! ------------------------------------------------------------------------
            ! Set up the Chebyshev interpolation and asymptotic expansion of modified
            ! spherical Bessel functions.
            ! -----------------------------------------------------------------------
            ! 1. Moreno-Flores, R., Alvarez-Mendez, R., Vela, A., and
            !    Koster, A.M., Half-Numerical Evaluation of Pseudopotential
            !    Integrals, J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            ! 2. McMurchie, L. E., and Davidson, E. R., Calculation of Integrals
            !    over ab initio Pseudopotentials, J. Comp. Phys. 44, 289 (1981);
            !    doi: 10.1016/0021-9991(81)90053-X
            !
            integer, intent(in) :: gto_lmax
            integer, intent(in) :: pp_lmax

            integer :: i, j, k
            real(F64) :: a, b
            real(F64), dimension(INTERP_N) :: interp_x, interp_f
            integer :: max_n
            integer(I64), dimension(:), allocatable :: t0, t1, t2
            !
            ! Accuracy of the reference spherical Bessel function
            !
            real(F64), parameter :: eps = 1.0E-15_F64
            !
            ! The maximum order of a modified spherical Bessel function
            ! --------------------------------------------------------------
            ! GTO_LMAX + PP_LMAX is the maximum N for the \gamma_{AB} integrals
            ! of the momentum-dependent part of the pseudopotential. See Eq. 11
            ! in [1].
            !
            ! 2 * GTO_LMAX is the maximum N for the \chi_{AB} integrals of the
            ! momentum-independent part of the pseudopotential. See Eq. 19
            ! in [2] and the comment below this equation.
            !
            max_n = max(gto_lmax + pp_lmax, 2 * gto_lmax)
            allocate(BESSEL_TABLE(INTERP_N, NINTERVALS, 0:max_n))
            !
            ! Generate coefficients of the expansion of i_n(x) in terms
            ! of the Chebyshev polynomials
            !
            do k = 0, max_n
                  do i = 1, NINTERVALS
                        a = INTERP_XMIN + real(i - 1, F64) * DELTAX
                        b = INTERP_XMIN + real(i, F64) * DELTAX
                        call cheb_nodes(interp_x, INTERP_N, a, b)
                        do j = 1, INTERP_N
                              interp_f(j) = pp_spherbessel_n(k, interp_x(j), eps)
                        end do
                        call cheb_coeffs(BESSEL_TABLE(:, i, k), interp_f, INTERP_N)
                  end do
            end do
            !
            ! Asymptotic forms of modified spherical Bessel functions. These
            ! expansions will be used for X > INTERP_XMAX.
            ! ---
            ! Use recurrence relation to generate higher-order Bessel functions:
            !
            ! i_{n+1)(x) = i_{n-1}(x) - (2n + 1) / x i_n(x)
            ! i_0 = 1/(2 x)
            ! 1_1 = 1/(2 x) - 1/(2 x^2)
            !
            allocate(BESSEL_ASYMP(max(2,max_n+1), 0:max_n))
            allocate(t0(max_n+1))
            allocate(t1(max_n+1))
            allocate(t2(max_n+1))
            !
            ! Coefficients of the asymptotic expansion are stored in BESSEL_ASYMP:
            ! i_n(x) = Sum_{k=1}^{n+1} BESSEL_ASYMP(k, n) 1 / x^k
            !
            BESSEL_ASYMP = ZERO
            t0 = 0
            t1 = 0
            t0(1) = 1
            t1(1) = 1
            t1(2) = -1

            BESSEL_ASYMP(1, 0) = ONE/TWO
            BESSEL_ASYMP(1, 1) = ONE/TWO
            BESSEL_ASYMP(2, 1) = -ONE/TWO

            do k = 2, max_n
                  t2 = t0
                  do i = 2, k+1
                        t2(i) = t2(i) - (2 * k - 1) * t1(i - 1)
                  end do

                  do i = 1, k+1
                        BESSEL_ASYMP(i, k) = ONE/TWO * real(t2(i), F64)
                  end do

                  t0 = t1
                  t1 = t2
            end do
      end subroutine pp_spherbessel_tabulate


      subroutine pp_Free()
            if (allocated(ECP_U)) deallocate(ECP_U)
            if (allocated(CHEB_W1)) deallocate(CHEB_W1)
            if (allocated(CHEB_X1)) deallocate(CHEB_X1)
            if (allocated(CHEB_W2)) deallocate(CHEB_W2)
            if (allocated(CHEB_X2)) deallocate(CHEB_X2)
            if (allocated(CHEB_W3)) deallocate(CHEB_W3)
            if (allocated(CHEB_X3)) deallocate(CHEB_X3)
            if (allocated(ECP_K0)) deallocate(ECP_K0)
            if (allocated(ECP_LMAX)) deallocate(ECP_LMAX)
            if (allocated(ECP_NGAUSS)) deallocate(ECP_NGAUSS)
            if (allocated(ECP_NKL)) deallocate(ECP_NKL)
            if (allocated(ECP_NCORE)) deallocate(ECP_NCORE)
            if (allocated(ECP_COEFF)) deallocate(ECP_COEFF)
            if (allocated(ECP_SO_COEFF)) deallocate(ECP_SO_COEFF)
            if (allocated(ECP_EXPN)) deallocate(ECP_EXPN)
            if (allocated(ECP_ATOM)) deallocate(ECP_ATOM)
            if (allocated(ECP_OMEGA)) deallocate(ECP_OMEGA)
            if (allocated(ECP_LOCALPP)) deallocate(ECP_LOCALPP)
            if (allocated(ECP_BINOM)) deallocate(ECP_BINOM)
            if (allocated(ECP_ZNUM)) deallocate(ECP_ZNUM)
            if (allocated(ECP_ANGFIDX)) deallocate(ECP_ANGFIDX)
            if (allocated(ECP_INUCLZ)) deallocate(ECP_INUCLZ)
            if (allocated(ECP_LL)) deallocate(ECP_LL)
            if (allocated(ECP_MM)) deallocate(ECP_MM)
            if (allocated(ECP_NN)) deallocate(ECP_NN)
            if (allocated(BESSEL_TABLE)) deallocate(BESSEL_TABLE)
            if (allocated(BESSEL_ASYMP)) deallocate(BESSEL_ASYMP)
            if (allocated(ECP_LVECTOR)) deallocate(ECP_LVECTOR)
            ECP_ENABLED = .false.
            ECP_SPIN_ORBIT = .false.
            ECP_NATOM = 0
      end subroutine pp_Free


      subroutine pp_queryecp(basis_path, element, lmax, ngauss, ncoreel, spin_orbit, citation)
            character(*), intent(in) :: basis_path
            integer, intent(in)      :: element
            integer, intent(out)     :: lmax
            integer, intent(out)     :: ngauss
            integer, intent(out)     :: ncoreel
            logical, intent(out)     :: spin_orbit
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


      subroutine pp_displayheader(spin_orbit)
            logical, intent(in) :: spin_orbit
            
            call toprule()
            call msg("Loaded pseudopotential parameters")
            if (spin_orbit) then
                  call msg("Spin-orbit pseudopotential is available")
            end if
            call midrule()
            call blankline()
      end subroutine pp_displayheader
      

      subroutine pp_displayparams(z, spin_orbit, ecp_path, citation)
            integer, intent(in) :: z
            logical, intent(in) :: spin_orbit
            character(*), intent(in) :: ecp_path
            character(*), intent(in) :: citation
            
            character(len=DEFLEN) :: line
            integer :: idx, k0, k
            integer :: lmax, l
            integer :: ngauss
            integer :: igauss

            idx = ECP_IELEMENT(z)
            k0 = ECP_K0(idx)
            lmax = ECP_LMAX(idx)
            call msg(ELNAME_LONG(z))
            call msg("Parameters file: " // ecp_path)
            if (citation .ne. "") then
                  call msg("Reference: " // citation)
            end if
            call msg("ECP accounts for " // str(ECP_NCORE(idx)) // " core electrons")
            call blankline()
            if (spin_orbit) then
                  write(line, "(4X,A15,4X,A15,4X,A3,4X,A15)") cfield("Coeff", 15), cfield("Coeff(SO)", 15), &
                        "R^n", cfield("Exponent", 15)
            else
                  write(line, "(4X,A15,4X,A3,4X,A15)") cfield("Coeff", 15), "R^n", cfield("Exponent", 15)
            end if
            call msg(line)
            call blankline()
            call msg("Local part")
            call blankline()
            igauss = k0
            ngauss = ECP_NGAUSS(1, idx)
            if (.not. ECP_LOCALPP(idx)) then
                  call msg("(Local part not present)")
            end if
            do k = 1, ngauss
                  if (spin_orbit) then
                        write(line, "(4X,F15.8,4X,15X,4X,I3,4X,F15.8)") &
                              ECP_COEFF(igauss), ECP_NKL(igauss), ECP_EXPN(igauss)
                  else
                        write(line, "(4X,F15.8,4X,I3,4X,F15.8)") &
                              ECP_COEFF(igauss), ECP_NKL(igauss), ECP_EXPN(igauss)
                  end if
                  call msg(line)
                  igauss = igauss + 1
            end do

            do l = 0, lmax
                  call blankline()
                  call msg("L = " // str(l))
                  call blankline()
                  ngauss = ECP_NGAUSS(2+l, idx)
                  do k = 1, ngauss
                        if (spin_orbit) then
                              if (l > 0) then
                                    write(line, "(4X,F15.8,4X,F15.8,4X,I3,4X,F15.8)") &
                                          ECP_COEFF(igauss), ECP_SO_COEFF(igauss), &
                                          ECP_NKL(igauss), ECP_EXPN(igauss)
                              else
                                    write(line, "(4X,F15.8,4X,15X,4X,I3,4X,F15.8)") &
                                          ECP_COEFF(igauss), ECP_NKL(igauss), ECP_EXPN(igauss)
                              end if
                        else
                              write(line, "(4X,F15.8,4X,I3,4X,F15.8)") &
                                    ECP_COEFF(igauss), ECP_NKL(igauss), ECP_EXPN(igauss)
                        end if
                        call msg(line)
                        igauss = igauss + 1
                  end do
            end do
      end subroutine pp_displayparams


      pure subroutine pp_loadgto(Phi, Shell, AOBasis)
            type(tgtodef), intent(out) :: Phi
            integer, intent(in)        :: Shell
            type(TAOBasis), intent(in) :: AOBasis

            integer :: ParamsIdx, NP, N

            associate ( &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  NAngFunc => AOBasis%NAngFuncCart &
                  )
                  ParamsIdx = ShellParamsIdx(Shell)
                  Phi%L = ShellMomentum(ParamsIdx)
                  NP = NPrimitives(ParamsIdx)
                  Phi%nprm = NP
                  Phi%cntr(1:NP) = CntrCoeffs(1:NP, ParamsIdx)
                  Phi%expn(1:NP) = Exponents(1:NP, ParamsIdx)
                  N = NAngFunc(ParamsIdx)
                  Phi%norm(1:N) = NormFactors(1:N, ParamsIdx)
            end associate
      end subroutine pp_loadgto


      pure subroutine pp_loadgto_grad(Phi_lo, Phi_hi, Shell, AOBasis)
            type(tgtodef), intent(out) :: Phi_lo
            type(tgtodef), intent(out) :: Phi_hi
            integer, intent(in)        :: Shell
            type(TAOBasis), intent(in) :: AOBasis

            integer :: i

            call pp_loadgto(Phi_lo, Shell, AOBasis)
            Phi_hi = Phi_lo
            Phi_hi%L = Phi_hi%L + 1
            Phi_lo%L = Phi_lo%L - 1
            do i = 1, Phi_hi%nprm
                  Phi_hi%cntr(i) = -TWO * Phi_hi%expn(i) * Phi_hi%cntr(i)
            end do
      end subroutine pp_loadgto_grad


      pure function pp_isecp(z)
            !
            ! Is ECP potential enabled for the element of the atomic number Z?
            !
            logical             :: pp_isecp
            integer, intent(in) :: z
            
            if (ECP_IELEMENT(z) > 0) then
                  pp_isecp = .true.
            else
                  pp_isecp = .false.
            end if
      end function pp_isecp


      pure function pp_numaxayaz(alpha)
            integer             :: pp_numaxayaz
            integer, intent(in) :: alpha

            pp_numaxayaz = ((alpha + 1) * (alpha + 2) * (alpha + 3)) / 6
      end function pp_numaxayaz
      

      pure function pp_axayazpos(ax, ay, az)
            integer             :: pp_axayazpos
            integer, intent(in) :: ax
            integer, intent(in) :: ay
            integer, intent(in) :: az

            integer :: alpha

            alpha = ax + ay + az
            pp_axayazpos = pp_numaxayaz(alpha-1) + lxlylzpos(ax, ay, az)
      end function pp_axayazpos

      
      pure function pp_readomega(lambda, mu, l, m, ax, ay, az)
            real(F64) :: pp_readomega
            integer, intent(in) :: lambda
            integer, intent(in) :: mu
            integer, intent(in) :: l
            integer, intent(in) :: m
            integer, intent(in) :: ax
            integer, intent(in) :: ay
            integer, intent(in) :: az

            integer :: i1, i2, i3

            i1 = slmpos(lambda, mu)
            i2 = slmpos(l, m)
            i3 = pp_axayazpos(ax, ay, az)
            pp_readomega = ECP_OMEGA(i1, i2, i3)
      end function pp_readomega


      function pp_omega(i, j, k, l, m, lambda, mu, xyzwork)
            ! --------------------------------------------------------------
            ! Calculate Omega^{IJK}_{LM,lambda,mu} integral, Eq. 18 in [1]
            ! --------------------------------------------------------------
            ! 1. Moreno-Flores, R., Alvarez-Mendez, R., Vela, A., and
            !    Koster, A.M., Half-Numerical Evaluation of Pseudopotential
            !    Integrals, J. Comput. Chem. 27, 1009 (2006)
            ! --------------------------------------------------------------
            real(F64) :: pp_omega
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: k
            integer, intent(in) :: l
            integer, intent(in) :: m
            integer, intent(in) :: lambda
            integer, intent(in) :: mu
            real(F64), dimension(:), intent(out) :: xyzwork

            integer :: lx, ly, lz
            integer :: w, w0
            
            w0 = ulmpos(l, m)
            pp_omega = ZERO
            w = w0
            do lx = 0, l
                  do ly = 0, l - lx
                        lz = l - lx - ly
                        pp_omega = pp_omega + ECP_U(w) * &
                              rshv(lambda, mu, i+lx, j+ly, k+lz, ECP_U, xyzwork)
                        w = w + 1
                  end do
            end do
      end function pp_omega
      

      subroutine pp_allocate_scratch(tcc, tab, tac, tchiac, tchiab, slma, slmb, slmk, xyzwork)
            real(F64), dimension(:), allocatable, intent(out)          :: tcc
            real(F64), dimension(:, :, :, :), allocatable, intent(out) :: tab
            real(F64), dimension(:, :, :), allocatable, intent(out)    :: tac
            real(F64), dimension(:, :), allocatable, intent(out)       :: tchiac
            real(F64), dimension(:, :), allocatable, intent(out)       :: tchiab
            real(F64), dimension(:), allocatable, intent(out)          :: slma
            real(F64), dimension(:), allocatable, intent(out)          :: slmb
            real(F64), dimension(:), allocatable, intent(out)          :: slmk
            real(F64), dimension(:), allocatable, intent(out)          :: xyzwork
            
            allocate(tcc(ECP_TCC))
            allocate(tab(0:ECP_TAB(1), 0:ECP_TAB(2), 0:ECP_TAB(3), 0:ECP_TAB(4)))
            allocate(tac(0:ECP_TAC(1), 0:ECP_TAC(2), 0:ECP_TAC(3)))
            allocate(tchiac(0:ECP_TCHIAC(1), 0:ECP_TCHIAC(2)))
            allocate(tchiab(0:ECP_TCHIAB(1), 0:ECP_TCHIAB(2)))
            allocate(slma(ECP_SLMA))
            allocate(slmb(ECP_SLMB))
            allocate(slmk(ECP_SLMK))
            allocate(xyzwork(ECP_XYZWORK))
      end subroutine pp_allocate_scratch
      
      
      pure subroutine pp_decode_pq(pq, n, p, q)
            !
            ! Decode a lower-triangle compound index into individual
            ! indices:
            ! PQ -> (P, Q)
            ! Assumptions:
            ! 0) P = 1, 2, ..., N,
            !    Q = 1, 2, ..., N,
            ! 1) P >= Q (diagonal indices admissible)
            !
            ! An example of how this algorithm traverses an N=3 triangle:
            !
            !      Q
            !    1
            ! P  2 5
            !    3 6 4
            !
            integer, intent(in)  :: pq
            integer, intent(in)  :: n
            integer, intent(out) :: p
            integer, intent(out) :: q

            integer :: q_base
            integer :: v
            integer :: interval1
            integer :: in1, in2
            !
            ! pq = (q_base - 1) * (n + 1) + v
            !
            q_base = (pq - 1) / (n + 1) + 1
            v = pq - (n + 1) * (q_base - 1)
            !
            ! Decide if v is in interval_1 or interval_2:
            ! in1 == 1 and in2 == 0 if v <= INTERVAL1
            ! in1 == 0 and in2 == 1 if v > INTERVAL1
            !
            interval1 = n - q_base + 1
            in2 = v / (interval1 + 1)
            !
            ! 1 -> 0, 0 -> 1
            !
            in1 = ieor((in2), 1)

            p = in1 * (q_base + v - 1) + in2 * (v - interval1 + n - q_base)          
            q = in1 * q_base + in2 * interval1
      end subroutine pp_decode_pq
      

      subroutine pp_Vgrad(Vx, Vy, Vz, c, AOBasis, System, ECPFile)
            real(F64), dimension(:, :), intent(inout) :: Vx
            real(F64), dimension(:, :), intent(inout) :: Vy
            real(F64), dimension(:, :), intent(inout) :: Vz
            integer, intent(in)                       :: c
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System
            type(TStringList), intent(in)             :: ECPFile

            integer, dimension(:), allocatable :: ZList, AtomElementMap, ZCount
            integer :: NElements

            allocate(AtomElementMap(System%NAtoms))
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_REAL_ATOMS)
            call pp_Init(AOBasis, System, ECPFile, .true., .false.)
            call pp_Vgrad_2(Vx, Vy, Vz, c, AOBasis, System)
            call pp_Free()
      end subroutine pp_Vgrad

      
      subroutine pp_Vgrad_2(vxmat, vymat, vzmat, c, AOBasis, System)
            ! -------------------------------------------------------------------------
            ! Calculate the gradient of the pseudopotential. X, Y, and Z components of
            ! the gradient are stored in three separate symmetric matrices. The
            ! matrix elements of the gradient are defined as
            ! VQMAT(A,B) = <\phi_a | d/dR_{C,q} V_{PP} | \phi_b>.
            ! \phi_a and \phi_b are Cartesian Gaussian orbitals (centered
            ! on any nucleus or a dummy center), R_{C,q}, q \in \{X, Y, Z\} is
            ! the q-th coordinate of the nucleus C, and V_{PP} is the pseudopotential
            ! due to all ECP nuclei present in the molecule.
            !
            ! Note that only nucleus C contributes to d/dR_{C,q} V_{PP}.
            ! The specified nucleus must belong to the set of non-dummy centers.
            ! Change sign of the integrals to get derivatives of V_{PP} with respect
            ! to electronic coordinates. Sum the integrals over all non-dummy centers
            ! to get the total pseudopotential gradient.
            ! -------------------------------------------------------------------------
            ! VXMAT, - Input/output. On exit, these matrices are updated by adding the
            ! VYMAT    following matrix elements:
            ! VZMAT    <\phi_a | d/dR_{C,q} V_{PP} | \phi_b>
            !          where q = X, Y, Z, respectively. Only lower triangle of each
            !          symmetric matrix contains meaningful numbers.
            ! C      - Index of the nucleus with respect to which the differentiation
            !          is performed.
            !
            real(F64), dimension(:, :), intent(inout) :: vxmat
            real(F64), dimension(:, :), intent(inout) :: vymat
            real(F64), dimension(:, :), intent(inout) :: vzmat
            integer, intent(in)                       :: c
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System

            integer :: a, b
            integer :: shella, shellb
            integer :: shellab, shellab_max
            type(tgtodef) :: phia_hi, phia_lo
            type(tgtodef) :: phib_hi, phib_lo
            type(tgtodef) :: phia, phib
            real(F64), dimension(ECP_GTO_MAX_NFUNC**2) :: gab_a_lo, gab_a_hi
            real(F64), dimension(ECP_GTO_MAX_NFUNC**2) :: gab_b_lo, gab_b_hi

            real(F64), dimension(:), allocatable :: tcc
            real(F64), dimension(:, :, :, :), allocatable :: tab
            real(F64), dimension(:, :, :), allocatable :: tac
            real(F64), dimension(:, :), allocatable :: tchiac
            real(F64), dimension(:, :), allocatable :: tchiab
            real(F64), dimension(:), allocatable :: slma
            real(F64), dimension(:), allocatable :: slmb
            real(F64), dimension(:), allocatable :: slmk
            real(F64), dimension(:), allocatable :: xyzwork
            !
            ! Test if ECPINT module has been initialized to compute the gradient of
            ! the pseudopotential
            !
            if (.not. ECP_CALCGRAD) then
                  call msg("Pseudopotential module not initialized to compute derivatives", MSG_ERROR)
                  error stop
            end if

            if (c < 1 .or. c > System%NAtoms) then
                  call msg("Atom index outside of the allowed range", MSG_ERROR)
                  error stop
            end if
            !
            ! Return if no pseudopotential is centered at the atom C.
            ! Note that pseudopotentials are not present on ghost atoms.
            !
            if (.not. pp_isecp(System%ZNumbers(c))) then
                  if (sys_IsDummyAtom(System, c))then
                        return
                  end if                  
            end if
            associate ( &
                  ShellCenters => AOBasis%ShellCenters, &
                  NShells => AOBasis%NShells &
                  )
                  
                  shellab_max = ((NShells + 1) * NShells) / 2
                  !$omp parallel &
                  !$omp default(shared) &
                  !$omp private(tcc, tab, tac, tchiac, tchiab, slma, slmb, slmk, xyzwork) &
                  !$omp private(gab_a_lo, gab_a_hi, gab_b_lo, gab_b_hi, a, b, shella, shellb) &
                  !$omp private(phia, phib, phia_hi, phia_lo, phib_hi, phib_lo) &
                  !$omp shared(vxmat, vymat, vzmat)

                  call pp_allocate_scratch(tcc, tab, tac, tchiac, tchiab, slma, slmb, slmk, xyzwork)

                  !$omp do schedule(guided)            
                  do shellab = 1, shellab_max
                        !
                        ! Extract individual indices from the compound shell-pair index
                        !
                        call pp_decode_pq(shellab, NShells, shella, shellb)
                        a = ShellCenters(shella)
                        b = ShellCenters(shellb)
                        call pp_loadgto_grad(phia_lo, phia_hi, shella, AOBasis)
                        call pp_loadgto(phia, shella, AOBasis)
                        call pp_loadgto_grad(phib_lo, phib_hi, shellb, AOBasis)
                        call pp_loadgto(phib, shellb, AOBasis)
                        gab_a_lo = ZERO
                        gab_a_hi = ZERO
                        gab_b_lo = ZERO
                        gab_b_hi = ZERO
                        if ((a .ne. c) .and. (b .ne. c)) then
                              if (phia%l > 0) call pp_ecpab(gab_a_lo, a, b, c, phia_lo, phib, &
                                    slma, slmb, slmk, tab, tchiab, xyzwork, System)
                              call pp_ecpab(gab_a_hi, a, b, c, phia_hi, phib, &
                                    slma, slmb, slmk, tab, tchiab, xyzwork, System)
                              if (phib%l > 0) call pp_ecpab(gab_b_lo, a, b, c, phia, phib_lo, &
                                    slma, slmb, slmk, tab, tchiab, xyzwork, System)
                              call pp_ecpab(gab_b_hi, a, b, c, phia, phib_hi, &
                                    slma, slmb, slmk, tab, tchiab, xyzwork, System)
                        else if ((a .ne. c) .and. (b .eq. c)) then
                              if (phia%l > 0) call pp_ecpac(gab_a_lo, a, c, phia_lo, phib, &
                                    slma, tac, tchiac, xyzwork, System)
                              call pp_ecpac(gab_a_hi, a, c, phia_hi, phib, &
                                    slma, tac, tchiac, xyzwork, System)
                              if (phib%l > 0) call pp_ecpac(gab_b_lo, a, c, phia, phib_lo, &
                                    slma, tac, tchiac, xyzwork, System)
                              call pp_ecpac(gab_b_hi, a, c, phia, phib_hi, &
                                    slma, tac, tchiac, xyzwork, System)
                        else if ((a .eq. c) .and. (b .ne. c)) then
                              if (phia%l > 0) call pp_ecpbc(gab_a_lo, b, c, phia_lo, phib, &
                                    slma, tac, tchiac, xyzwork, System)
                              call pp_ecpbc(gab_a_hi, b, c, phia_hi, phib, &
                                    slma, tac, tchiac, xyzwork, System)
                              if (phib%l > 0) call pp_ecpbc(gab_b_lo, b, c, phia, phib_lo, &
                                    slma, tac, tchiac, xyzwork, System)
                              call pp_ecpbc(gab_b_hi, b, c, phia, phib_hi, &
                                    slma, tac, tchiac, xyzwork, System)
                        else
                              !
                              ! a == c .and. b == c
                              !
                              if (phia%l > 0) call pp_ecpcc(gab_a_lo, c, phia_lo, phib, tcc, xyzwork, System)
                              call pp_ecpcc(gab_a_hi, c, phia_hi, phib, tcc, xyzwork, System)
                              if (phib%l > 0) call pp_ecpcc(gab_b_lo, c, phia, phib_lo, tcc, xyzwork, System)
                              call pp_ecpcc(gab_b_hi, c, phia, phib_hi, tcc, xyzwork, System)
                        end if
                        !
                        ! Two threads cannot update the matrices at the same time because
                        ! each parallel task corresponds to a distinct bra-ket shell pair
                        !
                        call pp_vxyzmat_update(vxmat, vymat, vzmat, gab_a_lo, gab_a_hi, &
                              gab_b_lo, gab_b_hi, shella, shellb, phia, phib, AOBasis)
                  end do
                  !$omp end do nowait
                  deallocate(tcc)
                  deallocate(tab)
                  deallocate(tac)
                  deallocate(tchiac)
                  deallocate(tchiab)
                  deallocate(slma)
                  deallocate(slmb)
                  deallocate(slmk)
                  deallocate(xyzwork)
                  !$omp end parallel
            end associate
      end subroutine pp_Vgrad_2


      subroutine pp_V(V, AOBasis, System, ECPFile)
            real(F64), dimension(:, :), intent(inout) :: V
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System
            type(TStringList), intent(in)             :: ECPFile

            integer, dimension(:), allocatable :: ZList, AtomElementMap, ZCount
            integer :: NElements

            allocate(AtomElementMap(System%NAtoms))
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_REAL_ATOMS)
            call pp_Init(AOBasis, System, ECPFile, .false., (System%SubsystemKind==SYS_TOTAL))
            call pp_V_2(V, AOBasis, System)
            call pp_Free()
      end subroutine pp_V
      
      
      subroutine pp_V_2(vmat, AOBasis, System)
            real(F64), dimension(:, :), intent(inout) :: vmat
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System

            integer :: a, b, c, cc
            integer :: shella, shellb
            integer :: shellab, shellab_max
            type(tgtodef) :: phia, phib
            real(F64), dimension(MAX_NFUNC**2) :: gab
            real(F64), dimension(:), allocatable :: tcc
            real(F64), dimension(:, :, :, :), allocatable :: tab
            real(F64), dimension(:, :, :), allocatable :: tac
            real(F64), dimension(:, :), allocatable :: tchiac
            real(F64), dimension(:, :), allocatable :: tchiab
            real(F64), dimension(:), allocatable :: slma
            real(F64), dimension(:), allocatable :: slmb
            real(F64), dimension(:), allocatable :: slmk
            real(F64), dimension(:), allocatable :: xyzwork

            if (.not. ECP_ENABLED .or. ECP_NATOM == 0) then
                  return
            end if
            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellCenters => AOBasis%ShellCenters &
                  )
                  shellab_max = ((NShells + 1) * NShells) / 2
                  !$omp parallel &
                  !$omp default(shared) &
                  !$omp private(tcc, tab, tac, tchiac, tchiab, slma, slmb, slmk, xyzwork) &
                  !$omp private(gab, a, b, c, cc, shella, shellb) &
                  !$omp private(phia, phib) &
                  !$omp shared(vmat)

                  call pp_allocate_scratch(tcc, tab, tac, tchiac, tchiab, slma, slmb, slmk, xyzwork)

                  !$omp do schedule(guided)            
                  do shellab = 1, shellab_max
                        !
                        ! Extract individual indices from the compound shell-pair index
                        !
                        call pp_decode_pq(shellab, NShells, shella, shellb)
                        a = ShellCenters(shella)
                        b = ShellCenters(shellb)
                        call pp_loadgto(phia, shella, AOBasis)
                        call pp_loadgto(phib, shellb, AOBasis)
                        !
                        ! Loop over ECP centers
                        !
                        gab = ZERO
                        do cc = 1, ECP_NATOM
                              c = ECP_ATOM(cc)
                              if ((a .ne. c) .and. (b .ne. c)) then
                                    call pp_ecpab(gab, a, b, c, phia, phib, &
                                          slma, slmb, slmk, tab, tchiab, xyzwork, System)
                              else if ((a .ne. c) .and. (b .eq. c)) then
                                    call pp_ecpac(gab, a, c, phia, phib, slma, tac, tchiac, xyzwork, System)
                              else if ((a .eq. c) .and. (b .ne. c)) then
                                    call pp_ecpbc(gab, b, c, phia, phib, slma, tac, tchiac, xyzwork, System)
                              else
                                    !
                                    ! a == c .and. b == c
                                    !
                                    call pp_ecpcc(gab, c, phia, phib, tcc, xyzwork, System) 
                              end if
                        end do
                        !
                        ! Not two threads will update the matrices at the same time because
                        ! each parallel task corresponds to a distinct bra-ket shell pair
                        !
                        call pp_vmat_update(vmat, gab, shella, shellb, phia, phib, AOBasis)
                  end do
                  !$omp end do nowait
                  deallocate(tcc)
                  deallocate(tab)
                  deallocate(tac)
                  deallocate(tchiac)
                  deallocate(tchiab)
                  deallocate(slma)
                  deallocate(slmb)
                  deallocate(slmk)
                  deallocate(xyzwork)
                  !$omp end parallel
            end associate
      end subroutine pp_V_2


      pure subroutine pp_vxyzmat_update(vxmat, vymat, vzmat, gab_a_lo, gab_a_hi, &
            gab_b_lo, gab_b_hi, shella, shellb, phia, phib, AOBasis)
            
            real(F64), dimension(:,:), intent(inout) :: vxmat
            real(F64), dimension(:,:), intent(inout) :: vymat
            real(F64), dimension(:,:), intent(inout) :: vzmat
            real(F64), dimension(:), intent(in)      :: gab_a_lo
            real(F64), dimension(:), intent(in)      :: gab_a_hi
            real(F64), dimension(:), intent(in)      :: gab_b_lo
            real(F64), dimension(:), intent(in)      :: gab_b_hi
            integer, intent(in)                      :: shella
            integer, intent(in)                      :: shellb
            type(tgtodef), intent(in)                :: phia
            type(tgtodef), intent(in)                :: phib
            type(TAOBasis), intent(in)               :: AOBasis

            real(F64) :: tx, ty, tz
            real(F64) :: norm_a, norm_b, norm_ab
            integer :: nfunca, nfuncb
            integer :: nfunca_hi, nfunca_lo
            integer :: idxa0, idxb0
            integer :: la, lb
            integer :: vshift
            integer :: i, j
            integer :: r, s
            integer :: v
            integer :: lax, lay, laz
            integer :: lbx, lby, lbz

            idxa0 = AOBasis%ShellLocCart(shella)
            la = phia%l
            nfunca = basis_NAngFuncCart(la)
            nfunca_hi = basis_NAngFuncCart(la+1)
            if (la > 0) then
                  nfunca_lo = basis_NAngFuncCart(la-1)
            else
                  nfunca_lo = 0
            end if
            
            idxb0 = AOBasis%ShellLocCart(shellb)
            lb = phib%l
            nfuncb = basis_NAngFuncCart(lb)

            do j = 1, nfuncb
                  s = idxb0 + j - 1
                  lbx = ECP_LL(j, lb)
                  lby = ECP_MM(j, lb)
                  lbz = ECP_NN(j, lb)
                  norm_b = phib%norm(j)
                  do i = 1, nfunca
                        r  = idxa0 + i - 1
                        lax = ECP_LL(i, la)
                        lay = ECP_MM(i, la)
                        laz = ECP_NN(i, la)
                        norm_a = phia%norm(i)
                        
                        vshift = (j - 1) * nfunca_hi
                        !
                        ! Bra orbital differentiated (raised angular momentum)
                        !
                        v = pp_angfuncidx(lax+1, lay, laz) + vshift
                        tx = gab_a_hi(v)

                        v = pp_angfuncidx(lax, lay+1, laz) + vshift
                        ty = gab_a_hi(v)

                        v = pp_angfuncidx(lax, lay, laz+1) + vshift
                        tz = gab_a_hi(v)
                        !
                        ! Bra orbital differentiated (lowered angular momentum)
                        !
                        vshift = (j - 1) * nfunca_lo
                        
                        if (lax > 0) then
                              v = pp_angfuncidx(lax-1, lay, laz) + vshift
                              tx = tx + real(lax, F64) * gab_a_lo(v)
                        end if

                        if (lay > 0) then
                              v = pp_angfuncidx(lax, lay-1, laz) + vshift
                              ty = ty + real(lay, F64) * gab_a_lo(v)
                        end if

                        if (laz > 0) then
                              v = pp_angfuncidx(lax, lay, laz-1) + vshift
                              tz = tz + real(laz, F64) * gab_a_lo(v)
                        end if
                        !
                        ! Ket orbital differentiated (raised angular momentum)
                        !
                        v = i + (pp_angfuncidx(lbx+1, lby, lbz) - 1) * nfunca
                        tx = tx + gab_b_hi(v)

                        v = i + (pp_angfuncidx(lbx, lby+1, lbz) - 1) * nfunca
                        ty = ty + gab_b_hi(v)

                        v = i + (pp_angfuncidx(lbx, lby, lbz+1) - 1) * nfunca
                        tz = tz + gab_b_hi(v)
                        !
                        ! Ket orbital differentiated (lowered angular momentum)
                        !
                        if (lbx > 0) then
                              v = i + (pp_angfuncidx(lbx-1, lby, lbz) - 1) * nfunca
                              tx = tx + real(lbx, F64) * gab_b_lo(v)
                        end if

                        if (lby > 0) then
                              v = i + (pp_angfuncidx(lbx, lby-1, lbz) - 1) * nfunca
                              ty = ty + real(lby, F64) * gab_b_lo(v)
                        end if

                        if (lbz > 0) then
                              v = i + (pp_angfuncidx(lbx, lby, lbz-1) - 1) * nfunca
                              tz = tz + real(lbz, F64) * gab_b_lo(v)
                        end if

                        norm_ab = norm_a * norm_b
                        vxmat(r, s) = vxmat(r, s) + norm_ab * tx
                        vymat(r, s) = vymat(r, s) + norm_ab * ty
                        vzmat(r, s) = vzmat(r, s) + norm_ab * tz
                  end do
            end do
      end subroutine pp_vxyzmat_update


      pure subroutine pp_vmat_update(vmat, gab, shella, shellb, phia, phib, AOBasis)
            real(F64), dimension(:,:), intent(inout) :: vmat
            real(F64), dimension(:), intent(in)      :: gab
            integer, intent(in)                      :: shella
            integer, intent(in)                      :: shellb
            type(tgtodef), intent(in)                :: phia
            type(tgtodef), intent(in)                :: phib
            type(TAOBasis), intent(in)               :: AOBasis

            real(F64) :: norm_a, norm_b, prefac
            integer :: nfunca, nfuncb
            integer :: idxa0, idxb0
            integer :: i, j, k, l
            integer :: v

            idxa0 = AOBasis%ShellLocCart(shella)
            idxb0 = AOBasis%ShellLocCart(shellb)
            nfunca = basis_NAngFuncCart(phia%l)
            nfuncb = basis_NAngFuncCart(phib%l)
            v = 1
            do j = 1, nfuncb
                  norm_b = phib%norm(j)
                  do i = 1, nfunca
                        norm_a = phia%norm(i) 
                        prefac = norm_a * norm_b
                        k = idxa0 + i - 1
                        l = idxb0 + j - 1
                        vmat(k, l) = vmat(k, l) + prefac * gab(v)
                        v = v + 1
                  end do
            end do
      end subroutine pp_vmat_update


      subroutine pp_ecpcc(gab, c, phia, phib, tcc, xyzwork, System)
            real(F64), dimension(:), intent(inout) :: gab
            integer, intent(in)                    :: c
            type(tgtodef), intent(in)              :: phia
            type(tgtodef), intent(in)              :: phib
            real(F64), dimension(:), intent(out)   :: tcc
            real(F64), dimension(:), intent(out)   :: xyzwork
            type(TSystem), intent(in)              :: System
  
            integer :: lax, lay, laz
            integer :: lbx, lby, lbz
            integer :: la, lb
            integer :: nfunca, nfuncb
            integer :: i, j
            integer :: v
            integer :: ecpcenter
            real(F64) :: tgam, tchi
            integer :: lmax

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(System%ZNumbers(c))
            nfunca = basis_NAngFuncCart(la)
            nfuncb = basis_NAngFuncCart(lb)
            lmax = ECP_LMAX(ecpcenter)

            call pp_preptcc(tcc, phia, phib, ecpcenter, lmax)

            if (.not. ECP_LOCALPP(ecpcenter)) then
                  v = 1
                  do j = 1, nfuncb
                        lbx = ECP_LL(j, lb)
                        lby = ECP_MM(j, lb)
                        lbz = ECP_NN(j, lb)
                        do i = 1, nfunca
                              lax = ECP_LL(i, la)
                              lay = ECP_MM(i, la)
                              laz = ECP_NN(i, la)
                              gab(v) = gab(v) + pp_gamcc(lax, lay, laz, &
                                    lbx, lby, lbz, ecpcenter, tcc, xyzwork)
                              v = v + 1
                        end do
                  end do
            else
                  v = 1
                  do j = 1, nfuncb
                        lbx = ECP_LL(j, lb)
                        lby = ECP_MM(j, lb)
                        lbz = ECP_NN(j, lb)
                        do i = 1, nfunca
                              lax = ECP_LL(i, la)
                              lay = ECP_MM(i, la)
                              laz = ECP_NN(i, la)
                              tgam = pp_gamcc(lax, lay, laz, lbx, lby, lbz, &
                                    ecpcenter, tcc, xyzwork)
                              tchi = pp_chicc(lax, lay, laz, lbx, lby, lbz, tcc)
                              gab(v) = gab(v) + tgam + tchi
                              v = v + 1
                        end do
                  end do
            end if
      end subroutine pp_ecpcc


      subroutine pp_preptcc(tcc, phia, phib, ecpcenter, lmax)
            real(F64), dimension(:), intent(out) :: tcc
            type(tgtodef), intent(in) :: phia
            type(tgtodef), intent(in) :: phib
            integer, intent(in) :: ecpcenter
            integer, intent(in) :: lmax

            type(TINT_PARAMS) :: tp
            integer :: l
            integer :: k0

            tp%phia = phia
            tp%phib = phib
            tp%ecpcenter = ecpcenter
            k0 = ECP_K0(ecpcenter)

            if (ECP_LOCALPP(ecpcenter)) then
                  tp%l = -1
                  tp%k0 = k0
                  TCC(1) = pp_ccint(tp)
            end if

            k0 = k0 + ECP_NGAUSS(1, ecpcenter)
            do l = 0, lmax
                  tp%l = l
                  tp%k0 = k0
                  TCC(2+l) = pp_ccint(tp)
                  k0 = k0 + ECP_NGAUSS(2+l, ecpcenter)
            end do
      end subroutine pp_preptcc


      subroutine pp_preptac(tac, phia, phib, lena, ecpcenter, lmax)
            real(F64), dimension(0:, 0:, 0:), intent(out) :: tac
            type(tgtodef), intent(in)    :: phia
            type(tgtodef), intent(in)    :: phib
            real(F64), intent(in) :: lena
            integer, intent(in)          :: ecpcenter
            integer, intent(in)          :: lmax

            type(TINT_PARAMS) :: tp
            integer :: alpha, k0
            integer :: l, lambda1
            integer :: incl1
            integer :: npoint
            integer :: lambda10, lambda11
            logical :: conv
            real(F64) :: tac_element

            tp%phia = phia
            tp%phib = phib
            tp%ecpcenter = ecpcenter
            tp%lena = lena

            do alpha = 0, phia%l
                  tp%alpha = alpha
                  k0 = ECP_K0(ecpcenter)
                  k0 = k0 + ECP_NGAUSS(1, ecpcenter)
                  do l = 0, lmax
                        tp%l = l
                        tp%k0 = k0
                        lambda10 = max(l-alpha, 0)
                        lambda11 = l + alpha
                        incl1 = modulo(lambda11-lambda10, 2)
                        do lambda1 = lambda10+incl1, lambda11, 2
                              tp%lambdaa = lambda1
                              call pp_cheb_integrate(tac_element, CHEB_EPS, CHEB_NMAX, npoint, conv, &
                                    pp_tintac, tp)
                              TAC(lambda1, l, alpha) = tac_element
                        end do
                        k0 = k0 + ECP_NGAUSS(2+l, ecpcenter)
                  end do
            end do
      end subroutine pp_preptac


      subroutine pp_preptchiac(tchiac, phia, phib, lena, ecpcenter)
            real(F64), dimension(0:, 0:), intent(out) :: tchiac
            type(tgtodef), intent(in)    :: phia
            type(tgtodef), intent(in)    :: phib
            real(F64), intent(in) :: lena
            integer, intent(in)          :: ecpcenter

            type(TINT_PARAMS) :: tp
            integer :: alpha
            integer :: lambda1
            integer :: incl1
            integer :: npoint
            integer :: lambda10, lambda11
            logical :: conv
            real(F64) :: tchiac_element
            integer :: la, lb

            la = phia%l
            lb = phib%l
            tp%phia = phia
            tp%phib = phib
            tp%ecpcenter = ecpcenter
            tp%lena = lena

            do alpha = 0, la
                  tp%alpha = alpha
                  lambda10 = 0
                  lambda11 = lb + alpha
                  incl1 = modulo(lambda11-lambda10, 2)
                  do lambda1 = lambda10+incl1, lambda11, 2
                        tp%lambdaa = lambda1
                        call pp_cheb_integrate(tchiac_element, CHEB_EPS, CHEB_NMAX, npoint, conv, &
                              pp_tintchiac, tp)
                        TCHIAC(lambda1, alpha) = tchiac_element
                  end do
            end do
      end subroutine pp_preptchiac


      subroutine pp_preptchiab(tchiab, phia, phib, iprma, iprmb, &
            la, lb, lena, lenb, klen, ecpcenter)
            
            real(F64), dimension(0:, 0:), intent(out) :: tchiab
            type(tgtodef), intent(in)    :: phia
            type(tgtodef), intent(in)    :: phib
            integer, intent(in)          :: iprma
            integer, intent(in)          :: iprmb
            integer, intent(in)          :: la
            integer, intent(in)          :: lb
            real(F64), intent(in)        :: lena
            real(F64), intent(in)        :: lenb
            real(F64), intent(in)        :: klen
            integer, intent(in)          :: ecpcenter

            type(TINT_PARAMS) :: tp
            integer :: n
            integer :: lambda1
            integer :: incl1
            integer :: npoint
            integer :: lambda10, lambda11
            logical :: conv
            real(F64) :: tchiab_element

            tp%phia = phia
            tp%phib = phib
            tp%iprma = iprma
            tp%iprmb = iprmb
            tp%ecpcenter = ecpcenter
            tp%lena = lena
            tp%lenb = lenb
            tp%klen = klen
            do n = 0, la + lb
                  tp%n = n
                  lambda10 = 0
                  lambda11 = n
                  incl1 = modulo(lambda11-lambda10, 2)
                  do lambda1 = lambda10+incl1, lambda11, 2
                        tp%lambdaa = lambda1
                        call pp_cheb_integrate(tchiab_element, CHEB_EPS, CHEB_NMAX, npoint, conv, &
                              pp_tintchiab, tp)
                        TCHIAB(lambda1, n) = tchiab_element
                  end do
            end do
      end subroutine pp_preptchiab


      subroutine pp_ecpac(gab, a, c, phia, phib, slma, tac, tchiac, xyzwork, System)
            real(F64), dimension(:), intent(inout) :: gab
            integer, intent(in)                         :: a
            integer, intent(in)                         :: c
            type(tgtodef), intent(in)                   :: phia
            type(tgtodef), intent(in)                   :: phib
            real(F64), dimension(:), intent(out) :: slma
            real(F64), dimension(0:, 0:, 0:), intent(out) :: tac
            real(F64), dimension(0:, 0:), intent(out) :: tchiac
            real(F64), dimension(:), intent(out) :: xyzwork
            type(TSystem), intent(in)                     :: System

            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: ax, ay, az
            real(F64) :: axn, ayn, azn
            integer :: lax, lay, laz
            integer :: lbx, lby, lbz
            integer :: la, lb
            real(F64) :: lena
            integer :: nfunca, nfuncb
            integer :: i, j
            integer :: v
            integer :: ecpcenter
            real(F64) :: prefac
            integer :: lmax
            real(F64) :: tgam, tchi

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(System%ZNumbers(c))
            nfunca = basis_NAngFuncCart(la)
            nfuncb = basis_NAngFuncCart(lb)

            ax(0) = ONE
            ax(1) = System%AtomCoords(1, a) - System%AtomCoords(1, c)
            ax(2) = ax(1)**2

            ay(0) = ONE
            ay(1) = System%AtomCoords(2, a) - System%AtomCoords(2, c)
            ay(2) = ay(1)**2

            az(0) = ONE
            az(1) = System%AtomCoords(3, a) - System%AtomCoords(3, c)
            az(2) = az(1)**2

            do i = 3, la
                  ax(i) = ax(1) * ax(i-1)
                  ay(i) = ay(1) * ay(i-1)
                  az(i) = az(1) * az(i-1)
            end do

            lena = sqrt(ax(2) + ay(2) + az(2))
            axn = ax(1) / lena
            ayn = ay(1) / lena
            azn = az(1) / lena
            !
            ! Genarate array of real spherical harmonics
            !
            lmax = ECP_LMAX(ecpcenter)
            if (.not. ECP_LOCALPP(ecpcenter)) then
                  call rsh(SLMA, lmax+la, axn, ayn, azn)
            else
                  call rsh(SLMA, max(lmax+la, la+lb), axn, ayn, azn)
            end if
            !
            ! Tabulate radial integrals. All Cartesian Gaussian functions
            ! in a shell share the same set of radial integrals. In other
            ! words, the radial integrals computed above can be re-used by
            ! orbitals that differ by in an angular function.
            !
            call pp_preptac(tac, phia, phib, lena, ecpcenter, lmax)
            if (ECP_LOCALPP(ecpcenter)) then
                  call pp_preptchiac(tchiac, phia, phib, lena, ecpcenter)
            end if

            if (.not. ECP_LOCALPP(ecpcenter)) then
                  v = 1
                  prefac = FOUR * PI
                  do j = 1, nfuncb
                        lbx = ECP_LL(j, lb)
                        lby = ECP_MM(j, lb)
                        lbz = ECP_NN(j, lb)
                        do i = 1, nfunca
                              lax = ECP_LL(i, la)
                              lay = ECP_MM(i, la)
                              laz = ECP_NN(i, la)
                              gab(v) = gab(v) + prefac * pp_gamac(lax, lay, laz, lbx, lby, lbz, &
                                    ax, ay, az, SLMA, ecpcenter, tac, xyzwork)
                              v = v +  1
                        end do
                  end do
            else
                  v = 1
                  prefac = FOUR * PI
                  do j = 1, nfuncb
                        lbx = ECP_LL(j, lb)
                        lby = ECP_MM(j, lb)
                        lbz = ECP_NN(j, lb)
                        do i = 1, nfunca
                              lax = ECP_LL(i, la)
                              lay = ECP_MM(i, la)
                              laz = ECP_NN(i, la)
                              tgam = pp_gamac(lax, lay, laz, lbx, lby, lbz, &
                                    ax, ay, az, SLMA, ecpcenter, tac, xyzwork)
                              tchi = pp_chiac(lax, lay, laz, lbx, lby, lbz, ax, ay, az, &
                                    SLMA, tchiac, xyzwork)
                              gab(v) = gab(v) + prefac * (tgam + tchi)
                              v = v + 1
                        end do
                  end do
            end if
      end subroutine pp_ecpac


      subroutine pp_ecpbc(gab, b, c, phia, phib, slmb, tbc, tchibc, xyzwork, System)
            real(F64), dimension(:), intent(inout) :: gab
            integer, intent(in)                         :: b
            integer, intent(in)                         :: c
            type(tgtodef), intent(in)                   :: phia
            type(tgtodef), intent(in)                   :: phib
            real(F64), dimension(:), intent(out) :: slmb
            real(F64), dimension(0:, 0:, 0:), intent(out) :: tbc
            real(F64), dimension(0:, 0:), intent(out) :: tchibc
            real(F64), dimension(:), intent(out) :: xyzwork
            type(TSystem), intent(in) :: System

            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: bx, by, bz
            real(F64) :: bxn, byn, bzn
            integer :: lax, lay, laz
            integer :: lbx, lby, lbz
            integer :: la, lb
            real(F64) :: lenb
            integer :: nfunca, nfuncb
            integer :: i, j
            integer :: v
            integer :: ecpcenter
            real(F64) :: prefac
            integer :: lmax
            real(F64) :: tgam, tchi

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(System%ZNumbers(c))
            nfunca = basis_NAngFuncCart(la)
            nfuncb = basis_NAngFuncCart(lb)

            bx(0) = ONE
            bx(1) = System%AtomCoords(1, b) - System%AtomCoords(1, c)
            bx(2) = bx(1)**2

            by(0) = ONE
            by(1) = System%AtomCoords(2, b) - System%AtomCoords(2, c)
            by(2) = by(1)**2

            bz(0) = ONE
            bz(1) = System%AtomCoords(3, b) - System%AtomCoords(3, c)
            bz(2) = bz(1)**2

            do i = 3, lb
                  bx(i) = bx(1) * bx(i-1)
                  by(i) = by(1) * by(i-1)
                  bz(i) = bz(1) * bz(i-1)
            end do

            lenb = sqrt(bx(2) + by(2) + bz(2))
            bxn = bx(1) / lenb
            byn = by(1) / lenb
            bzn = bz(1) / lenb
            !
            ! Genarate array of real spherical harmonics
            !
            lmax = ECP_LMAX(ecpcenter)
            if (.not. ECP_LOCALPP(ecpcenter)) then
                  call rsh(SLMB, lmax+lb, bxn, byn, bzn)
            else
                  call rsh(SLMB, max(lmax+lb, la+lb), bxn, byn, bzn)
            end if
            !
            ! Tabulate radial integrals. All Cartesian Gaussian functions
            ! in a shell share the same set of radial integrals. In other
            ! words, the radial integrals computed above can be re-used by
            ! orbitals that differ by in an angular function.
            !
            call pp_preptac(tbc, phib, phia, lenb, ecpcenter, lmax)
            if (ECP_LOCALPP(ecpcenter)) then
                  call pp_preptchiac(tchibc, phib, phia, lenb, ecpcenter)
            end if

            if (.not. ECP_LOCALPP(ecpcenter)) then
                  v = 1
                  prefac = FOUR * PI
                  do j = 1, nfuncb
                        lbx = ECP_LL(j, lb)
                        lby = ECP_MM(j, lb)
                        lbz = ECP_NN(j, lb)
                        do i = 1, nfunca
                              lax = ECP_LL(i, la)
                              lay = ECP_MM(i, la)
                              laz = ECP_NN(i, la)
                              gab(v) = gab(v) + prefac * pp_gamac(lbx, lby, lbz, lax, lay, laz, &
                                    bx, by, bz, SLMB, ecpcenter, tbc, xyzwork)
                              v = v + 1
                        end do
                  end do
            else
                  v = 1
                  prefac = FOUR * PI
                  do j = 1, nfuncb
                        lbx = ECP_LL(j, lb)
                        lby = ECP_MM(j, lb)
                        lbz = ECP_NN(j, lb)
                        do i = 1, nfunca
                              lax = ECP_LL(i, la)
                              lay = ECP_MM(i, la)
                              laz = ECP_NN(i, la)
                              tgam = pp_gamac(lbx, lby, lbz, lax, lay, laz, &
                                    bx, by, bz, SLMB, ecpcenter, tbc, xyzwork)
                              tchi = pp_chiac(lbx, lby, lbz, lax, lay, laz, bx, by, bz, &
                                    SLMB, tchibc, xyzwork)
                              gab(v) = gab(v) + prefac * (tgam + tchi)
                              v = v + 1
                        end do
                  end do
            end if
      end subroutine pp_ecpbc


      subroutine pp_ecpab(gab, a, b, c, phia, phib, &
            slma, slmb, slmk, tab, tchiab, xyzwork, System)
            
            real(F64), dimension(:), intent(inout) :: gab
            integer, intent(in)                  :: a
            integer, intent(in)                  :: b
            integer, intent(in)                  :: c
            type(tgtodef), intent(in)            :: phia
            type(tgtodef), intent(in)            :: phib
            real(F64), dimension(:), intent(out) :: slma
            real(F64), dimension(:), intent(out) :: slmb
            real(F64), dimension(:), intent(out) :: slmk
            real(F64), dimension(0:, 0:, 0:, 0:), intent(out) :: tab
            real(F64), dimension(0:, 0:), intent(out) :: tchiab
            real(F64), dimension(:), intent(out)  :: xyzwork
            type(TSystem), intent(in)            :: System

            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: ax, ay, az
            real(F64) :: axn, ayn, azn
            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: bx, by, bz
            real(F64) :: bxn, byn, bzn

            integer :: lax, lay, laz
            integer :: lbx, lby, lbz
            integer :: la, lb
            real(F64) :: lena, lenb
            integer :: nfunca, nfuncb, nint
            integer :: i, j
            integer :: l
            integer :: v
            integer :: ecpcenter
            real(F64) :: prefac
            integer :: lmax
            integer :: npoint
            logical :: conv
            type(TINT_PARAMS) :: tp
            real(F64) :: tab_element
            integer :: alpha, beta, n, lambda1, lambda2, k0
            integer :: lambda10, lambda11
            integer :: lambda20, lambda21
            integer :: incl1, incl2
            integer :: iprma, iprmb
            real(F64) :: kvecx, kvecy, kvecz
            real(F64) :: klen, klen2
            real(F64) :: kxn, kyn, kzn
            real(F64) :: zetaa, zetab
            real(F64) :: prefac0

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(System%ZNumbers(c))
            nfunca = basis_NAngFuncCart(la)
            nfuncb = basis_NAngFuncCart(lb)
            nint = nfunca * nfuncb

            ax(0) = ONE
            ax(1) = System%AtomCoords(1, a) - System%AtomCoords(1, c)
            ax(2) = ax(1)**2

            ay(0) = ONE
            ay(1) = System%AtomCoords(2, a) - System%AtomCoords(2, c)
            ay(2) = ay(1)**2

            az(0) = ONE
            az(1) = System%AtomCoords(3, a) - System%AtomCoords(3, c)
            az(2) = az(1)**2

            do i = 3, la
                  ax(i) = ax(1) * ax(i-1)
                  ay(i) = ay(1) * ay(i-1)
                  az(i) = az(1) * az(i-1)
            end do

            lena = sqrt(ax(2) + ay(2) + az(2))
            axn = ax(1) / lena
            ayn = ay(1) / lena
            azn = az(1) / lena

            bx(0) = ONE
            bx(1) = System%AtomCoords(1, b) - System%AtomCoords(1, c)
            bx(2) = bx(1)**2

            by(0) = ONE
            by(1) = System%AtomCoords(2, b) - System%AtomCoords(2, c)
            by(2) = by(1)**2

            bz(0) = ONE
            bz(1) = System%AtomCoords(3, b) - System%AtomCoords(3, c)
            bz(2) = bz(1)**2
            
            do i = 3, lb
                  bx(i) = bx(1) * bx(i-1)
                  by(i) = by(1) * by(i-1)
                  bz(i) = bz(1) * bz(i-1)
            end do            

            lenb = sqrt(bx(2) + by(2) + bz(2))
            bxn = bx(1) / lenb
            byn = by(1) / lenb
            bzn = bz(1) / lenb
            !
            ! Genarate array of real spherical harmonics
            !
            lmax = ECP_LMAX(ecpcenter)
            call rsh(SLMA, lmax+la, axn, ayn, azn)
            call rsh(SLMB, lmax+lb, bxn, byn, bzn)

            tp%phia = phia
            tp%phib = phib
            tp%ecpcenter = ecpcenter
            tp%lena = lena
            tp%lenb = lenb
            !
            ! Pre-calculate radial integrals. All Cartesian Gaussian functions
            ! in a shell share the same set of radial integrals. The radial
            ! integrals computed above can be re-used by orbitals that differ
            ! only by an angular function index.
            !
            do n = 0, la + lb
                  do alpha = n-lb, min(n, la)
                        tp%alpha = alpha
                        beta = n - alpha
                        tp%beta = beta
                        k0 = ECP_K0(ecpcenter)
                        k0 = k0 + ECP_NGAUSS(1, ecpcenter)
                        do l = 0, lmax
                              tp%l = l
                              tp%k0 = k0
                              lambda10 = max(l-alpha, 0)
                              lambda11 = l + alpha
                              incl1 = modulo(lambda11-lambda10, 2)
                              do lambda1 = lambda10+incl1, lambda11, 2
                                    tp%lambdaa = lambda1
                                    lambda20 = max(l-beta, 0)
                                    lambda21 = l + beta
                                    incl2 = modulo(lambda21-lambda20, 2)
                                    do lambda2 = lambda20+incl2, lambda21, 2
                                          tp%lambdab = lambda2
                                          call pp_cheb_integrate(tab_element, CHEB_EPS, CHEB_NMAX, npoint, conv, &
                                                pp_tintab, tp)
                                          TAB(lambda2, lambda1, l, n) = tab_element
                                    end do
                              end do
                              k0 = k0 + ECP_NGAUSS(2+l, ecpcenter)
                        end do
                  end do
            end do

            v = 1
            prefac = SIXTEEN * PI**2
            do j = 1, nfuncb
                  lbx = ECP_LL(j, lb)
                  lby = ECP_MM(j, lb)
                  lbz = ECP_NN(j, lb)
                  do i = 1, nfunca
                        lax = ECP_LL(i, la)
                        lay = ECP_MM(i, la)
                        laz = ECP_NN(i, la)
                        gab(v) = gab(v) + prefac * pp_gamab(lax, lay, laz, lbx, lby, lbz, &
                              ax, ay, az, bx, by, bz, SLMA, SLMB, ecpcenter, tab)
                        v = v + 1
                  end do
            end do
            !
            ! Local part of the ECP
            !
            if (ECP_LOCALPP(ecpcenter)) then
                  do iprma = 1, phia%nprm
                        do iprmb = 1, phib%nprm
                              zetaa = phia%expn(iprma)
                              zetab = phib%expn(iprmb)
                              kvecx = TWO * (zetaa * ax(1) + zetab * bx(1))
                              kvecy = TWO * (zetaa * ay(1) + zetab * by(1))
                              kvecz = TWO * (zetaa * az(1) + zetab * bz(1))
                              klen2 = kvecx**2 + kvecy**2 + kvecz**2
                              if (klen2 > ZERO) then
                                    klen = sqrt(klen2)
                                    kxn = kvecx / klen
                                    kyn = kvecy / klen
                                    kzn = kvecz / klen
                              else
                                    klen = ZERO
                                    kxn = ZERO
                                    kyn = ZERO
                                    kzn = ZERO
                              end if
                              call rsh(SLMK, la+lb, kxn, kyn, kzn)
                              call pp_preptchiab(tchiab, phia, phib, iprma, iprmb, &
                                    la, lb, lena, lenb, klen, ecpcenter)
                              prefac0 = FOUR * PI * phia%cntr(iprma) * phib%cntr(iprmb)
                              v = 1
                              do j = 1, nfuncb
                                    lbx = ECP_LL(j, lb)
                                    lby = ECP_MM(j, lb)
                                    lbz = ECP_NN(j, lb)
                                    do i = 1, nfunca
                                          lax = ECP_LL(i, la)
                                          lay = ECP_MM(i, la)
                                          laz = ECP_NN(i, la)
                                          gab(v) = gab(v) + prefac0 * pp_chiab(lax, lay, laz, &
                                                lbx, lby, lbz, ax, ay, az, bx, by, bz, SLMK, &
                                                tchiab, xyzwork)
                                          v = v + 1 
                                    end do
                              end do
                        end do
                  end do
            end if
      end subroutine pp_ecpab


      function pp_gamcc(lax, lay, laz, lbx, lby, lbz, ecpcenter, tcc, xyzwork)
            real(F64)    :: pp_gamcc
            integer, intent(in) :: lax, lay, laz
            integer, intent(in) :: lbx, lby, lbz
            integer, intent(in) :: ecpcenter
            real(F64), dimension(:), intent(in) :: tcc
            real(F64), dimension(:), intent(out) :: xyzwork

            integer :: l

            pp_gamcc = ZERO
            do l = 0, ECP_LMAX(ecpcenter)
                  pp_gamcc = pp_gamcc + pp_gamccl(l, lax, lay, laz, lbx, lby, lbz, tcc, xyzwork)
            end do
      end function pp_gamcc


      function pp_gamac(lax, lay, laz, lbx, lby, lbz, ax, ay, az, &
            wslma, ecpcenter, tac, xyzwork)
            
            real(F64)                            :: pp_gamac
            integer, intent(in)                         :: lax, lay, laz
            integer, intent(in)                         :: lbx, lby, lbz
            real(F64), dimension(0:), intent(in) :: ax, ay, az
            real(F64), dimension(:), intent(in)  :: wslma
            integer, intent(in)                         :: ecpcenter
            real(F64), dimension(0:, 0:, 0:), intent(in) :: tac
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64) :: axayaz
            real(F64) :: binomax, binomay, binomaz
            real(F64) :: phase
            integer :: alpx, alpy, alpz, alpha
            integer :: la
            integer :: l

            la = lax + lay + laz
            pp_gamac = ZERO
            do alpx = 0, lax
                  binomax = ECP_BINOM(lax, alpx)
                  do alpy = 0, lay
                        binomay = ECP_BINOM(lay, alpy)
                        do alpz = 0, laz
                              binomaz = ECP_BINOM(laz, alpz)
                              axayaz = binomax * binomay * binomaz * &
                                    ax(lax-alpx) * ay(lay-alpy) * az(laz-alpz)
                              alpha = alpx + alpy + alpz
                              phase = dphase(la-alpha)
                              do l = 0, ECP_LMAX(ecpcenter)
                                    pp_gamac = pp_gamac + phase * axayaz * &
                                          pp_gamacl(l, alpx, alpy, alpz, lbx, lby, lbz, wslma, &
                                          tac, xyzwork)
                              end do
                        end do
                  end do
            end do
      end function pp_gamac


      function pp_chiac(lax, lay, laz, lbx, lby, lbz, ax, ay, az, wslma, tchiac, xyzwork)
            real(F64)                            :: pp_chiac
            integer, intent(in)                  :: lax, lay, laz
            integer, intent(in)                  :: lbx, lby, lbz
            real(F64), dimension(0:), intent(in) :: ax, ay, az
            real(F64), dimension(:), intent(in)  :: wslma
            real(F64), dimension(0:, 0:), intent(in) :: tchiac
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64) :: axayaz
            real(F64) :: binomax, binomay, binomaz
            real(F64) :: phase
            real(F64) :: vs
            real(F64) :: slma
            real(F64) :: sum0, sum1
            real(F64) :: tchiac_element
            integer :: x, y, z
            integer :: alpx, alpy, alpz, alpha
            integer :: la, lb
            integer :: lambda, lambda0, lambda1, incl, mu

            la = lax + lay + laz
            lb = lbx + lby + lbz
            pp_chiac = ZERO
            do alpx = 0, lax
                  binomax = ECP_BINOM(lax, alpx)
                  do alpy = 0, lay
                        binomay = ECP_BINOM(lay, alpy)
                        do alpz = 0, laz
                              binomaz = ECP_BINOM(laz, alpz)
                              axayaz = binomax * binomay * binomaz * &
                                    ax(lax-alpx) * ay(lay-alpy) * az(laz-alpz)
                              alpha = alpx + alpy + alpz
                              phase = dphase(la-alpha)
                              lambda0 = 0
                              lambda1 = alpha + lb
                              incl = modulo(lambda1-lambda0, 2)
                              x = alpx + lbx
                              y = alpy + lby
                              z = alpz + lbz
                              sum1 = ZERO
                              do lambda = lambda0+incl, lambda1, 2
                                    tchiac_element = TCHIAC(lambda, alpha)
                                    sum0 = ZERO
                                    do mu = -lambda, lambda
                                          vs = rshv(lambda, mu, x, y, z, ECP_U, XYZWORK)
                                          slma = wslma(slmpos(lambda, mu))
                                          sum0 = sum0 + vs * slma
                                    end do
                                    sum1 = sum1 + tchiac_element * sum0
                              end do
                              pp_chiac = pp_chiac + axayaz * phase * sum1
                        end do
                  end do
            end do
      end function pp_chiac


      function pp_gamab(lax, lay, laz, lbx, lby, lbz, ax, ay, az, &
            bx, by, bz, wslma, wslmb, ecpcenter, tab)
            
            real(F64)                            :: pp_gamab
            integer, intent(in)                  :: lax, lay, laz
            integer, intent(in)                  :: lbx, lby, lbz
            real(F64), dimension(0:), intent(in) :: ax, ay, az
            real(F64), dimension(0:), intent(in) :: bx, by, bz
            real(F64), dimension(:), intent(in)  :: wslma
            real(F64), dimension(:), intent(in)  :: wslmb
            integer, intent(in)                  :: ecpcenter
            real(F64), dimension(0:, 0:, 0:, 0:), intent(in) :: tab

            real(F64) :: axayaz
            real(F64) :: bxbybz
            real(F64) :: binomax, binomay, binomaz
            real(F64) :: binombx, binomby, binombz
            real(F64) :: phase
            integer :: alpx, alpy, alpz, alpha
            integer :: betx, bety, betz, beta
            integer :: la, lb
            integer :: l

            la = lax + lay + laz
            lb = lbx + lby + lbz

            pp_gamab = ZERO
            do alpx = 0, lax
                  binomax = ECP_BINOM(lax, alpx)
                  do alpy = 0, lay
                        binomay = ECP_BINOM(lay, alpy)
                        do alpz = 0, laz
                              binomaz = ECP_BINOM(laz, alpz)
                              axayaz = binomax * binomay * binomaz * &
                                    ax(lax-alpx) * ay(lay-alpy) * az(laz-alpz)
                              do betx = 0, lbx
                                    binombx = ECP_BINOM(lbx, betx)
                                    do bety = 0, lby
                                          binomby = ECP_BINOM(lby, bety)
                                          do betz = 0, lbz
                                                binombz = ECP_BINOM(lbz, betz)
                                                bxbybz = binombx * binomby * binombz * &
                                                      bx(lbx-betx) * by(lby-bety) * bz(lbz-betz)
                                                alpha = alpx + alpy + alpz
                                                beta = betx + bety + betz
                                                phase = dphase(la+lb-alpha-beta)
                                                do l = 0, ECP_LMAX(ecpcenter)
                                                      pp_gamab = pp_gamab + phase * axayaz * bxbybz * &
                                                            pp_gamabl(l, alpx, alpy, alpz, betx, bety, betz, &
                                                            wslma, wslmb, tab)
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end function pp_gamab


      function pp_chiab(lax, lay, laz, lbx, lby, lbz, ax, ay, az, &
            bx, by, bz, wslmk, tchiab, xyzwork)
            
            real(F64)                            :: pp_chiab
            integer, intent(in)                  :: lax, lay, laz
            integer, intent(in)                  :: lbx, lby, lbz
            real(F64), dimension(0:), intent(in) :: ax, ay, az
            real(F64), dimension(0:), intent(in) :: bx, by, bz
            real(F64), dimension(:), intent(in)  :: wslmk
            real(F64), dimension(0:, 0:), intent(in) :: tchiab
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64) :: axayaz
            real(F64) :: bxbybz
            real(F64) :: binomax, binomay, binomaz
            real(F64) :: binombx, binomby, binombz
            real(F64) :: phase
            integer :: alpx, alpy, alpz, alpha
            integer :: betx, bety, betz, beta
            integer :: la, lb
            integer :: lambda10, lambda11, lambda1, mu1
            integer :: n
            integer :: incl1
            integer :: x, y, z
            real(F64) :: sum0, sum1
            real(F64) :: tchiab_element, vab

            la = lax + lay + laz
            lb = lbx + lby + lbz

            pp_chiab = ZERO
            do alpx = 0, lax
                  binomax = ECP_BINOM(lax, alpx)
                  do alpy = 0, lay
                        binomay = ECP_BINOM(lay, alpy)
                        do alpz = 0, laz
                              binomaz = ECP_BINOM(laz, alpz)
                              axayaz = binomax * binomay * binomaz * &
                                    ax(lax-alpx) * ay(lay-alpy) * az(laz-alpz)
                              do betx = 0, lbx
                                    binombx = ECP_BINOM(lbx, betx)
                                    do bety = 0, lby
                                          binomby = ECP_BINOM(lby, bety)
                                          do betz = 0, lbz
                                                binombz = ECP_BINOM(lbz, betz)
                                                bxbybz = binombx * binomby * binombz * &
                                                      bx(lbx-betx) * by(lby-bety) * bz(lbz-betz)
                                                alpha = alpx + alpy + alpz
                                                beta = betx + bety + betz
                                                phase = dphase(la+lb-alpha-beta)
                                                n = alpha + beta
                                                lambda10 = 0
                                                lambda11 = n
                                                incl1 = modulo(lambda11-lambda10, 2)
                                                x = alpx + betx
                                                y = alpy + bety
                                                z = alpz + betz
                                                sum1 = ZERO
                                                do lambda1 = lambda10+incl1, lambda11, 2
                                                      tchiab_element = TCHIAB(lambda1, n)
                                                      sum0 = ZERO
                                                      do mu1 = -lambda1, lambda1
                                                            vab = rshv(lambda1, mu1, x, y, z, &
                                                                  ECP_U, XYZWORK)
                                                            sum0 = sum0 + vab * wslmk(slmpos(lambda1, mu1))
                                                      end do
                                                      sum1 = sum1 + tchiab_element * sum0
                                                end do
                                                pp_chiab = pp_chiab + phase * axayaz * bxbybz * sum1
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end function pp_chiab


      function pp_gamabl(l, alpx, alpy, alpz, betx, bety, betz, wslma, wslmb, tab)
            !
            ! Compute GammaAB (Eq. 11 in Ref. 1) for the given Alpha and Beta. Compared to Eq. 11,
            ! the summation over the projector's l and m numbers, is also carried out.
            ! 
            ! 1. Flores-Moreno, R., Alvarez-Mendez, R.J., Vela, A., Koster, A.M. J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            !
            real(F64)                           :: pp_gamabl
            integer, intent(in)                 :: l
            integer, intent(in)                 :: alpx
            integer, intent(in)                 :: alpy
            integer, intent(in)                 :: alpz
            integer, intent(in)                 :: betx
            integer, intent(in)                 :: bety
            integer, intent(in)                 :: betz
            real(F64), dimension(:), intent(in) :: wslma
            real(F64), dimension(:), intent(in) :: wslmb
            real(F64), dimension(0:, 0:, 0:, 0:), intent(in) :: tab

            real(F64) :: omegaa, omegab, slma, slmb
            real(F64) :: tab_element
            integer :: lambda1, lambda2, mu1, mu2, m
            integer :: lambda10, lambda11, lambda20, lambda21
            integer :: alpha, beta, n
            integer :: incl1, incl2
            real(F64), dimension((l+alpx+alpy+alpz)/2+1) :: omegaa_slma
            real(F64), dimension((l+betx+bety+betz)/2+1) :: omegab_slmb
            integer :: v1, v2

            alpha = alpx + alpy + alpz
            beta = betx + bety + betz
            n = alpha + beta
            lambda10 = max(l-alpha, 0)
            lambda11 = l + alpha
            incl1 = modulo(lambda11-lambda10, 2)
            lambda20 = max(l-beta, 0)
            lambda21 = l + beta
            incl2 = modulo(lambda21-lambda20, 2)
            pp_gamabl = ZERO
            do m = -l, l
                  !
                  ! Carry out summations over mu1 and mu2, which can be done before
                  ! the summations over lambda1 and lambda2.
                  !
                  v1 = 1
                  do lambda1 = lambda10+incl1, lambda11, 2
                        omegaa_slma(v1) = ZERO
                        do mu1 = -lambda1, lambda1
                              omegaa = pp_readomega(lambda1, mu1, l, m, alpx, alpy, alpz)
                              slma = wslma(slmpos(lambda1, mu1))
                              omegaa_slma(v1) = omegaa_slma(v1) + omegaa * slma
                        end do
                        v1 = v1 + 1
                  end do

                  v2 = 1
                  do lambda2 = lambda20+incl2, lambda21, 2
                        omegab_slmb(v2) = ZERO
                        do mu2 = -lambda2, lambda2
                              omegab = pp_readomega(lambda2, mu2, l, m, betx, bety, betz)
                              slmb = wslmb(slmpos(lambda2, mu2))
                              omegab_slmb(v2) = omegab_slmb(v2) + omegab * slmb
                        end do
                        v2 = v2 + 1
                  end do
                  
                  v1 = 1
                  do lambda1 = lambda10+incl1, lambda11, 2
                        v2 = 1
                        do lambda2 = lambda20+incl2, lambda21, 2
                              tab_element = TAB(lambda2, lambda1, l, n)
                              pp_gamabl = pp_gamabl + tab_element * omegaa_slma(v1) * omegab_slmb(v2)
                              v2 = v2 + 1
                        end do
                        v1 = v1 + 1
                  end do
            end do
      end function pp_gamabl


      function pp_gamacl(l, alpx, alpy, alpz, lbx, lby, lbz, wslma, tac, xyzwork)
            real(F64)                           :: pp_gamacl
            integer, intent(in)                 :: l
            integer, intent(in)                 :: alpx
            integer, intent(in)                 :: alpy
            integer, intent(in)                 :: alpz
            integer, intent(in)                 :: lbx
            integer, intent(in)                 :: lby
            integer, intent(in)                 :: lbz
            real(F64), dimension(:), intent(in) :: wslma
            real(F64), dimension(0:, 0:, 0:), intent(in) :: tac
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64) :: omegaa, slma
            real(F64) :: tac_element, vb
            integer :: lambda1, mu1, m
            integer :: lambda10, lambda11
            integer :: alpha
            integer :: incl1

            alpha = alpx + alpy + alpz
            lambda10 = max(l-alpha, 0)
            lambda11 = l + alpha
            incl1 = modulo(lambda11-lambda10, 2)
            pp_gamacl = ZERO
            do lambda1 = lambda10+incl1, lambda11, 2
                  tac_element = TAC(lambda1, l, alpha)
                  do m = -l, l
                        do mu1 = -lambda1, lambda1
                              omegaa = pp_readomega(lambda1, mu1, l, m, alpx, alpy, alpz)
                              slma = wslma(slmpos(lambda1, mu1))
                              vb = rshv(l, m, lbx, lby, lbz, ECP_U, XYZWORK)
                              pp_gamacl = pp_gamacl + tac_element * omegaa * vb * slma
                        end do
                  end do
            end do
      end function pp_gamacl


      function pp_gamccl(l, lax, lay, laz, lbx, lby, lbz, tcc, xyzwork)
            real(F64)           :: pp_gamccl
            integer, intent(in) :: l
            integer, intent(in) :: lax
            integer, intent(in) :: lay
            integer, intent(in) :: laz
            integer, intent(in) :: lbx
            integer, intent(in) :: lby
            integer, intent(in) :: lbz
            real(F64), dimension(:), intent(in) :: tcc
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64) :: tcc_element
            real(F64) :: va, vb
            integer :: m

            tcc_element = TCC(2+l) 
            pp_gamccl = ZERO
            do m = -l, l
                  va = rshv(l, m, lax, lay, laz, ECP_U, XYZWORK)
                  vb = rshv(l, m, lbx, lby, lbz, ECP_U, XYZWORK)
                  pp_gamccl = pp_gamccl + tcc_element * va * vb
            end do
      end function pp_gamccl


      pure function pp_chicc(lax, lay, laz, lbx, lby, lbz, tcc)
            real(F64)    :: pp_chicc
            integer, intent(in) :: lax
            integer, intent(in) :: lay
            integer, intent(in) :: laz
            integer, intent(in) :: lbx
            integer, intent(in) :: lby
            integer, intent(in) :: lbz
            real(F64), dimension(:), intent(in) :: tcc

            real(F64) :: rint
            real(F64) :: omegaint
            integer :: x, y, z
            real(F64), parameter :: fourpi = FOUR * PI
            real(F64) :: a, b, c, d

            x = lax + lbx
            y = lay + lby
            z = laz + lbz

            if ((modulo(x, 2) .ne. 0) .or. &
                  (modulo(y, 2) .ne. 0) .or. &
                  (modulo(z, 2) .ne. 0)) then
                  pp_chicc = ZERO
                  return
            end if
            
            a = dblfact(x-1)
            b = dblfact(y-1)
            c = dblfact(z-1)
            d = dblfact(x+y+z+1)
            omegaint = fourpi * a * b * c / d
            rint = TCC(1)
            pp_chicc = omegaint * rint
      end function pp_chicc


      pure subroutine pp_cheb_r(r, jac, x)
            ! --------------------------------------------------------------
            ! Transformation to radial variable and the Jacobian (R**2 not
            ! included). See Eq. 27 in [1].
            ! --------------------------------------------------------------
            ! 1. Moreno-Flores, R., Alvarez-Mendez, R., Vela, A., and
            !    Koster, A.M., Half-Numerical Evaluation of Pseudopotential
            !    Integrals, J. Comput. Chem. 27, 1009 (2006)
            ! --------------------------------------------------------------
            real(F64), intent(out) :: r
            real(F64), intent(out) :: jac
            real(F64), intent(in)  :: x

            real(F64), parameter :: log2 = log(TWO)

            r = ONE / log2 * log(TWO/(ONE-x))
            jac = ONE / (log2 * (ONE - x))
      end subroutine pp_cheb_r


      pure subroutine pp_cheb_precalc(points, weights, p)
            ! --------------------------------------------------
            ! Calculate and store points and weights of Gauss-
            ! Cheyshev quadrature:
            ! WEIGHTS(I+1) <- Omega_{2*I+1),
            ! POINTS(I+1) <- r_{2*I+1},
            ! I = 0, 1, ..., (N-1)/2.
            ! Jacobian of R->X transformation is incorporated
            ! into quadrature weights. See [1] for details
            ! of the recurrence relations used here.
            ! --------------------------------------------------
            ! 1. Perez-Jorda, Jose M., San-Fabian, Emilio, and
            !    Moscardo, Federico, A simple, reliable and
            !    efficient scheme for automatic numerical
            !    integration, Comp. Phys. Comm. 70, 271 (1992)
            !
            real(F64), dimension(:), intent(out) :: points
            real(F64), dimension(:), intent(out) :: weights
            integer, intent(in)                  :: p

            real(F64) :: s, c, s1, c1, s0, c0
            real(F64) :: x, w, r, jac, sold, cold
            integer :: k

            s1 = sin(PI/real((p+1)/2, F64))
            c1 = cos(PI/real((p+1)/2, F64))
            c0 = sqrt(0.5d+0 * (ONE + c1))
            s0 = s1 / (two * c0)
            s = s0
            c = c0
            do k = 1, p, 2
                  x = real(p+1-2*k, F64) / real(p+1, F64) + TWO / PI * (ONE + TWO/THREE * s**2) * s * c
                  w = s**4
                  call pp_cheb_r(r, jac, x)
                  points((k+1)/2) = r
                  weights((k+1)/2) = w * jac
                  sold = s
                  cold = c
                  s = sold * c1 + cold * s1
                  c = cold * c1 - sold * s1
            end do
      end subroutine pp_cheb_precalc


      subroutine pp_cheb_integrate(val, eps, nmax, npoint, conv, &
            integrand, tp)

            real(F64), intent(out)        :: val
            real(F64), intent(in)         :: eps
            integer, intent(in)           :: nmax
            integer, intent(out)          :: npoint
            logical, intent(out)          :: conv
            type(TINT_PARAMS), intent(in) :: tp
            procedure(pp_tintab)      :: integrand
            ! interface
            !       function integrand(r, t)
            !             import :: TINT_PARAMS
            !             real(F64) :: integrand
            !             real(F64), intent(in) :: r
            !             type(TINT_PARAMS), intent(in) :: t
            !       end function integrand
            ! end interface
            
            real(F64) :: approx1, approx2, approx3
            real(F64) :: c, s, c0, c1, s0, s1
            real(F64) :: x, r, w
            real(F64) :: cold, sold
            real(F64) :: jac
            real(F64) :: f
            integer :: p, k, i
            !
            ! First approximation
            !
            p = CHEB_N1
            approx1 = ZERO
            do i = 1, p
                  w = CHEB_W1(i)
                  r = CHEB_X1(i)
                  f = integrand(r, tp)
                  approx1 = approx1 + w * f
            end do
            approx1 = real(16, F64)/real(3*(p+1), F64) * approx1
            !
            ! Second approximation
            !
            p = 2 * p + 1
            approx2 = ZERO
            do i = 1, (p - 1) / 2
                  w = CHEB_W2(i)
                  r = CHEB_X2(i)
                  f = integrand(r, tp)
                  approx2 = approx2 + w * f
            end do
            approx2 = ONE/TWO * approx1 + real(16, F64)/real(3*(p+1), F64) * approx2
            !
            ! Third approximation
            !
            p = 2 * p + 1
            approx3 = ZERO
            do i = 1, (p - 1) / 2
                  w = CHEB_W3(i)
                  r = CHEB_X3(i)
                  f = integrand(r, tp)
                  approx3 = approx3 + w * f
            end do
            approx3 = ONE/TWO * approx2 + real(16, F64)/real(3*(p+1), F64) * approx3
            !
            ! Higher-order approximations
            !
            conv = .false.
            if ((approx3 - approx2)**2 <= abs(approx3 - approx1) * eps) then
                  conv = .true.
            end if

            p = 2 * p + 1

            do while ((p <= nmax) .and. (.not. conv))
                  approx1 = approx2
                  approx2 = approx3
                  
                  s1 = sin(PI/real((p+1)/2, F64))
                  c1 = cos(PI/real((p+1)/2, F64))
                  c0 = sqrt(ONE/TWO * (ONE + c1))
                  s0 = s1 / (two * c0)
                  s = s0
                  c = c0
                  approx3 = ZERO
                  do k = 1, p, 2
                        x = real(p+1-2*k, F64) / real(p+1, F64) + TWO / PI * (ONE + TWO/THREE * s**2) * s * c
                        call pp_cheb_r(r, jac, x)
                        w = s**4 * jac
                        f = integrand(r, tp)
                        approx3 = approx3 + w * f
                        sold = s
                        cold = c
                        s = sold * c1 + cold * s1
                        c = cold * c1 - sold * s1
                  end do
                  approx3 = ONE/TWO * approx2 + real(16, F64)/real(3*(p+1), F64) * approx3

                  if ((approx3 - approx2)**2 <= abs(approx3 - approx1) * eps) then
                        conv = .true.
                  end if

                  p = 2 * p + 1
            end do

            npoint = (p - 1) / 2
            val = approx3
      end subroutine pp_cheb_integrate


      pure function pp_falambda(r, lambda, phi, a, la)
            real(F64)                    :: pp_falambda
            real(F64), intent(in)        :: r
            integer, intent(in)          :: lambda
            type(tgtodef), intent(in)    :: phi
            real(F64), intent(in)        :: a
            integer, intent(in)          :: la

            integer :: i, ka
            real(F64) :: alpha, c
            real(F64) :: ra2
            real(F64) :: klambda, karg
            real(F64) :: e2
            real(F64) :: rla

            ra2 = (r - a)**2
            ka = phi%nprm 
            pp_falambda = ZERO
            do i = 1, ka
                  alpha = phi%expn(i) 
                  c = phi%cntr(i) 
                  karg = TWO * alpha * a * r
                  klambda = pp_spherbessel(lambda, karg)
                  e2 = exp(-alpha * ra2)
                  pp_falambda = pp_falambda + c * klambda * e2
            end do
            rla = r**la
            pp_falambda = pp_falambda * rla
      end function pp_falambda


      pure function pp_tintab(r, tp)
            real(F64)              :: pp_tintab
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: fa, fb, ul

            associate (phia => tp%phia, phib => tp%phib, &
                  lambdaa => tp%lambdaa, lambdab => tp%lambdab, l => tp%l, &
                  ecpcenter => tp%ecpcenter, lena => tp%lena, lenb => tp%lenb, &
                  k0 => tp%k0, alpha => tp%alpha, beta => tp%beta)
                  
                  ul = pp_ulpot(r, l, ecpcenter, k0)
                  fa = pp_falambda(r, lambdaa, phia, lena, alpha)
                  fb = pp_falambda(r, lambdab, phib, lenb, beta)
                  pp_tintab = fa * fb * ul
            end associate
      end function pp_tintab


      pure function pp_tintac(r, tp)
            real(F64)              :: pp_tintac
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: fa, phibval, ul
            
            associate (phia => tp%phia, phib => tp%phib, &
                  lambdaa => tp%lambdaa, l => tp%l, &
                  ecpcenter => tp%ecpcenter, lena => tp%lena, &
                  k0 => tp%k0, alpha => tp%alpha)
                  
                  ul = pp_ulpot(r, l, ecpcenter, k0)
                  fa = pp_falambda(r, lambdaa, phia, lena, alpha)
                  phibval = pp_phir(r, phib)
                  pp_tintac = fa * phibval * ul
            end associate
      end function pp_tintac


      pure function pp_tintchiac(r, tp)
            real(F64)              :: pp_tintchiac
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: fa, phibval, ul
            integer :: k0
            
            associate (phia => tp%phia, phib => tp%phib, &
                  lambdaa => tp%lambdaa, ecpcenter => tp%ecpcenter, &
                  lena => tp%lena, alpha => tp%alpha) 
                  
                  k0 = ECP_K0(ecpcenter)
                  ul = pp_ulpot(r, -1, ecpcenter, k0)
                  fa = pp_falambda(r, lambdaa, phia, lena, alpha)
                  phibval = pp_phir(r, phib)
                  pp_tintchiac = fa * phibval * ul
            end associate
      end function pp_tintchiac


      pure function pp_tintchiab(r, tp)
            real(F64)              :: pp_tintchiab
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: r2
            real(F64) :: ul
            integer :: k0
            real(F64) :: zeta1, zeta2
            real(F64) :: bess, e
            real(F64) :: lena2, lenb2
            real(F64) :: rn
            
            r2 = r**2

            associate (phia => tp%phia, phib => tp%phib, &
                  lambdaa => tp%lambdaa, ecpcenter => tp%ecpcenter, &
                  lena => tp%lena, lenb => tp%lenb, n => tp%n, &
                  klen => tp%klen, iprma => tp%iprma, iprmb => tp%iprmb)
                  
                  rn = r**n
                  lena2 = lena**2
                  lenb2 = lenb**2
                  k0 = ECP_K0(ecpcenter)
                  ul = pp_ulpot(r, -1, ecpcenter, k0)
                  zeta1 = phia%expn(iprma)
                  zeta2 = phib%expn(iprmb)
                  bess = pp_spherbessel(lambdaa, klen * r)
                  e = exp(-zeta1 * (r2 + lena2) - zeta2 * (r2 + lenb2) + klen * r)
                  pp_tintchiab = bess * e * ul * rn
            end associate
      end function pp_tintchiab


      pure function pp_tintcc(r, tp)
            real(F64)              :: pp_tintcc
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: phiaval, phibval, ul

            associate (l => tp%l, phia => tp%phia, phib => tp%phib, &
                  ecpcenter => tp%ecpcenter, k0 => tp%k0)
                  
                  phiaval = pp_phir(r, phia)
                  phibval = pp_phir(r, phib)
                  ul = pp_ulpot(r, l, ecpcenter, k0)
                  pp_tintcc = phiaval * phibval * ul
            end associate
      end function pp_tintcc


      pure function pp_tintchicc(r, tp)
            real(F64)              :: pp_tintchicc
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: phiaval, phibval, ul
            integer :: k0

            associate (phia => tp%phia, phib => tp%phib, &
                  ecpcenter => tp%ecpcenter)
                  k0 = ECP_K0(ecpcenter)
                  phiaval = pp_phir(r, phia)
                  phibval = pp_phir(r, phib)
                  ul = pp_ulpot(r, -1, ecpcenter, k0)
                  pp_tintchicc = phiaval * phibval * ul
            end associate
      end function pp_tintchicc


      pure function pp_phir(r, phi)
            !
            ! Compute radial part of a Cartesian
            ! Gaussian contracted orbital.
            ! 
            ! The contraction coefficients that are
            ! used do not include the L-dependent
            ! normalization constants.
            !
            real(F64)             :: pp_phir
            real(F64), intent(in) :: r
            type(tgtodef), intent(in)    :: phi

            integer :: i, ka
            real(F64) :: alpha, c
            real(F64) :: r2
            real(F64) :: e2
            real(F64) :: rl

            r2 = r**2
            ka = phi%nprm
            pp_phir = ZERO
            do i = 1, ka
                  alpha = phi%expn(i)
                  c = phi%cntr(i)
                  e2 = exp(-alpha * r2)
                  pp_phir = pp_phir + c * e2
            end do
            rl = r**phi%l
            pp_phir = rl * pp_phir
      end function pp_phir


      pure function pp_ulpot(r, l, ecpcenter, k0)
            ! ----------------------------------------------------
            ! Compute the numerical value of the pseudopotential
            ! U_L at the radius R.
            ! ----------------------------------------------------
            ! R    - Input, the radius at which potential
            !        is evalueated
            ! L    - Angular momentum part: L=-1 for the local
            !        part of U_L, L=0 for S, L=1 for P, etc.
            ! IDX  - Index of ECP center
            ! K0   - Index of the first coefficient belonging to
            !        U_L in ECP_EXPN array
            !
            real(F64)                            :: pp_ulpot
            real(F64), intent(in)                :: r
            integer, intent(in)                  :: l
            integer, intent(in)                  :: ecpcenter
            integer, intent(in)                  :: k0

            integer :: ngauss
            real(F64) :: e, alpha, c
            real(F64), dimension(0:2) :: rn
            integer :: nkl
            integer :: k
            
            rn(0) = ONE
            rn(1) = r
            rn(2) = r**2
            ngauss = ECP_NGAUSS(2+l, ecpcenter)
            pp_ulpot = ZERO
            do k = k0, k0 + ngauss - 1
                  alpha = ECP_EXPN(k)
                  c = ECP_COEFF(k)
                  nkl = ECP_NKL(k)
                  e = exp(-alpha * rn(2))
                  pp_ulpot = pp_ulpot + c * rn(nkl) * e
            end do
      end function pp_ulpot

      
      pure function pp_spherbessel_interp(m, x)
            !
            ! Return an interpolated value of the Boys function F_m(X)
            ! for a given X in (0, X_ASYMPTOTIC).
            !
            real(F64)             :: pp_spherbessel_interp
            integer, intent(in)   :: m
            real(F64), intent(in) :: x

            real(F64) :: ab_sum
            integer :: k

            k = floor((x - INTERP_XMIN) / DELTAX)
            !
            ! a = INTERP_XMIN + real(k, F64) * DELTAX
            ! b = INTERP_XMIN + real(k+1, F64) * DELTAX
            !
            ab_sum = TWO * INTERP_XMIN + real(2 * k + 1, F64) * DELTAX
            call cheb_approx_9(pp_spherbessel_interp, x, BESSEL_TABLE(:, k+1, m), ab_sum, DELTAX)
      end function pp_spherbessel_interp


      pure function pp_spherbessel(n, x)
            !
            ! Modified spherical Bessel function of the first kind
            ! scaled by EXP(-X),
            ! SPHERBESSEL(N,X) = SQRT(PI/(2*X)) * I_{N+1/2}(X) * EXP(-X),
            ! where I_{N+1/2}(X) denotes a modified Bessel function
            ! of the first kind. If needed, lower order spherical Bessel
            ! functions can be generated using numerically stable
            ! backward recurrence relation,
            ! SPHERBESSEL(n-1,X) = (2 * N + 1) / X * SPHERBESSEL(N,X) 
            ! + SPHERBESSEL(n+1,X)
            !
            ! The relative error of this function is below 6.1E-15:
            ! |s_{ref}(n,x) - s_{approx}(n,x)| / s_{ref} <= 6.1E-15 for n <=12
            ! and for 0 <= x < Inf.
            !
            real(F64)             :: pp_spherbessel
            integer, intent(in)   :: n
            real(F64), intent(in) :: x
            !
            ! Accuracy of the spherical Besel function
            !
            real(F64), parameter :: eps = 1.0E-14_F64
            real(F64) :: invx, t
            integer :: k

            if (x < INTERP_XMIN) then
                  !
                  ! Chebyshev interpolation does not work well
                  ! for small X, so here we use the reference (slow)
                  ! spherbessel implementation.
                  !
                  pp_spherbessel = pp_spherbessel_n(n, x, eps)
            else if (x < INTERP_XMAX) then
                  pp_spherbessel = pp_spherbessel_interp(n, x)
            else
                  !
                  ! Asymptotic expansion:
                  ! i_n(x) = Sum_{k=1}^{n+1} BESSEL_ASYMP(k, n) 1 / x^k
                  !
                  invx = ONE / x
                  t = BESSEL_ASYMP(n+1, n)
                  do k = n, 1, -1
                        t = BESSEL_ASYMP(k, n) + invx * t
                  end do
                  pp_spherbessel = invx * t
            end if
      end function pp_spherbessel
      

      pure function pp_spherbessel_n(n, x, eps)
            ! -------------------------------------------------------------
            ! Arbitrary order modified spherical Bessel function of the
            ! first kind.
            ! -------------------------------------------------------------
            ! 1. Segura, J., Fernandez de Cordoba, P., and Ratis, Yu.L.,
            !    A code to evaluate modified Bessel functions based on
            !    the continued fraction method,
            !    Comp. Phys. Comm. 105, 263 (1997)
            ! -------------------------------------------------------------
            ! N   
            !     Non-negative integer, the order of the modified
            !     spherical Bessel function of the first kind
            ! X   
            !     Argument of the Bessel function, x>=0
            ! EPS
            !     Parameter controlling the accuracy of the continued
            !     fraction H
            !
            !
            real(F64)             :: pp_spherbessel_n
            integer, intent(in)   :: n
            real(F64), intent(in) :: x
            real(F64), intent(in) :: eps

            real(F64) :: k1, k2, knew
            real(F64) :: i1, i2
            real(F64) :: xinv
            real(F64) :: b, c, d, dc, h
            real(F64) :: d2n1
            real(F64), parameter :: halfpi = frac12 * PI
            integer :: i
            integer, parameter :: maxit = 1000
            integer, parameter :: maxit2 = 2 * maxit

            if (x > ZERO) then
                  xinv = ONE / x
                  k1 = halfpi * xinv
                  k2 = halfpi * (xinv + xinv**2)
                  d2n1 = THREE
                  do i = 2, n
                        knew = d2n1 * xinv * k2 + k1
                        k1 = k2
                        k2 = knew
                        d2n1 = real(2 * i + 1, F64)
                  end do
                  !
                  ! Steed's algorithm is used to compute 
                  ! the continued fraction H of Eq. 7 in [1]
                  !
                  d = ONE / (d2n1 * xinv)
                  dc = d
                  c = dc
                  i = 2
                  do while (abs(dc/c) > eps .and. i <= maxit2)
                        b = (d2n1 + real(i, F64)) * xinv
                        d = ONE / (d + b)
                        dc = (b * d - ONE) * dc
                        c = c + dc
                        i = i + 2
                  end do
                  h = c
                  !
                  ! Wronskian, Eq. 9 in [1]
                  !
                  i1 = PI/TWO / (x**2 * (h * k1 + k2))
                  if (n .eq. 0) then
                        pp_spherbessel_n = i1
                  else
                        i2 = h * i1
                        pp_spherbessel_n = i2
                  end if
            else
                  if (n > 0) then
                        pp_spherbessel_n = ZERO
                  else
                        pp_spherbessel_n = ONE
                  end if
            end if
      end function pp_spherbessel_n

      
      pure function pp_ccint(tp)
            real(F64)              :: pp_ccint
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: v, akl, ckl
            real(F64) :: cr, ar, cs, as
            real(F64) :: a, c
            integer :: k, r, s
            integer :: nkl, n, ngauss

            associate (l => tp%l, phia => tp%phia, phib => tp%phib, &
                  ecpcenter => tp%ecpcenter, k0 => tp%k0)
                  v = ZERO
                  ngauss = ECP_NGAUSS(2+l, ecpcenter)
                  do k = k0, k0 + ngauss -1
                        akl = ECP_EXPN(k)
                        nkl = ECP_NKL(k)
                        ckl = ECP_COEFF(k)
                        n = nkl + phia%l + phib%l 
                        do r = 1, phia%nprm
                              cr = phia%cntr(r)
                              ar = phia%expn(r)
                              do s = 1, phib%nprm
                                    cs = phib%cntr(s)
                                    as = phib%expn(s)
                                    a = akl + ar + as
                                    c = ckl * cr * cs
                                    v = v + c * pp_gaussint(n, a)
                              end do
                        end do
                  end do
                  pp_ccint = v
            end associate
      end function pp_ccint


      pure function pp_gaussint(k, a)
            !
            ! Int_0^Inf x^k Exp(-a x^2) dx
            !
            real(F64)             :: pp_gaussint
            integer, intent(in)   :: k
            real(F64), intent(in) :: a

            real(F64) :: t1, t2, t3
            integer :: n

            n = k / 2

            if (modulo(k, 2) .eq. 0) then
                  !
                  ! Int_0^Inf x^{2n} Exp(-a x^2) dx
                  !
                  t1 = dblfact(2*n-1)
                  t2 = real(2**(n+1), F64)
                  t3 = sqrt(PI/(a**(k+1)))
                  pp_gaussint = t1 / t2 * t3
            else
                  !
                  ! Int_0^Inf x^{2n+1} Exp(-a x^2) dx
                  !
                  t1 = fact(n)
                  t2 = TWO
                  t3 = a**(n+1)
                  pp_gaussint = t1 / (t2 * t3)
            end if
      end function pp_gaussint


      pure function pp_angfuncidx(lx, ly, lz)
            !
            ! The index of an angular function belonging to
            ! an orbital shell having the orbital angular momentum
            ! L=LX + LY + LZ. LX, LY, LZ define the angular
            ! prefactor of a Gaussian atomic orbital:
            ! phi(r) = X^{LX} Y^{LY} Z^{LZ} EXP(-ALPHA R^2).
            !
            integer             :: pp_angfuncidx
            integer, intent(in) :: lx
            integer, intent(in) :: ly
            integer, intent(in) :: lz

            integer :: l, iang0

            l = lx + ly + lz
            iang0 = pp_numaxayaz(l-1)
            pp_angfuncidx = ECP_ANGFIDX(iang0+lxlylzpos(lx, ly, lz))
      end function pp_angfuncidx
end module Pseudopotential
