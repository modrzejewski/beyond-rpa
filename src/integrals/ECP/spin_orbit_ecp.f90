! ----------------------------------------------------------------------
! ONE-ELECTRON INTEGRALS OF THE SPIN-ORBIT PART OF RELATIVISTIC
! EFFECTIVE CORE POTENTIALS
! ----------------------------------------------------------------------
!
! Spin-Orbit Pseudopotential
! --------------------------
! The following short intro on relativistic pseudopotentials (PPs) is
! designed to help the user of this module in navigating through different
! conventions of PPs tabulated in the literature.
!
! The full relativistic pseudopotential (SO + averaged parts)
! has the form
!
! U = Sum_{l=0} Sum_{j=|l-1/2|}^{l+1/2} U_{lj}(r) P_{lj},
!
! where
!
! P_{lj} = Sum_{m=-j}^j |ljm><ljm|.
! 
! To isolate the spin-orbit coupling part, U is written as a sum
! of the scalar, j-averaged potential and the spin-orbit coupling part.
! The sum of these two is the full relativistic U(r):
!
! U = Uav + Uso
! Uav = Sum_{l=0}^{lmax} Uav_l(r) P_l
! Uso = Sum_{l=1}^{lmax} DeltaU_l(r) * 1/(2l+1) * (l * P_{l,l+1/2} - (l+1) * P_{l,l-1/2})
!
! where
!
! P_l = P_{l,l-1/2} + P_{l,l+1/2}
! Uav_l(r) = 1/(2l+1) * (l * U_{l,l-1/2}(r) + (l+1) * U_{l,l+1/2}(r))
! DeltaU_l(r) = U_{l,l+1/2}(r) - U_{l,l-1/2}(r)
!
! (Based one Eqs. 79-86 in Ref. 1 and Eqs. 1-7 in Ref. 2) Note that in our implementation
! the spin-orbit part do not include a local, multiplicative pseudopotential.
!
! The matrix elements of the scalar pseudopotential Uav(r) are computed with
! the subroutines of the ECPINT module. This module provides the subroutines
! for computing the matrix of Uso(r).
!
! To avoid using the projectors on states with specified j, our implementation uses
! the methematically equivalent form of Uso(r) with explicit L and S
!
! Uso = Sum_{l=1}^{lmax} 2*DeltaU_l(r)/(2l+1) P_l L*S P_l =
!          Sum_{l=1}^{lmax} Uso_l(r) P_l L*S P_l
! Uso_l(r) = 2/(2l+1) * (U_{l,l+1/2}(r) - U_{l,l-1/2}(r)) for l >= 1
!
! where L is the operator (vector) of orbital angular momentum.
! Detailed derivation of this form of Uso(r) is provided in Ref. 3.
!
! Parametrization of Uav(r) and Uso(r)
! -------------------------------------
! Uav = Sum_{l=0}^{lmax} Uav_l(r) P_l
! Uso = Sum_{l=1}^{lmax} Uso_l(r) P_l L*S P_l
! Uav_l(r) = Sum_{k=0}^{ngauss} Cav_{kl} r^{n_{kl}} Exp(-Alpha_{kl} r^2)
! Uso_l(r) = Sum_{k=1}^{ngauss} Cso_{kl} r^{n_{kl}} Exp(-Alpha_{kl} r^2)
!
! Cso_{KL}
!       Linear expansion coefficients of Uso_l(r). Cso_{KL} is stored in
!       the global array ECP_SO_COEFF.
! Cav_{KL}
!       Linear expansion coefficients of Uav_l(r). Stored in ECP_COEFF.
!
! r^{n_{kl}}, 0 <= n_{kl} <= 2
!       Radial prefactor. The exponent n_{kl} is stored in the global
!       array ECP_NKL. r^{n_{kl}} includes the r^2 part of the Jacobian
!       of Cartesian->spherical transformation.
!
! Alpha_{kl}
!       Gaussian exponent stored in the global array ECP_EXPN.
!
! The set of exponents (Alpha_{kl}) and exponents of R (n_{kl}) is the
! same for Uav and Uso.
!
! Pseudopotential input format
! ----------------------------
! A textfile containing the relativistic PP parameters is constructed
! as follows.
!
! $ecp
! X-Spin-Orbit-ECP NCoreElectrons lmax+1
! NGaussS CommentString
!   Cav_{1, S}  n_{1, S} Alpha_{1, S}
!   Cav_{2, S}  n_{1, S} Alpha_{2, S}
!                      .
!                      .
!   (Definition of the S-component of the pseudopotential)
!                      .
!                      .
!   C_{NGaussS, S} n_{NGaussS, S} Alpha_{NGaussS, S}
! NGaussP CommentString      
!   Cav_{1, P}  Cso_{1, P}  n_{1, P} Alpha_{1, P}
!   Cav_{2, P}  Cso_{2, P}  n_{1, P} Alpha_{2, P}
!                      .
!                      .
!   (Definition of the P-component of the pseudopotential)
!                      .
!                      .
!   Cav_{NGaussP, P} Cso_{NGaussP, P} n_{NGaussP, P} Alpha_{NGaussP, P}
!                      .
!                      .
!                      .
! NGaussLmax CommentString
!   Cav_{1, Lmax}  Cso_{1, Lmax}  n_{1, Lmax} Alpha_{1, Lmax}
!   Cav_{2, Lmax}  Cso_{2, Lmax}  n_{1, Lmax} Alpha_{2, Lmax}
!                      .
!                      .
!   (Definition of the Lmax-component of the pseudopotential)
!                      .
!                      .
!   Cav_{NGaussLmax, Lmax}  Cso_{NGaussLmax, Lmax}  n_{NGaussLmax, Lmax} Alpha_{NGaussLmax, Lmax}
! $end
! 
! X
!          Symbol of the chemical element for which the
!          pseudopotential is defined.
! NCoreElectrons
!          Number of core electrons represented by the pseudopotential.
!
! Note that the coefficients of the spin-orbit part are given only
! for the P and higher angular momenta.
!
! Example: formatting coefficients from Ref. 4
! ---------------------------------------------
! Table 1 in Ref. 4 includes the following coefficients for Cu:
!
! l  j       Alpha            C             n 
! -----------------------------------------------------------
! s  1/2     30.110543        355.750512    2
! p  1/2     32.692614        233.909794    2
! p  3/2     32.770339        233.891173    2
!
! Note that there is no local pseudopotential in Ref. 4 and
! the Jacobian factor r^2 is included in the parameter n.
! Using the definitions
!
! Uav_l(r) = 1/(2l+1) * (l * U_{l,l-1/2}(r) + (l+1) * U_{l,l+1/2}(r))
! Uso_l(r) = 2/(2l+1) * (U_{l,l+1/2}(r) - U_{l,l-1/2}(r)) for l >= 1
!
! we get the j-averaged and spin-orbit parts of the pseudopotential:
!
! Uav(s) = r^2 * 355.750512 * Exp(-30.110543 * r^2)
! Uav(p) = r^2 * 1/(2*1+1) * (1 * 233.909794 * Exp(-32.692614 * r^2) + (1+1) * 233.89117 * Exp(-32.770339 * r^2))
! Uso(p) = r^2 * 2/(2*1+1) * (-233.909794 * Exp(-32.692614 * r^2) + 233.89117 * Exp(-32.770339 * r^2))
!
! Therefore, the table of coefficients in the format ready for
! use in this module is
!
! l   Cav               Cso       n         Alpha
! ----------------------------------------------------
! s   355.750512                  2         30.110543
! p   77.9699313   -155.939863    2         32.692614
! p   155.927447    155.927447    2         32.770339
!
! These numerical values can be now used directly in subroutines for the scalar and
! spin-orbit parts of the PP. 
!
! REFERENCES
!
! 1. Dolg, M. and Cao, X. Chem. Rev. 112, 403 (2012); doi: 10.1021/cr2001383
! 2. LaJohn, L.A., Christiansen, P.A., Ross, R.B., Atashroo, T., and Ermler, W.
!    J. Chem. Phys. 87, 2812 (1987); doi: 10.1063/1.453069
! 3. Pitzer, R. and Winter, N.W. J. Phys. Chem. 92, 3061 (1988); doi: 10.1021/j100322a011
! 4. Figgen, D., Rauhut, G., Dolg, M., and Stoll, H. Chem. Phys. 311, 227 (2005);
!    doi: 10.1016/j.chemphys.2004.10.005
!
module spin_orbit_ecp
      use arithmetic
      use math_constants
      use ecpint

      implicit none

contains

      subroutine so_pseudopot(vx, vy, vz)
            !
            ! Compute the matrix elements of the operator
            !
            ! V = Sum_{l=1}^{LMax} Uso_l(r) Pl 1/i L Pl
            !
            ! The matrices Vx, Vy, and Vz correspond to the Cartesian components
            ! of the orbital angular momentum operator: Lx, Ly, and Lz. The matrix
            ! of the spin-orbit PP is obtained by multiplication of V with the spin
            ! angular momentum operator
            !
            ! Uso = i * (Vx, Vy, Vz) * (Sx, Sy, Sz)
            !
            ! The matrix elements <l,mm| L |l,m> are purely imaginary in the real spherical
            ! harmonics basis; consequently, all numerical integrals are real. The matrices
            ! Vx, Vy, and Vz are real and antisymmetric.
            !
            ! See Eqs. 42-43 in Ref. 3 and Eq. 38 in Ref. 4 for a demonstration of how Vx, Vy,
            ! and Vz are used to compute singlet-triplet spin-orbit couplings.
            !
            ! The main part of the integration scheme for Uso is given in Ref. 1.
            ! However, we use a more efficient and numerically stable numerical algorithm
            ! for the radial integrals which is described in Ref. 2. The use of Eq. 8 in Ref. 2
            ! allows us to treat all primitive Gaussian functions at once in the radial
            ! integral; this is not possible in the scheme of Ref. 1. To compute the
            ! integrals of Uso, we upgraded Eq. 11 of Ref. 2, but other formulas stay the same
            ! as for scalar PPs.
            ! 
            ! 1. Pitzer, R.M. and Winter, N.W. Int. J. Quant. Chem. 40, 773 (1991);
            !    doi: 10.1002/qua.560400606
            !
            ! 2. Flores-Moreno, R., Alvarez-Mendez, R.J., Vela, A., Koster, A.M. J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            !
            ! 3. Vahtras, O., Agren, H., Jorgensen, P., Jensen, H.J., Helgaker, T., and Olsen, J.
            !    J. Chem. Phys. 96, 2118 (1992); doi: 10.1063/1.462063
            !
            ! 4. Helmich-Paris, B., Hattig, C., and Wullen, C. J. Chem. Theory Comput. 12, 1892 (2016);
            !    doi: 10.1021/acs.jctc.5b01197
            !
            real(F64), dimension(:, :), intent(out) :: vx
            real(F64), dimension(:, :), intent(out) :: vy
            real(F64), dimension(:, :), intent(out) :: vz

            integer :: a, b, c, cc
            integer :: shella, shellb
            integer :: shellab, shellab_max
            type(tgtodef) :: phia, phib
            real(F64), dimension(MAX_NFUNC**2) :: gabx, gaby, gabz
            real(F64), dimension(:), allocatable :: tcc
            real(F64), dimension(:, :, :, :), allocatable :: tab
            real(F64), dimension(:, :, :), allocatable :: tac
            real(F64), dimension(:), allocatable :: slma
            real(F64), dimension(:), allocatable :: slmb
            real(F64), dimension(:), allocatable :: xyzwork

            vx = ZERO
            vy = ZERO
            vz = ZERO
            
            if (.not. ECP_SPIN_ORBIT .or. ECP_NATOM == 0) then
                  return
            end if
            shellab_max = ((NSHELL + 1) * NSHELL) / 2
            !$omp parallel &
            !$omp default(shared) &
            !$omp private(tcc, tab, tac, slma, slmb, xyzwork) &
            !$omp private(gabx, gaby, gabz, a, b, c, cc, shella, shellb) &
            !$omp private(phia, phib) &
            !$omp shared(vx, vy, vz)

            call so_allocate_scratch(tcc, tab, tac, slma, slmb, xyzwork)

            !$omp do schedule(guided)            
            do shellab = 1, shellab_max
                  !
                  ! Extract individual indices from the compound shell-pair index
                  !
                  call decode_pq(shellab, NSHELL, shella, shellb)
                  a = SHATOM(shella)
                  b = SHATOM(shellb)
                  call loadgto(phia, shella)
                  call loadgto(phib, shellb)
                  !
                  ! Loop over ECP centers
                  !
                  gabx = ZERO
                  gaby = ZERO
                  gabz = ZERO
                  do cc = 1, ECP_NATOM
                        c = ECP_ATOM(cc)
                        if ((a .ne. c) .and. (b .ne. c)) then
                              call so_ecpab(gabx, gaby, gabz, a, b, c, phia, phib, &
                                    slma, slmb, tab)
                        else if ((a .ne. c) .and. (b .eq. c)) then
                              call so_ecpac(gabx, gaby, gabz, a, c, phia, phib, slma, tac, xyzwork)
                        else if ((a .eq. c) .and. (b .ne. c)) then
                              call so_ecpbc(gabx, gaby, gabz, b, c, phia, phib, slma, tac, xyzwork)
                        else
                              !
                              ! a == c .and. b == c
                              !
                              call so_ecpcc(gabx, gaby, gabz, c, phia, phib, tcc, xyzwork) 
                        end if
                  end do
                  !
                  ! No two threads will update the matrices at the same time because
                  ! each parallel task corresponds to a distinct bra-ket shell pair
                  !
                  call vmat_update(vx, gabx, shella, shellb, phia, phib)
                  call vmat_update(vy, gaby, shella, shellb, phia, phib)
                  call vmat_update(vz, gabz, shella, shellb, phia, phib)
            end do
            !$omp end do nowait
            deallocate(tcc)
            deallocate(tab)
            deallocate(tac)
            deallocate(slma)
            deallocate(slmb)
            deallocate(xyzwork)
            !$omp end parallel
      end subroutine so_pseudopot


      subroutine so_allocate_scratch(tcc, tab, tac, slma, slmb, xyzwork)
            real(F64), dimension(:), allocatable, intent(out)          :: tcc
            real(F64), dimension(:, :, :, :), allocatable, intent(out) :: tab
            real(F64), dimension(:, :, :), allocatable, intent(out)    :: tac
            real(F64), dimension(:), allocatable, intent(out)          :: slma
            real(F64), dimension(:), allocatable, intent(out)          :: slmb
            real(F64), dimension(:), allocatable, intent(out)          :: xyzwork
            !
            ! The subroutines for the spin-orbit part of the PP do not employ
            ! the local and L=0 parts. Therefore, the required array size for TCC is
            ! ECP_TCC-2, where ECP_TCC is the size used for the scalar PP.
            ! Also, in the TAB and TAC arrays, the index corresponding to the projected
            ! angular momentum starts from L=1 instead of L=0.
            !
            allocate(tcc(ECP_TCC-2))
            allocate(tab(0:ECP_TAB(1), 0:ECP_TAB(2), 1:ECP_TAB(3), 0:ECP_TAB(4)))
            allocate(tac(0:ECP_TAC(1), 1:ECP_TAC(2), 0:ECP_TAC(3)))
            allocate(slma(ECP_SLMA))
            allocate(slmb(ECP_SLMB))
            allocate(xyzwork(ECP_XYZWORK))
      end subroutine so_allocate_scratch


      subroutine so_ecpab(gabx, gaby, gabz, a, b, c, phia, phib, slma, slmb, tab)
            !
            ! 1. Flores-Moreno, R., Alvarez-Mendez, R.J., Vela, A., Koster, A.M. J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            !
            real(F64), dimension(:), intent(inout) :: gabx
            real(F64), dimension(:), intent(inout) :: gaby
            real(F64), dimension(:), intent(inout) :: gabz
            integer, intent(in)                  :: a
            integer, intent(in)                  :: b
            integer, intent(in)                  :: c
            type(tgtodef), intent(in)            :: phia
            type(tgtodef), intent(in)            :: phib
            real(F64), dimension(:), intent(out) :: slma
            real(F64), dimension(:), intent(out) :: slmb
            real(F64), dimension(0:, 0:, 1:, 0:), intent(out) :: tab

            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: ax, ay, az
            real(F64) :: axn, ayn, azn
            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: bx, by, bz
            real(F64) :: bxn, byn, bzn
            real(F64), dimension(3) :: gamab
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

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(INUCLZ(c))
            nfunca = nfunc(la)
            nfuncb = nfunc(lb)
            nint = nfunca * nfuncb

            ax(0) = ONE
            ax(1) = ATOMR(1, a) - ATOMR(1, c)
            ax(2) = ax(1)**2

            ay(0) = ONE
            ay(1) = ATOMR(2, a) - ATOMR(2, c)
            ay(2) = ay(1)**2

            az(0) = ONE
            az(1) = ATOMR(3, a) - ATOMR(3, c)
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
            bx(1) = ATOMR(1, b) - ATOMR(1, c)
            bx(2) = bx(1)**2

            by(0) = ONE
            by(1) = ATOMR(2, b) - ATOMR(2, c)
            by(2) = by(1)**2

            bz(0) = ONE
            bz(1) = ATOMR(3, b) - ATOMR(3, c)
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
            ! Genarate real spherical harmonics for l <= lmax + la
            ! and all possible m values. The definition of RSHs
            ! is given in the comments for the RSH subroutine.
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
                        !
                        ! There are no local and S components of the spin-orbit pseudopotential;
                        ! therefore, the loop over l starts from 1 and the reading frame (k0)
                        ! starts at the coefficients and exponents for P functions.
                        !
                        k0 = ECP_K0(ecpcenter) + ECP_NGAUSS(1, ecpcenter) + ECP_NGAUSS(2, ecpcenter)
                        do l = 1, lmax
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
                                          call cheb_integrate(tab_element, CHEB_EPS, CHEB_NMAX, npoint, conv, &
                                                so_tintab, tp)
                                          TAB(lambda2, lambda1, l, n) = tab_element
                                    end do
                              end do
                              k0 = k0 + ECP_NGAUSS(2+l, ecpcenter)
                        end do
                  end do
            end do

            v = 1
            !
            ! The prefactor 16 Pi^2 comes from Eq. 8 in Ref. 1 applied
            ! two times: for bra and ket orbitals
            !
            prefac = SIXTEEN * PI**2
            do j = 1, nfuncb
                  lbx = ECP_LL(j, lb)
                  lby = ECP_MM(j, lb)
                  lbz = ECP_NN(j, lb)
                  do i = 1, nfunca
                        lax = ECP_LL(i, la)
                        lay = ECP_MM(i, la)
                        laz = ECP_NN(i, la)
                        call so_gamab(gamab, lax, lay, laz, lbx, lby, lbz, ax, ay, az, bx, by, bz, &
                              SLMA, SLMB, ecpcenter, tab)
                        gabx(v) = gabx(v) + prefac * gamab(1)
                        gaby(v) = gaby(v) + prefac * gamab(2)
                        gabz(v) = gabz(v) + prefac * gamab(3)
                        v = v + 1
                  end do
            end do
      end subroutine so_ecpab


      subroutine so_ecpac(gabx, gaby, gabz, a, c, phia, phib, slma, tac, xyzwork)
            !
            ! 1. Flores-Moreno, R., Alvarez-Mendez, R.J., Vela, A., Koster, A.M. J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            !
            real(F64), dimension(:), intent(inout) :: gabx
            real(F64), dimension(:), intent(inout) :: gaby
            real(F64), dimension(:), intent(inout) :: gabz
            integer, intent(in)                    :: a
            integer, intent(in)                    :: c
            type(tgtodef), intent(in)              :: phia
            type(tgtodef), intent(in)              :: phib
            real(F64), dimension(:), intent(out) :: slma
            real(F64), dimension(0:, 1:, 0:), intent(out) :: tac
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: ax, ay, az
            real(F64), dimension(3) :: gamac
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

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(INUCLZ(c))
            nfunca = nfunc(la)
            nfuncb = nfunc(lb)

            ax(0) = ONE
            ax(1) = ATOMR(1, a) - ATOMR(1, c)
            ax(2) = ax(1)**2

            ay(0) = ONE
            ay(1) = ATOMR(2, a) - ATOMR(2, c)
            ay(2) = ay(1)**2

            az(0) = ONE
            az(1) = ATOMR(3, a) - ATOMR(3, c)
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
            call rsh(SLMA, lmax+la, axn, ayn, azn)
            !
            ! Tabulate radial integrals. All Cartesian Gaussian functions
            ! in a shell share the same set of radial integrals. In other
            ! words, the radial integrals computed above can be re-used by
            ! orbitals that differ by in an angular function.
            !
            call so_preptac(tac, phia, phib, lena, ecpcenter, lmax)
            v = 1
            !
            ! The prefactor 4 Pi comes from Eq. 8 in Ref. 1 applied
            ! for PhiA
            !
            prefac = FOUR * PI
            do j = 1, nfuncb
                  lbx = ECP_LL(j, lb)
                  lby = ECP_MM(j, lb)
                  lbz = ECP_NN(j, lb)
                  do i = 1, nfunca
                        lax = ECP_LL(i, la)
                        lay = ECP_MM(i, la)
                        laz = ECP_NN(i, la)
                        call so_gamac(gamac, lax, lay, laz, lbx, lby, lbz, ax, ay, az, SLMA, ecpcenter, tac, xyzwork)
                        gabx(v) = gabx(v) + prefac * gamac(1)
                        gaby(v) = gaby(v) + prefac * gamac(2)
                        gabz(v) = gabz(v) + prefac * gamac(3)
                        v = v +  1
                  end do
            end do
      end subroutine so_ecpac


      subroutine so_ecpbc(gabx, gaby, gabz, b, c, phia, phib, slmb, tbc, xyzwork)
            real(F64), dimension(:), intent(inout)        :: gabx
            real(F64), dimension(:), intent(inout)        :: gaby
            real(F64), dimension(:), intent(inout)        :: gabz
            integer, intent(in)                           :: b
            integer, intent(in)                           :: c
            type(tgtodef), intent(in)                     :: phia
            type(tgtodef), intent(in)                     :: phib
            real(F64), dimension(:), intent(out)          :: slmb
            real(F64), dimension(0:, 1:, 0:), intent(out) :: tbc
            real(F64), dimension(:), intent(out)          :: xyzwork

            real(F64), dimension(0:max(2,ECP_GTO_MAXL)) :: bx, by, bz
            real(F64), dimension(3) :: gamac
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

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(INUCLZ(c))
            nfunca = nfunc(la)
            nfuncb = nfunc(lb)

            bx(0) = ONE
            bx(1) = ATOMR(1, b) - ATOMR(1, c)
            bx(2) = bx(1)**2

            by(0) = ONE
            by(1) = ATOMR(2, b) - ATOMR(2, c)
            by(2) = by(1)**2

            bz(0) = ONE
            bz(1) = ATOMR(3, b) - ATOMR(3, c)
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
            call rsh(SLMB, lmax+lb, bxn, byn, bzn)
            !
            ! Tabulate radial integrals. All Cartesian Gaussian functions
            ! in a shell share the same set of radial integrals. In other
            ! words, the radial integrals computed above can be re-used by
            ! orbitals that differ by in an angular function.
            !
            call so_preptac(tbc, phib, phia, lenb, ecpcenter, lmax)

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
                        call so_gamac(gamac, lbx, lby, lbz, lax, lay, laz, bx, by, bz, SLMB, ecpcenter, tbc, xyzwork)
                        !
                        ! Note the minus sign; we requested the transposed element
                        ! <PhiB| 1/i L | PhiA>, and the matrix representation
                        ! of 1/i L is antisymmetric.
                        !
                        gabx(v) = gabx(v) - prefac * gamac(1)
                        gaby(v) = gaby(v) - prefac * gamac(2)
                        gabz(v) = gabz(v) - prefac * gamac(3)
                        v = v + 1
                  end do
            end do
      end subroutine so_ecpbc
      

      subroutine so_preptac(tac, phia, phib, lena, ecpcenter, lmax)
            real(F64), dimension(0:, 1:, 0:), intent(out) :: tac
            type(tgtodef), intent(in)                     :: phia
            type(tgtodef), intent(in)                     :: phib 
            real(F64), intent(in)                         :: lena
            integer, intent(in)                           :: ecpcenter
            integer, intent(in)                           :: lmax

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
                  k0 = ECP_K0(ecpcenter) + ECP_NGAUSS(1, ecpcenter) + ECP_NGAUSS(2, ecpcenter)
                  do l = 1, lmax
                        tp%l = l
                        tp%k0 = k0
                        lambda10 = max(l-alpha, 0)
                        lambda11 = l + alpha
                        incl1 = modulo(lambda11-lambda10, 2)
                        do lambda1 = lambda10+incl1, lambda11, 2
                              tp%lambdaa = lambda1
                              call cheb_integrate(tac_element, CHEB_EPS, CHEB_NMAX, npoint, conv, &
                                    so_tintac, tp)
                              tac(lambda1, l, alpha) = tac_element
                        end do
                        k0 = k0 + ECP_NGAUSS(2+l, ecpcenter)
                  end do
            end do
      end subroutine so_preptac
      
      
      subroutine so_ecpcc(gabx, gaby, gabz, c, phia, phib, tcc, xyzwork)
            !
            ! 1. Flores-Moreno, R., Alvarez-Mendez, R.J., Vela, A., Koster, A.M. J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            !
            real(F64), dimension(:), intent(inout) :: gabx
            real(F64), dimension(:), intent(inout) :: gaby
            real(F64), dimension(:), intent(inout) :: gabz
            integer, intent(in)                    :: c
            type(tgtodef), intent(in)              :: phia
            type(tgtodef), intent(in)              :: phib
            real(F64), dimension(:), intent(out)   :: tcc
            real(F64), dimension(:), intent(out)   :: xyzwork
  
            integer :: lax, lay, laz
            integer :: lbx, lby, lbz
            integer :: la, lb
            integer :: nfunca, nfuncb
            integer :: i, j
            integer :: v
            integer :: ecpcenter
            real(F64), dimension(3) :: gamcc
            integer :: lmax

            la = phia%l
            lb = phib%l

            ecpcenter = ECP_IELEMENT(INUCLZ(c))
            nfunca = nfunc(la)
            nfuncb = nfunc(lb)
            lmax = ECP_LMAX(ecpcenter)

            call so_preptcc(tcc, phia, phib, ecpcenter, lmax)

            v = 1
            do j = 1, nfuncb
                  lbx = ECP_LL(j, lb)
                  lby = ECP_MM(j, lb)
                  lbz = ECP_NN(j, lb)
                  do i = 1, nfunca
                        lax = ECP_LL(i, la)
                        lay = ECP_MM(i, la)
                        laz = ECP_NN(i, la)
                        call so_gamcc(gamcc, lax, lay, laz, lbx, lby, lbz, ecpcenter, tcc, xyzwork)
                        !
                        ! No prefactor of 4Pi is needed because Eq. 8 of Ref. 1
                        ! is not used when PhiA and PhiB are both centered on C.
                        !
                        gabx(v) = gabx(v) + gamcc(1)
                        gaby(v) = gaby(v) + gamcc(2)
                        gabz(v) = gabz(v) + gamcc(3)
                        v = v + 1
                  end do
            end do
      end subroutine so_ecpcc


      subroutine so_preptcc(tcc, phia, phib, ecpcenter, lmax)
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
            k0 = ECP_K0(ecpcenter) + ECP_NGAUSS(1, ecpcenter) + ECP_NGAUSS(2, ecpcenter)
            do l = 1, lmax
                  tp%l = l
                  tp%k0 = k0
                  tcc(l) = so_ccint(tp)
                  k0 = k0 + ECP_NGAUSS(2+l, ecpcenter)
            end do
      end subroutine so_preptcc


      pure function so_ccint(tp)
            real(F64) :: so_ccint
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
                        ckl = ECP_SO_COEFF(k)
                        n = nkl + phia%l + phib%l 
                        do r = 1, phia%nprm
                              cr = phia%cntr(r)
                              ar = phia%expn(r)
                              do s = 1, phib%nprm
                                    cs = phib%cntr(s)
                                    as = phib%expn(s)
                                    a = akl + ar + as
                                    c = ckl * cr * cs
                                    v = v + c * gaussint(n, a)
                              end do
                        end do
                  end do
                  so_ccint = v
            end associate
      end function so_ccint

      
      subroutine so_gamab(gamab, lax, lay, laz, lbx, lby, lbz, ax, ay, az, &
            bx, by, bz, wslma, wslmb, ecpcenter, tab)
            
            real(F64), dimension(3), intent(out) :: gamab
            integer, intent(in)                  :: lax, lay, laz
            integer, intent(in)                  :: lbx, lby, lbz
            real(F64), dimension(0:), intent(in) :: ax, ay, az
            real(F64), dimension(0:), intent(in) :: bx, by, bz
            real(F64), dimension(:), intent(in)  :: wslma
            real(F64), dimension(:), intent(in)  :: wslmb
            integer, intent(in)                  :: ecpcenter
            real(F64), dimension(0:, 0:, 1:, 0:), intent(in) :: tab

            real(F64) :: axayaz
            real(F64) :: bxbybz
            real(F64) :: binomax, binomay, binomaz
            real(F64) :: binombx, binomby, binombz
            real(F64) :: phase
            real(F64), dimension(3) :: gamabl
            integer :: alpx, alpy, alpz, alpha
            integer :: betx, bety, betz, beta
            integer :: la, lb
            integer :: l

            la = lax + lay + laz
            lb = lbx + lby + lbz

            gamab = ZERO
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
                                                !
                                                ! Note that we compute only the spin-orbit contribution
                                                ! to the pseudopotential, so we start at P angular functions, i.e., l=1
                                                !
                                                do l = 1, ECP_LMAX(ecpcenter)
                                                      call so_gamabl(gamabl, l, alpx, alpy, alpz, betx, &
                                                            bety, betz, wslma, wslmb, tab)
                                                      gamab = gamab + phase * axayaz * bxbybz * gamabl
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine so_gamab


      subroutine so_gamcc(gamcc, lax, lay, laz, lbx, lby, lbz, ecpcenter, tcc, xyzwork)
            real(F64), dimension(3), intent(out) :: gamcc
            integer, intent(in) :: lax, lay, laz
            integer, intent(in) :: lbx, lby, lbz
            integer, intent(in) :: ecpcenter
            real(F64), dimension(:), intent(in) :: tcc
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64), dimension(3) :: gamccl
            integer :: l

            gamcc = ZERO
            !
            ! Note that we compute only the spin-orbit contribution
            ! to the pseudopotential, so we start at P angular functions, i.e., l=1
            !
            do l = 1, ECP_LMAX(ecpcenter)
                  call so_gamccl(gamccl, l, lax, lay, laz, lbx, lby, lbz, tcc, xyzwork)
                  gamcc = gamcc + gamccl
            end do
      end subroutine so_gamcc


      subroutine so_gamac(gamac, lax, lay, laz, lbx, lby, lbz, ax, ay, az, &
            wslma, ecpcenter, tac, xyzwork)
            real(F64), dimension(3), intent(out) :: gamac
            integer, intent(in) :: lax, lay, laz
            integer, intent(in) :: lbx, lby, lbz
            real(F64), dimension(0:), intent(in) :: ax, ay, az
            real(F64), dimension(:), intent(in)  :: wslma
            integer, intent(in) :: ecpcenter
            real(F64), dimension(0:, 1:, 0:), intent(in) :: tac
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64) :: axayaz
            real(F64) :: binomax, binomay, binomaz
            real(F64) :: phase
            real(F64), dimension(3) :: gamacl
            integer :: alpx, alpy, alpz, alpha
            integer :: la
            integer :: l

            la = lax + lay + laz
            gamac = ZERO
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
                              !
                              ! Note that we compute only the spin-orbit contribution
                              ! to the pseudopotential, so we start at P angular functions, i.e., l=1
                              !
                              do l = 1, ECP_LMAX(ecpcenter)
                                    call so_gamacl(gamacl, l, alpx, alpy, alpz, &
                                          lbx, lby, lbz, wslma, tac, xyzwork)
                                    gamac = gamac + phase * axayaz * gamacl
                              end do
                        end do
                  end do
            end do
      end subroutine so_gamac
      

      subroutine so_gamabl(gamabl, l, alpx, alpy, alpz, betx, bety, betz, wslma, wslmb, tab)
            !
            ! Integrate the spin-orbit part of the pseudopotential:
            ! GammaAB <- \int_0^Inf r^2 Ul(r) \sum_(mm,m) \int PhiA Sl^(mm) dOmega \int PhiB Sl^m dOmega <mm| 1/i L |m>
            !
            ! To understand the formula implemented here, see the formula for Type-2 spin-orbit integrals
            ! in Ref. 2 (vAB). The actual formula derived in Ref. 2 is suboptimal; for example, it does not
            ! fold all contracted Gaussians in a shell into a single radial integral. Thus, instead of applying
            ! Ref. 2 directly, I modified Eq. 11 of Flores-Moreno et al. The only change is to take non-diagonal
            ! angular integrals Omega(A, mm) and Omega(B, m) and carry out the double summation over mm and m.
            ! 
            ! 1. Flores-Moreno, R., Alvarez-Mendez, R.J., Vela, A., Koster, A.M. J. Comput. Chem. 27, 1009 (2006);
            !    doi: 10.1002/jcc.20410
            ! 2. Pitzer, R.M. and Winter, N.W. Int. J. Quant. Chem. 40, 773 (1991);
            !    doi: 10.1002/qua.560400606
            !
            real(F64), dimension(3), intent(out) :: gamabl
            integer, intent(in)                  :: l
            integer, intent(in)                  :: alpx
            integer, intent(in)                  :: alpy
            integer, intent(in)                  :: alpz
            integer, intent(in)                  :: betx
            integer, intent(in)                  :: bety
            integer, intent(in)                  :: betz
            real(F64), dimension(:), intent(in)  :: wslma
            real(F64), dimension(:), intent(in)  :: wslmb
            real(F64), dimension(0:, 0:, 1:, 0:), intent(in) :: tab

            real(F64) :: omegaa, omegab, slma, slmb
            real(F64) :: tab_element
            integer :: lambda1, lambda2, mu1, mu2, m, mm
            integer :: lambda10, lambda11, lambda20, lambda21
            integer :: alpha, beta, n
            integer :: incl1, incl2
            real(F64), dimension((l+alpx+alpy+alpz)/2+1, -l:l) :: omegaa_slma
            real(F64), dimension((l+betx+bety+betz)/2+1, -l:l) :: omegab_slmb
            integer :: v1, v2

            alpha = alpx + alpy + alpz
            beta = betx + bety + betz
            n = alpha + beta
            lambda10 = max(l-alpha, 0)
            lambda11 = l + alpha
            incl1 = modulo(lambda11-lambda10, 2)
            lambda10 = lambda10 + incl1
            lambda20 = max(l-beta, 0)
            lambda21 = l + beta
            incl2 = modulo(lambda21-lambda20, 2)
            lambda20 = lambda20 + incl2
            gamabl = ZERO
            do m = -l, l
                  !
                  ! Carry out summations over mu1 and mu2, which can be done before
                  ! the summations over lambda1 and lambda2.
                  !
                  do lambda1 = lambda10, lambda11, 2
                        v1 = (lambda1 - lambda10) / 2 + 1
                        omegaa_slma(v1, m) = ZERO
                        do mu1 = -lambda1, lambda1
                              omegaa = readomega(lambda1, mu1, l, m, alpx, alpy, alpz)
                              slma = wslma(slmpos(lambda1, mu1))
                              omegaa_slma(v1, m) = omegaa_slma(v1, m) + omegaa * slma
                        end do
                  end do

                  do lambda2 = lambda20, lambda21, 2
                        v2 = (lambda2 - lambda20) / 2 + 1
                        omegab_slmb(v2, m) = ZERO
                        do mu2 = -lambda2, lambda2
                              omegab = readomega(lambda2, mu2, l, m, betx, bety, betz)
                              slmb = wslmb(slmpos(lambda2, mu2))
                              omegab_slmb(v2, m) = omegab_slmb(v2, m) + omegab * slmb
                        end do
                  end do
            end do
            !
            ! Avoid unrestricted summation over mm and m by using
            ! the antisymmetry of <mm|1/i L |m>:
            ! omega(A, mm)*omega(B, m) * <mm|1/i L|m> +
            ! omega(A, m)*omega(B, mm) * <m|1/i L|mm> =
            ! (omega(A, mm)*omega(B, m)-omega(A,m)*omega(B,mm))*<mm|1/i L|m>.
            ! Thus, the loop runs only over the unique pairs mm>m.
            !
            do mm = -l, l
                  do m = -l, mm - 1
                        do lambda1 = lambda10, lambda11, 2
                              do lambda2 = lambda20, lambda21, 2
                                    v1 = (lambda1 - lambda10) / 2 + 1
                                    v2 = (lambda2 - lambda20) / 2 + 1
                                    tab_element = TAB(lambda2, lambda1, l, n)
                                    gamabl = gamabl + tab_element * (omegaa_slma(v1, mm) * omegab_slmb(v2, m) &
                                          - omegaa_slma(v1, m) * omegab_slmb(v2, mm)) * ECP_LVECTOR(:, mm, m, l)
                              end do
                        end do
                  end do
            end do
      end subroutine so_gamabl


      subroutine so_gamacl(gamacl, l, alpx, alpy, alpz, lbx, lby, lbz, wslma, tac, xyzwork)
            !
            ! Integrate the spin-orbit part of the pseudopotential. This variant of the subroutine
            ! works for A =/ B = C, where A, B are the centers of PhiA and PhiB;
            ! the pseudopotential is centered on C.
            !
            ! See the comments for SO_GAMABL for more details.
            !
            real(F64), dimension(3), intent(out) :: gamacl
            integer, intent(in)                  :: l
            integer, intent(in)                  :: alpx
            integer, intent(in)                  :: alpy
            integer, intent(in)                  :: alpz
            integer, intent(in)                  :: lbx
            integer, intent(in)                  :: lby
            integer, intent(in)                  :: lbz
            real(F64), dimension(:), intent(in)  :: wslma
            real(F64), dimension(0:, 1:, 0:), intent(in) :: tac
            real(F64), dimension(:), intent(out) :: xyzwork

            real(F64), dimension((l+alpx+alpy+alpz)/2+1, -l:l) :: omegaa_slma
            real(F64) :: omegaa, slma
            real(F64) :: tac_element, vb_m, vb_mm
            integer :: lambda1, mu1, m, mm, v1
            integer :: lambda10, lambda11
            integer :: alpha
            integer :: incl1

            alpha = alpx + alpy + alpz
            lambda10 = max(l-alpha, 0)
            lambda11 = l + alpha
            incl1 = modulo(lambda11-lambda10, 2)
            lambda10 = lambda10 + incl1
            gamacl = ZERO
            do m = -l, l
                  do lambda1 = lambda10, lambda11, 2
                        v1 = (lambda1 - lambda10) / 2 + 1
                        omegaa_slma(v1, m) = ZERO
                        do mu1 = -lambda1, lambda1
                              omegaa = readomega(lambda1, mu1, l, m, alpx, alpy, alpz)
                              slma = wslma(slmpos(lambda1, mu1))
                              omegaa_slma(v1, m) = omegaa_slma(v1, m) + omegaa * slma
                        end do
                  end do
            end do
                  
            do mm = -l, l
                  do m = -l, mm - 1
                        do lambda1 = lambda10, lambda11, 2
                              tac_element = tac(lambda1, l, alpha)
                              vb_m = rshv(l, m, lbx, lby, lbz, ECP_U, XYZWORK)
                              vb_mm = rshv(l, mm, lbx, lby, lbz, ECP_U, XYZWORK)
                              v1 = (lambda1 - lambda10) / 2 + 1
                              gamacl = gamacl + tac_element * (omegaa_slma(v1, mm) * vb_m - omegaa_slma(v1, m) * vb_mm) &
                                    * ECP_LVECTOR(:, mm, m, l)
                        end do
                  end do
            end do
      end subroutine so_gamacl


      subroutine so_gamccl(gamccl, l, lax, lay, laz, lbx, lby, lbz, tcc, xyzwork)
            !
            ! Integrate the spin-orbit part of the pseudopotential. This variant of the subroutine
            ! works for A = B = C, where A, B are the centers of PhiA and PhiB;
            ! the pseudopotential is centered on C.
            !
            ! See the comments for SO_GAMABL for more details.
            !
            real(F64), dimension(3), intent(out) :: gamccl
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
            real(F64) :: va_m, va_mm, vb_m, vb_mm
            integer :: m, mm

            tcc_element = tcc(l) 
            gamccl = ZERO
            do mm = -l, l
                  do m = -l, mm - 1
                        va_m = rshv(l, m, lax, lay, laz, ECP_U, XYZWORK)
                        va_mm = rshv(l, mm, lax, lay, laz, ECP_U, XYZWORK)
                        vb_m = rshv(l, m, lbx, lby, lbz, ECP_U, XYZWORK)
                        vb_mm = rshv(l, mm, lbx, lby, lbz, ECP_U, XYZWORK)
                        gamccl = gamccl + tcc_element * (va_mm * vb_m - va_m * vb_mm) * ECP_LVECTOR(:, mm, m, l)
                  end do
            end do
      end subroutine so_gamccl


      pure function so_ulpot(r, l, ecpcenter, k0)
            ! ----------------------------------------------------
            ! Compute the radial part of the spin-orbit
            ! pseudopotential Ul(R)
            ! ----------------------------------------------------
            ! R
            !       Input, the radius at which potential
            !       is evalueated
            ! L
            !       Angular momentum (L>=1) 
            ! ECPCENTER
            !       Index of the ECP center
            ! K0
            !       Index of the first coefficient belonging to
            !       U_L in ECP_SO_COEFF array
            !
            real(F64)                            :: so_ulpot
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
            so_ulpot = ZERO
            do k = k0, k0 + ngauss - 1
                  alpha = ECP_EXPN(k)
                  c = ECP_SO_COEFF(k)
                  nkl = ECP_NKL(k)
                  e = exp(-alpha * rn(2))
                  so_ulpot = so_ulpot + c * rn(nkl) * e
            end do
      end function so_ulpot


      pure function so_tintab(r, tp)
            real(F64)              :: so_tintab
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: fa, fb, ul

            associate (phia => tp%phia, phib => tp%phib, &
                  lambdaa => tp%lambdaa, lambdab => tp%lambdab, l => tp%l, &
                  ecpcenter => tp%ecpcenter, lena => tp%lena, lenb => tp%lenb, &
                  k0 => tp%k0, alpha => tp%alpha, beta => tp%beta)
                  
                  ul = so_ulpot(r, l, ecpcenter, k0)
                  fa = falambda(r, lambdaa, phia, lena, alpha)
                  fb = falambda(r, lambdab, phib, lenb, beta)
                  so_tintab = fa * fb * ul
            end associate
      end function so_tintab


      pure function so_tintac(r, tp)
            real(F64)              :: so_tintac
            real(F64), intent(in)  :: r
            type(TINT_PARAMS), intent(in) :: tp

            real(F64) :: fa, phibval, ul
            
            associate (phia => tp%phia, phib => tp%phib, &
                  lambdaa => tp%lambdaa, l => tp%l, &
                  ecpcenter => tp%ecpcenter, lena => tp%lena, &
                  k0 => tp%k0, alpha => tp%alpha)
                  
                  ul = so_ulpot(r, l, ecpcenter, k0)
                  fa = falambda(r, lambdaa, phia, lena, alpha)
                  phibval = phir(r, phib)
                  so_tintac = fa * phibval * ul
            end associate
      end function so_tintac
end module spin_orbit_ecp
