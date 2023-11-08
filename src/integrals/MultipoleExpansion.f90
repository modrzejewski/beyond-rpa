! ---------------------------------------------------------
! Computation of the Lth-order multipole interaction matrix
! for the Erf(omega r12)/r12 operator
! ---------------------------------------------------------
! 
! The main method for the computation of the matrix elements Tpq
! is the MULT_COEFF_GAUSS1 method, which uses a least-squares fit
! to the exact potential and a 1s Gaussian weight
! function exp(-alpha/2 s**2). The remaining computational routes,
! i.e., MULT_COEFF_GAUSS2, MULT_COEFF_WERNER, and MULT_COEFF_TAYLOR
! use other weight functions/Taylor expansion and are included
! only for testing.
!
! The numerical stability of the MULT_COEFF_GAUSS1 computational route
! is tested for L <= 20.
!
! The least-squares fitting subroutine for R>0 involves
! two-dimensional numerical integrals (radial: Gauss-Hermite,
! spherical: Gauss-Legendre). For the set of parameters
! NRadialQuad=40, NSpherQuad=20, L=20, R=1.0 a.u.,
! the maximum relative error on the full set of 121 numerical
! integrals is RelError=2.0E-06.
!
! The weight function parameter should be re-optimized if the user assumes
! a new range of effective exponents or changes the maximum angular momentum.
!
! -----------------------------------
! Raw output from the weight-function
! optimization subroutine
! -----------------------------------
!
! Starting search for the optimal weight function
! Weight type: exp(-alpha/2 s**2)
! Distance between orbital centers: 2.000E+000
! Bra angular momentum: 5
! Ket angular momentum: 5
! Smallest exponent: 2.9999999999999999E-001
! Largest exponent: 1.0000000000000000E+001
! Number of exponents: 100
! Using Cartesian multipoles
! Lmax = 20
! #    alpha          RMSE           
! 0    1.500E-001     1.528E-004     
! 1    2.394E-001     1.080E-005     
! 2    6.247E-001     5.601E-004     
! 3    2.842E-001     3.451E-006     
! 4    4.240E-001     2.998E-006     
! 5    3.571E-001     6.508E-007     
! 6    3.571E-001     6.516E-007     
! 7    3.827E-001     4.667E-007     
! 8    3.734E-001     4.791E-007     
! 9    3.985E-001     8.173E-007     
! 10   3.788E-001     4.590E-007     
! 11   3.791E-001     4.588E-007     
! 12   3.800E-001     4.590E-007     
! 13   3.794E-001     4.588E-007     
! 14   3.794E-001     4.588E-007     
! 15   3.796E-001     4.588E-007     
! 16   3.795E-001     4.588E-007     
! 17   3.795E-001     4.588E-007     
! Converged solution
! alpha = 3.795E-001
! RMSE = 4.588E-007
! Uncertainty in alpha = 9.720E-005
!
module MultipoleExpansion
      use arithmetic
      use math_constants
      use spherh
      use string
      use uniopt
      use display
      use quadratures
      use real_linalg
      use auto2e
      
      implicit none
      !
      ! Methods for the computation of the multipole interaction matrix
      !
      integer, parameter :: MULT_COEFF_TAYLOR = 1
      integer, parameter :: MULT_COEFF_WERNER = 2
      integer, parameter :: MULT_COEFF_GAUSS1 = 3
      integer, parameter :: MULT_COEFF_GAUSS2 = 4
      !
      ! Number of points for radial quadrature.
      ! Corresponds to the Gauss-Hermite quadrature
      ! of order n=2*NRadialQuad
      !
      integer, parameter :: NRadialQuad = 40
      !
      ! Number of points for angular quadrature. Corresponds to the Gauss-Legendre
      ! quadrature of order n=2*NSpherQuad
      !
      integer, parameter :: NSpherQuad = 20
      !
      ! GammaHalfK(k) = Gamma(1/2+k) for k = 0, 1, 2, ..., 40
      !
      real(F64), dimension(0:40), parameter :: GammaHalfK = [ &
1.7724538509055160272981674833411_F64, &
0.88622692545275801364908374167057_F64, &
1.3293403881791370204736256125059_F64, &
3.3233509704478425511840640312646_F64, &
11.631728396567448929144224109426_F64, &
52.342777784553520181149008492418_F64, &
287.8852778150443609963195467083_F64, &
1871.254305797788346476077053604_F64, &
14034.40729348341259857057790203_F64, &
119292.46199460900708784991216725_F64, &
1.1332783889487855673345741655889e6_F64, &
1.1899423083962248457013028738683e7_F64, &
1.3684336546556585725564983049486e8_F64, &
1.7105420683195732156956228811857e9_F64, &
2.3092317922314238411890908896007e10_F64, &
3.3483860987355645697241817899211e11_F64, &
5.1899984530401250830724817743777e12_F64, &
8.5634974475162063870695949277232e13_F64, &
1.4986120533153361177371791123516e15_F64, &
2.7724322986333718178137813578504e16_F64, &
5.4062429823350750447368736478082e17_F64, &
1.1082798113786903841710590978007e19_F64, &
2.3828015944641843259677770602715e20_F64, &
5.3613035875444147334274983856108e21_F64, &
1.2599063430729374623554621206185e23_F64, &
3.0867705405286967827708821955154e24_F64, &
7.8712648783481767960657495985643e25_F64, &
2.0858851927622668509574236436195e27_F64, &
5.7361842800962338401329150199538e28_F64, &
1.6348125198274266444378807806868e30_F64, &
4.8226969334909086010917483030261e31_F64, &
1.470922564714727123332983232423e33_F64, &
4.6334060788513904384988971821324e34_F64, &
1.505856975626701892512141584193e36_F64, &
5.0446208683494513399156743070466e37_F64, &
1.7403941995805607122709076359311e39_F64, &
6.1783994085109905285617221075553e40_F64, &
2.2551157841065115429250285692577e42_F64, &
8.4566841903994182859688571347163e43_F64, &
3.2558234133037760400980099968658e45_F64, &
1.286050248254991535838713948762e47_F64 &
]
            
contains

      subroutine MultipoleIntMatrix(T, R, Lmax, LmaxSpher, Ltransf, omega, alpha, WeightFunc)
            real(F64), dimension(:, :), intent(out) :: T
            real(F64), dimension(:, :), intent(in)  :: R
            integer, intent(in)                     :: Lmax
            integer, intent(in)                     :: LmaxSpher
            integer, intent(in)                     :: Ltransf
            real(F64), intent(in)                   :: omega
            real(F64), intent(in)                   :: alpha
            integer, intent(in)                     :: WeightFunc
            
            integer :: Natoms
            integer :: NFittedCoeffs
            real(F64), dimension(:, :), allocatable :: Tlmk, Txyz
            integer, dimension(-1:Lmax) :: Plmk, Nlmk, Pxyz, Pc
            real(F64), dimension(NRadialQuad) :: s_points, s_weights
            real(F64), dimension(NSpherQuad) :: t_points, t_weights
            logical :: converged
            real(F64), dimension(:, :, :), allocatable :: Euvw
            real(F64), dimension(:, :), allocatable :: DmnMatrixForm
            real(F64), dimension(:, :), allocatable :: AfitAB, AfitAA
            real(F64), dimension(:), allocatable :: DmnVectorForm, D0n
            real(F64), dimension(:, :), allocatable :: Wxyz
            real(F64), dimension(:), allocatable :: CspherM, CspherT
            real(F64), dimension(3) :: DeltaR
            integer :: m, n
            integer :: AtomA, AtomB
            integer :: i0, i1, j0, j1
            real(F64), dimension(0:Lmax, 0:Lmax) :: binom

            call BinomTable(binom, Lmax)
            if (WeightFunc == MULT_COEFF_WERNER) then
                  call quad_GaussLaguerre(s_points, s_weights, converged, NRadialQuad, 1.0E-14_F64)
            else
                  call quad_GaussHermite(s_points, s_weights, converged, 2*NRadialQuad, 1.0E-14_F64)
            end if
            call quad_GaussLegendre(t_points, t_weights, converged, 2*NSpherQuad, 1.0E-14_F64)
            Natoms = size(R, dim=2)
            NFittedCoeffs = 0
            do m = 0, Lmax
                  do n = 0, (Lmax-m)/2
                        NFittedCoeffs = NFittedCoeffs + 1
                  end do
            end do
            allocate(AfitAB(0:NFittedCoeffs-1, 0:NFittedCoeffs-1))
            allocate(DmnVectorForm(0:NFittedCoeffs-1))
            allocate(Euvw(0:Lmax, 0:Lmax, 0:Lmax))
            allocate(DmnMatrixForm(0:Lmax, 0:Lmax/2))
            allocate(AfitAA(0:Lmax/2, 0:Lmax/2))
            allocate(D0n(0:Lmax/2))

            call NSpherMulti_P(Plmk, Lmax, LmaxSpher)
            call NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            call NCartMulti_P(Pxyz, Lmax)
            call NSpherCoeffs_P(Pc, Lmax, LmaxSpher)
            allocate(CspherT(Pc(Lmax)))
            allocate(CspherM(Pc(Lmax)))
            call SpherCoeffs(CspherT, CspherM, Lmax, LmaxSpher)

            allocate(Txyz(Pxyz(Lmax), Pxyz(Lmax)))
            allocate(Wxyz(Pxyz(Lmax), Plmk(Lmax)))
            allocate(Tlmk(Plmk(Lmax), Plmk(Lmax)))
            
            do AtomB = 1, Natoms
                  do AtomA = 1, Natoms
                        if (AtomA .ne. AtomB) then
                              DeltaR = R(:, AtomB) - R(:, AtomA)
                              call T_AB_Fit(Txyz, DeltaR, Lmax, omega, alpha, WeightFunc, s_points, s_weights, &
                                    t_points, t_weights, DmnVectorForm, DmnMatrixForm, AfitAB, Euvw, binom)
                        else
                              call T_AA_Fit(Txyz, Lmax, omega, alpha, WeightFunc, s_points, s_weights, D0n, &
                                    AfitAA, Euvw, binom)
                        end if
                        call transf_Tlmk_Txyz(Tlmk, Txyz, CspherT, Lmax, Ltransf, Wxyz, &
                              Plmk, Nlmk, Pxyz, Pc)
                        i0 = Plmk(Lmax) * (AtomA-1) + 1
                        i1 = Plmk(Lmax) * AtomA
                        j0 = Plmk(Lmax) * (AtomB-1) + 1
                        j1 = Plmk(Lmax) * AtomB
                        T(i0:i1, j0:j1) = Tlmk
                  end do
            end do
      end subroutine MultipoleIntMatrix


      subroutine T_AB_Taylor(T, R, L, omega)
            real(F64), dimension(:, :), intent(out) :: T
            real(F64), dimension(3), intent(in)     :: R
            integer, intent(in)                     :: L
            real(F64), intent(in)                   :: omega
            
            real(F64), dimension(:), allocatable :: A, C
            integer :: n, Lp, Lq, p, q, s, OffsetP, OffsetQ, OffsetS
            integer :: uq, vq, wq, up, vp, wp
            integer :: J

            n = NCartMulti(L)
            allocate(A(n))
            allocate(C(n))
            call VeeLRDeriv(A, C, R, omega, L)
            T(1:n, 1:n) = ZERO
            do J = 0, L
                  do Lq = 0, J
                        do Lp = 0, J - Lq
                              do uq = 0, Lq
                                    do vq = 0, Lq - uq
                                          do up = 0, Lp
                                                do vp = 0, Lp - up
                                                      OffsetP = NCartMulti(Lp-1)
                                                      OffsetQ = NCartMulti(Lq-1)
                                                      OffsetS = NCartMulti(Lp+Lq-1)
                                                      wp = Lp - up - vp
                                                      wq = Lq - uq - vq
                                                      p = OffsetP + lxlylzpos(up, vp, wp)
                                                      q = OffsetQ + lxlylzpos(uq, vq, wq)
                                                      s = OffsetS + lxlylzpos(up+uq, vp+vq, wp+wq)
                                                      T(p, q) = (-1)**Lp * A(s) / (fact(up)*fact(vp)*fact(wp) &
                                                            *fact(uq)*fact(vq)*fact(wq))
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine T_AB_Taylor
      

      subroutine T_AB_Fit(T, R, L, omega, alpha, WeightFunc, s_points, s_weights, &
            t_points, t_weights, DmnVectorForm, DmnMatrixForm, A, Euvw, binom)
            real(F64), dimension(:, :), intent(out)       :: T
            real(F64), dimension(3), intent(in)           :: R
            integer, intent(in)                           :: L
            real(F64), intent(in)                         :: omega
            real(F64), intent(in)                         :: alpha
            integer, intent(in)                           :: WeightFunc
            real(F64), dimension(NRadialQuad), intent(in) :: s_points, s_weights
            real(F64), dimension(NSpherQuad), intent(in)  :: t_points, t_weights
            real(F64), dimension(:), intent(out)          :: DmnVectorForm
            real(F64), dimension(0:, 0:), intent(out)     :: DmnMatrixForm
            real(F64), dimension(:, :), intent(out)       :: A
            real(F64), dimension(0:, 0:, 0:), intent(out) :: Euvw
            real(F64), dimension(0:, 0:), intent(in)      :: binom

            real(F64) :: RLength
            integer :: m, n, p

            RLength = norm2(R)
            call DmnCoeffs_AB_Fit(DmnVectorForm, A, L, omega, RLength, alpha, t_points, &
                  t_weights, s_points, s_weights, WeightFunc)
            DmnMatrixForm = ZERO
            p = 1
            do m = 0, L
                  do n = 0, (L-m)/2
                        DmnMatrixForm(m, n) = DmnVectorForm(p)
                        p = p + 1
                  end do
            end do
            call EuvwCoeffs(Euvw, L, DmnMatrixForm, R(1), R(2), R(3))
            call TpqCoeffs(T, L, Euvw, binom)
      end subroutine T_AB_Fit
      

      subroutine T_AA_Fit(T, L, omega, alpha, WeightFunc, s_points, s_weights, D0n, A, Euvw, binom)
            real(F64), dimension(:, :), intent(out)       :: T
            integer, intent(in)                           :: L
            real(F64), intent(in)                         :: omega
            real(F64), intent(in)                         :: alpha
            integer, intent(in)                           :: WeightFunc
            real(F64), dimension(NRadialQuad), intent(in) :: s_points, s_weights
            real(F64), dimension(0:), intent(out)         :: D0n
            real(F64), dimension(0:, 0:), intent(out)     :: A
            real(F64), dimension(0:, 0:, 0:), intent(out) :: Euvw
            real(F64), dimension(0:, 0:), intent(in)        :: binom

            call DmnCoeffs_AA_Fit(D0n, A, L, omega, alpha, s_points, s_weights, WeightFunc)
            call EuvwCoeffsDiag(Euvw, L, D0n)
            call TpqCoeffs(T, L, Euvw, binom)
      end subroutine T_AA_Fit


      subroutine OptimizeAlpha(La, Lb, MinExponent, MaxExponent, NExponents, R, Omega, &
            LMulti, LmaxSpher, Ltransf, AbsTol, WeightFunc, SpherTransf)
            integer, intent(in)   :: La, Lb
            real(F64), intent(in) :: MinExponent
            real(F64), intent(in) :: MaxExponent
            integer, intent(in)   :: NExponents
            real(F64), intent(in) :: R
            real(F64), intent(in) :: Omega
            integer, intent(in)   :: LMulti
            integer, intent(in)   :: LmaxSpher
            integer, intent(in)   :: Ltransf
            real(F64), intent(in) :: AbsTol
            integer, intent(in)   :: WeightFunc
            logical, intent(in)   :: SpherTransf
            
            real(F64), parameter :: MinAlphaGauss1 = 0.30_F64
            real(F64), parameter :: MaxAlphaGauss1 = 2.00_F64
            real(F64), parameter :: MinAlphaGauss2 = 0.05_F64
            real(F64), parameter :: MaxAlphaGauss2 = 1.50_F64
            real(F64), parameter :: MinAlphaWerner = 2.80_F64
            real(F64), parameter :: MaxAlphaWerner = 4.00_F64
            real(F64), parameter :: min_abscissa = 0.0_F64
            real(F64), parameter :: max_abscissa = 30.0_F64
             
            real(F64) :: f1, f2, f3
            real(F64) :: x1, x2, x3
            integer :: info
            real(F64) :: next_abscissa
            real(F64) :: fmin, xmin
            real(F64) :: fnew
            type(tunidata) :: pd
            real(F64) :: uncertainty
            real(F64), parameter :: SmallInts = 1.0E-6_F64
            integer, parameter :: max_niter = 100
            real(F64), dimension(3) :: Ra, Rb
            real(F64) :: RMSError, AvgAbsError, MaxAbsError, AvgRelError, MaxRelError
            character(:), allocatable :: line
            logical :: SingleCenter

            if (R > ZERO) then
                  SingleCenter = .false.
            else
                  SingleCenter = .true.
            end if
            call msg("Starting search for the optimal weight function")

            select case (WeightFunc)
            case (MULT_COEFF_GAUSS1)
                  x1 = MinAlphaGauss1
                  x3 = MaxAlphaGauss1
                  call msg("Weight type: exp(-alpha/2 s**2)")
            case (MULT_COEFF_GAUSS2)
                  x1 = MinAlphaGauss2
                  x3 = MaxAlphaGauss2
                  call msg("Weight type: (1/alpha+s**2)exp(-alpha s**2)")
            case (MULT_COEFF_WERNER)
                  x1 = MinAlphaWerner
                  x3 = MaxAlphaWerner
                  call msg("Weight type: (1/alpha+s)exp(-alpha s)")
            end select
            
            if (SingleCenter) then
                  call msg("Orbitals located at the same center")
            else
                  call msg("Distance between orbital centers: " // str(R,d=3))
            end if
            call msg("Bra angular momentum: " // str(La))
            call msg("Ket angular momentum: " // str(Lb))
            call msg("Smallest exponent: " // str(MinExponent))
            call msg("Largest exponent: " // str(MaxExponent))
            call msg("Number of exponents: " // str(NExponents))
            if (SpherTransf) then
                  call msg("Using spherical multipoles")
                  call msg("Lmax = " // str(LMulti))
                  call msg("LmaxSpher = " // str(LmaxSpher))
            else
                  call msg("Using Cartesian multipoles")
                  call msg("Lmax = " // str(LMulti))
            end if

            x2 = (x1 + x3) / TWO
                  
            Ra = [ZERO, ZERO, ZERO]
            Rb = [ZERO, ZERO, R]
            
            call EstimateVmultError(RMSError, AvgAbsError, MaxAbsError, AvgRelError, MaxRelError, &
                  SmallInts, x1, Ra, Rb, Omega, LMulti, LmaxSpher, Ltransf, SingleCenter, WeightFunc, La, Lb, MinExponent, &
                  MaxExponent, NExponents, SpherTransf)
            f1 = RMSError

            call EstimateVmultError(RMSError, AvgAbsError, MaxAbsError, AvgRelError, MaxRelError, &
                  SmallInts, x2, Ra, Rb, Omega, LMulti, LmaxSpher, Ltransf, SingleCenter, WeightFunc, La, Lb, MinExponent, &
                  MaxExponent, NExponents, SpherTransf)
            f2 = RMSError

            call EstimateVmultError(RMSError, AvgAbsError, MaxAbsError, AvgRelError, MaxRelError, &
                  SmallInts, x3, Ra, Rb, Omega, LMulti, LmaxSpher, Ltransf, SingleCenter, WeightFunc, La, Lb, MinExponent, &
                  MaxExponent, NExponents, SpherTransf)
            f3 = RMSError
            
            call uni_firstiter(next_abscissa, x1, f1, x2, f2, x3, f3, AbsTol, &
                  max_niter, min_abscissa, max_abscissa, pd, info)

            line = lfield("#", 5) // lfield("alpha", 15) // lfield("RMSE", 15)
            call msg(line)
            do while (info .eq. UNI_CONTINUE)
                  call EstimateVmultError(RMSError, AvgAbsError, MaxAbsError, AvgRelError, MaxRelError, &
                        SmallInts, next_abscissa, Ra, Rb, Omega, LMulti, LmaxSpher, Ltransf, SingleCenter, WeightFunc, &
                        La, Lb, MinExponent, MaxExponent, NExponents, SpherTransf)
                  fnew = RMSError
                  line = lfield(str(pd%niter), 5) // lfield(str(next_abscissa, d=3), 15) // lfield(str(RMSError, d=3), 15)
                  call msg(line)
                  call uni_nextiter(next_abscissa, fnew, pd, info)
                  if (info == UNI_EXCEEDED_MAX_NITER) then
                        call msg("Maximum number of iterations have been exceeded")
                  else if (info == UNI_MULTIPLE_MINIMA) then
                        call msg("Error: the objective function has multiple minima")
                  end if
            end do

            if (info == UNI_CONVERGED) then
                  call uni_getsolution(xmin, fmin, uncertainty, pd)
                  call msg("Converged solution")
                  call msg("alpha = " // str(xmin, d=3))
                  call msg("RMSE = " // str(fmin, d=3))
                  call msg("Uncertainty in alpha = " // str(uncertainty, d=3))
            end if
      end subroutine OptimizeAlpha

      
      subroutine EstimateVmultError(RMSError, AvgAbsError, MaxAbsError, AvgRelError, MaxRelError, &
            eps, alpha, Ra, Rb, Omega, LMulti, LmaxSpher, Ltransf, SingleCenter, WeightFunc, La, Lb, &
            MinExponent, MaxExponent, NExponents, SpherTransf)
            real(F64), intent(out)              :: RMSError
            real(F64), intent(out)              :: AvgAbsError
            real(F64), intent(out)              :: MaxAbsError
            real(F64), intent(out)              :: AvgRelError
            real(F64), intent(out)              :: MaxRelError
            real(F64), intent(in)               :: eps
            real(F64), intent(in)               :: alpha
            real(F64), dimension(3), intent(in) :: Ra, Rb
            real(F64), intent(in)               :: Omega
            integer, intent(in)                 :: LMulti
            integer, intent(in)                 :: LmaxSpher
            integer, intent(in)                 :: Ltransf
            logical, intent(in)                 :: SingleCenter
            integer, intent(in)                 :: WeightFunc
            integer, intent(in)                 :: La, Lb
            real(F64), intent(in)               :: MinExponent, MaxExponent
            integer, intent(in)                 :: NExponents
            logical, intent(in)                 :: SpherTransf

            integer :: na, nb
            integer, parameter :: NprimA = 1
            integer, parameter :: NprimB = 1
            real(F64), dimension(:), allocatable :: NormA
            real(F64), dimension(:), allocatable :: NormB
            real(F64), dimension(NprimA), parameter :: CntrA = [1.0_F64]
            real(F64), dimension(NprimB), parameter :: CntrB = [1.0_F64]
            real(F64), dimension(NprimA) :: ExpA
            real(F64), dimension(NprimB) :: ExpB
            !
            ! Dummy S function
            !
            integer, parameter :: Ldummy = 0
            integer, parameter :: NprimDummy = 1
            real(F64), dimension(((Ldummy+1)*(Ldummy+2))/2), parameter :: NormDummy = [1.0_F64]
            real(F64), dimension(NprimDummy), parameter :: CntrDummy = [1.0_F64]
            real(F64), dimension(NprimDummy), parameter :: ExpDummy = [0.0_F64]
            real(F64), dimension(3), parameter :: RDummy = [0.0_F64, 0.0_F64, 0.0_F64]
            real(F64), dimension(:, :), allocatable :: Vexact
            real(F64), dimension(:, :), allocatable :: Vmult
            real(F64), dimension(:, :), allocatable :: MxyzA, MxyzB, MlmkA, MlmkB, Txyz, Tlmk
            real(F64), dimension(3) :: DeltaR
            integer :: k, l
            integer :: Nnonzero
            real(F64) :: Kappa
            integer :: a, ax, ay, az, b, bx, by, bz
            real(F64) :: NormInt, ExpK, ExpL
            real(F64) :: RelError, AbsError
            real(F64) :: WorstExpA, WorstExpB, WorstVexact, WorstVmult
            real(F64), dimension(:), allocatable :: CspherT, CspherM
            integer, dimension(-1:LMulti) :: Plmk, Nlmk, Pxyz, Pc
            integer :: NFittedCoeffs
            real(F64), dimension(NRadialQuad) :: s_points, s_weights
            real(F64), dimension(NSpherQuad) :: t_points, t_weights
            logical :: converged
            real(F64), dimension(:, :, :), allocatable :: Euvw
            real(F64), dimension(:, :), allocatable :: DmnMatrixForm
            real(F64), dimension(:, :), allocatable :: AfitAB, AfitAA
            real(F64), dimension(:), allocatable :: D0n
            real(F64), dimension(:), allocatable :: DmnVectorForm
            real(F64), dimension(:, :), allocatable :: Wxyz
            real(F64), dimension(:, :), allocatable :: MultWork
            integer :: m, n
            real(F64), dimension(0:LMulti, 0:LMulti) :: binom

            call BinomTable(binom, LMulti)
            call NSpherCoeffs_P(Pc, LMulti, LmaxSpher)
            allocate(CspherT(Pc(LMulti)))
            allocate(CspherM(Pc(LMulti)))
            call SpherCoeffs(CspherT, CspherM, LMulti, LmaxSpher)

            if (WeightFunc == MULT_COEFF_WERNER) then
                  call quad_GaussLaguerre(s_points, s_weights, converged, NRadialQuad, 1.0E-14_F64)
            else
                  call quad_GaussHermite(s_points, s_weights, converged, 2*NRadialQuad, 1.0E-14_F64)
            end if
            call quad_GaussLegendre(t_points, t_weights, converged, 2*NSpherQuad, 1.0E-14_F64)

            NFittedCoeffs = 0
            do m = 0, LMulti
                  do n = 0, (LMulti-m)/2
                        NFittedCoeffs = NFittedCoeffs + 1
                  end do
            end do
            allocate(AfitAB(0:NFittedCoeffs-1, 0:NFittedCoeffs-1))            
            allocate(DmnVectorForm(NFittedCoeffs))
            allocate(Euvw(0:LMulti, 0:LMulti, 0:LMulti))
            allocate(DmnMatrixForm(0:LMulti, 0:LMulti/2))
            allocate(AfitAA(0:LMulti/2, 0:LMulti/2))
            allocate(D0n(0:LMulti/2))

            call NSpherMulti_P(Plmk, LMulti, LmaxSpher)
            call NSpherMulti_N(Nlmk, LMulti, LmaxSpher)
            call NCartMulti_P(Pxyz, LMulti)

            allocate(Tlmk(Plmk(LMulti), Plmk(LMulti)))
            allocate(Txyz(Pxyz(LMulti), Pxyz(LMulti)))
            allocate(Wxyz(Pxyz(LMulti), Plmk(LMulti)))
            
            na = ((La+1)*(La+2))/2
            nb = ((Lb+1)*(Lb+2))/2
            Kappa = ONE/Omega**2
            DeltaR = Rb - Ra
            allocate(MxyzA(Pxyz(LMulti), na))
            allocate(MxyzB(Pxyz(LMulti), nb))
            allocate(MlmkA(Plmk(LMulti), na))
            allocate(MlmkB(Plmk(LMulti), nb))
            allocate(NormA(na))
            allocate(NormB(nb))
            allocate(Vexact(na, nb))
            allocate(Vmult(na, nb))
            if (SpherTransf) then
                  allocate(MultWork(na, Plmk(LMulti)))
            else
                  allocate(MultWork(na, Pxyz(LMulti)))
            end if
            RMSError = ZERO
            AvgAbsError = ZERO
            MaxAbsError = ZERO
            MaxRelError = ZERO
            AvgRelError = ZERO
            WorstExpA = ZERO
            WorstExpB = ZERO
            WorstVexact = ZERO
            WorstVmult = ZERO
            Nnonzero = 0
            do k = 1, NExponents
                  do l = k, NExponents
                        ExpK = MinExponent + (k-1) * (MaxExponent-MinExponent)/(NExponents-1)
                        ExpL = MinExponent + (l-1) * (MaxExponent-MinExponent)/(NExponents-1)
                        ExpA = ExpL
                        ExpB = ExpK
                        a = 1
                        do ax = La, 0, -1
                              do ay = La-ax, 0, -1
                                    az = La - ax - ay
                                    NormInt = Moment1D(ax, TWO*ExpL) * Moment1D(ay, TWO*ExpL) * Moment1D(az, TWO*ExpL)
                                    NormA(a) = sqrt(ONE/NormInt)
                                    call CartMultipoles(MxyzA(:, a), LMulti, ax, ay, az, CntrA, NormA(a), ExpA, NprimA)
                                    a = a + 1
                              end do
                        end do
                        b = 1
                        do bx = Lb, 0, -1
                              do by = Lb - bx, 0, -1
                                    bz = Lb - bx - by
                                    NormInt = Moment1D(bx, TWO*ExpK) * Moment1D(by, TWO*ExpK) * Moment1D(bz, TWO*ExpK)
                                    NormB(b) = sqrt(ONE/NormInt)
                                    call CartMultipoles(MxyzB(:, b), LMulti, bx, by, bz, CntrB, NormB(b), ExpB, NprimB)
                                    b = b + 1
                              end do
                        end do

                        if (SpherTransf) then
                              call transf_Vlmk_Vxyz(MlmkA, MxyzA, CspherM, LMulti, Ltransf, &
                                    Plmk, Nlmk, Pxyz, Pc, na)
                              call transf_Vlmk_Vxyz(MlmkB, MxyzB, CspherM, LMulti, Ltransf, &
                                    Plmk, Nlmk, Pxyz, Pc, nb)
                        end if

                        if (SingleCenter) then
                              call AUTO2EERI(auto2e_idx(Lb, LDummy, La, LDummy))%ptr(Vexact, &
                                    RDummy,     CntrB,     NormB,     ExpB,     NprimB, &
                                    RDummy, CntrDummy, NormDummy, ExpDummy, NprimDummy, &
                                    RDummy,     CntrA,     NormA,     ExpA,     NprimA, &
                                    RDummy, CntrDummy, NormDummy, ExpDummy, NprimDummy, &
                                    Kappa)
                              call T_AA_Fit(Txyz, LMulti, Omega, alpha, WeightFunc, s_points, &
                                    s_weights, D0n, AfitAA, Euvw, binom)
                        else
                              call AUTO2EERI(auto2e_idx(Lb, LDummy, La, LDummy))%ptr(Vexact, &
                                    Rb,     CntrB,     NormB,     ExpB,     NprimB, &
                                    RDummy, CntrDummy, NormDummy, ExpDummy, NprimDummy, &
                                    Ra,     CntrA,     NormA,     ExpA,     NprimA, &
                                    RDummy, CntrDummy, NormDummy, ExpDummy, NprimDummy, &
                                    Kappa)
                              if (WeightFunc == MULT_COEFF_TAYLOR) then
                                    call T_AB_Taylor(Txyz, DeltaR, LMulti, Omega)
                              else
                                    call T_AB_Fit(Txyz, DeltaR, LMulti, Omega, alpha, WeightFunc, &
                                          s_points, s_weights, t_points, t_weights, DmnVectorForm, &
                                          DmnMatrixForm, AfitAB, Euvw, binom)
                              end if
                        end if

                        if (SpherTransf) then
                              call transf_Tlmk_Txyz(Tlmk, Txyz, CspherT, LMulti, Ltransf, &
                                    Wxyz, Plmk, Nlmk, Pxyz, Pc)
                              call real_aTb(MultWork, MlmkA, Tlmk)
                              call real_ab(Vmult, MultWork, MlmkB)
                        else
                              call real_aTb(MultWork, MxyzA, Txyz)
                              call real_ab(Vmult, MultWork, MxyzB)
                        end if

                        do b = 1, nb
                              do a = 1, na
                                    if (abs(Vexact(a, b)) > eps) then
                                          AbsError = abs(Vexact(a, b) - Vmult(a, b))
                                          RMSError = RMSError + (Vexact(a, b) - Vmult(a, b))**2
                                          MaxAbsError = max(MaxAbsError, AbsError)
                                          RelError = (Vexact(a, b) - Vmult(a, b)) / Vexact(a, b)
                                          if (abs(RelError) > MaxRelError) then
                                                WorstExpA = ExpL
                                                WorstExpB = ExpK
                                                WorstVmult = Vmult(a, b)
                                                WorstVexact = Vexact(a, b)
                                                MaxRelError = abs(RelError)
                                          end if
                                          AvgAbsError = AvgAbsError + AbsError
                                          AvgRelError = AvgRelError + abs(RelError)
                                          Nnonzero = Nnonzero + 1
                                    end if
                              end do
                        end do
                  end do
            end do
            if (Nnonzero > 0) then
                  RMSError = sqrt(RMSError / Nnonzero)
                  AvgAbsError = AvgAbsError / Nnonzero
                  AvgRelError = AvgRelError / Nnonzero
            end if
      end subroutine EstimateVmultError


      subroutine BinomTable(B, Lmax)
            real(F64), dimension(0:, 0:), intent(out) :: B
            integer, intent(in)                       :: Lmax

            integer :: m, n

            do n = 0, Lmax
                  do m = 0, Lmax
                        B(m, n) = binom(m, n)
                  end do
            end do
      end subroutine BinomTable

      
      subroutine SpherCoeffs(CspherT, CspherM, Lmax, LmaxSpher)
            !
            ! Compute the matrices of the Cartesian->Spherical multipole transformation
            ! for the multipole moment integrals and the interaction matrix.
            !
            ! The transformation coefficients are stored in contiguous blocks
            ! for angular momenta J=LmaxSpher+1...Lmax. Each block has the length of
            ! Nxyz(J) * Nlmk(J). The coefficients for J=0...LmaxSpher are entirely
            ! skipped.
            !
            ! Multipole moment integrals
            ! --------------------------
            !
            ! Mspher = CspherM**T * Mcart
            !
            ! Interaction matrix
            ! ------------------
            ! Tspher = CspherT**T Tcart CspherT
            !
            ! The spherical multipoles are defined as Slmk = r**(l+2k) Slm,
            ! where l<=min(J,LmaxSpher) and l+2k=J.
            !
            real(F64), dimension(:), intent(out) :: CspherT
            real(F64), dimension(:), intent(out) :: CspherM
            integer, intent(in)                  :: Lmax
            integer, intent(in)                  :: LmaxSpher

            real(F64), dimension(:), allocatable :: Ulm
            real(F64), dimension(:), allocatable :: work
            integer :: MaxNxyz
            integer :: J, Nxyz, BlockLength, l, m
            integer, dimension(-1:Lmax) :: Pc, Nlmk

            call NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            call NSpherCoeffs_P(Pc, Lmax, LmaxSpher)
            MaxNxyz = ((LmaxSpher+1)*(LmaxSpher+2))/2
            allocate(work(MaxNxyz))
            allocate(Ulm(numulm(LmaxSpher)))
            do l = 0, LmaxSpher
                  do m = -l, l
                        call rshu(Ulm(ulmpos(l,m):), l, m)
                  end do
            end do
            do J = 0, Lmax
                  Nxyz = ((J + 1) * (J + 2)) / 2
                  BlockLength = Nlmk(J) * Nxyz
                  call SpherCoeffs_BlockJ(CspherT(Pc(J-1)+1:Pc(J-1)+BlockLength), &
                        CspherM(Pc(J-1)+1:Pc(J-1)+BlockLength), Nxyz, J, LmaxSpher, &
                        Ulm, work)
            end do
      end subroutine SpherCoeffs


      subroutine SpherCoeffs_BlockJ(Cint, Cmult, ldc, J, LmaxSpher, Ulm, work)
            !
            ! Compute the matrices of the Cartesian->Spherical multipole transformation
            ! for the multipole moment integrals and the interaction matrix. The coefficients
            ! are computed for the Cartesian angular momentum J and a series of spherical
            ! angular momenta satisfying l <= min(J, Lmax).
            !
            ! Multipole moment integrals
            ! --------------------------
            !
            ! Mspher = Cmult**T * Mcart
            !
            ! Interaction matrix
            ! ------------------
            ! Tspher = Cint**T Tcart Cint
            !
            ! The spherical multipoles are defined as Slmk = r**(l+2k) Slm,
            ! where l<=min(J,LmaxSpher) and l+2k=J.
            !
            real(F64), dimension(ldc, *), intent(out) :: Cint
            real(F64), dimension(ldc, *), intent(out) :: Cmult
            integer, intent(in)                       :: ldc
            integer, intent(in)                       :: J
            integer, intent(in)                       :: LmaxSpher
            real(F64), dimension(:), intent(in)       :: Ulm
            real(F64), dimension(:), intent(out)      :: work

            integer :: l, m, kappa, kappa0, kappa1
            integer :: p, lmk, up, vp, wp
            !
            ! Loop bounds for kappa are derived from the inequality
            ! 0 <= l <= min(J,LmaxSpher), where l = J - 2*kappa
            !
            kappa0 = (J - min(LmaxSpher, J)) / 2 + modulo(J - min(J, LmaxSpher), 2)
            kappa1 = J / 2
            lmk = 1
            do kappa = kappa0, kappa1
                  l = J - 2 * kappa
                  do m = -l, l
                        do up = 0, J
                              do vp = 0, J - up
                                    wp = J - up - vp
                                    p = lxlylzpos(up, vp, wp)
                                    Cint(p, lmk) = rshv(l, m, up, vp, wp, Ulm, work)
                              end do
                        end do
                        call ClmkCoeffs(Cmult(:, lmk), l, kappa, Ulm(ulmpos(l,m):))
                        lmk = lmk + 1
                  end do
            end do
      end subroutine SpherCoeffs_BlockJ


      subroutine transf_Tlmk_Txyz(Tlmk, Txyz, C, Lmax, Ltransf, Wxyz, Plmk, Nlmk, Pxyz, Pc)
            real(F64), dimension(:, :), intent(out) :: Tlmk
            real(F64), dimension(:, :), intent(in)  :: Txyz
            real(F64), dimension(:), intent(in)     :: C
            integer, intent(in)                     :: Lmax
            integer, intent(in)                     :: Ltransf
            real(F64), dimension(:, :), intent(out) :: Wxyz
            integer, dimension(-1:), intent(in)     :: Plmk
            integer, dimension(-1:), intent(in)     :: Nlmk
            integer, dimension(-1:), intent(in)     :: Pxyz
            integer, dimension(-1:), intent(in)     :: Pc

            integer :: Ncart, Nxyz, NxyzTot, Nvecs
            integer :: J
            integer :: w0, w1, t0, t1, c0, c1

            NxyzTot = Pxyz(Lmax)
            !
            ! For the angular momenta K=0...Ltransf-1 the transformation is skipped
            ! because the number of Slmk functions is the same as the number
            ! of Cartesian polynomials.
            !
            Ncart = Pxyz(Ltransf-1)
            Wxyz(:, 1:Ncart) = Txyz(:, 1:Ncart)
            !
            ! Transform column indices
            !
            do J = Ltransf, Lmax
                  Nxyz = ((J + 1) * (J + 2)) / 2
                  w0 = Plmk(J-1) + 1
                  w1 = Plmk(J)
                  t0 = Pxyz(J-1) + 1
                  t1 = Pxyz(J)
                  c0 = Pc(J-1) + 1
                  c1 = Pc(J)
                  call real_ab_x(Wxyz(:, w0:w1), NxyzTot, Txyz(:, t0:t1), &
                        NxyzTot, C(c0:c1), Nxyz, NxyzTot, Nlmk(J), Nxyz)
            end do
            !
            ! Transform row indices
            !
            Nvecs = Plmk(Lmax)
            call transf_Vlmk_Vxyz(Tlmk, Wxyz, C, Lmax, Ltransf, &
                  Plmk, Nlmk, Pxyz, Pc, Nvecs)
      end subroutine transf_Tlmk_Txyz


      subroutine transf_Vlmk_Vxyz(Vlmk, Vxyz, C, Lmax, Ltransf, &
            Plmk, Nlmk, Pxyz, Pc, Nvecs)
            real(F64), dimension(*), intent(out) :: Vlmk
            real(F64), dimension(*), intent(in)  :: Vxyz
            real(F64), dimension(*), intent(in)  :: C
            integer, intent(in)                  :: Lmax
            integer, intent(in)                  :: Ltransf
            integer, dimension(-1:), intent(in)  :: Plmk
            integer, dimension(-1:), intent(in)  :: Nlmk
            integer, dimension(-1:), intent(in)  :: Pxyz
            integer, dimension(-1:), intent(in)  :: Pc
            integer, intent(in)                  :: Nvecs

            integer :: J
            integer :: Nxyz, NxyzTot, NlmkTot
            integer :: lmk0, lmk1, xyz0, xyz1, c0, c1
            !
            ! For the angular momenta J=0...Ltransf-1 the transformation is skipped
            ! because the number of Slmk functions is the same as the number
            ! of Cartesian polynomials.
            !
            NlmkTot = Plmk(Lmax)
            NxyzTot = Pxyz(Lmax)
            call transf_Vlmk_Vxyz_CopyCartesian(Vlmk, Vxyz, NlmkTot, NxyzTot, &
                  Pxyz(Ltransf-1), Nvecs)
            do J = Ltransf, Lmax
                  lmk0 = Plmk(J-1) + 1
                  lmk1 = Plmk(J) + NlmkTot * (Nvecs - 1)
                  xyz0 = Pxyz(J-1) + 1
                  xyz1 = Pxyz(J) + NxyzTot * (Nvecs - 1)
                  c0 = Pc(J-1) + 1
                  c1 = Pc(J)
                  Nxyz = ((J+1)*(J+2)) / 2
                  call transf_Vlmk_Vxyz_BlockJ(Vlmk(lmk0:lmk1), Vxyz(xyz0:xyz1), C(c0:c1), &
                        Nlmk(J), Nxyz, NlmkTot, NxyzTot, Nvecs)
            end do
      end subroutine transf_Vlmk_Vxyz
      

      subroutine transf_Vlmk_Vxyz_BlockJ(Vlmk, Vxyz, C, Nlmk, Nxyz, NlmkTot, NxyzTot, Nvecs)
            real(F64), dimension(NlmkTot, *), intent(inout) :: Vlmk
            real(F64), dimension(NxyzTot, *), intent(in)    :: Vxyz
            real(F64), dimension(Nxyz, *), intent(in)       :: C
            integer, intent(in)                             :: Nlmk
            integer, intent(in)                             :: Nxyz
            integer, intent(in)                             :: NlmkTot
            integer, intent(in)                             :: NxyzTot
            integer, intent(in)                             :: Nvecs
            
            call real_aTb_x(Vlmk, NlmkTot, C, Nxyz, Vxyz, NxyzTot, Nlmk, Nvecs, Nxyz)
      end subroutine transf_Vlmk_Vxyz_BlockJ


      subroutine transf_Vlmk_Vxyz_CopyCartesian(Vlmk, Vxyz, NlmkTot, NxyzTot, Ncart, Nvecs)
            real(F64), dimension(NlmkTot, *), intent(inout) :: Vlmk
            real(F64), dimension(NxyzTot, *), intent(in)    :: Vxyz
            integer, intent(in)                             :: NlmkTot
            integer, intent(in)                             :: NxyzTot
            integer, intent(in)                             :: Ncart
            integer, intent(in)                             :: Nvecs
            
            Vlmk(1:Ncart, 1:Nvecs) = Vxyz(1:Ncart, 1:Nvecs)
      end subroutine transf_Vlmk_Vxyz_CopyCartesian
            

      function Moment1D(l, alpha)
            real(F64) :: Moment1D
            integer, intent(in) :: l
            real(F64), intent(in) :: alpha
            !
            ! Integrate(-Inf,+Inf) x**(2*l) * exp(-alpha * x**2) dx
            !
            Moment1D = dblfact(2*l-1) / (TWO * alpha)**l * Sqrt(PI/alpha)
      end function Moment1D


      subroutine CartMultipoles(M, L, xa, ya, za, CntrA, NormA, ExpA, NprimA)
            real(F64), dimension(:), intent(out) :: M
            integer, intent(in)                  :: L
            integer, intent(in)                  :: xa, ya, za
            real(F64), dimension(:), intent(in)  :: CntrA
            real(F64), intent(in)                :: NormA
            real(F64), dimension(:), intent(in)  :: ExpA
            integer, intent(in)                  :: NprimA

            real(F64), dimension(0:L) :: XInts, YInts, ZInts
            
            integer :: OffsetP, Lp, p, k
            integer :: x, y, z, dx, dy, dz, t, u, v
            integer :: n

            dx = modulo(xa, 2)
            dy = modulo(ya, 2)
            dz = modulo(za, 2)
            n = NCartMulti(L)
            M(1:n) = ZERO
            do k = 1, NprimA
                  XInts = ZERO
                  YInts = ZERO
                  ZInts = ZERO
                  do t = 0, (L-dx)/2
                        x = 2 * t + dx
                        XInts(x) = Moment1D((xa+x)/2, ExpA(k))
                  end do
                  do u = 0, (L-dy)/2
                        y = 2 * u + dy
                        YInts(y) = Moment1D((ya+y)/2, ExpA(k))
                  end do
                  do v = 0, (L-dz)/2
                        z = 2 * v + dz
                        ZInts(z) = Moment1D((za+z)/2, ExpA(k))
                  end do
                  do Lp = 0, L
                        do x = 0, Lp
                              do y = 0, Lp - x
                                    z = Lp - x - y
                                    OffsetP = NCartMulti(Lp-1)
                                    p = OffsetP + lxlylzpos(x, y, z)
                                    M(p) = M(p) + CntrA(k) * XInts(x) * YInts(y) * ZInts(z)
                              end do
                        end do
                  end do
            end do
            M(1:n) = NormA * M(1:n)
      end subroutine CartMultipoles


      function NCartMulti(Lmax)
            !
            ! Total number of Cartesian multipoles x**u y**v z**w up to order Lmax.
            !
            integer :: NCartMulti
            integer, intent(in) :: Lmax
            NCartMulti = ((Lmax+1)*(Lmax+2)*(Lmax+3))/6
      end function NCartMulti


      subroutine NCartMulti_P(Pxyz, Lmax)
            !
            ! Total number of Cartesian multipoles up order J <= Lmax.
            !
            integer, intent(in) :: Lmax
            integer, dimension(-1:), intent(out) :: Pxyz
            integer :: J

            Pxyz(-1) = 0
            do J = 0, Lmax
                  Pxyz(J) = NCartMulti(J)
            end do
      end subroutine NCartMulti_P


      function NSpherMulti(Lmax, LmaxSpher)
            !
            !
            ! Compute the total number of spherical multipoles up to order Lmax.
            ! The following condidtions apply:
            ! Slmk = r**(l+2k) Slm, l+2k=Lmax, 0 <= l <= LmaxSpher
            !
            integer :: NSpherMulti
            integer, intent(in) :: Lmax
            integer, intent(in) :: LmaxSpher

            integer :: l

            NSpherMulti = 0
            do l = 0, LmaxSpher
                  NSpherMulti = NSpherMulti + (2 * l + 1) * (1 + (Lmax - l) / 2)
            end do
      end function NSpherMulti

      
      subroutine NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            !
            ! Compute the number of spherical multipoles for each J <= Lmax.
            ! The following condidtions apply:
            ! Slmk = r**(l+2k) Slm, l+2k=J, 0 <= l <= min(J,LmaxSpher)
            !
            integer, dimension(-1:), intent(out) :: Nlmk
            integer, intent(in)                  :: Lmax
            integer, intent(in)                  :: LmaxSpher
            
            integer :: J, kappa0, kappa1

            Nlmk(-1) = 0
            do J = 0, Lmax
                  kappa0 = (J-min(LmaxSpher,J))/2+modulo(J-min(J,LmaxSpher),2)
                  kappa1 = J / 2                  
                  Nlmk(J) = (kappa1 - kappa0 + 1) * (2 * J + 1 - 2 * (kappa0 + kappa1))
            end do
      end subroutine NSpherMulti_N
      

      subroutine NSpherMulti_P(Plmk, Lmax, LmaxSpher)
            !
            ! Compute the total number of spherical multipoles up to
            ! order J <= Lmax.
            ! The following condidtions apply:
            ! Slmk = r**(l+2k) Slm, l+2k=J, 0 <= l <= min(J,LmaxSpher)
            !
            integer, dimension(-1:), intent(out) :: Plmk
            integer, intent(in)                  :: Lmax
            integer, intent(in)                  :: LmaxSpher
            
            integer :: J
            integer, dimension(-1:Lmax) :: Nlmk

            call NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            Plmk(-1) = 0
            do J = 0, Lmax
                  Plmk(J) = sum(Nlmk(0:J))
            end do
      end subroutine NSpherMulti_P


      subroutine NSpherCoeffs_P(Pc, Lmax, LmaxSpher)
            !
            ! Compute the number of coefficients of the Cartesian->spherical
            ! transform matrix
            !
            integer, dimension(-1:), intent(out) :: Pc
            integer, intent(in)                  :: Lmax
            integer, intent(in)                   :: LmaxSpher

            integer, dimension(-1:Lmax) :: Nlmk
            integer :: Nxyz
            integer :: J

            call NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            Pc(-1) = 0
            do J = 0, Lmax
                  Nxyz = ((J+1)*(J+2))/2
                  if (J > 0) then
                        Pc(J) = Pc(J-1) + Nxyz * Nlmk(J)
                  else
                        Pc(J) = Nxyz * Nlmk(J)
                  end if
            end do
      end subroutine NSpherCoeffs_P
      
      
      subroutine EuvwCoeffs(Euvw, L, Dmn, Rx, Ry, Rz)
            real(F64), dimension(0:, 0:, 0:), intent(out) :: Euvw
            integer, intent(in) :: L
            real(F64), dimension(0:, 0:), intent(in) :: Dmn
            real(F64), intent(in) :: Rx, Ry, Rz

            integer :: u, v, w
            integer :: uu, vv, ww

            Euvw = ZERO
            do w = 0, L
                  do v = 0, L - w
                        do u = 0, L - w - v
                              do ww = 0, w/2
                                    do vv = 0, v/2
                                          do uu = 0, u/2
                                                Euvw(u, v, w) = Euvw(u, v, w) + Dmn(u+v+w-2*(uu+vv+ww), uu+vv+ww) &
                                                      * fact(u+v+w-2*(uu+vv+ww))/(fact(u-2*uu)*fact(v-2*vv)*fact(w-2*ww)) &
                                                      * fact(uu+vv+ww)/(fact(uu)*fact(vv)*fact(ww)) &
                                                      * Rx**(u-2*uu) * Ry**(v-2*vv) * Rz**(w-2*ww)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine EuvwCoeffs


      subroutine EuvwCoeffsDiag(Euvw, L, D0n)
            real(F64), dimension(0:, 0:, 0:), intent(out) :: Euvw
            integer, intent(in) :: L
            real(F64), dimension(0:), intent(in) :: D0n

            integer :: u, v, w
            integer :: uu, vv, ww

            Euvw = ZERO
            do ww = 0, L/2
                  do vv = 0, (L - 2*ww)/2
                        do uu = 0, (L - 2*ww - 2*vv)/2
                              u = 2 * uu
                              v = 2 * vv
                              w = 2 * ww
                              Euvw(u, v, w) = D0n(uu+vv+ww) * fact(uu+vv+ww)/(fact(uu)*fact(vv)*fact(ww))
                        end do
                  end do
            end do
      end subroutine EuvwCoeffsDiag


      subroutine TpqCoeffs(T, L, E, binom)
            real(F64), dimension(:, :), intent(out)      :: T
            integer, intent(in)                          :: L
            real(F64), dimension(0:, 0:, 0:), intent(in) :: E
            real(F64), dimension(0:, 0:), intent(in)       :: binom
            
            integer :: NMulti, J, Lp, Lq, uq, vq, wq, up, vp, wp
            integer :: OffsetP, OffsetQ, p, q

            NMulti = NCartMulti(L)
            T(1:NMulti, 1:NMulti) = ZERO
            do J = 0, L
                  do Lq = 0, J
                        do Lp = 0, J - Lq
                              do uq = 0, Lq
                                    do vq = 0, Lq - uq
                                          do up = 0, Lp
                                                do vp = 0, Lp - up
                                                      OffsetP = NCartMulti(Lp-1)
                                                      OffsetQ = NCartMulti(Lq-1)
                                                      wp = Lp - up - vp
                                                      wq = Lq - uq - vq
                                                      p = OffsetP + lxlylzpos(up, vp, wp)
                                                      q = OffsetQ + lxlylzpos(uq, vq, wq)
                                                      T(p, q) = (-1)**(up+vp+wp) * binom(up+uq, up) * binom(vp+vq,vp) &
                                                            * binom(wp+wq,wp) * E(up+uq,vp+vq,wp+wq)
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine TpqCoeffs
      
      
      subroutine Cs_Eq25(C, L, R)
            !
            ! C(sx,sy,sz) = (d**sx/dRx**sx)(d**sy/dRy**sy)(d**sz/dRz**sz) 1/2*Exp(-|R|**2)
            ! The recursion step is defined in Eq. 25 of
            ! Boateng, H.A. and Todorov, I.T., J. Chem. Phys. 142, 034117 (2015);
            ! doi: 10.1063/1.4905952
            !
            real(F64), dimension(:), intent(out) :: C
            integer, intent(in) :: L
            real(F64), dimension(3), intent(in) :: R

            integer :: M
            integer :: sx, sy, sz, s
            integer :: ux, uy, uz, tx, ty, tz
            integer :: OffsetA,  OffsetB, OffsetC
            real(F64) :: RNorm

            RNorm = norm2(R)
            !
            ! ||s|| = 0
            !
            C(1) = ONE/TWO * exp(-RNorm**2)
            !
            ! ||s|| = 1
            !
            C(2) = -2 * R(3) * C(1)
            C(3) = -2 * R(2) * C(1)
            C(4) = -2 * R(1) * C(1)
            !
            ! ||s|| > 1
            !
            do M = 2, L
                  OffsetA = NCartMulti(M-3)
                  OffsetB = NCartMulti(M-2)
                  OffsetC = NCartMulti(M-1)
                  s = OffsetC + 1
                  do sx = 0, M
                        do sy = 0, M - sx
                              sz = M - sx - sy
                              !
                              ! The max function is used to eliminate the indices
                              ! below the lower bound of C. This trick doesn't
                              ! change any computed value because the corresponding
                              ! elements of C are multipolied by zeros.
                              !
                              ux = OffsetA + max(1, lxlylzpos(sx-2, sy, sz))
                              uy = OffsetA + max(1, lxlylzpos(sx, sy-2, sz))
                              uz = OffsetA + max(1, lxlylzpos(sx, sy, sz-2))
                              tx = OffsetB + max(1, lxlylzpos(sx-1, sy, sz))
                              ty = OffsetB + max(1, lxlylzpos(sx, sy-1, sz))
                              tz = OffsetB + max(1, lxlylzpos(sx, sy, sz-1))
                              C(s) = -TWO/M * (sx * R(1) * C(tx) + sy * R(2) * C(ty) + sz * R(3) * C(tz) &
                                    + sx * (sx - 1) * C(ux) + sy * (sy - 1) * C(uy) + sz * (sz - 1) * C(uz))
                              s = s + 1
                        end do
                  end do
            end do
      end subroutine Cs_Eq25


      subroutine Bs_Eq27(B, L, R, C)
            !
            ! B(sx,sy,sz) = (d**sx/dRx**sx)(d**sy/dRy**sy)(d**sz/dRz**sz) Sqrt(Pi)/2*Erf(|R|)/|R|
            ! The recursion step is defined in Eq. 27 of
            ! Boateng, H.A. and Todorov, I.T., J. Chem. Phys. 142, 034117 (2015);
            ! doi: 10.1063/1.4905952
            !
            real(F64), dimension(:), intent(out) :: B
            integer, intent(in) :: L
            real(F64), dimension(3), intent(in) :: R
            real(F64), dimension(:), intent(in) :: C

            integer :: M
            integer :: sx, sy, sz, s
            integer :: ux, uy, uz, tx, ty, tz
            integer :: OffsetA,  OffsetB, OffsetC
            real(F64) :: RNorm, RNorm2

            RNorm = norm2(R)
            RNorm2 = RNorm**2
            !
            ! ||s|| = 0
            !
            B(1) = sqrt(PI)/TWO * erf(RNorm)/RNorm
            !
            ! ||s|| = 1
            !
            B(2) = 1/RNorm2 * (-R(3) * B(1) - C(2))
            B(3) = 1/RNorm2 * (-R(2) * B(1) - C(3))
            B(4) = 1/RNorm2 * (-R(1) * B(1) - C(4))
            !
            ! ||s|| > 1
            !
            do M = 2, L
                  OffsetA = NCartMulti(M-3)
                  OffsetB = NCartMulti(M-2)
                  OffsetC = NCartMulti(M-1)
                  s = OffsetC + 1
                  do sx = 0, M
                        do sy = 0, M - sx
                              sz = M - sx - sy
                              !
                              ! The max function is used to eliminate the indices
                              ! below the lower bound of B. This trick doesn't
                              ! change any computed value because the corresponding
                              ! elements of B are multipolied by zeros.
                              !
                              ux = OffsetA + max(1, lxlylzpos(sx-2, sy, sz))
                              uy = OffsetA + max(1, lxlylzpos(sx, sy-2, sz))
                              uz = OffsetA + max(1, lxlylzpos(sx, sy, sz-2))
                              tx = OffsetB + max(1, lxlylzpos(sx-1, sy, sz))
                              ty = OffsetB + max(1, lxlylzpos(sx, sy-1, sz))
                              tz = OffsetB + max(1, lxlylzpos(sx, sy, sz-1))
                              B(s) = 1/RNorm2 * ((ONE/M-TWO) * (sx*R(1)*B(tx) + sy*R(2)*B(ty) + sz*R(3)*B(tz)) + &
                                    (ONE/M-ONE) * (sx*(sx-1)*B(ux) + sy*(sy-1)*B(uy) + sz*(sz-1)*B(uz)) - C(s))
                              s = s + 1
                        end do
                  end do
            end do
      end subroutine Bs_Eq27


      subroutine VeeLRDeriv(A, C, R, omega, L)
            !
            ! A(sx,sy,sz) = (d**sx/dRx**sx)(d**sy/dRy**sy)(d**sz/dRz**sz) Erf(omega*|R|)/|R|
            !
            real(F64), dimension(:), intent(out) :: A
            real(F64), dimension(:), intent(out) :: C
            real(F64), dimension(3), intent(in)  :: R
            real(F64), intent(in)                :: omega
            integer, intent(in)                  :: L

            real(F64), dimension(3) :: Rs
            integer :: M
            integer :: p, q

            Rs = omega * R
            call Cs_Eq25(C, L, Rs)
            call Bs_Eq27(A, L, Rs, C)
            do M = 0, L
                  p = NCartMulti(M-1) + 1
                  q = NCartMulti(M)
                  A(p:q) = TWO/sqrt(PI)*omega**(M+1) * A(p:q)
            end do
      end subroutine VeeLRDeriv


      subroutine DmnCoeffs_AB_Fit(DmnVectorForm, A, L, omega, R, alpha, t_points, t_weights, &
            dr_points, dr_weights, WeightFunc)
            real(F64), dimension(:), intent(out)    :: DmnVectorForm
            real(F64), dimension(:, :), intent(out) :: A
            integer, intent(in)                     :: L
            real(F64), intent(in)                   :: omega
            real(F64), intent(in)                   :: R
            real(F64), intent(in)                   :: alpha
            real(F64), dimension(:), intent(in)     :: t_points, t_weights
            real(F64), dimension(:), intent(in)     :: dr_points, dr_weights
            integer, intent(in)                     :: WeightFunc

            call AMatrix_AB_Fit(A, L, alpha, R, WeightFunc)
            call BVector_AB_Fit(DmnVectorForm, L, omega, R, alpha, t_points, t_weights, &
                  dr_points, dr_weights, WeightFunc)
            call linear_system(DmnVectorForm, A)
      end subroutine DmnCoeffs_AB_Fit


      subroutine DmnCoeffs_AA_Fit(D0n, A, L, omega, alpha, dr_points, dr_weights, WeightFunc)
            real(F64), dimension(0:), intent(out)     :: D0n
            real(F64), dimension(0:, 0:), intent(out) :: A
            integer, intent(in)                       :: L
            real(F64), intent(in)                     :: omega
            real(F64), intent(in)                     :: alpha
            real(F64), dimension(:), intent(in)       :: dr_points, dr_weights
            integer, intent(in)                       :: WeightFunc

            call AMatrix_AA_Fit(A, L, alpha, WeightFunc)
            call BVector_AA_Fit(D0n, L, omega, alpha, dr_points, dr_weights, WeightFunc)
            call linear_system(D0n, A)
      end subroutine DmnCoeffs_AA_Fit


      subroutine AMatrix_AB_Fit(A, L, alpha, R, WeightFunc)
            real(F64), dimension(:, :), intent(out) :: A
            integer, intent(in)                     :: L
            real(F64), intent(in)                   :: alpha
            real(F64), intent(in)                   :: R
            integer, intent(in)                     :: WeightFunc

            integer :: m, n, mm, nn
            integer :: p, q
            real(F64) :: pref

            A = ZERO
            select case (WeightFunc)
            case (MULT_COEFF_WERNER)
                  q = 1
                  do m = 0, L
                        do n = 0, (L-m)/2
                              p = 1
                              do mm = 0, L
                                    do nn = 0, (L-mm)/2
                                          if (modulo(m+mm, 2) == 0) then
                                                pref = TWO/(alpha**(m+2*n)*(ONE + m + mm))
                                                A(p, q) = R**m * pref * fact(2+m+mm+2*n+2*nn)*(FOUR+m+mm+2*n+2*nn)
                                          end if
                                          p = p + 1
                                    end do
                              end do
                              q = q + 1
                        end do
                  end do
            case (MULT_COEFF_GAUSS1)
                  q = 1
                  do m = 0, L
                        do n = 0, (L-m)/2
                              p = 1
                              do mm = 0, L
                                    do nn = 0, (L-mm)/2
                                          if (modulo(m+mm, 2) == 0) then
                                                pref = sqrt(TWO/alpha)**(m+2*n)
                                                A(p, q) = R**m * pref * GammaHalfK(1+(m+mm)/2+n+nn) / (ONE + m + mm)
                                          end if
                                          p = p + 1
                                    end do
                              end do
                              q = q + 1
                        end do
                  end do
            case (MULT_COEFF_GAUSS2)
                  q = 1
                  do m = 0, L
                        do n = 0, (L-m)/2
                              p = 1
                              do mm = 0, L
                                    do nn = 0, (L-mm)/2
                                          if (modulo(m+mm, 2) == 0) then
                                                pref = (R**m/sqrt(alpha)**(m+2*n))*(FIVE+m+mm+2*n+2*nn)/(TWO*(1+m+mm))
                                                A(p, q) = pref * GammaHalfK(1+(m+mm)/2+n+nn)
                                          end if
                                          p = p + 1
                                    end do
                              end do
                              q = q + 1
                        end do
                  end do
            end select
      end subroutine AMatrix_AB_Fit


      subroutine AMatrix_AA_Fit(A, L, alpha, WeightFunc)
            real(F64), dimension(0:, 0:), intent(out) :: A
            integer, intent(in)                       :: L
            real(F64), intent(in)                     :: alpha
            integer, intent(in)                       :: WeightFunc

            integer :: n, nn

            A = ZERO
            select case (WeightFunc)
            case (MULT_COEFF_WERNER)
                  do n = 0, L/2
                        do nn = 0, L/2
                              A(nn, n) = fact(4 + 2 * (n + nn)) / (alpha**(1 + 2 * n) * (3 + 2 * (n + nn)))
                        end do
                  end do
            case (MULT_COEFF_GAUSS1)
                  do n = 0, L/2
                        do nn = 0, L/2
                              A(nn, n) = (ONE/TWO) * GammaHalfK(1+n+nn) / (sqrt(alpha/TWO)*(alpha/TWO)**n)
                        end do
                  end do
            case (MULT_COEFF_GAUSS2)
                  do n = 0, L/2
                        do nn = 0, L/2
                              A(nn, n) = (ONE/TWO) * (GammaHalfK(1+n+nn) + GammaHalfK(2+n+nn)) &
                                    / (sqrt(alpha)*alpha**n)
                        end do
                  end do
            end select
      end subroutine AMatrix_AA_Fit


      subroutine BVector_AB_Fit(B, L, omega, R, alpha, t_points, t_weights, dr_points, dr_weights, WeightFunc)
            real(F64), dimension(:), intent(out) :: B
            integer, intent(in)                  :: L
            real(F64), intent(in)                :: omega
            real(F64), intent(in)                :: R
            real(F64), intent(in)                :: alpha
            real(F64), dimension(:), intent(in)  :: t_points
            real(F64), dimension(:), intent(in)  :: t_weights
            real(F64), dimension(:), intent(in)  :: dr_points
            real(F64), dimension(:), intent(in)  :: dr_weights
            integer, intent(in)                  :: WeightFunc
            
            integer :: p, m, n
            real(F64) :: bABmn

            select case (WeightFunc)
            case (MULT_COEFF_WERNER)
                  p = 1
                  do m = 0, L
                        do n = 0, (L-m)/2
                              call VeeLRNumInt_AB_Exp(bABmn, m, n, omega, R, alpha, t_points, t_weights, &
                                    dr_points, dr_weights)
                              B(p) = bABmn
                              p = p + 1
                        end do
                  end do
            case (MULT_COEFF_GAUSS1)
                  p = 1
                  do m = 0, L
                        do n = 0, (L-m)/2
                              call VeeLRNumInt_AB_Gauss1(bABmn, m, n, omega, R, alpha, t_points, t_weights, &
                                    dr_points, dr_weights)
                              B(p) = bABmn
                              p = p + 1
                        end do
                  end do
            case (MULT_COEFF_GAUSS2)
                  p = 1
                  do m = 0, L
                        do n = 0, (L-m)/2
                              call VeeLRNumInt_AB_Gauss2(bABmn, m, n, omega, R, alpha, t_points, t_weights, &
                                    dr_points, dr_weights)
                              B(p) = bABmn
                              p = p + 1
                        end do
                  end do
            end select
      end subroutine BVector_AB_Fit


      subroutine BVector_AA_Fit(B, L, omega, alpha, dr_points, dr_weights, WeightFunc)
            real(F64), dimension(0:), intent(out) :: B
            integer, intent(in)                   :: L
            real(F64), intent(in)                 :: omega
            real(F64), intent(in)                 :: alpha
            real(F64), dimension(:), intent(in)   :: dr_points
            real(F64), dimension(:), intent(in)   :: dr_weights
            integer, intent(in)                   :: WeightFunc
            
            integer :: n
            real(F64) :: bn

            select case (WeightFunc)
            case (MULT_COEFF_WERNER)
                  do n = 0, L/2
                        call VeeLRNumInt_AA_Exp(bn, n, omega, alpha, dr_points, dr_weights)
                        B(n) = bn
                  end do
            case (MULT_COEFF_GAUSS1)
                  do n = 0, L/2
                        call VeeLRNumInt_AA_Gauss1(bn, n, omega, alpha, dr_points, dr_weights)
                        B(n) = bn
                  end do
            case (MULT_COEFF_GAUSS2)
                  do n = 0, L/2
                        call VeeLRNumInt_AA_Gauss2(bn, n, omega, alpha, dr_points, dr_weights)
                        B(n) = bn
                  end do
            end select
      end subroutine BVector_AA_Fit


      subroutine VeeLRNumInt_AA_Gauss1(bn, n, omega, alpha, s_points, s_weights)
            real(F64), intent(out)              :: bn
            integer, intent(in)                 :: n
            real(F64), intent(in)               :: omega
            real(F64), intent(in)               :: alpha
            real(F64), dimension(:), intent(in) :: s_points
            real(F64), dimension(:), intent(in) :: s_weights

            real(F64), dimension(NRadialQuad) :: RadialFunction
            real(F64) :: scal

            scal = sqrt(TWO/alpha)
            RadialFunction = erf(omega*scal * s_points) * s_points**(2*n+1)
            bn = sum(RadialFunction * s_weights)
      end subroutine VeeLRNumInt_AA_Gauss1

      
      subroutine VeeLRNumInt_AA_Gauss2(bn, n, omega, alpha, s_points, s_weights)
            real(F64), intent(out)              :: bn
            integer, intent(in)                 :: n
            real(F64), intent(in)               :: omega
            real(F64), intent(in)               :: alpha
            real(F64), dimension(:), intent(in) :: s_points
            real(F64), dimension(:), intent(in) :: s_weights

            real(F64), dimension(NRadialQuad) :: RadialFunction
            real(F64) :: scal

            scal = sqrt(ONE/alpha)
            RadialFunction = erf(omega*scal * s_points) * (ONE + s_points**2) * s_points**(2*n+1)
            bn = sum(RadialFunction * s_weights)
      end subroutine VeeLRNumInt_AA_Gauss2

      
      subroutine VeeLRNumInt_AB_Gauss1(bABmn, m, n, omega, R, alpha, t_points, t_weights, &
            s_points, s_weights)
            
            real(F64), intent(out)              :: bABmn
            integer, intent(in)                 :: m, n
            real(F64), intent(in)               :: omega
            real(F64), intent(in)               :: R
            real(F64), intent(in)               :: alpha
            real(F64), dimension(:), intent(in) :: t_points
            real(F64), dimension(:), intent(in) :: t_weights
            real(F64), dimension(:), intent(in) :: s_points
            real(F64), dimension(:), intent(in) :: s_weights

            integer :: k
            real(F64), dimension(NRadialQuad) :: r12, RadialFunction
            real(F64) :: RadialIntegral, t

            bABmn = ZERO
            do k = 1, NSpherQuad
                  t = t_points(k)
                  r12 = sqrt(TWO/alpha * s_points**2 + TWO*sqrt(TWO/alpha)*R*t * s_points + R**2)
                  RadialFunction = erf(omega * r12) / r12 * s_points**(m+2*n+2)
                  RadialIntegral = sum(RadialFunction * s_weights)
                  bABmn = bABmn + t_weights(k) * t**m * RadialIntegral
            end do

            do k = 1, NSpherQuad
                  t = -t_points(k)
                  r12 = sqrt(TWO/alpha * s_points**2 + TWO*sqrt(TWO/alpha)*R*t * s_points + R**2)
                  RadialFunction = erf(omega * r12) / r12 * s_points**(m+2*n+2)
                  RadialIntegral = sum(RadialFunction * s_weights)
                  bABmn = bABmn + t_weights(k) * t**m * RadialIntegral
            end do
      end subroutine VeeLRNumInt_AB_Gauss1


      subroutine VeeLRNumInt_AB_Gauss2(bABmn, m, n, omega, R, alpha, t_points, t_weights, &
            s_points, s_weights)
            
            real(F64), intent(out)              :: bABmn
            integer, intent(in)                 :: m, n
            real(F64), intent(in)               :: omega
            real(F64), intent(in)               :: R
            real(F64), intent(in)               :: alpha
            real(F64), dimension(:), intent(in) :: t_points
            real(F64), dimension(:), intent(in) :: t_weights
            real(F64), dimension(:), intent(in) :: s_points
            real(F64), dimension(:), intent(in) :: s_weights

            integer :: k
            real(F64), dimension(NRadialQuad) :: r12, RadialFunction
            real(F64) :: RadialIntegral, t, alpha12

            bABmn = ZERO
            alpha12 = sqrt(alpha)
            do k = 1, NSpherQuad
                  t = t_points(k)
                  r12 = sqrt(s_points**2/alpha + TWO/alpha12*R*t * s_points + R**2)
                  RadialFunction = erf(omega * r12) / r12 * s_points**(m+2*n+2) * (ONE + s_points**2)
                  RadialIntegral = sum(RadialFunction * s_weights)
                  bABmn = bABmn + t_weights(k) * t**m * RadialIntegral
            end do

            do k = 1, NSpherQuad
                  t = -t_points(k)
                  r12 = sqrt(s_points**2/alpha + TWO/alpha12*R*t * s_points + R**2)
                  RadialFunction = erf(omega * r12) / r12 * s_points**(m+2*n+2) * (ONE + s_points**2)
                  RadialIntegral = sum(RadialFunction * s_weights)
                  bABmn = bABmn + t_weights(k) * t**m * RadialIntegral
            end do
      end subroutine VeeLRNumInt_AB_Gauss2


      subroutine VeeLRNumInt_AA_Exp(bn, n, omega, alpha, s_points, s_weights)
            real(F64), intent(out)              :: bn
            integer, intent(in)                 :: n
            real(F64), intent(in)               :: omega
            real(F64), intent(in)               :: alpha
            real(F64), dimension(:), intent(in) :: s_points
            real(F64), dimension(:), intent(in) :: s_weights

            real(F64), dimension(NRadialQuad) :: RadialFunction

            RadialFunction = erf(omega/alpha * s_points) * (ONE + s_points) * s_points**(2*n+1)
            bn = sum(RadialFunction * s_weights)
      end subroutine VeeLRNumInt_AA_Exp


      subroutine VeeLRNumInt_AB_Exp(bABmn, m, n, omega, R, alpha, t_points, t_weights, &
            s_points, s_weights)
            
            real(F64), intent(out)              :: bABmn
            integer, intent(in)                 :: m, n
            real(F64), intent(in)               :: omega
            real(F64), intent(in)               :: R
            real(F64), intent(in)               :: alpha
            real(F64), dimension(:), intent(in) :: t_points
            real(F64), dimension(:), intent(in) :: t_weights
            real(F64), dimension(:), intent(in) :: s_points
            real(F64), dimension(:), intent(in) :: s_weights

            integer :: k
            real(F64), dimension(NRadialQuad) :: r12, RadialFunction
            real(F64) :: RadialIntegral, t

            bABmn = ZERO
            do k = 1, NSpherQuad
                  t = t_points(k)
                  r12 = sqrt(ONE/alpha**2*s_points**2 + TWO/alpha*R*t*s_points + R**2)
                  RadialFunction = erf(omega * r12) / r12 * s_points**(m+2*n+2) * (ONE + s_points)
                  RadialIntegral = sum(RadialFunction * s_weights)
                  bABmn = bABmn + t_weights(k) * t**m * RadialIntegral
            end do

            do k = 1, NSpherQuad
                  t = -t_points(k)
                  r12 = sqrt(ONE/alpha**2*s_points**2 + TWO/alpha*R*t*s_points + R**2)
                  RadialFunction = erf(omega * r12) / r12 * s_points**(m+2*n+2) * (ONE + s_points)
                  RadialIntegral = sum(RadialFunction * s_weights)
                  bABmn = bABmn + t_weights(k) * t**m * RadialIntegral
            end do
      end subroutine VeeLRNumInt_AB_Exp
end module MultipoleExpansion
