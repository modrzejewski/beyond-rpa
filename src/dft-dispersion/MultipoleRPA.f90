module MultipoleRPA
      use arithmetic
      use math_constants
      use quadratures
      use MultipoleExpansion
      use Auto2e
      use AutoRPA
      use OverlapIntegrals
      use display
      use string
      use clock
      
      implicit none
      
contains

      subroutine mrpa_preamble(TotNlmk, TotNxyz, NAtom, Lmax, LmaxSpher, &
            Omega, Alpha, NOcc, NVirt)
            integer, intent(in) :: TotNlmk, TotNxyz, Natom, Lmax, LmaxSpher
            real(F64), intent(in) :: Omega, Alpha
            integer, intent(in) :: NOcc, NVirt

            call msg("Multipole Random-Phase Approximation", underline=.true.)
            call msg("Long-range correction for DFT correlation energy")
            call msg("Multipole expansion of Erf(Omega r12)/r12 truncated at Lmax=" // str(Lmax))
            call msg("Least-squares fitting employs the Gaussian weight function exp(-Alpha/2 r**2)")
            call msg("Alpha = " // str(Alpha, d=4))
            call msg("Omega = " // str(Omega, d=4))
            call msg("Spherically-adapted multipoles Slmk=r**(2k)Slm constrained to l <= " // str(LmaxSpher))
            call msg("Number of atoms (including ghost centers): " // str(NAtom))
            call msg("Distributed Cartesian multipoles: " // str(TotNxyz))
            call msg("Distributed spherical multipoles: " // str(TotNlmk))
            call msg("Occupied orbitals: " // str(NOcc))
            call msg("Virtual orbitals: " // str(NVirt))
      end subroutine mrpa_preamble
      

      subroutine mrpa_energy(Ecorr, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, AtomCoords, &
            AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, &
            Exponents, NormFactor, Omega, Lmax, LmaxSpher, LmaxGTO, Alpha, NFreq)

            real(F64), intent(out)                     :: Ecorr
            real(F64), dimension(:, :), intent(in)     :: OccCoeffs
            real(F64), dimension(:, :), intent(in)     :: VirtCoeffs
            real(F64), dimension(:), intent(in)        :: OccEnergies
            real(F64), dimension(:), intent(in)        :: VirtEnergies
            real(F64), dimension(:, :), intent(in)     :: AtomCoords
            integer, dimension(:), intent(in)          :: AtomShellIdx
            integer, dimension(:), intent(in)          :: ShellOrbitalIdx
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:), intent(in)          :: ShellMomentum
            integer, dimension(:), intent(in)          :: NPrimitives
            real(F64), dimension(:, :), intent(in)     :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)     :: Exponents
            real(F64), dimension(:, :), intent(in)     :: NormFactor
            real(F64), intent(in)                      :: Omega
            integer, intent(in)                        :: Lmax
            integer, intent(in)                        :: LmaxSpher
            integer, intent(in)                        :: LmaxGTO
            real(F64), intent(in)                      :: Alpha
            integer, intent(in)                        :: NFreq

            integer :: TotNlmk, TotNxyz
            real(F64), dimension(:, :, :), allocatable :: MDM
            real(F64), dimension(:, :), allocatable :: T, work, PiU
            integer :: Ltransf
            real(F64), dimension(:), allocatable :: Freq, FreqWeight
            integer :: u
            real(F64) :: EcorrU
            logical :: converged
            integer :: RankT, NOcc, NVirt
            integer, parameter :: WeightFunc = MULT_COEFF_GAUSS1
            real(F64), parameter :: RankThresh = 1.0E-6_F64


            !---------------------- debug ---------------------------------
            real(F64), dimension(:, :, :, :), allocatable :: M_Almk_a_i_Debug
            real(F64), dimension(:, :, :, :), allocatable :: VaibjRef
            ! ------------------- end debug -------------------------------

            Ltransf = LmaxSpher + 1
            NAtom = size(AtomCoords, dim=2)
            TotNlmk = NSpherMulti(Lmax, LmaxSpher) * NAtom
            TotNxyz = NCartMulti(Lmax) * NAtom
            NOcc = size(OccCoeffs, dim=2)
            NVirt = size(VirtCoeffs, dim=2)
            call mrpa_preamble(TotNlmk, TotNxyz, NAtom, Lmax, LmaxSpher, &
                  Omega, Alpha, NOcc, NVirt)

            ! ------------------------------ debug -----------------------------
            print *, "*************************************"
            call mrpa_AOIntsError(size(OccCoeffs,dim=1), AtomCoords, AtomShellIdx, &
                  ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, &
                  CntrCoeffs, Exponents, NormFactor, Omega, Lmax, LmaxSpher, &
                  LmaxGTO, Alpha)
            stop
            ! ------------------------------------------------------------------

            
            !
            ! M**T Delta(u) M
            !
            allocate(MDM(TotNlmk, TotNlmk, NFreq))
            allocate(Freq(NFreq))
            allocate(FreqWeight(NFreq))
            call quad_CasimirPolder(Freq, FreqWeight, converged, NFreq, 1.0E-14_F64)
            call mrpa_MDM(MDM, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, AtomCoords, &
                  AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, &
                  Exponents, NormFactor, Freq, Lmax, LmaxSpher, LmaxGTO, M_Almk_a_i_Debug)
            !
            ! T and T**(1/2)
            !
            call msg("Computing multipole interaction matrix T")
            allocate(T(TotNlmk, TotNlmk))
            call MultipoleIntMatrix(T, AtomCoords, Lmax, LmaxSpher, Ltransf, Omega, Alpha, WeightFunc)

            ! -------------------------------------- debug -----------------------------------------------------------

            ! call generate_ovov(VaibjRef, OccCoeffs, VirtCoeffs, AtomCoords, &
            !       AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, &
            !       Exponents, NormFactor, Omega, LmaxGTO)

            ! call check_ovov(VaibjRef, M_Almk_a_i_Debug, TotNlmk, size(VirtCoeffs,dim=2), T)
            ! ---------------------------------------end debug ----------------------------------------------------------
            
            call mrpa_NonsingularSqrt(T, RankT, RankThresh)
            call msg("Computed matrix T of dimension " // str(TotNlmk))
            call msg("Threshold for small eigenvalues of T: " // str(RankThresh, d=3))
            call msg("Discarded " // str(TotNlmk-RankT) // " vectors corresponding to small eigenvalues")
            call msg("Pi(u) has dimension " // str(RankT))
            !
            ! Pi(u) = T**(1/2) M**T Delta(u) M T**(1/2)
            ! Ecorr = 1/(2*Pi) Int(0,Inf) Tr(Log(1-Pi(u))+Pi(u)) du
            !
            call msg("Computing Ecorr by numerical integration (" // str(NFreq) // " quadrature points)")
            allocate(PiU(RankT, RankT))
            allocate(work(RankT, TotNlmk))
            Ecorr = ZERO
            do u = 1, NFreq
                  call real_aTba(PiU, T(:, TotNlmk-RankT+1:TotNlmk), MDM(:, :, u), work)
                  call mrpa_TrLog(EcorrU, PiU)
                  Ecorr = Ecorr + ONE/(TWO*PI) * EcorrU * FreqWeight(u)
            end do
            call msg("Multipole RPA long-range correlation energy: " // str(tokcal(Ecorr), d=3) // " kcal/mol")
            call blankline()
      end subroutine mrpa_energy
      

      subroutine mrpa_MDM(MDM, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, AtomCoords, &
            AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, &
            Exponents, NormFactor, Freq, Lmax, LmaxSpher, LmaxGTO, M_Almk_a_i_Debug)

            real(F64), dimension(:, :, :), intent(out) :: MDM
            real(F64), dimension(:, :), intent(in)     :: OccCoeffs
            real(F64), dimension(:, :), intent(in)     :: VirtCoeffs
            real(F64), dimension(:), intent(in)        :: OccEnergies
            real(F64), dimension(:), intent(in)        :: VirtEnergies
            real(F64), dimension(:, :), intent(in)     :: AtomCoords
            integer, dimension(:), intent(in)          :: AtomShellIdx
            integer, dimension(:), intent(in)          :: ShellOrbitalIdx
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:), intent(in)          :: ShellMomentum
            integer, dimension(:), intent(in)          :: NPrimitives
            real(F64), dimension(:, :), intent(in)     :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)     :: Exponents
            real(F64), dimension(:, :), intent(in)     :: NormFactor
            real(F64), dimension(:), intent(in)        :: Freq
            integer, intent(in)                        :: Lmax
            integer, intent(in)                        :: LmaxSpher
            integer, intent(in)                        :: LmaxGTO
            
            real(F64), dimension(:, :, :, :), allocatable, intent(out) :: M_Almk_a_i_Debug

            integer :: NAtom, NOcc, NVirt
            integer :: i, a, AtomA, AtomB
            integer :: OrbA0, OrbA1, OrbB0, OrbB1, Na, Nb, La, Lb
            integer :: ShA0, ShA1, ShA, ShB0, ShB1, ShB
            integer :: Almk0, Almk1, Blmk0, Blmk1
            integer :: ShellParamsA, ShellParamsB
            integer :: u, q
            integer :: NFreq
            real(F64) :: Dia
            integer :: NlmkPerAtom, NxyzPerAtom
            real(F64), dimension(:, :), allocatable :: M_Axyz_p_q
            real(F64), dimension(:, :), allocatable :: M_Axyz_i_q
            real(F64), dimension(:, :), allocatable :: M_Axyz_i_a
            real(F64), dimension(:, :, :), allocatable :: M_Almk_i_a
            real(F64), dimension(:), allocatable :: Cocc
            real(F64), dimension(:), allocatable :: Cvirt
            real(F64), dimension(:, :), allocatable :: MDMAB
            integer :: MaxNAngFunc
            real(F64), dimension(:), allocatable :: CspherM, CspherT
            integer, dimension(-1:Lmax) :: Plmk, Nlmk, Pxyz, Pc
            integer :: Ltransf
            ! -------------------------------------------------------
            ! Variables used for the Cartesian->spherical multipole
            ! transformation
            ! -------------------------------------------------------
            Ltransf = LmaxSpher + 1
            call NSpherMulti_P(Plmk, Lmax, LmaxSpher)
            call NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            call NCartMulti_P(Pxyz, Lmax)
            call NSpherCoeffs_P(Pc, Lmax, LmaxSpher)
            allocate(CspherT(Pc(Lmax)))
            allocate(CspherM(Pc(Lmax)))
            call SpherCoeffs(CspherT, CspherM, Lmax, LmaxSpher)
            
            call autorpa_init()

            NFreq = size(Freq)
            MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
            allocate(Cocc(MaxNAngFunc))
            allocate(Cvirt(MaxNAngFunc))
            NAtom = size(AtomCoords, dim=2)
            NOcc = size(OccCoeffs, dim=2)
            NVirt = size(VirtCoeffs, dim=2)
            NlmkPerAtom = Plmk(Lmax)
            NxyzPerAtom = Pxyz(Lmax)

            ! ---------------------------- debug ---------------------------------
            allocate(M_Almk_a_i_Debug(NlmkPerAtom, NAtom, NVirt, NOcc))
            ! --------------------------------------------------------------------

            
            allocate(M_Axyz_p_q(NxyzPerAtom, MaxNAngFunc**2))
            allocate(M_Axyz_i_q(NxyzPerAtom, MaxNAngFunc))
            allocate(M_Axyz_i_a(NxyzPerAtom, NVirt))
            allocate(M_Almk_i_a(NlmkPerAtom, NVirt, NAtom))
            allocate(MDMAB(NlmkPerAtom, NlmkPerAtom))
            MDM = ZERO
            do i = 1, NOcc
                  AtA: do AtomA = 1, NAtom
                        ShA0 = AtomShellIdx(AtomA)
                        ShA1 = AtomShellIdx(AtomA+1) - 1
                        M_Axyz_i_a = ZERO
                        ShellA: do ShA = ShA0, ShA1
                              ShellParamsA = ShellParamsIdx(ShA)
                              La = ShellMomentum(ShellParamsA)
                              Na = ((La + 1) * (La + 2)) / 2
                              OrbA0 = ShellOrbitalIdx(ShA)
                              OrbA1 = OrbA0 + Na - 1
                              M_Axyz_i_q = ZERO
                              AtB: do AtomB = 1, NAtom
                                    ShB0 = AtomShellIdx(AtomB)
                                    ShB1 = AtomShellIdx(AtomB+1) - 1
                                    ShellB: do ShB = ShB0, ShB1
                                          ShellParamsB = ShellParamsIdx(ShB)
                                          Lb = ShellMomentum(ShellParamsB)
                                          Nb = ((Lb + 1) * (Lb + 2)) / 2                                          
                                          OrbB0 = ShellOrbitalIdx(ShB)
                                          OrbB1 = OrbB0 + Nb - 1
                                          
                                          call AutoRPACartMulti(autorpa_idx(Lb, La))%ptr(M_Axyz_p_q, &
                                                AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                                                Exponents(:, ShellParamsB), NPrimitives(ShellParamsB), &
                                                AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                                                Exponents(:, ShellParamsA), NPrimitives(ShellParamsA))
                                          
                                          Cocc(1:Nb) = NormFactor(1:Nb, ShellParamsB) * OccCoeffs(OrbB0:OrbB1, i)
                                          call mrpa_OccIndexTransform(M_Axyz_i_q, M_Axyz_p_q, Cocc, Na, Nb, NxyzPerAtom)
                                    end do ShellB
                              end do AtB
                              
                              do a = 1, NVirt
                                    Cvirt(1:Na) = NormFactor(1:Na, ShellParamsA) * VirtCoeffs(OrbA0:OrbA1, a)
                                    do q = 1, Na
                                          M_Axyz_i_a(:, a) = M_Axyz_i_a(:, a) + Cvirt(q) * M_Axyz_i_q(:, q)
                                    end do
                              end do
                        end do ShellA
                        !
                        ! Cartesian -> spherical transformation of multpole moment integrals
                        !
                        call transf_Vlmk_Vxyz(M_Almk_i_a(:, :, AtomA), M_Axyz_i_a, &
                              CspherM, Lmax, Ltransf, Plmk, Nlmk, Pxyz, Pc, NVirt)
                  end do AtA


                  ! ---------------------------- debug ------------------------------------

                  do a = 1, Nvirt
                        do AtomA = 1, NAtom
                              M_Almk_a_i_Debug(:, AtomA, a, i) = &
                                    M_Almk_i_a(:, a, AtomA)
                        end do
                  end do


                  ! ---------------------------- end debug ---------------------------------

                  do u = 1, NFreq
                        do AtomB = 1, NAtom
                              do AtomA = 1, NAtom
                                    MDMAB = ZERO
                                    do a = 1, NVirt
                                          Dia = FOUR * (OccEnergies(i) - VirtEnergies(a)) &
                                                / ((OccEnergies(i) - VirtEnergies(a))**2 + Freq(u)**2)
                                          call real_vwT(MDMAB, M_Almk_i_a(:, a, AtomA), &
                                                M_Almk_i_a(:, a, AtomB), Dia)
                                    end do
                                    Almk0 = NlmkPerAtom * (AtomA - 1) + 1
                                    Almk1 = NlmkPerAtom * AtomA
                                    Blmk0 = NlmkPerAtom * (AtomB - 1) + 1
                                    Blmk1 = NlmkPerAtom * AtomB
                                    MDM(Almk0:Almk1, Blmk0:Blmk1, u) = MDM(Almk0:Almk1, Blmk0:Blmk1, u) + MDMAB
                              end do
                        end do
                  end do
            end do
      end subroutine mrpa_MDM


      subroutine mrpa_OccIndexTransform(M_Axyz_i_q, M_Axyz_p_q, Cocc, Na, Nb, NxyzPerAtom)
            real(F64), dimension(NxyzPerAtom, *), intent(inout)  :: M_Axyz_i_q
            real(F64), dimension(NxyzPerAtom, Nb, *), intent(in) :: M_Axyz_p_q
            real(F64), dimension(*), intent(in)                  :: Cocc
            integer, intent(in)                                  :: Na
            integer, intent(in)                                  :: Nb
            integer, intent(in)                                  :: NxyzPerAtom

            integer :: p, q
            
            do q = 1, Na
                  do p = 1, Nb
                        M_Axyz_i_q(:, q) = M_Axyz_i_q(:, q) + Cocc(p) * M_Axyz_p_q(:, p, q)
                  end do
            end do
      end subroutine mrpa_OccIndexTransform


      subroutine mrpa_NonsingularSqrt(T, RankT, thresh)
            real(F64), dimension(:, :), intent(inout) :: T
            integer, intent(out)                      :: RankT
            real(F64), intent(in)                     :: thresh

            real(F64), dimension(:), allocatable :: w
            integer :: n, k

            n = size(T, dim=1)
            allocate(w(n))
            call symmetric_eigenproblem(w, T, n, .true.)
            RankT = n
            SmallEvals: do k = 1, n
                  if (w(k) < thresh) then
                        RankT = RankT - 1
                  else
                        exit SmallEvals
                  end if
            end do SmallEvals
            do k = n-RankT+1, n
                  T(:, k) = sqrt(w(k)) * T(:, k)
            end do
      end subroutine mrpa_NonsingularSqrt


      subroutine mrpa_LowRankApprox(LowRankV, V, thresh)
            real(F64), dimension(:, :), allocatable, intent(out) :: LowRankV
            real(F64), dimension(:, :), intent(inout)            :: V
            real(F64), intent(in)                                :: thresh
            
            real(F64), dimension(:), allocatable :: w
            integer :: n, k, l, ApproxRank

            n = size(V, dim=1)
            allocate(w(n))
            call symmetric_eigenproblem(w, V, n, .true.)
            ApproxRank = n
            SmallEvals: do k = 1, n
                  if (w(k) < thresh) then
                        ApproxRank = ApproxRank - 1
                  else
                        exit SmallEvals
                  end if
            end do SmallEvals
            allocate(LowRankV(n, ApproxRank))
            do k = 1, ApproxRank
                  l = n - ApproxRank + k
                  LowRankV(:, k) = sqrt(w(l)) * V(:, l)
            end do
      end subroutine mrpa_LowRankApprox

      

      ! subroutine mrpa_SquareRoot(T, work)
      !       real(F64), dimension(:, :), intent(inout) :: T
      !       real(F64), dimension(:, :), intent(out)   :: work

      !       real(F64), dimension(:), allocatable :: w
      !       integer :: n, k

      !       n = size(T, dim=1)
      !       allocate(w(n))
      !       call symmetric_eigenproblem(w, T, n, .true.)
      !       work = ZERO
      !       do k = 1, n
      !             if (w(k) < 1.0E-5_F64) then
      !                   print *, "eigenvalue of T lower than zero: ", w(k)
      !                   w(k) = ZERO
      !             end if
                        
      !             call real_vwT(work, T(:, k), T(:, k), sqrt(w(k)))
      !       end do
      !       T = work
      ! end subroutine mrpa_SquareRoot
      

      subroutine mrpa_TrLog(Tr, PiU)
            !
            ! Compute Tr(Log(1 - Pi(u)) + Pi(u))
            !
            real(F64), intent(out)                    :: Tr
            real(F64), dimension(:, :), intent(inout) :: PiU

            real(F64) :: Tr1, Tr2
            real(F64), dimension(:), allocatable :: w
            integer :: n, k

            n = size(PiU, dim=1)
            !
            ! Tr(Pi(u))
            !
            Tr1 = ZERO
            do k = 1, n
                  Tr1 = Tr1 + PiU(k, k)
            end do
            !
            ! Tr(Log(1-Pi(u)))
            !
            allocate(w(n))            
            PiU = -PiU
            do k = 1, n
                  PiU(k, k) = ONE + PiU(k, k)
            end do
            call symmetric_eigenproblem(w, PiU, n, .false.)
            Tr2 = ZERO
            do k = 1, n
                  Tr2 = Tr2 + log(w(k))
            end do
            Tr = Tr1 + Tr2
      end subroutine mrpa_TrLog


      subroutine check_PiU(PiU)
            real(F64), dimension(:, :), intent(in) :: PiU

            real(F64), dimension(:, :), allocatable :: work
            real(F64), dimension(:), allocatable :: eigenvals
            integer :: n
            integer :: k

            print *, "================= checking matrix Pi(u) =================="
            n = size(PiU, dim=1)
            allocate(work(n, n))
            allocate(eigenvals(n))
            work = PiU
            call symmetric_eigenproblem(eigenvals, work, n, .true.)
            do k = 1, n
                  print *, "w(" // str(k) // ")=" // str(eigenvals(k))
            end do
            print *, "=========================================================="
      end subroutine check_PiU


      subroutine generate_ovov(VaibjRef, OccCoeffs, VirtCoeffs, AtomCoords, &
            AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, &
            Exponents, NormFactor, Omega, LmaxGTO)

            real(F64), dimension(:, :), intent(in)     :: OccCoeffs
            real(F64), dimension(:, :), intent(in)     :: VirtCoeffs
            real(F64), dimension(:, :), intent(in)     :: AtomCoords
            integer, dimension(:), intent(in)          :: AtomShellIdx
            integer, dimension(:), intent(in)          :: ShellOrbitalIdx
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:), intent(in)          :: ShellMomentum
            integer, dimension(:), intent(in)          :: NPrimitives
            real(F64), dimension(:, :), intent(in)     :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)     :: Exponents
            real(F64), dimension(:, :), intent(in)     :: NormFactor
            real(F64), intent(in)                      :: Omega
            integer, intent(in)                        :: LmaxGTO

            real(F64), dimension(:, :, :, :), allocatable, intent(out) :: VaibjRef
            integer :: NOcc, NVirt, NAtom
            integer :: i, a, j, b, idx
            integer :: p, q, r, s
            integer :: AtomA, AtomB, ShA, ShB, ShA0, ShA1, ShB0, ShB1, Lb, Nb, La, Na
            integer :: AtomC, AtomD, ShC, ShD, ShC0, ShC1, ShD0, ShD1, Lc, Nc, Ld, Nd
            integer :: OrbA0, OrbA1, OrbB0, OrbB1, ShellParamsA, ShellParamsB, OrbC0, OrbC1, ShellParamsC
            integer :: OrbD0, OrbD1, ShellParamsD
            real(F64), dimension(:), allocatable :: CoccI, CoccJ, CvirtA, CvirtB
            real(F64), dimension(:), allocatable :: Gpqrs
            integer :: MaxNAngFunc

            print *, "=========== generating (ai|bj) integrals ======================"
            
            NOcc = size(OccCoeffs, dim=2)
            NVirt = size(VirtCoeffs, dim=2)
            NAtom = size(AtomCoords, dim=2)
            MaxNAngFunc = ((LmaxGTO+1)*(LmaxGTO+2))/2
            allocate(VaibjRef(NVirt, NOcc, NVirt, NOcc))
            allocate(CoccI(MaxNAngFunc))
            allocate(CoccJ(MaxNAngFunc))
            allocate(CvirtA(MaxNAngFunc))
            allocate(CvirtB(MaxNAngFunc))
            allocate(Gpqrs(MaxnAngFunc**4))
            VaibjRef = ZERO

            print *, "allocated matrices"
            
            AtA: do AtomA = 1, NAtom
                  ShA0 = AtomShellIdx(AtomA)
                  ShA1 = AtomShellIdx(AtomA+1) - 1
                  ShellA: do ShA = ShA0, ShA1
                        ShellParamsA = ShellParamsIdx(ShA)
                        La = ShellMomentum(ShellParamsA)
                        Na = ((La + 1) * (La + 2)) / 2
                        OrbA0 = ShellOrbitalIdx(ShA)
                        OrbA1 = OrbA0 + Na - 1
                        AtB: do AtomB = 1, NAtom
                              ShB0 = AtomShellIdx(AtomB)
                              ShB1 = AtomShellIdx(AtomB+1) - 1
                              ShellB: do ShB = ShB0, ShB1
                                    ShellParamsB = ShellParamsIdx(ShB)
                                    Lb = ShellMomentum(ShellParamsB)
                                    Nb = ((Lb + 1) * (Lb + 2)) / 2                                          
                                    OrbB0 = ShellOrbitalIdx(ShB)
                                    OrbB1 = OrbB0 + Nb - 1
                                    AtC: do AtomC = 1, NAtom
                                          ShC0 = AtomShellIdx(AtomC)
                                          ShC1 = AtomShellIdx(AtomC+1) - 1
                                          ShellC: do ShC = ShC0, ShC1
                                                ShellParamsC = ShellParamsIdx(ShC)
                                                Lc = ShellMomentum(ShellParamsC)
                                                Nc = ((Lc + 1) * (Lc + 2)) / 2                                          
                                                OrbC0 = ShellOrbitalIdx(ShC)
                                                OrbC1 = OrbC0 + Nc - 1
                                                AtD: do AtomD = 1, NAtom
                                                      ShD0 = AtomShellIdx(AtomD)
                                                      ShD1 = AtomShellIdx(AtomD+1) - 1
                                                      ShellD: do ShD = ShD0, ShD1
                                                            ShellParamsD = ShellParamsIdx(ShD)
                                                            Ld = ShellMomentum(ShellParamsD)
                                                            Nd = ((Ld + 1) * (Ld + 2)) / 2                                          
                                                            OrbD0 = ShellOrbitalIdx(ShD)
                                                            OrbD1 = OrbD0 + Nd - 1


                                                            call Auto2eERI(auto2e_idx(La, Lb, Lc, Ld))%ptr(&
                                                                  Gpqrs, &
                                                                  !
                                                                  ! ShellA
                                                                  !
                                                                  AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                                                                  NormFactor(:, ShellParamsA), Exponents(:, ShellParamsA), &
                                                                  NPrimitives(ShellParamsA), &
                                                                  !
                                                                  ! ShellB
                                                                  !
                                                                  AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                                                                  NormFactor(:, ShellParamsB), Exponents(:, ShellParamsB), &
                                                                  NPrimitives(ShellParamsB), &
                                                                  !
                                                                  ! ShellC
                                                                  !
                                                                  AtomCoords(:, AtomC), CntrCoeffs(:, ShellParamsC), &
                                                                  NormFactor(:, ShellParamsC), Exponents(:, ShellParamsC), &
                                                                  NPrimitives(ShellParamsC), &
                                                                  !
                                                                  ! ShellD
                                                                  !
                                                                  AtomCoords(:, AtomD), CntrCoeffs(:, ShellParamsD), &
                                                                  NormFactor(:, ShellParamsD), Exponents(:, ShellParamsD), &
                                                                  NPrimitives(ShellParamsD), &
                                                                  ONE/Omega**2)


                                                            do j = 1, NOcc
                                                                  CoccJ(1:Na) = &
                                                                        OccCoeffs(OrbA0:OrbA1, j)
                                                                  do b = 1, NVirt
                                                                        CvirtB(1:Nb) = &
                                                                              VirtCoeffs(OrbB0:OrbB1, b)
                                                                        do i = 1, NOcc
                                                                              CoccI(1:Nc) = &
                                                                                    OccCoeffs(OrbC0:OrbC1, i)
                                                                              do a = 1, NVirt
                                                                                    CvirtA(1:Nd) = &
                                                                                          VirtCoeffs(OrbD0:OrbD1, a)
                                                                                    idx = 1
                                                                                    do p = 1, Na
                                                                                          do q = 1, Nb
                                                                                                do r = 1, Nc
                                                                                                      do s = 1, Nd
                                                                       VaibjRef(a, i, b, j) = &
                                                                             VaibjRef(a, i, b, j) + &
                                                                             CoccJ(p)*CvirtB(q)*CoccI(r)*CvirtA(s) &
                                                                             * Gpqrs(idx)
                                                                       idx = idx + 1
                                                                                                      end do
                                                                                                end do
                                                                                          end do
                                                                                    end do
                                                                                    
                                                                              end do
                                                                        end do
                                                                  end do
                                                            end do
                                                      end do ShellD
                                                end do AtD
                                          end do ShellC
                                    end do AtC
                              end do ShellB
                        end do AtB
                  end do ShellA
            end do AtA
      end subroutine generate_ovov


      subroutine check_ovov(VaibjRef, M_Almk_a_i, ldM, NVirt, T)
            real(F64), dimension(:, :, :, :), intent(in) :: VaibjRef
            real(F64), dimension(ldM, NVirt, *), intent(in) :: M_Almk_a_i
            real(F64), dimension(:, :), intent(in)       :: T
            integer, intent(in) :: ldM, NVirt

            integer :: NOcc
            integer :: a, i, b, j, q
            real(F64) :: Vapprox
            real(F64), dimension(:), allocatable :: TM

            print *, "============== checking (ai|bj) integrals ======================"
            allocate(TM(ldM))
            NOcc = size(VaibjRef, dim=2)
            do j = 1, NOcc
                  do b = 1, NVirt
                        do i = 1, NOcc
                              do a = 1, NVirt
                                    TM = ZERO
                                    do q = 1, ldM
                                          TM = TM + M_Almk_a_i(q, b, j) * T(:, q)
                                    end do
                                    Vapprox = dot_product(M_Almk_a_i(:, a, i), TM)
                                    if (abs(VaibjRef(a,i,b,j))>1.0E-6_F64) then
                                          print *, str(a) // str(i) // str(b) // str(j) // &
                                                "   " // str(Vapprox,d=3) // "   " // str(VaibjRef(a, i, b, j),d=3)
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine check_ovov


      subroutine mrpa_AOIntsError(NOrbAO, AtomCoords, AtomShellIdx, &
            ShellOrbitalIdx, ShellParamsIdx, ShellMomentum, NPrimitives, &
            CntrCoeffs, Exponents, NormFactor, Omega, Lmax, LmaxSpher, &
            LmaxGTO, Alpha)

            integer, intent(in)                        :: NOrbAO
            real(F64), dimension(:, :), intent(in)     :: AtomCoords
            integer, dimension(:), intent(in)          :: AtomShellIdx
            integer, dimension(:), intent(in)          :: ShellOrbitalIdx
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:), intent(in)          :: ShellMomentum
            integer, dimension(:), intent(in)          :: NPrimitives
            real(F64), dimension(:, :), intent(in)     :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)     :: Exponents
            real(F64), dimension(:, :), intent(in)     :: NormFactor
            real(F64), intent(in)                      :: Omega
            integer, intent(in)                        :: Lmax
            integer, intent(in)                        :: LmaxSpher
            integer, intent(in)                        :: LmaxGTO
            real(F64), intent(in)                      :: Alpha

            real(F64), dimension(:, :), allocatable :: Mlmk, T, Tab
            real(F64), dimension(:, :), allocatable :: Vexact, Vmult
            real(F64), dimension(:), allocatable :: GabExact, GabMult
            real(F64), dimension(:), allocatable :: CspherT, CspherM
            real(F64), dimension(:), allocatable :: TransfWork
            integer, dimension(-1:Lmax) :: Plmk, Nlmk, Pxyz, Pc
            integer :: NAtoms, NlmkPerAtom, TotNlmk, Ltransf, MaxNAngFunc
            integer :: AtomA, ShA, ShA0, ShA1, La, Na, OrbA0, OrbA1, ShellParamsA
            integer :: AtomB, ShB, ShB0, ShB1, Lb, Nb, OrbB0, OrbB1, ShellParamsB
            integer :: lmkA0, lmkA1, lmkB0, lmkB1
            real(F64) :: Kappa
            real(F64) :: TimeV
            logical :: ComputeExact
            integer :: ApproxRankMulti
            integer :: ApproxRankExact
            real(F64) :: RMSEexact, RMSEmulti, MaxAbsErrorMulti, MaxAbsErrorDirect
            real(F64), dimension(:, :), allocatable :: LowRankT
            real(F64), dimension(:, :), allocatable :: LowRankVmulti
            real(F64), dimension(:, :), allocatable :: LowRankVexact
            real(F64), dimension(:, :), allocatable :: Vref
            !
            ! Dummy S function
            !
            integer, parameter :: Ldummy = 0
            integer, parameter :: NprimDummy = 1
            real(F64), dimension(((Ldummy+1)*(Ldummy+2))/2), parameter :: NormDummy = [1.0_F64]
            real(F64), dimension(NprimDummy), parameter :: CntrDummy = [1.0_F64]
            real(F64), dimension(NprimDummy), parameter :: ExpDummy = [0.0_F64]
            real(F64), dimension(3), parameter :: RDummy = [0.0_F64, 0.0_F64, 0.0_F64]
            integer :: p, q
            real(F64), parameter :: LowRankThresh = 1.0E-6_F64
            integer, parameter :: WeightFunc = MULT_COEFF_GAUSS1
            type(tclock) :: time
            
            Kappa = ONE / Omega**2
            Ltransf = LmaxSpher + 1
            call NSpherCoeffs_P(Pc, Lmax, LmaxSpher)
            allocate(CspherT(Pc(Lmax)))
            allocate(CspherM(Pc(Lmax)))
            call SpherCoeffs(CspherT, CspherM, Lmax, LmaxSpher)
            call NSpherMulti_P(Plmk, Lmax, LmaxSpher)
            call NSpherMulti_N(Nlmk, Lmax, LmaxSpher)
            call NCartMulti_P(Pxyz, Lmax)
            NlmkPerAtom = Plmk(Lmax)
            NAtoms = size(AtomCoords, dim=2)
            TotNlmk = NlmkPerAtom * NAtoms
            allocate(T(TotNlmk, TotNlmk))
            allocate(Tab(NlmkPerAtom, NlmkPerAtom))
            allocate(Mlmk(NlmkPerAtom, NOrbAO))
            MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
            allocate(GabExact(MaxNAngFunc**2))
            allocate(GabMult(MaxNAngFunc**2))
            allocate(Vexact(NOrbAO, NOrbAO))
            allocate(Vref(NOrbAO, NOrbAO))
            allocate(Vmult(NOrbAO, NOrbAO))
            allocate(TransfWork(MaxNAngFunc*NlmkPerAtom))


            ComputeExact = .true.
            call msg("Starting computation of two-center Coulomb integrals")
            call clock_start(time)            
            if (.not. ComputeExact) then
                  call mrpa_Mlmk(Mlmk, NAtoms, AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, &
                        ShellMomentum, NPrimitives, CntrCoeffs, Exponents, NormFactor, Lmax, &
                        Ltransf, LmaxGTO, CspherM, Plmk, Nlmk, Pxyz, Pc)
                  call MultipoleIntMatrix(T, AtomCoords, Lmax, LmaxSpher, Ltransf, &
                        Omega, Alpha, WeightFunc)
            end if
            AtB: do AtomB = 1, NAtom
                  ShB0 = AtomShellIdx(AtomB)
                  ShB1 = AtomShellIdx(AtomB+1) - 1
                  lmkB0 = NlmkPerAtom * (AtomB - 1) + 1
                  lmkB1 = NlmkPerAtom * AtomB
                  ShellB: do ShB = ShB0, ShB1
                        ShellParamsB = ShellParamsIdx(ShB)
                        Lb = ShellMomentum(ShellParamsB)
                        Nb = ((Lb + 1) * (Lb + 2)) / 2                                          
                        OrbB0 = ShellOrbitalIdx(ShB)
                        OrbB1 = OrbB0 + Nb - 1
                        AtA: do AtomA = 1, NAtom
                              ShA0 = AtomShellIdx(AtomA)
                              ShA1 = AtomShellIdx(AtomA+1) - 1
                              lmkA0 = NlmkPerAtom * (AtomA - 1) + 1
                              lmkA1 = NlmkPerAtom * AtomA
                              ShellA: do ShA = ShA0, ShA1
                                    ShellParamsA = ShellParamsIdx(ShA)
                                    La = ShellMomentum(ShellParamsA)
                                    Na = ((La + 1) * (La + 2)) / 2
                                    OrbA0 = ShellOrbitalIdx(ShA)
                                    OrbA1 = OrbA0 + Na - 1
                                    if (La >= 4 .and. Lb >= 4) then
                                    if (ComputeExact) then
                                          call Auto2eERI(auto2e_idx(Lb, LDummy, La, LDummy))%ptr(GabExact, &
                                                !
                                                ! Shell B
                                                !
                                                AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                                                NormFactor(:, ShellParamsB), Exponents(:, ShellParamsB), &
                                                NPrimitives(ShellParamsB), &
                                                !
                                                ! Dummy 1s function
                                                !
                                                RDummy, CntrDummy, NormDummy, ExpDummy, NprimDummy, &
                                                !
                                                ! Shell A
                                                !
                                                AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                                                NormFactor(:, ShellParamsA), Exponents(:, ShellParamsA), &
                                                NPrimitives(ShellParamsA), &
                                                !
                                                ! Dummy 1s function
                                                !
                                                RDummy, CntrDummy, NormDummy, ExpDummy, NprimDummy, &
                                                Kappa)
                                          call copy_AOBlock(Vref(OrbA0:OrbA1, OrbB0:OrbB1), GabExact, Na, Nb)
                                    else
                                          Tab = T(lmkA0:lmkA1, lmkB0:lmkB1)
                                          call real_aTb_x(TransfWork, Na, Mlmk(:, OrbA0:OrbA1), NlmkPerAtom, &
                                                Tab, NlmkPerAtom, Na, NlmkPerAtom, NlmkPerAtom)
                                          call real_ab_x(GabMult, Na, TransfWork, Na, Mlmk(:, OrbB0:OrbB1), &
                                                NlmkPerAtom, Na, Nb, NlmkPerAtom)
                                          call copy_AOBlock(Vmult(OrbA0:OrbA1, OrbB0:OrbB1), GabMult, Na, Nb)
                                    end if
                              end if
                              end do ShellA
                        end do AtA
                  end do ShellB
            end do AtB
            TimeV = clock_readwall(time)
            call msg("Two-center Coulomb integrals computed in " // str(TimeV) // " seconds")
            
            Vexact = Vref
            call mrpa_LowRankApprox(LowRankVexact, Vexact, LowRankThresh)
            call real_abT(Vexact, LowRankVexact, LowRankVexact)
            call mrpa_LowRankApprox(LowRankT, T, LowRankThresh)
            call real_abT(T, LowRankT, LowRankT)
            do AtomB = 1, NAtom
                  ShB0 = AtomShellIdx(AtomB)
                  ShB1 = AtomShellIdx(AtomB+1) - 1
                  lmkB0 = NlmkPerAtom * (AtomB - 1) + 1
                  lmkB1 = NlmkPerAtom * AtomB
                  do ShB = ShB0, ShB1
                        ShellParamsB = ShellParamsIdx(ShB)
                        Lb = ShellMomentum(ShellParamsB)
                        Nb = ((Lb + 1) * (Lb + 2)) / 2                                          
                        OrbB0 = ShellOrbitalIdx(ShB)
                        OrbB1 = OrbB0 + Nb - 1
                        do AtomA = 1, NAtom
                              ShA0 = AtomShellIdx(AtomA)
                              ShA1 = AtomShellIdx(AtomA+1) - 1
                              lmkA0 = NlmkPerAtom * (AtomA - 1) + 1
                              lmkA1 = NlmkPerAtom * AtomA
                              do ShA = ShA0, ShA1
                                    ShellParamsA = ShellParamsIdx(ShA)
                                    La = ShellMomentum(ShellParamsA)
                                    Na = ((La + 1) * (La + 2)) / 2
                                    OrbA0 = ShellOrbitalIdx(ShA)
                                    OrbA1 = OrbA0 + Na - 1

                                    Tab = T(lmkA0:lmkA1, lmkB0:lmkB1)
                                    call real_aTb_x(TransfWork, Na, Mlmk(:, OrbA0:OrbA1), NlmkPerAtom, &
                                          Tab, NlmkPerAtom, Na, NlmkPerAtom, NlmkPerAtom)
                                    call real_ab_x(GabMult, Na, TransfWork, Na, Mlmk(:, OrbB0:OrbB1), &
                                          NlmkPerAtom, Na, Nb, NlmkPerAtom)
                                    call copy_AOBlock(Vmult(OrbA0:OrbA1, OrbB0:OrbB1), GabMult, Na, Nb)
                              end do
                        end do
                  end do
            end do

            RMSEexact = ZERO
            RMSEmulti = ZERO
            MaxAbsErrorDirect = ZERO
            MaxAbsErrorMulti = ZERO
            do q = 1, NOrbAO
                  do p = 1, NOrbAO
                        RMSEexact = RMSEexact + (Vexact(p, q) - Vref(p, q))**2
                        RMSEmulti = RMSEmulti + (Vmult(p, q) - Vref(p, q))**2
                        MaxAbsErrorDirect = max(abs(Vexact(p, q) - Vref(p, q)), MaxAbsErrorDirect)
                        MaxAbsErrorMulti = max(abs(Vmult(p, q) - Vref(p, q)), MaxAbsErrorMulti)
                  end do
            end do
            RMSEexact = Sqrt(RMSEexact)
            RMSEmulti = Sqrt(RMSEmulti)
            call msg("Threshold for small singular values: " // str(LowRankThresh, d=3))
            call msg("Dimension of LowRankT: " // str(size(LowRankT, dim=2)))
            call msg("Dimension of LowRankVdirect: " // str(size(LowRankVexact, dim=2)))
            call msg("||Vmulti - Vexact||F = " // str(RMSEmulti))
            call msg("||Vdirect - Vexact||F = " // str(RMSEexact))
            call msg("||Vmulti - Vexact||max = " // str(MaxAbsErrorMulti))
            call msg("||Vdirect - Vexact||max = " // str(MaxAbsErrorDirect))
      end subroutine mrpa_AOIntsError


      subroutine copy_AOBlock(V, Gab, Na, Nb)
            real(F64), dimension(:, :), intent(out) :: V
            real(F64), dimension(Na, *), intent(in) :: Gab
            integer, intent(in)                     :: Na, Nb
            V = Gab(:, 1:Nb)
      end subroutine copy_AOBlock

      
      subroutine mrpa_Mlmk(Mlmk, NAtoms, AtomShellIdx, ShellOrbitalIdx, ShellParamsIdx, &
            ShellMomentum, NPrimitives, CntrCoeffs, Exponents, NormFactor, Lmax, &
            Ltransf, LmaxGTO, CspherM, Plmk, Nlmk, Pxyz, Pc)

            real(F64), dimension(:, :), intent(out)    :: Mlmk
            integer, intent(in)                        :: NAtoms
            integer, dimension(:), intent(in)          :: AtomShellIdx
            integer, dimension(:), intent(in)          :: ShellOrbitalIdx
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:), intent(in)          :: ShellMomentum
            integer, dimension(:), intent(in)          :: NPrimitives
            real(F64), dimension(:, :), intent(in)     :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)     :: Exponents
            real(F64), dimension(:, :), intent(in)     :: NormFactor
            integer, intent(in)                        :: Lmax
            integer, intent(in)                        :: Ltransf
            integer, intent(in)                        :: LmaxGTO
            real(F64), dimension(:), intent(in)        :: CspherM
            integer, dimension(-1:), intent(in)        :: Plmk, Nlmk, Pxyz, Pc

            integer :: NxyzPerAtom, MaxNAngFunc
            integer :: a, ax, ay, az
            integer :: AtomA, ShA, ShA0, ShA1, La, Na, OrbA0, OrbA1
            integer :: ShellParamsA
            real(F64), dimension(:, :), allocatable :: Mxyz

            NxyzPerAtom = Pxyz(Lmax)
            MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
            allocate(Mxyz(NxyzPerAtom, MaxNAngFunc))
            
            do AtomA = 1, NAtoms
                  ShA0 = AtomShellIdx(AtomA)
                  ShA1 = AtomShellIdx(AtomA+1) - 1
                  do ShA = ShA0, ShA1
                        ShellParamsA = ShellParamsIdx(ShA)
                        La = ShellMomentum(ShellParamsA)
                        Na = ((La + 1) * (La + 2)) / 2
                        OrbA0 = ShellOrbitalIdx(ShA)
                        OrbA1 = OrbA0 + Na - 1
                        a = 1
                        do ax = La, 0, -1
                              do ay = La-ax, 0, -1
                                    az = La - ax - ay
                                    call CartMultipoles(Mxyz(:, a), Lmax, ax, ay, az, &
                                          CntrCoeffs(:, ShellParamsA), NormFactor(a, ShellParamsA), &
                                          Exponents(:, ShellParamsA), NPrimitives(ShellParamsA))
                                    a = a + 1
                              end do
                        end do
                        call transf_Vlmk_Vxyz(Mlmk(:, OrbA0:OrbA1), Mxyz(:, 1:Na), CspherM, Lmax, &
                              Ltransf, Plmk, Nlmk, Pxyz, Pc, Na)
                  end do
            end do
      end subroutine mrpa_Mlmk
end module MultipoleRPA
