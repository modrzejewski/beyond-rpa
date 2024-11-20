module rpa_HF
      use arithmetic
      use math_constants
      use real_linalg
      use TwoStepCholesky
      use OneElectronInts
      use rpa_definitions
      use TwoStepCholesky_definitions
      use sys_definitions
      use basis_sets
      use clock
      use string
      use rpa_Cholesky

      implicit none

contains

      subroutine rpa_HF_Dpqk(Dpqk, DpqkLoc, NOcc, NSpins, NSystems, SCFOutput, AOBasis)
            !
            ! Gather the 1-electron reduced density matrices of the total system and
            ! all of its subsystems into the Dpqk array. For example, for a molecular
            ! trimer ABC, the Dpqk array contains 1-RDMs for ABC, AB, BC, AC, A, B,
            ! and C. The Dpqk array is required for a batch computation of the Fock matrix
            ! for all systems at once.
            !
            real(F64), dimension(:), allocatable, intent(out)       :: Dpqk
            integer, dimension(:, :), allocatable, intent(out)      :: DpqkLoc
            integer, dimension(:, :), intent(out)                   :: NOcc
            integer, dimension(:), intent(out)                      :: NSpins
            integer, intent(in)                                     :: NSystems
            type(TSCFOutput), dimension(:), intent(in)              :: SCFOutput
            type(TAOBasis), intent(in)                              :: AOBasis

            integer :: NAO, NDensities
            integer :: k, s, l
            integer :: i0, i1
            integer :: MaxNOcc
            real(F64), dimension(:, :), allocatable :: Cpi
            real(F64), dimension(:, :), allocatable :: Dpq

            NAO = AOBasis%NAOSpher
            do k = 1, NSystems                  
                  NSpins(k) = size(SCFOutput(k)%C_oao, dim=3)
                  NOcc(:, k) = SCFOutput(k)%NOcc(:)
            end do
            MaxNOcc = maxval(NOcc)            
            NDensities = sum(NSpins)
            allocate(Cpi(NAO, MaxNOcc))
            allocate(Dpq(NAO, NAO))
            allocate(Dpqk(NAO**2*NDensities))
            allocate(DpqkLoc(AOBasis%NShells, AOBasis%NShells))
            call rpa_HF_DpqkLoc(DpqkLoc, NDensities, AOBasis)
            l = 0
            do k = 1, NSystems
                  do s = 1, NSpins(k)
                        l = l + 1
                        if (NSpins(k) > 0) then
                              i0 = 1
                              i1 = NOcc(s, k)
                              call real_ab(Cpi(:, i0:i1), SCFOutput(k)%MOBasisVecsSpher, &
                                    SCFOutput(k)%C_oao(:, i0:i1, s))
                              call real_abT(Dpq, Cpi(:, i0:i1), Cpi(:, i0:i1))
                        else
                              Dpq = ZERO
                        end if
                        call rpa_HF_UpdateDpqk(Dpqk, l, Dpq, DpqkLoc, AOBasis)
                  end do
            end do
      end subroutine rpa_HF_Dpqk
      

      subroutine rpa_HF_DpqkLoc(DpqkLoc, NDensities, AOBasis)
            integer, dimension(:, :), intent(out) :: DpqkLoc
            integer, intent(in)                   :: NDensities
            type(TAOBasis), intent(in)            :: AOBasis

            integer :: ShellA, ShellB
            integer :: n, Na, Nb

            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncSpher &
                  )
                  n = 1
                  do ShellB = 1, NShells
                        do ShellA = 1, NShells
                              Na = NAngFunc(ShellParamsIdx(ShellA))
                              Nb = NAngFunc(ShellParamsIdx(ShellB))                        
                              DpqkLoc(ShellA, ShellB) = n
                              n = n + Na * Nb * NDensities
                        end do
                  end do
            end associate
      end subroutine rpa_HF_DpqkLoc


      subroutine rpa_HF_JKpqkLoc(JKpqkLoc, JkpqkDim, NDensities, AOBasis)
            integer, dimension(:, :), intent(out) :: JKpqkLoc
            integer, intent(out)                  :: JKpqkDim
            integer, intent(in)                   :: NDensities
            type(TAOBasis), intent(in)            :: AOBasis

            integer :: ShellA, ShellB
            integer :: n, Na, Nb

            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncSpher &
                  )
                  JKpqkLoc = 0
                  JKpqkDim = 0
                  n = 1
                  do ShellB = 1, NShells
                        do ShellA = ShellB, NShells
                              Na = NAngFunc(ShellParamsIdx(ShellA))
                              Nb = NAngFunc(ShellParamsIdx(ShellB))                        
                              JKpqkLoc(ShellA, ShellB) = n
                              n = n + Na * Nb * NDensities
                              JKpqkDim = JKpqkDim + Na * Nb * NDensities
                        end do
                  end do
            end associate
      end subroutine rpa_HF_JKpqkLoc
      

      subroutine rpa_HF_UpdateDpqk(Dpqk, k, Dpq, DpqLoc, AOBasis)
            real(F64), dimension(:), intent(inout)  :: Dpqk
            integer, intent(in)                     :: k
            real(F64), dimension(:, :), intent(in)  :: Dpq
            integer, dimension(:, :), intent(in)    :: DpqLoc
            type(TAOBasis), intent(in)              :: AOBasis

            integer :: ShellA, ShellB
            integer :: Na, Nb
            integer :: a0, a1, b0, b1, p0, p1

            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncSpher, &
                  ShellLoc => AOBasis%ShellLocSpher &
                  )
                  !$omp parallel do collapse(2) &
                  !$omp private(ShellA, ShellB) &
                  !$omp private(Na, Nb, a0, a1, b0, b1, p0, p1)
                  do ShellB = 1, NShells
                        do ShellA = 1, NShells
                              Na = NAngFunc(ShellParamsIdx(ShellA))
                              Nb = NAngFunc(ShellParamsIdx(ShellB))
                              a0 = ShellLoc(ShellA)
                              a1 = ShellLoc(ShellA) + Na - 1
                              b0 = ShellLoc(ShellB)
                              b1 = ShellLoc(ShellB) + Nb - 1
                              p0 = DpqLoc(ShellA, ShellB) + Na * Nb * (k - 1)
                              p1 = DpqLoc(ShellA, ShellB) + Na * Nb * k - 1
                              Dpqk(p0:p1) = reshape(Dpq(a0:a1, b0:b1), [Na*Nb])
                        end do
                  end do
                  !$omp end parallel do
            end associate
      end subroutine rpa_HF_UpdateDpqk


      subroutine rpa_HF_UnpackJKpqk(Fpq, k, JKpqk, JKpqkLoc, AOBasis, Alpha)
            real(F64), dimension(:, :), intent(inout) :: Fpq
            integer, intent(in)                       :: k
            real(F64), dimension(:), intent(in)       :: JKpqk
            integer, dimension(:, :), intent(in)      :: JKpqkLoc
            type(TAOBasis), intent(in)                :: AOBasis
            real(F64), intent(in)                     :: Alpha

            integer :: ShellA, ShellB, ShellAB
            integer :: Na, Nb
            integer :: a0, a1, b0, b1, p0, p1

            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncSpher, &
                  ShellLoc => AOBasis%ShellLocSpher &
                  )
                  !$omp parallel do &
                  !$omp private(ShellA, ShellB, ShellAB) &
                  !$omp private(Na, Nb, a0, a1, b0, b1, p0, p1)
                  do ShellAB = 1, (NShells*(NShells+1))/2
                        call rpa_HF_pq2p_ge_q(ShellA, ShellB, ShellAB, NShells)
                        Na = NAngFunc(ShellParamsIdx(ShellA))
                        Nb = NAngFunc(ShellParamsIdx(ShellB))
                        a0 = ShellLoc(ShellA)
                        a1 = ShellLoc(ShellA) + Na - 1
                        b0 = ShellLoc(ShellB)
                        b1 = ShellLoc(ShellB) + Nb - 1
                        p0 = JKpqkLoc(ShellA, ShellB) + Na * Nb * (k - 1)
                        p1 = JKpqkLoc(ShellA, ShellB) + Na * Nb * k - 1
                        if (ShellA /= ShellB) then
                              Fpq(a0:a1, b0:b1) = Fpq(a0:a1, b0:b1) + Alpha * reshape(JKpqk(p0:p1), [Na, Nb])
                              Fpq(b0:b1, a0:a1) = Fpq(b0:b1, a0:a1) + Alpha * transpose(reshape(JKpqk(p0:p1), [Na, Nb]))
                        else
                              Fpq(a0:a1, b0:b1) = Fpq(a0:a1, b0:b1) + Alpha * reshape(JKpqk(p0:p1), [Na, Nb])
                        end if
                  end do
                  !$omp end parallel do
            end associate
      end subroutine rpa_HF_UnpackJKpqk
      

      subroutine rpa_HF_UnpackDpqk(Dpq, k, Dpqk, DpqkLoc, AOBasis)
            real(F64), dimension(:, :), intent(inout) :: Dpq
            integer, intent(in)                       :: k
            real(F64), dimension(:), intent(in)       :: Dpqk
            integer, dimension(:, :), intent(in)      :: DpqkLoc
            type(TAOBasis), intent(in)                :: AOBasis

            integer :: ShellA, ShellB
            integer :: Na, Nb
            integer :: a0, a1, b0, b1, p0, p1

            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncSpher, &
                  ShellLoc => AOBasis%ShellLocSpher &
                  )
                  !$omp parallel do collapse(2) &
                  !$omp private(ShellA, ShellB) &
                  !$omp private(Na, Nb, a0, a1, b0, b1, p0, p1)
                  do ShellB = 1, NShells
                        do ShellA = 1, NShells
                              Na = NAngFunc(ShellParamsIdx(ShellA))
                              Nb = NAngFunc(ShellParamsIdx(ShellB))
                              a0 = ShellLoc(ShellA)
                              a1 = ShellLoc(ShellA) + Na - 1
                              b0 = ShellLoc(ShellB)
                              b1 = ShellLoc(ShellB) + Nb - 1
                              p0 = DpqkLoc(ShellA, ShellB) + Na * Nb * (k - 1)
                              p1 = DpqkLoc(ShellA, ShellB) + Na * Nb * k - 1
                              Dpq(a0:a1, b0:b1) = reshape(Dpqk(p0:p1), [Na, Nb])
                        end do
                  end do
                  !$omp end parallel do
            end associate
      end subroutine rpa_HF_UnpackDpqk
      

      subroutine rpa_HF_Permute(Jcdab, Kabcd, Kabdc, Kbacd, Kbadc, Jabcd, &            
            Na, Nb, Nc, Nd)
            
            integer, intent(in)                               :: Na, Nb, Nc, Nd
            real(F64), dimension(Nc, Nd, Na, Nb), intent(out) :: Jcdab
            real(F64), dimension(Na, Nc, Nb, Nd), intent(out) :: Kabcd
            real(F64), dimension(Na, Nd, Nb, Nc), intent(out) :: Kabdc
            real(F64), dimension(Nb, Nc, Na, Nd), intent(out) :: Kbacd
            real(F64), dimension(Nb, Nd, Na, Nc), intent(out) :: Kbadc
            real(F64), dimension(Na, Nb, Nc, Nd), intent(in)  :: Jabcd

            integer :: a, b, c, d
            real(F64) :: t

            do d = 1, Nd
                  do c = 1, Nc
                        do b = 1, Nb
                              do a = 1, Na
                                    t = Jabcd(a, b, c, d)
                                    ! (AB|CD)
                                    Kabcd(a, c, b, d) = t
                                    ! (BA|CD)
                                    Kbacd(b, c, a, d) = t
                                    ! (AB|DC)
                                    Kabdc(a, d, b, c) = t
                                    ! (BA|DC)
                                    Kbadc(b, d, a, c) = t
                                    ! (CD|AB)
                                    Jcdab(c, d, a, b) = t
                              end do
                        end do
                  end do
            end do
      end subroutine rpa_HF_Permute


      subroutine rpa_HF_D(Drs, Dpqk, DpqkLoc, Nrs, ShR, ShS, k)
            integer, intent(in)                       :: Nrs, ShR, ShS
            real(F64), dimension(Nrs), intent(out)    :: Drs
            real(F64), dimension(:), intent(in)       :: Dpqk
            integer, dimension(:, :), intent(in)      :: DpqkLoc
            integer, intent(in)                       :: k

            integer :: p0, p1

            p0 = DpqkLoc(ShR, ShS) + Nrs * (k - 1)
            p1 = DpqkLoc(ShR, ShS) + Nrs * k - 1
            Drs(:) = Dpqk(p0:p1)
      end subroutine rpa_HF_D


      subroutine rpa_HF_Write(Fpq, JKpqkLoc, W, Npq, ShP, ShQ, k)
            integer, intent(in)                            :: Npq, ShP, ShQ, k
            real(F64), dimension(:), intent(inout)         :: Fpq
            integer, dimension(:, :), intent(in)           :: JKpqkLoc
            real(F64), dimension(Npq), intent(in)          :: W

            integer :: r, Offset

            Offset = JKpqkLoc(ShP, ShQ) + Npq * (k - 1) - 1
            do r = 1, Npq
                  !$omp atomic
                  Fpq(Offset+r) = Fpq(Offset+r) + W(r)
            end do
      end subroutine rpa_HF_Write


      subroutine rpa_HF_Symmetrize(Spq, Wpq, Np)
            integer, intent(in)                          :: Np
            real(F64), dimension(Np, Np), intent(out)    :: Spq
            real(F64), dimension(Np, Np), intent(in)     :: Wpq

            Spq(:, :) = Wpq(:, :) + transpose(Wpq(:, :))
      end subroutine rpa_HF_Symmetrize


      subroutine rpa_HF_Transpose(S, W, Np, Nq)
            integer, intent(in)                       :: Np, Nq
            real(F64), dimension(Nq, Np), intent(out) :: S
            real(F64), dimension(Np, Nq), intent(in)  :: W

            S(:, :) = transpose(W(:, :))
      end subroutine rpa_HF_Transpose
      

      subroutine rpa_HF_Contract_VD(Wpq, Vpqrs, Drs, Npq, Nrs, Alpha)
            integer, intent(in)                        :: Npq, Nrs
            real(F64), dimension(Npq), intent(out)     :: Wpq
            real(F64), dimension(Npq, Nrs), intent(in) :: Vpqrs
            real(F64), dimension(Nrs), intent(in)      :: Drs
            real(F64), intent(in)                      :: Alpha

            integer :: t

            Wpq = ZERO
            do t = 1, Nrs
                  Wpq(:) = Wpq(:) + Vpqrs(:, t) * Drs(t)
            end do
            Wpq(:) = Alpha * Wpq(:)
      end subroutine rpa_HF_Contract_VD
      

      subroutine rpa_HF_IntegralsLoop(Jpqk, Kpqk, JKpqkLoc, Dpqk, DpqkLoc, &
            NDensities, AOBasis, Chol2Vecs)
            
            real(F64), dimension(:), intent(out)       :: Jpqk
            real(F64), dimension(:), intent(out)       :: Kpqk
            integer, dimension(:, :), intent(in)       :: JKpqkLoc
            real(F64), dimension(:), intent(in)        :: Dpqk
            integer, dimension(:, :), intent(in)       :: DpqkLoc
            integer, intent(in)                        :: NDensities
            type(TAOBasis), intent(in)                 :: AOBasis
            type(TChol2Vecs), intent(in)               :: Chol2Vecs
            
            real(F64), dimension((2*AOBasis%LmaxGTO+1)**4) :: Jabcd, Jcdab
            real(F64), dimension((2*AOBasis%LmaxGTO+1)**4) :: Kabcd, Kabdc, Kbacd, Kbadc
            real(F64), dimension((2*AOBasis%LmaxGTO+1)**2) :: D
            real(F64), dimension((2*AOBasis%LmaxGTO+1)**2) :: W
            real(F64), dimension((2*AOBasis%LmaxGTO+1)**2) :: S
            
            integer :: NShellQuartets, ShABCD, ShAB, ShCD 
            integer :: ShellParamsA, ShellParamsB, ShellParamsC, ShellParamsD
            integer :: ShA, ShB, ShC, ShD
            integer :: La, Lb, Lc, Ld
            integer :: Na, Nb, Nc, Nd
            integer :: AtomA, AtomB, AtomC, AtomD
            integer :: a0, a1, b0, b1, c0, c1, d0, d1
            real(F64) :: Fabcd, Gab, Gcd
            integer :: k
            integer, parameter :: PtrOffset = AUTO2E_SPHER_OFFSET
            real(F64), parameter :: Kappa = ZERO

            Jpqk = ZERO
            Kpqk = ZERO
            associate ( &
                  NShellPairs    => Chol2Vecs%NShellPairs, &
                  ShellPairs     => Chol2Vecs%ShellPairs, &
                  ShellCenters   => AOBasis%ShellCenters, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum  => AOBasis%ShellMomentum, &
                  NAngFunc       => AOBasis%NAngFuncSpher, &
                  ShellLoc       => AOBasis%ShellLocSpher, &
                  NormFactors    => AOBasis%NormFactorsSpher, &
                  CntrCoeffs     => AOBasis%CntrCoeffs, &
                  Exponents      => AOBasis%Exponents, &
                  NPrimitives    => AOBasis%NPrimitives, &
                  AtomCoords     => AOBasis%AtomCoords &
                  )
                  NShellQuartets = (NShellPairs * (NShellPairs + 1)) / 2
                  !$omp parallel do schedule(guided) &
                  !$omp private(ShABCD, ShAB, ShCD) &
                  !$omp private(ShA, ShB, ShC, ShD) &
                  !$omp private(AtomA, AtomB, AtomC, AtomD) &
                  !$omp private(ShellParamsA, ShellParamsB, ShellParamsC, ShellParamsD) &
                  !$omp private(La, Lb, Lc, Ld, Na, Nb, Nc, Nd) &
                  !$omp private(a0, a1, b0, b1, c0, c1, d0, d1) &
                  !$omp private(Jabcd, Jcdab) &
                  !$omp private(Kabcd, Kabdc, Kbacd, Kbadc) &
                  !$omp private(D) &
                  !$omp private(Fabcd, Gab, Gcd) &
                  !$omp private(W, S) &
                  !$omp private(k)
                  do ShABCD = 1, NShellQuartets
                        call rpa_HF_pq2p_ge_q(ShAB, ShCD, ShABCD, NShellPairs)
                        if (ShellPairs(1, ShAB) > ShellPairs(1, ShCD)) then
                              ShA = ShellPairs(1, ShAB)
                              ShB = ShellPairs(2, ShAB)
                              ShC = ShellPairs(1, ShCD)
                              ShD = ShellPairs(2, ShCD)
                        else if (ShellPairs(1, ShCD) > ShellPairs(1, ShAB)) then
                              ShA = ShellPairs(1, ShCD)
                              ShB = ShellPairs(2, ShCD)
                              ShC = ShellPairs(1, ShAB)
                              ShD = ShellPairs(2, ShAB)
                        else if (ShellPairs(2, ShAB) > ShellPairs(2, ShCD)) then
                              ShA = ShellPairs(1, ShAB)
                              ShB = ShellPairs(2, ShAB)
                              ShC = ShellPairs(1, ShCD)
                              ShD = ShellPairs(2, ShCD)
                        else if (ShellPairs(2, ShCD) > ShellPairs(2, ShAB)) then
                              ShA = ShellPairs(1, ShCD)
                              ShB = ShellPairs(2, ShCD)
                              ShC = ShellPairs(1, ShAB)
                              ShD = ShellPairs(2, ShAB)
                        else
                              ShA = ShellPairs(1, ShAB)
                              ShB = ShellPairs(2, ShAB)
                              ShC = ShellPairs(1, ShCD)
                              ShD = ShellPairs(2, ShCD)
                        end if
                        AtomA = ShellCenters(ShA)
                        AtomB = ShellCenters(ShB)
                        AtomC = ShellCenters(ShC)
                        AtomD = ShellCenters(ShD)

                        ShellParamsB = ShellParamsIdx(ShB)
                        ShellParamsA = ShellParamsIdx(ShA)
                        ShellParamsC = ShellParamsIdx(ShC)
                        ShellParamsD = ShellParamsIdx(ShD)

                        La = ShellMomentum(ShellParamsA)
                        Lb = ShellMomentum(ShellParamsB)
                        Lc = ShellMomentum(ShellParamsC)
                        Ld = ShellMomentum(ShellParamsD)

                        Na = NAngFunc(ShellParamsA)
                        Nb = NAngFunc(ShellParamsB)
                        Nc = NAngFunc(ShellParamsC)                        
                        Nd = NAngFunc(ShellParamsD)

                        a0 = ShellLoc(ShA)
                        a1 = ShellLoc(ShA) + Na - 1
                        b0 = ShellLoc(ShB)
                        b1 = ShellLoc(ShB) + Nb - 1
                        c0 = ShellLoc(ShC)
                        c1 = ShellLoc(ShC) + Nc - 1
                        d0 = ShellLoc(ShD)
                        d1 = ShellLoc(ShD) + Nd - 1

                        call Auto2eERI(PtrOffset+auto2e_idx(Ld, Lc, Lb, La))%ptr( &
                              Jabcd, &
                              !
                              ! ShellD
                              !
                              AtomCoords(:, AtomD), CntrCoeffs(:, ShellParamsD), &
                              NormFactors(:, ShellParamsD), Exponents(:, ShellParamsD), &
                              NPrimitives(ShellParamsD), &
                              !
                              ! ShellC
                              !
                              AtomCoords(:, AtomC), CntrCoeffs(:, ShellParamsC), &
                              NormFactors(:, ShellParamsC), Exponents(:, ShellParamsC), &
                              NPrimitives(ShellParamsC), &
                              !
                              ! ShellB
                              !
                              AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                              NormFactors(:, ShellParamsB), Exponents(:, ShellParamsB), &
                              NPrimitives(ShellParamsB), &
                              !
                              ! ShellA
                              !
                              AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                              NormFactors(:, ShellParamsA), Exponents(:, ShellParamsA), &
                              NPrimitives(ShellParamsA), &
                              Kappa)

                        Fabcd = ONE
                        if (ShAB == ShCD) then
                              Fabcd = Fabcd / TWO
                        end if
                        if (ShA == ShB) then
                              Fabcd = Fabcd / TWO
                              Gab = TWO
                        else
                              Gab = ONE
                        end if
                        if (ShC == ShD) then
                              Fabcd = Fabcd / TWO
                              Gcd = TWO
                        else
                              Gcd = ONE
                        end if
                        call rpa_HF_Permute(Jcdab, Kabcd, Kabdc, Kbacd, Kbadc, Jabcd, &            
                              Na, Nb, Nc, Nd)
                        do k = 1, NDensities
                              ! ---------------------------------------------------------
                              ! Coulomb matrix contributions in the lower triangle of Jpq
                              ! ---------------------------------------------------------
                              ! (AB|CD) + (AB|DC) + (BA|CD) + (BA|DC)
                              call rpa_HF_D(D, Dpqk, DpqkLoc, Nc*Nd, ShC, ShD, k)
                              call rpa_HF_Contract_VD(W, Jabcd, D, Na*Nb, Nc*Nd, Fabcd*Gab*TWO)
                              call rpa_HF_Write(Jpqk, JKpqkLoc, W, Na*Nb, ShA, ShB, k)
                              ! (CD|AB) + (CD|BA) + (DC|AB) + (DC|BA)
                              call rpa_HF_D(D, Dpqk, DpqkLoc, Na*Nb, ShA, ShB, k)
                              call rpa_HF_Contract_VD(W, Jcdab, D, Nc*Nd, Na*Nb, Fabcd*Gcd*TWO)
                              call rpa_HF_Write(Jpqk, JKpqkLoc, W, Nc*Nd, ShC, ShD, k)
                              ! ----------------------------------------------------------
                              ! Exchange matrix contributions in the lower triangle of Kpq
                              ! ----------------------------------------------------------
                              if (ShA > ShC) then
                                    ! (AB|CD)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Nb*Nd, ShB, ShD, k)
                                    call rpa_HF_Contract_VD(W, Kabcd, D, Na*Nc, Nb*Nd, Fabcd)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, W, Na*Nc, ShA, ShC, k)
                              else
                                    ! ShA == ShC
                                    ! (AB|CD) + (CD|AB)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Nb*Nd, ShB, ShD, k)
                                    call rpa_HF_Contract_VD(W, Kabcd, D, Na*Nc, Nb*Nd, Fabcd)
                                    call rpa_HF_Symmetrize(S, W, Na)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, S, Na*Nc, ShA, ShC, k)
                              end if
                              if (ShA > ShD) then
                                    ! (AB|DC)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Nb*Nc, ShB, ShC, k)
                                    call rpa_HF_Contract_VD(W, Kabdc, D, Na*Nd, Nb*Nc, Fabcd)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, W, Na*Nd, ShA, ShD, k)
                              else
                                    ! ShA == ShD
                                    ! (AB|DC) + (DC|AB)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Nb*Nc, ShB, ShC, k)
                                    call rpa_HF_Contract_VD(W, Kabdc, D, Na*Nd, Nb*Nc, Fabcd)
                                    call rpa_HF_Symmetrize(S, W, Na)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, S, Na*Nd, ShA, ShD, k)
                              end if
                              if (ShB > ShC) then
                                    ! (BA|CD)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Na*Nd, ShA, ShD, k)
                                    call rpa_HF_Contract_VD(W, Kbacd, D, Nb*Nc, Na*Nd, Fabcd)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, W, Nb*Nc, ShB, ShC, k)
                              else if (ShC > ShB) then
                                    ! (CD|BA)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Na*Nd, ShA, ShD, k)
                                    call rpa_HF_Contract_VD(W, Kbacd, D, Nb*Nc, Na*Nd, Fabcd)
                                    call rpa_HF_Transpose(S, W, Nb, Nc)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, S, Nc*Nb, ShC, ShB, k)
                              else
                                    ! ShB == ShC
                                    ! (BA|CD) + (CD|BA)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc, Na*Nd, ShA, ShD, k)
                                    call rpa_HF_Contract_VD(W, Kbacd, D, Nb*Nc, Na*Nd, Fabcd)
                                    call rpa_HF_Symmetrize(S, W, Nb)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, S, Nb*Nc, ShB, ShC, k)
                              end if
                              if (ShB > ShD) then
                                    ! (BA|DC)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc,  Na*Nc, ShA, ShC, k)
                                    call rpa_HF_Contract_VD(W, Kbadc, D, Nb*Nd, Na*Nc, Fabcd)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, W, Nb*Nd, ShB, ShD, k)
                              else if (ShD > ShB) then
                                    ! (DC|BA)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc,  Na*Nc, ShA, ShC, k)
                                    call rpa_HF_Contract_VD(W, Kbadc, D, Nb*Nd, Na*Nc, Fabcd)
                                    call rpa_HF_Transpose(S, W, Nb, Nd)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, S, Nd*Nb, ShD, ShB, k)
                              else
                                    ! ShB == ShD
                                    ! (BA|DC) + (DC|BA)
                                    call rpa_HF_D(D, Dpqk, DpqkLoc,  Na*Nc, ShA, ShC, k)
                                    call rpa_HF_Contract_VD(W, Kbadc, D, Nb*Nd, Na*Nc, Fabcd)
                                    call rpa_HF_Symmetrize(S, W, Nb)
                                    call rpa_HF_Write(Kpqk, JKpqkLoc, S, Nb*Nd, ShB, ShD, k)
                              end if
                        end do
                  end do
                  !$omp end parallel do
            end associate
      end subroutine rpa_HF_IntegralsLoop
      

      pure subroutine rpa_HF_pq2p_ge_q(p, q, pq, n)
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
            ! 1
            ! 2 5
            ! 3 6 4
            !
            integer, intent(out) :: p
            integer, intent(out) :: q
            integer, intent(in)  :: pq
            integer, intent(in)  :: n

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
      end subroutine rpa_HF_pq2p_ge_q
      

      subroutine rpa_HF_Fpq(MeanFieldStates, System, Dpqk, DpqkLoc, &
            NOcc, NSpins, Chol2Vecs, AOBasis)
            
            type(TMeanField), dimension(:), intent(out)   :: MeanFieldStates
            type(TSystem), intent(inout)                  :: System
            real(F64), dimension(:), intent(in)           :: Dpqk
            integer, dimension(:, :), intent(in)          :: DpqkLoc
            integer, dimension(:, :), intent(in)          :: NOcc
            integer, dimension(:), intent(in)             :: NSpins
            type(TChol2Vecs), intent(in)                  :: Chol2Vecs
            type(TAOBasis), intent(in)                    :: AOBasis

            integer :: NAO
            integer :: s, k, l
            real(F64), dimension(:, :), allocatable :: Tpq, Vpq
            real(F64), dimension(:, :), allocatable :: Dpq
            real(F64), dimension(:), allocatable :: Jpqk, Kpqk
            integer, dimension(:, :), allocatable :: JKpqkLoc
            integer :: JKpqkDim
            real(F64) :: EHFTwoEl, Enucl, EHbare
            real(F64) :: TrDJK, TrDT, TrDV
            integer :: NDensities, NSystems
            integer, dimension(2) :: t

            NAO = AOBasis%NAOSpher
            NSystems = size(MeanFieldStates)
            NDensities = sum(NSpins)
            allocate(JKpqkLoc(AOBasis%NShells, AOBasis%NShells))
            call rpa_HF_JKpqkLoc(JKpqkLoc, JkpqkDim, NDensities, AOBasis)
            allocate(Jpqk(JKpqkDim))
            allocate(Kpqk(JKpqkDim))
            !
            ! Exchange and Coulomb matrices for the supersystem
            ! and all subsystems
            !
            call rpa_HF_IntegralsLoop(Jpqk, Kpqk, JKpqkLoc, Dpqk, DpqkLoc, NDensities, AOBasis, Chol2Vecs)
            allocate(Vpq(NAO, NAO))
            allocate(Tpq(NAO, NAO))
            allocate(Dpq(NAO, NAO))
            call ints1e_T(Tpq, AOBasis)
            do k = 1, NSystems
                  call sys_Init(System, k)
                  allocate(MeanFieldStates(k)%F_ao(NAO, NAO, NSpins(k)))
                  associate (Fpq => MeanFieldStates(k)%F_ao(:, :, :))
                        call sys_NuclearRepulsion(Enucl, System)
                        l = 0
                        if (k > 1) l = sum(NSpins(1:k-1))
                        t(1) = l + 1
                        t(2) = l + 2
                        do s = 1, NSpins(k)
                              Fpq(:, :, s) = ZERO
                        end do
                        do s = 1, NSpins(k)
                              if (NOcc(s, k) > 0) then
                                    if (NSpins(k) == 1) then
                                          !
                                          ! Closed shells
                                          !
                                          call rpa_HF_UnpackJKpqk(Fpq(:, :, s), t(s), Jpqk, JKpqkLoc, AOBasis, TWO)
                                          call rpa_HF_UnpackJKpqk(Fpq(:, :, s), t(s), Kpqk, JKpqkLoc, AOBasis, -ONE)
                                    else
                                          !
                                          ! Open shells
                                          !
                                          if (NOcc(1, k) > 0) &
                                                call rpa_HF_UnpackJKpqk(Fpq(:, :, 1), t(s), Jpqk, JKpqkLoc, AOBasis, ONE)
                                          if (NOcc(2, k) > 0) &
                                                call rpa_HF_UnpackJKpqk(Fpq(:, :, 2), t(s), Jpqk, JKpqkLoc, AOBasis, ONE)
                                          call rpa_HF_UnpackJKpqk(Fpq(:, :, s), t(s), Kpqk, JKpqkLoc, AOBasis, -ONE)
                                    end if
                              end if
                        end do
                        EHFTwoEl = ZERO
                        EHbare = ZERO
                        call ints1e_Vne(Vpq, AOBasis, System)                  
                        do s = 1, NSpins(k)
                              if (NOcc(s, k) > 0) then
                                    call rpa_HF_UnpackDpqk(Dpq, t(s), Dpqk, DpqkLoc, AOBasis)
                                    call real_vw_x(TrDJK, Dpq, Fpq(:, :, s), NAO**2)
                                    call real_vw_x(TrDV, Dpq, Vpq, NAO**2)
                                    call real_vw_x(TrDT, Dpq, Tpq, NAO**2)
                                    if (NSpins(k) == 1) then
                                          EHFTwoEl = EHFTwoEl + TWO * (ONE/TWO) * TrDJK
                                          EHbare = EHbare + TWO * (TrDV + TrDT)
                                    else
                                          EHFTwoEl = EHFTwoEl + (ONE/TWO) * TrDJK
                                          EHbare = EHbare + TrDV + TrDT
                                    end if
                                    Fpq(:, :, s) = Fpq(:, :, s) + Tpq(:, :)
                                    Fpq(:, :, s) = Fpq(:, :, s) + Vpq(:, :)
                              end if
                        end do
                        MeanFieldStates(k)%EtotHF = Enucl + EHbare + EHFTwoEl
                        MeanFieldStates(k)%NOcc(:) = NOcc(:, k)
                        MeanFieldStates(k)%NSpins = NSpins(k)
                  end associate
            end do
      end subroutine rpa_HF_Fpq
end module rpa_HF
