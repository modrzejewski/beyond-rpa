module Fock
      use arithmetic
      use Auto2e
      use sort
      use basis_sets
      use coulomb
      use exchange
      use display

      implicit none
      !
      ! Two-electron integrals permutation labels (bit flags)
      !
      integer, parameter, private                             :: EXCH_ABCD = 2**0
      integer, parameter, private                             :: EXCH_ABDC = 2**1
      integer, parameter, private                             :: EXCH_BACD = 2**2
      integer, parameter, private                             :: EXCH_BADC = 2**3
      integer, parameter, private                             :: EXCH_CDAB = 2**4
      integer, parameter, private                             :: EXCH_CDBA = 2**5
      integer, parameter, private                             :: EXCH_DCAB = 2**6
      integer, parameter, private                             :: EXCH_DCBA = 2**7
      integer, parameter, private                             :: COUL_ABCD = 2**8
      integer, parameter, private                             :: COUL_CDAB = 2**9
      
      integer, parameter                                      :: COUL_ALL = &
            COUL_ABCD + COUL_CDAB
      integer, parameter                                      :: EXCH_ALL = &
            EXCH_ABCD + EXCH_ABDC + EXCH_BACD + EXCH_BADC + &
            EXCH_CDAB + EXCH_CDBA + EXCH_DCAB + EXCH_DCBA
      integer, parameter                                      :: COULEXCH_ALL = COUL_ALL + EXCH_ALL

contains

      subroutine fock_RhoVMaxAB(RhoVMaxAB, V, RhoAB, Nab)
            real(F64), intent(out)                        :: RhoVMaxAB
            real(F64), dimension(Nab, Nab), intent(inout) :: V
            real(F64), dimension(Nab), intent(in)         :: RhoAB
            integer, intent(in)                           :: Nab

            integer :: q

            do q = 1, Nab
                  V(:, q) = RhoAB(q)**2 * V(:, q)
            end do
            V = abs(V)
            RhoVMaxAB = sqrt(maxval(V))
      end subroutine fock_RhoVMaxAB

      
      subroutine fock_VMax(VMax, IVMax, RhoVMax, IRhoVMax, Rho, ShellPairLoc, ShellSubsets, NSubsets, ShellCenters, &
            AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, &
            NormFactors, Kappa, MaxNAngFunc, MaxSubsetDim, SpherAO)
            !
            ! (1) Sort two-electron integrals according to their absolute values, in decreasing
            ! order. The sorting is done within bra indices belonging to a single shell subset.
            ! (2) Sort two-electron integrals (ab|ab), multiplied by density matrix elements Rho(a,b),
            ! in decreasing order. The sorting is done within bra indices belonging to a single shell
            ! subset. It's assumed that Rho contains both halves of data, and it's allowed to be either
            ! symmetric or anti-symmetric.
            !
            real(F64), dimension(:, :), intent(out)  :: VMax
            integer, dimension(:, :), intent(out)    :: IVMax
            real(F64), dimension(:, :), intent(out)  :: RhoVMax
            integer, dimension(:, :), intent(out)    :: IRhoVMax
            real(F64), dimension(:), intent(in)      :: Rho
            integer, dimension(:, :), intent(in)     :: ShellPairLoc
            integer, dimension(:, :), intent(in)     :: ShellSubsets
            integer, intent(in)                      :: NSubsets
            integer, dimension(:), intent(in)        :: ShellCenters
            real(F64), dimension(:, :), intent(in)   :: AtomCoords
            integer, dimension(:), intent(in)        :: ShellParamsIdx
            integer, dimension(:), intent(in)        :: ShellMomentum
            integer, dimension(:), intent(in)        :: NAngFunc
            integer, dimension(:), intent(in)        :: NPrimitives
            real(F64), dimension(:, :), intent(in)   :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)   :: Exponents
            real(F64), dimension(:, :), intent(in)   :: NormFactors
            real(F64), intent(in)                    :: Kappa
            integer, intent(in)                      :: MaxNAngFunc
            integer, intent(in)                      :: MaxSubsetDim
            logical, intent(in)                      :: SpherAO

            integer :: AtomA, AtomB, SubsetA, SubsetB
            integer :: ShellParamsA, ShellParamsB, LocAB
            integer :: ShellA, ShellB, b
            integer :: La, Lb
            integer :: Na, Nb, Naabb, Nab
            real(F64), dimension(MaxNAngFunc**4) :: V
            real(F64), dimension(:), allocatable :: RhoVsort, Vsort
            integer, dimension(:), allocatable :: IRhoVsort, IVsort
            real(F64) :: VMaxAB, RhoVMaxAB
            integer :: PtrOffset

            if (SpherAO) then
                  PtrOffset = AUTO2E_SPHER_OFFSET
            else
                  PtrOffset = 0
            end if
            !$omp parallel &
            !$omp private(SubsetA, SubsetB, AtomA, AtomB) &
            !$omp private(ShellA, ShellB, b) &
            !$omp private(ShellParamsA, ShellParamsB) &
            !$omp private(La, Lb, Na, Nb, Naabb) &
            !$omp private(V, VMaxAB, Vsort, IVsort) &
            !$omp private(Nab, RhoVMaxAB, RhoVsort, IRhoVsort, LocAB) &
            !$omp default(shared)
            allocate(Vsort(MaxSubsetDim))
            allocate(IVsort(MaxSubsetDim))
            allocate(RhoVsort(MaxSubsetDim))
            allocate(IRhoVsort(MaxSubsetDim))
            !$omp do collapse(2) schedule(guided)
            do SubsetA = 1, NSubsets
                  do SubsetB = 1, NSubsets
                        do ShellA = ShellSubsets(1, SubsetA), ShellSubsets(2, SubsetA)
                              do ShellB = ShellSubsets(1, SubsetB), ShellSubsets(2, SubsetB)

                                    AtomA = ShellCenters(ShellA)
                                    ShellParamsA = ShellParamsIdx(ShellA)
                                    La = ShellMomentum(ShellParamsA)
                                    Na = NAngFunc(ShellParamsA)

                                    AtomB = ShellCenters(ShellB)
                                    ShellParamsB = ShellParamsIdx(ShellB)
                                    Lb = ShellMomentum(ShellParamsB)
                                    Nb = NAngFunc(ShellParamsB)                                    

                                    call Auto2eERI(PtrOffset+auto2e_idx(La, Lb, La, Lb))%ptr( &
                                          V, &
                                          !
                                          ! ShellA
                                          !
                                          AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                                          NormFactors(:, ShellParamsA), Exponents(:, ShellParamsA), &
                                          NPrimitives(ShellParamsA), &
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
                                          !
                                          ! ShellB
                                          !
                                          AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                                          NormFactors(:, ShellParamsB), Exponents(:, ShellParamsB), &
                                          NPrimitives(ShellParamsB), &
                                          Kappa)
                                    !
                                    ! Max two-electron integral in a shell quartet
                                    !
                                    Naabb = Na**2 * Nb**2
                                    V(1:Naabb) = abs(V(1:Naabb))
                                    VMaxAB = sqrt(maxval(V(1:Naabb)))
                                    !
                                    ! Put the minus sign in front of every integral because
                                    ! DSORT sorts the elements in increasing order
                                    !
                                    b = ShellB - ShellSubsets(1, SubsetB) + 1
                                    Vsort(b) = -VMaxAB
                                    IVsort(b) = ShellB
                                    !
                                    ! Max two-electron integral in a shell quartet,
                                    ! multiplied by a density matrix element
                                    !
                                    Nab = Na * Nb
                                    LocAB = ShellPairLoc(ShellB, ShellA)
                                    call fock_RhoVMaxAB(RhoVMaxAB, V(1:Naabb), Rho(LocAB:LocAB+Nab-1), Nab)
                                    b = ShellB - ShellSubsets(1, SubsetB) + 1
                                    RhoVsort(b) = -RhoVMaxAB
                                    IRhoVsort(b) = ShellB
                              end do
                              Nb = ShellSubsets(2, SubsetB) - ShellSubsets(1, SubsetB) + 1
                              call dsort(Vsort(1:Nb), IVsort(1:Nb), Nb)                              
                              call dsort(RhoVsort(1:Nb), IRhoVsort(1:Nb), Nb)
                              VMax(ShellSubsets(1,SubsetB):ShellSubsets(2,SubsetB), ShellA) = -Vsort(1:Nb)
                              IVMax(ShellSubsets(1,SubsetB):ShellSubsets(2,SubsetB), ShellA) = IVsort(1:Nb)
                              RhoVMax(ShellSubsets(1,SubsetB):ShellSubsets(2,SubsetB), ShellA) = -RhoVsort(1:Nb)
                              IRhoVMax(ShellSubsets(1,SubsetB):ShellSubsets(2,SubsetB), ShellA) = IRhoVsort(1:Nb)
                        end do
                  end do
            end do
            !$omp end do
            deallocate(Vsort)
            deallocate(IVsort)
            deallocate(RhoVsort)
            deallocate(IRhoVsort)
            !$omp end parallel
      end subroutine fock_VMax


      subroutine fock_ShellQuartet_SymRho(K, J, Flags, ShellA, ShellB, ShellC, ShellD, &
            KCoeff, JCoeffAB, JCoeffCD, Rho, ShellPairLoc, ShellCenters, AtomCoords, ShellParamsIdx, &
            ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
            MaxNAngFunc, PtrOffset)
            
            real(F64), dimension(:), intent(inout) :: K
            real(F64), dimension(:), intent(inout) :: J
            integer, intent(in)                    :: Flags
            integer, intent(in)                    :: ShellA, ShellB, ShellC, ShellD
            real(F64), intent(in)                  :: KCoeff
            real(F64), intent(in)                  :: JCoeffAB, JCoeffCD
            real(F64), dimension(:), intent(in)    :: Rho
            integer, dimension(:, :), intent(in)   :: ShellPairLoc
            integer, dimension(:), intent(in)      :: ShellCenters
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, dimension(:), intent(in)      :: NPrimitives
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            real(F64), dimension(:, :), intent(in) :: NormFactors
            real(F64), intent(in)                  :: Kappa
            integer, intent(in)                    :: MaxNAngFunc
            integer, intent(in)                    :: PtrOffset

            integer :: w
            integer :: Nab, Ncd, Nabcd
            integer :: exchbits, coulbits
            integer :: s0, s1, ab0, ab1, cd0, cd1
            integer :: AtomA, ShellParamsA, La, Na
            integer :: AtomB, ShellParamsB, Lb, Nb
            integer :: AtomC, ShellParamsC, Lc, Nc
            integer :: AtomD, ShellParamsD, Ld, Nd
            real(F64) :: JScalAB, JScalCD
            real(F64), dimension(MaxNAngFunc**4) :: Gabcd
            real(F64), dimension(MaxNAngFunc**2) :: Scratch

            AtomA = ShellCenters(ShellA)
            ShellParamsA = ShellParamsIdx(ShellA)
            La = ShellMomentum(ShellParamsA)
            Na = NAngFunc(ShellParamsA)

            AtomB = ShellCenters(ShellB)
            ShellParamsB = ShellParamsIdx(ShellB)
            Lb = ShellMomentum(ShellParamsB)
            Nb = NAngFunc(ShellParamsB)

            AtomC = ShellCenters(ShellC)
            ShellParamsC = ShellParamsIdx(ShellC)
            Lc = ShellMomentum(ShellParamsC)
            Nc = NAngFunc(ShellParamsC)

            AtomD = ShellCenters(ShellD)
            ShellParamsD = ShellParamsIdx(ShellD)
            Ld = ShellMomentum(ShellParamsD)
            Nd = NAngFunc(ShellParamsD)

            Nabcd = Na * Nb * Nc * Nd
            Nab = Na * Nb
            Ncd = Nc * Nd
            !
            ! Compute two-electron integrals (BA|DC). Note the permuted order of shells!
            !
            call Auto2eERI(PtrOffset+auto2e_idx(Lb, La, Ld, Lc))%ptr( &
                  Gabcd, &
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
                  Kappa)

            exchbits = iand(Flags, EXCH_ALL)
            coulbits = iand(Flags, COUL_ALL)
            JScalAB = JCoeffAB
            JScalCD = JCoeffCD
            !
            ! Exchange integrals
            !
            if (exchbits > 0) then
                  do w = 1, Nabcd
                        Gabcd(w) = KCoeff * Gabcd(w)
                  end do
                  JScalAB = JCoeffAB / KCoeff
                  JScalCD = JCoeffCD / KCoeff
                  !
                  ! The EXCH_ABCD flag corresponds to the contribution K(a,c) <- K(a,c) + Rho(b, d) * (ab|cd)
                  !
                  if (iand(exchbits, EXCH_ABCD) > 0) then
                        call exch_digest_RhoAC(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_ABDC) > 0) then
                        call exch_digest_RhoAD(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_BACD) > 0) then
                        call exch_digest_RhoBC(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_BADC) > 0) then
                        call exch_digest_RhoBD(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_CDAB) > 0) then
                        call exch_digest_RhoCA(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_CDBA) > 0) then
                        call exch_digest_RhoCB(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_DCAB) > 0) then
                        call exch_digest_RhoDA(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_DCBA) > 0) then
                        call exch_digest_RhoDB(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if
            end if
            !
            ! Coulomb integrals
            !
            if (coulbits > 0) then
                  ab0 = ShellPairLoc(ShellA, ShellB)
                  ab1 = ShellPairLoc(ShellA, ShellB) + Nab - 1
                  cd0 = ShellPairLoc(ShellC, ShellD)
                  cd1 = ShellPairLoc(ShellC, ShellD) + Ncd - 1
                  if (iand(coulbits, COUL_ALL) == COUL_ALL) then
                        call coul_Jab(Scratch, Rho(cd0:cd1), Gabcd, Nab, Ncd, JScalAB)
                        s0 = ab0
                        s1 = ab1
                        call fock_Offload(J(s0:s1), Scratch, s1-s0+1)
                        call coul_Jcd(Scratch, Rho(ab0:ab1), Gabcd, Nab, Ncd, JScalCD)
                        s0 = cd0
                        s1 = cd1
                        call fock_Offload(J(s0:s1), Scratch, s1-s0+1)
                  else if (iand(coulbits, COUL_ABCD) > 0) then                            
                        call coul_Jab(Scratch, Rho(cd0:cd1), Gabcd, Nab, Ncd, JScalAB)
                        s0 = ab0
                        s1 = ab1
                        call fock_Offload(J(s0:s1), Scratch, s1-s0+1)
                  else
                        call coul_Jcd(Scratch, Rho(ab0:ab1), Gabcd, Nab, Ncd, JScalCD)
                        s0 = cd0
                        s1 = cd1
                        call fock_Offload(J(s0:s1), Scratch, s1-s0+1)
                  end if
            end if
      end subroutine fock_ShellQuartet_SymRho


      subroutine fock_ShellQuartet_AntiSymRho(K, Flags, ShellA, ShellB, ShellC, ShellD, &
            KCoeff, Rho, ShellPairLoc, ShellCenters, AtomCoords, ShellParamsIdx, &
            ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
            MaxNAngFunc, PtrOffset)
            !
            ! Compute the contributions to the Hartree-Fock exchange matrix in the case of an antisymmetric
            ! density matrix Rho.
            !
            ! The definition of the exchange matrix element is (note the interchanged indices pq):
            !
            ! K(p, q) <- K(p, q) + KCoeff * Rho(r, s) * (qr|ps)
            ! Rho(r, s) = -Rho(s, r)
            !
            ! The code below employs the antisymmetry of K and computes the matrix element as
            !
            ! K(p, q) <- K(p, q) - KCoeff * Rho(r, s) * (pr|qs)
            !
            ! The bit mask variable Flags controls which permutations of (pq|rs) contribute to K.
            !
            real(F64), dimension(:), intent(inout) :: K
            integer, intent(in)                    :: Flags
            integer, intent(in)                    :: ShellA, ShellB, ShellC, ShellD
            real(F64), intent(in)                  :: KCoeff
            real(F64), dimension(:), intent(in)    :: Rho
            integer, dimension(:, :), intent(in)   :: ShellPairLoc
            integer, dimension(:), intent(in)      :: ShellCenters
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, dimension(:), intent(in)      :: NPrimitives
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            real(F64), dimension(:, :), intent(in) :: NormFactors
            real(F64), intent(in)                  :: Kappa
            integer, intent(in)                    :: MaxNAngFunc
            integer, intent(in)                    :: PtrOffset

            integer :: w
            integer :: Nab, Ncd, Nabcd
            integer :: exchbits
            integer :: s0, s1
            integer :: AtomA, ShellParamsA, La, Na
            integer :: AtomB, ShellParamsB, Lb, Nb
            integer :: AtomC, ShellParamsC, Lc, Nc
            integer :: AtomD, ShellParamsD, Ld, Nd
            real(F64), dimension(MaxNAngFunc**4) :: Gabcd
            real(F64), dimension(MaxNAngFunc**2) :: Scratch

            exchbits = iand(Flags, EXCH_ALL)
            !
            ! Exchange integrals
            !
            if (exchbits > 0) then
                  AtomA = ShellCenters(ShellA)
                  ShellParamsA = ShellParamsIdx(ShellA)
                  La = ShellMomentum(ShellParamsA)
                  Na = NAngFunc(ShellParamsA)

                  AtomB = ShellCenters(ShellB)
                  ShellParamsB = ShellParamsIdx(ShellB)
                  Lb = ShellMomentum(ShellParamsB)
                  Nb = NAngFunc(ShellParamsB)

                  AtomC = ShellCenters(ShellC)
                  ShellParamsC = ShellParamsIdx(ShellC)
                  Lc = ShellMomentum(ShellParamsC)
                  Nc = NAngFunc(ShellParamsC)

                  AtomD = ShellCenters(ShellD)
                  ShellParamsD = ShellParamsIdx(ShellD)
                  Ld = ShellMomentum(ShellParamsD)
                  Nd = NAngFunc(ShellParamsD)

                  Nabcd = Na * Nb * Nc * Nd
                  Nab = Na * Nb
                  Ncd = Nc * Nd
                  !
                  ! Compute two-electron integrals (BA|DC). Note the permuted order of shells!
                  !
                  call Auto2eERI(PtrOffset+auto2e_idx(Lb, La, Ld, Lc))%ptr( &
                        Gabcd, &
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
                        Kappa)
                  do w = 1, Nabcd
                        !
                        ! The minus sign is required so that K(p,q) adheres to the definition
                        ! K(p, q) <- K(p, q) + KCoeff * Rho(r, s) * (qr|ps)
                        !
                        Gabcd(w) = -KCoeff * Gabcd(w)
                  end do
                  !
                  ! Compute all contributions to the exchange matrix, which
                  ! originate from the computed batch of integrals (ab|cd).
                  ! Full permutational symmetry is used (8 contribs).
                  ! The flag EXCH_UVXY corresponds to +-Rho(v,y) (ab|cd).
                  !
                  ! Note that some contributions are added to K while others
                  ! are subtracted from K to account for the antisymmetry of Rho.
                  !
                  if (iand(exchbits, EXCH_ABCD) > 0) then
                        !
                        ! K(a,c) = K(a,c) - KCoeff * Rho(d,b) * (ab|cd)
                        !
                        call exch_digest_RhoAC(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload_m(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_ABDC) > 0) then
                        !
                        ! K(a,d) = K(a,d) - KCoeff * Rho(c,b) * (ab|cd)
                        !
                        call exch_digest_RhoAD(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload_m(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_BACD) > 0) then
                        !
                        ! K(b,c) = K(b,c) - KCoeff * Rho(d,a) * (ab|cd)
                        !
                        call exch_digest_RhoBC(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload_m(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_BADC) > 0) then
                        !
                        ! K(b,d) = K(b,d) - KCoeff * Rho(c,a) * (ab|cd)
                        !                       
                        call exch_digest_RhoBD(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload_m(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_CDAB) > 0) then
                        !
                        ! K(c,a) = K(c,a) + KCoeff * Rho(d,b) * (ab|cd)
                        !
                        call exch_digest_RhoCA(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_CDBA) > 0) then
                        !
                        ! K(c,b) = K(c,b) + KCoeff * Rho(d,a) * (ab|cd)
                        !
                        call exch_digest_RhoCB(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_DCAB) > 0) then
                        !
                        ! K(d,a) = K(d,a) + KCoeff * Rho(c,b) * (ab|cd)
                        !
                        call exch_digest_RhoDA(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_DCBA) > 0) then
                        !
                        ! K(d,b) = K(d,b) + KCoeff * Rho(c,a) * (ab|cd)
                        !
                        call exch_digest_RhoDB(Scratch, s0, s1, Rho, &
                              ShellB, Nb, ShellA, Na, ShellD, Nd, ShellC, Nc, Gabcd, ShellPairLoc)
                        call fock_Offload(K(s0:s1), Scratch, s1-s0+1)
                  end if
            end if
      end subroutine fock_ShellQuartet_AntiSymRho


      subroutine fock_Offload(a, Scratch, n)
            real(F64), dimension(:), intent(inout) :: a
            real(F64), dimension(:), intent(in)    :: Scratch
            integer, intent(in)                           :: n
            
            integer :: k

            do k = 1, n
                  !$omp atomic
                  a(k) = a(k) + Scratch(k)
            end do
      end subroutine fock_Offload


      subroutine fock_Offload_m(a, Scratch, n)
            real(F64), dimension(:), intent(inout) :: a
            real(F64), dimension(:), intent(in)    :: Scratch
            integer, intent(in)                    :: n
            
            integer :: k

            do k = 1, n
                  !$omp atomic
                  a(k) = a(k) - Scratch(k)
            end do
      end subroutine fock_Offload_m

      
      subroutine fock_SubsetQuartet(K, J, SubsetA, SubsetB, SubsetC, SubsetD, Rho, RhoMax, &
            VMax, IVMax, RhoVMax, IRhoVMax, ShellPairLoc, ShellSubsets, ShellCenters, AtomCoords, &
            ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, &
            Kappa, MaxNAngFunc, SpherAO, ExchContrib, CoulContrib, AntisymRho, &
            JCoeff, KCoeff, Thresh, QuartetFlags, QuartetLoc, Work, IWork)
            !
            ! (1) Determine the nonnegligible shell quartets of integrals used for
            ! the construction of the Fock matrix. The shell quartets belong to the given
            ! shell subsets A, B, C, and D.
            ! (2) Multiply the two-electron integrals by Rho (symmetric or antisymmetric)
            ! and update the Coulomb/exchange matrices.
            ! 
            ! The subsets A, B, C, and D should be canonically ordered because every
            ! allowed index permutation is employed by both screening and core compute
            ! subroutines.
            !
            real(F64), dimension(:), intent(inout)   :: K
            real(F64), dimension(:), intent(inout)   :: J
            integer, intent(in)                      :: SubsetA, SubsetB, SubsetC, SubsetD
            real(F64), dimension(:), intent(in)      :: Rho            
            real(F64), dimension(:, :), intent(in)   :: RhoMax
            real(F64), dimension(:, :), intent(in)   :: VMax
            integer, dimension(:, :), intent(in)     :: IVMax
            real(F64), dimension(:, :), intent(in)   :: RhoVMax
            integer, dimension(:, :), intent(in)     :: IRhoVMax
            integer, dimension(:, :), intent(in)     :: ShellPairLoc
            integer, dimension(:, :), intent(in)     :: ShellSubsets
            integer, dimension(:), intent(in)        :: ShellCenters
            real(F64), dimension(:, :), intent(in)   :: AtomCoords
            integer, dimension(:), intent(in)        :: ShellParamsIdx
            integer, dimension(:), intent(in)        :: ShellMomentum
            integer, dimension(:), intent(in)        :: NAngFunc
            integer, dimension(:), intent(in)        :: NPrimitives
            real(F64), dimension(:, :), intent(in)   :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)   :: Exponents
            real(F64), dimension(:, :), intent(in)   :: NormFactors
            real(F64), intent(in)                    :: Kappa
            integer, intent(in)                      :: MaxNAngFunc
            logical, intent(in)                      :: SpherAO
            logical, intent(in)                      :: ExchContrib
            logical, intent(in)                      :: CoulContrib
            logical, intent(in)                      :: AntisymRho
            real(F64), intent(in)                    :: JCoeff
            real(F64), intent(in)                    :: KCoeff
            real(F64), intent(in)                    :: Thresh
            integer, dimension(:), intent(out)       :: QuartetFlags
            integer, dimension(:), intent(out)       :: QuartetLoc
            real(F64), dimension(:), intent(out)     :: Work
            integer, dimension(:), intent(out)       :: IWork

            integer :: NQuartets
            integer :: n1, n2, n3, n4
            integer :: m1, m2, m3, m4
            integer :: Na, Nb, Nc, Nd
            integer :: ShellA, ShellB, ShellC, ShellD
            integer :: Flags, Idx
            integer :: v
            real(F64) :: ExchThresh, CoulThresh
            real(F64) :: JCoeffAB, JCoeffCD
            integer :: PtrOffset

            if (SpherAO) then
                  PtrOffset = AUTO2E_SPHER_OFFSET
            else
                  PtrOffset = 0
            end if
            if (abs(KCoeff) > ZERO) then
                  ExchThresh = Thresh / abs(KCoeff)
            else
                  ExchThresh = huge(ONE)
            end if
            if (abs(JCoeff) > ZERO) then
                  CoulThresh = Thresh / abs(JCoeff)
            else
                  CoulThresh = huge(ONE)
            end if
            
            NQuartets = 0
            QuartetLoc = 0
            QuartetFlags = 0
            
            Na = ShellSubsets(2, SubsetA) - ShellSubsets(1, SubsetA) + 1
            Nb = ShellSubsets(2, SubsetB) - ShellSubsets(1, SubsetB) + 1
            Nc = ShellSubsets(2, SubsetC) - ShellSubsets(1, SubsetC) + 1
            Nd = ShellSubsets(2, SubsetD) - ShellSubsets(1, SubsetD) + 1
            
            n1 = 1
            n2 = Na
            n3 = Na * Nb
            n4 = Na * Nb * Nc

            m1 = ShellSubsets(1, SubsetA)
            m2 = ShellSubsets(1, SubsetB)
            m3 = ShellSubsets(1, SubsetC)
            m4 = ShellSubsets(1, SubsetD)
            !
            ! Build a list of unique two-electron integrals contributing
            ! to the Coulomb and exchange matrices. For each unique integral,
            ! the bit flags set in QuartetFlags by the screening subroutines
            ! carry the information on which index permutations are accepted
            ! and are to be processed further.
            !
            JCoeffAB = JCoeff
            JCoeffCD = JCoeff
            if (SubsetA /= SubsetC .or. SubsetB /= SubsetD) then
                  if (ExchContrib) then
                        !
                        ! Exchange matrix
                        !
                        call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                              SubsetA, SubsetB, SubsetC, SubsetD, &
                              m1, m2, m3, m4, &
                              n1, n2, n3, n4, &
                              EXCH_ABCD, Work, IWork, ShellSubsets, ExchThresh)
                        call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                              SubsetC, SubsetD, SubsetA, SubsetB, &
                              m3, m4, m1, m2, &
                              n3, n4, n1, n2, &
                              EXCH_CDAB, Work, IWork, ShellSubsets, ExchThresh)
                  end if
                  if (CoulContrib) then
                        !
                        ! Coulomb matrix
                        !
                        call coul_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoVmax, IRhoVmax, Vmax, IVmax, &
                              SubsetA, SubsetB, SubsetC, SubsetD, &
                              m1, m2, m3, m4, &
                              n1, n2, n3, n4, &
                              COUL_ABCD, ShellSubsets, CoulThresh)
                        call coul_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoVmax, IRhoVmax, Vmax, IVmax, &
                              SubsetC, SubsetD, SubsetA, SubsetB, &
                              m3, m4, m1, m2, &
                              n3, n4, n1, n2, &
                              COUL_CDAB, ShellSubsets, CoulThresh)
                  end if
                  if ((SubsetA /= SubsetB) .and. (SubsetC /= SubsetD)) then
                        !
                        ! Coulomb contribution scaling:
                        ! J(a,b) <- J(a,b) + JCoeffAB * (ab|cd) * Rho(c,d)
                        ! J(c,d) <- J(c,d) + JCoeffCD * (cd|ab) * Rho(a,b)
                        !
                        JCoeffAB = JCoeff * TWO
                        JCoeffCD = JCoeff * TWO
                        if (ExchContrib) then
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetB, SubsetA, SubsetD, SubsetC, &
                                    m2, m1, m4, m3, &
                                    n2, n1, n4, n3, &
                                    EXCH_BADC, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetD, SubsetC, SubsetB, SubsetA, &
                                    m4, m3, m2, m1, &
                                    n4, n3, n2, n1, &
                                    EXCH_DCBA, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetA, SubsetB, SubsetD, SubsetC, &
                                    m1, m2, m4, m3, &
                                    n1, n2, n4, n3, &
                                    EXCH_ABDC, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetD, SubsetC, SubsetA, SubsetB, &
                                    m4, m3, m1, m2, &
                                    n4, n3, n1, n2, &
                                    EXCH_DCAB, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetB, SubsetA, SubsetC, SubsetD, &
                                    m2, m1, m3, m4, &
                                    n2, n1, n3, n4, &
                                    EXCH_BACD, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetC, SubsetD, SubsetB, SubsetA, &
                                    m3, m4, m2, m1, &
                                    n3, n4, n2, n1, &
                                    EXCH_CDBA, Work, IWork, ShellSubsets, ExchThresh)
                        end if
                  else if (SubsetC /= SubsetD) then
                        JCoeffAB = JCoeff * TWO
                        if (ExchContrib) then
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetA, SubsetB, SubsetD, SubsetC, &
                                    m1, m2, m4, m3, &
                                    n1, n2, n4, n3, &
                                    EXCH_ABDC, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetD, SubsetC, SubsetA, SubsetB, &
                                    m4, m3, m1, m2, &
                                    n4, n3, n1, n2, &
                                    EXCH_DCAB, Work, IWork, ShellSubsets, ExchThresh)
                        end if
                  else if (SubsetA /= SubsetB) then
                        JCoeffCD = JCoeff * TWO
                        if (ExchContrib) then
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetB, SubsetA, SubsetC, SubsetD, &
                                    m2, m1, m3, m4, &
                                    n2, n1, n3, n4, &
                                    EXCH_BACD, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetC, SubsetD, SubsetB, SubsetA, &
                                    m3, m4, m2, m1, &
                                    n3, n4, n2, n1, &
                                    EXCH_CDBA, Work, IWork, ShellSubsets, ExchThresh)
                        end if
                  end if
            else
                  if (ExchContrib) then
                        call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                              SubsetA, SubsetB, SubsetC, SubsetD, &
                              m1, m2, m3, m4, &
                              n1, n2, n3, n4, &
                              EXCH_ABCD, Work, IWork, ShellSubsets, ExchThresh)
                  end if
                  if (CoulContrib) then
                        call coul_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoVmax, IRhoVmax, Vmax, IVmax, &
                              SubsetA, SubsetB, SubsetC, SubsetD, &
                              m1, m2, m3, m4, &
                              n1, n2, n3, n4, &
                              COUL_ABCD, ShellSubsets, CoulThresh)
                  end if
                  if ((SubsetA /= SubsetB) .and. (SubsetC /= SubsetD)) then
                        JCoeffAB = JCoeff * TWO
                        JCoeffCD = JCoeff * TWO
                        if (ExchContrib) then
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetB, SubsetA, SubsetD, SubsetC, &
                                    m2, m1, m4, m3, &
                                    n2, n1, n4, n3, &
                                    EXCH_BADC, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetA, SubsetB, SubsetD, SubsetC, &
                                    m1, m2, m4, m3, &
                                    n1, n2, n4, n3, &
                                    EXCH_ABDC, Work, IWork, ShellSubsets, ExchThresh)
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetB, SubsetA, SubsetC, SubsetD, &
                                    m2, m1, m3, m4, &
                                    n2, n1, n3, n4, &
                                    EXCH_BACD, Work, IWork, ShellSubsets, ExchThresh)
                        end if
                  else if (SubsetC /= SubsetD) then
                        JCoeffAB = JCoeff * TWO
                        if (ExchContrib) then
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetA, SubsetB, SubsetD, SubsetC, &
                                    m1, m2, m4, m3, &
                                    n1, n2, n4, n3, &
                                    EXCH_ABDC, Work, IWork, ShellSubsets, ExchThresh)
                        end if
                  else if (SubsetA /= SubsetB) then
                        JCoeffCD = JCoeff * TWO
                        if (ExchContrib) then
                              call exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, VMax, IVMax, &
                                    SubsetB, SubsetA, SubsetC, SubsetD, &
                                    m2, m1, m3, m4, &
                                    n2, n1, n3, n4, &
                                    EXCH_BACD, Work, IWork, ShellSubsets, ExchThresh)
                        end if
                  end if
            end if
            if (.not. AntisymRho) then
                  do v = 1, NQuartets
                        !
                        ! Idx = (ShellA - m1) * n1 + (ShellB - m2) * n2 + &
                        ! (ShellC - m3) * n3 + (ShellD - m4) * n4 + 1
                        ! where
                        ! n1 < n2 < n3 < n4
                        !
                        Flags = ibits(QuartetFlags(v), 0, 10)
                        Idx = ishft(QuartetFlags(v), -10)
                        Idx = Idx - 1
                        ShellD = Idx / n4 + m4
                        Idx = Idx - (ShellD - m4) * n4
                        ShellC = Idx / n3 + m3
                        Idx = Idx - (ShellC - m3) * n3
                        ShellB = Idx / n2 + m2
                        Idx = Idx - (ShellB - m2) * n2
                        ShellA = Idx / n1 + m1
                        call fock_ShellQuartet_SymRho(K, J, Flags, ShellA, ShellB, ShellC, ShellD, &
                              KCoeff, JCoeffAB, JCoeffCD, Rho, ShellPairLoc, ShellCenters, AtomCoords, ShellParamsIdx, &
                              ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
                              MaxNAngFunc, PtrOffset)
                  end do
            else
                  do v = 1, NQuartets
                        Flags = ibits(QuartetFlags(v), 0, 10)
                        Idx = ishft(QuartetFlags(v), -10)
                        Idx = Idx - 1
                        ShellD = Idx / n4 + m4
                        Idx = Idx - (ShellD - m4) * n4
                        ShellC = Idx / n3 + m3
                        Idx = Idx - (ShellC - m3) * n3
                        ShellB = Idx / n2 + m2
                        Idx = Idx - (ShellB - m2) * n2
                        ShellA = Idx / n1 + m1
                        !
                        ! Antisymmetric density matrix. This code used in the case of complex
                        ! density matrices, e.g., in real-time TDDFT propagation.
                        ! Only the exchange part is computed because the Coulomb part
                        ! is exactly zero when Rho is antisymmetric.
                        !
                        call fock_ShellQuartet_AntiSymRho(K, Flags, ShellA, ShellB, ShellC, ShellD, &
                              KCoeff, Rho, ShellPairLoc, ShellCenters, AtomCoords, ShellParamsIdx, &
                              ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
                              MaxNAngFunc, PtrOffset)
                  end do
            end if
      end subroutine fock_SubsetQuartet


      subroutine fock_Rho1D(RhoMax, Rho1D, ShellPairLoc, Rho, ShellLoc, ShellParamsIdx, NAngFunc, NShells)
            real(F64), dimension(:, :), intent(out) :: RhoMax
            real(F64), dimension(:), intent(out)    :: Rho1D
            integer, dimension(:, :), intent(out)   :: ShellPairLoc
            real(F64), dimension(:, :), intent(in)  :: Rho
            integer, dimension(:), intent(in)       :: ShellLoc
            integer, dimension(:), intent(in)       :: ShellParamsIdx
            integer, dimension(:), intent(in)       :: NAngFunc
            integer, intent(in)                     :: NShells

            integer :: ShellA, ShellB
            integer :: ShellParamsA, ShellParamsB
            integer :: Na, Nb
            integer :: a0, a1, b0, b1, p0, p1
            integer :: n

            n = 1
            do ShellB = 1, NShells
                  do ShellA = 1, NShells
                        ShellParamsA = ShellParamsIdx(ShellA)
                        Na = NAngFunc(ShellParamsA)
                        ShellParamsB = ShellParamsIdx(ShellB)
                        Nb = NAngFunc(ShellParamsB)
                        ShellPairLoc(ShellA, ShellB) = n
                        p0 = ShellPairLoc(ShellA, ShellB)
                        p1 = ShellPairLoc(ShellA, ShellB) + Na * Nb - 1
                        a0 = ShellLoc(ShellA)
                        a1 = ShellLoc(ShellA) + Na - 1
                        b0 = ShellLoc(ShellB)
                        b1 = ShellLoc(ShellB) + Nb - 1
                        Rho1D(p0:p1) = reshape(Rho(a0:a1, b0:b1), [Na*Nb])
                        RhoMax(ShellA, ShellB) = maxval(abs(Rho1D(p0:p1)))
                        n = n + Na * Nb
                  end do
            end do
      end subroutine fock_Rho1D


      subroutine fock_F2D(F2D, F1D, ShellPairLoc, ShellLoc, ShellParamsIdx, NAngFunc, NShells)
            real(F64), dimension(:, :), intent(inout) :: F2D
            real(F64), dimension(:), intent(in)       :: F1D
            integer, dimension(:, :), intent(in)      :: ShellPairLoc
            integer, dimension(:), intent(in)         :: ShellLoc
            integer, dimension(:), intent(in)         :: ShellParamsIdx
            integer, dimension(:), intent(in)         :: NAngFunc
            integer, intent(in)                       :: NShells

            integer :: ShellA, ShellB
            integer :: ShellParamsA, ShellParamsB
            integer :: Na, Nb
            integer :: a0, a1, b0, b1, p0, p1

            do ShellB = 1, NShells
                  do ShellA = 1, NShells
                        ShellParamsA = ShellParamsIdx(ShellA)
                        Na = NAngFunc(ShellParamsA)
                        ShellParamsB = ShellParamsIdx(ShellB)
                        Nb = NAngFunc(ShellParamsB)
                        p0 = ShellPairLoc(ShellA, ShellB)
                        p1 = ShellPairLoc(ShellA, ShellB) + Na * Nb - 1
                        a0 = ShellLoc(ShellA)
                        a1 = ShellLoc(ShellA) + Na - 1
                        b0 = ShellLoc(ShellB)
                        b1 = ShellLoc(ShellB) + Nb - 1
                        F2D(a0:a1, b0:b1) = F2D(a0:a1, b0:b1) + reshape(F1D(p0:p1), [Na, Nb])
                  end do
            end do
      end subroutine fock_F2D
      

      subroutine fock_ShellSubsets(SubsetBounds, SubsetDim, NSubsets, MaxSubsetDim, &
            NShells, NAO, ShellParamsIdx, NAngFunc)
            integer, dimension(:, :), allocatable, intent(out) :: SubsetBounds
            integer, dimension(:), allocatable, intent(out)    :: SubsetDim
            integer, intent(out)                               :: NSubsets
            integer, intent(out)                               :: MaxSubsetDim
            integer, intent(in)                                :: NShells
            integer, intent(in)                                :: NAO
            integer, dimension(:), intent(in)                  :: ShellParamsIdx
            integer, dimension(:), intent(in)                  :: NAngFunc

            integer :: AvgNSubsets, TargetDim, k, s
            integer, dimension(3) :: BoundErr, BoundLoc
            integer :: MaxNSubsets
            !
            ! Max number of AO indices in a single subset. Note that MaxBlockDim
            ! corresponds to the total number of angular functions and not to the nubmer
            ! of orbital shells.
            !
            ! Tradeoffs when choosing the value of MaxBlockDim
            !
            ! Bigger MaxBlockDim =>
            ! (i) reduces some overheads thanks to earlier loop exits in
            ! the integral screening subroutines
            ! (ii) less efficient parallelization because of the lower number
            ! of parallel loop cycles
            ! (iii) bigger arrays to allocate, e.g., QuartetFlags.
            ! Can be a deciding factor when there's a large number of threads.
            !
            integer, parameter :: MaxBlockDim = 50
            integer, parameter :: MinNSubsets = 5
            integer, parameter :: MinNShells = 2

            if (NShells < MinNSubsets) then
                  call msg("Cannot form proper subsets of orbital shells: not enough shells", MSG_ERROR)
                  error stop
            end if
            MaxNSubsets = NShells / MinNShells
            allocate(SubsetBounds(2, MaxNSubsets))
            allocate(SubsetDim(MaxNSubsets))
            AvgNSubsets = NAO / MaxBlockDim
            if (modulo(NAO, MaxBlockDim) > 0) AvgNSubsets = AvgNSubsets + 1
            TargetDim = NAO / max(AvgNSubsets,MinNSubsets)
            if (modulo(NAO, max(AvgNSubsets,MinNSubsets)) > 0) TargetDim = TargetDim + 1
            do k = 1, MaxNSubsets
                  if (k == 1) then
                        SubsetBounds(1, k) = 1
                  else
                        if (SubsetBounds(2, k-1) < NShells .and. &
                              SubsetBounds(2, k-1) > 0) then
                              SubsetBounds(1, k) = SubsetBounds(2, k-1) + 1
                        else
                              SubsetBounds(1, k) = 0
                        end if
                  end if
                  SubsetDim(k) = 0
                  SubsetBounds(2, k) = SubsetBounds(1, k)
                  if (SubsetBounds(1, k) > 0) then
                        if (k < MaxNSubsets) then
                              BoundErr = 0
                              BoundLoc = 0
                              do s = SubsetBounds(1, k), NShells
                                    SubsetDim(k) = SubsetDim(k) + NAngFunc(ShellParamsIdx(s))
                                    if (s - SubsetBounds(1,k) + 1 >= MinNShells .and. (NShells-s >= MinNShells .or. s==NShells)) then
                                          if (SubsetDim(k) >= TargetDim) then
                                                BoundLoc(2) = s
                                                BoundErr(2) = SubsetDim(k) - TargetDim
                                                exit
                                          else
                                                BoundLoc(1) = s
                                                BoundErr(1) = SubsetDim(k) - TargetDim
                                          end if
                                    end if
                              end do
                              if (BoundLoc(1) > 0 .and. BoundLoc(2) > 0) then
                                    if (abs(BoundErr(1)) < abs(BoundErr(2))) then
                                          SubsetBounds(2, k) = BoundLoc(1)
                                          SubsetDim(k) = BoundErr(1) + TargetDim
                                    else
                                          SubsetBounds(2, k) = BoundLoc(2)
                                          SubsetDim(k) = BoundErr(2) + TargetDim
                                    end if
                              else if (BoundLoc(1) > 0) then
                                    SubsetBounds(2, k) = BoundLoc(1)
                                    SubsetDim(k) = BoundErr(1) + TargetDim
                              else if (BoundLoc(2) > 0) then
                                    SubsetBounds(2, k) = BoundLoc(2)
                                    SubsetDim(k) = BoundErr(2) + TargetDim
                              else
                                    SubsetBounds(2, k) = NShells
                                    if (k > 1) then
                                          SubsetDim(k) = NAO - sum(SubsetDim(1:k-1))
                                    else
                                          SubsetDim(k) = NAO
                                    end if
                              end if
                        else
                              SubsetBounds(2, k) = NShells
                              if (k > 1) then
                                    SubsetDim(k) = NAO - sum(SubsetDim(1:k-1))
                              else
                                    SubsetDim(k) = NAO
                              end if
                        end if
                  end if
            end do
            NSubsets = 0
            do k = 1, MaxNSubsets
                  if (SubsetDim(k) > 0) then
                        NSubsets = NSubsets + 1
                  end if
            end do
            MaxSubsetDim = 0
            do k = 1, NSubsets
                  MaxSubsetDim = max(MaxSubsetDim,SubsetBounds(2,k)-SubsetBounds(1,k)+1)
            end do
      end subroutine fock_ShellSubsets


      subroutine fock_SubsetLoop(K, J, Rho, RhoMax, VMax, IVmax, RhoVMax, IRhoVMax, ShellPairLoc, &
            ShellSubsets, NSubsets, MaxSubsetDim, AOBasis, NAngFunc, MaxNAngFunc, NormFactors, SpherAO, &
            ExchContrib, KCoeff, Kappa, AntisymRho, CoulContrib, JCoeff, Thresh)
            !
            ! Integral-direct algorithm  for HF exchange / Coulomb matrices based on the ONX algorithm
            ! reported in the literature (linear scaling exchange for large systems) but modified to
            ! use the full permutational symmetry of integrals, as well as to reuse the same integrals
            ! between exchange and Coulomb matrices.
            !
            ! The products Rho(b,d)*(ab|cd) and Rho(c,d)*(ab|cd) which contribute to the Coulomb and
            ! exchange matrices are screened using the Schwarz inequality:
            !
            ! |JCoeff*Rho(c,d)*(ab|cd)| > Thresh
            ! |KCoeff*Rho(b,d)*(ab|cd)| > Thresh
            ! 
            real(F64), dimension(:), intent(inout)  :: K
            real(F64), dimension(:), intent(inout)  :: J
            real(F64), dimension(:), intent(in)     :: Rho
            real(F64), dimension(:, :), intent(in)  :: RhoMax
            real(F64), dimension(:, :), intent(in)  :: VMax
            integer, dimension(:, :), intent(in)    :: IVMax
            real(F64), dimension(:, :), intent(in)  :: RhoVMax
            integer, dimension(:, :), intent(in)    :: IRhoVMax
            integer, dimension(:, :), intent(in)    :: ShellPairLoc
            integer, dimension(:, :), intent(in)    :: ShellSubsets
            integer, intent(in)                     :: NSubsets
            integer, intent(in)                     :: MaxSubsetDim
            type(TAOBasis), intent(in)              :: AOBasis
            integer, dimension(:), intent(in)       :: NAngFunc
            integer, intent(in)                     :: MaxNAngFunc
            real(F64), dimension(:, :), intent(in)  :: NormFactors
            logical, intent(in)                     :: SpherAO
            logical, intent(in)                     :: ExchContrib
            real(F64), intent(in)                   :: KCoeff
            real(F64), intent(in)                   :: Kappa
            logical, intent(in)                     :: AntisymRho
            logical, intent(in)                     :: CoulContrib
            real(F64), intent(in)                   :: JCoeff
            real(F64), intent(in)                   :: Thresh
            
            integer :: SubsetA, SubsetB, SubsetC, SubsetD
            integer :: pqrs, pq, rs, p, q, r, s
            integer :: Nab, Nabcd
            integer, dimension(:), allocatable :: QuartetLoc, QuartetFlags
            real(F64), dimension(MaxSubsetDim) :: Work
            integer, dimension(MaxSubsetDim) :: IWork
            integer :: ThisImage, NImages            

            Nab = (NSubsets * (NSubsets + 1)) / 2
            Nabcd = (Nab * (Nab + 1)) / 2
            NImages = num_images()
            ThisImage = this_image()
            !$omp parallel &
            !$omp private(SubsetA, SubsetB, SubsetC, SubsetD) &
            !$omp private(pqrs, pq, rs, p, q, r, s) &
            !$omp private(QuartetFlags, QuartetLoc, Work, IWork) &
            !$omp default(shared)
            allocate(QuartetFlags(MaxSubsetDim**4))
            allocate(QuartetLoc(MaxSubsetDim**4))
            !$omp do schedule(dynamic)
            !
            ! A concurrent Fortran image will perform only the loop cycles
            ! for which modulo(pqrs,NImages)==ThisImage
            !
            do pqrs = ThisImage, Nabcd, NImages
                  call fock_DecodePQ(pqrs, Nab, pq, rs)
                  call fock_DecodePQ(pq, NSubsets, p, q)
                  call fock_DecodePQ(rs, NSubsets, r, s)
                  !
                  ! The algorithm used in the subroutine DecodePQ guarantees that p>=q and r>=s.
                  ! However, due to the way the set of index pairs is traversed, it's not guaranteed
                  ! that pq>=rs. The complete canonical order is enforced by the following conditions.
                  !
                  if (p > r .or. (p == r .and. q > s)) then
                        SubsetA = p
                        SubsetB = q
                        SubsetC = r
                        SubsetD = s
                  else
                        SubsetA = r
                        SubsetB = s
                        SubsetC = p
                        SubsetD = q
                  end if
                  call fock_SubsetQuartet(K, J, SubsetA, SubsetB, SubsetC, SubsetD, Rho, RhoMax, &
                        VMax, IVMax, RhoVMax, IRhoVMax, ShellPairLoc, ShellSubsets, &
                        AOBasis%ShellCenters, &
                        AOBasis%AtomCoords, &
                        AOBasis%ShellParamsIdx, &
                        AOBasis%ShellMomentum, &
                        NAngFunc, &
                        AOBasis%NPrimitives, &
                        AOBasis%CntrCoeffs, &
                        AOBasis%Exponents, &
                        NormFactors, &
                        Kappa, &
                        MaxNAngFunc, &
                        SpherAO, &
                        ExchContrib, CoulContrib, AntisymRho, &
                        JCoeff, KCoeff, Thresh, QuartetFlags, QuartetLoc, Work, IWork)
            end do
            !$omp end do
            !
            ! Explicit deallocation is needed here because implicit deallocation
            ! is not guaranteed to work inside parallel blocks.
            !
            deallocate(QuartetFlags, QuartetLoc)
            !$omp end parallel
      end subroutine fock_SubsetLoop


      pure subroutine fock_DecodePQ(pq, n, p, q)
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
      end subroutine fock_DecodePQ


      function fock_RhoTrace(Rho, A)
            !
            ! Calculate the average value <A> = Tr(Rho A).
            ! Only the lower triangles of Rho and A are referenced.
            !
            real(F64)                              :: fock_RhoTrace
            real(F64), dimension(:, :), intent(in) :: Rho
            real(F64), dimension(:, :), intent(in) :: A

            integer :: i, j, NAO

            NAO = size(Rho, dim=1)
            fock_RhoTrace = ZERO
            do j = 1, NAO
                  fock_RhoTrace = fock_RhoTrace + (ONE/TWO) * Rho(j, j) * A(j, j)
                  do i = j + 1, NAO
                        fock_RhoTrace = fock_RhoTrace + Rho(i, j) * A(i, j)
                  end do
            end do
            fock_RhoTrace = TWO * fock_RhoTrace
      end function fock_RhoTrace
      

      subroutine fock_BufferDim(DimJK, DimRho1D, AOBasis)
            integer, intent(out)       :: DimJK
            integer, intent(out)       :: DimRho1D
            type(TAOBasis), intent(in) :: AOBasis

            integer :: NAO
            
            if (AOBasis%SpherAO) then
                  NAO = AOBasis%NAOSpher
            else
                  NAO = AOBasis%NAOCart
            end if
            DimJK = NAO**2
            DimRho1D = NAO**2
      end subroutine fock_BufferDim
      
      
      subroutine fock_JK(F_ao, K, J, Rho1D, Rho_ao, AOBasis, ExchContrib, KFrac, &
            AntisymRho, LCExchange, SHExchange, Omega, CoulContrib, Thresh, Vx_ao)
            real(F64), dimension(:, :, :), intent(out)   :: F_ao
            real(F64), dimension(:), intent(out)         :: K
            real(F64), dimension(:), intent(out)         :: J
            real(F64), dimension(:), intent(out)         :: Rho1D
            real(F64), dimension(:, :, :), intent(in)    :: Rho_ao
            type(TAOBasis), intent(in)                   :: AOBasis
            logical, intent(in)                          :: ExchContrib
            real(F64), intent(in)                        :: KFrac
            logical, intent(in)                          :: AntisymRho
            logical, intent(in)                          :: LCExchange
            logical, intent(in)                          :: SHExchange
            real(F64), intent(in)                        :: Omega
            logical, intent(in)                          :: CoulContrib
            real(F64), intent(in)                        :: Thresh
            real(F64), dimension(:, :, :), optional, intent(out) :: Vx_ao

            integer :: s, p
            integer :: NAO, NShells
            integer :: NSpins, MaxSubsetDim, MaxNAngFunc, NSubsets
            logical :: SpinUnres
            real(F64) :: Kappa
            integer, dimension(:), allocatable :: NAngFunc, ShellLoc
            real(F64), dimension(:, :), allocatable :: NormFactors
            integer, dimension(:, :), allocatable :: ShellPairLoc
            integer, dimension(:, :), allocatable :: SubsetBounds
            integer, dimension(:), allocatable :: SubsetDim
            integer, dimension(:, :), allocatable :: IVMax, IRhoVMax
            real(F64), dimension(:, :), allocatable :: RhoMax, VMax, RhoVMax
            logical :: SpherAO
            integer :: ThisImage, NImages
            real(F64) :: KCoeff, KCoeffLR, KFracLR
            real(F64), parameter :: JCoeff = ONE

            NImages = num_images()
            ThisImage = this_image()
            NSpins = size(Rho_ao, dim=3)
            SpinUnres = (NSpins > 1)
            F_ao = ZERO
            if (present(Vx_ao)) then
                  Vx_ao = ZERO
            end if
            !
            ! Range-separation parameter for the range-separated
            ! Coulomb operator 1/r=erfc(omega*r)/r + erf(omega*r)/r
            !
            if (Omega > ZERO .and. (LCExchange .or. SHExchange)) then
                  Kappa = ONE / Omega**2
            else
                  Kappa = ZERO
            end if
            if (SpinUnres) then
                  KCoeff = -KFrac
            else
                  KCoeff = -(ONE/TWO) * KFrac
            end if
            if (LCExchange .or. SHExchange) then
                  !
                  ! Let a and b represent a portion of the exact exchange
                  ! at short and long range, respectively. We will first evaluate
                  ! the exchange integrals with the full 1/R12 operator and subsequently,
                  ! in a separatete call to the main computational subroutine, the long-range
                  ! exchange integrals with the operator Erf(Omega*R12)/R12.
                  !
                  ! a <- fraction of short-range exchange
                  ! b <- fraction of long-range exchange
                  ! a E_x^{HF,SR} + b E_x^{HF,LR} =
                  ! a E_x^{HF} + (b - a) E_x^{HF,LR}
                  !
                  if (SHExchange) then
                        KFracLR = ZERO
                  else
                        KFracLR = ONE
                  end if
                  if (SpinUnres) then
                        KCoeffLR = -(KFracLR - KFrac)
                  else
                        KCoeffLR = -(ONE/TWO) * (KFracLR - KFrac)
                  end if
            else
                  KCoeffLR = ZERO
            end if
            if (AOBasis%SpherAO) then
                  MaxNAngFunc = 2 * AOBasis%LmaxGTO + 1
                  allocate(NAngFunc, source=AOBasis%NAngFuncSpher)
                  allocate(NormFactors, source=AOBasis%NormFactorsSpher)
                  allocate(ShellLoc, source=AOBasis%ShellLocSpher)
                  NAO = AOBasis%NAOSpher
                  NShells = AOBasis%NShells
                  SpherAO = .true.
            else
                  MaxNAngFunc = ((AOBasis%LmaxGTO + 1) * (AOBasis%LmaxGTO + 2)) / 2
                  allocate(NAngFunc, source=AOBasis%NAngFuncCart)
                  allocate(NormFactors, source=AOBasis%NormFactorsCart)
                  allocate(ShellLoc, source=AOBasis%ShellLocCart)
                  NAO = AOBasis%NAOCart
                  NShells = AOBasis%NShells                  
                  SpherAO = .false.
            end if
            allocate(RhoMax(NShells, NShells))
            allocate(VMax(NShells, NShells))
            allocate(IVMax(NShells, NShells))
            allocate(RhoVMax(NShells, NShells))
            allocate(IRhoVMax(NShells, NShells))
            allocate(ShellPairLoc(NShells, NShells))
            call fock_ShellSubsets(SubsetBounds, SubsetDim, NSubsets, MaxSubsetDim, NShells, NAO, &
                  AOBasis%ShellParamsIdx, NAngFunc)
            do s = 1, NSpins
                  K = ZERO
                  J = ZERO
                  !
                  ! Translate the density matrix into a one-dimensional array where
                  ! the matrix elements corresponding to the angular functions of
                  ! a bra-ket shell pair are kept in contiguous memory locations.
                  ! This type of storage enables an efficient memory access during
                  ! the Fock matrix build.
                  !
                  call fock_Rho1D(RhoMax, Rho1D, ShellPairLoc, Rho_ao(:, :, s), ShellLoc, &
                        AOBasis%ShellParamsIdx, NAngFunc, NShells)
                  !
                  ! Compute intermediates employed for integral screening
                  !
                  call fock_VMax(VMax, IVMax, RhoVMax, IRhoVMax, Rho1D, ShellPairLoc, SubsetBounds, NSubsets, &
                        AOBasis%ShellCenters, &
                        AOBasis%AtomCoords, &
                        AOBasis%ShellParamsIdx, &
                        AOBasis%ShellMomentum, &
                        NAngFunc, &
                        AOBasis%NPrimitives, &
                        AOBasis%CntrCoeffs, &
                        AOBasis%Exponents, &
                        NormFactors, &
                        ZERO, MaxNAngFunc, MaxSubsetDim, &
                        SpherAO)
                  call fock_SubsetLoop(K, J, Rho1D, RhoMax, VMax, IVmax, RhoVMax, IRhoVMax, ShellPairLoc, &
                        SubsetBounds, NSubsets, MaxSubsetDim, AOBasis, NAngFunc, MaxNAngFunc, NormFactors, &
                        SpherAO, (abs(KCoeff)>ZERO), KCoeff, ZERO, AntisymRho, CoulContrib, JCoeff, Thresh)
                  if (SHExchange .or. LCExchange) then
                        call fock_VMax(VMax, IVMax, RhoVMax, IRhoVMax, Rho1D, ShellPairLoc, SubsetBounds, NSubsets, &
                              AOBasis%ShellCenters, &
                              AOBasis%AtomCoords, &
                              AOBasis%ShellParamsIdx, &
                              AOBasis%ShellMomentum, &
                              NAngFunc, &
                              AOBasis%NPrimitives, &
                              AOBasis%CntrCoeffs, &
                              AOBasis%Exponents, &
                              NormFactors, &
                              Kappa, MaxNAngFunc, MaxSubsetDim, &
                              SpherAO)
                        call fock_SubsetLoop(K, J, Rho1D, RhoMax, VMax, IVmax, RhoVMax, IRhoVMax, ShellPairLoc, &
                              SubsetBounds, NSubsets, MaxSubsetDim, AOBasis, NAngFunc, MaxNAngFunc, NormFactors, &
                              SpherAO, .true., KCoeffLR, Kappa, AntisymRho, .false., ZERO, Thresh)
                  end if
                  if (ExchContrib) then
                        call fock_F2D(F_ao(:, :, s), K, ShellPairLoc, ShellLoc, &
                              AOBasis%ShellParamsIdx, NAngFunc, NShells)
                        if (present(Vx_ao)) then
                              call fock_F2D(Vx_ao(:, :, s), K, ShellPairLoc, ShellLoc, &
                                    AOBasis%ShellParamsIdx, NAngFunc, NShells)
                        end if
                  end if
                  if (CoulContrib) then
                        do p = 1, NSpins
                              call fock_F2D(F_ao(:, :, p), J, ShellPairLoc, ShellLoc, &
                                    AOBasis%ShellParamsIdx, NAngFunc, NShells)
                        end do
                  end if
            end do
      end subroutine fock_JK
end module Fock
