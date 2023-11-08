module rpa_CC_Singles
      use arithmetic
      use math_constants
      use real_linalg
      use rpa_definitions
      use ParallelCholesky

      implicit none
      
contains

      subroutine rpa_CC_Singles_Summary(T1Approx)
            integer, intent(in) :: T1Approx

            call msg("T1 amplitudes")
            select case (T1Approx)
            case (RPA_T1_DIRECT_RING_CCSD)
                  call msg(lfield("", 30) // "single-iteration solution of the direct ring singles equation (WORK IN PROGRESS)")
                  call msg(lfield("", 30) // "0=Lambda*Fai + Tai*(Ea-Ei) + 2*Lambda*Sum(ck)(Tck*Vckai+Fck*Tckai)")
                  call msg(lfield("", 30) // "  + 4*Lambda*Sum(bjck)Tck*Vckbj*Tbjai")
            case (RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE)
                  call msg(lfield("", 30) // "single-iteration solution of the direct ring+exchange singles equation (WORK IN PROGRESS)")
                  call msg(lfield("", 30) // "0=Lambda*Fai + Tai*(Ea-Ei) + Lambda*Sum(ck)(Tck*(2*Vckai-Vciak)+Fck*(2*Tckai-Tciak))")
                  call msg(lfield("", 30) // "  + 4*Lambda*Sum(bjck)Tck*Vckbj*Tbjai - 2*Lambda*Sum(bjck)Tck*Vcjbk*Tbjai")
            case (RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE_PLUS_LADDER)
                  call msg(lfield("", 30) // "single-iteration solution of the direct ring+exchange singles equation (WORK IN PROGRESS)")
                  call msg(lfield("", 30) // "0=Lambda*Fai + Tai*(Ea-Ei) + Lambda*Sum(ck)(Tck*(2*Vckai-Vciak)+Fck*(2*Tckai-Tciak))")
                  call msg(lfield("", 30) // "  + 4*Lambda*Sum(bjck)Tck*Vckbj*Tbjai - 2*Lambda*Sum(bjck)Tck*Vcjbk*Tbjai")
                  call msg(lfield("", 30) // "  + 2*Lambda*Sum(bcj)Vabcj*Tcjbi - 2*Lambda*Sum(bjk)Vijbk*Tbkaj")
            case (RPA_T1_MEAN_FIELD) 
                  call msg(lfield("", 30) // "lambda-dependent mean-field operator summed up to infinity")
            end select
      end subroutine rpa_CC_Singles_Summary


      subroutine rpa_RCxyT(RCxy, R, ldR, Cxy, Nxy, Npq, NVecs)
            real(F64), dimension(NVecs, Nxy), intent(inout) :: RCxy
            real(F64), dimension(ldR, Npq), intent(in)      :: R
            integer, intent(in)                             :: ldR
            real(F64), dimension(Nxy, Npq), intent(in)      :: Cxy
            integer, intent(in)                             :: Nxy
            integer, intent(in)                             :: Npq
            integer, intent(in)                             :: NVecs
            
            call real_abT_x(RCxy, NVecs, R, ldR, Cxy, Nxy, NVecs, &
                  Nxy, Npq, alpha=ONE, beta=ONE)
      end subroutine rpa_RCxyT
      
      
      subroutine rpa_CxT(Cxp, Cpx, NX, NAO)
            real(F64), dimension(NX, NAO), intent(out) :: Cxp
            real(F64), dimension(NAO, NX), intent(in)  :: Cpx
            integer, intent(in)                        :: NX
            integer, intent(in)                        :: NAO

            Cxp = transpose(Cpx)
      end subroutine rpa_CxT
      
      
      subroutine rpa_Cxy(Cxy, SubsetBounds, XCoeffs, YCoeffs, NX, NY, Npq, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, &
            NAngFunc, NAO)
            
            real(F64), dimension(NX*NY, Npq), intent(out)                :: Cxy
            integer, dimension(2), intent(in)                            :: SubsetBounds
            real(F64), dimension(NX, NAO), intent(in)                    :: XCoeffs
            real(F64), dimension(NY, NAO), intent(in)                    :: YCoeffs
            integer, intent(in)                                          :: NX
            integer, intent(in)                                          :: NY
            integer, intent(in)                                          :: Npq
            integer, dimension(:, :), intent(in)                         :: ShellPairs
            integer, dimension(:, :), intent(in)                         :: ShellPairLoc
            integer, dimension(:), intent(in)                            :: ShellPairDim
            integer, dimension(:), intent(in)                            :: ShellLoc
            integer, dimension(:), intent(in)                            :: ShellParamsIdx
            integer, dimension(:), intent(in)                            :: NAngFunc
            integer, intent(in)                                          :: NAO

            integer :: ShAB, LocAB, Nab, pq0, pq1
            integer :: ShA, ShellParamsA, Na, LocA
            integer :: ShB, ShellParamsB, Nb, LocB

            !$omp parallel do schedule(guided) &
            !$omp default(shared) &
            !$omp private(ShA, Na, ShellParamsA, LocA) &
            !$omp private(ShB, Nb, ShellParamsB, LocB) &
            !$omp private(Nab, LocAB) &
            !$omp private(pq0, pq1) &
            !$omp private(ShAB)
            do ShAB = SubsetBounds(1), SubsetBounds(2)
                  LocAB = ShellPairLoc(SUBSET_STORAGE, ShAB)
                  Nab = ShellPairDim(ShAB)
                  pq0 = LocAB
                  pq1 = LocAB + Nab - 1
                  
                  ShA = ShellPairs(1, ShAB)
                  ShellParamsA = ShellParamsIdx(ShA)
                  Na = NAngFunc(ShellParamsA)
                  LocA = ShellLoc(ShA)
                  
                  ShB = ShellPairs(2, ShAB)
                  ShellParamsB = ShellParamsIdx(ShB)
                  Nb = NAngFunc(ShellParamsB)
                  LocB = ShellLoc(ShB)

                  if (ShA /= ShB) then
                        call rpa_Cxy_ab(Cxy(:, pq0:pq1), XCoeffs, YCoeffs, &
                              Na, Nb, LocA, LocB, NX, NY)
                  else
                        call rpa_Cxy_aa(Cxy(:, pq0:pq1), XCoeffs, YCoeffs, &
                              Na, LocA, NX, NY)
                  end if
            end do
            !$omp end parallel do
      end subroutine rpa_Cxy


      subroutine rpa_Cxy_ab(C, XCoeffs, YCoeffs, Na, Nb, LocA, LocB, NX, NY)
            !
            ! ShA /= ShB
            ! ShC /= ShD
            !
            real(F64), dimension(NX, NY, *), intent(out)      :: C
            real(F64), dimension(NX, *), intent(in)           :: XCoeffs
            real(F64), dimension(NY, *), intent(in)           :: YCoeffs
            integer, intent(in)                               :: Na
            integer, intent(in)                               :: Nb
            integer, intent(in)                               :: LocA
            integer, intent(in)                               :: LocB
            integer, intent(in)                               :: NX
            integer, intent(in)                               :: NY
            
            integer :: pp, qq
            integer :: p, q, pq
            integer :: OffsetP, OffsetQ
            integer :: iota

            OffsetP = LocA - 1
            OffsetQ = LocB - 1
            do q = 1, Nb
                  do p = 1, Na
                        pp = OffsetP + p
                        qq = OffsetQ + q
                        pq = p+(q-1)*Na
                        do iota = 1, NY
                              C(:, iota, pq) = YCoeffs(iota, pp) * XCoeffs(:, qq) + YCoeffs(iota, qq) * XCoeffs(:, pp)
                        end do
                  end do
            end do
      end subroutine rpa_Cxy_ab


      subroutine rpa_Cxy_aa(C, XCoeffs, YCoeffs, Na, LocA, NX, NY)
            !
            ! ShA == ShB
            !
            real(F64), dimension(NX, NY, *), intent(out) :: C
            real(F64), dimension(NX, *), intent(in)      :: XCoeffs
            real(F64), dimension(NY, *), intent(in)      :: YCoeffs
            integer, intent(in)                          :: Na
            integer, intent(in)                          :: LocA
            integer, intent(in)                          :: NX
            integer, intent(in)                          :: NY
            
            integer :: pp, qq
            integer :: p, q, pq
            integer :: OffsetP, OffsetQ
            integer :: iota

            OffsetP = LocA - 1
            OffsetQ = LocA - 1
            pq = 1
            do q = 1, Na
                  !
                  ! Diagonal
                  !
                  p = q
                  pp = OffsetP + p
                  qq = OffsetQ + q
                  do iota = 1, NY
                        C(:, iota, pq) = YCoeffs(iota, pp) * XCoeffs(:, qq)
                  end do
                  pq = pq + 1
                  !
                  ! Off-diagonal
                  !
                  do p = q + 1, Na
                        pp = OffsetP + p
                        qq = OffsetQ + q
                        do iota = 1, NY
                              C(:, iota, pq) = YCoeffs(iota, pp) * XCoeffs(:, qq) &
                                    + YCoeffs(iota, qq) * XCoeffs(:, pp)
                        end do
                        pq = pq + 1
                  end do
            end do
      end subroutine rpa_Cxy_aa


      subroutine rpa_CC_UT2vv_W2kbi(W2kbi, Vlbi, Vkai, Ak, NVecsT2, W1, NOcc, NVirt, Nb, b0, b1, NVecsPiU)
            real(F64), dimension(NVecsPiU, Nb, NOcc), intent(out)  :: W2kbi
            real(F64), dimension(NVecsT2, Nb), intent(out)         :: Vlbi
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in) :: Vkai
            real(F64), dimension(NVecsT2), intent(in)              :: Ak
            integer, intent(in)                                    :: NVecsT2
            real(F64), dimension(NVecsPiU, NVecsT2), intent(in)    :: W1
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: Nb
            integer, intent(in)                                    :: b0, b1
            integer, intent(in)                                    :: NVecsPiU
            
            integer :: i, b
            
            do i = 1, NOcc
                  Vlbi = Vkai(:, b0:b1, i)
                  do b = 1, Nb
                        Vlbi(:, b) = Ak(:) * Vlbi(:, b)
                  end do
                  !
                  ! W2(K,bi) = Sum(L) W1(K,L)*V(L,bi)*A(L) ! computed for fixed i; dimension (NVecsPiU,Nb)
                  !
                  call real_ab(W2kbi(:, 1:Nb, i), W1, Vlbi(:, 1:Nb))
            end do
      end subroutine rpa_CC_UT2vv_W2kbi

      
      subroutine rpa_CC_UT2vv_Direct(UT2vv, W1, Rkpq, Vkai, Ak, VirtCoeffs, NOcc, NVirt, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
            SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, MaxVirtBatch)
            !
            ! UT2vv(a, i) = Sum(bcj) (ab|cj) T(cj,bi)
            !             = Sum(bcj) Sum(KL) R(K,ab) R(K,cj) V(L,cj) V(L,bi) A(L)
            !             = Sum(Kb)  R(K,ab) W2(K,bi)
            !
            ! Intermediates
            ! --------------
            !
            ! W1(K,L) = Sum(cj) R(K,cj)*V(L,cj)
            ! W2(K,bi) = Sum(L) W1(K,L)*V(L,bi)*A(L)
            !
            real(F64), dimension(NVirt, NOcc), intent(out)             :: UT2vv
            real(F64), dimension(NVecsPiU, NVecsT2), intent(in)        :: W1
            real(F64), dimension(:, :, :), intent(in)                  :: Rkpq[*]
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)     :: Vkai
            real(F64), dimension(NVecsT2), intent(in)                  :: Ak
            real(F64), dimension(:, :), intent(in)                     :: VirtCoeffs
            integer, intent(in)                                        :: NOcc
            integer, intent(in)                                        :: NVirt
            integer, dimension(:, :), intent(in)                       :: ShellPairs
            integer, dimension(:, :), intent(in)                       :: ShellPairLoc
            integer, dimension(:), intent(in)                          :: ShellPairDim
            integer, dimension(:), intent(in)                          :: ShellLoc
            integer, dimension(:), intent(in)                          :: ShellParamsIdx
            integer, dimension(:, :), intent(in)                       :: SubsetBounds
            integer, dimension(:), intent(in)                          :: SubsetDim
            integer, dimension(2), intent(in)                          :: NSubsets
            integer, dimension(:), intent(in)                          :: NAngFunc
            integer, intent(in)                                        :: NAO
            integer, intent(in)                                        :: NVecsPiU
            integer, intent(in)                                        :: NVecsT2
            integer, intent(in)                                        :: MaxVirtBatch

            integer :: ldR
            integer :: b0, b1
            integer :: Na, Nb, Nab
            real(F64), dimension(:, :), allocatable :: Rkba
            real(F64), dimension(:, :), allocatable :: W2kbi
            real(F64), dimension(:, :), allocatable :: Vlbi
            real(F64), dimension(:), allocatable :: CaT, CbT
            real(F64), dimension(:), allocatable :: Cba
            integer :: X, Y, Npq, Nabpq
            integer :: MaxSubsetDim, SubsetIdx
            integer :: ThisImage

            ThisImage = this_image()
            ldR = size(Rkpq, dim=1)
            allocate(Rkba(NVecsPiU, MaxVirtBatch*NVirt))
            allocate(W2kbi(NVecsPiU, MaxVirtBatch*NOcc))
            allocate(Vlbi(NVecsT2, MaxVirtBatch))
            MaxSubsetDim = maxval(SubsetDim)
            allocate(CbT(MaxVirtBatch*NAO))
            allocate(CaT(NVirt*NAO))
            call rpa_CxT(CaT, VirtCoeffs, NVirt, NAO)
            allocate(Cba(NVirt*MaxVirtBatch*MaxSubsetDim))
            UT2vv = ZERO
            do b0 = 1, NVirt, MaxVirtBatch
                  b1 = min(b0+MaxVirtBatch-1, NVirt)
                  Nb = b1 - b0 + 1
                  Na = NVirt
                  Nab = Na * Nb
                  call rpa_CxT(CbT(1:Nb*NAO), VirtCoeffs(:, b0:b1), Nb, NAO)
                  Rkba = ZERO
                  Y = ThisImage
                  do X = 1, NSubsets(1)
                        SubsetIdx = X + (Y - 1) * NSubsets(1)
                        Npq = SubsetDim(SubsetIdx)
                        Nabpq = Nab * Npq
                        if (Nabpq > 0) then
                              call rpa_Cxy(Cba(1:Nabpq), SubsetBounds(:, SubsetIdx), CbT, CaT, &
                                    Nb, Na, Npq, ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                    ShellParamsIdx, NAngFunc, NAO)
                              !
                              ! Rkba(k,ba) -> Sum(pq) Rkpq(k,pq) * Cba(ba,pq)
                              !
                              call rpa_RCxyT(Rkba, Rkpq, ldR, Cba, Nab, Npq, NVecsPiU)
                        end if
                  end do
                  !
                  ! UT2vv(a, i) = Sum(bcj) (ab|cj) T(cj,bi)
                  !             = Sum(bcj) Sum(KL) R(K,ab) R(K,cj) V(L,cj) V(L,bi) A(L)
                  !             = Sum(kb)  R(k,ab) W2(k,bi)
                  !             = Sum(kb)  R(k,ba) W2(k,bi)
                  !             = Sum(kb)  R(1:NVecsPiU*Nb,a)**T W2(1:NVecsPiU*Nb,i)
                  !
                  ! W2(K,bi) = Sum(L) W1(K,L)*V(L,bi)*A(L)
                  !
                  !
                  call rpa_CC_UT2vv_W2kbi(W2kbi, Vlbi, Vkai, Ak, NVecsT2, W1, NOcc, NVirt, Nb, b0, b1, NVecsPiU)
                  !
                  ! Matrix-matrix operation
                  !
                  ! UT2vv(1:NVirt,1:NOcc) = UT2vv(1:NVirt,1:NOcc)
                  !                         + R(1:NVecsPiU*Nb,1:NVirt)**T W2(1:NVecsPiU*Nb, 1:NOcc)
                  !
                  call real_aTb_x(UT2vv, NVirt, Rkba, NVecsPiU*Nb, W2kbi, NVecsPiU*Nb, NVirt, NOcc, NVecsPiU*Nb, ONE, ONE)
            end do
      end subroutine rpa_CC_UT2vv_Direct


      subroutine rpa_CC_UT2oo_W2kji(W2kja, Vlja, Vkai, Ak, NVecsT2, W1, NOcc, NVirt, Nj, j0, j1, NVecsPiU)
            real(F64), dimension(NVecsPiU, Nj, NVirt), intent(out) :: W2kja
            real(F64), dimension(NVecsT2, Nj), intent(out)         :: Vlja
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in) :: Vkai
            real(F64), dimension(NVecsT2), intent(in)              :: Ak
            integer, intent(in)                                    :: NVecsT2
            real(F64), dimension(NVecsPiU, NVecsT2), intent(in)    :: W1
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: Nj
            integer, intent(in)                                    :: j0, j1
            integer, intent(in)                                    :: NVecsPiU

            integer :: a, j
            
            do a = 1, NVirt
                  do j = j0, j1
                        Vlja(:, j-j0+1) = Ak(:) * Vkai(:, a, j)
                  end do
                  !
                  ! W2(K,ja) = Sum(L) V(L,ja)*W1(K,L)*A(L)
                  !
                  call real_ab(W2kja(:, 1:Nj, a), W1, Vlja(:, 1:Nj))
            end do
      end subroutine rpa_CC_UT2oo_W2kji
      

      subroutine rpa_CC_UT2oo_Direct(UT2oo, W1, Rkpq, Vkai, Ak, OccCoeffs, NOcc, NVirt, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
            SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, MaxOccBatch)
            !
            ! UT2oo(a, i) = Sum(bkj) (ij|bk) T(bk,aj)
            !             = Sum(bkj) Sum(KL) R(K,ij) V(L,aj) R(K,bk) V(L,bk) A(L)
            !             = Sum(j) Sum(KL) R(K,ij) V(L,aj) W1(K,L) A(L)
            !             = Sum(Kj) R(K,ji) W2(K,ja)
            !
            ! Intermediates
            ! --------------
            !
            ! W1(K,L) = Sum(cj) R(K,cj)*V(L,cj)
            ! W2(K,ja) = Sum(L) V(L,aj)*W1(K,L)*A(L)
            !
            real(F64), dimension(:, :), intent(out)                    :: UT2oo
            real(F64), dimension(NVecsPiU, NVecsT2), intent(in)        :: W1
            real(F64), dimension(:, :, :), intent(in)                  :: Rkpq[*]
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)     :: Vkai
            real(F64), dimension(NVecsT2), intent(in)                  :: Ak
            real(F64), dimension(:, :), intent(in)                     :: OccCoeffs
            integer, intent(in)                                        :: NOcc
            integer, intent(in)                                        :: NVirt
            integer, dimension(:, :), intent(in)                       :: ShellPairs
            integer, dimension(:, :), intent(in)                       :: ShellPairLoc
            integer, dimension(:), intent(in)                          :: ShellPairDim
            integer, dimension(:), intent(in)                          :: ShellLoc
            integer, dimension(:), intent(in)                          :: ShellParamsIdx
            integer, dimension(:, :), intent(in)                       :: SubsetBounds
            integer, dimension(:), intent(in)                          :: SubsetDim
            integer, dimension(2), intent(in)                          :: NSubsets
            integer, dimension(:), intent(in)                          :: NAngFunc
            integer, intent(in)                                        :: NAO
            integer, intent(in)                                        :: NVecsPiU
            integer, intent(in)                                        :: NVecsT2
            integer, intent(in)                                        :: MaxOccBatch

            integer :: ldR
            integer :: j0, j1
            integer :: Ni, Nj, Nij
            real(F64), dimension(:, :), allocatable :: Rkji
            real(F64), dimension(:, :), allocatable :: W2kja
            real(F64), dimension(:, :), allocatable :: Vlja
            real(F64), dimension(:), allocatable :: CiT, CjT
            real(F64), dimension(:), allocatable :: Cji
            integer :: X, Y, Npq, Nijpq
            integer :: MaxSubsetDim, SubsetIdx
            integer :: ThisImage

            ThisImage = this_image()
            ldR = size(Rkpq, dim=1)
            allocate(Rkji(NVecsPiU, MaxOccBatch*NOcc))
            allocate(W2kja(NVecsPiU, MaxOccBatch*NVirt))
            MaxSubsetDim = maxval(SubsetDim)
            allocate(CjT(MaxOccBatch*NAO))
            allocate(CiT(NOcc*NAO))
            call rpa_CxT(CiT, OccCoeffs, NOcc, NAO)
            allocate(Cji(NOcc*MaxOccBatch*MaxSubsetDim))
            allocate(Vlja(NVecsT2, NOcc))
            UT2oo = ZERO
            do j0 = 1, NOcc, MaxOccBatch
                  j1 = min(j0+MaxOccBatch-1, NOcc)
                  Ni = NOcc
                  Nj = j1 - j0 + 1
                  Nij = Ni * Nj
                  call rpa_CxT(CjT(1:Nj*NAO), OccCoeffs(:, j0:j1), Nj, NAO)
                  Rkji = ZERO
                  Y = ThisImage
                  do X = 1, NSubsets(1)
                        SubsetIdx = X + (Y - 1) * NSubsets(1)
                        Npq = SubsetDim(SubsetIdx)
                        Nijpq = Nij * Npq
                        if (Nijpq > 0) then
                              call rpa_Cxy(Cji(1:Nijpq), SubsetBounds(:, SubsetIdx), CjT, CiT, &
                                    Nj, Ni, Npq, ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                    ShellParamsIdx, NAngFunc, NAO)
                              !
                              ! Rkji(k,ji) -> Sum(pq) Rkpq(k,pq) * Cji(ji,pq)
                              !
                              call rpa_RCxyT(Rkji, Rkpq, ldR, Cji, Nij, Npq, NVecsPiU)
                        end if
                  end do
                  !
                  ! W2(K,ja) = Sum(L) V(L,aj)*W1(K,L)*A(L)
                  !
                  call rpa_CC_UT2oo_W2kji(W2kja, Vlja, Vkai, Ak, NVecsT2, W1, NOcc, NVirt, Nj, j0, j1, NVecsPiU)
                  !
                  ! Matrix-matrix operation
                  !
                  ! UT2oo(1:NVirt,1:NOcc) = UT2oo(1:NVirt,1:NOcc)
                  !                         + W2(1:NVecsPiU*Nj, 1:NVirt)**T R(1:NVecsPiU*Nj,1:NOcc)
                  !
                  call real_aTb_x(UT2oo, NVirt, W2kja, NVecsPiU*Nj, Rkji, NVecsPiU*Nj, NVirt, NOcc, NVecsPiU*Nj, ONE, ONE)
            end do
      end subroutine rpa_CC_UT2oo_Direct


      subroutine rpa_CC_UT2vo_Direct(T2Uvo_ij, T2Uvo_ab, W1, Vkai, Ak, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2)
            !
            ! T2Uvo(i,j) = Sum(cdl) T(ci,dl)*(dl|cj)
            !            = Sum(cdl)Sum(KL) V(L,ci) A(L) R(K,dl) V(L,dl) R(K,cj)
            !            = Sum(c) Sum(KL) V(L,ci) W1(K,L) R(K,cj) A(L)
            !            = Sum(c)Sum(K) R(K,cj) W2(K,ci)
            !
            ! T2Uvo(a,b) = Sum(dkl) T(ak,dl)*(dl|bk)
            !            = Sum(dkl)Sum(KL) V(L,ak)*V(L,dl)*R(K,dl)*R(K,bk)*A(L)
            !            = Sum(k)Sum(KL) V(L,ak)*W1(K,L)*R(K,bk)*A(L)
            !            = Sum(k)Sum(K) R(K,bk)*W2(K,ak)
            !
            ! Note that T2Uvo_ij and T2Uvo_ab are not symmetric and possibly
            ! need symmetrizing when used in the oo and vv blocks of the Brueckner
            ! hamiltonian.
            !
            ! Intermediates
            ! -------------
            ! W1(K,L) = Sum(dl) R(K,dl)*V(L,dl)
            ! W2(K,ci) = Sum(L) V(L,ci)*W1(K,L)*A(L)
            ! W2(K,ak) = Sum(L) V(L,ak)*W1(K,L)*A(L)
            !
            real(F64), dimension(NOcc, NOcc), intent(out)           :: T2Uvo_ij
            real(F64), dimension(NVirt, NVirt), intent(out)         :: T2Uvo_ab
            real(F64), dimension(NVecsPiU, NVecsT2), intent(in)     :: W1
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
            real(F64), dimension(NVecsT2), intent(in)               :: Ak
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in) :: Rkai
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt
            integer, intent(in)                                     :: NVecsPiU
            integer, intent(in)                                     :: NVecsT2

            real(F64), dimension(:, :), allocatable :: W2Kci
            real(F64), dimension(:, :), allocatable :: VLci
            real(F64), dimension(:), allocatable :: T2Ui
            integer :: i, k, c, a

            allocate(W2Kci(NVecsPiU, NVirt))
            allocate(VLci(NVecsT2, NVirt))
            allocate(T2Ui(NOcc))
            !
            ! T2Uvo(i,j) = Sum(cdl) T(ci,dl)*(dl|cj)
            !            = Sum(cdl)Sum(KL) V(L,ci) A(L) R(K,dl) V(L,dl) R(K,cj)
            !            = Sum(c) Sum(KL) V(L,ci) W1(K,L) R(K,cj) A(L)
            !            = Sum(c)Sum(K) R(K,cj) W2(K,ci)
            !
            do i = 1, NOcc
                  do c = 1, NVirt
                        VLci(:, c) = Ak(:) * Vkai(:, c, i)
                  end do
                  call real_ab(W2kci, W1, VLci)
                  !
                  ! Matrix-vector operation
                  ! Sum(c)Sum(K) R(K,cj) W2(K,ci)
                  ! T2Uvo(i,j) = T2Uvo(i,j) + W2(K,c,i)*R(K,c,j)
                  ! R(1:NVecsPiU*NVirt,1:NOcc)**T W2(1:NVecsPiU*NVirt,i)
                  !
                  call real_aTv_x(T2Ui, Rkai, NVecsPiU*NVirt, W2kci, NVecsPiU*NVirt, NOcc, ONE, ZERO)
                  T2Uvo_ij(i, :) = T2Ui
            end do
            !
            ! T2Uvo(a,b) = Sum(dkl) T(ak,dl)*(dl|bk)
            !            = Sum(dkl)Sum(KL) V(L,ak)*V(L,dl)*R(K,dl)*R(K,bk)*A(L)
            !            = Sum(k)Sum(KL) V(L,ak)*W1(K,L)*R(K,bk)*A(L)
            !            = Sum(k)Sum(K) R(K,bk)*W2(K,ak)
            !
            associate (VLak => VLci, &
                  W2Kak => W2Kci)
                  T2Uvo_ab = ZERO
                  do k = 1, NOcc
                        do a = 1, NVirt
                              VLak(:, a) = Ak(:) * Vkai(:, a, k)
                        end do
                        call real_ab(W2Kak, W1, VLak)
                        !
                        ! Matrix-matrix operation
                        ! T2Uvo(a,b) = T2Uvo(a,b) + Sum(K) W2(K,ak)*R(K,bk)
                        ! T2Uvo(1:NVirt,1:NVirt) = T2Uvo(1:NVirt,1:NVirt) + W2(1:NVecsPiU,1:NVirt,k)**T * R(1:NVecsPiU,1:NVirt,k)
                        !
                        call real_aTb_x(T2Uvo_ab, NVirt, W2Kak, NVecsPiU, Rkai(:, :, k), NVecsPiU, NVirt, NVirt, NVecsPiU, ONE, ONE)
                  end do
            end associate
      end subroutine rpa_CC_UT2vo_Direct


      subroutine rpa_CC_UT2_Direct(UT2vv, UT2oo, UT2vo_ij, UT2vo_ab, &
            Rkai, Rkpq, Vkai, Ak, OccCoeffs, VirtCoeffs, NOcc, NVirt, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
            SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, BruecknerOffdiag, BruecknerDiag)
            
            real(F64), dimension(NVirt, NOcc), intent(out)             :: UT2vv
            real(F64), dimension(NVirt, NOcc), intent(out)             :: UT2oo
            real(F64), dimension(NOcc, NOcc), intent(out)              :: UT2vo_ij
            real(F64), dimension(NVirt, NVirt), intent(out)            :: UT2vo_ab
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)    :: Rkai
            real(F64), dimension(:, :, :), intent(in)                  :: Rkpq[*]
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)     :: Vkai
            real(F64), dimension(NVecsT2), intent(in)                  :: Ak
            real(F64), dimension(:, :), intent(in)                     :: OccCoeffs
            real(F64), dimension(:, :), intent(in)                     :: VirtCoeffs
            integer, intent(in)                                        :: NOcc
            integer, intent(in)                                        :: NVirt
            integer, dimension(:, :), intent(in)                       :: ShellPairs
            integer, dimension(:, :), intent(in)                       :: ShellPairLoc
            integer, dimension(:), intent(in)                          :: ShellPairDim
            integer, dimension(:), intent(in)                          :: ShellLoc
            integer, dimension(:), intent(in)                          :: ShellParamsIdx
            integer, dimension(:, :), intent(in)                       :: SubsetBounds
            integer, dimension(:), intent(in)                          :: SubsetDim
            integer, dimension(2), intent(in)                          :: NSubsets
            integer, dimension(:), intent(in)                          :: NAngFunc
            integer, intent(in)                                        :: NAO
            integer, intent(in)                                        :: NVecsPiU
            integer, intent(in)                                        :: NVecsT2
            logical, intent(in)                                        :: BruecknerOffdiag
            logical, intent(in)                                        :: BruecknerDiag
            
            real(F64), dimension(:, :), allocatable :: W1
            integer, parameter :: MaxBatch = 8
            !
            ! W1 intermediate matrix used to generate all UT2 matrices
            ! W1(k,l) = Sum(cj) R(k,cj) V(l,cj)
            !
            if (BruecknerDiag .or. BruecknerOffdiag) then
                  allocate(W1(NVecsPiU, NVecsT2))
                  call real_abT_x(W1, NVecsPiU, Rkai, NVecsPiU, Vkai, NVecsT2, &
                        NVecsPiU, NVecsT2, NVirt*NOcc, ONE, ZERO)
            end if
            if (BruecknerOffdiag) then
                  !
                  ! UT2vv(a, i) = Sum(bcj) (ab|cj) T(cj,bi)
                  !
                  call rpa_CC_UT2vv_Direct(UT2vv, W1, Rkpq, Vkai, Ak, VirtCoeffs, NOcc, NVirt, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, MaxBatch)
                  !
                  ! UT2oo(a, i) = Sum(bkj) (ij|bk) T(bk,aj)
                  !
                  call rpa_CC_UT2oo_Direct(UT2oo, W1, Rkpq, Vkai, Ak, OccCoeffs, NOcc, NVirt, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, MaxBatch)
            end if
            if (BruecknerDiag) then
                  !
                  ! T2Uvo(i,j) = Sum(cdl) T(ci,dl)*(dl|cj)
                  ! T2Uvo(a,b) = Sum(dkl) T(ak,dl)*(dl|bk)
                  !
                  call rpa_CC_UT2vo_Direct(UT2vo_ij, UT2vo_ab, W1, Vkai, Ak, Rkai, &
                        NOcc, NVirt, NVecsPiU, NVecsT2)
            end if
      end subroutine rpa_CC_UT2_Direct
     

     subroutine rpa_CC_Singles_T2F_Direct(T2F, Fai, Vkai, Ak, NOcc, NVirt, NVecsT2, W1)
           real(F64), dimension(NVirt, NOcc), intent(out)             :: T2F
           real(F64), dimension(NVirt, NOcc), intent(in)              :: Fai
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)     :: Vkai
           real(F64), dimension(NVecsT2), intent(in)                  :: Ak
           integer, intent(in)                                        :: NOcc
           integer, intent(in)                                        :: NVirt
           integer, intent(in)                                        :: NVecsT2
           real(F64), dimension(:), intent(out)                       :: W1

           integer :: k
           !
           ! W1(k) = Ak * Sum(ai) V(k,ai) F(ai)
           !
           W1 = ZERO
           call real_av_x(W1, Vkai, NVecsT2, Fai, NVecsT2, NVirt*NOcc, ONE, ZERO)
           do k = 1, NVecsT2
                 W1(k) = W1(k) * Ak(k)
           end do
           !
           ! Sum(k) V(k,ai)**T W1(k)
           !
           T2F = ZERO
           call real_aTv_x(T2F, Vkai, NVecsT2, W1, NVecsT2, NVirt*NOcc, ONE, ZERO)
     end subroutine rpa_CC_Singles_T2F_Direct


     subroutine rpa_CC_Singles_T2F_Exchange(T2Fx, Fai, Vkai, Ak, NOcc, NVirt, NVecsT2, W3)
           !
           ! T2Fx(ai) = Sum(bj) T(aj,bi)F(bj)
           !
           real(F64), dimension(NVirt, NOcc), intent(out)             :: T2Fx
           real(F64), dimension(NVirt, NOcc), intent(in)              :: Fai
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)     :: Vkai
           real(F64), dimension(NVecsT2), intent(in)                  :: Ak
           integer, intent(in)                                        :: NOcc
           integer, intent(in)                                        :: NVirt
           integer, intent(in)                                        :: NVecsT2
           real(F64), dimension(NVecsT2, NOcc), intent(out)           :: W3

           integer :: k, i, j, a

            W3 = ZERO
            T2Fx = ZERO
            do i = 1, NOcc
                  !
                  ! W3(1:NVecsT2, 1:NOcc) = V(1:NVecsT2, 1:NVirt, i) F(1:NVirt, 1:NOcc)
                  !
                  call real_ab_x(W3, NVecsT2, Vkai(:, :, i), NVecsT2, Fai, NVirt, NVecsT2, NOcc, NVirt, ONE, ZERO)
                  do j = 1, NOcc
                        do a = 1, NVirt
                              do k = 1, NVecsT2
                                    T2Fx(a, i) = T2Fx(a, i) + Vkai(k, a, j) * W3(k, j) * Ak(k)
                              end do
                        end do
                  end do
            end do
      end subroutine rpa_CC_Singles_T2F_Exchange
     

      subroutine rpa_CC_Singles_VT1_Direct(VT1, T1, Rkai, NOcc, NVirt, NVecsPiU, W2)
            !
            ! Sum(bj) (ai|bj) T(bj)
            !
           real(F64), dimension(NVirt, NOcc), intent(out)             :: VT1
           real(F64), dimension(NVirt, NOcc), intent(in)              :: T1
           real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)    :: Rkai
           integer, intent(in)                                        :: NOcc
           integer, intent(in)                                        :: NVirt
           integer, intent(in)                                        :: NVecsPiU
           real(F64), dimension(:), intent(out)                       :: W2
           !
           ! W2(k) = Sum(ai) R(k,ai) T(ai)
           !
           W2 = ZERO
           call real_av_x(W2, Rkai, NVecsPiU, T1, NVecsPiU, NVirt*NOcc, ONE, ZERO)
           !
           ! Sum(k) V(k,ai)**T W1(k)
           !
           VT1 = ZERO
           call real_aTv_x(VT1, Rkai, NVecsPiU, W2, NVecsPiU, NVirt*NOcc, ONE, ZERO)
     end subroutine rpa_CC_Singles_VT1_Direct


     subroutine rpa_CC_Singles_VT1_Exchange(VT1x, T1, Rkai, NOcc, NVirt, NVecsPiU, W4)
           !
           ! VT1x(ai) = Sum(bj) V(aj,bi) T1(bj)
           !
           real(F64), dimension(NVirt, NOcc), intent(out)             :: VT1x
           real(F64), dimension(NVirt, NOcc), intent(in)              :: T1
           real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)    :: Rkai
           integer, intent(in)                                        :: NOcc
           integer, intent(in)                                        :: NVirt
           integer, intent(in)                                        :: NVecsPiU
           real(F64), dimension(NVecsPiU, NOcc), intent(out)          :: W4

           integer :: k, i, j, a

            W4 = ZERO
            VT1x = ZERO
            do i = 1, NOcc
                  !
                  ! W4(1:NVecsPiU, 1:NOcc) = R(1:NVecsPiU, 1:NVirt, i) T1(1:NVirt, 1:NOcc)
                  !
                  call real_ab_x(W4, NVecsPiU, Rkai(:, :, i), NVecsPiU, T1, NVirt, NVecsPiU, NOcc, NVirt, ONE, ZERO)
                  do j = 1, NOcc
                        do a = 1, NVirt
                              do k = 1, NVecsPiU
                                    VT1x(a, i) = VT1x(a, i) + Rkai(k, a, j) * W4(k, j)
                              end do
                        end do
                  end do
            end do
      end subroutine rpa_CC_Singles_VT1_Exchange


      subroutine rpa_CC_Singles_T1VT2_Direct(T1VT2, VT1, Vkai, Ak, NOcc, NVirt, NVecsT2, W)
            !
            ! T1VT2(ai) = Sum(bjck) T1(ck)(ck|bj)T2(bj,ai)
            !
            real(F64), dimension(NVirt, NOcc), intent(out)            :: T1VT2
            real(F64), dimension(NVirt, NOcc), intent(in)             :: VT1
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)    :: Vkai
            real(F64), dimension(NVecsT2), intent(in)                 :: Ak
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt
            integer, intent(in)                                       :: NVecsT2
            real(F64), dimension(NVecsT2), intent(out)                :: W

            integer :: k
            !
            ! W(k) = Sum(ai) VT1(ai)V(k,ai)A(k)
            !
            W = ZERO
            call real_av_x(W, Vkai, NVecsT2, VT1, NVecsT2, NVirt*NOcc, ONE, ZERO)
            do k = 1, NVecsT2
                  W(k) = W(k) * Ak(k)
            end do
            !
            ! T1VT2(ai) = Sum(k) W(k)*V(k,ai)
            ! T1VT2 = V(1:NVecsT2,1:NVirt*NOcc)**T W(1:NVecsT2)
            !
            T1VT2 = ZERO
            call real_aTv_x(T1VT2, Vkai, NVecsT2, W, NVecsT2, NVirt*NOcc, ONE, ZERO)
      end subroutine rpa_CC_Singles_T1VT2_Direct

      
      subroutine rpa_CC_Singles_T1VT2_Exchange(T1VT2x, VT1x, Vkai, Ak, NOcc, NVirt, NVecsT2, W)
            !
            ! T1VT2x(ai) = Sum(bjck) T1(ck)(cj|bk)T2(bj,ai)
            !
            real(F64), dimension(NVirt, NOcc), intent(out)            :: T1VT2x
            real(F64), dimension(NVirt, NOcc), intent(in)             :: VT1x
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)    :: Vkai
            real(F64), dimension(NVecsT2), intent(in)                 :: Ak
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt
            integer, intent(in)                                       :: NVecsT2
            real(F64), dimension(NVecsT2), intent(out)                :: W

            integer :: k
            !
            ! W(k) = Sum(ai) VT1x(ai)V(k,ai)A(k)
            !
            W = ZERO
            call real_av_x(W, Vkai, NVecsT2, VT1x, NVecsT2, NVirt*NOcc, ONE, ZERO)
            do k = 1, NVecsT2
                  W(k) = W(k) * Ak(k)
            end do
            !
            ! T1VT2x(ai) = Sum(k) W(k)*V(k,ai)
            ! T1VT2x = V(1:NVecsT2,1:NVirt*NOcc)**T W(1:NVecsT2)
            !
            T1VT2x = ZERO
            call real_aTv_x(T1VT2x, Vkai, NVecsT2, W, NVecsT2, NVirt*NOcc, ONE, ZERO)
      end subroutine rpa_CC_Singles_T1VT2_Exchange


      subroutine rpa_CC_Singles_ringCCSD(T1, hHFai, Lambda, NOcc, NVirt, SemiOccCoeffs, &
           SemiVirtCoeffs, SemiFii, SemiFaa, Vkai, Ak, NVecsT2, Rkai, NVecsPiU, T1Approx, &
           ChiOrbitals, OccCoeffs_ao, VirtCoeffs_ao, SemiOccCoeffs_ao, SemiVirtCoeffs_ao, &
           Rkpq, ShellPairs, ShellPairLoc, ShellPairDim, &
           ShellLoc, ShellParamsIdx, SubsetBounds, SubsetDim, NSubsets, NAngFunc, NAO)
           
           real(F64), dimension(NVirt, NOcc), intent(out)             :: T1
           real(F64), dimension(NVirt, NOcc), intent(in)              :: hHFai
           real(F64), intent(in)                                      :: Lambda
           integer, intent(in)                                        :: NOcc
           integer, intent(in)                                        :: NVirt
           real(F64), dimension(:, :), intent(in)                     :: SemiOccCoeffs
           real(F64), dimension(:, :), intent(in)                     :: SemiVirtCoeffs
           real(F64), dimension(:), intent(in)                        :: SemiFii
           real(F64), dimension(:), intent(in)                        :: SemiFaa
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)     :: Vkai
           real(F64), dimension(NVecsT2), intent(in)                  :: Ak
           integer, intent(in)                                        :: NVecsT2
           real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)    :: Rkai
           integer, intent(in)                                        :: NVecsPiU
           integer, intent(in)                                        :: T1Approx
           integer, intent(in)                                        :: ChiOrbitals
           real(F64), dimension(:, :), intent(in)                     :: OccCoeffs_ao
           real(F64), dimension(:, :), intent(in)                     :: VirtCoeffs_ao
           real(F64), dimension(:, :), intent(in)                     :: SemiOccCoeffs_ao
           real(F64), dimension(:, :), intent(in)                     :: SemiVirtCoeffs_ao
           real(F64), dimension(:, :, :), intent(in)                  :: Rkpq[*]
           integer, dimension(:, :), intent(in)                       :: ShellPairs
           integer, dimension(:, :), intent(in)                       :: ShellPairLoc
           integer, dimension(:), intent(in)                          :: ShellPairDim
           integer, dimension(:), intent(in)                          :: ShellLoc
           integer, dimension(:), intent(in)                          :: ShellParamsIdx
           integer, dimension(:, :), intent(in)                       :: SubsetBounds
           integer, dimension(:), intent(in)                          :: SubsetDim
           integer, dimension(2), intent(in)                          :: NSubsets
           integer, dimension(:), intent(in)                          :: NAngFunc
           integer, intent(in)                                        :: NAO

           integer :: a, i, k
           real(F64), dimension(:, :), allocatable :: hHFai_semi, X, G
           real(F64), dimension(:), allocatable :: W1, W2
           real(F64), dimension(:, :), allocatable :: W3, W4
           real(F64), dimension(:, :), allocatable :: T2F, VT1, T2Fx, VT1x, T1VT2, T1VT2x, UT2oo, UT2vv
           real(F64), dimension(:, :), allocatable :: UT2vo_ij, UT2vo_ab
           logical :: BruecknerOffdiag, BruecknerDiag
           integer, parameter :: MaxNIters = 2
           logical :: Exchange, Ladder

           if ((T1Approx == RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE) .or. &
                 (T1Approx == RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE_PLUS_LADDER)) then
                 Exchange = .true.
           else
                 Exchange = .false.
           end if
           if (T1Approx == RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE_PLUS_LADDER) then
                 Ladder = .true.
           else
                 Ladder = .false.
           end if
           !
           ! Right-hand side of the T1 amplitude equation
           ! Transform the Hartree-Fock hamiltonian to the semicanonical basis
           !
           allocate(hHFai_semi(NVirt, NOcc))
           allocate(X(NVirt, NOcc))
           call real_ab(X, hHFai, SemiOccCoeffs)
           call real_aTb(hHFai_semi, SemiVirtCoeffs, X)
           do i = 1, NOcc
                 do a = 1, NVirt
                       T1(a, i) = -Lambda * hHFai_semi(a, i) / (SemiFaa(a) - SemiFii(i))
                 end do
           end do
           if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                 !
                 ! Transform T1 amplitudes to the canonical basis of hKS
                 !
                 call real_abT(X, T1, SemiOccCoeffs)
                 call real_ab(T1, SemiVirtCoeffs, X)
           end if
           
           if (T1Approx /= RPA_T1_MEAN_FIELD) then
                 allocate(G(NVirt, NOcc))
                 allocate(W1(NVecsT2))
                 allocate(W2(NVecsPiU))
                 allocate(T2F(NVirt, NOcc))
                 allocate(VT1(NVirt, NOcc))
                 allocate(T1VT2(NVirt, NOcc))
                 if (Exchange) then
                       allocate(T2Fx(NVirt, NOcc))
                       allocate(VT1x(NVirt, NOcc))
                       allocate(T1VT2x(NVirt, NOcc))
                       allocate(W3(NVecsT2, NOcc))
                       allocate(W4(NVecsPiU, NOcc))
                 end if
                 if (Ladder) then
                       allocate(UT2vv(NVirt, NOcc))
                       allocate(UT2oo(NVirt, NOcc))
                 end if                       
                 if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                       call rpa_CC_Singles_T2F_Direct(T2F, hHFai, Vkai, Ak, NOcc, NVirt, NVecsT2, W1)                 
                       if (Exchange) then
                             call rpa_CC_Singles_T2F_Exchange(T2Fx, hHFai, Vkai, Ak, NOcc, NVirt, NVecsT2, W3)
                       end if
                 else
                       call rpa_CC_Singles_T2F_Direct(T2F, hHFai_semi, Vkai, Ak, NOcc, NVirt, NVecsT2, W1)                 
                       if (Exchange) then
                             call rpa_CC_Singles_T2F_Exchange(T2Fx, hHFai_semi, Vkai, Ak, NOcc, NVirt, NVecsT2, W3)
                       end if
                 end if
                 if (Ladder) then
                       allocate(UT2vo_ij(0, 0))
                       allocate(UT2vo_ab(0, 0))
                       BruecknerOffdiag = .true.
                       BruecknerDiag = .false.
                       if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                             call rpa_CC_UT2_Direct(UT2vv, UT2oo, UT2vo_ij, UT2vo_ab, &
                                   Rkai, Rkpq, Vkai, Ak, OccCoeffs_ao, VirtCoeffs_ao, NOcc, NVirt, &
                                   ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                                   SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, BruecknerOffdiag, BruecknerDiag)
                       else
                             call rpa_CC_UT2_Direct(UT2vv, UT2oo, UT2vo_ij, UT2vo_ab, &
                                   Rkai, Rkpq, Vkai, Ak, SemiOccCoeffs_ao, SemiVirtCoeffs_ao, NOcc, NVirt, &
                                   ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                                   SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU, NVecsT2, BruecknerOffdiag, BruecknerDiag)
                       end if                       
                 end if
                 do k = 1, MaxNIters
                       call rpa_CC_Singles_VT1_Direct(VT1, T1, Rkai, NOcc, NVirt, NVecsPiU, W2)
                       call rpa_CC_Singles_T1VT2_Direct(T1VT2, VT1, Vkai, Ak, NOcc, NVirt, NVecsT2, W1)
                       if (Exchange) then
                             call rpa_CC_Singles_VT1_Exchange(VT1x, T1, Rkai, NOcc, NVirt, NVecsPiU, W4)
                             call rpa_CC_Singles_T1VT2_Exchange(T1VT2x, VT1x, Vkai, Ak, NOcc, NVirt, NVecsT2, W1)
                       end if
                       if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                             G = Lambda * hHFai
                       else
                             G = Lambda * hHFai_semi
                       end if
                       do i = 1, NOcc
                             do a = 1, NVirt
                                   G(a, i) = G(a, i) &
                                         + Lambda * TWO * T2F(a, i) &
                                         + Lambda * TWO * VT1(a, i) &
                                         + Lambda * FOUR * T1VT2(a, i)
                             end do
                       end do
                       if (Exchange) then
                             do i = 1, NOcc
                                   do a = 1, NVirt
                                         G(a, i) = G(a, i) &
                                               - Lambda * T2Fx(a, i) &
                                               - Lambda * VT1x(a, i) &
                                               - Lambda * TWO * T1VT2x(a, i)
                                   end do
                             end do
                       end if
                       if (Ladder) then
                             do i = 1, NOcc
                                   do a = 1, NVirt
                                         G(a, i) = G(a, i) &
                                               + Lambda * TWO * UT2vv(a, i) &
                                               - Lambda * TWO * UT2oo(a, i)
                                   end do
                             end do
                       end if
                       if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                             !
                             ! Transform G to the semicanonical basis of F(Lambda)
                             !
                             call real_ab(X, G, SemiOccCoeffs)
                             call real_aTb(G, SemiVirtCoeffs, X)
                       end if
                       do i = 1, NOcc
                             do a = 1, NVirt
                                   T1(a, i) = -G(a, i) / (SemiFaa(a) - SemiFii(i))
                             end do
                       end do
                       if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                             !
                             ! Transform T1 amplitudes to the canonical basis of hKS
                             !
                             call real_abT(X, T1, SemiOccCoeffs)
                             call real_ab(T1, SemiVirtCoeffs, X)
                       end if
                 end do
           end if
     end subroutine rpa_CC_Singles_ringCCSD
end module rpa_CC_Singles
