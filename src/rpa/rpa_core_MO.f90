module rpa_core_MO
      use arithmetic
      use math_constants
      use ParallelCholesky
      use real_linalg
      use rpa_definitions
      use GaussPRNG

      implicit none
      
contains
      
      subroutine rpa_BroadcastVecs(A, B, Img)
            real(F64), dimension(:, :, :), intent(inout) :: A[*]
            real(F64), dimension(:, :, :), intent(in)    :: B
            integer, intent(in)                          :: Img

            integer :: k, z, nz, p0, p1
            integer :: Y
            integer :: NCols, NChunks, m, n
            integer :: ThisImage, NImages

            NImages = num_images()
            ThisImage = this_image()
            m = size(A, dim=1)
            n = size(A, dim=2)
            nz = size(A, dim=3)
            NCols = max(1, RPA_MAX_TRANSFER / m)
            if (modulo(n, NCols) > 0) then
                  NChunks = n / NCols + 1
            else
                  NChunks = n / NCols
            end if
            if (ThisImage == Img) then
                  do z = 1, nz
                        do k = 1, NChunks
                              p0 = 1 + (k-1) * NCols
                              p1 = min(k * NCols, n)
                              do Y = 1, NImages
                                    A(:, p0:p1, z)[Y] = B(:, p0:p1, z)
                              end do
                        end do
                  end do
            end if
      end subroutine rpa_BroadcastVecs

      
      subroutine rpa_Cab(C, OccCoeffs, VirtCoeffs, Na, Nb, LocA, LocB, NOcc, NVirt)
            !
            ! ShA /= ShB
            ! ShC /= ShD
            !
            real(F64), dimension(NVirt, NOcc, *), intent(out) :: C
            real(F64), dimension(NOcc, *), intent(in)         :: OccCoeffs
            real(F64), dimension(NVirt, *), intent(in)        :: VirtCoeffs
            integer, intent(in)                               :: Na
            integer, intent(in)                               :: Nb
            integer, intent(in)                               :: LocA
            integer, intent(in)                               :: LocB
            integer, intent(in)                               :: NOcc
            integer, intent(in)                               :: NVirt
            
            integer :: pp, qq
            integer :: p, q, pq
            integer :: OffsetP, OffsetQ
            integer :: iota, alpha

            OffsetP = LocA - 1
            OffsetQ = LocB - 1
            do q = 1, Nb
                  do p = 1, Na
                        pp = OffsetP + p
                        qq = OffsetQ + q
                        pq = p+(q-1)*Na
                        do iota = 1, NOcc
                              do alpha = 1, NVirt
                                    C(alpha, iota, pq) = OccCoeffs(iota, pp) * VirtCoeffs(alpha, qq) &
                                          + OccCoeffs(iota, qq) * VirtCoeffs(alpha, pp)
                              end do
                        end do
                  end do
            end do
      end subroutine rpa_Cab


      subroutine rpa_Caa(C, OccCoeffs, VirtCoeffs, Na, LocA, NOcc, NVirt)
            !
            ! ShA == ShB
            !
            real(F64), dimension(NVirt, NOcc, *), intent(out) :: C
            real(F64), dimension(NOcc, *), intent(in)         :: OccCoeffs
            real(F64), dimension(NVirt, *), intent(in)        :: VirtCoeffs
            integer, intent(in)                               :: Na
            integer, intent(in)                               :: LocA
            integer, intent(in)                               :: NOcc
            integer, intent(in)                               :: NVirt
            
            integer :: pp, qq
            integer :: p, q, pq
            integer :: OffsetP, OffsetQ
            integer :: iota, alpha

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
                  do iota = 1, NOcc
                        do alpha = 1, NVirt
                              C(alpha, iota, pq) = OccCoeffs(iota, pp) * VirtCoeffs(alpha, qq)
                        end do
                  end do
                  pq = pq + 1
                  !
                  ! Off-diagonal
                  !
                  do p = q + 1, Na
                        pp = OffsetP + p
                        qq = OffsetQ + q
                        do iota = 1, NOcc
                              do alpha = 1, NVirt
                                    C(alpha, iota, pq) = OccCoeffs(iota, pp) * VirtCoeffs(alpha, qq) &
                                          + OccCoeffs(iota, qq) * VirtCoeffs(alpha, pp)
                              end do
                        end do
                        pq = pq + 1
                  end do
            end do
      end subroutine rpa_Caa


      subroutine rpa_C(C, SubsetBounds, OccCoeffs, VirtCoeffs, NOcc, NVirt, Npq, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, &
            NAngFunc, NAO)
            
            real(F64), dimension(NOcc*NVirt, Npq), intent(out) :: C
            integer, dimension(2), intent(in)                  :: SubsetBounds
            real(F64), dimension(NOcc, NAO), intent(in)        :: OccCoeffs
            real(F64), dimension(NVirt, NAO), intent(in)       :: VirtCoeffs
            integer, intent(in)                                :: NOcc
            integer, intent(in)                                :: NVirt
            integer, intent(in)                                :: Npq
            integer, dimension(:, :), intent(in)               :: ShellPairs
            integer, dimension(:, :), intent(in)               :: ShellPairLoc
            integer, dimension(:), intent(in)                  :: ShellPairDim
            integer, dimension(:), intent(in)                  :: ShellLoc
            integer, dimension(:), intent(in)                  :: ShellParamsIdx
            integer, dimension(:), intent(in)                  :: NAngFunc
            integer, intent(in)                                :: NAO

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
                        call rpa_Cab(C(:, pq0:pq1), OccCoeffs, VirtCoeffs, &
                              Na, Nb, LocA, LocB, NOcc, NVirt)
                  else
                        call rpa_Caa(C(:, pq0:pq1), OccCoeffs, VirtCoeffs, &
                              Na, LocA, NOcc, NVirt)                        
                  end if
            end do
            !$omp end parallel do
      end subroutine rpa_C


      subroutine rpa_CRGxy(CRG, RG, C, Nai, Npq, NVecs)
            real(F64), dimension(NVecs, Nai), intent(inout) :: CRG
            real(F64), dimension(NVecs, Npq), intent(in)    :: RG
            real(F64), dimension(Nai, Npq), intent(in)      :: C
            integer, intent(in)                             :: Nai
            integer, intent(in)                             :: Npq
            integer, intent(in)                             :: NVecs
            
            call real_abT_x(CRG, NVecs, RG, NVecs, C, Nai, NVecs, &
                  Nai, Npq, alpha=ONE, beta=ONE)
      end subroutine rpa_CRGxy


      subroutine rpa_CT(CT, C, m0, m1, NMO, NAO)
            real(F64), dimension(NMO, NAO), intent(out) :: CT
            real(F64), dimension(:, :), intent(in)      :: C
            integer, intent(in)                         :: m0
            integer, intent(in)                         :: m1
            integer, intent(in)                         :: NMO
            integer, intent(in)                         :: NAO

            CT = transpose(C(:, m0:m1))
      end subroutine rpa_CT

      
      subroutine rpa_CRG(CRG, RG, OccCoeffs, VirtCoeffs, NOcc, NVirt, OccBounds, VirtBounds, &
            NSpins, ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
            SubsetDim, NSubsets, NAngFunc, NAO, NVecs)

            real(F64), dimension(:, :, :), intent(out) :: CRG
            real(F64), dimension(:, :, :), intent(in)  :: RG
            real(F64), dimension(:, :, :), intent(in)  :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)  :: VirtCoeffs
            integer, dimension(2, 2), intent(in)       :: OccBounds
            integer, dimension(2, 2), intent(in)       :: VirtBounds
            integer, dimension(2), intent(in)          :: NOcc
            integer, dimension(2), intent(in)          :: NVirt
            integer, intent(in)                        :: NSpins
            integer, dimension(:, :), intent(in)       :: ShellPairs
            integer, dimension(:, :), intent(in)       :: ShellPairLoc
            integer, dimension(:), intent(in)          :: ShellPairDim
            integer, dimension(:), intent(in)          :: ShellLoc
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:, :), intent(in)       :: SubsetBounds
            integer, dimension(:), intent(in)          :: SubsetDim
            integer, dimension(2), intent(in)          :: NSubsets
            integer, dimension(:), intent(in)          :: NAngFunc
            integer, intent(in)                        :: NAO
            integer, intent(in)                        :: NVecs

            integer :: x, y, s
            integer :: SubsetIdx, Npq, Nai, Nip, Nap, Naipq
            integer :: i0, i1, a0, a1
            integer :: MaxSubsetDim, MaxNai
            integer :: ThisImage
            integer :: ldRG
            real(F64), dimension(:), allocatable :: C
            real(F64), dimension(:, :), allocatable :: OccCoeffsT, VirtCoeffsT
            real(F64), dimension(:, :), allocatable :: RGxy

            ThisImage = this_image()
            CRG = ZERO
            MaxSubsetDim = maxval(SubsetDim)
            ldRG = size(RG, dim=1)
            allocate(RGxy(NVecs, MaxSubsetDim))
            MaxNai = 0
            do s = 1, NSpins
                  Nai = NOcc(s) * NVirt(s)
                  MaxNai = max(MaxNai, Nai)
            end do
            allocate(OccCoeffsT(max(NOcc(1), NOcc(2))*NAO, NSpins))
            allocate(VirtCoeffsT(max(NVirt(1), NVirt(2))*NAO, NSpins))
            do s = 1, NSpins
                  Nai = NOcc(s) * NVirt(s)
                  if (Nai > 0) then
                        Nip = NOcc(s) * NAO
                        Nap = NVirt(s) * NAO
                        i0 = OccBounds(1, s)
                        i1 = OccBounds(2, s)
                        a0 = VirtBounds(1, s)
                        a1 = VirtBounds(2, s)
                        call rpa_CT(OccCoeffsT(1:Nip, s), OccCoeffs(:, :, s), i0, i1, NOcc(s), NAO)
                        call rpa_CT(VirtCoeffsT(1:Nap, s), VirtCoeffs(:, :, s), a0, a1, NVirt(s), NAO)
                  end if
            end do
            allocate(C(MaxNai*MaxSubsetDim))
            do Y = 1, NSubsets(2)
                  do X = 1, NSubsets(1)
                        SubsetIdx = X + (Y - 1) * NSubsets(1)
                        Npq = SubsetDim(SubsetIdx)
                        RGxy(1:NVecs, 1:Npq) = RG(1:NVecs, 1:Npq, X)
                        call co_broadcast(RGxy, source_image=Y)
                        do s = 1, NSpins
                              Nai = NVirt(s) * NOcc(s)
                              Nap = NVirt(s) * NAO
                              Nip = NOcc(s) * NAO
                              Naipq = Nai * Npq
                              if (Naipq > 0) then
                                    call rpa_C(C(1:Naipq), SubsetBounds(:, SubsetIdx), OccCoeffsT(1:Nip, s), VirtCoeffsT(1:Nap, s), &
                                          NOcc(s), NVirt(s), Npq, ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                          ShellParamsIdx, NAngFunc, NAO)
                                    call rpa_CRGxy(CRG(:, :, s), RGxy, C, Nai, Npq, NVecs)
                              end if
                        end do
                  end do
            end do
      end subroutine rpa_CRG


      subroutine rpa_OVSubsets(OccBounds, VirtBounds, OccSubsetDim, VirtSubsetDim, &
            NOcc, NVirt, NSpins)
            
            integer, dimension(:, :, :), intent(out) :: OccBounds
            integer, dimension(:, :, :), intent(out) :: VirtBounds
            integer, dimension(:, :), intent(out)    :: OccSubsetDim
            integer, dimension(:, :), intent(out)    :: VirtSubsetDim
            integer, dimension(2), intent(in)        :: NOcc
            integer, dimension(2), intent(in)        :: NVirt
            integer, intent(in)                      :: NSpins

            integer :: NImages
            integer :: NTasks
            integer :: k, s

            NImages = num_images()
            OccBounds = 0
            VirtBounds = 0
            OccSubsetDim = 0
            VirtSubsetDim = 0
            do s = 1, NSpins
                  if (NOcc(s)*NVirt(s)>0) then
                        NTasks = NVirt(s)
                  else
                        NTasks = 0
                  end if
                  kloop: do k = 1, NImages
                        if (NTasks > 0) then
                              OccBounds(1, s, k) = 1
                              OccBounds(2, s, k) = NOcc(s)
                              OccSubsetDim(s, k) = OccBounds(2, s, k) - OccBounds(1, s, k) + 1
                              VirtBounds(1, s, k) = NVirt(s) - NTasks + 1
                              VirtBounds(2, s, k) = NVirt(s) - NTasks + max(1, NTasks / (NImages-k+1))
                              VirtSubsetDim(s, k) = VirtBounds(2, s, k) - VirtBounds(1, s, k) + 1
                              NTasks = NTasks - VirtSubsetDim(s, k)
                        else
                              exit kloop
                        end if
                  end do kloop
            end do
      end subroutine rpa_OVSubsets


      subroutine rpa_GPiUG_MO(GPiUG, CRG, DCRG, OccEnergies, VirtEnergies, NOcc, NVirt, &
            OccBounds, VirtBounds, NSpins, Freq)
            
            real(F64), dimension(:, :), intent(inout) :: GPiUG
            real(F64), dimension(:, :, :), intent(in) :: CRG
            real(F64), dimension(:, :), intent(out)   :: DCRG
            real(F64), dimension(:, :), intent(in)    :: OccEnergies
            real(F64), dimension(:, :), intent(in)    :: VirtEnergies
            integer, dimension(2), intent(in)         :: NOcc
            integer, dimension(2), intent(in)         :: NVirt
            integer, dimension(2, 2), intent(in)      :: OccBounds
            integer, dimension(2, 2), intent(in)      :: VirtBounds
            integer, intent(in)                       :: NSpins
            real(F64), intent(in)                     :: Freq

            real(F64) :: Prefactor, dai, Wai
            integer :: Nai, a, i, ai
            integer :: OffsetA, OffsetI
            integer :: NVecs
            integer :: s
            !
            ! Prefactor for scaling the matrix GPiUG:
            ! 
            ! (i) Take into account the factor of 2 which results from
            ! the summation of a complex number and its conjugate
            ! 
            ! 1/(Ea-Ei+iu)+1/(Ea-Ei-iu) = 2(Ea-Ei)/((Ea-Ei)**2 + u**2)
            ! 
            ! (ii) Multiply Pi(u) by the factor of 2 to account for the spin summation
            ! over doubly occupied closed-shell orbitals. The summation over spins
            ! is given explicitly in Eq. 58 of Ref. 1.
            !
            if (NSpins > 1) then
                  Prefactor = TWO
            else
                  Prefactor = FOUR
            end if
            NVecs = size(GPiUG, dim=1)
            do s = 1, NSpins
                  Nai = NOcc(s) * NVirt(s)
                  if (Nai > 0) then
                        OffsetA = VirtBounds(1, s) - 1
                        OffsetI = OccBounds(1, s) - 1
                        !$omp parallel do collapse(2) &
                        !$omp default(shared) &
                        !$omp private(i, a, ai, dai, Wai)
                        do i = 1, NOcc(s)
                              do a = 1, NVirt(s)
                                    ai = a + (i - 1) * NVirt(s)
                                    dai = VirtEnergies(OffsetA+a, s) - OccEnergies(OffsetI+i, s)
                                    Wai = dai / (dai**2 + Freq**2)
                                    DCRG(:, ai) = Wai * CRG(:, ai, s)
                              end do
                        end do
                        !$omp end parallel do
                        call real_abT_x(GPiUG, NVecs, CRG(:, :, s), NVecs, DCRG, NVecs, &
                              NVecs, NVecs, Nai, alpha=Prefactor, beta=ONE)
                  end if
            end do
      end subroutine rpa_GPiUG_MO  
end module rpa_core_MO
