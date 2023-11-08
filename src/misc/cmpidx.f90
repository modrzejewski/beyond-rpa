! ------------------------------------------------------
!                          CMPIDX
! ------------------------------------------------------
! A set of subroutines for translating compound orbital
! index into individual indices, and for the inverse
! mapping. Use this module for:
! 1) Replacing multiple loops over a set of orbitals
!    by a single loop,
! 2) Parallelizing nested, non-rectangular loop, e.g.,
!    do q = 1, n
!       do p = q, n
!          some number crunching
!       end do
!    end do
! ------------------------------------------------------
module cmpidx
      implicit none
      save

      integer, dimension(:), allocatable :: bj_offset
      integer, dimension(:), allocatable :: ai_offset
      integer, dimension(:), allocatable :: aiaj_offset_i
      integer, dimension(:), allocatable :: aiaj_offset_j

      integer, dimension(:), allocatable :: abb_ijk_offset_a
      integer, dimension(:), allocatable :: abb_ijk_offset_b
      integer, dimension(:), allocatable :: abb_ijk_offset_j
      integer, dimension(:), allocatable :: abb_ijk_offset_k
      integer, dimension(:), allocatable :: abb_iji_offset_sm
      integer, dimension(:), allocatable :: abb_iji_offset_gt

contains

      pure function a_i2ai(a, i, nvirt, nocc)
            !
            ! (A, I) -> AI
            ! Virtual index changes faster than the occupied one.
            ! The lowest virtual index is NOCC+1.
            !
            integer             :: a_i2ai
            integer, intent(in) :: a
            integer, intent(in) :: i
            integer, intent(in) :: nvirt
            integer, intent(in) :: nocc

            a_i2ai = (i - 1) * nvirt + a - nocc
      end function a_i2ai


      pure function i_a2ia(i, a, nocc)
            !
            ! (I, A) -> IA
            ! Occupied index changes faster than the virtual one.
            ! The lowest virtual index is NOCC+1.
            !
            integer             :: i_a2ia
            integer, intent(in) :: i
            integer, intent(in) :: a
            integer, intent(in) :: nocc
            
            i_a2ia = (a - nocc - 1) * nocc + i
      end function i_a2ia


      pure subroutine ai2a_i(ai, nocc, nvirt, a, i)
            !
            ! AI -> (A, I)
            ! Virtual index changes faster than the occupied one.
            ! The lowest virtual index is NOCC+1.
            !
            integer, intent(in)  :: ai
            integer, intent(in)  :: nocc
            integer, intent(in)  :: nvirt
            integer, intent(out) :: a
            integer, intent(out) :: i

            i = (ai - 1) / nvirt + 1
            a = ai - (i - 1) * nvirt + nocc
      end subroutine ai2a_i

      
      pure subroutine ia2i_a(ia, nocc, i, a)
            !
            ! IA -> (I, A)
            ! Occupied index changes faster than the virtual one.
            ! The lowest virtual index is NOCC+1.
            !
            integer, intent(in)  :: ia
            integer, intent(in)  :: nocc
            integer, intent(out) :: i
            integer, intent(out) :: a

            a = (ia - 1) / nocc + nocc + 1
            i = ia - (a - nocc - 1) * nocc
      end subroutine ia2i_a


      pure function p_ge_q2pq(p, q, m)
            !
            ! Compute compound 2-index, (pq), assuming that p is always
            ! greater or equal q. Min. index: 1, max index: M.
            ! Example: P and Q enumerate, respectively, rows and columns
            ! of a symmetric matrix. The compound 2-index enumerates 
            ! consecutive elements of the lower triangle of the 5x5 matrix.
            !
            ! M = 5
            !             Q 
            !    | 1                |
            !    | 2  6             |
            ! P  | 3  7  10         |
            !    | 4  8  11  13     |
            !    | 5  9  12  14  15 |
            !
            integer             :: p_ge_q2pq
            integer, intent(in) :: p
            integer, intent(in) :: q
            integer, intent(in) :: m

            integer :: i1, i2

            i1 = ((2 * m - q + 2) * (q - 1)) / 2
            i2 = p - q + 1
            p_ge_q2pq = i1 + i2
      end function p_ge_q2pq


      pure subroutine pq2p_ge_q(pq, n, p, q)
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
      end subroutine pq2p_ge_q


      pure subroutine ai_ge_bj(k, npair, nocc, nvirt, a, b, i, j)
            !
            ! Decode compound ov-pair index K into individual
            ! orbital indices:
            ! K -> (AI, BJ) -> (A, I, B, J).
            ! Assumptions:
            ! 0) AI >= BJ (diagonal pairs admissible),
            ! 1) AI changes faster then BJ,
            ! 2) Virtual index (A) changes faster
            !    than the occupied one (I).
            !
            integer, intent(in) :: k
            integer, intent(in) :: npair
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt
            integer, intent(out) :: a
            integer, intent(out) :: b
            integer, intent(out) :: i 
            integer, intent(out) :: j

            integer :: ai, bj

            call pq2p_ge_q(k, npair, ai, bj)
            call ai2a_i(ai, nocc, nvirt, a, i)
            call ai2a_i(bj, nocc, nvirt, b, j)
      end subroutine ai_ge_bj

      pure subroutine triang_idx_ai(k, nocc, a, i, ai)
            !
            ! K -> (I, A, AI)
            ! Assumptions:
            ! 0) Occupied index changes faster than the virtual one
            ! 1) K \in 1, 2, ..., NOCC * NVIRT
            !
            integer, intent(in)  :: k
            integer, intent(in)  :: nocc
            integer, intent(out) :: a
            integer, intent(out) :: i 
            integer, intent(out) :: ai

            ai = k
            call ia2i_a(ai, nocc, i, a)

      end subroutine triang_idx_ai

      pure subroutine triang_idx_aiai(k, nocc, a, b, i, j, ai, bj)
            !
            ! K -> (I, A, J, B, AI, BJ)
            ! Assumptions:
            ! 0) ai == bj
            ! 1) Occupied index changes faster than the virtual one
            ! 2) K \in 1, 2, ..., NOCC * NVIRT
            !
            integer, intent(in)  :: k
            integer, intent(in)  :: nocc
            integer, intent(out) :: a
            integer, intent(out) :: b
            integer, intent(out) :: i 
            integer, intent(out) :: j
            integer, intent(out) :: ai
            integer, intent(out) :: bj

            ai = k
            bj = k
            call ia2i_a(ai, nocc, i, a)
            j = i
            b = a
      end subroutine triang_idx_aiai


      pure subroutine triang_idx_aiaj_inej(k, nocc, a, b, i, j, ai, bj)
            !
            ! K -> (I, A, J, B, AI, BJ)
            ! Assumptions:
            ! 0) ai > aj,
            ! 1) i != j,
            ! 2) a == b,
            ! 3) AI changes faster then BJ,
            ! 4) Occupied index changes faster than virtual one
            ! 5) K \in 1, 2, ..., NVIRT * (((NOCC - 1) * NOCC) / 2)
            !
            integer, intent(in)  :: k
            integer, intent(in)  :: nocc
            integer, intent(out) :: a 
            integer, intent(out) :: b
            integer, intent(out) :: i 
            integer, intent(out) :: j
            integer, intent(out) :: ai
            integer, intent(out) :: bj

            integer :: in1, n, m

            n = ((nocc - 1) * nocc) / 2
            m = (k - 1) / n
            in1 = k - m * n
            a = m + nocc + 1
            b = a
            i = aiaj_offset_i(in1) 
            j = aiaj_offset_j(in1)
            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(j, b, nocc)
      end subroutine triang_idx_aiaj_inej


      pure subroutine triang_idx_aiaj(k, nocc, a, b, i, j, ai, bj)
            !
            ! K -> (I, A, J, B, AI, BJ)
            ! Assumptions:
            ! 0) ai > aj
            ! 1) a == b
            ! 2) i == j is admissible
            ! 3) AI changes faster then BJ
            ! 4) Occupied index changes faster than the virtual one
            ! 5) K \in 1, 2, ..., NVIRT * ((NOCC * (NOCC + 1)) / 2)
            !
            integer, intent(in)  :: k
            integer, intent(in)  :: nocc
            integer, intent(out) :: a
            integer, intent(out) :: b
            integer, intent(out) :: i 
            integer, intent(out) :: j
            integer, intent(out) :: ai
            integer, intent(out) :: bj

            integer :: v, s, t

            t = (k - 1) / ((nocc - 1) * nocc)
            a = t + nocc + 1
            b = a
            i = (k-1)/(nocc-1) + 1 - t * nocc
            s = k + 1 - ((k-1) / (nocc-1)) * (nocc -1)
            v = (1 + sign(1, -abs(i-s)) ) / 2
            j = s - v * s + v
            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(j, b, nocc)
      end subroutine triang_idx_aiaj


      pure subroutine triang_idx_aibi(k, nocc, a, b, i, j, ai, bj)
            !
            ! K -> (I, A, J, B, AI, BJ)
            ! Assumptions:
            ! 0) ai > bj
            ! 1) a != b (this is redundant, it follows from the zeroth condition)
            ! 2) i == j
            ! 3) AI changes faster than BJ
            ! 4) Occupied index changes faster than the virtual one
            ! 5) K \in 1, 2, ..., NOCC * ((NVIRT * (NVIRT - 1)) / 2)
            !
            integer, intent(in)  :: k
            integer, intent(in)  :: nocc
            integer, intent(out) :: a
            integer, intent(out) :: b
            integer, intent(out) :: i 
            integer, intent(out) :: j
            integer, intent(out) :: ai
            integer, intent(out) :: bj

            integer :: in1

            in1 = (k-1) / nocc            
            bj = k - in1 * nocc + bj_offset(in1) * nocc
            ai = k - nocc * in1 + nocc + ai_offset(in1) * nocc
            call ia2i_a(ai, nocc, i, a)
            call ia2i_a(bj, nocc, j, b)
      end subroutine triang_idx_aibi


      pure subroutine triang_idx_aibj_aneb(k, nocc, a, b, i, j, ai, bj)
            ! 
            ! K -> (I, A, J, B, AI, BJ)
            ! Assumptions:
            ! 0) ai > bj
            ! 1) a != b
            ! 2) AI changes faster than BJ
            ! 3) Occupied index changes faster than to virtual one
            ! 4) K \in 1, 2, ..., ((NVIRT * (NVIRT - 1)) / 2) * NOCC**2
            ! 5) i == j is admissible
            !
            integer, intent(in)  :: k
            integer, intent(in)  :: nocc
            integer, intent(out) :: a
            integer, intent(out) :: b
            integer, intent(out) :: i 
            integer, intent(out) :: j
            integer, intent(out) :: ai
            integer, intent(out) :: bj

            integer :: in1, nocc2

            nocc2 = nocc**2
            in1 = (k-1) / nocc2
            bj = (k-1)/nocc + 1 - in1 * nocc + bj_offset(in1) * nocc
            ai = k - nocc * ((k-1) / nocc) + nocc + ai_offset(in1) * nocc
            call ia2i_a(ai, nocc, i, a)
            call ia2i_a(bj, nocc, j, b)
      end subroutine triang_idx_aibj_aneb

      pure subroutine triang_idx_abb_ijk(u, nocc, a, b, i, j, k, ai, bj, ck)
            ! 
            ! U -> (I, A, J, B, K, AI, BJ, CK)
            ! Assumptions:
            ! 0) ai > bj
            ! 1) bj > bk
            ! 2) a != b
            ! 3) j > k
            ! 4) i != j != k
            ! 5) AI changes faster than BJ faster than CK
            ! 6) Occupied index changes faster than to virtual one
            ! 7) K \in 1, 2, ..., 
            ! ((nvirt * (nvirt-1)) / 2) * (((nocc-2)*(nocc-1)) / 2)
            ! * nocc
            !
            integer, intent(in) :: u
            integer, intent(in) :: nocc
            integer, intent(out) :: a, b, i, j, k
            integer, intent(out) :: ai, bj, ck
            integer :: occ_block
            integer :: occ_little_block
            integer :: in1, in2
            integer :: in3, s, v
            integer :: t, w


            occ_little_block = ((nocc-2)*(nocc-1)) / 2
            occ_block = nocc * occ_little_block
            in1 = (u-1)/occ_block 
            in2 = (u-1)/occ_little_block 
            ai = nocc + 1 + in2 +  nocc * (abb_ijk_offset_a(in1) - in1)
            call ia2i_a(ai, nocc, i, a)
            b = nocc + 1 + abb_ijk_offset_b(in1)
            
            in3 = u - in2 * occ_little_block - 1
            
            s = abb_ijk_offset_j(in3)
            v = 1 - (1 + sign(1, -(i-s+1)) ) / 2
            j = s - v
            t = abb_ijk_offset_k(in3)
            w = 1 - (1 + sign(1, -(i-t+1)) ) / 2
            k = t - w

            bj = i_a2ia(j, b, nocc)
            ck = i_a2ia(k, b, nocc)

      end subroutine triang_idx_abb_ijk

      pure subroutine triang_idx_abb_iji(u, nocc, a, b, i, j, ai, bj, ck)
            ! 
            ! U -> (I, A, J, B, AI, BJ, CK)
            ! Assumptions:
            ! 0) ai > bj
            ! 1) bj > bi
            ! 2) a != b
            ! 3) j > i
            ! 4) i != j 
            ! 5) AI changes faster than BJ faster than CK
            ! 6) Occupied index changes faster than to virtual one
            ! 7) K \in 1, 2, ..., 
            ! ((nvirt * (nvirt-1)) / 2) * ((nocc*(nocc-1)) / 2)
            !
            integer, intent(in) :: u
            integer, intent(in) :: nocc
            integer, intent(out) :: a, b, i, j
            integer, intent(out) :: ai, bj, ck
            integer :: occ_block
            integer :: in1


            occ_block = (nocc * (nocc - 1)) / 2
            in1 = (u-1)/occ_block 
            a = abb_ijk_offset_a(in1) + nocc + 2
            b = abb_ijk_offset_b(in1) + nocc + 1
            i = abb_iji_offset_sm(u - in1 * occ_block-1)
            j = abb_iji_offset_gt(u - in1 * occ_block-1)

            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(j, b, nocc)
            ck = i_a2ia(i, b, nocc)

      end subroutine triang_idx_abb_iji

      pure subroutine triang_idx_abb_iik(u, nocc, a, b, i, k, ai, bj, ck)
            ! 
            ! U -> (I, A, J, B, AI, BJ, CK)
            ! Assumptions:
            ! 0) ai > bi
            ! 1) bi > bk
            ! 2) a != b
            ! 3) i > k
            ! 4) i != k 
            ! 5) AI changes faster than BJ faster than CK
            ! 6) Occupied index changes faster than to virtual one
            ! 7) K \in 1, 2, ..., 
            ! ((nvirt * (nvirt-1)) / 2) * ((nocc*(nocc-1)) / 2)
            !
            integer, intent(in) :: u
            integer, intent(in) :: nocc
            integer, intent(out) :: a, b, i, k
            integer, intent(out) :: ai, bj, ck
            integer :: occ_block
            integer :: in1

            occ_block = (nocc * (nocc - 1)) / 2
            in1 = (u-1)/occ_block 
            a = abb_ijk_offset_a(in1) + nocc + 2
            b = abb_ijk_offset_b(in1) + nocc + 1
            i = abb_iji_offset_gt(u - in1 * occ_block-1)
            k = abb_iji_offset_sm(u - in1 * occ_block-1)

            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(i, b, nocc)
            ck = i_a2ia(k, b, nocc)
            
      end subroutine triang_idx_abb_iik

      subroutine triang_idx_aac_ijk(u, nocc, a, c, i, j, k, ai, bj, ck)
            ! 
            ! U -> (I, A, J, B, K, AI, BJ, CK)
            ! Assumptions:
            ! 0) ai > aj
            ! 1) aj > ck
            ! 2) a != c
            ! 3) i > j
            ! 4) i != j != k
            ! 5) AI changes faster than BJ faster than CK
            ! 6) Occupied index changes faster than to virtual one
            ! 7) K \in 1, 2, ..., 
            ! ((nvirt * (nvirt-1)) / 2) * (((nocc-2)*(nocc-1)) / 2)
            ! * nocc
            !
            integer, intent(in) :: u
            integer, intent(in) :: nocc
            integer, intent(out) :: a, c, i, j, k
            integer, intent(out) :: ai, bj, ck
            integer :: occ_block
            integer :: occ_little_block
            integer :: in1, in2, in3
            integer :: s, v
            integer :: t, w
            integer :: ai0, i0, j0, k0


            occ_little_block = ((nocc-2)*(nocc-1)) / 2
            occ_block = nocc * occ_little_block
            in1 = (u-1)/occ_block 
            in2 = (u-1)/occ_little_block 
            ai0 = nocc + 1 + in2 +  nocc * (abb_ijk_offset_a(in1) - in1)
            call ia2i_a(ai0, nocc, i0, a)
            c = nocc + 1 + abb_ijk_offset_b(in1)
            
            in3 = u - in2 * occ_little_block - 1
            s = abb_ijk_offset_j(in3)
            v = 1 - (1 + sign(1, -(i0-s+1)) ) / 2
            j0 = s - v
            t = abb_ijk_offset_k(in3)
            w = 1 - (1 + sign(1, -(i0-t+1)) ) / 2
            k0 = t - w
            
            i = j0
            j = k0
            k = i0

            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(j, a, nocc)
            ck = i_a2ia(k, c, nocc)
            
      end subroutine triang_idx_aac_ijk

      pure subroutine triang_idx_aac_ijj(u, nocc, a, c, i, j, ai, bj, ck)
            ! 
            ! U -> (I, A, J, C, AI, BJ, CK)
            ! Assumptions:
            ! 0) ai > aj
            ! 1) aj > cj
            ! 2) a != c
            ! 3) i > j
            ! 4) i != j 
            ! 5) AI changes faster than BJ faster than CK
            ! 6) Occupied index changes faster than to virtual one
            ! 7) K \in 1, 2, ..., 
            ! ((nvirt * (nvirt-1)) / 2) * ((nocc*(nocc-1)) / 2)
            !
            integer, intent(in) :: u
            integer, intent(in) :: nocc
            integer, intent(out) :: a, c, i, j
            integer, intent(out) :: ai, bj, ck
            integer :: occ_block
            integer :: in1

            occ_block = (nocc * (nocc - 1)) / 2
            in1 = (u-1)/occ_block 
            a = abb_ijk_offset_a(in1) + nocc + 2
            c = abb_ijk_offset_b(in1) + nocc + 1
            i = abb_iji_offset_gt(u - in1 * occ_block-1)
            j = abb_iji_offset_sm(u - in1 * occ_block-1)

            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(j, a, nocc)
            ck = i_a2ia(j, c, nocc)

      end subroutine triang_idx_aac_ijj

      pure subroutine triang_idx_aac_iji(u, nocc, a, c, i, j, ai, bj, ck)
            ! 
            ! U -> (I, A, J, C, AI, BJ, CK)
            ! Assumptions:
            ! 0) ai > aj
            ! 1) aj > ci
            ! 2) a != c
            ! 3) i > j
            ! 4) i != j 
            ! 5) AI changes faster than BJ faster than CK
            ! 6) Occupied index changes faster than to virtual one
            ! 7) K \in 1, 2, ..., 
            ! ((nvirt * (nvirt-1)) / 2) * ((nocc*(nocc-1)) / 2)
            !
            integer, intent(in) :: u
            integer, intent(in) :: nocc
            integer, intent(out) :: a, c, i, j
            integer, intent(out) :: ai, bj, ck
            integer :: occ_block
            integer :: in1

            occ_block = (nocc * (nocc - 1)) / 2
            in1 = (u-1)/occ_block 
            a = abb_ijk_offset_a(in1) + nocc + 2
            c = abb_ijk_offset_b(in1) + nocc + 1
            i = abb_iji_offset_gt(u - in1 * occ_block-1)
            j = abb_iji_offset_sm(u - in1 * occ_block-1)

            ai = i_a2ia(i, a, nocc)
            bj = i_a2ia(j, a, nocc)
            ck = i_a2ia(i, c, nocc)

      end subroutine triang_idx_aac_iji

      subroutine fill_offset_doubles(nocc, nvirt)
            integer, intent(in) :: nvirt
            integer, intent(in) :: nocc
            
            integer :: i, j, k, l

            allocate(bj_offset(0:nvirt *(nvirt-1) / 2 - 1))
            allocate(ai_offset(0:nvirt *(nvirt-1) / 2 - 1))
            allocate(aiaj_offset_i(1:(nocc*(nocc-1))/2 ))
            allocate(aiaj_offset_j(1:(nocc*(nocc-1))/2 ))

            ai_offset = 0
            bj_offset = 0
            aiaj_offset_i = 0
            aiaj_offset_j = 0

            k = 1
            l = nvirt *(nvirt-1) / 2 - 1
            do i = 1, nvirt -1
                  do j = 1, k
                        bj_offset(l) = nvirt - 1 - k
                        l = l -1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nvirt -1
                  do j = k, nvirt -1
                        ai_offset(l) = j - 1
                        l = l + 1
                  end do
                  k = k + 1
            end do

           l = 1
            do i = 2, nocc
                  do j = 1, i-1
                        aiaj_offset_i(l) = i
                        aiaj_offset_j(l) = j
                        l = l + 1
                  end do
            end do
      end subroutine fill_offset_doubles

      subroutine fill_offset_triples(nocc, nvirt)
            integer, intent(in) :: nvirt, nocc
            integer :: i, j, k, l

            allocate(abb_ijk_offset_a(0:(nvirt*(nvirt-1))/2-1))
            allocate(abb_ijk_offset_b(0:(nvirt*(nvirt-1))/2-1))
            allocate(abb_ijk_offset_j(0:((nocc-1)*(nocc-2))/2-1))
            allocate(abb_ijk_offset_k(0:((nocc-1)*(nocc-2))/2-1))

            allocate(abb_iji_offset_sm(0:((nocc-1)*nocc)/2-1))
            allocate(abb_iji_offset_gt(0:((nocc-1)*nocc)/2-1))
            
            k = 1
            l = 0
            do i = 1, nvirt -1
                  do j = 1, k
                        abb_ijk_offset_a(l) = k-1
                        l = l + 1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nvirt -1
                  do j = 1, k
                        abb_ijk_offset_b(l) = j-1
                        l = l + 1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nocc -2
                  do j = 1, k
                        abb_ijk_offset_j(l) = 3 + k-1
                        l = l + 1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nocc -2
                  do j = 1, k
                        abb_ijk_offset_k(l) = 2 + j-1
                        l = l + 1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nocc -1
                  do j = 1, k
                        abb_iji_offset_gt(l) = 2 + k-1
                        l = l + 1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nocc - 1
                  do j = 1, k
                        abb_iji_offset_sm(l) = 1 + j-1
                        l = l + 1
                  end do
                  k = k + 1
            end do

      end subroutine fill_offset_triples

      subroutine fill_offset_doubles_free()
            deallocate(bj_offset)
            deallocate(ai_offset)
            deallocate(aiaj_offset_i)
            deallocate(aiaj_offset_j)
      end subroutine fill_offset_doubles_free

      subroutine fill_offset_triples_free()
            deallocate(abb_ijk_offset_a)
            deallocate(abb_ijk_offset_b)
            deallocate(abb_ijk_offset_j)
            deallocate(abb_ijk_offset_k)

            deallocate(abb_iji_offset_sm)
            deallocate(abb_iji_offset_gt)

      end subroutine fill_offset_triples_free
end module cmpidx
