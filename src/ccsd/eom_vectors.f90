module eom_vectors

      use cc3_intermediates
      use s_gen
      use basis
      use symmetry

      implicit none

      integer, private :: ev_nocc0, ev_nvirt0, ev_nocc1, ev_nvirt1, ev_nocc, ev_nvirt, ev_npair
      integer, private :: qbj, qbj2
      integer, private :: qck, qck2
      integer, private :: q00
      integer, private :: nidx_ccsd_sing, nidx_ccsd_trip
      integer, private :: nidx


contains

      subroutine eom_vectors_init(nc0, nc1, nv0, nv1, nc, nv)
            integer, intent(in) :: nc0, nc1
            integer, intent(in) :: nv0, nv1
            integer, intent(in) :: nc, nv

            ev_nocc = nc
            ev_nvirt = nv
            ev_npair = ev_nocc * ev_nvirt
            ev_nocc0 = nc0
            ev_nocc1 = nc1
            ev_nvirt0 = nv0
            ev_nvirt1 = nv1
            nidx_ccsd_sing = ev_npair + (ev_npair * (ev_npair + 1)) / 2 
            nidx_ccsd_trip = ev_npair + ev_npair * (ev_nvirt - 1) * (ev_nocc - 1)/ 4 + ev_npair * (ev_npair - 1) / 2
            

            qbj  = 3 + 6 * ev_npair
            qbj2 = -3
            qck  = 2 + 3 * ev_npair * (2 + ev_npair)
            qck2 = -3 * (1 + ev_npair)
            q00  = -3 * ev_npair * (3 + ev_npair)

      end subroutine eom_vectors_init


      function r1(R, a, i)
            double precision :: r1
            double precision, dimension(:), intent(in) :: R
            integer, intent(in) :: a, i
            integer :: ai

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1

            r1 = R(ai)

      end function r1

      function r2(R, a, i, b, j)
            double precision :: r2
            double precision, dimension(:), intent(in) :: R
            integer, intent(in) :: a, i, b, j
            integer :: ai, bj, aibj

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1


            if(ai.gt.bj)then
                  aibj = ((2 * ev_npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1 + ev_npair
                  r2 = R(aibj)
            else if (ai.lt.bj)then
                  aibj = ((2 * ev_npair - ai + 2) * (ai - 1)) / 2 + bj - ai + 1 + ev_npair
                  r2 = R(aibj)
            else if (ai.eq.bj)then
                  aibj = ((2 * ev_npair - ai + 2) * (ai - 1)) / 2  + 1 + ev_npair
                  r2 = TWO * R(aibj)
            end if

      end function r2

      function r2p(R, a, i, b, j)
            double precision :: r2p
            double precision, dimension(:), intent(in) :: R
            integer, intent(in) :: a, i, b, j
            integer :: aibj

            if (a.gt.b.and.i.gt.j) then
                  aibj = (ev_nocc * (ev_nocc - 1) * (a - ev_nvirt0) * (a - 1 - ev_nvirt0)) / 4 + &
                        ((a - ev_nvirt0) * (i - 1) * (i - 2)) / 2 + (b - ev_nvirt0) * (i - 1) + j + ev_npair
                  r2p = R(aibj)
            else if (a.gt.b.and.i.lt.j) then
                  aibj = (ev_nocc * (ev_nocc - 1) * (a - ev_nvirt0) * (a - 1 - ev_nvirt0)) / 4 + &
                        ((a - ev_nvirt0) * (j - 1) * (j - 2)) / 2 + (b - ev_nvirt0) * (j - 1) + i + ev_npair
                  r2p = -R(aibj)
            else if (a.lt.b.and.i.gt.j) then
                  aibj = (ev_nocc * (ev_nocc - 1) * (b - ev_nvirt0) * (b - 1 - ev_nvirt0)) / 4 + &
                        ((b - ev_nvirt0) * (i - 1) * (i - 2)) / 2 + (a - ev_nvirt0) * (i - 1) + j + ev_npair
                  r2p = -R(aibj)
            else if (a.lt.b.and.i.lt.j) then
                  aibj = (ev_nocc * (ev_nocc - 1) * (b - ev_nvirt0) * (b - 1 - ev_nvirt0)) / 4 + &
                        ((b - ev_nvirt0) * (j - 1) * (j - 2)) / 2 + (a - ev_nvirt0) * (j - 1) + i + ev_npair
                  r2p = R(aibj)
            else
                  r2p = zero
            end if
      end function r2p

      function r2m(R, a, i, b, j)
            double precision :: r2m
            double precision, dimension(:), intent(in) :: R
            integer, intent(in) :: a, i, b, j
            integer :: ai, bj, aibj
            integer :: nidx_plus

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1

            nidx_plus = ev_npair * (ev_nvirt - 1) * (ev_nocc - 1)/ 4


            if(ai.gt.bj)then
                  aibj = ((ai - 1) * (ai - 2)) / 2 + bj + ev_npair + nidx_plus
                  r2m = R(aibj)
            else if (ai.lt.bj)then
                  aibj = ((bj - 1) * (bj - 2)) / 2 + ai + ev_npair + nidx_plus
                  r2m = R(aibj)
            else if (ai .eq. bj)then
                  r2m =zero
            end if

      end function r2m

      subroutine r22(r2, R, a, i, b, j)
            double precision, intent(out) :: r2
            double precision, dimension(:), intent(in) :: R
            integer, intent(in) :: a, i, b, j
            integer :: ai, bj, aibj

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1


            if(ai.gt.bj)then
                  aibj = ((2 * ev_npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1 + ev_npair
                  r2 = R(aibj)
            else if (ai.lt.bj)then
                  aibj = ((2 * ev_npair - ai + 2) * (ai - 1)) / 2 + bj - ai + 1 + ev_npair
                  print*, 'tutaj', a, b, i, j, ai, bj, ev_nvirt0, ev_nocc, ev_nocc0, aibj
                  r2 = R(aibj)
            else if (ai.eq.bj)then
                  aibj = ((2 * ev_npair - ai + 2) * (ai - 1)) / 2  + 1 + ev_npair
                  r2 = TWO * R(aibj)
            end if

      end subroutine r22

      function r3_trip(R, a, i, b, j, c, k)
            real(F64) :: r3_trip
            real(F64), dimension(:), intent(in) :: R
            integer, intent(in) :: a, i, b, j, c, k
            integer :: ai, bj, ck, aibjck

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
            ck = (c - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1

            if (b .eq. c) then
                  r3_trip = zero
            else if (j .eq. k) then
                  r3_trip = zero
            else if (b .lt. c .and. j .gt. k) then
                  aibjck = nidx_ccsd_trip + idx(a, i, c, j, b, k)
                  r3_trip = R(aibjck)
            else if (b .gt. c .and. j .lt. k) then
                  aibjck = nidx_ccsd_trip + idx(a, i, b, k, c, j)
                  r3_trip = R(aibjck)
            else if (b .lt. c .and. j .lt. k) then
                  aibjck = nidx_ccsd_trip + idx(a, i, c, j, b, k)
                  r3_trip = R(aibjck)
            else if (b .gt. c .and. j .gt. k) then
                  aibjck = nidx_ccsd_trip + idx(a, i, b, j, c, k)
                  r3_trip = R(aibjck)
            else
                  r3_trip = zero
            end if

      contains
            function idx(a, i, b, j, c, k)
                  integer :: idx
                  integer, intent(in) :: a, i, b, j, c, k

                  idx = (ai-1) * ev_npair * (ev_nvirt - 1)*(ev_nocc - 1)/4 + &
                        (ev_nocc * (ev_nocc - 1) * (b - ev_nvirt0) * (b - 1 - ev_nvirt0)) / 4 + &
                        ((b - ev_nvirt0) * (j - 1) * (j - 2)) / 2 + (c - ev_nvirt0) * (j - 1) + k

            end function idx
      end function r3_trip
                  

      function r3(R, a, i, b, j, c, k)
            double precision :: r3
            double precision, dimension(:), intent(in) :: R
            integer, intent(in) :: a, i, b, j, c, k
            integer :: ai, bj, ck, aibjck

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
            ck = (c - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1

            if((ai.ne.bj).and.(ai.ne.ck).and.(bj.ne.ck))then
                  if((ai > bj).and.(bj > ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  else if((ai > ck).and.(ck > bj))then
                        aibjck = nidx_ccsd_sing + mu3(ai, ck, bj)
                  else if((bj > ai).and.(ai > ck))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ai, ck)
                  else if((bj > ck).and.(ck > ai))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ck, ai)
                  else if((ck > ai).and.(ai > bj))then
                        aibjck = nidx_ccsd_sing + mu3(ck, ai, bj)
                  else if((ck > bj).and.(bj > ai))then
                        aibjck = nidx_ccsd_sing + mu3(ck, bj, ai)
                  end if
                  r3 = R(aibjck)
            else if((ai == bj).and.(bj == ck))then
                  aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  r3 = SIX * R(aibjck)
            else      
                  if((ai == bj).and.(bj > ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  else if((ai == bj).and.(bj < ck))then
                        aibjck = nidx_ccsd_sing + mu3(ck, ai, bj)
                  else if((ai > bj).and.(bj == ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  else if((ai < bj).and.(bj == ck))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ck, ai)
                  else if((ai > bj).and.(ai == ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, ck, bj)
                  else if((ai < bj).and.(ai == ck))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ai, ck)
                  end if
                  r3 = THREE * R(aibjck)
            end if

      end function r3

      function l1(L, a, i)
            double precision :: l1
            double precision, dimension(:), intent(in) :: L
            integer, intent(in) :: a, i
            integer :: ai

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1

            l1 = L(ai)

      end function l1

      function l2(L, a, i, b, j)
            double precision :: l2
            double precision, dimension(:), intent(in) :: L
            integer, intent(in) :: a, i, b, j
            integer :: ai, bj, aibj

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1

            if(ai.gt.bj)then
                  aibj = ((2 * ev_npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1 + ev_npair
                  l2 = L(aibj)
            else if (ai.lt.bj)then
                  aibj = ((2 * ev_npair - ai + 2) * (ai - 1)) / 2 + bj - ai + 1+ ev_npair
                  l2 = L(aibj)
            else if (ai.eq.bj)then
                  aibj = ((2 * ev_npair - ai + 2) * (ai - 1)) / 2  + 1+ ev_npair
                  l2 = TWO * L(aibj)
            end if

      end function l2

      function l3(L, a, i, b, j, c, k)
            double precision :: l3
            double precision, dimension(:), intent(in) :: L
            integer, intent(in) :: a, i, b, j, c, k
            integer :: ai, bj, ck, aibjck

            ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
            bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
            ck = (c - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1

            if((ai.ne.bj).and.(ai.ne.ck).and.(bj.ne.ck))then
                  if((ai > bj).and.(bj > ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  else if((ai > ck).and.(ck > bj))then
                        aibjck = nidx_ccsd_sing + mu3(ai, ck, bj)
                  else if((bj > ai).and.(ai > ck))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ai, ck)
                  else if((bj > ck).and.(ck > ai))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ck, ai)
                  else if((ck > ai).and.(ai > bj))then
                        aibjck = nidx_ccsd_sing + mu3(ck, ai, bj)
                  else if((ck > bj).and.(bj > ai))then
                        aibjck = nidx_ccsd_sing + mu3(ck, bj, ai)
                  end if
                  l3 = L(aibjck)
            else if((ai == bj).and.(bj == ck))then
                  aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  l3 = SIX * L(aibjck)
            else      
                  if((ai == bj).and.(bj > ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  else if((ai == bj).and.(bj < ck))then
                        aibjck = nidx_ccsd_sing + mu3(ck, ai, bj)
                  else if((ai > bj).and.(bj == ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, bj, ck)
                  else if((ai < bj).and.(bj == ck))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ck, ai)
                  else if((ai > bj).and.(ai == ck))then
                        aibjck = nidx_ccsd_sing + mu3(ai, ck, bj)
                  else if((ai < bj).and.(ai == ck))then
                        aibjck = nidx_ccsd_sing + mu3(bj, ai, ck)
                  end if
                  l3 = THREE * L(aibjck)
            end if

      end function l3

      function mu3(ai, bj, ck)
            !
            ! Compute compound three-electron index
            ! (assumed that ai >= bj >= ck)
            !
            integer :: mu3
            integer, intent(in) :: ai, bj, ck
            integer :: mu31, mu32
            !
            ! Compound index is compouted relative
            ! to the first element of matrix block.
            ! Braoffset/ketoffset should be added
            ! to get the absolute position
            !
            mu31 = (qbj + qbj2 * bj) * bj 
            mu32 = (qck + (qck2 + ck) * ck) * ck
            mu3  = ai + (mu31 + mu32 + q00) / 6
      end function mu3


      subroutine translate_triples_base(L, nidx_ccsd, nidx, nsought, ev_nocc0, ev_nocc1, ev_nvirt0, ev_nvirt1)
            !
            ! Translate the triples part of the left EOM-CC eigenvectors
            ! from the biorthonormal, nonredundant basis
            ! to the primitive, nonredundant basis.
            ! The transformation is required because Paldus 
            ! derives expressions for the transition moments in the
            ! redundant, primitive basis. The transformation
            ! to the redundant basis is carried out in the 
            ! l1, l2, l3, r1, r2, r3 functions.
            ! For more information, please consult the pdf
            ! file in the ./doc/cc/ directory.
            !
            double precision, dimension(:,:), intent(inout) :: L
            integer, intent(in) :: nidx_ccsd
            integer, intent(in) :: nidx
            integer, intent(in) :: nsought
            integer, intent(in) :: ev_nocc0, ev_nocc1
            integer, intent(in) :: ev_nvirt0, ev_nvirt1

            double precision, dimension(:), allocatable :: L_aux

            double precision :: val
            integer :: pos

            integer :: a, b, c, i, j, k
            integer :: ai, bj, ck
            integer :: w
            integer :: offset
            integer :: ev_nocc, ev_nvirt

            ev_nocc = ev_nocc1 - ev_nocc0 + 1
            ev_nvirt = ev_nvirt1 - ev_nvirt0 + 1

            offset = nidx_ccsd
            allocate(L_aux(nidx-nidx_ccsd))


            L_aux = ZERO
            do w = 1, nsought
                  L_aux = L(nidx_ccsd+1:nidx, w)
                  L(nidx_ccsd+1:nidx, w) = zero


                  do c = ev_nvirt0, ev_nvirt1
                        do b = c + 1, ev_nvirt1
                              do a = b + 1, ev_nvirt1
                                    do i = ev_nocc0, ev_nocc1
                                          ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          do j = ev_nocc0, ev_nocc1
                                                bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                kloop: do k = ev_nocc0, ev_nocc1
                                                      ck = (c - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1
                                                      pos = mu3(ai, bj, ck)
                                                      val = L_aux(pos)
                                                      if ((i > j).and.(j > k))then
                                                            exit kloop
                                                      else if ((i > j).and.(k > i))then
                                                            !kij
                                                            call v3_update(val, L(:, w), a, b, c, i, j, k, offset)
                                                      else if((i > k).and. (k > j))then    
                                                            !ikj
                                                            call v1_update(val, L(:, w), a, b, c, i, j, k, offset)
                                                      else if ((j > i).and.(i > k))then                                
                                                            !jik
                                                            call v2_update(val, L(:, w), a, b, c, i, j, k, offset)
                                                      else if ((i < j).and.(k > j))then 
                                                            !kji
                                                            call v5_update(val, L(:, w), a, b, c, i, j, k, offset)                               
                                                      else if ((j > k).and.(k > i))then                                
                                                            !jki
                                                            call v4_update(val, L(:, w), a, b, c, i, j, k, offset)
                                                      end if
                                                end do kloop
                                          end do
                                    end do
                              end do
                        end do
                  end do

                  do c = ev_nvirt0, ev_nvirt1
                        do b = c + 1 , ev_nvirt1
                              do a = b + 1, ev_nvirt1
                                    do j = ev_nocc0, ev_nocc1
                                          do i = j + 1, ev_nocc1
                                                ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                ck = (c - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)
                                                call v0_update(val, L(:, w), a, b, c, i, j, i, offset) !iji
                                                ck = (c - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v0_update(val, L(:, w), a, b, c, i, j, j, offset) !ijj
                                                bj = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1

                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)
                                                call v0_update(val, L(:, w), a, b, c, i, i, j, offset) !iij

                                                ai = (a - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                bj = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                ck = (c - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v0_update(val, L(:, w), a, b, c, j, i, j, offset) !jij
                                                ck = (c - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)
                                                call v0_update(val, L(:, w), a, b, c, j, i, i, offset) !jii
                                                bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                ck = (c - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1

                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v0_update(val, L(:, w), a, b, c, j, j, i, offset) !jji

                                          end do
                                    end do
                              end do
                        end do
                  end do

                  do b = ev_nvirt0, ev_nvirt1
                        do a = b + 1, ev_nvirt1
                              do j = ev_nocc0, ev_nocc1
                                    do i = j + 1, ev_nocc1
                                          ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          bj = (a - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                          ck = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1

                                          pos = mu3(ai, bj, ck)
                                          val = L_aux(pos)

                                          call v_update(val, L(:, w), a, a, b, i, j, j, offset)

                                          ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          bj = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          ck = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                          pos = mu3(ai, bj, ck)
                                          val = L_aux(pos)

                                          call v_update(val, L(:, w), a, b, b, i, i, j, offset)

                                    end do
                              end do
                        end do
                  end do

                  do k = ev_nocc0, ev_nocc1
                        do j = k + 1, ev_nocc1
                              do i = j + 1, ev_nocc1
                                    do b = ev_nvirt0, ev_nvirt1
                                          do a = b + 1, ev_nvirt1

                                                ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                bj = (a - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1
                                                ck = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v6_update(val, L(:, w), a, a, b, i, k, j, offset)

                                                ai = (a - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                bj = (a - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1
                                                ck = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v6_update(val, L(:, w), a, a, b, j, k, i, offset)


                                                ai = (a - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                bj = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                ck = (b - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v6_update(val, L(:, w), a, b, b, j, i, k, offset)

                                                ai = (a - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1
                                                bj = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                                ck = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                                pos = mu3(ai, bj, ck)
                                                val = L_aux(pos)

                                                call v6_update(val, L(:, w), a, b, b, k, i, j, offset)
                                          end do
                                    end do
                              end do
                        end do
                  end do

                  do j = ev_nocc0, ev_nocc1
                        do i = j + 1, ev_nocc1
                              do b = ev_nvirt0, ev_nvirt1
                                    do a = b + 1, ev_nvirt1
                                          ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          bj = (a - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                          ck = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          pos = mu3(ai, bj, ck)
                                          val = L_aux(pos)
                                          call v_update(val, L(:, w), a, a, b, i, j, i, offset)

                                          ai = (a - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                          bj = (b - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                                          ck = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                                          pos = mu3(ai, bj, ck)
                                          val = L_aux(pos)
                                          call v_update(val, L(:, w), a, b, b, j, i, j, offset)

                                    end do
                              end do
                        end do
                  end do
            end do

            deallocate(L_aux)

      contains

            subroutine v_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1

                  !
                  ! <~ijk| = 1/2 <ijk|
                  !
                  c1 = 1.d+0/2.d+0

                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c1 * val

            end subroutine v_update

            subroutine v0_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2

                  !
                  ! <~ijk| = 1/3 <ijk| + 1/6<jik|
                  !
                  c1 = 1.d+0/3.d+0
                  c2 = 1.d+0/6.d+0

                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c1 * val
                  L(offset + compute_pos(a, b, c, j, i, k)) = L(offset + compute_pos(a, b, c, j, i, k)) + c2 * val

            end subroutine v0_update

            subroutine v6_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2

                  !
                  ! <~ijk| = 1/3 <ijk| + 1/6<kji|
                  !
                  c1 = 1.d+0/3.d+0
                  c2 = 1.d+0/6.d+0

                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c1 * val
                  L(offset + compute_pos(a, b, c, k, j, i)) = L(offset + compute_pos(a, b, c, k, j, i)) + c2 * val

            end subroutine v6_update

            subroutine v1_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2, c3, c4, c5

                  !
                  ! <~ijk| = 1/4 <ijk| + 1/12<kij| + 1/6 <kji| + 1/6<jik| + 1/12<jki|
                  !
                  c1 = 0.25d+0
                  c2 = 1.d+0/12.d+0
                  c3 = 1.d+0/6.d+0
                  c4 = 1.d+0/6.d+0
                  c5 = 1.d+0/12.d+0

                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c1 * val
                  L(offset + compute_pos(a, b, c, k, i, j)) = L(offset + compute_pos(a, b, c, k, i, j)) + c2 * val
                  L(offset + compute_pos(a, b, c, k, j, i)) = L(offset + compute_pos(a, b, c, k, j, i)) + c3 * val
                  L(offset + compute_pos(a, b, c, j, i, k)) = L(offset + compute_pos(a, b, c, j, i, k)) + c4 * val
                  L(offset + compute_pos(a, b, c, j, k, i)) = L(offset + compute_pos(a, b, c, j, k, i)) + c5 * val

            end subroutine v1_update

            subroutine v2_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2, c3, c4, c5

                  !
                  ! <~ijk| = 1/12 <jki| + 1/4<ijk| + 1/6 <ikj| + 1/6<kji| + 1/12<kij|
                  !
                  c1 = 1.d+0/12.d+0
                  c2 = 1.d+0/4.d+0
                  c3 = 1.d+0/6.d+0
                  c4 = 1.d+0/6.d+0
                  c5 = 1.d+0/12.d+0
                  L(offset + compute_pos(a, b, c, j, k, i)) = L(offset + compute_pos(a, b, c, j, k, i)) + c1 * val
                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c2 * val
                  L(offset + compute_pos(a, b, c, i, k, j)) = L(offset + compute_pos(a, b, c, i, k, j)) + c3 * val
                  L(offset + compute_pos(a, b, c, k, j, i)) = L(offset + compute_pos(a, b, c, k, j, i)) + c4 * val
                  L(offset + compute_pos(a, b, c, k, i, j)) = L(offset + compute_pos(a, b, c, k, i, j)) + c5 * val

            end subroutine v2_update

            subroutine v3_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2, c3, c4, c5

                  !
                  ! <~ijk| = 1/6 <kji| + 1/6<ijk| + 1/3 <ijk| + 1/6<jki| + 1/6<jik|
                  !
                  c1 = 1.d+0/6.d+0
                  c2 = 1.d+0/6.d+0
                  c3 = 1.d+0/3.d+0
                  c4 = 1.d+0/6.d+0
                  c5 = 1.d+0/6.d+0
                  L(offset + compute_pos(a, b, c, k, j, i)) = L(offset + compute_pos(a, b, c, k, j, i)) + c1 * val
                  L(offset + compute_pos(a, b, c, i, k, j)) = L(offset + compute_pos(a, b, c, i, k, j)) + c2 * val
                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c3 * val
                  L(offset + compute_pos(a, b, c, j, k, i)) = L(offset + compute_pos(a, b, c, j, k, i)) + c4 * val
                  L(offset + compute_pos(a, b, c, j, i, k)) = L(offset + compute_pos(a, b, c, j, i, k)) + c5 * val

            end subroutine v3_update

            subroutine v4_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2, c3, c4, c5

                  !
                  ! <~ijk| = 1/6 <jik| + 1/6<kji| + 1/6 <kij| + 1/3<ijk| + 1/6<ikj|
                  !
                  c1 = 1.d+0/6.d+0
                  c2 = 1.d+0/6.d+0
                  c3 = 1.d+0/6.d+0
                  c4 = 1.d+0/3.d+0
                  c5 = 1.d+0/6.d+0
                  L(offset + compute_pos(a, b, c, j, i, k)) = L(offset + compute_pos(a, b, c, j, i, k)) + c1 * val
                  L(offset + compute_pos(a, b, c, k, j, i)) = L(offset + compute_pos(a, b, c, k, j, i)) + c2 * val
                  L(offset + compute_pos(a, b, c, k, i, j)) = L(offset + compute_pos(a, b, c, k, i, j)) + c3 * val
                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c4 * val
                  L(offset + compute_pos(a, b, c, i, k, j)) = L(offset + compute_pos(a, b, c, i, k, j)) + c5 * val

            end subroutine v4_update

            subroutine v5_update(val, L, a, b, c, i, j, k, offset)
                  double precision, intent(in) :: val
                  double precision, dimension(:), intent(inout) :: L
                  integer, intent(in) :: a, b, c
                  integer, intent(in) :: i, j, k
                  integer, intent(in) :: offset
                  double precision :: c1, c2, c3, c4, c5

                  !
                  ! <~ijk| = 1/12 <kij| + 1/12<jki| + 1/6 <jik| + 1/6<ikj| + 1/4<ijk|
                  !
                  c1 = 1.d+0/12.d+0
                  c2 = 1.d+0/12.d+0
                  c3 = 1.d+0/6.d+0
                  c4 = 1.d+0/6.d+0
                  c5 = 1.d+0/4.d+0
                  L(offset + compute_pos(a, b, c, k, i, j)) = L(offset + compute_pos(a, b, c, k, i, j)) + c1 * val
                  L(offset + compute_pos(a, b, c, j, k, i)) = L(offset + compute_pos(a, b, c, j, k, i)) + c2 * val
                  L(offset + compute_pos(a, b, c, j, i, k)) = L(offset + compute_pos(a, b, c, j, i, k)) + c3 * val
                  L(offset + compute_pos(a, b, c, i, k, j)) = L(offset + compute_pos(a, b, c, i, k, j)) + c4 * val
                  L(offset + compute_pos(a, b, c, i, j, k)) = L(offset + compute_pos(a, b, c, i, j, k)) + c5 * val

            end subroutine v5_update

            function compute_pos(a, b, c, i, j, k)
                  integer :: compute_pos
                  integer, intent(in) :: a, b, c, i, j, k
                  integer :: ai, bj, ck

                  ai = (a - ev_nvirt0) * ev_nocc + (i - ev_nocc0) + 1
                  bj = (b - ev_nvirt0) * ev_nocc + (j - ev_nocc0) + 1
                  ck = (c - ev_nvirt0) * ev_nocc + (k - ev_nocc0) + 1

                  compute_pos = mu3(ai, bj, ck)

            end function compute_pos

      end subroutine translate_triples_base


end module eom_vectors
