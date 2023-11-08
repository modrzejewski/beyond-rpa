module lresp
      use gparam
      use xckernel
      use ints

      implicit none

contains

      ! subroutine tdhf_lresp_aibj(aaibj, baibj, fpqrs, faibj, fabij, &
      !       mocoeff, nocc, nvirt, bj0, bj1, maxnrs, maxnbj, maxnij, &
      !       iwork)
            
      !       double precision, dimension(:, :), intent(inout) :: aaibj
      !       double precision, dimension(:, :), intent(inout) :: baibj
      !       double precision, dimension(:, :), intent(out)   :: fpqrs
      !       double precision, dimension(:, :), intent(out)   :: faibj
      !       double precision, dimension(:, :), intent(out)   :: fabij
      !       double precision, dimension(:, :), intent(in)    :: mocoeff
      !       integer, intent(in)                              :: nocc
      !       integer, intent(in)                              :: nvirt
      !       integer, intent(in)                              :: ibj0
      !       integer, intent(in)                              :: ibj1
      !       integer, intent(in)                              :: maxnrs
      !       integer, intent(in)                              :: maxnbj
      !       integer, intent(in)                              :: maxnij
      !       integer, dimension(:), intent(out)               :: iwork

      !       integer :: bj, bjstart, bjstop
      !       integer :: b, j, a, i
      !       integer :: bstart, bstop, jstart, jstop
      !       integer :: iai, ibj
      !       integer :: shift

      !       shift = -ibj0 + 1
      !       bjstart = ibj0
      !       bjstop = ibj0
      !       do bj = ibj0+nbj-1, ibj1, nbj
      !             bjstop = bj
      !             !
      !             ! BJ -> (B, J)
      !             !
      !             call faibj_b_j(bstart, jstart, ibjstart, nvirt)
      !             call faibj_b_j(bstop, jstop, ibjstop, nvirt)

      !             call motransf_aibj(faibj, mocoeff, nocc, nvirt, fpqrs, &
      !                   maxnrs, wpqrs, wairs, bstart, bstop, jstart, jstop, iwork)

      !             do j = jstart, jstop
      !                   iaj = faibj_iaj(a, j, nvirt)
      !                   do b = bstart, bstop
      !                         ibj = faibj_ibj(b, j, nvirt)
      !                         !
      !                         ! Diagonal occupied indices i==j
      !                         !
      !                         i = j
      !                         ibi = faibj_ibj(b, i, nvirt)
      !                         !
      !                         ! Read (AI|BI) integral
      !                         !
      !                         aibj = faibj(iai, ibj+shift)

                              

      !                         !
      !                         ! Off-diagonal occupied indices
      !                         !
      !                         do i = j+1, nocc
      !                               ibi = faibj_ibj(b, i, nvirt)
      !                               !
      !                               ! Read (AI|BJ) integral
      !                               !
      !                               aibj = faibj(iai, ibj+shift)


                                    
      !                         end do
      !                   end do
      !             end do

      !             bjstart = bjstop + 1
      !             bjstop = bjstop + 1
      !       end do
      ! end subroutine tdhf_lresp_aibj
end module lresp
