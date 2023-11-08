module guess_davidson

      use gparam
      use linalg
      use t1_transformed_int
      use math_constants
      use arithmetic
      use cc_gparams
      use ccjac_block_diag
      use symmetry
!      use jacobian_triplet
      use threads

      implicit none
      save


contains

      ! subroutine task_guess_driver(iexci, t2, wrci, rtstart, ltstart, nocc0, nocc1, nvirt0, nvirt1, &
      !       e_total, e_electron, order, irrep0, irrep1, sd_dom, nocc, nvirt)

      !       integer, dimension(:), intent(in)       :: iexci
      !       real(F64), dimension(nocc+1:nactive, nocc+1:nactive, &
      !             nocc, nocc), intent(in)                                   :: t2
      !       real(F64), dimension(:, :), intent(out) :: wrci
      !       real(F64), dimension(:,:), intent(out)  :: rtstart, ltstart
      !       integer, intent(in)                     :: nocc0, nocc1
      !       integer, intent(in)                     :: nvirt0, nvirt1
      !       integer, intent(in) :: nocc, nvirt

      !       real(F64), intent(in)                   :: e_total, e_electron
      !       integer, intent(in)                     :: order
      !       integer, dimension(:,:), intent(in)     :: irrep0
      !       integer, dimension(:,:), intent(in)     :: irrep1
      !       integer, intent(in)                     :: sd_dom

      !       double precision, dimension(:), allocatable :: guess

      !       nocc = nocc1 - nocc0 + 1
      !       nvirt = nvirt1 - nvirt0 + 1
      !       nexc_single = nocc * nvirt
      !       nidx = nexc_single + (nexc_single * (nexc_single + 1)) / 2
      !       nexc_double = nidx - nexc_single

      !       allocate(guess(nidx))

      !       rtstart = zero
      !       ltstart = zero
      !       wrci = zero

      !       call guess_update(guess, t2, nocc, nocc0, nocc1, &
      !             nvirt0, nvirt1, nactive, npair, nidx_table, nvirt, &
      !             irrep0, irrep1, irrep_idx)

      !       if (sd_dom .eq. CIS_METHOD) then
      !             do j = 1, iexci(i)
      !                   wrci(j, i) = e_new(j)
      !                   rtstart(1:nexc_single, rtoffset + j) = guess_vec_r(:, j)
      !                   ltstart(1:nexc_single, rtoffset + j) = guess_vec_l(:, j)
      !             end do
      !       else if (sd_dom .eq. CID_METHOD) then 
      !             do j = 1, iexci(i)
      !                   wrci(j, i) = e_new(j)
      !                   rtstart(nexc_single+1:nexc_single + nexc_double, rtoffset + j) = &
      !                         guess_vec_r(2:nexc_double + 1, j)
      !                   ltstart(nexc_single+1:nexc_single + nexc_double, rtoffset + j) = &
      !                         guess_vec_l(2:nexc_double + 1, j)
      !             end do
      !       end if

      !       if (allocated(e_new_merged)) deallocate(e_new_merged)
      !       if (allocated(dy)) deallocate(dy)
      !       deallocate(e_new)

      !       if (allocated(guess_vec_r)) deallocate(guess_vec_r)
      !       if (allocated(guess_vec_l)) deallocate(guess_vec_l)

      !       deallocate(guess)

      ! end subroutine task_guess_driver


      subroutine guess_update(guess, t2, nocc, nocc0, nocc1, &
            nvirt0, nvirt1, nactive, npair, nidx_table, nvirt, &
            irrep0, irrep1, irrep_idx, sdim, ddim)

            real(F64), dimension(:), intent(out) :: guess
            integer, intent(in)                                             :: nocc
            integer, intent(in)                                             :: nactive
            integer, intent(in)                                             :: nvirt
            real(F64), dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(in)                                   :: t2
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, dimension(2), intent(in)                               :: nidx_table
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(out) :: sdim, ddim
            integer, parameter :: plus = 1
            integer, parameter :: minus = -1
            integer :: nidx_ccsd
            integer :: ntasks

            guess = zero

!            if (cc_multip == cc_singlet) then
                  nidx_ccsd = nidx_table(1)        

                  call ccjac_11_driver_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, nidx_ccsd, &
                        nvirt, nocc, &
                        irrep0, irrep1, irrep_idx, sdim)
                  call ccjac_22_driver_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
                        nvirt, nocc, &
                        irrep0, irrep1, irrep_idx, ddim)
 !           end if

            ! else if (cc_multip == cc_triplet) then
            !       nidx_plus = nidx_table(1)
            !       nidx_minus = nidx_table(2)

            !       call ccjac_11_driver_triplet_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, &
            !             nvirt, nocc, &
            !             irrep0, irrep1, irrep_idx)
            !       call ccjac_22_driver_triplet(guess, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            !             nidx_plus, &
            !             nvirt, nocc, &
            !             irrep0, irrep1, irrep_idx, plus, plus)
            !       call ccjac_22_driver_triplet(guess, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            !             nidx_plus, &
            !             nvirt, nocc, &
            !             irrep0, irrep1, irrep_idx, plus, minus)
            !       call ccjac_22_driver_triplet(guess, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            !             nidx_plus, &
            !             nvirt, nocc, &
            !             irrep0, irrep1, irrep_idx, minus, plus)
            !       call ccjac_22_driver_triplet(guess, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            !             nidx_plus, &
            !             nvirt, nocc, &
            !             irrep0, irrep1, irrep_idx, minus, minus)
            ! end if

      end subroutine guess_update

      subroutine ccjac_11_driver_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1,  &
            nidx_ccsd, nvirt, nocc, &
            irrep0, irrep1, irrep_idx, sdim)

            real(F64), dimension(:), intent(inout) :: guess
            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: nidx_ccsd
            integer, intent(in)                                             :: nvirt, nocc
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(out) :: sdim

            integer :: ntasks

            integer :: m0a, m1a
            integer :: m0i, m1i
            integer :: bj
            integer :: idims

            integer, dimension(:, :), allocatable :: isingles


            call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

            ntasks = 0
            sdim = 0
            do bj = 1, idims

                  call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a)

                  call ccjac_11_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, &
                        m0a, m1a, m0i, m1i, 1, 1, sdim)
            end do

            deallocate(isingles)


      end subroutine ccjac_11_driver_diag

      subroutine ccjac_22_driver_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_ccsd, nvirt, nocc, &
            irrep0, irrep1, irrep_idx, ddim)
            real(F64), dimension(:), intent(inout) :: guess
            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair, nidx_ccsd
            integer, intent(in)                                             :: nvirt, nocc

            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(out) :: ddim

            integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b
            integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j

            integer, dimension(:,:), allocatable :: idoubles
            integer :: idim
            integer :: m0a, m1a, m0b, m1b
            integer :: m0i, m1i, m0j, m1j

            integer :: bj, idimd

            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))            
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idim, .false., .true.)


             ddim = 0
            do bj = 1, idim
                  call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                        m0j, m1j, m0b, m1b)
                  call ccjac_22_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, &
                        m0a, m1a, m0b, m1b, &
                        m0i, m1i, m0j, m1j, npair + 1, npair + 1, ddim)
            end do

      end subroutine ccjac_22_driver_diag

end module guess_davidson
