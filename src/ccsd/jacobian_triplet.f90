module jacobian_triplet
use cc_gparams
      use gparam
      use linalg
      use t1_transformed_int
      use math_constants
      use arithmetic

      use ccjac_block_11_triplet_dav
      use ccjac_block_12_tripletp_dav
      use ccjac_block_12_tripletm_dav
      use ccjac_block_13_triplet_dav
      use ccjac_block_21_tripletp_dav
      use ccjac_block_21_tripletm_dav
      use ccjac_block_21_cc3_tripletp_dav
      use ccjac_block_21_cc3_tripletm_dav
      use ccjac_block_22_tripletpp_dav
      use ccjac_block_22_tripletpm_dav
      use ccjac_block_22_tripletmp_dav
      use ccjac_block_22_tripletmm_dav
      use ccjac_block_23_tripletp_dav
      use ccjac_block_23_tripletm_dav
      use ccjac_block_31_triplet_dav
      use ccjac_block_32_tripletp_dav
      use ccjac_block_32_tripletm_dav
      use ccjac_block_33_triplet_dav

      use symmetry

      use cc3_intermediates_for_21
      use davidson_main
      use threads

      implicit none
      save

      integer, parameter :: cc_plus = 1
      integer, parameter :: cc_minus = -1


contains


      subroutine ccjac_11_dav_driver_triplet(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1,  &
            bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx

            integer                            :: pa, pb, pi, pj
            integer, dimension(:), allocatable :: n0a, n1a, n0b, n1b
            integer, dimension(:), allocatable :: n0i, n1i, n0j, n1j

            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            integer :: m0a, m1a, m0b, m1b
            integer :: m0i, m1i, m0j, m1j
            integer :: bj, kj
            integer :: idims
            integer :: nocc
            integer, dimension(:, :), allocatable :: isingles

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            nocc = nocc1 - nocc0 + 1

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))


            ! !**************************************************************************************

            ! call ccjac_11_triplet_dav(sigup_diag, sigup_nondiag, &
            !       t2, nocc0, nocc1, nvirt0, nvirt1, &
            !       nvirt0, nvirt1, nvirt0, nvirt1, nocc0, nocc1, nocc0, nocc1, 1, 1)
            
            print*, 'jac_triplet'
            call irrep_singless(irrep_idx, irrep0, irrep1, &
                  isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(irrep_idx, irrep0, irrep1, &
                  isingles, POINT_GROUP, idims, .false.)

            if (dav_converging_right())then

                  do kj = 1, idims

                        call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)

                        ntasks = 0

                        do bj = 1, idims

                              call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                                    m0i, m1i, m0a, m1a)

                              do pa = m0a, m1a, bintv
                              do pi = m0i, m1i, binto

                                    ntasks = ntasks + 1

                                    n0b(ntasks) = m0b
                                    n1b(ntasks) = m1b
                                    n0j(ntasks) = m0j
                                    n1j(ntasks) = m1j

                                    call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                    call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))
                                    
                                    if (ntasks == max_ntasks)  then
                                          
                                                call dotasks_11_triplet(sigup_diag, sigup_nondiag, &
                                                      t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                      n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)
                                                ntasks = 0
                                          end if
                                    end do
                              end do
                        end do

                        if(ntasks .gt.0)then

                              call dotasks_11_triplet(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
                                    nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)             
                        end if
                  end do
            else

                  do bj = 1, idims
                        call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)

                        ntasks = 0

                        do kj = 1, idims
                              call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
                                    m0j, m1j, m0b, m1b)

                              do pb = m0b, m1b, kintv
                              do pj = m0j, m1j, kinto

                                    ntasks = ntasks + 1

                                    n0a(ntasks) = m0a
                                    n1a(ntasks) = m1a
                                    n0i(ntasks) = m0i
                                    n1i(ntasks) = m1i

                                    call loop_boundaries(pb, m1b, kintv, n0b(ntasks), n1b(ntasks))
                                    call loop_boundaries(pj, m1j, kinto, n0j(ntasks), n1j(ntasks))
                                    
                                    if (ntasks == max_ntasks)  then
                                          call dotasks_11_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)
                                          ntasks = 0
                                    end if
                              end do
                              end do
                        end do

                        if(ntasks .gt.0)then
                              call dotasks_11_triplet(sigup_diag, sigup_nondiag, t2, nocc0, &
                                    nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)             
                        end if
                  end do
            end if

            deallocate(isingles)
            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)

      end subroutine ccjac_11_dav_driver_triplet

      subroutine dotasks_11_triplet(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j
            integer, intent(in) :: ntasks
            integer :: k

            !$omp parallel private(k) default(shared)
            !$omp do 
            do k = 1, ntasks
                  call ccjac_11_triplet_dav(sigup_diag, sigup_nondiag, &
                        t2, nocc0, nocc1, nvirt0, nvirt1, &
                        n0a(k), n1a(k), n0b(k), n1b(k), n0i(k), n1i(k), n0j(k), n1j(k), 1, 1)
            end do
            !$omp end do
            !$omp end parallel

      end subroutine dotasks_11_triplet

      subroutine ccjac_12_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, rmfold)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair, nidx_plus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(in)                                             :: rmfold

            integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b, n0c, n1c
            integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j, n0k, n1k

            integer, dimension(:,:), allocatable :: idoubles
            integer, dimension(:,:), allocatable :: isingles
            integer :: idims, idimd
            integer :: m0a, m1a
            integer :: m0b, m1b, m0c, m1c
            integer :: m0i, m1i
            integer :: m0j, m1j, m0k, m1k
            integer :: bj, kj
            integer :: pa, pb, pc, pi, pj, pk
            integer :: nocc
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))

            nocc = nocc1 - nocc0 + 1

            print*, 'nidx_plus, npair', nidx_plus, npair

            ! if (rmfold == cc_plus) then
            !             call ccjac_12_tripletp_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, 1, npair + 1)
          
            ! else if (rmfold == cc_minus) then
            !             call ccjac_12_tripletm_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, 1, npair + nidx_plus + 1)
            ! end if


            call irrep_singless(irrep_idx, irrep0, irrep1, &
                  isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(irrep_idx, irrep0, irrep1, &
                  isingles, POINT_GROUP, idims, .false.)

            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true., rmfold)
            allocate(idoubles(4, idimd))
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true., rmfold)


            if (dav_converging_right())then

                  do kj = 1, idimd
                        call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)
                        call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)

                        ntasks = 0
                        do bj = 1, idims
                              call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                                    m0i, m1i, m0a, m1a)

                              do pa = m0a, m1a, bintv
                              do pi = m0i, m1i, binto

                                    ntasks = ntasks + 1
                                    
                                    n0b(ntasks) = m0b
                                    n1b(ntasks) = m1b
                                    n0j(ntasks) = m0j
                                    n1j(ntasks) = m1j
                                    n0c(ntasks) = m0c
                                    n1c(ntasks) = m1c
                                    n0k(ntasks) = m0k
                                    n1k(ntasks) = m1k

                                    call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                    call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                                    if (ntasks == max_ntasks)  then
                                          call dotasks_12_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0c, n1c, &
                                                n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks, rmfold, nidx_plus)
                                          ntasks = 0
                                    end if
                                    end do
                              end do
                        end do

                        if(ntasks .gt.0)then
                              call dotasks_12_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks, rmfold, nidx_plus)
                        end if
                  end do
            else

                  do bj = 1, idims
                        call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)

                        ntasks = 0

                        do kj = 1, idimd
                              call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                                    m0j, m1j, m0b, m1b)
                              call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
                                    m0k, m1k, m0c, m1c)

                              do pc = m0c, m1c, kintv
                              do pk = m0k, m1k, kinto
                              do pb = m0b, m1b, kintv
                              do pj = m0j, m1j, kinto
                                    ntasks = ntasks + 1
                                    
                                    n0a(ntasks) = m0a
                                    n1a(ntasks) = m1a
                                    n0i(ntasks) = m0i
                                    n1i(ntasks) = m1i

                                    call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                                    call loop_boundaries(pk, m1k, kinto, n0k(ntasks), n1k(ntasks))
                                    call loop_boundaries(pb, m1b, kintv, n0b(ntasks), n1b(ntasks))
                                    call loop_boundaries(pj, m1j, kinto, n0j(ntasks), n1j(ntasks))

                                    if (ntasks == max_ntasks)  then
                                          call dotasks_12_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0c, n1c, &
                                                n0i, n1i, n0j, n1j, n0k, n1k, npair, &
                                                ntasks, rmfold, nidx_plus)
                                          ntasks = 0
                                    end if
                              end do
                              end do
                              end do
                              end do
                        end do

                        if(ntasks .gt.0)then
                              call dotasks_12_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks, rmfold, nidx_plus)
                        end if
                  end do
            end if

            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)


      end subroutine ccjac_12_dav_driver_triplet

      subroutine dotasks_12_triplet(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks, rmfold, nidx_plus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in) :: npair
            integer, intent(in) :: rmfold
            integer, intent(in) :: nidx_plus

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k
            integer, intent(in) :: ntasks
            integer :: k

            if (rmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)                                                                          
                  !$omp do                                                                                               
                  do k = 1, ntasks
                        call ccjac_12_tripletp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), 1, npair + 1)
                  end do
                  !$omp end do                                                                                               
                  !$omp end parallel 
          
            else if (rmfold == cc_minus) then

                  !$omp parallel private(k) default(shared)                                                                          
                  !$omp do                                                                                               
                  do k = 1, ntasks
                        call ccjac_12_tripletm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), 1, npair + nidx_plus + 1)
                  end do
                  !$omp end do                                                                                               
                  !$omp end parallel 

            end if
                                                                                    

      end subroutine dotasks_12_triplet
      

      subroutine ccjac_21_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, lmfold, cc3)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair, nidx_plus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(in)                                             :: lmfold
            logical, optional, intent(in)                                   :: cc3
            

            integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b, n0c, n1c
            integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j, n0k, n1k

            integer :: pa, pb, pc, pi, pj, pk
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            integer, dimension(:,:), allocatable :: idoubles
            integer, dimension(:,:), allocatable :: isingles
            integer :: idims, idimd
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k
            integer :: nocc
            integer :: bj, kj

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))

            nocc = nocc1 - nocc0 + 1          



            ! if (lmfold == cc_plus) then
            !       if (present(cc3))then
            !             call ccjac_21_cc3_tripletp_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + 1, 1)
            !       else
            !             call ccjac_21_tripletp_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + 1, 1)
            !       end if
            ! else if (lmfold == cc_minus) then
            !       if (present(cc3)) then
            !             call ccjac_21_cc3_tripletm_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + nidx_plus + 1, 1)
            !       else
            !             call ccjac_21_tripletm_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + nidx_plus + 1, 1)
            !       end if
            ! end if


            
            call irrep_singless(irrep_idx, irrep0, irrep1, &
                  isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(irrep_idx, irrep0, irrep1, &
                  isingles, POINT_GROUP, idims, .false.)


            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true., lmfold)
            allocate(idoubles(4, idimd))

            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true., lmfold)

            if (dav_converging_right())then

                  do kj = 1, idims
                        call loop_boundaries_sp(isingles(:, kj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)

                        ntasks = 0

                        do bj = 1, idimd
                              call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                                    m0i, m1i, m0a, m1a)
                              call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                                    m0j, m1j, m0b, m1b)


                              do pb = m0b, m1b, bintv
                              do pj = m0j, m1j, binto
                              do pa = m0a, m1a, bintv
                              do pi = m0i, m1i, binto

                                    ntasks = ntasks + 1

                                    n0c(ntasks) = m0c
                                    n1c(ntasks) = m1c
                                    n0k(ntasks) = m0k
                                    n1k(ntasks) = m1k
                                    
                                    call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                                    call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                                    call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                    call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))


                                    if (ntasks == max_ntasks)  then
                                          if (present(cc3)) then

                                                call dotasks_21_triplet_cc3(sigup_diag, sigup_nondiag, &
                                                      t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                      n0a, n1a, n0b, n1b, n0c, n1c, &
                                                      n0i, n1i, n0j, n1j, n0k, n1k, &
                                                      npair, ntasks, lmfold, nidx_plus)

                                                ntasks = 0
                                          else
                                                call dotasks_21_triplet(sigup_diag, sigup_nondiag, &
                                                      t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                      n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                                      n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)
                                                ntasks = 0
                                          end if
                                    end if
                              end do
                              end do
                              end do
                              end do
                   end do

                        if(ntasks .gt.0)then
                              if (present(cc3)) then

                                    call dotasks_21_triplet_cc3(sigup_diag, sigup_nondiag, &
                                          t2, nocc0, nocc1, nvirt0, nvirt1, &
                                          n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                          n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)

                              else
                                    call dotasks_21_triplet(sigup_diag, sigup_nondiag, &
                                          t2, nocc0, nocc1, nvirt0, nvirt1, &
                                          n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                          n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)
                              end if
                        end if
                  end do
            else

                  do bj = 1, idimd
                        call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)

                        ntasks = 0

                        do kj = 1, idims
                              call loop_boundaries_sp(isingles(1:2, kj), irrep0, irrep1, &
                                    m0k, m1k, m0c, m1c)

                              do pc = m0c, m1c, kintv
                                    do pk = m0k, m1k, kinto

                                          ntasks = ntasks + 1

                                          n0a(ntasks) = m0a
                                          n1a(ntasks) = m1a
                                          n0b(ntasks) = m0b
                                          n1b(ntasks) = m1b
                                          n0i(ntasks) = m0i
                                          n1i(ntasks) = m1i
                                          n0j(ntasks) = m0j
                                          n1j(ntasks) = m1j

                                          call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                                          call loop_boundaries(pk, m1k,  kinto, n0k(ntasks), n1k(ntasks))

                                          if (ntasks == max_ntasks)  then
                                                if (present(cc3)) then

                                                      call dotasks_21_triplet_cc3(sigup_diag, sigup_nondiag, &
                                                            t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                            n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                                            n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)
                                                      ntasks = 0
                                                else
                                                      call dotasks_21_triplet(sigup_diag, sigup_nondiag, &
                                                            t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                            n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                                            n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)
                                                      ntasks = 0
                                                end if
                                          end if
                                    end do
                              end do
                        end do
                        if(ntasks .gt.0)then
                              if (present(cc3)) then

                                    call dotasks_21_triplet_cc3(sigup_diag, sigup_nondiag, &
                                          t2, nocc0, nocc1, nvirt0, nvirt1, &
                                          n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                          n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)
                              else
                                    call dotasks_21_triplet(sigup_diag, sigup_nondiag, &
                                          t2, nocc0, nocc1, nvirt0, nvirt1, &
                                          n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, &
                                          n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)
                              end if
                        end if
                  end do
            end if

            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)


      end subroutine ccjac_21_dav_driver_triplet

      subroutine dotasks_21_triplet(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: lmfold, nidx_plus

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k
            integer, intent(in) :: ntasks
            integer :: k

            if (lmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)               
                  !$omp do                                                                                        
                  do k = 1, ntasks
                        call ccjac_21_tripletp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + 1, 1)
                  end do
                  !$omp end do
                  !$omp end parallel

            else if (lmfold == cc_minus) then

                  !$omp parallel private(k) default(shared)               
                  !$omp do                                                                                        
                  do k = 1, ntasks
                        call ccjac_21_tripletm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + nidx_plus + 1, 1)
                  end do
                  !$omp end do
                  !$omp end parallel

            end if

      end subroutine dotasks_21_triplet


      subroutine dotasks_21_triplet_cc3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks, lmfold, nidx_plus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: lmfold, nidx_plus

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k
            integer, intent(in) :: ntasks
            integer :: k

            if (lmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)               
                  !$omp do                                                                                        
                  do k = 1, ntasks
                        call ccjac_21_cc3_tripletp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + 1, 1)
                  end do
                  !$omp end do
                  !$omp end parallel

            else if (lmfold == cc_minus) then


                  !$omp parallel private(k) default(shared)               
                  !$omp do                                                                                        
                  do k = 1, ntasks
                        call ccjac_21_cc3_tripletm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + nidx_plus + 1, 1)
                  end do
                  !$omp end do
                  !$omp end parallel

            end if

      end subroutine dotasks_21_triplet_cc3


      subroutine ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, lmfold, rmfold)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair, nidx_plus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(in)                                             :: lmfold, rmfold

            integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l

            integer        :: pa, pb, pc, pd, pi, pj, pk, pl
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            integer, dimension(:,:), allocatable :: idoubles_left
            integer, dimension(:,:), allocatable :: idoubles_right
            integer :: idim_left, idim_right
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c, m0d, m1d
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k, m0l, m1l
            integer :: nocc
            integer :: bj, kj

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0d(max_ntasks))
            allocate(n1d(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))
            allocate(n0l(max_ntasks))
            allocate(n1l(max_ntasks))

            nocc = nocc1 - nocc0 + 1   


           ! if (lmfold == cc_plus .and. rmfold == cc_plus) then

           !              call ccjac_22_tripletpp_dav(sigup_diag, sigup_nondiag, &
           !                    t2, nocc0, nocc1, nvirt0, nvirt1, &
           !                    nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,&
           !                    nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
           !                    npair + 1, npair + 1)
     

           !  else if (lmfold == cc_plus .and. rmfold == cc_minus) then

           !              call ccjac_22_tripletpm_dav(sigup_diag, sigup_nondiag, &
           !                    t2, nocc0, nocc1, nvirt0, nvirt1, &
           !                    nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,&
           !                    nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
           !                    npair + 1, npair + nidx_plus + 1)
   

           !  else if (lmfold == cc_minus .and. rmfold == cc_plus) then

           !              call ccjac_22_tripletmp_dav(sigup_diag, sigup_nondiag, &
           !                    t2, nocc0, nocc1, nvirt0, nvirt1, &
           !                    nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,&
           !                    nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
           !                    npair + nidx_plus + 1, npair + 1)
     

           !  else if (lmfold == cc_minus .and. rmfold == cc_minus) then


           !              call ccjac_22_tripletmm_dav(sigup_diag, sigup_nondiag, &
           !                    t2, nocc0, nocc1, nvirt0, nvirt1, &
           !                    nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,&
           !                    nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
           !                    npair + nidx_plus + 1, npair + nidx_plus + 1)
           !  end if  
            

            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles_left, POINT_GROUP, idim_left, .true., .true., lmfold)
            allocate(idoubles_left(4, idim_left))
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles_left, POINT_GROUP, idim_left, .false., .true., lmfold)


            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles_right, POINT_GROUP, idim_right, .true., .true., rmfold)
            allocate(idoubles_right(4, idim_right))
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles_right, POINT_GROUP, idim_right, .false., .true., rmfold)

            if (dav_converging_right())then

                  do kj = 1, idim_right

                        call loop_boundaries_sp(idoubles_right(1:2, kj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)
                        call loop_boundaries_sp(idoubles_right(3:4, kj), irrep0, irrep1, &
                              m0l, m1l, m0d, m1d)

                        ntasks = 0
                        do bj = 1, idim_left

                              call loop_boundaries_sp(idoubles_left(1:2, bj), irrep0, irrep1, &
                                    m0i, m1i, m0a, m1a)
                              call loop_boundaries_sp(idoubles_left(3:4, bj), irrep0, irrep1, &
                                    m0j, m1j, m0b, m1b)

                              do pb = m0b, m1b, bintv
                                    do pj = m0j, m1j, binto
                                          do pa = m0a, m1a, bintv
                                                do pi = m0i, m1i, binto

                                                      ntasks = ntasks + 1

                                                      n0c(ntasks) = m0c
                                                      n1c(ntasks) = m1c
                                                      n0d(ntasks) = m0d
                                                      n1d(ntasks) = m1d
                                                      n0k(ntasks) = m0k
                                                      n1k(ntasks) = m1k
                                                      n0l(ntasks) = m0l
                                                      n1l(ntasks) = m1l

                                                      call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                                                      call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                                                      call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                                      call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                                                      if (ntasks == max_ntasks)  then

                                                            call dotasks_22_triplet(sigup_diag, sigup_nondiag, &
                                                                  t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                                                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, &
                                                                  ntasks, lmfold, rmfold, nidx_plus)
                                                            ntasks = 0
                                                      end if
                                                end do
                                          end do
                                    end do
                              end do
                        end do

                        if(ntasks .gt. 0)then

                              call dotasks_22_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, &
                                    ntasks, lmfold, rmfold, nidx_plus)                 
                        end if
                  end do
            else

                  do bj = 1, idim_left
                        call loop_boundaries_sp(idoubles_left(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(idoubles_left(3:4, bj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)

                        ntasks = 0

                        do kj = 1, idim_right
                              call loop_boundaries_sp(idoubles_right(1:2, kj), irrep0, irrep1, &
                                    m0k, m1k, m0c, m1c)
                              call loop_boundaries_sp(idoubles_right(3:4, kj), irrep0, irrep1, &
                                    m0l, m1l, m0d, m1d)

                              do pd = m0d, m1d, kintv
                                    do pl = m0l, m1l, kinto
                                          do pc = m0c, m1c, kintv
                                                do pk = m0k, m1k, kinto

                                                      ntasks = ntasks + 1

                                                      n0a(ntasks) = m0a
                                                      n1a(ntasks) = m1a
                                                      n0b(ntasks) = m0b
                                                      n1b(ntasks) = m1b
                                                      n0i(ntasks) = m0i
                                                      n1i(ntasks) = m1i
                                                      n0j(ntasks) = m0j
                                                      n1j(ntasks) = m1j

                                                      call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                                                      call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))
                                                      call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                                                      call loop_boundaries(pk, m1k, kinto, n0k(ntasks), n1k(ntasks))

                                                      if (ntasks == max_ntasks)  then
                                                            call dotasks_22_triplet(sigup_diag, sigup_nondiag, &
                                                                  t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                                                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                                                  npair, ntasks, lmfold, rmfold, nidx_plus)
                                                            ntasks = 0
                                                      end if
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                        if(ntasks .gt. 0)then

                              call dotasks_22_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, &
                                    ntasks, lmfold, rmfold, nidx_plus)                 
                        end if
                  end do
            end if

            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0d)
            deallocate(n1d)
            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)
            deallocate(n0l)
            deallocate(n1l)


      end subroutine ccjac_22_dav_driver_triplet

      subroutine dotasks_22_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, &
            n1j, n0k, n1k, n0l, n1l, npair, ntasks, lmfold, rmfold, nidx_plus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
            integer, intent(in) :: ntasks
            integer, intent(in)                                             :: lmfold, rmfold
            integer, intent(in)                                             :: nidx_plus
            integer :: k

            if (lmfold == cc_plus .and. rmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_22_tripletpp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
                              npair + 1, npair + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            else if (lmfold == cc_plus .and. rmfold == cc_minus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_22_tripletpm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
                              npair + 1, npair + nidx_plus + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            else if (lmfold == cc_minus .and. rmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_22_tripletmp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
                              npair + nidx_plus + 1, npair + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            else if (lmfold == cc_minus .and. rmfold == cc_minus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_22_tripletmm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
                              npair + nidx_plus + 1, npair + nidx_plus + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            end if

      end subroutine dotasks_22_triplet

!-----------------------------------------------------------------------------------------------------------------------------------

      subroutine ccjac_13_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, nidx_minus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx

            integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l

            integer :: pa, pb, pc, pd, pi, pj, pk, pl
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            integer, dimension(:,:), allocatable :: isingles
            integer, dimension(:,:), allocatable :: itriples
            integer :: idims, idimt
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c, m0d, m1d
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k, m0l, m1l
            integer :: nocc
            integer :: bj, kj

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0d(max_ntasks))
            allocate(n1d(max_ntasks))

            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))
            allocate(n0l(max_ntasks))
            allocate(n1l(max_ntasks))

            nocc = nocc1 - nocc0 + 1            

            ! call ccjac_13_triplet_dav(sigup_diag, sigup_nondiag, &
            !       t2, nocc0, nocc1, nvirt0, nvirt1, &
            !       nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !       nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
            !       1, npair + nidx_plus + nidx_minus + 1)


           call irrep_singless(irrep_idx, irrep0, irrep1, &
                 isingles, POINT_GROUP, idims, .true.)
           allocate(isingles(2, idims))
           call irrep_singless(irrep_idx, irrep0, irrep1, &
                 isingles, POINT_GROUP, idims, .false.)


           call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                 itriples, POINT_GROUP, idimt, .true.)
            allocate(itriples(6, idimt))
            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                  itriples, POINT_GROUP, idimt, .false.)

            if (dav_converging_right())then

                  do kj = 1, idimt

                        call loop_boundaries_sp(itriples(1:2, kj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)
                        call loop_boundaries_sp(itriples(3:4, kj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)
                        call loop_boundaries_sp(itriples(5:6, kj), irrep0, irrep1, &
                              m0l, m1l, m0d, m1d)

                        ntasks = 0
                        do bj = 1, idims

                              call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                                    m0i, m1i, m0a, m1a)

                              do pa = m0a, m1a, bintv
                              do pi = m0i, m1i, binto

                                    ntasks = ntasks + 1
                                    
                                    n0b(ntasks) = m0b
                                    n1b(ntasks) = m1b
                                    n0c(ntasks) = m0c
                                    n1c(ntasks) = m1c
                                    n0d(ntasks) = m0d
                                    n1d(ntasks) = m1d

                                    n0j(ntasks) = m0j
                                    n1j(ntasks) = m1j
                                    n0k(ntasks) = m0k
                                    n1k(ntasks) = m1k
                                    n0l(ntasks) = m0l
                                    n1l(ntasks) = m1l

                                    call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                    call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))
                                    
                                    if (ntasks == max_ntasks)  then
                                          
                                          call dotasks_13_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                                n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, &
                                                ntasks, nidx_plus, nidx_minus)
                                          ntasks = 0
                                    end if
                               end do
                               end do
                        end do

                        if(ntasks .gt. 0)then

                              call dotasks_13_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, &
                                    ntasks, nidx_plus, nidx_minus)                 
                        end if
                  end do
            else

                  do bj = 1, idims
                        call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)

                        ntasks = 0

                        do kj = 1, idimt
                              call loop_boundaries_sp(itriples(1:2, kj), irrep0, irrep1, &
                                    m0j, m1j, m0b, m1b)
                              call loop_boundaries_sp(itriples(3:4, kj), irrep0, irrep1, &
                                    m0k, m1k, m0c, m1c)
                              call loop_boundaries_sp(itriples(5:6, kj), irrep0, irrep1, &
                                    m0l, m1l, m0d, m1d)

                              do pd = m0d, m1d, kintv
                              do pl = m0l, m1l, kinto
                              do pc = m0c, m1c, kintv
                              do pk = m0k, m1k, kinto
                              do pb = m0b, m1b, kintv
                              do pj = m0j, m1j, kinto

                                    ntasks = ntasks + 1
                                    
                                    n0a(ntasks) = m0a
                                    n1a(ntasks) = m1a
                                    n0i(ntasks) = m0i
                                    n1i(ntasks) = m1i

                                    call loop_boundaries(pb, m1b, kintv, n0b(ntasks), n1b(ntasks))
                                    call loop_boundaries(pj, m1j, kinto, n0j(ntasks), n1j(ntasks))                                   
                                    call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                                    call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))
                                    call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                                    call loop_boundaries(pk, m1k, kinto, n0k(ntasks), n1k(ntasks))

                                    if (ntasks == max_ntasks)  then

                                          call dotasks_13_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                                n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                                npair, ntasks, nidx_plus, nidx_minus)
                                          ntasks = 0
                                    end if
                              end do
                              end do
                              end do
                              end do
                              end do
                              end do
                        end do
                        if(ntasks .gt. 0)then
                              
                              call dotasks_13_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, &
                                    ntasks, nidx_plus, nidx_minus)                 
                        end if
                  end do
            end if

            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0d)
            deallocate(n1d)

            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)
            deallocate(n0l)
            deallocate(n1l)

      end subroutine ccjac_13_dav_driver_triplet

      subroutine dotasks_13_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
            n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                  npair, ntasks, nidx_plus, nidx_minus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
            integer, intent(in) :: ntasks
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer :: k

            
            !$omp parallel private(k) default(shared)    
            !$omp do                                                        
            do k = 1, ntasks
                  call ccjac_13_triplet_dav(sigup_diag, sigup_nondiag, &
                        t2, nocc0, nocc1, nvirt0, nvirt1, &
                        n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), &
                        n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
                        1, npair + nidx_plus + nidx_minus + 1)
            end do
            !$omp end do       
            !$omp end parallel

      end subroutine dotasks_13_triplet


      subroutine ccjac_23_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, nidx_minus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, lmfold)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(in)                                             :: lmfold

            integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
            integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m

            integer :: pa, pb, pc, pd, pe, pi, pj, pk, pl, pm
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            integer, dimension(:,:), allocatable :: idoubles
            integer, dimension(:,:), allocatable :: itriples
            integer :: idimd, idimt
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c, m0d, m1d
            integer :: m0e, m1e
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k, m0l, m1l
            integer :: m0m, m1m
            integer :: nocc
            integer :: bj, kj

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0d(max_ntasks))
            allocate(n1d(max_ntasks))
            allocate(n0e(max_ntasks))
            allocate(n1e(max_ntasks))

            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))
            allocate(n0l(max_ntasks))
            allocate(n1l(max_ntasks))
            allocate(n0m(max_ntasks))
            allocate(n1m(max_ntasks))

            nocc = nocc1 - nocc0 + 1            

            ! if (lmfold == cc_plus) then

            !             call ccjac_23_tripletp_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
            !                   npair + 1, npair + nidx_plus + nidx_minus + 1)

            ! else if (lmfold == cc_minus) then

            !             call ccjac_23_tripletm_dav(sigup_diag, sigup_nondiag, &
            !                   t2, nocc0, nocc1, nvirt0, nvirt1, &
            !                   nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,nvirt0, nvirt1, &
            !                   nocc0, nocc1, nocc0, nocc1, nocc0, nocc1,nocc0, nocc1, nocc0, nocc1, &
            !                   npair + nidx_plus + 1, npair + nidx_plus + nidx_minus + 1)

            ! end if


            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true., lmfold)
            allocate(idoubles(4, idimd))
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true., lmfold)

            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                  itriples, POINT_GROUP, idimt, .true.)
            allocate(itriples(6, idimt))
            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                  itriples, POINT_GROUP, idimt, .false.)

            if (dav_converging_right())then

                  do kj = 1, idimt

                        call loop_boundaries_sp(itriples(1:2, kj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)
                        call loop_boundaries_sp(itriples(3:4, kj), irrep0, irrep1, &
                              m0l, m1l, m0d, m1d)
                        call loop_boundaries_sp(itriples(5:6, kj), irrep0, irrep1, &
                              m0m, m1m, m0e, m1e)

                        ntasks = 0
                        do bj = 1, idimd

                              call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                                    m0i, m1i, m0a, m1a)
                              call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                                    m0j, m1j, m0b, m1b)

                              do pb = m0b, m1b, bintv
                              do pj = m0j, m1j, binto
                              do pa = m0a, m1a, bintv
                              do pi = m0i, m1i, binto

                                    ntasks = ntasks + 1
                                    
                                    n0e(ntasks) = m0e
                                    n1e(ntasks) = m1e
                                    n0m(ntasks) = m0m
                                    n1m(ntasks) = m1m
                                    
                                    n0c(ntasks) = m0c
                                    n1c(ntasks) = m1c
                                    n0k(ntasks) = m0k
                                    n1k(ntasks) = m1k

                                    n0d(ntasks) = m0d
                                    n1d(ntasks) = m1d
                                    n0l(ntasks) = m0l
                                    n1l(ntasks) = m1l

                                    call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                                    call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                                    call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                    call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))
                                    
                                    if (ntasks == max_ntasks)  then
                                          
                                          call dotasks_23_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                                n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                                npair, ntasks, lmfold, nidx_plus, nidx_minus)

                                          ntasks = 0
                                    end if
                               end do
                               end do
                               end do
                               end do
                        end do

                        if(ntasks .gt. 0)then

                              call dotasks_23_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                    npair, ntasks, lmfold, nidx_plus, nidx_minus)

                        end if
                  end do
            else

                  do bj = 1, idimd
                        call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)

                        ntasks = 0

                        do kj = 1, idimt
                              call loop_boundaries_sp(itriples(1:2, kj), irrep0, irrep1, &
                                    m0k, m1k, m0c, m1c)
                              call loop_boundaries_sp(itriples(3:4, kj), irrep0, irrep1, &
                                    m0l, m1l, m0d, m1d)
                              call loop_boundaries_sp(itriples(5:6, kj), irrep0, irrep1, &
                                    m0m, m1m, m0e, m1e)

                              do pe = m0e, m1e, kintv
                              do pm = m0m, m1m, kinto
                              do pd = m0d, m1d, kintv
                              do pl = m0l, m1l, kinto
                              do pc = m0c, m1c, kintv
                              do pk = m0k, m1k, kinto

                                    ntasks = ntasks + 1
                                    
                                    n0a(ntasks) = m0a
                                    n1a(ntasks) = m1a
                                    n0b(ntasks) = m0b
                                    n1b(ntasks) = m1b

                                    n0i(ntasks) = m0i
                                    n1i(ntasks) = m1i
                                    n0j(ntasks) = m0j
                                    n1j(ntasks) = m1j

                                    call loop_boundaries(pe, m1e, kintv, n0e(ntasks), n1e(ntasks))
                                    call loop_boundaries(pm, m1m, kinto, n0m(ntasks), n1m(ntasks))                                   
                                    call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                                    call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))
                                    call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                                    call loop_boundaries(pk, m1k, kinto, n0k(ntasks), n1k(ntasks))

                                    if (ntasks == max_ntasks)  then

                                          call dotasks_23_triplet(sigup_diag, sigup_nondiag, &
                                                t2, nocc0, nocc1, nvirt0, nvirt1, &
                                                n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                                n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                                npair, ntasks, lmfold, nidx_plus, nidx_minus)

                                          ntasks = 0
                                    end if
                              end do
                              end do
                              end do
                              end do
                              end do
                              end do
                        end do
                        if(ntasks .gt. 0)then
                              
                              call dotasks_23_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                    npair, ntasks, lmfold, nidx_plus, nidx_minus)

                        end if
                  end do
            end if

            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0d)
            deallocate(n1d)
            deallocate(n0e)
            deallocate(n1e)

            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)
            deallocate(n0l)
            deallocate(n1l)
            deallocate(n0m)
            deallocate(n1m)


      end subroutine ccjac_23_dav_driver_triplet

      subroutine dotasks_23_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e,&
            n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                  npair, ntasks, lmfold, nidx_plus, nidx_minus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair

            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
            integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m
            integer, intent(in) :: ntasks
            integer, intent(in)                                             :: lmfold
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer :: k

            if (lmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_23_tripletp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), n0m(k), n1m(k), &
                              npair + 1, npair + nidx_plus + nidx_minus + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            else if (lmfold == cc_minus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_23_tripletm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), n0m(k), n1m(k), &
                              npair + nidx_plus + 1, npair + nidx_plus + nidx_minus + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            end if

      end subroutine dotasks_23_triplet

      subroutine ccjac_31_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, nidx_minus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            
            integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
            integer                                 :: pa, pb, pc, pd, pi, pj, pk, pl

            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks
            integer, dimension(:,:), allocatable :: isingles
            integer, dimension(:,:), allocatable :: itriples
            integer :: idims, idimt
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c, m0d, m1d
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k, m0l, m1l
            integer :: nocc
            integer :: bj, kj

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0d(max_ntasks))
            allocate(n1d(max_ntasks))

            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))
            allocate(n0l(max_ntasks))
            allocate(n1l(max_ntasks))

           nocc = nocc1 - nocc0 + 1

           ! call ccjac_31_triplet_dav(sigup_diag, sigup_nondiag, &
           !       t2, nocc0, nocc1, nvirt0, nvirt1, &
           !       nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,&
           !       nocc0, nocc1, nocc0,nocc1, nocc0,nocc1, nocc0,nocc1,&
           !       npair + nidx_plus + nidx_minus + 1, 1)


           call irrep_singless(irrep_idx, irrep0, irrep1, &
                 isingles, POINT_GROUP, idims, .true.)
           allocate(isingles(2, idims))
           call irrep_singless(irrep_idx, irrep0, irrep1, &
                 isingles, POINT_GROUP, idims, .false.)

            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                  itriples, POINT_GROUP, idimt, .true.)
            allocate(itriples(6, idimt))
            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                  itriples, POINT_GROUP, idimt, .false.)

           if (dav_converging_right())then

                 do kj = 1, idims
                       call loop_boundaries_sp(isingles(1:2, kj), irrep0, irrep1, &
                             m0l, m1l, m0d, m1d)

                       ntasks = 0

                       do bj = 1, idimt
                             call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
                                   m0i, m1i, m0a, m1a)
                             call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
                                   m0j, m1j, m0b, m1b)
                             call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
                                   m0k, m1k, m0c, m1c)

                             do pc = m0c, m1c, bintv
                             do pk = m0k, m1k, binto
                             do pb = m0b, m1b, bintv
                             do pj = m0j, m1j, binto
                             do pa = m0a, m1a, bintv
                             do pi = m0i, m1i, binto

                                   ntasks = ntasks + 1

                                   n0d(ntasks) = m0d
                                   n1d(ntasks) = m1d
                                   n0l(ntasks) = m0l
                                   n1l(ntasks) = m1l
                                   
                                   call loop_boundaries(pc, m1c, bintv, n0c(ntasks), n1c(ntasks))
                                   call loop_boundaries(pk, m1k, binto, n0k(ntasks), n1k(ntasks))
                                   call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                                   call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                                   call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                   call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                                   if (ntasks == max_ntasks)  then
                                         
                                         call dotasks_31_triplet(sigup_diag, sigup_nondiag, &
                                               t2, nocc0, nocc1, nvirt0, nvirt1, &
                                               n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                               n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                               npair, ntasks, nidx_plus, nidx_minus)

                                         ntasks = 0
                                   end if
                              end do
                              end do
                              end do
                              end do
                              end do
                              end do
                        end do
                        if(ntasks .gt. 0)then
                              call dotasks_31_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                    npair, ntasks, nidx_plus, nidx_minus)
                              ntasks = 0
                        end if
                  end do
            else
                  
                  do bj = 1, idimt
                        call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)
                        call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)

                        ntasks = 0

                        do kj = 1, idims
                              call loop_boundaries_sp(isingles(1:2, kj), irrep0, irrep1, &
                                    m0l, m1l, m0d, m1d)

                             do pl = m0l, m1l, kinto
                             do pd = m0d, m1d, kintv

                                   ntasks = ntasks + 1

                                   n0a(ntasks) = m0a
                                   n1a(ntasks) = m1a
                                   n0b(ntasks) = m0b
                                   n1b(ntasks) = m1b
                                   n0c(ntasks) = m0c
                                   n1c(ntasks) = m1c

                                   n0i(ntasks) = m0i
                                   n1i(ntasks) = m1i
                                   n0j(ntasks) = m0j
                                   n1j(ntasks) = m1j
                                   n0k(ntasks) = m0k
                                   n1k(ntasks) = m1k

                                   call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                                   call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))

                                   if (ntasks == max_ntasks)  then
                                         
                                         call dotasks_31_triplet(sigup_diag, sigup_nondiag, &
                                               t2, nocc0, nocc1, nvirt0, nvirt1, &
                                               n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                               n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                               npair, ntasks, nidx_plus, nidx_minus)
                                         ntasks = 0
                                   end if
                              end do
                              end do
                        end do
                        if(ntasks .gt. 0)then

                              call dotasks_31_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                    npair, ntasks, nidx_plus, nidx_minus)
                              ntasks = 0
                        end if
                  end do
            end if
            
            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0d)
            deallocate(n1d)

            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)
            deallocate(n0l)
            deallocate(n1l)
            
      end subroutine ccjac_31_dav_driver_triplet

      subroutine dotasks_31_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
            n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
            npair, ntasks, nidx_plus, nidx_minus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: nidx_plus, nidx_minus


            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
            integer, intent(in) :: ntasks
            integer :: k


            !$omp parallel private(k) default(shared)    
            !$omp do                                                        
            do k = 1, ntasks
                  call ccjac_31_triplet_dav(sigup_diag, sigup_nondiag, &
                        t2, nocc0, nocc1, nvirt0, nvirt1, &
                        n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), &
                        n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
                        npair + nidx_plus + nidx_minus + 1, 1)
            end do
            !$omp end do       
            !$omp end parallel 

      end subroutine dotasks_31_triplet


      subroutine ccjac_32_dav_driver_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
            nidx_plus, nidx_minus, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, rmfold)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer, intent(in)                                             :: bintv, binto
            integer, intent(in)                                             :: kintv, kinto 
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, intent(in)                                             :: rmfold
            
            integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
            integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m
            integer                                 :: pa, pb, pc, pd, pe, pi, pj, pk, pl, pm

            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks
            integer, dimension(:,:), allocatable :: idoubles
            integer, dimension(:,:), allocatable :: itriples
            integer :: idimd, idimt
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c, m0d, m1d, m0e, m1e
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k, m0l, m1l, m0m, m1m
            integer :: nocc
            integer :: bj, kj

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0c(max_ntasks))
            allocate(n1c(max_ntasks))
            allocate(n0d(max_ntasks))
            allocate(n1d(max_ntasks))
            allocate(n0e(max_ntasks))
            allocate(n1e(max_ntasks))

            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))
            allocate(n0k(max_ntasks))
            allocate(n1k(max_ntasks))
            allocate(n0l(max_ntasks))
            allocate(n1l(max_ntasks))
            allocate(n0m(max_ntasks))
            allocate(n1m(max_ntasks))

           nocc = nocc1 - nocc0 + 1

           ! call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, &
           !       nvirt0, idoubles, POINT_GROUP, idimd, .true., .true.)
           ! allocate(idoubles(4, idimd))
           ! call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, &
           !       nvirt0, idoubles, POINT_GROUP, idimd, .false., .true.)


            ! if (rmfold == cc_plus) then
            !       call ccjac_32_tripletp_dav(sigup_diag, sigup_nondiag, &
            !             t2, nocc0, nocc1, nvirt0, nvirt1, &
            !             nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !             nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
            !             npair + nidx_plus + nidx_minus + 1, npair + 1)
            ! else if (rmfold == cc_minus) then
            !       call ccjac_32_tripletm_dav(sigup_diag, sigup_nondiag, &
            !             t2, nocc0, nocc1, nvirt0, nvirt1, &
            !             nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1,&
            !             nocc0, nocc1, nocc0, nocc1, nocc0, nocc1,nocc0, nocc1, nocc0, nocc1, &
            !       npair + nidx_plus + nidx_minus + 1, npair + nidx_plus + 1)
            ! end if

            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, &
                  nvirt0, idoubles, POINT_GROUP, idimd, .true., .true., rmfold)
            allocate(idoubles(4, idimd))
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, &
                  nvirt0, idoubles, POINT_GROUP, idimd, .false., .true., rmfold)

           call irrep_triples_triplet(irrep_idx, irrep0, irrep1,  &
                 itriples, POINT_GROUP, idimt, .true.)
           allocate(itriples(6, idimt))
           call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                 itriples, POINT_GROUP, idimt, .false.)

           if (dav_converging_right())then

                 do kj = 1, idimd
                       call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                             m0l, m1l, m0d, m1d)
                       call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
                             m0m, m1m, m0e, m1e)

                       ntasks = 0

                       do bj = 1, idimt
                             call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
                                   m0i, m1i, m0a, m1a)
                             call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
                                   m0j, m1j, m0b, m1b)
                             call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
                                   m0k, m1k, m0c, m1c)

                             do pc = m0c, m1c, bintv
                             do pk = m0k, m1k, binto
                             do pb = m0b, m1b, bintv
                             do pj = m0j, m1j, binto
                             do pa = m0a, m1a, bintv
                             do pi = m0i, m1i, binto

                                   ntasks = ntasks + 1

                                   n0e(ntasks) = m0e
                                   n1e(ntasks) = m1e
                                   n0m(ntasks) = m0m
                                   n1m(ntasks) = m1m

                                   n0d(ntasks) = m0d
                                   n1d(ntasks) = m1d
                                   n0l(ntasks) = m0l
                                   n1l(ntasks) = m1l
                                   
                                   call loop_boundaries(pc, m1c, bintv, n0c(ntasks), n1c(ntasks))
                                   call loop_boundaries(pk, m1k, binto, n0k(ntasks), n1k(ntasks))
                                   call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                                   call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                                   call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                                   call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                                   if (ntasks == max_ntasks)  then
                                         
                                         call dotasks_32_triplet(sigup_diag, sigup_nondiag, &
                                               t2, nocc0, nocc1, nvirt0, nvirt1, &
                                               n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                               n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                               npair, ntasks, rmfold, nidx_plus, nidx_minus)
                                         ntasks = 0
                                   end if
                              end do
                              end do
                              end do
                              end do
                              end do
                              end do
                        end do
                        if(ntasks .gt. 0)then
                              call dotasks_32_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                    npair, ntasks, rmfold, nidx_plus, nidx_minus)
                              ntasks = 0
                        end if
                  end do
            else
                  
                  do bj = 1, idimt
                        call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
                              m0j, m1j, m0b, m1b)
                        call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
                              m0k, m1k, m0c, m1c)

                        ntasks = 0

                        do kj = 1, idimd
                              call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                                    m0l, m1l, m0d, m1d)
                              call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
                                    m0m, m1m, m0e, m1e)

                             do pm = m0m, m1m, kinto
                             do pe = m0e, m1e, kintv
                             do pl = m0l, m1l, kinto
                             do pd = m0d, m1d, kintv

                                   ntasks = ntasks + 1

                                   n0a(ntasks) = m0a
                                   n1a(ntasks) = m1a
                                   n0b(ntasks) = m0b
                                   n1b(ntasks) = m1b
                                   n0c(ntasks) = m0c
                                   n1c(ntasks) = m1c

                                   n0i(ntasks) = m0i
                                   n1i(ntasks) = m1i
                                   n0j(ntasks) = m0j
                                   n1j(ntasks) = m1j
                                   n0k(ntasks) = m0k
                                   n1k(ntasks) = m1k

                                   call loop_boundaries(pe, m1e, kintv, n0e(ntasks), n1e(ntasks))
                                   call loop_boundaries(pm, m1m, kinto, n0m(ntasks), n1m(ntasks))
                                   call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                                   call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))

                                   if (ntasks == max_ntasks)  then

                                         call dotasks_32_triplet(sigup_diag, sigup_nondiag, &
                                               t2, nocc0, nocc1, nvirt0, nvirt1, &
                                               n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                               n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                               npair, ntasks, rmfold, nidx_plus, nidx_minus)
                                         ntasks = 0
                                   end if
                              end do
                              end do
                              end do
                              end do
                        end do

                        if(ntasks .gt. 0)then

                              call dotasks_32_triplet(sigup_diag, sigup_nondiag, &
                                    t2, nocc0, nocc1, nvirt0, nvirt1, &
                                    n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                    n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
                                    npair, ntasks, rmfold, nidx_plus, nidx_minus)
                              ntasks = 0
                        end if
                  end do
            end if
            
            deallocate(n0a)
            deallocate(n1a)
            deallocate(n0b)
            deallocate(n1b)
            deallocate(n0c)
            deallocate(n1c)
            deallocate(n0d)
            deallocate(n1d)
            deallocate(n0e)
            deallocate(n1e)

            deallocate(n0i)
            deallocate(n1i)
            deallocate(n0j)
            deallocate(n1j)
            deallocate(n0k)
            deallocate(n1k)
            deallocate(n0l)
            deallocate(n1l)
            deallocate(n0m)
            deallocate(n1m)

            
      end subroutine ccjac_32_dav_driver_triplet

      subroutine dotasks_32_triplet(sigup_diag, sigup_nondiag, &
            t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
            n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m, &
            npair, ntasks, rmfold, nidx_plus, nidx_minus)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
            integer, intent(in)                                             :: nocc0, nvirt0
            integer, intent(in)                                             :: nocc1, nvirt1
            integer, intent(in)                                             :: npair
            integer, intent(in)                                             :: nidx_plus, nidx_minus
            integer, intent(in)                                             :: rmfold


            integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
            integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m
            integer, intent(in) :: ntasks
            integer :: k


            if (rmfold == cc_plus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_32_tripletp_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), n0m(k), n1m(k), &
                              npair + nidx_plus + nidx_minus + 1, npair + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            else if (rmfold == cc_minus) then

                  !$omp parallel private(k) default(shared)    
                  !$omp do                                                        
                  do k = 1, ntasks
                        call ccjac_32_tripletm_dav(sigup_diag, sigup_nondiag, &
                              t2, nocc0, nocc1, nvirt0, nvirt1, &
                              n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
                              n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), n0m(k), n1m(k), &
                              npair + nidx_plus + nidx_minus + 1, npair + nidx_plus + 1)
                  end do
                  !$omp end do       
                  !$omp end parallel

            end if
                             
      end subroutine dotasks_32_triplet


      subroutine loop_boundaries(p, n, int, n0, n1)
            integer, intent(in) :: p
            integer, intent(in) :: n
            integer, intent(in) :: int
            integer, intent(out) :: n0, n1

            n0 = p
            n1 = min(n+1, p+int)-1

      end subroutine loop_boundaries



end module jacobian_triplet
