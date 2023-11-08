module cc3_intermediates_for_21

      use basis
      use cc3_intermediates
      use davidson_main
      use t1_transformed_int
      use symmetry
      use cc_gparams

      implicit none

      double precision, dimension(:,:,:,:), allocatable :: w1
      double precision, dimension(:,:,:,:), allocatable :: w2
      integer :: ai, aj, al, bi, bj, bl, ck, di, dj, dl, ai2, bj2, dl2

      integer, dimension(:, :), allocatable :: gitriples_A
      integer, dimension(:, :), allocatable :: gitriples_B1
      integer, dimension(:, :), allocatable :: gitriples_B2
      integer, dimension(:, :), allocatable :: gitriples_C1
      integer, dimension(:, :), allocatable :: gitriples_C2
      integer, dimension(:, :), allocatable :: gitriples_D1
      integer, dimension(:, :), allocatable :: gitriples_D2
      integer, dimension(:, :), allocatable :: gitriples_D3
      integer, dimension(:, :), allocatable :: gitriples_D4
      integer :: gidimt_A
      integer :: gidimt_B1
      integer :: gidimt_B2
      integer :: gidimt_C1
      integer :: gidimt_C2
      integer :: gidimt_D1
      integer :: gidimt_D2
      integer :: gidimt_D3
      integer :: gidimt_D4

      integer, dimension(:,:), allocatable :: girrep0, girrep1

contains


      subroutine init_block_21_cc3(nocc, nocc0, nvirt0, irrep0, irrep1)

            integer, intent(in)                              :: nocc
            integer, intent(in)                              :: nocc0, nvirt0
            integer, dimension(:, :), intent(in)             :: irrep0
            integer, dimension(:, :), intent(in)             :: irrep1
            integer :: full_sym
            integer :: order

            if (POINT_GROUP == D2h) then
                  full_sym = REP_Ag
                  order = D2h_order
            else if (POINT_GROUP == C2v) then
                  full_sym = REP_A1
                  order = C2v_order
            end if
            
            call irrep_triples(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_A, POINT_GROUP, gidimt_A, .true., .false.)
            allocate(gitriples_A(6, gidimt_A))
            call irrep_triples(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_A, POINT_GROUP, gidimt_A, .false., .false.)

            call irrep_triples_B1(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_B1, POINT_GROUP, gidimt_B1, .true.)
            allocate(gitriples_B1(6, gidimt_B1))
            call irrep_triples_B1(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_B1, POINT_GROUP, gidimt_B1, .false.)

            call irrep_triples_B2(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_B2, POINT_GROUP, gidimt_B2, .true.)
            allocate(gitriples_B2(6, gidimt_B2))
            call irrep_triples_B2(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_B2, POINT_GROUP, gidimt_B2, .false.)

           call irrep_triples_C1(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_C1, POINT_GROUP, gidimt_C1, .true.)
            allocate(gitriples_C1(6, gidimt_C1))
            call irrep_triples_C1(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_C1, POINT_GROUP, gidimt_C1, .false.)

            call irrep_triples_C2(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_C2, POINT_GROUP, gidimt_C2, .true.)
            allocate(gitriples_C2(6, gidimt_C2))
            call irrep_triples_C2(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_C2, POINT_GROUP, gidimt_C2, .false.)
            
            call irrep_triples_D1(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D1, POINT_GROUP, gidimt_D1, .true.)
            allocate(gitriples_D1(6, gidimt_D1))
            call irrep_triples_D1(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D1, POINT_GROUP, gidimt_D1, .false.)

            call irrep_triples_D2(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D2, POINT_GROUP, gidimt_D2, .true.)
            allocate(gitriples_D2(6, gidimt_D2))
            call irrep_triples_D2(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D2, POINT_GROUP, gidimt_D2, .false.)

            call irrep_triples_D3(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D3, POINT_GROUP, gidimt_D3, .true.)
            allocate(gitriples_D3(6, gidimt_D3))
            call irrep_triples_D3(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D3, POINT_GROUP, gidimt_D3, .false.)

            call irrep_triples_D4(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D4, POINT_GROUP, gidimt_D4, .true.)
            allocate(gitriples_D4(6, gidimt_D4))
            call irrep_triples_D4(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples_D4, POINT_GROUP, gidimt_D4, .false.)


            allocate(girrep0(2, order))
            allocate(girrep1(2, order))

            girrep0 = irrep0
            girrep1 = irrep1
            
      end subroutine init_block_21_cc3

      subroutine ccjac_21_cc3_dav_triples_part_sym(sigup_diag, sigup_nondiag, nocc0,&
            nocc1, nvirt0, nvirt1, bra0, ket0)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: bra0, ket0
            !
            ! Local variables
            !
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: braoffset, ketoffset

            integer, dimension(:,:), allocatable                    :: itriples
            integer :: m0i, m1i, m0j, m1j, m0l, m1l
            integer :: m0a, m1a, m0b, m1b, m0d, m1d
            integer :: m0q, m1q, m0u, m1u
            integer :: pa, pb, pd, pi, pj, pl
            integer :: kj
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks

            real(F64) :: time1, time2

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(itriples(max_ntasks, 6))
            !
            ! Offset of the jacobian blocks
            !
            braoffset = bra0 - 1
            ketoffset = ket0 - 1
            !
            ! Number of occupied and virtual orbitals
            ! present in calculations
            !
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nactive = nocc + nvirt


            !$ time1 = omp_get_wtime()

            ntasks = 0
!            print*, 'gimt_A', gidimt_A
            do kj = 1, gidimt_A
                  
                  call loop_boundaries_sp(gitriples_A(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_A(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  call loop_boundaries_sp(gitriples_A(5:6, kj), girrep0, girrep1, &
                        m0l, m1l, m0d, m1d)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pd = m0d, minval([m1d, pb-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])
                  do pl = m0l, minval([m1l, pj-1])
                        ntasks = ntasks + 1
!                        write(*, '(6I4)') pa, pb, pd, pi, pj, pl
                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pb
                        itriples(ntasks, 4) = pj
                        itriples(ntasks, 5) = pd
                        itriples(ntasks, 6) = pl

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_A_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
                  end do
                  end do
            end do


            if(ntasks .gt.0)then
                  call dotasks_up_A_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na A', time2 - time1
!$ time1 = omp_get_wtime()


            ntasks = 0
            do kj = 1, gidimt_C1
                  
                  call loop_boundaries_sp(gitriples_C1(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_C1(3:4, kj), girrep0, girrep1, &
                        m0q, m1q, m0b, m1b)
                  call loop_boundaries_sp(gitriples_C1(5:6, kj), girrep0, girrep1, &
                        m0j, m1j, m0d, m1d)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pd = m0d, minval([m1d, pb-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])
                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pb
                        itriples(ntasks, 4) = pi
                        itriples(ntasks, 5) = pd
                        itriples(ntasks, 6) = pj

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_C1_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_C1_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na C1', time2 - time1
!$ time1 = omp_get_wtime()


            ntasks = 0
            do kj = 1, gidimt_C2

                  call loop_boundaries_sp(gitriples_C2(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_C2(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  call loop_boundaries_sp(gitriples_C2(5:6, kj), girrep0, girrep1, &
                        m0q, m1q, m0d, m1d)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pd = m0d, minval([m1d, pb-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])
                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pb
                        itriples(ntasks, 4) = pj
                        itriples(ntasks, 5) = pd
                        itriples(ntasks, 6) = pj

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_C2_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if

                  end do
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_C2_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na C2', time2 - time1
!$ time1 = omp_get_wtime()



            ntasks = 0
            do kj = 1, gidimt_B1
                  
                  call loop_boundaries_sp(gitriples_B1(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_B1(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0q, m1q)
                  call loop_boundaries_sp(gitriples_B1(5:6, kj), girrep0, girrep1, &
                        m0l, m1l, m0b, m1b)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])
                  do pl = m0l, minval([m1l, pj-1])
                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pa
                        itriples(ntasks, 4) = pj
                        itriples(ntasks, 5) = pb
                        itriples(ntasks, 6) = pl

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_B1_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_B1_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na B1', time2 - time1
!$ time1 = omp_get_wtime()


            ntasks = 0
            do kj = 1, gidimt_B2
                  
                  call loop_boundaries_sp(gitriples_B2(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_B2(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  call loop_boundaries_sp(gitriples_B2(5:6, kj), girrep0, girrep1, &
                        m0l, m1l, m0q, m1q)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])
                  do pl = m0l, minval([m1l, pj-1])
                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pb
                        itriples(ntasks, 4) = pj
                        itriples(ntasks, 5) = pb
                        itriples(ntasks, 6) = pl

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_B2_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_B2_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na B2', time2 - time1
!$ time1 = omp_get_wtime()



            ntasks = 0
            do kj = 1, gidimt_D1
                  
                  call loop_boundaries_sp(gitriples_D1(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_D1(5:6, kj), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])

                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pa
                        itriples(ntasks, 4) = pi
                        itriples(ntasks, 5) = pb
                        itriples(ntasks, 6) = pj

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_D1_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_D1_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na D1', time2 - time1
!$ time1 = omp_get_wtime()


            ntasks = 0
            do kj = 1, gidimt_D2
                  
                  call loop_boundaries_sp(gitriples_D2(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_D2(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0q, m1q)
                  call loop_boundaries_sp(gitriples_D2(5:6, kj), girrep0, girrep1, &
                        m0u, m1u, m0b, m1b)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])

                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pa
                        itriples(ntasks, 4) = pj
                        itriples(ntasks, 5) = pb
                        itriples(ntasks, 6) = pj

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_D2_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_D2_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na D2', time2 - time1
!$ time1 = omp_get_wtime()


           ntasks = 0
            do kj = 1, gidimt_D3
                  
                  call loop_boundaries_sp(gitriples_D3(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_D3(3:4, kj), girrep0, girrep1, &
                        m0q, m1q, m0b, m1b)
                  call loop_boundaries_sp(gitriples_D3(5:6, kj), girrep0, girrep1, &
                        m0j, m1j, m0u, m1u)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])

                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pb
                        itriples(ntasks, 4) = pi
                        itriples(ntasks, 5) = pb
                        itriples(ntasks, 6) = pj

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_D3_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_D3_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na D3', time2 - time1
!$ time1 = omp_get_wtime()


           ntasks = 0
            do kj = 1, gidimt_D4
                  
                  call loop_boundaries_sp(gitriples_D4(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples_D4(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)

                  do pa = m0a, m1a
                  do pb = m0b, minval([m1b, pa-1])
                  do pi = m0i, m1i
                  do pj = m0j, minval([m1j, pi-1])

                        ntasks = ntasks + 1

                        itriples(ntasks, 1) = pa
                        itriples(ntasks, 2) = pi
                        itriples(ntasks, 3) = pb
                        itriples(ntasks, 4) = pj
                        itriples(ntasks, 5) = pb
                        itriples(ntasks, 6) = pj

                        if (ntasks == max_ntasks)  then
                              call dotasks_up_D4_driver(ntasks, itriples, braoffset,&
                                    ketoffset, nocc, nocc0, nvirt0,&
                                    nactive, npair, sigup_diag, sigup_nondiag)
                              ntasks = 0
                        end if
               
                  end do
                  end do
                  end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_up_D4_driver(ntasks, itriples, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
                  ntasks = 0
            end if

!$ time2 = omp_get_wtime()                                                                                                                   
            print*, 'czas na D4', time2 - time1
!$ time1 = omp_get_wtime()


      end subroutine ccjac_21_cc3_dav_triples_part_sym

      subroutine dotasks_up_A_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer :: a, i, b, j, d, l
            integer :: tl
!            !$omp parallel private(tl, a, i, b, j, d, l) default(shared)                                                                                                                
!            !$omp do schedule(guided)                                                                                                                                                    
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  b = itriples(tl, 3)
                  j = itriples(tl, 4)
                  d = itriples(tl, 5)
                  l = itriples(tl, 6)

                  call up_A_driver(a, b, d, i, j, l, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
 !           !$omp end do                                                                                                                                                                
!!$omp end parallel                                                                                                                                                           


      end subroutine dotasks_up_A_driver

      subroutine dotasks_up_B1_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)
            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer :: a, i, b, j, l
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j, l) default(shared)                                                                                                                   
            !!$omp do schedule(guided)                                                                                                                                                    
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  a = itriples(tl, 3)
                  j = itriples(tl, 4)
                  b = itriples(tl, 5)
                  l = itriples(tl, 6)

                  call up_B1_driver(a, b, i, j, l, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
           ! !$omp end do                                                                                                                                                                
            !!$omp end parallel                                                                                                                                                           

      end subroutine dotasks_up_B1_driver

      subroutine dotasks_up_B2_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)
            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer :: a, i, b, j, l
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j, l) default(shared)                                                                                                                   
            !!$omp do schedule(guided)                                                                                                                                                    
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  b = itriples(tl, 3)
                  j = itriples(tl, 4)
                  b = itriples(tl, 5)
                  l = itriples(tl, 6)

                  call up_B2_driver(a, b, i, j, l, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                                                                                                                                                               
            !!$omp end parallel                                                                                                                                                           

      end subroutine dotasks_up_B2_driver

      subroutine dotasks_up_C1_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer :: a, i, b, j, d
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j, d) default(shared)                                                                                                                    
            !!$omp do schedule(guided)                                                                                                                                                    
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  b = itriples(tl, 3)
                  i = itriples(tl, 4)
                  d = itriples(tl, 5)
                  j = itriples(tl, 6)

                  call up_C1_driver(a, b, d, i, j, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                                                                                                                                                                 
            !!$omp end parallel                                                                                                                                                           

      end subroutine dotasks_up_C1_driver

      subroutine dotasks_up_C2_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer :: a, i, b, j, d
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j, d) default(shared)         
            !!$omp do schedule(guided)
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  b = itriples(tl, 3)
                  j = itriples(tl, 4)
                  d = itriples(tl, 5)
                  j = itriples(tl, 6)

                  call up_C2_driver(a, b, d, i, j, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                
            !!$omp end parallel                                                    
      end subroutine dotasks_up_C2_driver

      subroutine dotasks_up_D1_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)
            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            
            integer :: a, i, b, j
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j) default(shared)                                              
            !!$omp do schedule(guided)
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  a = itriples(tl, 3)
                  i = itriples(tl, 4)
                  b = itriples(tl, 5)
                  j = itriples(tl, 6)

                  call up_D1_driver(a, b, i, j, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                                                                                                                    
            !!$omp end parallel

      end subroutine dotasks_up_D1_driver

      subroutine dotasks_up_D2_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)
            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            
            integer :: a, i, b, j
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j) default(shared)                                              
            !!$omp do schedule(guided)
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  a = itriples(tl, 3)
                  j = itriples(tl, 4)
                  b = itriples(tl, 5)
                  j = itriples(tl, 6)

                  call up_D2_driver(a, b, i, j, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                                                                                                                    
            !!$omp end parallel

      end subroutine dotasks_up_D2_driver


      subroutine dotasks_up_D3_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)
            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            
            integer :: a, i, b, j
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j) default(shared)                                              
            !!$omp do schedule(guided)
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  b = itriples(tl, 3)
                  i = itriples(tl, 4)
                  b = itriples(tl, 5)
                  j = itriples(tl, 6)

                  call up_D3_driver(a, b, i, j, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                                                                                                                    
            !!$omp end parallel

      end subroutine dotasks_up_D3_driver

      subroutine dotasks_up_D4_driver(ntasks, itriples, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)
            integer, intent(in) :: ntasks
            integer, dimension(:,:), intent(in) :: itriples
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            
            integer :: a, i, b, j
            integer :: tl

            !!$omp parallel private(tl, a, i, b, j) default(shared)                                              
            !!$omp do schedule(guided)
            do tl = 1, ntasks
                  a = itriples(tl, 1)
                  i = itriples(tl, 2)
                  b = itriples(tl, 3)
                  j = itriples(tl, 4)
                  b = itriples(tl, 5)
                  j = itriples(tl, 6)

                  call up_D4_driver(a, b, i, j, braoffset,&
                        ketoffset, nocc, nocc0, nvirt0,&
                        nactive, npair, sigup_diag, sigup_nondiag)
            end do
            !!$omp end do                                                                                                                    
            !!$omp end parallel

      end subroutine dotasks_up_D4_driver

      subroutine up_D1_driver(a, b, i, j, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2
            double precision :: parenth
            integer :: ai, aj, bi, bj

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1

            tt1 = t3(nocc, nactive, a, a, b, i, i, j)
            tt2 = t3(nocc, nactive, a, a, b, i, j, i)

            parenth = 0.5d+0 * (-2.d+0 * tt1 + tt2 + tt2)
            call up_D_sm(ai, ai, bj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt2 + tt1 + tt2)
            call up_D_sm(ai, aj, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt1 + tt2 + tt1)
            call up_D_sm(ai, bj, ai, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt2 + tt1 + tt2)
            call up_D_sm(ai, bi, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt2 + tt2 + tt2)
            call up_D_sm(aj, bi, ai, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

      end subroutine up_D1_driver

      subroutine up_D2_driver(a, b, i, j, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2
            double precision :: parenth
            integer :: ai, aj, bi, bj

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1

            tt1 = t3(nocc, nactive, a, a, b, i, j, j)
            tt2 = t3(nocc, nactive, a, a, b, j, j, i)

            parenth = 0.5d+0 * (-2.d+0 * tt2 + tt1 + tt1)
            call up_D_sm(aj, aj, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt1 + tt1 + tt2)
            call up_D_sm(ai, aj, bj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt1 +tt1  + tt1)
            call up_D_sm(ai, bj, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 *  tt2+ tt1 + tt2)
            call up_D_sm(aj, bi, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 *  tt1+ tt2 + tt1)
            call up_D_sm(aj, bj, ai, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

      end subroutine up_D2_driver

      subroutine up_D3_driver(a, b, i, j, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2
            double precision :: parenth
            integer :: ai, aj, bi, bj

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1

            tt1 = t3(nocc, nactive, a, b, b, i, i, j)
            tt2 = t3(nocc, nactive, a, b, b, j, i, i)

            parenth = (-2.d+0 * tt1 + tt1 + tt2)
            call up_D_sm(ai, bi, bj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt2 + tt2 + tt1)
            call up_D_sm(aj, bi, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = 0.5d+0*(-2.d+0 * tt2 + tt1 + tt1)
            call up_D_sm(bi, bi, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt1 + tt2 + tt1)
            call up_D_sm(bi, bj, ai, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt1 + tt1 + tt1)
            call up_D_sm(ai, bj, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

      end subroutine up_D3_driver

      subroutine up_D4_driver(a, b, i, j, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2
            double precision :: parenth
            integer :: ai, aj, bi, bj

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1

            tt1 = t3(nocc, nactive, a, b, b, i, j, j)
            tt2 = t3(nocc, nactive, a, b, b, j, j, i)

            parenth = (-2.d+0 * tt2 + tt2 + tt1)
            call up_D_sm(aj, bj, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt1 + tt1 + tt2)
            call up_D_sm(ai, bj, bj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = 0.5d+0 * (-2.d+0 * tt1 + tt2 + tt2)
            call up_D_sm(bj, bj, ai, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt2 + tt2 + tt1)
            call up_D_sm(bi, bj, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

            parenth = (-2.d+0 * tt2 + tt2 + tt2)
            call up_D_sm(aj, bi, bj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  parenth)

      end subroutine up_D4_driver


      subroutine up_B1_driver(a, b, i, j, l, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: l
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2, tt3
            integer :: ai, aj, al, bi, bj, bl

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1
            al = (a - nvirt0) * nocc + (l - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1
            bl = (b - nvirt0) * nocc + (l - nocc0) + 1

            tt1 = t3(nocc, nactive, a, a, b, i, j, l)
            tt2 = t3(nocc, nactive, a, a, b, i, l, j)
            tt3 = t3(nocc, nactive, a, a, b, j, l, i)

            call up_B(ai, aj, bl, ai, al, bj, aj, al, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt2, tt3,&
                  tt2, tt1, tt3, tt3, tt1, tt2)
            call up_B(ai, bl, aj, ai, bj, al, aj, bi, al, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt2, tt1,&
                  tt2, tt1, tt2, tt3, tt1, tt3)
            call up_B(aj, bl, ai, al, bj, ai, al, bi, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt3, tt1,&
                  tt2, tt3, tt2, tt3, tt2, tt3)

      end subroutine up_B1_driver

      subroutine up_B2_driver(a, b, i, j, l, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: l
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2, tt3
            integer :: ai, aj, al, bi, bj, bl

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1
            al = (a - nvirt0) * nocc + (l - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1
            bl = (b - nvirt0) * nocc + (l - nocc0) + 1

            tt1 = t3(nocc, nactive, a, b, b, i, j, l)
            tt2 = t3(nocc, nactive, a, b, b, l, i, j)
            tt3 = t3(nocc, nactive, a, b, b, j, l, i)

            call up_B(ai, bj, bl, al, bi, bj, aj, bl, bi, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt1, tt2,&
                  tt2, tt2, tt3, tt3, tt3, tt1)
            call up_B(ai, bl, bj, al, bj, bi, aj, bi, bl, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt1, tt3,&
                  tt2, tt2, tt1, tt3, tt3, tt2)
            call up_B(bj, bl, ai, bi, bj, al, bi, bl, aj, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt2, tt3,&
                  tt2, tt3, tt1, tt3, tt2, tt1)

      end subroutine up_B2_driver
                  
      subroutine up_C1_driver(a, b, d, i, j, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: d
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

           double precision :: tt1, tt2, tt3

            tt1 = t3(nocc, nactive, a, b, d, i, i, j)
            tt2 = t3(nocc, nactive, a, b, d, i, j, i)
            tt3 = t3(nocc, nactive, a, b, d, j, i, i)


            call up_C(a, b, d, i, i, j, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt2, tt3,&
                  tt2, tt1, tt2,&
                  tt3, tt3, tt1)

            call up_C(a, d, b, i, i, j, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt2, tt1, tt3,&
                  tt1, tt2, tt1,&
                  tt3, tt3, tt2)

            call up_C(b, d, a, i, i, j, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt3, tt1, tt2,&
                  tt1, tt3, tt1,&
                  tt2, tt2, tt3)

      end subroutine up_C1_driver


      subroutine up_C2_driver(a, b, d, i, j, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: d
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt4, tt5, tt6

            tt4 = t3(nocc, nactive, a, b, d, j, j, i)
            tt5 = t3(nocc, nactive, a, b, d, j, i, j)
            tt6 = t3(nocc, nactive, a, b, d, i, j, j)

            call up_C(a, b, d, j, j, i, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt4, tt5, tt6,&
                  tt5, tt4, tt5,&
                  tt6, tt6, tt4)

            call up_C(a, d, b, j, j, i, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt5, tt4, tt6,&
                  tt4, tt5, tt4,&
                  tt6, tt6, tt5)

            call up_C(b, d, a, j, j, i, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt6, tt4, tt5,&
                  tt4, tt6, tt4,&
                  tt5, tt5, tt6)

      end subroutine up_C2_driver
      
      subroutine up_A_driver(a, b, d, i, j, l, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag)

            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: d
            integer, intent(in) :: i
            integer, intent(in) :: j
            integer, intent(in) :: l
            integer, intent(in) :: braoffset
            integer, intent(in) :: ketoffset
            integer, intent(in) :: nocc
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nactive
            integer, intent(in) :: npair
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            double precision :: tt1, tt2, tt3, tt4, tt5, tt6


            tt1 = t3(nocc, nactive, a, b, d, i, j, l)
            tt2 = t3(nocc, nactive, a, b, d, i, l, j)
            tt3 = t3(nocc, nactive, a, b, d, j, i, l)
            tt4 = t3(nocc, nactive, a, b, d, j, l, i)
            tt5 = t3(nocc, nactive, a, b, d, l, i, j)
            tt6 = t3(nocc, nactive, a, b, d, l, j, i)



            call up_A(a, b, d, i, j, l, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt1, tt2, tt6,&
                  tt2, tt1, tt4,&
                  tt3, tt4, tt5,&
                  tt4, tt3, tt2,&
                  tt5, tt6, tt3,&
                  tt6, tt5, tt1)

            call up_A(a, d, b, i, j, l, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt2, tt1, tt5,&
                  tt1, tt2, tt3,&
                  tt4, tt3, tt6,&
                  tt3, tt4, tt1,&
                  tt6, tt5, tt4,&
                  tt5, tt6, tt2)

            call up_A(b, d, a, i, j, l, braoffset,&
                  ketoffset, nocc, nocc0, nvirt0,&
                  nactive, npair, sigup_diag, sigup_nondiag, &
                  tt5, tt3, tt2,&
                  tt3, tt5, tt1,&
                  tt6, tt1, tt4,&
                  tt1, tt6, tt3,&
                  tt4, tt2, tt6,&                                                                          
                  tt2, tt4, tt5)
      end subroutine up_A_driver


      subroutine ccjac_21_cc3_dav_triples_part(sigup_diag, sigup_nondiag, nocc0,&
            nocc1, nvirt0, nvirt1, bra0, ket0)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: bra0, ket0
            !
            ! Local variables
            !
            double precision :: tt1, tt2, tt3, tt4, tt5, tt6
            integer :: a, b, d
            integer :: i, j, l
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: braoffset, ketoffset
            double precision :: parenth
            !
            ! Offset of the jacobian blocks
            !
            braoffset = bra0 - 1
            ketoffset = ket0 - 1
            !
            ! Number of occupied and virtual orbitals
            ! present in calculations
            !
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nactive = nocc + nvirt

            aloop: do a = nvirt0,nvirt1
                  bloop: do b = nvirt0,a-1
                        dloop: do d = nvirt0,b -1
                              i1loop: do i = nocc0,nocc1
                                    j1loop: do j = nocc0,i-1
                                          l1loop: do l = nocc0,j -1
                                                !print*,' czesc A'

                                                tt1 = t3(nocc, nactive, a, b, d, i, j, l)
                                                tt2 = t3(nocc, nactive, a, b, d, i, l, j)
                                                tt3 = t3(nocc, nactive, a, b, d, j, i, l)
                                                tt4 = t3(nocc, nactive, a, b, d, j, l, i)
                                                tt5 = t3(nocc, nactive, a, b, d, l, i, j)
                                                tt6 = t3(nocc, nactive, a, b, d, l, j, i)
                                                
                                                call up_A(a, b, d, i, j, l, braoffset,&
                                                      ketoffset, nocc, nocc0, nvirt0,&
                                                      nactive, npair, sigup_diag, sigup_nondiag, &
                                                      tt1, tt2, tt6,&
                                                      tt2, tt1, tt4,&
                                                      tt3, tt4, tt5,&
                                                      tt4, tt3, tt2,&
                                                      tt5, tt6, tt3,&
                                                      tt6, tt5, tt1)

                                                call up_A(a, d, b, i, j, l, braoffset,&
                                                      ketoffset, nocc, nocc0, nvirt0,&
                                                      nactive, npair, sigup_diag, sigup_nondiag, &
                                                      tt2, tt1, tt5,&
                                                      tt1, tt2, tt3,&
                                                      tt4, tt3, tt6,&
                                                      tt3, tt4, tt1,&
                                                      tt6, tt5, tt4,&
                                                      tt5, tt6, tt2)

                                                call up_A(b, d, a, i, j, l, braoffset,&
                                                      ketoffset, nocc, nocc0, nvirt0,&
                                                      nactive, npair, sigup_diag, sigup_nondiag, &
                                                      tt5, tt3, tt2,&
                                                      tt3, tt5, tt1,&
                                                      tt6, tt1, tt4,&
                                                      tt1, tt6, tt3,&
                                                      tt4, tt2, tt6,&                                                
                                                      tt2, tt4, tt5)

                                          end do l1loop
                                          ! czesc C

                                          tt1 = t3(nocc, nactive, a, b, d, i, i, j)
                                          tt2 = t3(nocc, nactive, a, b, d, i, j, i)
                                          tt3 = t3(nocc, nactive, a, b, d, j, i, i)

                                          tt4 = t3(nocc, nactive, a, b, d, j, j, i)
                                          tt5 = t3(nocc, nactive, a, b, d, j, i, j)
                                          tt6 = t3(nocc, nactive, a, b, d, i, j, j)
                                          
                                          call up_C(a, b, d, i, i, j, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt2, tt3,&
                                                tt2, tt1, tt2,&
                                                tt3, tt3, tt1)

                                          call up_C(a, d, b, i, i, j, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt2, tt1, tt3,&
                                                tt1, tt2, tt1,&
                                                tt3, tt3, tt2)

                                          call up_C(b, d, a, i, i, j, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt3, tt1, tt2,&
                                                tt1, tt3, tt1,&
                                                tt2, tt2, tt3)

                                          call up_C(a, b, d, j, j, i, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt4, tt5, tt6,&
                                                tt5, tt4, tt5,&
                                                tt6, tt6, tt4)

                                          call up_C(a, d, b, j, j, i, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt5, tt4, tt6,&
                                                tt4, tt5, tt4,&
                                                tt6, tt6, tt5)

                                          call up_C(b, d, a, j, j, i, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt6, tt4, tt5,&
                                                tt4, tt6, tt4,&
                                                tt5, tt5, tt6)

                                    end do j1loop
                              end do i1loop
                        end do dloop
                        i2loop: do i = nocc0,nocc1
                              j2loop: do j = nocc0,i-1
                                    l2loop: do l = nocc0,j-1
                                          ! czesc B
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                          aj = (a - nvirt0) * nocc + (j - nocc0) + 1
                                          al = (a - nvirt0) * nocc + (l - nocc0) + 1

                                          bi = (b - nvirt0) * nocc + (i - nocc0) + 1
                                          bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                                          bl = (b - nvirt0) * nocc + (l - nocc0) + 1

                                          tt1 = t3(nocc, nactive, a, a, b, i, j, l)
                                          tt2 = t3(nocc, nactive, a, a, b, i, l, j)
                                          tt3 = t3(nocc, nactive, a, a, b, j, l, i)

                                          call up_B(ai, aj, bl, ai, al, bj, aj, al, bi, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt2, tt3,&
                                                tt2, tt1, tt3, tt3, tt1, tt2)
                                          call up_B(ai, bl, aj, ai, bj, al, aj, bi, al, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt2, tt1,&
                                                tt2, tt1, tt2, tt3, tt1, tt3)
                                          call up_B(aj, bl, ai, al, bj, ai, al, bi, aj, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt3, tt1,&
                                                tt2, tt3, tt2, tt3, tt2, tt3)

                                          tt1 = t3(nocc, nactive, a, b, b, i, j, l)
                                          tt2 = t3(nocc, nactive, a, b, b, l, i, j)
                                          tt3 = t3(nocc, nactive, a, b, b, j, l, i)

                                          call up_B(ai, bj, bl, al, bi, bj, aj, bl, bi, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt1, tt2,&
                                                tt2, tt2, tt3, tt3, tt3, tt1)
                                          call up_B(ai, bl, bj, al, bj, bi, aj, bi, bl, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt1, tt3,&
                                                tt2, tt2, tt1, tt3, tt3, tt2)
                                          call up_B(bj, bl, ai, bi, bj, al, bi, bl, aj, braoffset,&
                                                ketoffset, nocc, nocc0, nvirt0,&
                                                nactive, npair, sigup_diag, sigup_nondiag, &
                                                tt1, tt2, tt3,&
                                                tt2, tt3, tt1, tt3, tt2, tt1)

                                    end do l2loop
                                    ! czesc D

                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    aj = (a - nvirt0) * nocc + (j - nocc0) + 1

                                    bi = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (j - nocc0) + 1

                                    tt1 = t3(nocc, nactive, a, a, b, i, i, j)
                                    tt2 = t3(nocc, nactive, a, a, b, i, j, i)

                                    parenth = 0.5d+0 * (-2.d+0 * tt1 + tt2 + tt2)
                                    call up_D_sm(ai, ai, bj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt2 + tt1 + tt2)
                                    call up_D_sm(ai, aj, bi, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt1 + tt2 + tt1)
                                    call up_D_sm(ai, bj, ai, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt2 + tt1 + tt2)
                                    call up_D_sm(ai, bi, aj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt2 + tt2 + tt2)
                                    call up_D_sm(aj, bi, ai, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    tt1 = t3(nocc, nactive, a, a, b, i, j, j)
                                    tt2 = t3(nocc, nactive, a, a, b, j, j, i)

                                    parenth = 0.5d+0 * (-2.d+0 * tt2 + tt1 + tt1)
                                    call up_D_sm(aj, aj, bi, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt1 + tt1 + tt2)
                                    call up_D_sm(ai, aj, bj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt1 +tt1  + tt1)
                                    call up_D_sm(ai, bj, aj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)
                                    
                                    parenth = (-2.d+0 *  tt2+ tt1 + tt2)
                                    call up_D_sm(aj, bi, aj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 *  tt1+ tt2 + tt1)
                                    call up_D_sm(aj, bj, ai, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    tt1 = t3(nocc, nactive, a, b, b, i, i, j)
                                    tt2 = t3(nocc, nactive, a, b, b, j, i, i)

                                    parenth = (-2.d+0 * tt1 + tt1 + tt2)
                                    call up_D_sm(ai, bi, bj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt2 + tt2 + tt1)
                                    call up_D_sm(aj, bi, bi, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = 0.5d+0*(-2.d+0 * tt2 + tt1 + tt1)
                                    call up_D_sm(bi, bi, aj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt1 + tt2 + tt1)
                                    call up_D_sm(bi, bj, ai, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt1 + tt1 + tt1)
                                    call up_D_sm(ai, bj, bi, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    tt1 = t3(nocc, nactive, a, b, b, i, j, j)
                                    tt2 = t3(nocc, nactive, a, b, b, j, j, i)
                                    
                                    parenth = (-2.d+0 * tt2 + tt2 + tt1)
                                    call up_D_sm(aj, bj, bi, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt1 + tt1 + tt2)
                                    call up_D_sm(ai, bj, bj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = 0.5d+0 * (-2.d+0 * tt1 + tt2 + tt2)
                                    call up_D_sm(bj, bj, ai, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                                    parenth = (-2.d+0 * tt2 + tt2 + tt1)
                                    call up_D_sm(bi, bj, aj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)
                                    
                                    parenth = (-2.d+0 * tt2 + tt2 + tt2)
                                    call up_D_sm(aj, bi, bj, braoffset,&
                                          ketoffset, nocc, nocc0, nvirt0,&
                                          nactive, npair, sigup_diag, sigup_nondiag, &
                                          parenth)

                              end do j2loop
                        end do i2loop
                  end do bloop
            end do aloop

      end subroutine ccjac_21_cc3_dav_triples_part

      subroutine generate_intermediates_block_21_init(nocc, nactive, nocc0, nvirt0, irrep0, irrep1, multip)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            integer, intent(in) :: nocc0, nvirt0
            integer, dimension(:, :), intent(in)          :: irrep0, irrep1
            integer :: multip

            allocate(w1(nocc+1:nactive, nocc, nocc, nocc))
            allocate(w2(nocc+1:nactive, nocc+1:nactive, nocc+1:nactive, nocc))
            w1 = 0.d+0
            w2 = 0.d+0
            if (multip == cc_singlet)then
                  call init_block_21_cc3(nocc, nocc0, nvirt0, irrep0, irrep1)
            end if

      end subroutine generate_intermediates_block_21_init


      subroutine generate_intermediates_block_21_free(multip)
            integer, intent(in) :: multip
            deallocate(w1)
            deallocate(w2)
            if (multip == cc_singlet)then
                  deallocate(gitriples_A)
                  deallocate(gitriples_B1)
                  deallocate(gitriples_B2)
                  deallocate(gitriples_C1)
                  deallocate(gitriples_C2)
                  deallocate(gitriples_D1)
                  deallocate(gitriples_D2)
                  deallocate(gitriples_D3)
                  deallocate(gitriples_D4)
                  deallocate(girrep0)
                  deallocate(girrep1)
            end if
      end subroutine generate_intermediates_block_21_free


      subroutine generate_intermediates_block_21(method, nocc, nactive)
            integer, intent(in) :: method
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive

            integer :: a, b, c
            integer :: i, j, k

            if (method .eq. THEORY_CCSD)then
                  w1 = 0.d+0
                  w2 = 0.d+0
            else if (method .eq. THEORY_CC3) then
                  do a = nocc + 1, nactive
                        do i = 1, nocc
                              do j = 1, nocc
                                    do k = 1, nocc
                                          w1(a, i, j, k) = calc_w1(a, i, j, k, nocc, nactive)
                                    end do
                              end do
                        end do
                  end do

                  do a = nocc + 1, nactive
                        do b = nocc + 1, nactive
                              do c = nocc + 1, nactive
                                    do i = 1, nocc
                                          w2(a, b, c, i) = calc_w2(a, b, c, i, nocc, nactive)
                                    end do
                              end do
                        end do
                  end do
            else
                  call msg("ERROR: UNRECOGNIZED KEYWORD IN REQUEST FOR EOM-CC INTERMEDIATES", &
                        priority=MSG_ERROR)
                  stop
            end if
      end subroutine generate_intermediates_block_21


      subroutine up_A(a, b, d, i, j, l, braoffset,&
            ketoffset, nocc, nocc0, nvirt0, &
            nactive, npair, sigup_diag, sigup_nondiag, &
            tt1, tt2, tt3,&
            tt4, tt5, tt6,&
            tt7, tt8, tt9,&
            tt10, tt11, tt12,&
            tt13, tt14, tt15,&
            tt16, tt17, tt18)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer, intent(in) :: a, b, d, i, j, l
            integer, intent(in) :: braoffset, ketoffset
            integer, intent(in) :: nocc0, nvirt0
            integer, intent(in) :: nocc, nactive, npair
            double precision, intent(in) :: tt1, tt2, tt3, tt4, tt5, tt6
            double precision, intent(in) :: tt7, tt8, tt9, tt10, tt11, tt12
            double precision, intent(in) :: tt13, tt14, tt15, tt16, tt17, tt18

            integer :: c, k
            integer :: ibra1, ibra2, ibra3, ibra4, ibra5, ibra6
            double precision :: a_contr1, a_contr2, a_contr3, &
                  a_contr4, a_contr5, a_contr6
            double precision :: contr1, contr2, contr3, &
                  contr4, contr5, contr6
            integer :: iket

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            aj = (a - nvirt0) * nocc + (j - nocc0) + 1
            al = (a - nvirt0) * nocc + (l - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1
            bl = (b - nvirt0) * nocc + (l - nocc0) + 1

            ibra1 = calc_ibra(ai, bj, npair, braoffset)
            ibra2 = calc_ibra(ai, bl, npair, braoffset)
            ibra3 = calc_ibra(aj, bi, npair, braoffset)
            ibra4 = calc_ibra(aj, bl, npair, braoffset)
            ibra5 = calc_ibra(al, bi, npair, braoffset)
            ibra6 = calc_ibra(al, bj, npair, braoffset)

            a_contr1 = -2.d+0*(-2.d+0*tt1 + tt2 + tt3)
            a_contr2 = -2.d+0*(-2.d+0*tt4 + tt5 + tt6)
            a_contr3 = -2.d+0*(-2.d+0*tt7 + tt8 + tt9)
            a_contr4 = -2.d+0*(-2.d+0*tt10 + tt11 + tt12)
            a_contr5 = -2.d+0*(-2.d+0*tt13 + tt14 + tt15)
            a_contr6 = -2.d+0*(-2.d+0*tt16 + tt17 + tt18)


            do c = nocc + 1, nactive
                  do k = 1, nocc
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        iket = ketoffset + ck

                        contr1 = (tovov(k, c, l, d) - 0.5d+0*tovov(k, d, l, c))*  a_contr1
                         if(ibra1 == iket)then
                               call sigup_diag(contr1, ibra1)
                        else
                              call sigup_nondiag(contr1, ibra1, iket)
                         end if

                        contr2 = (tovov(k, c, j, d) - 0.5d+0*tovov(k, d, j, c))*  a_contr2
                        if(ibra2 == iket)then
                              call sigup_diag(contr2, ibra2)
                        else
                              call sigup_nondiag(contr2, ibra2, iket)
                        end if

                        contr3 = (tovov(k, c, l, d) - 0.5d+0*tovov(k, d, l, c))*  a_contr3
                        if(ibra3 == iket)then
                              call sigup_diag(contr3, ibra3)
                        else
                              call sigup_nondiag(contr3, ibra3, iket)
                        end if

                        contr4 = (tovov(k, c, i, d) - 0.5d+0*tovov(k, d, i, c))*  a_contr4
                        if(ibra4 == iket)then
                              call sigup_diag(contr4, ibra4)
                        else
                              call sigup_nondiag(contr4, ibra4, iket)
                        end if
                        
                        contr5 = (tovov(k, c, j, d) - 0.5d+0*tovov(k, d, j, c))*  a_contr5
                        if(ibra5 == iket)then
                              call sigup_diag(contr5, ibra5)
                        else
                              call sigup_nondiag(contr5, ibra5, iket)
                        end if
                        
                        contr6 = (tovov(k, c, i, d) - 0.5d+0*tovov(k, d, i, c))*  a_contr6
                        if(ibra6 == iket)then
                              call sigup_diag(contr6, ibra6)
                        else
                              call sigup_nondiag(contr6, ibra6, iket)
                        end if
                     end do
                  end do
      end subroutine up_A


      subroutine up_C(a, b, d, i, j, l, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag, &
            tt1, tt2, tt3,&
            tt4, tt5, tt6,&
            tt7, tt8, tt9)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer, intent(in) :: a, b, d, i, j, l
            integer, intent(in) :: braoffset, ketoffset
            integer, intent(in) :: nocc0, nvirt0
            integer, intent(in) :: nocc, nactive, npair
            double precision, intent(in) :: tt1, tt2, tt3, tt4, tt5, tt6
            double precision, intent(in) :: tt7, tt8, tt9

            integer :: c, k
            integer :: ibra1, ibra2, ibra3
            double precision :: a_contr1, a_contr2, a_contr3
            double precision :: contr1, contr2, contr3
            integer :: iket

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            al = (a - nvirt0) * nocc + (l - nocc0) + 1

            bi = (b - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1
            bl = (b - nvirt0) * nocc + (l - nocc0) + 1

            ibra1 = calc_ibra(ai, bj, npair, braoffset)
            ibra2 = calc_ibra(ai, bl, npair, braoffset)
            ibra3 = calc_ibra(al, bi, npair, braoffset)

            a_contr1 = -2.d+0*(-2.d+0*tt1 + tt2 + tt3)
            a_contr2 = -2.d+0*(-2.d+0*tt4 + tt5 + tt6)
            a_contr3 = -2.d+0*(-2.d+0*tt7 + tt8 + tt9)

            do c = nocc + 1, nactive
                  do k = 1, nocc
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        iket = ketoffset + ck
                        contr1 = (tovov(k, c, l, d) - 0.5d+0*tovov(k, d, l, c))*  a_contr1
                        if(ibra1 == iket)then
                              call sigup_diag(contr1, ibra1)
                        else
                              call sigup_nondiag(contr1, ibra1, iket)
                        end if

                        contr2 = (tovov(k, c, j, d) - 0.5d+0*tovov(k, d, j, c))*  a_contr2
                        if(ibra2 == iket)then
                              call sigup_diag(contr2, ibra2)
                        else
                              call sigup_nondiag(contr2, ibra2, iket)
                        end if

                        contr3 = (tovov(k, c, j, d) - 0.5d+0*tovov(k, d, j, c))*  a_contr3
                        if(ibra3 == iket)then
                              call sigup_diag(contr3, ibra3)
                        else
                              call sigup_nondiag(contr3, ibra3, iket)
                        end if
                  end do
            end do

      end subroutine up_C

      subroutine up_B(a1, a2, a3, b1, b2, b3, c1, c2, c3, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag, &
            tt1, tt2, tt3,&
            tt4, tt5, tt6,&
            tt7, tt8, tt9)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer, intent(in) :: a1, a2, a3, b1, b2, b3, c1, c2, c3
            integer, intent(in) :: braoffset, ketoffset
            integer, intent(in) :: nocc0, nvirt0
            integer, intent(in) :: nocc, nactive, npair
            double precision, intent(in) :: tt1, tt2, tt3, tt4, tt5, tt6
            double precision, intent(in) :: tt7, tt8, tt9

            integer :: c, k, d1, d2, d3, l1, l2, l3
            integer :: ibra1, ibra2, ibra3
            double precision :: a_contr1, a_contr2, a_contr3
            double precision :: contr1, contr2, contr3
            integer :: iket

            ibra1 = calc_ibra(a1, a2, npair, braoffset)
            ibra2 = calc_ibra(b1, b2, npair, braoffset)
            ibra3 = calc_ibra(c1, c2, npair, braoffset)

            a_contr1 = -2.d+0*(-2.d+0*tt1 + tt2 + tt3)
            a_contr2 = -2.d+0*(-2.d+0*tt4 + tt5 + tt6)
            a_contr3 = -2.d+0*(-2.d+0*tt7 + tt8 + tt9)

            d1 = (a3 - 1) / nocc + nocc+1
            l1 = a3 - nocc * (d1 - (nocc + 1))

            d2 = (b3 - 1) / nocc + nocc+1
            l2 = b3 - nocc * (d2 - (nocc + 1))

            d3 = (c3 - 1) / nocc + nocc+1
            l3 = c3 - nocc * (d3 - (nocc + 1))

            do c = nocc + 1, nactive
                  do k = 1, nocc
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        iket = ketoffset + ck
                        contr1 = (tovov(k, c, l1, d1) - 0.5d+0*tovov(k, d1, l1, c))* a_contr1
                        if(ibra1 == iket)then
                              call sigup_diag(contr1, ibra1)
                        else
                              call sigup_nondiag(contr1, ibra1, iket)
                        end if

                        contr2 = (tovov(k, c, l2, d2) - 0.5d+0*tovov(k, d2, l2, c))* a_contr2
                        if(ibra2 == iket)then
                              call sigup_diag(contr2, ibra2)
                        else
                              call sigup_nondiag(contr2, ibra2, iket)
                        end if

                        contr3 = (tovov(k, c, l3, d3) - 0.5d+0*tovov(k, d3, l3, c))* a_contr3
                        if(ibra3 == iket)then
                              call sigup_diag(contr3, ibra3)
                        else
                              call sigup_nondiag(contr3, ibra3, iket)
                        end if

                  end do
            end do

      end subroutine up_B

      subroutine up_D_sm(a1, a2, a3, braoffset,&
            ketoffset, nocc, nocc0, nvirt0,&
            nactive, npair, sigup_diag, sigup_nondiag, &
            parenth)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

            integer, intent(in) :: a1, a2, a3
            integer, intent(in) :: braoffset, ketoffset
            integer, intent(in) :: nocc0, nvirt0
            integer, intent(in) :: nocc, nactive, npair
            double precision, intent(in) :: parenth
            
            integer :: c, k, d, l
            integer :: ibra
            double precision :: a_contr
            double precision :: contr
            integer :: iket

            ibra = calc_ibra(a1, a2, npair, braoffset)

            a_contr = -2.d+0*parenth

            d = (a3 - 1) / nocc + nocc+1
            l = a3 - nocc * (d - (nocc + 1))

            do c = nocc + 1, nactive
                  do k = 1, nocc
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        iket = ketoffset + ck
                        contr = (tovov(k, c, l, d) - 0.5d+0*tovov(k, d, l, c))*  a_contr
                        if(ibra == iket)then
                              call sigup_diag(contr, ibra)
                        else
                              call sigup_nondiag(contr, ibra, iket)
                        end if
                  end do
            end do

      end subroutine up_D_sm

      function calc_ibra(ai, bj, npair, braoffset)
            integer :: calc_ibra
            integer, intent(in) :: ai, bj
            integer, intent(in) :: npair
            integer, intent(in) :: braoffset

            calc_ibra = braoffset + &
                  ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1

      end function calc_ibra

end module cc3_intermediates_for_21
