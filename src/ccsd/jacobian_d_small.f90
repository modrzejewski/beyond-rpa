module jacobian_d_small

      use eom_cc3_13_mem
      use eom_cc3_23_mem
      use eom_cc3_31_mem
      use eom_cc3_32_mem
      use eom_cc3_13_mem_noR
      use eom_cc3_23_mem_noR
      use eom_cc3_31_mem_noR
      use eom_cc3_32_mem_noR

      use davidson_main
      use math_constants
      use io
      use basis
      use arithmetic
      use journal
      use symmetry


      implicit none

      real(F64), dimension(:, :), allocatable, private :: MEM_VEC_ROWS
      ! real(F64), dimension(:, :), allocatable, private :: MEM_VEC_ROWS_R
      integer, parameter, private :: z0 = 0, z1 = 1, z2 = 2, z3 = 3, z4 = 4, z5 = 5, z6 = 6, z7 = 7, z8 =8, z9 = 9, z10 = 10
contains

      subroutine d_small_init(memspace, n)
            integer(I64), intent(in) :: memspace
            integer, intent(in) :: n
!             integer(I64) :: singlevector, halfspace
!             integer :: max_rows
!             print*, 'w dsmall'
!             singlevector = vectorsize(int(DAV_AORDER, I64))
!             halfspace = memspace / 2

!             max_rows = int(halfspace / singlevector) - 2

! !            allocate(MEM_VEC_ROWS(max_rows, n))

!             allocate(MEM_VEC_ROWS(max_rows, DAV_AORDER))
! !            allocate(MEM_VEC_ROWS_R(max_rows, DAV_AORDER))


      end subroutine d_small_init

      subroutine d_small_free()
            if (allocated(MEM_VEC_ROWS)) deallocate(MEM_VEC_ROWS)
      end subroutine d_small_free

      subroutine d_small_driver(d_small, t2, eorb, nocc0, nvirt0, nocc, nvirt, nactive, irrep0, irrep1, irrep_idx)
            real(F64), dimension(:, :), intent(inout) :: d_small
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            real(F64), dimension(:), intent(in) :: eorb
            integer, intent(in) :: nocc0, nvirt0, nocc, nvirt, nactive
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            integer, dimension(:, :), allocatable :: itriples
                        integer, dimension(:, :), allocatable :: itriples_abc
            integer, dimension(:, :), allocatable :: itriples_aac, itriples_abb

            integer, dimension(:, :), allocatable :: itriples_ijk_z0
            integer, dimension(:, :), allocatable :: itriples_ijk_z3
            integer, dimension(:, :), allocatable :: itriples_ijk_z4
            integer, dimension(:, :), allocatable :: itriples_ijk_z5
            integer, dimension(:, :), allocatable :: itriples_ijk_z6
            
            integer, dimension(:, :), allocatable :: itriples_iji_z1
            integer, dimension(:, :), allocatable :: itriples_iji_z7
            integer, dimension(:, :), allocatable :: itriples_iji_z9            

            integer, dimension(:, :), allocatable :: itriples_ijj_z2
            integer, dimension(:, :), allocatable :: itriples_ijj_z8
            integer, dimension(:, :), allocatable :: itriples_iij_z10

            integer, dimension(:, :), allocatable :: nbounds_abc
            integer, dimension(:, :), allocatable :: nbounds_aac, nbounds_abb
            integer, dimension(:, :), allocatable :: nbounds_ijk_z0
            integer, dimension(:, :), allocatable :: nbounds_ijk_z3
            integer, dimension(:, :), allocatable :: nbounds_ijk_z4
            integer, dimension(:, :), allocatable :: nbounds_ijk_z5
            integer, dimension(:, :), allocatable :: nbounds_ijk_z6
                        
            integer, dimension(:, :), allocatable :: nbounds_iji_z1
            integer, dimension(:, :), allocatable :: nbounds_iji_z7
            integer, dimension(:, :), allocatable :: nbounds_iji_z9
            
            integer, dimension(:, :), allocatable :: nbounds_ijj_z2
            integer, dimension(:, :), allocatable :: nbounds_ijj_z8
            integer, dimension(:, :), allocatable :: nbounds_iij_z10
            integer, dimension(:, :), allocatable :: target_pair_idx

            integer :: idimt, idimt_abc, idimt_aac, idimt_abb
            integer :: idimt_ijk_z0, idimt_ijk_z3, idimt_ijk_z4, idimt_ijk_z5, idimt_ijk_z6
            integer :: idimt_iji_z1, idimt_iji_z7, idimt_iji_z9
            integer :: idimt_ijj_z2, idimt_ijj_z8, idimt_iij_z10
                        integer :: n
            integer :: pos, cont
            integer :: info
            integer :: n0i, n1i, n0j, n1j, n0k, n1k
            integer :: n0a, n1a, n0b, n1b, n0c, n1c
            integer nidx, npair
            integer :: order
            integer :: a, i, b, j, c, k
            integer :: a0, b0, a1, b1, i0, i1, j0, j1
            integer :: nl0d, nl1d, nl0e, nl1e, nl0l, nl1l, nl0m, nl1m
            integer :: nr0d, nr1d, nr0e, nr1e, nr0l, nr1l, nr0m, nr1m
            integer :: n0d, n1d, n0e, n1e, n0l, n1l, n0m, n1m
            integer, dimension(:,:), allocatable :: idoubles
            integer, dimension(:,:), allocatable :: isingles
            integer :: idims, idimd
            integer :: kj, bj, kjl, kjr, bjl, bjr
            integer :: occ_idx, virt_idx
            integer :: virt_bound, occ_bound
            integer :: ntrial
            integer, dimension(:), allocatable      :: zn0a, zn1a, zn0b, zn1b, zn0c, zn1c, zn0d, zn1d
            integer, dimension(:), allocatable      :: zn0i, zn1i, zn0j, zn1j, zn0k, zn1k, zn0l, zn1l
            integer                                 :: pa, pb, pc, pd, pi, pj, pk, pl
            integer :: m0a, m1a, m0b, m1b, m0c, m1c
            integer :: m0d, m1d
            integer :: m0i, m1i, m0j, m1j, m0k, m1k
            integer :: m0l, m1l
            integer :: nthreads
            integer :: kkl
            real(F64) :: pluszon
            real(F64) :: eorb_diff
            integer :: iket

            real(F64) :: dprzed, dpo
            integer :: l, d, e, p, ere
            integer :: lr, ll, mr, ml, el, er, dl, dr, drlr, ermr
            integer :: loop_cycle, my_ID, num_threads
            real(F64) :: time1, time2, time3, time4, time_plusz0, time_plusz1, time_plusz2, time_plusz5, time_plusz6
            real(F64), dimension(:), allocatable :: sums
            integer :: pp, kk, max_ntasks, ntasks, ntt, jh
            integer, dimension(:), allocatable :: ta, tb, ti, tj, tc, tk
            double precision, dimension(:), allocatable :: w
            
            !$ time_plusz0 = omp_get_wtime() 
            !$ time_plusz5 = omp_get_wtime()
            if (POINT_GROUP == C2v) then
                  order = C2v_order
            else if (POINT_GROUP == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            call cc_gparams_init(nocc, nvirt, nocc0, nvirt0)

            npair = nocc*nvirt
            nidx = npair + ((npair + 1) * npair) / 2

            ntrial = DAV_BASISDIM
            n = DAV_NSOUGHT

!            do i = 1, ntrial
            !    call VECBOOK%read(i, pos, cont)
            !    if (cont == CONT_ROWS) then
            !       MEM_VEC_ROWS(i, :) = VECROWS(pos, :)
            !    else if (cont == CONT_COLS) then
            !       MEM_VEC_ROWS(i, :) = VECCOLS(:, pos)
            !    else
            !       call io_record_read(VECRECS, i, MEM_VEC_ROWS(i, :), info)
            !    end if
 !           end do

            allocate(nbounds_abc(2, order))
            allocate(nbounds_aac(2, order))
            allocate(nbounds_abb(2, order))
            allocate(nbounds_ijk_z0(2, order))
            allocate(nbounds_ijk_z3(2, order))
            allocate(nbounds_ijk_z4(2, order))
            allocate(nbounds_ijk_z5(2, order))
            allocate(nbounds_ijk_z6(2, order))

            allocate(nbounds_iji_z1(2, order))
            allocate(nbounds_iji_z7(2, order))
            allocate(nbounds_iji_z9(2, order))
            allocate(nbounds_ijj_z2(2, order))
            allocate(nbounds_ijj_z8(2, order))
            allocate(nbounds_iij_z10(2, order))

            allocate(target_pair_idx(2, order))

            call irrep_triples_abc(nbounds_abc, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_abc, POINT_GROUP, idimt_abc, .true.)
            allocate(itriples_abc(3, idimt_abc))
            call irrep_triples_abc(nbounds_abc, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_abc, POINT_GROUP, idimt_abc, .false.)

            !------------------------------------aac----------------------
            call irrep_triples_aac(nbounds_aac, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_aac, POINT_GROUP, idimt_aac, .true., z3)
            allocate(itriples_aac(3, idimt_aac))
            call irrep_triples_aac(nbounds_aac, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_aac, POINT_GROUP, idimt_aac, .false., z3)
            
            call irrep_triples_aac(nbounds_abb, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_abb, POINT_GROUP, idimt_abb, .true., z5)
            allocate(itriples_abb(3, idimt_abb))
            call irrep_triples_aac(nbounds_abb, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_abb, POINT_GROUP, idimt_abb, .false., z5)

            !------------------------------------ijk----------------------
            call irrep_triples_ijk(nbounds_ijk_z0, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z0, POINT_GROUP, idimt_ijk_z0, .true., z0)
            allocate(itriples_ijk_z0(3, idimt_ijk_z0))
            call irrep_triples_ijk(nbounds_ijk_z0, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z0, POINT_GROUP, idimt_ijk_z0, .false., z0)

            
            call irrep_triples_ijk(nbounds_ijk_z3, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z3, POINT_GROUP, idimt_ijk_z3, .true., z3)
            allocate(itriples_ijk_z3(3, idimt_ijk_z3))
            call irrep_triples_ijk(nbounds_ijk_z3, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z3, POINT_GROUP, idimt_ijk_z3, .false., z3)

            call irrep_triples_ijk(nbounds_ijk_z4, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z4, POINT_GROUP, idimt_ijk_z4, .true., z4)
            allocate(itriples_ijk_z4(3, idimt_ijk_z4))
            call irrep_triples_ijk(nbounds_ijk_z4, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z4, POINT_GROUP, idimt_ijk_z4, .false., z4)

            call irrep_triples_ijk(nbounds_ijk_z5, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z5, POINT_GROUP, idimt_ijk_z5, .true., z5)
            allocate(itriples_ijk_z5(3, idimt_ijk_z5))
            call irrep_triples_ijk(nbounds_ijk_z5, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z5, POINT_GROUP, idimt_ijk_z5, .false., z5)

            call irrep_triples_ijk(nbounds_ijk_z6, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z6, POINT_GROUP, idimt_ijk_z6, .true., z6)
            allocate(itriples_ijk_z6(3, idimt_ijk_z6))
            call irrep_triples_ijk(nbounds_ijk_z6, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijk_z6, POINT_GROUP, idimt_ijk_z6, .false., z6)

            !------------------------------------iji----------------------
            call irrep_triples_iji(nbounds_iji_z1, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iji_z1, POINT_GROUP, idimt_iji_z1, .true., z1)
            allocate(itriples_iji_z1(3, idimt_iji_z1))
            call irrep_triples_iji(nbounds_iji_z1, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iji_z1, POINT_GROUP, idimt_iji_z1, .false., z1)

            call irrep_triples_iji(nbounds_iji_z7, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iji_z7, POINT_GROUP, idimt_iji_z7, .true., z7)
            allocate(itriples_iji_z7(3, idimt_iji_z7))
            call irrep_triples_iji(nbounds_iji_z7, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iji_z7, POINT_GROUP, idimt_iji_z7, .false., z7)

            call irrep_triples_iji(nbounds_iji_z9, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iji_z9, POINT_GROUP, idimt_iji_z9, .true., z9)
            allocate(itriples_iji_z9(3, idimt_iji_z9))
            call irrep_triples_iji(nbounds_iji_z9, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iji_z9, POINT_GROUP, idimt_iji_z9, .false., z9)
            !------------------------------------ijj----------------------
            call irrep_triples_iji(nbounds_ijj_z2, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijj_z2, POINT_GROUP, idimt_ijj_z2, .true., z2)
            allocate(itriples_ijj_z2(3, idimt_ijj_z2))
            call irrep_triples_iji(nbounds_ijj_z2, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijj_z2, POINT_GROUP, idimt_ijj_z2, .false., z2)

            call irrep_triples_iji(nbounds_ijj_z8, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijj_z8, POINT_GROUP, idimt_ijj_z8, .true., z8)
            allocate(itriples_ijj_z8(3, idimt_ijj_z8))
            call irrep_triples_iji(nbounds_ijj_z8, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_ijj_z8, POINT_GROUP, idimt_ijj_z8, .false., z8)

            call irrep_triples_iji(nbounds_iij_z10, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iij_z10, POINT_GROUP, idimt_iij_z10, .true., z10)
            allocate(itriples_iij_z10(3, idimt_iij_z10))
            call irrep_triples_iji(nbounds_iij_z10, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples_iij_z10, POINT_GROUP, idimt_iij_z10, .false., z10)

            call target_pairs(POINT_GROUP, irrep_idx, target_pair_idx)

            call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)
                        
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, &
                 POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, &
                 POINT_GROUP, idimd, .false., .true.)

            call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples, POINT_GROUP, idimt, .true., .false.)
            allocate(itriples(6, idimt))
            call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, &
                 nocc, nvirt0, itriples, POINT_GROUP, idimt, .false., .false.)
            !$ time_plusz6 = omp_get_wtime()                                                                                                           
            print*, 'PLUSZPLUSZplusz56', time_plusz6 - time_plusz5
            !$ time_plusz2 = omp_get_wtime() 
            print*, 'allololo1', time_plusz2 - time_plusz0
            npair = nocc * (nactive - nocc)

            !$ time1 = omp_get_wtime()
            SIGROWS_CC3 = zero
            !$ time2 = omp_get_wtime()
            print*, 'aaaa', time2-time1
            d_small = zero
            !$ time1 = omp_get_wtime()
            !z0
            !$ time_plusz5 = omp_get_wtime()
            do kk = 1, order
               virt_bound = target_pair_idx(1, kk)
               occ_bound = target_pair_idx(2, kk)
               !$omp parallel default(shared) &
               !$omp private(virt_idx, occ_idx, a, b, c, i, j, k, eorb_diff)
               !$omp do collapse(2) reduction(+:d_small)
               do virt_idx = nbounds_abc(1, virt_bound), nbounds_abc(2, virt_bound)
                  do occ_idx = nbounds_ijk_z0(1, occ_bound), nbounds_ijk_z0(2, occ_bound)
                     a = itriples_abc(1, virt_idx)
                     b = itriples_abc(2, virt_idx)
                     c = itriples_abc(3, virt_idx)
                     i = itriples_ijk_z0(1, occ_idx)
                     j = itriples_ijk_z0(2, occ_idx)
                     k = itriples_ijk_z0(3, occ_idx)
                     eorb_diff = eorb(a) + eorb(b) + eorb(c) &
                          - eorb(i) - eorb(j) - eorb(k)
                     call z0_driver(a, i, b, j, c, k, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, ntrial, &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)

                  end do
               end do
               !$omp end do
               !$omp end parallel
            end do
!            !$ time2 = omp_get_wtime()
!            print*, 'pluszowo2', time2-time1
!            stop

            !$ time_plusz6 = omp_get_wtime()                                                                                                          
            print*, 'PLUSZPLUSZplusz56-2222', time_plusz6 - time_plusz5

!*****************************************************************************************
      ! z1 aibjcia>b>c (iji) i = k, j!=i
            !$ time1 = omp_get_wtime()
      do k = 1, order
         virt_bound = target_pair_idx(1, k)
         occ_bound = target_pair_idx(2, k)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, b, c, i, j, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small)
         do virt_idx = nbounds_abc(1, virt_bound), nbounds_abc(2, virt_bound)
            do occ_idx = nbounds_iji_z1(1, occ_bound), nbounds_iji_z1(2, occ_bound)
               a = itriples_abc(1, virt_idx)
               b = itriples_abc(2, virt_idx)
               c = itriples_abc(3, virt_idx)
               i = itriples_iji_z1(1, occ_idx)
               j = itriples_iji_z1(2, occ_idx)
               eorb_diff = eorb(a) + eorb(b) + eorb(c) &
                    - eorb(i) - eorb(j) - eorb(i)
               call z1_driver(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, ntrial, &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

      !$ time2 = omp_get_wtime()                                                                                                                  
      print*, 'pluszowo-z1', time2-time1 
      !$ time1 = omp_get_wtime()
      !*****************************************************************************************

      ! z2 aibjcj  a>b>c (ijj) j = k, j!=i                                                                      
      do k = 1, order
         virt_bound = target_pair_idx(1, k)
         occ_bound = target_pair_idx(2, k)
!$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, b, c, i, j,  eorb_diff)
         !$omp do collapse(2) reduction (+:d_small)
         do virt_idx = nbounds_abc(1, virt_bound), nbounds_abc(2, virt_bound)
            do occ_idx = nbounds_ijj_z2(1, occ_bound), nbounds_ijj_z2(2, occ_bound)
               a = itriples_abc(1, virt_idx)
               b = itriples_abc(2, virt_idx)
               c = itriples_abc(3, virt_idx)
               i = itriples_ijj_z2(1, occ_idx)
               j = itriples_ijj_z2(2, occ_idx)
               eorb_diff = eorb(a) + eorb(b) + eorb(c) &
                    - eorb(i) - eorb(j) - eorb(j)
               call z2_driver(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z2', time2-time1
      !$ time1 = omp_get_wtime()

      ! z34 aiajck aac (ijk) a>c, j < k, i > (j, k)
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, c, i, j, k, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_aac(1, virt_bound), nbounds_aac(2, virt_bound)
            do occ_idx = nbounds_ijk_z3(1, occ_bound), nbounds_ijk_z3(2, occ_bound)
               a = itriples_aac(1, virt_idx)
               c = itriples_aac(3, virt_idx)
               i = itriples_ijk_z3(1, occ_idx)
               j = itriples_ijk_z3(2, occ_idx)
               k = itriples_ijk_z3(3, occ_idx)
               eorb_diff = eorb(a) + eorb(a) + eorb(c) &
                    - eorb(i) - eorb(j) - eorb(k)
               call z34_driver(a, i, j, c, k, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z34-1', time2-time1
      !$ time1 = omp_get_wtime()

      ! z34 aiajck  aac (ijk) a>c, j < k, i > j, i < k
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, c, i, j, k, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_aac(1, virt_bound), nbounds_aac(2, virt_bound)
            do occ_idx = nbounds_ijk_z4(1, occ_bound), nbounds_ijk_z4(2, occ_bound)
               a = itriples_aac(1, virt_idx)
               c = itriples_aac(3, virt_idx)
               i = itriples_ijk_z4(1, occ_idx)
               j = itriples_ijk_z4(2, occ_idx)
               k = itriples_ijk_z4(3, occ_idx)
               !              write(*, '(5I5)') a, c, i, j, k               

               eorb_diff = eorb(a) + eorb(a) + eorb(c) &
                    - eorb(i) - eorb(j) - eorb(k)
               call z34_driver(a, i, j, c, k, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
        !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z34-2', time2-time1
      !$ time1 = omp_get_wtime()

      !z5 abb (ijk) a>b, j > k, i > k, i < j
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, b, i, j, k, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_abb(1, virt_bound), nbounds_abb(2, virt_bound)
            do occ_idx = nbounds_ijk_z5(1, occ_bound), nbounds_ijk_z5(2, occ_bound)
               a = itriples_abb(1, virt_idx)
               b = itriples_abb(2, virt_idx)
               i = itriples_ijk_z5(1, occ_idx)
               j = itriples_ijk_z5(2, occ_idx)
               k = itriples_ijk_z5(3, occ_idx)
               eorb_diff = eorb(a) + eorb(b) + eorb(b) &
                    - eorb(i) - eorb(j) - eorb(k)
               call z56_driver(a, i, b, j, k, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z56-1', time2-time1
      !$ time1 = omp_get_wtime()

      ! z6 abb (ijk) a>b, j>k, i<j, i<k 
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, b, i, j, k, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_abb(1, virt_bound), nbounds_abb(2, virt_bound)
            do occ_idx = nbounds_ijk_z6(1, occ_bound), nbounds_ijk_z6(2, occ_bound)
               a = itriples_abb(1, virt_idx)
               b = itriples_abb(2, virt_idx)
               i = itriples_ijk_z6(1, occ_idx)
               j = itriples_ijk_z6(2, occ_idx)
               k = itriples_ijk_z6(3, occ_idx)
               eorb_diff = eorb(a) + eorb(b) + eorb(b) &
                    - eorb(i) - eorb(j) - eorb(k)
               call z56_driver(a, i, b, j, k, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z56-2', time2-time1
      !$ time1 = omp_get_wtime()


      ! z7 aac (iji) i = k, i > j                                                          
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, i, j, c, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_aac(1, virt_bound), nbounds_aac(2, virt_bound)
            do occ_idx = nbounds_iji_z7(1, occ_bound), nbounds_iji_z7(2, occ_bound)
               a = itriples_aac(1, virt_idx)
               c = itriples_aac(3, virt_idx)
               i = itriples_iji_z7(1, occ_idx)
               j = itriples_iji_z7(2, occ_idx)
               eorb_diff = eorb(a) + eorb(a) + eorb(c) &
                    - eorb(i) - eorb(j) - eorb(i)
               call z7_driver(a, i, j, c, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z7', time2-time1
      !$ time1 = omp_get_wtime()

      ! z8 aac (ijj) j = k, i > j                                                                                   
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, i, j, c, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_aac(1, virt_bound), nbounds_aac(2, virt_bound)
            do occ_idx = nbounds_ijj_z8(1, occ_bound), nbounds_ijj_z8(2, occ_bound)
               a = itriples_aac(1, virt_idx)
               c = itriples_aac(3, virt_idx)
               i = itriples_ijj_z8(1, occ_idx)               
               j = itriples_ijj_z8(2, occ_idx)
               eorb_diff = eorb(a) + eorb(a) + eorb(c) &
                    - eorb(i) - eorb(j) - eorb(j)
               call z8_driver(a, i, j, c, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do


!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z8', time2-time1
      !$ time1 = omp_get_wtime()

      ! z9 abb (iji) a>b, i<j
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, i, b, j, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_abb(1, virt_bound), nbounds_abb(2, virt_bound)
            do occ_idx = nbounds_iji_z9(1, occ_bound), nbounds_iji_z9(2, occ_bound)
               a = itriples_abb(1, virt_idx)
               b = itriples_abb(2, virt_idx)
               i = itriples_iji_z9(1, occ_idx)               
               j = itriples_iji_z9(2, occ_idx)
               eorb_diff = eorb(a) + eorb(b) + eorb(b) &
                    - eorb(i) - eorb(j) - eorb(i)
               call z9_driver(a, i, b, j, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do

!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z9', time2-time1
      !$ time1 = omp_get_wtime()

      ! z10 abb (iij) a>b, i>j
      do kk = 1, order
         virt_bound = target_pair_idx(1, kk)
         occ_bound = target_pair_idx(2, kk)
         !$omp parallel default(shared) &
         !$omp private(virt_idx, occ_idx, a, i, b, j, eorb_diff)
         !$omp do collapse(2) reduction (+:d_small) 
         do virt_idx = nbounds_abb(1, virt_bound), nbounds_abb(2, virt_bound)
            do occ_idx = nbounds_iij_z10(1, occ_bound), nbounds_iij_z10(2, occ_bound)
               a = itriples_abb(1, virt_idx)
               b = itriples_abb(2, virt_idx)
               i = itriples_iij_z10(1, occ_idx)               
               j = itriples_iij_z10(2, occ_idx)
               eorb_diff = eorb(a) + eorb(b) + eorb(b) &
                    - eorb(i) - eorb(i) - eorb(j)
               call z10_driver(a, i, b, j, t2, isingles, idoubles, idims, idimd, &
                    irrep0, irrep1, VECROWS, VECROWS, ntrial, ntrial,  &
                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
!$ time2 = omp_get_wtime()                                                                                                                         
      print*, 'pluszowo-z10', time2-time1


      !$ time2 = omp_get_wtime()
      print*, 'allololololo', time2-time1
            
! ntasks = 1
! ntt = 1
!       ! print*, 'z1'
!       ! z1 aibjcia>b>c (iji) i = k, j!=i
!       do c = n0c, n1c
!          do j = n0j, n1j
!             b0 = max(c + 1, n0b)
!             do b = b0, n1b
!                iloop01: do i = n0ik, n1ik
!                   if (i == j) cycle iloop01
!                   a0 = max(b + 1, n0a)
!                   do a = a0, n1a
!                      tc(ntasks) = c
!                      tj(ntasks) = j
!                      tb(ntasks) = b
!                      ti(ntasks) = i
!                      ta(ntasks) = a
!                      if(ntasks == max_ntasks) then
!                         !$omp parallel private(kk, eorb_diff) default(shared)
!                         !$omp do reduction(+:d_small, SIGROWS_CC3)
                        
!                         do kk = 1, ntasks
!                            eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tc(kk)) &
!                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(ti(kk))

!                            if (dav_converging_right()) then
!                               call z1_driver(ta(kk), ti(kk), tb(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                                    irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                    nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
!                            end if
!                         end do
!                         !$omp end do
!                         !$omp end parallel
!                         ntasks = 0
!                         tc = 0
!                         tj = 0
!                         tb = 0
!                         ti = 0 
!                         ta = 0

!                      end if
!                      ntasks = ntasks + 1

!                   end do
!                end do iloop01
!             end do
!          end do
!       end do
      
!       if (ntasks.gt.0)then
!              do kk = 1, ntasks-1
!                 eorb_diff = eorb(ta(kk))  + eorb(tb(kk)) + eorb(tc(kk)) &
!                      - eorb(ti(kk)) - eorb(tj(kk)) - eorb(ti(kk))
                
!                 if (dav_converging_right()) then
!                    call z1_driver(ta(kk), ti(kk), tb(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, SIGROWS_CC3)
!                 end if
!              end do
!           end if

! !$ time2 = omp_get_wtime() 

!           print*, 'time2-time1', time2-time1


!           ntasks = 1
!           ntt = 1
! !          print*, 'z2'
!             ! z2 aibjcj  a>b>c (ijj) j = k, j!=i                                                                      
!             do c = n0c, n1c
!                   do j = n0jk, n1jk
!                         b0 = max(c + 1, n0b)
!                         do b = b0, n1b
!                               iloop02: do i = n0i, n1i
!                                     if (i == j) cycle iloop02
!                                     a0 = max(b + 1, n0a)
!                                     do a = a0, n1a

!                                        tc(ntasks) = c
!                                        tj(ntasks) = j
!                                        tb(ntasks) = b
!                                        ti(ntasks) = i
!                                        ta(ntasks) = a
!                                        if(ntasks == max_ntasks) then
                                          
! !                                          !$omp parallel private(kk, eorb_diff) default(shared)
! !                                          !$omp do reduction(+:d_small, SIGROWS_CC3)
!                                           do kk = 1, ntasks
!                                             eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tc(kk)) &
!                                                  - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tj(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z2_driver(ta(kk), ti(kk), tb(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do iloop02
!                              end do
!                           end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(tb(kk)) + eorb(tc(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tj(kk))
                
!                 if (dav_converging_right()) then
!                    call z2_driver(ta(kk), ti(kk), tb(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if



!           ntasks = 1
!           ntt = 1
!           ! ! z34 aiajck aac (ijk) a>c, j < k, i > (j, k)
!           do k = n0k, n1k
!              do c = n0c, n1c
!                 j1 = min(k - 1, n1j)
!                 do j = n0j, j1
!                    i0 = max(n0i, k+1, j+1)
!                    do i = i0, n1i
!                       a0 = max(c + 1, n0ab)
!                       do a = a0, n1ab

                         
!                          tc(ntasks) = c
!                          tj(ntasks) = j
!                          tk(ntasks) = k
!                          ti(ntasks) = i
!                          ta(ntasks) = a
!                          if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                             do kk = 1, ntasks
!                                eorb_diff = eorb(ta(kk)) + eorb(ta(kk)) + eorb(tc(kk)) &
!                                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                               
!                                if (dav_converging_right()) then
!                                   call z34_driver(ta(kk), ti(kk), tj(kk), tc(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                                        irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                        nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                else
!                                   !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                                   !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                                   !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                   !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                                   !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                                   !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
!                                           !!$omp end do
!                                           !!$omp end parallel
!                                           ntasks = 0
!                                        end if
!                                        ntasks = ntasks + 1
!                                        ntt = ntt + 1
!                                     end do
!                                  end do
!                               end do
!                            end do
!                         end do
                        
!                         if (ntasks.gt.0)then
!                            do kk = 1, ntasks-1
!                               eorb_diff = eorb(ta(kk))  + eorb(ta(kk)) + eorb(tc(kk)) &
!                                    - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                              
!                               if (dav_converging_right()) then
!                                  call z34_driver(ta(kk), ti(kk), tj(kk), tc(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                                       irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                       nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                  ! else
!                                  !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                                  !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                                  !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                  !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                                  !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                                  !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                               end if
!                            end do
!                         end if
                        
                                    

!             ntasks = 1
!             ntt = 1

!             ! z34 aiajck  aac (ijk) a>c, j < k, i > j, i < k
!             do k = n0k, n1k
!                   do c = n0c, n1c
!                         j1 = min(k - 1, n1j)
!                         do j = n0j, j1
!                               i0 = max(n0i, j+1)
!                               i1 = min(n1i, k-1)
!                               do i = i0, i1
!                                     a0 = max(c + 1, n0ab)
!                                     do a = a0, n1ab
                                    
!                                        tc(ntasks) = c
!                                        tj(ntasks) = j
!                                        tk(ntasks) = k
!                                        ti(ntasks) = i
!                                        ta(ntasks) = a
!                                        if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(ta(kk)) + eorb(tc(kk)) &
!                                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z34_driver(ta(kk), ti(kk), tj(kk), tc(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                           end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(ta(kk)) + eorb(tc(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                
!                 if (dav_converging_right()) then
!                    call z34_driver(ta(kk), ti(kk), tj(kk), tc(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if


!           ntasks = 1
!           ntt = 1

!             !z5 abb (ijk) a>b, j > k, i > k, i < j
!             do k = n0k, n1k
!                   j0 = max(k + 1, n0j)
!                   do j = j0, n1j
!                         do b = n0bc, n1bc
!                               i0 = max(n0i, k+1)
!                               i1 = min(n1i, j-1)
!                               do i = i0, i1
!                                     a0 = max(b + 1, n0a)
!                                     do a = a0, n1a
                                       
                                    
!                                        tb(ntasks) = b
!                                        tj(ntasks) = j
!                                        tk(ntasks) = k
!                                        ti(ntasks) = i
!                                        ta(ntasks) = a
!                                        if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tb(kk)) &
!                                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z56_driver(ta(kk), ti(kk), tb(kk), tj(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                           end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(tb(kk)) + eorb(tb(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                
!                 if (dav_converging_right()) then
!                    call z56_driver(ta(kk), ti(kk), tb(kk), tj(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if

!             ntasks = 1
!             ntt = 1

!             ! z6 abb (ijk) a>b, j>k, i<j, i<k 
!             do k = n0k, n1k
!                   j0 = max(k + 1, n0j)
!                   do j = j0, n1j
!                         do b = n0bc, n1bc
!                               i1 = min(n1i, j-1, k-1)
!                               do i = n0i, i1
!                                     a0 = max(b + 1, n0a)
!                                     do a = a0, n1a
                                                                              
                                    
!                                        tb(ntasks) = b
!                                        tj(ntasks) = j
!                                        tk(ntasks) = k
!                                        ti(ntasks) = i
!                                        ta(ntasks) = a
!                                        if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tb(kk)) &
!                                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z56_driver(ta(kk), ti(kk), tb(kk), tj(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                           end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(tb(kk)) + eorb(tb(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tk(kk))
                
!                 if (dav_converging_right()) then
!                    call z56_driver(ta(kk), ti(kk), tb(kk), tj(kk), tk(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if


!             ntasks = 1
!             ntt = 1
!             ! z7 aac (iji) i = k, i > j                                                          
!             do c = n0c, n1c
!                   do j = n0j, n1j
!                         i0 = max(n0ik, j+1)
!                         do i = i0, n1ik
!                               a0 = max(c + 1, n0ab)
!                               do a = a0, n1ab

!                                  tc(ntasks) = c
!                                  tj(ntasks) = j
!                                  ti(ntasks) = i
!                                  ta(ntasks) = a
!                                  if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tc(kk)) &
!                                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(ti(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z7_driver(ta(kk), ti(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(ta(kk)) + eorb(tc(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(ti(kk))
                
!                 if (dav_converging_right()) then
!                    call z7_driver(ta(kk), ti(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if

!             ntasks = 1
!             ntt = 1

!             ! z8 aac (ijj) j = k, i > j                                                                                                                    
!             do c = n0c, n1c
!                   do j = n0jk, n1jk
!                         i0 = max(n0i, j+1)
!                         do i = i0, n1i
!                               a0 = max(c + 1, n0ab)
!                               do a = a0, n1ab
                                 
!                                  tc(ntasks) = c
!                                  tj(ntasks) = j
!                                  ti(ntasks) = i
!                                  ta(ntasks) = a
!                                  if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(ta(kk)) + eorb(tc(kk)) &
!                                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tj(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z8_driver(ta(kk), ti(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(ta(kk)) + eorb(tc(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(tj(kk))
                
!                 if (dav_converging_right()) then
!                    call z8_driver(ta(kk), ti(kk), tj(kk), tc(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if



!             ntasks = 1
!             ntt = 1

!             ! z9 abb (iji) a>b, i<j
!             do j = n0j, n1j
!                   do b = n0bc, n1bc
!                         i1 = min(n1ik, j-1)
!                         do i = n0ik, i1
!                               a0 = max(b + 1, n0a)
!                               do a = a0, n1a
                                 
!                                  tb(ntasks) = b
!                                  tj(ntasks) = j
!                                  ti(ntasks) = i
!                                  ta(ntasks) = a
!                                  if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tb(kk)) &
!                                                 - eorb(ti(kk)) - eorb(tj(kk)) - eorb(ti(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z9_driver(ta(kk), ti(kk), tb(kk), tj(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(tb(kk)) + eorb(tb(kk)) &
!                     - eorb(ti(kk)) - eorb(tj(kk)) - eorb(ti(kk))
                
!                 if (dav_converging_right()) then
!                    call z9_driver(ta(kk), ti(kk), tb(kk), tj(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if

!             ntasks = 1
!             ntt = 1

!             ! z10 abb (iij) a>b, i>j
!             do j = n0j, n1j
!                   do b = n0bc, n1bc
!                         i0 = max(n0ik, j+1)
!                         do i = i0, n1ik
!                               a0 = max(b + 1, n0a)
!                               do a = a0, n1a

!                                  tb(ntasks) = b
!                                  tj(ntasks) = j
!                                  ti(ntasks) = i
!                                  ta(ntasks) = a
!                                  if(ntasks == max_ntasks) then
                                          
! !!$omp parallel private(kk, eorb_diff) default(shared)
! !!$omp do reduction(+:d_small, SIGROWS_CC3)
!                                         do kk = 1, ntasks
!                                            eorb_diff = eorb(ta(kk)) + eorb(tb(kk)) + eorb(tb(kk)) &
!                                                 - eorb(ti(kk)) - eorb(ti(kk)) - eorb(tj(kk))
                                       
!                                              if (dav_converging_right()) then
!                                                call z10_driver(ta(kk), ti(kk), tb(kk), tj(kk), t2, isingles, idoubles, idims, idimd, &
!                                                     irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                                                     nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             else
! !                                                !    call z2_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
! !                                                !    call z2_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
! !                                                !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
! !                                                !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                                             end if
!                                           end do
! ! !                                         !$omp end do
! ! !                                         !$omp end parallel
!                                          ntasks = 0
!                                       end if
!                                       ntasks = ntasks + 1
!                                       ntt = ntt + 1
!                                    end do
!                                 end do
!                              end do
!                        end do

!          if (ntasks.gt.0)then
!             do kk = 1, ntasks-1
!                eorb_diff = eorb(ta(kk))  + eorb(tb(kk)) + eorb(tb(kk)) &
!                     - eorb(ti(kk)) - eorb(ti(kk)) - eorb(tj(kk))
                
!                 if (dav_converging_right()) then
!                    call z10_driver(ta(kk), ti(kk), tb(kk), tj(kk), t2, isingles, idoubles, idims, idimd, &
!                         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, &
!                         nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    ! else
!                    !    call z1_driver_13(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, MEM_VEC_ROWS, ntrial, ntrial, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                    !    call z1_driver_23(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
!                    !         irrep0, irrep1, MEM_VEC_ROWS, vdav_bar, ntrial, ntrial_bar, nocc, &
!                    !         nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!                 end if
!              end do
!           end if

            !print*, 'd_smal'
            !print*, d_small(1:ntrial, 1:ntrial)
            !print*, ''
            
            deallocate(idoubles)
            deallocate(isingles)
            !$ time_plusz1 = omp_get_wtime()
            print*, 'time na allllll', time_plusz1 - time_plusz0

contains
            
            subroutine  update_d_small(d_small, ntrial_bar, ntrial, sigma_l, sigma_r, eorb_diff)
                  
                  real(F64), dimension(:,:), intent(inout) :: d_small
                  integer, intent(in) :: ntrial_bar
                  integer, intent(in) :: ntrial
                  real(F64), dimension(:), intent(in) :: sigma_l
                  real(F64), dimension(:), intent(in) :: sigma_r
                  real(F64), intent(in) :: eorb_diff

                  integer :: p, q

                  do q = 1, ntrial
                        do p = 1, ntrial_bar
                              d_small(p, q) = d_small(p, q) + sigma_l(p) * sigma_r(q) !/ (eorb_diff - WRSMALL(1))
                        end do
                  end do
!                  !print*, 'EORBDIFF', eorb_diff - WRSMALL(1)
            end subroutine update_d_small


            ! subroutine comp_and_update_d_small(d_small, sigma_13, sigma_23, sigma_31, sigma_32, vdav_bar, vdav, ntrial_bar, ntrial, &
            !       a, i, b, j, c, k, eorb_diff)

            !       real(F64), dimension(:,:), intent(inout) :: d_small
            !       integer, intent(in) :: ntrial_bar
            !       integer, intent(in) :: ntrial
            !       real(F64), dimension(:,:), intent(in) :: vdav_bar, vdav
            !       real(F64), dimension(:), intent(inout) :: sigma_13, sigma_23
            !       real(F64), dimension(:), intent(inout) :: sigma_31, sigma_32
            !       integer, intent(in) :: a, i, b, j, c, k
            !       real(F64), intent(in) :: eorb_diff
            !       real(F64), dimension(:), intent(in) :: omega


            !       call sub_eom_cc3_13_mem_bjckdl(sigma_13, a, i, b, j, c, k, vdav_bar, ntrial_bar)
            !       call sub_eom_cc3_23_mem_bjckdl(sigma_23, a, i, b, j, c, k, vdav_bar, ntrial_bar)
            !       call sub_eom_cc3_31_mem_mem_bjckdl(sigma_31, a, i, b, j, c, k, vdav, ntrial)
            !       call sub_eom_cc3_32_mem_bjckdl(sigma_32, a, i, b, j, c, k, vdav, ntrial)
                  
            !       call update_d_small(d_small, ntrial_bar, ntrial, sigma_13, sigma_31, eorb_diff)
            !       call update_d_small(d_small, ntrial_bar, ntrial, sigma_13, sigma_32, eorb_diff)
            !       call update_d_small(d_small, ntrial_bar, ntrial, sigma_23, sigma_31, eorb_diff)
            !       call update_d_small(d_small, ntrial_bar, ntrial, sigma_23, sigma_32, eorb_diff)

            ! end subroutine comp_and_update_d_small



          end subroutine d_small_driver

          
          subroutine block_13_driver_noR(sigma_v1, sigma_v2, sigma_v3, sigma_v4, sigma_v5, &
               a, i, b, j, c, k, d_small, sigrows, &
               vdav, ntrial, nocc0, nvirt0, nocc, &
               n0d, n1d, n0l, n1l, eorb, wr)

            real(F64), dimension(:), intent(in) :: sigma_v1, sigma_v2
            real(F64), dimension(:), intent(in) :: sigma_v3, sigma_v4, sigma_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:, :), intent(inout) :: d_small
            real(F64), dimension(:, :), intent(inout) :: sigrows
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial, nocc0, nvirt0, nocc
            integer, intent(in) :: n0l, n1l, n0d, n1d
            real(F64), intent(in) :: eorb, wr

            call sub_eom_cc3_13_mem_noR_z0(a, i, b, k, c, j, sigrows,  d_small, &
                  sigma_v1, vdav, ntrial, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_13_mem_noR_z0(a, j, b, i, c, k, sigrows,  d_small, &
                  sigma_v2, vdav, ntrial, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_13_mem_noR_z0(a, j, b, k, c, i, sigrows,  d_small, &
                  sigma_v3, vdav, ntrial, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_13_mem_noR_z0(a, k, b, i, c, j, sigrows,  d_small, &
                  sigma_v4, vdav, ntrial, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_13_mem_noR_z0(a, k, b, j, c, i, sigrows,  d_small, &
                  sigma_v5, vdav, ntrial, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, eorb, wr)

      end subroutine block_13_driver_noR

      subroutine block_31_driver_noR(sigma_v1, sigma_v2, sigma_v3, sigma_v4, sigma_v5, &
            t2, nocc, nactive, a, i, b, j, c, k, d_small, sigrows, &
            vdav, ntrial, nocc0, nvirt0, &
            n0d, n1d, n0l, n1l, eorb, wr)

            real(F64), dimension(:), intent(in) :: sigma_v1, sigma_v2
            real(F64), dimension(:), intent(in) :: sigma_v3, sigma_v4, sigma_v5
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, intent(in) :: nocc, nactive
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:, :), intent(inout) :: d_small
            real(F64), dimension(:, :), intent(inout) :: sigrows
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial, nocc0, nvirt0
            integer, intent(in) :: n0l, n1l, n0d, n1d
            real(F64), intent(in) :: eorb, wr

            call sub_eom_cc3_31_mem_noR_vp_z0(t2, nocc, nactive, a, i, b, k, c, j, sigrows,  d_small, &
                  sigma_v1, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_31_mem_noR_vp_z0(t2, nocc, nactive, a, j, b, i, c, k, sigrows,  d_small, &
                  sigma_v2, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_31_mem_noR_vp_z0(t2, nocc, nactive, a, j, b, k, c, i, sigrows,  d_small, &
                  sigma_v3, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_31_mem_noR_vp_z0(t2, nocc, nactive, a, k, b, i, c, j, sigrows,  d_small, &
                  sigma_v4, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
            call sub_eom_cc3_31_mem_noR_vp_z0(t2, nocc, nactive, a, k, b, j, c, i, sigrows,  d_small, &
                  sigma_v5, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)

      end subroutine block_31_driver_noR

       subroutine block_32_driver_noR(sigma_v1, sigma_v2, sigma_v3, sigma_v4, sigma_v5, &
             nactive, a, i, b, j, c, k, d_small, sigrows, &
             vdav, ntrial, npair, nocc0, nvirt0, nocc, &
             n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m, eorb, wr)

            real(F64), dimension(:), intent(in) :: sigma_v1, sigma_v2
            real(F64), dimension(:), intent(in) :: sigma_v3, sigma_v4, sigma_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:, :), intent(inout) :: d_small, sigrows
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial, npair, nocc0, nvirt0, nocc
            integer, intent(in) :: n0l, n1l, n0d, n1d, n0e, n1e, n0m, n1m
            real(F64), intent(in) :: eorb, wr
            integer, intent(in) :: nactive

            call sub_eom_cc3_32_mem_noR_vp_z0(a, i, b, k, c, j, sigrows,  d_small, &
                  sigma_v1, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,  n0e, n1e, n0m, n1m, eorb, wr)
            call sub_eom_cc3_32_mem_noR_vp_z0(a, j, b, i, c, k, sigrows,  d_small, &
                  sigma_v2, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,  n0e, n1e, n0m, n1m, eorb, wr)
            call sub_eom_cc3_32_mem_noR_vp_z0(a, j, b, k, c, i, sigrows,  d_small, &
                  sigma_v3, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,  n0e, n1e, n0m, n1m, eorb, wr)
            call sub_eom_cc3_32_mem_noR_vp_z0(a, k, b, i, c, j, sigrows,  d_small, &
                  sigma_v4, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,  n0e, n1e, n0m, n1m, eorb, wr)
            call sub_eom_cc3_32_mem_noR_vp_z0(a, k, b, j, c, i, sigrows,  d_small, &
                  sigma_v5, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,  n0e, n1e, n0m, n1m, eorb, wr)

      end subroutine block_32_driver_noR

         subroutine block_23_driver_noR(sigma_v1, sigma_v2, sigma_v3, sigma_v4, sigma_v5, &
               a, i, b, j, c, k, d_small, sigrows, &
               vdav_bar, ntrial_bar, npair, nocc0, nvirt0, nocc, &
               n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m, eorb, wr)

            real(F64), dimension(:), intent(in) :: sigma_v1, sigma_v2
            real(F64), dimension(:), intent(in) :: sigma_v3, sigma_v4, sigma_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:, :), intent(inout) :: d_small
            real(F64), dimension(:, :), intent(inout) :: sigrows
            real(F64), dimension(:,:), intent(in) :: vdav_bar
            integer, intent(in) :: ntrial_bar, npair, nocc0, nvirt0, nocc
            integer, intent(in) :: n0l, n1l, n0d, n1d, n0e, n1e, n0m, n1m
            real(F64), intent(in) :: eorb, wr

            call sub_eom_cc3_23_mem_noR_z0(a, i, b, k, c, j, sigrows,  d_small, &
                  sigma_v1, vdav_bar, ntrial_bar, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, n0e, n1e, n0m, n1m,  eorb, wr)
            call sub_eom_cc3_23_mem_noR_z0(a, j, b, i, c, k, sigrows,  d_small, &
                  sigma_v2, vdav_bar, ntrial_bar, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, n0e, n1e, n0m, n1m,  eorb, wr)
            call sub_eom_cc3_23_mem_noR_z0(a, j, b, k, c, i, sigrows,  d_small, &
                  sigma_v3, vdav_bar, ntrial_bar, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, n0e, n1e, n0m, n1m,  eorb, wr)
            call sub_eom_cc3_23_mem_noR_z0(a, k, b, i, c, j, sigrows,  d_small, &
                  sigma_v4, vdav_bar, ntrial_bar, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, n0e, n1e, n0m, n1m,  eorb, wr)
            call sub_eom_cc3_23_mem_noR_z0(a, k, b, j, c, i, sigrows,  d_small, &
                  sigma_v5, vdav_bar, ntrial_bar, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l, n0e, n1e, n0m, n1m,  eorb, wr)

          end subroutine block_23_driver_noR


          subroutine block_31_driver(sigma_31_v1, sigma_31_v2, sigma_31_v3, sigma_31_v4, sigma_31_v5, &
               t2, nocc, nactive, a, i, b, j, c, k, vdav, ntrial, &
               n0d, n1d, n0l, n1l)

            real(F64), dimension(:), intent(out) :: sigma_31_v1, sigma_31_v2, sigma_31_v3
            real(F64), dimension(:), intent(out) :: sigma_31_v4, sigma_31_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, intent(in) :: nocc, nactive
            integer, intent(in) :: n0l, n1l, n0d, n1d
            real(F64), dimension(:), allocatable :: part_v1, part_v2, part_v3, part_v4, part_v5

            allocate(part_v1(ntrial))
            allocate(part_v2(ntrial))
            allocate(part_v3(ntrial))
            allocate(part_v4(ntrial))
            allocate(part_v5(ntrial))


            call sub_eom_cc3_31_mem_vp_z0(part_v1, t2, nocc, nactive, a, i, b, k, c, j, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_31_mem_vp_z0(part_v2, t2, nocc, nactive, a, j, b, i, c, k, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_31_mem_vp_z0(part_v3, t2, nocc, nactive, a, j, b, k, c, i, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_31_mem_vp_z0(part_v4, t2, nocc, nactive, a, k, b, i, c, j, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_31_mem_vp_z0(part_v5, t2, nocc, nactive, a, k, b, j, c, i, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)

            sigma_31_v1 = one/four * part_v1 + one/twelve * part_v2 &
                 + one/six * part_v3 + one/six* part_v4 &
                 +one/twelve * part_v5
            sigma_31_v2 = one/twelve * part_v1 + one/four * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/twelve* part_v5
            sigma_31_v3 = one/six * part_v1 + one/six * part_v2 &
                 +one/three * part_v3 + one/six* part_v4 &
                 +one/six * part_v5
            sigma_31_v4 = one/six * part_v1 + one/six * part_v2 &
                 +one/six * part_v3 + one/three* part_v4 &
                 +one/six * part_v5
            sigma_31_v5 = one/twelve * part_v1 + one/twelve * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/four* part_v5

            deallocate(part_v1)
            deallocate(part_v2)
            deallocate(part_v3)
            deallocate(part_v4)
            deallocate(part_v5)



          end subroutine block_31_driver



          subroutine block_32_driver(sigma_32_v1, sigma_32_v2, sigma_32_v3, sigma_32_v4, sigma_32_v5, &
               a, i, b, j, c, k, vdav, ntrial, npair, nocc, nocc0, nvirt0, &
               n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)

            real(F64), dimension(:), intent(out) :: sigma_32_v1, sigma_32_v2, sigma_32_v3
            real(F64), dimension(:), intent(out) :: sigma_32_v4, sigma_32_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial, npair, nocc, nocc0, nvirt0
            integer, intent(in) :: n0l, n1l, n0d, n1d, n0m, n1m, n0e, n1e
            real(F64), dimension(:), allocatable :: part_v1, part_v2, part_v3, part_v4, part_v5

            allocate(part_v1(ntrial))
            allocate(part_v2(ntrial))
            allocate(part_v3(ntrial))
            allocate(part_v4(ntrial))
            allocate(part_v5(ntrial))


            call sub_eom_cc3_32_mem_vp_z0(part_v1, a, i, b, k, c, j, vdav, ntrial, &
                   npair, nocc, nocc0, nvirt0, n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_32_mem_vp_z0(part_v2, a, j, b, i, c, k, vdav, ntrial, &
                   npair, nocc, nocc0, nvirt0, n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_32_mem_vp_z0(part_v3, a, j, b, k, c, i, vdav, ntrial, &
                   npair, nocc, nocc0, nvirt0, n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_32_mem_vp_z0(part_v4, a, k, b, i, c, j, vdav, ntrial, &
                   npair, nocc, nocc0, nvirt0, n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_32_mem_vp_z0(part_v5, a, k, b, j, c, i, vdav, ntrial, &
                   npair, nocc, nocc0, nvirt0, n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)


            sigma_32_v1 = one/four * part_v1 + one/twelve * part_v2 &
                 +one/six* part_v3 + one/six* part_v4 &
                 +one/twelve * part_v5
            sigma_32_v2 = one/twelve * part_v1 + one/four * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/twelve* part_v5
            sigma_32_v3 = one/six * part_v1 + one/six * part_v2 &
                 +one/three * part_v3 + one/six* part_v4 &
                 +one/six * part_v5
            sigma_32_v4 = one/six * part_v1 + one/six * part_v2 &
                 +one/six * part_v3 + one/three* part_v4 &
                 +one/six * part_v5
            sigma_32_v5 = one/twelve * part_v1 + one/twelve * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/four* part_v5

            deallocate(part_v1)
            deallocate(part_v2)
            deallocate(part_v3)
            deallocate(part_v4)
            deallocate(part_v5)

            
          end subroutine block_32_driver

          subroutine block_13_driver(sigma_13_v1, sigma_13_v2, sigma_13_v3, sigma_13_v4, sigma_13_v5, &
               t2, nocc, nactive, a, i, b, j, c, k, vdav, ntrial, &
               n0d, n1d, n0l, n1l)

            real(F64), dimension(:), intent(out) :: sigma_13_v1, sigma_13_v2, sigma_13_v3
            real(F64), dimension(:), intent(out) :: sigma_13_v4, sigma_13_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, intent(in) :: nocc, nactive
            integer, intent(in) :: n0l, n1l, n0d, n1d
                        real(F64), dimension(:), allocatable :: part_v1, part_v2, part_v3, part_v4, part_v5

            allocate(part_v1(ntrial))
            allocate(part_v2(ntrial))
            allocate(part_v3(ntrial))
            allocate(part_v4(ntrial))
            allocate(part_v5(ntrial))




            call sub_eom_cc3_13_mem_z0(part_v1, a, i, b, k, c, j, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_13_mem_z0(part_v2, a, j, b, i, c, k, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_13_mem_z0(part_v3, a, j, b, k, c, i, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_13_mem_z0(part_v4, a, k, b, i, c, j, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)
            call sub_eom_cc3_13_mem_z0(part_v5, a, k, b, j, c, i, vdav, ntrial, &
                 n0d, n1d, n0l, n1l)

            sigma_13_v1 = one/four * part_v1 + one/twelve *part_v2 &
                 +one/six* part_v3 + one/six* part_v4 &
                 +one/twelve * part_v5
            sigma_13_v2 = one/twelve * part_v1 + one/four * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/twelve* part_v5
            sigma_13_v3 = one/six * part_v1 + one/six * part_v2 &
                 +one/three * part_v3 + one/six* part_v4 &
                 +one/six * part_v5
            sigma_13_v4 = one/six * part_v1 + one/six * part_v2 &
                 +one/six * part_v3 + one/three* part_v4 &
                 +one/six * part_v5
            sigma_13_v5 = one/twelve * part_v1 + one/twelve * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/four* part_v5
            deallocate(part_v1)
            deallocate(part_v2)
            deallocate(part_v3)
            deallocate(part_v4)
            deallocate(part_v5)


      end subroutine block_13_driver

         subroutine block_23_driver(sigma_23_v1, sigma_23_v2, sigma_23_v3, sigma_23_v4, sigma_23_v5, &
               a, i, b, j, c, k, vdav, ntrial, nocc, &
               n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)

            real(F64), dimension(:), intent(out) :: sigma_23_v1, sigma_23_v2, sigma_23_v3
            real(F64), dimension(:), intent(out) :: sigma_23_v4, sigma_23_v5
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:,:), intent(in) :: vdav
            integer, intent(in) :: ntrial, nocc
            integer, intent(in) :: n0l, n1l, n0d, n1d, n0m, n1m, n0e, n1e
            real(F64), dimension(:), allocatable :: part_v1, part_v2, part_v3, part_v4, part_v5

            allocate(part_v1(ntrial))
            allocate(part_v2(ntrial))
            allocate(part_v3(ntrial))
            allocate(part_v4(ntrial))
            allocate(part_v5(ntrial))




            call sub_eom_cc3_23_mem_z0(part_v1, nocc, a, i, b, k, c, j, vdav, ntrial, &
                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_23_mem_z0(part_v2, nocc, a, j, b, i, c, k, vdav, ntrial, &
                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_23_mem_z0(part_v3, nocc, a, j, b, k, c, i, vdav, ntrial, &
                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_23_mem_z0(part_v4, nocc, a, k, b, i, c, j, vdav, ntrial, &
                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
            call sub_eom_cc3_23_mem_z0(part_v5, nocc, a, k, b, j, c, i, vdav, ntrial, &
                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)

            sigma_23_v1 = one/four * part_v1 + one/twelve *part_v2 &
                 +one/six* part_v3 + one/six* part_v4 &
                 +one/twelve * part_v5
            sigma_23_v2 = one/twelve * part_v1 + one/four * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/twelve* part_v5
            sigma_23_v3 = one/six * part_v1 + one/six * part_v2 &
                 +one/three * part_v3 + one/six* part_v4 &
                 +one/six * part_v5
            sigma_23_v4 = one/six * part_v1 + one/six * part_v2 &
                 +one/six * part_v3 + one/three* part_v4 &
                 +one/six * part_v5
            sigma_23_v5 = one/twelve * part_v1 + one/twelve * part_v2 &
                 +one/six * part_v3 + one/six* part_v4 &
                 +one/four* part_v5

            deallocate(part_v1)
            deallocate(part_v2)
            deallocate(part_v3)
            deallocate(part_v4)
            deallocate(part_v5)

          end subroutine block_23_driver


          subroutine z0_driver(a, i, b, j, c, k, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, ntrial, nocc, nocc0, nvirt0, nactive, &
               npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, b, j, c, k
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav
            integer, intent(in) :: ntrial
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows
            real(F64), dimension(:), allocatable :: sig 

            integer :: kjr, kjl, bjl, bjr, bj
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e
            real(F64), dimension(:), allocatable :: sig_v1, sig_v2, sig_v3, sig_v4, sig_v5
                        
            allocate(sig_v1(ntrial))
            allocate(sig_v2(ntrial))
            allocate(sig_v3(ntrial))
            allocate(sig_v4(ntrial))
            allocate(sig_v5(ntrial))
            sig_v1 = zero
            sig_v2 = zero
            sig_v3 = zero
            sig_v4 = zero
            sig_v5 = zero
            
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive

            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive
            ! do kjr = 1, idims
            !       call loop_boundaries_sp(isingles(1:2, kjr), irrep0, irrep1, &
            !            nr0l, nr1l, nr0d, nr1d)
                  
                  call block_31_driver(sig_v1, sig_v2, sig_v3, &
                       sig_v4, sig_v5, &
                       t2, nocc, nactive, a, i, b, j, c, k, VECROWS, ntrial, &
                       nr0d, nr1d, nr0l, nr1l)
                  
                  ! call block_13_driver_noR(sig_v1,  sig_v2, sig_v3, &
                  !      sig_v4, sig_v5, &
                  !      a, i, b, j, c, k, d_small,  sigrows, &
                  !      VECROWS, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
                  
                  call block_23_driver_noR(sig_v1,  sig_v2, sig_v3, &
                       sig_v4, sig_v5, &
                       a, i, b, j, c, k, d_small,  sigrows, &
                       VECROWS, ntrial, npair, nocc0, nvirt0, nocc, &
                       nl0d,nl1d,nl0l,nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
               ! end do

               nr0l = 1
               nr1l = nocc
               nr0d = nvirt0
               nr1d = nactive
               nr0m = 1
               nr1m = nocc
               nr0e = nvirt0
               nr1e = nactive

               sig_v1 = zero
               sig_v2 = zero
               sig_v3 = zero
               sig_v4 = zero
               sig_v5 = zero
               ! do bj = 1, idimd                                                                                                   
               !       call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &                                                 
               !             nr0l, nr1l, nr0d, nr1d)                                                                                    
               !       call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &                                                 
               !             nr0m, nr1m, nr0e, nr1e)    
                     
               call block_32_driver(sig_v1, sig_v2, sig_v3, &
                    sig_v4, sig_v5, &
                    a, i, b, j, c, k, VECROWS, ntrial, npair, nocc, nocc0, nvirt0, &
                    nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
               call block_13_driver_noR(sig_v1,  sig_v2, sig_v3, &
                    sig_v4, sig_v5, &
                    a, i, b, j, c, k, d_small,  sigrows, &
                    VECROWS, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))               
               call block_23_driver_noR(sig_v1,  sig_v2, sig_v3, &
                    sig_v4, sig_v5, &
                    a, i, b, j, c, k, d_small,  sigrows, &
                    VECROWS, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d,nl1d,nl0l,nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
         ! end do

               
               deallocate(sig_v1)
               deallocate(sig_v2)
               deallocate(sig_v3)
               deallocate(sig_v4)
               deallocate(sig_v5)

          end subroutine z0_driver

          
          subroutine z1_driver(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, ntrial, nocc, nocc0, nvirt0, nactive, &
               npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, b, j, c
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav
            integer, intent(in) :: ntrial
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows
            real(F64), dimension(:), allocatable :: sig 

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive
            allocate(sig(ntrial))

            sig = zero
            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)

               call sub_eom_cc3_31_mem_v0_z1(sig, t2, nocc, nactive, a, i, b, j, c, &
                    vdav, ntrial, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z1(a, i, b, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_23_mem_noR_z1(a, i, b, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do
            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive
               call sub_eom_cc3_32_mem_v0_z1(sig, a, i, b, j, c, vdav, ntrial, &
                    npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
               call sub_eom_cc3_23_mem_noR_z1(a, i, b, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_13_mem_noR_z1(a, i, b, j, c, sigrows,  d_small, &                    
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)
          end subroutine z1_driver
         
          subroutine z2_driver(a, i, b, j, c, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, &
               nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, b, j, c
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig

            allocate(sig(ntrial))
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive

            sig = zero
            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               call sub_eom_cc3_31_mem_v0_z2(sig, t2, nocc, nactive, a, i, b, j, c, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z2(a, i, b, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))              
               call sub_eom_cc3_23_mem_noR_z2(a, i, b, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do
            
            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive

            call sub_eom_cc3_32_mem_v0_z2(sig, a, i, b, j, c, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z2(a, i, b, j, c, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z2(a, i, b, j, c, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)

          end subroutine z2_driver

          subroutine z34_driver(a, i, j, c, k, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, j, c, k
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive
            allocate(sig(ntrial))

            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               call sub_eom_cc3_31_mem_v6_z34(sig, t2, nocc, nactive, a, i, j, c, k, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z34(a, i, j, c, k, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_23_mem_noR_z34(a, i, j, c, k, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do

            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive

            call sub_eom_cc3_32_mem_v6_z34(sig, a, i, j, c, k, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z34(a, i, j, c, k, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z34(a, i, j, c, k, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)

          end subroutine z34_driver


          subroutine z56_driver(a, i, b, j, k, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, b, j, k
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig
            allocate(sig(ntrial))
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive


            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               call sub_eom_cc3_31_mem_v6_z56(sig, t2, nocc, nactive, a, i, b, j, k, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z56(a, i, b, j, k, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_23_mem_noR_z56(a, i, b, j, k, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do

            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive
            
            call sub_eom_cc3_32_mem_v6_z56(sig, a, i, b, j, k, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z56(a, i, b, j, k, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z56(a, i, b, j, k, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)
            
          end subroutine z56_driver



          subroutine z7_driver(a, i, j, c, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, j, c
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig
            allocate(sig(ntrial))
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive


            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               ! compute A31 * R1
               call sub_eom_cc3_31_mem_v06_z7(sig, t2, nocc, nactive, a, i, j, c, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               ! Compute A13 and multiply by A31 * R1
               call sub_eom_cc3_13_mem_noR_z7(a, i, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               ! Compute A23 and multiply by A31 * R1
               call sub_eom_cc3_23_mem_noR_z7(a, i, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do
            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive
            call sub_eom_cc3_32_mem_v06_z7(sig, a, i, j, c, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z7(a, i, j, c, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z7(a, i, j, c, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)
            
          end subroutine z7_driver

          subroutine z8_driver(a, i, j, c, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, j, c
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig
            allocate(sig(ntrial))
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive


            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               call sub_eom_cc3_31_mem_v06_z8(sig, t2, nocc, nactive, a, i, j, c, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z8(a, i, j, c, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_23_mem_noR_z8(a, i, j, c, sigrows,  d_small, &
                       sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                       nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do
            
            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive
            call sub_eom_cc3_32_mem_v06_z8(sig, a, i, j, c, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z8(a, i, j, c, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z8(a, i, j, c, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)
            
          end subroutine z8_driver


          subroutine z9_driver(a, i, b, j, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, b, j
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig

            allocate(sig(ntrial))
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive


            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               call sub_eom_cc3_31_mem_v06_z9(sig, t2, nocc, nactive, a, i, b, j, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z9(a, i, b, j, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_23_mem_noR_z9(a, i, b, j, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                       nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do

            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive
            
            call sub_eom_cc3_32_mem_v06_z9(sig, a, i, b, j, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z9(a, i, b, j, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z9(a, i, b, j, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)
            
          end subroutine z9_driver

          
          subroutine z10_driver(a, i, b, j, t2, isingles, idoubles, idims, idimd, &
               irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small, sigrows)
            integer, intent(in) :: a, i, b, j
            real(F64), dimension(:,:,:,:), intent(in) :: t2
            integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
            integer, intent(in) :: idims, idimd
            real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
            integer, intent(in) :: ntrial, ntrial_bar
            integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
            real(F64), intent(in) :: eorb_diff
            real(F64), dimension(:,:), intent(inout) :: d_small
            real(F64), dimension(:,:), intent(inout) :: sigrows

            integer :: kjr, kjl, bjl, bjr
            integer :: nr0l, nr1l, nr0d, nr1d, nr0m, nr1m, nr0e, nr1e
            integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

            real(F64), dimension(:), allocatable :: sig
            allocate(sig(ntrial))
            nl0l = 1
            nl1l = nocc
            nl0d = nvirt0
            nl1d = nactive
            nl0m = 1
            nl1m = nocc
            nl0e = nvirt0
            nl1e = nactive


            do kjr = 1, idims
               call loop_boundaries_sp(isingles(1:2,kjr), irrep0, irrep1, &
                    nr0l, nr1l, nr0d, nr1d)
               call sub_eom_cc3_31_mem_v06_z10(sig, t2, nocc, nactive, a, i, b, j, &
                    vdav_bar, ntrial_bar, &
                    nr0d, nr1d, nr0l, nr1l)
               call sub_eom_cc3_13_mem_noR_z10(a, i, b, j, sigrows,  d_small, &
                    sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
               call sub_eom_cc3_23_mem_noR_z10(a, i, b, j, sigrows,  d_small, &
                    sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                    nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            end do
            sig = zero
            nr0l = 1
            nr1l = nocc
            nr0d = nvirt0
            nr1d = nactive
            nr0m = 1
            nr1m = nocc
            nr0e = nvirt0
            nr1e = nactive

                  
            call sub_eom_cc3_32_mem_v06_z10(sig, a, i, b, j, vdav_bar, ntrial_bar, &
                 npair, nocc, nocc0, nvirt0, nr0d, nr1d, nr0l, nr1l, nr0e, nr1e, nr0m, nr1m)
            call sub_eom_cc3_23_mem_noR_z10(a, i, b, j, sigrows,  d_small, &
                 sig, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
                 nl0d, nl1d, nl0l, nl1l, nl0e, nl1e, nl0m, nl1m, eorb_diff, WRSMALL(1))
            call sub_eom_cc3_13_mem_noR_z10(a, i, b, j, sigrows,  d_small, &
                 sig, vdav, ntrial, nocc0, nvirt0, nocc, nl0d,nl1d,nl0l,nl1l, eorb_diff, WRSMALL(1))
            deallocate(sig)
            
          end subroutine z10_driver



          

!           subroutine z1_driver_13(a, i, b, j, c, isingles, idoubles, idims, idimd, &
!                irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!             integer, intent(in) :: a, i, b, j, c
!             integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
!             integer, intent(in) :: idims, idimd
!             real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
!             integer, intent(in) :: ntrial, ntrial_bar
!             integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
!             real(F64), intent(in) :: eorb_diff
!             real(F64), dimension(:,:), intent(inout) :: d_small

!             integer :: kjr, kjl, bjl
!             integer :: nr0l, nr1l, nr0d, nr1d
!             integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

!             real(F64), dimension(:), allocatable :: sigma_13

!             allocate(sigma_13(ntrial))
!             deallocate(sigma_13)


!                                               else 

!                                           do kj = 1, idims
!                                                 call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
!                                                       n0l, n1l, n0d, n1d)
!                                           call sub_eom_cc3_13_mem_z1(sigma_13(:, kj), a, i, b, j, c, vdav_bar, ntrial_bar, &
!                                                 n0d, n1d, n0l, n1l)
!                                           call sub_eom_cc3_31_mem_noR_v0_z1(t2, nocc, nactive, a, i, b, j, c, sigrows,  d_small, &
!                                                 sigma_13(:, kj), vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb_diff, WRSMALL(1))
!                                           do bj = 1, idimd
!                                                 call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
!                                                       n0l, n1l, n0d, n1d)
!                                                 call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
!                                                       n0m, n1m, n0e, n1e)                                         
!                                                 call sub_eom_cc3_32_mem_noR_v0_z1(nactive, a, i, b, j, c, SIGROWS_CC3,  d_small, &
!                                                       sigma_13(:, kj), vdav, ntrial, npair, nocc0, nvirt0, nocc, &
!                                                       n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m, eorb_diff, WRSMALL(1))
!                                           end do
!                                     end do
!                                           bj = 1
!                                           n0l = 1
!                                           n1l = nocc
!                                           n0d = nvirt0
!                                           n1d = nactive
!                                           n0m = 1
!                                           n1m = nocc
!                                           n0e = nvirt0
!                                           n1e = nactive
!                                     ! do bj = 1, idimd
!                                     !       call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
!                                     !             n0l, n1l, n0d, n1d)
!                                     !       call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
!                                     !             n0m, n1m, n0e, n1e)
!                                           call sub_eom_cc3_23_mem_z1(sigma_23, nocc, a, i, b, j, c, vdav_bar, ntrial_bar, &
!                                                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
!                                           call sub_eom_cc3_32_mem_noR_v0_z1(a, i, b, j, c, SIGROWS_CC3,  d_small, &
!                                                 sigma_23, vdav, ntrial, npair, nocc0, nvirt0, nocc, &
!                                                 n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m, eorb_diff, WRSMALL(1))
!                                           ! do kj = 1, idims
!                                           !       call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
!                                           !             n0l, n1l, n0d, n1d)
!                                                 call sub_eom_cc3_31_mem_noR_v0_z1(t2, nocc, nactive, a, i, b, j, c, SIGROWS_CC3,  d_small, &
!                                                       sigma_23, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb_diff, WRSMALL(1))
!                                     !       end do
!                                     ! end do
!                                     end if


!                                        !    call sub_eom_cc3_23_mem_z1(sigma_23, a, i, b, j, c, vdav, ntrial, &
!                                        !         n0d, n1d, n0l, n1l, n0e, n1e, n0m, n1m)
!                                        !    call update_d_small(d_small, ntrial_bar, ntrial, sigma_23, sigma_32, eorb_diff)

!                                        !    do kj = 1, idims
!                                        !       call update_d_small(d_small, ntrial_bar, ntrial, sigma_13(:, kj), sigma_32, eorb_diff)
!                                        !       call update_d_small(d_small, ntrial_bar, ntrial, sigma_23, sig(:, kj), eorb_diff)
!                                        !    end do
!                                        ! end do


!           end subroutine z1_driver_13

          
!           subroutine z1_driver_23(a, i, b, j, c, isingles, idoubles, idims, idimd, &
!                irrep0, irrep1, vdav, vdav_bar, ntrial, ntrial_bar, nocc, nocc0, nvirt0, nactive, npair, eorb_diff, d_small)
!             integer, intent(in) :: a, i, b, j, c
!             integer, dimension(:,:), intent(in) :: isingles, idoubles, irrep0, irrep1
!             integer, intent(in) :: idims, idimd
!             real(F64), dimension(:, :), intent(in) :: vdav, vdav_bar
!             integer, intent(in) :: ntrial, ntrial_bar
!             integer, intent(in) :: nocc, nocc0, nvirt0, nactive, npair
!             real(F64), intent(in) :: eorb_diff
!             real(F64), dimension(:,:), intent(inout) :: d_small

!             integer :: kjr, kjl, bjl
!             integer :: nr0l, nr1l, nr0d, nr1d
!             integer :: nl0l, nl1l, nl0d, nl1d, nl0m, nl1m, nl0e, nl1e

!             real(F64), dimension(:), allocatable :: sigma_23

!             allocate(sigma_23(ntrial))
            
!             deallocate(sigma_23)

!           end subroutine z1_driver_23

  



    end module jacobian_d_small
