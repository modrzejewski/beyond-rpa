module jacobian

  use gparam
  use linalg
  use t1_transformed_int
  use math_constants
  use arithmetic
  use cc_gparams
  use ccjac_block_11_dav
  use ccjac_block_12_dav
  use ccjac_block_13_dav
  use ccjac_block_21_dav
  use ccjac_block_21_cc3_dav
  use ccjac_block_22_dav
  use ccjac_block_23_dav
  use ccjac_block_31_dav
  use ccjac_block_32_dav
  use ccjac_block_33_dav
  use ccjac_block_33_dav_mem
  use symmetry
  use jacobian_triplet

  use cc3_intermediates_for_21
  use davidson_main
  use threads
  


  implicit none

 integer, private :: bintv, binto
  integer, private :: kintv, kinto
integer, private, parameter :: MAXT = 100

  save


contains

!       subroutine calculate_r3dim_small(nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1, &
!             irrep0, irrep1, r3dim_small, all_ttasks)
!             integer, intent(in)                              :: nocc, nvirt
!             integer, intent(in)                              :: nactive
!             integer, intent(in)                              :: nocc0, nocc1
!             integer, intent(in)                              :: nvirt0, nvirt1
!             integer, dimension(:, :), intent(in)             :: irrep0
!             integer, dimension(:, :), intent(in)             :: irrep1
!             integer, intent(out) :: r3dim_small
!             integer, intent(out) :: all_ttasks
            
!             integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
!             integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
!             integer                                 :: pa, pb, pc, pd, pi, pj, pk, pl
!             integer :: nthreads
!             integer :: max_ntasks
!             integer :: ntasks
!             integer :: aq
!             integer :: m0a, m1a, m0b, m1b
!             integer :: m0c, m1c, m0d, m1d, m0e, m1e
!             integer :: m0i, m1i, m0j, m1j
!             integer :: m0k, m1k, m0l, m1l, m0m, m1m
!             integer :: bj, kj
!             integer :: kk, offset, k, aw
!             integer, dimension(:,:), allocatable :: itriples
!             integer :: idimt


!             ! bintv = CC_bi6
!             ! binto = 5
!             ! kintv = 6
!             ! kinto = 5           
            
!             nthreads = OMP_NTHREAD
!             max_ntasks = MAXT * nthreads
            
!             allocate(n0a(max_ntasks))
!             allocate(n1a(max_ntasks))
!             allocate(n0b(max_ntasks))
!             allocate(n1b(max_ntasks))
!             allocate(n0c(max_ntasks))
!             allocate(n1c(max_ntasks))
!             allocate(n0d(max_ntasks))
!             allocate(n1d(max_ntasks))
!             allocate(n0i(max_ntasks))
!             allocate(n1i(max_ntasks))
!             allocate(n0j(max_ntasks))
!             allocate(n1j(max_ntasks))
!             allocate(n0k(max_ntasks))
!             allocate(n1k(max_ntasks))
!             allocate(n0l(max_ntasks))
!             allocate(n1l(max_ntasks))



!             call irrep_triples(6, irrep0, irrep1, nocc0, nocc, nvirt0, &
!                   itriples, POINT_GROUP, idimt, .true., .false.)
!             allocate(itriples(6, idimt))
!             call irrep_triples(6, irrep0, irrep1, nocc0, nocc, nvirt0, &
!                   itriples, POINT_GROUP, idimt, .false., .false.)
            
!             all_ttasks = 0
!             r3dim_small = 0
!             ntasks = 0
!             do bj = 1, idimt
!                   call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
!                         m0i, m1i, m0a, m1a)
!                   call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
!                         m0j, m1j, m0b, m1b)
!                   call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
!                         m0k, m1k, m0c, m1c)
                  
!                   ntasks = ntasks + 1


!                                n0b(ntasks) = m0b
!                                n1b(ntasks) = m1b

!                                n0c(ntasks) = m0c
!                                n1c(ntasks) = m1c

!                                n0a(ntasks) = m0a
!                                n1a(ntasks) = m1a

!                                n0j(ntasks) = m0j
!                                n1j(ntasks) = m1j

!                                n0k(ntasks) = m0k
!                                n1k(ntasks) = m1k

!                                n0i(ntasks) = m0i
!                                n1i(ntasks) = m1i


!                                n0d(ntasks) = m0d
!                                n1d(ntasks) = m1d
!                                n0l(ntasks) = m0l
!                                n1l(ntasks) = m1l


                               
!                                if (ntasks == max_ntasks)  then
!                                      call dotasks_31_count(nocc0, nocc1, nvirt0, nvirt1, &
!                                            n0a, n1a, n0b, n1b, n0c, n1c,  &
!                                            n0i, n1i, n0j, n1j, n0k, n1k, ntasks, aq)
!                                      r3dim_small = r3dim_small + aq
!                                      ntasks = 0
!                                      stop
!                                end if
!                                all_ttasks = all_ttasks + 1
!                          end do

!     if(ntasks .gt. 0)then
!           call dotasks_31_count(nocc0, nocc1, nvirt0, nvirt1, &
!                 n0a, n1a, n0b, n1b, n0c, n1c,  &
!                 n0i, n1i, n0j, n1j, n0k, n1k, ntasks, aq)
!           r3dim_small = r3dim_small + aq
!     end if

!     print*, 'ntasks', ntasks, max_ntasks
!     print*, 'all_tasks', all_ttasks
!     print*, r3dim_small

!     deallocate(itriples)
!     deallocate(n0a)
!     deallocate(n1a)
!     deallocate(n0b)
!     deallocate(n1b)
!     deallocate(n0c)
!     deallocate(n1c)
!     deallocate(n0d)
!     deallocate(n1d)
!     deallocate(n0i)
!     deallocate(n1i)
!     deallocate(n0j)
!     deallocate(n1j)
!     deallocate(n0k)
!     deallocate(n1k)
!     deallocate(n0l)
!     deallocate(n1l)



! end subroutine calculate_r3dim_small

      subroutine calculate_r3limits(nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1, &
            irrep0, irrep1, r3limits)
            integer, intent(in)                              :: nocc, nvirt
            integer, intent(in)                              :: nactive
            integer, intent(in)                              :: nocc0, nocc1
            integer, intent(in)                              :: nvirt0, nvirt1
            integer, dimension(:, :), intent(in)             :: irrep0
                integer, dimension(:, :), intent(in)             :: irrep1
            integer, dimension(:,:), intent(inout) :: r3limits

            integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
            integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks
            integer :: aq
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c, m0d, m1d, m0e, m1e
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k, m0l, m1l, m0m, m1m
            integer :: bj, kj
            integer :: all_ttasks, kk, offset, k, aw
            integer, dimension(:,:), allocatable :: itriples
            integer :: idimt

            nthreads = OMP_NTHREAD
            max_ntasks = MAXT * nthreads

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

            call irrep_triples(6, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  itriples, POINT_GROUP, idimt, .true., .false.)
            allocate(itriples(6, idimt))
            call irrep_triples(6, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  itriples, POINT_GROUP, idimt, .false., .false.)



            ntasks = 0
            kk = 1
            offset = 0
!print*, 'idimt', idimt
            do bj = 1,idimt
                  call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
                        m0j, m1j, m0b, m1b)
                  call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
                        m0k, m1k, m0c, m1c)

                  if (bj == 1) then
                        r3limits(bj, 1) = 1
                  else
                        r3limits(bj, 1) = r3limits(bj-1, 2) + 1
                  end if
                  
                  aw = (m1a-m0a+1)*(m1b-m0b+1)*&
                        (m1c-m0c+1)*(m1i-m0i+1)*&
                        (m1j-m0j+1)*(m1k-m0k+1)
                  

                  r3limits(bj, 2) = r3limits(bj, 1) + aw
!                  write(*, '(1I4, 2I20, 12I4)') bj, (r3limits(bj, :)), m0a, m1a, m0i, m1i, m0b, m1b, m0j, m1j, m0c, m1c, m0k, m1k
                  call calc_s(m0i, m1i, m0a, m1a, m0j, m1j, m0b, m1b, m0k, m1k, m0c, m1c, r3limits(bj, 1))




                  ! ntasks = ntasks + 1
                  
                  ! n0b(ntasks) = m0b
                  ! n1b(ntasks) = m1b
                  
                  ! n0c(ntasks) = m0c
                  ! n1c(ntasks) = m1c
                  
                  ! n0a(ntasks) = m0a
                  ! n1a(ntasks) = m1a
                  
                  ! n0j(ntasks) = m0j
                  ! n1j(ntasks) = m1j
                  
                  ! n0k(ntasks) = m0k
                  ! n1k(ntasks) = m1k
                  
                  ! n0i(ntasks) = m0i
                  ! n1i(ntasks) = m1i
                  
                  
                  ! n0d(ntasks) = m0d
                  ! n1d(ntasks) = m1d
                  ! n0l(ntasks) = m0l
                  ! n1l(ntasks) = m1l
                  


                  ! if (ntasks == max_ntasks)  then
                  !       do k = 1, ntasks
                  !             aw = (n1a(k)-n0a(k)+1)*(n1b(k)-n0b(k)+1)*&
                  !                   (n1c(k)-n0c(k)+1)*(n1i(k)-n0i(k)+1)*&
                  !                   (n1j(k)-n0j(k)+1)*(n1k(k)-n0k(k)+1)
                  !             aq = aq + aw
                              
                  !             r3limits(kk, 1) = offset + 1
                  !             r3limits(kk, 2) = offset + aw
                              
                  !             offset = offset + aw
                  !             kk = kk + 1
                  !       end do
                  !       ntasks = 0
                  ! end if
                  
            end do
    
            ! if (ntasks .gt. 0)  then
            !       do k = 1, ntasks
            !             aw = (n1a(k)-n0a(k)+1)*(n1b(k)-n0b(k)+1)*&
            !                   (n1c(k)-n0c(k)+1)*(n1i(k)-n0i(k)+1)*&
            !                   (n1j(k)-n0j(k)+1)*(n1k(k)-n0k(k)+1)
            !             aq = aq + aw
                        
            !             r3limits(kk, 1) = offset + 1
            !             r3limits(kk, 2) = offset + aw
                
            !             offset = offset + aw
            !             kk = kk + 1
            !       end do
            ! end if
            
    deallocate(itriples)
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

end subroutine calculate_r3limits

      subroutine calc_s(i0, i1, a0, a1, j0, j1, b0, b1, k0, k1, c0, c1, offset)
            integer, intent(in) :: i0, i1, a0, a1, j0, j1, b0, b1, k0, k1, c0, c1
            integer, intent(in) :: offset

            integer :: ni, na, nj, nb, nk, nc
            integer :: mi, ma, mj, mb, mk, mc
            integer :: ibra, iket
            integer :: a, b, c, d, i, j, k, l
            
            ni = i1 - i0 + 1
            na = a1 - a0 + 1
            nj = j1 - j0 + 1
            nb = b1 - b0 + 1
            nk = k1 - k0 + 1
            nc = c1 - c0 + 1

            mi = 1
            ma = ni
            mj = ni * na
            mb = ni * na * nj
            mk = ni * na * nj * nb
            mc = ni * na * nj * nb * nk
            

            do c = c0, c1
                  do k = k0, k1
                        do b = b0, b1
                              do j = j0, j1
                                    do a = a0, a1
                                          do i = i0, i1
           ibra = offset - 1 + (i-i0)+mi + (a-a0)*ma + (j-j0)*mj + (b-b0)*mb + (k-k0)*mk + (c-c0)*mc
!           print*, ibra
           ! print*, ibra, a, i, b, j, c, k
!                                                iket = ...
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            
      end subroutine calc_s

      

  subroutine jacobian_update_driver2(method, eorb, t2, nocc, nocc0, nocc1, &
       nvirt0, nvirt1, nactive, npair, nidx_table, &
       irrep0, irrep1, irrep_idx, multip, r3limits)

    integer, intent(in)                                             :: method
    integer, intent(in)                                             :: nocc
    integer, intent(in)                                             :: nactive
    real(F64), dimension(:), intent(in)                             :: eorb
    real(F64), dimension(nocc+1:nactive, nocc+1:nactive, &
         nocc, nocc), intent(in)                                   :: t2
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair
    integer, dimension(2), intent(in)                               :: nidx_table
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, intent(in)                                             :: multip
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer :: nvirt

    nvirt = nvirt1 - nvirt0 + 1

    bintv = CC_BINTV!6!6
    binto = CC_BINTO!6!5
    kintv = CC_KINTV!1!6
    kinto = CC_KINTO!1!5


    if (dav_converging_right())then
          print*, 'JACOBIAN UPDATE RITGHT'
!       kintv = nvirt
!       kinto = nocc
       !                  print*, 'right', kintv, kinto, bintv, binto
       call jacobian_update(dav_sigma_update_diag, dav_sigma_update_right_nondiag, &
            method, eorb, t2, nocc, nocc0, nocc1, &
            nvirt0, nvirt1, nactive, npair, nidx_table, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, multip, r3limits)
    else
!       bintv = nvirt
!       binto = nocc
       print*, 'JACOBIAN UPDATE LEFT'
       !                  print*, 'left', kintv, kinto, bintv, binto
       call jacobian_update(dav_sigma_update_diag, dav_sigma_update_left_nondiag, &
            method, eorb, t2, nocc, nocc0, nocc1, &
            nvirt0, nvirt1, nactive, npair, nidx_table, bintv, binto, kintv, kinto, &
            irrep0, irrep1, irrep_idx, multip, r3limits)
    end if

end subroutine jacobian_update_driver2

  subroutine jacobian_update(sigup_diag, sigup_nondiag, method, eorb, t2, nocc, nocc0, nocc1, &
       nvirt0, nvirt1, nactive, npair, nidx_table, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx, multip, r3limits)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    integer, intent(in)                                             :: method
    integer, intent(in)                                             :: nocc
    integer, intent(in)                                             :: nactive
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto
    real(F64), dimension(:), intent(in)                             :: eorb
    real(F64), dimension(nocc+1:nactive, nocc+1:nactive, &
         nocc, nocc), intent(in)                                   :: t2
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair
    integer, dimension(2), intent(in)                               :: nidx_table
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, intent(in)                                             :: multip
    integer, dimension(:,:), intent(in)                             :: r3limits
    integer :: nidx_ccsd
    integer :: nidx_plus
    integer :: nidx_minus
    integer, parameter :: plus = 1
    integer, parameter :: minus = -1

    real(F64) :: tm1, tm2
    type(tclock) :: time
    real(F64) :: time1, time2

    if (multip == cc_singlet) then
       nidx_ccsd = nidx_table(1)        

       if (method .eq. THEORY_CCSD)then
          print*, '11start'
          tm1 = omp_get_wtime()
          call ccjac_11_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 = omp_get_wtime()
          print*, 'blok 11', tm2 - tm1
          tm1 = omp_get_wtime()
          call ccjac_12_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 =omp_get_wtime()
          print*, 'blok 12', tm2 - tm1
          tm1 =omp_get_wtime()
          call ccjac_21_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 =omp_get_wtime()
          print*, 'blok 21', tm2 - tm1
          tm1 =omp_get_wtime()

          call ccjac_22_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 =omp_get_wtime()
          print*, 'blok 22', tm2 - tm1

          tm1 =omp_get_wtime()

       end if

       if (method .eq. THEORY_CC3_MEM)then

             print*, "calculating ccjac_21_cc3_dav_triples_part..."
          ! call ccjac_21_cc3_dav_triples_part_sym(sigup_diag, sigup_nondiag, nocc0,&
          !      nocc1, nvirt0, nvirt1, npair + 1, 1)

          call clock_start(time)
          call ccjac_11_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          print*, '11_mem', clock_readwall(time)
          call clock_start(time)
          call ccjac_12_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          print*, '12 mem', clock_readwall(time)
          call clock_start(time)

          call ccjac_21_cc3_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          print*, '21 mem', clock_readwall(time)
          call clock_start(time)
          call ccjac_22_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          print*, '22 mem', clock_readwall(time)

       end if


       if (method .eq. THEORY_CC3)then
          call clock_start(time)
          ! call ccjac_21_cc3_dav_triples_part(sigup_diag, sigup_nondiag, nocc0,&
          !       nocc1, nvirt0, nvirt1, npair + 1, 1)

          ! print*, 'Computing ccjac_21_cc3_triples_part'
          ! call ccjac_21_cc3_dav_triples_part_sym(sigup_diag, sigup_nondiag, nocc0,&
          !      nocc1, nvirt0, nvirt1, npair + 1, 1)

          ! print*, '21_triples_part', clock_readwall(time)
          ! call clock_start(time)

!          call ccjac_33_dav(sigup_diag, eorb, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, irrep_idx, nidx_ccsd + 1, nidx_ccsd + 1)


         call ccjac_33_dav_driver(sigup_diag,&
                       t2, eorb, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
                       bintv, binto, kintv, kinto, &
                       irrep0, irrep1, irrep_idx, r3limits)

!           print*, '33 block', clock_readwall(time)

          call clock_start(time)

          call ccjac_11_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          print*, '11 block', clock_readwall(time)
          call clock_start(time)

          call ccjac_12_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)                 
          print*, '12 block', clock_readwall(time)
          call clock_start(time)
          !$ time1 = omp_get_wtime() 
          call ccjac_21_cc3_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          !$ time2 = omp_get_wtime()                                                                                                                   
          print*, 'czas na 21', time2 - time1  
          print*, '21 block', clock_readwall(time)
          call clock_start(time)

          call ccjac_22_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          print*, '22 block', clock_readwall(time)
          call clock_start(time)

          call ccjac_13_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, r3limits)
          print*, '13 block', clock_readwall(time)
          call clock_start(time)

          call ccjac_31_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, r3limits)
          print*, '31 block', clock_readwall(time)
          call clock_start(time)

          call ccjac_23_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, r3limits)                  
          print*, '23 block-lala', clock_readwall(time)
          call clock_start(time)


          call ccjac_32_dav_driver(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, nidx_ccsd, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, r3limits)
          print*, '32 block', clock_readwall(time)


       end if
    else if (multip == cc_triplet) then
       nidx_plus = nidx_table(1)
       nidx_minus = nidx_table(2)


       if (method .eq. THEORY_CCSD)then

          !----------------------------  11  ----------------------------------
          print*, 'teraz bedzie 11'
          tm1 = omp_get_wtime()
          call ccjac_11_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)

          tm2 = omp_get_wtime()
          print*, 'blok 11', tm2 - tm1
          !----------------------------  12p  ---------------------------------                     
          tm1 = omp_get_wtime()
          print*, 'teraz bedzie 12p'
          call ccjac_12_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 12p', tm2 - tm1
          !----------------------------  12m  ---------------------------------
          print*, 'teraz bedzie 12m'
          tm1 =omp_get_wtime()
          call ccjac_12_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 12m', tm2 - tm1
          !----------------------------  21p  ---------------------------------
          tm1 =omp_get_wtime()
          print*, 'teraz bedzie 2p1'
          call ccjac_21_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 2p1', tm2 - tm1
          !----------------------------  21m  ---------------------------------
          tm1 =omp_get_wtime()
          print*, 'teraz bedzie 2m1'
          call ccjac_21_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 2m1', tm2 - tm1


          tm1 = omp_get_wtime()
          ! call triplet_11_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, &
          !       bintv, binto, kintv, kinto,&
          !       nocc0, nocc1, nocc0, nocc1, 1, 1)
          ! print*, 'ccjac'




          tm1 =omp_get_wtime()   
          print*, 'teraz bedzie 22pp'                     
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 2p2p', tm2 - tm1
          tm1 =omp_get_wtime()

          tm1 =omp_get_wtime()   
          print*, 'teraz bedzie 22pm'                                          
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 2p2m', tm2 - tm1
          tm1 =omp_get_wtime()

          tm1 =omp_get_wtime()  
          print*, 'teraz bedzie 22mp'                                           
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 2m2p', tm2 - tm1
          tm1 =omp_get_wtime()

          tm1 =omp_get_wtime()  
          print*, 'teraz bedzie 22mm', minus, minus                                                                 
          ! call triplet_22m_msub(sigup_diag, sigup_nondiag, &                                                         
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + 1, npair + nidx_plus + 1)
          ! print*, 'pelne 22'
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus, minus)
          ! stop
          tm2 =omp_get_wtime()
          print*, 'blok 2m2m', tm2 - tm1
          tm1 =omp_get_wtime()
          print*, 'jestem w tej [petli', method, THEORY_CC3, THEORY_CCSD


       else if (method .eq. THEORY_CC3) then

             print*, 'nidx_plusz'
             print*, 1, npair
             print*, npair + 1, npair + nidx_plus
             print*, npair + nidx_plus + 1, npair + nidx_plus+nidx_minus

          !----------------------------  33  --------------pika--------------------
          print*, 'zaczynam 33'
          tm1 = omp_get_wtime()
          call ccjac_33_triplet_dav(sigup_diag, eorb, nocc0, nocc1, nvirt0, &
               nvirt1, npair + nidx_plus + nidx_minus + 1, npair + nidx_plus + nidx_minus + 1, &
               irrep0, irrep1, irrep_idx)
          tm2 = omp_get_wtime()
          print*, 'blok 33', tm2 - tm1

          !----------------------------  11  ----------------------------------
          tm1 = omp_get_wtime()
          call ccjac_11_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 = omp_get_wtime()
          print*, 'blok 11', tm2 - tm1
          
          ! !----------------------------  12p  ---------------------------------
          tm1 = omp_get_wtime()
          print*, 'blok 12m'
          call ccjac_12_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 12p', tm2 - tm1

          ! !----------------------------  12m  ---------------------------------
          tm1 =omp_get_wtime()
          call ccjac_12_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 12m', tm2 - tm1

          ! !----------------------------  21p  ---------------------------------
          tm1 =omp_get_wtime()
          print*, 'teraz bedzie blok 21p'
          call ccjac_21_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus, .True.)
          tm2 =omp_get_wtime()
          
          ! print*, 'blok 2p1', tm2 - tm1
          ! tm1 =omp_get_wtime()
          ! call triplet_21p_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair, 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 21p proste', tm2 - tm1
          

          ! !----------------------------  21m  ---------------------------------
          ! ! tm1 =omp_get_wtime()
          ! print*, 'teraz bedzie blok 21m'
          call ccjac_21_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus, .True.)
          tm2 =omp_get_wtime()
          ! print*, 'blok 2m1', tm2 - tm1
          
          ! tm1 =omp_get_wtime()
          ! call triplet_21m_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair + nidx_plus, 1)

!          call triplet_21m_R2m(sigup_diag, sigup_nondiag, &    
!                t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair + nidx_plus, 1)      
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 21m proste', tm2 - tm1
          

          ! !----------------------------  22pp  --------------------------------
          tm1 =omp_get_wtime()                        
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 2p2p', tm2 - tm1
          tm1 =omp_get_wtime()
          ! tm1 =omp_get_wtime()
          ! call triplet_22pp_sub(sigup_diag, sigup_nondiag, &
          !             t2, nocc0, nocc1, nvirt0, nvirt1, npair + 1, npair + 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 22pp', tm2 - tm1

          ! !----------------------------  22pm  --------------------------------
          tm1 =omp_get_wtime()                        
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus, minus)
          tm2 =omp_get_wtime()
          ! print*, 'blok 2p2m', tm2 - tm1
          ! tm1 =omp_get_wtime()

          ! call triplet_22pm_sub(sigup_diag, sigup_nondiag, &
          !             t2, nocc0, nocc1, nvirt0, nvirt1, npair + 1, npair + nidx_plus + 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 22pm', tm2 - tm1
          
          ! !----------------------------  22mp  --------------------------------
          tm1 =omp_get_wtime()                        
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 2m2p', tm2 - tm1
          tm1 =omp_get_wtime()
          
          ! tm1 =omp_get_wtime()
          ! call triplet_22mp_sub(sigup_diag, sigup_nondiag, &
          !             t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + 1, npair + 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 22mp', tm2 - tm1
          
          
          ! !----------------------------  22mm  --------------------------------

          tm1 =omp_get_wtime()                        
          call ccjac_22_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 2m2m', tm2 - tm1
          tm1 =omp_get_wtime()
          
          ! tm1 =omp_get_wtime()
          ! call triplet_22mm_sub(sigup_diag, sigup_nondiag, &                                                                                           
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + 1, npair + nidx_plus + 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 22mm', tm2 - tm1
          

          ! !----------------------------  31  ----------------------------------

          tm1 =omp_get_wtime()
          call ccjac_31_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus,  nidx_minus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 =omp_get_wtime()
          print*, 'blok 31 z ccjac', tm2 - tm1

          ! tm1 =omp_get_wtime()
          ! print*, 'teraz bedzie 31'
          ! call triplet_31_sub(sigup_diag, sigup_nondiag, &                                                       
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + nidx_minus + 1, 1)                     
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 31 z ccjac proste', tm2 - tm1

          ! call triplet_31_R1(sigup_diag, sigup_nondiag, &                                                                                                           
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + nidx_minus + 1, 1)                                                                             
          ! tm2 =omp_get_wtime()                                                                                                                                       
          ! print*, 'blok 31 z ccjac proste', tm2 - tm1            
          ! stop

          ! !----------------------------  13  ----------------------------------
          tm1 =omp_get_wtime()
          call ccjac_13_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, nidx_minus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx)
          tm2 =omp_get_wtime()
          print*, 'blok 13', tm2 - tm1
          ! tm1 =omp_get_wtime()
          ! call triplet_13_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1, npair + nidx_plus + nidx_minus + 1)

          ! call triplet_13_R3(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1, npair + nidx_plus + nidx_minus + 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 13 proste', tm2 - tm1
          

          ! !----------------------------  32p  ---------------------------------
          tm1 =omp_get_wtime()
          call ccjac_32_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, nidx_minus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 32p', tm2 - tm1

          ! tm1 =omp_get_wtime()
          ! call triplet_32p_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + nidx_minus + 1, npair + 1)
          ! tm2 =omp_get_wtime()                                                                                                                         
          ! print*, 'blok 32 proste', tm2 - tm1

          ! call triplet_32p_R2p(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + nidx_minus + 1, 1)                                    
          ! tm2 =omp_get_wtime()                                                                                                        
          ! print*, 'blok 32p z ccjac proste', tm2 - tm1                                                                                          
          ! stop

          

          ! !----------------------------  32m  ---------------------------------
          tm1 =omp_get_wtime()
          call ccjac_32_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, nidx_minus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 32m', tm2 - tm1

          ! tm1 =omp_get_wtime()
          ! call triplet_32m_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + nidx_minus + 1, npair + nidx_plus + 1)
          ! tm2 =omp_get_wtime()
          ! print*, 'blok 32m', tm2 - tm1
          
          ! call triplet_32m_R2m(sigup_diag, sigup_nondiag, &                                                                                              
          !       t2, nocc0, nocc1, nvirt0, nvirt1, npair + nidx_plus + nidx_minus + 1, 1)                                              
          ! tm2 =omp_get_wtime()                                                          
          ! print*, 'blok 32m z ccjac proste', tm2 - tm1                                                                 
          ! stop                                            


          ! !----------------------------  23p  ---------------------------------
          tm1 =omp_get_wtime()
          call ccjac_23_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus, nidx_minus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, plus)
          tm2 =omp_get_wtime()
          print*, 'blok 2p3', tm2 - tm1
          
          ! tm1 =omp_get_wtime()
          ! call triplet_23p_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair, npair + nidx_plus + nidx_minus + 1)
          ! tm2 =omp_get_wtime()
          ! call triplet_23p_R2p(sigup_diag, sigup_nondiag, &                                                                                                          
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair, npair + nidx_plus + nidx_minus + 1)
          ! tm2 =omp_get_wtime()                                                                                                                                       
          ! print*, 'blok 32m z ccjac proste', tm2 - tm1        
          ! ! print*, 'blok 23p proste', tm2 - tm1


          ! !----------------------------  23m  ---------------------------------
          tm1 =omp_get_wtime()
          call ccjac_23_dav_driver_triplet(sigup_diag, sigup_nondiag, &
               t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
               nidx_plus,  nidx_minus, &
               bintv, binto, kintv, kinto, &
               irrep0, irrep1, irrep_idx, minus)
          tm2 =omp_get_wtime()
          print*, 'blok 2m3', tm2 - tm1
          ! tm1 =omp_get_wtime()
          ! call triplet_23m_sub(sigup_diag, sigup_nondiag, &
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair + nidx_plus , npair + nidx_plus + nidx_minus + 1)
          ! call triplet_23m_R2m(sigup_diag, sigup_nondiag, &                                                                                                                            
          !       t2, nocc0, nocc1, nvirt0, nvirt1, 1 + npair, npair + nidx_plus + nidx_minus + 1)
          ! stop
          ! ! tm2 =omp_get_wtime()
          ! print*, 'blok 23m proste', tm2 - tm1
          
          

       end if
    end if

  end subroutine jacobian_update

  subroutine ccjac_11_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1,  &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx

    integer        :: pa, pb, pi, pj
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
    max_ntasks = MAXT * nthreads

    nocc = nocc1 - nocc0 + 1

    allocate(n0a(max_ntasks))
    allocate(n1a(max_ntasks))
    allocate(n0b(max_ntasks))
    allocate(n1b(max_ntasks))
    allocate(n0i(max_ntasks))
    allocate(n1i(max_ntasks))
    allocate(n0j(max_ntasks))
    allocate(n1j(max_ntasks))

    ! call ccjac_11_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1, nvirt0, nvirt1, nocc0, nocc1, nocc0, nocc1, 1, 1)
         ! n0a(k), n1a(k), n0b(k), n1b(k), n0i(k), n1i(k), n0j(k), n1j(k), 1, 1)


    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
    allocate(isingles(2, idims))
    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

    if (dav_converging_right())then

       do kj = 1, idims

          call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
               m0j, m1j, m0b, m1b)

          ntasks = 0

          do bj = 1, idims

             call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                  m0i, m1i, m0a, m1a)

             ! do pa = m0a, m1a, bintv
             !    do pi = m0i, m1i, binto

                   ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i
                   
                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j

                   ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                   ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                   if (ntasks == max_ntasks)  then

                      call dotasks_11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)
                      ntasks = 0
                   end if
             !    end do
             ! end do
          end do

          if(ntasks .gt.0)then

             call dotasks_11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
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
                      call dotasks_11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, ntasks)
                      ntasks = 0
                   end if
                end do
             end do
          end do

          if(ntasks .gt.0)then
             call dotasks_11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
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

  end subroutine ccjac_11_dav_driver

  subroutine dotasks_11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
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
    !$omp do schedule(dynamic) 
    do k = 1, ntasks
       call ccjac_11_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0i(k), n1i(k), n0j(k), n1j(k), 1, 1)
    end do
    !$omp end do
    !$omp end parallel

  end subroutine dotasks_11

  subroutine ccjac_12_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx

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
    max_ntasks = MAXT * nthreads
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

    ! call ccjac_12_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
    !      nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, 1, npair + 1)
         ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
         ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), 1, npair + 1)




    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
    allocate(isingles(2, idims))
    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
         idoubles, POINT_GROUP, idimd, .true., .true.)
    allocate(idoubles(4, idimd))
    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
         idoubles, POINT_GROUP, idimd, .false., .true.)

    if (dav_converging_right())then

       do kj = 1, idimd
          call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
               m0j, m1j, m0b, m1b)
          call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
               m0k, m1k, m0c, m1c)
          ntasks = 0
          do bj = 1, idims
             call loop_boundaries_sp(isingles(:, bj), irrep0, irrep1, &
                  m0i, m1i, m0a, m1a)

             ! do pa = m0a, m1a, bintv
             !    do pi = m0i, m1i, binto

                   ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i

                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j
                   n0c(ntasks) = m0c
                   n1c(ntasks) = m1c
                   n0k(ntasks) = m0k
                   n1k(ntasks) = m1k

                   ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                   ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                   if (ntasks == max_ntasks)  then
                      call dotasks_12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0c, n1c, &
                           n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
                      ntasks = 0
                   end if
             !    end do
             ! end do
          end do

          if(ntasks .gt.0)then
             call dotasks_12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, &
                  n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
          end if
       end do
    else

       do bj = 1, idims
          call loop_boundaries_sp(isingles(:, bj), irrep0, irrep1, &
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
                            call dotasks_12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, &
                                 n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
                            ntasks = 0
                         end if
                      end do
                   end do
                end do
             end do
          end do

          if(ntasks .gt.0)then
             call dotasks_12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, &
                  n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
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


  end subroutine ccjac_12_dav_driver

  subroutine dotasks_12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in) :: npair

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k
    integer, intent(in) :: ntasks
    integer :: k

    !$omp parallel private(k) default(shared)                                                                          
    !$omp do schedule(dynamic)                                                                                               
    do k = 1, ntasks
       call ccjac_12_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), 1, npair + 1)
    end do
    !$omp end do                                                                                               
    !$omp end parallel                                                                                               

  end subroutine dotasks_12


  subroutine ccjac_21_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx

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
    max_ntasks = MAXT * nthreads
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

    ! call ccjac_21_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
    !      nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + 1, 1)
         ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
         ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + 1, 1)


    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
    allocate(isingles(2, idims))
    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)


    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .true., .true.)
    allocate(idoubles(4, idimd))
    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .false., .true.)

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


             ! do pb = m0b, m1b, bintv
             !    do pj = m0j, m1j, binto
             !       do pa = m0a, m1a, bintv
             !          do pi = m0i, m1i, binto

                         ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i


                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j


                         n0c(ntasks) = m0c
                         n1c(ntasks) = m1c
                         n0k(ntasks) = m0k
                         n1k(ntasks) = m1k

                         ! call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                         ! call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                         ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                         ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))


                         if (ntasks == max_ntasks)  then
                            call dotasks_21(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
                            ntasks = 0
                         end if
             !          end do
             !       end do
             !    end do
             ! end do
          end do

          if(ntasks .gt.0)then
             call dotasks_21(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
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
             call loop_boundaries_sp(isingles(:, kj), irrep0, irrep1, &
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
                      call dotasks_21(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
                      ntasks = 0
                   end if
                end do
             end do
          end do
          if(ntasks .gt.0)then
             call dotasks_21(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
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


  end subroutine ccjac_21_dav_driver

  subroutine dotasks_21(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k
    integer, intent(in) :: ntasks
    integer :: k

    !$omp parallel private(k) default(shared)               
    !$omp do schedule(dynamic)                                                                                        
    do k = 1, ntasks
       call ccjac_21_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + 1, 1)
    end do
    !$omp end do
    !$omp end parallel

  end subroutine dotasks_21


  subroutine ccjac_22_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx

    integer, dimension(:), allocatable         :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
    integer, dimension(:), allocatable         :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l

    integer        :: pa, pb, pc, pd, pi, pj, pk, pl
    integer :: nthreads
    integer :: max_ntasks
    integer :: ntasks

    integer, dimension(:,:), allocatable :: idoubles
    integer :: idim
    integer :: m0a, m1a, m0b, m1b
    integer :: m0c, m1c, m0d, m1d
    integer :: m0i, m1i, m0j, m1j
    integer :: m0k, m1k, m0l, m1l
    integer :: nocc
    integer :: bj, kj

    nthreads = OMP_NTHREAD
    max_ntasks = MAXT * nthreads

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

    ! call ccjac_22_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
    !      nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + 1, npair + 1)
         ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
         ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), npair + 1, npair + 1)

    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idim, .true., .true.)
    allocate(idoubles(4, idim))
    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idim, .false., .true.)

    if (dav_converging_right())then

       do kj = 1, idim
          call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
               m0k, m1k, m0c, m1c)
          call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
               m0l, m1l, m0d, m1d)

          ntasks = 0
          do bj = 1, idim
             call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                  m0i, m1i, m0a, m1a)
             call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                  m0j, m1j, m0b, m1b)

             ! do pb = m0b, m1b, bintv
             !    do pj = m0j, m1j, binto
             !       do pa = m0a, m1a, bintv
             !          do pi = m0i, m1i, binto

                         ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i


                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j


                         n0c(ntasks) = m0c
                         n1c(ntasks) = m1c
                         n0d(ntasks) = m0d
                         n1d(ntasks) = m1d
                         n0k(ntasks) = m0k
                         n1k(ntasks) = m1k
                         n0l(ntasks) = m0l
                         n1l(ntasks) = m1l

                         ! call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                         ! call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                         ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                         ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                         if (ntasks == max_ntasks)  then
                            call dotasks_22(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                 n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, ntasks)
                            ntasks = 0
                         end if
             !          end do
             !       end do
             !    end do
             ! end do
          end do

          if(ntasks .gt. 0)then
             call dotasks_22(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, ntasks)                 
          end if
       end do
    else

       do bj = 1, idim
          call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
               m0i, m1i, m0a, m1a)
          call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
               m0j, m1j, m0b, m1b)

          ntasks = 0

          do kj = 1, idim
             call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                  m0k, m1k, m0c, m1c)
             call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
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
                            call dotasks_22(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                 n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, ntasks)
                            ntasks = 0
                         end if
                      end do
                   end do
                end do
             end do
          end do
          if(ntasks .gt. 0)then

             call dotasks_22(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, ntasks)                 
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


  end subroutine ccjac_22_dav_driver

  subroutine dotasks_22(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, npair, ntasks)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
    integer, intent(in) :: ntasks
    integer :: k

    !$omp parallel private(k) default(shared)    
    !$omp do schedule(dynamic)                                                        
    do k = 1, ntasks
       call ccjac_22_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), npair + 1, npair + 1)
    end do
    !$omp end do       
    !$omp end parallel

  end subroutine dotasks_22

  subroutine ccjac_21_cc3_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx

    integer, dimension(:), allocatable        :: n0a, n1a, n0b, n1b, n0c, n1c
    integer, dimension(:), allocatable        :: n0i, n1i, n0j, n1j, n0k, n1k
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
    max_ntasks = MAXT * nthreads
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

    ! call ccjac_21_cc3_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
    !      nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, npair + 1, 1)
         ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k),&
         ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + 1, 1)

    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
    allocate(isingles(2, idims))
    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)


    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .true., .true.)
    allocate(idoubles(4, idimd))
    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .false., .true.)

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
                do pj = m0j, m1j,binto
                   do pa = m0a, m1a, bintv
                      do pi = m0i, m1i, binto

                         ntasks = ntasks + 1

                   ! n0a(ntasks) = m0a
                   ! n1a(ntasks) = m1a
                   ! n0i(ntasks) = m0i
                   ! n1i(ntasks) = m1i


                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j


                         n0c(ntasks) = m0c
                         n1c(ntasks) = m1c
                         n0k(ntasks) = m0k
                         n1k(ntasks) = m1k

                         call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                         call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                         call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                         call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))


                         if (ntasks == max_ntasks)  then
                            call dotasks_21_cc3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
                            ntasks = 0
                         end if
                      end do
                   end do
                end do
             end do
          end do

          if(ntasks .gt.0)then
             call dotasks_21_cc3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
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
             call loop_boundaries_sp(isingles(:, kj), irrep0, irrep1, &
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
                   call loop_boundaries(pk, m1k, kinto, n0k(ntasks), n1k(ntasks))

                   if (ntasks == max_ntasks)  then
                      call dotasks_21_cc3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
                      ntasks = 0
                   end if
                end do
             end do
          end do
          if(ntasks .gt.0)then
             call dotasks_21_cc3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)
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


  end subroutine ccjac_21_cc3_dav_driver

  subroutine dotasks_21_cc3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, npair, ntasks)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k
    integer, intent(in) :: ntasks
    integer :: k, nc, nv
    real(F64), dimension(:,:,:), allocatable :: scr
    nv = nvirt1 - nvirt0 + 1
    nc = nocc1 - nocc0 + 1
    allocate(scr(nv,nc,nv))

    !$omp parallel private(k, scr) default(shared)    
    !$omp do schedule(dynamic)                                                        
    do k = 1, ntasks
       call ccjac_21_cc3_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k),&
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), npair + 1, 1)
    end do
    !$omp end do       
    !$omp end parallel

  end subroutine dotasks_21_cc3


  subroutine ccjac_13_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx, r3limits)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer, dimension(:), allocatable        :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
    integer, dimension(:), allocatable        :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
    integer, dimension(:,:), allocatable :: isingles
    integer, dimension(:,:), allocatable :: itriples
    integer :: pa, pb, pc, pd, pi, pj, pk, pl
    integer :: m0a, m1a
    integer :: m0b, m1b, m0c, m1c, m0d, m1d
    integer :: m0i, m1i
    integer :: m0j, m1j, m0k, m1k, m0l, m1l
    integer :: bj, kj
    integer :: idims, idimt
    integer :: nocc
    integer :: nthreads
    integer :: max_ntasks
    integer :: ntasks
    integer :: kk
    real(F64) :: tm1, tm2
    integer, dimension(:), allocatable :: kj_ntasks

    nthreads = OMP_NTHREAD
    max_ntasks = MAXT * nthreads
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
    allocate(kj_ntasks(max_ntasks))

    nocc = nocc1 - nocc0 + 1

    ! call ccjac_13_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !       nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
    !       nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, 1, nidx_ccsd + 1)
          ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
          ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), 1, nidx_ccsd + 1)


    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
    allocate(isingles(2, idims))
    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)


    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, &
         nocc, nvirt0, itriples, POINT_GROUP, idimt, .true., .false.)
    allocate(itriples(6, idimt))
    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, &
         nocc, nvirt0, itriples, POINT_GROUP, idimt, .false., .false.)


    if (dav_converging_right())then

          tm1 = omp_get_wtime()

          kk = 1
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

             ! do pa = m0a, m1a, bintv
             !    do pi = m0i, m1i, binto
             !          if (bintv.lt.(m1a-m0a+1))then
             !                print*, 'binbinbintv', m0a, m1a, bintv
             !          end if
             !          write(*,'(6I5)') kj, bj, pa, m0a, m1a, bintv

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

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a

                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i
                   kj_ntasks(ntasks) = kj

                   ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                   ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))
                   ! print*, n0a, n1a
                   ! stop

                   if (ntasks == max_ntasks)  then
                      call dotasks_13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                           n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, kj, r3limits, kj_ntasks)
                      ntasks = 0
                   end if

             !    end do
             ! end do
          end do

          if(ntasks .gt. 0)then

             call dotasks_13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, kj, r3limits, kj_ntasks)
          end if
       end do

      ! tm2 = omp_get_wtime()
      ! print*, 'blok 1333333', tm2 - tm1
      ! stop

   else

       do bj = 1, idims
          call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
               m0i, m1i, m0a, m1a)

          ntasks = 0
          kk = 1
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

                               call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                               call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))
                               call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                               call loop_boundaries(pk, m1k, kinto, n0k(ntasks), n1k(ntasks))
                               call loop_boundaries(pb, m1b, kintv, n0b(ntasks), n1b(ntasks))
                               call loop_boundaries(pj, m1j, kinto, n0j(ntasks), n1j(ntasks))

                               if (ntasks == max_ntasks)  then
                                  call dotasks_13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                       n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, kj, r3limits, kj_ntasks)
                                  kk = kk + 1
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
             call dotasks_13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, kj, r3limits, kj_ntasks)
          end if
       end do
    end if
      ! tm2 = omp_get_wtime()
      !  print*, 'blok 1333333', tm2 - tm1
      !  stop

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


  end subroutine ccjac_13_dav_driver

  subroutine dotasks_13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, kj, r3limits, kj_n)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: nidx_ccsd

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
    integer, intent(in) :: ntasks
    integer, intent(in) :: kj
    integer, dimension(:,:), intent(in) :: r3limits
    integer, dimension(:), intent(in) :: kj_n
    integer :: k

    !$omp parallel private(k) default(shared)    
    !$omp do schedule(dynamic)                                                        
    do k = 1, ntasks
       call ccjac_13_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), 1, nidx_ccsd + 1, r3limits(kj_n(k), 1))
    end do
    !$omp end do       
    !$omp end parallel

  end subroutine dotasks_13



  subroutine ccjac_31_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx, r3limits)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
    integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
    integer                                 :: pa, pb, pc, pd, pi, pj, pk, pl

    integer, dimension(:,:), allocatable :: itriples
    integer, dimension(:,:), allocatable :: isingles
    integer :: idims, idimt
    integer :: m0a, m1a, m0b, m1b, m0c, m1c
    integer :: m0d, m1d
    integer :: m0i, m1i, m0j, m1j, m0k, m1k
    integer :: m0l, m1l
    integer :: nocc
    integer :: bj, kj

    integer :: nthreads
    integer :: max_ntasks
    integer :: ntasks
    integer :: r3dim_small, aq
    integer, dimension(:), allocatable :: kj_ntasks

    nthreads = OMP_NTHREAD
    max_ntasks = MAXT * nthreads

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
    allocate(kj_ntasks(max_ntasks))

    nocc = nocc1 - nocc0 + 1

    ! call ccjac_31_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1,nvirt0, nvirt1,nvirt0, nvirt1,nvirt0, nvirt1, &
    !      nocc0, nocc1,nocc0, nocc1,nocc0, nocc1,nocc0, nocc1, nidx_ccsd + 1, 1)
         ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
         ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), nidx_ccsd + 1, 1)

    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
    allocate(isingles(2, idims))
    call irrep_singless(irrep_idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, &
         nocc, nvirt0, itriples, POINT_GROUP, idimt, .true., .false.)
    allocate(itriples(6, idimt))
    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, &
         nocc, nvirt0, itriples, POINT_GROUP, idimt, .false., .false.)

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

             ! do pc = m0c, m1c, bintv
             !    do pk = m0k, m1k, binto
             !       do pb = m0b, m1b, bintv
             !          do pj = m0j, m1j, binto
             !             do pa = m0a, m1a, bintv
             !                do pi = m0i, m1i, binto

                               ntasks = ntasks + 1

                               n0b(ntasks) = m0b
                               n1b(ntasks) = m1b
                               
                               n0c(ntasks) = m0c
                               n1c(ntasks) = m1c
                               
                               n0a(ntasks) = m0a
                               n1a(ntasks) = m1a
                               
                               n0j(ntasks) = m0j
                               n1j(ntasks) = m1j

                               n0k(ntasks) = m0k
                               n1k(ntasks) = m1k

                               n0i(ntasks) = m0i
                               n1i(ntasks) = m1i


                               n0d(ntasks) = m0d
                               n1d(ntasks) = m1d
                               n0l(ntasks) = m0l
                               n1l(ntasks) = m1l
                               kj_ntasks(ntasks) = bj

                               ! call loop_boundaries(pc, m1c, bintv, n0c(ntasks), n1c(ntasks))
                               ! call loop_boundaries(pk, m1k, binto, n0k(ntasks), n1k(ntasks))
                               ! call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                               ! call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                               ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                               ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                               if (ntasks == max_ntasks)  then
                                  call dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                                       n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, r3limits, kj_ntasks)
                                  ntasks = 0
                               end if
             !                end do
             !             end do
             !          end do
             !       end do
             !    end do
             ! end do
          end do

          if(ntasks .gt. 0)then
             call dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, r3limits, kj_ntasks)
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

             do pd = m0d, m1d, kintv
                do pl = m0l, m1l, kinto

                   ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i

                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j

                   n0c(ntasks) = m0c
                   n1c(ntasks) = m1c
                   n0k(ntasks) = m0k
                   n1k(ntasks) = m1k

                   call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                   call loop_boundaries(pl, m1l, kinto, n0l(ntasks), n1l(ntasks))

                   if (ntasks == max_ntasks)  then
                      call dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                           n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                           n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, r3limits, kj_ntasks)
                      ntasks = 0
                   end if
                end do
             end do
          end do

          if(ntasks .gt. 0)then
             call dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, r3limits, kj_ntasks)
          end if
       end do
    end if
!-------------------------------------------------------------
    ! ntasks = 0

    ! do pd = nvirt0, nvirt1, kintv
    !   do pl = nocc0, nocc1, kinto
    !     do pc = nvirt0, nvirt1, bintv
    !       do pk = nocc0, nocc1, binto
    !         do pb = pc, nvirt1, bintv
    !           do pj = nocc0, nocc1, binto
    !             do pa = pb, nvirt1, bintv
    !               do pi = nocc0, nocc1, binto

    !                     ntasks = ntasks + 1

    !                     call loop_boundaries(pd, nvirt1, kintv, n0d(ntasks), n1d(ntasks))
    !                     call loop_boundaries(pl, nocc1,  kinto, n0l(ntasks), n1l(ntasks))

    !                     call loop_boundaries(pc, nvirt1, bintv, n0c(ntasks), n1c(ntasks))
    !                     call loop_boundaries(pk, nocc1,  binto, n0k(ntasks), n1k(ntasks))
    !                     call loop_boundaries(pb, nvirt1, bintv, n0b(ntasks), n1b(ntasks))
    !                     call loop_boundaries(pj, nocc1,  binto, n0j(ntasks), n1j(ntasks))
    !                     call loop_boundaries(pa, nvirt1, bintv, n0a(ntasks), n1a(ntasks))
    !                     call loop_boundaries(pi, nocc1,  binto, n0i(ntasks), n1i(ntasks))

    !                     if (ntasks == max_ntasks)  then
    !                           call dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !                                 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
    !                                 n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks)
    !                           ntasks = 0
    !                     end if
    !               end do
    !             end do
    !           end do
    !         end do
    !       end do
    !     end do
    !   end do
    ! end do

    ! if(ntasks .gt. 0)then
    !       call dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !             n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, &
    !             n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks)
    ! end if

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


  end subroutine ccjac_31_dav_driver

  subroutine dotasks_31(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, nidx_ccsd, ntasks, r3limits, kj_n)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: nidx_ccsd
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l
    integer, intent(in) :: ntasks
    integer, dimension(:), intent(in) :: kj_n
    integer :: k

    !$omp parallel private(k) default(shared)    
    !$omp do schedule(dynamic)
    do k = 1, ntasks
       ! print*, n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k)
       ! print*, n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k)
       ! print*, ''
       call ccjac_31_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k),&
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), nidx_ccsd + 1, 1, r3limits(kj_n(k), 1))
    end do
    !$omp end do       
    !$omp end parallel

  end subroutine dotasks_31

  subroutine dotasks_31_count(nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, ntasks, aq)

        integer, intent(in)                                             :: nocc0, nvirt0
        integer, intent(in)                                             :: nocc1, nvirt1

        integer, intent(out) :: aq
        
        integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c
        integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k
        integer, intent(in) :: ntasks
        integer :: k, aw, npair

        npair = (nvirt1-nvirt0+1)*(nocc1-nocc0+1)

        aq = 0
        do k = 1, ntasks
              aw = (n1a(k)-n0a(k)+1)*(n1b(k)-n0b(k)+1)*(n1c(k)-n0c(k)+1)*(n1i(k)-n0i(k)+1)*(n1j(k)-n0j(k)+1)*(n1k(k)-n0k(k)+1)
              
              aq = aq + aw

        end do

  end subroutine dotasks_31_count


  subroutine ccjac_23_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx, r3limits)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
    integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m
    integer                                 :: pa, pb, pc, pd, pe, pi, pj, pk, pl, pm

    integer, dimension(:,:), allocatable :: idoubles
    integer, dimension(:,:), allocatable :: itriples
    integer :: idimd, idimt
    integer :: m0a, m1a, m0b, m1b
    integer :: m0c, m1c, m0d, m1d, m0e, m1e
    integer :: m0i, m1i, m0j, m1j
    integer :: m0k, m1k, m0l, m1l, m0m, m1m
    integer :: nocc
    integer :: bj, kj

    integer :: nthreads
    integer :: max_ntasks
    integer :: ntasks
    integer, dimension(:), allocatable :: kj_ntasks

    nthreads = OMP_NTHREAD
    max_ntasks = 10 * nthreads

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
    allocate(kj_ntasks(max_ntasks))

    nocc = nocc1 - nocc0 + 1

      ! call ccjac_23_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
      !       nvirt0, nvirt1, nvirt0, nvirt1,nvirt0, nvirt1,nvirt0, nvirt1,nvirt0, nvirt1, &
      !       nocc0, nocc1,   nocc0, nocc1,  nocc0, nocc1,  nocc0, nocc1,  nocc0, nocc1, npair + 1, nidx_ccsd+1)
            ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
            ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
            ! n0m(k), n1m(k), npair + 1, nidx_ccsd + 1)



    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .true., .true.)
    allocate(idoubles(4, idimd))
    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .false., .true.)

    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, POINT_GROUP, idimt, .true., .false.)
    allocate(itriples(6, idimt))
    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, POINT_GROUP, idimt, .false., .false.)

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

             ! do pb = m0b, m1b, bintv
             !    do pj = m0j, m1j, binto
             !       do pa = m0a, m1a, bintv
             !          do pi = m0i, m1i, binto

                         ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i


                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j


                         n0e(ntasks) = m0e
                         n1e(ntasks) = m1e
                         n0m(ntasks) = m0m
                         n1m(ntasks) = m1m
                         n0d(ntasks) = m0d
                         n1d(ntasks) = m1d
                         n0l(ntasks) = m0l
                         n1l(ntasks) = m1l
                         n0c(ntasks) = m0c
                         n1c(ntasks) = m1c
                         n0k(ntasks) = m0k
                         n1k(ntasks) = m1k
                         kj_ntasks(ntasks) = kj

                         ! call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                         ! call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                         ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                         ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                         if (ntasks == max_ntasks)  then
                            call dotasks_23(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                 n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                 n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
                            ntasks = 0
                         end if

          !             end do
          !          end do
          !       end do
          !    end do
                   end do
          if(ntasks .gt. 0)then
             call dotasks_23(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                  n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
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
                               n0i(ntasks) = m0i
                               n1i(ntasks) = m1i
                               n0b(ntasks) = m0b
                               n1b(ntasks) = m1b
                               n0j(ntasks) = m0j
                               n1j(ntasks) = m1j

                               call loop_boundaries(pe, m1e, kintv, n0e(ntasks), n1e(ntasks))
                               call loop_boundaries(pm, m1m,  kinto, n0m(ntasks), n1m(ntasks))
                               call loop_boundaries(pd, m1d, kintv, n0d(ntasks), n1d(ntasks))
                               call loop_boundaries(pl, m1l,  kinto, n0l(ntasks), n1l(ntasks))
                               call loop_boundaries(pc, m1c, kintv, n0c(ntasks), n1c(ntasks))
                               call loop_boundaries(pk, m1k,  kinto, n0k(ntasks), n1k(ntasks))

                               if (ntasks == max_ntasks)  then
                                  call dotasks_23(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                       n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                       n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
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
             call dotasks_23(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                  n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
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


  end subroutine ccjac_23_dav_driver

  subroutine dotasks_23(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
       n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_n)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, dimension(:,:), intent(in) :: r3limits

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m
    integer, intent(in) :: ntasks
    integer, dimension(:), intent(in) :: kj_n
    integer :: k

    !$omp parallel private(k) default(shared)    
    !$omp do schedule(dynamic)                                                        
    do k = 1, ntasks
       call ccjac_23_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
            n0m(k), n1m(k), npair + 1, nidx_ccsd + 1, r3limits(kj_n(k), 1))
    end do
    !$omp end do       
    !$omp end parallel

  end subroutine dotasks_23

    subroutine ccjac_33_dav_driver(sigup_diag, t2, eorb, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx, r3limits)

   procedure(dav_sigma_update_diag) :: sigup_diag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    real(F64),  dimension(:), intent(in)          :: eorb
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer, dimension(:), allocatable      :: n0a, n1a, n0b, n1b, n0c, n1c
    integer, dimension(:), allocatable      :: n0i, n1i, n0j, n1j, n0k, n1k


    integer :: nthreads  
    integer :: max_ntasks
    integer :: ntasks

    integer, dimension(:,:), allocatable :: itriples
    integer :: idimd, idimt
    integer :: m0a, m1a, m0b, m1b
    integer :: m0c, m1c
    integer :: m0i, m1i, m0j, m1j
    integer :: m0k, m1k
    integer :: nocc
    integer :: bj, kj

    nthreads = OMP_NTHREAD
    max_ntasks = MAXT * nthreads

    nocc = nocc1 - nocc0 + 1

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

    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, POINT_GROUP, idimt, .true., .false.)
    allocate(itriples(6, idimt))
    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, POINT_GROUP, idimt, .false., .false.)

    ntasks = 0


    do bj = 1, idimt
          call loop_boundaries_sp(itriples(1:2, bj), irrep0, irrep1, &
                m0i, m1i, m0a, m1a)
          call loop_boundaries_sp(itriples(3:4, bj), irrep0, irrep1, &
                m0j, m1j, m0b, m1b)
          call loop_boundaries_sp(itriples(5:6, bj), irrep0, irrep1, &
                m0k, m1k, m0c, m1c)
          
          ntasks = ntasks + 1
          
          n0a(ntasks) = m0a
          n1a(ntasks) = m1a
          n0i(ntasks) = m0i
          n1i(ntasks) = m1i
          
          
          n0b(ntasks) = m0b
          n1b(ntasks) = m1b
          n0j(ntasks) = m0j
          n1j(ntasks) = m1j
          
          
          n0c(ntasks) = m0c
          n1c(ntasks) = m1c
          n0k(ntasks) = m0k
          n1k(ntasks) = m1k
          
          if (ntasks == max_ntasks)  then
                call dotasks_33(sigup_diag, t2, eorb, nocc0, nocc1, nvirt0, nvirt1, &
                      n0a, n1a, n0b, n1b, n0c, n1c, &
                      n0i, n1i, n0j, n1j, n0k, n1k,&
                      npair, nidx_ccsd, ntasks, r3limits)
                ntasks = 0
          end if
    end do
    if(ntasks .gt. 0)then
          call dotasks_33(sigup_diag, t2, eorb, nocc0, nocc1, nvirt0, nvirt1, &
                n0a, n1a, n0b, n1b, n0c, n1c, &
                n0i, n1i, n0j, n1j, n0k, n1k,&
                npair, nidx_ccsd, ntasks, r3limits)
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

          
    end subroutine ccjac_33_dav_driver


  subroutine dotasks_33(sigup_diag, t2, eorb, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, &
       npair, nidx_ccsd, ntasks, r3limits)

    procedure(dav_sigma_update_diag) :: sigup_diag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    real(F64),  dimension(:), intent(in)          :: eorb
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, dimension(:,:), intent(in)  :: r3limits

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k
    integer, intent(in) :: ntasks
    integer :: k

    !$omp parallel private(k) default(shared)                                                              
    !$omp do schedule(dynamic)                                                                                                    
    do k = 1, ntasks
       call ccjac_33_dav_mem(sigup_diag, t2, eorb, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), &
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), &
            nidx_ccsd + 1, nidx_ccsd + 1, r3limits(k, 1))
    end do
    !$omp end do                                                                 
    !$omp end parallel                                                                                                      

end subroutine dotasks_33


  subroutine ccjac_32_dav_driver(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, npair, &
       nidx_ccsd, bintv, binto, kintv, kinto, &
       irrep0, irrep1, irrep_idx, r3limits)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2 
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, intent(in)                                             :: bintv, binto
    integer, intent(in)                                             :: kintv, kinto 
    integer, dimension(:,:), intent(in)                             :: irrep0
    integer, dimension(:,:), intent(in)                             :: irrep1
    integer, intent(in)                                             :: irrep_idx
    integer, dimension(:,:), intent(in)                             :: r3limits

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
integer, dimension(:), allocatable :: kj_ntasks

    nthreads = OMP_NTHREAD
    max_ntasks = MAXT * nthreads

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
    allocate(kj_ntasks(max_ntasks))

    nocc = nocc1 - nocc0 + 1

    ! call ccjac_32_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
    !      nvirt0, nvirt1, nvirt0, nvirt1,nvirt0, nvirt1,nvirt0, nvirt1,nvirt0, nvirt1, &
    !      nocc0, nocc1,   nocc0, nocc1,  nocc0, nocc1,  nocc0, nocc1,  nocc0, nocc1, nidx_ccsd+1, npair + 1)

         ! n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
         ! n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
         ! n0m(k), n1m(k), nidx_ccsd + 1, npair + 1)
    

    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .true., .true.)
    allocate(idoubles(4, idimd))
    call irrep_doubless(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, POINT_GROUP, idimd, .false., .true.)

    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, POINT_GROUP, idimt, .true., .false.)
    allocate(itriples(6, idimt))
    call irrep_triples(irrep_idx, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, POINT_GROUP, idimt, .false., .false.)

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


             ! do pc = m0c, m1c, bintv
             !    do pk = m0k, m1k, binto
             !       do pb = m0b, m1b, bintv
             !          do pj = m0j, m1j, binto
             !             do pa = m0a, m1a, bintv
             !                do pi = m0i, m1i, binto

                               ntasks = ntasks + 1

                   n0a(ntasks) = m0a
                   n1a(ntasks) = m1a
                   n0i(ntasks) = m0i
                   n1i(ntasks) = m1i


                   n0b(ntasks) = m0b
                   n1b(ntasks) = m1b
                   n0j(ntasks) = m0j
                   n1j(ntasks) = m1j


                   n0c(ntasks) = m0c
                   n1c(ntasks) = m1c
                   n0k(ntasks) = m0k
                   n1k(ntasks) = m1k


                               n0e(ntasks) = m0e
                               n1e(ntasks) = m1e
                               n0m(ntasks) = m0m
                               n1m(ntasks) = m1m
                               n0d(ntasks) = m0d
                               n1d(ntasks) = m1d
                               n0l(ntasks) = m0l
                               n1l(ntasks) = m1l
                               kj_ntasks(ntasks) = bj

                               ! call loop_boundaries(pc, m1c, bintv, n0c(ntasks), n1c(ntasks))
                               ! call loop_boundaries(pk, m1k, binto, n0k(ntasks), n1k(ntasks))
                               ! call loop_boundaries(pb, m1b, bintv, n0b(ntasks), n1b(ntasks))
                               ! call loop_boundaries(pj, m1j, binto, n0j(ntasks), n1j(ntasks))
                               ! call loop_boundaries(pa, m1a, bintv, n0a(ntasks), n1a(ntasks))
                               ! call loop_boundaries(pi, m1i, binto, n0i(ntasks), n1i(ntasks))

                               if (ntasks == max_ntasks)  then
                                  call dotasks_32(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                       n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                       n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
                                  ntasks = 0
                               end if
             !                end do
             !             end do
             !          end do
             !       end do
             !    end do
             ! end do
          end do
          if(ntasks .gt. 0)then
             call dotasks_32(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                  n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
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

             do pe = m0e, m1e, kintv
                do pl = m0l, m1l, kinto
                   do pd = m0d, m1d, kintv
                      do pm = m0m, m1m, kinto

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
                            call dotasks_32(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                                 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                                 n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                                 n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
                            ntasks = 0
                         end if
                      end do
                   end do
                end do
             end do
          end do
          if(ntasks .gt. 0)then
             call dotasks_32(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
                  n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, &
                  n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
                  n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_ntasks)
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


  end subroutine ccjac_32_dav_driver

  subroutine dotasks_32(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
       n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
       n0m, n1m, npair, nidx_ccsd, ntasks, r3limits, kj_n)

    procedure(dav_sigma_update_diag) :: sigup_diag
    procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag

    real(F64), dimension(:,:,:,:), intent(in)                       :: t2            
    integer, intent(in)                                             :: nocc0, nvirt0
    integer, intent(in)                                             :: nocc1, nvirt1
    integer, intent(in)                                             :: npair, nidx_ccsd
    integer, dimension(:,:), intent(in)                             :: r3limits

    integer, dimension(:), intent(in) :: n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e
    integer, dimension(:), intent(in) :: n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, n0m, n1m
    integer, intent(in) :: ntasks
    integer, dimension(:), intent(in) :: kj_n
    integer :: k

    !$omp parallel private(k) default(shared)    
    !$omp do schedule(dynamic)                                                        
    do k = 1, ntasks
       call ccjac_32_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a(k), n1a(k), n0b(k), n1b(k), n0c(k), n1c(k), n0d(k), n1d(k), n0e(k), n1e(k), &
            n0i(k), n1i(k), n0j(k), n1j(k), n0k(k), n1k(k), n0l(k), n1l(k), &
            n0m(k), n1m(k), nidx_ccsd + 1, npair + 1, r3limits(kj_n(k), 1))
    end do
    !$omp end do       
    !$omp end parallel

  end subroutine dotasks_32

  ! subroutine loop_boundaries(p, n, int, n0, n1)
  !        integer, intent(in) :: p
  !        integer, intent(in) :: n
  !        integer, intent(in) :: int
  !        integer, intent(out) :: n0, n1

  !        n0 = p
  !        n1 = min(n+1, p+int)-1

  !  end subroutine loop_boundaries



end module jacobian
