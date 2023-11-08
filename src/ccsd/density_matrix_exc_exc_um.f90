module density_matrix_exc_exc_um

      use cc3_intermediates
      use s_gen
      use basis
      use arithmetic
      use density_matrix_gr_exc
      use symmetry
      use density_exc_exc_functions_um
      use eom_vectors

      implicit none

      integer, private :: nocc0, nvirt0, nocc1, nvirt1, nocc, nvirt, npair
      integer, private :: qbj, qbj2
      integer, private :: qck, qck2
      integer, private :: q00
      integer, private :: nidx_ccsd
      integer, private :: nidx


contains

      subroutine density_exc_exc_init_um(nc0, nc1, nv0, nv1, nc, nv, nx)
            integer, intent(in) :: nc0, nc1
            integer, intent(in) :: nv0, nv1
            integer, intent(in) :: nc, nv
            integer, intent(in) :: nx

            nocc = nc
            nvirt = nv
            npair = nocc * nvirt
            nocc0 = nc0
            nocc1 = nc1
            nvirt0 = nv0
            nvirt1 = nv1
            nidx_ccsd = npair + (npair * (npair + 1)) / 2 
            nidx = nx

            qbj  = 3 + 6 * npair
            qbj2 = -3
            qck  = 2 + 3 * npair * (2 + npair)
            qck2 = -3 * (1 + npair)
            q00  = -3 * npair * (3 + npair)

      end subroutine density_exc_exc_init_um


      subroutine generate_density_exc_exc_um(rvecl, rvecu, t2, t1, s2, s1,  &
            nocc, nactive, method, dm_um, k1, k2, irrep0, irrep1, order)

            real(F64), dimension(:), intent(in)     :: rvecl
            real(F64), dimension(:), intent(in)     :: rvecu
            integer, intent(in)                       :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
            integer, intent(in)                                 :: method
            real(F64), dimension(:,:), intent(inout)  :: dm_um
            integer, intent(in) :: k1
            integer, intent(in) :: k2
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, intent(in) :: order
            integer, dimension(:, :), allocatable :: isingles_ij
            integer, dimension(:, :), allocatable :: isingles_ai
            integer, dimension(:, :), allocatable :: isingles_ia
            integer, dimension(:, :), allocatable :: isingles_ab
            integer, dimension(:, :), allocatable :: isingles
            integer :: idims_ij, idims_ai, idims_ia, idims_ab
            type(tclock) :: time, time1
            integer :: a, b, i, j, ij, ai, ia, ab
            integer :: ijbig, aibig, iabig, abbig
            integer :: n0i, n1i, n0j, n1j
            integer :: n0a, n1a, n0b, n1b
            integer :: m0i, m1i, m0j, m1j
            integer :: m0a, m1a, m0b, m1b
            integer :: ntasks
            integer :: pa, pb, pi, pj
            integer :: intv, into

            integer :: nthreads
            integer :: max_ntasks

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            allocate(isingles(max_ntasks, 2))


            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ij, POINT_GROUP, idims_ij, .true., 1, 1)
            allocate(isingles_ij(2, idims_ij))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ij, POINT_GROUP, idims_ij, .false., 1, 1)

            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ai, POINT_GROUP, idims_ai, .true., 2, 1)
            allocate(isingles_ai(2, idims_ai))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ai, POINT_GROUP, idims_ai, .false., 2, 1)

            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ia, POINT_GROUP, idims_ia, .true., 1, 2)
            allocate(isingles_ia(2, idims_ia))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ia, POINT_GROUP, idims_ia, .false., 1, 2)

            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ab, POINT_GROUP, idims_ab, .true., 2, 2)
            allocate(isingles_ab(2, idims_ab))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, 1, &
                  nocc, nocc+1, isingles_ab, POINT_GROUP, idims_ab, .false., 2, 2)


            ! print*, 'isingles - ij'
            ! do ijbig = 1, idims_ij
            !       print*, isingles_ij(1, ijbig), isingles_ij(2, ijbig)
            ! end do

            ! print*, 'isingles - ai'
            ! do aibig = 1, idims_ai
            !       print*, isingles_ai(1, aibig), isingles_ai(2, aibig)
            ! end do

            ! print*, 'isingles - ia'
            ! do iabig = 1, idims_ia
            !       print*, isingles_ia(1, iabig), isingles_ia(2, iabig)
            ! end do

            ! print*, 'isingles - ab'
            ! do abbig = 1, idims_ab
            !       print*, isingles_ab(1, abbig), isingles_ab(2, abbig)
            ! end do
            ! stop
            dm_um = zero

            print*, 'DENSITY FOR VECTOR...'
            call clock_start(time)
            ntasks = 0
            print*, 'LOOP OVER occ occ...'
            do ijbig = 1, idims_ij

                  call loop_boundaries_sp_qq(isingles_ij(1:2, ijbig), irrep0, irrep1, &
                        m0i, m1i, m0j, m1j, 1, 1)
                  
                  do pi = m0i, m1i
                        do pj = m0j, m1j
                              
                              ntasks = ntasks + 1
                              
                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pj
                              
                              if (ntasks == max_ntasks)  then
                                    
                                    call dotasks_dm_ij_um(method, dm_um(:,:), &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive)
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dm_ij_um(method, dm_um(:,:), &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive)                       
            end if

            call dmsg("TOTAL TIME IJBIG", clock_readwall(time))

            print*, 'LOOP OVER virt occ...'
            call clock_start(time)
            ntasks = 0

            do aibig = 1, idims_ai

                  call loop_boundaries_sp_qq(isingles_ai(1:2, aibig), irrep0, irrep1, &
                        m0a, m1a, m0i, m1i, 2, 1)


                  do pa = m0a, m1a
                        do pi = m0i, m1i

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pa
                              isingles(ntasks, 2) = pi

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dm_ai_um(method, dm_um(:,:), &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then
                  call dotasks_dm_ai_um(method, dm_um(:,:), &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive)
            end if
            call dmsg("TOTAL TIME AIBIG", clock_readwall(time))

            print*, 'LOOP OVER occ virt...'
            call clock_start(time)
            ntasks = 0
            do iabig = 1, idims_ia
                  call loop_boundaries_sp_qq(isingles_ia(1:2, iabig), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a, 1, 2)

                  do pa = m0a, m1a
                        do pi = m0i, m1i

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pa

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dm_ia_um(method, dm_um(:,:), &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dm_ia_um(method, dm_um(:,:), &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive)

            end if
            call dmsg("TOTAL TIME IABIG", clock_readwall(time))                  

            print*, 'LOOP OVER virt virt...'
            call clock_start(time)
            ntasks = 0                              
            do abbig = 1, idims_ab
                  call loop_boundaries_sp_qq(isingles_ab(1:2, abbig), irrep0, irrep1, &
                        m0a, m1a, m0b, m1b, 2, 2)

                  do pa = m0a, m1a
                        do pb = m0b, m1b

                              ntasks = ntasks + 1
                              isingles(ntasks, 1) = pa
                              isingles(ntasks, 2) = pb

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dm_ab_um(method, dm_um(:,:), &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dm_ab_um(method, dm_um(:,:), &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive)

            end if
            call dmsg("TOTAL TIME ABBIG", clock_readwall(time))

      end subroutine generate_density_exc_exc_um

     subroutine dotasks_dm_ij_um(theory, dm_um, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_um
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvecl
           real(F64), dimension(:), intent(in) :: rvecu
           real(F64), dimension(:, :, :, :), intent(in) :: t2 
           real(F64), dimension(:, :, :, :), intent(in) :: s2 
           real(F64), dimension(:, :), intent(in)                  :: t1 
           real(F64), dimension(:, :), intent(in)                  :: s1
           integer, intent(in) :: k1
           integer, intent(in) :: k2
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, j
           
           ! if (theory .eq. THEORY_CC3) then
                 
           !       !$omp parallel private(k, i, j) default(shared)                                                                                        
           !       !$omp do schedule(dynamic)                                                                                                                  
           !       do k = 1, ntasks
           !             i = isingles(k, 1)
           !             j = isingles(k, 2)
           !             dm_um(i, j)  = calc_D_oo_um_cc3(t2, t1, s2, s1, &
           !                   rvec, nocc, nactive, i, j)
           !       end do
           !       !$omp end do                                                                                                                                   
           !       !$omp end parallel   

           ! else 
                 if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, j) default(shared)                                                                         
                 !$omp do schedule(dynamic)                                                                                                                 
                 do k = 1, ntasks          

                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_um(i, j)  = dm_um(i, j) + calc_D_oo_um(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, j) 

                 end do
                 !$omp end do                                                                                                                                 
                 !$omp end parallel 
           end if

     end subroutine dotasks_dm_ij_um

     subroutine dotasks_dm_ai_um(theory, dm_um, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_um
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvecl
           real(F64), dimension(:), intent(in) :: rvecu
           real(F64), dimension(:, :, :, :), intent(in) :: t2 
           real(F64), dimension(:, :, :, :), intent(in) :: s2 
           real(F64), dimension(:, :), intent(in)                  :: t1 
           real(F64), dimension(:, :), intent(in)                  :: s1
           integer, intent(in) :: k1
           integer, intent(in) :: k2
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: a, i

           ! if (theory .eq. THEORY_CC3) then
           !       !$omp parallel private(k, a, i) default(shared)                                                            
           !       !$omp do schedule(dynamic)                                                                                                                       
           !       do k = 1, ntasks
           !             a = isingles(k, 1)
           !             i = isingles(k, 2)
           !             dm_um(a, i)  = calc_D_vo_um_cc3(t2, s2, s1, &
           !                   rvec, nocc, nactive, a, i)
           !       end do
           !       !$omp end do                                                                                                                                     
           !       !$omp end parallel                                                                                                                               
           ! else 
           if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, i) default(shared)                                                                                                    
                 !$omp do schedule(dynamic)                                                                                                                         
                    
                 do k = 1, ntasks

                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  

                       dm_um(a, i)  = dm_um(a, i) + calc_D_vo_um(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, i) 
                 end do
                 !$omp end do                                                                                                                                      
                 !$omp end parallel 
           end if
     end subroutine dotasks_dm_ai_um

     subroutine dotasks_dm_ia_um(theory, dm_um, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_um
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvecl
           real(F64), dimension(:), intent(in) :: rvecu
           real(F64), dimension(:, :, :, :), intent(in) :: t2 
           real(F64), dimension(:, :, :, :), intent(in) :: s2 
           real(F64), dimension(:, :), intent(in)                  :: t1 
           real(F64), dimension(:, :), intent(in)                  :: s1
           integer, intent(in) :: k1
           integer, intent(in) :: k2
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, a

           ! if (theory .eq. THEORY_CC3) then
           !       !$omp parallel private(k, i, a) default(shared)                                                                                       
           !       !$omp do schedule(dynamic)                                                                                                                       
           !       do k = 1, ntasks
           !             i = isingles(k, 1)
           !             a = isingles(k, 2)
           !             dm_um(i, a)  = calc_D_ov_um_cc3(t2, s2, &
           !                   rvec, nocc, nactive, i, a)
           !       end do
           !       !$omp end do                                                                                                                                     
           !       !$omp end parallel                                                                                                                               
           ! else 
                 if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, a) default(shared)                                                                                                    
                 !$omp do schedule(dynamic)                                                                                                                         
                    
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_um(i, a) = dm_um(i, a) + calc_D_ov_um(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, a) 
                 end do
                 !$omp end do                                                                                                                                      
                 !$omp end parallel                                                                                                                                 
                     
           end if
     end subroutine dotasks_dm_ia_um

      subroutine dotasks_dm_ab_um(theory, dm_um, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive)

            integer, intent(in)                   :: theory
            real(F64), dimension(:,:)             :: dm_um
            integer, dimension(:,:), intent(in)   :: isingles
            integer, intent(in)                   :: nocc
            real(F64), dimension(:), intent(in) :: rvecl
            real(F64), dimension(:), intent(in) :: rvecu
            real(F64), dimension(:, :, :, :), intent(in) :: t2 
            real(F64), dimension(:, :, :, :), intent(in) :: s2 
            real(F64), dimension(:, :), intent(in)                  :: t1 
            real(F64), dimension(:, :), intent(in)                  :: s1
            integer, intent(in) :: k1
            integer, intent(in) :: k2
            integer, intent(in)                   :: ntasks
            integer, intent(in)                   :: nactive
            integer :: k
            integer :: a, b

           ! if (theory .eq. THEORY_CC3) then
           !       !$omp parallel private(k, a, b) default(shared)                                                                                                  
           !       !$omp do schedule(dynamic)                                                                                                                       
           !       do k = 1, ntasks
           !             a = isingles(k, 1)
           !             b = isingles(k, 2)
           !             dm_um(a, b) = calc_D_vv_um_cc3(t2, t1, s2, s1, &
           !                   rvec, nocc, nactive, a, b)
           !       end do
           !       !$omp end do                                                                                                                                     
           !       !$omp end parallel                                                                                                                               
           ! else 
            if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, b) default(shared)                                                                                                    
                 !$omp do schedule(dynamic)                                                                                                                         
                    
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dm_um(a, b)  = dm_um(a, b) + calc_D_vv_um(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b) 
                 end do
                 !$omp end do                                                                                                                                      
                 !$omp end parallel 
           end if

     end subroutine dotasks_dm_ab_um






end module density_matrix_exc_exc_um
