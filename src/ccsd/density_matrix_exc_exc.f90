module density_matrix_exc_exc

      use cc3_intermediates
      use gparam
      use s_gen
      use basis
      use arithmetic
      ! use density_matrix_gr_exc
      use symmetry
      !      use density_exc_exc_functions
 !      use density_exc_exc_functions_so_pt0123_sym
 !      use density_exc_exc_functions_so_pt0123_asym
       use density_exc_exc_functions_so_left_pt0123
 !      use density_exc_exc_functions_cc3_so_pt0123
      
 !      use density_exc_exc_functions_pt0123      
 !      use density_exc_exc_functions_pt4
 !      use density_exc_exc_functions_triplet_pt012
 !      use density_exc_exc_functions_triplet_pt3
 !      use density_exc_exc_functions_triplet_pt4
 !      use density_exc_exc_functions_triplet
 ! !     use density_exc_exc_functions_cc3
 !      use density_exc_exc_functions_cc3_triplet_pt0123
 !      use density_exc_exc_functions_cc3_triplet_pt4
 !      use density_exc_exc_functions_cc3_pt0123
 !      use density_exc_exc_functions_cc3_pt4
      use eom_vectors


      use ss_ccsd_pt012
      use ss_ccsd_pt3
      use ss_ccsd_pt4
      
      use ss_cc3_pt012
      use ss_cc3_pt3a
      use ss_cc3_pt3b
      use ss_cc3_pt4

      use tt_ccsd_pt012a
      use tt_ccsd_pt012b
      use tt_ccsd_pt3a
      use tt_ccsd_pt3b     
      use tt_ccsd_pt4a
      use tt_ccsd_pt4b
      use tt_ccsd_pt4c
      use tt_ccsd_pt4d

      use tt_cc3_pt012a
      use tt_cc3_pt012b
      use tt_cc3_pt3a
      use tt_cc3_pt3b
      use tt_cc3_pt3c
      use tt_cc3_pt4

      use so_ccsd_pt012
      use so_ccsd_pt3
      use so_ccsd_pt4a
      use so_ccsd_pt4b
      
      use so_cc3_pt012
      use so_cc3_pt3a
      use so_cc3_pt3b
      use so_cc3_pt4

      implicit none

      integer, parameter, private :: sym = 1

contains

      function tm_exc_exc(tm_wl, tm_wm, tm_wr, sl, sr)

            real(F64) :: tm_exc_exc
            real(F64), intent(in) :: tm_wm
            real(F64), intent(in) :: tm_wl
            real(F64), intent(in) :: tm_wr
            real(F64), intent(in) :: sl
            real(F64), intent(in) :: sr


            print*, '-sl', -sl
            print*, 'sr', sr
            print*, '-sl*sr', -sl * sr
            print*, 'sqrt', sqrt(-sl * sr)
            print*, 'tm_wl', tm_wl
            print*, 'tm_wr', tm_wr
            print*, 'tm_wm', tm_wm
            print*, 'il', tm_wl * tm_wm * tm_wr
            tm_exc_exc = tm_wl * tm_wm * tm_wr / sqrt(-sl * sr)

      end function tm_exc_exc

      subroutine generate_density_exc_exc_wm(rvecl, rvecu, t2, t1, s2, s1,  &
            nocc, nactive, method, dm_wm, k1, k2, irrep0, irrep1, order, maxpt)

            real(F64), dimension(:), intent(in)     :: rvecl
            real(F64), dimension(:), intent(in)     :: rvecu
            integer, intent(in)                       :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
            integer, intent(in)                                 :: method
            real(F64), dimension(:,:), intent(inout)  :: dm_wm
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
            integer, intent(in) :: maxpt

            integer :: idims_ij, idims_ai, idims_ia, idims_ab
            type(tclock) :: time
            integer :: ijbig, aibig, iabig, abbig
            integer :: m0i, m1i, m0j, m1j
            integer :: m0a, m1a, m0b, m1b
            integer :: ntasks
            integer :: pa, pb, pi, pj

            integer :: nthreads
            integer :: max_ntasks
            integer :: nvirt


            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            allocate(isingles(max_ntasks, 2))


            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ij, POINT_GROUP, idims_ij, .true., 1, 1)
            allocate(isingles_ij(2, idims_ij))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ij, POINT_GROUP, idims_ij, .false., 1, 1)

            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ai, POINT_GROUP, idims_ai, .true., 2, 1)
            allocate(isingles_ai(2, idims_ai))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ai, POINT_GROUP, idims_ai, .false., 2, 1)

            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ia, POINT_GROUP, idims_ia, .true., 1, 2)
            allocate(isingles_ia(2, idims_ia))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ia, POINT_GROUP, idims_ia, .false., 1, 2)

            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ab, POINT_GROUP, idims_ab, .true., 2, 2)
            allocate(isingles_ab(2, idims_ab))
            call irrep_singless_pq_exc_exc(k1, k2, irrep0, irrep1, &
                  isingles_ab, POINT_GROUP, idims_ab, .false., 2, 2)



!            dm_wm = zero

            call msg('GENERATING DENSITY MATRIXC EXC EXC')
            call clock_start(time)
            ntasks = 0
            call msg('LOOP OVER occ occ...')
            do ijbig = 1, idims_ij
                  call loop_boundaries_sp_qq(isingles_ij(:, ijbig), irrep0, irrep1, &
                        m0i, m1i, m0j, m1j, 1, 1)

                  do pi = m0i, m1i
                        do pj = m0j, m1j
                              ntasks = ntasks + 1
                              
                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pj

                              
                              if (ntasks == max_ntasks)  then
                                    call dotasks_dm_ij_wm(method, dm_wm, &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then
                  call dotasks_dm_ij_wm(method, dm_wm, &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)                       
            end if
            
            call dmsg("DONE. TIME:", clock_readwall(time))


            call msg('LOOP OVER virt occ...')
            call clock_start(time)
            ntasks = 0

            do aibig = 1, idims_ai

                  call loop_boundaries_sp_qq(isingles_ai(:, aibig), irrep0, irrep1, &
                        m0a, m1a, m0i, m1i, 2, 1)

                  do pa = m0a, m1a
                        do pi = m0i, m1i

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pa
                              isingles(ntasks, 2) = pi

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dm_ai_wm(method, dm_wm, &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then
                  call dotasks_dm_ai_wm(method, dm_wm, &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)
            end if
            call dmsg("DONE. TIME:", clock_readwall(time))


            call msg('LOOP OVER occ virt...')
            call clock_start(time)

            ntasks = 0
            do iabig = 1, idims_ia

                  call loop_boundaries_sp_qq(isingles_ia(:, iabig), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a, 1, 2)

                  do pa = m0a, m1a
                        do pi = m0i, m1i

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pa

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dm_ia_wm(method, dm_wm, &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dm_ia_wm(method, dm_wm, &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)


            end if
            call dmsg("DONE. TIME:", clock_readwall(time))                  


            call msg('LOOP OVER virt virt...')
            call clock_start(time)

            ntasks = 0            
            do abbig = 1, idims_ab
                  call loop_boundaries_sp_qq(isingles_ab(:, abbig), irrep0, irrep1, &
                        m0a, m1a, m0b, m1b, 2, 2)
                  do pa = m0a, m1a
                        do pb = m0b, m1b

                              ntasks = ntasks + 1
                              isingles(ntasks, 1) = pa
                              isingles(ntasks, 2) = pb

                              if (ntasks == max_ntasks)  then
                                    call dotasks_dm_ab_wm(method, dm_wm, &
                                          isingles, nocc, rvecl, rvecu, &
                                          t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dm_ab_wm(method, dm_wm, &
                        isingles, nocc, rvecl, rvecu, &
                        t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

            end if
            call dmsg("DONE. TIME:", clock_readwall(time))


      end subroutine generate_density_exc_exc_wm

     subroutine dotasks_dm_ij_wm(theory, dm_wm, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

           integer, intent(in)                     :: theory
           real(F64), dimension(:,:), intent(inout):: dm_wm
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
           integer, intent(in) :: maxpt
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, j
           

           select case(maxpt)
           case(0)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                       if (abs(dm_wm(i, j)).gt.1.d-5)then                                                                                                                              
!                             print*, i, j, dm_wm(i, j)
                       end if 
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_cc3_pt0(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_triplet_cc3_pt0(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, i, j) default(shared)                                           
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                          i = isingles(k, 1)
                          j = isingles(k, 2)
                          dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                               rvecl, rvecu, k1, k2, i, j)
                       end do
                       !$omp end do                                                                                       
                       !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                              
                    !$omp do schedule(dynamic)                                              
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_so_cc3_pt0(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do      
                    !$omp end parallel
                 end if
           else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, i, j) default(shared)                                                            
                       !$omp do schedule(dynamic)                                                                                     
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             j = isingles(k, 2)
                             dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, j)
                             print*, 'dmwmdmwm', i, j, dm_wm(i, j)
                       end do
                       !$omp end do                                                                                   
                       !$omp end parallel    
                 end if
           end if

           case(1)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_cc3_pt1(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_triplet_cc3_pt1(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, i, j) default(shared)                                                                             
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             j = isingles(k, 2)                                    
                             dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, j)
                       end do
                       !$omp end do                                                                                                       
                       !$omp end parallel

                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                            
                    !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_so_cc3_pt1(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                               
                    !$omp end parallel
                 end if
                    else if (cc_multip == cc_mixed_left) then
                    if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                                              
                    do k = 1, ntasks
                       i = isingles(k, 1)
                              j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, j)
                       !if (abs(dm_wm(i, j)).gt.1.d-3)then                                                                                                   
                       print*, i, j, dm_wm(i, j)
                       !end if                                                                                                                               
                 end do
                 !$omp end do                                                                                                            
                 !$omp end parallel                                                                                                      
           end if

              end if
           case(2)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_cc3_pt2(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_triplet_cc3_pt2(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, i, j) default(shared)                                                                       
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             j = isingles(k, 2)
                             
                             dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, j)
                       end do
                       !$omp end do                              
                       !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)
                    !$omp do schedule(dynamic)
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_so_cc3_pt2(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                           
                    !$omp end parallel
                 end if
                    else if (cc_multip == cc_mixed_left) then
                    if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                                              
                    do k = 1, ntasks
                       i = isingles(k, 1)
                              j = isingles(k, 2)
                              dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, j)
                       !if (abs(dm_wm(i, j)).gt.1.d-3)then                                                                                                   
                       print*, i, j, dm_wm(i, j)
                       !end if                                                                                                                               
                 end do
                 !$omp end do                                                                                                            
                 !$omp end parallel                                                                                                      
           end if
              end if
           case(3)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)
                    !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)
                    !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_cc3_pt3(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_triplet_cc3_pt3(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, i, j) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             j = isingles(k, 2)
                             dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, j)
                       end do
                       !$omp end do
                       !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)
                    !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_so_cc3_pt3(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do
                    !$omp end parallel
                 end if
                    else if (cc_multip == cc_mixed_left) then
                    if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                                              
                          do k = 1, ntasks
                       i = isingles(k, 1)
                              j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, j)
                       !if (abs(dm_wm(i, j)).gt.1.d-3)then                                                                                                   
                       print*, i, j, dm_wm(i, j)
                       !end if                                                                                                                               
                 end do
                 !$omp end do                                                                                                            
                 !$omp end parallel                                                                                                     
           end if

              end if
           case(4)
                 if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_cc3_pt4(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                       
                    !$omp do schedule(dynamic)                                                                                                                  
                    
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) 

                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)                                                                                        
                    !$omp do schedule(dynamic)                                                                                                                  
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_triplet_cc3_pt4(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                                   
                    !$omp end parallel
                 end if

              else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, i, j) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       
                       dm_wm(i, j)  = dm_wm(i, j)  + calc_D_oo_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do                                                                                                                       
                    !$omp end parallel        
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, j) default(shared)
                    !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_wm(i, j)  = dm_wm(i, j) + calc_D_oo_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, j) + &
                            calc_D_oo_wm_so_cc3_pt4(t2, t1, s2, s1, &
                            nocc, nactive, rvecl, rvecu, k1, k2, i, j)
                    end do
                    !$omp end do
                    !$omp end parallel
                 end if
              end if
           end select
     end subroutine dotasks_dm_ij_wm

     subroutine dotasks_dm_ai_wm(theory, dm_wm, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_wm
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
           integer, intent(in)                   :: maxpt
           
           integer :: k
           integer :: a, i

           
           select case(maxpt)
           case(0)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, i)

!                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet(t2, t1, s2, s1, nocc, nactive, &
 !                           rvecl, rvecu, k1, k2, a, i)

                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)  
                        dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, i)
                     end do
                     !$omp end do
                     !$omp end parallel

                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)                                                                                 
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)
                        dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                     end do
                     !$omp end do                                 
                     !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed_left) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, i) default(shared)                                                                                          
                     !$omp do schedule(dynamic)                                                                                                               
                     do k = 1, ntasks
                                           a = isingles(k, 1)
                        i = isingles(k, 2)
                        dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, i)
!                        print*, a, i, dm_wm(a, i)
                     end do
                     !$omp end do                                                                                                                             
                     !$omp end parallel  
               end if
              end if
           case(1)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)
                    !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)  
                        dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, i)
                     end do
                     !$omp end do                                                                                                      
                     !$omp end parallel 
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)                                                                                 
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)  
                        dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, i)
                     end do
                     !$omp end do                                                                                                      
                     !$omp end parallel
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)                                                                                 
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
           else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)                                                                                                           
                        do k = 1, ntasks
                              a = isingles(k, 1)
                              i = isingles(k, 2)
                              dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, &
                                    rvecl, rvecu, k1, k2, a, i)
                              print*, a, i, dm_wm(a, i)
                        end do
                        !$omp end do                        
                        !$omp end parallel                                                                                                                   
                  end if
            end if
            
           case(2)
                            if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
           else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                              a = isingles(k, 1)
                              i = isingles(k, 2)
                              dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, &
                                    rvecl, rvecu, k1, k2, a, i)
                              print*, a, i, dm_wm(a, i)
                        end do
                        !$omp end do
                        !$omp end parallel
                  end if

              end if

           case(3)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                     
                    !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
           else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                              a = isingles(k, 1)
                              i = isingles(k, 2)
                              dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, &
                                    rvecl, rvecu, k1, k2, a, i)
                              print*, a, i, dm_wm(a, i)
                        end do
                        !$omp end do
                        !$omp end parallel
                  end if



              end if

           case(4)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                
                    !$omp do schedule(dynamic)                                                                                                               
                    
                    do k = 1, ntasks
                       
                       a = isingles(k, 1)                                                                                                                           
                       i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt4a(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_triplet_pt4b(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)

                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_triplet_pt4a(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_triplet_pt4b(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)+ &
                            calc_D_vo_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, i) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        a = isingles(k, 1)                                                                                              
                        i = isingles(k, 2)  
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                                                                                                                      
                    !$omp end parallel 
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                         
                    !$omp do schedule(dynamic)                                                                                 
                    do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_wm(a, i)  = dm_wm(a, i) + calc_D_vo_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i) + &
                            calc_D_vo_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, a, i)
                    end do
                    !$omp end do                                 
                    !$omp end parallel
                 end if
              end if
           end select

     end subroutine dotasks_dm_ai_wm

     subroutine dotasks_dm_ia_wm(theory, dm_wm, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_wm
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
           integer, intent(in)                   :: maxpt
           
           integer :: k
           integer :: i, a

           select case(maxpt)
           case(0)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_triplet) then
                                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, a)

                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                        
                          !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                 
                    !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
           else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             a = isingles(k, 2)
                             dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, a)
                       end do
                       !$omp end do
                       !$omp end parallel
                 end if
              end if
           case(1)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_triplet) then
                                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                             !$omp parallel private(k, a, i) default(shared)
                             !$omp do schedule(dynamic)
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do
                    !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
           else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             a = isingles(k, 2)
                            dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, a)
                       end do
                       !$omp end do
                       !$omp end parallel
                 end if


              end if
           case(2)
                            if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_triplet) then
                                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                          !$omp parallel private(k, a, i) default(shared)
                             !$omp do schedule(dynamic)                                                                                          
                             do k = 1, ntasks
                                   i = isingles(k, 1)
                                   a = isingles(k, 2)
                                   dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                         
                    !$omp end parallel
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
           else if (cc_multip == cc_mixed_left) then
              if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             a = isingles(k, 2)
                             dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, a)
                       end do
                       !$omp end do
                       !$omp end parallel
                 end if


              end if
           case(3)
                            if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_triplet) then
                                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)                                                                                             
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             a = isingles(k, 2)
                             dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, a)
                       end do
                       !$omp end do                                                                                                     
                       !$omp end parallel

                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
           else if (cc_multip == cc_mixed_left) then
              if (theory .eq. THEORY_CCSD) then
       
                       !$omp parallel private(k, a, i) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             i = isingles(k, 1)
                             a = isingles(k, 2)
                             dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, i, a)
                       end do
                       !$omp end do
                       !$omp end parallel
                 end if

              end if
           case(4)
              if (cc_multip == cc_singlet) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_triplet) then
                                  if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)                                                                                                    
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if
              else if (cc_multip == cc_mixed) then
                 if (theory .eq. THEORY_CCSD) then
                    !$omp parallel private(k, a, i) default(shared)
                    !$omp do schedule(dynamic)                                                                                                                         
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                                                             
                    !$omp end parallel                
                 else if (theory .eq. THEORY_CC3) then
                    !$omp parallel private(k, i, a) default(shared)                                                                               
                    !$omp do schedule(dynamic)                                                                                                                                        
                    do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_wm(i, a)  = dm_wm(i, a) + calc_D_ov_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a) + &
                            calc_D_ov_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                            rvecl, rvecu, k1, k2, i, a)
                    end do
                    !$omp end do                                                                                                       
                    !$omp end parallel            
                 end if

              end if
           end select

         end subroutine dotasks_dm_ia_wm

      subroutine dotasks_dm_ab_wm(theory, dm_wm, isingles, nocc, rvecl, rvecu, &
            t1, t2, s1, s2, k1, k2, ntasks, nactive, maxpt)

            integer, intent(in)                   :: theory
            real(F64), dimension(:,:)             :: dm_wm
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
            integer, intent(in)                   :: nactive, maxpt

            integer :: k
            integer :: a, b
            real(F64) :: time0_total, time1_total


            select case(maxpt)
            case(0)
               if (cc_multip == cc_singlet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                     
                     !$omp do schedule(dynamic)                     
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                        
                     !$omp do schedule(dynamic)                     
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)

                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                        
                     !$omp do schedule(dynamic)                        
                     do k = 1, ntasks

                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)

                     end do
                     !$omp end do                        
                     !$omp end parallel                        
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                                 
                                 a = isingles(k, 1)
                                 b = isingles(k, 2)
                                 dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                                        rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                          
                     !$omp end parallel   

                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_so_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
           else if (cc_multip == cc_mixed_left) then
              if (theory .eq. THEORY_CCSD) then
           
                       !$omp parallel private(k, a, b) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks
                             
                             a = isingles(k, 1)
                             b = isingles(k, 2)
                             dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, a, b)
                       end do


                       !$omp end do
                       !$omp end parallel
                    end if
                 end if
            case(1)
               if (cc_multip == cc_singlet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_triplet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                            
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                        !$omp parallel private(k, a, b) default(shared)                                                                             
                        !$omp do schedule(dynamic)
                        do k = 1, ntasks                                       
                           a = isingles(k, 1)
                           b = isingles(k, 2)
                           dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                                rvecl, rvecu, k1, k2, a, b)
                        end do
                        !$omp end do                                                                                                                
                        !$omp end parallel
                     else if (theory .eq. THEORY_CC3) then
                        !$omp parallel private(k, a, b) default(shared)                                                                               
                        !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_so_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do
                     !$omp end parallel   
                  end if
            else if (cc_multip == cc_mixed_left) then
               if (theory .eq. THEORY_CCSD) then
                  
                        !$omp parallel private(k, a, b) default(shared)
                        !$omp do schedule(dynamic)
                        do k = 1, ntasks
                             a = isingles(k, 1)
                             b = isingles(k, 2)
                             dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, a, b)
                       end do
                       !$omp end do
                       !$omp end parallel
                    end if
                 end if
            case(2)
               if (cc_multip == cc_singlet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                     
                     !$omp do schedule(dynamic)                                                                                                      
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_triplet) then
                                    if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                                              
                     
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)
                     !$omp do schedule(dynamic)
                     do k = 1, ntasks                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                             
                     !$omp end parallel
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_so_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)

                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
            else if (cc_multip == cc_mixed_left) then
                  if (theory .eq. THEORY_CCSD) then
                        !$omp parallel private(k, a, b) default(shared)
                        !$omp do schedule(dynamic)
                        do k = 1, ntasks                              
                              a = isingles(k, 1)
                              b = isingles(k, 2)
                              dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, &
                                    rvecl, rvecu, k1, k2, a, b)
                       end do
                       !$omp end do
                       !$omp end parallel
                 end if

              end if
            case(3)
                             if (cc_multip == cc_singlet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                                              
                     
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_triplet) then
                                    if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                                              
                     
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                              !$omp parallel private(k, a, b) default(shared)                                                                  
                              !$omp do schedule(dynamic)                                                                     
                           do k = 1, ntasks
                                    a = isingles(k, 1)
                                    b = isingles(k, 2)
                                    dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                                          rvecl, rvecu, k1, k2, a, b)
                              end do
                              !$omp end do                                                                                  
                              !$omp end parallel
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
   else if (cc_multip == cc_mixed_left) then
                 if (theory .eq. THEORY_CCSD) then
                       !$omp parallel private(k, a, b) default(shared)
                       !$omp do schedule(dynamic)
                       do k = 1, ntasks

                             a = isingles(k, 1)
                             b = isingles(k, 2)
                             dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, &
                                   rvecl, rvecu, k1, k2, a, b)
                       end do


                       !$omp end do
                       !$omp end parallel
                 end if

               end if
            case(4)
               if (cc_multip == cc_singlet) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                                              
                     
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if
               else if (cc_multip == cc_triplet) then
                                    if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                                              
                     
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do                                                                                                                                              
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                               
                     !$omp do schedule(dynamic)                                                                                                                                       
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                                           
                     !$omp end parallel   
                  end if


               else if (cc_multip == cc_mixed) then
                  if (theory .eq. THEORY_CCSD) then
                     !$omp parallel private(k, a, b) default(shared)                                                                                                          
                     !$omp do schedule(dynamic)                                                                                                                              
                     
                     do k = 1, ntasks
                        
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b)  = dm_wm(a, b) + calc_D_vv_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                     end do
                     !$omp end do
                     !$omp end parallel    
                  else if (theory .eq. THEORY_CC3) then
                     !$omp parallel private(k, a, b) default(shared)
                     !$omp do schedule(dynamic)
                     
                     do k = 1, ntasks
                        a = isingles(k, 1)
                        b = isingles(k, 2)
                        dm_wm(a, b) = dm_wm(a, b) + calc_D_vv_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)+&
                             calc_D_vv_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, &
                             rvecl, rvecu, k1, k2, a, b)
                        
                     end do
                     !$omp end do                                                                                                  
                     !$omp end parallel   
                  end if

               end if
          end select

 
     end subroutine dotasks_dm_ab_wm


!       subroutine generate_density_exc_exc_wlr(lvec, t2, t1, s2, s1,  &
!             nocc, nactive, method, dm_wl, irrep_idx, irrep0, irrep1, order)

!             real(F64), dimension(:), intent(in)     :: lvec
!             integer, intent(in)                       :: nocc, nactive
!             real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!             real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!             real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!             real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
!             integer, intent(in)                                 :: method
!             real(F64), dimension(:,:), intent(inout)  :: dm_wl
!             integer, intent(in) :: irrep_idx
!             integer, dimension(:, :), intent(in) :: irrep0
!             integer, dimension(:, :), intent(in) :: irrep1
!             integer, intent(in) :: order
!             integer, dimension(:, :), allocatable :: isingles_ij
!             integer, dimension(:, :), allocatable :: isingles_ai
!             integer, dimension(:, :), allocatable :: isingles_ia
!             integer, dimension(:, :), allocatable :: isingles_ab
!             integer, dimension(:, :), allocatable :: isingles
!             integer :: idims_ij, idims_ai, idims_ia, idims_ab
!             type(tclock) :: time, time1
!             integer :: a, b, i, j, ij, ai, ia, ab
!             integer :: ijbig, aibig, iabig, abbig
!             integer :: n0i, n1i, n0j, n1j
!             integer :: n0a, n1a, n0b, n1b
!             integer :: m0i, m1i, m0j, m1j
!             integer :: m0a, m1a, m0b, m1b
!             integer :: ntasks
!             integer :: pa, pb, pi, pj
!             integer :: intv, into

!             integer :: nthreads
!             integer :: max_ntasks

!             nthreads = OMP_NTHREAD
!             max_ntasks = 100 * nthreads
!             allocate(isingles(max_ntasks, 2))


!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ij, POINT_GROUP, idims_ij, .true., 1, 1)
!             allocate(isingles_ij(2, idims_ij))
!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ij, POINT_GROUP, idims_ij, .false., 1, 1)

!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ai, POINT_GROUP, idims_ai, .true., 2, 1)
!             allocate(isingles_ai(2, idims_ai))
!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ai, POINT_GROUP, idims_ai, .false., 2, 1)

!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ia, POINT_GROUP, idims_ia, .true., 1, 2)
!             allocate(isingles_ia(2, idims_ia))
!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ia, POINT_GROUP, idims_ia, .false., 1, 2)


!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ab, POINT_GROUP, idims_ab, .true., 2, 2)
!             allocate(isingles_ab(2, idims_ab))
!             call irrep_singless_pq(irrep_idx, irrep0, irrep1, 1, nocc, nocc+1, isingles_ab, POINT_GROUP, idims_ab, .false., 2, 2)

!             dm_wl = zero

! !            'DENSITY FOR VECTOR...'
!             call clock_start(time)
!             ntasks = 0
!             print*, 'LOOP OVER occ occ...'
!             do ijbig = 1, idims_ij
!                   call loop_boundaries_sp_qq(isingles_ij(1:2, ijbig), irrep0, irrep1, &
!                         m0i, m1i, m0j, m1j, 1, 1)
                  
!                   do pi = m0i, m1i
!                         do pj = m0j, m1j
                              
!                               ntasks = ntasks + 1
                              
!                               isingles(ntasks, 1) = pi
!                               isingles(ntasks, 2) = pj
                              
!                               if (ntasks == max_ntasks)  then
                                    
!                                     call dotasks_dm_ij_wl(method, dm_wl(:,:), &
!                                           isingles, nocc, lvec, &
!                                           t1, t2, s1, s2, ntasks, nactive)
!                                     ntasks = 0
!                               end if
!                         end do
!                   end do
!             end do

!             if (ntasks .gt. 0)  then
!                   call dotasks_dm_ij_wl(method, dm_wl(:,:), &
!                         isingles, nocc, lvec, &
!                         t1, t2, s1, s2, ntasks, nactive)                        
!             end if

!             call dmsg("TOTAL TIME IJBIG", clock_readwall(time))

!             print*, 'LOOP OVER virt occ...'
!             call clock_start(time)
!             ntasks = 0
!             do aibig = 1, idims_ai
!                   call loop_boundaries_sp_qq(isingles_ai(1:2, aibig), irrep0, irrep1, &
!                         m0a, m1a, m0i, m1i, 2, 1)


!                   do pa = m0a, m1a
!                         do pi = m0i, m1i

!                               ntasks = ntasks + 1

!                               isingles(ntasks, 1) = pa
!                               isingles(ntasks, 2) = pi

!                               if (ntasks == max_ntasks)  then

!                                     call dotasks_dm_ai_wl(method, dm_wl(:,:), &
!                                           isingles, nocc, lvec, &
!                                           t1, t2, s1, s2, ntasks, nactive)

!                                     ntasks = 0
!                               end if
!                         end do
!                   end do
!             end do

!             if (ntasks .gt. 0)  then
!                   call dotasks_dm_ai_wl(method, dm_wl(:,:), &
!                         isingles, nocc, lvec, &
!                         t1, t2, s1, s2, ntasks, nactive)
!             end if
!             call dmsg("TOTAL TIME AIBIG", clock_readwall(time))

!             print*, 'LOOP OVER occ virt...'
!             call clock_start(time)
!             ntasks = 0
!             do iabig = 1, idims_ia
!                   call loop_boundaries_sp_qq(isingles_ia(1:2, iabig), irrep0, irrep1, &
!                         m0i, m1i, m0a, m1a, 1, 2)

!                   do pa = m0a, m1a
!                         do pi = m0i, m1i

!                               ntasks = ntasks + 1

!                               isingles(ntasks, 1) = pi
!                               isingles(ntasks, 2) = pa

!                               if (ntasks == max_ntasks)  then

!                                     call dotasks_dm_ia_wl(method, dm_wl(:,:), &
!                                           isingles, nocc, lvec, &
!                                           t1, t2, s1, s2, ntasks, nactive)

!                                     ntasks = 0
!                               end if
!                         end do
!                   end do
!             end do

!             if (ntasks .gt. 0)  then

!                   call dotasks_dm_ia_wl(method, dm_wl(:,:), &
!                         isingles, nocc, lvec, &
!                         t1, t2, s1, s2, ntasks, nactive)

!             end if
!             call dmsg("TOTAL TIME IABIG", clock_readwall(time))                  

!             print*, 'LOOP OVER virt virt...'
!             call clock_start(time)
!             ntasks = 0                              
!             do abbig = 1, idims_ab
!                   call loop_boundaries_sp_qq(isingles_ab(1:2, abbig), irrep0, irrep1, &
!                         m0a, m1a, m0b, m1b, 2, 2)

!                   do pa = m0a, m1a
!                         do pb = m0b, m1b

!                               ntasks = ntasks + 1
!                               isingles(ntasks, 1) = pa
!                               isingles(ntasks, 2) = pb

!                               if (ntasks == max_ntasks)  then

!                                     call dotasks_dm_ab_wl(method, dm_wl(:,:), &
!                                           isingles, nocc, lvec, &
!                                           t1, t2, s1, s2, ntasks, nactive)

!                                     ntasks = 0
!                               end if
!                         end do
!                   end do
!             end do

!             if (ntasks .gt. 0)  then

!                   call dotasks_dm_ab_wl(method, dm_wl(:,:), &
!                         isingles, nocc, lvec, &
!                         t1, t2, s1, s2, ntasks, nactive)

!             end if
!             call dmsg("TOTAL TIME ABBIG", clock_readwall(time))

!       end subroutine generate_density_exc_exc_wlr


     ! subroutine dotasks_dm_ij_wl(theory, dm_wl, isingles, nocc, lvec, &
     !        t1, t2, s1, s2, ntasks, nactive)

     !       integer, intent(in)                   :: theory
     !       real(F64), dimension(:,:)             :: dm_wl
     !       integer, dimension(:,:), intent(in)   :: isingles
     !       integer, intent(in)                   :: nocc
     !       real(F64), dimension(:), intent(in) :: lvec
     !       real(F64), dimension(:, :, :, :), intent(in) :: t2 
     !       real(F64), dimension(:, :, :, :), intent(in) :: s2 
     !       real(F64), dimension(:, :), intent(in)                  :: t1 
     !       real(F64), dimension(:, :), intent(in)                  :: s1
     !       integer, intent(in)                   :: ntasks
     !       integer, intent(in)                   :: nactive
     !       integer :: k
     !       integer :: i, j
           
     !       ! if (theory .eq. THEORY_CC3) then
                 
     !       !       !$omp parallel private(k, i, j) default(shared)                                                                                        
     !       !       !$omp do schedule(dynamic)                                                                                                                  
     !       !       do k = 1, ntasks
     !       !             i = isingles(k, 1)
     !       !             j = isingles(k, 2)
     !       !             dm_wl(i, j)  = calc_D_oo_wl_cc3(t2, t1, s2, s1, &
     !       !                   lvec, nocc, nactive, i, j)
     !       !       end do
     !       !       !$omp end do                                                                                                                                   
     !       !       !$omp end parallel   

     !       ! else 
     !             if (theory .eq. THEORY_CCSD) then

     !             !$omp parallel private(k, i, j) default(shared)                                                                         
     !             !$omp do schedule(dynamic)                                                                                                                 
     !             do k = 1, ntasks                                                                                                                                 
     !                   i = isingles(k, 1)
     !                   j = isingles(k, 2)
     !                   dm_wl(i, j)  = calc_D_oo_wl(t2, t1, s2, s1, &                                                                                           
     !                         nocc, nactive, lvec, i, j)                                                                                                      
     !             end do
     !             !$omp end do                                                                                                                                 
     !             !$omp end parallel 
     !       end if

     ! end subroutine dotasks_dm_ij_wl

     ! subroutine dotasks_dm_ai_wl(theory, dm_wl, isingles, nocc, lvec, &
     !        t1, t2, s1, s2, ntasks, nactive)

     !       integer, intent(in)                   :: theory
     !       real(F64), dimension(:,:)             :: dm_wl
     !       integer, dimension(:,:), intent(in)   :: isingles
     !       integer, intent(in)                   :: nocc
     !       real(F64), dimension(:), intent(in) :: lvec
     !       real(F64), dimension(:, :, :, :), intent(in) :: t2 
     !       real(F64), dimension(:, :, :, :), intent(in) :: s2 
     !       real(F64), dimension(:, :), intent(in)                  :: t1 
     !       real(F64), dimension(:, :), intent(in)                  :: s1
     !       integer, intent(in)                   :: ntasks
     !       integer, intent(in)                   :: nactive
     !       integer :: k
     !       integer :: a, i

     !       ! if (theory .eq. THEORY_CC3) then
     !       !       !$omp parallel private(k, a, i) default(shared)                                                            
     !       !       !$omp do schedule(dynamic)                                                                                                                       
     !       !       do k = 1, ntasks
     !       !             a = isingles(k, 1)
     !       !             i = isingles(k, 2)
     !       !             dm_wl(a, i)  = calc_D_vo_wl_cc3(t2, s2, s1, &
     !       !                   rvec, nocc, nactive, a, i)
     !       !       end do
     !       !       !$omp end do                                                                                                                                     
     !       !       !$omp end parallel                                                                                                                               
     !       ! else 
     !             if (theory .eq. THEORY_CCSD) then

     !             !$omp parallel private(k, a, i) default(shared)                                                                                                    
     !             !$omp do schedule(dynamic)                                                                                                                         
                    
     !             do k = 1, ntasks
     !                   a = isingles(k, 1)                                                                                                                           
                    
     !                   i = isingles(k, 2)       
     !                   dm_wl(a, i)  = calc_D_vo_wl(t2, t1, s2, s1, &                                                                                              
     !                         nocc, nactive, lvec, a, i)                                                                                                          
     !             end do
     !             !$omp end do                                                                                                                                      
     !             !$omp end parallel 
     !       end if
     ! end subroutine dotasks_dm_ai_wl

     ! subroutine dotasks_dm_ia_wl(theory, dm_wl, isingles, nocc, lvec, &
     !        t1, t2, s1, s2, ntasks, nactive)

     !       integer, intent(in)                   :: theory
     !       real(F64), dimension(:,:)             :: dm_wl
     !       integer, dimension(:,:), intent(in)   :: isingles
     !       integer, intent(in)                   :: nocc
     !       real(F64), dimension(:), intent(in) :: lvec
     !       real(F64), dimension(:, :, :, :), intent(in) :: t2 
     !       real(F64), dimension(:, :, :, :), intent(in) :: s2 
     !       real(F64), dimension(:, :), intent(in)                  :: t1 
     !       real(F64), dimension(:, :), intent(in)                  :: s1
     !       integer, intent(in)                   :: ntasks
     !       integer, intent(in)                   :: nactive
     !       integer :: k
     !       integer :: i, a

     !       ! if (theory .eq. THEORY_CC3) then
     !       !       !$omp parallel private(k, i, a) default(shared)                                                                                       
     !       !       !$omp do schedule(dynamic)                                                                                                                       
     !       !       do k = 1, ntasks
     !       !             i = isingles(k, 1)
     !       !             a = isingles(k, 2)
     !       !             dm_wl(i, a)  = calc_D_ov_wl_cc3(t2, s2, &
     !       !                   rvec, nocc, nactive, i, a)
     !       !       end do
     !       !       !$omp end do                                                                                                                                     
     !       !       !$omp end parallel                                                                                                                               
     !       ! else 
     !             if (theory .eq. THEORY_CCSD) then

     !             !$omp parallel private(k, i, a) default(shared)                                                                                                    
     !             !$omp do schedule(dynamic)                                                                                                                         
                    
     !             do k = 1, ntasks
     !                   i = isingles(k, 1)
     !                   a = isingles(k, 2)
     !                   dm_wl(i, a) = zero ! calc_D_ov_wl(t2, t1, s2, s1, &                                                                                          
     !                         ! nocc, nactive, lvec, i, a)                     
     !             end do
     !             !$omp end do                                                                                                                                      
     !             !$omp end parallel                                                                                                                                 
                     
     !       end if
     ! end subroutine dotasks_dm_ia_wl

     !  subroutine dotasks_dm_ab_wl(theory, dm_wl, isingles, nocc, lvec, &
     !        t1, t2, s1, s2, ntasks, nactive)

     !        integer, intent(in)                   :: theory
     !        real(F64), dimension(:,:)             :: dm_wl
     !        integer, dimension(:,:), intent(in)   :: isingles
     !        integer, intent(in)                   :: nocc
     !        real(F64), dimension(:), intent(in) :: lvec
     !        real(F64), dimension(:, :, :, :), intent(in) :: t2 
     !        real(F64), dimension(:, :, :, :), intent(in) :: s2 
     !        real(F64), dimension(:, :), intent(in)                  :: t1 
     !        real(F64), dimension(:, :), intent(in)                  :: s1
     !        integer, intent(in)                   :: ntasks
     !        integer, intent(in)                   :: nactive
     !        integer :: k
     !        integer :: a, b

     !       ! if (theory .eq. THEORY_CC3) then
     !       !       !$omp parallel private(k, a, b) default(shared)                                                                                                  
     !       !       !$omp do schedule(dynamic)                                                                                                                       
     !       !       do k = 1, ntasks
     !       !             a = isingles(k, 1)
     !       !             b = isingles(k, 2)
     !       !             dm_wl(a, b) = calc_D_vv_wl_cc3(t2, t1, s2, s1, &
     !       !                   rvec, nocc, nactive, a, b)
     !       !       end do
     !       !       !$omp end do                                                                                                                                     
     !       !       !$omp end parallel                                                                                                                               
     !       ! else 
     !        if (theory .eq. THEORY_CCSD) then

     !             !$omp parallel private(k, a, b) default(shared)                                                                                                    
     !             !$omp do schedule(dynamic)                                                                                                                         
                    
     !             do k = 1, ntasks
     !                   a = isingles(k, 1)
     !                   b = isingles(k, 2)
     !                   dm_wl(a, b)  = calc_D_vv_wl(t2, t1, s2, s1, &                                                                                              
     !                         nocc, nactive, lvec, a, b)                                                                                                          
     !             end do
     !             !$omp end do                                                                                                                                       
     !             !$omp end parallel 
     !       end if

     ! end subroutine dotasks_dm_ab_wl

      subroutine tm_part(tm, x, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! ---------------------------------------------------------------------
            ! TM      - Output, requested transition moments
            ! N       - Number of requested transition moments
            ! NOCC0   - First occupied orbital
            ! NOCC1   - Last occupied orbital
            ! NVIRT0  - First virtual orbital
            ! NVIRT1  - Last virtual orbital
            ! DM      - Transition density
            !
            real(F64), intent(out)                              :: tm
            real(F64), dimension(:, :), intent(in)       :: x
            real(F64), dimension(:,:), intent(in)        :: dm
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer  :: nocc, nvirt
            
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            tm = tmjac(x, dm)

      contains

            function tmjac(x, dm)
                  !
                  ! Contract one-electron matrix elements in MO basis
                  ! with GAMMA and XI transition matrices:
                  !
                  ! TMJAC <- \sum_{pq} X_{pq} DM(X->W)_{qp}
                  !
                  real(F64) :: tmjac
                  real(F64), dimension(:,:), intent(in) :: x
                  real(F64), dimension(:,:), intent(in) :: dm
                  integer :: i, j, a, b
                  tmjac = ZERO
                  !
                  ! Occupied-occupied block
                  !
                  do j = nocc0, nocc1
                        do i = nocc0, nocc1
                              tmjac = tmjac + x(j, i) * dm(i, j)
                        !       if (abs(x(j, i)).gt.1.d-1.or.abs(dm(i,j)).gt.1.d-1)then
                        !             print*, 'oo', i, j, x(j, i) * dm(i, j)
                        !       end if
                        end do
                  end do
                  !
                  ! Occupied-virtual block
                  !
                  do i = nocc0, nocc1
                        do a = nvirt0, nvirt1
                              tmjac = tmjac + x(i, a) * dm(a, i)
                              ! if (abs(x(i, a)).gt.1.d-1.or.abs(dm(a,i)).gt.1.d-1)then
                              !       print*, 'ov', i, a, x(i, a) * dm(a, i)
                              ! end if
                              
                        end do
                  end do
                  !
                  ! Virtual-occupied block
                  !
                  do i = nocc0, nocc1            
                        do a = nvirt0, nvirt1
                              tmjac = tmjac + x(a, i) * dm(i, a)
                              ! if (abs(x(a, i)).gt.1.d-1.or.abs(dm(i,a)).gt.1.d-1)then
                              !       print*, 'vo', a, i, x(a, i) * dm(i, a)
                              ! end if
                              
                        end do
                  end do
                  !
                  ! Virtual-virtual block
                  !
                  do b = nvirt0, nvirt1
                        do a = nvirt0, nvirt1
                              tmjac = tmjac + x(b, a) * dm(a, b)
                              ! if (abs(x(b, a)).gt.1.d-1.or.abs(dm(a,b)).gt.1.d-1)then
                              !       print*, 'vv', a, b, x(b, a) * dm(a, b)
                              ! end if

                        end do
                  end do
            end function tmjac
      end subroutine tm_part

end module density_matrix_exc_exc
