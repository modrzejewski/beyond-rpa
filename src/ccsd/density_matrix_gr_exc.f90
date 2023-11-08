module density_matrix_gr_exc

      use cc3_intermediates
      use s_gen
      use basis
      use symmetry
      use gparam
      use eom_vectors
      use density_gr_exc_functions
      use density_gr_exc_functions_int

      implicit none

contains

      subroutine generate_density_gr_exc(lvec, rvec, t2, t1, s2, s1,  &
            nocc, nactive, method, dm_gamma, dm_xi, irrep_idx, irrep0, irrep1, order)

            real(F64), dimension(:), intent(in)     :: lvec
            real(F64), dimension(:), intent(in)     :: rvec
            integer, intent(in)                       :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
            integer, intent(in)                                 :: method
            double precision, dimension(:,:), intent(inout)  :: dm_gamma, dm_xi
            integer, intent(in) :: irrep_idx
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
            integer :: ijbig, aibig, iabig, abbig
            integer :: m0i, m1i, m0j, m1j
            integer :: m0a, m1a, m0b, m1b
            integer :: ntasks
            integer :: pa, pb, pi, pj

            integer :: nthreads
            integer :: max_ntasks

            logical :: int

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            allocate(isingles(max_ntasks, 2))


            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ij, POINT_GROUP, idims_ij, .true., 1, 1)
            allocate(isingles_ij(2, idims_ij))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ij, POINT_GROUP, idims_ij, .false., 1, 1)

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ai, POINT_GROUP, idims_ai, .true., 2, 1)
            allocate(isingles_ai(2, idims_ai))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ai, POINT_GROUP, idims_ai, .false., 2, 1)

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ia, POINT_GROUP, idims_ia, .true., 1, 2)
            allocate(isingles_ia(2, idims_ia))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ia, POINT_GROUP, idims_ia, .false., 1, 2)

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ab, POINT_GROUP, idims_ab, .true., 2, 2)
            allocate(isingles_ab(2, idims_ab))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ab, POINT_GROUP, idims_ab, .false., 2, 2) 


            if (DENSITY_INT .eqv. .True.) then
                  call msg("GENERATING DENSITY INTERMEDIATES...")
                  call clock_start(time)
                  if (method == THEORY_CCSD) then                  
                        call xi_intermediates_ccsd_init(nocc, nactive)
                        call gamma_intermediates_ccsd_init(nocc, nactive)
                        call xi_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, lvec) 
                        call gamma_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvec) 
                  else if (method == THEORY_CC3) then
                        print*, 'xi'
                        call xi_intermediates_cc3_init(nocc, nactive)
                        print*, 'gamma'
                        call gamma_intermediates_cc3_init(nocc, nactive)
                        print*, 'xi-nie in'
                        call xi_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, lvec) 
                        print*, 'gam nie in'
                        call gamma_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, rvec) 
                        print*, 'po gam nie in'
                  end if
                  call dmsg("DONE. TIME:", clock_readwall(time))
            end if
            print*, 'a'
            call msg("GENERATING DENSITY MATRIX...")
            call clock_start(time)
            ntasks = 0
            call msg('LOOP OVER occ occ...')
            do ijbig = 1, idims_ij
                  call loop_boundaries_sp_qq(isingles_ij(1:2, ijbig), irrep0, irrep1, &
                        m0i, m1i, m0j, m1j, 1, 1)

                  do pi = m0i, m1i
                        do pj = m0j, m1j

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pj

                              if (ntasks == max_ntasks)  then

                                    if (DENSITY_INT .eqv. .False.) then
                                          call dotasks_dm_ij(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    else
                                          call dotasks_dm_ij_int(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    end if
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  if (DENSITY_INT .eqv. .False.) then
                        call dotasks_dm_ij(method, dm_gamma(:,:), dm_xi(:,:), &
                              isingles, nocc, rvec, lvec, &
                              t1, t2, s1, s2, ntasks, nactive)                 
                  else
                        call dotasks_dm_ij_int(method, dm_gamma(:,:), dm_xi(:,:), &
                              isingles, nocc, rvec, lvec, &
                              t1, t2, s1, s2, ntasks, nactive)                        
                  end if
            end if

            call dmsg("DONE. TIME:", clock_readwall(time))

            call msg('LOOP OVER virt occ...')
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

                                    if (DENSITY_INT .eqv. .False.) then
                                          call dotasks_dm_ai(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    else
                                          call dotasks_dm_ai_int(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    end if
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  if (DENSITY_INT .eqv. .False.) then
                        call dotasks_dm_ai(method, dm_gamma(:,:), dm_xi(:,:), &
                              isingles, nocc, rvec, lvec, &
                              t1, t2, s1, s2, ntasks, nactive)
                  else
                        call dotasks_dm_ai_int(method, dm_gamma(:,:), dm_xi(:,:), &
                              isingles, nocc, rvec, lvec, &
                              t1, t2, s1, s2, ntasks, nactive)
                  end if
            end if
            call dmsg("DONE. TIME:", clock_readwall(time))
            
            call msg('LOOP OVER occ virt...')
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

                                    if (DENSITY_INT .eqv. .False.) then
                                          call dotasks_dm_ia(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    else
                                          call dotasks_dm_ia_int(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    end if
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then
                  if (DENSITY_INT .eqv. .False.) then
                         call dotasks_dm_ia(method, dm_gamma(:,:), dm_xi(:,:), &
                               isingles, nocc, rvec, lvec, &
                               t1, t2, s1, s2, ntasks, nactive)
                   else
                         call dotasks_dm_ia_int(method, dm_gamma(:,:), dm_xi(:,:), &
                               isingles, nocc, rvec, lvec, &
                               t1, t2, s1, s2, ntasks, nactive)
                   end if
            end if
            call dmsg("DONE. TIME:", clock_readwall(time))                  

            call msg('LOOP OVER virt virt...')
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
                                    
                                    if (DENSITY_INT .eqv. .False.) then
                                          call dotasks_dm_ab(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    else
                                          call dotasks_dm_ab_int(method, dm_gamma(:,:), dm_xi(:,:), &
                                                isingles, nocc, rvec, lvec, &
                                                t1, t2, s1, s2, ntasks, nactive)
                                    end if
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  if (DENSITY_INT .eqv. .False.) then
                        call dotasks_dm_ab(method, dm_gamma(:,:), dm_xi(:,:), &
                              isingles, nocc, rvec, lvec, &
                              t1, t2, s1, s2, ntasks, nactive)
                  else
                        call dotasks_dm_ab_int(method, dm_gamma(:,:), dm_xi(:,:), &
                              isingles, nocc, rvec, lvec, &
                              t1, t2, s1, s2, ntasks, nactive)
                  end if
            end if
            call dmsg("DONE. TIME:", clock_readwall(time))

            if (DENSITY_INT .eqv. .True.) then
                  if (method == THEORY_CCSD) then
                        call xi_intermediates_ccsd_free()
                        call gamma_intermediates_ccsd_free()
                  else if (method == THEORY_CC3) then
                        call xi_intermediates_cc3_free()
                        call gamma_intermediates_cc3_free()
                  end if
            end if

      end subroutine generate_density_gr_exc


     subroutine dotasks_dm_ij(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, j
           
           if (theory .eq. THEORY_CC3) then
                 
                 !$omp parallel private(k, i, j) default(shared)                                                                                        
                 !$omp do schedule(dynamic)                                                                                                                  
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)

                       dm_gamma(i, j)  = calc_D_oo_gamma_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, i, j)
                       dm_xi(i, j) = calc_D_oo_xi_cc3(t2, t1, &
                             lvec, nocc, nactive, i, j)
                 end do
                 !$omp end do                                                                                                                                   
                 !$omp end parallel   

           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, j) default(shared)                                                                         
                 !$omp do schedule(dynamic)                                                                                                                 
                 do k = 1, ntasks                                                                                                                                 
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_gamma(i, j)  = calc_D_oo_gamma(t1, s2, s1, &                                                                                           
                             rvec, nocc, nactive, i, j)                                                                                                      
                       dm_xi(i, j) = calc_D_oo_xi(t2, t1, &                                                                                                      
                             lvec, nocc, nactive, i, j)                                                                                                 
                 end do
                 !$omp end do                                                                                                                                 
                 !$omp end parallel 
           end if

     end subroutine dotasks_dm_ij


     subroutine dotasks_dm_ai(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: a, i

           if (theory .eq. THEORY_CC3) then

                 !$omp parallel private(k, a, i) default(shared)                                                                   
                 !$omp do schedule(dynamic)                                                                                           
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_gamma(a, i)  = calc_D_vo_gamma_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, a, i)
                       dm_xi(a, i) = calc_D_vo_xi_cc3(t2, t1, &
                                    lvec, nocc, nactive, a, i)                      
                 end do
                 !$omp end do                                                                                                                            
                 !$omp end parallel                                                                                                                     

           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, i) default(shared)                                                                                         
                 !$omp do schedule(dynamic)                                                                                                                   
                 do k = 1, ntasks
                       a = isingles(k, 1)                                                                                                           
                       i = isingles(k, 2)       
                       dm_gamma(a, i)  = calc_D_vo_gamma(t2, s2, s1, &                                                                                 
                             rvec, nocc, nactive, a, i)                                                                                                 
                       dm_xi(a, i) = calc_D_vo_xi(t2, &                                                                                               
                             lvec, nocc, nactive, a, i)   
                 end do
                 !$omp end do                                                                                                                   
                 !$omp end parallel 
           end if
     end subroutine dotasks_dm_ai




     subroutine dotasks_dm_ia(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, a

           if (theory .eq. THEORY_CC3) then
                 !$omp parallel private(k, i, a) default(shared)                                                                                       
                 !$omp do schedule(dynamic)                                                                                                        
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_gamma(i, a)  = calc_D_ov_gamma_cc3(t2, t1, s2, &
                             rvec, nocc, nactive, i, a)
                       dm_xi(i, a) = calc_D_ov_xi_cc3()
                 end do
                 !$omp end do                                                                                                                         
                 !$omp end parallel                                                                                                                
                                                                                                                                               
           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, a) default(shared)                                                                             
                 !$omp do schedule(dynamic)                                                                                                             
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_gamma(i, a) = calc_D_ov_gamma(t2, s2, &                                                                                 
                             rvec, nocc, nactive, i, a)                                                                                     
                       dm_xi(i, a) = calc_D_ov_xi()  
                 end do
                 !$omp end do                                                                                                                          
                 !$omp end parallel                                                                                                            
           end if
     end subroutine dotasks_dm_ia




      subroutine dotasks_dm_ab(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

            integer, intent(in)                   :: theory
            real(F64), dimension(:,:)             :: dm_gamma
            real(F64), dimension(:,:)             :: dm_xi
            integer, dimension(:,:), intent(in)   :: isingles
            integer, intent(in)                   :: nocc
            real(F64), dimension(:), intent(in) :: rvec
            real(F64), dimension(:), intent(in) :: lvec
            double precision, dimension(:, :, :, :), intent(in) :: t2 
            double precision, dimension(:, :, :, :), intent(in) :: s2 
            double precision, dimension(:, :), intent(in)                  :: t1 
            double precision, dimension(:, :), intent(in)                  :: s1
            integer, intent(in)                   :: ntasks
            integer, intent(in)                   :: nactive
            integer :: k
            integer :: a, b

           if (theory .eq. THEORY_CC3) then
                 !$omp parallel private(k, a, b) default(shared)                                                                                      
                 !$omp do schedule(dynamic)                                                                                                             
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dm_gamma(a, b) = calc_D_vv_gamma_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, a, b)
                       dm_xi(a, b) = calc_D_vv_xi_cc3(t2, t1, &
                             lvec, nocc, nactive, a, b)
                 end do
                 !$omp end do                                                                                                                          
                 !$omp end parallel                                                                                                                    

           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, b) default(shared)                                                                                      
                 !$omp do schedule(dynamic)                                                                                                           
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dm_gamma(a, b)  = calc_D_vv_gamma(t1, s2, s1, &                                                                                 
                             rvec, nocc, nactive, a, b)                                                                                             
                       dm_xi(a, b) = calc_D_vv_xi(t2, t1, &                                                                                     
                             lvec, nocc, nactive, a, b)
                 end do
                 !$omp end do                                                                                                                                  
                 !$omp end parallel 
           end if

     end subroutine dotasks_dm_ab



     subroutine dotasks_dm_ij_int(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, j
           
           if (theory .eq. THEORY_CC3) then
                 
                 !$omp parallel private(k, i, j) default(shared)                                                                                        
                 !$omp do schedule(dynamic)                                                                                                                  
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)

                       dm_gamma(i, j)  = calc_D_oo_gamma_with_int_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, i, j)
                       dm_xi(i, j) = calc_D_oo_xi_with_int_cc3(t2, t1, s2, s1, &
                             lvec, nocc, nactive, i, j)
                 end do
                 !$omp end do                                                                                                                                   
                 !$omp end parallel   

           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, j) default(shared)                                                                         
                 !$omp do schedule(dynamic)                                                                                                                 
                 do k = 1, ntasks                                                                                                                                 
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dm_gamma(i, j)  = calc_D_oo_gamma_with_int(t2, t1, s2, s1, &                                                                        
                             rvec, nocc, nactive, i, j)                                                                                                      
                       dm_xi(i, j) = calc_D_oo_xi_with_int(t2, t1, s2, s1, &                                                                                       
                             lvec, nocc, nactive, i, j)                                                                                                 
                 end do
                 !$omp end do                                                                                                                                 
                 !$omp end parallel 
           end if

     end subroutine dotasks_dm_ij_int


     subroutine dotasks_dm_ai_int(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: a, i

           if (theory .eq. THEORY_CC3) then

                 !$omp parallel private(k, a, i) default(shared)                                                                   
                 !$omp do schedule(dynamic)                                                                                           
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dm_gamma(a, i)  = calc_D_vo_gamma_with_int_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, a, i)
                       dm_xi(a, i) = calc_D_vo_xi_with_int_cc3(t2, t1, s2, s1, &
                                    lvec, nocc, nactive, a, i)                      
                 end do
                 !$omp end do                                                                                                                            
                 !$omp end parallel                                                                                                                     

           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, i) default(shared)                                                                                         
                 !$omp do schedule(dynamic)                                                                                                                   
                 do k = 1, ntasks
                       a = isingles(k, 1)                                                                                                           
                       i = isingles(k, 2)       
                       dm_gamma(a, i)  = calc_D_vo_gamma_with_int(t2, t1, s2, s1, &                                                                                 
                             rvec, nocc, nactive, a, i)                                                                                                 
                       dm_xi(a, i) = calc_D_vo_xi_with_int(t2, t1, s2, s1, &                                                                                               
                             lvec, nocc, nactive, a, i)   
                 end do
                 !$omp end do                                                                                                                   
                 !$omp end parallel 
           end if
     end subroutine dotasks_dm_ai_int


     subroutine dotasks_dm_ia_int(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
            t1, t2, s1, s2, ntasks, nactive)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, a

           if (theory .eq. THEORY_CC3) then
                 !$omp parallel private(k, i, a) default(shared)                                                                                       
                 !$omp do schedule(dynamic)                                                                                                        
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_gamma(i, a)  = calc_D_ov_gamma_with_int_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, i, a)
                       dm_xi(i, a) = zero
                 end do
                 !$omp end do                                                                                                                         
                 !$omp end parallel                                                                                                                
                                                                                                                                               
           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, a) default(shared)                                                                             
                 !$omp do schedule(dynamic)                                                                                                             
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dm_gamma(i, a) = calc_D_ov_gamma_with_int(t2, t1, s2, s1, &                                                                                 
                             rvec, nocc, nactive, i, a)                                                                                     
                       dm_xi(i, a) = zero
                 end do
                 !$omp end do                                                                                                                          
                 !$omp end parallel                                                                                                            
           end if
     end subroutine dotasks_dm_ia_int

     subroutine dotasks_dm_ab_int(theory, dm_gamma, dm_xi, isingles, nocc, rvec, lvec, &
           t1, t2, s1, s2, ntasks, nactive)
           
           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dm_gamma
           real(F64), dimension(:,:)             :: dm_xi
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: nocc
           real(F64), dimension(:), intent(in) :: rvec
           real(F64), dimension(:), intent(in) :: lvec
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: a, b
           
           if (theory .eq. THEORY_CC3) then
                 !$omp parallel private(k, a, b) default(shared)                                                                                      
                 !$omp do schedule(dynamic)                                                                                                             
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dm_gamma(a, b) = calc_D_vv_gamma_with_int_cc3(t2, t1, s2, s1, &
                             rvec, nocc, nactive, a, b)
                       dm_xi(a, b) = calc_D_vv_xi_with_int_cc3(t2, t1, s2, s1, &
                             lvec, nocc, nactive, a, b)
                 end do
                 !$omp end do                                                                                                                          
                 !$omp end parallel                                                                                                                    

           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, b) default(shared)                                                                                      
                 !$omp do schedule(dynamic)                                                                                                           
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dm_gamma(a, b)  = calc_D_vv_gamma_with_int(t2, t1, s2, s1, &                                                                                 
                             rvec, nocc, nactive, a, b)                                                                                             
                       dm_xi(a, b) = calc_D_vv_xi_with_int(t2, t1, s2, s1, &                                                                                     
                             lvec, nocc, nactive, a, b)
                 end do
                 !$omp end do                                                                                                                                  
                 !$omp end parallel 
           end if

     end subroutine dotasks_dm_ab_int


      subroutine tmccsd_density(tm, nocc0, nocc1, nvirt0, nvirt1, xga, yxi, &
            nocc, nvirt, dm_gamma, dm_xi)
            ! ---------------------------------------------------------------------
            ! TM      - Output, requested transition moments
            ! N       - Number of requested transition moments
            ! NOCC0   - First occupied orbital
            ! NOCC1   - Last occupied orbital
            ! NVIRT0  - First virtual orbital
            ! NVIRT1  - Last virtual orbital
            ! DM_GAMMA - Transition density (GAMMA)
            ! DM_XI   - Transition density (XI)
            !
            real(F64), intent(out)                               :: tm
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: nocc, nvirt
            double precision, dimension(:, :), intent(in)       :: xga
            double precision, dimension(:, :), intent(in)       :: yxi
            double precision, dimension(:,:), intent(in)        :: dm_gamma, dm_xi

            double precision :: tj, tg
            
            tj = tmjac(xga, dm_gamma)
            tg = tmjac(yxi, dm_xi)
            
            ! print*, '-----------------------------------------------------------------------------------------------------'
            ! print*, 'tj, tg', tj, tg

            print*, 'tj =', tj
            print*, 'tg =', tg
            tm =  tj * tg

      contains

            function tmjac(x, dm)
                  !
                  ! Contract one-electron matrix elements in MO basis
                  ! with GAMMA and XI transition matrices:
                  !
                  ! TMJAC <- \sum_{pq} X_{pq} DM(X->W)_{qp}
                  !
                  double precision :: tmjac
                  double precision, dimension(:,:), intent(in) :: x
                  double precision, dimension(:,:), intent(in) :: dm
                  integer :: i, j, a, b, k
                  integer :: ij, ia, ai, ab

                  tmjac = ZERO
                  !
                  ! Occupied-occupied block
                  !
                  do j = nocc0, nocc1
                        do i = nocc0, nocc1
                              ij = (j - nocc0) * nocc + (i - nocc0) + 1
                              tmjac = tmjac + x(j, i) * dm(i, j)
                        end do
                  end do
                  print*, 'tmjac oo', tmjac
                  !
                  ! Occupied-virtual block
                  !
                  k = nocc*nocc
                  do i = nocc0, nocc1
                        do a = nvirt0, nvirt1
                              ai = (i - nocc0) * nvirt + (a - nvirt0) + 1 + k
                              tmjac = tmjac + x(i, a) * dm(a, i)
                        end do
                  end do
                  print*, 'tmjac ov', tmjac
                  !
                  ! Virtual-occupied block
                  !
                  k = k + nvirt * nocc 
                  do i = nocc0, nocc1            
                        do a = nvirt0, nvirt1
                              ia = (a - nvirt0) * nocc + (i - nocc0) + 1 + k
                              tmjac = tmjac + x(a, i) * dm(i, a)
                        end do
                  end do
                  print*, 'tmjac vo', tmjac
                  !
                  ! Virtual-virtual block
                  !
                  k = k + nvirt * nocc 
                  do b = nvirt0, nvirt1
                        do a = nvirt0, nvirt1
                              ab = (b - nvirt0) * nvirt + (a - nvirt0) + 1 + k
                              tmjac = tmjac + x(b, a) * dm(a, b)
                        end do
                  end do
                  print*, 'tmjac vv', tmjac

            end function tmjac

      end subroutine tmccsd_density

end module density_matrix_gr_exc


!------------------------------------------------------------------------------------------
      ! subroutine dotasks_dm_ij(dm_gamma, dm_xi, n0i, n1i, n0j, n1j, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0i, n1i
      !       integer, dimension(:), intent(in)     :: n0j, n1j
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: i, j
      !       integer :: ijstart, ijstop, istep, jstep, ijdim, ij, ijtrue

      !       !$omp parallel private(k, ijstart, ijstop, istep, jstep, ijdim, ijtrue, i, j) default(shared)                              
      !       !$omp do schedule(dynamic)
      !       do k = 1, ntasks
      !             ijstart = (n0j(k) - 1) * nocc + n0i(k)
      !             ijstop  = (n1j(k) - 1) * nocc + n1i(k)
      !             istep = n1i(k) - n0i(k) + 1
      !             jstep = n1j(k) - n0j(k) + 1
      !             ijdim = istep * jstep
      !             do ij = 1, ijdim
      !                   ijtrue = ((ij-1)/jstep) * (nocc-jstep) + ijstart + ij -1
      !                   j = (ijtrue - 1) / nocc + 1 
      !                   i = ijtrue - nocc * (j -1) 
      !                   dm_gamma(i, j)  = calc_D_oo_gamma(t1, s2, s1, &
      !                         rvec, nocc, nactive, i, j)
      !                   dm_xi(i, j) = calc_D_oo_xi(t2, t1, &
      !                         lvec, nocc, nactive, i, j)
      !             end do
      !       end do
      !       !$omp end do                                                                                                                                                                
      !       !$omp end parallel  

      !       ! !$omp parallel private(k) default(shared)
      !       ! !$omp do 
      !       ! do k = 1, ntasks
      !       !       do i = n0i(k), n1i(k)
      !       !             do j = n0j(k), n1j(k)
      !       !                   dm_gamma(i, j)  = calc_D_oo_gamma(t1, s2, s1, &
      !       !                         rvec, nocc, nactive, i, j, w) 
      !       !                   dm_xi(i, j) = calc_D_oo_xi(t2, t1, &
      !       !                         lvec, nocc, nactive, i, j, w)
      !       !             end do
      !       !       end do
      !       ! end do
      !       ! !$omp end do
      !       ! !$omp end parallel
            
      ! end subroutine dotasks_dm_ij


    

      ! subroutine transition_density_statistics(total_nelements, isingles, irrep0, irrep1, idims)
      !       integer, intent(in) :: total_nelements
      !       integer, dimension(:,:), intent(in) :: isingles
      !       integer, dimension(:,:), intent(in) :: irrep0
      !       integer, dimension(:,:), intent(in) :: irrep1
      !       integer, intent(in) :: idims
      !       integer :: dim1, dim2, dim3, dim4
      !       integer :: nelements
      !       integer :: k
      !       integer :: m0i, m1i, m0j, m1j, m0a, m1a, m0b, m1b
      !       real(F64) :: percent_of_use

      !       nelements = 0
      !       do k = 1, idims
      !             call loop_boundaries_sp_qq(isingles(1:2, k), irrep0, irrep1, &
      !                   m0i, m1i, m0j, m1j, 1, 1)
      !             dim1 = (m1i-m0i+1) * (m1j-m0j+1)

      !             call loop_boundaries_sp_qq(isingles(1:2, k), irrep0, irrep1, &
      !                   m0a, m1a, m0i, m1i, 2, 1)
      !             dim2 = (m1i-m0i+1) * (m1a-m0a+1)

      !             call loop_boundaries_sp_qq(isingles(1:2, k), irrep0, irrep1, &
      !                   m0i, m1i, m0a, m1a, 1, 2)
      !             dim3 = (m1i-m0i+1) * (m1a-m0a+1)
      !             call loop_boundaries_sp_qq(isingles(1:2, k), irrep0, irrep1, &
      !                   m0a, m1a, m0b, m1b, 2, 2)
      !             dim4 = (m1a-m0a+1) * (m1b-m0b+1)

      !             nelements = nelements + dim1 + dim2 + dim3 + dim4
      !       end do

      !       percent_of_use = dble(nelements) / dble(total_nelements) * 100

      !       call dmsg("PERCENT OF ELEMENTS COMPUTED IN TDM", percent_of_use)


      ! end subroutine transition_density_statistics

      ! subroutine dotasks_dm_ai(dm_gamma, dm_xi, n0a, n1a, n0i, n1i, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0a, n1a
      !       integer, dimension(:), intent(in)     :: n0i, n1i
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: i, a
      !       integer :: ijstart, ijstop, istep, jstep, ijdim, ij, ijtrue

      !       !$omp parallel private(k, ijstart, ijstop, istep, jstep, ijdim, ijtrue, a, i) default(shared)
      !       !$omp do schedule(dynamic)
      !       do k = 1, ntasks

      !             ijstart = (n0i(k) - 1) * nvirt + n0a(k) - nocc
      !             ijstop = (n1i(k) - 1) * nvirt + n1a(k) - nocc 
      !             istep = n1a(k) - n0a(k) + 1
      !             jstep = n1i(k) - n0i(k) + 1
      !             ijdim = istep * jstep
                  
      !             do ij = 1, ijdim
      !                   ijtrue = ((ij-1)/istep) * (nvirt-istep) + ijstart + ij - 1
      !                   i = (ijtrue - 1) / nvirt + 1 
      !                   a = ijtrue - (i - 1) * nvirt + nocc
      !                   dm_gamma(a, i)  = calc_D_vo_gamma(t2, s2, s1, &
      !                         rvec, nocc, nactive, a, i)
      !                   dm_xi(a, i) = calc_D_vo_xi(t2, &
      !                         lvec, nocc, nactive, a, i)
      !             end do
      !       end do
      !       !$omp end do                                                                                                                                                               
      !       !$omp end parallel

      !       ! !$omp parallel private(k) default(shared)                                                                                                                 
      !       ! !$omp do                                                                                                                                                          
      !       ! do k = 1, ntasks
                  
      !       !       do a = n0a(k), n1a(k)
      !       !             do i = n0i(k), n1i(k)
      !       !                   dm_gamma(a, i)  = calc_D_vo_gamma(t2, s2, s1, &
      !       !                         rvec, nocc, nactive, a, i, w) 
      !       !                   dm_xi(a, i) = calc_D_vo_xi(t2, &
      !       !                         lvec, nocc, nactive, a, i, w)
      !       !             end do
      !       !       end do                              
      !       ! end do
      !       ! !$omp end do
      !       ! !$omp end parallel

      ! end subroutine dotasks_dm_ai


      ! subroutine dotasks_dm_ia(dm_gamma, dm_xi, n0i, n1i, n0a, n1a, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0i, n1i
      !       integer, dimension(:), intent(in)     :: n0a, n1a
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: i, a
      !       integer :: ijstart, ijstop, istep, jstep, ijdim, ij, ijtrue

      !       !$omp parallel private(k, ijstart, ijstop, istep, jstep, ijdim, ijtrue, a, i) default(shared)                  
      !       !$omp do  schedule(dynamic)                                                                                                                 
      !       do k = 1, ntasks
      !             ijstart = (n0a(k) - nocc - 1) * nocc + n0i(k)
      !             ijstop = (n1a(k) - nocc - 1) * nocc + n1i(k)
      !             istep = n1a(k) - n0a(k) + 1
      !             jstep = n1i(k) - n0i(k) + 1
      !             ijdim = istep * jstep

      !             do ij = 1, ijdim           
      !                   ijtrue = ((ij-1)/jstep) * (nocc-jstep) + ijstart + ij - 1
      !                   a = (ijtrue - 1) / nocc + nocc + 1
      !                   i = ijtrue - (a - nocc - 1) * nocc

      !                   dm_gamma(i, a) = calc_D_ov_gamma(t2, s2, &
      !                         rvec, nocc, nactive, i, a)
      !                   dm_xi(i, a) = calc_D_ov_xi()
      !             end do
      !       end do
      !       !$omp end do                                                                                                                                                              
      !       !$omp end parallel 

      !       ! !$omp parallel private(k) default(shared)
      !       ! !$omp do 
      !       ! do k = 1, ntasks
      !       !       do a = n0a(k), n1a(k)
      !       !             do i = n0i(k), n1i(k)
      !       !                   dm_gamma(i, a) = calc_D_ov_gamma(t2, s2, &
      !       !                         rvec, nocc, nactive, i, a, w) 
      !       !                   dm_xi(i, a) = calc_D_ov_xi()
      !       !             end do
      !       !       end do                              
      !       ! end do
      !       ! !$omp end do
      !       ! !$omp end parallel

      ! end subroutine dotasks_dm_ia


      ! subroutine dotasks_dm_ab(dm_gamma, dm_xi, n0a, n1a, n0b, n1b, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0a, n1a
      !       integer, dimension(:), intent(in)     :: n0b, n1b
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: a, b
      !       integer :: ijstart, ijstop, istep, jstep, ijdim, ij

      !       !$omp parallel private(k, ijstart, ijstop, istep, jstep, ijdim, a, b) default(shared)                                                      
      !       !$omp do   schedule(dynamic)                                                                                                                  
      !       do k = 1, ntasks           
      !             ijstart = (n0b(k) - nocc - 1) * nvirt + n0a(k) - nocc
      !             ijstop  = (n1b(k) - nocc - 1) * nvirt + n1a(k) - nocc
      !             istep = n1a(k) - n0a(k) + 1
      !             jstep = n1b(k) - n0b(k) + 1
      !             ijdim = istep * jstep

      !             do ij = 1, ijdim      
      !                   b = (ij - 1) / istep + n0b(k)
      !                   a = ij -1 + n0a(k) - ((ij - 1) / istep)*(nvirt - istep)
      !                   dm_gamma(a, b)  = calc_D_vv_gamma(t1, s2, s1, &
      !                         rvec, nocc, nactive, a, b)
      !                   dm_xi(a, b) = calc_D_vv_xi(t2, t1, &
      !                         lvec, nocc, nactive, a, b)
                        
      !             end do
      !       end do
      !       !$omp end do                                                                                                                                   
      !       !$omp end parallel   

      !       ! !$omp parallel private(k) default(shared)
      !       ! !$omp do 
      !       ! do k = 1, ntasks
      !       !       do a = n0a(k), n1a(k)
      !       !             do b = n0b(k), n1b(k)
      !       !                   dm_gamma(a, b)  = calc_D_vv_gamma(t1, s2, s1, &
      !       !                         rvec, nocc, nactive, a, b, w) 
      !       !                   dm_xi(a, b) = calc_D_vv_xi(t2, t1, &
      !       !                         lvec, nocc, nactive, a, b, w)
      !       !             end do
      !       !       end do                              
      !       ! end do
      !       ! !$omp end do
      !       ! !$omp end parallel
      ! end subroutine dotasks_dm_ab




! generate density:
            ! if(method .eq. THEORY_CCSD)then
            !       do w = 1, n
            !             !$omp parallel private(i, j, a, b, ij, ai, ia, ab) &                                                      
            !             !$omp default(shared)                                                                                        
            !             !$omp do schedule(guided)
            !             do ij = 1, nocc*nocc
            !                   j = (ij - 1) / nocc + 1 
            !                   i = ij - nocc * (j -1) 
            !                   dm_gamma(i, j, w)  = calc_D_oo_gamma(t1, s2, s1, &
            !                         rvec, nocc, nactive, i, j, w) 
            !                   dm_xi(i, j, w) = calc_D_oo_xi(t2, t1, &
            !                         lvec, nocc, nactive, i, j, w) 
            !             end do
            !             !$omp end do nowait

            !             !$omp do schedule(guided)
            !             do ai = 1, npair 
            !                   a = ai - nvirt * ((ai - 1) / nvirt ) + nocc
            !                   i = (ai - 1) / nvirt + 1 
            !                   dm_gamma(a, i, w)  = calc_D_vo_gamma(t2, s2, s1, &
            !                         rvec, nocc, nactive, a, i, w) 
            !                   dm_xi(a, i, w) = calc_D_vo_xi(t2, &
            !                         lvec, nocc, nactive, a, i, w)
            !             end do
            !             !$omp end do nowait 

            !             !$omp do schedule(guided)
            !             do ia = 1, npair
            !                   a = (ia - 1) / nocc + nocc+1
            !                   i = ia - nocc * (a - (nocc + 1))
            !                   dm_gamma(i, a, w) = calc_D_ov_gamma(t2, s2, &
            !                         rvec, nocc, nactive, i, a, w) 
            !                   dm_xi(i, a, w) = calc_D_ov_xi()
            !             end do
            !             !$omp end do nowait   

            !             !$omp do schedule(guided)
            !             do ab = 1, nvirt*nvirt
            !                   b = (ab - 1) / nvirt + nocc + 1 
            !                   a = ab - nvirt * (b - (nocc + 1)) + nocc
            !                   dm_gamma(a, b, w)  = calc_D_vv_gamma(t1, s2, s1, &
            !                         rvec, nocc, nactive, a, b, w) 
            !                   dm_xi(a, b, w) = calc_D_vv_xi(t2, t1, &
            !                         lvec, nocc, nactive, a, b, w)
            !             end do
            !             !$omp end do nowait   
            !             !$omp end parallel
            !       end do

            ! else if(method .eq. THEORY_CC3)then
            !       do w = 1, n
            !             !$omp parallel private(i, j, a, b, ij, ai, ia, ab) &
            !             !$omp default(shared)
            !             !$omp do schedule(guided)    
            !             do ij = 1, nocc*nocc
            !                   j = (ij - 1) / nocc + 1 
            !                   i = ij - nocc * (j - 1) 
            !                   dm_gamma(i, j, w)  = calc_D_oo_gamma_cc3(t2, t1, s2, s1, &
            !                         rvec, nocc, nactive, i, j, w) 
            !                   dm_xi(i, j, w) = calc_D_oo_xi_cc3(t2, t1, &
            !                         lvec, nocc, nactive, i, j, w) 
            !             end do
            !             !$omp end do nowait

            !             !$omp do schedule(guided)    
            !             do ai = 1, npair
            !                   a = ai - nvirt * ((ai - 1) / nvirt ) + nocc
            !                   i = (ai - 1) / nvirt +1 
            !                   dm_gamma(a, i, w)  = calc_D_vo_gamma_cc3(t2, s2, s1, &
            !                         rvec, nocc, nactive, a, i, w) 
            !                   dm_xi(a, i, w) = calc_D_vo_xi_cc3(t2, &
            !                         lvec, nocc, nactive, a, i, w)
            !             end do
            !             !$omp end do nowait

            !             !$omp do schedule(guided)    
            !             do ia = 1, npair
            !                   a = (ia - 1) / nocc + nocc + 1
            !                   i = ia - nocc * (a - (nocc + 1))
            !                   dm_gamma(i, a, w)  = calc_D_ov_gamma_cc3(t2, s2, &
            !                         rvec, nocc, nactive, i, a, w) 
            !                   dm_xi(i, a, w) = calc_D_ov_xi_cc3()
            !             end do
            !             !$omp end do nowait

            !             !$omp do schedule(guided)    
            !             do ab = 1, nvirt*nvirt
            !                   b = (ab - 1) / nvirt + nocc + 1 
            !                   a = ab - nvirt * (b - (nocc + 1)) + nocc
            !                   dm_gamma(a, b, w) = calc_D_vv_gamma_cc3(t2, t1, s2, s1, &
            !                         rvec, nocc, nactive, a, b, w) 
            !                   dm_xi(a, b, w) = calc_D_vv_xi_cc3(t2, t1, &
            !                         lvec, nocc, nactive, a, b, w)
            !             end do
            !             !$omp end do nowait
            !             !$omp end parallel
            !       end do
            ! end if

            ! print*, 'dmgamma'
            ! do i = 1, CC_NORB
            !       do j = 1, CC_NORB
            !             if(abs(dm_xi(i, j, 1)).gt.1.d-6)then
            !                   print*, i, j, dm_(i, j, 1)
            !             end if
            !       end do
            ! end do

            ! print*, nocc*nocc, npair, nvirt*nvirt, CC_NORB, CC_NORB
            ! print*, 'dm_gamma'
            ! print*, ''
            ! do i = 1, CC_NORB
            !       write(*,'(18F10.5)') dm_gamma(i, :, 1)
            ! end do
            ! print*, ''
            ! print*, 'dm_xi'
            ! print*, ''
            ! do i = 1, CC_NORB
            !       write(*,'(18F10.5)') dm_xi(i, :, 1)
            ! end do
            !            stop


      ! subroutine dotasks_dm_ij_cc3(dm_gamma, dm_xi, n0i, n1i, n0j, n1j, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0i, n1i
      !       integer, dimension(:), intent(in)     :: n0j, n1j
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: i, j

      !       !$omp parallel private(k) default(shared)
      !       !$omp do schedule(dynamic)
      !       do k = 1, ntasks
      !             do i = n0i(k), n1i(k)
      !                   do j = n0j(k), n1j(k)

      !                         dm_gamma(i, j)  = calc_D_oo_gamma_cc3(t2, t1, s2, s1, &
      !                               rvec, nocc, nactive, i, j) 
      !                         dm_xi(i, j) = calc_D_oo_xi_cc3(t2, t1, &
      !                               lvec, nocc, nactive, i, j) 
      !                   end do
      !             end do
      !       end do
      !       !$omp end do
      !       !$omp end parallel
            
      ! end subroutine dotasks_dm_ij_cc3

      ! subroutine dotasks_dm_ia_cc3(dm_gamma, dm_xi, n0i, n1i, n0a, n1a, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0i, n1i
      !       integer, dimension(:), intent(in)     :: n0a, n1a
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: i, a

      !       !$omp parallel private(k) default(shared)
      !       !$omp do schedule(dynamic)
      !       do k = 1, ntasks
      !             do a = n0a(k), n1a(k)
      !                   do i = n0i(k), n1i(k)

      !                         dm_gamma(i, a)  = calc_D_ov_gamma_cc3(t2, t1, s2, &
      !                               rvec, nocc, nactive, i, a) 
      !                         dm_xi(i, a) = calc_D_ov_xi_cc3()
      !                   end do
      !             end do                              
      !       end do
      !       !$omp end do
      !       !$omp end parallel

      ! end subroutine dotasks_dm_ia_cc3


      ! subroutine dotasks_dm_ai_cc3(dm_gamma, dm_xi, n0a, n1a, n0i, n1i, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0a, n1a
      !       integer, dimension(:), intent(in)     :: n0i, n1i
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: i, a

      !       !$omp parallel private(k) default(shared)
      !       !$omp do schedule(dynamic)
      !       do k = 1, ntasks
      !             do a = n0a(k), n1a(k)
      !                   do i = n0i(k), n1i(k)

      !                         dm_gamma(a, i)  = calc_D_vo_gamma_cc3(t2, t1, s2, s1, &
      !                               rvec, nocc, nactive, a, i) 
      !                         dm_xi(a, i) = calc_D_vo_xi_cc3(t2, t1, &
      !                               lvec, nocc, nactive, a, i)
      !                   end do
      !             end do                              
      !       end do
      !       !$omp end do
      !       !$omp end parallel

      ! end subroutine dotasks_dm_ai_cc3



      ! subroutine dotasks_dm_ab_cc3(dm_gamma, dm_xi, n0a, n1a, n0b, n1b, nocc, rvec, lvec, &
      !       t1, t2, s1, s2, ntasks, nactive)

      !       real(F64), dimension(:,:)             :: dm_gamma
      !       real(F64), dimension(:,:)             :: dm_xi
      !       integer, dimension(:), intent(in)     :: n0a, n1a
      !       integer, dimension(:), intent(in)     :: n0b, n1b
      !       integer, intent(in)                   :: nocc
      !       real(F64), dimension(:), intent(in) :: rvec
      !       real(F64), dimension(:), intent(in) :: lvec
      !       double precision, dimension(:, :, :, :), intent(in) :: t2 
      !       double precision, dimension(:, :, :, :), intent(in) :: s2 
      !       double precision, dimension(:, :), intent(in)                  :: t1 
      !       double precision, dimension(:, :), intent(in)                  :: s1
      !       integer, intent(in)                   :: ntasks
      !       integer, intent(in)                   :: nactive
      !       integer :: k
      !       integer :: a, b

      !       !$omp parallel private(k) default(shared)
      !       !$omp do schedule(dynamic)
      !       do k = 1, ntasks
      !             do a = n0a(k), n1a(k)
      !                   do b = n0b(k), n1b(k)

      !                         dm_gamma(a, b) = calc_D_vv_gamma_cc3(t2, t1, s2, s1, &
      !                               rvec, nocc, nactive, a, b) 
      !                         dm_xi(a, b) = calc_D_vv_xi_cc3(t2, t1, &
      !                               lvec, nocc, nactive, a, b)
      !                   end do
      !             end do                              
      !       end do
      !       !$omp end do
      !       !$omp end parallel
      ! end subroutine dotasks_dm_ab_cc3
