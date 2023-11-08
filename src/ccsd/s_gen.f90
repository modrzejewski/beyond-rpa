module s_gen

      use cc3_intermediates
      use symmetry
!      use jacobian
      use jacobian_triplet
      use ccsd_intermediates


      implicit none

      real(F64), dimension(:,:), allocatable :: s2_int_oo_1
      real(F64), dimension(:,:), allocatable :: s2_int_oo_2
      real(F64), dimension(:,:), allocatable :: s2_int_vv_1
      real(F64), dimension(:,:), allocatable :: s2_int_vv_2
      real(F64), dimension(:,:,:,:), allocatable :: s2_int_vo_vo_1
      real(F64), dimension(:,:,:,:), allocatable :: s2_int_vo_vo_2
      real(F64), dimension(:,:,:,:), allocatable :: s2_int_vo_vo_3
      real(F64), dimension(:,:,:,:), allocatable :: s2_int_vo_vo_4
      real(F64), dimension(:,:,:,:), allocatable :: s2_int_vv_vv

contains

      subroutine generate_s1s2(t2, t1, nocc, nactive, irrep0, irrep1, s1, s2, method, s_order)

            real(F64), dimension(:,:,:,:), intent(in) :: t2
            real(F64), dimension(:,:), intent(in) :: t1
            integer, intent(in)                   :: nocc
            integer, intent(in)                   :: nactive
            integer, dimension(:, :), intent(in)  :: irrep0
            integer, dimension(:, :), intent(in)  :: irrep1
            double precision, dimension(nocc+1:nactive,nocc), intent(out)                     :: s1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(out) :: s2 
            integer, intent(in)                                                               :: method
            integer, intent(in) :: s_order

            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles
            integer, dimension(:), allocatable    :: n0a, n1a, n0b, n1b
            integer, dimension(:), allocatable    :: n0i, n1i, n0j, n1j
            integer :: m0a, m1a, m0b, m1b
            integer :: m0i, m1i, m0j, m1j
            integer :: pa, pb, pi, pj
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks
            integer :: idims
            integer :: idimd
            integer :: full_sym
            integer :: i
            type(tclock)                      :: time
            integer :: a, b, j
            double precision :: licz, nor

            call clock_start(time)                  
            call s2_intermediates(t2, nocc, nactive)
            call dmsg("TOTAL TIME S-ORDER 3-INTERMEDIATES", clock_readwall(time)) 

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            allocate(n0a(max_ntasks))
            allocate(n1a(max_ntasks))
            allocate(n0b(max_ntasks))
            allocate(n1b(max_ntasks))
            allocate(n0i(max_ntasks))
            allocate(n1i(max_ntasks))
            allocate(n0j(max_ntasks))
            allocate(n1j(max_ntasks))

            if (POINT_GROUP == C2v) then
                  full_sym = REP_Ag
            else if (POINT_GROUP == D2h) then
                  full_sym = REP_A1
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            s1 = ZERO
            s2 = ZERO

!           nor = zero
!           do j = 1, nocc
!                 do a = nocc + 1, nactive
!                       write(*, '(2I5, F20.10)') a, j, t1(a, j)
!                       nor = nor + t1(a, j) ** 2
!                 end do
!           end do
!           print*, 'lalalalalalal', nor
!           do i = 1, nocc
!                do a = nocc + 1, nactive
!                      do b = nocc + 1, nactive
!                            do j = 1, nocc
!                                  if (abs(t2(a, b, i, j)).gt.1.d-3)then
!                                  write(*, '(4I5, 4F20.5)') a, b, i, j, t2(a, b, i, j)
!                            end if
!                                  if (abs(t2(a, b, i, j)) .gt. 1.d-5)then
!                                        write(*, '(F20.10)') t2(a, b, i, j)
!                                  end if

!                            end do
!                      end do
!                end do
!          end do!

!          licz = cc_correction_licz(t2, t1, nocc, nactive)
!          print*, 'licz', licz

!          stop

           call irrep_singless(full_sym, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
           allocate(isingles(2, idims))
           call irrep_singless(full_sym, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)
           
           call irrep_doubless(full_sym, irrep0, irrep1, 1, nocc, nocc+1, idoubles, POINT_GROUP, idimd, .true., .false.)
           allocate(idoubles(4, idimd))            
           call irrep_doubless(full_sym, irrep0, irrep1, 1, nocc, nocc+1, idoubles, POINT_GROUP, idimd, .false., .false.)

           ntasks = 0
           do i = 1, idims
                 call loop_boundaries_sp(isingles(1:2, i), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a)

                  do pa = m0a, m1a, 6
                        do pi = m0i, m1i, 5

                              ntasks = ntasks + 1
                              call loop_boundaries(pa, m1a, 6, n0a(ntasks), n1a(ntasks))
                              call loop_boundaries(pi, m1i, 5, n0i(ntasks), n1i(ntasks))

                              if (ntasks == max_ntasks)  then
                                    call dotasks_s1_gen(t2, t1, nocc, nactive, s1, method, &
                                          n0i, n1i, n0a, n1a, ntasks, s_order)
                                    ntasks = 0
                              end if

                        end do
                  end do

            end do
            if(ntasks .gt.0)then
                  call dotasks_s1_gen(t2, t1, nocc, nactive, s1, method, &
                        n0i, n1i, n0a, n1a, ntasks, s_order)
            end if

            ntasks = 0
            do i = 1, idimd
                  call loop_boundaries_sp(idoubles(1:2, i), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(idoubles(3:4, i), irrep0, irrep1, &
                        m0j, m1j, m0b, m1b)

                  do pb = m0b, m1b, 6
                        do pj = m0j, m1j, 5
                              do pa = m0a, m1a, 6
                                    do pi = m0i, m1i, 5

                                          ntasks = ntasks + 1

                                          call loop_boundaries(pb, m1b, 6, n0b(ntasks), n1b(ntasks))
                                          call loop_boundaries(pa, m1a, 6, n0a(ntasks), n1a(ntasks))
                                          call loop_boundaries(pi, m1i, 5, n0i(ntasks), n1i(ntasks))
                                          call loop_boundaries(pj, m1j, 5, n0j(ntasks), n1j(ntasks))


                                          if (ntasks == max_ntasks)  then
                                                call dotasks_s2_gen(t2, t1, nocc, nactive, s2, &
                                                      n0i, n1i, n0a, n1a, n0j, n1j, n0b, n1b, ntasks, method, s_order)
                                                ntasks = 0
                                          end if
                                    end do
                              end do
                        end do
                  end do
            end do

            if(ntasks .gt.0)then
                  call dotasks_s2_gen(t2, t1, nocc, nactive, s2, &
                        n0i, n1i, n0a, n1a, n0j, n1j, n0b, n1b, ntasks, method, s_order)
            end if


            call s2_intermediates_free()

      end subroutine generate_s1s2

      subroutine dotasks_s1_gen(t2, t1, nocc, nactive, s1, method, n0i, n1i, n0a, n1a, ntasks, s_order)
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                     :: t1 
            double precision, dimension(nocc+1:nactive,nocc), intent(out)                    :: s1
            integer, intent(in)                                                              :: method
            integer, dimension(:), intent(in) :: n0i
            integer, dimension(:), intent(in) :: n1i
            integer, dimension(:), intent(in) :: n0a
            integer, dimension(:), intent(in) :: n1a
            integer, intent(in) :: ntasks
            integer, intent(in) :: s_order
            integer :: a, i 
            integer :: k

            if(s_order==3)then
                  !$omp parallel private(k) default(shared)                                                                                      
                  !$omp do !schedule(dynamic)                                                                                                     
                  do k = 1, ntasks
                        if (method .eq. THEORY_CCSD)then
                              do a = n0a(k), n1a(k)
                                    do i = n0i(k), n1i(k)
                                          s1(a, i) = s1_ccsd_2(t1, nocc, nactive, a, i) &
                                                + s1_ccsd_3(t2, t1, nocc, nactive, a, i)
                                    end do
                              end do
                        else if (method .eq. THEORY_CC3)then
                              do a = n0a(k), n1a(k)
                                    do i = n0i(k), n1i(k)
                                          s1(a, i) = s1_cc3_2(t1, nocc, nactive, a, i) &
                                                + s1_cc3_3(t2, t1, nocc, nactive, a, i) 
                                                
                                                
                                    end do
                              end do
                        end if
                  end do
                  !$omp end do                                                                                                                            
                  !$omp end parallel 
            else if(s_order==4)then
                  !$omp parallel private(k) default(shared)                                                                                      
                  !$omp do !schedule(dynamic)                                                                                                     
                  do k = 1, ntasks
                        if (method .eq. THEORY_CCSD)then
                              do a = n0a(k), n1a(k)
                                    do i = n0i(k), n1i(k)
                                          s1(a, i) = s1_ccsd_2(t1, nocc, nactive, a, i) &
                                                + s1_ccsd_3(t2, t1, nocc, nactive, a, i) &
                                                + s1_ccsd_4(t2, t1, nocc, nactive, a, i) 

                                    end do
                              end do
                        else if (method .eq. THEORY_CC3)then
                              do a = n0a(k), n1a(k)
                                    do i = n0i(k), n1i(k)
                                          s1(a, i) = s1_cc3_2(t1, nocc, nactive, a, i) &
                                                + s1_cc3_3(t2, t1, nocc, nactive, a, i) &
                                                + s1_cc3_4(t2, t1, nocc, nactive, a, i) 
                                    end do
                              end do
                        end if
                  end do
                  !$omp end do                                                                                                                            
                  !$omp end parallel 
            else
                  call msg("S order not supported")
                  stop
            end if

            ! !$omp parallel private(k) default(shared)
            ! !$omp do !schedule(dynamic)  
            ! do k = 1, ntasks
            !       if (method .eq. THEORY_CCSD)then
            !             do a = n0a(k), n1a(k)
            !                   do i = n0i(k), n1i(k)
            !                         s1(a, i) = s11f(t1, nocc, nactive, a, i) &
            !                               + s12af(t2, t1, nocc, nactive, a, i)
            !                   end do
            !             end do
            !       else if (method .eq. THEORY_CC3)then
            !             do a = n0a(k), n1a(k)
            !                   do i = n0i(k), n1i(k)
            !                         s1(a, i) = s11f(t1, nocc, nactive, a, i) &
            !                               + s12af(t2, t1, nocc, nactive, a, i)&
            !                               + s12bf(t2, nocc, nactive, a, i) 
            !                   end do
            !             end do
            !       end if
            ! end do
            ! !$omp end do
            ! !$omp end parallel

      end subroutine dotasks_s1_gen

      subroutine dotasks_s2_gen(t2, t1, nocc, nactive, s2, n0i, n1i, n0a, n1a, n0j, n1j, n0b, n1b, ntasks, method, s_order)
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer :: a, i, b, j 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                     :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(out) :: s2 
            integer, dimension(:), intent(in) :: n0i
            integer, dimension(:), intent(in) :: n1i
            integer, dimension(:), intent(in) :: n0a
            integer, dimension(:), intent(in) :: n1a
            integer, dimension(:), intent(in) :: n0j
            integer, dimension(:), intent(in) :: n1j
            integer, dimension(:), intent(in) :: n0b
            integer, dimension(:), intent(in) :: n1b
            integer, intent(in) :: ntasks
            integer, intent(in)                                                              :: method
            integer, intent(in) :: s_order
            integer :: k

            if(s_order==3)then
                  !$omp parallel private(k) default(shared)
                  !$omp do !schedule(dynamic)  
                  do k = 1, ntasks
                        if (method .eq. THEORY_CCSD)then
                              do a = n0a(k), n1a(k)
                                    do b = n0b(k), n1b(k)
                                          do i = n0i(k), n1i(k)
                                                do j = n0j(k), n1j(k)
                                                      s2(a, b, i, j) = s2_ccsd_1(t2, nocc, nactive, a, i, b, j)  &
                                                            + s2_ccsd_3(t2, nocc, nactive, a, i, b, j) 
                                                      s2(b, a, j, i) = s2(a, b, i, j)
                                                      
                                                      !  s2(a, b, i, j) = s2_ccsd_1(t2, nocc, nactive, a, i, b, j)+ &
                                                      !        s2_ccsd_3(t2, nocc, nactive, a, i, b, j) 
                                                      ! s2(b, a, j, i) = s2_ccsd_1(t2, nocc, nactive, b, j, a, i)+ &
                                                      !       s2_ccsd_3(t2, nocc, nactive, b, j, a, i) 
                                                      ! print*, a, b, i, j, s2(b, a, j, i)-s2(a, b, i, j)
                                                      ! if(abs(s2(b, a, j, i)-s2(a, b, i, j)).gt.1.d-7)then
                                                      !       stop
                                                      ! end if
                                                      
                                                end do
                                          end do
                                    end do
                              end do
                        else if (method .eq. THEORY_CC3)then
                              do a = n0a(k), n1a(k)
                                    do b = n0b(k), n1b(k)
                                          do i = n0i(k), n1i(k)
                                                do j = n0j(k), n1j(k)
                                                      s2(a, b, i, j) = s2_cc3_1(t2, nocc, nactive, a, i, b, j) &
                                                            + s2_cc3_3(t2, nocc, nactive, a, i, b, j) 
                                                      s2(b, a, j, i) = s2(a, b, i, j)
                                                end do
                                          end do
                                    end do
                              end do
                        end if
                  end do
                  !$omp end do
                  !$omp end parallel
            else if(s_order==4)then
                  !$omp parallel private(k) default(shared)
                  !$omp do !schedule(dynamic)  
                  do k = 1, ntasks
                        if (method .eq. THEORY_CCSD)then
                              do a = n0a(k), n1a(k)
                                    do b = n0b(k), n1b(k)
                                          do i = n0i(k), n1i(k)
                                                do j = n0j(k), n1j(k)
                                                      s2(a, b, i, j) = s2_ccsd_1(t2, nocc, nactive, a, i, b, j)  &
                                                            + s2_ccsd_3(t2, nocc, nactive, a, i, b, j) 
                                                      s2(b, a, j, i) = s2(a, b, i, j)
                                                end do
                                          end do
                                    end do
                              end do
                        else if (method .eq. THEORY_CC3)then
                              do a = n0a(k), n1a(k)
                                    do b = n0b(k), n1b(k)
                                          do i = n0i(k), n1i(k)
                                                do j = n0j(k), n1j(k)
                                                      s2(a, b, i, j) = s2_cc3_1(t2, nocc, nactive, a, i, b, j) &
                                                            + s2_cc3_3(t2, nocc, nactive, a, i, b, j) &
                                                            + s2_cc3_4(t2, t1, nocc, nactive, a, i, b, j) 
                                                      s2(b, a, j, i) = s2(a, b, i, j)
                                                end do
                                          end do
                                    end do
                              end do
                        end if
                  end do
                  !$omp end do
                  !$omp end parallel

            else
                  call msg("S order not supported")
                  stop
            end if

            ! !$omp parallel private(k) default(shared)
            ! !$omp do !schedule(dynamic)  
            ! do k = 1, ntasks
            !       do a = n0a(k), n1a(k)
            !             do b = n0b(k), n1b(k)
            !                   do i = n0i(k), n1i(k)
            !                         do j = n0j(k), n1j(k)
            !                               s2(a, b, i, j) = s21f(t2, nocc, nactive, a, i, b, j) &
            !                                     + s23f(t2, nocc, nactive, a, i, b, j) 
            !                         end do
            !                   end do
            !             end do
            !       end do
            ! end do
            ! !$omp end do
            ! !$omp end parallel
      end subroutine dotasks_s2_gen


      subroutine s_init(t2, t1, nocc, nactive, s1, s2, method)
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in)  :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                      :: t1 
            double precision, dimension(nocc+1:nactive,nocc), intent(out)                     :: s1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(out) :: s2 
            integer, intent(in)                                                               :: method

            call s1_gen(t2, t1, nocc, nactive, s1, method)
            call s2_gen(t2, nocc, nactive, s2)


      end subroutine s_init

      subroutine s1_gen(t2, t1, nocc, nactive, s1, method)
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                     :: t1 
            double precision, dimension(nocc+1:nactive,nocc), intent(out)                    :: s1
            integer, intent(in)                                                              :: method
            integer :: a, i 

            if (method .eq. THEORY_CCSD)then
                  do a = nocc + 1, nactive
                        do i = 1, nocc
                              s1(a, i) = s11f(t1, nocc, nactive, a, i) &
                                    + s12af(t2, t1, nocc, nactive, a, i)
                        end do
                  end do
            else if (method .eq. THEORY_CC3)then
                  do a = nocc + 1, nactive
                        do i = 1, nocc
                              s1(a, i) = s11f(t1, nocc, nactive, a, i) &
                                    + s12af(t2, t1, nocc, nactive, a, i)&
                                    + s12bf(t2, nocc, nactive, a, i) 
                        end do
                  end do
            end if
      end subroutine s1_gen


      subroutine s2_gen(t2, nocc, nactive, s2)
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer :: a, i, b, j 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(out) :: s2 


            do a = nocc + 1, nactive
                  do b = nocc + 1, nactive
                        do i = 1, nocc
                              do j = 1, nocc
                                    s2(a, b, i, j) = s21f(t2, nocc, nactive, a, i, b, j) &
                                          + s23f(t2, nocc, nactive, a, i, b, j) 
                              end do
                        end do
                  end do
            end do
      end subroutine s2_gen


      function s11f(t1, nocc, nactive, a, i) 
            double precision :: s11f
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            integer, intent(in) :: a, i 
            integer :: s  
            double precision, dimension(0:0) :: term 
            term = 0.d+0
            term(0) = term(0) + t1(a,i)

            s11f = 0.d+0 
            do s = 0, 0
                  s11f = s11f + term(s)
            end do
      end function s11f


      function s12af(t2, t1, nocc, nactive, a, i) 
            double precision :: s12af
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            integer, intent(in) :: a, i 
            integer :: s ,j,b 
            double precision, dimension(0:1) :: term 
            term = 0.d+0
            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(0) = term(0) + t1(b,j) * t2(a,b,i,j)
                  end do
            end do

            term(0) = term(0) * 2.0d+0 

            do b = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(1) = term(1) + t1(b,j) * t2(a,b,j,i)
                  end do
            end do

            term(1) = -term(1) 


            s12af = 0.d+0 
            do s = 0, 1
                  s12af = s12af + term(s)
            end do

      end function s12af

      function s12bf(t2, nocc, nactive, a, i) 
            double precision :: s12bf
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer, intent(in) :: a, i 
            integer :: s ,k,b,c,j 
            double precision, dimension(0:3) :: term 
            term = 0.d+0

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do j = 1, nocc 
                                    term(0) = term(0) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,k,j,i)
                                    term(1) = term(1) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,i,j,k)
                              end do
                        end do
                  end do
            end do

            term(0) = term(0) * (-2.0d+0) 
            term(1) = term(1) * 1.9999999999999998d+0 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    term(2) = term(2) + t2(b,c,k,j) * t3(nocc, nactive, a,b,c,k,j,i)
                              end do
                        end do
                  end do
            end do


            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    term(3) = term(3) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,i,k,j)
                              end do
                        end do
                  end do
            end do

            term(3) = -term(3) 


            s12bf = 0.d+0 
            do s = 0, 3
                  s12bf = s12bf + term(s)
            end do

      end function s12bf

      function s21f(t2, nocc, nactive, a, i, b, j) 
            double precision :: s21f
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer, intent(in) :: a, i, b, j 
            integer :: s  
            double precision, dimension(0:0) :: term 
            term = 0.d+0
            term(0) = term(0) + t2(a,b,i,j)



            s21f = 0.d+0 
            do s = 0, 0
                  s21f = s21f + term(s)
            end do

      end function s21f

      function s23f(t2, nocc, nactive, a, i, b, j) 
            double precision :: s23f
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer, intent(in) :: a, i, b, j 
            integer :: s ,l,d,k,c 
            double precision, dimension(0:18) :: term 
            term = 0.d+0
            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    term(0) = term(0) + t2(c,d,k,l) * t2(a,b,k,l) * t2(c,d,i,j)
                                    term(1) = term(1) + t2(c,d,k,l) * t2(a,b,k,j) * t2(c,d,i,l)
                              end do
                        end do
                  end do
            end do

            term(1) = term(1) * (-1.9999999999999998d+0) 

            do l = 1, nocc 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    term(2) = term(2) + t2(c,d,l,k) * t2(a,b,k,j) * t2(c,d,i,l)
                                    term(3) = term(3) + t2(c,d,l,k) * t2(a,b,i,k) * t2(c,d,j,l)
                                    term(4) = term(4) + t2(c,d,k,l) * t2(a,b,i,k) * t2(c,d,j,l)
                                    term(5) = term(5) + t2(c,d,l,k) * t2(a,d,i,l) * t2(b,c,k,j)
                                    term(6) = term(6) + t2(c,d,l,k) * t2(a,c,k,i) * t2(b,d,j,l)
                                    term(7) = term(7) + t2(c,d,l,k) * t2(a,c,i,j) * t2(b,d,k,l)
                                    term(8) = term(8) + t2(c,d,l,k) * t2(a,d,k,l) * t2(b,c,j,i)
                                    term(9) = term(9) + t2(c,d,k,l) * t2(a,c,i,l) * t2(b,d,j,k)
                                    term(10) = term(10) + t2(c,d,k,l) * t2(a,d,i,k) * t2(b,c,j,l)
                                    term(11) = term(11) + t2(c,d,k,l) * t2(a,c,i,k) * t2(b,d,j,l)
                                    term(12) = term(12) + t2(c,d,k,l) * t2(a,d,i,l) * t2(b,c,j,k)
                              end do
                        end do
                  end do
            end do

            term(4) = term(4) * (-1.9999999999999998d+0) 
            term(9) = -term(9) 
            term(10) = -term(10) 
            term(11) = term(11) * 2.0d+0 
            term(12) = term(12) * 2.0d+0 

            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    term(13) = term(13) + t2(c,d,k,l) * t2(a,d,k,j) * t2(b,c,l,i)
                                    term(14) = term(14) + t2(c,d,k,l) * t2(a,c,i,j) * t2(b,d,k,l)
                                    term(15) = term(15) + t2(c,d,k,l) * t2(a,d,k,l) * t2(b,c,j,i)
                                    term(16) = term(16) + t2(c,d,k,l) * t2(a,c,k,i) * t2(b,d,j,l)
                                    term(17) = term(17) + t2(c,d,k,l) * t2(a,d,i,l) * t2(b,c,k,j)
                                    term(18) = term(18) + t2(c,d,k,l) * t2(a,c,k,i) * t2(b,d,l,j)
                              end do
                        end do
                  end do
            end do

            term(14) = term(14) * (-1.9999999999999998d+0) 
            term(15) = term(15) * (-1.9999999999999998d+0) 
            term(16) = term(16) * (-1.999999999999999d+0) 
            term(17) = term(17) * (-1.999999999999999d+0) 


            s23f = 0.d+0 
            do s = 0, 18
                  s23f = s23f + term(s)
            end do
      end function s23f

      !____________________________________________________________________________________________

      function s1_ccsd_2(t1, nocc, nactive, a, i) 
            double precision :: s1_ccsd_2
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i 
            integer :: s  
            double precision, dimension(0:0) :: term 
            term = 0.d+0
            term(0) = term(0) + t1(a,i)



            s1_ccsd_2 = 0.d+0 
            do s = 0, 0
                  s1_ccsd_2 = s1_ccsd_2 + term(s)
            end do

      end function s1_ccsd_2

      function s1_ccsd_3(t2, t1, nocc, nactive, a, i) 
            double precision :: s1_ccsd_3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i 
            integer :: s ,j,b 
            double precision, dimension(0:1) :: term 
            term = 0.d+0
            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(0) = term(0) + t1(b,j) * t2(a,b,i,j)
                  end do
            end do

            term(0) = term(0) * 2.0d+0 

            do b = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(1) = term(1) + t1(b,j) * t2(a,b,j,i)
                  end do
            end do

            term(1) = -term(1) 


            s1_ccsd_3 = 0.d+0 
            do s = 0, 1
                  s1_ccsd_3 = s1_ccsd_3 + term(s)
            end do

      end function s1_ccsd_3

      function s1_ccsd_4(t2, t1, nocc, nactive, a, i) 
            double precision :: s1_ccsd_4
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i 
            integer :: s ,j,c,k,b 
            double precision, dimension(0:7) :: term 
            term = 0.d+0
            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    term(0) = term(0) + t1(b,j) * t2(b,c,k,j) * t2(a,c,k,i)
                                    term(1) = term(1) + t1(a,k) * t2(b,c,k,j) * t2(b,c,j,i)
                              end do
                        end do
                  end do
            end do


            do k = 1, nocc 
                  do j = 1, nocc 
                        do c = nocc + 1, nactive 
                              do b = nocc + 1, nactive 
                                    term(2) = term(2) + t1(c,i) * t2(b,c,j,k) * t2(a,b,k,j)
                                    term(3) = term(3) + t1(c,k) * t2(b,c,j,k) * t2(a,b,i,j)
                              end do
                        end do
                  end do
            end do

            term(2) = term(2) * (-2.0d+0) 
            term(3) = term(3) * 4.0d+0 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              do k = 1, nocc 
                                    term(4) = term(4) + t1(c,i) * t2(b,c,k,j) * t2(a,b,k,j)
                                    term(5) = term(5) + t1(c,k) * t2(b,c,k,j) * t2(a,b,i,j)
                              end do
                        end do
                  end do
            end do

            term(5) = term(5) * (-2.0d+0) 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    term(6) = term(6) + t1(b,j) * t2(b,c,j,k) * t2(a,c,k,i)
                                    term(7) = term(7) + t1(a,k) * t2(b,c,j,k) * t2(b,c,j,i)
                              end do
                        end do
                  end do
            end do

            term(6) = term(6) * (-2.0d+0) 
            term(7) = term(7) * (-2.0d+0) 


            s1_ccsd_4 = 0.d+0 
            do s = 0, 7
                  s1_ccsd_4 = s1_ccsd_4 + term(s)
            end do

      end function s1_ccsd_4

      function s2_ccsd_1(t2, nocc, nactive, a, i, b, j) 
            double precision :: s2_ccsd_1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            
            integer, intent(in) :: a, i, b, j 
            integer :: s  
            double precision, dimension(0:0) :: term 
            term = 0.d+0
            term(0) = term(0) + t2(a,b,i,j)



            s2_ccsd_1 = 0.d+0 
            do s = 0, 0
                  s2_ccsd_1 = s2_ccsd_1 + term(s)
            end do

      end function s2_ccsd_1

      subroutine s2_intermediates(t2, nocc, nactive) 
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2             
            integer :: a, b, i, l, d, k, c 
            real(F64) :: sum1, sum2, sum


            allocate(s2_int_oo_1(nocc, nocc))
            allocate(s2_int_vv_1(nocc + 1:nactive, nocc + 1:nactive))
            allocate(s2_int_oo_2(nocc, nocc))
            allocate(s2_int_vv_2(nocc + 1 : nactive, nocc + 1 : nactive))
            allocate(s2_int_vo_vo_1(nocc + 1:nactive, nocc + 1:nactive, nocc, nocc))
            allocate(s2_int_vo_vo_2(nocc + 1:nactive, nocc + 1:nactive, nocc, nocc))
            allocate(s2_int_vo_vo_3(nocc + 1:nactive, nocc + 1:nactive, nocc, nocc))
            allocate(s2_int_vo_vo_4(nocc + 1:nactive, nocc + 1:nactive, nocc, nocc))
            allocate(s2_int_vv_vv(nocc + 1:nactive, nocc + 1:nactive, nocc + 1:nactive, nocc + 1:nactive))

            s2_int_oo_1 = zero
            s2_int_vv_1 = zero
            s2_int_oo_2 = zero
            s2_int_vv_2 = zero
            s2_int_vo_vo_1 = zero
            s2_int_vo_vo_2 = zero
            s2_int_vo_vo_3 = zero
            s2_int_vo_vo_4 = zero
            s2_int_vv_vv = zero

            do i = 1, nocc
                  do k = 1, nocc  
                        sum1 = zero
                        sum2 = zero
                        do d = nocc + 1, nactive
                              do c = nocc + 1, nactive 
                                    do l = 1, nocc
                                          sum1 = sum1 + t2(c, d, l, k) * t2(c, d, i, l)
                                          sum2 = sum2 + t2(c, d, k, l) * t2(c, d, i, l)
                                    end do
                              end do
                        end do
                        s2_int_oo_1(k, i) = sum1
                        s2_int_oo_2(k, i) = sum2
                  end do
            end do

            do b = nocc + 1, nactive
                  do c = nocc + 1, nactive 
                        sum1 = zero
                        sum2 = zero
                        do d = nocc + 1, nactive
                                    do l = 1, nocc
                                          do k = 1, nocc            
                                               sum1 = sum1 + t2(c, d, k, l) * t2(b, d, k, l)
                                               sum2 = sum2 + t2(c, d, l, k) * t2(b, d, k, l)
                                    end do
                              end do
                        end do
                        s2_int_vv_1(b, c) = sum1
                        s2_int_vv_2(b, c) = sum2
                  end do
            end do

            do a = nocc + 1, nactive
                  do i = 1, nocc
                        do c = nocc + 1, nactive
                              do k = 1, nocc
                                    sum = zero
                                    do d = nocc + 1, nactive
                                          do l = 1, nocc
                                                sum = sum + t2(d, c, l, k) * t2(d, a, l, i)                                                     
                                          end do
                                    end do
                                    s2_int_vo_vo_1(a, c, i, k) = sum
                              end do
                        end do
                  end do
            end do

            do a = nocc + 1, nactive
                  do i = 1, nocc
                        do d = nocc + 1, nactive                              
                              do k = 1, nocc
                                    sum = zero
                                    do c = nocc + 1, nactive
                                          do l = 1, nocc
                                                sum = sum + t2(d, c, l, k) * t2(c, a, l, i)
                                          end do
                                    end do
                                    s2_int_vo_vo_2(a, d, i, k) = sum
                              end do
                        end do
                  end do
            end do

            do a = nocc + 1, nactive
                  do i = 1, nocc
                        do d = nocc + 1, nactive                              
                              do l = 1, nocc
                                    sum = zero
                                    do c = nocc + 1, nactive
                                          do k = 1, nocc
                                                sum = sum + t2(d, c, l, k) * t2(c, a, i, k)
                                          end do
                                    end do
                                    s2_int_vo_vo_3(a, d, i, l) = sum
                              end do
                        end do
                  end do
            end do

           do a = nocc + 1, nactive
                  do i = 1, nocc
                        do c = nocc + 1, nactive                              
                              do l = 1, nocc
                                    sum = zero
                                    do d = nocc + 1, nactive
                                          do k = 1, nocc
                                                sum = sum + t2(d, c, l, k) * t2(d, a, i, k)
                                          end do
                                    end do
                                    s2_int_vo_vo_4(a, c, i, l) = sum
                              end do
                        end do
                  end do
            end do            

            do a = nocc + 1, nactive
                  do b = nocc + 1, nactive
                        do d = nocc + 1, nactive
                              do c = nocc + 1, nactive
                                    sum = zero
                                    do l = 1, nocc
                                          do k = 1, nocc
                                                sum = sum + t2(d, c, l, k) * t2(b, a, l, k)
                                          end do
                                    end do
                                    s2_int_vv_vv(b, a, d, c) = sum
                              end do
                        end do
                  end do
            end do

       
            
      end subroutine s2_intermediates

      subroutine s2_intermediates_free()
            deallocate(s2_int_oo_1)
            deallocate(s2_int_vv_1)
            deallocate(s2_int_oo_2)
            deallocate(s2_int_vv_2)
            deallocate(s2_int_vo_vo_1)
            deallocate(s2_int_vo_vo_2)
            deallocate(s2_int_vo_vo_3)
            deallocate(s2_int_vo_vo_4)
            deallocate(s2_int_vv_vv)
      end subroutine s2_intermediates_free

      ! function s2_ccsd_3(t2, nocc, nactive, a, i, b, j) 
      !       double precision :: s2_ccsd_3
      !       integer, intent(in) :: nocc, nactive
      !       double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            
      !       integer, intent(in) :: a, i, b, j 
      !       integer :: s ,l,d,k,c 
      !       double precision, dimension(0:18) :: term 
      !       term = 0.d+0
      !       do l = 1, nocc 
      !             do d = nocc + 1, nactive 
      !                   do k = 1, nocc 
      !                         do c = nocc + 1, nactive 
      !                               term(0) = term(0) + t2(c,d,k,l) * t2(a,b,k,l) * t2(c,d,i,j)
      !                               term(1) = term(1) + t2(c,d,k,l) * t2(a,b,k,j) * t2(c,d,i,l)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(1) = term(1) * (-1.9999999999999998d+0) 

      !       do l = 1, nocc 
      !             do k = 1, nocc 
      !                   do d = nocc + 1, nactive 
      !                         do c = nocc + 1, nactive 
      !                               term(2) = term(2) + t2(c,d,l,k) * t2(a,b,k,j) * t2(c,d,i,l)
      !                               term(3) = term(3) + t2(c,d,l,k) * t2(a,b,i,k) * t2(c,d,j,l)
      !                               term(4) = term(4) + t2(c,d,k,l) * t2(a,b,i,k) * t2(c,d,j,l)
      !                               term(5) = term(5) + t2(c,d,l,k) * t2(a,d,i,l) * t2(b,c,k,j)
      !                               term(6) = term(6) + t2(c,d,l,k) * t2(a,c,k,i) * t2(b,d,j,l)
      !                               term(7) = term(7) + t2(c,d,l,k) * t2(a,c,i,j) * t2(b,d,k,l)
      !                               term(8) = term(8) + t2(c,d,l,k) * t2(a,d,k,l) * t2(b,c,j,i)
      !                               term(9) = term(9) + t2(c,d,k,l) * t2(a,c,i,l) * t2(b,d,j,k)
      !                               term(10) = term(10) + t2(c,d,k,l) * t2(a,d,i,k) * t2(b,c,j,l)
      !                               term(11) = term(11) + t2(c,d,k,l) * t2(a,c,i,k) * t2(b,d,j,l)
      !                               term(12) = term(12) + t2(c,d,k,l) * t2(a,d,i,l) * t2(b,c,j,k)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(4) = term(4) * (-1.9999999999999998d+0) 
      !       term(9) = -term(9) 
      !       term(10) = -term(10) 
      !       term(11) = term(11) * 2.0d+0 
      !       term(12) = term(12) * 2.0d+0 

      !       do l = 1, nocc 
      !             do d = nocc + 1, nactive 
      !                   do c = nocc + 1, nactive 
      !                         do k = 1, nocc 
      !                               term(13) = term(13) + t2(c,d,k,l) * t2(a,d,k,j) * t2(b,c,l,i)
      !                               term(14) = term(14) + t2(c,d,k,l) * t2(a,c,i,j) * t2(b,d,k,l)
      !                               term(15) = term(15) + t2(c,d,k,l) * t2(a,d,k,l) * t2(b,c,j,i)
      !                               term(16) = term(16) + t2(c,d,k,l) * t2(a,c,k,i) * t2(b,d,j,l)
      !                               term(17) = term(17) + t2(c,d,k,l) * t2(a,d,i,l) * t2(b,c,k,j)
      !                               term(18) = term(18) + t2(c,d,k,l) * t2(a,c,k,i) * t2(b,d,l,j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(14) = term(14) * (-1.9999999999999998d+0) 
      !       term(15) = term(15) * (-1.9999999999999998d+0) 
      !       term(16) = term(16) * (-1.999999999999999d+0) 
      !       term(17) = term(17) * (-1.999999999999999d+0) 


      !       s2_ccsd_3 = 0.d+0 
      !       do s = 0, 18
      !             s2_ccsd_3 = s2_ccsd_3 + term(s)
      !       end do



      ! end function s2_ccsd_3


      function s2_ccsd_3(t2, nocc, nactive, a, i, b, j) 
            double precision :: s2_ccsd_3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            
            integer, intent(in) :: a, i, b, j 
            integer :: s ,l,d,k,c 
            double precision, dimension(0:18) :: term 
            term = 0.d+0

            do k = 1, nocc
                  term(0) = term(0) + t2(b, a, j, k) * s2_int_oo_1(k, i)
                  term(1) = term(1) + t2(b, a, k, i) * s2_int_oo_1(k, j)
                  term(2) = term(2) + t2(b, a, j, k) * s2_int_oo_2(k, i)
                  term(3) = term(3) + t2(b, a, k, i) * s2_int_oo_2(k, j)
            end do

            term(2) = -two * term(2)
            term(3) = -two * term(3)

            do c = nocc + 1, nactive
                  term(4) = term(4) + t2(c, a, j, i) * s2_int_vv_1(c, b)
                  term(5) = term(5) + t2(c, b, i, j) * s2_int_vv_1(c, a)
                  term(6) = term(6) + t2(c, a, j, i) * s2_int_vv_2(c, b)
                  term(7) = term(7) + t2(c, b, i, j) * s2_int_vv_2(c, a)
            end do

            term(4) = -two * term(4)
            term(5) = -two * term(5)

            do d = nocc + 1, nactive
                  do c = nocc + 1, nactive
                        term(8) = term(8) + t2(d, c, j, i) * s2_int_vv_vv(b, a, d, c)
                  end do
            end do
            

            do k = 1, nocc
                  do c = nocc + 1, nactive
                        term(9) = term(9) +   t2(c, b, j, k) * s2_int_vo_vo_1(a, c, i, k)
                        term(10) = term(10) +   t2(c, a, i, k) * s2_int_vo_vo_1(b, c, j, k)
                        term(11) = term(11) +   t2(c, a, k, i) * s2_int_vo_vo_1(b, c, j, k)
                        term(12) = term(12) + t2(c, b, k, j) * s2_int_vo_vo_1(a, c, i, k)
                  end do
            end do

            term(9)  = -two * term(9)
            term(10) = -two * term(10)
            term(11) = two * term(11)
            term(12) = two * term(12)

            do k = 1, nocc
                  do d = nocc + 1, nactive
                        term(13) = term(13) + t2(d, b, j, k) * s2_int_vo_vo_2(a, d, i, k)
                        term(14) = term(14) + t2(d, a, i, k) * s2_int_vo_vo_2(b, d, j, k)
                        term(15) = term(15) + t2(d, b, k, j) * s2_int_vo_vo_2(a, d, i, k)
                        term(16) = term(16) + t2(d, a, k, i) * s2_int_vo_vo_2(b, d, j, k)
                  end do
            end do

            term(15) = -term(15)
            term(16) = -term(16)

            do l = 1, nocc
                  do c = nocc + 1, nactive
                        term(17) = term(17) + t2(c, b, i, l) * s2_int_vo_vo_4(a, c, j, l)
                  end do
            end do

            do l = 1, nocc
                  do d = nocc + 1, nactive
                        term(18) = term(18) + t2(d, b, j, l) * s2_int_vo_vo_3(a, d, i, l)
                  end do
            end do


            s2_ccsd_3 = 0.d+0 
            do s = 0, 18
                  s2_ccsd_3 = s2_ccsd_3 + term(s)
            end do

      end function s2_ccsd_3

      function s1_cc3_2(t1, nocc, nactive, a, i) 
            double precision :: s1_cc3_2
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i 
            integer :: s  
            double precision, dimension(0:0) :: term 
            term = 0.d+0
            term(0) = term(0) + t1(a,i)



            s1_cc3_2 = 0.d+0 
            do s = 0, 0
                  s1_cc3_2 = s1_cc3_2 + term(s)
            end do

      end function s1_cc3_2

      function s1_cc3_3(t2, t1, nocc, nactive, a, i) 
            double precision :: s1_cc3_3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i 
            integer :: s ,l,k,c,d 
            double precision, dimension(0:5) :: term 
            term = 0.d+0
            do l = 1, nocc 
                  do k = 1, nocc 
                        do c = nocc + 1, nactive 
                              do d = nocc + 1, nactive 
                                    term(0) = term(0) + t2(c,d,k,l) * t3(nocc, nactive, a,c,d,k,i,l)
                                    term(1) = term(1) + t2(c,d,l,k) * t3(nocc, nactive, a,c,d,k,i,l)
                              end do
                        end do
                  end do
            end do

            term(0) = term(0) * (-2.0d+0) 

            do l = 1, nocc 
                  do c = nocc + 1, nactive 
                        do d = nocc + 1, nactive 
                              do k = 1, nocc 
                                    term(2) = term(2) + t2(c,d,k,l) * t3(nocc, nactive, a,c,d,i,k,l)
                              end do
                        end do
                  end do
            end do

            term(2) = term(2) * 1.9999999999999998d+0 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        do l = 1, nocc 
                              do d = nocc + 1, nactive 
                                    term(3) = term(3) + t2(c,d,k,l) * t3(nocc, nactive, a,c,d,i,l,k)
                              end do
                        end do
                  end do
            end do

            term(3) = -term(3) 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(4) = term(4) + t1(c,k) * t2(a,c,i,k)
                  end do
            end do

            term(4) = term(4) * 2.0d+0 

            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(5) = term(5) + t1(c,k) * t2(a,c,k,i)
                  end do
            end do

            term(5) = -term(5) 


            s1_cc3_3 = 0.d+0 
            do s = 0, 5
                  s1_cc3_3 = s1_cc3_3 + term(s)
            end do

      end function s1_cc3_3

      function s1_cc3_4(t2, t1, nocc, nactive, a, i) 
            double precision :: s1_cc3_4
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i 
            integer :: s ,l,m,d,e,c,k 
            double precision, dimension(0:25) :: term 
            term = 0.d+0
            do l = 1, nocc 
                  do m = 1, nocc 
                        do d = nocc + 1, nactive 
                              do e = nocc + 1, nactive 
                                    do c = nocc + 1, nactive 
                                          do k = 1, nocc 
                                                term(0) = term(0) + t2(d,e,l,m) * t2(a,c,k,i) * t3(nocc, nactive, c,d,e,m,k,l)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(0) = term(0) * (-0.7499999999999999d+0) 

            do l = 1, nocc 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do m = 1, nocc 
                                    do e = nocc + 1, nactive 
                                          do c = nocc + 1, nactive 
                                                term(1) = term(1) + t2(a,e,k,m) * t2(c,d,i,l) * t3(nocc, nactive, c,d,e,k,m,l)
                                                term(2) = term(2) + t2(c,e,k,m) * t2(a,d,i,l) * t3(nocc, nactive, c,d,e,k,m,l)
                                                term(3) = term(3) + t2(d,e,l,m) * t2(a,c,k,i) * t3(nocc, nactive, c,d,e,k,m,l)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(1) = term(1) * 2.0d+0 
            term(2) = term(2) * (-2.6666666666666674d+0) 

            do l = 1, nocc 
                  do m = 1, nocc 
                        do d = nocc + 1, nactive 
                              do e = nocc + 1, nactive 
                                    do k = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                term(4) = term(4) + t2(a,e,k,m) * t2(c,d,i,l) * t3(nocc, nactive, c,d,e,m,k,l)
                                                term(5) = term(5) + t2(c,e,k,m) * t2(a,d,i,l) * t3(nocc, nactive, c,d,e,m,k,l)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(4) = -term(4) 
            term(5) = term(5) * 1.4999999999999998d+0 

            do m = 1, nocc 
                  do l = 1, nocc 
                        do d = nocc + 1, nactive 
                              do e = nocc + 1, nactive 
                                    do k = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                term(6) = term(6) + t2(a,e,k,m) * t2(c,d,i,l) * t3(nocc, nactive, c,d,e,l,k,m)
                                                term(7) = term(7) + t2(c,e,k,m) * t2(a,d,i,l) * t3(nocc, nactive, c,d,e,l,k,m)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(6) = term(6) * 2.0d+0 
            term(7) = term(7) * (-1.333333333333333d+0) 

            do m = 1, nocc 
                  do l = 1, nocc 
                        do d = nocc + 1, nactive 
                              do e = nocc + 1, nactive 
                                    do c = nocc + 1, nactive 
                                          do k = 1, nocc 
                                                term(8) = term(8) + t2(d,e,l,m) * t2(a,c,k,i) * t3(nocc, nactive, c,d,e,l,k,m)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(8) = term(8) * 1.3333333333333337d+0 

            do m = 1, nocc 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do l = 1, nocc 
                                    do e = nocc + 1, nactive 
                                          do c = nocc + 1, nactive 
                                                term(9) = term(9) + t2(a,e,k,m) * t2(c,d,i,l) * t3(nocc, nactive, c,d,e,k,l,m)
                                                term(10) = term(10) + t2(c,e,k,m) * t2(a,d,i,l) * t3(nocc, nactive, c,d,e,k,l,m)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(9) = term(9) * (-4.0d+0) 
            term(10) = term(10) * 3.999999999999999d+0 

            do k = 1, nocc 
                  do l = 1, nocc 
                        do d = nocc + 1, nactive 
                              do m = 1, nocc 
                                    do e = nocc + 1, nactive 
                                          do c = nocc + 1, nactive 
                                                term(11) = term(11) + t2(a,e,k,m) * t2(c,d,i,l) * t3(nocc, nactive, c,d,e,l,m,k)
                                                term(12) = term(12) + t2(d,e,l,m) * t2(a,c,k,i) * t3(nocc, nactive, c,d,e,l,m,k)
                                                term(13) = term(13) + t2(c,e,k,m) * t2(a,d,i,l) * t3(nocc, nactive, c,d,e,l,m,k)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(11) = -term(11) 
            term(12) = term(12) * (-0.25d+0) 
            term(13) = term(13) * 0.5d+0 

            do k = 1, nocc 
                  do m = 1, nocc 
                        do d = nocc + 1, nactive 
                              do l = 1, nocc 
                                    do e = nocc + 1, nactive 
                                          do c = nocc + 1, nactive 
                                                term(14) = term(14) + t2(a,e,k,m) * t2(c,d,i,l) * t3(nocc, nactive, c,d,e,m,l,k)
                                                term(15) = term(15) + t2(c,e,k,m) * t2(a,d,i,l) * t3(nocc, nactive, c,d,e,m,l,k)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(14) = term(14) * 2.0d+0 
            term(15) = term(15) * (-2.0d+0) 

            do k = 1, nocc 
                  do m = 1, nocc 
                        do d = nocc + 1, nactive 
                              do e = nocc + 1, nactive 
                                    do c = nocc + 1, nactive 
                                          do l = 1, nocc 
                                                term(16) = term(16) + t2(d,e,l,m) * t2(a,c,k,i) * t3(nocc, nactive, c,d,e,m,l,k)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(16) = term(16) * 0.6666666666666665d+0 

            do m = 1, nocc 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do e = nocc + 1, nactive 
                                    do c = nocc + 1, nactive 
                                          do l = 1, nocc 
                                                term(17) = term(17) + t2(d,e,l,m) * t2(a,c,k,i) * t3(nocc, nactive, c,d,e,k,l,m)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            term(17) = term(17) * (-1.9999999999999998d+0) 

            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do l = 1, nocc 
                                    term(18) = term(18) + t1(d,l) * t2(c,d,l,k) * t2(a,c,k,i)
                              end do
                        end do
                  end do
            end do


            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    term(19) = term(19) + t1(c,i) * t2(c,d,k,l) * t2(a,d,k,l)
                                    term(20) = term(20) + t1(c,k) * t2(c,d,k,l) * t2(a,d,i,l)
                                    term(21) = term(21) + t1(a,k) * t2(c,d,k,l) * t2(c,d,i,l)
                              end do
                        end do
                  end do
            end do

            term(19) = term(19) * (-2.0d+0) 
            term(20) = term(20) * 4.0d+0 
            term(21) = term(21) * (-2.0d+0) 

            do l = 1, nocc 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    term(22) = term(22) + t1(c,i) * t2(c,d,l,k) * t2(a,d,k,l)
                                    term(23) = term(23) + t1(c,k) * t2(c,d,l,k) * t2(a,d,i,l)
                                    term(24) = term(24) + t1(a,k) * t2(c,d,l,k) * t2(c,d,i,l)
                              end do
                        end do
                  end do
            end do

            term(23) = term(23) * (-2.0d+0) 

            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    term(25) = term(25) + t1(d,l) * t2(c,d,k,l) * t2(a,c,k,i)
                              end do
                        end do
                  end do
            end do

            term(25) = term(25) * (-2.0d+0) 


            s1_cc3_4 = 0.d+0 
            do s = 0, 25
                  s1_cc3_4 = s1_cc3_4 + term(s)
            end do

      end function s1_cc3_4

      function s2_cc3_1(t2, nocc, nactive, a, i, b, j) 
            double precision :: s2_cc3_1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            
            integer, intent(in) :: a, i, b, j 
            integer :: s  
            double precision, dimension(0:0) :: term 
            term = 0.d+0
            term(0) = term(0) + t2(a,b,i,j)



            s2_cc3_1 = 0.d+0 
            do s = 0, 0
                  s2_cc3_1 = s2_cc3_1 + term(s)
            end do

      end function s2_cc3_1

      function s2_cc3_3(t2, nocc, nactive, a, i, b, j) 
            double precision :: s2_cc3_3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            
            integer, intent(in) :: a, i, b, j 
            integer :: s ,l,d,k,c 
            double precision, dimension(0:18) :: term 
            term = 0.d+0
            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    term(0) = term(0) + t2(c,d,k,l) * t2(a,b,k,l) * t2(c,d,i,j)
                                    term(1) = term(1) + t2(c,d,k,l) * t2(a,b,k,j) * t2(c,d,i,l)
                              end do
                        end do
                  end do
            end do

            term(1) = term(1) * (-1.9999999999999998d+0) 

            do l = 1, nocc 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    term(2) = term(2) + t2(c,d,l,k) * t2(a,b,k,j) * t2(c,d,i,l)
                                    term(3) = term(3) + t2(c,d,l,k) * t2(a,b,i,k) * t2(c,d,j,l)
                                    term(4) = term(4) + t2(c,d,k,l) * t2(a,b,i,k) * t2(c,d,j,l)
                                    term(5) = term(5) + t2(c,d,l,k) * t2(a,d,i,l) * t2(b,c,k,j)
                                    term(6) = term(6) + t2(c,d,l,k) * t2(a,c,k,i) * t2(b,d,j,l)
                                    term(7) = term(7) + t2(c,d,l,k) * t2(a,c,i,j) * t2(b,d,k,l)
                                    term(8) = term(8) + t2(c,d,l,k) * t2(a,d,k,l) * t2(b,c,j,i)
                                    term(9) = term(9) + t2(c,d,k,l) * t2(a,c,i,l) * t2(b,d,j,k)
                                    term(10) = term(10) + t2(c,d,k,l) * t2(a,d,i,k) * t2(b,c,j,l)
                                    term(11) = term(11) + t2(c,d,k,l) * t2(a,c,i,k) * t2(b,d,j,l)
                                    term(12) = term(12) + t2(c,d,k,l) * t2(a,d,i,l) * t2(b,c,j,k)
                              end do
                        end do
                  end do
            end do

            term(4) = term(4) * (-1.9999999999999998d+0) 
            term(9) = -term(9) 
            term(10) = -term(10) 
            term(11) = term(11) * 2.0d+0 
            term(12) = term(12) * 2.0d+0 

            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    term(13) = term(13) + t2(c,d,k,l) * t2(a,d,k,j) * t2(b,c,l,i)
                                    term(14) = term(14) + t2(c,d,k,l) * t2(a,c,i,j) * t2(b,d,k,l)
                                    term(15) = term(15) + t2(c,d,k,l) * t2(a,d,k,l) * t2(b,c,j,i)
                                    term(16) = term(16) + t2(c,d,k,l) * t2(a,c,k,i) * t2(b,d,j,l)
                                    term(17) = term(17) + t2(c,d,k,l) * t2(a,d,i,l) * t2(b,c,k,j)
                                    term(18) = term(18) + t2(c,d,k,l) * t2(a,c,k,i) * t2(b,d,l,j)
                              end do
                        end do
                  end do
            end do

            term(14) = term(14) * (-1.9999999999999998d+0) 
            term(15) = term(15) * (-1.9999999999999998d+0) 
            term(16) = term(16) * (-1.999999999999999d+0) 
            term(17) = term(17) * (-1.999999999999999d+0) 


            s2_cc3_3 = 0.d+0 
            do s = 0, 18
                  s2_cc3_3 = s2_cc3_3 + term(s)
            end do

      end function s2_cc3_3

      function s2_cc3_4(t2, t1, nocc, nactive, a, i, b, j) 
            double precision :: s2_cc3_4
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            
            integer, intent(in) :: a, i, b, j 
            integer :: s ,k,c 
            double precision, dimension(0:2) :: term 
            term = 0.d+0
            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(0) = term(0) + t1(c,k) * t3(nocc, nactive, a,b,c,i,j,k)
                        term(1) = term(1) + t1(c,k) * t3(nocc, nactive, a,b,c,i,k,j)
                        term(2) = term(2) + t1(c,k) * t3(nocc, nactive, a,b,c,k,j,i)
                  end do
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = -term(1) 
            term(2) = -term(2) 


            s2_cc3_4 = 0.d+0 
            do s = 0, 2
                  s2_cc3_4 = s2_cc3_4 + term(s)
            end do

      end function s2_cc3_4



end module s_gen
