module ccsd_intermediates

      use ccsd_transformed_integrals
      use cc3_intermediates
      use gparam
      use basis
      use automatic_t1_t2_amplitudes
      use symmetry
      use cc_gparams

      use cmpidx

      implicit none
      ! ---------------------------------------------------------------------------
      ! Scuseria, G.; Scheiner, A. ; Lee, T. et al. The Journal of Chemical Physics
      ! 86 no 5 p.2881-2890 (1987)
      ! 
      ! ---------------------------------------------------------------------------

contains

      function t1_amplitude(eorb, a, i, t2, t1, nocc, nactive)
            
            double precision :: t1_amplitude
            double precision, dimension(:), intent(in)                                  :: eorb
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, i

            
            t1_amplitude = automatic_t1_h(t2, nocc, nactive, a, i) &
                  / (eorb(i) - eorb(a) + CC_ETA) &
                  +t1(a,i)

      end function t1_amplitude

      function t1dgnst(t1, nocc, nvirt, nactive)
        double precision :: t1dgnst
        integer, intent(in) :: nocc, nvirt, nactive
        double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
        integer :: a, i, npair

        npair = nocc * nvirt

        t1dgnst = 0.d+0
        do i = 1, nocc                                                                                                          
           do a = nocc+1,nactive                                                                                           
              t1dgnst = t1dgnst + t1(a, i)**2 
           end do
        end do

        t1dgnst = sqrt(t1dgnst/NE)

      end function t1dgnst

      function t2_amplitude(eorb, a, b, i, j, t2, nocc, nactive)
            double precision :: t2_amplitude
            double precision, dimension(:), intent(in)                                  :: eorb
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, b, i, j

            t2_amplitude = automatic_t2_h_intermediates(t2, nocc, nactive, a, i, b, j)&
                        / (eorb(j) + eorb(i) - eorb(b) - eorb(a) + 2.d+0 * CC_ETA)&
                        +t2(a,b,i,j)

            ! t2_amplitude = automatic_t2_h(t2, nocc, nactive, a, i, b, j) &
            !       / (eorb(j) + eorb(i) - eorb(b) - eorb(a) + 2.d+0 * CC_ETA)&
            !       +t2(a,b,i,j)



      end function t2_amplitude

      subroutine ccsd_convrg_short(t1t2vecold, t1t2vec, n, q, err, ediff)
        double precision, dimension(:), intent(in) :: t1t2vecold
        double precision, dimension(:), intent(in) :: t1t2vec
        integer, intent(in) :: n
        logical, intent(out) :: q
        double precision, intent(out) ::  err
        double precision, intent(in) :: ediff

        integer :: k
        
        err = 0.d+0
        ! do k = 1, n
        !    err = err +  ((t1t2vecold(k) - t1t2vec(k)))**2
        ! end do

        !        sum = ZERO
        do k = 1, n
           err = err + abs(t1t2vecold(k) - t1t2vec(k))
           !          sum = sum + abs(t1t2vec(k))
        end do

        err = err / n
        !      sum = sum / n
        !        err = sqrt(err / n)
        !        err = sqrt(err)
        
        q = .false.
        if(err.lt.CC_AMP_THRESH ) then
           if(abs(ediff).lt.CC_E_THRESH) q = .true.
        end if
      end subroutine ccsd_convrg_short
        

      subroutine ccsd_convrg(nocc, nactive, t2old, t1old, t2new, t1new, q, err)
            integer, intent(in)                                                        :: nocc
            integer, intent(in)                                                        :: nactive
            double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(in)                                :: t2old
            double precision, dimension(nocc+1:nactive, nocc), intent(in)        :: t1old
            double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(in)                                :: t2new
            double precision, dimension(nocc+1:nactive, nocc), intent(in)        :: t1new
            logical, intent(out) :: q
            double precision, intent(out) :: err

            double precision, parameter :: eps = 1.d-8
            integer :: beta, gamma, u , v
            integer :: nv

            err = 0.d+0
            nv = nactive - nocc
            do u = 1, nocc
                  do beta = nocc+1,nactive
                        if(abs(t1new(beta, u)).gt.1.d-14)then
                              err = err +  ((t1old(beta, u) - t1new(beta, u)))**2
                        end if
                  end do
            end do

            do v = 1, nocc
                  do u = 1, nocc
                        do gamma = nocc+1,nactive
                              do beta = nocc + 1, nactive
                                    if(abs(t2new(gamma, beta, u, v)).gt.1.d-14)then
                                          err = err + (t2old(gamma, beta,u, v) - t2new(gamma, beta, u, v))**2
                                    end if
                              end do
                        end do
                  end do
            end do
            err = err / (nv*nocc*(1+nv*nocc))
            err = sqrt(err)
            q = .false.
            if(err.lt.eps) q = .true.

      end subroutine ccsd_convrg

      function cc_correction(t2, t1, nocc, nactive)
            
            double precision :: cc_correction
            integer, intent(in)  :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)     :: t1

            integer :: i, j, a, b
            integer :: licznik

            licznik = 0 
            cc_correction = zero
            do j = 1, nocc
                  do i = 1, nocc
                        do b = nocc + 1, nactive
                              do a = nocc + 1, nactive

                                    cc_correction = cc_correction + tau(t2, t1, nocc, nactive, a, b,i, j)&
                                          * (2.d+0 * vovo(a, i, b, j) - vovo(b, i, a, j))
                              end do
                        end do
                  end do
            end do

      end function cc_correction

      function cc_correction_licz(t2, t1, nocc, nactive)

           double precision :: cc_correction_licz
            integer, intent(in)  :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)     :: t1

            integer :: i, j, a, b, k
            integer :: licznik

            licznik = 0
            cc_correction_licz = zero
            do j = 1, nocc
                  do i = 1, nocc
                        do b = nocc + 1, nactive
                              do a = nocc + 1, nactive
                                    cc_correction_licz = cc_correction_licz + tau(t2, t1, nocc, nactive, a, b,i, j)
                              end do
                        end do
                  end do
            end do
            
      end function cc_correction_licz


      subroutine tm_mtrx(tmxx, tmyy, tmzz, tm)
            double precision, dimension(:), intent(in) :: tmxx
            double precision, dimension(:), intent(in) :: tmyy
            double precision, dimension(:), intent(in) :: tmzz
            double precision, dimension(:,:), intent(out) :: tm

            tm(:,1) = tmxx
            tm(:,2) = tmyy
            tm(:,3) = tmzz

      end subroutine tm_mtrx

      function tau(t2, t1, nocc, nactive, beta, gamma, u, v)
            double precision :: tau
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v


            tau = t2(beta, gamma, u, v) + t1(beta, u) * t1(gamma, v)
      end function tau


      subroutine irrep_idx_for_intermediates(gidimd, girrep0, girrep1, gidoubles, &
            i_ij, i_i, i_a, i_ai, i_bi, di_ij, di_i, di_a, di_ai, di_bi)
            integer, intent(in) :: gidimd
            integer, dimension(:,:), intent(in) :: girrep0
            integer, dimension(:,:), intent(in) :: girrep1
            integer, dimension(:,:), intent(in) :: gidoubles
            integer, dimension(:,:), allocatable,  intent(out) :: i_ij, i_ai, i_bi
            integer, dimension(:,:), allocatable, intent(out) :: i_a, i_i
            integer, intent(out) :: di_ij, di_ai, di_i, di_a, di_bi

            integer :: m0a, m1a, m0b, m1b
            integer :: m0i, m1i, m0j, m1j
            integer :: i, j, k
            logical :: add
            integer :: order
            integer, dimension(:,:), allocatable :: work
            

            if(POINT_GROUP == D2h) then
                  order = 8
            else if (POINT_GROUP == C2v)then
                  order = 4
            end if

            di_ij = 0
            di_i = 0
            di_a = 0
            di_ai = 0
            di_bi = 0

            allocate(work(8, gidimd * order * order))
            work = 0
            
            do i = 1, gidimd
                  call loop_boundaries_sp(gidoubles(1:2, i), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gidoubles(3:4, i), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  
                  !
                  ! Check ij pair
                  !
                  add = .true.
                  do j = 1, di_ij
                        if (m0i == work(1, j) .and. m0j == work(3, j)) then
                              add = .false.
                              exit
                        end if
                  end do
                  
                  if (add .eqv. .true.) then
                        do j = 1, order
                              if (girrep0(1, j) .ne. 0)then
                                    do k = 1, order
                                          if (girrep0(1, k) .ne. 0)then
                                                di_ij = di_ij + 1
                                                work(1, di_ij) = m0i
                                                work(2, di_ij) = m1i
                                                work(3, di_ij) = m0j
                                                work(4, di_ij) = m1j
                                                work(5, di_ij) = girrep0(1, j)
                                                work(6, di_ij) = girrep1(1, j)
                                                work(7, di_ij) = girrep0(1, k)
                                                work(8, di_ij) = girrep1(1, k)
                                          end if
                                    end do
                              end if
                        end do                        
                  end if
            end do

            allocate(i_ij(8, di_ij))
            i_ij = 0
            i_ij = work(:, 1:di_ij)
            work = 0
                        
            do i = 1, gidimd
                  call loop_boundaries_sp(gidoubles(1:2, i), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gidoubles(3:4, i), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  
                  !
                  ! Check ai pair
                  ! 
                  add = .true.
                  do j = 1, di_ai
                        if (m0b == work(1, j) .and. m0j == work(3, j)) then
                              add = .false.
                              exit
                        end if
                  end do

                  if (add .eqv. .true.) then
                        do j = 1, order
                              if (girrep0(2, j) .ne. 0)then
                                    do k = 1, order
                                          if (girrep0(1, k) .ne. 0)then
                                                di_ai = di_ai + 1
                                                work(1, di_ai) =m0b
                                                work(2, di_ai) =m1b
                                                work(3, di_ai) =m0j
                                                work(4, di_ai) =m1j
                                                work(5, di_ai) = girrep0(2, j)
                                                work(6, di_ai) = girrep1(2, j)
                                                work(7, di_ai) = girrep0(1, k)
                                                work(8, di_ai) = girrep1(1, k)
                                          end if
                                    end do
                              end if
                        end do

                  end if
             end do
             
             allocate(i_ai(8, di_ai))
             i_ai = 0
             i_ai = work(:, 1:di_ai)
             work = 0
             
            do i = 1, gidimd
                  call loop_boundaries_sp(gidoubles(1:2, i), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gidoubles(3:4, i), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  !
                  ! Check bi pair
                  !

                  add = .true.
                  do j = 1, di_bi
                        if (m0b == work(1, j) .and. m0i == work(3, j)) then
                              add = .false.
                              exit
                        end if
                  end do

                  if (add .eqv. .true.) then
                        do j = 1, order
                              if (girrep0(2, j) .ne. 0)then
                                    do k = 1, order
                                          if (girrep0(1, k) .ne. 0)then
                                                di_bi = di_bi + 1
                                                work(1, di_bi) =m0b
                                                work(2, di_bi) =m1b
                                                work(3, di_bi) =m0i
                                                work(4, di_bi) =m1i
                                                work(5, di_bi) = girrep0(2, j)
                                                work(6, di_bi) = girrep1(2, j)
                                                work(7, di_bi) = girrep0(1, k)
                                                work(8, di_bi) = girrep1(1, k)
                                          end if
                                    end do
                              end if
                        end do

                  end if
             end do

             allocate(i_bi(8, di_bi))
             i_bi = 0
             i_bi = work(:, 1:di_bi)
             deallocate(work)
             allocate(work(4, order*order))
             
             di_i = 0
             di_a = 0

             do i = 1, order
                   if (girrep0(1, i) .ne. 0)then
                         do j = 1, order
                               if (girrep0(1, j) .ne. 0)then
                                     di_i = di_i + 1
                                     work(1, di_i) = girrep0(1, i)
                                     work(2, di_i) = girrep1(1, i)
                                     work(3, di_i) = girrep0(1, j)
                                     work(4, di_i) = girrep1(1, j)
                               end if
                         end do

                   end if
             end do

             allocate(i_i(4, di_i))
             i_i = 0
             i_i = work(:, 1:di_i)
             work = 0

             do i = 1, order
                   if (girrep0(2, i) .ne. 0)then
                         do j = 1, order
                               if (girrep0(2, j) .ne. 0)then
                                     di_a = di_a + 1
                                     work(1, di_a) = girrep0(2, i)
                                     work(2, di_a) = girrep1(2, i)
                                     work(3, di_a) = girrep0(2, j)
                                     work(4, di_a) = girrep1(2, j)
                               end if
                         end do

                   end if                         
             end do
             allocate(i_a(4, di_a))
             i_a = 0
             i_a = work(:, 1:di_a)
             deallocate(work)

      end subroutine irrep_idx_for_intermediates

      subroutine t2_intermediates(t2, gidimd, ggirrep0, ggirrep1, gidoubles, max_ntasks, &
            nocc0, nocc1, nvirt0, nvirt1)

            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1
            real(F64), dimension(nvirt0:nvirt1,nvirt0:nvirt1,nocc0:nocc1,nocc0:nocc1), intent(in) :: t2
            integer, intent(in) :: gidimd
            integer, dimension(:,:), intent(in) :: ggirrep0
            integer, dimension(:,:), intent(in) :: ggirrep1
            integer, dimension(:,:), intent(in) :: gidoubles
            integer, intent(in) :: max_ntasks

            integer :: pi, pj, pk, pl
            integer :: pb, pc
            integer, dimension(:,:), allocatable :: idoubles, isingles
            integer :: ntasks
            integer :: i

            integer, dimension(:,:), allocatable :: i_ij, i_ai, i_bi
            integer, dimension(:,:), allocatable :: i_a, i_i
            integer :: di_ij, di_ai, di_i, di_a, di_bi

            call irrep_idx_for_intermediates(gidimd, ggirrep0, ggirrep1, gidoubles, &
                  i_ij, i_i, i_a, i_ai, i_bi, di_ij, di_i, di_a, di_ai, di_bi)

            allocate(isingles(max_ntasks, 2))
            allocate(idoubles(max_ntasks, 4))

            ntasks = 0
            
            do i = 1, di_ij
                  do pi = i_ij(1, i), i_ij(2, i)
                        do pj = i_ij(3, i), i_ij(4, i)
                              do pk = i_ij(5, i), i_ij(6, i)
                                    do pl = i_ij(7, i), i_ij(8, i)
                                          ntasks = ntasks + 1

                                          idoubles(ntasks, 1) = pi
                                          idoubles(ntasks, 2) = pj
                                          idoubles(ntasks, 3) = pk
                                          idoubles(ntasks, 4) = pl
                                          
                                          if (ntasks == max_ntasks) then
                  call t2_interm_1(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
                                                ntasks = 0
                                          end if
                                    end do
                              end do
                        end do
                  end do
            end do
            if (ntasks .gt. 0) then
                  call t2_interm_1(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
                  ntasks = 0
            end if

            do i = 1, di_ai
                  do pb = i_ai(1, i), i_ai(2, i)
                        do pj = i_ai(3, i), i_ai(4, i)
                              do pc = i_ai(5, i), i_ai(6, i)
                                    do pk = i_ai(7, i), i_ai(8, i)

                                          ntasks = ntasks + 1
                                          idoubles(ntasks, 1) = pb
                                          idoubles(ntasks, 2) = pj
                                          idoubles(ntasks, 3) = pc
                                          idoubles(ntasks, 4) = pk


                                          if (ntasks == max_ntasks) then
          call t2_interm_2(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
                                                ntasks = 0
                                          end if
                                    end do
                              end do
                        end do
                  end do
            end do
            if (ntasks .gt. 0) then
                  call t2_interm_2(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
                  ntasks = 0
            end if

            do i = 1, di_bi
                  do pb = i_bi(1, i), i_bi(2, i)
                        do pi = i_bi(3, i), i_bi(4, i)
                              do pc = i_bi(5, i), i_bi(6, i)
                                    do pk = i_bi(7, i), i_bi(8, i)

                                          ntasks = ntasks + 1
                                          idoubles(ntasks, 1) = pb
                                          idoubles(ntasks, 2) = pi
                                          idoubles(ntasks, 3) = pc
                                          idoubles(ntasks, 4) = pk


                                          if (ntasks == max_ntasks) then
          call t2_interm_2b(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
                                                ntasks = 0
                                          end if
                                    end do
                              end do
                        end do
                  end do
            end do
            if (ntasks .gt. 0) then
                  call t2_interm_2b(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
                  ntasks = 0
            end if

            do i = 1, di_i
                  do pi = i_i(1, i), i_i(2, i)
                        do pk = i_i(3, i), i_i(4, i)
                              
                              ntasks = ntasks + 1
                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pk

                              if (ntasks == max_ntasks) then
                                    call t2_interm_3(t2, nocc0, nocc1, nvirt0, nvirt1, isingles, ntasks)
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do
            
            if (ntasks .gt. 0) then
                  call t2_interm_3(t2, nocc0, nocc1, nvirt0, nvirt1, isingles, ntasks)
                  ntasks = 0
            end if

            do i = 1, di_a
                  do pb = i_a(1, i), i_a(2, i)
                        do pc = i_a(3, i), i_a(4, i)

                              ntasks = ntasks + 1
                              isingles(ntasks, 1) = pb
                              isingles(ntasks, 2) = pc

                              if (ntasks == max_ntasks) then
                                    call t2_interm_4(t2, nocc0, nocc1, nvirt0, nvirt1, isingles, ntasks)
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0) then
                  call t2_interm_4(t2, nocc0, nocc1, nvirt0, nvirt1, isingles, ntasks)
                  ntasks = 0
            end if


      end subroutine t2_intermediates    

end module ccsd_intermediates
