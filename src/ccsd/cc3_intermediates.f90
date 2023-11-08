module cc3_intermediates

      use t1_transformed_int
      use basis
      use common_sub
      use ccsd_transformed_integrals
      use automatic_t1_t2_amplitudes
      use symmetry
      use cc_gparams

      implicit none
      
      integer, private :: qbj, qbj2
      integer, private :: qck, qck2
      integer, private :: q00
      integer, private :: qnocc0, qnvirt0      
      
      double precision, dimension(:), allocatable :: t3m
      double precision, dimension(:), allocatable :: t3_temp

      
contains

      ! The CC3 model: An iterative coupled cluster approach including connected triples
      ! Koch, Henrik Christiansen, Ove Jo̸rgensen, Poul Sanchez De Merás, Alfredo M.
      ! The Journal of Chemical Physics 1997 v.106 issue 5 p. 1808

      function t1_amplitude_cc3(eorb, beta, u,t2, t1, nocc, nactive)
            double precision :: t1_amplitude_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(:), intent(in)                                  :: eorb
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: beta, u

            ! t1_amplitude_cc3 = (int_a1(t2, t1, nocc, nvirt, nactive, beta, u) + &
            !       tcts(t2, t1, eorb, beta, u, nocc, nvirt, nactive))/ (eorb(beta) - eorb(u))


            t1_amplitude_cc3 = (automatic_t1_h(t2, nocc,nactive, beta, u)  + &
                  tcts(beta, u, nocc, nactive))&
                  / (eorb(u) - eorb(beta) + CC_ETA)&
                  + t1(beta, u)

            ! t1_amplitude_cc3 = (automatic_t1(t2, t1, nocc, nvirt, nactive, beta, u) + &
            !       tcts(t2, t1,eorb, beta, u, nocc, nvirt, nactive))&
            !       / (eorb(u) - eorb(beta))

            ! call cpu_time(time0)
            ! t1_amplitude_cc3 = (automatic_t1(t2, t1, nocc, nvirt, nactive, beta, u) + &
            !       equation_100(eorb, t2, t1, nocc, nvirt, nactive, beta, u) )/ (eorb(u) - eorb(beta))
            ! call cpu_time(time1)
            ! print*, 'time na eq 100', time1-time0
      end function t1_amplitude_cc3

      function t2_amplitude_cc3(eorb, beta,gamma, u, v, t2, nocc, nactive)
            double precision :: t2_amplitude_cc3
            integer, intent(in) :: nocc,  nactive
            double precision, dimension(:), intent(in)                                  :: eorb
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v

            t2_amplitude_cc3 = (automatic_t2_h_intermediates(t2, nocc, nactive, beta, u, gamma, v))&
                  / (eorb(u) + eorb(v) - eorb(beta) - eorb(gamma) + 2.d+0 * CC_ETA)&
                  + t2(beta, gamma, u, v)

           ! t2_amplitude_cc3 = t2_amplitude_cc3 + tctd(beta, gamma, u, v, nocc, nactive) &
           !       / (eorb(u) + eorb(v) - eorb(beta) - eorb(gamma) + 2.d+0 * CC_ETA)


      end function t2_amplitude_cc3

      function t2_amplitude_tctd(eorb, a, b, i, j, t2, nocc, nactive)
            real(F64) :: t2_amplitude_tctd
            integer, intent(in) :: nocc,  nactive
            double precision, dimension(:), intent(in)                                  :: eorb
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
              integer, intent(in) :: a, b, i, j
            
            t2_amplitude_tctd = tctd(a, b, i, j, nocc, nactive) &
                  / (eorb(i) + eorb(j) - eorb(a) - eorb(b) + 2.d+0 * CC_ETA)

      end function t2_amplitude_tctd


      function t2_amplitude_tctd_symm(eorb, a, b, &
            i, j, t2, nocc, nactive, irrep0, irrep1, &
            gitd1, gitd2, gitd3, gitd1r, gitd2r, gitd3r, &
            gitd1_idx, gitd2_idx, gitd3_idx, &
            gitd1r_idx, gitd2r_idx, gitd3r_idx, idx, &
            icx)

            real(F64) :: t2_amplitude_tctd_symm
            integer, intent(in) :: nocc,  nactive
            real(F64), dimension(:), intent(in)                                  :: eorb
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, b, i, j
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            integer, dimension(:,:), intent(in) :: gitd1,  gitd2,  gitd3
            integer, dimension(:,:), intent(in) :: gitd1r, gitd2r, gitd3r
            integer, dimension(:,:), intent(in) :: gitd1_idx,  gitd2_idx,  gitd3_idx
            integer, dimension(:,:), intent(in) :: gitd1r_idx, gitd2r_idx, gitd3r_idx
            integer, intent(in) :: icx
            integer, intent(in) :: idx
            integer :: k, l, c, d
            integer :: p
            real(F64) :: int_101

            int_101 = zero

            do k = 1, nocc
                  do c = nocc + 1, nactive
                        int_101 = int_101 + (t3(nocc, nactive, a, b, c, i, j, k) &
                              - t3(nocc, nactive,a, b, c, k, j, i)) * in_fov(k, c, nocc)
                  end do
            end do

           do k = 1, nocc
                  do c = nocc + 1, nactive
                        int_101 = int_101 + (t3(nocc, nactive, b, a, c, j, i, k) &
                              - t3(nocc, nactive, b, a, c, k, i, j)) * in_fov(k, c, nocc)
                  end do
            end do

            do l = 1, nocc
                  do k = 1, nocc
                        do c = nocc + 1, nactive
                              int_101 = int_101 - (2.d+0 * t3(nocc, nactive, b, a, c, j, k, l)&
                                    - t3(nocc, nactive, b, a, c, l, k, j) - &
                                    t3(nocc, nactive, b, a, c, j, l, k))*&
                                    tovoo(l, c, k, i) 
                        end do
                  end do
            end do

            do l = 1, nocc
                  do k = 1, nocc
                        do c = nocc + 1, nactive
                              int_101 = int_101 - (2.d+0 * t3(nocc, nactive, a, b, c, i, k, l)&
                                    - t3(nocc, nactive, a, b, c, l, k, i) - &
                                    t3(nocc, nactive, a, b, c, i, l, k))*&
                                    tovoo(l, c, k, j) 
                        end do
                  end do
            end do

            int_101 = int_101 + int_101_part1(nocc, nactive, t2, a, i, b, j, irrep0, irrep1, &
                  gitd1_idx, gitd1, icx)

            int_101 = int_101 + int_101_part1(nocc, nactive, t2, b, j, a, i, irrep0, irrep1, &
                  gitd1r_idx, gitd1r, icx)

            int_101 = int_101 + int_101_part2(nocc, nactive, t2, a, i, b, j, irrep0, irrep1, &
                  gitd2_idx, gitd2, icx)

            int_101 = int_101 + int_101_part2(nocc, nactive, t2, b, j, a, i, irrep0, irrep1, &
                  gitd2r_idx, gitd2r, icx)

            int_101 = int_101 + int_101_part3(nocc, nactive, t2, a, i, b, j, irrep0, irrep1, &
                  gitd3_idx, gitd3, icx)

            int_101 = int_101 + int_101_part3(nocc, nactive, t2, b, j, a, i, irrep0, irrep1, &
                  gitd3r_idx, gitd3r, icx)


            t2_amplitude_tctd_symm = int_101 &
                  / (eorb(i) + eorb(j) - eorb(a) - eorb(b) + 2.d+0 * CC_ETA)


      end function t2_amplitude_tctd_symm


      function int_101_part1(nocc, nactive, t2, a, i, b, j, irrep0, irrep1, &
            gitd_idx, gitd, icx) 
            real(F64) :: int_101_part1
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, i, b, j
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            integer, dimension(:, :), intent(in) :: gitd_idx
            integer, dimension(:, :), intent(in) :: gitd
            integer, intent(in)    :: icx
            integer :: p, c, k, d

            int_101_part1 = zero

            do p = gitd_idx(icx, 1), gitd_idx(icx, 2)
                  do c = irrep0(2, gitd(1, p)), irrep1(2, gitd(1, p))
                        do k = irrep0(1, gitd(2, p)), irrep1(1, gitd(2, p))
                              do d = irrep0(2, gitd(3, p)), irrep1(2, gitd(3, p))

                                    int_101_part1 = int_101_part1 + &
                                          2.d+0 * t3(nocc, nactive,  b, c, d, j, i, k) * tvvov(a, c, k, d)
                              end do
                        end do
                  end do
            end do

            
      end function int_101_part1

     function int_101_part2(nocc, nactive, t2, a, i, b, j, irrep0, irrep1, &
            gitd_idx, gitd, icx) 
            real(F64) :: int_101_part2
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, i, b, j
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            integer, dimension(:, :), intent(in) :: gitd_idx
            integer, dimension(:, :), intent(in) :: gitd
            integer, intent(in)    :: icx
            integer :: p, c, k, d

            int_101_part2 = zero

            do p = gitd_idx(icx, 1), gitd_idx(icx, 2)
                  do c = irrep0(2, gitd(1, p)), irrep1(2, gitd(1, p))
                        do k = irrep0(1, gitd(2, p)), irrep1(1, gitd(2, p))
                              do d = irrep0(2, gitd(3, p)), irrep1(2, gitd(3, p))

                                    int_101_part2 = int_101_part2 - &
                                          t3(nocc, nactive, b, c, d, k, i, j) * tvvov(a, c, k, d)
                              end do
                        end do
                  end do
            end do

            
      end function int_101_part2

     function int_101_part3(nocc, nactive, t2, a, i, b, j, irrep0, irrep1, &
            gitd_idx, gitd, icx) 
            real(F64) :: int_101_part3
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, i, b, j
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            integer, dimension(:, :), intent(in) :: gitd_idx
            integer, dimension(:, :), intent(in) :: gitd
            integer, intent(in)    :: icx
            integer :: p, c, k, d

            int_101_part3 = zero

            do p = gitd_idx(icx, 1), gitd_idx(icx, 2)
                  do c = irrep0(2, gitd(1, p)), irrep1(2, gitd(1, p))
                        do k = irrep0(1, gitd(2, p)), irrep1(1, gitd(2, p))
                              do d = irrep0(2, gitd(3, p)), irrep1(2, gitd(3, p))

                                    int_101_part3 = int_101_part3 - &
                                          t3(nocc, nactive,  b, c, d, j, k, i) * tvvov(a, c, k, d)
                              end do
                        end do
                  end do
            end do

            
      end function int_101_part3


      subroutine t_equation_cc3_init(nocc0, nvirt0, nocc, nactive)
        integer, intent(in) :: nocc
        integer, intent(in) :: nactive
        integer, intent(in) :: nocc0, nvirt0
        
        allocate(t3_temp(nocc**2*(nactive-nocc)**2))
        call t3_init(nocc0, nvirt0, nocc, nactive)
        
      end subroutine t_equation_cc3_init

      subroutine t_equation_cc3_free()
            deallocate(t3_temp)
      end subroutine t_equation_cc3_free

      subroutine cc3_free()

        deallocate(t3m)
      end subroutine cc3_free

      function tcts(a, i, nocc, nactive)
            integer, intent(in) :: nocc, nactive
            double precision :: tcts
            integer, intent(in) :: a, i 
            integer :: b, c, j, k
            !
            ! Helgaker Mol. El. Structure (13.7.58(59)) p. 170 evince
            ! Projection onto biorthogonal bra vector
            ! This is different from Koch et al. paper 
            ! by the factor of 2 eq (90), (91).
            !
            tcts = 0.d+0
            do c = nocc + 1, nactive
                  do b = nocc + 1, nactive
                        do k = 1, nocc
                           do j = 1, nocc
                                    tcts = tcts + (t3(nocc, nactive,  a, b, c, i, j, k)&
                                          - t3(nocc, nactive,  a, b, c, k, j, i)) * lovov(j, b, k, c)
                              end do
                        end do
                  end do
            end do

      end function tcts

      ! function tcts_parallel(a, i, nocc, nactive)
        
      !       integer, intent(in) :: nocc, nactive
      !       double precision :: tcts_parallel
      !       integer, intent(in) :: a, i 
      !       integer :: b, c, j, k
      !       integer :: w
      !       !                                                                                                                        
      !       ! Helgaker Mol. El. Structure (13.7.58(59)) p. 170 evince                                                               
      !       ! Projection onto biorthogonal bra vector                                                                               
      !       ! This is different from Koch et al. paper                                                                              
      !       ! by the factor of 2 eq (90), (91).                                                                                     
      !       !                                                                                                                        
      !       tcts_parallel = 0.d+0
      !       w = 1
      !       do c = nocc + 1, nactive
      !             do b = nocc + 1, nactive
      !                   do k = 1, nocc
      !                         do j = 1, nocc
      !                               t3_temp(w) = (t3(nocc, nactive,  a, b, c, i, j, k)&
      !                                     - t3(nocc, nactive,  a, b, c, k, j, i)) * lovov(j, b, k, c)
      !                               w = w + 1
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       do w = 1, (nocc)**2*(nactive-nocc)**2
      !          tcts_parallel = tcts_parallel + t3_temp(w)
      !       end do

      !     end function tcts_parallel

      subroutine tcts_fast(t1new, nocc, nvirt, nactive)
            integer, intent(in) :: nocc, nvirt, nactive
            double precision, dimension(nocc + 1:nactive,nocc) , intent(inout) :: t1new

            integer :: a, b, c
            integer :: i, j, k
            integer :: ai, bj, ck
            double precision :: t
            integer :: npair

            npair = nocc * nvirt

            do ai = 1, npair
                  a = (ai - 1) / nocc + nocc+1
                  i = ai - nocc * (a - (nocc + 1))
                  bj_lp: do bj = 1, npair
                        if(bj == ai) cycle bj_lp
                        b = (bj - 1) / nocc + nocc+1
                        j = bj - nocc * (b - (nocc + 1))

                        t = t3(nocc, nactive,  a, a, b, i, i, j)
                        
                        call t1_digest(t1new, nocc, nactive, t,  a, a, b, i, i, j)
                        call t1_digest(t1new, nocc, nactive, t,  a, b, a, i, j, i)
                        call t1_digest(t1new, nocc, nactive, t,  b, a, a, j, i, i)

                  end do bj_lp
            end do

            do ai = 1, npair
                  a = (ai - 1) / nocc + nocc+1
                  i = ai - nocc * (a - (nocc + 1))
                  do bj = ai+1, npair
                        b = (bj - 1) / nocc + nocc+1
                        j = bj - nocc * (b - (nocc + 1))
                        do ck = bj+1, npair
                              c = (ck - 1) / nocc + nocc+1
                              k = ck - nocc * (c - (nocc + 1))
                              
                              t = t3(nocc, nactive,  a, b, c, i, j, k)

                              call t1_digest(t1new, nocc, nactive, t, a, b, c, i, j, k)
                              call t1_digest(t1new, nocc, nactive, t, a, c, b, i, k, j)
                              call t1_digest(t1new, nocc, nactive, t, b, a, c, j, i, k)
                              call t1_digest(t1new, nocc, nactive, t, b, c, a, j, k, i)
                              call t1_digest(t1new, nocc, nactive, t, c, a, b, k, i, j)
                              call t1_digest(t1new, nocc, nactive, t, c, b, a, k, j, i)
                        end do
                  end do
            end do
      end subroutine tcts_fast

      subroutine t1_digest(t1new, nocc, nactive, t,  a, b, c, i, j, k)
            integer, intent(in) :: nocc, nactive
            double precision, intent(in) :: t
            double precision, dimension(nocc + 1:nactive,nocc) , intent(inout) :: t1new
            integer, intent(in) :: a, b, c,  i, j, k
            
            t1new(a, i) = t1new(a, i) + t*lovov(j,b,k,c)
            t1new(a, k) = t1new(a, k) - t*lovov(j,b,i,c)
      end subroutine t1_digest

      function tctd_parallel(a, b, i, j, nocc, nactive)
            integer, intent(in) :: nocc, nactive
            double precision :: tctd_parallel
            integer, intent(in) :: a, b, i, j
            double precision :: time0, time1
            !                                                                                                                        
            ! Helgaker Mol. El. Structure (13.7.58(59)) p. 170 evince                                                                
            ! Projection onto biorthogonal bra vector                                                                                
            ! This is different from Koch et al. paper                                                                              
            ! by the factor of 2 eq (90), (91).                                                                                      
            !                                                                                                                        

            call cpu_time(time0)
            tctd_parallel  = (int_101_parallel(nocc, nactive, a, b, i, j)&
                  + int_101_parallel(nocc, nactive,  b, a, j, i))
            call cpu_time(time1)

          end function tctd_parallel


      function tctd(a, b, i, j, nocc, nactive)
            integer, intent(in) :: nocc, nactive
            double precision :: tctd
            integer, intent(in) :: a, b, i, j
            !
            ! Helgaker Mol. El. Structure (13.7.58(59)) p. 170 evince
            ! Projection onto biorthogonal bra vector
            ! This is different from Koch et al. paper 
            ! by the factor of 2 eq (90), (91).
            !

            tctd  = (int_101(nocc, nactive, a, b, i, j)&
                  + int_101(nocc, nactive,  b, a, j, i))

      end function tctd

      subroutine tctd_fast(t2new, t2, eorb, nocc, nvirt, nactive)
            integer, intent(in) :: nocc, nvirt, nactive
            double precision, dimension(nocc+1:nactive,nocc + 1:nactive,nocc,nocc) , intent(inout) :: t2new
            double precision, dimension(nocc+1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
            double precision, dimension(:), intent(in) :: eorb

            integer :: a, b, c, d
            integer :: i, j, k, l
            integer :: ai, bj, ck
            double precision :: t
            integer :: npair

            npair = nocc * nvirt

            do ai = 1, npair
                  a = (ai - 1) / nocc + nocc+1
                  i = ai - nocc * (a - (nocc + 1))
                  bj_lp: do bj = 1, npair
                        if(bj == ai) cycle bj_lp
                        b = (bj - 1) / nocc + nocc+1
                        j = bj - nocc * (b - (nocc + 1))

                        t = calc_t3(t2, nocc, nactive,eorb,  a, a, b, i, i, j)
                        call t2_digest(t2new, nocc, nactive, t,  a, a, b, i, i, j)
                        call t2_digest(t2new, nocc, nactive, t,  a, b, a, i, j, i)
                        call t2_digest(t2new, nocc, nactive, t,  b, a, a, j, i, i)
                        
                        do d = nocc + 1, nactive
                              call t2_digest_d(t2new, nocc, nactive, t, a, a, b, d, i, i, j)
                              call t2_digest_d(t2new, nocc, nactive, t, a, b, a, d, i, j, i)
                              call t2_digest_d(t2new, nocc, nactive, t, b, a, a, d, j, i, i)
                        end do
                        
                        do l = 1, nocc
                              call t2_digest_l(t2new, nocc, nactive, t, a, a, b, i, i, j, l)
                              call t2_digest_l(t2new, nocc, nactive, t, a, b, a, i, j, i, l)
                              call t2_digest_l(t2new, nocc, nactive, t, b, a, a, j, i, i, l)
                        end do
                  end do bj_lp
            end do

            do ai = 1, npair
                  a = (ai - 1) / nocc + nocc+1
                  i = ai - nocc * (a - (nocc + 1))
                  do bj = ai+1, npair
                        b = (bj - 1) / nocc + nocc+1
                        j = bj - nocc * (b - (nocc + 1))
                        do ck = bj+1, npair
                              c = (ck - 1) / nocc + nocc+1
                              k = ck - nocc * (c - (nocc + 1))
                              
                              t = calc_t3(t2, nocc, nactive,eorb,  a, b, c, i, j, k)
                              
                              call t2_digest(t2new, nocc, nactive, t,  a, b, c, i, j, k)
                              call t2_digest(t2new, nocc, nactive, t,  a, c, b, i, k, j)
                              call t2_digest(t2new, nocc, nactive, t,  b, a, c, j, i, k)
                              call t2_digest(t2new, nocc, nactive, t,  b, c, a, j, k, i)
                              call t2_digest(t2new, nocc, nactive, t,  c, a, b, k, i, j)
                              call t2_digest(t2new, nocc, nactive, t,  c, b, a, k, j, i)
                              
                              
                              do d = nocc + 1, nactive
                                    call t2_digest_d(t2new, nocc, nactive, t, a, b, c, d, i, j, k)
                                    call t2_digest_d(t2new, nocc, nactive, t, a, c, b, d, i, k, j)
                                    call t2_digest_d(t2new, nocc, nactive, t, b, a, c, d, j, i, k)
                                    call t2_digest_d(t2new, nocc, nactive, t, b, c, a, d, j, k, i)
                                    call t2_digest_d(t2new, nocc, nactive, t, c, a, b, d, k, i, j)
                                    call t2_digest_d(t2new, nocc, nactive, t, c, b, a, d, k, j, i)
                              end do
                              
                              do l = 1, nocc
                                    call t2_digest_l(t2new, nocc, nactive, t, a, b, c, i, j, k, l)
                                    call t2_digest_l(t2new, nocc, nactive, t, a, c, b, i, k, j, l)
                                    call t2_digest_l(t2new, nocc, nactive, t, b, a, c, j, i, k, l)
                                    call t2_digest_l(t2new, nocc, nactive, t, b, c, a, j, k, i, l)
                                    call t2_digest_l(t2new, nocc, nactive, t, c, a, b, k, i, j, l)
                                    call t2_digest_l(t2new, nocc, nactive, t, c, b, a, k, j, i, l)
                              end do

                        end do
                  end do
            end do
            
            
      end subroutine tctd_fast
      
      subroutine t2_digest(t2, nocc, nactive, t, a, b, c, i, j, k)
            integer, intent(in) :: nocc, nactive
            double precision, intent(in) :: t
            double precision, dimension(nocc+1:nactive,nocc + 1:nactive,nocc,nocc) , intent(inout) :: t2
            integer, intent(in) :: a, b, c,  i, j, k
            double precision :: t_int
            
            t_int = t * in_fov(k,c, nocc)
            call t2_update(t2, nocc, nactive, t_int, a, b, i, j)

            t_int = -t * in_fov(i,c, nocc)
            call t2_update(t2, nocc, nactive, t_int, a, b, k, j)
            
      end subroutine t2_digest

      subroutine t2_digest_d(t2, nocc, nactive, t, a, b, c,d, i, j, k)
            integer, intent(in) :: nocc, nactive
            double precision, intent(in) :: t
            double precision, dimension(nocc+1:nactive,nocc + 1:nactive,nocc,nocc) , intent(inout) :: t2
            integer, intent(in) :: a, b, c,d,  i, j, k
            double precision :: t_int
            
            t_int = t * tvvov(d,b,k,c)
            call t2_update(t2, nocc, nactive, 2.d+0 * t_int, d, a, j, i)

            t_int = -t * tvvov(d,b,i,c)
            call t2_update(t2, nocc, nactive, t_int, d, a, j, k)

            t_int = -t * tvvov(d,b,j,c)
            call t2_update(t2, nocc, nactive, t_int, d, a, k, i)

      end subroutine t2_digest_d

      subroutine t2_digest_l(t2, nocc, nactive, t, a, b, c, i, j, k, l)
            integer, intent(in) :: nocc, nactive
            double precision, intent(in) :: t
            double precision, dimension(nocc+1:nactive,nocc + 1:nactive,nocc,nocc) , intent(inout) :: t2
            integer, intent(in) :: a, b, c,  i, j, k, l
            double precision :: t_int
            
            t_int = -t * tovoo(k,c,j,l)
            call t2_update(t2, nocc, nactive, 2.d+0 * t_int, b, a, l, i)

            t_int = t * tovoo(i,c,j,l)
            call t2_update(t2, nocc, nactive, t_int, b, a, l, k)

            t_int = t * tovoo(j,c,k,l)
            call t2_update(t2, nocc, nactive, t_int, b, a, l, i)

      end subroutine t2_digest_l
      
      subroutine t2_update(t2, nocc, nactive, val, a, b, i, j)
            integer, intent(in) :: nocc, nactive
            double precision, intent(in) :: val
            double precision, dimension(nocc+1:nactive,nocc + 1:nactive,nocc,nocc) , intent(inout) :: t2
            integer, intent(in) :: a, b, i, j
            
            t2(a,b,i,j) = t2(a,b,i,j) + val
            t2(b,a,j,i) = t2(b,a,j,i) + val
            
      end subroutine t2_update

      function int_101_parallel(nocc, nactive, a, b, i, j)
            integer, intent(in) :: nocc, nactive
            double precision :: int_101_parallel
            integer, intent(in) :: a, b
            integer, intent(in) :: i , j
            integer :: c, k, d, l
            integer :: w

            int_101_parallel = 0.d+0

            w = 1
           do k = 1, nocc
                  do c = nocc + 1, nactive
                        t3_temp(w) =  (t3(nocc, nactive, a, b, c, i, j, k) &
                              - t3(nocc, nactive,a, b, c, k, j, i)) * in_fov(k, c, nocc)
                        w = w + 1
                  end do
            end do
            do w = 1, (nactive-nocc)*nocc
               int_101_parallel = int_101_parallel + t3_temp(w)
            end do
            
            w = 1
            do k = 1, nocc
                  do d = nocc  +1, nactive
                        do c = nocc + 1, nactive
                           t3_temp(w) =  (2.d+0 * t3(nocc, nactive,  b, c, d, j, i, k)&
                                    - t3(nocc, nactive, b, c, d, k, i, j) - &
                                    t3(nocc, nactive,  b, c, d, j, k, i))*&
                                    tvvov(a, c, k, d)
                           w = w + 1
                        end do
                  end do
            end do

            do w = 1, (nactive-nocc)**2*nocc
               int_101_parallel = int_101_parallel + t3_temp(w)
            end do
            
            w = 1
            do l = 1, nocc
                  do k = 1, nocc
                        do c = nocc + 1, nactive
                           t3_temp(w) = (2.d+0 * t3(nocc, nactive, b, a, c, j, k, l)&
                                    - t3(nocc, nactive, b, a, c, l, k, j) - &
                                    t3(nocc, nactive, b, a, c, j, l, k))*&
                                    tovoo(l, c, k, i)
                           w = w + 1
                        end do
                  end do
            end do

            do w = 1, (nactive-nocc)**2*nocc
               int_101_parallel = int_101_parallel + t3_temp(w)
            end do


          end function int_101_parallel

          function int_101(nocc, nactive, a, b, i, j)
            integer, intent(in) :: nocc, nactive
            double precision :: int_101
            integer, intent(in) :: a, b
            integer, intent(in) :: i , j
            integer :: c, k, d, l
            integer :: p1, p2

            int_101 = 0.d+0

            do k = 1, nocc
                  do c = nocc + 1, nactive
                        int_101 = int_101 + (t3(nocc, nactive, a, b, c, i, j, k) &
                              - t3(nocc, nactive,a, b, c, k, j, i)) * in_fov(k, c, nocc)
                  end do
            end do

            do k = 1, nocc
                  do d = nocc  +1, nactive
                        do c = nocc + 1, nactive
                              int_101 = int_101 + (2.d+0 * t3(nocc, nactive,  b, c, d, j, i, k)&
                                    - t3(nocc, nactive, b, c, d, k, i, j) - &
                                    t3(nocc, nactive,  b, c, d, j, k, i))*&
                                    tvvov(a, c, k, d)
                        end do
                  end do
            end do
            
            ! p1 = 0
            ! p2 = 0
            do l = 1, nocc
                  do k = 1, nocc
                        do c = nocc + 1, nactive
                              int_101 = int_101 - (2.d+0 * t3(nocc, nactive, b, a, c, j, k, l)&
                                    - t3(nocc, nactive, b, a, c, l, k, j) - &
                                    t3(nocc, nactive, b, a, c, j, l, k))*&
                                    tovoo(l, c, k, i) 
                              ! if (abs((2.d+0 * t3(nocc, nactive, b, a, c, j, k, l)&
                              !       - t3(nocc, nactive, b, a, c, l, k, j) - &
                              !       t3(nocc, nactive, b, a, c, j, l, k))*&
                              !       tovoo(l, c, k, i)).gt.1.d-9)then
                              !       p1 = p1 + 1
                              ! else
                              !       p2 = p2 + 1
                              ! end if

                        end do
                  end do
            end do
            ! print*, 'p1, p2', p1, p2



      end function int_101

      function int_101_symm(nocc, nactive, a, b, i, j)
            integer, intent(in) :: nocc, nactive
            double precision :: int_101_symm
            integer, intent(in) :: a, b
            integer, intent(in) :: i , j
            integer :: c, k, d, l

            int_101_symm = 0.d+0

            do k = 1, nocc
                  do c = nocc + 1, nactive
                        int_101_symm = int_101_symm + (t3(nocc, nactive, a, b, c, i, j, k) &
                              - t3(nocc, nactive,a, b, c, k, j, i)) * in_fov(k, c, nocc)
                  end do
            end do

            do k = 1, nocc
                  do d = nocc  +1, nactive
                        do c = nocc + 1, nactive
                              int_101_symm = int_101_symm + (2.d+0 * t3(nocc, nactive,  b, c, d, j, i, k)&
                                    - t3(nocc, nactive, b, c, d, k, i, j) - &
                                    t3(nocc, nactive,  b, c, d, j, k, i))*&
                                    tvvov(a, c, k, d)
                        end do
                  end do
            end do

            do l = 1, nocc
                  do k = 1, nocc
                        do c = nocc + 1, nactive
                              int_101_symm = int_101_symm - (2.d+0 * t3(nocc, nactive, b, a, c, j, k, l)&
                                    - t3(nocc, nactive, b, a, c, l, k, j) - &
                                    t3(nocc, nactive, b, a, c, j, l, k))*&
                                    tovoo(l, c, k, i)
                        end do
                  end do
            end do

      end function int_101_symm


      ! function t3(t2, t1, nocc, nvirt, nactive,eorb,  a, b, c, i, j, k)
      !       integer, intent(in) :: nocc, nvirt, nactive
      !       double precision, dimension(nocc + 1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
      !       double precision, dimension(nocc + 1:nactive,nocc) , intent(in) :: t1
      !       double precision :: t3
      !       double precision, dimension(:), intent(in) :: eorb
      !       double precision ::  time0, time1
      !       integer, intent(in) :: a, b, c
      !       integer, intent(in) :: i, j, k 
      !       call cpu_time(time0)
      !       t3 = -gamma_cc3(t2, t1,nocc, nvirt, nactive, a, b, c, i, j, k) /&
      !             en_denomi(nocc, nvirt, nactive, eorb,  a, b, c, i, j, k)

      !       call cpu_time(time1)
      !       if(abs(time1-time0).gt.1.d-6)then
      !             print*, abs(time1-time0)
      !       end if
      ! end function t3

      subroutine t3_init(nc0, nv0, nocc, nactive)
        
        integer, intent(in) :: nc0, nv0
        integer, intent(in) :: nocc,nactive
        integer :: npair, i
        
        npair = nocc * (nactive - nocc)
        qnocc0 = nc0
        qnvirt0 = nv0

        allocate(t3m((npair**3+3*npair**2+2*npair)/6))
        print*, 'ALLOCATE t3m', (npair**3+3*npair**2+2*npair)/6

        qbj  = 3 + 6 * npair
        qbj2 = -3
        qck  = 2 + 3 * npair * (2 + npair)
        qck2 = -3 * (1 + npair)
        q00  = -3 * npair * (3 + npair)
        
      end subroutine t3_init

      subroutine fill_t3(t2, eorb, nocc, nactive)
        double precision, dimension(:,:,:,:), intent(in)        :: t2
        double precision, dimension(:), intent(in)              :: eorb
        integer, intent(in)                                     :: nocc
        integer, intent(in)                                     :: nactive
        
        integer :: a, b, c, i, j, k
        integer :: ai, bj, ck
        integer :: npair

        npair = nocc * (nactive - nocc)

        t3m = 0.d+0

        do ck = 1, npair
           c = (ck - 1) / nocc + nocc+1
           k = ck - nocc * (c - (nocc + 1))
           do bj = ck, npair

              b = (bj - 1) / nocc + nocc+1
              j = bj - nocc * (b - (nocc + 1))
              do ai = bj, npair

                 a = (ai - 1) / nocc + nocc+1
                 i = ai - nocc * (a - (nocc + 1))
                 t3m(fmu3(ai, bj, ck)) = calc_t3(t2, nocc, nactive, eorb,  a, b, c, i, j, k)
              end do
           end do
        end do

      end subroutine fill_t3

      subroutine fill_t3_sym(t2, gitriples, gidimt, girrep0, girrep1, &
            eorb, nocc, nvirt, nactive)
            double precision, dimension(:,:,:,:), intent(in)        :: t2
            integer, dimension(:,:), intent(in)                     :: gitriples
            integer, intent(in)                                     :: gidimt
            integer, dimension(:,:), intent(in)                     :: girrep0
            integer, dimension(:,:), intent(in)                     :: girrep1
            double precision, dimension(:), intent(in)              :: eorb
            integer, intent(in)                                     :: nocc
            integer, intent(in)                                     :: nvirt
            integer, intent(in)                                     :: nactive
            
            
            integer, dimension(:,:), allocatable                    :: itriples
            integer :: a, b, c, i, j, k
            integer :: m0i, m1i, m0j, m1j, m0k, m1k
            integer :: m0a, m1a, m0b, m1b, m0c, m1c
            integer :: pa, pb, pc, pi, pj, pk
            integer :: kj
            integer :: tl
            integer :: ai, bj, ck
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks
            integer :: pai, pbj, pck
            

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            print*, nocc, nvirt
            allocate(itriples(max_ntasks, 6))

            t3m = 0.d+0

            ntasks = 0
            do kj = 1, gidimt
                  call loop_boundaries_sp(gitriples(1:2, kj), girrep0, girrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(gitriples(3:4, kj), girrep0, girrep1, &
                        m0j, m1j, m0b, m1b)
                  call loop_boundaries_sp(gitriples(5:6, kj), girrep0, girrep1, &
                        m0k, m1k, m0c, m1c)

                  do pc = m0c, m1c
                  do pk = m0k, m1k
                  do pb = maxval([m0b, pc]), m1b
                  do pj = m0j, m1j
                  do pa = maxval([m0a, pb]),  m1a
                  do pi = m0i, m1i

                        pai = (pa - nocc - 1) * nocc + pi
                        pbj = (pb - nocc - 1) * nocc + pj
                        pck = (pc - nocc - 1) * nocc + pk
                        
                        if(pai.ge.pbj.and.pbj.ge.pck)then

                              ntasks = ntasks + 1
                              itriples(ntasks, 1) = pa
                              itriples(ntasks, 2) = pi
                              itriples(ntasks, 3) = pb
                              itriples(ntasks, 4) = pj
                              itriples(ntasks, 5) = pc
                              itriples(ntasks, 6) = pk
                        end if

                        if (ntasks == max_ntasks)  then
                              !$omp parallel private(tl, a, i, b, j, c, k, ai, bj, ck) default(shared)                                  
                              !$omp do schedule(guided)
                              do tl = 1, ntasks
                                    a = itriples(tl, 1)
                                    i = itriples(tl, 2)
                                    b = itriples(tl, 3)
                                    j = itriples(tl, 4)
                                    c = itriples(tl, 5)
                                    k = itriples(tl, 6)
                                    
                                    ai = (a - nocc - 1) * nocc + i
                                    bj = (b - nocc - 1) * nocc + j
                                    ck = (c - nocc - 1) * nocc + k
                                    
                                    t3m(fmu3(ai, bj, ck)) = calc_t3(t2, nocc, nactive, eorb,  a, b, c, i, j, k)
                                    ! if (abs(t3m(fmu3(ai, bj, ck))).gt.1.d-10)then
                                    !       print*, a,b,c,i,j,k,t3m(fmu3(ai, bj, ck))
                                    ! end if
                                    
                                    
                              end do
                              !$omp end do                                                                                                                    
                              !$omp end parallel
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
                  !$omp parallel private(tl, a, i, b, j, c, k, ai, bj, ck) default(shared)
                  !$omp do schedule(guided)
                  do tl = 1, ntasks
                        a = itriples(tl, 1)
                        i = itriples(tl, 2)
                        b = itriples(tl, 3)
                        j = itriples(tl, 4)
                        c = itriples(tl, 5)
                        k = itriples(tl, 6)

                        ai = (a - nocc - 1) * nocc + i
                        bj = (b - nocc - 1) * nocc + j
                        ck = (c - nocc - 1) * nocc + k
                        
                        t3m(fmu3(ai, bj, ck)) = calc_t3(t2, nocc, nactive,eorb,  a, b, c, i, j, k)
                        ! if (abs(t3m(fmu3(ai, bj, ck))).gt.1.d-10)then
                        !       print*, t3m(fmu3(ai, bj, ck))
                        ! end if
                        

                  end do
                  !$omp end do                                                                                                                    
                  !$omp end parallel
                  ntasks = 0
            end if

            deallocate(itriples)

      end subroutine fill_t3_sym

      function read_t3z(nocc, nactive, a, b, c, i, j, k)
            double precision :: read_t3z
            integer, intent(in) :: nocc, nactive
            integer, intent(in) :: a, b, c, i, j, k
            integer :: ai, bj, ck
            integer :: npair

            npair = nocc * (nactive - nocc)

            ai = (a - qnvirt0) * nocc + (i - qnocc0) + 1
            bj = (b - qnvirt0) * nocc + (j - qnocc0) + 1
            ck = (c - qnvirt0) * nocc + (k - qnocc0) + 1

            read_t3z = t3m(fmu3(ai, bj, ck))

      end function read_t3z


      function read_t3(nocc, nactive, a, b, c, i, j, k)
        double precision :: read_t3
        integer, intent(in) :: nocc, nactive
        integer, intent(in) :: a, b, c, i, j, k
        integer :: ai, bj, ck
        integer :: npair

        npair = nocc * (nactive - nocc)

        ai = (a - qnvirt0) * nocc + (i - qnocc0) + 1
        bj = (b - qnvirt0) * nocc + (j - qnocc0) + 1
        ck = (c - qnvirt0) * nocc + (k - qnocc0) + 1
        
        call small_sort(ai, bj, ck)
        read_t3 = t3m(fmu3(ai, bj, ck))
        
      contains
        !
        ! Locally visible functions
        !
      subroutine small_sort(ai, bj, ck)
            integer, intent(inout) :: ai, bj, ck
            integer :: temp

            if(ai.lt.bj)then
                  temp = bj
                  bj = ai
                  ai = temp
            end if
            if(bj.lt.ck)then
                  temp = ck
                  ck = bj
                  bj = temp
            end if
            if(ai.lt.bj)then
                  temp = bj
                  bj= ai
                  ai= temp
            end if
      end subroutine small_sort

      end function read_t3

      function calc_t3(t2, nocc, nactive, eorb,  a, b, c, i, j, k)
            double precision :: calc_t3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(:), intent(in) :: eorb

            integer, intent(in) :: a, i, b, j, c, k 
            integer :: s ,d,l 
            double precision, dimension(0:11) :: term 

            term = 0.d+0
            term = 0.d+0

            do d = nocc + 1, nactive
                  term(0) = term(0) + t2(b,d,j,k) * tvvvo(c,d,a,i)
                  term(1) = term(1) + t2(c,d,k,j) * tvvvo(b,d,a,i)
                  term(2) = term(2) + t2(a,d,i,k) * tvvvo(c,d,b,j)
                  term(3) = term(3) + t2(a,d,i,j) * tvvvo(b,d,c,k)
                  term(4) = term(4) + t2(c,d,k,i) * tvvvo(a,d,b,j)
                  term(5) = term(5) + t2(b,d,j,i) * tvvvo(a,d,c,k)
            end do

            do l = 1, nocc
                  term(6) = term(6) - t2(b,c,j,l) * tvooo(a,i,l,k)
                  term(7) = term(7) - t2(b,c,l,k) * tvooo(a,i,l,j)
                  term(8) = term(8) - t2(a,c,i,l) * tvooo(b,j,l,k)
                  term(9) = term(9) - t2(a,b,i,l) * tvooo(c,k,l,j)
                  term(10) = term(10) - t2(a,c,l,k) * tvooo(b,j,l,i)
                  term(11) = term(11) - t2(a,b,l,j) * tvooo(c,k,l,i)
            end do

            calc_t3 = 0.d+0
            do s = 0, 11
                  calc_t3 = calc_t3 + term(s)
            end do

            calc_t3 = -calc_t3 / (eorb(a) + eorb(b) + eorb(c) - eorb(i) - eorb(j) - eorb(k))

          end function calc_t3

      function t3(nocc, nactive, a, b, c, i, j, k)
            double precision :: t3
            integer, intent(in) :: nocc, nactive
            integer, intent(in) :: a, i, b, j, c, k 
            
            t3 = read_t3(nocc, nactive, a, b, c, i, j, k)

      end function t3

      function t3z(nocc, nactive, a, b, c, i, j, k)
            double precision :: t3z
            integer, intent(in) :: nocc, nactive
            integer, intent(in) :: a, i, b, j, c, k 

                t3z = read_t3z(nocc, nactive, a, b, c, i, j, k)

          end function t3z


      function fmu3(ai, bj, ck)
        !                                                                                                                           
        ! Compute compound three-electron index                                                                                     
        ! (assumed that ai >= bj >= ck)                                                                                             
        !                                                                                                                           
        integer :: fmu3
        integer, intent(in) :: ai, bj, ck
        integer :: mu31, mu32
        !                                                                                                                           
        ! Compound index is compouted relative                                                                                      
        ! to the first element of matrix block.                                                                                     
        ! Braoffset/ketoffset should be added                                                                                       
        ! to get the absolute position                                                                                              
        !                                                                                                                           
        mu31 = (qbj + qbj2 * bj) * bj
        mu32 = (qck + (qck2 + ck) * ck) * ck
        fmu3  = ai + (mu31 + mu32 + q00) / 6
      end function fmu3
      
      function calc_w1(a, i, j, k, nocc, nactive)
            double precision :: calc_w1
            integer, intent(in) :: a, i, j, k
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            
            integer :: d, e, l

            calc_w1 = 0.d+0
            do d = nocc + 1, nactive
                  do e = nocc + 1, nactive
                        do l = 1, nocc
                              calc_w1 = calc_w1 + (tovov(k, e, l, d) - 0.5d+0*tovov(k, d, l, e))&
                                 * t3(nocc, nactive,  a, d, e, i, l, j)&
                                 - 0.5d+0 * tovov(k, e, l, d)&
                                 * t3(nocc, nactive,  a, d, e, l, i, j)
                        end do
                  end do
            end do

            calc_w1 = -1.d+0 * calc_w1
            
      end function calc_w1

      function calc_w2(a, b, c, i, nocc, nactive)
            double precision :: calc_w2
            integer, intent(in) :: a, b, c, i
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            
            integer :: d, l, m

            calc_w2 = 0.d+0
            do d = nocc + 1, nactive
                  do l = 1, nocc
                        do m = 1, nocc
                           calc_w2 = calc_w2 + (tovov(l, d, m, c) - 0.5d+0*tovov(l, c, m, d))&
                                 * t3(nocc, nactive,  a, b, d, m, i, l)&
                                 -0.5d+0 * tovov(l, d, m, c)&
                                 * t3(nocc, nactive,  a, b, d, m, l, i)
                        end do
                  end do
            end do

            calc_w2 = -1.d+0 * calc_w2
            
      end function calc_w2

      ! function gamma_cc3(t2, t1,nocc, nvirt, nactive, a, b, c, i, j, k)
      !       integer, intent(in) :: nocc, nvirt, nactive
      !       double precision, dimension(nocc + 1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
      !       double precision, dimension(nocc + 1:nactive,nocc) , intent(in) :: t1
      !       double precision :: gamma_cc3
      !       integer, intent(in) :: a, b, c
      !       integer, intent(in) :: i, j, k

      !       gamma_cc3 = int_98(t2, nocc, nactive, a, b, c, i, j, k) &
      !             + int_98(t2, nocc, nactive, a, c, b, i, k, j) &
      !             + int_98(t2, nocc, nactive, b, a, c, j, i, k) &
      !             + int_98(t2, nocc, nactive, c, a, b, k, i, j) &
      !             + int_98(t2, nocc, nactive, b, c, a, j, k, i) &
      !             + int_98(t2, nocc, nactive, c, b, a, k, j, i) 

      !       ! gamma_cc3 = int_98(t2,t1, nocc, nvirt, nactive, a, b, c, i, j, k) &
      !       !       + int_98(t2,t1, nocc, nvirt, nactive, a, c, b, i, k, j) &
      !       !       + int_98(t2,t1, nocc, nvirt, nactive, b, a, c, j, i, k) &
      !       !       + int_98(t2,t1, nocc, nvirt, nactive, c, a, b, k, i, j) &
      !       !       + int_98(t2,t1, nocc, nvirt, nactive, b, c, a, j, k, i) &
      !       !       + int_98(t2,t1, nocc, nvirt, nactive, c, b, a, k, j, i) 
      ! end function gamma_cc3

      function int_98(t2, nocc, nactive, a, b, c, i, j, k)
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc + 1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
            double precision :: int_98
            integer, intent(in) :: a, b, c
            integer, intent(in) :: i, j, k

            integer :: d, l

            int_98 = 0.d+0

            do d = nocc + 1, nactive

                  int_98 = int_98 + t2(a, d, i, j) * tvvvo(b,d,c,k) !tvovv(c, k, b, d)

                  ! !                  test = alternative_t1_1(t1, nocc, nactive, c,k,b,d)
                  !                   test2 = int_98_a(t2, t1,nocc, nvirt, nactive, a, b, c, d, i, j, k)
                  !                   if(abs(test2-tvvvo(b, d,c,k)).gt.1.d-10)then !tvovv(c, k, b, d)
                  !                         print*, ''
                  !                         print*,'-------------'
                  !                         print*, 'a','b','c','d','i','j','k'
                  !                         print*, a,b,c,d,i,j,k
                  !                         print*, test2
                  !                         print*, tvvvo(b, d,c,k)!tvovv(c, k, b, d)
                  !                         print*,'-------------'
                  !                         print*, ''
                  !                         stop
                  !                   end if
            end do
            do l = 1, nocc
                  int_98 = int_98 - t2(a, b, i, l) * tvooo(c, k, l, j)

                  ! test = alternative_t1_2(t1, nocc, nactive, c,k,l,j)
                  ! test2 = int_98_b(t2, t1, nocc, nvirt, nactive, a, b, c, i, j, k, l)
                  ! if(abs(test-test2).gt.1.d-10)then
                  !       print*, ''
                  !       print*,'-------------'
                  !       print*, 'a','b','c','i','j','k','l'
                  !       print*, a,b,c,i,j,k,l
                  !       print*, test
                  !       print*, test2
                  !       print*, tvooo(c, k, l, j)
                  !       print*,'-------------'
                  !       print*, ''
                  ! end if
            end do

      end function int_98

      !       function int_98(t2,t1, nocc, nvirt, nactive, a, b, c, i, j, k)
      !             integer, intent(in) :: nocc, nvirt, nactive
      !             double precision, dimension(nocc + 1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
      !             double precision, dimension(nocc + 1:nactive,nocc) , intent(in) :: t1
      !             double precision :: int_98
      !             integer, intent(in) :: a, b, c
      !             integer, intent(in) :: i, j, k
      !             integer :: d,e,f,l,m,n
      !             double precision :: tm1, tm2

      !             int_98 = 0.d+0

      !             call cpu_time(tm1)
      !             do d = nocc + 1, nactive
      !                   int_98 = int_98 + t2(a, d, i, j) * int_98_a(t2, t1, nocc, nvirt, nactive, a, b, c,d, i, j, k)
      !                   !alternative_t1_1(t1, nocc, nactive, c,k,b,d)
      !                   !int_98_a(t2, t1, nocc, nvirt, nactive, a, b, c,d, i, j, k)
      !             end do
      !             call cpu_time(tm2)
      ! !            print*, 'czas',tm2-tm1
      !             do l = 1, nocc
      !                   int_98 = int_98 - t2(a, b, i, l) * int_98_b(t2, t1, nocc, nvirt, nactive, a, b, c, i, j, k,l)
      !                   !alternative_t1_2(t1, nocc, nactive, c,k,l,j)
      !                   !int_98_b(t2, t1, nocc, nvirt, nactive, a, b, c, i, j, k,l)
      !             end do

      !       end function int_98

      ! function int_98_a(t2, t1,nocc, nvirt, nactive, a, b, c, d, i, j, k)
      !       integer, intent(in) :: nocc, nvirt, nactive
      !       double precision, dimension(nocc + 1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
      !       double precision, dimension(nocc + 1:nactive,nocc) , intent(in) :: t1
      !       double precision :: int_98_a
      !       integer, intent(in) :: a, b, c, d
      !       integer, intent(in) :: i, j, k
      !       integer :: e,f,l,m,n
      !       double precision :: s
      !       int_98_a = 0.d+0

      !       s = vvvo(b,d,c,k)

      !       do l = 1, nocc
      !             do m = 1, nocc
      !                   do e = nocc+1, nactive
      !                         s = s + t1(c, l)*t1(e,k)*t1(b,m)*vovo(e,l,d,m)
      !                   end do
      !             end do
      !       end do

      !       do l = 1, nocc
      !             do e = nocc+1, nactive
      !                   s = s - t1(e, k)*t1(b,l)*vvvo(c,e,d,l)
      !             end do
      !       end do

      !       do l = 1, nocc
      !             do m = 1, nocc
      !                   s = s + t1(c,l)*t1(b,m)*vooo(d,m,l,k)
      !             end do
      !       end do

      !       do l = 1, nocc
      !             do e = nocc+1, nactive
      !                   s = s - t1(c,l)*t1(e,k)*vvvo(b,d,e,l)
      !             end do
      !       end do

      !       do l = 1, nocc
      !             s = s - t1(b,l)*vovo(c,k,d,l)
      !       end do

      !       do e = nocc+1, nactive
      !             s = s + t1(e,k)*vvvv(c,e,b,d)
      !       end do

      !       do l = 1, nocc
      !             s = s - t1(c,l)*vvoo(b,d,l,k)
      !       end do

      !       int_98_a = s
      ! end function int_98_a

      ! function int_98_b(t2, t1, nocc, nactive, c, j, k, l)
      !   integer, intent(in) :: nocc, nactive
      !       double precision, dimension(nocc + 1:nactive,nocc + 1:nactive,nocc,nocc) , intent(in) :: t2
      !       double precision, dimension(nocc + 1:nactive,nocc) , intent(in) :: t1
      !       double precision :: int_98_b
      !       integer, intent(in) :: c
      !       integer, intent(in) :: j, k, l
      !       integer :: d,e,m
      !       double precision :: s
      !       int_98_b = 0.d+0

      !       s = vooo(c,k,l,j)

      !       do m = 1, nocc
      !             do e = nocc+1, nactive
      !                   do d = nocc+1, nactive
      !                         s = s - t1(c, m)*t1(d,k)*t1(e,j)*vovo(e,l,d,m)
      !                   end do
      !             end do
      !       end do

      !       do d = nocc+1, nactive
      !             do e = nocc+1, nactive
      !                   s = s + t1(d, k)*t1(e,j)*vvvo(c,d,e,l)
      !             end do
      !       end do

      !       do m = 1, nocc
      !             do d = nocc+1, nactive
      !                   s = s - t1(c,m)*t1(d,j)*vooo(d,l,m,k)
      !             end do
      !       end do

      !       do m = 1, nocc
      !             do d = nocc+1, nactive
      !                   s = s - t1(c,m)*t1(d,k)*vooo(d,m,l,j)
      !             end do
      !       end do

      !       do d = nocc+1, nactive
      !             s = s + t1(d,j)*vovo(c,k,d,l)
      !       end do

      !       do d = nocc+1, nactive
      !             s = s + t1(d,k)*vvoo(c,d,l,j)
      !       end do

      !       do m = 1, nocc
      !             s = s - t1(c,m)*oooo(m,k,l,j)
      !       end do

      !       int_98_b = s
      ! end function int_98_b


      function en_denomi(eorb, a, b, c, i, j, k)
            double precision :: en_denomi
            double precision, dimension(:), intent(in) :: eorb
            integer, intent(in) :: a, b, c
            integer, intent(in) :: i, j, k

            en_denomi = eorb(a) +  eorb(b) + eorb(c) - eorb(i) - eorb(j) - eorb(k)

      end function en_denomi

      ! Helgaker part 1 eq. 10.8.28 str 511 evince
      function in_fvv(b,c, nocc)
            double precision :: in_fvv
            integer, intent(in) :: b,c
            integer, intent(in) :: nocc
            integer :: l

            in_fvv = 0.d+0
            do l = 1, nocc
                  in_fvv = in_fvv + tloovv(b,c,l,l)
            end do
            in_fvv = in_fvv + tvv(b,c)

      end function in_fvv

      function in_fvo(a,i, nocc)
            double precision :: in_fvo
            integer, intent(in) :: a, i
            integer, intent(in) :: nocc
            integer :: l

            in_fvo = 0.d+0
            do l = 1, nocc
                  in_fvo = in_fvo + tlvooo(a,i,l,l)
            end do
            in_fvo = in_fvo + tvo(a,i)

      end function in_fvo

      function in_fov(k,c, nocc)
            double precision :: in_fov
            integer, intent(in) :: k,c
            integer, intent(in) :: nocc
            integer :: l

            in_fov = 0.d+0
            do l = 1, nocc
                  in_fov = in_fov + tlooov(k,c,l,l)
            end do
            in_fov = in_fov + tov(k,c)

      end function in_fov

      function in_foo(k,j, nocc)
            double precision :: in_foo
            integer, intent(in) :: k, j
            integer, intent(in) :: nocc
            integer :: l

            in_foo = 0.d+0

            do l = 1, nocc
                  in_foo = in_foo + tloooo(l,l,k,j)
            end do
            in_foo = in_foo + too(k,j)

      end function in_foo

      function tlvoov(a,i,k,c)
            double precision :: tlvoov
            integer, intent(in) :: a,i,k,c

            tlvoov = 2.d+0 * tvoov(a,i,k,c) - tvvoo(a,c,k,i)
      end function tlvoov

      function tlovov(k, d, l, c)
            double precision :: tlovov
            integer, intent(in) :: k, d, l, c

            tlovov = 2.d+0 * tovov(k, d, l, c) - tovov(k,c, l,d)
      end function tlovov

      function tlooov(k,c,l,j)
            double precision :: tlooov
            integer, intent(in) :: k,c, l,j

            tlooov = 2.d+0 * tovoo(k,c,l,j) - tovoo(l,c,k,j)

      end function tlooov

      function tloovv(a,c,k,l)
            double precision :: tloovv
            integer, intent(in) :: a,c,k,l

            tloovv = 2.d+0 * tvvoo(a,c,k,l) - tvoov(a,l,k,c)

      end function tloovv

      function tlovvv(k,d,a,c)
            double precision :: tlovvv
            integer, intent(in) :: k, d, a, c

            tlovvv = 2.d+0 * tvvov(a, c, k, d) - tvvov(a, d, k, c)
      end function tlovvv

      function tlovoo(i,c,k, l)
            double precision :: tlovoo
            integer, intent(in) :: i,c,k, l

            tlovoo = 2.d+0 * tovoo(i,c,k,l) - tovoo(k,c,i,l)

      end function tlovoo

      function tlvooo(a,i, k, l)
            double precision :: tlvooo
            integer, intent(in) :: i,a,k, l

            tlvooo = 2.d+0 * tvooo(a,i,k,l) - tvooo(a,l,k,i)

      end function tlvooo

      function tloooo(k,l,j,i)
            double precision :: tloooo
            integer, intent(in) :: k,l,j,i

            tloooo = 2.d+0 * toooo(k,l,j,i) - toooo(k,i,j,l)
      end function tloooo

      function lovov(l,d,k,c)
            double precision :: lovov
            integer, intent(in) :: l,d,k,c

            lovov = 2.d+0 * vovo(d,l,c,k) - vovo(c,l,d,k)
      end function lovov

      ! function uu(t2, nocc, nvirt, nactive, a, b, i, j)
      !       integer, intent(in) :: nocc, nvirt, nactive
      !       double precision :: uu
      !       double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in)  :: t2
      !       integer, intent(in) :: a, b, i, j

      !       uu = 2.d+0 * t2(a, b, i, j) - t2(a, b, j, i)
      ! end function uu

      ! function equation_100(eorb, t2, t1, nocc, nactive, a, i) 
      !       double precision, dimension(:), intent(in) :: eorb
      !       double precision :: equation_100
      !       integer, intent(in) :: nocc, nactive
      !       double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
      !       double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
      !       integer, intent(in) :: a, i 
      !       integer :: s ,k,b,j,c 
      !       double precision, dimension(0:3) :: term 
      !       term = 0.d+0 
      !       term = 0.d+0
      !       do k = 1, nocc
      !             do b = nocc + 1, nactive
      !                   do j = 1, nocc
      !                         do c = nocc + 1, nactive
      !                               term(0) = term(0) + vovo(c,k,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
      !                               term(1) = term(1) + vovo(c,k,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
      !                               term(2) = term(2) + vovo(c,j,b,k) * t3(nocc, nactive, a,b,c,k,j,i)
      !                         end do
      !                   end do
      !             end do
      !       end do
      !       term(0) = term(0) * (-2.0d+0)
      !       term(1) = term(1) * 2.0d+0
      !       term(2) = term(2) * 1.0d+0

      !       do j = 1, nocc
      !             do b = nocc + 1, nactive
      !                   do k = 1, nocc
      !                         do c = nocc + 1, nactive
      !                               term(3) = term(3) + vovo(c,k,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(3) = term(3) * (-1.0d+0)

      !       equation_100 = 0.d+0 
      !       do s = 0, 3
      !             equation_100 = equation_100 + term(s)
      !       end do
      ! end function equation_100

      ! function equation_101(eorb, t2, t1, nocc, nactive, a, i, b, j)
      !       double precision, dimension(:), intent(in) :: eorb
      !       double precision :: equation_101
      !       integer, intent(in) :: nocc, nactive
      !       double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
      !       double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
      !       integer, intent(in) :: a, i, b, j 
      !       integer :: s ,l,k,c,d 
      !       double precision, dimension(0:34) :: term 
      !       term = 0.d+0 
      !       term = 0.d+0
      !       do l = 1, nocc
      !             do k = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         term(0) = term(0) + vooo(c,l,k,i) * t3(nocc, nactive,  a,b,c,k,j,l)
      !                         term(1) = term(1) + vooo(c,l,k,j) * t3(nocc, nactive,  a,b,c,i,k,l)
      !                         term(2) = term(2) + vooo(c,k,l,j) * t3(nocc, nactive,  a,b,c,i,k,l)
      !                         term(3) = term(3) + vooo(c,k,l,i) * t3(nocc, nactive,  a,b,c,k,j,l)
      !                   end do
      !             end do
      !       end do

      !       term(0) = term(0) * (-2.0d+0)
      !       term(1) = term(1) * (-2.0d+0)

      !       ! To ma byc wykasowane bo paldus nie wpisal tutaj mnozenia przez energie orbitalne i nie uwzglednil
      !       ! bazy kanonicznej
      !       ! do k = 1, nocc
      !       !       do c = nocc + 1, nactive
      !       !             term(4) = term(4) + t3(t2,t1, nocc, nvirt, nactive, eorb,  a,b,c,i,j,k)
      !       !             term(5) = term(5) + t3(t2,t1, nocc, nvirt, nactive, eorb,  a,b,c,i,k,j)
      !       !             term(6) = term(6) + t3(t2,t1, nocc, nvirt, nactive, eorb,  a,b,c,k,j,i)
      !       !       end do
      !       ! end do

      !       ! term(4) = term(4) * 2.0d+0
      !       ! term(5) = term(5) * (-1.0d+0)
      !       ! term(6) = term(6) * (-1.0d+0)

      !       do k = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do d = nocc + 1, nactive
      !                         term(7) = term(7) + vvvo(c,b,d,k) * t3(nocc, nactive,  a,c,d,k,j,i)
      !                         term(8) = term(8) + vvvo(c,b,d,k) * t3(nocc, nactive,  a,c,d,i,j,k)
      !                         term(9) = term(9) + vvvo(c,a,d,k) * t3(nocc, nactive,  b,c,d,k,i,j)
      !                         term(10) = term(10) + vvvo(c,a,d,k) * t3(nocc, nactive,  b,c,d,j,i,k)
      !                   end do
      !             end do
      !       end do

      !       term(7) = term(7) * (-1.0d+0)
      !       term(8) = term(8) * 2.0d+0
      !       term(9) = term(9) * (-1.0d+0)
      !       term(10) = term(10) * 2.0d+0

      !       do c = nocc + 1, nactive
      !             do k = 1, nocc
      !                   do d = nocc + 1, nactive
      !                         term(11) = term(11) + vvvo(c,a,d,k) * t3(nocc, nactive,  b,c,d,j,k,i)
      !                         term(12) = term(12) + vvvo(c,b,d,k) * t3(nocc, nactive,  a,c,d,i,k,j)
      !                   end do
      !             end do
      !       end do

      !       term(11) = term(11) * (-1.0d+0)
      !       term(12) = term(12) * (-1.0d+0)

      !       do k = 1, nocc
      !             do l = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         term(13) = term(13) + vooo(c,k,l,j) * t3(nocc, nactive,  a,b,c,k,l,i)
      !                         term(14) = term(14) + vooo(c,l,k,i) * t3(nocc, nactive,  a,b,c,k,l,j)
      !                   end do
      !             end do
      !       end do

      !       do k = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do l = 1, nocc
      !                         do d = nocc + 1, nactive
      !                               term(15) = term(15) + vovo(d,l,c,k) * &
      !                                     t1(d,j) * t3(nocc, nactive,  a,b,c,k,l,i)
      !                               term(16) = term(16) + vovo(d,l,c,k) * &
      !                                     t1(c,i) * t3(nocc, nactive,  a,b,d,k,l,j)
      !                               term(17) = term(17) + vovo(d,l,c,k) * &
      !                                     t1(d,l) * t3(nocc, nactive,  a,b,c,i,j,k)
      !                               term(18) = term(18) + vovo(d,l,c,k) * &
      !                                     t1(d,l) * t3(nocc, nactive,  a,b,c,i,k,j)
      !                               term(19) = term(19) + vovo(d,l,c,k) *&
      !                                     t1(d,l) * t3(nocc, nactive,  a,b,c,k,j,i)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(17) = term(17) * (4.d0)!3.33333333333d+0
      !       term(18) = term(18) * (-2.0d+0)
      !       term(19) = term(19) * (-2.0d+0)

      !       do k = 1, nocc
      !             do l = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         do d = nocc + 1, nactive
      !                               term(20) = term(20) + vovo(d,k,c,l) * &
      !                                     t1(d,l) * t3(nocc, nactive,  a,b,c,k,j,i)
      !                         end do
      !                   end do
      !             end do
      !       end do


      !       do l = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do k = 1, nocc
      !                         do d = nocc + 1, nactive
      !                               term(21) = term(21) + vovo(d,k,c,l) * &
      !                                     t1(d,l) * t3(nocc, nactive,  a,b,c,i,k,j)
      !                               term(22) = term(22) + vovo(d,k,c,l) * &
      !                                     t1(c,j) * t3(nocc, nactive,  a,b,d,i,k,l)
      !                               term(23) = term(23) + vovo(d,l,c,k) * &
      !                                     t1(b,k) * t3(nocc, nactive,  a,c,d,i,j,l)
      !                               term(24) = term(24) + vovo(d,l,c,k) * &
      !                                     t1(a,k) * t3(nocc, nactive,  b,c,d,l,i,j)
      !                               term(25) = term(25) + vovo(d,l,c,k) * &
      !                                     t1(a,k) * t3(nocc, nactive,  b,c,d,j,i,l)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(23) = term(23) * (-2.0d+0)
      !       term(25) = term(25) * (-2.0d+0)

      !       do l = 1, nocc
      !             do k = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         do d = nocc + 1, nactive
      !                               term(26) = term(26) + vovo(d,l,c,k) * &
      !                                     t1(c,j) * t3(nocc, nactive,  a,b,d,i,k,l)
      !                               term(27) = term(27) + vovo(d,l,c,k) * &
      !                                     t1(c,i) * t3(nocc, nactive,  a,b,d,k,j,l)
      !                               term(28) = term(28) + vovo(d,k,c,l) * &
      !                                     t1(c,i) * t3(nocc, nactive,  a,b,d,k,j,l)
      !                               term(29) = term(29) + vovo(d,l,c,k) * &
      !                                     t1(d,k) * t3(nocc, nactive,  a,b,c,i,j,l)
      !                               term(30) = term(30) + vovo(d,l,c,k) * &
      !                                     t1(c,k) * t3(nocc, nactive,  a,b,d,i,j,l)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(26) = term(26) * (-2.0d+0)
      !       term(27) = term(27) * (-2.0d+0)
      !       term(29) = term(29) * (-2.d+0)!(-1.33333333333d+0)
      !       term(30) = term(30) * (0.d+0)!0.666666666667d+0

      !       do k = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do d = nocc + 1, nactive
      !                         do l = 1, nocc
      !                               term(31) = term(31) + vovo(d,l,c,k) * &
      !                                     t1(c,l) * t3(nocc, nactive,  a,b,d,i,j,k)
      !                               term(32) = term(32) + vovo(d,l,c,k) * &
      !                                     t1(b,l) * t3(nocc, nactive,  a,c,d,k,i,j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(31) = term(31) * (0.d0)!(-0.666666666667d+0)

      !       do l = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do d = nocc + 1, nactive
      !                         do k = 1, nocc
      !                               term(33) = term(33) + vovo(d,k,c,l) * &
      !                                     t1(a,k) * t3(nocc, nactive,  b,c,d,j,i,l)
      !                               term(34) = term(34) + vovo(d,k,c,l) * &
      !                                     t1(b,k) * t3(nocc, nactive,  a,c,d,i,j,l)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       equation_101 = 0.d+0 
      !       do s = 0, 34
      !             equation_101 = equation_101 + term(s)
      !       end do

      ! end function equation_101

      ! function alternative_t1_1(t1, nocc, nactive, c,k,b,d)
      !       double precision :: alternative_t1_1
      !       integer, intent(in) :: nocc, nactive
      !       double precision, dimension(nocc+1:nactive, nocc), intent(in) :: t1
      !       integer, intent(in) :: c, k, b, d
      !       double precision, dimension(:,:), allocatable :: xmx
      !       double precision, dimension(:,:), allocatable :: ymx
      !       double precision, dimension(:,:), allocatable :: imx
      !       integer :: i, a

      !       integer :: t, u, v

      !       allocate(imx(nactive, nactive))
      !       allocate(xmx(nactive, nactive))
      !       allocate(ymx(nactive, nactive))
      !       call identity_mx(nactive, imx)

      !       xmx = imx
      !       ymx = imx

      !       do i = 1, nocc
      !             do a = nocc + 1, nactive
      !                   xmx(a, i) = xmx(a, i) - t1(a, i)
      !             end do
      !       end do

      !       do i = 1, nocc
      !             do a = nocc + 1, nactive
      !                   ymx(i, a) = ymx(i, a) + t1(a, i)
      !             end do
      !       end do

      !       alternative_t1_1 = vvvo(b,d,c,k)
      !       do t = 1, nocc
      !             do u = nocc+1, nactive
      !                   do v = 1, nocc
      !                         alternative_t1_1 = alternative_t1_1 +&
      !                               xmx(c,t)*ymx(k,u)*xmx(b,v)*ymx(d,d)*vovo(d,v,u,t)
      !                   end do
      !             end do
      !       end do

      !       do u = nocc+1, nactive
      !             do v = 1, nocc
      !                   alternative_t1_1 = alternative_t1_1 +&
      !                         xmx(c,c)*ymx(k,u)*xmx(b,v)*ymx(d,d)*vvvo(c,u,d,v)
      !             end do
      !       end do

      !       do t = 1, nocc
      !             do v = 1, nocc
      !                   alternative_t1_1 = alternative_t1_1 +&
      !                         xmx(c,t)*ymx(k,k)*xmx(b,v)*ymx(d,d)*vooo(d,v,t,k)
      !             end do
      !       end do

      !       do t = 1, nocc
      !             do u = nocc+1, nactive
      !                   alternative_t1_1 = alternative_t1_1 +&
      !                         xmx(c,t)*ymx(k,u)*xmx(b,b)*ymx(d,d)*vvvo(b,d,u,t)
      !             end do
      !       end do

      !       do v = 1, nocc
      !             alternative_t1_1 = alternative_t1_1 +&
      !                   xmx(c,c)*ymx(k,k)*xmx(b,v)*ymx(d,d)*vovo(c,k,d,v)
      !       end do

      !       do u = nocc+1, nactive
      !             alternative_t1_1 = alternative_t1_1 +&
      !                   xmx(c,c)*ymx(k,u)*xmx(b,b)*ymx(d,d)*vvvv(c,u,b,d)
      !       end do

      !       do t = 1, nocc
      !             alternative_t1_1 = alternative_t1_1 +&
      !                   xmx(c,t)*ymx(k,k)*xmx(b,b)*ymx(d,d)*vvoo(b,d,t,k)
      !       end do

      !       deallocate(imx)
      !       deallocate(xmx)
      !       deallocate(ymx)

      ! end function alternative_t1_1

      ! function alternative_t1_2(t1, nocc, nactive, c,k,l,j)
      !       double precision :: alternative_t1_2
      !       integer, intent(in) :: nocc, nactive
      !       double precision, dimension(nocc+1:nactive, nocc), intent(in) :: t1
      !       integer, intent(in) :: c, k, l,j
      !       double precision, dimension(:,:), allocatable :: xmx
      !       double precision, dimension(:,:), allocatable :: ymx
      !       double precision, dimension(:,:), allocatable :: imx
      !       integer :: i, a

      !       integer :: t, u, w

      !       allocate(imx(nactive, nactive))
      !       allocate(xmx(nactive, nactive))
      !       allocate(ymx(nactive, nactive))
      !       call identity_mx(nactive, imx)

      !       xmx = imx
      !       ymx = imx

      !       do i = 1, nocc
      !             do a = nocc + 1, nactive
      !                   xmx(a, i) = xmx(a, i) - t1(a, i)
      !             end do
      !       end do

      !       do i = 1, nocc
      !             do a = nocc + 1, nactive
      !                   ymx(i, a) = ymx(i, a) + t1(a, i)
      !             end do
      !       end do

      !       alternative_t1_2 = vooo(c,k,l,j)
      !       do t = 1, nocc
      !             do u = nocc+1, nactive
      !                   do w = nocc+1, nactive
      !                         alternative_t1_2 = alternative_t1_2 +&
      !                               xmx(c,t)*ymx(k,u)*xmx(l,l)*ymx(j,w)*vovo(u,t,w,l)
      !                   end do
      !             end do
      !       end do

      !       do u = nocc+1, nactive
      !             do w = nocc+1, nactive
      !                   alternative_t1_2 = alternative_t1_2 +&
      !                         xmx(c,c)*ymx(k,u)*xmx(l,l)*ymx(j,w)*vvvo(u,c,w,l)
      !             end do
      !       end do

      !       do t = 1, nocc
      !             do w = nocc+1, nactive
      !                   alternative_t1_2 = alternative_t1_2 +&
      !                         xmx(c,t)*ymx(k,k)*xmx(l,l)*ymx(j,w)*vooo(w,l,k,t)
      !             end do
      !       end do

      !       do t = 1, nocc
      !             do u = nocc+1, nactive
      !                   alternative_t1_2 = alternative_t1_2 +&
      !                         xmx(c,t)*ymx(k,u)*xmx(l,l)*ymx(j,j)*vooo(u,t,j,l)
      !             end do
      !       end do

      !       do t = 1, nocc
      !             alternative_t1_2 = alternative_t1_2 +&
      !                   xmx(c,t)*ymx(k,k)*xmx(l,l)*ymx(j,j)*oooo(k,t,j,l)
      !       end do


      !       do u = nocc+1, nactive
      !             alternative_t1_2 = alternative_t1_2 +&
      !                   xmx(c,c)*ymx(k,u)*xmx(l,l)*ymx(j,j)*vvoo(u,c,j,l)
      !       end do

      !       do w = nocc+1, nactive
      !             alternative_t1_2 = alternative_t1_2 +&
      !                   xmx(c,c)*ymx(k,k)*xmx(l,l)*ymx(j,w)*vovo(c,k,w,l)
      !       end do

      !       deallocate(imx)
      !       deallocate(xmx)
      !       deallocate(ymx)


      ! end function alternative_t1_2

end module cc3_intermediates
