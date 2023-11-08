module drv_cc_aux
      
      use arithmetic
      use scf
!      use blas
      use io
      use davidson_main
      use density_matrix_gr_exc
      use density_matrix_ground
      use overlap_exc_exc_functions
      use ccsd
      use eom_vectors
      use symmetry
      use drv_wm_intermediates_init
      use blas1
      use blas2
      use blas3
      use blas4
      use blas5
      use blas6
      use blas_olenkifer
      use gparam
      use math_constants


      implicit none

      ! integer, parameter :: TRANS_DIP = 1
      ! integer, parameter :: TRANS_QUAD = 2
      ! integer, parameter :: TRANS_NON = 3
      ! integer, parameter :: TRANS_ONEAO = 4
      ! integer, parameter :: TRANS_ONEMO = 5
      real(F64), dimension(:,:), allocatable :: dm_wm_pt0
      real(F64), dimension(:,:), allocatable :: dm_wm_pt1
      real(F64), dimension(:,:), allocatable :: dm_wm_pt2
      real(F64), dimension(:,:), allocatable :: dm_wm_pt3
      real(F64), dimension(:,:), allocatable :: dm_wm_pt4

      integer, parameter, private :: prop_dip = 1
      integer, parameter, private :: prop_so =2



contains

      subroutine compute_transitions_gr_exc2(method, mocoeff, overlap, &
           t2, t1, s2, s1, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
           nactive, irrep0, irrep1, order, tst)

            integer, intent(in)                           :: method           
            real(F64), dimension(:, :), intent(in)        :: mocoeff
            real(F64), dimension(:, :), intent(in)        :: overlap
            real(F64), dimension(:, :, :, :),  intent(in) :: t2
            real(F64), dimension(:, :), intent(in)        :: t1
            real(F64), dimension(:, :, :, :),  intent(in) :: s2
            real(F64), dimension(:, :),  intent(in)       :: s1
            integer, intent(in)                           :: nocc0, nocc1
            integer, intent(in)                           :: nvirt0, nvirt1
            integer, intent(in)                           :: nocc, nvirt
            integer, intent(in)                           :: nactive
            integer, dimension(:, :), intent(in)          :: irrep0, irrep1
            integer, intent(in)                           :: order
            type(TState), dimension(:), intent(inout)     :: tst
            integer, dimension(:), allocatable      :: iexci_s_sum, iexci_d_sum


            real(F64), dimension(:, :), allocatable :: lvec_k1, rvec_k1
            real(F64), dimension(:, :), allocatable :: lvec_k2, rvec_k2
            real(F64), dimension(:, :), allocatable :: tm_s, tm_d
            integer, dimension(:), allocatable      :: lexci_s_sum, uexci_s_sum
            integer, dimension(:), allocatable      :: lexci_d_sum, uexci_d_sum
            integer, dimension(:), allocatable      :: lexci_sum(:, :), uexci_sum(:, :)

                        
            
            real(F64), dimension(:, :), allocatable :: dip_s, quad_s, non_s
            real(F64), dimension(:, :), allocatable :: dip_d, quad_d, non_d
            real(F64), dimension(:,:), allocatable :: dm_gamma, dm_xi
            real(F64), dimension(:,:), allocatable :: over_xx
            real(F64), dimension(:, :), allocatable :: super_tm


            real(F64), dimension(:,:,:), allocatable :: super_dm_gamma, super_dm_xi
            integer :: dm_xx_order
            real(F64) :: exc_exc_over_kk
            type(tclock)                          :: time
            

            integer   :: k_ss, k_sd, k_ds, k_dd
            integer   :: i, j, k1, k2, p
            real(F64) :: num_factor, tm_ee, tm_wm, tm_um
            integer   :: kk, ii, mli
            integer :: npair
            real(F64), dimension(:,:), allocatable :: norm_r1_s, norm_r1_d
            real(F64) :: sum, sum1, sum2, sum3, exc_exc_overlap
            integer :: p1, p2
            real(F64), dimension(:), allocatable :: tm_non
            real(F64) :: tmxx, tmyy, tmzz

            npair = nocc * nvirt

            allocate(lexci_s_sum(order))
            allocate(uexci_s_sum(order))
            allocate(lexci_d_sum(order))
            allocate(uexci_d_sum(order))

            allocate(lexci_sum(4, order))
            allocate(uexci_sum(4, order))

            allocate(dip_s(100, order))
            allocate(quad_s(100, order))
            allocate(non_s(100, order))


            dip_s = zero
            quad_s = zero
            non_s = zero

           do i = 1, 4
                  lexci_sum(i, :) = tst(i)%tlexci(2, :) - tst(i)%tlexci(1, :)
                  uexci_sum(i, :) = tst(i)%tuexci(2, :) - tst(i)%tuexci(1, :)
                  do j = 1, order
                        if (tst(i)%tlexci(2, j) .ne. 0) lexci_sum(i, j) = lexci_sum(i, j) + 1
                        if (tst(i)%tuexci(2, j) .ne. 0) uexci_sum(i, j) = uexci_sum(i, j) + 1
                  end do
            end do


            print*, 'lexci'
            write(*, '(8I4)')lexci_sum
            print*, 'uexci'
            write(*, '(8I4)')uexci_sum

            dm_xx_order = 1
            do i = 1, order
               do ii = 1, 4
                  if (uexci_sum(ii, i).ne.0)then
                     do k2 = tst(ii)%tuexci(1, i), tst(ii)%tuexci(2, i)
                        dm_xx_order = dm_xx_order + 1
                     end do
                  end if
               end do
            end do

            print*,'dm_xx_order',  dm_xx_order

            allocate(super_dm_gamma(dm_xx_order-1, CC_NORB, CC_NORB))
            allocate(super_dm_xi(dm_xx_order-1, CC_NORB, CC_NORB))
            allocate(super_tm(dm_xx_order-1, 2))

            super_dm_gamma = zero
            super_dm_xi = zero
            super_tm = zero

            if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed)then
                  if (triplet_present(lexci_sum)) then
                        print*, 'alokuje triplet w lewym'
                        allocate(lvec_k1(tst(3)%tnidx, 1))
                        allocate(rvec_k1(tst(3)%tnidx, 1))
                        lvec_k1 = zero
                        rvec_k1 = zero
                  else
                        print*, 'alokuje singlet w lewym', tst(1)%tnidx
                        allocate(lvec_k1(tst(1)%tnidx, 1))
                        allocate(rvec_k1(tst(1)%tnidx, 1))
                        lvec_k1= zero
                        rvec_k1= zero
                  end if
                  
                  if (triplet_present(uexci_sum)) then
                        print*, 'alokuje triplet', tst(3)%tnidx
                        allocate(lvec_k2(tst(3)%tnidx, 1))
                        allocate(rvec_k2(tst(3)%tnidx, 1))
                        lvec_k2= zero
                        rvec_k2= zero
                  else
                        print*, 'alokue singlet', tst(1)%tnidx
                        allocate(lvec_k2(tst(1)%tnidx, 1))
                        allocate(rvec_k2(tst(1)%tnidx, 1))
                        lvec_k2= zero
                        rvec_k2= zero
                  end if
            end if

            allocate(over_xx(dm_xx_order-1, 2))!**  
            over_xx = zero

            ! do i = 1, order
            !    do ii = 1, 4
            !       if (uexci_sum(ii, i).ne.0)then
            !          do k1 = tst(ii)%tuexci(1, i), tst(ii)%tuexci(2, i)
            !                print*, 'plusz', i, ii, k1
            !                call clock_start(time)


            !               !  call readconverged_nonsymmetric_single_right(tst(ii)%trecs(i), &
            !               !        k1, rvec_k2(1:tst(ii)%tnidx, 1))
            !               ! do j = 1, tst(ii)%tnidx
            !               !        print*, 'laaaaaaaaaaaaaaaaaa', rvec_k2(j, 1)
            !               !  end do

                           
            !                call readconverged_nonsymmetric_single(tst(ii)%trecs(i), &
            !                      k1, lvec_k2(1:tst(ii)%tnidx, 1), rvec_k2(1:tst(ii)%tnidx, 1))
                           
            !                ! do j = 1, tst(ii)%tnidx
            !                !       print*, '0laaaaaaaaaaaaaaaaaa', rvec_k2(j, 1)
            !                ! end do

            !             call exc_exc_over(method, exc_exc_over_kk, t1, t2, &
            !                  s1, s2, nocc, nactive, rvec_k2(1:tst(ii)%tnidx, 1), rvec_k2(1:tst(ii)%tnidx, 1), ii)
            !             call dmsg("TOTAL TIME - OVERLAP KK", clock_readwall(time))
            !             over_xx(dm_xx_order, 1) = exc_exc_over_kk
            !             print*, i, ii, k1, 'exc_exc_over', exc_exc_over_kk
            !          end do
            !       end if
            !    end do
            ! end do

            do i = 1, order
               do ii = 1, 4
                  if (uexci_sum(ii, i).ne.0)then
                     do k1 = tst(ii)%tuexci(1, i), tst(ii)%tuexci(2, i)
                           print*, '-----------------------------------------------------------------------------------------------------'
                           print*, 'przejscie do stanu', k1, 'z', i, '-tej symetrii'
                        call readconverged_nonsymmetric_single(tst(ii)%trecs(i), &
                             k1, lvec_k2(1:tst(ii)%tnidx, 1), rvec_k2(1:tst(ii)%tnidx, 1))
                        
!                        call la_biorth_olenkifer(.true., lvec_k2(1:tst(ii)%tnidx,:), rvec_k2(1:tst(ii)%tnidx, :), 0)

                        call exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvec_k2(:, 1), rvec_k2(:, 1))
                        print*, 'exc_over', exc_exc_overlap
                                                
                        call generate_density_gr_exc(lvec_k2(1:tst(ii)%tnidx, 1), rvec_k2(1:tst(ii)%tnidx, 1),&
                             t2, t1, s2, s1, &
                             nocc, nactive, method, super_dm_gamma(dm_xx_order, :,:), super_dm_xi(dm_xx_order, :,:), &
                             i, irrep0, irrep1, order)

                        call task_transmom_dip(tmxx, tmyy, tmzz, mocoeff, nocc0, nocc1, nvirt0, nvirt1,nocc, nvirt, &
                              super_dm_gamma(dm_xx_order, :,:),  super_dm_xi(dm_xx_order, :,:))

                        print*, 'tmxx-transmom_dip', tmxx
                        print*, 'tmxx-transmom_dip', tmyy
                        print*, 'tmxx-transmom_dip', tmzz
                     
                        call task_transmom_driver(mocoeff, super_dm_gamma(dm_xx_order, :,:), super_dm_xi(dm_xx_order, :,:), &
                              nocc0, nocc1, nvirt0, nvirt1, dip_s(k1, i), &
                              quad_s(k1, i), non_s(k1, i))
                        
                        ! super_tm(dm_xx_order, 1) = tm_wm * norm_factor
                        ! super_tm(dm_xx_order, 2) = tm_um * norm_factor

                      print*, 'dip', dip_s(k1, i)
                      print*, 'quad', quad_s(k1, i)
                      print*, '-----------------------------------------------------------------------------------------------------'
                      print*, ''

                   end do
                end if
             end do
          end do
            call midrule(width=100)


            call grexc_table_start2



            do i = 1, order
                  do ii = 1, 4
                        if (uexci_sum(ii, i).ne.0)then
                           if (ii == 1 .or. ii == 2)then
                                 mli = 1
                           else
                                 mli = 3
                           end if
                           do k1 = tst(ii)%tuexci(1, i), tst(ii)%tuexci(2, i)

                                 call grexc_table_continue2(print_rep_mult(i, mli), &
                                       k1, tst(ii)%twr_exc(k1, i), &
                                       dip_s(k1, i), quad_s(k1, i))
                                 
                           end do
                     end if
               end do
         end do
         call midrule(width=128)


            ! dm_xx_order = 1!**                                                                                                                                         
            ! do i = 1, order
            !       do ii = 1, 4
            !             if (uexci_sum(ii, i).ne.0)then
            !                   if (ii == 1 .or. ii == 2)then
            !                      mli = 1
            !                else
            !                      mli = 3
            !                end if
            !                do k1 = tst(ii)%tuexci(1, i), tst(ii)%tuexci(2, i)
            !                      call grexc_table_continue2(print_rep_mult(i, mli), k1, tst(ii)%twr_exc(k1, i),
                                 
            !                   call excexc_table_continue2(print_rep_mult(i, mli), print_rep_mult(j, mlj), &
            !                        k1, k2, tst(ii)%twr_exc(k1, i), tst(jj)%twr_exc(k2, j),&
            !                        super_tm(dm_xx_order, 1), super_tm(dm_xx_order, 2), one, &
            !                        one)
            !                                         dm_xx_order = dm_xx_order + 1
            !                end do
            !          end if
            !             end do
            !          end do
            !       end do
            ! end if
            !    end do
            ! end do


      contains

           function triplet_present(exci_sum)
                  logical :: triplet_present
                  integer, dimension(:,:), intent(in) :: exci_sum
                  integer :: i, ii

                  triplet_present = .false.
                  outer_loop: do i = 1, order
                        if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed)then
                              do ii = 3, 4
                                    if (exci_sum(ii, i).ne.0)then
                                          triplet_present = .true.
                                          exit outer_loop
                                    end if
                              end do
                        else if (cc_multip == cc_mixed_left)then
                              do ii = 1, 2
                                    if (exci_sum(ii, i).ne.0)then
                                          triplet_present = .true.
                                          exit outer_loop
                                    end if
                              end do

                              end if
                  end do outer_loop
            end function triplet_present


      subroutine exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvl, rvr)

            integer, intent(in) :: method
            real(F64), intent(out) :: exc_exc_overlap
            real(F64), dimension(:,:), intent(in) :: t1, s1
            real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(:), intent(in) :: rvl, rvr

           if (cc_multip == cc_singlet) then
                 call  d_overlap_ccsd_init(nocc, nactive)
                 call d_overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                 exc_exc_overlap =  d_overlap_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                 call d_overlap_ccsd_free()

            else if (cc_multip == cc_triplet) then
                  call d_overlap_triplet_ccsd_init(nocc, nactive)
                  call d_overlap_intermediates_triplet_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  exc_exc_overlap =  d_overlap_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  call d_overlap_triplet_ccsd_free()
            end if
      end subroutine exc_exc_over


      ! subroutine exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvl, rvr, i)

      !       integer, intent(in) :: method
      !       real(F64), intent(out) :: exc_exc_overlap
      !       real(F64), dimension(:,:), intent(in) :: t1, s1
      !       real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
      !       integer, intent(in) :: nocc, nactive
      !       real(F64), dimension(:), intent(in) :: rvl, rvr
      !       integer, intent(in) :: i
      !       integer :: multip


      !       if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed) then
      !             if (i == 1 .or. i == 2) then
      !                   multip = 1
      !             else if (i == 3 .or. i == 4) then
      !                   multip = 3
      !             end if
      !       else if (cc_multip == cc_mixed_left) then
      !             if (i == 3 .or. i == 4) then
      !                   multip = 1
      !             else if (i == 1 .or. i == 2) then
      !                   multip = 3
      !             end if
      !       end if


      !      if (cc_multip == cc_singlet) then
      !            call  d_overlap_ccsd_init(nocc, nactive)
      !            call d_overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !            exc_exc_overlap =  d_overlap_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !            call d_overlap_ccsd_free()

      !            ! if (method .eq. THEORY_CC3) then
      !            !        call d_overlap_cc3_init(nocc, nactive)
      !            !        call d_overlap_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !            !        exc_exc_overlap =  exc_exc_overlap + d_overlap_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !            !        call d_overlap_cc3_free()
      !            !  end if

      !       else if (cc_multip == cc_triplet) then
      !             call d_overlap_triplet_ccsd_init(nocc, nactive)
      !             call d_overlap_intermediates_triplet_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !             exc_exc_overlap =  d_overlap_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !             call d_overlap_triplet_ccsd_free()
      !             ! if (method .eq. THEORY_CC3) then
      !             !       call d_overlap_triplet_cc3_init(nocc, nactive)
      !             !       call d_overlap_intermediates_triplet_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !             !       exc_exc_overlap =  exc_exc_overlap + d_overlap_cc3_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
      !             !       call d_overlap_triplet_cc3_free()
      !             ! end if
      !       end if

      !       ! call overlap_ccsd_init(nocc, nactive)

      !       ! call overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)

      !       ! exc_exc_overlap = overlap_f(t2, t1, s2, s1, nocc, nactive, rvl, rvr)

      !       ! call overlap_ccsd_free()

      ! end subroutine exc_exc_over
            subroutine grexc_table_start2()
                  character(len=140) :: line
                  !
                  ! SYM1 SYM2 ST1 ST2 ENERGY1 ENERGY2 TML TMR TM
                  !
                  write(line, "(A1, 1X, A5, 1X, A1, 1X, A3, 1X, A1, 1X, A12, 1X, A1, 1X, A20, 1X, A1, 1X, A12, 1X, A1, 1X, A20, 1X, A1, 1X, A12, 1X, A1, 1X, A12, 1X, A1)")&
                        "|", "SYMM", "|", "ST", "|", "ENERGY [h]", "|", "ENERGY [cm]", "|","DIP_STR", "|","DIP_A", "|", "QUAD_STR", "|", "QUAD_A", "|"

                  call midrule(width=140)
                  call msg(line)
                  call midrule(width=140)
            end subroutine grexc_table_start2

            subroutine grexc_table_continue2(sym, st, e, dip, quad)
                  character(len=140) :: line
                  character(5), intent(in)   :: sym
                  integer, intent(in)   :: st
                  real(F64), intent(in) :: e
                  real(F64), intent(in) :: dip, quad
                  real(F64) :: quadA, dipA
                  !
                  ! SYM1 SYM2 ST1 ST2 ENERGY1 ENERGY2 TML TMR TM
                  !
                  quadA = totransprob_quad_shortley(quad, e, 5)
                  dipA = totransprob_dipole(dip, e, 3)
                  write(line, "(A1, 1X, A5, 1X, A1, 1X, I3, 1X, A1, 1X, F12.7, 1X, A1, 1X, F20.7, 1X, A1, 1X, F12.7, 1X, 1A, 1X, F20.7, 1X, 1A, 1X, F12.7, 1X, 1A, F12.7, 1X, 1A)")&
                        "|", sym, "|", st, "|", e, "|", tocm_1(e), "|", &
                        dip, "|", dipA, '|', quad, '|', quadA, '|'

                  call msg(line)


            end subroutine grexc_table_continue2

            subroutine task_transmom_driver(mocoeff, dm_gamma, dm_xi, &
                  nocc0, nocc1, nvirt0, nvirt1, dip, quad, non)

                  real(F64), dimension(:,:), intent(in) :: mocoeff
                  real(F64), dimension(:,:), intent(in) :: dm_gamma
                  real(F64), dimension(:,:), intent(in) :: dm_xi
                  integer, intent(in) :: nocc0, nocc1
                  integer, intent(in) :: nvirt0, nvirt1
                  real(F64), intent(out) :: dip
                  real(F64), intent(out) :: quad
                  real(F64), intent(out) :: non

                  real(F64), dimension(3) :: tm_dip_l, tm_dip_r, tm_dip
                  real(F64), dimension(6) :: tm_quad_l, tm_quad_r, tm_quad
                  real(F64), dimension(3) :: tm_non_l, tm_non_r, tm_non

                  real(F64)                                              :: tmxxxx
                  real(F64)                                              :: tmyyyy
                  real(F64)                                              :: tmzzzz
                  real(F64)                                              :: tmyxyx
                  real(F64)                                              :: tmzxzx
                  real(F64)                                              :: tmzyzy

                  call task_transmom(tm_dip_l, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1, TRANS_DIP)
                  call task_transmom(tm_dip_r, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1, TRANS_DIP)
                  print*, 'dip-gamma', tm_dip_l
                  print*, 'dip-xi', tm_dip_r
                  call vec_m(tm_dip, tm_dip_l, tm_dip_r)
                  
                  call task_line_strength(tm_dip, dip)

                  print*, 'task_transmom_quad_gamma'
                  call task_transmom(tm_quad_l, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1, TRANS_QUAD)
                  print*, 'task_transmom_quad_xi'
                  call task_transmom(tm_quad_r, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1, TRANS_QUAD)

                  ! call task_transmom_quad(tmxxxx, tmyyyy, tmzzzz, tmyxyx, tmzxzx, tmzyzy, &
                  !       mocoeff, nocc0, nocc1, nvirt0, nvirt1,nocc, nvirt, &
                  !       dm_gamma, dm_xi)

                  ! print*, 'xx', tmxxxx
                  ! print*, 'yy', tmyyyy
                  ! print*, 'zz', tmzzzz
                  ! print*, 'yx', tmyxyx
                  ! print*, 'zx', tmzxzx
                  ! print*, 'zy', tmzyzy

                  call vec_m(tm_quad, tm_quad_l, tm_quad_r)

                  call task_line_strength_quad(tm_quad, quad)

                  ! call task_transmom(tm_non_l, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1, TRANS_NON)
                  ! call task_transmom(tm_non_r, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1, TRANS_NON)
                  ! call vec_m(tm_non, tm_non_l, tm_non_r)

                  ! print*, 'tm_non', tm_non
                  ! print*, 'sqrt(tm_non)', abs(sqrt(tm_non(1))), abs(sqrt(tm_non(2))), abs(sqrt(tm_non(3)))

                  ! call task_line_strength(tm_dip, dip)
                  ! quad = -1.d+2
                  ! non = -1.d+2

            end subroutine task_transmom_driver

            subroutine vec_m(v_out, v1, v2)
                  real(F64), dimension(:), intent(out) :: v_out
                  real(F64), dimension(:), intent(in) :: v1, v2

                  integer :: i

                  do i = 1, size(v1, dim=1)
                        v_out(i) = v1(i) * v2(i)
                  end do

            end subroutine vec_m

      end subroutine compute_transitions_gr_exc2


      subroutine compute_transitions_gr_exc(method, mocoeff, overlap, &
            t2, t1, s2, s1, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
            nactive, nidx, irrep0, irrep1, order, convrecs_s, convrecs_d, &
            wr_exc_s, wr_exc_d, maxexc_s, maxexc_d, iexci_s, iexci_d)

            integer, intent(in)                           :: method           
            real(F64), dimension(:, :), intent(in)        :: mocoeff
            real(F64), dimension(:, :), intent(in)        :: overlap
            real(F64), dimension(:, :, :, :),  intent(in) :: t2
            real(F64), dimension(:, :), intent(in)        :: t1
            real(F64), dimension(:, :, :, :),  intent(in) :: s2
            real(F64), dimension(:, :),  intent(in)       :: s1
            integer, intent(in)                           :: nocc0, nocc1
            integer, intent(in)                           :: nvirt0, nvirt1
            integer, intent(in)                           :: nocc, nvirt
            integer, intent(in)                           :: nactive, nidx
            integer, dimension(:, :), intent(in)          :: irrep0, irrep1
            integer, intent(in)                           :: order
            type(trecgroup), dimension(:), intent(inout)  :: convrecs_s, convrecs_d
            real(F64), dimension(:, :), intent(in)        :: wr_exc_s, wr_exc_d
            integer, intent(in)                           :: maxexc_s, maxexc_d
            integer, dimension(:,:), intent(in)           :: iexci_s, iexci_d
            integer, dimension(:), allocatable      :: iexci_s_sum, iexci_d_sum


            real(F64), dimension(:, :), allocatable :: lvec_k1, rvec_k1
            real(F64), dimension(:, :), allocatable :: lvec_k2, rvec_k2
            real(F64), dimension(:, :), allocatable :: dip_s, quad_s, non_s
            real(F64), dimension(:, :), allocatable :: dip_d, quad_d, non_d
            integer, dimension(:), allocatable      :: lexci_s_sum, uexci_s_sum
            integer, dimension(:), allocatable      :: lexci_d_sum, uexci_d_sum
            real(F64), dimension(:,:), allocatable :: dm_gamma, dm_xi

            integer   :: k_ss, k_sd, k_ds, k_dd
            integer   :: i, j, k1, k2, p
            real(F64) :: num_factor, tm_ee, tm_wm, tm_um
            integer   :: kk, ll
            integer :: npair
            real(F64), dimension(:,:), allocatable :: norm_r1_s, norm_r1_d
            real(F64) :: sum, sum1, sum2, sum3, exc_exc_overlap
            integer :: p1, p2
            real(F64), dimension(:), allocatable :: tm_non
            real(F64) :: tmxx, tmyy, tmzz

            npair = nocc * nvirt

            allocate(dm_gamma(CC_NORB, CC_NORB))
            allocate(dm_xi(CC_NORB, CC_NORB))
            dm_gamma = zero
            dm_xi = zero

            allocate(norm_r1_s(maxexc_s, order))
            allocate(norm_r1_d(maxexc_d, order))

            allocate(dip_s(maxexc_s, order))
            allocate(quad_s(maxexc_s, order))
            allocate(non_s(maxexc_s, order))

            allocate(dip_d(maxexc_d, order))
            allocate(quad_d(maxexc_d, order))
            allocate(non_d(maxexc_d, order))

            dip_s = zero
            quad_s = zero
            non_s = zero

            dip_d = zero
            quad_d = zero
            non_d = zero

            allocate(iexci_s_sum(order))
            allocate(iexci_d_sum(order))


            iexci_s_sum = iexci_s(2, :) - iexci_s(1, :)

            do i = 1, order
                  if (iexci_s(2, i) .ne. 0) iexci_s_sum(i) = iexci_s_sum(i) + 1
            end do

            iexci_d_sum = iexci_d(2, :) - iexci_d(1, :)

            do i = 1, order
               if (iexci_d(2, i) .ne. 0) iexci_d_sum(i) = iexci_d_sum(i) + 1
            end do

            k_ss = 1
            k_sd = 1
            k_ds = 1
            k_dd = 1

            allocate(lvec_k1(nidx, 1))
            allocate(rvec_k1(nidx, 1))
            
            do i = 1, order

               if (iexci_s_sum(i) .ne. 0) then

                  do k1 = iexci_s(1, i), iexci_s(2, i)                              

                     print*, 'SYMETRIA', i, 'STAN', k1

                     call readconverged_nonsymmetric_single(convrecs_s(i), &
                          k1, lvec_k1(:, 1), rvec_k1(:, 1))

                     call la_biorth_olenkifer(.true., lvec_k1, rvec_k1, 0)

                     norm_r1_s(k1, i) = 0
                     do p = 1, npair
                        norm_r1_s(k1, i) = norm_r1_s(k1, i) + lvec_k1(p, 1)**2
                     end do
                     norm_r1_s(k1, i) = sqrt(norm_r1_s(k1, i))

                     call generate_density_gr_exc(lvec_k1(:, 1), rvec_k1(:, 1),&
                           t2, t1, s2, s1, &
                           nocc, nactive, method, dm_gamma, dm_xi, &
                           i, irrep0, irrep1, order) 


                     call exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvec_k1(:, 1), rvec_k1(:, 1))

                     if (SLATER_BASIS .eqv. .false.) then
                           allocate(tm_non(3))
                           call msg('NONADIABATIC COUPLING...')
                           call task_cc_grad_one(tm_non, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, dm_gamma, CC_NON)
                           print*, 'gamma-nonadiabatic', tm_non
                           call task_cc_grad_one(tm_non, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, dm_xi, CC_NON)
                           print*, 'xi nonadiabatic', tm_non
                           print*, 'nonadiabatic', tm_non(1)**2 * exc_exc_overlap, tm_non(2)**2 * exc_exc_overlap, &
                                 tm_non(3)**2 * exc_exc_overlap
                           print*, 'exc_exc_overlap', exc_exc_overlap
                           deallocate(tm_non)
                     end if

                     call task_transmom_dip(tmxx, tmyy, tmzz, mocoeff, nocc0, nocc1, nvirt0, nvirt1,nocc, nvirt, &
                           dm_gamma, dm_xi)
                     print*, 'tmxx-transmom_dip', tmxx
                     print*, 'tmxx-transmom_dip', tmyy
                     print*, 'tmxx-transmom_dip', tmzz
                     
                     call task_transmom_driver(mocoeff, dm_gamma, dm_xi, &
                           nocc0, nocc1, nvirt0, nvirt1, dip_s(k1, i), &
                           quad_s(k1, i), non_s(k1, i))

                      print*, 'przejscie do stanu', k1, 'z', i, '-tej symetrii'
                      print*, 'dip', dip_s(k1, i)

                    call task_transmom_driver(mocoeff, dm_xi, dm_xi, &
                           nocc0, nocc1, nvirt0, nvirt1, dip_s(k1, i), &
                           quad_s(k1, i), non_s(k1, i))

                    print*, 'xixi przejscie do stanu', k1, 'z', i, '-tej symetrii'
                    print*, 'dip', dip_s(k1, i), dip_s(k1, i) * exc_exc_overlap
                      
                    

                  end do
               end if

               if (iexci_d_sum(i) .ne. 0) then

                  do k1 = iexci_d(1, i), iexci_d(2, i)

                     call readconverged_nonsymmetric_single(convrecs_d(i), &
                          k1, lvec_k1(:, 1), rvec_k1(:, 1))

                     call la_biorth_olenkifer(.true., lvec_k1, rvec_k1, 0)

                     norm_r1_d(k1, i) = 0
                     do p = 1, npair
                        norm_r1_d(k1, i) = norm_r1_d(k1, i) + lvec_k1(p, 1)**2
                     end do
                     norm_r1_d(k1, i) = sqrt(norm_r1_d(k1, i))

                     call generate_density_gr_exc(lvec_k1(:, 1), rvec_k1(:, 1),&
                           t2, t1, s2, s1, &
                           nocc, nactive, method, dm_gamma, dm_xi, &
                           i, irrep0, irrep1, order) 

                     call exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvec_k1(:, 1), rvec_k1(:, 1))                     

                     call task_transmom_driver(mocoeff, dm_gamma, dm_xi, &
                           nocc0, nocc1, nvirt0, nvirt1, dip_d(k1, i), &
                           quad_d(k1, i), non_d(k1, i))

                     print*, 'exc_exc_overlap', exc_exc_overlap                     
                     print*, 'przejscie do stanu', k1, 'z', i, '-tej symetrii'
                     print*, 'dip', dip_d(k1, i)

                     call task_transmom_driver(mocoeff, dm_xi, dm_xi, &
                           nocc0, nocc1, nvirt0, nvirt1, dip_d(k1, i), &
                           quad_d(k1, i), non_d(k1, i))

                    print*, 'xixi przejscie do stanu', k1, 'z', i, '-tej symetrii'
                    print*, 'dip', dip_d(k1, i), dip_d(k1, i) * exc_exc_overlap

                      
                  end do
               end if
            end do

            deallocate(lvec_k1)
            deallocate(rvec_k1)


            call grexc_table_start2()

            
            do i = 1, order
               if (iexci_s_sum(i) .ne. 0) then
                  do k1 = iexci_s(1, i), iexci_s(2, i)
                     call grexc_table_continue2(print_rep(i), &
                          k1, wr_exc_s(k1, i), dip_s(k1, i))
                     
                  end do
               end if
               if (iexci_d_sum(i) .ne. 0) then
                  do k1 = iexci_d(1, i), iexci_d(2, i)
                     call grexc_table_continue2(print_rep(i), &
                          k1, wr_exc_d(k1, i), dip_d(k1, i))
                     
                  end do
               end if

            end do

            call midrule(width=100)

      contains

      subroutine exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvl, rvr)

            integer, intent(in) :: method
            real(F64), intent(out) :: exc_exc_overlap
            real(F64), dimension(:,:), intent(in) :: t1, s1
            real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(:), intent(in) :: rvl, rvr

           if (cc_multip == cc_singlet) then
                 call  d_overlap_ccsd_init(nocc, nactive)
                 call d_overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                 exc_exc_overlap =  d_overlap_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                 call d_overlap_ccsd_free()

                 ! if (method .eq. THEORY_CC3) then
                 !        call d_overlap_cc3_init(nocc, nactive)
                 !        call d_overlap_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                 !        exc_exc_overlap =  exc_exc_overlap + d_overlap_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                 !        call d_overlap_cc3_free()
                 !  end if

            else if (cc_multip == cc_triplet) then
                  call d_overlap_triplet_ccsd_init(nocc, nactive)
                  call d_overlap_intermediates_triplet_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  exc_exc_overlap =  d_overlap_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  call d_overlap_triplet_ccsd_free()
                  ! if (method .eq. THEORY_CC3) then
                  !       call d_overlap_triplet_cc3_init(nocc, nactive)
                  !       call d_overlap_intermediates_triplet_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  !       exc_exc_overlap =  exc_exc_overlap + d_overlap_cc3_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  !       call d_overlap_triplet_cc3_free()
                  ! end if
            end if

            ! call overlap_ccsd_init(nocc, nactive)

            ! call overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)

            ! exc_exc_overlap = overlap_f(t2, t1, s2, s1, nocc, nactive, rvl, rvr)

            ! call overlap_ccsd_free()

      end subroutine exc_exc_over
            subroutine grexc_table_start2()
                  character(len=100) :: line
                  !
                  ! SYM1 SYM2 ST1 ST2 ENERGY1 ENERGY2 TML TMR TM
                  !
                  write(line, "(A1, 1X, A5, 1X, A1, 1X, A3, 1X, A1, 1X, A12, 1X, A1, 1X, A12, 1X, A1, 1X)")&
                        "|", "SYMM", "|", "ST", "|", "ENERGY", "|","DIP_STR", "|"

                  call midrule(width=100)
                  call msg(line)
                  call midrule(width=100)
            end subroutine grexc_table_start2

            subroutine grexc_table_continue2(sym, st, e, dip)
                  character(len=128) :: line
                  character(5), intent(in)   :: sym
                  integer, intent(in)   :: st
                  real(F64), intent(in) :: e
                  real(F64), intent(in) :: dip
                  !
                  ! SYM1 SYM2 ST1 ST2 ENERGY1 ENERGY2 TML TMR TM
                  !
                  write(line, "(A1, 1X, A5, 1X, A1, 1X, I3, 1X, A1, 1X, F12.7, 1X, A1, 1X, F12.7, 1X, 1A)")&
                        "|", sym, "|", st, "|", e, "|",&
                        dip, "|"

                  call msg(line)

            end subroutine grexc_table_continue2

            subroutine task_transmom_driver(mocoeff, dm_gamma, dm_xi, &
                  nocc0, nocc1, nvirt0, nvirt1, dip, quad, non)

                  real(F64), dimension(:,:), intent(in) :: mocoeff
                  real(F64), dimension(:,:), intent(in) :: dm_gamma
                  real(F64), dimension(:,:), intent(in) :: dm_xi
                  integer, intent(in) :: nocc0, nocc1
                  integer, intent(in) :: nvirt0, nvirt1
                  real(F64), intent(out) :: dip
                  real(F64), intent(out) :: quad
                  real(F64), intent(out) :: non


                  real(F64), dimension(3) :: tm_dip_l, tm_dip_r, tm_dip
                  real(F64), dimension(6) :: tm_quad_l, tm_quad_r, tm_quad
                  real(F64), dimension(3) :: tm_non_l, tm_non_r, tm_non

                  real(F64)                                              :: tmxxxx
                  real(F64)                                              :: tmyyyy
                  real(F64)                                              :: tmzzzz
                  real(F64)                                              :: tmyxyx
                  real(F64)                                              :: tmzxzx
                  real(F64)                                              :: tmzyzy


                  call task_transmom(tm_dip_l, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1, TRANS_DIP)
                  call task_transmom(tm_dip_r, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1, TRANS_DIP)
                  print*, 'dipl', tm_dip_l
                  print*, 'dipr', tm_dip_r
                  call vec_m(tm_dip, tm_dip_l, tm_dip_r)

                  print*, 'task_transmom_quad_gamma'
                  call task_transmom(tm_quad_l, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1, TRANS_QUAD)
                  print*, 'tasl_transmom_quad_xi'
                  call task_transmom(tm_quad_r, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1, TRANS_QUAD)



                  call task_transmom_quad(tmxxxx, tmyyyy, tmzzzz, tmyxyx, tmzxzx, tmzyzy, &
                        mocoeff, nocc0, nocc1, nvirt0, nvirt1,nocc, nvirt, &
                        dm_gamma, dm_xi)

                  print*, 'xx', tmxxxx
                  print*, 'yy', tmyyyy
                  print*, 'zz', tmzzzz
                  print*, 'yx', tmyxyx
                  print*, 'zx', tmzxzx
                  print*, 'zy', tmzyzy



                  call vec_m(tm_quad, tm_quad_l, tm_quad_r)

                  call task_transmom(tm_non_l, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1, TRANS_NON)
                  call task_transmom(tm_non_r, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1, TRANS_NON)
                  call vec_m(tm_non, tm_non_l, tm_non_r)

                  print*, 'tm_non', tm_non
                  print*, 'sqrt(tm_non)', abs(sqrt(tm_non(1))), abs(sqrt(tm_non(2))), abs(sqrt(tm_non(3)))

                  call task_line_strength(tm_dip, dip)
                  quad = -1.d+2
                  non = -1.d+2

            end subroutine task_transmom_driver

            subroutine vec_m(v_out, v1, v2)
                  real(F64), dimension(:), intent(out) :: v_out
                  real(F64), dimension(:), intent(in) :: v1, v2

                  integer :: i

                  do i = 1, size(v1, dim=1)
                        v_out(i) = v1(i) * v2(i)
                  end do

            end subroutine vec_m



      end subroutine compute_transitions_gr_exc


      subroutine compute_transitions_exc_exc(method, mocoeff, overlap, &
           t2, t1, s2, s1, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
           nactive, irrep0, irrep1, order, tst, prop) 

            integer, intent(in)                           :: method           
            real(F64), dimension(:, :), intent(in)        :: mocoeff
            real(F64), dimension(:, :), intent(in)        :: overlap
            real(F64), dimension(:, :, :, :),  intent(in) :: t2
            real(F64), dimension(:, :), intent(in)        :: t1
            real(F64), dimension(:, :, :, :),  intent(in) :: s2
            real(F64), dimension(:, :),  intent(in)       :: s1
            integer, intent(in)                           :: nocc0, nocc1
            integer, intent(in)                           :: nvirt0, nvirt1
            integer, intent(in)                           :: nocc, nvirt
            integer, intent(in)                           :: nactive

            integer, dimension(:, :), intent(in)          :: irrep0, irrep1
            integer, intent(in)                           :: order
            type(TState), dimension(:), intent(inout)     :: tst
            integer, intent(in)                           :: prop


            real(F64), dimension(:, :), allocatable :: lvec_k1, rvec_k1
            real(F64), dimension(:, :), allocatable :: lvec_k2, rvec_k2
            real(F64), dimension(:, :), allocatable :: tm_ss, tm_dd
            real(F64), dimension(:, :), allocatable :: tm_sd, tm_ds

            real(F64), dimension(:, :), allocatable :: over_ss, over_dd
            real(F64), dimension(:, :), allocatable :: over_sd, over_ds

            integer, dimension(:), allocatable      :: lexci_s_sum, uexci_s_sum
            integer, dimension(:), allocatable      :: lexci_d_sum, uexci_d_sum
            integer, dimension(:), allocatable      :: lexci_sum(:, :), uexci_sum(:, :)

            integer   :: k_ss, k_sd, k_ds, k_dd
            integer   :: i, j, k1, k2, p
            real(F64) :: norm_factor, tm_ee, tm_wm, tm_um
            real(F64) :: exc_exc_over_kk, exc_exc_over_ll
            integer   :: kk, ll
            integer :: npair

            integer :: p1, p2, jl
            integer :: dm_ss_order, dm_sd_order, dm_ds_order, dm_dd_order
            real(F64), dimension(:,:), allocatable :: dm_ss, dm_sd, dm_ds, dm_dd
            real(F64), dimension(:,:), allocatable :: um_ss, um_sd, um_ds, um_dd
            integer :: ss_idx, sd_idx, ds_idx, dd_idx
            integer :: pt, idx
            integer :: qw1, qw2, qw3
            integer :: a, hh
            real(F64) :: ss
            type(tclock)                          :: time
            character(:), allocatable :: file
            character(:), allocatable :: title
            integer :: lll, iii

            integer :: dm_xx_order
            real(F64), dimension(:,:,:), allocatable :: super_dm_wm_sym, super_dm_wm_asym, super_dm_um_sym, super_dm_um_asym
            real(F64), dimension(:, :), allocatable :: super_tm
            integer :: ii, jj
            real(F64), dimension(:,:), allocatable :: over_xx
            integer :: mli, mlj
            integer :: pp
            real(F64) :: norm_plusz

            npair = nocc * nvirt
 
            allocate(lexci_s_sum(order))
            allocate(uexci_s_sum(order))
            allocate(lexci_d_sum(order))
            allocate(uexci_d_sum(order))

            allocate(lexci_sum(4, order))
            allocate(uexci_sum(4, order))
            
            do i = 1, 4
                  lexci_sum(i, :) = tst(i)%tlexci(2, :) - tst(i)%tlexci(1, :)
                  uexci_sum(i, :) = tst(i)%tuexci(2, :) - tst(i)%tuexci(1, :)
                  do j = 1, order
                        if (tst(i)%tlexci(2, j) .ne. 0) lexci_sum(i, j) = lexci_sum(i, j) + 1
                        if (tst(i)%tuexci(2, j) .ne. 0) uexci_sum(i, j) = uexci_sum(i, j) + 1
                  end do
            end do
            dm_xx_order = 0

            title = titleheader(method, cc_multip)

            dm_xx_order = 1!**
            do i = 1, order
               do ii = 1, 4
                     if (lexci_sum(ii, i).ne.0)then
                  do k1 = tst(ii)%tlexci(1, i), tst(ii)%tlexci(2, i)
                        print*, 'symetria', i, 'zestaw', ii, 'stan', k1
                     do j = 1, order
                        do jj = 1, 4
                           kk = 1
                           if (uexci_sum(jj, j).ne.0)then
                           do k2 = tst(jj)%tuexci(1, j), tst(jj)%tuexci(2, j)
                                 print*,'Symetria', j, 'zestaw', jj, 'stan', k2

                              dm_xx_order = dm_xx_order + 1
                              kk = kk + 1
                           end do
                     end if
                        end do
                     end do
                     print*, '-----------------------'
                  end do
            end if
               end do
            end do

            allocate(over_xx(dm_xx_order-1, 2))!**
            allocate(super_dm_wm_sym(dm_xx_order-1, CC_NORB, CC_NORB)) !**
            allocate(super_dm_wm_asym(dm_xx_order-1, CC_NORB, CC_NORB)) !** 
            allocate(super_dm_um_sym(dm_xx_order-1, CC_NORB, CC_NORB)) !**
            allocate(super_dm_um_asym(dm_xx_order-1, CC_NORB, CC_NORB)) !**  
            allocate(super_tm(dm_xx_order-1, 2))!**

            over_xx = zero
            super_dm_wm_sym = zero
            super_dm_wm_asym = zero
            super_dm_um_sym = zero
            super_dm_wm_asym = zero
            super_tm = zero
            
            if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed)then
            if (triplet_present(lexci_sum)) then           
                  print*, 'alokuje triplet w lewym'
                  allocate(lvec_k1(tst(3)%tnidx, 1))
                  allocate(rvec_k1(tst(3)%tnidx, 1))
            else
                  print*, 'alokuje singlet w lewym', tst(1)%tnidx
                  allocate(lvec_k1(tst(1)%tnidx, 1))
                  allocate(rvec_k1(tst(1)%tnidx, 1))
            end if

            if (triplet_present(uexci_sum)) then
                  print*, 'alokuje triplet', tst(3)%tnidx
                  allocate(lvec_k2(tst(3)%tnidx, 1))
                  allocate(rvec_k2(tst(3)%tnidx, 1))
            else
                  print*, 'alokue singlet', tst(1)%tnidx
                  allocate(lvec_k2(tst(1)%tnidx, 1))
                  allocate(rvec_k2(tst(1)%tnidx, 1))
            end if
      else if (cc_multip == cc_mixed_left) then
            if (triplet_present(lexci_sum)) then
                  print*, 'alokuje triplet w lewym', tst(1)%tnidx
                  allocate(lvec_k1(tst(1)%tnidx, 1))
                  allocate(rvec_k1(tst(1)%tnidx, 1))
            else
                    print*, 'alokuje singlet w lewym'
                  allocate(lvec_k1(tst(3)%tnidx, 1))
                  allocate(rvec_k1(tst(3)%tnidx, 1))
            end if

            if (triplet_present(uexci_sum)) then
                  print*, 'alokuje triplet', tst(1)%tnidx
                  allocate(lvec_k2(tst(1)%tnidx, 1))
                  allocate(rvec_k2(tst(1)%tnidx, 1))
            else
                  print*, 'alokue singlet', tst(3)%tnidx
                  allocate(lvec_k2(tst(3)%tnidx, 1))
                  allocate(rvec_k2(tst(3)%tnidx, 1))
            end if

      end if



            dm_xx_order = 1 !**
            do i = 1, order
               do ii = 1, 4
                     if (lexci_sum(ii, i).ne.0)then
                  do k1 = tst(ii)%tlexci(1, i), tst(ii)%tlexci(2, i)

                     call readconverged_nonsymmetric_single_right(tst(ii)%trecs(i), &
                           k1, rvec_k1(1:tst(ii)%tnidx, 1))


                     !k1, lvec_k1(1:tst(ii)%tnidx, 1), rvec_k1(1:tst(ii)%tnidx, 1))
                     ! call la_biorth_olenkifer(.true., lvec_k1(1:tst(ii)%tnidx, :), rvec_k1(1:tst(ii)%tnidx, :), 0)
                     do j = 1, order
                        do jj = 1, 4
                           kk = 1
                           if (uexci_sum(jj, j).ne.0)then
                           do k2 = tst(jj)%tuexci(1, j), tst(jj)%tuexci(2, j)


                              call readconverged_nonsymmetric_single_right(tst(jj)%trecs(j), &
                                    k2, rvec_k2(1:tst(jj)%tnidx, 1))


                              !k2, lvec_k2(1:tst(jj)%tnidx, 1), rvec_k2(1:tst(jj)%tnidx, 1))
                              ! call la_biorth_olenkifer(.true., lvec_k2(1:tst(jj)%tnidx, :), rvec_k2(1:tst(jj)%tnidx, :), 0)
                              print*, 'tst(jj)%tnidx', tst(ii)%tnidx, tst(jj)%tnidx
                              call msg('COMPUTING OVERLAP TERM KK...')
                              call clock_start(time)
                              call exc_exc_over(method, exc_exc_over_kk, t1, t2, &
                                   s1, s2, nocc, nactive, rvec_k1(1:tst(ii)%tnidx, 1), rvec_k1(1:tst(ii)%tnidx, 1), ii)
                              call dmsg("TOTAL TIME - OVERLAP KK", clock_readwall(time))
                              call clock_start(time)
                              print*, j, jj, tst(jj)%tnidx, tst(ii)%tnidx
                              call msg('COMPUTING OVERLAP TERM LL...')
                              call exc_exc_over(method, exc_exc_over_ll, t1, t2, &
                                   s1, s2, nocc, nactive, rvec_k2(1:tst(jj)%tnidx, 1), rvec_k2(1:tst(jj)%tnidx, 1), jj)
                              print*, '-----', i, ii, k1, j, jj, k2
                              call dmsg("TOTAL TIME - OVERLAP LL", clock_readwall(time))
                              over_xx(dm_xx_order, 1) = exc_exc_over_kk
                              over_xx(dm_xx_order, 2) = exc_exc_over_ll
                              dm_xx_order = dm_xx_order + 1
                              print*, 'exc_exc_over', exc_exc_over_kk, exc_exc_over_ll
                           end do
                     end if
                        end do
                     end do
                  end do
            end if
               end do
            end do

            do pt = 0, MAXPT !**

               print*, 'RZAD MBPT', pt
               dm_xx_order = 1
               do i = 1, order
                  do ii = 1, 4
                        if (lexci_sum(ii, i).ne.0)then
                              do k1 = tst(ii)%tlexci(1, i), tst(ii)%tlexci(2, i)
                                    call readconverged_nonsymmetric_single_right(tst(ii)%trecs(i), &
                                          k1, rvec_k1(1:tst(ii)%tnidx, 1))
                                    print*, 'rveck1 - sym', i, 'zest', ii, 'stan', k1
                                    kl: do iii = 1, size(rvec_k1, dim=1)
                                          if (abs(rvec_k1(iii, 1)).gt.1.d-5)then
                                                print*, rvec_k1(iii, 1)
                                                exit kl
                                          end if
                                    end do kl
                                    
                        norm_plusz = zero
                        do pp = 1, tst(ii)%tnidx
                              norm_plusz = norm_plusz + rvec_k1(pp, 1)*rvec_k1(pp, 1)
                        end do
                        print*, 'NORM PLUSZ', sqrt(norm_plusz)
                        do j = 1, order
                           do jj = 1, 4
                                 if (uexci_sum(jj, j).ne.0)then
                              do k2 = tst(jj)%tuexci(1, j), tst(jj)%tuexci(2, j)
                                 call readconverged_nonsymmetric_single_right(tst(jj)%trecs(j), &
                                       k2, rvec_k2(1:tst(jj)%tnidx, 1))
                                 print*, 'rveck2 - sym',j, 'zest', jj, 'stan', k2
                                 kl2: do iii = 1, size(rvec_k2, dim=1)
                                       if (abs(rvec_k2(iii, 1)).gt.1.d-5)then
                                             print*, rvec_k2(iii, 1)
                                             exit kl2
                                       end if
                                 end do kl2

                                 call clock_start(time)
                                 print*, 'LICZE MOMENT PRZEJSCA MIEDZY STANEM'!, ii, print_rep(i), 'oraz', jj, print_rep(j), k1, k2, dm_xx_order
                                 write(*, '(I2, A6, A6, I4, A5, I2, A6, A6, I4)') ii, print_rep(i), 'stan', k1, '---',jj, print_rep(j), 'stan',k2
                                 !print*, 'tst(ii)%tnidx', tst(ii)%tnidx, tst(jj)%tnidx
                                 call calc_exc_exc(method, mocoeff, overlap, nocc0, &
                                      nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, &
                                      irrep0, irrep1, order, t1, t2, s1, s2, &
                                      rvec_k1(:, 1), rvec_k2(:, 1), i, j, tm_wm, 1, k1, k2, &
                                      super_dm_wm_sym(dm_xx_order, :,:), super_dm_wm_asym(dm_xx_order, :,:), pt, prop)
                                 call dmsg("TOTAL TIME - CALC_EXC_EXC", clock_readwall(time))
                                 call clock_start(time)
                                 if (prop == prop_dip) then
                                       call calc_exc_exc(method, mocoeff, overlap, nocc0, &
                                             nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, &
                                             irrep0, irrep1, order, t1, t2, s1, s2, &
                                             rvec_k2(:, 1), rvec_k1(:, 1), j, i, tm_um, 1, k2, k1, &
                                             super_dm_um_sym(dm_xx_order, :,:), super_dm_um_asym(dm_xx_order, :,:), pt, prop)

                                 end if

                                 exc_exc_over_kk = over_xx(dm_xx_order, 1)
                                 exc_exc_over_ll = over_xx(dm_xx_order, 2)

                                 print*, '<kap(r1) | eta(r1)>, <kap(r2)|eta(r2)>', exc_exc_over_kk, exc_exc_over_ll
                                 norm_factor = one / sqrt((exc_exc_over_kk * exc_exc_over_ll))
                                 print*, 'norm factor', norm_factor
                                 print*, 'stan', k1, 'z symetrii', i, 'i stan', k2, 'z symetrii', j

                                 super_tm(dm_xx_order, 1) = tm_wm * norm_factor
                                 super_tm(dm_xx_order, 2) = tm_um * norm_factor
                                 print*, 'MOMENT PRZEJSCIA POLICZONY'
                                 print*, 'tm_wm:', tm_wm, 'tm_um:', tm_um
                                 print*, 'norm_factor', norm_factor
         write(*,'(2I4, A5, F20.15, 2I4, A5, F20.15)')ii, k1, print_rep(i), tst(ii)%twr_exc(k1, i), jj, k2, print_rep(j), tst(jj)%twr_exc(k2, j)
                                 tm_ee = norm_factor ** 2 * tm_wm * tm_um
                                 print*, 'PT=', pt,'miedzy stanami ss', k1, 'z',  i, 'oraz', k2, 'z', j, tm_wm
                                 dm_xx_order = dm_xx_order + 1
                                 
                              end do
                        end if
                           end do
                        end do
                     end do
               end if
                  end do
               end do
            end do

            call excexc_table_start2()
            
            dm_xx_order = 1!**
            do i = 1, order
               do ii = 1, 4
                     if (lexci_sum(ii, i).ne.0)then
                           if (ii == 1 .or. ii == 2)then
                                 mli = 1
                           else
                                 mli = 3
                           end if
                  do k1 = tst(ii)%tlexci(1, i), tst(ii)%tlexci(2, i)
                     do j = 1, order
                        do jj = 1, 4
                              if (uexci_sum(jj, j).ne.0)then
                                    if (jj == 1 .or. jj == 2)then
                                          mlj = 1
                                    else
                                          mlj = 3
                                    end if
                           do k2 = tst(jj)%tuexci(1, j), tst(jj)%tuexci(2, j)
                              
                              call excexc_table_continue2(print_rep_mult(i, mli), print_rep_mult(j, mlj), &
                                   k1, k2, tst(ii)%twr_exc(k1, i), tst(jj)%twr_exc(k2, j),&
                                   super_tm(dm_xx_order, 1), super_tm(dm_xx_order, 2), one, &
                                   one)     
                              dm_xx_order = dm_xx_order + 1
                           end do
                     end if
                        end do
                     end do
                  end do
            end if
               end do
            end do
            call midrule(width=128)

            if (allocated(dm_ss)) deallocate(dm_ss)
            if (allocated(dm_ds)) deallocate(dm_ds)
            if (allocated(dm_sd)) deallocate(dm_sd)
            if (allocated(dm_dd)) deallocate(dm_dd)

            if (allocated(um_ss)) deallocate(um_ss)
            if (allocated(um_ds)) deallocate(um_ds)
            if (allocated(um_sd)) deallocate(um_sd)
            if (allocated(um_dd)) deallocate(um_dd)

            
            deallocate(lvec_k1)
            deallocate(rvec_k1)
            deallocate(lvec_k2)
            deallocate(rvec_k2)


contains

           function triplet_present(exci_sum)
                  logical :: triplet_present
                  integer, dimension(:,:), intent(in) :: exci_sum
                  integer :: i, ii

                  triplet_present = .false.
                  outer_loop: do i = 1, order
                        if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed)then
                              do ii = 3, 4
                                    if (exci_sum(ii, i).ne.0)then
                                          triplet_present = .true.
                                          exit outer_loop
                                    end if
                              end do
                        else if (cc_multip == cc_mixed_left)then
                              do ii = 1, 2
                                    if (exci_sum(ii, i).ne.0)then
                                          triplet_present = .true.
                                          exit outer_loop
                                    end if
                              end do

                        end if
                  end do outer_loop
            end function triplet_present


      subroutine excexc_table_start2()
            character(len=128) :: line
            !
            ! SYM1 SYM2 ST1 ST2 ENERGY1 ENERGY2 TML TMR TM
            !
            write(line, "(A1, 1X, A5, 1X, A1, 1X, A5, 1X, A1, 1X, A3, 1X, A1, 1X, &
                  A3, 1X, A1, 1X, A12, 1X, A1, 1X, A12, 1X, A1, 1X, A12, 1X, A1, &
                  1X, A12, 1X, A1, 1X, A12, 1X, 1A, 1X, A9, 1X, 1A, 1X, A9, 1X, 1A)")&
                  "|", "SYMM1", "|", "SYMM2", "|", "ST1", "|", "ST2", "|", "ENERGY1", "|",&
                  "ENERGY2", "|", "TML", "|", "TMR", "|", "TM", "|", "%||T1||_1",&
                  "|", "%||T1||_2", "|"

            call midrule(width=128)
            call msg(line)
            call midrule(width=128)
      end subroutine excexc_table_start2

      subroutine excexc_table_continue2(sym1, sym2, st1, st2, e1, e2, tml, tmr, nr11, nr12)
            character(len=128) :: line
            character(5), intent(in)   :: sym1, sym2
            integer, intent(in)   :: st1, st2
            real(F64), intent(in) :: e1, e2
            real(F64), intent(in) :: tml, tmr
            real(F64), intent(in) :: nr11, nr12
            real(F64) :: tm
            tm = tml * tmr
            !
            ! SYM1 SYM2 ST1 ST2 ENERGY1 ENERGY2 TML TMR TM
            !
            write(line, "(A1, 1X, A5, 1X, A1, 1X, A5, 1X, A1, 1X, I3, 1X, A1, 1X, &
                  I3, 1X, A1, 1X, F12.7, 1X, A1, 1X, F12.7, 1X, A1, 1X, F12.7, 1X, A1, &
                  1X, F12.7, 1X, A1, 1X, F12.7, 1X, 1A, 1X, F9.5, 1X, 1A, 1X, F9.5, 1X, 1A)")&
                  "|", sym1, "|", sym2, "|", st1, "|", st2, "|", e1, "|",&
                  e2, "|", tml, "|", tmr, "|", tm, "|", nr11, "|", nr12, "|"

            call msg(line)
            !                  call midrule(width=104)
      end subroutine excexc_table_continue2
            
      subroutine calc_exc_exc(method, mocoeff, overlap, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, &
            irrep0, irrep1, order, t1, t2, s1, s2, rveck1, rveck2, k1, k2, tm_ee, wum, kk1, kk2, dm_wm_sym, dm_wm_asym, pt, prop)

            integer, intent(in) :: method
            real(F64), dimension(:,:), intent(in) :: mocoeff
            real(F64), dimension(:,:), intent(in) :: overlap
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            integer, intent(in) :: nocc, nvirt, nactive
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: order

            real(F64), dimension(:,:), intent(in) :: t1, s1
            real(F64), dimension(:,:,:,:), intent(in) :: t2, s2

            real(F64), dimension(:), intent(in) :: rveck1, rveck2
            integer, intent(in) :: k1, k2, kk1, kk2
            real(F64), intent(out) :: tm_ee
            integer, intent(in) :: wum
            real(F64), dimension(:,:), intent(inout) :: dm_wm_sym
            real(F64), dimension(:,:), intent(inout) :: dm_wm_asym
            integer, intent(in) :: pt
            integer, intent(in) :: prop

            real(F64), dimension(:), allocatable :: tmm, tmm2, tm_non
            real(F64), dimension(:,:), allocatable :: dm_gr
            real(F64) :: exc_exc_overlap
            integer :: a, b, i, j
            integer, parameter :: sym = 1
            integer, parameter :: asym = 2

            call eom_vectors_init(nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt)

            call exc_exc_init(method, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, &
                  t1, t2, s1, s2, rveck1, rveck2, pt)
            print*, 'generate density'

            if (prop == prop_dip) then
                  !print*, 'rveck1shark'
                  !do i = 1, size(rveck1)
                  !      print*, i, rveck1(i)
                  !end do
                  !print*, 'rveck2shark'
                  !do i = 1, size(rveck2)
                  !      print*, i, rveck2(i)
                  !end do
                  call generate_density_exc_exc_wm(rveck1, rveck2, t2, t1, s2, s1, &
                        nocc, nactive, method, dm_wm_sym, k1, k2, irrep0, irrep1, order, pt)
 !                 do i = 1, size(dm_wm_sym, dim=1)
!                        do j = 1, size(dm_wm_sym, dim=2)
                              !if (abs(dm_wm_sym(i, j)).gt.1.d-3)then
  !                                  print*, 'dm_wm_sym', i, j, dm_wm_sym(i, j)
                              !end if
   !                     end do
    !              end do
            else if (prop ==prop_so) then
                  call generate_density_exc_exc_wm(rveck1, rveck2, t2, t1, s2, s1, &
                        nocc, nactive, method, dm_wm_asym, k1, k2, irrep0, irrep1, order, pt)
            end if

            ! if (CC_SPIN_ORBIT) then
            !    call generate_density_exc_exc_wm(rveck1, rveck2, t2, t1, s2, s1, &
            !         nocc, nactive, method, dm_wm, k1, k2, irrep0, irrep1, order, pt, st)
            ! end if

            call exc_exc_free(method, pt)

            allocate(tmm(3))
            allocate(tmm2(3))
            print*, 'ola'

            if (prop == prop_dip) then      
                  call msg('COMPUTING TRANSITION MOMENTS...')
                  call task_transmom_dip_exc_exc(tmm, mocoeff, dm_wm_sym, nocc0, nocc1, nvirt0, nvirt1)
            else if (prop ==prop_so) then
                  call msg('COMPUTING SPIN-ORBIT TRANSITION MOMENTS... ASYMMMMM')
                  call task_transmom_so_exc_exc(tmm, mocoeff, dm_wm_asym, nocc0, nocc1, nvirt0, nvirt1, 2)

            end if
           
!            if (SLATER_BASIS .eqv. .false.)then

  !                 allocate(tm_non(3))
!                   call msg('NONADIABATIC COUPLING...START')
!                   call task_cc_grad_one(tm_non, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, dm_wm, cc_non)
!                   call msg('NACME WYNIK 1')
!                   print*, 'stan', kk1, 'z symetrii', k1, 'i stan', kk2, 'z symetrii', k2
!                   print*, tm_non
                  
!                   call task_cc_grad_one(tm_non, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, dm_wm, 2)
!                   call msg('NACME WYNIK 2')
!                   print*, 'stan', kk1, 'z symetrii', k1, 'i stan', kk2, 'z symetrii', k2
!                   print*, tm_non
                  
!                   deallocate(tm_non)
! ! 
    !        end if


           ! call msg('COMPUTING SPIN-ORBIT TRANSITION MOMENTS... SYMMMMM')
           ! call task_transmom_so_exc_exc(tmm, mocoeff, dm_wm_sym, nocc0, nocc1, nvirt0, nvirt1, 1)

            !call msg('COMPUTING SPIN-ORBIT TRANSITION MOMENTS... ASYMMMMM')
            !call task_transmom_so_exc_exc(tmm, mocoeff, dm_wm_asym, nocc0, nocc1, nvirt0, nvirt1, 2)


            ! allocate(dm_gr(CC_NORB, CC_NORB))
            ! call msg('COMPUTING GROUND DENSITY TERM...')

            ! call ground_dm(method, dm_gr, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
            !       nactive, t1, t2, s1, s2, rveck1, rveck2, irrep0, irrep1, order)

            ! call msg('DONE')

            ! call dipmom(dm_gr, overlap, tmm2)

            ! print*, 'tmm2', tmm2 

            ! call msg('COMPUTING OVERLAP TERM KL...')

            ! call exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rveck1, rveck2)

            ! call msg('DONE')

            ! print*, 'exc_exc_overlap KL:', exc_exc_overlap 

            ! tmm = tmm - tmm2 * exc_exc_overlap 

            call task_line_strength(tmm, tm_ee)

            ! print*, 'tm_ee without normalization =', tm_ee
            
            deallocate(tmm)
!            deallocate(dm_wm)
!            deallocate(dm_gr)

      end subroutine calc_exc_exc

      subroutine exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvl, rvr, i)

            integer, intent(in) :: method
            real(F64), intent(out) :: exc_exc_overlap
            real(F64), dimension(:,:), intent(in) :: t1, s1
            real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(:), intent(in) :: rvl, rvr
            type(tclock)                          :: time
            integer, intent(in) :: i
            integer :: multip

            if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed) then
                  if (i == 1 .or. i == 2) then
                        multip = 1
                  else if (i == 3 .or. i == 4) then
                        multip = 3
                  end if
            else if (cc_multip == cc_mixed_left) then
                  if (i == 3 .or. i == 4) then
                        multip = 1
                  else if (i == 1 .or. i == 2) then
                        multip = 3
                  end if
                  
            end if
            
            if (multip == cc_singlet) then
                  call clock_start(time)      
                  call  d_overlap_ccsd_init(nocc, nactive)
                  call d_overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  call dmsg("TOTAL TIME - OVERLAP INTERMEDIATES CCSD", clock_readwall(time))
                  call clock_start(time)
                  exc_exc_overlap =  d_overlap_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  call dmsg("TOTAL TIME - OVERLAP FUNCTION CCSD", clock_readwall(time))
                  call d_overlap_ccsd_free()

                  ! if (method .eq. THEORY_CC3) then
                  !       call clock_start(time)
                  !       call d_overlap_cc3_init(nocc, nactive)
                  !       call d_overlap_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  !       call dmsg("TOTAL TIME - OVERLAP INTERMEDIATES CC3", clock_readwall(time))
                  !       call clock_start(time)
                  !       exc_exc_overlap =  exc_exc_overlap + d_overlap_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  !       call dmsg("TOTAL TIME - OVERLAP FUNCTION CC3", clock_readwall(time))

                  !       call d_overlap_cc3_free()
                  ! end if
                        
            else if (multip == cc_triplet) then

                        call d_overlap_triplet_ccsd_init(nocc, nactive)
                        call d_overlap_intermediates_triplet_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                        exc_exc_overlap =  d_overlap_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                        call d_overlap_triplet_ccsd_free()
                  ! if (method .eq. THEORY_CC3) then
                  !       call d_overlap_triplet_cc3_init(nocc, nactive)
                  !       call d_overlap_intermediates_triplet_cc3(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  !       exc_exc_overlap =  exc_exc_overlap + d_overlap_cc3_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  !       call d_overlap_triplet_cc3_free()
                  ! end if
            end if

      end subroutine exc_exc_over


     !  subroutine exc_exc_over(method, exc_exc_overlap, t1, t2, s1, s2, nocc, nactive, rvl, rvr)

     !        integer, intent(in) :: method
     !        real(F64), intent(out) :: exc_exc_overlap
     !        real(F64), dimension(:,:), intent(in) :: t1, s1
     !        real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
     !        integer, intent(in) :: nocc, nactive
     !        real(F64), dimension(:), intent(in) :: rvl, rvr

     !        if (cc_multip == cc_singlet) then
     !              call overlap_ccsd_init(nocc, nactive)

     !              call overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr)

     !              exc_exc_overlap = overlap_f(t2, t1, s2, s1, nocc, nactive, rvl, rvr)

     !              call overlap_ccsd_free()
     !        else if (cc_multip == cc_triplet) then
     !              call overlap_ccsd_triplet_init(nocc, nactive)
                  
     !              call overlap_intermediates_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  
     !              exc_exc_overlap = overlap_f_triplet(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
                  
     !              call overlap_ccsd_triplet_free()
     !        end if

     ! end subroutine exc_exc_over

      subroutine ground_dm(method, dm_gr, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
            nactive, t1, t2, s1, s2, rvl, rvr, irrep0, irrep1, order)

            integer, intent(in) :: method
            real(F64), dimension(:,:), intent(out) :: dm_gr
            real(F64), dimension(:,:), intent(in) :: mocoeff
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt
            integer, intent(in) :: nactive
            real(F64), dimension(:,:), intent(in) :: t1, s1
            real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
            real(F64), dimension(:), intent(in) :: rvl, rvr
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: order

            real(F64), dimension(:,:), allocatable :: dmmo, dmao
            integer, parameter :: mbpt = 4
            integer :: i, j

            allocate(dmmo(nactive, nactive))
            allocate(dmao(CC_NORB, CC_NORB))
            call density_ground_init(nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
                  dmmo)

            call msg("GENERATING ONE-ELECTRON DENSITY MATRIX GROUND...")
            call generate_density_matrix_ground(t2, t1, s2, s1, &
                  nocc, nactive, method, dmmo, REP_Ag, irrep0, irrep1, order, mbpt)

            call dmmo_to_dmao(dmao, dmmo, mocoeff, nactive)

            call dmao_to_dmao_sym(dmao, dm_gr)

            deallocate(dmmo)
            deallocate(dmao)

      end subroutine ground_dm



            ! subroutine gr_over(method, dm_gr, exc_exc_overlap, mocoeff, overlap, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
            !       nactive, t1, t2, s1, s2, rvl, rvr, irrep0, irrep1, order)

            !       integer, intent(in) :: method
            !       real(F64), dimension(:,:), intent(out) :: dm_gr
            !       real(F64), intent(out) :: exc_exc_overlap
            !       real(F64), dimension(:,:), intent(in) :: mocoeff
            !       real(F64), dimension(:,:), intent(in) :: overlap
            !       integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt
            !       integer, intent(in) :: nactive
            !       real(F64), dimension(:,:), intent(in) :: t1, s1
            !       real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
            !       real(F64), dimension(:), intent(in) :: rvl, rvr
            !       integer, dimension(:,:), intent(in) :: irrep0
            !       integer, dimension(:,:), intent(in) :: irrep1
            !       integer, intent(in) :: order

            !       real(F64), dimension(:,:), allocatable :: dmmo, dmao
            !       integer, parameter :: mbpt = 4
            !       integer :: i, j

            !       ! call overlap_ccsd_init(nocc, nactive)

            !       ! call overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rvl, rvr) 
                  
            !       ! print*, 'overlap'
            !       ! exc_exc_overlap = overlap_f(t2, t1, s2, s1, nocc, nactive, rvl, rvr)
            !       ! print*, 'koniec overlap'

            !       ! call overlap_ccsd_free()

            !       allocate(dmmo(nactive, nactive))
            !       allocate(dmao(CC_NORB, CC_NORB))
            !       call density_ground_init(nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
            !             dmmo)

            !       call msg("GENERATING ONE-ELECTRON DENSITY MATRIX GROUND...")
            !       call generate_density_matrix_ground(t2, t1, s2, s1, &
            !             nocc, nactive, method, dmmo, REP_Ag, irrep0, irrep1, order, mbpt)

            !       call dmmo_to_dmao(dmao, dmmo, mocoeff, nactive)

            !       call dmao_to_dmao_sym(dmao, dm_gr)                   

            ! end subroutine gr_over

      subroutine compute_density_driver(i, gaxi_comp, ga, xi, method, mocoeff, nocc0, &
                  nocc1, nvirt0, nvirt1, nocc, nvirt, &
                  nactive, nidx, irrep0, irrep1, order, lvec, rvec, t2, t1, s2, s1)

                  integer, intent(in) :: i
                  integer, intent(inout) :: gaxi_comp
                  real(F64), intent(inout) :: ga, xi
                  integer, intent(in) :: method
                  real(F64), dimension(:,:), intent(in) :: mocoeff
                  integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
                  integer, intent(in) :: nocc, nvirt, nactive, nidx
                  integer, dimension(:, :), intent(in) :: irrep0, irrep1
                  integer, intent(in) :: order
                  real(F64), dimension(:), intent(in) :: lvec, rvec
                  real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
                  real(F64), dimension(:,:), intent(in) :: t1, s1
                  type(tclock)                          :: time
 
                  call clock_start(time)
                  if (gaxi_comp .eq. 0 )then
                        call msg('COMPUTING DENSITY MATRIX - TRANSITION GROUND EXC')
                        call compute_density(i, method, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
                              nactive, nidx, irrep0, irrep1, order, lvec, rvec, ga, xi, t2, t1, s2, s1)
                        gaxi_comp = 1
                  else
                        print*, ''
                        print*, 'JUZ BYLO LICZONE, WIEC NIE LICZE GROUND EXC'
                        print*, ''
                  end if
                  call dmsg("TOTAL TIME - DENSITY - DRIVER", clock_readwall(time))                                    


            end subroutine compute_density_driver

            subroutine compute_density(i, method, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
                  nactive, nidx, irrep0, irrep1, order, lvec, rvec, tm_sl, tm_sr, t2, t1, s2, s1)

                  integer, intent(in) :: i
                  integer, intent(in) :: method
                  real(F64), dimension(:,:), intent(in) :: mocoeff
                  integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
                  integer, intent(in) :: nocc, nvirt, nactive, nidx
                  integer, dimension(:,:), intent(in) :: irrep0, irrep1
                  integer, intent(in) :: order
                  real(F64), dimension(:), intent(in) :: lvec, rvec
                  real(F64), intent(out) :: tm_sl, tm_sr
                  real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
                  real(F64), dimension(:,:), intent(in) :: t1, s1


                  real(F64), dimension(:,:), allocatable :: dm_gamma, dm_xi
                  real(F64), dimension(:), allocatable :: tml, tmr
                  real(F64) :: tmxx, tmyy, tmzz

                  integer :: j, k

                  type(tclock)                          :: time
                  

                  allocate(dm_gamma(CC_NORB, CC_NORB))
                  allocate(dm_xi(CC_NORB, CC_NORB))
                  dm_gamma = ZERO
                  dm_xi =  ZERO

                  call clock_start(time)
                  ! do j = 1, size(lvec, dim=1)
                  !       if (abs(lvec(j)) .gt. 1.d-8)then
                  !             print*, j, ',', lvec(j)
                  !       end if
                  ! end do

                  call generate_density_gr_exc(lvec, rvec, t2, t1, s2, s1, &
                        nocc, nactive, method, dm_gamma, dm_xi, i, irrep0, irrep1, order) 
                  call dmsg("TOTAL TIME:", clock_readwall(time))
                  
                  
                  allocate(tml(3))
                  allocate(tmr(3))

                  call imsg('COMPUTING TRANSITION MOMENTS GAMMA for', i)
                  call task_transmom_dq_exc_exc(tml, mocoeff, dm_gamma, nocc0, nocc1, nvirt0, nvirt1)

                  call imsg('COMPUTIG TRANSITION MOMENTS XI for', i)

                  call task_transmom_dq_exc_exc(tmr, mocoeff, dm_xi, nocc0, nocc1, nvirt0, nvirt1)
                  
                  print*, 'tml', tml
                  print*, 'tmr', tmr
                  call task_line_strength(tml, tm_sl)
                  call task_line_strength(tmr, tm_sr)

                  print*, 'tmsl, tmsr', tm_sl, tm_sr


                  deallocate(tml)
                  deallocate(tmr)

                  deallocate(dm_gamma)
                  deallocate(dm_xi)

            end subroutine compute_density

      end subroutine compute_transitions_exc_exc

      function titleheader(method, multip)
            character(:), allocatable :: titleheader
            integer, intent(in) :: method
            integer, intent(in) :: multip

            if (multip == cc_singlet) then
                  select case(method)
                  case (THEORY_CCSD)
                        titleheader =  "eom_sing_ccsd_"
                  case (THEORY_CC3)
                        titleheader =  "eom_sing_cc3_"
                  end select
            else if (multip == cc_triplet) then
                  select case(method)
                  case (THEORY_CCSD)
                        titleheader =  "eom_trip_ccsd_"
                  case (THEORY_CC3)
                        titleheader =  "eom_trip_cc3_"
                  end select
            end if      
      end function titleheader


      subroutine eom_write_aux(method, convrecs_s, convrecs_d, order, iexci_s, iexci_d)
            integer, intent(in)                           :: method
            type(trecgroup), dimension(:), intent(inout)  :: convrecs_s, convrecs_d
            integer, intent(in)                           :: order
            integer, dimension(:), intent(in)           :: iexci_s, iexci_d

            character(:), allocatable :: filename
            character(:), allocatable :: title
            character(:), allocatable :: linkname
            integer :: nft1
            integer :: info
            integer :: i, k1
            character(:), allocatable :: titleh

            titleh = titleheader(method, cc_multip)

            
            do i = 1, order
                  if (iexci_s(i) .ne. 0) then
                        do k1 = 1, iexci_s(i)
                              title = titleh//'s_'//str(i)//'_'//str(k1)
                              filename = io_record_path_to_binary(convrecs_s(i), k1, info)
                              
                              linkname = io_record_path_to_link(title, info) 
                              if (io_exists(linkname)) then
                                    open(newunit=nft1, file=linkname, access='stream', form='unformatted', status='old')
                              else
                                    open(newunit=nft1, file=linkname, access='stream', form='unformatted', status='new')
                              end if
                              print*, filename, 'filename'
                              write(nft1)filename
                              close(nft1)

                        end do
                  end if

                 if (iexci_d(i) .ne. 0) then
                       do k1 = 1, iexci_d(i)
                              title = titleh//'d_'//str(i)//'_'//str(k1)
                              filename = io_record_path_to_binary(convrecs_d(i), k1, info)


                              linkname = io_record_path_to_link(title, info)
                              if (io_exists(linkname)) then
                                    open(newunit=nft1, file=linkname, access='stream', form='unformatted', status='old')
                              else
                                    open(newunit=nft1, file=linkname, access='stream', form='unformatted', status='new')
                              end if
                              write(nft1)filename
                              close(nft1)
                              end do
                  end if


            end do


      end subroutine eom_write_aux



end module drv_cc_aux
