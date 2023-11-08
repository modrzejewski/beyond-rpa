module ccsd
      use gparam
      use cmpidx
      use linalg
      use ccsd_transformed_integrals
      use t1_transformed_int
      use cc3_intermediates
!      use cc3_intermediates_for_21
      use ccsd_intermediates
      use density_matrix_gr_exc
      use density_matrix_exc_exc
      use sort
      use common_sub
      use cc_diis
      use jacobian
      use jacobian_d_small
      use multipole
      use spin_orbit_ecp
      use eomccsdprop 
      use ci
      use davidson_main
      use clock
      use symmetry
      use math_constants
      use automatic_t1_t2_amplitudes
      use scf_symm_driver
      use cc_gparams
      use slater_parser

      implicit none
      save

      integer                                        :: DIIS_T1T2_DIM     
      integer                                        :: DIIS_T1_START
      integer                                        :: DIIS_T2_START
      integer, parameter                             :: cc_sp_start = 1
      integer, parameter                             :: cc_sp_iter  = 2

      integer, dimension(:, :), allocatable :: gisingles
      integer, dimension(:, :), allocatable :: gidoubles
      integer, dimension(:, :), allocatable :: gitriples
      integer, dimension(:, :), allocatable :: gitd1, gitd2, gitd3
      integer, dimension(:, :), allocatable :: gitd1r, gitd2r, gitd3r
      integer, dimension(:, :), allocatable :: gitd1_idx, gitd2_idx, gitd3_idx
      integer, dimension(:, :), allocatable :: gitd1r_idx, gitd2r_idx, gitd3r_idx

      integer :: gidims
      integer :: gidimd
      integer :: gidimt
      integer :: gitd1_dim, gitd2_dim, gitd3_dim
      integer :: gitd1r_dim, gitd2r_dim, gitd3r_dim
      integer, dimension(:,:), allocatable :: ggirrep0, ggirrep1

      integer, parameter :: TRANS_DIP = 1
      integer, parameter :: TRANS_QUAD = 2
      integer, parameter :: TRANS_NON = 3
      integer, parameter :: TRANS_ONEAO = 4
      integer, parameter :: TRANS_ONEMO = 5


contains

      subroutine ccsd_init(mocoeff, nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1, eorb, &
            irrep0, irrep1)
            double precision, dimension(:,:), intent(in)     :: mocoeff
            integer, intent(in)                              :: nocc, nvirt
            integer, intent(in)                              :: nactive
            integer, intent(in)                              :: nocc0, nocc1
            integer, intent(in)                              :: nvirt0, nvirt1
            double precision, dimension(:), intent(in)       :: eorb
            integer, dimension(:, :), intent(in)             :: irrep0
            integer, dimension(:, :), intent(in)             :: irrep1
            integer :: full_sym
            integer :: order
            real(F64) :: time1, time2
            type(tclock) :: time


            call transformed_integrals_init(nocc, nactive)

            !$ time1 = omp_get_wtime()
            call clock_start(time)
            print*, 'transformed integrals...'
            call transformed_integrals(mocoeff, nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1)
            call dmsg("TOTAL TIME na transformed", clock_readwall(time))

            !$ time2 = omp_get_wtime()
!            print*, 'czas na transformed bez t1', time2 - time1

            print*, 'nocccc', nocc0, nocc1, nvirt0, nvirt1

            if(abs(eorb(nocc+1)-eorb(nocc)).lt.CC_MIN_HLGAP)then
                  CC_ETA = -(CC_MIN_HLGAP - abs(eorb(nocc+1)-eorb(nocc)))
            else
                  CC_ETA = ZERO
            end if

            if (POINT_GROUP == D2h) then
                  full_sym = REP_Ag
                  order = D2h_order
            else if (POINT_GROUP == C2v) then
                  full_sym = REP_A1
                  order = C2v_order
            end if


            call irrep_singless(full_sym, irrep0, irrep1, gisingles, POINT_GROUP, gidims, .true.)
            allocate(gisingles(2, gidims))
            call irrep_singless(full_sym, irrep0, irrep1, gisingles, POINT_GROUP, gidims, .false.)

            call irrep_doubless(full_sym, irrep0, irrep1, 1, &
                  nocc, nocc+1, gidoubles, POINT_GROUP, gidimd, .true., .false.)
            allocate(gidoubles(4, gidimd))
            call irrep_doubless(full_sym, irrep0, irrep1, 1, &
                  nocc, nocc+1, gidoubles, POINT_GROUP, gidimd, .false., .false.)

            call irrep_triples(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples, POINT_GROUP, gidimt, .true., .false.)
            allocate(gitriples(6, gidimt))
            call irrep_triples(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  gitriples, POINT_GROUP, gidimt, .false., .false.)

            ! call irrep_triples_doubles(full_sym, gidoubles, gidimd, irrep0, irrep1, &
            !       nocc0, nocc, nvirt0, &
            !       gitd1, gitd1_dim, gitd2, gitd2_dim, gitd3, gitd3_dim, &
            !       gitd1_idx, gitd2_idx, gitd3_idx, rev = .false.)

            ! call irrep_triples_doubles(full_sym, gidoubles, gidimd, irrep0, irrep1, &
            !       nocc0, nocc, nvirt0, &
            !       gitd1r, gitd1r_dim, gitd2r, gitd2r_dim, gitd3r, gitd3r_dim, &
            !       gitd1r_idx, gitd2r_idx, gitd3r_idx, rev = .true.)

            allocate(ggirrep0(2, order))
            allocate(ggirrep1(2, order))

            ggirrep0 = irrep0
            ggirrep1 = irrep1

      end subroutine ccsd_init

      subroutine cc_free(method)
            integer, intent(in)                                   :: method
            call ccsd_free()
            if(method .eq. THEORY_CC3) call cc3_free()
      end subroutine cc_free

      subroutine ccsd_free()

            call transformed_integrals_free()
            if (allocated(ggirrep0)) deallocate(ggirrep0)
            if (allocated(ggirrep1))deallocate(ggirrep1)
            if (allocated(gidoubles))deallocate(gidoubles)
            if (allocated(gisingles))deallocate(gisingles)
            if (allocated(gitriples))deallocate(gitriples)
            
      end subroutine ccsd_free


      subroutine task_cc(method, mocoeff, eorb, erhf_gauss, nocc, nvirt,&
            nactive, t2new, t1new, nocc0, nocc1, nvirt0, nvirt1)
            integer, intent(in)                                   :: method
            double precision, dimension(:,:), intent(in)          :: mocoeff
            double precision, dimension(:), intent(in)            :: eorb
            double precision, intent(in)                          :: erhf_gauss
            integer, intent(in)                                   :: nocc, nvirt
            integer, intent(in)                                   :: nactive
            double precision, dimension(nocc+1:nactive,&
                  nocc+1:nactive, nocc, nocc), intent(out)        :: t2new
            double precision, dimension(nocc+1:nactive,&
                  nocc), intent(out)                              :: t1new
            double precision, dimension(:), allocatable           :: t1t2vec, t1t2vecold
            integer, intent(in)                                   :: nocc0, nocc1
            integer, intent(in)                                   :: nvirt0, nvirt1

            double precision, dimension(:,:), allocatable         :: t1old
            double precision, dimension(:,:,:,:), allocatable     :: t2old
            double precision                                      :: cc_energy_old
            double precision                                      :: cc_energy, cc_energy_licz
            double precision                                      :: ediff
            double precision                                      :: ampltdiff

            integer                                               :: a, b, i, j
            integer                                               :: iter
            integer                                               :: npair
            logical                                               :: q
            double precision                                      :: time0, time1
            real(F64)                                             :: ttt1, ttt2
            double precision                                      :: time0_total, time1_total
            double precision                                      :: timetot
            double precision, parameter                           :: eps = 1.d-7
            integer :: alloc_size
            !                                                                                                                                      
            ! DIIS parameters                                                                                              
            !                                                                                                                                                

            double precision, dimension(:,:), allocatable         :: diis_P
            double precision, dimension(:), allocatable           :: diis_coeff
            integer, dimension(:), allocatable                    :: diis_idx
            integer                                               :: diis_iter, di
            integer                                               :: l, noflindep, diis_n
            double precision                                      :: t1_dg
            real(F64) :: erhf
            integer, parameter :: order = 8
            real(F64) :: emp2
            character(10) :: time2 
            double precision, dimension(:, :), allocatable :: soxao, soyao, sozao, quadzzao, quadzzmo
            double precision, dimension(:, :), allocatable :: soxmo, soymo, sozmo
            integer :: nft3, l1, l2

!             allocate(quadzzao(CC_NORB, CC_NORB))

!            allocate(quadzzmo(CC_NORB, CC_NORB))
!            call msg ("FROM TRANSMOM_Quad CCSD.f90: SLATER=TRUE")
!            l1 = CC_NORB
!            l2 = l1 * (l1 + 1) / 2
!            open(newunit=nft3, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
!            call read_lower_triangle(nft3, l2, 9, quadzzao)
!            close(nft3)
! !           call smfill(quadzzao)
!            print*, 'quadzzao'
!            do i = 1, CC_NORB
!                  do j = 1, CC_NORB
!                        if (abs(quadzzao(i, j)).gt.1.d+3)then
!                              print*, i, j, quadzzao(i, j)
!                        end if
!                  end do
!            end do
!            stop
!            print*, 'smfillllll'
!            call smfill(quadzzao)                                                                                                                                      
!            print*, 'quadzzao'
!            do i = 1, CC_NORB
!                  do j = 1, CC_NORB
!                        if (abs(quadzzao(i, j)).gt.1.d-2)then
!                              print*, i, j, quadzzao(i, j)
!                        end if
!                  end do
!            end do

!            stop
           !call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
           !call tm_part(tm_wm(3), quadzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
           !print*, 'zz', tm_wm(3)

            ! allocate(soxao(174, 174)) 
            ! allocate(soyao(174, 174))
            ! allocate(sozao(174, 174))
            ! allocate(soxmo(174, 174))
            ! allocate(soymo(174, 174))
            ! allocate(sozmo(174, 174))
           
            ! l1 = 174
            ! l2 = l1 * (l1 + 1) / 2
            ! print*,'read, soxao', l1, l2, 174
            ! open(newunit=nft3, file=SLATER_FILE_1E,  access='stream',  form='unformatted', status='old')
            ! call read_lower_triangle(nft3, l2,35, soxao)
            ! close(nft3)

            ! open(newunit=nft3, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
            ! call read_lower_lower_triangle(nft3, l2, 36, soyao)
            ! close(nft3)
            
            ! ! open(newunit=nft3, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
            ! ! call read_lower_lower_triangle(nft3, l2, 37, sozao)
            ! ! close(nft3)
            
            ! print*, 'CZYTAM ASYMASYMAYSM'
            ! call amfill(soxao)
            ! call amfill(soyao)
            ! call amfill(sozao)
            
          !   call atbc3(soxmo, mocoeff, soxao, mocoeff, 174, 174, 174)
          !   call atbc3(soymo, mocoeff, soyao, mocoeff, 174, 174, 174)
          !   call atbc3(sozmo, mocoeff, sozao, mocoeff, 174, 174, 174)

          !  print*, 'macierz so-x'
          !      do i = 1, size(soxmo, dim=1)
          !         do j = 1, size(soxmo, dim=2)
          !               if (abs(soxmo(i,j)).gt.1.d-4)then
          !                     print*, i, j, soxmo(i, j)
          !               end if
          !         end do
          !   end do

          !   print*, 'macierz so-y'
          !   do i = 1, size(soymo, dim=1)
          !         do j = 1, size(soymo, dim=2)
          !               if (abs(soymo(i,j)).gt.1.d-4)then
          !                     print*, i, j, soymo(i, j)
          !               end if
          !         end do
          !   end do

          !       print*, 'macierz so-z'
          !   do i = 1, size(sozmo, dim=1)
          !         do j = 1, size(sozmo, dim=2)
          !               if (abs(sozmo(i,j)).gt.1.d-8)then
          !                     print*, i, j, sozmo(i, j)
          !               end if
          !         end do
          !   end do

          ! print*, 'macierz so-x-wybrane1'
          ! do i = 3, 3
          !       do j = 123, 148
          !             if (abs(soxmo(i,j)).gt.1.d-4)then
          !                   print*, i, j, soxmo(i, j)
          !             end if
          !       end do
          ! end do


          ! print*, 'macierz so-x-wybrane2'
          ! do i = 97, 122
          !       do j = 123, 148
          !             if (abs(soxmo(i,j)).gt.1.d-4)then
          !                   print*, i, j, soxmo(i, j)
          !             end if
          !       end do
          ! end do


          ! print*, 'macierz so-y-wybrane'
          !   do i = 97, 122
          !       do j = 149, 174
          !             if (abs(soxmo(i,j)).gt.1.d-4)then
          !                   print*, i, j, soxmo(i, j)
          !             end if
          !       end do
          ! end do

            

            ! print*, 'macierz soxao-plusz'
            ! do i = 1, size(soxao, dim=1)
            !       do j = 1, size(soxao, dim=2)
            !             if (abs(soxao(i,j)).gt.1.d-8)then
            !                   print*, i, j, soxao(i, j), soxao(j, i)
            !             end if
            !       end do
            ! end do

            ! print*, 'macierz soyao-plusz'
            ! do i = 1, size(soyao, dim=1)
            !       do j = 1, size(soyao, dim=2)
            !             if (abs(soyao(i,j)).gt.1.d-8)then
            !                   print*, i, j, soyao(i, j), soyao(j, i)
            !             end if
            !       end do
            ! end do

           ! print*, 'macierz sozao'
           !  do i = 1, size(sozao, dim=1)
           !        do j = 1, size(sozao, dim=2)
           !              if (abs(sozao(i,j)).gt.1.d-8)then
           !                    print*, i, j, sozao(i, j)
           !              end if
           !        end do
           !  end do


           !   deallocate(soxao)
           !   deallocate(soyao)
           !   deallocate(sozao)


            !                                                                                
            ! CC PREAMBLE                                                                                                
            !                                                                                                                                                                          
            if(method .eq. THEORY_CCSD)then
                  call toprule()
                  call msg('CCSD MODULE')
                  call midrule()
                  call smsg("APPROXIMATE METHOD", 'CCSD')
                  call dmsg("AMPLITUDE CONVERGENCE",CC_AMP_THRESH , fmt="ES10.1")
            else if(method .eq. THEORY_CC3)then
                  call t_equation_cc3_init(nocc0, nvirt0, nocc, nactive)
                  call toprule()
                  call msg('CC3 MODULE')
                  call midrule()
                  call smsg("APPROXIMATE METHOD", 'CC3')
                  call dmsg("AMPLITUDE CONVERGENCE",eps , fmt="ES10.1")
            end if

            if (SLATER_BASIS) then
                  erhf = erhf_slater(eorb, mocoeff, nocc0, nocc1, nvirt0, nvirt1)
            else
                  erhf = erhf_gauss
            end if

            !                                                                                                                            
            ! Basic parameters
            !                                                                                                                                           
            npair = nocc * nvirt
            DIIS_T1T2_DIM = npair * (1 + npair)
            DIIS_T1_START = 1
            DIIS_T2_START = npair + 1
            !                                                                                                                                                   
            ! Allocate t1, t2, diis_P, diis_coeff                                                                                    
            !                                                                                                                                       
            allocate(t2old(nocc+1:nactive, nocc+1:nactive, nocc, nocc))
            allocate(t1old(nocc+1:nactive, nocc))
            allocate(diis_P(DIIS_T1T2_DIM, CC_DIIS_NMAX))
            allocate(t1t2vec(DIIS_T1T2_DIM))
            allocate(t1t2vecold(DIIS_T1T2_DIM))
            allocate(diis_coeff(CC_DIIS_NMAX))
            allocate(diis_idx(CC_DIIS_NMAX))

            alloc_size = (nactive-nocc)**2*nocc**2 + (nactive-nocc)*nocc+DIIS_T1T2_DIM*CC_DIIS_NMAX+&
                  DIIS_T1T2_DIM*2 + CC_DIIS_NMAX*2

            diis_P = ZERO
            t1old = ZERO
            t2old = ZERO
            diis_coeff = ZERO
            do i = 1, CC_DIIS_NMAX
                  diis_idx(i) = i
            end do

            iter = 0
            q = .false.
            call diis_init(CC_DIIS_NMAX, DIIS_T1T2_DIM)

            emp2 = zero
            do i = 1, nocc
                  do j = 1, nocc
                        do a = nocc + 1, nactive
                              do b = nocc + 1, nactive
                                    emp2 = emp2 + (two * vovo(a, i, b, j) - vovo(b, i, a, j)) * vovo(a, i, b, j) &
                                          / (eorb(i) + eorb(j) - eorb(a) - eorb(b))
                              end do
                        end do
                  end do
            end do
            print*, emp2+erhf, emp2, erhf

            print*, 'starting tequation'
            call tequation_driver(eorb, nocc, nvirt, t2old, t1old, &
                  t2new, t1new, method, cc_sp_start, nocc0, nocc1, nvirt0, nvirt)
            t2old = t2new
            t1old = t1new
            cc_energy_old = cc_correction(t2old, t1old, nocc, nactive)
            print*, 'MP2 = ', cc_energy_old+erhf, cc_energy_old, erhf
            !
            ! Generate T1 and T2 start amplitudes
            !
            diis_iter = 1

            do l = 1, CC_DIIS_NSTART
                  call t1_transformed_integrals2(t1old, mocoeff, nocc, nvirt, nactive,&
                        nocc0, nocc1, nvirt0, nvirt1)

                  call tequation_driver(eorb, nocc, nvirt, t2old, t1old, &
                        t2new, t1new, method, cc_sp_iter, nocc0, nocc1, nvirt0, nvirt1)

                  call t1t2_tovec(nocc, nactive, t1new, t2new, t1t2vec)
                  cc_energy = cc_correction(t2new, t1new, nocc, nactive)
                  print*, 'energy', cc_energy + erhf

                  if(diis_iter.eq.1)then
                        diis_P(:, 1) = t1t2vec
                        diis_iter = diis_iter + 1
                        diis_n = 2
                  else
                        call fill_diis_P(t1t2vec, diis_P, diis_iter, diis_idx, diis_n, noflindep)
                        diis_iter = compensate(diis_iter + 1 - noflindep)
                  end if
                  t1old = t1new
                  t2old = t2new
            end do
            call cpu_time(time0_total)
            !$ time0_total = omp_get_wtime()                                                                                                                                          
            call cctable_start()

            cc_energy_old = cc_correction(t2new, t1new,nocc, nactive)
            diis_n = clock_r(CC_DIIS_NSTART, 1)

            do while (q.eqv..false.)
                  !                                                                                                                                           
                  ! Establish current dimension of the diis_P matrix                                                                             
                  !                                                                                                                 
                  if(diis_iter.gt.CC_DIIS_NMAX)then
                        di = CC_DIIS_NMAX
                  else
                        di = diis_iter - 1
                  end if
                  !
                  ! Find diis extrapolation coefficients                                                                         
                  !                                                                                                                         
                  call diis(diis_P, di, DIIS_T1T2_DIM, diis_coeff, diis_idx)
                  !
                  ! diis_coeff contains (1:di) coefficients for the
                  ! extrapolated t1 and t2 amplitudes                                                                                                               
                  !                                                                                                                                          
                  ! Extrapolate t1old and t2old from diis_P and diis_coeff                                                                                 
                  call update_t_from_diis_P(nocc, nactive, t1old, t2old, diis_P, diis_coeff, di, diis_idx)
                  !                                                                                                                                       
                  ! Relax system for CC_DIIS_NRELAX iterations                                                                                              
                  ! Iterate without diis extrapolation                                                                                                     
                  !                                                                                                                                                
                  do i = 1, CC_DIIS_NRELAX
                        if(q.neqv..true.)then
                              !$ time0 = omp_get_wtime()    
                              !$ ttt1 = omp_get_wtime()
                              call t1_transformed_integrals2(t1old, mocoeff, nocc, nvirt, nactive,&
                                    nocc0, nocc1, nvirt0, nvirt1)
                              !$ ttt2 = omp_get_wtime()
                              print*, 'czas na transform', ttt2 - ttt1

                              call tequation_driver(eorb, nocc, nvirt, t2old, t1old, &
                                    t2new, t1new, method, cc_sp_iter, nocc0, nocc1, nvirt0, nvirt1)
                              !$ ttt1 = omp_get_wtime()
                              print*, 'czas na tequation', ttt1 - ttt2

                              !                                                                                                                                     
                              ! Find index of the previous vector                                                                                  
                              !
                              j = diis_idx(clock_l(diis_iter,1))
                              ! Read old vector for checking convergence                                                                                            
                              !
                              t1t2vecold = diis_P(:, j)

                              call t1t2_tovec(nocc, nactive, t1new, t2new, t1t2vec)
                              
                              cc_energy = cc_correction(t2new, t1new, nocc, nactive)
                              cc_energy_licz = cc_correction_licz(t2new, t1new, nocc, nactive)
                              ediff =  cc_energy - cc_energy_old

                              call ccsd_convrg_short(t1t2vecold, t1t2vec, DIIS_T1T2_DIM, q, ampltdiff, ediff)
                              cc_energy_old = cc_energy

                              call fill_diis_P(t1t2vec, diis_P, diis_iter, diis_idx, diis_n, noflindep)
                              diis_iter = compensate(diis_iter + 1 - noflindep)

                              !$ time1 = omp_get_wtime()                                                                                                           
                              call cctable_continue(iter, cc_energy+erhf, ampltdiff, ediff, time1-time0)
                              iter = iter + 1

                              if (i.lt.CC_DIIS_NRELAX)then
                                    t1old = t1new
                                    t2old = t2new
                              end if
                        end if
                  end do

                  if(q.neqv..true.)then
                        t1old = t1new
                        t2old = t2new
                  end if
            end do

            call cpu_time(time1_total)

            !$ time1_total = omp_get_wtime()                                                                                                                                           

            timetot = time1_total - time0_total
            t1_dg = t1dgnst(t1new, nocc, nvirt, nactive)
            call blankline()
            if(method .eq. THEORY_CCSD)then
                  call msg('CCSD CONVERGED')
                  call dmsg("CCSD TOTAL TIME [SECONDS] ", timetot, fmt="ES10.1")
                  call dmsg("T1 DIAGNOSTIC", t1_dg, fmt="ES10.1")
                  if((t1_dg.gt.T1_DIAG))then
                        call msg('WARNING: CCSD RESULTS NOT RELIABLE')
                  end if
                  call imsg("NUMBER OF ITERATIONS ", iter)
            else if(method .eq. THEORY_CC3)then
                  !                  call fill_t3(t2new, eorb, nocc, nactive)
                  call fill_t3_sym(t2new, gitriples, gidimt, ggirrep0, ggirrep1, &
                        eorb, nocc, nvirt, nactive)
                  call msg('CC3 CONVERGED')
                  call dmsg("CC3 TOTAL TIME [SECONDS] ", timetot, fmt="ES10.1")
                  call dmsg("T1 DIAGNOSTIC", t1_dg, fmt="ES10.1")
                  call imsg("NUMBER OF ITERATIONS ", iter)
                  call t_equation_cc3_free()
            end if

            deallocate(t2old)
            deallocate(t1old)
            deallocate(diis_P)
            deallocate(t1t2vec)
            deallocate(t1t2vecold)
            deallocate(diis_coeff)
            deallocate(diis_idx)

            call diis_free()
            !                                                                                                                                                                             
            ! CC SUMMARY                                                                                                                                                                  
            !                                                                                                                                                                             
            if(method .eq. THEORY_CCSD)then
                  call toprule()
                  call msg('CCSD COMPLETED')
                  call midrule()

                  call dmsg("TOTAL CCSD ENERGY", erhf + cc_energy)
                  print*, 'cc_energy_licz', cc_energy_licz
                  call dmsg("TOTAL CCSD ENERGY [eV]", toev(erhf+cc_energy))
                  call dmsg("CCSD CORRECTION", cc_energy)
                  call dmsg("CCSD CORRECTION [eV]", toev(cc_energy))
            else if(method .eq. THEORY_CC3)then
                  call toprule()
                  call msg('CC3 COMPLETED')
                  call midrule()

                  call dmsg("TOTAL CC3 ENERGY", erhf + cc_energy)
                  call dmsg("TOTAL CC3 ENERGY [eV]", toev(erhf+cc_energy))
                  call dmsg("CC3 CORRECTION", cc_energy)
                  call dmsg("CC3 CORRECTION [eV]", toev(cc_energy))

            end if

      contains

            function compensate(l)
                  integer :: compensate
                  integer, intent(in) :: l

                  if(l.gt.CC_DIIS_NMAX+1)then
                        compensate = CC_DIIS_NMAX + 1
                  else
                        compensate = l
                  end if
            end function compensate
      end subroutine task_cc


      subroutine fill_diis_P(t1t2vec, diis_P, diis_iter, diis_idx, diis_n, noflindep)
            double precision, dimension(:), intent(in)                    :: t1t2vec
            double precision, dimension(:,:), intent(inout)               :: diis_P
            integer, intent(in)                                           :: diis_iter
            integer, dimension(:), intent(inout)                          :: diis_idx
            integer, intent(inout)                                        :: diis_n
            integer, intent(out)                                          :: noflindep

            integer :: i, j
            double precision, dimension(:,:), allocatable :: gram, gramcopy
            integer :: m, temp
            logical :: condnum

            double precision :: ddot
            external ddot

            !
            ! diis_iter - current diis iteration
            ! diis_idx  - holds information of the true
            !             order of diis vectors. 
            !             diis_idx(1) = oldest vector
            !             diis_idx(CC_DIIS_NMAX) = newest vector
            ! diis_n    - integer, on entry holds information of
            !             where the t1t2vec should be added.
            !             On exit ...
            ! noflindep - number vector that shows linear dep.
            !             with t1t2vec, and are removed from
            !             diis_P matrix.
            !             
            !
            ! m is the dimension of the gram matrix
            ! When the CC_DIIS_NMAX limit is reached
            ! new vector is added in place of the
            ! oldest vector, but gram matrix is           
            ! created for all CC_DIIS_NMAX + 1 vectors
            !

            if (diis_iter.le.CC_DIIS_NMAX) then
                  m = diis_iter 
            else 
                  m = CC_DIIS_NMAX + 1
            end if


            allocate(gram(m, m))
            allocate(gramcopy(m,m))
            !
            ! Check linear dependence with t1t2vec
            ! 
            gram = zero
            gramcopy = zero
            do i = 1, m-1
                  do j = 1, i
                        gram(i, j) = ddot(DIIS_T1T2_DIM, diis_P(:, diis_idx(i)) , 1, diis_P(:,diis_idx(j)), 1)
                  end do
            end do
            do j = 1, m-1
                  gram(m, j) = ddot(DIIS_T1T2_DIM, t1t2vec(:) , 1, diis_P(:,diis_idx(j)), 1)
            end do
            gram(m, m) = ddot(DIIS_T1T2_DIM, t1t2vec(:) , 1, t1t2vec(:), 1)

            gramcopy = gram

            condnum = .true.
            noflindep = 0

            !
            ! Check if the old vectors from diis_P are linear
            ! dependent with t1t2vec. 
            ! noflindep is set to the number of lin. dep. vectors.
            ! diis_n is set to idicate new index of adding t1t2vec
            ! in the diis_P matrix without lin. dep. vectors
            !

            do while (condnum.eqv..true.)

                  condnum = issingular(gramcopy, m-noflindep, 1.d-15)

                  if (condnum.eqv..true.)then
                        noflindep = noflindep + 1
                        diis_n = clock_l(diis_n,1)
                        diis_P(:, diis_n) = t1t2vec(:)
                        do j = 1, m-noflindep
                              gram(m-noflindep, j) = ddot(DIIS_T1T2_DIM, diis_P(:, diis_n) , 1, diis_P(:,diis_idx(j)), 1)
                        end do
                        gramcopy = gram
                  end if
            end do

            !
            ! Add t1t2vec to diis_P matrix and change diis_idx
            !

            if(noflindep.eq.0)then
                  !
                  ! No vectors should be removed
                  !
                  if((m).eq.(CC_DIIS_NMAX + 1))then
                        !
                        ! The diis_P is already full. Add t1t2vec
                        ! in place of the oldest vector on the diis_idx(1) place.
                        !

                        diis_P(:, diis_idx(1)) = t1t2vec(:)
                        !
                        ! Change the order of the diis_idx
                        !
                        temp = diis_idx(1)
                        do i = 1, CC_DIIS_NMAX - 1
                              diis_idx(i) = diis_idx(i + 1)
                        end do
                        diis_idx(CC_DIIS_NMAX) = temp
                  else
                        !
                        ! diis_P has empty places. Add t1t2vec on the first
                        ! empty place. The order of diis_idx remains intact.
                        !
                        diis_P(:,diis_n) = t1t2vec(:)

                  end if
                  !
                  ! Change diis_n to the next empty index in diis_P, or
                  ! if now diis_P is full, change it to the index of the
                  ! oldest vector
                  !
                  diis_n = clock_r(diis_n,1)
            else
                  !
                  ! noflindep vectors where found to be linear 
                  ! dependent with t1t2vec. In this case diis_n has
                  ! already been changed to the appropriate index.
                  ! Add t1t2vec in the place nex to the newest not linear
                  ! dependent vector
                  ! 
                  diis_P(:, diis_n) = t1t2vec(:)
                  diis_n = clock_r(diis_n,1)
            end if

            deallocate(gram)
            deallocate(gramcopy)

      end subroutine fill_diis_P


      subroutine t1t2_tovec(nocc, nactive, t1, t2, t1t2vec)
            integer, intent(in)                                           :: nocc, nactive
            double precision, dimension(nocc+1:nactive, nocc), intent(in) :: t1
            double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(in)                                  :: t2
            double precision, dimension(:), intent(out)                   :: t1t2vec

            integer :: a, b, i, j, k

            k = DIIS_T1_START

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        t1t2vec(k) = t1(a, i)
                        k = k + 1
                  end do
            end do

            k = DIIS_T2_START

            do i = 1, nocc
                  do j = 1, nocc
                        do a = nocc + 1, nactive
                              do b = nocc + 1, nactive
                                    t1t2vec(k) = t2(a, b, i, j)
                                    k = k + 1
                              end do
                        end do
                  end do
            end do

      end subroutine t1t2_tovec

      function clock_l(n, s)
            integer :: clock_l
            integer, intent(in) :: n
            integer, intent(in) :: s

            if(n-s.le.0)then
                  clock_l = CC_DIIS_NMAX + (n-s)
            else
                  clock_l = n - s
            end if

      end function clock_l

      function clock_r(n, s)
            integer :: clock_r
            integer, intent(in) :: n
            integer, intent(in) :: s

            if(n+s.gt.CC_DIIS_NMAX)then
                  clock_r = s
            else
                  clock_r = n + s
            end if

      end function clock_r


      subroutine update_t_from_diis_P(nocc, nactive, t1, t2, diis_P, diis_coeff, diis_iter, diis_idx)
            integer, intent(in)                                              :: nocc, nactive
            double precision, dimension(nocc+1:nactive, nocc), intent(inout) :: t1
            double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(inout)                                  :: t2
            double precision, dimension(:,:), intent(in)                     :: diis_P
            double precision, dimension(:), intent(in)                       :: diis_coeff
            integer, intent(in)                                              :: diis_iter
            integer, dimension(:), intent(in)                             :: diis_idx

            integer :: a, i,  b, j, l, k

            t1 = ZERO
            t2 = ZERO
            do l = 1, diis_iter-1
                  k = DIIS_T1_START
                  do i = 1, nocc
                        do a = nocc + 1, nactive
                              t1(a, i) = t1(a, i) + diis_coeff(l) * diis_P(k, clock_r(diis_idx(l),1))
                              k = k + 1
                        end do
                  end do
            end do

            do l = 1, diis_iter-1
                  k = DIIS_T2_START
                  do j = 1, nocc
                        do i = 1, nocc
                              do b = nocc + 1, nactive
                                    do a = nocc + 1, nactive
                                          t2(a, b, i, j) = t2(a, b, i, j) + diis_coeff(l) * diis_P(k, clock_r(diis_idx(l),1))
                                          k = k + 1
                                    end do
                              end do
                        end do
                  end do
            end do


      end subroutine update_t_from_diis_P


      subroutine update_t1_t2_old(nocc, nactive, t1old, t2old, diis_P, st, it)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            double precision, dimension(nocc+1:nactive, nocc), intent(inout) :: t1old
            double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(inout)                                 :: t2old
            double precision, dimension(:,:), intent(in)                     :: diis_P
            integer, intent(in)                                              :: st
            integer, intent(in)                                              :: it

            integer :: a, b, i, j, k

            t1old = 0.d+0
            t2old = 0.d+0

            k = 1
            do i = 1, nocc
                  do a = nocc + 1, nactive
                        t1old(a, i) = t1old(a, i) + diis_P(k, it)
                        k = k + 1
                  end do
            end do

            k = st
            do i = 1, nocc
                  do j = 1, nocc
                        do a = nocc + 1, nactive
                              do b = nocc + 1, nactive
                                    t2old(a, b, i, j) = t2old(a, b, i, j) + diis_P(k, it)
                                    k = k + 1
                              end do
                        end do
                  end do
            end do

      end subroutine update_t1_t2_old


      subroutine task_eom_cc_dav(method, wr, wi, eorb, nocc0, &
            nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, t2, &
            nroots, degener, iexci, irrep0, irrep1, order, irrep_idx, &
            convrecs, prefix, guess_coeff_r, multip)
            !
            ! Creating Jacobian Matrix -- Helgaker 'Molecular Electronic Structure Energy part 2' p. 681 eq (13.6.23)
            ! Matrix elements generated by Paldus.
            !
            integer, intent(in)                                                 :: method
            double precision, dimension(:), intent(inout)                       :: wr
            double precision, dimension(:), intent(inout)                       :: wi
            double precision, dimension(:), intent(in)                          :: eorb
            integer, intent(in)                                                 :: nocc0, nvirt0
            integer, intent(in)                                                 :: nocc1, nvirt1
            integer, intent(in)                                                 :: nocc, nvirt
            integer, intent(in)                                                 :: nactive
            double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
                  nocc, nocc), intent(in)                                       :: t2
            integer, intent(in)                                                 :: nroots
            integer, dimension(:), intent(in)                                   :: degener
            integer, dimension(:), intent(in)                                   :: iexci
            integer, dimension(:,:), intent(in)                                 :: irrep0
            integer, dimension(:,:), intent(in)                                 :: irrep1
            integer, intent(in)                                                 :: order
            integer, intent(in)                                                 :: irrep_idx
            type(trecgroup), intent(out)                                        :: convrecs
            character(*), intent(in)                                            :: prefix
            real(F64), dimension(:, :), intent(in)                              :: guess_coeff_r
            integer, intent(in)                                                 :: multip
            real(F64), dimension(:, :), allocatable                             :: guess_coeff_l          
            integer :: guess_dim1, guess_dim2
            double precision, dimension(:), allocatable :: residual_l
            double precision, dimension(:), allocatable :: residual_r
            double precision, dimension(:), allocatable :: e_diff
            double precision, dimension(:), allocatable :: e_old, e_new
            integer :: npair, nidx, nidx_ccsd
            integer, dimension(2) :: nidx_table
            integer(F64) :: memspace_dav, memspace_d_small
            real(F64), dimension(:,:), allocatable :: d_small
            integer :: method_temp
            real(F64)            :: dav_conv

            integer :: gk
            integer :: dav_info
            integer :: i
            type(tclock) :: t_iter
            type(tclock) :: t_total
            logical :: rightvecs_converged, leftvecs_converged
            integer :: ntargetvecs
            integer :: it_temp
            logical :: restart
            real(F64) :: time1, time2
            real(F64) :: dupa1, dupa2, dupa3
            integer :: r3dim_small
            integer, dimension(:,:), allocatable :: r3limits
            integer :: all_ttasks, idimt
            integer, dimension(:,:), allocatable :: itriples 

            call clock_start(t_total)
            !
            ! EOM-CC3/EOM-CCSD PREAMBLE
            !
            call toprule()
            if (method .eq. THEORY_CCSD)then
                  call msg('EOM-CCSD MODULE')
                  call midrule()
            else if (method .eq. THEORY_CC3) then
                  call msg('EOM-CC3 MODULE')
                  call midrule()
            end if
            npair = nocc * nvirt
            if (EOM_MEM)then
                  method_temp = THEORY_CC3_MEM
            else
                  method_temp = method
            end if

            if (multip == cc_singlet) then
                  if (method_temp .eq. THEORY_CCSD) then
                        nidx = npair + ((npair + 1) * npair) / 2
                        nidx_table(1) = nidx
                        nidx_table(2) = -1
                        allocate(r3limits(1, 1))
                        r3dim_small = 0
                  else if (method_temp .eq. THEORY_CC3)then
                        nidx_ccsd = npair + ((npair + 1) * npair) / 2
                        nidx = npair + ((npair + 1) * npair) / 2 + (npair**3+3*npair**2+2*npair)/6
                        print*, 'SIZE OF THE CCSD PROBLEM', nidx_ccsd
                        print*, 'SIZE OF THE FULL CC3 PROBLEM', nidx 
                        nidx_table(1) = nidx_ccsd
                        nidx_table(2) = -1
                        r3dim_small = 0

                        allocate(itriples(1,1))
                        call irrep_triples(6, irrep0, irrep1, nocc0, nocc, nvirt0, &
                              itriples, POINT_GROUP, idimt, .true., .false.)
                        deallocate(itriples)
                        ! call calculate_r3dim_small(nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1, &
                        !       irrep0, irrep1, r3dim_small, all_ttasks)
                        allocate(r3limits(idimt, 2))
                        call calculate_r3limits(nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1, &
                              irrep0, irrep1, r3limits)
                        ! print*, 'r3lim', r3limits(idimt-1, 1:2)
                        ! print*, r3limits(idimt, 1:2)
                        r3dim_small = r3limits(idimt, 2)
                        print*, 'SIZE OF CC3 MEM PROBLEM', r3dim_small

                  else if (method_temp .eq. THEORY_CC3_MEM) then
                        nidx = npair + ((npair + 1) * npair) / 2
                        print*,'nidx CCCC mem', nidx
                        nidx_table(1) =nidx
                        nidx_table(2) =-1
                  end if
            else if (multip == cc_triplet) then
                  if (method_temp == THEORY_CCSD) then
                        nidx_table(1) = npair * (nvirt - 1) * (nocc - 1)/ 4
                        nidx_table(2) = npair * (npair - 1) / 2
                        nidx = npair + nidx_table(1) + nidx_table(2)
                  else if (method_temp .eq. THEORY_CC3)then
                        nidx_table(1) = npair * (nvirt - 1) * (nocc - 1)/ 4
                        nidx_table(2) = npair * (npair - 1) / 2
                        nidx = npair + (npair + 1) * nidx_table(1) + nidx_table(2)
                        ! The triples block aibjck, has the following constraints
                        ! on the indices: b > c, j > k. It's size is:
                        ! npair * nidx_table(1)
                  end if
            end if
            !
            ! Number of eigenvectors to converge
            !
            ntargetvecs = sum(degener)

            memspace_dav = int(CC_EOM_MEMSPACE / three)
            print*, CC_EOM_MEMSPACE

            memspace_d_small = CC_EOM_MEMSPACE - memspace_dav
            
!            call jac_temp_init(nidx)
!            print*, 'DAV UNIIIIIIIIT'
!            call dav_unittest()
!            stop
            
            dav_conv = DAV_QCONVTHRSH
            print*,'nidx CCCC-srrr', nidx

            call dav_init(nidx_ccsd, nidx, r3dim_small, nroots, degener, .false., .true., .true., &
                  guess_coeff_r, dav_conv, CC_EOM_CONV_MAXIT, memspace_dav, &
                  CC_EOM_DISKSPACE, .true., convrecs, prefix)

            if (EOM_MEM) then
                  !call d_small_init(memspace_d_small, nidx)
                  allocate(d_small(DAV_MAXNVEC,DAV_MAXNVEC))
                  d_small = ZERO
            end if
            
            ! Columns of C (C_BAR) matrix store eigenvectors
            ! expressed in the basis of columns of DAV_BASIS
            ! matrix
            !
            allocate(residual_r(ntargetvecs))
            allocate(residual_l(ntargetvecs))
            allocate(e_diff(ntargetvecs))
            allocate(e_old(ntargetvecs))
            allocate(e_new(ntargetvecs))

            residual_r = huge(ZERO)
            residual_l = huge(ZERO)
            e_diff = huge(ZERO)
            e_old = ZERO
            e_new = ZERO

            call eom_cctable_start()
            dav_info = DAV_CONTINUE
            leftvecs_converged = .false.
            rightvecs_converged = .false.
            ! if (cc_multip == cc_singlet)then
            !       call init_block_21_cc3(nocc, nocc0, nvirt0, irrep0, irrep1)
            ! end if
            !
            ! CONVERGING ITERATIVE SOLUTION OF EOM-CC NONSYMMETRIC EIGENSYSTEM
            !
            
            ! if (EOM_MEM)then
            !       method_temp = THEORY_CC3_MEM
            ! else
            !       method_temp = method
            ! end if


            it_temp = 0
            restart = .false.
            do while ((.not. leftvecs_converged) .and. (dav_info .ne. DAV_FAILURE))

                  it_temp = it_temp + 1
                  call clock_start(t_iter)
                  if (method_temp .eq. THEORY_CCSD) then
                        call jacobian_update_init(nocc, nvirt)
                  else if (method_temp .eq. THEORY_CC3 ) then
                        call jacobian_update_triples_init(nocc, nvirt)
                  else if (method_temp .eq. THEORY_CC3_MEM) then
                        call jacobian_update_triples_init(nocc, nvirt)
                  end if

                  call jacobian_update_driver2(method_temp, eorb, t2, nocc, &
                        nocc0, nocc1, nvirt0, nvirt1, nactive, npair, &
                        nidx_table, irrep0, irrep1, irrep_idx, multip, r3limits)

                  if (EOM_MEM) then
                        !$ time1 = omp_get_wtime()
                        call d_small_driver(d_small, t2, eorb, nocc0, nvirt0, nocc, nvirt, nactive, irrep0, irrep1, irrep_idx)
                        !$ time2 = omp_get_wtime()
                        print*, 'czas na dsmall', time2 - time1
                  end if

                  if (method_temp .eq. THEORY_CCSD) then
                        call jacobian_update_free()
                  else if (method_temp .eq. THEORY_CC3) then
                        call jacobian_update_triples_free()
                  else if (method_temp .eq. THEORY_CC3_MEM) then
                        call jacobian_update_triples_free()
                  end if


                  if (ONLYRIGHT) then
                        if (.not. rightvecs_converged) then
                              if (EOM_MEM .eqv. .true.) then
                                    call dav_iter(wr, wi, residual_r, e_diff, convrecs, dav_info, d_small = d_small)
                              else
                                    call dav_iter(wr, wi, residual_r, e_diff, convrecs, dav_info)
                              end if
                        end if
                  else
                        if (.not. rightvecs_converged) then
                              if (EOM_MEM .eqv. .true.) then
                                    call dav_iter(wr, wi, residual_r, e_diff, convrecs, dav_info, d_small = d_small)
                              else
                                    call dav_iter(wr, wi, residual_r, e_diff, convrecs, dav_info)
                              end if
                        else
                              if (EOM_MEM .eqv. .true.) then
                                    call dav_iter(wr, wi, residual_l, e_diff, convrecs, dav_info, d_small = d_small)
                              else
                                    call dav_iter(wr, wi, residual_l, e_diff, convrecs, dav_info)
                              end if
                        end if
                  end if
                  e_old = e_new
                  e_new = wr(1:DAV_NSOUGHT)

                  call midrule(width=20)
                  do i = 1, DAV_NSOUGHT
                        write(*, '(I3, F13.10)') i, e_new(i)
                  end do
                  call midrule(width=20)

                  if (dav_converging_right()) then

                        call eom_cctable_continue(DAV_NITER, DAV_BASISDIM, clock_readwall(t_iter), &
                              residual_r, e_diff, DAV_NSOUGHT)
                  else
                        call eom_cctable_continue(DAV_NITER, DAV_BASISDIM, clock_readwall(t_iter), &
                              residual_l, e_diff, DAV_NSOUGHT)
                  end if

                  if (dav_info == DAV_CONVERGED) then
                        if (ONLYRIGHT) then
                              if (.not. rightvecs_converged) then
                                    call msg("RIGHT EIGENVECTORS CONVERGED")
                                    rightvecs_converged = .true.
                                    leftvecs_converged = .true.
                                    call dav_free()
                                    call d_small_free()
                              end if
                        else

                              if (.not. rightvecs_converged) then
                                    call msg("RIGHT EIGENVECTORS CONVERGED")
                                    rightvecs_converged = .true.
                                    call dav_free()
                                    call d_small_free()
                                    
                                    call msg("STARTING SEARCH FOR LEFT EIGENVECTORS")
                                    guess_dim1 = size(guess_coeff_r, dim=1)
                                    guess_dim2 = size(guess_coeff_r, dim=2)
                                    allocate(guess_coeff_l(guess_dim1, guess_dim2))
                                    call readconverged_nonsymmetric_right(convrecs, .true., ntargetvecs, guess_coeff_l)
                                    call dav_init(nidx_ccsd, nidx, r3dim_small, nroots, degener, .false., .false., .true., &
                                          guess_coeff_l, dav_conv, CC_EOM_CONV_MAXIT, memspace_dav, &
                                          CC_EOM_DISKSPACE, .false.)
                                    deallocate(guess_coeff_l)
                                    !                              print*, 'teraz robie dav init'
                                    !call d_small_init(memspace_d_small, npair + ((npair + 1) * npair) / 2)
                                    restart = .false.
                              else
                                    call msg("LEFT EIGENVECTORS CONVERGED. EIGENSYSTEM SOLVED")
                                    leftvecs_converged = .true.
                                    call dav_free()
                                    call d_small_free()
                              end if
                        end if
                  end if
            end do

            !
            ! EOM-CC-DAVIDSON SUMMARY
            !
            call blankline()
            if (.not. leftvecs_converged .or. .not. rightvecs_converged) then
                  call msg("DAVIDSON SUBROUTINE FAILED TO CONVERGE")
                  call imsg("MAXIMUM NUMBER OF ITERATIONS", DAV_MAXIT)
                  stop
            end if

            call dmsg("EOM-CC TOTAL TIME [SECONDS] ", clock_readwall(t_total), fmt="ES10.1")
            call imsg("NUMBER OF ITERATIONS ", DAV_NITER)
            call blankline()
            call toprule()
            if (method_temp .eq. THEORY_CCSD) then
                  call msg('EOM-CCSD COMPLETED')
            else if(method_temp .eq. THEORY_CC3)then
                  call msg('EOM-CC3 COMPLETED')
            else if(method_temp .eq. THEORY_CC3_MEM)then
                  call msg('EOM-CC3 MEM COMPLETED')
            end if
            call blankline
            print*, 'dupa44444'
            !
            ! Display convergence info
            !
            call dav_convinfo(residual_l, residual_r)

            ! call toprule_double()
            ! if(method_temp .eq. THEORY_CCSD)then
            !       call msg('EOM-CCSD EXCITATION ENERGIES')
            ! else if(method_temp .eq. THEORY_CC3)then
            !       call msg('EOM-CC3 EXCITATION ENERGIES')
            ! else if(method_temp .eq. THEORY_CC3_MEM)then
            !       call msg('EOM-CC3 EXCITATION ENERGIES')
            ! end if
            ! call eom_cctable_summary_start()
!             do i = 1, DAV_NSOUGHT
! !                  print*, i, DAV_NSOUGHT
!                   !                  gk = compgk(i, degener_actual, (.not. CC_DEGENERACY), CC_NLEVELS)
!                   gk = 100
!                   call eom_cctable_summary_continue(i, wr(i), toev(wr(i)), tocm_1(wr(i)), irrep_idx, multip)
!                   print*, 'gowno'
!             end do
            ! print*, 'gowno2'
            ! call toprule_double()

            deallocate(residual_l)
            print*, 's2'
            deallocate(residual_r)
            print*, 's3'
            deallocate(e_old)
            print*, 's4'
            deallocate(e_new)
            print*, 's5'
            deallocate(e_diff)
      end subroutine task_eom_cc_dav


      subroutine jacobian_update_init(nocc, nvirt)
            integer, intent(in) :: nocc, nvirt

            call fill_offset_doubles(nocc, nvirt)
      end subroutine jacobian_update_init


      subroutine jacobian_update_triples_init(nocc, nvirt)
            integer, intent(in) :: nocc, nvirt
            type(tclock) :: time

            call clock_start(time)
            call fill_offset_doubles(nocc, nvirt)            
            call clock_start(time)
            call fill_offset_triples(nocc, nvirt)            

      end subroutine jacobian_update_triples_init

      subroutine jacobian_update_free()
            call fill_offset_doubles_free()
      end subroutine jacobian_update_free

      subroutine jacobian_update_triples_free()
            call fill_offset_doubles_free()
            call fill_offset_triples_free()
      end subroutine jacobian_update_triples_free

      ! subroutine task_cis(iexci, wrci, rtstart, eorb, nocc0, nocc1, nvirt0, nvirt1, nactive,  m, &
      !       e_nuclear, order, irrep0, irrep1, hdim, block_dim)

      !       integer,   dimension(:), intent(in)       :: iexci
      !       real(F64), dimension(:), intent(out)    :: wrci
      !       real(F64), dimension(:,:), intent(out)  :: rtstart
      !       real(F64), dimension(:), intent(in)     :: eorb
      !       integer, intent(in)                     :: nocc0, nocc1
      !       integer, intent(in)                     :: nvirt0, nvirt1
      !       integer, intent(in)                     :: nactive
      !       integer, intent(in)                     :: m
      !       integer, intent(in)                     :: hdim
      !       integer, dimension(:), intent(in)      :: block_dim
      !       real(F64), intent(in)                   :: e_nuclear
      !       integer, intent(in)                     :: order
      !       integer, dimension(:,:), intent(in)     :: irrep0
      !       integer, dimension(:,:), intent(in)     :: irrep1

      !       real(F64), dimension(:,:), allocatable  :: hci
      !       real(F64), dimension(:), allocatable    :: w
      !       real(F64), dimension(:), allocatable    :: work
      !       real(F64), dimension(1)                 :: work0

      !       integer, dimension(:), allocatable             :: iwork
      !       integer, dimension(:), allocatable             :: ifail

      !       integer :: lwork
      !       integer :: info
      !       integer :: d
      !       integer :: nocc, nvirt
      !       integer :: npair
      !       integer :: moutp, mout

      !       integer :: i, j
      !       integer, parameter :: OC = 1
      !       integer, parameter :: VT = 2

      !       real(F64), dimension(:, :), allocatable :: rtsmall

      !       real(F64), parameter :: abstol = 1.d-10

      !       integer :: bdim 
      !       integer :: offset
      !       integer :: s0, s1

      !       nocc = nocc1 - nocc0 + 1
      !       nvirt = nvirt1 - nvirt0 + 1
      !       npair = nocc * nvirt

      !       rtstart = zero


      !       s0 = 1
      !       offset = 0
      !       do i = 1, order
      !             if (iexci(i) .ne. 0) then

      !                   bdim = block_dim(i)

      !                   allocate(w(bdim))
      !                   allocate(iwork(5*bdim))
      !                   allocate(ifail(bdim))
      !                   allocate(hci(bdim, bdim))
      !                   allocate(rtsmall(bdim, bdim))                        
      !                   call create_hci(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
      !                         nvirt0, nvirt1, nactive, order, e_nuclear)

      !                   lwork = -1
      !                   call dsyevx('V', 'I', 'L', bdim, hci, bdim, ZERO,&
      !                         ZERO, 1, bdim, abstol, moutp, w, rtsmall, bdim, work0,&
      !                         lwork, iwork, ifail, info)

      !                   lwork = nint(work0(1))
      !                   d = max(1, lwork)
      !                   allocate(work(1:d))
      !                   call dsyevx('V', 'I', 'L', bdim, hci, bdim, ZERO,&
      !                         ZERO, 1, bdim, abstol, mout, w, rtsmall, bdim, work,&
      !                         lwork, iwork, ifail, info )

      !                   s0 = s0 + offset
      !                   s1 = s0 + bdim -1

      !                   call write_to_big(i, irrep0, irrep1, nocc0, nocc, &
      !                         nvirt0, order, w, wrci, rtsmall, rtstart, s0, s1)

      !                   offset = bdim

      !                   deallocate(work)
      !                   deallocate(w) 
      !                   deallocate(iwork)
      !                   deallocate(ifail)
      !                   deallocate(hci)
      !                   deallocate(rtsmall)

      !             end if
      !       end do

      !       !----------------------------------PELNY HAMILTONIAN-----------------------
      !       ! allocate(w(npair))
      !       ! allocate(iwork(5*npair))
      !       ! allocate(ifail(npair))
      !       ! allocate(hci(npair,npair))

      !       ! bra0 = 1
      !       ! ket0 = 1

      !       ! call ci_block_s_s(hci, eorb, nocc0, nocc1, nvirt0, nvirt1, nactive, 1, 1, e_nuclear, &
      !       !       n0i, n0j, n0a, n0b, n1i, n1j, n1a, n1b)

      !       ! lwork = -1
      !       ! call dsyevx('V', 'I', 'L', npair, hci, npair, ZERO,&
      !       !       ZERO, 1, m, abstol, moutp, w, rtstart, npair, work0,&
      !       !       lwork, iwork, ifail, info)


      !       ! lwork = nint(work0(1))
      !       ! d = max(1, lwork)
      !       ! allocate(work(1:d))
      !       ! call dsyevx('V', 'I', 'L', npair, hci, npair, 0.d+0,&
      !       !       0.d+0, 1, m, abstol, mout, w, rtstart, npair, work,&
      !       !       lwork, iwork, ifail, info )

      !       ! wrci = w(1:m)

      !       ! do i = 1, m
      !       !       print*, wrci(i)
      !       ! end do


      !       !            deallocate(w)
      !       !            deallocate(rtsmall)
      !       !            deallocate(iwork)
      !       !            deallocate(ifail)
      !       !            deallocate(hci)

      ! end subroutine task_cis


      ! subroutine create_hci(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
      !       nvirt0, nvirt1, nactive, order, e_nuclear)

      !       integer, dimension(:), intent(in)         :: iexci
      !       real(F64), dimension(:,:), intent(out)    :: hci
      !       integer, intent(in)                       :: i
      !       integer, dimension(:,:), intent(in)       :: irrep0
      !       integer, dimension(:,:), intent(in)       :: irrep1
      !       integer, intent(in)                       :: OC
      !       integer, intent(in)                       :: VT
      !       real(F64), dimension(:), intent(in)       :: eorb
      !       integer, intent(in)                       :: nocc0
      !       integer, intent(in)                       :: nocc1
      !       integer, intent(in)                       :: nvirt0
      !       integer, intent(in)                       :: nvirt1
      !       integer, intent(in)                       :: nactive
      !       integer, intent(in)                       :: order
      !       real(F64), intent(in)                     :: e_nuclear

      !       integer :: bra0
      !       integer :: ket0

      !       integer, dimension(:, :), allocatable :: isingles

      !       integer :: bj, kj
      !       integer :: bocdim, kocdim, bvtdim, kvtdim
      !       integer :: n0i, n0j, n0a, n0b
      !       integer :: n1i, n1j, n1a, n1b
      !       integer :: nocc
      !       integer :: idims

      !       nocc = nocc1 - nocc0 + 1


      !       hci = ZERO
      !       bra0 = 1
      !       ket0 = 1

      !       call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
      !       allocate(isingles(2, idims))
      !       call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

      !       do bj = 1, idims
      !             call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
      !                   n0i, n1i, n0a, n1a)

      !             ket0 = 1
      !             do kj = 1, idims
      !                   call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
      !                         n0j, n1j, n0b, n1b)

      !                   bocdim = (n1i-n0i+1)
      !                   bvtdim = (n1a-n0a+1)
      !                   kocdim = (n1j-n0j+1)
      !                   kvtdim = (n1b-n0b+1)


      !                   call ci_block_s_s(hci, eorb, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0, e_nuclear, &
      !                         n0i, n0j, n0a, n0b, n1i, n1j, n1a, n1b)

      !                   ket0 = ket0 + kocdim * kvtdim 

      !             end do
      !             bra0 = bra0 + bocdim * bvtdim  
      !       end do


      !       deallocate(isingles)

      ! end subroutine create_hci


      subroutine write_to_big(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
            order, w, wrci, rtsmall, rtstart, s0, s1)
            integer, intent(in)                 :: i
            real(F64), dimension(:), intent(in) :: w
            integer, dimension(:,:),intent(in)  :: irrep0
            integer, dimension(:,:),intent(in)  :: irrep1
            integer, intent(in)                 :: nocc0
            integer, intent(in)                 :: nocc
            integer, intent(in)                 :: nvirt0
            integer, intent(in)                 :: order
            real(F64), dimension(:), intent(inout) :: wrci
            real(F64), dimension(:,:), intent(in) :: rtsmall
            real(F64), dimension(:,:), intent(inout) :: rtstart
            integer, intent(in) :: s0
            integer, intent(in) :: s1

            integer, dimension(:, :), allocatable :: isingles

            integer :: bj
            integer :: bocdim, bvtdim
            integer :: n0i, n0a
            integer :: n1i, n1a
            integer :: aistart, aismall
            integer :: ibra
            integer :: bra0
            integer :: aa, ii
            integer :: k, l
            integer :: bdim
            integer :: idims

            bdim = s1-s0+1
            wrci(s0:s1) = w(1:bdim)
            bra0 = 0

            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

            l = 1
            do k = s0, s1
                  bra0 = 0
                  do bj = 1, idims

                        call loop_boundaries_sp(isingles(:, bj), irrep0, irrep1, &
                              n0i, n1i, n0a, n1a)


                        bocdim = (n1i-n0i+1)
                        bvtdim = (n1a-n0a+1)

                        do aa = n0a, n1a
                              do ii = n0i, n1i

                                    aistart = (aa - nvirt0) * nocc + (ii - nocc0) + 1
                                    aismall = (aa - n0a) * bocdim + (ii - n0i) + 1

                                    ibra = bra0 + aismall
                                    rtstart(aistart, k) = rtsmall(ibra, l)

                              end do
                        end do

                        bra0 = bra0 + bocdim * bvtdim  
                  end do
                  l = l + 1
            end do

            deallocate(isingles)

      end subroutine write_to_big

      subroutine write_to_big_cisd2(idx, irrep0, irrep1, nocc0, nocc, nvirt0, npair_small, npair_big, &
            order, w_small, w_big, rt_small, rt_big, s0_big, s1_big, s0_small, s1_small, dy)

            integer, intent(in)                 :: idx
            integer, dimension(:,:),intent(in)  :: irrep0
            integer, dimension(:,:),intent(in)  :: irrep1
            integer, intent(in)                 :: nocc0
            integer, intent(in)                 :: nocc
            integer, intent(in)                 :: nvirt0
            integer, intent(in)                 :: npair_small, npair_big
            integer, intent(in)                 :: order
            integer, intent(in), dimension(:)   :: dy
            real(F64), dimension(:), intent(in)      :: w_small
            real(F64), dimension(:, :), intent(inout)   :: w_big
            real(F64), dimension(:,:), intent(in)    :: rt_small
            real(F64), dimension(:,:), intent(inout) :: rt_big
            integer, intent(in) :: s0_big
            integer, intent(in) :: s1_big
            integer, intent(in) :: s0_small
            integer, intent(in) :: s1_small

            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles

            integer :: bdim
            integer :: bocdim, bvtdim
            integer :: n0i, n0a, n0j, n0b
            integer :: n1i, n1a, n1j, n1b
            integer :: ai, bj
            integer :: ibra_small, ibra_big
            integer :: a, i, b, j, a0
            integer :: k, l, m
            integer :: idims, idimd


            bdim = s1_big - s0_big + 1

            w_big(1:bdim, idx) = w_small(2:bdim+1)

            call irrep_singless(idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

            l = 2
            do k = s0_big, s1_big
                  rt_big(1:npair_small, k) = rt_small(2:npair_small+1, dy(l))
                  l = l + 1
            end do
            
            deallocate(isingles)

            call irrep_doubless(idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

            
            l = 2
            do k = s0_big,  s1_big
                  do m = 1, idimd

                     call loop_boundaries_sp(idoubles(1:2, m), irrep0, irrep1, &
                              n0i, n1i, n0a, n1a)
                        call loop_boundaries_sp(idoubles(3:4, m), irrep0, irrep1, &
                              n0j, n1j, n0b, n1b)

                        do b = n0b, n1b
                              do j = n0j, n1j
                                    a0 = max(n0a, b)
                                    do a = a0, n1a
                                          do i = n0i, n1i

                                                
                                                ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                                bj = (b - nvirt0) * nocc + (j - nocc0) + 1

                                                ibra_big = npair_big + &
                                                      ((2 * npair_big - bj + 2) * (bj - 1)) / 2 + ai - bj + 1

                                                ibra_small = npair_small + 1 + &
                                                      ((2 * npair_small - bj + 2) * (bj - 1))&
                                                      / 2 + ai - bj + 1

                                                rt_big(ibra_big, k) = rt_small(ibra_small, dy(l))
                                          end do
                                    end do
                              end do
                        end do
                  end do
                  l = l + 1
            end do


      end subroutine write_to_big_cisd2


      subroutine task_cisd2(iexci, wrci, rtstart, eorb, nocc0, nocc1, nvirt0, nvirt1, nvirt1_small, &
            e_total, e_electron, order, irrep0, irrep1)

            integer, dimension(:), intent(in)       :: iexci
            real(F64), dimension(:, :), intent(out) :: wrci
            real(F64), dimension(:,:), intent(out)  :: rtstart
            real(F64), dimension(:), intent(in)     :: eorb
            integer, intent(in)                     :: nocc0, nocc1
            integer, intent(in)                     :: nvirt0, nvirt1
            integer, intent(in)                     :: nvirt1_small

            real(F64), intent(in)                   :: e_total, e_electron
            integer, intent(in)                     :: order
            integer, dimension(:,:), intent(in)     :: irrep0
            integer, dimension(:,:), intent(in)     :: irrep1

            integer :: nocc, small_nvirt, big_nvirt
            integer :: npair_small, npair_big
            integer :: ddim, bdim, bdims

            real(F64), dimension(:, :), allocatable :: rtsmall ,ltsmall

            integer, dimension(:), allocatable :: dy

            real(F64), dimension(:,:), allocatable  :: hci
            real(F64), dimension(:), allocatable    :: w, wi
            real(F64), dimension(:), allocatable    :: work
            real(F64), dimension(1)                 :: work0

            integer, dimension(:), allocatable             :: iwork
            integer, dimension(:), allocatable             :: ifail
            real(F64) :: e_nuclear
            integer :: lwork
            integer :: info
            integer :: d
            integer :: i, j
            integer, parameter :: OC = 1
            integer, parameter :: VT = 2
            integer :: s0, s1
            integer :: s0_small, s1_small


            nocc = nocc1 - nocc0 + 1
            small_nvirt = nvirt1_small - nvirt0 + 1
            big_nvirt = nvirt1 - nvirt0 + 1

            npair_small = nocc * small_nvirt
            npair_big = nocc * big_nvirt

            ddim = (npair_small * (npair_small + 1)) / 2

            e_nuclear = e_total - e_electron
            rtstart = zero

            s0 = 1
            do i = 1, order
                  if (iexci(i) .ne. 0) then
                        bdim = 1 + npair_small + ddim   
                        bdims = bdim - 1

                        allocate(w(bdim))
                        allocate(wi(bdim))
                        allocate(iwork(5*bdim))
                        allocate(ifail(bdim))
                        allocate(hci(bdim, bdim))
                        allocate(rtsmall(bdim, bdim))
                        allocate(ltsmall(bdim, bdim))


                        hci = zero
                        w = zero
                        hci(1, 1) = e_total


                        ! call cisd_ss_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order)
                        ! print*, 'blok hd powinien byc w granicach', 1, npair_small + 1
                        ! call cisd_hd_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_dh_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_sd_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_ds_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_dd_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)

                        do j = 2, bdim
                              hci(j, j) = hci(j, j) + e_nuclear
                        end do


                        call dgeevquery(bdim, lwork, 'N', 'V')

                        d = max(1, lwork)
                        allocate(work(1:d))

                        call geevwrap(hci, w, wi, ltsmall, rtsmall, bdim, 'N', 'V', work)

                        do j = 1, 5
                              print*, w(j) - e_total
                        end do

                        allocate(dy(bdim))

                        do j = 1, bdim
                              dy(j) = j
                        end do

                        call dsort(w, dy, bdim)

                        do j = 1, 5
                              print*, w(j) - e_total
                        end do


                        if (i == 1) then
                              s0 = 1
                        else
                              s0 = sum(iexci(1:i-1)) + 1
                        end if

                        s1 = sum(iexci(1:i))

                        s0_small = 2
                        s1_small = bdim


                        call write_to_big_cisd2(i, irrep0, irrep1, nocc0, nocc, nvirt0, npair_small, npair_big, &
                              order, w, wrci, rtsmall, rtstart, s0, s1, s0_small, s1_small, dy)

                        do j = 1, iexci(i)
                              print*, wrci(j, i)-e_total
                        end do


                        deallocate(work)
                        deallocate(dy)
                        deallocate(w)
                        deallocate(wi)
                        deallocate(iwork)
                        deallocate(ifail)
                        deallocate(hci)
                        deallocate(rtsmall)
                        deallocate(ltsmall)

                  end if
            end do

      end subroutine task_cisd2


      ! subroutine task_cisd_dav(iexci, wrci, rtstart, eorb, nocc0, nocc1, nvirt0, nvirt1, &
      !       e_total, e_electron, order, irrep0, irrep1)

      !       integer, dimension(:), intent(in)       :: iexci
      !       real(F64), dimension(:, :), intent(out) :: wrci
      !       real(F64), dimension(:,:), intent(out)  :: rtstart
      !       real(F64), dimension(:), intent(in)     :: eorb
      !       integer, intent(in)                     :: nocc0, nocc1
      !       integer, intent(in)                     :: nvirt0, nvirt1

      !       real(F64), intent(in)                   :: e_total, e_electron
      !       integer, intent(in)                     :: order
      !       integer, dimension(:,:), intent(in)     :: irrep0
      !       integer, dimension(:,:), intent(in)     :: irrep1

      !       integer :: nocc, nvirt, nidx 
      !       integer :: npair
      !       integer :: ddim, bdim, bdims

      !       real(F64), dimension(:, :), allocatable :: rtsmall, ltsmall

      !       integer, dimension(:), allocatable :: dy

      !       real(F64), dimension(:,:), allocatable  :: hci
      !       real(F64), dimension(:), allocatable    :: w
      !       real(F64), dimension(:), allocatable    :: work
      !       real(F64), dimension(1)                 :: work0

      !       integer, dimension(:), allocatable             :: iwork
      !       integer, dimension(:), allocatable             :: ifail
      !       real(F64) :: e_nuclear
      !       integer :: lwork
      !       integer :: info
      !       integer :: d
      !       integer :: i, j
      !       integer, parameter :: OC = 1
      !       integer, parameter :: VT = 2
      !       integer :: s0, s1
      !       integer :: s0_small, s1_small

      !       real(F64), dimension(:,:), allocatable :: guess_single, guess_double, guess_coeff
      !       integer, dimension(:), allocatable :: degner
      !       integer :: nroots

      !       double precision, dimension(:), allocatable :: wr, wi
      !       double precision, dimension(:), allocatable :: residual_l
      !       double precision, dimension(:), allocatable :: residual_r
      !       double precision, dimension(:), allocatable :: e_diff
      !       double precision, dimension(:), allocatable :: e_old, e_new
      !       integer, dimension(:), allocatable :: degener
      !       type(tclock) :: t_iter, t_total
      !       type(trecgroup), dimension(:), allocatable  :: convrecs
      !       integer :: dav_info
      !       integer :: gk
      !       logical :: rightvecs_converged, leftvecs_converged

      !       call clock_start(t_total)

      !       nocc = nocc1 - nocc0 + 1
      !       nvirt = nvirt1 - nvirt0 + 1
      !       npair = nocc * nvirt
      !       nidx = npair + (npair * (npair + 1)) / 2

      !       e_nuclear = e_total - e_electron
      !       rtstart = zero
      !       allocate(convrecs(order))
      !       s0 = 1
      !       do i = 1, order
      !             if (iexci(i) .ne. 0) then
      !                print*, 'TERAZ TWORZE HCI DLA SYMETRII', i, 'O WYMIARZE', 1 + nidx
      !                bdim = 1 + nidx

      !                nroots = 2 * iexci(i)
      !                print*, nroots
      !                allocate(guess_single(nidx, iexci(i)))
      !                allocate(guess_double(nidx, iexci(i)))
      !                allocate(guess_coeff(nidx, nroots))
                                          
      !                call generate_guess_cisd_dav(iexci(i), guess_single, guess_double, &
      !                     nocc0, nocc1, nvirt0, nvirt1, eorb, &
      !                     i, irrep0, irrep1)

      !                guess_coeff(:, 1:iexci(i)) = guess_single(:, 1:iexci(i))
      !                guess_coeff(:, iexci(i)+1:nroots) = guess_double(:, 1:iexci(i))

      !                allocate(degener(nroots))
      !                degener = 1
                     
      !                call dav_init(bdim, nroots, degener, .false., .true., .true., &
      !                     guess_coeff, CC_EOM_CONV_THRESH, CC_EOM_CONV_MAXIT, CC_EOM_MEMSPACE, &
      !                     CC_EOM_DISKSPACE, .true., convrecs(i))
      !                !
      !                ! Columns of C (C_BAR) matrix store eigenvectors
      !                ! expressed in the basis of columns of DAV_BASIS
      !                ! matrix
      !                !
      !                allocate(wr(nroots))
      !                allocate(wi(nroots))
      !                allocate(residual_r(nroots))
      !                allocate(residual_l(nroots))
      !                allocate(e_diff(nroots))
      !                allocate(e_old(nroots))
      !                allocate(e_new(nroots))


      !                residual_r = huge(ZERO)
      !                residual_l = huge(ZERO)
      !                e_diff = huge(ZERO)
      !                e_old = ZERO
      !                e_new = ZERO

      !                call eom_cctable_start()
      !                dav_info = DAV_CONTINUE
      !                leftvecs_converged = .false.
      !                rightvecs_converged = .false.

      !                do while ((.not. leftvecs_converged) .and. (dav_info .ne. DAV_FAILURE))
      !                      call clock_start(t_iter)
      !                      print*, 'st'
      !                      call cisd_dav_driver(dav_sigma_update_diag, dav_sigma_update_right_nondiag, &
      !                            iexci, i, irrep0, irrep1, OC, VT, eorb, e_nuclear, e_total, nocc0, nocc1, &
      !                            nvirt0, nvirt1, order, npair)
      !                      print*, 'a'

      !                      if (.not. rightvecs_converged) then
      !                            print*, 'b'
      !                            print*, wr
      !                            call dav_iter(wr, wi, residual_r, e_diff, convrecs(i), dav_info)

      !                      else
      !                            print*, 'b2'
      !                            call dav_iter(wr, wi, residual_l, e_diff, convrecs(i), dav_info)

      !                      end if
      !                      print*, 'c'

      !                      e_old = e_new
      !                      e_new = wr(1:DAV_NSOUGHT)

      !                      do j = 1, DAV_NSOUGHT
      !                            print*, e_new(j)
      !                      end do
      !                      print*, 'continue'
      !                      if (dav_converging_right()) then
      !                            call eom_cctable_continue(DAV_NITER, DAV_BASISDIM, clock_readwall(t_iter), &
      !                                  residual_r, e_diff, DAV_NSOUGHT)
      !                      else
      !                            call eom_cctable_continue(DAV_NITER, DAV_BASISDIM, clock_readwall(t_iter), &
      !                                  residual_l, e_diff, DAV_NSOUGHT)
      !                      end if
      !                      print*, 'cont2'
      !                      if (dav_info == DAV_CONVERGED) then
      !                            if (.not. rightvecs_converged) then
      !                                  call msg("RIGHT EIGENVECTORS CONVERGED")
      !                                  call msg("STARTING SEARCH FOR LEFT EIGENVECTORS")
      !                                  rightvecs_converged = .true.
      !                                  call dav_free()
      !                                  call dav_init(nidx, nroots, degener, .false., .false., .true., &
      !                                        guess_coeff, CC_EOM_CONV_THRESH, CC_EOM_CONV_MAXIT, CC_EOM_MEMSPACE, &
      !                                        CC_EOM_DISKSPACE, .false.)
      !                            else
      !                                  call msg("LEFT EIGENVECTORS CONVERGED. EIGENSYSTEM SOLVED")
      !                                  leftvecs_converged = .true.
      !                                  call dav_free()
      !                            end if
      !                      end if
      !                      print*, 'sniez'
      !                end do
      !                !
      !                ! EOM-CC-DAVIDSON SUMMARY
      !                !
      !                call blankline()
      !                if (.not. leftvecs_converged .or. .not. rightvecs_converged) then
      !                      call msg("DAVIDSON SUBROUTINE FAILED TO CONVERGE")
      !                      call imsg("MAXIMUM NUMBER OF ITERATIONS", DAV_MAXIT)
      !                      stop
      !                end if

      !                call dmsg("EOM-CC TOTAL TIME [SECONDS] ", clock_readwall(t_total), fmt="ES10.1")
      !                call imsg("NUMBER OF ITERATIONS ", DAV_NITER)
      !                call blankline()
      !                call toprule()
      !                call msg('CISD COMPLETED')
      !                call blankline
      !                !
      !                ! Display convergence info
      !                !
      !                call dav_convinfo(residual_l, residual_r)

      !                call toprule_double()
      !                call msg('CISD EXCITATION ENERGIES')
      !                call eom_cctable_summary_start()
      !                do j = 1, DAV_NSOUGHT
      !                      !                  gk = compgk(i, degener_actual, (.not. CC_DEGENERACY), CC_NLEVELS)
      !                      gk = 100
      !                      call eom_cctable_summary_continue(j, wr(j), toev(wr(j)), tocm_1(wr(j)), i)
      !                end do
      !                call toprule_double()

      !                deallocate(residual_l)
      !                deallocate(residual_r)
      !                deallocate(e_old)
      !                deallocate(e_new)
      !                deallocate(e_diff)

                     ! allocate(w(bdim))
                     ! allocate(wi(bdim))
                     ! allocate(iwork(5*bdim))
                     ! allocate(ifail(bdim))
                        ! allocate(hci(bdim, bdim))
                        ! allocate(rtsmall(bdim, bdim))
                        ! allocate(ltsmall(bdim, bdim))


                        ! hci = zero
                        ! w = zero
                        ! hci(1, 1) = e_total
                        ! print*, 'tworzenie hamiltonianu ...'
                        ! print*, ''

                        ! call cisd_ss_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order)
                        ! print*, 'blok hd powinien byc w granicach', 1, npair_small + 1
                        ! call cisd_hd_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_dh_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_sd_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_ds_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)
                        ! call cisd_dd_driver(iexci, hci, i, irrep0, irrep1, OC, VT, eorb, nocc0, nocc1, &
                        !       nvirt0, nvirt1, order, npair_small + 1)

                        ! do j = 2, bdim
                        !       hci(j, j) = hci(j, j) + e_nuclear
                        ! end do

                        ! print*, 'diagonalizacjia...'
                        ! print*, ''
                        ! call dgeevquery(bdim, lwork, 'N', 'V')

                        ! d = max(1, lwork)
                        ! allocate(work(1:d))

                        ! call geevwrap(hci, w, wi, ltsmall, rtsmall, bdim, 'N', 'V', work)

                        ! do j = 1, 5
                        !       print*, w(j) - e_total
                        ! end do

                        ! allocate(dy(bdim))

                        ! do j = 1, bdim
                        !       dy(j) = j
                        ! end do
                        ! print*, 'sortowanie dy...'
                        ! print*, ''
                        ! call dsort(w, dy, bdim)

                        ! do j = 1, 5
                        !       print*, w(j) - e_total
                        ! end do


                        ! if (i == 1) then
                        !       s0 = 1
                        ! else
                        !       s0 = sum(iexci(1:i-1)) + 1
                        ! end if

                        ! s1 = sum(iexci(1:i))

                        ! s0_small = 2
                        ! s1_small = bdim

                        ! print*, dy(2)
                        ! do j = 2, 5+1
                        !       print*, 'rtssmall', rtsmall(j, dy(2))
                        ! end do

                        ! print*, ''
                        ! print*, 's0', s0, s1, s0_small, s1_small

                        ! call write_to_big_cisd2(i, irrep0, irrep1, nocc0, nocc, nvirt0, npair_small, npair_big, &
                        !       order, w, wrci, rtsmall, rtstart, s0, s1, s0_small, s1_small, dy)

                        ! do j = 1, iexci(i)
                        !       print*, wrci(j, i)-e_total
                        ! end do

                        ! do j = 1, 10
                        !       print*, 'rtbbbbiig', rtstart(j, s0)
                        ! end do

      !                   deallocate(work)
      !                   deallocate(dy)
      !                   deallocate(w)
      !                   deallocate(wi)
      !                   deallocate(iwork)
      !                   deallocate(ifail)
      !                   deallocate(hci)
      !                   deallocate(rtsmall)
      !                   deallocate(ltsmall)

      !             end if
      !       end do

      ! end subroutine task_cisd_dav



      subroutine sort_rtsmall(rtsmall, bdim, dy)
            real(F64), dimension(:, :), intent(inout) :: rtsmall
            integer, intent(in) :: bdim
            integer, dimension(:), intent(in) :: dy

            real(F64), dimension(:, :), allocatable :: temp
            integer :: i

            allocate(temp(bdim, bdim))

            do i = 1, bdim
                  temp(:, i) = rtsmall(:, dy(i))
            end do

            rtsmall = temp

            deallocate(temp)

      end subroutine sort_rtsmall


      subroutine task_cc_grad_one(tm, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, dm, r)

            real(F64), dimension(:), intent(out) :: tm
            real(F64)                                        :: tmxx
            real(F64)                                        :: tmyy
            real(F64)                                        :: tmzz

            double precision, dimension(:, :), intent(in)                       :: mocoeff
            integer, intent(in)                                                 :: nocc0, nvirt0
            integer, intent(in)                                                 :: nocc1, nvirt1
            integer, intent(in)                                                 :: nocc, nvirt
            double precision, dimension(:,:), intent(in)                        :: dm
            integer, intent(in) :: r

            double precision, dimension(:, :), allocatable :: gvxao, gvyao, gvzao
            double precision, dimension(:, :), allocatable :: gvxmo, gvymo, gvzmo

            integer :: nft2, l1, l2

            allocate(gvxao(CC_NORB, CC_NORB))
            allocate(gvyao(CC_NORB, CC_NORB))
            allocate(gvzao(CC_NORB, CC_NORB))
            allocate(gvxmo(CC_NORB, CC_NORB))
            allocate(gvymo(CC_NORB, CC_NORB))
            allocate(gvzmo(CC_NORB, CC_NORB))
            
            gvxao = zero
            gvyao = zero
            gvzao = zero
            
            print *, "WARNING! ONLY DERIVATIVES WITH RESPECT TO ATOM", CC_NON, "ARE COMPUTED"

            if (SLATER_BASIS) then

                  call msg ("FROM TRANSMOM_DIP CCSD.f90: SLATER=TRUE")
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_whole_matrix(nft2, l2, 29, gvxao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_whole_matrix(nft2, l2, 30, gvyao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_whole_matrix(nft2, l2, 31, gvzao)
                  close(nft2)
!                  print*, 'gvxao'
!                  print*, gvxao
!                  print*, 'gvzao'
!                  print*, gvzao
!                  print*, 'gvyao'
!                  print*, gvyao
                  if (ECP_GRAD) then
                        call msg('NOT IMPLEMENTED')
                        stop
                  end if
            else
                  call nfield1e(gvxao, gvyao, gvzao, r)
                  if (ECP_GRAD) call pseudopot_grad(gvxao, gvyao, gvzao, r)
            end if

            call smfill(gvxao)
            call smfill(gvyao)
            call smfill(gvzao)

            call atbc3(gvxmo, mocoeff, gvxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(gvymo, mocoeff, gvyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(gvzmo, mocoeff, gvzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call tm_part(tmxx, gvxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tmyy, gvymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            print*, 'PLUSZON'
            call tm_part(tmzz, gvzmo, dm, nocc0, nocc1, nvirt0, nvirt1)

            
            tm(1) = tmxx
            tm(2) = tmyy
            tm(3) = tmzz
            

      end subroutine task_cc_grad_one


      subroutine task_cc_grad(tmxx, tmyy, tmzz, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
            dm_gamma, dm_xi)

            real(F64), intent(out)                                              :: tmxx
            real(F64), intent(out)                                              :: tmyy
            real(F64), intent(out)                                              :: tmzz

            double precision, dimension(:, :), intent(in)                       :: mocoeff
            integer, intent(in)                                                 :: nocc0, nvirt0
            integer, intent(in)                                                 :: nocc1, nvirt1
            integer, intent(in)                                                 :: nocc, nvirt
            double precision, dimension(:,:), intent(in)                      :: dm_gamma, dm_xi

            double precision, dimension(:, :), allocatable :: gvxao, gvyao, gvzao
            double precision, dimension(:, :), allocatable :: gvxmo, gvymo, gvzmo

            allocate(gvxao(CC_NORB, CC_NORB))
            allocate(gvyao(CC_NORB, CC_NORB))
            allocate(gvzao(CC_NORB, CC_NORB))
            allocate(gvxmo(CC_NORB, CC_NORB))
            allocate(gvymo(CC_NORB, CC_NORB))
            allocate(gvzmo(CC_NORB, CC_NORB))

            gvxao = zero
            gvyao = zero
            gvzao = zero
            !
            ! The gradient of the total external potential
            ! (Coulomb potential + pseudopotential, AO basis)
            !
            print *, "WARNING! ONLY DERIVATIVES WITH RESPECT TO ATOM 2 ARE COMPUTED"
            call nfield1e(gvxao, gvyao, gvzao, 2)
            if (ECP_GRAD) call pseudopot_grad(gvxao, gvyao, gvzao, 2)

            call smfill(gvxao)
            call smfill(gvyao)
            call smfill(gvzao)
            !
            ! AO -> MO transform
            !
            call atbc3(gvxmo, mocoeff, gvxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(gvymo, mocoeff, gvyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(gvzmo, mocoeff, gvzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            !
            ! Contract one-electron integrals with the transition density
            ! matrix to obtain transition moments
            !
            call tmccsd_density(tmxx, nocc0, nocc1, nvirt0, nvirt1, &
                  gvxmo, gvxmo, nocc, nvirt, dm_gamma, dm_xi)
            call tmccsd_density(tmyy, nocc0, nocc1, nvirt0, nvirt1, &
                  gvymo, gvymo, nocc, nvirt, dm_gamma, dm_xi)
            call tmccsd_density(tmzz, nocc0, nocc1, nvirt0, nvirt1, &
                  gvzmo, gvzmo, nocc, nvirt, dm_gamma, dm_xi)

      end subroutine task_cc_grad

      subroutine cc_display_trans_vgrad(tmxx, tmyy, tmzz, eexct, w, degener, nacme)
            real(F64),  intent(in)                         :: tmxx
            real(F64),  intent(in)                         :: tmyy
            real(F64),  intent(in)                         :: tmzz
            real(F64),  intent(in)                         :: eexct
            integer, intent(in)                        :: w
            integer, dimension(:), intent(in)          :: degener
            real(F64), intent(out)                     :: nacme

            double precision :: tmsum
            double precision, dimension(:), allocatable :: s
            integer :: k
            integer :: sum, gk

            tmsum = tmzz

            if(tmsum.lt.zero)then
                  call msg("NONADIABATIC COUPLING LESS THEN ZERO:")
                  print*, tmsum
            else
                  call msg("NONADIABATIC COUPLING GREATER THEN ZERO:")
                  print*, tmsum

            end if
            nacme = sqrt(abs(tmsum)) / eexct

            !                                                                                                                        
            ! NONADIABATIC COUPLING SUMMARY        
            !                                                                                                         
            call blankline
            call toprule_double()
            call msg('NONADIABATIC RADIAL COUPLING')
            call nonad_table_start()

            call nonad_table_continue(w, eexct, nacme, 100)

            call toprule_double()

            call midrule()
      end subroutine cc_display_trans_vgrad

      subroutine task_line_strength(tm, ls)
            real(F64), dimension(:), intent(in) :: tm
            real(F64), intent(out) :: ls

            ls = sum(tm)

      end subroutine task_line_strength

      subroutine task_line_strength_quad(tm, ls)
            real(F64), dimension(:), intent(in) :: tm
            real(F64), intent(out) :: ls

            ls = tm(1) + tm(2) + tm(3) + two * (tm(4) + tm(5) + tm(6))


      end subroutine task_line_strength_quad



      subroutine task_ccsd_oscstrength(tmxx, tmyy, tmzz, eexct, w, degener, os, Aik)
            real(F64), intent(in) :: tmxx
            real(F64), intent(in) :: tmyy
            real(F64), intent(in) :: tmzz
            real(F64), intent(in) :: eexct
            real(F64), intent(out) :: os
            real(F64), intent(out) :: Aik
            integer, intent(in)                        :: w
            integer, dimension(:), intent(in)          :: degener
            real(F64) :: Aik1, Aik2, Aik21, Aik3, Aik31, Aik4, Aik41

            double precision :: tmsum, os2
            integer :: gk

            tmsum = tmxx + tmyy + tmzz
            print*, 'tmsum', tmsum
            os = frac23 * eexct * tmsum

            !
            ! DIPOLE OSCSTRENGTHS SUMMARY
            ! 
            call blankline
            call toprule_double()
            call msg('DIPOLE TRANSITION PROPERTIES')
            call dipole_table_start()

            ! gk = compgk(w, degener, (.not. CC_DEGENERACY), CC_NLEVELS)
            gk = 100
            Aik = (totransprob_dipole(tmsum, eexct, 1))*1.d-8

            if(abs(Aik).lt.1.d-12)then
                  Aik = zero
            end if
            if(abs(os).lt.1.d-12)then
                  os = zero
            end if
            call dipole_table_continue(w, eexct, os, Aik, gk)
            call toprule_double()
            call midrule()

      end subroutine task_ccsd_oscstrength

      subroutine properties_summary(wrdav_small, ntargetvecs, dip_os, &
            dip_trans, quad_ls, quad_trans, nacme)

            real(F64), dimension(:), intent(in) :: wrdav_small
            integer, intent(in) :: ntargetvecs
            real(F64), dimension(:), intent(in) :: dip_os
            real(F64), dimension(:), intent(in) :: dip_trans
            real(F64), dimension(:), intent(in) :: quad_ls
            real(F64), dimension(:), intent(in) :: quad_trans
            real(F64), dimension(:), intent(in) :: nacme

            integer :: i

            call blankline
            call toprule_double()
            call msg('DIPOLE TRANSITION PROPERTIES')
            call dipole_table_start()

            do i = 1, ntargetvecs
                  call dipole_table_continue(i, wrdav_small(i), dip_os(i), dip_trans(i), 100)
            end do
            call toprule_double()
            call midrule()

            call blankline
            call toprule_double()
            call msg('QUADRUPOLE TRANSITION PROBABILITIES')
            call quadrupole_table_start()
            do i = 1, ntargetvecs
                  call quadrupole_table_continue(i, wrdav_small(i), quad_ls(i), quad_trans(i), 100)
            end do
            call toprule_double()
            call midrule()

            call blankline
            call toprule_double()
            call msg('NONADIABATIC RADIAL COUPLING')
            call nonad_table_start()
            do i = 1, ntargetvecs
                  call nonad_table_continue(i, wrdav_small(i), nacme(i), 100)
            end do
            call toprule_double()

            call midrule()

      end subroutine properties_summary

      subroutine task_transmom_dip(tmxx, tmyy, tmzz, mocoeff, nocc0, nocc1, nvirt0, nvirt1,nocc, nvirt, &
            dm_gamma, dm_xi)

            real(F64),  intent(out)                         :: tmxx
            real(F64),  intent(out)                         :: tmyy
            real(F64),  intent(out)                         :: tmzz
            double precision, dimension(:, :), intent(in)                       :: mocoeff
            integer, intent(in)                                                 :: nocc0, nvirt0
            integer, intent(in)                                                 :: nocc1, nvirt1
            integer, intent(in)                                                 :: nocc, nvirt
            double precision, dimension(:,:), intent(in)                      :: dm_gamma, dm_xi

            double precision, dimension(:, :), allocatable :: dipxao, dipyao, dipzao
            double precision, dimension(:, :), allocatable :: dipxmo, dipymo, dipzmo

            integer :: nft2, l1, l2

            allocate(dipxao(CC_NORB, CC_NORB))
            allocate(dipyao(CC_NORB, CC_NORB))
            allocate(dipzao(CC_NORB, CC_NORB))
            allocate(dipxmo(CC_NORB, CC_NORB))
            allocate(dipymo(CC_NORB, CC_NORB))
            allocate(dipzmo(CC_NORB, CC_NORB))
            !
            ! Matrices of the electronic dipole moment operator
            ! (AO basis)
            !
            if (SLATER_BASIS) then

                  call msg ("FROM TRANSMOM_DIP CCSD.f90: SLATER=TRUE")
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 4, dipxao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 5, dipyao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 6, dipzao)
                  close(nft2)
            else
                  call dipole(dipxao, dipyao, dipzao, CHCENTER)
            end if

            call smfill(dipxao)
            call smfill(dipyao)
            call smfill(dipzao)
            !
            ! Matrices of the electronic dipole moment operator
            ! (MO basis)
            !
            call atbc3(dipxmo, mocoeff, dipxao, mocoeff, CC_NORB, CC_NORB, CC_NORB) 
            call atbc3(dipymo, mocoeff, dipyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(dipzmo, mocoeff, dipzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)


            call tmccsd_density(tmxx, nocc0, nocc1, nvirt0, nvirt1, &
                  dipxmo, dipxmo, nocc, nvirt, dm_gamma, dm_xi)
            call tmccsd_density(tmyy, nocc0, nocc1, nvirt0, nvirt1, &
                  dipymo, dipymo, nocc, nvirt, dm_gamma, dm_xi)
            call tmccsd_density(tmzz, nocc0, nocc1, nvirt0, nvirt1, &
                  dipzmo, dipzmo, nocc, nvirt, dm_gamma, dm_xi)

            deallocate(dipxao)
            deallocate(dipyao)
            deallocate(dipzao)
            deallocate(dipxmo)
            deallocate(dipymo)
            deallocate(dipzmo)
      end subroutine task_transmom_dip

      subroutine task_transmom_quad(tmxxxx, tmyyyy, tmzzzz, tmyxyx, tmzxzx, tmzyzy, &
            mocoeff, nocc0, nocc1, nvirt0, nvirt1,nocc, nvirt, &
            dm_gamma, dm_xi)

            real(F64), intent(out)                                              :: tmxxxx
            real(F64), intent(out)                                              :: tmyyyy
            real(F64), intent(out)                                              :: tmzzzz
            real(F64), intent(out)                                              :: tmyxyx
            real(F64), intent(out)                                              :: tmzxzx
            real(F64), intent(out)                                              :: tmzyzy

            double precision, dimension(:, :), intent(in)                       :: mocoeff
            integer, intent(in)                                                 :: nocc0, nvirt0
            integer, intent(in)                                                 :: nocc1, nvirt1
            integer, intent(in)                                                 :: nocc, nvirt
            double precision, dimension(:,:), intent(in)                        :: dm_gamma, dm_xi

            double precision, dimension(:, :), allocatable :: quadxxao, quadyyao, quadzzao
            double precision, dimension(:, :), allocatable :: quadxxmo, quadyymo, quadzzmo
            double precision, dimension(:, :), allocatable :: quadyxao, quadzxao, quadzyao
            double precision, dimension(:, :), allocatable :: quadyxmo, quadzxmo, quadzymo

            allocate(quadxxao(CC_NORB, CC_NORB))
            allocate(quadyyao(CC_NORB, CC_NORB))
            allocate(quadzzao(CC_NORB, CC_NORB))
            allocate(quadyxao(CC_NORB, CC_NORB))
            allocate(quadzxao(CC_NORB, CC_NORB))
            allocate(quadzyao(CC_NORB, CC_NORB))

            allocate(quadxxmo(CC_NORB, CC_NORB))
            allocate(quadyymo(CC_NORB, CC_NORB))
            allocate(quadzzmo(CC_NORB, CC_NORB))
            allocate(quadyxmo(CC_NORB, CC_NORB))
            allocate(quadzxmo(CC_NORB, CC_NORB))
            allocate(quadzymo(CC_NORB, CC_NORB))

            call quadrupole(quadxxao, quadyyao, quadzzao, quadyxao, quadzxao, quadzyao, CHCENTER, &
                  QUAD_TRACELESS_SHORTLEY)

            call smfill(quadxxao)
            call smfill(quadyyao)
            call smfill(quadzzao)
            call smfill(quadyxao)
            call smfill(quadzxao)
            call smfill(quadzyao)

            call atbc3(quadxxmo, mocoeff, quadxxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadyymo, mocoeff, quadyyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(quadyxmo, mocoeff, quadyxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzxmo, mocoeff, quadzxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzymo, mocoeff, quadzyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)


            print*, 'tmxxxx'
            call tmccsd_density(tmxxxx,&
                  nocc0, nocc1, nvirt0, nvirt1, quadxxmo, quadxxmo,nocc, nvirt, &
                  dm_gamma, dm_xi)
            call tmccsd_density(tmyyyy,&
                  nocc0, nocc1, nvirt0, nvirt1, quadyymo, quadyymo,nocc, nvirt, &
                  dm_gamma, dm_xi)
            call tmccsd_density(tmzzzz,&
                  nocc0, nocc1, nvirt0, nvirt1, quadzzmo, quadzzmo,nocc, nvirt, &
                  dm_gamma, dm_xi)


            call tmccsd_density(tmyxyx,&
                  nocc0, nocc1, nvirt0, nvirt1, quadyxmo, quadyxmo,nocc, nvirt, &
                  dm_gamma, dm_xi)
            call tmccsd_density(tmzxzx, &
                  nocc0, nocc1, nvirt0, nvirt1, quadzxmo, quadzxmo,nocc, nvirt, &
                  dm_gamma, dm_xi)
            call tmccsd_density(tmzyzy,&
                  nocc0, nocc1, nvirt0, nvirt1, quadzymo, quadzymo,nocc, nvirt, &
                  dm_gamma, dm_xi)

            deallocate(quadxxao)
            deallocate(quadyyao)
            deallocate(quadzzao)
            deallocate(quadxxmo)
            deallocate(quadyymo)
            deallocate(quadzzmo)

            deallocate(quadyxao)
            deallocate(quadzxao)
            deallocate(quadzyao)
            deallocate(quadyxmo)
            deallocate(quadzxmo)
            deallocate(quadzymo)
      end subroutine task_transmom_quad



      subroutine task_transmom(tm_wm, mocoeff, dm, nocc0, nocc1, nvirt0, nvirt1, op)

        real(F64), dimension(:), intent(out)    :: tm_wm
        real(F64), dimension(:, :), intent(in)  :: mocoeff
        real(F64), dimension(:,:), intent(in)   :: dm
        integer, intent(in)                     :: nocc0, nocc1
        integer, intent(in)                     :: nvirt0, nvirt1
        integer, intent(in)                     :: op
        real(F64), dimension(:, :), allocatable :: dipxao, dipyao, dipzao
        real(F64), dimension(:, :), allocatable :: dipxmo, dipymo, dipzmo
        real(F64), dimension(:, :), allocatable :: quadxxao, quadyyao, quadzzao
        real(F64), dimension(:, :), allocatable :: quadxxmo, quadyymo, quadzzmo
        real(F64), dimension(:, :), allocatable :: quadyxao, quadzxao, quadzyao
        real(F64), dimension(:, :), allocatable :: quadyxmo, quadzxmo, quadzymo
        real(F64), dimension(:, :), allocatable :: oneao, onemo
        double precision, dimension(:, :), allocatable :: gvxao, gvyao, gvzao
        double precision, dimension(:, :), allocatable :: gvxmo, gvymo, gvzmo
        real(F64) :: tmxx, tmxxxx
        real(F64) :: w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12
        integer :: i
        integer :: nft2, l1, l2
        integer :: j1, j2

        tm_wm = zero
        select case(op)
        case(TRANS_DIP)
           allocate(dipxao(CC_NORB, CC_NORB))
           allocate(dipyao(CC_NORB, CC_NORB))
           allocate(dipzao(CC_NORB, CC_NORB))

           allocate(dipxmo(CC_NORB, CC_NORB))
           allocate(dipymo(CC_NORB, CC_NORB))
           allocate(dipzmo(CC_NORB, CC_NORB))

            if (SLATER_BASIS) then
                  call msg ("FROM TRANSMOM_DIP CCSD.f90: SLATER=TRUE")
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 4, dipxao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 5, dipyao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 6, dipzao)
                  close(nft2)
            else
                  call dipole(dipxao, dipyao, dipzao, CHCENTER)
            end if


           call smfill(dipxao)
           call smfill(dipyao)
           call smfill(dipzao)

           call atbc3(dipxmo, mocoeff, dipxao, mocoeff, CC_NORB, CC_NORB, CC_NORB) 
           call atbc3(dipymo, mocoeff, dipyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
           call atbc3(dipzmo, mocoeff, dipzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

           call tm_part(tm_wm(1), dipxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
           call tm_part(tm_wm(2), dipymo, dm, nocc0, nocc1, nvirt0, nvirt1)
           call tm_part(tm_wm(3), dipzmo, dm, nocc0, nocc1, nvirt0, nvirt1)

           deallocate(dipxao)
           deallocate(dipyao)
           deallocate(dipzao)
           deallocate(dipxmo)
           deallocate(dipymo)
           deallocate(dipzmo)

        case(TRANS_QUAD)
           allocate(quadxxao(CC_NORB, CC_NORB))
           allocate(quadyyao(CC_NORB, CC_NORB))
           allocate(quadzzao(CC_NORB, CC_NORB))

           allocate(quadyxao(CC_NORB, CC_NORB))
           allocate(quadzxao(CC_NORB, CC_NORB))
           allocate(quadzyao(CC_NORB, CC_NORB))

           allocate(quadxxmo(CC_NORB, CC_NORB))
           allocate(quadyymo(CC_NORB, CC_NORB))
           allocate(quadzzmo(CC_NORB, CC_NORB))

           allocate(quadyxmo(CC_NORB, CC_NORB))
           allocate(quadzxmo(CC_NORB, CC_NORB))
           allocate(quadzymo(CC_NORB, CC_NORB))

           if (SLATER_BASIS) then
                  call msg ("FROM TRANSMOM_Quad CCSD.f90: SLATER=TRUE")
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 9, quadzzao)
                  close(nft2)
                  call smfill(quadzzao)
                  call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  call tm_part(tm_wm(3), quadzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  print*, 'zz', tm_wm(3)
            else
                  call quadrupole(quadxxao, quadyyao, quadzzao, quadyxao, quadzxao, quadzyao, CHCENTER, &
                        !                 QUAD_PRIMITIVE)
                        !                 QUAD_TRACELESS_BUCKINGHAM)
                        QUAD_TRACELESS_SHORTLEY)
                  
                  call smfill(quadxxao)
                  call smfill(quadyyao)
                  call smfill(quadzzao)
                  call smfill(quadyxao)
                  call smfill(quadzxao)
                  call smfill(quadzyao)
                  
                  call atbc3(quadxxmo, mocoeff, quadxxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  call atbc3(quadyymo, mocoeff, quadyyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  
                  call atbc3(quadyxmo, mocoeff, quadyxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  call atbc3(quadzxmo, mocoeff, quadzxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  call atbc3(quadzymo, mocoeff, quadzyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

                  call tm_part(tm_wm(1), quadxxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  call tm_part(tm_wm(2), quadyymo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  call tm_part(tm_wm(3), quadzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  
                  call tm_part(tm_wm(4), quadyxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  call tm_part(tm_wm(5), quadzxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  call tm_part(tm_wm(6), quadzymo, dm, nocc0, nocc1, nvirt0, nvirt1)
                  
                  print*, 'xx', tm_wm(1)
                  print*, 'yy', tm_wm(2)
                  print*, 'zz', tm_wm(3)
                  print*, 'yx', tm_wm(4)
                  print*, 'zx', tm_wm(5)
                  print*, 'zy', tm_wm(6)
            end if
                  
            deallocate(quadxxao)
            deallocate(quadyyao)
            deallocate(quadzzao)
            deallocate(quadxxmo)
            deallocate(quadyymo)
            deallocate(quadzzmo)
            
            deallocate(quadyxao)
            deallocate(quadzxao)
            deallocate(quadzyao)
            deallocate(quadyxmo)
            deallocate(quadzxmo)
            deallocate(quadzymo)
            

        case(TRANS_NON)

            allocate(gvxao(CC_NORB, CC_NORB))
            allocate(gvyao(CC_NORB, CC_NORB))
            allocate(gvzao(CC_NORB, CC_NORB))
            allocate(gvxmo(CC_NORB, CC_NORB))
            allocate(gvymo(CC_NORB, CC_NORB))
            allocate(gvzmo(CC_NORB, CC_NORB))
            
            gvxao = zero
            gvyao = zero
            gvzao = zero

            print *, "WARNING! ONLY DERIVATIVES WITH RESPECT TO ATOM 2 ARE COMPUTED"
            call nfield1e(gvxao, gvyao, gvzao, 2)
            if (ECP_GRAD) call pseudopot_grad(gvxao, gvyao, gvzao, 2)

            call smfill(gvxao)
            call smfill(gvyao)
            call smfill(gvzao)

            call atbc3(gvxmo, mocoeff, gvxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(gvymo, mocoeff, gvyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(gvzmo, mocoeff, gvzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call tm_part(tm_wm(1), gvxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tm_wm(2), gvymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tm_wm(3), gvzmo, dm, nocc0, nocc1, nvirt0, nvirt1)


        case(TRANS_ONEAO)
           allocate(oneao(CC_NORB, CC_NORB))

           oneao = zero
           do i = 1, CC_NORB
              oneao(i, i) = ten
           end do

           call tm_part(tm_wm(1), oneao, dm, nocc0, nocc1, nvirt0, nvirt1) 

           deallocate(oneao)

        case(TRANS_ONEMO)
           allocate(oneao(CC_NORB, CC_NORB))
           allocate(onemo(CC_NORB, CC_NORB))

           oneao = zero
           do i = 1, CC_NORB
              oneao(i, i) = ten
           end do

           call atbc3(onemo, mocoeff, oneao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

           call tm_part(tm_wm(1), onemo, dm, nocc0, nocc1, nvirt0, nvirt1)

           deallocate(oneao)
           deallocate(onemo)

        end select

      end subroutine task_transmom

      subroutine task_transmom_dip_exc_exc(tm_wm, mocoeff, dm, nocc0, nocc1, nvirt0, nvirt1)

            real(F64), dimension(:), intent(out)                  :: tm_wm
            double precision, dimension(:, :), intent(in)         :: mocoeff
            double precision, dimension(:,:), intent(in)          :: dm
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1


            double precision, dimension(:, :), allocatable :: dipxao, dipyao, dipzao
            double precision, dimension(:, :), allocatable :: dipxmo, dipymo, dipzmo
            real(F64) :: tmxx, tmyy, tmzz
            integer :: nft2, l1, l2


            allocate(dipxao(CC_NORB, CC_NORB))
            allocate(dipyao(CC_NORB, CC_NORB))
            allocate(dipzao(CC_NORB, CC_NORB))
            allocate(dipxmo(CC_NORB, CC_NORB))
            allocate(dipymo(CC_NORB, CC_NORB))
            allocate(dipzmo(CC_NORB, CC_NORB))
            !
            ! Matrices of the electronic dipole moment operator
            ! (AO basis)
            !

            if (SLATER_BASIS) then
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 4, dipxao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 5, dipyao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 6, dipzao)
                  close(nft2)
            else
                  call dipole(dipxao, dipyao, dipzao, CHCENTER)
            end if

            call smfill(dipxao)
            call smfill(dipyao)
            call smfill(dipzao)
            !
            ! Matrices of the electronic dipole moment operator
            ! (MO basis)
            !
            call atbc3(dipxmo, mocoeff, dipxao, mocoeff, CC_NORB, CC_NORB, CC_NORB) 
            call atbc3(dipymo, mocoeff, dipyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(dipzmo, mocoeff, dipzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            ! print*, ''
            ! print*, 'dipxmo'
            ! do i = 1, CC_NORB
            !       do j = 1, CC_NORB
            !             if (abs(dipxmo(i, j)).gt.1.d-5)then
            !                   print*, i, j, dipxmo(i, j)
            !             end if
            !       end do
            ! end do

            call tm_part(tmxx, dipxmo, dm, nocc0, nocc1, nvirt0, nvirt1)

            call tm_part(tmyy, dipymo, dm, nocc0, nocc1, nvirt0, nvirt1)

            call tm_part(tmzz, dipzmo, dm, nocc0, nocc1, nvirt0, nvirt1)


            tm_wm(1) = tmxx
            tm_wm(2) = tmyy
            tm_wm(3) = tmzz

            print*, 'tmxx', tmxx
            print*, 'tmyy', tmyy
            print*, 'tmzz', tmzz


            deallocate(dipxao)
            deallocate(dipyao)
            deallocate(dipzao)
            deallocate(dipxmo)
            deallocate(dipymo)
            deallocate(dipzmo)
      end subroutine task_transmom_dip_exc_exc

     subroutine task_transmom_so_exc_exc(tm_wm, mocoeff, dm, nocc0, nocc1, nvirt0, nvirt1, sym)

            real(F64), dimension(:), intent(out)                  :: tm_wm
            double precision, dimension(:, :), intent(in)         :: mocoeff
            double precision, dimension(:,:), intent(in)          :: dm
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer :: i, j
            integer, intent(in) :: sym


            double precision, dimension(:, :), allocatable :: soxao, soyao, sozao
            double precision, dimension(:, :), allocatable :: soxmo, soymo, sozmo


            real(F64) :: tmxx, tmyy, tmzz
            integer :: nft2, l1, l2


            allocate(soxao(CC_NORB, CC_NORB))
            allocate(soyao(CC_NORB, CC_NORB))
            allocate(sozao(CC_NORB, CC_NORB))
            allocate(soxmo(CC_NORB, CC_NORB))
            allocate(soymo(CC_NORB, CC_NORB))
            allocate(sozmo(CC_NORB, CC_NORB))

            !                                                                                                                                   
            ! Matrices of the electronic soole moment operator                                                                              
            ! (AO basis)                                                                                                                      
            !                                                                                                                                   

            if (SLATER_BASIS) then
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  if (sym == 1) then
                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_triangle(nft2, l2,35, soxao)
                        close(nft2)
                        
                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_triangle(nft2, l2, 36, soyao)
                        close(nft2)

                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_triangle(nft2, l2, 37, sozao)
                        close(nft2)

                        print*, 'CZYTAM SYMSYMSYM'

                        call smfill(soxao)
                        call smfill(soyao)
                        call smfill(sozao)

                        call atbc3(soxmo, mocoeff, soxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call atbc3(soymo, mocoeff, soyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call atbc3(sozmo, mocoeff, sozao, mocoeff, CC_NORB, CC_NORB, CC_NORB)                       
                        
                  else if (sym == 2) then
                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_lower_triangle(nft2, l2,35, soxao)
                        close(nft2)
                        
                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_lower_triangle(nft2, l2, 36, soyao)
                        close(nft2)

                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_lower_triangle(nft2, l2, 37, sozao)
                        close(nft2)

                        print*, 'CZYTAM ASYMASYMAYSM'
                        call amfill(soxao)
                        call amfill(soyao)
                        call amfill(sozao)
                        
                        call atbc3(soxmo, mocoeff, soxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call atbc3(soymo, mocoeff, soyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call atbc3(sozmo, mocoeff, sozao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  end if

            else
                  if (sym == 2) then
                        call so_pseudopot(soxao, soyao, sozao)
                        call amfill(soxao)
                        call amfill(soyao)
                        call amfill(sozao)
                        
                        call atbc3(soxmo, mocoeff, soxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call atbc3(soymo, mocoeff, soyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call atbc3(sozmo, mocoeff, sozao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                  end if
            end if

            ! print*, 'macierz soxao'
            ! do i = 1, size(soxao, dim=1)
            !       do j = 1, size(soxao, dim=2)
            !             if (abs(soxao(i,j)).gt.1.d-4)then
            !                   print*, i, j, soxao(i, j)
            !             end if
            !       end do
            ! end do

            ! print*, 'macierz soyao'
            ! do i = 1, size(soyao, dim=1)
            !       do j = 1, size(soyao, dim=2)
            !             if (abs(soyao(i,j)).gt.1.d-4)then
            !                   print*, i, j, soyao(i, j)
            !             end if
            !       end do
            ! end do
            



            ! print*, 'dmemememememem', nocc0, nocc1, nvirt0, nvirt1
            ! do i = 1, size(dm, dim=1)
            !       do j = 1, size(dm, dim=2)
            !             if (abs(dm(i,j)).gt.1.d-4)then
            !                   print*, i, j, dm(i, j)
            !             end if
            !       end do
            ! end do

            ! print*, 'macierz so-x'
            ! do i = 1, size(soxmo, dim=1)
            !       do j = 1, size(soxmo, dim=2)
            !             if (abs(soxmo(i,j)).gt.1.d-4)then
            !                   print*, i, j, soxmo(i, j)
            !             end if
            !       end do
            ! end do

            ! print*, 'macierz so-y'
            ! do i = 1, size(soymo, dim=1)
            !       do j = 1, size(soymo, dim=2)
            !             if (abs(soymo(i,j)).gt.1.d-4)then
            !                   print*, i, j, soymo(i, j)
            !             end if
            !       end do
            ! end do

            ! print*, 'macierz so-z'
            ! do i = 1, size(sozmo, dim=1)
            !       do j = 1, size(sozmo, dim=2)
            !             if (abs(sozmo(i,j)).gt.1.d-8)then
            !                   print*, i, j, sozmo(i, j)
            !             end if
            !       end do
            ! end do


            call tm_part(tmxx, soxmo, dm, nocc0, nocc1, nvirt0, nvirt1)            
            print*, 'macierz so-y'
            call tm_part(tmyy, soymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            print*, 'macierze so-z'
            call tm_part(tmzz, sozmo, dm, nocc0, nocc1, nvirt0, nvirt1)


            tm_wm(1) = tmxx
            tm_wm(2) = tmyy
            tm_wm(3) = tmzz
            
            print*, 'tmxx_so', tmxx
            print*, 'tmyy_so', tmyy
            print*, 'tmzz_so', tmzz

            deallocate(soxao)
            deallocate(soyao)
            deallocate(sozao)
            deallocate(soxmo)
            deallocate(soymo)
            deallocate(sozmo)
      end subroutine task_transmom_so_exc_exc

      subroutine task_transmom_dq_exc_exc(tm_wm, mocoeff, dm, nocc0, nocc1, nvirt0, nvirt1)

            real(F64), dimension(:), intent(out)                  :: tm_wm
            double precision, dimension(:, :), intent(in)         :: mocoeff
            double precision, dimension(:,:), intent(in)          :: dm
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            double precision, dimension(:, :), allocatable :: dipxao, dipyao, dipzao
            double precision, dimension(:, :), allocatable :: dipxmo, dipymo, dipzmo
            double precision, dimension(:, :), allocatable :: quadxxao, quadyyao, quadzzao
            double precision, dimension(:, :), allocatable :: quadxxmo, quadyymo, quadzzmo
            double precision, dimension(:, :), allocatable :: quadyxao, quadzxao, quadzyao
            double precision, dimension(:, :), allocatable :: quadyxmo, quadzxmo, quadzymo
            double precision, dimension(:, :), allocatable :: oneao, onemo
            real(F64), dimension(:, :), allocatable :: octxyzao, octxxxao, octyyyao, octzzzao
            real(F64), dimension(:, :), allocatable :: octxxyao, octxyyao, octxzzao, octxxzao, octyzzao, octyyzao
            real(F64), dimension(:, :), allocatable :: octxyzmo, octxxxmo, octyyymo, octzzzmo
            real(F64), dimension(:, :), allocatable :: octxxymo, octxyymo, octxzzmo, octxxzmo, octyzzmo, octyyzmo
            real(F64) :: tmxx, tmxxxx
            real(F64) :: w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12
            real(F64) :: oct1, oct2, oct3, oct4, oct5, oct6, oct7, oct8, oct9, oct10
            integer :: i, j

            allocate(dipxao(CC_NORB, CC_NORB))
            allocate(dipyao(CC_NORB, CC_NORB))
            allocate(dipzao(CC_NORB, CC_NORB))

            allocate(dipxmo(CC_NORB, CC_NORB))
            allocate(dipymo(CC_NORB, CC_NORB))
            allocate(dipzmo(CC_NORB, CC_NORB))

            allocate(quadxxao(CC_NORB, CC_NORB))
            allocate(quadyyao(CC_NORB, CC_NORB))
            allocate(quadzzao(CC_NORB, CC_NORB))

            allocate(quadyxao(CC_NORB, CC_NORB))
            allocate(quadzxao(CC_NORB, CC_NORB))
            allocate(quadzyao(CC_NORB, CC_NORB))

            allocate(quadxxmo(CC_NORB, CC_NORB))
            allocate(quadyymo(CC_NORB, CC_NORB))
            allocate(quadzzmo(CC_NORB, CC_NORB))

            allocate(quadyxmo(CC_NORB, CC_NORB))
            allocate(quadzxmo(CC_NORB, CC_NORB))
            allocate(quadzymo(CC_NORB, CC_NORB))

            allocate(oneao(CC_NORB, CC_NORB))
            allocate(onemo(CC_NORB, CC_NORB))

            allocate(octxyzao(CC_NORB, CC_NORB))
            allocate(octxxxao(CC_NORB, CC_NORB))
            allocate(octyyyao(CC_NORB, CC_NORB))
            allocate(octzzzao(CC_NORB, CC_NORB))

            allocate(octxxyao(CC_NORB, CC_NORB))
            allocate(octxyyao(CC_NORB, CC_NORB))
            allocate(octxxzao(CC_NORB, CC_NORB))
            allocate(octxzzao(CC_NORB, CC_NORB))
            allocate(octyyzao(CC_NORB, CC_NORB))
            allocate(octyzzao(CC_NORB, CC_NORB))

            allocate(octxyzmo(CC_NORB, CC_NORB))
            allocate(octxxxmo(CC_NORB, CC_NORB))
            allocate(octyyymo(CC_NORB, CC_NORB))
            allocate(octzzzmo(CC_NORB, CC_NORB))

            allocate(octxxymo(CC_NORB, CC_NORB))
            allocate(octxyymo(CC_NORB, CC_NORB))
            allocate(octxxzmo(CC_NORB, CC_NORB))
            allocate(octxzzmo(CC_NORB, CC_NORB))
            allocate(octyyzmo(CC_NORB, CC_NORB))
            allocate(octyzzmo(CC_NORB, CC_NORB))



            !
            ! Matrices of the electronic dipole moment operator
            ! (AO basis)
            !
            call dipole(dipxao, dipyao, dipzao, CHCENTER)

            call quadrupole(quadxxao, quadyyao, quadzzao, quadyxao, quadzxao, quadzyao, CHCENTER, &
                  QUAD_TRACELESS_BUCKINGHAM)
            ! print*, 'quadxxao'
            ! do i = 1, size(quadxxao, dim=1)                                                                                                                                            
            !       do j = 1, size(quadxxao, dim=2)                                                                                                                                      
            !             print*, quadxxao(i, j)                                                                                                                                         
            !       end do                                                                                                                                                               
            ! end do            
            ! print*, 'mocoeff', size(mocoeff, dim=1), size(mocoeff, dim=2)
            ! do i = 1, size(mocoeff, dim=1)                                                                                                                                            
            !       do j = 1, size(mocoeff, dim=2)                                                                                                                                      
            !             print*, mocoeff(i, j)                                                                                                                                         
            !       end do                                                                                                                                                               
            ! end do                                                                                                                                               
            



           call octupole(octxyzao, octxxxao, octyyyao, octzzzao, &
                 octxxyao, octxyyao, octxxzao, octxzzao, octyyzao, octyzzao, CHCENTER, QUAD_PRIMITIVE)

            call smfill(dipxao)
            call smfill(dipyao)
            call smfill(dipzao)
            call smfill(quadxxao)


            ! print*, 'quadxxmo'
            ! do i = 1, size(quadxxmo, dim=1)                                                                                                                                            
            !       do j = 1, size(quadxxmo, dim=2)                                                                                                                                      
            !             print*, quadxxmo(i, j)                                                                                                                                         
            !       end do                                                                                                                                                               
            ! end do                                                                                                                                                                     


           call smfill(quadyyao)
           call smfill(quadzzao)
           call smfill(quadyxao)
           call smfill(quadzxao)
           call smfill(quadzyao)

            call smfill(octxyzao)
            call smfill(octxxxao)
            call smfill(octyyyao)
            call smfill(octzzzao)

            call smfill(octxxyao)
            call smfill(octxyyao)
            call smfill(octxzzao)
            call smfill(octxxzao)
            call smfill(octyyzao)
            call smfill(octyzzao)

            oneao = zero
            do i = 1, CC_NORB
                  oneao(i, i) = ten
            end do

            ! do i = 1, size(quadxxao, dim=1)
            !       do j = 1, size(quadxxao, dim=2)
            !             print*, quadxxao(i, j)
            !       end do
            ! end do
            ! stop


            !
            ! Matrices of the electronic dipole moment operator
            ! (MO basis)
            !
            call atbc3(onemo, mocoeff, oneao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(dipxmo, mocoeff, dipxao, mocoeff, CC_NORB, CC_NORB, CC_NORB) 
            call atbc3(dipymo, mocoeff, dipyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(dipzmo, mocoeff, dipzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(quadxxmo, mocoeff, quadxxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadyymo, mocoeff, quadyyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(quadyxmo, mocoeff, quadyxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzxmo, mocoeff, quadzxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzymo, mocoeff, quadzyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(octxxxmo, mocoeff, octxxxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octyyymo, mocoeff, octyyyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octzzzmo, mocoeff, octzzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octxyzmo, mocoeff, octxyzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(octxxymo, mocoeff, octxxyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octxyymo, mocoeff, octxyyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octxxzmo, mocoeff, octxxzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octxzzmo, mocoeff, octxzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octyzzmo, mocoeff, octyzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(octyyzmo, mocoeff, octyyzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)



            ! print*, 'octszystkie onemo'

            ! do i = 1, CC_NORB
            !       print*, 'ONEMO', onemo(i, i)
            ! end do

            call tm_part(w1, dipxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(w2, dipymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(w3, dipzmo, dm, nocc0, nocc1, nvirt0, nvirt1)

            ! do i = 1, size(quadxxmo, dim=1)
            !       do j = 1, size(quadxxmo, dim=2)
            !             print*, 'quadxxmo', quadxxmo(i, j)
            !       end do
            ! end do

            call tm_part(w4, quadxxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, '----------------------------------------------------------------------------------'
            call tm_part(w5, quadyymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(w6, quadzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)

            call tm_part(w7, quadyxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(w8, quadzxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(w9, quadzymo, dm, nocc0, nocc1, nvirt0, nvirt1)

            call tm_part(w10, onemo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(w11, oneao, dm, nocc0, nocc1, nvirt0, nvirt1) 


            ! call tm_part(oct1, octxyzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct2, octxxxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct3, octyyymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct4, octzzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            
            ! call tm_part(oct5, octxxymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct6, octxyymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct7, octxxzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct8, octxzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct9, octyzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(oct10, octyyzmo, dm, nocc0, nocc1, nvirt0, nvirt1)



            print*, 'w1= dipx', w1
            print*, 'w2= dipy', w2
            print*, 'w3= dipz', w3
            print*, 'w4= qxx', w4
            print*, 'w5= qyy', w5
            print*, 'w6= qzz', w6
            print*, 'w7= qyx', w7
            print*, 'w8= qzx', w8
            print*, 'w9= qzy', w9
            print*, 'w10= onemo', w10
            print*, 'w11= oneao', w11

            ! print*, 'oct1= xyz', oct1
            ! print*, 'oct2= xxx', oct2
            ! print*, 'oct3= yyy', oct3
            ! print*, 'oct4= zzz', oct4
            ! print*, 'oct5= xxy', oct5
            ! print*, 'oct6= xyy', oct6
            ! print*, 'oct7= xxz', oct7
            ! print*, 'oct8= xzz', oct8
            ! print*, 'oct9= yzz', oct9
            ! print*, 'oct10= yyz', oct10
            
            print*, 'zostalo wybrane, w1, w10, w3'
            print*, ''
            print*, w1
            print*, w2
            print*, w3
            print*, w4
            print*, w5
            print*, w6
            print*, w7
            print*, w8
            print*, w9
            print*, w10
            print*, w11

            print*, ''
            print*, oct1
            print*, oct2
            print*, oct3
            print*, oct4
            print*, oct5
            print*, oct6
            print*, oct7
            print*, oct8
            print*, oct9
            print*, oct10
            print*, ''
            

            
            tm_wm(1) = w1
            tm_wm(2) = w10
            tm_wm(3) = w3!w4 + w5 + w6

            ! print*, 'tm_wm(1) = dipx, tm_wm(2) = onemo, tm_wm(3) = xx+yy+zz'

            ! print*, 'w1w2w3', w1, w2, w3
            ! tm_wm(3) = w1 + w2 + w3

            ! call tm_part(tm_wm(3), quadyxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(tm_wm(3), quadyxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! call tm_part(tmxx, dipymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx
            ! call tm_part(tmxx, dipzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx
            ! call tm_part(tmxx, quadxxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx
            ! call tm_part(tmxx, quadyymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx
            ! call tm_part(tmxx, quadzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx
            ! call tm_part(tmxx, quadzxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx
            ! call tm_part(tmxx, quadzymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            ! print*, 'tmxx', tmxx

            ! print*, 'drukuje tm z dq'
            ! do i = 1, size(tm_wm)
            !       print*, tm_wm(i)
            ! end do

            deallocate(dipxao)
            deallocate(dipyao)
            deallocate(dipzao)
            deallocate(dipxmo)
            deallocate(dipymo)
            deallocate(dipzmo)

            deallocate(quadxxao)
            deallocate(quadyyao)
            deallocate(quadzzao)
            deallocate(quadxxmo)
            deallocate(quadyymo)
            deallocate(quadzzmo)

            deallocate(quadyxao)
            deallocate(quadzxao)
            deallocate(quadzyao)
            deallocate(quadyxmo)
            deallocate(quadzxmo)
            deallocate(quadzymo)



      end subroutine task_transmom_dq_exc_exc

      subroutine task_transmom_quad_exc_exc(tm_wm, mocoeff, dm, nocc0, nocc1, nvirt0, nvirt1)

            real(F64), dimension(:), intent(out)                  :: tm_wm
            double precision, dimension(:, :), intent(in)         :: mocoeff
            double precision, dimension(:,:), intent(in)          :: dm
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1

            double precision, dimension(:, :), allocatable :: quadxxao, quadyyao, quadzzao
            double precision, dimension(:, :), allocatable :: quadxxmo, quadyymo, quadzzmo
            double precision, dimension(:, :), allocatable :: quadyxao, quadzxao, quadzyao
            double precision, dimension(:, :), allocatable :: quadyxmo, quadzxmo, quadzymo
            real(F64) :: tmxxxx, tmyyyy, tmzzzz, tmyxyx, tmzxzx, tmzyzy

            allocate(quadxxao(CC_NORB, CC_NORB))
            allocate(quadyyao(CC_NORB, CC_NORB))
            allocate(quadzzao(CC_NORB, CC_NORB))
            allocate(quadyxao(CC_NORB, CC_NORB))
            allocate(quadzxao(CC_NORB, CC_NORB))
            allocate(quadzyao(CC_NORB, CC_NORB))

            allocate(quadxxmo(CC_NORB, CC_NORB))
            allocate(quadyymo(CC_NORB, CC_NORB))
            allocate(quadzzmo(CC_NORB, CC_NORB))
            allocate(quadyxmo(CC_NORB, CC_NORB))
            allocate(quadzxmo(CC_NORB, CC_NORB))
            allocate(quadzymo(CC_NORB, CC_NORB))

            call quadrupole(quadxxao, quadyyao, quadzzao, quadyxao, quadzxao, quadzyao, CHCENTER, &
                  QUAD_TRACELESS_SHORTLEY)

            call smfill(quadxxao)
            call smfill(quadyyao)
            call smfill(quadzzao)
            call smfill(quadyxao)
            call smfill(quadzxao)
            call smfill(quadzyao)

            call atbc3(quadxxmo, mocoeff, quadxxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadyymo, mocoeff, quadyyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

            call atbc3(quadyxmo, mocoeff, quadyxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzxmo, mocoeff, quadzxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
            call atbc3(quadzymo, mocoeff, quadzyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)


            call tm_part(tmxxxx, quadxxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tmyyyy, quadyymo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tmzzzz, quadzzmo, dm, nocc0, nocc1, nvirt0, nvirt1)

            call tm_part(tmyxyx, quadyxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tmzxzx, quadzxmo, dm, nocc0, nocc1, nvirt0, nvirt1)
            call tm_part(tmzyzy, quadzymo, dm, nocc0, nocc1, nvirt0, nvirt1)

            tm_wm(1) = tmxxxx
            tm_wm(2) = tmyyyy
            tm_wm(3) = tmzzzz
            tm_wm(4) = tmyxyx
            tm_wm(5) = tmzxzx
            tm_wm(6) = tmzyzy


            ! print*,  'tmxxxx', tmxxxx
            ! print*,  'tmxxxx', tmyyyy
            ! print*,  'tmzzzz', tmzzzz
            ! print*,  'tmyxyx', tmyxyx
            ! print*,  'tmzxzx', tmzxzx
            ! print*,  'tmzyzy', tmzyzy

            deallocate(quadxxao)
            deallocate(quadyyao)
            deallocate(quadzzao)
            deallocate(quadxxmo)
            deallocate(quadyymo)
            deallocate(quadzzmo)

            deallocate(quadyxao)
            deallocate(quadzxao)
            deallocate(quadzyao)
            deallocate(quadyxmo)
            deallocate(quadzxmo)
            deallocate(quadzymo)

      end subroutine task_transmom_quad_exc_exc


      subroutine task_cc_oscstrength_quad(tmxxxx, tmyyyy, tmzzzz, tmyxyx,&
            tmzxzx, tmzyzy, eexct, w, degener, tmsum, Aik)

            real(F64), intent(out)                     :: tmxxxx
            real(F64), intent(out)                     :: tmyyyy
            real(F64), intent(out)                     :: tmzzzz
            real(F64), intent(out)                     :: tmyxyx
            real(F64), intent(out)                     :: tmzxzx
            real(F64), intent(out)                     :: tmzyzy
            real(F64), intent(in)                      :: eexct
            integer, intent(in)                        :: w
            integer, dimension(:), intent(in)          :: degener
            real(F64), intent(out) :: tmsum
            real(F64), intent(out) :: Aik

            integer :: gk

            tmsum = tmxxxx + tmyyyy + tmzzzz + &
                  two * (tmyxyx + tmzxzx + tmzyzy)


            ! print*, 'tmxxxx', tmxxxx
            ! print*, 'tmyyyy', tmyyyy
            ! print*, 'tmzzzz', tmzzzz
            ! print*, 'tmyxyx', tmyxyx
            ! print*, 'tmzxzx', tmzxzx
            ! print*, 'tmzyzy', tmzyzy
            ! print*, 'tmsum', tmsum
            !
            ! QUADRUPOLE OSCSTRENGTHS SUMMARY
            ! 
            call blankline
            call toprule_double()
            call msg('QUADRUPOLE OSCILLATOR STRENGTHS')
            call quadrupole_table_start()
            gk = 100
            Aik = totransprob_quad_shortley(tmsum, eexct, gk)
            if(abs(Aik).lt.1.d-12)then
                  Aik = zero
            end if
            if(tmsum.lt.1.d-12)then
                  tmsum= 0.d+0
            end if
            call quadrupole_table_continue(w, eexct, tmsum, Aik, gk)

            call toprule_double()
            call midrule()

      end subroutine task_cc_oscstrength_quad


      ! subroutine t_equation(eorb, nocc, nactive, t2old, t1old, t2new, t1new)
      !       double precision, dimension(:), intent(in)                           :: eorb
      !       integer, intent(in)                                                  :: nocc
      !       integer, intent(in)                                                  :: nactive
      !       double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
      !             nocc, nocc), intent(in)                          :: t2old
      !       double precision, dimension(nocc+1:nactive, nocc), intent(in)  :: t1old
      !       double precision, dimension(nocc+1:nactive, nocc+1:nactive, &
      !             nocc, nocc), intent(out)                         :: t2new
      !       double precision, dimension(nocc+1:nactive, nocc), intent(out) :: t1new

      !       integer :: a, b, i, j

      !       t1new = 0.d+0
      !       t2new = 0.d+0
      !       !$omp parallel private(j, i, b, a) &                                                                              
      !       !$omp default(shared)   

      !       !$omp do schedule(guided)  
      !       do i = 1, nocc
      !             do a = nocc+1,nactive
      !                   t1new(a, i) = t1_amplitude(eorb, a, i,t2old, t1old, nocc, nactive)!&
      !                   !                             / (eorb(i)-eorb(a))
      !             end do
      !       end do
      !       !$omp end do nowait

      !       !$omp do schedule(guided)
      !       do j = 1, nocc
      !             do i = j, nocc
      !                   do b = nocc+1, nactive
      !                         t2new(b, b, i, j) = t2_amplitude(eorb, b, b, i,&
      !                               j, t2old, nocc, nactive)!&
      !                         !                                    / (eorb(j) + eorb(i) - 2.d+0 * eorb(b))
      !                         t2new(b, b, j, i) = t2new(b, b, i, j)
      !                   end do
      !             end do
      !       end do
      !       !$omp end do nowait

      !       !$omp do schedule(guided)
      !       do j = 1, nocc
      !             do i = 1, nocc
      !                   do b = nocc+1, nactive
      !                         do a = b + 1, nactive
      !                               t2new(a, b, i, j) = &
      !                                     t2_amplitude(eorb, a, b, i, j, t2old, nocc, nactive)!&
      !                               !                                    / (eorb(j) + eorb(i) - eorb(b) - eorb(a))
      !                               t2new(b, a, j, i) = t2new(a, b, i, j)
      !                         end do
      !                   end do
      !             end do
      !       end do
      !       !$omp end do
      !       !$omp end parallel

      ! end subroutine t_equation

      subroutine tequation_driver(eorb, nocc, nvirt, t2old, t1old, &
            t2new, t1new, theory, init, nocc0, nocc1, nvirt0, nvirt1)

            double precision, dimension(:), intent(in)           :: eorb
            integer, intent(in)                                  :: nocc
            integer, intent(in)                                  :: nvirt
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old                     
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc), intent(in)                              :: t1old      
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc+1:nocc + nvirt,nocc,nocc), intent(out)    :: t2new                     
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc), intent(out)                             :: t1new 
            integer, intent(in)                                  :: theory
            integer, intent(in)                                  :: init
            integer, intent(in)                                  :: nocc0, nocc1, nvirt0, nvirt1

            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles
            integer, dimension(:), allocatable :: idoubles_comp_idx

            integer :: nactive, npair
            integer :: idims, idimd
            integer :: i
            logical :: cond1, cond2

            integer :: m0a, m1a, m0b, m1b
            integer :: m0i, m1i, m0j, m1j
            integer :: pa, pb, pi, pj
            integer :: nthreads
            integer :: max_ntasks
            integer :: ntasks
            integer :: ai
            integer :: sum
            real(F64) :: czas1, czas2
            integer :: counter
            

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads

            t1new = ZERO
            t2new = ZERO

            nactive = nocc + nvirt
            npair = nocc * nvirt

            allocate(isingles(max_ntasks, 2))
            allocate(idoubles(max_ntasks, 4))
            allocate(idoubles_comp_idx(max_ntasks))
            if (init == cc_sp_start) then
                  ntasks = 0
                  do i = 1, gidimd
                        call loop_boundaries_sp(gidoubles(1:2, i), ggirrep0, ggirrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(gidoubles(3:4, i), ggirrep0, ggirrep1, &
                              m0j, m1j, m0b, m1b)
                        ! print*, m0j, m1j
                        ! print*,  max(m0a, pb), m1a
                        ! print*, m0i, m1i
                        do pb = m0b, m1b
                              do pj = m0j, m1j
                                    do pa = max(m0a, pb), m1a
                                          do pi = m0i, m1i
                                                ntasks = ntasks + 1
!                                                print*, 'ntasks', ntasks
                                                idoubles(ntasks, 1) = pa
                                                idoubles(ntasks, 2) = pi
                                                idoubles(ntasks, 3) = pb
                                                idoubles(ntasks, 4) = pj
                                               
                                                if (ntasks == max_ntasks)  then
!                                                      print*, 'dota'
                                                      call dotasks_tequation_t2_start(eorb, nocc, nvirt, t2new, idoubles, ntasks)
                                                      ntasks = 0
                                                end if
                                          end do
                                    end do
                              end do
                        end do
                  end do
                  if(ntasks .gt.0)then
!                        print*, 'zerozero'
                        call dotasks_tequation_t2_start(eorb, nocc, nvirt, t2new, idoubles, ntasks)
                  end if

                  

            else if (init == cc_sp_iter) then
                  !$ czas1 = omp_get_wtime()
                  
                  if (theory .eq. THEORY_CC3) then
                        call fill_t3_sym(t2old, gitriples, gidimt, ggirrep0, ggirrep1, &                                                                
                              eorb, nocc, nvirt, nactive)                                                                                               
                  end if

                  ntasks = 0
                  do i = 1, gidims
                        call loop_boundaries_sp(gisingles(1:2, i), ggirrep0, ggirrep1, &
                              m0i, m1i, m0a, m1a)

                        do pa = m0a, m1a
                              do pi = m0i, m1i

                                    ntasks = ntasks + 1
                                    isingles(ntasks, 1) = pa
                                    isingles(ntasks, 2) = pi

                                    if (ntasks == max_ntasks)  then
                                          call dotasks_tequation_t1_2(eorb, nocc, nvirt, t2old, t1old, &
                                                t1new, theory, isingles, ntasks)
                                          ntasks = 0
                                    end if
                              end do
                        end do

                  end do

                  if(ntasks .gt.0)then
                        call dotasks_tequation_t1_2(eorb, nocc, nvirt, t2old, t1old, &
                              t1new, theory, isingles, ntasks)
                  end if

                  !$ czas2 = omp_get_wtime()
                  print*, 'czas na obliczenia T1', czas2 - czas1
                  call t2_intermediates_init(nocc0, nocc1, nvirt0, nvirt1)
                  call t2_intermediates(t2old, gidimd, ggirrep0, ggirrep1, gidoubles, max_ntasks, &
                        nocc0, nocc1, nvirt0, nvirt1)
                  ntasks = 0
                  counter = 0
                  do i = 1, gidimd
                        call loop_boundaries_sp(gidoubles(1:2, i), ggirrep0, ggirrep1, &
                              m0i, m1i, m0a, m1a)
                        call loop_boundaries_sp(gidoubles(3:4, i), ggirrep0, ggirrep1, &
                              m0j, m1j, m0b, m1b)

                        do pb = m0b, m1b
                              do pj = m0j, m1j
                                    do pa = maxval([m0a, pb]),  m1a
                                          do pi = m0i, m1i
                                                ntasks = ntasks + 1
!                                                print*,'ntasks', ntasks

                                                idoubles(ntasks, 1) = pa
                                                idoubles(ntasks, 2) = pi
                                                idoubles(ntasks, 3) = pb
                                                idoubles(ntasks, 4) = pj
                                                idoubles_comp_idx(ntasks) = i

                                                
                                                if (ntasks == max_ntasks)  then
                                                !      print*, 'dota3'
                                                      ! call dotasks_tequation_t2_2_test(eorb, nocc, nvirt, t2old, t1old, &
                                                      !       t2new, theory, idoubles, ntasks, counter, idoubles_comp_idx)
                                                      call dotasks_tequation_t2_2(eorb, nocc, nvirt, t2old, t1old, &
                                                            t2new, theory, idoubles, ntasks)
                                                      counter = counter + ntasks
                                                      ntasks = 0
                                                end if
                                          end do
                                    end do
                              end do
                        end do
                  end do

                  if(ntasks .gt.0)then
!                        print*, 'zero3'
                        ! call dotasks_tequation_t2_2_test(eorb, nocc, nvirt, t2old, t1old, &
                        !       t2new, theory, idoubles, ntasks, counter, idoubles_comp_idx)
                        call dotasks_tequation_t2_2(eorb, nocc, nvirt, t2old, t1old, &
                              t2new, theory, idoubles, ntasks)
                  end if

                  call t2_intermediates_free()
                  
                  !$ czas1 = omp_get_wtime()                                                                                                                     
                  print*, 'czas na obliczenia T2', czas1 - czas2
            end if


      end subroutine tequation_driver

      subroutine dotasks_tequation_t1_2(eorb, nocc, nvirt, t2old, t1old, &
            t1new, theory, isingles, ntasks)
            double precision, dimension(:), intent(in)           :: eorb
            integer, intent(in)                                  :: nocc
            integer, intent(in)                                  :: nvirt
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc), intent(in)                              :: t1old
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc), intent(out)                             :: t1new
            integer, intent(in)                                  :: theory
            integer, dimension(:,:), intent(in)                  :: isingles
            integer, intent(in)                                  :: ntasks

            integer :: nactive

            integer :: k
            integer :: a, i

            nactive = nocc + nvirt

            if (theory .eq. THEORY_CC3) then
                  !$omp parallel private(k, a, i) default(shared)                                                                                                   
                  !$omp do schedule(guided)
                  do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)
                        t1new(a, i) = t1_amplitude_cc3(eorb, a, i, t2old, t1old, nocc, nactive)
                  end do
                  !$omp end do                                                                                                                                   
                  !$omp end parallel
            end if

            if (theory .eq. THEORY_CCSD) then
                  !$omp parallel private(k, a, i) default(shared)                                                                                              
                  !$omp do schedule(guided)                                                                

                  do k = 1, ntasks
                        a = isingles(k, 1)
                        i = isingles(k, 2)
                        t1new(a, i) = t1_amplitude(eorb, a, i, t2old, t1old, nocc, nactive)
                  end do
                  !$omp end do                                                                                 
                  !$omp end parallel
            end if

      end subroutine dotasks_tequation_t1_2

      ! subroutine dotasks_tequation_t2(eorb, nocc, nvirt, t2old, t1old, &
      !       t2new, theory, &
      !       n0i, n1i, n0j, n1j, n0a, n1a, n0b, n1b, ntasks)
      !       double precision, dimension(:), intent(in)           :: eorb
      !       integer, intent(in)                                  :: nocc
      !       integer, intent(in)                                  :: nvirt
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old                     
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc), intent(in)                              :: t1old      
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(out)    :: t2new                     
      !       integer, intent(in)                                  :: theory
      !       integer, dimension(:), intent(in)                    :: n0i
      !       integer, dimension(:), intent(in)                    :: n1i
      !       integer, dimension(:), intent(in)                    :: n0j
      !       integer, dimension(:), intent(in)                    :: n1j
      !       integer, dimension(:), intent(in)                    :: n0a
      !       integer, dimension(:), intent(in)                    :: n1a
      !       integer, dimension(:), intent(in)                    :: n0b
      !       integer, dimension(:), intent(in)                    :: n1b
      !       integer, intent(in)                                  :: ntasks

      !       integer :: k

      !       !$omp parallel private(k) default(shared)
      !       !$omp do schedule(guided)  
      !       do k = 1, ntasks
      !             call tequation_t2(eorb, nocc, nvirt, t2old, t1old, &
      !                   t2new, theory, &
      !                   n0i(k), n1i(k), n0j(k), n1j(k), n0a(k), n1a(k), n0b(k), n1b(k))
      !       end do
      !       !$omp end do
      !       !$omp end parallel

      ! end subroutine dotasks_tequation_t2


      ! subroutine dotasks_tequation_t2_2_test(eorb, nocc, nvirt, t2old, t1old, &
      !       t2new, theory, idoubles, ntasks, counter, idoubles_comp_idx)
      !       double precision, dimension(:), intent(in)           :: eorb
      !       integer, intent(in)                                  :: nocc
      !       integer, intent(in)                                  :: nvirt
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc), intent(in)                              :: t1old
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(inout)    :: t2new
      !       integer, intent(in)                                  :: theory
      !       integer, dimension(:,:), intent(in)                  :: idoubles
      !       integer, intent(in)                                  :: ntasks
      !       integer, intent(in)                                  :: counter
      !       integer, dimension(:), intent(in)                    :: idoubles_comp_idx
      !       integer :: nactive

      !       integer :: k
      !       integer :: a, i, b, j


      !       nactive = nocc + nvirt


      !       if (theory .eq. THEORY_CC3) then

      !             !$omp parallel private(k, a, b, i, j) default(shared)
      !             !$omp do schedule(guided)

      !             do k = 1, ntasks
      !                   a = idoubles(k, 1)
      !                   i = idoubles(k, 2)
      !                   b = idoubles(k, 3)
      !                   j = idoubles(k, 4)

      !                   t2new(a, b, i, j) =  t2_amplitude(eorb, a, b, &
      !                         i, j, t2old, nocc, nactive)

      !                   t2new(a, b, i, j) = t2new(a, b, i, j) + &
      !                         t2_amplitude_tctd_symm(eorb, a, b, i, j, &
      !                         t2old, nocc, nactive, ggirrep0, ggirrep1, &
      !                         gitd1, gitd2, gitd3, gitd1r, gitd2r, gitd3r, &
      !                         gitd1_idx, gitd2_idx, gitd3_idx, &
      !                         gitd1r_idx, gitd2r_idx, gitd3r_idx, counter + k, idoubles_comp_idx(k))
      !                   t2new(b, a, j, i) = t2new(a, b, i, j)
      !             end do

      !             !$omp end do
      !             !$omp end parallel 

      !       end if
      ! end subroutine dotasks_tequation_t2_2_test


      subroutine dotasks_tequation_t2_2(eorb, nocc, nvirt, t2old, t1old, &
            t2new, theory, idoubles, ntasks)
            double precision, dimension(:), intent(in)           :: eorb
            integer, intent(in)                                  :: nocc
            integer, intent(in)                                  :: nvirt
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc), intent(in)                              :: t1old
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc+1:nocc + nvirt,nocc,nocc), intent(inout)    :: t2new

            integer, intent(in)                                  :: theory
            integer, dimension(:,:), intent(in)                  :: idoubles
            integer, intent(in)                                  :: ntasks
            integer :: nactive

            integer :: k
            integer :: a, i, b, j


            nactive = nocc + nvirt

            if (theory .eq. THEORY_CC3) then

                  !$omp parallel private(k, a, b, i, j) default(shared)
                  !$omp do schedule(guided)
                  do k = 1, ntasks
                        a = idoubles(k, 1)
                        i = idoubles(k, 2)
                        b = idoubles(k, 3)
                        j = idoubles(k, 4)

                        t2new(a, b, i, j) = t2_amplitude(eorb, a, b, &
                              i, j, t2old, nocc, nactive)&
                        + t2_amplitude_tctd (eorb, a, b, &
                              i, j, t2old, nocc, nactive)
                        t2new(b, a, j, i) = t2new(a, b, i, j)
                  end do 
                  !$omp end do
                  !$omp end parallel 


            else if (theory .eq. THEORY_CCSD) then

                  !$omp parallel private(k, a, b, i, j) default(shared)
                  !$omp do schedule(guided)   
                  do k = 1, ntasks
                        a = idoubles(k, 1)
                        i = idoubles(k, 2)
                        b = idoubles(k, 3)
                        j = idoubles(k, 4)

                        t2new(a, b, i, j) = t2_amplitude(eorb, a, b, &
                              i, j, t2old, nocc, nactive)
                        t2new(b, a, j, i) = t2new(a, b, i, j)
                  end do
                  !$omp end do
                  !$omp end parallel
            end if

      end subroutine dotasks_tequation_t2_2

      subroutine dotasks_tequation_t2_start(eorb, nocc, nvirt, t2new, idoubles, ntasks)
            double precision, dimension(:), intent(in)           :: eorb
            integer, intent(in)                                  :: nocc
            integer, intent(in)                                  :: nvirt
            double precision, dimension(nocc+1:nocc + nvirt,&
                  nocc+1:nocc + nvirt,nocc,nocc), intent(inout)  :: t2new
            integer, dimension(:,:), intent(in)                  :: idoubles
            integer, intent(in)                                  :: ntasks

            integer :: k
            integer :: a, i, b, j


            !$omp parallel private(k, a, b, i, j) default(shared)                            
            !$omp do schedule(guided)                                                                                                                                                    
            do k = 1, ntasks
                  a = idoubles(k, 1)
                  i = idoubles(k, 2)
                  b = idoubles(k, 3)
                  j = idoubles(k, 4)

                  t2new(a, b, i, j) = vovo(a, i, b, j)&
                        / (eorb(i) + eorb(j) - eorb(a) - eorb(b))
                  t2new(b, a, j, i) = t2new(a, b, i, j)

            end do
            !$omp end do                                                                                                                                                                
            !$omp end parallel                                                                                                                                   

      end subroutine dotasks_tequation_t2_start



      subroutine thrd(a, b, c, d, k)
            integer, intent(in) :: a, b
            integer, intent(in) :: c, d
            integer, intent(in) :: k

            integer :: thr
            thr = omp_get_thread_num()
            print*, thr, ', ', k, ',', a*b*c*d

      end subroutine thrd

      ! subroutine tequation_start(eorb, nocc, nvirt, t2, theory, &
      !       n0i, n1i, n0j, n1j, n0a, n1a, n0b, n1b)

      !       double precision, dimension(:), intent(in)           :: eorb
      !       integer, intent(in)                                  :: nocc
      !       integer, intent(in)                                  :: nvirt
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(inout)  :: t2                    
      !       integer, intent(in)                                  :: theory
      !       integer, intent(in)                                  :: n0i
      !       integer, intent(in)                                  :: n1i
      !       integer, intent(in)                                  :: n0j
      !       integer, intent(in)                                  :: n1j
      !       integer, intent(in)                                  :: n0a
      !       integer, intent(in)                                  :: n1a
      !       integer, intent(in)                                  :: n0b
      !       integer, intent(in)                                  :: n1b

      !       integer :: nactive, npair
      !       integer :: i, j, b, a

      !       nactive = nocc + nvirt
      !       npair = nocc * nvirt

      !       do j = n0j, n1j
      !             do i = j, n1i
      !                   do b = n0b, n1b
      !                         t2(b, b, i, j) = vovo(b, i, b, j)&
      !                               / (eorb(i) + eorb(j)  -2.d+0 *  eorb(b))
      !                         t2(b, b, j, i) = t2(b, b, i, j)
      !                   end do
      !             end do
      !       end do

      !       do j = n0j, n1j
      !             do i = n0i, n1i
      !                   do b = n0b, n1b
      !                         do a = b + 1, n1a
      !                               t2(a, b, i, j) = vovo(a, i, b, j)&
      !                                     / (eorb(i) + eorb(j) - eorb(a) - eorb(b))
      !                               t2(b, a, j, i) = t2(a, b, i, j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      ! end subroutine tequation_start

      ! subroutine tequation_t1(eorb, nocc, nvirt, t2old, t1old, &
      !       t1new, theory, &
      !       n0i, n1i, n0a, n1a)
      !       !
      !       ! Do a single iteration of CCSD/CC3 single and double amplitudes.
      !       !            
      !       double precision, dimension(:), intent(in)           :: eorb
      !       integer, intent(in)                                  :: nocc
      !       integer, intent(in)                                  :: nvirt
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old                     
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc), intent(in)                              :: t1old      
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc), intent(out)                             :: t1new 
      !       integer, intent(in)                                  :: theory
      !       integer, intent(in)                                  :: n0i
      !       integer, intent(in)                                  :: n1i
      !       integer, intent(in)                                  :: n0a
      !       integer, intent(in)                                  :: n1a

      !       integer :: nactive, npair
      !       integer :: a, i

      !       nactive = nocc + nvirt
      !       npair = nocc * nvirt

      !       if (theory .eq. THEORY_CC3) then
      !             do i = n0i, n1i
      !                   do a = n0a, n1a
      !                         t1new(a, i) = t1_amplitude_cc3(eorb, a, i, t2old, t1old, nocc, nactive)
      !                   end do
      !             end do
      !       end if

      !       if (theory .eq. THEORY_CCSD) then
      !             do i = n0i, n1i
      !                   do a = n0a, n1a
      !                         t1new(a, i) = t1_amplitude(eorb, a, i, t2old, t1old, nocc, nactive)
      !                   end do
      !             end do
      !       end if
      ! end subroutine tequation_t1


      ! subroutine tequation_t2(eorb, nocc, nvirt, t2old, t1old, &
      !       t2new, theory, &
      !       n0i, n1i, n0j, n1j, n0a, n1a, n0b, n1b)
      !       !
      !       ! Do a single iteration of CCSD/CC3 single and double amplitudes.
      !       !            
      !       double precision, dimension(:), intent(in)           :: eorb
      !       integer, intent(in)                                  :: nocc
      !       integer, intent(in)                                  :: nvirt
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(in)     :: t2old                     
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc), intent(in)                              :: t1old      
      !       double precision, dimension(nocc+1:nocc + nvirt,&
      !             nocc+1:nocc + nvirt,nocc,nocc), intent(out)    :: t2new                     
      !       integer, intent(in)                                  :: theory
      !       integer, intent(in)                                  :: n0i
      !       integer, intent(in)                                  :: n1i
      !       integer, intent(in)                                  :: n0j
      !       integer, intent(in)                                  :: n1j
      !       integer, intent(in)                                  :: n0a
      !       integer, intent(in)                                  :: n1a
      !       integer, intent(in)                                  :: n0b
      !       integer, intent(in)                                  :: n1b

      !       integer :: nactive, npair
      !       integer :: a, i, b, j

      !       nactive = nocc + nvirt
      !       npair = nocc * nvirt

      !       if (theory .eq. THEORY_CC3) then
      !             do j = n0j, n1j
      !                   do i = n0i, n1i
      !                         do b = n0b, n1b
      !                               do a = b, n1a
      !                                     t2new(a, b, i, j) = t2_amplitude_cc3(eorb, a, b, &
      !                                           i, j, t2old, nocc, nactive)  
      !                                     t2new(b, a, j, i) = t2new(a, b, i, j)
      !                               end do
      !                         end do
      !                   end do
      !             end do
      !       end if

      !       if (theory .eq. THEORY_CCSD) then
      !             do j = n0j, n1j
      !                   do b = n0b, n1b
      !                         do i = n0i, n1i
      !                               do a = b, n1a
      !                                     t2new(a, b, i, j) = t2_amplitude(eorb, a, b, &
      !                                           i, j, t2old, nocc, nactive)  
      !                                     t2new(b, a, j, i) = t2new(a, b, i, j)
      !                                     !                                          t2new(n0a, n0b, n0i, n0j) = t2_amplitude(eorb, n0a, n0b, &
      !                                     !                                                n0i, n0j, t2old, nocc, nactive)
      !                                     !                                          t2new(n0b, n0a, n0j, n0i) = t2new(n0a, n0b, n0i, n0j)
      !                               end do
      !                         end do
      !                   end do
      !             end do
      !       end if
      ! end subroutine tequation_t2

      ! subroutine tequation(eorb, nocc, nvirt, t2old, t1old, &
      !       t2new, t1new, theory)
      !       !                                                                                                              
      !       ! Do a single iteration of CCSD/CC3 single and double amplitudes.                                                                            
      !       !                                                                                                                                       
      !       double precision, dimension(:), intent(in)           :: eorb
      !       integer, intent(in)                                  :: nocc
      !       integer, intent(in)                                  :: nvirt
      !       double precision, dimension(nocc+1:nocc + nvirt,nocc+1:nocc + nvirt,nocc,nocc), intent(in)  :: t2old                     
      !       double precision, dimension(nocc+1:nocc + nvirt,nocc), intent(in)                           :: t1old      
      !       double precision, dimension(nocc+1:nocc + nvirt,nocc+1:nocc + nvirt,nocc,nocc), intent(out) :: t2new                     
      !       double precision, dimension(nocc+1:nocc + nvirt,nocc), intent(out)                          :: t1new 
      !       integer, intent(in)                                    :: theory

      !       integer :: nactive, npair
      !       integer :: ai, k, a, i, b, j
      !       integer :: a0, b0
      !       integer :: output_irrep

      !       t1new = ZERO
      !       t2new = ZERO

      !       nactive = nocc + nvirt
      !       npair = nocc * nvirt

      !       if (theory .eq. THEORY_CC3) then
      !             !                  call fill_t3(t2old, eorb, nocc, nactive)
      !             call fill_t3_sym(t2new, gitriples, gidimt, girrep0, girrep1, &
      !                   eorb, nocc, nvirt, nactive)
      !       end if

      !       if (theory .eq. THEORY_CC3) then
      !             !$omp parallel private(k, j, i, b, a, ai, bj, a0, b0) &                                                                         
      !             !$omp default(shared)                                                                                                       
      !             !$omp do schedule(guided)                                                                                            
      !             do ai = 1, npair
      !                   call ai2a_i(ai, nocc, nvirt, a, i)
      !                   a0 = a - nocc
      !                   t1new(a, i) = t1_amplitude_cc3(eorb, a, i, t2old, t1old, nocc, nactive)
      !                   !                        call dmsg("EOM-CC TOTAL TIME [SECONDS] ", clock_readwall(t_total), fmt="ES10.1")
      !             end do
      !             !$omp end do nowait         
      !             !$omp do schedule(guided)                                                                                            
      !             do k = 1, (npair + 1) * npair / 2
      !                   call ai_ge_bj(k, npair, nocc, nvirt, a, b, i, j)
      !                   a0 = a - nocc
      !                   b0 = b - nocc
      !                   t2new(a, b, i, j) = t2_amplitude_cc3(eorb, a, b, &
      !                         i, j, t2old, nocc, nactive)
      !                   t2new(b, a, j, i) = t2new(a, b, i, j)
      !             end do
      !             !$ome end do nowait                                                                                                
      !             !$omp end parallel                                                                                                       
      !       end if

      !       if (theory .eq. THEORY_CCSD) then

      !             !       do i = 1, nocc
      !             !             do a = nocc+1, nactive
      !             !                   output_irrep = morep_direct_product([a, i])
      !             !                   if(output_irrep.ne.1)then
      !             !                         t1new(a, i) = zero
      !             !                   else
      !             !                         t1new(a, i) = t1_amplitude(eorb, a, i, t2old, t1old, nocc, nactive)
      !             !                   end if
      !             !             end do                       
      !             !       end do

      !             !       do i = 1, nocc
      !             !             do j = 1, nocc
      !             !                   do b = nocc + 1, nactive
      !             !                         do a = b, nactive
      !             !                               output_irrep = morep_direct_product([a, b, i, j])
      !             !                               if(output_irrep.ne.1)then
      !             !                                     t2new(a, b, i, j) = zero
      !             !                                     t2new(b, a, j, i) = zero
      !             !                               else
      !             !                                     t2new(a, b, i, j) = t2_amplitude(eorb, a, b, &
      !             !                                           i, j, t2old, nocc, nactive)
      !             !                                     t2new(b, a, j, i) = t2new(a, b, i, j)
      !             !                               end if
      !             !                         end do
      !             !                   end do
      !             !             end do
      !             !       end do
      !             ! end if



      !             !$omp parallel private(k, j, i, b, a, ai, bj, a0, b0) &                                                                                                              
      !             !$omp default(shared)                                                                                                                                                
      !             !$omp do schedule(guided)                                                                                                                                           
      !             do ai = 1, npair
      !                   call ai2a_i(ai, nocc, nvirt, a, i)
      !                   output_irrep = morep_direct_product([a, i])
      !                   if(output_irrep.ne.1)then
      !                         t1new(a, i) = zero
      !                   else
      !                         t1new(a, i) = t1_amplitude(eorb, a, i, t2old, t1old, nocc, nactive)
      !                   end if
      !             end do
      !             !$omp end do nowait                                                                                                                           
      !             !$omp do schedule(guided)                                                                                                                                            
      !             do k = 1, (npair + 1) * npair / 2
      !                   call ai_ge_bj(k, npair, nocc, nvirt, a, b, i, j)
      !                   output_irrep = morep_direct_product([a, b, i, j]) 
      !                   if(output_irrep.ne.1)then                                                                                                                                     
      !                         t2new(a, b, i, j) = zero                                                                                                   
      !                   else
      !                         t2new(a, b, i, j) = t2_amplitude(eorb, a, b, &
      !                               i, j, t2old, nocc, nactive)
      !                         t2new(b, a, j, i) = t2new(a, b, i, j)
      !                   end if
      !             end do
      !             !$ome end do nowait                                                                                                                                                  
      !             !$omp end parallel                                                                                                                                
      !       end if

      !       !             ! do k = 1, (npair + 1) * npair / 2
      !       !       call ai_ge_bj(k, npair, nocc, nvirt, a, b, i, j)
      !       !       output_irrep = morep_direct_product([a, b, i, j])
      !       !       if(output_irrep.ne.1)then
      !       !             t2new(a, b, i, j) = zero
      !       !       end if
      !       !       ! if (abs(t2new(a,b, i, j)).gt.1.d-8)then
      !       !       !      print*, a, ',',b,',', i,',', j, ',', print_rep(output_irrep), t2new(a, b, i, j)
      !       !       ! end if
      !       ! end do

      !       ! do ai = 1, npair
      !       !       call ai2a_i(ai, nocc, nvirt, a, i)
      !       !       output_irrep = morep_direct_product([a, i])
      !       !       if(output_irrep.ne.1)then
      !       !             t1new(a, i) = zero
      !       !       end if
      !       !       ! if (abs(t1new(a,i)).gt.1.d-8)then
      !       !       !       print*, a, ',', i,',', print_rep(output_irrep), t1new(a, i)
      !       !       ! end if
      !       ! end do
      !       !            stop
      ! end subroutine tequation

      subroutine cctable_start()
            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME
            !
            write(line, "(A3,1X,A20,2X,A7,2X,A10,2X,A10)") &
                  "#", "ENERGY", "TDIFF", "EDIFF", "TIME"
            call midrule(width=76)
            call msg(line)
            call midrule(width=76)
      end subroutine cctable_start


      subroutine cctable_continue(iter, energy, ampltdiff, ediff, time)
            integer, intent(in)          :: iter
            double precision, intent(in) :: energy
            double precision, intent(in) :: ampltdiff
            double precision, intent(in) :: ediff
            double precision, intent(in) :: time

            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME
            !
            write(line, "(I3,1X,F20.10,2X,ES7.1,2X,ES10.1,2X,ES10.1)") &
                  iter, energy, ampltdiff, ediff, time
            call msg(line)
      end subroutine cctable_continue


      subroutine dipole_table_start()
            character(len=100) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME
            !
            write(line, "(A1,A3,1X,A1, A15,2X,A1, A20, A1, A15, 1X, A1, A4, A1)") &
                  '|',"#", '|',"EXC",'|', "f OSC STRENGTH [A.U.]",'|', 'Aik [10^8s-1]', '|', 'gk', '|'
            call midrule(width=100)
            call msg(line)
            call midrule(width=100)
      end subroutine dipole_table_start

      subroutine nonad_table_start()
            character(len=100) :: line
            !                                                                                                                    
            ! ITER ENERGY TDIFF EDIFF TIME                                                          
            !                                                                                           
            write(line, "(A1,A3,1X,A1, A15,2X,A1, A20, 1X, A1, A4, A1)") &
                  '|',"#", '|',"EXC",'|', "nonad coupling",'|', 'gk', '|'
            call midrule(width=100)
            call msg(line)
            call midrule(width=100)
      end subroutine nonad_table_start

      subroutine quadrupole_table_start()
            character(len=100) :: line
            !                                                
            ! ITER ENERGY TDIFF EDIFF TIME                   
            !                                                                                                                        
            write(line, "(A1,A3,1X,A1, A12,2X,A1, A20, A1, A15, 1X, A1, A4, A1 )") &
                  '|',"#", '|',"EXC",'|', " LINE STRENGTH [A.U.]",'|', 'Aik [s-1]', '|', 'gk', '|'
            call midrule(width=100)
            call msg(line)
            call midrule(width=100)
      end subroutine quadrupole_table_start


      subroutine dipole_table_continue(iter, energy, oscstr, aik, gk)
            integer, intent(in)          :: iter
            double precision, intent(in) :: energy
            double precision, intent(in) :: oscstr
            double precision, intent(in) :: aik
            integer, intent(in) :: gk
            character(len=100) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME
            !
            write(line, "(A1,I3,1X,A1, F15.6,2X,A1, ES20.5, A1, ES15.5, 1X, A1, I4, A1)") &
                  '|', iter,'|', energy,'|', oscstr, '|', aik, '|', gk, '|'
            call msg(line)
      end subroutine dipole_table_continue

      subroutine nonad_table_continue(iter, energy, oscstr, gk)
            integer, intent(in)          :: iter
            double precision, intent(in) :: energy
            double precision, intent(in) :: oscstr
            integer, intent(in) :: gk
            character(len=100) :: line
            !                                                                                                
            ! ITER ENERGY TDIFF EDIFF TIME                                                             
            !                                                                                          
            write(line, "(A1,I3,1X,A1, F15.6,2X,A1, ES20.5,1X, A1, I4, A1)") &
                  '|', iter,'|', energy,'|', oscstr, '|', gk, '|'
            call msg(line)
      end subroutine nonad_table_continue

      subroutine quadrupole_table_continue(iter, energy, oscstr, aik, gk)
            integer, intent(in)          :: iter
            double precision, intent(in) :: energy
            double precision, intent(in) :: oscstr
            double precision, intent(in) :: aik
            integer, intent(in) :: gk

            character(len=100) :: line
            !                                                                                                                       
            ! ITER ENERGY TDIFF EDIFF TIME                                                                                
            !                                                                                                                        
            write(line, "(A1,I3,1X,A1, F12.6,2X,A1, ES20.5, A1, ES15.5, 1X, A1, I4, A1)") &
                  '|', iter,'|', energy,'|', oscstr, '|', aik, '|', gk, '|'
            call msg(line)
      end subroutine quadrupole_table_continue


      subroutine eom_cctable_start()
            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME
            !
            write(line, "(A3,1X,A10,1X,A12,1X,A10,1X,A10)") &
                  "#", "NVECTORS",  "MAX.RESIDUAL", "MAX.EDIFF", "TIME [s]"
            call midrule(width=76)
            call msg(line)
            call midrule(width=76)
      end subroutine eom_cctable_start


      subroutine eom_cctable_continue(iter, nvec, time, residual, ediff, nsought)
            integer, intent(in)                        :: iter
            integer, intent(in)                        :: nvec
            double precision, intent(in)               :: time
            double precision, dimension(:), intent(in) :: residual
            double precision, dimension(:), intent(in) :: ediff
            integer, intent(in)                        :: nsought

            double precision :: max_residual, max_ediff
            character(len=DEFLEN) :: line

            max_residual = maxval(residual(1:nsought))
            max_ediff = maxval(ediff(1:nsought))
            !
            ! ITER NVECTORS MAX.RESIDUAL MAX.EDIFF TIME
            !
            write(line, "(I3,1X,I10,1X,ES12.1,1X,ES10.1,1X,ES10.1)") &
                  iter, nvec, max_residual, max_ediff, time
            call msg(line)
      end subroutine eom_cctable_continue


      subroutine eom_cctable_summary_start()
            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME
            !
            write(line, "(A1, A3, 1X, A1, A15, 2X, A1, A15, 2X, A1, A20, A1, A3, A1 )") &
                  '|', "#", '|', "HARTREE",'|', "eV.",'|', "cm-1",'|', 'gk', '|'
            call midrule(width=76)
            call msg(line)
            call toprule_double(width=76)
      end subroutine eom_cctable_summary_start


      subroutine eom_cctable_summary_continue(iter, e_h, eeV, ecm, irrep_idx, multip)
            integer, intent(in)          :: iter
            double precision, intent(in) :: e_h
            double precision, intent(in) :: eeV
            double precision, intent(in) :: ecm
            integer, intent(in)          :: irrep_idx
            integer, intent(in)          :: multip

            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY TDIFF EDIFF TIME 
            !
            print*, 'zozooz'
            write(line, "(A1, I3,1X, A1, F15.6,2X, A1, F15.6,2X,A1, F20.6, A1, A3, A1)") &
                  '|',iter,'|', e_h, '|', eeV,'|', ecm,'|', print_rep_mult(irrep_idx, multip), '|'
            print*, 'zzzz'
            call msg(line)
            print*, 'osos'
      end subroutine eom_cctable_summary_continue


      ! subroutine find_degeneracy_sym(stateidx, degener, nsym, levdeg, numlev, numlev_spec, numlev_aux, eexcit, n)
      !       ! --------------------------------------------------------                   
      !       ! Find indices of consecutive guess vectors belonging
      !       ! to given multiplets.
      !       ! --------------------------------------------------------                                              
      !       ! STATEIDX - On exit, STATEIDX(I) contains the index
      !       !            of the I-th vectors in the guess matrix.
      !       !            I = 1, SUM(NLEVELS)
      !       ! NSYM     - Number of symmetries (different multplicities)
      !       ! LEVDEG   - LEVDEG(I) contains the                                                                  
      !       !            degeneracy of I-th energy level, where                                                              
      !       !            I = 1, ..., NLEVELS                                                                                 
      !       ! NUMLEV   - NUMLEV(I) contains the requested number of
      !       !            levels in each symmetry (target vectors)
      !       ! NUMLEV_AUX - Requested number of levels in each symmetry
      !       !            (auxiliary, non-target vectors present in 
      !       !            the set of guess vectors)
      !       !
      !       ! EEXCIT   - Electronic energies sorted in ascending order                                             
      !       !
      !       ! N        - Total number of states obtained from CIS 
      !       !            from which the guess vectors are selected
      !       !            (also dimension of the EEXCIT array)
      !       !                                   
      !       integer, dimension(:), intent(out)         :: stateidx
      !       integer, dimension(:), intent(out)         :: degener
      !       integer, intent(in)                        :: nsym
      !       integer, dimension(:), intent(in)          :: levdeg
      !       integer, dimension(:), intent(in)          :: numlev
      !       integer, dimension(:,:), intent(in)        :: numlev_spec
      !       integer, dimension(:), intent(in)          :: numlev_aux
      !       double precision, dimension(:), intent(in) :: eexcit
      !       integer, intent(in)                        :: n
      !       integer, dimension(n)                      :: guess_deg
      !       integer, dimension(n)                      :: guess_deg_idx
      !       integer                                    :: guess_noflev

      !       integer :: nlevels_target, nlevels_aux, nlevels_total
      !       integer :: l, k, k_target, j, i, p
      !       integer :: mx

      !       do j = 1, CC_NSYM
      !             if (numlev(j) < 0 .or. numlev_aux(j) < 0) then
      !                   call msg("ERROR: NEGATIVE NUMBER OF STATES REQUESTED", &
      !                         priority=MSG_ERROR)
      !                   stop
      !             end if
      !       end do

      !       nlevels_target = sum(numlev(1:nsym))
      !       nlevels_aux = sum(numlev_aux(1:nsym))
      !       nlevels_total = nlevels_target + nlevels_aux

      !       mx = maxval(numlev(1:nsym))
      !       do j = 1, mx
      !             do i = 1, CC_NSYM
      !                   if(numlev_spec(i, j) < 0)then
      !                         call msg("ERROR: NEGATIVE NUMBER OF STATES REQUESTED", &
      !                               priority=MSG_ERROR)
      !                         stop
      !                   end if
      !             end do
      !       end do

      !       if (nlevels_total > n) then
      !             call msg("ERROR: NOT ENOUGH VECTORS COMPUTED FOR THE EOM-CC GUESS", &
      !                   priority=MSG_ERROR)
      !             stop
      !       end if

      !       print*, 'guess_dim', n
      !       print*, 'nlevels_total', nlevels_total

      !       if (n == 1) then
      !             if (nlevels_total == 1) then
      !                   degener(1) = 1
      !                   stateidx(1) = 1
      !                   return
      !             else
      !                   call msg("ERROR: COULD NOT COMPLETE SET OF GUESS VECTORS", &
      !                         priority=MSG_ERROR)
      !                   stop
      !             end if
      !       end if

      !       call degeneracy_scan(eexcit, n, guess_deg, guess_deg_idx, guess_noflev, CC_DEGEPS)

      !       do l = 1, CC_NSYM
      !             print*, numlev_spec(l, :)
      !       end do

      !       k_target = 1
      !       degener = 0
      !       k = 1
      !       do l = 1, CC_NSYM
      !             do j = 1, mx
      !                   do p = 1, guess_noflev
      !                         if (guess_deg(p) == levdeg(l))then
      !                               if (guess_deg_idx(p) == numlev_spec(l, j))then
      !                                     print*, guess_deg(p), guess_deg_idx(p)
      !                                     stateidx(k_target) = find_idx(p, guess_deg)
      !                                     degener(k_target) = guess_deg(p)
      !                                     k_target = k_target + 1
      !                                     exit
      !                               end if
      !                         end if
      !                   end do
      !             end do
      !       end do


      !       ! k_target = 1
      !       ! k_aux = nlevels_target + 1
      !       ! k = 1
      !       ! degeneracy = 0
      !       ! numlev_left = numlev + numlev_aux
      !       ! lloop: do l = 1, n
      !       !       degeneracy = degeneracy + 1
      !       !       if (abs(eexcit(l)-eexcit(l+1)) > abs(eexcit(l)*CC_DEGEPS)) then ! the nex state has differend degeneration
      !       !             do j = 1, nsym
      !       !                   if (levdeg(j) == degeneracy) then
      !       !                         if (numlev_left(j) > 0) then
      !       !                               if (numlev_left(j) > numlev_aux(j)) then
      !       !                                     numlev_left(j) = numlev_left(j) - 1
      !       !                                     stateidx(k_target) = l - degeneracy + 1
      !       !                                     degener(k_target) = degeneracy
      !       !                                     k_target = k_target + 1
      !       !                               else
      !       !                                     numlev_left(j) = numlev_left(j) - 1
      !       !                                     stateidx(k_aux) = l - degeneracy + 1
      !       !                                     degener(k_aux) = degeneracy
      !       !                                     k_aux = k_aux + 1
      !       !                               end if
      !       !                               k = k + 1
      !       !                               if (k > nlevels_total) then
      !       !                                     exit lloop
      !       !                               end if
      !       !                         end if
      !       !                   end if
      !       !             end do
      !       !             degeneracy = 0
      !       !       end if
      !       ! end do lloop
      !       !
      !       ! Test if all requested vectors have been found
      !       !
      !       do j = 1, nlevels_total
      !             if (degener(j) == 0)then
      !                   call msg("ERROR: COULD NOT COMPLETE SET OF GUESS VECTORS", &
      !                         priority=MSG_ERROR)
      !                   stop
      !             end if
      !       end do

      !       ! do j = 1, CC_NSYM
      !       !       if (numlev_left(j) > 0) then
      !       !             call msg("ERROR: COULD NOT COMPLETE SET OF GUESS VECTORS", &
      !       !                   priority=MSG_ERROR)
      !       !             stop
      !       !       end if
      !       ! end do
      ! end subroutine find_degeneracy_sym

      function find_idx(m, guess_deg)
            integer :: find_idx
            integer, intent(in) :: m
            integer, dimension(:), intent(in) :: guess_deg
            integer :: i

            find_idx = 1
            do i = 1, m-1
                  find_idx = find_idx + guess_deg(i)
            end do

      end function find_idx


      ! subroutine find_degeneracy(degeneracy, eexcit, nlevels, n)
      !       ! --------------------------------------------------------
      !       ! Find degeneracy of each energy level.
      !       ! --------------------------------------------------------
      !       ! DEGENERACY - On exit, DEGENERACY(I) contains the
      !       !              degeneracy of I-th energy level, where
      !       !              I = 1, ..., NLEVELS
      !       ! EEXCIT     - Electronic energies sorted in ascending
      !       ! NLEVELS    - Number of lowest energy levels for which
      !       !              degeneracies are computed
      !       ! N          - Number of states provided by the CIS guess
      !       !              (also dimension of the EEXCIT array)
      !       !
      !       integer, dimension(:), intent(out)         :: degeneracy
      !       double precision, dimension(:), intent(in) :: eexcit
      !       integer, intent(in)                        :: nlevels
      !       integer, intent(in)                        :: n

      !       integer :: i, l

      !       if (nlevels > n) then
      !             call msg("ERROR: NOT ENOUGH VECTORS COMPUTED FOR THE EOM-CC GUESS", &
      !                   priority=MSG_ERROR)
      !             stop
      !       end if

      !       if (n == 1) then
      !             degeneracy(1) = 1
      !             return
      !       end if

      !       degeneracy(1:nlevels) = 0
      !       i = 1
      !       l = 1
      !       do while ((l .lt. n) .and. (i .le. nlevels))
      !             degeneracy(i) = degeneracy(i) + 1
      !             if (abs(eexcit(l)-eexcit(l+1)) .gt. abs(eexcit(l)*CC_DEGEPS)) then
      !                   i = i + 1
      !             end if
      !             l = l + 1
      !       end do

      !       if (i <= nlevels) then
      !             call msg("ERROR: NOT ENOUGH VECTORS COMPUTED FOR THE EOM-CC GUESS", &
      !                   priority=MSG_ERROR)
      !             stop
      !       end if
      ! end subroutine find_degeneracy


      function compgk(k, degener, degenerate_present, nlevels)
            integer                           :: compgk
            integer, intent(in)               :: k
            integer, dimension(:), intent(in) :: degener
            logical, intent(in)               :: degenerate_present
            integer, intent(in)               :: nlevels

            integer :: sum, i

            if (.not. degenerate_present) then
                  compgk = degener(k)
            else
                  sum = 0
                  compgk = 0
                  degloop: do i = 1, nlevels
                        sum = sum + degener(i)
                        if(k.le.sum) then
                              compgk = degener(i)
                              exit degloop
                        end if
                  end do degloop
            end if
      end function compgk
end module ccsd
