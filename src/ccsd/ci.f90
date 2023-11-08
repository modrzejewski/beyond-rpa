module ci

      use ccsd_transformed_integrals
      use basis
      use cisd_block_02_mod_dav
      use cisd_block_20_mod_dav
      use cisd_block_11_mod_dav
      use cisd_block_12_mod_dav
      use cisd_block_21_mod_dav
      use cisd_block_22_mod_dav

      use cisd_block_02_mod
      use cisd_block_20_mod
      use cisd_block_11_mod
      use cisd_block_12_mod
      use cisd_block_21_mod
      use cisd_block_22_mod
      use guess_davidson

      use davidson_main
      use gparam


      implicit none
      
      integer, parameter :: CIS_METHOD = 1
      integer, parameter :: CID_METHOD = 2
      

contains

      subroutine task_cisd_dav2(t2, iexci, wrci, rtstart, eorb, nocc0, nocc1, nvirt0, nvirt1, nactive, &
            e_total, e_electron, order, irrep0, irrep1, sd_dom, multip)

            real(F64), dimension(:,:,:,:), intent(in)                                   :: t2 
            integer, dimension(:), intent(in)       :: iexci
            real(F64), dimension(:, :), intent(out) :: wrci
            real(F64), dimension(:,:), intent(out)  :: rtstart
            real(F64), dimension(:), intent(in)     :: eorb
            integer, intent(in)                     :: nocc0, nocc1
            integer, intent(in)                     :: nvirt0, nvirt1, nactive

            real(F64), intent(in)                   :: e_total, e_electron
            integer, intent(in)                     :: order
            integer, dimension(:,:), intent(in)     :: irrep0
            integer, dimension(:,:), intent(in)     :: irrep1
            integer, intent(in)                     :: sd_dom
            integer, intent(in)                     :: multip

            integer :: nocc, nvirt, nidx 
            integer :: sdim, ddim

            integer, dimension(:), allocatable :: dy

            real(F64), dimension(:,:), allocatable  :: hci
            real(F64) :: e_nuclear
            integer :: i, j
            integer, parameter :: OC = 1
            integer, parameter :: VT = 2

            real(F64), dimension(:,:), allocatable :: guess_vec_r, guess_vec_l
            integer :: nroots
            double precision, dimension(:), allocatable :: e_diff
            double precision, dimension(:), allocatable :: e_new
            real(F64), dimension(:), allocatable :: e_new_merged
            type(tclock) :: t_total
            integer, dimension(2) :: nidx_table

            integer :: nexc_single, nexc_double, nexc
            integer :: rtoffset

             integer :: lwork, liwork
             real(F64), dimension(:), allocatable :: work_sniez, eig, eig_wi
             integer, dimension(:), allocatable :: iwork_sniez
             integer :: npair
             real(F64), dimension(1, 1) :: foo
             real(F64), dimension(:,:), allocatable :: geev_sniez
             integer :: kj
             real(F64), dimension(:,:), allocatable :: hci2
             integer :: n_temp
             real(F64) :: time1, time2

            call clock_start(t_total)

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nexc_single = nocc * nvirt
            nidx = nexc_single + (nexc_single * (nexc_single + 1)) / 2
            nexc_double = nidx - nexc_single

            allocate(e_diff(nidx))

            ! allocate(hci(nidx+1, nidx+1))

            ! allocate(hci(nexc_single, nexc_single))
            ! allocate(eig(nexc_single))
            ! print*, 'nexc_single', nexc_single
            ! hci = zero

            
            ! allocate(hci(nexc_double, nexc_double))
            ! allocate(eig(nexc_double))
            ! hci = zero

            ! hci(1, 1) = e_total
            ! call cisd_block_12(hci, nocc0, nocc1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !       nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, &
            !       1, 2)

            ! call cisd_block_21(hci, nocc0, nocc1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !       nocc0, nocc1, nocc0, nocc1,nocc0, nocc1, 2, 1)

            ! call cisd_block_22(hci, nocc0, nocc1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
            !       nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, nocc0, nocc1, 2, 2, eorb)




           ! call cisd_block_11(hci, nocc0, nocc1, nvirt0, nvirt1, nvirt0, nvirt1, nvirt0, nvirt1, &
           !       nocc0, nocc1, nocc0, nocc1, 1, 1, eorb)

!            n_temp = nexc_double + 1

           ! !$ time1 = omp_get_wtime()
           ! allocate(hci2(n_temp, n_temp))
           ! hci2 = hci(1:n_temp, 1:n_temp)
   !         call dgeevquery(n_temp, lwork, 'N', 'N')
   !         allocate(work_sniez(lwork))
   !         allocate(eig_wi(n_temp))
   !         allocate(geev_sniez(n_temp, n_temp))
   !         call geevwrap(hci, eig, eig_wi, geev_sniez, geev_sniez, &
!                  n_temp, "N", "N", work_sniez)
 !!          !  !$ time2 = omp_get_wtime()
           !  print*, 'time1 - tim2', time2 - time1
   !         allocate(dy(n_temp))

            ! do kj = 1, n_temp
            !       dy(kj) = kj
            ! end do
            ! call dsort(eig, dy, n_temp)

            ! print*, 'a'
            ! do i = 1, 25
            !       print*, i, eig(i)
            ! end do

            ! stop

            ! call dav_unittest()
            ! stop

            e_nuclear = e_total - e_electron
            ! write(*, '(8I4)') irrep0(1, :)
            ! write(*, '(8I4)') irrep1(1, :)
            ! write(*, '(8I4)') irrep0(2, :)
            ! write(*, '(8I4)') irrep1(2, :)

            ! do i = 1, order
            !       if(iexci(i) .ne. 0) then
            !             print*, 'symetria', i

            !             call ci_dav_driver(dav_sigma_update_diag, dav_sigma_update_right_nondiag,&
            !                   iexci, i, irrep0, irrep1, &
            !                   e_nuclear, e_total, nocc0, nocc1, &
            !                   nvirt0, nvirt1, hci, .true., CIS_METHOD, eorb)

            !             !$ time1 = omp_get_wtime() 
            !             n_temp = nexc_single
            !             call dgeevquery(n_temp, lwork, 'N', 'N')
            !             allocate(work_sniez(lwork))
            !             allocate(eig_wi(n_temp))
            !             allocate(geev_sniez(n_temp, n_temp))
            !             call geevwrap(hci, eig, eig_wi, geev_sniez, geev_sniez, &
            !                   n_temp, "N", "N", work_sniez)
            !             !$ time2 = omp_get_wtime()                                                                                                                                                   
            !             print*, 'time1 - tim2', time2 - time1
            !             allocate(dy(n_temp))
                        
            !             do kj = 1, n_temp
            !                   dy(kj) = kj
            !             end do
            !             call dsort(eig, dy, n_temp)
                        
            !             print*, 'a'
            !             do kj = 1, 25
            !                   print*, kj, eig(kj)
            !             end do


            !       end if
            ! end do
                        

            

            e_nuclear = e_total - e_electron

            rtstart = zero
            wrci = zero

            if (multip == cc_singlet) then
                  nidx = npair + ((npair + 1) * npair) / 2
                  nidx_table(1) = nidx
                  nidx_table(2) = -1
                  print*, 'zguess sing - nidx_table, nidx', nidx_table, nidx
            else if (multip == cc_triplet) then
                  nidx_table(1) = npair * (nvirt - 1) * (nocc - 1)/ 4
                  nidx_table(2) = npair * (npair - 1) / 2
                  nidx = npair + nidx_table(1) + nidx_table(2)
                  print*, 'zguess trip -nidx_table, nidx', nidx_table, nidx
            end if

            do i = 1, order
                  if (iexci(i) .ne. 0) then
                     ! call generate_guess_cisd_dav(e_new_s, e_new_d, guess_single, guess_double, &
                     !      nocc0, nocc1, nvirt0, nvirt1, eorb, &
                     !      i, irrep0, irrep1, nroots_single, nroots_double)

                       ! call generate_guess_e_diff(nocc0, nocc1, nvirt0, nvirt1, eorb, &
                        !       i, irrep0, irrep1, sdim, ddim, e_diff)                     
                        
                        call guess_update(e_diff, t2, nocc, nocc0, nocc1, &
                              nvirt0, nvirt1, nactive, npair, nidx_table, nvirt, &
                              irrep0, irrep1, i, sdim, ddim)

                     
                        if (sd_dom .eq. CIS_METHOD) then
                           print*, 'cis', 'sdim', sdim
                           nroots = CISD_GUESS_S
                           print*, 'CISD_GUESS_S', CISD_GUESS_S
                           if (CISD_GUESS_S .gt. sdim) nroots = sdim
                           
                           if (iexci(i) .gt. nroots) then
                                 print*, iexci(i), nroots
                                 call msg("TOO MANY VECTORS REQUESTED", MSG_ERROR)
                                 stop
                           end if

                           nexc = nexc_single
                           print*, 'zguess guess_vec_r dimension', nexc, nroots
                           allocate(guess_vec_r(nexc, nroots))
                           allocate(guess_vec_l(nexc, nroots))        
                           allocate(e_new(nroots))
                           guess_vec_r = zero
                           guess_vec_l = zero
                           e_new = 0
                           print*, 'vec_cis'
                           call generate_guess_vec_cis(e_diff, e_new, guess_vec_r, &
                                 nocc0, nocc1, nvirt0, nvirt1,nroots)
                           print*, '----------------------'
                           print*, '----------------------'
                           do j = 1, nexc
                                 if (abs(guess_vec_r(j, 1)).gt.1.d-10)then
                                       print*, 'ggggg', guess_vec_r(j, 1)
                                 end if
                           end do


                     else if (sd_dom .eq. CID_METHOD) then
                           print*, 'cid'
                           nroots = CISD_GUESS_D
                           if (CISD_GUESS_D .gt. ddim) nroots = 1 + ddim

                           nexc = nexc_double + 1

                           allocate(guess_vec_r(nexc, nroots))
                           allocate(guess_vec_l(nexc, nroots))
                           allocate(e_new(nroots))
                           
                           call generate_guess_vec_cid(e_diff, e_new, guess_vec_r, &
                                 nocc0, nocc1, nvirt0, nvirt1, nroots)
                     end if


                     ! call generate_guess_vec_cisd(e_diff, e_new_s, e_new_d, guess_single, guess_double, &
                     !       nocc0, nocc1, nvirt0, nvirt1, eorb, &
                     !       i, nroots_single, nroots_double)

!                     e_new = e_new + e_total

                     ! do j = 1, nroots
                     !       print*, j, 'e-new', e_new(j)
                     ! end do

!                     e_new_d = e_new_d + e_total

                     
!                     if (nroots_single .gt. 0) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                     print*, 'cidav'
                     ! call ci_dav(i, iexci, sd_dom, nexc, nroots, &
                     !       guess_vec_r, guess_vec_l, e_total, e_electron, order, eorb, &
                     !       nocc0, nocc1, nvirt0, nvirt1, hci, irrep0, irrep1, &
                     !       e_new(1:iexci(i)))
!                     do j = 1, size(guess_vec_r, dim=1)
!                           print*, guess_vec_r(j, 1:3)
!                     end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                     call ci_dav(i, iexci, sd_dom, nexc, nroots, &
!                           guess_vec_r, guess_vec_l, e_total, e_electron, order, eorb, &
!                           nocc0, nocc1, nvirt0, nvirt1, hci, irrep0, irrep1, &
!                           e_new)



                     ! end if
                     ! if (nroots_double .gt. 0) then
                     !       call ci_dav(i, iexci, CID_METHOD, nexc_double + 1, nroots_double, &
                     !             guess_double, e_total, e_electron, order, eorb, &
                     !             nocc0, nocc1, nvirt0, nvirt1, hci, irrep0, irrep1, e_new_d(1:iexci(i)))
                     ! end if

                     
                     if (i .eq. 1) then
                           rtoffset = 0
                     else
                           rtoffset = sum(iexci(1:i-1))
                     end if

                   !   if (nroots_single .gt. 0 .and. nroots_double .gt. 0) then
!                            !
!                            ! Sort both energies from singles and doubles in
!                            ! ascending order
!                            !
!                            allocate(e_new_merged(nroots_single + nroots_double))
!                            allocate(dy(nroots_single + nroots_double))
                           
!                            e_new_merged(1:nroots_single) = e_new_s
!                            e_new_merged(nroots_single+1:nroots_single+nroots_double) =  e_new_d


!                            ! do j = 1, nroots_single + nroots_double
!                            !       print*, e_new_merged(j)
!                            ! end do

!                            do j = 1, nroots_single
!                                  dy(j) = j
!                            end do
                           
!                            l = 1
!                            do j = nroots_single + 1, nroots_single + nroots_double
!                                  dy(j) = 10**6 + l
!                                  l = l + 1
!                            end do

!                            call dsort(e_new_merged, dy, nroots_single + nroots_double)

!                            print*, 'po sort'

!                            ! do j = 1, nroots_single + nroots_double
!                            !       print*, e_new_merged(j), dy(j)
!                            ! end do

!                            do j = 1, iexci(i)
!                                  wrci(j, i) = e_new_merged(j)
! !                                 print*, j, e_new_merged(j), wrci(j, i)
!                                  if (dy(j) .lt. 10**6) then
!                                        rtstart(1:nexc_single, rtoffset + j) = guess_single(:, dy(j))
!                                  else
!                                        rtstart(nexc_single+1:nexc_single+nexc_double, rtoffset + j) = &
!                                              guess_double(2:nexc_double + 1, dy(j) - 10**6)

!                                  end if
!                            end do
                           
                     ! else if (nroots_single .gt. 0 .and. nroots_double .eq. 0)then
                     print*, 'sd_dom', sd_dom
                     if (sd_dom .eq. CIS_METHOD) then
                           do j = 1, iexci(i)
                                 print*, 'na miejsce', j, 'wpisuje wektor', j, nroots
                                 wrci(j, i) = e_new(j)
                                 rtstart(1:nexc_single, rtoffset + j) = guess_vec_r(:, j)
                           end do
                     else if (sd_dom .eq. CID_METHOD) then !(nroots_single .eq. 0 .and. nroots_double .gt. 0)then
                           do j = 1, iexci(i)
                                 wrci(j, i) = e_new(j)
                                 rtstart(nexc_single+1:nexc_single + nexc_double, rtoffset + j) = &
                                       guess_vec_r(2:nexc_double + 1, j)
                           end do
                     end if

                     if (allocated(e_new_merged)) deallocate(e_new_merged)
                     if (allocated(dy)) deallocate(dy)
                     deallocate(e_new) 

                     ! deallocate(e_new_d)
                     if (allocated(guess_vec_r)) deallocate(guess_vec_r)
                     if (allocated(guess_vec_l)) deallocate(guess_vec_l)

                  end if
            end do

      end subroutine task_cisd_dav2


      subroutine ci_dav(idx, iexci, ci_method, bdim, nroots, guess_coeff_r, guess_coeff_l, e_total, e_electron, order, eorb, &
            nocc0, nocc1, nvirt0, nvirt1, hci, irrep0, irrep1, e_new)

            integer, intent(in) :: idx 
            integer, dimension(:), intent(in) :: iexci
            integer ,intent(in) :: ci_method
            integer, intent(in) :: bdim
            integer, intent(in) :: nroots
            real(F64), dimension(:,:), intent(inout) :: guess_coeff_r, guess_coeff_l
            integer, dimension(:), allocatable :: degener
            real(F64), intent(in)                   :: e_total, e_electron
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), dimension(:,:), intent(inout) :: hci
            integer, intent(in) :: order
            integer, intent(in)                     :: nocc0, nocc1
            integer, intent(in)                     :: nvirt0, nvirt1
            integer, dimension(:,:), intent(in)     :: irrep0
            integer, dimension(:,:), intent(in)     :: irrep1
            real(F64), dimension(:), intent(inout) :: e_new

            integer, dimension(:), allocatable :: dy
            real(F64), dimension(:,:), allocatable :: work



            integer :: j
            real(F64)                   :: e_nuclear
            double precision, dimension(:), allocatable :: wr, wi
            double precision, dimension(:), allocatable :: residual_l
            double precision, dimension(:), allocatable :: residual_r
            double precision, dimension(:), allocatable :: e_diff
            double precision, dimension(:), allocatable :: e_old
            type(trecgroup) :: convrecs
            integer :: dav_info
            logical :: rightvecs_converged, leftvecs_converged
            type(tclock) :: t_iter, t_total
            integer :: nocc, nvirt, npair, nidx
            real(F64) :: convthresh
            integer :: iter
            

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nidx = npair + (npair * (npair + 1)) / 2


            e_nuclear = e_total - e_electron
            allocate(degener(nroots))
!            allocate(degener(iexci(idx)))
            degener = 1

            convthresh = CI_CONV_THRESH
            convthresh = 1.d-5
            
            call dav_init(bdim, bdim, nroots, 0, degener, .false., .true., .true., &
                  guess_coeff_r, convthresh, CC_EOM_CONV_MAXIT, CC_EOM_MEMSPACE, &
                  CC_EOM_DISKSPACE, .true., convrecs=convrecs, cisd=.true.)

            ! call dav_init(bdim, iexci(idx), degener, .false., .true., .true., &                                                                                 
            !       guess_coeff_r, convthresh, CC_EOM_CONV_MAXIT, CC_EOM_MEMSPACE, &  
            !       CC_EOM_DISKSPACE, .true., convrecs=convrecs, cisd=.true.)!prefix="dav_ci")

            allocate(wr(nroots))
            allocate(wi(nroots))
            allocate(residual_r(nroots))
            allocate(residual_l(nroots))
            allocate(e_diff(nroots))
            allocate(e_old(nroots))


            wr = e_new

            residual_r = huge(ZERO)
            residual_l = huge(ZERO)
            e_diff = huge(ZERO)
            e_old = ZERO
            e_new = ZERO
            call eom_citable_start()
            dav_info = DAV_CONTINUE
            leftvecs_converged = .false.
            rightvecs_converged = .false.

            iter = 1
            call clock_start(t_total)
            do while ((.not. leftvecs_converged) .and. (dav_info .ne. DAV_FAILURE))
                  call clock_start(t_iter)

                  if (dav_converging_right())then                  
                        call ci_dav_driver(dav_sigma_update_diag, dav_sigma_update_right_nondiag, &
                              iexci, idx, irrep0, irrep1, e_nuclear, e_total, nocc0, nocc1, &
                              nvirt0, nvirt1, hci, .false., ci_method, eorb)
                  else
                        call ci_dav_driver(dav_sigma_update_diag, dav_sigma_update_left_nondiag, &
                              iexci, idx, irrep0, irrep1, e_nuclear, e_total, nocc0, nocc1, &
                              nvirt0, nvirt1,  hci, .false., ci_method, eorb)
                  end if

                  if (.not. rightvecs_converged) then
                        call dav_iter(wr, wi, residual_r, e_diff, convrecs, dav_info, .true., e_total=e_total)
                  else
                        call dav_iter(wr, wi, residual_l, e_diff, convrecs, dav_info, .true., e_total=e_total)
                  end if


                  if (dav_converging_right()) then
                        call eom_citable_continue(DAV_NITER, DAV_BASISDIM, clock_readwall(t_iter), &
                              residual_r, e_diff, DAV_NSOUGHT)
                  else
                        call eom_citable_continue(DAV_NITER, DAV_BASISDIM, clock_readwall(t_iter), &
                              residual_l, e_diff, DAV_NSOUGHT)
                  end if

                  e_old = e_new
                  e_new = wr(1:DAV_NSOUGHT)

                  
                  call midrule(width=20)
                  do j = 1, DAV_NSOUGHT
                        e_new(j) = e_new(j) - e_total
                        write(*, '(I3, F13.10)') j, e_new(j)
                  end do
                  call midrule(width=20)


                  if (dav_info == DAV_CONVERGED) then

                        if (.not. rightvecs_converged) then
                              call msg("RIGHT EIGENVECTORS CONVERGED")
                              call msg("STARTING SEARCH FOR LEFT EIGENVECTORS")
                              rightvecs_converged = .true.
                              call dav_free()
                              ! call dav_init(bdim, iexci(idx), degener, .false., .false., .true., &
                              !       guess_coeff_r, convthresh, CC_EOM_CONV_MAXIT, CC_EOM_MEMSPACE, &
                              !       CC_EOM_DISKSPACE, .false., cisd = .true.)

                              call dav_init(bdim, bdim, nroots, 0, degener, .false., .false., .true., &
                                    guess_coeff_r, convthresh, CC_EOM_CONV_MAXIT, CC_EOM_MEMSPACE, &
                                    CC_EOM_DISKSPACE, .false., cisd = .true.)
                              iter = 1
                        else
                              call msg("LEFT EIGENVECTORS CONVERGED. EIGENSYSTEM SOLVED")
                              leftvecs_converged = .true.
                              call dav_free()
                        end if
                  end if

            end do

            allocate(dy(nroots))

            do j = 1, nroots
                  dy(j) = j
            end do

            call dsort(e_new, dy, nroots)

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
            call msg('CISD COMPLETED')
            call blankline
            !                                                                                                                          
            ! Display convergence info                                                                                        
            !                                                                                                                           
            call dav_convinfo(residual_l, residual_r)

            call toprule_double()
            call msg('CISD EXCITATION ENERGIES')
            call eom_citable_summary_start()
            do j = 1, DAV_NSOUGHT
                  call eom_citable_summary_continue(j, e_new(j), toev(e_new(j)), tocm_1(e_new(j)), idx)
            end do
            call toprule_double()


!            allocate(guess_coeff_l(bdim, nroots))
            guess_coeff_r = zero
            guess_coeff_l = zero

!            call readconverged_nonsymmetric(convrecs, .true., iexci(idx), guess_coeff_l, guess_coeff_r)
            call readconverged_nonsymmetric(convrecs, .true., nroots, guess_coeff_l, guess_coeff_r)
 !           deallocate(guess_coeff_l)

            allocate(work(bdim, nroots))

            work = guess_coeff_r
            do j = 1, nroots
                  guess_coeff_r(:, j) = work(:, dy(j))
            end do

            work = guess_coeff_l
            do j = 1, nroots
                  guess_coeff_l(:, j) = work(:, dy(j))
            end do

            deallocate(work)
            deallocate(dy)



            deallocate(residual_l)
            deallocate(residual_r)
            deallocate(e_old)
!            deallocate(e_new)
            deallocate(e_diff)
            deallocate(wr)
            deallocate(wi)
            deallocate(degener)

      end subroutine ci_dav


      subroutine generate_guess_cisd_dav(es, ed, guess_single, guess_double, &
            nocc0, nocc1, nvirt0, nvirt1, eorb, &
            idx, irrep0, irrep1, sdim, ddim)

            real(F64), dimension(:), intent(out) :: es, ed
            real(F64), dimension(:,:), intent(out) :: guess_single
            real(F64), dimension(:,:), intent(out) :: guess_double
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            real(F64), dimension(:), intent(in) :: eorb
            integer, intent(in) :: idx
            integer, dimension(:,:), intent(iN) :: irrep0, irrep1

            integer, dimension(:), allocatable    :: dy
            real(F64), dimension(:), allocatable  :: e_diff
            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles
            integer, intent(in) :: sdim, ddim
            integer :: kj
            integer :: idims, idimd
            integer :: n0i, n0a, n0j, n0b
            integer :: n1i, n1a, n1j, n1b
            integer :: ai, bj
            integer :: a, i, b, j, a0
            integer :: ibra
            integer :: nocc, nvirt
            integer :: npair, nidx, nid

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nidx = npair + (npair * (npair + 1)) / 2
            nid = nidx - npair
             
            allocate(e_diff(nidx))
            e_diff = 1.d+10

            call irrep_singless(idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

            do kj = 1, idims
                  call loop_boundaries_sp(isingles(:, kj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  do a = n0a, n1a
                        do i = n0i, n1i
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              e_diff(ai) = eorb(a) - eorb(i)
                        end do
                  end do

            end do

            deallocate(isingles)

            call irrep_doubless(idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

            do kj = 1, idimd
                  call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
                        n0j, n1j, n0b, n1b)
                  do b = n0b, n1b
                        do j = n0j, n1j
                              a0 = max(n0a, b)
                              do a = a0, n1a
                                    do i = n0i, n1i
                                          
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                          bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                                          
                                          ibra = npair + &
                                                ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1

                                          e_diff(ibra) = eorb(a) + eorb(b) - eorb(i) - eorb(j)                                          
                                    end do
                              end do
                        end do
                  end do
            end do
            deallocate(idoubles)

            if (sdim .gt. 0) then

                  allocate(dy(npair))
            
                  do kj = 1, npair
                        dy(kj) = kj
                  end do

                  call dsort(e_diff(1:npair), dy, npair)

                  
                  guess_single = zero
                  
                  do kj = 1, sdim
                        guess_single(dy(kj), kj) = one
                        es(kj) = e_diff(kj)
                  end do
                  
                  deallocate(dy)
            end if

            if (ddim .gt. 0) then

                  allocate(dy(nidx))
                  
                  do kj = 1, nidx
                        dy(kj) = kj
                  end do
                  
                  e_diff(1:npair) = 1.d+10
                  
                  call dsort(e_diff, dy, nidx)
                  
                  guess_double = zero
                  
                  do kj = 1, ddim
                        guess_double(dy(kj) - npair + 1, kj) = one
                        ed(kj) = e_diff(kj)
                  end do
            end if

      end subroutine generate_guess_cisd_dav


      subroutine generate_guess_vec_cisd(e_diff, es, ed, guess_single, guess_double, &
            nocc0, nocc1, nvirt0, nvirt1, sdim, ddim)

            real(F64), dimension(:), intent(inout) :: e_diff
            real(F64), dimension(:), intent(out) :: es, ed
            real(F64), dimension(:,:), intent(out) :: guess_single
            real(F64), dimension(:,:), intent(out) :: guess_double
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            integer, intent(in) :: sdim, ddim

            integer, dimension(:), allocatable    :: dy

            integer :: kj
            integer :: nocc, nvirt
            integer :: npair, nidx, nid

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nidx = npair + (npair * (npair + 1)) / 2
            nid = nidx - npair



            if (sdim .gt. 0) then

                  allocate(dy(npair))
            
                  do kj = 1, npair
                        dy(kj) = kj
                  end do

                  call dsort(e_diff(1:npair), dy, npair)

                  
                  guess_single = zero
                  
                  do kj = 1, sdim
                        guess_single(dy(kj), kj) = one
                        es(kj) = e_diff(kj)
                  end do
                  
                  deallocate(dy)
            end if

            if (ddim .gt. 0) then

                  allocate(dy(nidx))
                  
                  do kj = 1, nidx
                        dy(kj) = kj
                  end do
                  
                  e_diff(1:npair) = 1.d+10
                  
                  call dsort(e_diff, dy, nidx)
                  
                  guess_double = zero
                  
                  do kj = 1, ddim
                        guess_double(dy(kj) - npair + 1, kj) = one
                        ed(kj) = e_diff(kj)
                  end do
            end if

      end subroutine generate_guess_vec_cisd

      subroutine generate_guess_vec_cis(e_diff, e_new, guess_vec, &
            nocc0, nocc1, nvirt0, nvirt1, vdim)

            real(F64), dimension(:), intent(inout) :: e_diff
            real(F64), dimension(:), intent(out) :: e_new
            real(F64), dimension(:,:), intent(out) :: guess_vec
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            integer, intent(in) :: vdim

            integer, dimension(:), allocatable    :: dy

            integer :: kj
            integer :: nocc, nvirt
            integer :: npair, nidx, nid

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nidx = npair + (npair * (npair + 1)) / 2
            nid = nidx - npair
            print*, 'zguess nidx wwewnatrz generate_guess_vec_cis', nidx
            do kj = 1, nidx
                  if (abs(e_diff(kj)) < 1.d-8) then
                        e_diff(kj) = huge(zero)
                  end if
            end do

            allocate(dy(npair))

            do kj = 1, npair
                  dy(kj) = kj
            end do

            call dsort(e_diff(1:npair), dy, npair)


            guess_vec = zero
            do kj = 1, vdim
                  guess_vec(dy(kj), kj) = one
                  e_new(kj) = e_diff(kj)
            end do
            
            deallocate(dy)

      end subroutine generate_guess_vec_cis

      subroutine generate_guess_vec_cid(e_diff, e_new, guess_vec, &
            nocc0, nocc1, nvirt0, nvirt1, vdim)

            real(F64), dimension(:), intent(inout) :: e_diff
            real(F64), dimension(:), intent(out) :: e_new
            real(F64), dimension(:,:), intent(out) :: guess_vec
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            integer, intent(in) :: vdim

            integer, dimension(:), allocatable    :: dy

            integer :: kj
            integer :: nocc, nvirt
            integer :: npair, nidx, nid

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nidx = npair + (npair * (npair + 1)) / 2
            nid = nidx - npair


            allocate(dy(nidx))

            do kj = 1, nidx
                  dy(kj) = kj
            end do

            e_diff(1:npair) = 1.d+10

            call dsort(e_diff, dy, nidx)

            guess_vec = zero

            do kj = 1, vdim
                  guess_vec(dy(kj) - npair + 1, kj) = one
                  e_new(kj) = e_diff(kj)
            end do

      end subroutine generate_guess_vec_cid

      subroutine generate_guess_e_diff(nocc0, nocc1, nvirt0, nvirt1, eorb, &
            idx, irrep0, irrep1, sdim, ddim, e_diff)

            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            real(F64), dimension(:), intent(in) :: eorb
            integer, intent(in) :: idx
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            integer, intent(out) :: sdim, ddim
            real(F64), dimension(:), intent(out) :: e_diff


            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles

            integer :: kj
            integer :: idims, idimd
            integer :: n0i, n0a, n0j, n0b
            integer :: n1i, n1a, n1j, n1b
            integer :: ai, bj
            integer :: a, i, b, j, a0
            integer :: ibra
            integer :: nocc, nvirt
            integer :: npair

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt

            e_diff = 1.d+10

            call irrep_singless(idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(idx, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

            sdim = 0
            do kj = 1, idims
                  call loop_boundaries_sp(isingles(:, kj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  do a = n0a, n1a
                        do i = n0i, n1i
                              sdim = sdim + 1
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              e_diff(ai) = eorb(a) - eorb(i)
                        end do
                  end do

            end do

            deallocate(isingles)

            call irrep_doubless(idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(idx, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

            ddim = 0
            do kj = 1, idimd
                  call loop_boundaries_sp(idoubles(1:2, kj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  call loop_boundaries_sp(idoubles(3:4, kj), irrep0, irrep1, &
                        n0j, n1j, n0b, n1b)
                  do b = n0b, n1b
                        do j = n0j, n1j
                              a0 = max(n0a, b)
                              do a = a0, n1a
                                    do i = n0i, n1i
                                          ddim = ddim + 1
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                          bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                                          
                                          ibra = npair + &
                                                ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1

                                          e_diff(ibra) = eorb(a) + eorb(b) - eorb(i) - eorb(j)                                          
                                    end do
                              end do
                        end do
                  end do
            end do
            deallocate(idoubles)

      end subroutine generate_guess_e_diff

      

      subroutine  cisd_dav_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, &
            eorb, e_nuclear, e_total, nocc0, nocc1, &
            nvirt0, nvirt1, order, npair, hci, full_diag)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            integer, intent(in) :: i
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), intent(in) :: e_nuclear
            real(F64), intent(in) :: e_total
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            integer, intent(in) :: order
            integer, intent(in) :: npair
            real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag

            integer :: j

            if (full_diag)then
                  hci = zero
                  hci(1,1) = e_total
            else
                  call sigup_diag(e_total, 1)
            end if

            call cisd_ss_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                  nvirt0, nvirt1, hci, 2, 2, full_diag)
!            if (method .eq. CID_METHOD) then
            call cisd_hd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                  nvirt0, nvirt1, npair, hci, full_diag)
            call cisd_dh_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                  nvirt0, nvirt1, npair, hci, full_diag)
            call cisd_sd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                  nvirt0, nvirt1, npair, hci, full_diag)
            call cisd_ds_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                  nvirt0, nvirt1, npair, hci, full_diag)
            call cisd_dd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                  nvirt0, nvirt1, npair, hci, full_diag)
            ! end if


            if (full_diag) then
                  ! if (method .eq. CIS_METHOD) then
                  !       do j = 2, npair + 1
                  !             hci(j, j) = hci(j, j) + e_nuclear
                  !       end do
                  ! else if (method .eq. CID_METHOD) then
                        do j = 2, size(hci, dim=1)
                              hci(j, j) = hci(j, j) + e_nuclear
                        end do
                  ! end if
            end if

      end subroutine cisd_dav_driver

     subroutine  ci_dav_driver(sigup_diag, sigup_nondiag, iexci, i, irrep0, irrep1, &
            e_nuclear, e_total, nocc0, nocc1, &
            nvirt0, nvirt1, hci, full_diag, method, eorb)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            integer, dimension(:), intent(in) :: iexci
            integer, intent(in) :: i
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            real(F64), intent(in) :: e_nuclear
            real(F64), intent(in) :: e_total
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag
            integer, intent(in) :: method
            real(F64), dimension(:), intent(in) :: eorb
            integer :: j


            if (method .eq. CIS_METHOD) then

                  call cisd_ss_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                        nvirt0, nvirt1, hci, 1, 1, full_diag)

                  if (full_diag) then                  
                        do j = 1, size(hci, dim=1)
                              hci(j, j) = hci(j, j) + e_nuclear
                        end do
                  end if
            else if (method .eq. CID_METHOD) then
                  
                  call sigup_diag(e_total, 1)

                  call cisd_hd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                        nvirt0, nvirt1, 1, hci, full_diag)
                  call cisd_dh_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                        nvirt0, nvirt1, 1, hci, full_diag)
                  call cisd_dd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
                        nvirt0, nvirt1, 1, hci, full_diag)
                  ! do j = 2, size(hci, dim=1)
                  !       hci(j, j) = hci(j, j) + e_nuclear
                  ! end do
            end if
                  

      end subroutine ci_dav_driver


      subroutine cisd_ss_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
            nvirt0, nvirt1, hci, bra0, ket0, full_diag)
            
            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), intent(in) :: e_nuclear
            real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag
            
            integer, intent(in)                       :: i
            integer, dimension(:,:), intent(in)       :: irrep0
            integer, dimension(:,:), intent(in)       :: irrep1
            integer, intent(in)                       :: nocc0
            integer, intent(in)                       :: nocc1
            integer, intent(in)                       :: nvirt0
            integer, intent(in)                       :: nvirt1

            integer, intent(in) :: bra0
            integer, intent(in) :: ket0

            integer, dimension(:, :), allocatable :: isingles

            integer :: bj, kj
            integer :: n0i, n0j, n0a, n0b
            integer :: n1i, n1j, n1a, n1b
            integer :: nocc
            integer :: idims

            nocc = nocc1 - nocc0 + 1


            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)


            do bj = 1, idims
                  call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)

                  ! ket0 = 2
                  do kj = 1, idims
                        call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
                              n0j, n1j, n0b, n1b)


                        ! bocdim = (n1i-n0i+1)
                        ! bvtdim = (n1a-n0a+1)
                        ! kocdim = (n1j-n0j+1)
                        ! kvtdim = (n1b-n0b+1)
                        ! npairp = 1
                        ! call cisd_block_11(hci, npairp, newbraoffset, newketoffset, n0a, n1a, n0b, n1b, &
                        !       n0i, n1i, n0j, n1j, bra0, ket0, eorb)
                        if (full_diag) then
                              call cisd_block_11(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &                                                             
                                    n0i, n1i, n0j, n1j, bra0, ket0, eorb) 
                        else
                              call cisd_block_11_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0i, n1i, n0j, n1j, bra0, ket0, eorb, e_nuclear)
                        end if
                        ! call ci_block_s_s(hci, eorb, nocc0, nocc1, nvirt0, nvirt1, nactive, bra0, ket0, e_nuclear, &
                        !       n0i, n0j, n0a, n0b, n1i, n1j, n1a, n1b)
                        
                        ! ket0 = ket0 + kocdim * kvtdim
                  end do
                  ! bra0 = bra0 + bocdim * bvtdim
            end do

            deallocate(isingles)


      end subroutine cisd_ss_driver

     subroutine cisd_sd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
            nvirt0, nvirt1, offset, hci, full_diag)

           procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), intent(in) :: e_nuclear
           real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag


            integer, intent(in)                       :: i
            integer, dimension(:,:), intent(in)       :: irrep0
            integer, dimension(:,:), intent(in)       :: irrep1
            integer, intent(in)                       :: nocc0
            integer, intent(in)                       :: nocc1
            integer, intent(in)                       :: nvirt0
            integer, intent(in)                       :: nvirt1
            integer, intent(in)                       :: offset

            integer :: bra0
            integer :: ket0

            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles

            integer :: bj, kj
            integer :: n0i, n0j, n0k, n0a, n0b, n0c
            integer :: n1i, n1j, n1k, n1a, n1b, n1c
            integer :: nocc
            integer :: idims, idimd

            nocc = nocc1 - nocc0 + 1

            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)


            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

            bra0 = 2
            do bj = 1, idims
                  call loop_boundaries_sp(isingles(1:2, bj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)

                 ket0 = offset + 1
                  do kj = 1, idimd
                        call loop_boundaries_sp(idoubles(1:2,kj), irrep0, irrep1, &
                              n0j, n1j, n0b, n1b)
                        call loop_boundaries_sp(idoubles(3:4,kj), irrep0, irrep1, &
                              n0k, n1k, n0c, n1c)


                        ! bocdim = (n1i-n0i+1)
                        ! bvtdim = (n1a-n0a+1)
                        ! kocdim = (n1j-n0j+1) * (n1k-n0k+1)
                        ! kvtdim = (n1b-n0b+1) * (n1c-n0c+1)

                        if (full_diag) then
                              call cisd_block_12(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0)
                        else
                              call cisd_block_12_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0, eorb, e_nuclear) 
                        end if

                        ! ket0 = ket0 + kocdim * kvtdim

                  end do
                  ! bra0 = bra0 + bocdim * bvtdim
            end do

            deallocate(isingles)
            deallocate(idoubles)


      end subroutine cisd_sd_driver

     subroutine cisd_ds_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
            nvirt0, nvirt1, offset, hci, full_diag)

           procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), intent(in) :: e_nuclear
           real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag


            integer, intent(in)                       :: i
            integer, dimension(:,:), intent(in)       :: irrep0
            integer, dimension(:,:), intent(in)       :: irrep1
            integer, intent(in)                       :: nocc0
            integer, intent(in)                       :: nocc1
            integer, intent(in)                       :: nvirt0
            integer, intent(in)                       :: nvirt1
            integer, intent(in)                       :: offset

            integer :: bra0
            integer :: ket0

            integer, dimension(:, :), allocatable :: isingles
            integer, dimension(:, :), allocatable :: idoubles

            integer :: bj, kj
            integer :: n0i, n0j, n0k, n0a, n0b, n0c
            integer :: n1i, n1j, n1k, n1a, n1b, n1c
            integer :: nocc
            integer :: idims, idimd

            nocc = nocc1 - nocc0 + 1


            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .true.)
            allocate(isingles(2, idims))
            call irrep_singless(i, irrep0, irrep1, isingles, POINT_GROUP, idims, .false.)

            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

            bra0 = offset + 1
            do bj = 1, idimd
                  call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                        n0j, n1j, n0b, n1b)

                 ket0 = 2
                  do kj = 1, idims
                        call loop_boundaries_sp(isingles(1:2,kj), irrep0, irrep1, &
                              n0k, n1k, n0c, n1c)

                        ! bocdim = (n1i-n0i+1) * (n1j-n0j+1)
                        ! bvtdim = (n1a-n0a+1) * (n1b-n0b+1)
                        ! kocdim = (n1k-n0k+1)
                        ! kvtdim = (n1c-n0c+1)

                        if (full_diag) then
                              call cisd_block_21(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0)
                        else
                              call cisd_block_21_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0, eorb, e_nuclear) 
                        end if
                        
                        ! ket0 = ket0 + kocdim * kvtdim

                  end do
                  ! bra0 = bra0 + bocdim * bvtdim
            end do

            deallocate(isingles)
            deallocate(idoubles)


      end subroutine cisd_ds_driver

     subroutine cisd_dd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
            nvirt0, nvirt1, offset, hci, full_diag)

           procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), intent(in) :: e_nuclear
           real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag


            integer, intent(in)                       :: i
            integer, dimension(:,:), intent(in)       :: irrep0
            integer, dimension(:,:), intent(in)       :: irrep1
            integer, intent(in)                       :: nocc0
            integer, intent(in)                       :: nocc1
            integer, intent(in)                       :: nvirt0
            integer, intent(in)                       :: nvirt1
            integer, intent(in)                       :: offset

            integer :: bra0
            integer :: ket0

            integer, dimension(:, :), allocatable :: idoubles

            integer :: bj, kj
            integer :: n0i, n0j, n0k, n0l, n0a, n0b, n0c, n0d
            integer :: n1i, n1j, n1k, n1l, n1a, n1b, n1c, n1d
            integer :: nocc
            integer :: idimd

            nocc = nocc1 - nocc0 + 1


            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

            bra0 = offset + 1
            do bj = 1, idimd
                  call loop_boundaries_sp(idoubles(1:2, bj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  call loop_boundaries_sp(idoubles(3:4, bj), irrep0, irrep1, &
                        n0j, n1j, n0b, n1b)

                 ket0 = offset + 1
                  do kj = 1, idimd
                        call loop_boundaries_sp(idoubles(1:2,kj), irrep0, irrep1, &
                              n0k, n1k, n0c, n1c)
                        call loop_boundaries_sp(idoubles(3:4,kj), irrep0, irrep1, &
                              n0l, n1l, n0d, n1d)


                        ! bocdim = (n1i-n0i+1) * (n1j-n0j+1)
                        ! bvtdim = (n1a-n0a+1) * (n1b-n0b+1)
                        ! kocdim = (n1k-n0k+1) * (n1l-n0l+1)
                        ! kvtdim = (n1c-n0c+1) * (n1d-n0d+1)

                        if (full_diag) then
                              call cisd_block_22(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb)
                        else
                              call cisd_block_22_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                                    n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb, e_nuclear)
                        end if

                              ! ket0 = ket0 + kocdim * kvtdim

                  end do
                  ! bra0 = bra0 + bocdim * bvtdim
            end do

            deallocate(idoubles)

      end subroutine cisd_dd_driver

      subroutine cisd_hd_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
            nvirt0, nvirt1, offset, hci, full_diag)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            real(F64), dimension(:), intent(in) :: eorb
           real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag

            real(F64), intent(in) :: e_nuclear
            integer, intent(in)                       :: i
            integer, dimension(:,:), intent(in)       :: irrep0
            integer, dimension(:,:), intent(in)       :: irrep1
            integer, intent(in)                       :: nocc0
            integer, intent(in)                       :: nocc1
            integer, intent(in)                       :: nvirt0
            integer, intent(in)                       :: nvirt1
            integer, intent(in)                       :: offset

            integer :: bra0
            integer :: ket0

            integer, dimension(:, :), allocatable :: idoubles

            integer :: kj
            integer :: n0i, n0j, n0a, n0b
            integer :: n1i, n1j, n1a, n1b
            integer :: nocc
            integer :: idimd

            nocc = nocc1 - nocc0 + 1

            bra0 = 1
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)


            ket0 = offset + 1
            do kj = 1, idimd
                  call loop_boundaries_sp(idoubles(1:2,kj), irrep0, irrep1, &
                        n0i, n1i, n0a, n1a)
                  call loop_boundaries_sp(idoubles(3:4,kj), irrep0, irrep1, &
                        n0j, n1j, n0b, n1b)
                  

                  ! kocdim = (n1i-n0i+1) * (n1j-n0j+1)
                  ! kvtdim = (n1a-n0a+1) * (n1b-n0b+1)
                  
                  if (full_diag) then
                        call cisd_block_02(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                              n0i, n1i, n0j, n1j, bra0, ket0)
                  else
                        call cisd_block_02_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                              n0i, n1i, n0j, n1j, bra0, ket0, eorb, e_nuclear)
                  end if
                  
                  ! ket0 = ket0 + kocdim * kvtdim

            end do

            deallocate(idoubles)

            
      end subroutine cisd_hd_driver

      subroutine cisd_dh_driver(sigup_diag, sigup_nondiag, i, irrep0, irrep1, eorb, e_nuclear, nocc0, nocc1, &
            nvirt0, nvirt1, offset, hci, full_diag)

            procedure(dav_sigma_update_diag) :: sigup_diag
            procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), intent(in) :: e_nuclear
            real(F64), dimension(:,:), intent(out) :: hci
            logical, intent(in) :: full_diag


            integer, intent(in)                       :: i
            integer, dimension(:,:), intent(in)       :: irrep0
            integer, dimension(:,:), intent(in)       :: irrep1
            integer, intent(in)                       :: nocc0
            integer, intent(in)                       :: nocc1
            integer, intent(in)                       :: nvirt0
            integer, intent(in)                       :: nvirt1
            integer, intent(in)                       :: offset

            integer :: bra0
            integer :: ket0

            integer, dimension(:, :), allocatable :: idoubles

            integer :: bj
            integer :: n0i, n0j, n0a, n0b
            integer :: n1i, n1j, n1a, n1b
            integer :: nocc
            integer :: idimd

            nocc = nocc1 - nocc0 + 1

            ket0 = 1
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .true., .true.)
            allocate(idoubles(4, idimd))
            call irrep_doubless(i, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  idoubles, POINT_GROUP, idimd, .false., .true.)

           bra0 = offset + 1

           do bj = 1, idimd
                 call loop_boundaries_sp(idoubles(1:2,bj), irrep0, irrep1, &
                       n0i, n1i, n0a, n1a)
                 call loop_boundaries_sp(idoubles(3:4,bj), irrep0, irrep1, &
                       n0j, n1j, n0b, n1b)

                 
                 ! bocdim = (n1i-n0i+1) * (n1j-n0j+1)
                 ! bvtdim = (n1a-n0a+1) * (n1b-n0b+1)
                 if (full_diag) then
                       call cisd_block_20(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                             n0i, n1i, n0j, n1j, bra0, ket0)
                 else
                       call cisd_block_20_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, &
                             n0i, n1i, n0j, n1j, bra0, ket0, eorb, e_nuclear)
                 end if
                 ! bra0 = bra0 + bocdim * bvtdim

            end do

            deallocate(idoubles)


      end subroutine cisd_dh_driver


      ! subroutine ci_block_s_s(HCI, eorb, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0, e_nuclear, &
      !       n0i, n0j, n0a, n0b, n1i, n1j, n1a, n1b)
      !       double precision, dimension(:,:), intent(inout)     :: HCI
      !       double precision, dimension(:), intent(in)          :: eorb
      !       integer, intent(in)                                 :: nocc0, nocc1
      !       integer, intent(in)                                 :: nvirt0, nvirt1
      !       integer, intent(inout)                              :: bra0, ket0
      !       double precision, intent(in)                        :: e_nuclear
      !       integer, intent(in)                                 :: n0i, n0j, n0a, n0b
      !       integer, intent(in)                                 :: n1i, n1j, n1a, n1b
            

      !       integer :: a, b
      !       integer :: i, j
      !       integer :: ai, bj
      !       integer :: npair
      !       integer :: ibra, iket
      !       integer :: braoffset, ketoffset
      !       integer :: nocc, nvirt
      !       logical :: diag

      !       braoffset = bra0 - 1
      !       ketoffset = ket0 - 1
      !       nocc = nocc1 - nocc0 + 1
      !       nvirt = nvirt1 - nvirt0 + 1
      !       npair = nocc * nvirt



      !       diag = .false.
      !       if (n0i == n0j .and. n1i == n1j .and. n0a == n0b .and. n1a == n1b) diag = .true.

      !       if (diag .eqv. .true.) then
      !             !
      !             ! Elementary loop 1
      !             ! --------------------
      !             ! Free virtual indices: a
      !             ! Free occupied indices: i
      !             ! Equalities: b == a, j == i
      !             ! No equalities independent of the above can hold.
      !             !
      !             a_aiai: do a = n0a, n1a
      !                   i_aiai: do i = n0i, n1i
      !                         ai = (a - n0a) * (n1i-n0i+1) + (i - n0i) + 1
      !                         bj = (a - n0a) * (n1i-n0i+1) + (i - n0i) + 1
                              
      !                         ibra = braoffset + ai
      !                         iket = ketoffset + bj

      !                         if (cc_multip == cc_singlet) then
      !                               HCI(ibra, iket) = ci_s_s_aiai(eorb, a, i) + e_nuclear
      !                         else if (cc_multip == cc_triplet) then
      !                               HCI(ibra, iket) = ci_s_s_aiai_triplet(eorb, a, i) + e_nuclear
      !                         end if
      !                   end do i_aiai
      !             end do a_aiai
      !       end if
      !       !
      !       ! Elementary loop 2
      !       ! --------------------
      !       ! Free virtual indices: a, b
      !       ! Free occupied indices: i, j
      !       ! Equalities: none
      !       ! No equalities independent of the above can hold.
      !       !
                    
      !       b_aibj: do b = n0b, n1b
      !             j_aibj: do j = n0j, n1j
      !                   bj = (b - n0b) * (n1j-n0j+1) + (j - n0j) + 1
      !                   a_aibj: do a = n0a, n1a
      !                         if (a == b) cycle a_aibj
      !                         i_aibj: do i = n0i, n1i
      !                               if (i == j) cycle i_aibj

      !                               ai = (a - n0a) * (n1i-n0i+1) + (i - n0i) + 1

      !                               ibra = braoffset + ai
      !                               iket = ketoffset + bj 
                                    
      !                               if (cc_multip == cc_singlet) then
      !                                     HCI(ibra, iket) = ci_s_s_aibj(a, i, b, j)
      !                               else if (cc_multip == cc_triplet) then
      !                                     HCI(ibra, iket) = ci_s_s_aibj_triplet(a, i, b, j)
      !                               end if


      !                         end do i_aibj
      !                   end do a_aibj
      !             end do j_aibj
      !       end do b_aibj

      !        if (diag .eqv. .true.) then
      !             !
      !             ! Elementary loop 3
      !             ! --------------------
      !             ! Free virtual indices: a
      !             ! Free occupied indices: i, j
      !             ! Equalities: b == a
      !             ! No equalities independent of the above can hold.
      !             !
      !             j_aiaj: do j = n0j, n1j
      !                   a_aiaj: do a = n0a, n1a
      !                         bj = (a - n0a) * (n1j-n0j+1) + (j - n0j) + 1
      !                         i_aiaj: do i = n0i, n1i
      !                               if (i == j) cycle i_aiaj


      !                               ai = (a - n0a) * (n1i-n0i+1) + (i - n0i) + 1

      !                               ibra = braoffset + ai
      !                               iket = ketoffset + bj 
      !                               if (cc_multip == cc_singlet) then
      !                                     HCI(ibra, iket) = ci_s_s_aiaj(a, i, j)
      !                               else if (cc_multip == cc_triplet) then
      !                                     HCI(ibra, iket) = ci_s_s_aiaj_triplet(a, i, j)
      !                               end if

      !                         end do i_aiaj
      !                   end do a_aiaj
      !             end do j_aiaj

      !             !
      !             ! Elementary loop 4
      !             ! --------------------
      !             ! Free virtual indices: a, b
      !             ! Free occupied indices: i
      !             ! Equalities: j == i
      !             ! No equalities independent of the above can hold.
      !             !

      !             b_aibi: do b = n0b, n1b
      !                   a_aibi: do a = n0a, n1a
      !                         if (a == b) cycle a_aibi
      !                         i_aibi: do i = n0i, n1i

      !                                ai = (a - n0a) * (n1i-n0i+1) + (i - n0i) + 1
      !                                bj = (b - n0b) * (n1i-n0i+1) + (i - n0i) + 1

      !                                ibra = braoffset + ai
      !                                iket = ketoffset + bj 
      !                                if (cc_multip == cc_singlet) then

      !                                      HCI(ibra, iket) = ci_s_s_aibi(a, i, b)
      !                                else if (cc_multip == cc_triplet) then
      !                                      HCI(ibra, iket) = ci_s_s_aibi_triplet(a, i, b)
      !                                end if

      !                         end do i_aibi
      !                   end do a_aibi
      !             end do b_aibi
      !              end if
               
      ! end subroutine ci_block_s_s

      ! function ci_hf_hf(eorb, e_nuclear)
      !       double precision :: ci_hf_hf
      !       double precision, dimension(:), intent(in) :: eorb
      !       double precision, intent(in)                        :: e_nuclear

      !       integer :: i, j

      !       ci_hf_hf = 0.d+0

      !       do i = 1, ne / 2
      !             do j = 1, ne / 2

      !                   ci_hf_hf = ci_hf_hf -2.d+0 * oooo(i,i,j,j) + oooo(i,j,i,j)

      !             end do
      !             ci_hf_hf = ci_hf_hf + 2.d+0 * eorb(i)
      !       end do

      !       ci_hf_hf = ci_hf_hf + e_nuclear

      ! end function ci_hf_hf

      ! function ci_s_s_aibj(a, i, b, j)
      !       double precision :: ci_s_s_aibj
      !       integer, intent(in) :: a, i, b, j

      !       ci_s_s_aibj = 2.d+0 * vovo(a, i, b, j) - vvoo(a, b, i, j)

      ! end function ci_s_s_aibj

      ! function ci_s_s_aiaj(a, i, j)
      !       double precision :: ci_s_s_aiaj
      !       integer, intent(in) :: a, i, j

      !       ci_s_s_aiaj = 2.d+0 * vovo(a, i, a, j) - vvoo(a, a, i, j)

      ! end function ci_s_s_aiaj

      ! function ci_s_s_aibi(a, i, b)
      !       double precision :: ci_s_s_aibi
      !       integer, intent(in) :: a, i, b

      !       ci_s_s_aibi = 2.d+0 * vovo(a, i, b, i) - vvoo(a, b, i, i)

      ! end function ci_s_s_aibi

      ! function ci_s_s_aiai(eorb, a, i)
      !       double precision :: ci_s_s_aiai
      !       double precision, dimension(:), intent(in) :: eorb
      !       integer, intent(in) :: a, i
      !       integer :: k, l

      !       ci_s_s_aiai = - eorb(i) + eorb(a)  + 2.d+0 * vovo(a,i,a,i) - vvoo(a,a,i,i)

      !       do k = 1, ne / 2
      !             do l = 1, ne / 2
      !                   ci_s_s_aiai = ci_s_s_aiai - 2.d+0 * oooo(k, k, l, l) + oooo(k, l, k, l)
      !             end do
      !             ci_s_s_aiai = ci_s_s_aiai + 2.d+0 * eorb(k)
      !       end do

      ! end function ci_s_s_aiai



      ! function ci_hf_hf_triplet(eorb, e_nuclear)
      !       double precision :: ci_hf_hf_triplet
      !       double precision, dimension(:), intent(in) :: eorb
      !       double precision, intent(in)                        :: e_nuclear

      !       integer :: i, j

      !       ci_hf_hf_triplet = 0.d+0

      !       do i = 1, ne / 2
      !             do j = 1, ne / 2

      !                   ci_hf_hf_triplet = ci_hf_hf_triplet -2.d+0 * oooo(i,i,j,j) + oooo(i,j,i,j)

      !             end do
      !             ci_hf_hf_triplet = ci_hf_hf_triplet + 2.d+0 * eorb(i)
      !       end do

      !       ci_hf_hf_triplet = ci_hf_hf_triplet + e_nuclear

      ! end function ci_hf_hf_triplet

      ! function ci_s_s_aibj_triplet(a, i, b, j)
      !       double precision :: ci_s_s_aibj_triplet
      !       integer, intent(in) :: a, i, b, j

      !       ci_s_s_aibj_triplet =  - vvoo(a, b, i, j)

      ! end function ci_s_s_aibj_triplet

      ! function ci_s_s_aiaj_triplet(a, i, j)
      !       double precision :: ci_s_s_aiaj_triplet
      !       integer, intent(in) :: a, i, j

      !       ci_s_s_aiaj_triplet = - vvoo(a, a, i, j)

      ! end function ci_s_s_aiaj_triplet

      ! function ci_s_s_aibi_triplet(a, i, b)
      !       double precision :: ci_s_s_aibi_triplet
      !       integer, intent(in) :: a, i, b

      !       ci_s_s_aibi_triplet = - vvoo(a, b, i, i)

      ! end function ci_s_s_aibi_triplet

      ! function ci_s_s_aiai_triplet(eorb, a, i)
      !       double precision :: ci_s_s_aiai_triplet
      !       double precision, dimension(:), intent(in) :: eorb
      !       integer, intent(in) :: a, i
      !       integer :: k, l

      !       ci_s_s_aiai_triplet = - eorb(i) + eorb(a)  - vvoo(a,a,i,i)

      !       do k = 1, ne / 2
      !             do l = 1, ne / 2
      !                   ci_s_s_aiai_triplet = ci_s_s_aiai_triplet - 2.d+0 * oooo(k, k, l, l) + oooo(k, l, k, l)
      !             end do
      !             ci_s_s_aiai_triplet = ci_s_s_aiai_triplet + 2.d+0 * eorb(k)
      !       end do

      ! end function ci_s_s_aiai_triplet

      subroutine eom_citable_start()
            character(len=DEFLEN) :: line
            !                                                                                                                                                                           
            ! ITER ENERGY TDIFF EDIFF TIME                                                                                                                                              
            !                                                                                                                                                                           
            write(line, "(A3,1X,A10,1X,A12,1X,A10,1X,A10)") &
                  "#", "NVECTORS",  "MAX.RESIDUAL", "MAX.EDIFF", "TIME [s]"
            call midrule(width=76)
            call msg(line)
            call midrule(width=76)
      end subroutine eom_citable_start

    subroutine eom_citable_continue(iter, nvec, time, residual, ediff, nsought)
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
      end subroutine eom_citable_continue


      subroutine eom_citable_summary_start()
            character(len=DEFLEN) :: line
            !                                                                                                                                                                            
            ! ITER ENERGY TDIFF EDIFF TIME                                                                                                                                               
            !                                                                                                                                                                            
            write(line, "(A1, A3, 1X, A1, A15, 2X, A1, A15, 2X, A1, A20, A1, A3, A1 )") &
                  '|', "#", '|', "HARTREE",'|', "eV.",'|', "cm-1",'|', 'gk', '|'
            call midrule(width=76)
            call msg(line)
            call toprule_double(width=76)
      end subroutine eom_citable_summary_start

      subroutine eom_citable_summary_continue(iter, e_h, eeV, ecm, irrep_idx)
            integer, intent(in)          :: iter
            double precision, intent(in) :: e_h
            double precision, intent(in) :: eeV
            double precision, intent(in) :: ecm
            integer, intent(in)          :: irrep_idx

            character(len=DEFLEN) :: line
            !                                                                                                                                                                            
            ! ITER ENERGY TDIFF EDIFF TIME                                                                                                                                               
            !                                                                                                                                                                            
            write(line, "(A1, I3,1X, A1, F15.6,2X, A1, F15.6,2X,A1, F20.6, A1, A3, A1)") &
                  '|',iter,'|', e_h, '|', eeV,'|', ecm,'|', print_rep(irrep_idx), '|'
            call msg(line)
      end subroutine eom_citable_summary_continue





end module ci
