module drv_cc_prop
      use symmetry
      use math_constants
      use arithmetic
      use gparam
      use basis
      use initialize
      use display
      use string
      use slater_parser
      use ccsd
      use ci
      use t1_transformed_int
      use density_matrix_gr_exc
      use periodic
      use parser
      use auxint
      use ecpint
      use gridfunc
      use s_gen
      use scf
      use scf_symm_driver
      use eom_vectors
      use drv_cc_aux
      use io
      use cc_gparams

      use real_linalg


      implicit none

      integer, dimension(:, :), allocatable   :: iexci_s, iexci_d
      integer, dimension(:), allocatable :: iexci_s_sum, iexci_d_sum

      integer, dimension(:, :), allocatable :: lexci_s, lexci_d
      integer, dimension(:, :), allocatable :: uexci_s, uexci_d

      integer, dimension(:), allocatable :: lexci_s_sum, lexci_d_sum
      integer, dimension(:), allocatable :: uexci_s_sum, uexci_d_sum

      integer, dimension(:, :), allocatable :: sing_lexci_s, sing_lexci_d
      integer, dimension(:, :), allocatable :: trip_lexci_s, trip_lexci_d
      integer, dimension(:, :), allocatable :: sing_uexci_s, sing_uexci_d
      integer, dimension(:, :), allocatable :: trip_uexci_s, trip_uexci_d

      integer, dimension(:), allocatable :: sing_lexci_s_sum, sing_lexci_d_sum
      integer, dimension(:), allocatable :: trip_lexci_s_sum, trip_lexci_d_sum
      integer, dimension(:), allocatable :: sing_uexci_s_sum, sing_uexci_d_sum
      integer, dimension(:), allocatable :: trip_uexci_s_sum, trip_uexci_d_sum

      integer :: order
      
      integer, parameter, private :: prop_dip = 1
      integer, parameter, private :: prop_spin =2

contains

      subroutine task_cc_properties(molecule, method)
            type(tmolecule), intent(in) :: molecule
            integer, intent(in)         :: method
            integer :: i

            call data_load(molecule)
            call init_modules()



            ! Symmetry block

            if (POINT_GROUP == D2h) then
                  order = D2h_order
            else if (POINT_GROUP == C2v) then
                  order = C2v_order
            end if

            allocate(iexci_s(2, order))
            allocate(iexci_d(2, order))

            allocate(iexci_s_sum(order))
            allocate(iexci_d_sum(order))

            iexci_s = 0
            iexci_d = 0

            iexci_s_sum = 0
            iexci_d_sum = 0


            iexci_s = 0
            iexci_d = 0

            if (CC_IEXCI_S .ne. "0" ) then 
                  call check_cc_symmetry_irrep(iexci_s, CC_IEXCI_S)
            end if
            if( CC_IEXCI_D .ne. "0") then
                  call check_cc_symmetry_irrep(iexci_d, CC_IEXCI_D)
            end if

            do i = 1, order
                  if (iexci_s(1, i) .eq. 0)then
                        iexci_s_sum(i) = iexci_s(2, i)
                  else
                        iexci_s_sum(i) = iexci_s(2, i) - iexci_s(1, i) + 1
                  end if                
                  if (iexci_d(1, i) .eq. 0)then
                        iexci_d_sum(i) = iexci_d(2, i)
                  else
                        iexci_d_sum(i) = iexci_d(2, i) - iexci_d(1, i) + 1
                  end if  
            end do

            allocate(sing_lexci_s(2, order))
            allocate(sing_lexci_d(2, order))
            allocate(sing_uexci_s(2, order))
            allocate(sing_uexci_d(2, order))

            allocate(trip_lexci_s(2, order))
            allocate(trip_lexci_d(2, order))
            allocate(trip_uexci_s(2, order))
            allocate(trip_uexci_d(2, order))


            allocate(lexci_s(2, order))
            allocate(lexci_d(2, order))
            allocate(uexci_s(2, order))
            allocate(uexci_d(2, order))

            allocate(lexci_s_sum(order))
            allocate(lexci_d_sum(order))
            allocate(uexci_s_sum(order))
            allocate(uexci_d_sum(order))

            allocate(sing_lexci_s_sum(order))
            allocate(sing_lexci_d_sum(order))
            allocate(sing_uexci_s_sum(order))
            allocate(sing_uexci_d_sum(order))

            allocate(trip_lexci_s_sum(order))
            allocate(trip_lexci_d_sum(order))
            allocate(trip_uexci_s_sum(order))
            allocate(trip_uexci_d_sum(order))


            
            lexci_s = 0
            lexci_d = 0
            uexci_s = 0
            uexci_d = 0

            sing_lexci_s = 0
            sing_lexci_d = 0
            sing_uexci_s = 0
            sing_uexci_d = 0

            trip_lexci_s = 0
            trip_lexci_d = 0
            trip_uexci_s = 0
            trip_uexci_d = 0

            if (CC_LEXCI_S .ne. "0" .or. CC_LEXCI_D .ne. "0") then
                  
                  if (CC_LEXCI_S .ne. "0") call check_cc_symmetry_irrep(lexci_s, CC_LEXCI_S)
                  if (CC_LEXCI_D .ne. "0") call check_cc_symmetry_irrep(lexci_d, CC_LEXCI_D)
                  if (CC_UEXCI_S .ne. "0") call check_cc_symmetry_irrep(uexci_s, CC_UEXCI_S)
                  if (CC_UEXCI_D .ne. "0") call check_cc_symmetry_irrep(uexci_d, CC_UEXCI_D)

                  do i = 1, order
                     call comp_exci_sum(i, lexci_s, lexci_s_sum, order)
                     call comp_exci_sum(i, lexci_d, lexci_d_sum, order)
                     call comp_exci_sum(i, uexci_s, uexci_s_sum, order)
                     call comp_exci_sum(i, uexci_d, uexci_d_sum, order)
                  end do

           end if


           if (CC_SING_LEXCI_S .ne. "0") call check_cc_symmetry_irrep(sing_lexci_s, CC_SING_LEXCI_S)
           if (CC_SING_LEXCI_D .ne. "0") call check_cc_symmetry_irrep(sing_lexci_d, CC_SING_LEXCI_D)
           if (CC_SING_UEXCI_S .ne. "0") call check_cc_symmetry_irrep(sing_uexci_s, CC_SING_UEXCI_S)
           if (CC_SING_UEXCI_D .ne. "0") call check_cc_symmetry_irrep(sing_uexci_d, CC_SING_UEXCI_D)

           if (CC_TRIP_LEXCI_S .ne. "0") call check_cc_symmetry_irrep(trip_lexci_s, CC_TRIP_LEXCI_S)
           if (CC_TRIP_LEXCI_D .ne. "0") call check_cc_symmetry_irrep(trip_lexci_d, CC_TRIP_LEXCI_D)
           if (CC_TRIP_UEXCI_S .ne. "0") call check_cc_symmetry_irrep(trip_uexci_s, CC_TRIP_UEXCI_S)
           if (CC_TRIP_UEXCI_D .ne. "0") call check_cc_symmetry_irrep(trip_uexci_d, CC_TRIP_UEXCI_D)


           do i = 1, order
              call comp_exci_sum(i, sing_lexci_s, sing_lexci_s_sum, order)
              call comp_exci_sum(i, sing_lexci_d, sing_lexci_d_sum, order)
              call comp_exci_sum(i, sing_uexci_s, sing_uexci_s_sum, order)
              call comp_exci_sum(i, sing_uexci_d, sing_uexci_d_sum, order)

              call comp_exci_sum(i, trip_lexci_s, trip_lexci_s_sum, order)
              call comp_exci_sum(i, trip_lexci_d, trip_lexci_d_sum, order)
              call comp_exci_sum(i, trip_uexci_s, trip_uexci_s_sum, order)
              call comp_exci_sum(i, trip_uexci_d, trip_uexci_d_sum, order)

           end do


           do i = 1, order
                  if (iexci_s_sum(i) .gt. cisd_guess_s)then
                        call msg("TOO FEW CISD GUESS VECTORS FOR REQUESTED EOM_CC STATES", MSG_ERROR)
                      stop
                  end if
                  if (iexci_d_sum(i) .gt. cisd_guess_d)then
                        call msg("TOO FEW CISD GUESS VECTORS FOR REQUESTED EOM_CC STATES", MSG_ERROR)
                        stop
                  end if
            end do

            call subtask_cc_properties(molecule, method)

     end subroutine task_cc_properties
     
     
     subroutine comp_exci_sum(i, exci, exci_sum, order)
       integer, intent(in) :: i
       integer, intent(in) :: order
       integer, dimension(2, order), intent(in) :: exci
       integer, dimension(order), intent(out):: exci_sum       
       
       if (exci(1, i) .eq. 0 .and. exci(2, i) .ne. 0) then
          call msg("WRONG SYMMETRY INPUT", MSG_ERROR)
          stop
       else if (exci(1, i) .gt. exci(2, i))then
          call msg("WRONG SYMMETRY INPUT", MSG_ERROR)
          stop
       else if (exci(1, i) .eq. 0 .and. exci(2, i) .eq. 0) then
          exci_sum(i) = 0
       else 
          exci_sum(i) = exci(2, i) - exci(1, i) + 1
       end if

     end subroutine comp_exci_sum
      

      subroutine subtask_cc_properties(molecule, method)

            type(tmolecule), intent(in)                   :: molecule
            integer, intent(in)                           :: method           
            real(F64), dimension(:, :), allocatable       :: mocoeff
            real(F64), dimension(:), allocatable          :: eorb
            real(F64), dimension(:, :), allocatable       :: t1
            real(F64), dimension(:, :, :, :), allocatable :: t2
            real(F64), dimension(:, :), allocatable       :: s1
            real(F64), dimension(:, :, :, :), allocatable :: s2
            real(F64), dimension(:), allocatable          :: eorbactive
            real(F64), dimension(:, :), allocatable       :: overlap
            integer, dimension(:, :), allocatable         :: irrep0, irrep1
            type(tclock)       :: time
            integer            :: nactive
            real(F64)          :: erhf, enucl
            integer   :: i, j, k
            integer   :: nocc, nvirt
            integer   :: nocc0, nocc1
            integer   :: nvirt0, nvirt1
            integer   :: npair, nidx_ccsd, nidx_cc3, nidx
            integer   :: nidx_ccsd_sing, nidx_ccsd_trip
            integer   :: nidx_cc3_sing, nidx_cc3_trip

            double precision, dimension(:, :), allocatable :: soxao, soyao, sozao, quadzzao, quadzzmo
            double precision, dimension(:, :), allocatable :: soxmo, soymo, sozmo
            integer :: nft2, l1, l2, nft3

            real(F64), dimension(:, :), allocatable :: Rho
            real(F64), dimension(:, :), allocatable :: Qzz
            real(F64), dimension(:, :), allocatable :: RhoQ
            integer :: NN
            real(F64) :: av_value

            integer(I64) :: memspace_dav, memspace
            integer(I64) :: halfmem, halfdisk, quatmem
            integer :: max_rows, max_cols, max_disk
            integer :: max_rows_mem, max_cols_mem, max_disk_mem
            integer :: max_rows_sig, max_cols_sig
            integer(I64) :: singlevector
            integer, dimension(:), allocatable :: iexci_exc_guess_s
            integer :: sss




            if (IMG_ISMASTER) then

                  if (SLATER_BASIS) then

                        call scf_symm_slater(mocoeff, eorb, nactive, erhf, &
                              nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, overlap, enucl)

                        allocate(Rho(CC_NORB, CC_NORB))                        
                        allocate(RhoQ(CC_NORB, CC_NORB))
                        !
                        ! zakladamy, że C zawiera orbitale zajete
                        !
                        call real_abT(Rho, mocoeff(:, 1:nocc1), mocoeff(:, 1:nocc1))
                        allocate(quadzzao(CC_NORB, CC_NORB))
                        allocate(quadzzmo(CC_NORB, CC_NORB))
                        call msg ("FROM TRANSMOM_Quad CCSD.f90: SLATER=TRUE")
                        l1 = CC_NORB
                        l2 = l1 * (l1 + 1) / 2
                        open(newunit=nft3, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                        call read_lower_triangle(nft3, l2, 9, quadzzao)
                        close(nft3)
                        call smfill(quadzzao)
                        print*, 'quadzzao'                                                                                                                           
                        do i = 1, CC_NORB                                                                                                                            
                              do j = 1, CC_NORB                                                                                                                      
                                    if (abs(quadzzao(i, j)).gt.1.d+3)then                                                                                            
                                          print*, i, j, quadzzao(i, j)                                                                                               
                                    end if
                              end do
                        end do
                        
                        !call atbc3(quadzzmo, mocoeff, quadzzao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
                        call real_ab(RhoQ, Rho, quadzzao)
                        av_value = ZERO
                        do k = 1, CC_NORB
                              av_value = av_value + RhoQ(k, k)
                        end do
                        !
                        ! Bierzemy pod uwage podwojne obsadzenie orbitali zajetych
                        !
                        av_value = av_value * TWO
                        print *, "Moment kwadrupolowy Qzz/HF: ", av_value
                        

!                        stop

                       !           call smfill(quadzzao)                                                                                                                                      
                        ! print*, 'quadzzao'
                        ! do i = 1, CC_NORB
                        !       do j = 1, CC_NORB
                        !             if (abs(quadzzao(i, j)).gt.1.d+3)then
                        !                   print*, i, j, quadzzao(i, j)
                        !             end if
                        !       end do
                        ! end do

                        ! print*, 'smfillllll'
                        ! call smfill(quadzzao)
                        ! print*, 'quadzzao'
                        ! do i = 1, CC_NORB
                        !       do j = 1, CC_NORB
                        !             if (abs(quadzzao(i, j)).gt.1.d-2)then
                        !                   print*, i, j, quadzzao(i, j)
                        !             end if
                        !       end do
                        ! end do
                        

           

                        
!                        call slater_test()
!                        stop

 !                        allocate(soxao(CC_NORB, CC_NORB))
 !            allocate(soyao(CC_NORB, CC_NORB))
 !            allocate(sozao(CC_NORB, CC_NORB))
 ! allocate(soxmo(CC_NORB, CC_NORB))
 !            allocate(soymo(CC_NORB, CC_NORB))
 !            allocate(sozmo(CC_NORB, CC_NORB))

 !            l1 = CC_NORB
 !            l2 = l1 * (l1 + 1) / 2
 !            print*,'read, soxao', l1, l2

 !            open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
 !            call read_lower_triangle(nft2, l2,35, soxao)
 !            close(nft2)
 !            print*, 'read soyao'
 !            open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
 !            call read_lower_triangle(nft2, l2, 36, soyao)
 !            close(nft2)
 !            print*, 'read sozao'
 !            open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
 !            call read_lower_triangle(nft2, l2, 37, sozao)
 !            close(nft2)

 !            print*,'soxao', l1, l2
 !            print*, soxao
 !            stop

 !            print*, 'macierz soxao'
 !            do i = 1, size(soxao, dim=1)
 !                  do j = 1, size(soxao, dim=2)
 !                        if (abs(soxao(i,j)).gt.1.d-8)then
 !                              print*, i, j, soxao(i, j)
 !                        end if
 !                  end do
 !            end do


 !            call smfill(soxao)
 !            call smfill(soyao)
 !            call smfill(sozao)
            
 !            call atbc3(soxmo, mocoeff, soxao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
 !            call atbc3(soymo, mocoeff, soyao, mocoeff, CC_NORB, CC_NORB, CC_NORB)
 !            call atbc3(sozmo, mocoeff, sozao, mocoeff, CC_NORB, CC_NORB, CC_NORB)

 !            print*,'soxmo', l1, l2
 !            do i = 1, size(soxmo, dim=1)
 !                  do j = 1, size(soxmo, dim=2)
 !                        if (abs(soxmo(i,j)).gt.1.d-8)then
 !                              print*, i, j, soxmo(i, j)
 !                        end if
 !                  end do
 !            end do


 !            stop


                  else
                        CC_NORB = NORB
                        
                        call scf_symm_gauss(mocoeff, eorb, nactive, erhf, &
                              nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, overlap, enucl)
                        
                  end if

                  if (CC_FROZEN .ne. 0) then
                        call gen_frozen_irrep(irrep0, irrep1, eorb)
                  end if

                  nocc = nocc1 - nocc0 + 1
                  nvirt = nvirt1 - nvirt0 + 1
                  npair = nocc * nvirt
                        
                  allocate(eorbactive(nactive))
                  eorbactive = zero

                  eorbactive(1:nocc) = eorb(nocc0:nocc1)
                  eorbactive(nocc+1:nactive) = eorb(nvirt0:nvirt1)

                  ! print*, 'eorbactive'
                  ! do i = 1, nactive
                  !       print*, i, eorbactive(i)
                  ! end do

                  ! print*, 'moco'
                  ! do i = 1, nactive
                  !       do j = 1, nactive
                  !             if (abs(mocoeff(i, j)).gt.1.d-8)then
                  !                   print*, i, j, mocoeff(i, j)
                  !             end if
                  !       end do
                  ! end do



                  print*, 'npair', npair, npair * (npair - 1) / 2, (npair * (npair + 1)) / 2, npair * (nvirt - 1) * (nocc - 1)/ 4
!                  print*, 'lla', npair + (npair * (npair + 1)) / 2
                  nidx_ccsd_sing = npair + (npair * (npair + 1)) / 2
                  nidx_ccsd_trip = npair + npair * (nvirt - 1) * (nocc - 1)/ 4 + npair * (npair - 1) / 2
                  nidx_cc3_sing = npair + ((npair + 1) * npair) / 2 + (npair**3+3*npair**2+2*npair)/6
                  nidx_cc3_trip = npair + (npair + 1) * (npair * (nvirt - 1) * (nocc - 1)/ 4) &
                        + npair * (npair - 1) / 2


                  ! write(*,'(8I4)') irrep0(1, 1:8)
                  ! write(*,'(8I4)') irrep1(1, 1:8)
                  ! write(*,'(8I4)') irrep0(2, 1:8)
                  ! write(*,'(8I4)') irrep1(2, 1:8)


 !                 print*, 'lla', npair + (npair * (npair + 1)) / 2
                  ! if (cc_multip == cc_singlet) then
                  !       select case (method)
                  !       case (THEORY_CCSD)
                  !             nidx_ccsd = npair + (npair * (npair + 1)) / 2
                  !             nidx = nidx_ccsd
                  !       case (THEORY_CC3)
                  !             nidx_ccsd = npair + (npair * (npair + 1)) / 2
                  !             nidx_cc3 = npair + ((npair + 1) * npair) / 2 + (npair**3+3*npair**2+2*npair)/6
                  !             nidx = nidx_cc3
                  !       case default
                  !             nidx_ccsd = 0
                  !             nidx = 0
                  !       end select
                  ! else if (cc_multip == cc_triplet) then
                  !       select case (method)
                  !       case (THEORY_CCSD)
                  !             nidx = npair + npair * (nvirt - 1) * (nocc - 1)/ 4 + npair * (npair - 1) / 2
                  !       case (THEORY_CC3)
                  !             nidx_ccsd = npair + npair * (nvirt - 1) * (nocc - 1)/ 4 + npair * (npair - 1) / 2
                  !             nidx = npair + (npair + 1) * (npair * (nvirt - 1) * (nocc - 1)/ 4) &
                  !                   + npair * (npair - 1) / 2
                  !       case default
                  !             nidx_ccsd = 0
                  !             nidx = 0
                  !       end select
                  ! end if


                  !-----------------------------------------------------------------
                  
                  !--------------------------------------------------------------



!                  print*, 'lla', npair + (npair * (npair + 1)) / 2
                  print*, 'nocc=', nocc, 'nvirt=', nvirt, 'nidx_ccsd_sing=', nidx_ccsd_sing, &
                        'nidx_ccsd_trip=', nidx_ccsd_trip, 'nidx_cc3_sing=', nidx_cc3_sing, &
                        'nidx_cc3_trip', nidx_cc3_trip

                  allocate(t2(nocc+1:nactive, nocc+1:nactive, nocc, nocc))
                  allocate(t1(nocc+1:nactive, nocc))
                  allocate(s2(nocc+1:nactive, nocc+1:nactive, nocc, nocc))
                  allocate(s1(nocc+1:nactive, nocc))                  
                  
                  print*, 'ccsd init zaczynam'
                  call ccsd_init(mocoeff, nocc, nvirt, nactive, nocc0, nocc1,&
                        nvirt0, nvirt1, eorbactive, irrep0, irrep1)

                  call t1_transformed_integrals_init(nocc, nactive, nvirt, nvirt0)
                  !
                  ! Single point CC3: calculate T1, T2, and T3 amplitudes of the ground state
                  ! Single point CCSD: calculate CCSD amplitudes of the ground state
                  !



                  ! allocate(iexci_exc_guess_s(8))
                  ! do i = 1, 8
                  !       iexci_exc_guess_s(i) = max(sing_lexci_s(2, i), sing_uexci_s(2, i), trip_lexci_s(2, i), trip_uexci_s(2, i))
                  ! end do

                  ! sss = sum(iexci_exc_guess_s)
                  ! memspace = int(CC_EOM_MEMSPACE / three)
                  ! halfmem = memspace / 2
                  ! halfdisk = CC_EOM_DISKSPACE / 2

                  ! singlevector = vectorsize(int(3073598440, I64))
                  ! quatmem = memspace / 4
                  ! max_rows_sig = DAV_NTRIAL
                  ! print*, 'halfmem', halfmem

                  ! print*, 'single', singlevector
                  ! max_cols_sig = int((quatmem - max_rows_sig * singlevector) &
                  ! / singlevector) - 1
                  ! max_rows = sss
                  ! print*, 'halfmem', halfmem
                  ! print*, 'max_rows', max_rows
                  ! print*, 'single', singlevector
                  ! max_cols = int((halfmem - max_rows * singlevector) &
                  !       / singlevector) - 1
                  ! print*, 'max', max_cols
                  ! halfdisk = halfdisk - (sss * singlevector) / 2
                  ! max_disk = int(halfdisk / singlevector)
                  ! ! if (max_cols < 0) then
                  !       call msg("NOT ENOUGH MEMORY TO ALLOCATE THE REQUIRED NUMBER OF VECTORS", MSG_ERROR)
                  !       stop
                  ! end if


                  select case (method)
                  case (THEORY_CCSD)
                        print*, 'task_cc'
                        call task_cc(THEORY_CCSD, mocoeff, eorbactive, erhf,  nocc, nvirt, nactive, t2, t1, &
                              nocc0, nocc1, nvirt0, nvirt1)
                  case (THEORY_CC3)
                        call task_cc(THEORY_CC3, mocoeff, eorbactive, erhf,  nocc, nvirt, nactive, t2, t1, &
                              nocc0, nocc1, nvirt0, nvirt1)
                  end select

                  call msg("GENERATING S1 and S2 amplitudes...")
                  call clock_start(time)

                  call generate_s1s2(t2, t1, nocc, nactive, irrep0, irrep1, s1, s2, method, s_order)
                  ! s1 = t1
                  ! s2 = t2
                  call dmsg("TOTAL TIME", clock_readwall(time))    

                  !if  (sum(iexci_s(1, :)) .gt. 0 .or. sum(iexci_d(1, :)) .gt. 0) then
                  print*, 'PRZED'
                  if (PROP_GR_EXC .eqv. .true.)then
                     ! call task_prop(method, mocoeff, overlap, eorbactive, &
                     !      iexci_s, iexci_s, iexci_d, iexci_d, &
                     !      iexci_s, iexci_s, iexci_d, iexci_d, &
                     !      t1, t2, s1, s2, irrep0, irrep1, nidx_ccsd_sing, nidx_ccsd_trip, &
                     !      nidx_cc3_sing, nidx_cc3_trip, order, &
                     !      nocc0, nocc1, nvirt0, nvirt1, erhf, enucl, 'G')  

                    call task_prop(method, mocoeff, overlap, eorbactive, &
                          sing_lexci_s, trip_lexci_s, sing_uexci_s, trip_uexci_s, &
                          sing_lexci_d, trip_lexci_d, sing_uexci_d, trip_uexci_d, &
                          t1, t2, s1, s2, irrep0, irrep1, nidx_ccsd_sing, nidx_ccsd_trip, &
                          nidx_cc3_sing, nidx_cc3_trip, order, &
                          nocc0, nocc1, nvirt0, nvirt1, erhf, enucl, 'G')

                  end if


                  if (PROP_EXC_EXC_DIP .or. PROP_EXC_EXC_SO) then

                        if (sum(lexci_s(1, :)) .gt. 0 .or. sum(lexci_d(1, :)) .gt. 0 .or. &
                              sum(sing_lexci_s(1, :)) .gt. 0 .or. sum(sing_lexci_d(1, :)) .gt. 0 .or.&
                              sum(trip_lexci_s(1, :)) .gt. 0 .or. sum(trip_lexci_d(1, :)) .gt. 0 .or. &
                              sum(sing_uexci_s(1, :)) .gt. 0 .or. sum(sing_uexci_d(1, :)) .gt. 0 .or.&
                              sum(trip_uexci_s(1, :)) .gt. 0 .or. sum(trip_uexci_d(1, :)) .gt. 0 ) then

                              ! call task_prop(method, mocoeff, overlap, eorbactive, &
                              !      lexci_s, uexci_s, lexci_d, uexci_d, &
                              !      t1, t2, s1, s2, irrep0, irrep1, nidx, order, &
                              !      nocc0, nocc1, nvirt0, nvirt1, erhf, enucl, 'E')

                              call task_prop(method, mocoeff, overlap, eorbactive, &
                                    sing_lexci_s, trip_lexci_s, sing_uexci_s, trip_uexci_s, &
                                    sing_lexci_d, trip_lexci_d, sing_uexci_d, trip_uexci_d, &
                                    t1, t2, s1, s2, irrep0, irrep1, nidx_ccsd_sing, nidx_ccsd_trip, &
                                    nidx_cc3_sing, nidx_cc3_trip, order, &
                                    nocc0, nocc1, nvirt0, nvirt1, erhf, enucl, 'E')  
                        end if
                  end if

            else
                  call ksgen_slave()
            end if

            call free_modules()
            call data_free()

      end subroutine subtask_cc_properties

      subroutine task_prop(method, mocoeff, overlap, eorbactive, &
           sing_lexci_s, trip_lexci_s, sing_uexci_s, trip_uexci_s, &
           sing_lexci_d, trip_lexci_d, sing_uexci_d, trip_uexci_d, &
           ! lexci_s, uexci_s, lexci_d, uexci_d, &
           t1, t2, s1, s2, irrep0, irrep1, nidx_ccsd_sing, nidx_ccsd_trip, &
           nidx_cc3_sing, nidx_cc3_trip, order, &
           nocc0, nocc1, nvirt0, nvirt1, erhf, enucl, comp)

            integer, intent(in)                           :: method           
            real(F64), dimension(:, :), intent(in)        :: mocoeff
            real(F64), dimension(:, :), intent(in)        :: overlap
            real(F64), dimension(:), intent(in)           :: eorbactive
            ! In case of task_prop for ground state sing_lexci_s = sing_iexci_s
            ! In case of task_prop for ground state sing_lexci_d = sing_iexci_d
            ! In case of task_prop for ground state trip_lexci_s = trip_iexci_s
            ! In case of task_prop for ground state trip_lexci_d = trip_iexci_d
            ! In case of task_prop for ground state uexci irrelevant
            integer, dimension(:,:), intent(in)           :: sing_lexci_s, sing_uexci_s
            integer, dimension(:,:), intent(in)           :: sing_lexci_d, sing_uexci_d
            integer, dimension(:,:), intent(in)           :: trip_lexci_s, trip_uexci_s
            integer, dimension(:,:), intent(in)           :: trip_lexci_d, trip_uexci_d
            ! integer, dimension(:,:), intent(in)           :: lexci_s, uexci_s
            ! integer, dimension(:,:), intent(in)           :: lexci_d, uexci_d
            real(F64), dimension(:, :), intent(in)        :: t1
            real(F64), dimension(:, :, :, :),  intent(in) :: t2
            real(F64), dimension(:, :),  intent(in)       :: s1
            real(F64), dimension(:, :, :, :),  intent(in) :: s2
            integer, dimension(:, :), intent(in)          :: irrep0, irrep1
            integer, intent(in)                           :: nidx_ccsd_sing, nidx_ccsd_trip
            integer, intent(in)                           :: nidx_cc3_sing, nidx_cc3_trip
            integer, intent(in)                           :: order
            integer, intent(in)                           :: nocc0, nocc1
            integer, intent(in)                           :: nvirt0, nvirt1
            real(F64), intent(in)                         :: erhf, enucl
            character(1), intent(in)                      :: comp
            
            integer :: nocc, nvirt, nactive
            integer :: npair, nidx_ccsd
            real(F64), dimension(:, :), allocatable :: guess_coeff_rs, guess_coeff_rd
            real(F64), dimension(:, :), allocatable :: wrci_s, wrci_d

                        
            integer, dimension(:), allocatable :: iexci_exc_s
            integer, dimension(:), allocatable :: iexci_exc_d
            
            integer, dimension(:), allocatable :: sing_iexci_exc_s
            integer, dimension(:), allocatable :: sing_iexci_exc_d
            integer, dimension(:), allocatable :: trip_iexci_exc_s
            integer, dimension(:), allocatable :: trip_iexci_exc_d
            
            real(F64), dimension(:, :), allocatable :: wr_exc_s, wr_exc_d
            real(F64), dimension(:, :), allocatable :: wr_exc_sing_s, wr_exc_sing_d
            real(F64), dimension(:, :), allocatable :: wr_exc_trip_s, wr_exc_trip_d

            real(F64), dimension(:, :), allocatable :: wr_sing_exc_s, wr_sing_exc_d
            real(F64), dimension(:, :), allocatable :: wr_trip_exc_s, wr_trip_exc_d                       
            
            type(trecgroup), dimension(:), allocatable  :: convrecs_s, convrecs_d
            type(trecgroup), dimension(:), allocatable  :: singrecs_s, singrecs_d
            type(trecgroup), dimension(:), allocatable  :: triprecs_s, triprecs_d
            

            real(F64), dimension(:, :), allocatable :: lvec_k1, rvec_k1

            character(:), allocatable :: prefix
            integer :: i, k            
            integer :: zn
            integer :: maxexc_s, maxexc_d

            integer :: sing_maxexc_s, sing_maxexc_d
            integer :: trip_maxexc_s, trip_maxexc_d

            integer :: maxexc_guess_d, maxexc_guess_s

            integer, dimension(:), allocatable :: iexci_exc_guess_s
            integer, dimension(:), allocatable :: iexci_exc_guess_d
            integer :: multip

            type(TState), dimension(:), allocatable :: tst
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            nactive = nocc + nvirt
            npair = nocc * nvirt

            nidx_ccsd = npair + (npair * (npair + 1)) / 2



            ! print*, sing_lexci_s(2, :)
            ! print*, sing_uexci_s(2, :)

            allocate(iexci_exc_guess_s(order))
            allocate(iexci_exc_guess_d(order))

            allocate(sing_iexci_exc_s(order))
            allocate(sing_iexci_exc_d(order))
            allocate(trip_iexci_exc_s(order))
            allocate(trip_iexci_exc_d(order))

            ! iexci_exc_s = 0
            ! iexci_exc_d = 0

            iexci_exc_guess_s = 0
            iexci_exc_guess_d = 0
            sing_iexci_exc_s = 0
            sing_iexci_exc_d = 0

            trip_iexci_exc_s = 0
            trip_iexci_exc_d = 0

            do i = 1, order
                  ! iexci_exc_s(i) = max(lexci_s(2, i), uexci_s(2, i))
                  ! iexci_exc_d(i) = max(lexci_d(2, i), uexci_d(2, i))
               iexci_exc_guess_s(i) = max(sing_lexci_s(2, i), sing_uexci_s(2, i), trip_lexci_s(2, i), trip_uexci_s(2, i))
               iexci_exc_guess_d(i) = max(sing_lexci_d(2, i), sing_uexci_d(2, i), trip_lexci_d(2, i), trip_uexci_d(2, i))
               
               sing_iexci_exc_s(i) = max(sing_lexci_s(2, i), sing_uexci_s(2, i))
               print*, 'sing_iexci', sing_iexci_exc_s(i)
               sing_iexci_exc_d(i) = max(sing_lexci_d(2, i), sing_uexci_d(2, i))
               
               trip_iexci_exc_s(i) = max(trip_lexci_s(2, i), trip_uexci_s(2, i))
               trip_iexci_exc_d(i) = max(trip_lexci_d(2, i), trip_uexci_d(2, i))
            end do

            ! maxexc_s = maxval(iexci_exc_s) 
            ! maxexc_d = maxval(iexci_exc_d)

            maxexc_guess_s = maxval(iexci_exc_guess_s)
            maxexc_guess_d = maxval(iexci_exc_guess_d)

            sing_maxexc_s = maxval(sing_iexci_exc_s) 
            sing_maxexc_d = maxval(sing_iexci_exc_d) 
            
            trip_maxexc_s = maxval(trip_iexci_exc_s) 
            trip_maxexc_d = maxval(trip_iexci_exc_d) 

            ! allocate(guess_coeff_rs(nidx_ccsd, sum(iexci_exc_s)))
            ! allocate(wrci_s(maxexc_s, order))
            
            ! allocate(guess_coeff_rd(nidx_ccsd, sum(iexci_exc_d)))
            ! allocate(wrci_d(maxexc_d, order))

            allocate(guess_coeff_rs(nidx_ccsd_sing, sum(iexci_exc_guess_s)))
            allocate(wrci_s(maxexc_guess_s, order))
            
            allocate(guess_coeff_rd(nidx_ccsd_sing, sum(iexci_exc_guess_d)))
            allocate(wrci_d(maxexc_guess_d, order))
            
            call msg('COMPUTING TRIAL VECTORS FROM CISD PROGRAM...')
            guess_coeff_rs = zero
            guess_coeff_rd = zero

            if (maxexc_guess_s .gt. 0) then
               call task_cisd_dav2(t2, iexci_exc_guess_s, wrci_s, guess_coeff_rs, &
                    eorbactive, nocc0, nocc1, nvirt0, nvirt1, nactive, &
                    erhf, erhf-enucl, order, irrep0, irrep1, 1, cc_multip)
            end if
            if (maxexc_guess_d.gt. 0) then
               call task_cisd_dav2(t2, iexci_exc_guess_d, wrci_d, guess_coeff_rd, &
                    eorbactive, nocc0, nocc1, nvirt0, nvirt1, nactive, &
                    erhf, erhf-enucl, order, irrep0, irrep1, 2, cc_multip)
            end if

            ! do i = 1, 10
            !       print*, 'wrci_wrci_wrci', wrci_s
            ! end do

            ! if (maxexc_s .gt. 0) then
            !       call task_cisd_dav2(t2, iexci_exc_s, wrci_s, guess_coeff_rs, &
            !             eorbactive, nocc0, nocc1, nvirt0, nvirt1, nactive, &
            !             erhf, erhf-enucl, order, irrep0, irrep1, 1)
            ! end if
            ! if (maxexc_d.gt. 0) then
            !       call task_cisd_dav2(t2, iexci_exc_d, wrci_d, guess_coeff_rd, &
            !             eorbactive, nocc0, nocc1, nvirt0, nvirt1, nactive, &
            !             erhf, erhf-enucl, order, irrep0, irrep1, 2)
            ! end if

            allocate(tst(4))
            if (cc_multip == cc_mixed .or. cc_multip == cc_singlet .or. cc_multip == cc_triplet) then
                  call tstate_initgroup(tst(1), order, sing_lexci_s, sing_uexci_s, nidx_ccsd_sing, nidx_cc3_sing, method)
                  call tstate_initgroup(tst(2), order, sing_lexci_d, sing_uexci_d, nidx_ccsd_sing, nidx_cc3_sing, method)
                  call tstate_initgroup(tst(3), order, trip_lexci_s, trip_uexci_s, nidx_ccsd_trip, nidx_cc3_trip, method)
                  call tstate_initgroup(tst(4), order, trip_lexci_d, trip_uexci_d, nidx_ccsd_trip, nidx_cc3_trip, method)
!                  print*, 'ano', tst(1)%tiexci,tst(2)%tiexci, tst(3)%tiexci, tst(4)%tiexci
            else if (cc_multip == cc_mixed_left) then
                  print*, nidx_ccsd_sing, nidx_ccsd_trip, 'nidx_ccsd_sing, nidx_ccsd_trip'
                  call tstate_initgroup(tst(3), order, sing_lexci_s, sing_uexci_s, nidx_ccsd_sing, nidx_cc3_sing, method)
                  call tstate_initgroup(tst(4), order, sing_lexci_d, sing_uexci_d, nidx_ccsd_sing, nidx_cc3_sing, method)
                  call tstate_initgroup(tst(1), order, trip_lexci_s, trip_uexci_s, nidx_ccsd_trip, nidx_cc3_trip, method)
                  call tstate_initgroup(tst(2), order, trip_lexci_d, trip_uexci_d, nidx_ccsd_trip, nidx_cc3_trip, method) 
            end if
            if (sing_maxexc_s .ne. 0) then
 !               print*, 'alokuje singrecs'
               allocate(wr_exc_sing_s(sum(sing_iexci_exc_s), order))
               allocate(singrecs_s(order))
               wr_exc_sing_s = zero
            end if

            if (trip_maxexc_s .ne. 0) then               
               allocate(wr_exc_trip_s(sum(trip_iexci_exc_s), order))
               allocate(triprecs_s(order))
               wr_exc_trip_s = zero
            end if

            if (sing_maxexc_d .ne. 0) then               
               allocate(wr_exc_sing_d(sum(sing_iexci_exc_d), order))
               allocate(singrecs_d(order))
               wr_exc_sing_d = zero
            end if

            if (trip_maxexc_d .ne. 0) then               
               allocate(wr_exc_trip_d(sum(trip_iexci_exc_d), order))
               allocate(triprecs_d(order))
               wr_exc_trip_d = zero
            end if

            ! if (maxexc_s .ne. 0) then               
            !       allocate(wr_exc_s(sum(iexci_exc_s), order))
            !       allocate(convrecs_s(order))
            !       wr_exc_s = zero
            ! end if

            ! if (maxexc_d .ne. 0) then               
            !    allocate(wr_exc_d(sum(iexci_exc_d), order))
            !    allocate(convrecs_d(order))
            !    wr_exc_d = zero
            ! end if
            
            ! allocate(lvec_k1(nidx, 1))
            ! allocate(rvec_k1(nidx, 1))


            do i = 1, order          
!                  print*, 'i'
                  print*, 'TERAZ BEDZIE SYMETRIA', i, sing_iexci_exc_s(i)
               if (sing_iexci_exc_s(i) .ne. 0) then
!                     print*, 'AUUUUUU1'
                  prefix = "dav_eom_sym_sing_s_" // str(i) // "_"
!                  print*, 'podaje prefix numer ', prefix
                  call calc_vec(i, method, sing_lexci_s, sing_uexci_s, trip_lexci_s, trip_uexci_s, wr_exc_sing_s, wrci_s, &
                       guess_coeff_rs, singrecs_s(i), prefix, &
                       nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
                       eorbactive, t2, order, cc_singlet)
                  print*, 'dupadupa3'
                  ! call readconverged_nonsymmetric_single(singrecs_s(i), &
                  !       1, lvec_k1(:, 1), rvec_k1(:, 1))
                  ! stop
               end if
               
               if (sing_iexci_exc_d(i) .ne. 0) then
!                     print*, 'AUUUUUU2'
                  prefix = "dav_eom_sym_sing_d_" // str(i) // "_"
                  call calc_vec(i, method, sing_lexci_d, sing_uexci_d, trip_lexci_d, trip_uexci_d, wr_exc_sing_d, wrci_d, &
                       guess_coeff_rd, singrecs_d(i), prefix, &
                       nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
                       eorbactive, t2, order, cc_singlet)
               end if
               
               if (trip_iexci_exc_s(i) .ne. 0) then
                  prefix = "dav_eom_sym_trip_s_" // str(i) // "_"
                  call calc_vec(i, method, sing_lexci_s, sing_uexci_s, trip_lexci_s, trip_uexci_s, wr_exc_trip_s, wrci_s, &
                       guess_coeff_rs, triprecs_s(i), prefix, &
                       nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
                       eorbactive, t2, order, cc_triplet)
               end if
               
               if (trip_iexci_exc_d(i) .ne. 0) then
                  prefix = "dav_eom_sym_trip_d_" // str(i) // "_"
                  call calc_vec(i, method, sing_lexci_d, sing_uexci_d, trip_lexci_d, trip_uexci_d, wr_exc_trip_d, wrci_d, &
                       guess_coeff_rd, triprecs_d(i), prefix, &
                       nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
                       eorbactive, t2, order, cc_triplet)
               end if
            end do               

            print*, 'dupa1'
            
            call toprule_double()
            if(method .eq. THEORY_CCSD)then
                  call msg('EOM-CCSD EXCITATION ENERGIES')
            else if(method .eq. THEORY_CC3)then
                  call msg('EOM-CC3 EXCITATION ENERGIES')
            else if(method .eq. THEORY_CC3_MEM)then
                  call msg('EOM-CC3 EXCITATION ENERGIES')
            end if

            call eom_cctableall_summary_start()

           do i = 1, order
                 zn = 0
               if (sing_iexci_exc_s(i) .ne. 0) then
                     call eom_cctableall_summary_continue(i, 1, wr_exc_sing_s(:, i))
                     zn = 1
               end if

               if (sing_iexci_exc_d(i) .ne. 0) then
                     call eom_cctableall_summary_continue(i, 1, wr_exc_sing_d(:, i))
                     zn = 1
               end if

               if (trip_iexci_exc_s(i) .ne. 0) then
                     call eom_cctableall_summary_continue(i, 1, wr_exc_trip_s(:, i))
                     zn = 1
               end if

               if (trip_iexci_exc_d(i) .ne. 0) then
                     call eom_cctableall_summary_continue(i, 1, wr_exc_trip_d(:, i))
                     zn = 1
               end if
               if (zn ==1) then
                     call midrule(width=40)
               end if
            end do
            call toprule_double(width=40)
!            stop
            

            !
            ! Check if EOM-CC is to be executed or read from disk
            !
 !           if (EOM_READ .eqv. .false.)then 
                  ! do i = 1, order                  
                  !       if (iexci_exc_s(i) .ne. 0) then
                  !             print*, iexci_exc_s(i), 'stanow, zapisuje do', i
                  !             prefix = "dav_eom_sym_s_" // str(i) // "_"
                  !             print*, 'wrci_s', wrci_s
                  !             call calc_vec(i, method, lexci_s, uexci_s, wr_exc_s, wrci_s, &
                  !                   guess_coeff_rs, convrecs_s(i), prefix, &
                  !                   nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
                  !                   eorbactive, t2, order)
                  !       end if
                        
                  !       if (iexci_exc_d(i) .ne. 0) then
                  !             prefix = "dav_eom_sym_d_" // str(i) // "_"
                  !             call calc_vec(i, method, lexci_d, uexci_d, wr_exc_d, wrci_d, &
                  !                   guess_coeff_rd, convrecs_d(i), prefix, &
                  !                   nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
                  !                   eorbactive, t2, order)
                  !       end if
                  ! end do
!                  print*, 'EOM_WRITE', EOM_WRITE
!                  if(EOM_WRITE .eqv. .true.)then
!                        call eom_write_aux(method, convrecs_s, convrecs_d, order,iexci_exc_s, iexci_exc_d)
!                  end if
!            end if

            call eom_vectors_init(nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt)
            print*, 'dupa2'
            if (cc_multip == cc_singlet .or. cc_multip == cc_triplet .or. cc_multip == cc_mixed) then
                  if(maxval(tst(1)%tiexci) .ne. 0) then
                        tst(1)%trecs = singrecs_s
                        tst(1)%twr_exc = wr_exc_sing_s
                  end if
                  
                  if(maxval(tst(2)%tiexci) .ne. 0) then
                        tst(2)%trecs = singrecs_d
                        tst(2)%twr_exc = wr_exc_sing_d
                  end if
                  
                  if(maxval(tst(3)%tiexci) .ne. 0) then
                        tst(3)%trecs = triprecs_s
                        tst(3)%twr_exc = wr_exc_trip_s
                  end if
                  
                  if(maxval(tst(4)%tiexci) .ne. 0) then
                        tst(4)%trecs = triprecs_d
                        tst(4)%twr_exc = wr_exc_trip_d
                  end if
                  
                  tst(1)%tlexci = sing_lexci_s
                  tst(2)%tlexci = sing_lexci_d
                  tst(3)%tlexci = trip_lexci_s
                  tst(4)%tlexci = trip_lexci_d
                  
                  tst(1)%tuexci = sing_uexci_s
                  tst(2)%tuexci = sing_uexci_d
                  tst(3)%tuexci = trip_uexci_s
                  tst(4)%tuexci = trip_uexci_d
                  
                  tst(1)%tmaxexc = sing_maxexc_s
                  tst(2)%tmaxexc = sing_maxexc_d
                  tst(3)%tmaxexc = trip_maxexc_s
                  tst(4)%tmaxexc = trip_maxexc_d

            else if (cc_multip == cc_mixed_left) then
                  if(maxval(tst(3)%tiexci) .ne. 0) then
                        tst(3)%trecs = singrecs_s
                        tst(3)%twr_exc = wr_exc_sing_s
                  end if
                  
                  if(maxval(tst(4)%tiexci) .ne. 0) then
                        tst(4)%trecs = singrecs_d
                        tst(4)%twr_exc = wr_exc_sing_d
                  end if
                  
                  if(maxval(tst(1)%tiexci) .ne. 0) then
                        tst(1)%trecs = triprecs_s
                        tst(1)%twr_exc = wr_exc_trip_s
                  end if
                  
                  if(maxval(tst(2)%tiexci) .ne. 0) then
                        tst(2)%trecs = triprecs_d
                        tst(2)%twr_exc = wr_exc_trip_d
                  end if
                  
                  tst(3)%tlexci = sing_lexci_s
                  tst(4)%tlexci = sing_lexci_d
                  tst(1)%tlexci = trip_lexci_s
                  tst(2)%tlexci = trip_lexci_d
                  
                  tst(3)%tuexci = sing_uexci_s
                  tst(4)%tuexci = sing_uexci_d
                  tst(1)%tuexci = trip_uexci_s
                  tst(2)%tuexci = trip_uexci_d
                  
                  tst(3)%tmaxexc = sing_maxexc_s
                  tst(4)%tmaxexc = sing_maxexc_d
                  tst(1)%tmaxexc = trip_maxexc_s
                  tst(2)%tmaxexc = trip_maxexc_d
               end if

               print*, 'dupa3'
            if (comp .eq. 'G') then
                  call compute_transitions_gr_exc2(method, mocoeff, overlap, &
                        t2, t1, s2, s1, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
                        nactive, irrep0, irrep1, order, tst)
               ! call compute_transitions_gr_exc(method, mocoeff, overlap, t2, t1, s2, s1, nocc0, nocc1, &
               !      nvirt0, nvirt1, nocc, nvirt, nactive, nidx, irrep0, irrep1, &
               !      order, convrecs_s, convrecs_d, wr_exc_s, wr_exc_d, maxexc_s, maxexc_d, &
               !      lexci_s, lexci_d)

            else if (comp .eq. 'E') then
               print*, 'dupa4'   
                  if (PROP_EXC_EXC_DIP .eqv. .true.) then
                        call compute_transitions_exc_exc(method, mocoeff, overlap, t2, t1, s2, s1, nocc0, nocc1, &
                              nvirt0, nvirt1, nocc, nvirt, nactive, irrep0, irrep1, &
                              order, tst, prop_dip)
                  end if

                  if (PROP_EXC_EXC_SO .eqv. .true.) then
                        call compute_transitions_exc_exc(method, mocoeff, overlap, t2, t1, s2, s1, nocc0, nocc1, &
                              nvirt0, nvirt1, nocc, nvirt, nactive, irrep0, irrep1, &
                              order, tst, prop_spin)
                  end if
            else 
               call msg("UNRECOGNIZED KEY", MSG_ERROR)
               stop
            end if

            call cc_free(method)


            ! print*, 'lalak'
            ! if (maxexc_s .ne. 0) then
            !       do i = 1, order
            !             call io_record_deletegroup(convrecs_s(i))                                                          
            !       end do
            ! end if
            ! print*, 'kakak'
            ! if (maxexc_d .ne. 0) then
            !       do i = 1, order
            !             call io_record_deletegroup(convrecs_d(i))                                             
            !       end do
            ! end if
            ! print*, 'sakak'
          end subroutine task_prop
           
          subroutine calc_vec(k1, method, sing_lexci, sing_uexci, trip_lexci, trip_uexci, wr_exc, wrci, guess_coeff_r, convrecs, prefix, &
               nocc, nactive, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, &
               eorbactive, t2, order, multip)

            integer, intent(in)                           :: k1
            integer, intent(in)                           :: method           
            integer, dimension(:, :), intent(in)          :: sing_lexci, sing_uexci
            integer, dimension(:, :), intent(in)          :: trip_lexci, trip_uexci
            real(F64), dimension(:, :), intent(inout)     :: wr_exc
            real(F64), dimension(:,:), intent(in)         :: wrci
            real(F64), dimension(:,:), intent(in)         :: guess_coeff_r
            type(trecgroup), intent(out)                  :: convrecs
            character(*), intent(in)                      :: prefix
            integer, intent(in)                           :: nocc0, nocc1
            integer, intent(in)                           :: nvirt0, nvirt1
            integer, intent(in)                           :: nocc, nactive
            integer, dimension(:,:), intent(in)           :: irrep0
            integer, dimension(:,:), intent(in)           :: irrep1
            real(F64), dimension(:), intent(in)           :: eorbactive
            real(F64), dimension(:,:, :, :), intent(in)   :: t2
            integer, intent(in)                           :: order
            integer, intent(in)                           :: multip
            type(tclock)       :: time                  
            integer :: gs0, gs1, gdim
            integer :: ntargetvecs

            integer :: v0, p, q
            integer, dimension(:), allocatable :: lexci_sum, uexci_sum
            integer :: exctype

            real(F64), dimension(:), allocatable :: wrdav_small
            real(F64), dimension(:), allocatable :: widav_small
            integer, dimension(:), allocatable :: degener_small

            integer, dimension(:), allocatable :: exci_max

            integer :: i, j
            integer :: nvirt
            real(F64), dimension(:, :), allocatable :: lvec_k1, rvec_k1

            nvirt = nvirt1 - nvirt0 + 1

!            call generate_intermediates_block_21_init(nocc, nactive, nocc0, nvirt0, irrep0, irrep1, multip)!*

            if (multip == cc_singlet) then
               ntargetvecs = max(sing_lexci(2, k1), sing_uexci(2, k1))
            else if (multip == cc_triplet) then
               ntargetvecs = max(trip_lexci(2, k1), trip_uexci(2, k1))
            end if
            
            allocate(exci_max(order))

            allocate(lexci_sum(order))
            allocate(uexci_sum(order))

            do i = 1, order
               exci_max(i) = max(sing_lexci(2, i), sing_uexci(2, i), trip_lexci(2, i), trip_uexci(2, i))
            end do

            !                  ntargetvecs = iexci_exc(k1)

            if (k1 == 1) then
               gs0 = 1
               gs1 = sum(exci_max(1:k1))
            else
               gs0 = sum(exci_max(1:k1-1)) + 1
               gs1 = sum(exci_max(1:k1)) 
            end if
            gdim = gs1 - gs0 + 1 

            allocate(degener_small(ntargetvecs))
            allocate(wrdav_small(gdim))
            allocate(widav_small(gdim))

            wrdav_small = zero
            widav_small = zero
            degener_small = 1


            wrdav_small(1:exci_max(k1)) = wrci(1:exci_max(k1), k1)

            ! call clock_start(time)
            ! call msg("GENERATING INTERMEDIATES FOR BLOCK 21...")

            ! if (multip == cc_singlet)then
            !    call generate_intermediates_block_21(method, nocc, nactive)                              !*
            ! end if
            ! call msg("DONE")

!            call dmsg("TOTAL TIME", clock_readwall(time))
            call task_eom_cc_dav(method, wrdav_small, widav_small, eorbactive, nocc0, &
                 nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, t2, ntargetvecs, degener_small, &
                 exci_max, irrep0, irrep1, order, k1, convrecs, prefix, &
                 guess_coeff_r(:, gs0:gs1), multip)

            ! allocate(lvec_k1(nidx, 1))
            ! allocate(rvec_k1(nidx, 1))
            ! print*, 'pluszurrro2'
            ! call readconverged_nonsymmetric_single(convrecs, &
            !       1, lvec_k1(:, 1), rvec_k1(:, 1))
            ! stop


            wr_exc(1:gdim, k1) = wrdav_small

            print*, 'dupadupa1'
            deallocate(degener_small)
            deallocate(wrdav_small)
            deallocate(widav_small)

!            call generate_intermediates_block_21_free(multip)!*


          end subroutine calc_vec

     subroutine eom_cctableall_summary_start()
            character(len=50) :: line
            !                                                                                                                            
            ! ITER ENERGY TDIFF EDIFF TIME                                                                                                           
            !                                                                                                                                      
            write(line, "(A1, A5, 1X, A1, A15, 2X, A1 )") &
                  '|', "REP", '|', "HARTREE",'|'
            call midrule(width=40)
            call msg(line)
            call toprule_double(width=40)
      end subroutine eom_cctableall_summary_start

      subroutine eom_cctableall_summary_continue(irrep_idx, multip, e_h)
            real(F64), dimension(:), intent(in) :: e_h
            integer, intent(in)          :: irrep_idx
            integer, intent(in)          :: multip
            integer :: i

            character(len=DEFLEN) :: line
            !                                                                                                                           
            ! ITER ENERGY TDIFF EDIFF TIME                                                                                
            !           
            do i = 1, size(e_h, 1)
                  write(line, "(A1, A5, 1X, A1, F15.6, 2X, A1)") '|', print_rep_mult(irrep_idx, multip), '|', e_h(i), '|'
                  call msg(line)
            end do
      end subroutine eom_cctableall_summary_continue


end module drv_cc_prop




