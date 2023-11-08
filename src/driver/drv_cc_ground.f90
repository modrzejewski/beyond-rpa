module drv_cc_ground
use cc_gparams
      use math_constants
      use arithmetic
      use gparam
      use basis
      use initialize
      use display
      use string
      use slater_parser
      use ccsd
      use cc3_intermediates_for_21
      use davidson_main
      use t1_transformed_int
      use density_matrix_gr_exc
      use density_matrix_ground
      use periodic
      use parser
      use ecpint
      use symmetry
      use gridfunc
      use orbextension
      use s_gen
      use scf_definitions

      implicit none

contains

      subroutine task_cc_density(molecule, method, mbpt)
            type(tmolecule), intent(in) :: molecule
            integer, intent(in)         :: method
            integer, intent(in)         :: mbpt
            real(F64), dimension(:, :), allocatable     :: mocoeff
            real(F64), dimension(:, :), allocatable     :: mocoeff_sym
            integer, dimension(:), allocatable          :: mocoeff_rep
            double precision, dimension(:), allocatable :: eorb

            double precision :: erhf
            integer          :: nocc, nvirt
            integer          :: nocc0, nocc1
            integer          :: nvirt0, nvirt1
            integer          :: npair, nidx_ccsd, nidx_cc3, nidx
            integer          :: nexcluded

            double precision, dimension(:, :), allocatable       :: t1
            double precision, dimension(:, :, :, :), allocatable :: t2
            double precision, dimension(:, :), allocatable       :: s1
            double precision, dimension(:, :, :, :), allocatable :: s2

            double precision, dimension(:), allocatable          :: eorbactive
            double precision, dimension(:), allocatable          :: eorb_symm
            integer, dimension(:), allocatable                   :: block_dim

            integer, dimension(:), allocatable :: iexci
            integer, dimension(:, :), allocatable   :: iexci_s, iexci_d
            integer, dimension(:), allocatable :: iexci_s_sum, iexci_d_sum
            real(F64)          :: enucl

            double precision, dimension(:,:), allocatable :: dmao_sym

            type(tclock)                      :: time

            double precision, dimension(:, :), allocatable :: overlap, kinetic, attraction
            integer, dimension(:, :), allocatable          :: irrep0, irrep1

            type(TSCFOutput) :: scfresults
            type(TSCFParams) :: scfinput
            integer :: nactive
            integer :: guess_dim
            integer :: order
            integer, dimension(:), allocatable :: c_idx

!            character(:), allocatable :: file_scf, file_1e, file_2e, file_aux
            integer :: nft1, nft3, nft2
            integer, parameter :: rl1 = 8
            integer :: stat
            integer :: charge_a, charge_b
            integer :: l1, l2
            integer :: iend, i
            integer :: itemp
            real(F64) :: rtemp

            call data_load(molecule)
            call init_modules()
            !
            ! Symmetry block
            !
            if (POINT_GROUP == D2h) then
                  order = D2h_order
            else if (POINT_GROUP == C2v) then
                  order = C2v_order
            end if

            print*, 'b'
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




            ! allocate(iexci(order))
            ! allocate(irrep0(2, order))
            ! allocate(irrep1(2, order))
            ! allocate(block_dim(order))

            ! if (CC_IEXCI_D .ne. "0" )then
            !       call check_cc_symmetry_irrep(iexci, order, CC_IEXCI_S)
            ! else if (CC_IEXCI_D .ne. "0") then
            !       call check_cc_symmetry_irrep(iexci, order, CC_IEXCI_S)
            ! end if
            ! if (CC_IEXCI_S .ne. "0" .and. CC_IEXCI_D .ne. "0") then
            !       call msg("NOT PROGRMED!!!!")
            !       stop
            ! end if

!            call check_cc_symmetry_irrep(iexci, order, CC_IEXCI)

            if (IMG_ISMASTER) then


                  if (SLATER_BASIS) then
                        continue
                  else
                        CC_NORB = NORB
                        call scf_symm_gauss(mocoeff, eorb, nactive, erhf, &
                              nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, overlap, enucl)
                  end if
                  if (CC_FROZEN .ne. 0) then
                        call gen_frozen_irrep(irrep0, irrep1, eorb)
                  end if
                        ! allocate(mocoeff(CC_NORB, CC_NORB)) 
                        ! allocate(eorb(CC_NORB))
                        ! !
                        ! ! Perform self-consistent RHF
                        ! !
                        ! call scfinput%init(XCF_HF)
                        ! call ksdriver(scfresults, scfinput, mocoeff, eorb, auxinput)
                        ! erhf = scfresults%e_total
                        ! nexcluded = scfresults%nexcluded

                        ! !
                        ! ! First and last active occupied orbitals
                        ! !
                        ! nocc0 = 1
                        ! nocc1 = ne/2
                        ! nocc = nocc1 - nocc0 + 1
                        ! !
                        ! ! First and last active virtual orbitals
                        ! !                  
                        ! nvirt0 = nocc + 1
                        ! nvirt1 = norb - nexcluded
                        ! nvirt = nvirt1 - nvirt0 + 1
                        
                        ! allocate(overlap(CC_NORB, CC_NORB))
                        ! allocate(kinetic(CC_NORB, CC_NORB))
                        ! allocate(attraction(CC_NORB, CC_NORB))

                        ! call stv(overlap, kinetic, attraction)
                  !       ! call smfill(overlap)

                  ! end if

                  ! allocate(mocoeff_sym(CC_NORB, CC_NORB))
                  ! allocate(mocoeff_rep(CC_NORB))
                  ! allocate(eorb_symm(CC_NORB))
                        ! allocate(c_idx(CC_NORB))
                        
                  !
                  ! Total number of active orbitals (active+virtual)
                  !
                  nocc = nocc1 - nocc0 + 1
                  nvirt = nvirt1 - nvirt0 + 1
                  npair = nocc * nvirt
                  nactive = nocc + nvirt


                  print*, 'zz'
                  print*, 'nactive', nactive
                  allocate(eorbactive(nactive))
                  eorbactive = zero

                  eorbactive(1:nocc) = eorb(nocc0:nocc1)
                  eorbactive(nocc+1:nactive) = eorb(nvirt0:nvirt1)

                  if (cc_multip == cc_singlet) then
                        select case (method)
                        case (THEORY_CCSD)
                              nidx_ccsd = npair + (npair * (npair + 1)) / 2
                              nidx = nidx_ccsd
                        case (THEORY_CC3)
                              nidx_ccsd = npair + (npair * (npair + 1)) / 2
                              nidx_cc3 = npair + ((npair + 1) * npair) / 2 + (npair**3+3*npair**2+2*npair)/6
                              nidx = nidx_cc3
                        case default
                              nidx_ccsd = 0
                              nidx = 0
                              end select
                  else if (cc_multip == cc_triplet) then
                        select case (method)
                        case (THEORY_CCSD)
                              nidx = npair + npair * (nvirt - 1) * (nocc - 1)/ 4 + npair * (npair - 1) / 2
                        case (THEORY_CC3)
                              nidx_ccsd = npair + npair * (nvirt - 1) * (nocc - 1)/ 4 + npair * (npair - 1) / 2
                              nidx = npair + (npair + 1) * (npair * (nvirt - 1) * (nocc - 1)/ 4) &
                                    + npair * (npair - 1) / 2
                        case default
                              nidx_ccsd = 0
                              nidx = 0
                        end select
                  end if

                  print*, 'zo'

                  ! eorb_symm = eorb
                  ! call msg("MUSISZ TUTAJ DOPROGRAMOWAC SYMETRIE")
                  ! stop
                  ! ! call mosymmetrize(eorb_symm, mocoeff, mocoeff_sym, mocoeff_rep, POINT_GROUP, nactive, overlap, &
                  ! !       nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, c_idx)
                  ! call symmetry_info(mocoeff_rep, POINT_GROUP)      

                  ! call blocks_dimensions_s(iexci, irrep0, irrep1, guess_dim, order, block_dim)

                  ! mocoeff = mocoeff_sym
                  ! eorb = eorb_symm

                  ! allocate(eorbactive(nactive))
                  ! eorbactive(1:nocc) = eorb(nocc0:nocc1)
                  ! eorbactive(nocc+1:nactive) = eorb(nvirt0:nvirt1)


                  ! select case (method)
                  ! case (THEORY_CCSD)
                  !       nidx_ccsd = npair + (npair * (npair + 1)) / 2
                  !       nidx = nidx_ccsd
                  ! case (THEORY_CC3)
                  !       nidx_ccsd = npair + (npair * (npair + 1)) / 2
                  !       nidx_cc3 = npair + ((npair + 1) * npair) / 2 + (npair**3+3*npair**2+2*npair)/6
                  !       nidx = nidx_cc3
                  ! case default
                  !       nidx_ccsd = 0
                  !       nidx = 0
                  ! end select

                  ! allocate(t2(nocc+1:nactive, nocc+1:nactive, nocc, nocc))
                  ! allocate(t1(nocc+1:nactive, nocc))
                  ! allocate(s2(nocc+1:nactive, nocc+1:nactive, nocc, nocc))
                  ! allocate(s1(nocc+1:nactive, nocc))                  

                  ! call ccsd_init(mocoeff, nocc, nvirt, nactive, nocc0, nocc1,&
                  !       nvirt0, nvirt1, eorbactive, irrep0, irrep1)
                  ! call t1_transformed_integrals_init(nocc, nactive, nvirt, nvirt0)
                  ! !
                  ! ! Single point CC3: calculate T1, T2, and T3 amplitudes of the ground state
                  ! ! Single point CCSD: calculate T1 and T2 amplitudes of the ground state
                  ! !
                  ! select case (method)
                  ! case (THEORY_CCSD)
                  !       call task_cc(THEORY_CCSD, mocoeff, eorbactive, erhf,  nocc, nvirt, nactive, t2, t1, &
                  !             nocc0, nocc1, nvirt0, nvirt1)
                  ! case (THEORY_CC3)
                  !       call task_cc(THEORY_CC3, mocoeff, eorbactive, erhf,  nocc, nvirt, nactive, t2, t1, &
                  !             nocc0, nocc1, nvirt0, nvirt1)
                  ! end select

                  allocate(dmao_sym(CC_NORB, CC_NORB))

                  ! call msg("GENERATING S1 and S2 amplitudes...")

                  print*, 'nocc=', nocc, 'nvirt=', nvirt, 'nidx=', nidx

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
                  select case (method)
                  case (THEORY_CCSD)
                        print*, 'task_cc'
                        call task_cc(THEORY_CCSD, mocoeff, eorbactive, erhf,  nocc, nvirt, nactive, t2, t1, &
                              nocc0, nocc1, nvirt0, nvirt1)
                  case (THEORY_CC3)
                        call task_cc(THEORY_CC3, mocoeff, eorbactive, erhf,  nocc, nvirt, nactive, t2, t1, &
                              nocc0, nocc1, nvirt0, nvirt1)
                  end select

                  ! call msg("GENERATING S1 and S2 amplitudes...")
                  ! call clock_start(time)

                  ! call generate_s1s2(t2, t1, nocc, nactive, irrep0, irrep1, s1, s2, method, s_order)
                  ! call dmsg("TOTAL TIME", clock_readwall(time))

                  if(s_order==all_s_orders)then

                        call msg("S OPERATORS UP TO 2 ORDER MBPT")
                        s1 = t1
                        s2 = t2

                        call subtask_cc_density(molecule, dmao_sym, nocc0, nocc1, nvirt0, nvirt1, &
                              nocc, nvirt, nidx, CC_NORB, nactive, method, mbpt, &
                              t2, t1, s2, s1, irrep0, irrep1, order, overlap, mocoeff, 2)

                        call clock_start(time)                  
                        call generate_s1s2(t2, t1, nocc, nactive, irrep0, irrep1, s1, s2, method, 3)                  
                        call dmsg("TOTAL TIME S-ORDER 3", clock_readwall(time))
                        stop

                        call msg("S OPERATORS UP TO 3 ORDER MBPT")                        
                        call subtask_cc_density(molecule, dmao_sym, nocc0, nocc1, nvirt0, nvirt1, &
                              nocc, nvirt, nidx, CC_NORB, nactive, method, mbpt, &
                              t2, t1, s2, s1, irrep0, irrep1, order, overlap, mocoeff, 3)

                        call clock_start(time)                  
                        call generate_s1s2(t2, t1, nocc, nactive, irrep0, irrep1, s1, s2, method, 4)                  
                        call dmsg("TOTAL TIME", clock_readwall(time))

                        call msg("S OPERATORS UP TO 4 ORDER MBPT")

                        call subtask_cc_density(molecule, dmao_sym, nocc0, nocc1, nvirt0, nvirt1, &
                              nocc, nvirt, nidx, CC_NORB, nactive, method, mbpt, &
                              t2, t1, s2, s1, irrep0, irrep1, order, overlap, mocoeff, 4)
                  else
                        if(s_order==2)then
                              s1 = t1
                              s2 = t2

                              call subtask_cc_density(molecule, dmao_sym, nocc0, nocc1, nvirt0, nvirt1, &
                                    nocc, nvirt, nidx, CC_NORB, nactive, method, mbpt, &
                                    t2, t1, s2, s1, irrep0, irrep1, order, overlap, mocoeff, s_order)
                        else
                              print*, 'a jednak tu'
                              call clock_start(time)                  
                              call generate_s1s2(t2, t1, nocc, nactive, irrep0, irrep1, s1, s2, method, s_order)                  
                              ! s1 = t1
                              ! s2 = t2
                              call dmsg("TOTAL TIME", clock_readwall(time))
                              call subtask_cc_density(molecule, dmao_sym, nocc0, nocc1, nvirt0, nvirt1, &
                                    nocc, nvirt, nidx, CC_NORB, nactive, method, mbpt, &
                                    t2, t1, s2, s1, irrep0, irrep1, order, overlap, mocoeff, s_order)
                        end if
                  end if

                  !
                  ! Save the AO density matrix to a file.
                  !
                  if (CC_SAVERHO) then
                        if (CC_SAVERHO_MODE == FILEMODE_TEXT) then
                              !
                              ! Portable textfile
                              !
                              call io_text_write(dmao_sym, CC_SAVERHO_PATH)                        
                        else if (CC_SAVERHO_MODE == FILEMODE_BINARY) then
                              !
                              ! Binary format
                              !
                              call io_binary_write(dmao_sym, CC_SAVERHO_PATH)
                        end if
                  end if

                  deallocate(mocoeff)
                  deallocate(eorb)
                  deallocate(overlap)
!                  deallocate(kinetic)
!                  deallocate(attraction)
                  deallocate(eorbactive)

                  deallocate(t2)
                  deallocate(t1)
                  deallocate(s2)
                  deallocate(s1)
                  deallocate(dmao_sym)
                  call cc_free(method)                                                                                                  
                  call t1_transformed_integrals_free()                                                                         
                  call density_free()  
            end if

            deallocate(irrep0)
            deallocate(irrep1)

            call free_modules()
            call data_free()
            call symmetry_free()

      end subroutine task_cc_density


      subroutine subtask_cc_density(molecule, dmao_sym, nocc0, nocc1, &
            nvirt0, nvirt1, nocc, nvirt, nidx, CC_NORB, nactive, method, mbpt, &
            t2, t1, s2, s1, irrep0, irrep1, order, overlap, mocoeff, s_order_small)

            type(tmolecule), intent(in)            :: molecule            
            real(F64), dimension(:,:), intent(out) :: dmao_sym
            integer, intent(in)                    :: nocc, nvirt
            integer, intent(in)                    :: nocc0, nocc1
            integer, intent(in)                    :: nvirt0, nvirt1
            integer, intent(in)                    :: CC_NORB, nidx
            integer, intent(in)                    :: nactive
            integer, intent(in)                    :: method
            integer, intent(in)                    :: mbpt
            double precision, dimension(:,:), intent(in)     :: t1
            double precision, dimension(:,:,:,:), intent(in) :: t2
            double precision, dimension(:,:), intent(in)     :: s1
            double precision, dimension(:,:,:,:), intent(in) :: s2
            integer, dimension(:, :), allocatable            :: irrep0, irrep1
            integer, intent(in)                              :: order
            real(F64), dimension(:,:), intent(in)            :: mocoeff
            real(F64), dimension(:,:), intent(in)            :: overlap
            integer, intent(in)                              :: s_order_small
            double precision, dimension(:,:), allocatable    :: dmmo
            double precision, dimension(:,:), allocatable    :: dmao
            type(tclock)                                     :: time            
            integer                                          :: i, j
            real(F64), dimension(:,:), allocatable           :: dip, quad
            !            
            ! Generate One-electron density matrix                                                                              
            !                                                                                                    
            allocate(dmmo(nactive, nactive))
            allocate(dmao(CC_NORB, CC_NORB))
            call density_ground_init(nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, &
                  dmmo)

            allocate(dip(mbpt, 3))
            allocate(quad(mbpt, 6))
            do i = 0, mbpt - 1
                  if(i==0)then
                        j = 0
                  else
                        j = i + 1
                  end if
                  print*, '-----------------------------'
                  print*, 'RZAD', j+1
                  print*, '-----------------------------'
                  call clock_start(time)
                  call msg("")
                  call msg("GENERATING ONE-ELECTRON DENSITY MATRIX...")
                  call generate_density_matrix_ground(t2, t1, s2, s1, &
                        nocc, nactive, method, dmmo, REP_Ag, irrep0, irrep1, order, j)

                  call dmsg("TOTAL TIME", clock_readwall(time))
                  call dmmo_to_dmao(dmao, dmmo, mocoeff, nactive)
                  !
                  ! Symmetrization of the dmao matrix
                  ! The antisimetric part of the dmao
                  ! matrix does not contribute to the
                  ! calcultaion of observables (the
                  ! matrices are symmetric)
                  !
                  print*, 'aa'
                  call dmao_to_dmao_sym(dmao, dmao_sym)
                  print*, 'bb'

                  call dipmom(dmao_sym, overlap, dip(i+1, :))
                  call quadru(dmao_sym, quad(i+1, :))
            end do

            print*, '____________________________________________________________________________________'
            print*, '"' // trim(molecule%path) // '"'
            call msg("MOMENTY DIPOLOWE WYBRANYCH RZEDOW")
            call msg("S ORDER ...")
            print*, s_order_small
            print*, ''
            print*, 'DIPOLE DEBYE'
            print*, ''
            do i = 0, mbpt - 1
                  if(i==0)then
                        j = 0
                  else
                        j = i + 1
                  end if
                  print*, j, ',', todebye(dip(i+1, 3))
            end do
            print*, ''
            print*, 'QUADRUPOLE BUKINGHAM'
            print*, ''
            do i = 0, mbpt - 1
                  if(i==0)then
                        j = 0
                  else
                        j = i + 1
                  end if
                  print*, j, ',', toang(todebye((quad(i+1, 3))))
            end do

            deallocate(dmmo)
            deallocate(dmao)

      end subroutine subtask_cc_density

end module drv_cc_ground
