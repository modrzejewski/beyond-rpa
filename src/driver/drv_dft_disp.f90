module drv_dft_disp
      use arithmetic
      use math_constants
      use display
      use string
      use report
      use gparam
      use basis
      use initialize
      use dftd3
      use scf
      use mbd
      use drv_dft
      use real_scf
      use scf_definitions

      implicit none

contains

      subroutine dftd3_setup(usrparam, dftd3_usrdef, bj_damping)
            real(F64), dimension(5), intent(out) :: usrparam
            logical, intent(out)                 :: dftd3_usrdef
            logical, intent(out)                 :: bj_damping

            if (DFT_DISP == DISP_DFTD3_BJ) then
                  bj_damping = .true.
            else if (DFT_DISP == DISP_DFTD3) then
                  bj_damping = .false.
            else
                  call msg("DFT_DISP undefined: specify the kind of DFT-D3 damping", MSG_ERROR)
                  stop
            end if

            usrparam = ZERO
            if (bj_damping) then
                  usrparam(DFTD3_BJ_IS6) = 1.0_F64
                  dftd3_usrdef = .false.
                  if (DFTD3_BJ_A1 > ZERO .and. DFTD3_BJ_A2 > ZERO .and. DFTD3_BJ_S8 >= ZERO) then
                        usrparam(DFTD3_BJ_IA1) = DFTD3_BJ_A1
                        usrparam(DFTD3_BJ_IA2) = DFTD3_BJ_A2
                        usrparam(DFTD3_BJ_IS8) = DFTD3_BJ_S8
                        dftd3_usrdef = .true.
                  else if (DFTD3_BJ_A1 > ZERO .or. DFTD3_BJ_A2 > ZERO .or. DFTD3_BJ_S8 >= ZERO) then
                        call msg("One of DFT-D3(BJ) parameters (a1, a2, s8) is undefined", MSG_ERROR)
                        stop
                  end if
            else
                  usrparam(DFTD3_IS6) = 1.0_F64
                  usrparam(DFTD3_IR8) = 1.0_F64
                  usrparam(DFTD3_IAL) = 14.0_F64
                  dftd3_usrdef = .false.
                  if (DFTD3_R6 > ZERO .and. DFTD3_S8 >= ZERO) then
                        usrparam(DFTD3_IR6) = DFTD3_R6
                        usrparam(DFTD3_IS8) = DFTD3_S8
                        dftd3_usrdef = .true.
                  else if (DFTD3_R6 > ZERO .or. DFTD3_S8 >= ZERO) then
                        call msg("One of DFT-D3 parameters (r6, s8) is undefined", MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine dftd3_setup
      

      subroutine task_dftd3_optim()
            real(F64), parameter :: r6_lbound = 0.50_F64
            real(F64), parameter :: r6_ubound = 2.00_F64
            real(F64), parameter :: r6_step = 0.05_F64

            integer :: i, j, k
            integer, parameter :: nr6 = nint((r6_ubound - r6_lbound) / r6_step) + 1
            real(F64), dimension(nr6), parameter :: r6 = &            
                  [(r6_lbound + real(i, F64) * r6_step, i = 0, nr6-1)]

            real(F64), parameter :: s8_lbound = 0.00_F64
            real(F64), parameter :: s8_ubound = 2.00_F64
            real(F64), parameter :: s8_step = 0.05_F64

            integer, parameter :: ns8 = nint((s8_ubound - s8_lbound) / s8_step) + 1
            real(F64), dimension(ns8), parameter :: s8 = &            
                  [(s8_lbound + real(i, F64) * s8_step, i = 0, ns8-1)]

            real(F64), dimension(5) :: usrparam
            logical :: dftd3_usrdef, bj_damping
            integer, parameter :: ncells_max = 5
            integer :: ncells
            character(len=DEFLEN), dimension(ncells_max) :: cells
            type(tsystemdep_params) :: par
            integer :: ndimers
            type(tmolecule) :: geom_a, geom_b, geom_ab

            if (IMG_ISMASTER) then
                  call dftd3_setup(usrparam, dftd3_usrdef, bj_damping)
                  if (.not. dftd3_usrdef) then
                        call msg("Undefined parameters of the DFT-D3 damping function", MSG_ERROR)
                        stop
                  end if
                  
                  if (DOREPORT) then
                        if (bj_damping) then
                              ncells = 5
                              cells(1) = '"dimer"'
                              cells(2) = '"a1"'
                              cells(3) = '"a2"'
                              cells(4) = '"s8"'
                              cells(5) = '"Edisp [kcal/mol]"'
                        else
                              ncells = 4
                              cells(1) = '"dimer"'
                              cells(2) = '"R6"'
                              cells(3) = '"S8"'
                              cells(4) = '"Edisp [kcal/mol]"'
                        end if
                        call rep_update(cells, ncells)
                  end if

                  ndimers = NJOB / 3
                  call msg("DFT-D3 PARAMETERS OPTIMIZATION")
                  call imsg("GRID POINTS PER MOLECULE", ns8 * nr6)
                  call imsg("NUMBER OF MOLECULES", ndimers)
                  call imsg("TOTAL NUMBER OF GRID POINTS", ns8 * nr6 * ndimers)

                  do k = 1, ndimers
                        call dequeue_job(geom_a, par, 2 * k - 1, GEOM_MONOMER)
                        call dequeue_job(geom_b, par, 2 * k, GEOM_MONOMER)
                        call dequeue_job(geom_ab, par, k, GEOM_COMPLEX)
                        do i = 1, nr6
                              do j = 1, ns8
                                    usrparam(DFTD3_IR6) = r6(i)
                                    usrparam(DFTD3_IS8) = s8(j)
                                    call dftd3_optim_gridpoint(geom_a, geom_b, geom_ab, usrparam, bj_damping)
                              end do
                        end do
                  end do
            end if
      end subroutine task_dftd3_optim


      subroutine dftd3_optim_gridpoint(mola, molb, molab, usrpar, bj_damping)
            !
            ! Optimization of DFT-D3 parameters 
            ! ---
            ! Compute DFT-D3 contribution to interaction energy.
            ! This contribution should be added to a semilocal
            ! part of the interaction energy. User-defined parameters
            ! are utilized.
            ! --------------------------------------------------------
            !
            type(tmolecule), intent(in)                :: mola
            type(tmolecule), intent(in)                :: molb
            type(tmolecule), intent(in)                :: molab
            double precision, dimension(:), intent(in) :: usrpar
            logical, intent(in)                        :: bj_damping

            double precision :: energy_a, energy_b, energy_ab, eint
            type(txcdef) :: ixc

            integer, parameter :: ncells_max = 5
            integer :: ncells
            character(len=DEFLEN), dimension(ncells_max) :: cells

            if (IMG_ISMASTER) then
                  if (bj_damping) then
                        ncells = 5
                  else
                        ncells = 4
                  end if
                  !
                  ! Determine numerical code for the loaded 
                  ! XC functional
                  !
                  if (ROKS_ENABLED) then
                        call xcf_define(ixc, XCMODEL, AUX_NONE, .true.)
                  else
                        call xcf_define(ixc, XCMODEL, AUX_NONE, .false.)
                  end if
                  !
                  ! Monomer A
                  !
                  call data_load(mola)
                  call init_modules()
                  call compdftd3(ixc, energy_a, DFTD3_3BODY, bj_damping, usrpar)
                  call free_modules()
                  call data_free()
                  !
                  ! Monomer B
                  !
                  call data_load(molb)
                  call init_modules()
                  call compdftd3(ixc, energy_b, DFTD3_3BODY, bj_damping, usrpar)
                  call free_modules()
                  call data_free()
                  !
                  ! Dimer AB
                  !
                  call data_load(molab)
                  call init_modules()
                  call compdftd3(ixc, energy_ab, DFTD3_3BODY, bj_damping, usrpar)
                  call free_modules()
                  call data_free()

                  eint = energy_ab - energy_a - energy_b

                  call midrule()
                  call dmsg("EINT: DFT-D3 CONSTITUENT", eint)
                  call dmsg("EINT: DFT-D3 CONSTITUENT [KCAL/MOL]", tokcal(eint))
                  !
                  ! Add record to report file if requested
                  !
                  if (DOREPORT) then
                        if (bj_damping) then
                              cells(1) = '"'//trim(molab%path)//'"'
                              cells(2) = str(usrpar(DFTD3_BJ_IA1))
                              cells(3) = str(usrpar(DFTD3_BJ_IA2))
                              cells(4) = str(usrpar(DFTD3_BJ_IS8))
                              cells(5) = str(tokcal(eint))
                              call rep_update(cells, ncells)
                        else
                              cells(1) = '"'//trim(molab%path)//'"'
                              cells(2) = str(usrpar(DFTD3_IR6))
                              cells(3) = str(usrpar(DFTD3_IS8))
                              cells(4) = str(tokcal(eint))
                              call rep_update(cells, ncells)
                        end if
                  end if
            end if
      end subroutine dftd3_optim_gridpoint


      subroutine task_dftd3_sp(mola)
            type(tmolecule), intent(in) :: mola

            real(F64) :: e_dftd3
            real(F64), dimension(5) :: usrparam
            type(txcdef) :: ixc
            integer, parameter :: ncells = 2
            character(len=DEFLEN), dimension(ncells) :: cells
            logical :: dftd3_usrdef
            logical :: bj_damping

            if (IMG_ISMASTER) then
                  if (DFT_DISP == DISP_DFTD3_BJ) then
                        bj_damping = .true.
                  else if (DFT_DISP == DISP_DFTD3) then
                        bj_damping = .false.
                  else
                        call msg("Undefined type of DFT-D3", MSG_ERROR)
                        stop
                  end if

                  if (bj_damping) then
                        usrparam(DFTD3_BJ_IS6) = 1.0_F64
                        dftd3_usrdef = .false.
                        if (DFTD3_BJ_A1 > ZERO .and. DFTD3_BJ_A2 > ZERO .and. DFTD3_BJ_S8 >= ZERO) then
                              usrparam(DFTD3_BJ_IA1) = DFTD3_BJ_A1
                              usrparam(DFTD3_BJ_IA2) = DFTD3_BJ_A2
                              usrparam(DFTD3_BJ_IS8) = DFTD3_BJ_S8
                              dftd3_usrdef = .true.
                        else if (DFTD3_BJ_A1 > ZERO .or. DFTD3_BJ_A2 > ZERO .or. DFTD3_BJ_S8 >= ZERO) then
                              call msg("One of DFT-D3(BJ) parameters (a1, a2, s8) is undefined", MSG_ERROR)
                              stop
                        end if

                  else
                        usrparam(DFTD3_IS6) = 1.0_F64
                        usrparam(DFTD3_IR8) = 1.0_F64
                        usrparam(DFTD3_IAL) = 14.0_F64
                        dftd3_usrdef = .false.
                        if (DFTD3_R6 > ZERO .and. DFTD3_S8 >= ZERO) then
                              usrparam(DFTD3_IR6) = DFTD3_R6
                              usrparam(DFTD3_IS8) = DFTD3_S8
                              dftd3_usrdef = .true.
                        else if (DFTD3_R6 > ZERO .or. DFTD3_S8 >= ZERO) then
                              call msg("One of DFT-D3 parameters (r6, s8) is undefined", MSG_ERROR)
                              stop
                        end if
                  end if

                  call xcf_define(ixc, XCMODEL, AUX_NONE, ROKS_ENABLED)
                  call data_load(mola)
                  call init_modules()
                  if (dftd3_usrdef) then
                        call compdftd3(ixc, e_dftd3, DFTD3_3BODY, bj_damping, usrparam)
                  else
                        call compdftd3(ixc, e_dftd3, DFTD3_3BODY, bj_damping)
                  end if

                  if (allocated(TAG_DFT_DISP)) then
                        call msg(TAG_DFT_DISP // " " // str(e_dftd3))
                  end if

                  if (DOREPORT) then
                        cells(1) = '"'//trim(mola%path)//'"'
                        cells(2) = str(tokcal(e_dftd3))
                        call rep_update(cells, ncells)
                  end if

                  call free_modules()
                  call data_free()
            end if
      end subroutine task_dftd3_sp


      subroutine task_dftd3_int(mola, molb, molab)
            type(tmolecule), intent(in) :: mola
            type(tmolecule), intent(in) :: molb
            type(tmolecule), intent(in) :: molab

            real(F64) :: ea, eb, eab,  e_disp
            real(F64), dimension(5) :: usrparam
            type(txcdef) :: ixc
            integer, parameter :: ncells = 4
            character(len=DEFLEN), dimension(ncells) :: cells
            logical :: dftd3_usrdef
            logical :: bj_damping

            if (IMG_ISMASTER) then
                  if (DFT_DISP == DISP_DFTD3_BJ) then
                        bj_damping = .true.
                  else if (DFT_DISP == DISP_DFTD3) then
                        bj_damping = .false.
                  else
                        call msg("Undefined type of DFT-D3", MSG_ERROR)
                        stop
                  end if

                  if (bj_damping) then
                        usrparam(DFTD3_BJ_IS6) = 1.0_F64
                        dftd3_usrdef = .false.
                        if (DFTD3_BJ_A1 > ZERO .and. DFTD3_BJ_A2 > ZERO .and. DFTD3_BJ_S8 >= ZERO) then
                              usrparam(DFTD3_BJ_IA1) = DFTD3_BJ_A1
                              usrparam(DFTD3_BJ_IA2) = DFTD3_BJ_A2
                              usrparam(DFTD3_BJ_IS8) = DFTD3_BJ_S8
                              dftd3_usrdef = .true.
                        else if (DFTD3_BJ_A1 > ZERO .or. DFTD3_BJ_A2 > ZERO .or. DFTD3_BJ_S8 >= ZERO) then
                              call msg("One of DFT-D3(BJ) parameters (a1, a2, s8) is undefined", MSG_ERROR)
                              stop
                        end if

                  else
                        usrparam(DFTD3_IS6) = 1.0_F64
                        usrparam(DFTD3_IR8) = 1.0_F64
                        usrparam(DFTD3_IAL) = 14.0_F64
                        dftd3_usrdef = .false.
                        if (DFTD3_R6 > ZERO .and. DFTD3_S8 >= ZERO) then
                              usrparam(DFTD3_IR6) = DFTD3_R6
                              usrparam(DFTD3_IS8) = DFTD3_S8
                              dftd3_usrdef = .true.
                        else if (DFTD3_R6 > ZERO .or. DFTD3_S8 >= ZERO) then
                              call msg("One of DFT-D3 parameters (r6, s8) is undefined", MSG_ERROR)
                              stop
                        end if
                  end if

                  call xcf_define(ixc, XCMODEL, AUX_NONE, ROKS_ENABLED)
                  !
                  ! Monomer A
                  !
                  call data_load(mola)
                  call init_modules()
                  if (dftd3_usrdef) then
                        call compdftd3(ixc, ea, DFTD3_3BODY, bj_damping, usrparam)
                  else
                        call compdftd3(ixc, ea, DFTD3_3BODY, bj_damping)
                  end if
                  call free_modules()
                  call data_free()
                  !
                  ! Monomer B
                  !
                  call data_load(molb)
                  call init_modules()
                  if (dftd3_usrdef) then
                        call compdftd3(ixc, eb, DFTD3_3BODY, bj_damping, usrparam)
                  else
                        call compdftd3(ixc, eb, DFTD3_3BODY, bj_damping)
                  end if
                  call free_modules()
                  call data_free()
                  !
                  ! Dimer AB
                  !
                  call data_load(molab)
                  call init_modules()
                  if (dftd3_usrdef) then
                        call compdftd3(ixc, eab, DFTD3_3BODY, bj_damping, usrparam)
                  else
                        call compdftd3(ixc, eab, DFTD3_3BODY, bj_damping)
                  end if
                  call free_modules()
                  call data_free()

                  e_disp = eab - ea - eb

                  call toprule()
                  call msg("DFT-D3 CALCULATIONS COMPLETED")
                  call midrule()
                  call dmsg("MOLECULE A [KCAL/MOL]", tokcal(ea))
                  call dmsg("MOLECULE B [KCAL/MOL]", tokcal(eb))
                  call dmsg("MOLECULE AB [KCAL/MOL]", tokcal(eab))
                  call dmsg("E_DISP [KCAL/MOL]", tokcal(e_disp))

                  if (DOREPORT) then
                        cells(1) = '"'//trim(molab%path)//'"'
                        cells(2) = str(tokcal(ea))
                        cells(3) = str(tokcal(eb))
                        cells(4) = str(tokcal(e_disp))
                        call rep_update(cells, ncells)
                  end if
            end if
      end subroutine task_dftd3_int


      ! subroutine task_mbd_rsscs_optim()
      !       integer :: i, k
      !       real(F64), parameter :: beta_lbound = 0.005_F64
      !       real(F64), parameter :: beta_ubound = 2.0_F64
      !       real(F64), parameter :: beta_step = 0.005_F64
      !       integer, parameter :: nbeta = nint((beta_ubound - beta_lbound) / beta_step) + 1
      !       real(F64), dimension(nbeta), parameter :: beta = &            
      !             [(beta_lbound + real(i, F64) * beta_step, i = 0, nbeta-1)]

      !       integer, parameter :: ncells = 3
      !       character(len=DEFLEN), dimension(ncells) :: cells
      !       integer :: ndimers
      !       integer :: na, nb
      !       real(F64), dimension(:, :), allocatable :: coords
      !       integer, dimension(:), allocatable :: nuclz
      !       real(F64), dimension(:), allocatable :: vol_a, vol_b, vol_ab
      !       type(tmolecule) :: mol_a, mol_b, mol_ab
      !       real(F64) :: ea, eb, eab, e_disp
      !       type(tsystemdep_params) :: par

      !       if (IMG_ISMASTER) then
      !             if (DOREPORT) then
      !                   cells(1) = '"Dimer"'
      !                   cells(2) = '"Beta"'
      !                   cells(3) = '"Edisp [kcal/mol]"'
      !                   call rep_update(cells, ncells)
      !             end if
      !             ndimers = (NJOB - NHIRSH) / 3
      !             call msg("Optimizing damping function for the MBD-rsSCS dispersion correction")
      !             call imsg("Points per dimer", nbeta)
      !             call imsg("Number of dimers", ndimers)
      !             call imsg("Total number of data points", nbeta * ndimers)
      !             do k = 1, ndimers
      !                   call dequeue_job(mol_a, par, 2 * k - 1, GEOM_MONOMER)
      !                   call dequeue_job(mol_b, par, 2 * k, GEOM_MONOMER)
      !                   call dequeue_job(mol_ab, par, k, GEOM_COMPLEX)
      !                   call unpack_systemdep_params(par)
      !                   call task_mbd_rsscs_dispfree(vol_a, vol_b, vol_ab, na, nb, &
      !                         coords, nuclz, mol_a, mol_b, mol_ab)

      !                   do i = 1, nbeta
      !                         if (na > 1) then
      !                               call edisp_ardt2014(ea, coords(:, 1:na), nuclz(1:na), vol_a, &
      !                                     XCMODEL, beta(i))
      !                         else
      !                               ea = ZERO
      !                         end if
      !                         if (nb > 1) then
      !                               call edisp_ardt2014(eb, coords(:, na+1:na+nb), nuclz(na+1:na+nb), vol_b, &
      !                                     XCMODEL, beta(i))
      !                         else
      !                               eb = ZERO
      !                         end if
      !                         call edisp_ardt2014(eab, coords, nuclz, vol_ab, XCMODEL, beta(i))
      !                         e_disp = eab - ea - eb

      !                         call dmsg("BETA", beta(i))
      !                         call dmsg("Edisp(AB-A-B) [kcal/mol]", tokcal(e_disp))
      !                         call blankline()

      !                         if (DOREPORT) then
      !                               cells(1) = '"'//trim(mol_ab%path)//'"'
      !                               cells(2) = str(beta(i))
      !                               cells(3) = str(tokcal(e_disp))
      !                               call rep_update(cells, ncells)
      !                         end if
      !                   end do
      !             end do
      !       end if
      ! end subroutine task_mbd_rsscs_optim


      ! subroutine task_mbd_rsscs_dispfree(vol_a, vol_b, vol_ab, na, nb, &
      !       coords, nuclz, mola, molb, molab)

      !       real(F64), dimension(:), allocatable, intent(out) :: vol_a
      !       real(F64), dimension(:), allocatable, intent(out) :: vol_b
      !       real(F64), dimension(:), allocatable, intent(out) :: vol_ab
      !       integer, intent(out)                              :: na, nb
      !       real(F64), dimension(:, :), allocatable           :: coords
      !       integer, dimension(:), allocatable                :: nuclz
      !       type(tmolecule), intent(in)                       :: mola
      !       type(tmolecule), intent(in)                       :: molb
      !       type(tmolecule), intent(in)                       :: molab

      !       integer :: a0, a1
      !       integer :: b0, b1
      !       integer :: s
      !       real(F64) :: energy_a, energy_b, energy_ab, eint
      !       type(TSCFOutput) :: scfa, scfb, scfab
      !       type(TAOBasis) :: AOBasis
      !       type(TSCFParams) :: SCFParams
      !       real(F64), dimension(:, :), allocatable :: rho_hirsh
      !       integer, dimension(2, 2) :: RealAtomsA, RealAtomsB

      !       if (IMG_ISMASTER) then
      !             ! ------------------------------------------
      !             !                  Monomer A
      !             ! ------------------------------------------
      !             call data_load(mola)
      !             call init_modules()
      !             RealAtomsA = mola%real_atoms
      !             na = NRealAtoms()
      !             call SCFParams%init(XCMODEL)
      !             if (na > 1) then
      !                   call get_rho_atomic(rho_hirsh)
      !                   call SCFParams%AUXIntegral(AUX_HIRSHFELD_VOLUME, AUX_NONE, rho_hirsh)
      !             else
      !                   call SCFParams%AUXIntegral(AUX_NONE, AUX_NONE)
      !             end if
      !             associate ( &
      !                   AtomCoords => ATOMR, &
      !                   ShellCenters=>SHATOM, &
      !                   ShellParamsIdx => SH, &
      !                   ShellMomentum => SHTYPE, &
      !                   NPrimitives => NPRM, &
      !                   CntrCoeffs => CNTR, &
      !                   Exponents => EXPN, &
      !                   NormFactorsCart => CNTRNORM, &
      !                   SpherAO => SPHERBASIS &
      !                   )
      !                   call basis_NewAOBasis(AOBasis, AtomCoords, ShellCenters, ShellParamsIdx, ShellMomentum, &
      !                         NPrimitives, CntrCoeffs, Exponents, NormFactorsCart, SpherAO)
      !             end  associate
      !             call scf_driver_SpinUnres(scfa, SCFParams, AOBasis)
      !             if (na > 1) then
      !                   call move_alloc(scfa%AUXOut, vol_a)
      !             else
      !                   allocate(vol_a(0))
      !             end if
      !             call free_modules()
      !             call data_free()
      !             ! ------------------------------------------
      !             !                  Monomer B
      !             ! ------------------------------------------
      !             call data_load(molb)
      !             call init_modules()
      !             RealAtomsB = molb%real_atoms
      !             nb = NRealAtoms()
      !             if (nb > 1) then
      !                   call get_rho_atomic(rho_hirsh)
      !                   call SCFParams%AUXIntegral(AUX_HIRSHFELD_VOLUME, AUX_NONE, rho_hirsh)
      !             else
      !                   call SCFParams%AUXIntegral(AUX_NONE, AUX_NONE)
      !             end if
      !             call scf_driver_SpinUnres(scfb, SCFParams, AOBasis)
      !             if (nb > 1) then
      !                   call move_alloc(scfb%AUXOut, vol_b)
      !             else
      !                   allocate(vol_b(0))
      !             end if
      !             call free_modules()
      !             call data_free()
      !             ! -------------------------------------------
      !             !                 Dimer AB
      !             ! -------------------------------------------
      !             call data_load(molab)
      !             call init_modules()
      !             call get_rho_atomic(rho_hirsh)
      !             call SCFParams%AUXIntegral(AUX_HIRSHFELD_VOLUME, AUX_NONE, rho_hirsh)
      !             call scf_driver_SpinUnres(scfab, SCFParams, AOBasis)
      !             call move_alloc(scfab%AUXOut, vol_ab)
      !             allocate(coords(3, na+nb))
      !             allocate(nuclz(na+nb))
      !             na = 0
      !             do s = 1, 2
      !                   a0 = RealAtomsA(1, s)
      !                   a1 = RealAtomsA(2, s)
      !                   coords(:, na+1:na+a1-a0+1) = ATOMR(:, a0:a1)
      !                   nuclz(na+1:na+a1-a0+1) = INUCLZ(a0:a1)
      !                   na = na + a1 - a0 + 1
      !             end do
      !             nb = 0
      !             do s = 1, 2
      !                   b0 = RealAtomsB(1, s)
      !                   b1 = RealAtomsB(2, s)
      !                   coords(:, na+nb+1:na+nb+b1-b0+1) = ATOMR(:, b0:b1)
      !                   nuclz(na+nb+1:na+nb+b1-b0+1) = INUCLZ(b0:b1)
      !                   nb = nb + b1 - b0 + 1
      !             end do
      !             call free_modules()
      !             call data_free()
      !             energy_a = scfa%EtotDFT
      !             energy_b = scfb%EtotDFT
      !             energy_ab = scfab%EtotDFT
      !             eint = energy_ab - energy_a - energy_b
      !             call toprule()
      !             call msg("Dispfersion-free BSSE-corrected energies")
      !             call midrule()
      !             call dmsg("Molecule A", energy_a)
      !             call dmsg("Molecule B", energy_b)
      !             call dmsg("Molecule AB", energy_ab)
      !             call dmsg("Eint (DFT) [a.u.]", eint)
      !             call dmsg("Eint (DFT) [kcal/mol]", tokcal(eint))
      !             call blankline()
      !       else
      !             !
      !             ! Monomer A
      !             !
      !             call data_load(mola)
      !             call init_modules()
      !             call ksgen_slave()
      !             call free_modules()
      !             call data_free()
      !             !
      !             ! Monomer B
      !             !
      !             call data_load(molb)
      !             call init_modules()
      !             call ksgen_slave()
      !             call free_modules()
      !             call data_free()
      !             !
      !             ! Dimer AB
      !             !
      !             call data_load(molab)
      !             call init_modules()
      !             call ksgen_slave()
      !             call data_free()
      !             call free_modules()
      !       end if
      ! end subroutine task_mbd_rsscs_dispfree
end module drv_dft_disp
