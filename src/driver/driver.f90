program driver
      use arithmetic
      use math_constants
      use gparam
      use io
      use string
      use images
      use initialize
      use display
      use periodic
      use parser
      use report
      use gridfunc
      use hirshfeld
      use sys_definitions
      use scf_definitions
      use rpa_definitions
      use thc_definitions
      use TwoStepCholesky_definitions
      use drv_dft
      use drv_dft_rpa
      use drv_dft_disp
      use drv_mp2
      !@CC
      use drv_cc_prop
      use drv_cc_ground
      !@END CC
      
      implicit none

      character(len=:), allocatable :: cmd
      type(TSystem) :: System
      type(TSCFParams) :: SCFParams
      type(TRPAParams) :: RPAParams
      type(TChol2Params) :: Chol2Params
      type(TTHCParams) :: THCParams
      type(tmolecule) :: geom_a, geom_b, geom_ab
      type(tsystemdep_params) :: par
      integer, parameter :: max_ncells = 10
      character(len=DEFLEN), dimension(max_ncells) :: cells
      type(TSCFOutput) :: scfresults
      integer :: k
      integer :: n_neutral, n_cation, n_anion, ndimers
      integer :: njob_main
      !
      ! Init process-level parallelization
      !
      call img_setup()
      !
      ! Determine root directory
      !
      cmd = io_argv(0)
      call read_rootdir(cmd)
      !
      ! Absolute path to the input file
      ! ---
      ! Input file name should be provided as first argument
      ! after the executable name
      !
      INPUTFILE = io_argv(1)
      if (isblank(INPUTFILE)) then
            call msg("Input file has not been specified", MSG_ERROR)
            stop
      end if
      if (.not. io_exists(INPUTFILE)) then
            call msg("Input file is not accessible", MSG_ERROR)
            stop
      end if
      DIRSEP = io_argv(3)
      WORKDIR = dirname(INPUTFILE) // DIRSEP
      JOBTITLE = io_argv(4)
      call io_set_scratchdir(io_argv(2))
      call read_inputfile(System, SCFParams, RPAParams, Chol2Params, THCParams, INPUTFILE)
      call sys_Init(System, SYS_TOTAL)
      ! ---------------------------------------------------------
      ! Compute spherically-averaged densities of isolated atoms
      ! if the Hirshfeld population analysis is requested
      ! ---------------------------------------------------------
      if (SCFParams%Hirsh .and. System%NAtoms>1) then
            call task_dft_IsolatedHirshfeldAtoms_UKS(SCFParams, System)
      else
            allocate(SCFParams%AUXIn(0, 0))
            allocate(SCFParams%HirshVolumes(0))
      end if
      njob_main = NJOB

      select case(jobtype)
      case (JOB_VIS_RHO_DIFF)
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_visualize(geom_a)
            end do
      case (JOB_DFT_SP)
            if (DOREPORT .and. IMG_ISMASTER) then
                  write(cells(1), *) '"molecule"'
                  write(cells(2), *) '"charge"'
                  write(cells(3), *) '"total energy [a.u.]"'
                  write(cells(4), *) '"dispersion energy [kcal/mol]"'
                  write(cells(5), *) '"omega [1/bohr]"'
                  write(cells(6), *) '"Ehomo [eV]"'
                  write(cells(7), *) '"Elumo [eV]"'
                  write(cells(8), *) '"auxiliary integral [a.u.]"'
                  call rep_update(cells, 8)
            end if
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_dft_sp(scfresults, geom_a)
            end do

      case (JOB_REAL_UKS_RPA)
            call task_uks_rpa(System, SCFParams, RPAParams, Chol2Params, THCParams)
            
      case (JOB_REAL_UKS_SP)
            call task_dft_UKS(System, SCFParams)
            
      case (JOB_RTTDDFT_POLAR)
            if (DOREPORT .and. IMG_ISMASTER) then
                  write(cells(1), *) '"MOLECULE"'
                  write(cells(2), *) '"CHARGE"'
                  write(cells(3), *) '"E TOTAL [A.U.]"'
                  write(cells(4), *) '"OMEGA [1/BOHR]"'
                  write(cells(5), *) '"EHOMO [eV]"'
                  write(cells(6), *) '"ELUMO [eV]"'
                  write(cells(7), *) '"AUXINT [A.U.]"'
                  call rep_update(cells, 7)
            end if
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_rttddft_polar(scfresults, geom_a)
            end do

      case (JOB_REAL_UKS_INT)
            call task_dft_UKS(System, SCFParams)
            
      case (JOB_DFT_INT)
            if (DOREPORT .and. IMG_ISMASTER) then
                  if (NONSCF_XCMODEL == XCF_XC_RPA) then
                        write(cells(1), *) '"Dimer"'
                        write(cells(2), *) '"EtotDFT_Int (kcal/mol)"'
                        write(cells(3), *) '"EtotHF_Int (kcal/mol)"'
                        write(cells(4), *) '"EcSinglesRPA_Int (kcal/mol)"'
                        write(cells(5), *) '"EcRPA_Int (kcal/mol)"'
                        write(cells(6), *) '"EtotRPA_Int (kcal/mol)"'
                        call rep_update(cells, 6)
                  else
                        write(cells(1), *) '"Dimer"'
                        write(cells(2), *) '"EdispDFT_Int (kcal/mol)"'
                        write(cells(3), *) '"EtotDFT_Int (kcal/mol)"'
                        call rep_update(cells, 3)
                  end if
            end if
            ndimers = njob_main / 3
            do k = 1, ndimers
                  call dequeue_job(geom_a, par, 2 * k - 1, GEOM_MONOMER)
                  call dequeue_job(geom_b, par, 2 * k, GEOM_MONOMER)
                  call dequeue_job(geom_ab, par, k, GEOM_COMPLEX)
                  call unpack_systemdep_params(par)
                  ! if (JOBTYPE == JOB_REAL_UKS_INT) then
                  !       call task_dft_int_UKS(geom_a, geom_b, geom_ab, SCFParams)
                  ! else
                  call task_dft_int_ROKS(geom_a, geom_b, geom_ab)
                  ! end if
            end do

      case (JOB_DFT_OPTOMEGA)
            if (DOREPORT .and. IMG_ISMASTER) then
                  write(cells(1), *) '"MOLECULE"'
                  write(cells(2), *) '"J^2 [eV^2]"'
                  write(cells(3), *) '"OMEGA [1/BOHR]"'
                  write(cells(4), *) '"UNCERTAINTY [1/BOHR]"'
                  write(cells(5), *) '"CATION"'
                  write(cells(6), *) '"ANION"'
                  write(cells(7), *) '"CONVERGED"'
                  call rep_update(cells, 7)
            end if

            n_cation = 0
            n_neutral = 0
            n_anion = 0
            do while ((n_cation + n_neutral + n_anion) < njob_main)
                  call dequeue_job(geom_b, par, n_neutral+1, GEOM_NEUTRAL)
                  call unpack_systemdep_params(par)            
                  n_neutral = n_neutral + 1
                  select case (OPTOMEGA_J2TYPE)
                        case (OPTOMEGA_J2_CN)
                              call dequeue_job(geom_a, par, n_cation+1, GEOM_CATION)                              
                              call task_dft_optomega(geom_a, geom_b, geom_b)
                              n_cation = n_cation + 1
                        case (OPTOMEGA_J2_NA)
                              call dequeue_job(geom_ab, par, n_anion+1, GEOM_ANION)                              
                              call task_dft_optomega(geom_b, geom_b, geom_ab)
                              n_anion = n_anion + 1
                        case (OPTOMEGA_J2_CNA)
                              call dequeue_job(geom_a, par, n_cation+1, GEOM_CATION)                              
                              call dequeue_job(geom_ab, par, n_anion+1, GEOM_ANION)                              
                              call task_dft_optomega(geom_a, geom_b, geom_ab)
                              n_cation = n_cation + 1
                              n_anion = n_anion + 1
                  end select
            end do
      case (JOB_DFT_DISP_OPTIM)
            select case (DFT_DISP)
            case (DISP_DFTD3)
                  call task_dftd3_optim()
            ! case (DISP_MBD_RSSCS)
            !       call task_mbd_rsscs_optim()
            end select
      case (JOB_DFTD3_SP)
            if (DOREPORT .and. IMG_ISMASTER) then
                  write(cells(1), *) '"MOLECULE"'
                  write(cells(2), *) '"E(DFT-D3) [KCAL/MOL]"'
                  call rep_update(cells, 2)
            end if
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_dftd3_sp(geom_a)
            end do
      case (JOB_DFTD3_INT)
            if (DOREPORT .and. IMG_ISMASTER) then
                  write(cells(1), *) '"MOLECULE"'
                  write(cells(2), *) '"E(A) [KCAL/MOL]"'
                  write(cells(3), *) '"E(B) [KCAL/MOL]"'
                  write(cells(4), *) '"EDISP [KCAL/MOL]"'
                  call rep_update(cells, 4)
            end if

            ndimers = njob_main / 3
            do k = 1, ndimers
                  call dequeue_job(geom_a, par, 2 * k - 1, GEOM_MONOMER)
                  call dequeue_job(geom_b, par, 2 * k, GEOM_MONOMER)
                  call dequeue_job(geom_ab, par, k, GEOM_COMPLEX)
                  call unpack_systemdep_params(par)
                  call task_dftd3_int(geom_a, geom_b, geom_ab)
            end do
      case (JOB_MP2_SP)
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_mp2_sp(geom_a)
            end do
            !@CC
      case (JOB_CCSD_PROP)
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_cc_properties(geom_a, THEORY_CCSD)
            end do
      case (JOB_CC3_PROP)
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_cc_properties(geom_a, THEORY_CC3)
            end do
      case (JOB_CCSD_DENSITY)
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_cc_density(geom_a, THEORY_CCSD, MBPT_ORDER)
            end do
      case (JOB_CC3_DENSITY)
            do k = 1, njob_main
                  call dequeue_job(geom_a, par, k, GEOM_MONOMER)
                  call unpack_systemdep_params(par)
                  call task_cc_density(geom_a, THEORY_CC3, MBPT_ORDER)
            end do
            !@END CC
      end select
      
      stop 0
      
contains

      subroutine task_visualize(molecule)
            type(tmolecule), intent(in) :: molecule
            
            real(F64), dimension(:, :), allocatable :: rhoa
            real(F64), dimension(:, :), allocatable :: rhob
            real(F64), dimension(:), allocatable :: rho_tile
            type(tvoldata) :: vd
            integer, dimension(0) :: empty
            real(F64), dimension(0, 0) :: empty2

            if (IMG_ISMASTER) then
                  call data_load(molecule)
                  call init_modules()
                  
                  if (JOBTYPE == JOB_VIS_RHO_DIFF) then
                        if (.not. allocated(VIS_RHOA_PATH)) then
                              call msg("UNDEFINED VARIABLE: VIS_RHOA_PATH", MSG_ERROR)
                              stop
                        end if

                        if (.not. allocated(VIS_RHOB_PATH)) then
                              call msg("UNDEFINED VARIABLE: VIS_RHOB_PATH", MSG_ERROR)
                              stop
                        end if

                        if (.not. allocated(VIS_CUBEFILE_PATH)) then
                              call msg("UNDEFINED VARIABLE: VIS_CUBEFILE_PATH", MSG_ERROR)
                              stop
                        end if

                        allocate(rhoa(NORB, NORB))
                        allocate(rhob(NORB, NORB))

                        select case (VIS_RHOA_MODE)
                        case (FILEMODE_TEXT)
                              call io_text_read(rhoa, VIS_RHOA_PATH)
                        case (FILEMODE_BINARY)
                              call io_binary_read(rhoa, VIS_RHOA_PATH)
                        end select

                        select case (VIS_RHOB_MODE)
                        case (FILEMODE_TEXT)
                              call io_text_read(rhob, VIS_RHOB_PATH)
                        case (FILEMODE_BINARY)
                              call io_binary_read(rhob, VIS_RHOB_PATH)
                        end select
                        
                        call msg("DIFFERENCE OF TWO DENSITY MATRICES (RHOA - RHOB)")
                        call msg("RHOA READ FROM")
                        call msg(VIS_RHOA_PATH)
                        call msg("RHOB READ FROM")
                        call msg(VIS_RHOB_PATH)
                        
                        rhoa = rhoa - rhob
                        deallocate(rhob)                        
                        allocate(rho_tile(NORB * NORB))
                        call matrix2tile(rho_tile, rhoa)
                        
                        call vd%init(VOL_FUNC_RHO, .false., .true., &
                              VIS_CUBEFILE_SPACING, empty, empty, data_fmt="(6E18.10)")
                        call vol_write(INUCLZ, ATOMR, rho_tile, rho_tile, &
                              empty2, empty2, vd, root_file_path=VIS_CUBEFILE_PATH)

                        call msg("RHOA - RHOB HAS BEEN WRITTEN INTO A CUBE FILE")
                        call msg(VIS_CUBEFILE_PATH)
                        
                        deallocate(rhoa)
                        deallocate(rho_tile)
                        call free_modules()
                        call data_free()
                  end if
            end if
      end subroutine task_visualize
end program driver
