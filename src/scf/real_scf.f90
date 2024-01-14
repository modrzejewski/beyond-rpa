module real_scf
      use arithmetic
      use math_constants
      use io
      use gparam
      use h_xcfunc
      use scf_definitions
      use guess
      use real_linalg
      use uks_arh
      use mbd
      use fbuild
      use fock2el
      use scf
      use ecpint
      use spin_orbit_ecp
      use grid
      use scf_definitions
      use basis_sets
      use Fock
      use KohnSham
      use Slater
      use SCFMatrices
      use CholeskyCoulomb
      use CholeskyExchange
      use CholeskyFock
      use THCFock
      use thc_definitions
      use TwoStepCholesky_definitions
      use OneElectronInts
      use Pseudopotential, only : pp_V
      !$ use omp_lib
      
      implicit none

      integer, parameter :: SCFRuleWidth = 78
      
contains
      
      
      subroutine scf_SaveDensityFile(RhoAO, Mode, PathA, PathB)
            real(F64), dimension(:, :, :), intent(in) :: RhoAO
            integer, intent(in) :: Mode
            character(*), intent(in) :: PathA
            character(*), intent(in) :: PathB

            character(:), allocatable :: Path
            integer :: NSpin, s
            
            if (Mode .ne. FILEMODE_NONE) then
                  call midrule()
                  NSpin = size(RhoAO, dim=3)
                  do s = 1, NSpin
                        if (s == 1) then
                              Path = PathA
                        else
                              Path = PathB
                        end if
                        select case (Mode)
                        case (FILEMODE_TEXT)
                              call io_text_write(RhoAO(:, :, s), Path)
                        case (FILEMODE_BINARY)
                              call io_binary_write(RhoAO(:, :, s), Path)
                        end select
                        if (NSpin == 1) then
                              call msg("Saved total alpha+beta RhoAO:")
                        else
                              call msg("Saved spin component of RhoAO:")
                        end if
                        call msg(Path)
                  end do
                  call midrule()
            end if
      end subroutine scf_SaveDensityFile

      
      subroutine scf_AuxIntegrals(AUXOut, Rho_cao, SCFParams, System)
            real(F64), dimension(:), intent(inout)    :: AUXOut
            real(F64), dimension(:, :, :), intent(in) :: Rho_cao
            type(TSCFParams), intent(in)              :: SCFParams
            type(TSystem), intent(in)                 :: System

            integer :: i
            logical :: SpinUnres

            if (size(Rho_cao, dim=3) > 1) then
                  SpinUnres = .true.
            else
                  SpinUnres = .false.
            end if
            if (SCFParams%AUXInt_Type1 /= AUX_NONE) then
                  select case (SCFParams%AUXInt_Type1)
                  case (AUX_HIRSHFELD_POPULATION)
                        call hirshfeld_population_display(AUXOut, System)
                  case (AUX_HIRSHFELD_VOLUME)
                        call hirshfeld_volume_ratios(AUXOut, SCFParams%HirshVolumes, System)
                        call hirshfeld_volume_display(AUXOut, System)
                  case (AUX_HIRSHFELD_VOLUME_FREE)
                        call hirshfeld_volume_free_display(AUXOut)
                  case (AUX_GDD_GRAC)
                        call aux_gdd_grac_finalize(AUXOut, SpinUnres)
                  case (AUX_GDD_OMEGA)
                        call aux_gdd_finalize(AUXOut, GDD_NOUT_ALPHA, GDD_MUMIN)
                  case (AUX_XHOLE_DIST_FUKUI)
                        call aux_xhole_dist_fukui_finalize(AUXOut, SpinUnres)
                  case (AUX_REGULARIZED_VNUCL)
                        call msg("Computed matrix of regularized electrons-nuclei potential")
                  case (AUX_MODRZEJ2012_C_HOLE)
                        call modrzej2012_c_hole_finalize(AUXOut, SpinUnres)
                  case (AUX_BR_X_HOLE, AUX_PBE_X_HOLE, AUX_TPSS_X_HOLE)
                        call modrzej2016_x_hole_finalize(AUXOut, SpinUnres)
                  case (AUX_MCSv1_VC_LAMBDA, AUX_MCSv2_VC_LAMBDA)
                        call modrzej2016_vc_curve_display(AUXOut)
                  case (AUX_DENSITY_COMPARE)
                        call density_compare_display(AUXOut)
                  case default
                        call msg("Auxiliary numerical integrals", underline=.true.)
                        do i = 1, size(AUXOut)
                              call dmsg(str(i), AUXOut(i), fmt="F20.15")
                        end do
                  end select
            end if

            if (SCFParams%AUXInt_Type2 /= AUX_NONE) then
                  select case (SCFParams%AUXInt_Type2)
                  case (AUX_RHO_DIFF)
                        call CompareRhoInteraction(Rho_cao)
                  case (AUX_RHO_SPHER)
                        block
                              integer, parameter :: SpherGrid = 17
                              real(F64) :: x0, y0, z0
                              integer :: p0, p1
                              x0 = System%AtomCoords(1, 1)
                              y0 = System%AtomCoords(2, 1)
                              z0 = System%AtomCoords(3, 1)
                              call lebget(p0, SpherGrid)
                              p1 = p0 + LEBNPT(SpherGrid) - 1
                              call CompareRhoSpherical(Rho_cao, x0, y0, z0, GRD_LEBX(p0:p1), &
                                    GRD_LEBY(p0:p1), GRD_LEBZ(p0:p1), GRD_LEBW(p0:p1))
                        end block
                  end select
            end if
      end subroutine scf_AuxIntegrals

      
      subroutine scf_DispersionCorrection(Edisp, xcmodel, AUXOut, System)
            real(F64), intent(out)              :: Edisp
            type(txcdef), intent(in)            :: xcmodel
            real(F64), dimension(:), intent(in) :: AUXOut
            type(TSystem), intent(in)           :: System
            !
            ! User-defined parameters for DFT-D3 dispersion correction
            !
            real(F64), dimension(5) :: dftd3_usrparam
            real(F64), dimension(:, :), allocatable :: RealAtomCoords
            integer, dimension(:), allocatable :: RealAtomZ
            logical :: dftd3_usrdef
            integer :: s, n, a0, a1
            integer :: NRealAtoms, ThisImage, NImages

            ThisImage = this_image()
            NImages = num_images()
            NRealAtoms = 0
            do s = 1, 2
                  a0 = System%RealAtoms(1, s)
                  a1 = System%RealAtoms(2, s)                                    
                  NRealAtoms = NRealAtoms + a1 - a0 + 1
            end do
            Edisp = ZERO
            if (JOBTYPE /= JOB_DFT_DISP_OPTIM) then
                  if (ThisImage == 1) then
                        select case (DFT_DISP)
                        case (DISP_DFTD3)
                              ! --------------------------------------------------------------------
                              ! DFT-D3, damping function vanishes at Rab=0
                              ! J. Chem. Phys. 132, 154104 (2010); doi: 10.1063/1.3382344
                              ! --------------------------------------------------------------------
                              if (NRealAtoms > 1) then
                                    dftd3_usrparam(DFTD3_IS6) = 1.0_F64
                                    dftd3_usrparam(DFTD3_IR8) = 1.0_F64
                                    dftd3_usrparam(DFTD3_IAL) = 14.0_F64
                                    dftd3_usrdef = .false.
                                    if (DFTD3_R6 > ZERO .and. DFTD3_S8 >= ZERO) then
                                          dftd3_usrparam(DFTD3_IR6) = DFTD3_R6
                                          dftd3_usrparam(DFTD3_IS8) = DFTD3_S8
                                          dftd3_usrdef = .true.
                                    else if (DFTD3_R6 > ZERO .or. DFTD3_S8 >= ZERO) then
                                          call msg("One of DFT-D3 parameters (r6, s8) is undefined", MSG_ERROR)
                                          stop
                                    end if
                                    if (dftd3_usrdef) then
                                          call compdftd3(xcmodel, Edisp, DFTD3_3BODY, .false., dftd3_usrparam)
                                    else
                                          call compdftd3(xcmodel, Edisp, DFTD3_3BODY, .false.)
                                    end if
                              end if
                        case (DISP_DFTD3_BJ)
                              ! ---------------------------------------------------------------------
                              ! DFT-D3, rational Becke-Johnson damping function
                              ! Grimme, S., Ehrlich, S., Goerigk, L. J. Comp. Chem. 32, 1456 (2011);
                              ! doi: 10.1002/jcc.21759
                              ! ---------------------------------------------------------------------
                              dftd3_usrparam(DFTD3_BJ_IS6) = 1.0_F64
                              dftd3_usrdef = .false.
                              if (DFTD3_BJ_A1 > ZERO .and. DFTD3_BJ_A2 > ZERO .and. DFTD3_BJ_S8 >= ZERO) then
                                    dftd3_usrparam(DFTD3_BJ_IA1) = DFTD3_BJ_A1
                                    dftd3_usrparam(DFTD3_BJ_IA2) = DFTD3_BJ_A2
                                    dftd3_usrparam(DFTD3_BJ_IS8) = DFTD3_BJ_S8
                                    dftd3_usrdef = .true.
                              else if (DFTD3_BJ_A1 > ZERO .or. DFTD3_BJ_A2 > ZERO .or. DFTD3_BJ_S8 >= ZERO) then
                                    call msg("One of DFT-D3(BJ) parameters (a1, a2, s8) is undefined", MSG_ERROR)
                                    stop
                              end if
                              
                              if (dftd3_usrdef) then
                                    call compdftd3(xcmodel, Edisp, DFTD3_3BODY, .true., dftd3_usrparam)
                              else
                                    call compdftd3(xcmodel, Edisp, DFTD3_3BODY, .true.)
                              end if
                        case (DISP_MBD_RSSCS)
                              ! --------------------------------------------------------------------
                              ! MBD-rsSCS, J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
                              ! --------------------------------------------------------------------                      
                              allocate(RealAtomCoords(3, NRealAtoms))
                              allocate(RealAtomZ(NRealAtoms))
                              n = 0
                              do s = 1, 2
                                    a0 = System%RealAtoms(1, s)
                                    a1 = System%RealAtoms(2, s)
                                    RealAtomCoords(:, n+1:n+a1-a0+1) = System%AtomCoords(:, a0:a1)
                                    RealAtomZ(n+1:n+a1-a0+1) = System%ZNumbers(a0:a1)
                                    n = n + a1 - a0 + 1
                              end do
                              call edisp_ardt2014(Edisp, RealAtomCoords, RealAtomZ, &
                                    AUXOut, xcf_get_id(xcmodel), MBD_RSSCS_BETA)
                        end select
                  end if
                  if (NImages > 1) then
                        call co_broadcast(Edisp, source_image=1)
                  end if
            end if
      end subroutine scf_DispersionCorrection
            

      subroutine scf_RhoStart(Rho_cao, GuessType, PathA, PathB)
            real(F64), dimension(:, :, :), intent(out) :: Rho_cao
            integer, intent(in) :: GuessType
            character(*), intent(in) :: PathA
            character(*), intent(in) :: PathB

            integer :: NAOCart, NSpin
            integer :: ThisImage, NImages

            ThisImage = this_image()
            NImages = num_images()
            NAOCart = size(Rho_cao, dim=1)
            NSpin = size(Rho_cao, dim=3)            
            if (ThisImage == 1) then
                  select case (GuessType)
                  case (SCF_GUESS_HBARE)
                        call msg("SCF guess: bare nuclei Hamiltonian")
                        Rho_cao = ZERO
                  case (SCF_GUESS_ATOMIC)
                        call msg("SCF guess: superposition of atomic densities")
                        call guess_atomic(Rho_cao(:, :, 1))
                        if (NSpin == 2) then
                              Rho_cao(:, :, 1) = (ONE/TWO) * Rho_cao(:, :, 1)
                              Rho_cao(:, :, 2) = Rho_cao(:, :, 1)
                        end if
                  case (SCF_GUESS_TEXT_FILE)
                        if (NSpin == 2) then
                              call msg("SCF guess: reading RhoA from " // PathA)
                              call msg("SCF guess: reading RhoB from " // PathB)
                              call io_text_read(Rho_cao(:, :, 1), PathA)
                              call io_text_read(Rho_cao(:, :, 2), PathB)
                        else
                              call msg("SCF guess: reading RhoAB from " // PathA)
                              call io_text_read(Rho_cao(:, :, 1), PathA)
                        end if
                  case (SCF_GUESS_BINARY_FILE)
                        if (NSpin == 2) then
                              call msg("SCF guess: reading RhoA from " // PathA)
                              call msg("SCF guess: reading RhoB from " // PathB)
                              call io_binary_read(Rho_cao(:, :, 1), PathA)
                              call io_binary_read(Rho_cao(:, :, 2), PathB)
                        else
                              call msg("SCF guess: reading RhoAB from " // PathA)
                              call io_binary_read(Rho_cao(:, :, 1), PathA)
                        end if
                  case default
                        call msg("Unsupported type of SCF guess", MSG_ERROR)
                        stop
                  end select
            end if
            call co_broadcast(Rho_cao, source_image=1)
      end subroutine scf_RhoStart
      

      subroutine scf_TableHeader()
            character(:), allocatable :: line, f1, f2, f3, f4, f5, f6, f7, f8
            
            f1 = lfield("#", 5)
            f2 = cfield("Energy", 21)
            f3 = lfield("EDiff", 9) 
            f4 = lfield("RhoDiff", 9)
            f5 = lfield("OrbGrad", 9)
            f6 = lfield("OrbShift", 9)
            f7 = lfield("NStored", 8)
            f8 = lfield("Time", 9)
            line = f1 // f2 // f3 // f4 // f5 // f6 // f7 // f8
            call midrule(width=SCFRuleWidth)
            call msg(line)
            call midrule(width=SCFRuleWidth)
      end subroutine scf_TableHeader


      subroutine scf_TableRow(iter, Etot, EDiff, RhoDiff, OrbGrad, OrbShift, NStored, TimeIter, MicroIter)
            integer, intent(in)   :: iter
            real(F64), intent(in) :: Etot
            real(F64), intent(in) :: EDiff
            real(F64), intent(in) :: RhoDiff
            real(F64), intent(in) :: OrbGrad
            real(F64), intent(in) :: OrbShift
            integer, intent(in)   :: NStored
            real(F64), intent(in) :: TimeIter
            logical, intent(in)   :: MicroIter
            
            character(:), allocatable :: line, f1, f7, f8
            character(9) :: f3, f4, f5, f6
            character(21) :: f2

            if (MicroIter) then
                  f1 = lfield(str(iter)//"*", 5)
            else
                  f1 = lfield(str(iter), 5)
            end if
            write(f2, fmt="(F20.10,1X)") Etot
            if (iter == 0) then
                  f3 = ""
                  f4 = ""
                  f5 = ""
                  f6 = ""
            else if (iter == 1) then
                  !
                  ! Don't show the energy difference between iterations 0 and 1.
                  ! The energy in the 0th iteration might have been unphysically
                  ! low because of the guess rho violating the idempotency
                  ! and/or trace conditions.
                  !
                  f3 = ""
                  write(f4, fmt="(ES8.1,1X)") RhoDiff
                  f5 = ""
                  f6 = ""
            else
                  write(f3, fmt="(ES8.1,1X)") EDiff
                  write(f4, fmt="(ES8.1,1X)") RhoDiff
                  write(f5, fmt="(ES8.1,1X)") OrbGrad
                  write(f6, fmt="(ES8.1,1X)") OrbShift
            end if
            f7 = cfield(str(NStored), 8)
            f8 = lfield(str(TimeIter, 1), 9)
            line = f1 // f2 // f3 // f4 // f5 // f6 // f7 // f8
            call msg(line)
      end subroutine scf_TableRow


      subroutine scf_XCInfo(XCModel)
            type(TXCDef), intent(in) :: XCModel
            
            character(:), allocatable :: x, c
            character(:), allocatable :: MethodClass
            logical :: rs_hybrid
            logical :: global_hybrid
            logical :: screened_hybrid
            logical :: metaGGA

            call midrule()
            if (xcf_get_id(XCModel) .ne. XCF_HF) then
                  rs_hybrid = xcf_get_flag(XCModel, XCF_RSHYB)
                  screened_hybrid = xcf_get_flag(XCModel, XCF_SCREENED_HYBRID)
                  global_hybrid = (.not. rs_hybrid) .and. (abs(xcf_get_exx(XCModel)) > ZERO)
                  metaGGA = xcf_get_flag(XCModel, XCF_XC_MGGA)

                  if (rs_hybrid) then
                        if (screened_hybrid) then
                              MethodClass = "Screened hybrid "
                        else
                              MethodClass = "Long-range corrected hybrid "
                        end if
                  elseif (global_hybrid) then
                        MethodClass = "Global hybrid "
                  else
                        MethodClass = "Pure semilocal "
                  end if

                  if (metaGGA) then
                        MethodClass = MethodClass // "meta-generalized gradient approximation"
                  else
                        MethodClass = MethodClass // "generalized gradient approximation"
                  end if

                  call msg("Method class: " // MethodClass)
                  if (rs_hybrid) then
                        call msg("Omega: " // str(xcf_get_omega(XCModel), 4))
                        call msg("Short-range HF exchange fraction: " // str(xcf_get_srexx(XCModel), 4))
                        if (screened_hybrid) then
                              call msg("Long-range exchange at a fully semilocal level")
                        end if
                  end if

                  if (global_hybrid) then
                        call msg("HF exchange fraction: " // str(xcf_get_exx(XCModel), 4))
                  end if

                  call xcf_xcstring(XCModel, x, c)
                  if (x == c) then
                        call msg("Exchange-correlation: " // x)
                  else
                        call msg("Exchange: " // x)
                        call msg("Correlation: " // c)
                  end if
            else
                  MethodClass = "Hartree-Fock"
                  call msg("Exchange-correlation: " // MethodClass)
            end if
            if (XCModel%AsympVxc == AC_LFAS &
                  .or. XCModel%AsympVxc == AC_LFAS_v2 &
                  .or. XCModel%AsympVxc == AC_LFAS_FREE &
                  .or. XCModel%AsympVxc == AC_LFAS_v2_FREE) then
                  call msg("LFAs asymptotic correction of Chai et al.:")
                  call msg("Phys. Rev. A 87, 052510 (2013); doi: 10.1103/PhysRevA.87.052510")
                  call msg("AsympVxcOmega: " // str(XCModel%AsympVxcOmega, 4))
                  call msg("Scaling factor: " // str(ONE-XCModel%EXX, 4))
                  call msg("VxAC isn't a functional derivative of the energy")
                  if (XCModel%AsympVxc == AC_LFAS .or. XCModel%AsympVxc == AC_LFAS_FREE) then
                        call msg("Will compute energy contrib as 1/2 Int Rho(r) VxAC(r) dr")
                  else
                        call msg("Will compute energy contrib as Int Rho(r) VxAC(r) dr")
                        call msg("Prefactor 1/2 removed to enable direct energy minimization")
                        call msg("Exc won't be correct unless recomputed post-SCF")
                  end if
            end if
            if (XCModel%SlaterVxc) then
                  call msg("Exact exchange potential: using multiplicative potential VxSlater")
                  call msg("VxSlater employs resolution of the identity")
                  call msg("J. Chem. Phys. 115, 5718 (2001); doi: 10.1063/1.1398093")
            end if
      end subroutine scf_XCInfo

      
      subroutine scf_CPUInfo()
            integer :: NThreads, NImages
            
            !$omp parallel default(shared)
            !$omp master
            NThreads = 1
            !$ NThreads = omp_get_num_threads()
            !$omp end master
            !$omp end parallel
            NImages = num_images()
            call msg("Replicated-data parallel SCF", underline=.true.)
            call imsg("Processes", NImages)
            call imsg("Threads/process", NThreads)
      end subroutine scf_CPUInfo


      subroutine scf_GridDiagnostic(diag, lmgga)
            type(tgriddiag), intent(in) :: diag
            logical, intent(in)         :: lmgga

            call msg("Grid quality indicators evaluated at RhoConv")
            call dmsg("Rho", diag%nel, fmt="F10.6")
            call dmsg("Div(Rho)", diag%div, fmt="ES10.3")
            if (lmgga) then
                  call dmsg("Lapl(Rho)", diag%lap, fmt="ES10.3")
            end if
            call midrule()
      end subroutine scf_GridDiagnostic


      subroutine scf_BufferDim(DimTxc, DimJK, DimRho1D, NThreads, AOBasis)
            integer, intent(out)       :: DimTxc
            integer, intent(out)       :: DimJK
            integer, intent(out)       :: DimRho1D
            integer, intent(out)       :: NThreads
            type(TAOBasis), intent(in) :: AOBasis

            !$omp parallel default(shared)
            !$omp master
            NThreads = 1
            !$ NThreads = omp_get_num_threads()
            !$omp end master
            !$omp end parallel
            call fock_BufferDim(DimJK, DimRho1D, AOBasis)
            call ks_BufferDim(DimTxc, AOBasis)
      end subroutine scf_BufferDim


      subroutine scf_IdempotentGuess(Rho_ao, Cocc_ao, NOcc, BasisVecs_ao, OccNumber)
            !
            ! Transform guess density matrix into an idempotent matrix using
            ! NOcc eigenvectors corresponding to the largest occupation numbers.
            !
            ! (1) Transform the AO density matrix to the ortogonalized AO basis (OAO)
            ! (2) Diagonalize Rho_oao to obtain OAO eigenvectors
            ! (3) Compute AO occupied orbitals from NOcc OAO eigenvectors corresponding
            !     to the largest occupation numbers of the guess density matrix
            ! (4) Build idempotent AO density matrix from NOcc occupied AO orbitals
            !
            real(F64), dimension(:, :, :), intent(inout) :: Rho_ao
            real(F64), dimension(:, :, :), intent(out)   :: Cocc_ao
            integer, dimension(:), intent(in)            :: NOcc
            real(F64), dimension(:, :), intent(in)       :: BasisVecs_ao
            real(F64), intent(in)                        :: OccNumber
            
            real(F64), dimension(:, :), allocatable :: Rho_oao
            real(F64), dimension(:, :), allocatable :: W
            real(F64), dimension(:), allocatable :: Eigenvals
            integer :: NOAO, NAO
            integer :: NSpins, s
            integer :: ThisImage, NImages

            ThisImage = this_image()
            NImages = num_images()
            if (ThisImage == 1) then
                  NAO = size(BasisVecs_ao, dim=1)
                  NOAO = size(BasisVecs_ao, dim=2)
                  NSpins = size(Rho_ao, dim=3)
                  allocate(W(NAO, NOAO))
                  allocate(Rho_oao(NOAO, NOAO))
                  allocate(Eigenvals(NOAO))
                  Cocc_ao = ZERO
                  do s = 1, NSpins
                        if (NOcc(s) > 0) then
                              call real_ab(W, Rho_ao(:, :, s), BasisVecs_ao)
                              call real_aTb(Rho_oao, BasisVecs_ao, W)
                              Rho_oao = -Rho_oao
                              call symmetric_eigenproblem(Eigenvals, Rho_oao, NOAO, .true.)
                              call real_ab(Cocc_ao(:, 1:NOcc(s), s), BasisVecs_ao, Rho_oao(:, 1:NOcc(s)))
                              call real_abT(Rho_ao(:, :, s), Cocc_ao(:, 1:NOcc(s), s), Cocc_ao(:, 1:NOcc(s), s))
                              Rho_ao(:, :, s) = OccNumber * Rho_ao(:, :, s)
                        end if
                  end do
            end if
            call co_broadcast(Rho_ao, source_image=1)
            call co_broadcast(Cocc_ao, source_image=1)
      end subroutine scf_IdempotentGuess
      
      
      subroutine scf_F_RealRho(F_cao, F_sao, Eel, ExcDFT, diag, AUXOut, Txc, K, J, Rho1D, &
            xcmodel, Rho_cao, Rho_sao, Hbare_cao, AUXIn, AOBasis, System, ThreshFockJK, &
            GridKind, GridPruning, time_F)
            
            real(F64), dimension(:, :, :), intent(out)             :: F_cao[*]
            real(F64), dimension(:, :, :), intent(out)             :: F_sao[*]
            real(F64), intent(out)                                 :: Eel
            real(F64), intent(out)                                 :: ExcDFT
            type(tgriddiag), intent(out)                           :: diag
            real(F64), dimension(:), intent(out)                   :: AUXOut
            real(F64), dimension(:, :, :), intent(out)             :: Txc
            real(F64), dimension(:), intent(out)                   :: K
            real(F64), dimension(:), intent(out)                   :: J
            real(F64), dimension(:), intent(out)                   :: Rho1D
            type(txcdef), intent(in)                               :: xcmodel
            real(F64), dimension(:, :, :), intent(in)              :: Rho_cao
            real(F64), dimension(:, :, :), intent(in)              :: Rho_sao
            real(F64), dimension(:, :), intent(in)                 :: Hbare_cao
            real(F64), dimension(:, :), intent(in)                 :: AUXIn
            type(TAOBasis), intent(in)                             :: AOBasis
            type(TSystem), intent(in)                              :: System
            real(F64), intent(in)                                  :: ThreshFockJK
            integer, intent(in)                                    :: GridKind
            logical, intent(in)                                    :: GridPruning
            real(F64), intent(inout)                               :: time_F
            
            real(F64) :: Omega
            real(F64) :: KFrac
            integer :: NSpins, s
            real(F64) :: EHbare, EHFTwoEl
            real(F64), dimension(1, 1, 1) :: RhoEff_cao
            logical :: LCExchange, BareH, SpinUnres, SHExchange
            logical :: ExchContrib, CoulContrib, AntisymRho
            integer :: i
            type(TClock) :: timer_F
            integer :: ThisImage, NImages

            call clock_start(timer_F)
            ThisImage = this_image()
            NImages = num_images()
            F_cao = ZERO
            F_sao = ZERO
            SHExchange = xcf_get_flag(xcmodel, XCF_SCREENED_HYBRID)
            LCExchange = xcf_get_flag(xcmodel, XCF_RSHYB)
            NSpins = size(Rho_cao, dim=3)
            SpinUnres = (NSpins > 1)
            if (LCExchange .or. SHExchange) then
                  Omega = xcf_get_omega(xcmodel)
                  if (Omega < ZERO) then
                        call dmsg("Invalid value of the range-separation parameter", &
                              xcf_get_omega(xcmodel), priority=MSG_ERROR)
                        error stop
                  end if
                  KFrac = xcf_get_srexx(xcmodel)
            else
                  Omega = -ONE
                  KFrac = xcf_get_exx(xcmodel)
            end if
            ExchContrib = (abs(KFrac) > ZERO .or. LCExchange .or. SHExchange)
            AntisymRho = .false.
            CoulContrib = xcf_get_flag(xcmodel, XCF_HARTREE)
            BareH = xcf_get_flag(xcmodel, XCF_BAREH)
            ! -----------------------------------------------------------------------------
            ! Hartree-Fock exchange and Coulomb components of the Kohn-Sham hamiltonian
            ! -----------------------------------------------------------------------------
            if (AOBasis%SpherAO) then
                  call fock_JK(F_sao, K, J, Rho1D, Rho_sao, AOBasis, ExchContrib, KFrac, &
                        AntisymRho, LCExchange, SHExchange, Omega, CoulContrib, ThreshFockJK)
                  EHFTwoEl = ZERO
                  do s = 1, NSpins
                        EHFTwoEl = EHFTwoEl + (ONE/TWO) * fock_RhoTrace(Rho_sao(:, :, s), F_sao(:, :, s))
                  end do
            else
                  call fock_JK(F_cao, K, J, Rho1D, Rho_cao, AOBasis, ExchContrib, KFrac, &
                        AntisymRho, LCExchange, SHExchange, Omega, CoulContrib, ThreshFockJK)
                  EHFTwoEl = ZERO
                  do s = 1, NSpins
                        EHFTwoEl = EHFTwoEl + (ONE/TWO) * fock_RhoTrace(Rho_cao(:, :, s), F_cao(:, :, s))
                  end do
            end if
            ! -----------------------------------------------------------------------------
            ! Semilocal exchnge-correlation potential part of the Kohn-Sham hamiltonian
            ! -----------------------------------------------------------------------------
            if (xcf_numint(xcmodel)) then
                  call ks_Vxc(F_cao, ExcDFT, diag, AuxOut, Txc, Rho_cao, RhoEff_cao, xcmodel, &
                        AUXIn, AOBasis, System, GridKind, GridPruning)
            else
                  ExcDFT = ZERO
            end if
            sync all
            if (NImages > 1) then
                  if (ThisImage == 1) then
                        do i = 2, NImages
                              F_sao(:, :, :) = F_sao(:, :, :) + F_sao(:, :, :)[i]
                              F_cao(:, :, :) = F_cao(:, :, :) + F_cao(:, :, :)[i]
                        end do
                  end if
                  !                  call co_sum(F_sao, result_image=1)
                  !                  call co_sum(F_cao, result_image=1)
                  call co_sum(EHFTwoEl, result_image=1)
                  call co_sum(ExcDFT, result_image=1)                  
            end if
            EHbare = ZERO
            if (ThisImage==1) then
                  if (BareH) then
                        EHbare = ZERO
                        do s = 1, NSpins
                              EHbare = EHbare + fock_RhoTrace(Rho_cao(:, :, s), Hbare_cao)
                              F_cao(:, :, s) = F_cao(:, :, s) + Hbare_cao
                        end do
                  end if
                  Eel = EHbare + EHFTwoEl + ExcDFT
            else
                  Eel = ZERO
                  ExcDFT = ZERO
            end if
            time_F = time_F + clock_readwall(timer_F)
            sync all
      end subroutine scf_F_RealRho


      subroutine scf_F_Cholesky(F_cao, F_sao, Eel, ExcDFT, diag, AUXOut, Txc, &
            xcmodel, Cocc_cao, Cocc_sao, NOcc, Rho_cao, Rho_sao, Hbare_cao, AUXIn, &
            AOBasis, System, Rkpq, CholeskyBasis, GridKind, GridPruning, &
            MaxBufferDimMB, TargetBlockDim, time_F)
            
            real(F64), dimension(:, :, :), intent(out)             :: F_cao
            real(F64), dimension(:, :, :), intent(out)             :: F_sao
            real(F64), intent(out)                                 :: Eel
            real(F64), intent(out)                                 :: ExcDFT
            type(tgriddiag), intent(out)                           :: diag
            real(F64), dimension(:), intent(out)                   :: AUXOut
            real(F64), dimension(:, :, :), intent(out)             :: Txc
            type(TXCDef), intent(in)                               :: XCModel
            real(F64), dimension(:, :, :), intent(in)              :: Cocc_cao
            real(F64), dimension(:, :, :), intent(in)              :: Cocc_sao
            integer, dimension(:), intent(in)                      :: NOcc
            real(F64), dimension(:, :, :), intent(in)              :: Rho_cao
            real(F64), dimension(:, :, :), intent(in)              :: Rho_sao
            real(F64), dimension(:, :), intent(in)                 :: Hbare_cao
            real(F64), dimension(:, :), intent(in)                 :: AUXIn
            type(TAOBasis), intent(in)                             :: AOBasis
            type(TSystem), intent(in)                              :: System
            real(F64), dimension(:, :, :), intent(in)              :: Rkpq
            type(TChol2Vecs), intent(in)                           :: CholeskyBasis
            integer, intent(in)                                    :: GridKind
            logical, intent(in)                                    :: GridPruning
            integer, intent(in)                                    :: MaxBufferDimMB
            integer, intent(in)                                    :: TargetBlockDim
            real(F64), intent(inout)                               :: time_F
            
            integer :: NSpins, s
            real(F64) :: EHbare, EHFTwoEl
            real(F64) :: KFrac
            real(F64), dimension(1, 1, 1) :: RhoEff_cao
            logical :: BareHContrib, CoulContrib, ExchContrib, XCContrib
            type(TClock) :: timer_F
            integer :: ThisImage

            call clock_start(timer_F)
            ThisImage = this_image()
            F_cao = ZERO
            F_sao = ZERO
            CoulContrib = xcf_get_flag(xcmodel, XCF_HARTREE)
            KFrac = xcf_get_exx(xcmodel)
            ExchContrib = (abs(KFrac) > ZERO)
            XCContrib = xcf_numint(xcmodel)
            BareHContrib = xcf_get_flag(xcmodel, XCF_BAREH)
            NSpins = max(size(F_sao,dim=3),size(F_cao,dim=3))
            !
            ! Two-electron part of the Fock operator
            !
            if (AOBasis%SpherAO) then
                  call chf_JK(F_sao, EHFTwoEl, Rho_sao, Cocc_sao, Rkpq, CholeskyBasis, NOcc, AOBasis, &
                        CoulContrib, ExchContrib, KFrac, MaxBufferDimMB, TargetBlockDim, SumJK=.false.)
            else
                  call chf_JK(F_cao, EHFTwoEl, Rho_cao, Cocc_cao, Rkpq, CholeskyBasis, NOcc, AOBasis, &
                        CoulContrib, ExchContrib, KFrac, MaxBufferDimMB, TargetBlockDim, SumJK=.false.)
            end if
            !
            ! Semilocal exchnge-correlation potential
            !
            ExcDFT = ZERO
            if (XCContrib) then
                  call ks_Vxc(F_cao, ExcDFT, diag, AuxOut, Txc, Rho_cao, RhoEff_cao, xcmodel, &
                        AUXIn, AOBasis, System, GridKind, GridPruning)
                  call co_sum(ExcDFT, result_image=1)
            end if
            !
            ! Bare nuclei hamiltonian
            !
            EHbare = ZERO
            if (BareHContrib) then
                  if (ThisImage==1) then
                        EHbare = ZERO
                        do s = 1, NSpins
                              EHbare = EHbare + fock_RhoTrace(Rho_cao(:, :, s), Hbare_cao)
                              F_cao(:, :, s) = F_cao(:, :, s) + Hbare_cao
                        end do
                  end if
            end if
            if (ThisImage==1) then
                  Eel = EHbare + EHFTwoEl + ExcDFT
            else
                  Eel = ZERO
                  ExcDFT = ZERO
            end if
            if (AOBasis%SpherAO) call co_sum(F_sao, result_image=1)
            if ((.not. AOBasis%SpherAO) .or. XCContrib) call co_sum(F_cao, result_image=1)
            time_F = time_F + clock_readwall(timer_F)
      end subroutine scf_F_Cholesky


      subroutine scf_F_THC(F_cao, F_sao, Eel, ExcDFT, diag, AUXOut, Txc, &
            xcmodel, Cocc_cao, Cocc_sao, NOcc, Rho_cao, Rho_sao, Hbare_cao, AUXIn, &
            AOBasis, System, THCGrid, GridKind, GridPruning, time_F)
            
            real(F64), dimension(:, :, :), intent(out)             :: F_cao
            real(F64), dimension(:, :, :), intent(out)             :: F_sao
            real(F64), intent(out)                                 :: Eel
            real(F64), intent(out)                                 :: ExcDFT
            type(tgriddiag), intent(out)                           :: diag
            real(F64), dimension(:), intent(out)                   :: AUXOut
            real(F64), dimension(:, :, :), intent(out)             :: Txc
            type(TXCDef), intent(in)                               :: XCModel
            real(F64), dimension(:, :, :), intent(in)              :: Cocc_cao
            real(F64), dimension(:, :, :), intent(in)              :: Cocc_sao
            integer, dimension(:), intent(in)                      :: NOcc
            real(F64), dimension(:, :, :), intent(in)              :: Rho_cao
            real(F64), dimension(:, :, :), intent(in)              :: Rho_sao
            real(F64), dimension(:, :), intent(in)                 :: Hbare_cao
            real(F64), dimension(:, :), intent(in)                 :: AUXIn
            type(TAOBasis), intent(in)                             :: AOBasis
            type(TSystem), intent(in)                              :: System
            type(TCoulTHCGrid), intent(in)                         :: THCGrid
            integer, intent(in)                                    :: GridKind
            logical, intent(in)                                    :: GridPruning
            real(F64), intent(inout)                               :: time_F
            
            integer :: NSpins, s
            real(F64) :: EHbare, EHFTwoEl
            real(F64) :: KFrac
            real(F64), dimension(1, 1, 1) :: RhoEff_cao
            logical :: BareHContrib, CoulContrib, ExchContrib, XCContrib
            type(TClock) :: timer_F
            integer :: ThisImage

            call clock_start(timer_F)
            ThisImage = this_image()
            F_cao = ZERO
            F_sao = ZERO
            CoulContrib = xcf_get_flag(xcmodel, XCF_HARTREE)
            KFrac = xcf_get_exx(xcmodel)
            ExchContrib = (abs(KFrac) > ZERO)
            XCContrib = xcf_numint(xcmodel)
            BareHContrib = xcf_get_flag(xcmodel, XCF_BAREH)
            NSpins = max(size(F_sao,dim=3),size(F_cao,dim=3))
            !
            ! Two-electron part of the Fock operator
            !
            if (AOBasis%SpherAO) then
                  call thc_Fock_JK(F_sao, EHFTwoEl, Cocc_sao, Rho_sao, THCGrid%Zgh, THCGrid%Xgp, &
                        NOcc, CoulContrib, ExchContrib, KFrac, .false.)                  
            else
                  call thc_Fock_JK(F_cao, EHFTwoEl, Cocc_cao, Rho_cao, THCGrid%Zgh, THCGrid%Xgp, &
                        NOcc, CoulContrib, ExchContrib, KFrac, .false.)                  
            end if
            !
            ! Semilocal exchnge-correlation potential
            !
            ExcDFT = ZERO
            if (XCContrib) then
                  call ks_Vxc(F_cao, ExcDFT, diag, AuxOut, Txc, Rho_cao, RhoEff_cao, xcmodel, &
                        AUXIn, AOBasis, System, GridKind, GridPruning)
                  call co_sum(ExcDFT, result_image=1)
            end if
            !
            ! Bare nuclei hamiltonian
            !
            EHbare = ZERO
            if (BareHContrib) then
                  if (ThisImage==1) then
                        EHbare = ZERO
                        do s = 1, NSpins
                              EHbare = EHbare + fock_RhoTrace(Rho_cao(:, :, s), Hbare_cao)
                              F_cao(:, :, s) = F_cao(:, :, s) + Hbare_cao
                        end do
                  end if
            end if
            if (ThisImage==1) then
                  Eel = EHbare + EHFTwoEl + ExcDFT
            else
                  Eel = ZERO
                  ExcDFT = ZERO
            end if
            if (AOBasis%SpherAO) call co_sum(F_sao, result_image=1)
            if ((.not. AOBasis%SpherAO) .or. XCContrib) call co_sum(F_cao, result_image=1)
            time_F = time_F + clock_readwall(timer_F)
      end subroutine scf_F_THC
      

      function scf_RhoDiff(RhoK, RhoN)
            real(F64) :: scf_RhoDiff
            real(F64), dimension(:, :, :), intent(in) :: RhoK
            real(F64), dimension(:, :, :), intent(in) :: RhoN

            integer :: m, n, p, q, NSpin, s
            real(F64) :: t, w

            m = size(RhoK, dim=1)
            n = size(RhoK, dim=2)
            NSpin = size(RhoK, dim=3)
            t = ZERO
            do s = 1, NSpin
                  do q = 1, n
                        do p = 1, m
                              w = abs(RhoK(p, q, s) - RhoN(p, q, s))
                              t = max(t, w)
                        end do
                  end do
            end do
            scf_RhoDiff = t
      end function scf_RhoDiff


      subroutine scf_TransformF(F_oao, F_cao, F_sao, BasisVecs_cao, BasisVecs_sao, &
            Noao, NAOCart, NAOSpher, SpherAO, TransfWork)
            !
            ! Transform the Fock/Kohn-Sham matrix from the AO to the OAO basis
            !
            real(F64), dimension(:, :), intent(out)         :: F_oao
            real(F64), dimension(:, :), intent(inout)       :: F_cao
            real(F64), dimension(:, :), intent(inout)       :: F_sao
            real(F64), dimension(:, :), intent(in)          :: BasisVecs_cao
            real(F64), dimension(:, :), intent(in)          :: BasisVecs_sao
            integer, intent(in)                             :: Noao
            integer, intent(in)                             :: NAOCart
            integer, intent(in)                             :: NAOSpher
            logical, intent(in)                             :: SpherAO
            real(F64), dimension(NAOCart*Noao), intent(out) :: TransfWork
            
            call smfill(F_cao)
            call real_aTb_x(TransfWork, Noao, BasisVecs_cao, NAOCart, F_cao, NAOCart, &
                  Noao, NAOCart, NAOCart, ONE, ZERO)
            call real_ab_x(F_oao, Noao, TransfWork, Noao, BasisVecs_cao, NAOCart, &
                  Noao, Noao, NAOCart, ONE, ZERO)
            if (SpherAO) then
                  call smfill(F_sao)
                  call real_aTb_x(TransfWork, Noao, BasisVecs_sao, NAOSpher, F_sao, NAOSpher, &
                        Noao, NAOSpher, NAOSpher, ONE, ZERO)
                  call real_ab_x(F_oao, Noao, TransfWork, Noao, BasisVecs_sao, NAOSpher, &
                        Noao, Noao, NAOSpher, ONE, ONE)
            end if
      end subroutine scf_TransformF


      subroutine scf_OccCoeffs(Cocc_ao, Cocc_oao, BasisVecs_ao, NOcc)
            real(F64), dimension(:, :, :), intent(out) :: Cocc_ao
            real(F64), dimension(:, :, :), intent(in)  :: Cocc_oao
            real(F64), dimension(:, :), intent(in)     :: BasisVecs_ao
            integer, dimension(:), intent(in)          :: NOcc
            
            integer :: s, NSpins
            integer :: MaxNOcc

            MaxNOcc = size(Cocc_ao, dim=2)
            NSpins = size(Cocc_ao, dim=3)
            do s = 1, NSpins
                  if (NOcc(s) < MaxNOcc) then
                        Cocc_ao(:, NOcc(s)+1:MaxNOcc, s) = ZERO
                  end if
                  if (NOcc(s) > 0) then
                        call real_ab(Cocc_ao(:, 1:NOcc(s), s), BasisVecs_ao, Cocc_oao(:, 1:NOcc(s), s))
                  end if
            end do
      end subroutine scf_OccCoeffs


      subroutine scf_Rho(Rho, Cocc, NOcc, OccNumber)
            real(F64), dimension(:, :, :), intent(out) :: Rho
            real(F64), dimension(:, :, :), intent(in)  :: Cocc
            integer, dimension(:), intent(in)          :: NOcc
            real(F64), intent(in)                      :: OccNumber

            integer :: NSpins, s

            NSpins = size(Rho, dim=3)
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call real_abT(Rho(:, :, s), Cocc(:, 1:NOcc(s), s), Cocc(:, 1:NOcc(s), s))
                  else
                        Rho(:, :, s) = ZERO
                  end if
            end do
            Rho = OccNumber * Rho
      end subroutine scf_Rho
      
      
      subroutine scf_ConvergeOrbitals(Rho_cao, OrbEnergies, Converged, EtotDFT, EelDFT, ExcDFT, &
            Noao, Ehomo, Elumo, Hbare_cao, C_oao, MOBasisVecsCart, MOBasisVecsSpher, NVirt, AUXOut, &
            XCModel, NonSCF, NOcc, LinDepThresh, Enucl, MaxRhoDiff, MaxOrbGrad, MaxNIters, &
            ThreshFockJK, AUXIn, AOBasis, System, ECPFile, GridKind, GridPruning, UseCholeskyBasis, &
            UseTensorHypercontraction, MaxBufferDimMB, TargetBlockDim, CholeskyVecs, CholeskyBasis, &
            THCGrid)
            !
            ! Main loop of the spin-unrestricted self-consistent field KS/DFT computations
            !
            real(F64), dimension(:, :, :), intent(inout)              :: Rho_cao
            real(F64), dimension(:, :), allocatable, intent(out)      :: OrbEnergies
            logical, intent(out)                                      :: Converged
            real(F64), intent(out)                                    :: EtotDFT
            real(F64), intent(out)                                    :: EelDFT
            real(F64), intent(out)                                    :: ExcDFT
            integer, intent(out)                                      :: Noao
            real(F64), intent(out)                                    :: Ehomo
            real(F64), intent(out)                                    :: Elumo
            real(F64), dimension(:, :), allocatable, intent(out)      :: Hbare_cao
            real(F64), dimension(:, :, :), allocatable, intent(out)   :: C_oao
            real(F64), dimension(:, :), allocatable, intent(out)      :: MOBasisVecsCart
            real(F64), dimension(:, :), allocatable, intent(out)      :: MOBasisVecsSpher
            integer, dimension(2), intent(out)                        :: NVirt
            real(F64), dimension(:), intent(out)                      :: AUXOut
            type(txcdef), intent(in)                                  :: XCModel
            type(txcdef), intent(in)                                  :: NonSCF
            integer, dimension(2), intent(in)                         :: NOcc
            real(F64), intent(in)                                     :: LinDepThresh
            real(F64), intent(in)                                     :: Enucl
            real(F64), intent(in)                                     :: MaxRhoDiff
            real(F64), intent(in)                                     :: MaxOrbGrad
            integer, intent(in)                                       :: MaxNIters
            real(F64), intent(in)                                     :: ThreshFockJK
            real(F64), dimension(:, :), intent(in)                    :: AUXIn
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TSystem), intent(in)                                 :: System
            type(TStringList), intent(in)                             :: ECPFile
            integer, intent(in)                                       :: GridKind
            logical, intent(in)                                       :: GridPruning
            logical, intent(in)                                       :: UseCholeskyBasis
            logical, intent(in)                                       :: UseTensorHypercontraction
            integer, optional, intent(in)                             :: MaxBufferDimMB
            integer, optional, intent(in)                             :: TargetBlockDim
            real(F64), dimension(:, :, :), optional, intent(in)       :: CholeskyVecs
            type(TChol2Vecs), optional, intent(in)                    :: CholeskyBasis
            type(TCoulTHCGrid), optional, intent(in)                  :: THCGrid

            type(tgriddiag) :: gdiag
            real(F64), dimension(:, :, :), allocatable :: RhoK_cao, Ck_oao
            real(F64), dimension(:, :), allocatable :: Eigenvals, BasisVecs_cao
            real(F64), dimension(:, :), allocatable :: BasisVecs_sao
            real(F64), dimension(:, :, :), allocatable :: Fn_oao, Cn_oao
            real(F64), dimension(:, :, :), allocatable :: Fn_cao[:], Fn_sao[:]
            real(F64), dimension(:, :, :), allocatable :: RhoN_cao, RhoN_sao
            real(F64), dimension(:, :, :), allocatable :: Cocc_cao, Cocc_sao
            real(F64), dimension(:, :), allocatable :: S_cao, Ts_cao, Vne_cao
            real(F64), dimension(:), allocatable :: TransfWork
            integer :: NSpins, NVirtA, NVirtB
            integer :: NOccA, NOccB
            logical :: SpherAO
            integer :: NAOCart, NAOSpher
            real(F64) :: RhoDiff, EDiff, OrbGrad, OrbShift
            real(F64) :: EtotN, EtotK, EelN, EelK, ExcN, ExcK
            type(uarhdata) :: arh_data
            integer :: NIters, NMicroIters, NStored, s
            logical :: AcceptAllCn
            logical, allocatable :: ConvConds[:], DoMicroIters[:], DoMacroIters[:]
            type(tclock) :: timer_Iter, timer_Total
            real(F64) :: time_F, time_ARH, time_Iter
            integer, parameter :: MaxNMicroIters = 10
            real(F64), parameter :: MinOrbGrad = ZERO
            real(F64) :: OccNumber
            logical :: SpinUnres
            integer, dimension(2) :: OccRangeA, OccRangeB, VirtRangeA, VirtRangeB
            integer :: DimTxc, DimJK, DimRho1D
            real(F64), dimension(:, :, :), allocatable :: BufferTxc
            real(F64) :: memoryTxc, memoryJK, memoryRho1D
            real(F64), dimension(:), allocatable :: BufferK, BufferJ
            real(F64), dimension(:), allocatable :: BufferRho1D
            integer :: ThisImage, NImages, NThreads
            logical :: TrustRadiusUpdated
            logical, parameter :: GenerateGuessOrbitals = .true.

            ThisImage = this_image()
            NImages = num_images()
            allocate(ConvConds[*], DoMicroIters[*], DoMacroIters[*])
            SpherAO = AOBasis%SpherAO
            NAOCart = AOBasis%NAOCart
            NAOSpher = AOBasis%NAOSpher
            NOccA = NOcc(1)
            NOccB = NOcc(2)
            if (NOccA == NOccB) then
                  SpinUnres = .false.
                  NSpins = 1
                  OccNumber = TWO
                  call msg("Starting self-consistent field iterations (spin-restricted singlet)")
                  call msg("Alpha+beta electrons: " // str(NOccA+NOccB))
            else
                  SpinUnres = .true.
                  NSpins = 2
                  OccNumber = ONE
                  call msg("Starting self-consistent field iterations (spin-unrestricted)")
                  call msg("Alpha electrons: " // str(NOccA))
                  call msg("Beta electrons: " // str(NOccB))
            end if
            call scf_XCInfo(XCModel)
            call midrule()
            call scf_CPUInfo()
            if (xcf_numint(XCModel)) call gridsummary(GridKind, GridPruning)
            call clock_start(timer_Total)
            call clock_start(timer_Iter)
            time_F = ZERO
            time_ARH = ZERO
            call scf_BufferDim(DimTxc, DimJK, DimRho1D, NThreads, AOBasis)
            allocate(BufferK(DimJK))
            allocate(BufferJ(DimJK))
            allocate(BufferRho1D(DimRho1D))
            allocate(BufferTxc(DimTxc, NSpins, NThreads))
            memoryJK = (io_size_byte(BufferK)+io_size_byte(BufferJ)) / real(1024**3, F64)
            memoryTxc = io_size_byte(BufferTxc) / real(1024**3, F64)
            memoryRho1D = io_size_byte(BufferRho1D) / real(1024**3, F64)
            call msg("Allocated " // str(memoryJK+memoryTxc+memoryRho1D, d=1) // " GiB of scratch space for the KS matrix build")
            allocate(S_cao(NAOCart, NAOCart))
            allocate(Ts_cao(NAOCart, NAOCart))
            allocate(Vne_cao(NAOCart, NAOCart))
            allocate(Hbare_cao(NAOCart, NAOCart))
            !
            ! One-electron integrals: overlap, kinetic,
            ! and nuclei-electron attraction
            !
            call ints1e_OverlapMatrix(S_cao, AOBasis)
            call ints1e_Kinetic(Ts_cao, AOBasis)
            call ints1e_Coulomb(Vne_cao, AOBasis, System)
            !
            ! Add effective core potential to
            ! the nuclei-electrons potential
            !            
            call pp_V(Vne_cao, AOBasis, System, ECPFile)
            Hbare_cao = Ts_cao + Vne_cao
            !
            ! Compute the orthogonal basis vectors.
            ! Remove linear dependencies and transform vectors
            ! to the solid harmonic basis.
            !
            call smfill(S_cao)
            call basis_OAO(BasisVecs_cao, BasisVecs_sao, S_cao, AOBasis, LinDepThresh)
            call msg("Threshold for J, K: |Rho(r,s)*(pq|rs)|,|Rho(q,s)*(pq|rs)| > " // str(ThreshFockJK,d=1))
            call msg("Convergence conditions", underline=.true.)
            call msg("RhoDiff < " // str(MaxRhoDiff,d=1))
            call msg("OrbGrad < " // str(MaxOrbGrad,d=1))
            call msg("Max number of macroiters: " // str(MaxNIters))
            !
            ! Dimension of the orthogonal linearly-independent basis.
            ! Takes into account the transformation to the spherical
            ! harmonics basis.
            !
            Noao = size(BasisVecs_cao, dim=2)
            NVirtA = Noao - NOccA
            NVirtB = Noao - NOccB
            NVirt = [NVirtA, NVirtB]
            allocate(Eigenvals(Noao, NSpins))
            allocate(RhoN_cao(NAOCart, NAOCart, NSpins))
            allocate(RhoN_sao(NAOSpher, NAOSpher, NSpins))
            allocate(RhoK_cao(NAOCart, NAOCart, NSpins))
            allocate(Fn_oao(Noao, Noao, NSpins))
            allocate(Fn_sao(NAOSpher, NAOSpher, NSpins)[*])
            allocate(Fn_cao(NAOCart, NAOCart, NSpins)[*])
            allocate(Cn_oao(Noao, Noao, NSpins))
            allocate(Cocc_cao(NAOCart, max(NOcc(1),NOcc(2)), NSpins))
            allocate(Cocc_sao(NAOSpher, max(NOcc(1),NOcc(2)), NSpins))
            if (SpherAO) then
                  allocate(TransfWork(NAOSpher*NAOCart))
            else
                  allocate(TransfWork(Noao*NAOCart))
            end if
            if (ThisImage == 1) then
                  allocate(Ck_oao(Noao, Noao, NSpins))
            end if
            RhoK_cao = Rho_cao
            if (GenerateGuessOrbitals) then
                  call scf_IdempotentGuess(RhoK_cao, Cocc_cao, NOcc, BasisVecs_cao, OccNumber)
            end if
            if (SpherAO) then
                  do s = 1, NSpins
                        call SpherGTO_TransformMatrix(RhoN_sao(:, :, s), RhoK_cao(:, :, s), &
                              AOBasis%LmaxGTO, &
                              AOBasis%NormFactorsSpher, &
                              AOBasis%NormFactorsCart, &
                              AOBasis%ShellLocSpher, &
                              AOBasis%ShellLocCart, &
                              AOBasis%ShellMomentum, &
                              AOBasis%ShellParamsIdx, &
                              AOBasis%NAOSpher, &
                              AOBasis%NAOCart, &
                              AOBasis%NShells, TransfWork)
                        if (GenerateGuessOrbitals) then
                              call SpherGTO_TransformVectors(Cocc_sao(:, 1:NOcc(s), s), Cocc_cao(:, 1:NOcc(s), s), &
                                    AOBasis%LmaxGTO, &
                                    AOBasis%NormFactorsSpher, &
                                    AOBasis%NormFactorsCart, &
                                    AOBasis%ShellLocSpher, &
                                    AOBasis%ShellLocCart, &
                                    AOBasis%ShellMomentum, &
                                    AOBasis%ShellParamsIdx, &
                                    AOBasis%NAOSpher, &
                                    AOBasis%NAOCart, &
                                    AOBasis%NShells, NOcc(s))
                        end if
                  end do
            end if
            ! --------------------------------------------------------------------
            !    Compute the initial Kohn-Sham/Fock matrix Fk := F(RhoK)
            !    from the guess AO density matrix. Diagonalize Fk to get the
            !    initial set of MO vectors in the OAO basis (Cn_oao).
            ! --------------------------------------------------------------------
            if (UseCholeskyBasis .and. .not. UseTensorHypercontraction) then
                  call scf_F_Cholesky(Fn_cao, Fn_sao, EelK, ExcK, gdiag, AUXOut, BufferTxc, &
                        XCModel, Cocc_cao, Cocc_sao, NOcc, RhoK_cao, RhoN_sao, Hbare_cao, AUXIn, &
                        AOBasis, System, CholeskyVecs, CholeskyBasis, GridKind, GridPruning, &
                        MaxBufferDimMB, TargetBlockDim, time_F)
            else if (UseTensorHypercontraction) then
                  call scf_F_THC(Fn_cao, Fn_sao, EelK, ExcK, gdiag, AUXOut, BufferTxc, &
                        XCModel, Cocc_cao, Cocc_sao, NOcc, RhoK_cao, RhoN_sao, Hbare_cao, AUXIn, &
                        AOBasis, System, THCGrid, GridKind, GridPruning, time_F)                  
            else
                  call scf_F_RealRho(Fn_cao, Fn_sao, EelK, ExcK, gdiag, AUXOut, &
                        BufferTxc, BufferK, BufferJ, BufferRho1D, XCModel, RhoK_cao, RhoN_sao, &
                        Hbare_cao, AUXIn, AOBasis, System, ThreshFockJK, GridKind, GridPruning, time_F)
            end if
            EtotK = EelK + Enucl
            do s = 1, NSpins
                  if (ThisImage == 1) then
                        call scf_TransformF(Fn_oao(:, :, s), Fn_cao(:, :, s), Fn_sao(:, :, s), &
                              BasisVecs_cao, BasisVecs_sao, Noao, NAOCart, NAOSpher, SpherAO, &
                              TransfWork)
                        Cn_oao(:, :, s) = Fn_oao(:, :, s)
                        call symmetric_eigenproblem(Eigenvals(:, s), Cn_oao(:, :, s), Noao, .true.)
                  end if
            end do
            call co_broadcast(Cn_oao, source_image=1)
            call scf_OccCoeffs(Cocc_cao, Cn_oao, BasisVecs_cao, NOcc)
            call scf_OccCoeffs(Cocc_sao, Cn_oao, BasisVecs_sao, NOcc)
            call scf_Rho(RhoN_sao, Cocc_sao, NOcc, OccNumber)
            call scf_Rho(RhoN_cao, Cocc_cao, NOcc, OccNumber)            
            time_Iter = clock_readwall(timer_iter)
            call scf_TableHeader()
            call scf_TableRow(0, EtotK, ZERO, ZERO, ZERO, ZERO, 0, time_Iter, .false.)
            !
            ! Initialize the data structure for the SCF algorithm.
            !
            if (ThisImage == 1) then
                  OccRangeA = [1, NOccA]
                  OccRangeB = [1, NOccB]
                  VirtRangeA = [NOccA+1, NOccA+NVirtA]
                  VirtRangeB = [NOccB+1, NOccB+NVirtB]
                  call uarh_init(arh_data, OccNumber, OccRangeA, OccRangeB, VirtRangeA, &
                        VirtRangeB, Noao, (SpinUnres.and.NOccB>0))
            end if
            ! --------------------------------------------------------------------
            !     Build Fn := F(RhoN) to start the proper iterative process
            ! --------------------------------------------------------------------
            call clock_start(timer_Iter)
            if (UseCholeskyBasis .and. .not. UseTensorHypercontraction) then
                  call scf_F_Cholesky(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, BufferTxc, &
                        XCModel, Cocc_cao, Cocc_sao, NOcc, RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, &
                        AOBasis, System, CholeskyVecs, CholeskyBasis, GridKind, GridPruning, &
                        MaxBufferDimMB, TargetBlockDim, time_F)
            else if (UseTensorHypercontraction) then
                  call scf_F_THC(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, BufferTxc, &
                        XCModel, Cocc_cao, Cocc_sao, NOcc, RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, &
                        AOBasis, System, THCGrid, GridKind, GridPruning, time_F)
            else
                  call scf_F_RealRho(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, &
                        BufferTxc, BufferK, BufferJ, BufferRho1D, XCModel, &
                        RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, AOBasis, System, &
                        ThreshFockJK, GridKind, GridPruning, time_F)
            end if
            if (ThisImage == 1) then
                  do s = 1, NSpins
                        call scf_TransformF(Fn_oao(:, :, s), Fn_cao(:, :, s), Fn_sao(:, :, s), &
                              BasisVecs_cao, BasisVecs_sao, Noao, NAOCart, NAOSpher, SpherAO, &
                              TransfWork)
                  end do
            end if
            EtotN = EelN + Enucl
            RhoDiff = scf_RhoDiff(RhoK_cao, RhoN_cao)
            EDiff = EtotN - EtotK
            time_Iter = clock_readwall(timer_Iter)
            call scf_TableRow(1, EtotN, EDiff, RhoDiff, ZERO, ZERO, 0, time_Iter, .false.)
            Ck_oao = Cn_oao
            RhoK_cao = RhoN_cao
            EtotK = EtotN
            EelK = EelN
            ExcK = ExcN
            AcceptAllCn = .false.
            ConvConds = .false.
            DoMacroIters = .true.
            NIters = 1
            MacroIters: do while (DoMacroIters)
                  NIters = NIters + 1
                  NMicroIters = 0
                  DoMicroIters = .true.
                  MicroIters: do while (DoMicroIters)
                        NMicroIters = NMicroIters + 1
                        call clock_start(timer_Iter)
                        !
                        ! Compute updated molecular orbitals and density matrix
                        !
                        if (ThisImage == 1) then
                              call uarh_NextIter(arh_data, Cn_oao, OrbGrad, OrbShift, NStored, &
                                    Fn_oao, EelK, (NMicroIters>1), time_ARH)
                        end if
                        call co_broadcast(Cn_oao, source_image=1)
                        call scf_OccCoeffs(Cocc_cao, Cn_oao, BasisVecs_cao, NOcc)
                        call scf_OccCoeffs(Cocc_sao, Cn_oao, BasisVecs_sao, NOcc)
                        call scf_Rho(RhoN_sao, Cocc_sao, NOcc, OccNumber)
                        call scf_Rho(RhoN_cao, Cocc_cao, NOcc, OccNumber)                        
                        !
                        ! Use the new density matrix to compute the Kohn-Sham/Fock matrix
                        !
                        if (UseCholeskyBasis .and. .not. UseTensorHypercontraction) then
                              call scf_F_Cholesky(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, BufferTxc, &
                                    XCModel, Cocc_cao, Cocc_sao, NOcc, RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, &
                                    AOBasis, System, CholeskyVecs, CholeskyBasis, GridKind, GridPruning, &
                                    MaxBufferDimMB, TargetBlockDim, time_F)
                        else if (UseTensorHypercontraction) then
                              call scf_F_THC(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, BufferTxc, &
                                    XCModel, Cocc_cao, Cocc_sao, NOcc, RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, &
                                    AOBasis, System, THCGrid, GridKind, GridPruning, time_F)
                        else
                              call scf_F_RealRho(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, &
                                    BufferTxc, BufferK, BufferJ, BufferRho1D, XCModel, &
                                    RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, AOBasis, System, &
                                    ThreshFockJK, GridKind, GridPruning, time_F)
                        end if
                        EtotN = EelN + Enucl
                        !
                        ! Measures of convergence: max norm of the orbital gradient
                        ! matrix, max difference between the old and new AO density
                        ! matrices, and total energy difference
                        !
                        RhoDiff = scf_RhoDiff(RhoK_cao, RhoN_cao)
                        EDiff = EtotN - EtotK
                        if (ThisImage == 1) then
                              if (OrbGrad > MinOrbGrad) then
                                    call uarh_TrustRadius(arh_data, TrustRadiusUpdated, EelN, EelK)
                              else
                                    !
                                    ! During the final stages of the optimization
                                    ! the trust radius isn't updated.
                                    !
                                    AcceptAllCn = .true.
                                    call uarh_DisableShift(arh_data)
                              end if
                              if (EDiff <= ZERO .or. NMicroIters > MaxNMicroIters .or. AcceptAllCn .or. &
                                    (.not. TrustRadiusUpdated)) then
                                    DoMicroIters = .false.
                              else
                                    Cn_oao = Ck_oao
                              end if
                              time_Iter = clock_readwall(timer_Iter)
                              call scf_TableRow(NIters, EtotN, EDiff, RhoDiff, OrbGrad, &
                                    abs(OrbShift), NStored, time_Iter, (NMicroIters>1))
                              sync images(*)
                        else
                              sync images(1)
                              DoMicroIters = DoMicroIters[1]
                        end if
                  end do MicroIters
                  if (ThisImage == 1) then
                        do s = 1, NSpins
                              call scf_TransformF(Fn_oao(:, :, s), Fn_cao(:, :, s), Fn_sao(:, :, s), &
                                    BasisVecs_cao, BasisVecs_sao, Noao, NAOCart, NAOSpher, SpherAO, &
                                    TransfWork)
                        end do
                  end if
                  EtotK = EtotN
                  EelK = EelN
                  ExcK = ExcN
                  RhoK_cao = RhoN_cao
                  if (ThisImage == 1) then
                        Ck_oao = Cn_oao
                        ConvConds = (RhoDiff < MaxRhoDiff .and. OrbGrad < MaxOrbGrad)
                        if (ConvConds .or. NIters == MaxNIters) DoMacroIters = .false.
                        sync images(*)
                  else
                        sync images(1)
                        DoMacroIters = DoMacroIters[1]
                  end if
            end do MacroIters
            if (ThisImage == 1) then
                  Converged = ConvConds
            else
                  Converged = ConvConds[1]
            end if
            EtotDFT = EtotN
            EelDFT = EelN
            ExcDFT = ExcN
            if (Converged) then
                  call toprule(width=SCFRuleWidth)
                  call msg("SCF converged")
                  call dmsg("Converged energy", EtotDFT)
                  call midrule(width=SCFRuleWidth)
            else
                  call toprule(width=SCFRuleWidth)
                  call msg("SCF not converged")
                  call dmsg("Energy (last iteration)", EtotDFT)
                  call midrule(width=SCFRuleWidth)
            end if
            if (xcf_isgridxc(XCModel)) then
                  call scf_GridDiagnostic(gdiag, xcf_ismgga(XCModel))
            end if
            ! ----------------------------------------------------------------
            ! Non-SCF exchange-correlation energy. Example:
            ! semilocal DFT exchange-correlation energy computed with
            ! a converged Hartree-Fock density.
            !
            ! Note that the HOMO and LUMO orbitals computed at a later
            ! step are obtained from the non-SCF Kohn-Sham matrix.
            !
            ! If the non-SCF computation is enabled, the density matrix
            ! computed as an output of this subroutine corresponds to
            ! the eigenvectors of the non-SCF Kohn-Sham matrix.
            ! ----------------------------------------------------------------
            if (xcf_get_id(NonSCF) /= XCF_XC_NONE) then
                  call msg("Starting non-SCF DFT energy computation")
                  call clock_start(timer_Iter)
                  call scf_XCInfo(NonSCF)
                  if (.not. NonSCF%SlaterVxc) then
                        call scf_F_RealRho(Fn_cao, Fn_sao, EelN, ExcN, gdiag, AUXOut, &
                              BufferTxc, BufferK, BufferJ, BufferRho1D, NonSCF, &
                              RhoN_cao, RhoN_sao, Hbare_cao, AUXIn, AOBasis, System, &
                              ThreshFockJK, GridKind, GridPruning, time_F)
                  else
                        call slater_F_RI(Fn_cao, EelN, ExcN, NonSCF, RhoN_cao, Hbare_cao, &
                              S_cao, BasisVecs_cao, AOBasis, System, ThreshFockJK, gdiag, &
                              GridKind, GridPruning, time_F)
                        Fn_sao = ZERO
                  end if                  
                  do s = 1, NSpins
                        if (ThisImage == 1) then
                              call scf_TransformF(Fn_oao(:, :, s), Fn_cao(:, :, s), Fn_sao(:, :, s), &
                                    BasisVecs_cao, BasisVecs_sao, Noao, NAOCart, NAOSpher, &
                                    (SpherAO .and. .not. NonSCF%SlaterVxc), TransfWork)
                        end if
                  end do
                  EtotN = EelN + Enucl
                  EtotDFT = EtotN
                  EelDFT = EelN
                  ExcDFT = ExcN
                  time_Iter = clock_readwall(timer_Iter)
                  call midrule(width=SCFRuleWidth)
                  call scf_TableRow(0, EtotN, ZERO, ZERO, ZERO, ZERO, 0, time_Iter, .false.)
                  call midrule(width=SCFRuleWidth)
            end if
            !
            ! Compute final set of orbitals, orbital energies,
            ! and the density matrix
            !
            allocate(OrbEnergies(Noao, NSpins))
            allocate(C_oao(Noao, Noao, NSpins))
            if (ThisImage == 1) then
                  do s = 1, NSpins                        
                        call symmetric_eigenproblem(Eigenvals(:, s), Fn_oao(:, :, s), Noao, .true.)
                  end do
                  C_oao = Fn_oao
                  OrbEnergies = Eigenvals
            end if
            call co_broadcast(C_oao, source_image=1)
            call co_broadcast(OrbEnergies, source_image=1)
            !
            ! Compute the final density matrix out of the eigenvectors of the converged
            ! Kohn-Sham matrix. 
            !
            call scf_OccCoeffs(Cocc_cao, C_oao, BasisVecs_cao, NOcc)
            call scf_OccCoeffs(Cocc_sao, C_oao, BasisVecs_sao, NOcc)
            call scf_Rho(RhoN_sao, Cocc_sao, NOcc, OccNumber)
            call scf_Rho(RhoN_cao, Cocc_cao, NOcc, OccNumber)                        
            Rho_cao = RhoN_cao
            call move_alloc(from=BasisVecs_cao, to=MOBasisVecsCart)
            call move_alloc(from=BasisVecs_sao, to=MOBasisVecsSpher)
            if (xcf_get_id(NonSCF) /= XCF_XC_NONE) then
                  call msg("HOMO/LUMO energies from the diagonalization of non-SCF KS matrix")
            end if
            if (SpinUnres .and. NOccB > 0) then
                  Elumo = min(OrbEnergies(NOccA+1, 1), OrbEnergies(NOccB+1, 2))
                  Ehomo = max(OrbEnergies(NOccA, 1), OrbEnergies(NOccB, 2))
                  call dmsg("Ehomo(Alpha) [eV]", toev(OrbEnergies(NOccA, 1)))
                  call dmsg("Elumo(Alpha) [eV]", toev(OrbEnergies(NOccA+1, 1)))
                  call dmsg("Ehomo(Beta) [eV]", toev(OrbEnergies(NOccB, 2)))
                  call dmsg("Elumo(Beta) [eV]", toev(OrbEnergies(NOccB+1, 2)))
            else
                  Elumo = OrbEnergies(NOccA+1, 1)
                  Ehomo = OrbEnergies(NOccA, 1)
                  call dmsg("Ehomo [eV]", toev(Ehomo))
                  call dmsg("Elumo [eV]", toev(Elumo))
            end if
            call midrule()
            call msg("Total time for SCF: " // str(clock_readwall(timer_Total), d=1) // " seconds")
            call msg("Detailed timings in seconds")
            call msg("Kohn-Sham matrices " // str(time_F,d=1))
            call msg("SCF solver         " // str(time_ARH,d=1))
            call uarh_free(arh_data)
      end subroutine scf_ConvergeOrbitals


      subroutine scf_driver_SpinUnres(SCFOutput, SCFParams, AOBasis, System, CholeskyVecs, CholeskyBasis, THCGrid)
            !
            ! Driver routine for spin-unrestricted SCF
            !
            type(TSCFOutput), intent(out)                       :: SCFOutput
            type(TSCFParams), intent(in)                        :: SCFParams
            type(TAOBasis), intent(in)                          :: AOBasis
            type(TSystem), intent(in)                           :: System
            real(F64), dimension(:, :, :), optional, intent(in) :: CholeskyVecs
            type(TChol2Vecs), optional, intent(in)              :: CholeskyBasis
            type(TCoulTHCGrid), optional, intent(in)            :: THCGrid
            
            type(txcdef) :: XCModel, NonSCF
            real(F64) :: MaxRhoDiff, MaxOrbGrad, LinDepThresh
            integer :: MaxNIters, NSpins
            logical :: SpinUnres
            integer :: ThisImage
            character(:), allocatable :: GuessPathA, GuessPathB
            character(:), allocatable :: SavePathA, SavePathB

            ThisImage = this_image()
            associate (AUXInt_Type1 => SCFParams%AUXInt_Type1, AUXInt_Type2 => SCFParams%AUXInt_Type2, &
                  AUXIn => SCFParams%AUXIn, xcfunc => SCFParams%xcfunc, omega => SCFParams%omega, &
                  srexx => SCFParams%srexx, non_scf_xcfunc => SCFParams%non_scf_xcfunc, &
                  non_scf_omega => SCFParams%non_scf_omega, non_scf_srexx => SCFParams%non_scf_srexx, &
                  guess_type => SCFParams%guess_type, save_rho_mode => SCFParams%save_rho_mode, &
                  NOcc => SCFOutput%NOcc, NAOCart => AOBasis%NAOCart, NElectrons => System%NElectrons, &
                  Mult => System%Mult)

                  if (XCFunc == XCF_XC_NONE) then
                        call msg("Invalid model of exchange-correlation", MSG_ERROR)
                        error stop
                  end if
                  !
                  ! Number of alpha and beta occupied spin-orbitals
                  !
                  ! NOccA + NOccB = NElectrons
                  ! NOccA - NOccB = MULTIPLICITY - 1
                  ! NOccA = (NElectrons + MULTIPLICITY - 1) / 2
                  ! NOccB = NElectrons - NOccA
                  !                  
                  NOcc(1) = (NElectrons + Mult - 1) / 2
                  NOcc(2) = NElectrons - NOcc(1)
                  if (NOcc(1) == NOcc(2)) then
                        NSpins = 1
                        SpinUnres = .false.
                  else
                        NSpins = 2
                        SpinUnres = .true.
                  end if
                  !
                  ! Allocate space for auxiliary numerical integrals
                  !
                  allocate(SCFOutput%AUXOut(aux_arraydim(AUXInt_Type1, SpinUnres)))
                  call xcf_define(XCModel, xcfunc, AUXInt_Type1, SpinUnres)
                  call xcf_define(NonSCF, non_scf_xcfunc, AUX_NONE, SpinUnres)
                  NonSCF%SlaterVxc = SCFParams%SlaterVxc
                  if (SCFParams%AsympVxc == AC_LFAS_v2) then
                        if (System%NAtoms == 1) then
                              XCModel%AsympVxc = AC_LFAS_v2_FREE
                        else
                              XCModel%AsympVxc = AC_LFAS_v2
                        end if
                        XCModel%AsympVxcOmega = SCFParams%AsympVxcOmega
                  else if (SCFParams%AsympVxc == AC_LFAS) then
                        call xcf_define(NonSCF, xcfunc, AUXInt_Type1, SpinUnres)
                        if (System%NAtoms == 1) then
                              XCModel%AsympVxc = AC_LFAS_v2_FREE
                              NonSCF%AsympVxc = AC_LFAS_FREE
                        else
                              XCModel%AsympVxc = AC_LFAS_v2
                              NonSCF%AsympVxc = AC_LFAS
                        end if                              
                        XCModel%AsympVxcOmega = SCFParams%AsympVxcOmega
                        NonSCF%AsympVxcOmega = SCFParams%AsympVxcOmega
                  end if
                  !
                  ! Check if user defined non-default values for the range-separation
                  ! parameter and/or fraction of the short-range HF exchange
                  !
                  if (.not. omega < ZERO) call xcf_set_omega(XCModel, omega)
                  if (.not. srexx < ZERO) call xcf_set_srexx(XCModel, srexx)
                  if (non_scf_xcfunc .ne. XCF_XC_NONE) then
                        if (.not. non_scf_omega < ZERO) call xcf_set_omega(NonSCF, non_scf_omega)
                        if (.not. non_scf_srexx < ZERO) call xcf_set_srexx(NonSCF, non_scf_srexx)
                  end if
                  MaxRhoDiff = SCFParams%ConvThreshRho
                  MaxOrbGrad = SCFParams%ConvThreshGrad
                  LinDepThresh = SCFParams%LinDepThresh
                  MaxNIters = SCFParams%MaxNIters
                  call sys_NuclearRepulsion(SCFOutput%Enucl, System)
                  GuessPathA = ""
                  GuessPathB = ""
                  if (allocated(SCFParams%guess_rhoa_path)) GuessPathA = SCFParams%guess_rhoa_path
                  if (allocated(SCFParams%guess_rhob_path)) GuessPathB = SCFParams%guess_rhob_path
                  allocate(SCFOutput%Rho_cao(NAOCart, NAOCart, NSpins))
                  call scf_RhoStart(SCFOutput%Rho_cao, guess_type, GuessPathA, GuessPathB)
                  ! ------------------------------------------------------------------------
                  !                     MAIN SELF-CONSISTENT FIELD LOOP
                  ! ------------------------------------------------------------------------
                  if (SCFParams%UseCholeskyBasis .and. .not. SCFParams%UseTensorHypercontraction) then
                        call scf_ConvergeOrbitals( &
                              SCFOutput%Rho_cao, &
                              SCFOutput%OrbEnergies, &
                              SCFOutput%Converged, &
                              SCFOutput%EtotDFT, &
                              SCFOutput%EelDFT, &
                              SCFOutput%ExcDFT, &
                              SCFOutput%Noao, &
                              SCFOutput%Ehomo, &
                              SCFOutput%Elumo, &
                              SCFOutput%Hbare_cao, &
                              SCFOutput%C_oao, &
                              SCFOutput%MOBasisVecsCart, &
                              SCFOutput%MOBasisVecsSpher, &
                              SCFOutput%NVirt, &
                              SCFOutput%AUXOut, &
                              XCModel, NonSCF, &
                              SCFOutput%NOcc, &
                              LinDepThresh, &
                              SCFOutput%Enucl, &
                              MaxRhoDiff, MaxOrbGrad, MaxNIters, &
                              SCFParams%ThreshFockJK, &
                              AUXIn, AOBasis, System, &
                              SCFParams%ECPFile, &
                              SCFParams%GridKind, &
                              SCFParams%GridPruning, &
                              .true., &
                              .false., &
                              SCFParams%MaxBufferDimMB, &
                              SCFParams%TargetBlockDim, &
                              CholeskyVecs, &
                              CholeskyBasis &
                              )
                  else if (SCFParams%UseTensorHypercontraction) then
                        call scf_ConvergeOrbitals( &
                              SCFOutput%Rho_cao, &
                              SCFOutput%OrbEnergies, &
                              SCFOutput%Converged, &
                              SCFOutput%EtotDFT, &
                              SCFOutput%EelDFT, &
                              SCFOutput%ExcDFT, &
                              SCFOutput%Noao, &
                              SCFOutput%Ehomo, &
                              SCFOutput%Elumo, &
                              SCFOutput%Hbare_cao, &
                              SCFOutput%C_oao, &
                              SCFOutput%MOBasisVecsCart, &
                              SCFOutput%MOBasisVecsSpher, &
                              SCFOutput%NVirt, &
                              SCFOutput%AUXOut, &
                              XCModel, NonSCF, &
                              SCFOutput%NOcc, &
                              LinDepThresh, &
                              SCFOutput%Enucl, &
                              MaxRhoDiff, MaxOrbGrad, MaxNIters, &
                              SCFParams%ThreshFockJK, &
                              AUXIn, AOBasis, System, &
                              SCFParams%ECPFile, &
                              SCFParams%GridKind, &
                              SCFParams%GridPruning, &
                              .false., &
                              .true., &
                              THCGrid=THCGrid &
                              )
                  else
                        call scf_ConvergeOrbitals( &
                              SCFOutput%Rho_cao, &
                              SCFOutput%OrbEnergies, &
                              SCFOutput%Converged, &
                              SCFOutput%EtotDFT, &
                              SCFOutput%EelDFT, &
                              SCFOutput%ExcDFT, &
                              SCFOutput%Noao, &
                              SCFOutput%Ehomo, &
                              SCFOutput%Elumo, &
                              SCFOutput%Hbare_cao, &
                              SCFOutput%C_oao, &
                              SCFOutput%MOBasisVecsCart, &
                              SCFOutput%MOBasisVecsSpher, &
                              SCFOutput%NVirt, &
                              SCFOutput%AUXOut, &
                              XCModel, NonSCF, &
                              SCFOutput%NOcc, &
                              LinDepThresh, &
                              SCFOutput%Enucl, &
                              MaxRhoDiff, MaxOrbGrad, MaxNIters, &
                              SCFParams%ThreshFockJK, &
                              AUXIn, AOBasis, System, &
                              SCFParams%ECPFile, &
                              SCFParams%GridKind, &
                              SCFParams%GridPruning, &
                              .false., &
                              .false. &
                              )
                  end if
                  SCFOutput%Nexcluded = NAOCart - SCFOutput%Noao
                  ! ------------------------------------------------------------------------
                  !                ADDITIONAL NUMERICAL INTEGRALS ON THE GRID
                  ! ------------------------------------------------------------------------
                  if ((AUXInt_Type1 /= AUX_NONE) .or. (AUXInt_Type2 /= AUX_NONE)) then
                        if (ThisImage == 1) then
                              call scf_AuxIntegrals(SCFOutput%AUXOut, SCFOutput%Rho_cao, SCFParams, System)
                        end if
                  end if
                  if (DFT_DISP .ne. DISP_NONE) then
                        ! ------------------------------------------------------------------------
                        !                      POST-SCF DISPERSION CORRECTION
                        ! ------------------------------------------------------------------------
                        if (xcf_get_id(NonSCF) == XCF_XC_NONE) then
                              call scf_DispersionCorrection(SCFOutput%EdispDFT, XCModel, SCFOutput%AUXOut, System)
                        else
                              call scf_DispersionCorrection(SCFOutput%EdispDFT, NonSCF, SCFOutput%AUXOut, System)
                        end if
                        SCFOutput%EtotDFT = SCFOutput%EtotDFT + SCFOutput%EdispDFT
                        SCFOutput%EelDFT = SCFOutput%EtotDFT - SCFOutput%Enucl
                  else
                        SCFOutput%EdispDFT = ZERO
                  end if
                  ! -----------------------------------------------------------------------
                  !                   SAVE CONVERGED/NON-SCF DENSITY MATRIX
                  ! -----------------------------------------------------------------------
                  if (save_rho_mode .ne. FILEMODE_NONE) then
                        SavePathA = ""
                        SavePathB = ""
                        if (allocated(SCFParams%save_rhoa_path)) SavePathA = SCFParams%save_rhoa_path
                        if (allocated(SCFParams%save_rhob_path)) SavePathB = SCFParams%save_rhob_path
                        call scf_SaveDensityFile(SCFOutput%Rho_cao, save_rho_mode, SavePathA, SavePathB)
                  end if
            end associate
      end subroutine scf_driver_SpinUnres
end module real_scf
