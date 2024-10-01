module rpa_driver
      use arithmetic
      use math_constants
      use display
      use string
      use real_scf
      use scf_definitions
      use rpa_definitions
      use TensorHypercontraction
      use thc_definitions
      use OrbDiffHist
      use rpa
      use rpa_MeanField
      use basis_sets
      use fock
      use sys_definitions
      use ParallelCholesky
      use PostSCF
      use TwoStepCholesky_definitions

      implicit none

contains

      subroutine rpa_PostSCF(SCFOutput, SCFParams, AOBasis, RPAParams, System, CholeskyVecs, CholeskyBasis, THCGrid)
            !
            ! Driver subroutine for the post-SCF part of the RPA energy calculation. Includes
            ! the singles correction of Klimes et al. Works for the single-point energies of molecules
            ! as well as for interaction energies of complexes composed of two or three monomers.
            !
            ! 1. Klimes, J., Kaltak, M., Maggio, E., Kresse, G. J. Chem. Phys. 143, 102816 (2015);
            !    doi: 10.1063/1.4929346
            !
            type(TSCFOutput), dimension(:), intent(in)                :: SCFOutput
            type(TSCFParams), intent(in)                              :: SCFParams
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(inout)                           :: RPAParams
            type(TSystem), intent(inout)                              :: System
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: CholeskyVecs[:]
            type(TChol2Vecs), intent(inout)                           :: CholeskyBasis
            type(TCoulTHCGrid), intent(inout)                         :: THCGrid

            real(F64) :: EtotDFT_AB, EtotHF_AB, EtotRPA_AB, EcSingles_AB, EcRPA_AB, EcExchange_AB
            real(F64) :: EtotDFT_ABC, EtotHF_ABC, EtotRPA_ABC, EcSingles_ABC, EcRPA_ABC, EcExchange_ABC
            real(F64) :: EtotDFT_ABCD, EtotHF_ABCD, EtotRPA_ABCD, EcSingles_ABCD, EcRPA_ABCD, EcExchange_ABCD
            real(F64) :: EtotDFT_Nadd, EtotHF_Nadd, EtotRPA_Nadd, EcSingles_Nadd, EcRPA_Nadd, EcExchange_Nadd
            integer, parameter :: MaxNSubsystems = 15
            real(F64), dimension(MaxNSubsystems) :: EtotDFT, EtotRPA, EtotHF, EcSingles, EcRPA, EcExchange
            real(F64), dimension(MaxNSubsystems) :: EcRPA_Chi_MO, EcRPA_Chi_NO, EcRPA_T2_MO, EcRPA_T2_NO, EcRPA_T2_PNO
            integer :: TheoryLevel_NOBasis
            real(F64), dimension(:, :, :), allocatable :: NOBasis
            type(TRPAGrids) :: RPAGrids
            type(TRPABasis) :: RPABasis
            type(TMeanField), dimension(:), allocatable :: MeanFieldStates
            real(F64), dimension(:, :, :), allocatable :: RPABasisVecs[:]
            integer :: m, n, k, s, i
            integer :: NSpins, NSystems
            logical :: SpinUnres
            real(F64) :: DaiMaxThresh
            real(F64), dimension(:), allocatable :: EnergyDiffs
            real(F64), dimension(:, :), allocatable :: SinglePoints
            real(F64) :: T2CutoffCommonThresh
            logical :: FinishMacroLoop
            type(TClock) :: timer
            type(TRPAOutput), dimension(:), allocatable :: RPAOutput
            integer, parameter :: MaxMacroIters = 6

            allocate(EnergyDiffs(RPA_ENERGY_NCOMPONENTS))
            allocate(SinglePoints(RPA_ENERGY_NCOMPONENTS, MaxNSubsystems))
            if (System%SystemKind == SYS_MOLECULE) then
                  NSystems = 1
            else if (System%SystemKind == SYS_DIMER) then
                  NSystems = 3
            else if (System%SystemKind == SYS_TRIMER) then
                  NSystems = 7
            else ! Tetramer
                  NSystems = 15
            end if
            allocate(RPAOutput(NSystems))
            TheoryLevel_NOBasis = RPAParams%TheoryLevel
            !
            ! Initialize the cutoff threshold for discarding the eigenvectors of T2.
            ! To guarantee size consistent interaction energies, the same threshold
            ! should be applied to the supermolecule and to all of its subsystems.
            !
            ! A value < 0 means that T2CutoffCommon threshold is
            ! not initialized. Its value is defined during the supermolecule
            ! calculation, i.e., the energy calculation for k=1.
            !
            T2CutoffCommonThresh = -ONE
            SpinUnres = .false.
            n = 0
            do k = 1, NSystems
                  EtotDFT(k) = SCFOutput(k)%EtotDFT
                  NSpins = size(SCFOutput(k)%OrbEnergies, dim=2)
                  n = n + NSpins
                  SpinUnres = (NSpins>1)
            end do
            allocate(MeanFieldStates(NSystems))
            if (RPAParams%TensorHypercontraction) then
                  if (SCFParams%XCFunc == XCF_HF) then
                        call rpa_MeanField_RefineHF_Preamble(RPAParams, SCFParams)
                  end if
                  call clock_start(timer)
                  do k = 1, NSystems
                        call sys_Init(System, k)
                        if (SCFParams%XCFunc == XCF_HF) then
                              call rpa_MeanField_RefineHF(MeanFieldStates(k), SCFOutput(k), &
                                    SCFParams, RPAParams, AOBasis, System, THCGrid)
                        else                        
                              call rpa_MeanField_Semi(MeanFieldStates(k), SCFOutput(k), SCFParams, &
                                    RPAParams, AOBasis, System, THCGrid)
                        end if
                  end do
                  call msg("Mean-field calculation completed in " // str(clock_readwall(timer),d=1) // " seconds")
            end if
            allocate(RPAGrids%daiValues(RPA_HISTOGRAM_NBINS, n))
            allocate(RPAGrids%daiWeights(RPA_HISTOGRAM_NBINS, n))
            m = 1
            DaiMaxThresh = huge(ONE)
            do k = 1, NSystems
                  NSpins = size(SCFOutput(k)%OrbEnergies, dim=2)
                  if (k == 1 .and. RPAParams%GridLimitDai) then
                        !
                        ! Compute the maximum Ea-Ei difference
                        ! for the entire complex. That threshold
                        ! value is subsequently used to limit
                        ! the range of orbital excitations considered
                        ! during the grid optimization.
                        !
                        if (RPAParams%TensorHypercontraction) then
                              call rpa_DaiMaxThresh(DaiMaxThresh, &
                                    MeanFieldStates(k)%OrbEnergies, &
                                    MeanFieldStates(k)%NOcc, &
                                    MeanFieldStates(k)%NVirt, &
                                    MeanFieldStates(k)%NSpins, &
                                    RPAParams%CoreOrbThresh)
                        else
                              call rpa_DaiMaxThresh(DaiMaxThresh, &
                                    SCFOutput(k)%OrbEnergies, &
                                    SCFOutput(k)%NOcc, &
                                    SCFOutput(k)%NVirt, &
                                    NSpins, &
                                    RPAParams%CoreOrbThresh)
                        end if
                  end if
                  do s = 1, NSpins
                        if (RPAParams%TensorHypercontraction) then
                              call rpa_DaiHistogram( &
                                    RPAGrids%daiValues(:, m), &
                                    RPAGrids%daiWeights(:, m), &
                                    MeanFieldStates(k)%OrbEnergies(:, s), &
                                    MeanFieldStates(k)%NOcc(s), &
                                    MeanFieldStates(k)%NVirt(s), &
                                    RPAParams%CoreOrbThresh, &
                                    DaiMaxThresh)
                        else
                              call rpa_DaiHistogram( &
                                    RPAGrids%daiValues(:, m), &
                                    RPAGrids%daiWeights(:, m), &
                                    SCFOutput(k)%OrbEnergies(:, s), &
                                    SCFOutput(k)%NOcc(s), &
                                    SCFOutput(k)%NVirt(s), &
                                    RPAParams%CoreOrbThresh, &
                                    DaiMaxThresh)
                        end if
                        !
                        ! The index m runs over different subsystems, i.e., interacting molecules
                        ! as well as different spins if the calculation is open-shell
                        !
                        m = m + 1
                  end do
            end do            
            MacroIteration: do i = 1, MaxMacroIters
                  do k = 1, NSystems
                        call sys_Init(System, k)
                        call toprule()
                        call msg(cfield("RPA for " // sys_ChemicalFormula(System), 76))
                        call midrule()
                        call blankline()
                        if (SpinUnres) then
                              call msg("Using spin-unrestricted open-shell Kohn-Sham reference")
                        else
                              call msg("Using spin-restricted closed-shell Kohn-Sham reference")
                        end if
                        if (RPAParams%TensorHypercontraction) then
                              if (RPAParams%T2AuxOrbitals==RPA_AUX_NATURAL_ORBITALS .and. k > 1) then
                                    RPAParams%ComputeNaturalOrbitals = .true.
                                    RPAParams%TheoryLevel = RPA_THEORY_DIRECT_RING
                                    call rpa_THC_Etot(RPAOutput(k), MeanFieldStates(k), AOBasis, RPAParams, &
                                          RPAGrids, THCGrid, T2CutoffCommonThresh)
                                    EcRPA_T2_MO(k) = RPAOutput(k)%Energy(RPA_ENERGY_T2_DIRECT_RING)
                                    EcRPA_Chi_MO(k) = RPAOutput(k)%Energy(RPA_ENERGY_DIRECT_RING)
                                    call move_alloc(from=RPAOutput(k)%NaturalOrbitals, to=NOBasis)
                                    call rpa_ChangeOrbitalSpace(MeanFieldStates(k), NOBasis(:, :, 1))
                                    RPAParams%ComputeNaturalOrbitals = .false.
                                    RPAParams%TheoryLevel = TheoryLevel_NOBasis
                              end if
                                                            
                              call rpa_THC_Etot(RPAOutput(k), MeanFieldStates(k), AOBasis, RPAParams, &
                                    RPAGrids, THCGrid, T2CutoffCommonThresh)

                              EcRPA_T2_PNO(k) = RPAOutput(k)%Energy(RPA_ENERGY_PNO_DIRECT_RING)
                              EcRPA_T2_NO(k) = RPAOutput(k)%Energy(RPA_ENERGY_T2_DIRECT_RING)
                              EcRPA_Chi_NO(k) = RPAOutput(k)%Energy(RPA_ENERGY_DIRECT_RING)
                              if (RPAParams%T2AuxOrbitals==RPA_AUX_MOLECULAR_ORBITALS .or. &
                                    (RPAParams%T2AuxOrbitals==RPA_AUX_NATURAL_ORBITALS .and. k==1)) then
                                    EcRPA_T2_MO(k) = RPAOutput(k)%Energy(RPA_ENERGY_T2_DIRECT_RING)
                                    EcRPA_Chi_MO(k) = RPAOutput(k)%Energy(RPA_ENERGY_DIRECT_RING)
                              end if                              
                        else
                              if (RPAParams%CoupledClusters) then
                                    call rpa_CC_Etot(RPAOutput(k)%Energy, SCFOutput(k), AOBasis, RPAParams, &
                                          RPAGrids, RPABasisVecs, RPABasis, CholeskyVecs, CholeskyBasis, &
                                          SCFParams, System)
                              else
                                    call rpa_Etot(RPAOutput(k)%Energy, SCFOutput(k), SCFParams, AOBasis, &
                                          System, RPAParams, RPAGrids, RPABasisVecs, RPABasis, &
                                          CholeskyVecs, CholeskyBasis)
                              end if
                        end if
                  end do
                  FinishMacroLoop = .true.
                  if (RPAParams%TensorHypercontraction) then
                        if (RPAParams%TheoryLevel /= RPA_THEORY_DIRECT_RING) then
                              call rpa_SummaryOfErrors(EcRPA_Chi_MO, EcRPA_Chi_NO, EcRPA_T2_MO, &
                                    EcRPA_T2_NO, EcRPA_T2_PNO, RPAParams, System)
                              call rpa_EstimateT2EigenvalueError(FinishMacroLoop, RPAOutput, RPAParams, &
                                    System, T2CutoffCommonThresh, NSystems)
                              if (.not. FinishMacroLoop .and. i < MaxMacroIters .and. RPAParams%T2AdaptiveCutoff) then
                                    T2CutoffCommonThresh = T2CutoffCommonThresh / 10
                                    call blankline()
                                    call msg("Restarting RPA calculations with T2CutoffThresh = " // str(T2CutoffCommonThresh,d=1))
                                    call blankline()
                              end if
                        end if
                  end if
                  if (FinishMacroLoop) exit MacroIteration
            end do MacroIteration
            if (RPAParams%T2AuxOrbitals==RPA_AUX_NATURAL_ORBITALS) then
                  !
                  ! If the virual orbital space is truncated,
                  ! use the full-basis EcRPA contribution in the final energy
                  !
                  do k = 1, NSystems
                        RPAOutput(k)%Energy(RPA_ENERGY_DIRECT_RING) = EcRPA_Chi_MO(k)
                        call rpa_THC_GatherEnergyContribs(RPAOutput(k)%Energy, MeanFieldStates(k))
                  end do
            end if
            do k = 1, NSystems
                  EtotRPA(k) = RPAOutput(k)%Energy(RPA_ENERGY_TOTAL)
                  EtotHF(k) = RPAOutput(k)%Energy(RPA_ENERGY_HF)
                  EcSingles(k) = RPAOutput(k)%Energy(RPA_ENERGY_SINGLES)
                  EcRPA(k) = RPAOutput(k)%Energy(RPA_ENERGY_CORR)
                  EcExchange(k) = RPAOutput(k)%Energy(RPA_ENERGY_EXCHANGE)
                  SinglePoints(:, k) = RPAOutput(k)%Energy
                  SinglePoints(RPA_ENERGY_DFT, k) = EtotDFT(k)
            end do
            if (System%SystemKind == SYS_MOLECULE) then
                  if (RPAParams%CoupledClusters) then
                        call rpa_PrintEnergies(SinglePoints(:, 1), RPAParams, 1)
                  else
                        call msg("RPA Single-Point Energies (a.u.)", underline=.true.)

                        call msg(lfield("E(DFT)", 50) // lfield(str(EtotDFT(1), d=9), 20))
                        call msg(lfield("E(HF)", 50) // lfield(str(EtotHF(1), d=9), 20))
                        call msg(lfield("E(RPA singles)", 50) // lfield(str(EcSingles(1), d=9), 20))
                        call msg(lfield("E(RPA exchange)", 50) // lfield(str(EcExchange(1), d=9), 20))
                        call msg(lfield("E(RPA correlation)", 50) // lfield(str(EcRPA(1), d=9), 20))
                        call msg(lfield("E(RPA total)", 50) // lfield(str(EtotRPA(1), d=9), 20))
                  end if
            else if (System%SystemKind == SYS_DIMER) then
                  if (RPAParams%CoupledClusters) then
                        do k = 1, RPA_ENERGY_NCOMPONENTS
                              call rpa_Eint2Body(EnergyDiffs(k), SinglePoints(k, :))
                        end do
                        call rpa_PrintEnergies(EnergyDiffs, RPAParams, NSystems)
                  else
                        call msg("RPA 2-Body Interaction Energies (kcal/mol)", underline=.true.)

                        EtotDFT_AB = EtotDFT(SYS_TOTAL) - EtotDFT(SYS_MONO_A) - EtotDFT(SYS_MONO_B)
                        EtotRPA_AB = EtotRPA(SYS_TOTAL) - EtotRPA(SYS_MONO_A) - EtotRPA(SYS_MONO_B)
                        EtotHF_AB = EtotHF(SYS_TOTAL) - EtotHF(SYS_MONO_A) - EtotHF(SYS_MONO_B)
                        EcSingles_AB = EcSingles(SYS_TOTAL) - EcSingles(SYS_MONO_A) - EcSingles(SYS_MONO_B)
                        EcRPA_AB =  EcRPA(SYS_TOTAL) - EcRPA(SYS_MONO_A) - EcRPA(SYS_MONO_B)
                        EcExchange_AB = EcExchange(SYS_TOTAL) - EcExchange(SYS_MONO_A) - EcExchange(SYS_MONO_B)

                        call msg(lfield("Eint(DFT)", 30) // rfield(str(tokcal(EtotDFT_AB), d=6), 20))
                        call msg(lfield("Eint(HF)", 30) // rfield(str(tokcal(EtotHF_AB), d=6), 20))
                        call msg(lfield("Eint(RPA singles)", 30) // rfield(str(tokcal(EcSingles_AB), d=6), 20))
                        call msg(lfield("Eint(RPA exchange)", 30) // rfield(str(tokcal(EcExchange_AB), d=6), 20))
                        call msg(lfield("Eint(RPA correlation)", 30) // rfield(str(tokcal(EcRPA_AB), d=6), 20))
                        call msg(lfield("Eint(RPA total)", 30) // rfield(str(tokcal(EtotRPA_AB), d=6), 20))
                  end if
            else if (System%SystemKind == SYS_TRIMER) then
                  if (RPAParams%CoupledClusters) then
                        do k = 1, RPA_ENERGY_NCOMPONENTS
                              call rpa_EintNadd(EnergyDiffs(k), SinglePoints(k, :))
                        end do
                        call rpa_PrintEnergies(EnergyDiffs, RPAParams, NSystems)
                  else
                        call msg("RPA 3-Body Interaction Energies (kcal/mol)", underline=.true.)

                        EtotDFT_ABC = EtotDFT(SYS_TOTAL) - EtotDFT(SYS_MONO_A) - EtotDFT(SYS_MONO_B) - EtotDFT(SYS_MONO_C)
                        EtotRPA_ABC = EtotRPA(SYS_TOTAL) - EtotRPA(SYS_MONO_A) - EtotRPA(SYS_MONO_B) - EtotRPA(SYS_MONO_C)
                        EtotHF_ABC = EtotHF(SYS_TOTAL) - EtotHF(SYS_MONO_A) - EtotHF(SYS_MONO_B) - EtotHF(SYS_MONO_C)
                        EcSingles_ABC = EcSingles(SYS_TOTAL) - EcSingles(SYS_MONO_A) - EcSingles(SYS_MONO_B) - EcSingles(SYS_MONO_C)
                        EcRPA_ABC =  EcRPA(SYS_TOTAL) - EcRPA(SYS_MONO_A) - EcRPA(SYS_MONO_B) - EcRPA(SYS_MONO_C)
                        EcExchange_ABC =  EcExchange(SYS_TOTAL) - EcExchange(SYS_MONO_A) - EcExchange(SYS_MONO_B) - EcExchange(SYS_MONO_C)

                        call rpa_EintNadd(EtotDFT_Nadd, EtotDFT)
                        call rpa_EintNadd(EtotRPA_Nadd, EtotRPA)
                        call rpa_EintNadd(EtotHF_Nadd, EtotHF)
                        call rpa_EintNadd(EcSingles_Nadd, EcSingles)
                        call rpa_EintNadd(EcRPA_Nadd, EcRPA)
                        call rpa_EintNadd(EcExchange_Nadd, EcExchange)

                        call msg(lfield("EintABC(DFT)", 30) // rfield(str(tokcal(EtotDFT_ABC), d=6), 20))
                        call msg(lfield("EintABC(HF)", 30) // rfield(str(tokcal(EtotHF_ABC), d=6), 20))
                        call msg(lfield("EintABC(RPA singles)", 30) // rfield(str(tokcal(EcSingles_ABC), d=6), 20))
                        call msg(lfield("EintABC(RPA exchange)", 30) // rfield(str(tokcal(EcExchange_ABC), d=6), 20))
                        call msg(lfield("EintABC(RPA correlation)", 30) // rfield(str(tokcal(EcRPA_ABC), d=6), 20))
                        call msg(lfield("EintABC(RPA total)", 30) // rfield(str(tokcal(EtotRPA_ABC), d=6), 20))

                        call msg(lfield("EintNadd(DFT)", 30) // rfield(str(tokcal(EtotDFT_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(HF)", 30) // rfield(str(tokcal(EtotHF_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA singles)", 30) // rfield(str(tokcal(EcSingles_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA exchange)", 30) // rfield(str(tokcal(EcExchange_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA correlation)", 30) // rfield(str(tokcal(EcRPA_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA total)", 30) // rfield(str(tokcal(EtotRPA_Nadd), d=6), 20))
                  end if
            else ! Tetramer
                  if (RPAParams%CoupledClusters) then
                        do k = 1, RPA_ENERGY_NCOMPONENTS
                              call rpa_EintNadd4Body(EnergyDiffs(k), SinglePoints(k, :))
                        end do
                        call rpa_PrintEnergies(EnergyDiffs, RPAParams, NSystems)
                  else
                        call msg("RPA 4-Body Interaction Energies (kcal/mol)", underline=.true.)

                        EtotDFT_ABCD = EtotDFT(SYS_TOTAL) - EtotDFT(SYS_MONO_A) - EtotDFT(SYS_MONO_B) &
                              - EtotDFT(SYS_MONO_C) - EtotDFT(SYS_MONO_D)
                        EtotRPA_ABCD = EtotRPA(SYS_TOTAL) - EtotRPA(SYS_MONO_A) - EtotRPA(SYS_MONO_B) &
                              - EtotRPA(SYS_MONO_C) - EtotRPA(SYS_MONO_D)
                        EtotHF_ABCD = EtotHF(SYS_TOTAL) - EtotHF(SYS_MONO_A) - EtotHF(SYS_MONO_B) &
                              - EtotHF(SYS_MONO_C) - EtotHF(SYS_MONO_D)
                        EcSingles_ABCD = EcSingles(SYS_TOTAL) - EcSingles(SYS_MONO_A) - EcSingles(SYS_MONO_B) &
                              - EcSingles(SYS_MONO_C) - EcSingles(SYS_MONO_D)                  
                        EcRPA_ABCD =  EcRPA(SYS_TOTAL) - EcRPA(SYS_MONO_A) - EcRPA(SYS_MONO_B) &
                              - EcRPA(SYS_MONO_C) - EcRPA(SYS_MONO_D)
                        EcExchange_ABCD =  EcExchange(SYS_TOTAL) - EcExchange(SYS_MONO_A) - EcExchange(SYS_MONO_B) &
                              - EcExchange(SYS_MONO_C) - EcExchange(SYS_MONO_D)

                        call rpa_EintNadd4Body(EtotDFT_Nadd, EtotDFT)
                        call rpa_EintNadd4Body(EtotRPA_Nadd, EtotRPA)
                        call rpa_EintNadd4Body(EtotHF_Nadd, EtotHF)
                        call rpa_EintNadd4Body(EcSingles_Nadd, EcSingles)
                        call rpa_EintNadd4Body(EcRPA_Nadd, EcRPA)
                        call rpa_EintNadd4Body(EcExchange_Nadd, EcExchange)

                        call msg(lfield("EintABCD(DFT)", 30) // rfield(str(tokcal(EtotDFT_ABCD), d=6), 20))
                        call msg(lfield("EintABCD(HF)", 30) // rfield(str(tokcal(EtotHF_ABCD), d=6), 20))
                        call msg(lfield("EintABCD(RPA singles)", 30) // rfield(str(tokcal(EcSingles_ABCD), d=6), 20))
                        call msg(lfield("EintABCD(RPA exchange)", 30) // rfield(str(tokcal(EcExchange_ABCD), d=6), 20))
                        call msg(lfield("EintABCD(RPA correlation)", 30) // rfield(str(tokcal(EcRPA_ABCD), d=6), 20))
                        call msg(lfield("EintABCD(RPA total)", 30) // rfield(str(tokcal(EtotRPA_ABCD), d=6), 20))

                        call msg(lfield("EintNadd(DFT)", 30) // rfield(str(tokcal(EtotDFT_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(HF)", 30) // rfield(str(tokcal(EtotHF_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA singles)", 30) // rfield(str(tokcal(EcSingles_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA exchange)", 30) // rfield(str(tokcal(EcExchange_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA correlation)", 30) // rfield(str(tokcal(EcRPA_Nadd), d=6), 20))
                        call msg(lfield("EintNadd(RPA total)", 30) // rfield(str(tokcal(EtotRPA_Nadd), d=6), 20))
                  end if
            end if
            call blankline()
      end subroutine rpa_PostSCF


      subroutine rpa_Eint2Body(Eint, e)
            real(F64), intent(out)              :: Eint
            real(F64), dimension(:), intent(in) :: e

            Eint = e(SYS_TOTAL) - e(SYS_MONO_A) - e(SYS_MONO_B)
      end subroutine rpa_Eint2Body


      subroutine rpa_EintNadd(EintNadd, e)
            real(F64), intent(out)              :: EintNadd
            real(F64), dimension(:), intent(in) :: e

            real(F128) :: Eab, Ebc, Eac, Eabc

            Eabc = e(SYS_TOTAL) - e(SYS_MONO_A) - e(SYS_MONO_B) - e(SYS_MONO_C)
            Eab = e(SYS_DIMER_AB) - e(SYS_MONO_A) - e(SYS_MONO_B)
            Ebc = e(SYS_DIMER_BC) - e(SYS_MONO_B) - e(SYS_MONO_C)
            Eac = e(SYS_DIMER_AC) - e(SYS_MONO_A) - e(SYS_MONO_C)
            EintNadd = real(Eabc - Eab - Ebc - Eac, F64)
      end subroutine Rpa_EintNadd


      subroutine rpa_EintNadd4Body(EintNadd, e)
            real(F64), intent(out)              :: EintNadd
            real(F64), dimension(:), intent(in) :: e

            real(F128) :: Eabcd
            real(F128) :: Eab, Ebc, Eac, Ead, Ebd, Ecd
            real(F128) :: Eabc, Eabd, Eacd, Ebcd
            real(F128) :: EabcNadd, EabdNadd, EacdNadd, EbcdNadd

            Eabcd = e(SYS_TOTAL) - e(SYS_MONO_A) - e(SYS_MONO_B) - e(SYS_MONO_C) - e(SYS_MONO_D)

            Eabc = e(SYS_TRIMER_ABC) - e(SYS_MONO_A) - e(SYS_MONO_B) - e(SYS_MONO_C)
            Eabd = e(SYS_TRIMER_ABD) - e(SYS_MONO_A) - e(SYS_MONO_B) - e(SYS_MONO_D)
            Eacd = e(SYS_TRIMER_ACD) - e(SYS_MONO_A) - e(SYS_MONO_C) - e(SYS_MONO_D)
            Ebcd = e(SYS_TRIMER_BCD) - e(SYS_MONO_B) - e(SYS_MONO_C) - e(SYS_MONO_D)

            Eab = e(SYS_DIMER_AB) - e(SYS_MONO_A) - e(SYS_MONO_B)
            Ebc = e(SYS_DIMER_BC) - e(SYS_MONO_B) - e(SYS_MONO_C)
            Eac = e(SYS_DIMER_AC) - e(SYS_MONO_A) - e(SYS_MONO_C)
            Ead = e(SYS_DIMER_AD) - e(SYS_MONO_A) - e(SYS_MONO_D)
            Ebd = e(SYS_DIMER_BD) - e(SYS_MONO_B) - e(SYS_MONO_D)
            Ecd = e(SYS_DIMER_CD) - e(SYS_MONO_C) - e(SYS_MONO_D)

            EabcNadd = Eabc - Eab - Ebc - Eac
            EabdNadd = Eabd - Eab - Ead - Ebd
            EacdNadd = Eacd - Eac - Ead - Ecd
            EbcdNadd = Ebcd - Ebc - Ebd - Ecd

            EintNadd = real(Eabcd - Eab - Ebc - Eac - Ead - Ebd - Ecd &
                  - EabcNadd - EabdNadd - EacdNadd - EbcdNadd, F64)
      end subroutine Rpa_EintNadd4Body


      subroutine rpa_Etot(Energy, SCFOutput, SCFParams, AOBasis, System, RPAParams, RPAGrids, RPABasisVecs, &
            RPABasis, CholeskyVecs, CholeskyBasis)
            !
            ! Compute total random-phase approximation energy (EtotRPA) including the HF-like contribution
            ! (EtotHF=Enucl+Ekin+Ene+Ecoul+Eexch), the correction for singles (EcSingles, Eq. 33 in Ref. 1),
            ! and the direct random-phase correlation energy (EcRPA).
            !
            ! The direct RPA correlation, EcRPA, is computed using randomized trace estimation. The numerical
            ! precision is controlled by the set of thresholds defined in RPAParams.
            !
            ! The singles correction is computed using Eq. 33 in Ref. 1 instead of Eq. 32 in Ref. 1 to
            ! reduce the errors propagating from an imperfectly converged SCF.
            ! (The errors resulting from Eq. 32 would be significant, I checked that by
            ! performing HF SCF, for which EcSingles should be exactly zero.)
            !
            ! 1. Klimes, J., Kaltak, M., Maggio, E., Kresse, G. J. Chem. Phys. 143, 102816 (2015);
            !    doi: 10.1063/1.4929346
            !
            real(F64), dimension(:), intent(out)                      :: Energy
            type(TSCFOutput), intent(in)                              :: SCFOutput
            type(TSCFParams), intent(in)                              :: SCFParams
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TSystem), intent(in)                                 :: System
            type(TRPAParams), intent(in)                              :: RPAParams
            type(TRPAGrids), intent(inout)                            :: RPAGrids
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: RPABasisVecs[:]
            type(TRPABasis), intent(inout)                            :: RPABasis
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: CholeskyVecs[:]
            type(TChol2Vecs), intent(inout)                           :: CholeskyBasis

            integer :: NMO, NSpins, MaxNOcc, MaxNVirt
            type(txcdef) :: HFonDFT
            type(tclock) :: t_rpaexch
            type(tgriddiag) :: diag
            integer :: s
            real(F64) :: ExcDummy
            logical :: SpinUnres
            real(F64) :: OccNumber
            real(F64), dimension(1) :: AUXOut
            real(F64), dimension(1, 1) :: AUXIn
            real(F64), dimension(:, :, :), allocatable :: F_cao[:], F_sao[:]
            real(F64), dimension(:, :, :), allocatable :: Rho_sao
            real(F64), dimension(:, :), allocatable :: F_oao
            real(F64), dimension(:), allocatable :: TransfWork
            real(F64), dimension(:), allocatable :: HFEigenvals
            real(F64), dimension(:), allocatable :: BufferK
            real(F64), dimension(:), allocatable :: BufferJ
            real(F64), dimension(:), allocatable :: BufferRho1D
            real(F64), dimension(:, :, :), allocatable :: BufferTxc
            real(F64), dimension(:, :, :), allocatable :: OccCoeffs
            real(F64), dimension(:, :, :), allocatable :: VirtCoeffs
            real(F64), dimension(:, :), allocatable :: OccEnergies
            real(F64), dimension(:, :), allocatable :: VirtEnergies
            real(F64), dimension(:, :, :), allocatable :: F_ao
            integer :: DimJK, DimRho1D, DimTxc
            real(F64) :: EtotRPA, EtotHF, EcSingles, EcRPA
            real(F64) :: time_F
            integer :: ThisImage, NImages, NThreads

            ThisImage = this_image()
            NImages = num_images()
            Energy = ZERO

            associate ( &
                  C_oao => SCFOutput%C_oao, &
                  OrbEnergies => SCFOutput%OrbEnergies, &
                  MOBasisVecsCart => SCFOutput%MOBasisVecsCart, &
                  MOBasisVecsSpher => SCFOutput%MOBasisVecsSpher, &
                  Rho_cao => SCFOutput%Rho_cao, &
                  Hbare_cao => SCFOutput%Hbare_cao, &
                  Noao => SCFOutput%Noao, &
                  NOcc => SCFOutput%NOcc, &
                  NVirt => SCFOutput%NVirt, &
                  Enucl => SCFOutput%Enucl, &
                  NAOCart => AOBasis%NAOCart, &
                  NAOSpher => AOBasis%NAOSpher, &
                  SpherAO => AOBasis%SpherAO &
                  )
                  call clock_start(t_rpaexch)
                  call blankline()
                  call msg("Hartree-Fock contribution to RPA", underline=.true.)
                  NSpins = size(C_oao, dim=3)
                  MaxNOcc = maxval(NOcc(1:NSpins))
                  MaxNVirt = maxval(NVirt(1:NSpins))
                  NMO = Noao
                  if (NSpins > 1) then
                        SpinUnres = .true.
                        OccNumber = ONE
                  else
                        SpinUnres = .false.
                        OccNumber = TWO
                  end if
                  !
                  ! Define the Hartree-Fock Hamiltonian, built on DFT orbitals
                  !
                  call xcf_define(HFonDFT, XCF_HF, AUX_NONE, SpinUnres)
                  !
                  ! Build the exchange+Coulomb part of the Hartree-Fock Hamiltonian using
                  ! converged DFT orbitals (no DFT exchange-correlation)
                  !
                  call msg("Building Hartree-Fock Hamiltonian from DFT orbitals")
                  call msg("Threshold for J, K: |Rho(r,s)*(pq|rs)|,|Rho(q,s)*(pq|rs)| > " // str(SCFParams%ThreshFockJK,d=1))
                  allocate(F_cao(NAOCart, NAOCart, NSpins)[*])
                  if (SpherAO) then
                        allocate(F_sao(NAOSpher, NAOSpher, NSpins)[*])
                        allocate(Rho_sao(NAOSpher, NAOSpher, NSpins))
                        allocate(TransfWork(NAOSpher*NAOCart))
                        do s = 1, NSpins
                              call SpherGTO_TransformMatrix(Rho_sao(:, :, s), Rho_cao(:, :, s), &
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
                        end do
                  else
                        allocate(F_sao(1, 1, 1)[*])
                        allocate(Rho_sao(1, 1, 1))
                        allocate(TransfWork(NMO*NAOCart))
                  end if
                  call scf_BufferDim(DimTxc, DimJK, DimRho1D, NThreads, AOBasis)
                  allocate(BufferK(DimJK))
                  allocate(BufferJ(DimJK))
                  allocate(BufferRho1D(DimRho1D))
                  !
                  ! The scratch matrix for the xc potential won't be allocated
                  ! in its full size because only the Hartree-Fock hamiltonian
                  ! is requested
                  !
                  allocate(BufferTxc(1, 1, 1))
                  time_F = ZERO
                  call scf_F_RealRho(F_cao, F_sao, EtotHF, ExcDummy, diag, AUXOut, &
                        BufferTxc, BufferK, BufferJ, BufferRho1D, HFonDFT, &
                        Rho_cao, Rho_sao, Hbare_cao, AUXIn, AOBasis, System, &
                        SCFParams%ThreshFockJK, SCFParams%GridKind, SCFParams%GridPruning, time_F)
                  deallocate(BufferK, BufferJ, BufferRho1D, BufferTxc)
                  !
                  ! Hartree-Fock contribution based on DFT orbitals, i.e.,
                  ! without singles correction
                  !
                  EtotHF = EtotHF + Enucl
                  if (RPAParams%SinglesCorrection /= RPA_SINGLES_NONE .and. ThisImage == 1) then
                        call msg("Computing the singles correction:")
                        allocate(HFEigenvals(NMO))
                        allocate(F_oao(NMO, NMO))
                        EcSingles = ZERO
                        do s = 1, NSpins
                              call scf_TransformF(F_oao, F_cao(:, :, s), F_sao(:, :, s), MOBasisVecsCart, MOBasisVecsSpher, &
                                    NMO, NAOCart, NAOSpher, SpherAO, TransfWork)
                              if (RPAParams%SinglesCorrection == RPA_SINGLES_KLIMES) then
                                    if (s == 1) then
                                          call msg("Klimes et al.: Eq. 33 in J. Chem. Phys. 143, 102816 (2015); doi: 10.1063/1.4929346")
                                    end if
                                    call symmetric_eigenproblem(HFEigenvals, F_oao, NMO, .false.)
                                    !
                                    ! Eq. 33 in Ref. 1
                                    !
                                    if (SpherAO) then
                                          EcSingles = EcSingles + OccNumber * sum(HFEigenvals(1:NOcc(s))) &
                                                - fock_RhoTrace(Rho_cao(:, :, s), F_cao(:, :, s)) &
                                                - fock_RhoTrace(Rho_sao(:, :, s), F_sao(:, :, s))
                                    else
                                          EcSingles = EcSingles + OccNumber * sum(HFEigenvals(1:NOcc(s))) &
                                                - fock_RhoTrace(Rho_cao(:, :, s), F_cao(:, :, s))
                                    end if
                              else if (RPAParams%SinglesCorrection == RPA_SINGLES_REN) then
                                    if (s == 1) then
                                          call msg("Ren et al.: Eq. 25 in Phys. Rev. B 88, 035120 (2013); doi: 10.1103/PhysRevB.88.035120")
                                    end if
                                    call rpa_EcSingles_Ren2013(EcSingles, C_oao(:, :, s), F_oao, NOcc(s), NVirt(s))
                              end if
                        end do
                        if (RPAParams%SinglesCorrection == RPA_SINGLES_REN) then
                              EcSingles = OccNumber * EcSingles
                        end if
                        deallocate(HFEigenvals, F_oao)
                  else
                        EcSingles = ZERO
                  end if
                  deallocate(F_cao, F_sao, Rho_sao, TransfWork)
                  call msg("HF part of RPA completed in " // str(clock_readwall(t_rpaexch), d=1) // " seconds")
                  call msg(lfield("HF contribution (EtotHF)", 50) // lfield(str(EtotHF, d=10), 20))
                  if (RPAParams%SinglesCorrection) then
                        call msg(lfield("Singles correction (EcSingles)", 50) // lfield(str(EcSingles, d=10), 20))
                  end if
                  if (.not. RPAParams%DisableCorrelation) then
                        !
                        ! Compute DFT MO coefficients in the Cartesian AO basis
                        !
                        if (SpherAO) then
                              allocate(OccCoeffs(NAOSpher, MaxNOcc, NSpins))
                              allocate(VirtCoeffs(NAOSpher, MaxNVirt, NSpins))
                        else
                              allocate(OccCoeffs(NAOCart, MaxNOcc, NSpins))
                              allocate(VirtCoeffs(NAOCart, MaxNVirt, NSpins))
                        end if
                        allocate(OccEnergies(MaxNOcc, NSpins))
                        allocate(VirtEnergies(MaxNVirt, NSpins))
                        do s = 1, NSpins
                              if (NOcc(s) > 0) then
                                    if (SpherAO) then
                                          call real_ab(OccCoeffs(:, 1:NOcc(s), s), &
                                                MOBasisVecsSpher, C_oao(:, 1:NOcc(s), s))
                                          call real_ab(VirtCoeffs(:, 1:NVirt(s), s), MOBasisVecsSpher, &
                                                C_oao(:, NOcc(s)+1:NOcc(s)+NVirt(s), s))
                                    else
                                          call real_ab(OccCoeffs(:, 1:NOcc(s), s), &
                                                MOBasisVecsCart, C_oao(:, 1:NOcc(s), s))
                                          call real_ab(VirtCoeffs(:, 1:NVirt(s), s), MOBasisVecsCart, &
                                                C_oao(:, NOcc(s)+1:NOcc(s)+NVirt(s), s))
                                    end if
                                    OccEnergies(1:NOcc(s), s) = OrbEnergies(1:NOcc(s), s)
                                    VirtEnergies(1:NVirt(s), s) = OrbEnergies(NOcc(s)+1:NOcc(s)+NVirt(s), s)
                              else
                                    OccCoeffs(:, :, s) = ZERO
                                    VirtCoeffs(:, :, s) = ZERO
                                    OccEnergies(:, s) = ZERO
                                    VirtEnergies(:, s) = ZERO
                              end if
                        end do
                        allocate(F_ao(0, 0, 0))
                        call rpa_Ecorr_2(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
                              NOcc, NVirt, AOBasis, RPAParams, RPAGrids, RPABasisVecs, RPABasis, &
                              CholeskyVecs, CholeskyBasis, F_ao)
                  end if
                  EcRPA = Energy(RPA_ENERGY_CORR)
                  EtotRPA = EtotHF + EcSingles + EcRPA
                  call msg("RPA Single-Point Energies", underline=.true.)
                  call msg(lfield("HF contribution (EtotHF)", 50) // lfield(str(EtotHF, d=10), 20))
                  if (RPAParams%SinglesCorrection) then
                        call msg(lfield("Singles correction (EcSingles)", 50) // lfield(str(EcSingles, d=10), 20))
                        call msg(lfield("Direct RPA correlation (EcRPA)", 50) // lfield(str(EcRPA, d=10), 20))
                        call msg(lfield("Total energy (EtotRPA=EtotHF+EcSingles+EcRPA)", 50) // lfield(str(EtotRPA, d=10), 20))
                  else
                        call msg(lfield("Direct RPA correlation (EcRPA)", 50) // lfield(str(EcRPA, d=10), 20))
                        call msg(lfield("Total energy (EtotRPA=EtotHF+EcRPA)", 50) // lfield(str(EtotRPA, d=10), 20))
                  end if
                  call blankline()
            end associate
            Energy(RPA_ENERGY_TOTAL) = EtotRPA
            Energy(RPA_ENERGY_HF) = EtotHF
            Energy(RPA_ENERGY_SINGLES) = EcSingles
            if (NImages > 1) then
                  call co_broadcast(Energy, source_image=1)
            end if
      end subroutine rpa_Etot


      subroutine rpa_EcSingles_Ren2013(EcSingles, C_oao, F_oao, NOcc, NVirt)
            real(F64), intent(inout)               :: EcSingles
            real(F64), dimension(:, :), intent(in) :: C_oao
            real(F64), dimension(:, :), intent(in) :: F_oao
            integer, intent(in)                    :: NOcc
            integer, intent(in)                    :: NVirt

            integer :: i0, i1, a0, a1, a, i, NMO
            real(F64), dimension(:, :), allocatable :: Fij, Fab, Fai
            real(F64), dimension(:, :), allocatable :: OccVecs, VirtVecs
            real(F64), dimension(:), allocatable :: OccEnergies, VirtEnergies
            real(F64), dimension(:, :), allocatable :: TransfWork

            NMO = size(F_oao, dim=1)
            allocate(OccEnergies(NOcc))
            allocate(VirtEnergies(NVirt))
            allocate(OccVecs(NMO, NOcc))
            allocate(VirtVecs(NMO, NVirt))
            allocate(Fij(NOcc, NOcc))
            allocate(Fab(NVirt, NVirt))
            allocate(Fai(NVirt, NOcc))
            allocate(TransfWork(NMO, max(NOcc, NVirt)))
            i0 = 1
            i1 = NOcc
            a0 = NOcc + 1
            a1 = NOcc + NVirt
            call real_ab(TransfWork(:, 1:NOcc), F_oao, C_oao(:, i0:i1))
            call real_aTb(Fij, C_oao(:, i0:i1), TransfWork(:, 1:Nocc))
            call real_ab(TransfWork(:, 1:NVirt), F_oao, C_oao(:, a0:a1))
            call real_aTb(Fab, C_oao(:, a0:a1), TransfWork(:, 1:NVirt))
            call symmetric_eigenproblem(OccEnergies, Fij, NOcc, .true.)
            call symmetric_eigenproblem(VirtEnergies, Fab, NVirt, .true.)
            call real_ab(OccVecs, C_oao(:, i0:i1), Fij)
            call real_ab(VirtVecs, C_oao(:, a0:a1), Fab)
            call real_ab(TransfWork(:, 1:NOcc), F_oao, OccVecs)
            call real_aTb(Fai, VirtVecs, TransfWork(:, 1:NOcc))
            do i = 1, NOcc
                  do a = 1, NVirt
                        EcSingles = EcSingles + Fai(a, i)**2 / (OccEnergies(i) - VirtEnergies(a))
                  end do
            end do
      end subroutine rpa_EcSingles_Ren2013


      subroutine rpa_CC_Etot(Energy, SCFOutput, AOBasis, RPAParams, RPAGrids, RPABasisVecs, &
            RPABasis, CholeskyVecs, CholeskyBasis, SCFParams, System)

            real(F64), dimension(:), intent(out)                      :: Energy
            type(TSCFOutput), intent(in)                              :: SCFOutput
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(in)                              :: RPAParams
            type(TRPAGrids), intent(inout)                            :: RPAGrids
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: RPABasisVecs[:]
            type(TRPABasis), intent(inout)                            :: RPABasis
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: CholeskyVecs[:]
            type(TChol2Vecs), intent(inout)                           :: CholeskyBasis
            type(TSCFParams), intent(in)                              :: SCFParams
            type(TSystem), intent(in)                                 :: System

            integer :: NAO, NSpins, s
            real(F64) :: Etot
            real(F64) :: EtotHF, EhfTwoEl, EHbare, Enucl
            real(F64), dimension(:, :, :), allocatable :: OccCoeffs_ao, VirtCoeffs_ao, F_ao
            real(F64), dimension(:, :, :), allocatable :: Rho_ao
            real(F64), dimension(:, :), allocatable :: OccEnergies, VirtEnergies
            real(F64) :: time_F
            integer :: ThisImage

            ThisImage = this_image()
            Energy = ZERO
            associate ( &
                  NOcc => SCFOutput%NOcc, &
                  NVirt => SCFOutput%NVirt, &
                  C_oao => SCFOutput%C_oao, &
                  Noao => SCFOutput%Noao, &
                  NAOCart => AOBasis%NAOCart, &
                  NAOSpher => AOBasis%NAOSpher, &
                  SpherAO => AOBasis%SpherAO, &
                  OrbEnergies => SCFOutput%OrbEnergies, &
                  MOBasisVecsCart => SCFOutput%MOBasisVecsCart, &
                  MOBasisVecsSpher => SCFOutput%MOBasisVecsSpher &
                  )
                  NSpins = size(OrbEnergies, dim=2)
                  if (SpherAO) then
                        NAO = NAOSpher
                        call postscf_Rho(OccCoeffs_ao, VirtCoeffs_ao, Rho_ao, C_oao, MOBasisVecsSpher, &
                              NOcc, NVirt)
                  else
                        NAO = NAOCart
                        call postscf_Rho(OccCoeffs_ao, VirtCoeffs_ao, Rho_ao, C_oao, MOBasisVecsCart, &
                              NOcc, NVirt)
                  end if
                  allocate(F_ao(NAO, NAO, NSpins))
                  call postscf_FullFockMatrix(F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao, &
                        SCFParams, System, AOBasis, time_F)
                  Energy(RPA_ENERGY_HF) = EtotHF
                  if (.not. RPAParams%DisableCorrelation) then
                        allocate(OccEnergies(max(NOcc(1), NOcc(2)), NSpins))
                        allocate(VirtEnergies(max(NVirt(1), NVirt(2)), NSpins))
                        do s = 1, NSpins
                              OccEnergies(1:NOcc(s), s) = OrbEnergies(1:NOcc(s), s)
                              VirtEnergies(1:NVirt(s), s)= OrbEnergies(NOcc(s)+1:NOcc(s)+NVirt(s), s)
                        end do
                        call rpa_Ecorr_2(Energy, OccCoeffs_ao, VirtCoeffs_ao, OccEnergies, VirtEnergies, &
                              NOcc, NVirt, AOBasis, RPAParams, RPAGrids, RPABasisVecs, RPABasis, &
                              CholeskyVecs, CholeskyBasis, F_ao)
                  end if
            end associate
            Etot = Energy(RPA_ENERGY_HF) + &
                  Energy(RPA_ENERGY_1RDM_LINEAR) + &
                  Energy(RPA_ENERGY_1RDM_QUADRATIC) + &
                  Energy(RPA_ENERGY_CORR)
            Energy(RPA_ENERGY_TOTAL) = Etot
            call msg("Single-Point Energies (a.u.)", underline=.true.)
            if (RPAParams%TensorHypercontraction) then
                  call msg(lfield("mean field", 40) //        rfield(str(Energy(RPA_ENERGY_HF), d=8), 20))
                  call msg(lfield("1-RDM linear", 40) //       rfield(str(Energy(RPA_ENERGY_1RDM_LINEAR), d=8), 20))
                  call msg(lfield("1-RDM quadratic", 40) //    rfield(str(Energy(RPA_ENERGY_1RDM_QUADRATIC), d=8), 20))
                  call msg(lfield("direct ring", 40) //       rfield(str(Energy(RPA_ENERGY_DIRECT_RING), d=8), 20))
                  call msg(lfield("cumulant 1b/SOSEX", 40) // rfield(str(Energy(RPA_ENERGY_CUMULANT_1B), d=8), 20))
                  call msg(lfield("cumulant 2g/MBPT3", 40) // rfield(str(Energy(RPA_ENERGY_CUMULANT_2G), d=8), 20))
                  call msg(lfield("total energy", 40) //      rfield(str(Energy(RPA_ENERGY_TOTAL), d=8), 20))
            else
                  call msg(lfield("mean field", 40) //        rfield(str(Energy(RPA_ENERGY_HF), d=8), 20))
                  call msg(lfield("1-RDM linear", 40) //       rfield(str(Energy(RPA_ENERGY_1RDM_LINEAR), d=8), 20))
                  call msg(lfield("1-RDM quadratic", 40) //    rfield(str(Energy(RPA_ENERGY_1RDM_QUADRATIC), d=8), 20))
                  call msg(lfield("direct ring", 40) //       rfield(str(Energy(RPA_ENERGY_DIRECT_RING), d=8), 20))
                  call msg(lfield("exchange", 40) //          rfield(str(Energy(RPA_ENERGY_EXCHANGE), d=8), 20))
                  call msg(lfield("total energy", 40) //      rfield(str(Energy(RPA_ENERGY_TOTAL), d=8), 20))
            end if
            call blankline()            
            call co_broadcast(Energy, source_image=1)
      end subroutine rpa_CC_Etot


      subroutine rpa_THC_Etot(RPAOutput, MeanField, AOBasis, RPAParams, RPAGrids, THCGrid, &
            T2CutoffCommonThresh)
            
            type(TRPAOutput), intent(out)                             :: RPAOutput
            type(TMeanField), intent(in)                              :: MeanField
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(in)                              :: RPAParams
            type(TRPAGrids), intent(inout)                            :: RPAGrids
            type(TCoulTHCGrid), intent(inout)                         :: THCGrid
            real(F64), intent(inout)                                  :: T2CutoffCommonThresh
            
            real(F64), dimension(:, :), allocatable :: OccEnergies, VirtEnergies
            integer :: s

            RPAOutput%Energy = ZERO
            associate ( &
                  NOcc => MeanField%NOcc, &
                  NVirt => MeanField%NVirt, &
                  NSpins => MeanField%NSpins, &
                  NAOCart => AOBasis%NAOCart, &
                  NAOSpher => AOBasis%NAOSpher, &
                  OrbEnergies => MeanField%OrbEnergies, &
                  OccCoeffs_ao => MeanField%OccCoeffs_ao, &
                  VirtCoeffs_ao => MeanField%VirtCoeffs_ao, &
                  F_ao => MeanField%F_ao &
                  )
                  if (.not. RPAParams%DisableCorrelation) then
                        allocate(OccEnergies(max(NOcc(1), NOcc(2)), NSpins))
                        allocate(VirtEnergies(max(NVirt(1), NVirt(2)), NSpins))
                        do s = 1, NSpins
                              OccEnergies(1:NOcc(s), s) = OrbEnergies(1:NOcc(s), s)
                              VirtEnergies(1:NVirt(s), s)= OrbEnergies(NOcc(s)+1:NOcc(s)+NVirt(s), s)
                        end do
                        call rpa_THC_Ecorr_2(RPAOutput, OccCoeffs_ao, VirtCoeffs_ao, OccEnergies, VirtEnergies, &
                              F_ao, NOcc, NVirt, AOBasis, RPAParams, RPAGrids, THCGrid, &
                              T2CutoffCommonThresh)
                  end if
            end associate
            call rpa_THC_GatherEnergyContribs(RPAOutput%Energy, MeanField)
            associate (Energy => RPAOutput%Energy)
                  call msg("Single-Point Energies (a.u.)", underline=.true.)
                  if (RPAParams%PT_Order2) then
                        call msg(lfield("MP2 singlet pairs", 40) //    rfield(str(Energy(MP2_ENERGY_SINGLET_PAIR), d=8), 20))
                        call msg(lfield("MP2 triplet pairs", 40) //    rfield(str(Energy(MP2_ENERGY_TRIPLET_PAIR), d=8), 20))
                        call msg(lfield("direct MP2", 40) //           rfield(str(Energy(MP2_ENERGY_DIRECT), d=8), 20))
                        call msg(lfield("total MP2", 40) //            rfield(str(Energy(MP2_ENERGY_TOTAL), d=8), 20))
                  end if
                  if (RPAParams%PT_Order3) then
                        call msg(lfield("MP3 A", 40) // rfield(str(Energy(MP3_ENERGY_A), d=8), 20))
                        call msg(lfield("MP3 B", 40) // rfield(str(Energy(MP3_ENERGY_B), d=8), 20))
                        call msg(lfield("MP3 C", 40) // rfield(str(Energy(MP3_ENERGY_C), d=8), 20))
                        call msg(lfield("MP3 D", 40) // rfield(str(Energy(MP3_ENERGY_D), d=8), 20))
                        call msg(lfield("MP3 E", 40) // rfield(str(Energy(MP3_ENERGY_E), d=8), 20))
                        call msg(lfield("MP3 F", 40) // rfield(str(Energy(MP3_ENERGY_F), d=8), 20))
                        call msg(lfield("MP3 G", 40) // rfield(str(Energy(MP3_ENERGY_G), d=8), 20))
                        call msg(lfield("MP3 H", 40) // rfield(str(Energy(MP3_ENERGY_H), d=8), 20))
                        call msg(lfield("MP3 I", 40) // rfield(str(Energy(MP3_ENERGY_I), d=8), 20))
                        call msg(lfield("MP3 J", 40) // rfield(str(Energy(MP3_ENERGY_J), d=8), 20))
                        call msg(lfield("MP3 K", 40) // rfield(str(Energy(MP3_ENERGY_K), d=8), 20))
                        call msg(lfield("MP3 L", 40) // rfield(str(Energy(MP3_ENERGY_L), d=8), 20))
                        call msg(lfield("total MP3", 40) // rfield(str(Energy(MP3_ENERGY_TOTAL), d=8), 20))
                  end if
                  call msg(lfield("mean field", 40) //        rfield(str(Energy(RPA_ENERGY_HF), d=8), 20))
                  call msg(lfield("1-RDM linear", 40) //      rfield(str(Energy(RPA_ENERGY_1RDM_LINEAR), d=8), 20))
                  call msg(lfield("1-RDM quadratic", 40) //   rfield(str(Energy(RPA_ENERGY_1RDM_QUADRATIC), d=8), 20))
                  call msg(lfield("direct ring", 40) //       rfield(str(Energy(RPA_ENERGY_DIRECT_RING), d=8), 20))
                  call msg(lfield("cumulant 1b/SOSEX", 40) // rfield(str(Energy(RPA_ENERGY_CUMULANT_1B), d=8), 20))
                  call msg(lfield("cumulant 2g/MBPT3", 40) // rfield(str(Energy(RPA_ENERGY_CUMULANT_2G), d=8), 20))
                  call msg(lfield("total energy", 40) //      rfield(str(Energy(RPA_ENERGY_TOTAL), d=8), 20))                  
                  call blankline()            
            end associate
      end subroutine rpa_THC_Etot


      subroutine rpa_THC_GatherEnergyContribs(Energy, MeanField)
            real(F64), dimension(:), intent(inout) :: Energy
            type(TMeanField), intent(in)           :: MeanField

            Energy(RPA_ENERGY_HF) = MeanField%EtotHF
            Energy(RPA_ENERGY_1RDM_LINEAR) = MeanField%Ec1RDM_Linear
            Energy(RPA_ENERGY_1RDM_QUADRATIC) = MeanField%Ec1RDM_Quadratic
            Energy(RPA_ENERGY_SINGLES) = Energy(RPA_ENERGY_1RDM_LINEAR) + Energy(RPA_ENERGY_1RDM_QUADRATIC)
            Energy(RPA_ENERGY_CORR) = sum(Energy(RPA_CORRELATION_TERMS(1):RPA_CORRELATION_TERMS(2)))
            Energy(RPA_ENERGY_TOTAL) = &
                  Energy(RPA_ENERGY_HF) + &
                  Energy(RPA_ENERGY_1RDM_LINEAR) + &
                  Energy(RPA_ENERGY_1RDM_QUADRATIC) + &
                  Energy(RPA_ENERGY_CORR)
      end subroutine rpa_THC_GatherEnergyContribs


      subroutine rpa_EstimateT2EigenvalueError(SatisfiedAccuracyTarget, RPAOutput, RPAParams, &
            System, CutoffThresh, NSystems)
            
            logical, intent(out)                          :: SatisfiedAccuracyTarget
            type(TRPAOutput), dimension(:), intent(in)    :: RPAOutput
            type(TRPAParams), intent(in)                  :: RPAParams
            type(TSystem), intent(in)                     :: System
            real(F64), intent(in)                         :: CutoffThresh
            integer, intent(in)                           :: NSystems

            integer, parameter :: MaxNSystems = 15
            real(F64), dimension(MaxNSystems) :: EcRPA, Ec2g, EcSOSEX
            real(F64) :: DintRPA, Dint2g, DintSOSEX
            real(F64) :: RelDintRPA, RelDint2g, RelDintSOSEX
            integer, parameter :: NAltCutoffs = 10
            real(F64), dimension(0:NAltCutoffs) :: EintRPA, Eint2g, EintSOSEX
            integer :: i
            real(F64), parameter :: CutoffScaling = 1.1_F64
            real(F64), dimension(0:NAltCutoffs) :: AltCutoffs
            real(F64) :: Delta
            character(:), allocatable :: line

            SatisfiedAccuracyTarget = .true.
            if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023 .or. &
                  RPAParams%TheoryLevel == RPA_THEORY_JCTC2024) then
                  continue
            else
                  return
            end if
            Delta = CutoffThresh * (CutoffScaling - ONE) / NAltCutoffs
            AltCutoffs(0) = CutoffThresh
            do i = 1, NAltCutoffs
                  AltCutoffs(i) = CutoffThresh * CutoffScaling - (i - 1) * Delta
            end do
            if ( &
                  System%SystemKind == SYS_DIMER .or. &
                  System%SystemKind == SYS_TRIMER .or. &
                  System%SystemKind == SYS_TETRAMER &
                  ) then
                  call blankline()
                  call msg("sensitivity to perturbations of T2CutoffThresh")
                  call msg("all values in kcal/mol")
                  call blankline()
                  if (System%SystemKind == SYS_DIMER) then
                        line = lfield("T2CutoffThresh", 17) // rfield("Eint(direct ring)", 20)
                        if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                              line = line // rfield("Eint(2g)", 20) // rfield("Eint(SOSEX)", 20)
                        end if
                  else
                        line = lfield("T2CutoffThresh", 17) // rfield("EintNadd(direct ring)", 20)
                        if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                              line = line // rfield("EintNadd(2g)", 20) // rfield("EintNadd(SOSEX)", 20)
                        end if
                  end if
                  call msg(line)
                  do i = NAltCutoffs, 0, -1                        
                        call E_AltCutoff(EcRPA, Ec2g, EcSOSEX, RPAOutput, RPAParams, NSystems, AltCutoffs(i))
                        select case (System%SystemKind)
                        case (SYS_DIMER)
                              call rpa_Eint2Body(EintRPA(i), EcRPA)
                              if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                                    call rpa_Eint2Body(Eint2g(i), Ec2g)
                                    call rpa_Eint2Body(EintSOSEX(i), EcSOSEX)
                              end if
                        case (SYS_TRIMER)
                              call rpa_EintNadd(EintRPA(i), EcRPA)
                              if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                                    call rpa_EintNadd(Eint2g(i), Ec2g)
                                    call rpa_EintNadd(EintSOSEX(i), EcSOSEX)
                              end if
                        case (SYS_TETRAMER)
                              call rpa_EintNadd4Body(EintRPA(i), EcRPA)
                              if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                                    call rpa_EintNadd4Body(Eint2g(i), Ec2g)
                                    call rpa_EintNadd4Body(EintSOSEX(i), EcSOSEX)
                              end if
                        end select
                        line = lfield(str(AltCutoffs(i),d=3), 17) // rfield(str(tokcal(EintRPA(i)),d=6), 20)
                        if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                              line = line // rfield(str(tokcal(Eint2g(i)),d=6), 20) // rfield(str(tokcal(EintSOSEX(i)),d=6), 20)
                        end if
                        call msg(line)
                  end do
                  DintRPA = abs(maxval(EintRPA) - minval(EintRPA))
                  RelDintRPA = DintRPA / abs(EintRPA(0))
                  if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                        Dint2g = abs(maxval(Eint2g) - minval(Eint2g))
                        DintSOSEX = abs(maxval(EintSOSEX) - minval(EintSOSEX))
                        RelDint2g = Dint2g / abs(Eint2g(0))
                        RelDintSOSEX = DintSOSEX / abs(EintSOSEX(0))
                  else
                        Dint2g = ZERO
                        DintSOSEX = ZERO
                        RelDint2g = ZERO
                        RelDintSOSEX = ZERO
                  end if
                  line = lfield("max abs diff", 17) // rfield(str(tokcal(DintRPA),d=1), 20)
                  if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                        line = line // rfield(str(tokcal(Dint2g),d=1), 20) // rfield(str(tokcal(DintSOSEX),d=1), 20)
                  end if
                  call msg(line)
                  line = lfield("max rel diff", 17) // rfield(str(RelDintRPA,d=1), 20)
                  if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                        line = line // rfield(str(RelDint2g,d=1), 20) // rfield(str(RelDintSOSEX,d=1), 20)
                  end if
                  call msg(line)
                  if (RPAParams%T2AdaptiveCutoff) then
                        if ( &
                              tokcal(max(DintRPA, Dint2g, DintSOSEX)) < RPAParams%T2AdaptiveCutoffTargetKcal) then
                              SatisfiedAccuracyTarget = .true.
                              call msg("Error due to T2 eigenvalue cutoff is within the acceptable range of " &
                                    // str(RPAParams%T2AdaptiveCutoffTargetKcal,d=1) // " kcal/mol")
                        else
                              SatisfiedAccuracyTarget = .false.
                              call msg("Error due to T2 eigenvalue cutoff exceeds the target range of " &
                                    // str(RPAParams%T2AdaptiveCutoffTargetKcal,d=1) // " kcal/mol")
                        end if
                  else
                        SatisfiedAccuracyTarget = .true.
                  end if
                  call blankline()
            end if

      contains

            subroutine E_AltCutoff(EcRPA, Ec2g, EcSOSEX, RPAOutput, RPAParams, NSystems, CutoffThresh)
                  real(F64), dimension(:), intent(out)        :: EcRPA
                  real(F64), dimension(:), intent(out)        :: Ec2g
                  real(F64), dimension(:), intent(out)        :: EcSOSEX
                  type(TRPAOutput), dimension(:), intent(in)  :: RPAOutput
                  type(TRPAParams), intent(in)                :: RPAParams
                  integer, intent(in)                         :: NSystems
                  real(F64), intent(in)                       :: CutoffThresh

                  integer, dimension(MaxNSystems) :: NVecsT2
                  integer :: k, mu

                  EcRPA = ZERO
                  Ec2g = ZERO
                  EcSOSEX = ZERO
                  NVecsT2 = 0
                  do k = 1, NSystems
                        do mu = 1, RPAOutput(k)%NVecsT2
                              if (Abs(RPAOutput(k)%Am(mu)) > CutoffThresh) then
                                    NVecsT2(k) = mu
                              else
                                    exit
                              end if
                        end do
                        if (NVecsT2(k) > 0) then
                              EcRPA(k) = sum(RPAOutput(k)%EigRPA(1:NVecsT2(k)))
                              if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                                    Ec2g(k) = sum(RPAOutput(k)%Eig2g(1:NVecsT2(k)))
                                    EcSOSEX(k) = sum(RPAOutput(k)%EigSOSEX(1:NVecsT2(k)))
                              end if
                        end if
                  end do
            end subroutine E_AltCutoff
      end subroutine rpa_EstimateT2EigenvalueError


      subroutine rpa_EstimateEcRPAError(Error, RefVal, ApproxVal, EcRPA_Reference, EcRPA_Approx, System)
            real(F64), intent(out)                        :: Error
            real(F64), intent(out)                        :: RefVal, ApproxVal
            real(F64), dimension(:), intent(in)           :: EcRPA_Reference
            real(F64), dimension(:), intent(in)           :: EcRPA_Approx
            type(TSystem), intent(in)                     :: System
            
            real(F64) :: EintRPA_Reference, EintRPA_Approx        

            if ( &
                  System%SystemKind == SYS_DIMER .or. &
                  System%SystemKind == SYS_TRIMER .or. &
                  System%SystemKind == SYS_TETRAMER &
                  ) then
                  select case (System%SystemKind)
                  case (SYS_DIMER)
                        call rpa_Eint2Body(EintRPA_Reference, EcRPA_Reference)
                  case (SYS_TRIMER)
                        call rpa_EintNadd(EintRPA_Reference, EcRPA_Reference)
                  case (SYS_TETRAMER)
                        call rpa_EintNadd4Body(EintRPA_Reference, EcRPA_Reference)
                  end select
                  select case (System%SystemKind)
                  case (SYS_DIMER)
                        call rpa_Eint2Body(EintRPA_Approx, EcRPA_Approx)
                  case (SYS_TRIMER)
                        call rpa_EintNadd(EintRPA_Approx, EcRPA_Approx)
                  case (SYS_TETRAMER)
                        call rpa_EintNadd4Body(EintRPA_Approx, EcRPA_Approx)
                  end select
                  RefVal = tokcal(EintRPA_Reference)
                  ApproxVal = tokcal(EintRPA_Approx)
                  Error = tokcal(EintRPA_Approx - EintRPA_Reference)
            else
                  RefVal = EcRPA_Reference(1)
                  ApproxVal = EcRPA_Approx(1)
                  Error = EcRPA_Approx(1) - EcRPA_Reference(1)
            end if
      end subroutine rpa_EstimateEcRPAError


      subroutine rpa_SummaryOfErrors(EcRPA_Chi_MO, EcRPA_Chi_NO, EcRPA_T2_MO, &
            EcRPA_T2_NO, EcRPA_T2_PNO, RPAParams, System)
            
            real(F64), dimension(:), intent(in) :: EcRPA_Chi_MO
            real(F64), dimension(:), intent(in) :: EcRPA_Chi_NO
            real(F64), dimension(:), intent(in) :: EcRPA_T2_MO
            real(F64), dimension(:), intent(in) :: EcRPA_T2_NO
            real(F64), dimension(:), intent(in) :: EcRPA_T2_PNO
            type(TRPAParams), intent(in)        :: RPAParams
            type(TSystem), intent(in)           :: System
            
            real(F64) :: Error_NO, Error_T2, Error_PNO
            real(F64) :: Chi_MO, Chi_NO, T2_NO, T2_PNO

            call rpa_EstimateEcRPAError(Error_NO, Chi_MO, Chi_NO, EcRPA_Chi_MO, EcRPA_Chi_NO, System)
            call rpa_EstimateEcRPAError(Error_T2, Chi_NO, T2_NO, EcRPA_Chi_NO, EcRPA_T2_NO, System)
            if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2024) then
                  call rpa_EstimateEcRPAError(Error_PNO, T2_NO, T2_PNO, EcRPA_T2_NO, EcRPA_T2_PNO, System)
            end if            
            call midrule()
            call msg(cfield("Errors due to numerical approximations", 76))
            call midrule()
            if (System%SystemKind == SYS_MOLECULE) then
                  call msg("EcRPA single-point energies and errors are in a.u.")
                  call msg(lfield("", 30)    // rfield("energy", 20)  // rfield("approx-ref", 20))
            else
                  if (System%SystemKind == SYS_DIMER) then
                        call msg("EcRPA interaction energies and errors are in kcal/mol")
                  else
                        call msg("EcRPA nonadditive interaction energies and errors are in kcal/mol")
                  end if
                  call msg("error for each approximation is defined as energy(i)-energy(i-1)")
                  call blankline()
                  call msg(lfield("i", 3) // lfield("", 30) // &
                        rfield("direct-ring energy", 20)  // rfield("error", 20))
            end if
            call msg(lfield("1", 3) // lfield("accurate", 30) // rfield(str(Chi_MO,d=6), 20))
            call msg(lfield("2", 3) //lfield("natural orbitals", 30) // &
                  rfield(str(Chi_NO,d=6),20) // rfield(str(Error_NO, d=1), 20))
            call msg(lfield("3", 3) //lfield("eigendecomposition of T2", 30) // &
                  rfield(str(T2_NO,d=6),20) // rfield(str(Error_T2,d=1), 20))
            if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2024) then
                  call msg(lfield("4", 3) // lfield("pair-natural orbitals", 30) // &
                        rfield(str(T2_PNO,d=6),20) // rfield(str(Error_PNO,d=1), 20))
            end if
            call blankline()
      end subroutine rpa_SummaryOfErrors


      subroutine rpa_PrintEnergies(Energies, RPAParams, NSystems)
            real(F64), dimension(:), intent(in) :: Energies
            type(TRPAParams), intent(in)        :: RPAParams
            integer, intent(in)                 :: NSystems

            character(:), allocatable :: Prefix
            character(1), parameter :: Postfix = ")"
            integer, parameter :: ColWidth = 40
            logical :: kcal
            integer :: k
            character(ColWidth), dimension(RPA_ENERGY_NCOMPONENTS) :: Labels
            integer, parameter :: NTermsRPA = 5
            integer, parameter :: NTermsCC = 7
            integer, parameter :: NTermsTHC = 22
            integer, dimension(NTermsRPA), parameter :: TermsRPA = [ &
                  RPA_ENERGY_DFT, &
                  RPA_ENERGY_HF, &
                  RPA_ENERGY_SINGLES, &
                  RPA_ENERGY_CORR, &
                  RPA_ENERGY_TOTAL &
                  ]
            integer, dimension(NTermsCC), parameter :: TermsCC = [ &
                  RPA_ENERGY_DFT, &                                ! 1
                  RPA_ENERGY_HF, &                                 ! 2
                  RPA_ENERGY_1RDM_LINEAR, &                        ! 3
                  RPA_ENERGY_1RDM_QUADRATIC, &                     ! 4
                  RPA_ENERGY_DIRECT_RING, &                        ! 5
                  RPA_ENERGY_EXCHANGE, &                           ! 6
                  RPA_ENERGY_TOTAL &                               ! 7
                  ]
            integer, dimension(NTermsTHC), parameter :: TermsTHC = [ &
                  RPA_ENERGY_DFT, &                                ! 1
                  RPA_ENERGY_HF, &                                 ! 2
                  RPA_ENERGY_1RDM_LINEAR, &                        ! 3
                  RPA_ENERGY_1RDM_QUADRATIC, &                     ! 4
                  RPA_ENERGY_DIRECT_RING, &                        ! 5
                  RPA_ENERGY_CUMULANT_1B, &                        ! 6
                  RPA_ENERGY_CUMULANT_2B, &                        ! 7
                  RPA_ENERGY_CUMULANT_2C, &                        ! 8
                  RPA_ENERGY_CUMULANT_2D, &                        ! 9
                  RPA_ENERGY_CUMULANT_2E, &                        ! 10
                  RPA_ENERGY_CUMULANT_2F, &                        ! 11
                  RPA_ENERGY_CUMULANT_2G, &                        ! 12
                  RPA_ENERGY_CUMULANT_2H, &                        ! 13
                  RPA_ENERGY_CUMULANT_2I, &                        ! 14
                  RPA_ENERGY_CUMULANT_2J, &                        ! 15
                  RPA_ENERGY_CUMULANT_2K, &                        ! 16
                  RPA_ENERGY_CUMULANT_2L, &                        ! 17
                  RPA_ENERGY_CUMULANT_2M, &                        ! 18
                  RPA_ENERGY_CUMULANT_2N, &                        ! 19
                  RPA_ENERGY_CUMULANT_2O, &                        ! 20
                  RPA_ENERGY_CUMULANT_2P, &                        ! 21
                  RPA_ENERGY_TOTAL &                               ! 22
                  ]

            logical, dimension(RPA_ENERGY_NCOMPONENTS) :: DisplayedValues

            integer, parameter :: NTermsPT2 = 4
            integer, parameter :: NTermsPT3 = 13
            integer, dimension(NTermsPT2), parameter :: TermsPT2 = [ &
                  MP2_ENERGY_SINGLET_PAIR, &
                  MP2_ENERGY_TRIPLET_PAIR, &
                  MP2_ENERGY_DIRECT, &
                  MP2_ENERGY_TOTAL]
            integer, dimension(NTermsPT3), parameter :: TermsPT3 = [ &
                  MP3_ENERGY_A, &
                  MP3_ENERGY_B, &
                  MP3_ENERGY_C, &
                  MP3_ENERGY_D, &
                  MP3_ENERGY_E, &
                  MP3_ENERGY_F, &
                  MP3_ENERGY_G, &
                  MP3_ENERGY_H, &
                  MP3_ENERGY_I, &
                  MP3_ENERGY_J, &
                  MP3_ENERGY_K, &
                  MP3_ENERGY_L,&
                  MP3_ENERGY_TOTAL &
                  ]

            DisplayedValues = .false.
            if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2023) then
                  DisplayedValues(RPA_ENERGY_CUMULANT_1B) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2G) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2B) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2C) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2D) = .true.
            end if
            if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2024) then
                  DisplayedValues(RPA_ENERGY_CUMULANT_1B) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_PH3) = .true.
            end if
            if (RPAParams%TheoryLevel == RPA_THEORY_ALL) then
                  DisplayedValues(RPA_ENERGY_CUMULANT_1B) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2G) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2B) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2C) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2D) = .true.
                  !
                  DisplayedValues(RPA_ENERGY_CUMULANT_2E) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2H) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2K) = .true.
                  !
                  DisplayedValues(RPA_ENERGY_CUMULANT_2F) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2I) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2J) = .true.
                  DisplayedValues(RPA_ENERGY_CUMULANT_2L) = .true.                  
            end if
            DisplayedValues(RPA_ENERGY_DFT) = .true.
            DisplayedValues(RPA_ENERGY_HF) = .true.
            DisplayedValues(RPA_ENERGY_1RDM_LINEAR) = .true.
            DisplayedValues(RPA_ENERGY_1RDM_QUADRATIC) = .true.
            DisplayedValues(RPA_ENERGY_DIRECT_RING) = .true.
            DisplayedValues(RPA_ENERGY_TOTAL) = .true.
            if (RPAParams%PT_Order2) then
                  do k = 1, NTermsPT2
                        DisplayedValues(TermsPT2(k)) = .true.
                  end do
            end if
            if (RPAParams%PT_Order3) then
                  do k = 1, NTermsPT3
                        DisplayedValues(TermsPT3(k)) = .true.
                  end do
            end if
            if (NSystems == 1) then
                  Prefix = "E("
            else if (NSystems == 3) then
                  Prefix = "Eint("
            else
                  Prefix = "EintNadd("
            end if

            if (NSystems > 1) then
                  kcal = .true.
            else
                  kcal = .false.
            end if

            if (RPAParams%CoupledClusters) then
                  if (RPAParams%TensorHypercontraction) then                        
                        Labels(RPA_ENERGY_DFT)                         = lfield(Prefix // "DFT" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_HF)                          = lfield(Prefix // "HF" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_1RDM_LINEAR)                 = lfield(Prefix // "1-RDM linear" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_1RDM_QUADRATIC)              = lfield(Prefix // "1-RDM quadratic" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_DIRECT_RING)                 = lfield(Prefix // "direct ring" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_1B)                 = lfield(Prefix // "SOSEX" // Postfix, ColWidth)

                        Labels(RPA_ENERGY_CUMULANT_2B)                 = lfield(Prefix // "2b" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2C)                 = lfield(Prefix // "2c" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2D)                 = lfield(Prefix // "2d" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2E)                 = lfield(Prefix // "2e" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2F)                 = lfield(Prefix // "2f" // Postfix, ColWidth)                        
                        Labels(RPA_ENERGY_CUMULANT_2G)                 = lfield(Prefix // "2g" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2H)                 = lfield(Prefix // "2h" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2I)                 = lfield(Prefix // "2i" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2J)                 = lfield(Prefix // "2j" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2K)                 = lfield(Prefix // "2k" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2L)                 = lfield(Prefix // "2l" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2M)                 = lfield(Prefix // "2m" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2N)                 = lfield(Prefix // "2n" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2O)                 = lfield(Prefix // "2o" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_CUMULANT_2P)                 = lfield(Prefix // "2p" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_TOTAL)                       = lfield(Prefix // "total" // Postfix, ColWidth)

                        if (RPAParams%TheoryLevel == RPA_THEORY_JCTC2024) then
                              Labels(RPA_ENERGY_CUMULANT_PH3)          = lfield(Prefix // "3rd order ph" // Postfix, ColWidth)
                        end if
                  else
                        Labels(RPA_ENERGY_DFT)                         = lfield(Prefix // "DFT" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_HF)                          = lfield(Prefix // "HF" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_1RDM_LINEAR)                 = lfield(Prefix // "1-RDM linear" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_1RDM_QUADRATIC)              = lfield(Prefix // "1-RDM quadratic" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_DIRECT_RING)                 = lfield(Prefix // "direct ring" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_EXCHANGE)                    = lfield(Prefix // "exchange" // Postfix, ColWidth)
                        Labels(RPA_ENERGY_TOTAL)                       = lfield(Prefix // "total" // Postfix, ColWidth)
                  end if
            else
                  Labels(RPA_ENERGY_DFT)                         = lfield(Prefix // "DFT" // Postfix, ColWidth)
                  Labels(RPA_ENERGY_HF)                          = lfield(Prefix // "HF" // Postfix, ColWidth)
                  Labels(RPA_ENERGY_SINGLES)                     = lfield(Prefix // "RPA singles" // Postfix, ColWidth)
                  Labels(RPA_ENERGY_CORR)                        = lfield(Prefix // "RPA correlation" // Postfix, ColWidth)
                  Labels(RPA_ENERGY_TOTAL)                       = lfield(Prefix // "RPA total" // Postfix, ColWidth)
            end if
            
            if (RPAParams%PT_Order2) then
                  Labels(MP2_ENERGY_SINGLET_PAIR) = lfield(Prefix // "MP2 singlet pairs" // Postfix, ColWidth)
                  Labels(MP2_ENERGY_TRIPLET_PAIR) = lfield(Prefix // "MP2 triplet pairs" // Postfix, ColWidth)
                  Labels(MP2_ENERGY_DIRECT) = lfield(Prefix // "direct MP2" // Postfix, ColWidth)
                  Labels(MP2_ENERGY_TOTAL) = lfield(Prefix // "total MP2" // Postfix, ColWidth)
            end if
            if (RPAParams%PT_Order3) then
                  Labels(MP3_ENERGY_A) = lfield(Prefix // "MP3 A" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_B) = lfield(Prefix // "MP3 B" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_C) = lfield(Prefix // "MP3 C" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_D) = lfield(Prefix // "MP3 D" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_E) = lfield(Prefix // "MP3 E" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_F) = lfield(Prefix // "MP3 F" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_G) = lfield(Prefix // "MP3 G" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_H) = lfield(Prefix // "MP3 H" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_I) = lfield(Prefix // "MP3 I" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_J) = lfield(Prefix // "MP3 J" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_K) = lfield(Prefix // "MP3 K" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_L) = lfield(Prefix // "MP3 L" // Postfix, ColWidth)
                  Labels(MP3_ENERGY_TOTAL) = lfield(Prefix // "total MP3" // Postfix, ColWidth)
            end if

            if (NSystems == 1) then
                  call msg("RPA Single-Point Energies (a.u.)", underline=.true.)
            else if (NSystems == 3) then
                  call msg("RPA 2-Body Interaction Energies (kcal/mol)", underline=.true.)
            else if (NSystems == 7) then
                  call msg("RPA 3-Body Interaction Energies (kcal/mol)", underline=.true.)
            else
                  call msg("RPA 4-Body Interaction Energies (kcal/mol)", underline=.true.)
            end if
            !
            ! Perturbation theory terms. Used only for debugging
            !
            do k = 1, NTermsPT2
                  if (DisplayedValues(TermsPT2(k))) then
                        call rpa_EnergyTableRow(Labels(TermsPT2(k)), Energies(TermsPT2(k)), kcal)
                  end if
            end do
            do k = 1, NTermsPT3
                  if (DisplayedValues(TermsPT3(k))) then
                        call rpa_EnergyTableRow(Labels(TermsPT3(k)), Energies(TermsPT3(k)), kcal)
                  end if
            end do
            !
            ! Random-phase approximation and beyond-RPA energy components
            !
            if (RPAParams%CoupledClusters) then
                  if (RPAParams%TensorHypercontraction) then
                        do k = 1, NTermsTHC
                              if (DisplayedValues(TermsTHC(k))) then
                                    call rpa_EnergyTableRow(Labels(TermsTHC(k)), Energies(TermsTHC(k)), kcal)
                              end if
                        end do
                  else
                        do k = 1, NTermsCC
                              call rpa_EnergyTableRow(Labels(TermsCC(k)), Energies(TermsCC(k)), kcal)
                        end do
                  end if
            else
                  do k = 1, NTermsRPA
                        call rpa_EnergyTableRow(Labels(TermsRPA(k)), Energies(TermsRPA(k)), kcal)
                  end do
            end if
      end subroutine rpa_PrintEnergies


      subroutine rpa_EnergyTableRow(Label, E, kcal)
            character(*), intent(in) :: Label
            real(F64), intent(in)    :: E
            logical, intent(in)      :: kcal

            if (kcal) then
                  call msg(Label // rfield(str(tokcal(E), d=6), 20))
            else
                  call msg(Label // rfield(str(E, d=9), 20))
            end if
      end subroutine rpa_EnergyTableRow
end module rpa_driver
