module drv_dft
   use math_constants
   use arithmetic
   use gparam
   use basis
   use initialize
   use display
   use string
   use report
   use auxint
   use report
   use scf
   use cmplx_scf
   use real_scf
   use uniopt
   use h_xcfunc
   use rttddft
   use AtomicDensities
   use basis_sets
   use basis_definitions
   use scf_definitions
   use sys_definitions
   use rpa_driver
   use thc_definitions
   use TwoStepCholesky_definitions
   use drv_eri
   use TwoStepCholesky

   implicit none

contains

   subroutine task_dft_IsolatedHirshfeldAtoms_UKS(SCFParams, System, BasisAssign)
      !
      ! Generate spherically-averaged isolated atom densities for all elements
      ! in the system. Those densities are subsequently used to generate
      ! the Hirshfeld weights on the grid.
      !
      type(TSCFParams), intent(inout)    :: SCFParams
      type(TSystem), intent(in)          :: System
      type(TBasisAssignment), intent(in) :: BasisAssign

      type(TAOBasis) :: AOBasis
      type(TSystem) :: HirshAtom
      type(TSCFParams) :: HirshSCFParams
      type(TSCFOutput) :: HirshSCFOutput
      type(TAOBasis) :: HirshAOBasis
      integer :: k
      integer, dimension(:), allocatable :: ZList, ZCount, AtomElementMap
      integer :: NElements, ZNumber
      integer :: MaxNShells
      !
      ! Check the maximum number of orbital shells per single atom
      ! in the total system. That's needed to define the size
      ! of the atomic density matrix
      !
      call data_load_2(System)
      call init_modules()
      call basis_NewAOBasis(AOBasis, System, BasisAssign=BasisAssign)

      allocate(AtomElementMap(System%NAtoms))
      call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_ALL_ATOMS)
      call free_modules()
      call data_free()
      !
      ! Define an isolated atom
      !
      HirshAtom%SystemKind = SYS_MOLECULE
      HirshAtom%SubsystemAtoms(1) = 1
      HirshAtom%NAtoms = 1
      HirshAtom%RealAtoms(:, 1) = [1, 1]
      HirshAtom%RealAtoms(:, 2) = [1, 0]
      allocate(HirshAtom%AtomCoords(3, 1))
      HirshAtom%AtomCoords(:, 1) = [ZERO, ZERO, ZERO]
      allocate(HirshAtom%ZNumbers(1))
      HirshAtom%SubsystemCharges(1) = 0
      HirshAtom%Charge = 0
      !
      ! Parameters controlling the SCF for an isolated atom
      !
      HirshSCFParams%xcfunc = SCFParams%xcfunc
      HirshSCFParams%omega = SCFParams%omega
      HirshSCFParams%srexx = SCFParams%srexx
      HirshSCFParams%ConvThreshRho = 1.0E-6
      HirshSCFParams%ConvThreshGrad = 2.0E-5
      HirshSCFParams%AUXInt_Type1 = AUX_HIRSHFELD_VOLUME_FREE
      HirshSCFParams%ERI_Algorithm = SCF_ERI_EXACT
      MaxNShells = AOBasis%MaxNShells
      allocate(HirshSCFParams%AUXIn(0, 0))
      allocate(SCFParams%AUXIn(((MaxNShells+1)*MaxNShells)/2, NElements))
      allocate(SCFParams%HirshVolumes(NElements))
      if (SCFParams%AsympVxc == AC_LFAS) then
         HirshSCFParams%AsympVxc = AC_LFAS_v2
         HirshSCFParams%AsympVxcOmega = SCFParams%AsympVxcOmega
      end if

      do k = 1, NElements
         ZNumber = ZList(k)

         block
            type(TBasisAssignment) :: HirshBasisAssign
            type(TBasisRule)       :: GlobalRule
            integer                :: i

            ! Extract rule for this element from the parent system
            i = minloc(System%ZNumbers, dim=1, mask=(System%ZNumbers == ZNumber))
            GlobalRule = BasisAssign%get_atom_rule(i, ZNumber)
            GlobalRule%id = 0

            if (GlobalRule%FromLibrary) call HirshBasisAssign%set_library_dir(BasisAssign%LibraryDir)
            if (GlobalRule%GuessAvailable) call HirshBasisAssign%set_guess_dir(BasisAssign%GuessDir)
            call HirshBasisAssign%add_global_fallback(GlobalRule)

            HirshAtom%ZNumbers(1) = ZNumber
            HirshAtom%SubsystemMult(1) = unpaired_electrons(ZNumber) + 1
            HirshAtom%Mult = unpaired_electrons(ZNumber) + 1
            call sys_init(HirshAtom, SYS_TOTAL)

            call data_load_2(HirshAtom)
            call init_modules()
            call basis_NewAOBasis(HirshAOBasis, HirshAtom, BasisAssign=HirshBasisAssign)
            call scf_driver_SpinUnres(HirshSCFOutput, HirshSCFParams, HirshAOBasis, HirshAtom)
            !
            ! Generate spherically-averaged atomic densities (summed over spins)
            ! for all elements present in the system. Those densities are subsequently
            ! used to compute the Hirshfeld weights at each point of the numerical
            ! grid of the main system. The coefficients of the density matrices
            ! are passed to the subroutines evaluated on the numerical grid
            ! as the input array AUXIn.
            !
            call RhoSpherCoeffs(SCFParams%AUXIn(:, k), HirshSCFOutput%Rho_cao, HirshAOBasis)
            SCFParams%HirshVolumes(k) = HirshSCFOutput%AUXOut(1)

            call free_modules()
            call data_free()
         end block
      end do
   end subroutine task_dft_IsolatedHirshfeldAtoms_UKS


   subroutine task_dft_sp(scfresults, molecule, mocoeff_out, eorb_out)
      type(TSCFOutput), intent(out)       :: scfresults
      type(tmolecule), intent(in)          :: molecule
      double precision, dimension(:, :), &
         optional, intent(out)          :: mocoeff_out
      double precision, dimension(:), &
         optional, intent(out)          :: eorb_out

      double precision, dimension(:, :), allocatable :: mocoeff
      double precision, dimension(:), allocatable :: eorb
      integer, parameter :: ncells = 8
      character(len=DEFLEN), dimension(ncells) :: cells
      integer :: auxid
      type(TSCFParams) :: scfinput
      ! real(F64), dimension(:, :), allocatable :: rho_atomic

      call data_load(molecule)
      call init_modules()

      if (IMG_ISMASTER) then
         allocate(mocoeff(NORB, NORB))
         allocate(eorb(NORB))
         call scfinput%init(XCMODEL, LCSREXX, LCOMEGA)
         call scfinput%non_scf(NONSCF_XCMODEL, NONSCF_LCSREXX, NONSCF_LCOMEGA)
         if (allocated(SCF_GUESS_RHOA_PATH)) SCFInput%guess_rhoa_path = SCF_GUESS_RHOA_PATH
         if (allocated(SCF_GUESS_RHOB_PATH)) SCFInput%guess_rhob_path = SCF_GUESS_RHOB_PATH
         if (allocated(SCF_SAVE_RHOA_PATH)) SCFInput%save_rhoa_path = SCF_SAVE_RHOA_PATH
         if (allocated(SCF_SAVE_RHOB_PATH)) SCFInput%save_rhob_path = SCF_SAVE_RHOB_PATH
         SCFInput%save_rho_mode = SCF_SAVE_RHO_MODE
         SCFInput%guess_type = SCF_GUESS
         auxid = AUXINTEGRAL
         ! if (auxid == AUX_HIRSHFELD_VOLUME .or. &
         !       auxid == AUX_HIRSHFELD_POPULATION) then
         !       !
         !       ! The Hirshfeld analysis cannot be done for
         !       ! single atom systems
         !       !
         !       if (NRealAtoms() > 1) then
         !             call get_rho_atomic(rho_atomic)
         !             call scfinput%AUXIntegral(auxid, AUXINT_TYPE2, rho_atomic)
         !       else
         !             call scfinput%AUXIntegral(AUX_NONE, AUXINT_TYPE2)
         !       end if
         ! else
         call scfinput%AUXIntegral(auxid, AUXINT_TYPE2)
         ! end if

         call ksdriver(scfresults, scfinput, mocoeff, eorb)

         if (DOREPORT) then
            cells(1) = '"' // trim(molecule%path) // '"'
            cells(2) = str(dble(molecule%charge_frac) / dble(ROKS_NEUNIT))
            cells(3) = str(scfresults%EtotDFT)
            cells(4) = str(tokcal(scfresults%EdispDFT))
            cells(5) = str(LCOMEGA)
            cells(6) = str(toev(scfresults%Ehomo))
            cells(7) = str(toev(scfresults%Elumo))
            if (auxid .ne. AUX_HIRSHFELD_VOLUME .and. &
               auxid .ne. AUX_HIRSHFELD_POPULATION .and. &
               auxid .ne. AUX_NONE) then
               cells(8) = str(scfresults%AUXOut(1))
            else
               cells(8) = str(ZERO)
            end if
            call rep_update(cells, ncells)
         end if

         if (present(mocoeff_out)) mocoeff_out = mocoeff
         if (present(eorb_out)) eorb_out = eorb

         deallocate(mocoeff)
         deallocate(eorb)
      else
         call ksgen_slave()
      end if

      call free_modules()
      call data_free()
   end subroutine task_dft_sp


   subroutine task_rttddft_polar(scfresults, molecule)
      type(TSCFOutput), intent(out)       :: scfresults
      type(tmolecule), intent(in)          :: molecule

      integer, parameter :: ncells = 7
      character(len=DEFLEN), dimension(ncells) :: cells
      integer :: auxid
      type(TSCFParams) :: scfinput
      type(TAOBasis) :: AOBasis
      type(txcdef) :: ixc
      real(F64), dimension(:), allocatable :: eorb
      real(F64), dimension(:, :), allocatable :: rho_cao
      ! real(F64), dimension(:, :), allocatable :: rho_atomic
      integer :: k

      call data_load(molecule)
      call init_modules()

      if (IMG_ISMASTER) then
         allocate(rho_cao(NORB, NORB))
         allocate(eorb(NORB))
         call scfinput%init(XCMODEL, LCSREXX, LCOMEGA)
         call scfinput%non_scf(NONSCF_XCMODEL, NONSCF_LCSREXX, NONSCF_LCOMEGA)
         if (allocated(SCF_GUESS_RHOA_PATH)) SCFInput%guess_rhoa_path = SCF_GUESS_RHOA_PATH
         if (allocated(SCF_GUESS_RHOB_PATH)) SCFInput%guess_rhob_path = SCF_GUESS_RHOB_PATH
         if (allocated(SCF_SAVE_RHOA_PATH)) SCFInput%save_rhoa_path = SCF_SAVE_RHOA_PATH
         if (allocated(SCF_SAVE_RHOB_PATH)) SCFInput%save_rhob_path = SCF_SAVE_RHOB_PATH
         SCFInput%save_rho_mode = SCF_SAVE_RHO_MODE
         SCFInput%guess_type = SCF_GUESS
         auxid = AUXINTEGRAL
         ! if (auxid == AUX_HIRSHFELD_VOLUME .or. &
         !       auxid == AUX_HIRSHFELD_POPULATION) then
         !       !
         !       ! The Hirshfeld analysis cannot be done for
         !       ! single atom systems
         !       !
         !       if (NRealAtoms() > 1) then
         !             call get_rho_atomic(rho_atomic)
         !             call scfinput%AUXIntegral(auxid, AUXINT_TYPE2, rho_atomic)
         !       else
         !             call scfinput%AUXIntegral(AUX_NONE, AUXINT_TYPE2)
         !       end if
         ! else
         call scfinput%AUXIntegral(auxid, AUXINT_TYPE2)
         ! end if
         associate ( &
            AtomCoords => ATOMR, &
            ShellCenters=>SHATOM, &
            ShellParamsIdx => SH, &
            ShellMomentum => SHTYPE, &
            NPrimitives => NPRM, &
            CntrCoeffs => CNTR, &
            Exponents => EXPN, &
            NormFactorsCart => CNTRNORM, &
            SpherAO => SPHERBASIS, &
            R2MaxArray => R2MAX &
            )
            call basis_NewAOBasis_2(AOBasis, AtomCoords, ShellCenters, ShellParamsIdx, ShellMomentum, &
               NPrimitives, CntrCoeffs, Exponents, NormFactorsCart, R2MaxArray, SpherAO)
         end  associate
         ! ------------------------------------------------------------------
         !                     GROUND-STATE CALCULATION
         ! ------------------------------------------------------------------
         call ksdriver(scfresults, scfinput, rho_cao, eorb, outmatrix=SCF_OUT_RHO_AO)
         ! ------------------------------------------------------------------
         !                       REAL-TIME PROPAGATION
         ! ------------------------------------------------------------------
         call xcf_define(ixc, XCMODEL, AUX_NONE, .false.)
         do k = 1, size(RTTDDFT_POLAR_OMEGA)
            call real_time_polar(RTTDDFT_POLAR_OMEGA(k), ixc, NE/2, rho_cao, AOBasis, &
               LINDEP_THRESH, RTTDDFT_POLAR_NCYCLES, RTTDDFT_TIMESTEP, RTTDDFT_POLAR_FIELD)
         end do

         if (DOREPORT) then
            cells(1) = '"' // trim(molecule%path) // '"'
            cells(2) = str(dble(molecule%charge_frac) / dble(ROKS_NEUNIT))
            cells(3) = str(scfresults%EtotDFT)
            cells(4) = str(LCOMEGA)
            cells(5) = str(toev(scfresults%Ehomo))
            cells(6) = str(toev(scfresults%Elumo))
            if (auxid .ne. AUX_HIRSHFELD_VOLUME .and. &
               auxid .ne. AUX_HIRSHFELD_POPULATION .and. &
               auxid .ne. AUX_NONE) then

               cells(7) = str(scfresults%AUXOut(1))
            else
               cells(7) = str(ZERO)
            end if

            call rep_update(cells, ncells)
         end if

         deallocate(rho_cao)
         deallocate(eorb)
      else
         call ksgen_slave()
      end if

      call free_modules()
      call data_free()
   end subroutine task_rttddft_polar


   subroutine task_dft_int_ROKS(mola, molb, molab)
      type(tmolecule), intent(in) :: mola, molb, molab

      double precision :: energy_a, energy_b, energy_ab, eint, edisp
      integer, parameter :: ncells = 3
      character(len=DEFLEN), dimension(ncells) :: cells
      type(TSCFOutput) :: scfa, scfb, scfab
      integer :: auxid
      type(TSCFParams) :: scfinput
      double precision, dimension(:, :), allocatable :: mocoeff
      double precision, dimension(:), allocatable :: eorb
      ! real(F64), dimension(:, :), allocatable :: rho_atomic

      if (IMG_ISMASTER) then
         !
         ! Monomer A
         !
         call data_load(mola)
         call init_modules()
         allocate(mocoeff(NORB, NORB))
         allocate(eorb(NORB))
         call scfinput%init(XCMODEL, LCSREXX, LCOMEGA)
         call scfinput%non_scf(NONSCF_XCMODEL, NONSCF_LCSREXX, NONSCF_LCOMEGA)
         SCFInput%guess_type = SCF_GUESS
         auxid = AUXINTEGRAL
         ! if (auxid == AUX_HIRSHFELD_VOLUME .or. &
         !       auxid == AUX_HIRSHFELD_POPULATION) then
         !       !
         !       ! The Hirshfeld analysis cannot be done for
         !       ! single atom systems
         !       !
         !       if (NRealAtoms() > 1) then
         !             call get_rho_atomic(rho_atomic)
         !             call scfinput%AUXIntegral(auxid, AUXINT_TYPE2, rho_atomic)
         !       else
         !             call scfinput%AUXIntegral(AUX_NONE, AUXINT_TYPE2)
         !       end if
         ! else
         call scfinput%AUXIntegral(auxid, AUXINT_TYPE2)
         ! end if

         call ksdriver(scfa, scfinput, mocoeff, eorb)
         call free_modules()
         call data_free()
         !
         ! Monomer B
         !
         call data_load(molb)
         call init_modules()

         auxid = AUXINTEGRAL
         ! if (auxid == AUX_HIRSHFELD_VOLUME .or. &
         !       auxid == AUX_HIRSHFELD_POPULATION) then
         !       !
         !       ! The Hirshfeld analysis cannot be done for
         !       ! single atom systems
         !       !
         !       if (NRealAtoms() > 1) then
         !             call get_rho_atomic(rho_atomic)
         !             call scfinput%AUXIntegral(auxid, AUXINT_TYPE2, rho_atomic)
         !       else
         !             call scfinput%AUXIntegral(AUX_NONE, AUXINT_TYPE2)
         !       end if
         ! else
         call scfinput%AUXIntegral(auxid, AUXINT_TYPE2)
         ! end if

         call ksdriver(scfb, scfinput, mocoeff, eorb)
         call free_modules()
         call data_free()
         !
         ! Dimer
         !
         call data_load(molab)
         call init_modules()

         auxid = AUXINTEGRAL
         ! if (auxid == AUX_HIRSHFELD_VOLUME .or. &
         !       auxid == AUX_HIRSHFELD_POPULATION) then
         !       call get_rho_atomic(rho_atomic)
         !       call scfinput%AUXIntegral(auxid, AUXINT_TYPE2, rho_atomic)
         ! else
         call scfinput%AUXIntegral(auxid, AUXINT_TYPE2)
         ! end if
         !
         ! The sum of A and B densities is not used as the guess for AB
         ! because the quadratic convergence algorithm performs better
         ! if it has access to a limited number of density matrices outside
         ! the close neighbourhood of the converged solution.
         !
         call ksdriver(scfab, scfinput, mocoeff, eorb)
         call free_modules()
         call data_free()

         energy_a = scfa%EtotDFT
         energy_b = scfb%EtotDFT
         energy_ab = scfab%EtotDFT
         eint = energy_ab - energy_a - energy_b
         edisp = scfab%EdispDFT - scfa%EdispDFT - scfb%EdispDFT

         call toprule()
         call msg("CALCULATIONS COMPLETED")
         call midrule()
         call dmsg("MOLECULE A", energy_a)
         call dmsg("MOLECULE B", energy_b)
         call dmsg("MOLECULE AB", energy_ab)
         call dmsg("Eint (TOTAL) [a.u.]", eint)
         call dmsg("Eint (TOTAL) [kcal/mol]", tokcal(eint))
         if (DFT_DISP .ne. DISP_NONE) then
            call dmsg("Eint (DISPERSION) [kcal/mol]", tokcal(edisp))
         end if
         !
         ! Add record to report file if requested
         !
         if (DOREPORT) then
            cells(1) = '"'//trim(molab%path)//'"'
            cells(2) = str(tokcal(edisp))
            cells(3) = str(tokcal(eint))
            call rep_update(cells, ncells)
         end if
      else
         !
         ! Monomer A
         !
         call data_load(mola)
         call init_modules()
         call ksgen_slave()
         call free_modules()
         call data_free()
         !
         ! Monomer B
         !
         call data_load(molb)
         call init_modules()
         call ksgen_slave()
         call free_modules()
         call data_free()
         !
         ! Dimer AB
         !
         call data_load(molab)
         call init_modules()
         call ksgen_slave()
         call data_free()
         call free_modules()
      end if
   end subroutine task_dft_int_ROKS


   subroutine task_dft_UKS(System, SCFParams, Chol2Params, THCParams, BasisAssign)
      type(TSystem), intent(inout)       :: System
      type(TSCFParams), intent(in)       :: SCFParams
      type(TChol2Params), intent(in)     :: Chol2Params
      type(TTHCParams), intent(inout)    :: THCParams
      type(TBasisAssignment), intent(in) :: BasisAssign

      type(TSCFOutput), dimension(15) :: SCFOutput
      type(TAOBasis) :: AOBasis
      type(TChol2Vecs) :: Chol2Vecs
      type(TCoulTHCGrid) :: THCGrid
      real(F64), dimension(:, :, :), allocatable :: Rkpq[:]
      real(F64), dimension(15) :: EtotDFT, EdispDFT
      real(F64) :: EtotDFT_AB, EtotDFT_ABC, EtotDFT_ABCD
      real(F64) :: EdispDFT_AB, EdispDFT_ABC, EdispDFT_ABCD
      real(F64) :: EtotDFT_Nadd, EdispDFT_Nadd
      integer :: k, NSystems

      if (System%SystemKind == SYS_MOLECULE) then
         NSystems = 1
      else if (System%SystemKind == SYS_DIMER) then
         NSystems = 3
      else if (System%SystemKind == SYS_TRIMER) then
         NSystems = 7
      else ! Tetramer
         NSystems = 15
      end if

      call sys_Init(System, SYS_TOTAL)
      call data_load_2(System)
      call init_modules()
      call basis_NewAOBasis(AOBasis, System, BasisAssign=BasisAssign)

      call drv_eri_run(Rkpq, Chol2Vecs, THCGrid, &
         AOBasis, System, SCFParams, Chol2Params, THCParams)

      do k = 1, NSystems
         if (k > 1) then
            call sys_Init(System, k)
            call data_load_2(System)
            call init_modules()
         end if
         call scf_driver_SpinUnres(SCFOutput(k), SCFParams, AOBasis, System, &
            Rkpq, Chol2Vecs, THCGrid)
         if (.not. SCFOutput(k)%Converged) then
            call msg("SCF not converged. Cannot continue with a post-SCF calculation", MSG_ERROR)
            error stop
         end if
         EtotDFT(k) = SCFOutput(k)%EtotDFT
         EdispDFT(k) = SCFOutput(k)%EdispDFT
         call free_modules()
         call data_free()
      end do

      if (System%SystemKind == SYS_MOLECULE) then
         call msg("DFT Single-Point Energies (a.u.)", underline=.true.)

         call msg(lfield("E(disp)", 50) // lfield(str(EdispDFT(1), d=9), 20))
         call msg(lfield("E(DFT total)", 50)  // lfield(str(EtotDFT(1), d=9), 20))

      else if (System%SystemKind == SYS_DIMER) then
         call msg("DFT 2-Body Interaction Energies (kcal/mol)", underline=.true.)

         EdispDFT_AB = EdispDFT(SYS_TOTAL) - EdispDFT(SYS_MONO_A) - EdispDFT(SYS_MONO_B)
         EtotDFT_AB = EtotDFT(SYS_TOTAL) - EtotDFT(SYS_MONO_A) - EtotDFT(SYS_MONO_B)

         call msg(lfield("Eint(disp)", 30) // rfield(str(tokcal(EdispDFT_AB), d=6), 20))
         call msg(lfield("Eint(DFT total)", 30) // rfield(str(tokcal(EtotDFT_AB), d=6), 20))

      else if (System%SystemKind == SYS_TRIMER) then
         call msg("DFT 3-Body Interaction Energies (kcal/mol)", underline=.true.)

         EdispDFT_ABC = EdispDFT(SYS_TOTAL) - EdispDFT(SYS_MONO_A) - EdispDFT(SYS_MONO_B) - EdispDFT(SYS_MONO_C)
         EtotDFT_ABC = EtotDFT(SYS_TOTAL) - EtotDFT(SYS_MONO_A) - EtotDFT(SYS_MONO_B) - EtotDFT(SYS_MONO_C)

         call rpa_EintNadd(EdispDFT_Nadd, EdispDFT)
         call rpa_EintNadd(EtotDFT_Nadd, EtotDFT)

         call msg(lfield("EintABC(disp)", 30) // rfield(str(tokcal(EdispDFT_ABC), d=6), 20))
         call msg(lfield("EintABC(DFT total)", 30) // rfield(str(tokcal(EtotDFT_ABC), d=6), 20))

         call msg(lfield("EintNadd(disp)", 30) // rfield(str(tokcal(EdispDFT_Nadd), d=6), 20))
         call msg(lfield("EintNadd(DFT)", 30) // rfield(str(tokcal(EtotDFT_Nadd), d=6), 20))

      else ! Tetramer
         call msg("DFT 4-Body Interaction Energies (kcal/mol)", underline=.true.)

         EdispDFT_ABCD = EdispDFT(SYS_TOTAL) - EdispDFT(SYS_MONO_A) - EdispDFT(SYS_MONO_B) &
            - EdispDFT(SYS_MONO_C) - EdispDFT(SYS_MONO_D)
         EtotDFT_ABCD = EtotDFT(SYS_TOTAL) - EtotDFT(SYS_MONO_A) - EtotDFT(SYS_MONO_B) &
            - EtotDFT(SYS_MONO_C) - EtotDFT(SYS_MONO_D)

         call rpa_EintNadd4Body(EdispDFT_Nadd, EdispDFT)
         call rpa_EintNadd4Body(EtotDFT_Nadd, EtotDFT)

         call msg(lfield("EintABCD(disp)", 30) // rfield(str(tokcal(EdispDFT_ABCD), d=6), 20))
         call msg(lfield("EintABCD(DFT total)", 30) // rfield(str(tokcal(EtotDFT_ABCD), d=6), 20))

         call msg(lfield("EintNadd(disp)", 30) // rfield(str(tokcal(EdispDFT_Nadd), d=6), 20))
         call msg(lfield("EintNadd(DFT total)", 30) // rfield(str(tokcal(EtotDFT_Nadd), d=6), 20))
      end if
      call blankline()
   end subroutine task_dft_UKS


   subroutine task_dft_optomega(xyz_cation, xyz_neutral, xyz_anion)
      type(tmolecule), intent(in) :: xyz_cation
      type(tmolecule), intent(in) :: xyz_neutral
      type(tmolecule), intent(in) :: xyz_anion

      type(tunidata) :: optdata
      double precision :: omega
      real(F64) :: x1
      real(F64) :: x2
      real(F64) :: x3
      double precision :: max_omega
      double precision :: min_omega
      double precision, parameter :: abstol = 1.d-2
      integer, parameter :: max_niter = 100
      double precision :: f1, f2, f3, fnew
      double precision :: xmin, fmin, uncertainty
      double precision, dimension(max_niter+3) :: xvec, fvec, fvec2
      character(len=DEFLEN) :: line
      integer, dimension(max_niter+3) :: tvec
      integer :: k
      integer :: nscf
      integer :: info
      integer, dimension(IMG_IMSGSIZE) :: imessage
      double precision, dimension(1) :: dmessage
      integer, parameter :: ncells = 7
      character(len=DEFLEN), dimension(ncells) :: cells

      max_omega = OPTOMEGA_MAX
      min_omega = OPTOMEGA_MIN

      x1 = (max_omega - min_omega) / 7.5_F64
      x2 = (max_omega - min_omega) / 5.0_F64
      x3 = (max_omega - min_omega) / 3.0_F64

      LCOMEGA = x1
      call task_dft_j2(f1, xyz_cation, xyz_neutral, xyz_anion)
      LCOMEGA = x2
      call task_dft_j2(f2, xyz_cation, xyz_neutral, xyz_anion)
      LCOMEGA = x3
      call task_dft_j2(f3, xyz_cation, xyz_neutral, xyz_anion)

      if (IMG_ISMASTER) then
         xvec(1) = x1
         xvec(2) = x2
         xvec(3) = x3
         fvec(1) = f1
         fvec(2) = f2
         fvec(3) = f3
         nscf = 3
         call uni_firstiter(omega, x1, f1, x2, f2, x3, f3, abstol, max_niter, &
            min_omega, max_omega, optdata, info)
         do while (info == UNI_CONTINUE)
            if (IMG_ENABLED) then
               imessage(1) = IMG_MSG_NEXTTASK
               call img_toslaves(imessage)
               dmessage(1) = omega
               call img_toslaves(dmessage)
            end if
            LCOMEGA = omega
            call task_dft_j2(fnew, xyz_cation, xyz_neutral, xyz_anion)
            nscf = nscf + 1
            xvec(nscf) = omega
            fvec(nscf) = fnew
            call uni_nextiter(omega, fnew, optdata, info)
         end do
         call blankline()
         call toprule()
         call msg("SYSTEM-SPECIFIC OPTIMIZATION OF RANGE-SEPARATION PARAM FOR")
         call msg(trim(xyz_neutral%path))
         call midrule()
         select case (info)
          case (UNI_CONVERGED)
            call uni_getsolution(xmin, fmin, uncertainty, optdata)
            call msg("MINIMIZATION OF J^2 SUCCESSFULLY CONVERGED")
            call dmsg("CONVERGENCE THRESH [1/BOHR]", abstol, fmt="ES8.1")
            call dmsg("OPTIMAL OMEGA [1/BOHR]", xmin, fmt="F5.3")
            call dmsg("UNCERTAINTY [1/BOHR]", uncertainty, fmt="ES8.1")
            call dmsg("MINIMAL J^2 [eV^2]", fmin, fmt="ES8.1")
            call dmsg("SMALLEST ADMISSIBLE OMEGA [1/BOHR]", min_omega, fmt="F5.3")
            call dmsg("LARGEST ADMISSIBLE OMEGA [1/BOHR]", max_omega, fmt="F5.3")
          case (UNI_EXCEEDED_MAX_NITER)
            call msg("ERROR: MINIMIZATION OF J^2 NOT CONVERGED")
            call msg("MAXIMUM NUMBER OF ITERATIONS REACHED WITHOUT CONVERGENCE")
          case (UNI_MULTIPLE_MINIMA)
            call msg("ERROR: MINIMIZATION OF J^2 DISCONTINUED")
            call msg("J^2 HAS MULTPLE LOCAL MINIMA. TIGHTEN THE SEARCH REGION")
         end select
         call imsg("NUMBER OF SINGLE-POINT EVALUATIONS", nscf)
         do k = 1, nscf
            tvec(k) = k
         end do
         call dsort(xvec, tvec, nscf)
         fvec2(1:nscf) = fvec(1:nscf)
         do k = 1, nscf
            fvec(k) = fvec2(tvec(k))
         end do
         call blankline()
         write(line, "(A3,1X,A6,2X,A3)") "#", "OMEGA", "J^2"
         call msg(line)
         do k = 1, nscf
            write(line, "(I3,1X,F6.4,2X,ES8.1)") k, xvec(k), fvec(k)
            call msg(line)
         end do

         if (DOREPORT) then
            cells(1) = '"'//trim(xyz_neutral%path)//'"'
            cells(2) = str(fmin)
            cells(3) = str(xmin)
            cells(4) = str(uncertainty)
            select case (OPTOMEGA_J2TYPE)
             case (OPTOMEGA_J2_CN)
               cells(5) = '"YES"'
               cells(6) = '"NO"'
             case (OPTOMEGA_J2_NA)
               cells(5) = '"NO"'
               cells(6) = '"YES"'
             case (OPTOMEGA_J2_CNA)
               cells(5) = '"YES"'
               cells(6) = '"YES"'
            end select
            if (info == UNI_CONVERGED) then
               cells(7) = '"YES"'
            else
               cells(7) = '"NO"'
            end if
            call rep_update(cells, ncells)
         end if

         if (IMG_ENABLED) then
            imessage(1) = IMG_MSG_WORKDONE
            call img_toslaves(imessage)
         end if
      else
         tasks: do
            call img_frommaster(imessage)
            if (iand(imessage(1), IMG_MSG_NEXTTASK) > 0) then
               !
               ! Get the next OMEGA parameter from the master image
               !
               call img_frommaster(dmessage)
               omega = dmessage(1)
               LCOMEGA = omega
               call task_dft_j2(f1, xyz_cation, xyz_neutral, xyz_anion)
            else
               !
               ! Master has sent the IMG_MSG_WORKDONE message.
               ! Optimization of OMEGA is done.
               !
               exit tasks
            end if
         end do tasks
      end if
   end subroutine task_dft_optomega


   subroutine task_dft_j2(j2, xyz_cation, xyz_neutral, xyz_anion)
      double precision, intent(out) :: j2
      type(tmolecule), intent(in)   :: xyz_cation
      type(tmolecule), intent(in)   :: xyz_neutral
      type(tmolecule), intent(in)   :: xyz_anion

      type(TSCFOutput) :: results_cation, results_neutral, results_anion
      type(TSCFParams) :: scfinput
      double precision, dimension(:, :), allocatable :: wmatrix
      double precision, dimension(:), allocatable :: eorb
      double precision :: ip, ea, ehomo, elumo

      if (IMG_ISMASTER) then
         !
         ! Neutral molecule
         !
         call data_load(xyz_neutral)
         call init_modules()
         allocate(wmatrix(NORB, NORB))
         allocate(eorb(0))
         call scfinput%init(XCMODEL, LCSREXX, LCOMEGA)
         call ksdriver(results_neutral, scfinput, wmatrix, eorb, &
            outmatrix=SCF_OUT_RHO_AO)
         call free_modules()
         call data_free()
         !
         ! Cation
         !
         if (OPTOMEGA_J2TYPE == OPTOMEGA_J2_CNA &
            .or. OPTOMEGA_J2TYPE == OPTOMEGA_J2_CN) then
            call data_load(xyz_cation)
            call init_modules()
            call ksdriver(results_cation, scfinput, wmatrix, eorb, &
               outmatrix=SCF_OUT_NONE, guessmethod=SCF_GUESS_RHO)
            call free_modules()
            call data_free()
         end if
         !
         ! Anion
         !
         if (OPTOMEGA_J2TYPE == OPTOMEGA_J2_CNA &
            .or. OPTOMEGA_J2TYPE == OPTOMEGA_J2_NA) then
            call data_load(xyz_anion)
            call init_modules()
            call ksdriver(results_anion, scfinput, wmatrix, eorb, &
               outmatrix=SCF_OUT_NONE, guessmethod=SCF_GUESS_RHO)
            call free_modules()
            call data_free()
         end if
         deallocate(wmatrix)
         deallocate(eorb)

         ehomo = toev(results_neutral%Ehomo)
         elumo = toev(results_neutral%Elumo)

         call toprule()
         call msg("J^2 DIAGNOSTIC")
         call midrule()
         call dmsg("OMEGA [1/BOHR]", LCOMEGA)
         call dmsg("EHOMO(NEUTRAL) [eV]", toev(results_neutral%Ehomo))
         call dmsg("ELUMO(NEUTRAL) [eV]", toev(results_neutral%Elumo))

         select case (OPTOMEGA_J2TYPE)
          case (OPTOMEGA_J2_CNA)
            ip = toev(results_cation%EtotDFT-results_neutral%EtotDFT)
            ea = toev(results_neutral%EtotDFT-results_anion%EtotDFT)
            j2 = (ip+ehomo)**2 + (ea+elumo)**2
            call dmsg("IONIZATION POTENTIAL [eV]", ip)
            call dmsg("ELECTRON AFFINITY [eV]", ea)
            call msg("J^2 = (IP+EHOMO)^2 + (EA+ELUMO)^2")
          case (OPTOMEGA_J2_CN)
            ip = toev(results_cation%EtotDFT-results_neutral%EtotDFT)
            j2 = (ip+ehomo)**2
            call dmsg("IONIZATION POTENTIAL [eV]", ip)
            call msg("J^2 = (IP+EHOMO)^2")
          case (OPTOMEGA_J2_NA)
            ea = toev(results_neutral%EtotDFT-results_anion%EtotDFT)
            j2 = (ip+ehomo)**2 + (ea+elumo)**2
            call dmsg("ELECTRON AFFINITY [eV]", ea)
            call msg("J^2 = (EA+ELUMO)^2")
         end select

         call dmsg("J^2 [eV^2]", j2)
      else
         !
         ! Neutral molecule
         !
         call data_load(xyz_neutral)
         call init_modules()
         call ksgen_slave()
         call free_modules()
         call data_free()
         !
         ! Cation
         !
         if (OPTOMEGA_J2TYPE == OPTOMEGA_J2_CNA &
            .or. OPTOMEGA_J2TYPE == OPTOMEGA_J2_CN) then
            call data_load(xyz_cation)
            call init_modules()
            call ksgen_slave()
            call free_modules()
            call data_free()
         end if
         !
         ! Anion
         !
         if (OPTOMEGA_J2TYPE == OPTOMEGA_J2_CNA &
            .or. OPTOMEGA_J2TYPE == OPTOMEGA_J2_NA) then
            call data_load(xyz_anion)
            call init_modules()
            call ksgen_slave()
            call data_free()
            call free_modules()
         end if
         j2 = -ONE
      end if
   end subroutine task_dft_j2
end module drv_dft

