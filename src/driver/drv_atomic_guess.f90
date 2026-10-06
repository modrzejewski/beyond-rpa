module drv_atomic_guess
   use arithmetic
   use math_constants
   use gparam
   use basis
   use initialize
   use display
   use string
   use periodic
   use io
   use linalg
   use h_xcfunc
   use OneElectronInts
   use AtomicDensities
   use basis_sets
   use basis_definitions
   use scf_definitions
   use sys_definitions
   use real_scf
   use TwoStepCholesky_definitions
   use TwoStepCholesky

   implicit none
   private :: validate_spherical_average_
   private :: trace_ab_
   private :: max_abs_diff_

contains

   subroutine drv_IsolatedAtomSCF(AtomSCF, AtomBasis, Atom, ZNumber, Rule, &
      AtomSCFParams, LibraryDir, GuessDir)
      !
      ! Run SCF for an isolated neutral atom of element ZNumber in its
      ! ground-state spin multiplicity, with the basis set of Rule.
      ! The atom is read from the same lines as an xyz block of the input.
      ! LibraryDir and GuessDir are passed to the basis assignment of the
      ! atom; the resolved paths in Rule suffice for basis_Init.
      ! With Cholesky integrals, the Cholesky vectors are computed with
      ! the default parameters of TChol2Params.
      ! The caller calls free_modules and data_free after it has
      ! finished with AtomBasis.
      !
      type(TSCFOutput), intent(out)      :: AtomSCF
      type(TAOBasis), intent(out)        :: AtomBasis
      type(TSystem), intent(out)         :: Atom
      integer, intent(in)                :: ZNumber
      type(TBasisRule), intent(in)       :: Rule
      type(TSCFParams), intent(in)       :: AtomSCFParams
      character(*), optional, intent(in) :: LibraryDir
      character(*), optional, intent(in) :: GuessDir

      type(TBasisAssignment) :: AtomBasisAssign
      type(TBasisRule) :: GlobalRule
      type(TChol2Params) :: Chol2Params
      type(TChol2Vecs) :: Chol2Vecs
      real(F64), dimension(:, :, :), allocatable :: Rkpq[:]
      integer :: AtomIdx

      if (present(LibraryDir)) call AtomBasisAssign%set_library_dir(LibraryDir)
      if (present(GuessDir)) call AtomBasisAssign%set_guess_dir(GuessDir)
      GlobalRule = Rule
      GlobalRule%id = 0
      call AtomBasisAssign%add_global_fallback(GlobalRule)

      AtomIdx = -1
      call sys_Read_XYZ_NextLine(Atom, AtomIdx, "1", SYS_UNITS_BOHR)
      call sys_Read_XYZ_NextLine(Atom, AtomIdx, &
         "mult " // str(unpaired_electrons(ZNumber) + 1), SYS_UNITS_BOHR)
      call sys_Read_XYZ_NextLine(Atom, AtomIdx, &
         trim(ELNAME_SHORT(ZNumber)) // " 0.0 0.0 0.0", SYS_UNITS_BOHR)
      call sys_Init(Atom, SYS_TOTAL)

      call data_load_2(Atom)
      call init_modules()
      call basis_Init(AtomBasis, Atom, BasisAssign=AtomBasisAssign)
      if (AtomSCFParams%ERI_Algorithm == SCF_ERI_CHOLESKY) then
         call chol2_Algo_Koch_JCP2019(Chol2Vecs, AtomBasis, Chol2Params)
         call chol2_FullDimVectors(Rkpq, Chol2Vecs, AtomBasis, Chol2Params)
         call scf_driver_SpinUnres(AtomSCF, AtomSCFParams, AtomBasis, Atom, &
            Rkpq, Chol2Vecs)
      else
         call scf_driver_SpinUnres(AtomSCF, AtomSCFParams, AtomBasis, Atom)
      end if
   end subroutine drv_IsolatedAtomSCF


   function trace_ab_(A, B)
      !
      ! Return Tr(A B) = Sum(ij) A(i,j) B(j,i) without array temporaries
      !
      real(F64) :: trace_ab_
      real(F64), dimension(:, :), intent(in) :: A
      real(F64), dimension(:, :), intent(in) :: B

      integer :: i, j

      trace_ab_ = ZERO
      do j = 1, size(A, dim=2)
         do i = 1, size(A, dim=1)
            trace_ab_ = trace_ab_ + A(i, j) * B(j, i)
         end do
      end do
   end function trace_ab_


   function max_abs_diff_(A, B)
      !
      ! Return max|A(i,j) - B(i,j)| without array temporaries
      !
      real(F64) :: max_abs_diff_
      real(F64), dimension(:, :), intent(in) :: A
      real(F64), dimension(:, :), intent(in) :: B

      integer :: i, j

      max_abs_diff_ = ZERO
      do j = 1, size(A, dim=2)
         do i = 1, size(A, dim=1)
            max_abs_diff_ = max(max_abs_diff_, abs(A(i, j) - B(i, j)))
         end do
      end do
   end function max_abs_diff_


   subroutine validate_spherical_average_(RhoAvg_cao, Rho_cao, AOBasis)
      !
      ! Print diagnostics of RhoAvg_cao, the spherical average of the
      ! total atomic density Rho_cao (Cartesian AOs):
      ! the electron counts in Cartesian and solid harmonic AOs, the
      ! difference between averaging in the two bases, the change of
      ! the angular mean computed by RhoSpherCoeffs, and the largest
      ! change of a density matrix element.
      !
      real(F64), dimension(:, :), contiguous, intent(in) :: RhoAvg_cao
      real(F64), dimension(:, :), contiguous, intent(in) :: Rho_cao
      type(TAOBasis), intent(in)                         :: AOBasis

      real(F64), dimension(:, :, :), allocatable :: Rho_spin
      real(F64), dimension(:, :), allocatable :: Rho_sao, RhoAvg_sao, TRhoAvgT_sao
      real(F64), dimension(:, :), allocatable :: S_cao, S_sao, Work
      real(F64), dimension(:), allocatable :: RhoSpher, RhoAvgSpher
      real(F64) :: NElectronsCart, NElectronsSpher
      real(F64) :: BasisError, SpherError, MaxChange
      integer :: NPairs

      associate ( &
         NAOCart => AOBasis%NAOCart, &
         NAOSpher => AOBasis%NAOSpher, &
         NShells => AOBasis%NShells, &
         LmaxGTO => AOBasis%LmaxGTO, &
         NormFactorsSpher => AOBasis%NormFactorsSpher, &
         NormFactorsCart => AOBasis%NormFactorsCart, &
         ShellLocSpher => AOBasis%ShellLocSpher, &
         ShellLocCart => AOBasis%ShellLocCart, &
         ShellMomentum => AOBasis%ShellMomentum, &
         ShellParamsIdx => AOBasis%ShellParamsIdx &
         )
         NPairs = (NShells * (NShells + 1)) / 2
         allocate(Rho_sao(NAOSpher, NAOSpher))
         allocate(RhoAvg_sao(NAOSpher, NAOSpher))
         allocate(TRhoAvgT_sao(NAOSpher, NAOSpher))
         allocate(S_cao(NAOCart, NAOCart))
         allocate(S_sao(NAOSpher, NAOSpher))
         allocate(Work(NAOCart, NAOSpher))
         allocate(RhoSpher(NPairs))
         allocate(RhoAvgSpher(NPairs))
         allocate(Rho_spin(NAOCart, NAOCart, 1))
         !
         ! The same average done in solid harmonics must equal
         ! T RhoAvg_cao T**T
         !
         call SpherGTO_TransformMatrix(Rho_sao, Rho_cao, &
            LmaxGTO, NormFactorsSpher, NormFactorsCart, ShellLocSpher, &
            ShellLocCart, ShellMomentum, ShellParamsIdx, NAOSpher, NAOCart, &
            NShells, Work)
         call RhoSpherAverage(RhoAvg_sao, Rho_sao, AOBasis, SpherAO=.true.)
         call SpherGTO_TransformMatrix(TRhoAvgT_sao, RhoAvg_cao, &
            LmaxGTO, NormFactorsSpher, NormFactorsCart, ShellLocSpher, &
            ShellLocCart, ShellMomentum, ShellParamsIdx, NAOSpher, NAOCart, &
            NShells, Work)
         BasisError = max_abs_diff_(TRhoAvgT_sao, RhoAvg_sao)
         call ints1e_OverlapMatrix(S_cao, AOBasis)
         call smfill(S_cao)
         call SpherGTO_TransformMatrix_U(S_sao, S_cao, &
            LmaxGTO, NormFactorsSpher, NormFactorsCart, ShellLocSpher, &
            ShellLocCart, ShellMomentum, ShellParamsIdx, NAOSpher, NAOCart, &
            NShells, Work)
         NElectronsCart = trace_ab_(RhoAvg_cao, S_cao)
         NElectronsSpher = trace_ab_(RhoAvg_sao, S_sao)
         Rho_spin(:, :, 1) = Rho_cao
         call RhoSpherCoeffs(RhoSpher, Rho_spin, AOBasis)
         Rho_spin(:, :, 1) = RhoAvg_cao
         call RhoSpherCoeffs(RhoAvgSpher, Rho_spin, AOBasis)
         SpherError = maxval(abs(RhoAvgSpher - RhoSpher))
         MaxChange = max_abs_diff_(RhoAvg_cao, Rho_cao)
      end associate

      call msg(lfield("Tr(RhoAvg S), Cartesian AOs", 36) // rfield(str(NElectronsCart, d=10), 22))
      call msg(lfield("Tr(RhoAvg S), spherical AOs", 36) // rfield(str(NElectronsSpher, d=10), 22))
      call msg(lfield("max|T RhoAvg T**T - RhoAvg_sao|", 36) // rfield(str(BasisError, d=2), 22))
      call msg(lfield("max|c(RhoAvg) - c(Rho)|", 36) // rfield(str(SpherError, d=2), 22))
      call msg(lfield("max|RhoAvg - Rho|", 36) // rfield(str(MaxChange, d=2), 22))
   end subroutine validate_spherical_average_


   subroutine task_AtomicGuess(System, SCFParams, BasisAssign)
      !
      ! Generate guess densities for all elements of System in their
      ! assigned basis sets and write them to the work directory, the
      ! folder of the input file. Each density is the spherical average
      ! of the isolated-atom Hartree-Fock density. Existing files are not
      ! overwritten. scripts/fetch_basis_to_library.py writes the input
      ! into the guess folder of every new basis set.
      !
      type(TSystem), intent(in)          :: System
      type(TSCFParams), intent(in)       :: SCFParams
      type(TBasisAssignment), intent(in) :: BasisAssign

      type(TSCFParams) :: AtomSCFParams
      type(TSCFOutput) :: AtomSCF
      type(TAOBasis) :: AtomBasis
      type(TSystem) :: Atom
      type(TBasisRule) :: Rule
      integer, dimension(:), allocatable :: ZList, ZCount, AtomElementMap
      real(F64), dimension(:, :), allocatable :: Rho_tot, RhoAvg_cao
      character(:), allocatable :: GuessPath, Element
      integer :: NElements, k, a, s, Z

      AtomSCFParams%xcfunc = XCF_HF
      AtomSCFParams%guess_type = SCF_GUESS_HBARE
      !
      ! Cholesky integrals until the exact-integral Fock build handles
      ! atoms with fewer than five shells, e.g., H and He in cc-pVDZ
      ! (fock_ShellSubsets).
      !
      AtomSCFParams%ERI_Algorithm = SCF_ERI_CHOLESKY
      allocate(AtomSCFParams%AUXIn(0, 0))

      allocate(AtomElementMap(System%NAtoms))
      call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, &
         System, SYS_ALL_ATOMS)
      do k = 1, NElements
         Z = ZList(k)
         Element = trim(ELNAME_SHORT(Z))
         a = findloc(System%ZNumbers, Z, dim=1)
         call BasisAssign%get_atom_rule(Rule, a, Z)
         GuessPath = WORKDIR // lowercase(Element) // ".txt"
         if (io_exists(GuessPath)) then
            call msg("Skipped " // Element // ": " // GuessPath // " exists")
            cycle
         end if

         call drv_IsolatedAtomSCF(AtomSCF, AtomBasis, Atom, Z, Rule, &
            AtomSCFParams, LibraryDir=BasisAssign%LibraryDir, &
            GuessDir=BasisAssign%GuessDir)
         if (.not. AtomSCF%Converged) then
            call msg("Atom SCF not converged for " // Element, MSG_ERROR)
            error stop
         end if
         !
         ! Total density: the average is linear, so averaging alpha
         ! and beta separately and adding them gives the same result
         !
         allocate(Rho_tot(AtomBasis%NAOCart, AtomBasis%NAOCart))
         allocate(RhoAvg_cao(AtomBasis%NAOCart, AtomBasis%NAOCart))
         Rho_tot = AtomSCF%Rho_cao(:, :, 1)
         do s = 2, size(AtomSCF%Rho_cao, dim=3)
            Rho_tot = Rho_tot + AtomSCF%Rho_cao(:, :, s)
         end do
         call RhoSpherAverage(RhoAvg_cao, Rho_tot, AtomBasis, SpherAO=.false.)

         call msg("Atomic guess: " // Element // ", " // Rule%DisplayedName, &
            underline=.true.)
         call msg(lfield("E(HF) [a.u.]", 36) // rfield(str(AtomSCF%EtotDFT, d=10), 22))
         call validate_spherical_average_(RhoAvg_cao, Rho_tot, AtomBasis)
         if (this_image() == 1) then
            call io_text_write(RhoAvg_cao, GuessPath)
            call msg("Saved " // GuessPath)
         end if

         deallocate(Rho_tot, RhoAvg_cao)
         call free_modules()
         call data_free()
      end do
   end subroutine task_AtomicGuess

end module drv_atomic_guess
