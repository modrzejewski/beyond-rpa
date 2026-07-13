module basis_definitions
      use arithmetic
      use string
      use periodic
      use display
      use io, only: io_exists

      implicit none

      type TBasisRule
            integer :: id
            character(:), allocatable :: PathToParams
      end type TBasisRule

      type TBasisAssignment
            !
            ! Structure that maps basis set files to specific atoms or elements.
            !
            ! Fallbacks are resolved in the following priority:
            ! 1. AtomRules: Explicit atom indices (e.g., atom 3)
            ! 2. ElementRules: Element atomic numbers (e.g., Z=8 for O)
            ! 3. GlobalFallback: The fallback defined inside the `basis_assignment` block via '*'
            !
            logical :: Initialized = .false.

            type(TBasisRule), allocatable :: AtomRules(:)
            integer :: NAtomRules = 0

            type(TBasisRule), allocatable :: ElementRules(:)
            integer :: NElementRules = 0

            character(:), allocatable :: GlobalFallback
      contains
            procedure :: add_atom_rule => basis_add_atom_rule
            procedure :: add_element_rule => basis_add_element_rule
            procedure :: add_global_fallback => basis_add_global_fallback
            procedure :: read_line => basis_ReadAssignLine
            procedure :: assign => basis_assignment_copy
            generic :: assignment(=) => assign
      end type TBasisAssignment

      type TAOBasis
            !
            ! Gaussian atomic orbital basis set parameters and mapping arrays.
            !
            ! A contracted Cartesian Gaussian orbital centered on atom c at Rc has the form:
            !   Phi(r) = N * (x-Xc)**lx * (y-Yc)**ly * (z-Zc)**lz * Sum_i [ c_i * exp(-alpha_i * |r-Rc|**2) ]
            !
            ! Spherical AOs are formed by contracting Cartesian Gaussians with solid
            ! harmonic coefficients. The normalization factor N depends on whether
            ! the final AO is Cartesian or spherical (see docstrings in integrals/Auto2e).
            !
            ! Parameters:
            !   N       : Normalization constant from NormFactorsCart or NormFactorsSpher.
            !   lx,ly,lz: Cartesian polynomial exponents from CartPolyX/Y/Z.
            !   c_i     : Contraction coefficients from CntrCoeffs.
            !   alpha_i : Gaussian exponents from Exponents.
            !   Rc      : (Xc, Yc, Zc) coordinates of atom c from AtomCoords.
            !
            ! Scalars:
            !   SpherAO          : True if spherical AOs are preferred, but parameters for both spherical
            !                      and Cartesian AOs are present in this structure.
            !   NShellParams     : Total number of unique shell parameter sets. Using atom-specific basis
            !                      assignments can result in different basis sets for atoms of the same
            !                      element, increasing this number.
            !   NShells          : Total number of shells instantiated on atoms.
            !   LmaxGTO          : Maximum angular momentum in the basis.
            !   MaxNPrimitives   : Maximum primitives in any single shell.
            !   NAOSpher         : Total number of spherical basis functions.
            !   NAOCart          : Total number of Cartesian basis functions.
            !   NAtoms           : Number of atoms.
            !   MaxNShells       : Maximum shells assigned to any single atom.
            !
            ! Arrays:
            !   AtomCoords       : (3, NAtoms) Cartesian coordinates of atoms.
            !   ShellCenters     : (NShells) Atom index each shell is centered on.
            !   ShellParamsIdx   : (NShells) Index mapping a shell to its unique parameter set.
            !   ShellMomentum    : (NShellParams) Angular momentum quantum number (L).
            !   AtomShellMap     : (2, MaxNShellSegments, NAtoms) Start and end shell indices for
            !                      contiguous segments.
            !   AtomShellN       : (NAtoms) Number of contiguous shell segments per atom.
            !   NPrimitives      : (NShellParams) Number of Gaussian primitives per shell.
            !   CntrCoeffs       : (MaxNPrimitives, NShellParams) Contraction coefficients.
            !   Exponents        : (MaxNPrimitives, NShellParams) Gaussian exponents.
            !   NormFactorsCart  : (MaxNAngFuncCart, NShellParams) Normalization constants for
            !                      Cartesian functions.
            !   NormFactorsSpher : (MaxNAngFuncSpher, NShellParams) Normalization constants for
            !                      spherical functions.
            !   NAngFuncSpher    : (NShellParams) Number of spherical functions (2L+1).
            !   NAngFuncCart     : (NShellParams) Number of Cartesian functions.
            !   ShellLocSpher    : (NShells) Global starting index in the spherical basis.
            !   ShellLocCart     : (NShells) Global starting index in the Cartesian basis.
            !   CartPolyX/Y/Z    : (MaxNAngFuncCart, 0:LmaxGTO) Cartesian polynomial exponents (lx, ly, lz)
            !                      defining the angular part x**lx * y**ly * z**lz.
            !   R2Max            : (NShellParams) Effective squared radial extent of an orbital used
            !                      for grid screening.
            !   MaxAtomL         : (NAtoms) Maximum angular momentum on a given atom.
            !
            real(F64), dimension(:, :), allocatable :: AtomCoords
            integer, dimension(:), allocatable :: ShellCenters
            integer, dimension(:), allocatable :: ShellParamsIdx
            integer, dimension(:), allocatable :: ShellMomentum
            integer, dimension(:, :, :), allocatable :: AtomShellMap
            integer, dimension(:), allocatable :: AtomShellN
            integer, dimension(:), allocatable :: NPrimitives
            real(F64), dimension(:, :), allocatable :: CntrCoeffs
            real(F64), dimension(:, :), allocatable :: Exponents
            real(F64), dimension(:, :), allocatable :: NormFactorsCart
            real(F64), dimension(:, :), allocatable :: NormFactorsSpher
            integer, dimension(:), allocatable :: NAngFuncSpher
            integer, dimension(:), allocatable :: NAngFuncCart
            integer, dimension(:), allocatable :: ShellLocSpher
            integer, dimension(:), allocatable :: ShellLocCart
            integer, dimension(:, :), allocatable :: CartPolyX
            integer, dimension(:, :), allocatable :: CartPolyY
            integer, dimension(:, :), allocatable :: CartPolyZ
            real(F64), dimension(:), allocatable :: R2Max
            integer, dimension(:), allocatable :: MaxAtomL
            logical :: SpherAO
            integer :: NShellParams
            integer :: NShells
            integer :: LmaxGTO
            integer :: MaxNPrimitives
            integer :: NAOSpher
            integer :: NAOCart
            integer :: NAtoms
            integer :: MaxNShells
      end type TAOBasis

contains

      subroutine basis_add_atom_rule(BasisAssign, a, PathToParams)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            integer, intent(in)                    :: a
            character(*), intent(in)               :: PathToParams
            type(TBasisRule), allocatable :: temp(:)
            integer :: i

            if (allocated(BasisAssign%AtomRules)) then
                  allocate(temp(BasisAssign%NAtomRules + 1))
                  do i = 1, BasisAssign%NAtomRules
                        temp(i)%id = BasisAssign%AtomRules(i)%id
                        temp(i)%PathToParams = BasisAssign%AtomRules(i)%PathToParams
                  end do
                  temp(BasisAssign%NAtomRules + 1)%id = a
                  temp(BasisAssign%NAtomRules + 1)%PathToParams = PathToParams
                  call move_alloc(temp, BasisAssign%AtomRules)
            else
                  allocate(BasisAssign%AtomRules(1))
                  BasisAssign%AtomRules(1)%id = a
                  BasisAssign%AtomRules(1)%PathToParams = PathToParams
            end if
            BasisAssign%NAtomRules = BasisAssign%NAtomRules + 1
            BasisAssign%Initialized = .true.
      end subroutine basis_add_atom_rule

      subroutine basis_add_element_rule(BasisAssign, Z, PathToParams)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            integer, intent(in)                    :: Z
            character(*), intent(in)               :: PathToParams
            type(TBasisRule), allocatable :: temp(:)
            integer :: i

            if (allocated(BasisAssign%ElementRules)) then
                  allocate(temp(BasisAssign%NElementRules + 1))
                  do i = 1, BasisAssign%NElementRules
                        temp(i)%id = BasisAssign%ElementRules(i)%id
                        temp(i)%PathToParams = BasisAssign%ElementRules(i)%PathToParams
                  end do
                  temp(BasisAssign%NElementRules + 1)%id = Z
                  temp(BasisAssign%NElementRules + 1)%PathToParams = PathToParams
                  call move_alloc(temp, BasisAssign%ElementRules)
            else
                  allocate(BasisAssign%ElementRules(1))
                  BasisAssign%ElementRules(1)%id = Z
                  BasisAssign%ElementRules(1)%PathToParams = PathToParams
            end if
            BasisAssign%NElementRules = BasisAssign%NElementRules + 1
            BasisAssign%Initialized = .true.
      end subroutine basis_add_element_rule

      subroutine basis_add_global_fallback(BasisAssign, PathToParams)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            character(*), intent(in)               :: PathToParams

            BasisAssign%GlobalFallback = PathToParams
            BasisAssign%Initialized = .true.
      end subroutine basis_add_global_fallback

      subroutine basis_ReadAssignLine(BasisAssign, line, LibraryDir)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            character(*), intent(in)               :: line
            character(*), intent(in)               :: LibraryDir

            character(:), allocatable :: key, val, ResolvedPath
            character(:), allocatable :: DummyName, DummyBaseName
            integer :: start_idx, end_idx, a, Z

            call split(line, key, val)

            call basis_ResolvePath(ResolvedPath, DummyName, DummyBaseName, val, LibraryDir)

            if (key == "*") then
                  call BasisAssign%add_global_fallback(ResolvedPath)
            else if (index(key, "-") > 0) then
                  ! Atom range (e.g., 1-3)
                  read(key(1:index(key,"-")-1), *) start_idx
                  read(key(index(key,"-")+1:), *) end_idx
                  do a = start_idx, end_idx
                        call BasisAssign%add_atom_rule(a, ResolvedPath)
                  end do
            else if (isinteger(key)) then
                  ! Single Atom ID
                  read(key, *) a
                  call BasisAssign%add_atom_rule(a, ResolvedPath)
            else
                  ! Element symbol
                  Z = znumber_short(key)
                  if (Z > 0) then
                        call BasisAssign%add_element_rule(Z, ResolvedPath)
                  else
                        call msg("Unknown element symbol '" // key // &
                              "' in basis_assignment block.", priority=MSG_ERROR)
                        error stop
                  end if
            end if
      end subroutine basis_ReadAssignLine

      subroutine basis_assignment_copy(lhs, rhs)
            class(TBasisAssignment), intent(out) :: lhs
            class(TBasisAssignment), intent(in)  :: rhs
            integer :: i

            lhs%Initialized = rhs%Initialized
            lhs%NAtomRules = rhs%NAtomRules
            lhs%NElementRules = rhs%NElementRules

            if (rhs%NAtomRules > 0) then
                  allocate(lhs%AtomRules, source=rhs%AtomRules)
            end if

            if (rhs%NElementRules > 0) then
                  allocate(lhs%ElementRules, source=rhs%ElementRules)
            end if

            if (allocated(rhs%GlobalFallback)) then
                  lhs%GlobalFallback = rhs%GlobalFallback
            end if
      end subroutine basis_assignment_copy

      subroutine basis_ResolvePath(FullParamsPath, ParamsDisplayedName, ParamsBaseName, ValString, LibraryDir)
            !
            ! Map an EMSL basis set name or custom file path to an absolute path.
            !
            ! Parameters:
            !   FullParamsPath      : Output absolute path to the basis set parameters file.
            !   ParamsDisplayedName : Output standard name of the basis set. Suitable for displaying output messages.
            !   ParamsBaseName      : Output basis set params filename without directory path or extension.
            !   ValString           : Input basis string, either an EMSL alias or 'FILE path'.
            !   LibraryDir          : Input path to the directory containing standard basis sets.
            !
            character(:), allocatable, intent(out)           :: FullParamsPath
            character(:), allocatable, intent(out)           :: ParamsDisplayedName
            character(:), allocatable, intent(out)           :: ParamsBaseName
            character(*), intent(in)                         :: ValString
            character(*), intent(in)                         :: LibraryDir

            character(:), allocatable :: a1, a2, p, n

            call split(ValString, a1, a2)
            if (uppercase(a1) == "FILE") then
                  if (io_exists(a2)) then
                        p = a2
                        n = a2
                        ParamsBaseName = a2
                  else
                        call msg("Basis set coefficients file is inaccessible: " // a2, MSG_ERROR)
                        error stop
                  end if
            else
                  ! Standard EMSL basis sets mapped here
                  select case(uppercase(ValString))
                  case ("AHLRICHS-VDZ")
                        p = "ahlrichs-vdz"
                        n = "Ahlrichs VDZ"
                  case ("3-21G")
                        p = "3-21g"
                        n = "3-21G"
                  case ("3-21++G")
                        p = "3-21g_diff_all"
                        n = "3-21++G"
                  case ("6-31G")
                        p = "6-31g"
                        n = "6-31G"
                  case ("6-31G**")
                        p = "6-31g_pol_all"
                        n = "6-31G**"
                  case ("6-31++G")
                        p = "6-31g_diff_all"
                        n = "6-31++G"
                  case ("6-31+G**")
                        p = "6-31g_diff_pol_all"
                        n = "6-31+G**"
                  case ("6-31++G**")
                        p = "6-31g_diff_all_pol_all"
                        n = "6-31++G**"
                  case ("6-31G(3DF,3PD)", "6-31G(3DF, 3PD)")
                        p = "6-31g_3df_3pd"
                        n = "6-31G(3df,3dp)"
                  case ("6-311G")
                        p = "6-311g"
                        n = "6-311G"
                  case ("6-311G**")
                        p = "6-311g_pol_all"
                        n = "6-311G**"
                  case ("6-311+G*")
                        p = "6-311g_diff_pol"
                        n = "6-311+G*"
                  case ("6-311++G**")
                        p = "6-311g_diff_all_pol_all"
                        n = "6-311++G**"
                  case ("6-311++G(2D,2P)", "6-311++G(2D, 2P)")
                        p = "6-311g_diff_all_2d_2p"
                        n = "6-311++G(2d,2p)"
                  case ("6-311++G(3DF,3PD)", "6-311++G(3DF, 3PD)")
                        p = "6-311g_diff_all_3df_3pd"
                        n = "6-311++G(3df,3pd)"
                  case ("6-311(3+,3+)G**", "6-311(3+, 3+)G**", "6-311(3+,3+)G(d,p)", &
                        "6-311(3+, 3+)G(d, p)")
                        p = "6-311g_triple_diff_all_pol_all"
                        n = "6-311(3+,3+)G**"
                  case ("CC-PVDZ", "CC-PV(D+D)Z")
                        p = "cc-pvddz"
                        n = "cc-pVDZ"
                  case ("CC-PVDZ_OLD", "CC-PVDZ-OLD")
                        p = "cc-pvdz"
                        n = "cc-pVDZ (old)"
                  case ("CC-PVDZ-PP")
                        p = "cc-pvdz-pp"
                        n = "cc-pVDZ-PP"
                  case ("AUG-CC-PVDZ", "AUG-CC-PV(D+D)Z")
                        p = "aug-cc-pvddz"
                        n = "aug-cc-pVDZ"
                  case ("AUG-CC-PVDZ_OLD", "AUG-CC-PVDZ-OLD")
                        p = "aug-cc-pvdz"
                        n = "aug-cc-pVDZ (old)"
                  case ("AUG-CC-PVDZ-PP")
                        p = "aug-cc-pvdz-pp"
                        n = "aug-cc-pVDZ-PP"
                  case ("D-AUG-CC-PVDZ")
                        p = "d-aug-cc-pvdz"
                        n = "d-aug-cc-pVDZ"
                  case ("CC-PVTZ", "CC-PV(T+D)Z")
                        p = "cc-pvtdz"
                        n = "cc-pVTZ"
                  case ("CC-PVTZ_OLD", "CC-PVTZ-OLD")
                        p = "cc-pvtz"
                        n = "cc-pVTZ (old)"
                  case ("CC-PVTZ-PP")
                        p = "cc-pvtz-pp"
                        n = "cc-pVTZ-PP"
                  case ("AUG-CC-PVTZ", "AUG-CC-PV(T+D)Z")
                        p = "aug-cc-pvtdz"
                        n = "aug-cc-pVTZ"
                  case ("AUG-CC-PVTZ_OLD", "AUG-CC-PVTZ-OLD")
                        p = "aug-cc-pvtz"
                        n = "aug-cc-pVTZ (old)"
                  case ("AUG-CC-PVTZ-PP")
                        p = "aug-cc-pvtz-pp"
                        n = "aug-cc-pVTZ-PP"
                  case ("D-AUG-CC-PVTZ")
                        p = "d-aug-cc-pvtz"
                        n = "d-aug-cc-pVTZ"
                  case ("CC-PVQZ", "CC-PV(Q+D)Z")
                        p = "cc-pvqdz"
                        n = "cc-pVQZ"
                  case ("CC-PVQZ_OLD", "CC-PVQZ-OLD")
                        p = "cc-pvqz"
                        n = "cc-pVQZ (old)"
                  case ("AUG-CC-PVQZ", "AUG-CC-PV(Q+D)Z")
                        p = "aug-cc-pvqdz"
                        n = "aug-cc-pVQZ"
                  case ("AUG-CC-PVQZ_OLD", "AUG-CC-PVQZ-OLD")
                        p = "aug-cc-pvqz"
                        n = "aug-cc-pVQZ (old)"
                  case ("D-AUG-CC-PVQZ")
                        p = "d-aug-cc-pvqz"
                        n = "d-aug-cc-pVQZ"
                  case ("CC-PV5Z", "CC-PV(5+D)Z")
                        p = "cc-pv5dz"
                        n = "cc-pV5Z"
                  case ("CC-PV5Z_OLD", "CC-PV5Z-OLD")
                        p = "cc-pv5z"
                        n = "cc-pV5Z (old)"
                  case ("AUG-CC-PV5Z", "AUG-CC-PV(5+D)Z")
                        p = "aug-cc-pv5dz"
                        n = "aug-cc-pV5Z"
                  case ("AUG-CC-PV5Z_OLD", "AUG-CC-PV5Z-OLD")
                        p = "aug-cc-pv5z"
                        n = "aug-cc-pV5Z (old)"
                  case ("D-AUG-CC-PV5Z")
                        p = "d-aug-cc-pv5z"
                        n = "d-aug-cc-pV5Z"
                  case ("CC-PCVTZ")
                        p = "cc-pcvtz"
                        n = "cc-pCVTZ"
                  case ("AUG-CC-PCVTZ")
                        p = "aug-cc-pcvtz"
                        n = "aug-cc-pCVTZ"
                  case ("CC-PCVQZ")
                        p = "cc-pcvqz"
                        n = "cc-pCVQZ"
                  case ("AUG-CC-PCVQZ")
                        p = "aug-cc-pcvqz"
                        n = "aug-cc-pCVQZ"
                        ! ----------------------------------------------
                        !               (aug-)cc-pwCXZ
                        ! ----------------------------------------------
                  case ("CC-PWCVQZ")
                        p = "cc-pwcvqz"
                        n = "cc-pwCVQZ"
                  case ("AUG-CC-PWCVQZ")
                        p = "aug-cc-pwcvqz"
                        n = "aug-cc-pwCVQZ"
                  case ("CC-PWCV5Z")
                        p = "cc-pwcv5z"
                        n = "cc-pwCV5Z"
                  case ("AUG-CC-PWCV5Z")
                        p = "aug-cc-pwcv5z"
                        n = "aug-cc-pwCV5Z"
                  case ("DEF2-QZVP")
                        p = "def2-qzvp"
                        n = "Def2-QZVP"
                  case ("DEF2-QZVPD")
                        p = "def2-qzvpd"
                        n = "Def2-QZVPD"
                  case ("DEF2-QZVPP")
                        p = "def2-qzvpp"
                        n = "Def2-QZVPP"
                  case ("DEF2-QZVPPD")
                        p = "def2-qzvppd"
                        n = "Def2-QZVPPD"
                  case ("DEF2-SV(P)")
                        p = "def2-sv_p"
                        n = "Def2-SV(P)"
                  case ("DEF2-SVP")
                        p = "def2-svp"
                        n = "Def2-SVP"
                  case ("DEF2-SVPD")
                        p = "def2-svpd"
                        n = "Def2-SVPD"
                  case ("DEF2-TZVP")
                        p = "def2-tzvp"
                        n = "Def2-TZVP"
                  case ("DEF2-TZVPD")
                        p = "def2-tzvpd"
                        n = "Def2-TZVPD"
                  case ("DEF2-TZVPP")
                        p = "def2-tzvpp"
                        n = "Def2-TZVPP"
                  case ("DEF2-TZVPPD")
                        p = "def2-tzvppd"
                        n = "Def2-TZVPPD"
                  case ("SADLEJ-PVTZ")
                        p = "sadlej-pvtz"
                        n = "Sadlej-pVTZ"
                  case ("SAPPORO-QZP-2012")
                        p = "sapporo-qzp-2012"
                        n = "Sapporo-QZP-2012"
                  case ("CRENBL")
                        p = "crenbl"
                        n = "CRENBL"
                  case default
                        call msg("Unknown basis set", MSG_ERROR)
                        error stop
                  end select

                  ParamsBaseName = p
                  p = LibraryDir // p // ".txt"
            end if

            FullParamsPath = p
            ParamsDisplayedName = n
      end subroutine basis_ResolvePath
end module basis_definitions
