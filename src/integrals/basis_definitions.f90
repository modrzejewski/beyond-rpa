module basis_definitions
      use arithmetic
      use string
      use periodic
      use display

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

      subroutine basis_ReadAssignLine(BasisAssign, line)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            character(*), intent(in)               :: line

            character(:), allocatable :: key, val
            integer :: start_idx, end_idx, a, Z
            
            call split(line, key, val)

            if (key == "*") then
                  call BasisAssign%add_global_fallback(val)
            else if (index(key, "-") > 0) then
                  ! Atom range (e.g., 1-3)
                  read(key(1:index(key,"-")-1), *) start_idx
                  read(key(index(key,"-")+1:), *) end_idx
                  do a = start_idx, end_idx
                        call BasisAssign%add_atom_rule(a, val)
                  end do
            else if (isinteger(key)) then
                  ! Single Atom ID
                  read(key, *) a
                  call BasisAssign%add_atom_rule(a, val)
            else
                  ! Element symbol
                  Z = znumber_short(key)
                  if (Z > 0) then
                        call BasisAssign%add_element_rule(Z, val)
                  else
                        call msg("Unknown element symbol '" // key // &
                              "' in basis_assignment block.", priority=MSG_ERROR)
                        error stop
                  end if
            end if
      end subroutine basis_ReadAssignLine

end module basis_definitions
