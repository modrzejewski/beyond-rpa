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
            logical :: initialized = .false.
            
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
      end subroutine basis_add_element_rule

      subroutine basis_add_global_fallback(BasisAssign, PathToParams)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            character(*), intent(in)               :: PathToParams
            
            BasisAssign%GlobalFallback = PathToParams
      end subroutine basis_add_global_fallback

      subroutine basis_ReadAssignLine(BasisAssign, line)
            class(TBasisAssignment), intent(inout) :: BasisAssign
            character(*), intent(in)               :: line

            character(:), allocatable :: key, val
            integer :: start_idx, end_idx, a, Z
            
            BasisAssign%initialized = .true.
            
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
