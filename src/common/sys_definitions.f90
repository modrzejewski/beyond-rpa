module sys_definitions
      use arithmetic
      use periodic
      use string
      use sort
      use display
      use io
      use basis_definitions
      use ecp_definitions
      
      implicit none
      !
      ! Units used in the xyz file
      !
      integer, parameter :: SYS_UNITS_ANGSTROM = 1
      integer, parameter :: SYS_UNITS_BOHR = 2      
      !
      ! Subsystems required for single points, two-body interaction energies,
      ! and three-body nonadditive interaction energies
      !
      integer, parameter :: SYS_TOTAL = 1
      integer, parameter :: SYS_MONO_A = 2
      integer, parameter :: SYS_MONO_B = 3
      integer, parameter :: SYS_MONO_C = 4
      integer, parameter :: SYS_DIMER_AB = 5
      integer, parameter :: SYS_DIMER_BC = 6
      integer, parameter :: SYS_DIMER_AC = 7
      !
      ! Subsystems required for four-body nonadditive interaction energies
      !
      integer, parameter :: SYS_MONO_D = 8
      integer, parameter :: SYS_DIMER_AD = 9
      integer, parameter :: SYS_DIMER_BD = 10
      integer, parameter :: SYS_DIMER_CD = 11
      integer, parameter :: SYS_TRIMER_ABC = 12
      integer, parameter :: SYS_TRIMER_ABD = 13
      integer, parameter :: SYS_TRIMER_ACD = 14
      integer, parameter :: SYS_TRIMER_BCD = 15

      integer, parameter :: SYS_NONE = 0
      integer, parameter :: SYS_MOLECULE = 1
      integer, parameter :: SYS_DIMER = 2
      integer, parameter :: SYS_TRIMER = 3
      integer, parameter :: SYS_TETRAMER = 4

      integer, parameter :: SYS_ALL_ATOMS = 1
      integer, parameter :: SYS_REAL_ATOMS = 2
      integer, parameter :: SYS_GHOST_ATOMS = 3

      integer, parameter :: SYS_NO_PSEUDOPOTENTIAL = 0

      type TECPParams
            !
            ! True if this ECP subsystem has been initialized with parameters
            !
            logical :: Initialized = .false.
            !
            ! Number of embedding centers carrying an ECP.
            ! For the QM part of the system, this is 0 and
            ! attributes of TSystem (e.g. NAtoms) shall be used instead.
            !
            integer :: NEmbCenters = 0
            !
            ! Cartesian coordinates of ECP centers in bohr (3, NEmbCenters).
            ! Allocated only if TECPParams describes ECPs on embedding centers.
            ! For the QM part of the system, this array is unallocated
            ! and System%AtomCoords shall be used instead.
            !
            real(F64), dimension(:, :), allocatable :: Coords
            !
            ! Atomic numbers which identify the ECP centers (NEmbCenters).
            ! Allocated only if TECPParams describes ECPs on embedding centers.
            ! For the QM part of the system, this array is unallocated
            ! and System%ZNumbers shall be used instead.
            !
            integer, dimension(:), allocatable :: Z
            !
            ! Pseudopotential assignment rules inherited from TBasisAssignment
            !
            type(TECPAssignment) :: Assignment
      contains
            procedure :: free => ecp_params_free
      end type TECPParams

      type TSystem
            !
            ! Molecular or atomic system properties and configuration.
            ! Atomic centers are divided into real atoms and ghosts. One can 
            ! switch between different subsystems (e.g., dimers belonging to a 
            ! trimer ABC) using sys_Init
            ! ---
            !
            ! Cartesian xyz coordinates of atoms. The first dimension corresponds 
            ! to the x, y, z components and the second to the atom index
            !
            real(F64), dimension(:, :), allocatable :: AtomCoords
            !
            ! Nuclear charges (not affected by pseudopotentials)
            !
            integer, dimension(:), allocatable :: ZNumbers
            !
            ! Effective nuclear charges applied when a pseudopotential 
            ! is present. Unallocated if ECPs are not used
            !
            integer, dimension(:), allocatable :: ZNumbersECP
            !
            ! True if effective core potentials are used
            !
            logical :: ECPCharges = .false.
            !
            ! Additional point charges used to generate an electrostatic
            ! field for the molecule
            !
            integer :: NPointCharges = 0
            real(F64), dimension(:), allocatable :: PointCharges
            real(F64), dimension(:, :), allocatable :: PointChargeCoords
            !
            ! Pseudopotential parameters for quantum chemical and embedding regions
            !
            type(TECPParams) :: ECP
            type(TECPParams) :: EmbeddingECP
            !
            ! Spin multiplicity of the system (1 for singlet, 2 for doublet, etc.). This 
            ! variable is updated to the active subsystem's multiplicity when sys_Init is called
            !
            integer :: Mult = 1
            integer :: Charge = 0
            integer :: NElectrons
            !
            ! Number of atomic centers (real atoms + ghosts if present)
            !
            integer :: NAtoms = 0
            !
            ! Start and end indices for up to two contiguous segments of real atoms.
            ! A second segment allows for non-contiguous ranges (e.g., dimer AC
            ! from trimer ABC). Contiguous systems disable the second segment by
            ! setting its start index greater than its end index. Iteration is
            ! performed by an outer loop over segments and an inner loop over indices
            !
            integer, dimension(2, 2) :: RealAtoms
            !
            ! Number of atoms in each basic monomer fragment. The array has four 
            ! components because the code supports many-body complexes up to 
            ! tetramers. Indices 1-4 correspond to monomers A, B, C, and D
            !
            integer, dimension(4) :: SubsystemAtoms = [0, 0, 0, 0]
            !
            ! Molecular charge of each basic monomer fragment, using the same 
            ! indexing convention as SubsystemAtoms (1-4 for monomers A-D)
            !
            integer, dimension(4) :: SubsystemCharges = [0, 0, 0, 0]
            !
            ! Spin multiplicities for various subsystem combinations. The value
            ! for the active subsystem is selected by indexing this array with
            ! the SubsystemKind constant (e.g., SYS_TOTAL, SYS_MONO_A, SYS_DIMER_AB),
            ! which maps directly to its storage layout
            !
            integer, dimension(15) :: SubsystemMult = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
            !
            ! Type of the overall composite system (SYS_MOLECULE, SYS_DIMER,
            ! SYS_TRIMER, or SYS_TETRAMER). SYS_MOLECULE corresponds to a single 
            ! system without any consideration of subsystems
            !
            integer :: SystemKind = SYS_NONE
            !
            ! The specific subsystem currently active (e.g., SYS_MONO_A, SYS_DIMER_AB) 
            ! selected by sys_Init
            !
            integer :: SubsystemKind = SYS_NONE
            !
            ! Matrix of sorted interatomic distances, used to accelerate the 
            ! evaluation of Becke integration weights by screening distant atoms
            !
            real(F64), dimension(:, :), allocatable :: SortedDistances
            !
            ! Original atomic indices corresponding to the sorted interatomic distances.
            ! For example, SortedDistancesIdx(m, j) yields the global atom index of the 
            ! m-th closest atom to center j, which can be used to access AtomCoords
            !
            integer, dimension(:, :), allocatable :: SortedDistancesIdx
      contains
            procedure :: create_ecp_configs => sys_CreateECPConfigs
            procedure :: read_xyz => sys_Read_XYZ
            procedure :: read_embedding => sys_Read_Embedding
            procedure :: read_ecp => sys_ReadECP
      end type TSystem

contains

      subroutine ecp_params_free(this)
            !
            ! Deallocate ECP parameter arrays and reset counters.
            !
            class(TECPParams), intent(out) :: this
      end subroutine ecp_params_free


      subroutine sys_AddECPConfig_(Configs, AtomConfigMap, NConfigs, a, Z, Rule)
            !
            ! Query and deduplicate pseudopotential configuration for center a.
            !
            type(TECPConfig), intent(inout)                 :: Configs(:)
            integer, dimension(:), intent(inout)            :: AtomConfigMap
            integer, intent(inout)                          :: NConfigs
            integer, intent(in)                             :: a
            integer, intent(in)                             :: Z
            type(TBasisRule), intent(in)                    :: Rule

            logical :: found, spin_orbit
            integer :: c, lmax, ngauss, ncoreel
            character(:), allocatable :: PathToParams, citation

            PathToParams = Rule%PathToParams

            call pp_queryecp(PathToParams, Z, lmax, ngauss, ncoreel, spin_orbit, citation)
            if (ngauss <= 0) then
                  !
                  ! No pseudopotential data for the given element
                  ! were found in the parameter file. This is a standard
                  ! thing that happens when we check if there is any default
                  ! pseudopotential data associated with the chosen basis
                  ! set in the same basis set file.
                  !
                  AtomConfigMap(a) = SYS_NO_PSEUDOPOTENTIAL
                  return
            end if
            !
            ! Deduplicate ECP configs
            !
            found = .false.
            do c = 1, NConfigs
                  if (Configs(c)%Z == Z .and. Configs(c)%PathToParams == PathToParams) then
                        AtomConfigMap(a) = c
                        found = .true.
                        exit
                  end if
            end do

            if (.not. found) then
                  NConfigs = NConfigs + 1
                  Configs(NConfigs)%Z = Z
                  Configs(NConfigs)%PathToParams = PathToParams
                  Configs(NConfigs)%NCoreEl = ncoreel
                  Configs(NConfigs)%Lmax = lmax
                  Configs(NConfigs)%NGauss = ngauss
                  Configs(NConfigs)%SpinOrbit = spin_orbit
                  Configs(NConfigs)%Citation = citation
                  AtomConfigMap(a) = NConfigs
            end if
      end subroutine sys_AddECPConfig_


      subroutine sys_CreateECPConfigs(this, Configs, AtomConfigMap, NConfigs, embedding)
            !
            ! Generate unique pseudopotential configurations and mapping array for
            ! QM atoms or embedding centers.
            !
            class(TSystem), intent(in)                      :: this
            type(TECPConfig), allocatable, intent(out)      :: Configs(:)
            integer, dimension(:), allocatable, intent(out) :: AtomConfigMap
            integer, intent(out)                            :: NConfigs
            logical, optional, intent(in)                   :: embedding

            logical :: embedding_, found
            integer :: a, c, s, Z, NCenters
            type(TBasisRule) :: Rule
            type(TECPConfig), allocatable :: TempConfigs(:)

            if (present(embedding)) then
                  embedding_ = embedding
            else
                  embedding_ = .false.
            end if

            if (embedding_) then
                  NCenters = this%EmbeddingECP%NEmbCenters
                  allocate(AtomConfigMap(NCenters))
                  AtomConfigMap = SYS_NO_PSEUDOPOTENTIAL
                  NConfigs = 0

                  if (NCenters == 0 .or. .not. this%EmbeddingECP%Assignment%Initialized) then
                        allocate(Configs(0))
                        return
                  end if

                  allocate(Configs(NCenters))
                  do a = 1, NCenters
                        Z = this%EmbeddingECP%Z(a)
                        call this%EmbeddingECP%Assignment%get_atom_rule(Rule, a, Z, found)
                        
                        if (.not. found) then
                              call msg("No pseudopotential assigned for embedding center " // str(a) // &
                                    " (" // trim(ELNAME_SHORT(Z)) // ")", MSG_ERROR)
                              error stop
                        end if
                        !
                        ! Check for the existence of ECP data for the given center. It is
                        ! required that the data exist.
                        !
                        call sys_AddECPConfig_(Configs, AtomConfigMap, NConfigs, a, Z, Rule)
                        
                        if (AtomConfigMap(a) == SYS_NO_PSEUDOPOTENTIAL) then
                              call msg("No pseudopotential found for embedding center " // str(a) // &
                                    " (" // trim(ELNAME_SHORT(Z)) // ")", MSG_ERROR)
                              error stop
                        end if
                  end do
            else
                  NCenters = this%NAtoms
                  allocate(AtomConfigMap(NCenters))
                  AtomConfigMap = SYS_NO_PSEUDOPOTENTIAL
                  NConfigs = 0

                  if (.not. this%ECP%Assignment%Initialized) then
                        allocate(Configs(0))
                        return
                  end if

                  allocate(TempConfigs(NCenters))
                  do s = 1, 2
                        !
                        ! We skip the ghost centers because they provide only
                        ! basis set functions, not pseudopotentials. ECPs
                        ! on the embedding centers are handled in EmbeddingECP.
                        !
                        do a = this%RealAtoms(1, s), this%RealAtoms(2, s)
                              Z = this%ZNumbers(a)
                              call this%ECP%Assignment%get_atom_rule(Rule, a, Z, found)
                              if (found) then
                                    !
                                    ! Try to extract ECP parameters from the given
                                    ! parameter file. AtomConfigMap will keep SYS_NO_PSEUDOPOTENTIAL
                                    ! if no data is found.
                                    !
                                    call sys_AddECPConfig_(TempConfigs, AtomConfigMap, NConfigs, a, Z, Rule)
                              end if
                        end do
                  end do

                  allocate(Configs(NConfigs))
                  do c = 1, NConfigs
                        Configs(c) = TempConfigs(c)
                  end do
            end if
      end subroutine sys_CreateECPConfigs


      subroutine sys_SetEffectiveCores_(this)
            !
            ! Set system parameters that depend on effective core potentials.
            !
            class(TSystem), intent(inout) :: this

            integer :: a, c
            integer :: NConfigs
            type(TECPConfig), allocatable :: Configs(:)
            integer, dimension(:), allocatable :: AtomConfigMap
            integer, dimension(:), allocatable :: EffectiveCores

            call this%create_ecp_configs(Configs, AtomConfigMap, NConfigs)

            allocate(EffectiveCores(this%NAtoms))
            EffectiveCores = 0
            do a = 1, this%NAtoms
                  c = AtomConfigMap(a)
                  if (c /= SYS_NO_PSEUDOPOTENTIAL) then
                        EffectiveCores(a) = Configs(c)%NCoreEl
                  end if
            end do

            this%ECPCharges = any(EffectiveCores > 0)

            if (allocated(this%ZNumbersECP)) deallocate(this%ZNumbersECP)
            allocate(this%ZNumbersECP(this%NAtoms))
            this%ZNumbersECP = this%ZNumbers - EffectiveCores

            if (this%SubsystemKind /= SYS_NONE) then
                  call sys_Init(this, this%SubsystemKind)
            else
                  call sys_Init(this, SYS_TOTAL)
            end if
      end subroutine sys_SetEffectiveCores_


      subroutine sys_Init(System, i)
            type(TSystem), intent(inout) :: System
            integer, intent(in)          :: i

            integer :: p0, p1, q0, q1, s
            integer :: a0, a1, b0, b1, c0, c1, d0, d1
            
            associate (RealAtoms=>System%RealAtoms, NAtoms=>System%NAtoms, Charge=>System%Charge, &
                  SubsystemAtoms=>System%SubsystemAtoms, Mult=>System%Mult, SubsystemMult=>System%SubsystemMult, &
                  SubsystemCharges=>System%SubsystemCharges, SystemKind=>System%SystemKind, ZNumbers=>System%ZNumbers, &
                  NElectrons=>System%NElectrons, SubsystemKind=>System%SubsystemKind, ECPCharges=>System%ECPCharges, &
                  ZNumbersECP=>System%ZNumbersECP)
                  SubsystemKind = i
                  Charge = sum(SubsystemCharges)
                  NAtoms = sum(SubsystemAtoms)
                  p0 = 1
                  p1 = NAtoms
                  !
                  ! When q1<q0, the loops over atoms q0...q1 perform zero cycles.
                  ! That's why q0 and q1 are modified only for non-contiguous
                  ! ranges of atoms.
                  !
                  q0 = 1
                  q1 = 0
                  select case (SystemKind)
                  case (SYS_DIMER)
                        Charge = sum(SubsystemCharges(1:2))
                        select case (i)
                        case (SYS_MONO_A)
                              p0 = 1
                              p1 = SubsystemAtoms(1)
                              Charge = SubsystemCharges(1)
                        case (SYS_MONO_B)
                              p0 = SubsystemAtoms(1) + 1
                              p1 = NAtoms
                              Charge = SubsystemCharges(2)
                        end select
                  case (SYS_TRIMER)
                        Charge = sum(SubsystemCharges(1:3))
                        select case (i)
                        case (SYS_MONO_A)
                              p0 = 1
                              p1 = SubsystemAtoms(1)
                              Charge = SubsystemCharges(1)
                        case (SYS_MONO_B)
                              p0 = SubsystemAtoms(1) + 1
                              p1 = SubsystemAtoms(1) + SubsystemAtoms(2)
                              Charge = SubsystemCharges(2)
                        case (SYS_MONO_C)
                              p0 = SubsystemAtoms(1) + SubsystemAtoms(2) + 1
                              p1 = NAtoms
                              Charge = SubsystemCharges(3)
                        case (SYS_DIMER_AB)
                              p0 = 1
                              p1 = SubsystemAtoms(1) + SubsystemAtoms(2)
                              Charge = SubsystemCharges(1) + SubsystemCharges(2)
                        case (SYS_DIMER_BC)
                              p0 = SubsystemAtoms(1) + 1
                              p1 = NAtoms
                              Charge = SubsystemCharges(2) + SubsystemCharges(3)
                        case (SYS_DIMER_AC)
                              p0 = 1
                              p1 = SubsystemAtoms(1)
                              q0 = SubsystemAtoms(1) + SubsystemAtoms(2) + 1
                              q1 = NAtoms
                              Charge = SubsystemCharges(1) + SubsystemCharges(3)
                        end select
                  case (SYS_TETRAMER)
                        Charge = sum(SubsystemCharges(1:4))
                        a0 = 1
                        a1 = SubsystemAtoms(1)
                        b0 = SubsystemAtoms(1) + 1
                        b1 = SubsystemAtoms(1) + SubsystemAtoms(2)
                        c0 = SubsystemAtoms(1) + SubsystemAtoms(2) + 1
                        c1 = SubsystemAtoms(1) + SubsystemAtoms(2) + SubsystemAtoms(3)
                        d0 = SubsystemAtoms(1) + SubsystemAtoms(2) + SubsystemAtoms(3) + 1
                        d1 = SubsystemAtoms(1) + SubsystemAtoms(2) + SubsystemAtoms(3) + SubsystemAtoms(4)
                        select case (i)
                        case (SYS_MONO_A)
                              p0 = a0
                              p1 = a1
                              Charge = SubsystemCharges(1)
                        case (SYS_MONO_B)
                              p0 = b0
                              p1 = b1
                              Charge = SubsystemCharges(2)
                        case (SYS_MONO_C)
                              p0 = c0
                              p1 = c1
                              Charge = SubsystemCharges(3)
                        case (SYS_DIMER_AB)
                              p0 = a0
                              p1 = b1
                              Charge = SubsystemCharges(1) + SubsystemCharges(2)
                        case (SYS_DIMER_BC)
                              p0 = b0
                              p1 = c1
                              Charge = SubsystemCharges(2) + SubsystemCharges(3)
                        case (SYS_DIMER_AC)
                              p0 = a0
                              p1 = a1
                              q0 = c0
                              q1 = c1
                              Charge = SubsystemCharges(1) + SubsystemCharges(3)
                        case (SYS_MONO_D)
                              p0 = d0
                              p1 = d1
                              Charge = SubsystemCharges(4)
                        case (SYS_DIMER_AD)
                              p0 = a0
                              p1 = a1
                              q0 = d0
                              q1 = d1
                              Charge = SubsystemCharges(1) + SubsystemCharges(4)
                        case (SYS_DIMER_BD)
                              p0 = b0
                              p1 = b1
                              q0 = d0
                              q1 = d1
                              Charge = SubsystemCharges(2) + SubsystemCharges(4)
                        case (SYS_DIMER_CD)
                              p0 = c0
                              p1 = d1
                              Charge = SubsystemCharges(3) + SubsystemCharges(4)
                        case (SYS_TRIMER_ABC)
                              p0 = a0
                              p1 = c1
                              Charge = SubsystemCharges(1) + SubsystemCharges(2) + SubsystemCharges(3)
                        case (SYS_TRIMER_ABD)
                              p0 = a0
                              p1 = b1
                              q0 = d0
                              q1 = d1
                              Charge = SubsystemCharges(1) + SubsystemCharges(2) + SubsystemCharges(4)
                        case (SYS_TRIMER_ACD)
                              p0 = a0
                              p1 = a1
                              q0 = c0
                              q1 = d1
                              Charge = SubsystemCharges(1) + SubsystemCharges(3) + SubsystemCharges(4)
                        case (SYS_TRIMER_BCD)
                              p0 = b0
                              p1 = d1
                              Charge = SubsystemCharges(2) + SubsystemCharges(3) + SubsystemCharges(4)
                        end select
                  end select
                  RealAtoms(1, 1) = p0
                  RealAtoms(2, 1) = p1
                  RealAtoms(1, 2) = q0
                  RealAtoms(2, 2) = q1
                  NElectrons = 0
                  do s = 1, 2
                        if (RealAtoms(2, s) >= RealAtoms(1, s)) then
                              p0 = RealAtoms(1, s)
                              p1 = RealAtoms(2, s)
                              if (ECPCharges) then
                                    NElectrons = NElectrons + sum(ZNumbersECP(p0:p1))
                              else
                                    NElectrons = NElectrons + sum(ZNumbers(p0:p1))
                              end if
                        end if
                  end do
                  NElectrons = NElectrons - Charge
                  Mult = SubsystemMult(i)
            end associate
      end subroutine sys_Init

      
      subroutine sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, AtomsType)
            integer, dimension(:), allocatable, intent(out) :: ZList
            integer, dimension(:), allocatable, intent(out) :: ZCount
            integer, dimension(:), intent(out)              :: AtomElementMap
            integer, intent(out)                            :: NElements
            type(TSystem), intent(in)                       :: System
            integer, intent(in)                             :: AtomsType

            integer, dimension(KNOWN_ELEMENTS) :: AllElementsCount
            integer, dimension(KNOWN_ELEMENTS) :: RealElementsCount
            integer, dimension(KNOWN_ELEMENTS) :: GhostElementsCount
            integer, dimension(KNOWN_ELEMENTS) :: ZElementMap
            integer :: z, k, l, s

            if (AtomsType == SYS_ALL_ATOMS .or. AtomsType == SYS_GHOST_ATOMS) then
                  AllElementsCount = 0
                  do k = 1, System%NAtoms
                        z = System%ZNumbers(k)
                        AllElementsCount(z) = AllElementsCount(z) + 1
                  end do
            end if
            
            if (AtomsType == SYS_REAL_ATOMS .or. AtomsType == SYS_GHOST_ATOMS) then
                  RealElementsCount = 0
                  do s = 1, 2
                        do k = System%RealAtoms(1, s), System%RealAtoms(2, s)
                              z = System%ZNumbers(k)
                              RealElementsCount(z) = RealElementsCount(z) + 1
                        end do
                  end do
            end if
            
            if (AtomsType == SYS_GHOST_ATOMS) then
                  GhostElementsCount = AllElementsCount - RealElementsCount
            end if
            
            if (AtomsType == SYS_REAL_ATOMS) then
                  AllElementsCount = RealElementsCount
            else if (AtomsType == SYS_GHOST_ATOMS) then
                  AllElementsCount = GhostElementsCount
            end if
            
            NElements = 0
            do z = 1, KNOWN_ELEMENTS
                  if (AllElementsCount(z) > 0) NElements = NElements + 1
            end do
            allocate(ZList(NElements))
            allocate(ZCount(NElements))
            if (NElements > 0) then
                  l = 1
                  do z = 1, KNOWN_ELEMENTS
                        if (AllElementsCount(z) > 0) then
                              ZList(l) = z
                              if (AtomsType == SYS_ALL_ATOMS) then
                                    ZElementMap(z) = l
                              end if
                              ZCount(l) = AllElementsCount(z)
                              l = l + 1
                        end if
                  end do
                  if (AtomsType == SYS_ALL_ATOMS) then
                        do k = 1, System%NAtoms
                              AtomElementMap(k) = ZElementMap(System%ZNumbers(k))
                        end do
                  else
                        AtomElementMap = 0
                  end if
            end if
      end subroutine sys_ElementsList


      function sys_ChemicalFormula(System)
            !
            ! Compute chemical formula of a given molecule.
            !
            character(:), allocatable         :: sys_ChemicalFormula
            type(TSystem), intent(in)         :: System

            integer, dimension(:), allocatable :: ZListReal, ZListGhost
            integer, dimension(:), allocatable :: ZCountReal, ZCountGhost
            integer :: NElementsReal, NElementsGhost
            integer, dimension(KNOWN_ELEMENTS) :: AtomElementMap
            
            character(:), allocatable :: s1, s2
            integer :: k

            s1 = ""
            s2 = ""
            if (System%SystemKind == SYS_MOLECULE .or. System%SubsystemKind == SYS_TOTAL) then
                  call sys_ElementsList(ZListReal, ZCountReal, AtomElementMap, NElementsReal, System, SYS_ALL_ATOMS)
                  do k = 1, NElementsReal
                        s1 = s1 // trim(ELNAME_SHORT(ZListReal(k)))
                        if (ZCountReal(k) > 1) then
                              s1 = s1 // str(ZCountReal(k))
                        end if
                  end do
            else
                  select case (System%SubsystemKind)
                  case (SYS_MONO_A)     ! --- 1 ---
                        s1 = "Monomer A: "
                  case (SYS_MONO_B)     ! --- 2 ---
                        s1 = "Monomer B: "
                  case (SYS_MONO_C)     ! --- 3 ---
                        s1 = "Monomer C: "
                  case (SYS_DIMER_AB)   ! --- 4 ---
                        s1 = "Dimer AB: "
                  case (SYS_DIMER_BC)   ! --- 5 ---
                        s1 = "Dimer BC: "
                  case (SYS_DIMER_AC)   ! --- 6 ---
                        s1 = "Dimer AC: "
                  case (SYS_MONO_D)     ! --- 7 ---
                        s1 = "Monomer D: "
                  case (SYS_DIMER_AD)   ! --- 8 ---
                        s1 = "Dimer AD: "
                  case (SYS_DIMER_BD)   ! --- 9 ---
                        s1 = "Dimer BD: "
                  case (SYS_DIMER_CD)   ! --- 10 ---
                        s1 = "Dimer CD: "
                  case (SYS_TRIMER_ABC) ! --- 11 ---
                        s1 = "Trimer ABC: "
                  case (SYS_TRIMER_ABD) ! --- 12 ---
                        s1 = "Trimer ABD: "
                  case (SYS_TRIMER_ACD) ! --- 13 ---
                        s1 = "Trimer ACD: "
                  case (SYS_TRIMER_BCD) ! --- 14 ---
                        s1 = "Trimer BCD: "
                  end select
                  call sys_ElementsList(ZListReal, ZCountReal, AtomElementMap, NElementsReal, System, SYS_REAL_ATOMS)
                  call sys_ElementsList(ZListGhost, ZCountGhost, AtomElementMap, NElementsGhost, System, SYS_GHOST_ATOMS)
                  do k = 1, NElementsReal
                        s1 = s1 // trim(ELNAME_SHORT(ZListReal(k)))
                        if (ZCountReal(k) > 1) then
                              s1 = s1 // str(ZCountReal(k))
                        end if
                  end do
                  do k = 1, NElementsGhost
                        s2 = s2 // trim(ELNAME_SHORT(ZListGhost(k)))
                        if (ZCountGhost(k) > 1) then
                              s2 = s2 // str(ZCountGhost(k))
                        end if
                  end do
            end if
            if (System%Charge > 0 .and. System%Mult > 1) then
                  s1 = s1 // " (charge=" // str(System%Charge) // ", 2S+1=" // str(System%Mult) // ")"
            else if (System%Charge > 0) then
                  s1 = s1 // " (charge=" // str(System%Charge) // ")"
            else if (System%Mult > 1) then
                  s1 = s1 // " (2S+1=" // str(System%Mult) // ")"
            end if
            if (len(s2) > 0) then
                  sys_ChemicalFormula = s1 // " + ghost centers " // s2
            else 
                  sys_ChemicalFormula = s1
            end if
      end function sys_ChemicalFormula


      function sys_IsDummyAtom(System, k)
            logical :: sys_IsDummyAtom
            type(TSystem), intent(in) :: System
            integer, intent(in)       :: k

            sys_IsDummyAtom = .true.
            if (k >= System%RealAtoms(1, 1) .and. k <= System%RealAtoms(2, 1)) sys_IsDummyAtom = .false.
            if (k >= System%RealAtoms(1, 2) .and. k <= System%RealAtoms(2, 2)) sys_IsDummyAtom = .false.
      end function sys_IsDummyAtom


      subroutine sys_SortDistances(System)
            type(TSystem), intent(inout) :: System

            real(F64), dimension(3) :: ri, rj, d
            real(F64) :: dr
            integer :: i, j
            
            if (allocated(System%SortedDistances)) deallocate(System%SortedDistances)
            if (allocated(System%SortedDistancesIdx)) deallocate(System%SortedDistancesIdx)
            allocate(System%SortedDistances(System%NAtoms, System%NAtoms))
            allocate(System%SortedDistancesIdx(System%NAtoms, System%NAtoms))

            associate ( &
                  NAtoms => System%NAtoms, &
                  AtomCoords => System%AtomCoords, &
                  SortedDistances => System%SortedDistances, &
                  SortedDistancesIdx => System%SortedDistancesIdx &
                  )
                  if (NAtoms > 1) then
                        do i = 1, NAtoms
                              ri = AtomCoords(:, i)
                              SortedDistances(i, i) = ZERO
                              do j = 1, i - 1
                                    rj = AtomCoords(:, j)
                                    d = ri - rj
                                    dr = norm2(d)
                                    SortedDistances(i, j) = dr
                                    SortedDistances(j, i) = dr
                              end do
                        end do
                        !
                        ! For every nucleus J sort all
                        ! remaining nuclei according
                        ! to SORTEDDISTANCES(I, J)
                        !
                        do j = 1, NAtoms
                              do i = 1, NAtoms
                                    SortedDistancesIdx(i, j) = i
                              end do
                              call dsort(SortedDistances(:, j), SortedDistancesIdx(:, j), NAtoms)
                        end do
                  else
                        SortedDistances(1, 1) = ZERO
                        SortedDistancesIdx(1, 1) = 1
                  end if
            end associate
      end subroutine sys_SortDistances


      subroutine sys_NuclearRepulsion(Enucl, System)
            real(F64), intent(out)    :: Enucl
            type(TSystem), intent(in) :: System

            integer :: i, j, s, t
            real(F64), dimension(3) :: Ra, Rb, Rab
            real(F64) :: Dab, Qa, Qb, EnuclAB, EnuclAPC

            associate ( &
                  AtomCoords => System%AtomCoords, &
                  RealAtoms => System%RealAtoms, &
                  ECPCharges => System%ECPCharges, &
                  ZNumbers => System%ZNumbers, &
                  ZNumbersECP => System%ZNumbersECP &                  
                  )
                  EnuclAB = ZERO
                  EnuclAPC = ZERO
                  do s = 1, 2
                        do i = RealAtoms(1, s), RealAtoms(2, s)
                              Ra = AtomCoords(:, i)
                              if (ECPCharges) then
                                    Qa = real(ZNumbersECP(i), F64)
                              else
                                    Qa = real(ZNumbers(i), F64)
                              end if
                              do t = 1, 2
                                    do j = max(i+1, RealAtoms(1, t)), RealAtoms(2, t)
                                          Rb = AtomCoords(:, j)
                                          if (ECPCharges) then
                                                Qb = real(ZNumbersECP(j), F64)
                                          else
                                                Qb = real(ZNumbers(j), F64)
                                          end if
                                          Rab = Ra - Rb
                                          Dab = norm2(Rab)
                                          EnuclAB = EnuclAB + Qa * Qb / Dab
                                    end do
                              end do
                              !
                              ! Atom - point charge interaction (QM-MM)
                              !
                              if (System%NPointCharges > 0) then
                                    associate ( &
                                          NPointCharges => System%NPointCharges, &
                                          PointCharges => System%PointCharges, &
                                          PointChargeCoords => System%PointChargeCoords &
                                          )
                                          do j = 1, NPointCharges
                                                Rb = PointChargeCoords(:, j)
                                                Qb = PointCharges(j)
                                                Rab = Ra - Rb
                                                Dab = norm2(Rab)
                                                EnuclAPC = EnuclAPC + Qa * Qb / Dab
                                          end do
                                    end associate
                              end if
                        end do
                  end do
                  Enucl = EnuclAB + EnuclAPC
            end associate
      end subroutine sys_NuclearRepulsion


      subroutine sys_NuclearMultipoles(Dx, Dy, Dz, Qyx, Qzx, &
            Qzy, Qxx, Qyy, Qzz, Rc, System)
            
            real(F64), intent(out)    :: Dx, Dy, Dz
            real(F64), intent(out)    :: Qyx, Qzx, Qzy, Qxx, Qyy, Qzz
            real(F64), dimension(3)   :: Rc
            type(TSystem), intent(in) :: System

            integer :: s, p0, p1, p
            integer :: Z
            real(F64), dimension(3) :: Rp, Rpc

            Dx = ZERO
            Dy = ZERO
            Dz = ZERO
            Qyx = ZERO
            Qzx = ZERO
            Qzy = ZERO
            Qxx = ZERO
            Qyy = ZERO
            Qzz = ZERO
            do s = 1, 2
                  if (System%RealAtoms(2, s) >= System%RealAtoms(1, s)) then
                        p0 = System%RealAtoms(1, s)
                        p1 = System%RealAtoms(2, s)
                        do p = p0, p1
                              if (System%ECPCharges) then
                                    Z = System%ZNumbersECP(p) 
                              else
                                    Z = System%ZNumbers(p)
                              end if
                              Rp(:) = System%AtomCoords(:, p)
                              Rpc(:) = Rp(:) - Rc(:)
                              Dx = Dx + Rpc(1) * Z
                              Dy = Dy + Rpc(2) * Z
                              Dz = Dz + Rpc(3) * Z
                              Qyx = Qyx + Rpc(1)*Rpc(2) * Z
                              Qzx = Qzx + Rpc(1)*Rpc(3) * Z
                              Qzy = Qzy + Rpc(2)*Rpc(3) * Z
                              Qxx = Qxx + Rpc(1)**2 * Z
                              Qyy = Qyy + Rpc(2)**2 * Z
                              Qzz = Qzz + Rpc(3)**2 * Z
                        end do
                  end if
            end do
      end subroutine sys_NuclearMultipoles


      subroutine sys_ChargeCenter(Rc, System)
            real(F64), dimension(3), intent(out) :: Rc
            type(TSystem), intent(in)            :: System

            integer :: SumZ, Z
            integer :: s, p, p0, p1

            Rc = ZERO
            SumZ = System%NElectrons + System%Charge
            do s = 1, 2
                  if (System%RealAtoms(2, s) >= System%RealAtoms(1, s)) then
                        p0 = System%RealAtoms(1, s)
                        p1 = System%RealAtoms(2, s)
                        do p = p0, p1
                              if (System%ECPCharges) then
                                    Z = System%ZNumbersECP(p) 
                              else
                                    Z = System%ZNumbers(p)
                              end if
                              Rc(:) = Rc(:) + real(Z, F64)/SumZ * System%AtomCoords(:, p)
                        end do
                  end if
            end do
      end subroutine sys_ChargeCenter

      
      subroutine sys_Read_XYZ(System, FilePath, Units)
            class(TSystem), intent(out)   :: System
            character(*), intent(in)      :: FilePath
            integer, optional, intent(in) :: Units

            logical :: XYZDefined, XYZCompleted, InsideXYZ
            integer :: AtomIdx
            integer :: u
            character(:), allocatable :: key, val
            character(:), allocatable :: line
            logical :: eof
            integer :: Units0

            if (present(Units)) then
                  Units0 = Units
            else
                  Units0 = SYS_UNITS_ANGSTROM
            end if
            
            u = io_text_open(FilePath, "OLD")
            XYZDefined = .false.
            XYZCompleted = .false.
            InsideXYZ = .false.
            AtomIdx = -1
            lines: do
                  call io_text_readline(line, u, eof)
                  if (eof) then
                        exit lines
                  end if                  
                  if (isblank(line) .or. iscomment(line)) then
                        cycle lines
                  end if
                  call split(line, key, val)
                  key = uppercase(key)
                  if (key == "XYZ") then
                        XYZDefined = .true.
                        InsideXYZ = .true.
                        cycle lines
                  else if (key == "END") then
                        if (InsideXYZ) then
                              XYZCompleted = .true.
                              InsideXYZ = .false.
                              exit lines
                        else
                              cycle lines
                        end if
                  else
                        if (InsideXYZ) then
                              call sys_Read_XYZ_NextLine(System, AtomIdx, line, Units0)
                        end if
                  end  if
            end do lines
            close(u)
            
            if (.not. XYZDefined) then
                  call msg("XYZ coordinates not defined in file " // FilePath, MSG_ERROR)
                  error stop
            end if

            if (XYZDefined .and. .not. XYZCompleted) then
                  call msg("Unexpected end of file while reading xyz coordinates. " &
                        // "Missing END keyword.", MSG_ERROR)
                  error stop
            end if
            
            call sys_Init(System, SYS_TOTAL)
            call sys_SortDistances(System)
      end subroutine sys_Read_XYZ


      subroutine sys_Read_XYZ_NextLine(System, AtomIdx, line, Units)
            type(TSystem), intent(inout) :: System
            integer, intent(inout)       :: AtomIdx
            character(*), intent(in)     :: line
            integer, intent(in)          :: Units

            character(:), allocatable :: key, val
            character(:), allocatable :: element, coords
            integer :: k, z
            integer :: NSubsystems

            call split(line, key, val)
            key = uppercase(key)
            if (System%SystemKind == SYS_NONE) then
                  NSubsystems = IntListLength(line)
                  select case (NSubsystems)
                  case (1)
                        System%SystemKind = SYS_MOLECULE
                  case (2)
                        System%SystemKind = SYS_DIMER
                  case (3)
                        System%SystemKind = SYS_TRIMER
                  case (4)
                        System%SystemKind = SYS_TETRAMER
                  case default
                        call msg("First line of the XYZ block has an invalid format", MSG_ERROR)
                        error stop
                  end select
                  AtomIdx = 0
                  read(line, *) (System%SubsystemAtoms(k), k=1,System%SystemKind)
                  System%NAtoms = sum(System%SubsystemAtoms)
                  System%RealAtoms(:, 1) = [1, System%NAtoms]
                  System%RealAtoms(:, 2) = [1, 0]
                  allocate(System%AtomCoords(3, System%NAtoms))
                  allocate(System%ZNumbers(System%NAtoms))
            else if (key == "CHARGE" .or. key == "CHARGES") then
                  read(val, *) (System%SubsystemCharges(k), k=1,System%SystemKind)
                  System%Charge = sum(System%SubsystemCharges)
            else if (key == "MULT" .or. key == "MULTIPLICITY") then
                  if (System%SystemKind==SYS_MOLECULE) then
                        read(val, *) System%SubsystemMult(1)
                  else if (System%SystemKind==SYS_DIMER) then
                        read(val, *) (System%SubsystemMult(k), k=1,3)
                  else if (System%SystemKind==SYS_TRIMER) then
                        read(val, *) (System%SubsystemMult(k), k=1,7)
                  else ! Multiplicities of subsystems in a tetramer
                        read(val, *) (System%SubsystemMult(k), k=1,15)
                  end if
                  System%Mult = System%SubsystemMult(1)
            else
                  if (AtomIdx > -1) then
                        AtomIdx = AtomIdx + 1
                        if (AtomIdx <= System%NAtoms) then
                              element = key
                              coords = val
                              z = znumber_short(element)
                              if (z > 0) then
                                    System%ZNumbers(AtomIdx) = z
                                    read(coords, *) (System%AtomCoords(k, AtomIdx), k=1,3)
                                    if (Units == SYS_UNITS_ANGSTROM) then
                                          System%AtomCoords(:, AtomIdx) = tobohr(System%AtomCoords(:, AtomIdx))
                                    end if
                              else
                                    call msg("Unknown element: " // element, MSG_ERROR)
                                    error stop
                              end if
                        else
                              call msg("Inconsistent number of atoms specified", MSG_ERROR)
                              error stop
                        end if
                  else
                        call msg("Unspecified number of atoms", MSG_ERROR)
                        error stop
                  end if
            end if
      end subroutine sys_Read_XYZ_NextLine


      subroutine sys_Read_Embedding(System, FilePath, Units, LibraryDir)
            class(TSystem), intent(inout)      :: System
            character(*), intent(in)           :: FilePath
            integer, optional, intent(in)      :: Units
            character(*), optional, intent(in) :: LibraryDir

            logical :: EmbeddingDefined, InsideEmbedding, EmbeddingCompleted, HeaderRead
            integer :: ChargeIdx, ECPIdx
            integer :: u
            character(:), allocatable :: key, val
            character(:), allocatable :: line
            logical :: eof
            integer :: Units_

            if (present(Units)) then
                  Units_ = Units
            else
                  Units_ = SYS_UNITS_ANGSTROM
            end if
            
            u = io_text_open(FilePath, "OLD")
            EmbeddingDefined = .false.
            InsideEmbedding = .false.
            EmbeddingCompleted = .false.
            HeaderRead = .false.
            ChargeIdx = 0
            ECPIdx = 0
            
            System%NPointCharges = 0
            if (allocated(System%PointCharges)) deallocate(System%PointCharges)
            allocate(System%PointCharges(0))
            if (allocated(System%PointChargeCoords)) deallocate(System%PointChargeCoords)
            allocate(System%PointChargeCoords(3, 0))
            call System%EmbeddingECP%free()
            allocate(System%EmbeddingECP%Coords(3, 0))
            allocate(System%EmbeddingECP%Z(0))
            if (present(LibraryDir)) then
                  call System%EmbeddingECP%Assignment%set_library_dir(LibraryDir)
            end if

            lines: do
                  call io_text_readline(line, u, eof)
                  if (eof) exit lines
                  
                  if (isblank(line) .or. iscomment(line)) cycle lines
                  
                  call split(line, key, val)
                  key = uppercase(key)
                  
                  if (key == "EMBEDDING") then
                        EmbeddingDefined = .true.
                        InsideEmbedding = .true.
                        cycle lines
                  else if (key == "END") then
                        if (InsideEmbedding) then
                              InsideEmbedding = .false.
                              EmbeddingCompleted = .true.
                              exit lines
                        else
                              cycle lines
                        end if
                  else
                        if (InsideEmbedding) then
                              if (.not. HeaderRead) then
                                    call sys_Read_Embedding_Header(System%NPointCharges, &
                                          System%EmbeddingECP%NEmbCenters, line)

                                    if (allocated(System%PointCharges)) deallocate(System%PointCharges)
                                    allocate(System%PointCharges(System%NPointCharges))
                                    if (allocated(System%PointChargeCoords)) deallocate(System%PointChargeCoords)
                                    allocate(System%PointChargeCoords(3, System%NPointCharges))

                                    if (allocated(System%EmbeddingECP%Coords)) deallocate(System%EmbeddingECP%Coords)
                                    if (allocated(System%EmbeddingECP%Z)) deallocate(System%EmbeddingECP%Z)
                                    if (System%EmbeddingECP%NEmbCenters > 0) then
                                          allocate(System%EmbeddingECP%Coords(3, System%EmbeddingECP%NEmbCenters))
                                          allocate(System%EmbeddingECP%Z(System%EmbeddingECP%NEmbCenters))
                                          System%EmbeddingECP%Initialized = .true.
                                    else
                                          allocate(System%EmbeddingECP%Coords(3, 0))
                                          allocate(System%EmbeddingECP%Z(0))
                                    end if

                                    HeaderRead = .true.
                              else
                                    call sys_Read_Embedding_NextLine(System, ChargeIdx, ECPIdx, line, Units_)
                              end if
                        end if
                  end if
            end do lines
            close(u)
            
            if (EmbeddingDefined) then
                  if (.not. EmbeddingCompleted) then
                        call msg("Unexpected end of file while reading EMBEDDING block. " &
                              // "Missing END keyword.", MSG_ERROR)
                        error stop
                  end if
                  
                  if (ChargeIdx /= System%NPointCharges) then
                        call msg("Number of point charges read does not match the " &
                              // "specified number.", MSG_ERROR)
                        error stop
                  end if

                  if (ECPIdx /= System%EmbeddingECP%NEmbCenters) then
                        call msg("Number of ECP centers read does not match the " &
                              // "specified number in ecp_centers keyword.", MSG_ERROR)
                        error stop
                  end if
            end if
      end subroutine sys_Read_Embedding


      subroutine sys_Read_Embedding_Header(NPointCharges, NECPCenters, line)
            !
            ! Read the number of point charges and ECP centers from the embedding header.
            !
            integer, intent(out)     :: NPointCharges
            integer, intent(out)     :: NECPCenters
            character(*), intent(in) :: line

            integer :: i1, i2
            character(:), allocatable :: val, line_upper

            line_upper = uppercase(line)
            NPointCharges = 0
            NECPCenters = 0

            call sys_ExtractKeyVal_(val, i1, i2, line, line_upper, "POINT_CHARGES")
            if (i1 > 0) then
                  read(val, *) NPointCharges
            end if

            call sys_ExtractKeyVal_(val, i1, i2, line, line_upper, "ECP_CENTERS")
            if (i1 > 0) then
                  read(val, *) NECPCenters
            end if

            if (NPointCharges <= 0) then
                  call msg("Missing or invalid point_charges(N) in EMBEDDING header: " &
                        // trim(line), MSG_ERROR)
                  error stop
            end if

            if (NECPCenters < 0) then
                  call msg("Invalid number of ECP centers in EMBEDDING block. " &
                        // "Must be non-negative.", MSG_ERROR)
                  error stop
            end if

            if (NECPCenters > NPointCharges) then
                  call msg("Number of ECP centers cannot exceed number of point charges.", MSG_ERROR)
                  error stop
            end if
      end subroutine sys_Read_Embedding_Header


      subroutine sys_ExtractKeyVal_(val, i1, i2, s, s_upper, key)
            !
            ! Extract value and boundaries from KEY(VAL) within string S.
            !
            character(:), allocatable, intent(out) :: val
            integer, intent(out)                   :: i1
            integer, intent(out)                   :: i2
            character(*), intent(in)               :: s
            character(*), intent(in)               :: s_upper
            character(*), intent(in)               :: key

            integer :: k2
            character(:), allocatable :: key_tag

            val = ""
            i1 = 0
            i2 = 0

            key_tag = key // "("

            i1 = index(s_upper, key_tag)
            if (i1 == 0) return

            k2 = index(s(i1:), ")")
            if (k2 > 0) then
                  i2 = i1 + k2 - 1
            else
                  call msg("Missing closing ')' for " // key &
                        // " in EMBEDDING line: " // trim(s), MSG_ERROR)
                  error stop
            end if

            val = trim(adjustl(s(i1 + len(key_tag) : i2 - 1)))
            if (len_trim(val) == 0) then
                  call msg("Empty " // key // " specification in EMBEDDING line: " &
                        // trim(s), MSG_ERROR)
                  error stop
            end if
      end subroutine sys_ExtractKeyVal_


      subroutine sys_SplitEmbeddingLine(QPart, ECPPart, HasECP, CoordsPart, line)
            !
            ! Split an embedding line into charge, optional pseudopotential, and coordinates parts.
            !
            character(:), allocatable, intent(out) :: QPart
            character(:), allocatable, intent(out) :: ECPPart
            logical, intent(out)                   :: HasECP
            character(:), allocatable, intent(out) :: CoordsPart
            character(*), intent(in)               :: line

            integer :: iq1, iq2, iecp1, iecp2, last_close
            character(:), allocatable :: line_upper

            line_upper = uppercase(line)

            call sys_ExtractKeyVal_(QPart, iq1, iq2, line, line_upper, "Q")
            if (iq1 == 0) then
                  call msg("Missing Q(charge) in EMBEDDING line: " // trim(line), MSG_ERROR)
                  error stop
            end if

            call sys_ExtractKeyVal_(ECPPart, iecp1, iecp2, line, line_upper, "ECP")
            HasECP = (iecp1 > 0)

            if (HasECP) then
                  last_close = max(iq2, iecp2)
            else
                  last_close = iq2
            end if

            CoordsPart = adjustl(line(last_close+1:))
            if (len_trim(CoordsPart) == 0) then
                  call msg("Missing coordinates in EMBEDDING line: " // trim(line), MSG_ERROR)
                  error stop
            end if
      end subroutine sys_SplitEmbeddingLine


      subroutine sys_Read_Embedding_NextLine(System, ChargeIdx, ECPIdx, line, Units)
            !
            ! Read a single point-charge line and store coordinates and optional ECP.
            !
            type(TSystem), intent(inout) :: System
            integer, intent(inout)       :: ChargeIdx
            integer, intent(inout)       :: ECPIdx
            character(*), intent(in)     :: line
            integer, intent(in)          :: Units
            
            integer :: k, Z
            character(:), allocatable :: qstr, ecpstr, coords
            character(:), allocatable :: elem_key, params_name
            logical :: has_ecp

            if (ChargeIdx >= System%NPointCharges) then
                  call msg("Inconsistent number of point charges specified " &
                        // "in EMBEDDING block", MSG_ERROR)
                  error stop
            end if

            ChargeIdx = ChargeIdx + 1
            call sys_SplitEmbeddingLine(qstr, ecpstr, has_ecp, coords, line)
            read(qstr, *) System%PointCharges(ChargeIdx)
            read(coords, *) (System%PointChargeCoords(k, ChargeIdx), k=1,3)
            
            if (Units == SYS_UNITS_ANGSTROM) then
                  System%PointChargeCoords(:, ChargeIdx) = tobohr(System%PointChargeCoords(:, ChargeIdx))
            end if
            
            if (has_ecp) then
                  ECPIdx = ECPIdx + 1
                  if (ECPIdx > System%EmbeddingECP%NEmbCenters) then
                        call msg("More ECP centers found than specified in ecp_centers keyword: " &
                              // trim(line), MSG_ERROR)
                        error stop
                  end if

                  call split(ecpstr, elem_key, params_name)
                  if (len_trim(params_name) == 0) then
                        call msg("Missing pseudopotential name in ECP(...) specification: " &
                              // trim(line), MSG_ERROR)
                        error stop
                  end if

                  Z = znumber_short(elem_key)
                  if (Z <= 0) then
                        call msg("Unknown element symbol '" // elem_key // "' in ECP(...): " &
                              // trim(line), MSG_ERROR)
                        error stop
                  end if

                  System%EmbeddingECP%Coords(:, ECPIdx) = System%PointChargeCoords(:, ChargeIdx)
                  System%EmbeddingECP%Z(ECPIdx) = Z

                  call System%EmbeddingECP%Assignment%read_line(ecpstr)
            end if
      end subroutine sys_Read_Embedding_NextLine


      subroutine sys_ReadECP(this, FilePath, DefaultAssign)
            !
            ! Read pseudopotential assignments from an input file.
            !
            class(TSystem), intent(inout)                 :: this
            character(*), intent(in)                      :: FilePath
            class(TBasisAssignment), optional, intent(in) :: DefaultAssign

            logical :: ECPDefined, InsideECP, ECPCompleted
            integer :: u
            character(:), allocatable :: key, val
            character(:), allocatable :: line
            logical :: eof

            if (present(DefaultAssign)) then
                  if (DefaultAssign%Initialized) then
                        !
                        ! By default, we are looking for the basis set parameters
                        ! inside the files where the primary basis set is defined.
                        ! This is the baseline that we subsequently modify by
                        ! ECP-specific assignment of params.
                        !
                        this%ECP%Assignment = DefaultAssign
                  end if
            end if

            u = io_text_open(FilePath, "OLD")
            ECPDefined = .false.
            InsideECP = .false.
            ECPCompleted = .false.

            lines: do
                  call io_text_readline(line, u, eof)
                  if (eof) exit lines

                  if (isblank(line) .or. iscomment(line)) cycle lines

                  call split(line, key, val)
                  key = uppercase(key)

                  if (key == "ECP_ASSIGNMENT") then
                        ECPDefined = .true.
                        InsideECP = .true.
                        cycle lines
                  else if (key == "END") then
                        if (InsideECP) then
                              InsideECP = .false.
                              ECPCompleted = .true.
                              exit lines
                        else
                              cycle lines
                        end if
                  else
                        if (InsideECP) then
                              call this%ECP%Assignment%read_line(line)
                        end if
                  end if
            end do lines
            close(u)

            if (ECPDefined .and. .not. ECPCompleted) then
                  call msg("Unexpected end of file while reading ECP_ASSIGNMENT block. " &
                        // "Missing END keyword.", MSG_ERROR)
                  error stop
            end if
            !
            ! Attempt to find ECP data in all cases, even
            ! when no custom ECP_ASSIGNMENT block if found.
            ! In that case, we look for the ECP data in the
            ! AO basis set files. 
            !
            call sys_SetEffectiveCores_(this)
      end subroutine sys_ReadECP
end module sys_definitions
