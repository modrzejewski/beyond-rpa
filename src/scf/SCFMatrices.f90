!
! A set of subroutines for printing a full set of matrices required to
! perform an HF/post-HF calculation.
!
! The purpose of this module is to generate supporting materials for
! teaching.
!
module SCFMatrices
      use arithmetic
      use string
      use display
      use gparam
      use io
      use linalg
      use basis_sets
      use Auto2e
      use sys_definitions
      use real_linalg
      use OneElectronInts
      
      implicit none

contains

      function SCFMatrices_SubsystemLabel(System)
            character(:), allocatable :: SCFMatrices_SubsystemLabel
            type(TSystem), intent(in) :: System

            character(:), allocatable :: s1
            
            select case (System%SubsystemKind)
            case (SYS_MONO_A)     ! --- 1 ---
                  s1 = "Monomer_A"
            case (SYS_MONO_B)     ! --- 2 ---
                  s1 = "Monomer_B"
            case (SYS_MONO_C)     ! --- 3 ---
                  s1 = "Monomer_C"
            case (SYS_DIMER_AB)   ! --- 4 ---
                  s1 = "Dimer_AB"
            case (SYS_DIMER_BC)   ! --- 5 ---
                  s1 = "Dimer_BC"
            case (SYS_DIMER_AC)   ! --- 6 ---
                  s1 = "Dimer_AC"
            case (SYS_MONO_D)     ! --- 7 ---
                  s1 = "Monomer_D"
            case (SYS_DIMER_AD)   ! --- 8 ---
                  s1 = "Dimer_AD"
            case (SYS_DIMER_BD)   ! --- 9 ---
                  s1 = "Dimer_BD"
            case (SYS_DIMER_CD)   ! --- 10 ---
                  s1 = "Dimer_CD"
            case (SYS_TRIMER_ABC) ! --- 11 ---
                  s1 = "Trimer_ABC"
            case (SYS_TRIMER_ABD) ! --- 12 ---
                  s1 = "Trimer_ABD"
            case (SYS_TRIMER_ACD) ! --- 13 ---
                  s1 = "Trimer_ACD"
            case (SYS_TRIMER_BCD) ! --- 14 ---
                  s1 = "Trimer_BCD"
            case default
                  s1 = ""
            end select
            SCFMatrices_SubsystemLabel = s1
      end function SCFMatrices_SubsystemLabel

      
      subroutine SCFMatrices_OneElectron(SCFOutput, System, AOBasis)
            type(TSCFOutput), intent(in)           :: SCFOutput
            type(TSystem), intent(in)              :: System
            type(TAOBasis), intent(in)             :: AOBasis

            real(F64), dimension(:, :), allocatable :: W
            real(F64), dimension(:, :), allocatable :: OccCoeffs, VirtCoeffs, OccCoeffs_oao, VirtCoeffs_oao
            real(F64), dimension(:), allocatable :: OccEnergies, VirtEnergies
            real(F64) :: Enucl
            integer :: NAO, NOcc, NVirt, NMO
            integer :: i, a
            integer :: u
            character(:), allocatable :: SystemLabel
            real(F64), dimension(:, :), allocatable :: S_cao, Ts_cao, Vne_cao, Hbare_cao

            NAO = AOBasis%NAOCart
            NMO = SCFOutput%NOAO
            NOcc = SCFOutput%NOcc(1)
            NVirt = SCFOutput%NVirt(1)
            if (System%SubsystemKind == SYS_TOTAL) then
                  SystemLabel = ""
            else
                  SystemLabel = "_" // SCFMatrices_SubsystemLabel(System)
            end if
            allocate(S_cao(NAO, NAO))
            allocate(Ts_cao(NAO, NAO))
            allocate(Vne_cao(NAO, NAO))
            allocate(Hbare_cao(NAO, NAO))
            !
            ! One-electron integrals: overlap, kinetic,
            ! and nuclei-electron attraction
            !
            call ints1e_OverlapMatrix(S_cao, AOBasis)
            call ints1e_Kinetic(Ts_cao, AOBasis)
            call ints1e_Coulomb(Vne_cao, AOBasis, System)
            Hbare_cao = Ts_cao + Vne_cao
            call smfill(Hbare_cao)
            call smfill(Ts_cao)
            call smfill(S_cao)
            
            call io_text_write(Hbare_cao, IO_SCRATCHDIR // "SCF_HBare" // SystemLabel // ".txt")
            call io_text_write(S_cao, IO_SCRATCHDIR // "SCF_Overlap" // SystemLabel // ".txt")

            allocate(W(NAO, NAO))
            W(:, :) = SCFOutput%Rho_cao(:, :, 1)
            call smfill(W)
            call io_text_write(W, IO_SCRATCHDIR // "SCF_Rho" // SystemLabel // ".txt")
            
            allocate(OccCoeffs_oao(NMO, NOcc))
            allocate(VirtCoeffs_oao(NMO, NVirt))
            OccCoeffs_oao = SCFOutput%C_oao(:, 1:NOcc, 1)
            VirtCoeffs_oao = SCFOutput%C_oao(:, NOcc+1:NOcc+NVirt, 1)
            allocate(OccCoeffs(NAO, NOcc))
            allocate(VirtCoeffs(NAO, NVirt))
            call real_ab(OccCoeffs, SCFOutput%MOBasisVecsCart, OccCoeffs_oao)
            call real_ab(VirtCoeffs, SCFOutput%MOBasisVecsCart, VirtCoeffs_oao)
            !
            ! Coefficients of occupied orbitals
            !
            call io_text_write(OccCoeffs, IO_SCRATCHDIR // "SCF_OccCoeffs" // SystemLabel // ".txt")
            !
            ! Coefficients of virtual orbitals
            !
            call io_text_write(VirtCoeffs, IO_SCRATCHDIR // "SCF_VirtCoeffs" // SystemLabel // ".txt")

            allocate(OccEnergies(NOcc))
            allocate(VirtEnergies(NVirt))
            OccEnergies = SCFOutput%OrbEnergies(1:NOcc, 1)
            VirtEnergies = SCFOutput%OrbEnergies(NOcc+1:NOcc+NVirt, 1)
            !
            ! Energies of occupied orbitals
            !
            u = io_text_open(IO_SCRATCHDIR // "SCF_OccEnergies" // SystemLabel // ".txt", "REPLACE")
            do i = 1, NOcc
                  write(u, "(A)") str(OccEnergies(i))
            end do
            close(u)
            !
            ! Energies of virtual orbitals
            !
            u = io_text_open(IO_SCRATCHDIR // "SCF_VirtEnergies" // SystemLabel // ".txt", "REPLACE")
            do a = 1, NVirt
                  write(u, "(A)") str(VirtEnergies(a))
            end do
            close(u)
            !
            ! Number of AO/Occupied/Virtual orbitals
            !
            u = io_text_open(IO_SCRATCHDIR // "SCF_NOrbitals" // SystemLabel // ".txt", "REPLACE")
            write(u, "(A)") "Number of atomic orbitals: " // str(NAO)
            write(u, "(A)") "Number of occupied orbitals: " // str(NOcc)
            write(u, "(A)") "Number of virtual orbitals: " // str(NVirt)
            close(u)
            !
            ! Nuclear repulsion energy
            !
            Enucl = SCFOutput%Enucl
            u = io_text_open(IO_SCRATCHDIR // "NuclearRepulsion" // SystemLabel // ".txt", "REPLACE")
            write(u, "(A)") "Nuclear repulsion energy: " // str(Enucl)
            close(u)
      end subroutine SCFMatrices_OneElectron


      subroutine SCFMatrices_BasisSet(AOBasis)
            type(TAOBasis), intent(in) :: AOBasis

            integer :: ShA
            integer :: ShellParamsA
            integer :: a0
            integer :: a1
            integer :: La, Na
            integer :: u
            integer :: p
            integer :: x, y, z

            associate (LmaxGTO => AOBasis%LmaxGTO, &
                  ShellCenters => AOBasis%ShellCenters, &
                  AtomCoords => AOBasis%AtomCoords, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NShells => AOBasis%NShells, &
                  ShellLoc => AOBasis%ShellLocCart, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  NShellParams => AOBasis%NShellParams, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ &                  
                  )

                  call io_text_write(CntrCoeffs, IO_SCRATCHDIR // "CntrCoeffs" // ".txt")
                  call io_text_write(Exponents, IO_SCRATCHDIR // "Exponents" // ".txt")
                  call io_text_write(AtomCoords, IO_SCRATCHDIR // "AtomCoords" // ".txt")
                  u = io_text_open(IO_SCRATCHDIR // "AOParams" // ".txt", "REPLACE")
                  
                  do ShA = 1, NShells
                        ShellParamsA = ShellParamsIdx(ShA)
                        La = ShellMomentum(ShellParamsA)
                        Na = NAngFunc(ShellParamsA)
                        a0 = ShellLoc(ShA)
                        a1 = a0 + Na - 1
                        do p = a0, a1
                              x = CartPolyX(p-a0+1, La)
                              y = CartPolyY(p-a0+1, La)
                              z = CartPolyZ(p-a0+1, La)
                              write(u, "(A)") str(ShellParamsA) // " " // str(NPrimitives(ShellParamsA)) // " " &
                                    // str(x) // " " // str(y) // " " // str(z) &
                                    // " " //str(ShellCenters(ShA)) // " " // str(NormFactors(p-a0+1, ShellParamsA))
                        end do
                  end do

                  close(u)
            end associate
      end subroutine SCFMatrices_BasisSet


      subroutine SCFMatrices_TwoElectronIntegrals(AOBasis)
            type(TAOBasis), intent(in) :: AOBasis

            real(F64), dimension(:), allocatable :: Vabcd

            integer :: ShA, ShB, ShC, ShD
            integer :: NAO
            integer :: Na, Nb, Nc, Nd
            integer :: ShellParamsA, ShellParamsB, ShellParamsC, ShellParamsD
            integer :: i, a, b, c, d
            integer :: a0, b0, c0, d0
            integer :: a1, b1, c1, d1
            integer :: MaxNAngFunc
            real(F64), dimension(:, :, :, :), allocatable :: Vpqrs

            associate (LmaxGTO => AOBasis%LmaxGTO, &
                  ShellCenters => AOBasis%ShellCenters, &
                  AtomCoords => AOBasis%AtomCoords, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NShells => AOBasis%NShells, &
                  ShellLoc => AOBasis%ShellLocCart, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  NormFactors => AOBasis%NormFactorsCart &
                  )
                  !
                  ! Maximum number of Cartiesian angular functions
                  !
                  MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
                  NAO = AOBasis%NAOCart
                  allocate(Vabcd(MaxNAngFunc**4))
                  allocate(Vpqrs(NAO, NAO, NAO, NAO))
                  do ShD = 1, NShells
                        do ShC = 1, NShells
                              do ShB = 1, NShells
                                    do ShA = 1, NShells
                                          call compute_Vabcd_Auto2e(Vabcd, ShA, ShB, ShC, ShD, ShellCenters, AtomCoords, &
                                                ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, Exponents, &
                                                NormFactors)
                                          
                                          ShellParamsB = ShellParamsIdx(ShB)
                                          ShellParamsA = ShellParamsIdx(ShA)
                                          ShellParamsC = ShellParamsIdx(ShC)
                                          ShellParamsD = ShellParamsIdx(ShD)
                                          
                                          Na = NAngFunc(ShellParamsA)
                                          Nb = NAngFunc(ShellParamsB)
                                          Nc = NAngFunc(ShellParamsC)
                                          Nd = NAngFunc(ShellParamsD)

                                          a0 = ShellLoc(ShA)
                                          b0 = ShellLoc(ShB)
                                          c0 = ShellLoc(ShC)
                                          d0 = ShellLoc(ShD)

                                          a1 = a0 + Na - 1
                                          b1 = b0 + Nb - 1
                                          c1 = c0 + Nc - 1
                                          d1 = d0 + Nd - 1

                                          i = 1
                                          do d = d0, d1
                                                do c = c0, c1
                                                      do b = b0, b1
                                                            do a = a0, a1
                                                                  Vpqrs(a, b, c, d) = Vabcd(i)
                                                                  i = i + 1
                                                            end do
                                                      end do
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
                  !
                  ! Save two-electron integrals in a binary file
                  !
                  call io_binary_write_rankn(Vpqrs, NAO**4, IO_SCRATCHDIR // "TwoElectronIntegrals.bin")
                  
                  ! !
                  ! ! Print out two electron integrals to a text file
                  ! !
                  ! u = io_text_open(IO_SCRATCHDIR // "TwoElectronIntegrals.txt", "REPLACE")
                  ! do d = 1, NAO
                  !       do c = 1, NAO
                  !             do b = 1, NAO
                  !                   do a = 1, NAO
                  !                         write(u, "(A)") str(Vpqrs(a, b, c, d))
                  !                   end do
                  !             end do
                  !       end do
                  ! end do
                  ! close(u)
            end associate
      end subroutine SCFMatrices_TwoElectronIntegrals

      
      subroutine compute_Vabcd_Auto2e(Vabcd, ShA, ShB, ShC, ShD, ShellCenters, AtomCoords, &
            ShellParamsIdx, ShellMomentum, NPrimitives, CntrCoeffs, Exponents, &
            NormFactors)

            real(F64), dimension(:), intent(out)     :: Vabcd
            integer, intent(in)                      :: ShA
            integer, intent(in)                      :: ShB
            integer, intent(in)                      :: ShC
            integer, intent(in)                      :: ShD
            integer, dimension(:), intent(in)        :: ShellCenters
            real(F64), dimension(:, :), intent(in)   :: AtomCoords
            integer, dimension(:), intent(in)        :: ShellParamsIdx
            integer, dimension(:), intent(in)        :: ShellMomentum
            integer, dimension(:), intent(in)        :: NPrimitives
            real(F64), dimension(:, :), intent(in)   :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)   :: Exponents
            real(F64), dimension(:, :), intent(in)   :: NormFactors
            
            integer :: AtomA, AtomB, AtomC, AtomD
            integer :: ShellParamsA, ShellParamsB, ShellParamsC, ShellParamsD
            integer :: La, Lb, Lc, Ld

            real(F64), parameter :: Kappa = ZERO
            integer, parameter :: PtrOffset = 0

            AtomA = ShellCenters(ShA)
            AtomB = ShellCenters(ShB)
            AtomC = ShellCenters(ShC)
            AtomD = ShellCenters(ShD)

            ShellParamsB = ShellParamsIdx(ShB)
            ShellParamsA = ShellParamsIdx(ShA)
            ShellParamsC = ShellParamsIdx(ShC)
            ShellParamsD = ShellParamsIdx(ShD)

            La = ShellMomentum(ShellParamsA)
            Lb = ShellMomentum(ShellParamsB)
            Lc = ShellMomentum(ShellParamsC)
            Ld = ShellMomentum(ShellParamsD)
            
            call Auto2eERI(PtrOffset+auto2e_idx(Ld, Lc, Lb, La))%ptr( &
                  Vabcd, &
                  !
                  ! ShellD
                  !
                  AtomCoords(:, AtomD), CntrCoeffs(:, ShellParamsD), &
                  NormFactors(:, ShellParamsD), Exponents(:, ShellParamsD), &
                  NPrimitives(ShellParamsD), &
                  !
                  ! ShellC
                  !
                  AtomCoords(:, AtomC), CntrCoeffs(:, ShellParamsC), &
                  NormFactors(:, ShellParamsC), Exponents(:, ShellParamsC), &
                  NPrimitives(ShellParamsC), &
                  !
                  ! ShellB
                  !
                  AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                  NormFactors(:, ShellParamsB), Exponents(:, ShellParamsB), &
                  NPrimitives(ShellParamsB), &
                  !
                  ! ShellA
                  !
                  AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                  NormFactors(:, ShellParamsA), Exponents(:, ShellParamsA), &
                  NPrimitives(ShellParamsA), &
                  Kappa)
      end subroutine Compute_Vabcd_Auto2e
end module SCFMatrices
