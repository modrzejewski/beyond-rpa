module hirshfeld
      use math_constants
      use arithmetic
      use spherh
      use gparam
      use display
      use gto
      use ecpint
      use periodic
      use AtomicDensities
      use sys_definitions

      implicit none

contains

      subroutine hirshfeld_population_display(auxint, System)
            real(F64), dimension(:), intent(in) :: auxint
            type(TSystem), intent(in)           :: System
            
            real(F64) :: charge, n_electrons
            real(F64) :: x, y, z
            integer :: znum, k, i, s
            character(78) :: line
            real(F64) :: charge_sum

            call blankline()
            call msg("Hirshfeld atomic charges", underline=.true.)
            call blankline()
            write(line, "(3X,A18,1X,A18,1X,A18,1X,A18)") "X (angs)", "Y (angs)", "Z (angs)", "Charge"
            call msg(line)
            associate( &
                  RealAtoms => System%RealAtoms, &
                  AtomCoords => System%AtomCoords, &
                  ZNumbers => System%ZNumbers &
                  )
                  charge_sum = ZERO
                  k = 0
                  do s = 1, 2
                        do i = RealAtoms(1, s), RealAtoms(2, s)
                              k = k + 1
                              x = AtomCoords(1, i)
                              y = AtomCoords(2, i)
                              z = AtomCoords(3, i)
                              n_electrons = auxint(k)
                              charge = ECP_INUCLZ(i) - n_electrons
                              charge_sum = charge_sum + charge
                              znum = ZNumbers(i)
                              write(line, "(A2,1X,F18.8,1X,F18.8,1X,F18.8,1X,F18.6)") & 
                                    ELNAME_SHORT(znum), toang(x), toang(y), toang(z), charge
                              call msg(line)
                        end do
                  end do
            end associate
            write(line, "(49X,A10,1X,F18.6)") "Charge sum", charge_sum
            call msg(line)
            call blankline()
      end subroutine hirshfeld_population_display


      subroutine hirshfeld_volume_free_display(auxint)
            real(F64), dimension(:), intent(in) :: auxint

            call blankline()
            call msg("Atomic volume of a free atom", underline=.true.)
            call msg("Integration performed using spherically-averaged density")
            call msg("Int r**3*RhoSpher(r) dxdydz = " // str(auxint(1),d=6) // " a.u.")
            call msg("Int r**3*RhoSpher(r) dxdydz = " // str(auxint(1)*toang(ONE)**3,d=6) // " ang**3")
            call blankline()
      end subroutine hirshfeld_volume_free_display
      

      subroutine hirshfeld_volume_ratios(vol, vol_free, System)
            real(F64), dimension(:), intent(inout) :: vol
            real(F64), dimension(:), intent(in)    :: vol_free
            type(TSystem), intent(in)              :: System

            integer :: k, i, j, s
            integer, dimension(:), allocatable :: ZList, ZCount, AtomElementMap
            integer :: NElements
            !
            ! Get the list of all elements in the system
            !
            allocate(AtomElementMap(System%NAtoms))
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_ALL_ATOMS)
            !
            ! Loop over real atoms in the system
            !
            k = 0
            do s = 1, 2
                  do i = System%RealAtoms(1, s), System%RealAtoms(2, s)
                        j = AtomElementMap(i)
                        k = k + 1
                        vol(k) = vol(k) / vol_free(j)
                  end do
            end do
      end subroutine hirshfeld_volume_ratios


      subroutine hirshfeld_volume_display(vol_ratio, System)
            real(F64), dimension(:), intent(in) :: vol_ratio
            type(TSystem), intent(in)           :: System
            
            real(F64) :: x, y, z
            integer :: znum
            character(78) :: line
            integer :: k, i, s

            call blankline()
            call msg("Hirshfeld atomic volume ratios", underline=.true.)
            call blankline()
            write(line, "(3X,A18,1X,A18,1X,A18,1X,A18)") "X (angs)", "Y (angs)", "Z (angs)", "Veff/Vfree"
            call msg(line)
            k = 0
            associate( &
                  RealAtoms => System%RealAtoms, &
                  AtomCoords => System%AtomCoords, &
                  ZNumbers => System%ZNumbers)
                  do s = 1, 2
                        do i = RealAtoms(1, s), RealAtoms(2, s)
                              k = k + 1
                              x = AtomCoords(1, i)
                              y = AtomCoords(2, i)
                              z = AtomCoords(3, i)
                              znum = ZNumbers(i)
                              write(line, "(A2,1X,F18.8,1X,F18.8,1X,F18.8,1X,F18.6)") & 
                                    ELNAME_SHORT(znum), toang(x), toang(y), toang(z), vol_ratio(k)
                              call msg(line)
                        end do
                  end do
            end associate
            call blankline()
      end subroutine hirshfeld_volume_display
      

      pure subroutine hirshfeld_population(a, grid_weight, x, y, z, rho_tot, rho_atomic, &
            ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, AtomShellMap, &
            AtomShellN, MaxNShells, RealAtoms, AtomCoords, AtomElementMap)
            ! -----------------------------------------------------------------------
            ! Compute the effective charge of an Hirshfeld atom.
            !
            ! Data alignment in the input/output/scratch array A
            ! ---------------------------------------------------
            ! 
            ! N = number of real (non-dummy) atoms
            ! [ ..............................................................]
            !  |<------ N integrals -------->|<--- N elements for --------->|
            !           representing                intermediates
            !           N atomic charges           (scratch, destroyed
            !                                       on entry to this
            !                                       subroutine)
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rho_tot
            real(F64), dimension(:, :), intent(in) :: rho_atomic
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            integer, dimension(:), intent(in)      :: NPrimitives
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, intent(in)                    :: MaxNShells
            integer, dimension(:, :), intent(in)   :: RealAtoms
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, dimension(:), intent(in)      :: AtomElementMap

            integer :: offset_scratch
            real(F64) :: rho_atomic_sum, rho_k, r
            real(F64), dimension(3) :: rk
            integer :: k, n, i, s
            real(F64) :: hirshfeld_weight
            
            n = 0
            do s = 1, 2
                  n = n + (RealAtoms(2, s) - RealAtoms(1, s) + 1)
            end do
            offset_scratch = n
            a(offset_scratch+1:) = ZERO
            rho_atomic_sum = ZERO
            k = 0
            do s = 1, 2
                  do i = RealAtoms(1, s), RealAtoms(2, s)
                        k = k + 1
                        rk(1) = x - AtomCoords(1, i)
                        rk(2) = y - AtomCoords(2, i)
                        rk(3) = z - AtomCoords(3, i)
                        r = norm2(rk)
                        a(offset_scratch+k) = RhoSpherValue(rho_atomic(:, AtomElementMap(i)), r, i, &
                              ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                              AtomShellMap, AtomShellN, MaxNShells)
                        rho_atomic_sum = rho_atomic_sum + a(offset_scratch+k)
                  end do
            end do
            do k = 1, n
                  !
                  ! Electron density of K-th isolated atom
                  !
                  rho_k = a(offset_scratch+k)
                  !
                  ! Electron density of K-th isolated atom
                  ! divided by the sum of all isolated atom
                  ! densities
                  !
                  hirshfeld_weight = rho_k / rho_atomic_sum
                  a(k) = a(k) + grid_weight * hirshfeld_weight * rho_tot
            end do
      end subroutine hirshfeld_population

      
      pure subroutine hirshfeld_volume(a, grid_weight, x, y, z, rho_tot, rho_atomic, &
            ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
            AtomShellMap, AtomShellN, MaxNShells, RealAtoms, AtomCoords, AtomElementMap)
            ! -----------------------------------------------------------------------
            ! Compute the effective Hirshfeld volume of an atom
            ! in a molecule. (See Eq. 7 in Ref. 1)
            !
            ! Data alignment in the input/output/scratch array A
            ! ---------------------------------------------------
            ! 
            ! N = number of real (non-dummy) atoms
            ! [ ..............................................................]
            !  |<------ N integrals -------->|<--- N elements for --------->|
            !           representing                intermediates
            !           N atomic volumes            (scratch, destroyed
            !                                       on entry to this
            !                                       subroutine)
            ! V^{eff}_A <- A(A)
            ! 
            ! -----------------------------------------------------------------------
            ! 1. Tkatchenko, A., Scheffler, M., Accurate Molecular Van Der Waals
            !    Interactions from Ground-State Electron Density and Free-Atom
            !    Reference Data, Phys. Rev. Lett. 102, 073005 (2009); 
            !    doi: 10.1103/PhysRevLett.102.073005
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rho_tot
            real(F64), dimension(:, :), intent(in) :: rho_atomic
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            integer, dimension(:), intent(in)      :: NPrimitives
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, intent(in)                    :: MaxNShells
            integer, dimension(:, :), intent(in)   :: RealAtoms
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, dimension(:), intent(in)      :: AtomElementMap

            integer :: offset_scratch
            real(F64) :: r, r3, rho_atomic_sum, rho_k
            real(F64), dimension(3) :: rk
            integer :: k, n, i, s
            real(F64) :: hirshfeld_weight

            n = 0
            do s = 1, 2
                  n = n + (RealAtoms(2, s) - RealAtoms(1, s) + 1)
            end do
            offset_scratch = n
            a(offset_scratch+1:) = ZERO
            !
            ! For each atom in a molecule, there is an isolated atom
            ! density centered at its coordinates.
            !
            rho_atomic_sum = ZERO
            k = 0
            do s = 1, 2
                  do i = RealAtoms(1, s), RealAtoms(2, s)
                        k = k + 1
                        rk(1) = x - AtomCoords(1, i)
                        rk(2) = y - AtomCoords(2, i)
                        rk(3) = z - AtomCoords(3, i)
                        r = norm2(rk)
                        a(offset_scratch+k) = RhoSpherValue(rho_atomic(:, AtomElementMap(i)), r, i, &
                              ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                              AtomShellMap, AtomShellN, MaxNShells)
                        rho_atomic_sum = rho_atomic_sum + a(offset_scratch+k)
                  end do
            end do
            k = 0
            do s = 1, 2
                  do i = RealAtoms(1, s), RealAtoms(2, s)
                        k = k + 1
                        rk(1) = x - AtomCoords(1, i)
                        rk(2) = y - AtomCoords(2, i)
                        rk(3) = z - AtomCoords(3, i)
                        r = norm2(rk)
                        r3 = r**3
                        !
                        ! Electron density of K-th isolated atom
                        !
                        rho_k = a(offset_scratch+k)
                        !
                        ! Electron density of K-th isolated atom
                        ! divided by the sum of all isolated atom
                        ! densities
                        !
                        hirshfeld_weight = rho_k / rho_atomic_sum
                        a(k) = a(k) + grid_weight * r3 * hirshfeld_weight * rho_tot
                  end do
            end do
      end subroutine hirshfeld_volume


      pure subroutine hirshfeld_volume_free(a, grid_weight, x, y, z, OriginCoords, rho_spher, &
            ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, AtomShellMap, &
            AtomShellN, MaxNShells)
            !
            ! The volume of a free atom, that is, the average value of r**3.
            ! Call this subroutine only for isolated atoms. There should be
            ! no ghost atoms in the system.
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), dimension(3), intent(in)    :: OriginCoords
            real(F64), dimension(:), intent(in)    :: rho_spher
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            integer, dimension(:), intent(in)      :: NPrimitives
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, intent(in)                    :: MaxNShells

            real(F64) :: r, r3
            real(F64), dimension(3) :: rk
            !
            ! This subroutine can be called only for single-atom 
            ! systems
            !
            rk(1) = x - OriginCoords(1)
            rk(2) = y - OriginCoords(2)
            rk(3) = z - OriginCoords(3)
            r = norm2(rk)
            r3 = r**3
            a(1) = a(1) + grid_weight * r3 * RhoSpherValue(rho_spher, r, 1, ShellParamsIdx, &
                  CntrCoeffs, Exponents, NPrimitives, ShellMomentum, AtomShellMap, AtomShellN, &
                  MaxNShells)
      end subroutine hirshfeld_volume_free
end module hirshfeld
