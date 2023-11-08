module AtomicDensities
      use arithmetic
      use math_constants
      use gparam
      use spherh
      use display
      use string
      use gto
      use mgga
      use io
      use basis_sets
      
      implicit none

contains

      ! subroutine RhoSpherCompare_display(RhoVecDFT, RhoVecRef, r0, dr, n)
      !       real(F64), dimension(:), intent(in) :: RhoVecDFT
      !       real(F64), dimension(:), intent(in) :: RhoVecRef
      !       real(F64), intent(in) :: r0
      !       real(F64), intent(in) :: dr
      !       integer, intent(in) :: n

      !       real(F64) :: Rk, RhoKDFT, RhoKRef, RhoKDiff
      !       integer :: k
      !       character(:), allocatable :: line
      !       integer, parameter :: ColWidth = 18
      !       real(F64) :: MeanSquare

      !       call midrule()
      !       call msg("Spherically-averaged atomic density")
      !       call msg("RhoSpher(Rk) = 4Pi**2*Rk**2*(1/(4Pi))*Int Rho(Rk,Theta,Phi)*Sin(Theta)**2dThetadPhi")
      !       call msg("Rk = " // str(r0, d=5) // " + (k-1)*" // str(dr, d=5) // ", k=1, ..., " // str(n))
      !       line = lfield("Rk", ColWidth) // lfield("RhoSpherDFT(Rk)", ColWidth) // lfield("RhoSpherRef(Rk)", ColWidth) &
      !             // lfield("RhoDFT-RhoRef", ColWidth)
      !       call msg(line)
      !       MeanSquare = ZERO
      !       do k = 1, n
      !             Rk = r0 + (k-1) * dr
      !             RhoKDFT = RhoVecDFT(k)
      !             RhoKRef = RhoVecRef(k)
      !             RhoKDiff = RhoKDFT - RhoKRef
      !             line = lfield(str(Rk, d=5), ColWidth) // lfield(str(RhoKDFT, d=5), ColWidth) &
      !                   // lfield(str(RhoKRef, d=5), ColWidth) // lfield(str(RhoKDiff, d=5), ColWidth)
      !             call msg(line)
      !             MeanSquare = MeanSquare + ((RhoKDFT-RhoKRef)/(FOUR*PI*Rk**2))**2 * FOUR*PI*Rk**2 * dr
      !       end do
      !       print *, "MeanSquare = ", Sqrt(MeanSquare)
      !       call midrule()
      ! end subroutine RhoSpherCompare_display
      

      ! subroutine RhoSpher_display(RhoVec, r0, dr, n)
      !       real(F64), dimension(:), intent(in) :: RhoVec
      !       real(F64), intent(in) :: r0
      !       real(F64), intent(in) :: dr
      !       integer, intent(in) :: n

      !       real(F64) :: Rk, Rhok
      !       integer :: k
      !       character(:), allocatable :: line
      !       integer, parameter :: ColWidth = 15

      !       call midrule()
      !       call msg("Spherically-averaged atomic density")
      !       call msg("RhoSpher(Rk) = 4Pi**2*Rk**2*(1/(4Pi))*Int Rho(Rk,Theta,Phi)*Sin(Theta)**2dThetadPhi")
      !       call msg("Rk = " // str(r0, d=5) // " + (k-1)*" // str(dr, d=5) // ", k=1, ..., " // str(n))
      !       line = lfield("Rk", ColWidth) // lfield("RhoSpher(Rk)", ColWidth)
      !       call msg(line)
      !       do k = 1, n
      !             Rk = r0 + (k-1) * dr
      !             Rhok = RhoVec(k)
      !             line = lfield(str(Rk, d=5), ColWidth) // lfield(str(Rhok, d=5), ColWidth)
      !             call msg(line)
      !       end do
      !       call midrule()
      ! end subroutine RhoSpher_display

      
      ! subroutine RhoSpherVector(RhoVec, Rho_ao, r0, dr, n)
      !       !
      !       ! Compute the spherically-averaged density of a single-atom system for a sequence
      !       ! of n radial points
      !       !
      !       ! rk = r0 + (k-1) * dr,    k = 0, 1, ..., n
      !       !
      !       ! The Jacobian factor 4*Pi*rk**2 is included in the computed values; therefore,
      !       ! if the grid is dense enough, Sum(k) RhoVec(k)*dr should equal the number
      !       ! of electrons.
      !       !
      !       ! If present, the spin components of Rho_ao are summed up into a single total density
      !       ! matrix.
      !       !
      !       real(F64), dimension(:), intent(out) :: RhoVec
      !       real(F64), dimension(:, :, :), intent(in) :: Rho_ao
      !       real(F64), intent(in) :: r0
      !       real(F64), intent(in) :: dr
      !       integer, intent(in) :: n

      !       integer :: k, NSpin, s
      !       real(F64) :: r
      !       real(F64), dimension(:), allocatable :: SpherAvg

      !       RhoVec = ZERO
      !       allocate(SpherAvg((NSHELL*(NSHELL+1))/2))
      !       NSpin = size(Rho_ao, dim=3)
      !       do s = 1, NSpin
      !             call RhoSpherCoeffs(SpherAvg, Rho_ao(:, :, s))
      !             do k = 1, n
      !                   r = r0 + (k-1)*dr
      !                   RhoVec(k) = RhoVec(k) + FOUR*PI*r**2 * RhoSpherValue(SpherAvg, r, 1)
      !             end do
      !       end do
      ! end subroutine RhoSpherVector


      subroutine RhoSpherCoeffs(RhoAvg, Rho_cao, AOBasis)
            !
            ! Compute the coefficients of the spherically-averaged electron density,
            ! which are subsequently used by RhoSpherValue to compute the spherically
            ! averaged density at a particular point in space.
            !
            ! This subroutine can be called only for single-atom systems.
            !
            real(F64), dimension(:), intent(out)      :: RhoAvg
            real(F64), dimension(:, :, :), intent(in) :: Rho_cao
            type(TAOBasis), intent(in)                :: AOBasis

            integer :: k
            integer :: u, v, i, j, a, b, uu, vv
            integer :: Lmax, lu, lv
            integer :: nint, nu, nv
            integer :: lx, ly, lz
            real(F64) :: s, rhoji
            integer :: tau
            integer :: NSpins
            real(F64) :: spherint            
            real(F64), dimension(:), allocatable :: sphere_integrals
            !
            ! Maximum angular momentum of a product
            ! of two orbitals
            !
            Lmax = 2 * AOBasis%LmaxGTO
            !
            ! Number of Cartesian polynomials x**l*y**m*z**n
            ! for l+m+n=Lmax
            !
            nint = numxyz(Lmax)
            allocate(sphere_integrals(nint))
            NSpins = size(Rho_cao, dim=3)
            !
            ! Number of shells in a basis set
            ! for an isolated atom
            !
            associate ( &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  NAngFuncCart => AOBasis%NAngFuncCart, &
                  Exponents => AOBasis%Exponents, &
                  NormFactorsCart => AOBasis%NormFactorsCart, &
                  NShells => AOBasis%NShells, &
                  ShellLocCart => AOBasis%ShellLocCart, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ &
                  )
                  RhoAvg = ZERO
                  do tau = 1, NSpins
                        k = 1
                        do uu = 1, NShells
                              u = ShellParamsIdx(uu)
                              lu = ShellMomentum(u)
                              nu = NAngFuncCart(u)
                              do vv = uu, NShells
                                    v = ShellParamsIdx(vv)
                                    lv = ShellMomentum(v)
                                    nv = NAngFuncCart(v)
                                    s = ZERO
                                    call xyzint(sphere_integrals, lu+lv, 0, 0, 0)
                                    !
                                    ! Spherical average = Sum(angular funcs) 1/(4Pi) Integral PhiI PhiJ dOmega
                                    !
                                    do i = 1, nu
                                          do j = 1, nv
                                                lx = CartPolyX(i, lu) + CartPolyX(j, lv)
                                                ly = CartPolyY(i, lu) + CartPolyY(j, lv)
                                                lz = CartPolyZ(i, lu) + CartPolyZ(j, lv)
                                                spherint = sphere_integrals(lxlylzpos(lx, ly, lz))
                                                a = ShellLocCart(uu) + i - 1
                                                b = ShellLocCart(vv) + j - 1
                                                rhoji = Rho_cao(b, a, tau)
                                                s = s + rhoji * NormFactorsCart(i, u) * NormFactorsCart(j, v) &
                                                      * spherint / (FOUR * PI)
                                          end do
                                    end do
                                    if (uu /= vv) then
                                          RhoAvg(k) = RhoAvg(k) + TWO * s
                                    else
                                          RhoAvg(k) = RhoAvg(k) + s
                                    end if
                                    k = k + 1
                              end do
                        end do
                  end do
            end associate
      end subroutine RhoSpherCoeffs


      pure function RhoSpherValue(RhoCoeffs, r, k, ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, &
            ShellMomentum, AtomShellMap, AtomShellN, MaxNShells)
            !
            ! Compute spherically-averaged density of an atom K at
            ! a given radius r. The Jacobian 4*Pi*r**2 isn't
            ! included.
            !
            real(F64)                              :: RhoSpherValue
            real(F64), dimension(:), intent(in)    :: RhoCoeffs
            real(F64), intent(in)                  :: r
            integer, intent(in)                    :: k
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            integer, dimension(:), intent(in)      :: NPrimitives
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, intent(in)                    :: MaxNShells
            
            real(F64), dimension(MaxNShells) :: chi_k
            integer :: u, v
            integer :: NShellsK
            real(F64) :: s
            integer :: w
            integer :: l

            call AORadialPart(chi_k, r, k, ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, &
                  ShellMomentum, AtomShellMap, AtomShellN)
            NShellsK = 0
            do l = 1, AtomShellN(k)
                  NShellsK = NShellsK + AtomShellMap(2, l, k) - AtomShellMap(1, l, k) + 1
            end do
            RhoSpherValue = ZERO
            w = 1
            do u = 1, NShellsK
                  s = ZERO
                  do v = u, NShellsK
                        s = s + chi_k(v) * RhoCoeffs(w)
                        w = w + 1
                  end do
                  RhoSpherValue = RhoSpherValue + chi_k(u) * s
            end do
      end function RhoSpherValue

      
      pure subroutine AORadialPart(chi_i, r, i, ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, &
            ShellMomentum, AtomShellMap, AtomShellN)
            !
            ! Compute the radial part ChiI of an AO orbital PhiI:
            !
            ! PhiI = Sum(NPrimitives) Ck * (x-x0)**l (y-y0)**m (z-z0)**n Exp(-alpha*r**2)
            ! ChiI = r**(l+m+n) * Sum(NPrimitives) Ck * Exp(-alpha*r**2)
            ! r = Sqrt((x-x0)**2+(y-y0)**2+(z-z0)**2)
            !
            ! The Jacobian factor 4Pi*r**2 isn't included in ChiI.
            !
            real(F64), dimension(:), intent(out)   :: chi_i
            real(F64), intent(in)                  :: r
            integer, intent(in)                    :: i
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            integer, dimension(:), intent(in)      :: NPrimitives
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            
            integer :: k, k0, k1, s
            integer :: v
            real(F64) :: ao_val_spher
            integer :: l, m

            chi_i = ZERO
            m = 1
            do l = 1, AtomShellN(i)
                  k0 = AtomShellMap(1, l, i)
                  k1 = AtomShellMap(2, l, i)
                  !
                  ! Loop over orbital shells centered at the I-th atom
                  !
                  do k = k0, k1
                        s = ShellParamsIdx(k)
                        ao_val_spher = ZERO
                        do v = 1, NPrimitives(s)
                              ao_val_spher = ao_val_spher + CntrCoeffs(v, s) * exp(-Exponents(v, s) * r**2)
                        end do
                        !
                        ! Return the spherical part of the orbitals in the K-th shell.
                        ! The L-dependent coefficients NormFactors have been already contracted
                        ! in the spherically-averaged density matrix.
                        !
                        chi_i(m) = r**ShellMomentum(s) * ao_val_spher
                        m = m + 1
                  end do
            end do
      end subroutine AORadialPart


      pure subroutine density_rmsd(a, RhoMatrixRef, Rho, Sigma, Lapl, grid_weight, &
            orbval, shellidx, n0)
            !
            ! Compute squares of the differences between Rho, NablaRho, and LaplRho
            ! and the reference functions RhoRef, NablaRhoRef, and LaplRhoRef
            !
            ! 1. Int (Rho(r) - RhoRef(r))**2 dxdydz
            ! 2. Int (Sqrt(NablaRho(r)**2) - Sqrt(NablaRhoRef(r)**2)) dxdydz
            ! 3. Int (LaplRho(r) - LaplRhoRef(r))**2 dxdydz
            !
            ! The reference density matrix RhoRef is read from disk/memory and passed
            ! to this subroutine. The density matrix Rho corresponds to the current
            ! SCF computation.
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), dimension(:, :), intent(in) :: RhoMatrixRef
            real(F64), intent(in)                  :: Rho
            real(F64), intent(in)                  :: Sigma
            real(F64), intent(in)                  :: Lapl
            real(F64), intent(in)                  :: grid_weight
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0

            real(F64) :: RhoRef, SigmaRef, LaplRef, TauRef
            real(F64), dimension(3) :: GradRef
            !
            ! Compute reference density, gradient, and the laplacian
            !
            call mgga_RhoSigLapTau(RhoRef, GradRef, SigmaRef, LaplRef, TauRef, RhoMatrixRef, &
                  orbval, shellidx, n0)
            a(1) = a(1) + grid_weight * (RhoRef - Rho)**2
            a(2) = a(2) + grid_weight * (Sqrt(abs(SigmaRef)) - Sqrt(abs(Sigma)))**2
            a(3) = a(3) + grid_weight * (LaplRef - Lapl)**2
      end subroutine density_rmsd


      subroutine CompareRhoInteraction(RhoSpin_ao)
            !
            ! Compute noncovalent/covalent interaction-induced changes in the electron density,
            ! density gradient, and Laplacian.
            !
            real(F64), dimension(:, :, :), intent(in) :: RhoSpin_ao

            real(F64), dimension(:, :), allocatable :: RhoA_ao, RhoB_ao, RhoAB_ao
            real(F64), dimension(:, :), allocatable :: RhoRefA_ao, RhoRefB_ao, RhoRefAB_ao
            real(F64), dimension(:, :), allocatable :: WorkA, WorkB
            real(F64), dimension(:, :), allocatable :: RhoVec, SVec, LapVec
            integer :: j, NSpin
            integer :: k, l
            integer :: n, n0
            integer, dimension(:), allocatable :: shellidx
            real(F64), dimension(:), allocatable :: orbval
            real(F64) :: x, y, z, x0, y0, z0, x1, y1, z1, dx, dy, dz
            real(F64) :: rhoA, sigA, lapA, tauA, sA
            real(F64) :: rhoB, sigB, lapB, tauB, sB
            real(F64) :: rhoAB, sigAB, lapAB, tauAB, sAB
            real(F64), dimension(3) :: gradA, gradB, gradAB
            real(F64) :: rho, s, lap, rhoRef, sRef, lapRef
            real(F64) :: RhoDiff, SDiff, LapDiff
            real(F64) :: RhoInt, SInt, LapInt
            real(F64) :: RhoRMSD, SRMSD, LapRMSD
            logical :: RefExists, CSV
            character(:), allocatable :: line
            real(F64), dimension(12) :: PrintedData
            integer :: CSVFile
            integer :: a0, a1, b0, b1
            integer :: NorbA, NorbB
            integer :: NData
            integer, parameter :: ColWidth = 15
            integer, parameter :: NDigits = 4
            character(:), allocatable :: delim

            call midrule()
            call msg("Computing interaction-induced changes in electron density RhoAB-RhoA-RhoB")

            if (allocated(RHO_DIFF_RefA) .and. allocated(RHO_DIFF_RefB) .and. allocated(RHO_DIFF_RefAB)) then
                  RefExists = .true.
            else
                  RefExists = .false.
            end if

            if (minval(RHO_DIFF_MonoA) < 0 .or. minval(RHO_DIFF_MonoB) < 0) then
                  call msg("Cannot compute DeltaAB: unspecified monomers", MSG_ERROR)
                  stop
            end if

            if (allocated(RHO_DIFF_CSV)) then
                  CSV = .true.                  
            else
                  CSV = .false.
            end if

            if (CSV) then
                  CSVFile = io_text_open(RHO_DIFF_CSV, "REPLACE")
            end if

            if (CSV) then
                  delim = ";"
            else
                  delim = ""
            end if
            
            if (.not. (allocated(RHO_DIFF_A) .and. allocated(RHO_DIFF_B))) then
                  call msg("Incomplete set of paths defined for the RhoDiff procedure", MSG_ERROR)
                  stop
            end if
            
            allocate(RhoA_ao(NORB, NORB))
            allocate(RhoB_ao(NORB, NORB))
            allocate(RhoAB_ao(NORB, NORB))
            if (RefExists) then
                  allocate(RhoRefA_ao(NORB, NORB))
                  allocate(RhoRefB_ao(NORB, NORB))
                  allocate(RhoRefAB_ao(NORB, NORB))
            end if
            NSpin = size(RhoSpin_ao, dim=3)
            !
            ! Define the range of orbital indices
            ! on each interacting molecule
            !
            a0 = IDX(RHO_DIFF_MonoA(1))
            a1 = IDX(RHO_DIFF_MonoA(2)+1) - 1
            NorbA = a1 - a0 + 1
            b0 = IDX(RHO_DIFF_MonoB(1))
            b1 = IDX(RHO_DIFF_MonoB(2)+1) - 1
            NorbB = b1 - b0 + 1
            allocate(WorkA(NorbA, NorbA))
            allocate(WorkB(NorbB, NorbB))
            if (RefExists) then
                  if (RHO_DIFF_FILEMODE == FILEMODE_TEXT) then
                        call io_text_read(RhoRefAB_ao, RHO_DIFF_RefAB)
                        call io_text_read(WorkA, RHO_DIFF_RefA)
                        call io_text_read(WorkB, RHO_DIFF_RefB)
                  else
                        call io_binary_read(RhoRefAB_ao, RHO_DIFF_RefAB)
                        call io_binary_read(WorkA, RHO_DIFF_RefA)
                        call io_binary_read(WorkB, RHO_DIFF_RefB)
                  end if
                  RhoRefA_ao = ZERO
                  RhoRefB_ao = ZERO
                  RhoRefA_ao(a0:a1, a0:a1) = WorkA
                  RhoRefB_ao(b0:b1, b0:b1) = WorkB
            end if
            RhoAB_ao = ZERO
            do j = 1, NSpin
                  RhoAB_ao = RhoAB_ao + RhoSpin_ao(:, :, j)
            end do
            if (RHO_DIFF_FILEMODE == FILEMODE_TEXT) then
                  call io_text_read(WorkA, RHO_DIFF_A)
                  call io_text_read(WorkB, RHO_DIFF_B)
            else
                  call io_binary_read(WorkA, RHO_DIFF_A)
                  call io_binary_read(WorkB, RHO_DIFF_B)
            end if
            RhoA_ao = ZERO
            RhoB_ao = ZERO
            RhoA_ao(a0:a1, a0:a1) = WorkA
            RhoB_ao(b0:b1, b0:b1) = WorkB
            if (RHO_DIFF_ProbeN < 2) then
                  call msg("Invalid number of nodes requested for RhoDiff", MSG_ERROR)
                  stop
            end if
            n = RHO_DIFF_ProbeN
            x0 = RHO_DIFF_ProbeStart(1)
            y0 = RHO_DIFF_ProbeStart(2)
            z0 = RHO_DIFF_ProbeStart(3)
            x1 = RHO_DIFF_ProbeStop(1)
            y1 = RHO_DIFF_ProbeStop(2)
            z1 = RHO_DIFF_ProbeStop(3)
            dx = (x1 - x0) / (n - 1)
            dy = (y1 - y0) / (n - 1)
            dz = (z1 - z0) / (n - 1)
            if (RefExists) then
                  allocate(RhoVec(2, n))
                  allocate(SVec(2, n))
                  allocate(LapVec(2, n))
            else
                  allocate(RhoVec(1, n))
                  allocate(SVec(1, n))
                  allocate(LapVec(1, n))
            end if
            !$omp parallel &
            !$omp default(shared) &
            !$omp private(k, x, y, z, orbval, shellidx, n0) &
            !$omp private(rhoA, gradA, sigA, lapA, tauA, sA) &
            !$omp private(rhoB, gradB, sigB, lapB, tauB, sB) &
            !$omp private(rhoAB, gradAB, sigAB, lapAB, tauAB, sAB)
            allocate(orbval(NORB*MGGA_DELTAK))
            allocate(shellidx(NSHELL))
            !$omp do schedule(guided)
            do k = 1, n
                  x = x0 + (k - 1) * dx
                  y = y0 + (k - 1) * dy
                  z = z0 + (k - 1) * dz
                  call mgga_orbval(x, y, z, orbval, shellidx, n0)
                  call mgga_RhoSigLapTau(rhoA, gradA, sigA, lapA, tauA, RhoA_ao, orbval, shellidx, n0)
                  call mgga_RhoSigLapTau(rhoB, gradB, sigB, lapB, tauB, RhoB_ao, orbval, shellidx, n0)
                  call mgga_RhoSigLapTau(rhoAB, gradAB, sigAB, lapAB, tauAB, RhoAB_ao, orbval, shellidx, n0)
                  !
                  ! Compute reduced density gradients s = |NablaRho|/(2*(3*Pi**2)**(1/3)*Rho**(4/3))
                  !
                  sigA = max(ZERO, sigA)
                  rhoA = max(ZERO, rhoA)
                  sigB = max(ZERO, sigB)
                  rhoB = max(ZERO, rhoB)
                  sigAB = max(ZERO, sigAB)
                  rhoAB = max(ZERO, rhoAB)
                  sA = Sqrt(sigA)
                  sB = Sqrt(sigB)
                  sAB = Sqrt(sigAB)
                  RhoVec(1, k) = rhoAB - rhoA - rhoB
                  SVec(1, k) = sAB - sA - sB
                  LapVec(1, k) = lapAB - lapA - lapB
                  if (RefExists) then
                        call mgga_RhoSigLapTau(rhoA, gradA, sigA, lapA, tauA, RhoRefA_ao, orbval, shellidx, n0)
                        call mgga_RhoSigLapTau(rhoB, gradB, sigB, lapB, tauB, RhoRefB_ao, orbval, shellidx, n0)
                        call mgga_RhoSigLapTau(rhoAB, gradAB, sigAB, lapAB, tauAB, RhoRefAB_ao, orbval, shellidx, n0)
                        sigA = max(ZERO, sigA)
                        rhoA = max(ZERO, rhoA)
                        sigB = max(ZERO, sigB)
                        rhoB = max(ZERO, rhoB)
                        sigAB = max(ZERO, sigAB)
                        rhoAB = max(ZERO, rhoAB)
                        sA = Sqrt(sigA)
                        sB = Sqrt(sigB)
                        sAB = Sqrt(sigAB)
                        RhoVec(2, k) = rhoAB - rhoA - rhoB
                        SVec(2, k) = sAB - sA - sB
                        LapVec(2, k) = lapAB - lapA - lapB
                  end if
            end do
            !$omp end do
            !$omp end parallel
            ! -------------------------------------------------------------
            !                      Print data table
            ! -------------------------------------------------------------
            call msg("Starting point: (" // str(toang(x0),3) // "," // str(toang(y0),3) // "," // str(toang(z0),3) // ")")
            call msg("Stopping point: (" // str(toang(x1),3) // "," // str(toang(y1),3) // "," // str(toang(z1),3) // ")")
            call msg("Rho = RhoAB-RhoA-RhoB")
            call msg("SX = Sqrt(NablaRhoX*NablaRhoX)")
            call msg("S = SAB - SA - SB")
            call msg("Lap = Nabla**2(RhoAB-RhoA-RhoB)")
            call msg("Coordinates are given in angstroms")
            call msg("Electron densities, gradients, and the Laplacian are in a.u.")
            if (RefExists) then
                  line = lfield("x", ColWidth) // delim // lfield("y", ColWidth) // delim //  lfield("z", ColWidth) // &
                        delim // lfield("Rho", ColWidth) // delim // lfield("S", ColWidth) // delim // lfield("Lap", ColWidth) // &
                        delim // lfield("RhoRef", ColWidth) // delim // lfield("SRef", ColWidth) // &
                        delim // lfield("LapRef", ColWidth) // delim // lfield("Rho-RhoRef", ColWidth) // &
                        delim // lfield("S-SRef", ColWidth) // delim // lfield("Lap-LapRef", ColWidth)
            else
                  line = lfield("x", ColWidth) // delim // lfield("y", ColWidth) // delim // lfield("z", ColWidth) // &
                        lfield("Rho", ColWidth) // delim // lfield("S", ColWidth) // delim // lfield("Lap", ColWidth)
            end if
            if (CSV) then
                  write(CSVFile, "(A)") line
            else
                  call msg(line)
            end if
            RhoInt = ZERO
            SInt = ZERO
            LapInt = ZERO
            do k = 1, n
                  x = x0 + (k - 1) * dx
                  y = y0 + (k - 1) * dy
                  z = z0 + (k - 1) * dz
                  rho = RhoVec(1, k)
                  s = SVec(1, k)
                  lap = LapVec(1, k)
                  PrintedData(1) = toang(x)
                  PrintedData(2) = toang(y)
                  PrintedData(3) = toang(z)
                  PrintedData(4) = rho
                  PrintedData(5) = s
                  PrintedData(6) = lap
                  NData = 6
                  if (RefExists) then
                        rhoRef = RhoVec(2, k)
                        sRef = SVec(2, k)
                        lapRef = LapVec(2, k)
                        RhoDiff = rho - rhoRef
                        SDiff = s - sRef
                        LapDiff = lap - lapRef
                        RhoInt = RhoInt + RhoDiff**2
                        SInt = SInt + SDiff**2
                        LapInt = LapInt + LapDiff**2
                        PrintedData(7) = rhoRef
                        PrintedData(8) = sRef
                        PrintedData(9) = lapRef
                        PrintedData(10) = RhoDiff
                        PrintedData(11) = SDiff
                        PrintedData(12) = LapDiff
                        NData = 12
                  end if
                  line = ""
                  do l = 1, NData
                        if (l > 1) then
                              line = line // delim // lfield(str(PrintedData(l), d=NDigits), ColWidth)
                        else
                              line = line // lfield(str(PrintedData(l), d=NDigits), ColWidth)
                        end if
                  end do
                  if (CSV) then
                        write(CSVFile, "(A)") line
                  else
                        call msg(line)
                  end if
            end do
            if (RefExists) then
                  RhoRMSD = Sqrt(RhoInt/n)
                  SRMSD = Sqrt(SInt/n)
                  LapRMSD = Sqrt(LapInt/n)
                  call msg("Sqrt(Sum((Rho-RhoRef)(Rk))**2/n) = " // str(RhoRMSD, d=NDigits))
                  call msg("Sqrt(Sum((S-SRef)(Rk))**2/n) = " // str(SRMSD, d=NDigits))
                  call msg("Sqrt(Sum((Lap-LapRef)(Rk))**2/n) = " // str(LapRMSD, d=NDigits))
            end if
            if (CSV) then
                  close(CSVFile)
                  call msg("Data were written to " // RHO_DIFF_CSV)
            end if
            call midrule()
      end subroutine CompareRhoInteraction


      subroutine CompareRhoSpherical(Rho_ao, x0, y0, z0, SpherX, SpherY, SpherZ, SpherW)
            real(F64), dimension(:, :, :), intent(in) :: Rho_ao
            real(F64), intent(in) :: x0, y0, z0
            real(F64), dimension(:), intent(in) :: SpherX, SpherY, SpherZ, SpherW

            real(F64), dimension(:), allocatable :: RhoSpher, SigSpher, LapSpher
            real(F64), dimension(:), allocatable :: RhoRefSpher, SigRefSpher, LapRefSpher
            real(F64), dimension(:), allocatable :: orbval
            integer, dimension(:), allocatable :: shellidx
            real(F64) :: rho, lap, tau, sig
            real(F64) :: rhoRef, lapRef, tauRef, sigRef
            real(F64) :: rhoDiff, lapDiff, sigDiff
            real(F64), dimension(3) :: grad, gradRef, gradTot
            real(F64) :: x, y, z, r, w
            integer :: k, l, n0
            integer :: NSpher, NSpin, s
            character(:), allocatable :: line
            integer, parameter :: ColWidth = 15
            integer, parameter :: NDigits = 5
            real(F64) :: RhoInt, SigInt, LapInt, RhoRMSD, SigRMSD, LapRMSD
            real(F64) :: r0, dr
            integer :: n
            logical :: reference
            real(F64), dimension(:, :), allocatable :: RhoRef_ao
            integer :: CSVFile
            logical :: CSV

            if (allocated(RHO_SPHER_CSV)) then
                  CSV = .true.
            else
                  CSV = .false.
            end if

            if (CSV) then
                  CSVFile = io_text_open(RHO_SPHER_CSV, "REPLACE")
            end if
            
            if (allocated(RHO_SPHER_REF)) then
                  allocate(RhoRef_ao(NORB, NORB))
                  if (RHO_SPHER_FILEMODE == FILEMODE_TEXT) then
                        call io_text_read(RhoRef_ao, RHO_SPHER_REF)
                  else
                        call io_binary_read(RhoRef_ao, RHO_SPHER_REF)
                  end if
                  reference = .true.
            else
                  reference = .false.
            end if
            r0 = RHO_SPHER_R0
            dr = RHO_SPHER_DR
            n = RHO_SPHER_N
            NSpher = size(SpherX)
            NSpin = size(Rho_ao, dim=3)
            allocate(RhoSpher(n))
            allocate(SigSpher(n))
            allocate(LapSpher(n))
            allocate(RhoRefSpher(n))
            allocate(SigRefSpher(n))
            allocate(LapRefSpher(n))
            call midrule()
            call msg("Spherically-averaged atomic density")
            call msg("Numerical integration is done using " // str(NSpher) // "-point spherical grid")
            call msg("Origin of the spherical grid is at (" // str(toang(x0),3) // "," // str(toang(y0),3) // "," // str(toang(z0),3) // ")")
            RhoSpher = ZERO
            SigSpher = ZERO
            LapSpher = ZERO
            RhoRefSpher = ZERO
            SigRefSpher = ZERO
            LapRefSpher = ZERO
            !$omp parallel &
            !$omp private(orbval, shellidx, k, l, s, rho, grad, sig, lap, tau, x, y, z, r, w, n0) &
            !$omp private(rhoRef, gradRef, sigRef, lapRef, tauRef, gradTot) &
            !$omp default(shared)
            allocate(orbval(NORB*MGGA_DELTAK))
            allocate(shellidx(NSHELL))
            !$omp do schedule(guided)
            do k = 1, n
                  do l = 1, NSpher
                        r = r0 + (k-1) * dr
                        x = x0 + r * SpherX(l)
                        y = y0 + r * SpherY(l)
                        z = z0 + r * SpherZ(l)
                        w = FOUR*PI * r**2 * SpherW(l)                        
                        call mgga_orbval(x, y, z, orbval, shellidx, n0)
                        gradTot = ZERO
                        do s = 1, NSpin
                              call mgga_RhoSigLapTau(rho, grad, sig, lap, tau, Rho_ao(:, :, s), &
                                    orbval, shellidx, n0)
                              RhoSpher(k) = RhoSpher(k) + w * rho
                              gradTot = gradTot + grad
                              LapSpher(k) = LapSpher(k) + w * lap
                        end do
                        sig = abs(dot_product(gradTot, gradTot))
                        SigSpher(k) = sigSpher(k) + w * Sqrt(sig)
                        if (reference) then
                              call mgga_RhoSigLapTau(rhoRef, gradRef, sigRef, lapRef, tauRef, RhoRef_ao(:, :), &
                                    orbval, shellidx, n0)
                              RhoRefSpher(k) = RhoRefSpher(k) + w * rhoRef
                              sigRef = abs(dot_product(gradRef, gradRef))
                              SigRefSpher(k) = SigRefSpher(k) + w * Sqrt(sigRef)
                              LapRefSpher(k) = LapRefSpher(k) + w * lapRef
                        end if
                  end do
            end do
            !$omp end do nowait
            !$omp end parallel
            ! -------------------------------------------------------------
            !                      Print data table
            ! -------------------------------------------------------------
            call msg("<X> = 4Pi*Rk**2*(1/(4Pi))*Int X(Rk,Theta,Phi)*Sin(Theta)**2 dTheta dPhi")
            call msg("<Sig> = <Sqrt(NablaRho*NablaRho)>")
            call msg("<Lap> = <Nabla**2Rho>")
            call msg("Rk = " // str(r0, d=NDigits) // " + (k-1)*" // str(dr, d=NDigits) // ", k=1, ..., " // str(n))
            if (reference) then
                  call msg('The postfix "Ref" is added to the values computed using RhoRef_ao')
                  call msg("RhoRef_ao is read from " // RHO_SPHER_REF)
                  line = lfield("Rk [a.u.]", ColWidth) // lfield("<Rho>", ColWidth) // &
                        lfield("<Sig>", ColWidth) // lfield("<Lap>", ColWidth) // &
                        lfield("<RhoRef>", ColWidth) // lfield("<SigRef>", ColWidth) // &
                        lfield("<LapRef>", ColWidth) // lfield("<Rho-RhoRef>", ColWidth) // &
                        lfield("<Sig-SigRef>", ColWidth) // lfield("<Lap-LapRef>", ColWidth)
            else
                  line = lfield("Rk [a.u.]", ColWidth) // lfield("<Rho>", ColWidth) // &
                        lfield("<Sig>", ColWidth) // lfield("<Lap>", ColWidth)
            end if
            if (CSV) then
                  write(CSVFile, "(A)") line
            else
                  call msg(line)
            end if
            RhoInt = ZERO
            SigInt = ZERO
            LapInt = ZERO
            do k = 1, n
                  r = r0 + (k-1) * dr
                  rho = RhoSpher(k)
                  lap = LapSpher(k)
                  sig = SigSpher(k)
                  if (reference) then
                        rhoRef = RhoRefSpher(k)
                        lapRef = LapRefSpher(k)
                        sigRef = SigRefSpher(k)
                        RhoDiff = rho - rhoRef
                        SigDiff = sig - sigRef
                        LapDiff = lap - lapRef
                        line = lfield(str(r, d=NDigits), ColWidth) // lfield(str(rho, d=NDigits), ColWidth) &
                              // lfield(str(sig, d=NDigits), ColWidth) // lfield(str(lap, d=NDigits), ColWidth) &
                              // lfield(str(rhoRef, d=NDigits), ColWidth) // lfield(str(sigRef, d=NDigits), ColWidth) &
                              // lfield(str(lapRef, d=NDigits), ColWidth) // lfield(str(RhoDiff, d=NDigits), ColWidth) &
                              // lfield(str(SigDiff, d=NDigits), ColWidth) // lfield(str(LapDiff, d=NDigits), ColWidth)
                        RhoInt = RhoInt + (rho-rhoRef)**2
                        SigInt = SigInt + (sig-sigRef)**2
                        LapInt = LapInt + (lap-lapRef)**2
                  else
                        line = lfield(str(r, d=NDigits), ColWidth) // lfield(str(rho, d=NDigits), ColWidth) &
                              // lfield(str(sig, d=NDigits), ColWidth) // lfield(str(lap, d=NDigits), ColWidth)
                  end if
                  if (CSV) then
                        write(CSVFile, "(A)") line
                  else
                        call msg(line)
                  end if
            end do
            if (reference) then
                  RhoRMSD = Sqrt(RhoInt/n)
                  SigRMSD = Sqrt(SigInt/n)
                  LapRMSD = Sqrt(LapInt/n)
                  call msg("Sqrt(Sum(<Rho-RhoRef>(Rk))**2/n) = " // str(RhoRMSD, d=NDigits))
                  call msg("Sqrt(Sum(<Sig-SigRef>(Rk))**2/n) = " // str(SigRMSD, d=NDigits))
                  call msg("Sqrt(Sum(<Lap-LapRef>(Rk))**2/n) = " // str(LapRMSD, d=NDigits))
            end if
            if (CSV) then
                  close(CSVFile)
                  call msg("Data were written to " // RHO_SPHER_CSV)
            end if
            call midrule()
      end subroutine CompareRhoSpherical
end module AtomicDensities
