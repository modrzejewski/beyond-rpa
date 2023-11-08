module scfutils
      use arithmetic
      use math_constants
      use gparam
      use basis
      use linalg
      use ecpint

      implicit none

contains

      subroutine rhotrace(trace, rho, a)
            ! -----------------------------------------------------
            ! Calculate the average value of an observable A:
            ! <A> = TRACE <- Tr(RHO * A). Upper triangles of RHO
            ! and A matrices are not referenced.
            !
            real(F64), intent(out)                 :: trace
            real(F64), dimension(:, :), intent(in) :: rho
            real(F64), dimension(:, :), intent(in) :: a

            real(F64) :: t
            integer :: i, j

            trace = zero
            do j = 1, norb
                  trace = trace + rho(j, j) * a(j, j)
                  t = zero
                  do i = j + 1, norb
                        t = t + rho(i, j) * a(i, j)
                  end do
                  trace = trace + two * t
            end do
      end subroutine rhotrace


      function nuclrep()
            real(F64) :: nuclrep

            integer :: i, j, s, t
            real(F64) :: xa, ya, za
            real(F64) :: xb, yb, zb
            real(F64) :: qa, qb
            real(F64) :: r

            nuclrep = ZERO
            !
            ! Loop over non-dummy atoms
            !
            do s = 1, 2
                  do i = REAL_ATOMS(1, s), REAL_ATOMS(2, s)
                        xa = atomr(1, i)
                        ya = atomr(2, i)
                        za = atomr(3, i)
                        qa = real(ECP_INUCLZ(i), F64)
                        do t = 1, 2
                              do j = max(i+1, REAL_ATOMS(1, t)), REAL_ATOMS(2, t)
                                    xb = atomr(1, j)
                                    yb = atomr(2, j)
                                    zb = atomr(3, j)
                                    qb = real(ECP_INUCLZ(j), F64)
                                    r = sqrt((xa - xb)**2 + (ya - yb)**2 + (za - zb)**2)
                                    nuclrep = nuclrep + qa * qb / r
                              end do
                        end do
                  end do
            end do
      end function nuclrep


      subroutine rks_density(rho_ao, rho_oao, c, n, invsq)
            ! ---------------------------------------------
            ! Calculate density matrix in atomic orbital
            ! basis (AO) and symmetrically orthogonalized
            ! atomic orbital basis (OAO). Both lower and 
            ! upper traiangles of RHO_AO and RHO_OAO
            ! matrices are computed. Use this subroutine 
            ! in for closed-shell systems.
            ! Optionally, this subroutine can compute an
            ! individual contrubtion of a given occupied
            ! orbital to the AO density matrix.
            ! ---------------------------------------------
            ! RHO_AO  - Output, density matrix (AO)
            ! RHO_OAO - Output, density matrix (OAO)
            ! C       - Input, MO coefficients (OAO)
            ! N       - Input, number of doubly-occupied
            !           orbitals
            ! INVSQ   - Input, S^{-1/2} (AO), assumed that
            !           both lower and upper triangle
            !           of INVSQ are stored
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_ao
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_oao
            real(F64), dimension(:, :), contiguous, intent(in)  :: c
            integer, intent(in)                                 :: n
            real(F64), dimension(:, :), contiguous, intent(in)  :: invsq
            !
            ! 1. RHO_OAO <- S^{-1/2} C
            ! 2. RHO_AO  <- 2 * RHO_OAO RHO_OAO^T
            ! 3. RHO_OAO <- 2 * C C^T
            !
            call symmwrap("L", "L", NORB, n, ONE, invsq, c, ZERO, rho_oao)
            call syrkwrap("L", "N", NORB, n, TWO, rho_oao, ZERO, rho_ao)
            call syrkwrap("L", "N", NORB, n, TWO, c, ZERO, rho_oao)
            call smfill(rho_ao)
            call smfill(rho_oao)
      end subroutine rks_density


      subroutine calcvirt(mocoeff, rho, work, nocc, nvirt)
            real(F64), dimension(:, :), contiguous, intent(inout) :: mocoeff
            real(F64), dimension(:, :), contiguous, intent(in)    :: rho
            real(F64), dimension(:, :), contiguous, intent(out)   :: work
            integer, intent(in)                                   :: nocc
            integer, intent(in)                                   :: nvirt

            integer :: k

            work = rho
            call evd(work, mocoeff(:, nocc+1))
            !
            ! Eigenvectors are ordered according to increasing eigenvalues.
            ! The eigenvalues of the density matrix are either 1.0 (2.0) or
            ! 0.0. Those which equal zero belong to the virtual orbitals.
            !
            do k = 1, nvirt
                  mocoeff(:, nocc+k) = work(:, k)
            end do
      end subroutine calcvirt


      subroutine dipolevec(dip, dipx, dipy, dipz, rho)
            ! ------------------------------------------------------------------
            ! Calculate three components of dipole moment vector.
            ! ------------------------------------------------------------------
            ! DIP   - Output, dipole moment vector (x, y, z components)
            ! DIPX, - Matrices of X, Y, Z components of electronic dipole moment
            ! DIPY,   operator. (AO basis, upper triangle not referenced)
            ! DIPZ 
            ! RHO   - Density matrix (AO basis, both triangles referenced)
            !
            real(F64), dimension(3), intent(out)   :: dip
            real(F64), dimension(:, :), intent(in) :: dipx, dipy, dipz
            real(F64), dimension(:, :), intent(in) :: rho
            !
            ! Calculate electronic contribution to dipole moment
            !
            call rhotrace(dip(1), rho, dipx)
            call rhotrace(dip(2), rho, dipy)
            call rhotrace(dip(3), rho, dipz)
            !
            ! Sum electronic and nuclear contributions
            !
            dip(1) = NUCLDIP(1) - dip(1)
            dip(2) = NUCLDIP(2) - dip(2)
            dip(3) = NUCLDIP(3) - dip(3)
      end subroutine dipolevec


      subroutine quadrupolevec(quad, quadxx, quadyy, quadzz, quadyx, quadzx, quadzy, rho)
            ! --------------------------------------------------------------------------------
            ! Calculate components of quadrupole moment vector.
            ! --------------------------------------------------------------------------------
            ! QUAD    - Output, quadrupole moment vector (xx, yy, zz, yx, zx, zy components)
            ! QUADXX, - Matrices of X, Y, Z components of electronic quadrupole moment
            ! QUADYY,   operator. (AO basis, upper triangle not referenced) It is assumed
            ! QUADZZ    that these integrals do not contain the minus sign of the electronic
            ! QUADYX    charge.
            ! QUADZX 
            ! QUADZY 
            ! RHO      - Density matrix (AO basis, both triangles referenced)
            !
            real(F64), dimension(6), intent(out)   :: quad
            real(F64), dimension(:, :), intent(in) :: quadxx, quadyy, quadzz
            real(F64), dimension(:, :), intent(in) :: quadyx, quadzx, quadzy
            real(F64), dimension(:, :), intent(in) :: rho
            !
            ! Calculate electronic contribution to quadrupole moment
            !
            call rhotrace(quad(1), rho, quadxx)
            call rhotrace(quad(2), rho, quadyy)
            call rhotrace(quad(3), rho, quadzz)
            call rhotrace(quad(4), rho, quadyx)
            call rhotrace(quad(5), rho, quadzx)
            call rhotrace(quad(6), rho, quadzy)
            !
            ! Sum electronic and nuclear contributions
            !
            quad(1) = NUCLQUAD(1) - quad(1)
            quad(2) = NUCLQUAD(2) - quad(2)
            quad(3) = NUCLQUAD(3) - quad(3)
            quad(4) = NUCLQUAD(4) - quad(4)
            quad(5) = NUCLQUAD(5) - quad(5)
            quad(6) = NUCLQUAD(6) - quad(6)
      end subroutine quadrupolevec

      subroutine quadrupolevecm(quad, quadxx, quadyy, quadzz, quadyx, quadzx, quadzy, rho)
            ! --------------------------------------------------------------------------------
            ! Calculate components of quadrupole moment vector.
            ! --------------------------------------------------------------------------------
            ! QUAD    - Output, quadrupole moment vector (xx, yy, zz, yx, zx, zy components)
            ! QUADXX, - Matrices of X, Y, Z components of electronic quadrupole moment
            ! QUADYY,   operator. (AO basis, upper triangle not referenced) It is assumed
            ! QUADZZ    that these integrals do not contain the minus sign of the electronic
            ! QUADYX    charge.
            ! QUADZX
            ! QUADZY
            ! RHO      - Density matrix (AO basis, both triangles referenced)
            !
            real(F64), dimension(6), intent(out)   :: quad
            real(F64), dimension(:, :), intent(in) :: quadxx, quadyy, quadzz
            real(F64), dimension(:, :), intent(in) :: quadyx, quadzx, quadzy
            real(F64), dimension(:, :), intent(in) :: rho
            !
            ! Calculate electronic contribution to quadrupole moment
            !
            call rhotrace(quad(1), rho, quadxx)
            call rhotrace(quad(2), rho, quadyy)
            call rhotrace(quad(3), rho, quadzz)
            call rhotrace(quad(4), rho, quadyx)
            call rhotrace(quad(5), rho, quadzx)
            call rhotrace(quad(6), rho, quadzy)
            !
            ! Sum electronic and nuclear contributions
            !
            quad(1) = NUCLQUADM(1) - quad(1)
            quad(2) = NUCLQUADM(2) - quad(2)
            quad(3) = NUCLQUADM(3) - quad(3)
            quad(4) = NUCLQUADM(4) - quad(4)
            quad(5) = NUCLQUADM(5) - quad(5)
            quad(6) = NUCLQUADM(6) - quad(6)
      end subroutine quadrupolevecm


      subroutine commutator(c, a, b)
            ! --------------------------------------------------------
            ! C <- [A, B] = AB - BA
            ! --------------------------------------------------------
            ! A - symmetric matrix, assumed that left triangle
            !     is stored
            ! B - input, assumed that full matrix is stored
            ! C - ouptut, full matrix is computed as an output
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: c
            real(F64), dimension(:, :), contiguous, intent(in)  :: a, b
            !
            ! 1. C <- AB (DSYMM)
            ! 2. C <- BA + 1.d+0 * C (DSYMM)
            !
            call symmwrap("L", "L", NORB, NORB, one, a, b, zero, c)
            call symmwrap("R", "L", NORB, NORB, -one, a, b, one, c)
      end subroutine commutator

      
      function homo(eorb, n)
            real(F64)                           :: homo
            real(F64), dimension(:), intent(in) :: eorb
            integer, intent(in)                        :: n
            !
            ! Closed shell system is assumed
            !
            homo = eorb(n)
      end function homo

      
      function lumo(eorb, n)
            real(F64)                           :: lumo
            real(F64), dimension(:), intent(in) :: eorb
            integer, intent(in)                        :: n
            !
            ! Closed shell system is assumed
            !
            lumo = eorb(n + 1)
      end function lumo


      subroutine uksderiv(deriv, ksmatrixa, ksmatrixb, rho_oao, work, nv)
            ! -----------------------------------------------------------------
            ! Generate the matrix of first derivatives of the Kohn-Sham energy
            ! with respect to total density matrix elements. High spin
            ! configuration is assumed
            ! -----------------------------------------------------------------
            ! DERIV     - Output, matrix of first derivatives: 
            !             DERIV_{pq} = d E / d P_{pq}. Both lower and upper
            !             triangles are computed. 
            ! KSMATRIXA - Input, matrix of first derivatives with respect to
            ! KSMATRIXB   spin density matrix elements:
            !             KSMATRIXA_{pq} = d E / d D^\alpha_{pq}
            !             KSMATRIXB_{pq} = d E / d D^\beta_{pq}
            !             (OAO basis, lower triangle referenced)
            ! RHO_OAO   - Total density matrix, RHO = D^\alpha + D^\beta.
            !             Both upper and lower triangle are referenced
            !             (OAO basis, both triangles referenced)
            ! WORK      - Array for temporary storage    
            !
            ! NV        - Fractional occupation of the open shell orbitals
            !             (valence shell orbitals.) It is assumed that 
            !             for a single open shell, each orbital has the same
            !             occupation. NV values should satisfy 0 < NV < 2.
            !             Note that NV is the occupation number of spatial 
            !             orbitals and should not be confused with
            !             fractional occupation of spin-orbitals.
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: deriv
            real(F64), dimension(:, :), contiguous, intent(in)  :: ksmatrixa
            real(F64), dimension(:, :), contiguous, intent(in)  :: ksmatrixb
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho_oao
            real(F64), dimension(:, :), contiguous, intent(out) :: work
            real(F64), intent(in)                   :: nv

            real(F64) :: a, b, c
            !
            ! If there is no fractional occupation, i.e. NV==1:
            ! a = 3/2, b = -1/2, c = -1/2, d = 1/2.
            !
            a = (four - nv) / (four - two * nv)
            b = -one / (four - two * nv)
            c = -nv / (four - two * nv)

            deriv = ksmatrixa
            work = ksmatrixa - ksmatrixb
            !
            ! 1. DERIV <- A * KSMATRIXA + C * KSMATRIXB
            ! 2. DERIV <- DERIV + B * [(K^\alpha - K^\beta), RHO_OAO]_+
            !    [A , B]_+ denotes anticommutator
            !
            call weightedsum(deriv, ksmatrixb, a, c)
            call anticomm(deriv, work, rho_oao, b)
            call smfill(deriv)
      end subroutine uksderiv
end module scfutils
