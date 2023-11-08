! --------------------------------------------------------------------------
! ONE-ELECTRON INTEGRALS OF MULTIPOLE OPERATORS IN CARTESIAN GAUSSIAN
! BASIS SET
! --------------------------------------------------------------------------
! 1. Buckingham, A. D., Permanent and induced molecular moments
!    and long-range intermolecular forces. Adv. Chem. Phys., 107, 12 (1967)
! 2. Shortley, G., The Computation of Quadrupole and Magnetic-Dipole
!    Transition probabilities, Phys. Rev., 225, 57 (1940)
! 3. Helgaker, T., Jorgensen, P., Olsen, J., Section 9.5.3 in 
!    Molecular Electronic-Structure Theory, John Wiley and Sons 2000.
!
module multipole
      use math_constants
      use gparam
      use boys
      use hermite
      use gto
      
      implicit none
      !
      ! Representations of the quadrupole operator
      ! ---
      ! Primitive Cartesian:
      ! <a| Qxx |b> = <a| X**2 |b>
      ! <a| Qxy |b> = <a| X * Y |b>
      ! Note that "-" is not included in the definition of this integral.
      ! 
      integer, parameter :: QUAD_PRIMITIVE = 2**0
      !
      ! Traceless tensor. Buckingham's traceless quadrupole definition [1]
      ! <a| Qxx |b> = 1/2 <a| 3 * X**2 - (X**2+Y**2+Z**2)|b>
      ! <a| Qxy |b> = 1/2 <a| 3 * X * Y |b>
      ! Note that the minus sign is not included in the definition of
      ! this integral.
      !
      integer, parameter :: QUAD_TRACELESS_BUCKINGHAM = 2**1
      !
      ! Traceless tensor. Shortley's traceless quadrupole definition [2]
      ! eq. (1)
      ! <a| Qxx |b> = <a| X**2 - 1/3*(X**2+Y**2+Z**2)|b>
      ! <a| Qxy |b> = <a| X * Y |b>
      ! Note that the minus sign is not included in the definition of
      ! this integral.
      !
      integer, parameter :: QUAD_TRACELESS_SHORTLEY = 2**2

contains
      
      subroutine ints_dipole(shell1, a, shell2, b, dipx, dipy, dipz, rc)
            ! ----------------------------------------------------------
            ! Calculate one-electron integrals of the electric dipole
            ! operator, <a| X |b>, <a| Y |b>, and <a| Z |b>. McMurchie
            ! Davidson algorithm is employed to compute one-electron
            ! integrals in Cartesian Gaussian basis set. Note that
            ! the minus sign is not included in the dipole operator.
            ! Dipole operator is computed with respect to the origin
            ! of a reference frame provided as an argument. In general,
            ! the dipole moment can depend on the origin if it is not
            ! the first non-vanishing electric moment of the molecule.
            ! ----------------------------------------------------------
            ! 1. T. Helgaker, Molecular Electronic-Structure Theory,
            !    9.5.43
            ! ----------------------------------------------------------
            ! SHELL1,    - Indices of orbitals' shells
            ! SHELL2
            !
            ! A, B       - Indices of atoms
            !
            ! DIPX,      - X, Y, Z components of dipole moments
            ! DIPY,
            ! DIPZ
            !
            ! RC         - The origin of a frame of reference
            !
            integer, intent(in)                           :: shell1, shell2
            integer, intent(in)                           :: a, b
            double precision, dimension(:), intent(inout) :: dipx, dipy, dipz
            double precision, dimension(3), intent(in)    :: rc

            double precision :: alphai, betaj, gamma
            double precision, dimension(3) :: ara, brb, rp, rpa, rpb, rpc
            double precision :: xabsq, yabsq, zabsq
            double precision :: alpha_reduced

            ! Maximum value of a single upper index of E^{ij}_t
            integer, parameter :: max_index = 2 * max_l

            ! Dimension = \sum_{k = 0}^{max_index} (k + 1)^2

            double precision, dimension((2 * max_index**3 + 9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx, eijy, eijz
            double precision :: eij0, eij1, ekl0, ekl1, emn0, emn1
            double precision :: ex2, ey2, ez2
            double precision :: dx, dy, dz
            integer :: i, j, v1, v2, v
            integer :: la, lb, ma, mb, na, nb
            integer :: pos
            double precision :: const
            integer :: momentum1, momentum2, momentum
            double precision :: norm1, norm2, norm
            integer :: nints1, nints2, nints
 
            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum = momentum1 + momentum2

            associate ( &
                  lla => ll(:, momentum1), &
                  mma => mm(:, momentum1), &
                  nna => nn(:, momentum1), &
                  llb => ll(:, momentum2), &
                  mmb => mm(:, momentum2), &
                  nnb => nn(:, momentum2), &
                  ra => atomr(:, a), &
                  rb => atomr(:, b))

            xabsq = (ra(1) - rb(1))**2
            yabsq = (ra(2) - rb(2))**2
            zabsq = (ra(3) - rb(3))**2

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints = nints1 * nints2

            dipx(1:nints) = zero
            dipy(1:nints) = zero
            dipz(1:nints) = zero

            do i = 1, nprm(shell1)

                  alphai = expn(i, shell1)
                  ara = alphai * ra
                  
                  do j = 1, nprm(shell2)
                        betaj = expn(j, shell2)
                        brb = betaj * rb
                        gamma = alphai + betaj
                        alpha_reduced = alphai * betaj / gamma
 
                        const = exp(-alpha_reduced * (xabsq + yabsq + zabsq)) * pi32 / sqrt(gamma**3)

                        rp   = (ara + brb) / gamma
                        rpa  = rp - ra
                        rpb  = rp - rb
                        rpc  = rp - rc
                        !
                        ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                        ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook 
                        ! because the coeffs are linear functions of the seed so
                        ! it may be incorporated into const1.
                        !    
                        call eijmatrix(momentum, one, gamma, rpa(1), rpb(1), eijx)
                        call eijmatrix(momentum, one, gamma, rpa(2), rpb(2), eijy)
                        call eijmatrix(momentum, one, gamma, rpa(3), rpb(3), eijz)

                        ex2 = eijx(2)
                        ey2 = eijy(2)
                        ez2 = eijz(2)
                     
                        v = 1
                        do v1 = 1, nints1

                              la = lla(v1)
                              ma = mma(v1)
                              na = nna(v1)

                              norm1 = const * nrml(v1, i, shell1)

                              do v2 = 1, nints2
                              
                                    lb = llb(v2)
                                    mb = mmb(v2)
                                    nb = nnb(v2)

                                    norm2 = nrml(v2, j, shell2)

                                    pos     = eijmatrix_position(la + lb, lb, 0)
                                    eij0    = eijx(pos)
                                    eijx(2) = zero
                                    eij1    = eijx(pos + 1)
                                    eijx(2) = ex2

                                    pos     = eijmatrix_position(ma + mb, mb, 0)
                                    ekl0    = eijy(pos)
                                    eijy(2) = zero
                                    ekl1    = eijy(pos + 1)
                                    eijy(2) = ey2

                                    pos     = eijmatrix_position(na + nb, nb, 0)
                                    emn0    = eijz(pos)
                                    eijz(2) = zero
                                    emn1    = eijz(pos + 1)
                                    eijz(2) = ez2

                                    norm = norm1 * norm2

                                    dx = norm * (eij1 + rpc(1) * eij0) * ekl0 * emn0
                                    dy = norm * eij0 * (ekl1 + rpc(2) * ekl0) * emn0
                                    dz = norm * eij0 * ekl0 * (emn1 + rpc(3) * emn0)

                                    dipx(v) = dipx(v) + dx
                                    dipy(v) = dipy(v) + dy
                                    dipz(v) = dipz(v) + dz

                                    v = v + 1
                              end do
                        end do
                  end do
            end do
      end associate
      end subroutine ints_dipole


      subroutine storeints(a, b, i, j, m, n)
            double precision, dimension(:, :), intent(out) :: a
            double precision, dimension(:), intent(in) :: b
            integer, intent(in) :: i, j, m, n

            integer :: k, l
            integer :: v

            v = 1
            do l = 0, n - 1
                  do k = 0, m - 1
                        a(i + k, j + l) = a(i + k, j + l) + b(v)
                        v = v + 1
                  end do
            end do
      end subroutine storeints


      subroutine dipole_shellloop(dipx, dipy, dipz, ka, kb, rc)
            double precision, dimension(:, :), intent(inout) :: dipx, dipy, dipz
            integer, intent(in)                              :: ka, kb
            double precision, dimension(3), intent(in)       :: rc

            double precision, dimension(max_nfunc**2) :: dx, dy, dz
            integer :: shell1, shell2
            integer :: momentum1, momentum2
            integer :: nint1, nint2
            integer :: la, lb
            integer :: idx1, idx2

            shella: do la = sh0(ka), sh0(ka + 1) - 1
                  shell1 = sh(la)
                  momentum1 = shtype(shell1)
                  nint1 = nfunc(momentum1)
                  idx1 = shpos(la)
                  lb = sh0(kb)
                  shellb: do while ((lb .lt. sh0(kb + 1)) .and. (lb .le. la))
                        shell2 = sh(lb)
                        momentum2 = shtype(shell2)
                        nint2 = nfunc(momentum2)
                        idx2 = shpos(lb)
                        !
                        ! Note order of arguments passed to one-electron integrals
                        ! subroutine. It is transposed due to the way STV_STOREINTS 
                        ! works.
                        !
                        call ints_dipole(shell2, kb, shell1, ka, dx, dy, dz, rc)

                        call storeints(dipx, dx, idx1, idx2, nint1, nint2)
                        call storeints(dipy, dy, idx1, idx2, nint1, nint2)
                        call storeints(dipz, dz, idx1, idx2, nint1, nint2)

                        lb = lb + 1
                  end do shellb
            end do shella
      end subroutine dipole_shellloop


      subroutine dipole(dipx, dipy, dipz, rc)
            ! ---------------------------------------------------------------------
            ! Calculate one-electron integrals of the electric dipole operator,
            ! <a| X |b>, <a| Y |b>, and <a| Z |b>. The minus sign is not
            ! included. The dipole operator is computed with respect to the origin
            ! provided as the RC argument to this subroutine. Note that dipole
            ! moment depends on the origin if it is not the first non-vanishing
            ! electric moment of the molecule.
            ! ---------------------------------------------------------------------
            ! DIPX, DIPY, DIPZ - Output, matrices containing <a| P |b>
            !                    integrals. Only lower triangle of each
            !                    of the matrices contains meaningful numbers.
            ! RC               - The origin of the frame of reference with
            !                    respect to which the dipole operator is
            !                    evaluated.
            !
            double precision, dimension(:, :), intent(out) :: dipx, dipy, dipz
            double precision, dimension(3), intent(in)     :: rc

            integer :: ka, kb

            dipx = zero
            dipy = zero
            dipz = zero

            atoma: do ka = 1, natom
                  atomb: do kb = 1, ka
                        call dipole_shellloop(dipx, dipy, dipz, ka, kb, rc)
                  end do atomb
            end do atoma
      end subroutine dipole
      

      subroutine ints_quadrupole(shell1, a, shell2, b, dipxx, dipyy, dipzz,&
            dipyx, dipzx, dipzy, rc)

            integer, intent(in)                           :: shell1, shell2
            integer, intent(in)                           :: a, b
            double precision, dimension(:), intent(inout) :: dipxx, dipyy, dipzz
            double precision, dimension(:), intent(inout) :: dipyx, dipzx, dipzy
            double precision, dimension(3), intent(in)    :: rc

            double precision :: alphai, betaj, gamma
            double precision, dimension(3) :: ara, brb, rp, rpa, rpb, rpc
            double precision :: xabsq, yabsq, zabsq
            double precision :: alpha_reduced

            ! Maximum value of a single upper index of E^{ij}_t
            integer, parameter :: max_index = 2 * max_l

            ! Dimension = \sum_{k = 0}^{max_index} (k + 1)^2

            double precision, dimension((2 * max_index**3 + 9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx, eijy, eijz
            double precision :: eij0, eij1, eij2, ekl0, ekl1, ekl2, emn0, emn1, emn2
            double precision :: ex2, ey2, ez2
            double precision :: dxx, dyy, dzz, dyx, dzx, dzy
            integer :: i, j, v1, v2, v
            integer :: la, lb, ma, mb, na, nb
            integer :: pos
            double precision :: const
            integer :: momentum1, momentum2, momentum
            double precision :: norm1, norm2, norm
            integer :: nints1, nints2, nints
 
            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum = momentum1 + momentum2

            associate ( &
                  lla => ll(:, momentum1), &
                  mma => mm(:, momentum1), &
                  nna => nn(:, momentum1), &
                  llb => ll(:, momentum2), &
                  mmb => mm(:, momentum2), &
                  nnb => nn(:, momentum2), &
                  ra => atomr(:, a), &
                  rb => atomr(:, b))

            xabsq = (ra(1) - rb(1))**2
            yabsq = (ra(2) - rb(2))**2
            zabsq = (ra(3) - rb(3))**2

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints = nints1 * nints2

            dipyx = zero
            dipzx = zero
            dipzy = zero
            
            dipxx = zero
            dipyy = zero
            dipzz = zero


            do i = 1, nprm(shell1)
                  alphai = expn(i, shell1)
                  ara = alphai * ra
                  do j = 1, nprm(shell2)
                        betaj = expn(j, shell2)
                        brb = betaj * rb
                        gamma = alphai + betaj
                        alpha_reduced = alphai * betaj / gamma
 
                        const = exp(-alpha_reduced * (xabsq + yabsq + zabsq)) * pi32 / sqrt(gamma**3)

                        rp   = (ara + brb) / gamma
                        rpa  = rp - ra
                        rpb  = rp - rb
                        rpc  = rp - rc
                        !
                        ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                        ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook 
                        ! because the coeffs are linear functions of the seed so
                        ! it may be incorporated into const1.
                        !    
                        call eijmatrix(momentum, ONE, gamma, rpa(1), rpb(1), eijx)
                        call eijmatrix(momentum, ONE, gamma, rpa(2), rpb(2), eijy)
                        call eijmatrix(momentum, ONE, gamma, rpa(3), rpb(3), eijz)

                        ex2 = eijx(2)
                        ey2 = eijy(2)
                        ez2 = eijz(2)
                     
                        v = 1
                        do v1 = 1, nints1

                              la = lla(v1)
                              ma = mma(v1)
                              na = nna(v1)

                              norm1 = const * nrml(v1, i, shell1)

                              do v2 = 1, nints2
                              
                                    lb = llb(v2)
                                    mb = mmb(v2)
                                    nb = nnb(v2)

                                    norm2 = nrml(v2, j, shell2)

                                    pos     = eijmatrix_position(la + lb, lb, 0)
                                    eij0    = eijx(pos)
                                    eijx(2) = ZERO
                                    eij1    = eijx(pos + 1)
                                    eijx(2) = ex2
                                    if((la + lb).ge.2)then 
                                          eij2 = eijx(pos + 2) 
                                    else
                                          eij2 = ZERO  
                                    end if

                                    pos     = eijmatrix_position(ma + mb, mb, 0)
                                    ekl0    = eijy(pos)
                                    eijy(2) = ZERO
                                    ekl1    = eijy(pos + 1)
                                    eijy(2) = ey2
                                    if((ma + mb).ge.2)then 
                                          ekl2 = eijy(pos + 2) 
                                    else
                                          ekl2 = ZERO
                                    end if

                                    pos     = eijmatrix_position(na + nb, nb, 0)
                                    emn0    = eijz(pos)
                                    eijz(2) = ZERO
                                    emn1    = eijz(pos + 1)
                                    eijz(2) = ez2
                                    if((na + nb).ge.2)then 
                                          emn2 = eijz(pos + 2) 
                                    else
                                          emn2 = ZERO
                                    end if

                                    norm = norm1 * norm2

                                    dyx = norm * (eij1 + rpc(1) * eij0) * (ekl1 + rpc(2) * ekl0) * emn0
                                    dzx = norm * (eij1 + rpc(1) * eij0) * ekl0 * (emn1 + rpc(3) * emn0) 
                                    dzy = norm * eij0 * (ekl1 + rpc(2) * ekl0) * (emn1 + rpc(3) * emn0)
                                    
                                    dxx = norm * ekl0 * emn0 * &
                                          ((rpc(1)**2 + FRAC12 / gamma) * eij0 + &
                                          TWO * rpc(1) * eij1 + TWO * eij2)

                                    dyy = norm * eij0 * emn0 * &
                                          ((rpc(2)**2 + FRAC12 / gamma) * ekl0 + &
                                          TWO * rpc(2) * ekl1 + TWO * ekl2)

                                    dzz = norm * eij0 * ekl0 * &
                                          ((rpc(3)**2 + FRAC12 / gamma) * emn0 + &
                                          TWO * rpc(3) * emn1 + TWO * emn2)
                                    
                                    dipyx(v) = dipyx(v) + dyx
                                    dipzx(v) = dipzx(v) + dzx
                                    dipzy(v) = dipzy(v) + dzy
                                    
                                    dipxx(v) = dipxx(v) + dxx
                                    dipyy(v) = dipyy(v) + dyy
                                    dipzz(v) = dipzz(v) + dzz

                                    v = v + 1
                              end do
                        end do
                  end do
            end do
      end associate
      end subroutine ints_quadrupole


      subroutine quadrupole_shellloop(dipxx, dipyy, dipzz, dipyx, dipzx, dipzy, ka, kb, rc)
            double precision, dimension(:, :), intent(inout) :: dipxx, dipyy, dipzz
            double precision, dimension(:, :), intent(inout) :: dipyx, dipzx, dipzy
            integer, intent(in)                              :: ka, kb
            double precision, dimension(3), intent(in)       :: rc

            double precision, dimension(max_nfunc**2) :: dxx, dyy, dzz, dyx, dzx, dzy
            integer :: shell1, shell2
            integer :: momentum1, momentum2
            integer :: nint1, nint2
            integer :: la, lb
            integer :: idx1, idx2

            shella: do la = sh0(ka), sh0(ka + 1) - 1
                  shell1 = sh(la)
                  momentum1 = shtype(shell1)
                  nint1 = nfunc(momentum1)
                  idx1 = shpos(la)
                  lb = sh0(kb)
                  shellb: do while ((lb .lt. sh0(kb + 1)) .and. (lb .le. la))
                        shell2 = sh(lb)
                        momentum2 = shtype(shell2)
                        nint2 = nfunc(momentum2)
                        idx2 = shpos(lb)
                        !
                        ! Note order of arguments passed to one-electron integrals
                        ! subroutine. 
                        !
                        call ints_quadrupole(shell2, kb, shell1, ka, dxx, dyy, dzz,&
                              dyx, dzx, dzy, rc)

                        call storeints(dipxx, dxx, idx1, idx2, nint1, nint2)
                        call storeints(dipyy, dyy, idx1, idx2, nint1, nint2)
                        call storeints(dipzz, dzz, idx1, idx2, nint1, nint2)

                        call storeints(dipyx, dyx, idx1, idx2, nint1, nint2)
                        call storeints(dipzx, dzx, idx1, idx2, nint1, nint2)
                        call storeints(dipzy, dzy, idx1, idx2, nint1, nint2)

                        lb = lb + 1
                  end do shellb
            end do shella
      end subroutine quadrupole_shellloop


      subroutine quadrupole(quadxx, quadyy, quadzz, quadyx, quadzx, quadzy, rc, quad_def)
            ! -----------------------------------------------------------------------
            ! Compute one-electron integrals of the quadrupole operator in Cartesian
            ! Gaussian basis:
            ! QUADPQ(a, b) = <a|Qpq|b>.
            ! One of several definitions of the quadrupole matrix can be requested:
            ! primitive Cartesian, Buckingham's traceless tensor, and Shortley's
            ! traceless tensor. The minus sign of the electronic charge is not
            ! included in the integrals regardless of the requested definition.
            ! -----------------------------------------------------------------------
            ! QUADXX, QUADYY, QUADZZ - Output, matrices containing one-electron
            ! QUADYX, QUADZX, QUADZY   integrals of the quadrupole operator:
            !                       QUADPQ = <a|Qpq|b>. The definition of Qpq
            !                       depends on the value of the QUAD_DEF argument.
            !                       Only lower triangle of each of the matrices
            !                       contains meaningful numbers.
            !
            ! RC                  - The origin of a frame of reference. Note that
            !                       only first non-vanishing moment of charge
            !                       distribution does not depend on the origin.
            !                       
            ! QUAD_DEF            - The definition of Qpq operator. Allowed values
            !                       are: QUAD_PRIMITIVE, QUAD_TRACELESS_BUCKINGHAM,
            !                       and QUAD_TRACELESS_SHORTLEY. See declaration
            !                       of this constants for the detailed description
            !                       and the appropriate literature references.
            !
            double precision, dimension(:, :), intent(out) :: quadxx, quadyy, quadzz
            double precision, dimension(:, :), intent(out) :: quadyx, quadzx, quadzy
            double precision, dimension(3), intent(in)     :: rc
            integer, intent(in)                            :: quad_def

            integer :: ka, kb
            integer :: r, s
            double precision :: trace

            quadxx = ZERO
            quadyy = ZERO
            quadzz = ZERO

            quadyx = ZERO
            quadzx = ZERO
            quadzy = ZERO

            atoma: do ka = 1, natom
                  atomb: do kb = 1, ka
                        call quadrupole_shellloop(quadxx, quadyy, quadzz, quadyx, quadzx, quadzy, ka, kb, rc)
                  end do atomb
            end do atoma

            print*, 'quad_def', quad_def

            if (quad_def == QUAD_TRACELESS_BUCKINGHAM) then
                  print*, 'buck'
                  do s = 1, NORB
                        do r = s, NORB
                              trace = quadxx(r, s) + quadyy(r, s) + quadzz(r, s)
                              quadxx(r, s) = FRAC12 * (THREE * quadxx(r, s) - trace)
                              quadyy(r, s) = FRAC12 * (THREE * quadyy(r, s) - trace)
                              quadzz(r, s) = FRAC12 * (THREE * quadzz(r, s) - trace)
                              quadyx(r, s) = FRAC32 * quadyx(r, s)
                              quadzx(r, s) = FRAC32 * quadzx(r, s)
                              quadzy(r, s) = FRAC32 * quadzy(r, s)
                        end do
                  end do
            else if (quad_def == QUAD_TRACELESS_SHORTLEY) then
                  print*, 'short'
                  do s = 1, NORB
                        do r = s, NORB
                           trace = quadxx(r, s) + quadyy(r, s) + quadzz(r, s)
                           quadxx(r, s) = (quadxx(r, s) - FRAC13 * trace)
                           quadyy(r, s) = (quadyy(r, s) - FRAC13 * trace)
                           quadzz(r, s) = (quadzz(r, s) - FRAC13 * trace)
                        end do
                  end do
            end if
      end subroutine quadrupole


      subroutine ints_octupole(shell1, a, shell2, b, octxyz, octxxx, octyyy, octzzz, &
            octxxy, octxyy, octxxz, octxzz, octyyz, octyzz, rc)

            integer, intent(in)                           :: shell1, shell2
            integer, intent(in)                           :: a, b
            real(F64), dimension(:), intent(inout) :: octxyz, octxxx, octyyy, octzzz
            real(F64), dimension(:), intent(inout) :: octxxy, octxyy, octxxz, octxzz, octyyz, octyzz
            real(F64), dimension(3), intent(in)    :: rc

            real(F64) :: alphai, betaj, gamma
            real(F64), dimension(3) :: ara, brb, rp, rpa, rpb, rpc
            real(F64) :: xabsq, yabsq, zabsq
            real(F64) :: alpha_reduced

            ! Maximum value of a single upper index of E^{ij}_t
            integer, parameter :: max_index = 2 * max_l

            ! Dimension = \sum_{k = 0}^{max_index} (k + 1)^2

            real(F64), dimension((2 * max_index**3 + 9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx, eijy, eijz
            real(F64) :: eij0, eij1, eij2, eij3, eij4
            real(F64) :: ekl0, ekl1, ekl2, ekl3 
            real(F64) :: emn0, emn1, emn2, emn3
            real(F64) :: ex2, ey2, ez2
            real(F64) :: dxyz, dxxx, dyyy, dzzz
            real(F64) :: dxxy, dxyy, dxxz, dxzz, dyyz, dyzz
            integer :: i, j, v1, v2, v
            integer :: la, lb, ma, mb, na, nb
            integer :: pos
            real(F64) :: const
            integer :: momentum1, momentum2, momentum
            real(F64) :: norm1, norm2, norm
            integer :: nints1, nints2, nints
 
            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum = momentum1 + momentum2

            associate ( &
                  lla => ll(:, momentum1), &
                  mma => mm(:, momentum1), &
                  nna => nn(:, momentum1), &
                  llb => ll(:, momentum2), &
                  mmb => mm(:, momentum2), &
                  nnb => nn(:, momentum2), &
                  ra => atomr(:, a), &
                  rb => atomr(:, b))

            xabsq = (ra(1) - rb(1))**2
            yabsq = (ra(2) - rb(2))**2
            zabsq = (ra(3) - rb(3))**2

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints = nints1 * nints2


            octxyz = zero
            octxxx = zero
            octyyy = zero
            octzzz = zero
            
            octxxy = zero
            octxyy = zero
            
            octxxz = zero
            octxzz = zero

            octyyz = zero
            octyzz = zero

            do i = 1, nprm(shell1)
                  alphai = expn(i, shell1)
                  ara = alphai * ra
                  do j = 1, nprm(shell2)
                        betaj = expn(j, shell2)
                        brb = betaj * rb
                        gamma = alphai + betaj
                        alpha_reduced = alphai * betaj / gamma
 
                        const = exp(-alpha_reduced * (xabsq + yabsq + zabsq)) * pi32 / sqrt(gamma**3)

                        rp   = (ara + brb) / gamma
                        rpa  = rp - ra
                        rpb  = rp - rb
                        rpc  = rp - rc
                        !
                        ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                        ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook 
                        ! because the coeffs are linear functions of the seed so
                        ! it may be incorporated into const1.
                        !    
                        call eijmatrix(momentum, ONE, gamma, rpa(1), rpb(1), eijx)
                        call eijmatrix(momentum, ONE, gamma, rpa(2), rpb(2), eijy)
                        call eijmatrix(momentum, ONE, gamma, rpa(3), rpb(3), eijz)

                        ex2 = eijx(2)
                        ey2 = eijy(2)
                        ez2 = eijz(2)
                     
                        v = 1
                        do v1 = 1, nints1

                              la = lla(v1)
                              ma = mma(v1)
                              na = nna(v1)

                              norm1 = const * nrml(v1, i, shell1)

                              do v2 = 1, nints2
                              
                                    lb = llb(v2)
                                    mb = mmb(v2)
                                    nb = nnb(v2)

                                    norm2 = nrml(v2, j, shell2)

                                    pos     = eijmatrix_position(la + lb, lb, 0)
                                    eij0    = eijx(pos)
                                    eijx(2) = ZERO
                                    eij1    = eijx(pos + 1)
                                    eijx(2) = ex2
                                    if((la + lb).ge.2)then 
                                          eij2 = eijx(pos + 2) 
                                    else
                                          eij2 = ZERO  
                                    end if
                                    if((la+lb).ge.3)then
                                          eij3 = eijx(pos+3)
                                    else
                                          eij3 = ZERO
                                    end if
                                    if((la+lb).ge.4)then
                                          eij4 = eijx(pos+4)
                                    else
                                          eij4 = ZERO
                                    end if


                                    pos     = eijmatrix_position(ma + mb, mb, 0)
                                    ekl0    = eijy(pos)
                                    eijy(2) = ZERO
                                    ekl1    = eijy(pos + 1)
                                    eijy(2) = ey2
                                    if((ma + mb).ge.2)then 
                                          ekl2 = eijy(pos + 2) 
                                    else
                                          ekl2 = ZERO
                                    end if
                                    if((la+lb).ge.3)then
                                          ekl3 = eijy(pos+3)
                                    else
                                          ekl3 = ZERO
                                    end if


                                    pos     = eijmatrix_position(na + nb, nb, 0)
                                    emn0    = eijz(pos)
                                    eijz(2) = ZERO
                                    emn1    = eijz(pos + 1)
                                    eijz(2) = ez2
                                    if((na + nb).ge.2)then 
                                          emn2 = eijz(pos + 2) 
                                    else
                                          emn2 = ZERO
                                    end if
                                    if((la+lb).ge.3)then
                                          emn3 = eijz(pos+3)
                                    else
                                          emn3 = ZERO
                                    end if

                                    norm = norm1 * norm2


                                    

                                    dxyz = norm * (eij1 + rpc(1)*eij0) * (ekl1 + rpc(2)*ekl0) * (emn1 + rpc(3)*emn0) 

                                    ! dxxx = norm * ekl0 * emn0 * (eij0 * rpc(1) * (rpc(1)**2 + FRAC32 / gamma) + &
                                    !       three * eij1* (rpc(1)**2 + FRAC12 / gamma) + six * eij2 * rpc(1) + six * eij3)

                                    dxxx = norm * ekl0 * emn0 * (&
                                          eij0 * (rpc(1)**2 *(rpc(1)**2 + frac32 / gamma)  + &
                                          frac32 / gamma * (three * rpc(1)**2 + frac32 / gamma)) + &
                                          eij1 * rpc(1) * (rpc(1)**2 + frac32 / gamma + &
                                          three * rpc(1)**2 + frac32 / gamma + six / (two * gamma)) + &
                                          eij2 * two * (three * rpc(1)**2 + &
                                          frac32 / gamma + three * rpc(1)**2 + frac32 / gamma) + &
                                          eij3 * two * twelve * rpc(1) + &
                                          eij4 * two * twelve)

                                    dyyy = norm * eij0 * emn0 *(ekl0 * rpc(2) * (rpc(2)**2 + FRAC32 / gamma) +&
                                          three * ekl1* (rpc(2)**2 + FRAC12 / gamma) + six * ekl2 * rpc(2) + six * ekl3)

                                    dzzz = norm * eij0 * ekl0 *(emn0 * rpc(3) * (rpc(3)**2 + FRAC32 / gamma) +&
                                          three * emn1* (rpc(3)**2 + FRAC12 / gamma) + six * emn2 * rpc(3) + six * emn3)
                                    
                                    dxyy = norm * emn0 * (eij1 + rpc(1) * eij0) * &
                                          (ekl0 * (rpc(2)**2 + FRAC12) + two * ekl1 * rpc(2) + two * ekl2)

                                    dyyz = norm* eij0 * (emn1 + rpc(3) * emn0)* &
                                          (ekl0 * (rpc(2)**2 + FRAC12) + two * ekl1 * rpc(2) + two * ekl2)

                                    dxxy = norm* emn0 * (ekl1 + rpc(2) * ekl0)* &
                                          (eij0 * (rpc(1)**2 + FRAC12) + two * eij1 * rpc(1) + two * eij2)
                                    
                                    dyzz = norm* eij0 * (ekl1 + rpc(2) * ekl0)* &
                                          (emn0 * (rpc(3)**2 + FRAC12) + two * emn1 * rpc(3) + two * emn2)

                                    dxxz = norm* ekl0 * (emn1 + rpc(3) * emn0)* &
                                          (eij0 * (rpc(1)**2 + FRAC12) + two * eij1 * rpc(1) + two * eij2)

                                    dxzz = norm* ekl0 * (eij1 + rpc(1) * eij0)* &
                                          (emn0 * (rpc(3)**2 + FRAC12) + two * emn1 * rpc(3) + two * emn2)

                                     
                                    octxyz(v) = octxyz(v) + dxyz
                                    octxxx(v) = octxxx(v) + dxxx
                                    octyyy(v) = octyyy(v) + dyyy
                                    octzzz(v) = octzzz(v) + dzzz

                                    octxxy(v) = octxxy(v) + dxxy
                                    octxyy(v) = octxyy(v) + dxyy
                                    octxxz(v) = octxxz(v) + dxxz
                                    octxzz(v) = octxzz(v) + dxzz
                                    octyzz(v) = octyzz(v) + dyzz
                                    octyyz(v) = octyyz(v) + dyyz

                                    v = v + 1
                              end do
                        end do
                  end do
            end do
      end associate
      end subroutine ints_octupole


      subroutine octupole_shellloop(octxyz, octxxx, octyyy, octzzz, &
            octxxy, octxyy, octxxz, octxzz, octyyz, octyzz, ka, kb, rc)
            real(F64), dimension(:, :), intent(inout) :: octxyz, octxxx, octyyy, octzzz
            real(F64), dimension(:, :), intent(inout) :: octxxy, octxyy, octxzz, octxxz, octyzz, octyyz
            integer, intent(in)                       :: ka, kb
            real(F64), dimension(3), intent(in)       :: rc

            real(F64), dimension(max_nfunc**2) :: dxyz, dxxx, dyyy, dzzz, dxxy, dxyy, dxzz, dxxz, dyzz, dyyz
            integer :: shell1, shell2
            integer :: momentum1, momentum2
            integer :: nint1, nint2
            integer :: la, lb
            integer :: idx1, idx2

            shella: do la = sh0(ka), sh0(ka + 1) - 1
                  shell1 = sh(la)
                  momentum1 = shtype(shell1)
                  nint1 = nfunc(momentum1)
                  idx1 = shpos(la)
                  lb = sh0(kb)
                  shellb: do while ((lb .lt. sh0(kb + 1)) .and. (lb .le. la))
                        shell2 = sh(lb)
                        momentum2 = shtype(shell2)
                        nint2 = nfunc(momentum2)
                        idx2 = shpos(lb)
                        !
                        ! Note order of arguments passed to one-electron integrals
                        ! subroutine. 
                        !
                        call ints_octupole(shell2, kb, shell1, ka, dxyz, dxxx, dyyy, dzzz, &
                              dxxy, dxyy, dxxz, dxzz, dyyz, dyzz, rc)

                        call storeints(octxyz, dxyz, idx1, idx2, nint1, nint2)
                        call storeints(octxxx, dxxx, idx1, idx2, nint1, nint2)
                        call storeints(octyyy, dyyy, idx1, idx2, nint1, nint2)
                        call storeints(octzzz, dzzz, idx1, idx2, nint1, nint2)
                        
                        call storeints(octxxy, dxxy, idx1, idx2, nint1, nint2)
                        call storeints(octxyy, dxyy, idx1, idx2, nint1, nint2)
                        call storeints(octxxz, dxxz, idx1, idx2, nint1, nint2)
                        call storeints(octxzz, dxzz, idx1, idx2, nint1, nint2)
                        call storeints(octyzz, dyzz, idx1, idx2, nint1, nint2)
                        call storeints(octyyz, dyyz, idx1, idx2, nint1, nint2)

                        lb = lb + 1
                  end do shellb
            end do shella
      end subroutine octupole_shellloop


      subroutine octupole(octxyz, octxxx, octyyy, octzzz, &
            octxxy, octxyy, octxxz, octxzz, octyyz, octyzz, rc, quad_def)
            ! -----------------------------------------------------------------------
            ! Compute one-electron integrals of the octupole operator in Cartesian
            ! Gaussian basis:
            ! OCTPQ(a, b) = <a|Opq|b>.
            ! One of several definitions of the octupole matrix can be requested:
            ! primitive Cartesian, Buckingham's traceless tensor, and Shortley's
            ! traceless tensor. The minus sign of the electronic charge is not
            ! included in the integrals regardless of the requested definition.
            ! -----------------------------------------------------------------------
            ! OCTXYZ.... - Output, matrices containing one-electron
            !              integrals of the octupole operator:
            !              OCTPQ = <a|Opq|b>. The definition of Opq
            !              - always primitive.
            !              Only lower triangle of each of the matrices
            !              contains meaningful numbers.
            !
            ! RC         - The origin of a frame of reference. Note that
            !              only first non-vanishing moment of charge
            !              distribution does not depend on the origin.
            !                       
            ! QUAD_DEF            - The definition of Qpq operator. Allowed values
            !                       are: QUAD_PRIMITIVE. See declaration
            !                       of this constants for the detailed description
            !                       and the appropriate literature references.
            !
            real(F64), dimension(:, :), intent(out) :: octxyz, octxxx, octyyy, octzzz
            real(F64), dimension(:, :), intent(out) :: octxxy, octxyy, octxzz, octxxz, octyyz, octyzz
            real(F64), dimension(3), intent(in)     :: rc
            integer, intent(in)                     :: quad_def

            integer :: ka, kb

            octxyz = ZERO
            octxxx = ZERO
            octyyy = ZERO
            octzzz = ZERO

            octxxy = ZERO
            octxyy = ZERO

            octxxz = ZERO
            octxzz = ZERO

            octyyz = ZERO
            octyzz = ZERO

            atoma: do ka = 1, natom
                  atomb: do kb = 1, ka
                        call octupole_shellloop(octxyz, octxxx, octyyy, octzzz, &
                              octxxy, octxyy, octxxz, octxzz, octyyz, octyzz, &
                              ka, kb, rc)
                  end do atomb
            end do atoma

      end subroutine octupole

end module multipole
