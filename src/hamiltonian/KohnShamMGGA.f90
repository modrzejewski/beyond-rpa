module KohnShamMGGA
      use math_constants
      use arithmetic

      implicit none

      integer, parameter :: KSMGGA_DELTAK = 5

contains

      subroutine ksmgga_XOrb(x, y, z, orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
            AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, ShellMomentum, NPrimitives, &
            CntrCoeffs, Exponents, NormFactors, AtomShellMap, AtomShellN, MaxAtomL, R2Max)
            
            real(F64), intent(in)                  :: x, y, z
            real(F64), dimension(:), intent(out)   :: orbval
            integer, dimension(:), intent(out)     :: shellidx
            integer, intent(out)                   :: n0
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, dimension(:, 0:), intent(in)  :: CartPolyX, CartPolyY, CartPolyZ
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, intent(in)                    :: NAtoms
            integer, intent(in)                    :: LmaxGTO
            integer, intent(in)                    :: MaxNPrimitives
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:), intent(in)      :: NPrimitives
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            real(F64), dimension(:, :), intent(in) :: NormFactors
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, dimension(:), intent(in)      :: MaxAtomL
            real(F64), dimension(:), intent(in)    :: R2Max

            real(F64), dimension(0:max(LmaxGTO+2, 4)) :: xl, ym, zn
            real(F64), dimension(0:max(LmaxGTO,2)) :: g0x, g1x
            real(F64), dimension(0:max(LmaxGTO,2)) :: g0y, g1y
            real(F64), dimension(0:max(LmaxGTO,2)) :: g0z, g1z
            real(F64), dimension(0:max(LmaxGTO,2)) :: h0x, h1x, h2x
            real(F64), dimension(0:max(LmaxGTO,2)) :: h0y, h1y, h2y
            real(F64), dimension(0:max(LmaxGTO,2)) :: h0z, h1z, h2z
            real(F64), dimension(MaxNPrimitives) :: exptable
            real(F64) :: dl
            real(F64) :: alpha, alpha2
            real(F64) :: rsq, nrm, t
            real(F64) :: sum_0, sum_x, sum_y, sum_z
            real(F64) :: sum_xx, sum_yy, sum_zz
            integer :: i, k, l, m, n, s
            integer :: u, v, w
            integer :: momentum, np, nints
            integer :: a

            xl(0)  = ONE
            ym(0)  = ONE
            zn(0)  = ONE

            g0x(0) = ZERO
            g0y(0) = ZERO
            g0z(0) = ZERO

            g0x(1) = ONE
            g0y(1) = ONE
            g0z(1) = ONE

            h0x(0) = ZERO
            h0y(0) = ZERO
            h0z(0) = ZERO

            h0x(1) = ZERO
            h0y(1) = ZERO
            h0z(1) = ZERO

            h0x(2) = TWO
            h0y(2) = TWO
            h0z(2) = TWO

            h1x(0) = -TWO
            h1y(0) = -TWO
            h1z(0) = -TWO

            w = 1
            n0 = 1

            atompass: do i = 1, NAtoms
                  !
                  ! Generate polynomials
                  !
                  xl(1) = x - AtomCoords(1, i)
                  ym(1) = y - AtomCoords(2, i)
                  zn(1) = z - AtomCoords(3, i)

                  xl(2) = xl(1) * xl(1)
                  ym(2) = ym(1) * ym(1)
                  zn(2) = zn(1) * zn(1)

                  xl(3) = xl(2) * xl(1)
                  ym(3) = ym(2) * ym(1)
                  zn(3) = zn(2) * zn(1)

                  xl(4) = xl(3) * xl(1)
                  ym(4) = ym(3) * ym(1)
                  zn(4) = zn(3) * zn(1)
                  !
                  ! First derivative 
                  !
                  g0x(2) = TWO * xl(1)
                  g0y(2) = TWO * ym(1)
                  g0z(2) = TWO * zn(1)

                  g1x(0) = -TWO * xl(1)
                  g1y(0) = -TWO * ym(1)
                  g1z(0) = -TWO * zn(1)

                  g1x(1) = -TWO * xl(2)
                  g1y(1) = -TWO * ym(2)
                  g1z(1) = -TWO * zn(2)

                  g1x(2) = -TWO * xl(3)
                  g1y(2) = -TWO * ym(3)
                  g1z(2) = -TWO * zn(3)
                  !
                  ! Second derivative
                  !
                  h1x(1) = -SIX * xl(1)
                  h1y(1) = -SIX * ym(1)
                  h1z(1) = -SIX * zn(1)

                  h1x(2) = -TEN * xl(2)
                  h1y(2) = -TEN * ym(2)
                  h1z(2) = -TEN * zn(2)

                  h2x(0) = FOUR * xl(2)
                  h2y(0) = FOUR * ym(2)
                  h2z(0) = FOUR * zn(2)

                  h2x(1) = FOUR * xl(3)
                  h2y(1) = FOUR * ym(3)
                  h2z(1) = FOUR * zn(3)

                  h2x(2) = FOUR * xl(4)
                  h2y(2) = FOUR * ym(4)
                  h2z(2) = FOUR * zn(4)

                  do l = 3, MaxAtomL(i)
                        dl = real(l, F64)

                        xl(l + 2) = xl(l + 1) * xl(1)
                        ym(l + 2) = ym(l + 1) * ym(1)
                        zn(l + 2) = zn(l + 1) * zn(1)

                        g0x(l) = dl * xl(l - 1)
                        g0y(l) = dl * ym(l - 1)
                        g0z(l) = dl * zn(l - 1)

                        g1x(l) = -TWO * xl(l + 1)
                        g1y(l) = -TWO * ym(l + 1)
                        g1z(l) = -TWO * zn(l + 1)

                        t = dl * (dl - ONE)
                        h0x(l) = t * xl(l - 2)
                        h0y(l) = t * ym(l - 2)
                        h0z(l) = t * zn(l - 2)

                        t = -FOUR * dl - TWO
                        h1x(l) = t * xl(l)
                        h1y(l) = t * ym(l)
                        h1z(l) = t * zn(l)

                        h2x(l) = FOUR * xl(l + 2)
                        h2y(l) = FOUR * ym(l + 2)
                        h2z(l) = FOUR * zn(l + 2)
                  end do

                  rsq = xl(2) + ym(2) + zn(2)

                  do a = 1, AtomShellN(i)
                        shloop: do k = AtomShellMap(1, a, i), AtomShellMap(2, a, i)
                              s = ShellParamsIdx(k)
                              !
                              ! Check if the current shell gives any non-negligible
                              ! contribution. Note that the elements of SH are sorted in
                              ! decreasing order according to R2MAX for each atom, 
                              ! thus facilitating a safe loop break at this point.
                              !
                              if (rsq .gt. r2max(s)) then
                                    exit shloop
                              else
                                    shellidx(n0) = k
                                    n0 = n0 + 1
                              end if

                              momentum = ShellMomentum(s)
                              np = NPrimitives(s)
                              nints = NAngFunc(s)

                              do v = 1, np
                                    alpha = Exponents(v, s)
                                    exptable(v) = exp(-alpha * rsq)
                              end do
                              !
                              ! Sum over different angular functions
                              !
                              !
                              do v = 1, nints
                                    !
                                    ! Orbital value
                                    !
                                    sum_0 = ZERO
                                    !
                                    ! First derivative
                                    !
                                    sum_x = ZERO
                                    sum_y = ZERO
                                    sum_z = ZERO
                                    !
                                    ! Diagonal elements of Hessian
                                    !
                                    sum_xx = ZERO
                                    sum_yy = ZERO
                                    sum_zz = ZERO

                                    l = CartPolyX(v, momentum)
                                    m = CartPolyY(v, momentum)
                                    n = CartPolyZ(v, momentum)

                                    do u = 1, np
                                          nrm =  NormFactors(v, s) * CntrCoeffs(u, s)
                                          t = nrm * exptable(u)
                                          alpha = Exponents(u, s)
                                          alpha2 = alpha * alpha
                                          sum_0 = sum_0 + t
                                          !
                                          ! Gradient components
                                          !
                                          sum_x = sum_x + t * (g0x(l) + g1x(l) * alpha)
                                          sum_y = sum_y + t * (g0y(m) + g1y(m) * alpha)
                                          sum_z = sum_z + t * (g0z(n) + g1z(n) * alpha)
                                          !
                                          ! Diagonal elements of Hessian
                                          !
                                          sum_xx = sum_xx + t * (h0x(l) + h1x(l) * alpha + h2x(l) * alpha2)
                                          sum_yy = sum_yy + t * (h0y(m) + h1y(m) * alpha + h2y(m) * alpha2)
                                          sum_zz = sum_zz + t * (h0z(n) + h1z(n) * alpha + h2z(n) * alpha2)
                                    end do
                                    !
                                    ! Multiply calculated sum by Cartesian polynomial
                                    !
                                    sum_0 = sum_0 * xl(l) * ym(m) * zn(n)

                                    sum_x = sum_x * ym(m) * zn(n)
                                    sum_y = sum_y * xl(l) * zn(n)
                                    sum_z = sum_z * xl(l) * ym(m)

                                    sum_xx = sum_xx * ym(m) * zn(n)
                                    sum_yy = sum_yy * xl(l) * zn(n)
                                    sum_zz = sum_zz * xl(l) * ym(m)
                                    !
                                    ! Store calculated value
                                    !
                                    orbval(w) = sum_0
                                    w = w + 1
                                    orbval(w) = sum_x
                                    w = w + 1
                                    orbval(w) = sum_y
                                    w = w + 1
                                    orbval(w) = sum_z
                                    w = w + 1
                                    orbval(w) = sum_xx + sum_yy + sum_zz
                                    w = w + 1
                              end do
                        end do shloop
                  end do
            end do atompass
            !
            ! Number of non-negligible shells
            !
            n0 = n0 - 1
      end subroutine ksmgga_XOrb


      subroutine ksmgga_X(dmatrix, x, y, z, rho, grad, sigma, lapl, tau, &
            orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
            AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives,  ShellParamsIdx, &
            ShellMomentum, NPrimitives, CntrCoeffs, Exponents, NormFactors, &
            AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
            ! --------------------------------------------------
            ! Calculate electronic charge density and its
            ! gradient, Laplacian and kinetic energy density
            ! at point (X, Y, Z). Store atomic orbital values
            ! at this point.
            ! --------------------------------------------------
            !
            ! RHO     - Density matrix. Assumed that both lower
            !           and upper diagonal parts are stored
            !
            ! X, Y, Z - Coordinates of point at which requested
            !           variables are to be evaluated
            !
            ! RHO     - Output, value of density
            !
            ! GRAD    - Output, three components of gradient
            !
            ! SIGMA   - Output, GRAD(1)**2 + GRAD(2)**2 +
            !           GRAD(3)**2
            !
            ! LAPL    - Output, Laplacian of electronic density
            !
            ! TAU     - Output,
            !
            ! ORBVAL, - Array for storing temporary data.
            ! SHELLIDX  Let us designate number of orbitals
            !           centered at NCONTRIB nearest neighbours
            !           of center as NORB(NCONTRIB).
            !           DIM(ORBVAL) >= 4 * NORB(NCONTRIB)
            !           DIM(SHELLIDX) >= 1 * NORB(NCONTRIB)
            !
            ! N0      - Number of non-negligible shells
            !
            !
            real(F64), dimension(:, :), intent(in) :: dmatrix
            real(F64), intent(in)                  :: x, y, z
            real(F64), intent(out)                 :: rho
            real(F64), dimension(3), intent(out)   :: grad
            real(F64), intent(out)                 :: sigma
            real(F64), intent(out)                 :: lapl
            real(F64), intent(out)                 :: tau
            real(F64), dimension(:), intent(out)   :: orbval
            integer, dimension(:), intent(out)     :: shellidx
            integer, intent(out)                   :: n0
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, dimension(:, 0:), intent(in)  :: CartPolyX, CartPolyY, CartPolyZ
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, intent(in)                    :: NAtoms
            integer, intent(in)                    :: LmaxGTO
            integer, intent(in)                    :: MaxNPrimitives
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:), intent(in)      :: NPrimitives
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            real(F64), dimension(:, :), intent(in) :: NormFactors
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, dimension(:), intent(in)      :: MaxAtomL
            real(F64), dimension(:), intent(in)    :: R2Max
            integer, dimension(:), intent(in)      :: ShellLoc
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call ksmgga_XOrb(x, y, z, orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                  AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, ShellMomentum, NPrimitives, &
                  CntrCoeffs, Exponents, NormFactors, AtomShellMap, AtomShellN, MaxAtomL, R2Max)
            call ksmgga_RhoSigLapTau(rho, grad, sigma, lapl, tau, dmatrix, &
                  orbval, shellidx, n0, ShellParamsIdx, NAngFunc, ShellLoc)
      end subroutine ksmgga_X


      subroutine ksmgga_RhoSigLapTau(rho, grad, sigma, lapl, tau, dmatrix, &
            orbval, shellidx, n0, ShellParamsIdx, NAngFunc, ShellLoc)
            ! --------------------------------------------------
            ! Calculate electronic charge density and its
            ! gradient, Laplacian and kinetic energy density
            ! at a given point in space.
            ! --------------------------------------------------
            !
            ! RHO     - Density matrix. Assumed that both lower
            !           and upper diagonal parts are stored
            !
            ! RHO     - Output, value of density
            !
            ! GRAD    - Output, three components of gradient
            !
            ! SIGMA   - Output, GRAD(1)**2 + GRAD(2)**2 +
            !           GRAD(3)**2
            !
            ! LAPL    - Output, Laplacian of electronic density
            !
            ! TAU     - Output,
            !
            ! ORBVAL, - Array for storing temporary data.
            ! SHELLIDX  Let us designate number of orbitals
            !           centered at NCONTRIB nearest neighbours
            !           of center as NORB(NCONTRIB).
            !           DIM(ORBVAL) >= 4 * NORB(NCONTRIB)
            !           DIM(SHELLIDX) >= 1 * NORB(NCONTRIB)
            !
            ! N0      - Number of non-negligible shells
            !
            !
            real(F64), intent(out)                 :: rho
            real(F64), dimension(3), intent(out)   :: grad
            real(F64), intent(out)                 :: sigma
            real(F64), intent(out)                 :: lapl
            real(F64), intent(out)                 :: tau
            real(F64), dimension(:, :), intent(in) :: dmatrix
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, dimension(:), intent(in)      :: ShellLoc

            real(F64) :: sum_0, sum_x, sum_y, sum_z, sum_l
            real(F64) :: lapl1, lapl2, t
            real(F64) :: p_val, p_grdx, p_grdy, p_grdz, p_lapl
            real(F64) :: q_val, q_grdx, q_grdy, q_grdz, q_lapl
            integer :: uu, u, v, vv
            integer :: l, k
            integer :: p0, p1, q0, q1
            integer :: p, q
            integer, parameter :: deltak = KSMGGA_DELTAK

            rho  = ZERO
            grad = ZERO
            lapl1 = ZERO
            lapl2 = ZERO
            tau  = ZERO
            !
            ! Calculate orbital products
            ! Calculate gradients
            ! Calculate Laplacian
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  p0 = ShellLoc(u)
                  p1 = ShellLoc(u) + NAngFunc(ShellParamsIdx(u)) - 1
                  ploop: do p =  p0, p1
                        p_val  = orbval(k)
                        p_grdx = orbval(k + 1)
                        p_grdy = orbval(k + 2)
                        p_grdz = orbval(k + 3)
                        p_lapl = orbval(k + 4)

                        l = k
                        k = k + deltak
                        !
                        ! Diagonal element
                        !
                        t = (ONE/TWO) * dmatrix(p, p)
                        sum_0 = t * p_val
                        sum_x = t * p_grdx
                        sum_y = t * p_grdy
                        sum_z = t * p_grdz
                        sum_l = dmatrix(p, p) * p_lapl

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        q0 = p + 1
                        q1 = ShellLoc(u) + NAngFunc(ShellParamsIdx(u)) - 1
                        do q = q0, q1
                              q_val  = orbval(l)
                              q_grdx = orbval(l + 1)
                              q_grdy = orbval(l + 2)
                              q_grdz = orbval(l + 3)
                              q_lapl = orbval(l + 4)

                              l = l + deltak

                              sum_0 = sum_0 + dmatrix(q, p) * q_val
                              sum_x = sum_x + dmatrix(q, p) * q_grdx
                              sum_y = sum_y + dmatrix(q, p) * q_grdy
                              sum_z = sum_z + dmatrix(q, p) * q_grdz
                              sum_l = sum_l + dmatrix(q, p) * q_lapl
                        end do

                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              q0 = ShellLoc(v)
                              q1 = ShellLoc(v) + NAngFunc(ShellParamsIdx(v)) - 1
                              do q = q0, q1
                                    q_val  = orbval(l)
                                    q_grdx = orbval(l + 1)
                                    q_grdy = orbval(l + 2)
                                    q_grdz = orbval(l + 3)
                                    q_lapl = orbval(l + 4)

                                    l = l + deltak

                                    sum_0 = sum_0 + dmatrix(q, p) * q_val
                                    sum_x = sum_x + dmatrix(q, p) * q_grdx
                                    sum_y = sum_y + dmatrix(q, p) * q_grdy
                                    sum_z = sum_z + dmatrix(q, p) * q_grdz
                                    sum_l = sum_l + dmatrix(q, p) * q_lapl
                              end do
                        end do
                        rho     = rho + p_val * sum_0
                        grad(1) = grad(1) + p_val * sum_x + p_grdx * sum_0
                        grad(2) = grad(2) + p_val * sum_y + p_grdy * sum_0
                        grad(3) = grad(3) + p_val * sum_z + p_grdz * sum_0
                        lapl1   = lapl1 + p_val * sum_l
                        lapl2   = lapl2 + p_lapl * (sum_0 - (ONE/TWO) * dmatrix(p,p) * p_val)
                        tau     = tau + p_grdx * sum_x + p_grdy * sum_y + p_grdz * sum_z
                  end do ploop
            end do shloop1
            rho  = TWO * rho
            grad = TWO * grad
            sigma = grad(1)**2 + grad(2)**2 + grad(3)**2
            tau  = TWO * tau
            lapl = TWO * (lapl1 + lapl2 + tau)
      end subroutine ksmgga_RhoSigLapTau


      subroutine ksmgga_XU(dmatrixa, dmatrixb, x, y, z, rhoa, rhob, &
            grada, gradb, sigma_aa, sigma_bb, sigma_ab, lapla, laplb, taua, taub, &
            orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
            AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, &
            ShellMomentum, NPrimitives, CntrCoeffs, Exponents, NormFactors, &
            AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
            
            real(F64), dimension(:, :), intent(in) :: dmatrixa
            real(F64), dimension(:, :), intent(in) :: dmatrixb
            real(F64), intent(in)                  :: x, y, z
            real(F64), intent(out)                 :: rhoa
            real(F64), intent(out)                 :: rhob
            real(F64), dimension(3), intent(out)   :: grada
            real(F64), dimension(3), intent(out)   :: gradb
            real(F64), intent(out)                 :: sigma_aa
            real(F64), intent(out)                 :: sigma_bb
            real(F64), intent(out)                 :: sigma_ab
            real(F64), intent(out)                 :: lapla
            real(F64), intent(out)                 :: laplb
            real(F64), intent(out)                 :: taua
            real(F64), intent(out)                 :: taub
            real(F64), dimension(:), intent(out)   :: orbval
            integer, dimension(:), intent(out)     :: shellidx
            integer, intent(out)                   :: n0
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, dimension(:, 0:), intent(in)  :: CartPolyX, CartPolyY, CartPolyZ
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            integer, intent(in)                    :: NAtoms
            integer, intent(in)                    :: LmaxGTO
            integer, intent(in)                    :: MaxNPrimitives
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: ShellMomentum
            integer, dimension(:), intent(in)      :: NPrimitives
            real(F64), dimension(:, :), intent(in) :: CntrCoeffs
            real(F64), dimension(:, :), intent(in) :: Exponents
            real(F64), dimension(:, :), intent(in) :: NormFactors
            integer, dimension(:, :, :), intent(in):: AtomShellMap
            integer, dimension(:), intent(in)      :: AtomShellN
            integer, dimension(:), intent(in)      :: MaxAtomL
            real(F64), dimension(:), intent(in)    :: R2Max
            integer, dimension(:), intent(in)      :: ShellLoc

            real(F64) :: suma_0, suma_x, suma_y, suma_z, suma_l
            real(F64) :: sumb_0, sumb_x, sumb_y, sumb_z, sumb_l
            real(F64) :: lapla1, laplb1, lapla2, laplb2, ta, tb
            real(F64) :: p_val, p_grdx, p_grdy, p_grdz, p_lapl
            real(F64) :: q_val, q_grdx, q_grdy, q_grdz, q_lapl
            integer :: uu, u, v, vv
            integer :: l, k
            integer :: p, q
            integer :: p0, p1, q0, q1
            integer, parameter :: deltak = KSMGGA_DELTAK

            rhoa  = ZERO
            rhob  = ZERO
            grada = ZERO
            gradb = ZERO
            lapla1 = ZERO
            lapla2 = ZERO
            laplb1 = ZERO
            laplb2 = ZERO
            taua = ZERO
            taub = ZERO
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call ksmgga_XOrb(x, y, z, orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                  AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, ShellMomentum, NPrimitives, &
                  CntrCoeffs, Exponents, NormFactors, AtomShellMap, AtomShellN, MaxAtomL, R2Max)
            !
            ! Calculate orbital products
            ! Calculate gradients
            ! Calculate Laplacian
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  p0 = ShellLoc(u)
                  p1 = ShellLoc(u) + NAngFunc(ShellParamsIdx(u)) - 1
                  ploop: do p =  p0, p1
                        p_val  = orbval(k)
                        p_grdx = orbval(k + 1)
                        p_grdy = orbval(k + 2)
                        p_grdz = orbval(k + 3)
                        p_lapl = orbval(k + 4)

                        l = k
                        k = k + deltak
                        !
                        ! Diagonal element
                        !
                        ta = (ONE/TWO) * dmatrixa(p, p)
                        suma_0 = ta * p_val
                        suma_x = ta * p_grdx
                        suma_y = ta * p_grdy
                        suma_z = ta * p_grdz
                        suma_l = dmatrixa(p, p) * p_lapl

                        tb = (ONE/TWO) * dmatrixb(p, p)
                        sumb_0 = tb * p_val
                        sumb_x = tb * p_grdx
                        sumb_y = tb * p_grdy
                        sumb_z = tb * p_grdz
                        sumb_l = dmatrixb(p, p) * p_lapl

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        q0 = p + 1
                        q1 = ShellLoc(u) + NAngFunc(ShellParamsIdx(u)) - 1
                        do q = q0, q1
                              q_val  = orbval(l)
                              q_grdx = orbval(l + 1)
                              q_grdy = orbval(l + 2)
                              q_grdz = orbval(l + 3)
                              q_lapl = orbval(l + 4)

                              l = l + deltak

                              suma_0 = suma_0 + dmatrixa(q, p) * q_val
                              suma_x = suma_x + dmatrixa(q, p) * q_grdx
                              suma_y = suma_y + dmatrixa(q, p) * q_grdy
                              suma_z = suma_z + dmatrixa(q, p) * q_grdz
                              suma_l = suma_l + dmatrixa(q, p) * q_lapl

                              sumb_0 = sumb_0 + dmatrixb(q, p) * q_val
                              sumb_x = sumb_x + dmatrixb(q, p) * q_grdx
                              sumb_y = sumb_y + dmatrixb(q, p) * q_grdy
                              sumb_z = sumb_z + dmatrixb(q, p) * q_grdz
                              sumb_l = sumb_l + dmatrixb(q, p) * q_lapl
                        end do

                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              q0 = ShellLoc(v)
                              q1 = ShellLoc(v) + NAngFunc(ShellParamsIdx(v)) - 1
                              do q = q0, q1
                                    q_val  = orbval(l)
                                    q_grdx = orbval(l + 1)
                                    q_grdy = orbval(l + 2)
                                    q_grdz = orbval(l + 3)
                                    q_lapl = orbval(l + 4)

                                    l = l + deltak

                                    suma_0 = suma_0 + dmatrixa(q, p) * q_val
                                    suma_x = suma_x + dmatrixa(q, p) * q_grdx
                                    suma_y = suma_y + dmatrixa(q, p) * q_grdy
                                    suma_z = suma_z + dmatrixa(q, p) * q_grdz
                                    suma_l = suma_l + dmatrixa(q, p) * q_lapl

                                    sumb_0 = sumb_0 + dmatrixb(q, p) * q_val
                                    sumb_x = sumb_x + dmatrixb(q, p) * q_grdx
                                    sumb_y = sumb_y + dmatrixb(q, p) * q_grdy
                                    sumb_z = sumb_z + dmatrixb(q, p) * q_grdz
                                    sumb_l = sumb_l + dmatrixb(q, p) * q_lapl
                              end do
                        end do

                        rhoa     = rhoa + p_val * suma_0
                        rhob     = rhob + p_val * sumb_0

                        grada(1) = grada(1) + p_val * suma_x + p_grdx * suma_0
                        grada(2) = grada(2) + p_val * suma_y + p_grdy * suma_0
                        grada(3) = grada(3) + p_val * suma_z + p_grdz * suma_0

                        gradb(1) = gradb(1) + p_val * sumb_x + p_grdx * sumb_0
                        gradb(2) = gradb(2) + p_val * sumb_y + p_grdy * sumb_0
                        gradb(3) = gradb(3) + p_val * sumb_z + p_grdz * sumb_0

                        lapla1   = lapla1 + p_val * suma_l
                        lapla2   = lapla2 + p_lapl * (suma_0 - (ONE/TWO) * dmatrixa(p, p) * p_val)

                        laplb1   = laplb1 + p_val * sumb_l
                        laplb2   = laplb2 + p_lapl * (sumb_0 - (ONE/TWO) * dmatrixb(p, p) * p_val)

                        taua     = taua + p_grdx * suma_x + p_grdy * suma_y + p_grdz * suma_z
                        taub     = taub + p_grdx * sumb_x + p_grdy * sumb_y + p_grdz * sumb_z
                  end do ploop
            end do shloop1

            rhoa  = TWO * rhoa
            rhob  = TWO * rhob

            grada = TWO * grada
            gradb = TWO * gradb

            sigma_aa = grada(1)**2 + grada(2)**2 + grada(3)**2
            sigma_ab = grada(1) * gradb(1) + grada(2) * gradb(2) + grada(3) * gradb(3)
            sigma_bb = gradb(1)**2 + gradb(2)**2 + gradb(3)**2

            taua  = TWO * taua
            taub  = TWO * taub

            lapla = TWO * (lapla1 + lapla2 + taua)
            laplb = TWO * (laplb1 + laplb2 + taub)
      end subroutine ksmgga_XU


      subroutine ksmgga_Kernel(Vxc, Tv, Tu, Nv, Nu)
            real(F64), dimension(Nv, Nu), intent(inout) :: Vxc
            real(F64), dimension(5, Nv), intent(in)     :: Tv
            real(F64), dimension(5, Nu), intent(in)     :: Tu
            integer, intent(in)                         :: Nv
            integer, intent(in)                         :: Nu

            integer :: p, q, k
            real(F64) :: t
            
            do p = 1, Nu
                  do q = 1, Nv
                        t = ZERO
                        do k = 1, 5
                              t = t + Tv(k, q) * Tu(k, p)
                        end do
                        Vxc(q, p) = Vxc(q, p) + t
                  end do
            end do
      end subroutine ksmgga_Kernel

      
      subroutine ksmgga_Vxc(Vxc, exc,  nelectron, divergence, &
            kinint, laplint, shellidx, orbval, n0, rho, grad, lapl, tau, eps, &
            vrho, vsigma, vlapl, vtau, weight, deltak, NAngFunc, &
            ShellParamsIdx, ThreshRho, MaxNAngFunc, ShellPairLoc)

            real(F64), dimension(:), intent(inout)    :: Vxc
            real(F64), intent(inout)                  :: exc
            real(F64), intent(inout)                  :: nelectron
            real(F64), intent(inout)                  :: divergence
            real(F64), intent(inout)                  :: kinint
            real(F64), intent(inout)                  :: laplint
            integer, dimension(:), intent(in)         :: shellidx
            real(F64), dimension(:), intent(in)       :: orbval
            integer, intent(in)                       :: n0
            real(F64), intent(in)                     :: rho
            real(F64), dimension(3), intent(in)       :: grad
            real(F64), intent(in)                     :: lapl
            real(F64), intent(in)                     :: tau
            real(F64), intent(in)                     :: eps
            real(F64), intent(in)                     :: vrho
            real(F64), intent(in)                     :: vsigma
            real(F64), intent(in)                     :: vlapl
            real(F64), intent(in)                     :: vtau
            real(F64), intent(in)                     :: weight
            integer, intent(in)                       :: deltak
            integer, dimension(:), intent(in)         :: NAngFunc
            integer, dimension(:), intent(in)         :: ShellParamsIdx
            real(F64), intent(in)                     :: ThreshRho
            integer, intent(in)                       :: MaxNAngFunc
            integer, dimension(:, :), intent(in)      :: ShellPairLoc

            real(F64) :: vr, vs, vt, vl, vl1, vl2
            real(F64), dimension(3) :: g
            real(F64) :: p_0, p_x, p_y, p_z, p_l
            real(F64), dimension(5, MaxNAngFunc) :: Tu, Tv
            integer :: k, l
            integer :: u, uu, v, vv
            integer :: p, q
            integer :: z0, z1
            integer :: Nu, Nv
            !
            ! Return if electronic density is smaller than threshold
            !
            if ((rho .lt. ThreshRho) .or. (n0 .eq. 0)) return

            vr = vrho
            vs = vsigma
            vl = vlapl
            vt = vtau

            nelectron = nelectron + weight * rho
            divergence = divergence + weight * (grad(1) + grad(2) + grad(3))
            kinint = kinint + weight * tau
            laplint = laplint + weight * lapl
            !
            ! Update integral of the exchange-correlation energy
            !
            exc = exc + weight * eps * rho

            g = grad * TWO * vs * weight
            vr = vr * weight
            vl1 = vl * weight
            vl2 = (TWO * vl + vt) * weight
            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  Nu = NAngFunc(ShellParamsIdx(u))
                  do p = 1, Nu
                        p_0   = orbval(k+(p-1)*DeltaK)
                        p_x   = orbval(k+(p-1)*DeltaK + 1)
                        p_y   = orbval(k+(p-1)*DeltaK + 2)
                        p_z   = orbval(k+(p-1)*DeltaK + 3)
                        p_l   = orbval(k+(p-1)*DeltaK + 4)
                        Tu(1, p) = p_0 * vr + p_x * g(1) + p_y * g(2) + p_z * g(3) + p_l * vl1
                        Tu(2, p) = p_0 * g(1) + p_x * vl2
                        Tu(3, p) = p_0 * g(2) + p_y * vl2
                        Tu(4, p) = p_0 * g(3) + p_z * vl2
                        Tu(5, p) = p_0 * vl1
                  end do
                  l = k
                  k = k + Nu * DeltaK
                  do vv = uu, n0
                        v = shellidx(vv)
                        Nv = NAngFunc(ShellParamsIdx(v))
                        do q = 1, Nv
                              Tv(1, q) = orbval(l+(q-1)*DeltaK)
                              Tv(2, q) = orbval(l+(q-1)*DeltaK + 1)
                              Tv(3, q) = orbval(l+(q-1)*DeltaK + 2)
                              Tv(4, q) = orbval(l+(q-1)*DeltaK + 3)
                              Tv(5, q) = orbval(l+(q-1)*DeltaK + 4)
                        end do
                        l = l + Nv * DeltaK
                        z0 = ShellPairLoc(v, u)
                        z1 = ShellPairLoc(v, u) + Nu * Nv - 1
                        call ksmgga_Kernel(Vxc(z0:z1), Tv, Tu, Nv, Nu)
                  end do
            end do
      end subroutine ksmgga_Vxc


      subroutine ksmgga_UVxc(Vxc, exc,  &
            nelectron, divergence, kinint, laplint, shellidx, orbval, n0, &
            rhoa, rhob, grada, gradb, lapla, laplb, taua, taub, eps, &
            vrhoa, vrhob, vsigma_aa, vsigma_bb, vsigma_ab, vlapla, vlaplb, &
            vtaua, vtaub, weight, deltak, NAngFunc, &
            ShellParamsIdx, ThreshRho, MaxNAngFunc, ShellPairLoc)
            ! ----------------------------------------------------------------------
            ! Generate contribution to the exchange-correlation potential
            ! from a given single grid point. The GGA part of the matrix
            ! element is calculated accoring to Eq. 16 in [1]. Generalizing
            ! Pople's approach to meta-GGA functinonals, the exchange correlation
            ! energy is defined as
            ! E^{XC} = \int f(\rho_a, \rho_b, \sigma_{aa}, \sigma_{ab}, \sigma_{bb},
            !          lapl_a, lapl_b, \tau_a, tau_b)
            ! Partial derivatives passed as arguments to this subroutine are the
            ! partial derivatives of the above f function. EPS argument is the
            ! f function divided by total electronic density. Note that the f
            ! function is defined as an explicit function of \sigma_{ab} only,
            ! and not and explicit function of both \sigma_{ab} and \sigma_{ba}
            ! (which is consistent with Pople's paper). This fact is recognized
            ! in the definition of VSIGMA_AB variable.
            ! ----------------------------------------------------------------------
            ! 1. Pople, J.A., Gill, P.M.W., and Johnson, B.G., Kohn-Sham
            !    density-functinoal theory within a finite basis sets, 
            !    Chem. Phys. Lett., 199, 557(1992).
            !
            real(F64), dimension(:, :), intent(inout)    :: Vxc
            real(F64), intent(inout)                     :: exc
            real(F64), intent(inout)                     :: nelectron
            real(F64), intent(inout)                     :: divergence
            real(F64), intent(inout)                     :: kinint
            real(F64), intent(inout)                     :: laplint
            integer, dimension(:), intent(in)            :: shellidx
            real(F64), dimension(:), intent(in)          :: orbval
            integer, intent(in)                          :: n0
            real(F64), intent(in)                        :: rhoa
            real(F64), intent(in)                        :: rhob
            real(F64), dimension(3), intent(in)          :: grada
            real(F64), dimension(3), intent(in)          :: gradb
            real(F64), intent(in)                        :: lapla
            real(F64), intent(in)                        :: laplb
            real(F64), intent(in)                        :: taua
            real(F64), intent(in)                        :: taub
            real(F64), intent(in)                        :: eps
            real(F64), intent(in)                        :: vrhoa
            real(F64), intent(in)                        :: vrhob
            real(F64), intent(in)                        :: vsigma_aa
            real(F64), intent(in)                        :: vsigma_bb
            real(F64), intent(in)                        :: vsigma_ab
            real(F64), intent(in)                        :: vlapla
            real(F64), intent(in)                        :: vlaplb
            real(F64), intent(in)                        :: vtaua
            real(F64), intent(in)                        :: vtaub
            real(F64), intent(in)                        :: weight
            integer, intent(in)                          :: deltak
            integer, dimension(:), intent(in)            :: NAngFunc
            integer, dimension(:), intent(in)            :: ShellParamsIdx
            real(F64), intent(in)                        :: ThreshRho
            integer, intent(in)                          :: MaxNAngFunc
            integer, dimension(:, :), intent(in)         :: ShellPairLoc

            real(F64) :: rho
            real(F64) :: vra, vrb, vsaa, vsbb, vsab
            real(F64) :: vta, vtb, vla, vlb, vla1, vlb1, vla2, vlb2
            real(F64), dimension(3) :: ga, gb
            real(F64) :: p_0, p_x, p_y, p_z, p_l
            real(F64), dimension(5, MaxNAngFunc) :: Tua, Tub, Tv
            integer :: k, l
            integer :: u, uu, v, vv
            integer :: p, q
            integer :: z0, z1
            integer :: Nu, Nv

            rho = rhoa + rhob
            !
            ! Return if electronic density is smaller than threshold
            !
            if ((rho .lt. ThreshRho) .or. (n0 .eq. 0)) return

            vra  = vrhoa
            vrb  = vrhob
            vsaa = vsigma_aa
            vsab = vsigma_ab
            vsbb = vsigma_bb
            vla = vlapla
            vlb = vlaplb
            vta = vtaua
            vtb = vtaub
            !
            ! Update diagnostic integrals (NELECTRON should integrate
            ! to the number of electrons, divergence and laplint to zero,
            ! and kinint to the kinetic energy obtained from analytic
            ! formula)
            !
            nelectron = nelectron + weight * rho
            divergence = divergence + weight * (grada(1) + grada(2) + &
                  grada(3) + gradb(1) + gradb(2) + gradb(3))
            kinint = kinint + weight * (taua + taub)
            laplint = laplint + weight * (lapla + laplb)
            !
            ! Update integral of the exchange-correlation energy
            !
            exc = exc + weight * eps * rho

            vra = vra * weight
            vrb = vrb * weight
            vsaa = vsaa * TWO * weight
            vsab = vsab * weight
            vsbb = vsbb * TWO * weight

            ga = vsaa * grada + vsab * gradb
            gb = vsbb * gradb + vsab * grada

            vla1 = vla * weight
            vla2 = (TWO * vla + vta) * weight

            vlb1 = vlb * weight
            vlb2 = (TWO * vlb + vtb) * weight

            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  Nu = NAngFunc(ShellParamsIdx(u))
                  do p = 1, Nu
                        p_0   = orbval(k+(p-1)*DeltaK)
                        p_x   = orbval(k+(p-1)*DeltaK + 1)
                        p_y   = orbval(k+(p-1)*DeltaK + 2)
                        p_z   = orbval(k+(p-1)*DeltaK + 3)
                        p_l   = orbval(k+(p-1)*DeltaK + 4)

                        Tua(1, p) = p_0 * vra + p_x * ga(1) + p_y * ga(2) + p_z * ga(3) + p_l * vla1
                        Tua(2, p) = p_0 * ga(1) + p_x * vla2
                        Tua(3, p) = p_0 * ga(2) + p_y * vla2
                        Tua(4, p) = p_0 * ga(3) + p_z * vla2
                        Tua(5, p) = p_0 * vla1

                        Tub(1, p) = p_0 * vrb + p_x * gb(1) + p_y * gb(2) + p_z * gb(3) + p_l * vlb1
                        Tub(2, p) = p_0 * gb(1) + p_x * vlb2
                        Tub(3, p) = p_0 * gb(2) + p_y * vlb2
                        Tub(4, p) = p_0 * gb(3) + p_z * vlb2
                        Tub(5, p) = p_0 * vlb1
                  end do
                  l = k
                  k = k + Nu * DeltaK
                  do vv = uu, n0
                        v = shellidx(vv)
                        Nv = NAngFunc(ShellParamsIdx(v))
                        do q = 1, Nv
                              Tv(1, q) = orbval(l+(q-1)*DeltaK)
                              Tv(2, q) = orbval(l+(q-1)*DeltaK + 1)
                              Tv(3, q) = orbval(l+(q-1)*DeltaK + 2)
                              Tv(4, q) = orbval(l+(q-1)*DeltaK + 3)
                              Tv(5, q) = orbval(l+(q-1)*DeltaK + 4)
                        end do
                        l = l + Nv * DeltaK
                        z0 = ShellPairLoc(v, u)
                        z1 = ShellPairLoc(v, u) + Nu * Nv - 1
                        call ksmgga_Kernel(Vxc(z0:z1, 1), Tv, Tua, Nv, Nu)
                        call ksmgga_Kernel(Vxc(z0:z1, 2), Tv, Tub, Nv, Nu)
                  end do
            end do
      end subroutine ksmgga_UVxc
end module KohnShamMGGA
