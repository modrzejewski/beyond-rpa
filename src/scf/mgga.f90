module mgga
      use math_constants
      use gparam
      use basis
      use arithmetic
      use tiledmatrix

      implicit none

      integer, parameter :: MGGA_DELTAK = 5

contains

      pure subroutine mgga_orbval(x, y, z, orbval, shellidx, n0)
            real(F64), intent(in)                :: x, y, z
            real(F64), dimension(:), intent(out) :: orbval
            integer, dimension(:), intent(out)          :: shellidx
            integer, intent(out)                        :: n0

            real(F64), dimension(0:max(max_l+2, 4)) :: xl, ym, zn
            real(F64), dimension(0:max_l) :: g0x, g1x
            real(F64), dimension(0:max_l) :: g0y, g1y
            real(F64), dimension(0:max_l) :: g0z, g1z
            real(F64), dimension(0:max_l) :: h0x, h1x, h2x
            real(F64), dimension(0:max_l) :: h0y, h1y, h2y
            real(F64), dimension(0:max_l) :: h0z, h1z, h2z
            real(F64), dimension(max_nprm) :: exptable
            real(F64) :: dl
            real(F64) :: alpha, alpha2
            real(F64) :: rsq, nrm, t
            real(F64) :: sum_0, sum_x, sum_y, sum_z
            real(F64) :: sum_xx, sum_yy, sum_zz
            integer :: i, k, l, m, n, s
            integer :: u, v, w
            integer :: momentum, np, nints
            
            xl(0)  = one
            ym(0)  = one
            zn(0)  = one

            g0x(0) = zero
            g0y(0) = zero
            g0z(0) = zero

            g0x(1) = one
            g0y(1) = one
            g0z(1) = one

            h0x(0) = zero
            h0y(0) = zero
            h0z(0) = zero

            h0x(1) = zero
            h0y(1) = zero
            h0z(1) = zero

            h0x(2) = two
            h0y(2) = two
            h0z(2) = two

            h1x(0) = -two
            h1y(0) = -two
            h1z(0) = -two

            w = 1
            n0 = 1

            atompass: do i = 1, natom
                  !
                  ! Generate polynomials
                  !
                  xl(1) = x - atomr(1, i)
                  ym(1) = y - atomr(2, i)
                  zn(1) = z - atomr(3, i)

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
                  g0x(2) = two * xl(1)
                  g0y(2) = two * ym(1)
                  g0z(2) = two * zn(1)

                  g1x(0) = -two * xl(1)
                  g1y(0) = -two * ym(1)
                  g1z(0) = -two * zn(1)

                  g1x(1) = -two * xl(2)
                  g1y(1) = -two * ym(2)
                  g1z(1) = -two * zn(2)

                  g1x(2) = -two * xl(3)
                  g1y(2) = -two * ym(3)
                  g1z(2) = -two * zn(3)
                  !
                  ! Second derivative
                  !
                  h1x(1) = -six * xl(1)
                  h1y(1) = -six * ym(1)
                  h1z(1) = -six * zn(1)

                  h1x(2) = -ten * xl(2)
                  h1y(2) = -ten * ym(2)
                  h1z(2) = -ten * zn(2)

                  h2x(0) = four * xl(2)
                  h2y(0) = four * ym(2)
                  h2z(0) = four * zn(2)

                  h2x(1) = four * xl(3)
                  h2y(1) = four * ym(3)
                  h2z(1) = four * zn(3)

                  h2x(2) = four * xl(4)
                  h2y(2) = four * ym(4)
                  h2z(2) = four * zn(4)

                  dl = three
                  do l = 3, atoml(i)
                        xl(l + 2) = xl(l + 1) * xl(1)
                        ym(l + 2) = ym(l + 1) * ym(1)
                        zn(l + 2) = zn(l + 1) * zn(1)

                        g0x(l) = dl * xl(l - 1)
                        g0y(l) = dl * ym(l - 1)
                        g0z(l) = dl * zn(l - 1)

                        g1x(l) = -two * xl(l + 1)
                        g1y(l) = -two * ym(l + 1)
                        g1z(l) = -two * zn(l + 1)

                        t = dl * (dl - one)
                        h0x(l) = t * xl(l - 2)
                        h0y(l) = t * ym(l - 2)
                        h0z(l) = t * zn(l - 2)

                        t = -four * dl - two
                        h1x(l) = t * xl(l)
                        h1y(l) = t * ym(l)
                        h1z(l) = t * zn(l)

                        h2x(l) = four * xl(l + 2)
                        h2y(l) = four * ym(l + 2)
                        h2z(l) = four * zn(l + 2)

                        dl = dl + one
                  end do

                  rsq = xl(2) + ym(2) + zn(2)

                  shloop: do k = sh0(i), sh0(i + 1) - 1
                        s = sh(k)
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

                        momentum = shtype(s)
                        np = nprm(s)
                        nints = nfunc(momentum)

                        do v = 1, np
                              alpha = expn(v, s)
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
                              sum_0 = zero
                              !
                              ! First derivative
                              !
                              sum_x = zero
                              sum_y = zero
                              sum_z = zero
                              !
                              ! Diagonal elements of Hessian
                              !
                              sum_xx = zero
                              sum_yy = zero
                              sum_zz = zero

                              l = ll(v, momentum)
                              m = mm(v, momentum)
                              n = nn(v, momentum)

                              do u = 1, np
                                    nrm = nrml(v, u, s)
                                    t = nrm * exptable(u)
                                    alpha = expn(u, s)
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
            end do atompass
            !
            ! Number of non-negligible shells
            !
            n0 = n0 - 1
      end subroutine mgga_orbval


      pure subroutine mgga_vars_tile(dmatrix_tile, x, y, z, rho, grad, sigma, lapl, tau, &
            orbval, shellidx, n0)
            ! -----------------------------------------------------------------------
            ! Compute electronic charge density, its gradient, the Laplacian,
            ! and kinetic energy density at point R=(X, Y, Z). Compute AO values and
            ! derivatives at R.
            ! ------------------------------------------------------------------------
            !
            ! DMATRIX_TILE
            !           Density matrix in AO basis (tiled form)
            !
            ! X, Y, Z 
            !           Coordinates of the grid point
            !
            ! RHO     
            !           Electron density,
            !           \rho = \sum_{pq} D_{pq} \phi_p(r) \phi_q(r)
            !
            ! GRAD    
            !           Gradient of the density,
            !           \nabla \rho = \nabla \sum D_{pq} \phi_p(r) \phi_q(r)
            !
            ! SIGMA   
            !           Square of the gradient,
            !           \sigma = <\nabla\rho, \nabla\rho>
            !
            ! LAPL    
            !           The Laplacian of the electron density,
            !           \nabla^2 \rho = \nabla^2 \sum D_{pq} \phi_p(r) \phi_q(r)
            !
            ! TAU     - Density of the kinetic energy (without the 1/2 factor),
            !           \tau = \sum_{pq} D_{pq} \nabla\phi_p(r) \nabla\phi_q(r)
            !
            ! ORBVAL, SHELLIDX
            !
            !           Scratch arrays for stroring AO values, gradients, and
            !           second derivatives
            !
            ! N0      
            !           Number of orbital shells written to ORBVAL. N0 depends
            !           on the threshold GRID_AOTHRESH.
            !
            real(F64), dimension(:), intent(in)  :: dmatrix_tile
            real(F64), intent(in)                :: x, y, z
            real(F64), intent(out)               :: rho
            real(F64), dimension(3), intent(out) :: grad
            real(F64), intent(out)               :: sigma
            real(F64), intent(out)               :: lapl
            real(F64), intent(out)               :: tau
            real(F64), dimension(:), intent(out) :: orbval
            integer, dimension(:), intent(out)   :: shellidx
            integer, intent(out)                 :: n0
            
            real(F64) :: sum_0, sum_x, sum_y, sum_z, sum_l
            real(F64) :: p_val, p_grdx, p_grdy, p_grdz, p_lapl
            integer :: wp, wq
            integer :: k, l, p, q, qp
            integer :: p0, p1, q0, q1
            integer :: nints_u, nints_v
            integer :: uu, u, v, vv
            integer, parameter :: deltak = MGGA_DELTAK

            rho  = ZERO
            grad = ZERO
            lapl = ZERO
            tau  = ZERO
            !
            ! Calculate values of atomic orbitals
            !
            call mgga_orbval(x, y, z, orbval, shellidx, n0)
            !
            ! Calculate orbital products
            ! Calculate gradients
            ! Calculate Laplacian
            !
            k = 0
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  p0 = SHPOS(u)
                  p1 = SHPOS(u+1) - 1
                  nints_u = p1 - p0 + 1
                  !
                  ! Diagonal block
                  !
                  qp = tilepos(u, u)
                  wp = k + 1
                  do p = 1, nints_u
                        sum_0 = ZERO
                        sum_x = ZERO
                        sum_y = ZERO
                        sum_z = ZERO
                        sum_l = ZERO
                        wq = k + 1
                        do q = 1, nints_u
                              sum_0 = sum_0 + dmatrix_tile(qp) * orbval(wq)
                              sum_x = sum_x + dmatrix_tile(qp) * orbval(wq+1)
                              sum_y = sum_y + dmatrix_tile(qp) * orbval(wq+2)
                              sum_z = sum_z + dmatrix_tile(qp) * orbval(wq+3)
                              sum_l = sum_l + dmatrix_tile(qp) * orbval(wq+4)
                              
                              wq = wq + deltak
                              qp = qp + 1
                        end do
                        p_val = orbval(wp)
                        p_grdx = orbval(wp+1)
                        p_grdy = orbval(wp+2)
                        p_grdz = orbval(wp+3)
                        p_lapl = orbval(wp+4)

                        rho     = rho     + FRAC12 * sum_0 * p_val
                        grad(1) = grad(1) + FRAC12 * (p_val * sum_x + p_grdx * sum_0)
                        grad(2) = grad(2) + FRAC12 * (p_val * sum_y + p_grdy * sum_0)
                        grad(3) = grad(3) + FRAC12 * (p_val * sum_z + p_grdz * sum_0)
                        lapl    = lapl    + FRAC12 * (p_val * sum_l + p_lapl * sum_0)
                        tau     = tau     + FRAC12 * (p_grdx * sum_x + p_grdy * sum_y + p_grdz * sum_z)
                        
                        wp = wp + deltak
                  end do
                  !
                  ! Off-diagonal blocks
                  !
                  l = k + nints_u * deltak
                  shloop2: do vv = uu+1, n0
                        v = shellidx(vv)
                        q0 = SHPOS(v)
                        q1 = SHPOS(v+1) - 1
                        nints_v = q1 - q0 + 1
                        qp = tilepos(v, u)
                        wp = k + 1
                        do p = 1, nints_u
                              sum_0 = ZERO
                              sum_x = ZERO
                              sum_y = ZERO
                              sum_z = ZERO
                              sum_l = ZERO
                              wq = l + 1
                              do q = 1, nints_v
                                    sum_0 = sum_0 + dmatrix_tile(qp) * orbval(wq)
                                    sum_x = sum_x + dmatrix_tile(qp) * orbval(wq+1)
                                    sum_y = sum_y + dmatrix_tile(qp) * orbval(wq+2)
                                    sum_z = sum_z + dmatrix_tile(qp) * orbval(wq+3)
                                    sum_l = sum_l + dmatrix_tile(qp) * orbval(wq+4)

                                    wq = wq + deltak
                                    qp = qp + 1
                              end do
                              p_val = orbval(wp)
                              p_grdx = orbval(wp+1)
                              p_grdy = orbval(wp+2)
                              p_grdz = orbval(wp+3)
                              p_lapl = orbval(wp+4)

                              rho     = rho     + sum_0 * p_val
                              grad(1) = grad(1) + p_val * sum_x + p_grdx * sum_0
                              grad(2) = grad(2) + p_val * sum_y + p_grdy * sum_0
                              grad(3) = grad(3) + p_val * sum_z + p_grdz * sum_0
                              lapl    = lapl    + p_val * sum_l + p_lapl * sum_0
                              tau     = tau     + p_grdx * sum_x + p_grdy * sum_y + p_grdz * sum_z

                              wp = wp + deltak
                        end do
                        l = l + nints_v * deltak
                  end do shloop2
                  k = k + nints_u * deltak
            end do shloop1
            rho  = TWO * rho
            grad = TWO * grad
            sigma = grad(1)**2 + grad(2)**2 + grad(3)**2
            tau  = TWO * tau
            lapl = TWO * (lapl + tau)
      end subroutine mgga_vars_tile


      pure subroutine mgga_vars(dmatrix, x, y, z, rho, grad, sigma, lapl, tau, &
            orbval, shellidx, n0)
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
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call mgga_orbval(x, y, z, orbval, shellidx, n0)
            call mgga_RhoSigLapTau(rho, grad, sigma, lapl, tau, dmatrix, &
                  orbval, shellidx, n0)
      end subroutine mgga_vars


      pure subroutine mgga_RhoSigLapTau(rho, grad, sigma, lapl, tau, dmatrix, &
            orbval, shellidx, n0)
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

            real(F64) :: sum_0, sum_x, sum_y, sum_z, sum_l
            real(F64) :: lapl1, lapl2, t
            real(F64) :: p_val, p_grdx, p_grdy, p_grdz, p_lapl
            real(F64) :: q_val, q_grdx, q_grdy, q_grdz, q_lapl
            integer :: uu, u, v, vv
            integer :: l, k
            integer :: p, q
            integer, parameter :: deltak = MGGA_DELTAK

            rho  = zero
            grad = zero
            lapl1 = zero
            lapl2 = zero
            tau  = zero
            !
            ! Calculate orbital products
            ! Calculate gradients
            ! Calculate Laplacian
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
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
                        t = frac12 * dmatrix(p, p)
                        sum_0 = t * p_val
                        sum_x = t * p_grdx
                        sum_y = t * p_grdy
                        sum_z = t * p_grdz
                        sum_l = dmatrix(p, p) * p_lapl

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
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
                              do q = shpos(v), shpos(v + 1) - 1
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
                        lapl2   = lapl2 + p_lapl * (sum_0 - frac12 * dmatrix(p,p) * p_val)
                        tau     = tau + p_grdx * sum_x + p_grdy * sum_y + p_grdz * sum_z
                  end do ploop
            end do shloop1
            rho  = TWO * rho
            grad = TWO * grad
            sigma = grad(1)**2 + grad(2)**2 + grad(3)**2
            tau  = TWO * tau
            lapl = TWO * (lapl1 + lapl2 + tau)
      end subroutine mgga_RhoSigLapTau
      

      pure subroutine umgga_vars(dmatrixa, dmatrixb, x, y, z, rhoa, rhob, &
            grada, gradb, sigma_aa, sigma_bb, sigma_ab, lapla, laplb, taua, taub, &
            orbval, shellidx, n0)

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
            integer, dimension(:), intent(out)            :: shellidx
            integer, intent(out)                          :: n0

            real(F64) :: suma_0, suma_x, suma_y, suma_z, suma_l
            real(F64) :: sumb_0, sumb_x, sumb_y, sumb_z, sumb_l
            real(F64) :: lapla1, laplb1, lapla2, laplb2, ta, tb
            real(F64) :: p_val, p_grdx, p_grdy, p_grdz, p_lapl
            real(F64) :: q_val, q_grdx, q_grdy, q_grdz, q_lapl
            integer :: uu, u, v, vv
            integer :: l, k
            integer :: p, q
            integer, parameter :: deltak = MGGA_DELTAK

            rhoa  = zero
            rhob  = zero
            grada = zero
            gradb = zero
            lapla1 = zero
            lapla2 = zero
            laplb1 = zero
            laplb2 = zero
            taua = zero
            taub = zero
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call mgga_orbval(x, y, z, orbval, shellidx, n0)
            !
            ! Calculate orbital products
            ! Calculate gradients
            ! Calculate Laplacian
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
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
                        ta = frac12 * dmatrixa(p, p)
                        suma_0 = ta * p_val
                        suma_x = ta * p_grdx
                        suma_y = ta * p_grdy
                        suma_z = ta * p_grdz
                        suma_l = dmatrixa(p, p) * p_lapl

                        tb = frac12 * dmatrixb(p, p)
                        sumb_0 = tb * p_val
                        sumb_x = tb * p_grdx
                        sumb_y = tb * p_grdy
                        sumb_z = tb * p_grdz
                        sumb_l = dmatrixb(p, p) * p_lapl

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
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
                              do q = shpos(v), shpos(v + 1) - 1
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
                        lapla2   = lapla2 + p_lapl * (suma_0 - frac12 * dmatrixa(p, p) * p_val)

                        laplb1   = laplb1 + p_val * sumb_l
                        laplb2   = laplb2 + p_lapl * (sumb_0 - frac12 * dmatrixb(p, p) * p_val)

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
      end subroutine umgga_vars


      pure subroutine mgga_kscontrib(ksmatrix, exc,  nelectron, divergence, &
            kinint, laplint, shellidx, orbval, n0, rho, grad, lapl, tau, eps, &
            vrho, vsigma, vlapl, vtau, weight, deltak)
            
           real(F64), dimension(:, :), intent(inout) :: ksmatrix
           real(F64), intent(inout)                  :: exc
           real(F64), intent(inout)                  :: nelectron
           real(F64), intent(inout)                  :: divergence
           real(F64), intent(inout)                  :: kinint
           real(F64), intent(inout)                  :: laplint
           integer, dimension(:), intent(in)                :: shellidx
           real(F64), dimension(:), intent(in)       :: orbval
           integer, intent(in)                              :: n0
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
           integer, intent(in)                              :: deltak

           real(F64) :: vr, vs, vt, vl, vl1, vl2
           real(F64), dimension(3) :: g
           real(F64) :: p_0, p_x, p_y, p_z, p_l
           real(F64) :: q_0, q_x, q_y, q_z, q_l
           real(F64) :: r, sx, sy, sz, t
           integer :: k, l
           integer :: u, uu, v, vv
           integer :: p, q
           !
           ! Return if electronic density is smaller than threshold
           !
           if ((rho .lt. GRID_RHOTHRESH) .or. (n0 .eq. 0)) return

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
                 do p =  shpos(u), shpos(u + 1) - 1
                       p_0   = orbval(k)
                       p_x   = orbval(k + 1)
                       p_y   = orbval(k + 2)
                       p_z   = orbval(k + 3)
                       p_l   = orbval(k + 4)

                       r = p_0 * vl1
                       t = p_0 * vr + p_x * g(1) + p_y * g(2) + p_z * g(3) + p_l * vl1
                       sx = p_0 * g(1) + p_x * vl2
                       sy = p_0 * g(2) + p_y * vl2
                       sz = p_0 * g(3) + p_z * vl2

                       l = k
                       k = k + deltak
                       !
                       ! Diagonal block
                       !
                       do q = p, shpos(u + 1) - 1
                             q_0 = orbval(l)
                             q_x = orbval(l + 1)
                             q_y = orbval(l + 2)
                             q_z = orbval(l + 3)
                             q_l = orbval(l + 4)
                             l = l + deltak

                             ksmatrix(q, p) = ksmatrix(q, p) + &
                                   t * q_0 + sx * q_x + sy * q_y + sz * q_z + r * q_l
                       end do

                       do vv = uu + 1, n0
                             v = shellidx(vv)
                             do q = shpos(v), shpos(v + 1) - 1
                                   q_0 = orbval(l)
                                   q_x = orbval(l + 1)
                                   q_y = orbval(l + 2)
                                   q_z = orbval(l + 3)
                                   q_l = orbval(l + 4)
                                   l = l + deltak

                                   ksmatrix(q, p) = ksmatrix(q, p) + &
                                         t * q_0 + sx * q_x + sy * q_y + sz * q_z + r * q_l
                             end do
                       end do
                 end do
           end do
     end subroutine mgga_kscontrib


     pure subroutine umgga_kscontrib(ksmatrixa, ksmatrixb, exc,  &
           nelectron, divergence, kinint, laplint, shellidx, orbval, n0, &
           rhoa, rhob, grada, gradb, lapla, laplb, taua, taub, eps, &
           vrhoa, vrhob, vsigma_aa, vsigma_bb, vsigma_ab, vlapla, vlaplb, &
           vtaua, vtaub, weight, deltak)
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
           real(F64), dimension(:, :), intent(inout) :: ksmatrixa
           real(F64), dimension(:, :), intent(inout) :: ksmatrixb
           real(F64), intent(inout)                  :: exc
           real(F64), intent(inout)                  :: nelectron
           real(F64), intent(inout)                  :: divergence
           real(F64), intent(inout)                  :: kinint
           real(F64), intent(inout)                  :: laplint
           integer, dimension(:), intent(in)                :: shellidx
           real(F64), dimension(:), intent(in)       :: orbval
           integer, intent(in)                              :: n0
           real(F64), intent(in)                     :: rhoa
           real(F64), intent(in)                     :: rhob
           real(F64), dimension(3), intent(in)       :: grada
           real(F64), dimension(3), intent(in)       :: gradb
           real(F64), intent(in)                     :: lapla
           real(F64), intent(in)                     :: laplb
           real(F64), intent(in)                     :: taua
           real(F64), intent(in)                     :: taub
           real(F64), intent(in)                     :: eps
           real(F64), intent(in)                     :: vrhoa
           real(F64), intent(in)                     :: vrhob
           real(F64), intent(in)                     :: vsigma_aa
           real(F64), intent(in)                     :: vsigma_bb
           real(F64), intent(in)                     :: vsigma_ab
           real(F64), intent(in)                     :: vlapla
           real(F64), intent(in)                     :: vlaplb
           real(F64), intent(in)                     :: vtaua
           real(F64), intent(in)                     :: vtaub
           real(F64), intent(in)                     :: weight
           integer, intent(in)                       :: deltak

           real(F64) :: rho
           real(F64) :: vra, vrb, vsaa, vsbb, vsab
           real(F64) :: vta, vtb, vla, vlb, vla1, vlb1, vla2, vlb2
           real(F64), dimension(3) :: ga, gb
           real(F64) :: p_0, p_x, p_y, p_z, p_l
           real(F64) :: q_0, q_x, q_y, q_z, q_l
           real(F64) :: ra, sax, say, saz, ta
           real(F64) :: rb, sbx, sby, sbz, tb
           integer :: k, l
           integer :: u, uu, v, vv
           integer :: p, q

           rho = rhoa + rhob
           !
           ! Return if electronic density is smaller than threshold
           !
           if ((rho .lt. GRID_RHOTHRESH) .or. (n0 .eq. 0)) return
           
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
                 do p =  shpos(u), shpos(u + 1) - 1
                       p_0   = orbval(k)
                       p_x   = orbval(k + 1)
                       p_y   = orbval(k + 2)
                       p_z   = orbval(k + 3)
                       p_l   = orbval(k + 4)

                       ra = p_0 * vla1
                       ta = p_0 * vra + p_x * ga(1) + p_y * ga(2) + p_z * ga(3) + p_l * vla1
                       sax = p_0 * ga(1) + p_x * vla2
                       say = p_0 * ga(2) + p_y * vla2
                       saz = p_0 * ga(3) + p_z * vla2

                       rb = p_0 * vlb1
                       tb = p_0 * vrb + p_x * gb(1) + p_y * gb(2) + p_z * gb(3) + p_l * vlb1
                       sbx = p_0 * gb(1) + p_x * vlb2
                       sby = p_0 * gb(2) + p_y * vlb2
                       sbz = p_0 * gb(3) + p_z * vlb2

                       l = k
                       k = k + deltak
                       !
                       ! Diagonal block
                       !
                       do q = p, shpos(u + 1) - 1
                             q_0 = orbval(l)
                             q_x = orbval(l + 1)
                             q_y = orbval(l + 2)
                             q_z = orbval(l + 3)
                             q_l = orbval(l + 4)
                             l = l + deltak

                             ksmatrixa(q, p) = ksmatrixa(q, p) + &
                                   ta * q_0 + sax * q_x + say * q_y + saz * q_z + ra * q_l
                             ksmatrixb(q, p) = ksmatrixb(q, p) + &
                                   tb * q_0 + sbx * q_x + sby * q_y + sbz * q_z + rb * q_l
                       end do

                       do vv = uu + 1, n0
                             v = shellidx(vv)
                             do q = shpos(v), shpos(v + 1) - 1
                                   q_0 = orbval(l)
                                   q_x = orbval(l + 1)
                                   q_y = orbval(l + 2)
                                   q_z = orbval(l + 3)
                                   q_l = orbval(l + 4)
                                   l = l + deltak

                                   ksmatrixa(q, p) = ksmatrixa(q, p) + &
                                         ta * q_0 + sax * q_x + say * q_y + saz * q_z + ra * q_l
                                   ksmatrixb(q, p) = ksmatrixb(q, p) + &
                                         tb * q_0 + sbx * q_x + sby * q_y + sbz * q_z + rb * q_l
                             end do
                       end do
                 end do
           end do
     end subroutine umgga_kscontrib
end module mgga
