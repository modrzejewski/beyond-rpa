module gga
      use math_constants
      use gparam
      use basis

      implicit none

      integer, parameter :: GGA_DELTAK = 4

contains

      pure subroutine gga_orbval(x, y, z, orbval, shellidx, n0)
            double precision, intent(in)                :: x, y, z
            double precision, dimension(:), intent(out) :: orbval
            integer, dimension(:), intent(out)          :: shellidx
            integer, intent(out)                        :: n0

            double precision, dimension(0:max(max_l+1, 3)) :: xl, ym, zn
            double precision, dimension(0:max_l) :: g0x, g1x
            double precision, dimension(0:max_l) :: g0y, g1y
            double precision, dimension(0:max_l) :: g0z, g1z
            double precision, dimension(max_nprm) :: exptable
            double precision :: dl
            double precision :: alpha
            double precision :: rsq, nrm, t
            double precision :: sum_0, sum_x, sum_y, sum_z
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

                  dl = three
                  do l = 3, atoml(i)
                        xl(l + 1) = xl(l) * xl(1)
                        ym(l + 1) = ym(l) * ym(1)
                        zn(l + 1) = zn(l) * zn(1)

                        g0x(l) = dl * xl(l - 1)
                        g0y(l) = dl * ym(l - 1)
                        g0z(l) = dl * zn(l - 1)

                        g1x(l) = -two * xl(l + 1)
                        g1y(l) = -two * ym(l + 1)
                        g1z(l) = -two * zn(l + 1)

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

                              l = ll(v, momentum)
                              m = mm(v, momentum)
                              n = nn(v, momentum)

                              do u = 1, np
                                    nrm = nrml(v, u, s)
                                    t = nrm * exptable(u)
                                    alpha = expn(u, s)
                                    sum_0 = sum_0 + t
                                    !
                                    ! Gradient components
                                    !
                                    sum_x = sum_x + t * (g0x(l) + g1x(l) * alpha)
                                    sum_y = sum_y + t * (g0y(m) + g1y(m) * alpha)
                                    sum_z = sum_z + t * (g0z(n) + g1z(n) * alpha)
                              end do
                              !
                              ! Multiply calculated sum by Cartesian polynomial
                              !
                              sum_0 = sum_0 * xl(l) * ym(m) * zn(n)

                              sum_x = sum_x * ym(m) * zn(n)
                              sum_y = sum_y * xl(l) * zn(n)
                              sum_z = sum_z * xl(l) * ym(m)
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
                        end do
                  end do shloop
            end do atompass
            !
            ! Number of non-negligible shells
            !
            n0 = n0 - 1            
      end subroutine gga_orbval


      pure subroutine gga_vars(dmatrix, x, y, z, rho, grad, sigma, orbval, &
            shellidx, n0)
            
            double precision, dimension(:, :), intent(in) :: dmatrix
            double precision, intent(in)                  :: x, y, z
            double precision, intent(out)                 :: rho
            double precision, dimension(3), intent(out)   :: grad
            double precision, intent(out)                 :: sigma
            double precision, dimension(:), intent(out)   :: orbval
            integer, dimension(:), intent(out)            :: shellidx
            integer, intent(out)                          :: n0

            double precision :: p_val, p_grdx, p_grdy, p_grdz
            double precision :: q_val, q_grdx, q_grdy, q_grdz
            double precision :: sum_0, sum_x, sum_y, sum_z
            double precision :: t
            integer :: k, l, p, q
            integer :: uu, u, v, vv
            integer, parameter :: deltak = GGA_DELTAK

            rho  = zero
            grad = zero
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call gga_orbval(x, y, z, orbval, shellidx, n0)
            !
            ! Calculate orbital products
            ! Calculate gradients
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
                        p_val  = orbval(k)
                        p_grdx = orbval(k + 1)
                        p_grdy = orbval(k + 2)
                        p_grdz = orbval(k + 3)

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

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
                              q_val  = orbval(l)
                              q_grdx = orbval(l + 1)
                              q_grdy = orbval(l + 2)
                              q_grdz = orbval(l + 3)

                              l = l + deltak

                              sum_0 = sum_0 + dmatrix(q, p) * q_val
                              sum_x = sum_x + dmatrix(q, p) * q_grdx
                              sum_y = sum_y + dmatrix(q, p) * q_grdy
                              sum_z = sum_z + dmatrix(q, p) * q_grdz
                        end do

                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_val  = orbval(l)
                                    q_grdx = orbval(l + 1)
                                    q_grdy = orbval(l + 2)
                                    q_grdz = orbval(l + 3)

                                    l = l + deltak

                                    sum_0 = sum_0 + dmatrix(q, p) * q_val
                                    sum_x = sum_x + dmatrix(q, p) * q_grdx
                                    sum_y = sum_y + dmatrix(q, p) * q_grdy
                                    sum_z = sum_z + dmatrix(q, p) * q_grdz
                              end do
                        end do

                        rho     = rho + p_val * sum_0
                        grad(1) = grad(1) + p_val * sum_x + p_grdx * sum_0
                        grad(2) = grad(2) + p_val * sum_y + p_grdy * sum_0
                        grad(3) = grad(3) + p_val * sum_z + p_grdz * sum_0
                  end do ploop
            end do shloop1

            rho  = two * rho
            grad = two * grad
            sigma = grad(1)**2 + grad(2)**2 + grad(3)**2
      end subroutine gga_vars


      pure subroutine ugga_vars(dmatrixa, dmatrixb, x, y, z, rhoa, rhob, &
            grada, gradb, sigma_aa, sigma_bb, sigma_ab, orbval, shellidx, &
            n0)
            
            double precision, dimension(:, :), intent(in) :: dmatrixa
            double precision, dimension(:, :), intent(in) :: dmatrixb
            double precision, intent(in)                  :: x, y, z
            double precision, intent(out)                 :: rhoa
            double precision, intent(out)                 :: rhob
            double precision, dimension(3), intent(out)   :: grada
            double precision, dimension(3), intent(out)   :: gradb
            double precision, intent(out)                 :: sigma_aa
            double precision, intent(out)                 :: sigma_bb
            double precision, intent(out)                 :: sigma_ab
            double precision, dimension(:), intent(out)   :: orbval
            integer, dimension(:), intent(out)            :: shellidx
            integer, intent(out)                          :: n0

            double precision :: p_val, p_grdx, p_grdy, p_grdz
            double precision :: q_val, q_grdx, q_grdy, q_grdz
            double precision :: suma_0, suma_x, suma_y, suma_z
            double precision :: sumb_0, sumb_x, sumb_y, sumb_z
            double precision :: ta, tb
            integer :: k, l, p, q
            integer :: uu, u, v, vv
            integer, parameter :: deltak = GGA_DELTAK

            rhoa  = zero
            rhob  = zero
            grada = zero
            gradb = zero
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call gga_orbval(x, y, z, orbval, shellidx, n0)
            !
            ! Calculate orbital products
            ! Calculate gradients
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
                        p_val  = orbval(k)
                        p_grdx = orbval(k + 1)
                        p_grdy = orbval(k + 2)
                        p_grdz = orbval(k + 3)

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

                        tb = frac12 * dmatrixb(p, p)
                        sumb_0 = tb * p_val
                        sumb_x = tb * p_grdx
                        sumb_y = tb * p_grdy
                        sumb_z = tb * p_grdz

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
                              q_val  = orbval(l)
                              q_grdx = orbval(l + 1)
                              q_grdy = orbval(l + 2)
                              q_grdz = orbval(l + 3)

                              l = l + deltak

                              suma_0 = suma_0 + dmatrixa(q, p) * q_val
                              suma_x = suma_x + dmatrixa(q, p) * q_grdx
                              suma_y = suma_y + dmatrixa(q, p) * q_grdy
                              suma_z = suma_z + dmatrixa(q, p) * q_grdz

                              sumb_0 = sumb_0 + dmatrixb(q, p) * q_val
                              sumb_x = sumb_x + dmatrixb(q, p) * q_grdx
                              sumb_y = sumb_y + dmatrixb(q, p) * q_grdy
                              sumb_z = sumb_z + dmatrixb(q, p) * q_grdz
                        end do

                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_val  = orbval(l)
                                    q_grdx = orbval(l + 1)
                                    q_grdy = orbval(l + 2)
                                    q_grdz = orbval(l + 3)

                                    l = l + deltak

                                    suma_0 = suma_0 + dmatrixa(q, p) * q_val
                                    suma_x = suma_x + dmatrixa(q, p) * q_grdx
                                    suma_y = suma_y + dmatrixa(q, p) * q_grdy
                                    suma_z = suma_z + dmatrixa(q, p) * q_grdz

                                    sumb_0 = sumb_0 + dmatrixb(q, p) * q_val
                                    sumb_x = sumb_x + dmatrixb(q, p) * q_grdx
                                    sumb_y = sumb_y + dmatrixb(q, p) * q_grdy
                                    sumb_z = sumb_z + dmatrixb(q, p) * q_grdz
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
                  end do ploop
            end do shloop1

            rhoa  = two * rhoa
            rhob  = two * rhob

            grada = two * grada
            gradb = two * gradb

            sigma_aa = grada(1)**2 + grada(2)**2 + grada(3)**2
            sigma_ab = grada(1) * gradb(1) + grada(2) * gradb(2) + grada(3) * gradb(3)
            sigma_bb = gradb(1)**2 + gradb(2)**2 + gradb(3)**2
      end subroutine ugga_vars


      pure subroutine gga_kscontrib(ksmatrix, exc, nelectron, &
            divergence, shellidx, orbval, n0, rho, grad, &
            eps, vrho, vsigma, weight, deltak)
            ! -------------------------------------------------------------
            ! Calculate pure exchange-correlation
            ! contributions to Kohn-Sham matrix.
            !
            ! E_{xc} = \int f(r) dr = \int \eps(r) \rho(r) dr
            ! F_{pq}^{XC} <- WEIGHT * (VRHO * \phi_p * \phi_q + 
            ! 2 * VSIGMA * GRAD \cdot \nabla (\phi_p \phi_q))
            !
            ! -------------------------------------------------------------
            ! 1. Pople, J.A., Gill, P.M.W., Johnson, B.G.
            !    Kohn-Sham density-functional theory
            !    within a finite basis set,
            !    Chem. Phys. Lett. 199, 557
            !
            double precision, dimension(:, :), intent(inout) :: ksmatrix
            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nelectron
            double precision, intent(inout)                  :: divergence
            integer, dimension(:), intent(in)                :: shellidx
            double precision, dimension(:), intent(in)       :: orbval
            integer, intent(in)                              :: n0
            double precision, intent(in)                     :: rho
            double precision, dimension(3), intent(in)       :: grad
            double precision, intent(in)                     :: eps
            double precision, intent(in)                     :: vrho
            double precision, intent(in)                     :: vsigma
            double precision, intent(in)                     :: weight
            integer, intent(in)                              :: deltak

            double precision, dimension(3) :: g
            double precision :: vr, vs, t
            double precision :: p_0, p_x, p_y, p_z
            double precision :: q_0, q_x, q_y, q_z
            double precision :: p_0gx, p_0gy, p_0gz
            integer :: k, l
            integer :: u, uu, v, vv
            integer :: p, q
            !
            ! Return if electronic density is smaller than threshold
            !
            if ((rho .lt. GRID_RHOTHRESH) .or. (n0 .eq. 0)) return

            vr = vrho
            vs = vsigma

            nelectron = nelectron + weight * rho
            divergence = divergence + weight * (grad(1) + grad(2) + grad(3))
            exc = exc + weight * eps * rho

            g = grad * two * vs * weight
            vr = vr * weight
            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  do p =  shpos(u), shpos(u + 1) - 1
                        p_0 = orbval(k)
                        p_x = orbval(k + 1)
                        p_y = orbval(k + 2)
                        p_z = orbval(k + 3)
                        
                        t = vr * p_0 + g(1) * p_x + &
                              g(2) * p_y + g(3) * p_z

                        p_0gx = p_0 * g(1)
                        p_0gy = p_0 * g(2)
                        p_0gz = p_0 * g(3)

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
                              l = l + deltak

                              ksmatrix(q, p) = ksmatrix(q, p) + &
                                    t * q_0 + p_0gx * q_x + &
                                    p_0gy * q_y + p_0gz * q_z
                        end do

                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_0 = orbval(l)
                                    q_x = orbval(l + 1)
                                    q_y = orbval(l + 2)
                                    q_z = orbval(l + 3)
                                    l = l + deltak

                                    ksmatrix(q, p) = ksmatrix(q, p) + &
                                          t * q_0 + p_0gx * q_x + &
                                          p_0gy * q_y + p_0gz * q_z
                              end do
                        end do
                  end do
            end do
      end subroutine gga_kscontrib


      pure subroutine ugga_kscontrib(ksmatrixa, ksmatrixb, exc, &
            nelectron, divergence, shellidx, orbval, n0, rhoa, rhob, &
            grada, gradb, eps, vrhoa, vrhob, vsigma_aa, &
            vsigma_bb, vsigma_ab, weight, deltak)
            ! ----------------------------------------------------------------
            ! Calculate pure exchange-correlation
            ! contributions to Kohn-Sham matrix.
            !
            ! E_{xc} = \int f(r) dr = \int \eps(r) \rho(r) dr
            ! F_{pq}^{XC\alpha} <- WEIGHT * (VRHOA * \phi_p * \phi_q + 
            ! (2 * VSIGMA_AA * GRADA + VSIGMA_AB * GRADB) \cdot
            ! \nabla (\phi_p \phi_q))
            !
            ! ----------------------------------------------------------------
            ! 1. Pople, J.A., Gill, P.M.W., Johnson, B.G.
            !    Kohn-Sham density-functional theory
            !    within a finite basis set,
            !    Chem. Phys. Lett. 199, 557
            !
            double precision, dimension(:, :), intent(inout) :: ksmatrixa
            double precision, dimension(:, :), intent(inout) :: ksmatrixb
            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nelectron
            double precision, intent(inout)                  :: divergence
            integer, dimension(:), intent(in)                :: shellidx
            double precision, dimension(:), intent(in)       :: orbval
            integer, intent(in)                              :: n0
            double precision, intent(in)                     :: rhoa
            double precision, intent(in)                     :: rhob
            double precision, dimension(3), intent(in)       :: grada
            double precision, dimension(3), intent(in)       :: gradb
            double precision, intent(in)                     :: eps
            double precision, intent(in)                     :: vrhoa
            double precision, intent(in)                     :: vrhob
            double precision, intent(in)                     :: vsigma_aa
            double precision, intent(in)                     :: vsigma_bb
            double precision, intent(in)                     :: vsigma_ab
            double precision, intent(in)                     :: weight
            integer, intent(in)                              :: deltak

            double precision :: rho
            double precision, dimension(3) :: ga, gb
            double precision :: vra, vsaa, vsab
            double precision :: vrb, vsbb
            double precision :: ta, tb
            double precision :: p_0, p_x, p_y, p_z
            double precision :: q_0, q_x, q_y, q_z
            double precision :: p_0gax, p_0gay, p_0gaz
            double precision :: p_0gbx, p_0gby, p_0gbz
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
            !
            ! Update numerical integrals (energy and diagnostics)
            !
            nelectron = nelectron + weight * rho
            divergence = divergence + weight * (grada(1) + grada(2) + &
                  grada(3) + gradb(1) + gradb(2) + gradb(3))
            exc = exc + weight * eps * rho

            vra = vra * weight
            vrb = vrb * weight
            vsaa = vsaa * two * weight
            vsab = vsab * weight
            vsbb = vsbb * two * weight
            
            ga = vsaa * grada + vsab * gradb
            gb = vsbb * gradb + vsab * grada

            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  do p =  shpos(u), shpos(u + 1) - 1
                        p_0 = orbval(k)
                        p_x = orbval(k + 1)
                        p_y = orbval(k + 2)
                        p_z = orbval(k + 3)

                        ta = vra * p_0 + ga(1) * p_x + ga(2) * p_y + ga(3) * p_z

                        p_0gax = p_0 * ga(1)
                        p_0gay = p_0 * ga(2)
                        p_0gaz = p_0 * ga(3)

                        tb = vrb * p_0 + gb(1) * p_x + gb(2) * p_y + gb(3) * p_z

                        p_0gbx = p_0 * gb(1)
                        p_0gby = p_0 * gb(2)
                        p_0gbz = p_0 * gb(3)

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
                              l = l + deltak

                              ksmatrixa(q, p) = ksmatrixa(q, p) + &
                                    ta * q_0 + p_0gax * q_x + &
                                    p_0gay * q_y + p_0gaz * q_z

                              ksmatrixb(q, p) = ksmatrixb(q, p) + &
                                    tb * q_0 + p_0gbx * q_x + &
                                    p_0gby * q_y + p_0gbz * q_z
                        end do

                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_0 = orbval(l)
                                    q_x = orbval(l + 1)
                                    q_y = orbval(l + 2)
                                    q_z = orbval(l + 3)
                                    l = l + deltak

                                    ksmatrixa(q, p) = ksmatrixa(q, p) + &
                                          ta * q_0 + p_0gax * q_x + &
                                          p_0gay * q_y + p_0gaz * q_z

                                    ksmatrixb(q, p) = ksmatrixb(q, p) + &
                                          tb * q_0 + p_0gbx * q_x + &
                                          p_0gby * q_y + p_0gbz * q_z
                              end do
                        end do
                  end do
            end do
      end subroutine ugga_kscontrib
end module gga
