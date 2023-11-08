module KohnShamGGA
      use math_constants
      use arithmetic

      implicit none

      integer, parameter :: KSGGA_DELTAK = 4

contains

      subroutine ksgga_XOrb(x, y, z, orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
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

            real(F64), dimension(0:max(LmaxGTO+1, 3)) :: xl, ym, zn
            real(F64), dimension(0:max(LmaxGTO,2)) :: g0x, g1x
            real(F64), dimension(0:max(LmaxGTO,2)) :: g0y, g1y
            real(F64), dimension(0:max(LmaxGTO,2)) :: g0z, g1z
            real(F64), dimension(MaxNPrimitives) :: exptable
            real(F64) :: dl
            real(F64) :: alpha
            real(F64) :: rsq, nrm, t
            real(F64) :: sum_0, sum_x, sum_y, sum_z
            integer :: i, k, l, m, n, s
            integer :: u, v, w
            integer :: a
            integer :: momentum, np, nints
            
            xl(0)  = ONE
            ym(0)  = ONE
            zn(0)  = ONE

            g0x(0) = ZERO
            g0y(0) = ZERO
            g0z(0) = ZERO

            g0x(1) = ONE
            g0y(1) = ONE
            g0z(1) = ONE

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

                  do l = 3, MaxAtomL(i)
                        dl = real(l, F64)
                        
                        xl(l + 1) = xl(l) * xl(1)
                        ym(l + 1) = ym(l) * ym(1)
                        zn(l + 1) = zn(l) * zn(1)

                        g0x(l) = dl * xl(l - 1)
                        g0y(l) = dl * ym(l - 1)
                        g0z(l) = dl * zn(l - 1)

                        g1x(l) = -TWO * xl(l + 1)
                        g1y(l) = -TWO * ym(l + 1)
                        g1z(l) = -TWO * zn(l + 1)
                  end do

                  rsq = xl(2) + ym(2) + zn(2)

                  shellsegments: do a = 1, AtomShellN(i)
                        shloop: do k = AtomShellMap(1, a, i), AtomShellMap(2, a, i)
                              s = ShellParamsIdx(k)
                              !
                              ! Check if the current shell gives any non-negligible
                              ! contribution. Note that the elements of SH are sorted in
                              ! decreasing order according to R2MAX for each atom, 
                              ! thus facilitating a safe loop break at this point.
                              !
                              if (rsq .gt. R2Max(s)) then
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

                                    l = CartPolyX(v, momentum)
                                    m = CartPolyY(v, momentum)
                                    n = CartPolyZ(v, momentum)

                                    do u = 1, np
                                          nrm = NormFactors(v, s) * CntrCoeffs(u, s)
                                          t = nrm * exptable(u)
                                          alpha = Exponents(u, s)
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
                  end do shellsegments
            end do atompass
            !
            ! Number of non-negligible shells
            !
            n0 = n0 - 1
      end subroutine ksgga_XOrb


      subroutine ksgga_X(dmatrix, x, y, z, rho, grad, sigma, orbval, &
            shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
            AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, &
            ShellMomentum, NPrimitives, CntrCoeffs, Exponents, NormFactors, &
            AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
            
            real(F64), dimension(:, :), intent(in) :: dmatrix
            real(F64), intent(in)                  :: x, y, z
            real(F64), intent(out)                 :: rho
            real(F64), dimension(3), intent(out)   :: grad
            real(F64), intent(out)                 :: sigma
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

            real(F64) :: p_val, p_grdx, p_grdy, p_grdz
            real(F64) :: q_val, q_grdx, q_grdy, q_grdz
            real(F64) :: sum_0, sum_x, sum_y, sum_z
            real(F64) :: t
            integer :: k, l, p, q
            integer :: p0, p1, q0, q1
            integer :: uu, u, v, vv
            integer, parameter :: deltak = KSGGA_DELTAK

            rho  = ZERO
            grad = ZERO
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call ksgga_XOrb(x, y, z, orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                  AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, ShellMomentum, NPrimitives, &
                  CntrCoeffs, Exponents, NormFactors, AtomShellMap, AtomShellN, MaxAtomL, R2Max)
            !
            ! Calculate orbital products
            ! Calculate gradients
            !
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  p0 = ShellLoc(u)
                  p1 = ShellLoc(u) + NAngFunc(ShellParamsIdx(u)) - 1
                  ploop: do p = p0, p1
                        p_val  = orbval(k)
                        p_grdx = orbval(k + 1)
                        p_grdy = orbval(k + 2)
                        p_grdz = orbval(k + 3)

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

                              l = l + deltak

                              sum_0 = sum_0 + dmatrix(q, p) * q_val
                              sum_x = sum_x + dmatrix(q, p) * q_grdx
                              sum_y = sum_y + dmatrix(q, p) * q_grdy
                              sum_z = sum_z + dmatrix(q, p) * q_grdz
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

            rho  = TWO * rho
            grad = TWO * grad
            sigma = grad(1)**2 + grad(2)**2 + grad(3)**2
      end subroutine ksgga_X


      subroutine ksgga_XU(dmatrixa, dmatrixb, x, y, z, rhoa, rhob, &
            grada, gradb, sigma_aa, sigma_bb, sigma_ab, orbval, shellidx, &
            n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
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

            real(F64) :: p_val, p_grdx, p_grdy, p_grdz
            real(F64) :: q_val, q_grdx, q_grdy, q_grdz
            real(F64) :: suma_0, suma_x, suma_y, suma_z
            real(F64) :: sumb_0, sumb_x, sumb_y, sumb_z
            real(F64) :: ta, tb
            integer :: p0, p1, q0, q1
            integer :: k, l, p, q
            integer :: uu, u, v, vv
            integer, parameter :: deltak = KSGGA_DELTAK

            rhoa  = ZERO
            rhob  = ZERO
            grada = ZERO
            gradb = ZERO
            !
            ! Calculate orbital values and spatial
            ! derivatives
            !
            call ksgga_XOrb(x, y, z, orbval, shellidx, n0, NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                  AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, ShellParamsIdx, ShellMomentum, NPrimitives, &
                  CntrCoeffs, Exponents, NormFactors, AtomShellMap, AtomShellN, MaxAtomL, R2Max)
            !
            ! Calculate orbital products
            ! Calculate gradients
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

                        tb = (ONE/TWO) * dmatrixb(p, p)
                        sumb_0 = tb * p_val
                        sumb_x = tb * p_grdx
                        sumb_y = tb * p_grdy
                        sumb_z = tb * p_grdz

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
                              q0 = ShellLoc(v)
                              q1 = ShellLoc(v) + NAngFunc(ShellParamsIdx(v)) - 1
                              do q = q0, q1
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

            rhoa  = TWO * rhoa
            rhob  = TWO * rhob

            grada = TWO * grada
            gradb = TWO * gradb

            sigma_aa = grada(1)**2 + grada(2)**2 + grada(3)**2
            sigma_ab = grada(1) * gradb(1) + grada(2) * gradb(2) + grada(3) * gradb(3)
            sigma_bb = gradb(1)**2 + gradb(2)**2 + gradb(3)**2
      end subroutine ksgga_XU


      subroutine ksgga_Kernel(Vxc, Tv, Tu, Nv, Nu)
            real(F64), dimension(Nv, Nu), intent(inout) :: Vxc
            real(F64), dimension(4, Nv), intent(in)     :: Tv
            real(F64), dimension(4, Nu), intent(in)     :: Tu
            integer, intent(in)                         :: Nv
            integer, intent(in)                         :: Nu

            integer :: p, q, k
            real(F64) :: t
            
            do p = 1, Nu
                  do q = 1, Nv
                        t = ZERO
                        do k = 1, 4
                              t = t + Tv(k, q) * Tu(k, p)
                        end do
                        Vxc(q, p) = Vxc(q, p) + t
                  end do
            end do
      end subroutine ksgga_Kernel
      
      
      subroutine ksgga_Vxc(Vxc, exc, nelectron, &
            divergence, shellidx, orbval, n0, rho, grad, &
            eps, vrho, vsigma, weight, deltak, NAngFunc, &
            ShellParamsIdx, ThreshRho, MaxNAngFunc, ShellPairLoc)
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
            real(F64), dimension(:), intent(inout)    :: Vxc
            real(F64), intent(inout)                  :: exc
            real(F64), intent(inout)                  :: nelectron
            real(F64), intent(inout)                  :: divergence
            integer, dimension(:), intent(in)         :: shellidx
            real(F64), dimension(:), intent(in)       :: orbval
            integer, intent(in)                       :: n0
            real(F64), intent(in)                     :: rho
            real(F64), dimension(3), intent(in)       :: grad
            real(F64), intent(in)                     :: eps
            real(F64), intent(in)                     :: vrho
            real(F64), intent(in)                     :: vsigma
            real(F64), intent(in)                     :: weight
            integer, intent(in)                       :: deltak
            integer, dimension(:), intent(in)         :: NAngFunc
            integer, dimension(:), intent(in)         :: ShellParamsIdx
            real(F64), intent(in)                     :: ThreshRho
            integer, intent(in)                       :: MaxNAngFunc
            integer, dimension(:, :), intent(in)      :: ShellPairLoc

            real(F64), dimension(3) :: g
            real(F64) :: vr, vs
            real(F64) :: p_0, p_x, p_y, p_z
            real(F64), dimension(4, MaxNAngFunc) :: Tu, Tv
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

            nelectron = nelectron + weight * rho
            divergence = divergence + weight * (grad(1) + grad(2) + grad(3))
            exc = exc + weight * eps * rho

            g = grad * TWO * vs * weight
            vr = vr * weight
            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  Nu = NAngFunc(ShellParamsIdx(u))
                  do p = 1, Nu
                        p_0 = orbval(k+(p-1)*DeltaK)
                        p_x = orbval(k+(p-1)*DeltaK + 1)
                        p_y = orbval(k+(p-1)*DeltaK + 2)
                        p_z = orbval(k+(p-1)*DeltaK + 3)                        
                        Tu(1, p) = vr * p_0 + g(1) * p_x + &
                              g(2) * p_y + g(3) * p_z
                        Tu(2, p) = p_0 * g(1)
                        Tu(3, p) = p_0 * g(2)
                        Tu(4, p) = p_0 * g(3)
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
                        end do
                        l = l + Nv * DeltaK
                        z0 = ShellPairLoc(v, u)
                        z1 = ShellPairLoc(v, u) + Nu * Nv - 1
                        call ksgga_Kernel(Vxc(z0:z1), Tv, Tu, Nv, Nu)
                  end do
            end do
      end subroutine ksgga_Vxc


      subroutine ksgga_UVxc(Vxc, exc, &
            nelectron, divergence, shellidx, orbval, n0, rhoa, rhob, &
            grada, gradb, eps, vrhoa, vrhob, vsigma_aa, &
            vsigma_bb, vsigma_ab, weight, deltak, NAngFunc, &
            ShellParamsIdx, ThreshRho, MaxNAngFunc, ShellPairLoc)
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
            real(F64), dimension(:, :), intent(inout)  :: Vxc
            real(F64), intent(inout)                   :: exc
            real(F64), intent(inout)                   :: nelectron
            real(F64), intent(inout)                   :: divergence
            integer, dimension(:), intent(in)          :: shellidx
            real(F64), dimension(:), intent(in)        :: orbval
            integer, intent(in)                        :: n0
            real(F64), intent(in)                      :: rhoa
            real(F64), intent(in)                      :: rhob
            real(F64), dimension(3), intent(in)        :: grada
            real(F64), dimension(3), intent(in)        :: gradb
            real(F64), intent(in)                      :: eps
            real(F64), intent(in)                      :: vrhoa
            real(F64), intent(in)                      :: vrhob
            real(F64), intent(in)                      :: vsigma_aa
            real(F64), intent(in)                      :: vsigma_bb
            real(F64), intent(in)                      :: vsigma_ab
            real(F64), intent(in)                      :: weight
            integer, intent(in)                        :: deltak
            integer, dimension(:), intent(in)          :: NAngFunc
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            real(F64), intent(in)                      :: ThreshRho
            integer, intent(in)                        :: MaxNAngFunc
            integer, dimension(:, :), intent(in)       :: ShellPairLoc

            real(F64) :: rho
            real(F64), dimension(3) :: ga, gb
            real(F64) :: vra, vsaa, vsab
            real(F64) :: vrb, vsbb
            real(F64) :: p_0, p_x, p_y, p_z
            real(F64), dimension(4, MaxNAngFunc) :: Tua, Tub, Tv
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
            !
            ! Update numerical integrals (energy and diagnostics)
            !
            nelectron = nelectron + weight * rho
            divergence = divergence + weight * (grada(1) + grada(2) + &
                  grada(3) + gradb(1) + gradb(2) + gradb(3))
            exc = exc + weight * eps * rho

            vra = vra * weight
            vrb = vrb * weight
            vsaa = vsaa * TWO * weight
            vsab = vsab * weight
            vsbb = vsbb * TWO * weight
            
            ga = vsaa * grada + vsab * gradb
            gb = vsbb * gradb + vsab * grada

            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  Nu = NAngFunc(ShellParamsIdx(u))
                  do p = 1, Nu
                        p_0 = orbval(k+(p-1)*DeltaK)
                        p_x = orbval(k+(p-1)*DeltaK + 1)
                        p_y = orbval(k+(p-1)*DeltaK + 2)
                        p_z = orbval(k+(p-1)*DeltaK + 3)

                        Tua(1, p) = vra * p_0 + ga(1) * p_x + ga(2) * p_y + ga(3) * p_z
                        Tua(2, p) = p_0 * ga(1)
                        Tua(3, p) = p_0 * ga(2)
                        Tua(4, p) = p_0 * ga(3)

                        Tub(1, p) = vrb * p_0 + gb(1) * p_x + gb(2) * p_y + gb(3) * p_z
                        Tub(2, p) = p_0 * gb(1)
                        Tub(3, p) = p_0 * gb(2)
                        Tub(4, p) = p_0 * gb(3)
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
                        end do
                        l = l + Nv * DeltaK
                        z0 = ShellPairLoc(v, u)
                        z1 = ShellPairLoc(v, u) + Nu * Nv - 1
                        call ksgga_Kernel(Vxc(z0:z1, 1), Tv, Tua, Nv, Nu)
                        call ksgga_Kernel(Vxc(z0:z1, 2), Tv, Tub, Nv, Nu)
                  end do
            end do
      end subroutine ksgga_UVxc
end module KohnShamGGA
