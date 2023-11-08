module lda
      use arithmetic
      use math_constants
      use gparam
      use basis
      use tiledmatrix
      
      implicit none

      integer, parameter :: LDA_DELTAK = 1

contains
      
      pure subroutine lda_orbval(x, y, z, orbval, shellidx, n0)
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), dimension(:), intent(out)   :: orbval
            integer, dimension(:), intent(out)     :: shellidx
            integer, intent(out)                   :: n0
            
            real(F64), dimension(0:max(MAX_L, 2)) :: xl, ym, zn
            real(F64), dimension(MAX_NPRM) :: exptable
            real(F64) :: alpha
            real(F64) :: rsq
            real(F64) :: ao_val, ao_val_spher
            integer :: offset
            integer :: i, k, l, m, n, s
            integer :: u, v
            integer :: momentum, np, nints
            
            n0 = 0
            offset = 0
            xl(0)  = ONE
            ym(0)  = ONE
            zn(0)  = ONE
            !
            ! Loop over all atoms (both real and dummy centers)
            !
            atompass: do i = 1, NATOM
                  !
                  ! Generate Cartesian polynomials
                  !
                  xl(1) = x - ATOMR(1, i)
                  ym(1) = y - ATOMR(2, i)
                  zn(1) = z - ATOMR(3, i)

                  xl(2) = xl(1) * xl(1)
                  ym(2) = ym(1) * ym(1)
                  zn(2) = zn(1) * zn(1)

                  do l = 3, ATOML(i)
                        xl(l) = xl(l-1) * xl(1)
                        ym(l) = ym(l-1) * ym(1)
                        zn(l) = zn(l-1) * zn(1)
                  end do

                  rsq = xl(2) + ym(2) + zn(2)
                  !
                  ! Loop over orbital shells centered at the I-th atom
                  !
                  shloop: do k = SH0(i), SH0(i + 1) - 1
                        !
                        ! The class of the K-th shell.
                        !
                        s = SH(k)
                        !
                        ! Check if the current shell gives any non-negligible
                        ! contribution. Note that the elements of SH are sorted in
                        ! decreasing order according to R2MAX for each atom, 
                        ! thus facilitating a safe loop break at this point.
                        !
                        if (rsq > R2MAX(s)) then
                              exit shloop
                        else
                              n0 = n0 + 1
                              shellidx(n0) = k
                        end if
                        !
                        ! Cartesian angular momentum of the K-th shell
                        !
                        momentum = SHTYPE(s)
                        !
                        ! Number of primitive Gaussian functions
                        !
                        np = NPRM(s)
                        !
                        ! Number of Cartesian angular functions (Cartesian
                        ! polynomials) in the K-th shell
                        !
                        nints = nfunc(momentum)

                        do v = 1, np
                              alpha = EXPN(v, s)
                              exptable(v) = exp(-alpha * rsq)
                        end do

                        ao_val_spher = ZERO
                        do u = 1, np
                              ao_val_spher = ao_val_spher + CNTR(u, s) * exptable(u)
                        end do
                        !
                        ! Sum over Cartesian angular functions
                        !
                        do v = 1, nints
                              l = ll(v, momentum)
                              m = mm(v, momentum)
                              n = nn(v, momentum)
                              !
                              ! Multiply spherical part of the AO by the Cartesian polynomial
                              !
                              ao_val = ao_val_spher * xl(l) * ym(m) * zn(n) * CNTRNORM(v, s)
                              orbval(offset+v) = ao_val
                        end do
                        offset = offset + nints
                  end do shloop
            end do atompass
      end subroutine lda_orbval

      
      pure subroutine lda_vars(dmatrix_tiled, x, y, z, rho, orbval, &
            shellidx, n0)
            
            real(F64), dimension(:), intent(in)  :: dmatrix_tiled
            real(F64), intent(in)                :: x, y, z
            real(F64), intent(out)               :: rho
            real(F64), dimension(:), intent(out) :: orbval
            integer, dimension(:), intent(out)   :: shellidx
            integer, intent(out)                 :: n0

            real(F64) :: sum_0
            integer :: k, l, p, q, qp
            integer :: p0, p1, q0, q1
            integer :: nints_u, nints_v
            integer :: uu, u, v, vv

            rho  = ZERO
            !
            ! Calculate values of atomic orbitals
            !
            call lda_orbval(x, y, z, orbval, shellidx, n0)
            !
            ! Calculate orbital products
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
                  do p = 1, nints_u
                        sum_0 = ZERO
                        do q = 1, nints_u
                              sum_0 = sum_0 + dmatrix_tiled(qp) * orbval(k+q)
                              qp = qp + 1
                        end do
                        rho = rho + FRAC12 * sum_0 * orbval(k+p)
                  end do
                  !
                  ! Off-diagonal blocks
                  !
                  l = k + nints_u
                  shloop2: do vv = uu+1, n0
                        v = shellidx(vv)
                        q0 = SHPOS(v)
                        q1 = SHPOS(v+1) - 1
                        nints_v = q1 - q0 + 1
                        qp = tilepos(v, u)
                        do p = 1, nints_u
                              sum_0 = ZERO
                              do q = 1, nints_v
                                    sum_0 = sum_0 + dmatrix_tiled(qp) * orbval(l+q)
                                    qp = qp + 1
                              end do
                              rho = rho + sum_0 * orbval(k+p)
                        end do
                        l = l + nints_v
                  end do shloop2
                  k = k + nints_u
            end do shloop1
            rho = TWO * rho
      end subroutine lda_vars
      
      
      pure subroutine ulda_vars(dmatrixa_tiled, dmatrixb_tiled, &
            x, y, z, rhoa, rhob, orbval, shellidx, n0)
            
            real(F64), dimension(:), intent(in)  :: dmatrixa_tiled
            real(F64), dimension(:), intent(in)  :: dmatrixb_tiled
            real(F64), intent(in)                :: x, y, z
            real(F64), intent(out)               :: rhoa
            real(F64), intent(out)               :: rhob
            real(F64), dimension(:), intent(out) :: orbval
            integer, dimension(:), intent(out)   :: shellidx
            integer, intent(out)                 :: n0

            real(F64) :: suma_0, sumb_0
            integer :: k, l, p, q, qp
            integer :: p0, p1, q0, q1
            integer :: nints_u, nints_v
            integer :: uu, u, v, vv

            rhoa = ZERO
            rhob = ZERO
            !
            ! Calculate values of atomic orbitals
            !
            call lda_orbval(x, y, z, orbval, shellidx, n0)
            !
            ! Calculate orbital products
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
                  do p = 1, nints_u
                        suma_0 = ZERO
                        sumb_0 = ZERO
                        do q = 1, nints_u
                              suma_0 = suma_0 + dmatrixa_tiled(qp) * orbval(k+q)
                              sumb_0 = sumb_0 + dmatrixb_tiled(qp) * orbval(k+q)
                              qp = qp + 1
                        end do
                        rhoa = rhoa + FRAC12 * suma_0 * orbval(k+p)
                        rhob = rhob + FRAC12 * sumb_0 * orbval(k+p)
                  end do
                  !
                  ! Off-diagonal blocks
                  !
                  l = k + nints_u
                  shloop2: do vv = uu+1, n0
                        v = shellidx(vv)
                        q0 = SHPOS(v)
                        q1 = SHPOS(v+1) - 1
                        nints_v = q1 - q0 + 1
                        qp = tilepos(v, u)
                        do p = 1, nints_u
                              suma_0 = ZERO
                              sumb_0 = ZERO
                              do q = 1, nints_v
                                    suma_0 = suma_0 + dmatrixa_tiled(qp) * orbval(l+q)
                                    sumb_0 = sumb_0 + dmatrixb_tiled(qp) * orbval(l+q)
                                    qp = qp + 1
                              end do
                              rhoa = rhoa + suma_0 * orbval(k+p)
                              rhob = rhob + sumb_0 * orbval(k+p)
                        end do
                        l = l + nints_v
                  end do shloop2
                  k = k + nints_u
            end do shloop1
            rhoa = TWO * rhoa
            rhob = TWO * rhob
      end subroutine ulda_vars
end module lda
