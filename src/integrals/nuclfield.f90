! ------------------------------------------------------------------------
! ONE-ELECTRON INTEGRALS OF THE ELECTRIC FIELD OPERATOR (\nabla V_{ext})
! ------------------------------------------------------------------------
! Calculate electric field (gradient of the Coulomb potential) due to
! the specified nucleus (\alpha). X, Y, and Z components of
! the electric field are stored in three separete symmetric matrices. The
! corresponding matrix elements are defined as
! <\phi_a | d/dR_{\alpha,q} V_{ext} | \phi_b> =
! <\phi_a | d/dR_{\alpha,q} -Z_\alpha / |r-R_\alpha| | \phi_b> =
! <\phi_a | -Z_\alpha (q-R_{\alpha,q}) / ((r-R_\alpha)^2)^{3/2} | \phi_b>
! where \phi_a and \phi_b are Cartesian Gaussian orbitals (centered
! on any nucleus or dummy center), R_{\alpha,q}, q \in \{X, Y, Z\} is
! a q-th coordinate of the nucleus \alpha,
! and V_{ext} is the Coulomb potential of the nuclei:
! V_{ext} = \sum_\beta -Z_\beta / |r-R_\beta|.
! ------------------------------------------------------------------------
module nuclfield
      use math_constants
      use hermite
      use boys
      use ecpint
      use gparam

      implicit none

contains

      subroutine vqab(vcx, vcy, vcz, eijx, eijy, eijz, &
            rtuv, la, lb, ma, mb, na, nb)
            
            double precision, intent(out)              :: vcx, vcy, vcz
            double precision, dimension(:), intent(in) :: eijx, eijy, eijz
            double precision, dimension(:, :, :)       :: rtuv
            integer, intent(in)                        :: la, lb, ma, mb, na, nb

            integer :: t, u, v, pos1, pos2, pos3
            double precision :: sum1rx, sum2rx, sum3rx
            double precision :: sum1ry, sum2ry, sum3ry
            double precision :: sum1rz, sum2rz, sum3rz

            double precision :: et, eu, ev

            pos1 = eijmatrix_position(la + lb, lb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(na + nb, nb, 0)

            sum1rx = ZERO
            sum1ry = ZERO
            sum1rz = ZERO
            do v = 0, na + nb
                  ev = eijz(pos3 + v)
                  sum2rx = ZERO
                  sum2ry = ZERO
                  sum2rz = ZERO
                  do u = 0, ma + mb
                        eu = eijy(pos2 + u)
                        sum3rx = ZERO
                        sum3ry = ZERO
                        sum3rz = ZERO
                        do t = 0, la + lb
                              et = eijx(pos1 + t)
                              sum3rx = sum3rx + et * rtuv(t + 2, u + 1, v + 1)
                              sum3ry = sum3ry + et * rtuv(t + 1, u + 2, v + 1)
                              sum3rz = sum3rz + et * rtuv(t + 1, u + 1, v + 2)
                        end do
                        sum2rx = sum2rx + eu * sum3rx
                        sum2ry = sum2ry + eu * sum3ry
                        sum2rz = sum2rz + eu * sum3rz
                  end do
                  sum1rx = sum1rx + ev * sum2rx
                  sum1ry = sum1ry + ev * sum2ry
                  sum1rz = sum1rz + ev * sum2rz
            end do
            vcx = -sum1rx
            vcy = -sum1ry
            vcz = -sum1rz
      end subroutine vqab

      
      subroutine vqabbatch(shell1, a, shell2, b, c, vcx, vcy, vcz)
            integer, intent(in)                         :: shell1
            integer, intent(in)                         :: a
            integer, intent(in)                         :: shell2
            integer, intent(in)                         :: b
            integer, intent(in)                         :: c
            double precision, dimension(:), intent(out) :: vcx
            double precision, dimension(:), intent(out) :: vcy
            double precision, dimension(:), intent(out) :: vcz

            integer, dimension(MAX_NFUNC) :: lla, llb, mma, mmb, nna, nnb
            double precision :: alphai, betaj, gamma
            double precision, dimension(3) :: ra, rb
            double precision, dimension(3) :: ara, brb, p, pa, pb, rab
            double precision :: xabsq, yabsq, zabsq
            double precision :: alpha_reduced
            !
            ! Maximum value of a single upper index of E^{ij}_t
            !
            integer, parameter :: max_index = 2 * max_l + 2
            !
            ! Dimension = \sum_{k = 0}^{max_index} (k + 1)^2
            !
            integer, parameter :: eijdim = (2 * max_index**3 + 9 * max_index**2 + 13 * max_index + 6) / 6
            double precision, dimension(eijdim) :: eijx, eijy, eijz
            integer :: i, j, v1, v2, v
            integer :: la, lb, ma, mb, na, nb
            double precision :: const1, const2
            integer :: momentum1, momentum2, momentum
            integer :: nints1, nints2, nints
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            double precision, dimension(max_chidx) :: fmarray
            double precision, dimension(3) :: rc, rpc
            double precision :: vx, vy, vz
            double precision :: norma, normb, normab
            double precision :: cntra, cntrb, cntrab
            double precision :: dcz

            momentum1 = SHTYPE(shell1)
            momentum2 = SHTYPE(shell2)
            momentum = momentum1 + momentum2

            lla = LL(:, momentum1)
            mma = MM(:, momentum1)
            nna = NN(:, momentum1)

            llb = LL(:, momentum2)
            mmb = MM(:, momentum2)
            nnb = NN(:, momentum2)

            ra = ATOMR(:, a)
            rb = ATOMR(:, b)
            rc = ATOMR(:, c)
            rab = ra - rb

            xabsq = (ra(1) - rb(1))**2
            yabsq = (ra(2) - rb(2))**2
            zabsq = (ra(3) - rb(3))**2

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints = nints1 * nints2

            vcx(1:nints) = ZERO
            vcy(1:nints) = ZERO
            vcz(1:nints) = ZERO

            dcz = dble(ECP_INUCLZ(c))

            do i = 1, nprm(shell1)
                  alphai = expn(i, shell1)
                  ara = alphai * ra
                  cntra = CNTR(i, shell1)
                  do j = 1, nprm(shell2)
                        betaj = expn(j, shell2)
                        brb = betaj * rb
                        cntrb = CNTR(j, shell2)
                        cntrab = cntra * cntrb
                        gamma = alphai + betaj
                        alpha_reduced = alphai * betaj / gamma

                        const1 = exp(-alpha_reduced * (xabsq + yabsq + zabsq))
                        const2 = const1
                        const1 = const1 * pi32 / sqrt(gamma**3)
                        const2 = const2 * two * pi / gamma

                        p = (ara + brb) / gamma
                        pa = p - ra
                        pb = p - rb
                        !
                        ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                        ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook
                        ! because the coeffs are linear functions of the seed so
                        ! it may be incorporated into const1.
                        !
                        call eijmatrix(momentum, one, gamma, pa(1), pb(1), eijx)
                        call eijmatrix(momentum, one, gamma, pa(2), pb(2), eijy)
                        call eijmatrix(momentum, one, gamma, pa(3), pb(3), eijz)
                        rpc = p - rc
                        call fm(momentum + 1, gamma * dot_product(rpc, rpc), fmarray)
                        call chints(momentum + 1, fmarray, rtuv, rpc(1), rpc(2), rpc(3), gamma)

                        v = 1
                        do v1 = 1, nints1
                              la = lla(v1)
                              ma = mma(v1)
                              na = nna(v1)
                              norma = -const2 * dcz * cntrab * CNTRNORM(v1, shell1)
                              do v2 = 1, nints2
                                    lb = llb(v2)
                                    mb = mmb(v2)
                                    nb = nnb(v2)
                                    normb = CNTRNORM(v2, shell2)
                                    normab = norma * normb
                                    call vqab(vx, vy, vz, eijx, eijy, eijz, &
                                          rtuv, la, lb, ma, mb, na, nb)
                                    vcx(v) = vcx(v) + normab * vx
                                    vcy(v) = vcy(v) + normab * vy
                                    vcz(v) = vcz(v) + normab * vz
                                    v = v + 1
                              end do
                        end do
                  end do
            end do
      end subroutine vqabbatch

      
      subroutine nfield1e(vxmat, vymat, vzmat, alpha)
            ! -------------------------------------------------------------------------
            ! Calculate electric field (gradient of the Coulomb potential) due to
            ! the specified nucleus (\alpha). X, Y, and Z components of
            ! the electric field are stored in three separete symmetric matrices. The
            ! corresponding matrix elements are defined as
            ! <\phi_a | d/dR_{\alpha,q} V_{ext} | \phi_b> =
            ! <\phi_a | d/dR_{\alpha,q} -Z_\alpha / |r-R_\alpha| | \phi_b> =
            ! <\phi_a | -Z_\alpha (q-R_{\alpha,q}) / ((r-R_\alpha)^2)^{3/2} | \phi_b>
            ! where \phi_a and \phi_b are Cartesian Gaussian orbitals (centered
            ! on any nucleus or dummy center), R_{\alpha,q}, q \in \{X, Y, Z\} is
            ! a q-th coordinate of the nucleus \alpha, and V_{ext} is the Coulomb
            ! potential of all nuclei present in the molecule:
            ! V_{ext} = \sum_\beta -Z_\beta / |r-R_\beta|.
            ! Note that only nucleus \alpha contributes to d/dR_{\alpha,q} V_{ext}.
            ! The specified nucleus must belong to the set of non-dummy centers.
            ! Nuclear charges specified in ECP module are used. However, the
            ! pseudopotential contribution to the field is not computed in this
            ! module.
            ! Change sign of the integrals to get derivatives of the potential
            ! with respect to electronic coordinates. Sum the integrals over all
            ! non-dummy centers to get the total electric field.
            ! -------------------------------------------------------------------------
            ! 1. Nuclear attraction integral - T. Helgaker, Molecular
            !    Electronic-Structure Theory, Eqs 9.9.21 and 9.9.32, p. 375-6
            ! -------------------------------------------------------------------------
            ! VXMAT, - Input/output. On exit, these matrices are updated by adding the
            ! VYMAT    following matrix elements:
            ! VZMAT    <\phi_a | d/dR_{\alpha,q} V_{ext} | \phi_b>
            !          where q = X, Y, Z, respectively. Only lower triangle of each
            !          symmetric matrix is referenced.
            ! ALPHA  - Index of the nucleus which generates the electric field
            !
            double precision, dimension(:, :), intent(inout) :: vxmat
            double precision, dimension(:, :), intent(inout) :: vymat
            double precision, dimension(:, :), intent(inout) :: vzmat
            integer, intent(in)                              :: alpha
            
            integer :: a, b
            integer :: shella, shellb
            integer :: ishella, ishellb
            integer :: p0, q0, p1, q1, p, q, v
            double precision, dimension(MAX_NFUNC**2) :: vcx, vcy, vcz

            if (isdummy(alpha)) then
               return
            end if


            do b = 1, NATOM
                  do a = b, NATOM
                        do shellb = sh0(b), sh0(b+1) - 1
                              do shella = max(shellb, sh0(a)), sh0(a+1) - 1
                                    ishella = SH(shella)
                                    ishellb = SH(shellb)
                                    call vqabbatch(ishellb, b, ishella, a, alpha, vcx, vcy, vcz)
                                    p0 = SHPOS(shella)
                                    p1 = SHPOS(shella+1) - 1
                                    q0 = SHPOS(shellb)
                                    q1 = SHPOS(shellb+1) - 1
                                    v = 1
                                    do q = q0, q1
                                          do p = p0, p1
                                                vxmat(p, q) = vxmat(p, q) + vcx(v)
                                                vymat(p, q) = vymat(p, q) + vcy(v)
                                                vzmat(p, q) = vzmat(p, q) + vcz(v)
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine nfield1e
end module nuclfield
