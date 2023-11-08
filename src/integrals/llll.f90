module llll
      use math_constants
      use arithmetic
      use boys
      use hermite
      use lcexch
      use gto

      implicit none

contains

      subroutine abtransf(gabtuv, eijx, eijy, eijz, rtuv, la, lb, ma, mb, na, nb, lket)
            real(F64), dimension(0:, 0:, 0:), intent(out):: gabtuv
            real(F64), dimension(:), intent(in)          :: eijx, eijy, eijz
            real(F64), dimension(:, :, :), intent(in)    :: rtuv
            integer, intent(in)                          :: la, lb, ma, mb, na, nb
            integer, intent(in)                          :: lket

            integer          :: t, u, v
            real(F64) :: sum1, sum2, sum3
            real(F64) :: et, eu, ev
            real(F64) :: phase1, phase2, phase3
            integer :: pos1, pos2, pos3
            integer :: tau, nu, phi

            pos1 = eijmatrix_position(na + nb, nb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(la + lb, lb, 0)

            phase1 = one
            do phi = 0, lket
                  phase2 = phase1
                  do nu = 0, lket - phi
                        phase3 = phase2
                        do tau = 0, lket - nu - phi

                              sum1 = zero
                              do v = 0, na + nb
                                    ev = eijz(pos1 + v)
                                    sum2 = zero
                                    do u = 0, ma + mb
                                          eu = eijy(pos2 + u)
                                          sum3 = zero
                                          do t = 0, la + lb
                                                et = eijx(pos3 + t)
                                                sum3 = sum3 + et * rtuv(1 + t + tau, 1 + u + nu, 1 + v + phi)
                                          end do
                                          sum2 = sum2 + eu * sum3
                                    end do
                                    sum1 = sum1 + ev * sum2
                              end do

                              gabtuv(tau, nu, phi) = phase3 * sum1

                              phase3 = -phase3
                        end do
                        phase2 = -phase2
                  end do
                  phase1 = -phase1
            end do
      end subroutine abtransf


      subroutine cdtransf(gabcd, eijx, eijy, eijz, gabtuv, lc, ld, mc, md, nc, nd)
            real(F64), intent(out)              :: gabcd
            real(F64), dimension(:), intent(in) :: eijx, eijy, eijz
            real(F64), dimension(0:, 0:, 0:)    :: gabtuv
            integer, intent(in)                        :: lc, ld, mc, md, nc, nd

            real(F64) :: ephi, enu, etau
            real(F64) :: sum1, sum2, sum3
            integer :: pos1, pos2, pos3
            integer :: phi, nu, tau

            pos1 = eijmatrix_position(nc + nd, nd, 0)
            pos2 = eijmatrix_position(mc + md, md, 0)
            pos3 = eijmatrix_position(lc + ld, ld, 0)

            sum1 = zero
            do phi = 0, nc + nd
                  ephi = eijz(pos1 + phi)
                  sum2 = zero
                  do nu = 0, mc + md
                        enu = eijy(pos2 + nu)
                        sum3 = zero
                        do tau = 0, lc + ld
                              etau = eijx(pos3 + tau)
                              sum3 = sum3 + etau * gabtuv(tau, nu, phi)
                        end do
                        sum2 = sum2 + enu * sum3
                  end do
                  sum1 = sum1 + ephi * sum2
            end do

            gabcd = sum1
      end subroutine cdtransf


      subroutine ints2e_llll(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            ! -----------------------------------------------------------
            ! Calculate electron repulsion integrals using
            ! McMurchie-Davidson scheme. This is subroutine for arbitrary
            ! angular momenta. Subroutines for specific quartets are
            ! are generated automatically using this code as a template.
            ! -----------------------------------------------------------
            ! 1. T. Helgaker, Molecular Electronic-Structure Theory
            ! -----------------------------------------------------------
            ! SHELL1,    - Indices of shells
            ! SHELL2,
            ! SHELL3,
            ! SHELL4
            !
            ! A, B, C, D - Indices of atoms
            !
            ! GABCD      - Array of dimension (1:n), n >= NINTS
            !              in which electronic repulsion integrals
            !              are stored as an output
            !
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            real(F64), dimension(:), intent(out) :: gabcd

            real(F64) :: alpha_a, alpha_b, alpha_c, alpha_d, p, q
            real(F64), dimension(3) :: ra, rb, rc, rd, rp, rq, ara, brb, crc, drd, rpa, rpb, rqc, rqd, rpq
            real(F64) :: xabsq, yabsq, zabsq, xcdsq, ycdsq, zcdsq
            real(F64) :: alpha_reduced1, alpha_reduced2, alpha
            real(F64) :: kabcd, abexponent

            ! Maximum value of single index of Hermite Coulomb repulsion integral
            integer, parameter :: max_index = 2 * max_l

            real(F64), dimension((2 * max_index**3 + &
                  9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx1, eijy1, eijz1, eijx2, eijy2, eijz2

            real(F64) :: const1
            integer :: i, j, k, l
            integer :: v1, v2, v3, v4, v
            integer :: la, lb, ma, mb, na, nb, lc, ld, mc, md, nc, nd
            integer :: momentum1, momentum2, momentum3, momentum4
            integer :: nints1, nints2, nints3, nints4, nints
            real(F64) :: norm1, norm2, norm3, norm4, norm12, norm123, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            real(F64), dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            real(F64), dimension(0:max_index, 0:max_index, 0:max_index) :: gabtuv
            real(F64), dimension(max_chidx) :: fmarray
            real(F64) :: gv
            !
            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum3 = shtype(shell3)
            momentum4 = shtype(shell4)

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints3 = nfunc(momentum3)
            nints4 = nfunc(momentum4)
            nints = nints1 * nints2 * nints3 * nints4

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

            associate ( &
                  lla => ll(:, momentum1), &
                  llb => ll(:, momentum2), &
                  llc => ll(:, momentum3), &
                  lld => ll(:, momentum4), &
                  mma => mm(:, momentum1), &
                  mmb => mm(:, momentum2), &
                  mmc => mm(:, momentum3), &
                  mmd => mm(:, momentum4), &
                  nna => nn(:, momentum1), &
                  nnb => nn(:, momentum2), &
                  nnc => nn(:, momentum3), &
                  nnd => nn(:, momentum4))

            xabsq = (ra(1) - rb(1))**2
            yabsq = (ra(2) - rb(2))**2
            zabsq = (ra(3) - rb(3))**2
            xcdsq = (rc(1) - rd(1))**2
            ycdsq = (rc(2) - rd(2))**2
            zcdsq = (rc(3) - rd(3))**2

            gabcd(1:nints) = zero

            do i = 1, nprm(shell1)
                  alpha_a = expn(i, shell1)
                  ara = alpha_a * ra

                  do j = 1, nprm(shell2)
                        alpha_b = expn(j, shell2)
                        brb = alpha_b * rb
                        p = alpha_a + alpha_b
                        alpha_reduced1 = alpha_a * alpha_b / p

                        abexponent = alpha_reduced1 * (xabsq + yabsq + zabsq)

                        rp = (ara + brb) / p
                        rpa = rp - ra
                        rpb = rp - rb
                        !
                        ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                        ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook
                        ! because the coeffs are linear functions of the seed so
                        ! it may be incorporated into const1.
                        !
                        call eijmatrix(momentum1 + momentum2, one, p, rpa(1), rpb(1), eijx1)
                        call eijmatrix(momentum1 + momentum2, one, p, rpa(2), rpb(2), eijy1)
                        call eijmatrix(momentum1 + momentum2, one, p, rpa(3), rpb(3), eijz1)

                        do k = 1, nprm(shell3)
                              alpha_c = expn(k, shell3)
                              crc = alpha_c * rc

                              do l = 1, nprm(shell4)
                                    alpha_d = expn(l, shell4)
                                    drd = alpha_d * rd

                                    q = alpha_c + alpha_d
                                    alpha_reduced2 = alpha_c * alpha_d / q

                                    kabcd = exp(-alpha_reduced2 * (xcdsq + ycdsq + zcdsq) &
                                          - abexponent)

                                    rq = (crc + drd) / q
                                    rqc = rq - rc
                                    rqd = rq - rd
                                    !
                                    ! alpha = p * q / (p + q)
                                    ! This line is modified to account for ERF screening
                                    ! function
                                    !
                                    alpha = lce_alpha(p, q)
                                    rpq = rp - rq
                                    !
                                    ! const1 = two * pi52 / (p * q * sqrt(p + q)) * kabcd
                                    ! This line is modified to account for ERF screening
                                    ! function
                                    !
                                    const1 = lce_normfactor(p, q) * kabcd

                                    call eijmatrix(momentum3 + momentum4, const1, q, rqc(1), rqd(1), eijx2)
                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(2), rqd(2), eijy2)
                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(3), rqd(3), eijz2)

                                    call fm(momentum1 + momentum2 + momentum3 + momentum4, &
                                          alpha * dot_product(rpq, rpq), fmarray)
                                    call chints(momentum1 + momentum2 + momentum3 + momentum4, &
                                          fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1
                                    do v1 = 1, nints1

                                          la = lla(v1)
                                          ma = mma(v1)
                                          na = nna(v1)
                                          norm1 = nrml(v1, i, shell1)

                                          do v2 = 1, nints2

                                                lb = llb(v2)
                                                mb = mmb(v2)
                                                nb = nnb(v2)
                                                norm2 = nrml(v2, j, shell2)
                                                norm12 = norm1 * norm2
                                                !
                                                ! Hermite-->Cartesian transformation
                                                ! of bra indices
                                                !
                                                call abtransf(gabtuv, eijx1, eijy1, eijz1, rtuv, &
                                                      la, lb, ma, mb, na, nb, momentum3 + momentum4)

                                                do v3 = 1, nints3

                                                      lc = llc(v3)
                                                      mc = mmc(v3)
                                                      nc = nnc(v3)
                                                      norm3 = nrml(v3, k, shell3)
                                                      norm123 = norm12 * norm3

                                                      do v4 = 1, nints4
                                                            ld = lld(v4)
                                                            md = mmd(v4)
                                                            nd = nnd(v4)
                                                            norm4 = nrml(v4, l, shell4)
                                                            norm = norm123 * norm4
                                                            !
                                                            ! Hermite-->Cartesian transformation
                                                            ! of ket indices
                                                            !
                                                            call cdtransf(gv, eijx2, eijy2, eijz2, &
                                                                  gabtuv, lc, ld, mc, md, nc, nd)
                                                            gabcd(v) = gabcd(v) + norm * gv

                                                            v = v + 1
                                                      end do
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end associate
      end subroutine ints2e_llll
end module llll
