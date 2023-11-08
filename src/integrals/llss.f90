module llss
      use math_constants
      use ericonst
      use gto
      use boys
      use hermite
      use lcexch

      implicit none

contains

      subroutine ints2e_llss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, ketsum)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: ketsum

            if (ketsum .gt. 0) then
                  call ints2e_llss(shell3, c, shell4, d, shell1, a, shell2, b, gabcd)
            else
                  call ints2e_llss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            end if
      end subroutine ints2e_llss_perm


      function repint_llss(eijx1, eijy1, eijz1, rtuv, la, lb, ma, mb, na, nb)
            double precision :: repint_llss

            double precision, dimension(:), intent(in) :: eijx1, eijy1, eijz1
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, ma, mb, na, nb

            integer :: pos1, pos2, pos3
            integer :: t, u, v
            double precision :: x, y, z
            double precision :: et, eu, ev

            pos1 = eijmatrix_position(la + lb, lb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(na + nb, nb, 0)

            z = zero
            do v = 0, na + nb
                  ev = eijz1(pos3 + v)
                  y = zero
                  do u = 0, ma + mb
                        eu = eijy1(pos2 + u)
                        x = zero
                        do t = 0, la + lb
                              et = eijx1(pos1 + t)
                              x = x + et * rtuv(t + 1, u + 1, v + 1)
                        end do
                        y = y + eu * x
                  end do
                  z = z + ev * y
            end do

            repint_llss = z
      end function repint_llss


      subroutine ints2e_llss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            !
            ! **********************************************************
            ! DESCRIPTION:
            ! **********************************************************
            !
            ! Calculate electron repulsion integrals
            !
            ! **********************************************************
            ! REFERENCES:
            ! **********************************************************
            !
            ! 1. T. Helgaker, Molecular Electronic-Structure Theory,
            ! eq. 9.9.33
            !
            ! **********************************************************
            ! INPUTS:
            ! **********************************************************
            !
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
            !
            !
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd

            double precision :: alpha_a, alpha_b, alpha_c, alpha_d, p, q
            double precision, dimension(3) :: ra, rb, rc, rd, rp, rq, ara, brb, crc, drd, rpa, rpb, rqc, rqd, rpq
            double precision :: xabsq, yabsq, zabsq, xcdsq, ycdsq, zcdsq
            double precision :: alpha_reduced1, alpha_reduced2, alpha
            double precision :: kabcd, abexponent

            ! Maximum value of single index of Hermite Coulomb repulsion integral
            integer, parameter :: max_index = 2 * max_l

            double precision, dimension((2 * max_index**3 + &
                  9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx1, eijy1, eijz1

            double precision :: const1
            integer, dimension(:), pointer :: lla, llb, mma, mmb, nna, nnb
            integer :: i, j, k, l
            integer :: v1, v2, v
            integer :: la, lb, ma, mb, na, nb
            integer, parameter :: momentum3 = 0
            integer, parameter :: momentum4 = 0
            integer, parameter :: nints3 = 1
            integer, parameter :: nints4 = 1
            integer :: momentum1, momentum2
            integer :: nints1, nints2, nints
            double precision :: norm1, norm2, norm34, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            double precision, dimension(max_chidx) :: fmarray

            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints = nints1 * nints2 * nints3 * nints4

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

            lla => ll(:, momentum1)
            llb => ll(:, momentum2)

            mma => mm(:, momentum1)
            mmb => mm(:, momentum2)

            nna => nn(:, momentum1)
            nnb => nn(:, momentum2)

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
                                    !
                                    alpha = lce_alpha(p, q)
                                    rpq = rp - rq
                                    !
                                    ! const1 = two * pi52 / (p * q * sqrt(p + q)) * kabcd
                                    !
                                    const1 = lce_normfactor(p, q) * kabcd

                                    call fm(momentum1 + momentum2 + momentum3 + momentum4, &
                                          alpha * dot_product(rpq, rpq), fmarray)
                                    call chints(momentum1 + momentum2 + momentum3 + momentum4, &
                                          fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1
                                    norm34 = const1 * nrml(1, k, shell3) * nrml(1, l, shell4)

                                    laloop: do v1 = 1, nints1
                                          la = lla(v1)
                                          ma = mma(v1)
                                          na = nna(v1)
                                          norm1 = norm34 * nrml(v1, i, shell1)

                                          lbloop: do v2 = 1, nints2

                                                lb = llb(v2)
                                                mb = mmb(v2)
                                                nb = nnb(v2)
                                                norm2 = nrml(v2, j, shell2)
                                                norm = norm1 * norm2

                                                gabcd(v) = gabcd(v) + norm &
                                                      * repint_llss(eijx1, eijy1, eijz1, &
                                                      rtuv, la, lb, ma, mb, na, nb)

                                                v = v + 1
                                          end do lbloop
                                    end do laloop
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_llss
end module llss
