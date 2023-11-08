module spsp
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite_automatic
      use hermite
      use lcexch

      implicit none

contains

      subroutine ints2e_spsp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: m1, m2, m3, m4

            integer, dimension(0:1) :: s1, a1, s2, a2


            s1(m1) = shell1
            s1(m2) = shell2
            a1(m1) = a
            a1(m2) = b
            s2(m3) = shell3
            s2(m4) = shell4
            a2(m3) = c
            a2(m4) = d
            call ints2e_spsp(s1(0), a1(0), s1(1), a1(1), s2(0), a2(0), s2(1), a2(1), gabcd)
      end subroutine ints2e_spsp_perm


      function repint_spsp(eab0, eab1, ecd0, ecd1, rtuv, &
            lb, ld, mb, md, nb, nd)
            double precision :: repint_spsp
            
            double precision, intent(in) :: eab0, eab1
            double precision, intent(in) :: ecd0, ecd1
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: lb, ld, mb, md, nb, nd

            double precision :: t1, t2

            t1 = eab0 * (ecd0 * rtuv(1, 1, 1) - ecd1 * rtuv(1 + ld, 1 + md, 1 + nd))
            t2 = eab1 * (ecd0 * rtuv(1 + lb, 1 + mb, 1 + nb) &
                  - ecd1 * rtuv(1 + lb + ld, 1 + mb + md, 1 + nb + nd))
            repint_spsp = t1 + t2
      end function repint_spsp


      subroutine ints2e_spsp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
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
            double precision :: const1
            integer, dimension(:), pointer :: llb, mmb, nnb
            integer, dimension(:), pointer :: lld, mmd, nnd
            integer :: i, j, k, l
            integer :: v2, v4, v
            integer :: lb, mb, nb, ld, md, nd
            integer, parameter :: momentum2 = 1
            integer, parameter :: momentum4 = 1
            integer, parameter :: nints1 = 1
            integer, parameter :: nints2 = 3
            integer, parameter :: nints3 = 1
            integer, parameter :: nints4 = 3
            integer, parameter :: nints = nints1 * nints2 * nints3 * nints4
            double precision :: norm1, norm2, norm3, norm4, norm13, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(3, 3, 3) :: rtuv
            double precision, dimension(3) :: fmarray

            double precision :: ecd0, ecd1
            double precision :: eab0, eab1

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

            llb => ll(:, momentum2)
            lld => ll(:, momentum4)

            mmb => mm(:, momentum2)
            mmd => mm(:, momentum4)

            nnb => nn(:, momentum2)
            nnd => nn(:, momentum4)

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

                        eab1 = one / (two * p)

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
                                    ecd1 = one / (two * q)
                                    call f2(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_2(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1

                                    norm1 = const1 * nrml(1, i, shell1)
                                    norm3 = nrml(1, k, shell3)
                                    norm13 = norm1 * norm3

                                    pbloop: do v2 = 1, 3
                                          lb = llb(v2)
                                          mb = mmb(v2)
                                          nb = nnb(v2)
                                          norm2 = norm13 * nrml(v2, j, shell2)
                                          eab0 = rpb(v2)

                                          pdloop: do v4 = 1, 3
                                                ld = lld(v4)
                                                md = mmd(v4)
                                                nd = nnd(v4)
                                                norm4 = nrml(v4, l, shell4)
                                                norm = norm2 * norm4
                                                ecd0 = rqd(v4)

                                                gabcd(v) = gabcd(v) + norm & 
                                                      * repint_spsp(eab0, eab1, ecd0, ecd1, rtuv, &
                                                      lb, ld, mb, md, nb, nd)
                                                v = v + 1
                                          end do pdloop
                                    end do pbloop
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_spsp
end module spsp
