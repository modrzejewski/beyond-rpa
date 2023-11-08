module spss
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite
      use hermite_automatic
      use lcexch

      implicit none

contains

      subroutine ints2e_spss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            integer, intent(in) :: permutation
            double precision, dimension(:), intent(out) :: gabcd

            select case (permutation)
            case (ERI_PSSS)
                  call ints2e_spss(shell2, b, shell1, a, shell3, c, shell4, d, gabcd)
            case (ERI_SSPS)
                  call ints2e_spss(shell4, d, shell3, c, shell1, a, shell2, b, gabcd) 
            case (ERI_SSSP)     
                  call ints2e_spss(shell3, c, shell4, d, shell1, a, shell2, b, gabcd) 
            case default
                  call ints2e_spss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            end select
      end subroutine ints2e_spss_perm


      function repint_spss(eab0, eab1, rtuv, lb,mb, nb)
            double precision :: repint_spss
            
            double precision, intent(in) :: eab0, eab1
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: lb, mb, nb

            double precision :: t1, t2

            t1 = eab0 * rtuv(1, 1, 1)
            t2 = eab1 * rtuv(1 + lb, 1 + mb, 1 + nb)
            repint_spss = t1 + t2
      end function repint_spss


      subroutine ints2e_spss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
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
            integer :: i, j, k, l
            integer :: v
            integer :: lb, mb, nb
            integer, parameter :: momentum2 = 1
            integer, parameter :: nints = 3
            double precision :: norm1, norm2, norm3, norm4, norm134, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(2, 2, 2) :: rtuv
            double precision, dimension(2) :: fmarray

            double precision :: eab0, eab1

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

            llb => ll(:, momentum2)
            mmb => mm(:, momentum2)
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

                                    call f1(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_1(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    norm1 = const1 * nrml(1, i, shell1)
                                    norm3 = nrml(1, k, shell3)
                                    norm4 = nrml(1, l, shell4)
                                    norm134 = norm1 * norm3 * norm4

                                    pbloop: do v = 1, 3
                                          lb = llb(v)
                                          mb = mmb(v)
                                          nb = nnb(v)
                                          norm2 = nrml(v, j, shell2)
                                          norm = norm2 * norm134
                                          eab0 = rpb(v)

                                          gabcd(v) = gabcd(v) + norm & 
                                                * repint_spss(eab0, eab1, rtuv, lb, mb, nb)
                                    end do pbloop
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_spss
end module spss
