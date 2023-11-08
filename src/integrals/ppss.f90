module ppss
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite_automatic
      use hermite
      use lcexch

      implicit none

contains

      subroutine ints2e_ppss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            integer, intent(in) :: permutation
            double precision, dimension(:), intent(out) :: gabcd

            if (permutation .eq. ERI_SSPP) then
                  call ints2e_ppss(shell3, c, shell4, d, shell1, a, shell2, b, gabcd)
            else
                  call ints2e_ppss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            end if
      end subroutine ints2e_ppss_perm


      function repint_ppss(eab0, eab1a, eab1b, eab2, rtuv, la, lb, ma, mb, na, nb)
            double precision :: repint_ppss
            
            double precision, intent(in) :: eab0, eab1a, eab1b, eab2
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, ma, mb, na, nb

            double precision :: t1, t2, t3, t4

            t1 = eab0 * rtuv(1, 1, 1)
            t2 = eab1a * rtuv(1 + la, 1 + ma, 1 + na)
            t3 = eab1b * rtuv(1 + lb, 1 + mb, 1 + nb)
            t4 = eab2 * rtuv(1 + la + lb, 1 + ma + mb, 1 + na + nb)

            repint_ppss = t1 + t2 + t3 + t4
      end function repint_ppss


      function repint_ppssd(eab0, eab1, eab2, rtuv, la, ma, na)
            double precision :: repint_ppssd
            
            double precision, intent(in) :: eab0, eab1, eab2
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, ma, na

            double precision :: t1, t2, t3

            t1 = eab0 * rtuv(1, 1, 1)
            t2 = eab1 * rtuv(1 + la, 1 + ma, 1 + na)
            t3 = eab2 * rtuv(1 + la + la, 1 + ma + ma, 1 + na + na)

            repint_ppssd = t1 + t2 + t3
      end function repint_ppssd


      subroutine ints2e_ppss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
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
            integer, dimension(:), pointer :: lla, llb, mma, mmb, nna, nnb
            integer :: i, j, k, l
            integer :: v1, v2, v
            integer :: la, lb, ma, mb, na, nb
            integer, parameter :: momentum1 = 1
            integer, parameter :: momentum2 = 1
            integer, parameter :: nints = 9
            double precision :: norm1, norm2, norm34, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(3, 3, 3) :: rtuv
            double precision, dimension(3) :: fmarray

            double precision :: pinv, pinv2
            double precision, dimension(3, 3) :: eab0
            double precision, dimension(3)    :: eab1a, eab1b
            double precision                  :: eab2
            double precision, dimension(3)    :: eab0d
            double precision, dimension(3)    :: eab1d
            double precision                  :: eab2d

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

                        pinv = one / (two * p)
                        pinv2 = pinv**2

                        eab0(1, 1) = rpa(1) * rpb(1)
                        eab0(2, 1) = rpa(2) * rpb(1)
                        eab0(3, 1) = rpa(3) * rpb(1)

                        eab0(1, 2) = rpa(1) * rpb(2)
                        eab0(2, 2) = rpa(2) * rpb(2)
                        eab0(3, 2) = rpa(3) * rpb(2)

                        eab0(1, 3) = rpa(1) * rpb(3)
                        eab0(2, 3) = rpa(2) * rpb(3)
                        eab0(3, 3) = rpa(3) * rpb(3)                        

                        eab1a(1)   = pinv * rpb(1)
                        eab1a(2)   = pinv * rpb(2)
                        eab1a(3)   = pinv * rpb(3)

                        eab1b(1)   = rpa(1) * pinv
                        eab1b(2)   = rpa(2) * pinv
                        eab1b(3)   = rpa(3) * pinv

                        eab2       = pinv2

                        eab0d(1)   = eab0(1, 1) + pinv
                        eab0d(2)   = eab0(2, 2) + pinv
                        eab0d(3)   = eab0(3, 3) + pinv
                        
                        eab1d(1)   = pinv * (rpa(1) + rpb(1))
                        eab1d(2)   = pinv * (rpa(2) + rpb(2))
                        eab1d(3)   = pinv * (rpa(3) + rpb(3))

                        eab2d      = pinv2

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
                                    call f2(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_2(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1
                                    norm34 = const1 * nrml(1, k, shell3) * nrml(1, l, shell4)
                                    
                                    laloop: do v1 = 1, 3
                                          la = lla(v1)
                                          ma = mma(v1)
                                          na = nna(v1)
                                          norm1 = norm34 * nrml(v1, i, shell1)

                                          lbloop1: do v2 = 1, 3
                                                if (v2 .ne. v1) then
                                                      !
                                                      ! Off-diagonal
                                                      !
                                                      lb = llb(v2)
                                                      mb = mmb(v2)
                                                      nb = nnb(v2)
                                                      norm2 = nrml(v2, j, shell2)
                                                      norm = norm1 * norm2

                                                      gabcd(v) = gabcd(v) + norm &
                                                            * repint_ppss(eab0(v1, v2), eab1a(v2), &
                                                            eab1b(v1), eab2, rtuv, la, lb, ma, mb, na, nb)

                                                      v = v + 1
                                                else
                                                      !
                                                      ! Diagonal
                                                      !
                                                      lb = llb(v1)
                                                      mb = mmb(v1)
                                                      nb = nnb(v1)
                                                      norm2 = nrml(v1, j, shell2)
                                                      norm = norm1 * norm2

                                                      gabcd(v) = gabcd(v) + norm &
                                                            * repint_ppssd(eab0d(v1), eab1d(v1), eab2d, rtuv, la, ma, na)

                                                      v = v + 1                                                      
                                                end if
                                          end do lbloop1
                                    end do laloop
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_ppss
end module ppss
