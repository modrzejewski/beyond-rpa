module pppp
      use math_constants
      use gparam
      use boys
      use hermite_automatic
      use hermite
      use lcexch

      implicit none

contains

      function repint_pppp0(eab0, eab1a, eab1b, eab2, rtuv, &
            la, lb, ma, mb, na, nb, r, s, t)
            double precision :: repint_pppp0
            
            double precision, intent(in) :: eab0, eab1a, eab1b, eab2
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, ma, mb, na, nb
            integer, intent(in) :: r, s, t

            double precision :: t1, t2, t3, t4

            t1 = eab0 * rtuv(1 + r, 1 + s, 1 + t)
            t2 = eab1a * rtuv(1 + la + r, 1 + ma + s, 1 + na + t)
            t3 = eab1b * rtuv(1 + lb + r, 1 + mb + s, 1 + nb + t)
            t4 = eab2 * rtuv(1 + la + lb + r, 1 + ma + mb + s, 1 + na + nb + t)

            repint_pppp0 = t1 - t2 - t3 + t4
      end function repint_pppp0


      function repint_pppp0d(eab0, eab1, eab2, rtuv, la, ma, na, r, s, t)
            double precision :: repint_pppp0d
            
            double precision, intent(in) :: eab0, eab1, eab2
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, ma, na
            integer, intent(in) :: r, s, t

            double precision :: t1, t2, t3

            t1 = eab0 * rtuv(1 + r, 1 + s, 1 + t)
            t2 = eab1 * rtuv(1 + la + r, 1 + ma + s, 1 + na + t)
            t3 = eab2 * rtuv(1 + la + la + r, 1 + ma + ma + s, 1 + na + na + t)

            repint_pppp0d = t1 - t2 + t3
      end function repint_pppp0d

      
      function repint_pppp_nn(eab0, eab1a, eab1b, eab2, &
            ecd0, ecd1c, ecd1d, ecd2, rtuv, la, lb, lc, ld, &
            ma, mb, mc, md, na, nb, nc, nd)
            double precision :: repint_pppp_nn

            double precision, intent(in) :: eab0, eab1a, eab1b, eab2
            double precision, intent(in) :: ecd0, ecd1c, ecd1d, ecd2
            double precision, dimension(:, :, :), intent(in) :: rtuv
            integer, intent(in) :: la, lb, lc, ld
            integer, intent(in) :: ma, mb, mc, md
            integer, intent(in) :: na, nb, nc, nd
            
            double precision :: t1, t2, t3, t4

            t1 = eab0 * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, 0, 0, 0)
            t2 = eab1a * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, la, ma, na)
            t3 = eab1b * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, lb, mb, nb)
            t4 = eab2 * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, la + lb, ma + mb, na + nb)

            repint_pppp_nn = t1 + t2 + t3 + t4
      end function repint_pppp_nn


      function repint_pppp_nd(eab0, eab1a, eab1b, eab2, &
            ecd0, ecd1, ecd2, rtuv, la, lb, lc, &
            ma, mb, mc, na, nb, nc)
            double precision :: repint_pppp_nd

            double precision, intent(in) :: eab0, eab1a, eab1b, eab2
            double precision, intent(in) :: ecd0, ecd1, ecd2
            double precision, dimension(:, :, :), intent(in) :: rtuv
            integer, intent(in) :: la, lb, lc
            integer, intent(in) :: ma, mb, mc
            integer, intent(in) :: na, nb, nc
            
            double precision :: t1, t2, t3, t4

            t1 = eab0 * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, 0, 0, 0)
            t2 = eab1a * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, la, ma, na)
            t3 = eab1b * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, lb, mb, nb)
            t4 = eab2 * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, la + lb, ma + mb, na + nb)

            repint_pppp_nd = t1 + t2 + t3 + t4
      end function repint_pppp_nd

      
      function repint_pppp_dn(eab0, eab1, eab2, &
            ecd0, ecd1c, ecd1d, ecd2, rtuv, la, lc, ld, &
            ma, mc, md, na, nc, nd)
            double precision :: repint_pppp_dn

            double precision, intent(in) :: eab0, eab1, eab2
            double precision, intent(in) :: ecd0, ecd1c, ecd1d, ecd2
            double precision, dimension(:, :, :), intent(in) :: rtuv
            integer, intent(in) :: la, lc, ld
            integer, intent(in) :: ma, mc, md
            integer, intent(in) :: na, nc, nd
            
            double precision :: t1, t2, t3

            t1 = eab0 * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, 0, 0, 0)
            t2 = eab1 * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, la, ma, na)
            t3 = eab2 * repint_pppp0(ecd0, ecd1c, ecd1d, ecd2, rtuv, &
                  lc, ld, mc, md, nc, nd, la + la, ma + ma, na + na)

            repint_pppp_dn = t1 + t2 + t3
      end function repint_pppp_dn


      function repint_pppp_dd(eab0, eab1, eab2, &
            ecd0, ecd1, ecd2, rtuv, la, lc, &
            ma, mc, na, nc)
            double precision :: repint_pppp_dd

            double precision, intent(in) :: eab0, eab1, eab2
            double precision, intent(in) :: ecd0, ecd1, ecd2
            double precision, dimension(:, :, :), intent(in) :: rtuv
            integer, intent(in) :: la, lc
            integer, intent(in) :: ma, mc
            integer, intent(in) :: na, nc
            
            double precision :: t1, t2, t3

            t1 = eab0 * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, 0, 0, 0)
            t2 = eab1 * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, la, ma, na)
            t3 = eab2 * repint_pppp0d(ecd0, ecd1, ecd2, rtuv, &
                  lc, mc, nc, la + la, ma + ma, na + na)

            repint_pppp_dd = t1 + t2 + t3
      end function repint_pppp_dd


      subroutine ints2e_pppp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
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
            integer, dimension(:), pointer :: llc, lld, mmc, mmd, nnc, nnd
            integer :: i, j, k, l
            integer :: v1, v2, v3, v4, v
            integer :: la, lb, ma, mb, na, nb, lc, ld, mc, md, nc, nd
            integer, parameter :: momentum1 = 1
            integer, parameter :: momentum2 = 1
            integer, parameter :: momentum3 = 1
            integer, parameter :: momentum4 = 1
            integer, parameter :: nints = 81
            double precision :: norm1, norm2, norm3, norm4, norm12, norm123, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(5, 5, 5) :: rtuv
            double precision, dimension(5) :: fmarray

            double precision :: pinv, pinv2
            double precision :: qinv, qinv2
            double precision, dimension(3, 3) :: eab0, ecd0
            double precision, dimension(3)    :: eab1a, eab1b, ecd1c, ecd1d
            double precision                  :: eab2, ecd2
            double precision, dimension(3)    :: eab0diag, ecd0diag
            double precision, dimension(3)    :: eab1diag, ecd1diag
            double precision                  :: eab2diag, ecd2diag

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

            lla => ll(:, momentum1)
            llb => ll(:, momentum2)
            llc => ll(:, momentum3)
            lld => ll(:, momentum4)

            mma => mm(:, momentum1)
            mmb => mm(:, momentum2)
            mmc => mm(:, momentum3)
            mmd => mm(:, momentum4)

            nna => nn(:, momentum1)
            nnb => nn(:, momentum2)
            nnc => nn(:, momentum3)
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

                        pinv        = one / (two * p)
                        pinv2       = pinv**2

                        eab0(1, 1)  = rpa(1) * rpb(1)
                        eab0(2, 1)  = rpa(2) * rpb(1)
                        eab0(3, 1)  = rpa(3) * rpb(1)

                        eab0(1, 2)  = rpa(1) * rpb(2)
                        eab0(2, 2)  = rpa(2) * rpb(2)
                        eab0(3, 2)  = rpa(3) * rpb(2)

                        eab0(1, 3)  = rpa(1) * rpb(3)
                        eab0(2, 3)  = rpa(2) * rpb(3)
                        eab0(3, 3)  = rpa(3) * rpb(3)                        

                        eab1a(1)    = pinv * rpb(1)
                        eab1a(2)    = pinv * rpb(2)
                        eab1a(3)    = pinv * rpb(3)

                        eab1b(1)    = rpa(1) * pinv
                        eab1b(2)    = rpa(2) * pinv
                        eab1b(3)    = rpa(3) * pinv

                        eab2        = pinv2

                        eab0diag(1) = eab0(1, 1) + pinv
                        eab0diag(2) = eab0(2, 2) + pinv
                        eab0diag(3) = eab0(3, 3) + pinv
                        
                        eab1diag(1) = pinv * (rpa(1) + rpb(1))
                        eab1diag(2) = pinv * (rpa(2) + rpb(2))
                        eab1diag(3) = pinv * (rpa(3) + rpb(3))

                        eab2diag    = pinv2

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

                                    qinv        = one / (two * q)
                                    qinv2       = qinv**2

                                    ecd0(1, 1)  = rqc(1) * rqd(1)
                                    ecd0(2, 1)  = rqc(2) * rqd(1)
                                    ecd0(3, 1)  = rqc(3) * rqd(1)

                                    ecd0(1, 2)  = rqc(1) * rqd(2)
                                    ecd0(2, 2)  = rqc(2) * rqd(2)
                                    ecd0(3, 2)  = rqc(3) * rqd(2)

                                    ecd0(1, 3)  = rqc(1) * rqd(3)
                                    ecd0(2, 3)  = rqc(2) * rqd(3)
                                    ecd0(3, 3)  = rqc(3) * rqd(3)                        

                                    ecd1c(1)    = qinv * rqd(1)
                                    ecd1c(2)    = qinv * rqd(2)
                                    ecd1c(3)    = qinv * rqd(3)

                                    ecd1d(1)    = rqc(1) * qinv
                                    ecd1d(2)    = rqc(2) * qinv
                                    ecd1d(3)    = rqc(3) * qinv

                                    ecd2        = qinv2

                                    ecd0diag(1) = ecd0(1, 1) + qinv
                                    ecd0diag(2) = ecd0(2, 2) + qinv
                                    ecd0diag(3) = ecd0(3, 3) + qinv

                                    ecd1diag(1) = qinv * (rqc(1) + rqd(1))
                                    ecd1diag(2) = qinv * (rqc(2) + rqd(2))
                                    ecd1diag(3) = qinv * (rqc(3) + rqd(3))

                                    ecd2diag    = qinv2
                                    !
                                    ! const1 = two * pi52 / (p * q * sqrt(p + q)) * kabcd
                                    !
                                    const1 = lce_normfactor(p, q) * kabcd
                                    call f4(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_4(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1
                                    
                                    laloop: do v1 = 1, 3
                                          la = lla(v1)
                                          ma = mma(v1)
                                          na = nna(v1)
                                          norm1 = const1 * nrml(v1, i, shell1)

                                          lbloop1: do v2 = 1, 3
                                                lb = llb(v2)
                                                mb = mmb(v2)
                                                nb = nnb(v2)
                                                norm2 = nrml(v2, j, shell2)
                                                norm12 = norm1 * norm2

                                                if (v2 .ne. v1) then
                                                      do v3 = 1, 3
                                                            lc = llc(v3)
                                                            mc = mmc(v3)
                                                            nc = nnc(v3)
                                                            norm3 = nrml(v3, k, shell3)
                                                            norm123 = norm12 * norm3
                                                            
                                                            do v4 = 1, 3
                                                                  ld = lld(v4)
                                                                  md = mmd(v4)
                                                                  nd = nnd(v4)
                                                                  norm4 = nrml(v4, l, shell4)
                                                                  norm = norm123 * norm4

                                                                  if (v4 .ne. v3) then
                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_pppp_nn(eab0(v1, v2), eab1a(v2), &
                                                                              eab1b(v1), eab2, ecd0(v3, v4), ecd1c(v4), &
                                                                              ecd1d(v3), ecd2, &
                                                                              rtuv, la, lb, lc, ld, &
                                                                              ma, mb, mc, md, na, nb, nc, nd)
                                                                  else
                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_pppp_nd(eab0(v1, v2), eab1a(v2), &
                                                                              eab1b(v1), eab2, ecd0diag(v3), ecd1diag(v3), &
                                                                              ecd2diag, rtuv, la, lb, lc, &
                                                                              ma, mb, mc, na, nb, nc)
                                                                  end if
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                else
                                                      do v3 = 1, 3
                                                            lc = llc(v3)
                                                            mc = mmc(v3)
                                                            nc = nnc(v3)
                                                            norm3 = nrml(v3, k, shell3)
                                                            norm123 = norm12 * norm3

                                                            do v4 = 1, 3
                                                                  ld = lld(v4)
                                                                  md = mmd(v4)
                                                                  nd = nnd(v4)
                                                                  norm4 = nrml(v4, l, shell4)
                                                                  norm = norm123 * norm4

                                                                  if (v4 .ne. v3) then
                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_pppp_dn(eab0diag(v1), eab1diag(v1), &
                                                                              eab2diag, ecd0(v3, v4), ecd1c(v4), ecd1d(v3),&
                                                                              ecd2diag, rtuv, la, lc, ld, &
                                                                              ma, mc, md, na, nc, nd)
                                                                  else
                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_pppp_dd(eab0diag(v1), eab1diag(v1), &
                                                                              eab2diag, ecd0diag(v3), ecd1diag(v3), &
                                                                              ecd2diag, rtuv, la, lc, &
                                                                              ma, mc, na, nc)
                                                                  end if
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                end if
                                          end do lbloop1
                                    end do laloop
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_pppp
end module pppp
