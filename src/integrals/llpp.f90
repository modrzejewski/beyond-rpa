module llpp
      use math_constants
      use ericonst
      use gto
      use boys
      use hermite
      use lcexch

      implicit none

contains

      subroutine ints2e_llpp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, brasum)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: brasum

            if (brasum .eq. 2) then
                  call ints2e_llpp(shell3, c, shell4, d, shell1, a, shell2, b, gabcd, .true.)
            else
                  call ints2e_llpp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, .false.)
            end if
      end subroutine ints2e_llpp_perm


      function repint_llpp(eijx1, eijy1, eijz1, ecd0, ecd1c, ecd1d, ecd2, rtuv, la, lb, lc, ld, &
            ma, mb, mc, md, na, nb, nc, nd)
            double precision :: repint_llpp

            double precision, dimension(:), intent(in) :: eijx1, eijy1, eijz1
            double precision, intent(in) :: ecd0, ecd1c, ecd1d, ecd2
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, lc, ld
            integer, intent(in) :: ma, mb, mc, md
            integer, intent(in) :: na, nb, nc, nd

            integer :: pos1, pos2, pos3
            integer :: t, u, v
            double precision :: x, y, z
            double precision :: et, eu, ev
            double precision :: t1, t2, t3, t4

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

                              t1 = ecd0 * rtuv(1 + t, 1 + u, 1 + v)
                              t2 = ecd1c * rtuv(1 + lc + t, 1 + mc + u, 1 + nc + v)
                              t3 = ecd1d * rtuv(1 + ld + t, 1 + md + u, 1 + nd + v)
                              t4 = ecd2 * rtuv(1 + lc + ld + t, 1 + mc + md + u, 1 + nc + nd + v)

                              x = x + et * (t1 - t2 - t3 + t4)
                        end do
                        y = y + eu * x
                  end do
                  z = z + ev * y
            end do

            repint_llpp = z
      end function repint_llpp


      function repint_llppd(eijx1, eijy1, eijz1, ecd0, ecd1, ecd2, rtuv, la, lb, lc, &
            ma, mb, mc, na, nb, nc)
            double precision :: repint_llppd

            double precision, dimension(:), intent(in) :: eijx1, eijy1, eijz1
            double precision, intent(in) :: ecd0, ecd1, ecd2
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, lc
            integer, intent(in) :: ma, mb, mc
            integer, intent(in) :: na, nb, nc

            integer :: pos1, pos2, pos3
            integer :: t, u, v
            double precision :: x, y, z
            double precision :: et, eu, ev
            double precision :: t1, t2, t3

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

                              t1 = ecd0 * rtuv(1 + t, 1 + u, 1 + v)
                              t2 = ecd1 * rtuv(1 + lc + t, 1 + mc + u, 1 + nc + v)
                              t3 = ecd2 * rtuv(1 + lc + lc + t, 1 + mc + mc + u, 1 + nc + nc + v)

                              x = x + et * (t1 - t2 + t3)

                        end do
                        y = y + eu * x
                  end do
                  z = z + ev * y
            end do

            repint_llppd = z
      end function repint_llppd


      subroutine ints2e_llpp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, trans)
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
            logical, intent(in) :: trans

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
            integer, dimension(:), pointer :: llc, lld, mmc, mmd, nnc, nnd
            integer :: i, j, k, l
            integer :: v1, v2, v3, v4, v
            integer :: la, lb, ma, mb, na, nb, lc, ld, mc, md, nc, nd
            integer :: momentum1, momentum2
            integer :: nints1, nints2
            integer, parameter :: momentum3 = 1
            integer, parameter :: momentum4 = 1
            integer, parameter :: nints3 = 3
            integer, parameter :: nints4 = 3
            integer :: nints
            double precision :: norm1, norm2, norm3, norm4, norm12, norm123, norm34, norm134, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            double precision, dimension(max_chidx) :: fmarray

            double precision :: qinv, qinv2
            double precision, dimension(3, 3) :: ecd0
            double precision, dimension(3)    :: ecd1c, ecd1d
            double precision                  :: ecd2
            double precision, dimension(3)    :: ecd0diag
            double precision, dimension(3)    :: ecd1diag
            double precision                  :: ecd2diag

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

                                    call fm(momentum1 + momentum2 + momentum3 + momentum4, &
                                          alpha * dot_product(rpq, rpq), fmarray)
                                    call chints(momentum1 + momentum2 + momentum3 + momentum4, &
                                          fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1

                                    if (trans .eqv. .false.) then
                                          laloop: do v1 = 1, nints1
                                                la = lla(v1)
                                                ma = mma(v1)
                                                na = nna(v1)
                                                norm1 = const1 * nrml(v1, i, shell1)

                                                lbloop1: do v2 = 1, nints2
                                                      lb = llb(v2)
                                                      mb = mmb(v2)
                                                      nb = nnb(v2)
                                                      norm2 = nrml(v2, j, shell2)
                                                      norm12 = norm1 * norm2

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
                                                                              * repint_llpp(eijx1, eijy1, eijz1, &
                                                                              ecd0(v3, v4), ecd1c(v4), ecd1d(v3), ecd2, &
                                                                              rtuv, la, lb, lc, ld, &
                                                                              ma, mb, mc, md, na, nb, nc, nd)
                                                                  else
                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_llppd(eijx1, eijy1, eijz1, &
                                                                              ecd0diag(v3), ecd1diag(v3), ecd2diag, rtuv, &
                                                                              la, lb, lc, &
                                                                              ma, mb, mc, na, nb, nc)
                                                                  end if
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                end do lbloop1
                                          end do laloop
                                    else
                                          !
                                          ! If .TRANSPOSE. .EQV. .TRUE.
                                          !
                                          do v3 = 1, 3
                                                lc = llc(v3)
                                                mc = mmc(v3)
                                                nc = nnc(v3)
                                                norm3 = const1 * nrml(v3, k, shell3)

                                                do v4 = 1, 3
                                                      ld = lld(v4)
                                                      md = mmd(v4)
                                                      nd = nnd(v4)
                                                      norm4 = nrml(v4, l, shell4)
                                                      norm34 = norm3 * norm4

                                                      if (v4 .ne. v3) then
                                                            do v1 = 1, nints1
                                                                  la = lla(v1)
                                                                  ma = mma(v1)
                                                                  na = nna(v1)
                                                                  norm1 = nrml(v1, i, shell1)
                                                                  norm134 = norm1 * norm34

                                                                  do v2 = 1, nints2
                                                                        lb = llb(v2)
                                                                        mb = mmb(v2)
                                                                        nb = nnb(v2)
                                                                        norm2 = nrml(v2, j, shell2)
                                                                        norm = norm2 * norm134

                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_llpp(eijx1, eijy1, eijz1, &
                                                                              ecd0(v3, v4), ecd1c(v4), ecd1d(v3), ecd2, &
                                                                              rtuv, la, lb, lc, ld, &
                                                                              ma, mb, mc, md, na, nb, nc, nd)

                                                                        v = v + 1
                                                                  end do
                                                            end do
                                                      else
                                                            do v1 = 1, nints1
                                                                  la = lla(v1)
                                                                  ma = mma(v1)
                                                                  na = nna(v1)
                                                                  norm1 = nrml(v1, i, shell1)
                                                                  norm134 = norm1 * norm34

                                                                  do v2 = 1, nints2
                                                                        lb = llb(v2)
                                                                        mb = mmb(v2)
                                                                        nb = nnb(v2)
                                                                        norm2 = nrml(v2, j, shell2)
                                                                        norm = norm2 * norm134

                                                                        gabcd(v) = gabcd(v) + norm &
                                                                              * repint_llppd(eijx1, eijy1, eijz1, &
                                                                              ecd0diag(v3), ecd1diag(v3), ecd2diag, rtuv, &
                                                                              la, lb, lc, &
                                                                              ma, mb, mc, na, nb, nc)

                                                                        v = v + 1
                                                                  end do
                                                            end do
                                                      end if
                                                end do
                                          end do
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_llpp
end module llpp
