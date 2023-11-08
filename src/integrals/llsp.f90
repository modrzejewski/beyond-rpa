module llsp
      use math_constants
      use ericonst
      use gto
      use boys
      use hermite
      use lcexch

      implicit none

contains

      subroutine ints2e_llsp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: m1, m2, m3, m4
            
            integer :: ketsum
            integer, dimension(0:1) :: shell, atom

            ketsum = m3 + m4

            if (ketsum .eq. 1) then
                  shell(m3) = shell3
                  atom(m3) = c
                  shell(m4) = shell4
                  atom(m4) = d
                  call ints2e_llsp(shell1, a, shell2, b, shell(0), atom(0), shell(1), atom(1), gabcd, .false.)
            else
                  shell(m1) = shell1
                  atom(m1) = a
                  shell(m2) = shell2
                  atom(m2) = b
                  call ints2e_llsp(shell3, c, shell4, d, shell(0), atom(0), shell(1), atom(1), gabcd, .true.)
            end if


!            select case (permutation)
!            case (ERI_LLPS)
!                  call ints2e_llsp(shell1, a, shell2, b, shell4, d, shell3, c, gabcd, .false.)
!            case (ERI_SPLL)
!                  call ints2e_llsp(shell3, c, shell4, d, shell1, a, shell2, b, gabcd, .true.)
!            case (ERI_PSLL)
!                  call ints2e_llsp(shell3, c, shell4, d, shell2, b, shell1, a, gabcd, .true.)
!            case default
!                  call ints2e_llsp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, .false.)
!            end select

      end subroutine ints2e_llsp_perm


      function repint_llsp(ecd0, ecd1, eijx1, eijy1, eijz1, rtuv, la, &
            lb, ld, ma, mb, md, na, nb, nd)
            double precision :: repint_llsp

            double precision, intent(in) :: ecd0, ecd1
            double precision, dimension(:), intent(in) :: eijx1, eijy1, eijz1
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, ld, ma, mb, md, na, nb, nd

            integer :: pos1, pos2, pos3
            integer :: t, u, v
            double precision :: x, y, z
            double precision :: w
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
                              w = ecd0 * rtuv(t + 1, u + 1, v + 1) &
                                    - ecd1 * rtuv(t + ld + 1, u + md + 1, v + nd + 1)
                              x = x + et * w
                        end do
                        y = y + eu * x
                  end do
                  z = z + ev * y
            end do

            repint_llsp = z
      end function repint_llsp


      subroutine ints2e_llsp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, trans)
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
            integer, dimension(:), pointer :: lld, mmd, nnd
            integer :: i, j, k, l
            integer :: v1, v2, v4, v
            integer :: la, lb, ma, mb, na, nb, ld, md, nd
            integer, parameter :: momentum3 = 0
            integer, parameter :: momentum4 = 1
            integer, parameter :: nints3 = 1
            integer, parameter :: nints4 = 3
            integer :: momentum1, momentum2
            integer :: nints1, nints2, nints
            double precision :: norm1, norm2, norm12, norm3, norm4, norm41, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            double precision, dimension(max_chidx) :: fmarray
            double precision :: ecd0, ecd1

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
            lld => ll(:, momentum4)

            mma => mm(:, momentum1)
            mmb => mm(:, momentum2)
            mmd => mm(:, momentum4)

            nna => nn(:, momentum1)
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
                                    ecd1 = one / (two * q)

                                    call fm(momentum1 + momentum2 + momentum3 + momentum4, &
                                          alpha * dot_product(rpq, rpq), fmarray)
                                    call chints(momentum1 + momentum2 + momentum3 + momentum4, &
                                          fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1
                                    norm3 = const1 * nrml(1, k, shell3)

                                    if (trans .eqv. .false.) then
                                          laloop: do v1 = 1, nints1
                                                la = lla(v1)
                                                ma = mma(v1)
                                                na = nna(v1)
                                                norm1 = norm3 * nrml(v1, i, shell1)

                                                lbloop: do v2 = 1, nints2

                                                      lb = llb(v2)
                                                      mb = mmb(v2)
                                                      nb = nnb(v2)
                                                      norm2 = nrml(v2, j, shell2)
                                                      norm12 = norm1 * norm2

                                                      ploop: do v4 = 1, nints4
                                                            ld = lld(v4)
                                                            md = mmd(v4)
                                                            nd = nnd(v4)
                                                            norm4 = nrml(v4, l, shell4)
                                                            norm = norm12 * norm4
                                                            ecd0 = rqd(v4)

                                                            gabcd(v) = gabcd(v) + norm &
                                                                  * repint_llsp(ecd0, ecd1, &
                                                                  eijx1, eijy1, eijz1, rtuv,&
                                                                  la, lb, ld, ma, mb, md, na, nb, nd)

                                                            v = v + 1
                                                      end do ploop
                                                end do lbloop
                                          end do laloop
                                    else
                                          ploop2: do v4 = 1, nints4
                                                ld = lld(v4)
                                                md = mmd(v4)
                                                nd = nnd(v4)
                                                norm4 = norm3 * nrml(v4, l, shell4)
                                                ecd0 = rqd(v4)

                                                laloop2: do v1 = 1, nints1
                                                      la = lla(v1)
                                                      ma = mma(v1)
                                                      na = nna(v1)
                                                      norm1 = nrml(v1, i, shell1)
                                                      norm41 = norm4 * norm1

                                                      lbloop2: do v2 = 1, nints2
                                                            lb = llb(v2)
                                                            mb = mmb(v2)
                                                            nb = nnb(v2)
                                                            norm2 = nrml(v2, j, shell2)
                                                            norm = norm41 * norm2

                                                            gabcd(v) = gabcd(v) + norm &
                                                                  * repint_llsp(ecd0, ecd1, &
                                                                  eijx1, eijy1, eijz1, rtuv,&
                                                                  la, lb, ld, ma, mb, md, na, nb, nd)

                                                            v = v + 1
                                                      end do lbloop2
                                                end do laloop2
                                          end do ploop2
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_llsp
end module llsp
