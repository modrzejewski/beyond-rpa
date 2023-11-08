module ppsp
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite_automatic
      use hermite
      use lcexch

      implicit none

contains

      subroutine ints2e_ppsp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: permutation

            select case (permutation)
            case (ERI_PPPS)
                  call ints2e_ppsp(shell1, a, shell2, b, shell4, d, shell3, c, gabcd, .false.)
            case (ERI_PSPP)
                  call ints2e_ppsp(shell3, c, shell4, d, shell2, b, shell1, a, gabcd, .true.)
            case (ERI_SPPP)
                  call ints2e_ppsp(shell3, c, shell4, d, shell1, a, shell2, b, gabcd, .true.)
            case default
                  call ints2e_ppsp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, .false.)
            end select
      end subroutine ints2e_ppsp_perm


      function repint_ppsp(eab0, eab1a, eab1b, eab2, ecd0, ecd1, rtuv, la, lb, ld, ma, mb, md, na, nb, nd)
            double precision :: repint_ppsp
            
            double precision, intent(in) :: eab0, eab1a, eab1b, eab2, ecd0, ecd1
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, ld, ma, mb, md, na, nb, nd

            double precision :: t1, t2, t3, t4

            t1 = eab0 * (ecd0 * rtuv(1, 1, 1) - ecd1 * rtuv(1 + ld, 1 + md, 1 + nd))
            t2 = eab1a * (ecd0 * rtuv(1 + la, 1 + ma, 1 + na) - ecd1 * rtuv(1 + la + ld, 1 + ma + md, 1 + na + nd))
            t3 = eab1b * (ecd0 * rtuv(1 + lb, 1 + mb, 1 + nb) - ecd1 * rtuv(1 + lb + ld, 1 + mb + md, 1 + nb + nd))
            t4 = eab2 * (ecd0 * rtuv(1 + la + lb, 1 + ma + mb, 1 + na + nb) &
                  - ecd1 * rtuv(1 + la + lb + ld, 1 + ma + mb + md, 1 + na + nb + nd))

            repint_ppsp = t1 + t2 + t3 + t4
      end function repint_ppsp


      function repint_ppspd(eab0, eab1, eab2, ecd0, ecd1, rtuv, la, ld, ma, md, na, nd)
            double precision :: repint_ppspd
            
            double precision, intent(in) :: eab0, eab1, eab2, ecd0, ecd1
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, ld, ma, md, na, nd

            double precision :: t1, t2, t3

            t1 = eab0 * (ecd0 * rtuv(1, 1, 1) - ecd1 * rtuv(1 + ld, 1 + md, 1 + nd))
            t2 = eab1 * (ecd0 * rtuv(1 + la, 1 + ma, 1 + na) - ecd1 * rtuv(1 + la + ld, 1 + ma + md, 1 + na + nd))
            t3 = eab2 * (ecd0 * rtuv(1 + la + la, 1 + ma + ma, 1 + na + na) &
                  - ecd1 * rtuv(1 + la + la + ld, 1 + ma + ma + md, 1 + na + na + nd))

            repint_ppspd = t1 + t2 + t3
      end function repint_ppspd


      subroutine ints2e_ppsp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, trans)
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

            double precision :: const1
            integer, dimension(:), pointer :: lla, llb, mma, mmb, nna, nnb
            integer, dimension(:), pointer :: lld, mmd, nnd
            integer :: i, j, k, l
            integer :: v1, v2, v3, v
            integer :: la, lb, ma, mb, na, nb, ld, md, nd
            integer, parameter :: momentum1 = 1
            integer, parameter :: momentum2 = 1
            integer, parameter :: momentum4 = 1
            integer, parameter :: nints = 27
            double precision :: norm1, norm2, norm12, norm3, norm4, norm41, norm
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(4, 4, 4) :: rtuv
            double precision, dimension(4) :: fmarray

            double precision :: pinv, pinv2
            double precision, dimension(3, 3) :: eab0
            double precision, dimension(3)    :: eab1a, eab1b
            double precision                  :: eab2
            double precision, dimension(3)    :: eab0d
            double precision, dimension(3)    :: eab1d
            double precision                  :: eab2d
            double precision                  :: ecd0, ecd1

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
                                    ecd1 = one / (two * q)

                                    call f3(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_3(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

                                    v = 1
                                    norm3 = const1 * nrml(1, k, shell3)
                                    
                                    if (trans .eqv. .false.) then
                                          laloop: do v1 = 1, 3
                                                la = lla(v1)
                                                ma = mma(v1)
                                                na = nna(v1)
                                                norm1 = norm3 * nrml(v1, i, shell1)

                                                lbloop1: do v2 = 1, 3
                                                      if (v2 .ne. v1) then
                                                            !
                                                            ! Off-diagonal
                                                            !
                                                            lb = llb(v2)
                                                            mb = mmb(v2)
                                                            nb = nnb(v2)
                                                            norm2 = nrml(v2, j, shell2)
                                                            norm12 = norm1 * norm2

                                                            ldloop1: do v3 = 1, 3
                                                                  ld = lld(v3)
                                                                  md = mmd(v3)
                                                                  nd = nnd(v3)
                                                                  norm4 = nrml(v3, l, shell4)
                                                                  norm = norm12 * norm4
                                                                  ecd0 = rqd(v3)

                                                                  gabcd(v) = gabcd(v) + norm &
                                                                        * repint_ppsp(eab0(v1, v2), eab1a(v2), &
                                                                        eab1b(v1), eab2, ecd0, ecd1, rtuv, &
                                                                        la, lb, ld, ma, mb, md, na, nb, nd)

                                                                  v = v + 1
                                                            end do ldloop1
                                                      else
                                                            !
                                                            ! Diagonal
                                                            !
                                                            lb = llb(v1)
                                                            mb = mmb(v1)
                                                            nb = nnb(v1)
                                                            norm2 = nrml(v1, j, shell2)
                                                            norm12 = norm1 * norm2

                                                            ldloop2: do v3 = 1, 3
                                                                  ld = lld(v3)
                                                                  md = mmd(v3)
                                                                  nd = nnd(v3)
                                                                  norm4 = nrml(v3, l, shell4)
                                                                  norm = norm12 * norm4
                                                                  ecd0 = rqd(v3)

                                                                  gabcd(v) = gabcd(v) + norm &
                                                                        * repint_ppspd(eab0d(v1), eab1d(v1), &
                                                                        eab2d, ecd0, ecd1, rtuv, la, ld, ma, md, na, nd)

                                                                  v = v + 1
                                                            end do ldloop2
                                                      end if
                                                end do lbloop1
                                          end do laloop
                                    else
                                          !
                                          ! REVERT .EQV. .TRUE.
                                          !
                                          ldloop3: do v3 = 1, 3
                                                ld = lld(v3)
                                                md = mmd(v3)
                                                nd = nnd(v3)
                                                norm4 = norm3 * nrml(v3, l, shell4)
                                                ecd0 = rqd(v3)

                                                laloop2: do v1 = 1, 3
                                                      la = lla(v1)
                                                      ma = mma(v1)
                                                      na = nna(v1)
                                                      norm1 = nrml(v1, i, shell1)
                                                      norm41 = norm4 * norm1

                                                      lbloop2: do v2 = 1, 3
                                                            if (v2 .ne. v1) then
                                                                  !
                                                                  ! Off-diagonal
                                                                  !
                                                                  lb = llb(v2)
                                                                  mb = mmb(v2)
                                                                  nb = nnb(v2)
                                                                  norm2 = nrml(v2, j, shell2)
                                                                  norm = norm41 * norm2

                                                                  gabcd(v) = gabcd(v) + norm &
                                                                        * repint_ppsp(eab0(v1, v2), eab1a(v2), &
                                                                        eab1b(v1), eab2, ecd0, ecd1, rtuv, &
                                                                        la, lb, ld, ma, mb, md, na, nb, nd)

                                                                  v = v + 1
                                                            else
                                                                  !
                                                                  ! Diagonal
                                                                  !
                                                                  lb = llb(v1)
                                                                  mb = mmb(v1)
                                                                  nb = nnb(v1)
                                                                  norm2 = nrml(v1, j, shell2)
                                                                  norm = norm41 * norm2

                                                                  gabcd(v) = gabcd(v) + norm &
                                                                        * repint_ppspd(eab0d(v1), eab1d(v1), &
                                                                        eab2d, ecd0, ecd1, rtuv, la, ld, ma, md, na, nd)

                                                                  v = v + 1
                                                            end if
                                                      end do lbloop2
                                                end do laloop2
                                          end do ldloop3
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_ppsp
end module ppsp
