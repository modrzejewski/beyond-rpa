module ints
      use math_constants
      use arithmetic
      use gparam
      use ericonst
      use boys
      use hermite
      use ecpint
      !
      ! SPECIFIC INTEGRAL SUBROUTINES
      !
      use ssss ! 1
      use spss ! 2
      use spsp ! 3
      use ppss ! 4
      use ppsp ! 5
      use pppp ! 6
      use llpp ! 7
      use llsp ! 8
      use llss ! 9
      use dsss ! 10
      use ddss ! 11
      use dsds ! 12
      use dppp ! 13
      use dpss ! 14
      use dspp ! 15
      use ddps ! 16
      use dsps ! 17
      use dpps ! 18
      use dpds ! 19
      use ddds ! 20
      use fsss ! 21
      use fsps ! 22
      use fsds ! 23
      use fpss ! 24
      use fdss ! 25
      use fpps ! 26
      use fspp ! 27
      use llll ! 28

      implicit none

contains

      function kinint(eijx, eijy, eijz, sab, xy, yz, zx, la, lb, ma, mb, na, nb, betaj)
            !
            ! Calculate kinetic energy integral between unnormalized gauss orbitals.
            ! J.T. Ferman, E.F. Valeev, Fundamentals of Molecular Integral Evaluation, eq. 4.8
            !
            double precision :: kinint

            double precision, dimension(:), intent(in) :: eijx, eijy, eijz
            integer, intent(in) :: la, lb, ma, mb, na, nb
            double precision, intent(in) :: betaj
            double precision, intent(in) :: sab, xy, yz, zx

            double precision :: ix, iy, iz
            double precision :: const1, int0, int1
            double precision :: sab2
            double precision :: dlb, dmb, dnb
            !
            ! Eq. 4.8
            !
            ! T_{12} = I_x + I_y + I_z
            ! <+2|_x = x^{l + 2} y^m z^n \exp(-\alpha r^2)
            ! I_x = \alpha_2 * (2 * l2 + 1) <0|0> - 2 \alpha_2^2 <0|+2>_x - l2(l2-1)/2 <0|-2>_x
            !
            dlb = dble(lb)
            dmb = dble(mb)
            dnb = dble(nb)
            const1 = two * betaj**2
            sab2 = betaj * sab

            if (lb .ge. 2) then
                  int0 = eijx(eijmatrix_position(la + lb - 2, lb - 2, 0)) * yz
                  int1 = eijx(eijmatrix_position(la + lb + 2, lb + 2, 0)) * yz
                  ix = (two * dlb + one) * sab2 - const1 * int1 - dlb * (dlb - one) / two * int0
            else
                  int1 = eijx(eijmatrix_position(la + lb + 2, lb + 2, 0)) * yz
                  ix = (two * dlb + one) * sab2 - const1 * int1
            end if

            if (mb .ge. 2) then
                  int0 = eijy(eijmatrix_position(ma + mb - 2, mb - 2, 0)) * zx
                  int1 = eijy(eijmatrix_position(ma + mb + 2, mb + 2, 0)) * zx
                  iy = (two * dmb + one) * sab2 - const1 * int1 - dmb * (dmb - one) / two * int0
            else
                  int1 = eijy(eijmatrix_position(ma + mb + 2, mb + 2, 0)) * zx
                  iy = (two * dmb + one) * sab2 - const1 * int1
            end if

            if (nb .ge. 2) then
                  int0 = eijz(eijmatrix_position(na + nb - 2, nb - 2, 0)) * xy
                  int1 = eijz(eijmatrix_position(na + nb + 2, nb + 2, 0)) * xy
                  iz = (two * dnb + one) * sab2 - const1 * int1 - dnb * (dnb - one) / two * int0
            else
                  int1 = eijz(eijmatrix_position(na + nb + 2, nb + 2, 0)) * xy
                  iz = (two * dnb + one) * sab2 - const1 * int1
            end if

            kinint = ix + iy + iz
      end function kinint


      function attrint(eijx, eijy, eijz, rtuv, la, lb, ma, mb, na, nb)
            double precision :: attrint

            double precision, dimension(:), intent(in) :: eijx, eijy, eijz
            double precision, dimension(:, :, :) :: rtuv
            integer, intent(in) :: la, lb, ma, mb, na, nb

            integer :: t, u, v, pos1, pos2, pos3
            double precision :: x, y, z
            double precision :: et, eu, ev
            double precision :: r

            x = zero
            pos1 = eijmatrix_position(la + lb, lb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(na + nb, nb, 0)

            do t = 0, la + lb
                  et = eijx(pos1 + t)
                  y = zero
                  do u = 0, ma + mb
                        eu = eijy(pos2 + u)
                        z = zero
                        do v = 0, na + nb
                              ev = eijz(pos3 + v)
                              r = rtuv(t + 1, u + 1, v + 1)
                              z = z + ev * r
                        end do
                        y = y + eu * z
                  end do
                  x = x + et * y
            end do

            attrint = x
      end function attrint


      subroutine ints1e(shell1, a, shell2, b, overlap, kinetic, attraction)
            !
            ! **********************************************************
            ! DESCRIPTION:
            ! **********************************************************
            !
            ! Calculate one-electron integrals
            !
            ! **********************************************************
            ! REFERENCES:
            ! **********************************************************
            !
            ! 1. Overlap integral - T. Helgaker, Molecular
            !    Electronic-Structure Theory, eq. 9.5.41
            !
            ! 2. Nuclear attraction integral - T. Helgaker, Molecular
            !    Electronic-Structure Theory, eq. 9.9.32
            !
            ! 3. Kinetic energy operator integral -  J.T. Ferman,
            !    E.F. Valeev, Fundamentals of Molecular Integral
            !    Evaluation, eq. 4.8
            !
            ! **********************************************************
            ! INPUTS:
            ! **********************************************************
            !
            ! SHELL1,    - Indices of orbitals' shells
            ! SHELL2
            !
            ! A, B       - Indices of atoms
            !
            ! OVERLAP    - Array of dimension (1:n), n >= NINTS
            !              in which overlap integrals are stored
            !              as an ouput of the subroutine
            !
            ! KINETIC    - Array of dimension (1:n), n >= NINTS
            !              in  which kinetic energy operator
            !              integrals are stored as an output of the
            !              subroutine
            !
            ! ATTRACTION - Array of dimension (1:n), n >= NINTS
            !              in which Coulombic attraction integrals
            !              are stored as an output of the subroutine
            !
            !
            !
            integer, intent(in) :: shell1, shell2
            integer, intent(in) :: a, b
            double precision, dimension(:), intent(out) :: overlap, kinetic, attraction

            double precision :: alphai, betaj, gamma
            double precision, dimension(3) :: ara, brb, p, pa, pb
            double precision :: xabsq, yabsq, zabsq
            double precision :: alpha_reduced

            ! Maximum value of a single upper index of E^{ij}_t
            integer, parameter :: max_index = 2 * max_l + 2

            ! Dimension = \sum_{k = 0}^{max_index} (k + 1)^2

            double precision, &
                  dimension((2 * max_index**3 + 9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx, eijy, eijz
            double precision :: coeffx, coeffy, coeffz
            integer :: i, j, k, l, v1, v2, v, w
            integer :: la, lb, ma, mb, na, nb
            double precision :: const1, const2, sab, xy, yz, zx
            integer :: momentum1, momentum2, momentum
            double precision :: norm1, norm2, norm
            integer :: nints1, nints2, nints
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            double precision, dimension(max_chidx) :: fmarray
            double precision, dimension(3) :: rc, rpc
            integer :: NAtomicCharges, NPointCharges
            real(F64) :: qc

            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum = momentum1 + momentum2

            associate ( &
                  lla => ll(:, momentum1), &
                  mma => mm(:, momentum1), &
                  nna => nn(:, momentum1), &
                  llb => ll(:, momentum2), &
                  mmb => mm(:, momentum2), &
                  nnb => nn(:, momentum2), &
                  ra => atomr(:, a), &
                  rb => atomr(:, b))

            xabsq = (ra(1) - rb(1))**2
            yabsq = (ra(2) - rb(2))**2
            zabsq = (ra(3) - rb(3))**2

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints = nints1 * nints2

            overlap(1:nints) = zero
            kinetic(1:nints) = zero
            attraction(1:nints) = zero

            do i = 1, nprm(shell1)

                  alphai = expn(i, shell1)
                  ara = alphai * ra

                  do j = 1, nprm(shell2)
                        betaj = expn(j, shell2)
                        brb = betaj * rb
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
                        call eijmatrix(momentum + 2, one, gamma, pa(1), pb(1), eijx)
                        call eijmatrix(momentum + 2, one, gamma, pa(2), pb(2), eijy)
                        call eijmatrix(momentum + 2, one, gamma, pa(3), pb(3), eijz)
                        !
                        ! Loop over non-dummy atoms
                        !
                        NAtomicCharges = 0
                        do k = 1, 2
                              NAtomicCharges = NAtomicCharges + REAL_ATOMS(2, k) - REAL_ATOMS(1, k) + 1
                        end do
                        NPointCharges = POINT_CHARGES_N
                        w = 0
                        do k = 1, 2
                              do l = REAL_ATOMS(1, k), REAL_ATOMS(2, k)
                                    w = w + 1
                                    if (w <= NAtomicCharges) then
                                          rc = atomr(:, l)
                                          qc = real(ECP_INUCLZ(l), F64)
                                    else
                                          rc = POINT_CHARGES_R(:, w-NAtomicCharges)
                                          qc = POINT_CHARGES_Q(w-NAtomicCharges)
                                    end if

                                    rpc = p - rc
                                    call fm(momentum, gamma * dot_product(rpc, rpc), fmarray)
                                    call chints(momentum, fmarray, rtuv, rpc(1), rpc(2), rpc(3), gamma)

                                    v = 1
                                    do v1 = 1, nints1

                                          la = lla(v1)
                                          ma = mma(v1)
                                          na = nna(v1)
                                          norm1 = -const2 * qc * nrml(v1, i, shell1)

                                          do v2 = 1, nints2
                                                lb = llb(v2)
                                                mb = mmb(v2)
                                                nb = nnb(v2)
                                                norm2 = nrml(v2, j, shell2)
                                                norm = norm1 * norm2

                                                attraction(v) = attraction(v) + norm * attrint(eijx, eijy, eijz, &
                                                      rtuv, la, lb, ma, mb, na, nb)

                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do

                        v = 1
                        do v1 = 1, nints1

                              la = lla(v1)
                              ma = mma(v1)
                              na = nna(v1)

                              norm1 = const1 * nrml(v1, i, shell1)

                              do v2 = 1, nints2

                                    lb = llb(v2)
                                    mb = mmb(v2)
                                    nb = nnb(v2)

                                    norm2 = nrml(v2, j, shell2)

                                    coeffx = eijx(eijmatrix_position(la + lb, lb, 0))
                                    coeffy = eijy(eijmatrix_position(ma + mb, mb, 0))
                                    coeffz = eijz(eijmatrix_position(na + nb, nb, 0))

                                    xy = coeffx * coeffy
                                    yz = coeffy * coeffz
                                    zx = coeffz * coeffx

                                    sab = xy * coeffz

                                    norm = norm1 * norm2
                                    overlap(v) = overlap(v) + norm * sab
                                    kinetic(v) = kinetic(v) + norm * kinint(eijx, eijy, eijz, sab, &
                                          xy, yz, zx, la, lb, ma, mb, na, nb, betaj)
                                    v = v + 1
                              end do
                        end do
                  end do
            end do
      end associate
      end subroutine ints1e


      subroutine ints2e(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd

            integer :: m1, m2, m3, m4
            integer :: brasum, ketsum, lsum
            integer :: permutation, canonical

            m1 = shtype(shell1)
            m2 = shtype(shell2)
            m3 = shtype(shell3)
            m4 = shtype(shell4)

            call eritype(permutation, canonical, m1, m2, m3, m4)

            brasum = m1 + m2
            ketsum = m3 + m4
            lsum = brasum + ketsum

            select case (canonical)
            case (ERI_C_SSSS)
                  call ints2e_ssss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            case (ERI_C_PSSS)
                  call ints2e_spss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_DSSS)
                  call ints2e_dsss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_PPSS)
                  call ints2e_ppss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_PPPS)
                  call ints2e_ppsp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_PSPS)
                  call ints2e_spsp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_PPPP)
                  call ints2e_pppp(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            case (ERI_C_DSPS)
                  call ints2e_dsps_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_DDSS)
                  call ints2e_ddss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_DSDS)
                  call ints2e_dsds_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_DPPP)
                  call ints2e_dppp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_DPSS)
                  call ints2e_dpss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_DPPS)
                  call ints2e_dpps_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_DPDS)
                  call ints2e_dpds_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_DSPP)
                  call ints2e_dspp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_DDDS)
                  call ints2e_ddds_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_DDPS)
                  call ints2e_ddps_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_FSSS)
                  call ints2e_fsss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_FSPS)
                  call ints2e_fsps_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_FSDS)
                  call ints2e_fsds_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_FPSS)
                  call ints2e_fpss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_FDSS)
                  call ints2e_fdss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_FPPS)
                  call ints2e_fpps_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            case (ERI_C_FSPP)
                  call ints2e_fspp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            case (ERI_C_LLSS, ERI_C_LDSS, ERI_C_LPSS, ERI_C_LSSS)
                  call ints2e_llss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, ketsum)
            case (ERI_C_LLPS, ERI_C_LDPS, ERI_C_LPPS, ERI_C_LSPS)
                  call ints2e_llsp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2 ,m3, m4)
            case (ERI_C_LLPP, ERI_C_LDPP, ERI_C_LPPP, ERI_C_LSPP, ERI_C_DDPP)
                  call ints2e_llpp_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, brasum)
            case default
                  call ints2e_llll(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
            end select
      end subroutine ints2e
end module ints
