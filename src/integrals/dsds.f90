! File generated automatically. 2012-05-22 23:38:16 UTC

module dsds
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite
      use hermite_automatic
      use lcexch

      implicit none

contains


      subroutine ints2e_dsds_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, m1, m2, m3, m4)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: m1, m2, m3, m4
    
            integer, dimension(0:2) :: s1, a1, s2, a2

            s1(m1) = shell1
            s1(m2) = shell2
            a1(m1) = a
            a1(m2) = b
            s2(m3) = shell3
            s2(m4) = shell4
            a2(m3) = c
            a2(m4) = d
            call ints2e_dsds(s1(2), a1(2), s1(0), a1(0), s2(2), a2(2), s2(0), a2(0), gabcd)
       end subroutine ints2e_dsds_perm
    

    subroutine ints2e_dsds(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
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
                  9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx1, eijy1, eijz1, eijx2, eijy2, eijz2

            double precision :: const1
            integer :: i, j, k, l

            integer, parameter :: momentum1 = 2
            integer, parameter :: momentum2 = 0
            integer, parameter :: momentum3 = 2
            integer, parameter :: momentum4 = 0
            integer, parameter :: nints1 = 6
            integer, parameter :: nints2 = 1
            integer, parameter :: nints3 = 6
            integer, parameter :: nints4 = 1
            integer, parameter :: nints = nints1 * nints2 * nints3 * nints4
            
            double precision :: norm12, norm123
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(5, 5, 5) :: rtuv
            double precision, dimension(0:2, 0:2, 0:2) :: gabtuv
            double precision, dimension(5) :: fmarray

            double precision :: euv, cdsum
            double precision, dimension(4) ::  etuv
            double precision, dimension(21) :: etuv2

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

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

                                    call eijmatrix(momentum3 + momentum4, const1, q, rqc(1), rqd(1), eijx2)
                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(2), rqd(2), eijy2)
                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(3), rqd(3), eijz2)

                                    call f4(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_4(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

    
!
! ------------- AUTOMATICALLY GENERATED CODE -------------
!
euv = eijy2(1) * eijz2(1)
etuv2(1) = eijx2(6) * euv
etuv2(2) = eijx2(7) * euv
etuv2(3) = eijx2(8) * euv
euv = eijy2(2) * eijz2(1)
etuv2(4) = eijx2(2) * euv
etuv2(5) = eijx2(3) * euv
euv = eijy2(3) * eijz2(1)
etuv2(6) = eijx2(2) * euv
etuv2(7) = eijx2(3) * euv
euv = eijy2(1) * eijz2(2)
etuv2(8) = eijx2(2) * euv
etuv2(9) = eijx2(3) * euv
euv = eijy2(1) * eijz2(3)
etuv2(10) = eijx2(2) * euv
etuv2(11) = eijx2(3) * euv
euv = eijy2(6) * eijz2(1)
etuv2(12) = eijx2(1) * euv
euv = eijy2(7) * eijz2(1)
etuv2(13) = eijx2(1) * euv
euv = eijy2(8) * eijz2(1)
etuv2(14) = eijx2(1) * euv
euv = eijy2(2) * eijz2(2)
etuv2(15) = eijx2(1) * euv
euv = eijy2(3) * eijz2(2)
etuv2(16) = eijx2(1) * euv
euv = eijy2(2) * eijz2(3)
etuv2(17) = eijx2(1) * euv
euv = eijy2(3) * eijz2(3)
etuv2(18) = eijx2(1) * euv
euv = eijy2(1) * eijz2(6)
etuv2(19) = eijx2(1) * euv
euv = eijy2(1) * eijz2(7)
etuv2(20) = eijx2(1) * euv
euv = eijy2(1) * eijz2(8)
etuv2(21) = eijx2(1) * euv
norm12 = nrml(1, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(1)
etuv(1) = eijx1(6) * euv
etuv(2) = eijx1(7) * euv
etuv(3) = eijx1(8) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(2, 0, 0) =  etuv(1) * rtuv(3, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(3, 1, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(2) * rtuv(4, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(4, 1, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(3) * rtuv(5, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(1, 1, 0) =  etuv(1) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(2) * rtuv(3, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(3, 2, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(3) * rtuv(4, 2, 1)
gabtuv(0, 2, 0) =  etuv(1) * rtuv(1, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(2) * rtuv(2, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(3) * rtuv(3, 3, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(1, 0, 1) =  etuv(1) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(2) * rtuv(3, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(3, 1, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(3) * rtuv(4, 1, 2)
gabtuv(0, 1, 1) =  etuv(1) * rtuv(1, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(2) * rtuv(2, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(3) * rtuv(3, 2, 2)
gabtuv(0, 0, 2) =  etuv(1) * rtuv(1, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(2) * rtuv(2, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(3) * rtuv(3, 1, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(3) * gabtuv(2, 0, 0)
gabcd(1) = gabcd(1) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(4) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(5) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(7) * gabtuv(1, 1, 0)
gabcd(2) = gabcd(2) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(8) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(9) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(10) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(11) * gabtuv(1, 0, 1)
gabcd(3) = gabcd(3) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(4, k, shell3)
cdsum = etuv2(12) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(13) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(14) * gabtuv(0, 2, 0)
gabcd(4) = gabcd(4) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(5, k, shell3)
cdsum = etuv2(15) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(16) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(17) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(18) * gabtuv(0, 1, 1)
gabcd(5) = gabcd(5) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(6, k, shell3)
cdsum = etuv2(19) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(20) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(21) * gabtuv(0, 0, 2)
gabcd(6) = gabcd(6) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(2, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(2) * eijz1(1)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(3) * eijz1(1)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(2, 0, 0) =  etuv(1) * rtuv(3, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(2) * rtuv(4, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 2, 1)
gabtuv(1, 1, 0) =  etuv(1) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 2, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(2) * rtuv(3, 2, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(3) * rtuv(3, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(3, 2, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(4) * rtuv(4, 2, 1)
gabtuv(0, 2, 0) =  etuv(1) * rtuv(1, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(2) * rtuv(2, 3, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(3) * rtuv(2, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(2, 3, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(4) * rtuv(3, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(3) * rtuv(1, 4, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(4) * rtuv(2, 4, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(1, 0, 1) =  etuv(1) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(2) * rtuv(3, 1, 2)
gabtuv(0, 1, 1) =  etuv(1) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(2) * rtuv(2, 2, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(3) * rtuv(2, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(2, 2, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(4) * rtuv(3, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(3) * rtuv(1, 3, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(4) * rtuv(2, 3, 2)
gabtuv(0, 0, 2) =  etuv(1) * rtuv(1, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(2) * rtuv(2, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(3) * rtuv(1, 2, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(4) * rtuv(2, 2, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(3) * gabtuv(2, 0, 0)
gabcd(7) = gabcd(7) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(4) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(5) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(7) * gabtuv(1, 1, 0)
gabcd(8) = gabcd(8) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(8) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(9) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(10) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(11) * gabtuv(1, 0, 1)
gabcd(9) = gabcd(9) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(4, k, shell3)
cdsum = etuv2(12) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(13) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(14) * gabtuv(0, 2, 0)
gabcd(10) = gabcd(10) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(5, k, shell3)
cdsum = etuv2(15) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(16) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(17) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(18) * gabtuv(0, 1, 1)
gabcd(11) = gabcd(11) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(6, k, shell3)
cdsum = etuv2(19) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(20) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(21) * gabtuv(0, 0, 2)
gabcd(12) = gabcd(12) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(3, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(2)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(1) * eijz1(3)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(2, 0, 0) =  etuv(1) * rtuv(3, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(2) * rtuv(4, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(1, 1, 0) =  etuv(1) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(2) * rtuv(3, 2, 1)
gabtuv(0, 2, 0) =  etuv(1) * rtuv(1, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(2) * rtuv(2, 3, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 2)
gabtuv(1, 0, 1) =  etuv(1) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 1, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(2) * rtuv(3, 1, 2)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(3) * rtuv(3, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(3, 1, 2)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(4) * rtuv(4, 1, 2)
gabtuv(0, 1, 1) =  etuv(1) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(2) * rtuv(2, 2, 2)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(3) * rtuv(2, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(2, 2, 2)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(4) * rtuv(3, 2, 2)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(3) * rtuv(1, 3, 2)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(4) * rtuv(2, 3, 2)
gabtuv(0, 0, 2) =  etuv(1) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(2) * rtuv(2, 1, 3)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(3) * rtuv(2, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(2, 1, 3)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(4) * rtuv(3, 1, 3)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(3) * rtuv(1, 2, 3)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(4) * rtuv(2, 2, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(3) * rtuv(1, 1, 4)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(4) * rtuv(2, 1, 4)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(3) * gabtuv(2, 0, 0)
gabcd(13) = gabcd(13) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(4) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(5) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(7) * gabtuv(1, 1, 0)
gabcd(14) = gabcd(14) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(8) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(9) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(10) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(11) * gabtuv(1, 0, 1)
gabcd(15) = gabcd(15) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(4, k, shell3)
cdsum = etuv2(12) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(13) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(14) * gabtuv(0, 2, 0)
gabcd(16) = gabcd(16) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(5, k, shell3)
cdsum = etuv2(15) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(16) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(17) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(18) * gabtuv(0, 1, 1)
gabcd(17) = gabcd(17) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(6, k, shell3)
cdsum = etuv2(19) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(20) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(21) * gabtuv(0, 0, 2)
gabcd(18) = gabcd(18) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(4, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(6) * eijz1(1)
etuv(1) = eijx1(1) * euv
euv = eijy1(7) * eijz1(1)
etuv(2) = eijx1(1) * euv
euv = eijy1(8) * eijz1(1)
etuv(3) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(2, 0, 0) =  etuv(1) * rtuv(3, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(1, 1, 0) =  etuv(1) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(2) * rtuv(3, 2, 1)
gabtuv(0, 2, 0) =  etuv(1) * rtuv(1, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 3, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 3, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(2) * rtuv(2, 3, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 3, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(3) * rtuv(3, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(2) * rtuv(1, 4, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 4, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(3) * rtuv(2, 4, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(3) * rtuv(1, 5, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(1, 0, 1) =  etuv(1) * rtuv(2, 1, 2)
gabtuv(0, 1, 1) =  etuv(1) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 2, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(2) * rtuv(2, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(2) * rtuv(1, 3, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 3, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(3) * rtuv(2, 3, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(3) * rtuv(1, 4, 2)
gabtuv(0, 0, 2) =  etuv(1) * rtuv(1, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(2) * rtuv(1, 2, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(3) * rtuv(1, 3, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(3) * gabtuv(2, 0, 0)
gabcd(19) = gabcd(19) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(4) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(5) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(7) * gabtuv(1, 1, 0)
gabcd(20) = gabcd(20) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(8) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(9) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(10) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(11) * gabtuv(1, 0, 1)
gabcd(21) = gabcd(21) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(4, k, shell3)
cdsum = etuv2(12) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(13) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(14) * gabtuv(0, 2, 0)
gabcd(22) = gabcd(22) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(5, k, shell3)
cdsum = etuv2(15) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(16) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(17) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(18) * gabtuv(0, 1, 1)
gabcd(23) = gabcd(23) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(6, k, shell3)
cdsum = etuv2(19) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(20) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(21) * gabtuv(0, 0, 2)
gabcd(24) = gabcd(24) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(5, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(2) * eijz1(2)
etuv(1) = eijx1(1) * euv
euv = eijy1(3) * eijz1(2)
etuv(2) = eijx1(1) * euv
euv = eijy1(2) * eijz1(3)
etuv(3) = eijx1(1) * euv
euv = eijy1(3) * eijz1(3)
etuv(4) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(2, 0, 0) =  etuv(1) * rtuv(3, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(1, 1, 0) =  etuv(1) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(2) * rtuv(3, 2, 1)
gabtuv(0, 2, 0) =  etuv(1) * rtuv(1, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 3, 1)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(2) * rtuv(2, 3, 1)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(2) * rtuv(1, 4, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 2)
gabtuv(1, 0, 1) =  etuv(1) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 1, 2)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(3) * rtuv(3, 1, 2)
gabtuv(0, 1, 1) =  etuv(1) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 2, 2)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(2) * rtuv(2, 2, 2)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(3) * rtuv(2, 2, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 2, 2)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(4) * rtuv(3, 2, 2)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(2) * rtuv(1, 3, 2)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(3) * rtuv(1, 3, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 3, 2)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(4) * rtuv(2, 3, 2)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(4) * rtuv(1, 4, 2)
gabtuv(0, 0, 2) =  etuv(1) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 1, 3)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(3) * rtuv(2, 1, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(2) * rtuv(1, 2, 3)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(3) * rtuv(1, 2, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 2, 3)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(4) * rtuv(2, 2, 3)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(4) * rtuv(1, 3, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(3) * rtuv(1, 1, 4)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(4) * rtuv(1, 2, 4)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(3) * gabtuv(2, 0, 0)
gabcd(25) = gabcd(25) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(4) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(5) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(7) * gabtuv(1, 1, 0)
gabcd(26) = gabcd(26) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(8) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(9) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(10) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(11) * gabtuv(1, 0, 1)
gabcd(27) = gabcd(27) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(4, k, shell3)
cdsum = etuv2(12) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(13) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(14) * gabtuv(0, 2, 0)
gabcd(28) = gabcd(28) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(5, k, shell3)
cdsum = etuv2(15) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(16) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(17) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(18) * gabtuv(0, 1, 1)
gabcd(29) = gabcd(29) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(6, k, shell3)
cdsum = etuv2(19) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(20) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(21) * gabtuv(0, 0, 2)
gabcd(30) = gabcd(30) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(6, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(6)
etuv(1) = eijx1(1) * euv
euv = eijy1(1) * eijz1(7)
etuv(2) = eijx1(1) * euv
euv = eijy1(1) * eijz1(8)
etuv(3) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(2, 0, 0) =  etuv(1) * rtuv(3, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(1, 1, 0) =  etuv(1) * rtuv(2, 2, 1)
gabtuv(0, 2, 0) =  etuv(1) * rtuv(1, 3, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 1, 2)
gabtuv(1, 0, 1) =  etuv(1) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 1, 2)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(2) * rtuv(3, 1, 2)
gabtuv(0, 1, 1) =  etuv(1) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 2, 2)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(2) * rtuv(2, 2, 2)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(2) * rtuv(1, 3, 2)
gabtuv(0, 0, 2) =  etuv(1) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 1, 3)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 3)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(2) * rtuv(2, 1, 3)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 1, 3)
gabtuv(2, 0, 0) = gabtuv(2, 0, 0) + etuv(3) * rtuv(3, 1, 3)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(2) * rtuv(1, 2, 3)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 2, 3)
gabtuv(1, 1, 0) = gabtuv(1, 1, 0) + etuv(3) * rtuv(2, 2, 3)
gabtuv(0, 2, 0) = gabtuv(0, 2, 0) + etuv(3) * rtuv(1, 3, 3)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(2) * rtuv(1, 1, 4)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 1, 4)
gabtuv(1, 0, 1) = gabtuv(1, 0, 1) + etuv(3) * rtuv(2, 1, 4)
gabtuv(0, 1, 1) = gabtuv(0, 1, 1) + etuv(3) * rtuv(1, 2, 4)
gabtuv(0, 0, 2) = gabtuv(0, 0, 2) + etuv(3) * rtuv(1, 1, 5)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(3) * gabtuv(2, 0, 0)
gabcd(31) = gabcd(31) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(4) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(5) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(7) * gabtuv(1, 1, 0)
gabcd(32) = gabcd(32) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(8) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(9) * gabtuv(1, 0, 0)
cdsum = cdsum + etuv2(10) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(11) * gabtuv(1, 0, 1)
gabcd(33) = gabcd(33) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(4, k, shell3)
cdsum = etuv2(12) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(13) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(14) * gabtuv(0, 2, 0)
gabcd(34) = gabcd(34) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(5, k, shell3)
cdsum = etuv2(15) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(16) * gabtuv(0, 1, 0)
cdsum = cdsum + etuv2(17) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(18) * gabtuv(0, 1, 1)
gabcd(35) = gabcd(35) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(6, k, shell3)
cdsum = etuv2(19) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(20) * gabtuv(0, 0, 1)
cdsum = cdsum + etuv2(21) * gabtuv(0, 0, 2)
gabcd(36) = gabcd(36) + norm123 * nrml(1, l, shell4) * cdsum
!
! -------- END OF AUTOMATICALLY GENERATED CODE ---------
!

                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_dsds
end module dsds
    
