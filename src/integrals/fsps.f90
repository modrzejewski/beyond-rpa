! File generated automatically. 2012-05-22 23:38:16 UTC

module fsps
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite
      use hermite_automatic
      use lcexch

      implicit none

contains
    

      subroutine ints2e_fsps_perm(shell1, a, shell2, b, shell3, c, shell4, d, &
           gabcd, m1, m2, m3, m4)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: m1, m2, m3, m4
            
            integer :: brasum
            integer, dimension(0:3) :: a1, s1
            integer, dimension(0:1) :: a2, s2
            
            brasum = m1 + m2

            if (brasum .eq. 3) then
                a1(m1) = a
                s1(m1) = shell1
                a1(m2) = b
                s1(m2) = shell2
                
                a2(m3) = c
                s2(m3) = shell3
                a2(m4) = d
                s2(m4) = shell4
                
                call ints2e_fsps(s1(3), a1(3), s1(0), a1(0), &
                    s2(1), a2(1), s2(0), a2(0), gabcd, 3, 3, 1, 1)
            else
                a1(m3) = c
                s1(m3) = shell3
                a1(m4) = d
                s1(m4) = shell4
            
                a2(m1) = a
                s2(m1) = shell1
                a2(m2) = b
                s2(m2) = shell2
    
                call ints2e_fsps(s1(3), a1(3), s1(0), a1(0), &
                    s2(1), a2(1), s2(0), a2(0), gabcd, 1, 1, 10, 10)
            end if
      end subroutine ints2e_fsps_perm
    

    subroutine ints2e_fsps(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, ka, kb, kc, kd)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: ka, kb, kc, kd

            integer :: idx
        
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

            integer, parameter :: momentum1 = 3
            integer, parameter :: momentum2 = 0
            integer, parameter :: momentum3 = 1
            integer, parameter :: momentum4 = 0
            integer, parameter :: nints1 = 10
            integer, parameter :: nints2 = 1
            integer, parameter :: nints3 = 3
            integer, parameter :: nints4 = 1
            integer, parameter :: nints = nints1 * nints2 * nints3 * nints4
            
            double precision :: norm12, norm123
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(5, 5, 5) :: rtuv
            double precision, dimension(0:1, 0:1, 0:1) :: gabtuv
            double precision, dimension(5) :: fmarray

            double precision :: euv, cdsum
            double precision, dimension(8) ::  etuv
            double precision, dimension(6) :: etuv2

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
etuv2(1) = eijx2(2) * euv
etuv2(2) = eijx2(3) * euv
euv = eijy2(2) * eijz2(1)
etuv2(3) = eijx2(1) * euv
euv = eijy2(3) * eijz2(1)
etuv2(4) = eijx2(1) * euv
euv = eijy2(1) * eijz2(2)
etuv2(5) = eijx2(1) * euv
euv = eijy2(1) * eijz2(3)
etuv2(6) = eijx2(1) * euv
norm12 = nrml(1, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(1)
etuv(1) = eijx1(15) * euv
etuv(2) = eijx1(16) * euv
etuv(3) = eijx1(17) * euv
etuv(4) = eijx1(18) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(3, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(4, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(4, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(5, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(3, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(4, 2, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(3, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(4, 1, 2)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 0 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 0 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 0 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(2, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(2) * eijz1(1)
etuv(1) = eijx1(6) * euv
etuv(2) = eijx1(7) * euv
etuv(3) = eijx1(8) * euv
euv = eijy1(3) * eijz1(1)
etuv(4) = eijx1(6) * euv
etuv(5) = eijx1(7) * euv
etuv(6) = eijx1(8) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(3, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(4, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(3, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(3, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(3, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(4, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(2, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(3, 3, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(3, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(2, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(3, 2, 2)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 1 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 1 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 1 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(3, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(2)
etuv(1) = eijx1(6) * euv
etuv(2) = eijx1(7) * euv
etuv(3) = eijx1(8) * euv
euv = eijy1(1) * eijz1(3)
etuv(4) = eijx1(6) * euv
etuv(5) = eijx1(7) * euv
etuv(6) = eijx1(8) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(3, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(4, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(3, 2, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(3, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(3, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(3, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(4, 1, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(2, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(3, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(2, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(3, 1, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 2 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 2 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 2 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(4, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(6) * eijz1(1)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(7) * eijz1(1)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
euv = eijy1(8) * eijz1(1)
etuv(5) = eijx1(2) * euv
etuv(6) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(3, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 3, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(1, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(2, 3, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(2, 3, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(2, 3, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(3, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(1, 4, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(2, 4, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(2, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(1, 3, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(2, 3, 2)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 3 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 3 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 3 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(5, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(2) * eijz1(2)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(3) * eijz1(2)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
euv = eijy1(2) * eijz1(3)
etuv(5) = eijx1(2) * euv
etuv(6) = eijx1(3) * euv
euv = eijy1(3) * eijz1(3)
etuv(7) = eijx1(2) * euv
etuv(8) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(3, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(2, 3, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(2, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(3, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(1, 2, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(7) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(2, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(2, 2, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(7) * rtuv(2, 2, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(8) * rtuv(2, 2, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(8) * rtuv(3, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(7) * rtuv(1, 3, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(8) * rtuv(2, 3, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(2, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(7) * rtuv(1, 2, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(8) * rtuv(2, 2, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 4 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 4 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 4 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(6, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(6)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(1) * eijz1(7)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
euv = eijy1(1) * eijz1(8)
etuv(5) = eijx1(2) * euv
etuv(6) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(3, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(3, 1, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(2, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 1, 3)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(2, 1, 3)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(2, 1, 3)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(2, 1, 3)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(3, 1, 3)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(1, 2, 3)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(2, 2, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(1, 1, 4)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(2, 1, 4)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 5 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 5 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 5 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(7, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(15) * eijz1(1)
etuv(1) = eijx1(1) * euv
euv = eijy1(16) * eijz1(1)
etuv(2) = eijx1(1) * euv
euv = eijy1(17) * eijz1(1)
etuv(3) = eijx1(1) * euv
euv = eijy1(18) * eijz1(1)
etuv(4) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 3, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 3, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 4, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 4, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 4, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 5, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 3, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 4, 2)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 6 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 6 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 6 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(8, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(6) * eijz1(2)
etuv(1) = eijx1(1) * euv
euv = eijy1(7) * eijz1(2)
etuv(2) = eijx1(1) * euv
euv = eijy1(8) * eijz1(2)
etuv(3) = eijx1(1) * euv
euv = eijy1(6) * eijz1(3)
etuv(4) = eijx1(1) * euv
euv = eijy1(7) * eijz1(3)
etuv(5) = eijx1(1) * euv
euv = eijy1(8) * eijz1(3)
etuv(6) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 3, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 3, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 3, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 4, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 2, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(1, 2, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(2, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 3, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(1, 3, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(1, 3, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(2, 3, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(1, 4, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(1, 2, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(1, 3, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 7 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 7 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 7 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(9, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(2) * eijz1(6)
etuv(1) = eijx1(1) * euv
euv = eijy1(3) * eijz1(6)
etuv(2) = eijx1(1) * euv
euv = eijy1(2) * eijz1(7)
etuv(3) = eijx1(1) * euv
euv = eijy1(3) * eijz1(7)
etuv(4) = eijx1(1) * euv
euv = eijy1(2) * eijz1(8)
etuv(5) = eijx1(1) * euv
euv = eijy1(3) * eijz1(8)
etuv(6) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 2, 1)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 3, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 1, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 2, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 2, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 2, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 3, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 1, 3)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(5) * rtuv(1, 1, 3)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(5) * rtuv(2, 1, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 2, 3)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(5) * rtuv(1, 2, 3)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(6) * rtuv(1, 2, 3)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(6) * rtuv(2, 2, 3)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(6) * rtuv(1, 3, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(5) * rtuv(1, 1, 4)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(6) * rtuv(1, 2, 4)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 8 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 8 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 8 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(10, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(15)
etuv(1) = eijx1(1) * euv
euv = eijy1(1) * eijz1(16)
etuv(2) = eijx1(1) * euv
euv = eijy1(1) * eijz1(17)
etuv(3) = eijx1(1) * euv
euv = eijy1(1) * eijz1(18)
etuv(4) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(1, 0, 0) = -etuv(1) * rtuv(2, 1, 1)
gabtuv(0, 1, 0) = -etuv(1) * rtuv(1, 2, 1)
gabtuv(0, 0, 1) = -etuv(1) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 1, 2)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(2) * rtuv(2, 1, 2)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(2) * rtuv(1, 2, 2)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(2) * rtuv(1, 1, 3)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 3)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(3) * rtuv(2, 1, 3)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(3) * rtuv(1, 2, 3)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(3) * rtuv(1, 1, 4)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 1, 4)
gabtuv(1, 0, 0) = gabtuv(1, 0, 0) - etuv(4) * rtuv(2, 1, 4)
gabtuv(0, 1, 0) = gabtuv(0, 1, 0) - etuv(4) * rtuv(1, 2, 4)
gabtuv(0, 0, 1) = gabtuv(0, 0, 1) - etuv(4) * rtuv(1, 1, 5)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(2) * gabtuv(1, 0, 0)
idx = 9 * ka + 0 * kb + 0 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(2, k, shell3)
cdsum = etuv2(3) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(4) * gabtuv(0, 1, 0)
idx = 9 * ka + 0 * kb + 1 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
norm123 = norm12 * nrml(3, k, shell3)
cdsum = etuv2(5) * gabtuv(0, 0, 0)
cdsum = cdsum + etuv2(6) * gabtuv(0, 0, 1)
idx = 9 * ka + 0 * kb + 2 * kc + 0 * kd + 1
gabcd(idx) = gabcd(idx) + norm123 * nrml(1, l, shell4) * cdsum
!
! -------- END OF AUTOMATICALLY GENERATED CODE ---------
!

                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_fsps
end module fsps
    
