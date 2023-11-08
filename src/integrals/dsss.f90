! File generated automatically. 2012-05-22 23:38:16 UTC

module dsss
      use math_constants
      use ericonst
      use gparam
      use boys
      use hermite
      use hermite_automatic
      use lcexch

      implicit none

contains
    

      subroutine ints2e_dsss_perm(shell1, a, shell2, b, shell3, c, shell4, d, gabcd, permutation)
            integer, intent(in) :: a, b, c, d
            integer, intent(in) :: shell1, shell2, shell3, shell4
            double precision, dimension(:), intent(out) :: gabcd
            integer, intent(in) :: permutation

            select case (permutation)
            case (ERI_SDSS)
                call ints2e_dsss(shell2, b, shell1, a, shell3, c, shell4, d, gabcd)
            case (ERI_SSDS)
                call ints2e_dsss(shell3, c, shell4, d, shell1, a, shell2, b, gabcd)
            case (ERI_SSSD)
                call ints2e_dsss(shell4, d, shell3, c, shell1, a, shell2, b, gabcd)
            case default
                call ints2e_dsss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)    
            end select

      end subroutine ints2e_dsss_perm
    

    subroutine ints2e_dsss(shell1, a, shell2, b, shell3, c, shell4, d, gabcd)
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
            integer, parameter :: momentum3 = 0
            integer, parameter :: momentum4 = 0
            integer, parameter :: nints1 = 6
            integer, parameter :: nints2 = 1
            integer, parameter :: nints3 = 1
            integer, parameter :: nints4 = 1
            integer, parameter :: nints = nints1 * nints2 * nints3 * nints4
            
            double precision :: norm12, norm123
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(3, 3, 3) :: rtuv
            double precision, dimension(0:0, 0:0, 0:0) :: gabtuv
            double precision, dimension(3) :: fmarray

            double precision :: euv, cdsum
            double precision, dimension(4) ::  etuv
            double precision, dimension(1) :: etuv2

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

                                    call f2(alpha * dot_product(rpq, rpq), fmarray)
                                    call chints_2(fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)

    
!
! ------------- AUTOMATICALLY GENERATED CODE -------------
!
euv = eijy2(1) * eijz2(1)
etuv2(1) = eijx2(1) * euv
norm12 = nrml(1, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(1)
etuv(1) = eijx1(6) * euv
etuv(2) = eijx1(7) * euv
etuv(3) = eijx1(8) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(3, 1, 1)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
gabcd(1) = gabcd(1) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(2, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(2) * eijz1(1)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(3) * eijz1(1)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 2, 1)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
gabcd(2) = gabcd(2) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(3, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(2)
etuv(1) = eijx1(2) * euv
etuv(2) = eijx1(3) * euv
euv = eijy1(1) * eijz1(3)
etuv(3) = eijx1(2) * euv
etuv(4) = eijx1(3) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(2, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(2, 1, 2)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
gabcd(3) = gabcd(3) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(4, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(6) * eijz1(1)
etuv(1) = eijx1(1) * euv
euv = eijy1(7) * eijz1(1)
etuv(2) = eijx1(1) * euv
euv = eijy1(8) * eijz1(1)
etuv(3) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 3, 1)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
gabcd(4) = gabcd(4) + norm123 * nrml(1, l, shell4) * cdsum
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
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 2, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(4) * rtuv(1, 2, 2)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
gabcd(5) = gabcd(5) + norm123 * nrml(1, l, shell4) * cdsum
norm12 = nrml(6, i, shell1) *  nrml(1, j, shell2)
euv = eijy1(1) * eijz1(6)
etuv(1) = eijx1(1) * euv
euv = eijy1(1) * eijz1(7)
etuv(2) = eijx1(1) * euv
euv = eijy1(1) * eijz1(8)
etuv(3) = eijx1(1) * euv
gabtuv(0, 0, 0) =  etuv(1) * rtuv(1, 1, 1)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(2) * rtuv(1, 1, 2)
gabtuv(0, 0, 0) = gabtuv(0, 0, 0) + etuv(3) * rtuv(1, 1, 3)
norm123 = norm12 * nrml(1, k, shell3)
cdsum = etuv2(1) * gabtuv(0, 0, 0)
gabcd(6) = gabcd(6) + norm123 * nrml(1, l, shell4) * cdsum
!
! -------- END OF AUTOMATICALLY GENERATED CODE ---------
!

                              end do
                        end do
                  end do
            end do
      end subroutine ints2e_dsss
end module dsss
    
