module jengine
      use math_constants
      use arithmetic
      use boys
      use hermite
      use gto
      use gparam
      use ericonst
      use ssss ! 1
      use spss ! 2
      use spsp ! 3
      use ppss ! 4
      use ppsp ! 5
      use pppp ! 6
      use dsss ! 7
      use ddss ! 8
      use dsds ! 9
      use dppp ! 10
      use dpss ! 11
      use dspp ! 12
      use ddps ! 13
      use dsps ! 14
      use dpps ! 15
      use dpds ! 16
      use ddds ! 17
      use fsss ! 18
      use fsps ! 19
      use fsds ! 20
      use fpss ! 21
      use fdss ! 22
      use fpps ! 23
      use fspp ! 24
      use llll

      implicit none

contains

      pure subroutine ftuv(rhoab, eijx, eijy, eijz, f, &
            ninta, lla, mma, nna, coeffa, &
            nintb, llb, mmb, nnb, coeffb)

            real(F64), dimension(:), intent(in)           :: rhoab
            real(F64), dimension(:), intent(in)           :: eijx, eijy, eijz
            real(F64), dimension(0:, 0:, 0:), intent(out) :: f
            integer, intent(in)                           :: ninta
            integer, dimension(:), intent(in)             :: lla, mma, nna
            real(F64), dimension(:), intent(in)           :: coeffa
            integer, intent(in)                           :: nintb
            integer, dimension(:), intent(in)             :: llb, mmb, nnb
            real(F64), dimension(:), intent(in)           :: coeffb

            real(F64) :: ev, euv, etuv
            real(F64) :: ca, cb, cab
            integer :: pos1, pos2, pos3
            integer :: t, u, v
            integer :: a, b
            integer :: la, ma, na, lb, mb, nb
            integer :: idxab

            f = ZERO
            idxab = 1

            do b = 1, nintb
                  lb = llb(b)
                  mb = mmb(b)
                  nb = nnb(b)

                  cb = coeffb(b)

                  do a = 1, ninta
                        la = lla(a)
                        ma = mma(a)
                        na = nna(a)

                        ca = coeffa(a)
                        cab = ca * cb * rhoab(idxab)
                        idxab = idxab + 1

                        pos1 = eijmatrix_position(la + lb, lb, 0)
                        pos2 = eijmatrix_position(ma + mb, mb, 0)
                        pos3 = eijmatrix_position(na + nb, nb, 0)

                        do v = 0, na + nb
                              ev = cab * eijz(pos3 + v)
                              do u = 0, ma + mb
                                    euv = eijy(pos2 + u) * ev
                                    do t = 0, la + lb
                                          etuv = eijx(pos1 + t) * euv
                                          f(t, u, v) = f(t, u, v) + etuv
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine ftuv


      pure subroutine ftuv_phase(fp, f, momentum)
            real(F64), dimension(:), intent(out)         :: fp
            real(F64), dimension(0:, 0:, 0:), intent(in) :: f
            integer, intent(in)                          :: momentum

            integer :: k
            integer :: t, u, v
            real(F64) :: phase, phasev, phaseu

            k = 1
            phasev = one
            do v = 0, momentum
                  phaseu = phasev
                  do u = 0, momentum - v
                        phase = phaseu
                        do t = 0, momentum - v - u
                              fp(k) = f(t, u, v) * phase
                              k = k + 1
                              phase = -phase
                        end do
                        phaseu = -phaseu
                  end do
                  phasev = -phasev
            end do
      end subroutine ftuv_phase


      pure subroutine linear_arrange(flin, f, m)
            real(F64), dimension(:), intent(out)         :: flin
            real(F64), dimension(0:, 0:, 0:), intent(in) :: f
            integer, intent(in)                          :: m

            integer :: k
            integer :: t, u, v

            k = 1
            do v = 0, m
                  do u = 0, m - v
                        do t = 0, m - v - u
                              flin(k) = f(t, u, v)
                              k = k + 1
                        end do
                  end do
            end do
      end subroutine linear_arrange


      pure subroutine alternating_sequence(g, e, n)
            !
            ! [e_0, e_1, e_2, ..., e_n] -> [e_0, -e_1, e_2, ..., (-1)^n e_n]
            !
            real(F64), dimension(:), contiguous, intent(out) :: g
            real(F64), dimension(:), contiguous, intent(in)  :: e
            integer, intent(in)                              :: n

            integer :: k

            g(1:n+1) = e(1:n+1)

            do k = 1, n, 2
                  g(k + 1) = -g(k + 1)
            end do
      end subroutine alternating_sequence


      pure subroutine inner_n(g, mab, f, r, tau, nu, phi)
            real(F64), intent(out)                          :: g
            integer, intent(in)                             :: mab
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            integer :: k
            integer :: t, u, v

            k = 1
            g = ZERO
            do v = 0, mab
                  do u = 0, mab - v
                        do t = 0, mab - v - u
                              g = g + f(k) * r(t + tau + 1, u + nu + 1, phi + v + 1)
                              k = k + 1
                        end do
                  end do
            end do
      end subroutine inner_n

      pure subroutine inner_0(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
      end subroutine inner_0


      pure subroutine inner_1(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(1+tau, 2+nu, 1+phi)
            t = t + f(4) * r(1+tau, 1+nu, 2+phi)
      end subroutine inner_1


      pure subroutine inner_2(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(1+tau, 2+nu, 1+phi)
            t = t + f(5) * r(2+tau, 2+nu, 1+phi)
            t = t + f(6) * r(1+tau, 3+nu, 1+phi)
            t = t + f(7) * r(1+tau, 1+nu, 2+phi)
            t = t + f(8) * r(2+tau, 1+nu, 2+phi)
            t = t + f(9) * r(1+tau, 2+nu, 2+phi)
            t = t + f(10) * r(1+tau, 1+nu, 3+phi)
      end subroutine inner_2


      pure subroutine inner_3(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(4+tau, 1+nu, 1+phi)
            t = t + f(5) * r(1+tau, 2+nu, 1+phi)
            t = t + f(6) * r(2+tau, 2+nu, 1+phi)
            t = t + f(7) * r(3+tau, 2+nu, 1+phi)
            t = t + f(8) * r(1+tau, 3+nu, 1+phi)
            t = t + f(9) * r(2+tau, 3+nu, 1+phi)
            t = t + f(10) * r(1+tau, 4+nu, 1+phi)
            t = t + f(11) * r(1+tau, 1+nu, 2+phi)
            t = t + f(12) * r(2+tau, 1+nu, 2+phi)
            t = t + f(13) * r(3+tau, 1+nu, 2+phi)
            t = t + f(14) * r(1+tau, 2+nu, 2+phi)
            t = t + f(15) * r(2+tau, 2+nu, 2+phi)
            t = t + f(16) * r(1+tau, 3+nu, 2+phi)
            t = t + f(17) * r(1+tau, 1+nu, 3+phi)
            t = t + f(18) * r(2+tau, 1+nu, 3+phi)
            t = t + f(19) * r(1+tau, 2+nu, 3+phi)
            t = t + f(20) * r(1+tau, 1+nu, 4+phi)
      end subroutine inner_3


      pure subroutine inner_4(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(4+tau, 1+nu, 1+phi)
            t = t + f(5) * r(5+tau, 1+nu, 1+phi)
            t = t + f(6) * r(1+tau, 2+nu, 1+phi)
            t = t + f(7) * r(2+tau, 2+nu, 1+phi)
            t = t + f(8) * r(3+tau, 2+nu, 1+phi)
            t = t + f(9) * r(4+tau, 2+nu, 1+phi)
            t = t + f(10) * r(1+tau, 3+nu, 1+phi)
            t = t + f(11) * r(2+tau, 3+nu, 1+phi)
            t = t + f(12) * r(3+tau, 3+nu, 1+phi)
            t = t + f(13) * r(1+tau, 4+nu, 1+phi)
            t = t + f(14) * r(2+tau, 4+nu, 1+phi)
            t = t + f(15) * r(1+tau, 5+nu, 1+phi)
            t = t + f(16) * r(1+tau, 1+nu, 2+phi)
            t = t + f(17) * r(2+tau, 1+nu, 2+phi)
            t = t + f(18) * r(3+tau, 1+nu, 2+phi)
            t = t + f(19) * r(4+tau, 1+nu, 2+phi)
            t = t + f(20) * r(1+tau, 2+nu, 2+phi)
            t = t + f(21) * r(2+tau, 2+nu, 2+phi)
            t = t + f(22) * r(3+tau, 2+nu, 2+phi)
            t = t + f(23) * r(1+tau, 3+nu, 2+phi)
            t = t + f(24) * r(2+tau, 3+nu, 2+phi)
            t = t + f(25) * r(1+tau, 4+nu, 2+phi)
            t = t + f(26) * r(1+tau, 1+nu, 3+phi)
            t = t + f(27) * r(2+tau, 1+nu, 3+phi)
            t = t + f(28) * r(3+tau, 1+nu, 3+phi)
            t = t + f(29) * r(1+tau, 2+nu, 3+phi)
            t = t + f(30) * r(2+tau, 2+nu, 3+phi)
            t = t + f(31) * r(1+tau, 3+nu, 3+phi)
            t = t + f(32) * r(1+tau, 1+nu, 4+phi)
            t = t + f(33) * r(2+tau, 1+nu, 4+phi)
            t = t + f(34) * r(1+tau, 2+nu, 4+phi)
            t = t + f(35) * r(1+tau, 1+nu, 5+phi)
      end subroutine inner_4


      pure subroutine inner_5(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(4+tau, 1+nu, 1+phi)
            t = t + f(5) * r(5+tau, 1+nu, 1+phi)
            t = t + f(6) * r(6+tau, 1+nu, 1+phi)
            t = t + f(7) * r(1+tau, 2+nu, 1+phi)
            t = t + f(8) * r(2+tau, 2+nu, 1+phi)
            t = t + f(9) * r(3+tau, 2+nu, 1+phi)
            t = t + f(10) * r(4+tau, 2+nu, 1+phi)
            t = t + f(11) * r(5+tau, 2+nu, 1+phi)
            t = t + f(12) * r(1+tau, 3+nu, 1+phi)
            t = t + f(13) * r(2+tau, 3+nu, 1+phi)
            t = t + f(14) * r(3+tau, 3+nu, 1+phi)
            t = t + f(15) * r(4+tau, 3+nu, 1+phi)
            t = t + f(16) * r(1+tau, 4+nu, 1+phi)
            t = t + f(17) * r(2+tau, 4+nu, 1+phi)
            t = t + f(18) * r(3+tau, 4+nu, 1+phi)
            t = t + f(19) * r(1+tau, 5+nu, 1+phi)
            t = t + f(20) * r(2+tau, 5+nu, 1+phi)
            t = t + f(21) * r(1+tau, 6+nu, 1+phi)
            t = t + f(22) * r(1+tau, 1+nu, 2+phi)
            t = t + f(23) * r(2+tau, 1+nu, 2+phi)
            t = t + f(24) * r(3+tau, 1+nu, 2+phi)
            t = t + f(25) * r(4+tau, 1+nu, 2+phi)
            t = t + f(26) * r(5+tau, 1+nu, 2+phi)
            t = t + f(27) * r(1+tau, 2+nu, 2+phi)
            t = t + f(28) * r(2+tau, 2+nu, 2+phi)
            t = t + f(29) * r(3+tau, 2+nu, 2+phi)
            t = t + f(30) * r(4+tau, 2+nu, 2+phi)
            t = t + f(31) * r(1+tau, 3+nu, 2+phi)
            t = t + f(32) * r(2+tau, 3+nu, 2+phi)
            t = t + f(33) * r(3+tau, 3+nu, 2+phi)
            t = t + f(34) * r(1+tau, 4+nu, 2+phi)
            t = t + f(35) * r(2+tau, 4+nu, 2+phi)
            t = t + f(36) * r(1+tau, 5+nu, 2+phi)
            t = t + f(37) * r(1+tau, 1+nu, 3+phi)
            t = t + f(38) * r(2+tau, 1+nu, 3+phi)
            t = t + f(39) * r(3+tau, 1+nu, 3+phi)
            t = t + f(40) * r(4+tau, 1+nu, 3+phi)
            t = t + f(41) * r(1+tau, 2+nu, 3+phi)
            t = t + f(42) * r(2+tau, 2+nu, 3+phi)
            t = t + f(43) * r(3+tau, 2+nu, 3+phi)
            t = t + f(44) * r(1+tau, 3+nu, 3+phi)
            t = t + f(45) * r(2+tau, 3+nu, 3+phi)
            t = t + f(46) * r(1+tau, 4+nu, 3+phi)
            t = t + f(47) * r(1+tau, 1+nu, 4+phi)
            t = t + f(48) * r(2+tau, 1+nu, 4+phi)
            t = t + f(49) * r(3+tau, 1+nu, 4+phi)
            t = t + f(50) * r(1+tau, 2+nu, 4+phi)
            t = t + f(51) * r(2+tau, 2+nu, 4+phi)
            t = t + f(52) * r(1+tau, 3+nu, 4+phi)
            t = t + f(53) * r(1+tau, 1+nu, 5+phi)
            t = t + f(54) * r(2+tau, 1+nu, 5+phi)
            t = t + f(55) * r(1+tau, 2+nu, 5+phi)
            t = t + f(56) * r(1+tau, 1+nu, 6+phi)
      end subroutine inner_5


      pure subroutine inner_6(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-15 22:32:45 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(4+tau, 1+nu, 1+phi)
            t = t + f(5) * r(5+tau, 1+nu, 1+phi)
            t = t + f(6) * r(6+tau, 1+nu, 1+phi)
            t = t + f(7) * r(7+tau, 1+nu, 1+phi)
            t = t + f(8) * r(1+tau, 2+nu, 1+phi)
            t = t + f(9) * r(2+tau, 2+nu, 1+phi)
            t = t + f(10) * r(3+tau, 2+nu, 1+phi)
            t = t + f(11) * r(4+tau, 2+nu, 1+phi)
            t = t + f(12) * r(5+tau, 2+nu, 1+phi)
            t = t + f(13) * r(6+tau, 2+nu, 1+phi)
            t = t + f(14) * r(1+tau, 3+nu, 1+phi)
            t = t + f(15) * r(2+tau, 3+nu, 1+phi)
            t = t + f(16) * r(3+tau, 3+nu, 1+phi)
            t = t + f(17) * r(4+tau, 3+nu, 1+phi)
            t = t + f(18) * r(5+tau, 3+nu, 1+phi)
            t = t + f(19) * r(1+tau, 4+nu, 1+phi)
            t = t + f(20) * r(2+tau, 4+nu, 1+phi)
            t = t + f(21) * r(3+tau, 4+nu, 1+phi)
            t = t + f(22) * r(4+tau, 4+nu, 1+phi)
            t = t + f(23) * r(1+tau, 5+nu, 1+phi)
            t = t + f(24) * r(2+tau, 5+nu, 1+phi)
            t = t + f(25) * r(3+tau, 5+nu, 1+phi)
            t = t + f(26) * r(1+tau, 6+nu, 1+phi)
            t = t + f(27) * r(2+tau, 6+nu, 1+phi)
            t = t + f(28) * r(1+tau, 7+nu, 1+phi)
            t = t + f(29) * r(1+tau, 1+nu, 2+phi)
            t = t + f(30) * r(2+tau, 1+nu, 2+phi)
            t = t + f(31) * r(3+tau, 1+nu, 2+phi)
            t = t + f(32) * r(4+tau, 1+nu, 2+phi)
            t = t + f(33) * r(5+tau, 1+nu, 2+phi)
            t = t + f(34) * r(6+tau, 1+nu, 2+phi)
            t = t + f(35) * r(1+tau, 2+nu, 2+phi)
            t = t + f(36) * r(2+tau, 2+nu, 2+phi)
            t = t + f(37) * r(3+tau, 2+nu, 2+phi)
            t = t + f(38) * r(4+tau, 2+nu, 2+phi)
            t = t + f(39) * r(5+tau, 2+nu, 2+phi)
            t = t + f(40) * r(1+tau, 3+nu, 2+phi)
            t = t + f(41) * r(2+tau, 3+nu, 2+phi)
            t = t + f(42) * r(3+tau, 3+nu, 2+phi)
            t = t + f(43) * r(4+tau, 3+nu, 2+phi)
            t = t + f(44) * r(1+tau, 4+nu, 2+phi)
            t = t + f(45) * r(2+tau, 4+nu, 2+phi)
            t = t + f(46) * r(3+tau, 4+nu, 2+phi)
            t = t + f(47) * r(1+tau, 5+nu, 2+phi)
            t = t + f(48) * r(2+tau, 5+nu, 2+phi)
            t = t + f(49) * r(1+tau, 6+nu, 2+phi)
            t = t + f(50) * r(1+tau, 1+nu, 3+phi)
            t = t + f(51) * r(2+tau, 1+nu, 3+phi)
            t = t + f(52) * r(3+tau, 1+nu, 3+phi)
            t = t + f(53) * r(4+tau, 1+nu, 3+phi)
            t = t + f(54) * r(5+tau, 1+nu, 3+phi)
            t = t + f(55) * r(1+tau, 2+nu, 3+phi)
            t = t + f(56) * r(2+tau, 2+nu, 3+phi)
            t = t + f(57) * r(3+tau, 2+nu, 3+phi)
            t = t + f(58) * r(4+tau, 2+nu, 3+phi)
            t = t + f(59) * r(1+tau, 3+nu, 3+phi)
            t = t + f(60) * r(2+tau, 3+nu, 3+phi)
            t = t + f(61) * r(3+tau, 3+nu, 3+phi)
            t = t + f(62) * r(1+tau, 4+nu, 3+phi)
            t = t + f(63) * r(2+tau, 4+nu, 3+phi)
            t = t + f(64) * r(1+tau, 5+nu, 3+phi)
            t = t + f(65) * r(1+tau, 1+nu, 4+phi)
            t = t + f(66) * r(2+tau, 1+nu, 4+phi)
            t = t + f(67) * r(3+tau, 1+nu, 4+phi)
            t = t + f(68) * r(4+tau, 1+nu, 4+phi)
            t = t + f(69) * r(1+tau, 2+nu, 4+phi)
            t = t + f(70) * r(2+tau, 2+nu, 4+phi)
            t = t + f(71) * r(3+tau, 2+nu, 4+phi)
            t = t + f(72) * r(1+tau, 3+nu, 4+phi)
            t = t + f(73) * r(2+tau, 3+nu, 4+phi)
            t = t + f(74) * r(1+tau, 4+nu, 4+phi)
            t = t + f(75) * r(1+tau, 1+nu, 5+phi)
            t = t + f(76) * r(2+tau, 1+nu, 5+phi)
            t = t + f(77) * r(3+tau, 1+nu, 5+phi)
            t = t + f(78) * r(1+tau, 2+nu, 5+phi)
            t = t + f(79) * r(2+tau, 2+nu, 5+phi)
            t = t + f(80) * r(1+tau, 3+nu, 5+phi)
            t = t + f(81) * r(1+tau, 1+nu, 6+phi)
            t = t + f(82) * r(2+tau, 1+nu, 6+phi)
            t = t + f(83) * r(1+tau, 2+nu, 6+phi)
            t = t + f(84) * r(1+tau, 1+nu, 7+phi)
      end subroutine inner_6


      pure subroutine inner_7(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-17 20:24:46 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(4+tau, 1+nu, 1+phi)
            t = t + f(5) * r(5+tau, 1+nu, 1+phi)
            t = t + f(6) * r(6+tau, 1+nu, 1+phi)
            t = t + f(7) * r(7+tau, 1+nu, 1+phi)
            t = t + f(8) * r(8+tau, 1+nu, 1+phi)
            t = t + f(9) * r(1+tau, 2+nu, 1+phi)
            t = t + f(10) * r(2+tau, 2+nu, 1+phi)
            t = t + f(11) * r(3+tau, 2+nu, 1+phi)
            t = t + f(12) * r(4+tau, 2+nu, 1+phi)
            t = t + f(13) * r(5+tau, 2+nu, 1+phi)
            t = t + f(14) * r(6+tau, 2+nu, 1+phi)
            t = t + f(15) * r(7+tau, 2+nu, 1+phi)
            t = t + f(16) * r(1+tau, 3+nu, 1+phi)
            t = t + f(17) * r(2+tau, 3+nu, 1+phi)
            t = t + f(18) * r(3+tau, 3+nu, 1+phi)
            t = t + f(19) * r(4+tau, 3+nu, 1+phi)
            t = t + f(20) * r(5+tau, 3+nu, 1+phi)
            t = t + f(21) * r(6+tau, 3+nu, 1+phi)
            t = t + f(22) * r(1+tau, 4+nu, 1+phi)
            t = t + f(23) * r(2+tau, 4+nu, 1+phi)
            t = t + f(24) * r(3+tau, 4+nu, 1+phi)
            t = t + f(25) * r(4+tau, 4+nu, 1+phi)
            t = t + f(26) * r(5+tau, 4+nu, 1+phi)
            t = t + f(27) * r(1+tau, 5+nu, 1+phi)
            t = t + f(28) * r(2+tau, 5+nu, 1+phi)
            t = t + f(29) * r(3+tau, 5+nu, 1+phi)
            t = t + f(30) * r(4+tau, 5+nu, 1+phi)
            t = t + f(31) * r(1+tau, 6+nu, 1+phi)
            t = t + f(32) * r(2+tau, 6+nu, 1+phi)
            t = t + f(33) * r(3+tau, 6+nu, 1+phi)
            t = t + f(34) * r(1+tau, 7+nu, 1+phi)
            t = t + f(35) * r(2+tau, 7+nu, 1+phi)
            t = t + f(36) * r(1+tau, 8+nu, 1+phi)
            t = t + f(37) * r(1+tau, 1+nu, 2+phi)
            t = t + f(38) * r(2+tau, 1+nu, 2+phi)
            t = t + f(39) * r(3+tau, 1+nu, 2+phi)
            t = t + f(40) * r(4+tau, 1+nu, 2+phi)
            t = t + f(41) * r(5+tau, 1+nu, 2+phi)
            t = t + f(42) * r(6+tau, 1+nu, 2+phi)
            t = t + f(43) * r(7+tau, 1+nu, 2+phi)
            t = t + f(44) * r(1+tau, 2+nu, 2+phi)
            t = t + f(45) * r(2+tau, 2+nu, 2+phi)
            t = t + f(46) * r(3+tau, 2+nu, 2+phi)
            t = t + f(47) * r(4+tau, 2+nu, 2+phi)
            t = t + f(48) * r(5+tau, 2+nu, 2+phi)
            t = t + f(49) * r(6+tau, 2+nu, 2+phi)
            t = t + f(50) * r(1+tau, 3+nu, 2+phi)
            t = t + f(51) * r(2+tau, 3+nu, 2+phi)
            t = t + f(52) * r(3+tau, 3+nu, 2+phi)
            t = t + f(53) * r(4+tau, 3+nu, 2+phi)
            t = t + f(54) * r(5+tau, 3+nu, 2+phi)
            t = t + f(55) * r(1+tau, 4+nu, 2+phi)
            t = t + f(56) * r(2+tau, 4+nu, 2+phi)
            t = t + f(57) * r(3+tau, 4+nu, 2+phi)
            t = t + f(58) * r(4+tau, 4+nu, 2+phi)
            t = t + f(59) * r(1+tau, 5+nu, 2+phi)
            t = t + f(60) * r(2+tau, 5+nu, 2+phi)
            t = t + f(61) * r(3+tau, 5+nu, 2+phi)
            t = t + f(62) * r(1+tau, 6+nu, 2+phi)
            t = t + f(63) * r(2+tau, 6+nu, 2+phi)
            t = t + f(64) * r(1+tau, 7+nu, 2+phi)
            t = t + f(65) * r(1+tau, 1+nu, 3+phi)
            t = t + f(66) * r(2+tau, 1+nu, 3+phi)
            t = t + f(67) * r(3+tau, 1+nu, 3+phi)
            t = t + f(68) * r(4+tau, 1+nu, 3+phi)
            t = t + f(69) * r(5+tau, 1+nu, 3+phi)
            t = t + f(70) * r(6+tau, 1+nu, 3+phi)
            t = t + f(71) * r(1+tau, 2+nu, 3+phi)
            t = t + f(72) * r(2+tau, 2+nu, 3+phi)
            t = t + f(73) * r(3+tau, 2+nu, 3+phi)
            t = t + f(74) * r(4+tau, 2+nu, 3+phi)
            t = t + f(75) * r(5+tau, 2+nu, 3+phi)
            t = t + f(76) * r(1+tau, 3+nu, 3+phi)
            t = t + f(77) * r(2+tau, 3+nu, 3+phi)
            t = t + f(78) * r(3+tau, 3+nu, 3+phi)
            t = t + f(79) * r(4+tau, 3+nu, 3+phi)
            t = t + f(80) * r(1+tau, 4+nu, 3+phi)
            t = t + f(81) * r(2+tau, 4+nu, 3+phi)
            t = t + f(82) * r(3+tau, 4+nu, 3+phi)
            t = t + f(83) * r(1+tau, 5+nu, 3+phi)
            t = t + f(84) * r(2+tau, 5+nu, 3+phi)
            t = t + f(85) * r(1+tau, 6+nu, 3+phi)
            t = t + f(86) * r(1+tau, 1+nu, 4+phi)
            t = t + f(87) * r(2+tau, 1+nu, 4+phi)
            t = t + f(88) * r(3+tau, 1+nu, 4+phi)
            t = t + f(89) * r(4+tau, 1+nu, 4+phi)
            t = t + f(90) * r(5+tau, 1+nu, 4+phi)
            t = t + f(91) * r(1+tau, 2+nu, 4+phi)
            t = t + f(92) * r(2+tau, 2+nu, 4+phi)
            t = t + f(93) * r(3+tau, 2+nu, 4+phi)
            t = t + f(94) * r(4+tau, 2+nu, 4+phi)
            t = t + f(95) * r(1+tau, 3+nu, 4+phi)
            t = t + f(96) * r(2+tau, 3+nu, 4+phi)
            t = t + f(97) * r(3+tau, 3+nu, 4+phi)
            t = t + f(98) * r(1+tau, 4+nu, 4+phi)
            t = t + f(99) * r(2+tau, 4+nu, 4+phi)
            t = t + f(100) * r(1+tau, 5+nu, 4+phi)
            t = t + f(101) * r(1+tau, 1+nu, 5+phi)
            t = t + f(102) * r(2+tau, 1+nu, 5+phi)
            t = t + f(103) * r(3+tau, 1+nu, 5+phi)
            t = t + f(104) * r(4+tau, 1+nu, 5+phi)
            t = t + f(105) * r(1+tau, 2+nu, 5+phi)
            t = t + f(106) * r(2+tau, 2+nu, 5+phi)
            t = t + f(107) * r(3+tau, 2+nu, 5+phi)
            t = t + f(108) * r(1+tau, 3+nu, 5+phi)
            t = t + f(109) * r(2+tau, 3+nu, 5+phi)
            t = t + f(110) * r(1+tau, 4+nu, 5+phi)
            t = t + f(111) * r(1+tau, 1+nu, 6+phi)
            t = t + f(112) * r(2+tau, 1+nu, 6+phi)
            t = t + f(113) * r(3+tau, 1+nu, 6+phi)
            t = t + f(114) * r(1+tau, 2+nu, 6+phi)
            t = t + f(115) * r(2+tau, 2+nu, 6+phi)
            t = t + f(116) * r(1+tau, 3+nu, 6+phi)
            t = t + f(117) * r(1+tau, 1+nu, 7+phi)
            t = t + f(118) * r(2+tau, 1+nu, 7+phi)
            t = t + f(119) * r(1+tau, 2+nu, 7+phi)
            t = t + f(120) * r(1+tau, 1+nu, 8+phi)
      end subroutine inner_7


      pure subroutine inner_8(t, f, r, tau, nu, phi)
            !
            ! Subroutine generated automatically on 2014-05-17 20:24:46 UTC
            !
            real(F64), intent(out)                          :: t
            real(F64), dimension(:), contiguous, intent(in) :: f
            real(F64), dimension(:, :, :), intent(in)       :: r
            integer, intent(in)                             :: tau, nu, phi

            t = f(1) * r(1+tau, 1+nu, 1+phi)
            t = t + f(2) * r(2+tau, 1+nu, 1+phi)
            t = t + f(3) * r(3+tau, 1+nu, 1+phi)
            t = t + f(4) * r(4+tau, 1+nu, 1+phi)
            t = t + f(5) * r(5+tau, 1+nu, 1+phi)
            t = t + f(6) * r(6+tau, 1+nu, 1+phi)
            t = t + f(7) * r(7+tau, 1+nu, 1+phi)
            t = t + f(8) * r(8+tau, 1+nu, 1+phi)
            t = t + f(9) * r(9+tau, 1+nu, 1+phi)
            t = t + f(10) * r(1+tau, 2+nu, 1+phi)
            t = t + f(11) * r(2+tau, 2+nu, 1+phi)
            t = t + f(12) * r(3+tau, 2+nu, 1+phi)
            t = t + f(13) * r(4+tau, 2+nu, 1+phi)
            t = t + f(14) * r(5+tau, 2+nu, 1+phi)
            t = t + f(15) * r(6+tau, 2+nu, 1+phi)
            t = t + f(16) * r(7+tau, 2+nu, 1+phi)
            t = t + f(17) * r(8+tau, 2+nu, 1+phi)
            t = t + f(18) * r(1+tau, 3+nu, 1+phi)
            t = t + f(19) * r(2+tau, 3+nu, 1+phi)
            t = t + f(20) * r(3+tau, 3+nu, 1+phi)
            t = t + f(21) * r(4+tau, 3+nu, 1+phi)
            t = t + f(22) * r(5+tau, 3+nu, 1+phi)
            t = t + f(23) * r(6+tau, 3+nu, 1+phi)
            t = t + f(24) * r(7+tau, 3+nu, 1+phi)
            t = t + f(25) * r(1+tau, 4+nu, 1+phi)
            t = t + f(26) * r(2+tau, 4+nu, 1+phi)
            t = t + f(27) * r(3+tau, 4+nu, 1+phi)
            t = t + f(28) * r(4+tau, 4+nu, 1+phi)
            t = t + f(29) * r(5+tau, 4+nu, 1+phi)
            t = t + f(30) * r(6+tau, 4+nu, 1+phi)
            t = t + f(31) * r(1+tau, 5+nu, 1+phi)
            t = t + f(32) * r(2+tau, 5+nu, 1+phi)
            t = t + f(33) * r(3+tau, 5+nu, 1+phi)
            t = t + f(34) * r(4+tau, 5+nu, 1+phi)
            t = t + f(35) * r(5+tau, 5+nu, 1+phi)
            t = t + f(36) * r(1+tau, 6+nu, 1+phi)
            t = t + f(37) * r(2+tau, 6+nu, 1+phi)
            t = t + f(38) * r(3+tau, 6+nu, 1+phi)
            t = t + f(39) * r(4+tau, 6+nu, 1+phi)
            t = t + f(40) * r(1+tau, 7+nu, 1+phi)
            t = t + f(41) * r(2+tau, 7+nu, 1+phi)
            t = t + f(42) * r(3+tau, 7+nu, 1+phi)
            t = t + f(43) * r(1+tau, 8+nu, 1+phi)
            t = t + f(44) * r(2+tau, 8+nu, 1+phi)
            t = t + f(45) * r(1+tau, 9+nu, 1+phi)
            t = t + f(46) * r(1+tau, 1+nu, 2+phi)
            t = t + f(47) * r(2+tau, 1+nu, 2+phi)
            t = t + f(48) * r(3+tau, 1+nu, 2+phi)
            t = t + f(49) * r(4+tau, 1+nu, 2+phi)
            t = t + f(50) * r(5+tau, 1+nu, 2+phi)
            t = t + f(51) * r(6+tau, 1+nu, 2+phi)
            t = t + f(52) * r(7+tau, 1+nu, 2+phi)
            t = t + f(53) * r(8+tau, 1+nu, 2+phi)
            t = t + f(54) * r(1+tau, 2+nu, 2+phi)
            t = t + f(55) * r(2+tau, 2+nu, 2+phi)
            t = t + f(56) * r(3+tau, 2+nu, 2+phi)
            t = t + f(57) * r(4+tau, 2+nu, 2+phi)
            t = t + f(58) * r(5+tau, 2+nu, 2+phi)
            t = t + f(59) * r(6+tau, 2+nu, 2+phi)
            t = t + f(60) * r(7+tau, 2+nu, 2+phi)
            t = t + f(61) * r(1+tau, 3+nu, 2+phi)
            t = t + f(62) * r(2+tau, 3+nu, 2+phi)
            t = t + f(63) * r(3+tau, 3+nu, 2+phi)
            t = t + f(64) * r(4+tau, 3+nu, 2+phi)
            t = t + f(65) * r(5+tau, 3+nu, 2+phi)
            t = t + f(66) * r(6+tau, 3+nu, 2+phi)
            t = t + f(67) * r(1+tau, 4+nu, 2+phi)
            t = t + f(68) * r(2+tau, 4+nu, 2+phi)
            t = t + f(69) * r(3+tau, 4+nu, 2+phi)
            t = t + f(70) * r(4+tau, 4+nu, 2+phi)
            t = t + f(71) * r(5+tau, 4+nu, 2+phi)
            t = t + f(72) * r(1+tau, 5+nu, 2+phi)
            t = t + f(73) * r(2+tau, 5+nu, 2+phi)
            t = t + f(74) * r(3+tau, 5+nu, 2+phi)
            t = t + f(75) * r(4+tau, 5+nu, 2+phi)
            t = t + f(76) * r(1+tau, 6+nu, 2+phi)
            t = t + f(77) * r(2+tau, 6+nu, 2+phi)
            t = t + f(78) * r(3+tau, 6+nu, 2+phi)
            t = t + f(79) * r(1+tau, 7+nu, 2+phi)
            t = t + f(80) * r(2+tau, 7+nu, 2+phi)
            t = t + f(81) * r(1+tau, 8+nu, 2+phi)
            t = t + f(82) * r(1+tau, 1+nu, 3+phi)
            t = t + f(83) * r(2+tau, 1+nu, 3+phi)
            t = t + f(84) * r(3+tau, 1+nu, 3+phi)
            t = t + f(85) * r(4+tau, 1+nu, 3+phi)
            t = t + f(86) * r(5+tau, 1+nu, 3+phi)
            t = t + f(87) * r(6+tau, 1+nu, 3+phi)
            t = t + f(88) * r(7+tau, 1+nu, 3+phi)
            t = t + f(89) * r(1+tau, 2+nu, 3+phi)
            t = t + f(90) * r(2+tau, 2+nu, 3+phi)
            t = t + f(91) * r(3+tau, 2+nu, 3+phi)
            t = t + f(92) * r(4+tau, 2+nu, 3+phi)
            t = t + f(93) * r(5+tau, 2+nu, 3+phi)
            t = t + f(94) * r(6+tau, 2+nu, 3+phi)
            t = t + f(95) * r(1+tau, 3+nu, 3+phi)
            t = t + f(96) * r(2+tau, 3+nu, 3+phi)
            t = t + f(97) * r(3+tau, 3+nu, 3+phi)
            t = t + f(98) * r(4+tau, 3+nu, 3+phi)
            t = t + f(99) * r(5+tau, 3+nu, 3+phi)
            t = t + f(100) * r(1+tau, 4+nu, 3+phi)
            t = t + f(101) * r(2+tau, 4+nu, 3+phi)
            t = t + f(102) * r(3+tau, 4+nu, 3+phi)
            t = t + f(103) * r(4+tau, 4+nu, 3+phi)
            t = t + f(104) * r(1+tau, 5+nu, 3+phi)
            t = t + f(105) * r(2+tau, 5+nu, 3+phi)
            t = t + f(106) * r(3+tau, 5+nu, 3+phi)
            t = t + f(107) * r(1+tau, 6+nu, 3+phi)
            t = t + f(108) * r(2+tau, 6+nu, 3+phi)
            t = t + f(109) * r(1+tau, 7+nu, 3+phi)
            t = t + f(110) * r(1+tau, 1+nu, 4+phi)
            t = t + f(111) * r(2+tau, 1+nu, 4+phi)
            t = t + f(112) * r(3+tau, 1+nu, 4+phi)
            t = t + f(113) * r(4+tau, 1+nu, 4+phi)
            t = t + f(114) * r(5+tau, 1+nu, 4+phi)
            t = t + f(115) * r(6+tau, 1+nu, 4+phi)
            t = t + f(116) * r(1+tau, 2+nu, 4+phi)
            t = t + f(117) * r(2+tau, 2+nu, 4+phi)
            t = t + f(118) * r(3+tau, 2+nu, 4+phi)
            t = t + f(119) * r(4+tau, 2+nu, 4+phi)
            t = t + f(120) * r(5+tau, 2+nu, 4+phi)
            t = t + f(121) * r(1+tau, 3+nu, 4+phi)
            t = t + f(122) * r(2+tau, 3+nu, 4+phi)
            t = t + f(123) * r(3+tau, 3+nu, 4+phi)
            t = t + f(124) * r(4+tau, 3+nu, 4+phi)
            t = t + f(125) * r(1+tau, 4+nu, 4+phi)
            t = t + f(126) * r(2+tau, 4+nu, 4+phi)
            t = t + f(127) * r(3+tau, 4+nu, 4+phi)
            t = t + f(128) * r(1+tau, 5+nu, 4+phi)
            t = t + f(129) * r(2+tau, 5+nu, 4+phi)
            t = t + f(130) * r(1+tau, 6+nu, 4+phi)
            t = t + f(131) * r(1+tau, 1+nu, 5+phi)
            t = t + f(132) * r(2+tau, 1+nu, 5+phi)
            t = t + f(133) * r(3+tau, 1+nu, 5+phi)
            t = t + f(134) * r(4+tau, 1+nu, 5+phi)
            t = t + f(135) * r(5+tau, 1+nu, 5+phi)
            t = t + f(136) * r(1+tau, 2+nu, 5+phi)
            t = t + f(137) * r(2+tau, 2+nu, 5+phi)
            t = t + f(138) * r(3+tau, 2+nu, 5+phi)
            t = t + f(139) * r(4+tau, 2+nu, 5+phi)
            t = t + f(140) * r(1+tau, 3+nu, 5+phi)
            t = t + f(141) * r(2+tau, 3+nu, 5+phi)
            t = t + f(142) * r(3+tau, 3+nu, 5+phi)
            t = t + f(143) * r(1+tau, 4+nu, 5+phi)
            t = t + f(144) * r(2+tau, 4+nu, 5+phi)
            t = t + f(145) * r(1+tau, 5+nu, 5+phi)
            t = t + f(146) * r(1+tau, 1+nu, 6+phi)
            t = t + f(147) * r(2+tau, 1+nu, 6+phi)
            t = t + f(148) * r(3+tau, 1+nu, 6+phi)
            t = t + f(149) * r(4+tau, 1+nu, 6+phi)
            t = t + f(150) * r(1+tau, 2+nu, 6+phi)
            t = t + f(151) * r(2+tau, 2+nu, 6+phi)
            t = t + f(152) * r(3+tau, 2+nu, 6+phi)
            t = t + f(153) * r(1+tau, 3+nu, 6+phi)
            t = t + f(154) * r(2+tau, 3+nu, 6+phi)
            t = t + f(155) * r(1+tau, 4+nu, 6+phi)
            t = t + f(156) * r(1+tau, 1+nu, 7+phi)
            t = t + f(157) * r(2+tau, 1+nu, 7+phi)
            t = t + f(158) * r(3+tau, 1+nu, 7+phi)
            t = t + f(159) * r(1+tau, 2+nu, 7+phi)
            t = t + f(160) * r(2+tau, 2+nu, 7+phi)
            t = t + f(161) * r(1+tau, 3+nu, 7+phi)
            t = t + f(162) * r(1+tau, 1+nu, 8+phi)
            t = t + f(163) * r(2+tau, 1+nu, 8+phi)
            t = t + f(164) * r(1+tau, 2+nu, 8+phi)
            t = t + f(165) * r(1+tau, 1+nu, 9+phi)
      end subroutine inner_8


      pure function abcontrib_n(eijx, eijy, eijz, f, lc, mc, nc, ld, md, nd, mab, r)
            real(F64)                                       :: abcontrib_n
            real(F64), dimension(:), contiguous, intent(in) :: eijx, eijy, eijz
            real(F64), dimension(:), contiguous, intent(in) :: f
            integer, intent(in)                             :: lc, mc, nc
            integer, intent(in)                             :: ld, md, nd
            integer, intent(in)                             :: mab
            real(F64), dimension(:, :, :), intent(in)       :: r

            integer :: tau, nu, phi
            integer :: pos1, pos2, pos3
            real(F64) :: sum1, sum2, sum3
            real(F64), dimension(0:2 * MAX_L) :: alt_eijx, alt_eijy, alt_eijz

            pos1 = eijmatrix_position(lc + ld, ld, 0)
            pos2 = eijmatrix_position(mc + md, md, 0)
            pos3 = eijmatrix_position(nc + nd, nd, 0)

            call alternating_sequence(alt_eijx, eijx(pos1:), lc + ld)
            call alternating_sequence(alt_eijy, eijy(pos2:), mc + md)
            call alternating_sequence(alt_eijz, eijz(pos3:), nc + nd)

            abcontrib_n = ZERO
            do phi = 0, nc + nd
                  sum1 = ZERO
                  do nu = 0, mc + md
                        sum2 = ZERO
                        do tau = 0, lc + ld
                              call inner_n(sum3, mab, f, r, tau, nu, phi)
                              sum2 = sum2 + alt_eijx(tau) * sum3
                        end do
                        sum1 = sum1 + alt_eijy(nu) * sum2
                  end do
                  abcontrib_n = abcontrib_n + alt_eijz(phi) * sum1
            end do
      end function abcontrib_n


      pure function abcontrib_auto(eijx, eijy, eijz, f, lc, mc, nc, ld, md, nd, r, inner_loop)
            real(F64)                                       :: abcontrib_auto
            real(F64), dimension(:), contiguous, intent(in) :: eijx, eijy, eijz
            real(F64), dimension(:), contiguous, intent(in) :: f
            integer, intent(in)                             :: lc, mc, nc
            integer, intent(in)                             :: ld, md, nd
            real(F64), dimension(:, :, :), intent(in)       :: r
            procedure(inner_0)                              :: inner_loop

            integer :: tau, nu, phi
            integer :: pos1, pos2, pos3
            real(F64) :: sum1, sum2, sum3
            real(F64), dimension(0:2 * MAX_L) :: alt_eijx, alt_eijy, alt_eijz

            pos1 = eijmatrix_position(lc + ld, ld, 0)
            pos2 = eijmatrix_position(mc + md, md, 0)
            pos3 = eijmatrix_position(nc + nd, nd, 0)

            call alternating_sequence(alt_eijx, eijx(pos1:), lc + ld)
            call alternating_sequence(alt_eijy, eijy(pos2:), mc + md)
            call alternating_sequence(alt_eijz, eijz(pos3:), nc + nd)

            abcontrib_auto = ZERO
            do phi = 0, nc + nd
                  sum1 = ZERO
                  do nu = 0, mc + md
                        sum2 = ZERO
                        do tau = 0, lc + ld
                              call inner_loop(sum3, f, r, tau, nu, phi)
                              sum2 = sum2 + alt_eijx(tau) * sum3
                        end do
                        sum1 = sum1 + alt_eijy(nu) * sum2
                  end do
                  abcontrib_auto = abcontrib_auto + alt_eijz(phi) * sum1
            end do
      end function abcontrib_auto


      pure function cdcontrib_n(eijx, eijy, eijz, f, la, ma, na, lb, mb, nb, mcd, r)
            real(F64)                                       :: cdcontrib_n
            real(F64), dimension(:), contiguous, intent(in) :: eijx, eijy, eijz
            real(F64), dimension(:), contiguous, intent(in) :: f
            integer, intent(in)                             :: la, ma, na
            integer, intent(in)                             :: lb, mb, nb
            integer, intent(in)                             :: mcd
            real(F64), dimension(:, :, :), intent(in)       :: r

            integer :: t, u, v
            integer :: pos1, pos2, pos3
            real(F64) :: sum1, sum2, sum3

            pos1 = eijmatrix_position(la + lb, lb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(na + nb, nb, 0)
            cdcontrib_n = ZERO

            vloop: do v = 0, na + nb
                  sum1 = ZERO
                  uloop: do u = 0, ma + mb
                        sum2 = ZERO
                        tloop: do t = 0, la + lb
                              call inner_n(sum3, mcd, f, r, t, u, v)
                              sum2 = sum2 + eijx(pos1 + t) * sum3
                        end do tloop
                        sum1 = sum1 + eijy(pos2 + u) * sum2
                  end do uloop
                  cdcontrib_n = cdcontrib_n + eijz(pos3 + v) * sum1
            end do vloop
      end function cdcontrib_n

      
      pure function cdcontrib_auto(eijx, eijy, eijz, f, la, ma, na, lb, mb, nb, r, inner_loop)
            real(F64)                                       :: cdcontrib_auto
            real(F64), dimension(:), contiguous, intent(in) :: eijx, eijy, eijz
            real(F64), dimension(:), contiguous, intent(in) :: f
            integer, intent(in)                             :: la, ma, na
            integer, intent(in)                             :: lb, mb, nb
            real(F64), dimension(:, :, :), intent(in)       :: r
            procedure(inner_0)                              :: inner_loop

            integer :: t, u, v
            integer :: pos1, pos2, pos3
            real(F64) :: sum1, sum2, sum3

            pos1 = eijmatrix_position(la + lb, lb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(na + nb, nb, 0)
            cdcontrib_auto = ZERO
            vloop: do v = 0, na + nb
                  sum1 = ZERO
                  uloop: do u = 0, ma + mb
                        sum2 = ZERO
                        tloop: do t = 0, la + lb
                              call inner_loop(sum3, f, r, t, u, v)
                              sum2 = sum2 + eijx(pos1 + t) * sum3
                        end do tloop
                        sum1 = sum1 + eijy(pos2 + u) * sum2
                  end do uloop
                  cdcontrib_auto = cdcontrib_auto + eijz(pos3 + v) * sum1
            end do vloop
      end function cdcontrib_auto

      
      pure subroutine jab_contract(t, rho, gabcd, nab, ncd, scalab)
            real(F64), dimension(:), intent(out) :: t
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: gabcd
            integer, intent(in)                  :: nab, ncd
            real(F64), intent(in)                :: scalab
            
            real(F64) :: w
            integer :: r, s, v

            v = 1
            do r = 1, nab
                  w = ZERO
                  do  s = 1, ncd
                        w = w + rho(s) * gabcd(v)
                        v = v + 1
                  end do
                  t(r) = scalab * w
            end do
      end subroutine jab_contract


      pure subroutine jcd_contract(t, rho, gabcd, nab, ncd, scalcd)
            real(F64), dimension(:), intent(out) :: t
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: gabcd
            integer, intent(in)                  :: nab, ncd
            real(F64), intent(in)                :: scalcd

            real(F64) rab
            integer :: r, s, v

            v = 1
            t(1:ncd) = ZERO
            do r = 1, nab
                  rab = rho(r) * scalcd
                  do s = 1, ncd
                        t(s) = t(s) + rab * gabcd(v)
                        v = v + 1
                  end do
            end do
      end subroutine jcd_contract

      
      subroutine jeng_tile(jab, jcd, shell1, a, shell2, b, shell3, c, shell4, d, &
            rhoab, rhocd, calcab, calccd, scalab, scalcd)
            ! ----------------------------------------------------------
            ! Compute (AB| ) and/or ( |CD) contributions to the Coulomb
            ! matrix.
            ! ----------------------------------------------------------
            ! 1. T. Helgaker, Molecular Electronic-Structure
            !    Theory, eq. 9.9.33,
            ! 2. Reine, S.S., Density functional
            !    theory for large molecular systems,
            !    PhD thesis, University of Oslo 2009
            ! ---------------------------------------------------
            ! SHELL1,    - Indices of orbital shells
            ! SHELL2,
            ! SHELL3,
            ! SHELL4
            !
            ! A, B, C, D - Indices of atoms
            !
            ! JAB      -   Array of dimension (1:n), n >= NINTS
            ! JCD          in which electronic repulsion
            !              integrals are stored as an output
            !
            real(F64), dimension(:), intent(out) :: jab
            real(F64), dimension(:), intent(out) :: jcd
            integer, intent(in)                  :: a, b, c, d
            integer, intent(in)                  :: shell1, shell2
            integer, intent(in)                  :: shell3, shell4
            real(F64), dimension(:), intent(in)  :: rhoab, rhocd
            logical, intent(in)                  :: calcab, calccd
            real(F64), intent(in)                :: scalab, scalcd

            integer :: m1, m2, m3, m4
            real(F64), dimension(MAX_NFUNC**4) :: gabcd
            integer :: permutation, canonical
            integer :: nab, ncd

            m1 = shtype(shell1)
            m2 = shtype(shell2)
            m3 = shtype(shell3)
            m4 = shtype(shell4)
            nab = nfunc(m1) * nfunc(m2)
            ncd = nfunc(m3) * nfunc(m4)
            !
            ! NOTE THE INVERTED ORDER OF SHELL INDICES: (BA|DC).
            ! This ordering of two-electron integrals is required 
            ! for contraction with the density matrix
            !
            call eritype(permutation, canonical, m2, m1, m4, m3)

            select case (canonical)
            case (ERI_C_SSSS)
                  call ints2e_ssss(shell2, b, shell1, a, shell4, d, shell3, c, gabcd)
                  jab(1) = scalab * gabcd(1) * rhocd(1)
                  jcd(1) = scalcd * gabcd(1) * rhoab(1)
            case (ERI_C_PSSS)
                  call ints2e_spss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DSSS)
                  call ints2e_dsss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_PPSS)
                   call ints2e_ppss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                   if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                   if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_PPPS)
                  call ints2e_ppsp_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_PSPS)
                  call ints2e_spsp_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_PPPP)
                  call ints2e_pppp(shell2, b, shell1, a, shell4, d, shell3, c, gabcd)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DSPS)
                  call ints2e_dsps_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DDSS)
                  call ints2e_ddss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DSDS)
                  call ints2e_dsds_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DPPP)
                  call ints2e_dppp_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DPSS)
                  call ints2e_dpss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DPPS)
                  call ints2e_dpps_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DPDS)
                  call ints2e_dpds_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DSPP)
                  call ints2e_dspp_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DDDS)
                  call ints2e_ddds_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_DDPS)
                  call ints2e_ddps_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FSSS)
                  call ints2e_fsss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FSPS)
                  call ints2e_fsps_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FSDS)
                  call ints2e_fsds_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FPSS)
                  call ints2e_fpss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FDSS)
                  call ints2e_fdss_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FPPS)
                  call ints2e_fpps_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, m2, m1, m4, m3)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case (ERI_C_FSPP)
                  call ints2e_fspp_perm(shell2, b, shell1, a, shell4, d, shell3, c, gabcd, permutation)
                  if (calcab) call jab_contract(jab, rhocd, gabcd, nab, ncd, scalab)
                  if (calccd) call jcd_contract(jcd, rhoab, gabcd, nab, ncd, scalcd)
            case default
                  call jeng_tile_high_l(jab, jcd, shell1, a, shell2, b, shell3, c, shell4, d, &
                        rhoab, rhocd, calcab, calccd, scalab, scalcd)
            end select
      end subroutine jeng_tile


      subroutine jeng_tile_high_l(jab, jcd, shell1, a, shell2, b, shell3, c, shell4, d, &
            rhoab, rhocd, calcab, calccd, scalab, scalcd)
            ! ----------------------------------------------------------
            ! Compute contributions to the Coulomb matrix:
            ! Jab = SCALAB * \sum_{CD} (AB|CD) \rho_{CD}, 
            ! Jcd = SCALCD * \sum_{AB} (AB|CD) \rho_{AB}.
            ! ----------------------------------------------------------
            ! 1. T. Helgaker, Molecular Electronic-Structure
            !    Theory, eq. 9.9.33,
            ! 2. Reine, S.S., Density functional
            !    theory for large molecular systems,
            !    PhD thesis, University of Oslo 2009
            ! ---------------------------------------------------
            ! SHELL1,    - Indices of orbital shells
            ! SHELL2,
            ! SHELL3,
            ! SHELL4
            !
            ! A, B, C, D - Indices of atoms
            !
            ! JAB      -   Array of dimension (1:n), n >= NINTS
            ! JCD          in which electronic repulsion
            !              integrals are stored as an output
            !
            real(F64), dimension(:), intent(out) :: jab
            real(F64), dimension(:), intent(out) :: jcd
            integer, intent(in)                  :: a, b, c, d
            integer, intent(in)                  :: shell1, shell2
            integer, intent(in)                  :: shell3, shell4
            real(F64), dimension(:), intent(in)  :: rhoab, rhocd
            logical, intent(in)                  :: calcab, calccd
            real(F64), intent(in)                :: scalab, scalcd

            real(F64) :: alpha_a, p, q
            real(F64), dimension(MAX_NPRM) :: alpha_b, alpha_c, alpha_d
            real(F64), dimension(3) :: ra, rb, rc, rd
            real(F64), dimension(3) :: rp, rq, ara, brb, crc, drd
            real(F64), dimension(3) :: rpa, rpb, rqc, rqd, rpq
            real(F64) :: xabsq, yabsq, zabsq, xcdsq, ycdsq, zcdsq
            real(F64) :: alpha_reduced1, alpha_reduced2, alpha
            real(F64) :: kabcd, abexponent
            !
            ! Maximum value of a single index
            ! of Hermite Coulomb repulsion integral
            !
            integer, parameter :: max_index = 2 * max_l
            integer, parameter :: eij_dim = (2 * max_index**3 + 9 * max_index**2 + 13 * max_index + 6) / 6
            real(F64), dimension(eij_dim) :: eijx1, eijy1, eijz1, eijx2, eijy2, eijz2

            real(F64) :: const1
            integer :: i, j, k, l
            integer :: v1, v2, v3, v4, v
            integer :: momentum1, momentum2, momentum3, momentum4
            integer :: m12, m34
            integer :: nints1, nints2, nints3, nints4
            real(F64) :: c1, c2
            integer :: nab, ncd
            !
            ! Coulomb integrals by Hermite expansion
            !
            real(F64), dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            real(F64), dimension(max_chidx) :: fmarray

            real(F64), dimension(0:max_index, 0:max_index, 0:max_index) :: ftuv3idx
            integer, parameter :: max_lin_dim = ((max_index + 1) * (max_index + 2) * (max_index + 3)) / 6
            real(F64), dimension(max_lin_dim) :: ftuvab, ftuvcd

            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum3 = shtype(shell3)
            momentum4 = shtype(shell4)
            m12 = momentum1 + momentum2
            m34 = momentum3 + momentum4

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints3 = nfunc(momentum3)
            nints4 = nfunc(momentum4)

            ra = atomr(:, a)
            rb = atomr(:, b)
            rc = atomr(:, c)
            rd = atomr(:, d)

            associate ( &
                  lla => ll(:, momentum1), &
                  llb => ll(:, momentum2), &
                  llc => ll(:, momentum3), &
                  lld => ll(:, momentum4), &
                  mma => mm(:, momentum1), &
                  mmb => mm(:, momentum2), &
                  mmc => mm(:, momentum3), &
                  mmd => mm(:, momentum4), &
                  nna => nn(:, momentum1), &
                  nnb => nn(:, momentum2), &
                  nnc => nn(:, momentum3), &
                  nnd => nn(:, momentum4))

                  xabsq = (ra(1) - rb(1))**2
                  yabsq = (ra(2) - rb(2))**2
                  zabsq = (ra(3) - rb(3))**2
                  xcdsq = (rc(1) - rd(1))**2
                  ycdsq = (rc(2) - rd(2))**2
                  zcdsq = (rc(3) - rd(3))**2

                  nab = nints1 * nints2
                  ncd = nints3 * nints4
                  if (calcab) jab(1:nab) = ZERO
                  if (calccd) jcd(1:ncd) = ZERO

                  do j = 1, NPRM(shell2)
                        alpha_b(j) = expn(j, shell2)
                  end do

                  do k = 1, NPRM(shell3)
                        alpha_c(k) = expn(k, shell3)
                  end do

                  do l = 1, NPRM(shell4)
                        alpha_d(l) = expn(l, shell4)
                  end do

                  do i = 1, nprm(shell1)
                        alpha_a = expn(i, shell1)
                        ara = alpha_a * ra

                        do j = 1, nprm(shell2)
                              brb = alpha_b(j) * rb

                              p = alpha_a + alpha_b(j)
                              alpha_reduced1 = alpha_a * alpha_b(j) / p

                              abexponent = alpha_reduced1 * (xabsq + yabsq + zabsq)

                              rp = (ara + brb) / p
                              rpa = rp - ra
                              rpb = rp - rb
                              !
                              ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                              ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook
                              ! because the coeffs are linear functions of the seed so
                              ! it may be incorporated into const1.
                              !
                              call eijmatrix(momentum1 + momentum2, one, p, rpa(1), rpb(1), eijx1)
                              call eijmatrix(momentum1 + momentum2, one, p, rpa(2), rpb(2), eijy1)
                              call eijmatrix(momentum1 + momentum2, one, p, rpa(3), rpb(3), eijz1)
                              !
                              ! Contract AB indices
                              !
                              if (calccd) then
                                    call ftuv(rhoab, eijx1, eijy1, eijz1, ftuv3idx, &
                                          nints1, lla, mma, nna, nrml(:, i, shell1), &
                                          nints2, llb, mmb, nnb, nrml(:, j, shell2))
                                    call linear_arrange(ftuvab, ftuv3idx, m12)
                              end if

                              do k = 1, nprm(shell3)
                                    crc = alpha_c(k) * rc

                                    do l = 1, nprm(shell4)
                                          drd = alpha_d(l) * rd

                                          q = alpha_c(k) + alpha_d(l)
                                          alpha_reduced2 = alpha_c(k) * alpha_d(l) / q

                                          kabcd = exp(-alpha_reduced2 * (xcdsq + ycdsq + zcdsq) &
                                                - abexponent)

                                          rq = (crc + drd) / q
                                          rqc = rq - rc
                                          rqd = rq - rd

                                          alpha = p * q / (p + q)
                                          rpq = rp - rq

                                          const1 = two * pi52 / (p * q * sqrt(p + q)) * kabcd

                                          call eijmatrix(momentum3 + momentum4, one, q, rqc(1), rqd(1), eijx2)
                                          call eijmatrix(momentum3 + momentum4, one, q, rqc(2), rqd(2), eijy2)
                                          call eijmatrix(momentum3 + momentum4, one, q, rqc(3), rqd(3), eijz2)

                                          call fm(momentum1 + momentum2 + momentum3 + momentum4, &
                                                alpha * dot_product(rpq, rpq), fmarray)
                                          call chints(momentum1 + momentum2 + momentum3 + momentum4, &
                                                fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)
                                          !
                                          ! CD indices contracted with the density matrix.
                                          ! Jab block (column-major order)
                                          !
                                          if (calcab) then
                                                call ftuv(rhocd, eijx2, eijy2, eijz2, ftuv3idx, &
                                                      nints3, llc, mmc, nnc, nrml(:, k, shell3), &
                                                      nints4, lld, mmd, nnd, nrml(:, l, shell4))
                                                !
                                                ! Multiply each element of Ftuv matrix by
                                                ! (-1)**(t+u+v) factor
                                                !
                                                call ftuv_phase(ftuvcd, ftuv3idx, momentum3 + momentum4)
                                                v = 1
                                                select case (m34)
                                                case (0)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_0)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (1)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_1)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (2)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_2)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (3)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_3)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (4)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_4)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (5)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_5)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (6)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_6)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (7)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_7)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (8)
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_auto(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), rtuv, inner_8)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case default
                                                      do v2 = 1, nints2
                                                            c2 = nrml(v2, j, shell2) * const1 * scalab
                                                            do v1 = 1, nints1
                                                                  c1 = nrml(v1, i, shell1)
                                                                  jab(v) = jab(v) + c1 * c2 * &
                                                                        cdcontrib_n(eijx1, eijy1, eijz1, ftuvcd, &
                                                                        lla(v1), mma(v1), nna(v1), llb(v2), mmb(v2), &
                                                                        nnb(v2), m34, rtuv)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                end select
                                          end if
                                          !
                                          ! AB indices contracted with the density matrix.
                                          ! Jcd block (column-major order)
                                          !
                                          if (calccd) then !(iand(flags, COUL_PERM_CDAB) .gt. 0) then
                                                v = 1
                                                select case (m12)
                                                case (0)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_0)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (1)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_1)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (2)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_2)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (3)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_3)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (4)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_4)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (5)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_5)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (6)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_6)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (7)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_7)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case (8)
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_auto(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), rtuv, inner_8)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                case default
                                                      do v4 = 1, nints4
                                                            c2 = nrml(v4, l, shell4) * const1 * scalcd
                                                            do v3 = 1, nints3
                                                                  c1 = nrml(v3, k, shell3)
                                                                  jcd(v) = jcd(v) + c1 * c2 * &
                                                                        abcontrib_n(eijx2, eijy2, eijz2, ftuvab, &
                                                                        llc(v3), mmc(v3), nnc(v3), lld(v4), mmd(v4), &
                                                                        nnd(v4), m12, rtuv)
                                                                  v = v + 1
                                                            end do
                                                      end do
                                                end select
                                          end if
                                    end do
                              end do
                        end do
                  end do
            end associate
      end subroutine jeng_tile_high_l
!end module jengine

! ==========================================================================
! ----------------------- OLD, WELL-TESTED J-ENGINE ------------------------
! ==========================================================================
! module jengine
!       use math_constants
!       use boys
!       use hermite

!       implicit none

! contains

      function abcontrib(eijx, eijy, eijz, f, lc, mc, nc, ld, md, nd, mab, r)
            double precision                                    :: abcontrib
            double precision, dimension(:), intent(in)          :: eijx, eijy, eijz
            double precision, dimension(0:, 0:, 0:), intent(in) :: f
            integer, intent(in)                                 :: lc, mc, nc
            integer, intent(in)                                 :: ld, md, nd
            integer, intent(in)                                 :: mab
            double precision, dimension(:, :, :), intent(in)    :: r

            integer :: t, u, v, tau, nu, phi
            integer :: pos1, pos2, pos3
            double precision :: sum1, sum2, sum3
            double precision :: etau, enu, ephi
            double precision :: phasephi, phasenu, phase

            pos1 = eijmatrix_position(lc + ld, ld, 0)
            pos2 = eijmatrix_position(mc + md, md, 0)
            pos3 = eijmatrix_position(nc + nd, nd, 0)
            abcontrib = zero

            phasephi = one
            do phi = 0, nc + nd
                  ephi = eijz(pos3 + phi)
                  sum1 = zero
                  phasenu = phasephi
                  do nu = 0, mc + md
                        enu = eijy(pos2 + nu)
                        sum2 = zero
                        phase = phasenu
                        do tau = 0, lc + ld
                              etau = eijx(pos1 + tau)
                              sum3 = zero
                              do v = 0, mab
                                    do u = 0, mab - v
                                          do t = 0, mab - v - u
                                                sum3 = sum3 + f(t, u, v) * &
                                                      r(t + tau + 1, u + nu + 1, phi + v + 1)
                                          end do
                                    end do
                              end do
                              sum2 = sum2 + phase * etau * sum3
                              phase = -phase
                        end do
                        phasenu = -phasenu
                        sum1 = sum1 + enu * sum2
                  end do
                  phasephi = -phasephi
                  abcontrib = abcontrib + ephi * sum1
            end do
      end function abcontrib


      function cdcontrib(eijx, eijy, eijz, f, la, ma, na, lb, mb, nb, mcd, r)
            double precision                                    :: cdcontrib
            double precision, dimension(:), intent(in)          :: eijx, eijy, eijz
            double precision, dimension(0:, 0:, 0:), intent(in) :: f
            integer, intent(in)                                 :: la, ma, na
            integer, intent(in)                                 :: lb, mb, nb
            integer, intent(in)                                 :: mcd
            double precision, dimension(:, :, :), intent(in)    :: r

            integer :: t, u, v, tau, nu, phi
            integer :: pos1, pos2, pos3
            double precision :: sum1, sum2, sum3
            double precision :: et, eu, ev

            pos1 = eijmatrix_position(la + lb, lb, 0)
            pos2 = eijmatrix_position(ma + mb, mb, 0)
            pos3 = eijmatrix_position(na + nb, nb, 0)
            cdcontrib = zero

            vloop: do v = 0, na + nb
                  ev = eijz(pos3 + v)
                  sum1 = zero
                  uloop: do u = 0, ma + mb
                        eu = eijy(pos2 + u)
                        sum2 = zero
                        tloop: do t = 0, la + lb
                              et = eijx(pos1 + t)
                              sum3 = zero
                              do phi = 0, mcd
                                    do nu = 0, mcd - phi
                                          do tau = 0, mcd - phi - nu
                                                sum3 = sum3 + f(tau, nu, phi) * &
                                                      r(t + tau + 1, u + nu + 1, phi + v + 1)
                                          end do
                                    end do
                              end do
                              sum2 = sum2 + et * sum3
                        end do tloop
                        sum1 = sum1 + eu * sum2
                  end do uloop
                  cdcontrib = cdcontrib + ev * sum1
            end do vloop
      end function cdcontrib


      subroutine ftuv_old(rhoab, eijx, eijy, eijz, f, &
            ninta, lla, mma, nna, coeffa, &
            nintb, llb, mmb, nnb, coeffb)

            double precision, dimension(:)             :: rhoab
            double precision, dimension(:), intent(in) :: eijx, eijy, eijz
            double precision, dimension(0:, 0:, 0:)    :: f
            integer, intent(in)                        :: ninta
            integer, dimension(:), intent(in)          :: lla, mma, nna
            double precision, dimension(:), intent(in) :: coeffa
            integer, intent(in)                        :: nintb
            integer, dimension(:), intent(in)          :: llb, mmb, nnb
            double precision, dimension(:), intent(in) :: coeffb

            double precision :: ev, euv, etuv
            double precision :: ca, cb, cab
            integer :: pos1, pos2, pos3
            integer :: t, u, v
            integer :: a, b
            integer :: la, ma, na, lb, mb, nb
            integer :: idxab

            f = zero
            idxab = 1

            do b = 1, nintb
                  lb = llb(b)
                  mb = mmb(b)
                  nb = nnb(b)

                  cb = coeffb(b)

                  do a = 1, ninta
                        la = lla(a)
                        ma = mma(a)
                        na = nna(a)

                        ca = coeffa(a)
                        cab = ca * cb * rhoab(idxab)
                        idxab = idxab + 1

                        pos1 = eijmatrix_position(la + lb, lb, 0)
                        pos2 = eijmatrix_position(ma + mb, mb, 0)
                        pos3 = eijmatrix_position(na + nb, nb, 0)

                        do v = 0, na + nb
                              ev = cab * eijz(pos3 + v)
                              do u = 0, ma + mb
                                    euv = eijy(pos2 + u) * ev
                                    do t = 0, la + lb
                                          etuv = eijx(pos1 + t) * euv
                                          f(t, u, v) = f(t, u, v) + etuv
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine ftuv_old

      
      subroutine ftuv_phase_old(f, momentum)
            double precision, dimension(0:, 0:, 0:), intent(inout) :: f
            integer, intent(in)                                    :: momentum

            integer :: t, u, v
            double precision :: phase, phasev, phaseu

            phasev = one
            do v = 0, momentum
                  phaseu = phasev
                  do u = 0, momentum - v
                        phase = phaseu
                        do t = 0, momentum - v - u
                              f(t, u, v) = f(t, u, v) * phase
                              phase = -phase
                        end do
                        phaseu = -phaseu
                  end do
                  phasev = -phasev
            end do
      end subroutine ftuv_phase_old


!       subroutine jeng(shell1, a, shell2, b, shell3, c, shell4, d, &
!             rhoab, rhocd, jab, jcd, calcab, calccd, scalab, scalcd)
!             !
!             ! ---------------------------------------------------
!             ! Calculate contributions to Coulomb operator matrix
!             ! ---------------------------------------------------
!             ! 1. T. Helgaker, Molecular Electronic-Structure
!             !    Theory, eq. 9.9.33,
!             ! 2. Reine, S.S., Density functional
!             !    theory for large molecular systems,
!             !    PhD thesis, University of Oslo 2009
!             ! ---------------------------------------------------
!             ! SHELL1,    - Indices of shells
!             ! SHELL2,
!             ! SHELL3,
!             ! SHELL4
!             !
!             ! DENSITY    - DENSITY MATRIX (ONLY LOWER-DIAGONAL
!             !              PART IS SIGNIFICANT)
!             !
!             ! A, B, C, D - Indices of atoms
!             !
!             ! JAB      -   Array of dimension (1:n), n >= NINTS
!             !              in which electronic repulsion
!             !              integrals are stored as an output
!             !
!             integer, intent(in)                         :: a, b, c, d
!             integer, intent(in)                         :: shell1, shell2
!             integer, intent(in)                         :: shell3, shell4
!             double precision, dimension(:), intent(in)  :: rhoab, rhocd
!             double precision, dimension(:), intent(out) :: jab, jcd
!             logical, intent(in)                         :: calcab, calccd
!             double precision, intent(in)                :: scalab, scalcd

!             double precision :: alpha_a, alpha_b, alpha_c, alpha_d, p, q
!             double precision, dimension(3) :: ra, rb, rc, rd
!             double precision, dimension(3) :: rp, rq, ara, brb, crc, drd
!             double precision, dimension(3) :: rpa, rpb, rqc, rqd, rpq
!             double precision :: xabsq, yabsq, zabsq, xcdsq, ycdsq, zcdsq
!             double precision :: alpha_reduced1, alpha_reduced2, alpha
!             double precision :: kabcd, abexponent
!             !
!             ! Maximum value of single index
!             ! of Hermite Coulomb repulsion integral
!             !
!             integer, parameter :: max_index = 2 * max_l

!             double precision, dimension((2 * max_index**3 + &
!                   9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx1, eijy1, eijz1, eijx2, eijy2, eijz2

!             double precision :: const1
!             integer, dimension(:), pointer :: lla, llb, mma, mmb, nna, nnb
!             integer, dimension(:), pointer :: llc, lld, mmc, mmd, nnc, nnd
!             integer :: i, j, k, l
!             integer :: v1, v2, v3, v4, v
!             integer :: la, lb, ma, mb, na, nb, lc, ld, mc, md, nc, nd
!             integer :: momentum1, momentum2, momentum3, momentum4
!             integer :: nints1, nints2, nints3, nints4
!             double precision :: c1, c2
!             !
!             ! Coulomb integrals by Hermite expansion
!             !
!             double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
!             double precision, dimension(max_chidx) :: fmarray

!             double precision, dimension(0:max_index, 0:max_index, 0:max_index) :: ftuvab
!             double precision, dimension(0:max_index, 0:max_index, 0:max_index) :: ftuvcd

!             momentum1 = shtype(shell1)
!             momentum2 = shtype(shell2)
!             momentum3 = shtype(shell3)
!             momentum4 = shtype(shell4)

!             nints1 = nfunc(momentum1)
!             nints2 = nfunc(momentum2)
!             nints3 = nfunc(momentum3)
!             nints4 = nfunc(momentum4)

!             ra = atomr(:, a)
!             rb = atomr(:, b)
!             rc = atomr(:, c)
!             rd = atomr(:, d)

!             lla => ll(:, momentum1)
!             llb => ll(:, momentum2)
!             llc => ll(:, momentum3)
!             lld => ll(:, momentum4)

!             mma => mm(:, momentum1)
!             mmb => mm(:, momentum2)
!             mmc => mm(:, momentum3)
!             mmd => mm(:, momentum4)

!             nna => nn(:, momentum1)
!             nnb => nn(:, momentum2)
!             nnc => nn(:, momentum3)
!             nnd => nn(:, momentum4)

!             xabsq = (ra(1) - rb(1))**2
!             yabsq = (ra(2) - rb(2))**2
!             zabsq = (ra(3) - rb(3))**2
!             xcdsq = (rc(1) - rd(1))**2
!             ycdsq = (rc(2) - rd(2))**2
!             zcdsq = (rc(3) - rd(3))**2

!             jab(1:nints1 * nints2) = zero
!             jcd(1:nints3 * nints4) = zero

!             do i = 1, nprm(shell1)
!                   alpha_a = expn(i, shell1)
!                   ara = alpha_a * ra

!                   do j = 1, nprm(shell2)
!                         alpha_b = expn(j, shell2)
!                         brb = alpha_b * rb

!                         p = alpha_a + alpha_b
!                         alpha_reduced1 = alpha_a * alpha_b / p

!                         abexponent = alpha_reduced1 * (xabsq + yabsq + zabsq)

!                         rp = (ara + brb) / p
!                         rpa = rp - ra
!                         rpb = rp - rb
!                         !
!                         ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
!                         ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook
!                         ! because the coeffs are linear functions of the seed so
!                         ! it may be incorporated into const1.
!                         !
!                         call eijmatrix(momentum1 + momentum2, one, p, rpa(1), rpb(1), eijx1)
!                         call eijmatrix(momentum1 + momentum2, one, p, rpa(2), rpb(2), eijy1)
!                         call eijmatrix(momentum1 + momentum2, one, p, rpa(3), rpb(3), eijz1)
!                         !
!                         ! Contract AB indices
!                         !
!                         if (calccd) then !(iand(flags, COUL_PERM_CDAB) .gt. 0) then
!                               call ftuv(rhoab, eijx1, eijy1, eijz1, ftuvab, &
!                                     nints1, lla, mma, nna, nrml(:, i, shell1), &
!                                     nints2, llb, mmb, nnb, nrml(:, j, shell2))
!                         end if

!                         do k = 1, nprm(shell3)
!                               alpha_c = expn(k, shell3)
!                               crc = alpha_c * rc

!                               do l = 1, nprm(shell4)
!                                     alpha_d = expn(l, shell4)
!                                     drd = alpha_d * rd

!                                     q = alpha_c + alpha_d
!                                     alpha_reduced2 = alpha_c * alpha_d / q

!                                     kabcd = exp(-alpha_reduced2 * (xcdsq + ycdsq + zcdsq) &
!                                           - abexponent)

!                                     rq = (crc + drd) / q
!                                     rqc = rq - rc
!                                     rqd = rq - rd

!                                     alpha = p * q / (p + q)
!                                     rpq = rp - rq

!                                     const1 = two * pi52 / (p * q * sqrt(p + q)) * kabcd

!                                     call eijmatrix(momentum3 + momentum4, one, q, rqc(1), rqd(1), eijx2)
!                                     call eijmatrix(momentum3 + momentum4, one, q, rqc(2), rqd(2), eijy2)
!                                     call eijmatrix(momentum3 + momentum4, one, q, rqc(3), rqd(3), eijz2)

!                                     call fm(momentum1 + momentum2 + momentum3 + momentum4, &
!                                           alpha * dot_product(rpq, rpq), fmarray)
!                                     call chints(momentum1 + momentum2 + momentum3 + momentum4, &
!                                           fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)
!                                     !
!                                     ! CD indices contracted
!                                     ! Jab block (column-major order)
!                                     !
!                                     if (calcab) then !(iand(flags, COUL_PERM_ABCD) .gt. 0) then
!                                           call ftuv(rhocd, eijx2, eijy2, eijz2, ftuvcd, &
!                                                 nints3, llc, mmc, nnc, nrml(:, k, shell3), &
!                                                 nints4, lld, mmd, nnd, nrml(:, l, shell4))
!                                           !
!                                           ! Multiply each element of Ftuv matrix by
!                                           ! (-1)**(t+u+v) factor
!                                           !
!                                           call ftuv_phase(ftuvcd, momentum3 + momentum4)
!                                           v = 1
!                                           do v2 = 1, nints2
!                                                 lb = llb(v2)
!                                                 mb = mmb(v2)
!                                                 nb = nnb(v2)
!                                                 c2 = nrml(v2, j, shell2) * const1 * scalab

!                                                 do v1 = 1, nints1
!                                                       la = lla(v1)
!                                                       ma = mma(v1)
!                                                       na = nna(v1)
!                                                       c1 = nrml(v1, i, shell1)
                                                
!                                                       jab(v) = jab(v) + c1 * c2 * &
!                                                             cdcontrib(eijx1, eijy1, eijz1, ftuvcd, &
!                                                             la, ma, na, lb, mb, nb, momentum3 + momentum4, rtuv)

!                                                       v = v + 1
!                                                 end do
!                                           end do
!                                     end if
!                                     !
!                                     ! AB indices contracted
!                                     ! Jcd block (column-major order)
!                                     !
!                                     if (calccd) then !(iand(flags, COUL_PERM_CDAB) .gt. 0) then
!                                           v = 1
!                                           do v4 = 1, nints4
!                                                 ld = lld(v4)
!                                                 md = mmd(v4)
!                                                 nd = nnd(v4)
!                                                 c2 = nrml(v4, l, shell4) * const1 * scalcd

!                                                 do v3 = 1, nints3
!                                                       lc = llc(v3)
!                                                       mc = mmc(v3)
!                                                       nc = nnc(v3)
!                                                       c1 = nrml(v3, k, shell3)

!                                                       jcd(v) = jcd(v) + c1 * c2 * &
!                                                             abcontrib(eijx2, eijy2, eijz2, ftuvab, &
!                                                             lc, mc, nc, ld, md, nd, momentum1 + momentum2, rtuv)
!                                                       v = v + 1
!                                                 end do
!                                           end do
!                                     end if
!                               end do
!                         end do
!                   end do
!             end do
!       end subroutine jeng


      subroutine jeng_tile_old(jab, jcd, shell1, a, shell2, b, shell3, c, shell4, d, &
            rhoab, rhocd, calcab, calccd, scalab, scalcd)
            !
            ! ---------------------------------------------------
            ! Calculate contributions to Coulomb operator matrix.
            ! 
            ! ---------------------------------------------------
            ! 1. T. Helgaker, Molecular Electronic-Structure
            !    Theory, eq. 9.9.33,
            ! 2. Reine, S.S., Density functional
            !    theory for large molecular systems,
            !    PhD thesis, University of Oslo 2009
            ! ---------------------------------------------------
            ! SHELL1,    - Indices of orbital shells
            ! SHELL2,
            ! SHELL3,
            ! SHELL4
            !
            ! A, B, C, D - Indices of atoms
            !
            ! JAB      -   Array of dimension (1:n), n >= NINTS
            ! JCD          in which electronic repulsion
            !              integrals are stored as an output
            !
            double precision, dimension(:), intent(out) :: jab
            double precision, dimension(:), intent(out) :: jcd
            integer, intent(in)                         :: a, b, c, d
            integer, intent(in)                         :: shell1, shell2
            integer, intent(in)                         :: shell3, shell4
            double precision, dimension(:), intent(in)  :: rhoab, rhocd
            logical, intent(in)                         :: calcab, calccd
            double precision, intent(in)                :: scalab, scalcd

            double precision :: alpha_a, alpha_b, alpha_c, alpha_d, p, q
            double precision, dimension(3) :: ra, rb, rc, rd
            double precision, dimension(3) :: rp, rq, ara, brb, crc, drd
            double precision, dimension(3) :: rpa, rpb, rqc, rqd, rpq
            double precision :: xabsq, yabsq, zabsq, xcdsq, ycdsq, zcdsq
            double precision :: alpha_reduced1, alpha_reduced2, alpha
            double precision :: kabcd, abexponent
            !
            ! Maximum value of single index
            ! of Hermite Coulomb repulsion integral
            !
            integer, parameter :: max_index = 2 * max_l

            double precision, dimension((2 * max_index**3 + &
                  9 * max_index**2 + 13 * max_index + 6) / 6) :: eijx1, eijy1, eijz1, eijx2, eijy2, eijz2

            double precision :: const1
            integer, dimension(:), pointer :: lla, llb, mma, mmb, nna, nnb
            integer, dimension(:), pointer :: llc, lld, mmc, mmd, nnc, nnd
            integer :: i, j, k, l
            integer :: v1, v2, v3, v4, v
            integer :: la, lb, ma, mb, na, nb, lc, ld, mc, md, nc, nd
            integer :: momentum1, momentum2, momentum3, momentum4
            integer :: nints1, nints2, nints3, nints4
            double precision :: c1, c2
            integer :: nab, ncd
            !
            ! Coulomb integrals by Hermite expansion
            !
            double precision, dimension(max_chidx, max_chidx, max_chidx) :: rtuv
            double precision, dimension(max_chidx) :: fmarray

            double precision, dimension(0:max_index, 0:max_index, 0:max_index) :: ftuvab
            double precision, dimension(0:max_index, 0:max_index, 0:max_index) :: ftuvcd

            momentum1 = shtype(shell1)
            momentum2 = shtype(shell2)
            momentum3 = shtype(shell3)
            momentum4 = shtype(shell4)

            nints1 = nfunc(momentum1)
            nints2 = nfunc(momentum2)
            nints3 = nfunc(momentum3)
            nints4 = nfunc(momentum4)

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

            nab = nints1 * nints2
            ncd = nints3 * nints4
            if (calcab) jab(1:nab) = ZERO
            if (calccd) jcd(1:ncd) = ZERO

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
                        !
                        ! Seed for calculating E^{ij}_t coeffs is 1.d+0 instead of
                        ! exp(-alpha_reduces * xabsq) as in Helgaker's textbook
                        ! because the coeffs are linear functions of the seed so
                        ! it may be incorporated into const1.
                        !
                        call eijmatrix(momentum1 + momentum2, one, p, rpa(1), rpb(1), eijx1)
                        call eijmatrix(momentum1 + momentum2, one, p, rpa(2), rpb(2), eijy1)
                        call eijmatrix(momentum1 + momentum2, one, p, rpa(3), rpb(3), eijz1)
                        !
                        ! Contract AB indices
                        !
                        if (calccd) then
                              call ftuv_old(rhoab, eijx1, eijy1, eijz1, ftuvab, &
                                    nints1, lla, mma, nna, nrml(:, i, shell1), &
                                    nints2, llb, mmb, nnb, nrml(:, j, shell2))
                        end if

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

                                    alpha = p * q / (p + q)
                                    rpq = rp - rq

                                    const1 = two * pi52 / (p * q * sqrt(p + q)) * kabcd

                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(1), rqd(1), eijx2)
                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(2), rqd(2), eijy2)
                                    call eijmatrix(momentum3 + momentum4, one, q, rqc(3), rqd(3), eijz2)

                                    call fm(momentum1 + momentum2 + momentum3 + momentum4, &
                                          alpha * dot_product(rpq, rpq), fmarray)
                                    call chints(momentum1 + momentum2 + momentum3 + momentum4, &
                                          fmarray, rtuv, rpq(1), rpq(2), rpq(3), alpha)
                                    !
                                    ! CD indices contracted
                                    ! Jab block (column-major order)
                                    !
                                    if (calcab) then
                                          call ftuv_old(rhocd, eijx2, eijy2, eijz2, ftuvcd, &
                                                nints3, llc, mmc, nnc, nrml(:, k, shell3), &
                                                nints4, lld, mmd, nnd, nrml(:, l, shell4))
                                          !
                                          ! Multiply each element of Ftuv matrix by
                                          ! (-1)**(t+u+v) factor
                                          !
                                          call ftuv_phase_old(ftuvcd, momentum3 + momentum4)
                                          v = 1
                                          do v2 = 1, nints2
                                                lb = llb(v2)
                                                mb = mmb(v2)
                                                nb = nnb(v2)
                                                c2 = nrml(v2, j, shell2) * const1 * scalab

                                                do v1 = 1, nints1
                                                      la = lla(v1)
                                                      ma = mma(v1)
                                                      na = nna(v1)
                                                      c1 = nrml(v1, i, shell1)
                                                
                                                      jab(v) = jab(v) + c1 * c2 * &
                                                            cdcontrib(eijx1, eijy1, eijz1, ftuvcd, &
                                                            la, ma, na, lb, mb, nb, momentum3 + momentum4, rtuv)

                                                      v = v + 1
                                                end do
                                          end do
                                    end if
                                    !
                                    ! AB indices contracted
                                    ! Jcd block (column-major order)
                                    !
                                    if (calccd) then !(iand(flags, COUL_PERM_CDAB) .gt. 0) then
                                          v = 1
                                          do v4 = 1, nints4
                                                ld = lld(v4)
                                                md = mmd(v4)
                                                nd = nnd(v4)
                                                c2 = nrml(v4, l, shell4) * const1 * scalcd

                                                do v3 = 1, nints3
                                                      lc = llc(v3)
                                                      mc = mmc(v3)
                                                      nc = nnc(v3)
                                                      c1 = nrml(v3, k, shell3)

                                                      jcd(v) = jcd(v) + c1 * c2 * &
                                                            abcontrib(eijx2, eijy2, eijz2, ftuvab, &
                                                            lc, mc, nc, ld, md, nd, momentum1 + momentum2, rtuv)
                                                      v = v + 1
                                                end do
                                          end do
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine jeng_tile_old
end module jengine
