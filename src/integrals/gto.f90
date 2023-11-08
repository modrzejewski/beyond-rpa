module gto
      use math_constants
      use gparam
      use specf

      implicit none
      save

      double precision, dimension(:, :, :), allocatable :: sphercoeff
      
contains

      subroutine gto_init()
            integer :: l

            allocate(sphercoeff(max_nfunc, 2 * max_l + 1, 0:max_l))

            do l = 0, max_l
                  call sphergto(sphercoeff(:, :, l), l)
            end do
      end subroutine gto_init

      
      subroutine gto_free()
            deallocate(sphercoeff)
      end subroutine gto_free


      pure function nfunc(t)
            ! ----------------------------------------
            ! Calculate the number of Cartesian GTOs
            ! belonging to a given Cartesian angular
            ! momentum.
            ! ----------------------------------------
            ! T
            !    Angular momentum: 0 for s, 1 for p,
            !    2 for d, ...
            !
            integer             :: nfunc
            integer, intent(in) :: t

            nfunc = ((t + 1) * (t + 2)) / 2
      end function nfunc


      subroutine cartpoly(momentum, l, m, n)
            ! -------------------------------------------
            ! Define order of Cartesian GTOs belonging
            ! to a given Cartesian angular momentum. Get
            ! sequence of exponents l, m, n defining
            ! Cartesian polynomials:
            ! G_{lmn} = x^l y^m z^n exp(-\alpha r^2),
            ! l + m + n = MOMENTUM
            ! 
            integer, intent(in) :: momentum
            integer, dimension(:), intent(out) :: l, m, n

            integer :: i, j
            integer :: idx

            l = 0
            m = 0
            n = 0
            idx = 1
            do i = momentum, 0, -1
                  do j = momentum - i, 0, -1
                        l(idx) = i
                        m(idx) = j
                        n(idx) = momentum - i - j
                        idx = idx + 1
                  end do
            end do
      end subroutine cartpoly


      function nslm(l, m)
            double precision    :: nslm
            integer, intent(in) :: l, m
            
            double precision :: sqra, sqrb, sqrc
            
            sqra = sqrt(two * dble(ifact(l + abs(m))))
            sqrb = sqrt(dble(ifact(l - abs(m))))
            if (m .eq. 0) then
                  sqrc = sqrt(two)
            else
                  sqrc = one
            end if
            
            nslm = sqra * sqrb / sqrc / (dble(2**abs(m)) * dble(ifact(l)))
      end function nslm
      
      
      function vm(m)
            integer             :: vm
            integer, intent(in) :: m

            if (m .ge. 0) then
                  vm = 0
            else
                  vm = 1
            end if
      end function vm


      function clmtuv(l, m, t, u, v)
            double precision    :: clmtuv
            integer, intent(in) :: l, m, t, u, v
            
            integer :: a, b, c
            
            a = ibinom(l, t) * ibinom(l - t, abs(m) + t)
            b = ibinom(t, u) * ibinom(abs(m), v)
            c = (-1)**(t + (v - vm(m)) / 2) * 4**t

            clmtuv = dble(a * b) / dble(c)
      end function clmtuv


      subroutine sphergto(coeff, l)
            ! --------------------------------------------------------
            ! Calculate coefficients of transformation from Cartesian
            ! Gaussians to spherical-harmonic GTOs (real-valued
            ! solid harmonics):
            ! G_{lm} = \sum_{tuv} C^{lm}_{tuv} G_{tuv},
            ! G_{tuv} = x^t y^u z^v exp(-\alpha r^2).
            ! A compound index is used for enumerating Cartesian GTOs
            ! belonging to a given Cartesian angular momentum.
            ! --------------------------------------------------------
            ! 1. Helgaker, T., Jorgensen, P., Olsen, J., Molecular
            !    Electronic-Structure Theory, Wiley & Sons Chichester
            !    2000. Eqs. 6.4.47-6.4.50, p. 215
            ! --------------------------------------------------------
            ! COEFF - Output, COEFF(IDX(T, U, V), M) = C^{lm}_{tuv}
            ! L     - Input, angular momentum. T + U + V = L
            !
            double precision, dimension(:, :), intent(out) :: coeff
            integer, intent(in)                            :: l
            
            integer, dimension(0:max_l, 0:max_l, 0:max_l) :: cart_idx
            integer, dimension(max_nfunc) :: cart_t, cart_u, cart_v
            integer :: k, m, t, u, v
            integer :: x, y, z
            integer :: i
            integer :: absm

            call cartpoly(l, cart_t, cart_u, cart_v)
            !
            ! Compute CART_IDX matrix which stores
            ! index K of a Cartesian Gaussian function:
            ! G_k^L = x^t y^u z^v exp(-\alpha r^2) =>
            ! CART_LMN(T, U, V) = K,
            ! G_k^L - k-th Cartesian Gaussian in belonging
            ! to shell of Cartesian angular momentum L
            !
            do k = 1, nfunc(l)
                  cart_idx(cart_t(k), cart_u(k), cart_v(k)) = k
            end do
            
            coeff = zero

            do m = -l, l
                  absm = abs(m)

                  do t = 0, (l - absm) / 2
                        do u = 0, t
                              do k = 0, (absm - vm(m)) / 2
                                    v = 2 * k + vm(m)

                                    x = 2 * t + absm - 2 * u - v
                                    y = 2 * u + v
                                    z = l - 2 * t - absm

                                    i = cart_idx(x, y, z)
                                    coeff(i, m + l + 1) = coeff(i, m + l + 1) &
                                          + nslm(l, m) * clmtuv(l, m, t, u, v)
                              end do
                        end do
                  end do
            end do
      end subroutine sphergto
end module gto
