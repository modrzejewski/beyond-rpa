module screen
      use math_constants
      use arithmetic
      use gparam
      use sort
      use ints

      implicit none

contains

      subroutine screenint(a, ia)
            ! ------------------------------------------------------
            ! Sort two-electron integrals according to decreasing
            ! absolute values. Sorting is carried out inside atomic
            ! subsets. Example:
            ! * A(SH0(K1), b) = \max_a SQRT(|(ab|ab)|) (a restricted 
            !   to shells centered at atom K1) 
            ! * A(SH0(K1)+1, b) = second largest SQRT(|(ab|ab)|)
            ! ...
            !
            ! In our notation (ab|ab) is the representative of
            ! a shell quarted, i.e., the largest integral that set.
            ! ------------------------------------------------------
            ! 1. Ahmadi, G.R., Almlof, J., The Coulomb operator
            !    in a Gaussian product basis,
            !    Chem. Phys. Lett. 246, 364(1995), Eq. 28
            ! ------------------------------------------------------
            ! A  - Output, see the above description
            !
            ! IA - Output, contains indices of integrals, see
            !      above description
            !
            !
            real(F64), dimension(:, :), intent(out) :: a
            integer, dimension(:, :), intent(out)          :: ia

            real(F64), dimension(max_nfunc**4) :: intarray
            integer :: shell1, shell2
            integer :: momentum1, momentum2
            integer :: nint1, nint2, nint
            integer :: la, lb
            integer :: k0, k1
            integer :: ka, kb
            integer :: u

            do ka = 1, natom
                  do kb = 1, ka
                        do la = sh0(ka), sh0(ka + 1) - 1
                              shell1 = sh(la)
                              momentum1 = shtype(shell1)
                              nint1 = nfunc(momentum1)
                              lb = sh0(kb)
                              do while ((lb .lt. sh0(kb + 1)) .and. (lb .le. la))
                                    shell2 = sh(lb)
                                    momentum2 = shtype(shell2)
                                    nint2 = nfunc(momentum2)
                                    nint = nint1**2 * nint2**2
                                    call ints2e(shell1, ka, shell2, kb, shell1, ka, shell2, kb, intarray)
                                    do u = 1, nint
                                          intarray(u) = abs(intarray(u))
                                    end do
                                    !
                                    ! Minus sign results from the fact that
                                    ! DSORT subroutine sorts in increasing order
                                    !
                                    a(lb, la) = -sqrt(maxval(intarray(1:nint)))
                                    ia(lb, la) = lb
                                    a(la, lb) = a(lb, la)
                                    ia(la, lb) = la
                                    lb = lb + 1
                              end do
                        end do
                  end do
            end do

            do ka = 1, natom
                  do la = sh0(ka), sh0(ka + 1) - 1
                        do kb = 1, natom
                              k0 = sh0(kb)
                              k1 = sh0(kb + 1)
                              !
                              ! DSORT sorts array in increasing order
                              !
                              call dsort(a(k0:, la), ia(k0:, la), k1 - k0)
                        end do
                  end do
            end do

            a = -a
      end subroutine screenint


      subroutine screenrho(rho, rhomax)
            !
            ! Generate the RHOMAX matrix defined as
            ! RHOMAX(MM, NN) = Max{P, Q} ABS(Rho(P, Q)).
            ! The orbital indices P and Q  belong to
            ! the shells M and N. RHOMAX is a symmetric
            ! matrix. Both lower and upper triangles are
            ! generated.
            !
            ! It is assumed that only the lower triangle
            ! blocks of Rho store meaningful data. Rho can
            ! represent either a symmetric or an antisymmetric
            ! matrix.
            !
            ! 1. Ahmadi, G.R., Almlof, J., The Coulomb
            !    operator in a Gaussian product basis,
            !    Chem. Phys. Lett. 246, 364(1995), Eq. 27
            !
            !
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(out) :: rhomax

            real(F64), dimension(max_nfunc**2) :: work
            integer :: m, n
            integer :: p, q
            integer :: m0, m1, n0, n1
            integer :: idx, nfunc

            do n = 1, nshell
                  do m = n, nshell
                        n0 = shpos(n)
                        n1 = shpos(n + 1) - 1
                        m0 = shpos(m)
                        m1 = shpos(m + 1) - 1
                        nfunc = (n1 - n0 + 1) * (m1 - m0 + 1)
                        
                        idx = 1
                        do q = n0, n1
                              do p = m0, m1
                                    work(idx) = abs(rho(p, q))
                                    idx = idx + 1
                              end do
                        end do

                        rhomax(m, n) = maxval(work(1:nfunc))
                        rhomax(n, m) = rhomax(m, n)
                  end do
            end do
      end subroutine screenrho


      subroutine screenrhoeri(rho, rhoerimax, irhoerimax)
            ! ---------------------------------------------------
            ! Compute shell quartet representatives:
            ! |RHO(a,b)| * SQRT(|(ab|ab)|).
            ! The computed elements are sorted (along the first
            ! index) in descending order inside atomic batches.
            !
            ! Rho can represent either a symmetric or
            ! an antisymmetric matrix. It is assumed that
            ! both upper and lower triangles of Rho store
            ! meaningful data.
            !
            ! ---------------------------------------------------
            ! 1. Ahmadi, G.R., Almlof, J., The Coulomb operator
            !    in a Gaussian product basis,
            !    Chem. Phys. Lett. 246, 364(1995), Eq. 28
            ! ---------------------------------------------------
            ! RHO - Input, density matrix 
            !
            ! A   - Output, see description of the subroutine
            !
            ! IA  - Output, contains indices of integrals, see
            !       description of the subroutine
            !
            !
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(out) :: rhoerimax
            integer, dimension(:, :), intent(out)   :: irhoerimax

            real(F64), dimension(max_nfunc**4) :: intarray
            real(F64) :: rhoab
            integer :: shell1, shell2
            integer :: momentum1, momentum2
            integer :: nint1, nint2, nint
            integer :: la, lb
            integer :: k0, k1
            integer :: ka, kb
            integer :: u, v1, v2, v3, v4
            integer :: a0, b0
            integer :: a, b

            do ka = 1, natom
                  do kb = 1, ka
                        do la = sh0(ka), sh0(ka + 1) - 1
                              shell1 = sh(la)
                              momentum1 = shtype(shell1)
                              nint1 = nfunc(momentum1)
                              lb = sh0(kb)
                              do while ((lb .lt. sh0(kb + 1)) .and. (lb .le. la))
                                    shell2 = sh(lb)
                                    momentum2 = shtype(shell2)
                                    nint2 = nfunc(momentum2)
                                    nint = nint1**2 * nint2**2
                                    call ints2e(shell1, ka, shell2, kb, shell1, ka, shell2, kb, intarray)
                                    
                                    a0 = shpos(la)
                                    b0 = shpos(lb)
                                    u = 1
                                    do v1 = 1, nint1
                                          do v2 = 1, nint2
                                                do v3 = 1, nint1
                                                      do v4 = 1, nint2
                                                            a = a0 + v3 - 1
                                                            b = b0 + v4 - 1
                                                            rhoab = rho(b, a)
                                                            intarray(u) = abs(rhoab**2 * intarray(u))
                                                            u = u + 1
                                                      end do
                                                end do
                                          end do
                                    end do

                                    rhoerimax(lb, la) = -sqrt(maxval(intarray(1:nint)))
                                    irhoerimax(lb, la) = lb
                                    rhoerimax(la, lb) = rhoerimax(lb, la)
                                    irhoerimax(la, lb) = la
                                    lb = lb + 1
                              end do
                        end do
                  end do
            end do

            do ka = 1, natom
                  do la = sh0(ka), sh0(ka + 1) - 1
                        do kb = 1, natom
                              k0 = sh0(kb)
                              k1 = sh0(kb + 1)
                              !
                              ! Sorting in increasing order. Note that
                              ! values were multiplied by -1
                              !
                              call dsort(rhoerimax(k0:, la), irhoerimax(k0:, la), k1 - k0)
                        end do
                  end do
            end do

            rhoerimax = -rhoerimax
      end subroutine screenrhoeri
end module screen
