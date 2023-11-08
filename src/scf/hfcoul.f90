module hfcoul
      use math_constants
      use gparam
      use ints
      use jengine

      implicit none

      double precision, private   :: threshold

contains

      subroutine hfcoul_setthresh(t, jscal)
            double precision, intent(in) :: t
            double precision, intent(in) :: jscal

            threshold = t / abs(jscal)
      end subroutine hfcoul_setthresh


      subroutine dbg_coulomb(jmatrix, rho)
            double precision, dimension(:, :), intent(out) :: jmatrix
            double precision, dimension(:, :), intent(in)  :: rho

            integer :: aa, bb
            integer :: ka, kb

            jmatrix = zero

            do ka = 1, natom
                  do aa = sh0(ka), sh0(ka + 1) - 1
                        do kb = 1, natom
                              do bb = sh0(kb), sh0(kb + 1) - 1
                                    call dbg_jint(aa, ka, bb, kb, rho, jmatrix)
                              end do
                        end do
                  end do
            end do
      end subroutine dbg_coulomb


      subroutine dbg_jint(aa, ka, bb, kb, rho, jmatrix)
            integer, intent(in) :: aa, bb, ka, kb
            double precision, dimension(:, :), intent(in) :: rho
            double precision, dimension(:, :), intent(inout) :: jmatrix

            double precision, dimension(max_nfunc**4) :: gabcd
            integer :: kc, kd
            integer :: cc, dd, c, d, a, b
            integer :: nfunca, nfuncb, nfuncc, nfuncd
            integer :: idxa, idxb, idxc, idxd
            integer :: a0, b0, c0, d0
            integer :: u

            a = sh(aa)
            b = sh(bb)
            nfunca = nfunc(shtype(a))
            nfuncb = nfunc(shtype(b))

            do kc = 1, natom
                  do cc = sh0(kc), sh0(kc + 1) - 1
                        c = sh(cc)
                        nfuncc = nfunc(shtype(c))
                        do kd = 1, natom
                              do dd = sh0(kd), sh0(kd + 1) - 1
                                    d = sh(dd)
                                    nfuncd = nfunc(shtype(d))

                                    call ints2e(a, ka, b, kb, c, kc, d, kd, gabcd)

                                    a0 = shpos(aa)
                                    b0 = shpos(bb)
                                    c0 = shpos(cc)
                                    d0 = shpos(dd)

                                    u = 1
                                    do idxa = a0, a0 + nfunca - 1
                                          do idxb = b0, b0 + nfuncb - 1
                                                do idxc = c0, c0 + nfuncc - 1
                                                      do idxd = d0, d0 + nfuncd - 1
                                                            jmatrix(idxa, idxb) = jmatrix(idxa, idxb) + &
                                                                  rho(idxc, idxd) * gabcd(u)
                                                            u = u + 1
                                                      end do
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine dbg_jint


      subroutine coulgetrho(rho, rhoab, a, b)
            double precision, dimension(:, :), intent(in) :: rho
            double precision, dimension(:), intent(out)   :: rhoab
            integer, intent(in)                           :: a, b
            
            integer :: v
            integer :: r, s, r0, r1, s0, s1

            r0 = shpos(a)
            r1 = shpos(a + 1) - 1
            s0 = shpos(b)
            s1 = shpos(b + 1) - 1

            v = 1
            do s = s0, s1
                  do r = r0, r1
                        rhoab(v) = rho(r, s)
                        v = v + 1
                  end do
            end do
      end subroutine coulgetrho


      pure subroutine coulscreen(rhoerimax, irhoerimax, erimax, ierimax, &
            k1, k2, k3, k4, m1, m2, m3, m4, n1, n2, n3, n4, &
            quartflag, flag)
            ! -------------------------------------------------------------------
            ! Compute shell quartets that give nonnegligible Coulomb matrix
            ! contribution. Schwarz inequality is used for screening.
            ! -------------------------------------------------------------------
            ! RHOERIMAX  - Matrix of |RHO(C, D)| * SQRT((CD|CD)) elements, see
            ! IRHOERIMAX   SCREEN module documentation
            !
            ! ERIMAX     - Matrix of SQRT((AB|AB)) elements, see SCREEN module
            ! IERIMAX      documentation
            !
            ! K1, K2,    - Quartet of atom indices
            ! K3, K4
            ! 
            ! M1, M2,    - Quantities required to compute shell quartet index
            ! M3, M4,      (N x N x N x N -> N map which is invariant with respect
            ! N1, N2,      to permutation group of real-valued two-electron
            ! N3, N4       integral). See text of this subroutine for the actual
            !              representation of the index formula
            ! 
            ! QUARTFLAG  - List of FLAGS that control each quartet specific 
            !              contribution to the Fock matrix
            ! FLAG       - Flag that controls the shell quartet's contribution to
            !              the Fock matrix
            !
            double precision, dimension(:, :), intent(in) :: rhoerimax
            integer, dimension(:, :), intent(in)          :: irhoerimax
            double precision, dimension(:, :), intent(in) :: erimax
            integer, dimension(:, :), intent(in)          :: ierimax
            integer, intent(in)                           :: k1, k2, k3, k4
            integer, intent(in)                           :: m1, m2, m3, m4
            integer, intent(in)                           :: n1, n2, n3, n4
            integer, dimension(:), intent(inout)          :: quartflag
            integer, intent(in)                           :: flag

            double precision :: estim, estim_cd, estim_ab
            integer :: i, j
            integer :: aa, a0, a1
            integer :: bb, b0, b1
            integer :: cc, c0, c1
            integer :: dd, d0, d1
            logical :: contrib
            integer :: idx
            !
            ! Contribution to Coulomb matrix:
            ! D(c, d) * (ab|cd),
            ! where a, b, c, d are shell indices, and
            ! a \in k1,
            ! b \in k2,
            ! c \in k3, 
            ! d \in k4,
            ! where k1, k2, k3, k4 are atom indices
            !
            a0 = sh0(k1)
            a1 = sh0(k1 + 1) - 1
            b0 = sh0(k2)
            b1 = sh0(k2 + 1) - 1
            c0 = sh0(k3)
            c1 = sh0(k3 + 1) - 1
            d0 = sh0(k4)
            d1 = sh0(k4 + 1) - 1
            dloop: do dd = d0, d1
                  cloop: do i = c0, c1
                        !
                        ! RHOERIMAX array contains values of
                        ! |RHO(c, d)| SQRT((cd|cd)) sorted in
                        ! descending order for fixed D index.
                        ! IRHOERIMAX contains corresponding
                        ! sequence of C shells' indices.
                        !
                        estim_cd = rhoerimax(i, dd)
                        cc = irhoerimax(i, dd)
                        contrib = .false.
                        bloop: do bb = b0, b1
                              estim_ab = erimax(a0, bb)
                              !
                              ! ESTIM <- |RHO(C, D)| SQRT((AB|AB)) SQRT(CD|CD)
                              ! >=|RHO(C,D) (AB|CD)|
                              !
                              estim = estim_ab * estim_cd
                              !
                              ! Jump if largest value of SQRT((ab|ab))
                              ! is not enough to satisfy Schwarz
                              ! inequality
                              !
                              if (estim .lt. threshold) cycle bloop
                              
                              aloop: do j = a0, a1
                                    !
                                    ! ERIMAX array contains values of
                                    ! SQRT((ab|ab)) sorted in decreasing 
                                    ! order (shell index b fixed).
                                    !
                                    aa = ierimax(j, bb)
                                    !
                                    ! Only lower half of the Coulomb matrix
                                    ! is referenced
                                    !
                                    if (bb .gt. aa) cycle aloop
                                    estim_ab = erimax(j, bb)
                                    estim = estim_ab * estim_cd
                                    
                                    if (estim .gt. threshold) then
                                          !
                                          ! For a given (ab|cd) shell quartet,
                                          ! IDX is invariant of permutation if 
                                          ! correct M1, M2, M3, M4, N1, N2, N3,
                                          ! N4 values were provided. Set bit
                                          ! denoting permutation label given by
                                          ! FLAG value.
                                          !
                                          idx = (aa - m1) * n1 + (bb - m2) * n2 + &
                                                (cc - m3) * n3 + (dd - m4) * n4 + 1
                                          quartflag(idx) = ior(quartflag(idx), flag)
                                          contrib = .true.
                                    else
                                          exit aloop
                                    end if
                              end do aloop
                        end do bloop
                        
                        if (.not. contrib) exit cloop
                  end do cloop
            end do dloop
      end subroutine coulscreen
end module hfcoul
