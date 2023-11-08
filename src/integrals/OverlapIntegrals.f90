module OverlapIntegrals
      use arithmetic
      use math_constants
      use spherh

      implicit none

contains
      


      subroutine overlap_ABC(S, Ra, La, CntrA, ExpA, NormA, NprimA, Rb, Lb, CntrB, &
            ExpB, NormB, NprimB, Rc, Lc, CntrC, ExpC, NormC, NprimC, BinomTable)
            !
            ! Compute overlap integrals for a triple of Cartesian Gaussian shells
            ! of contracted atomic orbitals. This subroutine will work for any angular momenta
            ! La and Lb provided that the binomial coefficients C(max(La,Lb),k) are stored
            ! in BinomTable.
            !
            real(F64), dimension(((La+1)*(La+2))/2, ((Lb+1)*(Lb+2))/2, *), intent(out) :: S
            real(F64), dimension(3), intent(in)          :: Ra, Rb, Rc
            integer, intent(in)                          :: La, Lb, Lc
            real(F64), dimension(*), intent(in)          :: CntrA, NormA, ExpA, CntrB, NormB, ExpB
            real(F64), dimension(*), intent(in)          :: CntrC, NormC, ExpC
            integer, intent(in)                          :: NprimA, NprimB, NprimC
            real(F64), dimension(0:, 0:), intent(in)     :: BinomTable
            
            real(F64), dimension(3) :: Rp, Rpa, Rpb, Rpc, Rab, Rac, Rbc
            real(F64), dimension(3, 0:La) :: PA
            real(F64), dimension(3, 0:Lb) :: PB
            real(F64), dimension(3, 0:Lc) :: PC
            real(F64) :: GaussFactor, Rab2, Rac2, Rbc2
            real(F64) :: AlphaA, AlphaB, AlphaC, AlphaP
            real(F64), dimension(3) :: W
            real(F64), dimension(0:(La+Lb+Lc)/2) :: OneDInts
            real(F64), dimension(0:La, 0:Lb, 0:Lc) :: XInts, YInts, ZInts
            integer :: Na, Nb, Nc
            integer :: lx, ly, lz
            integer, dimension(((La+1)*(La+2))/2) :: ax, ay, az
            integer, dimension(((Lb+1)*(Lb+2))/2) :: bx, by, bz
            integer, dimension(((Lc+1)*(Lc+2))/2) :: cx, cy, cz
            integer :: t, u, v
            integer :: k, l, m
            integer :: a, b, c

            Na = ((La + 1) * (La + 2)) / 2
            Nb = ((Lb + 1) * (Lb + 2)) / 2
            Nc = ((Lc + 1) * (Lc + 2)) / 2
            a = 1
            do lx = La, 0, -1
                  do ly = La-lx, 0, -1
                        lz = La - lx - ly
                        ax(a) = lx
                        ay(a) = ly
                        az(a) = lz
                        a = a + 1
                  end do
            end do
            b = 1
            do lx = Lb, 0, -1
                  do ly = Lb-lx, 0, -1
                        lz = Lb - lx - ly
                        bx(b) = lx
                        by(b) = ly
                        bz(b) = lz
                        b = b + 1
                  end do
            end do
            c = 1
            do lx = Lc, 0, -1
                  do ly = Lc-lx, 0, -1
                        lz = Lc - lx - ly
                        cx(c) = lx
                        cy(c) = ly
                        cz(c) = lz
                        c = c + 1
                  end do
            end do
            OneDInts = ZERO
            S(:, :, 1:Nc) = ZERO
            Rab = Ra - Rb
            Rac = Ra - Rc
            Rbc = Rb - Rc
            Rab2 = dot_product(Rab, Rab)
            Rac2 = dot_product(Rac, Rac)
            Rbc2 = dot_product(Rbc, Rbc)
            do m = 1, NprimC
                  do l = 1, NprimB
                        do k = 1, NprimA
                              AlphaA = ExpA(k)
                              AlphaB = ExpB(l)
                              AlphaC = ExpC(m)
                              AlphaP = ExpA(k) + ExpB(l) + ExpC(m)
                              GaussFactor = exp(-(AlphaA*AlphaB*Rab2+AlphaA*AlphaC*Rac2+AlphaB*AlphaC*Rbc2)/AlphaP)
                              Rp = (AlphaA*Ra + AlphaB*Rb + AlphaC*Rc) / AlphaP
                              Rpa = Rp - Ra
                              Rpb = Rp - Rb
                              Rpc = Rp - Rc
                              do a = 0, La
                                    PA(:, a) = Rpa**a
                              end do
                              do b = 0, Lb
                                    PB(:, b) = Rpb**b
                              end do
                              do c = 0, Lc
                                    PC(:, c) = Rpc**c
                              end do
                              do t = 0, (La+Lb+Lc)/2
                                    !
                                    ! Integrate(-Inf,+Inf) x**(2*t) * exp(-AlphaP * x**2) dx
                                    !
                                    OneDInts(t) = dblfact(2*t-1) / (TWO * AlphaP)**t * Sqrt(PI/AlphaP)
                              end do
                              do c = 0, Lc
                                    do b = 0, Lb
                                          do a = 0, La
                                                W = ZERO
                                                do v = 0, c
                                                      do u = 0, b
                                                            do t = modulo(a+b+c-u-v,2), a, 2
                                                                  W = W + BinomTable(a, t) * PA(:, t) &
                                                                        * BinomTable(b, u) * PB(:, u) &
                                                                        * BinomTable(c, v) * PC(:, v) &
                                                                        * OneDInts((a+b+c-t-u-v)/2)
                                                            end do
                                                      end do
                                                end do
                                                XInts(a, b, c) = W(1)
                                                YInts(a, b, c) = W(2)
                                                ZInts(a, b, c) = W(3)
                                          end do
                                    end do
                              end do
                              XInts = GaussFactor * CntrA(k) * CntrB(l) * CntrC(m) * XInts
                              do c = 1, Nc
                                    do b = 1, Nb
                                          do a = 1, Na
                                                S(a, b, c) = S(a, b, c) + &
                                                      NormA(a) * NormB(b) * NormC(c) &
                                                      * XInts(ax(a), bx(b), cx(c)) &
                                                      * YInts(ay(a), by(b), cy(c)) &
                                                      * ZInts(az(a), bz(b), cz(c))
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine overlap_ABC
end module OverlapIntegrals
