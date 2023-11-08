module rpa_core_SpinUnres
      use arithmetic
      use math_constants
      use real_linalg
      use ParallelCholesky
      
      implicit none

contains

      subroutine rpa_Wabcd_SpinUnres(W, RhoOcc, RhoVirt, LaplaceW, Na, Nb, Nc, Nd, &
            LocA, LocB, LocC, LocD, Nab, Ncd, NAO, NLaplace)
            !
            ! ShA /= ShB
            ! ShC /= ShD
            !
            real(F64), dimension(:, :), intent(out)                 :: W
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoVirt
            real(F64), dimension(:), intent(in)                     :: LaplaceW
            integer, intent(in)                                     :: Na, Nb, Nc, Nd
            integer, intent(in)                                     :: LocA, LocB, LocC, LocD
            integer, intent(in)                                     :: Nab, Ncd
            integer, intent(in)                                     :: NAO
            integer, intent(in)                                     :: NLaplace

            integer :: pp, qq, rr, ss
            integer :: p, q, r, s
            integer :: OffsetP, OffsetQ, OffsetR, OffsetS
            integer :: k
            real(F64), dimension(Nab, Ncd) :: Tabcd

            OffsetP = LocA - 1
            OffsetQ = LocB - 1
            OffsetR = LocC - 1
            OffsetS = LocD - 1
            Tabcd = ZERO
            do s = 1, Nd
                  do r = 1, Nc
                        do q = 1, Nb
                              do p = 1, Na
                                    pp = OffsetP + p
                                    qq = OffsetQ + q
                                    rr = OffsetR + r
                                    ss = OffsetS + s
                                    do k = 1, NLaplace
                                          Tabcd(p+(q-1)*Na, r+(s-1)*Nc) = Tabcd(p+(q-1)*Na, r+(s-1)*Nc) + &
                                                LaplaceW(k) * ( &
                                                ! Alpha
                                                RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                                RhoOcc(k, qq, rr, 1) * RhoVirt(k, pp, ss, 1) + &
                                                RhoOcc(k, pp, ss, 1) * RhoVirt(k, qq, rr, 1) + &
                                                RhoOcc(k, qq, ss, 1) * RhoVirt(k, pp, rr, 1) + &
                                                ! Beta
                                                RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                                RhoOcc(k, qq, rr, 2) * RhoVirt(k, pp, ss, 2) + &
                                                RhoOcc(k, pp, ss, 2) * RhoVirt(k, qq, rr, 2) + &
                                                RhoOcc(k, qq, ss, 2) * RhoVirt(k, pp, rr, 2) &
                                                )
                                    end do
                              end do
                        end do
                  end do
            end do
            W = Tabcd
      end subroutine rpa_Wabcd_SpinUnres


      subroutine rpa_Wabcc_SpinUnres(W, RhoOcc, RhoVirt, LaplaceW, Na, Nb, Nc, &
            LocA, LocB, LocC, Nab, Ncd, NAO, NLaplace)
            !
            ! ShA /= ShB
            ! ShC == ShD
            !
            real(F64), dimension(:, :), intent(out)                 :: W
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoVirt
            real(F64), dimension(:), intent(in)                     :: LaplaceW
            integer, intent(in)                                     :: Na, Nb, Nc
            integer, intent(in)                                     :: LocA, LocB, LocC
            integer, intent(in)                                     :: Nab, Ncd
            integer, intent(in)                                     :: NAO
            integer, intent(in)                                     :: NLaplace

            integer :: pp, qq, rr, ss
            integer :: p, q, r, s, rs
            integer :: OffsetP, OffsetQ, OffsetR, OffsetS
            integer :: k
            real(F64), dimension(Nab, Ncd) :: Tabcd
            
            OffsetP = LocA - 1
            OffsetQ = LocB - 1
            OffsetR = LocC - 1
            OffsetS = LocC - 1
            Tabcd = ZERO
            rs = 1
            do s = 1, Nc
                  !
                  ! --- Diagonal ---
                  !
                  r = s
                  do q = 1, Nb
                        do p = 1, Na
                              pp = OffsetP + p
                              qq = OffsetQ + q
                              rr = OffsetR + r
                              ss = OffsetS + s
                              do k = 1, NLaplace
                                    Tabcd(p+(q-1)*Na, rs) = Tabcd(p+(q-1)*Na, rs) + &
                                          LaplaceW(k) * ( &
                                          ! Alpha
                                          RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                          RhoOcc(k, qq, rr, 1) * RhoVirt(k, pp, ss, 1) + &
                                          ! Beta
                                          RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                          RhoOcc(k, qq, rr, 2) * RhoVirt(k, pp, ss, 2) &
                                          )
                              end do
                        end do
                  end do
                  rs = rs + 1
                  !
                  ! --- Off-diagonal ---
                  !
                  do r = s + 1, Nc
                        do q = 1, Nb
                              do p = 1, Na
                                    pp = OffsetP + p
                                    qq = OffsetQ + q
                                    rr = OffsetR + r
                                    ss = OffsetS + s
                                    do k = 1, NLaplace
                                          Tabcd(p+(q-1)*Na, rs) = Tabcd(p+(q-1)*Na, rs) + &
                                                LaplaceW(k) * ( &
                                                ! Alpha
                                                RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                                RhoOcc(k, qq, rr, 1) * RhoVirt(k, pp, ss, 1) + &
                                                RhoOcc(k, pp, ss, 1) * RhoVirt(k, qq, rr, 1) + &
                                                RhoOcc(k, qq, ss, 1) * RhoVirt(k, pp, rr, 1) + &
                                                ! Beta
                                                RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                                RhoOcc(k, qq, rr, 2) * RhoVirt(k, pp, ss, 2) + &
                                                RhoOcc(k, pp, ss, 2) * RhoVirt(k, qq, rr, 2) + &
                                                RhoOcc(k, qq, ss, 2) * RhoVirt(k, pp, rr, 2) &
                                                )
                                    end do
                              end do
                        end do
                        rs = rs + 1
                  end do
            end do
            W = Tabcd
      end subroutine rpa_Wabcc_SpinUnres
      
      
      subroutine rpa_Waacd_SpinUnres(W, RhoOcc, RhoVirt, LaplaceW, Na, Nc, Nd, &
            LocA, LocC, LocD, Nab, Ncd, NAO, NLaplace)
            !
            ! ShA == ShB
            ! ShC /= ShD
            !
            real(F64), dimension(:, :), intent(out)                 :: W
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoVirt
            real(F64), dimension(:), intent(in)                     :: LaplaceW
            integer, intent(in)                                     :: Na, Nc, Nd
            integer, intent(in)                                     :: LocA, LocC, LocD
            integer, intent(in)                                     :: Nab, Ncd
            integer, intent(in)                                     :: NAO
            integer, intent(in)                                     :: NLaplace

            integer :: pp, qq, rr, ss
            integer :: p, q, r, s, pq
            integer :: OffsetP, OffsetQ, OffsetR, OffsetS
            integer :: k
            real(F64), dimension(Nab, Ncd) :: Tabcd

            OffsetP = LocA - 1
            OffsetQ = LocA - 1
            OffsetR = LocC - 1
            OffsetS = LocD - 1
            Tabcd = ZERO
            do s = 1, Nd
                  do r = 1, Nc
                        pq = 1
                        do q = 1, Na
                              !
                              ! Diagonal
                              !
                              p = q
                              pp = OffsetP + p
                              qq = OffsetQ + q
                              rr = OffsetR + r
                              ss = OffsetS + s
                              do k = 1, NLaplace
                                    Tabcd(pq, r+(s-1)*Nc) = Tabcd(pq, r+(s-1)*Nc) + &
                                          LaplaceW(k) * ( &
                                          ! Alpha
                                          RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                          RhoOcc(k, pp, ss, 1) * RhoVirt(k, qq, rr, 1) + &
                                          ! Beta
                                          RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                          RhoOcc(k, pp, ss, 2) * RhoVirt(k, qq, rr, 2) &
                                          )
                              end do
                              pq = pq + 1
                              !
                              ! Off-diagonal
                              !
                              do p = q + 1, Na
                                    pp = OffsetP + p
                                    qq = OffsetQ + q
                                    rr = OffsetR + r
                                    ss = OffsetS + s
                                    do k = 1, NLaplace
                                          Tabcd(pq, r+(s-1)*Nc) = Tabcd(pq, r+(s-1)*Nc) + &
                                                LaplaceW(k) * ( &
                                                ! Alpha
                                                RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                                RhoOcc(k, qq, rr, 1) * RhoVirt(k, pp, ss, 1) + &
                                                RhoOcc(k, pp, ss, 1) * RhoVirt(k, qq, rr, 1) + &
                                                RhoOcc(k, qq, ss, 1) * RhoVirt(k, pp, rr, 1) + &
                                                ! Beta
                                                RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                                RhoOcc(k, qq, rr, 2) * RhoVirt(k, pp, ss, 2) + &
                                                RhoOcc(k, pp, ss, 2) * RhoVirt(k, qq, rr, 2) + &
                                                RhoOcc(k, qq, ss, 2) * RhoVirt(k, pp, rr, 2) &
                                                )
                                    end do
                                    pq = pq + 1
                              end do
                        end do
                  end do
            end do
            W = Tabcd
      end subroutine rpa_Waacd_SpinUnres
      

      subroutine rpa_Waacc_SpinUnres(W, RhoOcc, RhoVirt, LaplaceW, Na, Nc, &
            LocA, LocC, Nab, Ncd, NAO, NLaplace)
            !
            ! ShA == ShB
            ! ShC == ShD
            !
            real(F64), dimension(:, :), intent(out)                 :: W
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in) :: RhoVirt
            real(F64), dimension(:), intent(in)                     :: LaplaceW
            integer, intent(in)                                     :: Na, Nc
            integer, intent(in)                                     :: LocA, LocC
            integer, intent(in)                                     :: Nab, Ncd
            integer, intent(in)                                     :: NAO
            integer, intent(in)                                     :: NLaplace

            integer :: pp, qq, rr, ss
            integer :: p, q, r, s, pq, rs
            integer :: OffsetP, OffsetQ, OffsetR, OffsetS
            integer :: k
            real(F64), dimension(Nab, Ncd) :: Tabcd

            OffsetP = LocA - 1
            OffsetQ = LocA - 1
            OffsetR = LocC - 1
            OffsetS = LocC - 1
            Tabcd = ZERO
            rs = 1
            do s = 1, Nc
                  !
                  ! Diagonal
                  !
                  r = s
                  pq = 1
                  do q = 1, Na
                        !
                        ! Diagonal
                        !
                        p = q
                        pp = OffsetP + p
                        qq = OffsetQ + q
                        rr = OffsetR + r
                        ss = OffsetS + s
                        do k = 1, NLaplace
                              Tabcd(pq, rs) = Tabcd(pq, rs) + LaplaceW(k) * ( &
                                    ! Alpha
                                    RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                    ! Beta
                                    RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) &
                                    )
                        end do
                        pq = pq + 1
                        !
                        ! Off-diagonal
                        !
                        do p = q + 1, Na
                              pp = OffsetP + p
                              qq = OffsetQ + q
                              rr = OffsetR + r
                              ss = OffsetS + s
                              do k = 1, NLaplace
                                    Tabcd(pq, rs) = Tabcd(pq, rs) + &
                                          LaplaceW(k) * ( &
                                          ! Alpha
                                          RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                          RhoOcc(k, qq, rr, 1) * RhoVirt(k, pp, ss, 1) + &
                                          ! Beta
                                          RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                          RhoOcc(k, qq, rr, 2) * RhoVirt(k, pp, ss, 2) &
                                          )
                              end do
                              pq = pq + 1
                        end do
                  end do
                  rs = rs + 1
                  !
                  ! Off-diagonal
                  !
                  do r = s+1, Nc
                        pq = 1
                        do q = 1, Na
                              !
                              ! Diagonal
                              !
                              p = q
                              pp = OffsetP + p
                              qq = OffsetQ + q
                              rr = OffsetR + r
                              ss = OffsetS + s
                              do k = 1, NLaplace
                                    Tabcd(pq, rs) = Tabcd(pq, rs) + &
                                          LaplaceW(k) * ( &
                                          ! Alpha
                                          RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                          RhoOcc(k, pp, ss, 1) * RhoVirt(k, qq, rr, 1) + &
                                          ! Beta
                                          RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                          RhoOcc(k, pp, ss, 2) * RhoVirt(k, qq, rr, 2) &
                                          )
                              end do
                              pq = pq + 1
                              !
                              ! Off-diagonal
                              !
                              do p = q + 1, Na
                                    pp = OffsetP + p
                                    qq = OffsetQ + q
                                    rr = OffsetR + r
                                    ss = OffsetS + s
                                    do k = 1, NLaplace
                                          Tabcd(pq, rs) = Tabcd(pq, rs) + &
                                                LaplaceW(k) * ( &
                                                ! Alpha
                                                RhoOcc(k, pp, rr, 1) * RhoVirt(k, qq, ss, 1) + &
                                                RhoOcc(k, qq, rr, 1) * RhoVirt(k, pp, ss, 1) + &
                                                RhoOcc(k, pp, ss, 1) * RhoVirt(k, qq, rr, 1) + &
                                                RhoOcc(k, qq, ss, 1) * RhoVirt(k, pp, rr, 1) + &
                                                ! Beta
                                                RhoOcc(k, pp, rr, 2) * RhoVirt(k, qq, ss, 2) + &
                                                RhoOcc(k, qq, rr, 2) * RhoVirt(k, pp, ss, 2) + &
                                                RhoOcc(k, pp, ss, 2) * RhoVirt(k, qq, rr, 2) + &
                                                RhoOcc(k, qq, ss, 2) * RhoVirt(k, pp, rr, 2) &
                                                )
                                    end do
                                    pq = pq + 1
                              end do
                        end do
                        rs = rs + 1
                  end do
            end do
            W = Tabcd
      end subroutine rpa_Waacc_SpinUnres


      subroutine rpa_W_SpinUnres(W, NBra, NKet, RhoOcc, RhoVirt, ShellPairs, ShellPairLoc, &
            ShellPairDim, ShellLoc, ShellParamsIdx, NAngFunc, LaplaceW, NAO, &
            NLaplace, BraBounds, KetBounds, IgnoreParity)

            real(F64), dimension(NBra, NKet), intent(out)               :: W
            integer, intent(in)                                         :: NBra, NKet
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)     :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)     :: RhoVirt
            integer, dimension(:, :), intent(in)                        :: ShellPairs
            integer, dimension(:, :), intent(in)                        :: ShellPairLoc
            integer, dimension(:), intent(in)                           :: ShellPairDim
            integer, dimension(:), intent(in)                           :: ShellLoc
            integer, dimension(:), intent(in)                           :: ShellParamsIdx
            integer, dimension(:), intent(in)                           :: NAngFunc
            real(F64), dimension(:), intent(in)                         :: LaplaceW
            integer, intent(in)                                         :: NAO
            integer, intent(in)                                         :: NLaplace
            integer, dimension(2), intent(in)                           :: BraBounds
            integer, dimension(2), intent(in)                           :: KetBounds
            logical, intent(in)                                         :: IgnoreParity

            integer :: ShA, Na, ShellParamsA, LocA
            integer :: ShB, Nb, ShellParamsB, LocB
            integer :: ShC, Nc, ShellParamsC, LocC
            integer :: ShD, Nd, ShellParamsD, LocD
            integer :: Nab, LocAB, Ncd, LocCD
            integer :: pq0, pq1, rs0, rs1
            integer :: ShCD, ShAB
            integer :: mod_ab, mod_cd
            real(F64), dimension(NLaplace) :: EffLaplaceW

            if (IgnoreParity) then
                  !$omp parallel do collapse(2) schedule(guided) &
                  !$omp default(shared) &
                  !$omp private(ShA, Na, ShellParamsA, LocA) &
                  !$omp private(ShB, Nb, ShellParamsB, LocB) &
                  !$omp private(ShC, Nc, ShellParamsC, LocC) &
                  !$omp private(ShD, Nd, ShellParamsD, LocD) &
                  !$omp private(Nab, LocAB, Ncd, LocCD) &
                  !$omp private(pq0, pq1, rs0, rs1) &
                  !$omp private(ShCD, ShAB)
                  do ShCD = KetBounds(1), KetBounds(2)
                        do ShAB = BraBounds(1), BraBounds(2)
                              ShC = ShellPairs(1, ShCD)                  
                              ShD = ShellPairs(2, ShCD)
                              LocCD = ShellPairLoc(SUBSET_STORAGE, ShCD)
                              Ncd = ShellPairDim(ShCD)
                              rs0 = LocCD
                              rs1 = LocCD + Ncd - 1

                              ShellParamsD = ShellParamsIdx(ShD)
                              Nd = NAngFunc(ShellParamsD)
                              LocD = ShellLoc(ShD)

                              ShellParamsC = ShellParamsIdx(ShC)
                              Nc = NAngFunc(ShellParamsC)
                              LocC = ShellLoc(ShC)                        

                              LocAB = ShellPairLoc(SUBSET_STORAGE, ShAB)
                              Nab = ShellPairDim(ShAB)
                              pq0 = LocAB
                              pq1 = LocAB + Nab - 1

                              ShA = ShellPairs(1, ShAB)
                              ShellParamsA = ShellParamsIdx(ShA)
                              Na = NAngFunc(ShellParamsA)
                              LocA = ShellLoc(ShA)

                              ShB = ShellPairs(2, ShAB)
                              ShellParamsB = ShellParamsIdx(ShB)
                              Nb = NAngFunc(ShellParamsB)
                              LocB = ShellLoc(ShB)

                              if (ShC /= ShD) then
                                    if (ShA /= ShB) then
                                          call rpa_Wabcd_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                LaplaceW, Na, Nb, Nc, Nd, LocA, LocB, LocC, LocD, Nab, Ncd, &
                                                NAO, NLaplace)
                                    else
                                          call rpa_Waacd_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                LaplaceW, Na, Nc, Nd, LocA, LocC, LocD, Nab, Ncd, &
                                                NAO, NLaplace)
                                    end if
                              else
                                    if (ShA /= ShB) then
                                          call rpa_Wabcc_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                LaplaceW, Na, Nb, Nc, LocA, LocB, LocC, Nab, Ncd, &
                                                NAO, NLaplace)
                                    else
                                          call rpa_Waacc_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                LaplaceW, Na, Nc, LocA, LocC, Nab, Ncd, NAO, NLaplace)
                                    end if
                              end if
                        end do
                  end do
                  !$omp end parallel do
            else
                  W = ZERO
                  !$omp parallel do schedule(guided) &
                  !$omp default(shared) &
                  !$omp private(ShA, Na, ShellParamsA, LocA) &
                  !$omp private(ShB, Nb, ShellParamsB, LocB) &
                  !$omp private(ShC, Nc, ShellParamsC, LocC) &
                  !$omp private(ShD, Nd, ShellParamsD, LocD) &
                  !$omp private(Nab, LocAB, Ncd, LocCD) &
                  !$omp private(pq0, pq1, rs0, rs1) &
                  !$omp private(ShCD, ShAB, EffLaplaceW) &
                  !$omp private(mod_ab, mod_cd)
                  do ShCD = KetBounds(1), KetBounds(2)
                        do ShAB = BraBounds(1), BraBounds(2)
                              mod_ab = modulo(ShAB, 2)
                              mod_cd = modulo(ShCD, 2)
                              if ((mod_ab == mod_cd .and. ShAB <= ShCD) .or. (mod_ab /= mod_cd .and. ShAB > ShCD)) then
                                    ShC = ShellPairs(1, ShCD)                  
                                    ShD = ShellPairs(2, ShCD)
                                    LocCD = ShellPairLoc(SUBSET_STORAGE, ShCD)
                                    Ncd = ShellPairDim(ShCD)
                                    rs0 = LocCD
                                    rs1 = LocCD + Ncd - 1

                                    ShellParamsD = ShellParamsIdx(ShD)
                                    Nd = NAngFunc(ShellParamsD)
                                    LocD = ShellLoc(ShD)

                                    ShellParamsC = ShellParamsIdx(ShC)
                                    Nc = NAngFunc(ShellParamsC)
                                    LocC = ShellLoc(ShC)                        

                                    LocAB = ShellPairLoc(SUBSET_STORAGE, ShAB)
                                    Nab = ShellPairDim(ShAB)
                                    pq0 = LocAB
                                    pq1 = LocAB + Nab - 1

                                    ShA = ShellPairs(1, ShAB)
                                    ShellParamsA = ShellParamsIdx(ShA)
                                    Na = NAngFunc(ShellParamsA)
                                    LocA = ShellLoc(ShA)

                                    ShB = ShellPairs(2, ShAB)
                                    ShellParamsB = ShellParamsIdx(ShB)
                                    Nb = NAngFunc(ShellParamsB)
                                    LocB = ShellLoc(ShB)
                                    !
                                    ! Rescale the diagonal shell blocks of W(pq,rs) by 1/2.
                                    ! To account for the symmetry of W, after the computaton of GRWRG,
                                    ! the nonsymmetric GRWRG is symmetrized: GRWRG <- GRWRG + GRWRG**T.
                                    !
                                    if (ShAB /= ShCD) then
                                          EffLaplaceW = LaplaceW(1:NLaplace)
                                    else
                                          EffLaplaceW = ONE/TWO * LaplaceW(1:NLaplace)
                                    end if

                                    if (ShC /= ShD) then
                                          if (ShA /= ShB) then
                                                call rpa_Wabcd_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                      EffLaplaceW, Na, Nb, Nc, Nd, LocA, LocB, LocC, LocD, Nab, Ncd, &
                                                      NAO, NLaplace)
                                          else
                                                call rpa_Waacd_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                      EffLaplaceW, Na, Nc, Nd, LocA, LocC, LocD, Nab, Ncd, &
                                                      NAO, NLaplace)
                                          end if
                                    else
                                          if (ShA /= ShB) then
                                                call rpa_Wabcc_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                      EffLaplaceW, Na, Nb, Nc, LocA, LocB, LocC, Nab, Ncd, &
                                                      NAO, NLaplace)
                                          else
                                                call rpa_Waacc_SpinUnres(W(pq0:pq1, rs0:rs1), RhoOcc, RhoVirt, &
                                                      EffLaplaceW, Na, Nc, LocA, LocC, Nab, Ncd, NAO, NLaplace)
                                          end if
                                    end if
                              end if
                        end do
                  end do
                  !$omp end parallel do
            end if
      end subroutine rpa_W_SpinUnres
      

      subroutine rpa_WRG_SpinUnres(WRG, RG, ldRG, W, NBra, NKet, RhoOcc, RhoVirt, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, NAngFunc, LaplaceW, &
            NAO, NLaplace, NVecs, BraBounds, KetBounds, IgnoreParity, &
            time_W, time_WRG)
            !
            ! WRG(:, cd) <- WRG(:, cd) + Sum(ab) RG(:, ab) * W(ab, cd)
            !
            real(F64), dimension(NVecs, *), intent(inout)               :: WRG
            real(F64), dimension(ldRG, *), intent(in)                   :: RG
            integer, intent(in)                                         :: ldRG
            real(F64), dimension(NBra, NKet), intent(out)               :: W
            integer, intent(in)                                         :: NBra
            integer, intent(in)                                         :: NKet
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)     :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)     :: RhoVirt
            integer, dimension(:, :), intent(in)                        :: ShellPairs
            integer, dimension(:, :), intent(in)                        :: ShellPairLoc
            integer, dimension(:), intent(in)                           :: ShellPairDim
            integer, dimension(:), intent(in)                           :: ShellLoc
            integer, dimension(:), intent(in)                           :: ShellParamsIdx
            integer, dimension(:), intent(in)                           :: NAngFunc
            real(F64), dimension(:)                                     :: LaplaceW
            integer, intent(in)                                         :: NAO
            integer, intent(in)                                         :: NLaplace
            integer, intent(in)                                         :: NVecs
            integer, dimension(2), intent(in)                           :: BraBounds
            integer, dimension(2), intent(in)                           :: KetBounds
            logical, intent(in)                                         :: IgnoreParity
            real(F64), intent(inout)                                    :: time_W
            real(F64), intent(inout)                                    :: time_WRG

            type(tclock) :: timer

            call clock_start(timer)
            call rpa_W_SpinUnres(W, NBra, NKet, RhoOcc, RhoVirt, ShellPairs, ShellPairLoc, &
                  ShellPairDim, ShellLoc, ShellParamsIdx, NAngFunc, LaplaceW, NAO, &
                  NLaplace, BraBounds, KetBounds, IgnoreParity)
            time_W = time_W + clock_readwall(timer)
            call clock_start(timer)
            call real_ab_x(WRG, NVecs, RG, ldRG, W, NBra, NVecs, NKet, NBra, alpha=ONE, beta=ONE)
            time_WRG = time_WRG + clock_readwall(timer)
      end subroutine rpa_WRG_SpinUnres


      subroutine rpa_GPiUG_BraLoop_SpinUnres(GPiUG, WRG, W, RGBra, RGKet, RhoOcc, RhoVirt, ShellPairs, ShellPairLoc, &
            ShellPairDim, ShellLoc, LaplaceW, ShellParamsIdx, NAngFunc, NAO, NRandom, &
            NLaplace, By, Kx, Ky, SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
            
            real(F64), dimension(:, :), intent(inout)                 :: GPiUG
            real(F64), dimension(:, :), intent(out)                   :: WRG
            real(F64), dimension(:), intent(out)                      :: W
            real(F64), dimension(:, :, :), intent(in)                 :: RGBra
            real(F64), dimension(:, :), intent(in)                    :: RGKet
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)   :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)   :: RhoVirt
            integer, dimension(:, :), intent(in)                      :: ShellPairs
            integer, dimension(:, :), intent(in)                      :: ShellPairLoc
            integer, dimension(:), intent(in)                         :: ShellPairDim
            integer, dimension(:), intent(in)                         :: ShellLoc
            real(F64), dimension(:), intent(in)                       :: LaplaceW
            integer, dimension(:), intent(in)                         :: ShellParamsIdx
            integer, dimension(:), intent(in)                         :: NAngFunc
            integer, intent(in)                                       :: NAO
            integer, intent(in)                                       :: NRandom
            integer, intent(in)                                       :: NLaplace
            integer, intent(in)                                       :: By
            integer, intent(in)                                       :: Kx, Ky
            integer, dimension(:, :), intent(in)                      :: SubsetBounds
            integer, dimension(:), intent(in)                         :: SubsetDim
            integer, dimension(2), intent(in)                         :: NSubsets
            real(F64), intent(inout)                                  :: time_W
            real(F64), intent(inout)                                  :: time_WRG
            real(F64), intent(inout)                                  :: time_GRWRG

            integer :: Bra, Ket, Bx
            integer :: NBra, NKet
            integer :: mod_Bra, mod_Ket
            integer :: ldRGBra, ldRGKet
            
            type(tclock) :: timer

            ldRGBra = size(RGBra, dim=1)
            ldRGKet = size(RGKet, dim=1)
            Ket = Kx + (Ky - 1) * NSubsets(1)
            NKet = SubsetDim(Ket)
            WRG = ZERO
            do Bx = 1, NSubsets(1)
                  Bra = Bx + (By - 1) * NSubsets(1)
                  NBra = SubsetDim(Bra)
                  mod_Bra = modulo(Bra, 2)
                  mod_Ket = modulo(Ket, 2)
                  if (NBra > 0 .and. NKet > 0 .and. &
                        ((mod_Bra == mod_Ket .and. Bra <= Ket) .or. &
                        (mod_Bra /= mod_Ket .and. Bra > Ket))) then
                        !
                        ! WRG(:, cd) <- Sum(ab) RG(:, ab) * W(ab, cd)
                        ! ab = bra AO orbital pair indices
                        ! cd = ket AO orbital pair indices
                        !
                        call rpa_WRG_SpinUnres(WRG, RGBra(:, :, Bx), ldRGBra, W(1:NBra*NKet), NBra, NKet, RhoOcc, RhoVirt, &
                              ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, NAngFunc, LaplaceW, &
                              NAO, NLaplace, NRandom, SubsetBounds(:, Bra), &
                              SubsetBounds(:, Ket), (Bra/=Ket), time_W, time_WRG)
                  end if
            end do
            !
            ! Dielectric matrix reduced to the effective dimensions
            !
            call clock_start(timer)
            call real_abT_x(GPiUG, NRandom, RGKet, ldRGKet, WRG, NRandom, &
                  NRandom, NRandom, NKet, alpha=ONE, beta=ONE)
            time_GRWRG = time_GRWRG + clock_readwall(timer)
      end subroutine rpa_GPiUG_BraLoop_SpinUnres


      subroutine rpa_GPiUG_SpinUnres(GPiUG, WRG, W, RGBra, RGKet, RhoOcc, RhoVirt, ShellPairs, ShellPairLoc, &
            ShellPairDim, ShellLoc, LaplaceW, ShellParamsIdx, NAngFunc, NAO, NRandom, &
            NLaplace, By, Ky, Transpose, SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
            
            real(F64), dimension(:, :), intent(inout)                 :: GPiUG
            real(F64), dimension(:, :), intent(out)                   :: WRG
            real(F64), dimension(:), intent(out)                      :: W
            real(F64), dimension(:, :, :), intent(in)                 :: RGBra
            real(F64), dimension(:, :, :), intent(in)                 :: RGKet
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)   :: RhoOcc
            real(F64), dimension(NLaplace, NAO, NAO, 2), intent(in)   :: RhoVirt
            integer, dimension(:, :), intent(in)                      :: ShellPairs
            integer, dimension(:, :), intent(in)                      :: ShellPairLoc
            integer, dimension(:), intent(in)                         :: ShellPairDim
            integer, dimension(:), intent(in)                         :: ShellLoc
            real(F64), dimension(:), intent(in)                       :: LaplaceW
            integer, dimension(:), intent(in)                         :: ShellParamsIdx
            integer, dimension(:), intent(in)                         :: NAngFunc
            integer, intent(in)                                       :: NAO
            integer, intent(in)                                       :: NRandom
            integer, intent(in)                                       :: NLaplace
            integer, intent(in)                                       :: By
            integer, intent(in)                                       :: Ky
            logical, intent(in)                                       :: Transpose
            integer, dimension(:, :), intent(in)                      :: SubsetBounds
            integer, dimension(:), intent(in)                         :: SubsetDim
            integer, dimension(2), intent(in)                         :: NSubsets
            real(F64), intent(inout)                                  :: time_W
            real(F64), intent(inout)                                  :: time_WRG
            real(F64), intent(inout)                                  :: time_GRWRG

            integer :: Bx, Kx
            
            do Kx = 1, NSubsets(1)
                  call rpa_GPiUG_BraLoop_SpinUnres(GPiUG, WRG, W, RGBra, &
                        RGKet(:, :, Kx), RhoOcc, RhoVirt, ShellPairs, &
                        ShellPairLoc, ShellPairDim, ShellLoc, &
                        LaplaceW, ShellParamsIdx, NAngFunc, NAO, &
                        NRandom, NLaplace, By, Kx, Ky, SubsetBounds, &
                        SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
            end do
            if (Transpose) then
                  do Bx = 1, NSubsets(1)
                        call rpa_GPiUG_BraLoop_SpinUnres(GPiUG, WRG, W, RGKet, &
                              RGBra(:, :, Bx), RhoOcc, RhoVirt, ShellPairs, &
                              ShellPairLoc, ShellPairDim, ShellLoc, &
                              LaplaceW, ShellParamsIdx, NAngFunc, NAO, &
                              NRandom, NLaplace, Ky, Bx, By, SubsetBounds, &
                              SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                  end do
            end if
      end subroutine rpa_GPiUG_SpinUnres
end module rpa_core_SpinUnres
