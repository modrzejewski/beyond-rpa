module rpa_CC_Exchange
      use arithmetic
      use math_constants
      use rpa_definitions
      use real_linalg

      implicit none

contains

      subroutine rpa_CC_Exchange_Summary(ExchangeApprox)
            integer, intent(in) :: ExchangeApprox

            call msg("Exchange contribution")
            select case (ExchangeApprox)
            case (RPA_EXCHANGE_NONE)
                  call msg(lfield("", 30) // "none")
            case (RPA_EXCHANGE_CUMULANT_LINEAR)
                  call msg(lfield("", 30) // "EcExchange = Int(Lambda) -2*Sum(aibj) (aj|bi)*S(ai,bj;Lambda)")
            case (RPA_EXCHANGE_SOSEX)
                  call msg(lfield("", 30) // "EcExchange = -Sum(aibj) (aj|bi)*T(ai,bj;Lambda=1)")
            case (RPA_EXCHANGE_MBPT3_1)
                  call msg(lfield("", 30) // "EcExchange = -d/dLambda Sum(aibj) (aj|bi)*T(ai,bj;Lambda)|Lambda=1")
                  call msg(lfield("", 30) // "using analytic differentiation of T(Lambda)")
            case (RPA_EXCHANGE_MBPT3_1_NUMERICAL)
                  call msg(lfield("", 30) // "EcExchange = -d/dLambda Sum(aibj) (aj|bi)*T(ai,bj;Lambda)|Lambda=1")
                  call msg(lfield("", 30) // "using the Lyapunov equation to solve for dT/dLambda")
            case (RPA_EXCHANGE_MBPT3_2)
                  call msg(lfield("", 30) // "EcExchange = -d/dLambda Sum(aibj) (aj|bi)*T(ai,bj;Lambda)|Lambda=1")
                  call msg(lfield("", 30) // "             + 2*Sum(aibjck) (ai|bj)T(ak,ci;Lambda=1)*T(cj,bk;Lambda=1)")
                  call msg(lfield("", 30) // "using the Lyapunov equation to solve for dT/dLambda")
            end select
      end subroutine rpa_CC_Exchange_Summary


      subroutine rpa_CC_Exchange_SOSEX(EcExchange, Alpha, Vkai, Ak, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2, S2Operator)
            !
            ! EcExchange =  -Alpha * Sum(abij) (aj|bi)*Taibj
            !
            real(F64), intent(out)                                       :: EcExchange
            real(F64), intent(in)                                        :: Alpha
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)       :: Vkai
            real(F64), dimension(NVecsT2), intent(in)                    :: Ak
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)      :: Rkai
            integer, intent(in)                                          :: NOcc
            integer, intent(in)                                          :: NVirt
            integer, intent(in)                                          :: NVecsPiU
            integer, intent(in)                                          :: NVecsT2
            logical, intent(in)                                          :: S2Operator

            real(F64), dimension(:, :), allocatable :: W_ij, W_ji
            real(F64), dimension(:), allocatable :: Bk
            real(F64) :: EcExchange_ij
            integer :: i, j, k
            integer :: ThisImage

            ThisImage = this_image()
            allocate(W_ij(NVecsPiU, NVecsT2))
            allocate(W_ji(NVecsPiU, NVecsT2))
            allocate(Bk(NVecsT2))
            if (S2Operator) then
                  do k = 1, NVecsT2
                        Bk(k) = Ak(k) / (ONE - FOUR*Ak(k)**2)
                  end do
            else
                  Bk(:) = Ak
            end if
            EcExchange = ZERO
            do j = 1, NOcc
                  do i = j, NOcc
                        !
                        ! W(1:NVecsPiU,1:NVecsT2) = R(1:NVecsPiU,1:NVirt,i) V(1:NVecsT2,1:NVirt,j)**T
                        !
                        call real_abT_x(W_ij, NVecsPiU, Rkai(:, :, i), NVecsPiU, Vkai(:, :, j), NVecsT2, &
                              NVecsPiU, NVecsT2, NVirt, ONE, ZERO)
                        if (i /= j) then
                              call real_abT_x(W_ji, NVecsPiU, Rkai(:, :, j), NVecsPiU, Vkai(:, :, i), NVecsT2, &
                                    NVecsPiU, NVecsT2, NVirt, ONE, ZERO)
                        end if
                        call co_sum(W_ij, result_image=1)
                        if (i /= j) then
                              call co_sum(W_ji, result_image=1)
                        end if
                        if (ThisImage == 1) then
                              if (i /= j) then
                                    call rpa_CC_EcExchange1(EcExchange_ij, W_ij, W_ji, Bk, NVecsPiU, NVecsT2)
                                    EcExchange = EcExchange + TWO * EcExchange_ij
                              else
                                    call rpa_CC_EcExchange1(EcExchange_ij, W_ij, W_ij, Bk, NVecsPiU, NVecsT2)
                                    EcExchange = EcExchange + EcExchange_ij
                              end if
                        end if
                  end do
            end do
            EcExchange = Alpha * EcExchange
            sync all
      end subroutine rpa_CC_Exchange_SOSEX


      subroutine rpa_CC_EcExchange1(EcExchange, Wij, Wji, Ak, NVecsChol, NVecsT2)
            real(F64), intent(out)                 :: EcExchange
            real(F64), dimension(:, :), intent(in) :: Wij
            real(F64), dimension(:, :), intent(in) :: Wji
            real(F64), dimension(:), intent(in)    :: Ak
            integer, intent(in)                    :: NVecsChol
            integer, intent(in)                    :: NVecsT2

            integer :: k, l

            EcExchange = ZERO
            do l = 1, NVecsT2
                  do k = 1, NVecsChol
                        EcExchange = EcExchange - Wji(k, l) * Wij(k, l) * Ak(l)
                  end do
            end do
      end subroutine rpa_CC_EcExchange1


      subroutine rpa_CC_X(X, Vkai, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2)
            !
            ! X(k  l)  =  Sum(aibj) U(k,ai)(aj|bi)U(l,bj)
            !          =  Sum(aibj) Sum(m) U(k,ai)R(m,aj)R(m,bi)U(l,bj)
            !          =  Sum(ij) Sum(m) Wji(mk)Wij(ml)
            !
            ! Wij(mk)  = Sum(a) R(m,ai)U(k,aj)
            ! Wji(mk)  = Sum(b) R(m,bj)U(k,bi)
            !
            real(F64), dimension(NVecsT2, NVecsT2), intent(out)          :: X
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)       :: Vkai
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)      :: Rkai
            integer, intent(in)                                          :: NOcc
            integer, intent(in)                                          :: NVirt
            integer, intent(in)                                          :: NVecsPiU
            integer, intent(in)                                          :: NVecsT2

            real(F64), dimension(:, :), allocatable :: Wij, Wji
            integer :: i, j
            real(F64) :: Alpha
            integer :: ThisImage

            ThisImage = this_image()
            allocate(Wij(NVecsPiU, NVecsT2))
            allocate(Wji(NVecsPiU, NVecsT2))
            X = ZERO
            do j = 1, NOcc
                  do i = j, NOcc
                        !
                        ! W(1:NVecsPiU,1:NVecsT2) = R(1:NVecsPiU,1:NVirt,i) V(1:NVecsT2,1:NVirt,j)**T
                        !
                        call real_abT_x(Wij, NVecsPiU, Rkai(:, :, i), NVecsPiU, Vkai(:, :, j), NVecsT2, &
                              NVecsPiU, NVecsT2, NVirt, ONE, ZERO)
                        if (i /= j) then
                              call real_abT_x(Wji, NVecsPiU, Rkai(:, :, j), NVecsPiU, Vkai(:, :, i), NVecsT2, &
                                    NVecsPiU, NVecsT2, NVirt, ONE, ZERO)
                        end if
                        call co_sum(Wij, result_image=1)
                        if (i /= j) then
                              call co_sum(Wji, result_image=1)
                        end if
                        if (ThisImage == 1) then
                              !
                              ! X <- X + Wji**T Wij
                              !
                              if (i == j) then
                                    Alpha = ONE
                              else
                                    Alpha = TWO
                              end if
                              call real_aTb_x(X, NVecsT2, Wji, NVecsPiU, Wij, NVecsPiU, NVecsT2, NVecsT2, NVecsPiU, Alpha, ONE)
                        end if
                  end do
            end do
            sync all
      end subroutine rpa_CC_X


      subroutine rpa_CC_Ukl(Ukl, Vkai, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2)
            !
            ! U(kl)    =  Sum(aibj) V(k,ai)(ai|bj)V(l,bj)
            !          =  Sum(aibj) Sum(m) V(k,ai)R(m,aj)R(m,bi)V(l,bj)
            !          =  Sum(m) W(km)W(lm)
            !
            ! W(ml)  = Sum(ai) R(m,ai)V(l,ai)
            ! W(mk)  = Sum(bj) R(m,bj)V(k,bi)
            !
            real(F64), dimension(NVecsT2, NVecsT2), intent(out)          :: Ukl
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)       :: Vkai
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)      :: Rkai
            integer, intent(in)                                          :: NOcc
            integer, intent(in)                                          :: NVirt
            integer, intent(in)                                          :: NVecsPiU
            integer, intent(in)                                          :: NVecsT2

            real(F64), dimension(:, :), allocatable :: W
            integer :: ThisImage

            ThisImage = this_image()
            allocate(W(NVecsPiU, NVecsT2))
            W = ZERO
            Ukl = ZERO
            call real_abT_x(W, NVecsPiU, Rkai, NVecsPiU, Vkai, NVecsT2, NVecsPiU, NVecsT2, NVirt*NOcc, ONE, ZERO)
            call co_sum(W, result_image=1)
            if (ThisImage == 1) then
                  call real_aTb(Ukl, W, W)
            end if
            sync all
      end subroutine rpa_CC_Ukl


      subroutine rpa_CC_Dkl(Dkl, Vkai, OccEnergies, VirtEnergies, NOcc, NVirt, NVecsT2)
            integer, intent(in)                                     :: NVecsT2
            real(F64), dimension(NVecsT2, NVecsT2), intent(out)     :: Dkl
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
            real(F64), dimension(:), intent(in)                     :: OccEnergies
            real(F64), dimension(:), intent(in)                     :: VirtEnergies
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt

            integer :: i, a
            real(F64) :: dai

            Dkl = ZERO
            do i = 1, NOcc
                  do a = 1, NVirt
                        dai = VirtEnergies(a) - OccEnergies(i)
                        call real_vwT(Dkl, Vkai(:, a, i), Vkai(:, a, i), dai)
                  end do
            end do
      end subroutine rpa_CC_Dkl


      subroutine rpa_CC_Exchange_MBPT3_1(EcExchange, Vkai, Ak, NVecsT2, Rkai, NVecsPiU, &
            OccEnergies, VirtEnergies, NOcc, NVirt)

            real(F64), intent(out)                                  :: EcExchange
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
            real(F64), dimension(NVecsT2), intent(in)               :: Ak
            integer, intent(in)                                     :: NVecsT2
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in) :: Rkai
            integer, intent(in)                                     :: NVecsPiU
            real(F64), dimension(:), intent(in)                     :: OccEnergies
            real(F64), dimension(:), intent(in)                     :: VirtEnergies
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt

            integer :: m, n
            real(F64) :: Dmn, Umn, Am, An
            real(F64) :: TrXK
            real(F64), dimension(:, :), allocatable :: A, B, X

            allocate(A(NVecsT2, NVecsT2))
            allocate(B(NVecsT2, NVecsT2))
            allocate(X(NVecsT2, NVecsT2))
            call rpa_CC_Ukl(A, Vkai, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2)
            call rpa_CC_Dkl(B, Vkai, OccEnergies, VirtEnergies, NOcc, NVirt, NVecsT2)
            do n = 1, NVecsT2
                  do m = 1, NVecsT2
                        Dmn = B(m, n)
                        Umn = A(m, n)
                        Am = Ak(m)
                        An = Ak(n)                        
                        A(m, n) = Am*Dmn + Dmn*An
                        B(m, n) = Dmn + TWO*Umn + FOUR*Umn*An
                  end do
            end do
            !
            ! Solve A = B**T*X + X*B
            !
            call real_Lyapunov(X, B, A)
            call rpa_CC_TrXK(TrXK, X, Vkai, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2)
            EcExchange = -TrXK
      end subroutine rpa_CC_Exchange_MBPT3_1


      subroutine rpa_CC_TrXK(TrXK, Xmn, Vkai, Rkai, NOcc, NVirt, NVecsPiU, NVecsT2)
            !
            ! (dT/dLambda)aibj = Sum(mn) Vmai*Xmn*Vnbj
            ! (ai|bj) = Sum(k) Rkai*Rkbj
            ! TrXK =  Sum(abij) (aj|bi)*Taibj = Sum(aibj)Sum(mnk) Vmai*Xmn*Vnbj*Rkaj*Rkbi
            !
            real(F64), intent(out)                                       :: TrXK
            real(F64), dimension(NVecsT2, NVecsT2), intent(in)           :: Xmn
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)       :: Vkai
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in)      :: Rkai
            integer, intent(in)                                          :: NOcc
            integer, intent(in)                                          :: NVirt
            integer, intent(in)                                          :: NVecsPiU
            integer, intent(in)                                          :: NVecsT2

            real(F64), dimension(:, :), allocatable :: W_ij, W_ji, W
            real(F64) :: TrXKij
            integer :: i, j, m, n
            integer :: ThisImage

            ThisImage = this_image()
            allocate(W_ij(NVecsPiU, NVecsT2))
            allocate(W_ji(NVecsPiU, NVecsT2))
            allocate(W(NVecsT2, NVecsT2))
            TrXK = ZERO
            do j = 1, NOcc
                  do i = j, NOcc
                        !
                        ! W(1:NVecsPiU,1:NVecsT2) = R(1:NVecsPiU,1:NVirt,i) V(1:NVecsT2,1:NVirt,j)**T
                        !
                        call real_abT_x(W_ij, NVecsPiU, Rkai(:, :, i), NVecsPiU, Vkai(:, :, j), NVecsT2, &
                              NVecsPiU, NVecsT2, NVirt, ONE, ZERO)
                        if (i /= j) then
                              call real_abT_x(W_ji, NVecsPiU, Rkai(:, :, j), NVecsPiU, Vkai(:, :, i), NVecsT2, &
                                    NVecsPiU, NVecsT2, NVirt, ONE, ZERO)
                        end if
                        call co_sum(W_ij, result_image=1)
                        if (i /= j) then
                              call co_sum(W_ji, result_image=1)
                        end if
                        if (ThisImage == 1) then
                              if (i /= j) then
                                    call real_aTb(W, W_ji, W_ij)
                              else
                                    call real_aTb(W, W_ij, W_ij)
                              end if
                              TrXKij = ZERO
                              do n = 1, NVecsT2
                                    do m = 1, NVecsT2
                                          TrXKij = TrXKij + Xmn(m, n) * W(m, n)
                                    end do
                              end do
                              if (i /= j) then
                                    TrXK = TrXK + TWO * TrXKij
                              else
                                    TrXK = TrXK + TrXKij
                              end if
                        end if
                  end do
            end do
            sync all
      end subroutine rpa_CC_TrXK


      subroutine rpa_CC_TrW2(TrW2, Rkai, Vkai, Ak, NOcc, NVirt, NVecsPiU, NVecsT2)
            real(F64), intent(out)                                  :: TrW2
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in) :: Rkai
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
            real(F64), dimension(NVecsT2), intent(in)               :: Ak
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt
            integer, intent(in)                                     :: NVecsPiU
            integer, intent(in)                                     :: NVecsT2

            real(F64), dimension(:), allocatable :: Wm
            real(F64), dimension(:, :), allocatable :: Vli
            real(F64), dimension(:, :), allocatable :: Wai
            integer :: c, k, i, l

            allocate(Wm(NVecsPiU))
            allocate(Wai(NVirt, NOcc))
            allocate(Vli(NVecsT2, NOcc))
            TrW2 = ZERO
            do c = 1, NVirt
                  do i = 1, NOcc
                        Vli(:, i) = Ak(:) * Vkai(:, c, i)
                  end do
                  do k = 1, NOcc
                        !
                        ! Wai(1:NVirt, 1:NOcc) = V(1:NVecsT2,1:NVirt,k)**T*Vli(1:NVecsT2,1:NOcc)
                        !
                        call real_aTb(Wai, Vkai(:, :, k), Vli)
                        !
                        ! Wm(1:NVecsT2) = R(1:NVecsPiU,1:NVirt*NOcc)*Wai(1:NVirt*NOcc)
                        !
                        call real_av_x(Wm, Rkai, NVecsPiU, Wai, NVecsPiU, NVirt*NOcc, ONE, ZERO)
                        do l = 1, NVecsPiU
                              TrW2 = TrW2 + Wm(l)**2
                        end do
                  end do
            end do
      end subroutine rpa_CC_TrW2


      subroutine rpa_CC_Exchange_MBPT3_2(EcExchange, Rkai, Vkai, Ak, NOcc, NVirt, NVecsPiU, NVecsT2)
            real(F64), intent(out)                                  :: EcExchange
            real(F64), dimension(NVecsPiU, NVirt, NOcc), intent(in) :: Rkai
            real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
            real(F64), dimension(NVecsT2), intent(in)               :: Ak
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt
            integer, intent(in)                                     :: NVecsPiU
            integer, intent(in)                                     :: NVecsT2

            real(F64) :: TrW2

            call rpa_CC_TrW2(TrW2, Rkai, Vkai, Ak, NOcc, NVirt, NVecsPiU, NVecsT2)
            EcExchange = TWO * TrW2
      end subroutine rpa_CC_Exchange_MBPT3_2
end module rpa_CC_Exchange
