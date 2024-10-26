module THCFock
      use arithmetic
      use real_linalg
      use thc_definitions

      implicit none

contains

      subroutine thc_Fock_Exchange(Kpq, XXgh, XZXXgp, Xig, Xgp, Zgh, KScal)
            !
            ! Compute the exchange matrix in AO basis
            !
            ! K(p,q) = -KScal * Sum(i) (pi|iq)
            !
            ! For Hartree-Fock use KScal=1.0.
            !
            real(F64), dimension(:, :), intent(inout) :: Kpq
            real(F64), dimension(:, :), intent(out)   :: XXgh
            real(F64), dimension(:, :), intent(out)   :: XZXXgp
            real(F64), dimension(:, :), intent(in)    :: Xig
            real(F64), dimension(:, :), intent(in)    :: Xgp
            real(F64), dimension(:, :), intent(in)    :: Zgh
            real(F64), intent(in)                     :: KScal

            integer :: NGridTHC, NAO

            NGridTHC = size(Xgp, dim=1)
            NAO = size(Xgp, dim=2)
            !
            ! [XX](g,h) = Sum(i) X(i,g)*X(i,g)
            !
            call real_aTb(XXgh, Xig, Xig)
            !
            ! [ZXX](g,h) = Z(g,h)*[XX](x,h)
            !
            XXgh = XXgh * Zgh
            !
            ! [XZXX](g,p) = Sum(h) [ZXX](x,h)*X(h,p)
            !
            call real_ab(XZXXgp, XXgh, Xgp)
            !
            ! K(p,q) = Sum(g) X(g,p) [XZXX](g,q)
            !
            call real_aTb_x(Kpq, NAO, Xgp, NGridTHC, XZXXgp, NGridTHC, &
                  NAO, NAO, NGridTHC, -KScal, ONE)            
      end subroutine thc_Fock_Exchange


      subroutine thc_Fock_Coulomb(Jpq, XXg, ZXXg, XZXXgp, Xig, Xgp, Zgh, JScal)
            !
            ! Compute the Coulomb matrix in AO basis
            !
            ! J(p,q) = JScal * Sum(i) (pq|ii)
            !
            ! For closed-shell Hartree-Fock use JScal=2.0.
            !
            real(F64), dimension(:, :), intent(inout) :: Jpq
            real(F64), dimension(:), intent(out)      :: XXg
            real(F64), dimension(:), intent(out)      :: ZXXg
            real(F64), dimension(:, :), intent(out)   :: XZXXgp
            real(F64), dimension(:, :), intent(in)    :: Xig
            real(F64), dimension(:, :), intent(in)    :: Xgp
            real(F64), dimension(:, :), intent(in)    :: Zgh
            real(F64), intent(in)                     :: JScal

            integer :: NGridTHC, NAO, NOcc
            integer :: g, p
            real(F64) :: t

            NGridTHC = size(Xgp, dim=1)
            NAO = size(Xgp, dim=2)
            NOcc = size(Xig, dim=1)
            !
            ! [XX](g) = Sum(i) X(i,g)*X(i,g)
            !
            !$omp parallel do private(g, t)
            do g = 1, NGridTHC
                  call real_vw_x(t, Xig(:, g), Xig(:, g), NOcc)
                  XXg(g) = t
            end do
            !$omp end parallel do
            !
            ! [ZXX](g) = Sum(h) Z(g,h)*[XX](h)
            !
            call real_Av(ZXXg, Zgh, XXg)
            !
            ! [XZXX](g,p) = X(g,p) * [ZXX](g)
            !
            !$omp parallel do private(p)
            do p = 1, NAO
                  XZXXgp(:, p) = Xgp(:, p) * ZXXg(:)
            end do
            !$omp end parallel do
            !
            ! J(p,q) = Sum(g) X(g,p)*XZXXgp(g,q)
            !
            call real_aTb_x(Jpq, NAO, Xgp, NGridTHC, XZXXgp, NGridTHC, &
                  NAO, NAO, NGridTHC, JScal, ONE)
      end subroutine thc_Fock_Coulomb


      subroutine thc_Fock_JK(JKpq, EHFTwoEl, Cpi, Rho_ao, Zgh, Xgp, NOcc, CoulContrib, &
            ExchContrib, KScal)

            real(F64), dimension(:, :, :), intent(out) :: JKpq
            real(F64), intent(out)                     :: EHFTwoEl
            real(F64), dimension(:, :, :), intent(in)  :: Cpi
            real(F64), dimension(:, :, :), intent(in)  :: Rho_ao
            real(F64), dimension(:, :), intent(in)     :: Zgh
            real(F64), dimension(:, :), intent(in)     :: Xgp
            integer, dimension(:), intent(in)          :: NOcc
            logical, intent(in)                        :: CoulContrib
            logical, intent(in)                        :: ExchContrib
            real(F64), intent(in)                      :: KScal

            integer :: NSpins, NGridTHC, NAO
            real(F64), dimension(:), allocatable :: XXg, ZXXg
            real(F64), dimension(:, :), allocatable :: XXgh, XZXXgp, Xig, Jpq
            real(F64) :: TrRhoJK
            integer :: s
            
            NSpins = size(Cpi, dim=3)
            NGridTHC = size(Zgh, dim=1)
            NAO = size(Cpi, dim=1)
            allocate(XZXXgp(NGridTHC, NAO))
            if (CoulContrib) then
                  allocate(XXg(NGridTHC))
                  allocate(ZXXg(NGridTHC))
                  if (NSpins == 2) allocate(Jpq(NAO, NAO))
            end if
            if (ExchContrib) then
                  allocate(XXgh(NGridTHC, NGridTHC))
            end if
            JKpq = ZERO
            do s = 1, NSpins
                  allocate(Xig(NOcc(s), NGridTHC))
                  !
                  ! Xig(i,g) = Sum(p) Cpi(p,i)*Xgp(g,p)
                  !
                  call real_aTbT(Xig, Cpi(:, 1:NOcc(s), s), Xgp)
                  if (CoulContrib) then
                        if (NSpins == 2) then
                              Jpq = ZERO
                              call thc_Fock_Coulomb(Jpq, XXg, ZXXg, XZXXgp, Xig, Xgp, Zgh, ONE)
                              JKpq(:, :, 1) = JKpq(:, :, 1) + Jpq
                              JKpq(:, :, 2) = JKpq(:, :, 2) + Jpq
                        else
                              call thc_Fock_Coulomb(JKpq(:, :, 1), XXg, ZXXg, XZXXgp, Xig, Xgp, Zgh, TWO)
                        end if
                  end if
                  if (ExchContrib) then
                        call thc_Fock_Exchange(JKpq(:, :, s), XXgh, XZXXgp, Xig, Xgp, Zgh, KScal)
                  end if
                  deallocate(Xig)
            end do
            !
            ! Coulomb+exchange part of the total energy
            !
            EHFTwoEl = ZERO
            do s = 1, NSpins
                  call real_vw_x(TrRhoJK, Rho_ao(:, :, s), JKpq(:, :, s), NAO**2)
                  EHFTwoEl = EHFTwoEl + (ONE/TWO) * TrRhoJK
            end do
            call co_sum(EHFTwoEl)
      end subroutine thc_Fock_JK
end module THCFock
