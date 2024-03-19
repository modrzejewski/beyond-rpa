module CholeskyFock
      use arithmetic
      use CholeskyCoulomb
      use CholeskyExchange
      use ParallelCholesky
      use TwoStepCholesky_definitions
      use basis_sets
      
      implicit none

contains

      function chf_RhoTrace(Rho, A)
            !
            ! Calculate the average value <A> = Tr(Rho A).
            ! Only the lower triangles of Rho and A are referenced.
            !
            real(F64)                              :: chf_RhoTrace
            real(F64), dimension(:, :), intent(in) :: Rho
            real(F64), dimension(:, :), intent(in) :: A

            integer :: i, j, NAO

            NAO = size(Rho, dim=1)
            chf_RhoTrace = ZERO
            do j = 1, NAO
                  chf_RhoTrace = chf_RhoTrace + (ONE/TWO) * Rho(j, j) * A(j, j)
                  do i = j + 1, NAO
                        chf_RhoTrace = chf_RhoTrace + Rho(i, j) * A(i, j)
                  end do
            end do
            chf_RhoTrace = TWO * chf_RhoTrace
      end function chf_RhoTrace

      
      subroutine chf_JK(JK_ao, EHFTwoEl, Rho_ao, Cocc_ao, Rkpq, CholeskyBasis, NOcc, AOBasis, &
            CoulContrib, ExchContrib, KScal, MaxBufferDimMB, TargetBlockDim, SumJK)
            !
            ! Closed-shell case
            ! -----------------
            ! JK(p,q,1) = Sum(rs) (pq|rs)*Rho(rs,1) - KScal * Sum(i) (pi|iq)
            ! (index i denotes occupied orbitals)
            !
            ! Open-shell case
            ! ---------------
            ! JK(p,q,sigma) = Sum(rs) (pq|rs)*Rho(rs,sigma) - KScal * Sum(i) (pi|iq)
            ! (index i denotes occupied sigma spin-orbitals)
            !
            ! For Hartree-Fock use KScal=1.0.
            !
            ! EHFTwoEl is the JK matrix trace with the density matrix, i.e.,
            ! the JK integrals contribution to the Hartree-Fock total energy.
            !
            ! The JK matrix is optionally summed over all images and stored
            ! in the master image's memory. The EHFTwoElEnergy is always summed
            ! over all images.
            !
            real(F64), dimension(:, :, :), intent(out) :: JK_ao
            real(F64), intent(out)                     :: EHFTwoEl
            real(F64), dimension(:, :, :), intent(in)  :: Rho_ao
            real(F64), dimension(:, :, :), intent(in)  :: Cocc_ao
            real(F64), dimension(:, :, :), intent(in)  :: Rkpq
            type(TChol2Vecs), intent(in)               :: CholeskyBasis
            integer, dimension(:), intent(in)          :: NOcc
            type(TAOBasis), intent(in)                 :: AOBasis
            logical, intent(in)                        :: CoulContrib
            logical, intent(in)                        :: ExchContrib
            real(F64), intent(in)                      :: KScal
            integer, intent(in)                        :: MaxBufferDimMB
            integer, intent(in)                        :: TargetBlockDim
            logical, optional, intent(in)              :: SumJK

            integer :: NSpins, s
            integer :: ThisImage
            logical :: SumOverImages
            
            ThisImage = this_image()
            if (present(SumJK)) then
                  SumOverImages = SumJK
            else
                  SumOverImages = .true.
            end if
            NSpins = size(Rho_ao, dim=3)
            JK_ao = ZERO
            if (CoulContrib) then
                  call coul_J(JK_ao(:, :, 1), Rho_ao, AOBasis, Rkpq, CholeskyBasis)
                  if (NSpins == 2) JK_ao(:, :, 2) = JK_ao(:, :, 1)
            end if
            if (ExchContrib) then
                  call chf_K(JK_ao, Cocc_ao, NOcc, AOBasis, Rkpq, CholeskyBasis, &
                        MaxBufferDimMB, TargetBlockDim, KScal)
            end if
            EHFTwoEl = ZERO
            do s = 1, NSpins
                  EHFTwoEl = EHFTwoEl + (ONE/TWO) * chf_RhoTrace(Rho_ao(:, :, s), JK_ao(:, :, s))
            end do
            call co_sum(EHFTwoEl)
            if (SumOverImages) then
                  call co_sum(JK_ao, result_image=1)
            end if
      end subroutine chf_JK
end module CholeskyFock
