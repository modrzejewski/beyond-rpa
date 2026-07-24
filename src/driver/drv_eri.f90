module drv_eri
      use math_constants
      use arithmetic
      use sys_definitions
      use basis_definitions
      use scf_definitions
      use TwoStepCholesky_definitions
      use rpa_definitions
      use TwoStepCholesky
      use TensorHypercontraction
      use display

      implicit none

contains

      subroutine drv_eri_run(Rkpq, Chol2Vecs, THCGrid, &
            AOBasis, System, SCFParams, Chol2Params, THCParams, &
            RPAParams)
            
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: Rkpq[:]
            type(TChol2Vecs), intent(out)                           :: Chol2Vecs
            type(TCoulTHCGrid), intent(out)                         :: THCGrid
            type(TAOBasis), intent(in)                              :: AOBasis
            type(TSystem), intent(in)                               :: System
            type(TSCFParams), intent(in)                            :: SCFParams
            type(TChol2Params), intent(in)                          :: Chol2Params
            type(TTHCParams), intent(inout)                         :: THCParams
            type(TRPAParams), intent(in), optional                  :: RPAParams
            
            logical :: PostSCF_Active
            logical :: PostSCF_THC
            logical :: RequiresFullCholesky
            logical :: RequiresTHC
            
            PostSCF_Active = present(RPAParams)
            PostSCF_THC = .false.
            if (PostSCF_Active) then
                  PostSCF_THC = (RPAParams%Algorithm == RPA_ALGO_JCTC2025)
            end if

            RequiresTHC = (SCFParams%ERI_ALGORITHM == SCF_ERI_THC) .or. PostSCF_THC
            
            RequiresFullCholesky = (SCFParams%ERI_ALGORITHM == SCF_ERI_CHOLESKY)
            if (PostSCF_Active .and. .not. PostSCF_THC) then
                  !
                  ! If there is a post-SCF step (e.g. rPT2) but it doesn't use THC,
                  ! it strictly requires full dense Cholesky vectors.
                  !
                  RequiresFullCholesky = .true.
            end if

            if (RequiresTHC) then
                  !
                  ! Two stage THC: the SCF step requires tighter thresholds than RPA.
                  !
                  if (SCFParams%ERI_ALGORITHM == SCF_ERI_THC .and. PostSCF_THC) then
                        THCParams%QRThresh = SCFParams%THC_QRThresh
                        THCParams%QRThreshReduced = RPAParams%THC_QRThresh
                        if (THCParams%QRThreshReduced < THCParams%QRThresh) then
                              call msg("Invalid THC thresholds: SCF_QRThresh > QRThreshReduced", MSG_ERROR)
                              error stop
                        end if
                  else if (PostSCF_THC) then
                        THCParams%QRThresh = RPAParams%THC_QRThresh
                        THCParams%QRThreshReduced = -ONE
                  else if (SCFParams%ERI_ALGORITHM == SCF_ERI_THC) then
                        THCParams%QRThresh = SCFParams%THC_QRThresh
                        THCParams%QRThreshReduced = -ONE
                  end if
            end if

            if (RequiresFullCholesky) then
                  call chol2_Algo_Koch_JCP2019(Chol2Vecs, AOBasis, Chol2Params)
                  call chol2_FullDimVectors(Rkpq, Chol2Vecs, AOBasis, Chol2Params)
                  
                  if (RequiresTHC) then
                        !
                        ! The THC grid is required, but the dense Rkpq tensor has already
                        ! been requested. The quadratic memory algorithm cannot be used here.
                        ! Build the grid using the existing Rkpq tensor.
                        !
                        THCParams%THC_QuadraticMemory = .false.
                        call thc_Grid( &
                              THCGrid%Xgp, &
                              THCGrid%NGrid, &
                              THCGrid%NGridReduced, &
                              THCParams%THC_BeckeGridKind, &
                              THCParams%PhiSquaredThresh, &
                              THCParams%QRThresh, &          
                              THCParams%QRThreshReduced, &
                              THCParams%THC_BlockDim, &
                              AOBasis, System)
                        call thc_Z( &
                              THCGrid%Zgk, &
                              THCGrid%ZgkReduced, &
                              THCGrid%NGrid, &
                              THCGrid%NGridReduced, &
                              THCGrid%Xgp, &
                              Rkpq, Chol2Vecs, Chol2Params, &
                              AOBasis, THCParams)
                  end if
            else if (RequiresTHC) then
                  !
                  ! The THC grid is required, and no part of the workflow demands the
                  ! dense Rkpq tensor. Use the highly efficient quadratic memory algorithm.
                  !
                  THCParams%THC_QuadraticMemory = .true.
                  call thc_CoulombMatrix_QuadraticMemory(THCGrid, Chol2Vecs, AOBasis, &
                        System, THCParams, Chol2Params)
                  allocate(Rkpq(0, 0, 0)[*])
            else
                  allocate(Rkpq(0, 0, 0)[*])
            end if
      end subroutine drv_eri_run

end module drv_eri
