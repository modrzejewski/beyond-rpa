module drv_dft_rpa
      use scf_definitions
      use sys_definitions
      use thc_definitions
      use basis_sets
      use real_scf
      use rpa_driver
      use basis
      use initialize
      use ParallelCholesky
      use TwoStepCholesky_definitions
      use thc_definitions
      use TwoStepCholesky
      use CABS
      use PostSCF
      use MolproInterface

      implicit none

contains

      subroutine task_uks_rpa(System, SCFParams, RPAParams, Chol2Params, THCParams)
            type(TSystem), intent(inout)    :: System
            type(TSCFParams), intent(in)    :: SCFParams
            type(TRPAParams), intent(inout) :: RPAParams
            type(TChol2Params), intent(in)  :: Chol2Params
            type(TTHCParams), intent(inout) :: THCParams
            
            type(TSCFOutput), dimension(15) :: SCFOutput
            type(TAOBasis) :: AOBasis
            type(TChol2Vecs) :: Chol2Vecs
            type(TCoulTHCGrid) :: THCGrid
            real(F64), dimension(:, :, :), allocatable :: Rkpq[:]
            integer :: NSystems
            integer :: k
            real(F64) :: time_Fock

            if (System%SystemKind == SYS_MOLECULE) then
                  NSystems = 1
            else if (System%SystemKind == SYS_DIMER) then
                  NSystems = 3
            else if (System%SystemKind == SYS_TRIMER) then
                  NSystems = 7
            else ! Tetramer
                  NSystems = 15
            end if

            time_Fock = ZERO
            
            call sys_Init(System, SYS_TOTAL)
            call data_load_2(System)
            call init_modules()
            call basis_NewAOBasis(AOBasis, System, SCFParams%AOBasisPath, SCFParams%SpherAO)
            ! -------------------------------------------------------------------------------
            !                        Tensor hypercontraction
            ! -------------------------------------------------------------------------------
            if (SCFParams%ERI_ALGORITHM==SCF_ERI_THC .and. RPAParams%TensorHypercontraction) then
                  !
                  ! Both SCF and post-SCF employ the tensor
                  ! hypercontraction decoposition. Enable
                  ! two levels of THC grid: tight for SCF and
                  ! loose for the post-SCF step
                  !
                  THCParams%QRThresh = SCFParams%THC_QRThresh
                  THCParams%QRThreshReduced = RPAParams%THC_QRThresh
                  if (THCParams%QRThreshReduced < THCParams%QRThresh) then
                        call msg("Invalid THC thresholds: SCF_QRThresh > QRThreshReduced", MSG_ERROR)
                        error stop
                  end if
            else if (RPAParams%TensorHypercontraction) then
                  THCParams%QRThresh = RPAParams%THC_QRThresh
                  THCParams%QRThreshReduced = -ONE
            else if (SCFParams%ERI_ALGORITHM==SCF_ERI_THC) then
                  THCParams%QRThresh = SCFParams%THC_QRThresh
                  THCParams%QRThreshReduced = -ONE
            end if
            if ((SCFParams%ERI_Algorithm == SCF_ERI_EXACT .or. &
                  SCFParams%ERI_Algorithm == SCF_ERI_THC) &
                  .and. RPAParams%TensorHypercontraction) then
                  THCParams%THC_QuadraticMemory = .true.
                  call thc_CoulombMatrix_QuadraticMemory(THCGrid, Chol2Vecs, AOBasis, &
                        System, THCParams, Chol2Params)
                  allocate(Rkpq(0, 0, 0)[*])
            else
                  call chol2_Algo_Koch_JCP2019(Chol2Vecs, AOBasis, Chol2Params)
                  call chol2_FullDimVectors(Rkpq, Chol2Vecs, AOBasis, Chol2Params)
                  if (RPAParams%TensorHypercontraction) then
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
                        allocate(THCGrid%Zgh(THCGrid%NGrid, THCGrid%NGrid))
                        call real_abT(THCGrid%Zgh, THCGrid%Zgk, THCGrid%Zgk)
                  end if
            end if
            do k = 1, NSystems
                  if (k > 1) then
                        call sys_Init(System, k)
                        call data_load_2(System)
                        call init_modules()
                  end if                  
                  call scf_driver_SpinUnres(SCFOutput(k), SCFParams, AOBasis, System, &
                        Rkpq, Chol2Vecs, THCGrid)
                  if (.not. SCFOutput(k)%Converged) then
                        call msg("SCF not converged. Cannot continue with a post-SCF calculation", MSG_ERROR)
                        error stop
                  end if
                  if (k < NSystems) then
                        call free_modules()
                        call data_free()
                  end if
            end do
            if (RPAParams%TensorHypercontraction) deallocate(Rkpq)
            if (SCFParams%ERI_Algorithm==SCF_ERI_THC .and. RPAParams%TensorHypercontraction) then
                  call thc_ReduceGrid(THCGrid)
            end if
            call rpa_PostSCF(SCFOutput, SCFParams, AOBasis, RPAParams, &
                  System, Rkpq, Chol2Vecs, Chol2Params, THCGrid)            
            call free_modules()
            call data_free()
      end subroutine task_uks_rpa
end module drv_dft_rpa
