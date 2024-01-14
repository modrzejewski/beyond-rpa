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
            type(TSystem), intent(inout)   :: System
            type(TSCFParams), intent(in)   :: SCFParams
            type(TRPAParams), intent(in)   :: RPAParams
            type(TChol2Params), intent(in) :: Chol2Params
            type(TTHCParams), intent(in)   :: THCParams
            
            type(TSCFOutput), dimension(15) :: SCFOutput
            type(TAOBasis) :: AOBasis
            type(TChol2Vecs) :: CholeskyBasis
            type(TCoulTHCGrid) :: THCGrid
            real(F64), dimension(:, :, :), allocatable :: CholeskyVecs[:]
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
            ! ------------------------------------------------------------------------
            !              Cholesky decomposition of the Coulomb matrix
            ! ------------------------------------------------------------------------
            if (SCFParams%UseCholeskyBasis .or. RPAParams%TensorHypercontraction) then
                  if (THCParams%THC_QuadraticMemory) then
                        call thc_CoulombMatrix_QuadraticMemory(THCGrid, AOBasis, &
                              System, THCParams, Chol2Params)
                        allocate(CholeskyVecs(0, 0, 0)[*])
                  else
                        call chol2_Algo_Koch_JCP2019(CholeskyBasis, AOBasis, Chol2Params)
                        call chol2_FullDimVectors(CholeskyVecs, CholeskyBasis, AOBasis, Chol2Params)
                        ! call chol_CoulombMatrix_B(CholeskyVecs, CholeskyBasis, AOBasis, RPAParams)
                        if (RPAParams%TensorHypercontraction) then
                              associate ( &
                                    BeckeGridKind => RPAParams%THC_BeckeGridKind, & ! parent molecular grid
                                    QRThresh => RPAParams%THC_QRThresh, & ! threshold for rank-revealing QR/Cholesky
                                    BlockDim => RPAParams%THC_BlockDim, & ! block dimension for the on the fly THC/Cholesky
                                    NCholesky => CholeskyBasis%NVecs, &
                                    NSubsets => CholeskyBasis%NSubsets, &
                                    ShellPairs => CholeskyBasis%ShellPairs, &
                                    ShellPairLoc => CholeskyBasis%ShellPairLoc, &
                                    ShellPairDim => CholeskyBasis%ShellPairDim, &
                                    SubsetDim => CholeskyBasis%SubsetDim, &
                                    SubsetBounds => CholeskyBasis%SubsetBounds, &
                                    Rkpq => CholeskyVecs &
                                    )
                                    call thc_Grid(THCGrid%Xgp, BeckeGridKind, QRThresh, BlockDim, AOBasis, System)
                                    call thc_Z(THCGrid%Zgk, THCGrid%Xgp, Rkpq, CholeskyBasis, Chol2Params, AOBasis, THCParams)
                              end  associate
                        end if
                  end if
            else
                  allocate(CholeskyVecs(0, 0, 0)[*])
            end if
            do k = 1, NSystems
                  if (k > 1) then
                        call sys_Init(System, k)
                        call data_load_2(System)
                        call init_modules()
                  end if                  
                  call scf_driver_SpinUnres(SCFOutput(k), SCFParams, AOBasis, System, &
                        CholeskyVecs, CholeskyBasis, THCGrid)
                  if (.not. SCFOutput(k)%Converged) then
                        call msg("SCF not converged. Cannot continue with a post-SCF calculation", MSG_ERROR)
                        error stop
                  end if
                  
                  if (k < NSystems) then
                        call free_modules()
                        call data_free()
                  end if
            end do

            if (RPAParams%TensorHypercontraction) deallocate(CholeskyVecs)
            call rpa_PostSCF(SCFOutput, SCFParams, AOBasis, RPAParams, System, CholeskyVecs, CholeskyBasis, THCGrid)
            
            call free_modules()
            call data_free()
      end subroutine task_uks_rpa
end module drv_dft_rpa
