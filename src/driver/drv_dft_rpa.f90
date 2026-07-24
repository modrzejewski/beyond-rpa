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
   use basis_definitions
   use drv_eri

   implicit none

contains

   subroutine task_uks_rpa(System, SCFParams, RPAParams, Chol2Params, THCParams, BasisAssign)
      type(TSystem), intent(inout)               :: System
      type(TSCFParams), intent(in)               :: SCFParams
      type(TRPAParams), intent(inout)            :: RPAParams
      type(TChol2Params), intent(in)             :: Chol2Params
      type(TTHCParams), intent(inout)            :: THCParams
      type(TBasisAssignment), intent(in)         :: BasisAssign

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
      call basis_NewAOBasis(AOBasis, System, BasisAssign=BasisAssign)
      !
      ! Precompute tensors required for electron repulsion integrals
      !
      call drv_eri_run(Rkpq, Chol2Vecs, THCGrid, &
         AOBasis, System, SCFParams, Chol2Params, THCParams, &
         RPAParams)

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
      if (RPAParams%Algorithm == RPA_ALGO_JCTC2025) deallocate(Rkpq)
      !
      ! Switch to the coarse THC grid because numerical tests
      ! indicate that the post-SCF steps are less sensitive
      ! to grid point density
      !
      if (SCFParams%ERI_Algorithm == SCF_ERI_THC .and. &
          RPAParams%Algorithm == RPA_ALGO_JCTC2025) then
         call thc_ReduceGrid(THCGrid)
      end if
      call rpa_PostSCF(SCFOutput, SCFParams, AOBasis, RPAParams, &
         System, Rkpq, Chol2Vecs, THCGrid)
      call free_modules()
      call data_free()
   end subroutine task_uks_rpa
end module drv_dft_rpa
