module CABS
      use arithmetic
      use sys_definitions
      use real_linalg
      use linalg
      use basis_sets
      use OneElectronInts

      implicit none

contains

      subroutine basis_CABS(V_CABS_cao, TotalBasis, PrimaryBasis, AuxBasis, System, LinDepThresh)
            !
            ! Compute the complimentary auxiliary basis set vectors, i.e., a set of column vectors
            ! belonging to AuxBasis, and, at the same time, orthogonal to PrimaryBasis.
            !
            real(F64), dimension(:, :), allocatable, intent(out) :: V_CABS_cao
            type(TAOBasis), intent(out)                          :: TotalBasis
            type(TAOBasis), intent(in)                           :: PrimaryBasis
            type(TAOBasis), intent(in)                           :: AuxBasis
            type(TSystem), intent(in)                            :: System
            real(F64), intent(in)                                :: LinDepThresh

            real(F64), dimension(:, :), allocatable :: STotal_cao
            real(F64), dimension(:, :), allocatable :: SAux_cao
            real(F64), dimension(:, :), allocatable :: SPrimary_cao
            real(F64), dimension(:, :), allocatable :: S_cao, S_oao
            real(F64), dimension(:, :), allocatable :: V_Primary_cao, V_Primary_sao
            real(F64), dimension(:, :), allocatable :: V_Aux_cao, V_Aux_sao
            real(F64), dimension(:, :), allocatable :: W
            real(F64), dimension(:, :), allocatable :: U, V
            real(F64), dimension(:), allocatable :: Sigma
            integer :: NOAOAux, NOAOPrimary, NCABS
            
            if (PrimaryBasis%SpherAO .neqv. AuxBasis%SpherAO) then
                  call msg("Inconsistent SpherAO parameters of the primary and auxiliary basis sets", MSG_ERROR)
                  error stop
            end if
            call basis_FuseBasisSets(TotalBasis, PrimaryBasis, AuxBasis, System, PrimaryBasis%SpherAO)

            associate ( &
                  NAO => TotalBasis%NAOCart, &
                  NAOPrimary => PrimaryBasis%NAOCart, &
                  NAOAux => AuxBasis%NAOCart &
                  )
                  allocate(STotal_cao(NAO, NAO))
                  allocate(SAux_cao(NAOAux, NAOAux))
                  allocate(SPrimary_cao(NAOPrimary, NAOPrimary))
                  allocate(S_cao(NAOPrimary, NAOAux))
                  !
                  ! Total overlap matrix and primary-primary and aux-aux blocks
                  ! expressed in atomic orbitals
                  !
                  call ints1e_OverlapMatrix(STotal_cao, TotalBasis)
                  call smfill(STotal_cao)
                  S_cao(1:NAOPrimary, 1:NAOAux) = STotal_cao(1:NAOPrimary, NAOPrimary+1:NAOPrimary+NAOAux)
                  SPrimary_cao(1:NAOPrimary, 1:NAOPrimary) = STotal_cao(1:NAOPrimary, 1:NAOPrimary)
                  SAux_cao(1:NAOAux, 1:NAOAux) = STotal_cao(NAOPrimary+1:NAOPrimary+NAOAux, NAOPrimary+1:NAOPrimary+NAOAux)
                  !
                  ! Linearly independent vectors of the primary and aux bases
                  !
                  call basis_OAO(V_Primary_cao, V_Primary_sao, SPrimary_cao, PrimaryBasis, LinDepThresh, .true.)
                  call basis_OAO(V_Aux_cao, V_Aux_sao, SAux_cao, AuxBasis, LinDepThresh, .true.)
                  NOAOAux = size(V_Aux_cao, dim=2)
                  NOAOPrimary = size(V_Primary_cao, dim=2)
                  NCABS = NOAOAux - NOAOPrimary
                  if (NCABS <= 0) then
                        call msg("Not enough auxiliary basis functions to generate vectors orthogonal to the primary basis", MSG_ERROR)
                        error stop
                  end if
                  !
                  ! Primary-Aux block of the overlap matrix expressed in
                  ! the orthogonalized nonredundant basis
                  !
                  allocate(W(NAOPrimary, NOAOAux))
                  allocate(S_oao(NOAOPrimary, NOAOAux))
                  call real_ab(W, S_cao, V_Aux_cao)
                  call real_aTb(S_oao, V_Primary_cao, W)
                  !
                  ! -------------- Matrices in the SVD routine -------
                  ! A = U Diag(Sigma) V**T
                  !
                  ! A          (m, n)
                  ! U          (m, m)
                  ! V          (n, n)
                  ! Sigma      array of size >= min(m, n)
                  !
                  allocate(U(NOAOPrimary, NOAOPrimary))
                  allocate(V(NOAOAux, NOAOAux))
                  allocate(Sigma(min(NOAOPrimary, NOAOAux)))
                  call real_SVD(U, V, Sigma, S_oao)
                  allocate(V_CABS_cao(NAOAux, NCABS))
                  call real_ab(V_CABS_cao, V_Aux_cao, V(:, NOAOPrimary+1:NOAOAux))
            end associate            
      end subroutine basis_CABS
end module CABS
