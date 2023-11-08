module PostSCF
      use arithmetic
      use real_scf
      use Fock
      use OneElectronInts
      use real_linalg
      use linalg
      use real_scf
      use Fock
      use clock
      use Pseudopotential, only: pp_V

      implicit none
      
contains

      subroutine postscf_F_AOPlusAux(F_ao, EtotHF, C_ao, SCFParams, System, AOBasisTotal, NAO, NAOTotal, NOcc)
            real(F64), dimension(NAOTotal, NAOTotal, 1), intent(out) :: F_ao
            real(F64), intent(out)                                   :: EtotHF
            real(F64), dimension(NAO, NOcc), intent(in)              :: C_ao
            type(TSCFParams), intent(in)                             :: SCFParams
            type(TSystem), intent(in)                                :: System
            type(TAOBasis), intent(in)                               :: AOBasisTotal
            integer, intent(in)                                      :: NAO
            integer, intent(in)                                      :: NAOTotal
            integer, intent(in)                                      :: NOcc

            real(F64), dimension(:, :), allocatable :: Rho_ao
            real(F64), dimension(:, :, :), allocatable :: Rho_ao_total
            real(F64) :: time_F
            real(F64) :: EHFTwoEl, EHbare, Enucl
            
            allocate(Rho_ao(NAO, NAO))
            allocate(Rho_ao_total(NAOTotal, NAOTotal, 1))
            !
            ! Rho <- 2 * Cocc Cocc**T
            !
            call real_abT_x(Rho_ao, NAO, C_ao, NAO, C_ao, NAO, &
                  NAO, NAO, NOcc, TWO, ZERO)
            Rho_ao_total = ZERO
            Rho_ao_total(1:NAO, 1:NAO, 1) = Rho_ao
            !
            ! Compute the Fock matrix in full AO+Aux basis
            !
            time_F = ZERO
            call postscf_FullFockMatrix(F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao_total, &
                  SCFParams, System, AOBasisTotal, time_F)
      end subroutine postscf_F_AOPlusAux

      
      subroutine postscf_Rho(OccCoeffs_ao, VirtCoeffs_ao, Rho_ao, C_oao, BasisVecs_ao, NOcc, NVirt)
            real(F64), dimension(:, :, :), allocatable, intent(out) :: OccCoeffs_ao
            real(F64), dimension(:, :, :), allocatable, intent(out) :: VirtCoeffs_ao
            real(F64), dimension(:, :, :), allocatable, intent(out) :: Rho_ao
            real(F64), dimension(:, :, :), contiguous, intent(in)   :: C_oao
            real(F64), dimension(:, :), intent(in)                  :: BasisVecs_ao
            integer, dimension(2), intent(in)                       :: NOcc
            integer, dimension(2), intent(in)                       :: NVirt

            integer :: MaxNi, MaxNa, NMO, NAO, NSpins
            integer :: s
            real(F64) :: OccNumber

            NMO = size(BasisVecs_ao, dim=2)
            NAO = size(BasisVecs_ao, dim=1)
            NSpins = size(C_oao, dim=3)
            MaxNi = 0
            MaxNa = 0
            do s = 1, NSpins
                  MaxNi = max(MaxNi, NOcc(s))
                  MaxNa = max(MaxNa, NVirt(s))
            end do            
            allocate(OccCoeffs_ao(NAO, MaxNi, NSpins))
            allocate(VirtCoeffs_ao(NAO, MaxNa, NSpins))
            allocate(Rho_ao(NAO, NAO, NSpins))
            OccCoeffs_ao = ZERO
            VirtCoeffs_ao = ZERO
            Rho_ao = ZERO
            if (NSpins == 1) then
                  OccNumber = TWO
            else
                  OccNumber = ONE
            end if
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call postscf_AOCoeffs(OccCoeffs_ao(:, 1:NOcc(s), s), C_oao(:, 1:NOcc(s), s), BasisVecs_ao)
                        call postscf_AOCoeffs(VirtCoeffs_ao(:, 1:NVirt(s), s), C_oao(:, NOcc(s)+1:NOcc(s)+NVirt(s), s), BasisVecs_ao)
                        call real_abT_x(Rho_ao(:, :, s), NAO, OccCoeffs_ao(:, :, s), NAO, OccCoeffs_ao(:, :, s), NAO, &
                              NAO, NAO, NOcc(s), OccNumber, ZERO)
                  else
                        OccCoeffs_ao(:, :, s) = ZERO
                        VirtCoeffs_ao(:, :, s) = ZERO
                        Rho_ao(:, :, s) = ZERO
                  end if
            end do
      end subroutine postscf_Rho
      

      subroutine postscf_F_mo(OccCoeffs_ao, VirtCoeffs_ao, F_mo, EtotHF, EHFTwoEl, EHbare, &
            C_oao, BasisVecs_ao, SCFParams, System, AOBasis, NOcc, NVirt, time_F)
            !
            ! Compute the Fock matrix in MO basis.
            !
            real(F64), dimension(:, :, :), allocatable, intent(out) :: OccCoeffs_ao
            real(F64), dimension(:, :, :), allocatable, intent(out) :: VirtCoeffs_ao
            real(F64), dimension(:, :, :), allocatable, intent(out) :: F_mo
            real(F64), intent(out)                                  :: EtotHF
            real(F64), intent(out)                                  :: EHFTwoEl
            real(F64), intent(out)                                  :: EHbare            
            real(F64), dimension(:, :, :), contiguous, intent(in)   :: C_oao
            real(F64), dimension(:, :), intent(in)                  :: BasisVecs_ao
            type(TSCFParams), intent(in)                            :: SCFParams
            type(TSystem), intent(in)                               :: System
            type(TAOBasis), intent(in)                              :: AOBasis
            integer, dimension(2), intent(in)                       :: NOcc
            integer, dimension(2), intent(in)                       :: NVirt
            real(F64), intent(inout)                                :: time_F
            
            integer :: s
            integer :: NMO, NAO, NSpins
            integer :: MaxNi, MaxNa
            real(F64) :: Enucl
            real(F64) :: OccNumber
            real(F64), dimension(:, :, :), allocatable :: Rho_ao, F_ao
            real(F64), dimension(:, :), allocatable :: FC, C
            integer :: ThisImage

            ThisImage = this_image()
            NMO = size(BasisVecs_ao, dim=2)
            NAO = size(BasisVecs_ao, dim=1)
            NSpins = size(C_oao, dim=3)
            MaxNi = 0
            MaxNa = 0
            do s = 1, NSpins
                  MaxNi = max(MaxNi, NOcc(s))
                  MaxNa = max(MaxNa, NVirt(s))
            end do            
            allocate(OccCoeffs_ao(NAO, MaxNi, NSpins))
            allocate(VirtCoeffs_ao(NAO, MaxNa, NSpins))
            allocate(Rho_ao(NAO, NAO, NSpins))
            allocate(F_ao(NAO, NAO, NSpins))
            OccCoeffs_ao = ZERO
            VirtCoeffs_ao = ZERO
            Rho_ao = ZERO
            if (NSpins == 1) then
                  OccNumber = TWO
            else
                  OccNumber = ONE
            end if
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call postscf_AOCoeffs(OccCoeffs_ao(:, 1:NOcc(s), s), C_oao(:, 1:NOcc(s), s), BasisVecs_ao)
                        call postscf_AOCoeffs(VirtCoeffs_ao(:, 1:NVirt(s), s), C_oao(:, NOcc(s)+1:NOcc(s)+NVirt(s), s), BasisVecs_ao)
                        call real_abT_x(Rho_ao(:, :, s), NAO, OccCoeffs_ao(:, :, s), NAO, OccCoeffs_ao(:, :, s), NAO, &
                              NAO, NAO, NOcc(s), OccNumber, ZERO)
                  else
                        OccCoeffs_ao(:, :, s) = ZERO
                        VirtCoeffs_ao(:, :, s) = ZERO
                        Rho_ao(:, :, s) = ZERO
                  end if
            end do
            call postscf_FullFockMatrix(F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao, &
                  SCFParams, System, AOBasis, time_F)
            !
            ! Transformation to AO basis
            !
            allocate(FC(NAO, NMO))
            allocate(F_mo(NMO, NMO, NSpins))
            allocate(C(NAO, NMO))
            if (ThisImage==1) then
                  do s = 1, NSpins
                        if (NOcc(s) > 0) then
                              C(:, 1:NOcc(s)) = OccCoeffs_ao(:, 1:NOcc(s), s)
                              C(:, NOcc(s)+1:NOcc(s)+NVirt(s)) = VirtCoeffs_ao(:, 1:NVirt(s), s)
                              call real_ab_x(FC, NAO, F_ao(:, :, s), NAO, C, NAO, &
                                    NAO, NMO, NAO, ONE, ZERO)                  
                              call real_aTb_x(F_mo(:, :, s), NMO, C, NAO, FC, NAO, &
                                    NMO, NMO, NAO, ONE, ZERO)
                        else
                              F_mo(:, :, s) = ZERO
                        end if
                  end do
            end if
            call co_broadcast(OccCoeffs_ao, source_image=1)
            call co_broadcast(VirtCoeffs_ao, source_image=1)
            call co_broadcast(F_mo, source_image=1)
            sync all            
      end subroutine postscf_F_mo


      subroutine postscf_Semicanonical(F_ao, OccCoeffs_ao, VirtCoeffs_ao, Fai, Fii, Faa, OrbEnergies, &
            EtotHF, EHFTwoEl, EHbare, C_oao, BasisVecs_ao, SCFParams, System, AOBasis, &
            NOcc, NVirt, time_F)

            real(F64), dimension(:, :, :), allocatable, intent(out) :: F_ao
            real(F64), dimension(:, :, :), allocatable, intent(out) :: OccCoeffs_ao
            real(F64), dimension(:, :, :), allocatable, intent(out) :: VirtCoeffs_ao
            real(F64), dimension(:, :), allocatable, intent(out)    :: Fai
            real(F64), dimension(:, :), allocatable, intent(out)    :: Fii
            real(F64), dimension(:, :), allocatable, intent(out)    :: Faa
            real(F64), dimension(:, :), allocatable, intent(out)    :: OrbEnergies
            real(F64), intent(out)                                  :: EtotHF
            real(F64), intent(out)                                  :: EHFTwoEl
            real(F64), intent(out)                                  :: EHbare
            real(F64), dimension(:, :, :), contiguous, intent(in)   :: C_oao
            real(F64), dimension(:, :), intent(in)                  :: BasisVecs_ao
            type(TSCFParams), intent(in)                            :: SCFParams
            type(TSystem), intent(in)                               :: System
            type(TAOBasis), intent(in)                              :: AOBasis
            integer, dimension(2), intent(in)                       :: NOcc
            integer, dimension(2), intent(in)                       :: NVirt
            real(F64), intent(inout)                                :: time_F

            integer :: s
            integer :: NMO, NAO, NSpins
            integer :: MaxNi, MaxNa, MaxNai
            real(F64) :: Enucl
            real(F64) :: OccNumber
            real(F64), dimension(:, :, :), allocatable :: Rho_ao
            integer :: ThisImage

            ThisImage = this_image()
            NMO = size(BasisVecs_ao, dim=2)
            NAO = size(BasisVecs_ao, dim=1)
            NSpins = size(C_oao, dim=3)
            MaxNi = 0
            MaxNa = 0
            MaxNai = 0
            do s = 1, NSpins
                  MaxNi = max(MaxNi, NOcc(s))
                  MaxNa = max(MaxNa, NVirt(s))
                  MaxNai = max(MaxNai, NVirt(s)*NOcc(s))
            end do            
            allocate(OccCoeffs_ao(NAO, MaxNi, NSpins))
            allocate(VirtCoeffs_ao(NAO, MaxNa, NSpins))
            allocate(Fai(MaxNai, NSpins))
            allocate(Faa(MaxNa, NSpins))
            allocate(Fii(MaxNi, NSpins))
            allocate(Rho_ao(NAO, NAO, NSpins))
            allocate(F_ao(NAO, NAO, NSpins))
            OccCoeffs_ao = ZERO
            VirtCoeffs_ao = ZERO
            Rho_ao = ZERO
            if (NSpins == 1) then
                  OccNumber = TWO
            else
                  OccNumber = ONE
            end if
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call postscf_AOCoeffs(OccCoeffs_ao(:, 1:NOcc(s), s), C_oao(:, 1:NOcc(s), s), BasisVecs_ao)
                        call postscf_AOCoeffs(VirtCoeffs_ao(:, 1:NVirt(s), s), C_oao(:, NOcc(s)+1:NOcc(s)+NVirt(s), s), BasisVecs_ao)
                        call real_abT_x(Rho_ao(:, :, s), NAO, OccCoeffs_ao(:, :, s), NAO, OccCoeffs_ao(:, :, s), NAO, &
                              NAO, NAO, NOcc(s), OccNumber, ZERO)
                  end if
            end do
            call postscf_FullFockMatrix(F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao, &
                  SCFParams, System, AOBasis, time_F)            
            if (ThisImage == 1) then
                  call postscf_SemicanonicalOrbitals_Spin(OccCoeffs_ao, VirtCoeffs_ao, Fai, Fii, Faa, F_ao, NOcc, NVirt)
            end if
            call co_broadcast(OccCoeffs_ao, source_image=1)
            call co_broadcast(VirtCoeffs_ao, source_image=1)
            call co_broadcast(Fai, source_image=1)
            call co_broadcast(Fii, source_image=1)
            call co_broadcast(Faa, source_image=1)
            call co_broadcast(F_ao, source_image=1)
            allocate(OrbEnergies(NMO, NSpins))
            OrbEnergies = ZERO
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        OrbEnergies(1:NOcc(s), s) = Fii(1:NOcc(s), s)
                        OrbEnergies(NOcc(s)+1:NOcc(s)+NVirt(s), s) = Faa(1:NVirt(s), s)
                  end if
            end do
            sync all
      end subroutine postscf_Semicanonical


      subroutine postscf_CopyOVBlocks(F_mo, Fai, NOcc, NVirt)
            real(F64), dimension(NOcc+NVirt, NOcc+NVirt), intent(inout) :: F_mo
            real(F64), dimension(NVirt, NOcc), intent(in)               :: Fai
            integer, intent(in)                                         :: NOcc
            integer, intent(in)                                         :: NVirt

            integer :: a, i

            do i = 1, NOcc
                  do a = 1, NVirt
                        F_mo(NOcc+a, i) = Fai(a, i)
                        F_mo(i, NOcc+a) = Fai(a, i)
                  end do
            end do
      end subroutine postscf_CopyOVBlocks


      subroutine postscf_AOCoeffs(C_ao, C_oao, MOBasisVecs)
            real(F64), dimension(:, :), intent(out) :: C_ao
            real(F64), dimension(:, :), intent(in)  :: C_oao
            real(F64), dimension(:, :), intent(in)  :: MOBasisVecs

            integer :: NAO, NMO, N

            N = size(C_oao, dim=2)
            NAO = size(MOBasisVecs, dim=1)
            NMO = size(MOBasisVecs, dim=2)
            call real_ab_x(C_ao, NAO, MOBasisVecs, NAO, C_oao, NMO, &
                  NAO, N, NMO, ONE, ZERO)
      end subroutine postscf_AOCoeffs

      
      subroutine postscf_SemicanonicalOrbitals_Spin(OccCoeffs_ao, VirtCoeffs_ao, Fai, Fii, Faa, F_ao, NOcc, NVirt)
            real(F64), dimension(:, :, :), intent(inout) :: OccCoeffs_ao
            real(F64), dimension(:, :, :), intent(inout) :: VirtCoeffs_ao
            real(F64), dimension(:, :), intent(out)      :: Fai
            real(F64), dimension(:, :), intent(out)      :: Fii
            real(F64), dimension(:, :), intent(out)      :: Faa
            real(F64), dimension(:, :, :), intent(in)    :: F_ao
            integer, dimension(2), intent(in)            :: NOcc
            integer, dimension(2), intent(in)            :: NVirt
            
            integer :: s, NSpins, NAO
            
            NSpins = size(F_ao, dim=3)
            NAO = size(F_ao, dim=1)
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call postscf_SemicanonicalOrbitals_NoSpin(OccCoeffs_ao(:, 1:NOcc(s), s), VirtCoeffs_ao(:, 1:NVirt(s), s), &
                              Fai(1:NOcc(s)*NVirt(s), s), Fii(:, s), Faa(:, s), F_ao(:, :, s), Nocc(s), NVirt(s), NAO)
                  else
                        Fai(:, s) = ZERO
                        Faa(:, s) = ZERO
                        Fii(:, s) = ZERO
                  end if
            end do
      end subroutine postscf_SemicanonicalOrbitals_Spin
      

      subroutine postscf_SemicanonicalOrbitals_NoSpin(OccCoeffs_ao, VirtCoeffs_ao, Fai, Fii, Faa, F_ao, Nocc, NVirt, NAO)
            real(F64), dimension(NAO, NOcc), intent(inout)  :: OccCoeffs_ao
            real(F64), dimension(NAO, NVirt), intent(inout) :: VirtCoeffs_ao
            real(F64), dimension(NVirt, NOcc), intent(out)  :: Fai
            real(F64), dimension(NOcc), intent(out)         :: Fii
            real(F64), dimension(NVirt), intent(out)        :: Faa
            real(F64), dimension(:, :), intent(in)          :: F_ao
            integer, intent(in)                             :: NOcc
            integer, intent(in)                             :: NVirt
            integer, intent(in)                             :: NAO

            real(F64), dimension(:, :), allocatable :: Fij, Fab
            real(F64), dimension(:, :), allocatable :: FC
            
            allocate(FC(NAO, max(NOcc,NVirt)))
            ! ----------------------------------------------------------
            !                 Occupied-occupied block
            ! ----------------------------------------------------------
            allocate(Fij(NOcc, NOcc))            
            Fij = ZERO
            FC = ZERO
            call real_ab_x(FC, NAO, F_ao, NAO, OccCoeffs_ao, NAO, &
                  NAO, NOcc, NAO, ONE, ZERO)                  
            call real_aTb_x(Fij, NOcc, OccCoeffs_ao, NAO, FC, NAO, &
                  NOcc, NOcc, NAO, ONE, ZERO)            
            call symmetric_eigenproblem(Fii, Fij, NOcc, .true.)
            FC(:, 1:NOcc) = OccCoeffs_ao
            call real_ab_x(OccCoeffs_ao, NAO, FC, NAO, Fij, NOcc, &
                  NAO, NOcc, NOcc, ONE, ZERO)
            deallocate(Fij)
            ! ----------------------------------------------------------
            !                  Virtual-virtual block
            ! ----------------------------------------------------------
            allocate(Fab(NVirt, NVirt))    
            Fab = ZERO
            FC = ZERO
            call real_ab_x(FC, NAO, F_ao, NAO, VirtCoeffs_ao, NAO, &
                  NAO, NVirt, NAO, ONE, ZERO)                  
            call real_aTb_x(Fab, NVirt, VirtCoeffs_ao, NAO, FC, NAO, &
                  NVirt, NVirt, NAO, ONE, ZERO)            
            call symmetric_eigenproblem(Faa, Fab, NVirt, .true.)
            FC(:, 1:NVirt) = VirtCoeffs_ao
            call real_ab_x(VirtCoeffs_ao, NAO, FC, NAO, Fab, NVirt, &
                  NAO, NVirt, NVirt, ONE, ZERO)
            deallocate(Fab)            
            ! ---------------------------------------------------------
            ! Transform the virtual-occupied block of the Fock matrix
            ! to semicanonical basis
            ! ---------------------------------------------------------
            call real_ab_x(FC, NAO, F_ao, NAO, OccCoeffs_ao, NAO, &
                  NAO, NOcc, NAO, ONE, ZERO)
            call real_aTb_x(Fai, NVirt, VirtCoeffs_ao, NAO, FC, NAO, &
                  NVirt, NOcc, NAO, ONE, ZERO)
      end subroutine postscf_SemicanonicalOrbitals_NoSpin
      
      
      subroutine postscf_FullFockMatrix(F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao, &
            SCFParams, System, AOBasis, time_F)
            !
            ! Compute full Fock matrix in AO basis including the kinetic operator,
            ! nuclei-electrons potential, possibly pseudopotential, and the Coulomb
            ! and exchange matrices. The AO basis is spherical or Cartesian depending
            ! on the SpherAO parameter in AOBasis.
            !
            real(F64), dimension(:, :, :), intent(out) :: F_ao
            real(F64), intent(out)                     :: EtotHF
            real(F64), intent(out)                     :: EHFTwoEl
            real(F64), intent(out)                     :: EHbare
            real(F64), intent(out)                     :: Enucl
            real(F64), dimension(:, :, :), intent(in)  :: Rho_ao
            type(TSCFParams), intent(in)               :: SCFParams
            type(TSystem), intent(in)                  :: System
            type(TAOBasis), intent(in)                 :: AOBasis
            real(F64), intent(inout)                   :: time_F

            integer :: NSpins, s
            integer :: DimTxc, DimJK, DimRho1D, NThreads
            real(F64), dimension(:, :), allocatable :: Ts_cao, Hbare_cao
            real(F64), dimension(:, :), allocatable :: Hbare_sao
            real(F64), dimension(:), allocatable :: TransfWork
            real(F64), dimension(:), allocatable :: BufferK, BufferJ, BufferRho1D
            type(TClock) :: timer_F
            integer :: ThisImage

            call blankline()
            call msg("Building Hartree-Fock Hamiltonian for post-SCF calculations")
            call msg("Threshold for J, K: |Rho(r,s)*(pq|rs)|,|Rho(q,s)*(pq|rs)| > " // str(SCFParams%ThreshFockJK,d=1))
            ThisImage = this_image()
            call clock_start(timer_F)
            associate ( &
                  ECPFile => SCFParams%ECPFile, &
                  SpherAO => AOBasis%SpherAO, &
                  NAOCart => AOBasis%NAOCart, &
                  NAOSpher => AOBasis%NAOSpher, &
                  ThreshFockJK => SCFParams%ThreshFockJK &
                  )
                  NSpins = size(Rho_ao, dim=3)
                  !
                  ! One-electron part of the hamiltonian
                  !
                  allocate(Ts_cao(NAOCart, NAOCart))
                  allocate(Hbare_cao(NAOCart, NAOCart))
                  call ints1e_Kinetic(Ts_cao, AOBasis)
                  call ints1e_Coulomb(Hbare_cao, AOBasis, System)
                  !
                  ! Add effective core potential to
                  ! the nuclei-electrons potential
                  !            
                  call pp_V(Hbare_cao, AOBasis, System, ECPFile)
                  Hbare_cao = Hbare_cao + Ts_cao
                  if (SpherAO) then
                        !
                        ! Transform the bare-nuclei hamiltonian to the spherical
                        ! AO basis. From now on, all components of the Fock matrix
                        ! will be expressed in the same basis which makes
                        ! the MO transformation simpler.
                        !
                        allocate(TransfWork(NAOSpher*NAOCart))
                        allocate(Hbare_sao(NAOSpher, NAOSpher))
                        call smfill(Hbare_cao)
                        call SpherGTO_TransformMatrix_U(Hbare_sao, Hbare_cao, &
                              AOBasis%LmaxGTO, &
                              AOBasis%NormFactorsSpher, &
                              AOBasis%NormFactorsCart, &
                              AOBasis%ShellLocSpher, &
                              AOBasis%ShellLocCart, &
                              AOBasis%ShellMomentum, &
                              AOBasis%ShellParamsIdx, &
                              AOBasis%NAOSpher, &
                              AOBasis%NAOCart, &
                              AOBasis%NShells, TransfWork)
                  end if
                  call scf_BufferDim(DimTxc, DimJK, DimRho1D, NThreads, AOBasis)
                  allocate(BufferK(DimJK))
                  allocate(BufferJ(DimJK))
                  allocate(BufferRho1D(DimRho1D))
                  !
                  ! Hartree-Fock exchange and Coulomb matrices
                  !
                  call fock_JK(F_ao, BufferK, BufferJ, BufferRho1D, Rho_ao, AOBasis, .true., ONE, &
                        .false., .false., .false., -ONE, .true., ThreshFockJK)
                  call co_sum(F_ao, result_image=1)
                  EHFTwoEl = ZERO
                  EHbare = ZERO
                  Enucl = ZERO
                  if (ThisImage == 1) then
                        do s = 1, NSpins
                              EHFTwoEl = EHFTwoEl + (ONE/TWO) * fock_RhoTrace(Rho_ao(:, :, s), F_ao(:, :, s))
                        end do
                        do s = 1, NSpins
                              if (SpherAO) then
                                    EHbare = EHbare + fock_RhoTrace(Rho_ao(:, :, s), Hbare_sao)
                                    F_ao(:, :, s) = F_ao(:, :, s) + Hbare_sao
                              else
                                    EHbare = EHbare + fock_RhoTrace(Rho_ao(:, :, s), Hbare_cao)
                                    F_ao(:, :, s) = F_ao(:, :, s) + Hbare_cao
                              end if
                              call smfill(F_ao(:, :, s))
                        end do
                        call sys_NuclearRepulsion(Enucl, System)                              
                  end if
                  call co_broadcast(EHFTwoEl, source_image=1)
                  call co_broadcast(EHbare, source_image=1)
                  call co_broadcast(Enucl, source_image=1)
                  EtotHF = EHbare + EHFTwoEl + Enucl
                  time_F = time_F + clock_readwall(timer_F)
            end associate            
            sync all
            call msg("HF matrix build completed in " // str(clock_readwall(timer_F), d=1) // " seconds")
            call msg("Total HF energy: " // str(EtotHF,d=10))
      end subroutine postscf_FullFockMatrix


      subroutine postscf_EHFTwoEl(EHFTwoEl, Rho_ao, AOBasis, ThreshFockJK)
            !
            ! Compute the two-electron part of the Hartree-Fock energy.
            !
            real(F64), intent(out)                     :: EHFTwoEl
            real(F64), dimension(:, :, :), intent(in)  :: Rho_ao
            type(TAOBasis), intent(in)                 :: AOBasis
            real(F64), intent(in)                      :: ThreshFockJK

            integer :: NSpins, s
            integer :: DimTxc, DimJK, DimRho1D, NThreads
            real(F64), dimension(:), allocatable :: BufferK, BufferJ, BufferRho1D
            real(F64), dimension(:, :, :), allocatable :: F_ao
            integer :: ThisImage

            ThisImage = this_image()
            associate ( &
                  SpherAO => AOBasis%SpherAO, &
                  NAOCart => AOBasis%NAOCart, &
                  NAOSpher => AOBasis%NAOSpher &
                  )
                  NSpins = size(Rho_ao, dim=3)
                  call scf_BufferDim(DimTxc, DimJK, DimRho1D, NThreads, AOBasis)
                  allocate(BufferK(DimJK))
                  allocate(BufferJ(DimJK))
                  allocate(BufferRho1D(DimRho1D))
                  if (SpherAO) then
                        allocate(F_ao(NAOSpher, NAOSpher, NSpins))
                  else
                        allocate(F_ao(NAOCart, NAOCart, NSpins))
                  end if
                  !
                  ! Hartree-Fock exchange and Coulomb matrices
                  !
                  call fock_JK(F_ao, BufferK, BufferJ, BufferRho1D, Rho_ao, AOBasis, .true., ONE, &
                        .false., .false., .false., -ONE, .true., ThreshFockJK)
                  call co_sum(F_ao, result_image=1)
                  EHFTwoEl = ZERO
                  if (ThisImage == 1) then
                        do s = 1, NSpins
                              EHFTwoEl = EHFTwoEl + (ONE/TWO) * fock_RhoTrace(Rho_ao(:, :, s), F_ao(:, :, s))
                        end do
                  end if
                  call co_broadcast(EHFTwoEl, source_image=1)
            end associate            
            sync all
      end subroutine postscf_EHFTwoEl
end module PostSCF
