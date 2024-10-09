! -----------------------------------------------------------------------
!               SECOND-ORDER SCF SOLVER FOR SPIN-RESTRICTED
!                   AND SPIN-UNRESTRICTED KOHN-SHAM
! -----------------------------------------------------------------------
module uks_arh
      use arithmetic
      use math_constants
      use gparam
      use display
      use real_linalg
      use matexp
      use clock

      implicit none
      !
      ! Maximum number of stored density matrices
      !
      integer, parameter            :: UARH_MAX_NSTOR = 8
      !
      ! First-iteration value of trust radius (X matrix norm)
      !
      real(F64), parameter          :: UARH_START_TRUST = 0.5_F64
      !
      ! Threshold value at which the norm of X is switched from max to Frobenius
      !
      real(F64), parameter          :: UARH_FROBENIUS = 0.05_F64
      !
      ! Number of stored density matrices at which the second-order algorithm
      ! is enabled
      !
      integer, parameter            :: UARH_SECOND_ORDER = 2
      real(F64), parameter          :: UARH_LINDEP_THRESH = 1.0E-7_F64
      !
      ! Value of the occupied-virtual energy gap which enables energy shift
      !
      real(F64), parameter          :: UARH_MINGAP = 1.0E-2_F64
      !
      ! Threshold for linear dependencies among the density
      ! matrices stored in memory
      !
      real(F64), parameter          :: UARH_ORTH_THRESH = SCF_ORTH_THRESH
      integer, parameter            :: UARH_NORMTYP_FROB = 0
      integer, parameter            :: UARH_NORMTYP_MAX  = 1

      type uarhdata
            !
            ! Occupation number, scales the energy derivative: dE/dDpq = OccNumber*Fpq
            !
            real(F64) :: OccNumber
            !
            ! Definition of the occupied and virtual orbitals which are active
            ! during the optimization
            ! Occupied index (i): i in [OCC0, OCC1]
            ! Virtual index (a):  a in [VIRT0, VIRT1]
            !
            integer, dimension(2) :: OccRangeA
            integer, dimension(2) :: OccRangeB
            integer, dimension(2) :: VirtRangeA
            integer, dimension(2) :: VirtRangeB
            real(F64), dimension(:, :, :, :), allocatable :: Fk_oao
            real(F64), dimension(:, :, :), allocatable :: CkA_oao
            real(F64), dimension(:, :, :), allocatable :: CkB_oao
            integer, dimension(UARH_MAX_NSTOR) :: idx_map
            integer :: NStored = 0
            real(F64) :: shift = ZERO
            logical :: enable_shift = .true.
            real(F64) :: trust_radius = UARH_START_TRUST
            real(F64) :: Epred 
            integer :: norm_type = UARH_NORMTYP_MAX
            real(F64) :: XNorm
            logical :: SpinUnres
      end type uarhdata

 contains

       subroutine uarh_init(a, OccNumber, OccRangeA, OccRangeB, VirtRangeA, VirtRangeB, &
             Noao, SpinUnres)
             type(uarhdata), intent(out)       :: a
             real(F64)                         :: OccNumber
             integer, dimension(2), intent(in) :: OccRangeA
             integer, dimension(2), intent(in) :: OccRangeB
             integer, dimension(2), intent(in) :: VirtRangeA
             integer, dimension(2), intent(in) :: VirtRangeB
             integer, intent(in)               :: Noao
             logical, intent(in)               :: SpinUnres

             integer :: NoccA, NoccB
             !
             ! Note that some of the fields of A reset to their default values
             ! as a result of the intent(out) declaration.
             !
             a%OccNumber = OccNumber
             a%SpinUnres = SpinUnres
             if (SpinUnres) then
                   a%OccRangeA = OccRangeA
                   a%OccRangeB = OccRangeB
                   a%VirtRangeA = VirtRangeA
                   a%VirtRangeB = VirtRangeB
                   NoccA = OccRangeA(2) - OccRangeA(1) + 1
                   NoccB = OccRangeB(2) - OccRangeB(1) + 1
                   if (NoccA == 0 .or. NoccB == 0) then
                         call msg("Cannot handle zero occupied orbitals in the spin-unrestricted mode", MSG_ERROR)
                         stop
                   end if
                   allocate(a%CkA_oao(Noao, NoccA, UARH_MAX_NSTOR))
                   allocate(a%CkB_oao(Noao, NoccB, UARH_MAX_NSTOR))
                   allocate(a%Fk_oao(Noao, Noao, UARH_MAX_NSTOR, 2))
             else
                   a%OccRangeA = OccRangeA
                   a%OccRangeB = OccRangeB
                   a%VirtRangeA = VirtRangeA
                   a%VirtRangeB = VirtRangeB
                   NoccA = OccRangeA(2) - OccRangeA(1) + 1
                   allocate(a%CkA_oao(Noao, NoccA, UARH_MAX_NSTOR))
                   allocate(a%CkB_oao(0, 0, 0))
                   allocate(a%Fk_oao(Noao, Noao, UARH_MAX_NSTOR, 1))
             end if
       end subroutine uarh_init


       subroutine uarh_free(a)
             type(uarhdata), intent(out) :: a
       end subroutine uarh_free


       subroutine uarh_DisableShift(a)
             type(uarhdata), intent(inout) :: a
             a%enable_shift = .false.
       end subroutine uarh_DisableShift


       subroutine uarh_TrustRadius(d, updated, EelN, EelK)
             ! ----------------------------------------------------------
             ! Update the trust radius for the ARH quadratic model
             ! of energy, see eq. 4 in [2]. The trust radius is updated
             ! according to the difference between the predicted
             ! electronic energy and EelN.
             ! ----------------------------------------------------------
             ! 1. Host, S. et al., The augmented Roothaan-Hall
             !    method for optimizing Hartree-Fock and Kohn-Sham
             !    density matrices,
             !    J. Chem. Phys. 129, 124106(2008)
             ! 2. Host, S. et al., A ground-state-directed
             !    optimization scheme for the Kohn-Sham energy,
             !    Phys. Chem. Chem. Phys. 10, 5344(2008)
             !
             type(uarhdata), intent(inout) :: d
             logical, intent(out)          :: updated
             real(F64), intent(in)         :: EelN
             real(F64), intent(in)         :: EelK
             
             real(F64) :: r, denom
             !
             ! Check the quality of energy estimate:
             ! Eqs. 50 & 51 in [1]
             !
             denom = d%Epred - EelK
             if (denom >= ZERO) then
                   !
                   ! Avoid division by zero and numerically inaccurate denominators.
                   ! The predicted energy should by definition be lower
                   ! than the prev iter's energy.
                   !
                   call msg("Skipping trust radius update", MSG_WARNING)
                   updated = .false.
                   return
             end if
             r = (EelN - EelK) / denom
             !
             ! A higher the value of R means the ARH energy estimate
             ! is close to the true energy
             !
             if (r >= 0.75_F64) then
                   updated = .true.
                   d%trust_radius = 1.2_F64 * d%trust_radius
             else if (r >= 0.00_F64) then
                   updated = .false.
                   continue
             else
                   updated = .true.
                   !
                   ! Because the predicted energy is always lower than
                   ! EOLD, R can be negative only if EelN > EelK
                   !
                   d%trust_radius = 0.7_F64 * d%trust_radius
                   !
                   ! If trust radius is still greater than XNORM,
                   ! decreasing the trust radius will not the change
                   ! step size!
                   !
                   if (d%trust_radius > d%xnorm) d%trust_radius = 0.7_F64 * d%xnorm
             end if
       end subroutine uarh_TrustRadius

       
       function uarh_freeidx(idx_map, nstor)
             integer                           :: uarh_freeidx
             integer, dimension(:), intent(in) :: idx_map
             integer, intent(in)               :: nstor
             
             integer :: i, j
             logical :: occupied
             
             uarh_freeidx = -1
             if (nstor >= UARH_MAX_NSTOR) then
                   uarh_freeidx = -1
             else if (nstor > 0 .and. nstor < UARH_MAX_NSTOR) then
                   iloop: do i = 1, UARH_MAX_NSTOR
                         occupied = .false.
                         jloop: do j = 1, nstor
                               if (idx_map(j) == i) then
                                     occupied = .true.
                                     exit jloop
                               end if
                         end do jloop

                         if (.not. occupied) then
                               uarh_freeidx = i
                               exit iloop
                         end if
                   end do iloop
             else if (nstor == 0) then
                   uarh_freeidx = 1
             end if
       end function uarh_freeidx

       
       subroutine uarh_store(Fk_oao, CkA_oao, CkB_oao, idx_map, NStored, &
             Fn_oao, CnA_oao, CnB_oao, SpinUnres)
             
             real(F64), dimension(:, :, :, :), intent(inout) :: Fk_oao
             real(F64), dimension(:, :, :), intent(inout)    :: CkA_oao
             real(F64), dimension(:, :, :), intent(inout)    :: CkB_oao
             integer, dimension(:), intent(inout)            :: idx_map
             integer, intent(inout)                          :: NStored
             real(F64), dimension(:, :, :), intent(in)       :: Fn_oao
             real(F64), dimension(:, :), intent(in)          :: CnA_oao
             real(F64), dimension(:, :), intent(in)          :: CnB_oao
             logical, intent(in)                             :: SpinUnres

             integer :: i, idx1

             if (NStored < UARH_MAX_NSTOR) then
                   i = uarh_freeidx(idx_map, NStored)
                   Fk_oao(:, :, i, 1) = Fn_oao(:, :, 1)
                   CkA_oao(:, :, i) = CnA_oao
                   if (SpinUnres) then
                         Fk_oao(:, :, i, 2) = Fn_oao(:, :, 2)
                         CkB_oao(:, :, i) = CnB_oao
                   end if
                   idx_map(NStored+1) = i
                   NStored = NStored + 1
             else
                   !
                   ! idx_map(1) is the index of the oldest stored pair (Fk, Dk),
                   ! idx_map(2) is the second oldest etc.
                   ! Overwrite the oldest pair with (Fn, Dn).
                   !
                   idx1 = idx_map(1)
                   Fk_oao(:, :, idx1, 1) = Fn_oao(:, :, 1)
                   CkA_oao(:, :, idx1) = CnA_oao
                   if (SpinUnres) then
                         Fk_oao(:, :, idx1, 2) = Fn_oao(:, :, 2)
                         CkB_oao(:, :, idx1) = CnB_oao
                   end if
                   do i = 1, UARH_MAX_NSTOR - 1
                         idx_map(i) = idx_map(i + 1)
                   end do
                   idx_map(UARH_MAX_NSTOR) = idx1
             end if
       end subroutine uarh_store


       subroutine uarh_del(idx_map, i, nstor)
             !
             ! Delete the i-th pair of matrices (Dk, Fk)
             ! i=1     Oldest pair
             ! i=nstor Newest pair
             !
             integer, dimension(:), intent(inout) :: idx_map
             integer, intent(in)                  :: i
             integer, intent(in)                  :: nstor

             integer :: k

             if (nstor < 0 .or. nstor > UARH_MAX_NSTOR) then
                   call msg("Invalid value of NSTOR on entry to UARH_DEL", MSG_ERROR)
                   stop
             end if

             if (i <= 0) then
                   call msg("Invalid value of I on entry to UARH_DEL (I <= 0)", MSG_ERROR)
                   stop
             end if

             if (i > nstor) then
                   call msg("Invalid value of I on entry to UARH_DEL (I > NSTOR)", MSG_ERROR)
                   stop
             end if
                   
             if (nstor > 0) then
                   do k = i, nstor - 1
                         idx_map(k) = idx_map(k + 1)
                   end do
             end if
       end subroutine uarh_del


       subroutine uarh_islindep(lindep, del_list, GramMatrix)
             !
             ! Test if the density matrix from the n-th iteration, Dn,
             ! is linearly independent Dk's from the previous iterations
             !
             logical, intent(out)                            :: lindep
             integer, dimension(:), allocatable, intent(out) :: del_list
             real(F64), dimension(:, :), intent(in)          :: GramMatrix

             real(F64) :: mineig, TrD
             real(F64), dimension(:, :), allocatable :: ts
             real(F64), dimension(:), allocatable :: w
             integer :: nremoved
             integer :: k, m
             integer :: n

             n = size(GramMatrix, dim=1)
             allocate(ts(n, n))
             allocate(w(n))
             nremoved = 0
             TrD = GramMatrix(n, n)
             !
             ! If addition of RHO(N) causes linear dependence, try to delete
             ! try to delete, in the following order, RHO(N-1), RHO(N-2), etc.
             ! It is tested numerically that this algorithm leads to the largest
             ! number of density matrices at the end of SCF.
             !
             delloop: do k = 1, n - 1
                   !
                   ! Compute the eigenvalues of the Gram matrix
                   !
                   ts = GramMatrix / TrD
                   call symmetric_eigenproblem(w, ts, n-nremoved, .false.)
                   !
                   ! Check if smallest eigenvalue of the overlap
                   ! matrix is below the linear dependency threshold
                   !
                   mineig = minval(w(1:n-nremoved))
                   if (mineig < UARH_LINDEP_THRESH) then
                         nremoved = nremoved + 1
                   else
                         exit delloop
                   end if
             end do delloop

             if (nremoved > 0) then
                   lindep = .true.
                   allocate(del_list(nremoved))
                   do m = 1, nremoved
                         del_list(m) = n - m
                   end do
             else
                   lindep = .false.
             end if
       end subroutine uarh_islindep


       subroutine uarh_ov2oao(X_oao, Wpi, X_ov, Cocc_ov, Cvirt_ov, NOcc, NVirt, NOAO)
             ! 
             ! Transform X_ov from the occupied-virtual basis to the orthogonalized
             ! atomic orbitals basis. Take into account the virtual-occupied block
             ! of X_ov which is not stored in memory.
             !
             ! X_oao = Sum_{ia} (Xia * Ci Ca**H - Conj(Xia) * C_a C_i**H)
             !
             integer, intent(in)                            :: NOcc, NVirt, NOAO
             real(F64), dimension(NOAO, NOAO), intent(out)  :: X_oao
             real(F64), dimension(NOAO, NOcc), intent(out)  :: Wpi
             real(F64), dimension(NOcc, NVirt), intent(in)  :: X_ov
             real(F64), dimension(NOAO, NOcc), intent(in)   :: Cocc_ov
             real(F64), dimension(NOAO, NVirt), intent(in)  :: Cvirt_ov

             integer :: p
             
             call real_abT(Wpi, Cvirt_ov, X_ov)
             call real_abT(X_oao, Cocc_ov, Wpi)
             call real_abT_x(X_oao, NOAO, Wpi, NOAO, Cocc_ov, NOAO, NOAO, NOAO, NOcc, &
                   -ONE, ONE)
             do p = 1, NOAO
                   X_oao(p, p) = ZERO
             end do
       end subroutine uarh_ov2oao
       

       subroutine uarh_find_smaller(XA_ov, XB_ov, sigma_X, shift, X_norm, FnA_ov, FnB_ov, &
             FkA_ov, FkB_ov, DkA_ov, DkB_ov, EigA, EigB, Tinv, nstor, TargetNorm, norm_type, &
             gap, SecondOrder, SpinUnres)
             !
             ! Find an X matrix for which ||X|| < TargetNorm
             !
             real(F64), dimension(:, :), intent(out)   :: XA_ov
             real(F64), dimension(:, :), intent(out)   :: XB_ov
             real(F64), dimension(:), intent(out)      :: sigma_X
             real(F64), intent(out)                    :: shift
             real(F64), intent(out)                    :: X_norm
             real(F64), dimension(:, :), intent(in)    :: FnA_ov
             real(F64), dimension(:, :), intent(in)    :: FnB_ov
             real(F64), dimension(:, :, :), intent(in) :: FkA_ov
             real(F64), dimension(:, :, :), intent(in) :: FkB_ov
             real(F64), dimension(:, :, :), intent(in) :: DkA_ov
             real(F64), dimension(:, :, :), intent(in) :: DkB_ov
             real(F64), dimension(:), intent(in)       :: EigA
             real(F64), dimension(:), intent(in)       :: EigB
             real(F64), dimension(:, :), intent(in)    :: Tinv
             integer, intent(in)                       :: nstor
             real(F64), intent(in)                     :: TargetNorm
             integer, intent(in)                       :: norm_type
             real(F64), intent(in)                     :: gap
             logical, intent(in)                       :: SecondOrder
             logical, intent(in)                       :: SpinUnres

             integer :: k
             real(F64) :: Delta, shift0

             if (gap < ZERO .or. abs(gap) < UARH_MINGAP) then
                   Delta = -(ONE/TWO) * UARH_MINGAP
                   shift0 = gap + Delta
             else
                   Delta = -(ONE/TWO) * gap
                   shift0 = Delta
             end if

             shift = shift0

             do k = 1, 50
                   if (SecondOrder) then
                         call uarh_X(XA_ov, XB_ov, sigma_X, FnA_ov, FnB_ov, FkA_ov, FkB_ov, DkA_ov, DkB_ov, &
                               EigA, EigB, Tinv, nstor, shift, SpinUnres)
                   else
                         call uarh_Y(XA_ov, XB_ov, FnA_ov, FnB_ov, EigA, EigB, shift, SpinUnres)
                         sigma_X = ZERO
                   end if
                   X_norm = uarh_matrixnorm(XA_ov, XB_ov, norm_type, SpinUnres)
                   if (X_norm < TargetNorm) then
                         exit
                   else
                         Delta = Delta * TWO
                         shift = shift0 + Delta
                   end if
             end do
       end subroutine uarh_find_smaller

       
       function uarh_maxnorm(A_ov)
             !
             ! Compute the max norm of the matrix A:
             ! MaxNorm = Max(|Aij|)
             !
             real(F64) :: uarh_maxnorm
             real(F64), dimension(:, :), intent(in) :: A_ov

             integer :: Nocc, Nvirt
             integer :: i, a
             real(F64) :: sij, sij_max

             Nocc = size(A_ov, dim=1)
             Nvirt = size(A_ov, dim=2)
             sij_max = ZERO
             !$omp parallel do private(a, i, sij) reduction(max:sij_max) collapse(2)
             do a = 1, Nvirt
                   do i = 1, Nocc
                         sij = abs(A_ov(i, a))
                         sij_max = max(sij_max, sij)
                   end do
             end do
             !$omp end parallel do
             uarh_maxnorm = sij_max
       end function uarh_maxnorm
       

       function uarh_frobnorm(A_ov)
             !
             ! Compute the Frobenius norm of the matrix A:
             ! FrobNorm = Sqrt(Tr(A A**H)).
             !
             ! The sum computed by this subroutine is multiplied by two to
             ! compensate for the virtual-occupied block which is not
             ! stored in memory.
             !
             real(F64) :: uarh_frobnorm
             real(F64), dimension(:, :), intent(in) :: A_ov

             integer :: Nocc, Nvirt
             real(F64) :: sij_sum

             Nocc = size(A_ov, dim=1)
             Nvirt = size(A_ov, dim=2)
             call real_vw_x(sij_sum, A_ov, A_ov, NVirt*NOcc)
             !
             ! Multiply by two to make up for the remaining
             ! virtal-occupied block
             !
             uarh_frobnorm = sqrt(TWO * sij_sum)
       end function uarh_frobnorm
       

       function uarh_matrixnorm(XA_ov, XB_ov, norm_type, SpinUnres)
             real(F64)                              :: uarh_matrixnorm
             real(F64), dimension(:, :), intent(in) :: XA_ov
             real(F64), dimension(:, :), intent(in) :: XB_ov
             integer, intent(in)                    :: norm_type
             logical, intent(in)                    :: SpinUnres

             real(F64) :: sA, sB
             
             if (norm_type == UARH_NORMTYP_MAX) then
                   if (SpinUnres) then
                         sA = uarh_maxnorm(XA_ov)
                         sB = uarh_maxnorm(XB_ov)
                   else
                         sA = uarh_maxnorm(XA_ov)
                         sB = ZERO
                   end if
                   uarh_matrixnorm = max(sA, sB)
             else
                   if (SpinUnres) then
                         sA = uarh_frobnorm(XA_ov)
                         sB = uarh_frobnorm(XB_ov)
                   else
                         sA = uarh_frobnorm(XA_ov)
                         sB = ZERO
                   end if
                   uarh_matrixnorm = hypot(sA, sB)
             end if
       end function uarh_matrixnorm

       
       subroutine uarh_EnergyModel(Emodel, X_ov, sigma_X, Fn_ov, Fk_ov, Tinv, eig, &
             NStored, OccNumber, SecondOrder)
             real(F64), intent(out)                    :: Emodel
             real(F64), dimension(:, :), intent(in)    :: X_ov
             real(F64), dimension(:), intent(in)       :: sigma_X
             real(F64), dimension(:, :), intent(in)    :: Fn_ov
             real(F64), dimension(:, :, :), intent(in) :: Fk_ov
             real(F64), dimension(:, :), intent(in)    :: Tinv
             real(F64), dimension(:), intent(in)       :: eig
             integer, intent(in)                       :: NStored
             real(F64)                                 :: OccNumber
             logical, intent(in)                       :: SecondOrder

             integer :: Nocc, Nvirt, k, l, i, a
             real(F64) :: e11, e21, e22, e22k, Fnii, Fnaa
             real(F64) :: DeltaFkia
             real(F64), dimension(NStored) :: TS

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             e11 = ZERO
             e21 = ZERO
             !$omp parallel do private(a, i, Fnaa, Fnii) reduction(+:e11,e21) collapse(2)
             do a = 1, Nvirt
                   do i = 1, Nocc
                         Fnaa = eig(Nocc+a)
                         Fnii = eig(i)
                         e21 = e21 + X_ov(i, a)**2 * (Fnaa - Fnii)
                         e11 = e11 + TWO * X_ov(i, a) * Fn_ov(i, a)
                   end do
             end do
             !$omp end parallel do
             e11 = OccNumber * e11
             e21 = OccNumber * e21
             e22 = ZERO
             if (SecondOrder) then
                   TS = ZERO
                   do k = 1, NStored
                         do l = 1, NStored
                               TS(k) = TS(k) + Tinv(k, l) * sigma_X(l)
                         end do
                   end do
                   do k = 1, NStored
                         e22k = ZERO
                         !$omp parallel do private(a, i, DeltaFkia) reduction(+:e22k) collapse(2)
                         do a = 1, Nvirt
                               do i = 1, Nocc
                                     DeltaFkia = Fk_ov(i, a, k) - Fn_ov(i, a)
                                     e22k = e22k + TWO * DeltaFkia * X_ov(i, a) * TS(k)
                               end do
                         end do
                         !$omp end parallel do
                         e22 = e22 + e22k
                   end do
                   e22 = (ONE/TWO) * OccNumber * e22
             end if
             Emodel = e11 + e21 + e22
       end subroutine uarh_EnergyModel


       subroutine uarh_Omega(Omega, Tinv, FnA_ov, FnB_ov, FkA_ov, FkB_ov, DkA_ov, DkB_ov, &
             EigA, EigB, NStored, shift, SpinUnres)
             
             real(F64), dimension(:, :), intent(out)   :: Omega
             real(F64), dimension(:, :), intent(in)    :: Tinv
             real(F64), dimension(:, :), intent(in)    :: FnA_ov
             real(F64), dimension(:, :), intent(in)    :: FnB_ov
             real(F64), dimension(:, :, :), intent(in) :: FkA_ov
             real(F64), dimension(:, :, :), intent(in) :: FkB_ov
             real(F64), dimension(:, :, :), intent(in) :: DkA_ov
             real(F64), dimension(:, :, :), intent(in) :: DkB_ov
             real(F64), dimension(:), intent(in)       :: EigA
             real(F64), dimension(:), intent(in)       :: EigB
             integer, intent(in)                       :: NStored
             real(F64), intent(in)                     :: shift
             logical, intent(in)                       :: SpinUnres

             integer :: k, l, m
             real(F64), dimension(:, :), allocatable :: tau

             allocate(tau(NStored, NStored))
             tau = ZERO
             call uarh_tauA(tau, FnA_ov, FkA_ov, DkA_ov, EigA, NStored, shift)
             if (SpinUnres) then
                   call uarh_tauA(tau, FnB_ov, FkB_ov, DkB_ov, EigB, NStored, shift)
             end if
             do l = 1, NStored
                   do m = 1, NStored
                         if (m == l) then
                               Omega(m, l) = ONE
                         else
                               Omega(m, l) = ZERO
                         end if
                         do k = 1, NStored
                               Omega(m, l) = Omega(m, l) - tau(k, m) * Tinv(k, l)
                         end do
                   end do
             end do
       end subroutine uarh_Omega
             

       subroutine uarh_tauA(tau, Fn_ov, Fk_ov, Dk_ov, eig, NStored, shift)
             real(F64), dimension(:, :), intent(inout) :: tau
             real(F64), dimension(:, :), intent(in)    :: Fn_ov
             real(F64), dimension(:, :, :), intent(in) :: Fk_ov
             real(F64), dimension(:, :, :), intent(in) :: Dk_ov
             real(F64), dimension(:), intent(in)       :: eig
             integer, intent(in)                       :: NStored
             real(F64), intent(in)                     :: shift

             real(F64) :: DeltaFkia, Dmia
             real(F64) :: Fnaa, Fnii
             integer :: k, m, i, a, Nocc, Nvirt
             real(F64), dimension(NStored, NStored) :: T

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             T = ZERO
             !$omp parallel do private(m, k, a, i) &
             !$omp private(DeltaFkia, Dmia, Fnaa, Fnii) &
             !$omp reduction(+:T) collapse(4)
             do m = 1, NStored
                   do k = 1, NStored
                         do a = 1, Nvirt
                               do i = 1, Nocc
                                     DeltaFkia = Fk_ov(i, a, k) - Fn_ov(i, a)
                                     Dmia = Dk_ov(i, a, m)
                                     Fnaa = eig(Nocc+a)
                                     Fnii = eig(i)
                                     T(k, m) = T(k, m) - TWO * DeltaFkia * Dmia / (Fnaa - Fnii - shift)
                               end do
                         end do
                   end do
             end do
             !$omp end parallel do
             Tau(1:NStored, 1:NStored) = Tau(1:NStored, 1:NStored) + T
       end subroutine uarh_tauA


       subroutine uarh_sigma_YA(sigma_Y, Fn_ov, eig, Dk_ov, NStored, shift)
             real(F64), dimension(:), intent(inout)    :: sigma_Y
             real(F64), dimension(:, :), intent(in)    :: Fn_ov
             real(F64), dimension(:), intent(in)       :: eig
             real(F64), dimension(:, :, :), intent(in) :: Dk_ov
             integer, intent(in)                       :: NStored
             real(F64), intent(in)                     :: shift

             real(F64) :: Yia, Dkia
             integer :: k, i, a, Nocc, Nvirt
             real(F64), dimension(NStored) :: S

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             S = ZERO
             !$omp parallel do private(k, a, i) &
             !$omp private(Dkia, Yia) reduction(+:S) &
             !$omp collapse(3)
             do k = 1, NStored
                   do a = 1, Nvirt
                         do i = 1, Nocc
                               Dkia = Dk_ov(i, a, k)
                               Yia = -Fn_ov(i, a) / (eig(Nocc+a) - eig(i) - shift)
                               S(k) = S(k) + TWO * Dkia * Yia
                         end do
                   end do
             end do
             !$omp end parallel do
             Sigma_Y(1:NStored) = Sigma_Y(1:NStored) + S
       end subroutine uarh_sigma_YA


       subroutine uarh_XA(X_ov, Fn_ov, Fk_ov, eig, sigma_X, Tinv, NStored, shift)
             real(F64), dimension(:, :), intent(out)   :: X_ov
             real(F64), dimension(:, :), intent(in)    :: Fn_ov
             real(F64), dimension(:, :, :), intent(in) :: Fk_ov
             real(F64), dimension(:), intent(in)       :: eig
             real(F64), dimension(:), intent(in)       :: sigma_X
             real(F64), dimension(:, :), intent(in)    :: Tinv
             integer, intent(in)                       :: NStored
             real(F64), intent(in)                     :: shift

             real(F64) :: Fnii, Fnaa
             real(F64) :: DeltaFkia
             integer :: k, l, i, a, Nocc, Nvirt
             real(F64), dimension(NStored) :: TS

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             !$omp parallel do private(a, i) collapse(2)
             do a = 1, Nvirt
                   do i = 1, Nocc
                         X_ov(i, a) = -Fn_ov(i, a) / (eig(Nocc+a) - eig(i) - shift)
                   end do
             end do
             !$omp end parallel do
             TS = ZERO
             do k = 1, NStored
                   do l = 1, NStored
                         TS(k) = TS(k) + Tinv(k, l) * sigma_X(l)
                   end do
             end do
             do k = 1, NStored
                   !$omp parallel do private(a, i) &
                   !$omp private(DeltaFkia, Fnaa, Fnii) &
                   !$omp collapse(2)
                   do a = 1, Nvirt
                         do i = 1, Nocc
                               DeltaFkia = Fk_ov(i, a, k) - Fn_ov(i, a)
                               Fnaa = eig(Nocc+a)
                               Fnii = eig(i)
                               X_ov(i, a) = X_ov(i, a) - ONE / (Fnaa - Fnii - shift) &
                                     * DeltaFkia * TS(k)
                         end do
                   end do
                   !$omp end parallel do
             end do
       end subroutine uarh_XA


       subroutine uarh_X(XA_ov, XB_ov, sigma_X, FnA_ov, FnB_ov, FkA_ov, FkB_ov, DkA_ov, DkB_ov, &
             EigA, EigB, Tinv, NStored, shift, SpinUnres)

             real(F64), dimension(:, :), intent(out)   :: XA_ov
             real(F64), dimension(:, :), intent(out)   :: XB_ov
             real(F64), dimension(:), intent(out)      :: sigma_X
             real(F64), dimension(:, :), intent(in)    :: FnA_ov
             real(F64), dimension(:, :), intent(in)    :: FnB_ov
             real(F64), dimension(:, :, :), intent(in) :: FkA_ov
             real(F64), dimension(:, :, :), intent(in) :: FkB_ov
             real(F64), dimension(:, :, :), intent(in) :: DkA_ov
             real(F64), dimension(:, :, :), intent(in) :: DkB_ov
             real(F64), dimension(:), intent(in)       :: EigA
             real(F64), dimension(:), intent(in)       :: EigB
             real(F64), dimension(:, :), intent(in)    :: Tinv
             integer, intent(in)                       :: NStored
             real(F64), intent(in)                     :: shift
             logical, intent(in)                       :: SpinUnres

             real(F64), dimension(NStored, NStored) :: Omega
             
             call uarh_Omega(Omega, Tinv, FnA_ov, FnB_ov, FkA_ov, FkB_ov, DkA_ov, DkB_ov, &
                   EigA, EigB, NStored, shift, SpinUnres)
             sigma_X = ZERO
             call uarh_sigma_YA(sigma_X, FnA_ov, EigA, DkA_ov, NStored, shift)
             if (SpinUnres) then
                   call uarh_sigma_YA(sigma_X, FnB_ov, EigB, DkB_ov, NStored, shift)
             end if
             !
             ! Solve the linar system Omega sigma(X) = sigma(Y)
             !
             call linear_system(sigma_X, Omega)
             call uarh_XA(XA_ov, FnA_ov, FkA_ov, EigA, sigma_X, Tinv, NStored, shift)
             if (SpinUnres) then
                   call uarh_XA(XB_ov, FnB_ov, FkB_ov, EigB, sigma_X, Tinv, NStored, shift)
             end if
       end subroutine uarh_X
       

       subroutine uarh_YA(Y_ov, Fn_ov, eig, shift)
             real(F64), dimension(:, :), intent(out) :: Y_ov
             real(F64), dimension(:, :), intent(in)  :: Fn_ov
             real(F64), dimension(:), intent(in)     :: eig
             real(F64), intent(in)                   :: shift

             integer :: i, a
             integer :: Nocc, Nvirt

             Nocc = size(Y_ov, dim=1)
             Nvirt = size(Y_ov, dim=2)
             !$omp parallel do private(a, i) collapse(2)
             do a = 1, Nvirt
                   do i = 1, Nocc
                         Y_ov(i, a) = -Fn_ov(i, a) / (eig(Nocc+a) - eig(i) - shift)
                   end do
             end do
             !$omp end parallel do
       end subroutine uarh_YA

       
       subroutine uarh_Y(YA_ov, YB_ov, FnA_ov, FnB_ov, EigA, EigB, shift, SpinUnres)
             real(F64), dimension(:, :), intent(out) :: YA_ov
             real(F64), dimension(:, :), intent(out) :: YB_ov
             real(F64), dimension(:, :), intent(in)  :: FnA_ov
             real(F64), dimension(:, :), intent(in)  :: FnB_ov
             real(F64), dimension(:), intent(in)     :: EigA
             real(F64), dimension(:), intent(in)     :: EigB
             real(F64), intent(in)                   :: shift
             logical, intent(in)                     :: SpinUnres

             call uarh_YA(YA_ov, FnA_ov, EigA, shift)
             if (SpinUnres) then
                   call uarh_YA(YB_ov, FnB_ov, EigB, shift)
             end if
       end subroutine uarh_Y


       subroutine uarh_ovtrans(Fk_ov, Dk_ov, Fk_oao, Ck_oao, idx_map, C_ov, NStored)
             !
             ! Transform the D and F matrices from the previous iterations to the current-iteration
             ! occupied-virtual block-diagonalizaing basis. The new basis is spanned by the columns of Cov.
             !
             real(F64), dimension(:, :, :), intent(out) :: Fk_ov
             real(F64), dimension(:, :, :), intent(out) :: Dk_ov
             real(F64), dimension(:, :, :), intent(in)  :: Fk_oao
             real(F64), dimension(:, :, :), intent(in)  :: Ck_oao
             integer, dimension(:), intent(in)          :: idx_map
             real(F64), dimension(:, :), intent(in)     :: C_ov
             integer, intent(in)                        :: NStored

             integer :: k, kk
             integer :: i0, i1, a0, a1
             integer :: Nocc, Nvirt

             Nocc = size(Fk_ov, dim=1)
             Nvirt = size(Fk_ov, dim=2)
             i0 = 1
             i1 = Nocc
             a0 = Nocc + 1
             a1 = Nocc + Nvirt
             do k = 1, NStored
                   kk = idx_map(k)
                   call uarh_DensityMatrix(Dk_ov(:, :, k), Ck_oao(:, :, kk), C_ov)
                   call real_aTbc(Fk_ov(:, :, k), C_ov(:, i0:i1), Fk_oao(:, :, kk), C_ov(:, a0:a1))
             end do
       end subroutine uarh_ovtrans


       subroutine uarh_Transform_FD(Fk_ov, Dk_ov, Fk_oao_oao, Ck_oao_occ, IdxMap, Cn_oao_mo, NStored)
             !
             ! (1) Use stored occupied orbitals to generate density in the semicanonical basis.
             ! (2) Transform stored Fock matrices to the semicanonical basis.
             !
             real(F64), dimension(:, :, :), intent(out) :: Fk_ov
             real(F64), dimension(:, :, :), intent(out) :: Dk_ov
             real(F64), dimension(:, :, :), intent(in)  :: Fk_oao_oao
             real(F64), dimension(:, :, :), intent(in)  :: Ck_oao_occ
             integer, dimension(:), intent(in)          :: IdxMap
             real(F64), dimension(:, :), intent(in)     :: Cn_oao_mo
             integer, intent(in)                        :: NStored

             integer :: NOAO, NOcc, NVirt
             integer :: i0, i1, a0, a1
             integer :: k
             real(F64), dimension(:, :), allocatable :: W_occ_mo
             
             NOAO = size(Ck_oao_occ, dim=1)
             NOcc = size(Ck_oao_occ, dim=2)
             NVirt = size(Dk_ov, dim=2)
             i0 = 1
             i1 = Nocc
             a0 = Nocc + 1
             a1 = Nocc + Nvirt
             allocate(W_occ_mo(NOcc, NOAO))
             do k = 1, NStored
                   call real_aTb(W_occ_mo, Ck_oao_occ(:, :, IdxMap(k)), Cn_oao_mo)
                   call real_aTb(Dk_ov(:, :, k), W_occ_mo(:, i0:i1), W_occ_mo(:, a0:a1))
                   call real_aTb(W_occ_mo, Cn_oao_mo(:, i0:i1), Fk_oao_oao(:, :, IdxMap(k)))
                   call real_ab(Fk_ov(:, :, k), W_Occ_mo, Cn_oao_mo(:, a0:a1))
             end do
       end subroutine uarh_Transform_FD
       

       subroutine uarh_stable_invert(Tinv, T)
             !
             ! Invert the matrix T. Use symmetric diagonalization to get good
             ! numerical stability. (The numerical stability is an issue when
             ! stored density matrices result from small steps.)
             !
             real(F64), dimension(:, :), intent(out)   :: Tinv
             real(F64), dimension(:, :), intent(in)    :: T

             integer :: i, j, n

             n = size(Tinv, dim=1)
             do j = 1, n
                   do i = j, n
                         Tinv(i, j) = T(i, j)
                         Tinv(j, i) = T(i, j)
                   end do
             end do
             call real_invert_robust(Tinv)
       end subroutine uarh_stable_invert
       

       subroutine uarh_T(T, G)
             real(F64), dimension(:, :), intent(out) :: T
             real(F64), dimension(:, :), intent(in)  :: G

             integer :: k, l, NStored

             NStored = size(T, dim=1)
             !
             ! Tkl = Sum(PQ) (DkPQ - DnPQ) * (DlPQ - DnPQ) = Gkl + Gnn - Gnl - Gnk
             ! 
             do l = 1, NStored
                   do k = l, NStored
                         T(k, l) = G(k, l) + G(NStored+1, NStored+1) - G(NStored+1, l) - G(NStored+1, k)
                   end do
             end do
       end subroutine uarh_T

       
       subroutine uarh_Overlap(G, Ck_oao, Dn_oao, idx_map, NStored, Wk, Wl)
             real(F64), dimension(:, :), intent(inout) :: G
             real(F64), dimension(:, :, :), intent(in) :: Ck_oao
             real(F64), dimension(:, :), intent(in)    :: Dn_oao
             integer, dimension(:), intent(in)         :: idx_map
             integer, intent(in)                       :: NStored
             real(F64), dimension(:, :), intent(out)   :: Wk
             real(F64), dimension(:, :), intent(out)   :: Wl

             integer :: Noao, p, k, l, kk, ll
             real(F64) :: TrD
             real(F64) :: Skl
             
             Noao = size(Dn_oao, dim=1)
             TrD = ZERO
             do p = 1, Noao
                   TrD = TrD + Dn_oao(p, p)
             end do
             do l = 1, NStored+1
                   G(l, l) = G(l, l) + TrD
             end do
             do l = 1, NStored
                   ll = idx_map(l)
                   call real_abT(Wl, Ck_oao(:, :, ll), Ck_oao(:, :, ll))
                   do k = l+1, NStored
                         kk = idx_map(k)
                         call real_abT(Wk, Ck_oao(:, :, kk), Ck_oao(:, :, kk))
                         call real_vw_x(Skl, Wk, Wl, NOAO**2)
                         G(k, l) = G(k, l) + Skl
                   end do
                   call real_vw_x(Skl, Dn_oao, Wl, NOAO**2)
                   G(NStored+1, l) = G(NStored+1, l) + Skl
             end do
       end subroutine uarh_Overlap
       
       
       subroutine uarh_ov_basis(C, evals, Foao, Cocc, Cvirt)
             !
             ! Compute the basis in which the virtual-virtual and occupied-occupied blocks
             ! of the Hamiltonian are diagonal. Note that the occupied-virtual (virtual-occupied)
             ! blocks are nonzero until the convergence is reached.
             !
             real(F64), dimension(:, :), intent(out) :: C
             real(F64), dimension(:), intent(out)    :: evals
             real(F64), dimension(:, :), intent(in)  :: Foao
             real(F64), dimension(:, :), intent(in)  :: Cocc
             real(F64), dimension(:, :), intent(in)  :: Cvirt

             integer :: Nocc, Nvirt
             real(F64), dimension(:, :), allocatable :: Focc, Fvirt

             Nocc = size(Cocc, dim=2)
             Nvirt = size(Cvirt, dim=2)
             
             allocate(Focc(Nocc, Nocc))
             call real_aTbc(Focc, Cocc, Foao, Cocc)
             call symmetric_eigenproblem(evals(1:Nocc), Focc, Nocc, .true.)
             call real_ab(C(:, 1:Nocc), Cocc, Focc)
             deallocate(Focc)

             allocate(Fvirt(Nvirt, Nvirt))
             call real_aTbc(Fvirt, Cvirt, Foao, Cvirt)
             call symmetric_eigenproblem(evals(Nocc+1:Nocc+Nvirt), Fvirt, Nvirt, .true.)
             call real_ab(C(:, Nocc+1:Nocc+Nvirt), Cvirt, Fvirt)
             deallocate(Fvirt)
       end subroutine uarh_ov_basis


       subroutine uarh_DensityMatrix(D_ov, Cocc_oao, C_ov)
             !
             ! Dov <- Cocc(OV)**T (Cocc(OAO) Cocc(OAO)**T) Cvirt(OV)
             !
             real(F64), dimension(:, :), intent(out) :: D_ov
             real(F64), dimension(:, :), intent(in)  :: Cocc_oao
             real(F64), dimension(:, :), intent(in)  :: C_ov

             real(F64), dimension(:, :), allocatable :: W1, W2
             integer :: Nocc, Nvirt, Noao, i0, i1, a0, a1

             Noao = size(Cocc_oao, dim=1)
             Nocc = size(D_ov, dim=1)
             Nvirt = size(D_ov, dim=2)
             i0 = 1
             i1 = Nocc
             a0 = Nocc + 1
             a1 = Nocc + Nvirt
             allocate(W1(Nocc, Nocc))
             allocate(W2(Nocc, Nvirt))
             call real_aTb(W1, C_ov(:, i0:i1), Cocc_oao)
             call real_aTb(W2, Cocc_oao, C_ov(:, a0:a1))
             call real_ab(D_ov, W1, W2)
       end subroutine uarh_DensityMatrix


       subroutine uarh_NextIter(d, C_oao, OrbGradMax, OrbShift, NStored, Fn_oao, &
             EelK, RetryIter, time_ARH_Total, time_ARH_X, time_ARH_ExpX, &
             time_ARH_Equations, time_ARH_EnergyEstimate)
             !
             ! Driver subroutine for the spin-unrestricted SCF solver.
             !
             ! 1. Host, S. et al., The augmented Roothaan-Hall
             !    method for optimizing Hartree-Fock and Kohn-Sham
             !    density matrices,
             !    J. Chem. Phys. 129, 124106(2008)
             ! 2. Thogersen, L., Olsen, J., Kohn, A., Jorgensen, P.,
             !    Salek, P., Helgeker T., The trust-region self-
             !    consistent field method in Kohn-Sham density-
             !    functional theory,
             !    J. Chem. Phys. 123, 074103(2005)
             !
             type(uarhdata)                                           :: d
             real(F64), dimension(:, :, :), contiguous, intent(inout) :: C_oao
             real(F64), intent(out)                                   :: OrbGradMax
             real(F64), intent(out)                                   :: OrbShift
             integer, intent(out)                                     :: NStored
             real(F64), dimension(:, :, :), contiguous, intent(in)    :: Fn_oao
             real(F64), intent(in)                                    :: EelK
             logical, intent(in)                                      :: RetryIter
             real(F64), intent(inout)                                 :: time_ARH_Total
             real(F64), intent(inout)                                 :: time_ARH_X
             real(F64), intent(inout)                                 :: time_ARH_ExpX
             real(F64), intent(inout)                                 :: time_ARH_Equations
             real(F64), intent(inout)                                 :: time_ARH_EnergyEstimate

             integer :: k
             logical :: SecondOrder
             logical :: lindep
             integer :: Noao, NoccA, NoccB, NvirtA, NvirtB
             real(F64) :: TargetNorm
             real(F64) :: shift_maxnorm
             real(F64) :: shift0, shift1
             real(F64), parameter :: eps = 1.0E-3_F64
             real(F64) :: GapA, GapB, gap
             real(F64) :: Xnorm0, Xnorm1, Xnorm_mid
             real(F64) :: MaxOverlapA, MaxOverlapB
             real(F64), dimension(:, :), allocatable :: X_oao, expX_oao
             real(F64), dimension(:, :), allocatable :: Wpq, Wpi
             real(F64), dimension(:, :), allocatable :: CA_ov, CB_ov, XA_ov, XB_ov, FnA_ov, FnB_ov
             real(F64), dimension(:, :), allocatable :: Tinv, T, GramMatrix
             real(F64), dimension(:), allocatable :: sigma_X
             real(F64), dimension(:, :, :), allocatable :: FkA_ov, FkB_ov, DkA_ov, DkB_ov, Dn_oao
             real(F64), dimension(:), allocatable :: EigA, EigB
             real(F64) :: Emodel, EmodelA, EmodelB
             integer, dimension(:), allocatable :: del_list
             integer :: i0A, i1A, a0A, a1A
             integer :: i0B, i1B, a0B, a1B
             logical :: converged
             type(TClock) :: timer_ARH_Total, timer_ARH_ExpX, timer_ARH_X, timer_ARH_Equations
             type(TClock) :: timer_ARH_EnergyEstimate
             
             call clock_start(timer_ARH_total)
             NStored = d%NStored
             associate (shift => d%shift, norm_type => d%norm_type, Xnorm => d%Xnorm, enable_shift => d%enable_shift, &
                   trust_radius => d%trust_radius, OccRangeA => d%OccRangeA, OccRangeB => d%OccRangeB, &
                   VirtRangeA => d%VirtRangeA, VirtRangeB => d%VirtRangeB, Fk_oao => d%Fk_oao, &
                   CkA_oao => d%CkA_oao, CkB_oao => d%CkB_oao, idx_map => d%idx_map, Epred => d%Epred, &
                   OccNumber => d%OccNumber, SpinUnres => d%SpinUnres)

                   if (RetryIter) then
                         !
                         ! Delete the newest density matrix if this iteration is redone.
                         ! Otherwise, Dn_oao would be exactly equal to Dk_oao(NStored),
                         ! which would hamper the inversion of the T matrix.
                         !
                         call uarh_del(idx_map, NStored, NStored)
                         if (NStored > 0) NStored = NStored - 1
                   end if
                   Noao = size(Fn_oao, dim=1)
                   i0A = OccRangeA(1)
                   i1A = OccRangeA(2)
                   a0A = VirtRangeA(1)
                   a1A = VirtRangeA(2)
                   i0B = OccRangeB(1)
                   i1B = OccRangeB(2)
                   a0B = VirtRangeB(1)
                   a1B = VirtRangeB(2)
                   NoccA = i1A - i0A + 1
                   NvirtA = a1A - a0A + 1
                   NoccB = i1B - i0B + 1
                   NvirtB = a1B - a0B + 1
                   allocate(XA_ov(NoccA, NvirtA))
                   allocate(CA_ov(Noao, NoccA+NvirtA))
                   allocate(FnA_ov(NoccA, NvirtA))
                   allocate(FkA_ov(NoccA, NvirtA, NStored))
                   allocate(DkA_ov(NoccA, NvirtA, NStored))
                   allocate(EigA(NoccA+NvirtA))
                   if (SpinUnres) then
                         allocate(XB_ov(NoccB, NvirtB))
                         allocate(CB_ov(Noao, NoccB+NvirtB))
                         allocate(FnB_ov(NoccB, NvirtB))
                         allocate(FkB_ov(NoccB, NvirtB, NStored))
                         allocate(DkB_ov(NoccB, NvirtB, NStored))
                         allocate(EigB(NoccB+NvirtB))
                         allocate(Dn_oao(Noao, Noao, 2))
                   else
                         allocate(XB_ov(0, 0))
                         allocate(CB_ov(0, 0))
                         allocate(FnB_ov(0, 0))
                         allocate(FkB_ov(0, 0, NStored))
                         allocate(DkB_ov(0, 0, NStored))
                         allocate(EigB(0))
                         allocate(Dn_oao(Noao, Noao, 1))
                   end if
                   allocate(X_oao(Noao, Noao))
                   allocate(expX_oao(Noao, Noao))
                   allocate(Wpq(NOAO, NOAO))
                   allocate(Wpi(NOAO, max(NOccA, NOccB)))
                   allocate(Tinv(NStored, NStored))
                   allocate(T(NStored, NStored))
                   allocate(GramMatrix(NStored+1, NStored+1))
                   allocate(sigma_X(NStored))
                   !
                   ! Check if MO vectors are orthogonal. Perform
                   ! orthogonalization if orthogonality check 
                   ! fails
                   !
                   call real_conditional_gramschmidt(.true., C_oao(:, :, 1), UARH_ORTH_THRESH, MaxOverlapA)
                   if (SpinUnres) then
                         call real_conditional_gramschmidt(.true., C_oao(:, :, 2), UARH_ORTH_THRESH, MaxOverlapB)
                   else
                         MaxOverlapB = ZERO
                   end if
                   if (max(MaxOverlapA, MaxOverlapB) > UARH_ORTH_THRESH) then
                         call msg("MOs were orthonormalized by the ARH subroutine")
                         call msg("Max off-diagonal overlap |Sij| = " // str(max(MaxOverlapA, MaxOverlapB), 1))
                   end if
                   !
                   ! Compute the occupied-virtual block-diagonalizing basis, i.e., the basis
                   ! in which the occupied-occupied and virtual-virtual blocks of F are diagonal.
                   !
                   call clock_start(timer_ARH_Equations)
                   call uarh_ov_basis(CA_ov, EigA, Fn_oao(:, :, 1), C_oao(:, i0A:i1A, 1), C_oao(:, a0A:a1A, 1))
                   call real_aTbc(FnA_ov, CA_ov(:, 1:NoccA), Fn_oao(:, :, 1), CA_ov(:, NoccA+1:))
                   call real_abT(Dn_oao(:, :, 1), C_oao(:, i0A:i1A, 1), C_oao(:, i0A:i1A, 1))
                   !
                   ! Compute the max norm of the orbital gradient matrix (convergence measure)
                   !
                   OrbGradMax = uarh_maxnorm(FnA_ov)
                   if (SpinUnres) then
                         call uarh_ov_basis(CB_ov, EigB, Fn_oao(:, :, 2), C_oao(:, i0B:i1B, 2), C_oao(:, a0B:a1B, 2))
                         call real_aTbc(FnB_ov, CB_ov(:, 1:NoccB), Fn_oao(:, :, 2), CB_ov(:, NoccB+1:))
                         call real_abT(Dn_oao(:, :, 2), C_oao(:, i0B:i1B, 2), C_oao(:, i0B:i1B, 2))
                         OrbGradMax = max(OrbGradMax, uarh_maxnorm(FnB_ov))
                   end if
                   if (NStored > 0) then
                         associate (Wk => expX_oao, Wl => Wpq)
                               GramMatrix = ZERO
                               call uarh_Overlap(GramMatrix, CkA_oao, Dn_oao(:, :, 1), idx_map, NStored, Wk, Wl)
                               if (SpinUnres) then
                                     call uarh_Overlap(GramMatrix, CkB_oao, Dn_oao(:, :, 2), idx_map, NStored, Wk, Wl)
                               end if
                         end associate
                   end if                   
                   if (NStored >= UARH_SECOND_ORDER) then
                         SecondOrder = .true.
                         call uarh_T(T, GramMatrix)
                         call uarh_stable_invert(Tinv, T)
                         call uarh_ovtrans(FkA_ov, DkA_ov, Fk_oao(:, :, :, 1), CkA_oao, idx_map, CA_ov, NStored)
                         if (SpinUnres) then
                               call uarh_ovtrans(FkB_ov, DkB_ov, Fk_oao(:, :, :, 2), CkB_oao, idx_map, CB_ov, NStored)
                         end if
                   else
                         SecondOrder = .false.
                   end if
                   time_ARH_Equations = time_ARH_Equations + clock_readwall(timer_ARH_Equations)
100                continue
                   !
                   ! Check if the Hessian is positive definite.
                   ! See eqs. 29b, 36a in [2]
                   !
                   GapA = EigA(NoccA+1) - EigA(NoccA)
                   if (SpinUnres) then
                         GapB = EigB(NoccB+1) - EigB(NoccB)
                         gap = min(GapA, GapB)
                   else
                         gap = GapA
                   end if
                   if (gap < UARH_MINGAP) then
                         shift_maxnorm = gap - UARH_MINGAP
                   else
                         shift_maxnorm = ZERO
                   end if
                   shift = shift_maxnorm
                   call clock_start(timer_ARH_X)
                   if (SecondOrder) then
                         call uarh_X(XA_ov, XB_ov, sigma_X, FnA_ov, FnB_ov, FkA_ov, FkB_ov, DkA_ov, DkB_ov, &
                               EigA, EigB, Tinv, NStored, shift, SpinUnres)
                   else
                         call uarh_Y(XA_ov, XB_ov, FnA_ov, FnB_ov, EigA, EigB, shift, SpinUnres)
                         sigma_X = ZERO
                   end if
                   time_ARH_X = time_ARH_X + clock_readwall(timer_ARH_X)
                   shift0 = shift
                   Xnorm0 = uarh_matrixnorm(XA_ov, XB_ov, norm_type, SpinUnres)
                   !
                   ! Reduce trust radius if ||X_ov|| << trust_radius
                   ! See section C in [1]
                   !
                   if (Xnorm0 * TWO < trust_radius) trust_radius = trust_radius * 0.7_F64
                   !
                   ! Use the bisection method to adjust the orbital energy shift
                   ! so that the norm ||X_ov|| is equal to the trust radius
                   !
                   if (Xnorm0 > trust_radius .and. enable_shift) then
                         call clock_start(timer_ARH_X)
                         !
                         ! Find X_ov such that ||X_ov|| < trust_radius
                         !
                         TargetNorm = trust_radius
                         call uarh_find_smaller(XA_ov, XB_ov, sigma_X, shift1, Xnorm1, FnA_ov, FnB_ov, &
                               FkA_ov, FkB_ov, DkA_ov, DkB_ov, EigA, EigB, Tinv, NStored, TargetNorm, norm_type, &
                               gap, SecondOrder, SpinUnres)
                         converged = .false.
                         bisection: do k = 1, 50
                               Xnorm_mid = (Xnorm0 + Xnorm1) / TWO
                               if (abs(Xnorm0 - Xnorm1) < abs(Xnorm_mid * eps)) then
                                     converged = .true.
                                     exit bisection
                               end if
                               shift = (shift0 + shift1) / TWO
                               if (SecondOrder) then
                                     call uarh_X(XA_ov, XB_ov, sigma_X, FnA_ov, FnB_ov, FkA_ov, FkB_ov, DkA_ov, DkB_ov, &
                                           EigA, EigB, Tinv, NStored, shift, SpinUnres)
                               else
                                     call uarh_Y(XA_ov, XB_ov, FnA_ov, FnB_ov, EigA, EigB, shift, SpinUnres)
                                     sigma_X = ZERO
                               end if
                               Xnorm = uarh_matrixnorm(XA_ov, XB_ov, norm_type, SpinUnres)
                               if (Xnorm > TargetNorm) then
                                     shift0 = shift
                                     Xnorm0 = Xnorm
                               else
                                     shift1 = shift
                                     Xnorm1 = Xnorm
                               end if
                         end do bisection
                         time_ARH_X = time_ARH_X + clock_readwall(timer_ARH_X)
                         if (.not. converged) then
                               call msg("SCF solver: failed to solve ||X(shift)|| = trust_radius", MSG_WARNING)
                         end if
                   end if
                   !
                   ! Check if ||X_ov|| is large enough to switch the norm type to Frobenius
                   !
                   Xnorm = uarh_matrixnorm(XA_ov, XB_ov, norm_type, SpinUnres)
                   if (Xnorm < UARH_FROBENIUS .and. norm_type == UARH_NORMTYP_MAX) then
                         norm_type = UARH_NORMTYP_FROB
                         Xnorm = uarh_matrixnorm(XA_ov, XB_ov, norm_type, SpinUnres)
                         !
                         ! Update the trust radius to ||X_ov||
                         !
                         trust_radius = Xnorm
                   end if
                   !
                   ! 1. Compute the predicted total energy after the orbital rotation.
                   ! 2. The predicted energy should be lower than the previous-iteration
                   !    energy. If it's not, the Hessian is projected onto an insufficient
                   !    number of matrices. In that case, recompute X_ov using simpler,
                   !    first-order method.
                   !
                   call clock_start(timer_ARH_EnergyEstimate)
                   call uarh_EnergyModel(EmodelA, XA_ov, sigma_X, FnA_ov, FkA_ov, Tinv, EigA, &
                         NStored, OccNumber, SecondOrder)
                   if (SpinUnres) then
                         call uarh_EnergyModel(EmodelB, XB_ov, sigma_X, FnB_ov, FkB_ov, Tinv, EigB, &
                               NStored, OccNumber, SecondOrder)
                   else
                         EmodelB = ZERO
                   end if
                   Emodel = EmodelA + EmodelB
                   Epred = EelK + Emodel
                   time_ARH_EnergyEstimate = time_ARH_EnergyEstimate + clock_readwall(timer_ARH_EnergyEstimate)
                   if (((Epred - EelK) > ZERO) .and. SecondOrder .and. enable_shift) then
                         SecondOrder = .false.
                         goto 100
                   end if
                   OrbShift = shift
                   !
                   ! Check if linear dependencies arise when adding Dn to the set of density matrices
                   !
                   if (NStored > 0) then
                         call uarh_islindep(lindep, del_list, GramMatrix)
                         if (lindep) then
                               do k = 1, size(del_list)
                                     !
                                     ! DEL_LIST is a list of indices sorted in decreasing order
                                     !
                                     call uarh_del(idx_map, del_list(k), NStored)
                                     if (NStored > 0) NStored = NStored - 1
                               end do
                         end if
                   end if
                   !
                   ! Store new pair of density and KS matrices
                   !
                   if (SpinUnres) then
                         call uarh_store(Fk_oao, CkA_oao, CkB_oao, idx_map, NStored, &
                               Fn_oao, C_oao(:, i0A:i1A, 1), C_oao(:, i0B:i1B, 2), SpinUnres)
                   else
                         call uarh_store(Fk_oao, CkA_oao, CkB_oao, idx_map, NStored, &
                               Fn_oao, C_oao(:, i0A:i1A, 1), C_oao(:, i0A:i1A, 1), SpinUnres)
                   end if
                   !
                   ! Compute the orbital rotation matrix X in the orthogonalized
                   ! atomic orbital basis:
                   ! Xoao = Cocc X Cvirt**H - Cvirt X Xocc**H
                   !
                   call clock_start(timer_ARH_ExpX)
                   call uarh_ov2oao(X_oao, Wpi(:, 1:NOccA), XA_ov, &
                         CA_ov(:, 1:NoccA), CA_ov(:, NoccA+1:NOccA+NVirtA), &
                         NOccA, NVirtA, NOAO)
                   call matrix_exponential_real(expX_oao, X_oao, Wpq)
                   !
                   ! Update molecular orbitals: C <- exp(X**H) C
                   !
                   call real_aTb(Wpq, expX_oao, C_oao(:, :, 1))
                   C_oao(:, :, 1) = Wpq
                   if (SpinUnres) then
                         if (NOccB > 0) then
                               call uarh_ov2oao(X_oao, Wpi(:, 1:NOccB), XB_ov, &
                                     CB_ov(:, 1:NoccB), CB_ov(:, NoccB+1:NOccB+NVirtB), &
                                     NOccB, NVirtB, NOAO)
                               call matrix_exponential_real(expX_oao, X_oao, Wpq)
                               call real_aTb(Wpq, expX_oao, C_oao(:, :, 2))
                               C_oao(:, :, 2) = Wpq
                         else
                               C_oao(:, :, 2) = ZERO
                         end if
                   end if
                   time_ARH_ExpX = time_ARH_ExpX + clock_readwall(timer_ARH_ExpX)
             end associate
             d%NStored = NStored
             time_ARH_Total = time_ARH_Total + clock_readwall(timer_ARH_Total)
       end subroutine uarh_NextIter
 end module uks_arh
