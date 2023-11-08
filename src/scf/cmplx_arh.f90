! -----------------------------------------------------------------------
!        AUGMENTED ROOTHAN-HALL SCF SOLVER FOR COMPLEX ORBITALS
! -----------------------------------------------------------------------
! Use this module for complex-orbital restricted/unrestricted SCF
! computations. The distinction between the restricted and unrestricted
! cases is done according to the value of OCC_NUMBER. 
!
module cmplx_arh
      use arithmetic
      use math_constants
      use gparam
      use display
      use linalg
      use cmplx_linalg
      use real_linalg
      use matexp

      implicit none
      !
      ! Maximum number of stored density matrices
      !
      integer, parameter            :: CARH_MAX_NSTOR     = 8
      !
      ! First-iteration value of trust radius (X matrix norm)
      !
      real(F64), parameter          :: CARH_START_TRUST   = 0.5d+0
      !
      ! Threshold value at which the norm of X is switched from max to Frobenius
      !
      real(F64), parameter          :: CARH_FROBENIUS     = 0.05d+0
      !
      ! Number of stored density matrices at which the second-order algorithm
      ! is enabled
      !
      integer, parameter            :: CARH_SECOND_ORDER  = 2
      real(F64), parameter          :: CARH_LINDEP_THRESH = 1.0E-7_F64
      !
      ! Value of the occupied-virtual energy gap which enables energy shift
      !
      real(F64), parameter          :: CARH_MINGAP        = 1.0E-2_F64
      !
      ! Threshold for linear dependencies among the density
      ! matrices stored in memory
      !
      real(F64), parameter          :: CARH_ORTH_THRESH = SCF_ORTH_THRESH
      integer, parameter            :: CARH_NORMTYP_FROB = 0
      integer, parameter            :: CARH_NORMTYP_MAX  = 1

      type carhdata
            !
            ! Occupation number. dE/dDpq = occ_number*Fpq
            !
            real(F64) :: occ_number
            !
            ! Definition of the occupied and virtual orbitals which are active
            ! during the optimization
            ! Occupied index (i): i in [OCC0, OCC1]
            ! Virtual index (a):  a in [VIRT0, VIRT1]
            !
            integer, dimension(2) :: occ_range
            integer, dimension(2) :: virt_range
            complex(F64), dimension(:, :, :), allocatable :: Fk_oao
            complex(F64), dimension(:, :, :), allocatable :: Dk_oao
            integer, dimension(CARH_MAX_NSTOR) :: idx_map
            integer :: NStored = 0
            real(F64) :: shift = ZERO
            logical :: enable_shift = .true.
            real(F64) :: trust_radius = CARH_START_TRUST
            real(F64) :: Epred 
            integer :: norm_type = CARH_NORMTYP_MAX
            real(F64) :: Xnorm
      end type carhdata

 contains

       subroutine carh_init(a, occ_number, occ_range, virt_range, Noao)
             type(carhdata), intent(out)       :: a
             real(F64)                         :: occ_number
             integer, dimension(2), intent(in) :: occ_range
             integer, dimension(2), intent(in) :: virt_range
             integer, intent(in)               :: Noao
             !
             ! Note that some of the fields of A reset to their default values
             ! as a result of the intent(out) declaration.
             !
             a%occ_range = occ_range
             a%virt_range = virt_range
             a%occ_number = occ_number
             allocate(a%Fk_oao(Noao, Noao, CARH_MAX_NSTOR))
             allocate(a%Dk_oao(Noao, Noao, CARH_MAX_NSTOR))
       end subroutine carh_init


       subroutine carh_free(a)
             type(carhdata), intent(out) :: a
       end subroutine carh_free


       subroutine carh_DisableShift(a)
             type(carhdata), intent(inout) :: a
             a%enable_shift = .false.
       end subroutine carh_DisableShift


       subroutine carh_TrustRadius(d, EelN, EelK)
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
             type(carhdata), intent(inout) :: d
             real(F64), intent(in)         :: EelN
             real(F64), intent(in)         :: EelK
             
             real(F64) :: r, denom
             logical :: small_denom
             real(F64) :: min_denom
             !
             ! Check the quality of energy estimate:
             ! Eqs. 50 & 51 in [1]
             !
             denom = d%Epred - EelK
             min_denom = abs(1.0E-15_F64 * EelK)
             if (abs(denom) < min_denom) then
                   small_denom = .true.                   
                   denom = sign(denom, min_denom)
             else
                   small_denom = .false.
             end if
             r = (EelN - EelK) / denom

             print *, "r = ", r

             
             !
             ! A higher the value of R means the ARH energy estimate
             ! is close to the true energy
             !
             if (r >= 0.75_F64) then
                   d%trust_radius = 1.2_F64 * d%trust_radius
             else if (r >= 0.25_F64) then
                   continue
             else if (r >= 0.00_F64) then
                   if (.not. small_denom) then
                         d%trust_radius = 0.7_F64 * d%trust_radius
                   end if
             else
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
       end subroutine carh_TrustRadius

       
       function carh_freeidx(idx_map, nstor)
             integer                           :: carh_freeidx
             integer, dimension(:), intent(in) :: idx_map
             integer, intent(in)               :: nstor
             
             integer :: i, j
             logical :: occupied
             
             carh_freeidx = -1
             if (nstor >= CARH_MAX_NSTOR) then
                   carh_freeidx = -1
             else if (nstor > 0 .and. nstor < CARH_MAX_NSTOR) then
                   iloop: do i = 1, CARH_MAX_NSTOR
                         occupied = .false.
                         jloop: do j = 1, nstor
                               if (idx_map(j) == i) then
                                     occupied = .true.
                                     exit jloop
                               end if
                         end do jloop

                         if (.not. occupied) then
                               carh_freeidx = i
                               exit iloop
                         end if
                   end do iloop
             else if (nstor == 0) then
                   carh_freeidx = 1
             end if
       end function carh_freeidx

       
       subroutine carh_store(Fk_oao, Dk_oao, idx_map, nstor, Fn_oao, Dn_oao)
             complex(F64), dimension(:, :, :), intent(inout) :: Fk_oao
             complex(F64), dimension(:, :, :), intent(inout) :: Dk_oao
             integer, dimension(:), intent(inout)            :: idx_map
             integer, intent(inout)                          :: nstor
             complex(F64), dimension(:, :), intent(in)       :: Fn_oao
             complex(F64), dimension(:, :), intent(in)       :: Dn_oao

             integer :: i, idx1

             if (nstor < CARH_MAX_NSTOR) then
                   i = carh_freeidx(idx_map, nstor)
                   Fk_oao(:, :, i) = Fn_oao
                   Dk_oao(:, :, i) = Dn_oao
                   idx_map(nstor + 1) = i
                   nstor = nstor + 1
             else
                   !
                   ! idx_map(1) is the index of the oldest stored pair (Fk, Dk),
                   ! idx_map(2) is the second oldest etc.
                   ! Overwrite the oldest pair with (Fn, Dn).
                   !
                   idx1 = idx_map(1)
                   Fk_oao(:, :, idx1) = Fn_oao
                   Dk_oao(:, :, idx1) = Dn_oao
                   do i = 1, CARH_MAX_NSTOR - 1
                         idx_map(i) = idx_map(i + 1)
                   end do
                   idx_map(CARH_MAX_NSTOR) = idx1
             end if
       end subroutine carh_store


       subroutine carh_del(idx_map, i, nstor)
             !
             ! Delete the i-th pair of matrices (Dk, Fk)
             ! i=1     Oldest pair
             ! i=nstor Newest pair
             !
             integer, dimension(:), intent(inout) :: idx_map
             integer, intent(in)                  :: i
             integer, intent(in)                  :: nstor

             integer :: k

             if (nstor < 0 .or. nstor > CARH_MAX_NSTOR) then
                   call msg("Invalid value of NSTOR on entry to CARH_DEL", MSG_ERROR)
                   stop
             end if

             if (i <= 0) then
                   call msg("Invalid value of I on entry to CARH_DEL (I <= 0)", MSG_ERROR)
                   stop
             end if

             if (i > nstor) then
                   call msg("Invalid value of I on entry to CARH_DEL (I > NSTOR)", MSG_ERROR)
                   stop
             end if
                   
             if (nstor > 0) then
                   do k = i, nstor - 1
                         idx_map(k) = idx_map(k + 1)
                   end do
             end if
       end subroutine carh_del


       function carh_DOverlap(Da, Db)
             real(F64) :: carh_DOverlap
             complex(F64), dimension(:, :), intent(in) :: Da
             complex(F64), dimension(:, :), intent(in) :: Db

             integer :: p, q, Noao
             real(F64) :: s

             Noao = size(Da, dim=1)
             s = ZERO
             do q = 1, Noao
                   s = s + real(Da(q, q)) * real(Db(q, q))
                   do p = q + 1, Noao
                         s = s + TWO * real(conjg(Da(p, q)) * Db(p, q))
                   end do
             end do
             carh_DOverlap = s
       end function carh_DOverlap
       
       
       subroutine carh_islindep(lindep, del_list, Dn_oao, Dk_oao, idx_map, nstor)
             !
             ! Test if the density matrix from the n-th iteration, Dn,
             ! is linearly independent Dk's from the previous iterations
             !
             logical, intent(out)                            :: lindep
             integer, dimension(:), allocatable, intent(out) :: del_list
             complex(F64), dimension(:, :), intent(in)       :: Dn_oao
             complex(F64), dimension(:, :, :), intent(in)    :: Dk_oao
             integer, dimension(:), intent(in)               :: idx_map
             integer, intent(in)                             :: nstor

             real(F64) :: mineig, s
             real(F64), dimension(:, :), allocatable :: overlap, ts
             real(F64), dimension(:), allocatable :: w
             integer :: nremoved
             integer :: k, l, kk, ll, m
             integer :: n

             if (nstor == 0) then
                   lindep = .false.
                   return
             end if
             n = nstor + 1
             allocate(overlap(n, n))
             allocate(ts(n, n))
             allocate(w(n))
             !
             ! Build the lower triangle of OVERLAP
             !
             overlap = ZERO
             do l = 1, n - 1
                   do k = l, n - 1
                         ll = idx_map(l)
                         kk = idx_map(k)
                         overlap(k, l) = carh_DOverlap(Dk_oao(:, :, kk), Dk_oao(:, :, ll))
                   end do
             end do
             do k = 1, n - 1
                   kk = idx_map(k)
                   overlap(n, k) = carh_DOverlap(Dn_oao, Dk_oao(:, :, kk))
             end do
             overlap(n, n) = carh_DOverlap(Dn_oao, Dn_oao)
             !
             ! Normalize the Gram matrix
             !
             s = overlap(n, n)
             overlap = overlap / s
             nremoved = 0
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
                   ts = overlap
                   call symmetric_eigenproblem(w, ts, n-nremoved, .false.)
                   !
                   ! Check if smallest eigenvalue of the overlap
                   ! matrix is below the linear dependency threshold
                   !
                   mineig = minval(w(1:n-nremoved))
                   if (mineig < CARH_LINDEP_THRESH) then
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
       end subroutine carh_islindep

       
       subroutine carh_ov2oao(X_oao, X_ov, C_occ, C_virt)
             ! 
             ! Transform X_ov from the occupied-virtual basis to the orthogonalized
             ! atomic orbitals basis. Take into account the virtual-occupied block
             ! of X_ov, which is not stored in memory.
             !
             ! X_oao = Sum_{ia} (Xia * Ci Ca**H - Conj(Xia) * C_a C_i**H)
             !
             real(F64), dimension(:, :, :), contiguous, intent(out) :: X_oao
             complex(F64), dimension(:, :), contiguous, intent(in)  :: X_ov
             complex(F64), dimension(:, :), contiguous, intent(in)  :: C_occ
             complex(F64), dimension(:, :), contiguous, intent(in)  :: C_virt

             integer :: i, a, p, q
             integer :: Nocc, Nvirt, Noao
             complex(F64) :: Xia, t1, t2

             Noao = size(X_oao, dim=1)
             Nocc = size(C_occ, dim=2)
             Nvirt = size(C_virt, dim=2)
             X_oao = ZERO
             do a = 1, Nvirt
                   do i = 1, Nocc
                         Xia = X_ov(i, a)
                         do q = 1, Noao
                               do p = 1, Noao
                                     t1 = Xia * C_occ(p, i) * conjg(C_virt(q, a))
                                     t2 = conjg(Xia) * C_virt(p, a) * conjg(C_occ(q, i))
                                     X_oao(p, q, 1) = X_oao(p, q, 1) + real(t1 - t2)
                                     X_oao(p, q, 2) = X_oao(p, q, 2) + aimag(t1 - t2)
                               end do
                         end do
                   end do
             end do
             do p = 1, Noao
                   X_oao(p, p, 1) = ZERO
                   X_oao(p, p, 2) = ZERO
             end do
       end subroutine carh_ov2oao
       

       subroutine carh_find_smaller(X_ov, sigma_X, shift, X_norm, Fn_ov, Fk_ov, Dk_ov, eig, Tinv, &
             nstor, targetnorm, norm_type, gap, second_order)

             complex(F64), dimension(:, :), intent(out)   :: X_ov
             real(F64), dimension(:), intent(out)         :: sigma_X
             real(F64), intent(out)                       :: shift
             real(F64), intent(out)                       :: X_norm
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             complex(F64), dimension(:, :, :), intent(in) :: Fk_ov
             complex(F64), dimension(:, :, :), intent(in) :: Dk_ov
             real(F64), dimension(:), intent(in)          :: eig
             real(F64), dimension(:, :), intent(in)       :: Tinv
             integer, intent(in)                          :: nstor
             real(F64), intent(in)                        :: targetnorm
             integer, intent(in)                          :: norm_type
             real(F64), intent(in)                        :: gap
             logical, intent(in)                          :: second_order

             integer :: k
             real(F64) :: Delta, shift0

             if (gap < ZERO .or. abs(gap) < CARH_MINGAP) then
                   Delta = -(ONE/TWO) * CARH_MINGAP
                   shift0 = gap + Delta
             else
                   Delta = -(ONE/TWO) * gap
                   shift0 = Delta
             end if

             shift = shift0

             do k = 1, 50
                   if (second_order) then
                         call carh_X(X_ov, sigma_X, Fn_ov, Fk_ov, Dk_ov, eig, Tinv, nstor, shift)
                   else
                         call carh_Y(X_ov, Fn_ov, eig, shift)
                         sigma_X = ZERO
                   end if
                   
                   X_norm = carh_matrixnorm(X_ov, norm_type)
                   if (X_norm < targetnorm) then
                         exit
                   else
                         Delta = Delta * TWO
                         shift = shift0 + Delta
                   end if
             end do
       end subroutine carh_find_smaller

       
       function carh_maxnorm(A_ov)
             !
             ! Compute the max norm of the matrix A:
             ! MaxNorm = Max(|Aij|)
             !
             real(F64) :: carh_maxnorm
             complex(F64), dimension(:, :), intent(in) :: A_ov

             integer :: Nocc, Nvirt
             integer :: i, a
             real(F64) :: sij, sij_max

             Nocc = size(A_ov, dim=1)
             Nvirt = size(A_ov, dim=2)
             sij_max = ZERO
             do a = 1, Nvirt
                   do i = 1, Nocc
                         sij = real(conjg(A_ov(i, a)) * A_ov(i, a))
                         sij_max = max(sij_max, sij)
                   end do
             end do
             carh_maxnorm = sqrt(sij_max)
       end function carh_maxnorm


       function carh_frobnorm(A_ov)
             !
             ! Compute the Frobenius norm of the matrix A:
             ! FrobNorm = Sqrt(Tr(A A**H)). The sum computed
             ! in this subroutine is multiplied by two to
             ! compensate for the virtual-occupied block which
             ! is not stored in memory.
             !
             real(F64) :: carh_frobnorm
             complex(F64), dimension(:, :), intent(in) :: A_ov

             integer :: Nocc, Nvirt
             integer :: i, a
             real(F64) :: sij, sij_sum

             Nocc = size(A_ov, dim=1)
             Nvirt = size(A_ov, dim=2)
             sij_sum = ZERO
             do a = 1, Nvirt
                   do i = 1, Nocc
                         sij = real(conjg(A_ov(i, a)) * A_ov(i, a))
                         sij_sum = sij_sum + sij
                   end do
             end do
             !
             ! Multiply by two to make up for the remaining
             ! virtal-occupied block
             !
             carh_frobnorm = sqrt(TWO * sij_sum)
       end function carh_frobnorm
       

       function carh_matrixnorm(X_ov, norm_type)
             real(F64)                                 :: carh_matrixnorm
             complex(F64), dimension(:, :), intent(in) :: X_ov
             integer, intent(in)                       :: norm_type

             if (norm_type == CARH_NORMTYP_MAX) then
                   carh_matrixnorm = carh_maxnorm(X_ov)
             else
                   carh_matrixnorm = carh_frobnorm(X_ov)
             end if
       end function carh_matrixnorm

       
       subroutine carh_model_energy(Emodel, X_ov, sigma_X, Fn_ov, Fk_ov, Tinv, eig, &
             nstor, w, second_order)
             
             real(F64), intent(out)                       :: Emodel
             complex(F64), dimension(:, :), intent(in)    :: X_ov
             real(F64), dimension(:), intent(in)          :: sigma_X
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             complex(F64), dimension(:, :, :), intent(in) :: Fk_ov
             real(F64), dimension(:, :), intent(in)       :: Tinv
             real(F64), dimension(:), intent(in)          :: eig
             integer, intent(in)                          :: nstor
             real(F64)                                    :: w
             logical, intent(in)                          :: second_order

             integer :: Nocc, Nvirt, k, l, i, a
             real(F64) :: e11, e21, e22, Fnii, Fnaa
             complex(F64) :: DeltaFkia

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             e11 = ZERO
             do a = 1, Nvirt
                   do i = 1, Nocc
                         e11 = e11 + TWO * real(X_ov(i, a) * Fn_ov(i, a))
                   end do
             end do
             e11 = w * e11

             e21 = ZERO
             do a = 1, Nvirt
                   do i = 1, Nocc
                         Fnaa = eig(Nocc+a)
                         Fnii = eig(i)
                         e21 = e21 + real(X_ov(i, a) * conjg(X_ov(i, a))) * (Fnaa - Fnii)
                   end do
             end do
             e21 = w * e21

             e22 = ZERO
             if (second_order) then
                   do l = 1, nstor
                         do k = 1, nstor
                               do a = 1, Nvirt
                                     do i = 1, Nocc
                                           DeltaFkia = Fk_ov(i, a, k) - Fn_ov(i, a)
                                           e22 = e22 + TWO * real(DeltaFkia * X_ov(i, a)) * Tinv(k, l) * sigma_X(l)
                                     end do
                               end do
                         end do
                   end do
                   e22 = (ONE/TWO) * w * e22
             end if
             Emodel = e11 + e21 + e22
       end subroutine carh_model_energy


       subroutine carh_Omega(Omega, Tinv, Fn_ov, Fk_ov, Dk_ov, eig, nstor, shift)
             real(F64), dimension(:, :), intent(out)      :: Omega
             real(F64), dimension(:, :), intent(in)       :: Tinv
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             complex(F64), dimension(:, :, :), intent(in) :: Fk_ov
             complex(F64), dimension(:, :, :), intent(in) :: Dk_ov
             real(F64), dimension(:), intent(in)          :: eig
             integer, intent(in)                          :: nstor
             real(F64), intent(in)                        :: shift

             integer :: k, l, m
             real(F64), dimension(:, :), allocatable :: tau

             allocate(tau(nstor, nstor))
             call carh_tau(tau, Fn_ov, Fk_ov, Dk_ov, eig, nstor, shift)
             do l = 1, nstor
                   do m = 1, nstor
                         if (m == l) then
                               Omega(m, l) = ONE
                         else
                               Omega(m, l) = ZERO
                         end if
                         do k = 1, nstor
                               Omega(m, l) = Omega(m, l) - tau(k, m) * Tinv(k, l)
                         end do
                   end do
             end do
       end subroutine carh_Omega
             

       subroutine carh_tau(tau, Fn_ov, Fk_ov, Dk_ov, eig, nstor, shift)
             real(F64), dimension(:, :), intent(out)      :: tau
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             complex(F64), dimension(:, :, :), intent(in) :: Fk_ov
             complex(F64), dimension(:, :, :), intent(in) :: Dk_ov
             real(F64), dimension(:), intent(in)          :: eig
             integer, intent(in)                          :: nstor
             real(F64), intent(in)                        :: shift

             complex(F64) :: DeltaFkia, Dmia
             real(F64) :: Fnaa, Fnii
             integer :: k, m, i, a, Nocc, Nvirt

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             tau = ZERO
             do m = 1, nstor
                   do k = 1, nstor
                         do a = 1, Nvirt
                               do i = 1, Nocc
                                     DeltaFkia = Fk_ov(i, a, k) - Fn_ov(i, a)
                                     Dmia = Dk_ov(i, a, m)
                                     Fnaa = eig(Nocc+a)
                                     Fnii = eig(i)
                                     tau(k, m) = tau(k, m) - TWO * real(DeltaFkia * Dmia) / (Fnaa - Fnii - shift)
                               end do
                         end do
                   end do
             end do
       end subroutine carh_tau
       

       subroutine carh_sigma_Y(sigma_Y, Fn_ov, eig, Dk_ov, nstor, shift)
             real(F64), dimension(:), intent(out)         :: sigma_Y
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             real(F64), dimension(:), intent(in)          :: eig
             complex(F64), dimension(:, :, :), intent(in) :: Dk_ov
             integer, intent(in)                          :: nstor
             real(F64), intent(in)                        :: shift

             complex(F64) :: Yia, Dkia
             integer :: k, i, a, Nocc, Nvirt

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             do k = 1, nstor
                   sigma_Y(k) = ZERO
                   do a = 1, Nvirt
                         do i = 1, Nocc
                               Dkia = Dk_ov(i, a, k)
                               Yia = carh_Yia(i, a, Fn_ov, eig, shift)
                               sigma_Y(k) = sigma_Y(k) + TWO * real(conjg(Dkia) * Yia)
                         end do
                   end do
             end do
       end subroutine carh_sigma_Y


       subroutine carh_X(X_ov, sigma_X, Fn_ov, Fk_ov, Dk_ov, eig, Tinv, nstor, shift)
             complex(F64), dimension(:, :), intent(out)   :: X_ov
             real(F64), dimension(:), intent(out)         :: sigma_X
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             complex(F64), dimension(:, :, :), intent(in) :: Fk_ov
             complex(F64), dimension(:, :, :), intent(in) :: Dk_ov
             real(F64), dimension(:), intent(in)          :: eig
             real(F64), dimension(:, :), intent(in)       :: Tinv
             integer, intent(in)                          :: nstor
             real(F64), intent(in)                        :: shift

             real(F64), dimension(nstor, nstor) :: Omega
             real(F64) :: Fnii, Fnaa
             complex(F64) :: DeltaFkia
             integer :: k, l, i, a, Nocc, Nvirt

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             call carh_Omega(Omega, Tinv, Fn_ov, Fk_ov, Dk_ov, eig, nstor, shift)
             call carh_sigma_Y(sigma_X, Fn_ov, eig, Dk_ov, nstor, shift)
             !
             ! Solve the linar system Omega sigma(X) = sigma(Y)
             !
             call linear_system(sigma_X, Omega)
             
             do a = 1, Nvirt
                   do i = 1, Nocc
                         X_ov(i, a) = carh_Yia(i, a, Fn_ov, eig, shift)
                   end do
             end do
             
             do l = 1, nstor
                   do k = 1, nstor
                         do a = 1, Nvirt
                               do i = 1, Nocc
                                     DeltaFkia = Fk_ov(i, a, k) - Fn_ov(i, a)
                                     Fnaa = eig(Nocc+a)
                                     Fnii = eig(i)
                                     X_ov(i, a) = X_ov(i, a) - ONE / (Fnaa - Fnii - shift) &
                                           * conjg(DeltaFkia) * Tinv(k, l) * sigma_X(l)
                               end do
                         end do
                   end do
             end do
       end subroutine carh_X
       

       function carh_Yia(i, a, Fn_ov, eig, shift)
             complex(F64)                              :: carh_Yia
             integer, intent(in)                       :: i
             integer, intent(in)                       :: a
             complex(F64), dimension(:, :), intent(in) :: Fn_ov
             real(F64), dimension(:), intent(in)       :: eig
             real(F64), intent(in)                     :: shift

             integer :: Nocc

             Nocc = size(Fn_ov, dim=1)
             carh_Yia = -conjg(Fn_ov(i, a)) / (eig(Nocc+a) - eig(i) - shift)
       end function carh_Yia

       
       subroutine carh_Y(Y_ov, Fn_ov, eig, shift)
             complex(F64), dimension(:, :), intent(out) :: Y_ov
             complex(F64), dimension(:, :), intent(in)  :: Fn_ov
             real(F64), dimension(:), intent(in)        :: eig
             real(F64), intent(in)                      :: shift

             integer :: i, a
             integer :: Nocc, Nvirt

             Nocc = size(Fn_ov, dim=1)
             Nvirt = size(Fn_ov, dim=2)
             do a = 1, Nvirt
                   do i = 1, Nocc
                         Y_ov(i, a) = carh_Yia(i, a, Fn_ov, eig, shift)
                   end do
             end do
       end subroutine carh_Y

       
       subroutine carh_ovtrans(Fk_ov, Dk_ov, Fk_oao, Dk_oao, idx_map, Cov, Nocc, Nvirt, nstor)
             !
             ! Transform the D and F matrices from the previous iterations to the current-iteration
             ! occupied-virtual block-diagonalizaing basis. The new basis is spanned by the columns of Cov.
             !
             complex(F64), dimension(:, :, :), intent(out) :: Fk_ov
             complex(F64), dimension(:, :, :), intent(out) :: Dk_ov
             complex(F64), dimension(:, :, :), intent(in)  :: Fk_oao
             complex(F64), dimension(:, :, :), intent(in)  :: Dk_oao
             integer, dimension(:), intent(in)             :: idx_map
             complex(F64), dimension(:, :), intent(in)     :: Cov
             integer, intent(in)                           :: Nocc
             integer, intent(in)                           :: Nvirt
             integer, intent(in)                           :: nstor

             integer :: k, kk
             integer :: i0, i1, a0, a1

             i0 = 1
             i1 = Nocc
             a0 = Nocc + 1
             a1 = Nocc + Nvirt
             do k = 1, nstor
                   kk = idx_map(k)
                   call cmplx_aHbc(Fk_ov(:, :, k), Cov(:, i0:i1), Fk_oao(:, :, kk), Cov(:, a0:a1))
                   call cmplx_aHbc(Dk_ov(:, :, k), Cov(:, i0:i1), Dk_oao(:, :, kk), Cov(:, a0:a1))
             end do
       end subroutine carh_ovtrans
       

       subroutine carh_stable_invert(Tinverse, T)
             !
             ! Invert the matrix Tkl. All matrices (output TINVERSE included)
             ! are asummed to have the leading dimension CARH_MAX_NSTOR. For
             ! good numerical stability, the inverse is computed by diagonalization.
             !
             real(F64), dimension(:, :), intent(out)   :: Tinverse
             real(F64), dimension(:, :), intent(inout) :: T

             integer :: lwork
             real(F64), dimension(:), allocatable :: work
             real(F64), dimension(1) :: work0, w0
             real(F64), dimension(:), allocatable :: w
             real(F64), dimension(:, :), allocatable :: work2
             integer :: info
             integer :: i, j, k, n

             external :: dsyev

             n = size(tinverse, dim=1)
             allocate(work2(n, n))
             allocate(w(n))
             !
             ! Symmetric matrix T is inverted using diagonalization
             ! subroutine because of its good numerical stability.
             !
             call dsyev("V", "L", n, Tinverse, n, w0, work0, -1, info)
             lwork = ceiling(work0(1))
             allocate(work(lwork))
             call dsyev("V", "L", n, T, n, w, work, lwork, info)
             do i = 1, n
                   work2(:, i) = t(:, i) / w(i)
             end do
             !
             ! Tinv <- C (1/w) C**T
             !
             do j = 1, n
                   do i = 1, n
                         Tinverse(i, j) = ZERO
                         do k = 1, n
                               Tinverse(i, j) = Tinverse(i, j) + work2(i, k) * T(j, k)
                         end do
                   end do
             end do
       end subroutine carh_stable_invert
       

       subroutine carh_Tinv(Tinv, Dk_oao, Dn_oao, idx_map, nstor)
             !
             ! Compute T^(-1), where T_{kl} is a dot product of density matrix differences:
             ! T_{kl} = Sum_{pq} Conjg(Dkn)_{pq} Dln_{pq},
             ! D_{kn} = D_k - D_n
             ! The matrices are not transformed to the basis spanned by the occupied-occupied
             ! and virtual-virtual eigenvectors because an orthogonal transformation does not
             ! change the dot product.
             ! 
             real(F64), dimension(:, :), intent(out)      :: Tinv
             complex(F64), dimension(:, :, :), intent(in) :: Dk_oao
             complex(F64), dimension(:, :), intent(in)    :: Dn_oao
             integer, dimension(:), intent(in)            :: idx_map
             integer, intent(in)                          :: nstor

             real(F64), dimension(:, :), allocatable :: T
             real(F64) :: tij
             complex(F64) :: a, b
             integer :: i, j
             integer :: ii, jj
             integer :: k, l
             integer :: Noao

             allocate(T(nstor, nstor))
             T = ZERO
             Noao = size(Dn_oao, dim=1)
             do j = 1, nstor
                   do i = j, nstor
                         ii = idx_map(i)
                         jj = idx_map(j)
                         tij = ZERO
                         associate (di => Dk_oao(:, :, ii), dj => Dk_oao(:, :, jj))
                               do l = 1, Noao
                                     !
                                     ! Diagonal contribution to matrix dot product
                                     !
                                     a = di(l, l) - Dn_oao(l, l)
                                     tij = tij + real(conjg(a) * a)
                                     !
                                     ! Off-diagonal contribution to matrix dot product
                                     !
                                     do k = l + 1, Noao
                                           a = di(k, l) - Dn_oao(k, l)
                                           b = dj(k, l) - Dn_oao(k, l)
                                           tij = tij + TWO * real(conjg(a) * b)
                                     end do
                               end do
                         end associate
                         T(i, j) = tij
                   end do
             end do
             call carh_stable_invert(Tinv, T)
       end subroutine carh_Tinv

       
       subroutine carh_ov_basis(C, evals, Foao, Cocc, Cvirt)
             !
             ! Compute the basis in which the virtual-virtual and occupied-occupied blocks
             ! of the Hamiltonian are diagonal. Note that the occupied-virtual (virtual-occupied)
             ! blocks are nonzero until the convergence is reached.
             !
             complex(F64), dimension(:, :), intent(out) :: C
             real(F64), dimension(:), intent(out)       :: evals
             complex(F64), dimension(:, :), intent(in)  :: Foao
             complex(F64), dimension(:, :), intent(in)  :: Cocc
             complex(F64), dimension(:, :), intent(in)  :: Cvirt

             integer :: Nocc, Nvirt
             complex(F64), dimension(:, :), allocatable :: Focc, Fvirt

             Nocc = size(Cocc, dim=2)
             Nvirt = size(Cvirt, dim=2)
             
             allocate(Focc(Nocc, Nocc))
             call cmplx_aHbc(Focc, Cocc, Foao, Cocc)
             call hermitian_eigenproblem(evals(1:Nocc), Focc, Nocc, .true.)
             call cmplx_ab(C(:, 1:Nocc), Cocc, Focc)
             deallocate(Focc)

             allocate(Fvirt(Nvirt, Nvirt))
             call cmplx_aHbc(Fvirt, Cvirt, Foao, Cvirt)
             call hermitian_eigenproblem(evals(Nocc+1:Nocc+Nvirt), Fvirt, Nvirt, .true.)
             call cmplx_ab(C(:, Nocc+1:Nocc+Nvirt), Cvirt, Fvirt)
             deallocate(Fvirt)
       end subroutine carh_ov_basis


       subroutine carh_expXH_C(C, exp_X, work)
             !
             ! C <- exp(X)**H C
             !
             complex(F64), dimension(:, :), intent(inout) :: C
             real(F64), dimension(:, :, :), intent(in)    :: exp_X
             real(F64), dimension(:, :, :), intent(out)   :: work

             integer :: p, q, k, Noao
             real(F64) :: a_re, a_im, b_re, b_im

             Noao = size(C, dim=1)
             work = ZERO
             do q = 1, Noao
                   do p = 1, Noao
                         do k = 1, Noao
                               a_re = exp_X(k, p, 1)
                               a_im = -exp_X(k, p, 2)
                               b_re = real(C(k, q))
                               b_im = aimag(C(k, q))
                               work(p, q, 1) = work(p, q, 1) + a_re * b_re - a_im * b_im
                               work(p, q, 2) = work(p, q, 2) + a_re * b_im + a_im * b_re
                         end do
                   end do
             end do

             do q = 1, Noao
                   do p = 1, Noao
                         C(p, q) = cmplx(work(p, q, 1), work(p, q, 2), kind=F64)
                   end do
             end do
       end subroutine carh_expXH_C


       subroutine carh_NextIter(d, C_oao, OrbGradMax, OrbShift, NStored, Fn_oao, EelK, RetryIter)
             !
             ! Main subroutine for the augmented Roothaan-Hall complex-orbital SCF solver.
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
             type(carhdata)                                           :: d
             complex(F64), dimension(:, :), contiguous, intent(inout) :: C_oao
             real(F64), intent(out)                                   :: OrbGradMax
             real(F64), intent(out)                                   :: OrbShift
             integer, intent(out)                                     :: NStored
             complex(F64), dimension(:, :), contiguous, intent(in)    :: Fn_oao
             real(F64), intent(in)                                    :: EelK
             logical, intent(in)                                      :: RetryIter

             integer :: k
             logical :: second_order
             logical :: lindep
             integer :: Noao, Nocc, Nvirt
             real(F64) :: targetnorm
             real(F64) :: shift_maxnorm
             real(F64) :: shift0, shift1
             real(F64), parameter :: eps = 1.0E-3_F64
             real(F64) :: gap
             real(F64) :: Xnorm0, Xnorm1, Xnorm_mid
             real(F64) :: max_overlap
             real(F64), dimension(:, :, :), allocatable :: X_oao, exp_X, exp_work
             complex(F64), dimension(:, :), allocatable :: C_ov, X_ov, Fn_ov, Dn_oao
             real(F64), dimension(:, :), allocatable :: Tinv
             real(F64), dimension(:), allocatable :: sigma_X
             complex(F64), dimension(:, :, :), allocatable :: Fk_ov, Dk_ov
             real(F64), dimension(:), allocatable :: eig
             real(F64) :: Emodel
             integer, dimension(:), allocatable :: del_list
             integer :: i0, i1, a0, a1
             logical :: converged

             NStored = d%NStored
             associate (shift => d%shift, norm_type => d%norm_type, Xnorm => d%Xnorm, enable_shift => d%enable_shift, &
                   trust_radius => d%trust_radius, occ_range => d%occ_range, virt_range => d%virt_range, Fk_oao => d%Fk_oao, &
                   Dk_oao => d%Dk_oao, idx_map => d%idx_map, Epred => d%Epred, occ_number => d%occ_number)

                   if (RetryIter) then
                         !
                         ! Delete the newest density matrix if this iteration is redone.
                         ! Otherwise, Dn_oao would be exactly equal to Dk_oao(NStored),
                         ! which would hamper the inversion of the T matrix.
                         !
                         call carh_del(idx_map, NStored, NStored)
                         if (NStored > 0) NStored = NStored - 1
                   end if
                   
                   Noao = size(Fn_oao, dim=1)
                   i0 = occ_range(1)
                   i1 = occ_range(2)
                   a0 = virt_range(1)
                   a1 = virt_range(2)
                   Nocc = i1 - i0 + 1
                   Nvirt = a1 - a0 + 1
                   allocate(X_oao(Noao, Noao, 2))
                   allocate(X_ov(Nocc, Nvirt))
                   allocate(exp_X(Noao, Noao, 2))
                   allocate(exp_work(Noao, Noao, 2))
                   allocate(C_ov(Noao, Nocc+Nvirt))
                   allocate(Dn_oao(Noao, Noao))
                   allocate(Fn_ov(Nocc, Nvirt))
                   allocate(Fk_ov(Nocc, Nvirt, NStored))
                   allocate(Dk_ov(Nocc, Nvirt, NStored))
                   allocate(eig(Nocc+Nvirt))
                   allocate(Tinv(NStored, NStored))
                   allocate(sigma_X(NStored))
                   !
                   ! Check if MO vectors are orthogonal. Perform
                   ! orthogonalization if orthogonality check 
                   ! fails
                   !
                   call cmplx_conditional_gramschmidt(.true., C_oao, CARH_ORTH_THRESH, max_overlap)
                   if (max_overlap > CARH_ORTH_THRESH) then
                         call msg("MOs were orthonormalized by the ARH subroutine")
                         call msg("Max off-diagonal overlap |Sij| = " // str(max_overlap, 1))
                   end if
                   !
                   ! Compute the occupied-virtual block-diagonalizing basis, i.e., the basis
                   ! in which the occupied-occupied and virtual-virtual blocks of F are diagonal.
                   !
                   call carh_ov_basis(C_ov, eig, Fn_oao, C_oao(:, i0:i1), C_oao(:, a0:a1))
                   call cmplx_abH(Dn_oao, C_oao(:, i0:i1), C_oao(:, i0:i1))
                   call cmplx_aHbc(Fn_ov, C_ov(:, 1:Nocc), Fn_oao, C_ov(:, Nocc+1:Nocc+Nvirt))
                   !
                   ! Compute the max norm of the orbital gradient matrix (convergence measure)
                   !
                   OrbGradMax = carh_maxnorm(Fn_ov)
                   if (NStored >= CARH_SECOND_ORDER) then
                         second_order = .true.
                         call carh_Tinv(Tinv, Dk_oao, Dn_oao, idx_map, NStored)
                         call carh_ovtrans(Fk_ov, Dk_ov, Fk_oao, Dk_oao, idx_map, C_ov, Nocc, Nvirt, NStored)
                   else
                         second_order = .false.
                   end if
100                continue
                   !
                   ! Check if the Hessian is positive definite.
                   ! See eqs. 29b, 36a in [2]
                   !
                   gap = eig(Nocc+1) - eig(Nocc)
                   if (gap < CARH_MINGAP) then
                         shift_maxnorm = gap - CARH_MINGAP
                   else
                         shift_maxnorm = ZERO
                   end if
                   shift = shift_maxnorm
                   if (second_order) then
                         call carh_X(X_ov, sigma_X, Fn_ov, Fk_ov, Dk_ov, eig, Tinv, NStored, shift)
                   else
                         call carh_Y(X_ov, Fn_ov, eig, shift)
                         sigma_X = ZERO
                   end if
                   shift0 = shift
                   Xnorm0 = carh_matrixnorm(X_ov, norm_type)
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
                         !
                         ! Find X_ov such that ||X_ov|| < trust_radius
                         !
                         targetnorm = trust_radius
                         call carh_find_smaller(X_ov, sigma_X, shift1, Xnorm1, Fn_ov, Fk_ov, Dk_ov, eig, Tinv, &
                               NStored, targetnorm, norm_type, gap, second_order)
                         converged = .false.
                         bisection: do k = 1, 50
                               Xnorm_mid = (Xnorm0 + Xnorm1) / TWO
                               if (abs(Xnorm0 - Xnorm1) < abs(Xnorm_mid * eps)) then
                                     converged = .true.
                                     exit bisection
                               end if
                               shift = (shift0 + shift1) / TWO
                               if (second_order) then
                                     call carh_X(X_ov, sigma_X, Fn_ov, Fk_ov, Dk_ov, eig, Tinv, NStored, shift)
                               else
                                     call carh_Y(X_ov, Fn_ov, eig, shift)
                                     sigma_X = ZERO
                               end if
                               Xnorm = carh_matrixnorm(X_ov, norm_type)
                               if (Xnorm > targetnorm) then
                                     shift0 = shift
                                     Xnorm0 = Xnorm
                               else
                                     shift1 = shift
                                     Xnorm1 = Xnorm
                               end if
                         end do bisection
                         if (.not. converged) then
                               call msg("Complex-orbital SCF solver: failed to solve ||X(shift)|| = trust_radius", MSG_WARNING)
                         end if
                   end if
                   !
                   ! Check if ||X_ov|| is large enough to switch the norm type to Frobenius
                   !
                   Xnorm = carh_matrixnorm(X_ov, norm_type)
                   if (Xnorm < CARH_FROBENIUS .and. norm_type == CARH_NORMTYP_MAX) then
                         norm_type = CARH_NORMTYP_FROB
                         Xnorm = carh_matrixnorm(X_ov, norm_type)
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
                   call carh_model_energy(Emodel, X_ov, sigma_X, Fn_ov, Fk_ov, Tinv, eig, &
                         NStored, occ_number, second_order)
                   Epred = EelK + Emodel
                   if (((Epred - EelK) > ZERO) .and. second_order .and. enable_shift) then
                         second_order = .false.
                         goto 100
                   end if
                   OrbShift = shift
                   !
                   ! Compute the orbital rotation matrix X in the orthogonalized
                   ! atomic orbital basis:
                   ! Xoao = Cocc X Cvirt**H - Cvirt X Xocc**H
                   !
                   call carh_ov2oao(X_oao, X_ov, C_ov(:, 1:Nocc), C_ov(:, Nocc+1:Nocc+Nvirt))
                   !
                   ! Compute exp(X)
                   !
                   call matrix_exponential_cplx(exp_X, X_oao, exp_work)
                   !
                   ! Update molecular orbitals: C <- exp(X**H) C
                   !
                   call carh_expXH_C(C_oao, exp_X, exp_work)
                   !
                   ! Check if linear dependencies arise when adding Dn to the set of density matrices
                   !
                   call carh_islindep(lindep, del_list, Dn_oao, Dk_oao, idx_map, NStored)
                   if (lindep) then
                         do k = 1, size(del_list)
                               !
                               ! DEL_LIST is a list of indices sorted in decreasing order
                               !
                               call carh_del(idx_map, del_list(k), NStored)
                               if (NStored > 0) NStored = NStored - 1
                         end do
                   end if
                   !
                   ! Store new pair of density and KS matrices
                   !
                   call carh_store(Fk_oao, Dk_oao, idx_map, NStored, Fn_oao, Dn_oao)
             end associate
             d%NStored = NStored
       end subroutine carh_NextIter


       subroutine test_solutions(X_ov, Fn_ov, eig, Tinv, Dk_ov, Fk_ov, NStored, shift)
             complex(F64), dimension(:, :), intent(in)    :: X_ov
             complex(F64), dimension(:, :), intent(in)    :: Fn_ov
             real(F64), dimension(:), intent(in)          :: eig
             real(F64), dimension(:, :), intent(in)       :: Tinv
             complex(F64), dimension(:, :, :), intent(in) :: Dk_ov
             complex(F64), dimension(:, :, :), intent(in) :: Fk_ov
             integer, intent(in)                          :: NStored
             real(F64), intent(in)                        :: shift

             integer :: i, a, l, k, Nocc, Nvirt
             real(F64), dimension(CARH_MAX_NSTOR) :: sigmaX
             complex(F64), dimension(:, :), allocatable :: Gradient
             complex(F64) :: t1, t2, t3

             Nocc = size(X_ov, dim=1)
             Nvirt = size(X_ov, dim=2)
             sigmaX = ZERO
             do l = 1, NStored
                   do a = 1, Nvirt
                         do i = 1, Nocc
                               sigmaX(l) = sigmaX(l) + TWO * real(conjg(Dk_ov(i, a, l)) * X_ov(i, a))
                         end do
                   end do
             end do
             allocate(Gradient(Nocc, Nvirt))
             Gradient = ZERO
             do a = 1, Nvirt
                   do i = 1, Nocc
                         t1 = Fn_ov(i, a)
                         t2 = conjg(X_ov(i, a)) * (eig(Nocc+a) - eig(i) - shift)
                         t3 = ZERO
                         do k = 1, NStored
                               do l = 1, NStored
                                     t3 = t3 + (Fk_ov(i, a, k) - Fn_ov(i, a)) * Tinv(k, l) * sigmaX(l)
                               end do
                         end do
                         Gradient(i, a) = t1 + t2 + t3
                   end do
             end do

             print *, "-------------------------------------------------------------------"
             call geprn(real(Gradient))
             print *, "-------------------------------------------------------------------"
       end subroutine test_solutions
end module cmplx_arh
