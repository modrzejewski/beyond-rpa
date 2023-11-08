module arh
      use gparam
      use arithmetic
      use math_constants
      use display
      use linalg
      use real_linalg
      use sort

      implicit none
      ! -----------------------------------------------------------------------
      ! AUGMENTED ROOTHAN-HALL SCF SOLVER MODIFIED/GENERALIZED BY 
      ! Modrzejewski, M., Rajchel, L., Szczesniak, M. M., Chalasinski, G.
      ! J. Chem. Phys. 136, 204109 (2012)
      ! -----------------------------------------------------------------------
      ! ARH_MAX_NSTOR     - Maximum number of stored density matrices
      !
      ! ARH_START_TRUST   - Start value of trust radius (X matrix norm)
      !
      ! ARH_MAX_GUESS     - Maximum value of trust radius
      !
      ! ARH_FROBENIUS     - Threshold value of X matrix norm to switch from 
      !                     max norm to the Frobenius matrix norm
      !
      ! ARH_TRIGGER       - Enable ARH if number of iterations >= ARH_TRIGGER, 
      !                     fall back to Roothaan-Hall otherwise
      !
      ! ARH_LINDEP_THRESH - Threshold for linear dependencies between stored
      !                     density matrices
      ! ARH_MINGAP        - The value of an occupied-virtual (core-valence) orbital
      !                     energy gap below which shifting is performed
      !
      integer, parameter                   :: ARH_MAX_NSTOR     = 8
      double precision, parameter          :: ARH_START_TRUST   = 0.5d+0
      double precision, parameter          :: ARH_FROBENIUS     = 0.05d+0
      integer, parameter                   :: ARH_TRIGGER       = 2
      double precision, parameter          :: ARH_LINDEP_THRESH = 1.0E-7_F64
      double precision, parameter          :: ARH_MINGAP        = 1.0E-2_F64
      real(F64), parameter                 :: ARH_ORTH_THRESH = SCF_ORTH_THRESH
      integer, parameter                   :: ARH_NORMTYP_FROB = 0
      integer, parameter                   :: ARH_NORMTYP_MAX  = 1

      type arhdata
            !
            ! Currently allowed occupation numbers
            ! --------------------------------------
            ! Closed-shell case:
            ! - OCCUPATION_NUMBER(1) = 2.d+0
            ! - OCCUPATION_NUMBER(2) not referenced
            ! Open-shell case:
            ! - OCCUPATION_NUMBER(1) = X, 0 < X <= 2.d+0
            ! - OCCUPATION_NUMBER(2) = Y, X <= Y <= 2.d+0
            ! One-electron or valence-only system:
            ! - OCCUPATION_NUMBER(1) = 1.d+0 or X, 0 < X <= 2.d+0
            ! - OCCUPATION_NUMBER(2) not referenced
            !
            double precision, dimension(2)                    :: occupation_number
            !
            ! Number of core / valence orbitals. In the open
            ! shell case, the occupation of
            ! core/valence orbitals is OCCUPATION_NUMBER(1) and
            ! OCCUPATION_NUMBER(2), respectively. In the closed
            ! shell case, doubly occupied orbitals are assumed.
            !
            integer                                           :: ncore
            integer                                           :: nvalence
            !
            ! Definition of occupied orbitals active
            ! during optimization and the space of virtual 
            ! orbitals.
            ! Occupied index (i): i in <OCC0, OCC1>
            ! Virtual index (a):  a in <VIRT0, VIRT1>
            !
            integer                                           :: occ0, nocc
            integer                                           :: virt0, nvirt
            !
            ! SIGMA intermediate
            !
            double precision, dimension(ARH_MAX_NSTOR)        :: sigma
            !
            ! T^{-1} intermediate
            !
            double precision, &
                  dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR)     :: tinverse

            double precision, dimension(:, :, :), allocatable :: fvec
            double precision, dimension(:, :, :), allocatable :: dvec
            integer, dimension(ARH_MAX_NSTOR)                 :: idx
            integer                                           :: nstor
            real(F64)                                         :: shift
            logical                                           :: enable_shift = .true.
            ! --------- Eigenvalue decomposition --------
            integer                                           :: lwork
            integer                                           :: liwork
            double precision, dimension(:), allocatable       :: work
            integer, dimension(:), allocatable                :: iwork
            ! - Antysymmetric matrix of rotation params -
            double precision, dimension(:, :), allocatable    :: xmo
            double precision, dimension(:, :), allocatable    :: xao
            ! --------------- Trust radius --------------
            double precision                                  :: h
            ! --------------- Working arrys -------------
            double precision, dimension(:, :), allocatable    :: vec
            double precision, dimension(:, :), allocatable    :: fov
            double precision, dimension(:, :, :), allocatable :: fkov
            double precision, dimension(:, :, :), allocatable :: dkov
            double precision, dimension(:), allocatable       :: eig
            double precision, dimension(:, :), allocatable    :: work2
            ! ----------------- Energy ------------------
            double precision                                  :: enew
            double precision                                  :: eold
            double precision                                  :: epred

            integer                                           :: normtyp
            double precision                                  :: xnorm
            !
            ! The Frobenius norm of the orbital gradient matrix:
            ! ||G|| = ||(F^{VO} - F^{OV})||.
            ! This quantity can be used as a measure of SCF convergence
            ! when divided by the square root of the number of electrons.
            ! (Recommended by Salek et al. J. Chem. Phys. 126, 114110 (2007);
            ! doi: 10.1063/1.2464111)
            !
            real(F64)                                         :: orb_grad_frob
            !
            ! The maximum norm of the orbital gradient matrix
            !
            real(F64)                                         :: orb_grad_max
      end type arhdata

 contains

       subroutine arhdata_init(a, occupation_number, ncore, &
             occ0, occ1, virt0, virt1)
             
             type(arhdata)                  :: a
             double precision, dimension(2) :: occupation_number
             integer, intent(in)            :: ncore
             integer, intent(in)            :: occ0, occ1
             integer, intent(in)            :: virt0, virt1

             integer                        :: nocc, nvirt, nvec
             integer                        :: xdim1, xdim2
             integer                        :: nvalence
             integer                        :: lwork, liwork
             integer                        :: info
             integer, dimension(1)          :: iw
             double precision, dimension(1) :: dw
             integer                        :: maxn

             external :: dsyevd

             nocc = occ1 - occ0 + 1
             nvirt = virt1 - virt0 + 1
             nvalence = nocc - ncore
             a%occ0 = occ0
             a%virt0 = virt0
             a%nocc  = nocc
             a%nvirt = nvirt

             a%ncore = ncore
             a%nvalence = nvalence
             a%occupation_number = occupation_number

             xdim1 = nocc
             xdim2 = nvalence + nvirt
             nvec = nocc + nvirt

             allocate(a%fvec(NORB, NORB, ARH_MAX_NSTOR))
             allocate(a%dvec(NORB, NORB, ARH_MAX_NSTOR))
             allocate(a%xmo(xdim1, xdim2))
             allocate(a%xao(NORB, NORB))
             allocate(a%fov(xdim1, xdim2))
             allocate(a%fkov(xdim1, xdim2, ARH_MAX_NSTOR))
             allocate(a%dkov(xdim1, xdim2, ARH_MAX_NSTOR))
             allocate(a%eig(NORB))
             allocate(a%vec(NORB, nvec))
             allocate(a%work2(NORB, NORB))

             a%nstor = 0
             a%shift = ZERO
             !
             ! Query eigenvalue decomposition driver
             ! Allocate working arrays for decomposition
             ! of occupied-occupied or virtual-virtual blocks
             ! of Fock matrix
             !
             maxn = max(ncore, nvalence, nvirt)
             lwork = -1
             liwork = -1
             call dsyevd("V", "L", maxn, dw, maxn, dw, dw, lwork, iw, liwork, info)
             lwork = ceiling(dw(1))
             liwork = iw(1)
             allocate(a%work(lwork))
             allocate(a%iwork(liwork))
             a%lwork = lwork
             a%liwork = liwork
             a%h = ARH_START_TRUST
             a%normtyp = ARH_NORMTYP_MAX
       end subroutine arhdata_init


       subroutine arhdata_free(a)
             type(arhdata) :: a

             deallocate(a%fvec)
             deallocate(a%dvec)
             deallocate(a%work)
             deallocate(a%iwork)
             deallocate(a%xmo)
             deallocate(a%xao)
             deallocate(a%fov)
             deallocate(a%fkov)
             deallocate(a%dkov)
             deallocate(a%eig)
             deallocate(a%vec)
             deallocate(a%work2)

             a%nstor = 0
             a%shift = zero
             a%normtyp = ARH_NORMTYP_MAX
             a%h = ARH_START_TRUST
             a%enable_shift = .true.
       end subroutine arhdata_free


       subroutine arhdata_reset(a)
             type(arhdata) :: a

             a%nstor = 0
             a%shift = zero
             a%h = ARH_START_TRUST
             a%normtyp = ARH_NORMTYP_MAX
             a%enable_shift = .true.
       end subroutine arhdata_reset


       subroutine motooao(xmo, xoao, c, ncore, nvalence, nvirt)
             ! ------------------------------------------------------------
             ! Transform occupied-virtual block written in MO basis, XMO,
             ! to OAO basis. Tranformation results in full antisymmetric
             ! matrix XOAO:
             !
             ! XOAO = \sum_{ia} (XMO_{ia} * C_i C_a^T
             ! - XMO_{ia} * C_a C_i^T)
             ! ------------------------------------------------------------
             ! XMO     - Input, rectangular matrix written in MO basis
             ! XOAO    - Output, antisymmetic matrix in OAO basis
             ! C       - Matrix of columns which represent occupied and 
             !           virtual orbitals written in OAO basis
             ! NCORE,  - Number of core, valence, and virtual orbitals
             ! NVALENCE
             ! NVIRT
             !
             double precision, dimension(:, :), contiguous, intent(in)  :: xmo
             double precision, dimension(:, :), contiguous, intent(out) :: xoao
             double precision, dimension(:, :), contiguous, intent(in)  :: c
             integer, intent(in)                                        :: ncore
             integer, intent(in)                                        :: nvalence
             integer, intent(in)                                        :: nvirt

             integer :: k, l
             integer :: i, j, a
             double precision :: xia, xja, xij
             
             external :: dger
             !
             ! Core-virtual contribution
             !
             xoao = ZERO
             do a = nvalence + 1, nvalence + nvirt
                   do i = 1, ncore
                         xia = xmo(i, a)
                         call dger(NORB, NORB, xia, c(:, i), 1, c(:, ncore+a), 1, xoao, NORB)
                   end do
             end do
             !
             ! Valence-virtual contribution
             !
             if (nvalence > 0) then
                   do a = nvalence + 1, nvalence + nvirt
                         do j = ncore + 1, ncore + nvalence
                               xja = xmo(j, a)
                               call dger(NORB, NORB, xja, c(:, j), 1, c(:, ncore+a), 1, xoao, NORB)
                         end do
                   end do
             end if
             !
             ! Core-valence contribution
             !
             if (nvalence > 0) then
                   do j = 1, nvalence
                         do i = 1, ncore
                               xij = xmo(i, j)
                               call dger(NORB, NORB, xij, c(:, i), 1, c(:, ncore+j), 1, xoao, NORB)
                         end do
                   end do
             end if
             !
             ! Cache-ignorant loop
             !
             do l = 1, norb
                   xoao(l, l) = zero
                   do k = l + 1, norb
                         xoao(k, l) = xoao(k, l) - xoao(l, k)
                         xoao(l, k) = -xoao(k, l)
                   end do
             end do
       end subroutine motooao


       function matrixnorm(x, m, n, normtyp)
             double precision                              :: matrixnorm
             double precision, dimension(:, :), intent(in) :: x
             integer, intent(in)                           :: m, n
             integer, intent(in)                           :: normtyp

             if (normtyp .eq. ARH_NORMTYP_MAX) then
                   matrixnorm = maxnorm(x, m, n)
             else
                   matrixnorm = frobnorm(x, m, n)
             end if
       end function matrixnorm


       function freeidx(d)
             integer       :: freeidx
             type(arhdata) :: d
             
             integer :: i, j
             integer :: nstor
             logical :: occupied
             
             nstor = d%nstor
             freeidx = -1

             if (nstor .ge. ARH_MAX_NSTOR) then
                   freeidx = -1
             else if ((nstor > 0) .and. (nstor .lt. ARH_MAX_NSTOR)) then
                   iloop: do i = 1, ARH_MAX_NSTOR
                         occupied = .false.
                         jloop: do j = 1, nstor
                               if (d%idx(j) .eq. i) then
                                     occupied = .true.
                                     exit jloop
                               end if
                         end do jloop

                         if (.not. occupied) then
                               freeidx = i
                               exit iloop
                         end if
                   end do iloop
             else if (nstor .eq. 0) then
                   freeidx = 1
             end if
       end function freeidx


       subroutine up(a, ksmatrix, rho)
             type(arhdata) :: a
             double precision, dimension(:, :), intent(in) :: ksmatrix
             double precision, dimension(:, :), intent(in) :: rho

             integer :: i, idx1, nstor

             nstor = a%nstor

             if (nstor .lt. ARH_MAX_NSTOR) then
                   i = freeidx(a)
                   a%fvec(:, :, i) = ksmatrix
                   a%dvec(:, :, i) = rho
                   a%idx(nstor + 1) = i
                   a%nstor = a%nstor + 1
             else
                   !
                   ! A%IDX(1) is the index of the oldest (F, D)
                   ! pair, A%IDX(2) is the second oldest etc.
                   ! Overwrite the oldest (F, D) pair with new 
                   ! (F, D) pair. Change indices so now the oldest
                   ! pair is the former second oldest etc.
                   !
                   idx1 = a%idx(1)
                   a%fvec(:, :, idx1) = ksmatrix
                   a%dvec(:, :, idx1) = rho
                   do i = 1, ARH_MAX_NSTOR - 1
                         a%idx(i) = a%idx(i + 1)
                   end do
                   a%idx(ARH_MAX_NSTOR) = idx1
             end if
       end subroutine up


       subroutine del(d, i)
             ! --------------------------------------
             ! Delete I-th pair of matrices, (D, F):
             ! I=1     Oldest pair
             ! I=NSTOR Newest pair
             !
             type(arhdata)       :: d
             integer, intent(in) :: i

             integer :: k, nstor

             nstor = d%nstor
             if (nstor < 0 .or. nstor > ARH_MAX_NSTOR) then
                   call msg("Invalid value of NSTOR on entry to DEL", MSG_ERROR)
                   stop
             end if

             if (i <= 0) then
                   call msg("Invalid value of I on entry to DEL (I <= 0)", MSG_ERROR)
                   stop
             end if

             if (i > nstor) then
                   call msg("Invalid value of I on entry to DEL (I > NSTOR)", MSG_ERROR)
                   stop
             end if
                   
             if (nstor > 0) then
                   do k = i, nstor - 1
                         d%idx(k) = d%idx(k + 1)
                   end do
                   d%nstor = d%nstor - 1
             end if
       end subroutine del


       subroutine islindep(d, lindep, rho, del_list)
             type(arhdata), intent(in)                       :: d
             logical, intent(out)                            :: lindep
             real(F64), dimension(:, :), intent(in)          :: rho 
             integer, dimension(:), allocatable, intent(out) :: del_list

             real(F64) :: mineig, s
             real(F64), dimension(:, :), allocatable :: overlap, ts
             real(F64), dimension(:), allocatable :: w
             integer :: ndeleted
             integer :: k, l, kk, ll, m
             integer :: n
             integer :: nstor
             !
             ! Number of stored density matrices
             ! (the newest density matrix does not count as stored)
             !
             nstor = d%nstor
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
             do l = 1, n - 1
                   ll = d%idx(l)
                   do k = l, n - 1
                         kk = d%idx(k)
                         overlap(k, l) = sydotp(d%dvec(:, :, kk), d%dvec(:, :, ll))
                   end do
             end do

             do k = 1, n - 1
                   kk = d%idx(k)
                   overlap(n, k) = sydotp(rho, d%dvec(:, :, kk))
             end do
             overlap(n, n) = sydotp(rho, rho)
             !
             ! Normalize the Gram matrix
             !
             s = overlap(n, n)
             overlap = overlap / s
             ndeleted = 0
             !
             ! If addition of RHO(N) causes linear dependence, try to delete
             ! try to delete, in the following order, RHO(N-1), RHO(N-2), etc.
             ! It is tested numerically that this algorithm leads to the largest
             ! number of density matrices at the end of SCF.
             !
             delloop: do k = 1, n - 1
                   !
                   ! Compute eigenvalues of the Gram matrix
                   !
                   ts = overlap
                   call symmetric_eigenproblem(w, ts, n-ndeleted, .false.)
                   !
                   ! Check if smallest eigenvalue of the overlap
                   ! matrix is below the linear dependency threshold
                   !
                   mineig = minval(w(1:n-ndeleted))
                   if (mineig < ARH_LINDEP_THRESH) then
                         ndeleted = ndeleted + 1
                   else
                         exit delloop
                   end if
             end do delloop

             if (ndeleted > 0) then
                   lindep = .true.
                   allocate(del_list(ndeleted))
                   do m = 1, ndeleted
                         del_list(m) = n - m
                   end do
             else
                   lindep = .false.
             end if
       end subroutine islindep


       subroutine inv(tinverse, t, nstor)
             ! ------------------------------------------------------
             ! Invert matrix of dot products of density differences.
             ! All matrices (output TINVERSE included) are asummed
             ! to have the leading dimension of ARH_MAX_NSTOR. For
             ! good numerical stability inverse by diagonalization
             ! if performed.
             ! ------------------------------------------------------
             ! TINVERSE - Output, T^{-1} matrix
             ! T        - Input, matrix of dot products:
             !            <D_{kn} | D_{ln}>
             ! NSTOR    - Input, number of stored density matrices,
             !            order of T matrix
             !
             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR), intent(out)   :: tinverse
             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR), intent(inout) :: t
             integer, intent(in)                                                      :: nstor

             integer, parameter :: lwork = max(ARH_MAX_NSTOR**2, 3 * ARH_MAX_NSTOR - 1)
             double precision, dimension(lwork)  :: work
             double precision, dimension(ARH_MAX_NSTOR) :: w
             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR) :: work2
             integer :: info
             integer :: i, j, k
             external :: dsyev
             !
             ! Symmetric matrix T is inverted using diagonalization
             ! subroutine due to its good numerical stability.
             !
             call dsyev("V", "L", nstor, t, ARH_MAX_NSTOR, w, work, lwork, info)
             work2 = t
             do i = 1, nstor
                   work2(1:nstor, i) = work2(1:nstor, i) / w(i)
             end do

             do j = 1, nstor
                   do i = 1, nstor
                         tinverse(i, j) = zero
                         do k = 1, nstor
                               tinverse(i, j) = tinverse(i, j) + work2(i, k) * t(j, k)
                         end do
                   end do
             end do
       end subroutine inv


       subroutine comptinverse(d, tinverse, dn)
             ! --------------------------------------------------------------
             ! Invert matrix of dot products of density differences:
             ! <D_{kn} | D_{ln}>, D_{kn} = D_k - D_n, D_n - newest 
             ! density matrix. Neither density matrices from previous
             ! iterations, nor the newest density matrix need to be 
             ! transformed to the basis diagonalizing occupied-occupied
             ! and virtual-virtual blocks of the Fock matrix. This is
             ! due to the invariance of matrix dot product with respect
             ! to orthogonal transformation.
             ! --------------------------------------------------------------
             ! D        - Input, ARHDATA object
             ! TINVERSE - Output, inverse of the dot product matrix:
             !            T_{kl} = (M^{-1})_{kl}, M_{kl} = <D_{kn} | D_{ln}>
             ! DN       - Input, density matrix from last iteration
             !            (non-stored)
             !
             type(arhdata)                                                          :: d
             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR), intent(out) :: tinverse
             double precision, dimension(:, :), intent(in)                          :: dn

             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR) :: t
             double precision :: tij
             integer :: i, j
             integer :: ii, jj
             integer :: k, l
             integer :: nstor

             nstor = d%nstor

             do j = 1, nstor
                   jj = d%idx(j)
                   do i = j, nstor
                         ii = d%idx(i)
                         tij = zero
                         associate (di => d%dvec(:, :, ii), dj => d%dvec(:, :, jj))
                               do l = 1, norb
                                     !
                                     ! Diagonal contribution to matrix dot product
                                     !
                                     tij = tij + (di(l, l) - dn(l, l)) * (dj(l, l) - dn(l, l))
                                     !
                                     ! Off-diagonal contribution to matrix dot product
                                     !
                                     do k = l + 1, norb
                                           tij = tij + two * (di(k, l) - dn(k, l)) * (dj(k, l) - dn(k, l))
                                     end do
                               end do
                         end associate
                         t(i, j) = tij
                   end do
             end do

             call inv(tinverse, t, nstor)
       end subroutine comptinverse


       subroutine compsigma(d, sigma)
             type(arhdata)                                  :: d
             double precision, dimension(:), intent(out)    :: sigma

             double precision :: t
             double precision :: dl_ia, dl_ja, dl_ij
             double precision :: ni, nj
             integer :: l, i, j, a

             associate (nocc => d%nocc, nvirt => d%nvirt, &
                   nvalence => d%nvalence, x => d%xmo, nstor => d%nstor, &
                   ncore => d%ncore)

                   ni = d%occupation_number(1)
                   nj = d%occupation_number(2)

                   do l = 1, nstor
                         !
                         ! Sigma intermediate:
                         ! core-virtual contribution
                         !
                         t = ZERO
                         do a = nvalence + 1, nvalence + nvirt
                               do i = 1, ncore
                                     dl_ia = d%dkov(i, a, l)
                                     t = t + x(i, a) * dl_ia
                               end do
                         end do
                         sigma(l) = -TWO * ni * t
                         !
                         ! Sigma intermediate:
                         ! valence-virtual contribution
                         !
                         if (nvalence > 0) then
                               t = ZERO
                               do a = nvalence + 1, nvalence + nvirt
                                     do j = ncore + 1, ncore + nvalence
                                           dl_ja = d%dkov(j, a, l)
                                           t = t + x(j, a) * dl_ja
                                     end do
                               end do
                               sigma(l) = sigma(l) - TWO * nj * t
                         end if
                         !
                         ! Sigma intermediate:
                         ! core-valence contribution
                         !
                         if (nvalence > 0) then
                               t = ZERO
                               do j = 1, nvalence
                                     do i = 1, ncore
                                           dl_ij = d%dkov(i, j, l)
                                           t = t + x(i, j) * dl_ij
                                     end do
                               end do
                               sigma(l) = sigma(l) - TWO * (ni - nj) * t
                         end if
                   end do
             end associate
       end subroutine compsigma


       subroutine comptau(d, tau, shift)
             type(arhdata)                                  :: d
             double precision, dimension(:, :), intent(out) :: tau
             double precision, intent(in)                   :: shift

             double precision :: t
             double precision :: dl_ia, dl_ja, dl_ij
             double precision :: fkn_ia, fkn_ja, fkn_ij
             double precision :: ni, nj
             double precision :: ediff
             integer :: k, l
             integer :: i, j, a

             associate (nocc => d%nocc, nvirt => d%nvirt, eig => d%eig, &
                   nvalence => d%nvalence, x => d%xmo, nstor => d%nstor, &
                   ncore => d%ncore)

                   ni = d%occupation_number(1)
                   nj = d%occupation_number(2)

                   do l = 1, nstor
                         do k = 1, nstor
                               !
                               ! Core-virtual contribution
                               !
                               t = ZERO
                               do a = nvalence + 1, nvalence + nvirt
                                     do i = 1, ncore
                                           ediff = eig(ncore+a) - eig(i) - shift
                                           fkn_ia = d%fkov(i, a, k) - d%fov(i, a)
                                           dl_ia = d%dkov(i, a, l)
                                           t = t + fkn_ia * dl_ia / ediff
                                     end do
                               end do
                               tau(k, l) = -TWO * ni * t
                               !
                               ! Valence-virtual contribution
                               !
                               if (nvalence > 0) then
                                     t = ZERO
                                     do a = nvalence + 1, nvalence + nvirt
                                           do j = ncore + 1, ncore + nvalence
                                                 ediff = eig(ncore+a) - eig(j) - shift
                                                 fkn_ja = d%fkov(j, a, k) - d%fov(j, a)
                                                 dl_ja = d%dkov(j, a, l)
                                                 t = t + fkn_ja * dl_ja / ediff
                                           end do
                                     end do
                                     tau(k, l) = tau(k, l) - TWO * nj * t
                               end if
                               !
                               ! Core-valence contribution
                               !
                               if (nvalence > 0) then
                                     t = ZERO
                                     do j = 1, nvalence
                                           do i = 1, ncore
                                                 ediff = eig(ncore+j) - eig(i) - shift
                                                 fkn_ij = d%fkov(i, j, k) - d%fov(i, j)
                                                 dl_ij = d%dkov(i, j, l)
                                                 t = t + fkn_ij * dl_ij / ediff
                                           end do
                                     end do
                                     tau(k, l) = tau(k, l) - TWO * (ni - nj) * t
                               end if
                         end do
                   end do
             end associate
       end subroutine comptau


       subroutine compomega(omega, tau, tinverse, nstor)
             double precision, dimension(:, :), intent(out) :: omega
             double precision, dimension(:, :), intent(in)  :: tau
             double precision, dimension(:, :), intent(in)  :: tinverse
             integer, intent(in)                            :: nstor

             integer :: k, l, m

             do l = 1, nstor
                   do k = 1, nstor
                         omega(k, l) = zero
                         
                         do m = 1, nstor
                               omega(k, l) = omega(k, l) - tau(m, k) * tinverse(m, l)
                         end do
                         
                         if (k .eq. l) omega(k, k) = omega(k, k) + one
                   end do
             end do
       end subroutine compomega


       subroutine ovtrans(d)
             !
             ! Transform stored density matrices and
             ! first derivative matrices to VEC basis
             !
             type(arhdata)                                 :: d

             integer :: k, kk
             integer :: dim1, dim2

             associate (nocc => d%nocc, nvirt => d%nvirt, &
                   ncore => d%ncore, nvalence => d%nvalence, vec => d%vec)
                   
                   dim1 = nocc
                   dim2 = nvalence + nvirt

                   do k = 1, d%nstor
                         kk = d%idx(k)
                         call atbc(d%fkov(:, :, k), vec(:, 1:), d%fvec(:, :, kk), vec(:, ncore+1:), dim1, dim2)
                         call atbc(d%dkov(:, :, k), vec(:, 1:), d%dvec(:, :, kk), vec(:, ncore+1:), dim1, dim2)
                   end do
             end associate
       end subroutine ovtrans


       subroutine xarh(d, shift)
             type(arhdata)                                 :: d
             double precision, intent(in)                  :: shift

             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR) :: tau
             double precision, dimension(ARH_MAX_NSTOR, ARH_MAX_NSTOR) :: omega
             double precision, dimension(ARH_MAX_NSTOR) :: sigma
             !
             ! Solution of linear system
             !
             integer, dimension(ARH_MAX_NSTOR) :: ipiv
             integer :: info

             double precision :: fkn_ia, fkn_ja, fkn_ij
             double precision :: ediff, s
             integer :: i, j, a
             integer :: k, l

             external :: dgetrf
             external :: dgetrs

             associate (nocc => d%nocc, nvirt => d%nvirt, &
                   nvalence => d%nvalence, ncore => d%ncore, &
                   nstor => d%nstor, x => d%xmo, &
                   tinverse => d%tinverse, eig => d%eig)
                   
                   call xrh(d, shift)
                   !
                   ! Compute SIGMA0, TAU, and OMEGA intermediates
                   !
                   call compsigma(d, sigma)
                   call comptau(d, tau, shift)
                   call compomega(omega, tau, tinverse, nstor)
                   !
                   ! 1. Compute LU factorization of OMEGA
                   ! 2. Solve linear system
                   !    OMEGA * SIGMA = SIGMA0
                   !
                   call dgetrf(nstor, nstor, omega, &
                         ARH_MAX_NSTOR, ipiv, info)
                   call dgetrs("N", nstor, 1, omega, &
                         ARH_MAX_NSTOR, ipiv, sigma, ARH_MAX_NSTOR, info)
                   !
                   ! SIGMA intermediate will be used later
                   ! to compute model energy prediction
                   !
                   d%sigma = sigma
                   !
                   ! Compute the X parameter matrix
                   !
                   do k = 1, nstor
                         do l = 1, nstor
                               s = tinverse(k, l) * sigma(l)
                               !
                               ! Core-virtual contribution
                               !
                               do a = nvalence + 1, nvalence + nvirt
                                     do i = 1, ncore
                                           fkn_ia = d%fkov(i, a, k) - d%fov(i, a)
                                           ediff = eig(ncore+a) - eig(i) - shift
                                           x(i, a) = x(i, a) + fkn_ia * s / ediff
                                     end do
                               end do
                               !
                               ! Valence-virtual contribution
                               !
                               if (nvalence > 0) then
                                     do a = nvalence + 1, nvalence + nvirt
                                           do j = ncore + 1, ncore + nvalence
                                                 fkn_ja = d%fkov(j, a, k) - d%fov(j, a)
                                                 ediff = eig(ncore+a) - eig(j) - shift
                                                 x(j, a) = x(j, a) + fkn_ja * s / ediff
                                           end do
                                     end do
                               end if
                               !
                               ! Core-valence contribution
                               !
                               if (nvalence > 0) then
                                     do j = 1, nvalence
                                           do i = 1, ncore
                                                 fkn_ij = d%fkov(i, j, k) - d%fov(i, j)
                                                 ediff = eig(ncore+j) - eig(i) - shift
                                                 x(i, j) = x(i, j) + fkn_ij * s / ediff
                                           end do
                                     end do
                               end if
                         end do
                   end do
             end associate
       end subroutine xarh


       subroutine setbasis(d, fao, c)
             !
             ! Compute basis in which core-core, valence-valence, 
             ! and virtual-virtual blocks of the first derivative
             ! matrix are diagonal. Transform first derivative
             ! matrix
             !
             type(arhdata)                                              :: d
             double precision, dimension(:, :), contiguous, intent(in)  :: fao
             double precision, dimension(:, :), contiguous, intent(in)  :: c

             integer :: core0, valence0, virt0

             associate (nocc => d%nocc, nvirt => d%nvirt, &
                   fov => d%fov, eig => d%eig, vec => d%vec, &
                   ncore => d%ncore, nvalence => d%nvalence, &
                   work => d%work2)
                   ! 
                   ! Core-core block
                   !
                   core0 = d%occ0
                   work = fao
                   call subevd(work, c(:, core0:), eig(1:), ncore)
                   vec(:, 1:ncore) = work(:, 1:ncore)
                   !
                   ! Valence-valence block
                   !
                   if (nvalence > 0) then
                         valence0 = d%occ0 + ncore
                         work = fao
                         call subevd(work, c(:, valence0:), eig(ncore+1:), nvalence)
                         vec(:, ncore+1:ncore+nvalence) = work(:, 1:nvalence)
                   end if
                   !
                   ! Virtual-virtual block
                   !
                   virt0 = d%virt0
                   work = fao
                   call subevd(work, c(:, virt0:), eig(nocc+1:), nvirt)
                   vec(:, nocc+1:nocc+nvirt) = work(:, 1:nvirt)
                   !
                   ! Transform Fock matrix to new basis
                   !
                   call atbc(fov, vec(:, 1:), fao, vec(:, ncore+1:), nocc, nvalence+nvirt)
             end associate
       end subroutine setbasis


       subroutine gradient_norm(d)
             type(arhdata), intent(inout) :: d
             
             integer :: i, j, a

             associate (fov => d%fov, nvirt => d%nvirt, &
                   nvalence => d%nvalence, ncore => d%ncore, &
                   orb_grad_frob => d%orb_grad_frob, orb_grad_max => d%orb_grad_max)

                   orb_grad_frob = ZERO
                   orb_grad_max = ZERO
                   !
                   ! Core-virtual block (occupied-virtual in closed shell case)
                   !
                   do a = nvalence + 1, nvalence + nvirt
                         do i = 1, ncore
                               orb_grad_frob = orb_grad_frob + fov(i, a)**2
                               orb_grad_max = max(orb_grad_max, abs(fov(i, a)))
                         end do
                   end do
                   !
                   ! Valence-virtual block (not present in closed shell case)
                   !
                   if (nvalence > 0) then
                         do a = nvalence + 1, nvalence + nvirt
                               do j = ncore + 1, ncore + nvalence
                                     orb_grad_frob = orb_grad_frob + fov(j, a)**2
                                     orb_grad_max = max(orb_grad_max, abs(fov(j, a)))
                               end do
                         end do
                   end if
                   !
                   ! Core-valence block (not present in closed shell case)
                   !
                   if (nvalence > 0) then
                         do j = 1, nvalence
                               do i = 1, ncore
                                     orb_grad_frob = orb_grad_frob + fov(i, j)**2
                                     orb_grad_max = max(orb_grad_max, abs(fov(i, j)))
                               end do
                         end do
                   end if
                   !
                   ! Multiply by two because the full gradient matrix
                   ! is antisymmetric. Note that this norm is size-extensive
                   ! and should be divided by SQRT(N_{el}) to get a
                   ! size-intensive measure.
                   !
                   orb_grad_frob = sqrt(TWO * orb_grad_frob)
             end associate
       end subroutine gradient_norm


       subroutine xrh(d, shift)
             ! ----------------------------------------------------------
             ! Compute X matrix at (non-augmented) Roothaan-Hall level.
             ! X matrix can be computed in simple way, as the basis
             ! consists of eigenvectors of occupied-occupied and 
             ! virtual-virtual blocks of the Fock matrix. See Eq. 6.31,
             ! p. 73 in [1]
             ! ----------------------------------------------------------
             ! 1. Reine, S. S., Density functional theory for large
             !    molecular systems, PhD thesis
             !
             type(arhdata)                                  :: d
             double precision, intent(in)                   :: shift

             integer :: i, j, a

             associate (fov => d%fov, eig => d%eig, x => d%xmo, &
                   nocc => d%nocc, nvirt => d%nvirt, &
                   nvalence => d%nvalence, ncore => d%ncore)
                   if (nvalence > 0) then
                         !
                         ! Valence-valence amplitudes are ZERO by definition
                         !
                         do j = 1, nvalence
                               do i = ncore + 1, ncore + nvalence
                                     x(i, j) = ZERO
                               end do
                         end do
                   end if
                   !
                   ! Core-virtual parameters: X_{ia}
                   !
                   do a = nvalence + 1, nvalence + nvirt
                         do i = 1, ncore
                               !
                               ! Eq. 6.31 in [1] (sign is opposite
                               ! due to transposition)
                               !
                               x(i, a) = -fov(i, a) / (eig(ncore+a) - eig(i) - shift)
                         end do
                   end do
                   !
                   ! Valence-virtual parameters: X_{ja}
                   !
                   if (nvalence > 0) then
                         do a = nvalence + 1, nvalence + nvirt
                               do j = ncore + 1, ncore + nvalence
                                     x(j, a) = -fov(j, a) / (eig(ncore+a) - eig(j) - shift)
                               end do
                         end do
                   end if
                   !
                   ! Core-valence parameters: X_{ij}
                   !
                   if (nvalence > 0) then
                         do j = 1, nvalence
                               do i = 1, ncore
                                     x(i, j) = -fov(i, j) / (eig(ncore+j) - eig(i) - shift)
                               end do
                         end do
                   end if
             end associate
       end subroutine xrh


       subroutine trustup(d, err)
             ! --------------------------------------------------------
             ! Update the trust radius for the ARH quadratic model
             ! of energy, see eq. 4 in [2].
             ! EOLD and ENEW (components of ARHDATA) must be
             ! defined on entry to this subroutine.
             ! --------------------------------------------------------
             ! 1. Host, S. et al., The augmented Roothaan-Hall
             !    method for optimizing Hartree-Fock and Kohn-Sham
             !    density matrices,
             !    J. Chem. Phys. 129, 124106(2008)
             ! 2. Host, S. et al., A ground-state-directed
             !    optimization scheme for the Kohn-Sham energy,
             !    Phys. Chem. Chem. Phys. 10, 5344(2008)
             ! --------------------------------------------------------
             ! D   - ARHDATA structure
             ! ERR - Error code, 0 if density matrix is accepted,
             !       -1 if density matrix is rejected
             !
             type(arhdata)        :: d
             integer, intent(out) :: err
             
             real(F64) :: r, denom
             logical :: small_denom
             real(F64) :: min_denom             
             !
             ! Check the quality of energy estimate:
             ! Eqs. 50 & 51 in [1]
             !
             denom = d%epred - d%eold
             min_denom = abs(1.0E-15_F64 * d%eold)
             if (abs(denom) < min_denom) then
                   small_denom = .true.                   
                   denom = sign(denom, min_denom)
             else
                   small_denom = .false.
             end if
             r = (d%enew - d%eold) / denom
             !
             ! The higher the value of R, the better the ARH energy estimate
             !
             if (r >= 0.75d+0) then
                   err = 0
                   d%h = 1.2d+0 * d%h
             else if (r >= 0.25d+0) then
                   err = 0
             else if (r >= 0.00d+0) then
                   err = 0
                   if (.not. small_denom) then
                         d%h = 0.7d+0 * d%h
                   end if
             else
                   !
                   ! Because the predicted energy is always lower than
                   ! EOLD, R can be negative only if ENEW > EOLD
                   !
                   d%h = 0.7d+0 * d%h
                   !
                   ! If trust radius is still greater than XNORM,
                   ! decreasing the trust radius will not change
                   ! step size!
                   !
                   if (d%h > d%xnorm) d%h = 0.7d+0 * d%xnorm
                   err = -1
             end if
       end subroutine trustup


       function model_energy(d, arhsw)
             ! ------------------------------------------------
             ! Predict ARH energy. ARH prediction is
             ! required to set trust radius. Quadratic
             ! energy model is used, see eq. 4 in [1].
             ! NOTE: It is assumed that RH / ARH approximate
             ! model energy always decreases if RH / ARH
             ! equations are solved with sufficient accuracy.
             ! If it is not the case, EARHPRED may not work
             ! properly!
             ! ------------------------------------------------
             ! 1. Host, S. et al., A ground-state-directed
             !    optimization scheme for the Kohn-Sham energy,
             !    Phys. Chem. Chem. Phys. 10, 5344(2008)
             !
             double precision                              :: model_energy
             type(arhdata)                                 :: d
             logical, intent(in)                           :: arhsw

             double precision :: e11, e21, e22
             double precision :: fkn_ia, fkn_ja, fkn_ij
             double precision :: s, t
             integer :: i, j, a
             integer :: k, l

             associate (nocc => d%nocc, nvirt => d%nvirt, &
                   nvalence => d%nvalence, ncore => d%ncore, &
                   nstor => d%nstor, x => d%xmo, sigma => d%sigma, &
                   tinverse => d%tinverse, fov => d%fov, eig => d%eig, &
                   ni => d%occupation_number(1), nj => d%occupation_number(2))
                   !
                   ! Contribution first order in X,
                   ! first order in density matrix: E11
                   ! -----------------------------------
                   ! Core-virtual contribution
                   !
                   t = ZERO
                   do a = nvalence + 1, nvalence + nvirt
                         do i = 1, ncore
                               t = t + x(i, a) * fov(i, a)
                         end do
                   end do
                   e11 = TWO * ni * t
                   !
                   ! Valence-virtual contribution
                   !
                   if (nvalence > 0) then
                         t = ZERO
                         do a = nvalence + 1, nvalence + nvirt
                               do j = ncore + 1, ncore + nvalence
                                     t = t + x(j, a) * fov(j, a)
                               end do
                         end do
                         e11 = e11 + TWO * nj * t
                   end if
                   !
                   ! Core-valence contribution
                   !
                   if (nvalence > 0) then
                         t = ZERO
                         do j = 1, nvalence
                               do i = 1, ncore
                                     t = t + x(i, j) * fov(i, j)
                               end do
                         end do
                         e11 = e11 + TWO * (ni - nj) * t
                   end if
                   !
                   ! Contribution second order in X,
                   ! first order in density matrix: E21
                   ! -----------------------------------
                   ! Core-virtual contribution
                   !
                   t = ZERO
                   do a = nvalence + 1, nvalence + nvirt
                         do i = 1, ncore
                               t = t + x(i, a)**2 * (eig(ncore+a) - eig(i))
                         end do
                   end do
                   e21 = ni * t
                   !
                   ! Valence-virtual contribution
                   !
                   if (nvalence > 0) then
                         t = ZERO
                         do a = nvalence + 1, nvalence + nvirt
                               do j = ncore + 1, ncore + nvalence
                                     t = t + x(j, a)**2 * (eig(ncore+a) - eig(j))
                               end do
                         end do
                         e21 = e21 + nj * t
                   end if
                   !
                   ! Core-valence contribution
                   !
                   if (nvalence > 0) then
                         t = ZERO
                         do j = 1, nvalence
                               do i = 1, ncore 
                                     t = t + x(i, j)**2 * (eig(ncore+j) - eig(i))
                               end do
                         end do
                         e21 = e21 + (ni - nj) * t
                   end if
                   !
                   ! Contribution second order in X,
                   ! second order in density matrix: E22
                   ! ------------------------------------
                   !
                   e22 = ZERO
                   if (arhsw) then
                         do k = 1, nstor
                               do l = 1, nstor
                                     !
                                     ! Core-virtual contribution
                                     !
                                     s = ZERO
                                     do a = nvalence + 1, nvalence + nvirt
                                           do i = 1, ncore
                                                 fkn_ia = d%fkov(i, a, k) - d%fov(i, a)
                                                 s = s + x(i, a) * fkn_ia
                                           end do
                                     end do
                                     e22 = e22 - ni * tinverse(k, l) * sigma(l) * s
                                     !
                                     ! Valence-virtual contribution
                                     !
                                     if (nvalence > 0) then
                                           s = ZERO
                                           do a = nvalence + 1, nvalence + nvirt
                                                 do j = ncore + 1, ncore + nvalence
                                                       fkn_ja = d%fkov(j, a, k) - d%fov(j, a)
                                                       s = s + x(j, a) * fkn_ja
                                                 end do
                                           end do
                                           e22 = e22 - nj * tinverse(k, l) * sigma(l) * s
                                     end if
                                     !
                                     ! Core-valence contribution
                                     !
                                     if (nvalence > 0) then
                                           s = ZERO
                                           do j = 1, nvalence
                                                 do i = 1, ncore
                                                       fkn_ij = d%fkov(i, j, k) - d%fov(i, j)
                                                       s = s + x(i, j) * fkn_ij
                                                 end do
                                           end do
                                           e22 = e22 - (ni - nj) * tinverse(k, l) * sigma(l) * s
                                     end if
                               end do
                         end do
                   end if
             end associate
             
             model_energy = e11 + e21 + e22
       end function model_energy


       subroutine xmatrix(d, shift, arhsw)
             ! ----------------------------------------------------------------
             ! Compute antisymmetric matrix of orbital rotations (in basis
             ! of the eigenvectors of occupied-occupied and virtual-virtual
             ! blocks of the Fock matrix). Depending on ARHSW switching
             ! parameter, (non-augmented) Roothaan-Hall or augmented Roothaan-
             ! Hall method is used.
             ! ----------------------------------------------------------------
             ! D        - ARHDATA structure
             ! SHIFT    - Input, parameter for shifting eigenvalues (non-zero
             !            when step goes beyond trust radius or Roothaan-Hall
             !            Hessian is non-positive definite)
             ! ARHSW    - Input, if ARHSW == .TRUE., ARH algorithm is used,
             !            otherwise orbital rotations are computed by RH scheme
             !
             type(arhdata)                                  :: d
             double precision, intent(in)                   :: shift
             logical, intent(in)                            :: arhsw

             if (arhsw) then
                   call xarh(d, shift)
             else
                   call xrh(d, shift)
             end if
       end subroutine xmatrix


       subroutine find_smaller(d, shift, xnorm, targetnorm, gap, arhsw)
             type(arhdata)                 :: d
             double precision, intent(out) :: shift
             double precision, intent(out) :: xnorm
             double precision, intent(in)  :: targetnorm
             double precision, intent(in)  :: gap
             logical, intent(in)           :: arhsw

             integer :: k
             double precision :: deltas, shift0

             if (gap .lt. zero) then
                   deltas = frac12 * gap
                   shift0 = gap + deltas
             else
                   deltas = -frac12 * gap
                   shift0 = deltas
             end if

             shift = shift0

             do k = 1, 50
                   call xmatrix(d, shift, arhsw)
                   associate (nocc => d%nocc, nvalence => d%nvalence, &
                         nvirt => d%nvirt, normtyp => d%normtyp, x => d%xmo)
                         
                         xnorm = matrixnorm(x, nocc, nvalence+nvirt, normtyp)
                   end associate

                   if (xnorm .lt. targetnorm) then
                         exit
                   else
                         deltas = deltas * two
                         shift = shift0 + deltas
                   end if
             end do
       end subroutine find_smaller


       subroutine compgap(eig, ncore, nvalence, gap)
             !
             ! Compute the HOMO-LUMO gap in the current iteration.
             !
             real(F64), dimension(:), intent(in) :: eig
             integer, intent(in)                 :: ncore
             integer, intent(in)                 :: nvalence
             real(F64), intent(out)              :: gap

             real(F64) :: gapia, gapja, gapij

             if (nvalence > 0) then
                   gapij = eig(ncore+1) - eig(ncore)
                   gapja = eig(ncore+nvalence+1) - eig(ncore+nvalence)
                   gapia = eig(ncore+nvalence+1) - eig(ncore)
                   gap = min(gapij, gapja, gapia)
             else
                   gap = eig(ncore+1) - eig(ncore)
             end if
       end subroutine compgap


       subroutine arh_driver(d, rho, c, fao)
             ! -------------------------------------------------------
             ! D   - ARHDATA structure
             !
             ! RHO - Density matrix RHO = \sum_i n_i C_i C_i^T
             !       + \sum_j C_j C_j^T, n_i = OCCUPATION_NUMBER(1),
             !       n_j = OCCUPATION_NUMBER(2). Closed shell case:
             !       RHO = 2 * sum_i C_i C_i^T. C_i's and C_j's are
             !       core orbitals and valence orbitals, respectively.
             !       RHO is expressed in OAO basis. 
             !
             ! C   - Matrix of orbital coefficients in orthogonal
             !       (OAO) basis. The coefficients are stored
             !       column-wise.
             !       
             ! FAO - The matrix of first derivatives:
             !       FAO_{pq} = d E / d RHO_{pq}
             !       (OAO, both triangles referenced)
             ! --------------------------------------------------------
             ! 1. Host, S. et al., The augmented Roothaan–Hall
             !    method for optimizing Hartree–Fock and Kohn–Sham
             !    density matrices,
             !    J. Chem. Phys. 129, 124106(2008)
             ! 2. Thogersen, L., Olsen, J., Kohn, A., Jorgensen, P.,
             !    Salek, P., Helgeker T., The trust-region self-
             !    consistent field method in Kohn-Sham density-
             !    functional theory,
             !    J. Chem. Phys. 123, 074103(2005)
             !
             type(arhdata)                                                :: d
             double precision, dimension(:, :), contiguous, intent(in)    :: rho
             double precision, dimension(:, :), contiguous, intent(inout) :: c
             double precision, dimension(:, :), contiguous, intent(in)    :: fao

             integer :: k
             logical :: arhsw
             logical :: lindep
             !
             ! Convergence control
             !
             double precision            :: targetnorm
             double precision            :: maxshift
             double precision            :: shift0, shift1
             double precision, parameter :: eps = 1.d-3
             double precision            :: gap
             double precision            :: xnorm0, xnorm1
             real(F64) :: max_overlap
             integer, dimension(:), allocatable :: del_list

             associate (nocc => d%nocc, nvalence => d%nvalence, ncore => d%ncore, &
                   nvirt => d%nvirt, normtyp => d%normtyp, x => d%xmo, xao => d%xao, &
                   trust => d%h, vec => d%vec, eig => d%eig, tinverse => d%tinverse, &
                   shift => d%shift, xnorm => d%xnorm, nstor => d%nstor, &
                   enable_shift => d%enable_shift)
                   !
                   ! Check if MO vectors are orthogonal. Perform
                   ! orthogonalization if orthogonality check 
                   ! failed. All ARH subroutines assume MO vectors'
                   ! orthogonality (OAO basis)
                   !
                   call real_conditional_gramschmidt(.true., c, ARH_ORTH_THRESH, max_overlap)
                   if (max_overlap > ARH_ORTH_THRESH) then
                         call msg("MOs WERE ORTHONORMALIZED BY ARH")
                         call dmsg("OFF-DIAGONAL OVERLAP", max_overlap, fmt="ES10.1")
                   end if
                   !
                   ! Compute basis in which core-core, valence-valence,
                   ! and virtual-virtual block of the first-derivative
                   ! matrix are diagonal. Compute eigenvalues of the
                   ! core-core, valence-valence, and virtual-virtual
                   ! blocks
                   !
                   call setbasis(d, fao, c)
                   !
                   ! Compute Frobenius and max norms of the orbital gradient matrix
                   !
                   call gradient_norm(d)
                   
                   if (nstor >= ARH_TRIGGER) then
                         arhsw = .true.
                         call comptinverse(d, tinverse, rho)
                         call ovtrans(d)
                   else
                         arhsw = .false.
                   end if

100                continue
                   !
                   ! Check positive definitness of the Hessian
                   ! See eqs. 29b, 36a in [2]
                   !
                   call compgap(eig, ncore, nvalence, gap)
                   !
                   ! Compute the maximum value of energy shift. It will be used if
                   ! the orbital gap is negative or the norm of X is larger than
                   ! the trust radius.
                   !
                   if (gap < ARH_MINGAP) then
                         maxshift = gap - ARH_MINGAP
                         shift = maxshift
                   else
                         maxshift = ZERO
                         shift = ZERO
                   end if

                   call xmatrix(d, shift, arhsw)
                   xnorm0 = matrixnorm(x, nocc, nvalence+nvirt, normtyp)
                   !
                   ! Reduce trust radius if ||X||_{max} << h
                   ! See section C in [1]
                   !
                   if (xnorm0 * two .lt. trust) trust = trust * 0.7d+0
                   !
                   ! Compute X matrix with smaller norm if
                   ! current matrix does not comply with
                   ! TRUST threshold
                   !
                   if (xnorm0 >= trust .and. enable_shift) then
                         !
                         ! Set target norm of X matrix
                         !
                         targetnorm = trust
                         call find_smaller(d, shift1, xnorm1, targetnorm, gap, arhsw)
                         shift0 = maxshift
                         bisection: do k = 1, 30
                               if ((abs((xnorm0 - targetnorm) / targetnorm) .le. eps) &
                                     .and. (abs((xnorm1 - targetnorm) / targetnorm) .le. eps)) &
                                     exit bisection

                               shift = (shift0 + shift1) / two
                               call xmatrix(d, shift, arhsw)
                               xnorm = matrixnorm(x, nocc, nvalence+nvirt, normtyp)

                               if (xnorm > targetnorm) then
                                     shift0 = shift
                                     xnorm0 = xnorm
                               else
                                     shift1 = shift
                                     xnorm1 = xnorm
                               end if
                         end do bisection
                   end if
                   !
                   ! Stored X matrix norm will be required by
                   ! subroutine updating trust radius. Check
                   ! if norm type has to be switched to
                   ! Frobenius. (See iteration 14, table II
                   ! in [1].)
                   !
                   xnorm = matrixnorm(x, nocc, nvalence+nvirt, normtyp)
                   if (xnorm .le. ARH_FROBENIUS .and. normtyp .eq. ARH_NORMTYP_MAX) then
                         normtyp = ARH_NORMTYP_FROB
                         !
                         ! Norm type is switched to Frobenius from max norm
                         !
                         xnorm = matrixnorm(x, nocc, nvalence+nvirt, normtyp)
                         !
                         ! Update trust radius to current Frobenius norm
                         !
                         trust = xnorm
                   end if
                   !
                   ! 1. Calculate estimate of energy.
                   ! 2. Check if predicted change of energy is negative.
                   !    It can be positive in the case ARH Hessian is
                   !    projected onto an insufficient number of matrices.
                   !    If that is the case, compute the rotation matrix X
                   !    at the Roothan-Hall (RH) level instead of augmented RH.
                   !
                   d%epred = d%eold + model_energy(d, arhsw)
                   if (((d%epred - d%eold) > ZERO) .and. arhsw .and. enable_shift) then
                         arhsw = .false.
                         goto 100
                   end if
                   !
                   ! Compute skew-symmetric matrix X in AO basis
                   ! X_{AO} = C_{occ} X C_{virt}^T - C_{virt} X X_{occ}^T
                   !
                   call motooao(x, xao, vec, ncore, nvalence, nvirt)
                   !
                   ! Update molecular orbitals
                   !
                   call exp_transform(c, xao)
                   call islindep(d, lindep, rho, del_list)
                   if (lindep) then
                         do k = 1, size(del_list)
                               !
                               ! DEL_LIST is a list of indices sorted in decreasing order
                               !
                               call del(d, del_list(k))
                         end do
                   end if
                   !
                   ! Store new pair of density and KS matrices
                   !
                   call up(d, fao, rho)
             end associate
       end subroutine arh_driver
end module arh
