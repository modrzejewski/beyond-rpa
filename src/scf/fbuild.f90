module fbuild
      use gparam
      use clock
      use images
      use basis
      use hfexch
      use hfcoul
      use screen
      use fock2el
      use lcexch
      use arithmetic
      use math_constants

      implicit none

      double precision, dimension(:, :), allocatable, private, save :: erimax
      double precision, dimension(:, :), allocatable, private, save :: rhomax
      integer, dimension(:, :), allocatable, private, save          :: ierimax
      double precision, dimension(:, :), allocatable, private, save :: rhoerimax
      integer, dimension(:, :), allocatable, private, save          :: irhoerimax
      !
      ! ------------------------------------------------------------
      !                Incremental direct Fock build 
      ! ------------------------------------------------------------
      ! Fock matrices are formed using DELTA = RHO(n) - RHO(n-1)
      ! density matrix. Fock matrix from previous iterations
      ! is updated according to FOCK = FOCK(RHO(n-1)) + FOCK(DELTA).
      ! Density-matrix dependent screening of the Fock matrix should
      ! be enabled. Performance increase is substantial when SCF
      ! is close to convergence, when ||DELTA|| is small.
      !
      ! Note on incremental Fock build from GAMESS manual: "Cases
      ! with many diffuse functions in the basis set sometimes
      ! oscillate at the end, rather than converging. Turning this
      ! parameter off [FDIFF parameter] will normally give
      ! convergence."
      ! ------------------------------------------------------------
      ! 1. Schwegler, E., Linear scaling computation of the Fock
      !    matrix. II. Rigorous bounds on exchange integrals and
      !    incremental Fock builds, J. Chem. Phys. 106, 9708(1996).
      ! ------------------------------------------------------------
      !
contains

      subroutine fbuild_init()
            if (BUILDSKS) then
                  allocate(erimax(nshell, nshell))
                  allocate(rhomax(nshell, nshell))
                  allocate(ierimax(nshell, nshell))
                  allocate(rhoerimax(nshell, nshell))
                  allocate(irhoerimax(nshell, nshell))
                  call screenint(erimax, ierimax)
            end if
      end subroutine fbuild_init


      subroutine fbuild_free()
            if (BUILDSKS) then
                  deallocate(erimax)
                  deallocate(rhomax)
                  deallocate(ierimax)
                  deallocate(rhoerimax)
                  deallocate(irhoerimax)
            end if
      end subroutine fbuild_free


      function maxabsnorm(a)
            double precision                              :: maxabsnorm
            double precision, dimension(:, :), intent(in) :: a
            
            integer :: i, j

            maxabsnorm = zero
            do j = 1, norb
                  do i = j, norb
                        if (abs(a(i, j)) .gt. maxabsnorm) maxabsnorm = abs(a(i, j))
                  end do
            end do
      end function maxabsnorm


      subroutine fdirect(kmatrix, jmatrix, rhomatrix, kscal, lcexch, omega, srexx, &
            antisymmetric, ScreenedHybrid, chunk, imask)
            !
            ! Driver subroutine for the direct build of Hartree-Fock exchange
            ! (KMATRIX) and Coulomb (JMATRIX) matrices:
            !
            ! KMatrix(p, q) = KMatrix(p, q) + KScal * Sum_{rs} RhoMatrix(r, s) (qr|ps)
            ! JMatrix(p, q) = JMatrix(p, q) + Sum_{rs} RhoMatrix(r, s) (pq|rs)
            !
            ! Note the order of the pq indices in KMatrix, which is significant in
            ! the case when RhoMatrix is antisymmetric, e.g., for complex-valued
            ! orbitals.
            !
            ! The Coulomb matrix is exactly zero if RhoMatrix is antisymmetic
            ! (antisymmetric == .true.) and is not referenced in that case.
            !
            double precision, dimension(:, :), intent(inout) :: kmatrix
            double precision, dimension(:, :), intent(inout) :: jmatrix
            double precision, dimension(:, :), intent(in)    :: rhomatrix
            double precision, intent(in)                     :: kscal
            logical, intent(in)                              :: lcexch
            double precision, intent(in)                     :: omega
            double precision, intent(in)                     :: srexx
            logical, intent(in)                              :: antisymmetric
            logical, intent(in)                              :: ScreenedHybrid
            integer, dimension(2), optional, intent(in)      :: chunk
            integer, optional, intent(in)                    :: imask
            
            double precision :: kscal_lr, kscal_full
            integer :: imask0, imask_coul, imask_exch
            real(F64) :: lrexx
            real(F64) :: Kappa
            type(tclock) :: t_ints2e
            
            call clock_start(t_ints2e)

            if (present(imask)) then
                  imask0 = imask
            else
                  imask0 = COULEXCHSUM
            end if

            if (antisymmetric) then
                  imask0 = iand(imask0, EXCHSUM)
            end if

            if (lcexch .or. ScreenedHybrid) then
                  !
                  ! Initialize variables required for computing
                  ! the integrals with ERF(OMEGA*R)/R operator
                  !
                  call lce_setuplcfunc(omega, srexx)
                  Kappa = ONE/omega**2
                  !
                  ! Longe-range exchange integrals are requested.
                  ! Now, Coulomb and exchange integrals cannot be
                  ! evaluated in a single run. A bitmask representing
                  ! requested integrals, IMASK, is partitioned into
                  ! Coulomb and exchange integrals.
                  !
                  imask_coul = iand(imask0, COULSUM)
                  imask_exch = iand(imask0, EXCHSUM)
                  !
                  ! Let A and B represent a portion of
                  ! exact exchange in short- and long-range.
                  ! We evaluate exchange integrals with full
                  ! 1/r_{12} operator and long-range integrals
                  ! with ERF(\omega r_{12})/r_{12} scaled by
                  ! KSCAL_FULL and KSCAL_LR:
                  !
                  ! a <- SREXX
                  ! b <- LREXX
                  ! a E_x^{HF,SR} + b E_x^{HF,LR} =
                  ! a E_x^{HF} + (b - a) E_x^{HF,LR}
                  !
                  ! Here, it is assumed that KSCAL passed as
                  ! an argument to this subroutine is not
                  ! multiplied by neither A nor B.
                  !
                  if (ScreenedHybrid) then
                        lrexx = ZERO
                  else
                        lrexx = ONE
                  end if
                  kscal_full = srexx * kscal
                  kscal_lr = (lrexx - srexx) * kscal

                  if (present(chunk)) then
                        !
                        ! Long-range contribution to the exchange matrix
                        !
                        call lce_activate()
                        call fdirect0(kmatrix, jmatrix, rhomatrix, kscal_lr, imask_exch, antisymmetric, &
                              Kappa, chunk=chunk)
                        call lce_deactivate()
                        if (srexx > ZERO) then
                              !
                              ! Short-range HF-like exchange + portion of long range + Coulomb matrix
                              !
                              call fdirect0(kmatrix, jmatrix, rhomatrix, kscal_full, imask0, antisymmetric, &
                                    ZERO, chunk=chunk)
                        else
                              !
                              ! Coulomb matrix is built separately
                              !
                              call fdirect0(kmatrix, jmatrix, rhomatrix, kscal, imask_coul, antisymmetric, &
                                    ZERO, chunk=chunk)
                        end if
                  else
                        !
                        ! Long-range contribution to the exchange matrix
                        !
                        call lce_activate()
                        call fdirect0(kmatrix, jmatrix, rhomatrix, kscal_lr, imask_exch, antisymmetric, Kappa)
                        call lce_deactivate()
                        if (srexx > ZERO) then
                              !
                              ! Short-range HF-like exchange + portion of long range + Coulomb
                              !
                              call fdirect0(kmatrix, jmatrix, rhomatrix, kscal_full, imask0, antisymmetric, ZERO)
                        else
                              !
                              ! Coulomb matrix is built separately
                              !
                              call fdirect0(kmatrix, jmatrix, rhomatrix, kscal, imask_coul, antisymmetric, ZERO)
                        end if
                  end if
                  !
                  ! Reset variables needed for computing the integrals 
                  ! with ERF(OMEGA*R)/R operator
                  !
                  call lce_unsetlcfunc()
            else
                  if (present(chunk)) then
                        call fdirect0(kmatrix, jmatrix, rhomatrix, kscal, imask0, antisymmetric, ZERO, chunk=chunk)
                  else
                        call fdirect0(kmatrix, jmatrix, rhomatrix, kscal, imask0, antisymmetric, ZERO)
                  end if
            end if

            TIMINGS(TIME_INTS2E) = TIMINGS(TIME_INTS2E) + clock_readwall(t_ints2e)
      end subroutine fdirect


      subroutine fdirect0(kmatrix, jmatrix, rhomatrix, kscal, imask, antisymmetric, Kappa, chunk)
            ! -------------------------------------------------------------------------
            ! Driver subroutine for the direct build of Hartree-Fock exchange
            ! (KMATRIX) and Coulomb (JMATRIX) matrices:
            !
            ! KMatrix(p, q) = KMatrix(p, q) + KScal * Sum_{rs} RhoMatrix(r, s) (qr|ps)
            ! JMatrix(p, q) = JMatrix(p, q) + Sum_{rs} RhoMatrix(r, s) (pq|rs)
            !
            ! Note the order of the pq indices in KMatrix, which is significant in
            ! the case when RhoMatrix is antisymmetric, e.g., for complex-valued
            ! orbitals.
            ! -------------------------------------------------------------------------
            ! KMatrix
            !             HF/hybrid DFT exchange matrix.
            ! JMatrix
            !             Coulomb matrix. Not referenced if SEPKSCONTRIB == .FALSE
            !             or ANTISYMMETRIC == .TRUE.
            !
            ! RhoMatrix
            !             Density matrix in the AO basis. Assumed to 
            !             contain data in the lower and upper triangles. Can be
            !             symmetric or antisymmetric (see ANTISYMMETRIC).
            !
            ! KSCAL
            !             Scaling factor for the HF exchange matrix.
            !             In the case of restricted Hartree-Fock
            !             it should equal -1/2. In the case of a
            !             hybrid Kohn-Sham approximation depends on
            !             the exact-exchange mixing ratio.
            !
            ! IMASK
            !             Bit mask controlling which contributions to the Fock matrix
            !             should be computed
            !
            ! ANTISYMMETRIC
            !             TRUE if the density matrix provided to this subroutine
            !             is antisymmetric. FALSE if the density matrix is symmetric.
            !
            ! CHUNK
            !             Bra pair of atoms determining the chunk of work to be done.
            !             Used for parallelization of work.
            !
            double precision, dimension(:, :), intent(inout) :: kmatrix
            double precision, dimension(:, :), intent(inout) :: jmatrix
            double precision, dimension(:, :), intent(in)    :: rhomatrix
            double precision, intent(in)                     :: kscal
            integer, intent(in)                              :: imask
            logical, intent(in)                              :: antisymmetric
            real(F64), intent(in)                            :: Kappa
            integer, dimension(2), optional, intent(in)      :: chunk
            
            double precision, parameter :: jscal = one
            integer :: mask
            !
            ! Set thresholds for HF exchange and
            ! Coulomb matrices in accordance with
            ! global accuracy level
            !
            if (gacc .eq. ACC_LOW) then
                  call hfexch_setthresh(HFX_THRESH_LOW, abs(kscal))
                  call hfcoul_setthresh(HFC_THRESH_LOW, jscal)
            else
                  call hfexch_setthresh(HFX_THRESH_NORMAL, abs(kscal))
                  call hfcoul_setthresh(HFC_THRESH_NORMAL, jscal)
            end if
            !
            ! Bit mask determining required contributions.
            ! KSCAL factor is ZERO if pure DFT approximation
            ! is requested.
            !
            if (abs(kscal) > ZERO) then
                  mask = COULEXCHSUM
            else
                  mask = COULSUM
            end if

            mask = iand(mask, imask)

            if (mask .eq. 0) return

            call screenrho(rhomatrix, rhomax)
            call screenrhoeri(rhomatrix, rhoerimax, irhoerimax)

            if (present(chunk)) then
                  call fock2eldrv_tile(kmatrix, jmatrix, rhomatrix, rhomax, erimax, ierimax, rhoerimax, &
                        irhoerimax, kscal, jscal, mask, antisymmetric, Kappa, brapair=chunk)
            else
                  call fock2eldrv_tile(kmatrix, jmatrix, rhomatrix, rhomax, erimax, ierimax, rhoerimax, &
                        irhoerimax, kscal, jscal, mask, antisymmetric, Kappa)
            end if
      end subroutine fdirect0
      

      subroutine ufdirect(ksum, kdiff, jmatrix, rhomatrixa, rhomatrixb, &
            work, kscal, lcexch, omega, srexx, antisymmetric, ScreenedHybrid, chunk0, imask)
            ! -----------------------------------------------------------------
            ! Driver subroutine for direct build of Hartree-Fock
            ! exchange and Coulomb matrices (spin unpolarized version).
            ! Use FOCKAB subroutine to compute separate alpha, beta spin
            ! contributions:
            ! KMATRIXA = 1/2 * (KSUM + KDIFF)
            ! KMATRIXB = 1/2 * (KSUM - KDIFF)
            ! -----------------------------------------------------------------
            ! KSUM            - HF exchange matrix (D^\alpha+D^\beta)
            !
            ! KDIFF           - HF exchange matrix (D^\alpha-D^\beta)
            !
            ! JMATRIX         - Coulomb matrix. JMATRIX(P, Q) = 
            !                   \sum_{rs} [RHOMATRIXA(R, S)+RHOMATRIXB(R,S)]
            !                   * (PQ|RS)
            !
            ! RHOMATRIXA      - Alpha density matrix in AO basis. Assumed to 
            !                   contain significant data in lower and
            !                   upper triangles
            !
            ! RHOMATRIXB      - Beta density matrix in AO basis. Assumed to 
            !                   contain significant data in lower and
            !                   upper triangles
            !
            ! WORK            - Matrix for temporary storage
            !
            ! KSCAL           - Scaling factor for HF exchange matrix.
            !                   KMATRIXA(P, Q) = 
            !                   KSCAL * \sum_{rs} RHOMATRIXA(R, S) (QR|PS)
            !                   KMATRIXB(P, Q) = 
            !                   KSCAL * \sum_{rs} RHOMATRIXB(R, S) (QR|PS)
            ! ANTISYMMETRIC
            !                   TRUE if the density matrix provided to this
            !                   subroutine is antisymmetric. FALSE if the
            !                   density matrix is symmetric.
            !
            ! ScreenedHybrid
            !                   Enable range-separated hybrid exchange with
            !                   a fraction of exact exchange at short-range
            !                   and zero exact exchange at long range.
            !
            ! CHUNK0          - Bra pair of atoms which determines
            !                   chunk of work to be done by current
            !                   image
            !
            ! IMASK
            !                   Bit mask defining the classes of two-electron
            !                   integrals to compute
            !
            double precision, dimension(:, :), intent(inout) :: ksum
            double precision, dimension(:, :), intent(inout) :: kdiff
            double precision, dimension(:, :), intent(inout) :: jmatrix
            double precision, dimension(:, :), intent(in)    :: rhomatrixa
            double precision, dimension(:, :), intent(in)    :: rhomatrixb
            double precision, dimension(:, :), intent(out)   :: work
            double precision, intent(in)                     :: kscal
            logical, intent(in)                              :: lcexch
            double precision, intent(in)                     :: omega
            double precision, intent(in)                     :: srexx
            logical, intent(in)                              :: antisymmetric
            logical, intent(in)                              :: ScreenedHybrid
            integer, dimension(2), optional, intent(in)      :: chunk0
            integer, optional, intent(in)                    :: imask
            
            integer :: imask0, imask1
            
            if (present(imask)) then
                  imask0 = imask
            else
                  imask0 = COULEXCHSUM
            end if
            !
            ! Coulomb matrix and K(D^\alpha + D^\beta)
            !
            work = rhomatrixa + rhomatrixb
            if (present(chunk0)) then
                  call fdirect(ksum, jmatrix, work, kscal, &
                        lcexch, omega, srexx, antisymmetric, ScreenedHybrid, &
                        chunk=chunk0, imask=imask0)
            else
                  call fdirect(ksum, jmatrix, work, kscal, &
                        lcexch, omega, srexx, antisymmetric, ScreenedHybrid, &
                        imask=imask0)
            end if
            !
            ! K(D^\alpha - D^\beta)
            !
            work = rhomatrixa - rhomatrixb
            !
            ! Below, JMATRIX is not referenced. Only the exchange contribution
            ! is requested.
            !
            imask1 = iand(EXCHSUM, imask0)
            if (present(chunk0)) then
                  call fdirect(kdiff, jmatrix, work, kscal, &
                        lcexch, omega, srexx, antisymmetric, ScreenedHybrid, &
                        chunk=chunk0, imask=imask1)     
            else
                  call fdirect(kdiff, jmatrix, work, kscal, &
                        lcexch, omega, srexx, antisymmetric, ScreenedHybrid, &
                        imask=imask1)
            end if
      end subroutine ufdirect

      
      subroutine fockab(kmatrixa, kmatrixb)
            !
            ! KSUM  -> KMATRIXA (sum to alpha spin contrib)
            ! KDIFF -> KMATRIXB (difference to beta spin contrib)
            !
            double precision, dimension(:, :), intent(inout) :: kmatrixa
            double precision, dimension(:, :), intent(inout) :: kmatrixb

            double precision :: tsum, tdiff
            integer :: p, q
            !
            ! Build separate spin contributions to the Fock matrix
            !
            do q = 1, NORB
                  do p = q, NORB
                        tsum = kmatrixa(p, q)
                        tdiff = kmatrixb(p, q)
                        kmatrixa(p, q) = (ONE/TWO) * (tsum + tdiff)
                        kmatrixb(p, q) = (ONE/TWO) * (tsum - tdiff)
                  end do
            end do
      end subroutine fockab
end module fbuild
