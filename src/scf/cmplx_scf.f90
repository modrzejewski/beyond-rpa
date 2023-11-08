module cmplx_scf
      use arithmetic
      use math_constants
      use gparam
      use scf
      use real_scf
      use guess
      use cmplx_linalg
      use cmplx_arh
      use basis_sets
      
      implicit none

contains

      subroutine cscf_RhoStart(Rho_ao)
            real(F64), dimension(:, :, :), intent(out) :: Rho_ao

            integer :: m, n
            real(F64) :: r
            real(F64), parameter :: RandomScaling = 1.0E-3_F64

            call guess_atomic(Rho_ao(:, :, 1))
            do n = 1, NORB
                  Rho_ao(n, n, 2) = ZERO
                  do m = n + 1, NORB
                        !
                        ! Because in the complex case Rho is Hermitian, the imaginary part
                        ! is antisymmetric.
                        !
                        call random_number(r)
                        r = r - ONE/TWO
                        Rho_ao(m, n, 2) = RandomScaling * r
                        Rho_ao(n, m, 2) = -RandomScaling * r
                  end do
            end do
      end subroutine cscf_RhoStart
      

      subroutine cscf_TableHeader()
            character(:), allocatable :: line, f1, f2, f3, f4, f5, f6, f7, f8
            
            f1 = lfield("#", 5)
            f2 = cfield("Energy", 21)
            f3 = lfield("EDiff", 9) 
            f4 = lfield("RhoDiff", 9)
            f5 = lfield("OrbGrad", 9)
            f6 = lfield("OrbShift", 9)
            f7 = lfield("NStored", 8)
            f8 = lfield("Time", 9)
            line = f1 // f2 // f3 // f4 // f5 // f6 // f7 // f8
            call midrule(width=78)
            call msg(line)
            call midrule(width=78)
      end subroutine cscf_TableHeader


      subroutine cscf_TableRow(iter, Etot, EDiff, RhoDiff, OrbGrad, OrbShift, NStored, TimeIter, MicroIter)
            integer, intent(in)   :: iter
            real(F64), intent(in) :: Etot
            real(F64), intent(in) :: EDiff
            real(F64), intent(in) :: RhoDiff
            real(F64), intent(in) :: OrbGrad
            real(F64), intent(in) :: OrbShift
            integer, intent(in)   :: NStored
            real(F64), intent(in) :: TimeIter
            logical, intent(in)   :: MicroIter
            
            character(:), allocatable :: line, f1, f7, f8
            character(9) :: f3, f4, f5, f6
            character(21) :: f2

            if (MicroIter) then
                  f1 = lfield(str(iter)//"*", 5)
            else
                  f1 = lfield(str(iter), 5)
            end if
            write(f2, fmt="(F20.10,1X)") Etot
            if (iter == 0) then
                  f3 = ""
                  f4 = ""
                  f5 = ""
                  f6 = ""
            else if (iter == 1) then
                  write(f3, fmt="(ES8.1,1X)") EDiff
                  write(f4, fmt="(ES8.1,1X)") RhoDiff
                  f5 = ""
                  f6 = ""
            else
                  write(f3, fmt="(ES8.1,1X)") EDiff
                  write(f4, fmt="(ES8.1,1X)") RhoDiff
                  write(f5, fmt="(ES8.1,1X)") OrbGrad
                  write(f6, fmt="(ES8.1,1X)") OrbShift
            end if
            f7 = cfield(str(NStored), 8)
            f8 = lfield(str(TimeIter, 1), 9)
            line = f1 // f2 // f3 // f4 // f5 // f6 // f7 // f8
            call msg(line)
      end subroutine cscf_TableRow


      subroutine cscf_parallelism()
            integer :: nthr, nproc
            
            nthr = OMP_NTHREAD
            nproc = max(1, IMG_NALL - 1)
            call blankline()
            call msg("Replicated-data parallel SCF", underline=.true.)
            call imsg("Processes", nproc)
            call imsg("Threads/process", nthr)
            call blankline()
      end subroutine cscf_parallelism


      subroutine cscf_GridDiagnostic(diag, lmgga)
            type(tgriddiag), intent(in) :: diag
            logical, intent(in)         :: lmgga

            call blankline()
            call msg("Grid quality indicators (RhoConv)", underline=.true.)
            call dmsg("Rho", diag%nel, fmt="F10.6")
            call dmsg("Div(Rho)", diag%div, fmt="ES10.3")
            if (lmgga) then
                  call dmsg("Lapl(Rho)", diag%lap, fmt="ES10.3")
            end if
      end subroutine cscf_GridDiagnostic


      subroutine cscf_FF_r(FF_ao, Eel, Exc, diag, xcmodel, Rho_ao, Hbare_ao)
            real(F64), dimension(:, :, :), contiguous, intent(out) :: FF_ao
            real(F64), intent(out)                                 :: Eel
            real(F64), intent(out)                                 :: Exc
            type(tgriddiag), intent(out)                           :: diag
            type(txcdef), intent(in)                               :: xcmodel
            real(F64), dimension(:, :, :), contiguous, intent(in)  :: Rho_ao
            real(F64), dimension(:, :), contiguous, intent(in)     :: Hbare_ao
            
            logical :: rshyb, hartree, bareh, ScreenedHybrid
            real(F64) :: omega, srexx
            integer :: TwoElInts
            real(F64) :: EHbare, EHFIm, EHFTwoEl
            real(F64) :: Exc0, nel0, div0, kin0, lap0
            real(F64) :: XCoeff
            real(F64) :: nel, div, kin, lap
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            real(F64), dimension(1) :: dummy1
            real(F64), dimension(1, 1) :: dummy2
            type(txcdef) :: HFExchIm

            FF_ao = ZERO
            rshyb = xcf_get_flag(xcmodel, XCF_RSHYB)
            ScreenedHybrid = xcf_get_flag(xcmodel, XCF_SCREENED_HYBRID)
            if (rshyb .or. ScreenedHybrid) then
                  omega = xcf_get_omega(xcmodel)
                  if (omega < ZERO) then
                        call dmsg("Invalid value of the range-separation parameter", &
                              xcf_get_omega(xcmodel), priority=MSG_ERROR)
                        stop
                  end if
                  srexx = xcf_get_srexx(xcmodel)
            else
                  omega = -ONE
                  srexx = -ONE
            end if
            hartree = xcf_get_flag(xcmodel, XCF_HARTREE)
            if (hartree) then
                  TwoElInts = COULEXCHSUM
            else
                  TwoElInts = EXCHSUM
            end if
            bareh = xcf_get_flag(xcmodel, XCF_BAREH)
            XCoeff = -(ONE/TWO) * xcf_get_exx(xcmodel)
            !
            ! Define the exact HF exchange component of the exchange-correlation
            ! functional. This definition is used in a separate calculation
            ! which depends only on the imaginary (antisymmetric) part of the density
            ! matrix.
            !
            call xcf_define(HFExchIm, XCF_HF, AUX_NONE, .false.)
            call xcf_set_exx(HFExchIm, xcf_get_exx(xcmodel))
            if (xcf_get_flag(xcmodel, XCF_RSHYB)) then
                  call xcf_set_flag(HFExchIm, XCF_RSHYB, .true.)
                  call xcf_set_omega(HFExchIm, xcf_get_omega(xcmodel))
                  call xcf_set_srexx(HFExchIm, xcf_get_srexx(xcmodel))
            end if
            call xcf_set_flag(HFExchIm, XCF_BAREH, .false.)
            call xcf_set_flag(HFExchIm, XCF_HARTREE, .false.)
            call xcf_set_flag(HFExchIm, XCF_IMAGINARY_DENSITY, .true.)
            if (IMG_ENABLED) then
                  ! --------------------------------------------------------------
                  !      Real part of the HF exchange operator + DFT components
                  ! --------------------------------------------------------------
                  outmsg(1) = IMG_MSG_NEXTITER
                  call img_toslaves(outmsg)
                  call sendxcdef(xcmodel)
                  call img_smtoslaves(Rho_ao(:, :, 1))
                  call img_ksbalance(xcf_numint(xcmodel))
                  outmsg(1) = IMG_MSG_ITERDONE
                  call img_toslaves(outmsg)
                  if (bareh) then
                        call rhotrace(EHbare, Rho_ao(:, :, 1), Hbare_ao)
                        FF_ao(:, :, 1) = Hbare_ao
                  else
                        EHbare = ZERO
                  end if
                  Eel = EHbare
                  Exc = ZERO
                  nel = ZERO
                  div = ZERO
                  kin = ZERO
                  lap = ZERO
                  call img_ksresults(FF_ao(:, :, 1), Eel, Exc, nel, div, kin, lap)
                  ! -------------------------------------------------------------
                  !          Imaginary part of the HF exchange operator
                  ! -------------------------------------------------------------
                  outmsg(1) = IMG_MSG_NEXTITER
                  call img_toslaves(outmsg)
                  call sendxcdef(HFExchIm)
                  call img_smtoslaves(Rho_ao(:, :, 2))
                  call img_ksbalance(.false.)
                  outmsg(1) = IMG_MSG_ITERDONE
                  call img_toslaves(outmsg)
                  EHFIm = ZERO
                  Exc0 = ZERO
                  nel0 = ZERO
                  div0 = ZERO
                  kin0 = ZERO
                  lap0 = ZERO
                  call img_ksresults(FF_ao(:, :, 2), EHFIm, Exc0, nel0, div0, kin0, lap0)
                  Eel = Eel + EHFIm
            else
                  ! --------------------------------------------------------------
                  !      Real part of the HF exchange operator + DFT components
                  ! --------------------------------------------------------------
                  call fdirect(FF_ao(:, :, 1), FF_ao(:, :, 1), Rho_ao(:, :, 1), XCoeff, &
                        rshyb, omega, srexx, .false., ScreenedHybrid, imask=TwoElInts)
                  call rhotrace(EHFTwoEl, Rho_ao(:, :, 1), FF_ao(:, :, 1))
                  EHFTwoEl = (ONE/TWO) * EHFTwoEl
                  if (bareh) then
                        call rhotrace(EHbare, Rho_ao(:, :, 1), Hbare_ao)
                        FF_ao(:, :, 1) = FF_ao(:, :, 1) + Hbare_ao
                  else
                        EHbare = ZERO
                  end if
                  if (xcf_numint(xcmodel)) then
                        call xcorr(xcmodel, FF_ao(:, :, 1), FF_ao(:, :, 1), Rho_ao(:, :, 1), Rho_ao(:, :, 1), &
                              Exc, nel, div, kin, lap, dummy1, dummy2)
                  else
                        Exc = ZERO
                  end if
                  ! -------------------------------------------------------------
                  !          Imaginary part of the HF exchange operator
                  ! -------------------------------------------------------------
                  if (abs(XCoeff) > ZERO) then
                        call fdirect(FF_ao(:, :, 2), FF_ao(:, :, 2), Rho_ao(:, :, 2), XCoeff, &
                              rshyb, omega, srexx, .true., ScreenedHybrid, imask=TwoElInts)
                        call rhotrace(EHFIm, Rho_ao(:, :, 2), FF_ao(:, :, 2))
                        EHFIm = -(ONE/TWO) * EHFIm
                  else
                        EHFIm = ZERO
                  end if
                  Eel = EHbare + EHFTwoEl + Exc + EHFIm



                  print *, "EHFIm = ", EHFIm

                  
            end if
            
            if (xcf_numint(xcmodel)) then
                  diag%nel = nel
                  diag%div = div
                  diag%kin = kin
                  diag%lap = lap
            else
                  diag%nel = ZERO
                  diag%div = ZERO
                  diag%kin = ZERO
                  diag%lap = ZERO
            end if
      end subroutine cscf_FF_r
      

      function cscf_RhoDiff(RhoK, RhoN)
            real(F64) :: cscf_RhoDiff
            real(F64), dimension(:, :, :), intent(in) :: RhoK
            real(F64), dimension(:, :, :), intent(in) :: RhoN

            integer :: m, n, p, q
            real(F64) :: t_re, t_im, t

            m = size(RhoK, dim=1)
            n = size(RhoK, dim=2)
            t = ZERO
            do q = 1, n
                  do p = 1, m
                        t_re = RhoK(p, q, 1) - RhoN(p, q, 1)
                        t_im = RhoK(p, q, 2) - RhoN(p, q, 2)
                        t = max(t, hypot(t_re, t_im))
                  end do
            end do
            cscf_RhoDiff = t
      end function cscf_RhoDiff

      
      subroutine atba_transform(c, a, b, scratch)
            real(F64), dimension(:, :), contiguous, intent(out) :: c
            real(F64), dimension(:, :), contiguous, intent(in)  :: a
            real(F64), dimension(:, :), contiguous, intent(in)  :: b
            real(F64), dimension(:, :), contiguous, intent(out) :: scratch

            integer :: n, k
            integer :: lda, ldb, ldc
            external :: dgemm

            n = size(a, dim=2)
            k = size(b, dim=2)
            lda = size(a, dim=1)
            ldb = size(b, dim=1)
            ldc = size(c, dim=1)

            if ((size(c, dim=1) .ne. size(a, dim=2)) .or. &
                  (size(a, dim=1) .ne. size(b, dim=2)) .or. &
                  (size(b, dim=1) .ne. size(b, dim=2))) then
                  call msg("Inconsistent dimensions on entry to ATBA_TRANSFORM", MSG_ERROR)
                  stop
            end if

            if (size(scratch) < n * k) then
                  call msg("Scratch too small for ATBA_TRANSFORM ", MSG_ERROR)
                  stop
            end if
            !
            ! SCRATCH <- A^T B
            !
            call dgemm("T", "N", n, k, k, ONE, a, lda, b, ldb, ZERO, scratch, n)
            !
            ! C <- SCRATCH A
            !
            call dgemm("N", "N", n, n, k, ONE, scratch, n, a, lda, ZERO, c, ldc)
      end subroutine atba_transform


      subroutine abat_transform(c, a, b, scratch)
            real(F64), dimension(:, :), contiguous, intent(out) :: c
            real(F64), dimension(:, :), contiguous, intent(in)  :: a
            real(F64), dimension(:, :), contiguous, intent(in)  :: b
            real(F64), dimension(:, :), contiguous, intent(out) :: scratch

            integer :: n, k
            integer :: lda, ldb, ldc
            external :: dgemm

            scratch = ZERO
            c = ZERO
            n = size(a, dim=1)
            k = size(b, dim=1)
            lda = size(a, dim=1)
            ldb = size(b, dim=1)
            ldc = size(c, dim=1)

            if ((size(c, dim=1) .ne. size(a, dim=1)) .or. &
                  (size(a, dim=2) .ne. size(b, dim=2)) .or. &
                  (size(b, dim=1) .ne. size(b, dim=2))) then
                  call msg("Inconsistent dimensions on entry to ABAT_TRANSFORM", MSG_ERROR)
                  stop
            end if

            if (size(scratch) < n * k) then
                  call msg("Scratch too small for ABAT_TRANSFORM", MSG_ERROR)
                  stop
            end if
            !
            ! SCRATCH <- B A^T
            !
            call dgemm("N", "T", k, n, k, ONE, b, ldb, a, lda, ZERO, scratch, k)
            !
            ! C <- A SCRATCH
            !
            call dgemm("N", "N", n, n, k, ONE, a, lda, scratch, k, ZERO, c, ldc)
      end subroutine abat_transform


      subroutine split_cmplx(a, b)
            real(F64), dimension(:, :, :), intent(out) :: a
            complex(F64), dimension(:, :), intent(in)  :: b

            integer :: m, n, p, q

            m = size(b, dim=1)
            n = size(b, dim=2)
            do q = 1, n
                  do p = 1, m
                        a(p, q, 1) = real(b(p, q))
                        a(p, q, 2) = aimag(b(p, q))
                  end do
            end do
      end subroutine split_cmplx


      subroutine join_cmplx(a, b)
            complex(F64), dimension(:, :), intent(out) :: a
            real(F64), dimension(:, :, :), intent(in)  :: b

            integer :: m, n, p, q

            m = size(b, dim=1)
            n = size(b, dim=2)
            do q = 1, n
                  do p = 1, m
                        a(p, q) = cmplx(b(p, q, 1), b(p, q, 2), kind=F64)
                  end do
            end do
      end subroutine join_cmplx


      subroutine cscf_iters_r(RhoN_ao, eigenvals, converged, Etot, Exc, xcmodel, Nocc, &
            AOBasis, lindep, Enucl, MaxRhoDiff, MaxOrbGrad, MaxNIters)
            !
            ! Main loop of the complex-orbital self-consistent field KS/DFT computations
            !
            real(F64), dimension(:, :, :), contiguous, intent(inout) :: RhoN_ao
            real(F64), dimension(:), allocatable, intent(out)        :: eigenvals
            logical, intent(out)                                     :: converged
            real(F64), intent(out)                                   :: Etot
            real(F64), intent(out)                                   :: Exc
            type(txcdef), intent(in)                                 :: xcmodel
            integer, intent(in)                                      :: Nocc
            type(TAOBasis), intent(in)                               :: AOBasis
            real(F64), intent(in)                                    :: lindep
            real(F64), intent(in)                                    :: Enucl
            real(F64), intent(in)                                    :: MaxRhoDiff
            real(F64), intent(in)                                    :: MaxOrbGrad
            integer, intent(in)                                      :: MaxNIters

            type(tgriddiag) :: gdiag
            real(F64), dimension(:, :, :), allocatable :: FFn_ao, FFn_oao, RhoK_ao, DDn_oao
            complex(F64), dimension(:, :), allocatable :: Dn_oao, Fn_oao, Cn_oao, Ck_oao
            real(F64), dimension(:, :), allocatable :: Hbare_ao, S_ao, Ts_ao, Vne_ao
            real(F64), dimension(:, :), allocatable :: transf_work
            real(F64), dimension(:, :), allocatable :: BasisVecs_cao
            real(F64), dimension(:, :), allocatable :: BasisVecs_sao
            integer :: Noao
            real(F64) :: RhoDiff, EDiff, OrbGrad, OrbShift
            real(F64) :: EtotN, EtotK, EelN, EelK, ExcN, ExcK
            type(carhdata) :: arh_data
            integer :: NIters, NMicroIters, NStored
            logical :: AcceptAllCn, DoMicroIters
            type(tclock) :: t_iter, t_total
            real(F64) :: TimeIter
            integer, parameter :: MaxNMicroIters = 10
            real(F64), parameter :: MinOrbGrad = SCF_DISABLE_ARH_SHIFT_UNPOLAR
            real(F64), parameter :: OccupNum = TWO

            call clock_start(t_total)
            call clock_start(t_iter)
            call msg("Starting complex-orbital self-consistent field (spin-restricted)")
            call cscf_parallelism()
            
            allocate(S_ao(NORB, NORB))
            allocate(Ts_ao(NORB, NORB))
            allocate(Vne_ao(NORB, NORB))
            allocate(Hbare_ao(NORB, NORB))
            call stv(S_ao, Ts_ao, Vne_ao)
            call smfill(S_ao)
            Hbare_ao = Ts_ao + Vne_ao
            !
            ! Compute the orthogonal basis
            !
            call basis_OAO(BasisVecs_cao, BasisVecs_sao, S_ao, AOBasis, lindep)
            !
            ! Dimension of the orthogonal linearly-independent basis.
            ! Takes into account the transformation to the spherical
            ! harmonics basis.
            !
            Noao = size(BasisVecs_cao, dim=2)
            allocate(eigenvals(Noao))
            allocate(transf_work(Noao, NORB))
            allocate(FFn_ao(NORB, NORB, 2))
            allocate(FFn_oao(Noao, Noao, 2))
            allocate(Fn_oao(Noao, Noao))
            allocate(Cn_oao(Noao, Noao))
            allocate(Ck_oao(Noao, Noao))
            allocate(Dn_oao(Noao, Noao))
            allocate(DDn_oao(Noao, Noao, 2))
            allocate(RhoK_ao(NORB, NORB, 2))
            ! --------------------------------------------------------------------
            !    Compute the initial Kohn-Sham/Fock matrix Fk := F(RhoK)
            !    from the guess AO density matrix. Diagonalize Fk to get the
            !    initial set of MO vectors in the OAO basis (Cn_oao).
            ! --------------------------------------------------------------------
            RhoK_ao = RhoN_ao
            call cscf_FF_r(FFn_ao, EelK, ExcK, gdiag, xcmodel, RhoK_ao, Hbare_ao)
            EtotK = EelK + Enucl
            call smfill(FFn_ao(:, :, 1))
            call amfill(FFn_ao(:, :, 2))
            !
            ! Because BASISVECS_CAO is real, we transform the real and imaginary components
            ! of Fn_ao separately. As a result, the computational cost is 4n**3 instead 8n**3.
            !
            call atba_transform(FFn_oao(:, :, 1), BasisVecs_cao, FFn_ao(:, :, 1), transf_work)
            call atba_transform(FFn_oao(:, :, 2), BasisVecs_cao, FFn_ao(:, :, 2), transf_work)
            call join_cmplx(Fn_oao, FFn_oao)
            Cn_oao = Fn_oao
            call hermitian_eigenproblem(eigenvals, Cn_oao, Noao, .true.)
            call cmplx_abH(Dn_oao, Cn_oao(:, 1:Nocc), Cn_oao(:, 1:Nocc))
            call split_cmplx(DDn_oao, Dn_oao)
            call abat_transform(RhoN_ao(:, :, 1), BasisVecs_cao, DDn_oao(:, :, 1), transf_work)
            call abat_transform(RhoN_ao(:, :, 2), BasisVecs_cao, DDn_oao(:, :, 2), transf_work)
            RhoN_ao = OccupNum * RhoN_ao
            TimeIter = clock_readwall(t_iter)
            call cscf_TableHeader()
            call cscf_TableRow(0, EtotK, ZERO, ZERO, ZERO, ZERO, 0, TimeIter, .false.)
            !
            ! Initialize the data structure for the SCF algorithm
            !
            call carh_init(arh_data, OccupNum, [1, Nocc], [Nocc+1, Noao], Noao)
            ! --------------------------------------------------------------------
            !     Build Fn := F(RhoN) to start the proper iterative process
            ! --------------------------------------------------------------------
            call clock_start(t_iter)
            call cscf_FF_r(FFn_ao, EelN, ExcN, gdiag, xcmodel, RhoN_ao, Hbare_ao)
            call smfill(FFn_ao(:, :, 1))
            call amfill(FFn_ao(:, :, 2))
            call atba_transform(FFn_oao(:, :, 1), BasisVecs_cao, FFn_ao(:, :, 1), transf_work)
            call atba_transform(FFn_oao(:, :, 2), BasisVecs_cao, FFn_ao(:, :, 2), transf_work)
            call join_cmplx(Fn_oao, FFn_oao)
            EtotN = EelN + Enucl
            RhoDiff = cscf_RhoDiff(RhoK_ao, RhoN_ao)
            EDiff = EtotN - EtotK
            TimeIter = clock_readwall(t_iter)
            call cscf_TableRow(1, EtotN, EDiff, RhoDiff, ZERO, ZERO, 0, TimeIter, .false.)
            Ck_oao = Cn_oao
            RhoK_ao = RhoN_ao
            EtotK = EtotN
            EelK = EelN
            ExcK = ExcN
            AcceptAllCn = .false.
            converged = .false.
            MacroIters: do NIters = 2, MaxNIters
                  NMicroIters = 0
                  DoMicroIters = .true.
                  do while (DoMicroIters)
                        NMicroIters = NMicroIters + 1
                        call clock_start(t_iter)
                        !
                        ! Compute updated molecular orbitals and density matrix
                        !
                        call carh_NextIter(arh_data, Cn_oao, OrbGrad, OrbShift, NStored, Fn_oao, EelK, (NMicroiters>1))
                        call cmplx_abH(Dn_oao, Cn_oao(:, 1:Nocc), Cn_oao(:, 1:Nocc))
                        call split_cmplx(DDn_oao, Dn_oao)
                        call abat_transform(RhoN_ao(:, :, 1), BasisVecs_cao, DDn_oao(:, :, 1), transf_work)
                        call abat_transform(RhoN_ao(:, :, 2), BasisVecs_cao, DDn_oao(:, :, 2), transf_work)
                        RhoN_ao = OccupNum * RhoN_ao
                        !
                        ! Use the new density matrix to compute the Kohn-Sham/Fock matrix
                        !
                        call cscf_FF_r(FFn_ao, EelN, ExcN, gdiag, xcmodel, RhoN_ao, Hbare_ao)
                        call smfill(FFn_ao(:, :, 1))
                        call amfill(FFn_ao(:, :, 2))
                        EtotN = EelN + Enucl
                        !
                        ! Measures of convergence: max norm of the orbital gradient
                        ! matrix, max difference between the old and new AO density
                        ! matrices, and total energy difference
                        !
                        RhoDiff = cscf_RhoDiff(RhoK_ao, RhoN_ao)
                        EDiff = EtotN - EtotK
                        if (OrbGrad >= MinOrbGrad) then
                              call carh_TrustRadius(arh_data, EelN, EelK)
                        else
                              !
                              ! During the final stages of the optimization
                              ! the trust radius isn't updated.
                              !
                              AcceptAllCn = .true.
                              call carh_DisableShift(arh_data)
                        end if
                        if (EDiff < ZERO .or. NMicroIters > MaxNMicroIters .or. AcceptAllCn) then
                              DoMicroIters = .false.
                        else
                              Cn_oao = Ck_oao
                        end if
                        TimeIter = clock_readwall(t_iter)
                        call cscf_TableRow(NIters, EtotN, EDiff, RhoDiff, OrbGrad, &
                              abs(OrbShift), NStored, TimeIter, (NMicroIters>1))
                  end do
                  call atba_transform(FFn_oao(:, :, 1), BasisVecs_cao, FFn_ao(:, :, 1), transf_work)
                  call atba_transform(FFn_oao(:, :, 2), BasisVecs_cao, FFn_ao(:, :, 2), transf_work)
                  call join_cmplx(Fn_oao, FFn_oao)
                  EtotK = EtotN
                  EelK = EelN
                  ExcK = ExcN
                  Ck_oao = Cn_oao
                  RhoK_ao = RhoN_ao
                  if (RhoDiff < MaxRhoDiff .and. OrbGrad < MaxOrbGrad) then
                        converged = .true.
                        exit MacroIters
                  end if
            end do MacroIters
            Etot = EtotN
            Exc = ExcN
            call hermitian_eigenproblem(eigenvals, Fn_oao, Noao, .true.)
            if (converged) then
                  call toprule()
                  call msg("SCF converged")
                  call dmsg("Converged energy", Etot)
                  call midrule()
            else
                  call toprule()
                  call msg("SCF not converged")
                  call dmsg("Energy (last iteration)", Etot)
                  call midrule()
            end if
            call carh_free(arh_data)
      end subroutine cscf_iters_r


      subroutine cscf_r_driver(SCFResults, SCFParams, AOBasis)
            !
            ! Driver routine for complex-orbital spin-restricted SCF
            !
            type(TSCFOutput), intent(out) :: SCFResults
            type(TSCFParams), intent(in)  :: SCFParams
            type(TAOBasis), intent(in)    :: AOBasis

            type(txcdef) :: xcmodel
            real(F64), dimension(:, :, :), allocatable :: Rho_ao
            real(F64), dimension(:), allocatable :: eigenvals
            real(F64) :: MaxRhoDiff, MaxOrbGrad, LinearDeps
            real(F64) :: Etot, Exc, Enucl
            integer :: Nocc, MaxNIters
            logical :: converged

            allocate(Rho_ao(NORB, NORB, 2))
            allocate(eigenvals(NORB))

            call xcf_define(xcmodel, SCFParams%xcfunc, AUX_NONE, .false.)
            !
            ! Check if user defined non-default values for the range-separation
            ! parameter and/or fraction of the short-range HF exchange
            !
            if (.not. LCOMEGA < ZERO) call xcf_set_omega(xcmodel, LCOMEGA)
            if (.not. LCSREXX < ZERO) call xcf_set_srexx(xcmodel, LCSREXX)
            !
            ! Convergence thresholds
            !
            if (SCF_THRESH_DENSITY < ZERO) then
                  if (xcf_get_id(xcmodel) == XCF_HF) then
                        MaxRhoDiff = SCF_THRESH_DENSITY_DEFAULT_WF
                  else
                        MaxRhoDiff = SCF_THRESH_DENSITY_DEFAULT_DFT
                  end if
            else
                  MaxRhoDiff = SCF_THRESH_DENSITY
            end if
            if (SCF_THRESH_GRADIENT < ZERO) then
                  MaxOrbGrad = SCF_THRESH_GRADIENT_DEFAULT
            else
                  MaxOrbGrad = SCF_THRESH_GRADIENT
            end if
            LinearDeps = LINDEP_THRESH
            MaxNIters = SCF_MAXIT
            Nocc = NE / 2
            Enucl = nuclrep()
            call cscf_RhoStart(Rho_ao)
            call cscf_iters_r(Rho_ao, eigenvals, converged, Etot, Exc, xcmodel, Nocc, &
                  AOBasis, LinearDeps, Enucl, MaxRhoDiff, MaxOrbGrad, MaxNIters)
      end subroutine cscf_r_driver










      ! subroutine antisymmetric_Fx(vx_ref, density_matrix, XCoeff)
      !       real(F64), dimension(:, :), intent(out) :: vx_ref
      !       real(F64), dimension(:, :), intent(in)  :: density_matrix
      !       real(F64), intent(in) :: XCoeff

            
      !       integer :: a, b, c, d
      !       integer :: p, q, r, s
      !       integer :: p0, q0, p1, q1, r0, r1, s0, s1
      !       integer :: sa, sb, sc, sd
      !       integer :: na, nb, nc, nd
      !       integer :: ka, kb, kc, kd
      !       integer :: v

      !       real(F64), dimension(MAX_NFUNC**4) :: gabcd

      !       real(F64) :: eel, exc
      !       type(txcdef) :: bare_exchange
      !       real(F64), dimension(1) :: dummy1
      !       real(F64), dimension(1, 1) :: dummy2
      !       type(tgriddiag) :: diag

      !       real(F64) :: max_abs_error

      !       vx_ref = ZERO

      !       do b = 1, NSHELL
      !             do a = 1, NSHELL
      !                   p0 = SHPOS(a)
      !                   q0 = SHPOS(b)
      !                   sa = SH(a)
      !                   sb = SH(b)
      !                   na = nfunc(SHTYPE(sa))
      !                   nb = nfunc(SHTYPE(sb))
      !                   p1 = p0 + na - 1
      !                   q1 = q0 + nb - 1
      !                   ka = SHATOM(a)
      !                   kb = SHATOM(b)
      !                   do d = 1, NSHELL
      !                         do c = 1, NSHELL
      !                               r0 = SHPOS(c)
      !                               s0 = SHPOS(d)
      !                               sc = SH(c)
      !                               sd = SH(d)
      !                               nc = nfunc(SHTYPE(sc))
      !                               nd = nfunc(SHTYPE(sd))
      !                               r1 = r0 + nc - 1
      !                               s1 = s0 + nd - 1
      !                               kc = SHATOM(c)
      !                               kd = SHATOM(d)
      !                               !
      !                               ! Compute two-electron integrals (AC|BD)
      !                               !
      !                               call ints2e(sa, ka, sc, kc, sb, kb, sd, kd, gabcd)
      !                               v = 1
      !                               do p = p0, p1
      !                                     do r = r0, r1
      !                                           do q = q0, q1
      !                                                 do s = s0, s1
      !                                                       !
      !                                                       ! V_x(P, Q) <-- V_x(P, Q) + D(R, S) (PR|QS)
      !                                                       !
      !                                                       vx_ref(p, q) = vx_ref(p, q) + density_matrix(r, s) * gabcd(v)
      !                                                       v = v + 1
      !                                                 end do
      !                                           end do
      !                                     end do
      !                               end do
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       vx_ref = XCoeff * vx_ref
      ! end subroutine antisymmetric_Fx
end module cmplx_scf
