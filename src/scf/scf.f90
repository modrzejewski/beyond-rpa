module scf
      use arithmetic
      use math_constants
      use gparam
      use clock
      use io
      use images
      use gto
      use basis
      use display
      use linalg
      use ints
      use screen
      use hfexch
      use hfcoul
      use xcfunc
      use scfutils
      use arh
      use multipole
      use fbuild
      use dftd3
      use guess
      use lcexch
      use ecpint
      use nuclfield
      use roks
      use auxint
      use voldata
      use symmetry
      use h_xcfunc
      use scf_definitions
      use mbd
      use hirshfeld
      use real_linalg
      !$ use omp_lib

      implicit none
      !
      ! Matrices returned by the SCF subprogram
      ! ---
      ! Return no matrices
      !
      integer, parameter :: SCF_OUT_NONE       = 0
      !
      ! Return molecular orbitals in AO basis and orbital energies
      !
      integer, parameter :: SCF_OUT_MOCOEFF_AO = 1
      !
      ! Return total electronic density in AO basis
      !
      integer, parameter :: SCF_OUT_RHO_AO     = 2

contains

      subroutine scftable_start()
            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY RHODIFF EDIFF SHIFT NARH TIME
            !
            write(line, "(A3,1X,A20,1X,A8,1X,A8,1X,A8,2X,A4,1X,A8,1X,A8)") &
                  "#", "ENERGY", "RHODIFF", "EDIFF", "SHIFT", "NARH", "GRAD", "TIME"
            call midrule(width=76)
            call msg(line)
            call midrule(width=76)
      end subroutine scftable_start

      
      subroutine scftable_continue(iter, energy, rhodiff, ediff, shift, narh, orb_grad, time)
            integer, intent(in)   :: iter
            real(F64), intent(in) :: energy
            real(F64), intent(in) :: rhodiff
            real(F64), intent(in) :: ediff
            real(F64), intent(in) :: shift
            integer, intent(in)   :: narh
            real(F64), intent(in) :: orb_grad
            real(F64), intent(in) :: time
            
            character(len=DEFLEN) :: line
            !
            ! ITER ENERGY RHODIFF EDIFF SHIFT NARH TIME
            !
            write(line, "(I3,1X,F20.10,1X,ES8.1,1X,ES8.1,1X,ES8.1,2X,I4,1X,ES8.1,1X,ES8.1)") &
                  iter, energy, rhodiff, ediff, shift, narh, orb_grad, time
            call msg(line)
      end subroutine scftable_continue


      subroutine scf_parallelism()
            integer :: nthr, nproc
            
            nthr = OMP_NTHREAD
            nproc = max(1, IMG_NALL - 1)
            
            call blankline()
            call msg("REPLICATED DATA PARALLEL SCF", underline=.true.)
            call imsg("WORKING PROCESSES", nproc)
            call imsg("THREADS PER WORKING PROCESS", nthr)
            call blankline()
      end subroutine scf_parallelism


      subroutine displaygriddiag(diag, lmgga)
            type(tgriddiag), intent(in) :: diag
            logical, intent(in)         :: lmgga

            call blankline()
            call msg("GRID DIAGNOSTICS (LAST ITERATION)", underline=.true.)
            call dmsg("RHO", diag%nel, fmt="F10.6")
            call dmsg("DIV(RHO)", diag%div, fmt="ES10.3")
            if (lmgga) then
                  call dmsg("LAPL(RHO)", diag%lap, fmt="ES10.3")
            end if
      end subroutine displaygriddiag


      subroutine stv_storeints(a, b, i, j, m, n)
            real(F64), dimension(:, :), intent(inout) :: a
            real(F64), dimension(:), intent(in) :: b
            integer, intent(in) :: i, j, m, n

            integer :: k, l
            integer :: v

            v = 1
            do l = 0, n - 1
                  do k = 0, m - 1
                        a(i + k, j + l) = a(i + k, j + l) + b(v)
                        v = v + 1
                  end do
            end do
      end subroutine stv_storeints


      subroutine stv_shellloop(s, t, v, ka, kb)
            real(F64), dimension(:, :), intent(inout) :: s, t, v
            integer, intent(in) :: ka, kb

            real(F64), dimension(max_nfunc**2) :: sab, tab, vab
            integer :: shell1, shell2
            integer :: momentum1, momentum2
            integer :: nint1, nint2
            integer :: la, lb
            integer :: idx1, idx2

            shella: do la = sh0(ka), sh0(ka + 1) - 1
                  shell1 = sh(la)
                  momentum1 = shtype(shell1)
                  nint1 = nfunc(momentum1)
                  idx1 = shpos(la)
                  lb = sh0(kb)
                  shellb: do while ((lb .lt. sh0(kb + 1)) .and. (lb .le. la))
                        shell2 = sh(lb)
                        momentum2 = shtype(shell2)
                        nint2 = nfunc(momentum2)
                        idx2 = shpos(lb)
                        !
                        ! Note order of arguments passed to one-electron integrals
                        ! subroutine. It is transposed due to way STV_STOREINTS
                        ! works.
                        !
                        call ints1e(shell2, kb, shell1, ka, sab, tab, vab)

                        call stv_storeints(s, sab, idx1, idx2, nint1, nint2)
                        call stv_storeints(t, tab, idx1, idx2, nint1, nint2)
                        call stv_storeints(v, vab, idx1, idx2, nint1, nint2)

                        lb = lb + 1
                  end do shellb
            end do shella
      end subroutine stv_shellloop


      subroutine stv(s, t, v)
            real(F64), dimension(:, :), intent(out) :: s, t, v

            integer :: ka, kb
            integer :: q
            integer, parameter :: w = 15
            integer, parameter :: d = 3

            s = ZERO
            t = ZERO
            v = ZERO

            if (POINT_CHARGES_N > 0) then
                  call blankline()
                  call msg(str(POINT_CHARGES_N) // " point charges will be added to the external potential")
                  call msg(lfield("charge", w) // lfield("x [a.u.]", w) // lfield("y [a.u.]", w) // lfield("z [a.u.]", w))
                  do q = 1, POINT_CHARGES_N
                        call msg(lfield(str(POINT_CHARGES_Q(q), d), w) // lfield(str(POINT_CHARGES_R(1, q), d), w) &
                              // lfield(str(POINT_CHARGES_R(2, q), d), w) // lfield(str(POINT_CHARGES_R(3, q), d), w))
                  end do
                  call blankline()
            end if

            atoma: do ka = 1, natom
                  atomb: do kb = 1, ka
                        call stv_shellloop(s, t, v, ka, kb)
                  end do atomb
            end do atoma
      end subroutine stv


      subroutine ksgen_slave()
            type(txcdef) :: ixc
            real(F64), dimension(:, :), allocatable :: ksmatrixa
            real(F64), dimension(:, :), allocatable :: ksmatrixb
            real(F64), dimension(:, :), allocatable :: kmatrixa
            real(F64), dimension(:, :), allocatable :: kmatrixb
            real(F64), dimension(:, :), allocatable :: jmatrix
            real(F64), dimension(:, :), allocatable :: ksmatrix
            real(F64), dimension(:, :), allocatable :: fockmatrix
            real(F64), dimension(:, :), allocatable :: rhoa_ao
            real(F64), dimension(:, :), allocatable :: rhob_ao
            real(F64), dimension(:, :), allocatable :: rho_ao
            real(F64), dimension(:, :), allocatable :: work
            logical :: lpolrbuild
            logical :: done_fock, done_xc
            integer :: inttypes
            integer, dimension(IMG_IMSGSIZE) :: task
            integer :: mainmsg
            integer, dimension(2) :: focktask, xctask
            real(F64) :: eel, exc, exc0
            real(F64) :: nel, div, nel0, div0
            real(F64) :: kin, lap, kin0, lap0
            integer :: aux_id
            real(F64), dimension(:, :), allocatable :: aux_matrix_input
            real(F64), dimension(:), allocatable :: aux, aux0
            integer :: nauxint
            real(F64) :: exa, exb, ecoul
            real(F64) :: hfxfrac, ksxscal
            logical :: rshyb, hartree, imaginary, ScreenedHybrid
            real(F64) :: omega, srexx
            integer :: m, n
            real(F64), parameter :: hfxscal_pol = -ONE
            real(F64), parameter :: hfxscal_unpol = -FRAC12

            if (ROKS_ENABLED) then
                  allocate(ksmatrixa(NORB, NORB))
                  allocate(ksmatrixb(NORB, NORB))
                  allocate(kmatrixa(NORB, NORB))
                  allocate(kmatrixb(NORB, NORB))
                  allocate(jmatrix(NORB, NORB))
                  allocate(rhoa_ao(NORB, NORB))
                  allocate(rhob_ao(NORB, NORB))
                  allocate(work(NORB, NORB))
            end if

            allocate(fockmatrix(NORB, NORB))
            allocate(ksmatrix(NORB, NORB))
            allocate(rho_ao(NORB, NORB))

            nauxint = 0
            allocate(aux(0))
            allocate(aux0(0))

            done_xc = .false.
            done_fock = .false.
            lpolrbuild = .false.
            hartree = .false.
            ScreenedHybrid = .false.
            aux_id = AUX_NONE
            
            scfloop: do
                  !
                  ! Receive next message from the master image
                  !                 
                  call img_frommaster(task)
                  mainmsg = task(1)

                  if (iand(mainmsg, IMG_MSG_NEXTITER) .gt. 0) then
                        !
                        ! Initialize next KS build (next SCF iteration)
                        ! ---
                        ! Receive the definition of an XC functional and/or
                        ! an auxiliary integral for the current KS build. Different
                        ! XC functionals for different SCF iterations are admissible.
                        ! 
                        !
                        call recvxcdef(ixc)
                        aux_id = aux_get_id(ixc)

                        imaginary = xcf_get_flag(ixc, XCF_IMAGINARY_DENSITY)
                        if (imaginary) then
                              call msg("Error: Parallelization not available for complex-orbital SCF", MSG_ERROR)
                              stop
                        end if
                        ScreenedHybrid = xcf_get_flag(ixc, XCF_SCREENED_HYBRID)
                        rshyb = xcf_get_flag(ixc, XCF_RSHYB)
                        if (rshyb) then
                              omega = xcf_get_omega(ixc)
                              srexx = xcf_get_srexx(ixc)
                        else
                              omega = -ONE
                              srexx = -ONE
                        end if
                        hfxfrac = xcf_get_exx(ixc)
                        !
                        ! Test if Hartree term is requested
                        !
                        hartree = (xcf_get_flag(ixc, XCF_HARTREE) .and. .not. imaginary)
                        !
                        ! Receive density matrices
                        ! from the master image
                        !
                        if (xcf_isuncomp(ixc)) then
                              call img_smfrommaster(rhoa_ao, isfull=.true.)
                              call img_smfrommaster(rhob_ao, isfull=.true.)
                              rho_ao = rhoa_ao + rhob_ao
                              lpolrbuild = .true.
                              ksxscal = hfxscal_pol * hfxfrac
                        else
                              call img_smfrommaster(rho_ao, isfull=.true.)
                              lpolrbuild = .false.
                              ksxscal = hfxscal_unpol * hfxfrac
                        end if
                        
                        if (aux_id .ne. AUX_NONE .and. .not. imaginary) then
                              !
                              ! Set up the computation of auxiliary integrals
                              ! which are evaluated alongside the exchange-correlation
                              ! energy
                              !
                              call aux_matrix_input_dim(m, n, aux_id)
                              if (m > 0 .and. n > 0) then
                                    if (.not. allocated(aux_matrix_input)) then
                                          allocate(aux_matrix_input(m, n))
                                    end if
                                    call img_gmfrommaster(aux_matrix_input)
                              end if

                              nauxint =  aux_arraydim(aux_id, xcf_isuncomp(ixc))
                              if (size(aux) < nauxint) then
                                    deallocate(aux)
                                    deallocate(aux0)
                                    allocate(aux(nauxint))
                                    allocate(aux0(nauxint))
                              end if
                              aux = ZERO
                        else
                              nauxint = 0
                        end if

                        if (lpolrbuild) then
                              ksmatrixa = ZERO
                              ksmatrixb = ZERO
                              kmatrixa  = ZERO
                              kmatrixb  = ZERO
                              jmatrix   = ZERO
                        else
                              fockmatrix = ZERO
                              ksmatrix = ZERO
                        end if
                        
                        eel = ZERO
                        exc = ZERO
                        nel = ZERO
                        div = ZERO
                        kin = ZERO
                        lap = ZERO
                        
                        done_xc = .false.
                        done_fock = .false.

                  else if (iand(mainmsg, IMG_MSG_FOCKGEN) .gt. 0) then
                        !
                        ! Generate Coulomb and HF exchange
                        ! contributions to the Fock matrix
                        ! (bare nuclei Hamiltonian is stored
                        ! by master image)
                        !
                        done_fock = .true.
                        focktask(1) = task(2)
                        focktask(2) = task(3)
                        
                        if (hartree) then
                              inttypes = COULEXCHSUM
                        else
                              inttypes = EXCHSUM
                        end if

                        if (lpolrbuild) then
                              call ufdirect(kmatrixa, kmatrixb, jmatrix, rhoa_ao, rhob_ao, &
                                    work, ksxscal, rshyb, omega, srexx, imaginary, ScreenedHybrid, &
                                    chunk0=focktask, imask=inttypes)
                        else
                              call fdirect(fockmatrix, fockmatrix, rho_ao, ksxscal, &
                                    rshyb, omega, srexx, imaginary, ScreenedHybrid, chunk=focktask, &
                                    imask=inttypes)
                        end if

                  else if (iand(mainmsg, IMG_MSG_XCGEN) .gt. 0) then
                        !
                        ! Generate numerical quadrature 
                        ! contribution to the Kohn-Sham 
                        ! matrix (exchange-correlation)
                        !
                        if (.not. imaginary) then
                              done_xc = .true.
                              xctask(1) = task(2)
                              xctask(2) = task(2)
                              
                              if (lpolrbuild) then
                                    call xcorr(ixc, ksmatrixa, ksmatrixb, rhoa_ao, rhob_ao, exc0, nel0, div0, &
                                          kin0, lap0, aux0, aux_matrix_input, chunk=xctask)
                              else
                                    call xcorr(ixc, ksmatrix, ksmatrix, rho_ao, rho_ao, exc0, nel0, div0, &
                                          kin0, lap0, aux0, aux_matrix_input, chunk=xctask)
                              end if
                              
                              exc = exc + exc0
                              nel = nel + nel0
                              div = div + div0
                              kin = kin + kin0
                              lap = lap + lap0
                              if (aux_id .ne. AUX_NONE) then
                                    aux = aux + aux0
                              end if
                        end if

                  else if (iand(mainmsg, IMG_MSG_ITERDONE) .gt. 0) then
                        !
                        ! The KS matrix build is done. Send
                        ! partial results to the master image.
                        !
                        if (done_fock) then
                              if (lpolrbuild) then
                                    call fockab(kmatrixa, kmatrixb)
                                    call rhotrace(exa, rhoa_ao, kmatrixa)
                                    call rhotrace(exb, rhob_ao, kmatrixb)
                                    if (hartree) then
                                          call rhotrace(ecoul, rho_ao, jmatrix)
                                    else
                                          ecoul = ZERO
                                    end if

                                    if (.not. imaginary) then
                                          eel = frac12 * (exa + exb) + frac12 * ecoul
                                    else
                                          call msg("Error: Parallelization not available for complex SCF", MSG_ERROR)
                                          stop
                                          eel = ZERO
                                    end if
                              else
                                    if (.not. imaginary) then
                                          call rhotrace(eel, rho_ao, fockmatrix) 
                                          eel = (ONE/TWO) * eel
                                    else
                                          call rhotrace(eel, rho_ao, fockmatrix) 
                                          eel = -(ONE/TWO) * Eel
                                    end if
                              end if
                        end if

                        if (done_xc) then
                              if (lpolrbuild) then
                                    ksmatrixa = ksmatrixa + kmatrixa
                                    ksmatrixb = ksmatrixb + kmatrixb
                                    if (hartree) then
                                          ksmatrixa = ksmatrixa + jmatrix
                                          ksmatrixb = ksmatrixb + jmatrix
                                    end if
                              else
                                    ksmatrix = ksmatrix + fockmatrix
                              end if
                              eel = eel + exc
                        else
                              if (lpolrbuild) then
                                    if (hartree) then
                                          ksmatrixa = jmatrix + kmatrixa
                                          ksmatrixb = jmatrix + kmatrixb
                                    else
                                          ksmatrixa = kmatrixa
                                          ksmatrixb = kmatrixb
                                    end if
                              else
                                    ksmatrix = fockmatrix
                              end if
                        end if
                        !
                        ! Accumulate KS matrices and
                        ! other numerical integrals
                        ! computed on the molecular grid
                        !
                        if (lpolrbuild) then
                              call img_ksresults(ksmatrixa, eel, exc, &
                                    nel, div, kin, lap)
                              call img_ksresults(ksmatrixb, eel, exc, &
                                    nel, div, kin, lap, matonly=.true.)
                        else
                              call img_ksresults(ksmatrix, eel, exc, &
                                    nel, div, kin, lap)
                        end if
                        if (aux_id .ne. AUX_NONE .and. .not. imaginary) then
                              !
                              ! Send auxiliary (non-XC) integrals to
                              ! the master image
                              !
                              call img_reduce(aux, IMG_SUM)
                        end if

                  else if (iand(mainmsg, IMG_MSG_SCFDONE) .gt. 0) then
                        !
                        ! SCF converged. There is
                        ! no more work for this slave image
                        !
                        exit scfloop

                  end if
            end do scfloop

            if (ROKS_ENABLED) then
                  deallocate(ksmatrixa)
                  deallocate(ksmatrixb)
                  deallocate(kmatrixa)
                  deallocate(kmatrixb)
                  deallocate(jmatrix)
                  deallocate(rhoa_ao)
                  deallocate(rhob_ao)
                  deallocate(work)
            end if
            deallocate(fockmatrix)
            deallocate(ksmatrix)
            deallocate(rho_ao)
            deallocate(aux)
            deallocate(aux0)
            if (allocated(aux_matrix_input)) deallocate(aux_matrix_input)
      end subroutine ksgen_slave


      subroutine uksgen(ixc, deriv, ksmatrixa, ksmatrixb, jmatrix, &
            eel, exc, rho_ao, rho_oao, rhoa_ao, rhob_ao, hbare, &
            nbeta, nva, nvb, invsq, work, diag, auxint, aux_matrix_input)
            
            type(txcdef), intent(in)                            :: ixc
            real(F64), dimension(:, :), contiguous, intent(out) :: deriv
            real(F64), dimension(:, :), contiguous, intent(out) :: ksmatrixa
            real(F64), dimension(:, :), contiguous, intent(out) :: ksmatrixb
            real(F64), dimension(:, :), contiguous, intent(out) :: jmatrix
            real(F64), intent(out)                              :: eel, exc
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho_ao
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho_oao
            real(F64), dimension(:, :), contiguous, intent(in)  :: rhoa_ao
            real(F64), dimension(:, :), contiguous, intent(in)  :: rhob_ao
            real(F64), dimension(:, :), contiguous, intent(in)  :: hbare
            integer, intent(in)                                 :: nbeta
            real(F64), intent(in)                               :: nva
            real(F64), intent(in)                               :: nvb
            real(F64), dimension(:, :), contiguous, intent(in)  :: invsq
            real(F64), dimension(:, :), contiguous, intent(out) :: work
            type(tgriddiag), intent(out)                        :: diag
            real(F64), dimension(:), contiguous, intent(out)    :: auxint
            real(F64), dimension(:, :), contiguous, intent(in)  :: aux_matrix_input

            logical :: imaginary, rshyb, bareh, hartree, ScreenedHybrid
            real(F64) :: omega, srexx
            integer :: aux_id
            integer :: inttypes
            real(F64) :: ehfa, ehfb, ehbare
            real(F64) :: xcoeff
            real(F64) :: nel, div, kin, lap
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            integer :: m, n

            imaginary = xcf_get_flag(ixc, XCF_IMAGINARY_DENSITY)
            ScreenedHybrid = xcf_get_flag(ixc, XCF_SCREENED_HYBRID)
            rshyb = xcf_get_flag(ixc, XCF_RSHYB)
            if (rshyb .or. ScreenedHybrid) then
                  omega = xcf_get_omega(ixc)
                  if (omega < ZERO) then
                        call dmsg("INVALID VALUE OF RANGE SEPARATION PARAM", &
                              xcf_get_omega(ixc), priority=MSG_ERROR)
                        stop
                  end if
                  srexx = xcf_get_srexx(ixc)
            else
                  omega = -ONE
                  srexx = -ONE
            end if
            hartree = (xcf_get_flag(ixc, XCF_HARTREE) .and. .not. imaginary)
            bareh = (xcf_get_flag(ixc, XCF_BAREH) .and. .not. imaginary)
            xcoeff = -xcf_get_exx(ixc)
            if (hartree) then
                  inttypes = COULEXCHSUM
            else
                  inttypes = EXCHSUM
            end if
            aux_id = aux_get_id(ixc)

            if (IMG_ENABLED) then
                  !
                  ! Command slave images to start building KS matrix
                  ! (begin next SCF iteration)
                  !
                  outmsg(1) = IMG_MSG_NEXTITER
                  ! if (iand(GACC, ACC_NORMAL) .gt. 0) then
                  !       outmsg(1) = ior(outmsg(1), IMG_MSG_ACCNORM)
                  ! end if
                  call img_toslaves(outmsg)
                  !
                  ! Send the definition of the XC functional for
                  ! current SCF iteration. It is admissible
                  ! that XC functionals and/or auxiliary
                  ! integrals change between SCF iterations.
                  !
                  call sendxcdef(ixc)
                  !
                  ! Send AO density matrices to slaves
                  !
                  call img_smtoslaves(rhoa_ao)
                  call img_smtoslaves(rhob_ao)
                  if (aux_id .ne. AUX_NONE) then
                        call aux_matrix_input_dim(m, n, aux_id)
                        if (m > 0 .and. n > 0) then
                              call img_gmtoslaves(aux_matrix_input)
                        end if
                  end if
                  call img_ksbalance(xcf_numint(ixc))
                  outmsg(1) = IMG_MSG_ITERDONE
                  call img_toslaves(outmsg)

                  if (bareh) then
                        !
                        ! Add bare-nuclei Hamiltonian contribution
                        ! to the total electronic energy
                        !
                        call rhotrace(ehbare, rho_ao, hbare)
                        ksmatrixa = hbare
                        ksmatrixb = hbare
                  else
                        ehbare = ZERO
                        ksmatrixa = ZERO
                        ksmatrixb = ZERO
                  end if
                  eel = ehbare
                  exc = ZERO
                  nel = ZERO
                  div = ZERO
                  kin = ZERO
                  lap = ZERO
                  if (aux_id .ne. AUX_NONE) then
                        auxint = ZERO
                  end if
                  !
                  ! Accumulate KS matrix and other
                  ! numerical integrals computed on
                  ! the molecular grid
                  !
                  call img_ksresults(ksmatrixa, eel, exc, &
                        nel, div, kin, lap)
                  call img_ksresults(ksmatrixb, eel, exc, &
                        nel, div, kin, lap, matonly=.true.)
                  if (aux_id .ne. AUX_NONE .and. .not. imaginary) then
                        call img_reduce(auxint, IMG_SUM)
                  end if
            else
                  jmatrix = ZERO
                  ksmatrixa = ZERO
                  ksmatrixb = ZERO

                  call ufdirect(ksmatrixa, ksmatrixb, jmatrix, rhoa_ao, rhob_ao, &
                        work, xcoeff, rshyb, omega, srexx, imaginary, ScreenedHybrid, &
                        imask=inttypes)
                  call fockab(ksmatrixa, ksmatrixb)

                  if (hartree) then
                        ksmatrixa = ksmatrixa + jmatrix
                        ksmatrixb = ksmatrixb + jmatrix
                  end if

                  if (bareh) then
                        !
                        ! Add bare-nuclei Hamiltonian contribution
                        ! to the total electronic energy
                        !
                        call rhotrace(ehbare, rho_ao, hbare)
                        ksmatrixa = ksmatrixa + hbare
                        ksmatrixb = ksmatrixb + hbare
                  else
                        ehbare = ZERO
                  end if

                  if (.not. imaginary) then
                        call rhotrace(ehfa, rhoa_ao, ksmatrixa)
                        call rhotrace(ehfb, rhob_ao, ksmatrixb)
                        eel = FRAC12 * (ehbare + ehfa + ehfb)
                  else
                        eel = ZERO
                  end if

                  if (xcf_numint(ixc) .and. .not. imaginary) then
                        call xcorr(ixc, ksmatrixa, ksmatrixb, rhoa_ao, rhob_ao, &
                              exc, nel, div, kin, lap, auxint, aux_matrix_input)
                        eel = eel + exc
                  else
                        exc = ZERO
                  end if
            end if

            call oaotrans(ksmatrixa, invsq)
            !
            ! In special case ZERO beta density matrix,
            ! do not compute additional derivative matrix
            ! (hydrogen atom, triplet helium etc.)
            !
            if (nbeta == 0) then
                  ksmatrixb = ZERO
                  deriv = ksmatrixa
            else
                  call oaotrans(ksmatrixb, invsq)
                  call roks_orbgradient(deriv, ksmatrixa, ksmatrixb, &
                        rho_oao, work, nva, nvb)
            end if

            if (xcf_numint(ixc) .and. .not. imaginary) then
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
      end subroutine uksgen


      subroutine ksgen(ixc, ksmatrix, eel, exc, rho, hbare, invsq, diag, auxint, &
            aux_matrix_input, oaotransf)
            !
            ! Both triangles of symmetric KSMATRIX are generated if
            ! transformation to OAO basis is requested (OAOTRANSF
            ! is set to .TRUE.) Otherwise, only lower triangle contains 
            ! meaningful numbers.
            !
            type(txcdef), intent(in)                            :: ixc
            real(F64), dimension(:, :), contiguous, intent(out) :: ksmatrix
            real(F64), intent(out)                              :: eel, exc
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho
            real(F64), dimension(:, :), contiguous, intent(in)  :: hbare
            real(F64), dimension(:, :), contiguous, intent(in)  :: invsq
            type(tgriddiag), intent(out)                        :: diag
            real(F64), dimension(:), contiguous, intent(out)    :: auxint
            real(F64), dimension(:, :), contiguous, intent(in)  :: aux_matrix_input
            logical, optional, intent(in)                       :: oaotransf
            
            logical :: imaginary, rshyb, hartree, bareh, ScreenedHybrid
            real(F64) :: omega, srexx
            integer :: aux_id
            integer :: inttypes
            real(F64) :: ehbare
            real(F64) :: xcoeff
            real(F64) :: nel, div, kin, lap
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            logical :: ltransf
            integer :: m, n

            imaginary = xcf_get_flag(ixc, XCF_IMAGINARY_DENSITY)
            ScreenedHybrid = xcf_get_flag(ixc, XCF_SCREENED_HYBRID)
            rshyb = xcf_get_flag(ixc, XCF_RSHYB)
            if (rshyb  .or. ScreenedHybrid) then
                  omega = xcf_get_omega(ixc)
                  if (omega < ZERO) then
                        call dmsg("INVALID VALUE OF RANGE SEPARATION PARAM", &
                              xcf_get_omega(ixc), priority=MSG_ERROR)
                        stop
                  end if
                  srexx = xcf_get_srexx(ixc)
            else
                  omega = -ONE
                  srexx = -ONE
            end if
            hartree = (xcf_get_flag(ixc, XCF_HARTREE) .and. .not. imaginary)
            bareh = (xcf_get_flag(ixc, XCF_BAREH) .and. .not. imaginary)
            xcoeff = -FRAC12 * xcf_get_exx(ixc)
            
            if (hartree) then
                  inttypes = COULEXCHSUM
            else
                  inttypes = EXCHSUM
            end if

            if (present(oaotransf)) then
                  ltransf = oaotransf
            else
                  ltransf = .true.
            end if

            aux_id = aux_get_id(ixc)

            if (IMG_ENABLED) then
                  !
                  ! Command slave images to start building KS matrix
                  ! (begin next SCF iteration)
                  !
                  outmsg(1) = IMG_MSG_NEXTITER
                  ! if (iand(GACC, ACC_NORMAL) .gt. 0) then
                  !       outmsg(1) = ior(outmsg(1), IMG_MSG_ACCNORM)
                  ! end if       
                  
                  call img_toslaves(outmsg)
                  !
                  ! Send the definition of the XC functional for
                  ! current SCF iteration. It is admissible
                  ! that XC functionals and/or auxiliary
                  ! integrals change between SCF iterations.
                  !
                  call sendxcdef(ixc)
                  !
                  ! Send AO density matrix to slaves
                  !
                  call img_smtoslaves(rho)
                  if (aux_id .ne. AUX_NONE) then
                        call aux_matrix_input_dim(m, n, aux_id)
                        if (m > 0 .and. n > 0) then
                              call img_gmtoslaves(aux_matrix_input)
                        end if
                  end if
                  call img_ksbalance(xcf_numint(ixc))
                  outmsg(1) = IMG_MSG_ITERDONE
                  call img_toslaves(outmsg)
                  if (bareh) then
                        !
                        ! Add bare-nuclei Hamiltonian contribution
                        ! to the total electronic energy
                        !
                        call rhotrace(ehbare, rho, hbare)
                        ksmatrix = hbare
                  else
                        ehbare = ZERO
                        ksmatrix = ZERO
                  end if
                  eel = ehbare
                  exc = ZERO
                  nel = ZERO
                  div = ZERO
                  kin = ZERO
                  lap = ZERO
                  if (aux_id .ne. AUX_NONE) then
                        auxint = ZERO
                  end if
                  !
                  ! Accumulate KS matrix and diagnostic
                  ! integrals
                  !
                  call img_ksresults(ksmatrix, eel, exc, &
                        nel, div, kin, lap)
                  if (aux_id .ne. AUX_NONE .and. .not. imaginary) then
                        call img_reduce(auxint, IMG_SUM)
                  end if
            else
                  !
                  ! No parallelization at the images (processes) level
                  !                  
                  ksmatrix = ZERO

                  call fdirect(ksmatrix, ksmatrix, rho, xcoeff, &
                        rshyb, omega, srexx, imaginary, ScreenedHybrid, &
                        imask=inttypes)

                  if (bareh) then
                        !
                        ! Add bare-nuclei Hamiltonian contribution
                        ! to the total electronic energy
                        !
                        call rhotrace(ehbare, rho, hbare)
                        ksmatrix = ksmatrix + hbare
                  else
                        ehbare = ZERO
                  end if

                  if (.not. imaginary) then
                        !
                        ! Calculate the Hartree-Fock-like contribution to
                        ! the total electronic energy (i.e. everything except
                        ! exchange-correlation integral on the molecular grid)
                        !
                        call rhotrace(eel, rho, ksmatrix)
                        eel = FRAC12 * (ehbare + eel)
                  else
                        eel = ZERO
                  end if
                        
                  if (xcf_numint(ixc) .and. .not. imaginary) then
                        call xcorr(ixc, ksmatrix, ksmatrix, rho, rho, &
                              exc, nel, div, kin, lap, auxint, aux_matrix_input)
                        eel = eel + exc
                  else
                        exc = ZERO
                  end if
            end if
            
            if (ltransf) then
                  !
                  ! Transform KS matrix to OAO basis
                  !
                  call oaotrans(ksmatrix, invsq)
            end if

            if (xcf_numint(ixc) .and. .not. imaginary) then
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
      end subroutine ksgen
      

      subroutine spherao(c, nexcluded, overlap, eigenvals, work)
            real(F64), dimension(:, :), contiguous, intent(out)   :: c
            integer, intent(out)                                  :: nexcluded
            real(F64), dimension(:, :), contiguous, intent(inout) :: overlap
            real(F64), dimension(:), contiguous, intent(out)      :: eigenvals
            real(F64), dimension(:, :), contiguous, intent(out)   :: work

            real(F64) :: angdep, angdep0
            integer :: p, q
            integer :: p0, p1
            integer :: l, m
            integer :: ss, s
            integer :: n
            integer :: nexcluded_spher

            c = ZERO
            q = 1
            do ss = 1, nshell
                  s = sh(ss)
                  l = shtype(s)
                  p0 = shpos(ss)
                  p1 = p0 + nfunc(l) - 1

                  angdep0 = nrml(1, 1, s)
                  
                  do m = 1, 2 * l + 1
                        do p = p0, p1
                              !
                              ! Due to the fact that normalization / contraction
                              ! coefficient of an Cartesian AO primitive may
                              ! depend on the index of angular function, partial
                              ! unnormalizing is done before transformation
                              ! to spherical harmonic basis. The angular
                              ! dependence can be written as follows:
                              ! NRML(I(T,U,V), K, S) = C(K, S) * F(I(T,U,V)),
                              ! where F(I(T, U, V)) is the factor to be eliminated,
                              ! K - index of primitive, S - index of shell. Angular
                              ! factor is calculated as
                              ! F(I(T, U, V)) / F(1) = NRML(I, 1, S) / NRML(1, 1, S).
                              !
                              angdep = nrml(p - p0 + 1, 1, s) / angdep0
                              c(p, q) = sphercoeff(p - p0 + 1, m, l) / angdep
                        end do
                        q = q + 1
                  end do
            end do

            n = q - 1
            nexcluded = norb - n
            !
            ! Check for linear dependencies in spherical
            ! basis set.
            ! --
            ! Transform overlap matrix to spherical basis:
            ! WORK <- SC
            ! S <- C^T WORK
            !
            call symmwrap("L", "L", norb, n, ONE, overlap, c, ZERO, work)
            call gemmwrap("T", "N", n, n, norb, ONE, c, work, ZERO, overlap)
            !
            ! Check if there exists near zero eigenvalue of the overlap matrix 
            ! transformed to the spherical basis
            !
            call evd(overlap, eigenvals, n)
            call cartao(work, nexcluded_spher, overlap, eigenvals, norb0=n)
            if (nexcluded_spher > 0) then
                  !
                  ! OVERLAP <- C WORK
                  !
                  call gemmwrap("N", "N", norb, n - nexcluded_spher, n, ONE, c, work, ZERO, overlap)
                  c = overlap
                  n = n - nexcluded_spher
                  nexcluded = norb - n
            end if
      end subroutine spherao


      subroutine cartao(c, nexcluded, s_eigenvec, s_eigenval, norb0)
            real(F64), dimension(:, :), intent(out) :: c
            integer, intent(out)                    :: nexcluded
            real(F64), dimension(:, :), intent(in)  :: s_eigenvec
            real(F64), dimension(:), intent(in)     :: s_eigenval
            integer, optional, intent(in)           :: norb0

            real(F64) :: mineig
            integer :: k, l, n

            if (present(norb0)) then
                  n = norb0
            else
                  n = norb
            end if

            mineig = huge(ONE)
            nexcluded = 0
            l = 1
            do k = 1, n
                  if (s_eigenval(k) .lt. LINDEP_THRESH) then
                        nexcluded = nexcluded + 1
                  else
                        c(:, l) = s_eigenvec(:, k)
                        l = l + 1
                  end if
                  mineig = min(mineig, s_eigenval(k))
            end do

            call dmsg("SMALLEST EIGENVALUE OF S", mineig, fmt="ES10.1")
            call dmsg("THRESHOLD OF LINEAR DEPENDENCE", LINDEP_THRESH, fmt="ES10.1")
            if (nexcluded .gt. 0) then
                  call msg(str(nexcluded) // " VECTORS DEEMED LINEARLY DEPENDENT")
            else
                  call msg("NO VECTORS DEEMED LINEARLY DEPENDENT")
            end if
      end subroutine cartao


      subroutine moguess(ixc, guessmethod, c, rho_ao, aobasis, nexcluded, &
            eigenvals, invsq, sq, hbare, work)
            !
            ! Generate guess MO orbitals. The guess orbitals are restricted
            ! to the space of linear combinations of the columns of AOBASIS.
            ! This restriction enables the exclusion of selected vectors
            ! from the variational space.
            !
            type(txcdef), intent(in)                              :: ixc
            integer, intent(in)                                   :: guessmethod
            real(F64), dimension(:, :), contiguous, intent(out)   :: c
            real(F64), dimension(:, :), contiguous, intent(inout) :: rho_ao
            real(F64), dimension(:, :), contiguous, intent(in)    :: aobasis
            integer, intent(in)                                   :: nexcluded
            real(F64), dimension(:), contiguous, intent(out)      :: eigenvals
            real(F64), dimension(:, :), contiguous, intent(in)    :: invsq
            real(F64), dimension(:, :), contiguous, intent(in)    :: sq
            real(F64), dimension(:, :), contiguous, intent(in)    :: hbare
            real(F64), dimension(:, :), contiguous, intent(out)   :: work

            real(F64) :: eel, exc
            type(txcdef) :: ixc0
            integer :: nactive
            type(tgriddiag) :: griddiag
            real(F64), dimension(0) :: auxint
            real(F64), dimension(0, 0) :: aux_matrix_input
            !
            ! Kohn-Sham (Hartree-Fock) matrix is generated
            ! from spin-unpolarized density, regardless of the
            ! actual electronic configuration of the target system.
            !
            ixc0 = ixc
            call xcf_set_flag(ixc0, XCF_UNCOMP, .false.)
            call aux_set_id(ixc0, AUX_NONE)
            nactive = NORB - nexcluded
            call blankline()
            call msg("GUESS TYPE:")
            select case (guessmethod)
                  case (SCF_GUESS_HBARE)
                        call msg("EIGENVECTORS OF THE BARE NUCLEI HAMILTONIAN")
                        c = hbare 
                        call oaotrans(c, invsq)
                  case (SCF_GUESS_ATOMIC)
                        call msg("SUPERPOSITION OF ATOMIC SPHERICAL DENSITIES")
                        !
                        ! Generate zeroth-iteration density matrix out of superimposed
                        ! spherically-symmetric atomic densities
                        !
                        call guess_atomic(rho_ao)
                        call ksgen(ixc0, c, eel, exc, rho_ao, hbare, invsq, griddiag, &
                              auxint, aux_matrix_input)

                  case (SCF_GUESS_RHO)
                        call msg("DENSITY MATRIX SUPPLIED TO THE SCF SUBPROGRAM")
                        call ksgen(ixc0, c, eel, exc, rho_ao, hbare, invsq, griddiag, &
                              auxint, aux_matrix_input)

                  case (SCF_GUESS_TEXT_FILE, SCF_GUESS_BINARY_FILE)
                        call msg("GUESS DENSITY MATRIX READ FROM DISK")
                        call msg(SCF_GUESS_RHOA_PATH)
                        !
                        ! Read AO density matrix from disk
                        !
                        if (guessmethod == SCF_GUESS_BINARY_FILE) then
                              call io_binary_read(rho_ao, SCF_GUESS_RHOA_PATH)
                        else
                              call io_text_read(rho_ao, SCF_GUESS_RHOA_PATH)
                        end if

                        call ksgen(ixc0, c, eel, exc, rho_ao, hbare, invsq, griddiag, &
                              auxint, aux_matrix_input)

                  case default
                        call msg("INTERNAL ERROR: INVALID TYPE OF SCF GUESS", &
                              priority=MSG_ERROR)
                        stop
            end select
            !
            ! Transform vectors from AO to OAO
            !
            if (nexcluded > 0) then
                  work(:, nactive+1:NORB) = ZERO
            end if
            !
            ! WORK(OAO) <- S^{1/2} AOBASIS(AO)
            !
            call transform_orbitals(work(:, 1:nactive), sq, aobasis(:, 1:nactive))
            !
            ! Diagonalize the KS matrix in the variational subspace
            ! of dimension NACTIVE
            !
            call lowdin_ortho(work(:, 1:nactive))
            call subevd(c, work, eigenvals, nactive)
            !
            ! Calculate excluded vectors (excluded vectors
            ! form orthogonal complement to the active orbitals).
            ! Excluded vectors give 0 eigenvalues, other vectors
            ! give 1's.
            if (nexcluded > 0) then
                  call density_matrix(work, c(:, 1:nactive), ONE)
                  call nullspace(c(:, nactive+1:NORB), work, eigenvals)
            end if
      end subroutine moguess
      

      subroutine ksdriver(scfresults, scfinput, wmatrix, eorb, outmatrix, guessmethod)
            ! ----------------------------------------------------------------
            ! Perform self-consistent Kohn-Sham or restricted Hartree-
            ! Fock calculation. This subroutine utilizes augmented
            ! Roothaan-Hall SCF solver, which is a quasi-Newton algorithm for
            ! optimization of energy as a function of one-electron density 
            ! matrix.
            !
            ! When guess orbitals are provided, it is assumed that NEXCLUDED
            ! last columns of MOCOEFF matrix represent excluded vectors in 
            ! AO basis. Assumed that guess orbitals were generated in
            ! calculations with the same basis as currently set.
            ! ----------------------------------------------------------------
            ! SCFRESULTS - Converged results of the SCF iterations. See
            !              the description of TSCFOUTPUT structure
            ! SCFINPUT   - Input parameters specifying the exchange-correlation
            !              functional and controlling the SCF
            ! AUX_ID      - Input, identifier of the auxiliary (non-XC) integral
            !              to be computed on the molecular grid
            !              
            ! EORB       - On exit, orbital energies of converged KS matrix
            ! GUESSMETHOD- Optional input, method for generating guess
            !              molecular oribtals. Global variable SCF_GUESS
            !              if used if this argument is not specified.
            !
            type(TSCFOutput), intent(out)                         :: scfresults
            type(TSCFParams), intent(in)                          :: scfinput
            real(F64), dimension(:, :), contiguous, intent(inout) :: wmatrix
            real(F64), dimension(:), intent(out)                  :: eorb
            integer, optional, intent(in)                         :: outmatrix
            integer, optional, intent(in)                         :: guessmethod

            type(txcdef) :: ixc
            integer :: aux_id
            real(F64) :: e_total, e_disp
            real(F64) :: e_electron, e_purexc, e_nuclear, e_nonscf
            integer :: nexcluded
            real(F64), dimension(:, :), allocatable :: mocoeff_new, mocoeff_old, mocoeff_sym
            integer, dimension(:), allocatable             :: mocoeff_rep
            real(F64), dimension(:, :), allocatable :: ksmatrix
            real(F64), dimension(:, :), allocatable :: ksmatrix_old
            real(F64), dimension(:, :), allocatable :: overlap, kinetic, attraction
            real(F64), dimension(:, :), allocatable :: hbare
            real(F64), dimension(:, :), allocatable :: rho_ao, rho_ao_old
            real(F64), dimension(:), allocatable           :: rho_ao_tile
            real(F64), dimension(:, :), allocatable :: rho_oao
            real(F64), dimension(:, :), allocatable :: work
            !
            ! Matrices allocated only in open-shell case
            !
            real(F64), dimension(:, :), allocatable :: ksmatrixa
            real(F64), dimension(:, :), allocatable :: ksmatrixb
            real(F64), dimension(:, :), allocatable :: jmatrix
            real(F64), dimension(:, :), allocatable :: rhoa_ao
            real(F64), dimension(:), allocatable           :: rhoa_ao_tile
            real(F64), dimension(:, :), allocatable :: rhob_ao
            real(F64), dimension(:), allocatable           :: rhob_ao_tile
            real(F64)                               :: diplen
            real(F64), dimension(:, :), allocatable :: dipx, dipy, dipz
            real(F64), dimension(:, :), allocatable :: quadxx, quadyy, quadzz
            real(F64), dimension(:, :), allocatable :: quadyx, quadzx, quadzy
            real(F64) :: rho_ao_max_diff, max_orb_grad
            type(arhdata)                           :: arh1
            integer                                 :: info
            integer                                 :: nretry
            real(F64)                               :: deltae
            real(F64)                               :: deltarho
            real(F64)                               :: orb_grad, orb_grad_thresh
            integer, parameter                      :: max_nretry = 10
            logical                                 :: numint
            real(F64), dimension(:), allocatable    :: eigenvals
            real(F64), dimension(:, :), allocatable :: invsq, sq
            real(F64) :: ehomo, elumo
            real(F64), dimension(3) :: dip
            real(F64), dimension(6) :: quad
            !
            ! Timer
            !
            type(tclock) :: t_iter, t_total
            integer :: i
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            real(F64) :: shift
            integer :: guessmethod0, outmatrix0
            !
            ! Total occupation number of core (valence)
            ! orbitals. This is sum of alpha and beta
            ! spin contributions.
            !
            real(F64), dimension(2) :: occnum
            !
            ! Occupation number of alpha spin-orbital belonging
            ! to the open shell. OCCNUMA (CCNUMB)
            ! should lie in the admissible range, 
            ! 0 < ROKS_OPENOCCNUMA < 1.0,
            ! 0 <= ROKS_OPENOCCNUMB <= ROKS_OPENOCCNUMA.
            !
            real(F64), dimension(2) :: occnuma
            real(F64), dimension(2) :: occnumb
            !
            ! Number of core/valence orbitals. For most systems,
            ! valence orbitals are those which belong to the open shell,
            ! and core orbitals are doubly occupied. However, for several
            ! small systems, which do not have any doubly occupied orbitals,
            ! NVALENCE should be equal to zero, and NCORE equal to total number
            ! of orbitals.
            !
            integer :: ncore, nvalence
            integer :: nocc, nvirt, nactive
            integer :: occ0, occ1
            integer :: virt0, virt1
            integer :: nalpha, nbeta
            real(F64) :: nva, nvb
            !
            ! Grid diagnostics
            !
            type(tgriddiag) :: griddiag
            integer :: nauxint
            logical :: finalmo
            type(tvoldata) :: volumetric_data
            !
            ! User-defined parameters for DFT-D3 dispersion correction
            !
            real(F64), dimension(5) :: dftd3_usrparam
            logical :: dftd3_usrdef
            type(trecgroup) :: backup_rho
            integer, parameter :: backup_rho_idx = 1
            !
            ! The minimal duration of a single SCF iteration
            ! for which creating density backups is switched on
            !
            real(F64), parameter :: backup_rho_thresh = real(10 * 60, F64)
            character(8), parameter :: backup_rho_prefix = "scf_rho_"
            integer :: backup_info

            call clock_start(t_total)
            call clock_start(t_iter)
            !
            ! Initialize backup storage for density matrices.
            ! The backup matrices can be used to restart
            ! failed jobs.
            !
            call io_record_initgroup(backup_rho, 0_I64, backup_rho_prefix)
            aux_id = scfinput%AUXInt_Type1
            if (present(outmatrix)) then
                  outmatrix0 = outmatrix
            else
                  outmatrix0 = SCF_OUT_MOCOEFF_AO
            end if

            if (present(guessmethod)) then
                  guessmethod0 = guessmethod
            else
                  guessmethod0 = SCF_GUESS
            end if
            !
            ! Determine occupation numbers of core/valence orbitals
            !
            call roks_elconf(nocc, ncore, nvalence, nalpha, nbeta, occnuma, &
                  occnumb, nva, nvb, ROKS_NE, ROKS_NOPENELA, ROKS_NOPENELB, ROKS_NOPENORB)
            occnum = occnuma + occnumb

            allocate(mocoeff_new(NORB, NORB))
            allocate(mocoeff_old(NORB, NORB))
            allocate(mocoeff_sym(NORB, NORB))
            allocate(mocoeff_rep(NORB))
            allocate(ksmatrix(NORB, NORB))
            allocate(ksmatrix_old(NORB, NORB))
            allocate(rho_ao(NORB, NORB))
            allocate(rho_ao_tile(NORB**2))
            allocate(rho_ao_old(NORB, NORB))
            allocate(rho_oao(NORB, NORB))
            allocate(work(NORB, NORB))
            allocate(dipx(NORB, NORB))
            allocate(dipy(NORB, NORB))
            allocate(dipz(NORB, NORB))
            allocate(quadxx(NORB, NORB))
            allocate(quadyy(NORB, NORB))
            allocate(quadzz(NORB, NORB))
            allocate(quadyx(NORB, NORB))
            allocate(quadzx(NORB, NORB))
            allocate(quadzy(NORB, NORB))
            allocate(eigenvals(NORB))
            allocate(invsq(NORB, NORB))
            allocate(sq(NORB, NORB))
            allocate(overlap(NORB, NORB))
            allocate(kinetic(NORB, NORB))
            allocate(attraction(NORB, NORB))
            allocate(hbare(NORB, NORB))
            if (ROKS_ENABLED) then
                  allocate(ksmatrixa(NORB, NORB))
                  allocate(ksmatrixb(NORB, NORB))
                  allocate(jmatrix(NORB, NORB))
                  allocate(rhoa_ao(NORB, NORB))
                  allocate(rhoa_ao_tile(NORB**2))
                  allocate(rhob_ao(NORB, NORB))
                  allocate(rhob_ao_tile(NORB**2))
            end if
            allocate(scfresults%OrbEnergies(NORB, 1))
            !
            ! Generate structure containing the complete definition
            ! of the requested XC functional and/or auxiliary integrals
            ! on the molecular grid. This sturcture is used when
            ! interfacing with subprograms building the KS matrix.
            !
            if (ROKS_ENABLED) then
                  call xcf_define(ixc, scfinput%xcfunc, aux_id, .true.)
            else
                  call xcf_define(ixc, scfinput%xcfunc, aux_id, .false.)
            end if
            !
            ! Overwrite default values of the range separation
            ! parameter and fraction of short-range HF exchange
            ! if set by the user
            !
            if (.not. LCOMEGA < ZERO) call xcf_set_omega(ixc, LCOMEGA)
            if (.not. LCSREXX < ZERO) call xcf_set_srexx(ixc, LCSREXX)
            numint = xcf_numint(ixc)
            !
            ! Convergence thresholds
            !
            if (SCF_THRESH_DENSITY < ZERO) then
                  if (xcf_get_id(ixc) == XCF_HF) then
                        rho_ao_max_diff = SCF_THRESH_DENSITY_DEFAULT_WF
                  else
                        rho_ao_max_diff = SCF_THRESH_DENSITY_DEFAULT_DFT
                  end if
            else
                  rho_ao_max_diff = SCF_THRESH_DENSITY
            end if

            if (SCF_THRESH_GRADIENT < ZERO) then
                  max_orb_grad = SCF_THRESH_GRADIENT_DEFAULT
            else
                  max_orb_grad = SCF_THRESH_GRADIENT
            end if

            if (aux_id == AUX_NONE) then
                  allocate(scfresults%AUXOut(0))
                  nauxint = 0
            else
                  nauxint = aux_arraydim(aux_id, xcf_isuncomp(ixc))
                  allocate(scfresults%AUXOut(nauxint))
            end if
            !
            ! SCF preamble
            !
            call toprule()
            call msg("SCF MODULE")
            call midrule()
            !
            ! Display info on the selected exchange-correlation model
            !
            call xcf_display(ixc)
            call midrule()
            if (roks_isnoninteger()) then
                  call dmsg("ALPHA ELECTRONS", dble(roks_nealpha()) / dble(ROKS_NEUNIT), fmt="F20.3")
                  call dmsg("BETA ELECTRONS", dble(roks_nebeta()) / dble(ROKS_NEUNIT), fmt="F20.3")
            else
                  call imsg("ALPHA ELECTRONS", roks_nealpha() / ROKS_NEUNIT)
                  call imsg("BETA ELECTRONS", roks_nebeta() / ROKS_NEUNIT)
            end if
            if (ROKS_NOPENORB > 0) then
                  call imsg("ORBITALS IN THE OPEN SHELL", ROKS_NOPENORB)
                  call dmsg("OCCUPATION NUMBER (ALPHA)", &
                        dble(ROKS_NOPENELA)/dble(ROKS_NEUNIT*ROKS_NOPENORB), fmt="F5.3")
                  call dmsg("OCCUPATION NUMBER (BETA)", &
                        dble(ROKS_NOPENELB)/dble(ROKS_NEUNIT*ROKS_NOPENORB), fmt="F5.3")
            end if
            
            call dmsg("CONV THRESH (RHO_AO_MAX_DIFF)", rho_ao_max_diff, fmt="ES10.1")
            call dmsg("CONV THRESH (MAX_ORB_GRAD)", max_orb_grad, fmt="ES10.1") 
            call smsg("SCF SOLVER", "ARH")
            call scf_parallelism()
            call msg("RECORD FILE FOR BACKUP DENSITY MATRIX")
            call msg(record_linkname(backup_rho_prefix//str(backup_rho_idx)))
            call blankline()
            !
            ! Nuclear repulsion energy
            !
            e_nuclear = nuclrep()
            call stv(overlap, kinetic, attraction)
            !
            ! Effective core potential
            !
            call pseudopot(attraction)
            hbare = kinetic + attraction

            if (guessmethod0 .ne. SCF_GUESS_MOVEC) then
                  !
                  ! WORK <- eigenvectors of the overlap matrix
                  !
                  work = overlap
                  call evd(work, eigenvals)
                  !
                  ! Compute AO vectors belonging to variational space
                  !
                  if (SPHERBASIS) then
                        mocoeff_new = overlap
                        call spherao(mocoeff_old, nexcluded, mocoeff_new, eigenvals, work)
                  else
                        call cartao(mocoeff_old, nexcluded, work, eigenvals)
                  end if
            end if
            !
            ! Number of orbitals active in variational space 
            ! (the whole linear space spanned by linear combinations of
            ! Cartesian functions in divided into active vectors and 
            ! excluded vectors)
            !
            nactive = NORB - nexcluded
            nvirt = nactive - nocc
            occ0 = 1
            occ1 = nocc
            virt0 = nocc + 1
            virt1 = nocc + nvirt

            work = overlap
            call ssqinvsq(work, invsq, sq, eigenval=eigenvals)
            
            if (guessmethod0 == SCF_GUESS_RHO) then
                  !
                  ! Density matrix for the zeroth iteration has been provided
                  ! to the SCF subprogram
                  !
                  rho_ao = wmatrix
                  call moguess(ixc, guessmethod0, mocoeff_new, rho_ao, mocoeff_old, nexcluded, &
                        eigenvals, invsq, sq, hbare, work)
            else if (guessmethod0 .ne. SCF_GUESS_MOVEC) then
                  rho_ao = ZERO
                  call moguess(ixc, guessmethod0, mocoeff_new, rho_ao, mocoeff_old, nexcluded, &
                        eigenvals, invsq, sq, hbare, work)
            else
                  !
                  ! Transform guess orbitals to OAO basis
                  !
                  call symmwrap("L", "L", NORB, NORB, ONE, sq, wmatrix, ZERO, mocoeff_new)           
            end if
            !
            ! Initializing ARH solver
            ! ---
            ! If there are linear dependencies in AO basis,
            ! a subset of eigenvectors of the overlap matrix
            ! should be inactive during SCF optimization. 
            !
            call arhdata_init(arh1, occnum, ncore, &
                  occ0, occ1, virt0, virt1)
            !
            ! Display parameters of integration grid
            !
            if (numint) call gridsummary(GRD_FINE, .true.)
            if (ROKS_ENABLED) then
                  call roks_density(rho_ao, rho_oao, rhoa_ao, rhob_ao, mocoeff_new, &
                        invsq, ncore, nvalence, occnuma, occnumb)
                  call matrix2tile(rho_ao_tile, rho_ao)
                  call matrix2tile(rhoa_ao_tile, rhoa_ao)
                  call matrix2tile(rhob_ao_tile, rhob_ao)
                  call uksgen(ixc, ksmatrix, ksmatrixa, ksmatrixb, jmatrix, &
                        arh1%eold, e_purexc, rho_ao, rho_oao, rhoa_ao, rhob_ao, &
                        hbare, nbeta, nva, nvb, invsq, work, griddiag, scfresults%AUXOut, &
                        scfinput%AUXIn)
            else
                  call rks_density(rho_ao, rho_oao, mocoeff_new, nocc, invsq)
                  call matrix2tile(rho_ao_tile, rho_ao)
                  call ksgen(ixc, ksmatrix, arh1%eold, e_purexc, rho_ao, &
                        hbare, invsq, griddiag, scfresults%AUXOut, scfinput%AUXIn)
            end if
            deltarho = maxnorm(rho_ao)
            e_electron = arh1%eold
            e_total = e_electron + e_nuclear
            deltae = e_total
            orb_grad = ZERO
            !
            ! Display header of SCF cycles table
            ! ---
            ! ITER ENERGY RHODIFF EDIFF SHIFT NARH TIME
            !
            call blankline()
            call scftable_start()
            call scftable_continue(1, e_total, deltarho, deltae, &
                  ZERO, 0, orb_grad, clock_readwall(t_iter))
            arhloop: do i = 1, SCF_MAXIT
                  if (clock_readwall(t_iter) > backup_rho_thresh) then
                        !
                        ! Backup the density matrix if the previous
                        ! iteration was long enough
                        !
                        call io_record_write(backup_rho, backup_rho_idx, rho_ao, backup_info)
                  end if
                  call clock_start(t_iter)
                  
                  mocoeff_old = mocoeff_new
                  rho_ao_old = rho_ao
                  ksmatrix_old = ksmatrix
                  !
                  ! Construct updated molecular orbitals for the next
                  ! step using augmented Roothaan-Hall algoritm
                  !
                  call arh_driver(arh1, rho_oao, mocoeff_new, ksmatrix)
                  !
                  ! The max norm of the orbital gradient matrix
                  !
                  orb_grad = arh1%orb_grad_max

                  if (ROKS_ENABLED) then
                        call roks_density(rho_ao, rho_oao, rhoa_ao, rhob_ao, mocoeff_new, &
                              invsq, ncore, nvalence, occnuma, occnumb)
                        call matrix2tile(rho_ao_tile, rho_ao)
                        call matrix2tile(rhoa_ao_tile, rhoa_ao)
                        call matrix2tile(rhob_ao_tile, rhob_ao)
                  else
                        call rks_density(rho_ao, rho_oao, mocoeff_new, nocc, invsq)
                        call matrix2tile(rho_ao_tile, rho_ao)
                  end if
                  deltarho = maxdiff(rho_ao_old, rho_ao, NORB)

                  if (ROKS_ENABLED) then
                        call uksgen(ixc, ksmatrix, ksmatrixa, ksmatrixb, jmatrix, &
                              arh1%enew, e_purexc, rho_ao, rho_oao, rhoa_ao, rhob_ao, &
                              hbare, nbeta, nva, nvb, invsq, work, griddiag, scfresults%AUXOut, &
                              scfinput%AUXIn)
                  else
                        call ksgen(ixc, ksmatrix, arh1%enew, e_purexc, &
                              rho_ao, hbare, invsq, griddiag, scfresults%AUXOut, scfinput%AUXIn)
                  end if
                  deltae = arh1%enew - arh1%eold

                  if (ROKS_ENABLED) then
                        orb_grad_thresh = SCF_DISABLE_ARH_SHIFT_POLAR
                  else
                        orb_grad_thresh = SCF_DISABLE_ARH_SHIFT_UNPOLAR
                  end if
                  
                  if (orb_grad < orb_grad_thresh) then
                        !
                        ! Final stages of the density optimization are
                        ! approached: do not update trust radius and do not
                        ! add a shift value to orbital energies when computing
                        ! the ARH orbital rotation matrix.
                        !
                        info = 0
                  else
                        !
                        ! Update trust radius of the ARH model energy
                        !
                        if (arh1%enable_shift) then
                              call trustup(arh1, info)
                        else
                              info = 0
                        end if
                  end if

                  nretry = 1
                  do while ((info .eq. -1) .and. (nretry .le. max_nretry))
                        call msg("Reducing step size and recomputing KS matrix")
                        !
                        ! Delete the newest stored Fock matrix and density matrix
                        ! to avoid linear dependencies
                        !
                        if (arh1%nstor > 0) then
                              call del(arh1, arh1%nstor)
                        end if
                        !
                        ! Revert density matrix
                        ! and KS matrix
                        !
                        mocoeff_new = mocoeff_old
                        if (ROKS_ENABLED) then
                              call roks_density(rho_ao, rho_oao, rhoa_ao, rhob_ao, mocoeff_new, &
                                    invsq, ncore, nvalence, occnuma, occnumb)
                              call matrix2tile(rho_ao_tile, rho_ao)
                              call matrix2tile(rhoa_ao_tile, rhoa_ao)
                              call matrix2tile(rhob_ao_tile, rhob_ao)
                        else
                              call rks_density(rho_ao, rho_oao, mocoeff_new, nocc, invsq)
                              call matrix2tile(rho_ao_tile, rho_ao)
                        end if
                        ksmatrix = ksmatrix_old
                        !
                        ! Try again ARH optimization with 
                        ! lowered trust radius
                        !
                        call arh_driver(arh1, rho_oao, mocoeff_new, ksmatrix)
                        orb_grad = arh1%orb_grad_max

                        if (ROKS_ENABLED) then
                              call roks_density(rho_ao, rho_oao, rhoa_ao, rhob_ao, mocoeff_new, &
                                    invsq, ncore, nvalence, occnuma, occnumb)
                              call matrix2tile(rho_ao_tile, rho_ao)
                              call matrix2tile(rhoa_ao_tile, rhoa_ao)
                              call matrix2tile(rhob_ao_tile, rhob_ao)
                              call uksgen(ixc, ksmatrix, ksmatrixa, ksmatrixb, jmatrix, &
                                    arh1%enew, e_purexc, rho_ao, rho_oao, rhoa_ao, rhob_ao, &
                                    hbare, nbeta, nva, nvb, invsq, work, griddiag, scfresults%AUXOut, &
                                    scfinput%AUXIn)
                        else  
                              call rks_density(rho_ao, rho_oao, mocoeff_new, nocc, invsq)
                              call matrix2tile(rho_ao_tile, rho_ao)
                              call ksgen(ixc, ksmatrix, arh1%enew, e_purexc, rho_ao, &
                                    hbare, invsq, griddiag, scfresults%AUXOut, scfinput%AUXIn)
                        end if

                        call trustup(arh1, info)
                        nretry = nretry + 1
                  end do

                  e_electron = arh1%enew
                  e_total = e_electron + e_nuclear
                  deltae = arh1%enew - arh1%eold
                  arh1%eold = arh1%enew
                  shift = abs(arh1%shift)
                  !
                  ! ITER ENERGY RHODIFF EDIFF SHIFT NARH GRAD TIME
                  !
                  call scftable_continue(i + 1, e_total, deltarho, deltae, &
                        shift, arh1%nstor, orb_grad, clock_readwall(t_iter))
                  !
                  ! Test if convergence is reached
                  !
                  if (deltarho < rho_ao_max_diff .and. orb_grad < max_orb_grad) then
                        exit arhloop
                  end if
            end do arhloop

            if (deltarho < rho_ao_max_diff .and. orb_grad < max_orb_grad) then
                  call toprule()
                  call msg("SCF CONVERGED")
                  call dmsg("CONVERGED ENERGY", e_total)
                  call midrule()
            else
                  call msg("CONVERGENCE CRITERIA NOT MET", MSG_ERROR)
                  stop
            end if

            if (scfinput%non_scf_xcfunc .ne. XCF_XC_NONE) then
                  ! -------------------------------------
                  ! Non-self consistent energy evaluation
                  ! -------------------------------------
                  call clock_start(t_iter)
                  !
                  ! Define the non-SCF functional
                  !
                  if (ROKS_ENABLED) then
                        call xcf_define(ixc, scfinput%non_scf_xcfunc, aux_id, .true.)
                  else
                        call xcf_define(ixc, scfinput%non_scf_xcfunc, aux_id, .false.)
                  end if
                  if (.not. LCOMEGA < ZERO) call xcf_set_omega(ixc, LCOMEGA)
                  if (.not. LCSREXX < ZERO) call xcf_set_srexx(ixc, LCSREXX)
                  numint = xcf_numint(ixc)
                  
                  if (ROKS_ENABLED) then
                        call uksgen(ixc, ksmatrix, ksmatrixa, ksmatrixb, jmatrix, &
                              e_nonscf, e_purexc, rho_ao, rho_oao, rhoa_ao, rhob_ao, &
                              hbare, nbeta, nva, nvb, invsq, work, griddiag, scfresults%AUXOut, &
                              scfinput%AUXIn)
                  else
                        call ksgen(ixc, ksmatrix, e_nonscf, e_purexc, &
                              rho_ao, hbare, invsq, griddiag, scfresults%AUXOut, scfinput%AUXIn)
                  end if

                  e_electron = e_nonscf
                  if (xcf_get_flag(ixc, XCF_BAREH)) then
                        e_nonscf = e_nonscf + e_nuclear
                  end if
                  e_total = e_nonscf
                  
                  call blankline()
                  call msg("NON-SCF ENERGY EVALUATION", underline=.true.)
                  call xcf_display(ixc)
                  call blankline()
                  !
                  ! ITER ENERGY RHODIFF EDIFF SHIFT NARH TIME
                  !
                  call midrule()
                  call scftable_continue(1, e_total, ZERO, e_nonscf-e_electron, &
                        ZERO, 0, ZERO, clock_readwall(t_iter))
                  call midrule()
                  call blankline()
            else
                  e_nonscf = ZERO
            end if

            call dmsg("TOTAL TIME [SECONDS] ", clock_readwall(t_total), fmt="ES10.1")
            if (xcf_isgridxc(ixc)) then
                  call displaygriddiag(griddiag, (xcf_ismgga(ixc) .or. aux_ismgga(ixc)))
            end if
            !
            ! Save converged total density matrix in AO basis
            !
            if (SCF_SAVE_RHO_MODE .ne. FILEMODE_NONE) then
                  call blankline()
                  call msg("CONVERGED DENSITY MATRIX SAVED TO")
                  call msg(SCF_SAVE_RHOA_PATH)
                  if (SCF_SAVE_RHO_MODE == FILEMODE_TEXT) then
                        !
                        ! Portable textfile
                        !
                        call io_text_write(rho_ao, SCF_SAVE_RHOA_PATH)
                  else if (SCF_SAVE_RHO_MODE == FILEMODE_BINARY) then
                        !
                        ! Binary format
                        !
                        call io_binary_write(rho_ao, SCF_SAVE_RHOA_PATH)
                  end if
            end if
            
            finalmo = .false.

            if (outmatrix0 == SCF_OUT_MOCOEFF_AO .or. &
                  DFT_DISP == DISP_MULTIPOLE_RPA) then
                  finalmo = .true.
            end if
            !
            ! Test if a cube file containing volumetric data is
            ! requested. Test if writing to the cube file requires
            ! MO coefficients in AO basis. For example, the MO
            ! coefficients in AO basis are required if the cube
            ! file should contain a grid representation of MOs.
            !
            if (CUBEFILE_FUNC .ne. VOL_FUNC_NONE) then
                  if (.not. allocated(CUBEFILE_MO_IDX)) allocate(CUBEFILE_MO_IDX(0))
                  if (.not. allocated(CUBEFILE_AO_CENTERS)) allocate(CUBEFILE_AO_CENTERS(0))
                  
                  call volumetric_data%init(CUBEFILE_FUNC, ROKS_ENABLED, .true., &
                        CUBEFILE_SPACING, CUBEFILE_MO_IDX, CUBEFILE_AO_CENTERS)
                  !
                  ! Test if generating grid data requires molecular orbitals
                  !
                  if (vol_needs_mocoeff(volumetric_data)) then
                        finalmo = .true.
                  end if
            end if
            !
            ! 1. Diagonalize converged KS matrix: calculate orbital energies & MO coeffs
            ! 2. Transform MO coefficients back from OAO to AO basis and pass as output.
            !
            if (nexcluded .eq. 0) then
                  call evd(ksmatrix, eigenvals)
                  if (finalmo) then
                        if (size(eorb) >= size(eigenvals)) then
                              eorb = eigenvals
                        else
                              call msg("CANNOT WRITE ORBITAL ENERGIES TO EORB", MSG_ERROR)
                              stop
                        end if
                        call gemmwrap("N", "N", NORB, NORB, NORB, ONE, invsq, &
                              ksmatrix, ZERO, mocoeff_new)
                  end if
            else
                  call subevd(ksmatrix, mocoeff_new, eigenvals, NORB-nexcluded)
                  if (finalmo) then
                        if (size(eorb) >= size(eigenvals)) then
                              eorb = eigenvals
                        else
                              call msg("CANNOT WRITE ORBITAL ENERGIES TO EORB", MSG_ERROR)
                              stop
                        end if
                        !
                        ! Transform the final molecular orbitals to the AO basis.
                        ! Include the excluded vectors in the last NEXCLUDED columns.
                        !
                        ksmatrix(:, NORB-nexcluded+1:NORB) = mocoeff_new(:, NORB-nexcluded+1:NORB)
                        call real_gramschmidt(.true., ksmatrix, NORB-nexcluded)
                        call gemmwrap("N", "N", NORB, NORB, NORB, ONE, invsq, &
                              ksmatrix, ZERO, mocoeff_new)
                  end if
            end if
            !
            ! Write a file containting volumetric data: molecular orbitals,
            ! electron density, or other functions defined on the molcular
            ! grid.
            !
            if (CUBEFILE_FUNC .ne. VOL_FUNC_NONE) then
                  if (ROKS_ENABLED) then
                        call vol_write(INUCLZ, ATOMR, rhoa_ao_tile, rhob_ao_tile, &
                              mocoeff_new, mocoeff_new, volumetric_data)
                  else
                        call vol_write(INUCLZ, ATOMR, rho_ao_tile, rho_ao_tile, &
                              mocoeff_new, mocoeff_new, volumetric_data)
                  end if
            end if
            
            if (outmatrix0 == SCF_OUT_MOCOEFF_AO) then
                  wmatrix = mocoeff_new
            end if
            !
            ! HOMO and LUMO energies
            !
            ehomo = homo(eigenvals, nocc)
            elumo = lumo(eigenvals, nocc)

            if (outmatrix0 == SCF_OUT_RHO_AO) then
                  !
                  ! The returned matrix is RHO(ALPHA) + RHO(BETA)
                  ! in spin-unpolarized as well as spin-polarized case
                  !
                  wmatrix = rho_ao
            end if
            !
            ! Calculate electronic contribution
            ! to dipole moment. Nuclear contribution
            ! is stored in NUCLDIP global variable.
            !
            call dipole(dipx, dipy, dipz, origin)
            !
            ! Construct dipole vector. Add nuclear
            ! contribution
            !
            call dipolevec(dip, dipx, dipy, dipz, rho_ao)
            diplen = norm2(dip)
            !
            ! Calculate electronic contribution
            ! to quadrupole moment. Nuclear contribution
            ! is stored in NUCLQUAD global variable.
            ! Verified against Dalton 2011. 
            !
            call quadrupole(quadxx, quadyy, quadzz, quadyx, quadzx, quadzy, origin, &
                  QUAD_TRACELESS_BUCKINGHAM)
            call quadrupolevec(quad, quadxx, quadyy, quadzz, quadyx, quadzx, quadzy, rho_ao)
            !
            ! SCF summary
            !
            call dmsg("FINAL ENERGY", e_electron + e_nuclear)
            call dmsg("FINAL ENERGY [eV]", toev(e_electron+e_nuclear))
            call dmsg("NUCLEAR REPULSION", e_nuclear)
            call dmsg("PURE DFT XC", e_purexc)
            call dmsg("ELECTRONIC ENERGY", e_electron)
            call blankline()

            call msg("ORBITAL ENERGIES", underline=.true.)
            call dmsg("HOMO ENERGY [A.U.]", ehomo)
            call dmsg("LUMO ENERGY [A.U.]", elumo)
            call blankline()
            call dmsg("HOMO ENERGY [eV]", toev(ehomo))
            call dmsg("LUMO ENERGY [eV]", toev(elumo))
            call blankline()

            ! select case (aux_id)
            ! case (AUX_NONE)
            !       scfresults%AUXOut = ZERO
            ! case (AUX_HIRSHFELD_POPULATION)
            !       call hirshfeld_population_display(scfresults%AUXOut)
            ! case (AUX_HIRSHFELD_VOLUME)
            !       call hirshfeld_volume_ratios(scfresults%AUXOut)
            !       call hirshfeld_volume_display(scfresults%AUXOut)
            ! case (AUX_HIRSHFELD_VOLUME_FREE)
            !       continue
            ! case (AUX_GDD_GRAC)
            !       call aux_gdd_grac_finalize(scfresults%AUXOut, xcf_isuncomp(ixc))
            !       call blankline()
            ! case (AUX_GDD_OMEGA)
            !       call aux_gdd_finalize(scfresults%AUXOut, GDD_NOUT_ALPHA, GDD_MUMIN)
            !       call blankline()
            ! case (AUX_XHOLE_DIST_FUKUI)
            !       call aux_xhole_dist_fukui_finalize(scfresults%AUXOut, xcf_isuncomp(ixc))
            !       call blankline()
            ! case (AUX_REGULARIZED_VNUCL)
            !       call msg("Computed matrix of regularized electrons-nuclei potential")
            !       call blankline()
            ! case (AUX_MODRZEJ2012_C_HOLE)
            !       call modrzej2012_c_hole_finalize(scfresults%AUXOut, xcf_isuncomp(ixc))
            !       call blankline()
            ! case (AUX_BR_X_HOLE, AUX_PBE_X_HOLE, AUX_TPSS_X_HOLE)
            !       call modrzej2016_x_hole_finalize(scfresults%AUXOut, xcf_isuncomp(ixc))
            !       call blankline()
            ! case (AUX_MCSv1_VC_LAMBDA, AUX_MCSv2_VC_LAMBDA)
            !       call modrzej2016_vc_curve_display(scfresults%AUXOut)
            !       call blankline()
            ! case default
            !       call msg("AUXILIARY NON-XC INTEGRALS", underline=.true.)
            !       do i = 1, nauxint
            !             call dmsg("VALUE", scfresults%AUXOut(i), fmt="F20.15")
            !       end do
            !       call blankline()
            ! end select
            
            call msg("DIPOLE MOMENT [A.U.]", underline=.true.)
            call dmsg("LENGTH", diplen)
            call dmsg("X", dip(1))
            call dmsg("Y", dip(2))
            call dmsg("Z", dip(3))
            call blankline()

            call msg("DIPOLE MOMENT [DEBYE]", underline=.true.)
            call dmsg("LENGTH", todebye(diplen))
            call dmsg("X", todebye(dip(1)))
            call dmsg("Y", todebye(dip(2)))
            call dmsg("Z", todebye(dip(3)))

            call blankline()
            call msg("TRACELESS QUADRUPOLE MOMENT [A.U.]", underline=.true.)
            call dmsg("XX", quad(1))
            call dmsg("YY", quad(2))
            call dmsg("ZZ", quad(3))
            call dmsg("XY", quad(4))
            call dmsg("XZ", quad(5))
            call dmsg("YZ", quad(6))
            call blankline()
            call msg("TRACELESS QUADRUPOLE MOMENT [BUCKINGHAM = DEBYE * ANGS]", underline=.true.)
            call dmsg("XX", toang(todebye((quad(1)))))
            call dmsg("YY", toang(todebye((quad(2)))))
            call dmsg("ZZ", toang(todebye((quad(3)))))
            call dmsg("XY", toang(todebye((quad(4)))))
            call dmsg("XZ", toang(todebye((quad(5)))))
            call dmsg("YZ", toang(todebye((quad(6)))))
            call blankline()
            ! ----------------------------------------------------------------------------------
            !  No dispersion corrections. Dispaersion corrections are computed only in the new
            !  UKS code.
            ! ----------------------------------------------------------------------------------
            e_disp = ZERO
            !
            ! Store converged results
            !
            scfresults%EtotDFT = e_electron + e_nuclear
            scfresults%EelDFT = e_electron
            scfresults%ExcDFT = e_purexc
            scfresults%Ehomo = ehomo
            scfresults%Elumo = elumo
            scfresults%EdispDFT = e_disp
            scfresults%Nexcluded = Nexcluded

            if (IMG_ENABLED .and. IMG_ISMASTER) then
                  !
                  ! Order slave images to return
                  !
                  outmsg(1) = IMG_MSG_SCFDONE
                  call img_toslaves(outmsg)
            end if

            call arhdata_free(arh1)
            !
            ! Delete backup density matrices
            !
            call io_record_deletegroup(backup_rho)
      end subroutine ksdriver
end module scf
