module basis
      use math_constants
      use sort
      use gparam
      use periodic
      use display
      use gto
      use specf
      use parser
      use ecpint

      implicit none

contains

      pure function pqrsidx(p, q, r, s, base)
            ! ----------------------------------------------
            ! Calculate canonical index corresponding
            ! to (p, q, r, s) tuple. Underlying ordering
            ! is as follows:
            ! (ab|cd), a >= b, c >= d, (ab) >= (cd)
            ! ----------------------------------------------
            ! BASE - Maximum value of single index. Indices
            !        are assumed to be restricted to
            !        (1, BASE) range of integers.
            !
            integer :: pqrsidx
            integer, intent(in) :: p, q, r, s
            integer, intent(in) :: base

            integer :: i1, i2, base2
            
            if (p .gt. q) then
                  i1 = p * base + q
            else
                  i1 = q * base + p
            end if

            if (r .gt. s) then
                  i2 = r * base + s
            else
                  i2 = s * base + r
            end if

            base2 = (base * (base - 1)) / 2
            if (i1 .gt. i2) then
                  pqrsidx = i1 * base2 + i2
            else
                  pqrsidx = i2 * base2 + i1
            end if
      end function pqrsidx


      subroutine normalize(s)
            ! --------------------------------------------------
            ! Calculate normalization constants. Normalization
            ! constant is compound made of factor normalizing
            ! contracted gaussian sum to unity and contraction
            ! coefficient. There is in general different
            ! normalization constant for each primitive
            ! function.
            ! --------------------------------------------------
            ! 1. Fundamentals of Molecular
            !    Integral Evaluation, eqs. 2.11 and 2.25
            ! --------------------------------------------------
            ! S - Index of shell
            !
            integer, intent(in) :: s

            integer :: i, j
            integer :: k
            integer :: l, m, n
            integer :: imomentum
            double precision :: t
            double precision :: dmomentum
            double precision :: sum
            double precision :: const1
            double precision :: ai, aj
            double precision :: alphai, alphaj
            double precision :: nconst, lmnconst, lmnconst2
            double precision, dimension(max_nprm) :: cntr_norm

            imomentum = shtype(s)
            dmomentum = dble(imomentum)
            const1 = pi32 / two**(imomentum)
            !
            ! Sum over different x^l y^m z^n polynomials
            !
            lmn: do k = 1, nfunc(imomentum)
                  l = ll(k, imomentum)
                  m = mm(k, imomentum)
                  n = nn(k, imomentum)
                  !
                  ! (2l - 1)!! * (2m - 1)!! * (2n - 1)!!
                  !
                  lmnconst2 = dblfactorial(l) * dblfactorial(m) * dblfactorial(n)
                  lmnconst = sqrt(lmnconst2)
                  !
                  ! Temporary constants need to be computed bacause
                  ! Raw coefficients read from a basis set file
                  ! are assumed to multiply normalized primitive
                  ! Gaussians.
                  !
                  do i = 1, nprm(s)
                        alphai = expn(i, s)
                        t = cntr(i, s) * (two / pi)**(three / four) &
                              * two**imomentum * alphai**((two * dmomentum + three) / four)
                        cntr_norm(i) = t / lmnconst
                  end do
                  !
                  ! Calculate sum_{ij} a_i a_j / (alpha_i + alpha_j)^{L + 3/2}
                  !
                  sum = zero
                  do i = 1, nprm(s)
                        ai = cntr_norm(i)
                        alphai = expn(i, s)
                        do j = 1, i
                              aj = cntr_norm(j)
                              alphaj = expn(j, s)
                              if (i .eq. j) then
                                    sum = sum + ai * aj / (alphai + alphaj)**(dmomentum + frac32)
                              else
                                    sum = sum + two * ai * aj / (alphai + alphaj)**(dmomentum + frac32)
                              end if
                        end do
                  end do
                  !
                  ! Normalization constant, Eq. 2.25
                  !
                  nconst = one / (sqrt(const1 * sum) * lmnconst)
                  CNTRNORM(k, s) = ONE / (sqrt(const1 * sum) * lmnconst2)
                  !
                  ! Contraction coefficient should be glued
                  ! to normalization constant.
                  !
                  do i = 1, nprm(s)
                        nrml(k, i, s) = cntr_norm(i) * nconst
                  end do
            end do lmn
            !
            ! Normalize contraction coefficients for each primitive
            ! (still, they are independent of a particular angular
            ! function)
            !
            do i = 1, nprm(s)
                  alphai = expn(i, s)
                  t = CNTR(i, s) * (two / pi)**(three / four) &
                        * two**imomentum * alphai**((two * dmomentum + three) / four)
                  CNTR(i, s) = t
            end do
      end subroutine normalize


      subroutine gridscreen(shrmax)
            ! -----------------------------------------------------
            ! For each orbital shell, calculate sqare of maximum
            ! radius where AOs have non-negligible value.
            ! This esstimation is based on calculation of spherical
            ! average of the primitive with smallest exponent.
            ! -----------------------------------------------------
            ! SHRMAX - Output, array of size NUSHELL, contains
            !          sqare of maximum radius for each unique
            !          shell.
            !
            double precision, dimension(:), intent(out) :: shrmax

            double precision :: r
            integer :: k

            do k = 1, nushell
                  r = rmax(k, grid_aothresh)
                  shrmax(k) = r**2
            end do

      contains

            function rmax(shell, thresh)
                  double precision             :: rmax

                  integer, intent(in)          :: shell
                  double precision, intent(in) :: thresh

                  double precision :: alpha
                  double precision :: int1, int2
                  double precision :: r, a, c
                  integer          :: l, m, n
                  integer          :: momentum, ntypes, np
                  integer          :: idx
                  integer          :: u

                  momentum = shtype(shell)
                  ntypes = nfunc(momentum)
                  np = nprm(shell)
                  alpha = expn(1, shell)
                  idx = 1
                  !
                  ! Find the smallest exponent
                  !
                  do u = 2, np
                        if (expn(u, shell) .lt. alpha) then
                              alpha = expn(u, shell)
                              idx = u
                        end if
                  end do
                  !
                  ! Loop over angular functions
                  !
                  rmax = zero
                  do u = 1, ntypes
                        l = ll(u, momentum)
                        m = mm(u, momentum)
                        n = nn(u, momentum)
                        !
                        ! Calculate
                        ! c = CONTRACTION COEFF
                        !     * SPHERICAL PART OF || X^L Y^M Z^N * EXP(-ALPHA * R^2) ||
                        !     (INTEGRATION OVER SPHERE)
                        !     / (4 * PI)
                        !
                        c = abs(nrml(u, idx, shell))
                        ! c = zero
                        ! do v = 1, np
                        !       cc = abs(nrml(u, v, shell))
                        !       if (cc .gt. c) c = cc
                        ! end do

                        call int_sincos(2 * (l + m) + 1, 2 * n, zero, pi, int1)
                        call int_sincos(2 * m, 2 * l, zero, two * pi, int2)

                        c = c * sqrt(int1 * int2) / (four * pi)
                        a = thresh / c
                        !
                        ! Solve r**(2 + l + m + n) * exp(-alpha * r**2) = a
                        !
                        r = solve(alpha, 2 + l + m + n, a)
                        if (r .gt. rmax) rmax = r
                  end do
            end function rmax


            function solve(alpha, m, a)
                  ! -----------------------------------
                  ! Solve equation
                  ! r**m * exp(-alpha * r**2) - a = 0
                  ! in terms of r.
                  ! -----------------------------------
                  double precision :: solve

                  double precision, intent(in) :: alpha
                  integer, intent(in)          :: m
                  double precision, intent(in) :: a

                  double precision :: xleft, xright, x, y
                  double precision :: deltax
                  logical          :: conv

                  double precision, parameter :: eps = 1.d-12

                  if (m .eq. 0) then
                        xleft = zero
                  else
                        xleft = sqrt(dble(m) / (two * alpha))
                  end if
                  deltax = one / alpha
                  !
                  ! Calculate left and right starting
                  ! points for bisection algorithm
                  !
                  conv = .false.
                  do while (.not. conv)
                        xright = xleft + deltax
                        y = xright**m * exp(-alpha * xright**2)
                        if (y .lt. a) then
                              conv = .true.
                        else
                              xleft = xright
                        end if
                  end do
                  !
                  ! Bisection algorithm
                  !
                  do while (.not. (abs(xleft - xright) .lt. eps))
                        x = (xleft + xright) / two
                        y = x**m * exp(-alpha * x**2)
                        if (y .gt. a) then
                              xleft = x
                        else
                              xright = x
                        end if
                  end do

                  solve = xright
            end function solve
      end subroutine gridscreen


      subroutine data_load_2(System)
            type(TSystem), intent(in) :: System

            integer :: i, j, k, l, s, t
            double precision, dimension(:), allocatable :: shradius
            double precision, dimension(3) :: d, ri, rj
            double precision :: dr
            integer :: izsum
            integer :: n
            integer, dimension(KNOWN_ELEMENTS) :: maxl, atomshell
            integer, dimension(:), allocatable :: AtomElementMap
            integer, dimension(:), allocatable :: ZList
            integer, dimension(:), allocatable :: ZCount
            integer :: NElements
            integer :: gto_lmax
            integer :: nopenela, nopenelb
            integer :: ecpdiff
            real(F64) :: msum

            allocate(AtomElementMap(System%NAtoms))
            NATOM = System%NAtoms
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_ALL_ATOMS)
            allocate(dnuclz(NATOM))
            allocate(inuclz(NATOM))
            allocate(atoml(NATOM))
            allocate(dist(NATOM, NATOM))
            allocate(distun(NATOM, NATOM))
            allocate(idist(NATOM, NATOM))
            allocate(atomr(3, NATOM))
            allocate(idx(NATOM + 1))
            allocate(sh0(NATOM + 1))
            allocate(ELEMENT(NElements))
            !
            ! Determine elements present in the
            ! currently loaded molecule
            !
            NELEMENT = NElements
            ELEMENT(1:NELEMENT) = ZList(1:NELEMENT)
            nshell = 0
            nushell = 0
            do i = 1, NELEMENT
                  call querybasis(BASIS_SET_PATH, ZList(i), atomshell(i), maxl(i))
                  nshell = nshell + ZCount(i) * atomshell(i)
                  nushell = nushell + atomshell(i)
            end do
            gto_lmax = maxval(maxl(1:NELEMENT))
            max_atomnshell = maxval(atomshell(1:NElements))
            allocate(shtype(nushell))
            allocate(nprm(nushell))
            allocate(expn(max_nprm, nushell))
            allocate(CNTR(max_nprm, nushell))
            allocate(CNTRNORM(nfunc(gto_lmax), NUSHELL))
            allocate(nrml(max_nfunc, max_nprm, nushell))
            allocate(shpos(nshell + 1))
            allocate(sh(nshell))
            allocate(r2max(nushell))

            INUCLZ = System%ZNumbers
            ATOMR = System%AtomCoords
            REAL_ATOMS = System%RealAtoms
            CHARGE = real(System%Charge, F64)
            MULTIPLICITY = System%Mult
            NE = System%NElectrons
            NOPENELA = 0
            NOPENELB = 0
            ROKS_NOPENORB = 0
            ROKS_NOPENELA = 0
            ROKS_NOPENELB = 0
            ROKS_NE = 0
            !
            ! If an open shell is present, set the appropriate flags
            ! to enable open-shell computations.
            !
            if (ROKS_NOPENORB > 0) then
                  ROKS_ENABLED = .true.
                  !
                  ! In open-shell case Coulomb and HF exchange
                  ! matrices are stored in separate matrices
                  !
                  SEPKSCONTRIB = .true.
            else
                  ROKS_ENABLED = .false.
                  SEPKSCONTRIB = .false.
            end if
            !
            ! ECP parameters
            !
            call ecp_init(gto_lmax, ELEMENT, NELEMENT, ECP_PARAMS_PATH, ECP_GRAD)
            !
            ! Remove electrons represented by ECP
            !
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        ecpdiff = ECP_INUCLZ(i) - INUCLZ(i)
                        NE = NE + ecpdiff
                        ROKS_NE = ROKS_NE + ROKS_NEUNIT * ecpdiff
                  end do
            end do
            !
            ! Determine maximum angular momentum for
            ! each atom in the loaded basis set
            !
            do i = 1, NATOM
                  dnuclz(i) = dble(inuclz(i))
                  atomtypes: do j = 1, NElements
                        if (ZList(j) .eq. inuclz(i)) then
                              atoml(i) = maxl(j)
                              exit atomtypes
                        end if
                  end do atomtypes
            end do

            allocate(ELEMENT_IDX(NATOM))
            ELEMENT_IDX = AtomElementMap

            do i = 0, max_l
                  call cartpoly(i, ll(:, i), mm(:, i), nn(:, i))
            end do

            sh0(1) = 1
            do i = 1, NATOM
                  atomtypes2: do j = 1, NElements
                        if (ZList(j) .eq. inuclz(i)) then
                              sh0(i + 1) = sh0(i) + atomshell(j)
                              exit atomtypes2
                        end if
                  end do atomtypes2
            end do

            j = 1
            do i = 1, NElements
                  call getbasis(BASIS_SET_PATH, ZList(i), cntr(:, j:), &
                        expn(:, j:), shtype(j:), nprm(j:), atomshell(i))
                  do k = 1, NATOM
                        if (inuclz(k) .eq. ZList(i)) then
                              do l = 0, atomshell(i) - 1
                                    sh(sh0(k) + l) = j + l
                              end do
                        end if
                  end do

                  j = j + atomshell(i)
            end do

            do i = 1, nushell
                  call normalize(i)
            end do
            !
            ! Calculate sqares of radii where orbitals are non-negligible
            !
            call gridscreen(r2max)
            !
            ! Sort shell indices for every atom according to R2MAX,
            ! in decreasing order. Sorting is performed only within
            ! atomic orbitals belonging to one atom at a time
            !
            allocate(shradius(max_atomnshell))
            do i = 1, NATOM
                n = sh0(i + 1) - sh0(i)
                k = sh0(i)
                do j = k, sh0(i + 1) - 1
                    s = sh(j)
                    shradius(j - k + 1) = -r2max(s)
                end do
                call dsort(shradius, sh(k:), n)
            end do
            deallocate(shradius)

            NORB = 0
            do i = 1, NATOM
                  idx(i) = NORB + 1
                  do s = sh0(i), sh0(i + 1) - 1
                        shpos(s) = NORB + 1
                        NORB = NORB + nfunc(shtype(sh(s)))
                  end do
            end do
            shpos(nshell + 1) = NORB + 1
            idx(NATOM + 1) = NORB + 1

            if (NATOM .gt. 1) then
                  do i = 1, NATOM
                        ri = atomr(:, i)
                        dist(i, i) = zero
                        do j = 1, i - 1
                              rj = atomr(:, j)
                              d = ri - rj
                              dr = norm2(d)
                              dist(i, j) = dr
                              dist(j, i) = dr
                        end do
                  end do

                  distun(:, :) = dist
                  !
                  ! For every nucleus J sort all
                  ! remaining nuclei according
                  ! to DIST(I, J)
                  !
                  do j = 1, NATOM
                        do i = 1, NATOM
                              idist(i, j) = i
                        end do

                        call dsort(dist(:, j), idist(:, j), NATOM)
                  end do
            else
                  dist(1, 1) = zero
                  distun(1, 1) = zero
                  idist(1, 1) = 1
            end if
            !
            ! Calculate charge center
            !
            izsum = 0
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        izsum = izsum + ECP_INUCLZ(i)
                  end do
            end do
            chcenter = zero
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        chcenter = chcenter + dble(ECP_INUCLZ(i)) * atomr(:, i)
                  end do
            end do
            chcenter = chcenter / dble(izsum)
            !
            ! Establish center according to the input request
            !
            if(ORIG==CHCTR)then
                  origin = chcenter
            else if(ORIG==MCTR)then                  
                  !
                  ! Calculate mass center
                  !
                  if(ATOMIC_MASS==HI)then
                        mcenter = zero
                        msum = zero
                        do t = 1, 2
                              do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                                    mcenter = mcenter + atomic_mass_hi(INUCLZ(i)) * atomr(:, i)
                                    msum = msum + atomic_mass_hi(INUCLZ(i))
                              end do
                        end do
                        mcenter = mcenter / msum
                  else if(ATOMIC_MASS==AV)then
                        mcenter = zero
                        msum = zero
                        do t = 1, 2
                              do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                                    mcenter = mcenter + atomic_mass_av(INUCLZ(i)) * atomr(:, i)
                                    msum = msum + atomic_mass_av(INUCLZ(i))
                              end do
                        end do
                        mcenter = mcenter / msum
                  else
                        call msg("INCORRECT ATOMIC MASSES INPUT")
                        stop
                  end if
                  origin = mcenter
            end if
            !
            ! Calculate nuclear contribution to dipole moment
            !
            nucldip = zero
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        nucldip = nucldip + dble(ECP_INUCLZ(i)) * (atomr(:, i) - origin)
                  end do
            end do
            !                                                                                                                  
            ! Calculate nuclear contribution to quadrupole moment
            ! as defined by Buckingham relative to the center of mass
            ! 1. Buckingham, A. D., Permanent and induced molecular moments
            !    and long-range intermolecular forces. Adv. Chem. Phys., 107, 12 (1967)
            ! 2. L. Piela "Ideas of Quantum chemistry" eq. (12.7)
            !
            nuclquad = zero
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        nuclquad(1) = nuclquad(1) + 0.5d+0 * dble(ECP_INUCLZ(i)) * &
                              (2.d+0 * atomr_mc2(1, i)- atomr_mc2(2, i)- atomr_mc2(3, i))
                        nuclquad(2) = nuclquad(2) + 0.5d+0 * dble(ECP_INUCLZ(i)) * &
                              (2.d+0 * atomr_mc2(2, i)- atomr_mc2(1, i) - atomr_mc2(3, i))
                        nuclquad(3) = nuclquad(3) + 0.5d+0 * dble(ECP_INUCLZ(i)) * &
                              (2.d+0 * atomr_mc2(3, i) - atomr_mc2(2, i) - atomr_mc2(1, i))
                        nuclquad(4) = nuclquad(4) + 1.5d+0 * dble(ECP_INUCLZ(i)) * &
                              atomr_mc(1, i) * atomr_mc(2, i)
                        nuclquad(5) = nuclquad(5) + 1.5d+0 * dble(ECP_INUCLZ(i)) * &
                              atomr_mc(1, i) * atomr_mc(3, i)
                        nuclquad(6) = nuclquad(6) + 1.5d+0 * dble(ECP_INUCLZ(i)) * &
                              atomr_mc(2, i) * atomr_mc(3, i)
                  end do
            end do
            !
            ! Store shell centers to simplify loops over
            ! two-electron integrals
            !
            allocate(SHATOM(NSHELL))
            do i = 1, NATOM
                  do j = SH0(i), SH0(i+1)-1
                        SHATOM(j) = i
                  end do
            end do
            !
            ! Summary of loaded data
            !
            call toprule()
            call msg(cfield("SCF for " // sys_ChemicalFormula(System), 76))
            call midrule()
            call imsg("NUMBER OF ATOMS", NATOM)
            if (isint(CHARGE)) then
                  call imsg("CHARGE", nint(CHARGE))
            else
                  call dmsg("CHARGE", CHARGE, fmt="F20.3")
            end if
            call smsg("BASIS SET", BASIS_SET_NAME)
            if (SPHERBASIS) then
                  call smsg("SOLID HARMONICS BASIS", "ENABLED")
            else
                  call smsg("SOLID HARMONICS BASIS", "DISABLED")
            end if
            call imsg("NUMBER OF CARTESIAN GAUSSIAN ORBITALS", NORB)
            
            if (POINT_GROUP == C2v) then
                  call smsg("MOLECULE POINT GROUP", "C2v")
            else if (POINT_GROUP == D2h) then
                  call smsg("MOLECULE POINT GROUP", "D2h")
            end if
      end subroutine data_load_2


      subroutine data_load(xyz)
            type(tmolecule), intent(in) :: xyz

            integer :: i, j, k, l, s, t
            double precision, dimension(:), allocatable :: shradius
            double precision, dimension(3) :: d, ri, rj
            double precision :: dr
            integer :: izsum

            integer, dimension(KNOWN_ELEMENTS) :: ntype, itype, maxl, atomshell
            integer :: n
            integer :: gto_lmax
            integer :: nopenela, nopenelb
            integer :: ecpdiff
            real(F64) :: msum
      
            call querymolecule(xyz, NATOM, n, ntype, itype)
            allocate(dnuclz(NATOM))
            allocate(inuclz(NATOM))
            allocate(atoml(NATOM))
            allocate(dist(NATOM, NATOM))
            allocate(distun(NATOM, NATOM))
            allocate(idist(NATOM, NATOM))
            allocate(atomr(3, NATOM))
            allocate(idx(NATOM + 1))
            allocate(sh0(NATOM + 1))
            allocate(ELEMENT(n))
            !
            ! Determine elements present in the
            ! currently loaded molecule
            !
            NELEMENT = n
            ELEMENT(1:NELEMENT) = itype(1:NELEMENT)
            
            nshell = 0
            nushell = 0

            do i = 1, NELEMENT
                  call querybasis(BASIS_SET_PATH, itype(i), atomshell(i), maxl(i))
                  nshell = nshell + ntype(i) * atomshell(i)
                  nushell = nushell + atomshell(i)
            end do

            gto_lmax = maxval(maxl(1:NELEMENT))
            max_atomnshell = maxval(atomshell(1:n))

            allocate(shtype(nushell))
            allocate(nprm(nushell))
            allocate(expn(max_nprm, nushell))
            allocate(CNTR(max_nprm, nushell))
            allocate(CNTRNORM(nfunc(gto_lmax), NUSHELL))
            allocate(nrml(max_nfunc, max_nprm, nushell))
            allocate(shpos(nshell + 1))
            allocate(sh(nshell))
            allocate(r2max(nushell))

            call loadmolecule(xyz, INUCLZ, ATOMR, REAL_ATOMS, CHARGE, &
                  MULTIPLICITY, NE, ROKS_NOPENORB, nopenela, nopenelb, ROKS_NE, &
                  ROKS_NOPENELA, ROKS_NOPENELB)
            !
            ! If an open shell is present, set the appropriate flags
            ! to enable open-shell computations.
            !
            if (ROKS_NOPENORB > 0) then
                  ROKS_ENABLED = .true.
                  !
                  ! In open-shell case Coulomb and HF exchange
                  ! matrices are stored in separate matrices
                  !
                  SEPKSCONTRIB = .true.
            else
                  ROKS_ENABLED = .false.
                  SEPKSCONTRIB = .false.
            end if
            !
            ! ECP parameters
            !
            call ecp_init(gto_lmax, ELEMENT, NELEMENT, ECP_PARAMS_PATH, ECP_GRAD)
            !
            ! Remove electrons represented by ECP
            !
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        ecpdiff = ECP_INUCLZ(i) - INUCLZ(i)
                        NE = NE + ecpdiff
                        ROKS_NE = ROKS_NE + ROKS_NEUNIT * ecpdiff
                  end do
            end do
            !
            ! Determine maximum angular momentum for
            ! each atom in the loaded basis set
            !
            do i = 1, NATOM
                  dnuclz(i) = dble(inuclz(i))
                  atomtypes: do j = 1, n
                        if (itype(j) .eq. inuclz(i)) then
                              atoml(i) = maxl(j)
                              exit atomtypes
                        end if
                  end do atomtypes
            end do

            allocate(ELEMENT_IDX(NATOM))
            do k = 1, NELEMENT
                  do l = 1, NATOM
                        if (INUCLZ(l) == ELEMENT(k)) then
                              ELEMENT_IDX(l) = k
                        end if
                  end do
            end do

            do i = 0, max_l
                  call cartpoly(i, ll(:, i), mm(:, i), nn(:, i))
            end do

            sh0(1) = 1
            do i = 1, NATOM
                  atomtypes2: do j = 1, n
                        if (itype(j) .eq. inuclz(i)) then
                              sh0(i + 1) = sh0(i) + atomshell(j)
                              exit atomtypes2
                        end if
                  end do atomtypes2
            end do

            j = 1
            do i = 1, n
                  call getbasis(BASIS_SET_PATH, itype(i), cntr(:, j:), &
                        expn(:, j:), shtype(j:), nprm(j:), atomshell(i))
                  do k = 1, NATOM
                        if (inuclz(k) .eq. itype(i)) then
                              do l = 0, atomshell(i) - 1
                                    sh(sh0(k) + l) = j + l
                              end do
                        end if
                  end do

                  j = j + atomshell(i)
            end do

            do i = 1, nushell
                  call normalize(i)
            end do
            !
            ! Calculate sqares of radii where orbitals are non-negligible
            !
            call gridscreen(r2max)
            !
            ! Sort shell indices for every atom according to R2MAX,
            ! in decreasing order. Sorting is performed only within
            ! atomic orbitals belonging to one atom at a time
            !
            allocate(shradius(max_atomnshell))
            do i = 1, NATOM
                n = sh0(i + 1) - sh0(i)
                k = sh0(i)
                do j = k, sh0(i + 1) - 1
                    s = sh(j)
                    shradius(j - k + 1) = -r2max(s)
                end do
                call dsort(shradius, sh(k:), n)
            end do
            deallocate(shradius)

            NORB = 0
            do i = 1, NATOM
                  idx(i) = NORB + 1
                  do s = sh0(i), sh0(i + 1) - 1
                        shpos(s) = NORB + 1
                        NORB = NORB + nfunc(shtype(sh(s)))
                  end do
            end do
            shpos(nshell + 1) = NORB + 1
            idx(NATOM + 1) = NORB + 1

            if (NATOM .gt. 1) then
                  do i = 1, NATOM
                        ri = atomr(:, i)
                        dist(i, i) = zero
                        do j = 1, i - 1
                              rj = atomr(:, j)
                              d = ri - rj
                              dr = norm2(d)
                              dist(i, j) = dr
                              dist(j, i) = dr
                        end do
                  end do

                  distun(:, :) = dist
                  !
                  ! For every nucleus J sort all
                  ! remaining nuclei according
                  ! to DIST(I, J)
                  !
                  do j = 1, NATOM
                        do i = 1, NATOM
                              idist(i, j) = i
                        end do

                        call dsort(dist(:, j), idist(:, j), NATOM)
                  end do
            else
                  dist(1, 1) = zero
                  distun(1, 1) = zero
                  idist(1, 1) = 1
            end if
            !
            ! Calculate charge center
            !
            izsum = 0
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        izsum = izsum + ECP_INUCLZ(i)
                  end do
            end do
            chcenter = zero
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        chcenter = chcenter + dble(ECP_INUCLZ(i)) * atomr(:, i)
                  end do
            end do
            chcenter = chcenter / dble(izsum)
            !
            ! Establish center according to the input request
            !
            if(ORIG==CHCTR)then
                  origin = chcenter
            else if(ORIG==MCTR)then                  
                  !
                  ! Calculate mass center
                  !
                  if(ATOMIC_MASS==HI)then
                        mcenter = zero
                        msum = zero
                        do t = 1, 2
                              do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                                    mcenter = mcenter + atomic_mass_hi(INUCLZ(i)) * atomr(:, i)
                                    msum = msum + atomic_mass_hi(INUCLZ(i))
                              end do
                        end do
                        mcenter = mcenter / msum
                  else if(ATOMIC_MASS==AV)then
                        mcenter = zero
                        msum = zero
                        do t = 1, 2
                              do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                                    mcenter = mcenter + atomic_mass_av(INUCLZ(i)) * atomr(:, i)
                                    msum = msum + atomic_mass_av(INUCLZ(i))
                              end do
                        end do
                        mcenter = mcenter / msum
                  else
                        call msg("INCORRECT ATOMIC MASSES INPUT")
                        stop
                  end if
                  origin = mcenter
            end if
            !
            ! Calculate nuclear contribution to dipole moment
            !
            nucldip = zero
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        nucldip = nucldip + dble(ECP_INUCLZ(i)) * (atomr(:, i) - origin)
                  end do
            end do
            !                                                                                                                  
            ! Calculate nuclear contribution to quadrupole moment
            ! as defined by Buckingham relative to the center of mass
            ! 1. Buckingham, A. D., Permanent and induced molecular moments
            !    and long-range intermolecular forces. Adv. Chem. Phys., 107, 12 (1967)
            ! 2. L. Piela "Ideas of Quantum chemistry" eq. (12.7)
            !
            nuclquad = zero
            do t = 1, 2
                  do i = REAL_ATOMS(1, t), REAL_ATOMS(2, t)
                        nuclquad(1) = nuclquad(1) + 0.5d+0 * dble(ECP_INUCLZ(i)) * &
                              (2.d+0 * atomr_mc2(1, i)- atomr_mc2(2, i)- atomr_mc2(3, i))
                        nuclquad(2) = nuclquad(2) + 0.5d+0 * dble(ECP_INUCLZ(i)) * &
                              (2.d+0 * atomr_mc2(2, i)- atomr_mc2(1, i) - atomr_mc2(3, i))
                        nuclquad(3) = nuclquad(3) + 0.5d+0 * dble(ECP_INUCLZ(i)) * &
                              (2.d+0 * atomr_mc2(3, i) - atomr_mc2(2, i) - atomr_mc2(1, i))
                        nuclquad(4) = nuclquad(4) + 1.5d+0 * dble(ECP_INUCLZ(i)) * &
                              atomr_mc(1, i) * atomr_mc(2, i)
                        nuclquad(5) = nuclquad(5) + 1.5d+0 * dble(ECP_INUCLZ(i)) * &
                              atomr_mc(1, i) * atomr_mc(3, i)
                        nuclquad(6) = nuclquad(6) + 1.5d+0 * dble(ECP_INUCLZ(i)) * &
                              atomr_mc(2, i) * atomr_mc(3, i)
                  end do
            end do
            !
            ! Store shell centers to simplify loops over
            ! two-electron integrals
            !
            allocate(SHATOM(NSHELL))
            do i = 1, NATOM
                  do j = SH0(i), SH0(i+1)-1
                        SHATOM(j) = i
                  end do
            end do
            !
            ! Summary of loaded data
            !
            call toprule()
            call msg("LOADED NEW SYSTEM: " // chemical_formula(xyz))
            call midrule()
            call imsg("NUMBER OF ATOMS", NATOM)
            if (isint(CHARGE)) then
                  call imsg("CHARGE", nint(CHARGE))
            else
                  call dmsg("CHARGE", CHARGE, fmt="F20.3")
            end if
            call smsg("BASIS SET", BASIS_SET_NAME)
            if (SPHERBASIS) then
                  call smsg("SOLID HARMONICS BASIS", "ENABLED")
            else
                  call smsg("SOLID HARMONICS BASIS", "DISABLED")
            end if
            call imsg("NUMBER OF CARTESIAN GAUSSIAN ORBITALS", NORB)
            
            if (POINT_GROUP == C2v) then
                  call smsg("MOLECULE POINT GROUP", "C2v")
            else if (POINT_GROUP == D2h) then
                  call smsg("MOLECULE POINT GROUP", "D2h")
            end if
      end subroutine data_load


      function atomr_mc2(q, i)
            double precision :: atomr_mc2
            integer, intent(in) :: q
            integer, intent(in) :: i
            
            atomr_mc2 = (atomr(q, i) - origin(q))**2
      end function atomr_mc2


      function atomr_mc(q, i)
            double precision :: atomr_mc
            integer, intent(in) :: q
            integer, intent(in) :: i
            
            atomr_mc = atomr(q, i) - origin(q)
      end function atomr_mc


      subroutine data_free()
            deallocate(dnuclz)
            deallocate(inuclz)
            deallocate(atoml)
            deallocate(dist)
            deallocate(distun)
            deallocate(idist)
            deallocate(atomr)
            deallocate(idx)
            deallocate(sh0)
            deallocate(shtype)
            deallocate(nprm)
            deallocate(expn)
            deallocate(CNTR)
            deallocate(CNTRNORM)
            deallocate(nrml)
            deallocate(shpos)
            deallocate(sh)
            deallocate(r2max)
            deallocate(ELEMENT)
            deallocate(ELEMENT_IDX)
            deallocate(SHATOM)
            call ecp_free()
      end subroutine data_free
end module basis
