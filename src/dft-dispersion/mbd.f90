module mbd
      use math_constants
      use arithmetic
      use display
      use string
      use linalg
      use mbd_params
      use periodic
      use h_xcfunc

      implicit none

contains

      subroutine edisp_ardt2014(energy, coords, nuclz, volume_ratios, ixcmodel, &
            beta_userdef)
            ! ----------------------------------------------------------------
            ! Compute the many-body dispersion (MBD) energy based on
            ! the range-separated self-consistent screening equation (rsSCS)
            ! proposed by Ambrosetti et al. in [1].
            ! ----------------------------------------------------------------
            ! 1. Ambrosetti, A., Reilly, A.M., DiStasio Jr., R.A.,
            !    Tkatchenko, A., Long-range correlation energy 
            !    calculated from coupled atomic response functions,
            !    J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
            !
            real(F64), intent(out)                 :: energy
            real(F64), dimension(:, :), intent(in) :: coords
            integer, dimension(:), intent(in)      :: nuclz
            real(F64), dimension(:), intent(in)    :: volume_ratios
            integer, intent(in)                    :: ixcmodel
            real(F64), intent(in)                  :: beta_userdef
            
            real(F64), dimension(:), allocatable :: alpha_hirsh, alpha_scs
            real(F64), dimension(:), allocatable :: rvdw_hirsh, rvdw_scs
            real(F64), dimension(:, :), allocatable :: scs_matrix
            real(F64), dimension(:), allocatable :: c6_scs
            real(F64), dimension(:), allocatable :: omega_scs
            real(F64), dimension(:), allocatable :: eig_mbd

            integer :: natoms
            integer :: k, l
            real(F64) :: beta
            real(F64) :: omega, weight
            real(F64) :: s0, s1
            
            natoms = size(coords, dim=2)

            if (natoms == 1) then
                  energy = ZERO
                  return
            end if

            if (natoms .ne. size(nuclz) .or. natoms > size(volume_ratios)) then
                  call msg("Inconsistent array dimensions on entry to edisp_ardt2014", MSG_ERROR)
                  stop
            end if
            
            allocate(alpha_hirsh(natoms))
            allocate(alpha_scs(natoms))
            allocate(rvdw_hirsh(natoms))
            allocate(rvdw_scs(natoms))
            allocate(scs_matrix(3 * natoms, 3 * natoms))
            allocate(c6_scs(natoms))
            allocate(omega_scs(natoms))
            allocate(eig_mbd(3 * natoms))

            if (beta_userdef < ZERO) then
                  call ardt2014_beta(beta, ixcmodel)
            else
                  beta = beta_userdef
            end if
            call ardt2014_display_header(beta)
            call hirshfeld_atom_rvdw(rvdw_hirsh, nuclz, volume_ratios)
            c6_scs = ZERO

            do k = 1, QUAD_NFREQ
                  omega = QUAD_FREQ(k)
                  weight = QUAD_WEIGHT(k)
                  call hirshfeld_atom_alpha(alpha_hirsh, omega, nuclz, volume_ratios)
                  call scs_matrix_ardt2014_build(scs_matrix, coords, alpha_hirsh, rvdw_hirsh, beta)
                  call scs_matrix_invert(scs_matrix)
                  call scs_polarizability(alpha_scs, scs_matrix)
                  do l = 1, natoms
                        c6_scs(l) = c6_scs(l) + weight * alpha_scs(l)**2
                  end do
            end do

            c6_scs = THREE / PI * c6_scs
            !
            ! Polarizabilities for zero frequency
            !
            call hirshfeld_atom_alpha(alpha_hirsh, ZERO, nuclz, volume_ratios)
            call scs_matrix_ardt2014_build(scs_matrix, coords, alpha_hirsh, rvdw_hirsh, beta)
            call scs_matrix_invert(scs_matrix)
            call scs_polarizability(alpha_scs, scs_matrix)
            call ardt2014_display_scs(alpha_scs, c6_scs, nuclz)
            !
            ! Characteristic frequencies and van der Waals radii
            ! corresponding to the SCS polarizabilities
            !
            call scs_omega_rvdw(omega_scs, rvdw_scs, c6_scs, alpha_scs, nuclz)
            !
            ! Build and solve the ARDT2014 model eigensystem
            !
            call hamiltonian_ardt2014(eig_mbd, alpha_scs, omega_scs, rvdw_scs, coords, beta)
            !
            ! MBD energy, Eq. A3 in [1]
            !
            s0 = ZERO
            do k = 1, natoms
                  s0 = s0 + omega_scs(k)
            end do
            s1 = ZERO
            do k = 1, 3 * natoms
                  s1 = s1 + sqrt(eig_mbd(k))
            end do
            energy = FRAC12 * s1 - FRAC32 * s0

            call dmsg("MBD-rsSCS DISPERSION [a.u.]", energy)
            call dmsg("MBD-rsSCS DISPERSION [kcal/mol]", tokcal(energy))
            call blankline()
      end subroutine edisp_ardt2014


      subroutine ardt2014_display_header(beta)
            real(F64), intent(in) :: beta
            
            call msg("MBD-rsSCS dispersion correction", underline=.true.)
            call msg("J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104")
            call dmsg("Damping parameter (beta)", beta, fmt="F6.4")
      end subroutine ardt2014_display_header


      subroutine ardt2014_display_scs(alpha_scs, c6_scs, nuclz)
            real(F64), dimension(:), intent(in) :: alpha_scs
            real(F64), dimension(:), intent(in) :: c6_scs
            integer, dimension(:), intent(in)   :: nuclz
            
            integer :: k
            integer :: natoms
            character(34) :: line

            natoms = size(nuclz)
            call msg("SCS POLARIZABILITIES AND C6 COEFFICIENTS")
            write(line, "(A2,1X,A15,1X,A15)") "# ", "alpha [a.u.]", "C6 [a.u.]"
            call msg(line, underline=.true.)
            do k = 1, natoms
                  write(line, "(A2,1X,F15.6,1X,F15.6)") ELNAME_SHORT(nuclz(k)), alpha_scs(k), c6_scs(k)
                  call msg(line)
            end do
            call blankline()
      end subroutine ardt2014_display_scs


      subroutine ardt2014_beta(beta, ixcmodel)
            real(F64), intent(out) :: beta
            integer, intent(in)    :: ixcmodel

            select case (ixcmodel)
            case (XCF_XC_PBE)
                  beta = 0.83_F64
            case (XCF_XC_PBE0)
                  beta = 0.85_F64
            case (XCF_XC_MCS)
                  beta = 0.8032928699_F64
            case (XCF_XC_MCSH)
                  beta = 0.7241720448_F64
            case (XCF_XC_EC_PBE_TPSS)
                  !
                  ! Date: 23.12.2015
                  ! omega=0.350
                  ! srexx=0.000
                  ! Objective function: RMSD on the S22 set
                  ! RMSD = 0.253 kcal/mol
                  ! MAD = 0.203 kcal/mol
                  ! MAPD = 5.14%
                  ! MSD = -0.053 kcal/mol
                  !
                  beta = 0.70066_F64
            case (XCF_XC_EC_PBE)
                  !
                  ! Date: 23.12.2015
                  ! omega=0.350
                  ! srexx=0.000
                  ! Objective function: RMSD on the S22 set
                  ! RMSD = 0.249 kcal/mol
                  ! MAD = 0.203 kcal/mol
                  ! MAPD = 4.87%
                  ! MSD = -0.041 kcal/mol
                  !
                  beta = 0.73262_F64
            case default
                  call msg("Damping parameter unavailable for the selected functional", &
                        MSG_ERROR)
                  error stop
            end select
      end subroutine ardt2014_beta


      pure subroutine mbd_DampingFunction(Fsr, Flr, Rab, RvdwA, RvdwB, beta)
            !
            ! Compute the short- (Fsr) and long-range (Flr) parts of the damping function.
            ! The short-range part is used Eq. 12 in [1], and the long-range
            ! part is in Eq. 14 in [1].
            !
            ! 1. Ambrosetti, A., Reilly, A.M., DiStasio Jr., R.A.,
            !    Tkatchenko, A., Long-range correlation energy 
            !    calculated from coupled atomic response functions,
            !    J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
            !
            real(F64), intent(out) :: Fsr
            real(F64), intent(out) :: Flr
            real(F64), intent(in)  :: Rab
            real(F64), intent(in)  :: RvdwA
            real(F64), intent(in)  :: RvdwB
            real(F64), intent(in)  :: beta

            real(F64) :: Svdw, y
            !
            ! Fermi damping, Eq. 13 in [1]
            !
            Svdw = beta * (RvdwA + RvdwB)
            y = SIX * (Rab / Svdw - ONE)
            Flr = ONE / (ONE + exp(-y))
            !
            ! 1 - 1 / (1 + exp(-y)) = 1 / (1 + exp(y))
            !
            Fsr = ONE / (ONE + exp(y))
      end subroutine mbd_DampingFunction


      subroutine hamiltonian_ardt2014(eig_mbd, alpha_scs, omega_scs, rvdw_scs, coords, beta)
            ! -----------------------------------------------------------------------
            ! Build and solve the model Hamiltonian for coupled fluctuating dipoles
            ! interacting with the long-range potential, Eq. A2 in [1] and Eq. 1
            ! in [2].
            ! -----------------------------------------------------------------------
            ! 1. Ambrosetti, A., Reilly, A.M., DiStasio Jr., R.A.,
            !    Tkatchenko, A., Long-range correlation energy 
            !    calculated from coupled atomic response functions,
            !    J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
            ! 2. DiStasio, R.A., Lilienfeld, A., Tkatchenko, A., 
            !    PNAS 109, 14791 (2012); doi: 10.1073/pnas.1208121109
            !
            real(F64), dimension(:), contiguous, intent(out) :: eig_mbd
            real(F64), dimension(:), intent(in)              :: alpha_scs
            real(F64), dimension(:), intent(in)              :: omega_scs
            real(F64), dimension(:), intent(in)              :: rvdw_scs
            real(F64), dimension(:, :), intent(in)           :: coords
            real(F64), intent(in)                            :: beta

            real(F64), dimension(3, 3) :: tpq
            real(F64), dimension(:, :), allocatable :: c
            real(F64) :: w
            integer :: i, j, a, b, a0, b0
            integer :: natoms
            integer :: lwork, liwork
            real(F64), dimension(:), allocatable :: work
            integer, dimension(:), allocatable :: iwork
            
            natoms = size(coords, dim=2)
            allocate(c(3 * natoms, 3 * natoms))

            do j = 1, natoms
                  do i = j, natoms
                        if (i == j) then
                              tpq = ZERO
                              do a = 1, 3
                                    tpq(a, a) = omega_scs(j)**2
                              end do
                        else
                              call tpq_ardt2014_longrange(tpq, i, j, coords, rvdw_scs, beta)
                              w = omega_scs(i) * omega_scs(j) * sqrt(alpha_scs(i) * alpha_scs(j))
                              tpq = w * tpq
                        end if
                        a0 = 3 * (i - 1)
                        b0 = 3 * (j - 1)
                        do b = 1, 3
                              do a = 1, 3
                                    c(a0 + a, b0 + b) = tpq(a, b)
                              end do
                        end do
                  end do
            end do
            
           call dsyevdquery(3 * natoms, lwork, liwork, "N")
           allocate(work(lwork))
           allocate(iwork(liwork))
           call dsyevdwrap(c, eig_mbd, 3 * natoms, "N", work, iwork)
     end subroutine hamiltonian_ardt2014


      subroutine scs_omega_rvdw(omega_scs, rvdw_scs, c6_scs, alpha_scs, nuclz)
            real(F64), dimension(:), intent(out) :: omega_scs
            real(F64), dimension(:), intent(out) :: rvdw_scs
            real(F64), dimension(:), intent(in)  :: c6_scs
            real(F64), dimension(:), intent(in)  :: alpha_scs
            integer, dimension(:), intent(in)    :: nuclz
            
            integer :: natoms
            integer :: k
            real(F64) :: alpha, c6, rvdw
            
            natoms = size(nuclz)
            
            do k = 1, natoms
                  call free_atom_params(alpha, c6, rvdw, nuclz(k))
                  rvdw_scs(k) = rvdw * (alpha_scs(k)/alpha)**FRAC13
                  omega_scs(k) = char_freq(c6_scs(k), alpha_scs(k))
            end do
      end subroutine scs_omega_rvdw


      pure function sigma(alpha)
            ! ------------------------------------------------
            ! Compute width of a quantum harmonic oscillator,
            ! Eq. 9 in [1].
            ! ------------------------------------------------
            ! 1. Mayer, A., Phys. Rev. B 75, 45407 (2007);
            !    doi: 10.1103/PhysRevB.75.045407
            !
            real(F64)             :: sigma
            real(F64), intent(in) :: alpha

            sigma = (FRAC13 * sqrt(TWO/PI) * alpha)**FRAC13
      end function sigma

      
      pure function char_freq(c6, alpha)
            ! -----------------------------------------------------------
            ! Compute characteristic frequency (effective frequency)
            ! of a quantum harmonic oscillator, Eq. 5 in [1]
            ! -----------------------------------------------------------
            ! 1. Tkatchenko, A. and Scheffler, A., Phys. Rev. Lett. 
            !    102, 073005 (2009); doi: 10.1103/PhysRevLett.102.073005
            !
            real(F64)             :: char_freq
            real(F64), intent(in) :: c6
            real(F64), intent(in) :: alpha
            
            char_freq = FRAC43 * c6 / alpha**2
      end function char_freq


      subroutine hirshfeld_atom_rvdw(rvdw_hirsh, nuclz, volume_ratios)
            !
            ! Compute van der Waals radii of the Hirshfeld atoms.
            ! The free-atom data rescaled by the Hirshfeld volume
            ! ratio V^{eff}/V^{free}.
            !
            real(F64), dimension(:), intent(out) :: rvdw_hirsh
            integer, dimension(:), intent(in)    :: nuclz
            real(F64), dimension(:), intent(in)  :: volume_ratios

            integer :: natoms
            integer :: k
            real(F64) :: alpha, c6, rvdw

            natoms = size(nuclz)

            do k = 1, natoms
                  call free_atom_params(alpha, c6, rvdw, nuclz(k))
                  rvdw_hirsh(k) = (volume_ratios(k))**FRAC13 * rvdw
            end do
      end subroutine hirshfeld_atom_rvdw


      subroutine hirshfeld_atom_alpha(alpha_hirsh, omega, nuclz, volume_ratios)
            !
            ! Compute the omega-dependent polarizabilities of the Hirshfeld
            ! atoms. The free-atom data are rescaled by the Hirshfeld volume
            ! ratio V^{eff}/V^{free}.
            !
            real(F64), dimension(:), intent(out) :: alpha_hirsh
            real(F64), intent(in)                :: omega
            integer, dimension(:), intent(in)    :: nuclz
            real(F64), dimension(:), intent(in)  :: volume_ratios

            integer :: natoms
            integer :: k
            real(F64) :: alpha, c6, rvdw
            real(F64) :: omega0

            natoms = size(nuclz)

            do k = 1, natoms
                  call free_atom_params(alpha, c6, rvdw, nuclz(k))
                  omega0 = char_freq(c6, alpha)
                  alpha_hirsh(k) = volume_ratios(k) * alpha / (ONE + (omega/omega0)**2)
            end do
      end subroutine hirshfeld_atom_alpha


      subroutine free_atom_params(alpha, c6, rvdw, znum)
            ! --------------------------------------------------------------
            ! Free atom static polarizabilities, C6 parameters, and
            ! van der Waals radii (rvdw). The data are obtained from [1].
            ! --------------------------------------------------------------
            ! 1. Ambrosetti, A., Reilly, A.M., DiStasio Jr., R.A.,
            !    Tkatchenko, A., Long-range correlation energy 
            !    calculated from coupled atomic response functions,
            !    J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
            !
            real(F64), intent(out) :: alpha
            real(F64), intent(out) :: c6
            real(F64), intent(out) :: rvdw
            integer, intent(in)    :: znum

            select case (znum)
                  case (1:56)
                        alpha = FREE_ATOM_ARDT_2014_1(3 * (znum - 1) + 1)
                        c6    = FREE_ATOM_ARDT_2014_1(3 * (znum - 1) + 2)
                        rvdw  = FREE_ATOM_ARDT_2014_1(3 * (znum - 1) + 3)
                  case (72:86)
                        alpha = FREE_ATOM_ARDT_2014_2(3 * (znum - 72) + 1)
                        c6    = FREE_ATOM_ARDT_2014_2(3 * (znum - 72) + 2)
                        rvdw  = FREE_ATOM_ARDT_2014_2(3 * (znum - 72) + 3)
                  case default
                        call msg("FREE-ATOM PARAMETERS MISSING FOR Z = " // str(znum))
                        stop
            end select
      end subroutine free_atom_params

      
      pure subroutine tpq_ardt2014_shortrange(tpq, i, j, coords, alpha_hirsh, &
            rvdw_hirsh, beta)
            ! ---------------------------------------------------------------
            ! Compute the short range T_ij tensor (Eq. 12 in [1]) employed
            ! in the screening equation.
            ! ---------------------------------------------------------------
            ! 1. Ambrosetti, A., Reilly, A.M., DiStasio Jr., R.A.,
            !    Tkatchenko, A., Long-range correlation energy 
            !    calculated from coupled atomic response functions,
            !    J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
            !
            ! 2. Tkatchenko, A., Ambrosetti, A., DiStasio Jr., R.A.,
            !    Interatomic methods for the dispersion energy derived
            !    from the adiabatic connection fluctuation-dissipation
            !    theorem, J. Chem. Phys. 138, 74106 (2013);
            !    doi: 10.1063/1.4789814
            !
            real(F64), dimension(3, 3), intent(out) :: tpq
            integer, intent(in)                     :: i
            integer, intent(in)                     :: j
            real(F64), dimension(:, :), intent(in)  :: coords
            real(F64), dimension(:), intent(in)     :: alpha_hirsh
            real(F64), dimension(:), intent(in)     :: rvdw_hirsh
            real(F64), intent(in)                   :: beta

            real(F64) :: sigma_i, sigma_j, sigma_ij
            real(F64), dimension(3) :: r_ij_vec
            real(F64) :: r_ij
            real(F64) :: c1, c2, c3
            real(F64) :: emw2
            real(F64) :: w, Fsr, Flr
            integer :: a, b
            !
            ! Compute the dipole interaction tensor T_{ij},
            ! Eq. 12 in [2]. T_{ij} will be later multiplied
            ! by the Fermi function.
            !
            sigma_i = sigma(alpha_hirsh(i))
            sigma_j = sigma(alpha_hirsh(j))
            sigma_ij = sqrt(sigma_i**2 + sigma_j**2)
            
            r_ij_vec = coords(:, i) - coords(:, j)
            r_ij = norm2(r_ij_vec)
            r_ij_vec = r_ij_vec / r_ij
            w = r_ij / sigma_ij

            emw2 = exp(-w**2)
            c1 = (erf(w) - TWO / sqrt(PI) * w * emw2) / r_ij**3
            c2 = FOUR / sqrt(PI) * emw2 / sigma_ij**3
            c3 = -THREE * c1 + c2

            do b = 1, 3
                  do a = 1, 3
                        tpq(a, b) = r_ij_vec(a) * r_ij_vec(b) * c3
                  end do
            end do
            
            do a = 1, 3
                  tpq(a, a) = tpq(a, a) + c1
            end do
            call mbd_DampingFunction(Fsr, Flr, r_ij, rvdw_hirsh(i), rvdw_hirsh(j), beta)
            !
            ! Short range T_ij tensor, Eq. 12 in [1]
            !
            tpq = tpq * Fsr
      end subroutine tpq_ardt2014_shortrange


      pure subroutine tpq_ardt2014_longrange(tpq, i, j, coords, rvdw, beta)
            ! -----------------------------------------------------------------
            ! Compute the long-range T_{ij} tensor (Eq. 14 in [1]) employed
            ! in the model Hamiltonian (Eq. A2 in [1]).
            ! -----------------------------------------------------------------
            ! 1. Ambrosetti, A., Reilly, A.M., DiStasio Jr., R.A.,
            !    Tkatchenko, A., Long-range correlation energy 
            !    calculated from coupled atomic response functions,
            !    J. Chem. Phys. 140, 18A508 (2014); doi: 10.1063/1.4865104
            !
            ! 2. Tkatchenko, A., Ambrosetti, A., DiStasio Jr., R.A.,
            !    Interatomic methods for the dispersion energy derived
            !    from the adiabatic connection fluctuation-dissipation
            !    theorem, J. Chem. Phys. 138, 74106 (2013);
            !    doi: 10.1063/1.4789814
            !
            real(F64), dimension(3, 3), intent(out) :: tpq
            integer, intent(in)                     :: i
            integer, intent(in)                     :: j
            real(F64), dimension(:, :), intent(in)  :: coords
            real(F64), dimension(:), intent(in)     :: rvdw
            real(F64), intent(in)                   :: beta

            real(F64), dimension(3) :: r_ij_vec
            real(F64) :: r_ij, r_ij3
            real(F64) :: c1, c2
            integer :: a, b
            real(F64) :: Fsr, Flr
            
            r_ij_vec = coords(:, i) - coords(:, j)
            r_ij = norm2(r_ij_vec)
            r_ij_vec = r_ij_vec / r_ij
            r_ij3 = r_ij**3
            call mbd_DampingFunction(Fsr, Flr, r_ij, rvdw(i), rvdw(j), beta)
            c1 = -THREE * Flr / r_ij3
            c2 = Flr / r_ij3
            do b = 1, 3
                  do a = 1, 3
                        tpq(a, b) = r_ij_vec(a) * r_ij_vec(b) * c1
                  end do
            end do
            do a = 1, 3
                  tpq(a, a) = tpq(a, a) + c2
            end do
      end subroutine tpq_ardt2014_longrange


      pure subroutine scs_matrix_ardt2014_build(w, coords, alpha_hirsh, rvdw_hirsh, beta)
            ! -------------------------------------------------------------------
            ! Build the main matrix of the self-consistent screening method.
            ! The technical details of the SCS equations are discussed in the
            ! supporting info for [1] (see Eqs. 4 and 5 therein).
            ! -------------------------------------------------------------------
            ! 1. Bucko, T., Lebegue, S., Hafner, J., Angyan, J.G., 
            !    Phys. Rev. B. 87, 064110 (2013); doi: 10.1103/PhysRevB.87.064110
            !
            real(F64), dimension(:, :), intent(out) :: w
            real(F64), dimension(:, :), intent(in)  :: coords
            real(F64), dimension(:), intent(in)     :: alpha_hirsh
            real(F64), dimension(:), intent(in)     :: rvdw_hirsh
            real(F64), intent(in)                   :: beta
            
            real(F64), dimension(3, 3) :: tpq
            integer :: i, j, a, b, a0, b0
            integer :: natoms

            natoms = size(coords, dim=2)

            do j = 1, natoms
                  do i = j, natoms
                        if (i == j) then
                              tpq = ZERO
                              do a = 1, 3
                                    tpq(a, a) = ONE / alpha_hirsh(j)
                              end do
                        else
                              call tpq_ardt2014_shortrange(tpq, i, j, coords, alpha_hirsh, &
                                    rvdw_hirsh, beta)
                        end if
                        
                        a0 = 3 * (i - 1)
                        b0 = 3 * (j - 1)
                        do b = 1, 3
                              do a = 1, 3
                                    w(a0 + a, b0 + b) = tpq(a, b)
                                    w(b0 + b, a0 + a) = tpq(a, b)
                              end do
                        end do
                  end do
            end do
      end subroutine scs_matrix_ardt2014_build


      subroutine scs_matrix_invert(a)
            real(F64), dimension(:, :), contiguous, intent(inout) :: a

            integer :: n

            n = size(a, dim=1)
            call geinv(a, n)
      end subroutine scs_matrix_invert


      pure subroutine scs_polarizability(alpha_scs, scs_matrix)
            real(F64), dimension(:), intent(out)   :: alpha_scs
            real(F64), dimension(:, :), intent(in) :: scs_matrix
            
            integer :: natoms
            integer :: i, j, a, b, a0, b0
            real(F64), dimension(3, 3) :: alpha_tensor
            
            natoms = size(alpha_scs)

            do j = 1, natoms
                  alpha_tensor = ZERO
                  do i = 1, natoms
                        a0 = 3 * (i - 1)
                        b0 = 3 * (j - 1)
                        do b = 1, 3
                              do a = 1, 3
                                    alpha_tensor(a, b) = alpha_tensor(a, b) + scs_matrix(a0+a, b0+b)
                              end do
                        end do
                  end do

                  alpha_scs(j) = FRAC13 * (alpha_tensor(1, 1) + alpha_tensor(2, 2) + alpha_tensor(3, 3))
            end do
      end subroutine scs_polarizability
end module mbd
