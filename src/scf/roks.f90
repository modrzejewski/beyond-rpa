module roks
      use arithmetic
      use math_constants
      use gparam
      use linalg

      implicit none

contains

      subroutine roks_orbgradient(deriv, ksmatrixa, ksmatrixb, rho_oao, work, nva, nvb)
            ! ----------------------------------------------------------------------
            ! Generate the matrix of first derivatives of the restricted open-shell
            ! Kohn-Sham energy with respect to the elements of the total density
            ! matrix elements. Single open shell is assumed. Each spin-orbital
            ! belonging to the open shell is occupied by identical fraction of
            ! alpha (beta) electrons. 
            ! ----------------------------------------------------------------------
            ! DERIV     - Output, matrix of first derivatives: 
            !             DERIV_{pq} = d E / d P_{pq}. Only lower triangle is
            !             computed. Output is tranformed to OAO basis.
            !             Both triangles are computed.
            ! KSMATRIXA - Input, matrix of first derivatives with respect to
            ! KSMATRIXB   spin density matrix elements:
            !             KSMATRIXA_{pq} = d E / d D^\alpha_{pq}
            !             KSMATRIXB_{pq} = d E / d D^\beta_{pq}
            !             (OAO basis, lower triangle referenced)
            ! RHO_OAO   - Total density matrix, RHO = D^\alpha + D^\beta.
            !             Both upper and lower triangle are referenced
            !             (OAO basis, both triangles referenced)
            ! WORK      - Scratch array 
            ! NVA,NVB   - Fractional occupation of alpha (beta) spinoribtal
            !             belonging to the open shell. In this ROKS realization,
            !             each spinorbital belonging to the open shell has identical
            !             alpha (beta) occupation number. Due to a numerical
            !             instability, NVA+NVB should not be put equal to 2,
            !             even though it is a physically sound configuration.
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: deriv
            real(F64), dimension(:, :), contiguous, intent(in)  :: ksmatrixa
            real(F64), dimension(:, :), contiguous, intent(in)  :: ksmatrixb
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho_oao
            real(F64), dimension(:, :), contiguous, intent(out) :: work
            real(F64), intent(in)                               :: nva
            real(F64), intent(in)                               :: nvb

            real(F64) :: aa, ab
            real(F64) :: ba, bb
            real(F64) :: nv

            nv = nva + nvb
            aa = ONE / (FOUR - TWO * nv) + nva / (nv**2 - TWO * nv)
            ab = ONE / (FOUR - TWO * nv) + nvb / (nv**2 - TWO * nv)
            ba = -nv / (FOUR - TWO * nv) - TWO * nva / (nv**2 - TWO * nv)
            bb = -nv / (FOUR - TWO * nv) - TWO * nvb / (nv**2 - TWO * nv)
            !
            ! DERIV <- BA * KSMATRIXA + BB * KSMATRIXB
            !
            deriv = ksmatrixa
            call weightedsum(deriv, ksmatrixb, ba, bb)
            !
            ! WORK <- AA * KSMATRIXA + AB * KSMATRIXB
            !
            work = ksmatrixa
            call weightedsum(work, ksmatrixb, aa, ab)
            !
            ! DERIV <- DERIV + WORK * RHO_OAO + RHO_OAO * WORK
            !
            call anticomm(deriv, work, rho_oao, ONE)
            call smfill(deriv)
      end subroutine roks_orbgradient


      subroutine roks_density(rho_ao, rho_oao, rhoa_ao, rhob_ao, c, &
            invsq, ncore, nvalence, occnuma, occnumb)
            ! ---------------------------------------------------------------
            ! Calculate total density matrix (alpha + beta) as well as the
            ! separate spin contributions in restricted open-shell Kohn-Sham
            ! framework. It is assumed that alpha is the majority spin.
            ! Optionally, this subroutine can compute an individual
            ! contrubtion of a given occupied orbital to the AO density
            ! matrix.
            !
            ! Admissible occupation numbers
            ! ------------------------------
            !
            ! 0 < A, B <= 1, A+B < 2
            !
            ! #   OCCNUMA        OCCNUMB    PHYSICAL EXAMPLE
            ! =   ========       =======    ================
            ! 1   [1, a]         [1, 0]     Quintet ground state of
            !                               the nitrogen atom, a = 1
            ! 2   [1, a]         [1, b]     ClF^{+0.1} diatomic, with 0.1
            !                               beta electron removed (fractional
            !                               occupation of the beta HOMO
            !                               orbital)
            ! 
            ! 3   [a, 0]         [0, 0]     Hydrogen atom, a = 1
            !     (NVALENCE = 0)
            ! 4   [a, 0]         [b, 0]     Spin-unpolarized hydrogen atom,
            !     (NVALENCE = 0)            with 1/2 of alpha electron and 1/2
            !                               of beta electron.
            ! 
            ! Note that [a, b] where a < 1 is not admissible configuration,
            ! because the non-interacting Kohn-Sham determinant must be
            ! a ground-state wavefunction.
            ! ---------------------------------------------------------------
            ! RHO_AO   - Output, density matrix in AO basis (alpha +
            !            beta component)
            ! RHO_OAO  - Output, density matrix in OAO basis (alpha +
            !            beta component)
            ! RHOA_AO  - Output, alpha component of density matrix (AO)
            ! RHOB_AO  - Output, beta component of density matrix (AO)
            ! C        - Input, coefficients of MOs (OAO basis)
            ! INVSQ    - Input, S^{-1/2} matrix, upper triangle not
            !            referenced
            ! NCORE    - Number of core orbitals (occupation numbers
            !            OCCNUMA(1) and OCCNUMB(1))
            ! NBETA    - Number of valence orbitals (occupation numbers
            !            OCCNUMA(2) and OCCNUMB(2))
            ! OCCNUMA, - Occupation numbers of alpha (beta) 
            ! OCCNUMB    spin-orbitals
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_ao
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_oao
            real(F64), dimension(:, :), contiguous, intent(out) :: rhoa_ao
            real(F64), dimension(:, :), contiguous, intent(out) :: rhob_ao
            real(F64), dimension(:, :), contiguous, intent(in)  :: c
            real(F64), dimension(:, :), contiguous, intent(in)  :: invsq
            integer, intent(in)                                 :: ncore
            integer, intent(in)                                 :: nvalence
            real(F64), dimension(2), intent(in)                 :: occnuma
            real(F64), dimension(2), intent(in)                 :: occnumb

            real(F64) :: nv
            real(F64) :: tba
            real(F64), parameter :: eps = 1.d-12
            !
            ! Compute molecular coefficients in AO basis:
            ! RHO_OAO <- INVSQ * C.
            ! This coefficients will be used to compute
            ! density matrices in AO basis.
            !
            call symmwrap("L", "L", NORB, NORB, ONE, invsq, c, ZERO, rho_oao)

            if (occnuma(2) > eps) then
                  !
                  ! Case 1. OCCNUMA(1) == OCCNUMB(1) == 1.d+0
                  !         OCCNUMA(2) > 0
                  !         OCCNUMB(2) >= 0
                  ! ---
                  ! Compute core electrons contribution to the density matrix.
                  ! It is identical for alpha and beta electrons.
                  ! 
                  call syrkwrap("L", "N", NORB, ncore, ONE, rho_oao, ZERO, rhoa_ao)
                  rhob_ao = rhoa_ao
                  !
                  ! Compute valence electrons contribution to the density matrix.
                  ! Valence shell (open shell) can be occupied by alpha and beta
                  ! electrons simultaneously. Alpha spin is the majority spin.
                  !
                  if (occnumb(2) > eps) then
                        call syrkwrap("L", "N", NORB, nvalence, occnuma(2), rho_oao(:, ncore+1:), ZERO, rho_ao)
                        rhoa_ao = rhoa_ao + rho_ao
                        tba = occnumb(2) / occnuma(2)
                        rhob_ao = rhob_ao + tba * rho_ao
                  else
                        call syrkwrap("L", "N", NORB, nvalence, occnuma(2), rho_oao(:, ncore+1:), ONE, rhoa_ao)
                  end if
                  call smfill(rhoa_ao)
                  call smfill(rhob_ao)
            else
                  !
                  ! Case 2. OCCNUMA(1) > 0
                  !         OCCNUMB(1) >= 0
                  !         OCCNUMA(2) == OCCNUMB(2) == 0
                  !
                  call syrkwrap("L", "N", NORB, ncore, occnuma(1), rho_oao, ZERO, rhoa_ao)
                  if (occnumb(1) > eps) then
                        call smfill(rhoa_ao)
                        tba = occnumb(1) / occnuma(1)
                        rhob_ao = tba * rhoa_ao
                  else
                        rhob_ao = ZERO
                        call smfill(rhoa_ao)
                  end if
            end if
            !
            ! Total density matrix (AO basis)
            !
            rho_ao = rhoa_ao + rhob_ao
            !
            ! Total density matrix (OAO basis)
            !
            if (occnuma(2) > eps) then
                  !
                  ! Case 1. OCCNUMA(1) == OCCNUMB(1) == 1.d+0
                  !         OCCNUMA(2) > 0
                  !         OCCNUMB(2) >= 0
                  !
                  call syrkwrap("L", "N", NORB, ncore, TWO, c, ZERO, rho_oao)
                  nv = occnuma(2) + occnumb(2)
                  call syrkwrap("L", "N", NORB, nvalence, nv, c(:, ncore+1:), ONE, rho_oao)
            else
                  nv = occnuma(1) + occnumb(1)
                  call syrkwrap("L", "N", NORB, ncore, nv, c, ZERO, rho_oao)
            end if
            call smfill(rho_oao)
      end subroutine roks_density

      
      subroutine roks_elconf(nocc, ncore, nvalence, nalpha, nbeta, &
            occnuma, occnumb, nva, nvb, ne, nopenela, nopenelb, nopenorb)
            
            integer, intent(out)                 :: nocc
            integer, intent(out)                 :: ncore
            integer, intent(out)                 :: nvalence
            integer, intent(out)                 :: nalpha
            integer, intent(out)                 :: nbeta
            real(F64), dimension(2), intent(out) :: occnuma
            real(F64), dimension(2), intent(out) :: occnumb
            real(F64), intent(out)               :: nva
            real(F64), intent(out)               :: nvb
            integer, intent(in)                  :: ne
            integer, intent(in)                  :: nopenela
            integer, intent(in)                  :: nopenelb
            integer, intent(in)                  :: nopenorb

            integer :: nopenel, ncoreel

            if (nopenorb > 0) then
                  nopenel = nopenela + nopenelb
                  ncoreel = ne - nopenel
                  if (ncoreel > 0) then
                        !
                        ! Case 1. Doubly-occupied core orbitals are present.
                        !
                        ncore = ncoreel / (2 * ROKS_NEUNIT)
                        nvalence = nopenorb
                        occnuma(1) = ONE
                        occnumb(1) = ONE
                        occnuma(2) = dble(nopenela) / dble(ROKS_NEUNIT * nopenorb)
                        occnumb(2) = dble(nopenelb) / dble(ROKS_NEUNIT * nopenorb)
                        nva = occnuma(2)
                        nvb = occnumb(2)
                        nocc = ncore + nvalence

                        if (occnuma(2) > ONE .or. occnumb(2) > ONE) then
                              call msg("ROKS ERROR: OCCUPATION NUMBER CANNOT BE GREATER THAN ONE", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                        if (occnuma(2) < ZERO .or. occnumb(2) < ZERO) then
                              call msg("ROKS ERROR: OCCUPATION NUMBER CANNOT BE LOWER THAN ZERO", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                        if (nopenela < nopenelb) then
                              call msg("ROKS ERROR: ALPHA MUST BE THE MAJORITY SPIN", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                        if (modulo(ncoreel/ROKS_NEUNIT, 2) > 0) then
                              call msg("ROKS ERROR: WRONG NUMBER OF ELECTRONS SPECIFIED", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                  else
                        !
                        ! Case 2. Doubly-occupied are not present (as in hydrogen atom or triplet helium)
                        !
                        ncore = nopenorb
                        nvalence = 0
                        occnuma(1) = dble(nopenela) / dble(ROKS_NEUNIT * nopenorb)
                        occnumb(1) = dble(nopenelb) / dble(ROKS_NEUNIT * nopenorb)
                        nva = occnuma(1)
                        nvb = occnumb(1)
                        occnuma(2) = ZERO
                        occnumb(2) = ZERO
                        nocc = ncore + nvalence

                        if (occnuma(1) > ONE .or. occnumb(1) > ONE) then
                              call msg("ROKS ERROR: OCCUPATION NUMBER CANNOT BE GREATER THAN ONE", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                        if (occnuma(1) < ZERO .or. occnumb(1) < ZERO) then
                              call msg("ROKS ERROR: OCCUPATION NUMBER CANNOT BE LOWER THAN ZERO", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                        if (nopenela < nopenelb) then
                              call msg("ROKS ERROR: ALPHA MUST BE THE MAJORITY SPIN", &
                                    priority=MSG_ERROR)
                              stop
                        end if
                  end if
            else
                  ncore = ne / (ROKS_NEUNIT * 2)
                  nvalence = 0
                  nocc = ncore + nvalence
                  occnuma(1) = ONE
                  occnumb(1) = ONE
                  occnuma(2) = ZERO
                  occnumb(2) = ZERO
                  nva = ZERO
                  nvb = ZERO

                  if (modulo(ne, ROKS_NEUNIT) > 0) then
                        call msg("ROKS_ERROR: WRONG NUMBER OF OPEN SHELL ORBITALS")
                        stop
                  end if
                  if (modulo(ne/ROKS_NEUNIT, 2) > 0) then
                        call msg("ROKS ERROR: ODD NUMBER OF ELECTRONS", &
                              priority=MSG_ERROR)
                        stop
                  end if
            end if

            if (nopenela > 0) then
                  nalpha = ncore + nopenorb
            else
                  nalpha = ncore
            end if

            if (nopenelb > 0) then
                  nbeta = ncore + nopenorb
            else
                  nbeta = ncore
            end if
      end subroutine roks_elconf


      function roks_nealpha()
            !
            ! Number of alpha electrons in restricted open-shell
            ! Kohn-Sham calculations. The unit is 1/ROKS_NEUNIT
            ! of an electron. The number of alpha electrons can
            ! be non-integer.
            !
            integer :: roks_nealpha
            integer :: necore

            necore = ROKS_NE - ROKS_NOPENELA - ROKS_NOPENELB
            roks_nealpha = necore / 2 + ROKS_NOPENELA
      end function roks_nealpha


      function roks_nebeta()
            !
            ! Number of alpha electrons in restricted open-shell
            ! Kohn-Sham calculations. The unit is 1/ROKS_NEUNIT
            ! of an electron. The number of beta electrons can be
            ! non-integer.
            !
            integer :: roks_nebeta
            integer :: necore
            
            necore = ROKS_NE - ROKS_NOPENELA - ROKS_NOPENELB
            roks_nebeta = necore / 2 + ROKS_NOPENELB
      end function roks_nebeta

      
      function roks_isnoninteger()
            !
            ! Return .TRUE. if the number of alpha-spin and/or
            ! beta-spin electrons is non-integer.
            !
            logical :: roks_isnoninteger

            if (modulo(roks_nealpha(), ROKS_NEUNIT) > 0 .or. &
                  modulo(roks_nebeta(), ROKS_NEUNIT) > 0) then
                  roks_isnoninteger = .true.
            else
                  roks_isnoninteger = .false.
            end if
      end function roks_isnoninteger
end module roks
