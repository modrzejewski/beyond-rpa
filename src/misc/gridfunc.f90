! ------------------------------------------------------------
!            FUNCTIONS DEFINED ON A MOLECULAR GRID
! ------------------------------------------------------------
! This module contains functions defined on a molecular grid,
! but not present in the DFT subprogram.
!
module gridfunc
      use math_constants
      use gparam
      use arithmetic
      use gto

      implicit none

contains
      
      pure subroutine movalue(phi_k, c_k, x, y, z)
            ! -------------------------------------------------------------------
            ! Compute values of molecular orbitals at a single point of space,
            ! R=(X, Y, Z). Screening of small atomic orbitals is controlled by 
            ! the global parameter GRID_AOTHRESH. The number of molecular orbitals
            ! for which the values will be computed corresponds to the number of
            ! columns of C_K. Pass a single-column rank-2 array if only a single
            ! orbital should be computed.
            ! 
            ! Example
            ! -------
            ! Let C be the matrix of MO coefficients in AO basis obtained from
            ! the SCF subprogram. NOCC denotes the number of occupied orbitals.
            ! The following code demonstrates how to obtain values of all occupied
            ! orbitals at point R=(X, Y, Z).
            ! 
            ! allocate(phi_k(nocc))
            ! call movalue(phi_k, c(:, 1:nocc), x, y, z)
            ! 
            ! Now PHI_K(L) contains the value of L-th occupied orbital.
            ! Alternatively, we pass only the L-th column of C if we are 
            ! interested only in the value of L-th orbital:
            !
            ! allocate(phi_k(1))
            ! call movalue(phi_k, c(:, l:l), x, y, z)
            ! -------------------------------------------------------------------
            ! PHI_K
            !     Output, values of the selected molecular orbitals at
            !     the point R. On exit, PHI_K(W) is the value of the orbital 
            !     whose atomic orbital (AO) expansion was passed in C_K(:, W).
            !
            ! C_K
            !     AO expansions of the selected molecular orbitals. For each 
            !     column of C_K, a value of the corresponding molecular orbital
            !     at point R will be computed. 
            !
            ! X, Y, Z
            !     Spatial coordinates at which the orbital value should be 
            !     computed.
            !
            real(F64), dimension(:), intent(out)   :: phi_k
            real(F64), dimension(:, :), intent(in) :: c_k
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            
            real(F64), dimension(0:max(MAX_L, 2)) :: xl, ym, zn
            real(F64), dimension(MAX_NPRM) :: exptable
            real(F64) :: alpha
            real(F64) :: rsq
            real(F64) :: ao_val, ao_val_spher
            integer :: offset
            integer :: i, k, l, m, n, s
            integer :: u, v, w
            integer :: momentum, np, nints
            integer :: nk
            
            nk = size(c_k, dim=2)
            phi_k = ZERO
            xl(0)  = ONE
            ym(0)  = ONE
            zn(0)  = ONE
            !
            ! Loop over all atoms (both real and dummy centers)
            !
            atompass: do i = 1, NATOM
                  !
                  ! Generate Cartesian polynomials
                  !
                  xl(1) = x - ATOMR(1, i)
                  ym(1) = y - ATOMR(2, i)
                  zn(1) = z - ATOMR(3, i)

                  xl(2) = xl(1) * xl(1)
                  ym(2) = ym(1) * ym(1)
                  zn(2) = zn(1) * zn(1)

                  do l = 3, ATOML(i)
                        xl(l) = xl(l-1) * xl(1)
                        ym(l) = ym(l-1) * ym(1)
                        zn(l) = zn(l-1) * zn(1)
                  end do

                  rsq = xl(2) + ym(2) + zn(2)
                  !
                  ! Loop over orbital shells centered at the I-th atom
                  !
                  shloop: do k = SH0(i), SH0(i + 1) - 1
                        !
                        ! The class of the K-th shell.
                        !
                        s = SH(k)
                        !
                        ! Check if the current shell gives any non-negligible
                        ! contribution. Note that the elements of SH are sorted in
                        ! decreasing order according to R2MAX for each atom, 
                        ! thus facilitating a safe loop break at this point.
                        !
                        if (rsq > R2MAX(s)) then
                              exit shloop
                        end if
                        !
                        ! Cartesian angular momentum of the K-th shell
                        !
                        momentum = SHTYPE(s)
                        !
                        ! Number of primitive Gaussian functions
                        !
                        np = NPRM(s)
                        !
                        ! Number of Cartesian angular functions (Cartesian
                        ! polynomials) in the K-th shell
                        !
                        nints = nfunc(momentum)

                        do v = 1, np
                              alpha = EXPN(v, s)
                              exptable(v) = exp(-alpha * rsq)
                        end do

                        ao_val_spher = ZERO
                        do u = 1, np
                              ao_val_spher = ao_val_spher + CNTR(u, s) * exptable(u)
                        end do
                        !
                        ! Sum over Cartesian angular functions
                        !
                        offset = SHPOS(k) - 1
                        do v = 1, nints
                              l = ll(v, momentum)
                              m = mm(v, momentum)
                              n = nn(v, momentum)
                              !
                              ! Multiply spherical part of the AO by the Cartesian polynomial
                              !
                              ao_val = ao_val_spher * xl(l) * ym(m) * zn(n) * CNTRNORM(v, s)
                              do w = 1, nk
                                    phi_k(w) = phi_k(w) + c_k(offset+v, w) * ao_val
                              end do
                        end do
                  end do shloop
            end do atompass
      end subroutine movalue


      pure subroutine aovalue(chi_k, i, x, y, z)
            ! -------------------------------------------------------------------
            ! Compute values of atomic orbitals at a single point of space,
            ! R=(X, Y, Z). On exit, CHI_K contains the values of all AOs centered
            ! at the I-th atom.
            ! -------------------------------------------------------------------
            ! PHI_K
            !     Output, values of the atomic orbitals centered on the I-th
            !     atom.
            ! I
            !     Index of a real or dummy atom on which the AOs are centered.
            !
            ! X, Y, Z
            !     Spatial coordinates at which the orbital value should be 
            !     computed.
            !
            real(F64), dimension(:), intent(out)   :: chi_k
            integer, intent(in)                    :: i
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z

            real(F64), dimension(0:max(MAX_L, 2)) :: xl, ym, zn
            real(F64), dimension(MAX_NPRM) :: exptable
            real(F64) :: alpha
            real(F64) :: rsq
            real(F64) :: ao_val, ao_val_spher
            integer :: k, l, m, n, s
            integer :: u, v, w
            integer :: momentum, np, nints

            w = 1
            chi_k = ZERO
            xl(0)  = ONE
            ym(0)  = ONE
            zn(0)  = ONE
            !
            ! Generate Cartesian polynomials
            !
            xl(1) = x - ATOMR(1, i)
            ym(1) = y - ATOMR(2, i)
            zn(1) = z - ATOMR(3, i)

            xl(2) = xl(1) * xl(1)
            ym(2) = ym(1) * ym(1)
            zn(2) = zn(1) * zn(1)

            do l = 3, ATOML(i)
                  xl(l) = xl(l-1) * xl(1)
                  ym(l) = ym(l-1) * ym(1)
                  zn(l) = zn(l-1) * zn(1)
            end do

            rsq = xl(2) + ym(2) + zn(2)
            !
            ! Loop over orbital shells centered at the I-th atom
            !
            shloop: do k = SH0(i), SH0(i + 1) - 1
                  !
                  ! The class of the K-th shell.
                  !
                  s = SH(k)
                  !
                  ! Cartesian angular momentum of the K-th shell
                  !
                  momentum = SHTYPE(s)
                  !
                  ! Number of primitive Gaussian functions
                  !
                  np = NPRM(s)
                  !
                  ! Number of Cartesian angular functions (Cartesian
                  ! polynomials) in the K-th shell
                  !
                  nints = nfunc(momentum)

                  do v = 1, np
                        alpha = EXPN(v, s)
                        exptable(v) = exp(-alpha * rsq)
                  end do

                  ao_val_spher = ZERO
                  do u = 1, np
                        ao_val_spher = ao_val_spher + CNTR(u, s) * exptable(u)
                  end do
                  !
                  ! Loop over Cartesian angular functions
                  !
                  do v = 1, nints
                        l = ll(v, momentum)
                        m = mm(v, momentum)
                        n = nn(v, momentum)
                        !
                        ! Multiply spherical part of the AO by the Cartesian polynomial
                        !
                        ao_val = ao_val_spher * xl(l) * ym(m) * zn(n) * CNTRNORM(v, s)
                        chi_k(w) = ao_val
                        w = w + 1
                  end do
            end do shloop
      end subroutine aovalue
end module gridfunc

