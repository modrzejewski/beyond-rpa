! -------------------------------------------------------------------
!          A TEXTFILE REPRESENTATION OF VOLUMETRIC DATA
! -------------------------------------------------------------------
! Subprograms for writing cube files. A cube file is a text file
! containing representation of a three-dimensional function defined
! on a molecular grid. The cube file format can be read by several
! tools for molecular visualization. Use this format to represent
! electronic density, laplacian, molecular orbitals etc.
!
! When implementing a new function, remember to update 
! VOL_NEEDS_MOCOEFF if the function requires molecular orbitals.
!
module voldata
      use gparam
      use io
      use math_constants
      use arithmetic
      use lda
      use mgga
      use gridfunc
      use periodic
      use string
      use h_voldata
      use sort
      use auxint
      use linalg
      use multipole

      implicit none

      type tvoldata
            !
            ! Types of functions represented on the grid
            !
            integer :: func = VOL_FUNC_NONE
            logical :: openshell = .false.
            logical :: restricted = .true.
            !
            ! Spacing between grid points
            !
            integer :: spacing = VOL_SPACING_MEDIUM
            !
            ! Indices of molecular orbitals for which values
            ! on the grid are to be computed. Indices larger
            ! than NORB will be ignored.
            !
            integer, dimension(:), allocatable :: mo_idx
            !
            ! Indices of atoms for which atomic orbitals
            ! are to be computed. For example, if AO_CENTRES
            ! contains atom I, then all AOs centered on I
            ! will be stored in a cube file. Indices larger
            ! than NATOM will be ignored.
            !
            integer, dimension(:), allocatable :: ao_centers
            !
            ! Data format for the main numerical part of a cube file
            ! (ignored if blank)
            !
            character(DEFLEN) :: data_fmt
      contains
            procedure, pass :: init => vol_init
      end type tvoldata
      
contains

      subroutine vol_init(this, func, openshell, restricted, &
            spacing, mo_idx, ao_centers, data_fmt)
            
            class(tvoldata), intent(out)       :: this
            integer, intent(in)                :: func
            logical, intent(in)                :: openshell
            logical, intent(in)                :: restricted
            integer, intent(in)                :: spacing
            integer, dimension(:), intent(in)  :: mo_idx
            integer, dimension(:), intent(in)  :: ao_centers
            character(*), optional, intent(in) :: data_fmt

            integer :: nmo, nao

            this%func = func
            this%openshell = openshell
            this%restricted = restricted
            this%spacing = spacing
            
            if (iand(this%func, VOL_FUNC_MO) > 0) then
                  nmo = size(mo_idx)
                  allocate(this%mo_idx(nmo))
                  this%mo_idx = mo_idx
            end if

            if (iand(this%func, VOL_FUNC_AO) > 0) then
                  nao = size(ao_centers)
                  allocate(this%ao_centers(nao))
                  this%ao_centers = ao_centers
            end if

            if (present(data_fmt)) then
                  this%data_fmt = data_fmt
            end if
      end subroutine vol_init


      function vol_needs_mocoeff(vd)
            logical :: vol_needs_mocoeff
            type(tvoldata), intent(in) :: vd

            if (iand(vd%func, VOL_FUNC_MO) > 0) then
                  vol_needs_mocoeff = .true.
            else
                  vol_needs_mocoeff = .false.
            end if
      end function vol_needs_mocoeff


      subroutine vol_write(znum, coord, rhoa_ao_tile, rhob_ao_tile, &
            ca_ao, cb_ao, vd, root_file_path)
            
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            real(F64), dimension(:), intent(in)    :: rhoa_ao_tile
            real(F64), dimension(:), intent(in)    :: rhob_ao_tile
            real(F64), dimension(:, :), intent(in) :: ca_ao
            real(F64), dimension(:, :), intent(in) :: cb_ao
            type(tvoldata), intent(in)             :: vd
            character(*), optional                 :: root_file_path

            character(:), allocatable :: root_fname, fname, fmt
            integer, dimension(:), allocatable :: mo_idx, ao_centers
            integer :: nao, nmo

            nao = 0
            nmo = 0

            if (allocated(vd%mo_idx)) then
                  call validateidx(mo_idx, nmo, vd%mo_idx, 1, NORB)
            end if

            if (allocated(vd%ao_centers)) then
                  call validateidx(ao_centers, nao, vd%ao_centers, 1, NATOM)
            end if

            if (present(root_file_path)) then
                  root_fname = root_file_path
            else
                  root_fname = WORKDIR // JOBTITLE
            end if

            if (.not. isblank(vd%data_fmt)) then
                  fmt = trim(adjustl(vd%data_fmt))
            else
                  fmt = GRIDDATA_DEFAULT_FMT
            end if

            if (iand(vd%func, VOL_FUNC_MO) > 0 .and. nmo > 0) then
                  if (vd%openshell) then
                        if (vd%restricted) then
                              fname = root_fname // "_mo"
                              call write_mo(fname, mo_idx, &
                                    ca_ao, znum, coord, vd%spacing, fmt)
                        else
                              !
                              ! Different MOs for each spin
                              !
                              fname = root_fname // "_mo_alpha"
                              call write_mo(fname, mo_idx, &
                                    ca_ao, znum, coord, vd%spacing, fmt)
                              fname = root_fname // "_mo_beta"
                              call write_mo(fname, mo_idx, &
                                    cb_ao, znum, coord, vd%spacing, fmt)
                        end if
                  else
                        fname = root_fname // "_mo"
                        call write_mo(fname, mo_idx, &
                              ca_ao, znum, coord, vd%spacing, fmt)
                  end if
            end if

            if (iand(vd%func, VOL_FUNC_AO) > 0 .and. nao > 0) then
                  fname = root_fname // "_ao"
                  call write_ao(fname, ao_centers, znum, coord, vd%spacing, fmt)
            end if

            if (iand(vd%func, VOL_FUNC_RHO) > 0) then
                  if (vd%openshell) then
                        fname = root_fname // "_rho_alpha.cube"
                        call write_rho(fname, rhoa_ao_tile, znum, coord, vd%spacing, fmt)
                        fname = root_fname // "_rho_beta.cube"
                        call write_rho(fname, rhob_ao_tile, znum, coord, vd%spacing, fmt)
                  else
                        fname = root_fname // "_rho.cube"
                        call write_rho(fname, rhoa_ao_tile, znum, coord, vd%spacing, fmt)
                  end if
            end if

            if (iand(vd%func, VOL_FUNC_LAPL) > 0) then
                  if (vd%openshell) then
                        fname = root_fname // "_lapl_alpha.cube"
                        call write_lapl(fname, rhoa_ao_tile, znum, coord, vd%spacing, fmt)
                        fname = root_fname // "_lapl_beta.cube"
                        call write_lapl(fname, rhob_ao_tile, znum, coord, vd%spacing, fmt)
                  else
                        fname = root_fname // "_lapl.cube"
                        call write_lapl(fname, rhoa_ao_tile, znum, coord, vd%spacing, fmt)
                  end if
            end if

            if (iand(vd%func, VOL_FUNC_TAU_UEG_TAU) > 0) then
                  if (vd%openshell) then
                        fname = root_fname // "_tau_ueg_tau_alpha.cube"
                        call write_tau_ueg_tau(fname, rhoa_ao_tile, znum, &
                              coord, vd%spacing, .true., fmt)
                        fname = root_fname // "_tau_ueg_tau_beta.cube"
                        call write_tau_ueg_tau(fname, rhob_ao_tile, znum, &
                              coord, vd%spacing, .true., fmt)
                  else
                        fname = root_fname // "_tau_ueg_tau.cube"
                        call write_tau_ueg_tau(fname, rhoa_ao_tile, znum, &
                              coord, vd%spacing, .false., fmt)
                  end if
            end if

            if (iand(vd%func, VOL_FUNC_DX) > 0) then
                  if (vd%openshell) then
                        fname = root_fname // "_dx_alpha.cube"
                        call write_dx(fname, rhoa_ao_tile, znum, &
                              coord, vd%spacing, .true., fmt)
                        fname = root_fname // "_dx_beta.cube"
                        call write_dx(fname, rhob_ao_tile, znum, &
                              coord, vd%spacing, .true., fmt)
                  else
                        fname = root_fname // "_dx.cube"
                        call write_dx(fname, rhoa_ao_tile, znum, &
                              coord, vd%spacing, .false., fmt)
                  end if
            end if

            contains

                  subroutine validateidx(aout, n, ain, imin, imax)
                        integer, dimension(:), allocatable, intent(out) :: aout
                        integer, intent(out) :: n
                        integer, dimension(:), intent(in) :: ain
                        integer, intent(in) :: imin
                        integer, intent(in) :: imax
                        
                        integer :: k, l
                        integer, dimension(1) :: t
                        
                        n = 0
                        do k = 1, size(ain)
                              if (ain(k) >= imin .and. ain(k) <= imax) n = n + 1
                        end do

                        allocate(aout(n))

                        l = 1
                        do k = 1, size(ain)
                              if (ain(k) >= imin .and. ain(k) <= imax) then
                                    aout(l) = ain(k)
                                    l = l + 1
                              end if
                        end do
                        
                        if (n > 1) then
                              !
                              ! Sort indices in increasing order
                              !
                              call isort(aout, t, n, 1)
                        end if
                  end subroutine validateidx
      end subroutine vol_write
      

      subroutine write_mo(root_fname, mo_idx, mocoeff, znum, coord, npt_bohr, griddata_fmt)
            character(*), intent(in)               :: root_fname
            integer, dimension(:), intent(in)      :: mo_idx
            real(F64), dimension(:, :), intent(in) :: mocoeff
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            integer, intent(in)                    :: npt_bohr
            character(*), intent(in)               :: griddata_fmt

            real(F64), dimension(3) :: xaxis, yaxis, zaxis, origin
            real(F64) :: unitlen
            real(F64) :: a, b, c
            integer :: xnpt, ynpt, znpt
            integer :: nmo
            integer, dimension(:), allocatable :: u
            integer :: ix, iy, iz
            real(F64) :: x, y, z
            character(:), allocatable :: fname
            real(F64), dimension(:), allocatable :: zrow
            integer :: k

            unitlen = ONE / real(npt_bohr, F64)
            xaxis = [unitlen, ZERO, ZERO]
            yaxis = [ZERO, unitlen, ZERO]
            zaxis = [ZERO, ZERO, unitlen]
            call box(origin, a, b, c, znum, coord)
            xnpt = ceiling(a * real(npt_bohr, F64))
            ynpt = ceiling(b * real(npt_bohr, F64))
            znpt = ceiling(c * real(npt_bohr, F64))
            !
            ! Number of molecular orbitals
            !
            nmo = size(mo_idx)
            allocate(zrow(nmo*znpt))
            
            if (SEPARATE_ORB_FILES) then
                  !
                  ! Open separate file for each orbital
                  !
                  allocate(u(nmo))
                  do k = 1, nmo
                        fname = trim(root_fname) // "_" // str(k) // ".cube"
                        u(k) = io_text_open(fname, "REPLACE")
                        call header(u(k), znum, coord, origin, xnpt, xaxis, &
                              ynpt, yaxis, znpt, zaxis, "Molecular orbital #" // str(k) &
                              // " | Grid spacing " &
                              // str(npt_bohr) // " points/Bohr", &
                              "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z")
                  end do
            else
                  allocate(u(1))
                  fname = trim(root_fname) // ".cube"
                  u(1) = io_text_open(fname, "REPLACE")
                  call header(u(1), znum, coord, origin, xnpt, xaxis, &
                        ynpt, yaxis, znpt, zaxis, "Molecular orbitals | Grid spacing " &
                        // str(npt_bohr) // " points/Bohr", &
                        "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z", &
                        mo_idx)
            end if

            do ix = 0, xnpt-1
                  x = origin(1) + real(ix, F64) * unitlen
                  do iy = 0, ynpt-1
                        y = origin(2) + real(iy, F64) * unitlen
                        do iz = 0, znpt-1
                              z = origin(3) + real(iz, F64) * unitlen
                              call movalue(zrow(iz*nmo+1:), mocoeff(:, mo_idx), x, y, z)
                        end do
                        !
                        ! Each row of the text file contains at most 6 real numbers.
                        ! The write statement takes care of the newline characters.
                        !
                        if (SEPARATE_ORB_FILES) then
                              do k = 1, nmo
                                    write(u(k), griddata_fmt) zrow(k:znpt*nmo:nmo)
                              end do
                        else
                              write(u(1), griddata_fmt) zrow
                        end if
                  end do
            end do
            
            if (SEPARATE_ORB_FILES) then
                  do k = 1, nmo
                        close(u(k))
                  end do
            else
                  close(u(1))
            end if
      end subroutine write_mo


      subroutine write_ao(root_fname, ao_centers, znum, coord, npt_bohr, griddata_fmt)
            character(*), intent(in)               :: root_fname
            integer, dimension(:), intent(in)      :: ao_centers
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            integer, intent(in)                    :: npt_bohr
            character(*), intent(in)               :: griddata_fmt

            real(F64), dimension(3) :: xaxis, yaxis, zaxis, origin
            real(F64) :: unitlen
            real(F64) :: a, b, c
            integer :: xnpt, ynpt, znpt
            integer :: nao
            integer, dimension(:), allocatable :: u
            integer :: ix, iy, iz
            integer :: p0, p1, p, pp, k, dp, l
            real(F64) :: x, y, z
            character(:), allocatable :: fname
            real(F64), dimension(:), allocatable :: zrow
            integer, dimension(:), allocatable :: ao_idx
            integer :: ncenters

            unitlen = ONE / real(npt_bohr, F64)
            xaxis = [unitlen, ZERO, ZERO]
            yaxis = [ZERO, unitlen, ZERO]
            zaxis = [ZERO, ZERO, unitlen]
            call box(origin, a, b, c, znum, coord)
            xnpt = ceiling(a * real(npt_bohr, F64))
            ynpt = ceiling(b * real(npt_bohr, F64))
            znpt = ceiling(c * real(npt_bohr, F64))
            ncenters = size(ao_centers)
            !
            ! Number of atomic orbitals
            !
            nao = 0
            do pp = 1, ncenters
                  p = ao_centers(pp)
                  p0 = IDX(p)
                  p1 = IDX(p+1) - 1
                  nao = nao + (p1 - p0 + 1)
            end do

            allocate(ao_idx(nao))

            k = 1
            do pp = 1, ncenters
                  p = ao_centers(pp)
                  p0 = IDX(p)
                  p1 = IDX(p+1) - 1
                  dp = p1 - p0
                  ao_idx(k:k+dp) = [(l, l = p0, p1)]
                  k = k + dp + 1
            end do

            allocate(zrow(nao*znpt))

            if (SEPARATE_ORB_FILES) then
                  !
                  ! Open separate file for each orbital
                  !
                  allocate(u(nao))
                  do k = 1, nao
                        fname = trim(root_fname) // "_" // str(k) // ".cube"
                        u(k) = io_text_open(fname, "REPLACE")
                        call header(u(k), znum, coord, origin, xnpt, xaxis, &
                              ynpt, yaxis, znpt, zaxis, "Atomic orbital #" // str(k) &
                              // " | Grid spacing " // str(npt_bohr) // " points/Bohr", &
                              "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z")
                  end do
            else
                  allocate(u(1))
                  fname = trim(root_fname) // ".cube"
                  u(1) = io_text_open(fname, "REPLACE")
                  call header(u(1), znum, coord, origin, xnpt, xaxis, &
                        ynpt, yaxis, znpt, zaxis, "Atomic orbitals | Grid spacing " & 
                        // str(npt_bohr) // " points/Bohr", &
                        "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z", &
                        ao_idx)
            end if

            do ix = 0, xnpt-1
                  x = origin(1) + real(ix, F64) * unitlen
                  do iy = 0, ynpt-1
                        y = origin(2) + real(iy, F64) * unitlen
                        do iz = 0, znpt-1
                              z = origin(3) + real(iz, F64) * unitlen
                              k = 1
                              do pp = 1, ncenters
                                    p = ao_centers(pp)
                                    p0 = IDX(p)
                                    p1 = IDX(p+1) - 1
                                    dp = p1 - p0
                                    call aovalue(zrow(iz*nao+k:), p, x, y, z)
                                    k = k + dp + 1
                              end do
                        end do
                        !
                        ! Each row of the text file contains at most 6 real numbers.
                        ! The write statement takes care of the newline characters.
                        !
                        if (SEPARATE_ORB_FILES) then
                              do k = 1, nao
                                    write(u(k), griddata_fmt) zrow(k:znpt*nao:nao)
                              end do
                        else
                              write(u(1), griddata_fmt) zrow
                        end if
                  end do
            end do

            if (SEPARATE_ORB_FILES) then
                  do k = 1, nao
                        close(u(k))
                  end do
            else
                  close(u(1))
            end if
      end subroutine write_ao


      subroutine write_rho(fname, dmatrix_tile, znum, coord, npt_bohr, griddata_fmt)
            character(*), intent(in)               :: fname
            real(F64), dimension(:), intent(in)    :: dmatrix_tile
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            integer, intent(in)                    :: npt_bohr
            character(*), intent(in)               :: griddata_fmt

            real(F64), dimension(3) :: xaxis, yaxis, zaxis, origin
            real(F64) :: unitlen
            real(F64) :: a, b, c
            integer :: xnpt, ynpt, znpt
            integer :: u
            integer :: ix, iy, iz
            real(F64) :: x, y, z
            real(F64), dimension(:), allocatable :: zrow
            integer, dimension(:), allocatable :: shellidx
            real(F64), dimension(:), allocatable :: orbval
            integer :: n0

            unitlen = ONE / real(npt_bohr, F64)
            xaxis = [unitlen, ZERO, ZERO]
            yaxis = [ZERO, unitlen, ZERO]
            zaxis = [ZERO, ZERO, unitlen]
            call box(origin, a, b, c, znum, coord)
            xnpt = ceiling(a * real(npt_bohr, F64))
            ynpt = ceiling(b * real(npt_bohr, F64))
            znpt = ceiling(c * real(npt_bohr, F64))

            allocate(zrow(znpt))
            allocate(shellidx(NSHELL))
            allocate(orbval(LDA_DELTAK*NORB))

            u = io_text_open(fname, "REPLACE")

            call header(u, znum, coord, origin, xnpt, xaxis, &
                  ynpt, yaxis, znpt, zaxis, "Electronic density | Grid spacing " &
                  // str(npt_bohr) // " points/Bohr", &
                  "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z")

            do ix = 0, xnpt-1
                  x = origin(1) + real(ix, F64) * unitlen
                  do iy = 0, ynpt-1
                        y = origin(2) + real(iy, F64) * unitlen
                        do iz = 0, znpt-1
                              z = origin(3) + real(iz, F64) * unitlen
                              call lda_vars(dmatrix_tile, x, y, z, zrow(iz+1), orbval, &
                                    shellidx, n0)
                        end do
                        !
                        ! Each row of the text file contains at most 6 real numbers.
                        ! The write statement takes care of the newline characters.
                        !
                        write(u, griddata_fmt) zrow
                  end do
            end do
            close(u)
      end subroutine write_rho


      subroutine write_tau_ueg_tau(fname, dmatrix_tile, znum, coord, npt_bohr, &
            openshell, griddata_fmt)
            ! ---------------------------------------------------------------------
            ! Compute the ratio between the kinetic energy of the uniform
            ! electron gas (UEG) and the kinetic energy of a nonuniform
            ! system with the same density as the UEG at a given grid point: [1]
            !
            ! t_\sigma = \tau_\sigma^{UEG} / \tau_\sigma,
            ! \tau_\sigma^{UEG} = 3/5 (6 \pi^2)^{2/3} \rho_\sigma^{5/3},
            ! \tau_\sigma = \sum_{pq} D_{pq,\sigma} \phi_p(r) \phi_q(r),
            !
            ! where the 1/2 factor is ommited from the definition of \tau_\sigma.
            !
            ! The t_\sigma variable can be used to visualize the regions of space
            ! where localized orbitals dominate. [1]
            ! --------------------------------------------------------------------
            ! 1. Schmider, H.L., Becke, A.D., Chemical content of the kinetic
            !    energy density, J. Mol. Struct. 527, 51 (2000).
            !
            character(*), intent(in)               :: fname
            real(F64), dimension(:), intent(in)    :: dmatrix_tile
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            integer, intent(in)                    :: npt_bohr
            logical, intent(in)                    :: openshell
            character(*), intent(in)               :: griddata_fmt

            real(F64), dimension(3) :: xaxis, yaxis, zaxis, origin
            real(F64) :: unitlen
            real(F64) :: a, b, c
            integer :: xnpt, ynpt, znpt
            integer :: u
            integer :: ix, iy, iz
            real(F64) :: x, y, z
            real(F64), dimension(:), allocatable :: zrow
            integer, dimension(:), allocatable :: shellidx
            real(F64), dimension(:), allocatable :: orbval
            integer :: n0
            real(F64) :: rho, sigma, tau, lapl
            real(F64), dimension(3) :: grad
            real(F64), parameter :: c_tau_ueg = THREE / FIVE * (SIX * PI**2)**FRAC23
            real(F64) :: scal
            real(F64) :: rhoa, taua

            if (openshell) then
                  scal = ONE
            else
                  scal = FRAC12
            end if

            unitlen = ONE / real(npt_bohr, F64)
            xaxis = [unitlen, ZERO, ZERO]
            yaxis = [ZERO, unitlen, ZERO]
            zaxis = [ZERO, ZERO, unitlen]
            call box(origin, a, b, c, znum, coord)
            xnpt = ceiling(a * real(npt_bohr, F64))
            ynpt = ceiling(b * real(npt_bohr, F64))
            znpt = ceiling(c * real(npt_bohr, F64))

            allocate(zrow(znpt))
            allocate(shellidx(NSHELL))
            allocate(orbval(MGGA_DELTAK*NORB))

            u = io_text_open(fname, "REPLACE")

            call header(u, znum, coord, origin, xnpt, xaxis, &
                  ynpt, yaxis, znpt, zaxis, "tau_ueg / tau | Grid spacing " &
                  // str(npt_bohr) // " points/Bohr", &
                  "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z")

            do ix = 0, xnpt-1
                  x = origin(1) + real(ix, F64) * unitlen
                  do iy = 0, ynpt-1
                        y = origin(2) + real(iy, F64) * unitlen
                        do iz = 0, znpt-1
                              z = origin(3) + real(iz, F64) * unitlen
                              call mgga_vars_tile(dmatrix_tile, x, y, z, rho, grad, &
                                    sigma, lapl, tau, orbval, shellidx, n0)
                              rhoa = scal * rho
                              taua = scal * tau
                              zrow(iz+1) = c_tau_ueg * rhoa**FRAC53 / taua
                        end do
                        !
                        ! Each row of the text file contains at most 6 real numbers.
                        ! The write statement takes care of the newline characters.
                        !
                        write(u, griddata_fmt) zrow
                  end do
            end do
            close(u)
      end subroutine write_tau_ueg_tau


      subroutine write_dx(fname, dmatrix_tile, znum, coord, npt_bohr, &
            openshell, griddata_fmt)
            ! ------------------------------------------------------------------
            ! Compute average electron-exchange hole distance (DXNORM) at each
            ! point of the grid. [1]
            !
            ! dx = \frac{1}{\rho_\sigma}
            !      (\sum_{ij}r_{ij\sigma}\psi_{i\sigma}\psi_{j\sigma}) - r
            !
            ! dxnorm = \sqrt{dx(1)^2 + dx(2)^2 + dx(3)^2}
            ! ------------------------------------------------------------------
            ! 1. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            ! ------------------------------------------------------------------
            ! FNAME 
            !          Path to the output cube file
            !
            ! DMATRIX_TILE
            !          Input, total density matrix if OPENSHELL==.FALSE. or
            !          sigma-spin density matrix if OPENSHELL==.TRUE.
            ! ZNUM  
            !          Nuclear charges
            ! 
            ! COORD
            !          Nuclear coordinates
            !
            ! NPT_BOHR
            !          Spacing of the grid, in points/bohr
            !
            ! OPENSHELL
            !          Input, .TRUE. if DMATRIX_TILE contains a sigma-spin 
            !          density matrix. .FALSE. if DMATRIX_TILE contains
            !          total (alpha+beta) density matrix.
            !
            character(*), intent(in)               :: fname
            real(F64), dimension(:), intent(in)    :: dmatrix_tile
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            integer, intent(in)                    :: npt_bohr
            logical, intent(in)                    :: openshell
            character(*), intent(in)               :: griddata_fmt

            real(F64), dimension(3) :: xaxis, yaxis, zaxis, origin
            real(F64) :: unitlen
            real(F64) :: a, b, c
            integer :: xnpt, ynpt, znpt
            integer :: u
            integer :: ix, iy, iz
            real(F64) :: x, y, z
            real(F64), dimension(:), allocatable :: zrow
            integer, dimension(:), allocatable :: shellidx
            real(F64), dimension(:), allocatable :: orbval
            integer :: n0
            real(F64), dimension(3) :: dx
            real(F64) :: dxnorm
            real(F64) :: rho_scal
            real(F64) :: rhoa, rho
            real(F64), dimension(:, :), allocatable :: rhoa_ao
            real(F64), dimension(:, :), allocatable :: dmatax, dmatay, dmataz
            real(F64), dimension(:, :), allocatable :: dipx, dipy, dipz
            real(F64), dimension(3), parameter :: dip_origin = [ZERO, ZERO, ZERO]

            allocate(rhoa_ao(NORB, NORB))
            allocate(dipx(NORB, NORB))
            allocate(dipy(NORB, NORB))
            allocate(dipz(NORB, NORB))
            allocate(dmatax(NORB, NORB))
            allocate(dmatay(NORB, NORB))
            allocate(dmataz(NORB, NORB))
            !
            ! TILE2MATRIX requires initialized matrices
            !
            rhoa_ao = ZERO
            call tile2matrix(rhoa_ao, dmatrix_tile)
            call dipole(dipx, dipy, dipz, dip_origin)
            !
            ! DMATAX <- RHOA_AO^T DIPX RHOA_AO
            !
            call atsyba(dmatax, rhoa_ao, dipx, NORB)
            call atsyba(dmatay, rhoa_ao, dipy, NORB)
            call atsyba(dmataz, rhoa_ao, dipz, NORB)
            if (openshell) then
                  rho_scal = ONE
            else
                  rho_scal = FRAC12
                  !
                  ! Note that in spin-unpolarized case the RHOA_AO
                  ! matrix is equal to 2 * C_{occ} C_{occ}^T and
                  ! DMATAX is a quadratic function of RHOA_AO.
                  !
                  call scal_matrix(dmatax, FRAC14)
                  call scal_matrix(dmatay, FRAC14)
                  call scal_matrix(dmataz, FRAC14)
            end if
            call smfill(dmatax)
            call smfill(dmatay)
            call smfill(dmataz)

            unitlen = ONE / real(npt_bohr, F64)
            xaxis = [unitlen, ZERO, ZERO]
            yaxis = [ZERO, unitlen, ZERO]
            zaxis = [ZERO, ZERO, unitlen]
            call box(origin, a, b, c, znum, coord)
            xnpt = ceiling(a * real(npt_bohr, F64))
            ynpt = ceiling(b * real(npt_bohr, F64))
            znpt = ceiling(c * real(npt_bohr, F64))

            allocate(zrow(znpt))
            allocate(shellidx(NSHELL))
            allocate(orbval(LDA_DELTAK*NORB))

            u = io_text_open(fname, "REPLACE")

            call header(u, znum, coord, origin, xnpt, xaxis, &
                  ynpt, yaxis, znpt, zaxis, "Average electron-exchange hole distance | Grid spacing " &
                  // str(npt_bohr) // " points/Bohr", &
                  "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z")

            do ix = 0, xnpt-1
                  x = origin(1) + real(ix, F64) * unitlen
                  do iy = 0, ynpt-1
                        y = origin(2) + real(iy, F64) * unitlen
                        do iz = 0, znpt-1
                              z = origin(3) + real(iz, F64) * unitlen
                              call lda_vars(dmatrix_tile, x, y, z, rho, &
                                    orbval, shellidx, n0)
                              rhoa = rho_scal * rho
                              call vector_rho(dx, dmatax, dmatay, dmataz, &
                                    orbval, shellidx, n0, LDA_DELTAK)
                              !
                              ! DX is the vector defined in Eq. [15] in Ref. [1].
                              ! dx = (\frac{1}{\rho_\sigma} \sum_{ij} r_{ij\sigma} \psi_{i\sigma} \psi_{j\sigma}) - r
                              !
                              dx(1) = dx(1) / rhoa - x
                              dx(2) = dx(2) / rhoa - y
                              dx(3) = dx(3) / rhoa - z

                              dxnorm = sqrt(dx(1)**2 + dx(2)**2 + dx(3)**2)

                              zrow(iz+1) = dxnorm
                        end do
                        !
                        ! Each row of the text file contains at most 6 real numbers.
                        ! The write statement takes care of the newline characters.
                        !
                        write(u, griddata_fmt) zrow
                  end do
            end do
            close(u)

            deallocate(rhoa_ao)
            deallocate(dipx)
            deallocate(dipy)
            deallocate(dipz)
            deallocate(dmatax)
            deallocate(dmatay)
            deallocate(dmataz)
            deallocate(zrow)
            deallocate(shellidx)
            deallocate(orbval)
      end subroutine write_dx


      subroutine write_lapl(fname, dmatrix_tile, znum, coord, npt_bohr, griddata_fmt)
            character(*), intent(in)               :: fname
            real(F64), dimension(:), intent(in)    :: dmatrix_tile
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord
            integer, intent(in)                    :: npt_bohr
            character(*), intent(in)               :: griddata_fmt

            real(F64), dimension(3) :: xaxis, yaxis, zaxis, origin
            real(F64) :: unitlen
            real(F64) :: a, b, c
            integer :: xnpt, ynpt, znpt
            integer :: u
            integer :: ix, iy, iz
            real(F64) :: x, y, z
            real(F64), dimension(:), allocatable :: zrow
            integer, dimension(:), allocatable :: shellidx
            real(F64), dimension(:), allocatable :: orbval
            integer :: n0
            real(F64) :: rho, sigma, tau, lapl
            real(F64), dimension(3) :: grad

            unitlen = ONE / real(npt_bohr, F64)
            xaxis = [unitlen, ZERO, ZERO]
            yaxis = [ZERO, unitlen, ZERO]
            zaxis = [ZERO, ZERO, unitlen]
            call box(origin, a, b, c, znum, coord)
            xnpt = ceiling(a * real(npt_bohr, F64))
            ynpt = ceiling(b * real(npt_bohr, F64))
            znpt = ceiling(c * real(npt_bohr, F64))

            allocate(zrow(znpt))
            allocate(shellidx(NSHELL))
            allocate(orbval(MGGA_DELTAK*NORB))

            u = io_text_open(fname, "REPLACE")

            call header(u, znum, coord, origin, xnpt, xaxis, &
                  ynpt, yaxis, znpt, zaxis, "The Laplacian of the electron density | Grid spacing " &
                  // str(npt_bohr) // " points/Bohr", &
                  "Coordinates in atomic units | Outermost loop: X | Innermost loop: Z")

            do ix = 0, xnpt-1
                  x = origin(1) + real(ix, F64) * unitlen
                  do iy = 0, ynpt-1
                        y = origin(2) + real(iy, F64) * unitlen
                        do iz = 0, znpt-1
                              z = origin(3) + real(iz, F64) * unitlen
                              call mgga_vars_tile(dmatrix_tile, x, y, z, rho, grad, &
                                    sigma, lapl, tau, orbval, shellidx, n0)
                              zrow(iz+1) = lapl
                        end do
                        !
                        ! Each row of the text file contains at most 6 real numbers.
                        ! The write statement takes care of the newline characters.
                        !
                        write(u, griddata_fmt) zrow
                  end do
            end do
            close(u)
      end subroutine write_lapl
      

      subroutine box(origin, a, b, c, znum, coord)
            real(F64), dimension(3), intent(out)   :: origin
            real(F64), intent(out)                 :: a
            real(F64), intent(out)                 :: b
            real(F64), intent(out)                 :: c
            integer, dimension(:), intent(in)      :: znum
            real(F64), dimension(:, :), intent(in) :: coord

            integer :: natom
            real(F64) :: x0, x1, y0, y1, z0, z1
            real(F64) :: tx0, tx1, ty0, ty1, tz0, tz1
            integer :: k

            natom = size(coord, dim=2)
            call atombox(x0, x1, y0, y1, z0, z1, coord(:, 1), znum(1))

            do k = 2, natom
                  call atombox(tx0, tx1, ty0, ty1, tz0, tz1, coord(:, k), znum(k))
                  x0 = min(tx0, x0)
                  x1 = max(tx1, x1)

                  y0 = min(ty0, y0)
                  y1 = max(ty1, y1)

                  z0 = min(tz0, z0)
                  z1 = max(tz1, z1)
            end do

            origin = [x0, y0, z0]
            a = x1 - x0
            b = y1 - y0
            c = z1 - z0

            contains

                  subroutine atombox(x0, x1, y0, y1, z0, z1, coord, z)
                        real(F64), intent(out)              :: x0, x1
                        real(F64), intent(out)              :: y0, y1
                        real(F64), intent(out)              :: z0, z1
                        real(F64), dimension(3), intent(in) :: coord
                        integer, intent(in)                 :: z
                        
                        real(F64) :: r
                        
                        r = RADIUS_SCAL * ATOMIC_RADII(z)
                        x0 = coord(1) - r
                        x1 = coord(1) + r
                        y0 = coord(2) - r
                        y1 = coord(2) + r
                        z0 = coord(3) - r
                        z1 = coord(3) + r
                  end subroutine atombox
      end subroutine box


      subroutine header(u, znum, coord, origin, xnpt, xaxis, &
            ynpt, yaxis, znpt, zaxis, line1, line2, mo)
            
            integer, intent(in)                         :: u
            integer, dimension(:), intent(in)           :: znum
            real(F64), dimension(:, :), intent(in)      :: coord
            real(F64), dimension(3), intent(in)         :: origin
            integer, intent(in)                         :: xnpt
            real(F64), dimension(3)                     :: xaxis
            integer, intent(in)                         :: ynpt
            real(F64), dimension(3)                     :: yaxis
            integer, intent(in)                         :: znpt
            real(F64), dimension(3)                     :: zaxis
            character(*), intent(in)                    :: line1
            character(*), intent(in)                    :: line2
            integer, dimension(:), optional, intent(in) :: mo

            integer :: k
            character(:), allocatable :: t1, t2
            integer :: natom
            integer :: nmo
            
            if (present(mo)) then
                  nmo = size(mo)
            else
                  nmo = 0
            end if
            !
            ! First two lines are comments
            !
            write(u, "(A)") line1
            write(u, "(A)") line2 
            !
            ! Number of atoms, coordinates of the origin
            !
            natom = size(coord, dim=2)
            t1 = "(" // HEADER_IFMT // ",3" // HEADER_FFMT // ")"
            if (nmo > 0) then
                  write(u, t1) -natom, origin(1), origin(2), origin(3)
            else
                  write(u, t1) natom, origin(1), origin(2), origin(3)
            end if
            !
            ! Number of increments, axis coordinates
            ! Axis coordinates determine the step size.
            !
            write(u, t1) xnpt, xaxis(1), xaxis(2), xaxis(3)
            write(u, t1) ynpt, yaxis(1), yaxis(2), yaxis(3)
            write(u, t1) znpt, zaxis(1), zaxis(2), zaxis(3)
            !
            ! Atomic number, charge, and coordinates of an atom.
            ! Charge is always zero.
            !
            t2 = "(" // HEADER_IFMT // ",4" // HEADER_FFMT // ")"
            do k = 1, natom
                  write(u, t2) znum(k), ZERO, coord(1, k), coord(2, k), coord(3, k)
            end do
            !
            ! Number of MOs, indices of MOs
            !
            if (present(mo) .and. nmo > 0) then
                  write(u, "(10" // HEADER_IFMT // ")") nmo, mo
            end if
      end subroutine header
end module voldata

