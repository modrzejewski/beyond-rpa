module grid
      use gparam
      use math_constants
      use periodic
      use display
      use string
      use lebedev

      implicit none
      save
      !
      ! Threshold for single orbital values contributing to electronic
      ! density at each numerical grid point. Too large value of 
      ! GRID_AOTHRESH can be disastrous when large number of diffuse
      ! functions is present.
      !
      real(F64), parameter :: GRID_AOTHRESH = 1.0E-12_F64
      !
      ! Threshold for electronic density. XC contribution
      ! to the Kohn-Sham matrix is skipped if density at
      ! a given point in space is below this threshold.
      !
      real(F64), parameter :: GRID_RHOTHRESH = 1.0E-12_F64
      !
      ! ***************************************************************
      !                RADIAL GRID GLOBAL VARIABLES
      ! ***************************************************************
      !
      integer, dimension(KNOWN_ELEMENTS)          :: GRD_RGRIDK0
      double precision, dimension(:), allocatable :: GRD_RGRIDR
      double precision, dimension(:), allocatable :: GRD_RGRIDW
      integer, dimension(:), allocatable          :: GRD_RGRIDNS
      !
      ! Numerical constant used to compute the step function.
      ! Defined in Eq. 14 of Chem. Phys. Lett. 257, 213 (1996);
      ! doi: 10.1016/0009-2614(96)00600-8
      !
      double precision, parameter             :: becka = 0.64d+0
      !
      ! ***************************************************************
      !                   STANDARD GRID-1 DEFINITION
      ! ***************************************************************
      !
      ! REFERENCES:
      !
      ! 1. Gill, P., Johnson, B.G., Pople, J.A., 
      !    A standard grid for density functional calculations,
      !    Chem. Phys. Lett. 209, 506(1993)
      !
      ! ***************************************************************
      !
      ! double precision, dimension(18), parameter   :: sg1_radius = (/ &
      !       1.0000d+0, & ! 1  H    Radii for H...Ar are taken from Table 1 in [1]
      !       0.5882d+0, & ! 2  He

      !       3.0769d+0, & ! 3  Li
      !       2.0513d+0, & ! 4  Be
      !       1.5385d+0, & ! 5  B
      !       1.2308d+0, & ! 6  C
      !       1.0256d+0, & ! 7  N
      !       0.8791d+0, & ! 8  O
      !       0.7692d+0, & ! 9  F
      !       0.6838d+0, & ! 10 Ne

      !       4.0909d+0, & ! 11 Na
      !       3.1579d+0, & ! 12 Mg
      !       2.5714d+0, & ! 13 Al
      !       2.1687d+0, & ! 14 Si
      !       1.8750d+0, & ! 15 P
      !       1.6514d+0, & ! 16 S
      !       1.4754d+0, & ! 17 Cl
      !       1.3333d+0  & ! 18 Ar
      !       /)
      double precision, dimension(0:2), parameter :: sg1_alpha1 = (/ &
            ! --------------------
            ! VALUE      | ROW
            ! --------------------
            0.2500d+0, & ! 0 H-He
            0.1667d+0, & ! 1 Li-Ne
            0.1000d+0  & ! 2 Na-Ar
            /)
      double precision, dimension(0:2), parameter :: sg1_alpha2 = (/ &
            0.5000d+0, & ! 0 H-He
            0.5000d+0, & ! 1 Li-Ne
            0.4000d+0  & ! 2 Na-Ar
            /)
      double precision, dimension(0:2), parameter :: sg1_alpha3 = (/ &
            1.0000d+0, & ! 0 H-He
            0.9000d+0, & ! 1 Li-Ne
            0.8000d+0  & ! 2 Na-ar
            /)
      double precision, dimension(0:2), parameter :: sg1_alpha4 = (/ &
            4.5000d+0, & ! 0 H-He
            3.5000d+0, & ! 1 Li-Ne
            2.5000d+0  & ! 2 Na-Ar
            /)

      integer, parameter :: sg1_nr = 50
      integer, parameter :: sg1_ns(5) = (/1, 4, 7,  11,  7/)
      !
      ! ***************************************************************
      !                    PRE-DEFINED GRID SETTINGS
      ! ***************************************************************
      
      ! ------------------------ MEDIUM GRID --------------------------
      ! EULER-MACLAURIN / LEBEDEV (96; 14, 74, 146, 302, 146, 302)
      !
      integer, parameter :: medium_nr    = 96
      integer, parameter :: medium_ns(5) = (/2, 6, 9, 14, 9/)
      
      ! ------------------------- FINE GRID ---------------------------
      ! EULER-MACLAURIN / LEBEDEV (150; 26, 146, 302, 590, 302)
      ! 
      integer, parameter :: fine_nr      = 150
      integer, parameter :: fine_ns(5)   = (/3, 9, 14, 17, 14/)

      ! ----------------------- EXTRA FINE GRID -----------------------
      ! EULER-MACLAURIN / LEBEDEV (250; 38, 230, 590, 1202, 590)
      !
      !
      integer, parameter :: xfine_nr     = 250
      integer, parameter :: xfine_ns(5)  = (/4, 12, 17, 20, 17/)

      integer, private                        :: nrad
      integer, dimension(5), private          :: nleb
      ! ----------------------- QUALITY OF GRID -----------------------
      ! 1 - SG-1
      ! 2 - MEDIUM
      ! 3 - FINE
      ! 4 - EXTRA FINE
      !
      integer, protected :: GRD_QUALITY
      logical, protected :: GRD_PRUNE
      !
      ! Maximum number of spherical grid points
      ! in terms of NS parameter (see below):
      !
      integer, parameter :: max_ns = 21
      integer, dimension(max_ns), parameter :: lebnpt = (/ &
            6, &
            14, &
            26, &
            38, &
            50, &
            74, &
            86, &
            110, &
            146, &
            170, &
            194, &
            230, &
            266, &
            302, &
            350, &
            434, &
            590, &
            770, &
            974, &
            1202, &
            1454 &
            /)
      !
      ! sum(lebnpt(1:max_ns))
      !
      integer, parameter :: lebdim = 7486
      double precision, dimension(lebdim) :: GRD_LEBX, GRD_LEBY, GRD_LEBZ, GRD_LEBW
      !
      ! Number of grid points per atom of particular type.
      ! Element ordering the same as in ELEMENT global
      ! variable
      !
      integer, dimension(:), allocatable             :: GRD_ELNPT
      integer, dimension(2, 2)                       :: GRD_REAL_ATOMS
      double precision, dimension(:, :), allocatable :: GRD_DIST
      integer, dimension(:, :), allocatable          :: GRD_IDIST
      integer, dimension(:), allocatable             :: GRD_ABUND
      !
      ! ********************************************
      ! AVAILABLE LEBEDEV GRIDS
      ! ********************************************
      !
      ! LMAX       NUMBER OF POINTS   NS PARAMETER
      ! --------------------------------------------
      !   3                       6   1
      !   5                      14   2
      !   7                      26   3
      !   9                      38   4
      !  11                      50   5
      !  13                      74   6
      !  15                      86   7
      !  17                     110   8
      !  19                     146   9
      !  21                     170   10
      !  23                     194   11
      !  25                     230   12
      !  27                     266   13
      !  29                     302   14
      !  31                     350   15
      !  35                     434   16
      !  41                     590   17
      !  47                     770   18
      !  53                     974   19
      !  59                    1202   20
      !  65                    1454   21 -- MAX_NS --
      !  71                    1730
      !  77                    2030
      !  83                    2354
      !  89                    2702
      !  95                    3074
      ! 101                    3470
      ! 107                    3890
      ! 113                    4334
      ! 119                    4802
      ! 125                    5294
      ! 131                    5810
      !
      
contains
      
      subroutine grid_init(GridKind, GridPruning)
            integer, intent(in) :: GridKind
            logical, intent(in) :: GridPruning
            !
            ! Load grid according to current value
            ! of GRD_QUALITY and GRD_PRUNE variables
            !
            integer :: i, k, l, u
            integer :: k0
            integer :: n

            ! external :: ld0006, ld0014, ld0026, ld0038, ld0050
            ! external :: ld0074, ld0086, ld0110, ld0146, ld0170
            ! external :: ld0194, ld0230, ld0266, ld0302, ld0350
            ! external :: ld0434, ld0590, ld0770, ld0974, ld1202
            ! external :: ld1454

            allocate(GRD_ELNPT(NELEMENT))
            !
            ! Determine number of radial quadrature points
            ! and spherical quadrature points
            !
            call gridparam(GridKind, nrad, nleb)
            call ngridpoints(GRD_ELNPT, ELEMENT, NELEMENT, GridKind, GridPruning)
            !
            ! Generate radial grids
            !
            n = NELEMENT * nrad
            allocate(GRD_RGRIDR(n))
            allocate(GRD_RGRIDW(n))
            allocate(GRD_RGRIDNS(n))

            k0 = 1
            GRD_RGRIDK0(1:KNOWN_ELEMENTS) = -1
            do i = 1, NELEMENT
                  call emgen(GRD_RGRIDR(k0:), GRD_RGRIDW(k0:), GRD_RGRIDNS(k0:), nrad, ELEMENT(i))
                  GRD_RGRIDK0(ELEMENT(i)) = k0
                  k0 = k0 + nrad
            end do
            !            
            ! Generate spherical grids
            !
            k = 1
            k0 = 1

            call ld0006(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0014(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0026(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0038(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0050(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0074(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0086(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0110(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0146(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0170(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0194(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0230(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0266(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0302(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0350(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0434(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0590(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0770(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld0974(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld1202(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            k0 = k0 + lebnpt(k)
            k = k + 1

            call ld1454(GRD_LEBX(k0:), GRD_LEBY(k0:), GRD_LEBZ(k0:), GRD_LEBW(k0:), n)
            !
            ! Calculate abundance of elements
            ! in the integration grid
            !
            allocate(GRD_ABUND(NELEMENT))
            GRD_ABUND = 0
            do k = 1, NATOM
                  l = INUCLZ(k)
                  u = zidx(l)
                  GRD_ABUND(u) = GRD_ABUND(u) + 1
            end do
            allocate(GRD_DIST(NATOM, NATOM))
            allocate(GRD_IDIST(NATOM, NATOM))            
            GRD_DIST = dist
            GRD_IDIST = idist
      end subroutine grid_init


      subroutine grid_free()
            if (allocated(GRD_RGRIDR)) deallocate(GRD_RGRIDR)
            if (allocated(GRD_RGRIDW)) deallocate(GRD_RGRIDW)
            if (allocated(GRD_RGRIDNS)) deallocate(GRD_RGRIDNS)
            if (allocated(GRD_ELNPT)) deallocate(GRD_ELNPT)
            if (allocated(GRD_DIST)) deallocate(GRD_DIST)
            if (allocated(GRD_IDIST)) deallocate(GRD_IDIST)
            if (allocated(GRD_ABUND)) deallocate(GRD_ABUND)
      end subroutine grid_free

      
      function zidx(z)
            integer             :: zidx
            integer, intent(in) :: z

            integer :: k

            zidx = 0
            kloop: do k = 1, NELEMENT
                  if (ELEMENT(k) .eq. z) then
                        zidx = k
                        exit kloop
                  end if
            end do kloop
      end function zidx


      subroutine grid_set(q, p)
            integer, intent(in) :: q
            logical, intent(in) :: p

            GRD_QUALITY = q
            GRD_PRUNE = p
            call grid_free()
            call grid_init(q, p)
      end subroutine grid_set


      subroutine gridsummary(q, isprunned)
            integer, intent(in) :: q
            logical, intent(in) :: isprunned

            integer :: nradial, nspherical
            integer, dimension(5) :: s
            integer, dimension(known_elements) :: npt
            integer :: k

            call gridparam(q, nradial, s)
            nspherical = lebnpt(maxval(s))
            call ngridpoints(npt, ELEMENT, NELEMENT, q, isprunned)
            call midrule()
            call msg("Numerical quadrature", underline=.true.)
            call msg("Radial grid: Euler-Maclaurin, " // str(nradial) // " points")
            if (isprunned) then
                  call msg("Spherical grid: Lebedev, max " // str(nspherical) // " points, pruning enabled")
            else
                  call msg("Spherical grid: Lebedev, " // str(nspherical) // " points, pruning disabled")
            end if
            call msg("Grid points per atom:")
            do k = 1, NELEMENT
                  call imsg(ELNAME_LONG(ELEMENT(k)), npt(k))
            end do
      end subroutine gridsummary


      subroutine gridparam(q, nr, ns)
            integer, intent(in)   :: q
            integer, intent(out)  :: nr
            integer, dimension(5) :: ns

            nr = 0
            ns = 0
            
            select case (q)
            case (GRD_SG1) 
                  nr = sg1_nr
                  ns = sg1_ns
            case (GRD_MEDIUM)
                  nr = medium_nr
                  ns = medium_ns
            case (GRD_FINE)
                  nr = fine_nr
                  ns = fine_ns
            case (GRD_XFINE)
                  nr = xfine_nr
                  ns = xfine_ns
            end select
      end subroutine gridparam


      subroutine ngridpoints(n, z, nz, q, isprunned)
            ! -------------------------------------------------------------
            ! Calculate number of grid points per atom of atomic number Z,
            ! for a grid quality Q and prune flag P.
            ! -------------------------------------------------------------
            ! N         - Output, number of grid points
            ! Z         - Input, atomic number
            ! Q         - Input, quality of the grid
            ! ISPRUNNED - Input, prunning flag
            !
            integer, dimension(:), intent(out) :: n
            integer, dimension(:), intent(in)  :: z
            integer, intent(in)                :: nz
            integer, intent(in)                :: q
            logical, intent(in)                :: isprunned

            integer :: nradial, nspherical
            integer, dimension(5) :: s
            integer :: k, l
            double precision :: rl, wl
            integer :: nsl

            call gridparam(q, nradial, s)

            if (.not. isprunned) then
                  !
                  ! No prunning. Number of points of
                  ! the spherical grid is independent of
                  ! the distance from the center
                  !
                  nspherical = lebnpt(maxval(s))
                  do k = 1, nz
                        n(k) = nspherical * nradial
                  end do
            else
                  !
                  ! Grid prunning enabled. Number of points
                  ! of the spherical grid depends on the distance
                  ! from the center
                  !
                  do k = 1, nz
                        n(k) = 0
                        do l = 1, nradial
                              call empoint(l, z(k), nradial, s, isprunned, rl, wl, nsl)
                              n(k) = n(k) + lebnpt(nsl)
                        end do
                  end do
            end if
      end subroutine ngridpoints


      subroutine chebgen(x, w, n)
            ! -----------------------------------------
            ! Generate points and weights of Gauss-
            ! Chebyshev quadrature
            ! -----------------------------------------
            ! 1. Krack, M., Koster, A.M.,
            !    An adaptive numerical integrator
            !    for molecular integrals, 
            !    J. Chem. Phys. 108, 3226(1998)
            ! -----------------------------------------
            ! X - Points of integrand evaluation
            ! W - Weight of each evaluation point
            ! N - Requested order of quadrature
            !
            double precision, dimension(:), intent(out) :: x, w
            integer, intent(in) :: n

            double precision :: c, s, s2, s4
            double precision :: f, dn, di
            double precision :: xi, wi, ri
            double precision :: jac1, jac2
            integer :: i

            double precision, parameter :: ln2 = log(two)
            double precision, parameter :: d16 = 16.d+0

            dn = dble(n) + one
            f = d16 / (three * dn)

            di = one
            do i = 1, n
                  c = cos(di * pi / dn)
                  s = sin(di * pi / dn)
                  s2 = s**2
                  s4 = s2**2
                  !
                  ! Points of integrand evaluation
                  ! Eq. 9 in [1]
                  !
                  xi = (dn - two * di) / dn + two / pi * (one + frac23 * s2) * c * s
                  !
                  ! xi -> ri mapping
                  ! Eq. 13 in [1]
                  !
                  ri = one / ln2 * log(two / (one - xi))
                  !
                  ! (R, Omega) coordinates Jacobian
                  !
                  jac1 = ri**2
                  !
                  ! xi -> ri mapping Jacobian
                  ! Derived from eq. 13 in [1]
                  !
                  jac2 = one / (ln2 * (one - xi))
                  !
                  ! Weights of Gauss-Chebyshev quadrature
                  ! Eq. 10 in [1]
                  ! 4Pi factor added due to the normalization
                  ! of Lebedev grid weights to unity
                  !
                  wi = f * s4  * jac1 * jac2 * four * pi
                  x(i) = ri
                  w(i) = wi
                  di = di + one
            end do
      end subroutine chebgen


      subroutine emgen(x, w, ns, nr, z)
            ! ----------------------------------------
            ! Generate points and weights of Euler-
            ! Maclaurin quadrature as in SG-1 
            ! definition
            ! ----------------------------------------
            ! 1. Murray, W., Handy, N., Laming, G.,
            !    Quadrature schemes for integrals
            !    of density functional theory,
            !    Mol. Phys. 78, 997(1993)
            !
            ! 2. Gill, P., Johnson, B., Pople, J.,
            !    A standard grid for density
            !    functional calculations,
            !    Chem. Phys. Lett. 209, 506(1993)
            ! -----------------------------------------
            ! X  - Points of integrand evaluation
            ! W  - Weight of each evaluation point
            ! NS - Order of Lebedev quadrature
            !      corresponding to each point 
            !      of integration
            ! NR - Requested order of quadrature
            ! Z  - Nuclear charge
            !
            double precision, dimension(:), intent(out) :: x, w
            integer, dimension(:), intent(out) :: ns
            integer, intent(in) :: nr
            integer, intent(in) :: z

            integer :: i
            
            do i = 1, nr
                  call empoint(i, z, nr, nleb, GRD_PRUNE, x(i), w(i), ns(i))
            end do
      end subroutine emgen 


      subroutine empoint(i, z, nr, spher, isprunned, ri, wi, ns)
            integer, intent(in)               :: i
            integer, intent(in)               :: z
            integer, intent(in)               :: nr
            integer, dimension(5), intent(in) :: spher
            logical, intent(in)               :: isprunned
            double precision, intent(out)     :: ri, wi
            integer, intent(out)              :: ns

            double precision :: dn
            double precision :: radius
            double precision :: r1, r2, r3, r4
            double precision :: jac1, jac2
            double precision :: xi
            integer :: row

            logical :: sg1avail
            integer, parameter :: m = 2
            double precision, parameter :: dm = dble(m)
            !
            ! The parameters for pruning in all kinds of grids
            ! are derived from SG1 grid. Check if SG1 grid is
            ! available for the requested atom. If not, pruning
            ! will not be used.
            !
            row = periodic_row(z)
            if (row+1 > size(sg1_alpha1)) then
                  sg1avail = .false.
            else
                  sg1avail = .true.
            end if

            radius = ATOMIC_RADII(z)
            dn = dble(nr) 
            !
            ! Points of integrand evaluation
            ! Eq. 7 in [2]
            !
            xi = dble(i)
            ri = radius * xi**m / (dn + one - xi)**m
            !
            ! (R, Omega) coordinates Jacobian
            !
            jac1 = ri**2
            !
            ! xi -> ri mapping Jacobian
            ! Derived from eq. 13 in [1]
            !
            jac2 = (dn + one) * dm * radius * xi**(m - 1) / (dn + one - xi)**(m + 1)
            !
            ! Weights of Euler-Maclaurin quadrature
            ! 4\pi factor due to normalization
            ! of Lebedev grid weights to unity
            !
            wi = jac1 * jac2 * four * pi
            if (isprunned .and. sg1avail) then
                  r1 = sg1_alpha1(row) * radius
                  r2 = sg1_alpha2(row) * radius
                  r3 = sg1_alpha3(row) * radius
                  r4 = sg1_alpha4(row) * radius

                  if (ri .le. r1) then
                        ns = spher(1)
                  else if (ri .le. r2) then
                        ns = spher(2)
                  else if (ri .le. r3) then
                        ns = spher(3)
                  else if (ri .le. r4) then
                        ns = spher(4)
                  else
                        ns = spher(5)
                  end if
            else
                  ns = maxval(spher)
            end if
      end subroutine empoint

      
      function periodic_row(z)
            integer :: periodic_row
            integer, intent(in) :: z
            
            if (z .le. 2) then
                  periodic_row = 0
            else if (z .le. 10) then
                  periodic_row = 1
            else if (z .le. 18) then
                  periodic_row = 2
            else if (z .le. 36) then
                  periodic_row = 3
            else if (z .le. 54) then
                  periodic_row = 4
            else if (z .le. 86) then
                  periodic_row = 5
            else
                  periodic_row = 6
            end if
      end function periodic_row


      function grid_nradial()
            !
            ! Number of radial points of the currently loaded
            ! quadrature
            !
            integer :: grid_nradial

            grid_nradial = nrad
      end function grid_nradial


      subroutine rgridget(k0, znum)
            integer, intent(out) :: k0
            integer, intent(in)  :: znum

            k0 = GRD_RGRIDK0(znum)
      end subroutine rgridget

      
      subroutine lebget(k0, n)
            integer, intent(out) :: k0
            integer, intent(in)  :: n

            k0 = sum(lebnpt(1:n - 1)) + 1             
      end subroutine lebget
end module grid
