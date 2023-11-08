module h_voldata
      use arithmetic

      implicit none
      !
      ! Use a separate cube file for each molecular or atomic
      ! orbital. 
      !
      logical, parameter :: SEPARATE_ORB_FILES = .true.
      !
      ! Format specifier for grid data points
      !
      character(*), parameter :: GRIDDATA_DEFAULT_FMT = "(6E13.5)"
      !
      ! Format specifier for integer numbers in the header of a
      ! volumetric data file. HEADER_IFMT is used for formatting
      ! the number of atoms and atomic numbers.
      !
      character(*), parameter :: HEADER_IFMT = "I5"
      !
      ! Format specifier for floating point numbers in the header
      ! of a volumetric data file. HEADER_FFMT is used mainly
      ! for the output of atomic XYZ coordinates.
      !
      character(*), parameter :: HEADER_FFMT = "F12.6"
      !
      ! Scaling factor controlling the size of a box containing
      ! grid data. The size of the box is computed with the assumption
      ! that each atom occupies a sphere of radius RADIUS_SCAL * R.
      ! This parameter should be increased if the function computed
      ! on the grid is diffuse.
      !
      real(F64), parameter :: RADIUS_SCAL = 4.5_F64
      ! ---------------------------------------------------------------
      ! Generating volumetric data (cube files) representing functions
      ! on the molecular grid
      ! ---------------------------------------------------------------
      !
      ! Number of points per Bohr
      !
      integer, parameter :: VOL_SPACING_COARSE = 3
      integer, parameter :: VOL_SPACING_MEDIUM = 6
      integer, parameter :: VOL_SPACING_FINE   = 12
      !
      ! Available functions defined on the molecular grid
      ! ---
      !
      integer, parameter :: VOL_FUNC_NONE = 0
      !
      ! Values of molecular orbitals
      !
      integer, parameter :: VOL_FUNC_MO = 2**1
      !
      ! Electron density
      !
      integer, parameter :: VOL_FUNC_RHO = 2**2
      !
      ! The Laplacian of the electron density
      !
      integer, parameter :: VOL_FUNC_LAPL = 2**3
      !
      ! Becke's t_\sigma variable for detecting regions 
      ! of space where localized orbitals dominate:
      !
      ! t_\sigma = \tau_{\sigma,UEG} / \tau_{\sigma}.
      !
      ! See comments to WRITE_TAU_UEG_TAU.
      !
      integer, parameter :: VOL_FUNC_TAU_UEG_TAU = 2**4
      !
      ! Values of atomic orbitals
      !
      integer, parameter :: VOL_FUNC_AO = 2**5
      !
      ! Average electron--HF exchange hole distance,
      ! \sqrt{d_{X,\sigma}^2} 
      !
      integer, parameter :: VOL_FUNC_DX = 2**6
end module h_voldata
