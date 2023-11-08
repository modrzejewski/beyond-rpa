module h_xcfunc
      use arithmetic
      use math_constants
      use string
      use display

      implicit none
      !
      ! Any XC functional id code should fit into the first
      ! 12 bits of an integer
      !
      integer, parameter :: XCF_NAMELEN     = 12

      integer, parameter :: XCF_XC_NONE     = 0
      integer, parameter :: XCF_HF          = 1
      integer, parameter :: XCF_XC_B3LYP    = 100
      integer, parameter :: XCF_XC_PBE      = 101
      integer, parameter :: XCF_XC_PBE0     = 102
      integer, parameter :: XCF_XC_PW91     = 105
      integer, parameter :: XCF_XC_B3PW91   = 106
      integer, parameter :: XCF_XC_B3P86    = 107
      integer, parameter :: XCF_XC_O3LYP    = 108
      integer, parameter :: XCF_XC_X3LYP    = 109
      integer, parameter :: XCF_XC_HCTH_93  = 110
      integer, parameter :: XCF_XC_HCTH_120 = 111
      integer, parameter :: XCF_XC_HCTH_147 = 112
      integer, parameter :: XCF_XC_HCTH_407 = 113
      integer, parameter :: XCF_XC_XLYP     = 114
      integer, parameter :: XCF_XC_B1LYP    = 115
      integer, parameter :: &
            XCF_XC_BR89B94HYB               = 116
      integer, parameter :: XCF_X_M06L      = 119
      integer, parameter :: XCF_X_BR89      = 120
      integer, parameter :: XCF_X_TAU_HCTH  = 121
      integer, parameter :: XCF_XC_M05      = 122
      integer, parameter :: XCF_XC_M052X    = 123
      integer, parameter :: XCF_XC_M06      = 124
      integer, parameter :: XCF_XC_M062X    = 125
      integer, parameter :: XCF_XC_M06L     = 126
      integer, parameter :: XCF_XC_M06HF    = 127
      integer, parameter :: XCF_XC_HJS_PBE  = 129
      integer, parameter :: XCF_XC_LRCWPBEH = 130
      integer, parameter :: XCF_XC_M11      = 131
      integer, parameter :: XCF_XC_M11L     = 132
      integer, parameter :: XCF_XC_M08HX    = 133
      integer, parameter :: XCF_XC_M08SO    = 134
      integer, parameter :: XCF_X_HJS_PBE      = 138
      integer, parameter :: XCF_C_PBE          = 139
      integer, parameter :: XCF_X_HJS_PBESOL = 141
      integer, parameter :: XCF_XC_HJS_PBESOL = 142
      integer, parameter :: XCF_XC_PBESOL = 143
      integer, parameter :: XCF_XC_MCS = 145
      integer, parameter :: XCF_XC_MCSH = 146
      integer, parameter :: XCF_XC_MCSv2 = 150
      integer, parameter :: XCF_LR_HFXONLY = 151
      integer, parameter :: XCF_HFXONLY = 152
      integer, parameter :: XCF_XC_TPSS = 153
      integer, parameter :: XCF_XC_REVTPSS = 154
      integer, parameter :: XCF_X_EC_TPSS = 155
      integer, parameter :: XCF_X_B88 = 158
      integer, parameter :: XCF_XC_MVS = 160
      integer, parameter :: XCF_XC_MVSh = 161
      integer, parameter :: XCF_XC_BLYP = 162
      integer, parameter :: XCF_XC_HJS_BLYP = 164
      integer, parameter :: XCF_XC_MCSv3 = 165
      integer, parameter :: XCF_XC_EC_BLYP = 166
      integer, parameter :: XCF_XC_EC_PBEsol = 167
      integer, parameter :: XCF_XC_EC_PBE = 168
      integer, parameter :: XCF_XC_EC_TPSS = 169
      integer, parameter :: XCF_XC_EC_revTPSS = 170
      integer, parameter :: XCF_X_EC_B88 = 171
      integer, parameter :: XCF_X_HJS_B88 = 172
      integer, parameter :: XCF_X_EC_PBE = 173
      integer, parameter :: XCF_XC_EC_MVS = 174
      integer, parameter :: XCF_XC_EC_PBE_TPSS = 175
      integer, parameter :: XCF_X_PBE = 176
      integer, parameter :: XCF_XC_B88OPT_LYP = 177
      integer, parameter :: XCF_XC_HJS_PBETPSS = 178
      integer, parameter :: XCF_XC_SCAN = 179
      integer, parameter :: XCF_XC_SCAN0 = 180
      integer, parameter :: XCF_XC_LC_SCAN = 181
      integer, parameter :: XCF_XC_LC_LDATPSS = 182
      integer, parameter :: XCF_XC_LC_LDA = 183
      integer, parameter :: XCF_XC_SH_SCAN = 184
      integer, parameter :: XCF_XC_SH_PBE = 185
      integer, parameter :: XCF_XC_SH_HJS_PBE = 186
      integer, parameter :: XCF_XC_PW86PBE = 187
      integer, parameter :: XCF_XC_RPW86PBE = 188

      integer, parameter :: XCF_GGA_K_VW    = 200

      integer, parameter :: XCF_XC_RPA = 500
      !
      ! Asymptotic corrections for the exchange-correlation
      ! potential
      !
      integer, parameter :: AC_NONE = 0
      !
      ! Localized Fermi-Amaldi potential (LFAs) of Chai et al.
      ! Chi-Ruei Pan, Po-Tung Fang, and Jeng-Da Chai
      ! Phys. Rev. A 87, 052510 (2013); doi: 10.1103/PhysRevA.87.052510
      !
      integer, parameter :: AC_LFAS = 1
      !
      ! Localized Fermi-Amaldi potential of Chai et al., but
      ! with the corresponding energy formula two times the original.
      ! That modification makes the potential a functional
      ! derivative of the energy. Although the energy is now
      ! fictitious, a proper convergence is reached.
      ! The true energy can optionally be recalculated using
      ! the converged density.
      !
      integer, parameter :: AC_LFAS_v2 = 2
      !
      ! The same methods as LFAs and LFAs_v2, but intended for
      ! free atoms, that is, for the Hirshfeld weight=1 at all
      ! grid points.
      !
      integer, parameter :: AC_LFAS_FREE = 3
      integer, parameter :: AC_LFAS_v2_FREE = 4
      !
      ! Bit set to 1 if spin-uncompensated variant
      ! of functional is requested
      !
      integer, parameter :: XCF_UNCOMP      = 2**12
      !
      ! Bit set to 1 if the requested functional (pure, global hybrid,
      ! or range-separated hybrid) is of meta-GGA type. If this bit
      ! is 0, then only GGA ingredients will be computed at each
      ! grid point. For meta-GGA, the ingredients computed at each grid
      ! point are density, its gradient, the density of the kinetic
      ! energy, and the laplacian of the electronic density.
      !
      integer, parameter :: XCF_XC_MGGA     = 2**13
      !
      ! Bit set to 1 if the exchange part of the requested
      ! functional is of range-separated hybrid type.
      !
      integer, parameter :: XCF_RSHYB = 2**14
      !
      ! XC functional defined for the imaginary, antisymmetric part of
      ! the complex density matrix. Employed for the time propagation of
      ! the Kohn-Shame equation (See the appendix of Lopata, K. and Govind, N.,
      ! J. Chem. Theory Comput. 7, 1344 (2011); doi: 10.1021/ct200137z.)
      !
      integer, parameter :: XCF_IMAGINARY_DENSITY = 2**15
      !
      ! Index of the first bit of the auxiliary integral identifier.
      !
      integer, parameter :: AUX_NAMEPOS = 16
      !
      ! Length in bits of the auxiliary integral identifier.
      !
      integer, parameter :: AUX_NAMELEN = 12
      !
      ! Bit set to 1 if the requested auxiliary integral is
      ! of meta-GGA type. Otherwise, it is treated as of
      ! GGA type.
      !
      integer, parameter :: AUX_MGGA = 2**28
      !
      ! Include contribution from the bare-nuclei Hamiltonian,
      ! H_{1el} = T + V_{ne},
      ! where T is the kinetic energy operator and V_{ne} is
      ! the operator of nuclei-electrons attraction.
      !
      integer, parameter :: XCF_BAREH = 2**29
      !
      ! Include Coulomb (aka Hartree) term as defined in Hartree-Fock theory
      !
      integer, parameter :: XCF_HARTREE = 2**30
      integer, parameter :: XCF_SCREENED_HYBRID = 2**31
      !
      ! Auxiliary integral identifiers
      ! ---
      !
      integer, parameter :: AUX_NONE = XCF_XC_NONE
      integer, parameter :: AUX_ESTOMEGA_BR89 = 1
      integer, parameter :: AUX_EXCHDIP_BR89 = 2
      integer, parameter :: AUX_EXCHDIP_EXX = 3
      integer, parameter :: AUX_ESTOMEGA_EXX = 4
      integer, parameter :: AUX_GDD_OMEGA = 6
      !
      ! Integral of the density of the homogeneous electron gas
      ! kinetic energy
      !
      integer, parameter :: AUX_TAU_HEG = 7
      !
      ! Average electron-exchange hole distance weighted by the GRAC
      ! damping function
      !
      integer, parameter :: AUX_GDD_GRAC = 8
      !
      ! Average electron-exchange hole distance weighted by the Fukui
      ! function
      !
      integer, parameter :: AUX_XHOLE_DIST_FUKUI = 9
      !
      ! Atomic volumes based on the Hirshfeld partitioning
      ! (atoms in a molecule)
      !
      integer, parameter :: AUX_HIRSHFELD_VOLUME = 10
      !
      ! Atomic volumes based on the Hirshfeld partitioning
      ! (free atom)
      !
      integer, parameter :: AUX_HIRSHFELD_VOLUME_FREE = 11
      !
      ! Hirshfeld population analysis
      !
      integer, parameter :: AUX_HIRSHFELD_POPULATION = 12
      !
      ! Matrix of regularized nuclear potential,
      ! Misquitta, A. J., J. Chem. Theory Comput. 9, 5313 (2013);
      ! doi: 10.1021/ct400704a
      !
      integer, parameter :: AUX_REGULARIZED_VNUCL = 13
      !
      ! System-averaged meta-GGA correlation hole of Modrzejewski et al.
      ! Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
      ! doi: 10.1063/1.4768228
      !
      integer, parameter :: AUX_MODRZEJ2012_C_HOLE = 14
      integer, parameter :: AUX_BR_X_HOLE = 15
      integer, parameter :: AUX_PBE_X_HOLE = 16
      integer, parameter :: AUX_TPSS_X_HOLE = 17
      integer, parameter :: AUX_SR_REVTPSS_X = 19
      integer, parameter :: AUX_SR_TPSS_X = 20
      integer, parameter :: AUX_SR_LDA_X = 21
      !
      ! MCSv2 exchange-correlation energy at infinite coupling
      ! strength (lambda -> Infinity). Note that Vxc(inf) does
      ! not involve coupling constant integration.
      !
      integer, parameter :: AUX_MCSv2_VXC_INF = 22
      !
      ! The MCSv2 correlation energy integrand Vc at an arbitrary positive
      ! value of the coupling constant lambda
      !
      integer, parameter :: AUX_MCSv2_VC_LAMBDA = 23
      !
      ! PBE exchange-correlation energy at infinite coupling
      ! strength (lambda -> Infinity). Note that Vxc(inf) does
      ! not involve coupling constant integration.
      !
      integer, parameter :: AUX_PBE_VXC_INF = 24
      !
      ! Compute the derivative dVc/dlambda(lambda->0) using the correlation
      ! functional of Modrzejewski et al.
      !
      integer, parameter :: AUX_MCSv1_VC0 = 25
      !
      ! Short-range B88 exchange energy
      !
      integer, parameter :: AUX_SR_B88_X = 27
      !
      ! The MCSv1 correlation energy integrand Vc at an arbitrary positive
      ! value of the coupling constant lambda
      !
      integer, parameter :: AUX_MCSv1_VC_LAMBDA = 28
      !
      ! TPSS exchange-correlation energy at infinite coupling
      ! strength (lambda -> Infinity). Note that Vxc(inf) does
      ! not involve coupling constant integration.
      !
      integer, parameter :: AUX_TPSS_VXC_INF = 29
      !
      ! Compute the root-mean-square deviation between the computed density
      ! and a reference density read from a location in memory.
      !
      integer, parameter :: AUX_DENSITY_COMPARE = 30
      !
      ! Spherically-averaged density of atoms
      !
      integer, parameter :: AUX_RHO_SPHER = 31
      integer, parameter :: AUX_RHO_SPHER_COMPARE = 32
      integer, parameter :: AUX_RHO_DIFF = 33
      
      integer, parameter :: IVSIGMA_AA = 1
      integer, parameter :: IVSIGMA_AB = 2
      integer, parameter :: IVSIGMA_BB = 3
      !
      ! Convention of storing spin components
      ! of the second derivatives contributing to the
      ! XC kernel. It is consistent with LIBXC 1.2.
      ! Changing this convention will not affect the code
      ! beyond reading wrong numbers from LIBXC.
      !
      integer, parameter :: IV2RHO2_AA = 1
      integer, parameter :: IV2RHO2_AB = 2
      integer, parameter :: IV2RHO2_BB = 3

      integer, parameter :: IV2RHOSIGMA_A_AA = 1
      integer, parameter :: IV2RHOSIGMA_A_AB = 2
      integer, parameter :: IV2RHOSIGMA_A_BB = 3
      integer, parameter :: IV2RHOSIGMA_B_AA = 4
      integer, parameter :: IV2RHOSIGMA_B_AB = 5
      integer, parameter :: IV2RHOSIGMA_B_BB = 6

      integer, parameter :: IV2SIGMA2_AA_AA = 1
      integer, parameter :: IV2SIGMA2_AA_AB = 2
      integer, parameter :: IV2SIGMA2_AA_BB = 3
      integer, parameter :: IV2SIGMA2_AB_AB = 4
      integer, parameter :: IV2SIGMA2_AB_BB = 5
      integer, parameter :: IV2SIGMA2_BB_BB = 6
      ! ---------------------------------------------------
      !          MODELS OF THE DISPERSION ENERGY
      ! ---------------------------------------------------
      integer, parameter :: DISP_NONE = 0
      integer, parameter :: DISP_DFTD3 = 1
      integer, parameter :: DISP_MBD_RSSCS = 2
      integer, parameter :: DISP_DFTD3_BJ = 3
      integer, parameter :: DISP_MULTIPOLE_RPA = 4
      ! --------------------------------------------
      ! DEFINITION OF ONE-DETERMINANTAL METHOD
      ! AND/OR AUXILIARY INTEGRALS COMPUTED ON 
      ! THE MOLECULAR GRID
      ! --------------------------------------------
      type txcdef
            ! ---------------------------------------------------
            ! Numerical identifier of an exchange-correlation 
            ! functional and/or an auxiliary integral computed
            ! on the molecular grid. The Hartree-Fock approximation
            ! is treated as an exchange-correlation functional.
            ! When adding new components to this user-defined 
            ! type, remember to also update SENDXCDEF and
            ! RECVXCDEF.
            ! ---------------------------------------------------
            ! Bits                  | Description
            ! (inclusive intervals)
            ! ---------------------------------------------------
            ! 0 ... 11              Identifier of the exchange-
            !                       correlation functional
            ! 12                    Is the XC functional in a spin-uncompensated form?
            !                       Applies also to the auxiliary integral.
            ! 13                    Is the XC functional of meta-GGA type?
            ! 14                    Is the XC functional of range-separated hybrid type?
            ! 15                    Functional defined for imaginary density matrix
            ! 16 ... 27             Identifier of the auxliliary
            !                       integral
            ! 28                    Is the auxiliary integral of meta-GGA type?
            ! 29                    Include bare-nuclei Harmiltonian?
            ! 30                    Include Hartree term?
            ! 31                    Is the XC functional of screened range-separated hybrid type?
            !                       (only short range HF, long range fully at the semilocal level)
            !
            integer :: xc_id = XCF_XC_NONE
            !
            ! Range-separation parameter employed for the two-electron
            ! exchange integrals with the operator ERF(OMEGA*R)/R.
            ! Not referenced if XCF_RSHYB bit is set to 0.
            !
            real(F64) :: rs_omega = -ONE
            !
            ! Fraction of short-range exact HF exchange present
            ! in a range-separated exchange functional. Not
            ! referenced if XCF_RSHYB bit is set to 0.
            !
            real(F64) :: rs_srexx = -ONE
            !
            ! Fraction of exact HF exchange. Relevant for global
            ! hybrid exchange functionals.
            !
            real(F64) :: exx = ZERO
            !
            ! Asymptotic correction
            !
            integer :: AsympVxc = AC_NONE
            real(F64) :: AsympVxcOmega
            !
            ! Slater potential
            !
            logical :: SlaterVxc = .false.
      end type txcdef
      
contains

      pure subroutine xcf_set_flag(xcdef, iflag, val)
            type(txcdef), intent(inout) :: xcdef
            integer, intent(in)         :: iflag
            logical, intent(in)         :: val

            if (val) then
                  xcdef%xc_id = ior(xcdef%xc_id, iflag)
            else
                  xcdef%xc_id = iand(xcdef%xc_id, not(iflag))
            end if
      end subroutine xcf_set_flag


      pure function xcf_get_flag(xcdef, iflag)
            logical                  :: xcf_get_flag
            type(txcdef), intent(in) :: xcdef
            integer, intent(in)      :: iflag

            if (iand(xcdef%xc_id, iflag) > 0) then
                  xcf_get_flag = .true.
            else
                  xcf_get_flag = .false.
            end if
      end function xcf_get_flag


      pure function aux_get_id(ixc)
            !
            ! Extract the index of generic integral from a given
            ! integer. The following 12 bits: 14, 15, ..., 25 contain
            ! the index of requested generic integral
            !
            integer                  :: aux_get_id
            type(txcdef), intent(in) :: ixc
            
            aux_get_id = ibits(ixc%xc_id, AUX_NAMEPOS, AUX_NAMELEN)
      end function aux_get_id


      pure subroutine aux_set_id(ixc, auxname)
            !
            ! Save the index of the auxiliary integral AUXNAME
            !
            type(txcdef), intent(inout) :: ixc
            integer, intent(in)         :: auxname
            
            call mvbits(auxname, 0, AUX_NAMELEN, ixc%xc_id, AUX_NAMEPOS)
      end subroutine aux_set_id


      pure subroutine xcf_set_omega(xcdef, omega)
            type(txcdef), intent(inout)  :: xcdef
            double precision, intent(in) :: omega

            xcdef%rs_omega = omega
      end subroutine xcf_set_omega


      pure function xcf_get_omega(xcdef)
            double precision         :: xcf_get_omega
            type(txcdef), intent(in) :: xcdef

            xcf_get_omega = xcdef%rs_omega
      end function xcf_get_omega


      pure subroutine xcf_set_srexx(xcdef, srexx)
            type(txcdef), intent(inout)  :: xcdef
            double precision, intent(in) :: srexx

            xcdef%rs_srexx = srexx
      end subroutine xcf_set_srexx


      pure function xcf_get_srexx(xcdef)
            double precision         :: xcf_get_srexx
            type(txcdef), intent(in) :: xcdef
            
            xcf_get_srexx = xcdef%rs_srexx
      end function xcf_get_srexx


      pure function xcf_get_exx(xcdef)
            double precision         :: xcf_get_exx
            type(txcdef), intent(in) :: xcdef

            xcf_get_exx = xcdef%exx
      end function xcf_get_exx


      pure subroutine xcf_set_exx(xcdef, exx)
            type(txcdef), intent(inout) :: xcdef
            real(F64), intent(in)       :: exx

            xcdef%exx = exx
      end subroutine xcf_set_exx
      

      subroutine xcf_set_id(xcdef, xc_id)
            type(txcdef), intent(inout) :: xcdef
            integer, intent(in)         :: xc_id
            
            call mvbits(xc_id, 0, XCF_NAMELEN, xcdef%xc_id, 0)
      end subroutine xcf_set_id
      

      pure function xcf_get_id(ixc)
            !
            ! Extract name of the approximate method
            ! determined by the IXC descriptor
            !
            integer                  :: xcf_get_id
            type(txcdef), intent(in) :: ixc

            xcf_get_id = ibits(ixc%xc_id, 0, XCF_NAMELEN)
      end function xcf_get_id

      
      pure function xcf_isgridxc(ixc)
            !
            ! Test if IXC contains a definition of an exchange-correlation
            ! functional requiring numerical integration on the molecular
            ! grid. Auxiliary integrals are ignored by this function.
            !
            logical                  :: xcf_isgridxc
            type(txcdef), intent(in) :: ixc

            integer :: xcid

            xcid = xcf_get_id(ixc)
            if (xcid == XCF_XC_NONE .or. xcid == XCF_HF .or. &
                  xcid == XCF_LR_HFXONLY .or. xcid == XCF_HFXONLY) then
                  xcf_isgridxc = .false.
            else
                  xcf_isgridxc = .true.
            end if
      end function xcf_isgridxc


      pure function xcf_ismgga(ixc)
            logical                  :: xcf_ismgga
            type(txcdef), intent(in) :: ixc

            if (iand(ixc%xc_id, XCF_XC_MGGA) > 0) then
                  xcf_ismgga = .true.
            else
                  xcf_ismgga = .false.
            end if
      end function xcf_ismgga


      pure function aux_ismgga(ixc)
            logical                  :: aux_ismgga
            type(txcdef), intent(in) :: ixc

            if (iand(ixc%xc_id, AUX_MGGA) > 0) then
                  aux_ismgga = .true.
            else
                  aux_ismgga = .false.
            end if
      end function aux_ismgga


      pure function xcf_isrshyb(xcdef)
            logical                  :: xcf_isrshyb
            type(txcdef), intent(in) :: xcdef

            if (iand(xcdef%xc_id, XCF_RSHYB) > 0) then
                  xcf_isrshyb = .true.
            else
                  xcf_isrshyb = .false.
            end if
      end function xcf_isrshyb


      pure function xcf_isuncomp(ixc)
            !
            ! Determine whether IXC descriptor 
            ! refers to spin uncompensated variant
            ! of a functional/HF method
            !
            logical                  :: xcf_isuncomp
            type(txcdef), intent(in) :: ixc

            if (iand(ixc%xc_id, XCF_UNCOMP) .gt. 0) then
                  xcf_isuncomp = .true.
            else
                  xcf_isuncomp = .false.
            end if
      end function xcf_isuncomp


      pure function xcf_numint(ixc)
            !
            ! Test if the IXC identifier corresponds
            ! to a method involving numerical integration
            ! on the molecular grid. For instance, almost
            ! every DFT exchange-correlation functional 
            ! involves numerical integration. In contrast,
            ! pure Hartree-Fock calculations--which are
            ! treated as a subset of DFT--do not involve 
            ! numerical integration. XCF_NUMINT returns
            ! .TRUE. if there are any auxiliary (non-XC
            ! energy) integrals to be computed on the 
            ! molecular grid.
            !
            logical                  :: xcf_numint
            type(txcdef), intent(in) :: ixc

            integer :: xcid, aux_id

            if (ixc%SlaterVxc) then
                  !
                  ! Constructing model potential on the numerical grid
                  !
                  xcf_numint = .true.
            else if (ixc%AsympVxc == AC_LFAS) then
                  xcf_numint = .true.
            else
                  xcid = xcf_get_id(ixc)
                  aux_id = aux_get_id(ixc)
                  if ((xcid == XCF_XC_NONE .or. xcid == XCF_HF .or. &
                        xcid == XCF_LR_HFXONLY .or. xcid == XCF_HFXONLY) &
                        .and. aux_id == AUX_NONE) then
                        xcf_numint = .false.
                  else
                        xcf_numint = .true.
                  end if
            end if
      end function xcf_numint
      

      subroutine xcf_nullify(xcdef)
            type(txcdef), intent(out) :: xcdef

            xcdef%xc_id = XCF_XC_NONE
            xcdef%exx = ZERO
            xcdef%rs_omega = ZERO
            xcdef%rs_srexx = ZERO
      end subroutine xcf_nullify


      subroutine xcf_set_hartree(xcdef, hartree)
            !
            ! Enable/disable computing Coulomb (aka Hartree) term
            ! as defined in Hartree-Fock theory
            !
            type(txcdef), intent(inout) :: xcdef
            logical, intent(in)         :: hartree
            
            if (hartree) then
                  xcdef%xc_id = ior(xcdef%xc_id, XCF_HARTREE)
            else
                  xcdef%xc_id = iand(xcdef%xc_id, not(XCF_HARTREE))
            end if
      end subroutine xcf_set_hartree


      subroutine xcf_set_bareh(xcdef, bareh)
            !
            ! Enable/disable computing one-electron contribution
            ! (non-interacting kinetic energy plus nuclei-electrons
            ! attraction)
            !
            type(txcdef), intent(inout) :: xcdef
            logical, intent(in)         :: bareh

            if (bareh) then
                  xcdef%xc_id = ior(xcdef%xc_id, XCF_BAREH)
            else
                  xcdef%xc_id = iand(xcdef%xc_id, not(XCF_BAREH))
            end if
      end subroutine xcf_set_bareh


      function xcf_get_hartree(xcdef)
            logical                  :: xcf_get_hartree
            type(txcdef), intent(in) :: xcdef

            if (iand(xcdef%xc_id, XCF_HARTREE) > 0) then
                  xcf_get_hartree = .true.
            else
                  xcf_get_hartree = .false.
            end if
      end function xcf_get_hartree

      
      function xcf_get_bareh(xcdef)
            logical                  :: xcf_get_bareh
            type(txcdef), intent(in) :: xcdef
            
            if (iand(xcdef%xc_id, XCF_BAREH) > 0) then
                  xcf_get_bareh = .true.
            else
                  xcf_get_bareh = .false.
            end if
      end function xcf_get_bareh


      subroutine xcf_xcstring(xcdef, x, c)
            type(txcdef), intent(in)               :: xcdef
            character(:), allocatable, intent(out) :: x
            character(:), allocatable, intent(out) :: c

            select case (xcf_get_id(xcdef))
            case (XCF_HF)
                  x = "Hartree-Fock"
                  c = "No correlation"
            case (XCF_LR_HFXONLY)
                  x = "Long-range Hartree-Fock exchange"
                  c = "No correlation"
            case (XCF_HFXONLY)
                  x = "Full-range Hartree-Fock exchange"
                  c = "No correlation"
            case (XCF_X_BR89)
                  x = "Becke-Russel 89 [Phys. Rev. A 39, 3761 (1989)]"
                  c = "No correlation"
            case (XCF_X_TAU_HCTH)
                  x = "tau-HCTH [J. Chem. Phys. 116, 9559 (2002)]"
                  c = "No correlation"
            case (XCF_XC_PW91)
                  x = "PW 91 [Phys. Rev. B 46, 6671 (1992)]"
                  c = x
            case (XCF_XC_B3LYP)
                  x = "B3LYP [J. Phys. Chem. 98, 11623 (1994)]"
                  c = x
            case (XCF_C_PBE)
                  x = "Hartree-Fock"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_PBE)
                  x = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
                  c = x
            case (XCF_X_PBE)
                  x = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
                  c = "No correlation"
            case (XCF_XC_PBE0)
                  x = "PBE0 [J. Chem. Phys. 110, 5029 (1999); doi: 10.1063/1.478401]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_MVS)
                  x = "MVS [PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112]"
                  c = "MVS [PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112]"
            case (XCF_XC_MVSh)
                  x = "MVSh [PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112]"
                  c = "MVS [PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112]"
            case (XCF_XC_MCSv2)
                  x = "EC-B88 [Modrzejewski et al., unpublished]"
                  c = "MCSv2 correlation (AC-D) + VcInf(TPSS) [Modrzejewski et al., unpublished]"
            case (XCF_XC_MCSv3)
                  x = "EC-PBEsol [Modrzejewski et al., unpublished]"
                  c = "MCS correlation (AC-D) + VcInf(TPSS) [Modrzejewski et al., unpublished]"
            case (XCF_XC_EC_PBEsol)
                  x = "LC-PBEsol [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "PBEsol [Phys. Rev. Lett. 100, 136406 (2008); doi: 10.1103/PhysRevLett.100.136406]"
            case (XCF_XC_EC_PBE)
                  x = "LC-PBE [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_SH_PBE)
                  x = "PBE screened hybrid [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_EC_PBE_TPSS)
                  x = "LC-PBE [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "TPSS [Phys. Rev. Lett. 91, 146401 (2003); doi: 10.1103/PhysRevLett.91.146401]"
            case (XCF_XC_EC_MVS)
                  x = "LC-MVS [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "MVS [PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112]"
            case (XCF_XC_EC_revTPSS)
                  x = "LC-revTPSS [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "revTPSS [Phys. Rev. Lett. 103, 026403 (2009); doi: 10.1103/PhysRevLett.103.026403]"
            case (XCF_XC_EC_TPSS)
                  x = "LC-TPSS [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "TPSS [Phys. Rev. Lett. 91, 146401 (2003); doi: 10.1103/PhysRevLett.91.146401]"
            case (XCF_XC_TPSS)
                  x = "TPSS [Phys. Rev. Lett. 91, 146401 (2003); doi: 10.1103/PhysRevLett.91.146401]"
                  c = x
            case (XCF_XC_SCAN)
                  x = "SCAN [Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402]"
                  c = x
            case (XCF_XC_SCAN0)
                  x = "SCAN0 [J. Chem. Phys. 144, 044114 (2016); doi: 10.1063/1.4940734]"
                  c = "SCAN [Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402]"
            case (XCF_XC_LC_SCAN)
                  x = "LC-SCAN [Unpublished]"
                  c = "SCAN [Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402]"
            case (XCF_XC_SH_SCAN)
                  x = "SH-SCAN (screened hybrid) [Unpublished]"
                  c = "SCAN [Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402]"
            case (XCF_XC_LC_LDATPSS)
                  x = "LC-LDA [Unpublished]"
                  c = "TPSS [Phys. Rev. Lett. 91, 146401 (2003); doi: 10.1103/PhysRevLett.91.146401]"
            case (XCF_XC_LC_LDA)
                  x = "LC-LDA [Unpublished]"
                  c = "LDA"
            case (XCF_XC_revTPSS)
                  x = "revTPSS [Phys. Rev. Lett. 103, 026403 (2009); doi: 10.1103/PhysRevLett.103.026403]"
                  c = x
            case (XCF_X_EC_TPSS)
                  x = "EC-TPSS [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "No correlation"
            case (XCF_X_EC_B88)
                  x = "EC-B88 [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "No correlation"
            case (XCF_X_B88)
                  x = "B88 [Phys. Rev. A 6, 3098 (1988); doi: 10.1103/PhysRevA.38.3098]"
                  c = "No correlation"
            case (XCF_XC_BLYP)
                  x = "B88 [Phys. Rev. A 6, 3098 (1988); doi: 10.1103/PhysRevA.38.3098]"
                  c = "LYP [Chem. Phys. Lett. 157, 200 (1989); doi: 10.1016/0009-2614(89)87234-3]"
            case (XCF_XC_B88OPT_LYP)
                  x = "optB88 [J. Phys.: Condens. Matter 22, 022201 (2010); doi: 10.1088/0953-8984/22/2/022201]"
                  c = "LYP [Chem. Phys. Lett. 157, 200 (1989); doi: 10.1016/0009-2614(89)87234-3]"
            case (XCF_XC_EC_BLYP)
                  x = "EC-B88 [J. Chem. Theory Comput. 12, 3662 (2016); doi: 10.1021/acs.jctc.6b00406]"
                  c = "LYP [Chem. Phys. Lett. 157, 200 (1989); doi: 10.1016/0009-2614(89)87234-3]"
            case (XCF_XC_HCTH_93)
                  x = "HCTH-93 [J. Chem. Phys. 109, 6264 (1998)]"
                  c = x
            case (XCF_XC_HCTH_120)
                  x = "HCTH-120 [J. Chem. Phys. 112, 1670 (2000)]"
                  c = x
            case (XCF_XC_HCTH_147)
                  x = "HCTH-147 [J. Chem. Phys. 112, 1670 (2000)]"
                  c = x
            case (XCF_XC_HCTH_407)
                  x = "HCTH-407 [J. Chem. Phys. 114, 5497 (2001)]"
                  c = x
            case (XCF_XC_BR89B94HYB)
                  x = "Becke-Roussel 89 [Phys. Rev. A 39, 3761 (1989)]"
                  c = "Becke 94 [Int. J. Quantum Chem. Symp. 28, 625 (1994)]"
            case (XCF_XC_M05)
                  x = "M05 [J. Chem. Phys. 123, 161103 (2005)]"
                  c = x
            case (XCF_XC_M052X)
                  x = "M05-2X [J. Chem. Phys. 123, 161103 (2005)]"
                  c = x
            case (XCF_XC_M06)
                  x = "M06 [Theor. Chem. Acc. 120, 215 (2008)]"
                  c = x
            case (XCF_XC_M062X)
                  x = "M06-2X [Theor. Chem. Acc. 120, 215 (2008)]"
                  c = x                  
            case (XCF_XC_M06HF)
                  x = "M06-HF [J. Phys. Chem. A 110, 13126 (2006)]"
                  c = x
            case (XCF_XC_M06L)
                  x = "M06-L [J. Chem. Phys. 125, 194101 (2006)]"
                  c = x
            case (XCF_XC_PW86PBE)
                  x = "Perdew-Wang 1986 [Phys. Rev. B 33, 8800(R) (1986); doi: 10.1103/PhysRevB.33.8800]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_RPW86PBE)
                  x = "Refitted Perdew-Wang 1986 [J. Chem. Theory Comput. 5, 2574 (2009); doi: 10.1021/ct900365q]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_HJS_PBE)
                  x = "Henderson-Janesko-Scuseria wPBE [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_SH_HJS_PBE)
                  x = "Screened hybrid based on the HJS exchange hole for PBE [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_HJS_PBETPSS)
                  x = "Henderson-Janesko-Scuseria wPBE [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "TPSS [Phys. Rev. Lett. 91, 146401 (2003); doi: 10.1103/PhysRevLett.91.146401]"
            case (XCF_XC_HJS_BLYP)
                  x = "Henderson-Janesko-Scuseria wB88 [J. Chem. Theory Comput. 5, 754 (2009); doi: 10.1021/ct800530u]"
                  c = "LYP [Chem. Phys. Lett. 157, 200 (1989); doi: 10.1016/0009-2614(89)87234-3]"
            case (XCF_X_HJS_B88)
                  x = "Henderson-Janesko-Scuseria wB88 [J. Chem. Theory Comput. 5, 754 (2009); doi: 10.1021/ct800530u]"
                  c = "No correlation"
            case (XCF_X_HJS_PBE)
                  x = "Henderson-Janesko-Scuseria wPBE [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "No correlation"
            case (XCF_X_HJS_PBESOL)
                  x = "Henderson-Janesko-Scuseria wPBEsol [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "No correlation"
            case (XCF_XC_HJS_PBESOL)
                  x = "Henderson-Janesko-Scuseria wPBEsol [J. Chem. Phys. 128, 194105 (2008)]"
                  c = "PBEsol [Phys. Rev. Lett. 100, 136406 (2008); doi: 10.1103/PhysRevLett.100.136406]"
            case (XCF_XC_PBESOL)
                  x = "PBEsol [Phys. Rev. Lett. 100, 136406 (2008); doi: 10.1103/PhysRevLett.100.136406]"
                  c = x
            case (XCF_XC_MCS)
                  x = "Henderson-Janesko-Scuseria wPBEsol [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "MCS correlation [J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w]"
            case (XCF_XC_MCSH)
                  x = "Henderson-Janesko-Scuseria wPBEsol [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "MCS correlation [J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w]"
            case (XCF_XC_LRCWPBEH)
                  x = "Henderson-Janesko-Scuseria wPBE [J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797]"
                  c = "PBE [Phys. Rev. Lett. 77, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865]"
            case (XCF_XC_M11)
                  x = "M11 [J. Phys. Chem. Lett. 2, 2810 (2011)]"
                  c = x
            case (XCF_XC_M11L)
                  x = "M11L [J. Phys. Chem. Lett. 3, 117 (2012)]"
                  c = x
            case (XCF_XC_M08HX)
                  x = "M08HX [J. Chem. Theory Comput. 4, 1849 (2008)]"
                  c = x
            case (XCF_XC_M08SO)
                  x = "M08HX [J. Chem. Theory Comput. 4, 1849 (2008)]"
                  c = x
            case default
                  x = "user-defined"
                  c = "user-defined"
            end select
      end subroutine xcf_xcstring


      function xcf_str2id(s)
            !
            ! Translate a character-string keyword into
            ! integer code of an XC functional
            !
            integer                  :: xcf_str2id
            character(*), intent(in) :: s

            integer :: xcid

            xcid = XCF_XC_NONE
            select case (uppercase(s))
            case ("RHF", "HF")
                  xcid = XCF_HF
            case ("LR_HFXONLY", "LR-HFXONLY")
                  xcid = XCF_LR_HFXONLY
            case ("HFXONLY")
                  xcid = XCF_HFXONLY
            case ("BR89", "RKS-X-BR89")
                  xcid = XCF_X_BR89
            case ("RKS-X-TAU-HCTH")
                  xcid = XCF_X_TAU_HCTH
            case ("PW91", "RKS-XC-PW91")
                  xcid = XCF_XC_PW91
            case ("B3LYP", "RKS-XC-B3LYP")
                  xcid = XCF_XC_B3LYP
            case ("PBE", "RKS-XC-PBE")
                  xcid = XCF_XC_PBE
            case ("PBEX")
                  xcid = XCF_X_PBE
            case ("PBE0", "RKS-XC-PBE0")
                  xcid = XCF_XC_PBE0
            case ("MVS")
                  xcid = XCF_XC_MVS
            case ("MVSH")
                  xcid = XCF_XC_MVSh
            case ("MCSV2")
                  xcid = XCF_XC_MCSv2
            case ("MCSV3")
                  xcid = XCF_XC_MCSv3
            case ("TPSS")
                  xcid = XCF_XC_TPSS
            case ("SCAN")
                  xcid = XCF_XC_SCAN
            case ("SCAN0")
                  xcid = XCF_XC_SCAN0
            case ("LC-SCAN")
                  xcid = XCF_XC_LC_SCAN
            case ("SH-SCAN")
                  xcid = XCF_XC_SH_SCAN
            case ("SH-PBE")
                  xcid = XCF_XC_SH_PBE
            case ("SH-HJS-PBE")
                  xcid = XCF_XC_SH_HJS_PBE
            case ("LC-LDATPSS")
                  xcid = XCF_XC_LC_LDATPSS
            case ("LC-LDA")
                  xcid = XCF_XC_LC_LDA
            case ("REVTPSS")
                  xcid = XCF_XC_revTPSS
            case ("PW86PBE")
                  xcid = XCF_XC_PW86PBE
            case ("RPW86PBE")
                  xcid = XCF_XC_RPW86PBE
            case ("EC-TPSSX")
                  xcid = XCF_X_EC_TPSS
            case ("HF-PBE")
                  xcid = XCF_C_PBE
            case ("HCTH-93", "RKS-XC-HCTH-93")
                  xcid = XCF_XC_HCTH_93
            case ("HCTH-120", "RKS-XC-HCTH-120")
                  xcid = XCF_XC_HCTH_120
            case ("HCTH-147", "RKS-XC-HCTH-147")
                  xcid = XCF_XC_HCTH_147
            case ("HCTH-407", "RKS-XC-HCTH-407")
                  xcid = XCF_XC_HCTH_407
            case ("BR89B94HYB", "RKS-XC-BR89B94HYB")
                  xcid = XCF_XC_BR89B94HYB
            case ("M05")
                  xcid = XCF_XC_M05
            case ("M05-2X", "M052X")
                  xcid = XCF_XC_M052X
            case ("M06")
                  xcid = XCF_XC_M06
            case ("M06-2X", "M062X")
                  xcid = XCF_XC_M062X
            case ("M06-L", "M06L")
                  xcid = XCF_XC_M06L
            case ("M06-HF", "M06HF")
                  xcid = XCF_XC_M06HF
            case ("MCS")
                  xcid = XCF_XC_MCS
            case ("MCSH")
                  xcid = XCF_XC_MCSH
            case ("HJS_PBE", "HJS-PBE", "HJS_WPBE", "HJS-WPBE", "WPBE")
                  xcid = XCF_XC_HJS_PBE
            case ("HJS-PBETPSS", "HJS_PBETPSS")
                  xcid = XCF_XC_HJS_PBETPSS
            case ("HJS_PBEX", "HJS-PBEX", "HJS_WPBEX", "HJS-WPBEX", "WPBEX")
                  xcid = XCF_X_HJS_PBE
            case ("HJS_PBESOLX", "HJS-PBESOLX", "HJS_WPBESOLX", "HJS-WPBESOLX", "WPBESOLX")
                  xcid = XCF_X_HJS_PBESOL
            case ("HJS_PBESOL", "HJS-PBESOL", "HJS_WPBESOL", "HJS-WPBESOL", "WPBESOL")
                  xcid = XCF_XC_HJS_PBESOL
            case ("PBESOL", "PBE-SOL", "PBE_SOL")
                  xcid = XCF_XC_PBESOL
            case ("LC-WPBEH", "LC_WPBEH", "LRC-WPBEH", "LRC_WPBEH", "WPBEH")
                  !
                  ! The same functional as HJS_PBE, except the
                  ! default OMEGA and the portion of short-range 
                  ! exchange are different.
                  !
                  xcid = XCF_XC_LRCWPBEH
            case ("M11")
                  xcid = XCF_XC_M11
            case ("M11-L", "M11L")
                  xcid = XCF_XC_M11L
            case ("M08-HX", "M08HX")
                  xcid = XCF_XC_M08HX
            case ("M08-SO", "M08SO")
                  xcid = XCF_XC_M08SO
            case ("B88X")
                  xcid = XCF_X_B88
            case ("EC-B88X")
                  xcid = XCF_X_EC_B88
            case ("EC-PBEX")
                  xcid = XCF_X_EC_PBE
            case ("EC-BLYP")
                  xcid = XCF_XC_EC_BLYP
            case ("EC-PBESOL")
                  xcid = XCF_XC_EC_PBEsol
            case ("EC-PBE")
                  xcid = XCF_XC_EC_PBE
            case ("LC-PBETPSS", "EC-PBETPSS")
                  xcid = XCF_XC_EC_PBE_TPSS
            case ("EC-MVS")
                  xcid = XCF_XC_EC_MVS
            case ("EC-TPSS")
                  xcid = XCF_XC_EC_TPSS
            case ("EC-REVTPSS")
                  xcid = XCF_XC_EC_revTPSS
            case ("HJS-LC-BLYP", "HJS-BLYP")
                  xcid = XCF_XC_HJS_BLYP
            case ("HJS-LC-B88X", "HJS-B88X")
                  xcid = XCF_X_HJS_B88
            case ("BLYP")
                  xcid = XCF_XC_BLYP
            case ("B88OPT-LYP", "OPTB88-LYP")
                  xcid = XCF_XC_B88OPT_LYP
            case ("RPA")
                  xcid = XCF_XC_RPA
            case default
                  call msg("INVALID FUNCTIONAL NAME", MSG_ERROR)
                  call msg(uppercase(s), MSG_ERROR)
                  stop
            end select

            xcf_str2id = xcid
      end function xcf_str2id

      
      subroutine aux_str2id(aux_id, s)
            integer, intent(out)     :: aux_id
            character(*), intent(in) :: s

            aux_id = AUX_NONE
            select case(uppercase(s))
            case ("EXCHDIP_EXX")
                  aux_id = AUX_EXCHDIP_EXX
            case ("EXCHDIP-BR89", "EXCHDIP_BR89")
                  aux_id = AUX_EXCHDIP_BR89
            case ("GDD-OMEGA", "GDD_OMEGA")
                  aux_id = AUX_GDD_OMEGA
            case ("TAU_HEG", "TAU-HEG")
                  aux_id = AUX_TAU_HEG
            case ("GDD_GRAC")
                  aux_id = AUX_GDD_GRAC
            case ("XHOLE_DIST_FUKUI", "XHOLE-DIST-FUKUI")
                  aux_id = AUX_XHOLE_DIST_FUKUI
            case ("HIRSHFELD-VOLUME", "HIRSHFELD_VOLUME")
                  aux_id = AUX_HIRSHFELD_VOLUME
            case ("HIRSHFELD-POPULATION", "HIRSHFELD_POPULATION")
                  aux_id = AUX_HIRSHFELD_POPULATION
            case default
                  call msg("INVALID AUXILIARY INTEGRAL REQUESTED", &
                        priority=MSG_ERROR)
                  call smsg("KEYWORD USED", uppercase(s), priority=MSG_ERROR)
                  stop
            end select
      end subroutine aux_str2id

      
      subroutine xcf_components(xcdef, ix, ic)
            type(txcdef), intent(in) :: xcdef
            integer, intent(out)     :: ix
            integer, intent(out)     :: ic

            select case (xcf_get_id(xcdef))
            case (XCF_HF)
                  ix = XCF_HF
                  ic = XCF_XC_NONE
            case (XCF_XC_HJS_PBE, XCF_XC_LRCWPBEH)
                  ix = XCF_X_HJS_PBE
                  ic = XCF_C_PBE
            case (XCF_XC_M06HF)
                  ix = XCF_HF
                  ic = XCF_XC_M06HF
            case default
                  ix = XCF_XC_NONE
                  ic = XCF_XC_NONE
            end select
      end subroutine xcf_components


      subroutine xcf_define(xcdef, xc_id, aux_id, is_uncompensated)
            ! ------------------------------------------------------
            ! Create TXCDEF structure defining the
            ! exchange-correlation functional.
            ! ------------------------------------------------------
            ! XCDEF            
            !             Output, the sturcure defining the
            !             requested XC functional and/or auxiliary 
            !             integral on the molecular grid.
            !                    
            ! XC_ID
            !             Idnetifier of the requested DFT/HF method
            !
            ! AUX_ID    
            !             Identifier of the requested auxiliary
            !             integral on the molecular grid
            !
            ! IS_UNCOMPENSATED
            !             Flag determining whether spin
            !             uncompensated variant of the method
            !             should be used
            !
            type(txcdef), intent(out)    :: xcdef
            integer, intent(in)          :: xc_id
            integer, intent(in)          :: aux_id
            logical, intent(in)          :: is_uncompensated

            integer :: xcid
            real(F64) :: exx
            real(F64) :: omega
            real(F64) :: srexx
            logical :: h_1el, h_coul

            h_1el = .true.
            h_coul = .true.
            select case (xc_id)
            case (XCF_XC_NONE)
                  xcid = XCF_XC_NONE
                  exx = 0.00d+0
            case (XCF_HF)
                  xcid = XCF_HF
                  exx = 1.00d+0
            case (XCF_LR_HFXONLY)
                  xcid = XCF_LR_HFXONLY
                  xcid = ior(xcid, XCF_RSHYB)
                  srexx = 0.00_F64
                  exx = 1.00_F64
                  h_1el = .false.
                  h_coul = .false.
            case (XCF_HFXONLY)
                  xcid = XCF_HFXONLY
                  exx = 1.00_F64
                  h_1el = .false.
                  h_coul = .false.
            case (XCF_X_BR89)
                  xcid = ior(XCF_X_BR89, XCF_XC_MGGA)
                  exx = 0.00d+0
            case (XCF_X_TAU_HCTH)
                  xcid = ior(XCF_X_TAU_HCTH, XCF_XC_MGGA)
                  exx = 0.00d+0
            case (XCF_XC_PW91)
                  xcid = XCF_XC_PW91
                  exx = 0.00d+0
            case (XCF_XC_B3LYP)
                  xcid = XCF_XC_B3LYP
                  exx = 0.20d+0
            case (XCF_XC_PBE)
                  xcid = XCF_XC_PBE
                  exx = 0.00d+0
            case (XCF_XC_PW86PBE)
                  xcid = XCF_XC_PW86PBE
                  exx = 0.00d+0
            case (XCF_XC_RPW86PBE)
                  xcid = XCF_XC_RPW86PBE
                  exx = 0.00d+0
            case (XCF_X_PBE)
                  xcid = XCF_X_PBE
                  exx = 0.00d+0
            case (XCF_XC_PBE0)
                  xcid = XCF_XC_PBE0
                  exx = 0.25d+0
            case (XCF_XC_MVS)
                  xcid = ior(XCF_XC_MVS, XCF_XC_MGGA)
                  exx = 0.00_F64
            case (XCF_XC_HJS_PBETPSS)
                  xcid = ior(XCF_XC_HJS_PBETPSS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.000_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_MVSh)
                  xcid = ior(XCF_XC_MVSh, XCF_XC_MGGA)
                  exx = 0.25_F64
            case (XCF_XC_MCSv2)
                  xcid = ior(XCF_XC_MCSv2, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.470_F64
            case (XCF_XC_MCSv3)
                  xcid = ior(XCF_XC_MCSv3, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.470_F64
            case (XCF_XC_EC_BLYP)
                  xcid = ior(XCF_XC_EC_BLYP, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_EC_PBE)
                  xcid = ior(XCF_XC_EC_PBE, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.350_F64
            case (XCF_XC_SH_PBE)
                  xcid = ior(XCF_XC_SH_PBE, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  xcid = ior(xcid, XCF_SCREENED_HYBRID)
                  exx = 1.00_F64
                  srexx = 0.250_F64
                  omega = 0.150_F64
            case (XCF_XC_SH_HJS_PBE)
                  xcid = XCF_XC_SH_HJS_PBE
                  xcid = ior(xcid, XCF_RSHYB)
                  xcid = ior(xcid, XCF_SCREENED_HYBRID)
                  exx = 1.00_F64
                  srexx = 0.250_F64
                  omega = 0.110_F64
            case (XCF_XC_EC_PBE_TPSS)
                  xcid = ior(XCF_XC_EC_PBE_TPSS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.350_F64
            case (XCF_XC_LC_SCAN)
                  xcid = ior(XCF_XC_LC_SCAN, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_SH_SCAN)
                  xcid = ior(XCF_XC_SH_SCAN, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  xcid = ior(xcid, XCF_SCREENED_HYBRID)
                  exx = 1.00_F64
                  srexx = 0.250_F64
                  omega = 0.150_F64
            case (XCF_XC_LC_LDATPSS)
                  xcid = ior(XCF_XC_LC_LDATPSS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_LC_LDA)
                  xcid = ior(XCF_XC_LC_LDA, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_EC_PBEsol)
                  xcid = ior(XCF_XC_EC_PBEsol, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_EC_MVS)
                  xcid = ior(XCF_XC_EC_MVS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_X_HJS_B88)
                  xcid = ior(XCF_X_HJS_B88, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64
            case (XCF_XC_TPSS)
                  xcid = ior(XCF_XC_TPSS, XCF_XC_MGGA)
                  exx = 0.00_F64
            case (XCF_XC_SCAN)
                  xcid = ior(XCF_XC_SCAN, XCF_XC_MGGA)
                  exx = 0.00_F64
            case (XCF_XC_SCAN0)
                  xcid = ior(XCF_XC_SCAN0, XCF_XC_MGGA)
                  exx = 0.25_F64
            case (XCF_XC_EC_TPSS)
                  xcid = ior(XCF_XC_EC_TPSS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.00_F64
                  omega = 0.40_F64
            case (XCF_X_EC_TPSS)
                  xcid = ior(XCF_X_EC_TPSS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.00_F64
                  omega = 0.40_F64
            case (XCF_XC_revTPSS)
                  xcid = ior(XCF_XC_revTPSS, XCF_XC_MGGA)
                  exx = 0.00_F64
            case (XCF_XC_EC_revTPSS)
                  xcid = ior(XCF_XC_EC_revTPSS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.00_F64
                  omega = 0.40_F64
            case (XCF_X_EC_B88)
                  xcid = ior(XCF_X_EC_B88, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.00_F64
                  omega = 0.40_F64
            case (XCF_X_EC_PBE)
                  xcid = ior(XCF_X_EC_PBE, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.00_F64
                  omega = 0.40_F64
            case (XCF_XC_HJS_BLYP)
                  xcid = ior(XCF_XC_HJS_BLYP, XCF_RSHYB)
                  exx = 1.00_F64
                  srexx = 0.000_F64
                  omega = 0.400_F64                  
            case (XCF_X_B88)
                  xcid = XCF_X_B88
                  exx = 0.00_F64
            case (XCF_XC_BLYP)
                  xcid = XCF_XC_BLYP
                  exx = 0.00_F64
            case (XCF_XC_B88OPT_LYP)
                  xcid = XCF_XC_B88OPT_LYP
                  exx = 0.00_F64
            case (XCF_XC_HCTH_93)
                  xcid = XCF_XC_HCTH_93
                  exx = 0.00d+0
            case (XCF_XC_HCTH_120)
                  xcid = XCF_XC_HCTH_120
                  exx = 0.00d+0
            case (XCF_XC_HCTH_147)
                  xcid = XCF_XC_HCTH_147
                  exx = 0.00d+0
            case (XCF_XC_HCTH_407)
                  xcid = XCF_XC_HCTH_407
                  exx = 0.00d+0
            case (XCF_XC_MCS)
                  xcid = ior(XCF_XC_MCS, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  omega = 0.300_F64
                  srexx = 0.000_F64
            case (XCF_XC_MCSH)
                  xcid = ior(XCF_XC_MCSH, XCF_XC_MGGA)
                  xcid = ior(xcid, XCF_RSHYB)
                  exx = 1.00_F64
                  omega = 0.200_F64
                  srexx = 0.200_F64
            case (XCF_XC_HJS_PBE)
                  xcid = ior(XCF_XC_HJS_PBE, XCF_RSHYB)
                  exx = 1.00d+0
                  omega = 0.400_F64
                  srexx = 0.000_F64
            case (XCF_X_HJS_PBE)
                  xcid = ior(XCF_X_HJS_PBE, XCF_RSHYB)
                  exx = 1.00d+0
                  omega = 0.400_F64
                  srexx = 0.000_F64
            case (XCF_XC_HJS_PBESOL)
                  !
                  ! The default OMEGA and SREXX params for wPBEsol
                  ! are the same as for wPBE
                  !
                  xcid = ior(XCF_XC_HJS_PBESOL, XCF_RSHYB)
                  exx = 1.00d+0
                  omega = 0.400_F64
                  srexx = 0.000_F64
            case (XCF_X_HJS_PBESOL)
                  xcid = ior(XCF_X_HJS_PBESOL, XCF_RSHYB)
                  exx = 1.00d+0
                  omega = 0.400_F64
                  srexx = 0.000_F64
            case (XCF_XC_PBESOL)
                  xcid = XCF_XC_PBESOL
                  exx = 0.00d+0
            case (XCF_XC_LRCWPBEH)
                  !
                  ! The same functional as HJS_PBE, except the
                  ! default OMEGA and the portion of short-range 
                  ! exchange are different.
                  !
                  xcid = ior(XCF_XC_LRCWPBEH, XCF_RSHYB)
                  exx = 1.00d+0
                  omega = 0.200_F64
                  srexx = 0.200_F64
            case (XCF_XC_RPA)
                  xcid = XCF_XC_RPA
                  exx = 1.000_F64
            case default
                  call msg("INVALID XC IDENTIFIER", MSG_ERROR)
                  call imsg("XC_ID", xc_id, MSG_ERROR)
                  stop
            end select
            !
            ! Auxiliary (non-XC energy) integrals on the molecular grid
            ! ---
            ! Test if the requested auxiliary integral requires
            ! computing meta-GGA ingredients
            !
            if (aux_id == AUX_EXCHDIP_BR89 .or. &
                  aux_id == AUX_GDD_OMEGA .or. &
                  aux_id == AUX_XHOLE_DIST_FUKUI .or. &
                  aux_id == AUX_MODRZEJ2012_C_HOLE .or. &
                  aux_id == AUX_BR_X_HOLE .or. &
                  aux_id == AUX_PBE_X_HOLE .or. &
                  aux_id == AUX_TPSS_X_HOLE .or. &
                  aux_id == AUX_MCSv2_VXC_INF .or. &
                  aux_id == AUX_TPSS_VXC_INF .or. &
                  aux_id == AUX_MCSv2_VC_LAMBDA .or. &
                  aux_id == AUX_MCSv1_VC_LAMBDA .or. &
                  aux_id == AUX_MCSv1_VC0 .or. &
                  aux_id == AUX_DENSITY_COMPARE) then
                  xcid = ior(xcid, AUX_MGGA)
            end if
            !
            ! Spin uncompensated variant of functional
            !
            if (is_uncompensated) then
                  xcid = ior(xcid, XCF_UNCOMP)
            end if

            xcdef%xc_id = xcid
            if (iand(xcid, XCF_RSHYB) > 0) then
                  call xcf_set_omega(xcdef, omega)
                  call xcf_set_srexx(xcdef, srexx)
            else
                  call xcf_set_omega(xcdef, -ONE)
                  call xcf_set_srexx(xcdef, -ONE)
            end if
            call xcf_set_exx(xcdef, exx)
            !
            ! Enable the bare nuclei Hamiltonian
            !
            call xcf_set_flag(xcdef, XCF_BAREH, h_1el)
            !
            ! Enable the Hartree potential
            !
            call xcf_set_flag(xcdef, XCF_HARTREE, h_coul)
            !
            ! Copy the identifier bits of an auxiliary integral
            !
            call aux_set_id(xcdef, aux_id)
      end subroutine xcf_define

      
      subroutine xcf_display(xcdef)
            type(txcdef), intent(in) :: xcdef

            character(:), allocatable :: x, c

            call xcf_xcstring(xcdef, x, c)
            call msg("EXCHANGE-CORRELATION MODEL")
            if (x == c) then
                  call msg("XC " // x)
            else
                  call msg("X " // x)
                  call msg("C " // c)
            end if

            if (xcf_isrshyb(xcdef)) then
                  call dmsg("EXX FRACTION (SHORT RANGE)", xcf_get_srexx(xcdef), fmt="F5.3")
                  call dmsg("EXX FRACTION (LONG RANGE)", ONE, fmt="F5.3")
                  call dmsg("RS PARAMETER (OMEGA)", xcf_get_omega(xcdef), fmt="F6.3")
            else
                  call dmsg("EXX FRACTION", xcf_get_exx(xcdef), fmt="F5.3")
            end if

            if (.not. xcf_get_flag(xcdef, XCF_HARTREE)) then
                  call msg("Coulomb contribution disabled")
            end if

            if (.not. xcf_get_flag(xcdef, XCF_BAREH)) then
                  call msg("Kinetic and electron-nuclei contributions disabled")
            end if
      end subroutine xcf_display
end module h_xcfunc
