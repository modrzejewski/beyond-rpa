module xcfunc
      use gparam
      use clock
      use images
      use math_constants
      use arithmetic
      use grid
      use basis
      use br89
      use threads
      use gga
      use mgga
      use mfm
      use lcexch
      use rsbr89
      use auxint
      use h_xcfunc
      use hjs_x_energy
      use mvs_xc_energy
      use modrzej2014_xc_energy
      use modrzej2016_x_energy
      use modrzej2016_xc_energy
      use tpss_xc_energy
      use scan_xc_energy
      use b88_xc_energy
      use b88opt_xc_energy
      use pbe_xc_energy
      use hjs_xc_energy
      use pw86_xc_energy
      !$ use omp_lib

      implicit none
      save

      integer, private                                           :: nbatch
      integer, dimension(:, :), allocatable                      :: nstored
      double precision, dimension(:, :, :), allocatable, private :: orbval
      integer, dimension(:, :, :), allocatable, private          :: shellidx
      double precision, dimension(:, :), allocatable, private    :: rhovec
      double precision, dimension(:, :, :), allocatable, private :: urhovec
      double precision, dimension(:, :, :), allocatable, private :: gradvec
      double precision, dimension(:, :, :), allocatable, private :: gradavec
      double precision, dimension(:, :, :), allocatable, private :: gradbvec
      double precision, dimension(:, :), allocatable, private    :: sigmavec
      double precision, dimension(:, :, :), allocatable, private :: usigmavec
      double precision, dimension(:, :), allocatable, private    :: laplvec
      double precision, dimension(:, :, :), allocatable, private :: ulaplvec
      double precision, dimension(:, :), allocatable, private    :: tauvec
      double precision, dimension(:, :, :), allocatable, private :: utauvec
      double precision, dimension(:, :), allocatable, private    :: weightvec
      !
      ! Batch evaluation of XC potential on grid points
      !
      double precision, dimension(:, :), allocatable, private    :: epsvec
      double precision, dimension(:, :), allocatable, private    :: vrhovec
      double precision, dimension(:, :, :), allocatable, private :: uvrhovec
      double precision, dimension(:, :), allocatable, private    :: vsigmavec
      double precision, dimension(:, :, :), allocatable, private :: uvsigmavec
      double precision, dimension(:, :), allocatable, private    :: vlaplvec
      double precision, dimension(:, :, :), allocatable, private :: uvlaplvec
      double precision, dimension(:, :), allocatable, private    :: vtauvec
      double precision, dimension(:, :, :), allocatable, private :: uvtauvec
      !
      ! The identifier of the XC functional the module has been
      ! already initialized for
      !
      type(txcdef)                                        :: XCF_LOADED_DEF

contains

      subroutine xcfunc_init()
            integer :: nradial
            integer, dimension(5) :: ns

            call gridparam(GRD_FINE, nradial, ns)
            nbatch = LEBNPT(maxval(ns))
            call xcf_nullify(XCF_LOADED_DEF)
      end subroutine xcfunc_init


      subroutine gga_alloc()
            allocate(nstored(nbatch, omp_nthread))
            allocate(orbval(4 * norb, nbatch, omp_nthread))
            allocate(shellidx(nshell, nbatch, omp_nthread))
            allocate(weightvec(nbatch, omp_nthread))
            
            if (xcf_isuncomp(XCF_LOADED_DEF)) then
                  allocate(urhovec(2, nbatch, omp_nthread))
                  allocate(gradavec(3, nbatch, omp_nthread))
                  allocate(gradbvec(3, nbatch, omp_nthread))
                  allocate(usigmavec(3, nbatch, omp_nthread))
                  allocate(epsvec(nbatch, omp_nthread))
                  allocate(uvrhovec(2, nbatch, omp_nthread))
                  allocate(uvsigmavec(3, nbatch, omp_nthread))
            else
                  allocate(rhovec(nbatch, omp_nthread))
                  allocate(gradvec(3, nbatch, omp_nthread))
                  allocate(sigmavec(nbatch, omp_nthread))
                  allocate(epsvec(nbatch, omp_nthread))
                  allocate(vrhovec(nbatch, omp_nthread))
                  allocate(vsigmavec(nbatch, omp_nthread))
            end if
      end subroutine gga_alloc


      subroutine mgga_alloc()
            allocate(nstored(nbatch, omp_nthread))
            allocate(orbval(5 * norb, nbatch, omp_nthread))
            allocate(shellidx(nshell, nbatch, omp_nthread))
            allocate(weightvec(nbatch, omp_nthread))
            if (xcf_isuncomp(XCF_LOADED_DEF)) then
                  allocate(urhovec(2, nbatch, omp_nthread))
                  allocate(gradavec(3, nbatch, omp_nthread))
                  allocate(gradbvec(3, nbatch, omp_nthread))
                  allocate(usigmavec(3, nbatch, omp_nthread))
                  allocate(ulaplvec(2, nbatch, omp_nthread))
                  allocate(utauvec(2, nbatch, omp_nthread))
                  allocate(epsvec(nbatch, omp_nthread))
                  allocate(uvrhovec(2, nbatch, omp_nthread))
                  allocate(uvsigmavec(3, nbatch, omp_nthread))
                  allocate(uvlaplvec(2, nbatch, omp_nthread))
                  allocate(uvtauvec(2, nbatch, omp_nthread))
            else
                  allocate(rhovec(nbatch, omp_nthread))
                  allocate(gradvec(3, nbatch, omp_nthread))
                  allocate(sigmavec(nbatch, omp_nthread))
                  allocate(laplvec(nbatch, omp_nthread))
                  allocate(tauvec(nbatch, omp_nthread))
                  allocate(epsvec(nbatch, omp_nthread))
                  allocate(vrhovec(nbatch, omp_nthread))
                  allocate(vsigmavec(nbatch, omp_nthread))
                  allocate(vlaplvec(nbatch, omp_nthread))
                  allocate(vtauvec(nbatch, omp_nthread))
            end if
      end subroutine mgga_alloc


      subroutine gga_free()
            deallocate(nstored)
            deallocate(orbval)
            deallocate(shellidx)
            deallocate(weightvec)
            
            if (xcf_isuncomp(XCF_LOADED_DEF)) then
                  deallocate(urhovec)
                  deallocate(gradavec)
                  deallocate(gradbvec)
                  deallocate(usigmavec)
                  deallocate(epsvec)
                  deallocate(uvrhovec)
                  deallocate(uvsigmavec)
            else
                  deallocate(rhovec)
                  deallocate(gradvec)
                  deallocate(sigmavec)
                  deallocate(epsvec)
                  deallocate(vrhovec)
                  deallocate(vsigmavec)
            end if
      end subroutine gga_free
            

      subroutine mgga_free()
            deallocate(nstored)
            deallocate(orbval)
            deallocate(shellidx)
            deallocate(weightvec)
            if (xcf_isuncomp(XCF_LOADED_DEF)) then
                  deallocate(urhovec)
                  deallocate(gradavec)
                  deallocate(gradbvec)
                  deallocate(usigmavec)
                  deallocate(ulaplvec)
                  deallocate(utauvec)
                  deallocate(epsvec)
                  deallocate(uvrhovec)
                  deallocate(uvsigmavec)
                  deallocate(uvlaplvec)
                  deallocate(uvtauvec)
            else
                  deallocate(rhovec)
                  deallocate(gradvec)
                  deallocate(sigmavec)
                  deallocate(laplvec)
                  deallocate(tauvec)
                  deallocate(epsvec)
                  deallocate(vrhovec)
                  deallocate(vsigmavec)
                  deallocate(vlaplvec)
                  deallocate(vtauvec)
            end if
      end subroutine mgga_free


      subroutine destroyxc
            if (xcf_ismgga(XCF_LOADED_DEF) .or. aux_ismgga(XCF_LOADED_DEF)) then
                  call mgga_free()
            else
                  call gga_free()
            end if
            call xcf_nullify(XCF_LOADED_DEF)
      end subroutine destroyxc


      subroutine xcfunc_free()
            if (xcf_numint(XCF_LOADED_DEF)) then
                  call destroyxc()
            end if
      end subroutine xcfunc_free


      subroutine sendxcdef(xcdef)
            !
            ! Send the definition of an exchange-correlation functional
            ! and/or an auxiliary integral to slave images
            !
            type(txcdef), intent(in) :: xcdef
            
            double precision, dimension(3) :: doutmsg
            integer, dimension(IMG_IMSGSIZE) :: ioutmsg
            
            doutmsg(1) = xcf_get_omega(xcdef)
            doutmsg(2) = xcf_get_srexx(xcdef)
            doutmsg(3) = xcf_get_exx(xcdef)
            ioutmsg(1) = xcdef%xc_id
            
            call img_toslaves(doutmsg)
            call img_toslaves(ioutmsg)
      end subroutine sendxcdef


      subroutine recvxcdef(xcdef)
            !
            ! Receive the definition of an exchange-correlation functional
            ! and/or an auxiliary integral from the master image
            !
            type(txcdef), intent(out) :: xcdef
            
            double precision, dimension(3) :: dinmsg
            integer, dimension(IMG_IMSGSIZE) :: iinmsg
            
            call img_frommaster(dinmsg)
            call img_frommaster(iinmsg)

            xcdef%rs_omega = dinmsg(1)
            xcdef%rs_srexx = dinmsg(2)
            xcdef%exx = dinmsg(3)
            xcdef%xc_id = iinmsg(1)
      end subroutine recvxcdef

      
      subroutine setxcfunc(xcdef)
            ! ---------------------------------------------------------------
            ! Initialize the XCFUNC module to compute energy and derivatives
            ! using the exchange-correlation functional defined in XCDEF.
            ! This subroutine should be called before any Hartree-Fock
            ! or DFT calculations.
            ! -------------------------------------------------------------
            ! XCDEF - Identifier of the requested exchange-correlation
            !         functional. Note that the Hartree-Fock approximation
            !         is treated as an exchange-correlation functional.
            !
            type(txcdef), intent(in)     :: xcdef

            real(F64) :: omega_old, omega_new
            real(F64) :: srexx_old, srexx_new
            real(F64) :: exx_old, exx_new
            logical :: needs_alloc
            !
            ! Check if the requested functional is the same
            ! as the one previously loaded. Load the new 
            ! functional only if it differs from the old one.
            !
            if (xcf_get_id(xcdef) == xcf_get_id(XCF_LOADED_DEF) .and. &
                  (xcf_isuncomp(xcdef) .eqv. xcf_isuncomp(XCF_LOADED_DEF)) .and. &
                  aux_get_id(xcdef) == aux_get_id(XCF_LOADED_DEF)) then

                  needs_alloc = .false.
                  omega_old = xcf_get_omega(XCF_LOADED_DEF)
                  omega_new = xcf_get_omega(xcdef)
                  srexx_old = xcf_get_srexx(XCF_LOADED_DEF)
                  srexx_new = xcf_get_srexx(xcdef)
                  exx_old = xcf_get_exx(XCF_LOADED_DEF)
                  exx_new = xcf_get_exx(xcdef)
                  if (compare(omega_old, omega_new, THREE) .and. &
                        compare(srexx_old, srexx_new, THREE) .and. &
                        compare(exx_old, exx_new, THREE)) then
                        !
                        ! The requested functional is loaded. Nothing to reload.
                        !
                        return
                  end if
            else
                  needs_alloc = .true.
                  if (xcf_numint(XCF_LOADED_DEF)) then
                        call destroyxc()
                  end if
            end if
            
            XCF_LOADED_DEF = xcdef

            if (needs_alloc) then
                  if (.not. (xcf_ismgga(xcdef) .or. aux_ismgga(xcdef))) then
                        call gga_alloc()
                  else
                        call mgga_alloc()
                  end if
            end if
      end subroutine setxcfunc


      subroutine gga_batch(xcdef, npt, k)
            type(txcdef), intent(in) :: xcdef
            integer, intent(in) :: npt
            integer, intent(in) :: k

            select case (xcf_get_id(xcdef))
            case (XCF_XC_HJS_PBE, XCF_XC_LRCWPBEH)
                  call hjs_pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_SH_HJS_PBE)
                  call hjs_sh_pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_HJS_PBESOL)
                  call hjs_pbesol_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_B88)
                  call b88_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        rhovec(:, k), sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_BLYP)
                  call blyp_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        rhovec(:, k), sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_B88OPT_LYP)
                  call b88opt_lyp_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        rhovec(:, k), sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_HJS_BLYP)
                  call hjs_blyp_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        rhovec(:, k), sigmavec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_HJS_B88)
                  call hjs_b88_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        rhovec(:, k), sigmavec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_HJS_PBE)
                  call hjs_pbe_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        rhovec(:, k), sigmavec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_PBE, XCF_XC_PBE0)
                  call pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_PW86PBE)
                  call pw86pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_RPW86PBE)
                  call rpw86pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_X_PBE)
                  call pbe_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_PBEsol)
                  call pbesol_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), rhovec(:, k), &
                        sigmavec(:, k), npt, xcf_get_exx(xcdef))
            end select
      end subroutine gga_batch


      subroutine u_gga_batch(xcdef, npt, k)
            type(txcdef), intent(in) :: xcdef
            integer, intent(in) :: npt
            integer, intent(in) :: k
            
            select case (xcf_get_id(xcdef))
            case (XCF_XC_HJS_PBE, XCF_XC_LRCWPBEH)
                  call u_hjs_pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_SH_HJS_PBE)
                  call u_hjs_sh_pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))
              
            case (XCF_XC_HJS_PBESOL)
                  call u_hjs_pbesol_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_B88)
                  call u_b88_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_BLYP)
                  call u_blyp_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_B88OPT_LYP)
                  call u_b88opt_lyp_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_HJS_BLYP)
                  call u_hjs_blyp_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_HJS_B88)
                  call u_hjs_b88_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_HJS_PBE)
                  call u_hjs_pbe_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_PBE, XCF_XC_PBE0)
                  call u_pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_PW86PBE)
                  call u_pw86pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_RPW86PBE)
                  call u_rpw86pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_X_PBE)
                  call u_pbe_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))

            case (XCF_XC_PBEsol)
                  call u_pbesol_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), urhovec(:, :, k), &
                        usigmavec(:, :, k), npt, xcf_get_exx(xcdef))
            end select
      end subroutine u_gga_batch


      subroutine mgga_batch(xcdef, npt, k)
            type(txcdef), intent(in) :: xcdef
            integer, intent(in) :: npt
            integer, intent(in) :: k

            select case(xcf_get_id(xcdef))
            case (XCF_X_BR89)
                  call rks_br89(rhovec(:, k), sigmavec(:, k), laplvec(:, k), &
                        tauvec(:, k), epsvec(:, k), vrhovec(:, k), &
                        vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), npt)

            case(XCF_XC_BR89B94HYB)
                  call rks_br89b94hyb(rhovec(:, k), sigmavec(:, k), laplvec(:, k), &
                        tauvec(:, k), epsvec(:, k), vrhovec(:, k), &
                        vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_M05)
                  call m05(rhovec(:, k), sigmavec(:, k), tauvec(:, k), &
                        epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_M052X)
                  call m052x(rhovec(:, k), sigmavec(:, k), tauvec(:, k), &
                        epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_M11)
                  call m11(rhovec(:, k), sigmavec(:, k), tauvec(:, k), &
                        epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_M11L)
                  call m11l(rhovec(:, k), sigmavec(:, k), tauvec(:, k), &
                        epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_M08HX)
                  call m08hx(rhovec(:, k), sigmavec(:, k), tauvec(:, k), &
                        epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_M08SO)
                  call m08so(rhovec(:, k), sigmavec(:, k), tauvec(:, k), &
                        epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), &
                        vlaplvec(:, k), vtauvec(:, k), npt)

            case (XCF_XC_MCS)
                  call mcs_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), rhovec(:, k), &
                        sigmavec(:, k), tauvec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))
                  vlaplvec(:, k) = ZERO

            case (XCF_XC_MCSH)
                  call mcsh_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), rhovec(:, k), &
                        sigmavec(:, k), tauvec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))
                  vlaplvec(:, k) = ZERO

            case (XCF_XC_MVS, XCF_XC_MVSh)
                  call mvs_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), tauvec(:, k), npt, xcf_get_exx(xcdef))
                  vlaplvec(:, k) = ZERO

            case (XCF_XC_MCSv2)
                  call mcsv2_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_MCSv3)
                  call mcsv3_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_BLYP)
                  call ec_blyp_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_PBE)
                  call ec_pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_PBE_TPSS)
                  call ec_pbetpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_LC_SCAN)
                  call ec_scan_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_SH_SCAN)
                  call ec_sh_scan_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))
                  
            case (XCF_XC_SH_PBE)
                  call ec_sh_pbe_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_LC_LDA)
                  call ec_lda_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_LC_LDATPSS)
                  call ec_ldatpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_HJS_PBETPSS)
                  call hjs_pbetpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), rhovec(:, k), &
                        sigmavec(:, k), tauvec(:, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))
                  vlaplvec(:, k) = ZERO

            case (XCF_XC_EC_PBEsol)
                  call ec_pbesol_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_EC_B88)
                  call ec_b88_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_EC_PBE)
                  call ec_pbe_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_EC_TPSS)
                  call ec_tpss_xonly(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_TPSS)
                  call ec_tpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_revTPSS)
                  call ec_revtpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_MVS)
                  call ec_mvs_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vlaplvec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), laplvec(:, k), tauvec(:, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_TPSS)
                  call tpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), tauvec(:, k), npt, xcf_get_exx(xcdef))
                  vlaplvec(:, k) = ZERO

            case (XCF_XC_SCAN, XCF_XC_SCAN0)
                  call scan_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), tauvec(:, k), npt, xcf_get_exx(xcdef))
                  vlaplvec(:, k) = ZERO

            case (XCF_XC_revTPSS)
                  call revtpss_xc(epsvec(:, k), vrhovec(:, k), vsigmavec(:, k), vtauvec(:, k), &
                        rhovec(:, k), sigmavec(:, k), tauvec(:, k), npt, xcf_get_exx(xcdef))
                  vlaplvec(:, k) = ZERO
            end select
      end subroutine mgga_batch


      subroutine u_mgga_batch(xcdef, npt, k)
            type(txcdef), intent(in) :: xcdef
            integer, intent(in) :: npt
            integer, intent(in) :: k

            select case (xcf_get_id(xcdef))
            case (XCF_X_BR89)
                  call uks_br89(urhovec(:, :, k), usigmavec(:, :, k), &
                        ulaplvec(:, :, k), utauvec(:, :, k), epsvec(:, k), &
                        uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), npt)

            case (XCF_XC_M05)
                  call um05(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M052X)
                  call um052x(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M06)
                  call um06(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M062X)
                  call um062x(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M06L)
                  call um06l(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M06HF)
                  call um06hf(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M11)
                  call um11(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M11L)
                  call um11l(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M08HX)
                  call um08hx(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_M08SO)
                  call um08so(urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), &
                        epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), &
                        uvlaplvec(:, :, k), uvtauvec(:, :, k), npt)

            case (XCF_XC_MCS)
                  call u_mcs_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_srexx(xcdef), &
                        xcf_get_omega(xcdef))
                  uvlaplvec(:, :, k) = ZERO

            case (XCF_XC_MCSH)
                  call u_mcsh_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_srexx(xcdef), &
                        xcf_get_omega(xcdef))
                  uvlaplvec(:, :, k) = ZERO

            case (XCF_XC_MVS, XCF_XC_MVSh)
                  call u_mvs_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_exx(xcdef))
                  uvlaplvec(:, :, k) = ZERO

            case (XCF_XC_MCSv2)
                  call u_mcsv2_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), &
                        utauvec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_MCSv3)
                  call u_mcsv3_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), &
                        utauvec(:, :, k), npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_BLYP)
                  call u_ec_blyp_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_PBE)
                  call u_ec_pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_PBE_TPSS)
                  call u_ec_pbetpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_LC_SCAN)
                  call u_ec_scan_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_SH_SCAN)
                  call u_ec_sh_scan_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_SH_PBE)
                  call u_ec_sh_pbe_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_LC_LDA)
                  call u_ec_lda_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_LC_LDATPSS)
                  call u_ec_ldatpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_HJS_PBETPSS)
                  call u_hjs_pbetpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_srexx(xcdef), &
                        xcf_get_omega(xcdef))
                  uvlaplvec(:, :, k) = ZERO

            case (XCF_XC_EC_PBEsol)
                  call u_ec_pbesol_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_EC_B88)
                  call u_ec_b88_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_X_EC_PBE)
                  call u_ec_pbe_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))
                  
            case (XCF_X_EC_TPSS)
                  call u_ec_tpss_xonly(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_TPSS)
                  call u_ec_tpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_revTPSS)
                  call u_ec_revtpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_EC_MVS)
                  call u_ec_mvs_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvlaplvec(:, :, k), &
                        uvtauvec(:, :, k), urhovec(:, :, k), usigmavec(:, :, k), ulaplvec(:, :, k), utauvec(:, :, k), &
                        npt, xcf_get_srexx(xcdef), xcf_get_omega(xcdef))

            case (XCF_XC_TPSS)
                  call u_tpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_exx(xcdef))
                  uvlaplvec(:, :, k) = ZERO

            case (XCF_XC_SCAN, XCF_XC_SCAN0)
                  call u_scan_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_exx(xcdef))
                  uvlaplvec(:, :, k) = ZERO

            case (XCF_XC_revTPSS)
                  call u_revtpss_xc(epsvec(:, k), uvrhovec(:, :, k), uvsigmavec(:, :, k), uvtauvec(:, :, k), &
                        urhovec(:, :, k), usigmavec(:, :, k), utauvec(:, :, k), npt, xcf_get_exx(xcdef))
                  uvlaplvec(:, :, k) = ZERO
            end select
      end subroutine u_mgga_batch


      subroutine mgga_spherquad(rhomatrix, k, x0, y0, z0, r, rweight, ns, id)
            ! -------------------------------------------------
            ! Perform spherical part of integration. Calculate
            ! Becke's weights. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham
            ! matrix.
            ! -------------------------------------------------
            ! 1. Stratmann, R.E. et al., Achieving linear
            !    scaling in exchange- correlation density
            !    functional quadratures,
            !    Chem. Phys. Lett. 257, 213(1996)
            !
            ! 2. Becke, A.D.,  A multicenter numerical
            !    integration scheme for polyatomic
            !    molecules,
            !    J. Chem. Phys. 88, 2547(1988)
            ! -------------------------------------------------
            ! RHOMATRIX - Density matrix. Assumed that both
            !             lower and upper diagonal parts are
            !             stored
            ! K         - Central atom, see Becke's paper for
            !             explanation
            ! X0,Y0,Z0  - Coordinates of central atom
            ! R         - Radial quadrature point
            ! RWEIGHT   - Weight of radial quadrature point
            !             including (-1, 1) -> (0, \inf)
            !             transformation Jacobian and R**2.
            ! NS        - Index of the requested Lebedev grid
            ! ID        - Thread ID
            !
            double precision, dimension(:, :), intent(in)    :: rhomatrix
            integer, intent(in)                              :: k
            double precision, intent(in)                     :: x0, y0, z0, r
            double precision, intent(in)                     :: rweight
            integer, intent(in)                              :: ns
            integer, intent(in)                              :: id

            double precision :: x, y, z
            double precision :: ar
            double precision :: w1, w2, wm, s
            double precision :: rmk, rmg, mu
            double precision, dimension(3) :: rm
            integer :: u, l0
            integer :: idm, m
            integer :: wcntrb

            double precision :: const1 = two / (one - becka)
            !
            ! Check if Becke's weight is equal to one.
            ! Inequality 15 in [1]
            !
            ar = const1 * r
            if (ar .le. grd_dist(2, k)) then
                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)
                        weightvec(u, id) = rweight * lebw(u)
                        !
                        ! Generate next point in batch
                        !
                        call mgga_vars(rhomatrix, x, y, z, &
                              rhovec(u, id), gradvec(:, u, id), sigmavec(u, id), &
                              laplvec(u, id), tauvec(u, id), orbval(:, u, id), &
                              shellidx(:, u, id), nstored(u, id))
                  end do
                  end associate
            else
                  wcntrb = 1
                  do u = 3, NATOM
                        if (ar .le. grd_dist(u, k)) then
                              exit
                        else
                              wcntrb = wcntrb + 1
                        end if
                  end do

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  sphere: do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)

                        rmk = grd_dist(2, k)
                        idm = grd_idist(2, k)
                        rm = atomr(:, idm)
                        rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                        mu = (r - rmg) / (becka * rmk)

                        if (mu .lt. one) then
                              if (mu .le. -one) then
                                    w1 = one
                                    w2 = zero
                              else
                                    call step_function(mu, w1)
                                    call becke(idm, rmg, x, y, z, w2)
                              end if

                              weights: do m = 3, wcntrb + 1
                                    rmk = grd_dist(m, k)
                                    idm = grd_idist(m, k)
                                    rm = atomr(:, idm)
                                    rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                    mu = (r - rmg) / (becka * rmk)

                                    if (mu .ge. one) then
                                          !
                                          ! The step function employed in the weight factor is exactly
                                          ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                          ! must be defined as zero to avoid processing NaNs and/or unphysical
                                          ! values which are present due to the lack of initialization.
                                          !
                                          rhovec(u, id) = ZERO
                                          gradvec(:, u, id) = ZERO
                                          sigmavec(u, id) = ZERO
                                          laplvec(u, id) = ZERO
                                          tauvec(u, id) = ZERO
                                          weightvec(u, id) = ZERO
                                          nstored(u, id) = 0
                                          cycle sphere
                                    else if (mu .le. -one) then
                                          cycle weights
                                    else
                                          call step_function(mu, s)
                                          w1 = w1 * s
                                          call becke(idm, rmg, x, y, z, wm)
                                          w2 = w2 + wm
                                    end if
                              end do weights

                              w2 = w2 + w1
                              weightvec(u, id) = rweight * lebw(u) * w1 / w2

                              call mgga_vars(rhomatrix, x, y, z, &
                                    rhovec(u, id), gradvec(:, u, id), sigmavec(u, id), &
                                    laplvec(u, id), tauvec(u, id), orbval(:, u, id), &
                                    shellidx(:, u, id), nstored(u, id))
                        else
                              rhovec(u, id) = ZERO
                              gradvec(:, u, id) = ZERO
                              sigmavec(u, id) = ZERO
                              laplvec(u, id) = ZERO
                              tauvec(u, id) = ZERO
                              weightvec(u, id) = ZERO
                              nstored(u, id) = 0
                        end if
                  end do sphere
                  end associate
            end if
      end subroutine mgga_spherquad


      subroutine umgga_spherquad(rhomatrixa, rhomatrixb, k, x0, y0, z0, r, rweight, ns, id)
            ! -------------------------------------------------------
            ! Perform spherical part of integration. Calculate
            ! Becke's weights. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham
            ! matrix.
            ! -------------------------------------------------------
            ! 1. Stratmann, R.E. et al., Achieving linear
            !    scaling in exchange- correlation density
            !    functional quadratures,
            !    Chem. Phys. Lett. 257, 213(1996)
            !
            ! 2. Becke, A.D.,  A multicenter numerical
            !    integration scheme for polyatomic
            !    molecules,
            !    J. Chem. Phys. 88, 2547(1988)
            ! --------------------------------------------------------
            ! RHOMATRIXA - Density matrix of alpha (beta) electrons.
            ! RHOMATRIXB   Assumed that both lower and upper diagonal
            !              parts are stored
            ! K          - Central atom, see Becke's paper for
            !              explanation
            !
            ! X0,Y0,Z0   - Coordinates of central atom
            !
            ! R          - Radial quadrature point
            !
            ! RWEIGHT    - Weight of radial quadrature point
            !              including (-1, 1) -> (0, \inf)
            !              transformation Jacobian and R**2.
            !
            ! NS         - Index of requested Lebedev grid
            !
            ! ID         - Thread ID
            !
            double precision, dimension(:, :), intent(in)    :: rhomatrixa
            double precision, dimension(:, :), intent(in)    :: rhomatrixb
            integer, intent(in)                              :: k
            double precision, intent(in)                     :: x0, y0, z0, r
            double precision, intent(in)                     :: rweight
            integer, intent(in)                              :: ns
            integer, intent(in)                              :: id

            double precision :: x, y, z
            double precision :: ar
            double precision :: w1, w2, wm, s
            double precision :: rmk, rmg, mu
            double precision, dimension(3) :: rm
            integer :: u, l0
            integer :: idm, m
            integer :: wcntrb

            double precision :: const1 = two / (one - becka)
            !
            ! Check if Becke's weight equal to one.
            ! Inequality 15 in [1]
            !
            ar = const1 * r
            if (ar .le. grd_dist(2, k)) then

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)
                        weightvec(u, id) = rweight * lebw(u)
                        !
                        ! Generate next point in batch
                        !
                        call umgga_vars(rhomatrixa, rhomatrixb, x, y, z, &
                              urhovec(1, u, id), urhovec(2, u, id), gradavec(:, u, id), &
                              gradbvec(:, u, id), usigmavec(1, u, id), usigmavec(3, u, id), usigmavec(2, u, id), &
                              ulaplvec(1, u, id), ulaplvec(2, u, id), utauvec(1, u, id), utauvec(2, u, id), &
                              orbval(:, u, id), shellidx(:, u, id), nstored(u, id))
                  end do
                  end associate
            else
                  wcntrb = 1
                  do u = 3, NATOM
                        if (ar .le. grd_dist(u, k)) then
                              exit
                        else
                              wcntrb = wcntrb + 1
                        end if
                  end do

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  sphere: do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)

                        rmk = grd_dist(2, k)
                        idm = grd_idist(2, k)
                        rm = atomr(:, idm)
                        rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                        mu = (r - rmg) / (becka * rmk)

                        if (mu .lt. one) then
                              if (mu .le. -one) then
                                    w1 = one
                                    w2 = zero
                              else
                                    call step_function(mu, w1)
                                    call becke(idm, rmg, x, y, z, w2)
                              end if

                              weights: do m = 3, wcntrb + 1
                                    rmk = grd_dist(m, k)
                                    idm = grd_idist(m, k)
                                    rm = atomr(:, idm)
                                    rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                    mu = (r - rmg) / (becka * rmk)

                                    if (mu .ge. one) then
                                          !
                                          ! The step function employed in the weight factor is exactly
                                          ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                          ! must be defined as zero to avoid processing NaNs and/or unphysical
                                          ! values which are present due to the lack of initialization.
                                          !
                                          urhovec(:, u, id) = ZERO
                                          gradavec(:, u, id) = ZERO
                                          gradbvec(:, u, id) = ZERO
                                          usigmavec(:, u, id) = ZERO
                                          ulaplvec(:, u, id) = ZERO
                                          utauvec(:, u, id) = ZERO
                                          weightvec(u, id) = ZERO
                                          nstored(u, id) = 0
                                          cycle sphere
                                    else if (mu .le. -one) then
                                          cycle weights
                                    else
                                          call step_function(mu, s)
                                          w1 = w1 * s
                                          call becke(idm, rmg, x, y, z, wm)
                                          w2 = w2 + wm
                                    end if
                              end do weights

                              w2 = w2 + w1
                              weightvec(u, id) = rweight * lebw(u) * w1 / w2

                              call umgga_vars(rhomatrixa, rhomatrixb, x, y, z, &
                                    urhovec(1, u, id), urhovec(2, u, id), gradavec(:, u, id), &
                                    gradbvec(:, u, id), usigmavec(1, u, id), usigmavec(3, u, id), &
                                    usigmavec(2, u, id), ulaplvec(1, u, id), ulaplvec(2, u, id), &
                                    utauvec(1, u, id), utauvec(2, u, id), orbval(:, u, id), &
                                    shellidx(:, u, id), nstored(u, id))
                        else
                              urhovec(:, u, id) = ZERO
                              gradavec(:, u, id) = ZERO
                              gradbvec(:, u, id) = ZERO
                              usigmavec(:, u, id) = ZERO
                              ulaplvec(:, u, id) = ZERO
                              utauvec(:, u, id) = ZERO
                              weightvec(u, id) = ZERO
                              nstored(u, id) = 0
                        end if
                  end do sphere
                  end associate
            end if
      end subroutine umgga_spherquad


      subroutine gga_spherquad(rhomatrix, k, x0, y0, z0, r, rweight, ns, id)
            ! ----------------------------------------------------
            ! Perform spherical part of integration. Calculate
            ! Becke's weights. Call KSCONTRIB for each point
            ! in order to make contributions to the Kohn-Sham
            ! matrix.
            ! ----------------------------------------------------
            ! 1. Stratmann, R.E. et al., Achieving linear
            !    scaling in exchange- correlation density
            !    functional quadratures,
            !    Chem. Phys. Lett. 257, 213(1996)
            !
            ! 2. Becke, A.D.,  A multicenter numerical
            !    integration scheme for polyatomic
            !    molecules,
            !    J. Chem. Phys. 88, 2547(1988)
            ! ----------------------------------------------------
            ! RHOMATRIX - Density matrix. Assumed that both
            !             lower and upper diagonal parts are
            !             stored.
            !
            ! K        - Central atom, see Becke's paper for
            !            explanation
            !
            ! X0,Y0,Z0 - Coordinates of central atom
            !
            ! R        - Radial quadrature point
            !
            ! RWEIGHT  - Weight of radial quadrature point
            !            including (-1, 1) -> (0, \inf)
            !            transformation Jacobian and R**2.
            !
            ! NS       - Index of requested Lebedev grid
            !
            ! ID       - Thread ID
            !            
            !
            double precision, dimension(:, :), intent(in)    :: rhomatrix
            integer, intent(in)                              :: k
            double precision, intent(in)                     :: x0, y0, z0, r
            double precision, intent(in)                     :: rweight
            integer, intent(in)                              :: ns
            integer, intent(in)                              :: id

            double precision :: x, y, z
            double precision :: ar
            double precision :: w1, w2, wm, s
            double precision :: rmk, rmg, mu
            double precision, dimension(3) :: rm
            integer :: u, l0
            integer :: idm, m
            integer :: wcntrb

            double precision :: const1 = two / (one - becka)
            !
            ! Check if Becke's weight equal to one.
            ! Inequality 15 in [1]
            !
            ar = const1 * r
            if (ar .le. grd_dist(2, k)) then

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)
                        weightvec(u, id) = rweight * lebw(u)
                        !
                        ! Generate next point in batch
                        !
                        call gga_vars(rhomatrix, x, y, z, &
                              rhovec(u, id), gradvec(:, u, id), sigmavec(u, id), &
                              orbval(:, u, id), shellidx(:, u, id), nstored(u, id))
                  end do
                  end associate
            else
                  wcntrb = 1
                  do u = 3, NATOM
                        if (ar .le. grd_dist(u, k)) then
                              exit
                        else
                              wcntrb = wcntrb + 1
                        end if
                  end do

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  sphere: do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)

                        rmk = grd_dist(2, k)
                        idm = grd_idist(2, k)
                        rm = atomr(:, idm)
                        rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                        mu = (r - rmg) / (becka * rmk)

                        if (mu .lt. one) then
                              if (mu .le. -one) then
                                    w1 = one
                                    w2 = zero
                              else
                                    call step_function(mu, w1)
                                    call becke(idm, rmg, x, y, z, w2)
                              end if

                              weights: do m = 3, wcntrb + 1
                                    rmk = grd_dist(m, k)
                                    idm = grd_idist(m, k)
                                    rm = atomr(:, idm)
                                    rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                    mu = (r - rmg) / (becka * rmk)

                                    if (mu .ge. one) then
                                          !
                                          ! The step function employed in the weight factor is exactly
                                          ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                          ! must be defined as zero to avoid processing NaNs and/or unphysical
                                          ! values which are present due to the lack of initialization.
                                          !
                                          rhovec(u, id) = ZERO
                                          gradvec(:, u, id) = ZERO
                                          sigmavec(u, id) = ZERO
                                          weightvec(u, id) = ZERO
                                          nstored(u, id) = 0
                                          cycle sphere
                                    else if (mu .le. -one) then
                                          cycle weights
                                    else
                                          call step_function(mu, s)
                                          w1 = w1 * s
                                          call becke(idm, rmg, x, y, z, wm)
                                          w2 = w2 + wm
                                    end if
                              end do weights

                              w2 = w2 + w1
                              weightvec(u, id) = rweight * lebw(u) * w1 / w2

                              call gga_vars(rhomatrix, x, y, z, &
                                    rhovec(u, id), gradvec(:, u, id), sigmavec(u, id), &
                                    orbval(:, u, id), shellidx(:, u, id), nstored(u, id))
                        else
                              rhovec(u, id) = ZERO
                              gradvec(:, u, id) = ZERO
                              sigmavec(u, id) = ZERO
                              weightvec(u, id) = ZERO
                              nstored(u, id) = 0
                        end if
                  end do sphere
                  end associate
            end if
      end subroutine gga_spherquad


      subroutine ugga_spherquad(rhomatrixa, rhomatrixb, k, x0, y0, z0, &
            r, rweight, ns, id)
            ! ------------------------------------------------------
            ! Perform spherical part of integration. Calculate
            ! Becke's weights. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham
            ! matrix.
            ! ------------------------------------------------------
            ! 1. Stratmann, R.E. et al., Achieving linear
            !    scaling in exchange- correlation density
            !    functional quadratures,
            !    Chem. Phys. Lett. 257, 213(1996)
            !
            ! 2. Becke, A.D.,  A multicenter numerical
            !    integration scheme for polyatomic
            !    molecules,
            !    J. Chem. Phys. 88, 2547(1988)
            ! -------------------------------------------------------
            ! RHOMATRIXA - Spin density matrices. Assumed that both
            ! RHOMATRIXB   lower and upper diagonal parts are
            !              stored.
            !
            ! K          - Central atom, see Becke's paper for
            !              explanation
            !
            ! X0,Y0,Z0   - Coordinates of central atom
            !
            ! R          - Radial quadrature point
            !
            ! RWEIGHT    - Weight of radial quadrature point
            !              including (-1, 1) -> (0, \inf)
            !              transformation Jacobian and R**2.
            !
            ! NS         - Index of requested Lebedev grid
            !
            ! ID         - Id number of calling thread
            !            
            !
            double precision, dimension(:, :), intent(in)    :: rhomatrixa
            double precision, dimension(:, :), intent(in)    :: rhomatrixb
            integer, intent(in)                              :: k
            double precision, intent(in)                     :: x0, y0, z0, r
            double precision, intent(in)                     :: rweight
            integer, intent(in)                              :: ns
            integer, intent(in)                              :: id

            double precision :: x, y, z
            double precision :: ar
            double precision :: w1, w2, wm, s
            double precision :: rmk, rmg, mu
            double precision, dimension(3) :: rm
            integer :: u, l0
            integer :: idm, m
            integer :: wcntrb

            double precision :: const1 = two / (one - becka)
            !
            ! Check if Becke's weight equal to one.
            ! Inequality 15 in [1]
            !
            ar = const1 * r
            if (ar .le. grd_dist(2, k)) then

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)
                        weightvec(u, id) = rweight * lebw(u)
                        !
                        ! Generate next point in batch
                        !
                        call ugga_vars(rhomatrixa, rhomatrixb, x, y, z, &
                              urhovec(1, u, id), urhovec(2, u, id), gradavec(:, u, id), &
                              gradbvec(:, u, id), usigmavec(1, u, id), usigmavec(3, u, id), &
                              usigmavec(2, u, id), orbval(:, u, id), shellidx(:, u, id), &
                              nstored(u, id))
                  end do

                  end associate
            else
                  wcntrb = 1
                  do u = 3, NATOM
                        if (ar .le. grd_dist(u, k)) then
                              exit
                        else
                              wcntrb = wcntrb + 1
                        end if
                  end do

                  call lebget(l0, ns)
                  associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                        lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

                  sphere: do u = 1, lebnpt(ns)
                        x = x0 + r * lebx(u)
                        y = y0 + r * leby(u)
                        z = z0 + r * lebz(u)

                        rmk = grd_dist(2, k)
                        idm = grd_idist(2, k)
                        rm = atomr(:, idm)
                        rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                        mu = (r - rmg) / (becka * rmk)

                        if (mu .lt. one) then
                              if (mu .le. -one) then
                                    w1 = one
                                    w2 = zero
                              else
                                    call step_function(mu, w1)
                                    call becke(idm, rmg, x, y, z, w2)
                              end if

                              weights: do m = 3, wcntrb + 1
                                    rmk = grd_dist(m, k)
                                    idm = grd_idist(m, k)
                                    rm = atomr(:, idm)
                                    rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                    mu = (r - rmg) / (becka * rmk)

                                    if (mu .ge. one) then
                                          !
                                          ! The step function employed in the weight factor is exactly
                                          ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                          ! must be defined as zero to avoid processing NaNs and/or unphysical
                                          ! values which are present due to the lack of initialization.
                                          !
                                          urhovec(:, u, id) = ZERO
                                          gradavec(:, u, id) = ZERO
                                          gradbvec(:, u, id) = ZERO
                                          usigmavec(:, u, id) = ZERO
                                          weightvec(u, id) = ZERO
                                          nstored(u, id) = 0
                                          cycle sphere
                                    else if (mu .le. -one) then
                                          cycle weights
                                    else
                                          call step_function(mu, s)
                                          w1 = w1 * s
                                          call becke(idm, rmg, x, y, z, wm)
                                          w2 = w2 + wm
                                    end if
                              end do weights

                              w2 = w2 + w1
                              weightvec(u, id) = rweight * lebw(u) * w1 / w2

                              call ugga_vars(rhomatrixa, rhomatrixb, x, y, z, &
                                    urhovec(1, u, id), urhovec(2, u, id), gradavec(:, u, id), &
                                    gradbvec(:, u, id), usigmavec(1, u, id), usigmavec(3, u, id), &
                                    usigmavec(2, u, id), orbval(:, u, id), shellidx(:, u, id), &
                                    nstored(u, id))
                        else
                              urhovec(:, u, id) = ZERO
                              gradavec(:, u, id) = ZERO
                              gradbvec(:, u, id) = ZERO
                              usigmavec(:, u, id) = ZERO
                              weightvec(u, id) = ZERO
                              nstored(u, id) = 0
                        end if
                  end do sphere

                  end associate
            end if
      end subroutine ugga_spherquad


      subroutine gga_spherquad_mono(rhomatrix, x0, y0, z0, r, rweight, ns, id)
            ! 
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not needed. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham  matrix.
            ! See SPHERQUAD subroutine description.
            !
            double precision, dimension(:, :), intent(in) :: rhomatrix
            double precision, intent(in) :: x0, y0, z0, r
            double precision, intent(in) :: rweight
            integer, intent(in) :: ns
            integer, intent(in) :: id

            double precision :: x, y, z
            integer :: u, l0

            call lebget(l0, ns)
            associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                  lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

            do u = 1, lebnpt(ns)
                  x = x0 + r * lebx(u)
                  y = y0 + r * leby(u)
                  z = z0 + r * lebz(u)
                  weightvec(u, id) = rweight * lebw(u)

                  call gga_vars(rhomatrix, x, y, z, &
                        rhovec(u, id), gradvec(:, u, id), sigmavec(u, id), &
                        orbval(:, u, id), shellidx(:, u, id), nstored(u, id))
            end do

            end associate
      end subroutine gga_spherquad_mono


      subroutine ugga_spherquad_mono(rhomatrixa, rhomatrixb, x0, y0, z0, r, &
            rweight, ns, id)
            !
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not needed. . Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham  matrix.
            ! See SPHERQUAD subroutine description.
            !
            double precision, dimension(:, :), intent(in) :: rhomatrixa
            double precision, dimension(:, :), intent(in) :: rhomatrixb
            double precision, intent(in) :: x0, y0, z0, r
            double precision, intent(in) :: rweight
            integer, intent(in) :: ns
            integer, intent(in) :: id

            double precision :: x, y, z
            integer :: u, l0

            call lebget(l0, ns)
            associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                  lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

            do u = 1, lebnpt(ns)
                  x = x0 + r * lebx(u)
                  y = y0 + r * leby(u)
                  z = z0 + r * lebz(u)
                  weightvec(u, id) = rweight * lebw(u)

                  call ugga_vars(rhomatrixa, rhomatrixb, x, y, z, &
                        urhovec(1, u, id), urhovec(2, u, id), gradavec(:, u, id), &
                        gradbvec(:, u, id), usigmavec(1, u, id), usigmavec(3, u, id), &
                        usigmavec(2, u, id), orbval(:, u, id), shellidx(:, u, id), &
                        nstored(u, id))
            end do

            end associate
      end subroutine ugga_spherquad_mono


      subroutine mgga_spherquad_mono(rhomatrix, x0, y0, z0, r, rweight, &
            ns, id)
            !
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not needed. . Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham  matrix.
            ! See SPHERQUAD subroutine description.
            !
            double precision, dimension(:, :), intent(in) :: rhomatrix
            double precision, intent(in) :: x0, y0, z0, r
            double precision, intent(in) :: rweight
            integer, intent(in) :: ns
            integer, intent(in) :: id

            double precision :: x, y, z
            integer :: u, l0

            call lebget(l0, ns)
            associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                  lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

            do u = 1, lebnpt(ns)
                  x = x0 + r * lebx(u)
                  y = y0 + r * leby(u)
                  z = z0 + r * lebz(u)
                  weightvec(u, id) = rweight * lebw(u)
                  call mgga_vars(rhomatrix, x, y, z, &
                        rhovec(u, id), gradvec(:, u, id), sigmavec(u, id), &
                        laplvec(u, id), tauvec(u, id), orbval(:, u, id), &
                        shellidx(:, u, id), nstored(u, id))
            end do

            end associate
      end subroutine mgga_spherquad_mono


      subroutine umgga_spherquad_mono(rhomatrixa, rhomatrixb, x0, y0, z0, r, &
            rweight, ns, id)
            !
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not required. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham  matrix.
            ! See SPHERQUAD subroutine description.
            !
            double precision, dimension(:, :), intent(in) :: rhomatrixa
            double precision, dimension(:, :), intent(in) :: rhomatrixb
            double precision, intent(in) :: x0, y0, z0, r
            double precision, intent(in) :: rweight
            integer, intent(in) :: ns
            integer, intent(in) :: id

            double precision :: x, y, z
            integer :: u, l0

            call lebget(l0, ns)
            associate(lebx => GRD_LEBX(l0:), leby => GRD_LEBY(l0:), &
                  lebz => GRD_LEBZ(l0:), lebw => GRD_LEBW(l0:))

            do u = 1, lebnpt(ns)
                  x = x0 + r * lebx(u)
                  y = y0 + r * leby(u)
                  z = z0 + r * lebz(u)
                  weightvec(u, id) = rweight * lebw(u)

                  call umgga_vars(rhomatrixa, rhomatrixb, x, y, z, &
                        urhovec(1, u, id), urhovec(2, u, id), gradavec(:, u, id), &
                        gradbvec(:, u, id), usigmavec(1, u, id), usigmavec(3, u, id), &
                        usigmavec(2, u, id), ulaplvec(1, u, id), ulaplvec(2, u, id), &
                        utauvec(1, u, id), utauvec(2, u, id), orbval(:, u, id), &
                        shellidx(:, u, id), nstored(u, id))
            end do

            end associate
      end subroutine umgga_spherquad_mono


      subroutine gga_vxc(exc, nelectron, divergence, nbatch, deltak, id)

            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nelectron
            double precision, intent(inout)                  :: divergence
            integer, intent(in)                              :: nbatch
            integer, intent(in)                              :: deltak
            integer, intent(in)                              :: id

            integer :: batchpoint, n0
            double precision :: rho, eps, vrho, vsigma, weight
            double precision, dimension(3) :: grad

            do batchpoint = 1, nbatch
                  n0 = nstored(batchpoint, id)
                  rho = rhovec(batchpoint, id)
                  grad = gradvec(:, batchpoint, id)
                  eps = epsvec(batchpoint, id)
                  vrho = vrhovec(batchpoint, id)
                  vsigma = vsigmavec(batchpoint, id)
                  weight = weightvec(batchpoint, id)

                  call gga_kscontrib(PRVMAT(:, :, id), exc, &
                        nelectron, divergence, shellidx(:, batchpoint, id), &
                        orbval(:, batchpoint, id), n0, rho, grad, eps, vrho, &
                        vsigma, weight, deltak)
            end do
      end subroutine gga_vxc


      subroutine ugga_vxc(exc, nelectron, divergence, nbatch, deltak, id)
            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nelectron
            double precision, intent(inout)                  :: divergence
            integer, intent(in)                              :: nbatch
            integer, intent(in)                              :: deltak
            integer, intent(in)                              :: id

            integer :: batchpoint, n0
            double precision :: rhoa, rhob, eps
            double precision :: vrhoa, vrhob, vsigma_aa
            double precision :: vsigma_bb, vsigma_ab, weight
            double precision, dimension(3) :: grada, gradb

            do batchpoint = 1, nbatch
                  n0 = nstored(batchpoint, id)
                  rhoa = urhovec(1, batchpoint, id)
                  rhob = urhovec(2, batchpoint, id)
                  grada = gradavec(:, batchpoint, id)
                  gradb = gradbvec(:, batchpoint, id)
                  eps = epsvec(batchpoint, id)
                  vrhoa = uvrhovec(1, batchpoint, id)
                  vrhob = uvrhovec(2, batchpoint, id)
                  vsigma_aa = uvsigmavec(1, batchpoint, id)
                  vsigma_ab = uvsigmavec(2, batchpoint, id)
                  vsigma_bb = uvsigmavec(3, batchpoint, id)
                  weight = weightvec(batchpoint, id)

                  call ugga_kscontrib(PRVMAT(:, :, PRVIDA(id)), PRVMAT(:, :, PRVIDB(id)), &
                        exc, nelectron, divergence, shellidx(:, batchpoint, id), &
                        orbval(:, batchpoint, id), n0, rhoa, rhob, &
                        grada, gradb, eps, vrhoa, vrhob, vsigma_aa, &
                        vsigma_bb, vsigma_ab, weight, deltak)
            end do
      end subroutine ugga_vxc


      subroutine mgga_vxc(exc, nelectron, divergence, kinint, laplint, nbatch, deltak, id)

            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nelectron
            double precision, intent(inout)                  :: divergence
            double precision, intent(inout)                  :: kinint
            double precision, intent(inout)                  :: laplint
            integer, intent(in)                              :: nbatch
            integer, intent(in)                              :: deltak
            integer, intent(in)                              :: id

            integer :: batchpoint, n0
            double precision :: rho, lapl, tau, eps, vrho, vsigma, weight
            double precision :: vlapl, vtau
            double precision, dimension(3) :: grad

            do batchpoint = 1, nbatch
                  n0 = nstored(batchpoint, id)
                  rho = rhovec(batchpoint, id)
                  grad = gradvec(:, batchpoint, id)
                  lapl = laplvec(batchpoint, id)
                  tau = tauvec(batchpoint, id)
                  eps = epsvec(batchpoint, id)
                  vrho = vrhovec(batchpoint, id)
                  vsigma = vsigmavec(batchpoint, id)
                  vlapl = vlaplvec(batchpoint, id)
                  vtau = vtauvec(batchpoint, id)
                  weight = weightvec(batchpoint, id)

                  call mgga_kscontrib(PRVMAT(:, :, id), exc, &
                        nelectron, divergence, kinint, laplint, shellidx(:, batchpoint, id), &
                        orbval(:, batchpoint, id), n0, rho, grad, lapl, tau, eps, vrho, &
                        vsigma, vlapl, vtau, weight, deltak)
            end do
      end subroutine mgga_vxc


      subroutine umgga_vxc(exc, nelectron, divergence, kinint, laplint, &
            nbatch, deltak, id)
            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nelectron
            double precision, intent(inout)                  :: divergence
            double precision, intent(inout)                  :: kinint
            double precision, intent(inout)                  :: laplint
            integer, intent(in)                              :: nbatch
            integer, intent(in)                              :: deltak
            integer, intent(in)                              :: id

            integer :: batchpoint, n0
            double precision :: rhoa, rhob, eps, lapla, laplb, taua, taub
            double precision :: vrhoa, vrhob, vsigma_aa
            double precision :: vsigma_bb, vsigma_ab, weight
            double precision :: vlapla, vlaplb, vtaua, vtaub
            double precision, dimension(3) :: grada, gradb

            do batchpoint = 1, nbatch
                  n0 = nstored(batchpoint, id)
                  rhoa = urhovec(1, batchpoint, id)
                  rhob = urhovec(2, batchpoint, id)
                  grada = gradavec(:, batchpoint, id)
                  gradb = gradbvec(:, batchpoint, id)
                  lapla = ulaplvec(1, batchpoint, id)
                  laplb = ulaplvec(2, batchpoint, id)
                  taua = utauvec(1, batchpoint, id)
                  taub = utauvec(2, batchpoint, id)
                  eps = epsvec(batchpoint, id)
                  vrhoa = uvrhovec(1, batchpoint, id)
                  vrhob = uvrhovec(2, batchpoint, id)
                  vsigma_aa = uvsigmavec(1, batchpoint, id)
                  vsigma_ab = uvsigmavec(2, batchpoint, id)
                  vsigma_bb = uvsigmavec(3, batchpoint, id)
                  vlapla = uvlaplvec(1, batchpoint, id)
                  vlaplb = uvlaplvec(2, batchpoint, id)
                  vtaua = uvtauvec(1, batchpoint, id)
                  vtaub = uvtauvec(2, batchpoint, id)
                  weight = weightvec(batchpoint, id)

                  call umgga_kscontrib(PRVMAT(:, :, PRVIDA(id)), PRVMAT(:, :, PRVIDB(id)), &
                        exc, nelectron, divergence, kinint, laplint, &
                        shellidx(:, batchpoint, id), orbval(:, batchpoint, id), n0, &
                        rhoa, rhob, grada, gradb, lapla, laplb, taua, taub, eps, &
                        vrhoa, vrhob, vsigma_aa, vsigma_bb, vsigma_ab, vlapla, vlaplb, &
                        vtaua, vtaub, weight, deltak)
            end do
      end subroutine umgga_vxc

      
      subroutine gridint(xcdef, rhomatrixa, rhomatrixb, ksmatrixa, ksmatrixb, &
            exc, nelectron, divergence, kinint, laplint, aux, chunk)

            type(txcdef), intent(in)                           :: xcdef
            double precision, dimension(:, :), intent(in)      :: rhomatrixa
            double precision, dimension(:, :), intent(in)      :: rhomatrixb
            double precision, dimension(:, :), intent(inout)   :: ksmatrixa
            double precision, dimension(:, :), intent(inout)   :: ksmatrixb
            double precision, intent(inout)                    :: exc
            double precision, intent(inout)                    :: nelectron, divergence
            double precision, intent(inout)                    :: kinint, laplint
            double precision, dimension(:), intent(inout)      :: aux
            integer, dimension(2), optional, intent(in)        :: chunk

            logical :: uncomp
            double precision, dimension(:), allocatable :: taux
            integer :: nauxint
            double precision :: x0, y0, z0
            double precision :: rweight, r
            integer :: k0
            integer :: k, l
            integer :: znum
            integer :: nr, ns, nbatch
            integer :: id
            integer :: s0, s1
            integer :: point0
            integer :: deltak
            logical :: mgga_ingredients
            integer :: aux_id, xc_id
            logical :: aux_mgga, xc_mgga
            logical :: exchange_correlation, auxiliary_integral
            !
            ! Identifier of the auxiliary (non-XC) integral
            ! on the molecular grid
            !
            aux_id = aux_get_id(xcdef)
            aux_mgga = aux_ismgga(xcdef)
            exchange_correlation = xcf_isgridxc(xcdef)
            auxiliary_integral = (aux_id .ne. AUX_NONE)
            if (auxiliary_integral) then
                  nauxint = aux_arraydim(aux_id, xcf_isuncomp(xcdef))
                  allocate(taux(nauxint))
                  taux = ZERO
            else
                  nauxint = 0
                  allocate(taux(0))
            end if
            !
            ! Identifier of the exchange-correlation functional
            !
            xc_id = xcf_get_id(xcdef)
            xc_mgga = xcf_ismgga(xcdef)
            if (xcf_isuncomp(xcdef)) then
                  uncomp = .true.
            else
                  uncomp = .false.
            end if

            if (present(chunk)) then
                  s0 = chunk(1)
                  s1 = chunk(2)
            else
                  s0 = 1
                  s1 = NATOM
            end if
            !
            ! Check if exchange-correlation functional or
            ! auxiliary integral are of meta-GGA type
            !
            if (xc_mgga .or. aux_mgga) then
                  deltak = MGGA_DELTAK
                  mgga_ingredients = .true.
            else
                  deltak = GGA_DELTAK
                  mgga_ingredients = .false.
            end if
            !
            ! Number of radial grid points per single atom
            ! (the same number for all atoms)
            !
            nr = grid_nradial()
            !
            ! Reset matrices used to gather contributions
            ! to the KS matrix computed by individual threads.
            !
            call thr_reset()
            !$omp parallel &
            !$omp default(shared) &
            !$omp private(k, l, r, rweight, ns, nbatch, id, point0) &
            !$omp private(x0, y0, z0, znum, k0) &
            !$omp reduction(+:exc, nelectron, divergence, kinint, laplint) &
            !$omp reduction(+:taux)            
            id = omp_get_thread_num() + 1
            !$omp do schedule(guided) collapse(2)
            atomloop: do k = s0, s1
                  radialquad1: do l = 1, nr
                        x0 = atomr(1, k)
                        y0 = atomr(2, k)
                        z0 = atomr(3, k)
                        !
                        ! Get points and weights of radial quadrature
                        !
                        znum = inuclz(k)
                        call rgridget(k0, znum)
                        r = GRD_RGRIDR(k0+l-1)
                        rweight = GRD_RGRIDW(k0+l-1)
                        ns = GRD_RGRIDNS(k0+l-1)
                        nbatch = lebnpt(ns)
                        if (uncomp) then
                              !
                              ! Spin-uncompensated electronic density
                              !
                              if (NATOM > 1) then
                                    if (.not. mgga_ingredients) then
                                          !
                                          ! Generate ingredients for the XC functional: compute density,
                                          ! its gradient, the laplacian, the density of the kinetic energy,
                                          ! and values of the AOs at each grid point belonging to the sphere
                                          ! centered at X0, Y0, Z0. Compute quadrature weights for each point
                                          ! in the batch.
                                          !
                                          call ugga_spherquad(rhomatrixa, rhomatrixb, k, &
                                                x0, y0, z0, r, rweight, ns, id)
                                    else
                                          call umgga_spherquad(rhomatrixa, rhomatrixb, k, &
                                                x0, y0, z0, r, rweight, ns, id)
                                    end if
                              else
                                    !
                                    ! Code for computing integrals in one-atom systems.
                                    ! Weights for multi-atom integrations need not be computed here.
                                    !
                                    if (.not. mgga_ingredients) then
                                          call ugga_spherquad_mono(rhomatrixa, rhomatrixb, &
                                                x0, y0, z0, r, rweight, ns, id)
                                    else
                                          call umgga_spherquad_mono(rhomatrixa, rhomatrixb, &
                                                x0, y0, z0, r, rweight, ns, id)
                                    end if
                              end if
                              !
                              ! Compute contribution to exchange-correlation energy
                              ! and KS matrix
                              !
                              if (exchange_correlation) then
                                    if (.not. xc_mgga) then
                                          !
                                          ! Compute values and/or derivatives of the XC functional for each point in the batch.
                                          !
                                          call u_gga_batch(xcdef, nbatch, id)
                                          !
                                          ! Update XC matrix with the values computed at each point in the batch.
                                          !
                                          call ugga_vxc(exc, nelectron, divergence, nbatch, deltak, id)
                                    else
                                          call u_mgga_batch(xcdef, nbatch, id)
                                          call umgga_vxc(exc, nelectron, divergence, kinint, laplint, nbatch, deltak, id)
                                    end if
                              end if
                              ! !
                              ! ! Additional, non-XC integrals on the molecular grid. 
                              ! !
                              ! if (auxiliary_integral) then
                              !       if (aux_mgga) then
                              !             !
                              !             ! Generic integral of a function defined on the molecular grid.
                              !             ! Inputs to this function are the same as in a meta-GGA functional.
                              !             ! This is not exchange-correlation energy.
                              !             !
                              !             call lebget(point0, ns)
                              !             call umgga_auxint(taux, aux_id, x0, y0, z0, r, GRD_LEBX(point0:), &
                              !                   GRD_LEBY(point0:), GRD_LEBZ(point0:), nbatch, weightvec(:, id), &
                              !                   urhovec(:, :, id), usigmavec(:, :, id), ulaplvec(:, :, id), &
                              !                   utauvec(:, :, id), orbval(:, :, id), shellidx(:, :, id), &
                              !                   nstored(:, id), deltak)
                              !       else
                              !             call lebget(point0, ns)
                              !             call ugga_auxint(taux, aux_id, x0, y0, z0, r, GRD_LEBX(point0:), &
                              !                   GRD_LEBY(point0:), GRD_LEBZ(point0:), nbatch, weightvec(:, id), &
                              !                   urhovec(:, :, id), usigmavec(:, :, id), orbval(:, :, id), &
                              !                   shellidx(:, :, id), nstored(:, id), deltak)
                              !       end if
                              ! end if
                        else
                              !
                              ! Spin-compensated electronic density
                              !
                              if (NATOM > 1) then
                                    if (.not. mgga_ingredients) then
                                          call gga_spherquad(rhomatrixa, k, &
                                                x0, y0, z0, r, rweight, ns, id)
                                    else
                                          call mgga_spherquad(rhomatrixa, k, &
                                                x0, y0, z0, r, rweight, ns, id)
                                    end if
                              else
                                    if (.not. mgga_ingredients) then
                                          call gga_spherquad_mono(rhomatrixa, x0, y0, z0, &
                                                r, rweight, ns, id)
                                    else
                                          call mgga_spherquad_mono(rhomatrixa, x0, y0, z0, &
                                                r, rweight, ns, id)
                                    end if
                              end if

                              if (exchange_correlation) then
                                    if (.not. xc_mgga) then
                                          call gga_batch(xcdef, nbatch, id)
                                          call gga_vxc(exc, nelectron, divergence, nbatch, deltak, id)
                                    else
                                          call mgga_batch(xcdef, nbatch, id)
                                          call mgga_vxc(exc, nelectron, divergence, kinint, &
                                                laplint, nbatch, deltak, id)
                                    end if
                              end if

                              ! if (auxiliary_integral) then
                              !       if (aux_mgga) then
                              !             call lebget(point0, ns)
                              !             call mgga_auxint(taux, aux_id, x0, y0, z0, r, GRD_LEBX(point0:), &
                              !                   GRD_LEBY(point0:), GRD_LEBZ(point0:), nbatch, weightvec(:, id), &
                              !                   rhovec(:, id), sigmavec(:, id), laplvec(:, id), tauvec(:, id), &
                              !                   orbval(:, :, id), shellidx(:, :, id), nstored(:, id), deltak)
                              !       else
                              !             call lebget(point0, ns)
                              !             call gga_auxint(taux, aux_id, x0, y0, z0, r, GRD_LEBX(point0:), &
                              !                   GRD_LEBY(point0:), GRD_LEBZ(point0:), nbatch, weightvec(:, id), &
                              !                   rhovec(:, id), sigmavec(:, id), orbval(:, :, id), &
                              !                   shellidx(:, :, id), nstored(:, id), deltak)
                              !       end if
                              ! end if
                        end if
                  end do radialquad1
            end do atomloop
            !$omp end do nowait
            !$omp end parallel
            ! -----------------------------------------------
            ! Gather contributions to the KS matrix computed
            ! by individual threads
            !
            if (exchange_correlation) then
                  if (uncomp) then
                        call thr_reducea(ksmatrixa)
                        call thr_reduceb(ksmatrixb)
                  else
                        call thr_reducea(ksmatrixa)
                  end if
            end if
            if (auxiliary_integral) then
                  aux(1:nauxint) = aux(1:nauxint) + taux
            end if
            deallocate(taux)
      end subroutine gridint


      subroutine xcorr(xcdef, ksmatrixa, ksmatrixb, rhoa, rhob, exc, &
            nelectron, divergence, kinetic, laplacian, auxint, aux_matrix_input, &
            chunk)

            type(txcdef), intent(in)                           :: xcdef
            double precision, dimension(:, :), intent(inout)   :: ksmatrixa
            double precision, dimension(:, :), intent(inout)   :: ksmatrixb
            double precision, dimension(:, :), intent(in)      :: rhoa
            double precision, dimension(:, :), intent(in)      :: rhob
            double precision, intent(out)                      :: exc
            double precision, intent(out)                      :: nelectron
            double precision, intent(out)                      :: divergence
            double precision, intent(out)                      :: kinetic
            double precision, intent(out)                      :: laplacian
            double precision, dimension(:), intent(out)        :: auxint
            real(F64), dimension(:, :), contiguous, intent(in) :: aux_matrix_input
            integer, dimension(2), optional, intent(in)        :: chunk

            integer, dimension(2) :: c
            integer :: aux_id, xcf_id
            type(tclock) :: t_grid

            call clock_start(t_grid)
            !
            ! Determine chunk of work to be done
            !
            if (present(chunk)) then
                  c = chunk
            else
                  c = [1, NATOM]
            end if

            aux_id = aux_get_id(xcdef)
            xcf_id = xcf_get_id(xcdef)

            call setxcfunc(xcdef)
            call aux_setauxint(aux_id, xcf_isuncomp(xcdef), rhoa, rhob, aux_matrix_input)

            exc        = ZERO
            nelectron  = ZERO
            divergence = ZERO
            kinetic    = ZERO
            laplacian  = ZERO
            if (aux_id .ne. AUX_NONE) then
                  auxint = ZERO
            end if

            call grid_set(GRD_FINE, .true.)
            call gridint(xcdef, rhoa, rhob, ksmatrixa, ksmatrixb, exc, &
                  nelectron, divergence, kinetic, laplacian, auxint, &
                  chunk=c)

            TIMINGS(TIME_GRID) = TIMINGS(TIME_GRID) + clock_readwall(t_grid)
      end subroutine xcorr


      subroutine becke(i, rig, gx, gy, gz, w)
            integer, intent(in) :: i
            double precision, intent(in) :: rig
            double precision, intent(in) :: gx, gy, gz
            double precision, intent(out) :: w

            double precision :: rik, rkg
            double precision :: mu, s
            double precision, dimension(3) :: rk
            integer :: k, idk
            double precision :: r
            double precision, parameter :: const1 = two / (one - becka)

            r = const1 * rig
            w = one
            !
            ! Loop over all grid centers
            !
            atomloop: do k = 2, NATOM
                  rik = grd_dist(k, i)
                  idk = grd_idist(k, i)
                  rk = atomr(:, idk)
                  rkg = sqrt((gx - rk(1))**2 + (gy - rk(2))**2 + (gz - rk(3))**2)
                  mu = (rig - rkg) / (becka * rik)

                  if (mu .le. -one) then
                        if (rik .ge. r) then
                              exit atomloop
                        else
                              cycle atomloop
                        end if
                  else if (mu .ge. one) then
                        w = zero
                        exit atomloop
                  else
                        call step_function(mu, s)
                        w = w * s
                  end if
            end do atomloop
      end subroutine becke


      pure subroutine step_function(mu, s)
            ! -------------------------------------------------------------------------------
            ! Compute the step function (Eq. 8 in Ref. 1) for a given value of mu_ij.
            ! The polynomial employed to represent g(mu_ij) is defined in Eq. 14 of Ref. 1.
            ! It is assumed that the numerical constant a is already included in mu.
            ! This subroutine should only be use for for -1 <= mu <= 1.
            !
            ! The total step function of Stratmann et al. is defined as
            ! s(mu) = 1                     for mu <= -1
            ! s(mu) = step_function(mu, s)  for -1 < mu < 1
            ! s(mu) = 0                     for 1 >= mu
            ! (See Fig. 1 in Ref. 1)
            !
            ! -------------------------------------------------------------------------------
            ! Mu 
            !      The input for the step function, mu_ij defined in Eq. 4 scaled by the
            !      numerical constant a.
            ! S
            !      Value of the step function (Eq. 8)
            ! -------------------------------------------------------------------------------
            ! 1. Stratmann, R.E., Scuseria, G.E., Frisch, M.J., Achieving linear scaling
            !    in exchange-correlation density functional quadratures,
            !    Chem. Phys. Lett. 257, 213 (1996); doi: 10.1016/0009-2614(96)00600-8
            !
            real(F64), intent(in) :: mu
            real(F64), intent(out) :: s

            real(F64), parameter :: c1 = 35.0_F64 / 32.0_F64
            real(F64), parameter :: c3 = -35.0_F64 / 32.0_F64
            real(F64), parameter :: c5 = 21.0_F64 / 32.0_F64
            real(F64), parameter :: c7 = -5.0_F64 / 32.0_F64

            s = ONE/TWO - (c1 * mu + c3 * mu**3 + c5 * mu**5 + c7 * mu**7)
      end subroutine step_function
end module xcfunc
