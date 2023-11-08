module KohnSham
      use gparam
      use math_constants
      use arithmetic
      use grid
      use basis_sets
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
      use chai2013_x_asymp
      use slater_x_potential
      use scf_definitions
      use KohnShamGGA
      use KohnShamMGGA
      !$ use omp_lib

      implicit none

contains

      subroutine ks_Y_GGA_Sphere(YEps, YRho, YSig, XRho, XSig, XCDef, NSpher)
            real(F64), dimension(:), intent(out) :: YEps
            real(F64), dimension(:), intent(out) :: YRho
            real(F64), dimension(:), intent(out) :: YSig
            real(F64), dimension(:), intent(in)  :: XRho
            real(F64), dimension(:), intent(in)  :: XSig
            type(TXCDef), intent(in)             :: XCDef
            integer, intent(in)                  :: NSpher
            
            select case (xcf_get_id(XCDef))
            case (XCF_XC_HJS_PBE, XCF_XC_LRCWPBEH)
                  call hjs_pbe_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_SH_HJS_PBE)
                  call hjs_sh_pbe_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_HJS_PBESOL)
                  call hjs_pbesol_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_B88)
                  call b88_xonly(YEps, YRho, YSig, &
                        XRho, XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_BLYP)
                  call blyp_xc(YEps, YRho, YSig, &
                        XRho, XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_B88OPT_LYP)
                  call b88opt_lyp_xc(YEps, YRho, YSig, &
                        XRho, XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_HJS_BLYP)
                  call hjs_blyp_xc(YEps, YRho, YSig, &
                        XRho, XSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_HJS_B88)
                  call hjs_b88_xonly(YEps, YRho, YSig, &
                        XRho, XSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_HJS_PBE)
                  call hjs_pbe_xonly(YEps, YRho, YSig, &
                        XRho, XSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_PBE, XCF_XC_PBE0)
                  call pbe_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_X_PBE)
                  call pbe_xonly(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_PBEsol)
                  call pbesol_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_PW86PBE)
                  call pw86pbe_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_RPW86PBE)
                  call rpw86pbe_xc(YEps, YRho, YSig, XRho, &
                        XSig, NSpher, xcf_get_exx(XCDef))
            end select
      end subroutine ks_Y_GGA_Sphere


      subroutine ks_YU_GGA_Sphere(YEps, YURho, YUSig, XURho, XUSig, XCDef, NSpher)
            real(F64), dimension(:), intent(out)    :: YEps
            real(F64), dimension(:, :), intent(out) :: YURho
            real(F64), dimension(:, :), intent(out) :: YUSig
            real(F64), dimension(:, :), intent(in)  :: XURho
            real(F64), dimension(:, :), intent(in)  :: XUSig
            type(TXCDef), intent(in)                :: XCDef
            integer, intent(in)                     :: NSpher
            
            select case (xcf_get_id(XCDef))
            case (XCF_XC_HJS_PBE, XCF_XC_LRCWPBEH)
                  call u_hjs_pbe_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_SH_HJS_PBE)
                  call u_hjs_sh_pbe_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))
              
            case (XCF_XC_HJS_PBESOL)
                  call u_hjs_pbesol_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_B88)
                  call u_b88_xonly(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_BLYP)
                  call u_blyp_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_B88OPT_LYP)
                  call u_b88opt_lyp_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_HJS_BLYP)
                  call u_hjs_blyp_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_HJS_B88)
                  call u_hjs_b88_xonly(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_HJS_PBE)
                  call u_hjs_pbe_xonly(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_PBE, XCF_XC_PBE0)
                  call u_pbe_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_X_PBE)
                  call u_pbe_xonly(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_PBEsol)
                  call u_pbesol_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_PW86PBE)
                  call u_pw86pbe_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))

            case (XCF_XC_RPW86PBE)
                  call u_rpw86pbe_xc(YEps, YURho, YUSig, XURho, &
                        XUSig, NSpher, xcf_get_exx(XCDef))
            end select
      end subroutine ks_YU_GGA_Sphere


      subroutine ks_Y_MGGA_Sphere(YEps, YRho, YSig, YLap, YTau, XRho, XSig, XLap, XTau, XCDef, NSpher)
            real(F64), dimension(:), intent(out) :: YEps
            real(F64), dimension(:), intent(out) :: YRho
            real(F64), dimension(:), intent(out) :: YSig
            real(F64), dimension(:), intent(out) :: YLap
            real(F64), dimension(:), intent(out) :: YTau
            real(F64), dimension(:), intent(in)  :: XRho
            real(F64), dimension(:), intent(in)  :: XSig
            real(F64), dimension(:), intent(in)  :: XLap
            real(F64), dimension(:), intent(in)  :: XTau
            type(TXCDef), intent(in)             :: XCDef
            integer, intent(in)                  :: NSpher

            select case(xcf_get_id(XCDef))
            case (XCF_XC_MCS)
                  call mcs_xc(YEps, YRho, YSig, YTau, XRho, &
                        XSig, XTau, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))
                  YLap = ZERO

            case (XCF_XC_MCSH)
                  call mcsh_xc(YEps, YRho, YSig, YTau, XRho, &
                        XSig, XTau, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))
                  YLap = ZERO

            case (XCF_XC_MVS, XCF_XC_MVSh)
                  call mvs_xc(YEps, YRho, YSig, YTau, &
                        XRho, XSig, XTau, NSpher, xcf_get_exx(XCDef))
                  YLap = ZERO

            case (XCF_XC_MCSv2)
                  call mcsv2_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_MCSv3)
                  call mcsv3_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_BLYP)
                  call ec_blyp_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_PBE)
                  call ec_pbe_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_PBE_TPSS)
                  call ec_pbetpss_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_LC_SCAN)
                  call ec_scan_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_SH_SCAN)
                  call ec_sh_scan_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))
                  
            case (XCF_XC_SH_PBE)
                  call ec_sh_pbe_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_LC_LDA)
                  call ec_lda_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_LC_LDATPSS)
                  call ec_ldatpss_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_HJS_PBETPSS)
                  call hjs_pbetpss_xc(YEps, YRho, YSig, YTau, XRho, &
                        XSig, XTau, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))
                  YLap = ZERO

            case (XCF_XC_EC_PBEsol)
                  call ec_pbesol_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_EC_B88)
                  call ec_b88_xonly(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_EC_PBE)
                  call ec_pbe_xonly(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_EC_TPSS)
                  call ec_tpss_xonly(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_TPSS)
                  call ec_tpss_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_revTPSS)
                  call ec_revtpss_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_MVS)
                  call ec_mvs_xc(YEps, YRho, YSig, YLap, YTau, &
                        XRho, XSig, XLap, XTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_TPSS)
                  call tpss_xc(YEps, YRho, YSig, YTau, &
                        XRho, XSig, XTau, NSpher, xcf_get_exx(XCDef))
                  YLap = ZERO

            case (XCF_XC_SCAN, XCF_XC_SCAN0)
                  call scan_xc(YEps, YRho, YSig, YTau, &
                        XRho, XSig, XTau, NSpher, xcf_get_exx(XCDef))
                  YLap = ZERO

            case (XCF_XC_revTPSS)
                  call revtpss_xc(YEps, YRho, YSig, YTau, &
                        XRho, XSig, XTau, NSpher, xcf_get_exx(XCDef))
                  YLap = ZERO
            end select
      end subroutine ks_Y_MGGA_Sphere


      subroutine ks_YU_MGGA_Sphere(YEps, YURho, YUSig, YULap, YUTau, XURho, XUSig, XULap, XUTau, XCDef, NSpher)
            real(F64), dimension(:), intent(out)    :: YEps
            real(F64), dimension(:, :), intent(out) :: YURho
            real(F64), dimension(:, :), intent(out) :: YUSig
            real(F64), dimension(:, :), intent(out) :: YULap
            real(F64), dimension(:, :), intent(out) :: YUTau
            real(F64), dimension(:, :), intent(in)  :: XURho
            real(F64), dimension(:, :), intent(in)  :: XUSig
            real(F64), dimension(:, :), intent(in)  :: XULap
            real(F64), dimension(:, :), intent(in)  :: XUTau
            type(TXCDef), intent(in)                :: XCDef
            integer, intent(in)                     :: NSpher
            
            select case (xcf_get_id(XCDef))
            case (XCF_XC_MCS)
                  call u_mcs_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_srexx(XCDef), &
                        xcf_get_omega(XCDef))
                  YULap = ZERO

            case (XCF_XC_MCSH)
                  call u_mcsh_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_srexx(XCDef), &
                        xcf_get_omega(XCDef))
                  YULap = ZERO

            case (XCF_XC_MVS, XCF_XC_MVSh)
                  call u_mvs_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_exx(XCDef))
                  YULap = ZERO

            case (XCF_XC_MCSv2)
                  call u_mcsv2_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, &
                        XUTau, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_MCSv3)
                  call u_mcsv3_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, &
                        XUTau, NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_BLYP)
                  call u_ec_blyp_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_PBE)
                  call u_ec_pbe_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_PBE_TPSS)
                  call u_ec_pbetpss_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_LC_SCAN)
                  call u_ec_scan_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_SH_SCAN)
                  call u_ec_sh_scan_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_SH_PBE)
                  call u_ec_sh_pbe_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_LC_LDA)
                  call u_ec_lda_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_LC_LDATPSS)
                  call u_ec_ldatpss_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_HJS_PBETPSS)
                  call u_hjs_pbetpss_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_srexx(XCDef), &
                        xcf_get_omega(XCDef))
                  YULap = ZERO

            case (XCF_XC_EC_PBEsol)
                  call u_ec_pbesol_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_EC_B88)
                  call u_ec_b88_xonly(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_X_EC_PBE)
                  call u_ec_pbe_xonly(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))
                  
            case (XCF_X_EC_TPSS)
                  call u_ec_tpss_xonly(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_TPSS)
                  call u_ec_tpss_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_revTPSS)
                  call u_ec_revtpss_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_EC_MVS)
                  call u_ec_mvs_xc(YEps, YURho, YUSig, YULap, &
                        YUTau, XURho, XUSig, XULap, XUTau, &
                        NSpher, xcf_get_srexx(XCDef), xcf_get_omega(XCDef))

            case (XCF_XC_TPSS)
                  call u_tpss_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_exx(XCDef))
                  YULap = ZERO

            case (XCF_XC_SCAN, XCF_XC_SCAN0)
                  call u_scan_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_exx(XCDef))
                  YULap = ZERO

            case (XCF_XC_revTPSS)
                  call u_revtpss_xc(YEps, YURho, YUSig, YUTau, &
                        XURho, XUSig, XUTau, NSpher, xcf_get_exx(XCDef))
                  YULap = ZERO
            end select
      end subroutine ks_YU_MGGA_Sphere


      subroutine ks_X_MGGA_Sphere(XRho, XGrad, XSig, XLap, XTau, XOrb, XShells, XNShells, XWeights, &
            Rho, Center, X0, Y0, Z0, R, RWeight, UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, &
            NSpher, AOBasis, System)
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
            ! X0,Y0,Z0  - Coordinates of central atom
            ! R         - Radial quadrature point
            ! RWEIGHT   - Weight of radial quadrature point
            !             including (-1, 1) -> (0, \inf)
            !             transformation Jacobian and R**2.
            !
            real(F64), dimension(:), intent(out)      :: XRho
            real(F64), dimension(:, :), intent(out)   :: XGrad
            real(F64), dimension(:), intent(out)      :: XSig
            real(F64), dimension(:), intent(out)      :: XLap
            real(F64), dimension(:), intent(out)      :: XTau
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            integer, intent(in)                       :: Center
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System
            
            real(F64) :: x, y, z
            real(F64) :: ar
            real(F64) :: w1, w2, wm, s
            real(F64) :: rmk, rmg, mu
            real(F64), dimension(3) :: rm
            integer :: u
            integer :: idm, m
            integer :: wcntrb

            real(F64) :: const1 = TWO / (ONE - becka)

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart, &
                  SortedDistances => System%SortedDistances, &
                  SortedDistancesIdx => System%SortedDistancesIdx &
                  )
                  !
                  ! Check if Becke's weight is equal to one.
                  ! Inequality 15 in [1]
                  !
                  ar = const1 * r
                  if (ar <= SortedDistances(2, Center)) then
                        do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              XWeights(u) = RWeight * SphereWeights(u)
                              !
                              ! Generate next point in batch
                              !
                              call ksmgga_X(Rho(:, :, 1), x, y, z, &
                                    XRho(u), XGrad(:, u), XSig(u), &
                                    XLap(u), XTau(u), XOrb(:, u), &
                                    XShells(:, u), XNShells(u), &
                                    NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                    AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                    ShellParamsIdx, ShellMomentum, NPrimitives, &
                                    CntrCoeffs, Exponents, NormFactors, &
                                    AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
                        end do
                  else
                        wcntrb = 1
                        do u = 3, AOBasis%NAtoms
                              if (ar <= SortedDistances(u, Center)) then
                                    exit
                              else
                                    wcntrb = wcntrb + 1
                              end if
                        end do
                        sphere: do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              rmk = SortedDistances(2, Center)
                              idm = SortedDistancesIdx(2, Center)
                              rm = AtomCoords(:, idm)
                              rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                              mu = (r - rmg) / (becka * rmk)
                              if (mu .lt. ONE) then
                                    if (mu <= -ONE) then
                                          w1 = ONE
                                          w2 = ZERO
                                    else
                                          call ks_step_function(mu, w1)
                                          call ks_becke(idm, rmg, x, y, z, w2, &
                                                AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                    end if
                                    weights: do m = 3, wcntrb + 1
                                          rmk = SortedDistances(m, Center)
                                          idm = SortedDistancesIdx(m, Center)
                                          rm = AtomCoords(:, idm)
                                          rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                          mu = (r - rmg) / (becka * rmk)
                                          if (mu >= ONE) then
                                                !
                                                ! The step function employed in the weight factor is exactly
                                                ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                                ! must be defined as zero to avoid processing NaNs and/or unphysical
                                                ! values which are present due to the lack of initialization.
                                                !
                                                XRho(u) = ZERO
                                                XGrad(:, u) = ZERO
                                                XSig(u) = ZERO
                                                XLap(u) = ZERO
                                                XTau(u) = ZERO
                                                XWeights(u) = ZERO
                                                XNShells(u) = 0
                                                cycle sphere
                                          else if (mu <= -ONE) then
                                                cycle weights
                                          else
                                                call ks_step_function(mu, s)
                                                w1 = w1 * s
                                                call ks_becke(idm, rmg, x, y, z, wm, &
                                                      AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                                w2 = w2 + wm
                                          end if
                                    end do weights
                                    w2 = w2 + w1
                                    XWeights(u) = RWeight * SphereWeights(u) * w1 / w2
                                    call ksmgga_X(Rho(:, :, 1), x, y, z, &
                                          XRho(u), XGrad(:, u), XSig(u), &
                                          XLap(u), XTau(u), XOrb(:, u), &
                                          XShells(:, u), XNShells(u), &
                                          NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                          AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                          ShellParamsIdx, ShellMomentum, NPrimitives, &
                                          CntrCoeffs, Exponents, NormFactors, &
                                          AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
                              else
                                    XRho(u) = ZERO
                                    XGrad(:, u) = ZERO
                                    XSig(u) = ZERO
                                    XLap(u) = ZERO
                                    XTau(u) = ZERO
                                    XWeights(u) = ZERO
                                    XNShells(u) = 0
                              end if
                        end do sphere
                  end if
            end associate
      end subroutine ks_X_MGGA_Sphere


      subroutine ks_XU_MGGA_Sphere(XURho, XGradA, XGradB, XUSig, XULap, XUTau, &
            XOrb, XShells, XNShells, XWeights, Rho, Center, X0, Y0, Z0, R, RWeight, &
            UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, NSpher, AOBasis, System)
            
            real(F64), dimension(:, :), intent(out)   :: XURho
            real(F64), dimension(:, :), intent(out)   :: XGradA
            real(F64), dimension(:, :), intent(out)   :: XGradB
            real(F64), dimension(:, :), intent(out)   :: XUSig
            real(F64), dimension(:, :), intent(out)   :: XULap
            real(F64), dimension(:, :), intent(out)   :: XUTau
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            integer, intent(in)                       :: Center
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System

            real(F64) :: x, y, z
            real(F64) :: ar
            real(F64) :: w1, w2, wm, s
            real(F64) :: rmk, rmg, mu
            real(F64), dimension(3) :: rm
            integer :: u
            integer :: idm, m
            integer :: wcntrb

            real(F64) :: const1 = TWO / (ONE - becka)

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart, &
                  SortedDistances => System%SortedDistances, &
                  SortedDistancesIdx => System%SortedDistancesIdx &
                  )
                  !
                  ! Check if Becke's weight equal to ONE.
                  ! Inequality 15 in [1]
                  !
                  ar = const1 * r
                  if (ar <= SortedDistances(2, Center)) then
                        do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              XWeights(u) = RWeight * SphereWeights(u)
                              !
                              ! Generate next point in batch
                              !
                              call ksmgga_XU(Rho(:, :, 1), Rho(:, :, 2), x, y, z, &
                                    XURho(1, u), XURho(2, u), XGradA(:, u), &
                                    XGradB(:, u), XUSig(1, u), XUSig(3, u), XUSig(2, u), &
                                    XULap(1, u), XULap(2, u), XUTau(1, u), XUTau(2, u), &
                                    XOrb(:, u), XShells(:, u), XNShells(u), &
                                    NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                    AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                    ShellParamsIdx, ShellMomentum, NPrimitives, &
                                    CntrCoeffs, Exponents, NormFactors, &
                                    AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
                        end do
                  else
                        wcntrb = 1
                        do u = 3, NAtoms
                              if (ar <= SortedDistances(u, Center)) then
                                    exit
                              else
                                    wcntrb = wcntrb + 1
                              end if
                        end do
                        sphere: do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              rmk = SortedDistances(2, Center)
                              idm = SortedDistancesIdx(2, Center)
                              rm = AtomCoords(:, idm)
                              rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                              mu = (r - rmg) / (becka * rmk)
                              if (mu .lt. ONE) then
                                    if (mu <= -ONE) then
                                          w1 = ONE
                                          w2 = ZERO
                                    else
                                          call ks_step_function(mu, w1)
                                          call ks_becke(idm, rmg, x, y, z, w2, &
                                                AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                    end if
                                    weights: do m = 3, wcntrb + 1
                                          rmk = SortedDistances(m, Center)
                                          idm = SortedDistancesIdx(m, Center)
                                          rm = AtomCoords(:, idm)
                                          rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                          mu = (r - rmg) / (becka * rmk)
                                          if (mu >= ONE) then
                                                !
                                                ! The step function employed in the weight factor is exactly
                                                ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                                ! must be defined as zero to avoid processing NaNs and/or unphysical
                                                ! values which are present due to the lack of initialization.
                                                !
                                                XURho(:, u) = ZERO
                                                XGradA(:, u) = ZERO
                                                XGradB(:, u) = ZERO
                                                XUSig(:, u) = ZERO
                                                XULap(:, u) = ZERO
                                                XUTau(:, u) = ZERO
                                                XWeights(u) = ZERO
                                                XNShells(u) = 0
                                                cycle sphere
                                          else if (mu <= -ONE) then
                                                cycle weights
                                          else
                                                call ks_step_function(mu, s)
                                                w1 = w1 * s
                                                call ks_becke(idm, rmg, x, y, z, wm, &
                                                      AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                                w2 = w2 + wm
                                          end if
                                    end do weights
                                    w2 = w2 + w1
                                    XWeights(u) = RWeight * SphereWeights(u) * w1 / w2
                                    call ksmgga_XU(Rho(:, :, 1), Rho(:, :, 2), x, y, z, &
                                          XURho(1, u), XURho(2, u), XGradA(:, u), &
                                          XGradB(:, u), XUSig(1, u), XUSig(3, u), &
                                          XUSig(2, u), XULap(1, u), XULap(2, u), &
                                          XUTau(1, u), XUTau(2, u), XOrb(:, u), &
                                          XShells(:, u), XNShells(u), &
                                          NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                          AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                          ShellParamsIdx, ShellMomentum, NPrimitives, &
                                          CntrCoeffs, Exponents, NormFactors, &
                                          AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
                              else
                                    XURho(:, u) = ZERO
                                    XGradA(:, u) = ZERO
                                    XGradB(:, u) = ZERO
                                    XUSig(:, u) = ZERO
                                    XULap(:, u) = ZERO
                                    XUTau(:, u) = ZERO
                                    XWeights(u) = ZERO
                                    XNShells(u) = 0
                              end if
                        end do sphere
                  end if
            end associate
      end subroutine ks_XU_MGGA_Sphere


      subroutine ks_X_GGA_Sphere(XRho, XGrad, XSig, XOrb, XShells, XNShells, XWeights, Rho, &
            Center, X0, Y0, Z0, R, RWeight, UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, &
            NSpher, AOBasis, System)
            
            real(F64), dimension(:), intent(out)      :: XRho
            real(F64), dimension(:, :), intent(out)   :: XGrad
            real(F64), dimension(:), intent(out)      :: XSig
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            integer, intent(in)                       :: Center
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System

            real(F64) :: x, y, z
            real(F64) :: ar
            real(F64) :: w1, w2, wm, s
            real(F64) :: rmk, rmg, mu
            real(F64), dimension(3) :: rm
            integer :: u
            integer :: idm, m
            integer :: wcntrb

            real(F64) :: const1 = TWO / (ONE - becka)

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart, &
                  SortedDistances => System%SortedDistances, &
                  SortedDistancesIdx => System%SortedDistancesIdx &
                  )
                  !
                  ! Check if Becke's weight equal to ONE.
                  ! Inequality 15 in [1]
                  !
                  ar = const1 * r
                  if (ar <= SortedDistances(2, Center)) then
                        do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              XWeights(u) = RWeight * SphereWeights(u)
                              !
                              ! Generate next point in batch
                              !
                              call ksgga_X(Rho(:, :, 1), x, y, z, &
                                    XRho(u), XGrad(:, u), XSig(u), &
                                    XOrb(:, u), XShells(:, u), XNShells(u), &
                                    NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                    AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                    ShellParamsIdx, ShellMomentum, NPrimitives, &
                                    CntrCoeffs, Exponents, NormFactors, AtomShellMap, &
                                    AtomShellN, MaxAtomL, R2Max, ShellLoc)
                        end do
                  else
                        wcntrb = 1
                        do u = 3, NAtoms
                              if (ar <= SortedDistances(u, Center)) then
                                    exit
                              else
                                    wcntrb = wcntrb + 1
                              end if
                        end do
                        sphere: do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              rmk = SortedDistances(2, Center)
                              idm = SortedDistancesIdx(2, Center)
                              rm = AtomCoords(:, idm)
                              rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                              mu = (r - rmg) / (becka * rmk)
                              if (mu .lt. ONE) then
                                    if (mu <= -ONE) then
                                          w1 = ONE
                                          w2 = ZERO
                                    else
                                          call ks_step_function(mu, w1)
                                          call ks_becke(idm, rmg, x, y, z, w2, &
                                                AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                    end if
                                    weights: do m = 3, wcntrb + 1
                                          rmk = SortedDistances(m, Center)
                                          idm = SortedDistancesIdx(m, Center)
                                          rm = AtomCoords(:, idm)
                                          rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                          mu = (r - rmg) / (becka * rmk)
                                          if (mu >= ONE) then
                                                !
                                                ! The step function employed in the weight factor is exactly
                                                ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                                ! must be defined as zero to avoid processing NaNs and/or unphysical
                                                ! values which are present due to the lack of initialization.
                                                !
                                                XRho(u) = ZERO
                                                XGrad(:, u) = ZERO
                                                XSig(u) = ZERO
                                                XWeights(u) = ZERO
                                                XNShells(u) = 0
                                                cycle sphere
                                          else if (mu <= -ONE) then
                                                cycle weights
                                          else
                                                call ks_step_function(mu, s)
                                                w1 = w1 * s
                                                call ks_becke(idm, rmg, x, y, z, wm, &
                                                      AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                                w2 = w2 + wm
                                          end if
                                    end do weights
                                    w2 = w2 + w1
                                    XWeights(u) = RWeight * SphereWeights(u) * w1 / w2
                                    call ksgga_X(Rho(:, :, 1), x, y, z, &
                                          XRho(u), XGrad(:, u), XSig(u), &
                                          XOrb(:, u), XShells(:, u), XNShells(u), &
                                          NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                          AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                          ShellParamsIdx, ShellMomentum, NPrimitives, &
                                          CntrCoeffs, Exponents, NormFactors, AtomShellMap, &
                                          AtomShellN, MaxAtomL, R2Max, ShellLoc)
                              else
                                    XRho(u) = ZERO
                                    XGrad(:, u) = ZERO
                                    XSig(u) = ZERO
                                    XWeights(u) = ZERO
                                    XNShells(u) = 0
                              end if
                        end do sphere
                  end if
            end associate
      end subroutine ks_X_GGA_Sphere


      subroutine ks_XU_GGA_Sphere(XURho, XGradA, XGradB, XUSig, XOrb, XShells, XNShells, XWeights, &
            Rho, Center, X0, Y0, Z0, R, RWeight, UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, &
            NSpher, AOBasis, System)
            
            real(F64), dimension(:, :), intent(out)   :: XURho
            real(F64), dimension(:, :), intent(out)   :: XGradA
            real(F64), dimension(:, :), intent(out)   :: XGradB
            real(F64), dimension(:, :), intent(out)   :: XUSig
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            integer, intent(in)                       :: Center
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis
            type(TSystem), intent(in)                 :: System

            real(F64) :: x, y, z
            real(F64) :: ar
            real(F64) :: w1, w2, wm, s
            real(F64) :: rmk, rmg, mu
            real(F64), dimension(3) :: rm
            integer :: u
            integer :: idm, m
            integer :: wcntrb

            real(F64) :: const1 = TWO / (ONE - becka)

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart, &
                  SortedDistances => System%SortedDistances, &
                  SortedDistancesIdx => System%SortedDistancesIdx &
                  )
                  !
                  ! Check if Becke's weight equal to ONE.
                  ! Inequality 15 in [1]
                  !
                  ar = const1 * r
                  if (ar <= SortedDistances(2, Center)) then
                        do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              XWeights(u) = RWeight * SphereWeights(u)
                              !
                              ! Generate next point in batch
                              !
                              call ksgga_XU(Rho(:, :, 1), Rho(:, :, 2), x, y, z, &
                                    XURho(1, u), XURho(2, u), XGradA(:, u), &
                                    XGradB(:, u), XUSig(1, u), XUSig(3, u), &
                                    XUSig(2, u), XOrb(:, u), XShells(:, u), &
                                    XNShells(u), NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                    AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                    ShellParamsIdx, ShellMomentum, NPrimitives, &
                                    CntrCoeffs, Exponents, NormFactors, AtomShellMap, &
                                    AtomShellN, MaxAtomL, R2Max, ShellLoc)
                        end do
                  else
                        wcntrb = 1
                        do u = 3, NAtoms
                              if (ar <= SortedDistances(u, Center)) then
                                    exit
                              else
                                    wcntrb = wcntrb + 1
                              end if
                        end do
                        sphere: do u = 1, NSpher
                              x = X0 + R * UnitSphereX(u)
                              y = Y0 + R * UnitSphereY(u)
                              z = Z0 + R * UnitSphereZ(u)
                              rmk = SortedDistances(2, Center)
                              idm = SortedDistancesIdx(2, Center)
                              rm = AtomCoords(:, idm)
                              rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                              mu = (r - rmg) / (becka * rmk)
                              if (mu .lt. ONE) then
                                    if (mu <= -ONE) then
                                          w1 = ONE
                                          w2 = ZERO
                                    else
                                          call ks_step_function(mu, w1)
                                          call ks_becke(idm, rmg, x, y, z, w2, &
                                                AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                    end if
                                    weights: do m = 3, wcntrb + 1
                                          rmk = SortedDistances(m, Center)
                                          idm = SortedDistancesIdx(m, Center)
                                          rm = AtomCoords(:, idm)
                                          rmg = sqrt((x - rm(1))**2 + (y - rm(2))**2 + (z - rm(3))**2)
                                          mu = (r - rmg) / (becka * rmk)
                                          if (mu >= ONE) then
                                                !
                                                ! The step function employed in the weight factor is exactly
                                                ! zero for mu >= 1 (Fig. 1 in Ref. 1). Density and other ingredients
                                                ! must be defined as zero to avoid processing NaNs and/or unphysical
                                                ! values which are present due to the lack of initialization.
                                                !
                                                XURho(:, u) = ZERO
                                                XGradA(:, u) = ZERO
                                                XGradB(:, u) = ZERO
                                                XUSig(:, u) = ZERO
                                                XWeights(u) = ZERO
                                                XNShells(u) = 0
                                                cycle sphere
                                          else if (mu <= -ONE) then
                                                cycle weights
                                          else
                                                call ks_step_function(mu, s)
                                                w1 = w1 * s
                                                call ks_becke(idm, rmg, x, y, z, wm, &
                                                      AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
                                                w2 = w2 + wm
                                          end if
                                    end do weights
                                    w2 = w2 + w1
                                    XWeights(u) = RWeight * SphereWeights(u) * w1 / w2
                                    call ksgga_XU(Rho(:, :, 1), Rho(:, :, 2), x, y, z, &
                                          XURho(1, u), XURho(2, u), XGradA(:, u), &
                                          XGradB(:, u), XUSig(1, u), XUSig(3, u), &
                                          XUSig(2, u), XOrb(:, u), XShells(:, u), &
                                          XNShells(u), NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                                          AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                                          ShellParamsIdx, ShellMomentum, NPrimitives, &
                                          CntrCoeffs, Exponents, NormFactors, AtomShellMap, &
                                          AtomShellN, MaxAtomL, R2Max, ShellLoc)
                              else
                                    XURho(:, u) = ZERO
                                    XGradA(:, u) = ZERO
                                    XGradB(:, u) = ZERO
                                    XUSig(:, u) = ZERO
                                    XWeights(u) = ZERO
                                    XNShells(u) = 0
                              end if
                        end do sphere
                  end if
            end associate
      end subroutine ks_XU_GGA_Sphere


      subroutine ks_X_GGA_Sphere_Atom(XRho, XGrad, XSig, XOrb, XShells, XNShells, XWeights, Rho, &
            X0, Y0, Z0, R, RWeight, UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, &
            NSpher, AOBasis)
            ! 
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not needed. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham  matrix.
            ! See SPHERQUAD subroutine description.
            !
            real(F64), dimension(:), intent(out)      :: XRho
            real(F64), dimension(:, :), intent(out)   :: XGrad
            real(F64), dimension(:), intent(out)      :: XSig
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis

            real(F64) :: x, y, z
            integer :: u

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart &
                  )
                  do u = 1, NSpher
                        x = X0 + R * UnitSphereX(u)
                        y = Y0 + R * UnitSphereY(u)
                        z = Z0 + R * UnitSphereZ(u)
                        XWeights(u) = RWeight * SphereWeights(u)
                        call ksgga_X(Rho(:, :, 1), x, y, z, &
                              XRho(u), XGrad(:, u), XSig(u), &
                              XOrb(:, u), XShells(:, u), XNShells(u), &
                              NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                              AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                              ShellParamsIdx, ShellMomentum, NPrimitives, &
                              CntrCoeffs, Exponents, NormFactors, AtomShellMap, &
                              AtomShellN, MaxAtomL, R2Max, ShellLoc)
                  end do
            end associate
      end subroutine ks_X_GGA_Sphere_Atom


      subroutine ks_XU_GGA_Sphere_Atom(XURho, XGradA, XGradB, XUSig, XOrb, XShells, XNShells, XWeights, &
            Rho, X0, Y0, Z0, R, RWeight, UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, &
            NSpher, AOBasis)
            !
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not needed.
            !
            real(F64), dimension(:, :), intent(out)   :: XURho
            real(F64), dimension(:, :), intent(out)   :: XGradA
            real(F64), dimension(:, :), intent(out)   :: XGradB
            real(F64), dimension(:, :), intent(out)   :: XUSig
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis

            real(F64) :: x, y, z
            integer :: u

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart &
                  )
                  do u = 1, NSpher
                        x = X0 + R * UnitSphereX(u)
                        y = Y0 + R * UnitSphereY(u)
                        z = Z0 + R * UnitSphereZ(u)
                        XWeights(u) = RWeight * SphereWeights(u)
                        call ksgga_XU(Rho(:, :, 1), Rho(:, :, 2), x, y, z, &
                              XURho(1, u), XURho(2, u), XGradA(:, u), &
                              XGradB(:, u), XUSig(1, u), XUSig(3, u), &
                              XUSig(2, u), XOrb(:, u), XShells(:, u), &
                              XNShells(u), NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                              AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                              ShellParamsIdx, ShellMomentum, NPrimitives, &
                              CntrCoeffs, Exponents, NormFactors, AtomShellMap, &
                              AtomShellN, MaxAtomL, R2Max, ShellLoc)
                  end do
            end associate
      end subroutine ks_XU_GGA_Sphere_Atom


      subroutine ks_X_MGGA_Sphere_Atom(XRho, XGrad, XSig, XLap, XTau, XOrb, XShells, XNShells, XWeights, &
            Rho, X0, Y0, Z0, R, RWeight, UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, &
            NSpher, AOBasis)
            !
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not needed.
            !
            real(F64), dimension(:), intent(out)      :: XRho
            real(F64), dimension(:, :), intent(out)   :: XGrad
            real(F64), dimension(:), intent(out)      :: XSig
            real(F64), dimension(:), intent(out)      :: XLap
            real(F64), dimension(:), intent(out)      :: XTau
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis

            real(F64) :: x, y, z
            integer :: u

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart &
                  )
                  do u = 1, NSpher
                        x = X0 + R * UnitSphereX(u)
                        y = Y0 + R * UnitSphereY(u)
                        z = Z0 + R * UnitSphereZ(u)
                        XWeights(u) = RWeight * SphereWeights(u)
                        call ksmgga_X(Rho(:, :, 1), x, y, z, &
                              XRho(u), XGrad(:, u), XSig(u), &
                              XLap(u), XTau(u), XOrb(:, u), &
                              XShells(:, u), XNShells(u), &
                              NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                              AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                              ShellParamsIdx, ShellMomentum, NPrimitives, &
                              CntrCoeffs, Exponents, NormFactors, &
                              AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
                  end do
            end associate
      end subroutine ks_X_MGGA_Sphere_Atom


      subroutine ks_XU_MGGA_Sphere_Atom(XURho, XGradA, XGradB, XUSig, XULap, XUTau, &
            XOrb, XShells, XNShells, XWeights, Rho, X0, Y0, Z0, R, RWeight, &
            UnitSphereX, UnitSphereY, UnitSphereZ, SphereWeights, NSpher, AOBasis)
            !
            ! Perform spherical part of integration.  SINGLE-ATOM VERSION.
            ! Becke's weights are not required. Call KSCONTRIB for each point
            ! in order to make contributions to Kohn-Sham  matrix.
            ! See SPHERQUAD subroutine description.
            !
            real(F64), dimension(:, :), intent(out)   :: XURho
            real(F64), dimension(:, :), intent(out)   :: XGradA
            real(F64), dimension(:, :), intent(out)   :: XGradB
            real(F64), dimension(:, :), intent(out)   :: XUSig
            real(F64), dimension(:, :), intent(out)   :: XULap
            real(F64), dimension(:, :), intent(out)   :: XUTau
            real(F64), dimension(:, :), intent(out)   :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            real(F64), dimension(:, :, :), intent(in) :: Rho
            real(F64), intent(in)                     :: X0, Y0, Z0, R
            real(F64), intent(in)                     :: RWeight
            real(F64), dimension(:), intent(in)       :: UnitSphereX
            real(F64), dimension(:), intent(in)       :: UnitSphereY
            real(F64), dimension(:), intent(in)       :: UnitSphereZ
            real(F64), dimension(:), intent(in)       :: SphereWeights
            integer, intent(in)                       :: NSpher
            type(TAOBasis), intent(in)                :: AOBasis

            real(F64) :: x, y, z
            integer :: u

            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  NAtoms => AOBasis%NAtoms, &
                  CartPolyX => AOBasis%CartPolyX, &
                  CartPolyY => AOBasis%CartPolyY, &
                  CartPolyZ => AOBasis%CartPolyZ, &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  MaxNPrimitives => AOBasis%MaxNPrimitives, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactors => AOBasis%NormFactorsCart, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxAtomL => AOBasis%MaxAtomL, &
                  R2Max => AOBasis%R2Max, &
                  ShellLoc => AOBasis%ShellLocCart &
                  )
                  do u = 1, NSpher
                        x = X0 + R * UnitSphereX(u)
                        y = Y0 + R * UnitSphereY(u)
                        z = Z0 + R * UnitSphereZ(u)
                        XWeights(u) = RWeight * SphereWeights(u)
                        call ksmgga_XU(Rho(:, :, 1), Rho(:, :, 2), x, y, z, &
                              XURho(1, u), XURho(2, u), XGradA(:, u), &
                              XGradB(:, u), XUSig(1, u), XUSig(3, u), &
                              XUSig(2, u), XULap(1, u), XULap(2, u), &
                              XUTau(1, u), XUTau(2, u), XOrb(:, u), &
                              XShells(:, u), XNShells(u), &
                              NAngFunc, CartPolyX, CartPolyY, CartPolyZ, &
                              AtomCoords, NAtoms, LmaxGTO, MaxNPrimitives, &
                              ShellParamsIdx, ShellMomentum, NPrimitives, &
                              CntrCoeffs, Exponents, NormFactors, &
                              AtomShellMap, AtomShellN, MaxAtomL, R2Max, ShellLoc)
                  end do
            end associate
      end subroutine ks_XU_MGGA_Sphere_Atom


      subroutine ks_Vxc_GGA_Sphere(Vxc, exc, Nel, Div, XRho, XGrad, XOrb, &
            XShells, XNShells, XWeights, YEps, YRho, YSig, NSpher, DeltaK, AOBasis, &
            ShellPairLoc)
            real(F64), dimension(:, :), intent(inout)    :: Vxc
            real(F64), intent(inout)                     :: exc
            real(F64), intent(inout)                     :: Nel
            real(F64), intent(inout)                     :: Div
            real(F64), dimension(:), intent(in)          :: XRho
            real(F64), dimension(:, :), intent(in)       :: XGrad
            real(F64), dimension(:, :), intent(in)       :: XOrb
            integer, dimension(:, :), intent(in)         :: XShells
            integer, dimension(:), intent(in)            :: XNShells
            real(F64), dimension(:), intent(in)          :: XWeights
            real(F64), dimension(:), intent(in)          :: YEps
            real(F64), dimension(:), intent(in)          :: YRho
            real(F64), dimension(:), intent(in)          :: YSig
            integer, intent(in)                          :: NSpher
            integer, intent(in)                          :: DeltaK
            type(TAOBasis), intent(in)                   :: AOBasis
            integer, dimension(:, :), intent(in)         :: ShellPairLoc

            integer :: k, n0
            real(F64) :: rho, eps, vrho, vsigma, weight
            real(F64), dimension(3) :: grad
            integer :: MaxNAngFunc

            associate ( &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ThreshRho => GRID_RHOTHRESH, &
                  LmaxGTO => AOBasis%LmaxGTO &
                  )
                  MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
                  do k = 1, NSpher
                        n0 = XNShells(k)
                        rho = XRho(k)
                        grad = XGrad(:, k)
                        eps = YEps(k)
                        vrho = YRho(k)
                        vsigma = YSig(k)
                        weight = XWeights(k)
                        call ksgga_Vxc(Vxc(:, 1), exc, Nel, Div, &
                              XShells(:, k), XOrb(:, k), &
                              n0, rho, grad, eps, vrho, vsigma, weight, DeltaK, &
                              NAngFunc, ShellParamsIdx, ThreshRho, &
                              MaxNAngFunc, ShellPairLoc)
                  end do
            end associate
      end subroutine ks_Vxc_GGA_Sphere


      subroutine ks_UVxc_GGA_Sphere(Vxc, exc, Nel, Div, XURho, XGradA, XGradB, &
            XOrb, XShells, XNShells, XWeights, YEps, YURho, YUSig, NSpher, DeltaK, &
            AOBasis, ShellPairLoc)
            real(F64), dimension(:, :), intent(inout)    :: Vxc
            real(F64), intent(inout)                     :: exc
            real(F64), intent(inout)                     :: Nel
            real(F64), intent(inout)                     :: Div
            real(F64), dimension(:, :), intent(in)       :: XURho
            real(F64), dimension(:, :), intent(in)       :: XGradA
            real(F64), dimension(:, :), intent(in)       :: XGradB
            real(F64), dimension(:, :), intent(in)       :: XOrb
            integer, dimension(:, :), intent(in)         :: XShells
            integer, dimension(:), intent(in)            :: XNShells
            real(F64), dimension(:), intent(in)          :: XWeights
            real(F64), dimension(:), intent(in)          :: YEps
            real(F64), dimension(:, :), intent(in)       :: YURho
            real(F64), dimension(:, :), intent(in)       :: YUSig  
            integer, intent(in)                          :: NSpher
            integer, intent(in)                          :: DeltaK
            type(TAOBasis), intent(in)                   :: AOBasis
            integer, dimension(:, :), intent(in)         :: ShellPairLoc

            integer :: k, n0
            real(F64) :: rhoa, rhob, eps
            real(F64) :: vrhoa, vrhob, vsigma_aa
            real(F64) :: vsigma_bb, vsigma_ab, weight
            real(F64), dimension(3) :: grada, gradb
            integer :: MaxNAngFunc

            associate ( &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ThreshRho => GRID_RHOTHRESH, &
                  LmaxGTO => AOBasis%LmaxGTO &
                  )
                  MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
                  do k = 1, NSpher
                        n0 = XNShells(k)
                        rhoa = XURho(1, k)
                        rhob = XURho(2, k)
                        grada = XGradA(:, k)
                        gradb = XGradB(:, k)
                        eps = YEps(k)
                        vrhoa = YURho(1, k)
                        vrhob = YURho(2, k)
                        vsigma_aa = YUSig(1, k)
                        vsigma_ab = YUSig(2, k)
                        vsigma_bb = YUSig(3, k)
                        weight = XWeights(k)
                        call ksgga_UVxc(Vxc, &
                              exc, Nel, Div, XShells(:, k), &
                              XOrb(:, k), n0, rhoa, rhob, &
                              grada, gradb, eps, vrhoa, vrhob, vsigma_aa, &
                              vsigma_bb, vsigma_ab, weight, DeltaK, &
                              NAngFunc, ShellParamsIdx, ThreshRho, &
                              MaxNAngFunc, ShellPairLoc)
                  end do
            end associate
      end subroutine ks_UVxc_GGA_Sphere


      subroutine ks_Vxc_MGGA_Sphere(Vxc, exc, Nel, Div, Kin, Lap, &
            XRho, XGrad, XLap, XTau, XOrb, XShells, XNShells, XWeights, &
            YEps, YRho, YSig, YLap, YTau, NSpher, DeltaK, AOBasis, ShellPairLoc)
            real(F64), dimension(:, :), intent(inout)    :: Vxc
            real(F64), intent(inout)                     :: exc
            real(F64), intent(inout)                     :: Nel
            real(F64), intent(inout)                     :: Div
            real(F64), intent(inout)                     :: Kin
            real(F64), intent(inout)                     :: Lap
            real(F64), dimension(:), intent(in)          :: XRho
            real(F64), dimension(:, :), intent(in)       :: XGrad
            real(F64), dimension(:), intent(in)          :: XLap
            real(F64), dimension(:), intent(in)          :: XTau
            real(F64), dimension(:, :), intent(in)       :: XOrb
            integer, dimension(:, :), intent(in)         :: XShells
            integer, dimension(:), intent(in)            :: XNShells
            real(F64), dimension(:), intent(in)          :: XWeights
            real(F64), dimension(:), intent(in)          :: YEps
            real(F64), dimension(:), intent(in)          :: YRho
            real(F64), dimension(:), intent(in)          :: YSig
            real(F64), dimension(:), intent(in)          :: YLap
            real(F64), dimension(:), intent(in)          :: YTau
            integer, intent(in)                          :: NSpher
            integer, intent(in)                          :: DeltaK
            type(TAOBasis), intent(in)                   :: AOBasis
            integer, dimension(:, :), intent(in)         :: ShellPairLoc

            integer :: k, n0
            real(F64) :: rho, lapl, tau, eps, vrho, vsigma, weight
            real(F64) :: vlapl, vtau
            real(F64), dimension(3) :: grad
            integer :: MaxNAngFunc

            associate ( &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ThreshRho => GRID_RHOTHRESH, &
                  LmaxGTO => AOBasis%LmaxGTO &
                  )
                  MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
                  do k = 1, NSpher
                        n0 = XNShells(k)
                        rho = XRho(k)
                        grad = XGrad(:, k)
                        lapl = XLap(k)
                        tau = XTau(k)
                        eps = YEps(k)
                        vrho = YRho(k)
                        vsigma = YSig(k)
                        vlapl = YLap(k)
                        vtau = YTau(k)
                        weight = XWeights(k)
                        call ksmgga_Vxc(Vxc(:, 1), exc, &
                              Nel, Div, Kin, Lap, XShells(:, k), &
                              XOrb(:, k), n0, rho, grad, lapl, tau, eps, vrho, &
                              vsigma, vlapl, vtau, weight, DeltaK, &
                              NAngFunc, ShellParamsIdx, ThreshRho, &
                              MaxNAngFunc, ShellPairLoc)
                  end do
            end associate
      end subroutine ks_Vxc_MGGA_Sphere


      subroutine ks_UVxc_MGGA_Sphere(Vxc, exc, Nel, Div, Kin, Lap, &
            XURho, XGradA, XGradB, XULap, XUTau, XOrb, XShells, XNShells, XWeights, &
            YEps, YURho, YUSig, YULap, YUTau, NSpher, DeltaK, AOBasis, ShellPairLoc)
            real(F64), dimension(:, :), intent(inout)    :: Vxc
            real(F64), intent(inout)                     :: exc
            real(F64), intent(inout)                     :: Nel
            real(F64), intent(inout)                     :: Div
            real(F64), intent(inout)                     :: Kin
            real(F64), intent(inout)                     :: Lap
            real(F64), dimension(:, :), intent(in)       :: XURho
            real(F64), dimension(:, :), intent(in)       :: XGradA
            real(F64), dimension(:, :), intent(in)       :: XGradB
            real(F64), dimension(:, :), intent(in)       :: XULap
            real(F64), dimension(:, :), intent(in)       :: XUTau
            real(F64), dimension(:, :), intent(in)       :: XOrb
            integer, dimension(:, :), intent(in)         :: XShells
            integer, dimension(:), intent(in)            :: XNShells
            real(F64), dimension(:), intent(in)          :: XWeights
            real(F64), dimension(:), intent(in)          :: YEps
            real(F64), dimension(:, :), intent(in)       :: YURho
            real(F64), dimension(:, :), intent(in)       :: YUSig
            real(F64), dimension(:, :), intent(in)       :: YULap
            real(F64), dimension(:, :), intent(in)       :: YUTau
            integer, intent(in)                          :: NSpher
            integer, intent(in)                          :: DeltaK
            type(TAOBasis), intent(in)                   :: AOBasis
            integer, dimension(:, :), intent(in)         :: ShellPairLoc
            
            integer :: k, n0
            real(F64) :: rhoa, rhob, eps, lapla, laplb, taua, taub
            real(F64) :: vrhoa, vrhob, vsigma_aa
            real(F64) :: vsigma_bb, vsigma_ab, weight
            real(F64) :: vlapla, vlaplb, vtaua, vtaub
            real(F64), dimension(3) :: grada, gradb
            integer :: MaxNAngFunc

            associate ( &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ThreshRho => GRID_RHOTHRESH, &
                  LmaxGTO => AOBasis%LmaxGTO &
                  )
                  MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
                  do k = 1, NSpher
                        n0 = XNShells(k)
                        rhoa = XURho(1, k)
                        rhob = XURho(2, k)
                        grada = XGradA(:, k)
                        gradb = XGradB(:, k)
                        lapla = XULap(1, k)
                        laplb = XULap(2, k)
                        taua = XUTau(1, k)
                        taub = XUTau(2, k)
                        eps = YEps(k)
                        vrhoa = YURho(1, k)
                        vrhob = YURho(2, k)
                        vsigma_aa = YUSig(1, k)
                        vsigma_ab = YUSig(2, k)
                        vsigma_bb = YUSig(3, k)
                        vlapla = YULap(1, k)
                        vlaplb = YULap(2, k)
                        vtaua = YUTau(1, k)
                        vtaub = YUTau(2, k)
                        weight = XWeights(k)
                        call ksmgga_UVxc(Vxc, &
                              exc, Nel, Div, Kin, Lap, &
                              XShells(:, k), XOrb(:, k), n0, &
                              rhoa, rhob, grada, gradb, lapla, laplb, taua, taub, eps, &
                              vrhoa, vrhob, vsigma_aa, vsigma_bb, vsigma_ab, vlapla, vlaplb, &
                              vtaua, vtaub, weight, DeltaK, &
                              NAngFunc, ShellParamsIdx, ThreshRho, MaxNAngFunc, ShellPairLoc)
                  end do
            end associate
      end subroutine ks_UVxc_MGGA_Sphere


      subroutine ks_BufferDim(Dim, AOBasis)
            integer, intent(out)       :: Dim
            type(TAOBasis), intent(in) :: AOBasis
            
            integer :: ShellA, ShellB
            integer :: Na, Nb

            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncCart &
                  )                  
                  Dim = 0
                  do ShellB = 1, NShells
                        do ShellA = ShellB, NShells
                              Na = NAngFunc(ShellParamsIdx(ShellA))
                              Nb = NAngFunc(ShellParamsIdx(ShellB))
                              Dim = Dim + Na * Nb
                        end do
                  end do
            end associate
      end subroutine ks_BufferDim

      
      subroutine ks_ShellPairLoc(ShellPairLoc, NShells, ShellParamsIdx, NAngFunc)
            integer, dimension(:, :), intent(out) :: ShellPairLoc
            integer, intent(in)                   :: NShells
            integer, dimension(:), intent(in)     :: ShellParamsIdx
            integer, dimension(:), intent(in)     :: NAngFunc

            integer :: ShellA, ShellB
            integer :: Na, Nb
            integer :: N

            ShellPairLoc = 0
            N = 0
            do ShellB = 1, NShells
                  do ShellA = ShellB, NShells
                        Na = NAngFunc(ShellParamsIdx(ShellA))
                        Nb = NAngFunc(ShellParamsIdx(ShellB))
                        ShellPairLoc(ShellA, ShellB) = N + 1
                        N = N + Na * Nb
                  end do
            end do
      end subroutine ks_ShellPairLoc

                  
      subroutine ks_UpdateVxc(Vxc, Txc, ShellPairLoc, ShellLoc, NAngFunc, ShellParamsIdx, NShells)
            real(F64), dimension(:, :, :), intent(inout) :: Vxc
            real(F64), dimension(:, :), intent(in)       :: Txc
            integer, dimension(:, :), intent(in)         :: ShellPairLoc
            integer, dimension(:), intent(in)            :: ShellLoc
            integer, dimension(:), intent(in)            :: NAngFunc
            integer, dimension(:), intent(in)            :: ShellParamsIdx
            integer, intent(in)                          :: NShells

            integer :: ShellA, ShellB
            integer :: a0, a1, b0, b1, p0, p1
            integer :: Na, Nb
            integer :: NSpins, s

            NSpins = size(Vxc, dim=3)
            do s = 1, NSpins
                  do ShellB = 1, NShells
                        do ShellA = ShellB, NShells
                              Na = NAngFunc(ShellParamsIdx(ShellA))
                              a0 = ShellLoc(ShellA)
                              a1 = ShellLoc(ShellA) + Na - 1
                              Nb = NAngFunc(ShellParamsIdx(ShellB))
                              b0 = ShellLoc(ShellB)
                              b1 = ShellLoc(ShellB) + Nb - 1
                              p0 = ShellPairLoc(ShellA, ShellB)
                              p1 = ShellPairLoc(ShellA, ShellB) + Na * Nb - 1
                              Vxc(a0:a1, b0:b1, s) = Vxc(a0:a1, b0:b1, s) + reshape(Txc(p0:p1, s), [Na, Nb])
                        end do
                  end do
            end do
      end subroutine ks_UpdateVxc

      
      subroutine ks_MainLoop(Vxc, exc, Nel, Div, Kin, Lap, aux, Txc, Rho, RhoEff, XCDef, &
            AUXIn, AtomElementMap, AOBasis, System, GrdQuality)
            
            real(F64), dimension(:, :, :), intent(inout) :: Vxc
            real(F64), intent(inout)                     :: exc
            real(F64), intent(inout)                     :: Nel
            real(F64), intent(inout)                     :: Div
            real(F64), intent(inout)                     :: Kin
            real(F64), intent(inout)                     :: Lap
            real(F64), dimension(:), intent(inout)       :: aux
            real(F64), dimension(:, :, :), intent(out)   :: Txc
            real(F64), dimension(:, :, :), intent(in)    :: Rho
            real(F64), dimension(:, :, :), intent(in)    :: RhoEff
            type(TXCDef), intent(in)                     :: XCDef
            real(F64), dimension(:, :), intent(in)       :: AUXIn
            integer, dimension(:), intent(in)            :: AtomElementMap
            type(TAOBasis), intent(in)                   :: AOBasis
            type(TSystem), intent(in)                    :: System
            integer, intent(in)                          :: GrdQuality

            integer :: nauxint
            real(F64) :: x0, y0, z0
            real(F64) :: RWeight, r
            integer :: znum
            integer :: DeltaK
            logical :: mgga_ingredients
            integer :: aux_id, xc_id
            logical :: aux_mgga, xc_mgga
            logical :: exchange_correlation, auxiliary_integral
            integer, dimension(:), allocatable :: XNShells
            real(F64), dimension(:, :), allocatable :: XOrb
            integer, dimension(:, :), allocatable :: XShells
            real(F64), dimension(:, :), allocatable :: XGrad, XGradA, XGradB
            real(F64), dimension(:), allocatable :: XRho, XSig, XLap, XTau
            real(F64), dimension(:, :), allocatable :: XURho, XUSig, XULap, XUTau
            real(F64), dimension(:), allocatable :: XWeights, YEps
            real(F64), dimension(:), allocatable :: YRho, YSig, YLap, YTau
            real(F64), dimension(:, :), allocatable :: YURho, YUSig, YULap, YUTau
            real(F64), dimension(:, :), allocatable :: Taux
            integer, dimension(:, :), allocatable :: ShellPairLoc
            integer :: k, l, kl
            integer :: RGrdPtr, SGrdPtr
            integer :: s0, s1
            integer :: MaxNSpher, NSpher, SpherGrid
            logical :: SpinUnres
            integer, dimension(5) :: SpherGrids
            integer :: NRadial, Nkl
            integer :: ThisImage, NImages, ThisThread, NThreads
            integer :: MaxNAngFunc
            real(F64) :: EXXFrac

            ThisImage = this_image()
            NImages = num_images()
            !$omp parallel default(shared)
            !$omp master
            NThreads = 1
            !$ NThreads = omp_get_num_threads()
            !$omp end master
            !$omp end parallel
            MaxNAngFunc = ((AOBasis%LmaxGTO + 1) * (AOBasis%LmaxGTO + 2)) / 2
            allocate(ShellPairLoc(AOBasis%NShells, AOBasis%NShells))
            call ks_ShellPairLoc(ShellPairLoc, AOBasis%NShells, AOBasis%ShellParamsIdx, AOBasis%NAngFuncCart)
            !
            ! Number of radial grid points per single atom
            ! (the same number for all atoms)
            !
            call gridparam(GrdQuality, NRadial, SpherGrids)
            MaxNSpher = LEBNPT(maxval(SpherGrids))
            Nkl = AOBasis%NAtoms * NRadial
            !
            ! Identifier of the auxiliary (non-XC) integral
            ! on the molecular grid
            !
            aux_id = aux_get_id(XCDef)
            aux_mgga = aux_ismgga(XCDef)
            exchange_correlation = xcf_isgridxc(XCDef)
            auxiliary_integral = (aux_id /= AUX_NONE)
            if (auxiliary_integral) then
                  nauxint = aux_arraydim(aux_id, xcf_isuncomp(XCDef))
                  allocate(Taux(nauxint, NThreads))
                  Taux = ZERO
            else
                  nauxint = 0
                  allocate(Taux(0, 0))
            end if
            !
            ! Identifier of the exchange-correlation functional
            !
            xc_id = xcf_get_id(XCDef)
            xc_mgga = xcf_ismgga(XCDef)
            SpinUnres = xcf_isuncomp(XCDef)
            EXXFrac = xcf_get_exx(XCDef)
            !
            ! Check if exchange-correlation functional or
            ! auxiliary integral are of meta-GGA type
            !                  
            if (xc_mgga .or. aux_mgga) then
                  DeltaK = KSMGGA_DELTAK
                  mgga_ingredients = .true.
            else
                  DeltaK = KSGGA_DELTAK
                  mgga_ingredients = .false.
            end if
            Txc = ZERO
            !$omp parallel &
            !$omp default(shared) &
            !$omp private(k, l, kl, R, RWeight, SpherGrid, NSpher) &
            !$omp private(RGrdPtr, SGrdPtr, s0, s1) &
            !$omp private(x0, y0, z0, znum) &
            !$omp private(XShells, XNShells, XWeights, XOrb) &
            !$omp private(XRho, XSig, XLap, XTau, XGrad) &
            !$omp private(XURho, XUSig, XULap, XUTau, XGradA, XGradB) &
            !$omp private(YEps, YRho, YSig, YLap, YTau) &
            !$omp private(YURho, YUSig, YULap, YUTau) &
            !$omp private(ThisThread) &
            !$omp reduction(+:exc, Nel, Div, Kin, Lap)
            ThisThread = 1
            !$ ThisThread = omp_get_thread_num() + 1
            allocate(XNShells(MaxNSpher), XShells(AOBasis%NShells, MaxNSpher), XWeights(MaxNSpher))
            allocate(YEps(MaxNSpher))
            if (SpinUnres) then
                  allocate(XURho(2, MaxNSpher), XGradA(3, MaxNSpher), XGradB(3, MaxNSpher), &
                        XUSig(3, MaxNSpher), YURho(2, MaxNSpher), YUSig(3, MaxNSpher))
            else
                  allocate(XRho(MaxNSpher), XGrad(3, MaxNSpher), XSig(MaxNSpher), &
                        YRho(MaxNSpher), YSig(MaxNSpher))
            end if
            if (mgga_ingredients) then
                  allocate(XOrb(5*AOBasis%NAOCart, MaxNSpher))
                  if (SpinUnres) then
                        allocate(XULap(2, MaxNSpher), XUTau(2, MaxNSpher), &
                              YULap(2, MaxNSpher), YUTau(2, MaxNSpher))
                  else
                        allocate(XLap(MaxNSpher), XTau(MaxNSpher), &
                              YLap(MaxNSpher), YTau(MaxNSpher))
                  end if
            else
                  allocate(XOrb(4*AOBasis%NAOCart, MaxNSpher))
            end if
            !
            ! A concurrent Fortran image will perform those tasks for which modulo(kl,NImages)==ThisImage
            !
            !$omp do schedule(dynamic)
            do kl = ThisImage, Nkl, NImages
                  !
                  ! The compound index kl carries information on the grid shell l
                  ! and the atomic center k:
                  ! kl = (l - 1) + (k - 1) * NRadial + 1
                  ! The individual indices k and l are extracted using integer
                  ! division and modulo operations.
                  !
                  k = (kl - 1) / NRadial + 1
                  l = kl - (k - 1) * NRadial
                  X0 = AOBasis%AtomCoords(1, k)
                  Y0 = AOBasis%AtomCoords(2, k)
                  Z0 = AOBasis%AtomCoords(3, k)
                  !
                  ! Get points and weights of radial quadrature.
                  ! The spherical grid depends on the current node of the radial grid.
                  !
                  znum = System%ZNumbers(k)
                  call rgridget(RGrdPtr, znum)
                  R = GRD_RGRIDR(RGrdPtr+l-1)
                  RWeight = GRD_RGRIDW(RGrdPtr+l-1)
                  SpherGrid = GRD_RGRIDNS(RGrdPtr+l-1)
                  NSpher = LEBNPT(SpherGrid)
                  call lebget(SGrdPtr, SpherGrid)
                  s0 = SGrdPtr
                  s1 = SGrdPtr + NSpher - 1
                  if (SpinUnres) then
                        !
                        ! Spin-uncompensated electronic density
                        !
                        if (AOBasis%NAtoms > 1) then
                              if (.not. mgga_ingredients) then
                                    !
                                    ! Generate ingredients for the XC functional: compute density,
                                    ! its gradient, the laplacian, the density of the kinetic energy,
                                    ! and values of the AOs at each grid point belonging to the sphere
                                    ! centered at X0, Y0, Z0. Compute quadrature weights for each point
                                    ! in the batch.
                                    !
                                    call ks_XU_GGA_Sphere(XURho, XGradA, XGradB, XUSig, XOrb, XShells, XNShells, XWeights, &
                                          Rho, k, X0, Y0, Z0, R, RWeight, GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), &
                                          GRD_LEBZ(s0:s1), GRD_LEBW(s0:s1), NSpher, AOBasis, System)
                              else
                                    call ks_XU_MGGA_Sphere(XURho, XGradA, XGradB, XUSig, XULap, XUTau, &
                                          XOrb, XShells, XNShells, XWeights, Rho, k, X0, Y0, Z0, R, RWeight, &
                                          GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), &
                                          GRD_LEBW(s0:s1), NSpher, AOBasis, System)
                              end if
                        else
                              !
                              ! Code for computing integrals in one-atom systems.
                              ! Weights for multi-atom integrations need not be computed here.
                              !
                              if (.not. mgga_ingredients) then
                                    call ks_XU_GGA_Sphere_Atom(XURho, XGradA, XGradB, XUSig, XOrb, XShells, XNShells, XWeights, &
                                          Rho, X0, Y0, Z0, R, RWeight, GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), &
                                          GRD_LEBZ(s0:s1), GRD_LEBW(s0:s1), NSpher, AOBasis)
                              else
                                    call ks_XU_MGGA_Sphere_Atom(XURho, XGradA, XGradB, XUSig, XULap, XUTau, &
                                          XOrb, XShells, XNShells, XWeights, Rho, X0, Y0, Z0, R, RWeight, &
                                          GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), &
                                          GRD_LEBW(s0:s1), NSpher, AOBasis)
                              end if
                        end if
                        !
                        ! Compute contribution to exchange-correlation energy
                        ! and KS matrix
                        !
                        if (exchange_correlation) then
                              ! ------------------------------------------------------------------
                              ! Exchange-correlation energy density & derivatives for all grid
                              ! points in the batch
                              ! ------------------------------------------------------------------
                              if (.not. xc_mgga) then
                                    call ks_YU_GGA_Sphere(YEps, YURho, YUSig, XURho, XUSig, XCDef, NSpher)
                              else
                                    call ks_YU_MGGA_Sphere(YEps, YURho, YUSig, YULap, YUTau, &
                                          XURho, XUSig, XULap, XUTau, XCDef, NSpher)
                              end if
                              ! ------------------------------------------------------------------
                              ! Asymptotic correction to the exchange-correlation potential and
                              ! its contribution to the energy density
                              ! ------------------------------------------------------------------
                              select case(XCDef%AsympVxc)
                              case (AC_LFAS)
                                    call u_chai2013_x(YEps, YURho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XURho, XCDef%AsympVxcOmega, &
                                          ONE/TWO, ONE-EXXFrac, AtomElementMap, AUXIn, System, AOBasis)
                              case (AC_LFAS_v2)
                                    call u_chai2013_x(YEps, YURho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XURho, XCDef%AsympVxcOmega, &
                                          ONE, ONE-EXXFrac, AtomElementMap, AUXIn, System, AOBasis)
                              case (AC_LFAS_FREE)
                                    call u_chai2013_free_x(YEps, YURho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XURho, XCDef%AsympVxcOmega, &
                                          ONE/TWO, ONE-EXXFrac, System)
                              case (AC_LFAS_v2_FREE)                                          
                                    call u_chai2013_free_x(YEps, YURho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XURho, XCDef%AsympVxcOmega, &
                                          ONE, ONE-EXXFrac, System)
                              end select
                              ! ------------------------------------------------------------------
                              ! Slater potential computed according to Eq. 22 in
                              ! Della Sala, F. and Gorling, A. J. Chem. Phys. 115, 5718 (2001);
                              ! doi: 10.1063/1.1398093
                              ! ------------------------------------------------------------------
                              if (XCDef%SlaterVxc) then
                                    call slater_YU_Sphere(Exc, YURho, RhoEff, XURho, XOrb, &
                                          XShells, XNShells, XWeights, NSpher, DeltaK, AOBasis)
                              end if
                              ! ------------------------------------------------------------------
                              ! Contributions to the exchange-correlation matrix and
                              ! the exchange-correlation energy
                              ! ------------------------------------------------------------------
                              if (.not. xc_mgga) then
                                    !
                                    ! Update XC matrix with the values computed at each point in the batch.
                                    !
                                    call ks_UVxc_GGA_Sphere(Txc(:, :, ThisThread), exc, Nel, Div, XURho, XGradA, XGradB, &
                                          XOrb, XShells, XNShells, XWeights, YEps, YURho, YUSig, NSpher, &
                                          DeltaK, AOBasis, ShellPairLoc)
                              else

                                    call ks_UVxc_MGGA_Sphere(Txc(:, :, ThisThread), exc, Nel, Div, Kin, Lap, &
                                          XURho, XGradA, XGradB, XULap, XUTau, XOrb, XShells, XNShells, XWeights, &
                                          YEps, YURho, YUSig, YULap, YUTau, NSpher, DeltaK, AOBasis, ShellPairLoc)
                              end if
                        end if
                        !
                        ! Additional, non-XC integrals on the molecular grid. 
                        !
                        if (auxiliary_integral) then
                              if (aux_mgga) then
                                    !
                                    ! Generic integral of a function defined on the molecular grid.
                                    ! Inputs to this function are the same as in a meta-GGA functional.
                                    ! This is not exchange-correlation energy.
                                    !
                                    call umgga_auxint(Taux(:, ThisThread), aux_id, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XWeights, &
                                          XURho, XUSig, XULap, &
                                          XUTau, XOrb, XShells, &
                                          XNShells, DeltaK)
                              else
                                    call ugga_auxint(Taux(:, ThisThread), aux_id, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XWeights, XURho, &
                                          AUXIn, AtomElementMap, AOBasis, System)             
                              end if
                        end if
                  else
                        !
                        ! Spin-compensated electronic density
                        !
                        if (AOBasis%NAtoms > 1) then
                              if (.not. mgga_ingredients) then
                                    call ks_X_GGA_Sphere(XRho, XGrad, XSig, XOrb, XShells, XNShells, XWeights, Rho, &
                                          k, X0, Y0, Z0, R, RWeight, GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), &
                                          GRD_LEBZ(s0:s1), GRD_LEBW(s0:s1), NSpher, AOBasis, System)
                              else
                                    call ks_X_MGGA_Sphere(XRho, XGrad, XSig, XLap, XTau, XOrb, XShells, XNShells, XWeights, &
                                          Rho, k, X0, Y0, Z0, R, RWeight, GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), &
                                          GRD_LEBZ(s0:s1), GRD_LEBW(s0:s1), NSpher, AOBasis, System)
                              end if
                        else
                              if (.not. mgga_ingredients) then
                                    call ks_X_GGA_Sphere_Atom(XRho, XGrad, XSig, XOrb, XShells, XNShells, XWeights, Rho, &
                                          X0, Y0, Z0, R, RWeight, GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), &
                                          GRD_LEBZ(s0:s1), GRD_LEBW(s0:s1), NSpher, AOBasis)
                              else
                                    call ks_X_MGGA_Sphere_Atom(XRho, XGrad, XSig, XLap, XTau, XOrb, XShells, XNShells, XWeights, &
                                          Rho, X0, Y0, Z0, R, RWeight, GRD_LEBX(s0:s1), GRD_LEBY(s0:s1), &
                                          GRD_LEBZ(s0:s1), GRD_LEBW(s0:s1), NSpher, AOBasis)
                              end if
                        end if
                        if (exchange_correlation) then
                              ! ------------------------------------------------------------------
                              ! Exchange-correlation energy density & derivatives for all grid
                              ! points in the batch
                              ! ------------------------------------------------------------------
                              if (.not. xc_mgga) then
                                    call ks_Y_GGA_Sphere(YEps, YRho, YSig, XRho, XSig, XCDef, NSpher)
                              else
                                    call ks_Y_MGGA_Sphere(YEps, YRho, YSig, YLap, YTau, XRho, XSig, XLap, XTau, XCDef, NSpher)
                              end if
                              ! ------------------------------------------------------------------
                              ! Asymptotic correction to the exchange-correlation potential and
                              ! its contribution to the energy density
                              ! ------------------------------------------------------------------
                              select case(XCDef%AsympVxc)
                              case (AC_LFAS)
                                    call chai2013_x(YEps, YRho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XRho, XCDef%AsympVxcOmega, &
                                          ONE/TWO, ONE-EXXFrac, AtomElementMap, AUXIn, System, AOBasis)
                              case (AC_LFAS_v2)
                                    call chai2013_x(YEps, YRho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XRho, XCDef%AsympVxcOmega, &
                                          ONE, ONE-EXXFrac, AtomElementMap, AUXIn, System, AOBasis)
                              case (AC_LFAS_FREE)
                                    call chai2013_free_x(YEps, YRho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XRho, XCDef%AsympVxcOmega, &
                                          ONE/TWO, ONE-EXXFrac, System)
                              case (AC_LFAS_v2_FREE)                                          
                                    call chai2013_free_x(YEps, YRho, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XRho, XCDef%AsympVxcOmega, &
                                          ONE, ONE-EXXFrac, System)
                              end select
                              ! ------------------------------------------------------------------
                              ! Slater potential computed according to Eq. 22 in
                              ! Della Sala, F. and Gorling, A. J. Chem. Phys. 115, 5718 (2001);
                              ! doi: 10.1063/1.1398093
                              ! ------------------------------------------------------------------
                              if (XCDef%SlaterVxc) then
                                    call slater_Y_Sphere(Exc, YRho, RhoEff(:, :, 1), XRho, XOrb, &
                                          XShells, XNShells, XWeights, NSpher, DeltaK, AOBasis)
                              end if
                              ! ------------------------------------------------------------------
                              ! Contributions to the exchange-correlation matrix and
                              ! the exchange-correlation energy
                              ! ------------------------------------------------------------------
                              if (.not. xc_mgga) then
                                    call ks_Vxc_GGA_Sphere(Txc(:, :, ThisThread), exc, Nel, Div, XRho, XGrad, XOrb, &
                                          XShells, XNShells, XWeights, YEps, YRho, YSig, NSpher, DeltaK, AOBasis, &
                                          ShellPairLoc)
                              else
                                    call ks_Vxc_MGGA_Sphere(Txc(:, :, ThisThread), exc, Nel, Div, Kin, Lap, &
                                          XRho, XGrad, XLap, XTau, XOrb, XShells, XNShells, XWeights, &
                                          YEps, YRho, YSig, YLap, YTau, NSpher, DeltaK, AOBasis, ShellPairLoc)
                              end if
                        end if
                        if (auxiliary_integral) then
                              if (aux_mgga) then
                                    call mgga_auxint(Taux(:, ThisThread), aux_id, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XWeights, &
                                          XRho, XSig, XLap, XTau, XOrb, XShells, XNShells, DeltaK)
                              else                                          
                                    call gga_auxint(Taux(:, ThisThread), aux_id, X0, Y0, Z0, R, GRD_LEBX(s0:s1), &
                                          GRD_LEBY(s0:s1), GRD_LEBZ(s0:s1), NSpher, XWeights, XRho, &
                                          AUXIn, AtomElementMap, AOBasis, System)
                              end if
                        end if
                  end if
            end do
            !$omp end do
            !
            ! Explicit deallocation is needed here because implicit deallocation
            ! is not guaranteed to work inside parallel blocks.
            !
            deallocate(XNShells, XShells, XWeights)
            deallocate(YEps)
            if (SpinUnres) then
                  deallocate(XURho, XGradA, XGradB, XUSig, YURho, YUSig)
            else
                  deallocate(XRho, XGrad, XSig, YRho, YSig)
            end if
            if (mgga_ingredients) then
                  deallocate(XOrb)
                  if (SpinUnres) then
                        deallocate(XULap, XUTau, YULap, YUTau)
                  else
                        deallocate(XLap, XTau, YLap, YTau)
                  end if
            else
                  deallocate(XOrb)
            end if
            !$omp end parallel
            do k = 2, NThreads
                  Txc(:, :, 1) = Txc(:, :, 1) + Txc(:, :, k)
            end do
            call ks_UpdateVxc(Vxc, Txc(:, :, 1), ShellPairLoc, &
                  AOBasis%ShellLocCart, &
                  AOBasis%NAngFuncCart, &
                  AOBasis%ShellParamsIdx, &
                  AOBasis%NShells)
            if (auxiliary_integral) then
                  do k = 2, NThreads
                        Taux(:, 1) = Taux(:, 1) + Taux(:, k)
                  end do
                  aux(1:nauxint) = aux(1:nauxint) + Taux(:, 1)
            end if
      end subroutine ks_MainLoop


      subroutine ks_becke(i, rig, gx, gy, gz, w, AtomCoords, SortedDistances, SortedDistancesIdx, NAtoms)
            integer, intent(in)                    :: i
            real(F64), intent(in)                  :: rig
            real(F64), intent(in)                  :: gx, gy, gz
            real(F64), intent(out)                 :: w
            real(F64), dimension(:, :), intent(in) :: AtomCoords
            real(F64), dimension(:, :), intent(in) :: SortedDistances
            integer, dimension(:, :), intent(in)   :: SortedDistancesIdx
            integer, intent(in)                    :: NAtoms

            real(F64) :: rik, rkg
            real(F64) :: mu, s
            real(F64), dimension(3) :: rk
            integer :: k, idk
            real(F64) :: r
            real(F64), parameter :: const1 = TWO / (ONE - becka)

            r = const1 * rig
            w = ONE
            !
            ! Loop over all grid centers
            !
            atomloop: do k = 2, NAtoms
                  rik = SortedDistances(k, i)
                  idk = SortedDistancesIdx(k, i)
                  rk = AtomCoords(:, idk)
                  rkg = sqrt((gx - rk(1))**2 + (gy - rk(2))**2 + (gz - rk(3))**2)
                  mu = (rig - rkg) / (becka * rik)

                  if (mu <= -ONE) then
                        if (rik >= r) then
                              exit atomloop
                        else
                              cycle atomloop
                        end if
                  else if (mu >= ONE) then
                        w = ZERO
                        exit atomloop
                  else
                        call ks_step_function(mu, s)
                        w = w * s
                  end if
            end do atomloop
      end subroutine ks_becke


      pure subroutine ks_step_function(mu, s)
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
      end subroutine ks_step_function


      subroutine ks_Vxc(Vxc, ExcDFT, GridDiag, auxint, Txc, Rho, RhoEff, XCDef, AUXIn, AOBasis, System, GridKind, GridPruning)
            real(F64), dimension(:, :, :), intent(inout) :: Vxc
            real(F64), intent(out)                       :: ExcDFT
            type(TGridDiag), intent(out)                 :: GridDiag
            real(F64), dimension(:), intent(out)         :: auxint
            real(F64), dimension(:, :, :), intent(out)   :: Txc
            real(F64), dimension(:, :, :), intent(in)    :: Rho
            real(F64), dimension(:, :, :), intent(in)    :: RhoEff
            type(TXCDef), intent(in)                     :: XCDef
            real(F64), dimension(:, :), intent(in)       :: AUXIn
            type(TAOBasis), intent(in)                   :: AOBasis
            type(TSystem), intent(in)                    :: System
            integer, intent(in)                          :: GridKind
            logical, intent(in)                          :: GridPruning

            integer :: NSpins, NImages, ThisImage
            integer :: aux_id, xcf_id
            real(F64) :: Nel, Div, Kin, Lap, Exc
            integer, dimension(:), allocatable :: ZList, ZCount, AtomElementMap
            real(F64), dimension(:, :), allocatable :: RhoSpher
            integer :: MaxNShells
            integer :: NElements

            NImages = num_images()
            ThisImage = this_image()
            NSpins = size(Rho, dim=3)
            aux_id = aux_get_id(XCDef)
            xcf_id = xcf_get_id(XCDef)
            if (aux_id == AUX_HIRSHFELD_VOLUME_FREE) then
                  MaxNShells = AOBasis%MaxNShells
                  allocate(RhoSpher(((MaxNShells+1)*MaxNShells)/2, 1))
                  call RhoSpherCoeffs(RhoSpher(:, 1), Rho, AOBasis)
            else
                  if (NSpins > 1) then
                        call aux_setauxint(aux_id, xcf_isuncomp(XCDef), Rho(:, :, 1), Rho(:, :, 2), AUXIn)
                  else
                        call aux_setauxint(aux_id, xcf_isuncomp(XCDef), Rho(:, :, 1), Rho(:, :, 1), AUXIn)
                  end if
            end if
            Exc = ZERO
            Nel = ZERO
            Div = ZERO
            Kin = ZERO
            Lap = ZERO
            if (aux_id /= AUX_NONE) then
                  auxint = ZERO
            end if
            allocate(AtomElementMap(System%NAtoms))
            call sys_ElementsList(ZList, ZCount, AtomElementMap, NElements, System, SYS_ALL_ATOMS)
            !
            ! Initialize the molecular grid
            !
            call grid_set(GridKind, GridPruning)
            if (aux_id /= AUX_HIRSHFELD_VOLUME_FREE) then
                  call ks_MainLoop(Vxc, Exc, Nel, Div, Kin, Lap, auxint, Txc, Rho, RhoEff, XCDef, &
                        AUXIn, AtomElementMap, AOBasis, System, GridKind)
            else
                  call ks_MainLoop(Vxc, Exc, Nel, Div, Kin, Lap, auxint, Txc, Rho, RhoEff, XCDef, &
                        RhoSpher, AtomElementMap, AOBasis, System, GridKind)
            end if
            if (ThisImage == 1 .and. ( &
                  XCDef%AsympVxc == AC_LFAS &
                  .or. XCDef%AsympVxc == AC_LFAS_v2 &
                  .or. XCDef%AsympVxc == AC_LFAS_FREE &
                  .or. XCDef%AsympVxc == AC_LFAS_v2_FREE)) then
                  !
                  ! The double counting contribution of the localized Fermi-Amaldi
                  ! asympotic correction:
                  ! Eqs. 11 and 12 in Chi-Ruei Pan, Po-Tung Fang, and Jeng-Da Chai,
                  ! Phys. Rev. A 87, 052510 (2013);
                  ! doi: 10.1103/PhysRevA.87.052510
                  !
                  select case (XCDef%AsympVxc)
                  case (AC_LFAS, AC_LFAS_FREE)
                        Exc = Exc + (ONE-XCDef%EXX) * XCDef%AsympVxcOmega / Sqrt(Pi) * System%NElectrons
                  case (AC_LFAS_v2, AC_LFAS_v2_FREE)
                        Exc = Exc + (ONE-XCDef%EXX) * TWO * XCDef%AsympVxcOmega / Sqrt(Pi) * System%NElectrons
                  end select
            end if
            if (NImages > 1) then
                  call co_sum(Nel)
                  call co_sum(Div)
                  call co_sum(Kin)
                  call co_sum(Lap)
                  if (aux_id /= AUX_NONE) then
                        call co_sum(auxint)
                  end if
            end if
            ExcDFT = Exc
            GridDiag%Nel = Nel
            GridDiag%Div = Div
            GridDiag%Kin = Kin
            GridDiag%Lap = Lap
      end subroutine ks_Vxc
end module KohnSham
