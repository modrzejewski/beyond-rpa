module density_exc_exc_functions_so_pt0123_asym

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none
        !
    ! File generated automatically on 2017-12-29 16:50:56
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_asym_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_asym_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_asym_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_12_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_asym_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_asym_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_asym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_asym_pt1

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_24_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_25_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_26_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_33_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_45_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_52_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_53_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_54_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_56_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_58_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_59_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_60_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_61_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_63_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_65_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_66_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_67_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_68_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_70_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_73_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_74_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_75_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_83_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_84_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_85_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_87_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_88_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_92_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_93_so_asym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_94_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_95_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_96_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_97_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_98_so_asym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_99_so_asym_pt2

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_27_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_30_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_35_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_49_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_50_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_52_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_61_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_65_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_67_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_68_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_75_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_78_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_80_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_81_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_82_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_84_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_85_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_89_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_90_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_91_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_92_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_93_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_94_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_95_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_96_so_asym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_97_so_asym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_98_so_asym_pt3 



  contains
    
    subroutine wm_so_asym_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_so_asym_intermediates_ccsd_init_pt0
    
    subroutine wm_so_asym_intermediates_ccsd_free_pt0
    
    end subroutine wm_so_asym_intermediates_ccsd_free_pt0
    
    subroutine wm_so_asym_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

  end subroutine wm_so_asym_intermediates_ccsd_pt0

     subroutine wm_so_asym_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_asym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_so_asym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_so_asym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_so_asym_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_asym_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_6_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_7_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_8_so_asym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_so_asym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_so_asym_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_12_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_13_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_14_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_15_so_asym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_16_so_asym_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_asym_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_asym_pt1(nocc+1: nactive, 1: nocc))
wm_interm_0_so_asym_pt1 = zero 
wm_interm_1_so_asym_pt1 = zero 
wm_interm_2_so_asym_pt1 = zero 
wm_interm_3_so_asym_pt1 = zero 
wm_interm_4_so_asym_pt1 = zero 
wm_interm_5_so_asym_pt1 = zero 
wm_interm_6_so_asym_pt1 = zero 
wm_interm_7_so_asym_pt1 = zero 
wm_interm_8_so_asym_pt1 = zero 
wm_interm_9_so_asym_pt1 = zero 
wm_interm_10_so_asym_pt1 = zero 
wm_interm_11_so_asym_pt1 = zero 
wm_interm_12_so_asym_pt1 = zero 
wm_interm_13_so_asym_pt1 = zero 
wm_interm_14_so_asym_pt1 = zero 
wm_interm_15_so_asym_pt1 = zero 
wm_interm_16_so_asym_pt1 = zero 
wm_interm_17_so_asym_pt1 = zero 
wm_interm_18_so_asym_pt1 = zero 

    end subroutine wm_so_asym_intermediates_ccsd_init_pt1
    
    subroutine wm_so_asym_intermediates_ccsd_free_pt1
    deallocate(wm_interm_0_so_asym_pt1)
deallocate(wm_interm_1_so_asym_pt1)
deallocate(wm_interm_2_so_asym_pt1)
deallocate(wm_interm_3_so_asym_pt1)
deallocate(wm_interm_4_so_asym_pt1)
deallocate(wm_interm_5_so_asym_pt1)
deallocate(wm_interm_6_so_asym_pt1)
deallocate(wm_interm_7_so_asym_pt1)
deallocate(wm_interm_8_so_asym_pt1)
deallocate(wm_interm_9_so_asym_pt1)
deallocate(wm_interm_10_so_asym_pt1)
deallocate(wm_interm_11_so_asym_pt1)
deallocate(wm_interm_12_so_asym_pt1)
deallocate(wm_interm_13_so_asym_pt1)
deallocate(wm_interm_14_so_asym_pt1)
deallocate(wm_interm_15_so_asym_pt1)
deallocate(wm_interm_16_so_asym_pt1)
deallocate(wm_interm_17_so_asym_pt1)
deallocate(wm_interm_18_so_asym_pt1)

    end subroutine wm_so_asym_intermediates_ccsd_free_pt1
    
    subroutine wm_so_asym_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, k 

    !$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
wm_interm_0_so_asym_pt1(b, j) = wm_interm_0_so_asym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_1_so_asym_pt1(b, j) = wm_interm_1_so_asym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_2_so_asym_pt1(b, j) = wm_interm_2_so_asym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_3_so_asym_pt1(b, i, j, k) = wm_interm_3_so_asym_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,k)
end do 
wm_interm_4_so_asym_pt1(b, i, j, k) = wm_interm_4_so_asym_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_5_so_asym_pt1(j, k) = wm_interm_5_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_6_so_asym_pt1(j, k) = wm_interm_6_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_7_so_asym_pt1(j, k) = wm_interm_7_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_8_so_asym_pt1(b, j) = wm_interm_8_so_asym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_9_so_asym_pt1(b, j) = wm_interm_9_so_asym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_10_so_asym_pt1(b, i, j, k) = wm_interm_10_so_asym_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_11_so_asym_pt1(j, k) = wm_interm_11_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_12_so_asym_pt1(j, k) = wm_interm_12_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_13_so_asym_pt1(j, k) = wm_interm_13_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_14_so_asym_pt1(j, k) = wm_interm_14_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_15_so_asym_pt1(j, k) = wm_interm_15_so_asym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_16_so_asym_pt1(b, j, i, k) = wm_interm_16_so_asym_pt1(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_17_so_asym_pt1(b, i, j, k) = wm_interm_17_so_asym_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_18_so_asym_pt1(b, j) = wm_interm_18_so_asym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_so_asym_intermediates_ccsd_pt1

    subroutine wm_so_asym_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_6_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_8_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_9_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_10_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_14_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_16_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_19_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_20_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_25_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_26_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_27_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_30_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_31_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_34_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_35_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_36_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_39_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_40_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_41_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_43_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_44_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_45_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_46_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_50_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_52_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_55_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_56_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_57_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_59_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_60_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_61_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_62_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_so_asym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_67_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_68_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_69_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_70_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_71_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_72_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_73_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_74_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_75_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_76_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_77_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_78_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_79_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_80_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_81_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_82_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_83_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_84_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_85_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_86_so_asym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_87_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_88_so_asym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_89_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_90_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_91_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_92_so_asym_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_93_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_94_so_asym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_95_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_96_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_97_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_98_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_99_so_asym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_so_asym_pt2 = zero 
wm_interm_1_so_asym_pt2 = zero 
wm_interm_2_so_asym_pt2 = zero 
wm_interm_3_so_asym_pt2 = zero 
wm_interm_4_so_asym_pt2 = zero 
wm_interm_5_so_asym_pt2 = zero 
wm_interm_6_so_asym_pt2 = zero 
wm_interm_7_so_asym_pt2 = zero 
wm_interm_8_so_asym_pt2 = zero 
wm_interm_9_so_asym_pt2 = zero 
wm_interm_10_so_asym_pt2 = zero 
wm_interm_11_so_asym_pt2 = zero 
wm_interm_12_so_asym_pt2 = zero 
wm_interm_13_so_asym_pt2 = zero 
wm_interm_14_so_asym_pt2 = zero 
wm_interm_15_so_asym_pt2 = zero 
wm_interm_16_so_asym_pt2 = zero 
wm_interm_17_so_asym_pt2 = zero 
wm_interm_18_so_asym_pt2 = zero 
wm_interm_19_so_asym_pt2 = zero 
wm_interm_20_so_asym_pt2 = zero 
wm_interm_21_so_asym_pt2 = zero 
wm_interm_22_so_asym_pt2 = zero 
wm_interm_23_so_asym_pt2 = zero 
wm_interm_24_so_asym_pt2 = zero 
wm_interm_25_so_asym_pt2 = zero 
wm_interm_26_so_asym_pt2 = zero 
wm_interm_27_so_asym_pt2 = zero 
wm_interm_28_so_asym_pt2 = zero 
wm_interm_29_so_asym_pt2 = zero 
wm_interm_30_so_asym_pt2 = zero 
wm_interm_31_so_asym_pt2 = zero 
wm_interm_32_so_asym_pt2 = zero 
wm_interm_33_so_asym_pt2 = zero 
wm_interm_34_so_asym_pt2 = zero 
wm_interm_35_so_asym_pt2 = zero 
wm_interm_36_so_asym_pt2 = zero 
wm_interm_37_so_asym_pt2 = zero 
wm_interm_38_so_asym_pt2 = zero 
wm_interm_39_so_asym_pt2 = zero 
wm_interm_40_so_asym_pt2 = zero 
wm_interm_41_so_asym_pt2 = zero 
wm_interm_42_so_asym_pt2 = zero 
wm_interm_43_so_asym_pt2 = zero 
wm_interm_44_so_asym_pt2 = zero 
wm_interm_45_so_asym_pt2 = zero 
wm_interm_46_so_asym_pt2 = zero 
wm_interm_47_so_asym_pt2 = zero 
wm_interm_48_so_asym_pt2 = zero 
wm_interm_49_so_asym_pt2 = zero 
wm_interm_50_so_asym_pt2 = zero 
wm_interm_51_so_asym_pt2 = zero 
wm_interm_52_so_asym_pt2 = zero 
wm_interm_53_so_asym_pt2 = zero 
wm_interm_54_so_asym_pt2 = zero 
wm_interm_55_so_asym_pt2 = zero 
wm_interm_56_so_asym_pt2 = zero 
wm_interm_57_so_asym_pt2 = zero 
wm_interm_58_so_asym_pt2 = zero 
wm_interm_59_so_asym_pt2 = zero 
wm_interm_60_so_asym_pt2 = zero 
wm_interm_61_so_asym_pt2 = zero 
wm_interm_62_so_asym_pt2 = zero 
wm_interm_63_so_asym_pt2 = zero 
wm_interm_64_so_asym_pt2 = zero 
wm_interm_65_so_asym_pt2 = zero 
wm_interm_66_so_asym_pt2 = zero 
wm_interm_67_so_asym_pt2 = zero 
wm_interm_68_so_asym_pt2 = zero 
wm_interm_69_so_asym_pt2 = zero 
wm_interm_70_so_asym_pt2 = zero 
wm_interm_71_so_asym_pt2 = zero 
wm_interm_72_so_asym_pt2 = zero 
wm_interm_73_so_asym_pt2 = zero 
wm_interm_74_so_asym_pt2 = zero 
wm_interm_75_so_asym_pt2 = zero 
wm_interm_76_so_asym_pt2 = zero 
wm_interm_77_so_asym_pt2 = zero 
wm_interm_78_so_asym_pt2 = zero 
wm_interm_79_so_asym_pt2 = zero 
wm_interm_80_so_asym_pt2 = zero 
wm_interm_81_so_asym_pt2 = zero 
wm_interm_82_so_asym_pt2 = zero 
wm_interm_83_so_asym_pt2 = zero 
wm_interm_84_so_asym_pt2 = zero 
wm_interm_85_so_asym_pt2 = zero 
wm_interm_86_so_asym_pt2 = zero 
wm_interm_87_so_asym_pt2 = zero 
wm_interm_88_so_asym_pt2 = zero 
wm_interm_89_so_asym_pt2 = zero 
wm_interm_90_so_asym_pt2 = zero 
wm_interm_91_so_asym_pt2 = zero 
wm_interm_92_so_asym_pt2 = zero 
wm_interm_93_so_asym_pt2 = zero 
wm_interm_94_so_asym_pt2 = zero 
wm_interm_95_so_asym_pt2 = zero 
wm_interm_96_so_asym_pt2 = zero 
wm_interm_97_so_asym_pt2 = zero 
wm_interm_98_so_asym_pt2 = zero 
wm_interm_99_so_asym_pt2 = zero 

    end subroutine wm_so_asym_intermediates_ccsd_init_pt2
    
    subroutine wm_so_asym_intermediates_ccsd_free_pt2
    deallocate(wm_interm_0_so_asym_pt2)
deallocate(wm_interm_1_so_asym_pt2)
deallocate(wm_interm_2_so_asym_pt2)
deallocate(wm_interm_3_so_asym_pt2)
deallocate(wm_interm_4_so_asym_pt2)
deallocate(wm_interm_5_so_asym_pt2)
deallocate(wm_interm_6_so_asym_pt2)
deallocate(wm_interm_7_so_asym_pt2)
deallocate(wm_interm_8_so_asym_pt2)
deallocate(wm_interm_9_so_asym_pt2)
deallocate(wm_interm_10_so_asym_pt2)
deallocate(wm_interm_11_so_asym_pt2)
deallocate(wm_interm_12_so_asym_pt2)
deallocate(wm_interm_13_so_asym_pt2)
deallocate(wm_interm_14_so_asym_pt2)
deallocate(wm_interm_15_so_asym_pt2)
deallocate(wm_interm_16_so_asym_pt2)
deallocate(wm_interm_17_so_asym_pt2)
deallocate(wm_interm_18_so_asym_pt2)
deallocate(wm_interm_19_so_asym_pt2)
deallocate(wm_interm_20_so_asym_pt2)
deallocate(wm_interm_21_so_asym_pt2)
deallocate(wm_interm_22_so_asym_pt2)
deallocate(wm_interm_23_so_asym_pt2)
deallocate(wm_interm_24_so_asym_pt2)
deallocate(wm_interm_25_so_asym_pt2)
deallocate(wm_interm_26_so_asym_pt2)
deallocate(wm_interm_27_so_asym_pt2)
deallocate(wm_interm_28_so_asym_pt2)
deallocate(wm_interm_29_so_asym_pt2)
deallocate(wm_interm_30_so_asym_pt2)
deallocate(wm_interm_31_so_asym_pt2)
deallocate(wm_interm_32_so_asym_pt2)
deallocate(wm_interm_33_so_asym_pt2)
deallocate(wm_interm_34_so_asym_pt2)
deallocate(wm_interm_35_so_asym_pt2)
deallocate(wm_interm_36_so_asym_pt2)
deallocate(wm_interm_37_so_asym_pt2)
deallocate(wm_interm_38_so_asym_pt2)
deallocate(wm_interm_39_so_asym_pt2)
deallocate(wm_interm_40_so_asym_pt2)
deallocate(wm_interm_41_so_asym_pt2)
deallocate(wm_interm_42_so_asym_pt2)
deallocate(wm_interm_43_so_asym_pt2)
deallocate(wm_interm_44_so_asym_pt2)
deallocate(wm_interm_45_so_asym_pt2)
deallocate(wm_interm_46_so_asym_pt2)
deallocate(wm_interm_47_so_asym_pt2)
deallocate(wm_interm_48_so_asym_pt2)
deallocate(wm_interm_49_so_asym_pt2)
deallocate(wm_interm_50_so_asym_pt2)
deallocate(wm_interm_51_so_asym_pt2)
deallocate(wm_interm_52_so_asym_pt2)
deallocate(wm_interm_53_so_asym_pt2)
deallocate(wm_interm_54_so_asym_pt2)
deallocate(wm_interm_55_so_asym_pt2)
deallocate(wm_interm_56_so_asym_pt2)
deallocate(wm_interm_57_so_asym_pt2)
deallocate(wm_interm_58_so_asym_pt2)
deallocate(wm_interm_59_so_asym_pt2)
deallocate(wm_interm_60_so_asym_pt2)
deallocate(wm_interm_61_so_asym_pt2)
deallocate(wm_interm_62_so_asym_pt2)
deallocate(wm_interm_63_so_asym_pt2)
deallocate(wm_interm_64_so_asym_pt2)
deallocate(wm_interm_65_so_asym_pt2)
deallocate(wm_interm_66_so_asym_pt2)
deallocate(wm_interm_67_so_asym_pt2)
deallocate(wm_interm_68_so_asym_pt2)
deallocate(wm_interm_69_so_asym_pt2)
deallocate(wm_interm_70_so_asym_pt2)
deallocate(wm_interm_71_so_asym_pt2)
deallocate(wm_interm_72_so_asym_pt2)
deallocate(wm_interm_73_so_asym_pt2)
deallocate(wm_interm_74_so_asym_pt2)
deallocate(wm_interm_75_so_asym_pt2)
deallocate(wm_interm_76_so_asym_pt2)
deallocate(wm_interm_77_so_asym_pt2)
deallocate(wm_interm_78_so_asym_pt2)
deallocate(wm_interm_79_so_asym_pt2)
deallocate(wm_interm_80_so_asym_pt2)
deallocate(wm_interm_81_so_asym_pt2)
deallocate(wm_interm_82_so_asym_pt2)
deallocate(wm_interm_83_so_asym_pt2)
deallocate(wm_interm_84_so_asym_pt2)
deallocate(wm_interm_85_so_asym_pt2)
deallocate(wm_interm_86_so_asym_pt2)
deallocate(wm_interm_87_so_asym_pt2)
deallocate(wm_interm_88_so_asym_pt2)
deallocate(wm_interm_89_so_asym_pt2)
deallocate(wm_interm_90_so_asym_pt2)
deallocate(wm_interm_91_so_asym_pt2)
deallocate(wm_interm_92_so_asym_pt2)
deallocate(wm_interm_93_so_asym_pt2)
deallocate(wm_interm_94_so_asym_pt2)
deallocate(wm_interm_95_so_asym_pt2)
deallocate(wm_interm_96_so_asym_pt2)
deallocate(wm_interm_97_so_asym_pt2)
deallocate(wm_interm_98_so_asym_pt2)
deallocate(wm_interm_99_so_asym_pt2)

    end subroutine wm_so_asym_intermediates_ccsd_free_pt2
    
    subroutine wm_so_asym_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, k, c, l 

    !$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_0_so_asym_pt2(b, i, j, k) = wm_interm_0_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_1_so_asym_pt2(j, k) = wm_interm_1_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_2_so_asym_pt2(j, k) = wm_interm_2_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,k)
end do 
wm_interm_3_so_asym_pt2(b, i, j, k) = wm_interm_3_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_4_so_asym_pt2(b, c) = wm_interm_4_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_5_so_asym_pt2(b, j) = wm_interm_5_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_6_so_asym_pt2(b, c, j, k) = wm_interm_6_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_7_so_asym_pt2(b, c, j, k) = wm_interm_7_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_8_so_asym_pt2(b, c) = wm_interm_8_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_9_so_asym_pt2(b, c, j, k) = wm_interm_9_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_10_so_asym_pt2(b, c, j, k) = wm_interm_10_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_11_so_asym_pt2(b, c, j, k) = wm_interm_11_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_12_so_asym_pt2(b, c, j, k) = wm_interm_12_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_13_so_asym_pt2(b, j) = wm_interm_13_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_14_so_asym_pt2(b, i, j, k) = wm_interm_14_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_15_so_asym_pt2(b, c, j, k) = wm_interm_15_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_16_so_asym_pt2(b, c, j, k) = wm_interm_16_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_17_so_asym_pt2(b, c, j, k) = wm_interm_17_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_18_so_asym_pt2(b, c, j, k) = wm_interm_18_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
wm_interm_19_so_asym_pt2(b, j) = wm_interm_19_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_20_so_asym_pt2(b, j) = wm_interm_20_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_21_so_asym_pt2(b, i, j, k) = wm_interm_21_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_22_so_asym_pt2(i, j, k, l) = wm_interm_22_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_23_so_asym_pt2(b, c) = wm_interm_23_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_24_so_asym_pt2(b, c) = wm_interm_24_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_25_so_asym_pt2(b, j) = wm_interm_25_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_26_so_asym_pt2(b, c) = wm_interm_26_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_27_so_asym_pt2(b, c, j, k) = wm_interm_27_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_28_so_asym_pt2(b, c, j, k) = wm_interm_28_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_29_so_asym_pt2(b, c, j, k) = wm_interm_29_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_30_so_asym_pt2(b, c, j, k) = wm_interm_30_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_31_so_asym_pt2(b, c, j, k) = wm_interm_31_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_32_so_asym_pt2(i, j, k, l) = wm_interm_32_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_33_so_asym_pt2(j, k) = wm_interm_33_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_34_so_asym_pt2(j, k) = wm_interm_34_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_35_so_asym_pt2(j, k) = wm_interm_35_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_36_so_asym_pt2(b, j) = wm_interm_36_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_37_so_asym_pt2(b, j) = wm_interm_37_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_38_so_asym_pt2(b, c) = wm_interm_38_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_39_so_asym_pt2(b, c) = wm_interm_39_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_40_so_asym_pt2(b, c) = wm_interm_40_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_41_so_asym_pt2(i, j, k, l) = wm_interm_41_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_42_so_asym_pt2(j, k) = wm_interm_42_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_43_so_asym_pt2(j, k) = wm_interm_43_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_44_so_asym_pt2(j, k) = wm_interm_44_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_45_so_asym_pt2(b, j) = wm_interm_45_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_46_so_asym_pt2(b, i, j, k) = wm_interm_46_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_47_so_asym_pt2(b, c, j, k) = wm_interm_47_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
end do 
wm_interm_48_so_asym_pt2(b, i, j, k) = wm_interm_48_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_49_so_asym_pt2(b, c, j, k) = wm_interm_49_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_50_so_asym_pt2(b, j, i, k) = wm_interm_50_so_asym_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_51_so_asym_pt2(b, c, j, k) = wm_interm_51_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
end do 
end do 
wm_interm_52_so_asym_pt2(b, j) = wm_interm_52_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_53_so_asym_pt2(b, c) = wm_interm_53_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_54_so_asym_pt2(b, c) = wm_interm_54_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_55_so_asym_pt2(b, c, j, k) = wm_interm_55_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_56_so_asym_pt2(j, k) = wm_interm_56_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_57_so_asym_pt2(i, j, k, l) = wm_interm_57_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_58_so_asym_pt2(j, k) = wm_interm_58_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
end do 
end do 
wm_interm_59_so_asym_pt2(b, j) = wm_interm_59_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,i)
end do 
end do 
wm_interm_60_so_asym_pt2(b, j) = wm_interm_60_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_61_so_asym_pt2(b, j) = wm_interm_61_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_62_so_asym_pt2(b, j) = wm_interm_62_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_63_so_asym_pt2(b, j) = wm_interm_63_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_64_so_asym_pt2(b, j) = wm_interm_64_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_65_so_asym_pt2(b, j) = wm_interm_65_so_asym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_66_so_asym_pt2(i, j) = wm_interm_66_so_asym_pt2(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_67_so_asym_pt2(i, j) = wm_interm_67_so_asym_pt2(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t1(a,j)
end do 
wm_interm_68_so_asym_pt2(i, j) = wm_interm_68_so_asym_pt2(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j)
wm_interm_69_so_asym_pt2(a, b, i, j) = wm_interm_69_so_asym_pt2(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_70_so_asym_pt2(a, b) = wm_interm_70_so_asym_pt2(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_71_so_asym_pt2(i, j, k, l) = wm_interm_71_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,a,l)
end do 
end do 
wm_interm_72_so_asym_pt2(i, j, k, l) = wm_interm_72_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, b,k,a,i)
end do 
end do 
end do 
wm_interm_73_so_asym_pt2(j, k) = wm_interm_73_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_74_so_asym_pt2(j, k) = wm_interm_74_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_75_so_asym_pt2(j, k) = wm_interm_75_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_76_so_asym_pt2(b, c, j, k) = wm_interm_76_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_77_so_asym_pt2(b, c, j, k) = wm_interm_77_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_78_so_asym_pt2(b, c, j, k) = wm_interm_78_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_79_so_asym_pt2(b, c, j, k) = wm_interm_79_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_80_so_asym_pt2(b, c, j, k) = wm_interm_80_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_81_so_asym_pt2(b, c, j, k) = wm_interm_81_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_82_so_asym_pt2(b, c, j, k) = wm_interm_82_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,j,a,i)
end do 
end do 
end do 
wm_interm_83_so_asym_pt2(b, c) = wm_interm_83_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_84_so_asym_pt2(b, c) = wm_interm_84_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_85_so_asym_pt2(b, c) = wm_interm_85_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_86_so_asym_pt2(i, j, k, l) = wm_interm_86_so_asym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_87_so_asym_pt2(j, k) = wm_interm_87_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_88_so_asym_pt2(j, k) = wm_interm_88_so_asym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_89_so_asym_pt2(b, c, j, k) = wm_interm_89_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_90_so_asym_pt2(b, c, j, k) = wm_interm_90_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_91_so_asym_pt2(b, c, j, k) = wm_interm_91_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_92_so_asym_pt2(b, c, j, k) = wm_interm_92_so_asym_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_93_so_asym_pt2(b, c) = wm_interm_93_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_94_so_asym_pt2(b, c) = wm_interm_94_so_asym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_95_so_asym_pt2(b, i, j, k) = wm_interm_95_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
end do 
wm_interm_96_so_asym_pt2(b, i, j, k) = wm_interm_96_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_97_so_asym_pt2(b, j, i, k) = wm_interm_97_so_asym_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_98_so_asym_pt2(b, i, j, k) = wm_interm_98_so_asym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_99_so_asym_pt2(b, j, i, k) = wm_interm_99_so_asym_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_so_asym_intermediates_ccsd_pt2

    subroutine wm_so_asym_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_3_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_5_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_7_so_asym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_so_asym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_10_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_12_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_14_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_15_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_16_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_18_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_19_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_20_so_asym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_22_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_23_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_24_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_26_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_28_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_30_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_31_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_36_so_asym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_38_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_39_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_40_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_41_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_42_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_43_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_44_so_asym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_46_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_50_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_51_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_55_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_56_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_58_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_62_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_68_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_69_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_so_asym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_71_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_72_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_73_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_74_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_75_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_76_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_77_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_79_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_81_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_82_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_83_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_84_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_85_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_86_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_87_so_asym_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_88_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_90_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_91_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_92_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_93_so_asym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_94_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_95_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_96_so_asym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_97_so_asym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_98_so_asym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_so_asym_pt3 = zero 
wm_interm_1_so_asym_pt3 = zero 
wm_interm_2_so_asym_pt3 = zero 
wm_interm_3_so_asym_pt3 = zero 
wm_interm_4_so_asym_pt3 = zero 
wm_interm_5_so_asym_pt3 = zero 
wm_interm_6_so_asym_pt3 = zero 
wm_interm_7_so_asym_pt3 = zero 
wm_interm_8_so_asym_pt3 = zero 
wm_interm_9_so_asym_pt3 = zero 
wm_interm_10_so_asym_pt3 = zero 
wm_interm_11_so_asym_pt3 = zero 
wm_interm_12_so_asym_pt3 = zero 
wm_interm_13_so_asym_pt3 = zero 
wm_interm_14_so_asym_pt3 = zero 
wm_interm_15_so_asym_pt3 = zero 
wm_interm_16_so_asym_pt3 = zero 
wm_interm_17_so_asym_pt3 = zero 
wm_interm_18_so_asym_pt3 = zero 
wm_interm_19_so_asym_pt3 = zero 
wm_interm_20_so_asym_pt3 = zero 
wm_interm_21_so_asym_pt3 = zero 
wm_interm_22_so_asym_pt3 = zero 
wm_interm_23_so_asym_pt3 = zero 
wm_interm_24_so_asym_pt3 = zero 
wm_interm_25_so_asym_pt3 = zero 
wm_interm_26_so_asym_pt3 = zero 
wm_interm_27_so_asym_pt3 = zero 
wm_interm_28_so_asym_pt3 = zero 
wm_interm_29_so_asym_pt3 = zero 
wm_interm_30_so_asym_pt3 = zero 
wm_interm_31_so_asym_pt3 = zero 
wm_interm_32_so_asym_pt3 = zero 
wm_interm_33_so_asym_pt3 = zero 
wm_interm_34_so_asym_pt3 = zero 
wm_interm_35_so_asym_pt3 = zero 
wm_interm_36_so_asym_pt3 = zero 
wm_interm_37_so_asym_pt3 = zero 
wm_interm_38_so_asym_pt3 = zero 
wm_interm_39_so_asym_pt3 = zero 
wm_interm_40_so_asym_pt3 = zero 
wm_interm_41_so_asym_pt3 = zero 
wm_interm_42_so_asym_pt3 = zero 
wm_interm_43_so_asym_pt3 = zero 
wm_interm_44_so_asym_pt3 = zero 
wm_interm_45_so_asym_pt3 = zero 
wm_interm_46_so_asym_pt3 = zero 
wm_interm_47_so_asym_pt3 = zero 
wm_interm_48_so_asym_pt3 = zero 
wm_interm_49_so_asym_pt3 = zero 
wm_interm_50_so_asym_pt3 = zero 
wm_interm_51_so_asym_pt3 = zero 
wm_interm_52_so_asym_pt3 = zero 
wm_interm_53_so_asym_pt3 = zero 
wm_interm_54_so_asym_pt3 = zero 
wm_interm_55_so_asym_pt3 = zero 
wm_interm_56_so_asym_pt3 = zero 
wm_interm_57_so_asym_pt3 = zero 
wm_interm_58_so_asym_pt3 = zero 
wm_interm_59_so_asym_pt3 = zero 
wm_interm_60_so_asym_pt3 = zero 
wm_interm_61_so_asym_pt3 = zero 
wm_interm_62_so_asym_pt3 = zero 
wm_interm_63_so_asym_pt3 = zero 
wm_interm_64_so_asym_pt3 = zero 
wm_interm_65_so_asym_pt3 = zero 
wm_interm_66_so_asym_pt3 = zero 
wm_interm_67_so_asym_pt3 = zero 
wm_interm_68_so_asym_pt3 = zero 
wm_interm_69_so_asym_pt3 = zero 
wm_interm_70_so_asym_pt3 = zero 
wm_interm_71_so_asym_pt3 = zero 
wm_interm_72_so_asym_pt3 = zero 
wm_interm_73_so_asym_pt3 = zero 
wm_interm_74_so_asym_pt3 = zero 
wm_interm_75_so_asym_pt3 = zero 
wm_interm_76_so_asym_pt3 = zero 
wm_interm_77_so_asym_pt3 = zero 
wm_interm_78_so_asym_pt3 = zero 
wm_interm_79_so_asym_pt3 = zero 
wm_interm_80_so_asym_pt3 = zero 
wm_interm_81_so_asym_pt3 = zero 
wm_interm_82_so_asym_pt3 = zero 
wm_interm_83_so_asym_pt3 = zero 
wm_interm_84_so_asym_pt3 = zero 
wm_interm_85_so_asym_pt3 = zero 
wm_interm_86_so_asym_pt3 = zero 
wm_interm_87_so_asym_pt3 = zero 
wm_interm_88_so_asym_pt3 = zero 
wm_interm_89_so_asym_pt3 = zero 
wm_interm_90_so_asym_pt3 = zero 
wm_interm_91_so_asym_pt3 = zero 
wm_interm_92_so_asym_pt3 = zero 
wm_interm_93_so_asym_pt3 = zero 
wm_interm_94_so_asym_pt3 = zero 
wm_interm_95_so_asym_pt3 = zero 
wm_interm_96_so_asym_pt3 = zero 
wm_interm_97_so_asym_pt3 = zero 
wm_interm_98_so_asym_pt3 = zero 

    end subroutine wm_so_asym_intermediates_ccsd_init_pt3
    
    subroutine wm_so_asym_intermediates_ccsd_free_pt3
    deallocate(wm_interm_0_so_asym_pt3)
deallocate(wm_interm_1_so_asym_pt3)
deallocate(wm_interm_2_so_asym_pt3)
deallocate(wm_interm_3_so_asym_pt3)
deallocate(wm_interm_4_so_asym_pt3)
deallocate(wm_interm_5_so_asym_pt3)
deallocate(wm_interm_6_so_asym_pt3)
deallocate(wm_interm_7_so_asym_pt3)
deallocate(wm_interm_8_so_asym_pt3)
deallocate(wm_interm_9_so_asym_pt3)
deallocate(wm_interm_10_so_asym_pt3)
deallocate(wm_interm_11_so_asym_pt3)
deallocate(wm_interm_12_so_asym_pt3)
deallocate(wm_interm_13_so_asym_pt3)
deallocate(wm_interm_14_so_asym_pt3)
deallocate(wm_interm_15_so_asym_pt3)
deallocate(wm_interm_16_so_asym_pt3)
deallocate(wm_interm_17_so_asym_pt3)
deallocate(wm_interm_18_so_asym_pt3)
deallocate(wm_interm_19_so_asym_pt3)
deallocate(wm_interm_20_so_asym_pt3)
deallocate(wm_interm_21_so_asym_pt3)
deallocate(wm_interm_22_so_asym_pt3)
deallocate(wm_interm_23_so_asym_pt3)
deallocate(wm_interm_24_so_asym_pt3)
deallocate(wm_interm_25_so_asym_pt3)
deallocate(wm_interm_26_so_asym_pt3)
deallocate(wm_interm_27_so_asym_pt3)
deallocate(wm_interm_28_so_asym_pt3)
deallocate(wm_interm_29_so_asym_pt3)
deallocate(wm_interm_30_so_asym_pt3)
deallocate(wm_interm_31_so_asym_pt3)
deallocate(wm_interm_32_so_asym_pt3)
deallocate(wm_interm_33_so_asym_pt3)
deallocate(wm_interm_34_so_asym_pt3)
deallocate(wm_interm_35_so_asym_pt3)
deallocate(wm_interm_36_so_asym_pt3)
deallocate(wm_interm_37_so_asym_pt3)
deallocate(wm_interm_38_so_asym_pt3)
deallocate(wm_interm_39_so_asym_pt3)
deallocate(wm_interm_40_so_asym_pt3)
deallocate(wm_interm_41_so_asym_pt3)
deallocate(wm_interm_42_so_asym_pt3)
deallocate(wm_interm_43_so_asym_pt3)
deallocate(wm_interm_44_so_asym_pt3)
deallocate(wm_interm_45_so_asym_pt3)
deallocate(wm_interm_46_so_asym_pt3)
deallocate(wm_interm_47_so_asym_pt3)
deallocate(wm_interm_48_so_asym_pt3)
deallocate(wm_interm_49_so_asym_pt3)
deallocate(wm_interm_50_so_asym_pt3)
deallocate(wm_interm_51_so_asym_pt3)
deallocate(wm_interm_52_so_asym_pt3)
deallocate(wm_interm_53_so_asym_pt3)
deallocate(wm_interm_54_so_asym_pt3)
deallocate(wm_interm_55_so_asym_pt3)
deallocate(wm_interm_56_so_asym_pt3)
deallocate(wm_interm_57_so_asym_pt3)
deallocate(wm_interm_58_so_asym_pt3)
deallocate(wm_interm_59_so_asym_pt3)
deallocate(wm_interm_60_so_asym_pt3)
deallocate(wm_interm_61_so_asym_pt3)
deallocate(wm_interm_62_so_asym_pt3)
deallocate(wm_interm_63_so_asym_pt3)
deallocate(wm_interm_64_so_asym_pt3)
deallocate(wm_interm_65_so_asym_pt3)
deallocate(wm_interm_66_so_asym_pt3)
deallocate(wm_interm_67_so_asym_pt3)
deallocate(wm_interm_68_so_asym_pt3)
deallocate(wm_interm_69_so_asym_pt3)
deallocate(wm_interm_70_so_asym_pt3)
deallocate(wm_interm_71_so_asym_pt3)
deallocate(wm_interm_72_so_asym_pt3)
deallocate(wm_interm_73_so_asym_pt3)
deallocate(wm_interm_74_so_asym_pt3)
deallocate(wm_interm_75_so_asym_pt3)
deallocate(wm_interm_76_so_asym_pt3)
deallocate(wm_interm_77_so_asym_pt3)
deallocate(wm_interm_78_so_asym_pt3)
deallocate(wm_interm_79_so_asym_pt3)
deallocate(wm_interm_80_so_asym_pt3)
deallocate(wm_interm_81_so_asym_pt3)
deallocate(wm_interm_82_so_asym_pt3)
deallocate(wm_interm_83_so_asym_pt3)
deallocate(wm_interm_84_so_asym_pt3)
deallocate(wm_interm_85_so_asym_pt3)
deallocate(wm_interm_86_so_asym_pt3)
deallocate(wm_interm_87_so_asym_pt3)
deallocate(wm_interm_88_so_asym_pt3)
deallocate(wm_interm_89_so_asym_pt3)
deallocate(wm_interm_90_so_asym_pt3)
deallocate(wm_interm_91_so_asym_pt3)
deallocate(wm_interm_92_so_asym_pt3)
deallocate(wm_interm_93_so_asym_pt3)
deallocate(wm_interm_94_so_asym_pt3)
deallocate(wm_interm_95_so_asym_pt3)
deallocate(wm_interm_96_so_asym_pt3)
deallocate(wm_interm_97_so_asym_pt3)
deallocate(wm_interm_98_so_asym_pt3)

    end subroutine wm_so_asym_intermediates_ccsd_free_pt3
    
    subroutine wm_so_asym_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, c, j, k, l 

    !$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_0_so_asym_pt3(b, c, j, k) = wm_interm_0_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t1(a,k)
end do 
wm_interm_1_so_asym_pt3(b, j, i, k) = wm_interm_1_so_asym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_2_so_asym_pt3(b, c, j, k) = wm_interm_2_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_3_so_asym_pt3(b, c, j, k) = wm_interm_3_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_4_so_asym_pt3(b, c, j, k) = wm_interm_4_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_5_so_asym_pt3(b, c) = wm_interm_5_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_6_so_asym_pt3(b, c) = wm_interm_6_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_7_so_asym_pt3(i, j, k, l) = wm_interm_7_so_asym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,a,l)
end do 
end do 
wm_interm_8_so_asym_pt3(i, j, k, l) = wm_interm_8_so_asym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_9_so_asym_pt3(b, c) = wm_interm_9_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_10_so_asym_pt3(b, c, j, k) = wm_interm_10_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, b,k,a,i)
end do 
end do 
end do 
wm_interm_11_so_asym_pt3(j, k) = wm_interm_11_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_12_so_asym_pt3(b, c, j, k) = wm_interm_12_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_13_so_asym_pt3(j, k) = wm_interm_13_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_14_so_asym_pt3(b, c, j, k) = wm_interm_14_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_15_so_asym_pt3(j, k) = wm_interm_15_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_16_so_asym_pt3(b, j, i, k) = wm_interm_16_so_asym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_17_so_asym_pt3(j, k) = wm_interm_17_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_18_so_asym_pt3(j, k) = wm_interm_18_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_19_so_asym_pt3(j, k) = wm_interm_19_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_20_so_asym_pt3(i, j, k, l) = wm_interm_20_so_asym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_21_so_asym_pt3(b, c, j, k) = wm_interm_21_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_22_so_asym_pt3(b, c, j, k) = wm_interm_22_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_23_so_asym_pt3(b, c, j, k) = wm_interm_23_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_24_so_asym_pt3(b, c, j, k) = wm_interm_24_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_25_so_asym_pt3(b, c, j, k) = wm_interm_25_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_26_so_asym_pt3(b, c, j, k) = wm_interm_26_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_27_so_asym_pt3(b, c) = wm_interm_27_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,j,a,i)
end do 
end do 
end do 
wm_interm_28_so_asym_pt3(b, c) = wm_interm_28_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_29_so_asym_pt3(b, c, j, k) = wm_interm_29_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_30_so_asym_pt3(b, c) = wm_interm_30_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_31_so_asym_pt3(b, i, j, k) = wm_interm_31_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_32_so_asym_pt3(b, c, j, k) = wm_interm_32_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_33_so_asym_pt3(b, c, j, k) = wm_interm_33_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_34_so_asym_pt3(b, c) = wm_interm_34_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_35_so_asym_pt3(b, c) = wm_interm_35_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_36_so_asym_pt3(i, j, k, l) = wm_interm_36_so_asym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_37_so_asym_pt3(b, c, j, k) = wm_interm_37_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_38_so_asym_pt3(j, k) = wm_interm_38_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_39_so_asym_pt3(b, c, j, k) = wm_interm_39_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_40_so_asym_pt3(j, k) = wm_interm_40_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_41_so_asym_pt3(j, k) = wm_interm_41_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_42_so_asym_pt3(j, k) = wm_interm_42_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_43_so_asym_pt3(j, k) = wm_interm_43_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_44_so_asym_pt3(i, j, k, l) = wm_interm_44_so_asym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_45_so_asym_pt3(b, c, j, k) = wm_interm_45_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_46_so_asym_pt3(b, c, j, k) = wm_interm_46_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_47_so_asym_pt3(b, c, j, k) = wm_interm_47_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_48_so_asym_pt3(b, c, j, k) = wm_interm_48_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_49_so_asym_pt3(b, c) = wm_interm_49_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_50_so_asym_pt3(b, c) = wm_interm_50_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_51_so_asym_pt3(b, j) = wm_interm_51_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_52_so_asym_pt3(b, j) = wm_interm_52_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_53_so_asym_pt3(b, c) = wm_interm_53_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_54_so_asym_pt3(b, c) = wm_interm_54_so_asym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_55_so_asym_pt3(j, k) = wm_interm_55_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
end do 
wm_interm_56_so_asym_pt3(b, i, j, k) = wm_interm_56_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_57_so_asym_pt3(j, k) = wm_interm_57_so_asym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_58_so_asym_pt3(b, i, j, k) = wm_interm_58_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_59_so_asym_pt3(b, j, i, k) = wm_interm_59_so_asym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_60_so_asym_pt3(b, i, j, k) = wm_interm_60_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_61_so_asym_pt3(b, j) = wm_interm_61_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_62_so_asym_pt3(b, j) = wm_interm_62_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
end do 
end do 
wm_interm_63_so_asym_pt3(b, j) = wm_interm_63_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,i)
end do 
end do 
wm_interm_64_so_asym_pt3(b, j) = wm_interm_64_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_65_so_asym_pt3(b, j) = wm_interm_65_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_66_so_asym_pt3(b, c, j, k) = wm_interm_66_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_67_so_asym_pt3(b, j) = wm_interm_67_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_68_so_asym_pt3(b, j) = wm_interm_68_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,k,j)
end do 
wm_interm_69_so_asym_pt3(b, i, k, j) = wm_interm_69_so_asym_pt3(b, i, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_70_so_asym_pt3(i, j, k, l) = wm_interm_70_so_asym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,j,k)
end do 
wm_interm_71_so_asym_pt3(b, i, j, k) = wm_interm_71_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_72_so_asym_pt3(b, c, j, k) = wm_interm_72_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_73_so_asym_pt3(b, c, j, k) = wm_interm_73_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_74_so_asym_pt3(b, c, j, k) = wm_interm_74_so_asym_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_75_so_asym_pt3(a, b) = wm_interm_75_so_asym_pt3(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_76_so_asym_pt3(b, i, j, k) = wm_interm_76_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,k)
end do 
wm_interm_77_so_asym_pt3(b, i, j, k) = wm_interm_77_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t1(a,j)
end do 
wm_interm_78_so_asym_pt3(i, j) = wm_interm_78_so_asym_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_79_so_asym_pt3(b, i, j, k) = wm_interm_79_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
wm_interm_80_so_asym_pt3(b, j) = wm_interm_80_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_81_so_asym_pt3(b, j) = wm_interm_81_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_82_so_asym_pt3(b, j) = wm_interm_82_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,j)
wm_interm_83_so_asym_pt3(a, b, i, j) = wm_interm_83_so_asym_pt3(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_84_so_asym_pt3(b, j) = wm_interm_84_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_85_so_asym_pt3(b, j) = wm_interm_85_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_86_so_asym_pt3(b, i, j, k) = wm_interm_86_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,j)
wm_interm_87_so_asym_pt3(a, b, i, j) = wm_interm_87_so_asym_pt3(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_88_so_asym_pt3(b, j, i, k) = wm_interm_88_so_asym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_89_so_asym_pt3(a, b) = wm_interm_89_so_asym_pt3(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_90_so_asym_pt3(i, j) = wm_interm_90_so_asym_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_91_so_asym_pt3(b, j) = wm_interm_91_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
end do 
end do 
wm_interm_92_so_asym_pt3(b, j) = wm_interm_92_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_93_so_asym_pt3(i, j) = wm_interm_93_so_asym_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
end do 
wm_interm_94_so_asym_pt3(b, i, j, k) = wm_interm_94_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_95_so_asym_pt3(b, j) = wm_interm_95_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_96_so_asym_pt3(a, b) = wm_interm_96_so_asym_pt3(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_97_so_asym_pt3(b, j) = wm_interm_97_so_asym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_98_so_asym_pt3(b, i, j, k) = wm_interm_98_so_asym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_asym_intermediates_ccsd_pt3

        function calc_D_oo_wm_so_asym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_asym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,i,b,q)
term(1) = term(1) + r2(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(2) = term(2) + r2(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, b,q,a,i)
term(3) = term(3) + r2(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(4) = term(4) + r2(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(5) = term(5) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(5) = term(5) * (-2.0d+0) 


    calc_D_oo_wm_so_asym_pt0 = zero
    do s = 0, 5
    calc_D_oo_wm_so_asym_pt0 = calc_D_oo_wm_so_asym_pt0 + term(s)
    end do

    end function calc_D_oo_wm_so_asym_pt0
    
    function calc_D_ov_wm_so_asym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_asym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,q,i) * r1(vrdav_Rr, a,i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 


    calc_D_ov_wm_so_asym_pt0 = zero
    do s = 0, 0
    calc_D_ov_wm_so_asym_pt0 = calc_D_ov_wm_so_asym_pt0 + term(s)
    end do

    end function calc_D_ov_wm_so_asym_pt0
    
    function calc_D_vo_wm_so_asym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_asym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,a,q)
end do 
end do 

term(0) = term(0) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,a,i)
term(2) = term(2) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,q,p,i)
term(3) = term(3) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,q,p,i)
term(4) = term(4) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,p,q)
end do 
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (4.0d+0) 


    calc_D_vo_wm_so_asym_pt0 = zero
    do s = 0, 4
    calc_D_vo_wm_so_asym_pt0 = calc_D_vo_wm_so_asym_pt0 + term(s)
    end do

    end function calc_D_vo_wm_so_asym_pt0
    
    function calc_D_vv_wm_so_asym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_asym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, j, b 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(0) = term(0) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,j,a,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,i,a,j)
term(2) = term(2) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,i,p,j)
term(3) = term(3) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,p,j)
end do 
end do 
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,p,i)
end do 
end do 
end do 

term(4) = term(4) * (4.0d+0) 

do i = 1, nocc 
term(5) = term(5) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, p,i)
end do 

term(5) = term(5) * (2.0d+0) 


    calc_D_vv_wm_so_asym_pt0 = zero
    do s = 0, 5
    calc_D_vv_wm_so_asym_pt0 = calc_D_vv_wm_so_asym_pt0 + term(s)
    end do

    end function calc_D_vv_wm_so_asym_pt0


        function calc_D_oo_wm_so_asym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_asym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_so_asym_pt1 = zero

    end function calc_D_oo_wm_so_asym_pt1
    
    function calc_D_ov_wm_so_asym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_asym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:15) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_3_so_asym_pt1(a,p,i,j)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_4_so_asym_pt1(a,p,i,j)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_10_so_asym_pt1(a,p,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(3) = term(3) + s2(a,q,j,i) * wm_interm_4_so_asym_pt1(a,p,j,i)
term(4) = term(4) + s2(a,q,j,i) * wm_interm_10_so_asym_pt1(a,p,j,i)
end do 
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + s2(a,q,p,i) * wm_interm_0_so_asym_pt1(a,i)
term(6) = term(6) + s2(a,q,p,i) * wm_interm_1_so_asym_pt1(a,i)
term(7) = term(7) + s2(a,q,p,i) * wm_interm_2_so_asym_pt1(a,i)
term(8) = term(8) + s2(a,q,p,i) * wm_interm_8_so_asym_pt1(a,i)
term(9) = term(9) + s2(a,q,p,i) * wm_interm_9_so_asym_pt1(a,i)
end do 
end do 

term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (4.0d+0) 

do i = 1, nocc 
term(10) = term(10) + r1(vrdav_Rl, q,i) * wm_interm_5_so_asym_pt1(i,p)
term(11) = term(11) + r1(vrdav_Rl, q,i) * wm_interm_6_so_asym_pt1(i,p)
term(12) = term(12) + r1(vrdav_Rl, q,i) * wm_interm_7_so_asym_pt1(i,p)
term(13) = term(13) + r1(vrdav_Rl, q,i) * wm_interm_11_so_asym_pt1(i,p)
term(14) = term(14) + r1(vrdav_Rl, q,i) * wm_interm_12_so_asym_pt1(i,p)
term(15) = term(15) + r1(vrdav_Rl, q,i) * wm_interm_13_so_asym_pt1(i,p)
end do 

term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (4.0d+0) 
term(15) = term(15) * (-2.0d+0) 


    calc_D_ov_wm_so_asym_pt1 = zero
    do s = 0, 15
    calc_D_ov_wm_so_asym_pt1 = calc_D_ov_wm_so_asym_pt1 + term(s)
    end do

    end function calc_D_ov_wm_so_asym_pt1
    
    function calc_D_vo_wm_so_asym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_asym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, b 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + t2(a,p,j,i) * wm_interm_16_so_asym_pt1(a,i,j,q)
term(1) = term(1) + t2(a,p,j,i) * wm_interm_16_so_asym_pt1(a,j,i,q)
term(2) = term(2) + t2(a,p,j,i) * wm_interm_17_so_asym_pt1(a,j,i,q)
term(3) = term(3) + t2(a,p,j,i) * wm_interm_17_so_asym_pt1(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 

do i = 1, nocc 
term(4) = term(4) + r1(vrdav_Rr, p,i) * wm_interm_14_so_asym_pt1(i,q)
term(5) = term(5) + r1(vrdav_Rr, p,i) * wm_interm_15_so_asym_pt1(i,q)
end do 

term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(6) = term(6) + t2(a,p,q,i) * wm_interm_18_so_asym_pt1(a,i)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 


    calc_D_vo_wm_so_asym_pt1 = zero
    do s = 0, 6
    calc_D_vo_wm_so_asym_pt1 = calc_D_vo_wm_so_asym_pt1 + term(s)
    end do

    end function calc_D_vo_wm_so_asym_pt1
    
    function calc_D_vv_wm_so_asym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_asym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_so_asym_pt1 = zero

  end function calc_D_vv_wm_so_asym_pt1

      function calc_D_oo_wm_so_asym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_asym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, k, j 
    real(F64), dimension(0:366) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_47_so_asym_pt2(a,b,p,i) * wm_interm_6_so_asym_pt2(a,b,q,i)
term(1) = term(1) + wm_interm_47_so_asym_pt2(a,b,p,i) * wm_interm_7_so_asym_pt2(a,b,q,i)
term(2) = term(2) + wm_interm_47_so_asym_pt2(a,b,p,i) * wm_interm_9_so_asym_pt2(a,b,q,i)
term(3) = term(3) + wm_interm_10_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(4) = term(4) + wm_interm_11_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(5) = term(5) + wm_interm_12_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(6) = term(6) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_80_so_asym_pt2(b,a,i,q)
term(7) = term(7) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_80_so_asym_pt2(b,a,i,q)
term(8) = term(8) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_78_so_asym_pt2(b,a,i,q)
term(9) = term(9) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_78_so_asym_pt2(b,a,i,q)
term(10) = term(10) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_79_so_asym_pt2(b,a,i,q)
term(11) = term(11) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_79_so_asym_pt2(b,a,i,q)
term(12) = term(12) + wm_interm_49_so_asym_pt2(a,b,p,i) * wm_interm_6_so_asym_pt2(a,b,q,i)
term(13) = term(13) + wm_interm_49_so_asym_pt2(a,b,p,i) * wm_interm_7_so_asym_pt2(a,b,q,i)
term(14) = term(14) + wm_interm_49_so_asym_pt2(a,b,p,i) * wm_interm_9_so_asym_pt2(a,b,q,i)
term(15) = term(15) + wm_interm_10_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(16) = term(16) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_82_so_asym_pt2(b,a,i,q)
term(17) = term(17) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_82_so_asym_pt2(b,a,i,q)
term(18) = term(18) + wm_interm_11_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(19) = term(19) + wm_interm_12_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(20) = term(20) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_77_so_asym_pt2(b,a,i,q)
term(21) = term(21) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_77_so_asym_pt2(b,a,i,q)
term(22) = term(22) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_76_so_asym_pt2(b,a,i,q)
term(23) = term(23) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_76_so_asym_pt2(b,a,i,q)
term(24) = term(24) + wm_interm_51_so_asym_pt2(a,b,p,i) * wm_interm_9_so_asym_pt2(a,b,q,i)
term(25) = term(25) + wm_interm_10_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(26) = term(26) + wm_interm_12_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(27) = term(27) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_82_so_asym_pt2(b,a,i,q)
term(28) = term(28) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_77_so_asym_pt2(b,a,i,q)
term(29) = term(29) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_76_so_asym_pt2(b,a,i,q)
term(30) = term(30) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_81_so_asym_pt2(b,a,i,q)
term(31) = term(31) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_82_so_asym_pt2(b,a,i,q)
term(32) = term(32) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_76_so_asym_pt2(b,a,i,q)
term(33) = term(33) + wm_interm_51_so_asym_pt2(a,b,p,i) * wm_interm_7_so_asym_pt2(a,b,q,i)
term(34) = term(34) + wm_interm_27_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(35) = term(35) + wm_interm_11_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(36) = term(36) + wm_interm_55_so_asym_pt2(a,b,p,i) * wm_interm_6_so_asym_pt2(a,b,q,i)
term(37) = term(37) + wm_interm_55_so_asym_pt2(a,b,p,i) * wm_interm_7_so_asym_pt2(a,b,q,i)
term(38) = term(38) + wm_interm_55_so_asym_pt2(a,b,p,i) * wm_interm_9_so_asym_pt2(a,b,q,i)
term(39) = term(39) + wm_interm_10_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(40) = term(40) + wm_interm_11_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(41) = term(41) + wm_interm_12_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(42) = term(42) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_80_so_asym_pt2(b,a,i,q)
term(43) = term(43) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_80_so_asym_pt2(b,a,i,q)
term(44) = term(44) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_78_so_asym_pt2(b,a,i,q)
term(45) = term(45) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_78_so_asym_pt2(b,a,i,q)
term(46) = term(46) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_79_so_asym_pt2(b,a,i,q)
term(47) = term(47) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_79_so_asym_pt2(b,a,i,q)
term(48) = term(48) + wm_interm_15_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(49) = term(49) + wm_interm_16_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(50) = term(50) + wm_interm_17_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(51) = term(51) + wm_interm_18_so_asym_pt2(a,b,q,i) * wm_interm_47_so_asym_pt2(a,b,p,i)
term(52) = term(52) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_91_so_asym_pt2(b,a,i,q)
term(53) = term(53) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_91_so_asym_pt2(b,a,i,q)
term(54) = term(54) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_92_so_asym_pt2(b,a,i,q)
term(55) = term(55) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_92_so_asym_pt2(b,a,i,q)
term(56) = term(56) + wm_interm_15_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(57) = term(57) + wm_interm_16_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(58) = term(58) + wm_interm_17_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(59) = term(59) + wm_interm_18_so_asym_pt2(a,b,q,i) * wm_interm_49_so_asym_pt2(a,b,p,i)
term(60) = term(60) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_90_so_asym_pt2(b,a,i,q)
term(61) = term(61) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_90_so_asym_pt2(b,a,i,q)
term(62) = term(62) + wm_interm_28_so_asym_pt2(a,b,p,i) * wm_interm_89_so_asym_pt2(b,a,i,q)
term(63) = term(63) + wm_interm_31_so_asym_pt2(a,b,p,i) * wm_interm_89_so_asym_pt2(b,a,i,q)
term(64) = term(64) + wm_interm_17_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(65) = term(65) + wm_interm_18_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(66) = term(66) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_90_so_asym_pt2(b,a,i,q)
term(67) = term(67) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_89_so_asym_pt2(b,a,i,q)
term(68) = term(68) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_89_so_asym_pt2(b,a,i,q)
term(69) = term(69) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_90_so_asym_pt2(b,a,i,q)
term(70) = term(70) + wm_interm_16_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(71) = term(71) + wm_interm_15_so_asym_pt2(a,b,q,i) * wm_interm_51_so_asym_pt2(a,b,p,i)
term(72) = term(72) + wm_interm_15_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(73) = term(73) + wm_interm_16_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(74) = term(74) + wm_interm_17_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(75) = term(75) + wm_interm_18_so_asym_pt2(a,b,q,i) * wm_interm_55_so_asym_pt2(a,b,p,i)
term(76) = term(76) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_91_so_asym_pt2(b,a,i,q)
term(77) = term(77) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_91_so_asym_pt2(b,a,i,q)
term(78) = term(78) + wm_interm_29_so_asym_pt2(a,b,p,i) * wm_interm_92_so_asym_pt2(b,a,i,q)
term(79) = term(79) + wm_interm_30_so_asym_pt2(a,b,p,i) * wm_interm_92_so_asym_pt2(b,a,i,q)
term(80) = term(80) + wm_interm_28_so_asym_pt2(a,b,i,q) * wm_interm_77_so_asym_pt2(b,a,p,i)
term(81) = term(81) + wm_interm_28_so_asym_pt2(a,b,i,q) * wm_interm_82_so_asym_pt2(b,a,p,i)
term(82) = term(82) + wm_interm_28_so_asym_pt2(a,b,i,q) * wm_interm_81_so_asym_pt2(b,a,p,i)
term(83) = term(83) + wm_interm_11_so_asym_pt2(a,b,i,p) * wm_interm_55_so_asym_pt2(a,b,i,q)
term(84) = term(84) + wm_interm_27_so_asym_pt2(a,b,i,p) * wm_interm_55_so_asym_pt2(a,b,i,q)
term(85) = term(85) + wm_interm_27_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(86) = term(86) + wm_interm_11_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(87) = term(87) + wm_interm_51_so_asym_pt2(a,b,i,q) * wm_interm_7_so_asym_pt2(a,b,i,p)
term(88) = term(88) + wm_interm_55_so_asym_pt2(a,b,i,q) * wm_interm_6_so_asym_pt2(a,b,i,p)
term(89) = term(89) + wm_interm_12_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(90) = term(90) + wm_interm_31_so_asym_pt2(a,b,i,q) * wm_interm_77_so_asym_pt2(b,a,p,i)
term(91) = term(91) + wm_interm_51_so_asym_pt2(a,b,i,q) * wm_interm_9_so_asym_pt2(a,b,i,p)
term(92) = term(92) + wm_interm_31_so_asym_pt2(a,b,i,q) * wm_interm_82_so_asym_pt2(b,a,p,i)
term(93) = term(93) + wm_interm_10_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(94) = term(94) + wm_interm_31_so_asym_pt2(a,b,i,q) * wm_interm_81_so_asym_pt2(b,a,p,i)
term(95) = term(95) + wm_interm_30_so_asym_pt2(a,b,i,q) * wm_interm_77_so_asym_pt2(b,a,p,i)
term(96) = term(96) + wm_interm_30_so_asym_pt2(a,b,i,q) * wm_interm_82_so_asym_pt2(b,a,p,i)
term(97) = term(97) + wm_interm_30_so_asym_pt2(a,b,i,q) * wm_interm_81_so_asym_pt2(b,a,p,i)
term(98) = term(98) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_78_so_asym_pt2(b,a,p,i)
term(99) = term(99) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_79_so_asym_pt2(b,a,p,i)
term(100) = term(100) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_76_so_asym_pt2(b,a,p,i)
term(101) = term(101) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_81_so_asym_pt2(b,a,p,i)
term(102) = term(102) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_82_so_asym_pt2(b,a,p,i)
term(103) = term(103) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_80_so_asym_pt2(b,a,p,i)
term(104) = term(104) + wm_interm_11_so_asym_pt2(a,b,i,p) * wm_interm_49_so_asym_pt2(a,b,i,q)
term(105) = term(105) + wm_interm_27_so_asym_pt2(a,b,i,p) * wm_interm_49_so_asym_pt2(a,b,i,q)
term(106) = term(106) + wm_interm_11_so_asym_pt2(a,b,i,p) * wm_interm_47_so_asym_pt2(a,b,i,q)
term(107) = term(107) + wm_interm_27_so_asym_pt2(a,b,i,p) * wm_interm_47_so_asym_pt2(a,b,i,q)
term(108) = term(108) + wm_interm_49_so_asym_pt2(a,b,i,q) * wm_interm_6_so_asym_pt2(a,b,i,p)
term(109) = term(109) + wm_interm_47_so_asym_pt2(a,b,i,q) * wm_interm_6_so_asym_pt2(a,b,i,p)
term(110) = term(110) + wm_interm_28_so_asym_pt2(a,b,i,q) * wm_interm_90_so_asym_pt2(b,a,p,i)
term(111) = term(111) + wm_interm_28_so_asym_pt2(a,b,i,q) * wm_interm_89_so_asym_pt2(b,a,p,i)
term(112) = term(112) + wm_interm_15_so_asym_pt2(a,b,i,p) * wm_interm_55_so_asym_pt2(a,b,i,q)
term(113) = term(113) + wm_interm_16_so_asym_pt2(a,b,i,p) * wm_interm_55_so_asym_pt2(a,b,i,q)
term(114) = term(114) + wm_interm_16_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(115) = term(115) + wm_interm_15_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(116) = term(116) + wm_interm_17_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(117) = term(117) + wm_interm_31_so_asym_pt2(a,b,i,q) * wm_interm_90_so_asym_pt2(b,a,p,i)
term(118) = term(118) + wm_interm_18_so_asym_pt2(a,b,i,p) * wm_interm_51_so_asym_pt2(a,b,i,q)
term(119) = term(119) + wm_interm_31_so_asym_pt2(a,b,i,q) * wm_interm_89_so_asym_pt2(b,a,p,i)
term(120) = term(120) + wm_interm_30_so_asym_pt2(a,b,i,q) * wm_interm_90_so_asym_pt2(b,a,p,i)
term(121) = term(121) + wm_interm_30_so_asym_pt2(a,b,i,q) * wm_interm_89_so_asym_pt2(b,a,p,i)
term(122) = term(122) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_91_so_asym_pt2(b,a,p,i)
term(123) = term(123) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_92_so_asym_pt2(b,a,p,i)
term(124) = term(124) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_89_so_asym_pt2(b,a,p,i)
term(125) = term(125) + wm_interm_29_so_asym_pt2(a,b,i,q) * wm_interm_90_so_asym_pt2(b,a,p,i)
term(126) = term(126) + wm_interm_15_so_asym_pt2(a,b,i,p) * wm_interm_49_so_asym_pt2(a,b,i,q)
term(127) = term(127) + wm_interm_16_so_asym_pt2(a,b,i,p) * wm_interm_49_so_asym_pt2(a,b,i,q)
term(128) = term(128) + wm_interm_15_so_asym_pt2(a,b,i,p) * wm_interm_47_so_asym_pt2(a,b,i,q)
term(129) = term(129) + wm_interm_16_so_asym_pt2(a,b,i,p) * wm_interm_47_so_asym_pt2(a,b,i,q)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (4.0d+0) 
term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-8.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * (8.0d+0) 
term(50) = term(50) * (16.0d+0) 
term(51) = term(51) * (-16.0d+0) 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * (16.0d+0) 
term(54) = term(54) * (8.0d+0) 
term(55) = term(55) * (-16.0d+0) 
term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (8.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (-8.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (8.0d+0) 
term(64) = term(64) * (4.0d+0) 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * (8.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (8.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(86) = term(86) * (-2.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (-2.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(105) = term(105) * (-2.0d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (4.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (4.0d+0) 
term(111) = term(111) * (-4.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (-4.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-8.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (8.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(130) = term(130) + wm_interm_48_so_asym_pt2(a,q,i,p) * wm_interm_5_so_asym_pt2(a,i)
term(131) = term(131) + wm_interm_48_so_asym_pt2(a,q,p,i) * wm_interm_5_so_asym_pt2(a,i)
term(132) = term(132) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_48_so_asym_pt2(a,q,i,p)
term(133) = term(133) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_48_so_asym_pt2(a,q,p,i)
term(134) = term(134) + wm_interm_21_so_asym_pt2(a,p,i,q) * wm_interm_52_so_asym_pt2(a,i)
end do 
end do 

term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * (4.0d+0) 
term(134) = term(134) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(135) = term(135) + wm_interm_22_so_asym_pt2(p,i,j,q) * wm_interm_66_so_asym_pt2(j,i)
term(136) = term(136) + wm_interm_32_so_asym_pt2(q,i,j,p) * wm_interm_56_so_asym_pt2(i,j)
term(137) = term(137) + wm_interm_32_so_asym_pt2(i,q,p,j) * wm_interm_56_so_asym_pt2(i,j)
term(138) = term(138) + wm_interm_32_so_asym_pt2(q,i,p,j) * wm_interm_56_so_asym_pt2(i,j)
term(139) = term(139) + wm_interm_32_so_asym_pt2(q,i,j,p) * wm_interm_58_so_asym_pt2(i,j)
term(140) = term(140) + wm_interm_32_so_asym_pt2(i,q,p,j) * wm_interm_58_so_asym_pt2(i,j)
term(141) = term(141) + wm_interm_32_so_asym_pt2(q,i,p,j) * wm_interm_58_so_asym_pt2(i,j)
term(142) = term(142) + wm_interm_41_so_asym_pt2(q,i,j,p) * wm_interm_56_so_asym_pt2(i,j)
term(143) = term(143) + wm_interm_41_so_asym_pt2(i,q,p,j) * wm_interm_56_so_asym_pt2(i,j)
term(144) = term(144) + wm_interm_41_so_asym_pt2(q,i,p,j) * wm_interm_56_so_asym_pt2(i,j)
term(145) = term(145) + wm_interm_41_so_asym_pt2(q,i,j,p) * wm_interm_58_so_asym_pt2(i,j)
term(146) = term(146) + wm_interm_41_so_asym_pt2(i,q,p,j) * wm_interm_58_so_asym_pt2(i,j)
term(147) = term(147) + wm_interm_41_so_asym_pt2(q,i,p,j) * wm_interm_58_so_asym_pt2(i,j)
term(148) = term(148) + wm_interm_22_so_asym_pt2(p,i,j,q) * wm_interm_74_so_asym_pt2(j,i)
term(149) = term(149) + wm_interm_22_so_asym_pt2(p,i,j,q) * wm_interm_75_so_asym_pt2(j,i)
term(150) = term(150) + wm_interm_22_so_asym_pt2(p,i,j,q) * wm_interm_73_so_asym_pt2(j,i)
term(151) = term(151) + wm_interm_33_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(i,p,q,j)
term(152) = term(152) + wm_interm_33_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(p,i,j,q)
term(153) = term(153) + wm_interm_34_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(i,p,q,j)
term(154) = term(154) + wm_interm_34_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(p,i,j,q)
term(155) = term(155) + wm_interm_35_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(i,p,q,j)
term(156) = term(156) + wm_interm_35_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(p,i,j,q)
term(157) = term(157) + wm_interm_22_so_asym_pt2(p,i,j,q) * wm_interm_87_so_asym_pt2(j,i)
term(158) = term(158) + wm_interm_22_so_asym_pt2(p,i,j,q) * wm_interm_88_so_asym_pt2(j,i)
term(159) = term(159) + wm_interm_42_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(i,p,q,j)
term(160) = term(160) + wm_interm_42_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(p,i,j,q)
term(161) = term(161) + wm_interm_43_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(i,p,q,j)
term(162) = term(162) + wm_interm_43_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(p,i,j,q)
term(163) = term(163) + wm_interm_44_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(i,p,q,j)
term(164) = term(164) + wm_interm_44_so_asym_pt2(i,j) * wm_interm_57_so_asym_pt2(p,i,j,q)
end do 
end do 

term(135) = term(135) * (-2.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (-2.0d+0) 
term(140) = term(140) * (-2.0d+0) 
term(141) = term(141) * (4.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (2.0d+0) 
term(144) = term(144) * (-4.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (8.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(151) = term(151) * (0.5d+0) 
term(152) = term(152) * (0.5d+0) 
term(153) = term(153) * (-1.0d+0) 
term(154) = term(154) * (-1.0d+0) 
term(155) = term(155) * (0.5d+0) 
term(156) = term(156) * (0.5d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(161) = term(161) * (-2.0d+0) 
term(162) = term(162) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(165) = term(165) + wm_interm_22_so_asym_pt2(i,p,j,k) * wm_interm_71_so_asym_pt2(j,k,q,i)
term(166) = term(166) + wm_interm_22_so_asym_pt2(p,i,j,k) * wm_interm_71_so_asym_pt2(j,k,q,i)
term(167) = term(167) + wm_interm_22_so_asym_pt2(i,p,j,k) * wm_interm_72_so_asym_pt2(j,k,q,i)
term(168) = term(168) + wm_interm_22_so_asym_pt2(p,i,j,k) * wm_interm_72_so_asym_pt2(j,k,q,i)
term(169) = term(169) + wm_interm_22_so_asym_pt2(i,p,j,k) * wm_interm_86_so_asym_pt2(j,k,q,i)
term(170) = term(170) + wm_interm_22_so_asym_pt2(p,i,j,k) * wm_interm_86_so_asym_pt2(j,k,q,i)
end do 
end do 
end do 

term(165) = term(165) * (0.5d+0) 
term(166) = term(166) * (-1.0d+0) 
term(167) = term(167) * (-1.0d+0) 
term(168) = term(168) * (0.5d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(171) = term(171) + wm_interm_22_so_asym_pt2(p,i,j,k) * wm_interm_71_so_asym_pt2(j,k,i,q)
term(172) = term(172) + wm_interm_22_so_asym_pt2(i,p,j,k) * wm_interm_72_so_asym_pt2(j,k,i,q)
term(173) = term(173) + wm_interm_32_so_asym_pt2(i,q,j,k) * wm_interm_57_so_asym_pt2(i,p,k,j)
term(174) = term(174) + wm_interm_32_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(i,p,k,j)
term(175) = term(175) + wm_interm_32_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(i,p,j,k)
term(176) = term(176) + wm_interm_32_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(p,i,k,j)
term(177) = term(177) + wm_interm_32_so_asym_pt2(i,q,j,k) * wm_interm_57_so_asym_pt2(p,i,j,k)
term(178) = term(178) + wm_interm_32_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(p,i,j,k)
term(179) = term(179) + wm_interm_22_so_asym_pt2(p,i,j,k) * wm_interm_86_so_asym_pt2(j,k,i,q)
term(180) = term(180) + wm_interm_22_so_asym_pt2(i,p,j,k) * wm_interm_86_so_asym_pt2(j,k,i,q)
term(181) = term(181) + wm_interm_41_so_asym_pt2(i,q,j,k) * wm_interm_57_so_asym_pt2(i,p,k,j)
term(182) = term(182) + wm_interm_41_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(i,p,k,j)
term(183) = term(183) + wm_interm_41_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(i,p,j,k)
term(184) = term(184) + wm_interm_41_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(p,i,k,j)
term(185) = term(185) + wm_interm_41_so_asym_pt2(i,q,j,k) * wm_interm_57_so_asym_pt2(p,i,j,k)
term(186) = term(186) + wm_interm_41_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(p,i,j,k)
term(187) = term(187) + wm_interm_32_so_asym_pt2(i,j,k,p) * wm_interm_57_so_asym_pt2(j,i,k,q)
term(188) = term(188) + wm_interm_32_so_asym_pt2(i,j,k,p) * wm_interm_57_so_asym_pt2(i,j,q,k)
term(189) = term(189) + wm_interm_32_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(j,i,q,k)
term(190) = term(190) + wm_interm_32_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(i,j,q,k)
term(191) = term(191) + wm_interm_32_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(i,j,k,q)
term(192) = term(192) + wm_interm_32_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(j,i,k,q)
term(193) = term(193) + wm_interm_41_so_asym_pt2(i,j,k,p) * wm_interm_57_so_asym_pt2(j,i,k,q)
term(194) = term(194) + wm_interm_41_so_asym_pt2(i,j,k,p) * wm_interm_57_so_asym_pt2(i,j,q,k)
term(195) = term(195) + wm_interm_41_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(j,i,q,k)
term(196) = term(196) + wm_interm_41_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(i,j,q,k)
term(197) = term(197) + wm_interm_41_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(i,j,k,q)
term(198) = term(198) + wm_interm_41_so_asym_pt2(i,j,p,k) * wm_interm_57_so_asym_pt2(j,i,k,q)
end do 
end do 
end do 

term(171) = term(171) * (0.5d+0) 
term(172) = term(172) * (0.5d+0) 
term(173) = term(173) * (0.5d+0) 
term(174) = term(174) * (-1.0d+0) 
term(175) = term(175) * (0.5d+0) 
term(176) = term(176) * (0.5d+0) 
term(177) = term(177) * (0.5d+0) 
term(178) = term(178) * (-1.0d+0) 
term(179) = term(179) * (2.0d+0) 
term(180) = term(180) * (-2.0d+0) 
term(182) = term(182) * (-2.0d+0) 
term(186) = term(186) * (-2.0d+0) 
term(187) = term(187) * (0.5d+0) 
term(188) = term(188) * (0.5d+0) 
term(189) = term(189) * (0.5d+0) 
term(190) = term(190) * (-1.0d+0) 
term(191) = term(191) * (0.5d+0) 
term(192) = term(192) * (-1.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(198) = term(198) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(199) = term(199) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_71_so_asym_pt2(p,j,i,q)
term(200) = term(200) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_71_so_asym_pt2(p,j,q,i)
term(201) = term(201) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_71_so_asym_pt2(p,j,i,q)
term(202) = term(202) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_71_so_asym_pt2(p,j,q,i)
term(203) = term(203) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_72_so_asym_pt2(p,j,q,i)
term(204) = term(204) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_72_so_asym_pt2(p,j,q,i)
term(205) = term(205) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_71_so_asym_pt2(j,p,q,i)
term(206) = term(206) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_71_so_asym_pt2(j,p,q,i)
term(207) = term(207) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_72_so_asym_pt2(j,p,i,q)
term(208) = term(208) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_72_so_asym_pt2(j,p,q,i)
term(209) = term(209) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_72_so_asym_pt2(j,p,i,q)
term(210) = term(210) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_72_so_asym_pt2(j,p,q,i)
term(211) = term(211) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(p,j,i,q)
term(212) = term(212) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(p,j,q,i)
term(213) = term(213) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(p,j,i,q)
term(214) = term(214) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(p,j,q,i)
term(215) = term(215) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(j,p,q,i)
term(216) = term(216) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(j,p,q,i)
term(217) = term(217) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(j,p,i,q)
term(218) = term(218) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_86_so_asym_pt2(j,p,i,q)
end do 
end do 

term(199) = term(199) * (0.5d+0) 
term(200) = term(200) * (-1.0d+0) 
term(201) = term(201) * (-1.0d+0) 
term(202) = term(202) * (2.0d+0) 
term(203) = term(203) * (0.5d+0) 
term(204) = term(204) * (-1.0d+0) 
term(205) = term(205) * (0.5d+0) 
term(206) = term(206) * (-1.0d+0) 
term(207) = term(207) * (0.5d+0) 
term(208) = term(208) * (-1.0d+0) 
term(209) = term(209) * (-1.0d+0) 
term(210) = term(210) * (2.0d+0) 
term(211) = term(211) * (2.0d+0) 
term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * (4.0d+0) 
term(215) = term(215) * (2.0d+0) 
term(216) = term(216) * (-4.0d+0) 
term(217) = term(217) * (-2.0d+0) 
term(218) = term(218) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(219) = term(219) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_71_so_asym_pt2(k,p,j,i)
term(220) = term(220) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_72_so_asym_pt2(k,p,j,i)
term(221) = term(221) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_71_so_asym_pt2(p,k,j,i)
term(222) = term(222) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_72_so_asym_pt2(p,k,j,i)
term(223) = term(223) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_86_so_asym_pt2(k,p,j,i)
term(224) = term(224) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_86_so_asym_pt2(p,k,j,i)
end do 
end do 
end do 

term(219) = term(219) * (0.5d+0) 
term(220) = term(220) * (-1.0d+0) 
term(221) = term(221) * (-1.0d+0) 
term(222) = term(222) * (0.5d+0) 
term(223) = term(223) * (2.0d+0) 
term(224) = term(224) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(225) = term(225) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_72_so_asym_pt2(k,p,i,j)
term(226) = term(226) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_71_so_asym_pt2(p,k,i,j)
term(227) = term(227) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_86_so_asym_pt2(k,p,i,j)
term(228) = term(228) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_86_so_asym_pt2(p,k,i,j)
end do 
end do 
end do 

term(225) = term(225) * (0.5d+0) 
term(226) = term(226) * (0.5d+0) 
term(227) = term(227) * (-2.0d+0) 
term(228) = term(228) * (2.0d+0) 

do i = 1, nocc 
term(229) = term(229) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_66_so_asym_pt2(i,q)
term(230) = term(230) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_66_so_asym_pt2(i,q)
term(231) = term(231) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_66_so_asym_pt2(p,i)
term(232) = term(232) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_66_so_asym_pt2(p,i)
term(233) = term(233) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_73_so_asym_pt2(i,q)
term(234) = term(234) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_73_so_asym_pt2(i,q)
term(235) = term(235) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_74_so_asym_pt2(i,q)
term(236) = term(236) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_74_so_asym_pt2(i,q)
term(237) = term(237) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_75_so_asym_pt2(i,q)
term(238) = term(238) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_75_so_asym_pt2(i,q)
term(239) = term(239) + wm_interm_33_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(240) = term(240) + wm_interm_34_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(241) = term(241) + wm_interm_35_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(242) = term(242) + wm_interm_33_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(243) = term(243) + wm_interm_34_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(244) = term(244) + wm_interm_35_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(245) = term(245) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_87_so_asym_pt2(i,q)
term(246) = term(246) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_87_so_asym_pt2(i,q)
term(247) = term(247) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_88_so_asym_pt2(i,q)
term(248) = term(248) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_88_so_asym_pt2(i,q)
term(249) = term(249) + wm_interm_42_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(250) = term(250) + wm_interm_43_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(251) = term(251) + wm_interm_44_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(252) = term(252) + wm_interm_42_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(253) = term(253) + wm_interm_43_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(254) = term(254) + wm_interm_44_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(255) = term(255) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_74_so_asym_pt2(p,i)
term(256) = term(256) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_75_so_asym_pt2(p,i)
term(257) = term(257) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_73_so_asym_pt2(p,i)
term(258) = term(258) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_74_so_asym_pt2(p,i)
term(259) = term(259) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_75_so_asym_pt2(p,i)
term(260) = term(260) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_73_so_asym_pt2(p,i)
term(261) = term(261) + wm_interm_33_so_asym_pt2(i,p) * wm_interm_56_so_asym_pt2(i,q)
term(262) = term(262) + wm_interm_33_so_asym_pt2(i,p) * wm_interm_58_so_asym_pt2(i,q)
term(263) = term(263) + wm_interm_34_so_asym_pt2(i,p) * wm_interm_56_so_asym_pt2(i,q)
term(264) = term(264) + wm_interm_34_so_asym_pt2(i,p) * wm_interm_58_so_asym_pt2(i,q)
term(265) = term(265) + wm_interm_35_so_asym_pt2(i,p) * wm_interm_56_so_asym_pt2(i,q)
term(266) = term(266) + wm_interm_35_so_asym_pt2(i,p) * wm_interm_58_so_asym_pt2(i,q)
term(267) = term(267) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_87_so_asym_pt2(p,i)
term(268) = term(268) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_88_so_asym_pt2(p,i)
term(269) = term(269) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_87_so_asym_pt2(p,i)
term(270) = term(270) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_88_so_asym_pt2(p,i)
term(271) = term(271) + wm_interm_42_so_asym_pt2(i,p) * wm_interm_56_so_asym_pt2(i,q)
term(272) = term(272) + wm_interm_42_so_asym_pt2(i,p) * wm_interm_58_so_asym_pt2(i,q)
term(273) = term(273) + wm_interm_43_so_asym_pt2(i,p) * wm_interm_56_so_asym_pt2(i,q)
term(274) = term(274) + wm_interm_43_so_asym_pt2(i,p) * wm_interm_58_so_asym_pt2(i,q)
term(275) = term(275) + wm_interm_44_so_asym_pt2(i,p) * wm_interm_56_so_asym_pt2(i,q)
term(276) = term(276) + wm_interm_44_so_asym_pt2(i,p) * wm_interm_58_so_asym_pt2(i,q)
end do 

term(229) = term(229) * (-2.0d+0) 
term(230) = term(230) * (4.0d+0) 
term(231) = term(231) * (-2.0d+0) 
term(232) = term(232) * (4.0d+0) 
term(234) = term(234) * (-2.0d+0) 
term(236) = term(236) * (-2.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (4.0d+0) 
term(240) = term(240) * (-2.0d+0) 
term(242) = term(242) * (-2.0d+0) 
term(243) = term(243) * (4.0d+0) 
term(244) = term(244) * (-2.0d+0) 
term(245) = term(245) * (4.0d+0) 
term(246) = term(246) * (-8.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (8.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (-4.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (8.0d+0) 
term(254) = term(254) * (-4.0d+0) 
term(256) = term(256) * (-2.0d+0) 
term(258) = term(258) * (-2.0d+0) 
term(259) = term(259) * (4.0d+0) 
term(260) = term(260) * (-2.0d+0) 
term(262) = term(262) * (-2.0d+0) 
term(263) = term(263) * (-2.0d+0) 
term(264) = term(264) * (4.0d+0) 
term(266) = term(266) * (-2.0d+0) 
term(267) = term(267) * (4.0d+0) 
term(268) = term(268) * (-4.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (8.0d+0) 
term(271) = term(271) * (2.0d+0) 
term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (8.0d+0) 
term(275) = term(275) * (2.0d+0) 
term(276) = term(276) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(277) = term(277) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_69_so_asym_pt2(b,a,p,q)
term(278) = term(278) + wm_interm_69_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(279) = term(279) + wm_interm_28_so_asym_pt2(a,b,p,q) * wm_interm_70_so_asym_pt2(b,a)
term(280) = term(280) + wm_interm_31_so_asym_pt2(a,b,p,q) * wm_interm_70_so_asym_pt2(b,a)
term(281) = term(281) + wm_interm_30_so_asym_pt2(a,b,p,q) * wm_interm_70_so_asym_pt2(b,a)
term(282) = term(282) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_81_so_asym_pt2(b,a,p,q)
term(283) = term(283) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_82_so_asym_pt2(b,a,p,q)
term(284) = term(284) + wm_interm_81_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(285) = term(285) + wm_interm_82_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(286) = term(286) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_76_so_asym_pt2(b,a,p,q)
term(287) = term(287) + wm_interm_76_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(288) = term(288) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_80_so_asym_pt2(b,a,p,q)
term(289) = term(289) + wm_interm_80_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(290) = term(290) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_78_so_asym_pt2(b,a,p,q)
term(291) = term(291) + wm_interm_78_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(292) = term(292) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_79_so_asym_pt2(b,a,p,q)
term(293) = term(293) + wm_interm_79_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(294) = term(294) + wm_interm_53_so_asym_pt2(a,b) * wm_interm_9_so_asym_pt2(a,b,q,p)
term(295) = term(295) + wm_interm_10_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(296) = term(296) + wm_interm_12_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(297) = term(297) + wm_interm_53_so_asym_pt2(a,b) * wm_interm_7_so_asym_pt2(a,b,q,p)
term(298) = term(298) + wm_interm_27_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(299) = term(299) + wm_interm_11_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(300) = term(300) + wm_interm_54_so_asym_pt2(a,b) * wm_interm_9_so_asym_pt2(a,b,q,p)
term(301) = term(301) + wm_interm_10_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(302) = term(302) + wm_interm_54_so_asym_pt2(a,b) * wm_interm_7_so_asym_pt2(a,b,q,p)
term(303) = term(303) + wm_interm_12_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(304) = term(304) + wm_interm_27_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(305) = term(305) + wm_interm_11_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(306) = term(306) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_89_so_asym_pt2(b,a,p,q)
term(307) = term(307) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_90_so_asym_pt2(b,a,p,q)
term(308) = term(308) + wm_interm_89_so_asym_pt2(a,b,p,q) * wm_interm_8_so_asym_pt2(b,a)
term(309) = term(309) + wm_interm_8_so_asym_pt2(a,b) * wm_interm_90_so_asym_pt2(b,a,p,q)
term(310) = term(310) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_91_so_asym_pt2(b,a,p,q)
term(311) = term(311) + wm_interm_8_so_asym_pt2(a,b) * wm_interm_91_so_asym_pt2(b,a,p,q)
term(312) = term(312) + wm_interm_4_so_asym_pt2(a,b) * wm_interm_92_so_asym_pt2(b,a,p,q)
term(313) = term(313) + wm_interm_8_so_asym_pt2(a,b) * wm_interm_92_so_asym_pt2(b,a,p,q)
term(314) = term(314) + wm_interm_17_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(315) = term(315) + wm_interm_18_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(316) = term(316) + wm_interm_16_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(317) = term(317) + wm_interm_15_so_asym_pt2(a,b,q,p) * wm_interm_53_so_asym_pt2(a,b)
term(318) = term(318) + wm_interm_17_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(319) = term(319) + wm_interm_18_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(320) = term(320) + wm_interm_16_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(321) = term(321) + wm_interm_15_so_asym_pt2(a,b,q,p) * wm_interm_54_so_asym_pt2(a,b)
term(322) = term(322) + wm_interm_28_so_asym_pt2(a,b,p,q) * wm_interm_83_so_asym_pt2(b,a)
term(323) = term(323) + wm_interm_28_so_asym_pt2(a,b,p,q) * wm_interm_84_so_asym_pt2(b,a)
term(324) = term(324) + wm_interm_28_so_asym_pt2(a,b,p,q) * wm_interm_85_so_asym_pt2(b,a)
term(325) = term(325) + wm_interm_31_so_asym_pt2(a,b,p,q) * wm_interm_83_so_asym_pt2(b,a)
term(326) = term(326) + wm_interm_31_so_asym_pt2(a,b,p,q) * wm_interm_84_so_asym_pt2(b,a)
term(327) = term(327) + wm_interm_31_so_asym_pt2(a,b,p,q) * wm_interm_85_so_asym_pt2(b,a)
term(328) = term(328) + wm_interm_26_so_asym_pt2(a,b) * wm_interm_55_so_asym_pt2(a,b,p,q)
term(329) = term(329) + wm_interm_23_so_asym_pt2(a,b) * wm_interm_55_so_asym_pt2(a,b,p,q)
term(330) = term(330) + wm_interm_24_so_asym_pt2(a,b) * wm_interm_55_so_asym_pt2(a,b,p,q)
term(331) = term(331) + wm_interm_30_so_asym_pt2(a,b,p,q) * wm_interm_83_so_asym_pt2(b,a)
term(332) = term(332) + wm_interm_30_so_asym_pt2(a,b,p,q) * wm_interm_84_so_asym_pt2(b,a)
term(333) = term(333) + wm_interm_30_so_asym_pt2(a,b,p,q) * wm_interm_85_so_asym_pt2(b,a)
term(334) = term(334) + wm_interm_26_so_asym_pt2(a,b) * wm_interm_49_so_asym_pt2(a,b,p,q)
term(335) = term(335) + wm_interm_26_so_asym_pt2(a,b) * wm_interm_47_so_asym_pt2(a,b,p,q)
term(336) = term(336) + wm_interm_23_so_asym_pt2(a,b) * wm_interm_49_so_asym_pt2(a,b,p,q)
term(337) = term(337) + wm_interm_23_so_asym_pt2(a,b) * wm_interm_47_so_asym_pt2(a,b,p,q)
term(338) = term(338) + wm_interm_24_so_asym_pt2(a,b) * wm_interm_49_so_asym_pt2(a,b,p,q)
term(339) = term(339) + wm_interm_24_so_asym_pt2(a,b) * wm_interm_47_so_asym_pt2(a,b,p,q)
term(340) = term(340) + wm_interm_28_so_asym_pt2(a,b,p,q) * wm_interm_93_so_asym_pt2(b,a)
term(341) = term(341) + wm_interm_28_so_asym_pt2(a,b,p,q) * wm_interm_94_so_asym_pt2(b,a)
term(342) = term(342) + wm_interm_31_so_asym_pt2(a,b,p,q) * wm_interm_93_so_asym_pt2(b,a)
term(343) = term(343) + wm_interm_31_so_asym_pt2(a,b,p,q) * wm_interm_94_so_asym_pt2(b,a)
term(344) = term(344) + wm_interm_38_so_asym_pt2(a,b) * wm_interm_55_so_asym_pt2(a,b,p,q)
term(345) = term(345) + wm_interm_39_so_asym_pt2(a,b) * wm_interm_55_so_asym_pt2(a,b,p,q)
term(346) = term(346) + wm_interm_30_so_asym_pt2(a,b,p,q) * wm_interm_93_so_asym_pt2(b,a)
term(347) = term(347) + wm_interm_30_so_asym_pt2(a,b,p,q) * wm_interm_94_so_asym_pt2(b,a)
term(348) = term(348) + wm_interm_38_so_asym_pt2(a,b) * wm_interm_49_so_asym_pt2(a,b,p,q)
term(349) = term(349) + wm_interm_38_so_asym_pt2(a,b) * wm_interm_47_so_asym_pt2(a,b,p,q)
term(350) = term(350) + wm_interm_39_so_asym_pt2(a,b) * wm_interm_49_so_asym_pt2(a,b,p,q)
term(351) = term(351) + wm_interm_39_so_asym_pt2(a,b) * wm_interm_47_so_asym_pt2(a,b,p,q)
end do 
end do 

term(277) = term(277) * (-2.0d+0) 
term(278) = term(278) * (4.0d+0) 
term(279) = term(279) * (-2.0d+0) 
term(280) = term(280) * (4.0d+0) 
term(281) = term(281) * (-2.0d+0) 
term(283) = term(283) * (-2.0d+0) 
term(284) = term(284) * (-2.0d+0) 
term(285) = term(285) * (4.0d+0) 
term(287) = term(287) * (-2.0d+0) 
term(289) = term(289) * (-2.0d+0) 
term(291) = term(291) * (-2.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * (4.0d+0) 
term(294) = term(294) * (-2.0d+0) 
term(295) = term(295) * (4.0d+0) 
term(296) = term(296) * (-2.0d+0) 
term(297) = term(297) * (-2.0d+0) 
term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * (4.0d+0) 
term(301) = term(301) * (-2.0d+0) 
term(305) = term(305) * (-2.0d+0) 
term(306) = term(306) * (4.0d+0) 
term(307) = term(307) * (-4.0d+0) 
term(308) = term(308) * (-8.0d+0) 
term(309) = term(309) * (8.0d+0) 
term(310) = term(310) * (4.0d+0) 
term(311) = term(311) * (-8.0d+0) 
term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * (8.0d+0) 
term(314) = term(314) * (-8.0d+0) 
term(315) = term(315) * (8.0d+0) 
term(316) = term(316) * (-8.0d+0) 
term(317) = term(317) * (8.0d+0) 
term(318) = term(318) * (4.0d+0) 
term(319) = term(319) * (-4.0d+0) 
term(320) = term(320) * (4.0d+0) 
term(321) = term(321) * (-4.0d+0) 
term(323) = term(323) * (-2.0d+0) 
term(325) = term(325) * (-2.0d+0) 
term(326) = term(326) * (4.0d+0) 
term(327) = term(327) * (-2.0d+0) 
term(330) = term(330) * (-2.0d+0) 
term(332) = term(332) * (-2.0d+0) 
term(335) = term(335) * (-2.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (-2.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (4.0d+0) 
term(341) = term(341) * (-4.0d+0) 
term(342) = term(342) * (-8.0d+0) 
term(343) = term(343) * (8.0d+0) 
term(344) = term(344) * (4.0d+0) 
term(345) = term(345) * (-4.0d+0) 
term(346) = term(346) * (4.0d+0) 
term(347) = term(347) * (-4.0d+0) 
term(348) = term(348) * (4.0d+0) 
term(349) = term(349) * (-8.0d+0) 
term(350) = term(350) * (-4.0d+0) 
term(351) = term(351) * (8.0d+0) 

do a = nocc + 1, nactive 
term(352) = term(352) + r1(vrdav_Rl, a,p) * wm_interm_59_so_asym_pt2(a,q)
term(353) = term(353) + r1(vrdav_Rl, a,p) * wm_interm_60_so_asym_pt2(a,q)
term(354) = term(354) + r1(vrdav_Rl, a,p) * wm_interm_61_so_asym_pt2(a,q)
term(355) = term(355) + r1(vrdav_Rl, a,p) * wm_interm_62_so_asym_pt2(a,q)
term(356) = term(356) + r1(vrdav_Rl, a,p) * wm_interm_63_so_asym_pt2(a,q)
term(357) = term(357) + s1(a,p) * wm_interm_19_so_asym_pt2(a,q)
term(358) = term(358) + s1(a,p) * wm_interm_20_so_asym_pt2(a,q)
term(359) = term(359) + s1(a,p) * wm_interm_25_so_asym_pt2(a,q)
term(360) = term(360) + s1(a,p) * wm_interm_36_so_asym_pt2(a,q)
term(361) = term(361) + s1(a,p) * wm_interm_37_so_asym_pt2(a,q)
term(362) = term(362) + r1(vrdav_Rr, a,q) * wm_interm_64_so_asym_pt2(a,p)
term(363) = term(363) + r1(vrdav_Rr, a,q) * wm_interm_65_so_asym_pt2(a,p)
term(364) = term(364) + t1(a,q) * wm_interm_45_so_asym_pt2(a,p)
term(365) = term(365) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_52_so_asym_pt2(a,p)
term(366) = term(366) + wm_interm_52_so_asym_pt2(a,p) * wm_interm_5_so_asym_pt2(a,q)
end do 

term(352) = term(352) * (-2.0d+0) 
term(355) = term(355) * (-4.0d+0) 
term(356) = term(356) * (4.0d+0) 
term(357) = term(357) * (-1.0d+0) 
term(358) = term(358) * (2.0d+0) 
term(359) = term(359) * (-1.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (4.0d+0) 
term(363) = term(363) * (-2.0d+0) 
term(364) = term(364) * (-2.0d+0) 
term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (4.0d+0) 


    calc_D_oo_wm_so_asym_pt2 = zero
    do s = 0, 366
    calc_D_oo_wm_so_asym_pt2 = calc_D_oo_wm_so_asym_pt2 + term(s)
    end do

    end function calc_D_oo_wm_so_asym_pt2
    
    function calc_D_ov_wm_so_asym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_asym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b 
    real(F64), dimension(0:61) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_47_so_asym_pt2(q,a,i,j) * wm_interm_48_so_asym_pt2(a,i,j,p)
term(1) = term(1) + wm_interm_47_so_asym_pt2(q,a,i,j) * wm_interm_48_so_asym_pt2(a,i,p,j)
term(2) = term(2) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,p,j,i)
term(3) = term(3) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,p,j,i)
term(4) = term(4) + wm_interm_48_so_asym_pt2(a,i,j,p) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(5) = term(5) + wm_interm_48_so_asym_pt2(a,i,p,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(6) = term(6) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,j,p,i)
term(7) = term(7) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,j,p,i)
term(8) = term(8) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,p,j,i)
term(9) = term(9) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,p,j,i)
term(10) = term(10) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,j,p,i)
term(11) = term(11) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,j,p,i)
term(12) = term(12) + wm_interm_48_so_asym_pt2(a,i,p,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(13) = term(13) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,p,j,i)
term(14) = term(14) + wm_interm_48_so_asym_pt2(a,i,j,p) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(15) = term(15) + wm_interm_48_so_asym_pt2(a,i,p,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(16) = term(16) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,p,j,i)
term(17) = term(17) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,p,j,i)
term(18) = term(18) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_46_so_asym_pt2(a,j,p,i)
term(19) = term(19) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,j,p,i)
term(20) = term(20) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_50_so_asym_pt2(a,j,p,i)
end do 
end do 
end do 

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-8.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (-1.0d+0) 
term(20) = term(20) * (2.0d+0) 

do a = nocc + 1, nactive 
term(21) = term(21) + wm_interm_45_so_asym_pt2(a,p) * wm_interm_4_so_asym_pt2(q,a)
term(22) = term(22) + wm_interm_45_so_asym_pt2(a,p) * wm_interm_8_so_asym_pt2(q,a)
term(23) = term(23) + wm_interm_52_so_asym_pt2(a,p) * wm_interm_53_so_asym_pt2(q,a)
term(24) = term(24) + wm_interm_52_so_asym_pt2(a,p) * wm_interm_54_so_asym_pt2(q,a)
end do 

term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(25) = term(25) + wm_interm_48_so_asym_pt2(q,i,p,j) * wm_interm_56_so_asym_pt2(i,j)
term(26) = term(26) + wm_interm_48_so_asym_pt2(q,i,p,j) * wm_interm_58_so_asym_pt2(i,j)
end do 
end do 

term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(27) = term(27) + wm_interm_48_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(p,i,j,k)
term(28) = term(28) + wm_interm_48_so_asym_pt2(q,i,j,k) * wm_interm_57_so_asym_pt2(i,p,k,j)
end do 
end do 
end do 

term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (-1.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(29) = term(29) + wm_interm_22_so_asym_pt2(i,p,j,k) * wm_interm_50_so_asym_pt2(q,j,k,i)
term(30) = term(30) + wm_interm_22_so_asym_pt2(p,i,j,k) * wm_interm_46_so_asym_pt2(q,j,k,i)
end do 
end do 
end do 

term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(31) = term(31) + wm_interm_51_so_asym_pt2(q,a,p,i) * wm_interm_52_so_asym_pt2(a,i)
term(32) = term(32) + wm_interm_29_so_asym_pt2(q,a,p,i) * wm_interm_45_so_asym_pt2(a,i)
term(33) = term(33) + r2(vrdav_Rl, a,p,q,i) * wm_interm_59_so_asym_pt2(a,i)
term(34) = term(34) + r2(vrdav_Rl, a,p,q,i) * wm_interm_60_so_asym_pt2(a,i)
term(35) = term(35) + r2(vrdav_Rl, a,p,q,i) * wm_interm_61_so_asym_pt2(a,i)
term(36) = term(36) + r2(vrdav_Rl, a,p,q,i) * wm_interm_62_so_asym_pt2(a,i)
term(37) = term(37) + r2(vrdav_Rl, a,p,q,i) * wm_interm_63_so_asym_pt2(a,i)
end do 
end do 

term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(38) = term(38) + r2(vrdav_Rl, a,j,q,i) * wm_interm_95_so_asym_pt2(a,j,i,p)
term(39) = term(39) + r2(vrdav_Rl, a,j,q,i) * wm_interm_95_so_asym_pt2(a,i,j,p)
term(40) = term(40) + r2(vrdav_Rl, a,j,q,i) * wm_interm_96_so_asym_pt2(a,i,j,p)
term(41) = term(41) + r2(vrdav_Rl, a,j,q,i) * wm_interm_97_so_asym_pt2(a,i,j,p)
term(42) = term(42) + r2(vrdav_Rl, a,j,q,i) * wm_interm_97_so_asym_pt2(a,j,i,p)
end do 
end do 
end do 

term(38) = term(38) * (-1.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(43) = term(43) + r2(vrdav_Rl, a,p,q,i) * r1(vrdav_Rr, b,i) * wm_interm_4_so_asym_pt2(b,a)
term(44) = term(44) + r2(vrdav_Rl, a,p,q,i) * r1(vrdav_Rr, b,i) * wm_interm_8_so_asym_pt2(b,a)
end do 
end do 
end do 

term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(45) = term(45) + r1(vrdav_Rr, a,i) * s2(b,q,p,i) * wm_interm_53_so_asym_pt2(a,b)
term(46) = term(46) + r1(vrdav_Rr, a,i) * s2(b,q,p,i) * wm_interm_54_so_asym_pt2(a,b)
end do 
end do 
end do 

term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(47) = term(47) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_46_so_asym_pt2(q,p,j,i)
term(48) = term(48) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_46_so_asym_pt2(q,p,j,i)
term(49) = term(49) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_50_so_asym_pt2(q,j,p,i)
term(50) = term(50) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_50_so_asym_pt2(q,j,p,i)
end do 
end do 

term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (2.0d+0) 

do i = 1, nocc 
term(51) = term(51) + wm_interm_1_so_asym_pt2(p,i) * wm_interm_45_so_asym_pt2(q,i)
term(52) = term(52) + wm_interm_2_so_asym_pt2(p,i) * wm_interm_45_so_asym_pt2(q,i)
term(53) = term(53) + wm_interm_52_so_asym_pt2(q,i) * wm_interm_56_so_asym_pt2(p,i)
term(54) = term(54) + wm_interm_52_so_asym_pt2(q,i) * wm_interm_58_so_asym_pt2(p,i)
term(55) = term(55) + s1(q,i) * wm_interm_66_so_asym_pt2(p,i)
term(56) = term(56) + r1(vrdav_Rl, q,i) * wm_interm_67_so_asym_pt2(i,p)
term(57) = term(57) + s1(q,i) * wm_interm_74_so_asym_pt2(p,i)
term(58) = term(58) + s1(q,i) * wm_interm_75_so_asym_pt2(p,i)
term(59) = term(59) + s1(q,i) * wm_interm_73_so_asym_pt2(p,i)
term(60) = term(60) + s1(q,i) * wm_interm_87_so_asym_pt2(p,i)
term(61) = term(61) + s1(q,i) * wm_interm_88_so_asym_pt2(p,i)
end do 

term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-1.0d+0) 
term(58) = term(58) * (2.0d+0) 
term(59) = term(59) * (-1.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (4.0d+0) 


    calc_D_ov_wm_so_asym_pt2 = zero
    do s = 0, 61
    calc_D_ov_wm_so_asym_pt2 = calc_D_ov_wm_so_asym_pt2 + term(s)
    end do

    end function calc_D_ov_wm_so_asym_pt2
    
    function calc_D_vo_wm_so_asym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_asym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b 
    real(F64), dimension(0:177) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_21_so_asym_pt2(a,i,q,j) * wm_interm_6_so_asym_pt2(p,a,i,j)
term(1) = term(1) + wm_interm_21_so_asym_pt2(a,i,q,j) * wm_interm_7_so_asym_pt2(p,a,i,j)
term(2) = term(2) + wm_interm_21_so_asym_pt2(a,i,q,j) * wm_interm_9_so_asym_pt2(p,a,i,j)
term(3) = term(3) + wm_interm_10_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(4) = term(4) + wm_interm_11_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(5) = term(5) + wm_interm_12_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(6) = term(6) + wm_interm_21_so_asym_pt2(a,i,j,q) * wm_interm_9_so_asym_pt2(p,a,i,j)
term(7) = term(7) + wm_interm_10_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,j,q)
term(8) = term(8) + wm_interm_21_so_asym_pt2(a,i,j,q) * wm_interm_7_so_asym_pt2(p,a,i,j)
term(9) = term(9) + wm_interm_21_so_asym_pt2(a,i,j,q) * wm_interm_6_so_asym_pt2(p,a,i,j)
term(10) = term(10) + wm_interm_21_so_asym_pt2(a,i,j,q) * wm_interm_27_so_asym_pt2(p,a,i,j)
term(11) = term(11) + wm_interm_12_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,j,q)
term(12) = term(12) + wm_interm_0_so_asym_pt2(a,i,q,j) * wm_interm_28_so_asym_pt2(a,p,j,i)
term(13) = term(13) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,i,q)
term(14) = term(14) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,q,i)
term(15) = term(15) + wm_interm_0_so_asym_pt2(a,i,q,j) * wm_interm_29_so_asym_pt2(a,p,j,i)
term(16) = term(16) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,q,i)
term(17) = term(17) + wm_interm_0_so_asym_pt2(a,i,q,j) * wm_interm_30_so_asym_pt2(a,p,j,i)
term(18) = term(18) + wm_interm_0_so_asym_pt2(a,i,q,j) * wm_interm_31_so_asym_pt2(a,p,j,i)
term(19) = term(19) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,i,q)
term(20) = term(20) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,i,q)
term(21) = term(21) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,q,i)
term(22) = term(22) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_3_so_asym_pt2(a,j,q,i)
term(23) = term(23) + wm_interm_15_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(24) = term(24) + wm_interm_16_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(25) = term(25) + wm_interm_17_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(26) = term(26) + wm_interm_18_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,q,j)
term(27) = term(27) + wm_interm_17_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,j,q)
term(28) = term(28) + wm_interm_18_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,j,q)
term(29) = term(29) + wm_interm_16_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,j,q)
term(30) = term(30) + wm_interm_15_so_asym_pt2(p,a,i,j) * wm_interm_21_so_asym_pt2(a,i,j,q)
term(31) = term(31) + wm_interm_14_so_asym_pt2(a,i,q,j) * wm_interm_28_so_asym_pt2(a,p,j,i)
term(32) = term(32) + wm_interm_14_so_asym_pt2(a,i,q,j) * wm_interm_29_so_asym_pt2(a,p,j,i)
term(33) = term(33) + wm_interm_14_so_asym_pt2(a,i,q,j) * wm_interm_30_so_asym_pt2(a,p,j,i)
term(34) = term(34) + wm_interm_14_so_asym_pt2(a,i,q,j) * wm_interm_31_so_asym_pt2(a,p,j,i)
term(35) = term(35) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_99_so_asym_pt2(a,j,i,q)
term(36) = term(36) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_98_so_asym_pt2(a,i,j,q)
term(37) = term(37) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_98_so_asym_pt2(a,i,j,q)
term(38) = term(38) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_98_so_asym_pt2(a,j,i,q)
term(39) = term(39) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_99_so_asym_pt2(a,j,i,q)
term(40) = term(40) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_99_so_asym_pt2(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-1.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (8.0d+0) 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (4.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-8.0d+0) 
term(35) = term(35) * (-0.5d+0) 
term(36) = term(36) * (-0.5d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (2.0d+0) 

do a = nocc + 1, nactive 
term(41) = term(41) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_23_so_asym_pt2(p,a)
term(42) = term(42) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_24_so_asym_pt2(p,a)
term(43) = term(43) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_26_so_asym_pt2(p,a)
term(44) = term(44) + wm_interm_23_so_asym_pt2(p,a) * wm_interm_5_so_asym_pt2(a,q)
term(45) = term(45) + wm_interm_24_so_asym_pt2(p,a) * wm_interm_5_so_asym_pt2(a,q)
term(46) = term(46) + wm_interm_26_so_asym_pt2(p,a) * wm_interm_5_so_asym_pt2(a,q)
term(47) = term(47) + wm_interm_19_so_asym_pt2(a,q) * wm_interm_4_so_asym_pt2(a,p)
term(48) = term(48) + wm_interm_20_so_asym_pt2(a,q) * wm_interm_4_so_asym_pt2(a,p)
term(49) = term(49) + wm_interm_25_so_asym_pt2(a,q) * wm_interm_4_so_asym_pt2(a,p)
term(50) = term(50) + wm_interm_19_so_asym_pt2(a,q) * wm_interm_8_so_asym_pt2(a,p)
term(51) = term(51) + wm_interm_20_so_asym_pt2(a,q) * wm_interm_8_so_asym_pt2(a,p)
term(52) = term(52) + wm_interm_25_so_asym_pt2(a,q) * wm_interm_8_so_asym_pt2(a,p)
term(53) = term(53) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_38_so_asym_pt2(p,a)
term(54) = term(54) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_39_so_asym_pt2(p,a)
term(55) = term(55) + wm_interm_13_so_asym_pt2(a,q) * wm_interm_40_so_asym_pt2(p,a)
term(56) = term(56) + wm_interm_38_so_asym_pt2(p,a) * wm_interm_5_so_asym_pt2(a,q)
term(57) = term(57) + wm_interm_39_so_asym_pt2(p,a) * wm_interm_5_so_asym_pt2(a,q)
term(58) = term(58) + wm_interm_40_so_asym_pt2(p,a) * wm_interm_5_so_asym_pt2(a,q)
term(59) = term(59) + wm_interm_36_so_asym_pt2(a,q) * wm_interm_4_so_asym_pt2(a,p)
term(60) = term(60) + wm_interm_37_so_asym_pt2(a,q) * wm_interm_4_so_asym_pt2(a,p)
term(61) = term(61) + wm_interm_36_so_asym_pt2(a,q) * wm_interm_8_so_asym_pt2(a,p)
term(62) = term(62) + wm_interm_37_so_asym_pt2(a,q) * wm_interm_8_so_asym_pt2(a,p)
end do 

term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-1.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(63) = term(63) + wm_interm_0_so_asym_pt2(p,i,j,k) * wm_interm_22_so_asym_pt2(j,k,i,q)
term(64) = term(64) + wm_interm_0_so_asym_pt2(p,i,j,k) * wm_interm_22_so_asym_pt2(k,j,i,q)
term(65) = term(65) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_32_so_asym_pt2(q,i,j,k)
term(66) = term(66) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_32_so_asym_pt2(i,q,k,j)
term(67) = term(67) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_32_so_asym_pt2(q,i,k,j)
term(68) = term(68) + wm_interm_14_so_asym_pt2(p,i,j,k) * wm_interm_22_so_asym_pt2(k,j,i,q)
term(69) = term(69) + wm_interm_14_so_asym_pt2(p,i,j,k) * wm_interm_22_so_asym_pt2(j,k,i,q)
term(70) = term(70) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_41_so_asym_pt2(q,i,j,k)
term(71) = term(71) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_41_so_asym_pt2(i,q,k,j)
term(72) = term(72) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_41_so_asym_pt2(q,i,k,j)
end do 
end do 
end do 

term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (-1.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (-2.0d+0) 
term(72) = term(72) * (4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(73) = term(73) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_4_so_asym_pt2(b,a)
term(74) = term(74) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_8_so_asym_pt2(b,a)
end do 
end do 
end do 

term(73) = term(73) * (-1.0d+0) 
term(74) = term(74) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(75) = term(75) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_4_so_asym_pt2(b,a)
term(76) = term(76) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_8_so_asym_pt2(b,a)
end do 
end do 
end do 

term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(77) = term(77) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_4_so_asym_pt2(b,a)
term(78) = term(78) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_8_so_asym_pt2(b,a)
term(79) = term(79) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_4_so_asym_pt2(b,a)
term(80) = term(80) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_8_so_asym_pt2(b,a)
term(81) = term(81) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_26_so_asym_pt2(b,a)
term(82) = term(82) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_23_so_asym_pt2(b,a)
term(83) = term(83) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_24_so_asym_pt2(b,a)
term(84) = term(84) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_38_so_asym_pt2(b,a)
term(85) = term(85) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_39_so_asym_pt2(b,a)
end do 
end do 
end do 

term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (8.0d+0) 
term(81) = term(81) * (-1.0d+0) 
term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(86) = term(86) + wm_interm_22_so_asym_pt2(i,j,k,q) * wm_interm_3_so_asym_pt2(p,k,j,i)
end do 
end do 
end do 

term(86) = term(86) * (-1.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(87) = term(87) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_4_so_asym_pt2(b,a)
term(88) = term(88) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_8_so_asym_pt2(b,a)
end do 
end do 
end do 

term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(89) = term(89) + wm_interm_0_so_asym_pt2(p,i,j,q) * wm_interm_1_so_asym_pt2(j,i)
term(90) = term(90) + wm_interm_0_so_asym_pt2(p,i,q,j) * wm_interm_1_so_asym_pt2(j,i)
term(91) = term(91) + wm_interm_0_so_asym_pt2(p,i,j,q) * wm_interm_2_so_asym_pt2(j,i)
term(92) = term(92) + wm_interm_0_so_asym_pt2(p,i,q,j) * wm_interm_2_so_asym_pt2(j,i)
term(93) = term(93) + wm_interm_14_so_asym_pt2(p,i,q,j) * wm_interm_1_so_asym_pt2(j,i)
term(94) = term(94) + wm_interm_14_so_asym_pt2(p,i,j,q) * wm_interm_1_so_asym_pt2(j,i)
term(95) = term(95) + wm_interm_14_so_asym_pt2(p,i,q,j) * wm_interm_2_so_asym_pt2(j,i)
term(96) = term(96) + wm_interm_14_so_asym_pt2(p,i,j,q) * wm_interm_2_so_asym_pt2(j,i)
term(97) = term(97) + wm_interm_21_so_asym_pt2(p,i,q,j) * wm_interm_33_so_asym_pt2(i,j)
term(98) = term(98) + wm_interm_21_so_asym_pt2(p,i,q,j) * wm_interm_34_so_asym_pt2(i,j)
term(99) = term(99) + wm_interm_21_so_asym_pt2(p,i,q,j) * wm_interm_35_so_asym_pt2(i,j)
term(100) = term(100) + wm_interm_21_so_asym_pt2(p,i,q,j) * wm_interm_42_so_asym_pt2(i,j)
term(101) = term(101) + wm_interm_21_so_asym_pt2(p,i,q,j) * wm_interm_43_so_asym_pt2(i,j)
term(102) = term(102) + wm_interm_21_so_asym_pt2(p,i,q,j) * wm_interm_44_so_asym_pt2(i,j)
end do 
end do 

term(89) = term(89) * (-1.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (2.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (8.0d+0) 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * (-1.0d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-1.0d+0) 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * (4.0d+0) 
term(102) = term(102) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(103) = term(103) + wm_interm_0_so_asym_pt2(a,i,j,q) * wm_interm_29_so_asym_pt2(a,p,j,i)
term(104) = term(104) + wm_interm_14_so_asym_pt2(a,i,j,q) * wm_interm_28_so_asym_pt2(a,p,j,i)
term(105) = term(105) + wm_interm_14_so_asym_pt2(a,i,j,q) * wm_interm_29_so_asym_pt2(a,p,j,i)
term(106) = term(106) + wm_interm_14_so_asym_pt2(a,i,j,q) * wm_interm_30_so_asym_pt2(a,p,j,i)
term(107) = term(107) + wm_interm_14_so_asym_pt2(a,i,j,q) * wm_interm_31_so_asym_pt2(a,p,j,i)
end do 
end do 
end do 

term(103) = term(103) * (-1.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (4.0d+0) 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * (8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(108) = term(108) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_98_so_asym_pt2(a,j,i,q)
term(109) = term(109) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_98_so_asym_pt2(a,i,j,q)
term(110) = term(110) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_99_so_asym_pt2(a,i,j,q)
term(111) = term(111) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_99_so_asym_pt2(a,j,i,q)
end do 
end do 
end do 

term(108) = term(108) * (-0.5d+0) 
term(110) = term(110) * (-0.5d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(112) = term(112) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_3_so_asym_pt2(p,j,q,i)
term(113) = term(113) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_3_so_asym_pt2(p,j,q,i)
end do 
end do 

term(112) = term(112) * (-1.0d+0) 
term(113) = term(113) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(114) = term(114) + wm_interm_5_so_asym_pt2(a,i) * wm_interm_6_so_asym_pt2(p,a,q,i)
term(115) = term(115) + wm_interm_5_so_asym_pt2(a,i) * wm_interm_7_so_asym_pt2(p,a,q,i)
term(116) = term(116) + wm_interm_5_so_asym_pt2(a,i) * wm_interm_9_so_asym_pt2(p,a,q,i)
term(117) = term(117) + wm_interm_10_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(118) = term(118) + wm_interm_11_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(119) = term(119) + wm_interm_12_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(120) = term(120) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_6_so_asym_pt2(p,a,q,i)
term(121) = term(121) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_7_so_asym_pt2(p,a,q,i)
term(122) = term(122) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_9_so_asym_pt2(p,a,q,i)
term(123) = term(123) + wm_interm_10_so_asym_pt2(p,a,q,i) * wm_interm_13_so_asym_pt2(a,i)
term(124) = term(124) + wm_interm_11_so_asym_pt2(p,a,q,i) * wm_interm_13_so_asym_pt2(a,i)
term(125) = term(125) + wm_interm_12_so_asym_pt2(p,a,q,i) * wm_interm_13_so_asym_pt2(a,i)
term(126) = term(126) + wm_interm_15_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(127) = term(127) + wm_interm_16_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(128) = term(128) + wm_interm_17_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(129) = term(129) + wm_interm_18_so_asym_pt2(p,a,q,i) * wm_interm_5_so_asym_pt2(a,i)
term(130) = term(130) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_15_so_asym_pt2(p,a,q,i)
term(131) = term(131) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_16_so_asym_pt2(p,a,q,i)
term(132) = term(132) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_17_so_asym_pt2(p,a,q,i)
term(133) = term(133) + wm_interm_13_so_asym_pt2(a,i) * wm_interm_18_so_asym_pt2(p,a,q,i)
term(134) = term(134) + wm_interm_19_so_asym_pt2(a,i) * wm_interm_29_so_asym_pt2(a,p,i,q)
term(135) = term(135) + wm_interm_20_so_asym_pt2(a,i) * wm_interm_29_so_asym_pt2(a,p,i,q)
term(136) = term(136) + wm_interm_25_so_asym_pt2(a,i) * wm_interm_29_so_asym_pt2(a,p,i,q)
term(137) = term(137) + wm_interm_29_so_asym_pt2(a,p,i,q) * wm_interm_36_so_asym_pt2(a,i)
term(138) = term(138) + wm_interm_29_so_asym_pt2(a,p,i,q) * wm_interm_37_so_asym_pt2(a,i)
term(139) = term(139) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_64_so_asym_pt2(a,i)
term(140) = term(140) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_64_so_asym_pt2(a,i)
term(141) = term(141) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_65_so_asym_pt2(a,i)
term(142) = term(142) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_65_so_asym_pt2(a,i)
term(143) = term(143) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_64_so_asym_pt2(a,i)
term(144) = term(144) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_64_so_asym_pt2(a,i)
term(145) = term(145) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_65_so_asym_pt2(a,i)
term(146) = term(146) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_65_so_asym_pt2(a,i)
end do 
end do 

term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (2.0d+0) 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * (-1.0d+0) 
term(121) = term(121) * (2.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (-1.0d+0) 
term(125) = term(125) * (2.0d+0) 
term(126) = term(126) * (8.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (-16.0d+0) 
term(129) = term(129) * (16.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (4.0d+0) 
term(132) = term(132) * (8.0d+0) 
term(133) = term(133) * (-8.0d+0) 
term(134) = term(134) * (-1.0d+0) 
term(135) = term(135) * (2.0d+0) 
term(136) = term(136) * (-1.0d+0) 
term(137) = term(137) * (-4.0d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (-4.0d+0) 
term(140) = term(140) * (2.0d+0) 
term(141) = term(141) * (2.0d+0) 
term(142) = term(142) * (-1.0d+0) 
term(143) = term(143) * (8.0d+0) 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(147) = term(147) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_64_so_asym_pt2(a,i)
term(148) = term(148) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_65_so_asym_pt2(a,i)
end do 
end do 

term(147) = term(147) * (2.0d+0) 
term(148) = term(148) * (-1.0d+0) 

do i = 1, nocc 
term(149) = term(149) + wm_interm_19_so_asym_pt2(p,i) * wm_interm_1_so_asym_pt2(i,q)
term(150) = term(150) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_20_so_asym_pt2(p,i)
term(151) = term(151) + wm_interm_19_so_asym_pt2(p,i) * wm_interm_2_so_asym_pt2(i,q)
term(152) = term(152) + wm_interm_20_so_asym_pt2(p,i) * wm_interm_2_so_asym_pt2(i,q)
term(153) = term(153) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_25_so_asym_pt2(p,i)
term(154) = term(154) + wm_interm_25_so_asym_pt2(p,i) * wm_interm_2_so_asym_pt2(i,q)
term(155) = term(155) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_33_so_asym_pt2(q,i)
term(156) = term(156) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_34_so_asym_pt2(q,i)
term(157) = term(157) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_35_so_asym_pt2(q,i)
term(158) = term(158) + wm_interm_33_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
term(159) = term(159) + wm_interm_34_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
term(160) = term(160) + wm_interm_35_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
term(161) = term(161) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_36_so_asym_pt2(p,i)
term(162) = term(162) + wm_interm_1_so_asym_pt2(i,q) * wm_interm_37_so_asym_pt2(p,i)
term(163) = term(163) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_36_so_asym_pt2(p,i)
term(164) = term(164) + wm_interm_2_so_asym_pt2(i,q) * wm_interm_37_so_asym_pt2(p,i)
term(165) = term(165) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_42_so_asym_pt2(q,i)
term(166) = term(166) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_43_so_asym_pt2(q,i)
term(167) = term(167) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_44_so_asym_pt2(q,i)
term(168) = term(168) + wm_interm_42_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
term(169) = term(169) + wm_interm_43_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
term(170) = term(170) + wm_interm_44_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
term(171) = term(171) + r1(vrdav_Rr, p,i) * wm_interm_68_so_asym_pt2(i,q)
term(172) = term(172) + t1(p,i) * wm_interm_66_so_asym_pt2(i,q)
term(173) = term(173) + t1(p,i) * wm_interm_74_so_asym_pt2(i,q)
term(174) = term(174) + t1(p,i) * wm_interm_75_so_asym_pt2(i,q)
term(175) = term(175) + t1(p,i) * wm_interm_73_so_asym_pt2(i,q)
term(176) = term(176) + t1(p,i) * wm_interm_87_so_asym_pt2(i,q)
term(177) = term(177) + t1(p,i) * wm_interm_88_so_asym_pt2(i,q)
end do 

term(149) = term(149) * (-1.0d+0) 
term(150) = term(150) * (2.0d+0) 
term(151) = term(151) * (2.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (-1.0d+0) 
term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (-1.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-1.0d+0) 
term(158) = term(158) * (2.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (4.0d+0) 
term(163) = term(163) * (8.0d+0) 
term(164) = term(164) * (-8.0d+0) 
term(165) = term(165) * (-2.0d+0) 
term(166) = term(166) * (4.0d+0) 
term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (-8.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (2.0d+0) 
term(172) = term(172) * (2.0d+0) 
term(173) = term(173) * (-1.0d+0) 
term(174) = term(174) * (2.0d+0) 
term(175) = term(175) * (-1.0d+0) 
term(176) = term(176) * (-4.0d+0) 
term(177) = term(177) * (4.0d+0) 


    calc_D_vo_wm_so_asym_pt2 = zero
    do s = 0, 177
    calc_D_vo_wm_so_asym_pt2 = calc_D_vo_wm_so_asym_pt2 + term(s)
    end do

    end function calc_D_vo_wm_so_asym_pt2
    
    function calc_D_vv_wm_so_asym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_asym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b, l 
    real(F64), dimension(0:340) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_69_so_asym_pt2(a,p,j,i)
term(1) = term(1) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_69_so_asym_pt2(a,p,j,i)
term(2) = term(2) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_69_so_asym_pt2(a,p,j,i)
term(3) = term(3) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_69_so_asym_pt2(a,p,j,i)
term(4) = term(4) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_69_so_asym_pt2(q,a,j,i)
term(5) = term(5) + wm_interm_47_so_asym_pt2(q,a,i,j) * wm_interm_6_so_asym_pt2(p,a,i,j)
term(6) = term(6) + wm_interm_47_so_asym_pt2(q,a,i,j) * wm_interm_7_so_asym_pt2(p,a,i,j)
term(7) = term(7) + wm_interm_47_so_asym_pt2(q,a,i,j) * wm_interm_9_so_asym_pt2(p,a,i,j)
term(8) = term(8) + wm_interm_10_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(9) = term(9) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_78_so_asym_pt2(a,p,j,i)
term(10) = term(10) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_78_so_asym_pt2(a,p,j,i)
term(11) = term(11) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_79_so_asym_pt2(a,p,j,i)
term(12) = term(12) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_79_so_asym_pt2(a,p,j,i)
term(13) = term(13) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_80_so_asym_pt2(a,p,j,i)
term(14) = term(14) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_80_so_asym_pt2(a,p,j,i)
term(15) = term(15) + wm_interm_11_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(16) = term(16) + wm_interm_12_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(17) = term(17) + wm_interm_49_so_asym_pt2(q,a,i,j) * wm_interm_6_so_asym_pt2(p,a,i,j)
term(18) = term(18) + wm_interm_49_so_asym_pt2(q,a,i,j) * wm_interm_7_so_asym_pt2(p,a,i,j)
term(19) = term(19) + wm_interm_49_so_asym_pt2(q,a,i,j) * wm_interm_9_so_asym_pt2(p,a,i,j)
term(20) = term(20) + wm_interm_10_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(21) = term(21) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_77_so_asym_pt2(a,p,j,i)
term(22) = term(22) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_77_so_asym_pt2(a,p,j,i)
term(23) = term(23) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_76_so_asym_pt2(a,p,j,i)
term(24) = term(24) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_76_so_asym_pt2(a,p,j,i)
term(25) = term(25) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_82_so_asym_pt2(a,p,j,i)
term(26) = term(26) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_82_so_asym_pt2(a,p,j,i)
term(27) = term(27) + wm_interm_11_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(28) = term(28) + wm_interm_12_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(29) = term(29) + wm_interm_51_so_asym_pt2(q,a,i,j) * wm_interm_9_so_asym_pt2(p,a,i,j)
term(30) = term(30) + wm_interm_10_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(31) = term(31) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_77_so_asym_pt2(a,p,j,i)
term(32) = term(32) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_76_so_asym_pt2(a,p,j,i)
term(33) = term(33) + wm_interm_51_so_asym_pt2(q,a,i,j) * wm_interm_7_so_asym_pt2(p,a,i,j)
term(34) = term(34) + wm_interm_51_so_asym_pt2(q,a,i,j) * wm_interm_6_so_asym_pt2(p,a,i,j)
term(35) = term(35) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_76_so_asym_pt2(a,p,j,i)
term(36) = term(36) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_77_so_asym_pt2(a,p,j,i)
term(37) = term(37) + wm_interm_55_so_asym_pt2(q,a,i,j) * wm_interm_6_so_asym_pt2(p,a,i,j)
term(38) = term(38) + wm_interm_55_so_asym_pt2(q,a,i,j) * wm_interm_7_so_asym_pt2(p,a,i,j)
term(39) = term(39) + wm_interm_55_so_asym_pt2(q,a,i,j) * wm_interm_9_so_asym_pt2(p,a,i,j)
term(40) = term(40) + wm_interm_10_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(41) = term(41) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_78_so_asym_pt2(a,p,j,i)
term(42) = term(42) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_78_so_asym_pt2(a,p,j,i)
term(43) = term(43) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_79_so_asym_pt2(a,p,j,i)
term(44) = term(44) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_79_so_asym_pt2(a,p,j,i)
term(45) = term(45) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_82_so_asym_pt2(a,p,j,i)
term(46) = term(46) + wm_interm_12_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(47) = term(47) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_81_so_asym_pt2(a,p,j,i)
term(48) = term(48) + wm_interm_27_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(49) = term(49) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_80_so_asym_pt2(a,p,j,i)
term(50) = term(50) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_80_so_asym_pt2(a,p,j,i)
term(51) = term(51) + wm_interm_11_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(52) = term(52) + wm_interm_12_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(53) = term(53) + wm_interm_15_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(54) = term(54) + wm_interm_16_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(55) = term(55) + wm_interm_17_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(56) = term(56) + wm_interm_18_so_asym_pt2(p,a,i,j) * wm_interm_47_so_asym_pt2(q,a,i,j)
term(57) = term(57) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_91_so_asym_pt2(a,p,j,i)
term(58) = term(58) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_91_so_asym_pt2(a,p,j,i)
term(59) = term(59) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_92_so_asym_pt2(a,p,j,i)
term(60) = term(60) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_92_so_asym_pt2(a,p,j,i)
term(61) = term(61) + wm_interm_15_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(62) = term(62) + wm_interm_16_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(63) = term(63) + wm_interm_17_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(64) = term(64) + wm_interm_18_so_asym_pt2(p,a,i,j) * wm_interm_49_so_asym_pt2(q,a,i,j)
term(65) = term(65) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_90_so_asym_pt2(a,p,j,i)
term(66) = term(66) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_90_so_asym_pt2(a,p,j,i)
term(67) = term(67) + wm_interm_28_so_asym_pt2(q,a,i,j) * wm_interm_89_so_asym_pt2(a,p,j,i)
term(68) = term(68) + wm_interm_31_so_asym_pt2(q,a,i,j) * wm_interm_89_so_asym_pt2(a,p,j,i)
term(69) = term(69) + wm_interm_17_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(70) = term(70) + wm_interm_18_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(71) = term(71) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_90_so_asym_pt2(a,p,j,i)
term(72) = term(72) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_89_so_asym_pt2(a,p,j,i)
term(73) = term(73) + wm_interm_16_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(74) = term(74) + wm_interm_15_so_asym_pt2(p,a,i,j) * wm_interm_51_so_asym_pt2(q,a,i,j)
term(75) = term(75) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_89_so_asym_pt2(a,p,j,i)
term(76) = term(76) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_90_so_asym_pt2(a,p,j,i)
term(77) = term(77) + wm_interm_15_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(78) = term(78) + wm_interm_16_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(79) = term(79) + wm_interm_17_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(80) = term(80) + wm_interm_18_so_asym_pt2(p,a,i,j) * wm_interm_55_so_asym_pt2(q,a,i,j)
term(81) = term(81) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_91_so_asym_pt2(a,p,j,i)
term(82) = term(82) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_91_so_asym_pt2(a,p,j,i)
term(83) = term(83) + wm_interm_29_so_asym_pt2(q,a,i,j) * wm_interm_92_so_asym_pt2(a,p,j,i)
term(84) = term(84) + wm_interm_30_so_asym_pt2(q,a,i,j) * wm_interm_92_so_asym_pt2(a,p,j,i)
term(85) = term(85) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_77_so_asym_pt2(q,a,j,i)
term(86) = term(86) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_82_so_asym_pt2(q,a,j,i)
term(87) = term(87) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_81_so_asym_pt2(q,a,j,i)
term(88) = term(88) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_78_so_asym_pt2(q,a,j,i)
term(89) = term(89) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_79_so_asym_pt2(q,a,j,i)
term(90) = term(90) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_76_so_asym_pt2(q,a,j,i)
term(91) = term(91) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_77_so_asym_pt2(q,a,j,i)
term(92) = term(92) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_81_so_asym_pt2(q,a,j,i)
term(93) = term(93) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_80_so_asym_pt2(q,a,j,i)
term(94) = term(94) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_77_so_asym_pt2(q,a,j,i)
term(95) = term(95) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_77_so_asym_pt2(q,a,j,i)
term(96) = term(96) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_82_so_asym_pt2(q,a,j,i)
term(97) = term(97) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_82_so_asym_pt2(q,a,j,i)
term(98) = term(98) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_81_so_asym_pt2(q,a,j,i)
term(99) = term(99) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_81_so_asym_pt2(q,a,j,i)
term(100) = term(100) + wm_interm_12_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(101) = term(101) + wm_interm_51_so_asym_pt2(a,p,i,j) * wm_interm_9_so_asym_pt2(a,q,i,j)
term(102) = term(102) + wm_interm_10_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(103) = term(103) + wm_interm_11_so_asym_pt2(a,q,i,j) * wm_interm_55_so_asym_pt2(a,p,i,j)
term(104) = term(104) + wm_interm_27_so_asym_pt2(a,q,i,j) * wm_interm_55_so_asym_pt2(a,p,i,j)
term(105) = term(105) + wm_interm_27_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(106) = term(106) + wm_interm_51_so_asym_pt2(a,p,i,j) * wm_interm_7_so_asym_pt2(a,q,i,j)
term(107) = term(107) + wm_interm_51_so_asym_pt2(a,p,i,j) * wm_interm_6_so_asym_pt2(a,q,i,j)
term(108) = term(108) + wm_interm_55_so_asym_pt2(a,p,i,j) * wm_interm_6_so_asym_pt2(a,q,i,j)
term(109) = term(109) + wm_interm_11_so_asym_pt2(a,q,i,j) * wm_interm_49_so_asym_pt2(a,p,i,j)
term(110) = term(110) + wm_interm_27_so_asym_pt2(a,q,i,j) * wm_interm_49_so_asym_pt2(a,p,i,j)
term(111) = term(111) + wm_interm_11_so_asym_pt2(a,q,i,j) * wm_interm_47_so_asym_pt2(a,p,i,j)
term(112) = term(112) + wm_interm_27_so_asym_pt2(a,q,i,j) * wm_interm_47_so_asym_pt2(a,p,i,j)
term(113) = term(113) + wm_interm_49_so_asym_pt2(a,p,i,j) * wm_interm_6_so_asym_pt2(a,q,i,j)
term(114) = term(114) + wm_interm_47_so_asym_pt2(a,p,i,j) * wm_interm_6_so_asym_pt2(a,q,i,j)
term(115) = term(115) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_90_so_asym_pt2(q,a,j,i)
term(116) = term(116) + wm_interm_28_so_asym_pt2(a,p,i,j) * wm_interm_89_so_asym_pt2(q,a,j,i)
term(117) = term(117) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_91_so_asym_pt2(q,a,j,i)
term(118) = term(118) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_92_so_asym_pt2(q,a,j,i)
term(119) = term(119) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_89_so_asym_pt2(q,a,j,i)
term(120) = term(120) + wm_interm_29_so_asym_pt2(a,p,i,j) * wm_interm_90_so_asym_pt2(q,a,j,i)
term(121) = term(121) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_90_so_asym_pt2(q,a,j,i)
term(122) = term(122) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_90_so_asym_pt2(q,a,j,i)
term(123) = term(123) + wm_interm_30_so_asym_pt2(a,p,i,j) * wm_interm_89_so_asym_pt2(q,a,j,i)
term(124) = term(124) + wm_interm_31_so_asym_pt2(a,p,i,j) * wm_interm_89_so_asym_pt2(q,a,j,i)
term(125) = term(125) + wm_interm_17_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(126) = term(126) + wm_interm_18_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(127) = term(127) + wm_interm_15_so_asym_pt2(a,q,i,j) * wm_interm_55_so_asym_pt2(a,p,i,j)
term(128) = term(128) + wm_interm_16_so_asym_pt2(a,q,i,j) * wm_interm_55_so_asym_pt2(a,p,i,j)
term(129) = term(129) + wm_interm_16_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(130) = term(130) + wm_interm_15_so_asym_pt2(a,q,i,j) * wm_interm_51_so_asym_pt2(a,p,i,j)
term(131) = term(131) + wm_interm_15_so_asym_pt2(a,q,i,j) * wm_interm_49_so_asym_pt2(a,p,i,j)
term(132) = term(132) + wm_interm_16_so_asym_pt2(a,q,i,j) * wm_interm_49_so_asym_pt2(a,p,i,j)
term(133) = term(133) + wm_interm_15_so_asym_pt2(a,q,i,j) * wm_interm_47_so_asym_pt2(a,p,i,j)
term(134) = term(134) + wm_interm_16_so_asym_pt2(a,q,i,j) * wm_interm_47_so_asym_pt2(a,p,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (8.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (8.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-1.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (2.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (-1.0d+0) 
term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-1.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (8.0d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (-16.0d+0) 
term(56) = term(56) * (16.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (-16.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (16.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (8.0d+0) 
term(64) = term(64) * (-8.0d+0) 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (8.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (4.0d+0) 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (4.0d+0) 
term(79) = term(79) * (8.0d+0) 
term(80) = term(80) * (-8.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (8.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * (-1.0d+0) 
term(86) = term(86) * (-1.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-1.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (-1.0d+0) 
term(91) = term(91) * (2.0d+0) 
term(92) = term(92) * (-1.0d+0) 
term(93) = term(93) * (-1.0d+0) 
term(94) = term(94) * (-1.0d+0) 
term(95) = term(95) * (2.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-1.0d+0) 
term(101) = term(101) * (-1.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-1.0d+0) 
term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-1.0d+0) 
term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-1.0d+0) 
term(109) = term(109) * (-1.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * (-1.0d+0) 
term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (8.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (4.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(135) = term(135) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(j,i,k,l)
term(136) = term(136) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(135) = term(135) * (-1.0d+0) 
term(136) = term(136) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(137) = term(137) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_69_so_asym_pt2(q,p,j,i)
term(138) = term(138) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_69_so_asym_pt2(q,p,j,i)
term(139) = term(139) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_76_so_asym_pt2(q,p,j,i)
term(140) = term(140) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_77_so_asym_pt2(q,p,j,i)
term(141) = term(141) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_76_so_asym_pt2(q,p,j,i)
term(142) = term(142) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_77_so_asym_pt2(q,p,j,i)
term(143) = term(143) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_78_so_asym_pt2(q,p,j,i)
term(144) = term(144) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_78_so_asym_pt2(q,p,j,i)
term(145) = term(145) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_79_so_asym_pt2(q,p,j,i)
term(146) = term(146) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_79_so_asym_pt2(q,p,j,i)
term(147) = term(147) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_80_so_asym_pt2(q,p,j,i)
term(148) = term(148) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_80_so_asym_pt2(q,p,j,i)
term(149) = term(149) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_81_so_asym_pt2(q,p,j,i)
term(150) = term(150) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_81_so_asym_pt2(q,p,j,i)
term(151) = term(151) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_89_so_asym_pt2(q,p,j,i)
term(152) = term(152) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_90_so_asym_pt2(q,p,j,i)
term(153) = term(153) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_89_so_asym_pt2(q,p,j,i)
term(154) = term(154) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_90_so_asym_pt2(q,p,j,i)
term(155) = term(155) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_91_so_asym_pt2(q,p,j,i)
term(156) = term(156) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_91_so_asym_pt2(q,p,j,i)
term(157) = term(157) + wm_interm_1_so_asym_pt2(i,j) * wm_interm_92_so_asym_pt2(q,p,j,i)
term(158) = term(158) + wm_interm_2_so_asym_pt2(i,j) * wm_interm_92_so_asym_pt2(q,p,j,i)
end do 
end do 

term(137) = term(137) * (2.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (-1.0d+0) 
term(140) = term(140) * (2.0d+0) 
term(141) = term(141) * (2.0d+0) 
term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (2.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (-1.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-1.0d+0) 
term(150) = term(150) * (2.0d+0) 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (8.0d+0) 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (8.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(159) = term(159) + wm_interm_28_so_asym_pt2(q,p,i,j) * wm_interm_66_so_asym_pt2(j,i)
term(160) = term(160) + wm_interm_30_so_asym_pt2(q,p,i,j) * wm_interm_66_so_asym_pt2(j,i)
term(161) = term(161) + wm_interm_31_so_asym_pt2(q,p,i,j) * wm_interm_66_so_asym_pt2(j,i)
term(162) = term(162) + wm_interm_56_so_asym_pt2(i,j) * wm_interm_9_so_asym_pt2(p,q,i,j)
term(163) = term(163) + wm_interm_10_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(164) = term(164) + wm_interm_56_so_asym_pt2(i,j) * wm_interm_7_so_asym_pt2(p,q,i,j)
term(165) = term(165) + wm_interm_56_so_asym_pt2(i,j) * wm_interm_6_so_asym_pt2(p,q,i,j)
term(166) = term(166) + wm_interm_58_so_asym_pt2(i,j) * wm_interm_9_so_asym_pt2(p,q,i,j)
term(167) = term(167) + wm_interm_10_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(168) = term(168) + wm_interm_58_so_asym_pt2(i,j) * wm_interm_7_so_asym_pt2(p,q,i,j)
term(169) = term(169) + wm_interm_58_so_asym_pt2(i,j) * wm_interm_6_so_asym_pt2(p,q,i,j)
term(170) = term(170) + wm_interm_27_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(171) = term(171) + wm_interm_12_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(172) = term(172) + wm_interm_27_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(173) = term(173) + wm_interm_12_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(174) = term(174) + wm_interm_17_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(175) = term(175) + wm_interm_18_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(176) = term(176) + wm_interm_16_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(177) = term(177) + wm_interm_15_so_asym_pt2(p,q,i,j) * wm_interm_56_so_asym_pt2(i,j)
term(178) = term(178) + wm_interm_17_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(179) = term(179) + wm_interm_18_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(180) = term(180) + wm_interm_16_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(181) = term(181) + wm_interm_15_so_asym_pt2(p,q,i,j) * wm_interm_58_so_asym_pt2(i,j)
term(182) = term(182) + wm_interm_28_so_asym_pt2(q,p,i,j) * wm_interm_74_so_asym_pt2(j,i)
term(183) = term(183) + wm_interm_28_so_asym_pt2(q,p,i,j) * wm_interm_75_so_asym_pt2(j,i)
term(184) = term(184) + wm_interm_28_so_asym_pt2(q,p,i,j) * wm_interm_73_so_asym_pt2(j,i)
term(185) = term(185) + wm_interm_30_so_asym_pt2(q,p,i,j) * wm_interm_74_so_asym_pt2(j,i)
term(186) = term(186) + wm_interm_30_so_asym_pt2(q,p,i,j) * wm_interm_75_so_asym_pt2(j,i)
term(187) = term(187) + wm_interm_30_so_asym_pt2(q,p,i,j) * wm_interm_73_so_asym_pt2(j,i)
term(188) = term(188) + wm_interm_31_so_asym_pt2(q,p,i,j) * wm_interm_74_so_asym_pt2(j,i)
term(189) = term(189) + wm_interm_31_so_asym_pt2(q,p,i,j) * wm_interm_75_so_asym_pt2(j,i)
term(190) = term(190) + wm_interm_31_so_asym_pt2(q,p,i,j) * wm_interm_73_so_asym_pt2(j,i)
term(191) = term(191) + wm_interm_33_so_asym_pt2(i,j) * wm_interm_55_so_asym_pt2(q,p,i,j)
term(192) = term(192) + wm_interm_34_so_asym_pt2(i,j) * wm_interm_55_so_asym_pt2(q,p,i,j)
term(193) = term(193) + wm_interm_35_so_asym_pt2(i,j) * wm_interm_55_so_asym_pt2(q,p,i,j)
term(194) = term(194) + wm_interm_33_so_asym_pt2(i,j) * wm_interm_49_so_asym_pt2(q,p,i,j)
term(195) = term(195) + wm_interm_33_so_asym_pt2(i,j) * wm_interm_47_so_asym_pt2(q,p,i,j)
term(196) = term(196) + wm_interm_34_so_asym_pt2(i,j) * wm_interm_49_so_asym_pt2(q,p,i,j)
term(197) = term(197) + wm_interm_34_so_asym_pt2(i,j) * wm_interm_47_so_asym_pt2(q,p,i,j)
term(198) = term(198) + wm_interm_35_so_asym_pt2(i,j) * wm_interm_49_so_asym_pt2(q,p,i,j)
term(199) = term(199) + wm_interm_35_so_asym_pt2(i,j) * wm_interm_47_so_asym_pt2(q,p,i,j)
term(200) = term(200) + wm_interm_28_so_asym_pt2(q,p,i,j) * wm_interm_87_so_asym_pt2(j,i)
term(201) = term(201) + wm_interm_28_so_asym_pt2(q,p,i,j) * wm_interm_88_so_asym_pt2(j,i)
term(202) = term(202) + wm_interm_30_so_asym_pt2(q,p,i,j) * wm_interm_87_so_asym_pt2(j,i)
term(203) = term(203) + wm_interm_30_so_asym_pt2(q,p,i,j) * wm_interm_88_so_asym_pt2(j,i)
term(204) = term(204) + wm_interm_31_so_asym_pt2(q,p,i,j) * wm_interm_87_so_asym_pt2(j,i)
term(205) = term(205) + wm_interm_31_so_asym_pt2(q,p,i,j) * wm_interm_88_so_asym_pt2(j,i)
term(206) = term(206) + wm_interm_42_so_asym_pt2(i,j) * wm_interm_55_so_asym_pt2(q,p,i,j)
term(207) = term(207) + wm_interm_43_so_asym_pt2(i,j) * wm_interm_55_so_asym_pt2(q,p,i,j)
term(208) = term(208) + wm_interm_44_so_asym_pt2(i,j) * wm_interm_55_so_asym_pt2(q,p,i,j)
term(209) = term(209) + wm_interm_42_so_asym_pt2(i,j) * wm_interm_49_so_asym_pt2(q,p,i,j)
term(210) = term(210) + wm_interm_42_so_asym_pt2(i,j) * wm_interm_47_so_asym_pt2(q,p,i,j)
term(211) = term(211) + wm_interm_43_so_asym_pt2(i,j) * wm_interm_49_so_asym_pt2(q,p,i,j)
term(212) = term(212) + wm_interm_43_so_asym_pt2(i,j) * wm_interm_47_so_asym_pt2(q,p,i,j)
term(213) = term(213) + wm_interm_44_so_asym_pt2(i,j) * wm_interm_49_so_asym_pt2(q,p,i,j)
term(214) = term(214) + wm_interm_44_so_asym_pt2(i,j) * wm_interm_47_so_asym_pt2(q,p,i,j)
end do 
end do 

term(159) = term(159) * (2.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (-1.0d+0) 
term(163) = term(163) * (2.0d+0) 
term(164) = term(164) * (-1.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (2.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (2.0d+0) 
term(169) = term(169) * (-4.0d+0) 
term(170) = term(170) * (-1.0d+0) 
term(171) = term(171) * (-1.0d+0) 
term(172) = term(172) * (2.0d+0) 
term(173) = term(173) * (2.0d+0) 
term(174) = term(174) * (-4.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (-4.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (8.0d+0) 
term(179) = term(179) * (-8.0d+0) 
term(180) = term(180) * (8.0d+0) 
term(181) = term(181) * (-8.0d+0) 
term(182) = term(182) * (-1.0d+0) 
term(183) = term(183) * (2.0d+0) 
term(184) = term(184) * (-1.0d+0) 
term(185) = term(185) * (-1.0d+0) 
term(186) = term(186) * (2.0d+0) 
term(187) = term(187) * (-1.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (2.0d+0) 
term(191) = term(191) * (-1.0d+0) 
term(192) = term(192) * (2.0d+0) 
term(193) = term(193) * (-1.0d+0) 
term(194) = term(194) * (-1.0d+0) 
term(195) = term(195) * (2.0d+0) 
term(196) = term(196) * (2.0d+0) 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * (-1.0d+0) 
term(199) = term(199) * (2.0d+0) 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * (4.0d+0) 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (4.0d+0) 
term(204) = term(204) * (8.0d+0) 
term(205) = term(205) * (-8.0d+0) 
term(206) = term(206) * (-2.0d+0) 
term(207) = term(207) * (4.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (4.0d+0) 
term(211) = term(211) * (4.0d+0) 
term(212) = term(212) * (-8.0d+0) 
term(213) = term(213) * (-2.0d+0) 
term(214) = term(214) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(215) = term(215) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_72_so_asym_pt2(l,i,k,j)
term(216) = term(216) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_71_so_asym_pt2(i,l,k,j)
term(217) = term(217) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_86_so_asym_pt2(l,i,k,j)
term(218) = term(218) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_86_so_asym_pt2(i,l,k,j)
end do 
end do 
end do 
end do 
end do 

term(215) = term(215) * (-0.5d+0) 
term(216) = term(216) * (-0.5d+0) 
term(217) = term(217) * (2.0d+0) 
term(218) = term(218) * (-2.0d+0) 

do i = 1, nocc 
term(219) = term(219) + r1(vrdav_Rl, q,i) * wm_interm_59_so_asym_pt2(p,i)
term(220) = term(220) + r1(vrdav_Rl, q,i) * wm_interm_60_so_asym_pt2(p,i)
term(221) = term(221) + r1(vrdav_Rl, q,i) * wm_interm_61_so_asym_pt2(p,i)
term(222) = term(222) + r1(vrdav_Rl, q,i) * wm_interm_62_so_asym_pt2(p,i)
term(223) = term(223) + r1(vrdav_Rl, q,i) * wm_interm_63_so_asym_pt2(p,i)
term(224) = term(224) + s1(q,i) * wm_interm_19_so_asym_pt2(p,i)
term(225) = term(225) + s1(q,i) * wm_interm_20_so_asym_pt2(p,i)
term(226) = term(226) + s1(q,i) * wm_interm_25_so_asym_pt2(p,i)
term(227) = term(227) + s1(q,i) * wm_interm_36_so_asym_pt2(p,i)
term(228) = term(228) + s1(q,i) * wm_interm_37_so_asym_pt2(p,i)
term(229) = term(229) + r1(vrdav_Rr, p,i) * wm_interm_64_so_asym_pt2(q,i)
term(230) = term(230) + r1(vrdav_Rr, p,i) * wm_interm_65_so_asym_pt2(q,i)
term(231) = term(231) + t1(p,i) * wm_interm_45_so_asym_pt2(q,i)
term(232) = term(232) + wm_interm_13_so_asym_pt2(p,i) * wm_interm_52_so_asym_pt2(q,i)
term(233) = term(233) + wm_interm_52_so_asym_pt2(q,i) * wm_interm_5_so_asym_pt2(p,i)
end do 

term(219) = term(219) * (2.0d+0) 
term(220) = term(220) * (-1.0d+0) 
term(221) = term(221) * (-1.0d+0) 
term(222) = term(222) * (4.0d+0) 
term(223) = term(223) * (-4.0d+0) 
term(225) = term(225) * (-2.0d+0) 
term(227) = term(227) * (4.0d+0) 
term(228) = term(228) * (-4.0d+0) 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (2.0d+0) 
term(232) = term(232) * (2.0d+0) 
term(233) = term(233) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(234) = term(234) + r2(vrdav_Rl, a,k,q,j) * t2(a,p,l,i) * wm_interm_32_so_asym_pt2(j,k,l,i)
term(235) = term(235) + r2(vrdav_Rl, a,k,q,j) * t2(a,p,l,i) * wm_interm_41_so_asym_pt2(j,k,l,i)
end do 
end do 
end do 
end do 
end do 

term(234) = term(234) * (-1.0d+0) 
term(235) = term(235) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(236) = term(236) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_4_so_asym_pt2(b,a)
term(237) = term(237) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_8_so_asym_pt2(b,a)
end do 
end do 
end do 
end do 

term(236) = term(236) * (-1.0d+0) 
term(237) = term(237) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(238) = term(238) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_4_so_asym_pt2(b,a)
term(239) = term(239) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_8_so_asym_pt2(b,a)
term(240) = term(240) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_53_so_asym_pt2(a,b)
term(241) = term(241) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_54_so_asym_pt2(a,b)
term(242) = term(242) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_53_so_asym_pt2(a,b)
term(243) = term(243) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_54_so_asym_pt2(a,b)
term(244) = term(244) + r2(vrdav_Rl, a,i,q,j) * t2(b,p,j,i) * wm_interm_26_so_asym_pt2(a,b)
term(245) = term(245) + r2(vrdav_Rl, a,i,q,j) * t2(b,p,j,i) * wm_interm_23_so_asym_pt2(a,b)
term(246) = term(246) + r2(vrdav_Rl, a,i,q,j) * t2(b,p,j,i) * wm_interm_24_so_asym_pt2(a,b)
term(247) = term(247) + r2(vrdav_Rl, a,i,q,j) * t2(b,p,j,i) * wm_interm_38_so_asym_pt2(a,b)
term(248) = term(248) + r2(vrdav_Rl, a,i,q,j) * t2(b,p,j,i) * wm_interm_39_so_asym_pt2(a,b)
end do 
end do 
end do 
end do 

term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (-1.0d+0) 
term(242) = term(242) * (8.0d+0) 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * (-1.0d+0) 
term(245) = term(245) * (-1.0d+0) 
term(246) = term(246) * (2.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (4.0d+0) 

do a = nocc + 1, nactive 
term(249) = term(249) + wm_interm_4_so_asym_pt2(q,a) * wm_interm_70_so_asym_pt2(a,p)
term(250) = term(250) + wm_interm_70_so_asym_pt2(a,p) * wm_interm_8_so_asym_pt2(q,a)
term(251) = term(251) + wm_interm_4_so_asym_pt2(a,p) * wm_interm_70_so_asym_pt2(q,a)
term(252) = term(252) + wm_interm_70_so_asym_pt2(q,a) * wm_interm_8_so_asym_pt2(a,p)
term(253) = term(253) + wm_interm_4_so_asym_pt2(q,a) * wm_interm_83_so_asym_pt2(a,p)
term(254) = term(254) + wm_interm_83_so_asym_pt2(a,p) * wm_interm_8_so_asym_pt2(q,a)
term(255) = term(255) + wm_interm_4_so_asym_pt2(q,a) * wm_interm_84_so_asym_pt2(a,p)
term(256) = term(256) + wm_interm_84_so_asym_pt2(a,p) * wm_interm_8_so_asym_pt2(q,a)
term(257) = term(257) + wm_interm_4_so_asym_pt2(q,a) * wm_interm_85_so_asym_pt2(a,p)
term(258) = term(258) + wm_interm_85_so_asym_pt2(a,p) * wm_interm_8_so_asym_pt2(q,a)
term(259) = term(259) + wm_interm_23_so_asym_pt2(p,a) * wm_interm_53_so_asym_pt2(q,a)
term(260) = term(260) + wm_interm_24_so_asym_pt2(p,a) * wm_interm_53_so_asym_pt2(q,a)
term(261) = term(261) + wm_interm_23_so_asym_pt2(p,a) * wm_interm_54_so_asym_pt2(q,a)
term(262) = term(262) + wm_interm_24_so_asym_pt2(p,a) * wm_interm_54_so_asym_pt2(q,a)
term(263) = term(263) + wm_interm_26_so_asym_pt2(p,a) * wm_interm_53_so_asym_pt2(q,a)
term(264) = term(264) + wm_interm_26_so_asym_pt2(p,a) * wm_interm_54_so_asym_pt2(q,a)
term(265) = term(265) + wm_interm_4_so_asym_pt2(q,a) * wm_interm_93_so_asym_pt2(a,p)
term(266) = term(266) + wm_interm_8_so_asym_pt2(q,a) * wm_interm_93_so_asym_pt2(a,p)
term(267) = term(267) + wm_interm_4_so_asym_pt2(q,a) * wm_interm_94_so_asym_pt2(a,p)
term(268) = term(268) + wm_interm_8_so_asym_pt2(q,a) * wm_interm_94_so_asym_pt2(a,p)
term(269) = term(269) + wm_interm_38_so_asym_pt2(p,a) * wm_interm_53_so_asym_pt2(q,a)
term(270) = term(270) + wm_interm_39_so_asym_pt2(p,a) * wm_interm_53_so_asym_pt2(q,a)
term(271) = term(271) + wm_interm_38_so_asym_pt2(p,a) * wm_interm_54_so_asym_pt2(q,a)
term(272) = term(272) + wm_interm_39_so_asym_pt2(p,a) * wm_interm_54_so_asym_pt2(q,a)
term(273) = term(273) + wm_interm_4_so_asym_pt2(a,p) * wm_interm_83_so_asym_pt2(q,a)
term(274) = term(274) + wm_interm_4_so_asym_pt2(a,p) * wm_interm_84_so_asym_pt2(q,a)
term(275) = term(275) + wm_interm_4_so_asym_pt2(a,p) * wm_interm_85_so_asym_pt2(q,a)
term(276) = term(276) + wm_interm_83_so_asym_pt2(q,a) * wm_interm_8_so_asym_pt2(a,p)
term(277) = term(277) + wm_interm_84_so_asym_pt2(q,a) * wm_interm_8_so_asym_pt2(a,p)
term(278) = term(278) + wm_interm_85_so_asym_pt2(q,a) * wm_interm_8_so_asym_pt2(a,p)
term(279) = term(279) + wm_interm_26_so_asym_pt2(a,q) * wm_interm_54_so_asym_pt2(a,p)
term(280) = term(280) + wm_interm_26_so_asym_pt2(a,q) * wm_interm_53_so_asym_pt2(a,p)
term(281) = term(281) + wm_interm_23_so_asym_pt2(a,q) * wm_interm_54_so_asym_pt2(a,p)
term(282) = term(282) + wm_interm_23_so_asym_pt2(a,q) * wm_interm_53_so_asym_pt2(a,p)
term(283) = term(283) + wm_interm_24_so_asym_pt2(a,q) * wm_interm_54_so_asym_pt2(a,p)
term(284) = term(284) + wm_interm_24_so_asym_pt2(a,q) * wm_interm_53_so_asym_pt2(a,p)
term(285) = term(285) + wm_interm_4_so_asym_pt2(a,p) * wm_interm_93_so_asym_pt2(q,a)
term(286) = term(286) + wm_interm_4_so_asym_pt2(a,p) * wm_interm_94_so_asym_pt2(q,a)
term(287) = term(287) + wm_interm_8_so_asym_pt2(a,p) * wm_interm_93_so_asym_pt2(q,a)
term(288) = term(288) + wm_interm_8_so_asym_pt2(a,p) * wm_interm_94_so_asym_pt2(q,a)
term(289) = term(289) + wm_interm_38_so_asym_pt2(a,q) * wm_interm_54_so_asym_pt2(a,p)
term(290) = term(290) + wm_interm_38_so_asym_pt2(a,q) * wm_interm_53_so_asym_pt2(a,p)
term(291) = term(291) + wm_interm_39_so_asym_pt2(a,q) * wm_interm_54_so_asym_pt2(a,p)
term(292) = term(292) + wm_interm_39_so_asym_pt2(a,q) * wm_interm_53_so_asym_pt2(a,p)
end do 

term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (-4.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (-1.0d+0) 
term(254) = term(254) * (2.0d+0) 
term(255) = term(255) * (2.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * (-1.0d+0) 
term(258) = term(258) * (2.0d+0) 
term(259) = term(259) * (2.0d+0) 
term(260) = term(260) * (-4.0d+0) 
term(261) = term(261) * (-1.0d+0) 
term(262) = term(262) * (2.0d+0) 
term(263) = term(263) * (2.0d+0) 
term(264) = term(264) * (-1.0d+0) 
term(265) = term(265) * (-4.0d+0) 
term(266) = term(266) * (8.0d+0) 
term(267) = term(267) * (4.0d+0) 
term(268) = term(268) * (-8.0d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (-8.0d+0) 
term(271) = term(271) * (-4.0d+0) 
term(272) = term(272) * (4.0d+0) 
term(273) = term(273) * (-1.0d+0) 
term(274) = term(274) * (2.0d+0) 
term(275) = term(275) * (-1.0d+0) 
term(276) = term(276) * (2.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (2.0d+0) 
term(279) = term(279) * (-1.0d+0) 
term(280) = term(280) * (2.0d+0) 
term(281) = term(281) * (-1.0d+0) 
term(282) = term(282) * (2.0d+0) 
term(283) = term(283) * (2.0d+0) 
term(284) = term(284) * (-4.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (4.0d+0) 
term(287) = term(287) * (8.0d+0) 
term(288) = term(288) * (-8.0d+0) 
term(289) = term(289) * (-4.0d+0) 
term(290) = term(290) * (8.0d+0) 
term(291) = term(291) * (4.0d+0) 
term(292) = term(292) * (-8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(293) = term(293) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_53_so_asym_pt2(a,b)
term(294) = term(294) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_54_so_asym_pt2(a,b)
end do 
end do 
end do 
end do 

term(293) = term(293) * (2.0d+0) 
term(294) = term(294) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(295) = term(295) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_4_so_asym_pt2(b,a)
term(296) = term(296) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_8_so_asym_pt2(b,a)
end do 
end do 
end do 
end do 

term(295) = term(295) * (4.0d+0) 
term(296) = term(296) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(297) = term(297) + wm_interm_21_so_asym_pt2(p,i,j,k) * wm_interm_48_so_asym_pt2(q,i,k,j)
end do 
end do 
end do 

term(297) = term(297) * (2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(298) = term(298) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_53_so_asym_pt2(a,b)
term(299) = term(299) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_54_so_asym_pt2(a,b)
end do 
end do 
end do 
end do 

term(298) = term(298) * (-8.0d+0) 
term(299) = term(299) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(300) = term(300) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_71_so_asym_pt2(l,i,j,k)
term(301) = term(301) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_72_so_asym_pt2(l,i,j,k)
term(302) = term(302) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_71_so_asym_pt2(i,l,j,k)
term(303) = term(303) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_72_so_asym_pt2(i,l,j,k)
term(304) = term(304) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_86_so_asym_pt2(l,i,j,k)
term(305) = term(305) + s2(a,q,k,j) * t2(a,p,l,i) * wm_interm_86_so_asym_pt2(i,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(300) = term(300) * (-0.5d+0) 
term(303) = term(303) * (-0.5d+0) 
term(304) = term(304) * (-2.0d+0) 
term(305) = term(305) * (2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(306) = term(306) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_22_so_asym_pt2(l,k,i,j)
term(307) = term(307) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_22_so_asym_pt2(k,l,i,j)
end do 
end do 
end do 
end do 
end do 

term(306) = term(306) * (-0.5d+0) 

do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
term(308) = term(308) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_22_so_asym_pt2(k,l,j,i)
term(309) = term(309) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_22_so_asym_pt2(l,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(308) = term(308) * (-0.5d+0) 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(310) = term(310) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_22_so_asym_pt2(l,k,j,i)
term(311) = term(311) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_22_so_asym_pt2(l,k,j,i)
term(312) = term(312) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_22_so_asym_pt2(k,l,j,i)
end do 
end do 
end do 
end do 
end do 

term(310) = term(310) * (-0.5d+0) 
term(311) = term(311) * (-2.0d+0) 
term(312) = term(312) * (2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(313) = term(313) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_22_so_asym_pt2(k,l,i,j)
term(314) = term(314) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_22_so_asym_pt2(k,l,i,j)
term(315) = term(315) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_22_so_asym_pt2(l,k,i,j)
term(316) = term(316) + r2(vrdav_Rl, a,k,q,j) * t2(a,p,l,i) * wm_interm_32_so_asym_pt2(k,j,i,l)
term(317) = term(317) + r2(vrdav_Rl, a,k,q,j) * t2(a,p,l,i) * wm_interm_32_so_asym_pt2(j,k,i,l)
term(318) = term(318) + r2(vrdav_Rl, a,k,q,j) * t2(a,p,l,i) * wm_interm_41_so_asym_pt2(k,j,i,l)
term(319) = term(319) + r2(vrdav_Rl, a,k,q,j) * t2(a,p,l,i) * wm_interm_41_so_asym_pt2(j,k,i,l)
end do 
end do 
end do 
end do 
end do 

term(313) = term(313) * (-0.5d+0) 
term(314) = term(314) * (-2.0d+0) 
term(315) = term(315) * (2.0d+0) 
term(316) = term(316) * (-1.0d+0) 
term(317) = term(317) * (2.0d+0) 
term(318) = term(318) * (-2.0d+0) 
term(319) = term(319) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
term(320) = term(320) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(i,j,l,k)
term(321) = term(321) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(j,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(320) = term(320) * (-0.5d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(322) = term(322) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(j,i,k,l)
term(323) = term(323) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(322) = term(322) * (-0.5d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(324) = term(324) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(i,j,k,l)
term(325) = term(325) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(324) = term(324) * (-0.5d+0) 
term(325) = term(325) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(326) = term(326) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(j,i,l,k)
term(327) = term(327) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(j,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(326) = term(326) * (-0.5d+0) 
term(327) = term(327) * (-1.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(328) = term(328) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(i,j,l,k)
term(329) = term(329) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_57_so_asym_pt2(j,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(328) = term(328) * (-1.0d+0) 
term(329) = term(329) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(330) = term(330) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_4_so_asym_pt2(b,a)
term(331) = term(331) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_8_so_asym_pt2(b,a)
term(332) = term(332) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,j,i) * wm_interm_53_so_asym_pt2(a,b)
term(333) = term(333) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,j,i) * wm_interm_54_so_asym_pt2(a,b)
term(334) = term(334) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_4_so_asym_pt2(b,a)
term(335) = term(335) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_8_so_asym_pt2(b,a)
term(336) = term(336) + s2(a,q,i,j) * t2(b,p,j,i) * wm_interm_83_so_asym_pt2(b,a)
term(337) = term(337) + s2(a,q,i,j) * t2(b,p,j,i) * wm_interm_84_so_asym_pt2(b,a)
term(338) = term(338) + s2(a,q,i,j) * t2(b,p,j,i) * wm_interm_85_so_asym_pt2(b,a)
term(339) = term(339) + s2(a,q,i,j) * t2(b,p,j,i) * wm_interm_93_so_asym_pt2(b,a)
term(340) = term(340) + s2(a,q,i,j) * t2(b,p,j,i) * wm_interm_94_so_asym_pt2(b,a)
end do 
end do 
end do 
end do 

term(330) = term(330) * (-1.0d+0) 
term(331) = term(331) * (2.0d+0) 
term(332) = term(332) * (-4.0d+0) 
term(333) = term(333) * (2.0d+0) 
term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * (8.0d+0) 
term(336) = term(336) * (-1.0d+0) 
term(337) = term(337) * (2.0d+0) 
term(338) = term(338) * (-1.0d+0) 
term(339) = term(339) * (-4.0d+0) 
term(340) = term(340) * (4.0d+0) 


    calc_D_vv_wm_so_asym_pt2 = zero
    do s = 0, 340
    calc_D_vv_wm_so_asym_pt2 = calc_D_vv_wm_so_asym_pt2 + term(s)
    end do

    end function calc_D_vv_wm_so_asym_pt2


        function calc_D_oo_wm_so_asym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_asym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:59) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + wm_interm_17_so_asym_pt3(q,i) * wm_interm_78_so_asym_pt3(p,i)
term(1) = term(1) + wm_interm_18_so_asym_pt3(q,i) * wm_interm_78_so_asym_pt3(p,i)
term(2) = term(2) + wm_interm_19_so_asym_pt3(q,i) * wm_interm_78_so_asym_pt3(p,i)
term(3) = term(3) + wm_interm_41_so_asym_pt3(q,i) * wm_interm_78_so_asym_pt3(p,i)
term(4) = term(4) + wm_interm_42_so_asym_pt3(q,i) * wm_interm_78_so_asym_pt3(p,i)
term(5) = term(5) + wm_interm_43_so_asym_pt3(q,i) * wm_interm_78_so_asym_pt3(p,i)
term(6) = term(6) + wm_interm_17_so_asym_pt3(i,p) * wm_interm_78_so_asym_pt3(i,q)
term(7) = term(7) + wm_interm_18_so_asym_pt3(i,p) * wm_interm_78_so_asym_pt3(i,q)
term(8) = term(8) + wm_interm_19_so_asym_pt3(i,p) * wm_interm_78_so_asym_pt3(i,q)
term(9) = term(9) + wm_interm_41_so_asym_pt3(i,p) * wm_interm_78_so_asym_pt3(i,q)
term(10) = term(10) + wm_interm_42_so_asym_pt3(i,p) * wm_interm_78_so_asym_pt3(i,q)
term(11) = term(11) + wm_interm_43_so_asym_pt3(i,p) * wm_interm_78_so_asym_pt3(i,q)
term(12) = term(12) + wm_interm_55_so_asym_pt3(p,i) * wm_interm_90_so_asym_pt3(q,i)
term(13) = term(13) + wm_interm_57_so_asym_pt3(p,i) * wm_interm_90_so_asym_pt3(q,i)
term(14) = term(14) + wm_interm_55_so_asym_pt3(i,q) * wm_interm_90_so_asym_pt3(i,p)
term(15) = term(15) + wm_interm_57_so_asym_pt3(i,q) * wm_interm_90_so_asym_pt3(i,p)
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(16) = term(16) + wm_interm_20_so_asym_pt3(q,i,j,p) * wm_interm_78_so_asym_pt3(i,j)
term(17) = term(17) + wm_interm_20_so_asym_pt3(i,q,p,j) * wm_interm_78_so_asym_pt3(i,j)
term(18) = term(18) + wm_interm_20_so_asym_pt3(q,i,p,j) * wm_interm_78_so_asym_pt3(i,j)
term(19) = term(19) + wm_interm_44_so_asym_pt3(q,i,j,p) * wm_interm_78_so_asym_pt3(i,j)
term(20) = term(20) + wm_interm_44_so_asym_pt3(i,q,p,j) * wm_interm_78_so_asym_pt3(i,j)
term(21) = term(21) + wm_interm_44_so_asym_pt3(q,i,p,j) * wm_interm_78_so_asym_pt3(i,j)
term(22) = term(22) + wm_interm_70_so_asym_pt3(i,p,q,j) * wm_interm_90_so_asym_pt3(i,j)
term(23) = term(23) + wm_interm_70_so_asym_pt3(p,i,j,q) * wm_interm_90_so_asym_pt3(i,j)
end do 
end do 

term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (-1.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(24) = term(24) + wm_interm_21_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(25) = term(25) + wm_interm_22_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(26) = term(26) + wm_interm_26_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(27) = term(27) + wm_interm_23_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(28) = term(28) + wm_interm_25_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(29) = term(29) + wm_interm_29_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(30) = term(30) + wm_interm_45_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(31) = term(31) + wm_interm_46_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(32) = term(32) + wm_interm_47_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(33) = term(33) + wm_interm_48_so_asym_pt3(a,b,q,p) * wm_interm_75_so_asym_pt3(a,b)
term(34) = term(34) + wm_interm_83_so_asym_pt3(a,b,p,q) * wm_interm_9_so_asym_pt3(a,b)
term(35) = term(35) + wm_interm_5_so_asym_pt3(a,b) * wm_interm_83_so_asym_pt3(a,b,p,q)
term(36) = term(36) + wm_interm_6_so_asym_pt3(a,b) * wm_interm_83_so_asym_pt3(a,b,p,q)
term(37) = term(37) + wm_interm_34_so_asym_pt3(a,b) * wm_interm_83_so_asym_pt3(a,b,p,q)
term(38) = term(38) + wm_interm_35_so_asym_pt3(a,b) * wm_interm_83_so_asym_pt3(a,b,p,q)
term(39) = term(39) + wm_interm_53_so_asym_pt3(a,b) * wm_interm_87_so_asym_pt3(a,b,q,p)
term(40) = term(40) + wm_interm_54_so_asym_pt3(a,b) * wm_interm_87_so_asym_pt3(a,b,q,p)
term(41) = term(41) + wm_interm_72_so_asym_pt3(a,b,p,q) * wm_interm_89_so_asym_pt3(a,b)
term(42) = term(42) + wm_interm_73_so_asym_pt3(a,b,p,q) * wm_interm_89_so_asym_pt3(a,b)
term(43) = term(43) + wm_interm_74_so_asym_pt3(a,b,p,q) * wm_interm_89_so_asym_pt3(a,b)
end do 
end do 

term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + wm_interm_1_so_asym_pt3(a,p,i,q) * wm_interm_80_so_asym_pt3(a,i)
term(45) = term(45) + wm_interm_1_so_asym_pt3(a,p,i,q) * wm_interm_81_so_asym_pt3(a,i)
term(46) = term(46) + wm_interm_1_so_asym_pt3(a,p,i,q) * wm_interm_82_so_asym_pt3(a,i)
term(47) = term(47) + wm_interm_1_so_asym_pt3(a,p,i,q) * wm_interm_84_so_asym_pt3(a,i)
term(48) = term(48) + wm_interm_1_so_asym_pt3(a,p,i,q) * wm_interm_85_so_asym_pt3(a,i)
term(49) = term(49) + wm_interm_51_so_asym_pt3(a,i) * wm_interm_86_so_asym_pt3(a,p,i,q)
term(50) = term(50) + wm_interm_51_so_asym_pt3(a,i) * wm_interm_86_so_asym_pt3(a,i,p,q)
term(51) = term(51) + wm_interm_51_so_asym_pt3(a,i) * wm_interm_88_so_asym_pt3(a,p,i,q)
term(52) = term(52) + wm_interm_51_so_asym_pt3(a,i) * wm_interm_88_so_asym_pt3(a,i,p,q)
term(53) = term(53) + wm_interm_52_so_asym_pt3(a,i) * wm_interm_88_so_asym_pt3(a,p,i,q)
term(54) = term(54) + wm_interm_52_so_asym_pt3(a,i) * wm_interm_86_so_asym_pt3(a,p,i,q)
term(55) = term(55) + wm_interm_52_so_asym_pt3(a,i) * wm_interm_86_so_asym_pt3(a,i,p,q)
term(56) = term(56) + wm_interm_52_so_asym_pt3(a,i) * wm_interm_88_so_asym_pt3(a,i,p,q)
term(57) = term(57) + wm_interm_69_so_asym_pt3(a,p,q,i) * wm_interm_91_so_asym_pt3(a,i)
end do 
end do 

term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-1.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-1.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(58) = term(58) + wm_interm_51_so_asym_pt3(a,q) * wm_interm_91_so_asym_pt3(a,p)
term(59) = term(59) + wm_interm_52_so_asym_pt3(a,q) * wm_interm_91_so_asym_pt3(a,p)
end do 

term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-2.0d+0) 


    calc_D_oo_wm_so_asym_pt3 = zero
    do s = 0, 59
    calc_D_oo_wm_so_asym_pt3 = calc_D_oo_wm_so_asym_pt3 + term(s)
    end do

    end function calc_D_oo_wm_so_asym_pt3
    
    function calc_D_ov_wm_so_asym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_asym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b 
    real(F64), dimension(0:129) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_so_asym_pt3(q,a,i,j) * wm_interm_1_so_asym_pt3(a,j,p,i)
term(1) = term(1) + wm_interm_1_so_asym_pt3(a,i,p,j) * wm_interm_2_so_asym_pt3(q,a,j,i)
term(2) = term(2) + wm_interm_1_so_asym_pt3(a,i,p,j) * wm_interm_3_so_asym_pt3(q,a,j,i)
term(3) = term(3) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_2_so_asym_pt3(q,a,j,i)
term(4) = term(4) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_4_so_asym_pt3(q,a,j,i)
term(5) = term(5) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_3_so_asym_pt3(q,a,j,i)
term(6) = term(6) + wm_interm_10_so_asym_pt3(q,a,i,j) * wm_interm_1_so_asym_pt3(a,p,j,i)
term(7) = term(7) + wm_interm_12_so_asym_pt3(q,a,i,j) * wm_interm_1_so_asym_pt3(a,p,j,i)
term(8) = term(8) + wm_interm_14_so_asym_pt3(q,a,i,j) * wm_interm_1_so_asym_pt3(a,p,j,i)
term(9) = term(9) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_21_so_asym_pt3(a,q,i,j)
term(10) = term(10) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_22_so_asym_pt3(a,q,i,j)
term(11) = term(11) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_23_so_asym_pt3(a,q,i,j)
term(12) = term(12) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_24_so_asym_pt3(a,q,i,j)
term(13) = term(13) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_25_so_asym_pt3(a,q,i,j)
term(14) = term(14) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_26_so_asym_pt3(a,q,i,j)
term(15) = term(15) + wm_interm_16_so_asym_pt3(a,i,p,j) * wm_interm_24_so_asym_pt3(a,q,i,j)
term(16) = term(16) + wm_interm_16_so_asym_pt3(a,i,p,j) * wm_interm_29_so_asym_pt3(a,q,i,j)
term(17) = term(17) + wm_interm_16_so_asym_pt3(a,i,p,j) * wm_interm_25_so_asym_pt3(a,q,i,j)
term(18) = term(18) + wm_interm_21_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,i,p,j)
term(19) = term(19) + wm_interm_22_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,i,p,j)
term(20) = term(20) + wm_interm_26_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,i,p,j)
term(21) = term(21) + wm_interm_29_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,p,i,j)
term(22) = term(22) + wm_interm_25_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,p,i,j)
term(23) = term(23) + wm_interm_24_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_25_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,i,p,j)
term(25) = term(25) + wm_interm_23_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,i,p,j)
term(26) = term(26) + wm_interm_24_so_asym_pt3(a,q,i,j) * wm_interm_31_so_asym_pt3(a,i,p,j)
term(27) = term(27) + wm_interm_1_so_asym_pt3(a,i,p,j) * wm_interm_32_so_asym_pt3(q,a,j,i)
term(28) = term(28) + wm_interm_1_so_asym_pt3(a,i,p,j) * wm_interm_33_so_asym_pt3(q,a,j,i)
term(29) = term(29) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_33_so_asym_pt3(q,a,j,i)
term(30) = term(30) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_32_so_asym_pt3(q,a,j,i)
term(31) = term(31) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_37_so_asym_pt3(q,a,j,i)
term(32) = term(32) + wm_interm_1_so_asym_pt3(a,p,i,j) * wm_interm_39_so_asym_pt3(q,a,j,i)
term(33) = term(33) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_45_so_asym_pt3(a,q,i,j)
term(34) = term(34) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_46_so_asym_pt3(a,q,i,j)
term(35) = term(35) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_47_so_asym_pt3(a,q,i,j)
term(36) = term(36) + wm_interm_16_so_asym_pt3(a,p,i,j) * wm_interm_48_so_asym_pt3(a,q,i,j)
term(37) = term(37) + wm_interm_16_so_asym_pt3(a,i,p,j) * wm_interm_48_so_asym_pt3(a,q,i,j)
term(38) = term(38) + wm_interm_16_so_asym_pt3(a,i,p,j) * wm_interm_47_so_asym_pt3(a,q,i,j)
term(39) = term(39) + wm_interm_31_so_asym_pt3(a,i,p,j) * wm_interm_45_so_asym_pt3(a,q,i,j)
term(40) = term(40) + wm_interm_31_so_asym_pt3(a,i,p,j) * wm_interm_46_so_asym_pt3(a,q,i,j)
term(41) = term(41) + wm_interm_31_so_asym_pt3(a,p,i,j) * wm_interm_48_so_asym_pt3(a,q,i,j)
term(42) = term(42) + wm_interm_31_so_asym_pt3(a,p,i,j) * wm_interm_47_so_asym_pt3(a,q,i,j)
term(43) = term(43) + wm_interm_31_so_asym_pt3(a,i,p,j) * wm_interm_47_so_asym_pt3(a,q,i,j)
term(44) = term(44) + wm_interm_31_so_asym_pt3(a,i,p,j) * wm_interm_48_so_asym_pt3(a,q,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (-0.5d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (4.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(45) = term(45) + s2(a,q,p,i) * t1(b,i) * wm_interm_27_so_asym_pt3(b,a)
term(46) = term(46) + s2(a,q,p,i) * t1(b,i) * wm_interm_28_so_asym_pt3(b,a)
term(47) = term(47) + s2(a,q,p,i) * t1(b,i) * wm_interm_30_so_asym_pt3(b,a)
term(48) = term(48) + s2(a,q,p,i) * t1(b,i) * wm_interm_49_so_asym_pt3(b,a)
term(49) = term(49) + s2(a,q,p,i) * t1(b,i) * wm_interm_50_so_asym_pt3(b,a)
end do 
end do 
end do 

term(45) = term(45) * (-1.0d+0) 
term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (4.0d+0) 

do a = nocc + 1, nactive 
term(50) = term(50) + wm_interm_61_so_asym_pt3(a,p) * wm_interm_9_so_asym_pt3(a,q)
term(51) = term(51) + wm_interm_5_so_asym_pt3(a,q) * wm_interm_61_so_asym_pt3(a,p)
term(52) = term(52) + wm_interm_61_so_asym_pt3(a,p) * wm_interm_6_so_asym_pt3(a,q)
term(53) = term(53) + wm_interm_62_so_asym_pt3(a,p) * wm_interm_9_so_asym_pt3(a,q)
term(54) = term(54) + wm_interm_5_so_asym_pt3(a,q) * wm_interm_62_so_asym_pt3(a,p)
term(55) = term(55) + wm_interm_62_so_asym_pt3(a,p) * wm_interm_6_so_asym_pt3(a,q)
term(56) = term(56) + wm_interm_34_so_asym_pt3(a,q) * wm_interm_61_so_asym_pt3(a,p)
term(57) = term(57) + wm_interm_35_so_asym_pt3(a,q) * wm_interm_61_so_asym_pt3(a,p)
term(58) = term(58) + wm_interm_34_so_asym_pt3(a,q) * wm_interm_62_so_asym_pt3(a,p)
term(59) = term(59) + wm_interm_35_so_asym_pt3(a,q) * wm_interm_62_so_asym_pt3(a,p)
term(60) = term(60) + wm_interm_75_so_asym_pt3(q,a) * wm_interm_92_so_asym_pt3(a,p)
end do 

term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-1.0d+0) 
term(54) = term(54) * (-1.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (8.0d+0) 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(61) = term(61) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_7_so_asym_pt3(p,k,j,i)
term(62) = term(62) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_8_so_asym_pt3(k,p,j,i)
term(63) = term(63) + wm_interm_20_so_asym_pt3(i,j,k,p) * wm_interm_31_so_asym_pt3(q,j,i,k)
term(64) = term(64) + wm_interm_20_so_asym_pt3(i,j,p,k) * wm_interm_31_so_asym_pt3(q,j,i,k)
term(65) = term(65) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_36_so_asym_pt3(p,k,j,i)
term(66) = term(66) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_36_so_asym_pt3(k,p,j,i)
end do 
end do 
end do 

term(61) = term(61) * (-0.5d+0) 
term(62) = term(62) * (-0.5d+0) 
term(63) = term(63) * (-0.5d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(67) = term(67) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_7_so_asym_pt3(p,k,i,j)
term(68) = term(68) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_8_so_asym_pt3(p,k,i,j)
term(69) = term(69) + wm_interm_16_so_asym_pt3(q,i,j,k) * wm_interm_20_so_asym_pt3(i,j,k,p)
term(70) = term(70) + wm_interm_16_so_asym_pt3(q,i,j,k) * wm_interm_20_so_asym_pt3(j,i,p,k)
term(71) = term(71) + wm_interm_16_so_asym_pt3(q,i,j,k) * wm_interm_20_so_asym_pt3(i,j,p,k)
term(72) = term(72) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_7_so_asym_pt3(k,p,i,j)
term(73) = term(73) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_8_so_asym_pt3(k,p,i,j)
term(74) = term(74) + wm_interm_20_so_asym_pt3(i,j,p,k) * wm_interm_31_so_asym_pt3(q,i,j,k)
term(75) = term(75) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_36_so_asym_pt3(p,k,i,j)
term(76) = term(76) + wm_interm_16_so_asym_pt3(q,i,j,k) * wm_interm_44_so_asym_pt3(i,j,k,p)
term(77) = term(77) + wm_interm_16_so_asym_pt3(q,i,j,k) * wm_interm_44_so_asym_pt3(j,i,p,k)
term(78) = term(78) + wm_interm_16_so_asym_pt3(q,i,j,k) * wm_interm_44_so_asym_pt3(i,j,p,k)
term(79) = term(79) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_36_so_asym_pt3(k,p,i,j)
term(80) = term(80) + wm_interm_31_so_asym_pt3(q,i,j,k) * wm_interm_44_so_asym_pt3(j,i,k,p)
term(81) = term(81) + wm_interm_31_so_asym_pt3(q,i,j,k) * wm_interm_44_so_asym_pt3(i,j,p,k)
term(82) = term(82) + wm_interm_31_so_asym_pt3(q,i,j,k) * wm_interm_44_so_asym_pt3(j,i,p,k)
end do 
end do 
end do 

term(68) = term(68) * (-0.5d+0) 
term(69) = term(69) * (-0.5d+0) 
term(70) = term(70) * (-0.5d+0) 
term(72) = term(72) * (-0.5d+0) 
term(74) = term(74) * (-0.5d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-1.0d+0) 
term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(80) = term(80) * (-1.0d+0) 
term(81) = term(81) * (-1.0d+0) 
term(82) = term(82) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(83) = term(83) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_5_so_asym_pt3(a,b)
term(84) = term(84) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_6_so_asym_pt3(a,b)
term(85) = term(85) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_9_so_asym_pt3(a,b)
term(86) = term(86) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_34_so_asym_pt3(a,b)
term(87) = term(87) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_35_so_asym_pt3(a,b)
end do 
end do 
end do 

term(83) = term(83) * (-1.0d+0) 
term(84) = term(84) * (2.0d+0) 
term(85) = term(85) * (-1.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (4.0d+0) 

do i = 1, nocc 
term(88) = term(88) + wm_interm_17_so_asym_pt3(i,p) * wm_interm_61_so_asym_pt3(q,i)
term(89) = term(89) + wm_interm_18_so_asym_pt3(i,p) * wm_interm_61_so_asym_pt3(q,i)
term(90) = term(90) + wm_interm_19_so_asym_pt3(i,p) * wm_interm_61_so_asym_pt3(q,i)
term(91) = term(91) + wm_interm_17_so_asym_pt3(i,p) * wm_interm_62_so_asym_pt3(q,i)
term(92) = term(92) + wm_interm_18_so_asym_pt3(i,p) * wm_interm_62_so_asym_pt3(q,i)
term(93) = term(93) + wm_interm_19_so_asym_pt3(i,p) * wm_interm_62_so_asym_pt3(q,i)
term(94) = term(94) + wm_interm_41_so_asym_pt3(i,p) * wm_interm_61_so_asym_pt3(q,i)
term(95) = term(95) + wm_interm_42_so_asym_pt3(i,p) * wm_interm_61_so_asym_pt3(q,i)
term(96) = term(96) + wm_interm_43_so_asym_pt3(i,p) * wm_interm_61_so_asym_pt3(q,i)
term(97) = term(97) + wm_interm_41_so_asym_pt3(i,p) * wm_interm_62_so_asym_pt3(q,i)
term(98) = term(98) + wm_interm_42_so_asym_pt3(i,p) * wm_interm_62_so_asym_pt3(q,i)
term(99) = term(99) + wm_interm_43_so_asym_pt3(i,p) * wm_interm_62_so_asym_pt3(q,i)
term(100) = term(100) + wm_interm_78_so_asym_pt3(p,i) * wm_interm_92_so_asym_pt3(q,i)
end do 

term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (-4.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-1.0d+0) 
term(92) = term(92) * (2.0d+0) 
term(93) = term(93) * (-1.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (4.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(101) = term(101) + wm_interm_11_so_asym_pt3(i,j) * wm_interm_1_so_asym_pt3(q,j,p,i)
term(102) = term(102) + wm_interm_13_so_asym_pt3(i,j) * wm_interm_1_so_asym_pt3(q,j,p,i)
term(103) = term(103) + wm_interm_15_so_asym_pt3(i,j) * wm_interm_1_so_asym_pt3(q,j,p,i)
end do 
end do 

term(101) = term(101) * (-1.0d+0) 
term(102) = term(102) * (-1.0d+0) 
term(103) = term(103) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(104) = term(104) + wm_interm_29_so_asym_pt3(a,q,i,p) * wm_interm_61_so_asym_pt3(a,i)
term(105) = term(105) + wm_interm_25_so_asym_pt3(a,q,i,p) * wm_interm_61_so_asym_pt3(a,i)
term(106) = term(106) + wm_interm_24_so_asym_pt3(a,q,i,p) * wm_interm_61_so_asym_pt3(a,i)
term(107) = term(107) + wm_interm_29_so_asym_pt3(a,q,i,p) * wm_interm_62_so_asym_pt3(a,i)
term(108) = term(108) + wm_interm_25_so_asym_pt3(a,q,i,p) * wm_interm_62_so_asym_pt3(a,i)
term(109) = term(109) + wm_interm_24_so_asym_pt3(a,q,i,p) * wm_interm_62_so_asym_pt3(a,i)
term(110) = term(110) + wm_interm_48_so_asym_pt3(a,q,i,p) * wm_interm_61_so_asym_pt3(a,i)
term(111) = term(111) + wm_interm_47_so_asym_pt3(a,q,i,p) * wm_interm_61_so_asym_pt3(a,i)
term(112) = term(112) + wm_interm_48_so_asym_pt3(a,q,i,p) * wm_interm_62_so_asym_pt3(a,i)
term(113) = term(113) + wm_interm_47_so_asym_pt3(a,q,i,p) * wm_interm_62_so_asym_pt3(a,i)
end do 
end do 

term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (2.0d+0) 
term(107) = term(107) * (-1.0d+0) 
term(108) = term(108) * (2.0d+0) 
term(109) = term(109) * (-1.0d+0) 
term(110) = term(110) * (8.0d+0) 
term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(114) = term(114) + wm_interm_16_so_asym_pt3(q,i,p,j) * wm_interm_17_so_asym_pt3(i,j)
term(115) = term(115) + wm_interm_16_so_asym_pt3(q,i,p,j) * wm_interm_18_so_asym_pt3(i,j)
term(116) = term(116) + wm_interm_16_so_asym_pt3(q,i,p,j) * wm_interm_19_so_asym_pt3(i,j)
term(117) = term(117) + wm_interm_17_so_asym_pt3(i,j) * wm_interm_31_so_asym_pt3(q,p,i,j)
term(118) = term(118) + wm_interm_18_so_asym_pt3(i,j) * wm_interm_31_so_asym_pt3(q,p,i,j)
term(119) = term(119) + wm_interm_19_so_asym_pt3(i,j) * wm_interm_31_so_asym_pt3(q,p,i,j)
term(120) = term(120) + wm_interm_1_so_asym_pt3(q,i,p,j) * wm_interm_38_so_asym_pt3(j,i)
term(121) = term(121) + wm_interm_1_so_asym_pt3(q,i,p,j) * wm_interm_40_so_asym_pt3(j,i)
term(122) = term(122) + wm_interm_16_so_asym_pt3(q,i,p,j) * wm_interm_41_so_asym_pt3(i,j)
term(123) = term(123) + wm_interm_16_so_asym_pt3(q,i,p,j) * wm_interm_42_so_asym_pt3(i,j)
term(124) = term(124) + wm_interm_16_so_asym_pt3(q,i,p,j) * wm_interm_43_so_asym_pt3(i,j)
term(125) = term(125) + wm_interm_31_so_asym_pt3(q,p,i,j) * wm_interm_41_so_asym_pt3(i,j)
term(126) = term(126) + wm_interm_31_so_asym_pt3(q,p,i,j) * wm_interm_42_so_asym_pt3(i,j)
term(127) = term(127) + wm_interm_31_so_asym_pt3(q,p,i,j) * wm_interm_43_so_asym_pt3(i,j)
term(128) = term(128) + wm_interm_1_so_asym_pt3(q,i,p,j) * wm_interm_93_so_asym_pt3(j,i)
term(129) = term(129) + wm_interm_78_so_asym_pt3(i,j) * wm_interm_94_so_asym_pt3(q,i,p,j)
end do 
end do 

term(114) = term(114) * (-0.5d+0) 
term(116) = term(116) * (-0.5d+0) 
term(117) = term(117) * (-0.5d+0) 
term(119) = term(119) * (-0.5d+0) 
term(120) = term(120) * (-4.0d+0) 
term(121) = term(121) * (4.0d+0) 
term(122) = term(122) * (-1.0d+0) 
term(123) = term(123) * (2.0d+0) 
term(124) = term(124) * (-1.0d+0) 
term(125) = term(125) * (-1.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-1.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (2.0d+0) 


    calc_D_ov_wm_so_asym_pt3 = zero
    do s = 0, 129
    calc_D_ov_wm_so_asym_pt3 = calc_D_ov_wm_so_asym_pt3 + term(s)
    end do

    end function calc_D_ov_wm_so_asym_pt3
    
    function calc_D_vo_wm_so_asym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_asym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b 
    real(F64), dimension(0:168) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_12_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(1) = term(1) + wm_interm_14_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(2) = term(2) + wm_interm_4_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(3) = term(3) + wm_interm_3_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(4) = term(4) + wm_interm_2_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(5) = term(5) + wm_interm_10_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(6) = term(6) + wm_interm_3_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(7) = term(7) + wm_interm_4_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(8) = term(8) + wm_interm_12_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(9) = term(9) + wm_interm_14_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(10) = term(10) + wm_interm_0_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(11) = term(11) + wm_interm_10_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(12) = term(12) + wm_interm_58_so_asym_pt3(a,i,q,j) * wm_interm_72_so_asym_pt3(a,p,i,j)
term(13) = term(13) + wm_interm_58_so_asym_pt3(a,q,i,j) * wm_interm_72_so_asym_pt3(a,p,i,j)
term(14) = term(14) + wm_interm_58_so_asym_pt3(a,q,i,j) * wm_interm_66_so_asym_pt3(a,p,i,j)
term(15) = term(15) + wm_interm_56_so_asym_pt3(a,i,q,j) * wm_interm_66_so_asym_pt3(a,p,i,j)
term(16) = term(16) + wm_interm_56_so_asym_pt3(a,q,i,j) * wm_interm_66_so_asym_pt3(a,p,i,j)
term(17) = term(17) + wm_interm_56_so_asym_pt3(a,q,i,j) * wm_interm_72_so_asym_pt3(a,p,i,j)
term(18) = term(18) + wm_interm_58_so_asym_pt3(a,i,q,j) * wm_interm_73_so_asym_pt3(a,p,i,j)
term(19) = term(19) + wm_interm_58_so_asym_pt3(a,q,i,j) * wm_interm_73_so_asym_pt3(a,p,i,j)
term(20) = term(20) + wm_interm_58_so_asym_pt3(a,i,q,j) * wm_interm_74_so_asym_pt3(a,p,i,j)
term(21) = term(21) + wm_interm_58_so_asym_pt3(a,q,i,j) * wm_interm_74_so_asym_pt3(a,p,i,j)
term(22) = term(22) + wm_interm_56_so_asym_pt3(a,q,i,j) * wm_interm_73_so_asym_pt3(a,p,i,j)
term(23) = term(23) + wm_interm_56_so_asym_pt3(a,q,i,j) * wm_interm_74_so_asym_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_37_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(25) = term(25) + wm_interm_39_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(26) = term(26) + wm_interm_33_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(27) = term(27) + wm_interm_32_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,q,i)
term(28) = term(28) + wm_interm_32_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(29) = term(29) + wm_interm_33_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(30) = term(30) + wm_interm_37_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(31) = term(31) + wm_interm_39_so_asym_pt3(a,p,i,j) * wm_interm_69_so_asym_pt3(a,j,i,q)
term(32) = term(32) + wm_interm_59_so_asym_pt3(a,q,i,j) * wm_interm_72_so_asym_pt3(a,p,i,j)
term(33) = term(33) + wm_interm_59_so_asym_pt3(a,i,q,j) * wm_interm_72_so_asym_pt3(a,p,i,j)
term(34) = term(34) + wm_interm_59_so_asym_pt3(a,i,q,j) * wm_interm_66_so_asym_pt3(a,p,i,j)
term(35) = term(35) + wm_interm_59_so_asym_pt3(a,q,i,j) * wm_interm_66_so_asym_pt3(a,p,i,j)
term(36) = term(36) + wm_interm_59_so_asym_pt3(a,q,i,j) * wm_interm_73_so_asym_pt3(a,p,i,j)
term(37) = term(37) + wm_interm_59_so_asym_pt3(a,i,q,j) * wm_interm_73_so_asym_pt3(a,p,i,j)
term(38) = term(38) + wm_interm_59_so_asym_pt3(a,q,i,j) * wm_interm_74_so_asym_pt3(a,p,i,j)
term(39) = term(39) + wm_interm_59_so_asym_pt3(a,i,q,j) * wm_interm_74_so_asym_pt3(a,p,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (8.0d+0) 
term(31) = term(31) * (-8.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (8.0d+0) 
term(39) = term(39) * (-8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(40) = term(40) + wm_interm_36_so_asym_pt3(i,j,k,q) * wm_interm_71_so_asym_pt3(p,k,i,j)
end do 
end do 
end do 

term(40) = term(40) * (2.0d+0) 

do a = nocc + 1, nactive 
term(41) = term(41) + wm_interm_54_so_asym_pt3(a,p) * wm_interm_63_so_asym_pt3(a,q)
term(42) = term(42) + wm_interm_53_so_asym_pt3(a,p) * wm_interm_63_so_asym_pt3(a,q)
term(43) = term(43) + wm_interm_54_so_asym_pt3(a,p) * wm_interm_64_so_asym_pt3(a,q)
term(44) = term(44) + wm_interm_53_so_asym_pt3(a,p) * wm_interm_64_so_asym_pt3(a,q)
term(45) = term(45) + wm_interm_54_so_asym_pt3(a,p) * wm_interm_65_so_asym_pt3(a,q)
term(46) = term(46) + wm_interm_53_so_asym_pt3(a,p) * wm_interm_65_so_asym_pt3(a,q)
term(47) = term(47) + wm_interm_54_so_asym_pt3(a,p) * wm_interm_67_so_asym_pt3(a,q)
term(48) = term(48) + wm_interm_53_so_asym_pt3(a,p) * wm_interm_67_so_asym_pt3(a,q)
term(49) = term(49) + wm_interm_54_so_asym_pt3(a,p) * wm_interm_68_so_asym_pt3(a,q)
term(50) = term(50) + wm_interm_53_so_asym_pt3(a,p) * wm_interm_68_so_asym_pt3(a,q)
term(51) = term(51) + wm_interm_28_so_asym_pt3(a,p) * wm_interm_51_so_asym_pt3(a,q)
term(52) = term(52) + wm_interm_30_so_asym_pt3(a,p) * wm_interm_51_so_asym_pt3(a,q)
term(53) = term(53) + wm_interm_27_so_asym_pt3(a,p) * wm_interm_51_so_asym_pt3(a,q)
term(54) = term(54) + wm_interm_28_so_asym_pt3(a,p) * wm_interm_52_so_asym_pt3(a,q)
term(55) = term(55) + wm_interm_30_so_asym_pt3(a,p) * wm_interm_52_so_asym_pt3(a,q)
term(56) = term(56) + wm_interm_27_so_asym_pt3(a,p) * wm_interm_52_so_asym_pt3(a,q)
term(57) = term(57) + wm_interm_49_so_asym_pt3(a,p) * wm_interm_51_so_asym_pt3(a,q)
term(58) = term(58) + wm_interm_50_so_asym_pt3(a,p) * wm_interm_51_so_asym_pt3(a,q)
term(59) = term(59) + wm_interm_49_so_asym_pt3(a,p) * wm_interm_52_so_asym_pt3(a,q)
term(60) = term(60) + wm_interm_50_so_asym_pt3(a,p) * wm_interm_52_so_asym_pt3(a,q)
term(61) = term(61) + wm_interm_89_so_asym_pt3(p,a) * wm_interm_95_so_asym_pt3(a,q)
term(62) = term(62) + wm_interm_51_so_asym_pt3(a,q) * wm_interm_96_so_asym_pt3(a,p)
term(63) = term(63) + wm_interm_52_so_asym_pt3(a,q) * wm_interm_96_so_asym_pt3(a,p)
term(64) = term(64) + wm_interm_89_so_asym_pt3(p,a) * wm_interm_97_so_asym_pt3(a,q)
end do 

term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (8.0d+0) 
term(49) = term(49) * (4.0d+0) 
term(50) = term(50) * (-8.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (-1.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (-1.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(65) = term(65) + wm_interm_56_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(j,i,q,k)
term(66) = term(66) + wm_interm_56_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(i,j,q,k)
term(67) = term(67) + wm_interm_56_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(i,j,k,q)
term(68) = term(68) + wm_interm_56_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(j,i,k,q)
term(69) = term(69) + wm_interm_58_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(j,i,k,q)
term(70) = term(70) + wm_interm_58_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(i,j,q,k)
term(71) = term(71) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_8_so_asym_pt3(j,k,i,q)
term(72) = term(72) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_7_so_asym_pt3(k,j,i,q)
term(73) = term(73) + wm_interm_59_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(j,i,q,k)
term(74) = term(74) + wm_interm_59_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(i,j,q,k)
term(75) = term(75) + wm_interm_59_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(i,j,k,q)
term(76) = term(76) + wm_interm_59_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(j,i,k,q)
term(77) = term(77) + wm_interm_60_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(j,i,k,q)
term(78) = term(78) + wm_interm_60_so_asym_pt3(p,i,j,k) * wm_interm_70_so_asym_pt3(i,j,q,k)
term(79) = term(79) + wm_interm_36_so_asym_pt3(i,j,q,k) * wm_interm_71_so_asym_pt3(p,k,i,j)
end do 
end do 
end do 

term(65) = term(65) * (-0.5d+0) 
term(67) = term(67) * (-0.5d+0) 
term(69) = term(69) * (-0.5d+0) 
term(70) = term(70) * (-0.5d+0) 
term(71) = term(71) * (-0.5d+0) 
term(72) = term(72) * (-0.5d+0) 
term(73) = term(73) * (-1.0d+0) 
term(74) = term(74) * (2.0d+0) 
term(75) = term(75) * (-1.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (-1.0d+0) 
term(79) = term(79) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(80) = term(80) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_53_so_asym_pt3(a,b)
term(81) = term(81) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_54_so_asym_pt3(a,b)
end do 
end do 
end do 

term(80) = term(80) * (-8.0d+0) 
term(81) = term(81) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(82) = term(82) + wm_interm_12_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(83) = term(83) + wm_interm_14_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(84) = term(84) + wm_interm_10_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(85) = term(85) + wm_interm_3_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(86) = term(86) + wm_interm_4_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(87) = term(87) + wm_interm_0_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(88) = term(88) + wm_interm_3_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(89) = term(89) + wm_interm_4_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(90) = term(90) + wm_interm_12_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(91) = term(91) + wm_interm_14_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(92) = term(92) + wm_interm_0_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(93) = term(93) + wm_interm_10_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(94) = term(94) + wm_interm_37_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(95) = term(95) + wm_interm_39_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(96) = term(96) + wm_interm_32_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(97) = term(97) + wm_interm_33_so_asym_pt3(a,p,i,q) * wm_interm_51_so_asym_pt3(a,i)
term(98) = term(98) + wm_interm_32_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(99) = term(99) + wm_interm_33_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(100) = term(100) + wm_interm_37_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(101) = term(101) + wm_interm_39_so_asym_pt3(a,p,i,q) * wm_interm_52_so_asym_pt3(a,i)
term(102) = term(102) + wm_interm_63_so_asym_pt3(a,i) * wm_interm_66_so_asym_pt3(a,p,i,q)
term(103) = term(103) + wm_interm_64_so_asym_pt3(a,i) * wm_interm_66_so_asym_pt3(a,p,i,q)
term(104) = term(104) + wm_interm_65_so_asym_pt3(a,i) * wm_interm_66_so_asym_pt3(a,p,i,q)
term(105) = term(105) + wm_interm_66_so_asym_pt3(a,p,i,q) * wm_interm_67_so_asym_pt3(a,i)
term(106) = term(106) + wm_interm_66_so_asym_pt3(a,p,i,q) * wm_interm_68_so_asym_pt3(a,i)
end do 
end do 

term(82) = term(82) * (-4.0d+0) 
term(83) = term(83) * (8.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-1.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-1.0d+0) 
term(93) = term(93) * (2.0d+0) 
term(94) = term(94) * (-16.0d+0) 
term(95) = term(95) * (16.0d+0) 
term(96) = term(96) * (8.0d+0) 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (8.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(107) = term(107) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_7_so_asym_pt3(j,k,q,i)
term(108) = term(108) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_8_so_asym_pt3(j,k,q,i)
term(109) = term(109) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_7_so_asym_pt3(k,j,q,i)
term(110) = term(110) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_8_so_asym_pt3(k,j,q,i)
term(111) = term(111) + wm_interm_36_so_asym_pt3(i,j,q,k) * wm_interm_71_so_asym_pt3(p,k,j,i)
end do 
end do 
end do 

term(107) = term(107) * (-0.5d+0) 
term(110) = term(110) * (-0.5d+0) 
term(111) = term(111) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(112) = term(112) + wm_interm_13_so_asym_pt3(i,j) * wm_interm_71_so_asym_pt3(p,j,q,i)
term(113) = term(113) + wm_interm_15_so_asym_pt3(i,j) * wm_interm_71_so_asym_pt3(p,j,q,i)
term(114) = term(114) + wm_interm_11_so_asym_pt3(i,j) * wm_interm_71_so_asym_pt3(p,j,q,i)
term(115) = term(115) + wm_interm_38_so_asym_pt3(i,j) * wm_interm_71_so_asym_pt3(p,j,q,i)
term(116) = term(116) + wm_interm_40_so_asym_pt3(i,j) * wm_interm_71_so_asym_pt3(p,j,q,i)
end do 
end do 

term(112) = term(112) * (-1.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (-1.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(117) = term(117) + wm_interm_36_so_asym_pt3(i,j,k,q) * wm_interm_71_so_asym_pt3(p,k,j,i)
end do 
end do 
end do 

term(117) = term(117) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(118) = term(118) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_53_so_asym_pt3(a,b)
term(119) = term(119) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_54_so_asym_pt3(a,b)
term(120) = term(120) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_53_so_asym_pt3(a,b)
term(121) = term(121) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_54_so_asym_pt3(a,b)
term(122) = term(122) + s1(b,i) * t2(a,p,q,i) * wm_interm_28_so_asym_pt3(a,b)
term(123) = term(123) + s1(b,i) * t2(a,p,q,i) * wm_interm_30_so_asym_pt3(a,b)
term(124) = term(124) + s1(b,i) * t2(a,p,q,i) * wm_interm_27_so_asym_pt3(a,b)
term(125) = term(125) + s1(b,i) * t2(a,p,q,i) * wm_interm_49_so_asym_pt3(a,b)
term(126) = term(126) + s1(b,i) * t2(a,p,q,i) * wm_interm_50_so_asym_pt3(a,b)
end do 
end do 
end do 

term(118) = term(118) * (2.0d+0) 
term(119) = term(119) * (-1.0d+0) 
term(120) = term(120) * (8.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (-1.0d+0) 
term(123) = term(123) * (2.0d+0) 
term(124) = term(124) * (-1.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(127) = term(127) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_56_so_asym_pt3(p,i,q,j)
term(128) = term(128) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_56_so_asym_pt3(p,q,i,j)
term(129) = term(129) + wm_interm_56_so_asym_pt3(p,i,q,j) * wm_interm_57_so_asym_pt3(i,j)
term(130) = term(130) + wm_interm_56_so_asym_pt3(p,q,i,j) * wm_interm_57_so_asym_pt3(i,j)
term(131) = term(131) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_58_so_asym_pt3(p,q,i,j)
term(132) = term(132) + wm_interm_57_so_asym_pt3(i,j) * wm_interm_58_so_asym_pt3(p,q,i,j)
term(133) = term(133) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_59_so_asym_pt3(p,i,q,j)
term(134) = term(134) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_59_so_asym_pt3(p,q,i,j)
term(135) = term(135) + wm_interm_57_so_asym_pt3(i,j) * wm_interm_59_so_asym_pt3(p,i,q,j)
term(136) = term(136) + wm_interm_57_so_asym_pt3(i,j) * wm_interm_59_so_asym_pt3(p,q,i,j)
term(137) = term(137) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_60_so_asym_pt3(p,q,i,j)
term(138) = term(138) + wm_interm_57_so_asym_pt3(i,j) * wm_interm_60_so_asym_pt3(p,q,i,j)
term(139) = term(139) + wm_interm_71_so_asym_pt3(p,i,q,j) * wm_interm_93_so_asym_pt3(j,i)
term(140) = term(140) + wm_interm_90_so_asym_pt3(i,j) * wm_interm_98_so_asym_pt3(p,i,q,j)
end do 
end do 

term(127) = term(127) * (-1.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (2.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (-1.0d+0) 
term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (-2.0d+0) 
term(134) = term(134) * (4.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (-8.0d+0) 
term(137) = term(137) * (-2.0d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (2.0d+0) 
term(140) = term(140) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(141) = term(141) + r2p(vrdav_Rr, p,q,a,i) * s1(b,i) * wm_interm_53_so_asym_pt3(a,b)
term(142) = term(142) + r2p(vrdav_Rr, p,q,a,i) * s1(b,i) * wm_interm_54_so_asym_pt3(a,b)
end do 
end do 
end do 

term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (2.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(143) = term(143) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_53_so_asym_pt3(a,b)
term(144) = term(144) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_54_so_asym_pt3(a,b)
end do 
end do 
end do 

term(143) = term(143) * (2.0d+0) 
term(144) = term(144) * (-1.0d+0) 

do i = 1, nocc 
term(145) = term(145) + wm_interm_55_so_asym_pt3(i,q) * wm_interm_63_so_asym_pt3(p,i)
term(146) = term(146) + wm_interm_57_so_asym_pt3(i,q) * wm_interm_63_so_asym_pt3(p,i)
term(147) = term(147) + wm_interm_55_so_asym_pt3(i,q) * wm_interm_64_so_asym_pt3(p,i)
term(148) = term(148) + wm_interm_57_so_asym_pt3(i,q) * wm_interm_64_so_asym_pt3(p,i)
term(149) = term(149) + wm_interm_55_so_asym_pt3(i,q) * wm_interm_65_so_asym_pt3(p,i)
term(150) = term(150) + wm_interm_57_so_asym_pt3(i,q) * wm_interm_65_so_asym_pt3(p,i)
term(151) = term(151) + wm_interm_55_so_asym_pt3(i,q) * wm_interm_67_so_asym_pt3(p,i)
term(152) = term(152) + wm_interm_57_so_asym_pt3(i,q) * wm_interm_67_so_asym_pt3(p,i)
term(153) = term(153) + wm_interm_55_so_asym_pt3(i,q) * wm_interm_68_so_asym_pt3(p,i)
term(154) = term(154) + wm_interm_57_so_asym_pt3(i,q) * wm_interm_68_so_asym_pt3(p,i)
term(155) = term(155) + wm_interm_13_so_asym_pt3(i,q) * wm_interm_52_so_asym_pt3(p,i)
term(156) = term(156) + wm_interm_15_so_asym_pt3(i,q) * wm_interm_52_so_asym_pt3(p,i)
term(157) = term(157) + wm_interm_11_so_asym_pt3(i,q) * wm_interm_52_so_asym_pt3(p,i)
term(158) = term(158) + wm_interm_13_so_asym_pt3(i,q) * wm_interm_51_so_asym_pt3(p,i)
term(159) = term(159) + wm_interm_15_so_asym_pt3(i,q) * wm_interm_51_so_asym_pt3(p,i)
term(160) = term(160) + wm_interm_11_so_asym_pt3(i,q) * wm_interm_51_so_asym_pt3(p,i)
term(161) = term(161) + wm_interm_38_so_asym_pt3(i,q) * wm_interm_52_so_asym_pt3(p,i)
term(162) = term(162) + wm_interm_40_so_asym_pt3(i,q) * wm_interm_52_so_asym_pt3(p,i)
term(163) = term(163) + wm_interm_38_so_asym_pt3(i,q) * wm_interm_51_so_asym_pt3(p,i)
term(164) = term(164) + wm_interm_40_so_asym_pt3(i,q) * wm_interm_51_so_asym_pt3(p,i)
term(165) = term(165) + wm_interm_52_so_asym_pt3(p,i) * wm_interm_93_so_asym_pt3(i,q)
term(166) = term(166) + wm_interm_51_so_asym_pt3(p,i) * wm_interm_93_so_asym_pt3(i,q)
term(167) = term(167) + wm_interm_90_so_asym_pt3(q,i) * wm_interm_95_so_asym_pt3(p,i)
term(168) = term(168) + wm_interm_90_so_asym_pt3(q,i) * wm_interm_97_so_asym_pt3(p,i)
end do 

term(145) = term(145) * (-2.0d+0) 
term(146) = term(146) * (4.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(150) = term(150) * (-2.0d+0) 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * (8.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * (-1.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-1.0d+0) 
term(158) = term(158) * (2.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (4.0d+0) 
term(163) = term(163) * (8.0d+0) 
term(164) = term(164) * (-8.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (-4.0d+0) 
term(167) = term(167) * (2.0d+0) 
term(168) = term(168) * (-4.0d+0) 


    calc_D_vo_wm_so_asym_pt3 = zero
    do s = 0, 168
    calc_D_vo_wm_so_asym_pt3 = calc_D_vo_wm_so_asym_pt3 + term(s)
    end do

    end function calc_D_vo_wm_so_asym_pt3
    
    function calc_D_vv_wm_so_asym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_asym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b 
    real(F64), dimension(0:53) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_76_so_asym_pt3(p,k,j,i)
term(1) = term(1) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_79_so_asym_pt3(p,k,j,i)
term(2) = term(2) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_88_so_asym_pt3(q,j,k,i)
term(3) = term(3) + wm_interm_71_so_asym_pt3(p,i,j,k) * wm_interm_86_so_asym_pt3(q,k,j,i)
end do 
end do 
end do 

term(1) = term(1) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(4) = term(4) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_76_so_asym_pt3(p,k,i,j)
term(5) = term(5) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_77_so_asym_pt3(p,k,i,j)
term(6) = term(6) + wm_interm_1_so_asym_pt3(q,i,j,k) * wm_interm_79_so_asym_pt3(p,k,i,j)
end do 
end do 
end do 

term(4) = term(4) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + wm_interm_29_so_asym_pt3(a,q,i,j) * wm_interm_83_so_asym_pt3(a,p,i,j)
term(8) = term(8) + wm_interm_25_so_asym_pt3(a,q,i,j) * wm_interm_83_so_asym_pt3(a,p,i,j)
term(9) = term(9) + wm_interm_24_so_asym_pt3(a,q,i,j) * wm_interm_83_so_asym_pt3(a,p,i,j)
term(10) = term(10) + wm_interm_48_so_asym_pt3(a,q,i,j) * wm_interm_83_so_asym_pt3(a,p,i,j)
term(11) = term(11) + wm_interm_47_so_asym_pt3(a,q,i,j) * wm_interm_83_so_asym_pt3(a,p,i,j)
term(12) = term(12) + wm_interm_74_so_asym_pt3(q,a,i,j) * wm_interm_87_so_asym_pt3(p,a,i,j)
term(13) = term(13) + wm_interm_73_so_asym_pt3(q,a,i,j) * wm_interm_87_so_asym_pt3(p,a,i,j)
term(14) = term(14) + wm_interm_66_so_asym_pt3(q,a,i,j) * wm_interm_87_so_asym_pt3(p,a,i,j)
term(15) = term(15) + wm_interm_72_so_asym_pt3(q,a,i,j) * wm_interm_87_so_asym_pt3(p,a,i,j)
term(16) = term(16) + wm_interm_66_so_asym_pt3(a,p,i,j) * wm_interm_87_so_asym_pt3(a,q,i,j)
end do 
end do 
end do 

term(8) = term(8) * (-2.0d+0) 
term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (8.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (2.0d+0) 

do a = nocc + 1, nactive 
term(17) = term(17) + wm_interm_5_so_asym_pt3(p,a) * wm_interm_75_so_asym_pt3(q,a)
term(18) = term(18) + wm_interm_6_so_asym_pt3(p,a) * wm_interm_75_so_asym_pt3(q,a)
term(19) = term(19) + wm_interm_75_so_asym_pt3(q,a) * wm_interm_9_so_asym_pt3(p,a)
term(20) = term(20) + wm_interm_34_so_asym_pt3(p,a) * wm_interm_75_so_asym_pt3(q,a)
term(21) = term(21) + wm_interm_35_so_asym_pt3(p,a) * wm_interm_75_so_asym_pt3(q,a)
term(22) = term(22) + wm_interm_75_so_asym_pt3(a,p) * wm_interm_9_so_asym_pt3(a,q)
term(23) = term(23) + wm_interm_5_so_asym_pt3(a,q) * wm_interm_75_so_asym_pt3(a,p)
term(24) = term(24) + wm_interm_6_so_asym_pt3(a,q) * wm_interm_75_so_asym_pt3(a,p)
term(25) = term(25) + wm_interm_34_so_asym_pt3(a,q) * wm_interm_75_so_asym_pt3(a,p)
term(26) = term(26) + wm_interm_35_so_asym_pt3(a,q) * wm_interm_75_so_asym_pt3(a,p)
term(27) = term(27) + wm_interm_53_so_asym_pt3(q,a) * wm_interm_89_so_asym_pt3(p,a)
term(28) = term(28) + wm_interm_54_so_asym_pt3(q,a) * wm_interm_89_so_asym_pt3(p,a)
term(29) = term(29) + wm_interm_54_so_asym_pt3(a,p) * wm_interm_89_so_asym_pt3(a,q)
term(30) = term(30) + wm_interm_53_so_asym_pt3(a,p) * wm_interm_89_so_asym_pt3(a,q)
end do 

term(18) = term(18) * (-2.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(31) = term(31) + wm_interm_21_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(32) = term(32) + wm_interm_22_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(33) = term(33) + wm_interm_23_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(34) = term(34) + wm_interm_24_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(35) = term(35) + wm_interm_25_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(36) = term(36) + wm_interm_26_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(37) = term(37) + wm_interm_45_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(38) = term(38) + wm_interm_46_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(39) = term(39) + wm_interm_47_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(40) = term(40) + wm_interm_48_so_asym_pt3(p,q,i,j) * wm_interm_78_so_asym_pt3(i,j)
term(41) = term(41) + wm_interm_17_so_asym_pt3(i,j) * wm_interm_83_so_asym_pt3(q,p,i,j)
term(42) = term(42) + wm_interm_18_so_asym_pt3(i,j) * wm_interm_83_so_asym_pt3(q,p,i,j)
term(43) = term(43) + wm_interm_19_so_asym_pt3(i,j) * wm_interm_83_so_asym_pt3(q,p,i,j)
term(44) = term(44) + wm_interm_41_so_asym_pt3(i,j) * wm_interm_83_so_asym_pt3(q,p,i,j)
term(45) = term(45) + wm_interm_42_so_asym_pt3(i,j) * wm_interm_83_so_asym_pt3(q,p,i,j)
term(46) = term(46) + wm_interm_43_so_asym_pt3(i,j) * wm_interm_83_so_asym_pt3(q,p,i,j)
term(47) = term(47) + wm_interm_55_so_asym_pt3(i,j) * wm_interm_87_so_asym_pt3(p,q,i,j)
term(48) = term(48) + wm_interm_57_so_asym_pt3(i,j) * wm_interm_87_so_asym_pt3(p,q,i,j)
term(49) = term(49) + wm_interm_72_so_asym_pt3(q,p,i,j) * wm_interm_90_so_asym_pt3(i,j)
term(50) = term(50) + wm_interm_73_so_asym_pt3(q,p,i,j) * wm_interm_90_so_asym_pt3(i,j)
term(51) = term(51) + wm_interm_74_so_asym_pt3(q,p,i,j) * wm_interm_90_so_asym_pt3(i,j)
end do 
end do 

term(32) = term(32) * (-2.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 

do i = 1, nocc 
term(52) = term(52) + wm_interm_52_so_asym_pt3(p,i) * wm_interm_91_so_asym_pt3(q,i)
term(53) = term(53) + wm_interm_51_so_asym_pt3(p,i) * wm_interm_91_so_asym_pt3(q,i)
end do 

term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 


    calc_D_vv_wm_so_asym_pt3 = zero
    do s = 0, 53
    calc_D_vv_wm_so_asym_pt3 = calc_D_vv_wm_so_asym_pt3 + term(s)
    end do

    end function calc_D_vv_wm_so_asym_pt3

  end module density_exc_exc_functions_so_pt0123_asym
