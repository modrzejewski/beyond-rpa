module so_cc3_pt012
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    !
    ! File generated automatically on 2018-04-18 11:46:20
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_so_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_52_so_pt1

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_12_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_13_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_22_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_24_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_27_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_31_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_32_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_33_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_45_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_60_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_61_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_62_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_63_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_65_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_66_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_67_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_68_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_70_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_71_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_72_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_75_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_78_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_81_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_84_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_85_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_92_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_93_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_94_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_95_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_96_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_97_so_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_98_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_99_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_100_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_101_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_102_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_103_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_104_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_105_so_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_106_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_108_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_109_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_110_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_111_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_112_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_113_so_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_so_pt2 



    contains

    subroutine wm_so_intermediates_cc3_init_pt0(nocc, nactive)
      integer, intent(in) :: nocc
      integer, intent(in) :: nactive
    
    end subroutine wm_so_intermediates_cc3_init_pt0
    
    subroutine wm_so_intermediates_cc3_free_pt0
    
    end subroutine wm_so_intermediates_cc3_free_pt0
    
    subroutine wm_so_intermediates_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

    end subroutine wm_so_intermediates_cc3_pt0

        subroutine wm_so_intermediates_cc3_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_so_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_5_so_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_so_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_14_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_15_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_17_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_35_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_40_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_so_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_so_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_so_pt1(nocc+1: nactive, 1: nocc))
wm_interm_0_so_pt1 = zero 
wm_interm_1_so_pt1 = zero 
wm_interm_2_so_pt1 = zero 
wm_interm_3_so_pt1 = zero 
wm_interm_4_so_pt1 = zero 
wm_interm_5_so_pt1 = zero 
wm_interm_6_so_pt1 = zero 
wm_interm_7_so_pt1 = zero 
wm_interm_8_so_pt1 = zero 
wm_interm_9_so_pt1 = zero 
wm_interm_10_so_pt1 = zero 
wm_interm_11_so_pt1 = zero 
wm_interm_12_so_pt1 = zero 
wm_interm_13_so_pt1 = zero 
wm_interm_14_so_pt1 = zero 
wm_interm_15_so_pt1 = zero 
wm_interm_16_so_pt1 = zero 
wm_interm_17_so_pt1 = zero 
wm_interm_18_so_pt1 = zero 
wm_interm_19_so_pt1 = zero 
wm_interm_20_so_pt1 = zero 
wm_interm_21_so_pt1 = zero 
wm_interm_22_so_pt1 = zero 
wm_interm_23_so_pt1 = zero 
wm_interm_24_so_pt1 = zero 
wm_interm_25_so_pt1 = zero 
wm_interm_26_so_pt1 = zero 
wm_interm_27_so_pt1 = zero 
wm_interm_28_so_pt1 = zero 
wm_interm_29_so_pt1 = zero 
wm_interm_30_so_pt1 = zero 
wm_interm_31_so_pt1 = zero 
wm_interm_32_so_pt1 = zero 
wm_interm_33_so_pt1 = zero 
wm_interm_34_so_pt1 = zero 
wm_interm_35_so_pt1 = zero 
wm_interm_36_so_pt1 = zero 
wm_interm_37_so_pt1 = zero 
wm_interm_38_so_pt1 = zero 
wm_interm_39_so_pt1 = zero 
wm_interm_40_so_pt1 = zero 
wm_interm_41_so_pt1 = zero 
wm_interm_42_so_pt1 = zero 
wm_interm_43_so_pt1 = zero 
wm_interm_44_so_pt1 = zero 
wm_interm_45_so_pt1 = zero 
wm_interm_46_so_pt1 = zero 
wm_interm_47_so_pt1 = zero 
wm_interm_48_so_pt1 = zero 
wm_interm_49_so_pt1 = zero 
wm_interm_50_so_pt1 = zero 
wm_interm_51_so_pt1 = zero 
wm_interm_52_so_pt1 = zero 

    end subroutine wm_so_intermediates_cc3_init_pt1
    
    subroutine wm_so_intermediates_cc3_free_pt1
    deallocate(wm_interm_0_so_pt1)
deallocate(wm_interm_1_so_pt1)
deallocate(wm_interm_2_so_pt1)
deallocate(wm_interm_3_so_pt1)
deallocate(wm_interm_4_so_pt1)
deallocate(wm_interm_5_so_pt1)
deallocate(wm_interm_6_so_pt1)
deallocate(wm_interm_7_so_pt1)
deallocate(wm_interm_8_so_pt1)
deallocate(wm_interm_9_so_pt1)
deallocate(wm_interm_10_so_pt1)
deallocate(wm_interm_11_so_pt1)
deallocate(wm_interm_12_so_pt1)
deallocate(wm_interm_13_so_pt1)
deallocate(wm_interm_14_so_pt1)
deallocate(wm_interm_15_so_pt1)
deallocate(wm_interm_16_so_pt1)
deallocate(wm_interm_17_so_pt1)
deallocate(wm_interm_18_so_pt1)
deallocate(wm_interm_19_so_pt1)
deallocate(wm_interm_20_so_pt1)
deallocate(wm_interm_21_so_pt1)
deallocate(wm_interm_22_so_pt1)
deallocate(wm_interm_23_so_pt1)
deallocate(wm_interm_24_so_pt1)
deallocate(wm_interm_25_so_pt1)
deallocate(wm_interm_26_so_pt1)
deallocate(wm_interm_27_so_pt1)
deallocate(wm_interm_28_so_pt1)
deallocate(wm_interm_29_so_pt1)
deallocate(wm_interm_30_so_pt1)
deallocate(wm_interm_31_so_pt1)
deallocate(wm_interm_32_so_pt1)
deallocate(wm_interm_33_so_pt1)
deallocate(wm_interm_34_so_pt1)
deallocate(wm_interm_35_so_pt1)
deallocate(wm_interm_36_so_pt1)
deallocate(wm_interm_37_so_pt1)
deallocate(wm_interm_38_so_pt1)
deallocate(wm_interm_39_so_pt1)
deallocate(wm_interm_40_so_pt1)
deallocate(wm_interm_41_so_pt1)
deallocate(wm_interm_42_so_pt1)
deallocate(wm_interm_43_so_pt1)
deallocate(wm_interm_44_so_pt1)
deallocate(wm_interm_45_so_pt1)
deallocate(wm_interm_46_so_pt1)
deallocate(wm_interm_47_so_pt1)
deallocate(wm_interm_48_so_pt1)
deallocate(wm_interm_49_so_pt1)
deallocate(wm_interm_50_so_pt1)
deallocate(wm_interm_51_so_pt1)
deallocate(wm_interm_52_so_pt1)

    end subroutine wm_so_intermediates_cc3_free_pt1
    
    subroutine wm_so_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, c, k, l 

    !$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_0_so_pt1(c, k) = wm_interm_0_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_1_so_pt1(c, k) = wm_interm_1_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_2_so_pt1(c, k) = wm_interm_2_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_3_so_pt1(c, k) = wm_interm_3_so_pt1(c, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
wm_interm_4_so_pt1(b, c, j, k) = wm_interm_4_so_pt1(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
wm_interm_5_so_pt1(b, c, j, k) = wm_interm_5_so_pt1(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_6_so_pt1(c, k) = wm_interm_6_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_7_so_pt1(c, k) = wm_interm_7_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_8_so_pt1(c, k) = wm_interm_8_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_9_so_pt1(c, k) = wm_interm_9_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_10_so_pt1(b, c, k, j) = wm_interm_10_so_pt1(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_11_so_pt1(b, c, k, j) = wm_interm_11_so_pt1(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_12_so_pt1(b, c, j, k) = wm_interm_12_so_pt1(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_13_so_pt1(b, c, j, k) = wm_interm_13_so_pt1(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_14_so_pt1(c, k) = wm_interm_14_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_15_so_pt1(c, k) = wm_interm_15_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_16_so_pt1(c, k) = wm_interm_16_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_17_so_pt1(c, k) = wm_interm_17_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,i,b,k,c,l)
end do 
end do 
end do 
wm_interm_18_so_pt1(c, j, k, l) = wm_interm_18_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_19_so_pt1(c, j, k, l) = wm_interm_19_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_20_so_pt1(c, j, k, l) = wm_interm_20_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, l, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,l,b,k,c,i)
end do 
end do 
end do 
wm_interm_21_so_pt1(c, j, l, k) = wm_interm_21_so_pt1(c, j, l, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_22_so_pt1(c, k, j, l) = wm_interm_22_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_23_so_pt1(c, j, k, l) = wm_interm_23_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_24_so_pt1(c, j, k, l) = wm_interm_24_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_25_so_pt1(c, k, j, l) = wm_interm_25_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_26_so_pt1(c, j, k, l) = wm_interm_26_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_27_so_pt1(c, j, k, l) = wm_interm_27_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_28_so_pt1(c, j, k, l) = wm_interm_28_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_29_so_pt1(c, k, j, l) = wm_interm_29_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_30_so_pt1(c, k, j, l) = wm_interm_30_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_31_so_pt1(c, j, k, l) = wm_interm_31_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_32_so_pt1(c, k, j, l) = wm_interm_32_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_33_so_pt1(c, k, j, l) = wm_interm_33_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, b,l,a,i)
end do 
end do 
end do 
wm_interm_34_so_pt1(c, k, j, l) = wm_interm_34_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_35_so_pt1(c, j, k, l) = wm_interm_35_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, b,i,a,l)
end do 
end do 
end do 
wm_interm_36_so_pt1(c, j, k, l) = wm_interm_36_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, b,l,a,i)
end do 
end do 
end do 
wm_interm_37_so_pt1(c, j, k, l) = wm_interm_37_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_38_so_pt1(c, j, k, l) = wm_interm_38_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_39_so_pt1(c, j, k, l) = wm_interm_39_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_40_so_pt1(c, j, k, l) = wm_interm_40_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, b,l,a,i)
end do 
end do 
end do 
wm_interm_41_so_pt1(c, j, k, l) = wm_interm_41_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_42_so_pt1(c, k) = wm_interm_42_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_43_so_pt1(c, k) = wm_interm_43_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_44_so_pt1(c, k) = wm_interm_44_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2m(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_45_so_pt1(c, k, j, l) = wm_interm_45_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2m(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_46_so_pt1(c, k, j, l) = wm_interm_46_so_pt1(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2m(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_47_so_pt1(c, j, k, l) = wm_interm_47_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2m(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_48_so_pt1(c, j, k, l) = wm_interm_48_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2m(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_49_so_pt1(c, j, k, l) = wm_interm_49_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2m(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_50_so_pt1(c, j, k, l) = wm_interm_50_so_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_51_so_pt1(c, k) = wm_interm_51_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_52_so_pt1(c, k) = wm_interm_52_so_pt1(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_intermediates_cc3_pt1

        subroutine wm_so_intermediates_cc3_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_8_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_12_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_13_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_so_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_16_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_17_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_so_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_24_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_29_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_30_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_35_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_36_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_39_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_40_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_46_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_50_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_52_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_53_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_55_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_56_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_61_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_62_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_65_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_66_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_68_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_69_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_71_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_72_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_73_so_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_75_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_76_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_77_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_79_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_81_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_82_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_83_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_84_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_85_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_86_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_87_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_88_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_89_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_90_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_91_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_92_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_93_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_94_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_95_so_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_96_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_97_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_98_so_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_99_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_100_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_101_so_pt2(1: nocc, 1: nocc))
allocate(wm_interm_102_so_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_103_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_104_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_105_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_106_so_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_107_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_108_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_109_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_110_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_111_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_112_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_113_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_114_so_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_0_so_pt2 = zero 
wm_interm_1_so_pt2 = zero 
wm_interm_2_so_pt2 = zero 
wm_interm_3_so_pt2 = zero 
wm_interm_4_so_pt2 = zero 
wm_interm_5_so_pt2 = zero 
wm_interm_6_so_pt2 = zero 
wm_interm_7_so_pt2 = zero 
wm_interm_8_so_pt2 = zero 
wm_interm_9_so_pt2 = zero 
wm_interm_10_so_pt2 = zero 
wm_interm_11_so_pt2 = zero 
wm_interm_12_so_pt2 = zero 
wm_interm_13_so_pt2 = zero 
wm_interm_14_so_pt2 = zero 
wm_interm_15_so_pt2 = zero 
wm_interm_16_so_pt2 = zero 
wm_interm_17_so_pt2 = zero 
wm_interm_18_so_pt2 = zero 
wm_interm_19_so_pt2 = zero 
wm_interm_20_so_pt2 = zero 
wm_interm_21_so_pt2 = zero 
wm_interm_22_so_pt2 = zero 
wm_interm_23_so_pt2 = zero 
wm_interm_24_so_pt2 = zero 
wm_interm_25_so_pt2 = zero 
wm_interm_26_so_pt2 = zero 
wm_interm_27_so_pt2 = zero 
wm_interm_28_so_pt2 = zero 
wm_interm_29_so_pt2 = zero 
wm_interm_30_so_pt2 = zero 
wm_interm_31_so_pt2 = zero 
wm_interm_32_so_pt2 = zero 
wm_interm_33_so_pt2 = zero 
wm_interm_34_so_pt2 = zero 
wm_interm_35_so_pt2 = zero 
wm_interm_36_so_pt2 = zero 
wm_interm_37_so_pt2 = zero 
wm_interm_38_so_pt2 = zero 
wm_interm_39_so_pt2 = zero 
wm_interm_40_so_pt2 = zero 
wm_interm_41_so_pt2 = zero 
wm_interm_42_so_pt2 = zero 
wm_interm_43_so_pt2 = zero 
wm_interm_44_so_pt2 = zero 
wm_interm_45_so_pt2 = zero 
wm_interm_46_so_pt2 = zero 
wm_interm_47_so_pt2 = zero 
wm_interm_48_so_pt2 = zero 
wm_interm_49_so_pt2 = zero 
wm_interm_50_so_pt2 = zero 
wm_interm_51_so_pt2 = zero 
wm_interm_52_so_pt2 = zero 
wm_interm_53_so_pt2 = zero 
wm_interm_54_so_pt2 = zero 
wm_interm_55_so_pt2 = zero 
wm_interm_56_so_pt2 = zero 
wm_interm_57_so_pt2 = zero 
wm_interm_58_so_pt2 = zero 
wm_interm_59_so_pt2 = zero 
wm_interm_60_so_pt2 = zero 
wm_interm_61_so_pt2 = zero 
wm_interm_62_so_pt2 = zero 
wm_interm_63_so_pt2 = zero 
wm_interm_64_so_pt2 = zero 
wm_interm_65_so_pt2 = zero 
wm_interm_66_so_pt2 = zero 
wm_interm_67_so_pt2 = zero 
wm_interm_68_so_pt2 = zero 
wm_interm_69_so_pt2 = zero 
wm_interm_70_so_pt2 = zero 
wm_interm_71_so_pt2 = zero 
wm_interm_72_so_pt2 = zero 
wm_interm_73_so_pt2 = zero 
wm_interm_74_so_pt2 = zero 
wm_interm_75_so_pt2 = zero 
wm_interm_76_so_pt2 = zero 
wm_interm_77_so_pt2 = zero 
wm_interm_78_so_pt2 = zero 
wm_interm_79_so_pt2 = zero 
wm_interm_80_so_pt2 = zero 
wm_interm_81_so_pt2 = zero 
wm_interm_82_so_pt2 = zero 
wm_interm_83_so_pt2 = zero 
wm_interm_84_so_pt2 = zero 
wm_interm_85_so_pt2 = zero 
wm_interm_86_so_pt2 = zero 
wm_interm_87_so_pt2 = zero 
wm_interm_88_so_pt2 = zero 
wm_interm_89_so_pt2 = zero 
wm_interm_90_so_pt2 = zero 
wm_interm_91_so_pt2 = zero 
wm_interm_92_so_pt2 = zero 
wm_interm_93_so_pt2 = zero 
wm_interm_94_so_pt2 = zero 
wm_interm_95_so_pt2 = zero 
wm_interm_96_so_pt2 = zero 
wm_interm_97_so_pt2 = zero 
wm_interm_98_so_pt2 = zero 
wm_interm_99_so_pt2 = zero 
wm_interm_100_so_pt2 = zero 
wm_interm_101_so_pt2 = zero 
wm_interm_102_so_pt2 = zero 
wm_interm_103_so_pt2 = zero 
wm_interm_104_so_pt2 = zero 
wm_interm_105_so_pt2 = zero 
wm_interm_106_so_pt2 = zero 
wm_interm_107_so_pt2 = zero 
wm_interm_108_so_pt2 = zero 
wm_interm_109_so_pt2 = zero 
wm_interm_110_so_pt2 = zero 
wm_interm_111_so_pt2 = zero 
wm_interm_112_so_pt2 = zero 
wm_interm_113_so_pt2 = zero 
wm_interm_114_so_pt2 = zero 

    end subroutine wm_so_intermediates_cc3_init_pt2
    
    subroutine wm_so_intermediates_cc3_free_pt2
    deallocate(wm_interm_0_so_pt2)
deallocate(wm_interm_1_so_pt2)
deallocate(wm_interm_2_so_pt2)
deallocate(wm_interm_3_so_pt2)
deallocate(wm_interm_4_so_pt2)
deallocate(wm_interm_5_so_pt2)
deallocate(wm_interm_6_so_pt2)
deallocate(wm_interm_7_so_pt2)
deallocate(wm_interm_8_so_pt2)
deallocate(wm_interm_9_so_pt2)
deallocate(wm_interm_10_so_pt2)
deallocate(wm_interm_11_so_pt2)
deallocate(wm_interm_12_so_pt2)
deallocate(wm_interm_13_so_pt2)
deallocate(wm_interm_14_so_pt2)
deallocate(wm_interm_15_so_pt2)
deallocate(wm_interm_16_so_pt2)
deallocate(wm_interm_17_so_pt2)
deallocate(wm_interm_18_so_pt2)
deallocate(wm_interm_19_so_pt2)
deallocate(wm_interm_20_so_pt2)
deallocate(wm_interm_21_so_pt2)
deallocate(wm_interm_22_so_pt2)
deallocate(wm_interm_23_so_pt2)
deallocate(wm_interm_24_so_pt2)
deallocate(wm_interm_25_so_pt2)
deallocate(wm_interm_26_so_pt2)
deallocate(wm_interm_27_so_pt2)
deallocate(wm_interm_28_so_pt2)
deallocate(wm_interm_29_so_pt2)
deallocate(wm_interm_30_so_pt2)
deallocate(wm_interm_31_so_pt2)
deallocate(wm_interm_32_so_pt2)
deallocate(wm_interm_33_so_pt2)
deallocate(wm_interm_34_so_pt2)
deallocate(wm_interm_35_so_pt2)
deallocate(wm_interm_36_so_pt2)
deallocate(wm_interm_37_so_pt2)
deallocate(wm_interm_38_so_pt2)
deallocate(wm_interm_39_so_pt2)
deallocate(wm_interm_40_so_pt2)
deallocate(wm_interm_41_so_pt2)
deallocate(wm_interm_42_so_pt2)
deallocate(wm_interm_43_so_pt2)
deallocate(wm_interm_44_so_pt2)
deallocate(wm_interm_45_so_pt2)
deallocate(wm_interm_46_so_pt2)
deallocate(wm_interm_47_so_pt2)
deallocate(wm_interm_48_so_pt2)
deallocate(wm_interm_49_so_pt2)
deallocate(wm_interm_50_so_pt2)
deallocate(wm_interm_51_so_pt2)
deallocate(wm_interm_52_so_pt2)
deallocate(wm_interm_53_so_pt2)
deallocate(wm_interm_54_so_pt2)
deallocate(wm_interm_55_so_pt2)
deallocate(wm_interm_56_so_pt2)
deallocate(wm_interm_57_so_pt2)
deallocate(wm_interm_58_so_pt2)
deallocate(wm_interm_59_so_pt2)
deallocate(wm_interm_60_so_pt2)
deallocate(wm_interm_61_so_pt2)
deallocate(wm_interm_62_so_pt2)
deallocate(wm_interm_63_so_pt2)
deallocate(wm_interm_64_so_pt2)
deallocate(wm_interm_65_so_pt2)
deallocate(wm_interm_66_so_pt2)
deallocate(wm_interm_67_so_pt2)
deallocate(wm_interm_68_so_pt2)
deallocate(wm_interm_69_so_pt2)
deallocate(wm_interm_70_so_pt2)
deallocate(wm_interm_71_so_pt2)
deallocate(wm_interm_72_so_pt2)
deallocate(wm_interm_73_so_pt2)
deallocate(wm_interm_74_so_pt2)
deallocate(wm_interm_75_so_pt2)
deallocate(wm_interm_76_so_pt2)
deallocate(wm_interm_77_so_pt2)
deallocate(wm_interm_78_so_pt2)
deallocate(wm_interm_79_so_pt2)
deallocate(wm_interm_80_so_pt2)
deallocate(wm_interm_81_so_pt2)
deallocate(wm_interm_82_so_pt2)
deallocate(wm_interm_83_so_pt2)
deallocate(wm_interm_84_so_pt2)
deallocate(wm_interm_85_so_pt2)
deallocate(wm_interm_86_so_pt2)
deallocate(wm_interm_87_so_pt2)
deallocate(wm_interm_88_so_pt2)
deallocate(wm_interm_89_so_pt2)
deallocate(wm_interm_90_so_pt2)
deallocate(wm_interm_91_so_pt2)
deallocate(wm_interm_92_so_pt2)
deallocate(wm_interm_93_so_pt2)
deallocate(wm_interm_94_so_pt2)
deallocate(wm_interm_95_so_pt2)
deallocate(wm_interm_96_so_pt2)
deallocate(wm_interm_97_so_pt2)
deallocate(wm_interm_98_so_pt2)
deallocate(wm_interm_99_so_pt2)
deallocate(wm_interm_100_so_pt2)
deallocate(wm_interm_101_so_pt2)
deallocate(wm_interm_102_so_pt2)
deallocate(wm_interm_103_so_pt2)
deallocate(wm_interm_104_so_pt2)
deallocate(wm_interm_105_so_pt2)
deallocate(wm_interm_106_so_pt2)
deallocate(wm_interm_107_so_pt2)
deallocate(wm_interm_108_so_pt2)
deallocate(wm_interm_109_so_pt2)
deallocate(wm_interm_110_so_pt2)
deallocate(wm_interm_111_so_pt2)
deallocate(wm_interm_112_so_pt2)
deallocate(wm_interm_113_so_pt2)
deallocate(wm_interm_114_so_pt2)

    end subroutine wm_so_intermediates_cc3_free_pt2
    
    subroutine wm_so_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, c, j, l, k, m 

    !$omp parallel private(a, b, i, c, j, l, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,l,b,k,c,i)
end do 
end do 
end do 
wm_interm_0_so_pt2(c, j, l, k) = wm_interm_0_so_pt2(c, j, l, k) + sum 
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
wm_interm_1_so_pt2(j, k) = wm_interm_1_so_pt2(j, k) + sum 
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
wm_interm_2_so_pt2(j, k) = wm_interm_2_so_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,i,b,k,c,l)
end do 
end do 
end do 
wm_interm_3_so_pt2(c, j, k, l) = wm_interm_3_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_4_so_pt2(c, j, k, l) = wm_interm_4_so_pt2(c, j, k, l) + sum 
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
wm_interm_5_so_pt2(b, c) = wm_interm_5_so_pt2(b, c) + sum 
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
wm_interm_6_so_pt2(b, c, j, k) = wm_interm_6_so_pt2(b, c, j, k) + sum 
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
wm_interm_7_so_pt2(b, c) = wm_interm_7_so_pt2(b, c) + sum 
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
wm_interm_8_so_pt2(b, c, j, k) = wm_interm_8_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_9_so_pt2(c, j, k, l) = wm_interm_9_so_pt2(c, j, k, l) + sum 
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
wm_interm_10_so_pt2(b, c, j, k) = wm_interm_10_so_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_11_so_pt2(b, c) = wm_interm_11_so_pt2(b, c) + sum 
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
wm_interm_12_so_pt2(b, c) = wm_interm_12_so_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,m)
end do 
end do 
wm_interm_13_so_pt2(c, i, j, k, l, m) = wm_interm_13_so_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
wm_interm_14_so_pt2(i, j, k, l) = wm_interm_14_so_pt2(i, j, k, l) + sum 
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
wm_interm_15_so_pt2(b, c, j, k) = wm_interm_15_so_pt2(b, c, j, k) + sum 
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
wm_interm_16_so_pt2(j, k) = wm_interm_16_so_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_17_so_pt2(c, k, j, l) = wm_interm_17_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_18_so_pt2(c, k, j, l) = wm_interm_18_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_19_so_pt2(c, j, k, l) = wm_interm_19_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_20_so_pt2(c, j, k, l) = wm_interm_20_so_pt2(c, j, k, l) + sum 
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
wm_interm_21_so_pt2(i, j, k, l) = wm_interm_21_so_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,k,c,j) * s2(a,b,l,m)
end do 
end do 
wm_interm_22_so_pt2(c, i, k, j, l, m) = wm_interm_22_so_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
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
wm_interm_23_so_pt2(j, k) = wm_interm_23_so_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,m)
end do 
end do 
wm_interm_24_so_pt2(c, j, k, i, l, m) = wm_interm_24_so_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_25_so_pt2(c, j, k, l) = wm_interm_25_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_26_so_pt2(c, j, k, l) = wm_interm_26_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,l,m)
end do 
end do 
wm_interm_27_so_pt2(c, i, j, k, l, m) = wm_interm_27_so_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_28_so_pt2(c, k) = wm_interm_28_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_29_so_pt2(c, k) = wm_interm_29_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_30_so_pt2(c, k) = wm_interm_30_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_31_so_pt2(c, k) = wm_interm_31_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_32_so_pt2(c, k) = wm_interm_32_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_33_so_pt2(c, k) = wm_interm_33_so_pt2(c, k) + sum 
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
wm_interm_34_so_pt2(b, c, j, k) = wm_interm_34_so_pt2(b, c, j, k) + sum 
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
wm_interm_35_so_pt2(b, c, j, k) = wm_interm_35_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_36_so_pt2(c, k) = wm_interm_36_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_37_so_pt2(c, k) = wm_interm_37_so_pt2(c, k) + sum 
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
wm_interm_38_so_pt2(b, c, j, k) = wm_interm_38_so_pt2(b, c, j, k) + sum 
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
wm_interm_39_so_pt2(b, c, j, k) = wm_interm_39_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_40_so_pt2(c, j, k, l) = wm_interm_40_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_41_so_pt2(c, j, k, l) = wm_interm_41_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, b,i,a,l)
end do 
end do 
end do 
wm_interm_42_so_pt2(c, j, k, l) = wm_interm_42_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_43_so_pt2(c, k) = wm_interm_43_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_44_so_pt2(c, k) = wm_interm_44_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_45_so_pt2(c, k) = wm_interm_45_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_46_so_pt2(c, j, k, l) = wm_interm_46_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_47_so_pt2(c, j, k, l) = wm_interm_47_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, b,l,a,i)
end do 
end do 
end do 
wm_interm_48_so_pt2(c, j, k, l) = wm_interm_48_so_pt2(c, j, k, l) + sum 
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
wm_interm_49_so_pt2(b, c, j, k) = wm_interm_49_so_pt2(b, c, j, k) + sum 
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
wm_interm_50_so_pt2(b, c, j, k) = wm_interm_50_so_pt2(b, c, j, k) + sum 
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
wm_interm_51_so_pt2(b, c, j, k) = wm_interm_51_so_pt2(b, c, j, k) + sum 
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
wm_interm_52_so_pt2(b, c, j, k) = wm_interm_52_so_pt2(b, c, j, k) + sum 
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
wm_interm_53_so_pt2(b, c, j, k) = wm_interm_53_so_pt2(b, c, j, k) + sum 
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
wm_interm_54_so_pt2(b, c, j, k) = wm_interm_54_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, b,l,a,i)
end do 
end do 
end do 
wm_interm_55_so_pt2(c, j, k, l) = wm_interm_55_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, b,l,a,i)
end do 
end do 
end do 
wm_interm_56_so_pt2(c, k, j, l) = wm_interm_56_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_57_so_pt2(c, k, j, l) = wm_interm_57_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_58_so_pt2(c, k, j, l) = wm_interm_58_so_pt2(c, k, j, l) + sum 
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
wm_interm_59_so_pt2(b, c, j, k) = wm_interm_59_so_pt2(b, c, j, k) + sum 
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
wm_interm_60_so_pt2(b, c) = wm_interm_60_so_pt2(b, c) + sum 
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
wm_interm_61_so_pt2(b, c) = wm_interm_61_so_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_62_so_pt2(c, j, k, i, l, m) = wm_interm_62_so_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, b,l,a,m)
end do 
end do 
wm_interm_63_so_pt2(c, j, k, i, l, m) = wm_interm_63_so_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
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
wm_interm_64_so_pt2(b, c) = wm_interm_64_so_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * r2p(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_65_so_pt2(c, i, k, j, l, m) = wm_interm_65_so_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_66_so_pt2(c, i, j, k, l, m) = wm_interm_66_so_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, b,l,a,m)
end do 
end do 
wm_interm_67_so_pt2(c, i, j, k, l, m) = wm_interm_67_so_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * r2p(vrdav_Rr, b,l,a,m)
end do 
end do 
wm_interm_68_so_pt2(c, i, k, j, l, m) = wm_interm_68_so_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_69_so_pt2(c, j, k, l) = wm_interm_69_so_pt2(c, j, k, l) + sum 
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
wm_interm_70_so_pt2(j, k) = wm_interm_70_so_pt2(j, k) + sum 
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
wm_interm_71_so_pt2(j, k) = wm_interm_71_so_pt2(j, k) + sum 
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
wm_interm_72_so_pt2(j, k) = wm_interm_72_so_pt2(j, k) + sum 
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
wm_interm_73_so_pt2(i, j, k, l) = wm_interm_73_so_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_74_so_pt2(c, j, k, l) = wm_interm_74_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * t2(a,b,l,m)
end do 
end do 
wm_interm_75_so_pt2(c, i, k, j, l, m) = wm_interm_75_so_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_76_so_pt2(c, k, j, l) = wm_interm_76_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_77_so_pt2(c, k, j, l) = wm_interm_77_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,m)
end do 
end do 
wm_interm_78_so_pt2(c, j, k, i, l, m) = wm_interm_78_so_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_79_so_pt2(c, j, k, l) = wm_interm_79_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_80_so_pt2(c, j, k, l) = wm_interm_80_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,l,m)
end do 
end do 
wm_interm_81_so_pt2(c, i, j, k, l, m) = wm_interm_81_so_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2m(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_82_so_pt2(c, j, k, l) = wm_interm_82_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2m(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_83_so_pt2(c, j, k, l) = wm_interm_83_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_84_so_pt2(c, k) = wm_interm_84_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_85_so_pt2(c, k) = wm_interm_85_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2m(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_86_so_pt2(c, j, k, l) = wm_interm_86_so_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2m(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_87_so_pt2(c, j, k, l) = wm_interm_87_so_pt2(c, j, k, l) + sum 
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
wm_interm_88_so_pt2(b, c, j, k) = wm_interm_88_so_pt2(b, c, j, k) + sum 
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
wm_interm_89_so_pt2(b, c, j, k) = wm_interm_89_so_pt2(b, c, j, k) + sum 
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
wm_interm_90_so_pt2(b, c, j, k) = wm_interm_90_so_pt2(b, c, j, k) + sum 
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
wm_interm_91_so_pt2(b, c, j, k) = wm_interm_91_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2m(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_92_so_pt2(c, k, j, l) = wm_interm_92_so_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2m(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_93_so_pt2(c, k, j, l) = wm_interm_93_so_pt2(c, k, j, l) + sum 
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
wm_interm_94_so_pt2(b, c) = wm_interm_94_so_pt2(b, c) + sum 
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
wm_interm_95_so_pt2(b, c) = wm_interm_95_so_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2m(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_96_so_pt2(c, j, k, i, l, m) = wm_interm_96_so_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * r2m(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_97_so_pt2(c, i, k, j, l, m) = wm_interm_97_so_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_98_so_pt2(c, i, j, k, l, m) = wm_interm_98_so_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
wm_interm_99_so_pt2(j, k) = wm_interm_99_so_pt2(j, k) + sum 
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
wm_interm_100_so_pt2(j, k) = wm_interm_100_so_pt2(j, k) + sum 
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
wm_interm_101_so_pt2(j, k) = wm_interm_101_so_pt2(j, k) + sum 
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
wm_interm_102_so_pt2(i, j, k, l) = wm_interm_102_so_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_103_so_pt2(c, k) = wm_interm_103_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_104_so_pt2(c, k) = wm_interm_104_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_105_so_pt2(c, k) = wm_interm_105_so_pt2(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_106_so_pt2(c, k) = wm_interm_106_so_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s1(a,i)
end do 
end do 
wm_interm_107_so_pt2(b, c, j, k) = wm_interm_107_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s1(a,i)
end do 
end do 
wm_interm_108_so_pt2(b, c, k, j) = wm_interm_108_so_pt2(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s1(a,i)
end do 
end do 
wm_interm_109_so_pt2(b, c, k, j) = wm_interm_109_so_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t1(a,i)
end do 
end do 
wm_interm_110_so_pt2(b, c, j, k) = wm_interm_110_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t1(a,i)
end do 
end do 
wm_interm_111_so_pt2(b, c, k, j) = wm_interm_111_so_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t1(a,i)
end do 
end do 
wm_interm_112_so_pt2(b, c, j, k) = wm_interm_112_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t1(a,i)
end do 
end do 
wm_interm_113_so_pt2(b, c, k, j) = wm_interm_113_so_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t1(a,i)
end do 
end do 
wm_interm_114_so_pt2(b, c, j, k) = wm_interm_114_so_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_intermediates_cc3_pt2
    
    
    function calc_D_oo_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, c, b, a 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rl, a,p,b,j,c,i) * r3(vrdav_Rr, a,i,b,q,c,j)
term(1) = term(1) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,j,b,q,c,i)
term(2) = term(2) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,q,b,j,c,i)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-3.0d+0) 
term(1) = term(1) * (-3.0d+0) 
term(2) = term(2) * (2.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(3) = term(3) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,i,b,q,c,j)
term(4) = term(4) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,q,b,i,c,j)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-4.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(5) = term(5) + r3(vrdav_Rl, a,p,b,j,c,i) * r3(vrdav_Rr, a,j,b,q,c,i)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (4.0d+0) 


    calc_D_oo_wm_so_cc3_pt0 = zero
    do s = 0, 5
    calc_D_oo_wm_so_cc3_pt0 = calc_D_oo_wm_so_cc3_pt0 + term(s)
    end do

    end function calc_D_oo_wm_so_cc3_pt0
    
    function calc_D_ov_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, a,j,b,i)
term(1) = term(1) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, a,i,b,j)
term(2) = term(2) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, b,i,a,j)
term(3) = term(3) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, a,j,b,i)
term(4) = term(4) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 
term(3) = term(3) * (3.9999999999999996d+0) 
term(4) = term(4) * (-3.9999999999999996d+0) 


    calc_D_ov_wm_so_cc3_pt0 = zero
    do s = 0, 4
    calc_D_ov_wm_so_cc3_pt0 = calc_D_ov_wm_so_cc3_pt0 + term(s)
    end do

    end function calc_D_ov_wm_so_cc3_pt0
    
    function calc_D_vo_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(1) = term(1) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,b,j,p,i)
end do 
end do 
end do 
end do 

term(0) = term(0) * (6.0d+0) 
term(1) = term(1) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(2) = term(2) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,p,q)
end do 
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(3) = term(3) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,p,q)
end do 
end do 
end do 
end do 

term(3) = term(3) * (4.0d+0) 


    calc_D_vo_wm_so_cc3_pt0 = zero
    do s = 0, 3
    calc_D_vo_wm_so_cc3_pt0 = calc_D_vo_wm_so_cc3_pt0 + term(s)
    end do

    end function calc_D_vo_wm_so_cc3_pt0
    
    function calc_D_vv_wm_so_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, k, b, a 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rl, a,i,b,k,q,j) * r3(vrdav_Rr, a,k,b,j,p,i)
term(1) = term(1) + r3(vrdav_Rl, a,i,b,k,q,j) * r3(vrdav_Rr, a,j,b,k,p,i)
term(2) = term(2) + r3(vrdav_Rl, a,k,b,i,q,j) * r3(vrdav_Rr, a,j,b,k,p,i)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (3.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (3.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(3) = term(3) + r3(vrdav_Rl, a,k,b,i,q,j) * r3(vrdav_Rr, a,k,b,j,p,i)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(4) = term(4) + r3(vrdav_Rl, a,j,b,k,q,i) * r3(vrdav_Rr, a,k,b,j,p,i)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + r3(vrdav_Rl, a,j,b,k,q,i) * r3(vrdav_Rr, a,j,b,k,p,i)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (4.0d+0) 


    calc_D_vv_wm_so_cc3_pt0 = zero
    do s = 0, 5
    calc_D_vv_wm_so_cc3_pt0 = calc_D_vv_wm_so_cc3_pt0 + term(s)
    end do

    end function calc_D_vv_wm_so_cc3_pt0
    
    
    function calc_D_oo_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_cc3_pt1
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
    real(F64), dimension(0:27) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,b,i,p) * wm_interm_4_so_pt1(a,b,q,i)
term(1) = term(1) + s2(a,b,p,i) * wm_interm_5_so_pt1(a,b,i,q)
term(2) = term(2) + s2(a,b,p,i) * wm_interm_4_so_pt1(a,b,i,q)
term(3) = term(3) + s2(a,b,p,i) * wm_interm_4_so_pt1(a,b,q,i)
term(4) = term(4) + s2(a,b,i,p) * wm_interm_5_so_pt1(a,b,q,i)
term(5) = term(5) + s2(a,b,p,i) * wm_interm_5_so_pt1(a,b,q,i)
term(6) = term(6) + t2(a,b,q,i) * wm_interm_10_so_pt1(b,a,i,p)
term(7) = term(7) + t2(a,b,q,i) * wm_interm_11_so_pt1(b,a,p,i)
term(8) = term(8) + t2(a,b,q,i) * wm_interm_11_so_pt1(b,a,i,p)
term(9) = term(9) + t2(a,b,q,i) * wm_interm_12_so_pt1(b,a,p,i)
term(10) = term(10) + t2(a,b,q,i) * wm_interm_11_so_pt1(a,b,i,p)
term(11) = term(11) + t2(a,b,q,i) * wm_interm_12_so_pt1(a,b,i,p)
term(12) = term(12) + t2(a,b,q,i) * wm_interm_13_so_pt1(b,a,i,p)
term(13) = term(13) + t2(a,b,q,i) * wm_interm_10_so_pt1(a,b,p,i)
term(14) = term(14) + t2(a,b,q,i) * wm_interm_10_so_pt1(a,b,i,p)
term(15) = term(15) + t2(a,b,q,i) * wm_interm_12_so_pt1(a,b,p,i)
term(16) = term(16) + t2(a,b,q,i) * wm_interm_13_so_pt1(b,a,p,i)
term(17) = term(17) + t2(a,b,q,i) * wm_interm_13_so_pt1(a,b,p,i)
end do 
end do 
end do 

term(0) = term(0) * (6.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (6.0d+0) 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (0.3333333333333333d+0) 
term(7) = term(7) * (0.6666666666666666d+0) 
term(8) = term(8) * (-1.3333333333333333d+0) 
term(9) = term(9) * (0.6666666666666666d+0) 
term(10) = term(10) * (0.6666666666666666d+0) 
term(11) = term(11) * (0.6666666666666666d+0) 
term(12) = term(12) * (0.3333333333333333d+0) 
term(13) = term(13) * (0.3333333333333333d+0) 
term(14) = term(14) * (-0.6666666666666666d+0) 
term(15) = term(15) * (-1.3333333333333333d+0) 
term(16) = term(16) * (-0.6666666666666666d+0) 
term(17) = term(17) * (0.3333333333333333d+0) 

do a = nocc + 1, nactive 
term(18) = term(18) + r1(vrdav_Rl, a,p) * wm_interm_0_so_pt1(a,q)
term(19) = term(19) + r1(vrdav_Rl, a,p) * wm_interm_1_so_pt1(a,q)
term(20) = term(20) + r1(vrdav_Rl, a,p) * wm_interm_2_so_pt1(a,q)
term(21) = term(21) + r1(vrdav_Rl, a,p) * wm_interm_3_so_pt1(a,q)
term(22) = term(22) + r1(vrdav_Rr, a,q) * wm_interm_6_so_pt1(a,p)
term(23) = term(23) + r1(vrdav_Rr, a,q) * wm_interm_7_so_pt1(a,p)
term(24) = term(24) + r1(vrdav_Rr, a,q) * wm_interm_8_so_pt1(a,p)
term(25) = term(25) + r1(vrdav_Rr, a,q) * wm_interm_9_so_pt1(a,p)
end do 

term(18) = term(18) * (8.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (-6.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (-3.9999999999999996d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (1.9999999999999998d+0) 
term(25) = term(25) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(26) = term(26) + s2(a,b,i,p) * wm_interm_4_so_pt1(a,b,i,q)
term(27) = term(27) + s2(a,b,i,p) * wm_interm_5_so_pt1(a,b,i,q)
end do 
end do 
end do 

term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (4.0d+0) 


    calc_D_oo_wm_so_cc3_pt1 = zero
    do s = 0, 27
    calc_D_oo_wm_so_cc3_pt1 = calc_D_oo_wm_so_cc3_pt1 + term(s)
    end do

    end function calc_D_oo_wm_so_cc3_pt1
    
    function calc_D_ov_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_cc3_pt1
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
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_18_so_pt1(a,p,j,i)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_19_so_pt1(a,p,j,i)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_20_so_pt1(a,p,j,i)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_21_so_pt1(a,p,j,i)
term(4) = term(4) + r2(vrdav_Rl, a,j,q,i) * wm_interm_22_so_pt1(a,i,j,p)
term(5) = term(5) + r2(vrdav_Rl, a,j,q,i) * wm_interm_22_so_pt1(a,j,i,p)
term(6) = term(6) + r2(vrdav_Rl, a,j,q,i) * wm_interm_23_so_pt1(a,i,j,p)
term(7) = term(7) + r2(vrdav_Rl, a,j,q,i) * wm_interm_24_so_pt1(a,j,i,p)
term(8) = term(8) + r2(vrdav_Rl, a,j,q,i) * wm_interm_25_so_pt1(a,i,j,p)
term(9) = term(9) + r2(vrdav_Rl, a,j,q,i) * wm_interm_23_so_pt1(a,j,i,p)
term(10) = term(10) + r2(vrdav_Rl, a,j,q,i) * wm_interm_24_so_pt1(a,i,j,p)
term(11) = term(11) + r2(vrdav_Rl, a,j,q,i) * wm_interm_25_so_pt1(a,j,i,p)
end do 
end do 
end do 

term(0) = term(0) * (6.0d+0) 
term(1) = term(1) * (-8.0d+0) 
term(2) = term(2) * (3.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (6.0d+0) 
term(5) = term(5) * (-8.0d+0) 
term(6) = term(6) * (3.0d+0) 
term(7) = term(7) * (3.0d+0) 
term(8) = term(8) * (-8.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + r2(vrdav_Rl, a,p,q,i) * wm_interm_0_so_pt1(a,i)
term(13) = term(13) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt1(a,i)
term(14) = term(14) + r2(vrdav_Rl, a,p,q,i) * wm_interm_2_so_pt1(a,i)
term(15) = term(15) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt1(a,i)
term(16) = term(16) + s2(a,q,p,i) * wm_interm_14_so_pt1(a,i)
term(17) = term(17) + s2(a,q,p,i) * wm_interm_15_so_pt1(a,i)
term(18) = term(18) + s2(a,q,p,i) * wm_interm_16_so_pt1(a,i)
term(19) = term(19) + s2(a,q,p,i) * wm_interm_17_so_pt1(a,i)
end do 
end do 

term(12) = term(12) * (8.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-6.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (6.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + s2(a,q,j,i) * wm_interm_20_so_pt1(a,p,i,j)
term(21) = term(21) + s2(a,q,j,i) * wm_interm_18_so_pt1(a,p,i,j)
term(22) = term(22) + s2(a,q,j,i) * wm_interm_19_so_pt1(a,p,i,j)
term(23) = term(23) + s2(a,q,j,i) * wm_interm_21_so_pt1(a,p,i,j)
end do 
end do 
end do 

term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (-8.0d+0) 
term(22) = term(22) * (8.0d+0) 
term(23) = term(23) * (3.0d+0) 


    calc_D_ov_wm_so_cc3_pt1 = zero
    do s = 0, 23
    calc_D_ov_wm_so_cc3_pt1 = calc_D_ov_wm_so_cc3_pt1 + term(s)
    end do

    end function calc_D_ov_wm_so_cc3_pt1
    
    function calc_D_vo_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, a, i, b 
    real(F64), dimension(0:84) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_26_so_pt1(a,j,i,q)
term(1) = term(1) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_26_so_pt1(a,i,j,q)
term(2) = term(2) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_27_so_pt1(a,i,j,q)
term(3) = term(3) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_27_so_pt1(a,j,i,q)
term(4) = term(4) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_28_so_pt1(a,j,i,q)
term(5) = term(5) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_28_so_pt1(a,i,j,q)
term(6) = term(6) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_29_so_pt1(a,i,j,q)
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_29_so_pt1(a,j,i,q)
term(8) = term(8) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_30_so_pt1(a,i,j,q)
term(9) = term(9) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_30_so_pt1(a,j,i,q)
term(10) = term(10) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_31_so_pt1(a,j,i,q)
term(11) = term(11) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_31_so_pt1(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-1.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(12) = term(12) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_6_so_pt1(a,i)
term(13) = term(13) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_7_so_pt1(a,i)
term(14) = term(14) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_8_so_pt1(a,i)
term(15) = term(15) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_9_so_pt1(a,i)
end do 
end do 

term(12) = term(12) * (-1.9999999999999998d+0) 
term(13) = term(13) * (2.0d+0) 
term(15) = term(15) * (-1.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + t2(a,p,j,i) * wm_interm_32_so_pt1(a,i,j,q)
term(17) = term(17) + t2(a,p,j,i) * wm_interm_33_so_pt1(a,i,j,q)
term(18) = term(18) + t2(a,p,j,i) * wm_interm_34_so_pt1(a,i,j,q)
term(19) = term(19) + t2(a,p,j,i) * wm_interm_35_so_pt1(a,j,i,q)
term(20) = term(20) + t2(a,p,j,i) * wm_interm_36_so_pt1(a,j,i,q)
term(21) = term(21) + t2(a,p,j,i) * wm_interm_37_so_pt1(a,j,i,q)
term(22) = term(22) + t2(a,p,j,i) * wm_interm_38_so_pt1(a,i,j,q)
term(23) = term(23) + t2(a,p,j,i) * wm_interm_35_so_pt1(a,i,j,q)
term(24) = term(24) + t2(a,p,j,i) * wm_interm_32_so_pt1(a,j,i,q)
term(25) = term(25) + t2(a,p,j,i) * wm_interm_33_so_pt1(a,j,i,q)
term(26) = term(26) + t2(a,p,j,i) * wm_interm_37_so_pt1(a,i,j,q)
term(27) = term(27) + t2(a,p,j,i) * wm_interm_34_so_pt1(a,j,i,q)
term(28) = term(28) + t2(a,p,j,i) * wm_interm_39_so_pt1(a,j,i,q)
term(29) = term(29) + t2(a,p,j,i) * wm_interm_40_so_pt1(a,j,i,q)
term(30) = term(30) + t2(a,p,j,i) * wm_interm_41_so_pt1(a,j,i,q)
term(31) = term(31) + t2(a,p,j,i) * wm_interm_39_so_pt1(a,i,j,q)
term(32) = term(32) + t2(a,p,j,i) * wm_interm_40_so_pt1(a,i,j,q)
term(33) = term(33) + t2(a,p,j,i) * wm_interm_41_so_pt1(a,i,j,q)
term(34) = term(34) + t2(a,p,j,i) * wm_interm_45_so_pt1(a,i,j,q)
term(35) = term(35) + t2(a,p,j,i) * wm_interm_46_so_pt1(a,i,j,q)
term(36) = term(36) + t2(a,p,j,i) * wm_interm_47_so_pt1(a,j,i,q)
term(37) = term(37) + t2(a,p,j,i) * wm_interm_48_so_pt1(a,j,i,q)
term(38) = term(38) + t2(a,p,j,i) * wm_interm_48_so_pt1(a,i,j,q)
term(39) = term(39) + t2(a,p,j,i) * wm_interm_47_so_pt1(a,i,j,q)
term(40) = term(40) + t2(a,p,j,i) * wm_interm_45_so_pt1(a,j,i,q)
term(41) = term(41) + t2(a,p,j,i) * wm_interm_46_so_pt1(a,j,i,q)
term(42) = term(42) + t2(a,p,j,i) * wm_interm_49_so_pt1(a,j,i,q)
term(43) = term(43) + t2(a,p,j,i) * wm_interm_50_so_pt1(a,j,i,q)
term(44) = term(44) + t2(a,p,j,i) * wm_interm_49_so_pt1(a,i,j,q)
term(45) = term(45) + t2(a,p,j,i) * wm_interm_50_so_pt1(a,i,j,q)
end do 
end do 
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (-1.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (0.5d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-1.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_27_so_pt1(a,j,i,q)
term(47) = term(47) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_26_so_pt1(a,i,j,q)
term(48) = term(48) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_29_so_pt1(a,j,i,q)
term(49) = term(49) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_28_so_pt1(a,i,j,q)
term(50) = term(50) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_31_so_pt1(a,i,j,q)
term(51) = term(51) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_30_so_pt1(a,j,i,q)
term(52) = term(52) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_26_so_pt1(a,i,j,q)
term(53) = term(53) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_26_so_pt1(a,j,i,q)
term(54) = term(54) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_27_so_pt1(a,j,i,q)
term(55) = term(55) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_27_so_pt1(a,i,j,q)
term(56) = term(56) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_28_so_pt1(a,i,j,q)
term(57) = term(57) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_28_so_pt1(a,j,i,q)
term(58) = term(58) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_29_so_pt1(a,j,i,q)
term(59) = term(59) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_29_so_pt1(a,i,j,q)
term(60) = term(60) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_30_so_pt1(a,j,i,q)
term(61) = term(61) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_30_so_pt1(a,i,j,q)
term(62) = term(62) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_31_so_pt1(a,i,j,q)
term(63) = term(63) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_31_so_pt1(a,j,i,q)
end do 
end do 
end do 

term(46) = term(46) * (0.5d+0) 
term(47) = term(47) * (0.5d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (0.5d+0) 
term(51) = term(51) * (0.5d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (2.0d+0) 
term(61) = term(61) * (-2.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(64) = term(64) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_6_so_pt1(a,i)
term(65) = term(65) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_7_so_pt1(a,i)
term(66) = term(66) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_6_so_pt1(a,i)
term(67) = term(67) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_7_so_pt1(a,i)
term(68) = term(68) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_8_so_pt1(a,i)
term(69) = term(69) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_9_so_pt1(a,i)
term(70) = term(70) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_8_so_pt1(a,i)
term(71) = term(71) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_9_so_pt1(a,i)
term(72) = term(72) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_6_so_pt1(a,i)
term(73) = term(73) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_6_so_pt1(a,i)
term(74) = term(74) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_7_so_pt1(a,i)
term(75) = term(75) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_7_so_pt1(a,i)
term(76) = term(76) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_8_so_pt1(a,i)
term(77) = term(77) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_8_so_pt1(a,i)
term(78) = term(78) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_9_so_pt1(a,i)
term(79) = term(79) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_9_so_pt1(a,i)
term(80) = term(80) + t2(a,p,q,i) * wm_interm_42_so_pt1(a,i)
term(81) = term(81) + t2(a,p,q,i) * wm_interm_43_so_pt1(a,i)
term(82) = term(82) + t2(a,p,q,i) * wm_interm_44_so_pt1(a,i)
term(83) = term(83) + t2(a,p,q,i) * wm_interm_51_so_pt1(a,i)
term(84) = term(84) + t2(a,p,q,i) * wm_interm_52_so_pt1(a,i)
end do 
end do 

term(64) = term(64) * (3.9999999999999996d+0) 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (-1.9999999999999998d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-1.9999999999999998d+0) 
term(69) = term(69) * (2.0d+0) 
term(71) = term(71) * (-1.0d+0) 
term(72) = term(72) * (-7.999999999999999d+0) 
term(73) = term(73) * (7.999999999999999d+0) 
term(74) = term(74) * (8.0d+0) 
term(75) = term(75) * (-8.0d+0) 
term(76) = term(76) * (3.9999999999999996d+0) 
term(77) = term(77) * (-3.9999999999999996d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(81) = term(81) * (-1.9999999999999998d+0) 
term(83) = term(83) * (3.9999999999999996d+0) 
term(84) = term(84) * (-3.9999999999999996d+0) 


    calc_D_vo_wm_so_cc3_pt1 = zero
    do s = 0, 84
    calc_D_vo_wm_so_cc3_pt1 = calc_D_vo_wm_so_cc3_pt1 + term(s)
    end do

    end function calc_D_vo_wm_so_cc3_pt1
    
    function calc_D_vv_wm_so_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_cc3_pt1
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
    real(F64), dimension(0:27) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_4_so_pt1(a,p,i,j)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_4_so_pt1(p,a,i,j)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_5_so_pt1(a,p,i,j)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_5_so_pt1(p,a,i,j)
term(4) = term(4) + t2(a,p,j,i) * wm_interm_11_so_pt1(a,q,i,j)
term(5) = term(5) + t2(a,p,j,i) * wm_interm_12_so_pt1(a,q,i,j)
term(6) = term(6) + t2(a,p,j,i) * wm_interm_10_so_pt1(q,a,i,j)
term(7) = term(7) + t2(a,p,j,i) * wm_interm_12_so_pt1(q,a,i,j)
term(8) = term(8) + t2(a,p,j,i) * wm_interm_13_so_pt1(a,q,i,j)
term(9) = term(9) + t2(a,p,j,i) * wm_interm_13_so_pt1(q,a,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-0.6666666666666666d+0) 
term(5) = term(5) * (-0.6666666666666666d+0) 
term(6) = term(6) * (-0.3333333333333333d+0) 
term(7) = term(7) * (1.3333333333333333d+0) 
term(8) = term(8) * (0.6666666666666666d+0) 
term(9) = term(9) * (-0.3333333333333333d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(10) = term(10) + s2(a,q,j,i) * wm_interm_5_so_pt1(p,a,j,i)
term(11) = term(11) + s2(a,q,j,i) * wm_interm_4_so_pt1(p,a,j,i)
term(12) = term(12) + s2(a,q,j,i) * wm_interm_4_so_pt1(a,p,j,i)
term(13) = term(13) + s2(a,q,j,i) * wm_interm_5_so_pt1(a,p,j,i)
term(14) = term(14) + t2(a,p,j,i) * wm_interm_10_so_pt1(a,q,j,i)
term(15) = term(15) + t2(a,p,j,i) * wm_interm_11_so_pt1(a,q,j,i)
term(16) = term(16) + t2(a,p,j,i) * wm_interm_11_so_pt1(q,a,j,i)
term(17) = term(17) + t2(a,p,j,i) * wm_interm_12_so_pt1(q,a,j,i)
term(18) = term(18) + t2(a,p,j,i) * wm_interm_13_so_pt1(a,q,j,i)
term(19) = term(19) + t2(a,p,j,i) * wm_interm_10_so_pt1(q,a,j,i)
end do 
end do 
end do 

term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-6.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-0.3333333333333333d+0) 
term(15) = term(15) * (1.3333333333333333d+0) 
term(16) = term(16) * (-0.6666666666666666d+0) 
term(17) = term(17) * (-0.6666666666666666d+0) 
term(18) = term(18) * (-0.3333333333333333d+0) 
term(19) = term(19) * (0.6666666666666666d+0) 

do i = 1, nocc 
term(20) = term(20) + r1(vrdav_Rl, q,i) * wm_interm_0_so_pt1(p,i)
term(21) = term(21) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt1(p,i)
term(22) = term(22) + r1(vrdav_Rl, q,i) * wm_interm_2_so_pt1(p,i)
term(23) = term(23) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt1(p,i)
term(24) = term(24) + r1(vrdav_Rr, p,i) * wm_interm_6_so_pt1(q,i)
term(25) = term(25) + r1(vrdav_Rr, p,i) * wm_interm_7_so_pt1(q,i)
term(26) = term(26) + r1(vrdav_Rr, p,i) * wm_interm_8_so_pt1(q,i)
term(27) = term(27) + r1(vrdav_Rr, p,i) * wm_interm_9_so_pt1(q,i)
end do 

term(20) = term(20) * (-8.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (6.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (3.9999999999999996d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (-1.9999999999999998d+0) 
term(27) = term(27) * (2.0d+0) 


    calc_D_vv_wm_so_cc3_pt1 = zero
    do s = 0, 27
    calc_D_vv_wm_so_cc3_pt1 = calc_D_vv_wm_so_cc3_pt1 + term(s)
    end do

    end function calc_D_vv_wm_so_cc3_pt1
    
    
    function calc_D_oo_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_cc3_pt2
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
    real(F64), dimension(0:70) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,b,i) * wm_interm_107_so_pt2(a,b,i,q)
term(1) = term(1) + r2(vrdav_Rl, a,p,b,i) * wm_interm_107_so_pt2(a,b,q,i)
term(2) = term(2) + r2(vrdav_Rl, a,p,b,i) * wm_interm_107_so_pt2(b,a,q,i)
term(3) = term(3) + r2(vrdav_Rl, a,p,b,i) * wm_interm_107_so_pt2(b,a,i,q)
term(4) = term(4) + r2(vrdav_Rl, a,p,b,i) * wm_interm_108_so_pt2(a,b,i,q)
term(5) = term(5) + r2(vrdav_Rl, a,p,b,i) * wm_interm_109_so_pt2(a,b,q,i)
term(6) = term(6) + r2(vrdav_Rl, a,p,b,i) * wm_interm_109_so_pt2(a,b,i,q)
term(7) = term(7) + r2(vrdav_Rl, a,p,b,i) * wm_interm_108_so_pt2(b,a,q,i)
term(8) = term(8) + r2(vrdav_Rl, a,p,b,i) * wm_interm_108_so_pt2(b,a,i,q)
term(9) = term(9) + r2(vrdav_Rl, a,p,b,i) * wm_interm_109_so_pt2(b,a,i,q)
term(10) = term(10) + r2(vrdav_Rl, a,p,b,i) * wm_interm_108_so_pt2(a,b,q,i)
term(11) = term(11) + r2(vrdav_Rl, a,p,b,i) * wm_interm_109_so_pt2(b,a,q,i)
term(12) = term(12) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_110_so_pt2(a,b,p,i)
term(13) = term(13) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_110_so_pt2(a,b,p,i)
term(14) = term(14) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_110_so_pt2(b,a,p,i)
term(15) = term(15) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_110_so_pt2(a,b,i,p)
term(16) = term(16) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_110_so_pt2(b,a,i,p)
term(17) = term(17) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_111_so_pt2(a,b,p,i)
term(18) = term(18) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_111_so_pt2(a,b,p,i)
term(19) = term(19) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_111_so_pt2(b,a,p,i)
term(20) = term(20) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_111_so_pt2(a,b,i,p)
term(21) = term(21) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_111_so_pt2(b,a,i,p)
term(22) = term(22) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_112_so_pt2(a,b,p,i)
term(23) = term(23) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_112_so_pt2(a,b,p,i)
term(24) = term(24) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_113_so_pt2(a,b,p,i)
term(25) = term(25) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_112_so_pt2(a,b,i,p)
term(26) = term(26) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_113_so_pt2(a,b,i,p)
term(27) = term(27) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_113_so_pt2(b,a,p,i)
term(28) = term(28) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_113_so_pt2(b,a,p,i)
term(29) = term(29) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_112_so_pt2(b,a,p,i)
term(30) = term(30) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_113_so_pt2(b,a,i,p)
term(31) = term(31) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_112_so_pt2(b,a,i,p)
term(32) = term(32) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_114_so_pt2(a,b,i,p)
term(33) = term(33) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_114_so_pt2(b,a,i,p)
term(34) = term(34) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_114_so_pt2(a,b,p,i)
term(35) = term(35) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_114_so_pt2(b,a,p,i)
term(36) = term(36) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_114_so_pt2(b,a,p,i)
term(37) = term(37) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_110_so_pt2(a,b,p,i)
term(38) = term(38) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_110_so_pt2(a,b,p,i)
term(39) = term(39) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_110_so_pt2(a,b,i,p)
term(40) = term(40) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_111_so_pt2(a,b,p,i)
term(41) = term(41) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_111_so_pt2(a,b,p,i)
term(42) = term(42) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_111_so_pt2(a,b,i,p)
term(43) = term(43) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_112_so_pt2(a,b,p,i)
term(44) = term(44) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_112_so_pt2(a,b,p,i)
term(45) = term(45) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_113_so_pt2(a,b,p,i)
term(46) = term(46) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_112_so_pt2(a,b,i,p)
term(47) = term(47) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_113_so_pt2(a,b,i,p)
term(48) = term(48) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_113_so_pt2(a,b,p,i)
term(49) = term(49) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_114_so_pt2(a,b,i,p)
term(50) = term(50) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_114_so_pt2(a,b,p,i)
term(51) = term(51) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_114_so_pt2(a,b,p,i)
end do 
end do 
end do 

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-3.0d+0) 
term(5) = term(5) * (-3.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-3.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-3.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (0.3333333333333333d+0) 
term(18) = term(18) * (-0.6666666666666666d+0) 
term(19) = term(19) * (0.3333333333333333d+0) 
term(20) = term(20) * (0.3333333333333333d+0) 
term(21) = term(21) * (-0.6666666666666666d+0) 
term(22) = term(22) * (0.3333333333333333d+0) 
term(23) = term(23) * (-0.6666666666666666d+0) 
term(24) = term(24) * (0.16666666666666666d+0) 
term(25) = term(25) * (0.3333333333333333d+0) 
term(26) = term(26) * (-0.3333333333333333d+0) 
term(27) = term(27) * (0.16666666666666666d+0) 
term(28) = term(28) * (-0.3333333333333333d+0) 
term(29) = term(29) * (0.3333333333333333d+0) 
term(30) = term(30) * (0.16666666666666666d+0) 
term(31) = term(31) * (-0.6666666666666666d+0) 
term(32) = term(32) * (-0.3333333333333333d+0) 
term(33) = term(33) * (0.16666666666666666d+0) 
term(34) = term(34) * (0.16666666666666666d+0) 
term(35) = term(35) * (0.16666666666666666d+0) 
term(36) = term(36) * (-0.3333333333333333d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (1.3333333333333333d+0) 
term(41) = term(41) * (-1.3333333333333333d+0) 
term(42) = term(42) * (1.3333333333333333d+0) 
term(43) = term(43) * (1.3333333333333333d+0) 
term(44) = term(44) * (-1.3333333333333333d+0) 
term(45) = term(45) * (0.6666666666666666d+0) 
term(46) = term(46) * (1.3333333333333333d+0) 
term(47) = term(47) * (-0.6666666666666666d+0) 
term(48) = term(48) * (-0.6666666666666666d+0) 
term(49) = term(49) * (-0.6666666666666666d+0) 
term(50) = term(50) * (0.6666666666666666d+0) 
term(51) = term(51) * (-0.6666666666666666d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(52) = term(52) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_110_so_pt2(b,a,i,p)
term(53) = term(53) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_111_so_pt2(b,a,i,p)
term(54) = term(54) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_113_so_pt2(a,b,i,p)
term(55) = term(55) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_112_so_pt2(b,a,i,p)
term(56) = term(56) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_114_so_pt2(a,b,i,p)
term(57) = term(57) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_110_so_pt2(a,b,i,p)
term(58) = term(58) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_111_so_pt2(a,b,i,p)
term(59) = term(59) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_113_so_pt2(a,b,i,p)
term(60) = term(60) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_112_so_pt2(a,b,i,p)
term(61) = term(61) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_114_so_pt2(a,b,i,p)
end do 
end do 
end do 

term(52) = term(52) * (-1.0d+0) 
term(53) = term(53) * (0.3333333333333333d+0) 
term(54) = term(54) * (0.16666666666666666d+0) 
term(55) = term(55) * (0.3333333333333333d+0) 
term(56) = term(56) * (0.16666666666666666d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-1.3333333333333333d+0) 
term(59) = term(59) * (0.6666666666666666d+0) 
term(60) = term(60) * (-1.3333333333333333d+0) 
term(61) = term(61) * (0.6666666666666666d+0) 

do a = nocc + 1, nactive 
term(62) = term(62) + s1(a,p) * wm_interm_32_so_pt2(a,q)
term(63) = term(63) + s1(a,p) * wm_interm_33_so_pt2(a,q)
term(64) = term(64) + s1(a,p) * wm_interm_36_so_pt2(a,q)
term(65) = term(65) + s1(a,p) * wm_interm_37_so_pt2(a,q)
term(66) = term(66) + t1(a,q) * wm_interm_44_so_pt2(a,p)
term(67) = term(67) + t1(a,q) * wm_interm_45_so_pt2(a,p)
term(68) = term(68) + t1(a,q) * wm_interm_43_so_pt2(a,p)
term(69) = term(69) + t1(a,q) * wm_interm_84_so_pt2(a,p)
term(70) = term(70) + t1(a,q) * wm_interm_85_so_pt2(a,p)
end do 

term(62) = term(62) * (6.0d+0) 
term(63) = term(63) * (-8.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(67) = term(67) * (-1.9999999999999998d+0) 
term(69) = term(69) * (3.9999999999999996d+0) 
term(70) = term(70) * (-3.9999999999999996d+0) 


    calc_D_oo_wm_so_cc3_pt2 = zero
    do s = 0, 70
    calc_D_oo_wm_so_cc3_pt2 = calc_D_oo_wm_so_cc3_pt2 + term(s)
    end do

    end function calc_D_oo_wm_so_cc3_pt2
    
    function calc_D_ov_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, k, j, c, b, a, l 
    real(F64), dimension(0:999) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_49_so_pt2(a,c,i,k)
term(1) = term(1) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_50_so_pt2(a,c,i,k)
term(2) = term(2) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_51_so_pt2(a,c,i,k)
term(3) = term(3) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_52_so_pt2(a,c,i,k)
term(4) = term(4) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_53_so_pt2(a,c,i,k)
term(5) = term(5) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_54_so_pt2(a,c,i,k)
term(6) = term(6) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_34_so_pt2(c,b,p,k)
term(7) = term(7) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_35_so_pt2(c,b,p,k)
term(8) = term(8) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_49_so_pt2(b,c,i,k)
term(9) = term(9) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_50_so_pt2(b,c,i,k)
term(10) = term(10) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_51_so_pt2(b,c,i,k)
term(11) = term(11) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_52_so_pt2(b,c,i,k)
term(12) = term(12) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_34_so_pt2(c,b,p,j)
term(13) = term(13) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_35_so_pt2(c,b,p,j)
term(14) = term(14) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_53_so_pt2(b,c,i,k)
term(15) = term(15) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_54_so_pt2(b,c,i,k)
term(16) = term(16) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_49_so_pt2(a,c,j,k)
term(17) = term(17) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_50_so_pt2(a,c,j,k)
term(18) = term(18) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_51_so_pt2(a,c,j,k)
term(19) = term(19) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_52_so_pt2(a,c,j,k)
term(20) = term(20) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_53_so_pt2(a,c,j,k)
term(21) = term(21) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_54_so_pt2(a,c,j,k)
term(22) = term(22) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_34_so_pt2(c,b,p,i)
term(23) = term(23) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_35_so_pt2(c,b,p,i)
term(24) = term(24) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_34_so_pt2(c,a,p,k)
term(25) = term(25) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_35_so_pt2(c,a,p,k)
term(26) = term(26) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_34_so_pt2(c,a,p,j)
term(27) = term(27) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_35_so_pt2(c,a,p,j)
term(28) = term(28) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_51_so_pt2(b,c,i,k)
term(29) = term(29) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_52_so_pt2(b,c,i,k)
term(30) = term(30) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_54_so_pt2(b,c,i,k)
term(31) = term(31) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_38_so_pt2(c,a,p,k)
term(32) = term(32) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_39_so_pt2(c,a,p,k)
term(33) = term(33) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_50_so_pt2(b,c,i,k)
term(34) = term(34) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_59_so_pt2(b,c,i,k)
term(35) = term(35) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_53_so_pt2(b,c,i,k)
term(36) = term(36) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_51_so_pt2(a,c,j,k)
term(37) = term(37) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_52_so_pt2(a,c,j,k)
term(38) = term(38) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_50_so_pt2(a,c,j,k)
term(39) = term(39) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_49_so_pt2(a,c,j,k)
term(40) = term(40) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_54_so_pt2(a,c,j,k)
term(41) = term(41) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_59_so_pt2(a,c,j,k)
term(42) = term(42) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,i) * wm_interm_49_so_pt2(b,c,j,k)
term(43) = term(43) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,i) * wm_interm_53_so_pt2(b,c,j,k)
term(44) = term(44) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,i) * wm_interm_59_so_pt2(b,c,j,k)
term(45) = term(45) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_49_so_pt2(a,c,i,k)
term(46) = term(46) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_50_so_pt2(a,c,i,k)
term(47) = term(47) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_51_so_pt2(a,c,i,k)
term(48) = term(48) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_52_so_pt2(a,c,i,k)
term(49) = term(49) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_53_so_pt2(a,c,i,k)
term(50) = term(50) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_54_so_pt2(a,c,i,k)
term(51) = term(51) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_39_so_pt2(c,b,p,k)
term(52) = term(52) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_38_so_pt2(c,b,p,k)
term(53) = term(53) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_38_so_pt2(c,b,p,i)
term(54) = term(54) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_38_so_pt2(c,b,p,j)
term(55) = term(55) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,i) * wm_interm_39_so_pt2(c,b,p,j)
term(56) = term(56) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_39_so_pt2(c,b,p,j)
term(57) = term(57) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_51_so_pt2(a,c,i,p)
term(58) = term(58) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_52_so_pt2(a,c,i,p)
term(59) = term(59) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_54_so_pt2(a,c,i,p)
term(60) = term(60) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_50_so_pt2(a,c,i,p)
term(61) = term(61) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_59_so_pt2(a,c,i,p)
term(62) = term(62) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_53_so_pt2(a,c,i,p)
term(63) = term(63) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_49_so_pt2(b,c,k,p)
term(64) = term(64) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_53_so_pt2(b,c,k,p)
term(65) = term(65) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_59_so_pt2(b,c,k,p)
term(66) = term(66) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_51_so_pt2(a,c,k,p)
term(67) = term(67) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_52_so_pt2(a,c,k,p)
term(68) = term(68) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_54_so_pt2(a,c,k,p)
term(69) = term(69) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_50_so_pt2(a,c,k,p)
term(70) = term(70) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_59_so_pt2(a,c,k,p)
term(71) = term(71) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_53_so_pt2(a,c,k,p)
term(72) = term(72) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_51_so_pt2(b,c,i,p)
term(73) = term(73) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_52_so_pt2(b,c,i,p)
term(74) = term(74) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_54_so_pt2(b,c,i,p)
term(75) = term(75) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_50_so_pt2(b,c,i,p)
term(76) = term(76) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_59_so_pt2(b,c,i,p)
term(77) = term(77) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_53_so_pt2(b,c,i,p)
term(78) = term(78) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_39_so_pt2(c,a,p,j)
term(79) = term(79) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_38_so_pt2(c,a,p,j)
term(80) = term(80) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_88_so_pt2(a,c,i,k)
term(81) = term(81) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_89_so_pt2(a,c,i,k)
term(82) = term(82) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_90_so_pt2(a,c,i,k)
term(83) = term(83) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,k) * wm_interm_91_so_pt2(a,c,i,k)
term(84) = term(84) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_34_so_pt2(c,b,p,k)
term(85) = term(85) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_35_so_pt2(c,b,p,k)
term(86) = term(86) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_88_so_pt2(b,c,i,k)
term(87) = term(87) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_89_so_pt2(b,c,i,k)
term(88) = term(88) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_90_so_pt2(b,c,i,k)
term(89) = term(89) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,k) * wm_interm_91_so_pt2(b,c,i,k)
term(90) = term(90) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_34_so_pt2(c,b,p,j)
term(91) = term(91) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_35_so_pt2(c,b,p,j)
term(92) = term(92) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_34_so_pt2(c,b,p,j)
term(93) = term(93) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_35_so_pt2(c,b,p,j)
term(94) = term(94) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_88_so_pt2(a,c,j,k)
term(95) = term(95) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_89_so_pt2(a,c,j,k)
term(96) = term(96) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_90_so_pt2(a,c,j,k)
term(97) = term(97) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,k) * wm_interm_91_so_pt2(a,c,j,k)
term(98) = term(98) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_34_so_pt2(c,b,p,i)
term(99) = term(99) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_35_so_pt2(c,b,p,i)
term(100) = term(100) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_34_so_pt2(c,a,p,k)
term(101) = term(101) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_35_so_pt2(c,a,p,k)
term(102) = term(102) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_34_so_pt2(c,a,p,j)
term(103) = term(103) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_35_so_pt2(c,a,p,j)
term(104) = term(104) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_34_so_pt2(c,a,p,j)
term(105) = term(105) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_35_so_pt2(c,a,p,j)
term(106) = term(106) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_34_so_pt2(c,a,p,i)
term(107) = term(107) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_35_so_pt2(c,a,p,i)
term(108) = term(108) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_90_so_pt2(b,c,i,k)
term(109) = term(109) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_91_so_pt2(b,c,i,k)
term(110) = term(110) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_38_so_pt2(c,a,p,k)
term(111) = term(111) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_39_so_pt2(c,a,p,k)
term(112) = term(112) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_89_so_pt2(b,c,i,k)
term(113) = term(113) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,j) * wm_interm_88_so_pt2(b,c,i,k)
term(114) = term(114) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_90_so_pt2(a,c,j,k)
term(115) = term(115) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_91_so_pt2(a,c,j,k)
term(116) = term(116) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_89_so_pt2(a,c,j,k)
term(117) = term(117) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,i) * wm_interm_88_so_pt2(a,c,j,k)
term(118) = term(118) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,i) * wm_interm_88_so_pt2(b,c,j,k)
term(119) = term(119) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_38_so_pt2(c,a,p,i)
term(120) = term(120) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,k,i) * wm_interm_89_so_pt2(b,c,j,k)
term(121) = term(121) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_88_so_pt2(a,c,i,k)
term(122) = term(122) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_89_so_pt2(a,c,i,k)
term(123) = term(123) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_90_so_pt2(a,c,i,k)
term(124) = term(124) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,k,j) * wm_interm_91_so_pt2(a,c,i,k)
term(125) = term(125) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_39_so_pt2(c,b,p,k)
term(126) = term(126) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_38_so_pt2(c,b,p,k)
term(127) = term(127) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_38_so_pt2(c,b,p,i)
term(128) = term(128) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_38_so_pt2(c,b,p,j)
term(129) = term(129) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_39_so_pt2(c,b,p,j)
term(130) = term(130) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_39_so_pt2(c,b,p,j)
term(131) = term(131) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_38_so_pt2(c,b,p,j)
term(132) = term(132) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_90_so_pt2(a,c,i,p)
term(133) = term(133) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_91_so_pt2(a,c,i,p)
term(134) = term(134) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_89_so_pt2(a,c,i,p)
term(135) = term(135) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_88_so_pt2(a,c,i,p)
term(136) = term(136) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_88_so_pt2(b,c,k,p)
term(137) = term(137) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_89_so_pt2(b,c,k,p)
term(138) = term(138) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_90_so_pt2(a,c,k,p)
term(139) = term(139) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_91_so_pt2(a,c,k,p)
term(140) = term(140) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_89_so_pt2(a,c,k,p)
term(141) = term(141) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_88_so_pt2(a,c,k,p)
term(142) = term(142) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_90_so_pt2(b,c,i,p)
term(143) = term(143) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_91_so_pt2(b,c,i,p)
term(144) = term(144) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_89_so_pt2(b,c,i,p)
term(145) = term(145) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_88_so_pt2(b,c,i,p)
term(146) = term(146) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_39_so_pt2(c,a,p,j)
term(147) = term(147) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_38_so_pt2(c,a,p,j)
term(148) = term(148) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_39_so_pt2(c,a,p,j)
term(149) = term(149) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_38_so_pt2(c,a,p,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * (3.9999999999999996d+0) 
term(2) = term(2) * (3.9999999999999996d+0) 
term(3) = term(3) * (-7.999999999999999d+0) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (3.9999999999999996d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(9) = term(9) * (-1.9999999999999998d+0) 
term(10) = term(10) * (-1.9999999999999998d+0) 
term(11) = term(11) * (3.9999999999999996d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-1.0d+0) 
term(15) = term(15) * (-1.9999999999999998d+0) 
term(17) = term(17) * (-1.9999999999999998d+0) 
term(18) = term(18) * (-1.9999999999999998d+0) 
term(19) = term(19) * (3.9999999999999996d+0) 
term(21) = term(21) * (-1.9999999999999998d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (-1.0d+0) 
term(24) = term(24) * (0.5d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(29) = term(29) * (-1.9999999999999998d+0) 
term(31) = term(31) * (0.5d+0) 
term(32) = term(32) * (-1.0d+0) 
term(35) = term(35) * (-1.9999999999999998d+0) 
term(37) = term(37) * (-1.9999999999999998d+0) 
term(39) = term(39) * (-1.9999999999999998d+0) 
term(44) = term(44) * (-1.9999999999999998d+0) 
term(46) = term(46) * (-1.9999999999999998d+0) 
term(47) = term(47) * (-1.9999999999999998d+0) 
term(48) = term(48) * (3.9999999999999996d+0) 
term(50) = term(50) * (-1.9999999999999998d+0) 
term(51) = term(51) * (0.5d+0) 
term(52) = term(52) * (-1.0d+0) 
term(53) = term(53) * (0.5d+0) 
term(54) = term(54) * (0.5d+0) 
term(55) = term(55) * (0.5d+0) 
term(56) = term(56) * (-1.0d+0) 
term(57) = term(57) * (-1.0d+0) 
term(58) = term(58) * (2.0d+0) 
term(59) = term(59) * (-1.0d+0) 
term(60) = term(60) * (-1.0d+0) 
term(61) = term(61) * (-1.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (0.5d+0) 
term(64) = term(64) * (0.5d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (0.5d+0) 
term(67) = term(67) * (-1.0d+0) 
term(68) = term(68) * (0.5d+0) 
term(69) = term(69) * (0.5d+0) 
term(70) = term(70) * (0.5d+0) 
term(71) = term(71) * (-1.0d+0) 
term(72) = term(72) * (0.5d+0) 
term(73) = term(73) * (-1.0d+0) 
term(74) = term(74) * (0.5d+0) 
term(75) = term(75) * (0.5d+0) 
term(76) = term(76) * (0.5d+0) 
term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (0.5d+0) 
term(79) = term(79) * (-1.0d+0) 
term(80) = term(80) * (-7.999999999999999d+0) 
term(81) = term(81) * (7.999999999999999d+0) 
term(82) = term(82) * (15.999999999999998d+0) 
term(83) = term(83) * (-15.999999999999998d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (8.0d+0) 
term(86) = term(86) * (3.9999999999999996d+0) 
term(87) = term(87) * (-3.9999999999999996d+0) 
term(88) = term(88) * (-7.999999999999999d+0) 
term(89) = term(89) * (7.999999999999999d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (4.0d+0) 
term(94) = term(94) * (3.9999999999999996d+0) 
term(95) = term(95) * (-3.9999999999999996d+0) 
term(96) = term(96) * (-7.999999999999999d+0) 
term(97) = term(97) * (7.999999999999999d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * (8.0d+0) 
term(104) = term(104) * (4.0d+0) 
term(105) = term(105) * (-8.0d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (4.0d+0) 
term(108) = term(108) * (3.9999999999999996d+0) 
term(109) = term(109) * (-3.9999999999999996d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-2.0d+0) 
term(112) = term(112) * (3.9999999999999996d+0) 
term(113) = term(113) * (-3.9999999999999996d+0) 
term(114) = term(114) * (3.9999999999999996d+0) 
term(115) = term(115) * (-3.9999999999999996d+0) 
term(116) = term(116) * (3.9999999999999996d+0) 
term(117) = term(117) * (-3.9999999999999996d+0) 
term(118) = term(118) * (3.9999999999999996d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * (-3.9999999999999996d+0) 
term(121) = term(121) * (3.9999999999999996d+0) 
term(122) = term(122) * (-3.9999999999999996d+0) 
term(123) = term(123) * (-7.999999999999999d+0) 
term(124) = term(124) * (7.999999999999999d+0) 
term(125) = term(125) * (2.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (2.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (2.0d+0) 
term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (4.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (2.0d+0) 
term(137) = term(137) * (-2.0d+0) 
term(138) = term(138) * (2.0d+0) 
term(139) = term(139) * (-2.0d+0) 
term(140) = term(140) * (2.0d+0) 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-2.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (-2.0d+0) 
term(146) = term(146) * (2.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(149) = term(149) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(150) = term(150) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_34_so_pt2(c,b,p,k)
term(151) = term(151) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_35_so_pt2(c,b,p,k)
term(152) = term(152) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_34_so_pt2(c,b,p,i)
term(153) = term(153) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_35_so_pt2(c,b,p,i)
term(154) = term(154) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_34_so_pt2(c,a,p,k)
term(155) = term(155) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_35_so_pt2(c,a,p,k)
term(156) = term(156) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_34_so_pt2(c,a,p,i)
term(157) = term(157) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_35_so_pt2(c,a,p,i)
term(158) = term(158) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_38_so_pt2(c,a,p,k)
term(159) = term(159) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_38_so_pt2(c,a,p,i)
term(160) = term(160) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_39_so_pt2(c,b,p,k)
term(161) = term(161) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_38_so_pt2(c,b,p,k)
term(162) = term(162) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_38_so_pt2(c,b,p,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(150) = term(150) * (-1.0d+0) 
term(151) = term(151) * (2.0d+0) 
term(152) = term(152) * (0.5d+0) 
term(153) = term(153) * (-1.0d+0) 
term(154) = term(154) * (0.5d+0) 
term(155) = term(155) * (-1.0d+0) 
term(156) = term(156) * (-1.0d+0) 
term(157) = term(157) * (2.0d+0) 
term(158) = term(158) * (0.5d+0) 
term(159) = term(159) * (-1.0d+0) 
term(160) = term(160) * (0.5d+0) 
term(161) = term(161) * (-1.0d+0) 
term(162) = term(162) * (0.5d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(163) = term(163) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_5_so_pt2(c,b)
term(164) = term(164) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_5_so_pt2(c,b)
term(165) = term(165) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_7_so_pt2(c,b)
term(166) = term(166) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_7_so_pt2(c,b)
term(167) = term(167) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_5_so_pt2(c,a)
term(168) = term(168) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_7_so_pt2(c,a)
term(169) = term(169) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,i) * wm_interm_60_so_pt2(b,c)
term(170) = term(170) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,i) * wm_interm_61_so_pt2(b,c)
term(171) = term(171) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,i) * wm_interm_64_so_pt2(b,c)
term(172) = term(172) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,i) * wm_interm_60_so_pt2(a,c)
term(173) = term(173) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,i) * wm_interm_61_so_pt2(a,c)
term(174) = term(174) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,i) * wm_interm_64_so_pt2(a,c)
term(175) = term(175) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,j) * wm_interm_60_so_pt2(a,c)
term(176) = term(176) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,j) * wm_interm_61_so_pt2(a,c)
term(177) = term(177) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,j) * wm_interm_64_so_pt2(a,c)
term(178) = term(178) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_5_so_pt2(c,b)
term(179) = term(179) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_5_so_pt2(c,b)
term(180) = term(180) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_7_so_pt2(c,b)
term(181) = term(181) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_7_so_pt2(c,b)
term(182) = term(182) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_5_so_pt2(c,a)
term(183) = term(183) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_7_so_pt2(c,a)
term(184) = term(184) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_5_so_pt2(c,a)
term(185) = term(185) + r3(vrdav_Rl, a,p,b,j,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_7_so_pt2(c,a)
term(186) = term(186) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,i) * wm_interm_94_so_pt2(b,c)
term(187) = term(187) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(a,c,j,i) * wm_interm_95_so_pt2(b,c)
term(188) = term(188) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,i) * wm_interm_94_so_pt2(a,c)
term(189) = term(189) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,j,i) * wm_interm_95_so_pt2(a,c)
term(190) = term(190) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,j) * wm_interm_94_so_pt2(a,c)
term(191) = term(191) + r3(vrdav_Rl, a,p,b,j,q,i) * t2(b,c,i,j) * wm_interm_95_so_pt2(a,c)
end do 
end do 
end do 
end do 
end do 

term(164) = term(164) * (-1.9999999999999998d+0) 
term(165) = term(165) * (-1.9999999999999998d+0) 
term(166) = term(166) * (3.9999999999999996d+0) 
term(168) = term(168) * (-1.9999999999999998d+0) 
term(170) = term(170) * (-1.9999999999999998d+0) 
term(172) = term(172) * (-1.9999999999999998d+0) 
term(173) = term(173) * (3.9999999999999996d+0) 
term(174) = term(174) * (-1.9999999999999998d+0) 
term(176) = term(176) * (-1.9999999999999998d+0) 
term(178) = term(178) * (3.9999999999999996d+0) 
term(179) = term(179) * (-3.9999999999999996d+0) 
term(180) = term(180) * (-7.999999999999999d+0) 
term(181) = term(181) * (7.999999999999999d+0) 
term(182) = term(182) * (3.9999999999999996d+0) 
term(183) = term(183) * (-7.999999999999999d+0) 
term(184) = term(184) * (-3.9999999999999996d+0) 
term(185) = term(185) * (7.999999999999999d+0) 
term(186) = term(186) * (3.9999999999999996d+0) 
term(187) = term(187) * (-3.9999999999999996d+0) 
term(188) = term(188) * (-7.999999999999999d+0) 
term(189) = term(189) * (7.999999999999999d+0) 
term(190) = term(190) * (3.9999999999999996d+0) 
term(191) = term(191) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(192) = term(192) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_34_so_pt2(c,b,p,k)
term(193) = term(193) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_35_so_pt2(c,b,p,k)
term(194) = term(194) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_34_so_pt2(c,b,p,j)
term(195) = term(195) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_35_so_pt2(c,b,p,j)
term(196) = term(196) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_34_so_pt2(c,b,p,j)
term(197) = term(197) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_35_so_pt2(c,b,p,j)
term(198) = term(198) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_34_so_pt2(c,b,p,i)
term(199) = term(199) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_35_so_pt2(c,b,p,i)
term(200) = term(200) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_34_so_pt2(c,a,p,k)
term(201) = term(201) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_35_so_pt2(c,a,p,k)
term(202) = term(202) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_34_so_pt2(c,a,p,j)
term(203) = term(203) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_35_so_pt2(c,a,p,j)
term(204) = term(204) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_34_so_pt2(c,a,p,j)
term(205) = term(205) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_35_so_pt2(c,a,p,j)
term(206) = term(206) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_34_so_pt2(c,a,p,i)
term(207) = term(207) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_35_so_pt2(c,a,p,i)
term(208) = term(208) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_38_so_pt2(c,a,p,k)
term(209) = term(209) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_39_so_pt2(c,a,p,k)
term(210) = term(210) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_38_so_pt2(c,a,p,i)
term(211) = term(211) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_39_so_pt2(c,b,p,k)
term(212) = term(212) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_38_so_pt2(c,b,p,k)
term(213) = term(213) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_38_so_pt2(c,b,p,i)
term(214) = term(214) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_39_so_pt2(c,b,p,j)
term(215) = term(215) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_38_so_pt2(c,b,p,j)
term(216) = term(216) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_38_so_pt2(c,b,p,j)
term(217) = term(217) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_39_so_pt2(c,a,p,j)
term(218) = term(218) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_38_so_pt2(c,a,p,j)
term(219) = term(219) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_39_so_pt2(c,a,p,j)
term(220) = term(220) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_38_so_pt2(c,a,p,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(192) = term(192) * (2.0d+0) 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * (0.5d+0) 
term(195) = term(195) * (-1.0d+0) 
term(196) = term(196) * (-1.0d+0) 
term(197) = term(197) * (2.0d+0) 
term(198) = term(198) * (-1.0d+0) 
term(199) = term(199) * (2.0d+0) 
term(200) = term(200) * (-1.0d+0) 
term(201) = term(201) * (2.0d+0) 
term(202) = term(202) * (-1.0d+0) 
term(203) = term(203) * (2.0d+0) 
term(204) = term(204) * (2.0d+0) 
term(205) = term(205) * (-4.0d+0) 
term(206) = term(206) * (0.5d+0) 
term(207) = term(207) * (-1.0d+0) 
term(208) = term(208) * (-1.0d+0) 
term(209) = term(209) * (0.5d+0) 
term(210) = term(210) * (0.5d+0) 
term(211) = term(211) * (-1.0d+0) 
term(212) = term(212) * (2.0d+0) 
term(213) = term(213) * (-1.0d+0) 
term(214) = term(214) * (0.5d+0) 
term(215) = term(215) * (0.5d+0) 
term(216) = term(216) * (-1.0d+0) 
term(217) = term(217) * (0.5d+0) 
term(218) = term(218) * (-1.0d+0) 
term(219) = term(219) * (-1.0d+0) 
term(220) = term(220) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(221) = term(221) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_56_so_pt2(q,j,k,i)
term(222) = term(222) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_57_so_pt2(q,j,k,i)
term(223) = term(223) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_58_so_pt2(q,j,k,i)
term(224) = term(224) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_41_so_pt2(q,j,k,i)
term(225) = term(225) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_40_so_pt2(q,j,k,i)
term(226) = term(226) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_41_so_pt2(q,j,k,i)
term(227) = term(227) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_46_so_pt2(q,j,k,i)
term(228) = term(228) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_47_so_pt2(q,j,k,i)
term(229) = term(229) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_55_so_pt2(q,j,k,i)
term(230) = term(230) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_42_so_pt2(q,j,k,i)
term(231) = term(231) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_42_so_pt2(q,j,k,i)
term(232) = term(232) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_48_so_pt2(q,j,k,i)
term(233) = term(233) + wm_interm_73_so_pt2(i,j,k,p) * wm_interm_74_so_pt2(q,j,i,k)
term(234) = term(234) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_74_so_pt2(q,j,i,k)
term(235) = term(235) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_76_so_pt2(q,j,i,k)
term(236) = term(236) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_77_so_pt2(q,j,i,k)
term(237) = term(237) + wm_interm_73_so_pt2(i,j,k,p) * wm_interm_79_so_pt2(q,j,i,k)
term(238) = term(238) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_79_so_pt2(q,j,i,k)
term(239) = term(239) + wm_interm_73_so_pt2(i,j,k,p) * wm_interm_80_so_pt2(q,j,i,k)
term(240) = term(240) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_80_so_pt2(q,j,i,k)
term(241) = term(241) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_92_so_pt2(q,j,k,i)
term(242) = term(242) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_93_so_pt2(q,j,k,i)
term(243) = term(243) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_83_so_pt2(q,j,k,i)
term(244) = term(244) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_82_so_pt2(q,j,k,i)
term(245) = term(245) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_83_so_pt2(q,j,k,i)
term(246) = term(246) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_86_so_pt2(q,j,k,i)
term(247) = term(247) + wm_interm_14_so_pt2(p,i,j,k) * wm_interm_87_so_pt2(q,j,k,i)
term(248) = term(248) + wm_interm_14_so_pt2(i,p,j,k) * wm_interm_82_so_pt2(q,j,k,i)
term(249) = term(249) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_69_so_pt2(q,j,i,k)
term(250) = term(250) + wm_interm_102_so_pt2(i,j,k,p) * wm_interm_74_so_pt2(q,j,i,k)
term(251) = term(251) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_74_so_pt2(q,j,i,k)
term(252) = term(252) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_76_so_pt2(q,j,i,k)
term(253) = term(253) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_77_so_pt2(q,j,i,k)
term(254) = term(254) + wm_interm_102_so_pt2(i,j,k,p) * wm_interm_79_so_pt2(q,j,i,k)
term(255) = term(255) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_79_so_pt2(q,j,i,k)
term(256) = term(256) + wm_interm_102_so_pt2(i,j,k,p) * wm_interm_80_so_pt2(q,j,i,k)
term(257) = term(257) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_80_so_pt2(q,j,i,k)
end do 
end do 
end do 

term(221) = term(221) * (0.5d+0) 
term(222) = term(222) * (0.5d+0) 
term(223) = term(223) * (-1.0d+0) 
term(224) = term(224) * (0.5d+0) 
term(225) = term(225) * (0.5d+0) 
term(226) = term(226) * (-1.0d+0) 
term(227) = term(227) * (0.5d+0) 
term(228) = term(228) * (-1.0d+0) 
term(229) = term(229) * (0.5d+0) 
term(230) = term(230) * (-1.0d+0) 
term(231) = term(231) * (0.5d+0) 
term(232) = term(232) * (0.5d+0) 
term(233) = term(233) * (0.5d+0) 
term(234) = term(234) * (-1.0d+0) 
term(235) = term(235) * (-1.0d+0) 
term(236) = term(236) * (0.5d+0) 
term(237) = term(237) * (-1.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (0.5d+0) 
term(240) = term(240) * (-1.0d+0) 
term(241) = term(241) * (2.0d+0) 
term(242) = term(242) * (-2.0d+0) 
term(243) = term(243) * (2.0d+0) 
term(244) = term(244) * (2.0d+0) 
term(245) = term(245) * (-2.0d+0) 
term(246) = term(246) * (2.0d+0) 
term(247) = term(247) * (-2.0d+0) 
term(248) = term(248) * (-2.0d+0) 
term(251) = term(251) * (-2.0d+0) 
term(252) = term(252) * (-2.0d+0) 
term(254) = term(254) * (-2.0d+0) 
term(255) = term(255) * (4.0d+0) 
term(257) = term(257) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(258) = term(258) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_5_so_pt2(c,b)
term(259) = term(259) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_7_so_pt2(c,b)
term(260) = term(260) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_5_so_pt2(c,a)
term(261) = term(261) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_7_so_pt2(c,a)
term(262) = term(262) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_5_so_pt2(c,a)
term(263) = term(263) + r3(vrdav_Rl, a,p,b,j,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_7_so_pt2(c,a)
end do 
end do 
end do 
end do 
end do 

term(259) = term(259) * (-1.9999999999999998d+0) 
term(261) = term(261) * (-1.9999999999999998d+0) 
term(262) = term(262) * (-1.9999999999999998d+0) 
term(263) = term(263) * (3.9999999999999996d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(264) = term(264) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,i,p,j,l,k)
term(265) = term(265) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_75_so_pt2(b,p,i,j,l,k)
term(266) = term(266) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,i,j,l,k)
term(267) = term(267) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_78_so_pt2(b,i,p,j,l,k)
term(268) = term(268) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,i,j,p,l,k)
term(269) = term(269) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,j,i,p,l,k)
term(270) = term(270) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_81_so_pt2(b,j,i,p,l,k)
term(271) = term(271) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,p,i,j,l,k)
term(272) = term(272) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_81_so_pt2(b,p,i,j,l,k)
term(273) = term(273) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,i,p,j,l,k)
term(274) = term(274) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,p,i,j,l,k)
term(275) = term(275) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,i,j,l,k)
term(276) = term(276) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,i,p,j,l,k)
term(277) = term(277) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,i,j,p,l,k)
term(278) = term(278) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,j,i,p,l,k)
term(279) = term(279) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,p,i,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(264) = term(264) * (0.16666666666666666d+0) 
term(265) = term(265) * (0.16666666666666666d+0) 
term(266) = term(266) * (0.16666666666666666d+0) 
term(267) = term(267) * (0.16666666666666666d+0) 
term(268) = term(268) * (0.16666666666666666d+0) 
term(269) = term(269) * (-0.3333333333333333d+0) 
term(270) = term(270) * (0.16666666666666666d+0) 
term(271) = term(271) * (0.16666666666666666d+0) 
term(272) = term(272) * (-0.3333333333333333d+0) 
term(273) = term(273) * (0.6666666666666666d+0) 
term(274) = term(274) * (-0.6666666666666666d+0) 
term(275) = term(275) * (0.6666666666666666d+0) 
term(276) = term(276) * (-0.6666666666666666d+0) 
term(277) = term(277) * (0.6666666666666666d+0) 
term(278) = term(278) * (-0.6666666666666666d+0) 
term(279) = term(279) * (0.6666666666666666d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(280) = term(280) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,j,p,i,l,k)
term(281) = term(281) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_75_so_pt2(b,j,p,i,l,k)
term(282) = term(282) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,p,j,i,l,k)
term(283) = term(283) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_75_so_pt2(b,p,j,i,l,k)
term(284) = term(284) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,j,i,l,k)
term(285) = term(285) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_78_so_pt2(b,p,j,i,l,k)
term(286) = term(286) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,j,p,i,l,k)
term(287) = term(287) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_78_so_pt2(b,j,p,i,l,k)
term(288) = term(288) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_81_so_pt2(b,p,j,i,l,k)
term(289) = term(289) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,j,p,i,l,k)
term(290) = term(290) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,p,j,i,l,k)
term(291) = term(291) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,j,i,l,k)
term(292) = term(292) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,j,p,i,l,k)
term(293) = term(293) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,p,j,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(280) = term(280) * (-0.3333333333333333d+0) 
term(281) = term(281) * (0.16666666666666666d+0) 
term(282) = term(282) * (0.16666666666666666d+0) 
term(283) = term(283) * (-0.3333333333333333d+0) 
term(284) = term(284) * (-0.3333333333333333d+0) 
term(285) = term(285) * (0.16666666666666666d+0) 
term(286) = term(286) * (0.16666666666666666d+0) 
term(287) = term(287) * (-0.3333333333333333d+0) 
term(288) = term(288) * (0.16666666666666666d+0) 
term(289) = term(289) * (-0.6666666666666666d+0) 
term(290) = term(290) * (0.6666666666666666d+0) 
term(291) = term(291) * (-0.6666666666666666d+0) 
term(292) = term(292) * (0.6666666666666666d+0) 
term(293) = term(293) * (-0.6666666666666666d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(294) = term(294) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,j,p,i,k,l)
term(295) = term(295) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,j,i,k,l)
term(296) = term(296) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_81_so_pt2(b,j,p,i,k,l)
term(297) = term(297) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,j,p,i,k,l)
term(298) = term(298) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,j,i,k,l)
term(299) = term(299) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,j,p,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(294) = term(294) * (0.16666666666666666d+0) 
term(295) = term(295) * (0.16666666666666666d+0) 
term(296) = term(296) * (0.16666666666666666d+0) 
term(297) = term(297) * (0.6666666666666666d+0) 
term(298) = term(298) * (0.6666666666666666d+0) 
term(299) = term(299) * (-0.6666666666666666d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(300) = term(300) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,i,p,j,k,l)
term(301) = term(301) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_75_so_pt2(b,i,p,j,k,l)
term(302) = term(302) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,i,j,p,k,l)
term(303) = term(303) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_75_so_pt2(b,j,i,p,k,l)
term(304) = term(304) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_75_so_pt2(b,i,j,p,k,l)
term(305) = term(305) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,i,j,k,l)
term(306) = term(306) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_78_so_pt2(b,p,i,j,k,l)
term(307) = term(307) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,j,i,p,k,l)
term(308) = term(308) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_78_so_pt2(b,i,j,p,k,l)
term(309) = term(309) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_78_so_pt2(b,j,i,p,k,l)
term(310) = term(310) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_81_so_pt2(b,i,p,j,k,l)
term(311) = term(311) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,i,p,j,k,l)
term(312) = term(312) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,l,k) * wm_interm_81_so_pt2(b,i,j,p,k,l)
term(313) = term(313) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,j,i,p,k,l)
term(314) = term(314) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,i,j,p,k,l)
term(315) = term(315) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,i,p,j,k,l)
term(316) = term(316) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,i,j,p,k,l)
term(317) = term(317) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_75_so_pt2(a,j,i,p,k,l)
term(318) = term(318) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,p,i,j,k,l)
term(319) = term(319) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,j,i,p,k,l)
term(320) = term(320) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_78_so_pt2(a,i,j,p,k,l)
term(321) = term(321) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,i,p,j,k,l)
term(322) = term(322) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,j,i,p,k,l)
term(323) = term(323) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,l,k) * wm_interm_81_so_pt2(a,i,j,p,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(300) = term(300) * (-0.3333333333333333d+0) 
term(301) = term(301) * (0.16666666666666666d+0) 
term(302) = term(302) * (0.16666666666666666d+0) 
term(303) = term(303) * (0.16666666666666666d+0) 
term(304) = term(304) * (-0.3333333333333333d+0) 
term(305) = term(305) * (-0.3333333333333333d+0) 
term(306) = term(306) * (0.16666666666666666d+0) 
term(307) = term(307) * (0.16666666666666666d+0) 
term(308) = term(308) * (0.16666666666666666d+0) 
term(309) = term(309) * (-0.3333333333333333d+0) 
term(310) = term(310) * (-0.3333333333333333d+0) 
term(311) = term(311) * (0.16666666666666666d+0) 
term(312) = term(312) * (0.16666666666666666d+0) 
term(313) = term(313) * (0.16666666666666666d+0) 
term(314) = term(314) * (-0.3333333333333333d+0) 
term(315) = term(315) * (-0.6666666666666666d+0) 
term(316) = term(316) * (0.6666666666666666d+0) 
term(317) = term(317) * (-0.6666666666666666d+0) 
term(318) = term(318) * (-0.6666666666666666d+0) 
term(319) = term(319) * (0.6666666666666666d+0) 
term(320) = term(320) * (-0.6666666666666666d+0) 
term(321) = term(321) * (0.6666666666666666d+0) 
term(322) = term(322) * (0.6666666666666666d+0) 
term(323) = term(323) * (-0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(324) = term(324) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_40_so_pt2(b,j,k,i)
term(325) = term(325) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_42_so_pt2(b,j,k,i)
term(326) = term(326) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_55_so_pt2(b,j,k,i)
term(327) = term(327) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_56_so_pt2(b,j,k,i)
term(328) = term(328) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_57_so_pt2(b,j,k,i)
term(329) = term(329) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_58_so_pt2(b,j,k,i)
term(330) = term(330) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_46_so_pt2(b,j,k,i)
term(331) = term(331) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_47_so_pt2(b,j,k,i)
term(332) = term(332) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_48_so_pt2(b,j,k,i)
term(333) = term(333) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_46_so_pt2(b,k,j,i)
term(334) = term(334) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_47_so_pt2(b,k,j,i)
term(335) = term(335) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_48_so_pt2(b,k,j,i)
term(336) = term(336) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_41_so_pt2(b,k,j,i)
term(337) = term(337) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_40_so_pt2(b,k,j,i)
term(338) = term(338) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_55_so_pt2(b,k,j,i)
term(339) = term(339) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_56_so_pt2(b,k,j,i)
term(340) = term(340) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_57_so_pt2(b,k,j,i)
term(341) = term(341) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_58_so_pt2(b,k,j,i)
term(342) = term(342) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_82_so_pt2(b,j,k,i)
term(343) = term(343) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_83_so_pt2(b,j,k,i)
term(344) = term(344) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_92_so_pt2(b,j,k,i)
term(345) = term(345) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_93_so_pt2(b,j,k,i)
term(346) = term(346) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_86_so_pt2(b,j,k,i)
term(347) = term(347) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_87_so_pt2(b,j,k,i)
term(348) = term(348) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_86_so_pt2(b,k,j,i)
term(349) = term(349) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_87_so_pt2(b,k,j,i)
term(350) = term(350) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_83_so_pt2(b,k,j,i)
term(351) = term(351) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_82_so_pt2(b,k,j,i)
term(352) = term(352) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_92_so_pt2(b,k,j,i)
term(353) = term(353) + s2(a,q,p,i) * t2(a,b,k,j) * wm_interm_93_so_pt2(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(324) = term(324) * (0.5d+0) 
term(325) = term(325) * (0.5d+0) 
term(326) = term(326) * (-1.0d+0) 
term(327) = term(327) * (-1.0d+0) 
term(328) = term(328) * (-1.0d+0) 
term(329) = term(329) * (2.0d+0) 
term(330) = term(330) * (0.5d+0) 
term(331) = term(331) * (-1.0d+0) 
term(332) = term(332) * (0.5d+0) 
term(333) = term(333) * (-1.0d+0) 
term(334) = term(334) * (2.0d+0) 
term(335) = term(335) * (-1.0d+0) 
term(336) = term(336) * (0.5d+0) 
term(337) = term(337) * (-1.0d+0) 
term(338) = term(338) * (0.5d+0) 
term(339) = term(339) * (0.5d+0) 
term(340) = term(340) * (0.5d+0) 
term(341) = term(341) * (-1.0d+0) 
term(342) = term(342) * (2.0d+0) 
term(343) = term(343) * (-2.0d+0) 
term(344) = term(344) * (-4.0d+0) 
term(345) = term(345) * (4.0d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (-2.0d+0) 
term(348) = term(348) * (-4.0d+0) 
term(349) = term(349) * (4.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * (2.0d+0) 
term(353) = term(353) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(354) = term(354) + wm_interm_1_so_pt2(i,j) * wm_interm_40_so_pt2(q,p,j,i)
term(355) = term(355) + wm_interm_1_so_pt2(i,j) * wm_interm_41_so_pt2(q,p,j,i)
term(356) = term(356) + wm_interm_2_so_pt2(i,j) * wm_interm_40_so_pt2(q,p,j,i)
term(357) = term(357) + wm_interm_2_so_pt2(i,j) * wm_interm_41_so_pt2(q,p,j,i)
term(358) = term(358) + wm_interm_1_so_pt2(i,j) * wm_interm_42_so_pt2(q,p,j,i)
term(359) = term(359) + wm_interm_2_so_pt2(i,j) * wm_interm_42_so_pt2(q,p,j,i)
term(360) = term(360) + wm_interm_1_so_pt2(i,j) * wm_interm_46_so_pt2(q,p,j,i)
term(361) = term(361) + wm_interm_2_so_pt2(i,j) * wm_interm_46_so_pt2(q,p,j,i)
term(362) = term(362) + wm_interm_1_so_pt2(i,j) * wm_interm_47_so_pt2(q,p,j,i)
term(363) = term(363) + wm_interm_2_so_pt2(i,j) * wm_interm_47_so_pt2(q,p,j,i)
term(364) = term(364) + wm_interm_1_so_pt2(i,j) * wm_interm_48_so_pt2(q,p,j,i)
term(365) = term(365) + wm_interm_2_so_pt2(i,j) * wm_interm_48_so_pt2(q,p,j,i)
term(366) = term(366) + wm_interm_1_so_pt2(i,j) * wm_interm_41_so_pt2(q,j,p,i)
term(367) = term(367) + wm_interm_2_so_pt2(i,j) * wm_interm_41_so_pt2(q,j,p,i)
term(368) = term(368) + wm_interm_1_so_pt2(i,j) * wm_interm_55_so_pt2(q,j,p,i)
term(369) = term(369) + wm_interm_1_so_pt2(i,j) * wm_interm_42_so_pt2(q,j,p,i)
term(370) = term(370) + wm_interm_2_so_pt2(i,j) * wm_interm_55_so_pt2(q,j,p,i)
term(371) = term(371) + wm_interm_2_so_pt2(i,j) * wm_interm_42_so_pt2(q,j,p,i)
term(372) = term(372) + wm_interm_1_so_pt2(i,j) * wm_interm_56_so_pt2(q,j,p,i)
term(373) = term(373) + wm_interm_2_so_pt2(i,j) * wm_interm_56_so_pt2(q,j,p,i)
term(374) = term(374) + wm_interm_1_so_pt2(i,j) * wm_interm_57_so_pt2(q,j,p,i)
term(375) = term(375) + wm_interm_2_so_pt2(i,j) * wm_interm_57_so_pt2(q,j,p,i)
term(376) = term(376) + wm_interm_1_so_pt2(i,j) * wm_interm_58_so_pt2(q,j,p,i)
term(377) = term(377) + wm_interm_2_so_pt2(i,j) * wm_interm_58_so_pt2(q,j,p,i)
term(378) = term(378) + wm_interm_1_so_pt2(i,j) * wm_interm_82_so_pt2(q,p,j,i)
term(379) = term(379) + wm_interm_1_so_pt2(i,j) * wm_interm_83_so_pt2(q,p,j,i)
term(380) = term(380) + wm_interm_2_so_pt2(i,j) * wm_interm_82_so_pt2(q,p,j,i)
term(381) = term(381) + wm_interm_2_so_pt2(i,j) * wm_interm_83_so_pt2(q,p,j,i)
term(382) = term(382) + wm_interm_1_so_pt2(i,j) * wm_interm_86_so_pt2(q,p,j,i)
term(383) = term(383) + wm_interm_2_so_pt2(i,j) * wm_interm_86_so_pt2(q,p,j,i)
term(384) = term(384) + wm_interm_1_so_pt2(i,j) * wm_interm_87_so_pt2(q,p,j,i)
term(385) = term(385) + wm_interm_2_so_pt2(i,j) * wm_interm_87_so_pt2(q,p,j,i)
term(386) = term(386) + wm_interm_1_so_pt2(i,j) * wm_interm_83_so_pt2(q,j,p,i)
term(387) = term(387) + wm_interm_2_so_pt2(i,j) * wm_interm_83_so_pt2(q,j,p,i)
term(388) = term(388) + wm_interm_1_so_pt2(i,j) * wm_interm_82_so_pt2(q,j,p,i)
term(389) = term(389) + wm_interm_2_so_pt2(i,j) * wm_interm_82_so_pt2(q,j,p,i)
term(390) = term(390) + wm_interm_1_so_pt2(i,j) * wm_interm_92_so_pt2(q,j,p,i)
term(391) = term(391) + wm_interm_2_so_pt2(i,j) * wm_interm_92_so_pt2(q,j,p,i)
term(392) = term(392) + wm_interm_1_so_pt2(i,j) * wm_interm_93_so_pt2(q,j,p,i)
term(393) = term(393) + wm_interm_2_so_pt2(i,j) * wm_interm_93_so_pt2(q,j,p,i)
end do 
end do 

term(354) = term(354) * (0.5d+0) 
term(355) = term(355) * (-1.0d+0) 
term(356) = term(356) * (-1.0d+0) 
term(357) = term(357) * (2.0d+0) 
term(358) = term(358) * (0.5d+0) 
term(359) = term(359) * (-1.0d+0) 
term(360) = term(360) * (0.5d+0) 
term(361) = term(361) * (-1.0d+0) 
term(362) = term(362) * (-1.0d+0) 
term(363) = term(363) * (2.0d+0) 
term(364) = term(364) * (0.5d+0) 
term(365) = term(365) * (-1.0d+0) 
term(366) = term(366) * (0.5d+0) 
term(367) = term(367) * (-1.0d+0) 
term(368) = term(368) * (0.5d+0) 
term(369) = term(369) * (-1.0d+0) 
term(370) = term(370) * (-1.0d+0) 
term(371) = term(371) * (2.0d+0) 
term(372) = term(372) * (0.5d+0) 
term(373) = term(373) * (-1.0d+0) 
term(374) = term(374) * (0.5d+0) 
term(375) = term(375) * (-1.0d+0) 
term(376) = term(376) * (-1.0d+0) 
term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (2.0d+0) 
term(379) = term(379) * (-2.0d+0) 
term(380) = term(380) * (-4.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(382) = term(382) * (2.0d+0) 
term(383) = term(383) * (-4.0d+0) 
term(384) = term(384) * (-2.0d+0) 
term(385) = term(385) * (4.0d+0) 
term(386) = term(386) * (2.0d+0) 
term(387) = term(387) * (-4.0d+0) 
term(388) = term(388) * (-2.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (2.0d+0) 
term(391) = term(391) * (-4.0d+0) 
term(392) = term(392) * (-2.0d+0) 
term(393) = term(393) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(394) = term(394) + wm_interm_69_so_pt2(q,i,j,k) * wm_interm_73_so_pt2(i,j,k,p)
term(395) = term(395) + wm_interm_69_so_pt2(q,i,j,k) * wm_interm_73_so_pt2(j,i,p,k)
term(396) = term(396) + wm_interm_69_so_pt2(q,i,j,k) * wm_interm_73_so_pt2(i,j,p,k)
term(397) = term(397) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_74_so_pt2(q,i,j,k)
term(398) = term(398) + wm_interm_73_so_pt2(i,j,k,p) * wm_interm_76_so_pt2(q,i,j,k)
term(399) = term(399) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_76_so_pt2(q,i,j,k)
term(400) = term(400) + wm_interm_73_so_pt2(i,j,k,p) * wm_interm_77_so_pt2(q,i,j,k)
term(401) = term(401) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_77_so_pt2(q,i,j,k)
term(402) = term(402) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_79_so_pt2(q,i,j,k)
term(403) = term(403) + wm_interm_73_so_pt2(i,j,p,k) * wm_interm_80_so_pt2(q,i,j,k)
term(404) = term(404) + wm_interm_102_so_pt2(i,j,k,p) * wm_interm_69_so_pt2(q,i,j,k)
term(405) = term(405) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_69_so_pt2(q,i,j,k)
term(406) = term(406) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_74_so_pt2(q,i,j,k)
term(407) = term(407) + wm_interm_102_so_pt2(i,j,k,p) * wm_interm_76_so_pt2(q,i,j,k)
term(408) = term(408) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_76_so_pt2(q,i,j,k)
term(409) = term(409) + wm_interm_102_so_pt2(i,j,k,p) * wm_interm_77_so_pt2(q,i,j,k)
term(410) = term(410) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_77_so_pt2(q,i,j,k)
term(411) = term(411) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_79_so_pt2(q,i,j,k)
term(412) = term(412) + wm_interm_102_so_pt2(i,j,p,k) * wm_interm_80_so_pt2(q,i,j,k)
end do 
end do 
end do 

term(394) = term(394) * (0.5d+0) 
term(395) = term(395) * (0.5d+0) 
term(396) = term(396) * (-1.0d+0) 
term(397) = term(397) * (0.5d+0) 
term(398) = term(398) * (-1.0d+0) 
term(399) = term(399) * (2.0d+0) 
term(400) = term(400) * (0.5d+0) 
term(401) = term(401) * (-1.0d+0) 
term(402) = term(402) * (-1.0d+0) 
term(403) = term(403) * (0.5d+0) 
term(405) = term(405) * (-2.0d+0) 
term(407) = term(407) * (-2.0d+0) 
term(408) = term(408) * (4.0d+0) 
term(410) = term(410) * (-2.0d+0) 
term(411) = term(411) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(413) = term(413) + r3(vrdav_Rl, a,p,b,j,q,i) * r1(vrdav_Rr, a,i) * t1(b,j)
term(414) = term(414) + r3(vrdav_Rl, a,p,b,j,q,i) * r1(vrdav_Rr, b,i) * t1(a,j)
term(415) = term(415) + r3(vrdav_Rl, a,p,b,j,q,i) * r1(vrdav_Rr, a,j) * t1(b,i)
end do 
end do 
end do 
end do 

term(413) = term(413) * (3.9999999999999996d+0) 
term(414) = term(414) * (-1.9999999999999998d+0) 
term(415) = term(415) * (-1.9999999999999998d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(416) = term(416) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,j,i,p,k,l)
term(417) = term(417) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,p,j,i,k,l)
term(418) = term(418) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,j,p,i,k,l)
term(419) = term(419) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,j,p,i,k,l)
term(420) = term(420) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,p,j,i,k,l)
term(421) = term(421) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,j,i,p,k,l)
term(422) = term(422) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,p,j,i,k,l)
term(423) = term(423) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,j,p,i,k,l)
term(424) = term(424) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,j,p,i,k,l)
term(425) = term(425) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,p,j,i,k,l)
end do 
end do 
end do 
end do 

term(416) = term(416) * (0.16666666666666666d+0) 
term(417) = term(417) * (0.16666666666666666d+0) 
term(418) = term(418) * (0.16666666666666666d+0) 
term(419) = term(419) * (0.16666666666666666d+0) 
term(420) = term(420) * (-0.3333333333333333d+0) 
term(421) = term(421) * (0.3333333333333333d+0) 
term(422) = term(422) * (0.3333333333333333d+0) 
term(423) = term(423) * (0.3333333333333333d+0) 
term(424) = term(424) * (0.3333333333333333d+0) 
term(425) = term(425) * (-0.6666666666666666d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(426) = term(426) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_62_so_pt2(q,k,p,l,j,i)
term(427) = term(427) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_66_so_pt2(q,k,p,l,j,i)
term(428) = term(428) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_67_so_pt2(q,k,p,l,j,i)
term(429) = term(429) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_65_so_pt2(q,p,k,l,j,i)
term(430) = term(430) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_66_so_pt2(q,p,k,l,j,i)
term(431) = term(431) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_67_so_pt2(q,p,k,l,j,i)
term(432) = term(432) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_63_so_pt2(q,k,l,p,j,i)
term(433) = term(433) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_65_so_pt2(q,k,l,p,j,i)
term(434) = term(434) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_68_so_pt2(q,k,l,p,j,i)
term(435) = term(435) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_96_so_pt2(q,k,p,l,j,i)
term(436) = term(436) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_98_so_pt2(q,k,p,l,j,i)
term(437) = term(437) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_97_so_pt2(q,p,k,l,j,i)
term(438) = term(438) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_98_so_pt2(q,p,k,l,j,i)
term(439) = term(439) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_96_so_pt2(q,k,l,p,j,i)
term(440) = term(440) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_97_so_pt2(q,k,l,p,j,i)
end do 
end do 
end do 
end do 

term(426) = term(426) * (0.16666666666666666d+0) 
term(427) = term(427) * (0.16666666666666666d+0) 
term(428) = term(428) * (-0.3333333333333333d+0) 
term(429) = term(429) * (0.16666666666666666d+0) 
term(430) = term(430) * (-0.3333333333333333d+0) 
term(431) = term(431) * (0.16666666666666666d+0) 
term(432) = term(432) * (0.16666666666666666d+0) 
term(433) = term(433) * (0.16666666666666666d+0) 
term(434) = term(434) * (-0.3333333333333333d+0) 
term(435) = term(435) * (0.6666666666666666d+0) 
term(436) = term(436) * (0.6666666666666666d+0) 
term(437) = term(437) * (0.6666666666666666d+0) 
term(438) = term(438) * (-0.6666666666666666d+0) 
term(439) = term(439) * (-0.6666666666666666d+0) 
term(440) = term(440) * (0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(441) = term(441) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_62_so_pt2(q,k,p,l,i,j)
term(442) = term(442) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_63_so_pt2(q,k,p,l,i,j)
term(443) = term(443) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_67_so_pt2(q,k,p,l,i,j)
term(444) = term(444) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_65_so_pt2(q,p,k,l,i,j)
term(445) = term(445) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_66_so_pt2(q,p,k,l,i,j)
term(446) = term(446) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_68_so_pt2(q,p,k,l,i,j)
term(447) = term(447) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_62_so_pt2(q,k,l,p,i,j)
term(448) = term(448) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_63_so_pt2(q,k,l,p,i,j)
term(449) = term(449) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_68_so_pt2(q,k,l,p,i,j)
term(450) = term(450) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_96_so_pt2(q,k,p,l,i,j)
term(451) = term(451) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_98_so_pt2(q,k,p,l,i,j)
term(452) = term(452) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_97_so_pt2(q,p,k,l,i,j)
term(453) = term(453) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_98_so_pt2(q,p,k,l,i,j)
term(454) = term(454) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_96_so_pt2(q,k,l,p,i,j)
term(455) = term(455) + wm_interm_14_so_pt2(i,j,k,l) * wm_interm_97_so_pt2(q,k,l,p,i,j)
end do 
end do 
end do 
end do 

term(441) = term(441) * (-0.3333333333333333d+0) 
term(442) = term(442) * (0.16666666666666666d+0) 
term(443) = term(443) * (0.16666666666666666d+0) 
term(444) = term(444) * (-0.3333333333333333d+0) 
term(445) = term(445) * (0.16666666666666666d+0) 
term(446) = term(446) * (0.16666666666666666d+0) 
term(447) = term(447) * (0.16666666666666666d+0) 
term(448) = term(448) * (-0.3333333333333333d+0) 
term(449) = term(449) * (0.16666666666666666d+0) 
term(450) = term(450) * (-0.6666666666666666d+0) 
term(451) = term(451) * (-0.6666666666666666d+0) 
term(452) = term(452) * (-0.6666666666666666d+0) 
term(453) = term(453) * (0.6666666666666666d+0) 
term(454) = term(454) * (0.6666666666666666d+0) 
term(455) = term(455) * (-0.6666666666666666d+0) 

do i = 1, nocc 
term(456) = term(456) + wm_interm_1_so_pt2(p,i) * wm_interm_43_so_pt2(q,i)
term(457) = term(457) + wm_interm_2_so_pt2(p,i) * wm_interm_43_so_pt2(q,i)
term(458) = term(458) + wm_interm_1_so_pt2(p,i) * wm_interm_44_so_pt2(q,i)
term(459) = term(459) + wm_interm_2_so_pt2(p,i) * wm_interm_44_so_pt2(q,i)
term(460) = term(460) + wm_interm_1_so_pt2(p,i) * wm_interm_45_so_pt2(q,i)
term(461) = term(461) + wm_interm_2_so_pt2(p,i) * wm_interm_45_so_pt2(q,i)
term(462) = term(462) + wm_interm_1_so_pt2(p,i) * wm_interm_84_so_pt2(q,i)
term(463) = term(463) + wm_interm_2_so_pt2(p,i) * wm_interm_84_so_pt2(q,i)
term(464) = term(464) + wm_interm_1_so_pt2(p,i) * wm_interm_85_so_pt2(q,i)
term(465) = term(465) + wm_interm_2_so_pt2(p,i) * wm_interm_85_so_pt2(q,i)
term(466) = term(466) + wm_interm_103_so_pt2(q,i) * wm_interm_70_so_pt2(i,p)
term(467) = term(467) + wm_interm_104_so_pt2(q,i) * wm_interm_70_so_pt2(i,p)
term(468) = term(468) + wm_interm_103_so_pt2(q,i) * wm_interm_71_so_pt2(i,p)
term(469) = term(469) + wm_interm_104_so_pt2(q,i) * wm_interm_71_so_pt2(i,p)
term(470) = term(470) + wm_interm_103_so_pt2(q,i) * wm_interm_72_so_pt2(i,p)
term(471) = term(471) + wm_interm_104_so_pt2(q,i) * wm_interm_72_so_pt2(i,p)
term(472) = term(472) + wm_interm_105_so_pt2(q,i) * wm_interm_70_so_pt2(i,p)
term(473) = term(473) + wm_interm_106_so_pt2(q,i) * wm_interm_70_so_pt2(i,p)
term(474) = term(474) + wm_interm_105_so_pt2(q,i) * wm_interm_71_so_pt2(i,p)
term(475) = term(475) + wm_interm_106_so_pt2(q,i) * wm_interm_71_so_pt2(i,p)
term(476) = term(476) + wm_interm_105_so_pt2(q,i) * wm_interm_72_so_pt2(i,p)
term(477) = term(477) + wm_interm_106_so_pt2(q,i) * wm_interm_72_so_pt2(i,p)
term(478) = term(478) + wm_interm_103_so_pt2(q,i) * wm_interm_99_so_pt2(i,p)
term(479) = term(479) + wm_interm_104_so_pt2(q,i) * wm_interm_99_so_pt2(i,p)
term(480) = term(480) + wm_interm_100_so_pt2(i,p) * wm_interm_103_so_pt2(q,i)
term(481) = term(481) + wm_interm_100_so_pt2(i,p) * wm_interm_104_so_pt2(q,i)
term(482) = term(482) + wm_interm_101_so_pt2(i,p) * wm_interm_103_so_pt2(q,i)
term(483) = term(483) + wm_interm_101_so_pt2(i,p) * wm_interm_104_so_pt2(q,i)
term(484) = term(484) + wm_interm_105_so_pt2(q,i) * wm_interm_99_so_pt2(i,p)
term(485) = term(485) + wm_interm_106_so_pt2(q,i) * wm_interm_99_so_pt2(i,p)
term(486) = term(486) + wm_interm_100_so_pt2(i,p) * wm_interm_105_so_pt2(q,i)
term(487) = term(487) + wm_interm_100_so_pt2(i,p) * wm_interm_106_so_pt2(q,i)
term(488) = term(488) + wm_interm_101_so_pt2(i,p) * wm_interm_105_so_pt2(q,i)
term(489) = term(489) + wm_interm_101_so_pt2(i,p) * wm_interm_106_so_pt2(q,i)
end do 

term(457) = term(457) * (-1.9999999999999998d+0) 
term(459) = term(459) * (-1.9999999999999998d+0) 
term(460) = term(460) * (-1.9999999999999998d+0) 
term(461) = term(461) * (3.9999999999999996d+0) 
term(462) = term(462) * (3.9999999999999996d+0) 
term(463) = term(463) * (-7.999999999999999d+0) 
term(464) = term(464) * (-3.9999999999999996d+0) 
term(465) = term(465) * (7.999999999999999d+0) 
term(466) = term(466) * (-1.9999999999999998d+0) 
term(467) = term(467) * (2.0d+0) 
term(468) = term(468) * (3.9999999999999996d+0) 
term(469) = term(469) * (-4.0d+0) 
term(470) = term(470) * (-1.9999999999999998d+0) 
term(471) = term(471) * (2.0d+0) 
term(473) = term(473) * (-1.0d+0) 
term(474) = term(474) * (-1.9999999999999998d+0) 
term(475) = term(475) * (2.0d+0) 
term(477) = term(477) * (-1.0d+0) 
term(478) = term(478) * (-3.9999999999999996d+0) 
term(479) = term(479) * (4.0d+0) 
term(480) = term(480) * (7.999999999999999d+0) 
term(481) = term(481) * (-8.0d+0) 
term(482) = term(482) * (-3.9999999999999996d+0) 
term(483) = term(483) * (4.0d+0) 
term(484) = term(484) * (1.9999999999999998d+0) 
term(485) = term(485) * (-2.0d+0) 
term(486) = term(486) * (-3.9999999999999996d+0) 
term(487) = term(487) * (4.0d+0) 
term(488) = term(488) * (1.9999999999999998d+0) 
term(489) = term(489) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(490) = term(490) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_69_so_pt2(a,j,i,k)
term(491) = term(491) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_69_so_pt2(b,i,j,k)
term(492) = term(492) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_69_so_pt2(b,j,i,k)
term(493) = term(493) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_74_so_pt2(a,i,j,k)
term(494) = term(494) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_76_so_pt2(a,j,i,k)
term(495) = term(495) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_74_so_pt2(b,j,i,k)
term(496) = term(496) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_76_so_pt2(b,i,j,k)
term(497) = term(497) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_74_so_pt2(b,i,j,k)
term(498) = term(498) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_76_so_pt2(b,j,i,k)
term(499) = term(499) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_77_so_pt2(a,j,i,k)
term(500) = term(500) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_77_so_pt2(b,i,j,k)
term(501) = term(501) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_77_so_pt2(b,j,i,k)
term(502) = term(502) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_79_so_pt2(a,i,j,k)
term(503) = term(503) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_79_so_pt2(b,j,i,k)
term(504) = term(504) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_79_so_pt2(b,i,j,k)
term(505) = term(505) + r2p(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_80_so_pt2(a,i,j,k)
term(506) = term(506) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_80_so_pt2(b,j,i,k)
term(507) = term(507) + r2p(vrdav_Rr, a,i,b,j) * s2(a,q,p,k) * wm_interm_80_so_pt2(b,i,j,k)
term(508) = term(508) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_69_so_pt2(a,j,i,k)
term(509) = term(509) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_69_so_pt2(a,i,j,k)
term(510) = term(510) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_74_so_pt2(a,i,j,k)
term(511) = term(511) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_76_so_pt2(a,j,i,k)
term(512) = term(512) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_74_so_pt2(a,j,i,k)
term(513) = term(513) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_76_so_pt2(a,i,j,k)
term(514) = term(514) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_77_so_pt2(a,j,i,k)
term(515) = term(515) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_77_so_pt2(a,i,j,k)
term(516) = term(516) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_79_so_pt2(a,i,j,k)
term(517) = term(517) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_79_so_pt2(a,j,i,k)
term(518) = term(518) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_80_so_pt2(a,i,j,k)
term(519) = term(519) + r2m(vrdav_Rr, a,i,b,j) * s2(b,q,p,k) * wm_interm_80_so_pt2(a,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(490) = term(490) * (0.5d+0) 
term(491) = term(491) * (0.5d+0) 
term(492) = term(492) * (-1.0d+0) 
term(493) = term(493) * (0.5d+0) 
term(494) = term(494) * (-1.0d+0) 
term(495) = term(495) * (0.5d+0) 
term(496) = term(496) * (-1.0d+0) 
term(497) = term(497) * (-1.0d+0) 
term(498) = term(498) * (2.0d+0) 
term(499) = term(499) * (0.5d+0) 
term(500) = term(500) * (0.5d+0) 
term(501) = term(501) * (-1.0d+0) 
term(502) = term(502) * (-1.0d+0) 
term(503) = term(503) * (-1.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (0.5d+0) 
term(506) = term(506) * (0.5d+0) 
term(507) = term(507) * (-1.0d+0) 
term(508) = term(508) * (2.0d+0) 
term(509) = term(509) * (-2.0d+0) 
term(510) = term(510) * (2.0d+0) 
term(511) = term(511) * (-4.0d+0) 
term(512) = term(512) * (-2.0d+0) 
term(513) = term(513) * (4.0d+0) 
term(514) = term(514) * (2.0d+0) 
term(515) = term(515) * (-2.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (4.0d+0) 
term(518) = term(518) * (2.0d+0) 
term(519) = term(519) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(520) = term(520) + wm_interm_39_so_pt2(q,a,p,i) * wm_interm_43_so_pt2(a,i)
term(521) = term(521) + wm_interm_39_so_pt2(q,a,p,i) * wm_interm_44_so_pt2(a,i)
term(522) = term(522) + wm_interm_39_so_pt2(q,a,p,i) * wm_interm_45_so_pt2(a,i)
term(523) = term(523) + wm_interm_39_so_pt2(q,a,p,i) * wm_interm_84_so_pt2(a,i)
term(524) = term(524) + wm_interm_39_so_pt2(q,a,p,i) * wm_interm_85_so_pt2(a,i)
term(525) = term(525) + wm_interm_103_so_pt2(a,i) * wm_interm_53_so_pt2(a,q,i,p)
term(526) = term(526) + wm_interm_103_so_pt2(a,i) * wm_interm_59_so_pt2(a,q,i,p)
term(527) = term(527) + wm_interm_104_so_pt2(a,i) * wm_interm_53_so_pt2(a,q,i,p)
term(528) = term(528) + wm_interm_104_so_pt2(a,i) * wm_interm_59_so_pt2(a,q,i,p)
term(529) = term(529) + wm_interm_103_so_pt2(a,i) * wm_interm_49_so_pt2(a,q,i,p)
term(530) = term(530) + wm_interm_104_so_pt2(a,i) * wm_interm_49_so_pt2(a,q,i,p)
term(531) = term(531) + wm_interm_105_so_pt2(a,i) * wm_interm_53_so_pt2(a,q,i,p)
term(532) = term(532) + wm_interm_105_so_pt2(a,i) * wm_interm_59_so_pt2(a,q,i,p)
term(533) = term(533) + wm_interm_106_so_pt2(a,i) * wm_interm_53_so_pt2(a,q,i,p)
term(534) = term(534) + wm_interm_106_so_pt2(a,i) * wm_interm_59_so_pt2(a,q,i,p)
term(535) = term(535) + wm_interm_105_so_pt2(a,i) * wm_interm_49_so_pt2(a,q,i,p)
term(536) = term(536) + wm_interm_106_so_pt2(a,i) * wm_interm_49_so_pt2(a,q,i,p)
term(537) = term(537) + wm_interm_103_so_pt2(a,i) * wm_interm_88_so_pt2(a,q,i,p)
term(538) = term(538) + wm_interm_103_so_pt2(a,i) * wm_interm_89_so_pt2(a,q,i,p)
term(539) = term(539) + wm_interm_104_so_pt2(a,i) * wm_interm_88_so_pt2(a,q,i,p)
term(540) = term(540) + wm_interm_104_so_pt2(a,i) * wm_interm_89_so_pt2(a,q,i,p)
term(541) = term(541) + wm_interm_105_so_pt2(a,i) * wm_interm_88_so_pt2(a,q,i,p)
term(542) = term(542) + wm_interm_105_so_pt2(a,i) * wm_interm_89_so_pt2(a,q,i,p)
term(543) = term(543) + wm_interm_106_so_pt2(a,i) * wm_interm_88_so_pt2(a,q,i,p)
term(544) = term(544) + wm_interm_106_so_pt2(a,i) * wm_interm_89_so_pt2(a,q,i,p)
end do 
end do 

term(522) = term(522) * (-1.9999999999999998d+0) 
term(523) = term(523) * (3.9999999999999996d+0) 
term(524) = term(524) * (-3.9999999999999996d+0) 
term(525) = term(525) * (-1.9999999999999998d+0) 
term(526) = term(526) * (3.9999999999999996d+0) 
term(527) = term(527) * (2.0d+0) 
term(528) = term(528) * (-4.0d+0) 
term(529) = term(529) * (-1.9999999999999998d+0) 
term(530) = term(530) * (2.0d+0) 
term(532) = term(532) * (-1.9999999999999998d+0) 
term(533) = term(533) * (-1.0d+0) 
term(534) = term(534) * (2.0d+0) 
term(536) = term(536) * (-1.0d+0) 
term(537) = term(537) * (-7.999999999999999d+0) 
term(538) = term(538) * (7.999999999999999d+0) 
term(539) = term(539) * (8.0d+0) 
term(540) = term(540) * (-8.0d+0) 
term(541) = term(541) * (3.9999999999999996d+0) 
term(542) = term(542) * (-3.9999999999999996d+0) 
term(543) = term(543) * (-4.0d+0) 
term(544) = term(544) * (4.0d+0) 

do a = nocc + 1, nactive 
term(545) = term(545) + wm_interm_44_so_pt2(a,p) * wm_interm_5_so_pt2(q,a)
term(546) = term(546) + wm_interm_44_so_pt2(a,p) * wm_interm_7_so_pt2(q,a)
term(547) = term(547) + wm_interm_45_so_pt2(a,p) * wm_interm_5_so_pt2(q,a)
term(548) = term(548) + wm_interm_45_so_pt2(a,p) * wm_interm_7_so_pt2(q,a)
term(549) = term(549) + wm_interm_43_so_pt2(a,p) * wm_interm_5_so_pt2(q,a)
term(550) = term(550) + wm_interm_43_so_pt2(a,p) * wm_interm_7_so_pt2(q,a)
term(551) = term(551) + wm_interm_5_so_pt2(q,a) * wm_interm_84_so_pt2(a,p)
term(552) = term(552) + wm_interm_7_so_pt2(q,a) * wm_interm_84_so_pt2(a,p)
term(553) = term(553) + wm_interm_5_so_pt2(q,a) * wm_interm_85_so_pt2(a,p)
term(554) = term(554) + wm_interm_7_so_pt2(q,a) * wm_interm_85_so_pt2(a,p)
term(555) = term(555) + wm_interm_103_so_pt2(a,p) * wm_interm_64_so_pt2(a,q)
term(556) = term(556) + wm_interm_104_so_pt2(a,p) * wm_interm_64_so_pt2(a,q)
term(557) = term(557) + wm_interm_103_so_pt2(a,p) * wm_interm_60_so_pt2(a,q)
term(558) = term(558) + wm_interm_104_so_pt2(a,p) * wm_interm_60_so_pt2(a,q)
term(559) = term(559) + wm_interm_103_so_pt2(a,p) * wm_interm_61_so_pt2(a,q)
term(560) = term(560) + wm_interm_104_so_pt2(a,p) * wm_interm_61_so_pt2(a,q)
term(561) = term(561) + wm_interm_105_so_pt2(a,p) * wm_interm_64_so_pt2(a,q)
term(562) = term(562) + wm_interm_106_so_pt2(a,p) * wm_interm_64_so_pt2(a,q)
term(563) = term(563) + wm_interm_105_so_pt2(a,p) * wm_interm_60_so_pt2(a,q)
term(564) = term(564) + wm_interm_106_so_pt2(a,p) * wm_interm_60_so_pt2(a,q)
term(565) = term(565) + wm_interm_105_so_pt2(a,p) * wm_interm_61_so_pt2(a,q)
term(566) = term(566) + wm_interm_106_so_pt2(a,p) * wm_interm_61_so_pt2(a,q)
term(567) = term(567) + wm_interm_103_so_pt2(a,p) * wm_interm_94_so_pt2(a,q)
term(568) = term(568) + wm_interm_104_so_pt2(a,p) * wm_interm_94_so_pt2(a,q)
term(569) = term(569) + wm_interm_103_so_pt2(a,p) * wm_interm_95_so_pt2(a,q)
term(570) = term(570) + wm_interm_104_so_pt2(a,p) * wm_interm_95_so_pt2(a,q)
term(571) = term(571) + wm_interm_105_so_pt2(a,p) * wm_interm_94_so_pt2(a,q)
term(572) = term(572) + wm_interm_106_so_pt2(a,p) * wm_interm_94_so_pt2(a,q)
term(573) = term(573) + wm_interm_105_so_pt2(a,p) * wm_interm_95_so_pt2(a,q)
term(574) = term(574) + wm_interm_106_so_pt2(a,p) * wm_interm_95_so_pt2(a,q)
end do 

term(546) = term(546) * (-1.9999999999999998d+0) 
term(547) = term(547) * (-1.9999999999999998d+0) 
term(548) = term(548) * (3.9999999999999996d+0) 
term(550) = term(550) * (-1.9999999999999998d+0) 
term(551) = term(551) * (3.9999999999999996d+0) 
term(552) = term(552) * (-7.999999999999999d+0) 
term(553) = term(553) * (-3.9999999999999996d+0) 
term(554) = term(554) * (7.999999999999999d+0) 
term(555) = term(555) * (-1.9999999999999998d+0) 
term(556) = term(556) * (2.0d+0) 
term(557) = term(557) * (-1.9999999999999998d+0) 
term(558) = term(558) * (2.0d+0) 
term(559) = term(559) * (3.9999999999999996d+0) 
term(560) = term(560) * (-4.0d+0) 
term(562) = term(562) * (-1.0d+0) 
term(564) = term(564) * (-1.0d+0) 
term(565) = term(565) * (-1.9999999999999998d+0) 
term(566) = term(566) * (2.0d+0) 
term(567) = term(567) * (-7.999999999999999d+0) 
term(568) = term(568) * (8.0d+0) 
term(569) = term(569) * (7.999999999999999d+0) 
term(570) = term(570) * (-8.0d+0) 
term(571) = term(571) * (3.9999999999999996d+0) 
term(572) = term(572) * (-4.0d+0) 
term(573) = term(573) * (-3.9999999999999996d+0) 
term(574) = term(574) * (4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(575) = term(575) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,i,j,p,l,k)
term(576) = term(576) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,p,i,j,l,k)
term(577) = term(577) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,j,p,l,k)
term(578) = term(578) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,p,j,l,k)
term(579) = term(579) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,i,p,j,l,k)
term(580) = term(580) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,i,j,p,l,k)
term(581) = term(581) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,p,i,j,l,k)
term(582) = term(582) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,j,p,l,k)
term(583) = term(583) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,p,j,l,k)
term(584) = term(584) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,i,p,j,l,k)
end do 
end do 
end do 
end do 

term(575) = term(575) * (0.16666666666666666d+0) 
term(576) = term(576) * (0.16666666666666666d+0) 
term(577) = term(577) * (-0.3333333333333333d+0) 
term(578) = term(578) * (0.16666666666666666d+0) 
term(579) = term(579) * (0.16666666666666666d+0) 
term(580) = term(580) * (0.3333333333333333d+0) 
term(581) = term(581) * (0.3333333333333333d+0) 
term(582) = term(582) * (-0.6666666666666666d+0) 
term(583) = term(583) * (0.3333333333333333d+0) 
term(584) = term(584) * (0.3333333333333333d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(585) = term(585) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,j,i,p,l,k)
term(586) = term(586) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,j,i,p,l,k)
term(587) = term(587) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,j,p,i,l,k)
term(588) = term(588) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,p,j,i,l,k)
term(589) = term(589) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,j,i,p,l,k)
term(590) = term(590) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,j,i,p,l,k)
term(591) = term(591) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,j,p,i,l,k)
term(592) = term(592) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,p,j,i,l,k)
end do 
end do 
end do 
end do 

term(585) = term(585) * (-0.3333333333333333d+0) 
term(586) = term(586) * (0.16666666666666666d+0) 
term(587) = term(587) * (-0.3333333333333333d+0) 
term(588) = term(588) * (0.16666666666666666d+0) 
term(589) = term(589) * (-0.6666666666666666d+0) 
term(590) = term(590) * (0.3333333333333333d+0) 
term(591) = term(591) * (-0.6666666666666666d+0) 
term(592) = term(592) * (0.3333333333333333d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(593) = term(593) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_34_so_pt2(c,a,p,i)
term(594) = term(594) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_35_so_pt2(c,a,p,i)
term(595) = term(595) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_39_so_pt2(c,a,p,k)
term(596) = term(596) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_38_so_pt2(c,a,p,i)
term(597) = term(597) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_34_so_pt2(c,a,p,k)
term(598) = term(598) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_35_so_pt2(c,a,p,k)
term(599) = term(599) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_34_so_pt2(c,a,p,i)
term(600) = term(600) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_35_so_pt2(c,a,p,i)
term(601) = term(601) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_38_so_pt2(c,a,p,k)
term(602) = term(602) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_39_so_pt2(c,a,p,k)
term(603) = term(603) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_38_so_pt2(c,a,p,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(593) = term(593) * (0.5d+0) 
term(594) = term(594) * (-1.0d+0) 
term(595) = term(595) * (0.5d+0) 
term(596) = term(596) * (0.5d+0) 
term(597) = term(597) * (-2.0d+0) 
term(598) = term(598) * (4.0d+0) 
term(599) = term(599) * (2.0d+0) 
term(600) = term(600) * (-4.0d+0) 
term(601) = term(601) * (-2.0d+0) 
term(602) = term(602) * (2.0d+0) 
term(603) = term(603) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(604) = term(604) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,l,p,k,i,j)
term(605) = term(605) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,l,p,k,i,j)
term(606) = term(606) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,p,l,k,i,j)
term(607) = term(607) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,p,l,k,i,j)
term(608) = term(608) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,p,l,k,i,j)
term(609) = term(609) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,k,l,p,i,j)
term(610) = term(610) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,p,l,k,i,j)
term(611) = term(611) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,k,l,p,i,j)
term(612) = term(612) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,k,l,p,i,j)
term(613) = term(613) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,k,l,p,i,j)
term(614) = term(614) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,k,l,p,i,j)
term(615) = term(615) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,k,l,p,i,j)
term(616) = term(616) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,p,l,k,i,j)
term(617) = term(617) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,p,l,k,i,j)
term(618) = term(618) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,l,k,p,i,j)
term(619) = term(619) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,l,k,p,i,j)
term(620) = term(620) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,l,p,k,i,j)
term(621) = term(621) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,l,k,p,i,j)
term(622) = term(622) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,l,p,k,i,j)
term(623) = term(623) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,l,k,p,i,j)
term(624) = term(624) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,l,k,p,i,j)
term(625) = term(625) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,l,k,p,i,j)
term(626) = term(626) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,l,p,k,i,j)
term(627) = term(627) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,l,p,k,i,j)
term(628) = term(628) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,l,p,k,i,j)
term(629) = term(629) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,p,l,k,i,j)
term(630) = term(630) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,p,l,k,i,j)
term(631) = term(631) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,k,l,p,i,j)
term(632) = term(632) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,k,l,p,i,j)
term(633) = term(633) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,k,l,p,i,j)
term(634) = term(634) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,p,l,k,i,j)
term(635) = term(635) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,l,k,p,i,j)
term(636) = term(636) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,l,p,k,i,j)
term(637) = term(637) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,l,k,p,i,j)
term(638) = term(638) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,l,k,p,i,j)
term(639) = term(639) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,l,p,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(604) = term(604) * (-0.3333333333333333d+0) 
term(605) = term(605) * (0.6666666666666666d+0) 
term(606) = term(606) * (0.6666666666666666d+0) 
term(607) = term(607) * (-0.3333333333333333d+0) 
term(608) = term(608) * (0.16666666666666666d+0) 
term(609) = term(609) * (-0.3333333333333333d+0) 
term(610) = term(610) * (-0.3333333333333333d+0) 
term(611) = term(611) * (0.6666666666666666d+0) 
term(612) = term(612) * (0.16666666666666666d+0) 
term(613) = term(613) * (-0.3333333333333333d+0) 
term(614) = term(614) * (0.16666666666666666d+0) 
term(615) = term(615) * (-0.3333333333333333d+0) 
term(616) = term(616) * (-0.3333333333333333d+0) 
term(617) = term(617) * (0.16666666666666666d+0) 
term(618) = term(618) * (0.16666666666666666d+0) 
term(619) = term(619) * (-0.3333333333333333d+0) 
term(620) = term(620) * (0.16666666666666666d+0) 
term(621) = term(621) * (-0.3333333333333333d+0) 
term(622) = term(622) * (-0.3333333333333333d+0) 
term(623) = term(623) * (0.6666666666666666d+0) 
term(624) = term(624) * (0.16666666666666666d+0) 
term(625) = term(625) * (-0.3333333333333333d+0) 
term(626) = term(626) * (-0.3333333333333333d+0) 
term(627) = term(627) * (0.16666666666666666d+0) 
term(628) = term(628) * (-1.3333333333333333d+0) 
term(629) = term(629) * (1.3333333333333333d+0) 
term(630) = term(630) * (0.6666666666666666d+0) 
term(631) = term(631) * (-1.3333333333333333d+0) 
term(632) = term(632) * (0.6666666666666666d+0) 
term(633) = term(633) * (-0.6666666666666666d+0) 
term(634) = term(634) * (-0.6666666666666666d+0) 
term(635) = term(635) * (0.6666666666666666d+0) 
term(636) = term(636) * (0.6666666666666666d+0) 
term(637) = term(637) * (-1.3333333333333333d+0) 
term(638) = term(638) * (0.6666666666666666d+0) 
term(639) = term(639) * (-0.6666666666666666d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(640) = term(640) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,p,i,j,k,l)
term(641) = term(641) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,j,p,k,l)
term(642) = term(642) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,p,j,k,l)
term(643) = term(643) + wm_interm_73_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,p,i,j,k,l)
term(644) = term(644) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_75_so_pt2(q,p,i,j,k,l)
term(645) = term(645) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,j,p,k,l)
term(646) = term(646) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_78_so_pt2(q,i,p,j,k,l)
term(647) = term(647) + wm_interm_102_so_pt2(i,j,k,l) * wm_interm_81_so_pt2(q,p,i,j,k,l)
end do 
end do 
end do 
end do 

term(640) = term(640) * (-0.3333333333333333d+0) 
term(641) = term(641) * (0.16666666666666666d+0) 
term(642) = term(642) * (-0.3333333333333333d+0) 
term(643) = term(643) * (0.16666666666666666d+0) 
term(644) = term(644) * (-0.6666666666666666d+0) 
term(645) = term(645) * (0.3333333333333333d+0) 
term(646) = term(646) * (-0.6666666666666666d+0) 
term(647) = term(647) * (0.3333333333333333d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(648) = term(648) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,l,p,k,j,i)
term(649) = term(649) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,p,l,k,j,i)
term(650) = term(650) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,p,l,k,j,i)
term(651) = term(651) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,k,l,p,j,i)
term(652) = term(652) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,k,l,p,j,i)
term(653) = term(653) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,k,l,p,j,i)
term(654) = term(654) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,p,l,k,j,i)
term(655) = term(655) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,l,k,p,j,i)
term(656) = term(656) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,l,p,k,j,i)
term(657) = term(657) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,l,k,p,j,i)
term(658) = term(658) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,l,k,p,j,i)
term(659) = term(659) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,l,p,k,j,i)
term(660) = term(660) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,l,p,k,j,i)
term(661) = term(661) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,p,l,k,j,i)
term(662) = term(662) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,p,l,k,j,i)
term(663) = term(663) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,k,l,p,j,i)
term(664) = term(664) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,k,l,p,j,i)
term(665) = term(665) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,k,l,p,j,i)
term(666) = term(666) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,p,l,k,j,i)
term(667) = term(667) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,l,k,p,j,i)
term(668) = term(668) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,l,p,k,j,i)
term(669) = term(669) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,l,k,p,j,i)
term(670) = term(670) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,l,k,p,j,i)
term(671) = term(671) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,l,p,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(648) = term(648) * (-0.3333333333333333d+0) 
term(649) = term(649) * (-0.3333333333333333d+0) 
term(650) = term(650) * (0.16666666666666666d+0) 
term(651) = term(651) * (-0.3333333333333333d+0) 
term(652) = term(652) * (0.16666666666666666d+0) 
term(653) = term(653) * (0.16666666666666666d+0) 
term(654) = term(654) * (0.16666666666666666d+0) 
term(655) = term(655) * (0.16666666666666666d+0) 
term(656) = term(656) * (0.16666666666666666d+0) 
term(657) = term(657) * (-0.3333333333333333d+0) 
term(658) = term(658) * (0.16666666666666666d+0) 
term(659) = term(659) * (0.16666666666666666d+0) 
term(660) = term(660) * (1.3333333333333333d+0) 
term(661) = term(661) * (-1.3333333333333333d+0) 
term(662) = term(662) * (-0.6666666666666666d+0) 
term(663) = term(663) * (1.3333333333333333d+0) 
term(664) = term(664) * (0.6666666666666666d+0) 
term(665) = term(665) * (-0.6666666666666666d+0) 
term(666) = term(666) * (0.6666666666666666d+0) 
term(667) = term(667) * (-0.6666666666666666d+0) 
term(668) = term(668) * (-0.6666666666666666d+0) 
term(669) = term(669) * (1.3333333333333333d+0) 
term(670) = term(670) * (-0.6666666666666666d+0) 
term(671) = term(671) * (0.6666666666666666d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(672) = term(672) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_51_so_pt2(b,c,j,p)
term(673) = term(673) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_52_so_pt2(b,c,j,p)
term(674) = term(674) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_54_so_pt2(b,c,j,p)
term(675) = term(675) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_50_so_pt2(b,c,j,p)
term(676) = term(676) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_59_so_pt2(b,c,j,p)
term(677) = term(677) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_53_so_pt2(b,c,j,p)
term(678) = term(678) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_49_so_pt2(b,c,j,p)
term(679) = term(679) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_53_so_pt2(b,c,j,p)
term(680) = term(680) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_59_so_pt2(b,c,j,p)
term(681) = term(681) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_51_so_pt2(b,c,i,p)
term(682) = term(682) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_52_so_pt2(b,c,i,p)
term(683) = term(683) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_54_so_pt2(b,c,i,p)
term(684) = term(684) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_50_so_pt2(b,c,i,p)
term(685) = term(685) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_59_so_pt2(b,c,i,p)
term(686) = term(686) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_53_so_pt2(b,c,i,p)
term(687) = term(687) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_49_so_pt2(b,c,k,p)
term(688) = term(688) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_53_so_pt2(b,c,k,p)
term(689) = term(689) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_59_so_pt2(b,c,k,p)
term(690) = term(690) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_49_so_pt2(a,c,j,p)
term(691) = term(691) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_53_so_pt2(a,c,j,p)
term(692) = term(692) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_59_so_pt2(a,c,j,p)
term(693) = term(693) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_51_so_pt2(a,c,i,p)
term(694) = term(694) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_52_so_pt2(a,c,i,p)
term(695) = term(695) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_50_so_pt2(a,c,i,p)
term(696) = term(696) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_49_so_pt2(a,c,k,p)
term(697) = term(697) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_54_so_pt2(a,c,i,p)
term(698) = term(698) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_59_so_pt2(a,c,i,p)
term(699) = term(699) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_53_so_pt2(a,c,i,p)
term(700) = term(700) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_53_so_pt2(a,c,k,p)
term(701) = term(701) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_59_so_pt2(a,c,k,p)
term(702) = term(702) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_49_so_pt2(a,c,j,p)
term(703) = term(703) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_53_so_pt2(a,c,j,p)
term(704) = term(704) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_59_so_pt2(a,c,j,p)
term(705) = term(705) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_34_so_pt2(c,b,p,k)
term(706) = term(706) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_35_so_pt2(c,b,p,k)
term(707) = term(707) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_34_so_pt2(c,b,p,i)
term(708) = term(708) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_35_so_pt2(c,b,p,i)
term(709) = term(709) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_90_so_pt2(b,c,j,p)
term(710) = term(710) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_91_so_pt2(b,c,j,p)
term(711) = term(711) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_89_so_pt2(b,c,j,p)
term(712) = term(712) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_88_so_pt2(b,c,j,p)
term(713) = term(713) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_88_so_pt2(b,c,j,p)
term(714) = term(714) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_89_so_pt2(b,c,j,p)
term(715) = term(715) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_90_so_pt2(b,c,i,p)
term(716) = term(716) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_91_so_pt2(b,c,i,p)
term(717) = term(717) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_89_so_pt2(b,c,i,p)
term(718) = term(718) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_88_so_pt2(b,c,i,p)
term(719) = term(719) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_88_so_pt2(b,c,k,p)
term(720) = term(720) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_89_so_pt2(b,c,k,p)
term(721) = term(721) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_88_so_pt2(a,c,j,p)
term(722) = term(722) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_89_so_pt2(a,c,j,p)
term(723) = term(723) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_90_so_pt2(a,c,i,p)
term(724) = term(724) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_91_so_pt2(a,c,i,p)
term(725) = term(725) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_89_so_pt2(a,c,i,p)
term(726) = term(726) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_88_so_pt2(a,c,k,p)
term(727) = term(727) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_88_so_pt2(a,c,i,p)
term(728) = term(728) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_89_so_pt2(a,c,k,p)
term(729) = term(729) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_39_so_pt2(c,b,p,k)
term(730) = term(730) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_38_so_pt2(c,b,p,k)
term(731) = term(731) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_88_so_pt2(a,c,j,p)
term(732) = term(732) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_38_so_pt2(c,b,p,i)
term(733) = term(733) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_89_so_pt2(a,c,j,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(672) = term(672) * (0.5d+0) 
term(673) = term(673) * (-1.0d+0) 
term(674) = term(674) * (0.5d+0) 
term(675) = term(675) * (0.5d+0) 
term(676) = term(676) * (0.5d+0) 
term(677) = term(677) * (-1.0d+0) 
term(678) = term(678) * (0.5d+0) 
term(679) = term(679) * (0.5d+0) 
term(680) = term(680) * (-1.0d+0) 
term(681) = term(681) * (-1.0d+0) 
term(682) = term(682) * (2.0d+0) 
term(683) = term(683) * (-1.0d+0) 
term(684) = term(684) * (-1.0d+0) 
term(685) = term(685) * (-1.0d+0) 
term(686) = term(686) * (2.0d+0) 
term(687) = term(687) * (-1.0d+0) 
term(688) = term(688) * (-1.0d+0) 
term(689) = term(689) * (2.0d+0) 
term(690) = term(690) * (0.5d+0) 
term(691) = term(691) * (0.5d+0) 
term(692) = term(692) * (-1.0d+0) 
term(693) = term(693) * (0.5d+0) 
term(694) = term(694) * (-1.0d+0) 
term(695) = term(695) * (0.5d+0) 
term(696) = term(696) * (0.5d+0) 
term(697) = term(697) * (0.5d+0) 
term(698) = term(698) * (0.5d+0) 
term(699) = term(699) * (-1.0d+0) 
term(700) = term(700) * (0.5d+0) 
term(701) = term(701) * (-1.0d+0) 
term(702) = term(702) * (-1.0d+0) 
term(703) = term(703) * (-1.0d+0) 
term(704) = term(704) * (2.0d+0) 
term(705) = term(705) * (4.0d+0) 
term(706) = term(706) * (-8.0d+0) 
term(707) = term(707) * (-2.0d+0) 
term(708) = term(708) * (4.0d+0) 
term(709) = term(709) * (2.0d+0) 
term(710) = term(710) * (-2.0d+0) 
term(711) = term(711) * (2.0d+0) 
term(712) = term(712) * (-2.0d+0) 
term(713) = term(713) * (2.0d+0) 
term(714) = term(714) * (-2.0d+0) 
term(715) = term(715) * (-4.0d+0) 
term(716) = term(716) * (4.0d+0) 
term(717) = term(717) * (-4.0d+0) 
term(718) = term(718) * (4.0d+0) 
term(719) = term(719) * (-4.0d+0) 
term(720) = term(720) * (4.0d+0) 
term(721) = term(721) * (2.0d+0) 
term(722) = term(722) * (-2.0d+0) 
term(723) = term(723) * (2.0d+0) 
term(724) = term(724) * (-2.0d+0) 
term(725) = term(725) * (2.0d+0) 
term(726) = term(726) * (2.0d+0) 
term(727) = term(727) * (-2.0d+0) 
term(728) = term(728) * (-2.0d+0) 
term(729) = term(729) * (-2.0d+0) 
term(730) = term(730) * (4.0d+0) 
term(731) = term(731) * (-4.0d+0) 
term(732) = term(732) * (-2.0d+0) 
term(733) = term(733) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(734) = term(734) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,p,j,i)
term(735) = term(735) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,p,j,i)
term(736) = term(736) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,p,j,i)
term(737) = term(737) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,p,j,i)
term(738) = term(738) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,p,j,i)
term(739) = term(739) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,p,j,i)
term(740) = term(740) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_41_so_pt2(a,p,j,i)
term(741) = term(741) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_41_so_pt2(a,p,j,i)
term(742) = term(742) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,p,j,i)
term(743) = term(743) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,p,j,i)
term(744) = term(744) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,p,j,i)
term(745) = term(745) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,p,j,i)
term(746) = term(746) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,j,p,i)
term(747) = term(747) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,j,p,i)
term(748) = term(748) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,j,p,i)
term(749) = term(749) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,j,p,i)
term(750) = term(750) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,j,p,i)
term(751) = term(751) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,j,p,i)
term(752) = term(752) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,p,j,i)
term(753) = term(753) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,p,j,i)
term(754) = term(754) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,j,p,i)
term(755) = term(755) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,j,p,i)
term(756) = term(756) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,p,j,i)
term(757) = term(757) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,p,j,i)
term(758) = term(758) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,j,p,i)
term(759) = term(759) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,j,p,i)
term(760) = term(760) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,p,j,i)
term(761) = term(761) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,p,j,i)
term(762) = term(762) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,j,p,i)
term(763) = term(763) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,j,p,i)
term(764) = term(764) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,j,p,i)
term(765) = term(765) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,j,p,i)
term(766) = term(766) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_42_so_pt2(a,j,p,i)
term(767) = term(767) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_42_so_pt2(a,j,p,i)
term(768) = term(768) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,j,p,i)
term(769) = term(769) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,j,p,i)
term(770) = term(770) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_41_so_pt2(a,p,j,i)
term(771) = term(771) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,p,j,i)
term(772) = term(772) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,p,j,i)
term(773) = term(773) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_41_so_pt2(a,p,j,i)
term(774) = term(774) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,p,j,i)
term(775) = term(775) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_42_so_pt2(a,p,j,i)
term(776) = term(776) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,p,j,i)
term(777) = term(777) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,p,j,i)
term(778) = term(778) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,p,j,i)
term(779) = term(779) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,p,j,i)
term(780) = term(780) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,p,j,i)
term(781) = term(781) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,p,j,i)
term(782) = term(782) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,p,j,i)
term(783) = term(783) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,p,j,i)
term(784) = term(784) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,p,j,i)
term(785) = term(785) + wm_interm_51_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,p,i,j)
term(786) = term(786) + wm_interm_52_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,p,i,j)
term(787) = term(787) + wm_interm_50_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,p,i,j)
term(788) = term(788) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,p,i,j)
term(789) = term(789) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,p,i,j)
term(790) = term(790) + wm_interm_54_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,p,i,j)
term(791) = term(791) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,i,p,j)
term(792) = term(792) + wm_interm_53_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,i,p,j)
term(793) = term(793) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_69_so_pt2(a,i,p,j)
term(794) = term(794) + wm_interm_51_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,i,p,j)
term(795) = term(795) + wm_interm_52_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,i,p,j)
term(796) = term(796) + wm_interm_50_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,i,p,j)
term(797) = term(797) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,i,p,j)
term(798) = term(798) + wm_interm_51_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,p,i,j)
term(799) = term(799) + wm_interm_52_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,p,i,j)
term(800) = term(800) + wm_interm_50_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,p,i,j)
term(801) = term(801) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,p,i,j)
term(802) = term(802) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,i,p,j)
term(803) = term(803) + wm_interm_54_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,i,p,j)
term(804) = term(804) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,p,i,j)
term(805) = term(805) + wm_interm_54_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,p,i,j)
term(806) = term(806) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,p,i,j)
term(807) = term(807) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,i,p,j)
term(808) = term(808) + wm_interm_53_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,p,i,j)
term(809) = term(809) + wm_interm_53_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,i,p,j)
term(810) = term(810) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_74_so_pt2(a,p,i,j)
term(811) = term(811) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_76_so_pt2(a,i,p,j)
term(812) = term(812) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_40_so_pt2(a,j,p,i)
term(813) = term(813) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_41_so_pt2(a,j,p,i)
term(814) = term(814) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_46_so_pt2(a,j,p,i)
term(815) = term(815) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_47_so_pt2(a,j,p,i)
term(816) = term(816) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,j,p,i)
term(817) = term(817) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_42_so_pt2(a,j,p,i)
term(818) = term(818) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_42_so_pt2(a,j,p,i)
term(819) = term(819) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_55_so_pt2(a,j,p,i)
term(820) = term(820) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_48_so_pt2(a,j,p,i)
term(821) = term(821) + wm_interm_51_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,p,i,j)
term(822) = term(822) + wm_interm_52_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,p,i,j)
term(823) = term(823) + wm_interm_50_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,p,i,j)
term(824) = term(824) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,p,i,j)
term(825) = term(825) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,p,i,j)
term(826) = term(826) + wm_interm_54_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,p,i,j)
term(827) = term(827) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,i,p,j)
term(828) = term(828) + wm_interm_53_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,i,p,j)
term(829) = term(829) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_77_so_pt2(a,i,p,j)
term(830) = term(830) + wm_interm_51_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,i,p,j)
term(831) = term(831) + wm_interm_52_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,i,p,j)
term(832) = term(832) + wm_interm_54_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,i,p,j)
term(833) = term(833) + wm_interm_53_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,p,i,j)
term(834) = term(834) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,p,i,j)
term(835) = term(835) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,i,p,j)
term(836) = term(836) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,p,i,j)
term(837) = term(837) + wm_interm_50_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,i,p,j)
term(838) = term(838) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_79_so_pt2(a,i,p,j)
term(839) = term(839) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,j,p,i)
term(840) = term(840) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_56_so_pt2(a,j,p,i)
term(841) = term(841) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,j,p,i)
term(842) = term(842) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_57_so_pt2(a,j,p,i)
term(843) = term(843) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,j,p,i)
term(844) = term(844) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_58_so_pt2(a,j,p,i)
term(845) = term(845) + wm_interm_51_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,i,p,j)
term(846) = term(846) + wm_interm_52_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,i,p,j)
term(847) = term(847) + wm_interm_54_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,i,p,j)
term(848) = term(848) + wm_interm_53_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,p,i,j)
term(849) = term(849) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,p,i,j)
term(850) = term(850) + wm_interm_59_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,i,p,j)
term(851) = term(851) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,p,i,j)
term(852) = term(852) + wm_interm_50_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,i,p,j)
term(853) = term(853) + wm_interm_49_so_pt2(a,q,i,j) * wm_interm_80_so_pt2(a,i,p,j)
term(854) = term(854) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,p,j,i)
term(855) = term(855) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,p,j,i)
term(856) = term(856) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,p,j,i)
term(857) = term(857) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,p,j,i)
term(858) = term(858) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,p,j,i)
term(859) = term(859) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,p,j,i)
term(860) = term(860) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,p,j,i)
term(861) = term(861) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,p,j,i)
term(862) = term(862) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,j,p,i)
term(863) = term(863) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,j,p,i)
term(864) = term(864) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,j,p,i)
term(865) = term(865) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,j,p,i)
term(866) = term(866) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,p,j,i)
term(867) = term(867) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,p,j,i)
term(868) = term(868) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,j,p,i)
term(869) = term(869) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,j,p,i)
term(870) = term(870) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,p,j,i)
term(871) = term(871) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,p,j,i)
term(872) = term(872) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,j,p,i)
term(873) = term(873) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,j,p,i)
term(874) = term(874) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,j,p,i)
term(875) = term(875) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,j,p,i)
term(876) = term(876) + wm_interm_34_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,j,p,i)
term(877) = term(877) + wm_interm_35_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,j,p,i)
term(878) = term(878) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,p,j,i)
term(879) = term(879) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,p,j,i)
term(880) = term(880) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,p,j,i)
term(881) = term(881) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,p,j,i)
term(882) = term(882) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,p,j,i)
term(883) = term(883) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,p,j,i)
term(884) = term(884) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,p,j,i)
term(885) = term(885) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,p,j,i)
term(886) = term(886) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,p,j,i)
term(887) = term(887) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,p,j,i)
term(888) = term(888) + wm_interm_69_so_pt2(a,p,i,j) * wm_interm_90_so_pt2(a,q,i,j)
term(889) = term(889) + wm_interm_69_so_pt2(a,p,i,j) * wm_interm_91_so_pt2(a,q,i,j)
term(890) = term(890) + wm_interm_69_so_pt2(a,p,i,j) * wm_interm_89_so_pt2(a,q,i,j)
term(891) = term(891) + wm_interm_69_so_pt2(a,p,i,j) * wm_interm_88_so_pt2(a,q,i,j)
term(892) = term(892) + wm_interm_69_so_pt2(a,i,p,j) * wm_interm_88_so_pt2(a,q,i,j)
term(893) = term(893) + wm_interm_69_so_pt2(a,i,p,j) * wm_interm_89_so_pt2(a,q,i,j)
term(894) = term(894) + wm_interm_74_so_pt2(a,i,p,j) * wm_interm_90_so_pt2(a,q,i,j)
term(895) = term(895) + wm_interm_74_so_pt2(a,i,p,j) * wm_interm_91_so_pt2(a,q,i,j)
term(896) = term(896) + wm_interm_74_so_pt2(a,i,p,j) * wm_interm_89_so_pt2(a,q,i,j)
term(897) = term(897) + wm_interm_74_so_pt2(a,i,p,j) * wm_interm_88_so_pt2(a,q,i,j)
term(898) = term(898) + wm_interm_76_so_pt2(a,p,i,j) * wm_interm_90_so_pt2(a,q,i,j)
term(899) = term(899) + wm_interm_76_so_pt2(a,p,i,j) * wm_interm_91_so_pt2(a,q,i,j)
term(900) = term(900) + wm_interm_76_so_pt2(a,p,i,j) * wm_interm_89_so_pt2(a,q,i,j)
term(901) = term(901) + wm_interm_76_so_pt2(a,p,i,j) * wm_interm_88_so_pt2(a,q,i,j)
term(902) = term(902) + wm_interm_74_so_pt2(a,p,i,j) * wm_interm_88_so_pt2(a,q,i,j)
term(903) = term(903) + wm_interm_76_so_pt2(a,i,p,j) * wm_interm_88_so_pt2(a,q,i,j)
term(904) = term(904) + wm_interm_74_so_pt2(a,p,i,j) * wm_interm_89_so_pt2(a,q,i,j)
term(905) = term(905) + wm_interm_76_so_pt2(a,i,p,j) * wm_interm_89_so_pt2(a,q,i,j)
term(906) = term(906) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,j,p,i)
term(907) = term(907) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,j,p,i)
term(908) = term(908) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_86_so_pt2(a,j,p,i)
term(909) = term(909) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_87_so_pt2(a,j,p,i)
term(910) = term(910) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_82_so_pt2(a,j,p,i)
term(911) = term(911) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_83_so_pt2(a,j,p,i)
term(912) = term(912) + wm_interm_77_so_pt2(a,p,i,j) * wm_interm_90_so_pt2(a,q,i,j)
term(913) = term(913) + wm_interm_77_so_pt2(a,p,i,j) * wm_interm_91_so_pt2(a,q,i,j)
term(914) = term(914) + wm_interm_77_so_pt2(a,p,i,j) * wm_interm_89_so_pt2(a,q,i,j)
term(915) = term(915) + wm_interm_77_so_pt2(a,p,i,j) * wm_interm_88_so_pt2(a,q,i,j)
term(916) = term(916) + wm_interm_77_so_pt2(a,i,p,j) * wm_interm_88_so_pt2(a,q,i,j)
term(917) = term(917) + wm_interm_77_so_pt2(a,i,p,j) * wm_interm_89_so_pt2(a,q,i,j)
term(918) = term(918) + wm_interm_79_so_pt2(a,i,p,j) * wm_interm_90_so_pt2(a,q,i,j)
term(919) = term(919) + wm_interm_79_so_pt2(a,i,p,j) * wm_interm_91_so_pt2(a,q,i,j)
term(920) = term(920) + wm_interm_79_so_pt2(a,p,i,j) * wm_interm_88_so_pt2(a,q,i,j)
term(921) = term(921) + wm_interm_79_so_pt2(a,p,i,j) * wm_interm_89_so_pt2(a,q,i,j)
term(922) = term(922) + wm_interm_79_so_pt2(a,i,p,j) * wm_interm_89_so_pt2(a,q,i,j)
term(923) = term(923) + wm_interm_79_so_pt2(a,i,p,j) * wm_interm_88_so_pt2(a,q,i,j)
term(924) = term(924) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,j,p,i)
term(925) = term(925) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_92_so_pt2(a,j,p,i)
term(926) = term(926) + wm_interm_39_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,j,p,i)
term(927) = term(927) + wm_interm_38_so_pt2(q,a,i,j) * wm_interm_93_so_pt2(a,j,p,i)
term(928) = term(928) + wm_interm_80_so_pt2(a,i,p,j) * wm_interm_90_so_pt2(a,q,i,j)
term(929) = term(929) + wm_interm_80_so_pt2(a,i,p,j) * wm_interm_91_so_pt2(a,q,i,j)
term(930) = term(930) + wm_interm_80_so_pt2(a,p,i,j) * wm_interm_88_so_pt2(a,q,i,j)
term(931) = term(931) + wm_interm_80_so_pt2(a,p,i,j) * wm_interm_89_so_pt2(a,q,i,j)
term(932) = term(932) + wm_interm_80_so_pt2(a,i,p,j) * wm_interm_89_so_pt2(a,q,i,j)
term(933) = term(933) + wm_interm_80_so_pt2(a,i,p,j) * wm_interm_88_so_pt2(a,q,i,j)
end do 
end do 
end do 

term(734) = term(734) * (-1.0d+0) 
term(735) = term(735) * (2.0d+0) 
term(736) = term(736) * (2.0d+0) 
term(737) = term(737) * (-4.0d+0) 
term(738) = term(738) * (-1.0d+0) 
term(739) = term(739) * (2.0d+0) 
term(740) = term(740) * (0.5d+0) 
term(741) = term(741) * (-1.0d+0) 
term(742) = term(742) * (-1.0d+0) 
term(743) = term(743) * (2.0d+0) 
term(744) = term(744) * (0.5d+0) 
term(745) = term(745) * (-1.0d+0) 
term(746) = term(746) * (0.5d+0) 
term(747) = term(747) * (-1.0d+0) 
term(748) = term(748) * (0.5d+0) 
term(749) = term(749) * (-1.0d+0) 
term(750) = term(750) * (-1.0d+0) 
term(751) = term(751) * (2.0d+0) 
term(752) = term(752) * (0.5d+0) 
term(753) = term(753) * (-1.0d+0) 
term(754) = term(754) * (-1.0d+0) 
term(755) = term(755) * (2.0d+0) 
term(756) = term(756) * (0.5d+0) 
term(757) = term(757) * (-1.0d+0) 
term(758) = term(758) * (-1.0d+0) 
term(759) = term(759) * (2.0d+0) 
term(760) = term(760) * (-1.0d+0) 
term(761) = term(761) * (2.0d+0) 
term(762) = term(762) * (2.0d+0) 
term(763) = term(763) * (-4.0d+0) 
term(764) = term(764) * (0.5d+0) 
term(765) = term(765) * (-1.0d+0) 
term(766) = term(766) * (0.5d+0) 
term(767) = term(767) * (-1.0d+0) 
term(768) = term(768) * (-1.0d+0) 
term(769) = term(769) * (2.0d+0) 
term(770) = term(770) * (0.5d+0) 
term(771) = term(771) * (-1.0d+0) 
term(772) = term(772) * (0.5d+0) 
term(773) = term(773) * (-1.0d+0) 
term(774) = term(774) * (0.5d+0) 
term(775) = term(775) * (0.5d+0) 
term(776) = term(776) * (0.5d+0) 
term(777) = term(777) * (0.5d+0) 
term(778) = term(778) * (-1.0d+0) 
term(779) = term(779) * (0.5d+0) 
term(780) = term(780) * (-1.0d+0) 
term(781) = term(781) * (-1.0d+0) 
term(782) = term(782) * (2.0d+0) 
term(783) = term(783) * (0.5d+0) 
term(784) = term(784) * (-1.0d+0) 
term(785) = term(785) * (0.5d+0) 
term(786) = term(786) * (-1.0d+0) 
term(787) = term(787) * (0.5d+0) 
term(788) = term(788) * (-1.0d+0) 
term(789) = term(789) * (0.5d+0) 
term(790) = term(790) * (0.5d+0) 
term(791) = term(791) * (0.5d+0) 
term(792) = term(792) * (0.5d+0) 
term(793) = term(793) * (-1.0d+0) 
term(794) = term(794) * (0.5d+0) 
term(795) = term(795) * (-1.0d+0) 
term(796) = term(796) * (0.5d+0) 
term(797) = term(797) * (-1.0d+0) 
term(798) = term(798) * (-1.0d+0) 
term(799) = term(799) * (2.0d+0) 
term(800) = term(800) * (-1.0d+0) 
term(801) = term(801) * (2.0d+0) 
term(802) = term(802) * (0.5d+0) 
term(803) = term(803) * (0.5d+0) 
term(804) = term(804) * (-1.0d+0) 
term(805) = term(805) * (-1.0d+0) 
term(806) = term(806) * (0.5d+0) 
term(807) = term(807) * (-1.0d+0) 
term(808) = term(808) * (0.5d+0) 
term(809) = term(809) * (-1.0d+0) 
term(810) = term(810) * (-1.0d+0) 
term(811) = term(811) * (2.0d+0) 
term(812) = term(812) * (0.5d+0) 
term(813) = term(813) * (0.5d+0) 
term(814) = term(814) * (0.5d+0) 
term(815) = term(815) * (-1.0d+0) 
term(816) = term(816) * (0.5d+0) 
term(817) = term(817) * (-1.0d+0) 
term(818) = term(818) * (0.5d+0) 
term(819) = term(819) * (-1.0d+0) 
term(820) = term(820) * (0.5d+0) 
term(821) = term(821) * (0.5d+0) 
term(822) = term(822) * (-1.0d+0) 
term(823) = term(823) * (0.5d+0) 
term(824) = term(824) * (-1.0d+0) 
term(825) = term(825) * (0.5d+0) 
term(826) = term(826) * (0.5d+0) 
term(827) = term(827) * (0.5d+0) 
term(828) = term(828) * (0.5d+0) 
term(829) = term(829) * (-1.0d+0) 
term(830) = term(830) * (-1.0d+0) 
term(831) = term(831) * (2.0d+0) 
term(832) = term(832) * (-1.0d+0) 
term(833) = term(833) * (-1.0d+0) 
term(834) = term(834) * (2.0d+0) 
term(835) = term(835) * (-1.0d+0) 
term(836) = term(836) * (-1.0d+0) 
term(837) = term(837) * (-1.0d+0) 
term(838) = term(838) * (2.0d+0) 
term(839) = term(839) * (0.5d+0) 
term(840) = term(840) * (-1.0d+0) 
term(841) = term(841) * (0.5d+0) 
term(842) = term(842) * (-1.0d+0) 
term(843) = term(843) * (-1.0d+0) 
term(844) = term(844) * (2.0d+0) 
term(845) = term(845) * (0.5d+0) 
term(846) = term(846) * (-1.0d+0) 
term(847) = term(847) * (0.5d+0) 
term(848) = term(848) * (0.5d+0) 
term(849) = term(849) * (-1.0d+0) 
term(850) = term(850) * (0.5d+0) 
term(851) = term(851) * (0.5d+0) 
term(852) = term(852) * (0.5d+0) 
term(853) = term(853) * (-1.0d+0) 
term(854) = term(854) * (-4.0d+0) 
term(855) = term(855) * (8.0d+0) 
term(856) = term(856) * (4.0d+0) 
term(857) = term(857) * (-8.0d+0) 
term(858) = term(858) * (2.0d+0) 
term(859) = term(859) * (-4.0d+0) 
term(860) = term(860) * (-2.0d+0) 
term(861) = term(861) * (4.0d+0) 
term(862) = term(862) * (2.0d+0) 
term(863) = term(863) * (-4.0d+0) 
term(864) = term(864) * (-2.0d+0) 
term(865) = term(865) * (4.0d+0) 
term(866) = term(866) * (2.0d+0) 
term(867) = term(867) * (-4.0d+0) 
term(868) = term(868) * (-4.0d+0) 
term(869) = term(869) * (8.0d+0) 
term(870) = term(870) * (-2.0d+0) 
term(871) = term(871) * (4.0d+0) 
term(872) = term(872) * (4.0d+0) 
term(873) = term(873) * (-8.0d+0) 
term(874) = term(874) * (2.0d+0) 
term(875) = term(875) * (-4.0d+0) 
term(876) = term(876) * (-2.0d+0) 
term(877) = term(877) * (4.0d+0) 
term(878) = term(878) * (2.0d+0) 
term(879) = term(879) * (-2.0d+0) 
term(880) = term(880) * (2.0d+0) 
term(881) = term(881) * (-2.0d+0) 
term(882) = term(882) * (2.0d+0) 
term(883) = term(883) * (-2.0d+0) 
term(884) = term(884) * (2.0d+0) 
term(885) = term(885) * (-4.0d+0) 
term(886) = term(886) * (-2.0d+0) 
term(887) = term(887) * (4.0d+0) 
term(888) = term(888) * (2.0d+0) 
term(889) = term(889) * (-2.0d+0) 
term(890) = term(890) * (2.0d+0) 
term(891) = term(891) * (-2.0d+0) 
term(892) = term(892) * (2.0d+0) 
term(893) = term(893) * (-2.0d+0) 
term(894) = term(894) * (2.0d+0) 
term(895) = term(895) * (-2.0d+0) 
term(896) = term(896) * (2.0d+0) 
term(897) = term(897) * (-2.0d+0) 
term(898) = term(898) * (-4.0d+0) 
term(899) = term(899) * (4.0d+0) 
term(900) = term(900) * (-4.0d+0) 
term(901) = term(901) * (4.0d+0) 
term(902) = term(902) * (2.0d+0) 
term(903) = term(903) * (-4.0d+0) 
term(904) = term(904) * (-2.0d+0) 
term(905) = term(905) * (4.0d+0) 
term(906) = term(906) * (2.0d+0) 
term(907) = term(907) * (2.0d+0) 
term(908) = term(908) * (2.0d+0) 
term(909) = term(909) * (-2.0d+0) 
term(910) = term(910) * (-2.0d+0) 
term(911) = term(911) * (-2.0d+0) 
term(912) = term(912) * (2.0d+0) 
term(913) = term(913) * (-2.0d+0) 
term(914) = term(914) * (2.0d+0) 
term(915) = term(915) * (-2.0d+0) 
term(916) = term(916) * (2.0d+0) 
term(917) = term(917) * (-2.0d+0) 
term(918) = term(918) * (-4.0d+0) 
term(919) = term(919) * (4.0d+0) 
term(920) = term(920) * (-4.0d+0) 
term(921) = term(921) * (4.0d+0) 
term(922) = term(922) * (-4.0d+0) 
term(923) = term(923) * (4.0d+0) 
term(924) = term(924) * (2.0d+0) 
term(925) = term(925) * (-4.0d+0) 
term(926) = term(926) * (-2.0d+0) 
term(927) = term(927) * (4.0d+0) 
term(928) = term(928) * (2.0d+0) 
term(929) = term(929) * (-2.0d+0) 
term(930) = term(930) * (2.0d+0) 
term(931) = term(931) * (-2.0d+0) 
term(932) = term(932) * (2.0d+0) 
term(933) = term(933) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(934) = term(934) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,p,k,l,j,i)
term(935) = term(935) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,k,p,l,j,i)
term(936) = term(936) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,k,p,l,j,i)
term(937) = term(937) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,k,p,l,j,i)
term(938) = term(938) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,p,k,l,j,i)
term(939) = term(939) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,p,k,l,j,i)
term(940) = term(940) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,p,k,l,j,i)
term(941) = term(941) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,k,p,l,j,i)
term(942) = term(942) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,k,p,l,j,i)
term(943) = term(943) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,k,p,l,j,i)
term(944) = term(944) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,p,k,l,j,i)
term(945) = term(945) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,p,k,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(934) = term(934) * (0.16666666666666666d+0) 
term(935) = term(935) * (-0.3333333333333333d+0) 
term(936) = term(936) * (0.16666666666666666d+0) 
term(937) = term(937) * (0.16666666666666666d+0) 
term(938) = term(938) * (-0.3333333333333333d+0) 
term(939) = term(939) * (0.16666666666666666d+0) 
term(940) = term(940) * (0.6666666666666666d+0) 
term(941) = term(941) * (-1.3333333333333333d+0) 
term(942) = term(942) * (0.6666666666666666d+0) 
term(943) = term(943) * (-0.6666666666666666d+0) 
term(944) = term(944) * (-1.3333333333333333d+0) 
term(945) = term(945) * (0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(946) = term(946) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,p,k,l,i,j)
term(947) = term(947) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,p,k,l,i,j)
term(948) = term(948) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_62_so_pt2(b,k,p,l,i,j)
term(949) = term(949) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_63_so_pt2(b,k,p,l,i,j)
term(950) = term(950) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,k,p,l,i,j)
term(951) = term(951) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,k,p,l,i,j)
term(952) = term(952) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,k,p,l,i,j)
term(953) = term(953) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,k,p,l,i,j)
term(954) = term(954) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_65_so_pt2(b,p,k,l,i,j)
term(955) = term(955) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_66_so_pt2(b,p,k,l,i,j)
term(956) = term(956) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_68_so_pt2(b,p,k,l,i,j)
term(957) = term(957) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_67_so_pt2(b,p,k,l,i,j)
term(958) = term(958) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,p,k,l,i,j)
term(959) = term(959) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_96_so_pt2(b,k,p,l,i,j)
term(960) = term(960) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,k,p,l,i,j)
term(961) = term(961) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,k,p,l,i,j)
term(962) = term(962) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_97_so_pt2(b,p,k,l,i,j)
term(963) = term(963) + s2(a,q,j,i) * t2(a,b,l,k) * wm_interm_98_so_pt2(b,p,k,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(946) = term(946) * (-0.3333333333333333d+0) 
term(947) = term(947) * (0.16666666666666666d+0) 
term(948) = term(948) * (0.6666666666666666d+0) 
term(949) = term(949) * (-0.3333333333333333d+0) 
term(950) = term(950) * (-0.3333333333333333d+0) 
term(951) = term(951) * (0.16666666666666666d+0) 
term(952) = term(952) * (-0.3333333333333333d+0) 
term(953) = term(953) * (0.16666666666666666d+0) 
term(954) = term(954) * (0.6666666666666666d+0) 
term(955) = term(955) * (-0.3333333333333333d+0) 
term(956) = term(956) * (-0.3333333333333333d+0) 
term(957) = term(957) * (0.16666666666666666d+0) 
term(958) = term(958) * (-0.6666666666666666d+0) 
term(959) = term(959) * (1.3333333333333333d+0) 
term(960) = term(960) * (-0.6666666666666666d+0) 
term(961) = term(961) * (0.6666666666666666d+0) 
term(962) = term(962) * (1.3333333333333333d+0) 
term(963) = term(963) * (-0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(964) = term(964) + wm_interm_69_so_pt2(q,i,p,j) * wm_interm_70_so_pt2(i,j)
term(965) = term(965) + wm_interm_69_so_pt2(q,i,p,j) * wm_interm_71_so_pt2(i,j)
term(966) = term(966) + wm_interm_69_so_pt2(q,i,p,j) * wm_interm_72_so_pt2(i,j)
term(967) = term(967) + wm_interm_70_so_pt2(i,j) * wm_interm_74_so_pt2(q,p,i,j)
term(968) = term(968) + wm_interm_71_so_pt2(i,j) * wm_interm_74_so_pt2(q,p,i,j)
term(969) = term(969) + wm_interm_72_so_pt2(i,j) * wm_interm_74_so_pt2(q,p,i,j)
term(970) = term(970) + wm_interm_70_so_pt2(i,j) * wm_interm_76_so_pt2(q,i,p,j)
term(971) = term(971) + wm_interm_71_so_pt2(i,j) * wm_interm_76_so_pt2(q,i,p,j)
term(972) = term(972) + wm_interm_72_so_pt2(i,j) * wm_interm_76_so_pt2(q,i,p,j)
term(973) = term(973) + wm_interm_70_so_pt2(i,j) * wm_interm_77_so_pt2(q,i,p,j)
term(974) = term(974) + wm_interm_71_so_pt2(i,j) * wm_interm_77_so_pt2(q,i,p,j)
term(975) = term(975) + wm_interm_72_so_pt2(i,j) * wm_interm_77_so_pt2(q,i,p,j)
term(976) = term(976) + wm_interm_70_so_pt2(i,j) * wm_interm_79_so_pt2(q,p,i,j)
term(977) = term(977) + wm_interm_71_so_pt2(i,j) * wm_interm_79_so_pt2(q,p,i,j)
term(978) = term(978) + wm_interm_72_so_pt2(i,j) * wm_interm_79_so_pt2(q,p,i,j)
term(979) = term(979) + wm_interm_70_so_pt2(i,j) * wm_interm_80_so_pt2(q,p,i,j)
term(980) = term(980) + wm_interm_71_so_pt2(i,j) * wm_interm_80_so_pt2(q,p,i,j)
term(981) = term(981) + wm_interm_72_so_pt2(i,j) * wm_interm_80_so_pt2(q,p,i,j)
term(982) = term(982) + wm_interm_69_so_pt2(q,i,p,j) * wm_interm_99_so_pt2(i,j)
term(983) = term(983) + wm_interm_100_so_pt2(i,j) * wm_interm_69_so_pt2(q,i,p,j)
term(984) = term(984) + wm_interm_101_so_pt2(i,j) * wm_interm_69_so_pt2(q,i,p,j)
term(985) = term(985) + wm_interm_74_so_pt2(q,p,i,j) * wm_interm_99_so_pt2(i,j)
term(986) = term(986) + wm_interm_100_so_pt2(i,j) * wm_interm_74_so_pt2(q,p,i,j)
term(987) = term(987) + wm_interm_101_so_pt2(i,j) * wm_interm_74_so_pt2(q,p,i,j)
term(988) = term(988) + wm_interm_76_so_pt2(q,i,p,j) * wm_interm_99_so_pt2(i,j)
term(989) = term(989) + wm_interm_100_so_pt2(i,j) * wm_interm_76_so_pt2(q,i,p,j)
term(990) = term(990) + wm_interm_101_so_pt2(i,j) * wm_interm_76_so_pt2(q,i,p,j)
term(991) = term(991) + wm_interm_77_so_pt2(q,i,p,j) * wm_interm_99_so_pt2(i,j)
term(992) = term(992) + wm_interm_100_so_pt2(i,j) * wm_interm_77_so_pt2(q,i,p,j)
term(993) = term(993) + wm_interm_101_so_pt2(i,j) * wm_interm_77_so_pt2(q,i,p,j)
term(994) = term(994) + wm_interm_79_so_pt2(q,p,i,j) * wm_interm_99_so_pt2(i,j)
term(995) = term(995) + wm_interm_100_so_pt2(i,j) * wm_interm_79_so_pt2(q,p,i,j)
term(996) = term(996) + wm_interm_101_so_pt2(i,j) * wm_interm_79_so_pt2(q,p,i,j)
term(997) = term(997) + wm_interm_80_so_pt2(q,p,i,j) * wm_interm_99_so_pt2(i,j)
term(998) = term(998) + wm_interm_100_so_pt2(i,j) * wm_interm_80_so_pt2(q,p,i,j)
term(999) = term(999) + wm_interm_101_so_pt2(i,j) * wm_interm_80_so_pt2(q,p,i,j)
end do 
end do 

term(964) = term(964) * (0.5d+0) 
term(965) = term(965) * (-1.0d+0) 
term(966) = term(966) * (0.5d+0) 
term(967) = term(967) * (0.5d+0) 
term(968) = term(968) * (-1.0d+0) 
term(969) = term(969) * (0.5d+0) 
term(970) = term(970) * (-1.0d+0) 
term(971) = term(971) * (2.0d+0) 
term(972) = term(972) * (-1.0d+0) 
term(973) = term(973) * (0.5d+0) 
term(974) = term(974) * (-1.0d+0) 
term(975) = term(975) * (0.5d+0) 
term(976) = term(976) * (-1.0d+0) 
term(977) = term(977) * (2.0d+0) 
term(978) = term(978) * (-1.0d+0) 
term(979) = term(979) * (0.5d+0) 
term(980) = term(980) * (-1.0d+0) 
term(981) = term(981) * (0.5d+0) 
term(983) = term(983) * (-2.0d+0) 
term(986) = term(986) * (-2.0d+0) 
term(988) = term(988) * (-2.0d+0) 
term(989) = term(989) * (4.0d+0) 
term(990) = term(990) * (-2.0d+0) 
term(992) = term(992) * (-2.0d+0) 
term(994) = term(994) * (-2.0d+0) 
term(995) = term(995) * (4.0d+0) 
term(996) = term(996) * (-2.0d+0) 
term(998) = term(998) * (-2.0d+0) 


    calc_D_ov_wm_so_cc3_pt2 = zero
    do s = 0, 999
    calc_D_ov_wm_so_cc3_pt2 = calc_D_ov_wm_so_cc3_pt2 + term(s)
    end do

    end function calc_D_ov_wm_so_cc3_pt2
    
    function calc_D_vo_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, k, j, c, b, a, l 
    real(F64), dimension(0:357) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_6_so_pt2(b,c,i,k)
term(1) = term(1) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_6_so_pt2(a,c,i,k)
term(2) = term(2) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_6_so_pt2(b,c,i,k)
term(3) = term(3) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_6_so_pt2(a,c,i,k)
term(4) = term(4) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_6_so_pt2(a,c,j,k)
term(5) = term(5) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_6_so_pt2(a,c,j,k)
term(6) = term(6) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_6_so_pt2(b,c,j,k)
term(7) = term(7) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_6_so_pt2(b,c,j,k)
term(8) = term(8) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_8_so_pt2(b,c,i,k)
term(9) = term(9) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_8_so_pt2(a,c,i,k)
term(10) = term(10) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_8_so_pt2(b,c,i,k)
term(11) = term(11) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_8_so_pt2(a,c,i,k)
term(12) = term(12) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_8_so_pt2(a,c,j,k)
term(13) = term(13) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_8_so_pt2(a,c,j,k)
term(14) = term(14) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_8_so_pt2(b,c,j,k)
term(15) = term(15) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_8_so_pt2(b,c,j,k)
term(16) = term(16) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_10_so_pt2(b,c,i,k)
term(17) = term(17) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_10_so_pt2(a,c,i,k)
term(18) = term(18) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_10_so_pt2(a,c,j,k)
term(19) = term(19) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_10_so_pt2(b,c,j,k)
term(20) = term(20) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_10_so_pt2(b,c,j,k)
term(21) = term(21) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_10_so_pt2(a,c,j,k)
term(22) = term(22) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_10_so_pt2(a,c,i,k)
term(23) = term(23) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_10_so_pt2(b,c,i,k)
term(24) = term(24) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_15_so_pt2(b,c,i,k)
term(25) = term(25) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_15_so_pt2(a,c,i,k)
term(26) = term(26) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_15_so_pt2(b,c,i,k)
term(27) = term(27) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_15_so_pt2(a,c,i,k)
term(28) = term(28) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_15_so_pt2(a,c,j,k)
term(29) = term(29) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_15_so_pt2(a,c,j,k)
term(30) = term(30) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_15_so_pt2(b,c,j,k)
term(31) = term(31) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_15_so_pt2(b,c,j,k)
term(32) = term(32) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_34_so_pt2(c,a,i,q)
term(33) = term(33) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_34_so_pt2(c,b,i,q)
term(34) = term(34) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_34_so_pt2(c,b,k,q)
term(35) = term(35) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_15_so_pt2(a,c,k,q)
term(36) = term(36) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_10_so_pt2(a,c,k,q)
term(37) = term(37) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_35_so_pt2(c,a,i,q)
term(38) = term(38) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_35_so_pt2(c,b,i,q)
term(39) = term(39) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_15_so_pt2(a,c,i,q)
term(40) = term(40) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_35_so_pt2(c,b,k,q)
term(41) = term(41) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_34_so_pt2(c,a,k,q)
term(42) = term(42) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_10_so_pt2(a,c,i,q)
term(43) = term(43) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_35_so_pt2(c,a,k,q)
term(44) = term(44) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_38_so_pt2(c,a,i,q)
term(45) = term(45) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_38_so_pt2(c,b,i,q)
term(46) = term(46) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_38_so_pt2(c,b,k,q)
term(47) = term(47) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_39_so_pt2(c,a,k,q)
term(48) = term(48) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_39_so_pt2(c,b,k,q)
term(49) = term(49) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_39_so_pt2(c,a,i,q)
term(50) = term(50) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_39_so_pt2(c,b,i,q)
term(51) = term(51) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_38_so_pt2(c,a,k,q)
term(52) = term(52) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_8_so_pt2(a,c,k,q)
term(53) = term(53) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_6_so_pt2(a,c,k,q)
term(54) = term(54) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_8_so_pt2(a,c,i,q)
term(55) = term(55) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_6_so_pt2(a,c,i,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-12.0d+0) 
term(1) = term(1) * (16.0d+0) 
term(2) = term(2) * (24.0d+0) 
term(3) = term(3) * (-32.0d+0) 
term(4) = term(4) * (-12.0d+0) 
term(5) = term(5) * (24.0d+0) 
term(6) = term(6) * (16.0d+0) 
term(7) = term(7) * (-32.0d+0) 
term(8) = term(8) * (6.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-12.0d+0) 
term(11) = term(11) * (16.0d+0) 
term(12) = term(12) * (6.0d+0) 
term(13) = term(13) * (-12.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (16.0d+0) 
term(16) = term(16) * (6.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (6.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (6.0d+0) 
term(21) = term(21) * (-8.0d+0) 
term(22) = term(22) * (6.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (6.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (-12.0d+0) 
term(27) = term(27) * (16.0d+0) 
term(28) = term(28) * (6.0d+0) 
term(29) = term(29) * (-12.0d+0) 
term(30) = term(30) * (-8.0d+0) 
term(31) = term(31) * (16.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (3.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * (6.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (8.0d+0) 
term(38) = term(38) * (-6.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (8.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (8.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (3.0d+0) 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (3.0d+0) 
term(49) = term(49) * (4.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (6.0d+0) 
term(53) = term(53) * (-12.0d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (16.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(56) = term(56) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_5_so_pt2(c,a)
term(57) = term(57) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_7_so_pt2(c,a)
end do 
end do 
end do 
end do 
end do 

term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(58) = term(58) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,l,i) * wm_interm_24_so_pt2(b,j,k,q,i,l)
term(59) = term(59) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,l,i) * wm_interm_24_so_pt2(b,k,j,q,i,l)
term(60) = term(60) + r2(vrdav_Rl, a,j,b,k) * t2(b,p,l,i) * wm_interm_24_so_pt2(a,k,j,q,i,l)
term(61) = term(61) + r2(vrdav_Rl, a,j,b,k) * t2(b,p,l,i) * wm_interm_24_so_pt2(a,j,k,q,i,l)
term(62) = term(62) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,l,i) * wm_interm_24_so_pt2(b,q,j,k,i,l)
term(63) = term(63) + r2(vrdav_Rl, a,j,b,k) * t2(b,p,l,i) * wm_interm_24_so_pt2(a,q,j,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(58) = term(58) * (3.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (3.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (3.0d+0) 
term(63) = term(63) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(64) = term(64) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_6_so_pt2(a,c,j,k)
term(65) = term(65) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_8_so_pt2(a,c,j,k)
term(66) = term(66) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_10_so_pt2(a,c,j,k)
term(67) = term(67) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_15_so_pt2(a,c,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(64) = term(64) * (8.0d+0) 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (8.0d+0) 
term(67) = term(67) * (-4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(68) = term(68) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,l,i) * wm_interm_24_so_pt2(b,j,k,q,l,i)
term(69) = term(69) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,l,i) * wm_interm_24_so_pt2(b,q,j,k,l,i)
term(70) = term(70) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,l,i) * wm_interm_24_so_pt2(b,k,j,q,l,i)
term(71) = term(71) + r2(vrdav_Rl, a,j,b,k) * t2(b,p,l,i) * wm_interm_24_so_pt2(a,q,j,k,l,i)
term(72) = term(72) + r2(vrdav_Rl, a,j,b,k) * t2(b,p,l,i) * wm_interm_24_so_pt2(a,k,j,q,l,i)
term(73) = term(73) + r2(vrdav_Rl, a,j,b,k) * t2(b,p,l,i) * wm_interm_24_so_pt2(a,j,k,q,l,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(68) = term(68) * (-2.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (3.0d+0) 
term(72) = term(72) * (-2.0d+0) 
term(73) = term(73) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(74) = term(74) + wm_interm_0_so_pt2(p,i,j,q) * wm_interm_1_so_pt2(j,i)
term(75) = term(75) + wm_interm_0_so_pt2(p,i,q,j) * wm_interm_1_so_pt2(j,i)
term(76) = term(76) + wm_interm_0_so_pt2(p,i,j,q) * wm_interm_2_so_pt2(j,i)
term(77) = term(77) + wm_interm_0_so_pt2(p,i,q,j) * wm_interm_2_so_pt2(j,i)
term(78) = term(78) + wm_interm_16_so_pt2(i,j) * wm_interm_17_so_pt2(p,i,q,j)
term(79) = term(79) + wm_interm_16_so_pt2(i,j) * wm_interm_18_so_pt2(p,i,q,j)
term(80) = term(80) + wm_interm_16_so_pt2(i,j) * wm_interm_19_so_pt2(p,i,q,j)
term(81) = term(81) + wm_interm_16_so_pt2(i,j) * wm_interm_20_so_pt2(p,i,q,j)
term(82) = term(82) + wm_interm_17_so_pt2(p,i,q,j) * wm_interm_23_so_pt2(i,j)
term(83) = term(83) + wm_interm_18_so_pt2(p,i,q,j) * wm_interm_23_so_pt2(i,j)
term(84) = term(84) + wm_interm_19_so_pt2(p,i,q,j) * wm_interm_23_so_pt2(i,j)
term(85) = term(85) + wm_interm_20_so_pt2(p,i,q,j) * wm_interm_23_so_pt2(i,j)
term(86) = term(86) + wm_interm_16_so_pt2(i,j) * wm_interm_25_so_pt2(p,i,q,j)
term(87) = term(87) + wm_interm_16_so_pt2(i,j) * wm_interm_26_so_pt2(p,i,q,j)
term(88) = term(88) + wm_interm_23_so_pt2(i,j) * wm_interm_25_so_pt2(p,i,q,j)
term(89) = term(89) + wm_interm_23_so_pt2(i,j) * wm_interm_26_so_pt2(p,i,q,j)
end do 
end do 

term(74) = term(74) * (3.0d+0) 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (-6.0d+0) 
term(77) = term(77) * (8.0d+0) 
term(78) = term(78) * (6.0d+0) 
term(79) = term(79) * (-8.0d+0) 
term(80) = term(80) * (6.0d+0) 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * (-12.0d+0) 
term(83) = term(83) * (16.0d+0) 
term(84) = term(84) * (-12.0d+0) 
term(85) = term(85) * (16.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (8.0d+0) 
term(88) = term(88) * (8.0d+0) 
term(89) = term(89) * (-16.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(90) = term(90) + wm_interm_14_so_pt2(i,j,k,q) * wm_interm_3_so_pt2(p,k,j,i)
term(91) = term(91) + wm_interm_14_so_pt2(i,j,k,q) * wm_interm_4_so_pt2(p,k,j,i)
term(92) = term(92) + wm_interm_14_so_pt2(i,j,k,q) * wm_interm_9_so_pt2(p,k,j,i)
end do 
end do 
end do 

term(90) = term(90) * (6.0d+0) 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * (3.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(93) = term(93) + wm_interm_1_so_pt2(i,j) * wm_interm_3_so_pt2(p,j,q,i)
term(94) = term(94) + wm_interm_2_so_pt2(i,j) * wm_interm_3_so_pt2(p,j,q,i)
term(95) = term(95) + wm_interm_1_so_pt2(i,j) * wm_interm_4_so_pt2(p,j,q,i)
term(96) = term(96) + wm_interm_2_so_pt2(i,j) * wm_interm_4_so_pt2(p,j,q,i)
term(97) = term(97) + wm_interm_1_so_pt2(i,j) * wm_interm_3_so_pt2(p,j,i,q)
term(98) = term(98) + wm_interm_1_so_pt2(i,j) * wm_interm_4_so_pt2(p,j,i,q)
term(99) = term(99) + wm_interm_2_so_pt2(i,j) * wm_interm_3_so_pt2(p,j,i,q)
term(100) = term(100) + wm_interm_2_so_pt2(i,j) * wm_interm_4_so_pt2(p,j,i,q)
term(101) = term(101) + wm_interm_1_so_pt2(i,j) * wm_interm_9_so_pt2(p,j,q,i)
term(102) = term(102) + wm_interm_2_so_pt2(i,j) * wm_interm_9_so_pt2(p,j,q,i)
term(103) = term(103) + wm_interm_1_so_pt2(i,j) * wm_interm_9_so_pt2(p,j,i,q)
term(104) = term(104) + wm_interm_2_so_pt2(i,j) * wm_interm_9_so_pt2(p,j,i,q)
end do 
end do 

term(93) = term(93) * (6.0d+0) 
term(94) = term(94) * (-12.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (16.0d+0) 
term(97) = term(97) * (-4.0d+0) 
term(98) = term(98) * (8.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (-16.0d+0) 
term(101) = term(101) * (3.0d+0) 
term(102) = term(102) * (-6.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(105) = term(105) + wm_interm_14_so_pt2(i,j,k,q) * wm_interm_9_so_pt2(p,k,i,j)
term(106) = term(106) + wm_interm_14_so_pt2(i,j,k,q) * wm_interm_3_so_pt2(p,k,i,j)
term(107) = term(107) + wm_interm_14_so_pt2(i,j,k,q) * wm_interm_4_so_pt2(p,k,i,j)
end do 
end do 
end do 

term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * (8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(108) = term(108) + wm_interm_0_so_pt2(p,i,j,k) * wm_interm_14_so_pt2(j,k,i,q)
term(109) = term(109) + wm_interm_0_so_pt2(p,i,j,k) * wm_interm_14_so_pt2(k,j,i,q)
term(110) = term(110) + wm_interm_19_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,k,q)
term(111) = term(111) + wm_interm_20_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,q,k)
term(112) = term(112) + wm_interm_21_so_pt2(i,j,q,k) * wm_interm_25_so_pt2(p,i,j,k)
term(113) = term(113) + wm_interm_17_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,k,q)
term(114) = term(114) + wm_interm_21_so_pt2(i,j,q,k) * wm_interm_26_so_pt2(p,i,j,k)
term(115) = term(115) + wm_interm_18_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,k,q)
term(116) = term(116) + wm_interm_19_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,q,k)
term(117) = term(117) + wm_interm_20_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,k,q)
term(118) = term(118) + wm_interm_17_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,q,k)
term(119) = term(119) + wm_interm_21_so_pt2(i,j,k,q) * wm_interm_25_so_pt2(p,i,j,k)
term(120) = term(120) + wm_interm_21_so_pt2(i,j,k,q) * wm_interm_26_so_pt2(p,i,j,k)
term(121) = term(121) + wm_interm_18_so_pt2(p,i,j,k) * wm_interm_21_so_pt2(i,j,q,k)
end do 
end do 
end do 

term(108) = term(108) * (3.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (3.0d+0) 
term(111) = term(111) * (3.0d+0) 
term(112) = term(112) * (3.0d+0) 
term(113) = term(113) * (3.0d+0) 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(122) = term(122) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_9_so_pt2(b,i,k,j)
term(123) = term(123) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_3_so_pt2(b,i,k,j)
term(124) = term(124) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_4_so_pt2(b,i,k,j)
term(125) = term(125) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_0_so_pt2(b,i,k,j)
term(126) = term(126) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_17_so_pt2(b,k,j,i)
term(127) = term(127) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_17_so_pt2(b,j,k,i)
term(128) = term(128) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_20_so_pt2(b,j,k,i)
term(129) = term(129) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_18_so_pt2(b,k,j,i)
term(130) = term(130) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_19_so_pt2(b,j,k,i)
term(131) = term(131) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_18_so_pt2(b,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (8.0d+0) 
term(125) = term(125) * (3.0d+0) 
term(126) = term(126) * (3.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (3.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(132) = term(132) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_17_so_pt2(b,j,k,i)
term(133) = term(133) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_17_so_pt2(b,k,j,i)
term(134) = term(134) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_19_so_pt2(b,j,k,i)
term(135) = term(135) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_18_so_pt2(b,j,k,i)
term(136) = term(136) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_so_pt2(b,j,k,i)
term(137) = term(137) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_18_so_pt2(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(132) = term(132) * (3.0d+0) 
term(133) = term(133) * (-4.0d+0) 
term(134) = term(134) * (3.0d+0) 
term(135) = term(135) * (-4.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(138) = term(138) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j)
term(139) = term(139) + r1(vrdav_Rl, b,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j)
term(140) = term(140) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i)
term(141) = term(141) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i)
end do 
end do 
end do 
end do 

term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * (6.0d+0) 
term(140) = term(140) * (6.0d+0) 
term(141) = term(141) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(142) = term(142) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j)
end do 
end do 
end do 
end do 

term(142) = term(142) * (8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(143) = term(143) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,p,q) * s1(b,j)
end do 
end do 
end do 
end do 

term(143) = term(143) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(144) = term(144) + wm_interm_13_so_pt2(p,i,j,q,k,l) * wm_interm_14_so_pt2(k,l,i,j)
term(145) = term(145) + wm_interm_13_so_pt2(p,i,j,k,q,l) * wm_interm_14_so_pt2(l,k,i,j)
term(146) = term(146) + wm_interm_13_so_pt2(p,i,j,q,k,l) * wm_interm_14_so_pt2(l,k,i,j)
term(147) = term(147) + wm_interm_13_so_pt2(p,i,j,k,q,l) * wm_interm_14_so_pt2(k,l,i,j)
term(148) = term(148) + wm_interm_13_so_pt2(p,i,j,k,l,q) * wm_interm_14_so_pt2(l,k,i,j)
term(149) = term(149) + wm_interm_13_so_pt2(p,i,j,k,l,q) * wm_interm_14_so_pt2(k,l,i,j)
term(150) = term(150) + wm_interm_21_so_pt2(i,j,k,l) * wm_interm_22_so_pt2(p,i,j,q,k,l)
term(151) = term(151) + wm_interm_21_so_pt2(i,j,k,l) * wm_interm_24_so_pt2(p,i,j,q,k,l)
term(152) = term(152) + wm_interm_21_so_pt2(i,j,k,l) * wm_interm_27_so_pt2(p,i,j,q,k,l)
end do 
end do 
end do 
end do 

term(144) = term(144) * (3.0d+0) 
term(145) = term(145) * (3.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(149) = term(149) * (4.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (3.0d+0) 
term(152) = term(152) * (4.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(153) = term(153) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_5_so_pt2(c,b)
term(154) = term(154) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_7_so_pt2(c,b)
term(155) = term(155) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_11_so_pt2(a,c)
term(156) = term(156) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_12_so_pt2(a,c)
end do 
end do 
end do 
end do 
end do 

term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(157) = term(157) + wm_interm_34_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,i,q)
term(158) = term(158) + wm_interm_34_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,i,q)
term(159) = term(159) + wm_interm_34_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,i,q)
term(160) = term(160) + wm_interm_34_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,q,i)
term(161) = term(161) + wm_interm_34_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,q,i)
term(162) = term(162) + wm_interm_34_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,q,i)
term(163) = term(163) + wm_interm_0_so_pt2(a,i,q,j) * wm_interm_34_so_pt2(a,p,j,i)
term(164) = term(164) + wm_interm_39_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,q,i)
term(165) = term(165) + wm_interm_39_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,q,i)
term(166) = term(166) + wm_interm_39_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,q,i)
term(167) = term(167) + wm_interm_39_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,i,q)
term(168) = term(168) + wm_interm_0_so_pt2(a,i,q,j) * wm_interm_39_so_pt2(a,p,j,i)
term(169) = term(169) + wm_interm_39_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,i,q)
term(170) = term(170) + wm_interm_39_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,i,q)
term(171) = term(171) + wm_interm_38_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,i,q)
term(172) = term(172) + wm_interm_38_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,i,q)
term(173) = term(173) + wm_interm_38_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,i,q)
term(174) = term(174) + wm_interm_38_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,q,i)
term(175) = term(175) + wm_interm_38_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,q,i)
term(176) = term(176) + wm_interm_38_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,q,i)
term(177) = term(177) + wm_interm_35_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,i,q)
term(178) = term(178) + wm_interm_35_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,i,q)
term(179) = term(179) + wm_interm_35_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,i,q)
term(180) = term(180) + wm_interm_35_so_pt2(a,p,i,j) * wm_interm_9_so_pt2(a,j,q,i)
term(181) = term(181) + wm_interm_35_so_pt2(a,p,i,j) * wm_interm_3_so_pt2(a,j,q,i)
term(182) = term(182) + wm_interm_35_so_pt2(a,p,i,j) * wm_interm_4_so_pt2(a,j,q,i)
term(183) = term(183) + wm_interm_0_so_pt2(a,i,q,j) * wm_interm_38_so_pt2(a,p,j,i)
term(184) = term(184) + wm_interm_0_so_pt2(a,i,q,j) * wm_interm_35_so_pt2(a,p,j,i)
term(185) = term(185) + wm_interm_15_so_pt2(a,p,i,j) * wm_interm_17_so_pt2(a,q,i,j)
term(186) = term(186) + wm_interm_15_so_pt2(a,p,i,j) * wm_interm_17_so_pt2(a,i,q,j)
term(187) = term(187) + wm_interm_10_so_pt2(a,p,i,j) * wm_interm_17_so_pt2(a,i,q,j)
term(188) = term(188) + wm_interm_10_so_pt2(a,p,i,j) * wm_interm_17_so_pt2(a,q,i,j)
term(189) = term(189) + wm_interm_15_so_pt2(a,p,i,j) * wm_interm_20_so_pt2(a,i,q,j)
term(190) = term(190) + wm_interm_10_so_pt2(a,p,i,j) * wm_interm_19_so_pt2(a,i,q,j)
term(191) = term(191) + wm_interm_10_so_pt2(a,p,i,j) * wm_interm_20_so_pt2(a,i,q,j)
term(192) = term(192) + wm_interm_15_so_pt2(a,p,i,j) * wm_interm_18_so_pt2(a,q,i,j)
term(193) = term(193) + wm_interm_10_so_pt2(a,p,i,j) * wm_interm_18_so_pt2(a,i,q,j)
term(194) = term(194) + wm_interm_10_so_pt2(a,p,i,j) * wm_interm_18_so_pt2(a,q,i,j)
term(195) = term(195) + wm_interm_15_so_pt2(a,p,i,j) * wm_interm_19_so_pt2(a,i,q,j)
term(196) = term(196) + wm_interm_15_so_pt2(a,p,i,j) * wm_interm_18_so_pt2(a,i,q,j)
term(197) = term(197) + wm_interm_17_so_pt2(a,q,i,j) * wm_interm_8_so_pt2(a,p,i,j)
term(198) = term(198) + wm_interm_17_so_pt2(a,i,q,j) * wm_interm_8_so_pt2(a,p,i,j)
term(199) = term(199) + wm_interm_17_so_pt2(a,q,i,j) * wm_interm_6_so_pt2(a,p,i,j)
term(200) = term(200) + wm_interm_17_so_pt2(a,i,q,j) * wm_interm_6_so_pt2(a,p,i,j)
term(201) = term(201) + wm_interm_20_so_pt2(a,i,q,j) * wm_interm_8_so_pt2(a,p,i,j)
term(202) = term(202) + wm_interm_20_so_pt2(a,i,q,j) * wm_interm_6_so_pt2(a,p,i,j)
term(203) = term(203) + wm_interm_18_so_pt2(a,q,i,j) * wm_interm_8_so_pt2(a,p,i,j)
term(204) = term(204) + wm_interm_18_so_pt2(a,q,i,j) * wm_interm_6_so_pt2(a,p,i,j)
term(205) = term(205) + wm_interm_19_so_pt2(a,i,q,j) * wm_interm_8_so_pt2(a,p,i,j)
term(206) = term(206) + wm_interm_18_so_pt2(a,i,q,j) * wm_interm_8_so_pt2(a,p,i,j)
term(207) = term(207) + wm_interm_19_so_pt2(a,i,q,j) * wm_interm_6_so_pt2(a,p,i,j)
term(208) = term(208) + wm_interm_18_so_pt2(a,i,q,j) * wm_interm_6_so_pt2(a,p,i,j)
end do 
end do 
end do 

term(157) = term(157) * (6.0d+0) 
term(158) = term(158) * (-8.0d+0) 
term(159) = term(159) * (3.0d+0) 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (8.0d+0) 
term(163) = term(163) * (3.0d+0) 
term(164) = term(164) * (6.0d+0) 
term(165) = term(165) * (-8.0d+0) 
term(166) = term(166) * (3.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * (-4.0d+0) 
term(170) = term(170) * (8.0d+0) 
term(171) = term(171) * (6.0d+0) 
term(172) = term(172) * (-8.0d+0) 
term(173) = term(173) * (3.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (8.0d+0) 
term(177) = term(177) * (-12.0d+0) 
term(178) = term(178) * (16.0d+0) 
term(179) = term(179) * (-6.0d+0) 
term(180) = term(180) * (4.0d+0) 
term(181) = term(181) * (16.0d+0) 
term(182) = term(182) * (-16.0d+0) 
term(183) = term(183) * (3.0d+0) 
term(184) = term(184) * (-6.0d+0) 
term(185) = term(185) * (6.0d+0) 
term(186) = term(186) * (-8.0d+0) 
term(187) = term(187) * (6.0d+0) 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (6.0d+0) 
term(190) = term(190) * (6.0d+0) 
term(191) = term(191) * (-8.0d+0) 
term(192) = term(192) * (-8.0d+0) 
term(193) = term(193) * (-8.0d+0) 
term(194) = term(194) * (8.0d+0) 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (6.0d+0) 
term(198) = term(198) * (-8.0d+0) 
term(199) = term(199) * (-12.0d+0) 
term(200) = term(200) * (16.0d+0) 
term(201) = term(201) * (6.0d+0) 
term(202) = term(202) * (-12.0d+0) 
term(203) = term(203) * (-8.0d+0) 
term(204) = term(204) * (16.0d+0) 
term(205) = term(205) * (-4.0d+0) 
term(206) = term(206) * (8.0d+0) 
term(207) = term(207) * (8.0d+0) 
term(208) = term(208) * (-16.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(209) = term(209) + wm_interm_21_so_pt2(i,j,k,l) * wm_interm_22_so_pt2(p,i,j,q,l,k)
term(210) = term(210) + wm_interm_21_so_pt2(i,j,k,l) * wm_interm_24_so_pt2(p,i,j,q,l,k)
term(211) = term(211) + wm_interm_21_so_pt2(i,j,k,l) * wm_interm_27_so_pt2(p,i,j,q,l,k)
end do 
end do 
end do 
end do 

term(209) = term(209) * (3.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(212) = term(212) + wm_interm_10_so_pt2(a,p,i,q) * wm_interm_28_so_pt2(a,i)
term(213) = term(213) + wm_interm_10_so_pt2(a,p,i,q) * wm_interm_29_so_pt2(a,i)
term(214) = term(214) + wm_interm_10_so_pt2(a,p,i,q) * wm_interm_30_so_pt2(a,i)
term(215) = term(215) + wm_interm_10_so_pt2(a,p,i,q) * wm_interm_31_so_pt2(a,i)
term(216) = term(216) + wm_interm_32_so_pt2(a,i) * wm_interm_39_so_pt2(a,p,i,q)
term(217) = term(217) + wm_interm_33_so_pt2(a,i) * wm_interm_39_so_pt2(a,p,i,q)
term(218) = term(218) + wm_interm_36_so_pt2(a,i) * wm_interm_39_so_pt2(a,p,i,q)
term(219) = term(219) + wm_interm_37_so_pt2(a,i) * wm_interm_39_so_pt2(a,p,i,q)
end do 
end do 

term(212) = term(212) * (8.0d+0) 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * (-6.0d+0) 
term(215) = term(215) * (2.0d+0) 
term(216) = term(216) * (6.0d+0) 
term(217) = term(217) * (-8.0d+0) 
term(218) = term(218) * (-2.0d+0) 
term(219) = term(219) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(220) = term(220) + wm_interm_0_so_pt2(a,i,j,q) * wm_interm_34_so_pt2(a,p,j,i)
term(221) = term(221) + wm_interm_0_so_pt2(a,i,j,q) * wm_interm_39_so_pt2(a,p,j,i)
term(222) = term(222) + wm_interm_0_so_pt2(a,i,j,q) * wm_interm_38_so_pt2(a,p,j,i)
term(223) = term(223) + wm_interm_0_so_pt2(a,i,j,q) * wm_interm_35_so_pt2(a,p,j,i)
end do 
end do 
end do 

term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (3.0d+0) 
term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(224) = term(224) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_3_so_pt2(b,i,j,k)
term(225) = term(225) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_4_so_pt2(b,i,j,k)
term(226) = term(226) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_9_so_pt2(b,i,j,k)
term(227) = term(227) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_0_so_pt2(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(224) = term(224) * (6.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * (3.0d+0) 
term(227) = term(227) * (-2.0d+0) 

do i = 1, nocc 
term(228) = term(228) + wm_interm_16_so_pt2(i,q) * wm_interm_28_so_pt2(p,i)
term(229) = term(229) + wm_interm_16_so_pt2(i,q) * wm_interm_29_so_pt2(p,i)
term(230) = term(230) + wm_interm_23_so_pt2(i,q) * wm_interm_28_so_pt2(p,i)
term(231) = term(231) + wm_interm_23_so_pt2(i,q) * wm_interm_29_so_pt2(p,i)
term(232) = term(232) + wm_interm_16_so_pt2(i,q) * wm_interm_30_so_pt2(p,i)
term(233) = term(233) + wm_interm_23_so_pt2(i,q) * wm_interm_30_so_pt2(p,i)
term(234) = term(234) + wm_interm_16_so_pt2(i,q) * wm_interm_31_so_pt2(p,i)
term(235) = term(235) + wm_interm_23_so_pt2(i,q) * wm_interm_31_so_pt2(p,i)
term(236) = term(236) + wm_interm_1_so_pt2(i,q) * wm_interm_32_so_pt2(p,i)
term(237) = term(237) + wm_interm_1_so_pt2(i,q) * wm_interm_33_so_pt2(p,i)
term(238) = term(238) + wm_interm_2_so_pt2(i,q) * wm_interm_32_so_pt2(p,i)
term(239) = term(239) + wm_interm_2_so_pt2(i,q) * wm_interm_33_so_pt2(p,i)
term(240) = term(240) + wm_interm_1_so_pt2(i,q) * wm_interm_36_so_pt2(p,i)
term(241) = term(241) + wm_interm_1_so_pt2(i,q) * wm_interm_37_so_pt2(p,i)
term(242) = term(242) + wm_interm_2_so_pt2(i,q) * wm_interm_36_so_pt2(p,i)
term(243) = term(243) + wm_interm_2_so_pt2(i,q) * wm_interm_37_so_pt2(p,i)
end do 

term(228) = term(228) * (8.0d+0) 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * (-16.0d+0) 
term(231) = term(231) * (8.0d+0) 
term(232) = term(232) * (-6.0d+0) 
term(233) = term(233) * (12.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (-4.0d+0) 
term(236) = term(236) * (6.0d+0) 
term(237) = term(237) * (-8.0d+0) 
term(238) = term(238) * (-12.0d+0) 
term(239) = term(239) * (16.0d+0) 
term(240) = term(240) * (-2.0d+0) 
term(241) = term(241) * (4.0d+0) 
term(242) = term(242) * (4.0d+0) 
term(243) = term(243) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(244) = term(244) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_34_so_pt2(c,a,i,q)
term(245) = term(245) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_34_so_pt2(c,a,k,q)
term(246) = term(246) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_35_so_pt2(c,a,i,q)
term(247) = term(247) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_35_so_pt2(c,a,k,q)
term(248) = term(248) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_38_so_pt2(c,a,i,q)
term(249) = term(249) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_38_so_pt2(c,a,k,q)
term(250) = term(250) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_39_so_pt2(c,a,k,q)
term(251) = term(251) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_39_so_pt2(c,a,i,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(244) = term(244) * (3.0d+0) 
term(245) = term(245) * (-4.0d+0) 
term(246) = term(246) * (-6.0d+0) 
term(247) = term(247) * (8.0d+0) 
term(248) = term(248) * (3.0d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (3.0d+0) 
term(251) = term(251) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(252) = term(252) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_so_pt2(a,c)
term(253) = term(253) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_so_pt2(c,b)
term(254) = term(254) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_so_pt2(a,c)
term(255) = term(255) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_so_pt2(c,b)
term(256) = term(256) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_so_pt2(c,b)
term(257) = term(257) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_so_pt2(c,b)
term(258) = term(258) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_so_pt2(a,c)
term(259) = term(259) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_so_pt2(a,c)
term(260) = term(260) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_so_pt2(a,b)
term(261) = term(261) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_so_pt2(a,b)
term(262) = term(262) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_so_pt2(a,b)
term(263) = term(263) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_so_pt2(a,b)
term(264) = term(264) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_11_so_pt2(b,c)
term(265) = term(265) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_11_so_pt2(a,c)
term(266) = term(266) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_11_so_pt2(a,c)
term(267) = term(267) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_11_so_pt2(b,c)
term(268) = term(268) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_12_so_pt2(b,c)
term(269) = term(269) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_12_so_pt2(a,c)
term(270) = term(270) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_12_so_pt2(a,c)
term(271) = term(271) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_12_so_pt2(b,c)
end do 
end do 
end do 
end do 
end do 

term(252) = term(252) * (3.0d+0) 
term(253) = term(253) * (-8.0d+0) 
term(254) = term(254) * (-6.0d+0) 
term(255) = term(255) * (16.0d+0) 
term(256) = term(256) * (6.0d+0) 
term(257) = term(257) * (-12.0d+0) 
term(258) = term(258) * (-4.0d+0) 
term(259) = term(259) * (8.0d+0) 
term(260) = term(260) * (3.0d+0) 
term(261) = term(261) * (-6.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (8.0d+0) 
term(264) = term(264) * (-12.0d+0) 
term(265) = term(265) * (16.0d+0) 
term(266) = term(266) * (-12.0d+0) 
term(267) = term(267) * (16.0d+0) 
term(268) = term(268) * (6.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (6.0d+0) 
term(271) = term(271) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(272) = term(272) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_34_so_pt2(c,b,i,q)
term(273) = term(273) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_15_so_pt2(a,c,j,q)
term(274) = term(274) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_10_so_pt2(a,c,j,q)
term(275) = term(275) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_35_so_pt2(c,b,i,q)
term(276) = term(276) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_15_so_pt2(a,c,i,q)
term(277) = term(277) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_10_so_pt2(a,c,k,q)
term(278) = term(278) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_10_so_pt2(a,c,j,q)
term(279) = term(279) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_34_so_pt2(c,b,k,q)
term(280) = term(280) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_10_so_pt2(a,c,i,q)
term(281) = term(281) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_15_so_pt2(a,c,k,q)
term(282) = term(282) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_15_so_pt2(a,c,j,q)
term(283) = term(283) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_35_so_pt2(c,b,k,q)
term(284) = term(284) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_38_so_pt2(c,b,i,q)
term(285) = term(285) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_39_so_pt2(c,b,k,q)
term(286) = term(286) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_39_so_pt2(c,b,i,q)
term(287) = term(287) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_38_so_pt2(c,b,k,q)
term(288) = term(288) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_8_so_pt2(a,c,j,q)
term(289) = term(289) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_6_so_pt2(a,c,j,q)
term(290) = term(290) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_8_so_pt2(a,c,i,q)
term(291) = term(291) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_6_so_pt2(a,c,i,q)
term(292) = term(292) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_8_so_pt2(a,c,k,q)
term(293) = term(293) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_8_so_pt2(a,c,j,q)
term(294) = term(294) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_6_so_pt2(a,c,k,q)
term(295) = term(295) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_6_so_pt2(a,c,j,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (-8.0d+0) 
term(274) = term(274) * (6.0d+0) 
term(275) = term(275) * (8.0d+0) 
term(276) = term(276) * (6.0d+0) 
term(277) = term(277) * (6.0d+0) 
term(278) = term(278) * (-8.0d+0) 
term(279) = term(279) * (4.0d+0) 
term(280) = term(280) * (-4.0d+0) 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (8.0d+0) 
term(283) = term(283) * (-8.0d+0) 
term(284) = term(284) * (-4.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (4.0d+0) 
term(287) = term(287) * (4.0d+0) 
term(288) = term(288) * (-8.0d+0) 
term(289) = term(289) * (16.0d+0) 
term(290) = term(290) * (6.0d+0) 
term(291) = term(291) * (-12.0d+0) 
term(292) = term(292) * (-4.0d+0) 
term(293) = term(293) * (8.0d+0) 
term(294) = term(294) * (8.0d+0) 
term(295) = term(295) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(296) = term(296) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_34_so_pt2(c,a,k,q)
term(297) = term(297) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_34_so_pt2(c,b,k,q)
term(298) = term(298) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_35_so_pt2(c,a,k,q)
term(299) = term(299) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_35_so_pt2(c,b,k,q)
term(300) = term(300) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_34_so_pt2(c,b,k,q)
term(301) = term(301) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_34_so_pt2(c,a,k,q)
term(302) = term(302) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_35_so_pt2(c,b,k,q)
term(303) = term(303) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_35_so_pt2(c,a,k,q)
term(304) = term(304) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_38_so_pt2(c,a,k,q)
term(305) = term(305) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_38_so_pt2(c,b,k,q)
term(306) = term(306) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_39_so_pt2(c,a,k,q)
term(307) = term(307) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_39_so_pt2(c,b,k,q)
term(308) = term(308) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_39_so_pt2(c,b,k,q)
term(309) = term(309) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_39_so_pt2(c,a,k,q)
term(310) = term(310) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_38_so_pt2(c,b,k,q)
term(311) = term(311) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_38_so_pt2(c,a,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(296) = term(296) * (3.0d+0) 
term(297) = term(297) * (3.0d+0) 
term(298) = term(298) * (-6.0d+0) 
term(299) = term(299) * (-6.0d+0) 
term(300) = term(300) * (-2.0d+0) 
term(301) = term(301) * (-2.0d+0) 
term(302) = term(302) * (4.0d+0) 
term(303) = term(303) * (4.0d+0) 
term(304) = term(304) * (3.0d+0) 
term(305) = term(305) * (3.0d+0) 
term(306) = term(306) * (3.0d+0) 
term(307) = term(307) * (3.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * (-2.0d+0) 
term(311) = term(311) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(312) = term(312) + wm_interm_12_so_pt2(a,p) * wm_interm_28_so_pt2(a,q)
term(313) = term(313) + wm_interm_12_so_pt2(a,p) * wm_interm_29_so_pt2(a,q)
term(314) = term(314) + wm_interm_11_so_pt2(a,p) * wm_interm_28_so_pt2(a,q)
term(315) = term(315) + wm_interm_11_so_pt2(a,p) * wm_interm_29_so_pt2(a,q)
term(316) = term(316) + wm_interm_12_so_pt2(a,p) * wm_interm_30_so_pt2(a,q)
term(317) = term(317) + wm_interm_11_so_pt2(a,p) * wm_interm_30_so_pt2(a,q)
term(318) = term(318) + wm_interm_12_so_pt2(a,p) * wm_interm_31_so_pt2(a,q)
term(319) = term(319) + wm_interm_11_so_pt2(a,p) * wm_interm_31_so_pt2(a,q)
term(320) = term(320) + wm_interm_32_so_pt2(a,q) * wm_interm_5_so_pt2(a,p)
term(321) = term(321) + wm_interm_33_so_pt2(a,q) * wm_interm_5_so_pt2(a,p)
term(322) = term(322) + wm_interm_36_so_pt2(a,q) * wm_interm_5_so_pt2(a,p)
term(323) = term(323) + wm_interm_37_so_pt2(a,q) * wm_interm_5_so_pt2(a,p)
term(324) = term(324) + wm_interm_32_so_pt2(a,q) * wm_interm_7_so_pt2(a,p)
term(325) = term(325) + wm_interm_33_so_pt2(a,q) * wm_interm_7_so_pt2(a,p)
term(326) = term(326) + wm_interm_36_so_pt2(a,q) * wm_interm_7_so_pt2(a,p)
term(327) = term(327) + wm_interm_37_so_pt2(a,q) * wm_interm_7_so_pt2(a,p)
end do 

term(312) = term(312) * (8.0d+0) 
term(313) = term(313) * (-4.0d+0) 
term(314) = term(314) * (-16.0d+0) 
term(315) = term(315) * (8.0d+0) 
term(316) = term(316) * (-6.0d+0) 
term(317) = term(317) * (12.0d+0) 
term(318) = term(318) * (2.0d+0) 
term(319) = term(319) * (-4.0d+0) 
term(320) = term(320) * (6.0d+0) 
term(321) = term(321) * (-8.0d+0) 
term(322) = term(322) * (-2.0d+0) 
term(323) = term(323) * (4.0d+0) 
term(324) = term(324) * (-12.0d+0) 
term(325) = term(325) * (16.0d+0) 
term(326) = term(326) * (4.0d+0) 
term(327) = term(327) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(328) = term(328) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_6_so_pt2(a,c,i,k)
term(329) = term(329) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_6_so_pt2(a,c,j,k)
term(330) = term(330) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_6_so_pt2(a,c,i,k)
term(331) = term(331) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_8_so_pt2(a,c,i,k)
term(332) = term(332) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_8_so_pt2(a,c,j,k)
term(333) = term(333) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_8_so_pt2(a,c,i,k)
term(334) = term(334) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_10_so_pt2(a,c,j,k)
term(335) = term(335) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_10_so_pt2(a,c,i,k)
term(336) = term(336) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_10_so_pt2(a,c,i,k)
term(337) = term(337) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_15_so_pt2(a,c,i,k)
term(338) = term(338) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_15_so_pt2(a,c,j,k)
term(339) = term(339) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_15_so_pt2(a,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(328) = term(328) * (-16.0d+0) 
term(329) = term(329) * (-16.0d+0) 
term(330) = term(330) * (32.0d+0) 
term(331) = term(331) * (8.0d+0) 
term(332) = term(332) * (8.0d+0) 
term(333) = term(333) * (-16.0d+0) 
term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * (8.0d+0) 
term(336) = term(336) * (-4.0d+0) 
term(337) = term(337) * (8.0d+0) 
term(338) = term(338) * (8.0d+0) 
term(339) = term(339) * (-16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(340) = term(340) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,l,i,q,j,k)
term(341) = term(341) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,l,i,j,q,k)
term(342) = term(342) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,l,i,j,k,q)
term(343) = term(343) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,i,l,j,k,q)
term(344) = term(344) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,i,l,j,q,k)
term(345) = term(345) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,i,l,q,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(340) = term(340) * (3.0d+0) 
term(341) = term(341) * (-2.0d+0) 
term(342) = term(342) * (3.0d+0) 
term(343) = term(343) * (-4.0d+0) 
term(344) = term(344) * (3.0d+0) 
term(345) = term(345) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(346) = term(346) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_5_so_pt2(c,b)
term(347) = term(347) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_7_so_pt2(c,b)
term(348) = term(348) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_5_so_pt2(c,a)
term(349) = term(349) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_7_so_pt2(c,a)
term(350) = term(350) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_11_so_pt2(a,c)
term(351) = term(351) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_12_so_pt2(a,c)
end do 
end do 
end do 
end do 
end do 

term(346) = term(346) * (-2.0d+0) 
term(347) = term(347) * (4.0d+0) 
term(348) = term(348) * (4.0d+0) 
term(349) = term(349) * (-8.0d+0) 
term(350) = term(350) * (-16.0d+0) 
term(351) = term(351) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(352) = term(352) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,l,i,q,k,j)
term(353) = term(353) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,l,i,k,j,q)
term(354) = term(354) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,l,i,k,q,j)
term(355) = term(355) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,i,l,k,j,q)
term(356) = term(356) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,i,l,k,q,j)
term(357) = term(357) + s2(a,b,j,k) * t2(b,p,l,i) * wm_interm_13_so_pt2(a,i,l,q,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(352) = term(352) * (-4.0d+0) 
term(353) = term(353) * (-4.0d+0) 
term(354) = term(354) * (4.0d+0) 
term(355) = term(355) * (3.0d+0) 
term(356) = term(356) * (-4.0d+0) 
term(357) = term(357) * (4.0d+0) 


    calc_D_vo_wm_so_cc3_pt2 = zero
    do s = 0, 357
    calc_D_vo_wm_so_cc3_pt2 = calc_D_vo_wm_so_cc3_pt2 + term(s)
    end do

    end function calc_D_vo_wm_so_cc3_pt2
    
    function calc_D_vv_wm_so_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_cc3_pt2
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
    real(F64), dimension(0:64) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,q,i) * wm_interm_107_so_pt2(a,p,i,j)
term(1) = term(1) + r2(vrdav_Rl, a,j,q,i) * wm_interm_108_so_pt2(a,p,i,j)
term(2) = term(2) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt2(a,p,i,j)
term(3) = term(3) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_110_so_pt2(q,a,j,i)
term(4) = term(4) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_110_so_pt2(a,q,j,i)
term(5) = term(5) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_110_so_pt2(a,q,j,i)
term(6) = term(6) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_110_so_pt2(q,a,i,j)
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_111_so_pt2(q,a,j,i)
term(8) = term(8) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_111_so_pt2(a,q,j,i)
term(9) = term(9) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_111_so_pt2(a,q,j,i)
term(10) = term(10) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_111_so_pt2(q,a,i,j)
term(11) = term(11) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_112_so_pt2(q,a,j,i)
term(12) = term(12) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt2(q,a,j,i)
term(13) = term(13) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt2(a,q,j,i)
term(14) = term(14) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_112_so_pt2(a,q,j,i)
term(15) = term(15) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_112_so_pt2(a,q,j,i)
term(16) = term(16) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt2(a,q,i,j)
term(17) = term(17) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt2(q,a,j,i)
term(18) = term(18) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_112_so_pt2(q,a,i,j)
term(19) = term(19) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_114_so_pt2(q,a,j,i)
term(20) = term(20) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_114_so_pt2(a,q,j,i)
term(21) = term(21) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_114_so_pt2(a,q,i,j)
term(22) = term(22) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_114_so_pt2(q,a,j,i)
term(23) = term(23) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_110_so_pt2(q,a,i,j)
term(24) = term(24) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_110_so_pt2(q,a,j,i)
term(25) = term(25) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_110_so_pt2(a,q,j,i)
term(26) = term(26) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_110_so_pt2(a,q,i,j)
term(27) = term(27) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_111_so_pt2(q,a,i,j)
term(28) = term(28) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_111_so_pt2(q,a,j,i)
term(29) = term(29) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_111_so_pt2(a,q,j,i)
term(30) = term(30) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_111_so_pt2(a,q,i,j)
term(31) = term(31) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_112_so_pt2(q,a,i,j)
term(32) = term(32) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_112_so_pt2(q,a,j,i)
term(33) = term(33) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt2(q,a,j,i)
term(34) = term(34) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt2(q,a,i,j)
term(35) = term(35) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt2(a,q,i,j)
term(36) = term(36) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt2(a,q,j,i)
term(37) = term(37) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_112_so_pt2(a,q,j,i)
term(38) = term(38) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_112_so_pt2(a,q,i,j)
term(39) = term(39) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_114_so_pt2(q,a,j,i)
term(40) = term(40) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_114_so_pt2(q,a,i,j)
term(41) = term(41) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_114_so_pt2(a,q,i,j)
term(42) = term(42) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_114_so_pt2(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-8.0d+0) 
term(1) = term(1) * (6.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(7) = term(7) * (-0.3333333333333333d+0) 
term(8) = term(8) * (0.6666666666666666d+0) 
term(9) = term(9) * (-0.3333333333333333d+0) 
term(10) = term(10) * (-0.3333333333333333d+0) 
term(11) = term(11) * (-0.3333333333333333d+0) 
term(12) = term(12) * (0.3333333333333333d+0) 
term(13) = term(13) * (-0.16666666666666666d+0) 
term(14) = term(14) * (0.6666666666666666d+0) 
term(15) = term(15) * (-0.3333333333333333d+0) 
term(16) = term(16) * (-0.16666666666666666d+0) 
term(17) = term(17) * (-0.16666666666666666d+0) 
term(18) = term(18) * (-0.3333333333333333d+0) 
term(19) = term(19) * (0.3333333333333333d+0) 
term(20) = term(20) * (-0.16666666666666666d+0) 
term(21) = term(21) * (-0.16666666666666666d+0) 
term(22) = term(22) * (-0.16666666666666666d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (-1.3333333333333333d+0) 
term(28) = term(28) * (1.3333333333333333d+0) 
term(29) = term(29) * (-1.3333333333333333d+0) 
term(30) = term(30) * (1.3333333333333333d+0) 
term(31) = term(31) * (-1.3333333333333333d+0) 
term(32) = term(32) * (1.3333333333333333d+0) 
term(33) = term(33) * (-0.6666666666666666d+0) 
term(34) = term(34) * (0.6666666666666666d+0) 
term(35) = term(35) * (-0.6666666666666666d+0) 
term(36) = term(36) * (0.6666666666666666d+0) 
term(37) = term(37) * (-1.3333333333333333d+0) 
term(38) = term(38) * (1.3333333333333333d+0) 
term(39) = term(39) * (-0.6666666666666666d+0) 
term(40) = term(40) * (0.6666666666666666d+0) 
term(41) = term(41) * (-0.6666666666666666d+0) 
term(42) = term(42) * (0.6666666666666666d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(43) = term(43) + r2(vrdav_Rl, a,j,q,i) * wm_interm_107_so_pt2(a,p,j,i)
term(44) = term(44) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt2(a,p,j,i)
term(45) = term(45) + r2(vrdav_Rl, a,j,q,i) * wm_interm_108_so_pt2(a,p,j,i)
end do 
end do 
end do 

term(43) = term(43) * (8.0d+0) 
term(44) = term(44) * (6.0d+0) 
term(45) = term(45) * (-8.0d+0) 

do i = 1, nocc 
term(46) = term(46) + s1(q,i) * wm_interm_32_so_pt2(p,i)
term(47) = term(47) + s1(q,i) * wm_interm_33_so_pt2(p,i)
term(48) = term(48) + s1(q,i) * wm_interm_36_so_pt2(p,i)
term(49) = term(49) + s1(q,i) * wm_interm_37_so_pt2(p,i)
term(50) = term(50) + t1(p,i) * wm_interm_44_so_pt2(q,i)
term(51) = term(51) + t1(p,i) * wm_interm_45_so_pt2(q,i)
term(52) = term(52) + t1(p,i) * wm_interm_43_so_pt2(q,i)
term(53) = term(53) + t1(p,i) * wm_interm_84_so_pt2(q,i)
term(54) = term(54) + t1(p,i) * wm_interm_85_so_pt2(q,i)
end do 

term(46) = term(46) * (-6.0d+0) 
term(47) = term(47) * (8.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-0.9999999999999999d+0) 
term(51) = term(51) * (1.9999999999999998d+0) 
term(52) = term(52) * (-0.9999999999999999d+0) 
term(53) = term(53) * (-3.9999999999999996d+0) 
term(54) = term(54) * (3.9999999999999996d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(55) = term(55) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_110_so_pt2(q,a,i,j)
term(56) = term(56) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_110_so_pt2(a,q,i,j)
term(57) = term(57) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_111_so_pt2(q,a,i,j)
term(58) = term(58) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_111_so_pt2(a,q,i,j)
term(59) = term(59) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_112_so_pt2(q,a,i,j)
term(60) = term(60) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt2(q,a,i,j)
term(61) = term(61) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt2(a,q,i,j)
term(62) = term(62) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_112_so_pt2(a,q,i,j)
term(63) = term(63) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_114_so_pt2(q,a,i,j)
term(64) = term(64) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_114_so_pt2(a,q,i,j)
end do 
end do 
end do 

term(55) = term(55) * (-2.0d+0) 
term(57) = term(57) * (0.6666666666666666d+0) 
term(58) = term(58) * (-0.3333333333333333d+0) 
term(59) = term(59) * (0.6666666666666666d+0) 
term(60) = term(60) * (-0.16666666666666666d+0) 
term(61) = term(61) * (0.3333333333333333d+0) 
term(62) = term(62) * (-0.3333333333333333d+0) 
term(63) = term(63) * (-0.16666666666666666d+0) 
term(64) = term(64) * (0.3333333333333333d+0) 


    calc_D_vv_wm_so_cc3_pt2 = zero
    do s = 0, 64
    calc_D_vv_wm_so_cc3_pt2 = calc_D_vv_wm_so_cc3_pt2 + term(s)
    end do

  end function calc_D_vv_wm_so_cc3_pt2
  
  end module so_cc3_pt012
