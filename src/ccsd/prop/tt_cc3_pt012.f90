module tt_cc3_pt012
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none

        !
    ! File generated automatically on 2018-04-17 23:17:26
    !

        real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_32_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_33_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_34_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_50_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_51_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_56_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_58_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_59_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_69_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_71_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_72_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_triplet_pt1 
    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_15_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_26_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_27_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_28_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_32_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_40_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_56_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_57_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_59_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_61_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_63_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_66_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_71_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_74_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_75_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_79_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_80_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_81_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_83_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_84_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_85_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_92_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_93_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_94_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_95_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_96_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_97_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_98_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_99_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_100_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_101_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_102_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_103_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_104_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_105_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_106_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_108_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_109_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_110_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_111_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_112_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_113_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_115_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_116_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_117_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_118_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_119_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_120_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_121_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_122_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_123_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_124_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_125_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_126_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_127_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_128_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_129_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_130_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_131_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_132_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_133_triplet_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_134_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_135_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_136_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_137_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_138_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_139_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_140_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_141_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_142_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_143_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_144_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_145_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_146_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_147_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_148_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_149_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_150_triplet_pt2 

    
    contains
    
    subroutine wm_triplet_intermediates_cc3_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_triplet_intermediates_cc3_init_pt0
    
    subroutine wm_triplet_intermediates_cc3_free_pt0
    
    end subroutine wm_triplet_intermediates_cc3_free_pt0
    
    subroutine wm_triplet_intermediates_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

  end subroutine wm_triplet_intermediates_cc3_pt0

      subroutine wm_triplet_intermediates_cc3_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_17_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_19_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_20_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_40_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_44_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_54_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_55_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_56_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_57_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_58_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_59_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_60_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_66_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_68_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_70_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_71_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_72_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_73_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_triplet_pt1 = zero 
wm_interm_1_triplet_pt1 = zero 
wm_interm_2_triplet_pt1 = zero 
wm_interm_3_triplet_pt1 = zero 
wm_interm_4_triplet_pt1 = zero 
wm_interm_5_triplet_pt1 = zero 
wm_interm_6_triplet_pt1 = zero 
wm_interm_7_triplet_pt1 = zero 
wm_interm_8_triplet_pt1 = zero 
wm_interm_9_triplet_pt1 = zero 
wm_interm_10_triplet_pt1 = zero 
wm_interm_11_triplet_pt1 = zero 
wm_interm_12_triplet_pt1 = zero 
wm_interm_13_triplet_pt1 = zero 
wm_interm_14_triplet_pt1 = zero 
wm_interm_15_triplet_pt1 = zero 
wm_interm_16_triplet_pt1 = zero 
wm_interm_17_triplet_pt1 = zero 
wm_interm_18_triplet_pt1 = zero 
wm_interm_19_triplet_pt1 = zero 
wm_interm_20_triplet_pt1 = zero 
wm_interm_21_triplet_pt1 = zero 
wm_interm_22_triplet_pt1 = zero 
wm_interm_23_triplet_pt1 = zero 
wm_interm_24_triplet_pt1 = zero 
wm_interm_25_triplet_pt1 = zero 
wm_interm_26_triplet_pt1 = zero 
wm_interm_27_triplet_pt1 = zero 
wm_interm_28_triplet_pt1 = zero 
wm_interm_29_triplet_pt1 = zero 
wm_interm_30_triplet_pt1 = zero 
wm_interm_31_triplet_pt1 = zero 
wm_interm_32_triplet_pt1 = zero 
wm_interm_33_triplet_pt1 = zero 
wm_interm_34_triplet_pt1 = zero 
wm_interm_35_triplet_pt1 = zero 
wm_interm_36_triplet_pt1 = zero 
wm_interm_37_triplet_pt1 = zero 
wm_interm_38_triplet_pt1 = zero 
wm_interm_39_triplet_pt1 = zero 
wm_interm_40_triplet_pt1 = zero 
wm_interm_41_triplet_pt1 = zero 
wm_interm_42_triplet_pt1 = zero 
wm_interm_43_triplet_pt1 = zero 
wm_interm_44_triplet_pt1 = zero 
wm_interm_45_triplet_pt1 = zero 
wm_interm_46_triplet_pt1 = zero 
wm_interm_47_triplet_pt1 = zero 
wm_interm_48_triplet_pt1 = zero 
wm_interm_49_triplet_pt1 = zero 
wm_interm_50_triplet_pt1 = zero 
wm_interm_51_triplet_pt1 = zero 
wm_interm_52_triplet_pt1 = zero 
wm_interm_53_triplet_pt1 = zero 
wm_interm_54_triplet_pt1 = zero 
wm_interm_55_triplet_pt1 = zero 
wm_interm_56_triplet_pt1 = zero 
wm_interm_57_triplet_pt1 = zero 
wm_interm_58_triplet_pt1 = zero 
wm_interm_59_triplet_pt1 = zero 
wm_interm_60_triplet_pt1 = zero 
wm_interm_61_triplet_pt1 = zero 
wm_interm_62_triplet_pt1 = zero 
wm_interm_63_triplet_pt1 = zero 
wm_interm_64_triplet_pt1 = zero 
wm_interm_65_triplet_pt1 = zero 
wm_interm_66_triplet_pt1 = zero 
wm_interm_67_triplet_pt1 = zero 
wm_interm_68_triplet_pt1 = zero 
wm_interm_69_triplet_pt1 = zero 
wm_interm_70_triplet_pt1 = zero 
wm_interm_71_triplet_pt1 = zero 
wm_interm_72_triplet_pt1 = zero 
wm_interm_73_triplet_pt1 = zero 
wm_interm_74_triplet_pt1 = zero 

    end subroutine wm_triplet_intermediates_cc3_init_pt1
    
    subroutine wm_triplet_intermediates_cc3_free_pt1
    deallocate(wm_interm_0_triplet_pt1)
deallocate(wm_interm_1_triplet_pt1)
deallocate(wm_interm_2_triplet_pt1)
deallocate(wm_interm_3_triplet_pt1)
deallocate(wm_interm_4_triplet_pt1)
deallocate(wm_interm_5_triplet_pt1)
deallocate(wm_interm_6_triplet_pt1)
deallocate(wm_interm_7_triplet_pt1)
deallocate(wm_interm_8_triplet_pt1)
deallocate(wm_interm_9_triplet_pt1)
deallocate(wm_interm_10_triplet_pt1)
deallocate(wm_interm_11_triplet_pt1)
deallocate(wm_interm_12_triplet_pt1)
deallocate(wm_interm_13_triplet_pt1)
deallocate(wm_interm_14_triplet_pt1)
deallocate(wm_interm_15_triplet_pt1)
deallocate(wm_interm_16_triplet_pt1)
deallocate(wm_interm_17_triplet_pt1)
deallocate(wm_interm_18_triplet_pt1)
deallocate(wm_interm_19_triplet_pt1)
deallocate(wm_interm_20_triplet_pt1)
deallocate(wm_interm_21_triplet_pt1)
deallocate(wm_interm_22_triplet_pt1)
deallocate(wm_interm_23_triplet_pt1)
deallocate(wm_interm_24_triplet_pt1)
deallocate(wm_interm_25_triplet_pt1)
deallocate(wm_interm_26_triplet_pt1)
deallocate(wm_interm_27_triplet_pt1)
deallocate(wm_interm_28_triplet_pt1)
deallocate(wm_interm_29_triplet_pt1)
deallocate(wm_interm_30_triplet_pt1)
deallocate(wm_interm_31_triplet_pt1)
deallocate(wm_interm_32_triplet_pt1)
deallocate(wm_interm_33_triplet_pt1)
deallocate(wm_interm_34_triplet_pt1)
deallocate(wm_interm_35_triplet_pt1)
deallocate(wm_interm_36_triplet_pt1)
deallocate(wm_interm_37_triplet_pt1)
deallocate(wm_interm_38_triplet_pt1)
deallocate(wm_interm_39_triplet_pt1)
deallocate(wm_interm_40_triplet_pt1)
deallocate(wm_interm_41_triplet_pt1)
deallocate(wm_interm_42_triplet_pt1)
deallocate(wm_interm_43_triplet_pt1)
deallocate(wm_interm_44_triplet_pt1)
deallocate(wm_interm_45_triplet_pt1)
deallocate(wm_interm_46_triplet_pt1)
deallocate(wm_interm_47_triplet_pt1)
deallocate(wm_interm_48_triplet_pt1)
deallocate(wm_interm_49_triplet_pt1)
deallocate(wm_interm_50_triplet_pt1)
deallocate(wm_interm_51_triplet_pt1)
deallocate(wm_interm_52_triplet_pt1)
deallocate(wm_interm_53_triplet_pt1)
deallocate(wm_interm_54_triplet_pt1)
deallocate(wm_interm_55_triplet_pt1)
deallocate(wm_interm_56_triplet_pt1)
deallocate(wm_interm_57_triplet_pt1)
deallocate(wm_interm_58_triplet_pt1)
deallocate(wm_interm_59_triplet_pt1)
deallocate(wm_interm_60_triplet_pt1)
deallocate(wm_interm_61_triplet_pt1)
deallocate(wm_interm_62_triplet_pt1)
deallocate(wm_interm_63_triplet_pt1)
deallocate(wm_interm_64_triplet_pt1)
deallocate(wm_interm_65_triplet_pt1)
deallocate(wm_interm_66_triplet_pt1)
deallocate(wm_interm_67_triplet_pt1)
deallocate(wm_interm_68_triplet_pt1)
deallocate(wm_interm_69_triplet_pt1)
deallocate(wm_interm_70_triplet_pt1)
deallocate(wm_interm_71_triplet_pt1)
deallocate(wm_interm_72_triplet_pt1)
deallocate(wm_interm_73_triplet_pt1)
deallocate(wm_interm_74_triplet_pt1)

    end subroutine wm_triplet_intermediates_cc3_free_pt1
    
    subroutine wm_triplet_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_triplet_pt1(c, k) = wm_interm_0_triplet_pt1(c, k) + sum 
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
wm_interm_1_triplet_pt1(c, k) = wm_interm_1_triplet_pt1(c, k) + sum 
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
wm_interm_2_triplet_pt1(c, k) = wm_interm_2_triplet_pt1(c, k) + sum 
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
wm_interm_3_triplet_pt1(c, k) = wm_interm_3_triplet_pt1(c, k) + sum 
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
wm_interm_4_triplet_pt1(b, c, j, k) = wm_interm_4_triplet_pt1(b, c, j, k) + sum 
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
wm_interm_5_triplet_pt1(b, c, j, k) = wm_interm_5_triplet_pt1(b, c, j, k) + sum 
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
wm_interm_6_triplet_pt1(c, k) = wm_interm_6_triplet_pt1(c, k) + sum 
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
wm_interm_7_triplet_pt1(c, k) = wm_interm_7_triplet_pt1(c, k) + sum 
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
wm_interm_8_triplet_pt1(c, k) = wm_interm_8_triplet_pt1(c, k) + sum 
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
wm_interm_9_triplet_pt1(c, k) = wm_interm_9_triplet_pt1(c, k) + sum 
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
wm_interm_10_triplet_pt1(b, c, k, j) = wm_interm_10_triplet_pt1(b, c, k, j) + sum 
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
wm_interm_11_triplet_pt1(b, c, k, j) = wm_interm_11_triplet_pt1(b, c, k, j) + sum 
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
wm_interm_12_triplet_pt1(b, c, j, k) = wm_interm_12_triplet_pt1(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_13_triplet_pt1(b, c, j, k) = wm_interm_13_triplet_pt1(b, c, j, k) + sum 
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
wm_interm_14_triplet_pt1(b, c, j, k) = wm_interm_14_triplet_pt1(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_15_triplet_pt1(c, k) = wm_interm_15_triplet_pt1(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_16_triplet_pt1(c, k) = wm_interm_16_triplet_pt1(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_17_triplet_pt1(c, k) = wm_interm_17_triplet_pt1(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_18_triplet_pt1(c, k) = wm_interm_18_triplet_pt1(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_19_triplet_pt1(c, k) = wm_interm_19_triplet_pt1(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_20_triplet_pt1(c, k) = wm_interm_20_triplet_pt1(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_21_triplet_pt1(c, j, k, l) = wm_interm_21_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_22_triplet_pt1(c, j, k, l) = wm_interm_22_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_23_triplet_pt1(c, j, k, l) = wm_interm_23_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_24_triplet_pt1(c, j, k, l) = wm_interm_24_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_25_triplet_pt1(c, j, k, l) = wm_interm_25_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_26_triplet_pt1(c, j, k, l) = wm_interm_26_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_27_triplet_pt1(c, k, j, l) = wm_interm_27_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_28_triplet_pt1(c, j, k, l) = wm_interm_28_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_29_triplet_pt1(c, j, k, l) = wm_interm_29_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_30_triplet_pt1(c, k, j, l) = wm_interm_30_triplet_pt1(c, k, j, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_31_triplet_pt1(c, k) = wm_interm_31_triplet_pt1(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_32_triplet_pt1(c, k) = wm_interm_32_triplet_pt1(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_33_triplet_pt1(c, k) = wm_interm_33_triplet_pt1(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_34_triplet_pt1(c, k) = wm_interm_34_triplet_pt1(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_35_triplet_pt1(c, j, k, l) = wm_interm_35_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_36_triplet_pt1(c, j, k, l) = wm_interm_36_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_37_triplet_pt1(c, j, k, l) = wm_interm_37_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_38_triplet_pt1(c, j, k, l) = wm_interm_38_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_39_triplet_pt1(c, j, k, l) = wm_interm_39_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_40_triplet_pt1(c, j, k, l) = wm_interm_40_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_41_triplet_pt1(c, j, k, l) = wm_interm_41_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_42_triplet_pt1(c, k, j, l) = wm_interm_42_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_43_triplet_pt1(c, k, j, l) = wm_interm_43_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_44_triplet_pt1(c, j, k, l) = wm_interm_44_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_45_triplet_pt1(c, k, j, l) = wm_interm_45_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_46_triplet_pt1(c, k, j, l) = wm_interm_46_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_47_triplet_pt1(c, k, j, l) = wm_interm_47_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_48_triplet_pt1(c, j, k, l) = wm_interm_48_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_49_triplet_pt1(c, j, k, l) = wm_interm_49_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_50_triplet_pt1(c, k) = wm_interm_50_triplet_pt1(c, k) + sum 
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
wm_interm_51_triplet_pt1(c, k) = wm_interm_51_triplet_pt1(c, k) + sum 
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
wm_interm_52_triplet_pt1(c, j, k, l) = wm_interm_52_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_53_triplet_pt1(c, k) = wm_interm_53_triplet_pt1(c, k) + sum 
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
wm_interm_54_triplet_pt1(c, j, k, l) = wm_interm_54_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, b,i,a,l)
end do 
end do 
end do 
wm_interm_55_triplet_pt1(c, k, j, l) = wm_interm_55_triplet_pt1(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
end do 
end do 
wm_interm_56_triplet_pt1(c, k) = wm_interm_56_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_57_triplet_pt1(c, k) = wm_interm_57_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_58_triplet_pt1(c, k) = wm_interm_58_triplet_pt1(c, k) + sum 
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
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_59_triplet_pt1(c, k) = wm_interm_59_triplet_pt1(c, k) + sum 
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
wm_interm_60_triplet_pt1(c, j, k, l) = wm_interm_60_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_61_triplet_pt1(c, j, k, l) = wm_interm_61_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_62_triplet_pt1(c, j, k, l) = wm_interm_62_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, b,i,a,l)
end do 
end do 
end do 
wm_interm_63_triplet_pt1(c, j, k, l) = wm_interm_63_triplet_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
end do 
end do 
wm_interm_64_triplet_pt1(c, k) = wm_interm_64_triplet_pt1(c, k) + sum 
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
wm_interm_65_triplet_pt1(c, k, j, l) = wm_interm_65_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_66_triplet_pt1(c, k, j, l) = wm_interm_66_triplet_pt1(c, k, j, l) + sum 
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
wm_interm_67_triplet_pt1(c, j, k, l) = wm_interm_67_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_68_triplet_pt1(c, j, k, l) = wm_interm_68_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_69_triplet_pt1(c, k) = wm_interm_69_triplet_pt1(c, k) + sum 
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
wm_interm_70_triplet_pt1(c, k) = wm_interm_70_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_71_triplet_pt1(c, k) = wm_interm_71_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_72_triplet_pt1(c, k) = wm_interm_72_triplet_pt1(c, k) + sum 
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
wm_interm_73_triplet_pt1(c, j, k, l) = wm_interm_73_triplet_pt1(c, j, k, l) + sum 
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
wm_interm_74_triplet_pt1(c, j, k, l) = wm_interm_74_triplet_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt1
    
    subroutine wm_triplet_intermediates_cc3_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_8_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_15_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_18_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_19_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_31_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_33_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_35_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_40_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_47_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_52_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_53_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_55_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_56_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_57_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_59_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_60_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_62_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_63_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_67_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_68_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_71_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_72_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_73_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_75_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_76_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_77_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_79_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_80_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_81_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_82_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_83_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_84_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_85_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_86_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_87_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_88_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_89_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_90_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_91_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_92_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_93_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_94_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_95_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_96_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_97_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_98_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_99_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_100_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_101_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_102_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_103_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_104_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_105_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_106_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_107_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_108_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_109_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_110_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_111_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_112_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_113_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_114_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_115_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_116_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_117_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_118_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_119_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_120_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_121_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_122_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_123_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_124_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_125_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_126_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_127_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_128_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_129_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_130_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_131_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_132_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_133_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_134_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_135_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_136_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_137_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_138_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_139_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_140_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_141_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_142_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_143_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_144_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_145_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_146_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_147_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_148_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_149_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_150_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_0_triplet_pt2 = zero 
wm_interm_1_triplet_pt2 = zero 
wm_interm_2_triplet_pt2 = zero 
wm_interm_3_triplet_pt2 = zero 
wm_interm_4_triplet_pt2 = zero 
wm_interm_5_triplet_pt2 = zero 
wm_interm_6_triplet_pt2 = zero 
wm_interm_7_triplet_pt2 = zero 
wm_interm_8_triplet_pt2 = zero 
wm_interm_9_triplet_pt2 = zero 
wm_interm_10_triplet_pt2 = zero 
wm_interm_11_triplet_pt2 = zero 
wm_interm_12_triplet_pt2 = zero 
wm_interm_13_triplet_pt2 = zero 
wm_interm_14_triplet_pt2 = zero 
wm_interm_15_triplet_pt2 = zero 
wm_interm_16_triplet_pt2 = zero 
wm_interm_17_triplet_pt2 = zero 
wm_interm_18_triplet_pt2 = zero 
wm_interm_19_triplet_pt2 = zero 
wm_interm_20_triplet_pt2 = zero 
wm_interm_21_triplet_pt2 = zero 
wm_interm_22_triplet_pt2 = zero 
wm_interm_23_triplet_pt2 = zero 
wm_interm_24_triplet_pt2 = zero 
wm_interm_25_triplet_pt2 = zero 
wm_interm_26_triplet_pt2 = zero 
wm_interm_27_triplet_pt2 = zero 
wm_interm_28_triplet_pt2 = zero 
wm_interm_29_triplet_pt2 = zero 
wm_interm_30_triplet_pt2 = zero 
wm_interm_31_triplet_pt2 = zero 
wm_interm_32_triplet_pt2 = zero 
wm_interm_33_triplet_pt2 = zero 
wm_interm_34_triplet_pt2 = zero 
wm_interm_35_triplet_pt2 = zero 
wm_interm_36_triplet_pt2 = zero 
wm_interm_37_triplet_pt2 = zero 
wm_interm_38_triplet_pt2 = zero 
wm_interm_39_triplet_pt2 = zero 
wm_interm_40_triplet_pt2 = zero 
wm_interm_41_triplet_pt2 = zero 
wm_interm_42_triplet_pt2 = zero 
wm_interm_43_triplet_pt2 = zero 
wm_interm_44_triplet_pt2 = zero 
wm_interm_45_triplet_pt2 = zero 
wm_interm_46_triplet_pt2 = zero 
wm_interm_47_triplet_pt2 = zero 
wm_interm_48_triplet_pt2 = zero 
wm_interm_49_triplet_pt2 = zero 
wm_interm_50_triplet_pt2 = zero 
wm_interm_51_triplet_pt2 = zero 
wm_interm_52_triplet_pt2 = zero 
wm_interm_53_triplet_pt2 = zero 
wm_interm_54_triplet_pt2 = zero 
wm_interm_55_triplet_pt2 = zero 
wm_interm_56_triplet_pt2 = zero 
wm_interm_57_triplet_pt2 = zero 
wm_interm_58_triplet_pt2 = zero 
wm_interm_59_triplet_pt2 = zero 
wm_interm_60_triplet_pt2 = zero 
wm_interm_61_triplet_pt2 = zero 
wm_interm_62_triplet_pt2 = zero 
wm_interm_63_triplet_pt2 = zero 
wm_interm_64_triplet_pt2 = zero 
wm_interm_65_triplet_pt2 = zero 
wm_interm_66_triplet_pt2 = zero 
wm_interm_67_triplet_pt2 = zero 
wm_interm_68_triplet_pt2 = zero 
wm_interm_69_triplet_pt2 = zero 
wm_interm_70_triplet_pt2 = zero 
wm_interm_71_triplet_pt2 = zero 
wm_interm_72_triplet_pt2 = zero 
wm_interm_73_triplet_pt2 = zero 
wm_interm_74_triplet_pt2 = zero 
wm_interm_75_triplet_pt2 = zero 
wm_interm_76_triplet_pt2 = zero 
wm_interm_77_triplet_pt2 = zero 
wm_interm_78_triplet_pt2 = zero 
wm_interm_79_triplet_pt2 = zero 
wm_interm_80_triplet_pt2 = zero 
wm_interm_81_triplet_pt2 = zero 
wm_interm_82_triplet_pt2 = zero 
wm_interm_83_triplet_pt2 = zero 
wm_interm_84_triplet_pt2 = zero 
wm_interm_85_triplet_pt2 = zero 
wm_interm_86_triplet_pt2 = zero 
wm_interm_87_triplet_pt2 = zero 
wm_interm_88_triplet_pt2 = zero 
wm_interm_89_triplet_pt2 = zero 
wm_interm_90_triplet_pt2 = zero 
wm_interm_91_triplet_pt2 = zero 
wm_interm_92_triplet_pt2 = zero 
wm_interm_93_triplet_pt2 = zero 
wm_interm_94_triplet_pt2 = zero 
wm_interm_95_triplet_pt2 = zero 
wm_interm_96_triplet_pt2 = zero 
wm_interm_97_triplet_pt2 = zero 
wm_interm_98_triplet_pt2 = zero 
wm_interm_99_triplet_pt2 = zero 
wm_interm_100_triplet_pt2 = zero 
wm_interm_101_triplet_pt2 = zero 
wm_interm_102_triplet_pt2 = zero 
wm_interm_103_triplet_pt2 = zero 
wm_interm_104_triplet_pt2 = zero 
wm_interm_105_triplet_pt2 = zero 
wm_interm_106_triplet_pt2 = zero 
wm_interm_107_triplet_pt2 = zero 
wm_interm_108_triplet_pt2 = zero 
wm_interm_109_triplet_pt2 = zero 
wm_interm_110_triplet_pt2 = zero 
wm_interm_111_triplet_pt2 = zero 
wm_interm_112_triplet_pt2 = zero 
wm_interm_113_triplet_pt2 = zero 
wm_interm_114_triplet_pt2 = zero 
wm_interm_115_triplet_pt2 = zero 
wm_interm_116_triplet_pt2 = zero 
wm_interm_117_triplet_pt2 = zero 
wm_interm_118_triplet_pt2 = zero 
wm_interm_119_triplet_pt2 = zero 
wm_interm_120_triplet_pt2 = zero 
wm_interm_121_triplet_pt2 = zero 
wm_interm_122_triplet_pt2 = zero 
wm_interm_123_triplet_pt2 = zero 
wm_interm_124_triplet_pt2 = zero 
wm_interm_125_triplet_pt2 = zero 
wm_interm_126_triplet_pt2 = zero 
wm_interm_127_triplet_pt2 = zero 
wm_interm_128_triplet_pt2 = zero 
wm_interm_129_triplet_pt2 = zero 
wm_interm_130_triplet_pt2 = zero 
wm_interm_131_triplet_pt2 = zero 
wm_interm_132_triplet_pt2 = zero 
wm_interm_133_triplet_pt2 = zero 
wm_interm_134_triplet_pt2 = zero 
wm_interm_135_triplet_pt2 = zero 
wm_interm_136_triplet_pt2 = zero 
wm_interm_137_triplet_pt2 = zero 
wm_interm_138_triplet_pt2 = zero 
wm_interm_139_triplet_pt2 = zero 
wm_interm_140_triplet_pt2 = zero 
wm_interm_141_triplet_pt2 = zero 
wm_interm_142_triplet_pt2 = zero 
wm_interm_143_triplet_pt2 = zero 
wm_interm_144_triplet_pt2 = zero 
wm_interm_145_triplet_pt2 = zero 
wm_interm_146_triplet_pt2 = zero 
wm_interm_147_triplet_pt2 = zero 
wm_interm_148_triplet_pt2 = zero 
wm_interm_149_triplet_pt2 = zero 
wm_interm_150_triplet_pt2 = zero 

    end subroutine wm_triplet_intermediates_cc3_init_pt2
    
    subroutine wm_triplet_intermediates_cc3_free_pt2
    deallocate(wm_interm_0_triplet_pt2)
deallocate(wm_interm_1_triplet_pt2)
deallocate(wm_interm_2_triplet_pt2)
deallocate(wm_interm_3_triplet_pt2)
deallocate(wm_interm_4_triplet_pt2)
deallocate(wm_interm_5_triplet_pt2)
deallocate(wm_interm_6_triplet_pt2)
deallocate(wm_interm_7_triplet_pt2)
deallocate(wm_interm_8_triplet_pt2)
deallocate(wm_interm_9_triplet_pt2)
deallocate(wm_interm_10_triplet_pt2)
deallocate(wm_interm_11_triplet_pt2)
deallocate(wm_interm_12_triplet_pt2)
deallocate(wm_interm_13_triplet_pt2)
deallocate(wm_interm_14_triplet_pt2)
deallocate(wm_interm_15_triplet_pt2)
deallocate(wm_interm_16_triplet_pt2)
deallocate(wm_interm_17_triplet_pt2)
deallocate(wm_interm_18_triplet_pt2)
deallocate(wm_interm_19_triplet_pt2)
deallocate(wm_interm_20_triplet_pt2)
deallocate(wm_interm_21_triplet_pt2)
deallocate(wm_interm_22_triplet_pt2)
deallocate(wm_interm_23_triplet_pt2)
deallocate(wm_interm_24_triplet_pt2)
deallocate(wm_interm_25_triplet_pt2)
deallocate(wm_interm_26_triplet_pt2)
deallocate(wm_interm_27_triplet_pt2)
deallocate(wm_interm_28_triplet_pt2)
deallocate(wm_interm_29_triplet_pt2)
deallocate(wm_interm_30_triplet_pt2)
deallocate(wm_interm_31_triplet_pt2)
deallocate(wm_interm_32_triplet_pt2)
deallocate(wm_interm_33_triplet_pt2)
deallocate(wm_interm_34_triplet_pt2)
deallocate(wm_interm_35_triplet_pt2)
deallocate(wm_interm_36_triplet_pt2)
deallocate(wm_interm_37_triplet_pt2)
deallocate(wm_interm_38_triplet_pt2)
deallocate(wm_interm_39_triplet_pt2)
deallocate(wm_interm_40_triplet_pt2)
deallocate(wm_interm_41_triplet_pt2)
deallocate(wm_interm_42_triplet_pt2)
deallocate(wm_interm_43_triplet_pt2)
deallocate(wm_interm_44_triplet_pt2)
deallocate(wm_interm_45_triplet_pt2)
deallocate(wm_interm_46_triplet_pt2)
deallocate(wm_interm_47_triplet_pt2)
deallocate(wm_interm_48_triplet_pt2)
deallocate(wm_interm_49_triplet_pt2)
deallocate(wm_interm_50_triplet_pt2)
deallocate(wm_interm_51_triplet_pt2)
deallocate(wm_interm_52_triplet_pt2)
deallocate(wm_interm_53_triplet_pt2)
deallocate(wm_interm_54_triplet_pt2)
deallocate(wm_interm_55_triplet_pt2)
deallocate(wm_interm_56_triplet_pt2)
deallocate(wm_interm_57_triplet_pt2)
deallocate(wm_interm_58_triplet_pt2)
deallocate(wm_interm_59_triplet_pt2)
deallocate(wm_interm_60_triplet_pt2)
deallocate(wm_interm_61_triplet_pt2)
deallocate(wm_interm_62_triplet_pt2)
deallocate(wm_interm_63_triplet_pt2)
deallocate(wm_interm_64_triplet_pt2)
deallocate(wm_interm_65_triplet_pt2)
deallocate(wm_interm_66_triplet_pt2)
deallocate(wm_interm_67_triplet_pt2)
deallocate(wm_interm_68_triplet_pt2)
deallocate(wm_interm_69_triplet_pt2)
deallocate(wm_interm_70_triplet_pt2)
deallocate(wm_interm_71_triplet_pt2)
deallocate(wm_interm_72_triplet_pt2)
deallocate(wm_interm_73_triplet_pt2)
deallocate(wm_interm_74_triplet_pt2)
deallocate(wm_interm_75_triplet_pt2)
deallocate(wm_interm_76_triplet_pt2)
deallocate(wm_interm_77_triplet_pt2)
deallocate(wm_interm_78_triplet_pt2)
deallocate(wm_interm_79_triplet_pt2)
deallocate(wm_interm_80_triplet_pt2)
deallocate(wm_interm_81_triplet_pt2)
deallocate(wm_interm_82_triplet_pt2)
deallocate(wm_interm_83_triplet_pt2)
deallocate(wm_interm_84_triplet_pt2)
deallocate(wm_interm_85_triplet_pt2)
deallocate(wm_interm_86_triplet_pt2)
deallocate(wm_interm_87_triplet_pt2)
deallocate(wm_interm_88_triplet_pt2)
deallocate(wm_interm_89_triplet_pt2)
deallocate(wm_interm_90_triplet_pt2)
deallocate(wm_interm_91_triplet_pt2)
deallocate(wm_interm_92_triplet_pt2)
deallocate(wm_interm_93_triplet_pt2)
deallocate(wm_interm_94_triplet_pt2)
deallocate(wm_interm_95_triplet_pt2)
deallocate(wm_interm_96_triplet_pt2)
deallocate(wm_interm_97_triplet_pt2)
deallocate(wm_interm_98_triplet_pt2)
deallocate(wm_interm_99_triplet_pt2)
deallocate(wm_interm_100_triplet_pt2)
deallocate(wm_interm_101_triplet_pt2)
deallocate(wm_interm_102_triplet_pt2)
deallocate(wm_interm_103_triplet_pt2)
deallocate(wm_interm_104_triplet_pt2)
deallocate(wm_interm_105_triplet_pt2)
deallocate(wm_interm_106_triplet_pt2)
deallocate(wm_interm_107_triplet_pt2)
deallocate(wm_interm_108_triplet_pt2)
deallocate(wm_interm_109_triplet_pt2)
deallocate(wm_interm_110_triplet_pt2)
deallocate(wm_interm_111_triplet_pt2)
deallocate(wm_interm_112_triplet_pt2)
deallocate(wm_interm_113_triplet_pt2)
deallocate(wm_interm_114_triplet_pt2)
deallocate(wm_interm_115_triplet_pt2)
deallocate(wm_interm_116_triplet_pt2)
deallocate(wm_interm_117_triplet_pt2)
deallocate(wm_interm_118_triplet_pt2)
deallocate(wm_interm_119_triplet_pt2)
deallocate(wm_interm_120_triplet_pt2)
deallocate(wm_interm_121_triplet_pt2)
deallocate(wm_interm_122_triplet_pt2)
deallocate(wm_interm_123_triplet_pt2)
deallocate(wm_interm_124_triplet_pt2)
deallocate(wm_interm_125_triplet_pt2)
deallocate(wm_interm_126_triplet_pt2)
deallocate(wm_interm_127_triplet_pt2)
deallocate(wm_interm_128_triplet_pt2)
deallocate(wm_interm_129_triplet_pt2)
deallocate(wm_interm_130_triplet_pt2)
deallocate(wm_interm_131_triplet_pt2)
deallocate(wm_interm_132_triplet_pt2)
deallocate(wm_interm_133_triplet_pt2)
deallocate(wm_interm_134_triplet_pt2)
deallocate(wm_interm_135_triplet_pt2)
deallocate(wm_interm_136_triplet_pt2)
deallocate(wm_interm_137_triplet_pt2)
deallocate(wm_interm_138_triplet_pt2)
deallocate(wm_interm_139_triplet_pt2)
deallocate(wm_interm_140_triplet_pt2)
deallocate(wm_interm_141_triplet_pt2)
deallocate(wm_interm_142_triplet_pt2)
deallocate(wm_interm_143_triplet_pt2)
deallocate(wm_interm_144_triplet_pt2)
deallocate(wm_interm_145_triplet_pt2)
deallocate(wm_interm_146_triplet_pt2)
deallocate(wm_interm_147_triplet_pt2)
deallocate(wm_interm_148_triplet_pt2)
deallocate(wm_interm_149_triplet_pt2)
deallocate(wm_interm_150_triplet_pt2)

    end subroutine wm_triplet_intermediates_cc3_free_pt2
    
    subroutine wm_triplet_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, c, j, k, l, m 

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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_0_triplet_pt2(c, j, k, l) = wm_interm_0_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_1_triplet_pt2(j, k) = wm_interm_1_triplet_pt2(j, k) + sum 
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
wm_interm_2_triplet_pt2(j, k) = wm_interm_2_triplet_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_3_triplet_pt2(c, j, k, l) = wm_interm_3_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_4_triplet_pt2(c, j, k, l) = wm_interm_4_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_5_triplet_pt2(b, c) = wm_interm_5_triplet_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_6_triplet_pt2(b, c, j, k) = wm_interm_6_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_7_triplet_pt2(b, c) = wm_interm_7_triplet_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_8_triplet_pt2(b, c, j, k) = wm_interm_8_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_9_triplet_pt2(c, j, k, l) = wm_interm_9_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_10_triplet_pt2(c, j, k, l) = wm_interm_10_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_11_triplet_pt2(c, j, k, l) = wm_interm_11_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_12_triplet_pt2(b, c, j, k) = wm_interm_12_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_13_triplet_pt2(b, c, j, k) = wm_interm_13_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_14_triplet_pt2(b, c) = wm_interm_14_triplet_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,m)
end do 
end do 
wm_interm_15_triplet_pt2(c, i, j, k, l, m) = wm_interm_15_triplet_pt2(c, i, j, k, l, m) + sum 
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
wm_interm_16_triplet_pt2(i, j, k, l) = wm_interm_16_triplet_pt2(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_17_triplet_pt2(b, c, j, k) = wm_interm_17_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_18_triplet_pt2(j, k) = wm_interm_18_triplet_pt2(j, k) + sum 
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
wm_interm_19_triplet_pt2(c, k, j, l) = wm_interm_19_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_20_triplet_pt2(c, k, j, l) = wm_interm_20_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_21_triplet_pt2(c, j, k, l) = wm_interm_21_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_22_triplet_pt2(c, j, k, l) = wm_interm_22_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_23_triplet_pt2(c, j, k, l) = wm_interm_23_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_24_triplet_pt2(c, j, k, l) = wm_interm_24_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_25_triplet_pt2(i, j, k, l) = wm_interm_25_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_26_triplet_pt2(c, i, k, j, l, m) = wm_interm_26_triplet_pt2(c, i, k, j, l, m) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,m)
end do 
end do 
wm_interm_27_triplet_pt2(c, j, k, i, l, m) = wm_interm_27_triplet_pt2(c, j, k, i, l, m) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,l,m)
end do 
end do 
wm_interm_28_triplet_pt2(c, i, j, k, l, m) = wm_interm_28_triplet_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_29_triplet_pt2(b, c, j, k) = wm_interm_29_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_30_triplet_pt2(b, c) = wm_interm_30_triplet_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_31_triplet_pt2(j, k) = wm_interm_31_triplet_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_32_triplet_pt2(b, c) = wm_interm_32_triplet_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_33_triplet_pt2(b, c, j, k) = wm_interm_33_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_34_triplet_pt2(j, k) = wm_interm_34_triplet_pt2(j, k) + sum 
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
wm_interm_35_triplet_pt2(c, k) = wm_interm_35_triplet_pt2(c, k) + sum 
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
wm_interm_36_triplet_pt2(c, k) = wm_interm_36_triplet_pt2(c, k) + sum 
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
wm_interm_37_triplet_pt2(c, k) = wm_interm_37_triplet_pt2(c, k) + sum 
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
wm_interm_38_triplet_pt2(c, k) = wm_interm_38_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_39_triplet_pt2(c, k) = wm_interm_39_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_40_triplet_pt2(c, k) = wm_interm_40_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_41_triplet_pt2(c, k) = wm_interm_41_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_42_triplet_pt2(c, k) = wm_interm_42_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_43_triplet_pt2(c, k) = wm_interm_43_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_44_triplet_pt2(c, k) = wm_interm_44_triplet_pt2(c, k) + sum 
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
wm_interm_45_triplet_pt2(b, c, j, k) = wm_interm_45_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_46_triplet_pt2(b, c, j, k) = wm_interm_46_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_47_triplet_pt2(b, c, j, k) = wm_interm_47_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_48_triplet_pt2(b, c, j, k) = wm_interm_48_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_49_triplet_pt2(c, j, k, l) = wm_interm_49_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_50_triplet_pt2(c, j, k, l) = wm_interm_50_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,i,c,l)
end do 
end do 
end do 
wm_interm_51_triplet_pt2(c, j, k, l) = wm_interm_51_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_52_triplet_pt2(b, c, j, k) = wm_interm_52_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_53_triplet_pt2(b, c, j, k) = wm_interm_53_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_54_triplet_pt2(c, j, k, l) = wm_interm_54_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_55_triplet_pt2(b, c, j, k) = wm_interm_55_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_56_triplet_pt2(b, c) = wm_interm_56_triplet_pt2(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,m)
end do 
end do 
wm_interm_57_triplet_pt2(c, i, j, k, l, m) = wm_interm_57_triplet_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_58_triplet_pt2(b, c, j, k) = wm_interm_58_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_59_triplet_pt2(j, k) = wm_interm_59_triplet_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_60_triplet_pt2(i, j, k, l) = wm_interm_60_triplet_pt2(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_61_triplet_pt2(b, c) = wm_interm_61_triplet_pt2(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_62_triplet_pt2(j, k) = wm_interm_62_triplet_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_63_triplet_pt2(c, k) = wm_interm_63_triplet_pt2(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_64_triplet_pt2(c, k) = wm_interm_64_triplet_pt2(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_65_triplet_pt2(c, k) = wm_interm_65_triplet_pt2(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_66_triplet_pt2(c, k) = wm_interm_66_triplet_pt2(c, k) + sum 
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
wm_interm_67_triplet_pt2(c, j, k, l) = wm_interm_67_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_68_triplet_pt2(c, j, k, l) = wm_interm_68_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_69_triplet_pt2(c, j, k, l) = wm_interm_69_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_70_triplet_pt2(c, k) = wm_interm_70_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
end do 
end do 
wm_interm_71_triplet_pt2(c, k) = wm_interm_71_triplet_pt2(c, k) + sum 
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
wm_interm_72_triplet_pt2(c, k, j, l) = wm_interm_72_triplet_pt2(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2p(vrdav_Rr, b,i,a,l)
end do 
end do 
end do 
wm_interm_73_triplet_pt2(c, k, j, l) = wm_interm_73_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_74_triplet_pt2(c, k) = wm_interm_74_triplet_pt2(c, k) + sum 
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
wm_interm_75_triplet_pt2(c, k, j, l) = wm_interm_75_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_76_triplet_pt2(c, j, k, l) = wm_interm_76_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_77_triplet_pt2(c, j, k, l) = wm_interm_77_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2p(vrdav_Rr, b,i,a,l)
end do 
end do 
end do 
wm_interm_78_triplet_pt2(c, j, k, l) = wm_interm_78_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_79_triplet_pt2(c, k) = wm_interm_79_triplet_pt2(c, k) + sum 
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
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_80_triplet_pt2(c, k) = wm_interm_80_triplet_pt2(c, k) + sum 
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
wm_interm_81_triplet_pt2(c, k) = wm_interm_81_triplet_pt2(c, k) + sum 
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
wm_interm_82_triplet_pt2(c, j, k, l) = wm_interm_82_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
end do 
end do 
wm_interm_83_triplet_pt2(c, k) = wm_interm_83_triplet_pt2(c, k) + sum 
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
wm_interm_84_triplet_pt2(b, c, j, k) = wm_interm_84_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_85_triplet_pt2(b, c, j, k) = wm_interm_85_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_86_triplet_pt2(b, c, j, k) = wm_interm_86_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_87_triplet_pt2(b, c, j, k) = wm_interm_87_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_88_triplet_pt2(b, c, j, k) = wm_interm_88_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_89_triplet_pt2(b, c, j, k) = wm_interm_89_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_90_triplet_pt2(c, k, j, l) = wm_interm_90_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_91_triplet_pt2(c, j, k, l) = wm_interm_91_triplet_pt2(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_92_triplet_pt2(c, k) = wm_interm_92_triplet_pt2(c, k) + sum 
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
wm_interm_93_triplet_pt2(b, c, j, k) = wm_interm_93_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_94_triplet_pt2(b, c) = wm_interm_94_triplet_pt2(b, c) + sum 
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
wm_interm_95_triplet_pt2(b, c) = wm_interm_95_triplet_pt2(b, c) + sum 
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
wm_interm_96_triplet_pt2(c, j, k, i, l, m) = wm_interm_96_triplet_pt2(c, j, k, i, l, m) + sum 
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
wm_interm_97_triplet_pt2(c, j, k, i, l, m) = wm_interm_97_triplet_pt2(c, j, k, i, l, m) + sum 
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
wm_interm_98_triplet_pt2(b, c) = wm_interm_98_triplet_pt2(b, c) + sum 
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
wm_interm_99_triplet_pt2(c, i, k, j, l, m) = wm_interm_99_triplet_pt2(c, i, k, j, l, m) + sum 
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
wm_interm_100_triplet_pt2(c, i, j, k, l, m) = wm_interm_100_triplet_pt2(c, i, j, k, l, m) + sum 
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
wm_interm_101_triplet_pt2(c, i, j, k, l, m) = wm_interm_101_triplet_pt2(c, i, j, k, l, m) + sum 
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
wm_interm_102_triplet_pt2(c, i, k, j, l, m) = wm_interm_102_triplet_pt2(c, i, k, j, l, m) + sum 
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
wm_interm_103_triplet_pt2(c, j, k, l) = wm_interm_103_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_104_triplet_pt2(j, k) = wm_interm_104_triplet_pt2(j, k) + sum 
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
wm_interm_105_triplet_pt2(j, k) = wm_interm_105_triplet_pt2(j, k) + sum 
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
wm_interm_106_triplet_pt2(j, k) = wm_interm_106_triplet_pt2(j, k) + sum 
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
wm_interm_107_triplet_pt2(i, j, k, l) = wm_interm_107_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_108_triplet_pt2(c, j, k, l) = wm_interm_108_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_109_triplet_pt2(c, i, k, j, l, m) = wm_interm_109_triplet_pt2(c, i, k, j, l, m) + sum 
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
wm_interm_110_triplet_pt2(c, k, j, l) = wm_interm_110_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_111_triplet_pt2(c, k, j, l) = wm_interm_111_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_112_triplet_pt2(c, j, k, i, l, m) = wm_interm_112_triplet_pt2(c, j, k, i, l, m) + sum 
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
wm_interm_113_triplet_pt2(c, j, k, l) = wm_interm_113_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_114_triplet_pt2(c, j, k, l) = wm_interm_114_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_115_triplet_pt2(c, i, j, k, l, m) = wm_interm_115_triplet_pt2(c, i, j, k, l, m) + sum 
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
wm_interm_116_triplet_pt2(c, j, k, l) = wm_interm_116_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_117_triplet_pt2(c, j, k, l) = wm_interm_117_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_118_triplet_pt2(c, k) = wm_interm_118_triplet_pt2(c, k) + sum 
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
wm_interm_119_triplet_pt2(c, k) = wm_interm_119_triplet_pt2(c, k) + sum 
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
wm_interm_120_triplet_pt2(c, k, j, l) = wm_interm_120_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_121_triplet_pt2(c, k, j, l) = wm_interm_121_triplet_pt2(c, k, j, l) + sum 
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
wm_interm_122_triplet_pt2(c, j, k, l) = wm_interm_122_triplet_pt2(c, j, k, l) + sum 
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
wm_interm_123_triplet_pt2(c, j, k, l) = wm_interm_123_triplet_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_124_triplet_pt2(c, k) = wm_interm_124_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_125_triplet_pt2(c, k) = wm_interm_125_triplet_pt2(c, k) + sum 
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
wm_interm_126_triplet_pt2(b, c, j, k) = wm_interm_126_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_127_triplet_pt2(b, c, j, k) = wm_interm_127_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_128_triplet_pt2(b, c, j, k) = wm_interm_128_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_129_triplet_pt2(b, c, j, k) = wm_interm_129_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_130_triplet_pt2(b, c) = wm_interm_130_triplet_pt2(b, c) + sum 
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
wm_interm_131_triplet_pt2(b, c) = wm_interm_131_triplet_pt2(b, c) + sum 
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
wm_interm_132_triplet_pt2(c, j, k, i, l, m) = wm_interm_132_triplet_pt2(c, j, k, i, l, m) + sum 
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
wm_interm_133_triplet_pt2(c, i, k, j, l, m) = wm_interm_133_triplet_pt2(c, i, k, j, l, m) + sum 
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
wm_interm_134_triplet_pt2(c, i, j, k, l, m) = wm_interm_134_triplet_pt2(c, i, j, k, l, m) + sum 
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
wm_interm_135_triplet_pt2(j, k) = wm_interm_135_triplet_pt2(j, k) + sum 
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
wm_interm_136_triplet_pt2(j, k) = wm_interm_136_triplet_pt2(j, k) + sum 
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
wm_interm_137_triplet_pt2(j, k) = wm_interm_137_triplet_pt2(j, k) + sum 
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
wm_interm_138_triplet_pt2(i, j, k, l) = wm_interm_138_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_139_triplet_pt2(c, k) = wm_interm_139_triplet_pt2(c, k) + sum 
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
wm_interm_140_triplet_pt2(c, k) = wm_interm_140_triplet_pt2(c, k) + sum 
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
wm_interm_141_triplet_pt2(c, k) = wm_interm_141_triplet_pt2(c, k) + sum 
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
wm_interm_142_triplet_pt2(c, k) = wm_interm_142_triplet_pt2(c, k) + sum 
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
wm_interm_143_triplet_pt2(b, c, j, k) = wm_interm_143_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_144_triplet_pt2(b, c, k, j) = wm_interm_144_triplet_pt2(b, c, k, j) + sum 
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
wm_interm_145_triplet_pt2(b, c, k, j) = wm_interm_145_triplet_pt2(b, c, k, j) + sum 
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
wm_interm_146_triplet_pt2(b, c, j, k) = wm_interm_146_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_147_triplet_pt2(b, c, k, j) = wm_interm_147_triplet_pt2(b, c, k, j) + sum 
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
wm_interm_148_triplet_pt2(b, c, j, k) = wm_interm_148_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_149_triplet_pt2(b, c, k, j) = wm_interm_149_triplet_pt2(b, c, k, j) + sum 
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
wm_interm_150_triplet_pt2(b, c, j, k) = wm_interm_150_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt2
    
    
    function calc_D_oo_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt0
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

term(0) = term(0) * (-9.0d+0) 
term(1) = term(1) * (-9.0d+0) 
term(2) = term(2) * (10.0d+0) 

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

term(3) = term(3) * (10.0d+0) 
term(4) = term(4) * (-12.0d+0) 

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

term(5) = term(5) * (10.0d+0) 


    calc_D_oo_wm_triplet_cc3_pt0 = zero
    do s = 0, 5
    calc_D_oo_wm_triplet_cc3_pt0 = calc_D_oo_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt0
    
    function calc_D_ov_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt0
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
    
    calc_D_ov_wm_triplet_cc3_pt0 = zero
    do s = 0, 0
    calc_D_ov_wm_triplet_cc3_pt0 = calc_D_ov_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt0
    
    function calc_D_vo_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt0
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
    real(F64), dimension(0:21) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(1) = term(1) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(2) = term(2) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(3) = term(3) + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(4) = term(4) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(5) = term(5) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(6) = term(6) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,j,b,i)
term(7) = term(7) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,i,b,j)
term(8) = term(8) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,i,a,j)
term(9) = term(9) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,j,a,i)
term(10) = term(10) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,j,b,i)
term(11) = term(11) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(0) = term(0) * (3.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (3.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (12.0d+0) 
term(5) = term(5) * (-12.0d+0) 
term(6) = term(6) * (3.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (3.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (12.0d+0) 
term(11) = term(11) * (-12.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(12) = term(12) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 

term(12) = term(12) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(13) = term(13) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,p,q)
term(14) = term(14) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,p,q)
term(15) = term(15) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,j,b,i)
term(16) = term(16) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, b,j,a,i)
term(17) = term(17) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 

term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(18) = term(18) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,p,q)
term(19) = term(19) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,p,q)
term(20) = term(20) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,i,b,j)
term(21) = term(21) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (8.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (8.0d+0) 


    calc_D_vo_wm_triplet_cc3_pt0 = zero
    do s = 0, 21
    calc_D_vo_wm_triplet_cc3_pt0 = calc_D_vo_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt0
    
    function calc_D_vv_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt0
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
term(0) = term(0) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,k,b,i,q,j)
term(1) = term(1) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,i,b,k,q,j)
term(2) = term(2) + r3(vrdav_Rl, a,k,b,j,p,i) * r3(vrdav_Rr, a,i,b,k,q,j)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (9.0d+0) 
term(1) = term(1) * (-10.0d+0) 
term(2) = term(2) * (9.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(3) = term(3) + r3(vrdav_Rl, a,k,b,j,p,i) * r3(vrdav_Rr, a,k,b,i,q,j)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (-10.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(4) = term(4) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,k,b,j,q,i)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (-10.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,j,b,k,q,i)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (12.0d+0) 


    calc_D_vv_wm_triplet_cc3_pt0 = zero
    do s = 0, 5
    calc_D_vv_wm_triplet_cc3_pt0 = calc_D_vv_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt0
    
    function calc_D_oo_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt1
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
    real(F64), dimension(0:39) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,b,i,q) * wm_interm_4_triplet_pt1(a,b,p,i)
term(1) = term(1) + s2(a,b,q,i) * wm_interm_5_triplet_pt1(a,b,i,p)
term(2) = term(2) + s2(a,b,q,i) * wm_interm_4_triplet_pt1(a,b,i,p)
term(3) = term(3) + s2(a,b,q,i) * wm_interm_4_triplet_pt1(a,b,p,i)
term(4) = term(4) + s2(a,b,q,i) * wm_interm_5_triplet_pt1(a,b,p,i)
term(5) = term(5) + s2(a,b,i,q) * wm_interm_5_triplet_pt1(a,b,p,i)
term(6) = term(6) + t2(a,b,q,i) * wm_interm_10_triplet_pt1(b,a,i,p)
term(7) = term(7) + t2(a,b,q,i) * wm_interm_11_triplet_pt1(b,a,p,i)
term(8) = term(8) + t2(a,b,q,i) * wm_interm_12_triplet_pt1(b,a,i,p)
term(9) = term(9) + t2(a,b,q,i) * wm_interm_11_triplet_pt1(b,a,i,p)
term(10) = term(10) + t2(a,b,q,i) * wm_interm_12_triplet_pt1(b,a,p,i)
term(11) = term(11) + t2(a,b,q,i) * wm_interm_13_triplet_pt1(b,a,p,i)
term(12) = term(12) + t2(a,b,q,i) * wm_interm_13_triplet_pt1(b,a,i,p)
term(13) = term(13) + t2(a,b,q,i) * wm_interm_11_triplet_pt1(a,b,i,p)
term(14) = term(14) + t2(a,b,q,i) * wm_interm_13_triplet_pt1(a,b,i,p)
term(15) = term(15) + t2(a,b,q,i) * wm_interm_12_triplet_pt1(a,b,i,p)
term(16) = term(16) + t2(a,b,q,i) * wm_interm_14_triplet_pt1(b,a,i,p)
term(17) = term(17) + t2(a,b,q,i) * wm_interm_10_triplet_pt1(a,b,p,i)
term(18) = term(18) + t2(a,b,q,i) * wm_interm_11_triplet_pt1(a,b,p,i)
term(19) = term(19) + t2(a,b,q,i) * wm_interm_12_triplet_pt1(a,b,p,i)
term(20) = term(20) + t2(a,b,q,i) * wm_interm_13_triplet_pt1(a,b,p,i)
term(21) = term(21) + t2(a,b,q,i) * wm_interm_14_triplet_pt1(a,b,p,i)
end do 
end do 
end do 

term(0) = term(0) * (6.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (6.0d+0) 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (4.0d+0) 

do a = nocc + 1, nactive 
term(22) = term(22) + r1(vrdav_Rl, a,q) * wm_interm_0_triplet_pt1(a,p)
term(23) = term(23) + r1(vrdav_Rl, a,q) * wm_interm_1_triplet_pt1(a,p)
term(24) = term(24) + r1(vrdav_Rl, a,q) * wm_interm_2_triplet_pt1(a,p)
term(25) = term(25) + r1(vrdav_Rl, a,q) * wm_interm_3_triplet_pt1(a,p)
term(26) = term(26) + r1(vrdav_Rr, a,p) * wm_interm_6_triplet_pt1(a,q)
term(27) = term(27) + r1(vrdav_Rr, a,p) * wm_interm_7_triplet_pt1(a,q)
term(28) = term(28) + r1(vrdav_Rr, a,p) * wm_interm_8_triplet_pt1(a,q)
term(29) = term(29) + r1(vrdav_Rr, a,p) * wm_interm_9_triplet_pt1(a,q)
end do 

term(22) = term(22) * (8.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (-6.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (6.0d+0) 
term(29) = term(29) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(30) = term(30) + s2(a,b,i,q) * wm_interm_4_triplet_pt1(a,b,i,p)
term(31) = term(31) + s2(a,b,i,q) * wm_interm_5_triplet_pt1(a,b,i,p)
end do 
end do 
end do 

term(30) = term(30) * (-8.0d+0) 
term(31) = term(31) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rl, a,i) * wm_interm_0_triplet_pt1(a,i)
term(33) = term(33) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt1(a,i)
term(34) = term(34) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt1(a,i)
term(35) = term(35) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt1(a,i)
term(36) = term(36) + r1(vrdav_Rr, a,i) * wm_interm_6_triplet_pt1(a,i)
term(37) = term(37) + r1(vrdav_Rr, a,i) * wm_interm_7_triplet_pt1(a,i)
term(38) = term(38) + r1(vrdav_Rr, a,i) * wm_interm_8_triplet_pt1(a,i)
term(39) = term(39) + r1(vrdav_Rr, a,i) * wm_interm_9_triplet_pt1(a,i)
end do 
end do 

term(32) = term(32) * (-16.0d+0) 
term(33) = term(33) * (8.0d+0) 
term(34) = term(34) * (12.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (16.0d+0) 
term(37) = term(37) * (-8.0d+0) 
term(38) = term(38) * (-12.0d+0) 
term(39) = term(39) * (4.0d+0) 


    calc_D_oo_wm_triplet_cc3_pt1 = zero
    do s = 0, 39
    calc_D_oo_wm_triplet_cc3_pt1 = calc_D_oo_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt1
    
    function calc_D_ov_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt1
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
    real(F64), dimension(0:55) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_21_triplet_pt1(a,p,j,i)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_22_triplet_pt1(a,p,j,i)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_23_triplet_pt1(a,p,j,i)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_24_triplet_pt1(a,p,j,i)
term(4) = term(4) + s2(a,q,j,i) * wm_interm_25_triplet_pt1(a,p,j,i)
term(5) = term(5) + s2(a,q,j,i) * wm_interm_26_triplet_pt1(a,p,j,i)
term(6) = term(6) + s2(a,q,j,i) * wm_interm_35_triplet_pt1(a,p,j,i)
term(7) = term(7) + s2(a,q,j,i) * wm_interm_36_triplet_pt1(a,p,j,i)
term(8) = term(8) + s2(a,q,j,i) * wm_interm_37_triplet_pt1(a,p,j,i)
term(9) = term(9) + s2(a,q,j,i) * wm_interm_38_triplet_pt1(a,p,j,i)
term(10) = term(10) + t2(a,q,j,i) * wm_interm_45_triplet_pt1(a,i,j,p)
term(11) = term(11) + t2(a,q,j,i) * wm_interm_46_triplet_pt1(a,i,j,p)
term(12) = term(12) + t2(a,q,j,i) * wm_interm_47_triplet_pt1(a,i,j,p)
term(13) = term(13) + t2(a,q,j,i) * wm_interm_48_triplet_pt1(a,j,i,p)
term(14) = term(14) + t2(a,q,j,i) * wm_interm_49_triplet_pt1(a,j,i,p)
term(15) = term(15) + t2(a,q,j,i) * wm_interm_52_triplet_pt1(a,j,i,p)
term(16) = term(16) + t2(a,q,j,i) * wm_interm_49_triplet_pt1(a,i,j,p)
term(17) = term(17) + t2(a,q,j,i) * wm_interm_48_triplet_pt1(a,i,j,p)
term(18) = term(18) + t2(a,q,j,i) * wm_interm_45_triplet_pt1(a,j,i,p)
term(19) = term(19) + t2(a,q,j,i) * wm_interm_46_triplet_pt1(a,j,i,p)
term(20) = term(20) + t2(a,q,j,i) * wm_interm_54_triplet_pt1(a,i,j,p)
term(21) = term(21) + t2(a,q,j,i) * wm_interm_47_triplet_pt1(a,j,i,p)
term(22) = term(22) + t2(a,q,j,i) * wm_interm_55_triplet_pt1(a,i,j,p)
term(23) = term(23) + t2(a,q,j,i) * wm_interm_55_triplet_pt1(a,j,i,p)
term(24) = term(24) + t2(a,q,j,i) * wm_interm_60_triplet_pt1(a,j,i,p)
term(25) = term(25) + t2(a,q,j,i) * wm_interm_61_triplet_pt1(a,j,i,p)
term(26) = term(26) + t2(a,q,j,i) * wm_interm_54_triplet_pt1(a,j,i,p)
term(27) = term(27) + t2(a,q,j,i) * wm_interm_62_triplet_pt1(a,j,i,p)
term(28) = term(28) + t2(a,q,j,i) * wm_interm_63_triplet_pt1(a,j,i,p)
term(29) = term(29) + t2(a,q,j,i) * wm_interm_60_triplet_pt1(a,i,j,p)
term(30) = term(30) + t2(a,q,j,i) * wm_interm_62_triplet_pt1(a,i,j,p)
term(31) = term(31) + t2(a,q,j,i) * wm_interm_52_triplet_pt1(a,i,j,p)
term(32) = term(32) + t2(a,q,j,i) * wm_interm_63_triplet_pt1(a,i,j,p)
term(33) = term(33) + t2(a,q,j,i) * wm_interm_61_triplet_pt1(a,i,j,p)
term(34) = term(34) + t2(a,q,j,i) * wm_interm_65_triplet_pt1(a,i,j,p)
term(35) = term(35) + t2(a,q,j,i) * wm_interm_66_triplet_pt1(a,i,j,p)
term(36) = term(36) + t2(a,q,j,i) * wm_interm_67_triplet_pt1(a,j,i,p)
term(37) = term(37) + t2(a,q,j,i) * wm_interm_68_triplet_pt1(a,j,i,p)
term(38) = term(38) + t2(a,q,j,i) * wm_interm_68_triplet_pt1(a,i,j,p)
term(39) = term(39) + t2(a,q,j,i) * wm_interm_67_triplet_pt1(a,i,j,p)
term(40) = term(40) + t2(a,q,j,i) * wm_interm_65_triplet_pt1(a,j,i,p)
term(41) = term(41) + t2(a,q,j,i) * wm_interm_66_triplet_pt1(a,j,i,p)
term(42) = term(42) + t2(a,q,j,i) * wm_interm_73_triplet_pt1(a,j,i,p)
term(43) = term(43) + t2(a,q,j,i) * wm_interm_74_triplet_pt1(a,j,i,p)
term(44) = term(44) + t2(a,q,j,i) * wm_interm_73_triplet_pt1(a,i,j,p)
term(45) = term(45) + t2(a,q,j,i) * wm_interm_74_triplet_pt1(a,i,j,p)
end do 
end do 
end do 

term(0) = term(0) * (3.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (3.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (3.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (12.0d+0) 
term(7) = term(7) * (-12.0d+0) 
term(8) = term(8) * (6.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (1.5d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * (1.5d+0) 
term(13) = term(13) * (1.5d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (1.5d+0) 
term(16) = term(16) * (1.5d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (1.5d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (1.5d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (1.5d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (6.0d+0) 
term(35) = term(35) * (-6.0d+0) 
term(36) = term(36) * (6.0d+0) 
term(37) = term(37) * (-6.0d+0) 
term(38) = term(38) * (6.0d+0) 
term(39) = term(39) * (-6.0d+0) 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (8.0d+0) 
term(42) = term(42) * (6.0d+0) 
term(43) = term(43) * (-6.0d+0) 
term(44) = term(44) * (-8.0d+0) 
term(45) = term(45) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + s2(a,q,j,i) * wm_interm_23_triplet_pt1(a,p,i,j)
term(47) = term(47) + s2(a,q,j,i) * wm_interm_21_triplet_pt1(a,p,i,j)
term(48) = term(48) + s2(a,q,j,i) * wm_interm_22_triplet_pt1(a,p,i,j)
term(49) = term(49) + s2(a,q,j,i) * wm_interm_24_triplet_pt1(a,p,i,j)
term(50) = term(50) + s2(a,q,j,i) * wm_interm_25_triplet_pt1(a,p,i,j)
term(51) = term(51) + s2(a,q,j,i) * wm_interm_26_triplet_pt1(a,p,i,j)
term(52) = term(52) + s2(a,q,j,i) * wm_interm_37_triplet_pt1(a,p,i,j)
term(53) = term(53) + s2(a,q,j,i) * wm_interm_35_triplet_pt1(a,p,i,j)
term(54) = term(54) + s2(a,q,j,i) * wm_interm_36_triplet_pt1(a,p,i,j)
term(55) = term(55) + s2(a,q,j,i) * wm_interm_38_triplet_pt1(a,p,i,j)
end do 
end do 
end do 

term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (3.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-16.0d+0) 
term(54) = term(54) * (16.0d+0) 
term(55) = term(55) * (6.0d+0) 


    calc_D_ov_wm_triplet_cc3_pt1 = zero
    do s = 0, 55
    calc_D_ov_wm_triplet_cc3_pt1 = calc_D_ov_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt1
    
    function calc_D_vo_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt1
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
    real(F64), dimension(0:143) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_27_triplet_pt1(a,i,j,q)
term(1) = term(1) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_27_triplet_pt1(a,j,i,q)
term(2) = term(2) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_28_triplet_pt1(a,i,j,q)
term(3) = term(3) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_30_triplet_pt1(a,i,j,q)
term(4) = term(4) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_29_triplet_pt1(a,i,j,q)
term(5) = term(5) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_30_triplet_pt1(a,j,i,q)
term(6) = term(6) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_39_triplet_pt1(a,j,i,q)
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_39_triplet_pt1(a,i,j,q)
term(8) = term(8) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_40_triplet_pt1(a,i,j,q)
term(9) = term(9) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_40_triplet_pt1(a,j,i,q)
term(10) = term(10) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_41_triplet_pt1(a,j,i,q)
term(11) = term(11) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_41_triplet_pt1(a,i,j,q)
term(12) = term(12) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_42_triplet_pt1(a,i,j,q)
term(13) = term(13) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_43_triplet_pt1(a,i,j,q)
term(14) = term(14) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_44_triplet_pt1(a,j,i,q)
term(15) = term(15) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_44_triplet_pt1(a,i,j,q)
term(16) = term(16) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_43_triplet_pt1(a,j,i,q)
term(17) = term(17) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_42_triplet_pt1(a,j,i,q)
end do 
end do 
end do 

term(0) = term(0) * (3.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (3.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (1.5d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (1.5d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (1.5d+0) 
term(14) = term(14) * (1.5d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(18) = term(18) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_0_triplet_pt1(a,i)
term(19) = term(19) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_1_triplet_pt1(a,i)
term(20) = term(20) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_2_triplet_pt1(a,i)
term(21) = term(21) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_3_triplet_pt1(a,i)
term(22) = term(22) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_6_triplet_pt1(a,i)
term(23) = term(23) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_7_triplet_pt1(a,i)
term(24) = term(24) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_8_triplet_pt1(a,i)
term(25) = term(25) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_9_triplet_pt1(a,i)
end do 
end do 

term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (-3.0d+0) 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (3.0d+0) 
term(25) = term(25) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(26) = term(26) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_27_triplet_pt1(a,j,i,q)
term(27) = term(27) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_27_triplet_pt1(a,i,j,q)
term(28) = term(28) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_29_triplet_pt1(a,i,j,q)
term(29) = term(29) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_28_triplet_pt1(a,i,j,q)
term(30) = term(30) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_30_triplet_pt1(a,j,i,q)
term(31) = term(31) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_30_triplet_pt1(a,i,j,q)
term(32) = term(32) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_27_triplet_pt1(a,j,i,q)
term(33) = term(33) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_27_triplet_pt1(a,i,j,q)
term(34) = term(34) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_29_triplet_pt1(a,i,j,q)
term(35) = term(35) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_28_triplet_pt1(a,i,j,q)
term(36) = term(36) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_30_triplet_pt1(a,j,i,q)
term(37) = term(37) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_30_triplet_pt1(a,i,j,q)
term(38) = term(38) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_40_triplet_pt1(a,j,i,q)
term(39) = term(39) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_40_triplet_pt1(a,i,j,q)
term(40) = term(40) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_39_triplet_pt1(a,i,j,q)
term(41) = term(41) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_39_triplet_pt1(a,j,i,q)
term(42) = term(42) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_42_triplet_pt1(a,j,i,q)
term(43) = term(43) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_42_triplet_pt1(a,i,j,q)
term(44) = term(44) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_41_triplet_pt1(a,i,j,q)
term(45) = term(45) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_44_triplet_pt1(a,i,j,q)
term(46) = term(46) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_43_triplet_pt1(a,j,i,q)
term(47) = term(47) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_43_triplet_pt1(a,i,j,q)
term(48) = term(48) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_44_triplet_pt1(a,j,i,q)
term(49) = term(49) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_41_triplet_pt1(a,j,i,q)
term(50) = term(50) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_39_triplet_pt1(a,i,j,q)
term(51) = term(51) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_39_triplet_pt1(a,j,i,q)
term(52) = term(52) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_40_triplet_pt1(a,j,i,q)
term(53) = term(53) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_40_triplet_pt1(a,i,j,q)
term(54) = term(54) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_41_triplet_pt1(a,i,j,q)
term(55) = term(55) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_41_triplet_pt1(a,j,i,q)
term(56) = term(56) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_42_triplet_pt1(a,j,i,q)
term(57) = term(57) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_43_triplet_pt1(a,j,i,q)
term(58) = term(58) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_44_triplet_pt1(a,i,j,q)
term(59) = term(59) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_44_triplet_pt1(a,j,i,q)
term(60) = term(60) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_43_triplet_pt1(a,i,j,q)
term(61) = term(61) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_42_triplet_pt1(a,i,j,q)
end do 
end do 
end do 

term(26) = term(26) * (3.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (3.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (6.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (6.0d+0) 
term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (8.0d+0) 
term(38) = term(38) * (1.5d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (1.5d+0) 
term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (2.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (1.5d+0) 
term(46) = term(46) * (1.5d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (-2.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (6.0d+0) 
term(51) = term(51) * (-6.0d+0) 
term(52) = term(52) * (6.0d+0) 
term(53) = term(53) * (-6.0d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (8.0d+0) 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * (6.0d+0) 
term(58) = term(58) * (6.0d+0) 
term(59) = term(59) * (-6.0d+0) 
term(60) = term(60) * (-6.0d+0) 
term(61) = term(61) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(62) = term(62) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_27_triplet_pt1(a,i,j,q)
term(63) = term(63) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_27_triplet_pt1(a,j,i,q)
term(64) = term(64) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_28_triplet_pt1(a,i,j,q)
term(65) = term(65) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_30_triplet_pt1(a,i,j,q)
term(66) = term(66) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_29_triplet_pt1(a,i,j,q)
term(67) = term(67) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_30_triplet_pt1(a,j,i,q)
end do 
end do 
end do 

term(62) = term(62) * (6.0d+0) 
term(63) = term(63) * (-8.0d+0) 
term(64) = term(64) * (6.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(68) = term(68) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_0_triplet_pt1(a,i)
term(69) = term(69) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_1_triplet_pt1(a,i)
term(70) = term(70) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_0_triplet_pt1(a,i)
term(71) = term(71) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_1_triplet_pt1(a,i)
term(72) = term(72) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_2_triplet_pt1(a,i)
term(73) = term(73) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_2_triplet_pt1(a,i)
term(74) = term(74) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_3_triplet_pt1(a,i)
term(75) = term(75) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_3_triplet_pt1(a,i)
term(76) = term(76) + s2(a,p,q,i) * wm_interm_15_triplet_pt1(a,i)
term(77) = term(77) + s2(a,p,q,i) * wm_interm_16_triplet_pt1(a,i)
term(78) = term(78) + s2(a,p,q,i) * wm_interm_17_triplet_pt1(a,i)
term(79) = term(79) + s2(a,p,q,i) * wm_interm_18_triplet_pt1(a,i)
term(80) = term(80) + s2(a,p,q,i) * wm_interm_19_triplet_pt1(a,i)
term(81) = term(81) + s2(a,p,q,i) * wm_interm_20_triplet_pt1(a,i)
term(82) = term(82) + s2(a,p,i,q) * wm_interm_15_triplet_pt1(a,i)
term(83) = term(83) + s2(a,p,i,q) * wm_interm_16_triplet_pt1(a,i)
term(84) = term(84) + s2(a,p,i,q) * wm_interm_17_triplet_pt1(a,i)
term(85) = term(85) + s2(a,p,i,q) * wm_interm_18_triplet_pt1(a,i)
term(86) = term(86) + s2(a,p,i,q) * wm_interm_19_triplet_pt1(a,i)
term(87) = term(87) + s2(a,p,i,q) * wm_interm_20_triplet_pt1(a,i)
term(88) = term(88) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_0_triplet_pt1(a,i)
term(89) = term(89) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_1_triplet_pt1(a,i)
term(90) = term(90) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_0_triplet_pt1(a,i)
term(91) = term(91) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_1_triplet_pt1(a,i)
term(92) = term(92) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_2_triplet_pt1(a,i)
term(93) = term(93) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_2_triplet_pt1(a,i)
term(94) = term(94) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_3_triplet_pt1(a,i)
term(95) = term(95) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_3_triplet_pt1(a,i)
term(96) = term(96) + s2(a,p,q,i) * wm_interm_31_triplet_pt1(a,i)
term(97) = term(97) + s2(a,p,q,i) * wm_interm_32_triplet_pt1(a,i)
term(98) = term(98) + s2(a,p,q,i) * wm_interm_33_triplet_pt1(a,i)
term(99) = term(99) + s2(a,p,q,i) * wm_interm_34_triplet_pt1(a,i)
term(100) = term(100) + s2(a,p,i,q) * wm_interm_31_triplet_pt1(a,i)
term(101) = term(101) + s2(a,p,i,q) * wm_interm_32_triplet_pt1(a,i)
term(102) = term(102) + s2(a,p,i,q) * wm_interm_33_triplet_pt1(a,i)
term(103) = term(103) + s2(a,p,i,q) * wm_interm_34_triplet_pt1(a,i)
term(104) = term(104) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_6_triplet_pt1(a,i)
term(105) = term(105) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_7_triplet_pt1(a,i)
term(106) = term(106) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_6_triplet_pt1(a,i)
term(107) = term(107) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_7_triplet_pt1(a,i)
term(108) = term(108) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_8_triplet_pt1(a,i)
term(109) = term(109) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_8_triplet_pt1(a,i)
term(110) = term(110) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_9_triplet_pt1(a,i)
term(111) = term(111) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_9_triplet_pt1(a,i)
term(112) = term(112) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_6_triplet_pt1(a,i)
term(113) = term(113) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_7_triplet_pt1(a,i)
term(114) = term(114) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_6_triplet_pt1(a,i)
term(115) = term(115) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_7_triplet_pt1(a,i)
term(116) = term(116) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_8_triplet_pt1(a,i)
term(117) = term(117) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_8_triplet_pt1(a,i)
term(118) = term(118) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_9_triplet_pt1(a,i)
term(119) = term(119) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_9_triplet_pt1(a,i)
term(120) = term(120) + t2(a,p,i,q) * wm_interm_50_triplet_pt1(a,i)
term(121) = term(121) + t2(a,p,i,q) * wm_interm_51_triplet_pt1(a,i)
term(122) = term(122) + t2(a,p,i,q) * wm_interm_53_triplet_pt1(a,i)
term(123) = term(123) + t2(a,p,i,q) * wm_interm_56_triplet_pt1(a,i)
term(124) = term(124) + t2(a,p,i,q) * wm_interm_57_triplet_pt1(a,i)
term(125) = term(125) + t2(a,p,i,q) * wm_interm_58_triplet_pt1(a,i)
term(126) = term(126) + t2(a,p,i,q) * wm_interm_59_triplet_pt1(a,i)
term(127) = term(127) + t2(a,p,i,q) * wm_interm_64_triplet_pt1(a,i)
term(128) = term(128) + t2(a,p,q,i) * wm_interm_50_triplet_pt1(a,i)
term(129) = term(129) + t2(a,p,q,i) * wm_interm_51_triplet_pt1(a,i)
term(130) = term(130) + t2(a,p,q,i) * wm_interm_53_triplet_pt1(a,i)
term(131) = term(131) + t2(a,p,q,i) * wm_interm_56_triplet_pt1(a,i)
term(132) = term(132) + t2(a,p,q,i) * wm_interm_57_triplet_pt1(a,i)
term(133) = term(133) + t2(a,p,q,i) * wm_interm_58_triplet_pt1(a,i)
term(134) = term(134) + t2(a,p,q,i) * wm_interm_59_triplet_pt1(a,i)
term(135) = term(135) + t2(a,p,q,i) * wm_interm_64_triplet_pt1(a,i)
term(136) = term(136) + t2(a,p,i,q) * wm_interm_69_triplet_pt1(a,i)
term(137) = term(137) + t2(a,p,i,q) * wm_interm_70_triplet_pt1(a,i)
term(138) = term(138) + t2(a,p,i,q) * wm_interm_71_triplet_pt1(a,i)
term(139) = term(139) + t2(a,p,i,q) * wm_interm_72_triplet_pt1(a,i)
term(140) = term(140) + t2(a,p,q,i) * wm_interm_69_triplet_pt1(a,i)
term(141) = term(141) + t2(a,p,q,i) * wm_interm_70_triplet_pt1(a,i)
term(142) = term(142) + t2(a,p,q,i) * wm_interm_71_triplet_pt1(a,i)
term(143) = term(143) + t2(a,p,q,i) * wm_interm_72_triplet_pt1(a,i)
end do 
end do 

term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * (4.0d+0) 
term(72) = term(72) * (-3.0d+0) 
term(73) = term(73) * (6.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * (3.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (3.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (-6.0d+0) 
term(83) = term(83) * (8.0d+0) 
term(84) = term(84) * (-6.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (8.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (16.0d+0) 
term(89) = term(89) * (-8.0d+0) 
term(90) = term(90) * (-16.0d+0) 
term(91) = term(91) * (8.0d+0) 
term(92) = term(92) * (-12.0d+0) 
term(93) = term(93) * (12.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (12.0d+0) 
term(97) = term(97) * (-12.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (-24.0d+0) 
term(101) = term(101) * (24.0d+0) 
term(102) = term(102) * (16.0d+0) 
term(103) = term(103) * (-16.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (8.0d+0) 
term(107) = term(107) * (-4.0d+0) 
term(108) = term(108) * (3.0d+0) 
term(109) = term(109) * (-6.0d+0) 
term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (-16.0d+0) 
term(113) = term(113) * (8.0d+0) 
term(114) = term(114) * (16.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (12.0d+0) 
term(117) = term(117) * (-12.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (-6.0d+0) 
term(121) = term(121) * (8.0d+0) 
term(122) = term(122) * (-6.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (3.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (3.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * (2.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * (2.0d+0) 
term(136) = term(136) * (-24.0d+0) 
term(137) = term(137) * (24.0d+0) 
term(138) = term(138) * (16.0d+0) 
term(139) = term(139) * (-16.0d+0) 
term(140) = term(140) * (12.0d+0) 
term(141) = term(141) * (-12.0d+0) 
term(142) = term(142) * (-8.0d+0) 
term(143) = term(143) * (8.0d+0) 


    calc_D_vo_wm_triplet_cc3_pt1 = zero
    do s = 0, 143
    calc_D_vo_wm_triplet_cc3_pt1 = calc_D_vo_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt1
    
    function calc_D_vv_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt1
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
    real(F64), dimension(0:31) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_4_triplet_pt1(a,p,i,j)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_4_triplet_pt1(p,a,i,j)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_5_triplet_pt1(p,a,i,j)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_5_triplet_pt1(a,p,i,j)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_11_triplet_pt1(a,p,i,j)
term(5) = term(5) + t2(a,q,j,i) * wm_interm_12_triplet_pt1(a,p,i,j)
term(6) = term(6) + t2(a,q,j,i) * wm_interm_13_triplet_pt1(a,p,i,j)
term(7) = term(7) + t2(a,q,j,i) * wm_interm_10_triplet_pt1(p,a,i,j)
term(8) = term(8) + t2(a,q,j,i) * wm_interm_11_triplet_pt1(p,a,i,j)
term(9) = term(9) + t2(a,q,j,i) * wm_interm_12_triplet_pt1(p,a,i,j)
term(10) = term(10) + t2(a,q,j,i) * wm_interm_13_triplet_pt1(p,a,i,j)
term(11) = term(11) + t2(a,q,j,i) * wm_interm_14_triplet_pt1(p,a,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (4.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (-1.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + s2(a,q,j,i) * wm_interm_5_triplet_pt1(p,a,j,i)
term(13) = term(13) + s2(a,q,j,i) * wm_interm_4_triplet_pt1(p,a,j,i)
term(14) = term(14) + s2(a,q,j,i) * wm_interm_4_triplet_pt1(a,p,j,i)
term(15) = term(15) + s2(a,q,j,i) * wm_interm_5_triplet_pt1(a,p,j,i)
term(16) = term(16) + t2(a,q,j,i) * wm_interm_10_triplet_pt1(a,p,j,i)
term(17) = term(17) + t2(a,q,j,i) * wm_interm_12_triplet_pt1(a,p,j,i)
term(18) = term(18) + t2(a,q,j,i) * wm_interm_11_triplet_pt1(a,p,j,i)
term(19) = term(19) + t2(a,q,j,i) * wm_interm_13_triplet_pt1(a,p,j,i)
term(20) = term(20) + t2(a,q,j,i) * wm_interm_11_triplet_pt1(p,a,j,i)
term(21) = term(21) + t2(a,q,j,i) * wm_interm_13_triplet_pt1(p,a,j,i)
term(22) = term(22) + t2(a,q,j,i) * wm_interm_12_triplet_pt1(p,a,j,i)
term(23) = term(23) + t2(a,q,j,i) * wm_interm_14_triplet_pt1(a,p,j,i)
end do 
end do 
end do 

term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-6.0d+0) 
term(14) = term(14) * (8.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (-1.0d+0) 

do i = 1, nocc 
term(24) = term(24) + r1(vrdav_Rl, q,i) * wm_interm_0_triplet_pt1(p,i)
term(25) = term(25) + r1(vrdav_Rl, q,i) * wm_interm_1_triplet_pt1(p,i)
term(26) = term(26) + r1(vrdav_Rl, q,i) * wm_interm_2_triplet_pt1(p,i)
term(27) = term(27) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt1(p,i)
term(28) = term(28) + r1(vrdav_Rr, p,i) * wm_interm_6_triplet_pt1(q,i)
term(29) = term(29) + r1(vrdav_Rr, p,i) * wm_interm_7_triplet_pt1(q,i)
term(30) = term(30) + r1(vrdav_Rr, p,i) * wm_interm_8_triplet_pt1(q,i)
term(31) = term(31) + r1(vrdav_Rr, p,i) * wm_interm_9_triplet_pt1(q,i)
end do 

term(24) = term(24) * (-8.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (6.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (8.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-6.0d+0) 
term(31) = term(31) * (2.0d+0) 


    calc_D_vv_wm_triplet_cc3_pt1 = zero
    do s = 0, 31
    calc_D_vv_wm_triplet_cc3_pt1 = calc_D_vv_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt1
    
    
    function calc_D_oo_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt2
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
    real(F64), dimension(0:119) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_143_triplet_pt2(a,b,i,p)
term(1) = term(1) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_143_triplet_pt2(a,b,p,i)
term(2) = term(2) + r2p(vrdav_Rl, a,i,b,q) * wm_interm_143_triplet_pt2(a,b,p,i)
term(3) = term(3) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_144_triplet_pt2(a,b,i,p)
term(4) = term(4) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_145_triplet_pt2(a,b,p,i)
term(5) = term(5) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_145_triplet_pt2(a,b,i,p)
term(6) = term(6) + r2p(vrdav_Rl, a,i,b,q) * wm_interm_144_triplet_pt2(a,b,p,i)
term(7) = term(7) + r2p(vrdav_Rl, a,i,b,q) * wm_interm_145_triplet_pt2(a,b,p,i)
term(8) = term(8) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_144_triplet_pt2(a,b,p,i)
term(9) = term(9) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_143_triplet_pt2(a,b,i,p)
term(10) = term(10) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_143_triplet_pt2(a,b,p,i)
term(11) = term(11) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_143_triplet_pt2(a,b,p,i)
term(12) = term(12) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_144_triplet_pt2(a,b,i,p)
term(13) = term(13) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_145_triplet_pt2(a,b,p,i)
term(14) = term(14) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_145_triplet_pt2(a,b,i,p)
term(15) = term(15) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_144_triplet_pt2(a,b,p,i)
term(16) = term(16) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_145_triplet_pt2(a,b,p,i)
term(17) = term(17) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_144_triplet_pt2(a,b,p,i)
term(18) = term(18) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_146_triplet_pt2(a,b,q,i)
term(19) = term(19) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_146_triplet_pt2(b,a,q,i)
term(20) = term(20) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_146_triplet_pt2(b,a,q,i)
term(21) = term(21) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_146_triplet_pt2(a,b,i,q)
term(22) = term(22) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_146_triplet_pt2(b,a,i,q)
term(23) = term(23) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_147_triplet_pt2(a,b,q,i)
term(24) = term(24) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_147_triplet_pt2(b,a,q,i)
term(25) = term(25) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_147_triplet_pt2(b,a,q,i)
term(26) = term(26) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_147_triplet_pt2(a,b,i,q)
term(27) = term(27) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_147_triplet_pt2(b,a,i,q)
term(28) = term(28) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_148_triplet_pt2(a,b,q,i)
term(29) = term(29) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_148_triplet_pt2(a,b,q,i)
term(30) = term(30) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_149_triplet_pt2(a,b,q,i)
term(31) = term(31) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_148_triplet_pt2(a,b,i,q)
term(32) = term(32) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_149_triplet_pt2(b,a,q,i)
term(33) = term(33) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_149_triplet_pt2(b,a,q,i)
term(34) = term(34) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_148_triplet_pt2(b,a,q,i)
term(35) = term(35) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_149_triplet_pt2(b,a,i,q)
term(36) = term(36) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_150_triplet_pt2(a,b,i,q)
term(37) = term(37) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_146_triplet_pt2(a,b,q,i)
term(38) = term(38) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_150_triplet_pt2(b,a,i,q)
term(39) = term(39) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_150_triplet_pt2(a,b,q,i)
term(40) = term(40) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_150_triplet_pt2(a,b,q,i)
term(41) = term(41) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_150_triplet_pt2(b,a,q,i)
term(42) = term(42) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_146_triplet_pt2(a,b,q,i)
term(43) = term(43) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_146_triplet_pt2(a,b,q,i)
term(44) = term(44) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_146_triplet_pt2(a,b,i,q)
term(45) = term(45) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_147_triplet_pt2(a,b,q,i)
term(46) = term(46) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_147_triplet_pt2(a,b,q,i)
term(47) = term(47) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_147_triplet_pt2(a,b,i,q)
term(48) = term(48) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_148_triplet_pt2(a,b,q,i)
term(49) = term(49) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_148_triplet_pt2(a,b,q,i)
term(50) = term(50) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_149_triplet_pt2(a,b,q,i)
term(51) = term(51) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_148_triplet_pt2(a,b,i,q)
term(52) = term(52) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_149_triplet_pt2(a,b,q,i)
term(53) = term(53) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_149_triplet_pt2(a,b,i,q)
term(54) = term(54) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_150_triplet_pt2(a,b,i,q)
term(55) = term(55) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_150_triplet_pt2(a,b,q,i)
term(56) = term(56) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_150_triplet_pt2(a,b,q,i)
end do 
end do 
end do 

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-3.0d+0) 
term(4) = term(4) * (-3.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-3.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (8.0d+0) 
term(10) = term(10) * (-8.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (-6.0d+0) 
term(13) = term(13) * (-6.0d+0) 
term(14) = term(14) * (8.0d+0) 
term(15) = term(15) * (-6.0d+0) 
term(16) = term(16) * (8.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (-1.0d+0) 
term(35) = term(35) * (0.5d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (0.5d+0) 
term(39) = term(39) * (0.5d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (0.5d+0) 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * (8.0d+0) 
term(44) = term(44) * (-8.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(57) = term(57) + s1(a,i) * wm_interm_43_triplet_pt2(a,i)
term(58) = term(58) + s1(a,i) * wm_interm_44_triplet_pt2(a,i)
term(59) = term(59) + s1(a,i) * wm_interm_39_triplet_pt2(a,i)
term(60) = term(60) + s1(a,i) * wm_interm_41_triplet_pt2(a,i)
term(61) = term(61) + s1(a,i) * wm_interm_40_triplet_pt2(a,i)
term(62) = term(62) + s1(a,i) * wm_interm_42_triplet_pt2(a,i)
term(63) = term(63) + s1(a,i) * wm_interm_65_triplet_pt2(a,i)
term(64) = term(64) + s1(a,i) * wm_interm_66_triplet_pt2(a,i)
term(65) = term(65) + s1(a,i) * wm_interm_63_triplet_pt2(a,i)
term(66) = term(66) + s1(a,i) * wm_interm_64_triplet_pt2(a,i)
term(67) = term(67) + t1(a,i) * wm_interm_79_triplet_pt2(a,i)
term(68) = term(68) + t1(a,i) * wm_interm_80_triplet_pt2(a,i)
term(69) = term(69) + t1(a,i) * wm_interm_83_triplet_pt2(a,i)
term(70) = term(70) + t1(a,i) * wm_interm_70_triplet_pt2(a,i)
term(71) = term(71) + t1(a,i) * wm_interm_74_triplet_pt2(a,i)
term(72) = term(72) + t1(a,i) * wm_interm_81_triplet_pt2(a,i)
term(73) = term(73) + t1(a,i) * wm_interm_71_triplet_pt2(a,i)
term(74) = term(74) + t1(a,i) * wm_interm_92_triplet_pt2(a,i)
term(75) = term(75) + t1(a,i) * wm_interm_124_triplet_pt2(a,i)
term(76) = term(76) + t1(a,i) * wm_interm_125_triplet_pt2(a,i)
term(77) = term(77) + t1(a,i) * wm_interm_118_triplet_pt2(a,i)
term(78) = term(78) + t1(a,i) * wm_interm_119_triplet_pt2(a,i)
end do 
end do 

term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (8.0d+0) 
term(59) = term(59) * (6.0d+0) 
term(60) = term(60) * (6.0d+0) 
term(61) = term(61) * (-8.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (-16.0d+0) 
term(64) = term(64) * (16.0d+0) 
term(65) = term(65) * (24.0d+0) 
term(66) = term(66) * (-24.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (-6.0d+0) 
term(71) = term(71) * (-6.0d+0) 
term(72) = term(72) * (8.0d+0) 
term(73) = term(73) * (4.0d+0) 
term(74) = term(74) * (-4.0d+0) 
term(75) = term(75) * (16.0d+0) 
term(76) = term(76) * (-16.0d+0) 
term(77) = term(77) * (-24.0d+0) 
term(78) = term(78) * (24.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(79) = term(79) + r2p(vrdav_Rl, a,i,b,q) * wm_interm_143_triplet_pt2(a,b,i,p)
term(80) = term(80) + r2p(vrdav_Rl, a,i,b,q) * wm_interm_145_triplet_pt2(a,b,i,p)
term(81) = term(81) + r2p(vrdav_Rl, a,i,b,q) * wm_interm_144_triplet_pt2(a,b,i,p)
term(82) = term(82) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_143_triplet_pt2(a,b,i,p)
term(83) = term(83) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_145_triplet_pt2(a,b,i,p)
term(84) = term(84) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_144_triplet_pt2(a,b,i,p)
term(85) = term(85) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_146_triplet_pt2(b,a,i,q)
term(86) = term(86) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_147_triplet_pt2(b,a,i,q)
term(87) = term(87) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_148_triplet_pt2(a,b,i,q)
term(88) = term(88) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_149_triplet_pt2(a,b,i,q)
term(89) = term(89) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_149_triplet_pt2(b,a,i,q)
term(90) = term(90) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_148_triplet_pt2(b,a,i,q)
term(91) = term(91) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_150_triplet_pt2(a,b,i,q)
term(92) = term(92) + r2p(vrdav_Rr, a,i,b,p) * wm_interm_146_triplet_pt2(a,b,i,q)
term(93) = term(93) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_146_triplet_pt2(a,b,i,q)
term(94) = term(94) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_147_triplet_pt2(a,b,i,q)
term(95) = term(95) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_148_triplet_pt2(a,b,i,q)
term(96) = term(96) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_149_triplet_pt2(a,b,i,q)
term(97) = term(97) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_150_triplet_pt2(a,b,i,q)
end do 
end do 
end do 

term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (-3.0d+0) 
term(81) = term(81) * (2.0d+0) 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * (-6.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-2.0d+0) 
term(87) = term(87) * (-2.0d+0) 
term(88) = term(88) * (0.5d+0) 
term(89) = term(89) * (-1.0d+0) 
term(91) = term(91) * (0.5d+0) 
term(92) = term(92) * (2.0d+0) 
term(93) = term(93) * (8.0d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (2.0d+0) 

do a = nocc + 1, nactive 
term(98) = term(98) + s1(a,q) * wm_interm_39_triplet_pt2(a,p)
term(99) = term(99) + s1(a,q) * wm_interm_40_triplet_pt2(a,p)
term(100) = term(100) + s1(a,q) * wm_interm_41_triplet_pt2(a,p)
term(101) = term(101) + s1(a,q) * wm_interm_42_triplet_pt2(a,p)
term(102) = term(102) + s1(a,q) * wm_interm_43_triplet_pt2(a,p)
term(103) = term(103) + s1(a,q) * wm_interm_44_triplet_pt2(a,p)
term(104) = term(104) + s1(a,q) * wm_interm_63_triplet_pt2(a,p)
term(105) = term(105) + s1(a,q) * wm_interm_64_triplet_pt2(a,p)
term(106) = term(106) + s1(a,q) * wm_interm_65_triplet_pt2(a,p)
term(107) = term(107) + s1(a,q) * wm_interm_66_triplet_pt2(a,p)
term(108) = term(108) + t1(a,q) * wm_interm_74_triplet_pt2(a,p)
term(109) = term(109) + t1(a,q) * wm_interm_81_triplet_pt2(a,p)
term(110) = term(110) + t1(a,q) * wm_interm_70_triplet_pt2(a,p)
term(111) = term(111) + t1(a,q) * wm_interm_71_triplet_pt2(a,p)
term(112) = term(112) + t1(a,q) * wm_interm_79_triplet_pt2(a,p)
term(113) = term(113) + t1(a,q) * wm_interm_92_triplet_pt2(a,p)
term(114) = term(114) + t1(a,q) * wm_interm_80_triplet_pt2(a,p)
term(115) = term(115) + t1(a,q) * wm_interm_83_triplet_pt2(a,p)
term(116) = term(116) + t1(a,q) * wm_interm_118_triplet_pt2(a,p)
term(117) = term(117) + t1(a,q) * wm_interm_119_triplet_pt2(a,p)
term(118) = term(118) + t1(a,q) * wm_interm_124_triplet_pt2(a,p)
term(119) = term(119) + t1(a,q) * wm_interm_125_triplet_pt2(a,p)
end do 

term(98) = term(98) * (3.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (3.0d+0) 
term(101) = term(101) * (-2.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * (4.0d+0) 
term(104) = term(104) * (12.0d+0) 
term(105) = term(105) * (-12.0d+0) 
term(106) = term(106) * (-8.0d+0) 
term(107) = term(107) * (8.0d+0) 
term(108) = term(108) * (3.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (3.0d+0) 
term(111) = term(111) * (-2.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (-2.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (12.0d+0) 
term(117) = term(117) * (-12.0d+0) 
term(118) = term(118) * (-8.0d+0) 
term(119) = term(119) * (8.0d+0) 


    calc_D_oo_wm_triplet_cc3_pt2 = zero
    do s = 0, 119
    calc_D_oo_wm_triplet_cc3_pt2 = calc_D_oo_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt2
    
    function calc_D_ov_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, b, l, c 
    real(F64), dimension(0:641) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_45_triplet_pt2(a,q,j,i)
term(1) = term(1) + wm_interm_11_triplet_pt2(a,i,j,p) * wm_interm_45_triplet_pt2(a,q,j,i)
term(2) = term(2) + wm_interm_0_triplet_pt2(a,i,j,p) * wm_interm_45_triplet_pt2(a,q,j,i)
term(3) = term(3) + wm_interm_3_triplet_pt2(a,i,j,p) * wm_interm_45_triplet_pt2(a,q,j,i)
term(4) = term(4) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_48_triplet_pt2(a,q,j,i)
term(5) = term(5) + wm_interm_11_triplet_pt2(a,i,j,p) * wm_interm_48_triplet_pt2(a,q,j,i)
term(6) = term(6) + wm_interm_0_triplet_pt2(a,i,j,p) * wm_interm_48_triplet_pt2(a,q,j,i)
term(7) = term(7) + wm_interm_3_triplet_pt2(a,i,j,p) * wm_interm_48_triplet_pt2(a,q,j,i)
term(8) = term(8) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_47_triplet_pt2(a,q,j,i)
term(9) = term(9) + wm_interm_11_triplet_pt2(a,i,j,p) * wm_interm_47_triplet_pt2(a,q,j,i)
term(10) = term(10) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_46_triplet_pt2(a,q,j,i)
term(11) = term(11) + wm_interm_11_triplet_pt2(a,i,j,p) * wm_interm_46_triplet_pt2(a,q,j,i)
term(12) = term(12) + wm_interm_0_triplet_pt2(a,i,j,p) * wm_interm_47_triplet_pt2(a,q,j,i)
term(13) = term(13) + wm_interm_0_triplet_pt2(a,i,j,p) * wm_interm_46_triplet_pt2(a,q,j,i)
term(14) = term(14) + wm_interm_3_triplet_pt2(a,i,j,p) * wm_interm_47_triplet_pt2(a,q,j,i)
term(15) = term(15) + wm_interm_3_triplet_pt2(a,i,j,p) * wm_interm_46_triplet_pt2(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (3.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (3.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (3.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (3.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-6.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (8.0d+0) 
term(14) = term(14) * (3.0d+0) 
term(15) = term(15) * (-6.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,i,p)
term(17) = term(17) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,p,i)
term(18) = term(18) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(19) = term(19) + wm_interm_11_triplet_pt2(a,i,p,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(20) = term(20) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(21) = term(21) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,i,p)
term(22) = term(22) + wm_interm_3_triplet_pt2(a,i,p,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(23) = term(23) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,p,i)
term(24) = term(24) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_48_triplet_pt2(a,q,j,i)
term(25) = term(25) + wm_interm_11_triplet_pt2(a,i,p,j) * wm_interm_48_triplet_pt2(a,q,j,i)
term(26) = term(26) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,p,i)
term(27) = term(27) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,i,p)
term(28) = term(28) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_48_triplet_pt2(a,q,j,i)
term(29) = term(29) + wm_interm_3_triplet_pt2(a,i,p,j) * wm_interm_48_triplet_pt2(a,q,j,i)
term(30) = term(30) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,p,i)
term(31) = term(31) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,i,p)
term(32) = term(32) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,i,p)
term(33) = term(33) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,p,i)
term(34) = term(34) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_47_triplet_pt2(a,q,j,i)
term(35) = term(35) + wm_interm_11_triplet_pt2(a,i,p,j) * wm_interm_47_triplet_pt2(a,q,j,i)
term(36) = term(36) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,i,p)
term(37) = term(37) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,j,p,i)
term(38) = term(38) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_46_triplet_pt2(a,q,j,i)
term(39) = term(39) + wm_interm_11_triplet_pt2(a,i,p,j) * wm_interm_46_triplet_pt2(a,q,j,i)
term(40) = term(40) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_47_triplet_pt2(a,q,j,i)
term(41) = term(41) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_46_triplet_pt2(a,q,j,i)
term(42) = term(42) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,i,p)
term(43) = term(43) + wm_interm_3_triplet_pt2(a,i,p,j) * wm_interm_47_triplet_pt2(a,q,j,i)
term(44) = term(44) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,p,i)
term(45) = term(45) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,i,p)
term(46) = term(46) + wm_interm_3_triplet_pt2(a,i,p,j) * wm_interm_46_triplet_pt2(a,q,j,i)
term(47) = term(47) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_4_triplet_pt2(a,j,p,i)
term(48) = term(48) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_33_triplet_pt2(a,q,i,j)
term(49) = term(49) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_33_triplet_pt2(a,q,i,j)
term(50) = term(50) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_29_triplet_pt2(a,q,i,j)
term(51) = term(51) + wm_interm_13_triplet_pt2(a,q,i,j) * wm_interm_19_triplet_pt2(a,i,p,j)
term(52) = term(52) + wm_interm_13_triplet_pt2(a,q,i,j) * wm_interm_19_triplet_pt2(a,p,i,j)
term(53) = term(53) + wm_interm_17_triplet_pt2(a,q,i,j) * wm_interm_19_triplet_pt2(a,p,i,j)
term(54) = term(54) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_29_triplet_pt2(a,q,i,j)
term(55) = term(55) + wm_interm_17_triplet_pt2(a,q,i,j) * wm_interm_19_triplet_pt2(a,i,p,j)
term(56) = term(56) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_33_triplet_pt2(a,q,i,j)
term(57) = term(57) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_29_triplet_pt2(a,q,i,j)
term(58) = term(58) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_29_triplet_pt2(a,q,i,j)
term(59) = term(59) + wm_interm_13_triplet_pt2(a,q,i,j) * wm_interm_21_triplet_pt2(a,i,p,j)
term(60) = term(60) + wm_interm_17_triplet_pt2(a,q,i,j) * wm_interm_22_triplet_pt2(a,i,p,j)
term(61) = term(61) + wm_interm_17_triplet_pt2(a,q,i,j) * wm_interm_21_triplet_pt2(a,i,p,j)
term(62) = term(62) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_33_triplet_pt2(a,q,i,j)
term(63) = term(63) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_29_triplet_pt2(a,q,i,j)
term(64) = term(64) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_29_triplet_pt2(a,q,i,j)
term(65) = term(65) + wm_interm_13_triplet_pt2(a,q,i,j) * wm_interm_20_triplet_pt2(a,i,p,j)
term(66) = term(66) + wm_interm_17_triplet_pt2(a,q,i,j) * wm_interm_20_triplet_pt2(a,p,i,j)
term(67) = term(67) + wm_interm_17_triplet_pt2(a,q,i,j) * wm_interm_20_triplet_pt2(a,i,p,j)
term(68) = term(68) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_33_triplet_pt2(a,q,i,j)
term(69) = term(69) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_33_triplet_pt2(a,q,i,j)
term(70) = term(70) + wm_interm_13_triplet_pt2(a,q,i,j) * wm_interm_22_triplet_pt2(a,i,p,j)
term(71) = term(71) + wm_interm_13_triplet_pt2(a,q,i,j) * wm_interm_20_triplet_pt2(a,p,i,j)
term(72) = term(72) + wm_interm_12_triplet_pt2(a,q,i,j) * wm_interm_19_triplet_pt2(a,p,i,j)
term(73) = term(73) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_8_triplet_pt2(a,q,i,j)
term(74) = term(74) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_6_triplet_pt2(a,q,i,j)
term(75) = term(75) + wm_interm_12_triplet_pt2(a,q,i,j) * wm_interm_19_triplet_pt2(a,i,p,j)
term(76) = term(76) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_8_triplet_pt2(a,q,i,j)
term(77) = term(77) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_6_triplet_pt2(a,q,i,j)
term(78) = term(78) + wm_interm_12_triplet_pt2(a,q,i,j) * wm_interm_22_triplet_pt2(a,i,p,j)
term(79) = term(79) + wm_interm_12_triplet_pt2(a,q,i,j) * wm_interm_21_triplet_pt2(a,i,p,j)
term(80) = term(80) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_8_triplet_pt2(a,q,i,j)
term(81) = term(81) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_8_triplet_pt2(a,q,i,j)
term(82) = term(82) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_6_triplet_pt2(a,q,i,j)
term(83) = term(83) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_6_triplet_pt2(a,q,i,j)
term(84) = term(84) + wm_interm_12_triplet_pt2(a,q,i,j) * wm_interm_20_triplet_pt2(a,p,i,j)
term(85) = term(85) + wm_interm_12_triplet_pt2(a,q,i,j) * wm_interm_20_triplet_pt2(a,i,p,j)
term(86) = term(86) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_8_triplet_pt2(a,q,i,j)
term(87) = term(87) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_8_triplet_pt2(a,q,i,j)
term(88) = term(88) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_6_triplet_pt2(a,q,i,j)
term(89) = term(89) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_6_triplet_pt2(a,q,i,j)
term(90) = term(90) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,i,p)
term(91) = term(91) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,i,p)
term(92) = term(92) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,i,p)
term(93) = term(93) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,p,i)
term(94) = term(94) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,p,i)
term(95) = term(95) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,p,i)
term(96) = term(96) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,p,i)
term(97) = term(97) + wm_interm_45_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,i,p)
term(98) = term(98) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,p,i)
term(99) = term(99) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,p,i)
term(100) = term(100) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,p,i)
term(101) = term(101) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,i,p)
term(102) = term(102) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,i,p)
term(103) = term(103) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,i,p)
term(104) = term(104) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,i,p)
term(105) = term(105) + wm_interm_48_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,p,i)
term(106) = term(106) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,i,p)
term(107) = term(107) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,i,p)
term(108) = term(108) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,i,p)
term(109) = term(109) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,p,i)
term(110) = term(110) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,p,i)
term(111) = term(111) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,p,i)
term(112) = term(112) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,i,p)
term(113) = term(113) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,i,p)
term(114) = term(114) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,i,p)
term(115) = term(115) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_54_triplet_pt2(a,j,p,i)
term(116) = term(116) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_50_triplet_pt2(a,j,p,i)
term(117) = term(117) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_51_triplet_pt2(a,j,p,i)
term(118) = term(118) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,p,i)
term(119) = term(119) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,i,p)
term(120) = term(120) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,p,i)
term(121) = term(121) + wm_interm_46_triplet_pt2(a,q,i,j) * wm_interm_49_triplet_pt2(a,j,i,p)
term(122) = term(122) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_58_triplet_pt2(a,q,i,j)
term(123) = term(123) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_58_triplet_pt2(a,q,i,j)
term(124) = term(124) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_55_triplet_pt2(a,q,i,j)
term(125) = term(125) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_55_triplet_pt2(a,q,i,j)
term(126) = term(126) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_58_triplet_pt2(a,q,i,j)
term(127) = term(127) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_55_triplet_pt2(a,q,i,j)
term(128) = term(128) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_55_triplet_pt2(a,q,i,j)
term(129) = term(129) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_58_triplet_pt2(a,q,i,j)
term(130) = term(130) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_58_triplet_pt2(a,q,i,j)
term(131) = term(131) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_55_triplet_pt2(a,q,i,j)
term(132) = term(132) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_55_triplet_pt2(a,q,i,j)
term(133) = term(133) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_58_triplet_pt2(a,q,i,j)
term(134) = term(134) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(135) = term(135) + wm_interm_19_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(136) = term(136) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(137) = term(137) + wm_interm_19_triplet_pt2(a,i,p,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(138) = term(138) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(139) = term(139) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(140) = term(140) + wm_interm_22_triplet_pt2(a,i,p,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(141) = term(141) + wm_interm_21_triplet_pt2(a,i,p,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(142) = term(142) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(143) = term(143) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(144) = term(144) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(145) = term(145) + wm_interm_20_triplet_pt2(a,i,p,j) * wm_interm_52_triplet_pt2(a,q,i,j)
end do 
end do 
end do 

term(16) = term(16) * (3.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(20) = term(20) * (3.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (3.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (3.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (3.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (3.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-6.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (8.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (3.0d+0) 
term(41) = term(41) * (-6.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (8.0d+0) 
term(47) = term(47) * (-8.0d+0) 
term(48) = term(48) * (3.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (3.0d+0) 
term(51) = term(51) * (3.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (3.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (3.0d+0) 
term(57) = term(57) * (3.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (3.0d+0) 
term(60) = term(60) * (3.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (4.0d+0) 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (-2.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (4.0d+0) 
term(72) = term(72) * (3.0d+0) 
term(73) = term(73) * (3.0d+0) 
term(74) = term(74) * (-6.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (3.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (3.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (-6.0d+0) 
term(83) = term(83) * (8.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (8.0d+0) 
term(89) = term(89) * (-8.0d+0) 
term(90) = term(90) * (12.0d+0) 
term(91) = term(91) * (-12.0d+0) 
term(92) = term(92) * (6.0d+0) 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * (-16.0d+0) 
term(95) = term(95) * (16.0d+0) 
term(96) = term(96) * (6.0d+0) 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * (12.0d+0) 
term(99) = term(99) * (-12.0d+0) 
term(100) = term(100) * (6.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (-16.0d+0) 
term(103) = term(103) * (16.0d+0) 
term(104) = term(104) * (6.0d+0) 
term(105) = term(105) * (-8.0d+0) 
term(106) = term(106) * (12.0d+0) 
term(107) = term(107) * (-12.0d+0) 
term(108) = term(108) * (6.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (-16.0d+0) 
term(111) = term(111) * (16.0d+0) 
term(112) = term(112) * (-24.0d+0) 
term(113) = term(113) * (24.0d+0) 
term(114) = term(114) * (-12.0d+0) 
term(115) = term(115) * (8.0d+0) 
term(116) = term(116) * (32.0d+0) 
term(117) = term(117) * (-32.0d+0) 
term(118) = term(118) * (6.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-12.0d+0) 
term(121) = term(121) * (16.0d+0) 
term(122) = term(122) * (12.0d+0) 
term(123) = term(123) * (-12.0d+0) 
term(124) = term(124) * (12.0d+0) 
term(125) = term(125) * (-12.0d+0) 
term(126) = term(126) * (12.0d+0) 
term(127) = term(127) * (12.0d+0) 
term(128) = term(128) * (-12.0d+0) 
term(129) = term(129) * (-12.0d+0) 
term(130) = term(130) * (-16.0d+0) 
term(131) = term(131) * (-16.0d+0) 
term(132) = term(132) * (16.0d+0) 
term(133) = term(133) * (16.0d+0) 
term(134) = term(134) * (12.0d+0) 
term(135) = term(135) * (-12.0d+0) 
term(136) = term(136) * (-8.0d+0) 
term(137) = term(137) * (8.0d+0) 
term(138) = term(138) * (12.0d+0) 
term(139) = term(139) * (-16.0d+0) 
term(140) = term(140) * (-12.0d+0) 
term(141) = term(141) * (16.0d+0) 
term(142) = term(142) * (-16.0d+0) 
term(143) = term(143) * (16.0d+0) 
term(144) = term(144) * (16.0d+0) 
term(145) = term(145) * (-16.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(146) = term(146) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_45_triplet_pt2(c,a,p,k)
term(147) = term(147) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_46_triplet_pt2(c,a,p,k)
term(148) = term(148) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_45_triplet_pt2(c,a,p,i)
term(149) = term(149) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_46_triplet_pt2(c,a,p,i)
term(150) = term(150) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_48_triplet_pt2(c,a,p,k)
term(151) = term(151) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_47_triplet_pt2(c,a,p,i)
term(152) = term(152) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,k) * wm_interm_48_triplet_pt2(c,a,p,i)
term(153) = term(153) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_47_triplet_pt2(c,a,p,k)
term(154) = term(154) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_45_triplet_pt2(c,a,p,k)
term(155) = term(155) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_46_triplet_pt2(c,a,p,k)
term(156) = term(156) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_45_triplet_pt2(c,a,p,i)
term(157) = term(157) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_46_triplet_pt2(c,a,p,i)
term(158) = term(158) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_47_triplet_pt2(c,a,p,k)
term(159) = term(159) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_48_triplet_pt2(c,a,p,k)
term(160) = term(160) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_47_triplet_pt2(c,a,p,i)
term(161) = term(161) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,c,k) * wm_interm_48_triplet_pt2(c,a,p,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * (4.0d+0) 
term(148) = term(148) * (1.5d+0) 
term(149) = term(149) * (-3.0d+0) 
term(150) = term(150) * (1.5d+0) 
term(151) = term(151) * (1.5d+0) 
term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * (-6.0d+0) 
term(155) = term(155) * (12.0d+0) 
term(156) = term(156) * (6.0d+0) 
term(157) = term(157) * (-12.0d+0) 
term(158) = term(158) * (-6.0d+0) 
term(159) = term(159) * (6.0d+0) 
term(160) = term(160) * (6.0d+0) 
term(161) = term(161) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(162) = term(162) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,l,i,p,j,k)
term(163) = term(163) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,l,i,j,p,k)
term(164) = term(164) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,l,i,j,k,p)
term(165) = term(165) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,i,l,j,k,p)
term(166) = term(166) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,i,l,j,p,k)
term(167) = term(167) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,i,l,p,j,k)
term(168) = term(168) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,l,i,p,j,k)
term(169) = term(169) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,l,i,j,p,k)
term(170) = term(170) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,l,i,j,k,p)
term(171) = term(171) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,i,l,j,k,p)
term(172) = term(172) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,i,l,j,p,k)
term(173) = term(173) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,i,l,p,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(162) = term(162) * (3.0d+0) 
term(163) = term(163) * (-4.0d+0) 
term(164) = term(164) * (3.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * (3.0d+0) 
term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (6.0d+0) 
term(169) = term(169) * (-8.0d+0) 
term(170) = term(170) * (6.0d+0) 
term(171) = term(171) * (-8.0d+0) 
term(172) = term(172) * (6.0d+0) 
term(173) = term(173) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(174) = term(174) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_107_triplet_pt2(i,j,k,p)
term(175) = term(175) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_107_triplet_pt2(j,i,p,k)
term(176) = term(176) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_107_triplet_pt2(i,j,p,k)
term(177) = term(177) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_108_triplet_pt2(q,i,j,k)
term(178) = term(178) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_110_triplet_pt2(q,i,j,k)
term(179) = term(179) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_110_triplet_pt2(q,i,j,k)
term(180) = term(180) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_111_triplet_pt2(q,i,j,k)
term(181) = term(181) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_113_triplet_pt2(q,i,j,k)
term(182) = term(182) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_114_triplet_pt2(q,i,j,k)
term(183) = term(183) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_108_triplet_pt2(q,i,j,k)
term(184) = term(184) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_107_triplet_pt2(j,i,k,p)
term(185) = term(185) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_113_triplet_pt2(q,i,j,k)
term(186) = term(186) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_114_triplet_pt2(q,i,j,k)
term(187) = term(187) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_111_triplet_pt2(q,i,j,k)
term(188) = term(188) + wm_interm_120_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(k,p,i,j)
term(189) = term(189) + wm_interm_121_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(k,p,i,j)
term(190) = term(190) + wm_interm_120_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(p,k,i,j)
term(191) = term(191) + wm_interm_121_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(p,k,i,j)
term(192) = term(192) + wm_interm_117_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(k,p,i,j)
term(193) = term(193) + wm_interm_116_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(p,k,i,j)
term(194) = term(194) + wm_interm_117_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(p,k,i,j)
term(195) = term(195) + wm_interm_122_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(p,k,i,j)
term(196) = term(196) + wm_interm_122_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(k,p,i,j)
term(197) = term(197) + wm_interm_116_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(k,p,i,j)
term(198) = term(198) + wm_interm_123_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(p,k,i,j)
term(199) = term(199) + wm_interm_123_triplet_pt2(q,i,j,k) * wm_interm_16_triplet_pt2(k,p,i,j)
term(200) = term(200) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,k,p)
term(201) = term(201) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,p,k)
term(202) = term(202) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,p,k)
term(203) = term(203) + wm_interm_108_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,k,p)
term(204) = term(204) + wm_interm_108_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,p,k)
term(205) = term(205) + wm_interm_108_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,p,k)
term(206) = term(206) + wm_interm_110_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,k,p)
term(207) = term(207) + wm_interm_110_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,p,k)
term(208) = term(208) + wm_interm_110_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,p,k)
term(209) = term(209) + wm_interm_111_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,k,p)
term(210) = term(210) + wm_interm_111_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,k,p)
term(211) = term(211) + wm_interm_111_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,p,k)
term(212) = term(212) + wm_interm_110_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,k,p)
term(213) = term(213) + wm_interm_113_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,k,p)
term(214) = term(214) + wm_interm_113_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,p,k)
term(215) = term(215) + wm_interm_113_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,p,k)
term(216) = term(216) + wm_interm_114_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,k,p)
term(217) = term(217) + wm_interm_114_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,p,k)
term(218) = term(218) + wm_interm_114_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,p,k)
term(219) = term(219) + wm_interm_108_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,k,p)
term(220) = term(220) + wm_interm_103_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(j,i,k,p)
term(221) = term(221) + wm_interm_113_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,k,p)
term(222) = term(222) + wm_interm_114_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,k,p)
term(223) = term(223) + wm_interm_111_triplet_pt2(q,i,j,k) * wm_interm_138_triplet_pt2(i,j,p,k)
end do 
end do 
end do 

term(174) = term(174) * (1.5d+0) 
term(175) = term(175) * (1.5d+0) 
term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * (1.5d+0) 
term(178) = term(178) * (-2.0d+0) 
term(179) = term(179) * (2.0d+0) 
term(180) = term(180) * (1.5d+0) 
term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * (1.5d+0) 
term(183) = term(183) * (-2.0d+0) 
term(184) = term(184) * (-1.0d+0) 
term(185) = term(185) * (2.0d+0) 
term(186) = term(186) * (-1.0d+0) 
term(187) = term(187) * (-1.0d+0) 
term(188) = term(188) * (6.0d+0) 
term(189) = term(189) * (-6.0d+0) 
term(190) = term(190) * (-8.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (6.0d+0) 
term(193) = term(193) * (6.0d+0) 
term(194) = term(194) * (-6.0d+0) 
term(195) = term(195) * (6.0d+0) 
term(196) = term(196) * (-8.0d+0) 
term(197) = term(197) * (-6.0d+0) 
term(198) = term(198) * (-6.0d+0) 
term(199) = term(199) * (8.0d+0) 
term(200) = term(200) * (3.0d+0) 
term(201) = term(201) * (3.0d+0) 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (3.0d+0) 
term(204) = term(204) * (3.0d+0) 
term(205) = term(205) * (-2.0d+0) 
term(206) = term(206) * (-4.0d+0) 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (3.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (3.0d+0) 
term(212) = term(212) * (4.0d+0) 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * (4.0d+0) 
term(216) = term(216) * (3.0d+0) 
term(217) = term(217) * (3.0d+0) 
term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(224) = term(224) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,l,i,k,j,p)
term(225) = term(225) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,l,i,p,k,j)
term(226) = term(226) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,l,i,k,p,j)
term(227) = term(227) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,i,l,k,j,p)
term(228) = term(228) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,i,l,k,p,j)
term(229) = term(229) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_15_triplet_pt2(a,i,l,p,k,j)
term(230) = term(230) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,l,i,k,j,p)
term(231) = term(231) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,l,i,p,k,j)
term(232) = term(232) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,l,i,k,p,j)
term(233) = term(233) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,i,l,k,j,p)
term(234) = term(234) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,i,l,k,p,j)
term(235) = term(235) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_57_triplet_pt2(a,i,l,p,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (4.0d+0) 
term(227) = term(227) * (3.0d+0) 
term(228) = term(228) * (-4.0d+0) 
term(229) = term(229) * (4.0d+0) 
term(230) = term(230) * (-4.0d+0) 
term(231) = term(231) * (-8.0d+0) 
term(232) = term(232) * (8.0d+0) 
term(233) = term(233) * (6.0d+0) 
term(234) = term(234) * (-8.0d+0) 
term(235) = term(235) * (8.0d+0) 

do a = nocc + 1, nactive 
term(236) = term(236) + wm_interm_32_triplet_pt2(a,q) * wm_interm_35_triplet_pt2(a,p)
term(237) = term(237) + wm_interm_32_triplet_pt2(a,q) * wm_interm_36_triplet_pt2(a,p)
term(238) = term(238) + wm_interm_14_triplet_pt2(a,q) * wm_interm_35_triplet_pt2(a,p)
term(239) = term(239) + wm_interm_14_triplet_pt2(a,q) * wm_interm_36_triplet_pt2(a,p)
term(240) = term(240) + wm_interm_30_triplet_pt2(a,q) * wm_interm_35_triplet_pt2(a,p)
term(241) = term(241) + wm_interm_30_triplet_pt2(a,q) * wm_interm_36_triplet_pt2(a,p)
term(242) = term(242) + wm_interm_32_triplet_pt2(a,q) * wm_interm_37_triplet_pt2(a,p)
term(243) = term(243) + wm_interm_14_triplet_pt2(a,q) * wm_interm_37_triplet_pt2(a,p)
term(244) = term(244) + wm_interm_30_triplet_pt2(a,q) * wm_interm_37_triplet_pt2(a,p)
term(245) = term(245) + wm_interm_32_triplet_pt2(a,q) * wm_interm_38_triplet_pt2(a,p)
term(246) = term(246) + wm_interm_14_triplet_pt2(a,q) * wm_interm_38_triplet_pt2(a,p)
term(247) = term(247) + wm_interm_30_triplet_pt2(a,q) * wm_interm_38_triplet_pt2(a,p)
term(248) = term(248) + wm_interm_39_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(249) = term(249) + wm_interm_40_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(250) = term(250) + wm_interm_41_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(251) = term(251) + wm_interm_42_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(252) = term(252) + wm_interm_43_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(253) = term(253) + wm_interm_44_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(254) = term(254) + wm_interm_39_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(255) = term(255) + wm_interm_40_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(256) = term(256) + wm_interm_41_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(257) = term(257) + wm_interm_42_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(258) = term(258) + wm_interm_43_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(259) = term(259) + wm_interm_44_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(260) = term(260) + wm_interm_35_triplet_pt2(a,p) * wm_interm_56_triplet_pt2(a,q)
term(261) = term(261) + wm_interm_36_triplet_pt2(a,p) * wm_interm_56_triplet_pt2(a,q)
term(262) = term(262) + wm_interm_35_triplet_pt2(a,p) * wm_interm_61_triplet_pt2(a,q)
term(263) = term(263) + wm_interm_36_triplet_pt2(a,p) * wm_interm_61_triplet_pt2(a,q)
term(264) = term(264) + wm_interm_37_triplet_pt2(a,p) * wm_interm_56_triplet_pt2(a,q)
term(265) = term(265) + wm_interm_37_triplet_pt2(a,p) * wm_interm_61_triplet_pt2(a,q)
term(266) = term(266) + wm_interm_38_triplet_pt2(a,p) * wm_interm_56_triplet_pt2(a,q)
term(267) = term(267) + wm_interm_38_triplet_pt2(a,p) * wm_interm_61_triplet_pt2(a,q)
term(268) = term(268) + wm_interm_5_triplet_pt2(a,q) * wm_interm_63_triplet_pt2(a,p)
term(269) = term(269) + wm_interm_5_triplet_pt2(a,q) * wm_interm_64_triplet_pt2(a,p)
term(270) = term(270) + wm_interm_5_triplet_pt2(a,q) * wm_interm_65_triplet_pt2(a,p)
term(271) = term(271) + wm_interm_5_triplet_pt2(a,q) * wm_interm_66_triplet_pt2(a,p)
term(272) = term(272) + wm_interm_63_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(273) = term(273) + wm_interm_64_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(274) = term(274) + wm_interm_65_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
term(275) = term(275) + wm_interm_66_triplet_pt2(a,p) * wm_interm_7_triplet_pt2(a,q)
end do 

term(236) = term(236) * (4.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (4.0d+0) 
term(239) = term(239) * (-2.0d+0) 
term(240) = term(240) * (-8.0d+0) 
term(241) = term(241) * (4.0d+0) 
term(242) = term(242) * (-3.0d+0) 
term(243) = term(243) * (-3.0d+0) 
term(244) = term(244) * (6.0d+0) 
term(247) = term(247) * (-2.0d+0) 
term(248) = term(248) * (3.0d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (3.0d+0) 
term(251) = term(251) * (-2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (4.0d+0) 
term(254) = term(254) * (-6.0d+0) 
term(255) = term(255) * (8.0d+0) 
term(256) = term(256) * (-6.0d+0) 
term(257) = term(257) * (4.0d+0) 
term(258) = term(258) * (8.0d+0) 
term(259) = term(259) * (-8.0d+0) 
term(260) = term(260) * (16.0d+0) 
term(261) = term(261) * (-8.0d+0) 
term(262) = term(262) * (-16.0d+0) 
term(263) = term(263) * (8.0d+0) 
term(264) = term(264) * (-12.0d+0) 
term(265) = term(265) * (12.0d+0) 
term(266) = term(266) * (4.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * (12.0d+0) 
term(269) = term(269) * (-12.0d+0) 
term(270) = term(270) * (-8.0d+0) 
term(271) = term(271) * (8.0d+0) 
term(272) = term(272) * (-24.0d+0) 
term(273) = term(273) * (24.0d+0) 
term(274) = term(274) * (16.0d+0) 
term(275) = term(275) * (-16.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(276) = term(276) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_45_triplet_pt2(c,b,p,k)
term(277) = term(277) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_46_triplet_pt2(c,b,p,k)
term(278) = term(278) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,k) * wm_interm_45_triplet_pt2(c,b,p,i)
term(279) = term(279) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,k) * wm_interm_46_triplet_pt2(c,b,p,i)
term(280) = term(280) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_86_triplet_pt2(b,c,j,p)
term(281) = term(281) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_87_triplet_pt2(b,c,j,p)
term(282) = term(282) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_89_triplet_pt2(b,c,j,p)
term(283) = term(283) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_85_triplet_pt2(b,c,j,p)
term(284) = term(284) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_93_triplet_pt2(b,c,j,p)
term(285) = term(285) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_88_triplet_pt2(b,c,j,p)
term(286) = term(286) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_84_triplet_pt2(b,c,j,p)
term(287) = term(287) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_85_triplet_pt2(b,c,j,p)
term(288) = term(288) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_86_triplet_pt2(b,c,j,p)
term(289) = term(289) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_87_triplet_pt2(b,c,j,p)
term(290) = term(290) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_88_triplet_pt2(b,c,j,p)
term(291) = term(291) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_89_triplet_pt2(b,c,j,p)
term(292) = term(292) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_86_triplet_pt2(b,c,i,p)
term(293) = term(293) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_87_triplet_pt2(b,c,i,p)
term(294) = term(294) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_89_triplet_pt2(b,c,i,p)
term(295) = term(295) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_85_triplet_pt2(b,c,i,p)
term(296) = term(296) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_93_triplet_pt2(b,c,i,p)
term(297) = term(297) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_88_triplet_pt2(b,c,i,p)
term(298) = term(298) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_84_triplet_pt2(b,c,k,p)
term(299) = term(299) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_85_triplet_pt2(b,c,k,p)
term(300) = term(300) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_86_triplet_pt2(b,c,k,p)
term(301) = term(301) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_87_triplet_pt2(b,c,k,p)
term(302) = term(302) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_88_triplet_pt2(b,c,k,p)
term(303) = term(303) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_89_triplet_pt2(b,c,k,p)
term(304) = term(304) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_84_triplet_pt2(a,c,j,p)
term(305) = term(305) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_85_triplet_pt2(a,c,j,p)
term(306) = term(306) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_86_triplet_pt2(a,c,j,p)
term(307) = term(307) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_87_triplet_pt2(a,c,j,p)
term(308) = term(308) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_88_triplet_pt2(a,c,j,p)
term(309) = term(309) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_89_triplet_pt2(a,c,j,p)
term(310) = term(310) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_86_triplet_pt2(a,c,i,p)
term(311) = term(311) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_87_triplet_pt2(a,c,i,p)
term(312) = term(312) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_85_triplet_pt2(a,c,i,p)
term(313) = term(313) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_84_triplet_pt2(a,c,k,p)
term(314) = term(314) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_85_triplet_pt2(a,c,k,p)
term(315) = term(315) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_86_triplet_pt2(a,c,k,p)
term(316) = term(316) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_87_triplet_pt2(a,c,k,p)
term(317) = term(317) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_89_triplet_pt2(a,c,i,p)
term(318) = term(318) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_93_triplet_pt2(a,c,i,p)
term(319) = term(319) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_88_triplet_pt2(a,c,i,p)
term(320) = term(320) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_88_triplet_pt2(a,c,k,p)
term(321) = term(321) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_89_triplet_pt2(a,c,k,p)
term(322) = term(322) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_84_triplet_pt2(a,c,i,p)
term(323) = term(323) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_48_triplet_pt2(c,b,p,k)
term(324) = term(324) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_47_triplet_pt2(c,b,p,k)
term(325) = term(325) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_93_triplet_pt2(a,c,j,p)
term(326) = term(326) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,k) * wm_interm_47_triplet_pt2(c,b,p,i)
term(327) = term(327) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,j,c,k) * wm_interm_48_triplet_pt2(c,b,p,i)
term(328) = term(328) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_84_triplet_pt2(a,c,j,p)
term(329) = term(329) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_85_triplet_pt2(a,c,j,p)
term(330) = term(330) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_86_triplet_pt2(a,c,j,p)
term(331) = term(331) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_87_triplet_pt2(a,c,j,p)
term(332) = term(332) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_88_triplet_pt2(a,c,j,p)
term(333) = term(333) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_89_triplet_pt2(a,c,j,p)
term(334) = term(334) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_93_triplet_pt2(a,c,j,p)
term(335) = term(335) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_93_triplet_pt2(b,c,j,p)
term(336) = term(336) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_93_triplet_pt2(b,c,k,p)
term(337) = term(337) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_84_triplet_pt2(b,c,j,p)
term(338) = term(338) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_84_triplet_pt2(b,c,i,p)
term(339) = term(339) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_93_triplet_pt2(a,c,k,p)
term(340) = term(340) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_45_triplet_pt2(c,b,p,k)
term(341) = term(341) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_46_triplet_pt2(c,b,p,k)
term(342) = term(342) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_45_triplet_pt2(c,b,p,i)
term(343) = term(343) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_46_triplet_pt2(c,b,p,i)
term(344) = term(344) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_128_triplet_pt2(b,c,j,p)
term(345) = term(345) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_129_triplet_pt2(b,c,j,p)
term(346) = term(346) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_127_triplet_pt2(b,c,j,p)
term(347) = term(347) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_126_triplet_pt2(b,c,j,p)
term(348) = term(348) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_126_triplet_pt2(b,c,j,p)
term(349) = term(349) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_127_triplet_pt2(b,c,j,p)
term(350) = term(350) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_128_triplet_pt2(b,c,j,p)
term(351) = term(351) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_129_triplet_pt2(b,c,j,p)
term(352) = term(352) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_128_triplet_pt2(b,c,i,p)
term(353) = term(353) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_129_triplet_pt2(b,c,i,p)
term(354) = term(354) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_127_triplet_pt2(b,c,i,p)
term(355) = term(355) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_126_triplet_pt2(b,c,i,p)
term(356) = term(356) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_126_triplet_pt2(b,c,k,p)
term(357) = term(357) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_127_triplet_pt2(b,c,k,p)
term(358) = term(358) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_128_triplet_pt2(b,c,k,p)
term(359) = term(359) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_129_triplet_pt2(b,c,k,p)
term(360) = term(360) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_126_triplet_pt2(a,c,j,p)
term(361) = term(361) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_127_triplet_pt2(a,c,j,p)
term(362) = term(362) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_128_triplet_pt2(a,c,j,p)
term(363) = term(363) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_129_triplet_pt2(a,c,j,p)
term(364) = term(364) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_128_triplet_pt2(a,c,i,p)
term(365) = term(365) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_129_triplet_pt2(a,c,i,p)
term(366) = term(366) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_127_triplet_pt2(a,c,i,p)
term(367) = term(367) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_126_triplet_pt2(a,c,k,p)
term(368) = term(368) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_127_triplet_pt2(a,c,k,p)
term(369) = term(369) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_128_triplet_pt2(a,c,k,p)
term(370) = term(370) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_129_triplet_pt2(a,c,k,p)
term(371) = term(371) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_126_triplet_pt2(a,c,i,p)
term(372) = term(372) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_48_triplet_pt2(c,b,p,k)
term(373) = term(373) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_47_triplet_pt2(c,b,p,k)
term(374) = term(374) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_47_triplet_pt2(c,b,p,i)
term(375) = term(375) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,c,k) * wm_interm_48_triplet_pt2(c,b,p,i)
term(376) = term(376) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_126_triplet_pt2(a,c,j,p)
term(377) = term(377) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_127_triplet_pt2(a,c,j,p)
term(378) = term(378) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_128_triplet_pt2(a,c,j,p)
term(379) = term(379) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_129_triplet_pt2(a,c,j,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(276) = term(276) * (2.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (-1.0d+0) 
term(279) = term(279) * (2.0d+0) 
term(280) = term(280) * (1.5d+0) 
term(281) = term(281) * (-3.0d+0) 
term(282) = term(282) * (1.5d+0) 
term(283) = term(283) * (1.5d+0) 
term(284) = term(284) * (1.5d+0) 
term(285) = term(285) * (-2.0d+0) 
term(286) = term(286) * (1.5d+0) 
term(287) = term(287) * (-2.0d+0) 
term(288) = term(288) * (-2.0d+0) 
term(289) = term(289) * (4.0d+0) 
term(290) = term(290) * (1.5d+0) 
term(291) = term(291) * (-2.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * (4.0d+0) 
term(294) = term(294) * (-2.0d+0) 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (-2.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * (2.0d+0) 
term(300) = term(300) * (2.0d+0) 
term(301) = term(301) * (-4.0d+0) 
term(302) = term(302) * (-2.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (1.5d+0) 
term(305) = term(305) * (-1.0d+0) 
term(306) = term(306) * (-1.0d+0) 
term(307) = term(307) * (2.0d+0) 
term(308) = term(308) * (1.5d+0) 
term(309) = term(309) * (-1.0d+0) 
term(310) = term(310) * (1.5d+0) 
term(311) = term(311) * (-3.0d+0) 
term(312) = term(312) * (1.5d+0) 
term(313) = term(313) * (1.5d+0) 
term(314) = term(314) * (-2.0d+0) 
term(315) = term(315) * (-2.0d+0) 
term(316) = term(316) * (4.0d+0) 
term(317) = term(317) * (1.5d+0) 
term(318) = term(318) * (1.5d+0) 
term(319) = term(319) * (-1.0d+0) 
term(320) = term(320) * (1.5d+0) 
term(321) = term(321) * (-2.0d+0) 
term(322) = term(322) * (-2.0d+0) 
term(323) = term(323) * (-1.0d+0) 
term(324) = term(324) * (2.0d+0) 
term(325) = term(325) * (-2.0d+0) 
term(326) = term(326) * (-1.0d+0) 
term(327) = term(327) * (2.0d+0) 
term(328) = term(328) * (-2.0d+0) 
term(329) = term(329) * (2.0d+0) 
term(330) = term(330) * (2.0d+0) 
term(331) = term(331) * (-4.0d+0) 
term(332) = term(332) * (-2.0d+0) 
term(333) = term(333) * (2.0d+0) 
term(334) = term(334) * (2.0d+0) 
term(335) = term(335) * (-1.0d+0) 
term(336) = term(336) * (2.0d+0) 
term(337) = term(337) * (-1.0d+0) 
term(338) = term(338) * (2.0d+0) 
term(339) = term(339) * (-1.0d+0) 
term(340) = term(340) * (8.0d+0) 
term(341) = term(341) * (-16.0d+0) 
term(342) = term(342) * (-6.0d+0) 
term(343) = term(343) * (12.0d+0) 
term(344) = term(344) * (6.0d+0) 
term(345) = term(345) * (-6.0d+0) 
term(346) = term(346) * (6.0d+0) 
term(347) = term(347) * (-6.0d+0) 
term(348) = term(348) * (6.0d+0) 
term(349) = term(349) * (-6.0d+0) 
term(350) = term(350) * (-8.0d+0) 
term(351) = term(351) * (8.0d+0) 
term(352) = term(352) * (-8.0d+0) 
term(353) = term(353) * (8.0d+0) 
term(354) = term(354) * (-8.0d+0) 
term(355) = term(355) * (8.0d+0) 
term(356) = term(356) * (-8.0d+0) 
term(357) = term(357) * (8.0d+0) 
term(358) = term(358) * (8.0d+0) 
term(359) = term(359) * (-8.0d+0) 
term(360) = term(360) * (6.0d+0) 
term(361) = term(361) * (-6.0d+0) 
term(362) = term(362) * (-4.0d+0) 
term(363) = term(363) * (4.0d+0) 
term(364) = term(364) * (6.0d+0) 
term(365) = term(365) * (-6.0d+0) 
term(366) = term(366) * (6.0d+0) 
term(367) = term(367) * (6.0d+0) 
term(368) = term(368) * (-6.0d+0) 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * (8.0d+0) 
term(371) = term(371) * (-6.0d+0) 
term(372) = term(372) * (-6.0d+0) 
term(373) = term(373) * (8.0d+0) 
term(374) = term(374) * (-6.0d+0) 
term(375) = term(375) * (8.0d+0) 
term(376) = term(376) * (-8.0d+0) 
term(377) = term(377) * (8.0d+0) 
term(378) = term(378) * (8.0d+0) 
term(379) = term(379) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(380) = term(380) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,j,k,p,i,l)
term(381) = term(381) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,k,j,p,i,l)
term(382) = term(382) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,k,j,p,i,l)
term(383) = term(383) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,j,k,p,i,l)
term(384) = term(384) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,p,j,k,i,l)
term(385) = term(385) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,p,j,k,i,l)
term(386) = term(386) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,j,k,p,i,l)
term(387) = term(387) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,k,j,p,i,l)
term(388) = term(388) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,k,j,p,i,l)
term(389) = term(389) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,j,k,p,i,l)
term(390) = term(390) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,p,j,k,i,l)
term(391) = term(391) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,p,j,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(380) = term(380) * (3.0d+0) 
term(381) = term(381) * (-4.0d+0) 
term(382) = term(382) * (3.0d+0) 
term(383) = term(383) * (-2.0d+0) 
term(384) = term(384) * (3.0d+0) 
term(385) = term(385) * (-4.0d+0) 
term(386) = term(386) * (6.0d+0) 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * (6.0d+0) 
term(389) = term(389) * (-4.0d+0) 
term(390) = term(390) * (6.0d+0) 
term(391) = term(391) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(392) = term(392) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_72_triplet_pt2(q,j,k,i)
term(393) = term(393) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_73_triplet_pt2(q,j,k,i)
term(394) = term(394) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_72_triplet_pt2(q,j,k,i)
term(395) = term(395) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_73_triplet_pt2(q,j,k,i)
term(396) = term(396) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(q,j,k,i)
term(397) = term(397) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(q,j,k,i)
term(398) = term(398) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_68_triplet_pt2(q,j,k,i)
term(399) = term(399) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_67_triplet_pt2(q,j,k,i)
term(400) = term(400) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_68_triplet_pt2(q,j,k,i)
term(401) = term(401) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_76_triplet_pt2(q,j,k,i)
term(402) = term(402) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_76_triplet_pt2(q,j,k,i)
term(403) = term(403) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_91_triplet_pt2(q,j,k,i)
term(404) = term(404) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_69_triplet_pt2(q,j,k,i)
term(405) = term(405) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_69_triplet_pt2(q,j,k,i)
term(406) = term(406) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_77_triplet_pt2(q,j,k,i)
term(407) = term(407) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_78_triplet_pt2(q,j,k,i)
term(408) = term(408) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_77_triplet_pt2(q,j,k,i)
term(409) = term(409) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_78_triplet_pt2(q,j,k,i)
term(410) = term(410) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_67_triplet_pt2(q,j,k,i)
term(411) = term(411) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_90_triplet_pt2(q,j,k,i)
term(412) = term(412) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_82_triplet_pt2(q,j,k,i)
term(413) = term(413) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_82_triplet_pt2(q,j,k,i)
term(414) = term(414) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_108_triplet_pt2(q,j,i,k)
term(415) = term(415) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_108_triplet_pt2(q,j,i,k)
term(416) = term(416) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_110_triplet_pt2(q,j,i,k)
term(417) = term(417) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_111_triplet_pt2(q,j,i,k)
term(418) = term(418) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_111_triplet_pt2(q,j,i,k)
term(419) = term(419) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_110_triplet_pt2(q,j,i,k)
term(420) = term(420) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_113_triplet_pt2(q,j,i,k)
term(421) = term(421) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_113_triplet_pt2(q,j,i,k)
term(422) = term(422) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_90_triplet_pt2(q,j,k,i)
term(423) = term(423) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_91_triplet_pt2(q,j,k,i)
term(424) = term(424) + wm_interm_107_triplet_pt2(i,j,k,p) * wm_interm_114_triplet_pt2(q,j,i,k)
term(425) = term(425) + wm_interm_107_triplet_pt2(i,j,p,k) * wm_interm_114_triplet_pt2(q,j,i,k)
end do 
end do 
end do 

term(392) = term(392) * (1.5d+0) 
term(393) = term(393) * (-1.0d+0) 
term(394) = term(394) * (-2.0d+0) 
term(395) = term(395) * (2.0d+0) 
term(396) = term(396) * (1.5d+0) 
term(397) = term(397) * (-2.0d+0) 
term(398) = term(398) * (1.5d+0) 
term(399) = term(399) * (1.5d+0) 
term(400) = term(400) * (-2.0d+0) 
term(401) = term(401) * (1.5d+0) 
term(402) = term(402) * (-2.0d+0) 
term(403) = term(403) * (1.5d+0) 
term(404) = term(404) * (-2.0d+0) 
term(405) = term(405) * (1.5d+0) 
term(406) = term(406) * (1.5d+0) 
term(407) = term(407) * (-1.0d+0) 
term(408) = term(408) * (-2.0d+0) 
term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (-1.0d+0) 
term(411) = term(411) * (-2.0d+0) 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (1.5d+0) 
term(415) = term(415) * (-1.0d+0) 
term(416) = term(416) * (-2.0d+0) 
term(417) = term(417) * (-2.0d+0) 
term(418) = term(418) * (1.5d+0) 
term(419) = term(419) * (2.0d+0) 
term(420) = term(420) * (-2.0d+0) 
term(421) = term(421) * (2.0d+0) 
term(422) = term(422) * (2.0d+0) 
term(423) = term(423) * (-1.0d+0) 
term(424) = term(424) * (1.5d+0) 
term(425) = term(425) * (-2.0d+0) 

do i = 1, nocc 
term(426) = term(426) + wm_interm_1_triplet_pt2(p,i) * wm_interm_70_triplet_pt2(q,i)
term(427) = term(427) + wm_interm_1_triplet_pt2(p,i) * wm_interm_71_triplet_pt2(q,i)
term(428) = term(428) + wm_interm_2_triplet_pt2(p,i) * wm_interm_70_triplet_pt2(q,i)
term(429) = term(429) + wm_interm_2_triplet_pt2(p,i) * wm_interm_71_triplet_pt2(q,i)
term(430) = term(430) + wm_interm_1_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(431) = term(431) + wm_interm_2_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(432) = term(432) + wm_interm_1_triplet_pt2(p,i) * wm_interm_79_triplet_pt2(q,i)
term(433) = term(433) + wm_interm_2_triplet_pt2(p,i) * wm_interm_79_triplet_pt2(q,i)
term(434) = term(434) + wm_interm_1_triplet_pt2(p,i) * wm_interm_80_triplet_pt2(q,i)
term(435) = term(435) + wm_interm_2_triplet_pt2(p,i) * wm_interm_80_triplet_pt2(q,i)
term(436) = term(436) + wm_interm_1_triplet_pt2(p,i) * wm_interm_81_triplet_pt2(q,i)
term(437) = term(437) + wm_interm_2_triplet_pt2(p,i) * wm_interm_81_triplet_pt2(q,i)
term(438) = term(438) + wm_interm_1_triplet_pt2(p,i) * wm_interm_83_triplet_pt2(q,i)
term(439) = term(439) + wm_interm_2_triplet_pt2(p,i) * wm_interm_83_triplet_pt2(q,i)
term(440) = term(440) + wm_interm_1_triplet_pt2(p,i) * wm_interm_92_triplet_pt2(q,i)
term(441) = term(441) + wm_interm_2_triplet_pt2(p,i) * wm_interm_92_triplet_pt2(q,i)
term(442) = term(442) + wm_interm_118_triplet_pt2(q,i) * wm_interm_1_triplet_pt2(p,i)
term(443) = term(443) + wm_interm_119_triplet_pt2(q,i) * wm_interm_1_triplet_pt2(p,i)
term(444) = term(444) + wm_interm_118_triplet_pt2(q,i) * wm_interm_2_triplet_pt2(p,i)
term(445) = term(445) + wm_interm_119_triplet_pt2(q,i) * wm_interm_2_triplet_pt2(p,i)
term(446) = term(446) + wm_interm_124_triplet_pt2(q,i) * wm_interm_1_triplet_pt2(p,i)
term(447) = term(447) + wm_interm_124_triplet_pt2(q,i) * wm_interm_2_triplet_pt2(p,i)
term(448) = term(448) + wm_interm_125_triplet_pt2(q,i) * wm_interm_1_triplet_pt2(p,i)
term(449) = term(449) + wm_interm_125_triplet_pt2(q,i) * wm_interm_2_triplet_pt2(p,i)
term(450) = term(450) + wm_interm_104_triplet_pt2(i,p) * wm_interm_139_triplet_pt2(q,i)
term(451) = term(451) + wm_interm_104_triplet_pt2(i,p) * wm_interm_140_triplet_pt2(q,i)
term(452) = term(452) + wm_interm_105_triplet_pt2(i,p) * wm_interm_139_triplet_pt2(q,i)
term(453) = term(453) + wm_interm_105_triplet_pt2(i,p) * wm_interm_140_triplet_pt2(q,i)
term(454) = term(454) + wm_interm_106_triplet_pt2(i,p) * wm_interm_139_triplet_pt2(q,i)
term(455) = term(455) + wm_interm_106_triplet_pt2(i,p) * wm_interm_140_triplet_pt2(q,i)
term(456) = term(456) + wm_interm_104_triplet_pt2(i,p) * wm_interm_141_triplet_pt2(q,i)
term(457) = term(457) + wm_interm_105_triplet_pt2(i,p) * wm_interm_141_triplet_pt2(q,i)
term(458) = term(458) + wm_interm_106_triplet_pt2(i,p) * wm_interm_141_triplet_pt2(q,i)
term(459) = term(459) + wm_interm_104_triplet_pt2(i,p) * wm_interm_142_triplet_pt2(q,i)
term(460) = term(460) + wm_interm_105_triplet_pt2(i,p) * wm_interm_142_triplet_pt2(q,i)
term(461) = term(461) + wm_interm_106_triplet_pt2(i,p) * wm_interm_142_triplet_pt2(q,i)
term(462) = term(462) + wm_interm_135_triplet_pt2(i,p) * wm_interm_139_triplet_pt2(q,i)
term(463) = term(463) + wm_interm_135_triplet_pt2(i,p) * wm_interm_140_triplet_pt2(q,i)
term(464) = term(464) + wm_interm_136_triplet_pt2(i,p) * wm_interm_139_triplet_pt2(q,i)
term(465) = term(465) + wm_interm_136_triplet_pt2(i,p) * wm_interm_140_triplet_pt2(q,i)
term(466) = term(466) + wm_interm_137_triplet_pt2(i,p) * wm_interm_139_triplet_pt2(q,i)
term(467) = term(467) + wm_interm_137_triplet_pt2(i,p) * wm_interm_140_triplet_pt2(q,i)
term(468) = term(468) + wm_interm_135_triplet_pt2(i,p) * wm_interm_141_triplet_pt2(q,i)
term(469) = term(469) + wm_interm_136_triplet_pt2(i,p) * wm_interm_141_triplet_pt2(q,i)
term(470) = term(470) + wm_interm_137_triplet_pt2(i,p) * wm_interm_141_triplet_pt2(q,i)
term(471) = term(471) + wm_interm_135_triplet_pt2(i,p) * wm_interm_142_triplet_pt2(q,i)
term(472) = term(472) + wm_interm_136_triplet_pt2(i,p) * wm_interm_142_triplet_pt2(q,i)
term(473) = term(473) + wm_interm_137_triplet_pt2(i,p) * wm_interm_142_triplet_pt2(q,i)
end do 

term(426) = term(426) * (3.0d+0) 
term(427) = term(427) * (-2.0d+0) 
term(428) = term(428) * (-6.0d+0) 
term(429) = term(429) * (4.0d+0) 
term(430) = term(430) * (3.0d+0) 
term(431) = term(431) * (-6.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(433) = term(433) * (4.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * (4.0d+0) 
term(436) = term(436) * (-4.0d+0) 
term(437) = term(437) * (8.0d+0) 
term(438) = term(438) * (2.0d+0) 
term(439) = term(439) * (-4.0d+0) 
term(440) = term(440) * (2.0d+0) 
term(441) = term(441) * (-4.0d+0) 
term(442) = term(442) * (12.0d+0) 
term(443) = term(443) * (-12.0d+0) 
term(444) = term(444) * (-24.0d+0) 
term(445) = term(445) * (24.0d+0) 
term(446) = term(446) * (-8.0d+0) 
term(447) = term(447) * (16.0d+0) 
term(448) = term(448) * (8.0d+0) 
term(449) = term(449) * (-16.0d+0) 
term(450) = term(450) * (-4.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (8.0d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (-4.0d+0) 
term(455) = term(455) * (2.0d+0) 
term(456) = term(456) * (3.0d+0) 
term(457) = term(457) * (-6.0d+0) 
term(458) = term(458) * (3.0d+0) 
term(459) = term(459) * (-1.0d+0) 
term(460) = term(460) * (2.0d+0) 
term(461) = term(461) * (-1.0d+0) 
term(462) = term(462) * (-8.0d+0) 
term(463) = term(463) * (4.0d+0) 
term(464) = term(464) * (16.0d+0) 
term(465) = term(465) * (-8.0d+0) 
term(466) = term(466) * (-8.0d+0) 
term(467) = term(467) * (4.0d+0) 
term(468) = term(468) * (6.0d+0) 
term(469) = term(469) * (-12.0d+0) 
term(470) = term(470) * (6.0d+0) 
term(471) = term(471) * (-2.0d+0) 
term(472) = term(472) * (4.0d+0) 
term(473) = term(473) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(474) = term(474) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_45_triplet_pt2(c,b,p,k)
term(475) = term(475) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_46_triplet_pt2(c,b,p,k)
term(476) = term(476) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_45_triplet_pt2(c,b,p,j)
term(477) = term(477) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,i) * wm_interm_45_triplet_pt2(c,b,p,j)
term(478) = term(478) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_46_triplet_pt2(c,b,p,j)
term(479) = term(479) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,i) * wm_interm_46_triplet_pt2(c,b,p,j)
term(480) = term(480) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_45_triplet_pt2(c,b,p,i)
term(481) = term(481) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_46_triplet_pt2(c,b,p,i)
term(482) = term(482) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_45_triplet_pt2(c,a,p,k)
term(483) = term(483) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_46_triplet_pt2(c,a,p,k)
term(484) = term(484) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_45_triplet_pt2(c,a,p,j)
term(485) = term(485) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_46_triplet_pt2(c,a,p,j)
term(486) = term(486) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,j) * wm_interm_45_triplet_pt2(c,a,p,i)
term(487) = term(487) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,j) * wm_interm_46_triplet_pt2(c,a,p,i)
term(488) = term(488) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,i) * wm_interm_45_triplet_pt2(c,a,p,j)
term(489) = term(489) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,i) * wm_interm_46_triplet_pt2(c,a,p,j)
term(490) = term(490) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_47_triplet_pt2(c,a,p,k)
term(491) = term(491) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_48_triplet_pt2(c,a,p,k)
term(492) = term(492) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,j) * wm_interm_47_triplet_pt2(c,a,p,i)
term(493) = term(493) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,j) * wm_interm_48_triplet_pt2(c,a,p,i)
term(494) = term(494) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_48_triplet_pt2(c,b,p,k)
term(495) = term(495) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_47_triplet_pt2(c,b,p,k)
term(496) = term(496) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_47_triplet_pt2(c,b,p,i)
term(497) = term(497) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,j) * wm_interm_48_triplet_pt2(c,b,p,i)
term(498) = term(498) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_47_triplet_pt2(c,b,p,j)
term(499) = term(499) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,i) * wm_interm_48_triplet_pt2(c,b,p,j)
term(500) = term(500) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,c,k) * wm_interm_48_triplet_pt2(c,b,p,j)
term(501) = term(501) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,c,i) * wm_interm_47_triplet_pt2(c,b,p,j)
term(502) = term(502) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_86_triplet_pt2(a,c,i,p)
term(503) = term(503) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_87_triplet_pt2(a,c,i,p)
term(504) = term(504) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_89_triplet_pt2(a,c,i,p)
term(505) = term(505) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_85_triplet_pt2(a,c,i,p)
term(506) = term(506) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_93_triplet_pt2(a,c,i,p)
term(507) = term(507) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_88_triplet_pt2(a,c,i,p)
term(508) = term(508) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_84_triplet_pt2(a,c,i,p)
term(509) = term(509) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_84_triplet_pt2(b,c,k,p)
term(510) = term(510) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_88_triplet_pt2(b,c,k,p)
term(511) = term(511) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_93_triplet_pt2(b,c,k,p)
term(512) = term(512) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_86_triplet_pt2(a,c,k,p)
term(513) = term(513) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_87_triplet_pt2(a,c,k,p)
term(514) = term(514) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_89_triplet_pt2(a,c,k,p)
term(515) = term(515) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_85_triplet_pt2(a,c,k,p)
term(516) = term(516) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_84_triplet_pt2(a,c,k,p)
term(517) = term(517) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_93_triplet_pt2(a,c,k,p)
term(518) = term(518) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_86_triplet_pt2(b,c,i,p)
term(519) = term(519) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_87_triplet_pt2(b,c,i,p)
term(520) = term(520) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_89_triplet_pt2(b,c,i,p)
term(521) = term(521) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_85_triplet_pt2(b,c,i,p)
term(522) = term(522) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_84_triplet_pt2(b,c,i,p)
term(523) = term(523) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_93_triplet_pt2(b,c,i,p)
term(524) = term(524) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_48_triplet_pt2(c,a,p,j)
term(525) = term(525) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_88_triplet_pt2(a,c,k,p)
term(526) = term(526) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_88_triplet_pt2(b,c,i,p)
term(527) = term(527) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,c,k) * wm_interm_47_triplet_pt2(c,a,p,j)
term(528) = term(528) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_85_triplet_pt2(b,c,k,p)
term(529) = term(529) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_86_triplet_pt2(b,c,k,p)
term(530) = term(530) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_87_triplet_pt2(b,c,k,p)
term(531) = term(531) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,i) * wm_interm_48_triplet_pt2(c,a,p,j)
term(532) = term(532) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,k,c,i) * wm_interm_47_triplet_pt2(c,a,p,j)
term(533) = term(533) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_89_triplet_pt2(b,c,k,p)
term(534) = term(534) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_45_triplet_pt2(c,b,p,k)
term(535) = term(535) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_46_triplet_pt2(c,b,p,k)
term(536) = term(536) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_45_triplet_pt2(c,b,p,j)
term(537) = term(537) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_45_triplet_pt2(c,b,p,j)
term(538) = term(538) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_46_triplet_pt2(c,b,p,j)
term(539) = term(539) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_46_triplet_pt2(c,b,p,j)
term(540) = term(540) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_45_triplet_pt2(c,b,p,i)
term(541) = term(541) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_46_triplet_pt2(c,b,p,i)
term(542) = term(542) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_45_triplet_pt2(c,a,p,k)
term(543) = term(543) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_46_triplet_pt2(c,a,p,k)
term(544) = term(544) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_45_triplet_pt2(c,a,p,i)
term(545) = term(545) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_46_triplet_pt2(c,a,p,i)
term(546) = term(546) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_45_triplet_pt2(c,a,p,j)
term(547) = term(547) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_46_triplet_pt2(c,a,p,j)
term(548) = term(548) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_45_triplet_pt2(c,a,p,j)
term(549) = term(549) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_46_triplet_pt2(c,a,p,j)
term(550) = term(550) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_47_triplet_pt2(c,a,p,k)
term(551) = term(551) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_48_triplet_pt2(c,a,p,k)
term(552) = term(552) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_47_triplet_pt2(c,a,p,i)
term(553) = term(553) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,j) * wm_interm_48_triplet_pt2(c,a,p,i)
term(554) = term(554) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_48_triplet_pt2(c,b,p,k)
term(555) = term(555) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_47_triplet_pt2(c,b,p,k)
term(556) = term(556) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_47_triplet_pt2(c,b,p,i)
term(557) = term(557) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,j) * wm_interm_48_triplet_pt2(c,b,p,i)
term(558) = term(558) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_47_triplet_pt2(c,b,p,j)
term(559) = term(559) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_48_triplet_pt2(c,b,p,j)
term(560) = term(560) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,c,k) * wm_interm_48_triplet_pt2(c,b,p,j)
term(561) = term(561) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,c,i) * wm_interm_47_triplet_pt2(c,b,p,j)
term(562) = term(562) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_128_triplet_pt2(a,c,i,p)
term(563) = term(563) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_129_triplet_pt2(a,c,i,p)
term(564) = term(564) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_127_triplet_pt2(a,c,i,p)
term(565) = term(565) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_126_triplet_pt2(a,c,i,p)
term(566) = term(566) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_126_triplet_pt2(b,c,k,p)
term(567) = term(567) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_127_triplet_pt2(b,c,k,p)
term(568) = term(568) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_128_triplet_pt2(a,c,k,p)
term(569) = term(569) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_129_triplet_pt2(a,c,k,p)
term(570) = term(570) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_127_triplet_pt2(a,c,k,p)
term(571) = term(571) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_126_triplet_pt2(a,c,k,p)
term(572) = term(572) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_128_triplet_pt2(b,c,i,p)
term(573) = term(573) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_129_triplet_pt2(b,c,i,p)
term(574) = term(574) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_127_triplet_pt2(b,c,i,p)
term(575) = term(575) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_126_triplet_pt2(b,c,i,p)
term(576) = term(576) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_48_triplet_pt2(c,a,p,j)
term(577) = term(577) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_48_triplet_pt2(c,a,p,j)
term(578) = term(578) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,c,k) * wm_interm_47_triplet_pt2(c,a,p,j)
term(579) = term(579) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,c,i) * wm_interm_47_triplet_pt2(c,a,p,j)
term(580) = term(580) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_128_triplet_pt2(b,c,k,p)
term(581) = term(581) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_129_triplet_pt2(b,c,k,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(474) = term(474) * (-2.0d+0) 
term(475) = term(475) * (4.0d+0) 
term(476) = term(476) * (1.5d+0) 
term(477) = term(477) * (-2.0d+0) 
term(478) = term(478) * (-3.0d+0) 
term(479) = term(479) * (4.0d+0) 
term(480) = term(480) * (1.5d+0) 
term(481) = term(481) * (-3.0d+0) 
term(482) = term(482) * (1.5d+0) 
term(483) = term(483) * (-3.0d+0) 
term(484) = term(484) * (-2.0d+0) 
term(485) = term(485) * (4.0d+0) 
term(486) = term(486) * (-1.0d+0) 
term(487) = term(487) * (2.0d+0) 
term(488) = term(488) * (2.0d+0) 
term(489) = term(489) * (-4.0d+0) 
term(490) = term(490) * (1.5d+0) 
term(491) = term(491) * (-2.0d+0) 
term(492) = term(492) * (-1.0d+0) 
term(493) = term(493) * (2.0d+0) 
term(494) = term(494) * (1.5d+0) 
term(495) = term(495) * (-2.0d+0) 
term(496) = term(496) * (1.5d+0) 
term(497) = term(497) * (-2.0d+0) 
term(498) = term(498) * (1.5d+0) 
term(499) = term(499) * (1.5d+0) 
term(500) = term(500) * (-2.0d+0) 
term(501) = term(501) * (-2.0d+0) 
term(502) = term(502) * (-2.0d+0) 
term(503) = term(503) * (4.0d+0) 
term(504) = term(504) * (-2.0d+0) 
term(505) = term(505) * (-2.0d+0) 
term(506) = term(506) * (-2.0d+0) 
term(507) = term(507) * (2.0d+0) 
term(508) = term(508) * (2.0d+0) 
term(509) = term(509) * (1.5d+0) 
term(510) = term(510) * (1.5d+0) 
term(511) = term(511) * (-2.0d+0) 
term(512) = term(512) * (1.5d+0) 
term(513) = term(513) * (-3.0d+0) 
term(514) = term(514) * (1.5d+0) 
term(515) = term(515) * (1.5d+0) 
term(516) = term(516) * (-1.0d+0) 
term(517) = term(517) * (1.5d+0) 
term(518) = term(518) * (1.5d+0) 
term(519) = term(519) * (-3.0d+0) 
term(520) = term(520) * (1.5d+0) 
term(521) = term(521) * (1.5d+0) 
term(522) = term(522) * (-2.0d+0) 
term(523) = term(523) * (1.5d+0) 
term(524) = term(524) * (1.5d+0) 
term(525) = term(525) * (-2.0d+0) 
term(526) = term(526) * (-1.0d+0) 
term(527) = term(527) * (-2.0d+0) 
term(528) = term(528) * (-1.0d+0) 
term(529) = term(529) * (-1.0d+0) 
term(530) = term(530) * (2.0d+0) 
term(531) = term(531) * (-1.0d+0) 
term(532) = term(532) * (2.0d+0) 
term(533) = term(533) * (-1.0d+0) 
term(534) = term(534) * (-8.0d+0) 
term(535) = term(535) * (16.0d+0) 
term(536) = term(536) * (6.0d+0) 
term(537) = term(537) * (-6.0d+0) 
term(538) = term(538) * (-12.0d+0) 
term(539) = term(539) * (12.0d+0) 
term(540) = term(540) * (6.0d+0) 
term(541) = term(541) * (-12.0d+0) 
term(542) = term(542) * (6.0d+0) 
term(543) = term(543) * (-12.0d+0) 
term(544) = term(544) * (-6.0d+0) 
term(545) = term(545) * (12.0d+0) 
term(546) = term(546) * (-8.0d+0) 
term(547) = term(547) * (16.0d+0) 
term(548) = term(548) * (8.0d+0) 
term(549) = term(549) * (-16.0d+0) 
term(550) = term(550) * (6.0d+0) 
term(551) = term(551) * (-6.0d+0) 
term(552) = term(552) * (-6.0d+0) 
term(553) = term(553) * (8.0d+0) 
term(554) = term(554) * (6.0d+0) 
term(555) = term(555) * (-8.0d+0) 
term(556) = term(556) * (6.0d+0) 
term(557) = term(557) * (-8.0d+0) 
term(558) = term(558) * (6.0d+0) 
term(559) = term(559) * (6.0d+0) 
term(560) = term(560) * (-6.0d+0) 
term(561) = term(561) * (-6.0d+0) 
term(562) = term(562) * (-8.0d+0) 
term(563) = term(563) * (8.0d+0) 
term(564) = term(564) * (-8.0d+0) 
term(565) = term(565) * (8.0d+0) 
term(566) = term(566) * (6.0d+0) 
term(567) = term(567) * (-6.0d+0) 
term(568) = term(568) * (6.0d+0) 
term(569) = term(569) * (-6.0d+0) 
term(570) = term(570) * (6.0d+0) 
term(571) = term(571) * (-6.0d+0) 
term(572) = term(572) * (6.0d+0) 
term(573) = term(573) * (-6.0d+0) 
term(574) = term(574) * (6.0d+0) 
term(575) = term(575) * (-6.0d+0) 
term(576) = term(576) * (6.0d+0) 
term(577) = term(577) * (-6.0d+0) 
term(578) = term(578) * (-8.0d+0) 
term(579) = term(579) * (8.0d+0) 
term(580) = term(580) * (-4.0d+0) 
term(581) = term(581) * (4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(582) = term(582) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,p,j,k,l,i)
term(583) = term(583) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,j,k,p,l,i)
term(584) = term(584) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,k,j,p,l,i)
term(585) = term(585) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,j,k,p,l,i)
term(586) = term(586) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,p,j,k,l,i)
term(587) = term(587) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,k,j,p,l,i)
term(588) = term(588) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,p,j,k,l,i)
term(589) = term(589) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,j,k,p,l,i)
term(590) = term(590) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,k,j,p,l,i)
term(591) = term(591) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt2(a,j,k,p,l,i)
term(592) = term(592) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,p,j,k,l,i)
term(593) = term(593) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_27_triplet_pt2(b,k,j,p,l,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(582) = term(582) * (3.0d+0) 
term(583) = term(583) * (-4.0d+0) 
term(584) = term(584) * (-4.0d+0) 
term(585) = term(585) * (4.0d+0) 
term(586) = term(586) * (-2.0d+0) 
term(587) = term(587) * (4.0d+0) 
term(588) = term(588) * (6.0d+0) 
term(589) = term(589) * (-8.0d+0) 
term(590) = term(590) * (-8.0d+0) 
term(591) = term(591) * (8.0d+0) 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * (8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(594) = term(594) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_45_triplet_pt2(c,b,p,k)
term(595) = term(595) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_46_triplet_pt2(c,b,p,k)
term(596) = term(596) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_45_triplet_pt2(c,b,p,i)
term(597) = term(597) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_46_triplet_pt2(c,b,p,i)
term(598) = term(598) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_45_triplet_pt2(c,a,p,k)
term(599) = term(599) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_46_triplet_pt2(c,a,p,k)
term(600) = term(600) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_45_triplet_pt2(c,a,p,i)
term(601) = term(601) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_46_triplet_pt2(c,a,p,i)
term(602) = term(602) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_47_triplet_pt2(c,a,p,k)
term(603) = term(603) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_48_triplet_pt2(c,a,p,k)
term(604) = term(604) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_48_triplet_pt2(c,b,p,k)
term(605) = term(605) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_47_triplet_pt2(c,b,p,k)
term(606) = term(606) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_47_triplet_pt2(c,b,p,i)
term(607) = term(607) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,a,k) * wm_interm_48_triplet_pt2(c,b,p,i)
term(608) = term(608) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_47_triplet_pt2(c,a,p,i)
term(609) = term(609) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,j,b,k) * wm_interm_48_triplet_pt2(c,a,p,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(594) = term(594) * (-2.0d+0) 
term(595) = term(595) * (4.0d+0) 
term(596) = term(596) * (1.5d+0) 
term(597) = term(597) * (-3.0d+0) 
term(598) = term(598) * (1.5d+0) 
term(599) = term(599) * (-3.0d+0) 
term(600) = term(600) * (-2.0d+0) 
term(601) = term(601) * (4.0d+0) 
term(602) = term(602) * (1.5d+0) 
term(603) = term(603) * (-1.0d+0) 
term(604) = term(604) * (1.5d+0) 
term(605) = term(605) * (-2.0d+0) 
term(606) = term(606) * (1.5d+0) 
term(607) = term(607) * (-2.0d+0) 
term(608) = term(608) * (-2.0d+0) 
term(609) = term(609) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(610) = term(610) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_45_triplet_pt2(c,b,p,k)
term(611) = term(611) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_46_triplet_pt2(c,b,p,k)
term(612) = term(612) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_45_triplet_pt2(c,b,p,j)
term(613) = term(613) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_46_triplet_pt2(c,b,p,j)
term(614) = term(614) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_45_triplet_pt2(c,b,p,i)
term(615) = term(615) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_46_triplet_pt2(c,b,p,i)
term(616) = term(616) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_45_triplet_pt2(c,a,p,i)
term(617) = term(617) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_46_triplet_pt2(c,a,p,i)
term(618) = term(618) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_45_triplet_pt2(c,b,p,j)
term(619) = term(619) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_46_triplet_pt2(c,b,p,j)
term(620) = term(620) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_45_triplet_pt2(c,a,p,k)
term(621) = term(621) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_46_triplet_pt2(c,a,p,k)
term(622) = term(622) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_45_triplet_pt2(c,a,p,j)
term(623) = term(623) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_46_triplet_pt2(c,a,p,j)
term(624) = term(624) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_45_triplet_pt2(c,a,p,j)
term(625) = term(625) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_46_triplet_pt2(c,a,p,j)
term(626) = term(626) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_47_triplet_pt2(c,a,p,k)
term(627) = term(627) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_48_triplet_pt2(c,a,p,k)
term(628) = term(628) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_47_triplet_pt2(c,a,p,i)
term(629) = term(629) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,j) * wm_interm_48_triplet_pt2(c,a,p,i)
term(630) = term(630) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_47_triplet_pt2(c,b,p,i)
term(631) = term(631) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,j) * wm_interm_48_triplet_pt2(c,b,p,i)
term(632) = term(632) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_48_triplet_pt2(c,b,p,k)
term(633) = term(633) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_47_triplet_pt2(c,b,p,k)
term(634) = term(634) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_48_triplet_pt2(c,b,p,j)
term(635) = term(635) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_47_triplet_pt2(c,b,p,j)
term(636) = term(636) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,a,k) * wm_interm_47_triplet_pt2(c,b,p,j)
term(637) = term(637) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_48_triplet_pt2(c,a,p,j)
term(638) = term(638) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_48_triplet_pt2(c,a,p,j)
term(639) = term(639) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,a,i) * wm_interm_48_triplet_pt2(c,b,p,j)
term(640) = term(640) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,k,b,i) * wm_interm_47_triplet_pt2(c,a,p,j)
term(641) = term(641) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, c,i,b,k) * wm_interm_47_triplet_pt2(c,a,p,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(610) = term(610) * (2.0d+0) 
term(611) = term(611) * (-4.0d+0) 
term(612) = term(612) * (1.5d+0) 
term(613) = term(613) * (-3.0d+0) 
term(614) = term(614) * (-2.0d+0) 
term(615) = term(615) * (4.0d+0) 
term(616) = term(616) * (1.5d+0) 
term(617) = term(617) * (-3.0d+0) 
term(618) = term(618) * (-1.0d+0) 
term(619) = term(619) * (2.0d+0) 
term(620) = term(620) * (-1.0d+0) 
term(621) = term(621) * (2.0d+0) 
term(622) = term(622) * (-2.0d+0) 
term(623) = term(623) * (4.0d+0) 
term(624) = term(624) * (2.0d+0) 
term(625) = term(625) * (-4.0d+0) 
term(626) = term(626) * (-1.0d+0) 
term(627) = term(627) * (1.5d+0) 
term(628) = term(628) * (1.5d+0) 
term(629) = term(629) * (-2.0d+0) 
term(630) = term(630) * (-2.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (-2.0d+0) 
term(633) = term(633) * (2.0d+0) 
term(634) = term(634) * (1.5d+0) 
term(635) = term(635) * (1.5d+0) 
term(636) = term(636) * (-1.0d+0) 
term(637) = term(637) * (1.5d+0) 
term(638) = term(638) * (-2.0d+0) 
term(639) = term(639) * (-1.0d+0) 
term(640) = term(640) * (-2.0d+0) 
term(641) = term(641) * (2.0d+0) 


    calc_D_ov_wm_triplet_cc3_pt2 = zero
    do s = 0, 641
    calc_D_ov_wm_triplet_cc3_pt2 = calc_D_ov_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt2
    
    function calc_D_vo_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt2
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
    real(F64), dimension(0:1921) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_6_triplet_pt2(b,c,i,k)
term(1) = term(1) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_6_triplet_pt2(a,c,i,k)
term(2) = term(2) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_6_triplet_pt2(b,c,i,k)
term(3) = term(3) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_6_triplet_pt2(a,c,i,k)
term(4) = term(4) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_6_triplet_pt2(a,c,j,k)
term(5) = term(5) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_6_triplet_pt2(b,c,j,k)
term(6) = term(6) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_6_triplet_pt2(a,c,j,k)
term(7) = term(7) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_6_triplet_pt2(b,c,j,k)
term(8) = term(8) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_8_triplet_pt2(b,c,i,k)
term(9) = term(9) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_8_triplet_pt2(a,c,i,k)
term(10) = term(10) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_8_triplet_pt2(b,c,i,k)
term(11) = term(11) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_8_triplet_pt2(a,c,i,k)
term(12) = term(12) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_8_triplet_pt2(a,c,j,k)
term(13) = term(13) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_8_triplet_pt2(b,c,j,k)
term(14) = term(14) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_8_triplet_pt2(a,c,j,k)
term(15) = term(15) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_8_triplet_pt2(b,c,j,k)
term(16) = term(16) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_12_triplet_pt2(b,c,i,k)
term(17) = term(17) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_12_triplet_pt2(a,c,i,k)
term(18) = term(18) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_12_triplet_pt2(b,c,i,k)
term(19) = term(19) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_12_triplet_pt2(a,c,i,k)
term(20) = term(20) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_12_triplet_pt2(a,c,j,k)
term(21) = term(21) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_12_triplet_pt2(b,c,j,k)
term(22) = term(22) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_12_triplet_pt2(a,c,j,k)
term(23) = term(23) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_12_triplet_pt2(b,c,j,k)
term(24) = term(24) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_13_triplet_pt2(b,c,i,k)
term(25) = term(25) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_13_triplet_pt2(a,c,i,k)
term(26) = term(26) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_13_triplet_pt2(a,c,j,k)
term(27) = term(27) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_13_triplet_pt2(b,c,j,k)
term(28) = term(28) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_13_triplet_pt2(b,c,j,k)
term(29) = term(29) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_13_triplet_pt2(a,c,j,k)
term(30) = term(30) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_13_triplet_pt2(a,c,i,k)
term(31) = term(31) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_13_triplet_pt2(b,c,i,k)
term(32) = term(32) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_17_triplet_pt2(b,c,i,k)
term(33) = term(33) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_17_triplet_pt2(a,c,i,k)
term(34) = term(34) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_17_triplet_pt2(b,c,i,k)
term(35) = term(35) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_17_triplet_pt2(a,c,i,k)
term(36) = term(36) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_17_triplet_pt2(a,c,j,k)
term(37) = term(37) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_17_triplet_pt2(b,c,j,k)
term(38) = term(38) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_17_triplet_pt2(a,c,j,k)
term(39) = term(39) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_17_triplet_pt2(b,c,j,k)
term(40) = term(40) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_29_triplet_pt2(b,c,i,k)
term(41) = term(41) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_29_triplet_pt2(a,c,i,k)
term(42) = term(42) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_29_triplet_pt2(a,c,i,k)
term(43) = term(43) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_29_triplet_pt2(b,c,i,k)
term(44) = term(44) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_29_triplet_pt2(a,c,j,k)
term(45) = term(45) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_29_triplet_pt2(b,c,j,k)
term(46) = term(46) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_29_triplet_pt2(b,c,j,k)
term(47) = term(47) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_29_triplet_pt2(a,c,j,k)
term(48) = term(48) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_33_triplet_pt2(b,c,i,k)
term(49) = term(49) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_33_triplet_pt2(a,c,i,k)
term(50) = term(50) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_33_triplet_pt2(a,c,j,k)
term(51) = term(51) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_33_triplet_pt2(b,c,j,k)
term(52) = term(52) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,a,i,q)
term(53) = term(53) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,b,i,q)
term(54) = term(54) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(55) = term(55) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_33_triplet_pt2(a,c,k,q)
term(56) = term(56) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_13_triplet_pt2(a,c,k,q)
term(57) = term(57) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,a,i,q)
term(58) = term(58) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_17_triplet_pt2(a,c,k,q)
term(59) = term(59) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,b,i,q)
term(60) = term(60) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_29_triplet_pt2(a,c,k,q)
term(61) = term(61) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_33_triplet_pt2(a,c,i,q)
term(62) = term(62) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_29_triplet_pt2(a,c,i,q)
term(63) = term(63) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_17_triplet_pt2(a,c,i,q)
term(64) = term(64) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(65) = term(65) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(66) = term(66) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_13_triplet_pt2(a,c,i,q)
term(67) = term(67) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(68) = term(68) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,a,i,q)
term(69) = term(69) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,b,i,q)
term(70) = term(70) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(71) = term(71) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(72) = term(72) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(73) = term(73) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,a,i,q)
term(74) = term(74) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,b,i,q)
term(75) = term(75) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(76) = term(76) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_12_triplet_pt2(a,c,k,q)
term(77) = term(77) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_8_triplet_pt2(a,c,k,q)
term(78) = term(78) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_6_triplet_pt2(a,c,k,q)
term(79) = term(79) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_12_triplet_pt2(a,c,i,q)
term(80) = term(80) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_8_triplet_pt2(a,c,i,q)
term(81) = term(81) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_6_triplet_pt2(a,c,i,q)
term(82) = term(82) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_52_triplet_pt2(b,c,i,k)
term(83) = term(83) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_52_triplet_pt2(a,c,i,k)
term(84) = term(84) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_52_triplet_pt2(b,c,i,k)
term(85) = term(85) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_52_triplet_pt2(a,c,i,k)
term(86) = term(86) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_52_triplet_pt2(a,c,j,k)
term(87) = term(87) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_52_triplet_pt2(b,c,j,k)
term(88) = term(88) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_52_triplet_pt2(a,c,j,k)
term(89) = term(89) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_52_triplet_pt2(b,c,j,k)
term(90) = term(90) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_53_triplet_pt2(b,c,i,k)
term(91) = term(91) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_53_triplet_pt2(a,c,i,k)
term(92) = term(92) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_53_triplet_pt2(b,c,i,k)
term(93) = term(93) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_53_triplet_pt2(a,c,i,k)
term(94) = term(94) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_53_triplet_pt2(a,c,j,k)
term(95) = term(95) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_53_triplet_pt2(b,c,j,k)
term(96) = term(96) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_53_triplet_pt2(a,c,j,k)
term(97) = term(97) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_53_triplet_pt2(b,c,j,k)
term(98) = term(98) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_55_triplet_pt2(b,c,i,k)
term(99) = term(99) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_55_triplet_pt2(a,c,i,k)
term(100) = term(100) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_55_triplet_pt2(a,c,j,k)
term(101) = term(101) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_55_triplet_pt2(b,c,j,k)
term(102) = term(102) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_55_triplet_pt2(b,c,j,k)
term(103) = term(103) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_55_triplet_pt2(a,c,j,k)
term(104) = term(104) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_55_triplet_pt2(a,c,i,k)
term(105) = term(105) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_55_triplet_pt2(b,c,i,k)
term(106) = term(106) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_58_triplet_pt2(b,c,i,k)
term(107) = term(107) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_58_triplet_pt2(a,c,i,k)
term(108) = term(108) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_58_triplet_pt2(b,c,i,k)
term(109) = term(109) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_58_triplet_pt2(a,c,i,k)
term(110) = term(110) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_58_triplet_pt2(a,c,j,k)
term(111) = term(111) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_58_triplet_pt2(b,c,j,k)
term(112) = term(112) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_58_triplet_pt2(a,c,j,k)
term(113) = term(113) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_58_triplet_pt2(b,c,j,k)
term(114) = term(114) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,a,i,q)
term(115) = term(115) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,b,i,q)
term(116) = term(116) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(117) = term(117) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_58_triplet_pt2(a,c,k,q)
term(118) = term(118) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_55_triplet_pt2(a,c,k,q)
term(119) = term(119) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,a,i,q)
term(120) = term(120) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,b,i,q)
term(121) = term(121) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_58_triplet_pt2(a,c,i,q)
term(122) = term(122) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_55_triplet_pt2(a,c,i,q)
term(123) = term(123) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(124) = term(124) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(125) = term(125) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(126) = term(126) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,a,i,q)
term(127) = term(127) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,b,i,q)
term(128) = term(128) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(129) = term(129) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(130) = term(130) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(131) = term(131) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,a,i,q)
term(132) = term(132) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,b,i,q)
term(133) = term(133) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(134) = term(134) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_53_triplet_pt2(a,c,k,q)
term(135) = term(135) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_52_triplet_pt2(a,c,k,q)
term(136) = term(136) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_53_triplet_pt2(a,c,i,q)
term(137) = term(137) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_52_triplet_pt2(a,c,i,q)
term(138) = term(138) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_84_triplet_pt2(a,c,i,k)
term(139) = term(139) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_85_triplet_pt2(a,c,i,k)
term(140) = term(140) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_86_triplet_pt2(a,c,i,k)
term(141) = term(141) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_87_triplet_pt2(a,c,i,k)
term(142) = term(142) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_88_triplet_pt2(a,c,i,k)
term(143) = term(143) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_89_triplet_pt2(a,c,i,k)
term(144) = term(144) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_84_triplet_pt2(b,c,i,k)
term(145) = term(145) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_85_triplet_pt2(b,c,i,k)
term(146) = term(146) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_86_triplet_pt2(b,c,i,k)
term(147) = term(147) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_87_triplet_pt2(b,c,i,k)
term(148) = term(148) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_88_triplet_pt2(b,c,i,k)
term(149) = term(149) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_89_triplet_pt2(b,c,i,k)
term(150) = term(150) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_84_triplet_pt2(a,c,j,k)
term(151) = term(151) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_85_triplet_pt2(a,c,j,k)
term(152) = term(152) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_86_triplet_pt2(a,c,j,k)
term(153) = term(153) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_87_triplet_pt2(a,c,j,k)
term(154) = term(154) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_88_triplet_pt2(a,c,j,k)
term(155) = term(155) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_89_triplet_pt2(a,c,j,k)
term(156) = term(156) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_84_triplet_pt2(b,c,j,k)
term(157) = term(157) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_85_triplet_pt2(b,c,j,k)
term(158) = term(158) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_86_triplet_pt2(b,c,j,k)
term(159) = term(159) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_87_triplet_pt2(b,c,j,k)
term(160) = term(160) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_88_triplet_pt2(b,c,j,k)
term(161) = term(161) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_89_triplet_pt2(b,c,j,k)
term(162) = term(162) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_86_triplet_pt2(b,c,i,k)
term(163) = term(163) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_87_triplet_pt2(b,c,i,k)
term(164) = term(164) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_89_triplet_pt2(b,c,i,k)
term(165) = term(165) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_85_triplet_pt2(b,c,i,k)
term(166) = term(166) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_93_triplet_pt2(b,c,i,k)
term(167) = term(167) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_88_triplet_pt2(b,c,i,k)
term(168) = term(168) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_86_triplet_pt2(a,c,j,k)
term(169) = term(169) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_87_triplet_pt2(a,c,j,k)
term(170) = term(170) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_85_triplet_pt2(a,c,j,k)
term(171) = term(171) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_84_triplet_pt2(a,c,j,k)
term(172) = term(172) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_89_triplet_pt2(a,c,j,k)
term(173) = term(173) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_93_triplet_pt2(a,c,j,k)
term(174) = term(174) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_86_triplet_pt2(b,c,j,k)
term(175) = term(175) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_87_triplet_pt2(b,c,j,k)
term(176) = term(176) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_84_triplet_pt2(b,c,j,k)
term(177) = term(177) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_85_triplet_pt2(b,c,j,k)
term(178) = term(178) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_89_triplet_pt2(b,c,j,k)
term(179) = term(179) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_88_triplet_pt2(b,c,j,k)
term(180) = term(180) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_84_triplet_pt2(a,c,i,k)
term(181) = term(181) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_85_triplet_pt2(a,c,i,k)
term(182) = term(182) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_86_triplet_pt2(a,c,i,k)
term(183) = term(183) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_87_triplet_pt2(a,c,i,k)
term(184) = term(184) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_88_triplet_pt2(a,c,i,k)
term(185) = term(185) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_89_triplet_pt2(a,c,i,k)
term(186) = term(186) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_88_triplet_pt2(a,c,j,k)
term(187) = term(187) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_93_triplet_pt2(b,c,j,k)
term(188) = term(188) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_84_triplet_pt2(b,c,i,k)
term(189) = term(189) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_93_triplet_pt2(a,c,i,k)
term(190) = term(190) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_126_triplet_pt2(a,c,i,k)
term(191) = term(191) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_127_triplet_pt2(a,c,i,k)
term(192) = term(192) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_128_triplet_pt2(a,c,i,k)
term(193) = term(193) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_129_triplet_pt2(a,c,i,k)
term(194) = term(194) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_126_triplet_pt2(b,c,i,k)
term(195) = term(195) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_127_triplet_pt2(b,c,i,k)
term(196) = term(196) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_128_triplet_pt2(b,c,i,k)
term(197) = term(197) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_129_triplet_pt2(b,c,i,k)
term(198) = term(198) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_126_triplet_pt2(a,c,j,k)
term(199) = term(199) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_127_triplet_pt2(a,c,j,k)
term(200) = term(200) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_128_triplet_pt2(a,c,j,k)
term(201) = term(201) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_129_triplet_pt2(a,c,j,k)
term(202) = term(202) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_126_triplet_pt2(b,c,j,k)
term(203) = term(203) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_127_triplet_pt2(b,c,j,k)
term(204) = term(204) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_128_triplet_pt2(b,c,j,k)
term(205) = term(205) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_129_triplet_pt2(b,c,j,k)
term(206) = term(206) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_128_triplet_pt2(b,c,i,k)
term(207) = term(207) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_129_triplet_pt2(b,c,i,k)
term(208) = term(208) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_127_triplet_pt2(b,c,i,k)
term(209) = term(209) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_126_triplet_pt2(b,c,i,k)
term(210) = term(210) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_128_triplet_pt2(a,c,j,k)
term(211) = term(211) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_129_triplet_pt2(a,c,j,k)
term(212) = term(212) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_127_triplet_pt2(a,c,j,k)
term(213) = term(213) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_126_triplet_pt2(a,c,j,k)
term(214) = term(214) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_128_triplet_pt2(b,c,j,k)
term(215) = term(215) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_129_triplet_pt2(b,c,j,k)
term(216) = term(216) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_126_triplet_pt2(b,c,j,k)
term(217) = term(217) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_127_triplet_pt2(b,c,j,k)
term(218) = term(218) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_126_triplet_pt2(a,c,i,k)
term(219) = term(219) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_127_triplet_pt2(a,c,i,k)
term(220) = term(220) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_128_triplet_pt2(a,c,i,k)
term(221) = term(221) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_129_triplet_pt2(a,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (12.0d+0) 
term(3) = term(3) * (-16.0d+0) 
term(4) = term(4) * (-6.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (12.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (3.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-6.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (3.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-6.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (3.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (-6.0d+0) 
term(19) = term(19) * (8.0d+0) 
term(20) = term(20) * (3.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-6.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (3.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (3.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (3.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (3.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (3.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (-6.0d+0) 
term(35) = term(35) * (8.0d+0) 
term(36) = term(36) * (3.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (-6.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (3.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (3.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (3.0d+0) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (3.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (3.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * (3.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (3.0d+0) 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * (3.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (3.0d+0) 
term(59) = term(59) * (-6.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (8.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (3.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (3.0d+0) 
term(73) = term(73) * (4.0d+0) 
term(74) = term(74) * (-4.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (3.0d+0) 
term(77) = term(77) * (3.0d+0) 
term(78) = term(78) * (-6.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (8.0d+0) 
term(82) = term(82) * (-12.0d+0) 
term(83) = term(83) * (16.0d+0) 
term(84) = term(84) * (24.0d+0) 
term(85) = term(85) * (-32.0d+0) 
term(86) = term(86) * (-12.0d+0) 
term(87) = term(87) * (8.0d+0) 
term(88) = term(88) * (24.0d+0) 
term(89) = term(89) * (-16.0d+0) 
term(90) = term(90) * (12.0d+0) 
term(91) = term(91) * (-16.0d+0) 
term(92) = term(92) * (-24.0d+0) 
term(93) = term(93) * (32.0d+0) 
term(94) = term(94) * (12.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (-24.0d+0) 
term(97) = term(97) * (16.0d+0) 
term(98) = term(98) * (12.0d+0) 
term(99) = term(99) * (-16.0d+0) 
term(100) = term(100) * (12.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (12.0d+0) 
term(103) = term(103) * (-12.0d+0) 
term(104) = term(104) * (12.0d+0) 
term(105) = term(105) * (-12.0d+0) 
term(106) = term(106) * (12.0d+0) 
term(107) = term(107) * (-12.0d+0) 
term(108) = term(108) * (-12.0d+0) 
term(109) = term(109) * (16.0d+0) 
term(110) = term(110) * (12.0d+0) 
term(111) = term(111) * (-12.0d+0) 
term(112) = term(112) * (-12.0d+0) 
term(113) = term(113) * (8.0d+0) 
term(114) = term(114) * (-8.0d+0) 
term(115) = term(115) * (6.0d+0) 
term(116) = term(116) * (-8.0d+0) 
term(117) = term(117) * (12.0d+0) 
term(118) = term(118) * (-12.0d+0) 
term(119) = term(119) * (16.0d+0) 
term(120) = term(120) * (-12.0d+0) 
term(121) = term(121) * (-16.0d+0) 
term(122) = term(122) * (16.0d+0) 
term(123) = term(123) * (16.0d+0) 
term(124) = term(124) * (8.0d+0) 
term(125) = term(125) * (-16.0d+0) 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (6.0d+0) 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (-8.0d+0) 
term(130) = term(130) * (6.0d+0) 
term(131) = term(131) * (8.0d+0) 
term(132) = term(132) * (-8.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (12.0d+0) 
term(135) = term(135) * (-12.0d+0) 
term(136) = term(136) * (-16.0d+0) 
term(137) = term(137) * (16.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (8.0d+0) 
term(141) = term(141) * (-16.0d+0) 
term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * (8.0d+0) 
term(144) = term(144) * (3.0d+0) 
term(145) = term(145) * (-6.0d+0) 
term(146) = term(146) * (-6.0d+0) 
term(147) = term(147) * (12.0d+0) 
term(148) = term(148) * (3.0d+0) 
term(149) = term(149) * (-6.0d+0) 
term(150) = term(150) * (3.0d+0) 
term(151) = term(151) * (-6.0d+0) 
term(152) = term(152) * (-6.0d+0) 
term(153) = term(153) * (12.0d+0) 
term(154) = term(154) * (3.0d+0) 
term(155) = term(155) * (-6.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-8.0d+0) 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (3.0d+0) 
term(163) = term(163) * (-6.0d+0) 
term(164) = term(164) * (3.0d+0) 
term(165) = term(165) * (3.0d+0) 
term(166) = term(166) * (3.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (3.0d+0) 
term(169) = term(169) * (-6.0d+0) 
term(170) = term(170) * (3.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * (3.0d+0) 
term(173) = term(173) * (3.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (3.0d+0) 
term(177) = term(177) * (-2.0d+0) 
term(178) = term(178) * (-2.0d+0) 
term(179) = term(179) * (3.0d+0) 
term(180) = term(180) * (3.0d+0) 
term(181) = term(181) * (-4.0d+0) 
term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * (8.0d+0) 
term(184) = term(184) * (3.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (-2.0d+0) 
term(187) = term(187) * (-4.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (-2.0d+0) 
term(190) = term(190) * (-16.0d+0) 
term(191) = term(191) * (16.0d+0) 
term(192) = term(192) * (32.0d+0) 
term(193) = term(193) * (-32.0d+0) 
term(194) = term(194) * (12.0d+0) 
term(195) = term(195) * (-12.0d+0) 
term(196) = term(196) * (-24.0d+0) 
term(197) = term(197) * (24.0d+0) 
term(198) = term(198) * (12.0d+0) 
term(199) = term(199) * (-12.0d+0) 
term(200) = term(200) * (-24.0d+0) 
term(201) = term(201) * (24.0d+0) 
term(202) = term(202) * (-8.0d+0) 
term(203) = term(203) * (8.0d+0) 
term(204) = term(204) * (16.0d+0) 
term(205) = term(205) * (-16.0d+0) 
term(206) = term(206) * (12.0d+0) 
term(207) = term(207) * (-12.0d+0) 
term(208) = term(208) * (12.0d+0) 
term(209) = term(209) * (-12.0d+0) 
term(210) = term(210) * (12.0d+0) 
term(211) = term(211) * (-12.0d+0) 
term(212) = term(212) * (12.0d+0) 
term(213) = term(213) * (-12.0d+0) 
term(214) = term(214) * (-8.0d+0) 
term(215) = term(215) * (8.0d+0) 
term(216) = term(216) * (12.0d+0) 
term(217) = term(217) * (-12.0d+0) 
term(218) = term(218) * (12.0d+0) 
term(219) = term(219) * (-12.0d+0) 
term(220) = term(220) * (-16.0d+0) 
term(221) = term(221) * (16.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(222) = term(222) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,p,q) * s1(b,j)
end do 
end do 
end do 
end do 

term(222) = term(222) * (-8.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(223) = term(223) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,q,j,l,k)
term(224) = term(224) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,i,j,l,k)
term(225) = term(225) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,j,q,l,k)
term(226) = term(226) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,i,q,l,k)
term(227) = term(227) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_109_triplet_pt2(b,q,i,j,l,k)
term(228) = term(228) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_109_triplet_pt2(b,j,i,q,l,k)
term(229) = term(229) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,i,j,l,k)
term(230) = term(230) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,q,i,j,l,k)
term(231) = term(231) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,j,i,q,l,k)
term(232) = term(232) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,i,q,j,l,k)
term(233) = term(233) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,i,j,q,l,k)
term(234) = term(234) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,j,i,q,l,k)
term(235) = term(235) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,i,j,q,l,k)
term(236) = term(236) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,j,i,q,l,k)
term(237) = term(237) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,j,i,q,l,k)
term(238) = term(238) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,q,i,j,l,k)
term(239) = term(239) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,q,i,j,l,k)
term(240) = term(240) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,q,j,l,k)
term(241) = term(241) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,i,j,l,k)
term(242) = term(242) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,j,q,l,k)
term(243) = term(243) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,i,q,l,k)
term(244) = term(244) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,i,j,l,k)
term(245) = term(245) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,j,i,q,l,k)
term(246) = term(246) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,i,q,j,l,k)
term(247) = term(247) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,i,j,q,l,k)
term(248) = term(248) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,i,j,q,l,k)
term(249) = term(249) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,j,i,q,l,k)
term(250) = term(250) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,q,i,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(223) = term(223) * (0.5d+0) 
term(224) = term(224) * (-1.0d+0) 
term(225) = term(225) * (-1.0d+0) 
term(226) = term(226) * (2.0d+0) 
term(227) = term(227) * (0.5d+0) 
term(228) = term(228) * (-1.0d+0) 
term(229) = term(229) * (0.5d+0) 
term(230) = term(230) * (-1.0d+0) 
term(231) = term(231) * (-1.0d+0) 
term(232) = term(232) * (0.5d+0) 
term(233) = term(233) * (-1.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (0.5d+0) 
term(236) = term(236) * (-1.0d+0) 
term(237) = term(237) * (0.5d+0) 
term(238) = term(238) * (0.5d+0) 
term(239) = term(239) * (-1.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (-2.0d+0) 
term(242) = term(242) * (-4.0d+0) 
term(243) = term(243) * (4.0d+0) 
term(244) = term(244) * (2.0d+0) 
term(245) = term(245) * (-4.0d+0) 
term(246) = term(246) * (-2.0d+0) 
term(247) = term(247) * (4.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (-2.0d+0) 
term(250) = term(250) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(251) = term(251) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_6_triplet_pt2(a,c,j,k)
term(252) = term(252) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_8_triplet_pt2(a,c,j,k)
term(253) = term(253) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_12_triplet_pt2(a,c,j,k)
term(254) = term(254) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_13_triplet_pt2(a,c,j,k)
term(255) = term(255) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_17_triplet_pt2(a,c,j,k)
term(256) = term(256) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_29_triplet_pt2(a,c,j,k)
term(257) = term(257) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_33_triplet_pt2(a,c,j,k)
term(258) = term(258) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_52_triplet_pt2(a,c,j,k)
term(259) = term(259) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_53_triplet_pt2(a,c,j,k)
term(260) = term(260) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_55_triplet_pt2(a,c,j,k)
term(261) = term(261) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_58_triplet_pt2(a,c,j,k)
term(262) = term(262) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_86_triplet_pt2(a,c,j,k)
term(263) = term(263) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_87_triplet_pt2(a,c,j,k)
term(264) = term(264) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_85_triplet_pt2(a,c,j,k)
term(265) = term(265) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_84_triplet_pt2(a,c,j,k)
term(266) = term(266) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_93_triplet_pt2(a,c,j,k)
term(267) = term(267) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_89_triplet_pt2(a,c,j,k)
term(268) = term(268) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_84_triplet_pt2(b,c,j,k)
term(269) = term(269) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_88_triplet_pt2(b,c,j,k)
term(270) = term(270) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_93_triplet_pt2(b,c,j,k)
term(271) = term(271) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_88_triplet_pt2(a,c,j,k)
term(272) = term(272) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_85_triplet_pt2(b,c,j,k)
term(273) = term(273) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_86_triplet_pt2(b,c,j,k)
term(274) = term(274) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_87_triplet_pt2(b,c,j,k)
term(275) = term(275) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_89_triplet_pt2(b,c,j,k)
term(276) = term(276) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_128_triplet_pt2(a,c,j,k)
term(277) = term(277) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_129_triplet_pt2(a,c,j,k)
term(278) = term(278) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_127_triplet_pt2(a,c,j,k)
term(279) = term(279) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_126_triplet_pt2(a,c,j,k)
term(280) = term(280) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_126_triplet_pt2(b,c,j,k)
term(281) = term(281) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_127_triplet_pt2(b,c,j,k)
term(282) = term(282) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_128_triplet_pt2(b,c,j,k)
term(283) = term(283) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_129_triplet_pt2(b,c,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(251) = term(251) * (8.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (-4.0d+0) 
term(254) = term(254) * (4.0d+0) 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (4.0d+0) 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * (16.0d+0) 
term(259) = term(259) * (-16.0d+0) 
term(260) = term(260) * (16.0d+0) 
term(261) = term(261) * (-16.0d+0) 
term(262) = term(262) * (-2.0d+0) 
term(263) = term(263) * (4.0d+0) 
term(264) = term(264) * (-2.0d+0) 
term(265) = term(265) * (2.0d+0) 
term(266) = term(266) * (-2.0d+0) 
term(267) = term(267) * (-2.0d+0) 
term(268) = term(268) * (-2.0d+0) 
term(269) = term(269) * (-2.0d+0) 
term(270) = term(270) * (2.0d+0) 
term(271) = term(271) * (2.0d+0) 
term(272) = term(272) * (2.0d+0) 
term(273) = term(273) * (2.0d+0) 
term(274) = term(274) * (-4.0d+0) 
term(275) = term(275) * (2.0d+0) 
term(276) = term(276) * (-8.0d+0) 
term(277) = term(277) * (8.0d+0) 
term(278) = term(278) * (-8.0d+0) 
term(279) = term(279) * (8.0d+0) 
term(280) = term(280) * (-8.0d+0) 
term(281) = term(281) * (8.0d+0) 
term(282) = term(282) * (8.0d+0) 
term(283) = term(283) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(284) = term(284) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,q,i,k,l)
term(285) = term(285) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,j,i,k,l)
term(286) = term(286) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,j,i,k,l)
term(287) = term(287) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,j,q,i,k,l)
term(288) = term(288) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,q,j,i,k,l)
term(289) = term(289) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,q,i,k,l)
term(290) = term(290) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,j,i,k,l)
term(291) = term(291) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,j,i,k,l)
term(292) = term(292) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,j,q,i,k,l)
term(293) = term(293) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,q,j,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(284) = term(284) * (0.5d+0) 
term(285) = term(285) * (-1.0d+0) 
term(286) = term(286) * (0.5d+0) 
term(287) = term(287) * (0.5d+0) 
term(288) = term(288) * (-1.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (-4.0d+0) 
term(291) = term(291) * (2.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(294) = term(294) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,q,j,k,l)
term(295) = term(295) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_109_triplet_pt2(b,i,q,j,k,l)
term(296) = term(296) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,j,q,k,l)
term(297) = term(297) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,i,q,k,l)
term(298) = term(298) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,i,j,k,l)
term(299) = term(299) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_109_triplet_pt2(b,j,i,q,k,l)
term(300) = term(300) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_109_triplet_pt2(b,q,i,j,k,l)
term(301) = term(301) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,i,j,k,l)
term(302) = term(302) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,q,i,j,k,l)
term(303) = term(303) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,j,i,q,k,l)
term(304) = term(304) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,i,j,q,k,l)
term(305) = term(305) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,j,i,q,k,l)
term(306) = term(306) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,i,q,j,k,l)
term(307) = term(307) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,i,q,j,k,l)
term(308) = term(308) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,i,j,q,k,l)
term(309) = term(309) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,j,i,q,k,l)
term(310) = term(310) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,q,i,j,k,l)
term(311) = term(311) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,j,i,q,k,l)
term(312) = term(312) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,q,i,j,k,l)
term(313) = term(313) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,q,j,k,l)
term(314) = term(314) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,i,j,q,k,l)
term(315) = term(315) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,i,q,k,l)
term(316) = term(316) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,i,j,k,l)
term(317) = term(317) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,i,j,k,l)
term(318) = term(318) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,j,i,q,k,l)
term(319) = term(319) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,i,j,q,k,l)
term(320) = term(320) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,i,q,j,k,l)
term(321) = term(321) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,j,i,q,k,l)
term(322) = term(322) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,i,j,q,k,l)
term(323) = term(323) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,q,i,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(294) = term(294) * (-1.0d+0) 
term(295) = term(295) * (0.5d+0) 
term(296) = term(296) * (0.5d+0) 
term(297) = term(297) * (-1.0d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (0.5d+0) 
term(300) = term(300) * (-1.0d+0) 
term(301) = term(301) * (-1.0d+0) 
term(302) = term(302) * (0.5d+0) 
term(303) = term(303) * (0.5d+0) 
term(304) = term(304) * (0.5d+0) 
term(305) = term(305) * (-1.0d+0) 
term(306) = term(306) * (-1.0d+0) 
term(307) = term(307) * (0.5d+0) 
term(308) = term(308) * (0.5d+0) 
term(309) = term(309) * (-1.0d+0) 
term(310) = term(310) * (2.0d+0) 
term(311) = term(311) * (0.5d+0) 
term(312) = term(312) * (-1.0d+0) 
term(313) = term(313) * (-2.0d+0) 
term(314) = term(314) * (2.0d+0) 
term(315) = term(315) * (-2.0d+0) 
term(316) = term(316) * (4.0d+0) 
term(317) = term(317) * (-2.0d+0) 
term(318) = term(318) * (2.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(320) = term(320) * (2.0d+0) 
term(321) = term(321) * (2.0d+0) 
term(322) = term(322) * (-2.0d+0) 
term(323) = term(323) * (-4.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(324) = term(324) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_5_triplet_pt2(c,a)
term(325) = term(325) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_7_triplet_pt2(c,a)
term(326) = term(326) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_5_triplet_pt2(c,a)
term(327) = term(327) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_7_triplet_pt2(c,a)
term(328) = term(328) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_5_triplet_pt2(c,a)
term(329) = term(329) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_7_triplet_pt2(c,a)
term(330) = term(330) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_5_triplet_pt2(c,a)
term(331) = term(331) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_7_triplet_pt2(c,a)
end do 
end do 
end do 
end do 
end do 

term(324) = term(324) * (-4.0d+0) 
term(325) = term(325) * (8.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (16.0d+0) 
term(328) = term(328) * (-2.0d+0) 
term(329) = term(329) * (4.0d+0) 
term(330) = term(330) * (-8.0d+0) 
term(331) = term(331) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(332) = term(332) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,q,j,i)
term(333) = term(333) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,q,j,i)
term(334) = term(334) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,q,j,i)
term(335) = term(335) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,q,j,i)
term(336) = term(336) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,q,j,i)
term(337) = term(337) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,q,j,i)
term(338) = term(338) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,q,j,i)
term(339) = term(339) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,q,j,i)
term(340) = term(340) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,q,j,i)
term(341) = term(341) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,q,j,i)
term(342) = term(342) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,q,j,i)
term(343) = term(343) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,q,j,i)
term(344) = term(344) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,q,j,i)
term(345) = term(345) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,q,j,i)
term(346) = term(346) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,j,q,i)
term(347) = term(347) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,j,q,i)
term(348) = term(348) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,j,q,i)
term(349) = term(349) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,j,q,i)
term(350) = term(350) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,j,q,i)
term(351) = term(351) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,j,q,i)
term(352) = term(352) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,q,j,i)
term(353) = term(353) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,q,j,i)
term(354) = term(354) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,q,j,i)
term(355) = term(355) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,q,j,i)
term(356) = term(356) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,q,j,i)
term(357) = term(357) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,q,j,i)
term(358) = term(358) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,j,q,i)
term(359) = term(359) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,j,q,i)
term(360) = term(360) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,j,q,i)
term(361) = term(361) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,j,q,i)
term(362) = term(362) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,j,q,i)
term(363) = term(363) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,j,q,i)
term(364) = term(364) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,j,q,i)
term(365) = term(365) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,j,q,i)
term(366) = term(366) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,j,q,i)
term(367) = term(367) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,j,q,i)
term(368) = term(368) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,q,j,i)
term(369) = term(369) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,q,j,i)
term(370) = term(370) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,j,q,i)
term(371) = term(371) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,j,q,i)
term(372) = term(372) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,j,q,i)
term(373) = term(373) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,j,q,i)
term(374) = term(374) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,j,q,i)
term(375) = term(375) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,j,q,i)
term(376) = term(376) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,q,j,i)
term(377) = term(377) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,q,j,i)
term(378) = term(378) + wm_interm_45_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,j,q,i)
term(379) = term(379) + wm_interm_46_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,j,q,i)
term(380) = term(380) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,q,j,i)
term(381) = term(381) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,q,j,i)
term(382) = term(382) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,q,j,i)
term(383) = term(383) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,q,j,i)
term(384) = term(384) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,q,j,i)
term(385) = term(385) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,q,j,i)
term(386) = term(386) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,q,j,i)
term(387) = term(387) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,q,j,i)
term(388) = term(388) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,q,j,i)
term(389) = term(389) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,q,j,i)
term(390) = term(390) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,q,j,i)
term(391) = term(391) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,q,j,i)
term(392) = term(392) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,q,j,i)
term(393) = term(393) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,q,j,i)
term(394) = term(394) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,q,j,i)
term(395) = term(395) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,q,j,i)
term(396) = term(396) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,q,j,i)
term(397) = term(397) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,q,j,i)
term(398) = term(398) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,q,j,i)
term(399) = term(399) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,q,j,i)
term(400) = term(400) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,q,j,i)
term(401) = term(401) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,q,j,i)
term(402) = term(402) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(403) = term(403) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(404) = term(404) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(405) = term(405) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(406) = term(406) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(407) = term(407) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(408) = term(408) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(409) = term(409) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(410) = term(410) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(411) = term(411) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(412) = term(412) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(413) = term(413) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(414) = term(414) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(415) = term(415) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(416) = term(416) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(417) = term(417) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(418) = term(418) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(419) = term(419) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(420) = term(420) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(421) = term(421) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(422) = term(422) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(423) = term(423) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(424) = term(424) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(425) = term(425) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(426) = term(426) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(427) = term(427) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(428) = term(428) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(429) = term(429) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(430) = term(430) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(431) = term(431) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(432) = term(432) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(433) = term(433) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(434) = term(434) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(435) = term(435) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(436) = term(436) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(437) = term(437) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(438) = term(438) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,j,q,i)
term(439) = term(439) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,j,q,i)
term(440) = term(440) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,j,q,i)
term(441) = term(441) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_76_triplet_pt2(a,j,q,i)
term(442) = term(442) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,j,q,i)
term(443) = term(443) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,j,q,i)
term(444) = term(444) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(a,j,q,i)
term(445) = term(445) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,j,q,i)
term(446) = term(446) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,j,q,i)
term(447) = term(447) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,j,q,i)
term(448) = term(448) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_77_triplet_pt2(a,j,q,i)
term(449) = term(449) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_78_triplet_pt2(a,j,q,i)
term(450) = term(450) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(451) = term(451) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(452) = term(452) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(453) = term(453) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(454) = term(454) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(455) = term(455) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(456) = term(456) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_91_triplet_pt2(a,q,j,i)
term(457) = term(457) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(458) = term(458) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(459) = term(459) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(460) = term(460) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,j,q,i)
term(461) = term(461) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_82_triplet_pt2(a,j,q,i)
term(462) = term(462) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(463) = term(463) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(464) = term(464) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(465) = term(465) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(466) = term(466) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(467) = term(467) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(468) = term(468) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(469) = term(469) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(470) = term(470) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(471) = term(471) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(472) = term(472) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(473) = term(473) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(474) = term(474) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(475) = term(475) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(476) = term(476) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(a,j,q,i)
term(477) = term(477) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_67_triplet_pt2(a,j,q,i)
term(478) = term(478) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,q,j,i)
term(479) = term(479) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,j,q,i)
term(480) = term(480) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,j,q,i)
term(481) = term(481) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,j,q,i)
term(482) = term(482) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(483) = term(483) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(484) = term(484) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(485) = term(485) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(486) = term(486) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(487) = term(487) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(488) = term(488) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(489) = term(489) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(490) = term(490) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(491) = term(491) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(492) = term(492) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(493) = term(493) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(494) = term(494) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(495) = term(495) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(496) = term(496) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(497) = term(497) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(498) = term(498) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(499) = term(499) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(500) = term(500) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_93_triplet_pt2(a,p,i,j)
term(501) = term(501) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_88_triplet_pt2(a,p,i,j)
term(502) = term(502) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_75_triplet_pt2(a,j,q,i)
term(503) = term(503) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_90_triplet_pt2(a,j,q,i)
term(504) = term(504) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_72_triplet_pt2(a,j,q,i)
term(505) = term(505) + wm_interm_48_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,j,q,i)
term(506) = term(506) + wm_interm_47_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(a,j,q,i)
term(507) = term(507) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_86_triplet_pt2(a,p,i,j)
term(508) = term(508) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_87_triplet_pt2(a,p,i,j)
term(509) = term(509) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_89_triplet_pt2(a,p,i,j)
term(510) = term(510) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_84_triplet_pt2(a,p,i,j)
term(511) = term(511) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_85_triplet_pt2(a,p,i,j)
term(512) = term(512) + wm_interm_122_triplet_pt2(a,q,i,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(513) = term(513) + wm_interm_122_triplet_pt2(a,q,i,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(514) = term(514) + wm_interm_123_triplet_pt2(a,q,i,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(515) = term(515) + wm_interm_123_triplet_pt2(a,q,i,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(516) = term(516) + wm_interm_117_triplet_pt2(a,q,i,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(517) = term(517) + wm_interm_117_triplet_pt2(a,q,i,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(518) = term(518) + wm_interm_116_triplet_pt2(a,q,i,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(519) = term(519) + wm_interm_116_triplet_pt2(a,q,i,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(520) = term(520) + wm_interm_122_triplet_pt2(a,i,q,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(521) = term(521) + wm_interm_122_triplet_pt2(a,i,q,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(522) = term(522) + wm_interm_123_triplet_pt2(a,i,q,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(523) = term(523) + wm_interm_123_triplet_pt2(a,i,q,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(524) = term(524) + wm_interm_120_triplet_pt2(a,q,i,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(525) = term(525) + wm_interm_120_triplet_pt2(a,q,i,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(526) = term(526) + wm_interm_121_triplet_pt2(a,q,i,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(527) = term(527) + wm_interm_121_triplet_pt2(a,q,i,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(528) = term(528) + wm_interm_116_triplet_pt2(a,i,q,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(529) = term(529) + wm_interm_116_triplet_pt2(a,i,q,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(530) = term(530) + wm_interm_117_triplet_pt2(a,i,q,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(531) = term(531) + wm_interm_117_triplet_pt2(a,i,q,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(532) = term(532) + wm_interm_120_triplet_pt2(a,i,q,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(533) = term(533) + wm_interm_120_triplet_pt2(a,i,q,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(534) = term(534) + wm_interm_121_triplet_pt2(a,i,q,j) * wm_interm_45_triplet_pt2(p,a,j,i)
term(535) = term(535) + wm_interm_121_triplet_pt2(a,i,q,j) * wm_interm_46_triplet_pt2(p,a,j,i)
term(536) = term(536) + wm_interm_117_triplet_pt2(a,q,i,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(537) = term(537) + wm_interm_116_triplet_pt2(a,q,i,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(538) = term(538) + wm_interm_116_triplet_pt2(a,q,i,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(539) = term(539) + wm_interm_117_triplet_pt2(a,q,i,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(540) = term(540) + wm_interm_120_triplet_pt2(a,q,i,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(541) = term(541) + wm_interm_121_triplet_pt2(a,q,i,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(542) = term(542) + wm_interm_120_triplet_pt2(a,q,i,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(543) = term(543) + wm_interm_121_triplet_pt2(a,q,i,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(544) = term(544) + wm_interm_122_triplet_pt2(a,q,i,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(545) = term(545) + wm_interm_122_triplet_pt2(a,q,i,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(546) = term(546) + wm_interm_123_triplet_pt2(a,q,i,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(547) = term(547) + wm_interm_123_triplet_pt2(a,q,i,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(548) = term(548) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(549) = term(549) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(550) = term(550) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(551) = term(551) + wm_interm_103_triplet_pt2(a,q,i,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(552) = term(552) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(553) = term(553) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(554) = term(554) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(555) = term(555) + wm_interm_103_triplet_pt2(a,i,q,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(556) = term(556) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(557) = term(557) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(558) = term(558) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(559) = term(559) + wm_interm_108_triplet_pt2(a,i,q,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(560) = term(560) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(561) = term(561) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(562) = term(562) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(563) = term(563) + wm_interm_110_triplet_pt2(a,q,i,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(564) = term(564) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(565) = term(565) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(566) = term(566) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(567) = term(567) + wm_interm_108_triplet_pt2(a,q,i,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(568) = term(568) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(569) = term(569) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(570) = term(570) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(571) = term(571) + wm_interm_110_triplet_pt2(a,i,q,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(572) = term(572) + wm_interm_116_triplet_pt2(a,i,q,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(573) = term(573) + wm_interm_117_triplet_pt2(a,i,q,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(574) = term(574) + wm_interm_122_triplet_pt2(a,i,q,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(575) = term(575) + wm_interm_122_triplet_pt2(a,i,q,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(576) = term(576) + wm_interm_116_triplet_pt2(a,i,q,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(577) = term(577) + wm_interm_117_triplet_pt2(a,i,q,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(578) = term(578) + wm_interm_123_triplet_pt2(a,i,q,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(579) = term(579) + wm_interm_123_triplet_pt2(a,i,q,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(580) = term(580) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(581) = term(581) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(582) = term(582) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(583) = term(583) + wm_interm_111_triplet_pt2(a,q,i,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(584) = term(584) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(585) = term(585) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(586) = term(586) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(587) = term(587) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(588) = term(588) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(589) = term(589) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(590) = term(590) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(591) = term(591) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(592) = term(592) + wm_interm_113_triplet_pt2(a,q,i,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(593) = term(593) + wm_interm_113_triplet_pt2(a,i,q,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(594) = term(594) + wm_interm_120_triplet_pt2(a,i,q,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(595) = term(595) + wm_interm_121_triplet_pt2(a,i,q,j) * wm_interm_48_triplet_pt2(p,a,j,i)
term(596) = term(596) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(597) = term(597) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(598) = term(598) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(599) = term(599) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_129_triplet_pt2(a,p,i,j)
term(600) = term(600) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(601) = term(601) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(602) = term(602) + wm_interm_114_triplet_pt2(a,q,i,j) * wm_interm_127_triplet_pt2(a,p,i,j)
term(603) = term(603) + wm_interm_114_triplet_pt2(a,i,q,j) * wm_interm_126_triplet_pt2(a,p,i,j)
term(604) = term(604) + wm_interm_120_triplet_pt2(a,i,q,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(605) = term(605) + wm_interm_121_triplet_pt2(a,i,q,j) * wm_interm_47_triplet_pt2(p,a,j,i)
term(606) = term(606) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_128_triplet_pt2(a,p,i,j)
term(607) = term(607) + wm_interm_111_triplet_pt2(a,i,q,j) * wm_interm_129_triplet_pt2(a,p,i,j)
end do 
end do 
end do 

term(332) = term(332) * (-2.0d+0) 
term(333) = term(333) * (4.0d+0) 
term(334) = term(334) * (-2.0d+0) 
term(335) = term(335) * (2.0d+0) 
term(336) = term(336) * (4.0d+0) 
term(337) = term(337) * (-4.0d+0) 
term(338) = term(338) * (2.0d+0) 
term(339) = term(339) * (-4.0d+0) 
term(340) = term(340) * (1.5d+0) 
term(341) = term(341) * (-3.0d+0) 
term(342) = term(342) * (1.5d+0) 
term(343) = term(343) * (-2.0d+0) 
term(344) = term(344) * (-3.0d+0) 
term(345) = term(345) * (4.0d+0) 
term(346) = term(346) * (1.5d+0) 
term(347) = term(347) * (-3.0d+0) 
term(348) = term(348) * (1.5d+0) 
term(349) = term(349) * (-3.0d+0) 
term(350) = term(350) * (-2.0d+0) 
term(351) = term(351) * (4.0d+0) 
term(352) = term(352) * (1.5d+0) 
term(353) = term(353) * (-3.0d+0) 
term(354) = term(354) * (1.5d+0) 
term(355) = term(355) * (-3.0d+0) 
term(356) = term(356) * (-2.0d+0) 
term(357) = term(357) * (4.0d+0) 
term(358) = term(358) * (1.5d+0) 
term(359) = term(359) * (-3.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (1.5d+0) 
term(363) = term(363) * (-3.0d+0) 
term(364) = term(364) * (-1.0d+0) 
term(365) = term(365) * (2.0d+0) 
term(366) = term(366) * (-1.0d+0) 
term(367) = term(367) * (2.0d+0) 
term(368) = term(368) * (-1.0d+0) 
term(369) = term(369) * (2.0d+0) 
term(370) = term(370) * (-2.0d+0) 
term(371) = term(371) * (4.0d+0) 
term(372) = term(372) * (2.0d+0) 
term(373) = term(373) * (-4.0d+0) 
term(374) = term(374) * (-2.0d+0) 
term(375) = term(375) * (4.0d+0) 
term(376) = term(376) * (-1.0d+0) 
term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (2.0d+0) 
term(379) = term(379) * (-4.0d+0) 
term(380) = term(380) * (1.5d+0) 
term(381) = term(381) * (-1.0d+0) 
term(382) = term(382) * (1.5d+0) 
term(383) = term(383) * (-2.0d+0) 
term(384) = term(384) * (1.5d+0) 
term(385) = term(385) * (1.5d+0) 
term(386) = term(386) * (1.5d+0) 
term(387) = term(387) * (-1.0d+0) 
term(388) = term(388) * (-2.0d+0) 
term(389) = term(389) * (2.0d+0) 
term(390) = term(390) * (1.5d+0) 
term(391) = term(391) * (-2.0d+0) 
term(392) = term(392) * (1.5d+0) 
term(393) = term(393) * (-2.0d+0) 
term(394) = term(394) * (-2.0d+0) 
term(395) = term(395) * (1.5d+0) 
term(396) = term(396) * (-1.0d+0) 
term(397) = term(397) * (-2.0d+0) 
term(398) = term(398) * (2.0d+0) 
term(399) = term(399) * (-2.0d+0) 
term(400) = term(400) * (-2.0d+0) 
term(401) = term(401) * (2.0d+0) 
term(402) = term(402) * (1.5d+0) 
term(403) = term(403) * (-3.0d+0) 
term(404) = term(404) * (1.5d+0) 
term(405) = term(405) * (-2.0d+0) 
term(406) = term(406) * (1.5d+0) 
term(407) = term(407) * (1.5d+0) 
term(408) = term(408) * (-2.0d+0) 
term(409) = term(409) * (4.0d+0) 
term(410) = term(410) * (1.5d+0) 
term(411) = term(411) * (-2.0d+0) 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * (1.5d+0) 
term(414) = term(414) * (1.5d+0) 
term(415) = term(415) * (-3.0d+0) 
term(416) = term(416) * (1.5d+0) 
term(417) = term(417) * (-1.0d+0) 
term(418) = term(418) * (-2.0d+0) 
term(419) = term(419) * (4.0d+0) 
term(420) = term(420) * (-2.0d+0) 
term(421) = term(421) * (2.0d+0) 
term(422) = term(422) * (1.5d+0) 
term(423) = term(423) * (1.5d+0) 
term(424) = term(424) * (-2.0d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (-1.0d+0) 
term(427) = term(427) * (2.0d+0) 
term(428) = term(428) * (1.5d+0) 
term(429) = term(429) * (-1.0d+0) 
term(430) = term(430) * (2.0d+0) 
term(431) = term(431) * (-4.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(433) = term(433) * (2.0d+0) 
term(434) = term(434) * (-1.0d+0) 
term(435) = term(435) * (1.5d+0) 
term(436) = term(436) * (2.0d+0) 
term(437) = term(437) * (-2.0d+0) 
term(438) = term(438) * (1.5d+0) 
term(439) = term(439) * (1.5d+0) 
term(440) = term(440) * (-2.0d+0) 
term(441) = term(441) * (1.5d+0) 
term(442) = term(442) * (1.5d+0) 
term(443) = term(443) * (-2.0d+0) 
term(444) = term(444) * (1.5d+0) 
term(445) = term(445) * (-1.0d+0) 
term(446) = term(446) * (-2.0d+0) 
term(447) = term(447) * (2.0d+0) 
term(448) = term(448) * (1.5d+0) 
term(449) = term(449) * (-1.0d+0) 
term(450) = term(450) * (1.5d+0) 
term(451) = term(451) * (-3.0d+0) 
term(452) = term(452) * (1.5d+0) 
term(453) = term(453) * (1.5d+0) 
term(454) = term(454) * (-2.0d+0) 
term(455) = term(455) * (1.5d+0) 
term(456) = term(456) * (-1.0d+0) 
term(457) = term(457) * (1.5d+0) 
term(458) = term(458) * (1.5d+0) 
term(459) = term(459) * (-2.0d+0) 
term(460) = term(460) * (2.0d+0) 
term(461) = term(461) * (-2.0d+0) 
term(462) = term(462) * (-2.0d+0) 
term(463) = term(463) * (4.0d+0) 
term(464) = term(464) * (-2.0d+0) 
term(465) = term(465) * (2.0d+0) 
term(466) = term(466) * (-4.0d+0) 
term(467) = term(467) * (2.0d+0) 
term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (2.0d+0) 
term(470) = term(470) * (-2.0d+0) 
term(471) = term(471) * (2.0d+0) 
term(472) = term(472) * (-2.0d+0) 
term(473) = term(473) * (2.0d+0) 
term(474) = term(474) * (-2.0d+0) 
term(475) = term(475) * (2.0d+0) 
term(476) = term(476) * (-2.0d+0) 
term(477) = term(477) * (-1.0d+0) 
term(478) = term(478) * (2.0d+0) 
term(479) = term(479) * (1.5d+0) 
term(480) = term(480) * (1.5d+0) 
term(481) = term(481) * (-2.0d+0) 
term(482) = term(482) * (1.5d+0) 
term(483) = term(483) * (-3.0d+0) 
term(484) = term(484) * (-2.0d+0) 
term(485) = term(485) * (4.0d+0) 
term(486) = term(486) * (1.5d+0) 
term(487) = term(487) * (-2.0d+0) 
term(488) = term(488) * (1.5d+0) 
term(489) = term(489) * (1.5d+0) 
term(490) = term(490) * (1.5d+0) 
term(491) = term(491) * (-2.0d+0) 
term(492) = term(492) * (1.5d+0) 
term(493) = term(493) * (-2.0d+0) 
term(494) = term(494) * (-2.0d+0) 
term(495) = term(495) * (-2.0d+0) 
term(496) = term(496) * (-1.0d+0) 
term(497) = term(497) * (2.0d+0) 
term(498) = term(498) * (-1.0d+0) 
term(499) = term(499) * (2.0d+0) 
term(500) = term(500) * (-1.0d+0) 
term(501) = term(501) * (-1.0d+0) 
term(502) = term(502) * (-2.0d+0) 
term(503) = term(503) * (2.0d+0) 
term(504) = term(504) * (-2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (2.0d+0) 
term(507) = term(507) * (-1.0d+0) 
term(508) = term(508) * (2.0d+0) 
term(509) = term(509) * (-1.0d+0) 
term(510) = term(510) * (-1.0d+0) 
term(511) = term(511) * (-1.0d+0) 
term(512) = term(512) * (-8.0d+0) 
term(513) = term(513) * (16.0d+0) 
term(514) = term(514) * (8.0d+0) 
term(515) = term(515) * (-16.0d+0) 
term(516) = term(516) * (6.0d+0) 
term(517) = term(517) * (-12.0d+0) 
term(518) = term(518) * (-6.0d+0) 
term(519) = term(519) * (12.0d+0) 
term(520) = term(520) * (6.0d+0) 
term(521) = term(521) * (-12.0d+0) 
term(522) = term(522) * (-6.0d+0) 
term(523) = term(523) * (12.0d+0) 
term(524) = term(524) * (6.0d+0) 
term(525) = term(525) * (-12.0d+0) 
term(526) = term(526) * (-6.0d+0) 
term(527) = term(527) * (12.0d+0) 
term(528) = term(528) * (6.0d+0) 
term(529) = term(529) * (-12.0d+0) 
term(530) = term(530) * (-6.0d+0) 
term(531) = term(531) * (12.0d+0) 
term(532) = term(532) * (-8.0d+0) 
term(533) = term(533) * (16.0d+0) 
term(534) = term(534) * (8.0d+0) 
term(535) = term(535) * (-16.0d+0) 
term(536) = term(536) * (6.0d+0) 
term(537) = term(537) * (-6.0d+0) 
term(538) = term(538) * (6.0d+0) 
term(539) = term(539) * (-6.0d+0) 
term(540) = term(540) * (6.0d+0) 
term(541) = term(541) * (-6.0d+0) 
term(542) = term(542) * (-8.0d+0) 
term(543) = term(543) * (8.0d+0) 
term(544) = term(544) * (6.0d+0) 
term(545) = term(545) * (-8.0d+0) 
term(546) = term(546) * (-6.0d+0) 
term(547) = term(547) * (8.0d+0) 
term(548) = term(548) * (6.0d+0) 
term(549) = term(549) * (-6.0d+0) 
term(550) = term(550) * (6.0d+0) 
term(551) = term(551) * (-6.0d+0) 
term(552) = term(552) * (-8.0d+0) 
term(553) = term(553) * (8.0d+0) 
term(554) = term(554) * (6.0d+0) 
term(555) = term(555) * (-6.0d+0) 
term(556) = term(556) * (6.0d+0) 
term(557) = term(557) * (-6.0d+0) 
term(558) = term(558) * (6.0d+0) 
term(559) = term(559) * (-6.0d+0) 
term(560) = term(560) * (-8.0d+0) 
term(561) = term(561) * (8.0d+0) 
term(562) = term(562) * (-8.0d+0) 
term(563) = term(563) * (8.0d+0) 
term(564) = term(564) * (-4.0d+0) 
term(565) = term(565) * (4.0d+0) 
term(566) = term(566) * (6.0d+0) 
term(567) = term(567) * (-6.0d+0) 
term(568) = term(568) * (8.0d+0) 
term(569) = term(569) * (-8.0d+0) 
term(570) = term(570) * (-8.0d+0) 
term(571) = term(571) * (8.0d+0) 
term(572) = term(572) * (6.0d+0) 
term(573) = term(573) * (6.0d+0) 
term(574) = term(574) * (-8.0d+0) 
term(575) = term(575) * (6.0d+0) 
term(576) = term(576) * (-6.0d+0) 
term(577) = term(577) * (-6.0d+0) 
term(578) = term(578) * (8.0d+0) 
term(579) = term(579) * (-6.0d+0) 
term(580) = term(580) * (6.0d+0) 
term(581) = term(581) * (-6.0d+0) 
term(582) = term(582) * (6.0d+0) 
term(583) = term(583) * (-6.0d+0) 
term(584) = term(584) * (6.0d+0) 
term(585) = term(585) * (-6.0d+0) 
term(586) = term(586) * (-8.0d+0) 
term(587) = term(587) * (8.0d+0) 
term(588) = term(588) * (8.0d+0) 
term(589) = term(589) * (-8.0d+0) 
term(590) = term(590) * (-8.0d+0) 
term(591) = term(591) * (-8.0d+0) 
term(592) = term(592) * (8.0d+0) 
term(593) = term(593) * (8.0d+0) 
term(594) = term(594) * (6.0d+0) 
term(595) = term(595) * (-6.0d+0) 
term(596) = term(596) * (6.0d+0) 
term(597) = term(597) * (-6.0d+0) 
term(598) = term(598) * (-8.0d+0) 
term(599) = term(599) * (8.0d+0) 
term(600) = term(600) * (6.0d+0) 
term(601) = term(601) * (6.0d+0) 
term(602) = term(602) * (-6.0d+0) 
term(603) = term(603) * (-6.0d+0) 
term(604) = term(604) * (-8.0d+0) 
term(605) = term(605) * (8.0d+0) 
term(606) = term(606) * (-4.0d+0) 
term(607) = term(607) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(608) = term(608) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,l,q,k,j,i)
term(609) = term(609) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_100_triplet_pt2(b,q,l,k,j,i)
term(610) = term(610) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,q,l,k,j,i)
term(611) = term(611) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,q,l,k,j,i)
term(612) = term(612) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,k,l,q,j,i)
term(613) = term(613) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_97_triplet_pt2(b,q,l,k,j,i)
term(614) = term(614) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_97_triplet_pt2(b,k,l,q,j,i)
term(615) = term(615) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_99_triplet_pt2(b,k,l,q,j,i)
term(616) = term(616) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_100_triplet_pt2(b,k,l,q,j,i)
term(617) = term(617) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,k,l,q,j,i)
term(618) = term(618) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_102_triplet_pt2(b,k,l,q,j,i)
term(619) = term(619) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_99_triplet_pt2(b,q,l,k,j,i)
term(620) = term(620) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_102_triplet_pt2(b,q,l,k,j,i)
term(621) = term(621) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,l,k,q,j,i)
term(622) = term(622) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_102_triplet_pt2(b,l,q,k,j,i)
term(623) = term(623) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,l,k,q,j,i)
term(624) = term(624) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_97_triplet_pt2(b,l,k,q,j,i)
term(625) = term(625) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,l,q,k,j,i)
term(626) = term(626) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,l,q,k,j,i)
term(627) = term(627) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,q,l,k,j,i)
term(628) = term(628) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,q,l,k,j,i)
term(629) = term(629) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,k,l,q,j,i)
term(630) = term(630) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,k,l,q,j,i)
term(631) = term(631) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,k,l,q,j,i)
term(632) = term(632) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,q,l,k,j,i)
term(633) = term(633) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,l,k,q,j,i)
term(634) = term(634) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,l,q,k,j,i)
term(635) = term(635) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,l,k,q,j,i)
term(636) = term(636) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,l,q,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(608) = term(608) * (-1.0d+0) 
term(609) = term(609) * (-1.0d+0) 
term(610) = term(610) * (2.0d+0) 
term(611) = term(611) * (-1.0d+0) 
term(612) = term(612) * (2.0d+0) 
term(613) = term(613) * (0.5d+0) 
term(614) = term(614) * (-1.0d+0) 
term(615) = term(615) * (-1.0d+0) 
term(616) = term(616) * (0.5d+0) 
term(617) = term(617) * (-1.0d+0) 
term(618) = term(618) * (0.5d+0) 
term(619) = term(619) * (0.5d+0) 
term(620) = term(620) * (-1.0d+0) 
term(621) = term(621) * (0.5d+0) 
term(622) = term(622) * (0.5d+0) 
term(623) = term(623) * (-1.0d+0) 
term(624) = term(624) * (0.5d+0) 
term(625) = term(625) * (0.5d+0) 
term(626) = term(626) * (4.0d+0) 
term(627) = term(627) * (-4.0d+0) 
term(628) = term(628) * (-2.0d+0) 
term(629) = term(629) * (4.0d+0) 
term(630) = term(630) * (-2.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (2.0d+0) 
term(633) = term(633) * (-2.0d+0) 
term(634) = term(634) * (-2.0d+0) 
term(635) = term(635) * (-2.0d+0) 
term(636) = term(636) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(637) = term(637) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_5_triplet_pt2(c,b)
term(638) = term(638) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_7_triplet_pt2(c,b)
term(639) = term(639) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_5_triplet_pt2(c,a)
term(640) = term(640) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_7_triplet_pt2(c,a)
term(641) = term(641) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_5_triplet_pt2(c,a)
term(642) = term(642) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_7_triplet_pt2(c,a)
term(643) = term(643) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_5_triplet_pt2(c,b)
term(644) = term(644) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_7_triplet_pt2(c,b)
end do 
end do 
end do 
end do 
end do 

term(637) = term(637) * (3.0d+0) 
term(638) = term(638) * (-6.0d+0) 
term(639) = term(639) * (3.0d+0) 
term(640) = term(640) * (-6.0d+0) 
term(641) = term(641) * (-4.0d+0) 
term(642) = term(642) * (8.0d+0) 
term(643) = term(643) * (-2.0d+0) 
term(644) = term(644) * (4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(645) = term(645) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_10_triplet_pt2(a,k,i,j)
term(646) = term(646) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_11_triplet_pt2(a,k,i,j)
term(647) = term(647) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_9_triplet_pt2(a,k,i,j)
term(648) = term(648) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_0_triplet_pt2(a,k,i,j)
term(649) = term(649) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_3_triplet_pt2(a,k,i,j)
term(650) = term(650) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_4_triplet_pt2(a,k,i,j)
term(651) = term(651) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_50_triplet_pt2(a,k,i,j)
term(652) = term(652) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_51_triplet_pt2(a,k,i,j)
term(653) = term(653) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_54_triplet_pt2(a,k,i,j)
term(654) = term(654) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_49_triplet_pt2(a,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(645) = term(645) * (-6.0d+0) 
term(646) = term(646) * (8.0d+0) 
term(647) = term(647) * (-6.0d+0) 
term(648) = term(648) * (8.0d+0) 
term(649) = term(649) * (-6.0d+0) 
term(650) = term(650) * (4.0d+0) 
term(651) = term(651) * (-24.0d+0) 
term(652) = term(652) * (24.0d+0) 
term(653) = term(653) * (-12.0d+0) 
term(654) = term(654) * (16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(655) = term(655) + wm_interm_12_triplet_pt2(a,p,i,q) * wm_interm_35_triplet_pt2(a,i)
term(656) = term(656) + wm_interm_12_triplet_pt2(a,p,i,q) * wm_interm_36_triplet_pt2(a,i)
term(657) = term(657) + wm_interm_35_triplet_pt2(a,i) * wm_interm_8_triplet_pt2(a,p,i,q)
term(658) = term(658) + wm_interm_36_triplet_pt2(a,i) * wm_interm_8_triplet_pt2(a,p,i,q)
term(659) = term(659) + wm_interm_35_triplet_pt2(a,i) * wm_interm_6_triplet_pt2(a,p,i,q)
term(660) = term(660) + wm_interm_36_triplet_pt2(a,i) * wm_interm_6_triplet_pt2(a,p,i,q)
term(661) = term(661) + wm_interm_12_triplet_pt2(a,p,i,q) * wm_interm_37_triplet_pt2(a,i)
term(662) = term(662) + wm_interm_37_triplet_pt2(a,i) * wm_interm_8_triplet_pt2(a,p,i,q)
term(663) = term(663) + wm_interm_37_triplet_pt2(a,i) * wm_interm_6_triplet_pt2(a,p,i,q)
term(664) = term(664) + wm_interm_12_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(665) = term(665) + wm_interm_38_triplet_pt2(a,i) * wm_interm_8_triplet_pt2(a,p,i,q)
term(666) = term(666) + wm_interm_38_triplet_pt2(a,i) * wm_interm_6_triplet_pt2(a,p,i,q)
term(667) = term(667) + wm_interm_13_triplet_pt2(a,p,i,q) * wm_interm_35_triplet_pt2(a,i)
term(668) = term(668) + wm_interm_13_triplet_pt2(a,p,i,q) * wm_interm_36_triplet_pt2(a,i)
term(669) = term(669) + wm_interm_29_triplet_pt2(a,p,i,q) * wm_interm_35_triplet_pt2(a,i)
term(670) = term(670) + wm_interm_29_triplet_pt2(a,p,i,q) * wm_interm_36_triplet_pt2(a,i)
term(671) = term(671) + wm_interm_17_triplet_pt2(a,p,i,q) * wm_interm_35_triplet_pt2(a,i)
term(672) = term(672) + wm_interm_17_triplet_pt2(a,p,i,q) * wm_interm_36_triplet_pt2(a,i)
term(673) = term(673) + wm_interm_13_triplet_pt2(a,p,i,q) * wm_interm_37_triplet_pt2(a,i)
term(674) = term(674) + wm_interm_29_triplet_pt2(a,p,i,q) * wm_interm_37_triplet_pt2(a,i)
term(675) = term(675) + wm_interm_17_triplet_pt2(a,p,i,q) * wm_interm_37_triplet_pt2(a,i)
term(676) = term(676) + wm_interm_13_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(677) = term(677) + wm_interm_29_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(678) = term(678) + wm_interm_17_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(679) = term(679) + wm_interm_39_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(a,p,i,q)
term(680) = term(680) + wm_interm_40_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(a,p,i,q)
term(681) = term(681) + wm_interm_41_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(a,p,i,q)
term(682) = term(682) + wm_interm_42_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(a,p,i,q)
term(683) = term(683) + wm_interm_43_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(a,p,i,q)
term(684) = term(684) + wm_interm_44_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(a,p,i,q)
term(685) = term(685) + wm_interm_39_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(a,p,i,q)
term(686) = term(686) + wm_interm_40_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(a,p,i,q)
term(687) = term(687) + wm_interm_41_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(a,p,i,q)
term(688) = term(688) + wm_interm_42_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(a,p,i,q)
term(689) = term(689) + wm_interm_43_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(a,p,i,q)
term(690) = term(690) + wm_interm_44_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(a,p,i,q)
term(691) = term(691) + wm_interm_39_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(a,p,i,q)
term(692) = term(692) + wm_interm_40_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(a,p,i,q)
term(693) = term(693) + wm_interm_41_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(a,p,i,q)
term(694) = term(694) + wm_interm_42_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(a,p,i,q)
term(695) = term(695) + wm_interm_43_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(a,p,i,q)
term(696) = term(696) + wm_interm_44_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(a,p,i,q)
term(697) = term(697) + wm_interm_39_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(a,p,i,q)
term(698) = term(698) + wm_interm_40_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(a,p,i,q)
term(699) = term(699) + wm_interm_41_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(a,p,i,q)
term(700) = term(700) + wm_interm_42_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(a,p,i,q)
term(701) = term(701) + wm_interm_43_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(a,p,i,q)
term(702) = term(702) + wm_interm_44_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(a,p,i,q)
term(703) = term(703) + wm_interm_35_triplet_pt2(a,i) * wm_interm_53_triplet_pt2(a,p,i,q)
term(704) = term(704) + wm_interm_36_triplet_pt2(a,i) * wm_interm_53_triplet_pt2(a,p,i,q)
term(705) = term(705) + wm_interm_35_triplet_pt2(a,i) * wm_interm_52_triplet_pt2(a,p,i,q)
term(706) = term(706) + wm_interm_36_triplet_pt2(a,i) * wm_interm_52_triplet_pt2(a,p,i,q)
term(707) = term(707) + wm_interm_37_triplet_pt2(a,i) * wm_interm_53_triplet_pt2(a,p,i,q)
term(708) = term(708) + wm_interm_37_triplet_pt2(a,i) * wm_interm_52_triplet_pt2(a,p,i,q)
term(709) = term(709) + wm_interm_38_triplet_pt2(a,i) * wm_interm_53_triplet_pt2(a,p,i,q)
term(710) = term(710) + wm_interm_38_triplet_pt2(a,i) * wm_interm_52_triplet_pt2(a,p,i,q)
term(711) = term(711) + wm_interm_35_triplet_pt2(a,i) * wm_interm_55_triplet_pt2(a,p,i,q)
term(712) = term(712) + wm_interm_36_triplet_pt2(a,i) * wm_interm_55_triplet_pt2(a,p,i,q)
term(713) = term(713) + wm_interm_35_triplet_pt2(a,i) * wm_interm_58_triplet_pt2(a,p,i,q)
term(714) = term(714) + wm_interm_36_triplet_pt2(a,i) * wm_interm_58_triplet_pt2(a,p,i,q)
term(715) = term(715) + wm_interm_37_triplet_pt2(a,i) * wm_interm_55_triplet_pt2(a,p,i,q)
term(716) = term(716) + wm_interm_37_triplet_pt2(a,i) * wm_interm_58_triplet_pt2(a,p,i,q)
term(717) = term(717) + wm_interm_38_triplet_pt2(a,i) * wm_interm_55_triplet_pt2(a,p,i,q)
term(718) = term(718) + wm_interm_38_triplet_pt2(a,i) * wm_interm_58_triplet_pt2(a,p,i,q)
term(719) = term(719) + wm_interm_45_triplet_pt2(a,p,i,q) * wm_interm_63_triplet_pt2(a,i)
term(720) = term(720) + wm_interm_45_triplet_pt2(a,p,i,q) * wm_interm_64_triplet_pt2(a,i)
term(721) = term(721) + wm_interm_45_triplet_pt2(a,p,i,q) * wm_interm_65_triplet_pt2(a,i)
term(722) = term(722) + wm_interm_45_triplet_pt2(a,p,i,q) * wm_interm_66_triplet_pt2(a,i)
term(723) = term(723) + wm_interm_46_triplet_pt2(a,p,i,q) * wm_interm_63_triplet_pt2(a,i)
term(724) = term(724) + wm_interm_46_triplet_pt2(a,p,i,q) * wm_interm_64_triplet_pt2(a,i)
term(725) = term(725) + wm_interm_46_triplet_pt2(a,p,i,q) * wm_interm_65_triplet_pt2(a,i)
term(726) = term(726) + wm_interm_46_triplet_pt2(a,p,i,q) * wm_interm_66_triplet_pt2(a,i)
term(727) = term(727) + wm_interm_48_triplet_pt2(a,p,i,q) * wm_interm_63_triplet_pt2(a,i)
term(728) = term(728) + wm_interm_48_triplet_pt2(a,p,i,q) * wm_interm_64_triplet_pt2(a,i)
term(729) = term(729) + wm_interm_48_triplet_pt2(a,p,i,q) * wm_interm_65_triplet_pt2(a,i)
term(730) = term(730) + wm_interm_48_triplet_pt2(a,p,i,q) * wm_interm_66_triplet_pt2(a,i)
term(731) = term(731) + wm_interm_47_triplet_pt2(a,p,i,q) * wm_interm_63_triplet_pt2(a,i)
term(732) = term(732) + wm_interm_47_triplet_pt2(a,p,i,q) * wm_interm_64_triplet_pt2(a,i)
term(733) = term(733) + wm_interm_47_triplet_pt2(a,p,i,q) * wm_interm_65_triplet_pt2(a,i)
term(734) = term(734) + wm_interm_47_triplet_pt2(a,p,i,q) * wm_interm_66_triplet_pt2(a,i)
term(735) = term(735) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_79_triplet_pt2(a,i)
term(736) = term(736) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_79_triplet_pt2(a,i)
term(737) = term(737) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_80_triplet_pt2(a,i)
term(738) = term(738) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_80_triplet_pt2(a,i)
term(739) = term(739) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_83_triplet_pt2(a,i)
term(740) = term(740) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_83_triplet_pt2(a,i)
term(741) = term(741) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_70_triplet_pt2(a,i)
term(742) = term(742) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_70_triplet_pt2(a,i)
term(743) = term(743) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_74_triplet_pt2(a,i)
term(744) = term(744) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_74_triplet_pt2(a,i)
term(745) = term(745) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_81_triplet_pt2(a,i)
term(746) = term(746) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_81_triplet_pt2(a,i)
term(747) = term(747) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_71_triplet_pt2(a,i)
term(748) = term(748) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_71_triplet_pt2(a,i)
term(749) = term(749) + wm_interm_45_triplet_pt2(p,a,q,i) * wm_interm_92_triplet_pt2(a,i)
term(750) = term(750) + wm_interm_46_triplet_pt2(p,a,q,i) * wm_interm_92_triplet_pt2(a,i)
term(751) = term(751) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_70_triplet_pt2(a,i)
term(752) = term(752) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_71_triplet_pt2(a,i)
term(753) = term(753) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_70_triplet_pt2(a,i)
term(754) = term(754) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_71_triplet_pt2(a,i)
term(755) = term(755) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_74_triplet_pt2(a,i)
term(756) = term(756) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_74_triplet_pt2(a,i)
term(757) = term(757) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_79_triplet_pt2(a,i)
term(758) = term(758) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_79_triplet_pt2(a,i)
term(759) = term(759) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_80_triplet_pt2(a,i)
term(760) = term(760) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_80_triplet_pt2(a,i)
term(761) = term(761) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_81_triplet_pt2(a,i)
term(762) = term(762) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_81_triplet_pt2(a,i)
term(763) = term(763) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_83_triplet_pt2(a,i)
term(764) = term(764) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_83_triplet_pt2(a,i)
term(765) = term(765) + wm_interm_48_triplet_pt2(p,a,q,i) * wm_interm_92_triplet_pt2(a,i)
term(766) = term(766) + wm_interm_47_triplet_pt2(p,a,q,i) * wm_interm_92_triplet_pt2(a,i)
term(767) = term(767) + wm_interm_124_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(p,a,q,i)
term(768) = term(768) + wm_interm_124_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(p,a,q,i)
term(769) = term(769) + wm_interm_125_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(p,a,q,i)
term(770) = term(770) + wm_interm_125_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(p,a,q,i)
term(771) = term(771) + wm_interm_118_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(p,a,q,i)
term(772) = term(772) + wm_interm_118_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(p,a,q,i)
term(773) = term(773) + wm_interm_119_triplet_pt2(a,i) * wm_interm_45_triplet_pt2(p,a,q,i)
term(774) = term(774) + wm_interm_119_triplet_pt2(a,i) * wm_interm_46_triplet_pt2(p,a,q,i)
term(775) = term(775) + wm_interm_118_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(p,a,q,i)
term(776) = term(776) + wm_interm_119_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(p,a,q,i)
term(777) = term(777) + wm_interm_118_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(p,a,q,i)
term(778) = term(778) + wm_interm_119_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(p,a,q,i)
term(779) = term(779) + wm_interm_124_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(p,a,q,i)
term(780) = term(780) + wm_interm_124_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(p,a,q,i)
term(781) = term(781) + wm_interm_125_triplet_pt2(a,i) * wm_interm_48_triplet_pt2(p,a,q,i)
term(782) = term(782) + wm_interm_125_triplet_pt2(a,i) * wm_interm_47_triplet_pt2(p,a,q,i)
term(783) = term(783) + wm_interm_139_triplet_pt2(a,i) * wm_interm_88_triplet_pt2(a,p,i,q)
term(784) = term(784) + wm_interm_140_triplet_pt2(a,i) * wm_interm_88_triplet_pt2(a,p,i,q)
term(785) = term(785) + wm_interm_139_triplet_pt2(a,i) * wm_interm_84_triplet_pt2(a,p,i,q)
term(786) = term(786) + wm_interm_140_triplet_pt2(a,i) * wm_interm_84_triplet_pt2(a,p,i,q)
term(787) = term(787) + wm_interm_139_triplet_pt2(a,i) * wm_interm_85_triplet_pt2(a,p,i,q)
term(788) = term(788) + wm_interm_140_triplet_pt2(a,i) * wm_interm_85_triplet_pt2(a,p,i,q)
term(789) = term(789) + wm_interm_139_triplet_pt2(a,i) * wm_interm_89_triplet_pt2(a,p,i,q)
term(790) = term(790) + wm_interm_140_triplet_pt2(a,i) * wm_interm_89_triplet_pt2(a,p,i,q)
term(791) = term(791) + wm_interm_139_triplet_pt2(a,i) * wm_interm_86_triplet_pt2(a,p,i,q)
term(792) = term(792) + wm_interm_140_triplet_pt2(a,i) * wm_interm_86_triplet_pt2(a,p,i,q)
term(793) = term(793) + wm_interm_139_triplet_pt2(a,i) * wm_interm_87_triplet_pt2(a,p,i,q)
term(794) = term(794) + wm_interm_140_triplet_pt2(a,i) * wm_interm_87_triplet_pt2(a,p,i,q)
term(795) = term(795) + wm_interm_141_triplet_pt2(a,i) * wm_interm_88_triplet_pt2(a,p,i,q)
term(796) = term(796) + wm_interm_141_triplet_pt2(a,i) * wm_interm_84_triplet_pt2(a,p,i,q)
term(797) = term(797) + wm_interm_141_triplet_pt2(a,i) * wm_interm_85_triplet_pt2(a,p,i,q)
term(798) = term(798) + wm_interm_141_triplet_pt2(a,i) * wm_interm_89_triplet_pt2(a,p,i,q)
term(799) = term(799) + wm_interm_141_triplet_pt2(a,i) * wm_interm_86_triplet_pt2(a,p,i,q)
term(800) = term(800) + wm_interm_141_triplet_pt2(a,i) * wm_interm_87_triplet_pt2(a,p,i,q)
term(801) = term(801) + wm_interm_142_triplet_pt2(a,i) * wm_interm_88_triplet_pt2(a,p,i,q)
term(802) = term(802) + wm_interm_142_triplet_pt2(a,i) * wm_interm_84_triplet_pt2(a,p,i,q)
term(803) = term(803) + wm_interm_142_triplet_pt2(a,i) * wm_interm_85_triplet_pt2(a,p,i,q)
term(804) = term(804) + wm_interm_142_triplet_pt2(a,i) * wm_interm_89_triplet_pt2(a,p,i,q)
term(805) = term(805) + wm_interm_142_triplet_pt2(a,i) * wm_interm_86_triplet_pt2(a,p,i,q)
term(806) = term(806) + wm_interm_142_triplet_pt2(a,i) * wm_interm_87_triplet_pt2(a,p,i,q)
term(807) = term(807) + wm_interm_126_triplet_pt2(a,p,i,q) * wm_interm_139_triplet_pt2(a,i)
term(808) = term(808) + wm_interm_126_triplet_pt2(a,p,i,q) * wm_interm_140_triplet_pt2(a,i)
term(809) = term(809) + wm_interm_127_triplet_pt2(a,p,i,q) * wm_interm_139_triplet_pt2(a,i)
term(810) = term(810) + wm_interm_127_triplet_pt2(a,p,i,q) * wm_interm_140_triplet_pt2(a,i)
term(811) = term(811) + wm_interm_128_triplet_pt2(a,p,i,q) * wm_interm_139_triplet_pt2(a,i)
term(812) = term(812) + wm_interm_128_triplet_pt2(a,p,i,q) * wm_interm_140_triplet_pt2(a,i)
term(813) = term(813) + wm_interm_129_triplet_pt2(a,p,i,q) * wm_interm_139_triplet_pt2(a,i)
term(814) = term(814) + wm_interm_129_triplet_pt2(a,p,i,q) * wm_interm_140_triplet_pt2(a,i)
term(815) = term(815) + wm_interm_126_triplet_pt2(a,p,i,q) * wm_interm_141_triplet_pt2(a,i)
term(816) = term(816) + wm_interm_127_triplet_pt2(a,p,i,q) * wm_interm_141_triplet_pt2(a,i)
term(817) = term(817) + wm_interm_128_triplet_pt2(a,p,i,q) * wm_interm_141_triplet_pt2(a,i)
term(818) = term(818) + wm_interm_129_triplet_pt2(a,p,i,q) * wm_interm_141_triplet_pt2(a,i)
term(819) = term(819) + wm_interm_126_triplet_pt2(a,p,i,q) * wm_interm_142_triplet_pt2(a,i)
term(820) = term(820) + wm_interm_127_triplet_pt2(a,p,i,q) * wm_interm_142_triplet_pt2(a,i)
term(821) = term(821) + wm_interm_128_triplet_pt2(a,p,i,q) * wm_interm_142_triplet_pt2(a,i)
term(822) = term(822) + wm_interm_129_triplet_pt2(a,p,i,q) * wm_interm_142_triplet_pt2(a,i)
end do 
end do 

term(655) = term(655) * (-8.0d+0) 
term(656) = term(656) * (4.0d+0) 
term(657) = term(657) * (-8.0d+0) 
term(658) = term(658) * (4.0d+0) 
term(659) = term(659) * (16.0d+0) 
term(660) = term(660) * (-8.0d+0) 
term(661) = term(661) * (6.0d+0) 
term(662) = term(662) * (6.0d+0) 
term(663) = term(663) * (-12.0d+0) 
term(664) = term(664) * (-2.0d+0) 
term(665) = term(665) * (-2.0d+0) 
term(666) = term(666) * (4.0d+0) 
term(667) = term(667) * (4.0d+0) 
term(668) = term(668) * (-2.0d+0) 
term(669) = term(669) * (4.0d+0) 
term(670) = term(670) * (-2.0d+0) 
term(671) = term(671) * (-8.0d+0) 
term(672) = term(672) * (4.0d+0) 
term(673) = term(673) * (-3.0d+0) 
term(674) = term(674) * (-3.0d+0) 
term(675) = term(675) * (6.0d+0) 
term(678) = term(678) * (-2.0d+0) 
term(679) = term(679) * (-6.0d+0) 
term(680) = term(680) * (8.0d+0) 
term(681) = term(681) * (-6.0d+0) 
term(682) = term(682) * (4.0d+0) 
term(683) = term(683) * (8.0d+0) 
term(684) = term(684) * (-8.0d+0) 
term(685) = term(685) * (12.0d+0) 
term(686) = term(686) * (-16.0d+0) 
term(687) = term(687) * (12.0d+0) 
term(688) = term(688) * (-8.0d+0) 
term(689) = term(689) * (-16.0d+0) 
term(690) = term(690) * (16.0d+0) 
term(691) = term(691) * (3.0d+0) 
term(692) = term(692) * (-4.0d+0) 
term(693) = term(693) * (3.0d+0) 
term(694) = term(694) * (-2.0d+0) 
term(695) = term(695) * (-4.0d+0) 
term(696) = term(696) * (4.0d+0) 
term(697) = term(697) * (-6.0d+0) 
term(698) = term(698) * (8.0d+0) 
term(699) = term(699) * (-6.0d+0) 
term(700) = term(700) * (4.0d+0) 
term(701) = term(701) * (8.0d+0) 
term(702) = term(702) * (-8.0d+0) 
term(703) = term(703) * (-32.0d+0) 
term(704) = term(704) * (16.0d+0) 
term(705) = term(705) * (32.0d+0) 
term(706) = term(706) * (-16.0d+0) 
term(707) = term(707) * (24.0d+0) 
term(708) = term(708) * (-24.0d+0) 
term(709) = term(709) * (-8.0d+0) 
term(710) = term(710) * (8.0d+0) 
term(711) = term(711) * (16.0d+0) 
term(712) = term(712) * (-8.0d+0) 
term(713) = term(713) * (-16.0d+0) 
term(714) = term(714) * (8.0d+0) 
term(715) = term(715) * (-12.0d+0) 
term(716) = term(716) * (12.0d+0) 
term(717) = term(717) * (4.0d+0) 
term(718) = term(718) * (-4.0d+0) 
term(719) = term(719) * (-24.0d+0) 
term(720) = term(720) * (24.0d+0) 
term(721) = term(721) * (16.0d+0) 
term(722) = term(722) * (-16.0d+0) 
term(723) = term(723) * (48.0d+0) 
term(724) = term(724) * (-48.0d+0) 
term(725) = term(725) * (-32.0d+0) 
term(726) = term(726) * (32.0d+0) 
term(727) = term(727) * (12.0d+0) 
term(728) = term(728) * (-12.0d+0) 
term(729) = term(729) * (-8.0d+0) 
term(730) = term(730) * (8.0d+0) 
term(731) = term(731) * (-24.0d+0) 
term(732) = term(732) * (24.0d+0) 
term(733) = term(733) * (16.0d+0) 
term(734) = term(734) * (-16.0d+0) 
term(735) = term(735) * (4.0d+0) 
term(736) = term(736) * (-8.0d+0) 
term(737) = term(737) * (4.0d+0) 
term(738) = term(738) * (-8.0d+0) 
term(739) = term(739) * (-4.0d+0) 
term(740) = term(740) * (8.0d+0) 
term(741) = term(741) * (-6.0d+0) 
term(742) = term(742) * (12.0d+0) 
term(743) = term(743) * (-6.0d+0) 
term(744) = term(744) * (12.0d+0) 
term(745) = term(745) * (8.0d+0) 
term(746) = term(746) * (-16.0d+0) 
term(747) = term(747) * (4.0d+0) 
term(748) = term(748) * (-8.0d+0) 
term(749) = term(749) * (-4.0d+0) 
term(750) = term(750) * (8.0d+0) 
term(751) = term(751) * (-6.0d+0) 
term(752) = term(752) * (4.0d+0) 
term(753) = term(753) * (3.0d+0) 
term(754) = term(754) * (-2.0d+0) 
term(755) = term(755) * (-6.0d+0) 
term(756) = term(756) * (3.0d+0) 
term(757) = term(757) * (-2.0d+0) 
term(758) = term(758) * (4.0d+0) 
term(759) = term(759) * (-2.0d+0) 
term(760) = term(760) * (4.0d+0) 
term(761) = term(761) * (8.0d+0) 
term(762) = term(762) * (-4.0d+0) 
term(763) = term(763) * (2.0d+0) 
term(764) = term(764) * (-4.0d+0) 
term(765) = term(765) * (2.0d+0) 
term(766) = term(766) * (-4.0d+0) 
term(767) = term(767) * (16.0d+0) 
term(768) = term(768) * (-32.0d+0) 
term(769) = term(769) * (-16.0d+0) 
term(770) = term(770) * (32.0d+0) 
term(771) = term(771) * (-24.0d+0) 
term(772) = term(772) * (48.0d+0) 
term(773) = term(773) * (24.0d+0) 
term(774) = term(774) * (-48.0d+0) 
term(775) = term(775) * (-24.0d+0) 
term(776) = term(776) * (24.0d+0) 
term(777) = term(777) * (12.0d+0) 
term(778) = term(778) * (-12.0d+0) 
term(779) = term(779) * (-8.0d+0) 
term(780) = term(780) * (16.0d+0) 
term(781) = term(781) * (8.0d+0) 
term(782) = term(782) * (-16.0d+0) 
term(783) = term(783) * (-4.0d+0) 
term(784) = term(784) * (2.0d+0) 
term(785) = term(785) * (-4.0d+0) 
term(786) = term(786) * (2.0d+0) 
term(787) = term(787) * (8.0d+0) 
term(788) = term(788) * (-4.0d+0) 
term(789) = term(789) * (8.0d+0) 
term(790) = term(790) * (-4.0d+0) 
term(791) = term(791) * (8.0d+0) 
term(792) = term(792) * (-4.0d+0) 
term(793) = term(793) * (-16.0d+0) 
term(794) = term(794) * (8.0d+0) 
term(795) = term(795) * (3.0d+0) 
term(796) = term(796) * (3.0d+0) 
term(797) = term(797) * (-6.0d+0) 
term(798) = term(798) * (-6.0d+0) 
term(799) = term(799) * (-6.0d+0) 
term(800) = term(800) * (12.0d+0) 
term(801) = term(801) * (-1.0d+0) 
term(802) = term(802) * (-1.0d+0) 
term(803) = term(803) * (2.0d+0) 
term(804) = term(804) * (2.0d+0) 
term(805) = term(805) * (2.0d+0) 
term(806) = term(806) * (-4.0d+0) 
term(807) = term(807) * (-16.0d+0) 
term(808) = term(808) * (8.0d+0) 
term(809) = term(809) * (16.0d+0) 
term(810) = term(810) * (-8.0d+0) 
term(811) = term(811) * (32.0d+0) 
term(812) = term(812) * (-16.0d+0) 
term(813) = term(813) * (-32.0d+0) 
term(814) = term(814) * (16.0d+0) 
term(815) = term(815) * (12.0d+0) 
term(816) = term(816) * (-12.0d+0) 
term(817) = term(817) * (-24.0d+0) 
term(818) = term(818) * (24.0d+0) 
term(819) = term(819) * (-4.0d+0) 
term(820) = term(820) * (4.0d+0) 
term(821) = term(821) * (8.0d+0) 
term(822) = term(822) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(823) = term(823) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_9_triplet_pt2(a,k,j,i)
term(824) = term(824) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_10_triplet_pt2(a,k,j,i)
term(825) = term(825) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_11_triplet_pt2(a,k,j,i)
term(826) = term(826) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_0_triplet_pt2(a,k,j,i)
term(827) = term(827) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_3_triplet_pt2(a,k,j,i)
term(828) = term(828) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_4_triplet_pt2(a,k,j,i)
term(829) = term(829) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_9_triplet_pt2(b,i,k,j)
term(830) = term(830) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_10_triplet_pt2(b,i,k,j)
term(831) = term(831) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_11_triplet_pt2(b,i,k,j)
term(832) = term(832) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_0_triplet_pt2(b,i,k,j)
term(833) = term(833) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_3_triplet_pt2(b,i,k,j)
term(834) = term(834) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_4_triplet_pt2(b,i,k,j)
term(835) = term(835) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_54_triplet_pt2(a,k,j,i)
term(836) = term(836) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_50_triplet_pt2(a,k,j,i)
term(837) = term(837) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_51_triplet_pt2(a,k,j,i)
term(838) = term(838) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_49_triplet_pt2(a,k,j,i)
term(839) = term(839) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_54_triplet_pt2(b,i,k,j)
term(840) = term(840) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_50_triplet_pt2(b,i,k,j)
term(841) = term(841) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_51_triplet_pt2(b,i,k,j)
term(842) = term(842) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_49_triplet_pt2(b,i,k,j)
term(843) = term(843) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,k,j,i)
term(844) = term(844) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,j,k,i)
term(845) = term(845) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_22_triplet_pt2(b,j,k,i)
term(846) = term(846) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_21_triplet_pt2(b,j,k,i)
term(847) = term(847) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,k,j,i)
term(848) = term(848) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,j,k,i)
term(849) = term(849) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_67_triplet_pt2(b,j,k,i)
term(850) = term(850) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_68_triplet_pt2(b,j,k,i)
term(851) = term(851) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_67_triplet_pt2(b,j,k,i)
term(852) = term(852) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_68_triplet_pt2(b,j,k,i)
term(853) = term(853) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_69_triplet_pt2(b,j,k,i)
term(854) = term(854) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_69_triplet_pt2(b,j,k,i)
term(855) = term(855) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_72_triplet_pt2(b,j,k,i)
term(856) = term(856) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_73_triplet_pt2(b,j,k,i)
term(857) = term(857) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_72_triplet_pt2(b,j,k,i)
term(858) = term(858) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_73_triplet_pt2(b,j,k,i)
term(859) = term(859) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_75_triplet_pt2(b,j,k,i)
term(860) = term(860) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_75_triplet_pt2(b,j,k,i)
term(861) = term(861) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_76_triplet_pt2(b,j,k,i)
term(862) = term(862) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_76_triplet_pt2(b,j,k,i)
term(863) = term(863) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_77_triplet_pt2(b,j,k,i)
term(864) = term(864) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_78_triplet_pt2(b,j,k,i)
term(865) = term(865) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_77_triplet_pt2(b,j,k,i)
term(866) = term(866) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_78_triplet_pt2(b,j,k,i)
term(867) = term(867) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_82_triplet_pt2(b,j,k,i)
term(868) = term(868) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_82_triplet_pt2(b,j,k,i)
term(869) = term(869) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_76_triplet_pt2(b,k,j,i)
term(870) = term(870) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_77_triplet_pt2(b,k,j,i)
term(871) = term(871) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_78_triplet_pt2(b,k,j,i)
term(872) = term(872) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_76_triplet_pt2(b,k,j,i)
term(873) = term(873) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_77_triplet_pt2(b,k,j,i)
term(874) = term(874) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_78_triplet_pt2(b,k,j,i)
term(875) = term(875) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_82_triplet_pt2(b,k,j,i)
term(876) = term(876) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_82_triplet_pt2(b,k,j,i)
term(877) = term(877) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_68_triplet_pt2(b,k,j,i)
term(878) = term(878) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_67_triplet_pt2(b,k,j,i)
term(879) = term(879) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_90_triplet_pt2(b,j,k,i)
term(880) = term(880) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_91_triplet_pt2(b,k,j,i)
term(881) = term(881) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_68_triplet_pt2(b,k,j,i)
term(882) = term(882) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_67_triplet_pt2(b,k,j,i)
term(883) = term(883) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_90_triplet_pt2(b,j,k,i)
term(884) = term(884) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_91_triplet_pt2(b,k,j,i)
term(885) = term(885) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_91_triplet_pt2(b,j,k,i)
term(886) = term(886) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_91_triplet_pt2(b,j,k,i)
term(887) = term(887) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_72_triplet_pt2(b,k,j,i)
term(888) = term(888) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_72_triplet_pt2(b,k,j,i)
term(889) = term(889) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_75_triplet_pt2(b,k,j,i)
term(890) = term(890) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_75_triplet_pt2(b,k,j,i)
term(891) = term(891) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_90_triplet_pt2(b,k,j,i)
term(892) = term(892) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_90_triplet_pt2(b,k,j,i)
term(893) = term(893) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_69_triplet_pt2(b,k,j,i)
term(894) = term(894) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_69_triplet_pt2(b,k,j,i)
term(895) = term(895) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_73_triplet_pt2(b,k,j,i)
term(896) = term(896) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_73_triplet_pt2(b,k,j,i)
term(897) = term(897) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_116_triplet_pt2(b,j,k,i)
term(898) = term(898) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_117_triplet_pt2(b,j,k,i)
term(899) = term(899) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_116_triplet_pt2(b,j,k,i)
term(900) = term(900) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_117_triplet_pt2(b,j,k,i)
term(901) = term(901) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_120_triplet_pt2(b,j,k,i)
term(902) = term(902) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_121_triplet_pt2(b,j,k,i)
term(903) = term(903) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_120_triplet_pt2(b,j,k,i)
term(904) = term(904) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_121_triplet_pt2(b,j,k,i)
term(905) = term(905) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_122_triplet_pt2(b,j,k,i)
term(906) = term(906) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_122_triplet_pt2(b,j,k,i)
term(907) = term(907) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_123_triplet_pt2(b,j,k,i)
term(908) = term(908) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_123_triplet_pt2(b,j,k,i)
term(909) = term(909) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_122_triplet_pt2(b,k,j,i)
term(910) = term(910) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_123_triplet_pt2(b,k,j,i)
term(911) = term(911) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_122_triplet_pt2(b,k,j,i)
term(912) = term(912) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_123_triplet_pt2(b,k,j,i)
term(913) = term(913) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_117_triplet_pt2(b,k,j,i)
term(914) = term(914) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_116_triplet_pt2(b,k,j,i)
term(915) = term(915) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_117_triplet_pt2(b,k,j,i)
term(916) = term(916) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_116_triplet_pt2(b,k,j,i)
term(917) = term(917) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_120_triplet_pt2(b,k,j,i)
term(918) = term(918) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_120_triplet_pt2(b,k,j,i)
term(919) = term(919) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_121_triplet_pt2(b,k,j,i)
term(920) = term(920) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_121_triplet_pt2(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(823) = term(823) * (4.0d+0) 
term(824) = term(824) * (8.0d+0) 
term(825) = term(825) * (-8.0d+0) 
term(826) = term(826) * (-6.0d+0) 
term(827) = term(827) * (8.0d+0) 
term(828) = term(828) * (-8.0d+0) 
term(829) = term(829) * (-2.0d+0) 
term(830) = term(830) * (-4.0d+0) 
term(831) = term(831) * (4.0d+0) 
term(832) = term(832) * (3.0d+0) 
term(833) = term(833) * (-4.0d+0) 
term(834) = term(834) * (4.0d+0) 
term(835) = term(835) * (8.0d+0) 
term(836) = term(836) * (32.0d+0) 
term(837) = term(837) * (-32.0d+0) 
term(838) = term(838) * (-12.0d+0) 
term(839) = term(839) * (-4.0d+0) 
term(840) = term(840) * (-16.0d+0) 
term(841) = term(841) * (16.0d+0) 
term(842) = term(842) * (6.0d+0) 
term(843) = term(843) * (6.0d+0) 
term(844) = term(844) * (-4.0d+0) 
term(845) = term(845) * (6.0d+0) 
term(846) = term(846) * (-8.0d+0) 
term(847) = term(847) * (-8.0d+0) 
term(848) = term(848) * (8.0d+0) 
term(849) = term(849) * (1.5d+0) 
term(850) = term(850) * (-2.0d+0) 
term(851) = term(851) * (-3.0d+0) 
term(852) = term(852) * (4.0d+0) 
term(853) = term(853) * (1.5d+0) 
term(854) = term(854) * (-3.0d+0) 
term(855) = term(855) * (-2.0d+0) 
term(856) = term(856) * (2.0d+0) 
term(857) = term(857) * (4.0d+0) 
term(858) = term(858) * (-4.0d+0) 
term(859) = term(859) * (-2.0d+0) 
term(860) = term(860) * (4.0d+0) 
term(861) = term(861) * (1.5d+0) 
term(862) = term(862) * (-3.0d+0) 
term(863) = term(863) * (1.5d+0) 
term(864) = term(864) * (-1.0d+0) 
term(865) = term(865) * (-3.0d+0) 
term(866) = term(866) * (2.0d+0) 
term(867) = term(867) * (-2.0d+0) 
term(868) = term(868) * (4.0d+0) 
term(869) = term(869) * (-2.0d+0) 
term(870) = term(870) * (-2.0d+0) 
term(871) = term(871) * (2.0d+0) 
term(872) = term(872) * (4.0d+0) 
term(873) = term(873) * (4.0d+0) 
term(874) = term(874) * (-4.0d+0) 
term(875) = term(875) * (2.0d+0) 
term(876) = term(876) * (-4.0d+0) 
term(877) = term(877) * (1.5d+0) 
term(878) = term(878) * (-1.0d+0) 
term(879) = term(879) * (2.0d+0) 
term(880) = term(880) * (1.5d+0) 
term(881) = term(881) * (-3.0d+0) 
term(882) = term(882) * (2.0d+0) 
term(883) = term(883) * (-4.0d+0) 
term(884) = term(884) * (-3.0d+0) 
term(885) = term(885) * (-1.0d+0) 
term(886) = term(886) * (2.0d+0) 
term(887) = term(887) * (1.5d+0) 
term(888) = term(888) * (-3.0d+0) 
term(889) = term(889) * (1.5d+0) 
term(890) = term(890) * (-3.0d+0) 
term(891) = term(891) * (-2.0d+0) 
term(892) = term(892) * (4.0d+0) 
term(893) = term(893) * (-2.0d+0) 
term(894) = term(894) * (4.0d+0) 
term(895) = term(895) * (-1.0d+0) 
term(896) = term(896) * (2.0d+0) 
term(897) = term(897) * (6.0d+0) 
term(898) = term(898) * (-6.0d+0) 
term(899) = term(899) * (-12.0d+0) 
term(900) = term(900) * (12.0d+0) 
term(901) = term(901) * (-8.0d+0) 
term(902) = term(902) * (8.0d+0) 
term(903) = term(903) * (16.0d+0) 
term(904) = term(904) * (-16.0d+0) 
term(905) = term(905) * (6.0d+0) 
term(906) = term(906) * (-12.0d+0) 
term(907) = term(907) * (-6.0d+0) 
term(908) = term(908) * (12.0d+0) 
term(909) = term(909) * (-8.0d+0) 
term(910) = term(910) * (8.0d+0) 
term(911) = term(911) * (16.0d+0) 
term(912) = term(912) * (-16.0d+0) 
term(913) = term(913) * (6.0d+0) 
term(914) = term(914) * (-6.0d+0) 
term(915) = term(915) * (-12.0d+0) 
term(916) = term(916) * (12.0d+0) 
term(917) = term(917) * (6.0d+0) 
term(918) = term(918) * (-12.0d+0) 
term(919) = term(919) * (-6.0d+0) 
term(920) = term(920) * (12.0d+0) 

do i = 1, nocc 
term(921) = term(921) + wm_interm_18_triplet_pt2(i,q) * wm_interm_35_triplet_pt2(p,i)
term(922) = term(922) + wm_interm_18_triplet_pt2(i,q) * wm_interm_36_triplet_pt2(p,i)
term(923) = term(923) + wm_interm_34_triplet_pt2(i,q) * wm_interm_35_triplet_pt2(p,i)
term(924) = term(924) + wm_interm_34_triplet_pt2(i,q) * wm_interm_36_triplet_pt2(p,i)
term(925) = term(925) + wm_interm_31_triplet_pt2(i,q) * wm_interm_35_triplet_pt2(p,i)
term(926) = term(926) + wm_interm_31_triplet_pt2(i,q) * wm_interm_36_triplet_pt2(p,i)
term(927) = term(927) + wm_interm_18_triplet_pt2(i,q) * wm_interm_37_triplet_pt2(p,i)
term(928) = term(928) + wm_interm_34_triplet_pt2(i,q) * wm_interm_37_triplet_pt2(p,i)
term(929) = term(929) + wm_interm_31_triplet_pt2(i,q) * wm_interm_37_triplet_pt2(p,i)
term(930) = term(930) + wm_interm_18_triplet_pt2(i,q) * wm_interm_38_triplet_pt2(p,i)
term(931) = term(931) + wm_interm_34_triplet_pt2(i,q) * wm_interm_38_triplet_pt2(p,i)
term(932) = term(932) + wm_interm_31_triplet_pt2(i,q) * wm_interm_38_triplet_pt2(p,i)
term(933) = term(933) + wm_interm_1_triplet_pt2(i,q) * wm_interm_39_triplet_pt2(p,i)
term(934) = term(934) + wm_interm_1_triplet_pt2(i,q) * wm_interm_40_triplet_pt2(p,i)
term(935) = term(935) + wm_interm_1_triplet_pt2(i,q) * wm_interm_41_triplet_pt2(p,i)
term(936) = term(936) + wm_interm_1_triplet_pt2(i,q) * wm_interm_42_triplet_pt2(p,i)
term(937) = term(937) + wm_interm_1_triplet_pt2(i,q) * wm_interm_43_triplet_pt2(p,i)
term(938) = term(938) + wm_interm_1_triplet_pt2(i,q) * wm_interm_44_triplet_pt2(p,i)
term(939) = term(939) + wm_interm_2_triplet_pt2(i,q) * wm_interm_39_triplet_pt2(p,i)
term(940) = term(940) + wm_interm_2_triplet_pt2(i,q) * wm_interm_40_triplet_pt2(p,i)
term(941) = term(941) + wm_interm_2_triplet_pt2(i,q) * wm_interm_41_triplet_pt2(p,i)
term(942) = term(942) + wm_interm_2_triplet_pt2(i,q) * wm_interm_42_triplet_pt2(p,i)
term(943) = term(943) + wm_interm_2_triplet_pt2(i,q) * wm_interm_43_triplet_pt2(p,i)
term(944) = term(944) + wm_interm_2_triplet_pt2(i,q) * wm_interm_44_triplet_pt2(p,i)
term(945) = term(945) + wm_interm_35_triplet_pt2(p,i) * wm_interm_59_triplet_pt2(i,q)
term(946) = term(946) + wm_interm_36_triplet_pt2(p,i) * wm_interm_59_triplet_pt2(i,q)
term(947) = term(947) + wm_interm_35_triplet_pt2(p,i) * wm_interm_62_triplet_pt2(i,q)
term(948) = term(948) + wm_interm_36_triplet_pt2(p,i) * wm_interm_62_triplet_pt2(i,q)
term(949) = term(949) + wm_interm_37_triplet_pt2(p,i) * wm_interm_59_triplet_pt2(i,q)
term(950) = term(950) + wm_interm_37_triplet_pt2(p,i) * wm_interm_62_triplet_pt2(i,q)
term(951) = term(951) + wm_interm_38_triplet_pt2(p,i) * wm_interm_59_triplet_pt2(i,q)
term(952) = term(952) + wm_interm_38_triplet_pt2(p,i) * wm_interm_62_triplet_pt2(i,q)
term(953) = term(953) + wm_interm_1_triplet_pt2(i,q) * wm_interm_63_triplet_pt2(p,i)
term(954) = term(954) + wm_interm_1_triplet_pt2(i,q) * wm_interm_64_triplet_pt2(p,i)
term(955) = term(955) + wm_interm_1_triplet_pt2(i,q) * wm_interm_65_triplet_pt2(p,i)
term(956) = term(956) + wm_interm_1_triplet_pt2(i,q) * wm_interm_66_triplet_pt2(p,i)
term(957) = term(957) + wm_interm_2_triplet_pt2(i,q) * wm_interm_63_triplet_pt2(p,i)
term(958) = term(958) + wm_interm_2_triplet_pt2(i,q) * wm_interm_64_triplet_pt2(p,i)
term(959) = term(959) + wm_interm_2_triplet_pt2(i,q) * wm_interm_65_triplet_pt2(p,i)
term(960) = term(960) + wm_interm_2_triplet_pt2(i,q) * wm_interm_66_triplet_pt2(p,i)
end do 

term(921) = term(921) * (4.0d+0) 
term(922) = term(922) * (-2.0d+0) 
term(923) = term(923) * (4.0d+0) 
term(924) = term(924) * (-2.0d+0) 
term(925) = term(925) * (-8.0d+0) 
term(926) = term(926) * (4.0d+0) 
term(927) = term(927) * (-3.0d+0) 
term(928) = term(928) * (-3.0d+0) 
term(929) = term(929) * (6.0d+0) 
term(932) = term(932) * (-2.0d+0) 
term(933) = term(933) * (3.0d+0) 
term(934) = term(934) * (-4.0d+0) 
term(935) = term(935) * (3.0d+0) 
term(936) = term(936) * (-2.0d+0) 
term(937) = term(937) * (-4.0d+0) 
term(938) = term(938) * (4.0d+0) 
term(939) = term(939) * (-6.0d+0) 
term(940) = term(940) * (8.0d+0) 
term(941) = term(941) * (-6.0d+0) 
term(942) = term(942) * (4.0d+0) 
term(943) = term(943) * (8.0d+0) 
term(944) = term(944) * (-8.0d+0) 
term(945) = term(945) * (16.0d+0) 
term(946) = term(946) * (-8.0d+0) 
term(947) = term(947) * (-16.0d+0) 
term(948) = term(948) * (8.0d+0) 
term(949) = term(949) * (-12.0d+0) 
term(950) = term(950) * (12.0d+0) 
term(951) = term(951) * (4.0d+0) 
term(952) = term(952) * (-4.0d+0) 
term(953) = term(953) * (12.0d+0) 
term(954) = term(954) * (-12.0d+0) 
term(955) = term(955) * (-8.0d+0) 
term(956) = term(956) * (8.0d+0) 
term(957) = term(957) * (-24.0d+0) 
term(958) = term(958) * (24.0d+0) 
term(959) = term(959) * (16.0d+0) 
term(960) = term(960) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(961) = term(961) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j)
term(962) = term(962) + r1(vrdav_Rl, b,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j)
term(963) = term(963) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i)
term(964) = term(964) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i)
term(965) = term(965) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,i) * t1(b,j)
term(966) = term(966) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,i) * t1(a,j)
term(967) = term(967) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,j) * t1(b,i)
term(968) = term(968) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(961) = term(961) * (-8.0d+0) 
term(962) = term(962) * (6.0d+0) 
term(963) = term(963) * (6.0d+0) 
term(964) = term(964) * (-4.0d+0) 
term(965) = term(965) * (8.0d+0) 
term(966) = term(966) * (-6.0d+0) 
term(967) = term(967) * (-6.0d+0) 
term(968) = term(968) * (4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(969) = term(969) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j)
term(970) = term(970) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,i) * t1(b,j)
term(971) = term(971) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,j) * t1(b,i)
term(972) = term(972) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,i) * t1(a,j)
term(973) = term(973) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(969) = term(969) * (8.0d+0) 
term(970) = term(970) * (-4.0d+0) 
term(971) = term(971) * (4.0d+0) 
term(972) = term(972) * (4.0d+0) 
term(973) = term(973) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(974) = term(974) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_6_triplet_pt2(a,c,i,k)
term(975) = term(975) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_6_triplet_pt2(a,c,j,k)
term(976) = term(976) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_6_triplet_pt2(a,c,i,k)
term(977) = term(977) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_8_triplet_pt2(a,c,i,k)
term(978) = term(978) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_8_triplet_pt2(a,c,j,k)
term(979) = term(979) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_8_triplet_pt2(a,c,i,k)
term(980) = term(980) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_12_triplet_pt2(a,c,i,k)
term(981) = term(981) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_12_triplet_pt2(a,c,j,k)
term(982) = term(982) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_12_triplet_pt2(a,c,i,k)
term(983) = term(983) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_13_triplet_pt2(a,c,j,k)
term(984) = term(984) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_13_triplet_pt2(a,c,i,k)
term(985) = term(985) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_13_triplet_pt2(a,c,i,k)
term(986) = term(986) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_17_triplet_pt2(a,c,i,k)
term(987) = term(987) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_17_triplet_pt2(a,c,j,k)
term(988) = term(988) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_17_triplet_pt2(a,c,i,k)
term(989) = term(989) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_29_triplet_pt2(a,c,j,k)
term(990) = term(990) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_29_triplet_pt2(a,c,i,k)
term(991) = term(991) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_29_triplet_pt2(a,c,i,k)
term(992) = term(992) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_33_triplet_pt2(a,c,i,k)
term(993) = term(993) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_52_triplet_pt2(a,c,i,k)
term(994) = term(994) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_52_triplet_pt2(a,c,j,k)
term(995) = term(995) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_52_triplet_pt2(a,c,i,k)
term(996) = term(996) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_53_triplet_pt2(a,c,i,k)
term(997) = term(997) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_53_triplet_pt2(a,c,j,k)
term(998) = term(998) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_53_triplet_pt2(a,c,i,k)
term(999) = term(999) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_55_triplet_pt2(a,c,j,k)
term(1000) = term(1000) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_55_triplet_pt2(a,c,i,k)
term(1001) = term(1001) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_55_triplet_pt2(a,c,i,k)
term(1002) = term(1002) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_58_triplet_pt2(a,c,i,k)
term(1003) = term(1003) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_58_triplet_pt2(a,c,j,k)
term(1004) = term(1004) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_58_triplet_pt2(a,c,i,k)
term(1005) = term(1005) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_84_triplet_pt2(a,c,i,k)
term(1006) = term(1006) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_85_triplet_pt2(a,c,i,k)
term(1007) = term(1007) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_86_triplet_pt2(a,c,i,k)
term(1008) = term(1008) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_87_triplet_pt2(a,c,i,k)
term(1009) = term(1009) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_88_triplet_pt2(a,c,i,k)
term(1010) = term(1010) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_89_triplet_pt2(a,c,i,k)
term(1011) = term(1011) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_84_triplet_pt2(a,c,j,k)
term(1012) = term(1012) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_85_triplet_pt2(a,c,j,k)
term(1013) = term(1013) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_86_triplet_pt2(a,c,j,k)
term(1014) = term(1014) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_87_triplet_pt2(a,c,j,k)
term(1015) = term(1015) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_88_triplet_pt2(a,c,j,k)
term(1016) = term(1016) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_89_triplet_pt2(a,c,j,k)
term(1017) = term(1017) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_84_triplet_pt2(b,c,i,k)
term(1018) = term(1018) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_85_triplet_pt2(b,c,i,k)
term(1019) = term(1019) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_86_triplet_pt2(b,c,i,k)
term(1020) = term(1020) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_87_triplet_pt2(b,c,i,k)
term(1021) = term(1021) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_88_triplet_pt2(b,c,i,k)
term(1022) = term(1022) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_89_triplet_pt2(b,c,i,k)
term(1023) = term(1023) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_84_triplet_pt2(b,c,j,k)
term(1024) = term(1024) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_85_triplet_pt2(b,c,j,k)
term(1025) = term(1025) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_86_triplet_pt2(b,c,j,k)
term(1026) = term(1026) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_87_triplet_pt2(b,c,j,k)
term(1027) = term(1027) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_88_triplet_pt2(b,c,j,k)
term(1028) = term(1028) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_89_triplet_pt2(b,c,j,k)
term(1029) = term(1029) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_86_triplet_pt2(b,c,i,k)
term(1030) = term(1030) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_87_triplet_pt2(b,c,i,k)
term(1031) = term(1031) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_89_triplet_pt2(b,c,i,k)
term(1032) = term(1032) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_85_triplet_pt2(b,c,i,k)
term(1033) = term(1033) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_93_triplet_pt2(b,c,i,k)
term(1034) = term(1034) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_88_triplet_pt2(b,c,i,k)
term(1035) = term(1035) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_84_triplet_pt2(a,c,i,k)
term(1036) = term(1036) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_85_triplet_pt2(a,c,i,k)
term(1037) = term(1037) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_86_triplet_pt2(a,c,i,k)
term(1038) = term(1038) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_87_triplet_pt2(a,c,i,k)
term(1039) = term(1039) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_88_triplet_pt2(a,c,i,k)
term(1040) = term(1040) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_89_triplet_pt2(a,c,i,k)
term(1041) = term(1041) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_84_triplet_pt2(b,c,i,k)
term(1042) = term(1042) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_93_triplet_pt2(a,c,i,k)
term(1043) = term(1043) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_126_triplet_pt2(a,c,i,k)
term(1044) = term(1044) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_127_triplet_pt2(a,c,i,k)
term(1045) = term(1045) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_128_triplet_pt2(a,c,i,k)
term(1046) = term(1046) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_129_triplet_pt2(a,c,i,k)
term(1047) = term(1047) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_126_triplet_pt2(a,c,j,k)
term(1048) = term(1048) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_127_triplet_pt2(a,c,j,k)
term(1049) = term(1049) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_128_triplet_pt2(a,c,j,k)
term(1050) = term(1050) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_129_triplet_pt2(a,c,j,k)
term(1051) = term(1051) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_126_triplet_pt2(b,c,i,k)
term(1052) = term(1052) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_127_triplet_pt2(b,c,i,k)
term(1053) = term(1053) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_128_triplet_pt2(b,c,i,k)
term(1054) = term(1054) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_129_triplet_pt2(b,c,i,k)
term(1055) = term(1055) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_126_triplet_pt2(b,c,j,k)
term(1056) = term(1056) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_127_triplet_pt2(b,c,j,k)
term(1057) = term(1057) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_128_triplet_pt2(b,c,j,k)
term(1058) = term(1058) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_129_triplet_pt2(b,c,j,k)
term(1059) = term(1059) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_128_triplet_pt2(b,c,i,k)
term(1060) = term(1060) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_129_triplet_pt2(b,c,i,k)
term(1061) = term(1061) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_127_triplet_pt2(b,c,i,k)
term(1062) = term(1062) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_126_triplet_pt2(b,c,i,k)
term(1063) = term(1063) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_126_triplet_pt2(a,c,i,k)
term(1064) = term(1064) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_127_triplet_pt2(a,c,i,k)
term(1065) = term(1065) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_128_triplet_pt2(a,c,i,k)
term(1066) = term(1066) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_129_triplet_pt2(a,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(974) = term(974) * (-8.0d+0) 
term(975) = term(975) * (-16.0d+0) 
term(976) = term(976) * (16.0d+0) 
term(977) = term(977) * (4.0d+0) 
term(978) = term(978) * (8.0d+0) 
term(979) = term(979) * (-8.0d+0) 
term(980) = term(980) * (4.0d+0) 
term(981) = term(981) * (8.0d+0) 
term(982) = term(982) * (-8.0d+0) 
term(983) = term(983) * (-4.0d+0) 
term(984) = term(984) * (4.0d+0) 
term(985) = term(985) * (-4.0d+0) 
term(986) = term(986) * (4.0d+0) 
term(987) = term(987) * (8.0d+0) 
term(988) = term(988) * (-8.0d+0) 
term(989) = term(989) * (-4.0d+0) 
term(990) = term(990) * (4.0d+0) 
term(991) = term(991) * (-4.0d+0) 
term(992) = term(992) * (4.0d+0) 
term(993) = term(993) * (-16.0d+0) 
term(994) = term(994) * (-32.0d+0) 
term(995) = term(995) * (32.0d+0) 
term(996) = term(996) * (16.0d+0) 
term(997) = term(997) * (32.0d+0) 
term(998) = term(998) * (-32.0d+0) 
term(999) = term(999) * (-16.0d+0) 
term(1000) = term(1000) * (16.0d+0) 
term(1001) = term(1001) * (-16.0d+0) 
term(1002) = term(1002) * (16.0d+0) 
term(1003) = term(1003) * (16.0d+0) 
term(1004) = term(1004) * (-16.0d+0) 
term(1005) = term(1005) * (2.0d+0) 
term(1006) = term(1006) * (-4.0d+0) 
term(1007) = term(1007) * (-4.0d+0) 
term(1008) = term(1008) * (8.0d+0) 
term(1009) = term(1009) * (2.0d+0) 
term(1010) = term(1010) * (-4.0d+0) 
term(1011) = term(1011) * (-2.0d+0) 
term(1012) = term(1012) * (4.0d+0) 
term(1013) = term(1013) * (4.0d+0) 
term(1014) = term(1014) * (-8.0d+0) 
term(1015) = term(1015) * (-2.0d+0) 
term(1016) = term(1016) * (4.0d+0) 
term(1017) = term(1017) * (-2.0d+0) 
term(1018) = term(1018) * (4.0d+0) 
term(1019) = term(1019) * (4.0d+0) 
term(1020) = term(1020) * (-8.0d+0) 
term(1021) = term(1021) * (-2.0d+0) 
term(1022) = term(1022) * (4.0d+0) 
term(1023) = term(1023) * (2.0d+0) 
term(1024) = term(1024) * (-4.0d+0) 
term(1025) = term(1025) * (-4.0d+0) 
term(1026) = term(1026) * (8.0d+0) 
term(1027) = term(1027) * (2.0d+0) 
term(1028) = term(1028) * (-4.0d+0) 
term(1029) = term(1029) * (-2.0d+0) 
term(1030) = term(1030) * (4.0d+0) 
term(1031) = term(1031) * (-2.0d+0) 
term(1032) = term(1032) * (-2.0d+0) 
term(1033) = term(1033) * (-2.0d+0) 
term(1034) = term(1034) * (2.0d+0) 
term(1035) = term(1035) * (-2.0d+0) 
term(1036) = term(1036) * (2.0d+0) 
term(1037) = term(1037) * (2.0d+0) 
term(1038) = term(1038) * (-4.0d+0) 
term(1039) = term(1039) * (-2.0d+0) 
term(1040) = term(1040) * (2.0d+0) 
term(1041) = term(1041) * (2.0d+0) 
term(1042) = term(1042) * (2.0d+0) 
term(1043) = term(1043) * (8.0d+0) 
term(1044) = term(1044) * (-8.0d+0) 
term(1045) = term(1045) * (-16.0d+0) 
term(1046) = term(1046) * (16.0d+0) 
term(1047) = term(1047) * (-8.0d+0) 
term(1048) = term(1048) * (8.0d+0) 
term(1049) = term(1049) * (16.0d+0) 
term(1050) = term(1050) * (-16.0d+0) 
term(1051) = term(1051) * (-8.0d+0) 
term(1052) = term(1052) * (8.0d+0) 
term(1053) = term(1053) * (16.0d+0) 
term(1054) = term(1054) * (-16.0d+0) 
term(1055) = term(1055) * (8.0d+0) 
term(1056) = term(1056) * (-8.0d+0) 
term(1057) = term(1057) * (-16.0d+0) 
term(1058) = term(1058) * (16.0d+0) 
term(1059) = term(1059) * (-8.0d+0) 
term(1060) = term(1060) * (8.0d+0) 
term(1061) = term(1061) * (-8.0d+0) 
term(1062) = term(1062) * (8.0d+0) 
term(1063) = term(1063) * (-8.0d+0) 
term(1064) = term(1064) * (8.0d+0) 
term(1065) = term(1065) * (8.0d+0) 
term(1066) = term(1066) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(1067) = term(1067) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_100_triplet_pt2(b,l,q,k,i,j)
term(1068) = term(1068) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,l,q,k,i,j)
term(1069) = term(1069) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,q,l,k,i,j)
term(1070) = term(1070) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,q,l,k,i,j)
term(1071) = term(1071) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,k,l,q,i,j)
term(1072) = term(1072) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_99_triplet_pt2(b,k,l,q,i,j)
term(1073) = term(1073) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,k,l,q,i,j)
term(1074) = term(1074) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_102_triplet_pt2(b,q,l,k,i,j)
term(1075) = term(1075) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_100_triplet_pt2(b,l,k,q,i,j)
term(1076) = term(1076) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,l,k,q,i,j)
term(1077) = term(1077) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_99_triplet_pt2(b,l,q,k,i,j)
term(1078) = term(1078) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_102_triplet_pt2(b,l,q,k,i,j)
term(1079) = term(1079) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,l,k,q,i,j)
term(1080) = term(1080) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,l,q,k,i,j)
term(1081) = term(1081) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_97_triplet_pt2(b,l,q,k,i,j)
term(1082) = term(1082) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,l,q,k,i,j)
term(1083) = term(1083) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,q,l,k,i,j)
term(1084) = term(1084) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,q,l,k,i,j)
term(1085) = term(1085) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,k,l,q,i,j)
term(1086) = term(1086) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,k,l,q,i,j)
term(1087) = term(1087) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,k,l,q,i,j)
term(1088) = term(1088) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,q,l,k,i,j)
term(1089) = term(1089) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,l,k,q,i,j)
term(1090) = term(1090) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,l,q,k,i,j)
term(1091) = term(1091) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,l,k,q,i,j)
term(1092) = term(1092) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,l,q,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(1067) = term(1067) * (-1.0d+0) 
term(1068) = term(1068) * (2.0d+0) 
term(1069) = term(1069) * (-1.0d+0) 
term(1070) = term(1070) * (0.5d+0) 
term(1071) = term(1071) * (-1.0d+0) 
term(1072) = term(1072) * (0.5d+0) 
term(1073) = term(1073) * (0.5d+0) 
term(1074) = term(1074) * (0.5d+0) 
term(1075) = term(1075) * (0.5d+0) 
term(1076) = term(1076) * (-1.0d+0) 
term(1077) = term(1077) * (0.5d+0) 
term(1078) = term(1078) * (-1.0d+0) 
term(1079) = term(1079) * (0.5d+0) 
term(1080) = term(1080) * (-1.0d+0) 
term(1081) = term(1081) * (0.5d+0) 
term(1082) = term(1082) * (-4.0d+0) 
term(1083) = term(1083) * (4.0d+0) 
term(1084) = term(1084) * (2.0d+0) 
term(1085) = term(1085) * (-4.0d+0) 
term(1086) = term(1086) * (2.0d+0) 
term(1087) = term(1087) * (-2.0d+0) 
term(1088) = term(1088) * (-2.0d+0) 
term(1089) = term(1089) * (2.0d+0) 
term(1090) = term(1090) * (2.0d+0) 
term(1091) = term(1091) * (2.0d+0) 
term(1092) = term(1092) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1093) = term(1093) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_19_triplet_pt2(b,i,j,k)
term(1094) = term(1094) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_19_triplet_pt2(b,j,i,k)
term(1095) = term(1095) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_19_triplet_pt2(a,j,i,k)
term(1096) = term(1096) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_19_triplet_pt2(a,i,j,k)
term(1097) = term(1097) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_21_triplet_pt2(b,i,j,k)
term(1098) = term(1098) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_22_triplet_pt2(a,i,j,k)
term(1099) = term(1099) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_21_triplet_pt2(a,i,j,k)
term(1100) = term(1100) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_triplet_pt2(b,i,j,k)
term(1101) = term(1101) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt2(a,j,i,k)
term(1102) = term(1102) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt2(a,i,j,k)
term(1103) = term(1103) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_22_triplet_pt2(b,i,j,k)
term(1104) = term(1104) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_triplet_pt2(b,j,i,k)
term(1105) = term(1105) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_19_triplet_pt2(b,i,j,k)
term(1106) = term(1106) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_19_triplet_pt2(b,j,i,k)
term(1107) = term(1107) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_19_triplet_pt2(a,j,i,k)
term(1108) = term(1108) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_19_triplet_pt2(a,i,j,k)
term(1109) = term(1109) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_21_triplet_pt2(b,i,j,k)
term(1110) = term(1110) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_22_triplet_pt2(a,i,j,k)
term(1111) = term(1111) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_21_triplet_pt2(a,i,j,k)
term(1112) = term(1112) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_triplet_pt2(b,i,j,k)
term(1113) = term(1113) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt2(a,j,i,k)
term(1114) = term(1114) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt2(a,i,j,k)
term(1115) = term(1115) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_22_triplet_pt2(b,i,j,k)
term(1116) = term(1116) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_triplet_pt2(b,j,i,k)
term(1117) = term(1117) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_103_triplet_pt2(a,j,i,k)
term(1118) = term(1118) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_103_triplet_pt2(a,i,j,k)
term(1119) = term(1119) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_103_triplet_pt2(a,j,i,k)
term(1120) = term(1120) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_103_triplet_pt2(a,i,j,k)
term(1121) = term(1121) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_103_triplet_pt2(b,i,j,k)
term(1122) = term(1122) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_103_triplet_pt2(b,i,j,k)
term(1123) = term(1123) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_108_triplet_pt2(a,i,j,k)
term(1124) = term(1124) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_108_triplet_pt2(a,j,i,k)
term(1125) = term(1125) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_108_triplet_pt2(a,i,j,k)
term(1126) = term(1126) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_108_triplet_pt2(a,j,i,k)
term(1127) = term(1127) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_110_triplet_pt2(a,j,i,k)
term(1128) = term(1128) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_110_triplet_pt2(a,i,j,k)
term(1129) = term(1129) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_110_triplet_pt2(a,j,i,k)
term(1130) = term(1130) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_110_triplet_pt2(a,i,j,k)
term(1131) = term(1131) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_108_triplet_pt2(b,j,i,k)
term(1132) = term(1132) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_108_triplet_pt2(b,j,i,k)
term(1133) = term(1133) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_110_triplet_pt2(b,i,j,k)
term(1134) = term(1134) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_110_triplet_pt2(b,i,j,k)
term(1135) = term(1135) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_111_triplet_pt2(a,j,i,k)
term(1136) = term(1136) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_111_triplet_pt2(a,j,i,k)
term(1137) = term(1137) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_111_triplet_pt2(b,i,j,k)
term(1138) = term(1138) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_111_triplet_pt2(b,i,j,k)
term(1139) = term(1139) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_111_triplet_pt2(b,j,i,k)
term(1140) = term(1140) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_111_triplet_pt2(b,j,i,k)
term(1141) = term(1141) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_113_triplet_pt2(a,i,j,k)
term(1142) = term(1142) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_113_triplet_pt2(a,j,i,k)
term(1143) = term(1143) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_113_triplet_pt2(b,j,i,k)
term(1144) = term(1144) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_110_triplet_pt2(b,j,i,k)
term(1145) = term(1145) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_113_triplet_pt2(b,j,i,k)
term(1146) = term(1146) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_110_triplet_pt2(b,j,i,k)
term(1147) = term(1147) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_113_triplet_pt2(a,i,j,k)
term(1148) = term(1148) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_113_triplet_pt2(a,j,i,k)
term(1149) = term(1149) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_114_triplet_pt2(a,i,j,k)
term(1150) = term(1150) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_114_triplet_pt2(a,j,i,k)
term(1151) = term(1151) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_114_triplet_pt2(b,j,i,k)
term(1152) = term(1152) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_114_triplet_pt2(b,j,i,k)
term(1153) = term(1153) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_114_triplet_pt2(a,i,j,k)
term(1154) = term(1154) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_114_triplet_pt2(a,j,i,k)
term(1155) = term(1155) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_108_triplet_pt2(b,i,j,k)
term(1156) = term(1156) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_108_triplet_pt2(b,i,j,k)
term(1157) = term(1157) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_103_triplet_pt2(b,j,i,k)
term(1158) = term(1158) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_113_triplet_pt2(b,i,j,k)
term(1159) = term(1159) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_103_triplet_pt2(b,j,i,k)
term(1160) = term(1160) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_113_triplet_pt2(b,i,j,k)
term(1161) = term(1161) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_114_triplet_pt2(b,i,j,k)
term(1162) = term(1162) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,q) * wm_interm_114_triplet_pt2(b,i,j,k)
term(1163) = term(1163) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_111_triplet_pt2(a,i,j,k)
term(1164) = term(1164) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_111_triplet_pt2(a,i,j,k)
term(1165) = term(1165) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_103_triplet_pt2(a,j,i,k)
term(1166) = term(1166) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_103_triplet_pt2(a,i,j,k)
term(1167) = term(1167) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_103_triplet_pt2(a,j,i,k)
term(1168) = term(1168) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_103_triplet_pt2(a,i,j,k)
term(1169) = term(1169) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_108_triplet_pt2(a,i,j,k)
term(1170) = term(1170) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_108_triplet_pt2(a,j,i,k)
term(1171) = term(1171) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_108_triplet_pt2(a,i,j,k)
term(1172) = term(1172) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_108_triplet_pt2(a,j,i,k)
term(1173) = term(1173) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_110_triplet_pt2(a,j,i,k)
term(1174) = term(1174) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_110_triplet_pt2(a,i,j,k)
term(1175) = term(1175) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_110_triplet_pt2(a,j,i,k)
term(1176) = term(1176) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_110_triplet_pt2(a,i,j,k)
term(1177) = term(1177) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_111_triplet_pt2(a,j,i,k)
term(1178) = term(1178) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_111_triplet_pt2(a,j,i,k)
term(1179) = term(1179) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_111_triplet_pt2(a,i,j,k)
term(1180) = term(1180) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_111_triplet_pt2(a,i,j,k)
term(1181) = term(1181) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_113_triplet_pt2(a,i,j,k)
term(1182) = term(1182) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_113_triplet_pt2(a,j,i,k)
term(1183) = term(1183) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_113_triplet_pt2(a,i,j,k)
term(1184) = term(1184) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_113_triplet_pt2(a,j,i,k)
term(1185) = term(1185) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_114_triplet_pt2(a,i,j,k)
term(1186) = term(1186) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_114_triplet_pt2(a,j,i,k)
term(1187) = term(1187) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_114_triplet_pt2(a,i,j,k)
term(1188) = term(1188) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_114_triplet_pt2(a,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(1093) = term(1093) * (-6.0d+0) 
term(1094) = term(1094) * (8.0d+0) 
term(1095) = term(1095) * (-6.0d+0) 
term(1096) = term(1096) * (4.0d+0) 
term(1097) = term(1097) * (-6.0d+0) 
term(1098) = term(1098) * (-6.0d+0) 
term(1099) = term(1099) * (8.0d+0) 
term(1100) = term(1100) * (8.0d+0) 
term(1101) = term(1101) * (8.0d+0) 
term(1102) = term(1102) * (-8.0d+0) 
term(1103) = term(1103) * (4.0d+0) 
term(1104) = term(1104) * (-8.0d+0) 
term(1105) = term(1105) * (-12.0d+0) 
term(1106) = term(1106) * (16.0d+0) 
term(1107) = term(1107) * (-12.0d+0) 
term(1108) = term(1108) * (8.0d+0) 
term(1109) = term(1109) * (-12.0d+0) 
term(1110) = term(1110) * (-12.0d+0) 
term(1111) = term(1111) * (16.0d+0) 
term(1112) = term(1112) * (16.0d+0) 
term(1113) = term(1113) * (16.0d+0) 
term(1114) = term(1114) * (-16.0d+0) 
term(1115) = term(1115) * (8.0d+0) 
term(1116) = term(1116) * (-16.0d+0) 
term(1117) = term(1117) * (1.5d+0) 
term(1118) = term(1118) * (-2.0d+0) 
term(1119) = term(1119) * (-3.0d+0) 
term(1120) = term(1120) * (4.0d+0) 
term(1121) = term(1121) * (1.5d+0) 
term(1122) = term(1122) * (-3.0d+0) 
term(1123) = term(1123) * (1.5d+0) 
term(1124) = term(1124) * (-1.0d+0) 
term(1125) = term(1125) * (-3.0d+0) 
term(1126) = term(1126) * (2.0d+0) 
term(1127) = term(1127) * (-2.0d+0) 
term(1128) = term(1128) * (2.0d+0) 
term(1129) = term(1129) * (4.0d+0) 
term(1130) = term(1130) * (-4.0d+0) 
term(1131) = term(1131) * (1.5d+0) 
term(1132) = term(1132) * (-3.0d+0) 
term(1133) = term(1133) * (-2.0d+0) 
term(1134) = term(1134) * (4.0d+0) 
term(1135) = term(1135) * (1.5d+0) 
term(1136) = term(1136) * (-3.0d+0) 
term(1137) = term(1137) * (1.5d+0) 
term(1138) = term(1138) * (-3.0d+0) 
term(1139) = term(1139) * (-2.0d+0) 
term(1140) = term(1140) * (4.0d+0) 
term(1141) = term(1141) * (-2.0d+0) 
term(1142) = term(1142) * (2.0d+0) 
term(1143) = term(1143) * (-2.0d+0) 
term(1144) = term(1144) * (2.0d+0) 
term(1145) = term(1145) * (4.0d+0) 
term(1146) = term(1146) * (-4.0d+0) 
term(1147) = term(1147) * (4.0d+0) 
term(1148) = term(1148) * (-4.0d+0) 
term(1149) = term(1149) * (1.5d+0) 
term(1150) = term(1150) * (-2.0d+0) 
term(1151) = term(1151) * (1.5d+0) 
term(1152) = term(1152) * (-3.0d+0) 
term(1153) = term(1153) * (-3.0d+0) 
term(1154) = term(1154) * (4.0d+0) 
term(1155) = term(1155) * (-2.0d+0) 
term(1156) = term(1156) * (4.0d+0) 
term(1157) = term(1157) * (-1.0d+0) 
term(1158) = term(1158) * (2.0d+0) 
term(1159) = term(1159) * (2.0d+0) 
term(1160) = term(1160) * (-4.0d+0) 
term(1161) = term(1161) * (-1.0d+0) 
term(1162) = term(1162) * (2.0d+0) 
term(1163) = term(1163) * (-1.0d+0) 
term(1164) = term(1164) * (2.0d+0) 
term(1165) = term(1165) * (6.0d+0) 
term(1166) = term(1166) * (-6.0d+0) 
term(1167) = term(1167) * (-12.0d+0) 
term(1168) = term(1168) * (12.0d+0) 
term(1169) = term(1169) * (6.0d+0) 
term(1170) = term(1170) * (-6.0d+0) 
term(1171) = term(1171) * (-12.0d+0) 
term(1172) = term(1172) * (12.0d+0) 
term(1173) = term(1173) * (-8.0d+0) 
term(1174) = term(1174) * (8.0d+0) 
term(1175) = term(1175) * (16.0d+0) 
term(1176) = term(1176) * (-16.0d+0) 
term(1177) = term(1177) * (6.0d+0) 
term(1178) = term(1178) * (-12.0d+0) 
term(1179) = term(1179) * (-6.0d+0) 
term(1180) = term(1180) * (12.0d+0) 
term(1181) = term(1181) * (-8.0d+0) 
term(1182) = term(1182) * (8.0d+0) 
term(1183) = term(1183) * (16.0d+0) 
term(1184) = term(1184) * (-16.0d+0) 
term(1185) = term(1185) * (6.0d+0) 
term(1186) = term(1186) * (-6.0d+0) 
term(1187) = term(1187) * (-12.0d+0) 
term(1188) = term(1188) * (12.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(1189) = term(1189) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_5_triplet_pt2(c,a)
term(1190) = term(1190) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_7_triplet_pt2(c,a)
term(1191) = term(1191) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_5_triplet_pt2(c,b)
term(1192) = term(1192) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_7_triplet_pt2(c,b)
end do 
end do 
end do 
end do 
end do 

term(1189) = term(1189) * (-2.0d+0) 
term(1190) = term(1190) * (4.0d+0) 
term(1191) = term(1191) * (2.0d+0) 
term(1192) = term(1192) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1193) = term(1193) + wm_interm_15_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1194) = term(1194) + wm_interm_15_triplet_pt2(p,i,j,k,q,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1195) = term(1195) + wm_interm_15_triplet_pt2(p,i,j,k,q,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1196) = term(1196) + wm_interm_15_triplet_pt2(p,i,j,k,l,q) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1197) = term(1197) + wm_interm_15_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1198) = term(1198) + wm_interm_15_triplet_pt2(p,i,j,k,l,q) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1199) = term(1199) + wm_interm_25_triplet_pt2(i,j,k,l) * wm_interm_26_triplet_pt2(p,i,j,q,k,l)
term(1200) = term(1200) + wm_interm_25_triplet_pt2(i,j,k,l) * wm_interm_27_triplet_pt2(p,i,j,q,k,l)
term(1201) = term(1201) + wm_interm_25_triplet_pt2(i,j,k,l) * wm_interm_28_triplet_pt2(p,i,j,q,k,l)
term(1202) = term(1202) + wm_interm_26_triplet_pt2(p,i,j,q,k,l) * wm_interm_60_triplet_pt2(i,j,l,k)
term(1203) = term(1203) + wm_interm_26_triplet_pt2(p,i,j,q,k,l) * wm_interm_60_triplet_pt2(i,j,k,l)
term(1204) = term(1204) + wm_interm_27_triplet_pt2(p,i,j,q,k,l) * wm_interm_60_triplet_pt2(i,j,k,l)
term(1205) = term(1205) + wm_interm_28_triplet_pt2(p,i,j,q,k,l) * wm_interm_60_triplet_pt2(i,j,l,k)
term(1206) = term(1206) + wm_interm_27_triplet_pt2(p,i,j,q,k,l) * wm_interm_60_triplet_pt2(i,j,l,k)
term(1207) = term(1207) + wm_interm_28_triplet_pt2(p,i,j,q,k,l) * wm_interm_60_triplet_pt2(i,j,k,l)
term(1208) = term(1208) + wm_interm_100_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1209) = term(1209) + wm_interm_101_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1210) = term(1210) + wm_interm_101_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1211) = term(1211) + wm_interm_100_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1212) = term(1212) + wm_interm_101_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1213) = term(1213) + wm_interm_101_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1214) = term(1214) + wm_interm_102_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1215) = term(1215) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_109_triplet_pt2(p,q,i,j,k,l)
term(1216) = term(1216) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,i,j,q,k,l)
term(1217) = term(1217) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,i,q,j,k,l)
term(1218) = term(1218) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,q,i,j,k,l)
term(1219) = term(1219) + wm_interm_100_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1220) = term(1220) + wm_interm_102_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1221) = term(1221) + wm_interm_102_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1222) = term(1222) + wm_interm_101_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1223) = term(1223) + wm_interm_101_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1224) = term(1224) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,q,i,j,k,l)
term(1225) = term(1225) + wm_interm_132_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1226) = term(1226) + wm_interm_132_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1227) = term(1227) + wm_interm_134_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1228) = term(1228) + wm_interm_134_triplet_pt2(p,i,q,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1229) = term(1229) + wm_interm_132_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1230) = term(1230) + wm_interm_132_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1231) = term(1231) + wm_interm_133_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1232) = term(1232) + wm_interm_133_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1233) = term(1233) + wm_interm_134_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1234) = term(1234) + wm_interm_134_triplet_pt2(p,q,i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1235) = term(1235) + wm_interm_109_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1236) = term(1236) + wm_interm_109_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(j,i,l,k)
term(1237) = term(1237) + wm_interm_109_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
term(1238) = term(1238) + wm_interm_109_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1239) = term(1239) + wm_interm_109_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
term(1240) = term(1240) + wm_interm_109_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(i,j,k,l)
term(1241) = term(1241) + wm_interm_112_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(j,i,l,k)
term(1242) = term(1242) + wm_interm_112_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(i,j,k,l)
term(1243) = term(1243) + wm_interm_112_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
term(1244) = term(1244) + wm_interm_112_triplet_pt2(p,i,q,j,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1245) = term(1245) + wm_interm_112_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1246) = term(1246) + wm_interm_112_triplet_pt2(p,i,q,j,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
term(1247) = term(1247) + wm_interm_112_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
term(1248) = term(1248) + wm_interm_112_triplet_pt2(p,i,q,j,k,l) * wm_interm_138_triplet_pt2(i,j,k,l)
term(1249) = term(1249) + wm_interm_112_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(i,j,k,l)
term(1250) = term(1250) + wm_interm_132_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1251) = term(1251) + wm_interm_132_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1252) = term(1252) + wm_interm_133_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1253) = term(1253) + wm_interm_134_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1254) = term(1254) + wm_interm_133_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1255) = term(1255) + wm_interm_134_triplet_pt2(p,i,j,q,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1256) = term(1256) + wm_interm_115_triplet_pt2(p,i,q,j,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1257) = term(1257) + wm_interm_115_triplet_pt2(p,i,q,j,k,l) * wm_interm_138_triplet_pt2(j,i,l,k)
term(1258) = term(1258) + wm_interm_115_triplet_pt2(p,i,q,j,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
term(1259) = term(1259) + wm_interm_115_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(j,i,l,k)
term(1260) = term(1260) + wm_interm_115_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1261) = term(1261) + wm_interm_115_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(i,j,l,k)
term(1262) = term(1262) + wm_interm_115_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(j,i,l,k)
term(1263) = term(1263) + wm_interm_115_triplet_pt2(p,q,i,j,k,l) * wm_interm_138_triplet_pt2(i,j,k,l)
term(1264) = term(1264) + wm_interm_115_triplet_pt2(p,i,j,q,k,l) * wm_interm_138_triplet_pt2(j,i,k,l)
end do 
end do 
end do 
end do 

term(1193) = term(1193) * (3.0d+0) 
term(1194) = term(1194) * (-2.0d+0) 
term(1195) = term(1195) * (3.0d+0) 
term(1196) = term(1196) * (-4.0d+0) 
term(1197) = term(1197) * (-4.0d+0) 
term(1198) = term(1198) * (4.0d+0) 
term(1199) = term(1199) * (-2.0d+0) 
term(1200) = term(1200) * (3.0d+0) 
term(1201) = term(1201) * (4.0d+0) 
term(1202) = term(1202) * (6.0d+0) 
term(1203) = term(1203) * (-4.0d+0) 
term(1204) = term(1204) * (6.0d+0) 
term(1205) = term(1205) * (-8.0d+0) 
term(1206) = term(1206) * (-8.0d+0) 
term(1207) = term(1207) * (8.0d+0) 
term(1208) = term(1208) * (0.5d+0) 
term(1209) = term(1209) * (0.5d+0) 
term(1210) = term(1210) * (-1.0d+0) 
term(1211) = term(1211) * (0.5d+0) 
term(1212) = term(1212) * (0.5d+0) 
term(1213) = term(1213) * (-1.0d+0) 
term(1214) = term(1214) * (0.5d+0) 
term(1215) = term(1215) * (-1.0d+0) 
term(1216) = term(1216) * (0.5d+0) 
term(1217) = term(1217) * (-1.0d+0) 
term(1218) = term(1218) * (2.0d+0) 
term(1219) = term(1219) * (-1.0d+0) 
term(1220) = term(1220) * (0.5d+0) 
term(1221) = term(1221) * (-1.0d+0) 
term(1222) = term(1222) * (-1.0d+0) 
term(1223) = term(1223) * (2.0d+0) 
term(1224) = term(1224) * (0.5d+0) 
term(1225) = term(1225) * (2.0d+0) 
term(1226) = term(1226) * (-2.0d+0) 
term(1227) = term(1227) * (2.0d+0) 
term(1228) = term(1228) * (-2.0d+0) 
term(1229) = term(1229) * (-4.0d+0) 
term(1230) = term(1230) * (4.0d+0) 
term(1231) = term(1231) * (2.0d+0) 
term(1232) = term(1232) * (-2.0d+0) 
term(1233) = term(1233) * (2.0d+0) 
term(1234) = term(1234) * (-2.0d+0) 
term(1236) = term(1236) * (-2.0d+0) 
term(1240) = term(1240) * (-2.0d+0) 
term(1243) = term(1243) * (-2.0d+0) 
term(1245) = term(1245) * (-2.0d+0) 
term(1247) = term(1247) * (-2.0d+0) 
term(1248) = term(1248) * (-2.0d+0) 
term(1249) = term(1249) * (4.0d+0) 
term(1250) = term(1250) * (2.0d+0) 
term(1251) = term(1251) * (-2.0d+0) 
term(1252) = term(1252) * (2.0d+0) 
term(1253) = term(1253) * (-4.0d+0) 
term(1254) = term(1254) * (-2.0d+0) 
term(1255) = term(1255) * (4.0d+0) 
term(1257) = term(1257) * (-2.0d+0) 
term(1260) = term(1260) * (-2.0d+0) 
term(1261) = term(1261) * (-2.0d+0) 
term(1262) = term(1262) * (4.0d+0) 
term(1264) = term(1264) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1265) = term(1265) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_10_triplet_pt2(b,i,j,k)
term(1266) = term(1266) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_11_triplet_pt2(b,i,j,k)
term(1267) = term(1267) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_9_triplet_pt2(b,i,j,k)
term(1268) = term(1268) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_0_triplet_pt2(b,i,j,k)
term(1269) = term(1269) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_3_triplet_pt2(b,i,j,k)
term(1270) = term(1270) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_4_triplet_pt2(b,i,j,k)
term(1271) = term(1271) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_50_triplet_pt2(b,i,j,k)
term(1272) = term(1272) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_51_triplet_pt2(b,i,j,k)
term(1273) = term(1273) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_54_triplet_pt2(b,i,j,k)
term(1274) = term(1274) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_49_triplet_pt2(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(1265) = term(1265) * (3.0d+0) 
term(1266) = term(1266) * (-4.0d+0) 
term(1267) = term(1267) * (3.0d+0) 
term(1268) = term(1268) * (-4.0d+0) 
term(1269) = term(1269) * (3.0d+0) 
term(1270) = term(1270) * (-2.0d+0) 
term(1271) = term(1271) * (12.0d+0) 
term(1272) = term(1272) * (-12.0d+0) 
term(1273) = term(1273) * (6.0d+0) 
term(1274) = term(1274) * (-8.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1275) = term(1275) + wm_interm_25_triplet_pt2(i,j,k,l) * wm_interm_26_triplet_pt2(p,i,j,q,l,k)
term(1276) = term(1276) + wm_interm_25_triplet_pt2(i,j,k,l) * wm_interm_28_triplet_pt2(p,i,j,q,l,k)
term(1277) = term(1277) + wm_interm_25_triplet_pt2(i,j,k,l) * wm_interm_27_triplet_pt2(p,i,j,q,l,k)
term(1278) = term(1278) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_109_triplet_pt2(p,i,j,q,l,k)
term(1279) = term(1279) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_109_triplet_pt2(p,q,i,j,l,k)
term(1280) = term(1280) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,i,q,j,l,k)
term(1281) = term(1281) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,q,i,j,l,k)
term(1282) = term(1282) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,i,q,j,l,k)
term(1283) = term(1283) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,q,i,j,l,k)
term(1284) = term(1284) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,i,j,q,l,k)
end do 
end do 
end do 
end do 

term(1275) = term(1275) * (3.0d+0) 
term(1276) = term(1276) * (-4.0d+0) 
term(1277) = term(1277) * (-4.0d+0) 
term(1278) = term(1278) * (0.5d+0) 
term(1279) = term(1279) * (0.5d+0) 
term(1280) = term(1280) * (0.5d+0) 
term(1281) = term(1281) * (-1.0d+0) 
term(1282) = term(1282) * (0.5d+0) 
term(1283) = term(1283) * (-1.0d+0) 
term(1284) = term(1284) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(1285) = term(1285) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_57_triplet_pt2(p,k,l,q,i,j)
term(1286) = term(1286) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_96_triplet_pt2(p,k,q,l,i,j)
term(1287) = term(1287) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_97_triplet_pt2(p,k,q,l,i,j)
term(1288) = term(1288) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_96_triplet_pt2(p,q,k,l,i,j)
term(1289) = term(1289) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_97_triplet_pt2(p,q,k,l,i,j)
term(1290) = term(1290) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_99_triplet_pt2(p,q,k,l,i,j)
term(1291) = term(1291) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_96_triplet_pt2(p,k,l,q,i,j)
end do 
end do 
end do 
end do 

term(1285) = term(1285) * (6.0d+0) 
term(1286) = term(1286) * (-1.0d+0) 
term(1287) = term(1287) * (0.5d+0) 
term(1288) = term(1288) * (2.0d+0) 
term(1289) = term(1289) * (-1.0d+0) 
term(1290) = term(1290) * (-1.0d+0) 
term(1291) = term(1291) * (0.5d+0) 

do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(1292) = term(1292) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_57_triplet_pt2(p,k,l,i,q,j)
term(1293) = term(1293) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_57_triplet_pt2(p,k,l,i,j,q)
end do 
end do 
end do 
end do 

term(1292) = term(1292) * (-4.0d+0) 
term(1293) = term(1293) * (8.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(1294) = term(1294) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_57_triplet_pt2(p,k,l,j,q,i)
term(1295) = term(1295) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_57_triplet_pt2(p,k,l,j,i,q)
end do 
end do 
end do 
end do 

term(1294) = term(1294) * (6.0d+0) 
term(1295) = term(1295) * (-8.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1296) = term(1296) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,q,i,l,k)
term(1297) = term(1297) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_109_triplet_pt2(b,j,q,i,l,k)
term(1298) = term(1298) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,j,i,l,k)
term(1299) = term(1299) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,q,j,i,l,k)
term(1300) = term(1300) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,j,q,i,l,k)
term(1301) = term(1301) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_112_triplet_pt2(b,j,q,i,l,k)
term(1302) = term(1302) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,l,k) * wm_interm_115_triplet_pt2(b,q,j,i,l,k)
term(1303) = term(1303) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,j,q,i,l,k)
term(1304) = term(1304) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_109_triplet_pt2(a,q,j,i,l,k)
term(1305) = term(1305) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,q,j,i,l,k)
term(1306) = term(1306) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_112_triplet_pt2(a,j,q,i,l,k)
term(1307) = term(1307) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_115_triplet_pt2(a,q,j,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(1296) = term(1296) * (-1.0d+0) 
term(1297) = term(1297) * (0.5d+0) 
term(1298) = term(1298) * (0.5d+0) 
term(1299) = term(1299) * (0.5d+0) 
term(1300) = term(1300) * (0.5d+0) 
term(1301) = term(1301) * (-1.0d+0) 
term(1302) = term(1302) * (0.5d+0) 
term(1303) = term(1303) * (-2.0d+0) 
term(1304) = term(1304) * (2.0d+0) 
term(1305) = term(1305) * (-2.0d+0) 
term(1306) = term(1306) * (2.0d+0) 
term(1307) = term(1307) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(1308) = term(1308) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,k,j,i)
term(1309) = term(1309) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,j,k,i)
term(1310) = term(1310) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_22_triplet_pt2(b,j,k,i)
term(1311) = term(1311) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_21_triplet_pt2(b,j,k,i)
term(1312) = term(1312) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,k,j,i)
term(1313) = term(1313) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(1308) = term(1308) * (3.0d+0) 
term(1309) = term(1309) * (-2.0d+0) 
term(1310) = term(1310) * (3.0d+0) 
term(1311) = term(1311) * (-4.0d+0) 
term(1312) = term(1312) * (-4.0d+0) 
term(1313) = term(1313) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(1314) = term(1314) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,a,i,q)
term(1315) = term(1315) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(1316) = term(1316) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,a,i,q)
term(1317) = term(1317) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(1318) = term(1318) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,a,i,q)
term(1319) = term(1319) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(1320) = term(1320) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(1321) = term(1321) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,a,i,q)
term(1322) = term(1322) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,a,i,q)
term(1323) = term(1323) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(1324) = term(1324) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,a,i,q)
term(1325) = term(1325) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(1326) = term(1326) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,a,i,q)
term(1327) = term(1327) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(1328) = term(1328) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(1329) = term(1329) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,a,i,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(1314) = term(1314) * (3.0d+0) 
term(1315) = term(1315) * (-4.0d+0) 
term(1316) = term(1316) * (-6.0d+0) 
term(1317) = term(1317) * (8.0d+0) 
term(1318) = term(1318) * (3.0d+0) 
term(1319) = term(1319) * (-4.0d+0) 
term(1320) = term(1320) * (3.0d+0) 
term(1321) = term(1321) * (-4.0d+0) 
term(1322) = term(1322) * (6.0d+0) 
term(1323) = term(1323) * (-8.0d+0) 
term(1324) = term(1324) * (-12.0d+0) 
term(1325) = term(1325) * (16.0d+0) 
term(1326) = term(1326) * (6.0d+0) 
term(1327) = term(1327) * (-8.0d+0) 
term(1328) = term(1328) * (6.0d+0) 
term(1329) = term(1329) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1330) = term(1330) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,q,k,l,j,i)
term(1331) = term(1331) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,k,q,l,j,i)
term(1332) = term(1332) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_99_triplet_pt2(b,k,q,l,j,i)
term(1333) = term(1333) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,k,q,l,j,i)
term(1334) = term(1334) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_100_triplet_pt2(b,q,k,l,j,i)
term(1335) = term(1335) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,q,k,l,j,i)
term(1336) = term(1336) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,q,k,l,j,i)
term(1337) = term(1337) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,k,q,l,j,i)
term(1338) = term(1338) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,k,q,l,j,i)
term(1339) = term(1339) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,k,q,l,j,i)
term(1340) = term(1340) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,q,k,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(1330) = term(1330) * (0.5d+0) 
term(1331) = term(1331) * (-1.0d+0) 
term(1332) = term(1332) * (0.5d+0) 
term(1333) = term(1333) * (0.5d+0) 
term(1334) = term(1334) * (0.5d+0) 
term(1335) = term(1335) * (-1.0d+0) 
term(1336) = term(1336) * (2.0d+0) 
term(1337) = term(1337) * (-4.0d+0) 
term(1338) = term(1338) * (2.0d+0) 
term(1339) = term(1339) * (-2.0d+0) 
term(1340) = term(1340) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1341) = term(1341) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,q,k,l,i,j)
term(1342) = term(1342) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_97_triplet_pt2(b,q,k,l,i,j)
term(1343) = term(1343) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_96_triplet_pt2(b,k,q,l,i,j)
term(1344) = term(1344) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_97_triplet_pt2(b,k,q,l,i,j)
term(1345) = term(1345) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_99_triplet_pt2(b,k,q,l,i,j)
term(1346) = term(1346) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_100_triplet_pt2(b,k,q,l,i,j)
term(1347) = term(1347) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,k,q,l,i,j)
term(1348) = term(1348) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_102_triplet_pt2(b,k,q,l,i,j)
term(1349) = term(1349) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_101_triplet_pt2(b,q,k,l,i,j)
term(1350) = term(1350) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,q,k,l,i,j)
term(1351) = term(1351) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_132_triplet_pt2(b,k,q,l,i,j)
term(1352) = term(1352) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_133_triplet_pt2(b,k,q,l,i,j)
term(1353) = term(1353) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,k,q,l,i,j)
term(1354) = term(1354) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_134_triplet_pt2(b,q,k,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(1341) = term(1341) * (-1.0d+0) 
term(1342) = term(1342) * (0.5d+0) 
term(1343) = term(1343) * (2.0d+0) 
term(1344) = term(1344) * (-1.0d+0) 
term(1345) = term(1345) * (-1.0d+0) 
term(1346) = term(1346) * (0.5d+0) 
term(1347) = term(1347) * (-1.0d+0) 
term(1348) = term(1348) * (0.5d+0) 
term(1349) = term(1349) * (0.5d+0) 
term(1350) = term(1350) * (-2.0d+0) 
term(1351) = term(1351) * (4.0d+0) 
term(1352) = term(1352) * (-2.0d+0) 
term(1353) = term(1353) * (2.0d+0) 
term(1354) = term(1354) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1355) = term(1355) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(1356) = term(1356) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(1357) = term(1357) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(1358) = term(1358) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(1359) = term(1359) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(1360) = term(1360) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(1361) = term(1361) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(1362) = term(1362) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(1363) = term(1363) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(1364) = term(1364) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(1365) = term(1365) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(1366) = term(1366) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(1367) = term(1367) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(1368) = term(1368) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(1369) = term(1369) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(1370) = term(1370) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(1371) = term(1371) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(1372) = term(1372) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(1373) = term(1373) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(1374) = term(1374) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(1375) = term(1375) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(1376) = term(1376) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(1377) = term(1377) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_45_triplet_pt2(c,a,k,q)
term(1378) = term(1378) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_46_triplet_pt2(c,a,k,q)
term(1379) = term(1379) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
term(1380) = term(1380) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(1381) = term(1381) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(1382) = term(1382) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(1383) = term(1383) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,a,k,q)
term(1384) = term(1384) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(1385) = term(1385) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(1386) = term(1386) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_47_triplet_pt2(c,a,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(1355) = term(1355) * (3.0d+0) 
term(1356) = term(1356) * (3.0d+0) 
term(1357) = term(1357) * (-4.0d+0) 
term(1358) = term(1358) * (-6.0d+0) 
term(1359) = term(1359) * (-6.0d+0) 
term(1360) = term(1360) * (8.0d+0) 
term(1361) = term(1361) * (-2.0d+0) 
term(1362) = term(1362) * (4.0d+0) 
term(1363) = term(1363) * (3.0d+0) 
term(1364) = term(1364) * (3.0d+0) 
term(1365) = term(1365) * (-4.0d+0) 
term(1366) = term(1366) * (3.0d+0) 
term(1367) = term(1367) * (-2.0d+0) 
term(1368) = term(1368) * (3.0d+0) 
term(1369) = term(1369) * (-4.0d+0) 
term(1370) = term(1370) * (-2.0d+0) 
term(1371) = term(1371) * (6.0d+0) 
term(1372) = term(1372) * (6.0d+0) 
term(1373) = term(1373) * (-8.0d+0) 
term(1374) = term(1374) * (-12.0d+0) 
term(1375) = term(1375) * (-12.0d+0) 
term(1376) = term(1376) * (16.0d+0) 
term(1377) = term(1377) * (-4.0d+0) 
term(1378) = term(1378) * (8.0d+0) 
term(1379) = term(1379) * (6.0d+0) 
term(1380) = term(1380) * (6.0d+0) 
term(1381) = term(1381) * (-8.0d+0) 
term(1382) = term(1382) * (6.0d+0) 
term(1383) = term(1383) * (-4.0d+0) 
term(1384) = term(1384) * (6.0d+0) 
term(1385) = term(1385) * (-8.0d+0) 
term(1386) = term(1386) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(1387) = term(1387) + wm_interm_5_triplet_pt2(p,a) * wm_interm_74_triplet_pt2(a,q)
term(1388) = term(1388) + wm_interm_74_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1389) = term(1389) + wm_interm_5_triplet_pt2(p,a) * wm_interm_70_triplet_pt2(a,q)
term(1390) = term(1390) + wm_interm_5_triplet_pt2(p,a) * wm_interm_71_triplet_pt2(a,q)
term(1391) = term(1391) + wm_interm_70_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1392) = term(1392) + wm_interm_71_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1393) = term(1393) + wm_interm_5_triplet_pt2(p,a) * wm_interm_79_triplet_pt2(a,q)
term(1394) = term(1394) + wm_interm_79_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1395) = term(1395) + wm_interm_5_triplet_pt2(p,a) * wm_interm_80_triplet_pt2(a,q)
term(1396) = term(1396) + wm_interm_7_triplet_pt2(p,a) * wm_interm_80_triplet_pt2(a,q)
term(1397) = term(1397) + wm_interm_5_triplet_pt2(p,a) * wm_interm_81_triplet_pt2(a,q)
term(1398) = term(1398) + wm_interm_7_triplet_pt2(p,a) * wm_interm_81_triplet_pt2(a,q)
term(1399) = term(1399) + wm_interm_5_triplet_pt2(p,a) * wm_interm_83_triplet_pt2(a,q)
term(1400) = term(1400) + wm_interm_7_triplet_pt2(p,a) * wm_interm_83_triplet_pt2(a,q)
term(1401) = term(1401) + wm_interm_5_triplet_pt2(p,a) * wm_interm_92_triplet_pt2(a,q)
term(1402) = term(1402) + wm_interm_7_triplet_pt2(p,a) * wm_interm_92_triplet_pt2(a,q)
term(1403) = term(1403) + wm_interm_118_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(1404) = term(1404) + wm_interm_118_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1405) = term(1405) + wm_interm_119_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(1406) = term(1406) + wm_interm_119_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1407) = term(1407) + wm_interm_124_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(1408) = term(1408) + wm_interm_124_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1409) = term(1409) + wm_interm_125_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(1410) = term(1410) + wm_interm_125_triplet_pt2(a,q) * wm_interm_7_triplet_pt2(p,a)
term(1411) = term(1411) + wm_interm_139_triplet_pt2(a,q) * wm_interm_98_triplet_pt2(a,p)
term(1412) = term(1412) + wm_interm_140_triplet_pt2(a,q) * wm_interm_98_triplet_pt2(a,p)
term(1413) = term(1413) + wm_interm_139_triplet_pt2(a,q) * wm_interm_94_triplet_pt2(a,p)
term(1414) = term(1414) + wm_interm_140_triplet_pt2(a,q) * wm_interm_94_triplet_pt2(a,p)
term(1415) = term(1415) + wm_interm_139_triplet_pt2(a,q) * wm_interm_95_triplet_pt2(a,p)
term(1416) = term(1416) + wm_interm_140_triplet_pt2(a,q) * wm_interm_95_triplet_pt2(a,p)
term(1417) = term(1417) + wm_interm_141_triplet_pt2(a,q) * wm_interm_98_triplet_pt2(a,p)
term(1418) = term(1418) + wm_interm_141_triplet_pt2(a,q) * wm_interm_94_triplet_pt2(a,p)
term(1419) = term(1419) + wm_interm_141_triplet_pt2(a,q) * wm_interm_95_triplet_pt2(a,p)
term(1420) = term(1420) + wm_interm_142_triplet_pt2(a,q) * wm_interm_98_triplet_pt2(a,p)
term(1421) = term(1421) + wm_interm_142_triplet_pt2(a,q) * wm_interm_94_triplet_pt2(a,p)
term(1422) = term(1422) + wm_interm_142_triplet_pt2(a,q) * wm_interm_95_triplet_pt2(a,p)
term(1423) = term(1423) + wm_interm_130_triplet_pt2(a,p) * wm_interm_139_triplet_pt2(a,q)
term(1424) = term(1424) + wm_interm_130_triplet_pt2(a,p) * wm_interm_140_triplet_pt2(a,q)
term(1425) = term(1425) + wm_interm_131_triplet_pt2(a,p) * wm_interm_139_triplet_pt2(a,q)
term(1426) = term(1426) + wm_interm_131_triplet_pt2(a,p) * wm_interm_140_triplet_pt2(a,q)
term(1427) = term(1427) + wm_interm_130_triplet_pt2(a,p) * wm_interm_141_triplet_pt2(a,q)
term(1428) = term(1428) + wm_interm_131_triplet_pt2(a,p) * wm_interm_141_triplet_pt2(a,q)
term(1429) = term(1429) + wm_interm_130_triplet_pt2(a,p) * wm_interm_142_triplet_pt2(a,q)
term(1430) = term(1430) + wm_interm_131_triplet_pt2(a,p) * wm_interm_142_triplet_pt2(a,q)
end do 

term(1387) = term(1387) * (3.0d+0) 
term(1388) = term(1388) * (-6.0d+0) 
term(1389) = term(1389) * (3.0d+0) 
term(1390) = term(1390) * (-2.0d+0) 
term(1391) = term(1391) * (-6.0d+0) 
term(1392) = term(1392) * (4.0d+0) 
term(1393) = term(1393) * (-2.0d+0) 
term(1394) = term(1394) * (4.0d+0) 
term(1395) = term(1395) * (-2.0d+0) 
term(1396) = term(1396) * (4.0d+0) 
term(1397) = term(1397) * (-4.0d+0) 
term(1398) = term(1398) * (8.0d+0) 
term(1399) = term(1399) * (2.0d+0) 
term(1400) = term(1400) * (-4.0d+0) 
term(1401) = term(1401) * (2.0d+0) 
term(1402) = term(1402) * (-4.0d+0) 
term(1403) = term(1403) * (12.0d+0) 
term(1404) = term(1404) * (-24.0d+0) 
term(1405) = term(1405) * (-12.0d+0) 
term(1406) = term(1406) * (24.0d+0) 
term(1407) = term(1407) * (-8.0d+0) 
term(1408) = term(1408) * (16.0d+0) 
term(1409) = term(1409) * (8.0d+0) 
term(1410) = term(1410) * (-16.0d+0) 
term(1411) = term(1411) * (-4.0d+0) 
term(1412) = term(1412) * (2.0d+0) 
term(1413) = term(1413) * (-4.0d+0) 
term(1414) = term(1414) * (2.0d+0) 
term(1415) = term(1415) * (8.0d+0) 
term(1416) = term(1416) * (-4.0d+0) 
term(1417) = term(1417) * (3.0d+0) 
term(1418) = term(1418) * (3.0d+0) 
term(1419) = term(1419) * (-6.0d+0) 
term(1420) = term(1420) * (-1.0d+0) 
term(1421) = term(1421) * (-1.0d+0) 
term(1422) = term(1422) * (2.0d+0) 
term(1423) = term(1423) * (-16.0d+0) 
term(1424) = term(1424) * (8.0d+0) 
term(1425) = term(1425) * (16.0d+0) 
term(1426) = term(1426) * (-8.0d+0) 
term(1427) = term(1427) * (12.0d+0) 
term(1428) = term(1428) * (-12.0d+0) 
term(1429) = term(1429) * (-4.0d+0) 
term(1430) = term(1430) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(1431) = term(1431) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_57_triplet_pt2(p,k,l,q,j,i)
term(1432) = term(1432) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_96_triplet_pt2(p,k,q,l,j,i)
term(1433) = term(1433) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_96_triplet_pt2(p,q,k,l,j,i)
term(1434) = term(1434) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_99_triplet_pt2(p,q,k,l,j,i)
term(1435) = term(1435) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_96_triplet_pt2(p,k,l,q,j,i)
term(1436) = term(1436) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_97_triplet_pt2(p,k,l,q,j,i)
term(1437) = term(1437) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_99_triplet_pt2(p,k,l,q,j,i)
end do 
end do 
end do 
end do 

term(1431) = term(1431) * (-8.0d+0) 
term(1432) = term(1432) * (0.5d+0) 
term(1433) = term(1433) * (-1.0d+0) 
term(1434) = term(1434) * (0.5d+0) 
term(1435) = term(1435) * (-1.0d+0) 
term(1436) = term(1436) * (0.5d+0) 
term(1437) = term(1437) * (0.5d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1438) = term(1438) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_109_triplet_pt2(p,j,i,q,l,k)
term(1439) = term(1439) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,j,i,q,l,k)
term(1440) = term(1440) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,j,q,i,l,k)
term(1441) = term(1441) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,q,j,i,l,k)
term(1442) = term(1442) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,j,i,q,l,k)
end do 
end do 
end do 
end do 

term(1438) = term(1438) * (-1.0d+0) 
term(1439) = term(1439) * (0.5d+0) 
term(1440) = term(1440) * (-1.0d+0) 
term(1441) = term(1441) * (0.5d+0) 
term(1442) = term(1442) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1443) = term(1443) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_109_triplet_pt2(p,j,i,q,k,l)
term(1444) = term(1444) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_109_triplet_pt2(p,q,j,i,k,l)
term(1445) = term(1445) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,j,i,q,k,l)
term(1446) = term(1446) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,j,q,i,k,l)
term(1447) = term(1447) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_112_triplet_pt2(p,q,j,i,k,l)
term(1448) = term(1448) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,j,q,i,k,l)
term(1449) = term(1449) + wm_interm_107_triplet_pt2(i,j,k,l) * wm_interm_115_triplet_pt2(p,j,i,q,k,l)
end do 
end do 
end do 
end do 

term(1443) = term(1443) * (0.5d+0) 
term(1444) = term(1444) * (0.5d+0) 
term(1445) = term(1445) * (-1.0d+0) 
term(1446) = term(1446) * (0.5d+0) 
term(1447) = term(1447) * (-1.0d+0) 
term(1448) = term(1448) * (0.5d+0) 
term(1449) = term(1449) * (-1.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1450) = term(1450) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,b,i,q)
term(1451) = term(1451) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(1452) = term(1452) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_33_triplet_pt2(a,c,j,q)
term(1453) = term(1453) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_29_triplet_pt2(a,c,j,q)
term(1454) = term(1454) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_13_triplet_pt2(a,c,j,q)
term(1455) = term(1455) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_17_triplet_pt2(a,c,j,q)
term(1456) = term(1456) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,b,i,q)
term(1457) = term(1457) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_33_triplet_pt2(a,c,i,q)
term(1458) = term(1458) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_29_triplet_pt2(a,c,k,q)
term(1459) = term(1459) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_29_triplet_pt2(a,c,i,q)
term(1460) = term(1460) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_13_triplet_pt2(a,c,k,q)
term(1461) = term(1461) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_17_triplet_pt2(a,c,i,q)
term(1462) = term(1462) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_17_triplet_pt2(a,c,k,q)
term(1463) = term(1463) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_29_triplet_pt2(a,c,j,q)
term(1464) = term(1464) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_13_triplet_pt2(a,c,j,q)
term(1465) = term(1465) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_17_triplet_pt2(a,c,j,q)
term(1466) = term(1466) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(1467) = term(1467) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_33_triplet_pt2(a,c,k,q)
term(1468) = term(1468) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_33_triplet_pt2(a,c,j,q)
term(1469) = term(1469) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_13_triplet_pt2(a,c,i,q)
term(1470) = term(1470) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,b,i,q)
term(1471) = term(1471) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(1472) = term(1472) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(1473) = term(1473) + r2p(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,b,i,q)
term(1474) = term(1474) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_12_triplet_pt2(a,c,j,q)
term(1475) = term(1475) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_8_triplet_pt2(a,c,j,q)
term(1476) = term(1476) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_6_triplet_pt2(a,c,j,q)
term(1477) = term(1477) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_12_triplet_pt2(a,c,i,q)
term(1478) = term(1478) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_12_triplet_pt2(a,c,k,q)
term(1479) = term(1479) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_8_triplet_pt2(a,c,i,q)
term(1480) = term(1480) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_8_triplet_pt2(a,c,k,q)
term(1481) = term(1481) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_6_triplet_pt2(a,c,i,q)
term(1482) = term(1482) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_6_triplet_pt2(a,c,k,q)
term(1483) = term(1483) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_12_triplet_pt2(a,c,j,q)
term(1484) = term(1484) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_8_triplet_pt2(a,c,j,q)
term(1485) = term(1485) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_6_triplet_pt2(a,c,j,q)
term(1486) = term(1486) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,b,i,q)
term(1487) = term(1487) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_45_triplet_pt2(c,b,k,q)
term(1488) = term(1488) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_58_triplet_pt2(a,c,j,q)
term(1489) = term(1489) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_55_triplet_pt2(a,c,j,q)
term(1490) = term(1490) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,b,i,q)
term(1491) = term(1491) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_58_triplet_pt2(a,c,i,q)
term(1492) = term(1492) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_55_triplet_pt2(a,c,k,q)
term(1493) = term(1493) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_55_triplet_pt2(a,c,i,q)
term(1494) = term(1494) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_58_triplet_pt2(a,c,k,q)
term(1495) = term(1495) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_55_triplet_pt2(a,c,j,q)
term(1496) = term(1496) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_58_triplet_pt2(a,c,j,q)
term(1497) = term(1497) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_46_triplet_pt2(c,b,k,q)
term(1498) = term(1498) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,b,i,q)
term(1499) = term(1499) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_47_triplet_pt2(c,b,k,q)
term(1500) = term(1500) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,b,k,q)
term(1501) = term(1501) + r2m(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_48_triplet_pt2(c,b,i,q)
term(1502) = term(1502) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_53_triplet_pt2(a,c,j,q)
term(1503) = term(1503) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_52_triplet_pt2(a,c,j,q)
term(1504) = term(1504) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_53_triplet_pt2(a,c,i,q)
term(1505) = term(1505) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_53_triplet_pt2(a,c,k,q)
term(1506) = term(1506) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_52_triplet_pt2(a,c,i,q)
term(1507) = term(1507) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_52_triplet_pt2(a,c,k,q)
term(1508) = term(1508) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_53_triplet_pt2(a,c,j,q)
term(1509) = term(1509) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_52_triplet_pt2(a,c,j,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(1450) = term(1450) * (-2.0d+0) 
term(1451) = term(1451) * (4.0d+0) 
term(1452) = term(1452) * (-4.0d+0) 
term(1453) = term(1453) * (3.0d+0) 
term(1454) = term(1454) * (3.0d+0) 
term(1455) = term(1455) * (-2.0d+0) 
term(1456) = term(1456) * (4.0d+0) 
term(1457) = term(1457) * (3.0d+0) 
term(1458) = term(1458) * (3.0d+0) 
term(1459) = term(1459) * (-4.0d+0) 
term(1460) = term(1460) * (3.0d+0) 
term(1461) = term(1461) * (3.0d+0) 
term(1462) = term(1462) * (-4.0d+0) 
term(1463) = term(1463) * (-4.0d+0) 
term(1464) = term(1464) * (-4.0d+0) 
term(1465) = term(1465) * (4.0d+0) 
term(1466) = term(1466) * (-8.0d+0) 
term(1467) = term(1467) * (-2.0d+0) 
term(1468) = term(1468) * (4.0d+0) 
term(1469) = term(1469) * (-2.0d+0) 
term(1470) = term(1470) * (-2.0d+0) 
term(1471) = term(1471) * (4.0d+0) 
term(1472) = term(1472) * (-2.0d+0) 
term(1473) = term(1473) * (4.0d+0) 
term(1474) = term(1474) * (-2.0d+0) 
term(1475) = term(1475) * (-2.0d+0) 
term(1476) = term(1476) * (4.0d+0) 
term(1477) = term(1477) * (3.0d+0) 
term(1478) = term(1478) * (-4.0d+0) 
term(1479) = term(1479) * (3.0d+0) 
term(1480) = term(1480) * (-4.0d+0) 
term(1481) = term(1481) * (-6.0d+0) 
term(1482) = term(1482) * (8.0d+0) 
term(1483) = term(1483) * (4.0d+0) 
term(1484) = term(1484) * (4.0d+0) 
term(1485) = term(1485) * (-8.0d+0) 
term(1486) = term(1486) * (-4.0d+0) 
term(1487) = term(1487) * (8.0d+0) 
term(1488) = term(1488) * (-12.0d+0) 
term(1489) = term(1489) * (12.0d+0) 
term(1490) = term(1490) * (8.0d+0) 
term(1491) = term(1491) * (12.0d+0) 
term(1492) = term(1492) * (12.0d+0) 
term(1493) = term(1493) * (-12.0d+0) 
term(1494) = term(1494) * (-12.0d+0) 
term(1495) = term(1495) * (-16.0d+0) 
term(1496) = term(1496) * (16.0d+0) 
term(1497) = term(1497) * (-16.0d+0) 
term(1498) = term(1498) * (-4.0d+0) 
term(1499) = term(1499) * (8.0d+0) 
term(1500) = term(1500) * (-4.0d+0) 
term(1501) = term(1501) * (8.0d+0) 
term(1502) = term(1502) * (-8.0d+0) 
term(1503) = term(1503) * (8.0d+0) 
term(1504) = term(1504) * (12.0d+0) 
term(1505) = term(1505) * (-16.0d+0) 
term(1506) = term(1506) * (-12.0d+0) 
term(1507) = term(1507) * (16.0d+0) 
term(1508) = term(1508) * (16.0d+0) 
term(1509) = term(1509) * (-16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1510) = term(1510) + wm_interm_10_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(1511) = term(1511) + wm_interm_11_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(1512) = term(1512) + wm_interm_10_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(1513) = term(1513) + wm_interm_11_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(1514) = term(1514) + wm_interm_0_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(1515) = term(1515) + wm_interm_0_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(1516) = term(1516) + wm_interm_21_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,k,q)
term(1517) = term(1517) + wm_interm_22_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,k,q)
term(1518) = term(1518) + wm_interm_22_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,q,k)
term(1519) = term(1519) + wm_interm_23_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,q,k)
term(1520) = term(1520) + wm_interm_19_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,k,q)
term(1521) = term(1521) + wm_interm_23_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,k,q)
term(1522) = term(1522) + wm_interm_24_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,q,k)
term(1523) = term(1523) + wm_interm_20_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,k,q)
term(1524) = term(1524) + wm_interm_24_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,k,q)
term(1525) = term(1525) + wm_interm_21_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,q,k)
term(1526) = term(1526) + wm_interm_19_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,q,k)
term(1527) = term(1527) + wm_interm_20_triplet_pt2(p,i,j,k) * wm_interm_25_triplet_pt2(i,j,q,k)
term(1528) = term(1528) + wm_interm_21_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,k,q)
term(1529) = term(1529) + wm_interm_22_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,k,q)
term(1530) = term(1530) + wm_interm_22_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,q,k)
term(1531) = term(1531) + wm_interm_23_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,q,k)
term(1532) = term(1532) + wm_interm_19_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,k,q)
term(1533) = term(1533) + wm_interm_23_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,k,q)
term(1534) = term(1534) + wm_interm_24_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,q,k)
term(1535) = term(1535) + wm_interm_20_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,k,q)
term(1536) = term(1536) + wm_interm_24_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,k,q)
term(1537) = term(1537) + wm_interm_21_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,q,k)
term(1538) = term(1538) + wm_interm_19_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,q,k)
term(1539) = term(1539) + wm_interm_20_triplet_pt2(p,i,j,k) * wm_interm_60_triplet_pt2(i,j,q,k)
end do 
end do 
end do 

term(1510) = term(1510) * (3.0d+0) 
term(1511) = term(1511) * (-4.0d+0) 
term(1512) = term(1512) * (-4.0d+0) 
term(1513) = term(1513) * (4.0d+0) 
term(1514) = term(1514) * (3.0d+0) 
term(1515) = term(1515) * (-4.0d+0) 
term(1516) = term(1516) * (3.0d+0) 
term(1517) = term(1517) * (-2.0d+0) 
term(1518) = term(1518) * (3.0d+0) 
term(1519) = term(1519) * (3.0d+0) 
term(1520) = term(1520) * (3.0d+0) 
term(1521) = term(1521) * (-4.0d+0) 
term(1522) = term(1522) * (-4.0d+0) 
term(1523) = term(1523) * (-4.0d+0) 
term(1524) = term(1524) * (4.0d+0) 
term(1525) = term(1525) * (-4.0d+0) 
term(1526) = term(1526) * (-2.0d+0) 
term(1527) = term(1527) * (4.0d+0) 
term(1528) = term(1528) * (6.0d+0) 
term(1529) = term(1529) * (-4.0d+0) 
term(1530) = term(1530) * (6.0d+0) 
term(1531) = term(1531) * (6.0d+0) 
term(1532) = term(1532) * (6.0d+0) 
term(1533) = term(1533) * (-8.0d+0) 
term(1534) = term(1534) * (-8.0d+0) 
term(1535) = term(1535) * (-8.0d+0) 
term(1536) = term(1536) * (8.0d+0) 
term(1537) = term(1537) * (-8.0d+0) 
term(1538) = term(1538) * (-4.0d+0) 
term(1539) = term(1539) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1540) = term(1540) + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(c,b)
term(1541) = term(1541) + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(c,b)
term(1542) = term(1542) + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(c,b)
term(1543) = term(1543) + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(c,b)
end do 
end do 
end do 
end do 
end do 

term(1540) = term(1540) * (-2.0d+0) 
term(1541) = term(1541) * (4.0d+0) 
term(1542) = term(1542) * (3.0d+0) 
term(1543) = term(1543) * (-6.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1544) = term(1544) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_9_triplet_pt2(p,k,i,j)
term(1545) = term(1545) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_3_triplet_pt2(p,k,i,j)
term(1546) = term(1546) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_4_triplet_pt2(p,k,i,j)
term(1547) = term(1547) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_54_triplet_pt2(p,k,i,j)
term(1548) = term(1548) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_50_triplet_pt2(p,k,i,j)
term(1549) = term(1549) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_51_triplet_pt2(p,k,i,j)
term(1550) = term(1550) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_49_triplet_pt2(p,k,i,j)
end do 
end do 
end do 

term(1544) = term(1544) * (-2.0d+0) 
term(1545) = term(1545) * (-4.0d+0) 
term(1546) = term(1546) * (4.0d+0) 
term(1547) = term(1547) * (-4.0d+0) 
term(1548) = term(1548) * (-16.0d+0) 
term(1549) = term(1549) * (16.0d+0) 
term(1550) = term(1550) * (6.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1551) = term(1551) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,j,k,i)
term(1552) = term(1552) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,k,j,i)
term(1553) = term(1553) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_21_triplet_pt2(b,j,k,i)
term(1554) = term(1554) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,j,k,i)
term(1555) = term(1555) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_22_triplet_pt2(b,j,k,i)
term(1556) = term(1556) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,k,j,i)
term(1557) = term(1557) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,j,k,i)
term(1558) = term(1558) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,k,j,i)
term(1559) = term(1559) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_21_triplet_pt2(b,j,k,i)
term(1560) = term(1560) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,j,k,i)
term(1561) = term(1561) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_22_triplet_pt2(b,j,k,i)
term(1562) = term(1562) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt2(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(1551) = term(1551) * (3.0d+0) 
term(1552) = term(1552) * (-4.0d+0) 
term(1553) = term(1553) * (3.0d+0) 
term(1554) = term(1554) * (-4.0d+0) 
term(1555) = term(1555) * (-2.0d+0) 
term(1556) = term(1556) * (4.0d+0) 
term(1557) = term(1557) * (6.0d+0) 
term(1558) = term(1558) * (-8.0d+0) 
term(1559) = term(1559) * (6.0d+0) 
term(1560) = term(1560) * (-8.0d+0) 
term(1561) = term(1561) * (-4.0d+0) 
term(1562) = term(1562) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(1563) = term(1563) + wm_interm_1_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,q,i)
term(1564) = term(1564) + wm_interm_1_triplet_pt2(i,j) * wm_interm_4_triplet_pt2(p,j,q,i)
term(1565) = term(1565) + wm_interm_1_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,i,q)
term(1566) = term(1566) + wm_interm_1_triplet_pt2(i,j) * wm_interm_4_triplet_pt2(p,j,i,q)
term(1567) = term(1567) + wm_interm_2_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,q,i)
term(1568) = term(1568) + wm_interm_2_triplet_pt2(i,j) * wm_interm_4_triplet_pt2(p,j,q,i)
term(1569) = term(1569) + wm_interm_2_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,i,q)
term(1570) = term(1570) + wm_interm_2_triplet_pt2(i,j) * wm_interm_4_triplet_pt2(p,j,i,q)
term(1571) = term(1571) + wm_interm_1_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,j,q,i)
term(1572) = term(1572) + wm_interm_2_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,j,q,i)
term(1573) = term(1573) + wm_interm_1_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,j,i,q)
term(1574) = term(1574) + wm_interm_2_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,j,i,q)
term(1575) = term(1575) + wm_interm_1_triplet_pt2(i,j) * wm_interm_49_triplet_pt2(p,j,i,q)
term(1576) = term(1576) + wm_interm_1_triplet_pt2(i,j) * wm_interm_49_triplet_pt2(p,j,q,i)
term(1577) = term(1577) + wm_interm_2_triplet_pt2(i,j) * wm_interm_49_triplet_pt2(p,j,i,q)
term(1578) = term(1578) + wm_interm_2_triplet_pt2(i,j) * wm_interm_49_triplet_pt2(p,j,q,i)
term(1579) = term(1579) + wm_interm_1_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,j,q,i)
term(1580) = term(1580) + wm_interm_1_triplet_pt2(i,j) * wm_interm_51_triplet_pt2(p,j,q,i)
term(1581) = term(1581) + wm_interm_1_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,j,i,q)
term(1582) = term(1582) + wm_interm_1_triplet_pt2(i,j) * wm_interm_51_triplet_pt2(p,j,i,q)
term(1583) = term(1583) + wm_interm_2_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,j,q,i)
term(1584) = term(1584) + wm_interm_2_triplet_pt2(i,j) * wm_interm_51_triplet_pt2(p,j,q,i)
term(1585) = term(1585) + wm_interm_2_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,j,i,q)
term(1586) = term(1586) + wm_interm_2_triplet_pt2(i,j) * wm_interm_51_triplet_pt2(p,j,i,q)
term(1587) = term(1587) + wm_interm_1_triplet_pt2(i,j) * wm_interm_54_triplet_pt2(p,j,q,i)
term(1588) = term(1588) + wm_interm_2_triplet_pt2(i,j) * wm_interm_54_triplet_pt2(p,j,q,i)
term(1589) = term(1589) + wm_interm_1_triplet_pt2(i,j) * wm_interm_54_triplet_pt2(p,j,i,q)
term(1590) = term(1590) + wm_interm_2_triplet_pt2(i,j) * wm_interm_54_triplet_pt2(p,j,i,q)
term(1591) = term(1591) + wm_interm_1_triplet_pt2(i,j) * wm_interm_67_triplet_pt2(p,q,j,i)
term(1592) = term(1592) + wm_interm_1_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,j,i)
term(1593) = term(1593) + wm_interm_2_triplet_pt2(i,j) * wm_interm_67_triplet_pt2(p,q,j,i)
term(1594) = term(1594) + wm_interm_2_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,j,i)
term(1595) = term(1595) + wm_interm_1_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,j,i)
term(1596) = term(1596) + wm_interm_2_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,j,i)
term(1597) = term(1597) + wm_interm_1_triplet_pt2(i,j) * wm_interm_72_triplet_pt2(p,q,j,i)
term(1598) = term(1598) + wm_interm_1_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,j,i)
term(1599) = term(1599) + wm_interm_2_triplet_pt2(i,j) * wm_interm_72_triplet_pt2(p,q,j,i)
term(1600) = term(1600) + wm_interm_2_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,j,i)
term(1601) = term(1601) + wm_interm_1_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,q,j,i)
term(1602) = term(1602) + wm_interm_2_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,q,j,i)
term(1603) = term(1603) + wm_interm_1_triplet_pt2(i,j) * wm_interm_76_triplet_pt2(p,q,j,i)
term(1604) = term(1604) + wm_interm_2_triplet_pt2(i,j) * wm_interm_76_triplet_pt2(p,q,j,i)
term(1605) = term(1605) + wm_interm_1_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(p,q,j,i)
term(1606) = term(1606) + wm_interm_1_triplet_pt2(i,j) * wm_interm_78_triplet_pt2(p,q,j,i)
term(1607) = term(1607) + wm_interm_2_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(p,q,j,i)
term(1608) = term(1608) + wm_interm_2_triplet_pt2(i,j) * wm_interm_78_triplet_pt2(p,q,j,i)
term(1609) = term(1609) + wm_interm_1_triplet_pt2(i,j) * wm_interm_82_triplet_pt2(p,q,j,i)
term(1610) = term(1610) + wm_interm_2_triplet_pt2(i,j) * wm_interm_82_triplet_pt2(p,q,j,i)
term(1611) = term(1611) + wm_interm_1_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,j,q,i)
term(1612) = term(1612) + wm_interm_2_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,j,q,i)
term(1613) = term(1613) + wm_interm_1_triplet_pt2(i,j) * wm_interm_67_triplet_pt2(p,j,q,i)
term(1614) = term(1614) + wm_interm_2_triplet_pt2(i,j) * wm_interm_67_triplet_pt2(p,j,q,i)
term(1615) = term(1615) + wm_interm_1_triplet_pt2(i,j) * wm_interm_90_triplet_pt2(p,q,j,i)
term(1616) = term(1616) + wm_interm_2_triplet_pt2(i,j) * wm_interm_90_triplet_pt2(p,q,j,i)
term(1617) = term(1617) + wm_interm_1_triplet_pt2(i,j) * wm_interm_91_triplet_pt2(p,j,q,i)
term(1618) = term(1618) + wm_interm_2_triplet_pt2(i,j) * wm_interm_91_triplet_pt2(p,j,q,i)
term(1619) = term(1619) + wm_interm_1_triplet_pt2(i,j) * wm_interm_91_triplet_pt2(p,q,j,i)
term(1620) = term(1620) + wm_interm_2_triplet_pt2(i,j) * wm_interm_91_triplet_pt2(p,q,j,i)
term(1621) = term(1621) + wm_interm_1_triplet_pt2(i,j) * wm_interm_72_triplet_pt2(p,j,q,i)
term(1622) = term(1622) + wm_interm_2_triplet_pt2(i,j) * wm_interm_72_triplet_pt2(p,j,q,i)
term(1623) = term(1623) + wm_interm_1_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(p,j,q,i)
term(1624) = term(1624) + wm_interm_2_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(p,j,q,i)
term(1625) = term(1625) + wm_interm_1_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,j,q,i)
term(1626) = term(1626) + wm_interm_2_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,j,q,i)
term(1627) = term(1627) + wm_interm_1_triplet_pt2(i,j) * wm_interm_76_triplet_pt2(p,j,q,i)
term(1628) = term(1628) + wm_interm_2_triplet_pt2(i,j) * wm_interm_76_triplet_pt2(p,j,q,i)
term(1629) = term(1629) + wm_interm_1_triplet_pt2(i,j) * wm_interm_90_triplet_pt2(p,j,q,i)
term(1630) = term(1630) + wm_interm_2_triplet_pt2(i,j) * wm_interm_90_triplet_pt2(p,j,q,i)
term(1631) = term(1631) + wm_interm_1_triplet_pt2(i,j) * wm_interm_82_triplet_pt2(p,j,q,i)
term(1632) = term(1632) + wm_interm_2_triplet_pt2(i,j) * wm_interm_82_triplet_pt2(p,j,q,i)
term(1633) = term(1633) + wm_interm_1_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,j,q,i)
term(1634) = term(1634) + wm_interm_2_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,j,q,i)
term(1635) = term(1635) + wm_interm_1_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,j,q,i)
term(1636) = term(1636) + wm_interm_2_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,j,q,i)
term(1637) = term(1637) + wm_interm_1_triplet_pt2(i,j) * wm_interm_78_triplet_pt2(p,j,q,i)
term(1638) = term(1638) + wm_interm_2_triplet_pt2(i,j) * wm_interm_78_triplet_pt2(p,j,q,i)
end do 
end do 

term(1563) = term(1563) * (3.0d+0) 
term(1564) = term(1564) * (-2.0d+0) 
term(1565) = term(1565) * (-4.0d+0) 
term(1566) = term(1566) * (4.0d+0) 
term(1567) = term(1567) * (-6.0d+0) 
term(1568) = term(1568) * (4.0d+0) 
term(1569) = term(1569) * (8.0d+0) 
term(1570) = term(1570) * (-8.0d+0) 
term(1571) = term(1571) * (3.0d+0) 
term(1572) = term(1572) * (-6.0d+0) 
term(1573) = term(1573) * (-2.0d+0) 
term(1574) = term(1574) * (4.0d+0) 
term(1575) = term(1575) * (6.0d+0) 
term(1576) = term(1576) * (-8.0d+0) 
term(1577) = term(1577) * (-12.0d+0) 
term(1578) = term(1578) * (16.0d+0) 
term(1579) = term(1579) * (12.0d+0) 
term(1580) = term(1580) * (-12.0d+0) 
term(1581) = term(1581) * (-16.0d+0) 
term(1582) = term(1582) * (16.0d+0) 
term(1583) = term(1583) * (-24.0d+0) 
term(1584) = term(1584) * (24.0d+0) 
term(1585) = term(1585) * (32.0d+0) 
term(1586) = term(1586) * (-32.0d+0) 
term(1587) = term(1587) * (6.0d+0) 
term(1588) = term(1588) * (-12.0d+0) 
term(1589) = term(1589) * (-4.0d+0) 
term(1590) = term(1590) * (8.0d+0) 
term(1591) = term(1591) * (1.5d+0) 
term(1592) = term(1592) * (-2.0d+0) 
term(1593) = term(1593) * (-3.0d+0) 
term(1594) = term(1594) * (4.0d+0) 
term(1595) = term(1595) * (1.5d+0) 
term(1596) = term(1596) * (-3.0d+0) 
term(1597) = term(1597) * (-2.0d+0) 
term(1598) = term(1598) * (2.0d+0) 
term(1599) = term(1599) * (4.0d+0) 
term(1600) = term(1600) * (-4.0d+0) 
term(1601) = term(1601) * (-2.0d+0) 
term(1602) = term(1602) * (4.0d+0) 
term(1603) = term(1603) * (1.5d+0) 
term(1604) = term(1604) * (-3.0d+0) 
term(1605) = term(1605) * (1.5d+0) 
term(1606) = term(1606) * (-1.0d+0) 
term(1607) = term(1607) * (-3.0d+0) 
term(1608) = term(1608) * (2.0d+0) 
term(1609) = term(1609) * (-2.0d+0) 
term(1610) = term(1610) * (4.0d+0) 
term(1611) = term(1611) * (1.5d+0) 
term(1612) = term(1612) * (-3.0d+0) 
term(1613) = term(1613) * (-1.0d+0) 
term(1614) = term(1614) * (2.0d+0) 
term(1615) = term(1615) * (2.0d+0) 
term(1616) = term(1616) * (-4.0d+0) 
term(1617) = term(1617) * (1.5d+0) 
term(1618) = term(1618) * (-3.0d+0) 
term(1619) = term(1619) * (-1.0d+0) 
term(1620) = term(1620) * (2.0d+0) 
term(1621) = term(1621) * (1.5d+0) 
term(1622) = term(1622) * (-3.0d+0) 
term(1623) = term(1623) * (-2.0d+0) 
term(1624) = term(1624) * (4.0d+0) 
term(1625) = term(1625) * (1.5d+0) 
term(1626) = term(1626) * (-3.0d+0) 
term(1627) = term(1627) * (-2.0d+0) 
term(1628) = term(1628) * (4.0d+0) 
term(1629) = term(1629) * (-2.0d+0) 
term(1630) = term(1630) * (4.0d+0) 
term(1631) = term(1631) * (2.0d+0) 
term(1632) = term(1632) * (-4.0d+0) 
term(1633) = term(1633) * (-1.0d+0) 
term(1634) = term(1634) * (2.0d+0) 
term(1635) = term(1635) * (-2.0d+0) 
term(1636) = term(1636) * (4.0d+0) 
term(1637) = term(1637) * (2.0d+0) 
term(1638) = term(1638) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(1639) = term(1639) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_5_triplet_pt2(c,b)
term(1640) = term(1640) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_7_triplet_pt2(c,b)
term(1641) = term(1641) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_5_triplet_pt2(c,a)
term(1642) = term(1642) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_7_triplet_pt2(c,a)
end do 
end do 
end do 
end do 
end do 

term(1639) = term(1639) * (-2.0d+0) 
term(1640) = term(1640) * (4.0d+0) 
term(1641) = term(1641) * (2.0d+0) 
term(1642) = term(1642) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1643) = term(1643) + r2p(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_triplet_pt2(a,c)
term(1644) = term(1644) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(c,b)
term(1645) = term(1645) + r2p(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_triplet_pt2(a,c)
term(1646) = term(1646) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(c,b)
term(1647) = term(1647) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(c,b)
term(1648) = term(1648) + r2p(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_triplet_pt2(a,c)
term(1649) = term(1649) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(c,b)
term(1650) = term(1650) + r2p(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_triplet_pt2(a,c)
term(1651) = term(1651) + r2p(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(a,b)
term(1652) = term(1652) + r2p(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(a,b)
term(1653) = term(1653) + r2p(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(a,b)
term(1654) = term(1654) + r2p(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(a,b)
term(1655) = term(1655) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_14_triplet_pt2(b,c)
term(1656) = term(1656) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_14_triplet_pt2(a,c)
term(1657) = term(1657) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_14_triplet_pt2(a,c)
term(1658) = term(1658) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_14_triplet_pt2(b,c)
term(1659) = term(1659) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_30_triplet_pt2(b,c)
term(1660) = term(1660) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_30_triplet_pt2(a,c)
term(1661) = term(1661) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_30_triplet_pt2(a,c)
term(1662) = term(1662) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_30_triplet_pt2(b,c)
term(1663) = term(1663) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_32_triplet_pt2(b,c)
term(1664) = term(1664) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_32_triplet_pt2(a,c)
term(1665) = term(1665) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_32_triplet_pt2(a,c)
term(1666) = term(1666) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_32_triplet_pt2(b,c)
term(1667) = term(1667) + r2m(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_triplet_pt2(a,c)
term(1668) = term(1668) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(c,b)
term(1669) = term(1669) + r2m(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_triplet_pt2(a,c)
term(1670) = term(1670) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(c,b)
term(1671) = term(1671) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(c,b)
term(1672) = term(1672) + r2m(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_triplet_pt2(a,c)
term(1673) = term(1673) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(c,b)
term(1674) = term(1674) + r2m(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_triplet_pt2(a,c)
term(1675) = term(1675) + r2m(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(a,b)
term(1676) = term(1676) + r2m(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(a,b)
term(1677) = term(1677) + r2m(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_triplet_pt2(a,b)
term(1678) = term(1678) + r2m(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_triplet_pt2(a,b)
term(1679) = term(1679) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_56_triplet_pt2(b,c)
term(1680) = term(1680) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_56_triplet_pt2(a,c)
term(1681) = term(1681) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_56_triplet_pt2(a,c)
term(1682) = term(1682) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_56_triplet_pt2(b,c)
term(1683) = term(1683) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_61_triplet_pt2(b,c)
term(1684) = term(1684) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_61_triplet_pt2(a,c)
term(1685) = term(1685) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_61_triplet_pt2(a,c)
term(1686) = term(1686) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_61_triplet_pt2(b,c)
term(1687) = term(1687) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_5_triplet_pt2(c,b)
term(1688) = term(1688) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_5_triplet_pt2(c,b)
term(1689) = term(1689) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_7_triplet_pt2(c,b)
term(1690) = term(1690) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_7_triplet_pt2(c,b)
term(1691) = term(1691) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_5_triplet_pt2(c,a)
term(1692) = term(1692) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_5_triplet_pt2(c,a)
term(1693) = term(1693) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,i,c,j) * wm_interm_7_triplet_pt2(c,a)
term(1694) = term(1694) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_7_triplet_pt2(c,a)
term(1695) = term(1695) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_94_triplet_pt2(b,c)
term(1696) = term(1696) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_95_triplet_pt2(b,c)
term(1697) = term(1697) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_98_triplet_pt2(b,c)
term(1698) = term(1698) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_94_triplet_pt2(a,c)
term(1699) = term(1699) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_95_triplet_pt2(a,c)
term(1700) = term(1700) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_98_triplet_pt2(a,c)
term(1701) = term(1701) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_94_triplet_pt2(a,c)
term(1702) = term(1702) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_95_triplet_pt2(a,c)
term(1703) = term(1703) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_98_triplet_pt2(a,c)
term(1704) = term(1704) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_94_triplet_pt2(b,c)
term(1705) = term(1705) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_95_triplet_pt2(b,c)
term(1706) = term(1706) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_98_triplet_pt2(b,c)
term(1707) = term(1707) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_5_triplet_pt2(c,b)
term(1708) = term(1708) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_5_triplet_pt2(c,b)
term(1709) = term(1709) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_7_triplet_pt2(c,b)
term(1710) = term(1710) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_7_triplet_pt2(c,b)
term(1711) = term(1711) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_5_triplet_pt2(c,a)
term(1712) = term(1712) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_5_triplet_pt2(c,a)
term(1713) = term(1713) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_7_triplet_pt2(c,a)
term(1714) = term(1714) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_7_triplet_pt2(c,a)
term(1715) = term(1715) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_130_triplet_pt2(b,c)
term(1716) = term(1716) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_131_triplet_pt2(b,c)
term(1717) = term(1717) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_130_triplet_pt2(a,c)
term(1718) = term(1718) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_131_triplet_pt2(a,c)
term(1719) = term(1719) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_130_triplet_pt2(a,c)
term(1720) = term(1720) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_131_triplet_pt2(a,c)
term(1721) = term(1721) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_130_triplet_pt2(b,c)
term(1722) = term(1722) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_131_triplet_pt2(b,c)
end do 
end do 
end do 
end do 
end do 

term(1643) = term(1643) * (3.0d+0) 
term(1644) = term(1644) * (-4.0d+0) 
term(1645) = term(1645) * (-6.0d+0) 
term(1646) = term(1646) * (8.0d+0) 
term(1647) = term(1647) * (3.0d+0) 
term(1648) = term(1648) * (-2.0d+0) 
term(1649) = term(1649) * (-6.0d+0) 
term(1650) = term(1650) * (4.0d+0) 
term(1651) = term(1651) * (3.0d+0) 
term(1652) = term(1652) * (-6.0d+0) 
term(1653) = term(1653) * (-4.0d+0) 
term(1654) = term(1654) * (8.0d+0) 
term(1655) = term(1655) * (3.0d+0) 
term(1656) = term(1656) * (-4.0d+0) 
term(1657) = term(1657) * (3.0d+0) 
term(1658) = term(1658) * (-2.0d+0) 
term(1659) = term(1659) * (-6.0d+0) 
term(1660) = term(1660) * (8.0d+0) 
term(1661) = term(1661) * (-6.0d+0) 
term(1662) = term(1662) * (4.0d+0) 
term(1663) = term(1663) * (3.0d+0) 
term(1664) = term(1664) * (-4.0d+0) 
term(1665) = term(1665) * (3.0d+0) 
term(1666) = term(1666) * (-2.0d+0) 
term(1667) = term(1667) * (6.0d+0) 
term(1668) = term(1668) * (-12.0d+0) 
term(1669) = term(1669) * (-12.0d+0) 
term(1670) = term(1670) * (24.0d+0) 
term(1671) = term(1671) * (12.0d+0) 
term(1672) = term(1672) * (-4.0d+0) 
term(1673) = term(1673) * (-24.0d+0) 
term(1674) = term(1674) * (8.0d+0) 
term(1675) = term(1675) * (6.0d+0) 
term(1676) = term(1676) * (-12.0d+0) 
term(1677) = term(1677) * (-8.0d+0) 
term(1678) = term(1678) * (16.0d+0) 
term(1679) = term(1679) * (12.0d+0) 
term(1680) = term(1680) * (-16.0d+0) 
term(1681) = term(1681) * (12.0d+0) 
term(1682) = term(1682) * (-8.0d+0) 
term(1683) = term(1683) * (-12.0d+0) 
term(1684) = term(1684) * (16.0d+0) 
term(1685) = term(1685) * (-12.0d+0) 
term(1686) = term(1686) * (8.0d+0) 
term(1687) = term(1687) * (3.0d+0) 
term(1688) = term(1688) * (-4.0d+0) 
term(1689) = term(1689) * (-6.0d+0) 
term(1690) = term(1690) * (8.0d+0) 
term(1691) = term(1691) * (3.0d+0) 
term(1692) = term(1692) * (-2.0d+0) 
term(1693) = term(1693) * (-6.0d+0) 
term(1694) = term(1694) * (4.0d+0) 
term(1695) = term(1695) * (3.0d+0) 
term(1696) = term(1696) * (-6.0d+0) 
term(1697) = term(1697) * (3.0d+0) 
term(1698) = term(1698) * (-4.0d+0) 
term(1699) = term(1699) * (8.0d+0) 
term(1700) = term(1700) * (-4.0d+0) 
term(1701) = term(1701) * (3.0d+0) 
term(1702) = term(1702) * (-6.0d+0) 
term(1703) = term(1703) * (3.0d+0) 
term(1704) = term(1704) * (-2.0d+0) 
term(1705) = term(1705) * (4.0d+0) 
term(1706) = term(1706) * (-2.0d+0) 
term(1707) = term(1707) * (12.0d+0) 
term(1708) = term(1708) * (-12.0d+0) 
term(1709) = term(1709) * (-24.0d+0) 
term(1710) = term(1710) * (24.0d+0) 
term(1711) = term(1711) * (12.0d+0) 
term(1712) = term(1712) * (-12.0d+0) 
term(1713) = term(1713) * (-24.0d+0) 
term(1714) = term(1714) * (24.0d+0) 
term(1715) = term(1715) * (12.0d+0) 
term(1716) = term(1716) * (-12.0d+0) 
term(1717) = term(1717) * (-16.0d+0) 
term(1718) = term(1718) * (16.0d+0) 
term(1719) = term(1719) * (12.0d+0) 
term(1720) = term(1720) * (-12.0d+0) 
term(1721) = term(1721) * (-8.0d+0) 
term(1722) = term(1722) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(1723) = term(1723) + wm_interm_0_triplet_pt2(p,i,j,q) * wm_interm_1_triplet_pt2(j,i)
term(1724) = term(1724) + wm_interm_0_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1725) = term(1725) + wm_interm_0_triplet_pt2(p,i,j,q) * wm_interm_2_triplet_pt2(j,i)
term(1726) = term(1726) + wm_interm_0_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1727) = term(1727) + wm_interm_10_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1728) = term(1728) + wm_interm_10_triplet_pt2(p,i,j,q) * wm_interm_1_triplet_pt2(j,i)
term(1729) = term(1729) + wm_interm_10_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1730) = term(1730) + wm_interm_10_triplet_pt2(p,i,j,q) * wm_interm_2_triplet_pt2(j,i)
term(1731) = term(1731) + wm_interm_11_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1732) = term(1732) + wm_interm_11_triplet_pt2(p,i,j,q) * wm_interm_1_triplet_pt2(j,i)
term(1733) = term(1733) + wm_interm_11_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1734) = term(1734) + wm_interm_11_triplet_pt2(p,i,j,q) * wm_interm_2_triplet_pt2(j,i)
term(1735) = term(1735) + wm_interm_18_triplet_pt2(i,j) * wm_interm_19_triplet_pt2(p,i,q,j)
term(1736) = term(1736) + wm_interm_18_triplet_pt2(i,j) * wm_interm_20_triplet_pt2(p,i,q,j)
term(1737) = term(1737) + wm_interm_18_triplet_pt2(i,j) * wm_interm_21_triplet_pt2(p,i,q,j)
term(1738) = term(1738) + wm_interm_18_triplet_pt2(i,j) * wm_interm_22_triplet_pt2(p,i,q,j)
term(1739) = term(1739) + wm_interm_18_triplet_pt2(i,j) * wm_interm_23_triplet_pt2(p,i,q,j)
term(1740) = term(1740) + wm_interm_18_triplet_pt2(i,j) * wm_interm_24_triplet_pt2(p,i,q,j)
term(1741) = term(1741) + wm_interm_19_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(1742) = term(1742) + wm_interm_20_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(1743) = term(1743) + wm_interm_21_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(1744) = term(1744) + wm_interm_22_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(1745) = term(1745) + wm_interm_23_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(1746) = term(1746) + wm_interm_24_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(1747) = term(1747) + wm_interm_19_triplet_pt2(p,i,q,j) * wm_interm_34_triplet_pt2(i,j)
term(1748) = term(1748) + wm_interm_20_triplet_pt2(p,i,q,j) * wm_interm_34_triplet_pt2(i,j)
term(1749) = term(1749) + wm_interm_21_triplet_pt2(p,i,q,j) * wm_interm_34_triplet_pt2(i,j)
term(1750) = term(1750) + wm_interm_22_triplet_pt2(p,i,q,j) * wm_interm_34_triplet_pt2(i,j)
term(1751) = term(1751) + wm_interm_23_triplet_pt2(p,i,q,j) * wm_interm_34_triplet_pt2(i,j)
term(1752) = term(1752) + wm_interm_24_triplet_pt2(p,i,q,j) * wm_interm_34_triplet_pt2(i,j)
term(1753) = term(1753) + wm_interm_19_triplet_pt2(p,i,q,j) * wm_interm_59_triplet_pt2(i,j)
term(1754) = term(1754) + wm_interm_20_triplet_pt2(p,i,q,j) * wm_interm_59_triplet_pt2(i,j)
term(1755) = term(1755) + wm_interm_21_triplet_pt2(p,i,q,j) * wm_interm_59_triplet_pt2(i,j)
term(1756) = term(1756) + wm_interm_22_triplet_pt2(p,i,q,j) * wm_interm_59_triplet_pt2(i,j)
term(1757) = term(1757) + wm_interm_23_triplet_pt2(p,i,q,j) * wm_interm_59_triplet_pt2(i,j)
term(1758) = term(1758) + wm_interm_24_triplet_pt2(p,i,q,j) * wm_interm_59_triplet_pt2(i,j)
term(1759) = term(1759) + wm_interm_19_triplet_pt2(p,i,q,j) * wm_interm_62_triplet_pt2(i,j)
term(1760) = term(1760) + wm_interm_20_triplet_pt2(p,i,q,j) * wm_interm_62_triplet_pt2(i,j)
term(1761) = term(1761) + wm_interm_21_triplet_pt2(p,i,q,j) * wm_interm_62_triplet_pt2(i,j)
term(1762) = term(1762) + wm_interm_22_triplet_pt2(p,i,q,j) * wm_interm_62_triplet_pt2(i,j)
term(1763) = term(1763) + wm_interm_23_triplet_pt2(p,i,q,j) * wm_interm_62_triplet_pt2(i,j)
term(1764) = term(1764) + wm_interm_24_triplet_pt2(p,i,q,j) * wm_interm_62_triplet_pt2(i,j)
term(1765) = term(1765) + wm_interm_103_triplet_pt2(p,i,q,j) * wm_interm_104_triplet_pt2(i,j)
term(1766) = term(1766) + wm_interm_103_triplet_pt2(p,i,q,j) * wm_interm_105_triplet_pt2(i,j)
term(1767) = term(1767) + wm_interm_103_triplet_pt2(p,i,q,j) * wm_interm_106_triplet_pt2(i,j)
term(1768) = term(1768) + wm_interm_104_triplet_pt2(i,j) * wm_interm_108_triplet_pt2(p,q,i,j)
term(1769) = term(1769) + wm_interm_105_triplet_pt2(i,j) * wm_interm_108_triplet_pt2(p,q,i,j)
term(1770) = term(1770) + wm_interm_106_triplet_pt2(i,j) * wm_interm_108_triplet_pt2(p,q,i,j)
term(1771) = term(1771) + wm_interm_104_triplet_pt2(i,j) * wm_interm_110_triplet_pt2(p,i,q,j)
term(1772) = term(1772) + wm_interm_105_triplet_pt2(i,j) * wm_interm_110_triplet_pt2(p,i,q,j)
term(1773) = term(1773) + wm_interm_106_triplet_pt2(i,j) * wm_interm_110_triplet_pt2(p,i,q,j)
term(1774) = term(1774) + wm_interm_104_triplet_pt2(i,j) * wm_interm_111_triplet_pt2(p,i,q,j)
term(1775) = term(1775) + wm_interm_105_triplet_pt2(i,j) * wm_interm_111_triplet_pt2(p,i,q,j)
term(1776) = term(1776) + wm_interm_106_triplet_pt2(i,j) * wm_interm_111_triplet_pt2(p,i,q,j)
term(1777) = term(1777) + wm_interm_104_triplet_pt2(i,j) * wm_interm_111_triplet_pt2(p,q,i,j)
term(1778) = term(1778) + wm_interm_105_triplet_pt2(i,j) * wm_interm_111_triplet_pt2(p,q,i,j)
term(1779) = term(1779) + wm_interm_106_triplet_pt2(i,j) * wm_interm_111_triplet_pt2(p,q,i,j)
term(1780) = term(1780) + wm_interm_104_triplet_pt2(i,j) * wm_interm_110_triplet_pt2(p,q,i,j)
term(1781) = term(1781) + wm_interm_105_triplet_pt2(i,j) * wm_interm_110_triplet_pt2(p,q,i,j)
term(1782) = term(1782) + wm_interm_104_triplet_pt2(i,j) * wm_interm_113_triplet_pt2(p,q,i,j)
term(1783) = term(1783) + wm_interm_105_triplet_pt2(i,j) * wm_interm_113_triplet_pt2(p,q,i,j)
term(1784) = term(1784) + wm_interm_106_triplet_pt2(i,j) * wm_interm_110_triplet_pt2(p,q,i,j)
term(1785) = term(1785) + wm_interm_106_triplet_pt2(i,j) * wm_interm_113_triplet_pt2(p,q,i,j)
term(1786) = term(1786) + wm_interm_104_triplet_pt2(i,j) * wm_interm_114_triplet_pt2(p,q,i,j)
term(1787) = term(1787) + wm_interm_105_triplet_pt2(i,j) * wm_interm_114_triplet_pt2(p,q,i,j)
term(1788) = term(1788) + wm_interm_106_triplet_pt2(i,j) * wm_interm_114_triplet_pt2(p,q,i,j)
term(1789) = term(1789) + wm_interm_104_triplet_pt2(i,j) * wm_interm_108_triplet_pt2(p,i,q,j)
term(1790) = term(1790) + wm_interm_105_triplet_pt2(i,j) * wm_interm_108_triplet_pt2(p,i,q,j)
term(1791) = term(1791) + wm_interm_106_triplet_pt2(i,j) * wm_interm_108_triplet_pt2(p,i,q,j)
term(1792) = term(1792) + wm_interm_103_triplet_pt2(p,q,i,j) * wm_interm_104_triplet_pt2(i,j)
term(1793) = term(1793) + wm_interm_103_triplet_pt2(p,q,i,j) * wm_interm_105_triplet_pt2(i,j)
term(1794) = term(1794) + wm_interm_104_triplet_pt2(i,j) * wm_interm_113_triplet_pt2(p,i,q,j)
term(1795) = term(1795) + wm_interm_105_triplet_pt2(i,j) * wm_interm_113_triplet_pt2(p,i,q,j)
term(1796) = term(1796) + wm_interm_103_triplet_pt2(p,q,i,j) * wm_interm_106_triplet_pt2(i,j)
term(1797) = term(1797) + wm_interm_106_triplet_pt2(i,j) * wm_interm_113_triplet_pt2(p,i,q,j)
term(1798) = term(1798) + wm_interm_104_triplet_pt2(i,j) * wm_interm_114_triplet_pt2(p,i,q,j)
term(1799) = term(1799) + wm_interm_105_triplet_pt2(i,j) * wm_interm_114_triplet_pt2(p,i,q,j)
term(1800) = term(1800) + wm_interm_106_triplet_pt2(i,j) * wm_interm_114_triplet_pt2(p,i,q,j)
term(1801) = term(1801) + wm_interm_116_triplet_pt2(p,q,i,j) * wm_interm_1_triplet_pt2(j,i)
term(1802) = term(1802) + wm_interm_117_triplet_pt2(p,q,i,j) * wm_interm_1_triplet_pt2(j,i)
term(1803) = term(1803) + wm_interm_116_triplet_pt2(p,q,i,j) * wm_interm_2_triplet_pt2(j,i)
term(1804) = term(1804) + wm_interm_117_triplet_pt2(p,q,i,j) * wm_interm_2_triplet_pt2(j,i)
term(1805) = term(1805) + wm_interm_120_triplet_pt2(p,q,i,j) * wm_interm_1_triplet_pt2(j,i)
term(1806) = term(1806) + wm_interm_121_triplet_pt2(p,q,i,j) * wm_interm_1_triplet_pt2(j,i)
term(1807) = term(1807) + wm_interm_120_triplet_pt2(p,q,i,j) * wm_interm_2_triplet_pt2(j,i)
term(1808) = term(1808) + wm_interm_121_triplet_pt2(p,q,i,j) * wm_interm_2_triplet_pt2(j,i)
term(1809) = term(1809) + wm_interm_122_triplet_pt2(p,q,i,j) * wm_interm_1_triplet_pt2(j,i)
term(1810) = term(1810) + wm_interm_122_triplet_pt2(p,q,i,j) * wm_interm_2_triplet_pt2(j,i)
term(1811) = term(1811) + wm_interm_123_triplet_pt2(p,q,i,j) * wm_interm_1_triplet_pt2(j,i)
term(1812) = term(1812) + wm_interm_123_triplet_pt2(p,q,i,j) * wm_interm_2_triplet_pt2(j,i)
term(1813) = term(1813) + wm_interm_117_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1814) = term(1814) + wm_interm_117_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1815) = term(1815) + wm_interm_116_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1816) = term(1816) + wm_interm_116_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1817) = term(1817) + wm_interm_120_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1818) = term(1818) + wm_interm_120_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1819) = term(1819) + wm_interm_122_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1820) = term(1820) + wm_interm_122_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1821) = term(1821) + wm_interm_121_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1822) = term(1822) + wm_interm_121_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1823) = term(1823) + wm_interm_123_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(1824) = term(1824) + wm_interm_123_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(1825) = term(1825) + wm_interm_103_triplet_pt2(p,i,q,j) * wm_interm_135_triplet_pt2(i,j)
term(1826) = term(1826) + wm_interm_103_triplet_pt2(p,i,q,j) * wm_interm_136_triplet_pt2(i,j)
term(1827) = term(1827) + wm_interm_103_triplet_pt2(p,i,q,j) * wm_interm_137_triplet_pt2(i,j)
term(1828) = term(1828) + wm_interm_108_triplet_pt2(p,q,i,j) * wm_interm_135_triplet_pt2(i,j)
term(1829) = term(1829) + wm_interm_108_triplet_pt2(p,q,i,j) * wm_interm_136_triplet_pt2(i,j)
term(1830) = term(1830) + wm_interm_108_triplet_pt2(p,q,i,j) * wm_interm_137_triplet_pt2(i,j)
term(1831) = term(1831) + wm_interm_110_triplet_pt2(p,i,q,j) * wm_interm_135_triplet_pt2(i,j)
term(1832) = term(1832) + wm_interm_110_triplet_pt2(p,i,q,j) * wm_interm_136_triplet_pt2(i,j)
term(1833) = term(1833) + wm_interm_110_triplet_pt2(p,i,q,j) * wm_interm_137_triplet_pt2(i,j)
term(1834) = term(1834) + wm_interm_111_triplet_pt2(p,i,q,j) * wm_interm_135_triplet_pt2(i,j)
term(1835) = term(1835) + wm_interm_111_triplet_pt2(p,i,q,j) * wm_interm_136_triplet_pt2(i,j)
term(1836) = term(1836) + wm_interm_111_triplet_pt2(p,i,q,j) * wm_interm_137_triplet_pt2(i,j)
term(1837) = term(1837) + wm_interm_111_triplet_pt2(p,q,i,j) * wm_interm_135_triplet_pt2(i,j)
term(1838) = term(1838) + wm_interm_111_triplet_pt2(p,q,i,j) * wm_interm_136_triplet_pt2(i,j)
term(1839) = term(1839) + wm_interm_111_triplet_pt2(p,q,i,j) * wm_interm_137_triplet_pt2(i,j)
term(1840) = term(1840) + wm_interm_110_triplet_pt2(p,q,i,j) * wm_interm_135_triplet_pt2(i,j)
term(1841) = term(1841) + wm_interm_110_triplet_pt2(p,q,i,j) * wm_interm_136_triplet_pt2(i,j)
term(1842) = term(1842) + wm_interm_113_triplet_pt2(p,q,i,j) * wm_interm_135_triplet_pt2(i,j)
term(1843) = term(1843) + wm_interm_113_triplet_pt2(p,q,i,j) * wm_interm_136_triplet_pt2(i,j)
term(1844) = term(1844) + wm_interm_110_triplet_pt2(p,q,i,j) * wm_interm_137_triplet_pt2(i,j)
term(1845) = term(1845) + wm_interm_113_triplet_pt2(p,q,i,j) * wm_interm_137_triplet_pt2(i,j)
term(1846) = term(1846) + wm_interm_114_triplet_pt2(p,q,i,j) * wm_interm_135_triplet_pt2(i,j)
term(1847) = term(1847) + wm_interm_114_triplet_pt2(p,q,i,j) * wm_interm_136_triplet_pt2(i,j)
term(1848) = term(1848) + wm_interm_114_triplet_pt2(p,q,i,j) * wm_interm_137_triplet_pt2(i,j)
term(1849) = term(1849) + wm_interm_108_triplet_pt2(p,i,q,j) * wm_interm_135_triplet_pt2(i,j)
term(1850) = term(1850) + wm_interm_108_triplet_pt2(p,i,q,j) * wm_interm_136_triplet_pt2(i,j)
term(1851) = term(1851) + wm_interm_108_triplet_pt2(p,i,q,j) * wm_interm_137_triplet_pt2(i,j)
term(1852) = term(1852) + wm_interm_103_triplet_pt2(p,q,i,j) * wm_interm_135_triplet_pt2(i,j)
term(1853) = term(1853) + wm_interm_103_triplet_pt2(p,q,i,j) * wm_interm_136_triplet_pt2(i,j)
term(1854) = term(1854) + wm_interm_113_triplet_pt2(p,i,q,j) * wm_interm_135_triplet_pt2(i,j)
term(1855) = term(1855) + wm_interm_113_triplet_pt2(p,i,q,j) * wm_interm_136_triplet_pt2(i,j)
term(1856) = term(1856) + wm_interm_103_triplet_pt2(p,q,i,j) * wm_interm_137_triplet_pt2(i,j)
term(1857) = term(1857) + wm_interm_113_triplet_pt2(p,i,q,j) * wm_interm_137_triplet_pt2(i,j)
term(1858) = term(1858) + wm_interm_114_triplet_pt2(p,i,q,j) * wm_interm_135_triplet_pt2(i,j)
term(1859) = term(1859) + wm_interm_114_triplet_pt2(p,i,q,j) * wm_interm_136_triplet_pt2(i,j)
term(1860) = term(1860) + wm_interm_114_triplet_pt2(p,i,q,j) * wm_interm_137_triplet_pt2(i,j)
end do 
end do 

term(1723) = term(1723) * (3.0d+0) 
term(1724) = term(1724) * (-4.0d+0) 
term(1725) = term(1725) * (-6.0d+0) 
term(1726) = term(1726) * (8.0d+0) 
term(1727) = term(1727) * (3.0d+0) 
term(1728) = term(1728) * (-4.0d+0) 
term(1729) = term(1729) * (-6.0d+0) 
term(1730) = term(1730) * (8.0d+0) 
term(1731) = term(1731) * (-4.0d+0) 
term(1732) = term(1732) * (4.0d+0) 
term(1733) = term(1733) * (8.0d+0) 
term(1734) = term(1734) * (-8.0d+0) 
term(1735) = term(1735) * (3.0d+0) 
term(1736) = term(1736) * (-4.0d+0) 
term(1737) = term(1737) * (3.0d+0) 
term(1738) = term(1738) * (-2.0d+0) 
term(1739) = term(1739) * (-4.0d+0) 
term(1740) = term(1740) * (4.0d+0) 
term(1741) = term(1741) * (-6.0d+0) 
term(1742) = term(1742) * (8.0d+0) 
term(1743) = term(1743) * (-6.0d+0) 
term(1744) = term(1744) * (4.0d+0) 
term(1745) = term(1745) * (8.0d+0) 
term(1746) = term(1746) * (-8.0d+0) 
term(1747) = term(1747) * (3.0d+0) 
term(1748) = term(1748) * (-4.0d+0) 
term(1749) = term(1749) * (3.0d+0) 
term(1750) = term(1750) * (-2.0d+0) 
term(1751) = term(1751) * (-4.0d+0) 
term(1752) = term(1752) * (4.0d+0) 
term(1753) = term(1753) * (12.0d+0) 
term(1754) = term(1754) * (-16.0d+0) 
term(1755) = term(1755) * (12.0d+0) 
term(1756) = term(1756) * (-8.0d+0) 
term(1757) = term(1757) * (-16.0d+0) 
term(1758) = term(1758) * (16.0d+0) 
term(1759) = term(1759) * (-12.0d+0) 
term(1760) = term(1760) * (16.0d+0) 
term(1761) = term(1761) * (-12.0d+0) 
term(1762) = term(1762) * (8.0d+0) 
term(1763) = term(1763) * (16.0d+0) 
term(1764) = term(1764) * (-16.0d+0) 
term(1765) = term(1765) * (1.5d+0) 
term(1766) = term(1766) * (-3.0d+0) 
term(1767) = term(1767) * (1.5d+0) 
term(1768) = term(1768) * (1.5d+0) 
term(1769) = term(1769) * (-3.0d+0) 
term(1770) = term(1770) * (1.5d+0) 
term(1771) = term(1771) * (-2.0d+0) 
term(1772) = term(1772) * (4.0d+0) 
term(1773) = term(1773) * (-2.0d+0) 
term(1774) = term(1774) * (1.5d+0) 
term(1775) = term(1775) * (-3.0d+0) 
term(1776) = term(1776) * (1.5d+0) 
term(1777) = term(1777) * (-2.0d+0) 
term(1778) = term(1778) * (4.0d+0) 
term(1779) = term(1779) * (-2.0d+0) 
term(1780) = term(1780) * (2.0d+0) 
term(1781) = term(1781) * (-4.0d+0) 
term(1782) = term(1782) * (-2.0d+0) 
term(1783) = term(1783) * (4.0d+0) 
term(1784) = term(1784) * (2.0d+0) 
term(1785) = term(1785) * (-2.0d+0) 
term(1786) = term(1786) * (1.5d+0) 
term(1787) = term(1787) * (-3.0d+0) 
term(1788) = term(1788) * (1.5d+0) 
term(1789) = term(1789) * (-2.0d+0) 
term(1790) = term(1790) * (4.0d+0) 
term(1791) = term(1791) * (-2.0d+0) 
term(1792) = term(1792) * (-1.0d+0) 
term(1793) = term(1793) * (2.0d+0) 
term(1794) = term(1794) * (2.0d+0) 
term(1795) = term(1795) * (-4.0d+0) 
term(1796) = term(1796) * (-1.0d+0) 
term(1797) = term(1797) * (2.0d+0) 
term(1798) = term(1798) * (-1.0d+0) 
term(1799) = term(1799) * (2.0d+0) 
term(1800) = term(1800) * (-1.0d+0) 
term(1801) = term(1801) * (6.0d+0) 
term(1802) = term(1802) * (-6.0d+0) 
term(1803) = term(1803) * (-12.0d+0) 
term(1804) = term(1804) * (12.0d+0) 
term(1805) = term(1805) * (-8.0d+0) 
term(1806) = term(1806) * (8.0d+0) 
term(1807) = term(1807) * (16.0d+0) 
term(1808) = term(1808) * (-16.0d+0) 
term(1809) = term(1809) * (6.0d+0) 
term(1810) = term(1810) * (-12.0d+0) 
term(1811) = term(1811) * (-6.0d+0) 
term(1812) = term(1812) * (12.0d+0) 
term(1813) = term(1813) * (6.0d+0) 
term(1814) = term(1814) * (-12.0d+0) 
term(1815) = term(1815) * (-6.0d+0) 
term(1816) = term(1816) * (12.0d+0) 
term(1817) = term(1817) * (6.0d+0) 
term(1818) = term(1818) * (-12.0d+0) 
term(1819) = term(1819) * (-8.0d+0) 
term(1820) = term(1820) * (16.0d+0) 
term(1821) = term(1821) * (-6.0d+0) 
term(1822) = term(1822) * (12.0d+0) 
term(1823) = term(1823) * (8.0d+0) 
term(1824) = term(1824) * (-16.0d+0) 
term(1825) = term(1825) * (3.0d+0) 
term(1826) = term(1826) * (-6.0d+0) 
term(1827) = term(1827) * (3.0d+0) 
term(1828) = term(1828) * (3.0d+0) 
term(1829) = term(1829) * (-6.0d+0) 
term(1830) = term(1830) * (3.0d+0) 
term(1831) = term(1831) * (-4.0d+0) 
term(1832) = term(1832) * (8.0d+0) 
term(1833) = term(1833) * (-4.0d+0) 
term(1834) = term(1834) * (3.0d+0) 
term(1835) = term(1835) * (-6.0d+0) 
term(1836) = term(1836) * (3.0d+0) 
term(1837) = term(1837) * (-4.0d+0) 
term(1838) = term(1838) * (8.0d+0) 
term(1839) = term(1839) * (-4.0d+0) 
term(1840) = term(1840) * (4.0d+0) 
term(1841) = term(1841) * (-8.0d+0) 
term(1842) = term(1842) * (-4.0d+0) 
term(1843) = term(1843) * (8.0d+0) 
term(1844) = term(1844) * (4.0d+0) 
term(1845) = term(1845) * (-4.0d+0) 
term(1846) = term(1846) * (3.0d+0) 
term(1847) = term(1847) * (-6.0d+0) 
term(1848) = term(1848) * (3.0d+0) 
term(1849) = term(1849) * (-4.0d+0) 
term(1850) = term(1850) * (8.0d+0) 
term(1851) = term(1851) * (-4.0d+0) 
term(1852) = term(1852) * (-2.0d+0) 
term(1853) = term(1853) * (4.0d+0) 
term(1854) = term(1854) * (4.0d+0) 
term(1855) = term(1855) * (-8.0d+0) 
term(1856) = term(1856) * (-2.0d+0) 
term(1857) = term(1857) * (4.0d+0) 
term(1858) = term(1858) * (-2.0d+0) 
term(1859) = term(1859) * (4.0d+0) 
term(1860) = term(1860) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1861) = term(1861) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_9_triplet_pt2(p,k,j,i)
term(1862) = term(1862) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_3_triplet_pt2(p,k,j,i)
term(1863) = term(1863) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_4_triplet_pt2(p,k,j,i)
term(1864) = term(1864) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_50_triplet_pt2(p,k,j,i)
term(1865) = term(1865) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_51_triplet_pt2(p,k,j,i)
term(1866) = term(1866) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_54_triplet_pt2(p,k,j,i)
term(1867) = term(1867) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_49_triplet_pt2(p,k,j,i)
end do 
end do 
end do 

term(1861) = term(1861) * (3.0d+0) 
term(1862) = term(1862) * (3.0d+0) 
term(1863) = term(1863) * (-2.0d+0) 
term(1864) = term(1864) * (12.0d+0) 
term(1865) = term(1865) * (-12.0d+0) 
term(1866) = term(1866) * (6.0d+0) 
term(1867) = term(1867) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1868) = term(1868) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_5_triplet_pt2(c,b)
term(1869) = term(1869) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_7_triplet_pt2(c,b)
term(1870) = term(1870) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_5_triplet_pt2(c,a)
term(1871) = term(1871) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_7_triplet_pt2(c,a)
term(1872) = term(1872) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_14_triplet_pt2(a,c)
term(1873) = term(1873) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_30_triplet_pt2(a,c)
term(1874) = term(1874) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_32_triplet_pt2(a,c)
term(1875) = term(1875) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_5_triplet_pt2(c,b)
term(1876) = term(1876) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_7_triplet_pt2(c,b)
term(1877) = term(1877) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_5_triplet_pt2(c,a)
term(1878) = term(1878) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_7_triplet_pt2(c,a)
term(1879) = term(1879) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_56_triplet_pt2(a,c)
term(1880) = term(1880) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_61_triplet_pt2(a,c)
term(1881) = term(1881) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_5_triplet_pt2(c,b)
term(1882) = term(1882) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,j,c,i) * wm_interm_7_triplet_pt2(c,b)
term(1883) = term(1883) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_5_triplet_pt2(c,a)
term(1884) = term(1884) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, b,j,c,i) * wm_interm_7_triplet_pt2(c,a)
term(1885) = term(1885) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_94_triplet_pt2(a,c)
term(1886) = term(1886) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_95_triplet_pt2(a,c)
term(1887) = term(1887) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_98_triplet_pt2(a,c)
term(1888) = term(1888) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_94_triplet_pt2(b,c)
term(1889) = term(1889) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_95_triplet_pt2(b,c)
term(1890) = term(1890) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_98_triplet_pt2(b,c)
term(1891) = term(1891) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_5_triplet_pt2(c,b)
term(1892) = term(1892) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_7_triplet_pt2(c,b)
term(1893) = term(1893) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_5_triplet_pt2(c,a)
term(1894) = term(1894) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_7_triplet_pt2(c,a)
term(1895) = term(1895) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_130_triplet_pt2(a,c)
term(1896) = term(1896) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_131_triplet_pt2(a,c)
term(1897) = term(1897) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_130_triplet_pt2(b,c)
term(1898) = term(1898) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_131_triplet_pt2(b,c)
end do 
end do 
end do 
end do 
end do 

term(1868) = term(1868) * (-4.0d+0) 
term(1869) = term(1869) * (8.0d+0) 
term(1870) = term(1870) * (4.0d+0) 
term(1871) = term(1871) * (-8.0d+0) 
term(1872) = term(1872) * (4.0d+0) 
term(1873) = term(1873) * (-8.0d+0) 
term(1874) = term(1874) * (4.0d+0) 
term(1875) = term(1875) * (-8.0d+0) 
term(1876) = term(1876) * (16.0d+0) 
term(1877) = term(1877) * (8.0d+0) 
term(1878) = term(1878) * (-16.0d+0) 
term(1879) = term(1879) * (16.0d+0) 
term(1880) = term(1880) * (-16.0d+0) 
term(1881) = term(1881) * (-2.0d+0) 
term(1882) = term(1882) * (4.0d+0) 
term(1883) = term(1883) * (2.0d+0) 
term(1884) = term(1884) * (-4.0d+0) 
term(1885) = term(1885) * (2.0d+0) 
term(1886) = term(1886) * (-4.0d+0) 
term(1887) = term(1887) * (2.0d+0) 
term(1888) = term(1888) * (-2.0d+0) 
term(1889) = term(1889) * (4.0d+0) 
term(1890) = term(1890) * (-2.0d+0) 
term(1891) = term(1891) * (-8.0d+0) 
term(1892) = term(1892) * (16.0d+0) 
term(1893) = term(1893) * (8.0d+0) 
term(1894) = term(1894) * (-16.0d+0) 
term(1895) = term(1895) * (8.0d+0) 
term(1896) = term(1896) * (-8.0d+0) 
term(1897) = term(1897) * (-8.0d+0) 
term(1898) = term(1898) * (8.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1899) = term(1899) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_5_triplet_pt2(c,b)
term(1900) = term(1900) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_7_triplet_pt2(c,b)
term(1901) = term(1901) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_14_triplet_pt2(a,c)
term(1902) = term(1902) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_30_triplet_pt2(a,c)
term(1903) = term(1903) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_32_triplet_pt2(a,c)
term(1904) = term(1904) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_5_triplet_pt2(c,b)
term(1905) = term(1905) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_7_triplet_pt2(c,b)
term(1906) = term(1906) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_56_triplet_pt2(a,c)
term(1907) = term(1907) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_61_triplet_pt2(a,c)
term(1908) = term(1908) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_5_triplet_pt2(c,b)
term(1909) = term(1909) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,i,c,j) * wm_interm_7_triplet_pt2(c,b)
term(1910) = term(1910) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_94_triplet_pt2(a,c)
term(1911) = term(1911) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_95_triplet_pt2(a,c)
term(1912) = term(1912) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_98_triplet_pt2(a,c)
term(1913) = term(1913) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_94_triplet_pt2(b,c)
term(1914) = term(1914) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_95_triplet_pt2(b,c)
term(1915) = term(1915) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_98_triplet_pt2(b,c)
term(1916) = term(1916) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_5_triplet_pt2(c,b)
term(1917) = term(1917) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_7_triplet_pt2(c,b)
term(1918) = term(1918) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_130_triplet_pt2(a,c)
term(1919) = term(1919) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_131_triplet_pt2(a,c)
term(1920) = term(1920) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_130_triplet_pt2(b,c)
term(1921) = term(1921) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_131_triplet_pt2(b,c)
end do 
end do 
end do 
end do 
end do 

term(1899) = term(1899) * (4.0d+0) 
term(1900) = term(1900) * (-8.0d+0) 
term(1901) = term(1901) * (-4.0d+0) 
term(1902) = term(1902) * (8.0d+0) 
term(1903) = term(1903) * (-4.0d+0) 
term(1904) = term(1904) * (8.0d+0) 
term(1905) = term(1905) * (-16.0d+0) 
term(1906) = term(1906) * (-16.0d+0) 
term(1907) = term(1907) * (16.0d+0) 
term(1908) = term(1908) * (2.0d+0) 
term(1909) = term(1909) * (-4.0d+0) 
term(1910) = term(1910) * (-2.0d+0) 
term(1911) = term(1911) * (4.0d+0) 
term(1912) = term(1912) * (-2.0d+0) 
term(1913) = term(1913) * (2.0d+0) 
term(1914) = term(1914) * (-4.0d+0) 
term(1915) = term(1915) * (2.0d+0) 
term(1916) = term(1916) * (8.0d+0) 
term(1917) = term(1917) * (-16.0d+0) 
term(1918) = term(1918) * (-8.0d+0) 
term(1919) = term(1919) * (8.0d+0) 
term(1920) = term(1920) * (8.0d+0) 
term(1921) = term(1921) * (-8.0d+0) 


    calc_D_vo_wm_triplet_cc3_pt2 = zero
    do s = 0, 1921
    calc_D_vo_wm_triplet_cc3_pt2 = calc_D_vo_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt2
    
    function calc_D_vv_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt2
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
    real(F64), dimension(0:97) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_143_triplet_pt2(a,p,i,j)
term(1) = term(1) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_144_triplet_pt2(a,p,i,j)
term(2) = term(2) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_145_triplet_pt2(a,p,i,j)
term(3) = term(3) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_146_triplet_pt2(a,q,i,j)
term(4) = term(4) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_147_triplet_pt2(a,q,i,j)
term(5) = term(5) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_148_triplet_pt2(q,a,i,j)
term(6) = term(6) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_149_triplet_pt2(q,a,i,j)
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_149_triplet_pt2(a,q,i,j)
term(8) = term(8) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_148_triplet_pt2(a,q,i,j)
term(9) = term(9) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_150_triplet_pt2(q,a,i,j)
term(10) = term(10) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_146_triplet_pt2(q,a,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (3.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-0.5d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(11) = term(11) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_143_triplet_pt2(a,p,j,i)
term(12) = term(12) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_143_triplet_pt2(a,p,i,j)
term(13) = term(13) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_143_triplet_pt2(a,p,j,i)
term(14) = term(14) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_145_triplet_pt2(a,p,j,i)
term(15) = term(15) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_145_triplet_pt2(a,p,i,j)
term(16) = term(16) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_145_triplet_pt2(a,p,j,i)
term(17) = term(17) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_144_triplet_pt2(a,p,j,i)
term(18) = term(18) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_144_triplet_pt2(a,p,j,i)
term(19) = term(19) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_144_triplet_pt2(a,p,i,j)
term(20) = term(20) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_143_triplet_pt2(a,p,i,j)
term(21) = term(21) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_143_triplet_pt2(a,p,j,i)
term(22) = term(22) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_143_triplet_pt2(a,p,i,j)
term(23) = term(23) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_145_triplet_pt2(a,p,i,j)
term(24) = term(24) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_145_triplet_pt2(a,p,j,i)
term(25) = term(25) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_144_triplet_pt2(a,p,j,i)
term(26) = term(26) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_144_triplet_pt2(a,p,i,j)
term(27) = term(27) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_145_triplet_pt2(a,p,i,j)
term(28) = term(28) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_144_triplet_pt2(a,p,i,j)
term(29) = term(29) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_146_triplet_pt2(q,a,j,i)
term(30) = term(30) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_146_triplet_pt2(a,q,j,i)
term(31) = term(31) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(a,q,j,i)
term(32) = term(32) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(q,a,i,j)
term(33) = term(33) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(q,a,j,i)
term(34) = term(34) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_147_triplet_pt2(q,a,j,i)
term(35) = term(35) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_147_triplet_pt2(a,q,j,i)
term(36) = term(36) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(a,q,j,i)
term(37) = term(37) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(q,a,i,j)
term(38) = term(38) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(q,a,j,i)
term(39) = term(39) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_148_triplet_pt2(q,a,j,i)
term(40) = term(40) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_149_triplet_pt2(a,q,j,i)
term(41) = term(41) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(a,q,j,i)
term(42) = term(42) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(a,q,i,j)
term(43) = term(43) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(a,q,i,j)
term(44) = term(44) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(q,a,j,i)
term(45) = term(45) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(q,a,i,j)
term(46) = term(46) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(q,a,i,j)
term(47) = term(47) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_150_triplet_pt2(q,a,j,i)
term(48) = term(48) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_150_triplet_pt2(a,q,j,i)
term(49) = term(49) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(a,q,i,j)
term(50) = term(50) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(a,q,j,i)
term(51) = term(51) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(a,q,i,j)
term(52) = term(52) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(q,a,j,i)
term(53) = term(53) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(q,a,i,j)
term(54) = term(54) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(a,q,j,i)
term(55) = term(55) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(a,q,i,j)
term(56) = term(56) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_146_triplet_pt2(q,a,j,i)
term(57) = term(57) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(q,a,i,j)
term(58) = term(58) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(a,q,j,i)
term(59) = term(59) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(a,q,i,j)
term(60) = term(60) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_147_triplet_pt2(q,a,j,i)
term(61) = term(61) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(q,a,i,j)
term(62) = term(62) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(q,a,j,i)
term(63) = term(63) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(q,a,j,i)
term(64) = term(64) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(a,q,i,j)
term(65) = term(65) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(a,q,j,i)
term(66) = term(66) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(a,q,j,i)
term(67) = term(67) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_148_triplet_pt2(a,q,i,j)
term(68) = term(68) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_149_triplet_pt2(q,a,i,j)
term(69) = term(69) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(q,a,j,i)
term(70) = term(70) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(q,a,i,j)
term(71) = term(71) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(a,q,i,j)
term(72) = term(72) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_150_triplet_pt2(a,q,j,i)
end do 
end do 
end do 

term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (3.0d+0) 
term(15) = term(15) * (3.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * (3.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (-8.0d+0) 
term(21) = term(21) * (-8.0d+0) 
term(22) = term(22) * (8.0d+0) 
term(23) = term(23) * (6.0d+0) 
term(24) = term(24) * (-8.0d+0) 
term(25) = term(25) * (6.0d+0) 
term(26) = term(26) * (6.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-1.0d+0) 
term(40) = term(40) * (-0.5d+0) 
term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-0.5d+0) 
term(44) = term(44) * (-0.5d+0) 
term(46) = term(46) * (-1.0d+0) 
term(48) = term(48) * (-0.5d+0) 
term(49) = term(49) * (-0.5d+0) 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (-0.5d+0) 
term(53) = term(53) * (8.0d+0) 
term(54) = term(54) * (8.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-2.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (2.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (-2.0d+0) 
term(72) = term(72) * (2.0d+0) 

do i = 1, nocc 
term(73) = term(73) + s1(q,i) * wm_interm_39_triplet_pt2(p,i)
term(74) = term(74) + s1(q,i) * wm_interm_40_triplet_pt2(p,i)
term(75) = term(75) + s1(q,i) * wm_interm_41_triplet_pt2(p,i)
term(76) = term(76) + s1(q,i) * wm_interm_42_triplet_pt2(p,i)
term(77) = term(77) + s1(q,i) * wm_interm_43_triplet_pt2(p,i)
term(78) = term(78) + s1(q,i) * wm_interm_44_triplet_pt2(p,i)
term(79) = term(79) + s1(q,i) * wm_interm_63_triplet_pt2(p,i)
term(80) = term(80) + s1(q,i) * wm_interm_64_triplet_pt2(p,i)
term(81) = term(81) + s1(q,i) * wm_interm_65_triplet_pt2(p,i)
term(82) = term(82) + s1(q,i) * wm_interm_66_triplet_pt2(p,i)
term(83) = term(83) + t1(q,i) * wm_interm_74_triplet_pt2(p,i)
term(84) = term(84) + t1(q,i) * wm_interm_81_triplet_pt2(p,i)
term(85) = term(85) + t1(q,i) * wm_interm_70_triplet_pt2(p,i)
term(86) = term(86) + t1(q,i) * wm_interm_71_triplet_pt2(p,i)
term(87) = term(87) + t1(q,i) * wm_interm_79_triplet_pt2(p,i)
term(88) = term(88) + t1(q,i) * wm_interm_92_triplet_pt2(p,i)
term(89) = term(89) + t1(q,i) * wm_interm_80_triplet_pt2(p,i)
term(90) = term(90) + t1(q,i) * wm_interm_83_triplet_pt2(p,i)
term(91) = term(91) + t1(q,i) * wm_interm_118_triplet_pt2(p,i)
term(92) = term(92) + t1(q,i) * wm_interm_119_triplet_pt2(p,i)
term(93) = term(93) + t1(q,i) * wm_interm_124_triplet_pt2(p,i)
term(94) = term(94) + t1(q,i) * wm_interm_125_triplet_pt2(p,i)
end do 

term(73) = term(73) * (-3.0d+0) 
term(74) = term(74) * (4.0d+0) 
term(75) = term(75) * (-3.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (-12.0d+0) 
term(80) = term(80) * (12.0d+0) 
term(81) = term(81) * (8.0d+0) 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * (-3.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-3.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (-12.0d+0) 
term(92) = term(92) * (12.0d+0) 
term(93) = term(93) * (8.0d+0) 
term(94) = term(94) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(95) = term(95) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_143_triplet_pt2(a,p,j,i)
term(96) = term(96) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_145_triplet_pt2(a,p,j,i)
term(97) = term(97) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_144_triplet_pt2(a,p,j,i)
end do 
end do 
end do 

term(95) = term(95) * (8.0d+0) 
term(96) = term(96) * (6.0d+0) 
term(97) = term(97) * (-8.0d+0) 


    calc_D_vv_wm_triplet_cc3_pt2 = zero
    do s = 0, 97
    calc_D_vv_wm_triplet_cc3_pt2 = calc_D_vv_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt2
    
        

      end module tt_cc3_pt012
