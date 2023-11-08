module tt_cc3_pt012a
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
    
    

      end module tt_cc3_pt012a
