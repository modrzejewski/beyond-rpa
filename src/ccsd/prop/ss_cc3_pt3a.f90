module ss_cc3_pt3a
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

        
        
    implicit none
    !
    ! File generated automatically on 2018-04-19 16:09:39
    !
    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_10_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_20_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_21_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_22_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_23_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_30_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_33_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_35_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_37_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_39_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_pt3 
real(F64) :: wm_interm_42_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_46_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_49_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_51_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_59_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_66_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_67_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_68_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_69_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_70_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_75_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_82_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_83_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_84_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_85_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_86_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_89_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_90_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_91_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_92_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_93_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_94_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_95_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_96_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_97_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_98_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_99_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_100_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_101_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_102_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_103_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_104_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_105_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_106_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_108_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_109_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_110_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_111_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_112_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_113_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_115_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_116_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_117_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_118_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_119_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_120_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_121_pt3 
real(F64) :: wm_interm_122_pt3 
real(F64) :: wm_interm_123_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_124_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_125_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_126_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_127_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_128_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_129_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_130_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_131_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_132_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_133_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_134_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_135_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_136_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_137_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_138_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_139_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_140_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_141_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_142_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_143_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_144_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_145_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_146_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_147_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_148_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_149_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_150_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_151_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_152_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_153_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_154_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_155_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_156_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_157_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_158_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_159_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_160_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_161_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_162_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_163_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_164_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_165_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_166_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_167_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_168_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_169_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_170_pt3 

    contains
    
    subroutine wm_intermediates_cc3_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_1_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_3_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_pt3(1: nocc, 1: nocc))
allocate(wm_interm_5_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_6_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_8_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_pt3(1: nocc, 1: nocc))
allocate(wm_interm_13_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_20_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_24_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_26_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_28_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_30_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_31_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_32_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_33_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_34_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_40_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_44_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_46_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_50_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_53_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_55_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_56_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_58_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_pt3(1: nocc, 1: nocc))
allocate(wm_interm_60_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_61_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_65_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_66_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_67_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_68_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_69_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_70_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_71_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_72_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_73_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_74_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_75_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_76_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_77_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_78_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_79_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_81_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_82_pt3(1: nocc, 1: nocc))
allocate(wm_interm_83_pt3(1: nocc, 1: nocc))
allocate(wm_interm_84_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_85_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_86_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_87_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_88_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_90_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_91_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_92_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_93_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_94_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_95_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_96_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_97_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_98_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_99_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_100_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_101_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_102_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_103_pt3(1: nocc, 1: nocc))
allocate(wm_interm_104_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_105_pt3(1: nocc, 1: nocc))
allocate(wm_interm_106_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_107_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_108_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_109_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_110_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_111_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_112_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_113_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_114_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_115_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_116_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_117_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_118_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_119_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_120_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_121_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_124_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_125_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_126_pt3(1: nocc, 1: nocc))
allocate(wm_interm_127_pt3(1: nocc, 1: nocc))
allocate(wm_interm_128_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_129_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_130_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_131_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_132_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_133_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_134_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_135_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_136_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_137_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_138_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_139_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_140_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_141_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_142_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_143_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_144_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_145_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_146_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_147_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_148_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_149_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_150_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_151_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_152_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_153_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_154_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_155_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_156_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_157_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_158_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_159_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_160_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_161_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_162_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_163_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_164_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_165_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_166_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_167_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_168_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_169_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_170_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_pt3 = zero 
wm_interm_1_pt3 = zero 
wm_interm_2_pt3 = zero 
wm_interm_3_pt3 = zero 
wm_interm_4_pt3 = zero 
wm_interm_5_pt3 = zero 
wm_interm_6_pt3 = zero 
wm_interm_7_pt3 = zero 
wm_interm_8_pt3 = zero 
wm_interm_9_pt3 = zero 
wm_interm_10_pt3 = zero 
wm_interm_11_pt3 = zero 
wm_interm_12_pt3 = zero 
wm_interm_13_pt3 = zero 
wm_interm_14_pt3 = zero 
wm_interm_15_pt3 = zero 
wm_interm_16_pt3 = zero 
wm_interm_17_pt3 = zero 
wm_interm_18_pt3 = zero 
wm_interm_19_pt3 = zero 
wm_interm_20_pt3 = zero 
wm_interm_21_pt3 = zero 
wm_interm_22_pt3 = zero 
wm_interm_23_pt3 = zero 
wm_interm_24_pt3 = zero 
wm_interm_25_pt3 = zero 
wm_interm_26_pt3 = zero 
wm_interm_27_pt3 = zero 
wm_interm_28_pt3 = zero 
wm_interm_29_pt3 = zero 
wm_interm_30_pt3 = zero 
wm_interm_31_pt3 = zero 
wm_interm_32_pt3 = zero 
wm_interm_33_pt3 = zero 
wm_interm_34_pt3 = zero 
wm_interm_35_pt3 = zero 
wm_interm_36_pt3 = zero 
wm_interm_37_pt3 = zero 
wm_interm_38_pt3 = zero 
wm_interm_39_pt3 = zero 
wm_interm_40_pt3 = zero 
wm_interm_41_pt3 = zero 
wm_interm_42_pt3 = zero 
wm_interm_43_pt3 = zero 
wm_interm_44_pt3 = zero 
wm_interm_45_pt3 = zero 
wm_interm_46_pt3 = zero 
wm_interm_47_pt3 = zero 
wm_interm_48_pt3 = zero 
wm_interm_49_pt3 = zero 
wm_interm_50_pt3 = zero 
wm_interm_51_pt3 = zero 
wm_interm_52_pt3 = zero 
wm_interm_53_pt3 = zero 
wm_interm_54_pt3 = zero 
wm_interm_55_pt3 = zero 
wm_interm_56_pt3 = zero 
wm_interm_57_pt3 = zero 
wm_interm_58_pt3 = zero 
wm_interm_59_pt3 = zero 
wm_interm_60_pt3 = zero 
wm_interm_61_pt3 = zero 
wm_interm_62_pt3 = zero 
wm_interm_63_pt3 = zero 
wm_interm_64_pt3 = zero 
wm_interm_65_pt3 = zero 
wm_interm_66_pt3 = zero 
wm_interm_67_pt3 = zero 
wm_interm_68_pt3 = zero 
wm_interm_69_pt3 = zero 
wm_interm_70_pt3 = zero 
wm_interm_71_pt3 = zero 
wm_interm_72_pt3 = zero 
wm_interm_73_pt3 = zero 
wm_interm_74_pt3 = zero 
wm_interm_75_pt3 = zero 
wm_interm_76_pt3 = zero 
wm_interm_77_pt3 = zero 
wm_interm_78_pt3 = zero 
wm_interm_79_pt3 = zero 
wm_interm_80_pt3 = zero 
wm_interm_81_pt3 = zero 
wm_interm_82_pt3 = zero 
wm_interm_83_pt3 = zero 
wm_interm_84_pt3 = zero 
wm_interm_85_pt3 = zero 
wm_interm_86_pt3 = zero 
wm_interm_87_pt3 = zero 
wm_interm_88_pt3 = zero 
wm_interm_89_pt3 = zero 
wm_interm_90_pt3 = zero 
wm_interm_91_pt3 = zero 
wm_interm_92_pt3 = zero 
wm_interm_93_pt3 = zero 
wm_interm_94_pt3 = zero 
wm_interm_95_pt3 = zero 
wm_interm_96_pt3 = zero 
wm_interm_97_pt3 = zero 
wm_interm_98_pt3 = zero 
wm_interm_99_pt3 = zero 
wm_interm_100_pt3 = zero 
wm_interm_101_pt3 = zero 
wm_interm_102_pt3 = zero 
wm_interm_103_pt3 = zero 
wm_interm_104_pt3 = zero 
wm_interm_105_pt3 = zero 
wm_interm_106_pt3 = zero 
wm_interm_107_pt3 = zero 
wm_interm_108_pt3 = zero 
wm_interm_109_pt3 = zero 
wm_interm_110_pt3 = zero 
wm_interm_111_pt3 = zero 
wm_interm_112_pt3 = zero 
wm_interm_113_pt3 = zero 
wm_interm_114_pt3 = zero 
wm_interm_115_pt3 = zero 
wm_interm_116_pt3 = zero 
wm_interm_117_pt3 = zero 
wm_interm_118_pt3 = zero 
wm_interm_119_pt3 = zero 
wm_interm_120_pt3 = zero 
wm_interm_121_pt3 = zero 
wm_interm_122_pt3 = zero 
wm_interm_123_pt3 = zero 
wm_interm_124_pt3 = zero 
wm_interm_125_pt3 = zero 
wm_interm_126_pt3 = zero 
wm_interm_127_pt3 = zero 
wm_interm_128_pt3 = zero 
wm_interm_129_pt3 = zero 
wm_interm_130_pt3 = zero 
wm_interm_131_pt3 = zero 
wm_interm_132_pt3 = zero 
wm_interm_133_pt3 = zero 
wm_interm_134_pt3 = zero 
wm_interm_135_pt3 = zero 
wm_interm_136_pt3 = zero 
wm_interm_137_pt3 = zero 
wm_interm_138_pt3 = zero 
wm_interm_139_pt3 = zero 
wm_interm_140_pt3 = zero 
wm_interm_141_pt3 = zero 
wm_interm_142_pt3 = zero 
wm_interm_143_pt3 = zero 
wm_interm_144_pt3 = zero 
wm_interm_145_pt3 = zero 
wm_interm_146_pt3 = zero 
wm_interm_147_pt3 = zero 
wm_interm_148_pt3 = zero 
wm_interm_149_pt3 = zero 
wm_interm_150_pt3 = zero 
wm_interm_151_pt3 = zero 
wm_interm_152_pt3 = zero 
wm_interm_153_pt3 = zero 
wm_interm_154_pt3 = zero 
wm_interm_155_pt3 = zero 
wm_interm_156_pt3 = zero 
wm_interm_157_pt3 = zero 
wm_interm_158_pt3 = zero 
wm_interm_159_pt3 = zero 
wm_interm_160_pt3 = zero 
wm_interm_161_pt3 = zero 
wm_interm_162_pt3 = zero 
wm_interm_163_pt3 = zero 
wm_interm_164_pt3 = zero 
wm_interm_165_pt3 = zero 
wm_interm_166_pt3 = zero 
wm_interm_167_pt3 = zero 
wm_interm_168_pt3 = zero 
wm_interm_169_pt3 = zero 
wm_interm_170_pt3 = zero 

    end subroutine wm_intermediates_cc3_init_pt3
    
    subroutine wm_intermediates_cc3_free_pt3
    deallocate(wm_interm_0_pt3)
deallocate(wm_interm_1_pt3)
deallocate(wm_interm_2_pt3)
deallocate(wm_interm_3_pt3)
deallocate(wm_interm_4_pt3)
deallocate(wm_interm_5_pt3)
deallocate(wm_interm_6_pt3)
deallocate(wm_interm_7_pt3)
deallocate(wm_interm_8_pt3)
deallocate(wm_interm_9_pt3)
deallocate(wm_interm_10_pt3)
deallocate(wm_interm_11_pt3)
deallocate(wm_interm_12_pt3)
deallocate(wm_interm_13_pt3)
deallocate(wm_interm_14_pt3)
deallocate(wm_interm_15_pt3)
deallocate(wm_interm_16_pt3)
deallocate(wm_interm_17_pt3)
deallocate(wm_interm_18_pt3)
deallocate(wm_interm_19_pt3)
deallocate(wm_interm_20_pt3)
deallocate(wm_interm_21_pt3)
deallocate(wm_interm_22_pt3)
deallocate(wm_interm_23_pt3)
deallocate(wm_interm_24_pt3)
deallocate(wm_interm_25_pt3)
deallocate(wm_interm_26_pt3)
deallocate(wm_interm_27_pt3)
deallocate(wm_interm_28_pt3)
deallocate(wm_interm_29_pt3)
deallocate(wm_interm_30_pt3)
deallocate(wm_interm_31_pt3)
deallocate(wm_interm_32_pt3)
deallocate(wm_interm_33_pt3)
deallocate(wm_interm_34_pt3)
deallocate(wm_interm_35_pt3)
deallocate(wm_interm_36_pt3)
deallocate(wm_interm_37_pt3)
deallocate(wm_interm_38_pt3)
deallocate(wm_interm_39_pt3)
deallocate(wm_interm_40_pt3)
deallocate(wm_interm_41_pt3)
deallocate(wm_interm_43_pt3)
deallocate(wm_interm_44_pt3)
deallocate(wm_interm_45_pt3)
deallocate(wm_interm_46_pt3)
deallocate(wm_interm_47_pt3)
deallocate(wm_interm_48_pt3)
deallocate(wm_interm_49_pt3)
deallocate(wm_interm_50_pt3)
deallocate(wm_interm_51_pt3)
deallocate(wm_interm_52_pt3)
deallocate(wm_interm_53_pt3)
deallocate(wm_interm_54_pt3)
deallocate(wm_interm_55_pt3)
deallocate(wm_interm_56_pt3)
deallocate(wm_interm_57_pt3)
deallocate(wm_interm_58_pt3)
deallocate(wm_interm_59_pt3)
deallocate(wm_interm_60_pt3)
deallocate(wm_interm_61_pt3)
deallocate(wm_interm_62_pt3)
deallocate(wm_interm_63_pt3)
deallocate(wm_interm_64_pt3)
deallocate(wm_interm_65_pt3)
deallocate(wm_interm_66_pt3)
deallocate(wm_interm_67_pt3)
deallocate(wm_interm_68_pt3)
deallocate(wm_interm_69_pt3)
deallocate(wm_interm_70_pt3)
deallocate(wm_interm_71_pt3)
deallocate(wm_interm_72_pt3)
deallocate(wm_interm_73_pt3)
deallocate(wm_interm_74_pt3)
deallocate(wm_interm_75_pt3)
deallocate(wm_interm_76_pt3)
deallocate(wm_interm_77_pt3)
deallocate(wm_interm_78_pt3)
deallocate(wm_interm_79_pt3)
deallocate(wm_interm_80_pt3)
deallocate(wm_interm_81_pt3)
deallocate(wm_interm_82_pt3)
deallocate(wm_interm_83_pt3)
deallocate(wm_interm_84_pt3)
deallocate(wm_interm_85_pt3)
deallocate(wm_interm_86_pt3)
deallocate(wm_interm_87_pt3)
deallocate(wm_interm_88_pt3)
deallocate(wm_interm_89_pt3)
deallocate(wm_interm_90_pt3)
deallocate(wm_interm_91_pt3)
deallocate(wm_interm_92_pt3)
deallocate(wm_interm_93_pt3)
deallocate(wm_interm_94_pt3)
deallocate(wm_interm_95_pt3)
deallocate(wm_interm_96_pt3)
deallocate(wm_interm_97_pt3)
deallocate(wm_interm_98_pt3)
deallocate(wm_interm_99_pt3)
deallocate(wm_interm_100_pt3)
deallocate(wm_interm_101_pt3)
deallocate(wm_interm_102_pt3)
deallocate(wm_interm_103_pt3)
deallocate(wm_interm_104_pt3)
deallocate(wm_interm_105_pt3)
deallocate(wm_interm_106_pt3)
deallocate(wm_interm_107_pt3)
deallocate(wm_interm_108_pt3)
deallocate(wm_interm_109_pt3)
deallocate(wm_interm_110_pt3)
deallocate(wm_interm_111_pt3)
deallocate(wm_interm_112_pt3)
deallocate(wm_interm_113_pt3)
deallocate(wm_interm_114_pt3)
deallocate(wm_interm_115_pt3)
deallocate(wm_interm_116_pt3)
deallocate(wm_interm_117_pt3)
deallocate(wm_interm_118_pt3)
deallocate(wm_interm_119_pt3)
deallocate(wm_interm_120_pt3)
deallocate(wm_interm_121_pt3)
deallocate(wm_interm_124_pt3)
deallocate(wm_interm_125_pt3)
deallocate(wm_interm_126_pt3)
deallocate(wm_interm_127_pt3)
deallocate(wm_interm_128_pt3)
deallocate(wm_interm_129_pt3)
deallocate(wm_interm_130_pt3)
deallocate(wm_interm_131_pt3)
deallocate(wm_interm_132_pt3)
deallocate(wm_interm_133_pt3)
deallocate(wm_interm_134_pt3)
deallocate(wm_interm_135_pt3)
deallocate(wm_interm_136_pt3)
deallocate(wm_interm_137_pt3)
deallocate(wm_interm_138_pt3)
deallocate(wm_interm_139_pt3)
deallocate(wm_interm_140_pt3)
deallocate(wm_interm_141_pt3)
deallocate(wm_interm_142_pt3)
deallocate(wm_interm_143_pt3)
deallocate(wm_interm_144_pt3)
deallocate(wm_interm_145_pt3)
deallocate(wm_interm_146_pt3)
deallocate(wm_interm_147_pt3)
deallocate(wm_interm_148_pt3)
deallocate(wm_interm_149_pt3)
deallocate(wm_interm_150_pt3)
deallocate(wm_interm_151_pt3)
deallocate(wm_interm_152_pt3)
deallocate(wm_interm_153_pt3)
deallocate(wm_interm_154_pt3)
deallocate(wm_interm_155_pt3)
deallocate(wm_interm_156_pt3)
deallocate(wm_interm_157_pt3)
deallocate(wm_interm_158_pt3)
deallocate(wm_interm_159_pt3)
deallocate(wm_interm_160_pt3)
deallocate(wm_interm_161_pt3)
deallocate(wm_interm_162_pt3)
deallocate(wm_interm_163_pt3)
deallocate(wm_interm_164_pt3)
deallocate(wm_interm_165_pt3)
deallocate(wm_interm_166_pt3)
deallocate(wm_interm_167_pt3)
deallocate(wm_interm_168_pt3)
deallocate(wm_interm_169_pt3)
deallocate(wm_interm_170_pt3)

    end subroutine wm_intermediates_cc3_free_pt3
    
    subroutine wm_intermediates_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, c, k, l, m 

    !$omp parallel private(a, b, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j)
wm_interm_0_pt3(a, b, i, j) = wm_interm_0_pt3(a, b, i, j) + sum 
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
sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_1_pt3(c, k, j, l) = wm_interm_1_pt3(c, k, j, l) + sum 
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
sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_2_pt3(c, j, k, l) = wm_interm_2_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_3_pt3(c, k, j, l) = wm_interm_3_pt3(c, k, j, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_4_pt3(i, j) = wm_interm_4_pt3(i, j) + sum 
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
wm_interm_5_pt3(b, i, j, k) = wm_interm_5_pt3(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
wm_interm_6_pt3(b, c, j, k) = wm_interm_6_pt3(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
wm_interm_7_pt3(b, c, j, k) = wm_interm_7_pt3(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
wm_interm_8_pt3(b, c, j, k) = wm_interm_8_pt3(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_9_pt3(b, j) = wm_interm_9_pt3(b, j) + sum 
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
wm_interm_10_pt3(b, j) = wm_interm_10_pt3(b, j) + sum 
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
wm_interm_11_pt3(b, j, i, k) = wm_interm_11_pt3(b, j, i, k) + sum 
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
wm_interm_12_pt3(i, j) = wm_interm_12_pt3(i, j) + sum 
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
wm_interm_13_pt3(c, k, j, l) = wm_interm_13_pt3(c, k, j, l) + sum 
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
wm_interm_14_pt3(c, k, j, l) = wm_interm_14_pt3(c, k, j, l) + sum 
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
wm_interm_15_pt3(c, j, k, l) = wm_interm_15_pt3(c, j, k, l) + sum 
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
wm_interm_16_pt3(c, j, k, l) = wm_interm_16_pt3(c, j, k, l) + sum 
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
wm_interm_17_pt3(c, j, k, l) = wm_interm_17_pt3(c, j, k, l) + sum 
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
wm_interm_18_pt3(c, j, k, l) = wm_interm_18_pt3(c, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * s1(b,j)
wm_interm_19_pt3(a, b, i, j) = wm_interm_19_pt3(a, b, i, j) + sum 
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
wm_interm_20_pt3(c, k) = wm_interm_20_pt3(c, k) + sum 
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
wm_interm_21_pt3(c, k) = wm_interm_21_pt3(c, k) + sum 
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
wm_interm_22_pt3(c, k) = wm_interm_22_pt3(c, k) + sum 
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
wm_interm_23_pt3(c, k) = wm_interm_23_pt3(c, k) + sum 
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
wm_interm_24_pt3(b, c, j, k) = wm_interm_24_pt3(b, c, j, k) + sum 
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
wm_interm_25_pt3(b, c, j, k) = wm_interm_25_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s1(a,i)
end do 
end do 
wm_interm_26_pt3(b, c, j, k) = wm_interm_26_pt3(b, c, j, k) + sum 
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
wm_interm_27_pt3(b, c, k, j) = wm_interm_27_pt3(b, c, k, j) + sum 
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
wm_interm_28_pt3(b, c, k, j) = wm_interm_28_pt3(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s1(a,i)
end do 
end do 
wm_interm_29_pt3(b, c, j, k) = wm_interm_29_pt3(b, c, j, k) + sum 
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
wm_interm_30_pt3(a, b) = wm_interm_30_pt3(a, b) + sum 
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
wm_interm_31_pt3(a, b, i, j) = wm_interm_31_pt3(a, b, i, j) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,l,i,k)
end do 
end do 
end do 
wm_interm_32_pt3(c, j, l, k) = wm_interm_32_pt3(c, j, l, k) + sum 
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
wm_interm_33_pt3(a, b) = wm_interm_33_pt3(a, b) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_34_pt3(c, k) = wm_interm_34_pt3(c, k) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_35_pt3(c, k) = wm_interm_35_pt3(c, k) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,l,i,k)
end do 
end do 
end do 
wm_interm_36_pt3(c, j, l, k) = wm_interm_36_pt3(c, j, l, k) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_37_pt3(c, k) = wm_interm_37_pt3(c, k) + sum 
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
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_38_pt3(c, j, k, l) = wm_interm_38_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_39_pt3(c, k) = wm_interm_39_pt3(c, k) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_40_pt3(c, j, k, l) = wm_interm_40_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_41_pt3(c, j, k, l) = wm_interm_41_pt3(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_42_pt3 = wm_interm_42_pt3 + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
wm_interm_43_pt3(b, c, j, k) = wm_interm_43_pt3(b, c, j, k) + sum 
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
wm_interm_44_pt3(b, i, j, k) = wm_interm_44_pt3(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
wm_interm_45_pt3(b, c, j, k) = wm_interm_45_pt3(b, c, j, k) + sum 
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
wm_interm_46_pt3(b, j) = wm_interm_46_pt3(b, j) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_47_pt3(c, j, k, l) = wm_interm_47_pt3(c, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
wm_interm_48_pt3(b, c, j, k) = wm_interm_48_pt3(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,i,j)
end do 
end do 
wm_interm_49_pt3(b, j) = wm_interm_49_pt3(b, j) + sum 
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
wm_interm_50_pt3(b, c, j, k) = wm_interm_50_pt3(b, c, j, k) + sum 
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
sum = sum + s1(a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_51_pt3(b, j) = wm_interm_51_pt3(b, j) + sum 
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
wm_interm_52_pt3(b, c, j, k) = wm_interm_52_pt3(b, c, j, k) + sum 
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
wm_interm_53_pt3(b, c, k, j) = wm_interm_53_pt3(b, c, k, j) + sum 
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
wm_interm_54_pt3(b, c, j, k) = wm_interm_54_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_55_pt3(b, c, k, j) = wm_interm_55_pt3(b, c, k, j) + sum 
end do 
end do 
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
wm_interm_56_pt3(b, i, k, j) = wm_interm_56_pt3(b, i, k, j) + sum 
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
sum = sum + s1(a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_57_pt3(b, j) = wm_interm_57_pt3(b, j) + sum 
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
wm_interm_58_pt3(c, j, k, l) = wm_interm_58_pt3(c, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_59_pt3(i, j) = wm_interm_59_pt3(i, j) + sum 
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
wm_interm_60_pt3(a, b, i, j) = wm_interm_60_pt3(a, b, i, j) + sum 
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
wm_interm_61_pt3(c, j, k, l) = wm_interm_61_pt3(c, j, k, l) + sum 
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
wm_interm_62_pt3(c, k, j, l) = wm_interm_62_pt3(c, k, j, l) + sum 
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
wm_interm_63_pt3(c, k, j, l) = wm_interm_63_pt3(c, k, j, l) + sum 
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
wm_interm_64_pt3(c, j, k, l) = wm_interm_64_pt3(c, j, k, l) + sum 
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
wm_interm_65_pt3(c, j, k, l) = wm_interm_65_pt3(c, j, k, l) + sum 
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
wm_interm_66_pt3(c, k) = wm_interm_66_pt3(c, k) + sum 
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
wm_interm_67_pt3(a, b) = wm_interm_67_pt3(a, b) + sum 
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
wm_interm_68_pt3(c, k) = wm_interm_68_pt3(c, k) + sum 
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
wm_interm_69_pt3(c, k) = wm_interm_69_pt3(c, k) + sum 
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
wm_interm_70_pt3(c, k) = wm_interm_70_pt3(c, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t1(b,j)
wm_interm_71_pt3(a, b, i, j) = wm_interm_71_pt3(a, b, i, j) + sum 
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
wm_interm_72_pt3(b, c, j, k) = wm_interm_72_pt3(b, c, j, k) + sum 
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
wm_interm_73_pt3(b, c, k, j) = wm_interm_73_pt3(b, c, k, j) + sum 
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
wm_interm_74_pt3(b, c, j, k) = wm_interm_74_pt3(b, c, j, k) + sum 
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
wm_interm_75_pt3(b, c, k, j) = wm_interm_75_pt3(b, c, k, j) + sum 
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
wm_interm_76_pt3(b, c, j, k) = wm_interm_76_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_77_pt3(b, c, j, k) = wm_interm_77_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_78_pt3(b, c, j, k) = wm_interm_78_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_79_pt3(i, j, k, l) = wm_interm_79_pt3(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_80_pt3(b, c, j, k) = wm_interm_80_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_81_pt3(b, c, j, k) = wm_interm_81_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_82_pt3(j, k) = wm_interm_82_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_83_pt3(j, k) = wm_interm_83_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_84_pt3(c, j, k, l) = wm_interm_84_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_85_pt3(c, j, k, l) = wm_interm_85_pt3(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,m)
end do 
end do 
wm_interm_86_pt3(c, i, j, k, l, m) = wm_interm_86_pt3(c, i, j, k, l, m) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_87_pt3(b, c, j, k) = wm_interm_87_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_88_pt3(c, j, k, l) = wm_interm_88_pt3(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_89_pt3(c, k) = wm_interm_89_pt3(c, k) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_90_pt3(c, k) = wm_interm_90_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
end do 
end do 
wm_interm_91_pt3(c, k) = wm_interm_91_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 
wm_interm_92_pt3(c, k) = wm_interm_92_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_93_pt3(c, k) = wm_interm_93_pt3(c, k) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_94_pt3(c, k) = wm_interm_94_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_95_pt3(b, c) = wm_interm_95_pt3(b, c) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_96_pt3(b, c) = wm_interm_96_pt3(b, c) + sum 
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
wm_interm_97_pt3(b, c, j, k) = wm_interm_97_pt3(b, c, j, k) + sum 
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
wm_interm_98_pt3(b, c, j, k) = wm_interm_98_pt3(b, c, j, k) + sum 
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
wm_interm_99_pt3(b, c) = wm_interm_99_pt3(b, c) + sum 
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
wm_interm_100_pt3(b, c) = wm_interm_100_pt3(b, c) + sum 
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
sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t2(a,b,l,m)
end do 
end do 
wm_interm_101_pt3(c, j, k, i, l, m) = wm_interm_101_pt3(c, j, k, i, l, m) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_102_pt3(b, c, j, k) = wm_interm_102_pt3(b, c, j, k) + sum 
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
wm_interm_103_pt3(j, k) = wm_interm_103_pt3(j, k) + sum 
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
wm_interm_104_pt3(i, j, k, l) = wm_interm_104_pt3(i, j, k, l) + sum 
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
wm_interm_105_pt3(j, k) = wm_interm_105_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_106_pt3(c, j, k, l) = wm_interm_106_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_107_pt3(c, j, k, l) = wm_interm_107_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_108_pt3(c, k) = wm_interm_108_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_109_pt3(b, c, j, k) = wm_interm_109_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_110_pt3(b, c, j, k) = wm_interm_110_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_111_pt3(b, c, j, k) = wm_interm_111_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_112_pt3(b, c, j, k) = wm_interm_112_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_113_pt3(c, k) = wm_interm_113_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_114_pt3(c, j, k, l) = wm_interm_114_pt3(c, j, k, l) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_115_pt3(c, k) = wm_interm_115_pt3(c, k) + sum 
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
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_116_pt3(c, k) = wm_interm_116_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,l,k,i)
end do 
end do 
end do 
wm_interm_117_pt3(c, j, l, k) = wm_interm_117_pt3(c, j, l, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,m)
end do 
end do 
wm_interm_118_pt3(c, i, j, k, l, m) = wm_interm_118_pt3(c, i, j, k, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, l, m, k, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
do k = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,l,m,k)
end do 
end do 
wm_interm_119_pt3(c, i, j, l, m, k) = wm_interm_119_pt3(c, i, j, l, m, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_120_pt3(b, c) = wm_interm_120_pt3(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_121_pt3(b, c) = wm_interm_121_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_122_pt3 = wm_interm_122_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_123_pt3 = wm_interm_123_pt3 + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,m)
end do 
end do 
wm_interm_124_pt3(c, i, j, k, l, m) = wm_interm_124_pt3(c, i, j, k, l, m) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_125_pt3(i, j, k, l) = wm_interm_125_pt3(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_126_pt3(j, k) = wm_interm_126_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_127_pt3(j, k) = wm_interm_127_pt3(j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_128_pt3(b, i, j, k) = wm_interm_128_pt3(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,l,k,i)
end do 
end do 
end do 
wm_interm_129_pt3(c, j, l, k) = wm_interm_129_pt3(c, j, l, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_130_pt3(b, j) = wm_interm_130_pt3(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_131_pt3(b, j) = wm_interm_131_pt3(b, j) + sum 
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
sum = sum + t3(nocc, nactive, a,b,c,k,j,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_132_pt3(c, k, j, l) = wm_interm_132_pt3(c, k, j, l) + sum 
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
wm_interm_133_pt3(b, j) = wm_interm_133_pt3(b, j) + sum 
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
wm_interm_134_pt3(b, j) = wm_interm_134_pt3(b, j) + sum 
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
wm_interm_135_pt3(c, i, j, k, l, m) = wm_interm_135_pt3(c, i, j, k, l, m) + sum 
end do 
end do 
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
wm_interm_136_pt3(c, j, l, k) = wm_interm_136_pt3(c, j, l, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,i,b,k,c,l)
end do 
end do 
end do 
wm_interm_137_pt3(c, j, k, l) = wm_interm_137_pt3(c, j, k, l) + sum 
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
wm_interm_138_pt3(c, j, k, l) = wm_interm_138_pt3(c, j, k, l) + sum 
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
wm_interm_139_pt3(b, j, i, k) = wm_interm_139_pt3(b, j, i, k) + sum 
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
wm_interm_140_pt3(c, j, k, i, l, m) = wm_interm_140_pt3(c, j, k, i, l, m) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_141_pt3(c, j, k, l) = wm_interm_141_pt3(c, j, k, l) + sum 
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
wm_interm_142_pt3(b, i, j, k) = wm_interm_142_pt3(b, i, j, k) + sum 
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
wm_interm_143_pt3(b, i, j, k) = wm_interm_143_pt3(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_144_pt3(b, j) = wm_interm_144_pt3(b, j) + sum 
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
wm_interm_145_pt3(b, j) = wm_interm_145_pt3(b, j) + sum 
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
wm_interm_146_pt3(b, j, i, k) = wm_interm_146_pt3(b, j, i, k) + sum 
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
wm_interm_147_pt3(c, k) = wm_interm_147_pt3(c, k) + sum 
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
wm_interm_148_pt3(c, k) = wm_interm_148_pt3(c, k) + sum 
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
wm_interm_149_pt3(c, k) = wm_interm_149_pt3(c, k) + sum 
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
wm_interm_150_pt3(c, k) = wm_interm_150_pt3(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_151_pt3(b, j) = wm_interm_151_pt3(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_152_pt3(b, j) = wm_interm_152_pt3(b, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_153_pt3(c, j, k, l) = wm_interm_153_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_154_pt3(c, j, k, l) = wm_interm_154_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_155_pt3(c, k) = wm_interm_155_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_156_pt3(c, k) = wm_interm_156_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_157_pt3(c, j, k, l) = wm_interm_157_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_158_pt3(c, j, k, l) = wm_interm_158_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_159_pt3(c, k) = wm_interm_159_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_160_pt3(c, k) = wm_interm_160_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_161_pt3(c, k, j, l) = wm_interm_161_pt3(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_162_pt3(c, k, j, l) = wm_interm_162_pt3(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_163_pt3(c, j, k, i, l, m) = wm_interm_163_pt3(c, j, k, i, l, m) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * r2(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_164_pt3(c, i, k, j, l, m) = wm_interm_164_pt3(c, i, k, j, l, m) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_165_pt3(c, i, j, k, l, m) = wm_interm_165_pt3(c, i, j, k, l, m) + sum 
end do 
end do 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_166_pt3(b, j, i, k) = wm_interm_166_pt3(b, j, i, k) + sum 
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
wm_interm_167_pt3(c, i, j, k, l, m) = wm_interm_167_pt3(c, i, j, k, l, m) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * t2(a,b,l,m)
end do 
end do 
wm_interm_168_pt3(c, i, k, j, l, m) = wm_interm_168_pt3(c, i, k, j, l, m) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,m)
end do 
end do 
wm_interm_169_pt3(c, j, k, i, l, m) = wm_interm_169_pt3(c, j, k, i, l, m) + sum 
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
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,j,k)
end do 
wm_interm_170_pt3(b, i, j, k) = wm_interm_170_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_cc3_pt3
    


  end module ss_cc3_pt3a
