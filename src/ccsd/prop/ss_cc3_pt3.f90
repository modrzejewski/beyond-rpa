module ss_cc3_pt3
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
    

    
    function calc_D_oo_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, b 
    real(F64), dimension(0:739) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_128_pt3(a,i,j,p) * wm_interm_1_pt3(a,j,q,i)
term(1) = term(1) + wm_interm_128_pt3(a,i,j,p) * wm_interm_2_pt3(a,j,q,i)
term(2) = term(2) + wm_interm_128_pt3(a,i,j,p) * wm_interm_1_pt3(a,q,j,i)
term(3) = term(3) + wm_interm_128_pt3(a,i,j,p) * wm_interm_2_pt3(a,q,j,i)
term(4) = term(4) + wm_interm_128_pt3(a,i,j,p) * wm_interm_3_pt3(a,j,q,i)
term(5) = term(5) + wm_interm_128_pt3(a,i,j,p) * wm_interm_3_pt3(a,q,j,i)
term(6) = term(6) + wm_interm_143_pt3(a,i,j,p) * wm_interm_32_pt3(a,q,j,i)
term(7) = term(7) + wm_interm_143_pt3(a,i,j,p) * wm_interm_36_pt3(a,q,j,i)
term(8) = term(8) + wm_interm_146_pt3(a,i,j,p) * wm_interm_32_pt3(a,q,j,i)
term(9) = term(9) + wm_interm_146_pt3(a,i,j,p) * wm_interm_36_pt3(a,q,j,i)
term(10) = term(10) + wm_interm_11_pt3(a,i,j,q) * wm_interm_141_pt3(a,p,j,i)
term(11) = term(11) + wm_interm_11_pt3(a,i,j,q) * wm_interm_137_pt3(a,p,j,i)
term(12) = term(12) + wm_interm_11_pt3(a,i,j,q) * wm_interm_138_pt3(a,p,j,i)
term(13) = term(13) + wm_interm_11_pt3(a,i,j,q) * wm_interm_136_pt3(a,p,j,i)
term(14) = term(14) + wm_interm_153_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,j,i)
term(15) = term(15) + wm_interm_154_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,j,i)
term(16) = term(16) + wm_interm_157_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,j,i)
term(17) = term(17) + wm_interm_158_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,j,i)
term(18) = term(18) + wm_interm_161_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,j,i)
term(19) = term(19) + wm_interm_162_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-3.9999999999999996d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (1.9999999999999998d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (8.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 
term(10) = term(10) * (-1.9999999999999998d+0) 
term(11) = term(11) * (-3.9999999999999996d+0) 
term(12) = term(12) * (7.999999999999999d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(19) = term(19) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + wm_interm_128_pt3(a,i,p,j) * wm_interm_1_pt3(a,j,q,i)
term(21) = term(21) + wm_interm_128_pt3(a,i,p,j) * wm_interm_2_pt3(a,j,q,i)
term(22) = term(22) + wm_interm_128_pt3(a,i,p,j) * wm_interm_1_pt3(a,q,j,i)
term(23) = term(23) + wm_interm_128_pt3(a,i,p,j) * wm_interm_2_pt3(a,q,j,i)
term(24) = term(24) + wm_interm_128_pt3(a,i,p,j) * wm_interm_3_pt3(a,j,q,i)
term(25) = term(25) + wm_interm_128_pt3(a,i,p,j) * wm_interm_3_pt3(a,q,j,i)
term(26) = term(26) + wm_interm_5_pt3(a,q,i,j) * wm_interm_84_pt3(a,p,j,i)
term(27) = term(27) + wm_interm_5_pt3(a,q,i,j) * wm_interm_85_pt3(a,p,j,i)
term(28) = term(28) + wm_interm_5_pt3(a,q,i,j) * wm_interm_88_pt3(a,p,j,i)
term(29) = term(29) + wm_interm_129_pt3(a,p,i,j) * wm_interm_5_pt3(a,q,j,i)
term(30) = term(30) + wm_interm_129_pt3(a,p,i,j) * wm_interm_5_pt3(a,q,i,j)
term(31) = term(31) + wm_interm_5_pt3(a,q,i,j) * wm_interm_88_pt3(a,p,i,j)
term(32) = term(32) + wm_interm_5_pt3(a,q,i,j) * wm_interm_84_pt3(a,p,i,j)
term(33) = term(33) + wm_interm_5_pt3(a,q,i,j) * wm_interm_85_pt3(a,p,i,j)
term(34) = term(34) + wm_interm_128_pt3(a,p,i,j) * wm_interm_1_pt3(a,i,j,q)
term(35) = term(35) + wm_interm_128_pt3(a,p,i,j) * wm_interm_1_pt3(a,j,i,q)
term(36) = term(36) + wm_interm_5_pt3(a,i,q,j) * wm_interm_84_pt3(a,i,j,p)
term(37) = term(37) + wm_interm_5_pt3(a,i,q,j) * wm_interm_85_pt3(a,i,j,p)
term(38) = term(38) + wm_interm_128_pt3(a,p,i,j) * wm_interm_2_pt3(a,j,i,q)
term(39) = term(39) + wm_interm_128_pt3(a,p,i,j) * wm_interm_2_pt3(a,i,j,q)
term(40) = term(40) + wm_interm_5_pt3(a,i,q,j) * wm_interm_88_pt3(a,i,j,p)
term(41) = term(41) + wm_interm_5_pt3(a,i,q,j) * wm_interm_88_pt3(a,i,p,j)
term(42) = term(42) + wm_interm_128_pt3(a,p,i,j) * wm_interm_3_pt3(a,i,j,q)
term(43) = term(43) + wm_interm_128_pt3(a,p,i,j) * wm_interm_3_pt3(a,j,i,q)
term(44) = term(44) + wm_interm_5_pt3(a,i,q,j) * wm_interm_84_pt3(a,i,p,j)
term(45) = term(45) + wm_interm_5_pt3(a,i,q,j) * wm_interm_85_pt3(a,i,p,j)
term(46) = term(46) + wm_interm_5_pt3(a,i,j,q) * wm_interm_84_pt3(a,i,p,j)
term(47) = term(47) + wm_interm_5_pt3(a,i,j,q) * wm_interm_85_pt3(a,i,p,j)
term(48) = term(48) + wm_interm_5_pt3(a,i,j,q) * wm_interm_88_pt3(a,i,p,j)
term(49) = term(49) + wm_interm_5_pt3(a,i,j,q) * wm_interm_88_pt3(a,i,j,p)
term(50) = term(50) + wm_interm_5_pt3(a,i,j,q) * wm_interm_84_pt3(a,i,j,p)
term(51) = term(51) + wm_interm_5_pt3(a,i,j,q) * wm_interm_85_pt3(a,i,j,p)
term(52) = term(52) + wm_interm_128_pt3(a,p,i,j) * wm_interm_132_pt3(a,i,j,q)
term(53) = term(53) + wm_interm_128_pt3(a,p,i,j) * wm_interm_132_pt3(a,j,i,q)
term(54) = term(54) + wm_interm_11_pt3(a,i,q,j) * wm_interm_136_pt3(a,j,p,i)
term(55) = term(55) + wm_interm_11_pt3(a,i,q,j) * wm_interm_137_pt3(a,j,p,i)
term(56) = term(56) + wm_interm_11_pt3(a,i,q,j) * wm_interm_137_pt3(a,j,i,p)
term(57) = term(57) + wm_interm_11_pt3(a,i,q,j) * wm_interm_136_pt3(a,j,i,p)
term(58) = term(58) + wm_interm_11_pt3(a,i,q,j) * wm_interm_138_pt3(a,j,i,p)
term(59) = term(59) + wm_interm_11_pt3(a,i,q,j) * wm_interm_138_pt3(a,j,p,i)
term(60) = term(60) + wm_interm_11_pt3(a,q,i,j) * wm_interm_136_pt3(a,j,i,p)
term(61) = term(61) + wm_interm_11_pt3(a,q,i,j) * wm_interm_137_pt3(a,j,i,p)
term(62) = term(62) + wm_interm_11_pt3(a,q,i,j) * wm_interm_137_pt3(a,j,p,i)
term(63) = term(63) + wm_interm_11_pt3(a,q,i,j) * wm_interm_136_pt3(a,j,p,i)
term(64) = term(64) + wm_interm_11_pt3(a,q,i,j) * wm_interm_138_pt3(a,j,p,i)
term(65) = term(65) + wm_interm_11_pt3(a,q,i,j) * wm_interm_138_pt3(a,j,i,p)
term(66) = term(66) + wm_interm_139_pt3(a,q,i,j) * wm_interm_13_pt3(a,i,p,j)
term(67) = term(67) + wm_interm_139_pt3(a,q,i,j) * wm_interm_14_pt3(a,i,p,j)
term(68) = term(68) + wm_interm_139_pt3(a,q,i,j) * wm_interm_15_pt3(a,i,p,j)
term(69) = term(69) + wm_interm_139_pt3(a,q,i,j) * wm_interm_16_pt3(a,i,p,j)
term(70) = term(70) + wm_interm_139_pt3(a,q,i,j) * wm_interm_13_pt3(a,p,i,j)
term(71) = term(71) + wm_interm_139_pt3(a,q,i,j) * wm_interm_14_pt3(a,p,i,j)
term(72) = term(72) + wm_interm_139_pt3(a,i,q,j) * wm_interm_13_pt3(a,p,i,j)
term(73) = term(73) + wm_interm_139_pt3(a,i,q,j) * wm_interm_14_pt3(a,p,i,j)
term(74) = term(74) + wm_interm_139_pt3(a,i,q,j) * wm_interm_16_pt3(a,i,p,j)
term(75) = term(75) + wm_interm_139_pt3(a,i,q,j) * wm_interm_15_pt3(a,i,p,j)
term(76) = term(76) + wm_interm_139_pt3(a,i,q,j) * wm_interm_13_pt3(a,i,p,j)
term(77) = term(77) + wm_interm_139_pt3(a,i,q,j) * wm_interm_14_pt3(a,i,p,j)
term(78) = term(78) + wm_interm_11_pt3(a,i,q,j) * wm_interm_141_pt3(a,j,i,p)
term(79) = term(79) + wm_interm_11_pt3(a,i,q,j) * wm_interm_141_pt3(a,j,p,i)
term(80) = term(80) + wm_interm_11_pt3(a,q,i,j) * wm_interm_141_pt3(a,j,p,i)
term(81) = term(81) + wm_interm_11_pt3(a,q,i,j) * wm_interm_141_pt3(a,j,i,p)
term(82) = term(82) + wm_interm_142_pt3(a,q,i,j) * wm_interm_16_pt3(a,i,p,j)
term(83) = term(83) + wm_interm_142_pt3(a,q,i,j) * wm_interm_14_pt3(a,p,i,j)
term(84) = term(84) + wm_interm_13_pt3(a,p,i,j) * wm_interm_142_pt3(a,q,i,j)
term(85) = term(85) + wm_interm_13_pt3(a,i,p,j) * wm_interm_142_pt3(a,q,i,j)
term(86) = term(86) + wm_interm_142_pt3(a,q,i,j) * wm_interm_15_pt3(a,i,p,j)
term(87) = term(87) + wm_interm_142_pt3(a,q,i,j) * wm_interm_14_pt3(a,i,p,j)
term(88) = term(88) + wm_interm_142_pt3(a,i,q,j) * wm_interm_15_pt3(a,i,p,j)
term(89) = term(89) + wm_interm_142_pt3(a,i,q,j) * wm_interm_14_pt3(a,i,p,j)
term(90) = term(90) + wm_interm_13_pt3(a,i,p,j) * wm_interm_142_pt3(a,i,q,j)
term(91) = term(91) + wm_interm_13_pt3(a,p,i,j) * wm_interm_142_pt3(a,i,q,j)
term(92) = term(92) + wm_interm_142_pt3(a,i,q,j) * wm_interm_16_pt3(a,i,p,j)
term(93) = term(93) + wm_interm_142_pt3(a,i,q,j) * wm_interm_14_pt3(a,p,i,j)
term(94) = term(94) + wm_interm_146_pt3(a,i,j,p) * wm_interm_32_pt3(a,q,i,j)
term(95) = term(95) + wm_interm_146_pt3(a,i,j,p) * wm_interm_36_pt3(a,q,i,j)
term(96) = term(96) + wm_interm_143_pt3(a,i,j,p) * wm_interm_32_pt3(a,q,i,j)
term(97) = term(97) + wm_interm_143_pt3(a,i,j,p) * wm_interm_36_pt3(a,q,i,j)
term(98) = term(98) + wm_interm_106_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,j,i)
term(99) = term(99) + wm_interm_107_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,j,i)
term(100) = term(100) + wm_interm_106_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,i,j)
term(101) = term(101) + wm_interm_107_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,i,j)
term(102) = term(102) + wm_interm_114_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,j,i)
term(103) = term(103) + wm_interm_146_pt3(a,i,j,p) * wm_interm_38_pt3(a,q,i,j)
term(104) = term(104) + wm_interm_114_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,i,j)
term(105) = term(105) + wm_interm_146_pt3(a,i,j,p) * wm_interm_40_pt3(a,q,i,j)
term(106) = term(106) + wm_interm_143_pt3(a,i,j,p) * wm_interm_40_pt3(a,q,i,j)
term(107) = term(107) + wm_interm_143_pt3(a,i,j,p) * wm_interm_38_pt3(a,q,i,j)
term(108) = term(108) + wm_interm_117_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,i,j)
term(109) = term(109) + wm_interm_117_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,j,i)
term(110) = term(110) + wm_interm_11_pt3(a,i,j,q) * wm_interm_137_pt3(a,p,i,j)
term(111) = term(111) + wm_interm_11_pt3(a,i,j,q) * wm_interm_138_pt3(a,p,i,j)
term(112) = term(112) + wm_interm_11_pt3(a,i,j,q) * wm_interm_141_pt3(a,p,i,j)
term(113) = term(113) + wm_interm_11_pt3(a,i,j,q) * wm_interm_136_pt3(a,p,i,j)
term(114) = term(114) + wm_interm_13_pt3(a,i,j,p) * wm_interm_142_pt3(a,i,j,q)
term(115) = term(115) + wm_interm_13_pt3(a,i,j,p) * wm_interm_142_pt3(a,j,i,q)
term(116) = term(116) + wm_interm_139_pt3(a,i,j,q) * wm_interm_13_pt3(a,j,i,p)
term(117) = term(117) + wm_interm_139_pt3(a,i,j,q) * wm_interm_13_pt3(a,i,j,p)
term(118) = term(118) + wm_interm_142_pt3(a,i,j,q) * wm_interm_15_pt3(a,i,j,p)
term(119) = term(119) + wm_interm_142_pt3(a,i,j,q) * wm_interm_14_pt3(a,i,j,p)
term(120) = term(120) + wm_interm_142_pt3(a,i,j,q) * wm_interm_16_pt3(a,i,j,p)
term(121) = term(121) + wm_interm_142_pt3(a,i,j,q) * wm_interm_14_pt3(a,j,i,p)
term(122) = term(122) + wm_interm_139_pt3(a,i,j,q) * wm_interm_16_pt3(a,i,j,p)
term(123) = term(123) + wm_interm_139_pt3(a,i,j,q) * wm_interm_14_pt3(a,j,i,p)
term(124) = term(124) + wm_interm_139_pt3(a,i,j,q) * wm_interm_15_pt3(a,i,j,p)
term(125) = term(125) + wm_interm_139_pt3(a,i,j,q) * wm_interm_14_pt3(a,i,j,p)
term(126) = term(126) + wm_interm_146_pt3(a,i,p,j) * wm_interm_32_pt3(a,j,q,i)
term(127) = term(127) + wm_interm_143_pt3(a,i,p,j) * wm_interm_32_pt3(a,j,q,i)
term(128) = term(128) + wm_interm_146_pt3(a,p,i,j) * wm_interm_32_pt3(a,j,q,i)
term(129) = term(129) + wm_interm_143_pt3(a,p,i,j) * wm_interm_32_pt3(a,j,q,i)
term(130) = term(130) + wm_interm_146_pt3(a,p,i,j) * wm_interm_32_pt3(a,j,i,q)
term(131) = term(131) + wm_interm_146_pt3(a,i,p,j) * wm_interm_32_pt3(a,j,i,q)
term(132) = term(132) + wm_interm_143_pt3(a,i,p,j) * wm_interm_32_pt3(a,j,i,q)
term(133) = term(133) + wm_interm_143_pt3(a,p,i,j) * wm_interm_32_pt3(a,j,i,q)
term(134) = term(134) + wm_interm_106_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,j,p)
term(135) = term(135) + wm_interm_107_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,j,p)
term(136) = term(136) + wm_interm_146_pt3(a,i,p,j) * wm_interm_36_pt3(a,j,q,i)
term(137) = term(137) + wm_interm_143_pt3(a,i,p,j) * wm_interm_36_pt3(a,j,q,i)
term(138) = term(138) + wm_interm_146_pt3(a,p,i,j) * wm_interm_36_pt3(a,j,q,i)
term(139) = term(139) + wm_interm_143_pt3(a,p,i,j) * wm_interm_36_pt3(a,j,q,i)
term(140) = term(140) + wm_interm_146_pt3(a,i,p,j) * wm_interm_38_pt3(a,j,i,q)
term(141) = term(141) + wm_interm_143_pt3(a,i,p,j) * wm_interm_38_pt3(a,j,i,q)
term(142) = term(142) + wm_interm_146_pt3(a,p,i,j) * wm_interm_38_pt3(a,j,i,q)
term(143) = term(143) + wm_interm_143_pt3(a,p,i,j) * wm_interm_38_pt3(a,j,i,q)
term(144) = term(144) + wm_interm_117_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,j,p)
term(145) = term(145) + wm_interm_117_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,j,p)
term(146) = term(146) + wm_interm_114_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,j,p)
term(147) = term(147) + wm_interm_114_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,j,p)
term(148) = term(148) + wm_interm_146_pt3(a,i,p,j) * wm_interm_40_pt3(a,j,i,q)
term(149) = term(149) + wm_interm_143_pt3(a,i,p,j) * wm_interm_40_pt3(a,j,i,q)
term(150) = term(150) + wm_interm_146_pt3(a,p,i,j) * wm_interm_40_pt3(a,j,i,q)
term(151) = term(151) + wm_interm_146_pt3(a,p,i,j) * wm_interm_36_pt3(a,j,i,q)
term(152) = term(152) + wm_interm_146_pt3(a,i,p,j) * wm_interm_36_pt3(a,j,i,q)
term(153) = term(153) + wm_interm_143_pt3(a,i,p,j) * wm_interm_36_pt3(a,j,i,q)
term(154) = term(154) + wm_interm_143_pt3(a,p,i,j) * wm_interm_40_pt3(a,j,i,q)
term(155) = term(155) + wm_interm_143_pt3(a,p,i,j) * wm_interm_36_pt3(a,j,i,q)
term(156) = term(156) + wm_interm_117_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,p,j)
term(157) = term(157) + wm_interm_117_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,p,j)
term(158) = term(158) + wm_interm_114_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,p,j)
term(159) = term(159) + wm_interm_106_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,p,j)
term(160) = term(160) + wm_interm_107_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,p,j)
term(161) = term(161) + wm_interm_114_pt3(a,i,q,j) * wm_interm_44_pt3(a,i,p,j)
term(162) = term(162) + wm_interm_106_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,p,j)
term(163) = term(163) + wm_interm_107_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,p,j)
term(164) = term(164) + wm_interm_106_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,j,p)
term(165) = term(165) + wm_interm_107_pt3(a,i,j,q) * wm_interm_44_pt3(a,i,j,p)
term(166) = term(166) + wm_interm_158_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,i,j)
term(167) = term(167) + wm_interm_157_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,i,j)
term(168) = term(168) + wm_interm_161_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,i,j)
term(169) = term(169) + wm_interm_162_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,i,j)
term(170) = term(170) + wm_interm_153_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,i,j)
term(171) = term(171) + wm_interm_154_pt3(a,i,j,p) * wm_interm_56_pt3(a,q,i,j)
term(172) = term(172) + wm_interm_166_pt3(a,i,p,j) * wm_interm_58_pt3(a,q,i,j)
term(173) = term(173) + wm_interm_166_pt3(a,p,i,j) * wm_interm_58_pt3(a,q,i,j)
term(174) = term(174) + wm_interm_166_pt3(a,p,i,j) * wm_interm_58_pt3(a,i,q,j)
term(175) = term(175) + wm_interm_166_pt3(a,i,p,j) * wm_interm_58_pt3(a,i,q,j)
term(176) = term(176) + wm_interm_166_pt3(a,i,p,j) * wm_interm_61_pt3(a,i,q,j)
term(177) = term(177) + wm_interm_166_pt3(a,p,i,j) * wm_interm_61_pt3(a,i,q,j)
term(178) = term(178) + wm_interm_166_pt3(a,i,p,j) * wm_interm_62_pt3(a,q,i,j)
term(179) = term(179) + wm_interm_166_pt3(a,p,i,j) * wm_interm_62_pt3(a,q,i,j)
term(180) = term(180) + wm_interm_166_pt3(a,p,i,j) * wm_interm_61_pt3(a,q,i,j)
term(181) = term(181) + wm_interm_166_pt3(a,i,p,j) * wm_interm_61_pt3(a,q,i,j)
term(182) = term(182) + wm_interm_166_pt3(a,p,i,j) * wm_interm_62_pt3(a,i,q,j)
term(183) = term(183) + wm_interm_166_pt3(a,i,p,j) * wm_interm_62_pt3(a,i,q,j)
term(184) = term(184) + wm_interm_166_pt3(a,i,p,j) * wm_interm_63_pt3(a,q,i,j)
term(185) = term(185) + wm_interm_166_pt3(a,p,i,j) * wm_interm_63_pt3(a,q,i,j)
term(186) = term(186) + wm_interm_166_pt3(a,p,i,j) * wm_interm_63_pt3(a,i,q,j)
term(187) = term(187) + wm_interm_166_pt3(a,i,p,j) * wm_interm_63_pt3(a,i,q,j)
term(188) = term(188) + wm_interm_166_pt3(a,p,i,j) * wm_interm_64_pt3(a,q,i,j)
term(189) = term(189) + wm_interm_166_pt3(a,i,p,j) * wm_interm_64_pt3(a,q,i,j)
term(190) = term(190) + wm_interm_166_pt3(a,i,p,j) * wm_interm_64_pt3(a,i,q,j)
term(191) = term(191) + wm_interm_166_pt3(a,p,i,j) * wm_interm_64_pt3(a,i,q,j)
term(192) = term(192) + wm_interm_166_pt3(a,p,i,j) * wm_interm_65_pt3(a,q,i,j)
term(193) = term(193) + wm_interm_166_pt3(a,i,p,j) * wm_interm_65_pt3(a,q,i,j)
term(194) = term(194) + wm_interm_166_pt3(a,i,p,j) * wm_interm_65_pt3(a,i,q,j)
term(195) = term(195) + wm_interm_166_pt3(a,p,i,j) * wm_interm_65_pt3(a,i,q,j)
term(196) = term(196) + wm_interm_161_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,q,i)
term(197) = term(197) + wm_interm_162_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,q,i)
term(198) = term(198) + wm_interm_157_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,q,i)
term(199) = term(199) + wm_interm_158_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,q,i)
term(200) = term(200) + wm_interm_153_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,q,i)
term(201) = term(201) + wm_interm_154_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,q,i)
term(202) = term(202) + wm_interm_161_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,q,i)
term(203) = term(203) + wm_interm_162_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,q,i)
term(204) = term(204) + wm_interm_158_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,q,i)
term(205) = term(205) + wm_interm_157_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,q,i)
term(206) = term(206) + wm_interm_153_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,q,i)
term(207) = term(207) + wm_interm_154_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,q,i)
term(208) = term(208) + wm_interm_161_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,i,q)
term(209) = term(209) + wm_interm_162_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,i,q)
term(210) = term(210) + wm_interm_158_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,i,q)
term(211) = term(211) + wm_interm_157_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,i,q)
term(212) = term(212) + wm_interm_157_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,i,q)
term(213) = term(213) + wm_interm_158_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,i,q)
term(214) = term(214) + wm_interm_161_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,i,q)
term(215) = term(215) + wm_interm_162_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,i,q)
term(216) = term(216) + wm_interm_153_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,i,q)
term(217) = term(217) + wm_interm_154_pt3(a,i,p,j) * wm_interm_56_pt3(a,j,i,q)
term(218) = term(218) + wm_interm_153_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,i,q)
term(219) = term(219) + wm_interm_154_pt3(a,p,i,j) * wm_interm_56_pt3(a,j,i,q)
term(220) = term(220) + wm_interm_166_pt3(a,i,j,p) * wm_interm_61_pt3(a,i,j,q)
term(221) = term(221) + wm_interm_166_pt3(a,i,j,p) * wm_interm_61_pt3(a,j,i,q)
term(222) = term(222) + wm_interm_166_pt3(a,i,j,p) * wm_interm_58_pt3(a,j,i,q)
term(223) = term(223) + wm_interm_166_pt3(a,i,j,p) * wm_interm_58_pt3(a,i,j,q)
term(224) = term(224) + wm_interm_166_pt3(a,i,j,p) * wm_interm_64_pt3(a,i,j,q)
term(225) = term(225) + wm_interm_166_pt3(a,i,j,p) * wm_interm_64_pt3(a,j,i,q)
term(226) = term(226) + wm_interm_166_pt3(a,i,j,p) * wm_interm_62_pt3(a,j,i,q)
term(227) = term(227) + wm_interm_166_pt3(a,i,j,p) * wm_interm_62_pt3(a,i,j,q)
term(228) = term(228) + wm_interm_166_pt3(a,i,j,p) * wm_interm_63_pt3(a,j,i,q)
term(229) = term(229) + wm_interm_166_pt3(a,i,j,p) * wm_interm_63_pt3(a,i,j,q)
term(230) = term(230) + wm_interm_166_pt3(a,i,j,p) * wm_interm_65_pt3(a,i,j,q)
term(231) = term(231) + wm_interm_166_pt3(a,i,j,p) * wm_interm_65_pt3(a,j,i,q)
end do 
end do 
end do 

term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (1.9999999999999998d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-3.9999999999999996d+0) 
term(24) = term(24) * (8.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(29) = term(29) * (-1.9999999999999998d+0) 
term(31) = term(31) * (-1.9999999999999998d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (8.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (-3.9999999999999996d+0) 
term(43) = term(43) * (7.999999999999999d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (8.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (1.9999999999999998d+0) 
term(49) = term(49) * (-3.9999999999999996d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (8.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(55) = term(55) * (-3.9999999999999996d+0) 
term(56) = term(56) * (1.9999999999999998d+0) 
term(57) = term(57) * (-1.9999999999999998d+0) 
term(58) = term(58) * (-3.9999999999999996d+0) 
term(59) = term(59) * (7.999999999999999d+0) 
term(61) = term(61) * (-3.9999999999999996d+0) 
term(62) = term(62) * (1.9999999999999998d+0) 
term(63) = term(63) * (-1.9999999999999998d+0) 
term(64) = term(64) * (-3.9999999999999996d+0) 
term(65) = term(65) * (7.999999999999999d+0) 
term(67) = term(67) * (-2.0d+0) 
term(69) = term(69) * (-1.9999999999999998d+0) 
term(70) = term(70) * (-1.9999999999999998d+0) 
term(71) = term(71) * (3.9999999999999996d+0) 
term(73) = term(73) * (-1.9999999999999998d+0) 
term(75) = term(75) * (-1.9999999999999998d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(79) = term(79) * (-1.9999999999999998d+0) 
term(81) = term(81) * (-1.9999999999999998d+0) 
term(83) = term(83) * (-1.9999999999999998d+0) 
term(85) = term(85) * (-2.0d+0) 
term(86) = term(86) * (-1.9999999999999998d+0) 
term(87) = term(87) * (4.0d+0) 
term(89) = term(89) * (-2.0d+0) 
term(91) = term(91) * (-1.9999999999999998d+0) 
term(92) = term(92) * (-1.9999999999999998d+0) 
term(93) = term(93) * (3.9999999999999996d+0) 
term(94) = term(94) * (-2.0000000000000004d+0) 
term(95) = term(95) * (4.000000000000001d+0) 
term(97) = term(97) * (-2.0000000000000004d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * (8.0d+0) 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * (-2.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (1.9999999999999998d+0) 
term(111) = term(111) * (-3.9999999999999996d+0) 
term(113) = term(113) * (-1.9999999999999998d+0) 
term(115) = term(115) * (-1.9999999999999998d+0) 
term(117) = term(117) * (-1.9999999999999998d+0) 
term(119) = term(119) * (-1.9999999999999998d+0) 
term(120) = term(120) * (-1.9999999999999998d+0) 
term(121) = term(121) * (3.9999999999999996d+0) 
term(123) = term(123) * (-1.9999999999999998d+0) 
term(124) = term(124) * (-1.9999999999999998d+0) 
term(125) = term(125) * (3.9999999999999996d+0) 
term(127) = term(127) * (-2.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(133) = term(133) * (-2.0d+0) 
term(134) = term(134) * (2.0d+0) 
term(135) = term(135) * (-4.0d+0) 
term(136) = term(136) * (-1.9999999999999998d+0) 
term(137) = term(137) * (3.9999999999999996d+0) 
term(138) = term(138) * (3.9999999999999996d+0) 
term(139) = term(139) * (-1.9999999999999998d+0) 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * (-2.0d+0) 
term(144) = term(144) * (-2.0d+0) 
term(147) = term(147) * (-2.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * (-2.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * (8.0d+0) 
term(161) = term(161) * (-2.0d+0) 
term(162) = term(162) * (1.9999999999999998d+0) 
term(163) = term(163) * (-3.9999999999999996d+0) 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * (8.0d+0) 
term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (4.0d+0) 
term(171) = term(171) * (-2.0d+0) 
term(173) = term(173) * (-2.0d+0) 
term(175) = term(175) * (-2.0d+0) 
term(177) = term(177) * (-2.0d+0) 
term(178) = term(178) * (-2.0d+0) 
term(179) = term(179) * (4.0d+0) 
term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * (-2.0d+0) 
term(183) = term(183) * (4.0d+0) 
term(185) = term(185) * (-2.0d+0) 
term(187) = term(187) * (-2.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (4.0d+0) 
term(190) = term(190) * (-2.0d+0) 
term(191) = term(191) * (4.0d+0) 
term(193) = term(193) * (-2.0d+0) 
term(195) = term(195) * (-2.0d+0) 
term(197) = term(197) * (-2.0d+0) 
term(199) = term(199) * (-2.0d+0) 
term(200) = term(200) * (-2.0d+0) 
term(201) = term(201) * (4.0d+0) 
term(202) = term(202) * (-2.0d+0) 
term(203) = term(203) * (4.0d+0) 
term(205) = term(205) * (-2.0d+0) 
term(207) = term(207) * (-2.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(211) = term(211) * (-2.0d+0) 
term(213) = term(213) * (-2.0d+0) 
term(214) = term(214) * (-2.0d+0) 
term(215) = term(215) * (4.0d+0) 
term(217) = term(217) * (-2.0d+0) 
term(218) = term(218) * (-2.0d+0) 
term(219) = term(219) * (4.0d+0) 
term(221) = term(221) * (-2.0d+0) 
term(223) = term(223) * (-2.0d+0) 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (4.0d+0) 
term(226) = term(226) * (-2.0d+0) 
term(227) = term(227) * (4.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(231) = term(231) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(232) = term(232) + wm_interm_10_pt3(a,q) * wm_interm_89_pt3(a,p)
term(233) = term(233) + wm_interm_10_pt3(a,q) * wm_interm_90_pt3(a,p)
term(234) = term(234) + wm_interm_10_pt3(a,q) * wm_interm_91_pt3(a,p)
term(235) = term(235) + wm_interm_10_pt3(a,q) * wm_interm_92_pt3(a,p)
term(236) = term(236) + wm_interm_10_pt3(a,q) * wm_interm_93_pt3(a,p)
term(237) = term(237) + wm_interm_10_pt3(a,q) * wm_interm_94_pt3(a,p)
term(238) = term(238) + wm_interm_89_pt3(a,p) * wm_interm_9_pt3(a,q)
term(239) = term(239) + wm_interm_90_pt3(a,p) * wm_interm_9_pt3(a,q)
term(240) = term(240) + wm_interm_91_pt3(a,p) * wm_interm_9_pt3(a,q)
term(241) = term(241) + wm_interm_92_pt3(a,p) * wm_interm_9_pt3(a,q)
term(242) = term(242) + wm_interm_93_pt3(a,p) * wm_interm_9_pt3(a,q)
term(243) = term(243) + wm_interm_94_pt3(a,p) * wm_interm_9_pt3(a,q)
term(244) = term(244) + wm_interm_133_pt3(a,q) * wm_interm_20_pt3(a,p)
term(245) = term(245) + wm_interm_133_pt3(a,q) * wm_interm_21_pt3(a,p)
term(246) = term(246) + wm_interm_133_pt3(a,q) * wm_interm_22_pt3(a,p)
term(247) = term(247) + wm_interm_133_pt3(a,q) * wm_interm_23_pt3(a,p)
term(248) = term(248) + wm_interm_134_pt3(a,q) * wm_interm_20_pt3(a,p)
term(249) = term(249) + wm_interm_134_pt3(a,q) * wm_interm_21_pt3(a,p)
term(250) = term(250) + wm_interm_134_pt3(a,q) * wm_interm_22_pt3(a,p)
term(251) = term(251) + wm_interm_134_pt3(a,q) * wm_interm_23_pt3(a,p)
term(252) = term(252) + wm_interm_145_pt3(a,p) * wm_interm_34_pt3(a,q)
term(253) = term(253) + wm_interm_144_pt3(a,p) * wm_interm_34_pt3(a,q)
term(254) = term(254) + wm_interm_145_pt3(a,p) * wm_interm_35_pt3(a,q)
term(255) = term(255) + wm_interm_144_pt3(a,p) * wm_interm_35_pt3(a,q)
term(256) = term(256) + wm_interm_145_pt3(a,p) * wm_interm_37_pt3(a,q)
term(257) = term(257) + wm_interm_144_pt3(a,p) * wm_interm_37_pt3(a,q)
term(258) = term(258) + wm_interm_145_pt3(a,p) * wm_interm_39_pt3(a,q)
term(259) = term(259) + wm_interm_144_pt3(a,p) * wm_interm_39_pt3(a,q)
term(260) = term(260) + wm_interm_115_pt3(a,q) * wm_interm_46_pt3(a,p)
term(261) = term(261) + wm_interm_116_pt3(a,q) * wm_interm_46_pt3(a,p)
term(262) = term(262) + wm_interm_115_pt3(a,q) * wm_interm_49_pt3(a,p)
term(263) = term(263) + wm_interm_116_pt3(a,q) * wm_interm_49_pt3(a,p)
term(264) = term(264) + wm_interm_113_pt3(a,q) * wm_interm_46_pt3(a,p)
term(265) = term(265) + wm_interm_108_pt3(a,q) * wm_interm_46_pt3(a,p)
term(266) = term(266) + wm_interm_113_pt3(a,q) * wm_interm_49_pt3(a,p)
term(267) = term(267) + wm_interm_108_pt3(a,q) * wm_interm_49_pt3(a,p)
term(268) = term(268) + wm_interm_151_pt3(a,p) * wm_interm_66_pt3(a,q)
term(269) = term(269) + wm_interm_151_pt3(a,p) * wm_interm_68_pt3(a,q)
term(270) = term(270) + wm_interm_152_pt3(a,p) * wm_interm_66_pt3(a,q)
term(271) = term(271) + wm_interm_152_pt3(a,p) * wm_interm_68_pt3(a,q)
term(272) = term(272) + wm_interm_151_pt3(a,p) * wm_interm_69_pt3(a,q)
term(273) = term(273) + wm_interm_151_pt3(a,p) * wm_interm_70_pt3(a,q)
term(274) = term(274) + wm_interm_152_pt3(a,p) * wm_interm_69_pt3(a,q)
term(275) = term(275) + wm_interm_152_pt3(a,p) * wm_interm_70_pt3(a,q)
term(276) = term(276) + wm_interm_159_pt3(a,p) * wm_interm_51_pt3(a,q)
term(277) = term(277) + wm_interm_160_pt3(a,p) * wm_interm_51_pt3(a,q)
term(278) = term(278) + wm_interm_155_pt3(a,p) * wm_interm_51_pt3(a,q)
term(279) = term(279) + wm_interm_156_pt3(a,p) * wm_interm_51_pt3(a,q)
term(280) = term(280) + wm_interm_159_pt3(a,p) * wm_interm_57_pt3(a,q)
term(281) = term(281) + wm_interm_160_pt3(a,p) * wm_interm_57_pt3(a,q)
term(282) = term(282) + wm_interm_155_pt3(a,p) * wm_interm_57_pt3(a,q)
term(283) = term(283) + wm_interm_156_pt3(a,p) * wm_interm_57_pt3(a,q)
end do 

term(233) = term(233) * (-2.0d+0) 
term(235) = term(235) * (-2.0d+0) 
term(236) = term(236) * (-1.9999999999999998d+0) 
term(237) = term(237) * (3.9999999999999996d+0) 
term(238) = term(238) * (-2.0d+0) 
term(239) = term(239) * (4.0d+0) 
term(240) = term(240) * (-2.0d+0) 
term(241) = term(241) * (4.0d+0) 
term(242) = term(242) * (3.9999999999999996d+0) 
term(243) = term(243) * (-7.999999999999999d+0) 
term(244) = term(244) * (-8.0d+0) 
term(245) = term(245) * (8.0d+0) 
term(246) = term(246) * (4.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (4.0d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (-2.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-1.9999999999999998d+0) 
term(253) = term(253) * (3.9999999999999996d+0) 
term(254) = term(254) * (1.9999999999999996d+0) 
term(255) = term(255) * (-3.999999999999999d+0) 
term(256) = term(256) * (3.9999999999999996d+0) 
term(257) = term(257) * (-7.999999999999999d+0) 
term(258) = term(258) * (-3.9999999999999996d+0) 
term(259) = term(259) * (7.999999999999999d+0) 
term(260) = term(260) * (2.0d+0) 
term(261) = term(261) * (-4.0d+0) 
term(262) = term(262) * (-3.9999999999999996d+0) 
term(263) = term(263) * (7.999999999999999d+0) 
term(264) = term(264) * (-1.9999999999999998d+0) 
term(265) = term(265) * (3.9999999999999996d+0) 
term(266) = term(266) * (3.9999999999999996d+0) 
term(267) = term(267) * (-7.999999999999999d+0) 
term(268) = term(268) * (-7.999999999999999d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (3.9999999999999996d+0) 
term(271) = term(271) * (-4.0d+0) 
term(272) = term(272) * (3.9999999999999996d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (-1.9999999999999998d+0) 
term(275) = term(275) * (2.0d+0) 
term(276) = term(276) * (-3.9999999999999996d+0) 
term(277) = term(277) * (7.999999999999999d+0) 
term(278) = term(278) * (4.0d+0) 
term(279) = term(279) * (-8.0d+0) 
term(280) = term(280) * (1.9999999999999998d+0) 
term(281) = term(281) * (-3.9999999999999996d+0) 
term(282) = term(282) * (-2.0d+0) 
term(283) = term(283) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(284) = term(284) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,q,k,p,i,j)
term(285) = term(285) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,k,q,p,i,j)
end do 
end do 
end do 
end do 

term(284) = term(284) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(286) = term(286) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,q,k,p,j,i)
term(287) = term(287) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,k,q,p,j,i)
end do 
end do 
end do 
end do 

term(286) = term(286) * (3.9999999999999996d+0) 
term(287) = term(287) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(288) = term(288) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,q,k,j,i,p)
term(289) = term(289) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,q,k,j,p,i)
term(290) = term(290) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,k,q,j,i,p)
term(291) = term(291) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,k,q,j,p,i)
end do 
end do 
end do 
end do 

term(289) = term(289) * (-1.9999999999999998d+0) 
term(290) = term(290) * (-1.9999999999999998d+0) 
term(291) = term(291) * (3.9999999999999996d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(292) = term(292) + wm_interm_128_pt3(a,i,j,k) * wm_interm_1_pt3(a,j,k,i)
term(293) = term(293) + wm_interm_128_pt3(a,i,j,k) * wm_interm_1_pt3(a,k,j,i)
term(294) = term(294) + wm_interm_128_pt3(a,i,j,k) * wm_interm_2_pt3(a,k,j,i)
term(295) = term(295) + wm_interm_128_pt3(a,i,j,k) * wm_interm_2_pt3(a,j,k,i)
term(296) = term(296) + wm_interm_128_pt3(a,i,j,k) * wm_interm_3_pt3(a,j,k,i)
term(297) = term(297) + wm_interm_128_pt3(a,i,j,k) * wm_interm_3_pt3(a,k,j,i)
term(298) = term(298) + wm_interm_101_pt3(a,i,j,p,k,q) * wm_interm_128_pt3(a,k,j,i)
term(299) = term(299) + wm_interm_11_pt3(a,i,j,k) * wm_interm_136_pt3(a,k,j,i)
term(300) = term(300) + wm_interm_11_pt3(a,i,j,k) * wm_interm_137_pt3(a,k,j,i)
term(301) = term(301) + wm_interm_11_pt3(a,i,j,k) * wm_interm_138_pt3(a,k,j,i)
term(302) = term(302) + wm_interm_11_pt3(a,i,j,k) * wm_interm_141_pt3(a,k,j,i)
term(303) = term(303) + wm_interm_143_pt3(a,i,j,k) * wm_interm_32_pt3(a,k,j,i)
term(304) = term(304) + wm_interm_143_pt3(a,i,j,k) * wm_interm_36_pt3(a,k,j,i)
term(305) = term(305) + wm_interm_146_pt3(a,i,j,k) * wm_interm_32_pt3(a,k,j,i)
term(306) = term(306) + wm_interm_146_pt3(a,i,j,k) * wm_interm_36_pt3(a,k,j,i)
term(307) = term(307) + wm_interm_153_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,j,i)
term(308) = term(308) + wm_interm_154_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,j,i)
term(309) = term(309) + wm_interm_157_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,j,i)
term(310) = term(310) + wm_interm_158_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,j,i)
term(311) = term(311) + wm_interm_161_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,j,i)
term(312) = term(312) + wm_interm_162_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,j,i)
term(313) = term(313) + wm_interm_165_pt3(a,i,j,q,p,k) * wm_interm_56_pt3(a,k,j,i)
term(314) = term(314) + wm_interm_165_pt3(a,i,j,q,k,p) * wm_interm_56_pt3(a,k,j,i)
term(315) = term(315) + wm_interm_164_pt3(a,i,j,q,p,k) * wm_interm_56_pt3(a,k,j,i)
term(316) = term(316) + wm_interm_164_pt3(a,i,j,q,k,p) * wm_interm_56_pt3(a,k,j,i)
term(317) = term(317) + wm_interm_163_pt3(a,i,j,q,p,k) * wm_interm_56_pt3(a,k,j,i)
term(318) = term(318) + wm_interm_163_pt3(a,i,j,q,k,p) * wm_interm_56_pt3(a,k,j,i)
term(319) = term(319) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,j,p,i,q,k)
term(320) = term(320) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,j,p,i,q,k)
term(321) = term(321) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,p,j,i,q,k)
term(322) = term(322) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,p,j,i,q,k)
term(323) = term(323) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,p,j,i,k,q)
term(324) = term(324) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,p,j,i,k,q)
term(325) = term(325) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,p,j,i,k,q)
term(326) = term(326) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,p,j,i,q,k)
term(327) = term(327) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,j,p,i,k,q)
term(328) = term(328) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,j,p,i,k,q)
term(329) = term(329) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,j,p,i,q,k)
term(330) = term(330) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,j,p,i,k,q)
end do 
end do 
end do 
end do 

term(292) = term(292) * (-4.0d+0) 
term(293) = term(293) * (8.0d+0) 
term(294) = term(294) * (-3.9999999999999996d+0) 
term(295) = term(295) * (7.999999999999999d+0) 
term(296) = term(296) * (8.0d+0) 
term(297) = term(297) * (-16.0d+0) 
term(298) = term(298) * (8.0d+0) 
term(299) = term(299) * (-1.9999999999999998d+0) 
term(300) = term(300) * (7.999999999999999d+0) 
term(301) = term(301) * (-15.999999999999998d+0) 
term(302) = term(302) * (3.9999999999999996d+0) 
term(303) = term(303) * (4.0d+0) 
term(304) = term(304) * (-8.0d+0) 
term(305) = term(305) * (-2.0d+0) 
term(306) = term(306) * (4.0d+0) 
term(307) = term(307) * (4.0d+0) 
term(308) = term(308) * (-8.0d+0) 
term(309) = term(309) * (-2.0d+0) 
term(310) = term(310) * (4.0d+0) 
term(311) = term(311) * (-2.0d+0) 
term(312) = term(312) * (4.0d+0) 
term(313) = term(313) * (0.3333333333333333d+0) 
term(314) = term(314) * (-0.6666666666666666d+0) 
term(315) = term(315) * (-0.6666666666666666d+0) 
term(316) = term(316) * (1.3333333333333333d+0) 
term(317) = term(317) * (0.3333333333333333d+0) 
term(318) = term(318) * (-0.6666666666666666d+0) 
term(319) = term(319) * (-0.6666666666666666d+0) 
term(320) = term(320) * (0.3333333333333333d+0) 
term(321) = term(321) * (1.3333333333333333d+0) 
term(322) = term(322) * (-0.6666666666666666d+0) 
term(323) = term(323) * (-0.6666666666666666d+0) 
term(324) = term(324) * (0.3333333333333333d+0) 
term(325) = term(325) * (-0.6666666666666666d+0) 
term(326) = term(326) * (0.3333333333333333d+0) 
term(327) = term(327) * (1.3333333333333333d+0) 
term(328) = term(328) * (-0.6666666666666666d+0) 
term(329) = term(329) * (-0.6666666666666666d+0) 
term(330) = term(330) * (0.3333333333333333d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(331) = term(331) + wm_interm_84_pt3(a,p,i,q) * wm_interm_9_pt3(a,i)
term(332) = term(332) + wm_interm_85_pt3(a,p,i,q) * wm_interm_9_pt3(a,i)
term(333) = term(333) + wm_interm_88_pt3(a,p,i,q) * wm_interm_9_pt3(a,i)
term(334) = term(334) + wm_interm_129_pt3(a,p,i,q) * wm_interm_9_pt3(a,i)
term(335) = term(335) + wm_interm_89_pt3(a,i) * wm_interm_9_pt3(a,i)
term(336) = term(336) + wm_interm_90_pt3(a,i) * wm_interm_9_pt3(a,i)
term(337) = term(337) + wm_interm_129_pt3(a,p,q,i) * wm_interm_9_pt3(a,i)
term(338) = term(338) + wm_interm_91_pt3(a,i) * wm_interm_9_pt3(a,i)
term(339) = term(339) + wm_interm_88_pt3(a,p,q,i) * wm_interm_9_pt3(a,i)
term(340) = term(340) + wm_interm_92_pt3(a,i) * wm_interm_9_pt3(a,i)
term(341) = term(341) + wm_interm_84_pt3(a,p,q,i) * wm_interm_9_pt3(a,i)
term(342) = term(342) + wm_interm_85_pt3(a,p,q,i) * wm_interm_9_pt3(a,i)
term(343) = term(343) + wm_interm_93_pt3(a,i) * wm_interm_9_pt3(a,i)
term(344) = term(344) + wm_interm_94_pt3(a,i) * wm_interm_9_pt3(a,i)
term(345) = term(345) + wm_interm_10_pt3(a,i) * wm_interm_84_pt3(a,p,i,q)
term(346) = term(346) + wm_interm_10_pt3(a,i) * wm_interm_85_pt3(a,p,i,q)
term(347) = term(347) + wm_interm_10_pt3(a,i) * wm_interm_88_pt3(a,p,i,q)
term(348) = term(348) + wm_interm_10_pt3(a,i) * wm_interm_129_pt3(a,p,i,q)
term(349) = term(349) + wm_interm_10_pt3(a,i) * wm_interm_89_pt3(a,i)
term(350) = term(350) + wm_interm_10_pt3(a,i) * wm_interm_90_pt3(a,i)
term(351) = term(351) + wm_interm_10_pt3(a,i) * wm_interm_129_pt3(a,p,q,i)
term(352) = term(352) + wm_interm_10_pt3(a,i) * wm_interm_91_pt3(a,i)
term(353) = term(353) + wm_interm_10_pt3(a,i) * wm_interm_88_pt3(a,p,q,i)
term(354) = term(354) + wm_interm_10_pt3(a,i) * wm_interm_92_pt3(a,i)
term(355) = term(355) + wm_interm_10_pt3(a,i) * wm_interm_84_pt3(a,p,q,i)
term(356) = term(356) + wm_interm_10_pt3(a,i) * wm_interm_85_pt3(a,p,q,i)
term(357) = term(357) + wm_interm_10_pt3(a,i) * wm_interm_93_pt3(a,i)
term(358) = term(358) + wm_interm_10_pt3(a,i) * wm_interm_94_pt3(a,i)
term(359) = term(359) + wm_interm_130_pt3(a,i) * wm_interm_1_pt3(a,p,i,q)
term(360) = term(360) + wm_interm_131_pt3(a,i) * wm_interm_1_pt3(a,p,i,q)
term(361) = term(361) + wm_interm_130_pt3(a,i) * wm_interm_1_pt3(a,i,p,q)
term(362) = term(362) + wm_interm_131_pt3(a,i) * wm_interm_1_pt3(a,i,p,q)
term(363) = term(363) + wm_interm_130_pt3(a,i) * wm_interm_2_pt3(a,i,p,q)
term(364) = term(364) + wm_interm_131_pt3(a,i) * wm_interm_2_pt3(a,i,p,q)
term(365) = term(365) + wm_interm_130_pt3(a,i) * wm_interm_2_pt3(a,p,i,q)
term(366) = term(366) + wm_interm_131_pt3(a,i) * wm_interm_2_pt3(a,p,i,q)
term(367) = term(367) + wm_interm_5_pt3(a,p,q,i) * wm_interm_89_pt3(a,i)
term(368) = term(368) + wm_interm_5_pt3(a,p,q,i) * wm_interm_90_pt3(a,i)
term(369) = term(369) + wm_interm_5_pt3(a,p,q,i) * wm_interm_91_pt3(a,i)
term(370) = term(370) + wm_interm_5_pt3(a,p,q,i) * wm_interm_92_pt3(a,i)
term(371) = term(371) + wm_interm_130_pt3(a,i) * wm_interm_3_pt3(a,p,i,q)
term(372) = term(372) + wm_interm_131_pt3(a,i) * wm_interm_3_pt3(a,p,i,q)
term(373) = term(373) + wm_interm_130_pt3(a,i) * wm_interm_3_pt3(a,i,p,q)
term(374) = term(374) + wm_interm_131_pt3(a,i) * wm_interm_3_pt3(a,i,p,q)
term(375) = term(375) + wm_interm_5_pt3(a,p,q,i) * wm_interm_93_pt3(a,i)
term(376) = term(376) + wm_interm_5_pt3(a,p,q,i) * wm_interm_94_pt3(a,i)
term(377) = term(377) + wm_interm_5_pt3(a,p,i,q) * wm_interm_93_pt3(a,i)
term(378) = term(378) + wm_interm_5_pt3(a,p,i,q) * wm_interm_94_pt3(a,i)
term(379) = term(379) + wm_interm_5_pt3(a,p,i,q) * wm_interm_89_pt3(a,i)
term(380) = term(380) + wm_interm_5_pt3(a,p,i,q) * wm_interm_90_pt3(a,i)
term(381) = term(381) + wm_interm_5_pt3(a,p,i,q) * wm_interm_91_pt3(a,i)
term(382) = term(382) + wm_interm_5_pt3(a,p,i,q) * wm_interm_92_pt3(a,i)
term(383) = term(383) + wm_interm_130_pt3(a,i) * wm_interm_132_pt3(a,p,i,q)
term(384) = term(384) + wm_interm_131_pt3(a,i) * wm_interm_132_pt3(a,p,i,q)
term(385) = term(385) + wm_interm_130_pt3(a,i) * wm_interm_132_pt3(a,i,p,q)
term(386) = term(386) + wm_interm_131_pt3(a,i) * wm_interm_132_pt3(a,i,p,q)
term(387) = term(387) + wm_interm_133_pt3(a,i) * wm_interm_20_pt3(a,i)
term(388) = term(388) + wm_interm_133_pt3(a,i) * wm_interm_21_pt3(a,i)
term(389) = term(389) + wm_interm_133_pt3(a,i) * wm_interm_22_pt3(a,i)
term(390) = term(390) + wm_interm_133_pt3(a,i) * wm_interm_23_pt3(a,i)
term(391) = term(391) + wm_interm_134_pt3(a,i) * wm_interm_20_pt3(a,i)
term(392) = term(392) + wm_interm_134_pt3(a,i) * wm_interm_21_pt3(a,i)
term(393) = term(393) + wm_interm_134_pt3(a,i) * wm_interm_22_pt3(a,i)
term(394) = term(394) + wm_interm_134_pt3(a,i) * wm_interm_23_pt3(a,i)
term(395) = term(395) + wm_interm_143_pt3(a,q,i,p) * wm_interm_34_pt3(a,i)
term(396) = term(396) + wm_interm_143_pt3(a,q,i,p) * wm_interm_37_pt3(a,i)
term(397) = term(397) + wm_interm_144_pt3(a,i) * wm_interm_34_pt3(a,i)
term(398) = term(398) + wm_interm_144_pt3(a,i) * wm_interm_37_pt3(a,i)
term(399) = term(399) + wm_interm_143_pt3(a,i,q,p) * wm_interm_34_pt3(a,i)
term(400) = term(400) + wm_interm_143_pt3(a,i,q,p) * wm_interm_37_pt3(a,i)
term(401) = term(401) + wm_interm_145_pt3(a,i) * wm_interm_34_pt3(a,i)
term(402) = term(402) + wm_interm_145_pt3(a,i) * wm_interm_37_pt3(a,i)
term(403) = term(403) + wm_interm_146_pt3(a,q,i,p) * wm_interm_34_pt3(a,i)
term(404) = term(404) + wm_interm_146_pt3(a,q,i,p) * wm_interm_37_pt3(a,i)
term(405) = term(405) + wm_interm_146_pt3(a,i,q,p) * wm_interm_34_pt3(a,i)
term(406) = term(406) + wm_interm_146_pt3(a,i,q,p) * wm_interm_37_pt3(a,i)
term(407) = term(407) + wm_interm_146_pt3(a,q,i,p) * wm_interm_35_pt3(a,i)
term(408) = term(408) + wm_interm_145_pt3(a,i) * wm_interm_35_pt3(a,i)
term(409) = term(409) + wm_interm_146_pt3(a,q,i,p) * wm_interm_39_pt3(a,i)
term(410) = term(410) + wm_interm_145_pt3(a,i) * wm_interm_39_pt3(a,i)
term(411) = term(411) + wm_interm_143_pt3(a,q,i,p) * wm_interm_35_pt3(a,i)
term(412) = term(412) + wm_interm_143_pt3(a,q,i,p) * wm_interm_39_pt3(a,i)
term(413) = term(413) + wm_interm_144_pt3(a,i) * wm_interm_35_pt3(a,i)
term(414) = term(414) + wm_interm_144_pt3(a,i) * wm_interm_39_pt3(a,i)
term(415) = term(415) + wm_interm_108_pt3(a,i) * wm_interm_44_pt3(a,p,i,q)
term(416) = term(416) + wm_interm_108_pt3(a,i) * wm_interm_46_pt3(a,i)
term(417) = term(417) + wm_interm_108_pt3(a,i) * wm_interm_44_pt3(a,p,q,i)
term(418) = term(418) + wm_interm_108_pt3(a,i) * wm_interm_49_pt3(a,i)
term(419) = term(419) + wm_interm_143_pt3(a,i,q,p) * wm_interm_35_pt3(a,i)
term(420) = term(420) + wm_interm_143_pt3(a,i,q,p) * wm_interm_39_pt3(a,i)
term(421) = term(421) + wm_interm_113_pt3(a,i) * wm_interm_44_pt3(a,p,i,q)
term(422) = term(422) + wm_interm_113_pt3(a,i) * wm_interm_46_pt3(a,i)
term(423) = term(423) + wm_interm_113_pt3(a,i) * wm_interm_44_pt3(a,p,q,i)
term(424) = term(424) + wm_interm_113_pt3(a,i) * wm_interm_49_pt3(a,i)
term(425) = term(425) + wm_interm_146_pt3(a,i,q,p) * wm_interm_35_pt3(a,i)
term(426) = term(426) + wm_interm_146_pt3(a,i,q,p) * wm_interm_39_pt3(a,i)
term(427) = term(427) + wm_interm_115_pt3(a,i) * wm_interm_44_pt3(a,p,q,i)
term(428) = term(428) + wm_interm_115_pt3(a,i) * wm_interm_49_pt3(a,i)
term(429) = term(429) + wm_interm_115_pt3(a,i) * wm_interm_44_pt3(a,p,i,q)
term(430) = term(430) + wm_interm_115_pt3(a,i) * wm_interm_46_pt3(a,i)
term(431) = term(431) + wm_interm_116_pt3(a,i) * wm_interm_44_pt3(a,p,i,q)
term(432) = term(432) + wm_interm_116_pt3(a,i) * wm_interm_46_pt3(a,i)
term(433) = term(433) + wm_interm_116_pt3(a,i) * wm_interm_44_pt3(a,p,q,i)
term(434) = term(434) + wm_interm_116_pt3(a,i) * wm_interm_49_pt3(a,i)
term(435) = term(435) + wm_interm_133_pt3(a,i) * wm_interm_13_pt3(a,p,i,q)
term(436) = term(436) + wm_interm_133_pt3(a,i) * wm_interm_13_pt3(a,i,p,q)
term(437) = term(437) + wm_interm_133_pt3(a,i) * wm_interm_16_pt3(a,i,p,q)
term(438) = term(438) + wm_interm_133_pt3(a,i) * wm_interm_14_pt3(a,p,i,q)
term(439) = term(439) + wm_interm_133_pt3(a,i) * wm_interm_15_pt3(a,i,p,q)
term(440) = term(440) + wm_interm_133_pt3(a,i) * wm_interm_14_pt3(a,i,p,q)
term(441) = term(441) + wm_interm_134_pt3(a,i) * wm_interm_13_pt3(a,p,i,q)
term(442) = term(442) + wm_interm_134_pt3(a,i) * wm_interm_13_pt3(a,i,p,q)
term(443) = term(443) + wm_interm_134_pt3(a,i) * wm_interm_16_pt3(a,i,p,q)
term(444) = term(444) + wm_interm_134_pt3(a,i) * wm_interm_14_pt3(a,p,i,q)
term(445) = term(445) + wm_interm_134_pt3(a,i) * wm_interm_15_pt3(a,i,p,q)
term(446) = term(446) + wm_interm_134_pt3(a,i) * wm_interm_14_pt3(a,i,p,q)
term(447) = term(447) + wm_interm_142_pt3(a,i,p,q) * wm_interm_20_pt3(a,i)
term(448) = term(448) + wm_interm_142_pt3(a,i,p,q) * wm_interm_21_pt3(a,i)
term(449) = term(449) + wm_interm_142_pt3(a,p,i,q) * wm_interm_20_pt3(a,i)
term(450) = term(450) + wm_interm_142_pt3(a,p,i,q) * wm_interm_21_pt3(a,i)
term(451) = term(451) + wm_interm_139_pt3(a,p,i,q) * wm_interm_20_pt3(a,i)
term(452) = term(452) + wm_interm_139_pt3(a,p,i,q) * wm_interm_21_pt3(a,i)
term(453) = term(453) + wm_interm_139_pt3(a,i,p,q) * wm_interm_20_pt3(a,i)
term(454) = term(454) + wm_interm_139_pt3(a,i,p,q) * wm_interm_21_pt3(a,i)
term(455) = term(455) + wm_interm_142_pt3(a,i,p,q) * wm_interm_22_pt3(a,i)
term(456) = term(456) + wm_interm_142_pt3(a,p,i,q) * wm_interm_22_pt3(a,i)
term(457) = term(457) + wm_interm_139_pt3(a,p,i,q) * wm_interm_22_pt3(a,i)
term(458) = term(458) + wm_interm_139_pt3(a,i,p,q) * wm_interm_22_pt3(a,i)
term(459) = term(459) + wm_interm_142_pt3(a,i,p,q) * wm_interm_23_pt3(a,i)
term(460) = term(460) + wm_interm_142_pt3(a,p,i,q) * wm_interm_23_pt3(a,i)
term(461) = term(461) + wm_interm_139_pt3(a,p,i,q) * wm_interm_23_pt3(a,i)
term(462) = term(462) + wm_interm_139_pt3(a,i,p,q) * wm_interm_23_pt3(a,i)
term(463) = term(463) + wm_interm_11_pt3(a,p,i,q) * wm_interm_147_pt3(a,i)
term(464) = term(464) + wm_interm_11_pt3(a,p,i,q) * wm_interm_148_pt3(a,i)
term(465) = term(465) + wm_interm_11_pt3(a,p,i,q) * wm_interm_149_pt3(a,i)
term(466) = term(466) + wm_interm_11_pt3(a,p,i,q) * wm_interm_150_pt3(a,i)
term(467) = term(467) + wm_interm_11_pt3(a,i,p,q) * wm_interm_147_pt3(a,i)
term(468) = term(468) + wm_interm_11_pt3(a,i,p,q) * wm_interm_148_pt3(a,i)
term(469) = term(469) + wm_interm_11_pt3(a,i,p,q) * wm_interm_149_pt3(a,i)
term(470) = term(470) + wm_interm_11_pt3(a,i,p,q) * wm_interm_150_pt3(a,i)
term(471) = term(471) + wm_interm_144_pt3(a,i) * wm_interm_32_pt3(a,p,q,i)
term(472) = term(472) + wm_interm_145_pt3(a,i) * wm_interm_32_pt3(a,p,q,i)
term(473) = term(473) + wm_interm_145_pt3(a,i) * wm_interm_32_pt3(a,p,i,q)
term(474) = term(474) + wm_interm_144_pt3(a,i) * wm_interm_32_pt3(a,p,i,q)
term(475) = term(475) + wm_interm_106_pt3(a,p,q,i) * wm_interm_46_pt3(a,i)
term(476) = term(476) + wm_interm_107_pt3(a,p,q,i) * wm_interm_46_pt3(a,i)
term(477) = term(477) + wm_interm_144_pt3(a,i) * wm_interm_36_pt3(a,p,q,i)
term(478) = term(478) + wm_interm_145_pt3(a,i) * wm_interm_36_pt3(a,p,q,i)
term(479) = term(479) + wm_interm_144_pt3(a,i) * wm_interm_38_pt3(a,p,i,q)
term(480) = term(480) + wm_interm_145_pt3(a,i) * wm_interm_38_pt3(a,p,i,q)
term(481) = term(481) + wm_interm_117_pt3(a,p,q,i) * wm_interm_46_pt3(a,i)
term(482) = term(482) + wm_interm_117_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(483) = term(483) + wm_interm_114_pt3(a,p,q,i) * wm_interm_46_pt3(a,i)
term(484) = term(484) + wm_interm_114_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(485) = term(485) + wm_interm_144_pt3(a,i) * wm_interm_40_pt3(a,p,i,q)
term(486) = term(486) + wm_interm_145_pt3(a,i) * wm_interm_40_pt3(a,p,i,q)
term(487) = term(487) + wm_interm_145_pt3(a,i) * wm_interm_36_pt3(a,p,i,q)
term(488) = term(488) + wm_interm_144_pt3(a,i) * wm_interm_36_pt3(a,p,i,q)
term(489) = term(489) + wm_interm_117_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(490) = term(490) + wm_interm_117_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(491) = term(491) + wm_interm_114_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(492) = term(492) + wm_interm_106_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(493) = term(493) + wm_interm_107_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(494) = term(494) + wm_interm_114_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(495) = term(495) + wm_interm_106_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(496) = term(496) + wm_interm_107_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(497) = term(497) + wm_interm_106_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(498) = term(498) + wm_interm_107_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(499) = term(499) + wm_interm_151_pt3(a,i) * wm_interm_66_pt3(a,i)
term(500) = term(500) + wm_interm_151_pt3(a,i) * wm_interm_68_pt3(a,i)
term(501) = term(501) + wm_interm_152_pt3(a,i) * wm_interm_66_pt3(a,i)
term(502) = term(502) + wm_interm_152_pt3(a,i) * wm_interm_68_pt3(a,i)
term(503) = term(503) + wm_interm_151_pt3(a,i) * wm_interm_69_pt3(a,i)
term(504) = term(504) + wm_interm_151_pt3(a,i) * wm_interm_70_pt3(a,i)
term(505) = term(505) + wm_interm_152_pt3(a,i) * wm_interm_69_pt3(a,i)
term(506) = term(506) + wm_interm_152_pt3(a,i) * wm_interm_70_pt3(a,i)
term(507) = term(507) + wm_interm_153_pt3(a,q,i,p) * wm_interm_51_pt3(a,i)
term(508) = term(508) + wm_interm_154_pt3(a,q,i,p) * wm_interm_51_pt3(a,i)
term(509) = term(509) + wm_interm_155_pt3(a,i) * wm_interm_51_pt3(a,i)
term(510) = term(510) + wm_interm_156_pt3(a,i) * wm_interm_51_pt3(a,i)
term(511) = term(511) + wm_interm_157_pt3(a,q,i,p) * wm_interm_51_pt3(a,i)
term(512) = term(512) + wm_interm_158_pt3(a,q,i,p) * wm_interm_51_pt3(a,i)
term(513) = term(513) + wm_interm_153_pt3(a,i,q,p) * wm_interm_51_pt3(a,i)
term(514) = term(514) + wm_interm_154_pt3(a,i,q,p) * wm_interm_51_pt3(a,i)
term(515) = term(515) + wm_interm_159_pt3(a,i) * wm_interm_51_pt3(a,i)
term(516) = term(516) + wm_interm_160_pt3(a,i) * wm_interm_51_pt3(a,i)
term(517) = term(517) + wm_interm_161_pt3(a,q,i,p) * wm_interm_51_pt3(a,i)
term(518) = term(518) + wm_interm_162_pt3(a,q,i,p) * wm_interm_51_pt3(a,i)
term(519) = term(519) + wm_interm_161_pt3(a,i,q,p) * wm_interm_51_pt3(a,i)
term(520) = term(520) + wm_interm_162_pt3(a,i,q,p) * wm_interm_51_pt3(a,i)
term(521) = term(521) + wm_interm_158_pt3(a,i,q,p) * wm_interm_51_pt3(a,i)
term(522) = term(522) + wm_interm_157_pt3(a,i,q,p) * wm_interm_51_pt3(a,i)
term(523) = term(523) + wm_interm_157_pt3(a,q,i,p) * wm_interm_57_pt3(a,i)
term(524) = term(524) + wm_interm_158_pt3(a,q,i,p) * wm_interm_57_pt3(a,i)
term(525) = term(525) + wm_interm_161_pt3(a,q,i,p) * wm_interm_57_pt3(a,i)
term(526) = term(526) + wm_interm_162_pt3(a,q,i,p) * wm_interm_57_pt3(a,i)
term(527) = term(527) + wm_interm_159_pt3(a,i) * wm_interm_57_pt3(a,i)
term(528) = term(528) + wm_interm_160_pt3(a,i) * wm_interm_57_pt3(a,i)
term(529) = term(529) + wm_interm_153_pt3(a,q,i,p) * wm_interm_57_pt3(a,i)
term(530) = term(530) + wm_interm_154_pt3(a,q,i,p) * wm_interm_57_pt3(a,i)
term(531) = term(531) + wm_interm_155_pt3(a,i) * wm_interm_57_pt3(a,i)
term(532) = term(532) + wm_interm_156_pt3(a,i) * wm_interm_57_pt3(a,i)
term(533) = term(533) + wm_interm_158_pt3(a,i,q,p) * wm_interm_57_pt3(a,i)
term(534) = term(534) + wm_interm_157_pt3(a,i,q,p) * wm_interm_57_pt3(a,i)
term(535) = term(535) + wm_interm_153_pt3(a,i,q,p) * wm_interm_57_pt3(a,i)
term(536) = term(536) + wm_interm_154_pt3(a,i,q,p) * wm_interm_57_pt3(a,i)
term(537) = term(537) + wm_interm_161_pt3(a,i,q,p) * wm_interm_57_pt3(a,i)
term(538) = term(538) + wm_interm_162_pt3(a,i,q,p) * wm_interm_57_pt3(a,i)
term(539) = term(539) + wm_interm_166_pt3(a,p,i,q) * wm_interm_66_pt3(a,i)
term(540) = term(540) + wm_interm_166_pt3(a,i,p,q) * wm_interm_66_pt3(a,i)
term(541) = term(541) + wm_interm_166_pt3(a,p,i,q) * wm_interm_68_pt3(a,i)
term(542) = term(542) + wm_interm_166_pt3(a,i,p,q) * wm_interm_68_pt3(a,i)
term(543) = term(543) + wm_interm_166_pt3(a,p,i,q) * wm_interm_69_pt3(a,i)
term(544) = term(544) + wm_interm_166_pt3(a,i,p,q) * wm_interm_69_pt3(a,i)
term(545) = term(545) + wm_interm_166_pt3(a,p,i,q) * wm_interm_70_pt3(a,i)
term(546) = term(546) + wm_interm_166_pt3(a,i,p,q) * wm_interm_70_pt3(a,i)
term(547) = term(547) + wm_interm_151_pt3(a,i) * wm_interm_61_pt3(a,i,p,q)
term(548) = term(548) + wm_interm_151_pt3(a,i) * wm_interm_61_pt3(a,p,i,q)
term(549) = term(549) + wm_interm_151_pt3(a,i) * wm_interm_58_pt3(a,p,i,q)
term(550) = term(550) + wm_interm_151_pt3(a,i) * wm_interm_58_pt3(a,i,p,q)
term(551) = term(551) + wm_interm_151_pt3(a,i) * wm_interm_64_pt3(a,i,p,q)
term(552) = term(552) + wm_interm_151_pt3(a,i) * wm_interm_64_pt3(a,p,i,q)
term(553) = term(553) + wm_interm_151_pt3(a,i) * wm_interm_62_pt3(a,p,i,q)
term(554) = term(554) + wm_interm_151_pt3(a,i) * wm_interm_62_pt3(a,i,p,q)
term(555) = term(555) + wm_interm_151_pt3(a,i) * wm_interm_63_pt3(a,p,i,q)
term(556) = term(556) + wm_interm_151_pt3(a,i) * wm_interm_63_pt3(a,i,p,q)
term(557) = term(557) + wm_interm_151_pt3(a,i) * wm_interm_65_pt3(a,i,p,q)
term(558) = term(558) + wm_interm_151_pt3(a,i) * wm_interm_65_pt3(a,p,i,q)
term(559) = term(559) + wm_interm_152_pt3(a,i) * wm_interm_61_pt3(a,i,p,q)
term(560) = term(560) + wm_interm_152_pt3(a,i) * wm_interm_61_pt3(a,p,i,q)
term(561) = term(561) + wm_interm_152_pt3(a,i) * wm_interm_58_pt3(a,p,i,q)
term(562) = term(562) + wm_interm_152_pt3(a,i) * wm_interm_58_pt3(a,i,p,q)
term(563) = term(563) + wm_interm_152_pt3(a,i) * wm_interm_64_pt3(a,i,p,q)
term(564) = term(564) + wm_interm_152_pt3(a,i) * wm_interm_64_pt3(a,p,i,q)
term(565) = term(565) + wm_interm_152_pt3(a,i) * wm_interm_62_pt3(a,p,i,q)
term(566) = term(566) + wm_interm_152_pt3(a,i) * wm_interm_62_pt3(a,i,p,q)
term(567) = term(567) + wm_interm_152_pt3(a,i) * wm_interm_63_pt3(a,p,i,q)
term(568) = term(568) + wm_interm_152_pt3(a,i) * wm_interm_63_pt3(a,i,p,q)
term(569) = term(569) + wm_interm_152_pt3(a,i) * wm_interm_65_pt3(a,i,p,q)
term(570) = term(570) + wm_interm_152_pt3(a,i) * wm_interm_65_pt3(a,p,i,q)
term(571) = term(571) + wm_interm_155_pt3(a,i) * wm_interm_56_pt3(a,p,q,i)
term(572) = term(572) + wm_interm_156_pt3(a,i) * wm_interm_56_pt3(a,p,q,i)
term(573) = term(573) + wm_interm_159_pt3(a,i) * wm_interm_56_pt3(a,p,q,i)
term(574) = term(574) + wm_interm_160_pt3(a,i) * wm_interm_56_pt3(a,p,q,i)
term(575) = term(575) + wm_interm_159_pt3(a,i) * wm_interm_56_pt3(a,p,i,q)
term(576) = term(576) + wm_interm_160_pt3(a,i) * wm_interm_56_pt3(a,p,i,q)
term(577) = term(577) + wm_interm_155_pt3(a,i) * wm_interm_56_pt3(a,p,i,q)
term(578) = term(578) + wm_interm_156_pt3(a,i) * wm_interm_56_pt3(a,p,i,q)
end do 
end do 

term(331) = term(331) * (-4.0d+0) 
term(332) = term(332) * (8.0d+0) 
term(333) = term(333) * (-2.0d+0) 
term(334) = term(334) * (4.0d+0) 
term(335) = term(335) * (4.0d+0) 
term(336) = term(336) * (-8.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (-8.0d+0) 
term(341) = term(341) * (7.999999999999999d+0) 
term(342) = term(342) * (-15.999999999999998d+0) 
term(343) = term(343) * (-7.999999999999999d+0) 
term(344) = term(344) * (15.999999999999998d+0) 
term(345) = term(345) * (2.0d+0) 
term(346) = term(346) * (-4.0d+0) 
term(348) = term(348) * (-2.0d+0) 
term(349) = term(349) * (-2.0d+0) 
term(350) = term(350) * (4.0d+0) 
term(352) = term(352) * (-2.0d+0) 
term(353) = term(353) * (-2.0d+0) 
term(354) = term(354) * (4.0d+0) 
term(355) = term(355) * (-3.9999999999999996d+0) 
term(356) = term(356) * (7.999999999999999d+0) 
term(357) = term(357) * (3.9999999999999996d+0) 
term(358) = term(358) * (-7.999999999999999d+0) 
term(359) = term(359) * (2.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (-3.9999999999999996d+0) 
term(362) = term(362) * (7.999999999999999d+0) 
term(363) = term(363) * (0.6666666666666666d+0) 
term(364) = term(364) * (-1.3333333333333333d+0) 
term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (4.0d+0) 
term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-2.0d+0) 
term(370) = term(370) * (4.0d+0) 
term(371) = term(371) * (-4.0d+0) 
term(372) = term(372) * (8.0d+0) 
term(373) = term(373) * (7.999999999999999d+0) 
term(374) = term(374) * (-15.999999999999998d+0) 
term(375) = term(375) * (3.9999999999999996d+0) 
term(376) = term(376) * (-7.999999999999999d+0) 
term(377) = term(377) * (-1.9999999999999998d+0) 
term(378) = term(378) * (3.9999999999999996d+0) 
term(380) = term(380) * (-2.0d+0) 
term(382) = term(382) * (-2.0d+0) 
term(383) = term(383) * (1.3333333333333333d+0) 
term(384) = term(384) * (-2.6666666666666665d+0) 
term(385) = term(385) * (-2.0d+0) 
term(386) = term(386) * (4.0d+0) 
term(387) = term(387) * (16.0d+0) 
term(388) = term(388) * (-16.0d+0) 
term(389) = term(389) * (-8.0d+0) 
term(390) = term(390) * (8.0d+0) 
term(391) = term(391) * (-8.0d+0) 
term(392) = term(392) * (8.0d+0) 
term(393) = term(393) * (4.0d+0) 
term(394) = term(394) * (-4.0d+0) 
term(395) = term(395) * (1.9999999999999998d+0) 
term(396) = term(396) * (-3.9999999999999996d+0) 
term(397) = term(397) * (-7.999999999999999d+0) 
term(398) = term(398) * (15.999999999999998d+0) 
term(399) = term(399) * (-0.9999999999999999d+0) 
term(400) = term(400) * (1.9999999999999998d+0) 
term(401) = term(401) * (3.9999999999999996d+0) 
term(402) = term(402) * (-7.999999999999999d+0) 
term(403) = term(403) * (-0.9999999999999999d+0) 
term(404) = term(404) * (1.9999999999999998d+0) 
term(405) = term(405) * (1.9999999999999998d+0) 
term(406) = term(406) * (-3.9999999999999996d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (-2.0d+0) 
term(410) = term(410) * (8.0d+0) 
term(411) = term(411) * (-2.0d+0) 
term(412) = term(412) * (4.0d+0) 
term(413) = term(413) * (8.0d+0) 
term(414) = term(414) * (-16.0d+0) 
term(415) = term(415) * (4.0d+0) 
term(416) = term(416) * (-8.0d+0) 
term(417) = term(417) * (-8.0d+0) 
term(418) = term(418) * (16.0d+0) 
term(420) = term(420) * (-2.0d+0) 
term(421) = term(421) * (-2.0d+0) 
term(422) = term(422) * (4.0d+0) 
term(423) = term(423) * (4.0d+0) 
term(424) = term(424) * (-8.0d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (4.0d+0) 
term(427) = term(427) * (-4.0d+0) 
term(428) = term(428) * (8.0d+0) 
term(429) = term(429) * (2.0d+0) 
term(430) = term(430) * (-4.0d+0) 
term(431) = term(431) * (-4.0d+0) 
term(432) = term(432) * (8.0d+0) 
term(433) = term(433) * (8.0d+0) 
term(434) = term(434) * (-16.0d+0) 
term(435) = term(435) * (-3.9999999999999996d+0) 
term(436) = term(436) * (8.0d+0) 
term(437) = term(437) * (-3.9999999999999996d+0) 
term(438) = term(438) * (7.999999999999999d+0) 
term(439) = term(439) * (7.999999999999999d+0) 
term(440) = term(440) * (-16.0d+0) 
term(441) = term(441) * (1.9999999999999998d+0) 
term(442) = term(442) * (-4.0d+0) 
term(443) = term(443) * (1.9999999999999998d+0) 
term(444) = term(444) * (-3.9999999999999996d+0) 
term(445) = term(445) * (-3.9999999999999996d+0) 
term(446) = term(446) * (8.0d+0) 
term(447) = term(447) * (2.0d+0) 
term(448) = term(448) * (-2.0d+0) 
term(449) = term(449) * (-4.0d+0) 
term(450) = term(450) * (4.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (-2.0d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (4.0d+0) 
term(455) = term(455) * (-1.0d+0) 
term(456) = term(456) * (2.0d+0) 
term(457) = term(457) * (-1.0d+0) 
term(458) = term(458) * (2.0d+0) 
term(460) = term(460) * (-2.0d+0) 
term(462) = term(462) * (-2.0d+0) 
term(463) = term(463) * (1.9999999999999998d+0) 
term(464) = term(464) * (-3.9999999999999996d+0) 
term(465) = term(465) * (-1.9999999999999998d+0) 
term(466) = term(466) * (3.9999999999999996d+0) 
term(467) = term(467) * (-3.9999999999999996d+0) 
term(468) = term(468) * (7.999999999999999d+0) 
term(469) = term(469) * (3.9999999999999996d+0) 
term(470) = term(470) * (-7.999999999999999d+0) 
term(471) = term(471) * (-4.0d+0) 
term(472) = term(472) * (2.0d+0) 
term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (8.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (8.0d+0) 
term(477) = term(477) * (8.0d+0) 
term(478) = term(478) * (-4.0d+0) 
term(479) = term(479) * (-3.9999999999999996d+0) 
term(480) = term(480) * (1.9999999999999998d+0) 
term(482) = term(482) * (-2.0d+0) 
term(483) = term(483) * (-2.0d+0) 
term(485) = term(485) * (8.0d+0) 
term(486) = term(486) * (-4.0d+0) 
term(487) = term(487) * (8.0d+0) 
term(488) = term(488) * (-16.0d+0) 
term(489) = term(489) * (4.0d+0) 
term(490) = term(490) * (-2.0d+0) 
term(491) = term(491) * (-2.0d+0) 
term(492) = term(492) * (8.0d+0) 
term(493) = term(493) * (-16.0d+0) 
term(494) = term(494) * (4.0d+0) 
term(495) = term(495) * (-3.9999999999999996d+0) 
term(496) = term(496) * (7.999999999999999d+0) 
term(497) = term(497) * (2.0d+0) 
term(498) = term(498) * (-4.0d+0) 
term(499) = term(499) * (15.999999999999998d+0) 
term(500) = term(500) * (-16.0d+0) 
term(501) = term(501) * (-7.999999999999999d+0) 
term(502) = term(502) * (8.0d+0) 
term(503) = term(503) * (-7.999999999999999d+0) 
term(504) = term(504) * (8.0d+0) 
term(505) = term(505) * (3.9999999999999996d+0) 
term(506) = term(506) * (-4.0d+0) 
term(507) = term(507) * (4.0d+0) 
term(508) = term(508) * (-8.0d+0) 
term(509) = term(509) * (-8.0d+0) 
term(510) = term(510) * (16.0d+0) 
term(511) = term(511) * (-2.0d+0) 
term(512) = term(512) * (4.0d+0) 
term(513) = term(513) * (-2.0d+0) 
term(514) = term(514) * (4.0d+0) 
term(515) = term(515) * (7.999999999999999d+0) 
term(516) = term(516) * (-15.999999999999998d+0) 
term(517) = term(517) * (-2.0d+0) 
term(518) = term(518) * (4.0d+0) 
term(519) = term(519) * (4.0d+0) 
term(520) = term(520) * (-8.0d+0) 
term(521) = term(521) * (-2.0d+0) 
term(522) = term(522) * (4.0d+0) 
term(524) = term(524) * (-2.0d+0) 
term(526) = term(526) * (-2.0d+0) 
term(527) = term(527) * (-3.9999999999999996d+0) 
term(528) = term(528) * (7.999999999999999d+0) 
term(529) = term(529) * (-2.0d+0) 
term(530) = term(530) * (4.0d+0) 
term(531) = term(531) * (4.0d+0) 
term(532) = term(532) * (-8.0d+0) 
term(534) = term(534) * (-2.0d+0) 
term(536) = term(536) * (-2.0d+0) 
term(537) = term(537) * (-2.0d+0) 
term(538) = term(538) * (4.0d+0) 
term(539) = term(539) * (-3.9999999999999996d+0) 
term(540) = term(540) * (7.999999999999999d+0) 
term(541) = term(541) * (4.0d+0) 
term(542) = term(542) * (-8.0d+0) 
term(543) = term(543) * (1.9999999999999998d+0) 
term(544) = term(544) * (-3.9999999999999996d+0) 
term(545) = term(545) * (-2.0d+0) 
term(546) = term(546) * (4.0d+0) 
term(547) = term(547) * (2.0d+0) 
term(548) = term(548) * (-4.0d+0) 
term(549) = term(549) * (2.0d+0) 
term(550) = term(550) * (-4.0d+0) 
term(551) = term(551) * (-4.0d+0) 
term(552) = term(552) * (8.0d+0) 
term(553) = term(553) * (-4.0d+0) 
term(554) = term(554) * (8.0d+0) 
term(555) = term(555) * (2.0d+0) 
term(556) = term(556) * (-4.0d+0) 
term(557) = term(557) * (2.0d+0) 
term(558) = term(558) * (-4.0d+0) 
term(559) = term(559) * (-1.0d+0) 
term(560) = term(560) * (2.0d+0) 
term(561) = term(561) * (-1.0d+0) 
term(562) = term(562) * (2.0d+0) 
term(563) = term(563) * (2.0d+0) 
term(564) = term(564) * (-4.0d+0) 
term(565) = term(565) * (2.0d+0) 
term(566) = term(566) * (-4.0d+0) 
term(567) = term(567) * (-1.0d+0) 
term(568) = term(568) * (2.0d+0) 
term(569) = term(569) * (-1.0d+0) 
term(570) = term(570) * (2.0d+0) 
term(571) = term(571) * (-2.0d+0) 
term(572) = term(572) * (4.0d+0) 
term(573) = term(573) * (1.9999999999999998d+0) 
term(574) = term(574) * (-3.9999999999999996d+0) 
term(575) = term(575) * (-3.9999999999999996d+0) 
term(576) = term(576) * (7.999999999999999d+0) 
term(577) = term(577) * (4.0d+0) 
term(578) = term(578) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(579) = term(579) + wm_interm_5_pt3(a,i,j,k) * wm_interm_84_pt3(a,i,k,j)
term(580) = term(580) + wm_interm_5_pt3(a,i,j,k) * wm_interm_85_pt3(a,i,k,j)
term(581) = term(581) + wm_interm_5_pt3(a,i,j,k) * wm_interm_86_pt3(a,i,p,j,k,q)
term(582) = term(582) + wm_interm_5_pt3(a,i,j,k) * wm_interm_88_pt3(a,i,k,j)
term(583) = term(583) + wm_interm_5_pt3(a,i,j,k) * wm_interm_88_pt3(a,i,j,k)
term(584) = term(584) + wm_interm_5_pt3(a,i,j,k) * wm_interm_86_pt3(a,i,p,q,j,k)
term(585) = term(585) + wm_interm_5_pt3(a,i,j,k) * wm_interm_86_pt3(a,i,p,j,q,k)
term(586) = term(586) + wm_interm_5_pt3(a,i,j,k) * wm_interm_84_pt3(a,i,j,k)
term(587) = term(587) + wm_interm_5_pt3(a,i,j,k) * wm_interm_85_pt3(a,i,j,k)
term(588) = term(588) + wm_interm_101_pt3(a,p,i,j,k,q) * wm_interm_128_pt3(a,k,j,i)
term(589) = term(589) + wm_interm_101_pt3(a,p,i,j,k,q) * wm_interm_128_pt3(a,k,i,j)
term(590) = term(590) + wm_interm_101_pt3(a,i,p,j,k,q) * wm_interm_128_pt3(a,k,i,j)
term(591) = term(591) + wm_interm_101_pt3(a,i,p,j,k,q) * wm_interm_128_pt3(a,k,j,i)
term(592) = term(592) + wm_interm_101_pt3(a,i,j,p,k,q) * wm_interm_128_pt3(a,k,i,j)
term(593) = term(593) + wm_interm_11_pt3(a,i,j,k) * wm_interm_137_pt3(a,k,i,j)
term(594) = term(594) + wm_interm_11_pt3(a,i,j,k) * wm_interm_136_pt3(a,k,i,j)
term(595) = term(595) + wm_interm_11_pt3(a,i,j,k) * wm_interm_138_pt3(a,k,i,j)
term(596) = term(596) + wm_interm_139_pt3(a,i,j,k) * wm_interm_140_pt3(a,j,i,p,q,k)
term(597) = term(597) + wm_interm_139_pt3(a,i,j,k) * wm_interm_140_pt3(a,j,i,p,k,q)
term(598) = term(598) + wm_interm_139_pt3(a,i,j,k) * wm_interm_13_pt3(a,j,i,k)
term(599) = term(599) + wm_interm_139_pt3(a,i,j,k) * wm_interm_14_pt3(a,j,i,k)
term(600) = term(600) + wm_interm_139_pt3(a,i,j,k) * wm_interm_140_pt3(a,p,i,j,k,q)
term(601) = term(601) + wm_interm_139_pt3(a,i,j,k) * wm_interm_140_pt3(a,p,i,j,q,k)
term(602) = term(602) + wm_interm_139_pt3(a,i,j,k) * wm_interm_140_pt3(a,i,j,p,q,k)
term(603) = term(603) + wm_interm_139_pt3(a,i,j,k) * wm_interm_140_pt3(a,i,j,p,k,q)
term(604) = term(604) + wm_interm_139_pt3(a,i,j,k) * wm_interm_16_pt3(a,i,j,k)
term(605) = term(605) + wm_interm_139_pt3(a,i,j,k) * wm_interm_15_pt3(a,i,j,k)
term(606) = term(606) + wm_interm_139_pt3(a,i,j,k) * wm_interm_13_pt3(a,i,j,k)
term(607) = term(607) + wm_interm_139_pt3(a,i,j,k) * wm_interm_14_pt3(a,i,j,k)
term(608) = term(608) + wm_interm_11_pt3(a,i,j,k) * wm_interm_141_pt3(a,k,i,j)
term(609) = term(609) + wm_interm_140_pt3(a,p,i,j,q,k) * wm_interm_142_pt3(a,i,j,k)
term(610) = term(610) + wm_interm_140_pt3(a,i,j,p,k,q) * wm_interm_142_pt3(a,i,j,k)
term(611) = term(611) + wm_interm_142_pt3(a,i,j,k) * wm_interm_15_pt3(a,i,j,k)
term(612) = term(612) + wm_interm_142_pt3(a,i,j,k) * wm_interm_14_pt3(a,i,j,k)
term(613) = term(613) + wm_interm_140_pt3(a,i,j,p,q,k) * wm_interm_142_pt3(a,i,j,k)
term(614) = term(614) + wm_interm_140_pt3(a,i,j,p,q,k) * wm_interm_142_pt3(a,j,i,k)
term(615) = term(615) + wm_interm_140_pt3(a,p,i,j,k,q) * wm_interm_142_pt3(a,i,j,k)
term(616) = term(616) + wm_interm_140_pt3(a,i,j,p,k,q) * wm_interm_142_pt3(a,j,i,k)
term(617) = term(617) + wm_interm_13_pt3(a,i,j,k) * wm_interm_142_pt3(a,i,j,k)
term(618) = term(618) + wm_interm_13_pt3(a,i,j,k) * wm_interm_142_pt3(a,j,i,k)
term(619) = term(619) + wm_interm_142_pt3(a,i,j,k) * wm_interm_16_pt3(a,i,j,k)
term(620) = term(620) + wm_interm_142_pt3(a,i,j,k) * wm_interm_14_pt3(a,j,i,k)
term(621) = term(621) + wm_interm_146_pt3(a,i,j,k) * wm_interm_32_pt3(a,k,i,j)
term(622) = term(622) + wm_interm_146_pt3(a,i,j,k) * wm_interm_36_pt3(a,k,i,j)
term(623) = term(623) + wm_interm_143_pt3(a,i,j,k) * wm_interm_32_pt3(a,k,i,j)
term(624) = term(624) + wm_interm_143_pt3(a,i,j,k) * wm_interm_36_pt3(a,k,i,j)
term(625) = term(625) + wm_interm_106_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,k,j)
term(626) = term(626) + wm_interm_107_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,k,j)
term(627) = term(627) + wm_interm_106_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,j,k)
term(628) = term(628) + wm_interm_107_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,j,k)
term(629) = term(629) + wm_interm_114_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,k,j)
term(630) = term(630) + wm_interm_146_pt3(a,i,j,k) * wm_interm_38_pt3(a,k,i,j)
term(631) = term(631) + wm_interm_114_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,j,k)
term(632) = term(632) + wm_interm_146_pt3(a,i,j,k) * wm_interm_40_pt3(a,k,i,j)
term(633) = term(633) + wm_interm_143_pt3(a,i,j,k) * wm_interm_40_pt3(a,k,i,j)
term(634) = term(634) + wm_interm_143_pt3(a,i,j,k) * wm_interm_38_pt3(a,k,i,j)
term(635) = term(635) + wm_interm_117_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,j,k)
term(636) = term(636) + wm_interm_117_pt3(a,i,j,k) * wm_interm_44_pt3(a,i,k,j)
term(637) = term(637) + wm_interm_118_pt3(a,i,p,q,j,k) * wm_interm_44_pt3(a,i,k,j)
term(638) = term(638) + wm_interm_118_pt3(a,p,i,q,j,k) * wm_interm_44_pt3(a,i,k,j)
term(639) = term(639) + wm_interm_118_pt3(a,p,i,j,q,k) * wm_interm_44_pt3(a,i,k,j)
term(640) = term(640) + wm_interm_118_pt3(a,i,p,j,q,k) * wm_interm_44_pt3(a,i,k,j)
term(641) = term(641) + wm_interm_119_pt3(a,i,p,q,j,k) * wm_interm_146_pt3(a,j,k,i)
term(642) = term(642) + wm_interm_119_pt3(a,p,i,q,j,k) * wm_interm_146_pt3(a,j,k,i)
term(643) = term(643) + wm_interm_119_pt3(a,p,i,q,j,k) * wm_interm_143_pt3(a,j,k,i)
term(644) = term(644) + wm_interm_119_pt3(a,i,p,q,j,k) * wm_interm_143_pt3(a,j,k,i)
term(645) = term(645) + wm_interm_118_pt3(a,i,p,q,j,k) * wm_interm_44_pt3(a,i,j,k)
term(646) = term(646) + wm_interm_118_pt3(a,p,i,q,j,k) * wm_interm_44_pt3(a,i,j,k)
term(647) = term(647) + wm_interm_118_pt3(a,p,i,j,q,k) * wm_interm_44_pt3(a,i,j,k)
term(648) = term(648) + wm_interm_118_pt3(a,i,p,j,q,k) * wm_interm_44_pt3(a,i,j,k)
term(649) = term(649) + wm_interm_119_pt3(a,p,i,j,k,q) * wm_interm_146_pt3(a,k,j,i)
term(650) = term(650) + wm_interm_119_pt3(a,i,p,j,k,q) * wm_interm_146_pt3(a,k,j,i)
term(651) = term(651) + wm_interm_119_pt3(a,p,i,j,k,q) * wm_interm_143_pt3(a,k,j,i)
term(652) = term(652) + wm_interm_119_pt3(a,i,p,j,k,q) * wm_interm_143_pt3(a,k,j,i)
term(653) = term(653) + wm_interm_119_pt3(a,p,i,j,k,q) * wm_interm_146_pt3(a,j,k,i)
term(654) = term(654) + wm_interm_119_pt3(a,i,p,j,k,q) * wm_interm_146_pt3(a,j,k,i)
term(655) = term(655) + wm_interm_119_pt3(a,p,i,j,k,q) * wm_interm_143_pt3(a,j,k,i)
term(656) = term(656) + wm_interm_119_pt3(a,i,p,j,k,q) * wm_interm_143_pt3(a,j,k,i)
term(657) = term(657) + wm_interm_118_pt3(a,i,p,j,k,q) * wm_interm_44_pt3(a,i,k,j)
term(658) = term(658) + wm_interm_118_pt3(a,p,i,j,k,q) * wm_interm_44_pt3(a,i,k,j)
term(659) = term(659) + wm_interm_118_pt3(a,p,i,j,k,q) * wm_interm_44_pt3(a,i,j,k)
term(660) = term(660) + wm_interm_118_pt3(a,i,p,j,k,q) * wm_interm_44_pt3(a,i,j,k)
term(661) = term(661) + wm_interm_163_pt3(a,q,i,j,k,p) * wm_interm_56_pt3(a,k,i,j)
term(662) = term(662) + wm_interm_163_pt3(a,q,i,j,p,k) * wm_interm_56_pt3(a,k,i,j)
term(663) = term(663) + wm_interm_158_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,i,j)
term(664) = term(664) + wm_interm_157_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,i,j)
term(665) = term(665) + wm_interm_163_pt3(a,i,q,j,k,p) * wm_interm_56_pt3(a,k,i,j)
term(666) = term(666) + wm_interm_163_pt3(a,i,q,j,p,k) * wm_interm_56_pt3(a,k,i,j)
term(667) = term(667) + wm_interm_161_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,i,j)
term(668) = term(668) + wm_interm_162_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,i,j)
term(669) = term(669) + wm_interm_164_pt3(a,i,q,j,k,p) * wm_interm_56_pt3(a,k,i,j)
term(670) = term(670) + wm_interm_164_pt3(a,i,q,j,p,k) * wm_interm_56_pt3(a,k,i,j)
term(671) = term(671) + wm_interm_165_pt3(a,i,q,j,p,k) * wm_interm_56_pt3(a,k,i,j)
term(672) = term(672) + wm_interm_165_pt3(a,i,q,j,k,p) * wm_interm_56_pt3(a,k,i,j)
term(673) = term(673) + wm_interm_153_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,i,j)
term(674) = term(674) + wm_interm_154_pt3(a,i,j,k) * wm_interm_56_pt3(a,k,i,j)
term(675) = term(675) + wm_interm_164_pt3(a,q,i,j,k,p) * wm_interm_56_pt3(a,k,i,j)
term(676) = term(676) + wm_interm_164_pt3(a,q,i,j,p,k) * wm_interm_56_pt3(a,k,i,j)
term(677) = term(677) + wm_interm_165_pt3(a,q,i,j,k,p) * wm_interm_56_pt3(a,k,i,j)
term(678) = term(678) + wm_interm_165_pt3(a,q,i,j,p,k) * wm_interm_56_pt3(a,k,i,j)
term(679) = term(679) + wm_interm_166_pt3(a,i,j,k) * wm_interm_58_pt3(a,j,i,k)
term(680) = term(680) + wm_interm_166_pt3(a,i,j,k) * wm_interm_58_pt3(a,i,j,k)
term(681) = term(681) + wm_interm_166_pt3(a,i,j,k) * wm_interm_61_pt3(a,i,j,k)
term(682) = term(682) + wm_interm_166_pt3(a,i,j,k) * wm_interm_61_pt3(a,j,i,k)
term(683) = term(683) + wm_interm_166_pt3(a,i,j,k) * wm_interm_62_pt3(a,j,i,k)
term(684) = term(684) + wm_interm_166_pt3(a,i,j,k) * wm_interm_62_pt3(a,i,j,k)
term(685) = term(685) + wm_interm_166_pt3(a,i,j,k) * wm_interm_63_pt3(a,j,i,k)
term(686) = term(686) + wm_interm_166_pt3(a,i,j,k) * wm_interm_63_pt3(a,i,j,k)
term(687) = term(687) + wm_interm_165_pt3(a,i,q,j,p,k) * wm_interm_56_pt3(a,k,j,i)
term(688) = term(688) + wm_interm_165_pt3(a,i,q,j,k,p) * wm_interm_56_pt3(a,k,j,i)
term(689) = term(689) + wm_interm_165_pt3(a,q,i,j,k,p) * wm_interm_56_pt3(a,k,j,i)
term(690) = term(690) + wm_interm_165_pt3(a,q,i,j,p,k) * wm_interm_56_pt3(a,k,j,i)
term(691) = term(691) + wm_interm_166_pt3(a,i,j,k) * wm_interm_64_pt3(a,i,j,k)
term(692) = term(692) + wm_interm_166_pt3(a,i,j,k) * wm_interm_64_pt3(a,j,i,k)
term(693) = term(693) + wm_interm_163_pt3(a,q,i,j,p,k) * wm_interm_56_pt3(a,k,j,i)
term(694) = term(694) + wm_interm_163_pt3(a,q,i,j,k,p) * wm_interm_56_pt3(a,k,j,i)
term(695) = term(695) + wm_interm_163_pt3(a,i,j,q,p,k) * wm_interm_56_pt3(a,k,i,j)
term(696) = term(696) + wm_interm_163_pt3(a,i,j,q,k,p) * wm_interm_56_pt3(a,k,i,j)
term(697) = term(697) + wm_interm_164_pt3(a,i,j,q,p,k) * wm_interm_56_pt3(a,k,i,j)
term(698) = term(698) + wm_interm_164_pt3(a,i,j,q,k,p) * wm_interm_56_pt3(a,k,i,j)
term(699) = term(699) + wm_interm_165_pt3(a,i,j,q,k,p) * wm_interm_56_pt3(a,k,i,j)
term(700) = term(700) + wm_interm_165_pt3(a,i,j,q,p,k) * wm_interm_56_pt3(a,k,i,j)
term(701) = term(701) + wm_interm_164_pt3(a,q,i,j,k,p) * wm_interm_56_pt3(a,k,j,i)
term(702) = term(702) + wm_interm_164_pt3(a,q,i,j,p,k) * wm_interm_56_pt3(a,k,j,i)
term(703) = term(703) + wm_interm_166_pt3(a,i,j,k) * wm_interm_65_pt3(a,i,j,k)
term(704) = term(704) + wm_interm_166_pt3(a,i,j,k) * wm_interm_65_pt3(a,j,i,k)
term(705) = term(705) + wm_interm_164_pt3(a,i,q,j,p,k) * wm_interm_56_pt3(a,k,j,i)
term(706) = term(706) + wm_interm_164_pt3(a,i,q,j,k,p) * wm_interm_56_pt3(a,k,j,i)
term(707) = term(707) + wm_interm_163_pt3(a,i,q,j,k,p) * wm_interm_56_pt3(a,k,j,i)
term(708) = term(708) + wm_interm_163_pt3(a,i,q,j,p,k) * wm_interm_56_pt3(a,k,j,i)
term(709) = term(709) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,i,p,j,q,k)
term(710) = term(710) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,i,p,j,q,k)
term(711) = term(711) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,j,i,p,q,k)
term(712) = term(712) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,i,j,p,q,k)
term(713) = term(713) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,p,i,j,q,k)
term(714) = term(714) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,i,j,p,q,k)
term(715) = term(715) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,j,i,p,q,k)
term(716) = term(716) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,p,i,j,q,k)
term(717) = term(717) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,p,i,j,k,q)
term(718) = term(718) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,p,i,j,k,q)
term(719) = term(719) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,p,i,j,k,q)
term(720) = term(720) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,p,i,j,q,k)
term(721) = term(721) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,i,j,p,k,q)
term(722) = term(722) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,j,i,p,k,q)
term(723) = term(723) + wm_interm_166_pt3(a,i,j,k) * wm_interm_167_pt3(a,i,p,j,k,q)
term(724) = term(724) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,i,p,j,k,q)
term(725) = term(725) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,i,j,p,k,q)
term(726) = term(726) + wm_interm_166_pt3(a,i,j,k) * wm_interm_168_pt3(a,j,i,p,k,q)
term(727) = term(727) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,j,i,p,q,k)
term(728) = term(728) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,j,i,p,k,q)
term(729) = term(729) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,i,j,p,q,k)
term(730) = term(730) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,i,p,j,q,k)
term(731) = term(731) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,i,p,j,k,q)
term(732) = term(732) + wm_interm_166_pt3(a,i,j,k) * wm_interm_169_pt3(a,i,j,p,k,q)
end do 
end do 
end do 
end do 

term(579) = term(579) * (-4.0d+0) 
term(580) = term(580) * (8.0d+0) 
term(581) = term(581) * (-3.9999999999999996d+0) 
term(582) = term(582) * (-3.9999999999999996d+0) 
term(583) = term(583) * (7.999999999999999d+0) 
term(584) = term(584) * (-4.0d+0) 
term(585) = term(585) * (8.0d+0) 
term(586) = term(586) * (8.0d+0) 
term(587) = term(587) * (-16.0d+0) 
term(588) = term(588) * (1.9999999999999998d+0) 
term(589) = term(589) * (-3.9999999999999996d+0) 
term(590) = term(590) * (2.0d+0) 
term(591) = term(591) * (-4.0d+0) 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * (-3.9999999999999996d+0) 
term(594) = term(594) * (3.9999999999999996d+0) 
term(595) = term(595) * (7.999999999999999d+0) 
term(597) = term(597) * (-1.9999999999999998d+0) 
term(598) = term(598) * (-1.9999999999999998d+0) 
term(599) = term(599) * (3.9999999999999996d+0) 
term(601) = term(601) * (-1.9999999999999998d+0) 
term(602) = term(602) * (-1.9999999999999998d+0) 
term(603) = term(603) * (3.9999999999999996d+0) 
term(604) = term(604) * (-1.9999999999999998d+0) 
term(605) = term(605) * (3.9999999999999996d+0) 
term(606) = term(606) * (3.9999999999999996d+0) 
term(607) = term(607) * (-7.999999999999999d+0) 
term(608) = term(608) * (-1.9999999999999998d+0) 
term(610) = term(610) * (-1.9999999999999998d+0) 
term(611) = term(611) * (-1.9999999999999998d+0) 
term(612) = term(612) * (3.9999999999999996d+0) 
term(614) = term(614) * (-1.9999999999999998d+0) 
term(615) = term(615) * (-1.9999999999999998d+0) 
term(616) = term(616) * (3.9999999999999996d+0) 
term(617) = term(617) * (-1.9999999999999998d+0) 
term(618) = term(618) * (3.9999999999999996d+0) 
term(619) = term(619) * (3.9999999999999996d+0) 
term(620) = term(620) * (-7.999999999999999d+0) 
term(621) = term(621) * (4.0d+0) 
term(622) = term(622) * (-8.0d+0) 
term(623) = term(623) * (-2.0d+0) 
term(624) = term(624) * (4.0d+0) 
term(625) = term(625) * (-4.0d+0) 
term(626) = term(626) * (8.0d+0) 
term(627) = term(627) * (8.0d+0) 
term(628) = term(628) * (-16.0d+0) 
term(629) = term(629) * (-2.0d+0) 
term(630) = term(630) * (-2.0d+0) 
term(631) = term(631) * (4.0d+0) 
term(632) = term(632) * (4.0d+0) 
term(633) = term(633) * (-2.0d+0) 
term(634) = term(634) * (4.0d+0) 
term(635) = term(635) * (-2.0d+0) 
term(636) = term(636) * (4.0d+0) 
term(638) = term(638) * (-2.0d+0) 
term(640) = term(640) * (-2.0d+0) 
term(642) = term(642) * (-2.0d+0) 
term(644) = term(644) * (-2.0d+0) 
term(645) = term(645) * (-2.0d+0) 
term(646) = term(646) * (4.0d+0) 
term(647) = term(647) * (-2.0d+0) 
term(648) = term(648) * (4.0d+0) 
term(650) = term(650) * (-2.0d+0) 
term(651) = term(651) * (-1.9999999999999998d+0) 
term(652) = term(652) * (4.0d+0) 
term(653) = term(653) * (-1.9999999999999998d+0) 
term(654) = term(654) * (4.0d+0) 
term(656) = term(656) * (-2.0d+0) 
term(658) = term(658) * (-2.0d+0) 
term(660) = term(660) * (-2.0d+0) 
term(661) = term(661) * (0.3333333333333333d+0) 
term(662) = term(662) * (-0.6666666666666666d+0) 
term(663) = term(663) * (-2.0d+0) 
term(664) = term(664) * (4.0d+0) 
term(665) = term(665) * (-0.6666666666666666d+0) 
term(666) = term(666) * (1.3333333333333333d+0) 
term(667) = term(667) * (4.0d+0) 
term(668) = term(668) * (-8.0d+0) 
term(669) = term(669) * (0.3333333333333333d+0) 
term(670) = term(670) * (-0.6666666666666666d+0) 
term(671) = term(671) * (0.3333333333333333d+0) 
term(672) = term(672) * (-0.6666666666666666d+0) 
term(673) = term(673) * (-2.0d+0) 
term(674) = term(674) * (4.0d+0) 
term(675) = term(675) * (-0.6666666666666666d+0) 
term(676) = term(676) * (1.3333333333333333d+0) 
term(677) = term(677) * (0.3333333333333333d+0) 
term(678) = term(678) * (-0.6666666666666666d+0) 
term(679) = term(679) * (-2.0d+0) 
term(680) = term(680) * (4.0d+0) 
term(681) = term(681) * (-2.0d+0) 
term(682) = term(682) * (4.0d+0) 
term(683) = term(683) * (4.0d+0) 
term(684) = term(684) * (-8.0d+0) 
term(685) = term(685) * (-2.0d+0) 
term(686) = term(686) * (4.0d+0) 
term(687) = term(687) * (-0.6666666666666666d+0) 
term(688) = term(688) * (1.3333333333333333d+0) 
term(689) = term(689) * (-0.6666666666666666d+0) 
term(690) = term(690) * (1.3333333333333333d+0) 
term(691) = term(691) * (4.0d+0) 
term(692) = term(692) * (-8.0d+0) 
term(693) = term(693) * (0.3333333333333333d+0) 
term(694) = term(694) * (-0.6666666666666666d+0) 
term(695) = term(695) * (-0.6666666666666666d+0) 
term(696) = term(696) * (1.3333333333333333d+0) 
term(697) = term(697) * (0.3333333333333333d+0) 
term(698) = term(698) * (-0.6666666666666666d+0) 
term(699) = term(699) * (0.3333333333333333d+0) 
term(700) = term(700) * (-0.6666666666666666d+0) 
term(701) = term(701) * (0.3333333333333333d+0) 
term(702) = term(702) * (-0.6666666666666666d+0) 
term(703) = term(703) * (-2.0d+0) 
term(704) = term(704) * (4.0d+0) 
term(705) = term(705) * (0.3333333333333333d+0) 
term(706) = term(706) * (-0.6666666666666666d+0) 
term(707) = term(707) * (0.3333333333333333d+0) 
term(708) = term(708) * (-0.6666666666666666d+0) 
term(709) = term(709) * (0.3333333333333333d+0) 
term(710) = term(710) * (-0.6666666666666666d+0) 
term(711) = term(711) * (0.3333333333333333d+0) 
term(712) = term(712) * (-0.6666666666666666d+0) 
term(713) = term(713) * (-0.6666666666666666d+0) 
term(714) = term(714) * (0.3333333333333333d+0) 
term(715) = term(715) * (-0.6666666666666666d+0) 
term(716) = term(716) * (1.3333333333333333d+0) 
term(717) = term(717) * (0.3333333333333333d+0) 
term(718) = term(718) * (-0.6666666666666666d+0) 
term(719) = term(719) * (0.3333333333333333d+0) 
term(720) = term(720) * (-0.6666666666666666d+0) 
term(721) = term(721) * (0.3333333333333333d+0) 
term(722) = term(722) * (-0.6666666666666666d+0) 
term(723) = term(723) * (-0.6666666666666666d+0) 
term(724) = term(724) * (0.3333333333333333d+0) 
term(725) = term(725) * (-0.6666666666666666d+0) 
term(726) = term(726) * (1.3333333333333333d+0) 
term(727) = term(727) * (0.3333333333333333d+0) 
term(728) = term(728) * (-0.6666666666666666d+0) 
term(729) = term(729) * (-0.6666666666666666d+0) 
term(730) = term(730) * (1.3333333333333333d+0) 
term(731) = term(731) * (-0.6666666666666666d+0) 
term(732) = term(732) * (1.3333333333333333d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(733) = term(733) + wm_interm_5_pt3(a,i,j,k) * wm_interm_86_pt3(a,i,p,q,k,j)
term(734) = term(734) + wm_interm_5_pt3(a,i,j,k) * wm_interm_86_pt3(a,i,p,k,q,j)
term(735) = term(735) + wm_interm_5_pt3(a,i,j,k) * wm_interm_86_pt3(a,i,p,k,j,q)
term(736) = term(736) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,q,k,i,p,j)
term(737) = term(737) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,q,k,i,j,p)
term(738) = term(738) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,k,q,i,j,p)
term(739) = term(739) + wm_interm_11_pt3(a,i,j,k) * wm_interm_135_pt3(a,k,q,i,p,j)
end do 
end do 
end do 
end do 

term(733) = term(733) * (2.0d+0) 
term(734) = term(734) * (-4.0d+0) 
term(735) = term(735) * (1.9999999999999998d+0) 
term(737) = term(737) * (-1.9999999999999998d+0) 
term(739) = term(739) * (-1.9999999999999998d+0) 


    calc_D_oo_wm_cc3_pt3 = zero
    do s = 0, 739
    calc_D_oo_wm_cc3_pt3 = calc_D_oo_wm_cc3_pt3 + term(s)
    end do

    end function calc_D_oo_wm_cc3_pt3
    
    function calc_D_ov_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, l, b, c 
    real(F64), dimension(0:277) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_pt3(q,a,i,j) * wm_interm_1_pt3(a,p,j,i)
term(1) = term(1) + wm_interm_0_pt3(q,a,i,j) * wm_interm_1_pt3(a,j,p,i)
term(2) = term(2) + wm_interm_0_pt3(q,a,i,j) * wm_interm_2_pt3(a,j,p,i)
term(3) = term(3) + wm_interm_0_pt3(q,a,i,j) * wm_interm_2_pt3(a,p,j,i)
term(4) = term(4) + wm_interm_0_pt3(q,a,i,j) * wm_interm_3_pt3(a,p,j,i)
term(5) = term(5) + wm_interm_0_pt3(q,a,i,j) * wm_interm_3_pt3(a,j,p,i)
term(6) = term(6) + wm_interm_13_pt3(a,p,i,j) * wm_interm_31_pt3(a,q,i,j)
term(7) = term(7) + wm_interm_13_pt3(a,i,p,j) * wm_interm_31_pt3(a,q,i,j)
term(8) = term(8) + wm_interm_16_pt3(a,i,p,j) * wm_interm_31_pt3(a,q,i,j)
term(9) = term(9) + wm_interm_14_pt3(a,p,i,j) * wm_interm_31_pt3(a,q,i,j)
term(10) = term(10) + wm_interm_15_pt3(a,i,p,j) * wm_interm_31_pt3(a,q,i,j)
term(11) = term(11) + wm_interm_14_pt3(a,i,p,j) * wm_interm_31_pt3(a,q,i,j)
term(12) = term(12) + wm_interm_43_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,j,i)
term(13) = term(13) + wm_interm_44_pt3(a,p,i,j) * wm_interm_45_pt3(a,q,i,j)
term(14) = term(14) + wm_interm_44_pt3(a,p,i,j) * wm_interm_48_pt3(a,q,i,j)
term(15) = term(15) + wm_interm_43_pt3(a,q,i,j) * wm_interm_44_pt3(a,p,i,j)
term(16) = term(16) + wm_interm_44_pt3(a,p,i,j) * wm_interm_48_pt3(a,q,j,i)
term(17) = term(17) + wm_interm_44_pt3(a,p,i,j) * wm_interm_45_pt3(a,q,j,i)
term(18) = term(18) + wm_interm_55_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,i,j)
term(19) = term(19) + wm_interm_50_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,i,j)
term(20) = term(20) + wm_interm_53_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,i,j)
term(21) = term(21) + wm_interm_52_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,i,j)
term(22) = term(22) + wm_interm_53_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,i,j)
term(23) = term(23) + wm_interm_52_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_50_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,i,j)
term(25) = term(25) + wm_interm_55_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,i,j)
term(26) = term(26) + wm_interm_52_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,j,i)
term(27) = term(27) + wm_interm_50_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,j,i)
term(28) = term(28) + wm_interm_55_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,j,i)
term(29) = term(29) + wm_interm_50_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,j,i)
term(30) = term(30) + wm_interm_54_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,i,j)
term(31) = term(31) + wm_interm_54_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,i,j)
term(32) = term(32) + wm_interm_55_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,j,i)
term(33) = term(33) + wm_interm_52_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,j,i)
term(34) = term(34) + wm_interm_53_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,j,i)
term(35) = term(35) + wm_interm_54_pt3(q,a,i,j) * wm_interm_56_pt3(a,p,j,i)
term(36) = term(36) + wm_interm_54_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,j,i)
term(37) = term(37) + wm_interm_53_pt3(a,q,i,j) * wm_interm_56_pt3(a,p,j,i)
term(38) = term(38) + wm_interm_1_pt3(a,i,p,j) * wm_interm_77_pt3(q,a,j,i)
term(39) = term(39) + wm_interm_1_pt3(a,i,p,j) * wm_interm_78_pt3(q,a,j,i)
term(40) = term(40) + wm_interm_2_pt3(a,i,p,j) * wm_interm_78_pt3(q,a,j,i)
term(41) = term(41) + wm_interm_2_pt3(a,i,p,j) * wm_interm_77_pt3(q,a,j,i)
term(42) = term(42) + wm_interm_1_pt3(a,p,i,j) * wm_interm_78_pt3(q,a,j,i)
term(43) = term(43) + wm_interm_1_pt3(a,p,i,j) * wm_interm_77_pt3(q,a,j,i)
term(44) = term(44) + wm_interm_2_pt3(a,p,i,j) * wm_interm_77_pt3(q,a,j,i)
term(45) = term(45) + wm_interm_2_pt3(a,p,i,j) * wm_interm_78_pt3(q,a,j,i)
term(46) = term(46) + wm_interm_3_pt3(a,i,p,j) * wm_interm_77_pt3(q,a,j,i)
term(47) = term(47) + wm_interm_3_pt3(a,i,p,j) * wm_interm_78_pt3(q,a,j,i)
term(48) = term(48) + wm_interm_3_pt3(a,p,i,j) * wm_interm_78_pt3(q,a,j,i)
term(49) = term(49) + wm_interm_3_pt3(a,p,i,j) * wm_interm_77_pt3(q,a,j,i)
term(50) = term(50) + wm_interm_1_pt3(a,p,i,j) * wm_interm_80_pt3(q,a,j,i)
term(51) = term(51) + wm_interm_1_pt3(a,p,i,j) * wm_interm_81_pt3(q,a,j,i)
term(52) = term(52) + wm_interm_1_pt3(a,i,p,j) * wm_interm_80_pt3(q,a,j,i)
term(53) = term(53) + wm_interm_1_pt3(a,i,p,j) * wm_interm_81_pt3(q,a,j,i)
term(54) = term(54) + wm_interm_2_pt3(a,i,p,j) * wm_interm_80_pt3(q,a,j,i)
term(55) = term(55) + wm_interm_2_pt3(a,i,p,j) * wm_interm_81_pt3(q,a,j,i)
term(56) = term(56) + wm_interm_2_pt3(a,p,i,j) * wm_interm_80_pt3(q,a,j,i)
term(57) = term(57) + wm_interm_2_pt3(a,p,i,j) * wm_interm_81_pt3(q,a,j,i)
term(58) = term(58) + wm_interm_3_pt3(a,p,i,j) * wm_interm_80_pt3(q,a,j,i)
term(59) = term(59) + wm_interm_3_pt3(a,p,i,j) * wm_interm_81_pt3(q,a,j,i)
term(60) = term(60) + wm_interm_3_pt3(a,i,p,j) * wm_interm_80_pt3(q,a,j,i)
term(61) = term(61) + wm_interm_3_pt3(a,i,p,j) * wm_interm_81_pt3(q,a,j,i)
term(62) = term(62) + wm_interm_84_pt3(a,i,j,p) * wm_interm_87_pt3(q,a,i,j)
term(63) = term(63) + wm_interm_85_pt3(a,i,j,p) * wm_interm_87_pt3(q,a,i,j)
term(64) = term(64) + wm_interm_87_pt3(q,a,i,j) * wm_interm_88_pt3(a,i,j,p)
term(65) = term(65) + wm_interm_87_pt3(q,a,i,j) * wm_interm_88_pt3(a,i,p,j)
term(66) = term(66) + wm_interm_84_pt3(a,i,p,j) * wm_interm_87_pt3(q,a,i,j)
term(67) = term(67) + wm_interm_85_pt3(a,i,p,j) * wm_interm_87_pt3(q,a,i,j)
term(68) = term(68) + wm_interm_84_pt3(a,i,j,p) * wm_interm_97_pt3(q,a,i,j)
term(69) = term(69) + wm_interm_85_pt3(a,i,j,p) * wm_interm_97_pt3(q,a,i,j)
term(70) = term(70) + wm_interm_88_pt3(a,i,j,p) * wm_interm_97_pt3(q,a,i,j)
term(71) = term(71) + wm_interm_88_pt3(a,i,p,j) * wm_interm_97_pt3(q,a,i,j)
term(72) = term(72) + wm_interm_84_pt3(a,i,p,j) * wm_interm_97_pt3(q,a,i,j)
term(73) = term(73) + wm_interm_85_pt3(a,i,p,j) * wm_interm_97_pt3(q,a,i,j)
term(74) = term(74) + wm_interm_84_pt3(a,i,p,j) * wm_interm_98_pt3(q,a,i,j)
term(75) = term(75) + wm_interm_85_pt3(a,i,p,j) * wm_interm_98_pt3(q,a,i,j)
term(76) = term(76) + wm_interm_88_pt3(a,i,p,j) * wm_interm_98_pt3(q,a,i,j)
term(77) = term(77) + wm_interm_88_pt3(a,i,j,p) * wm_interm_98_pt3(q,a,i,j)
term(78) = term(78) + wm_interm_84_pt3(a,i,j,p) * wm_interm_98_pt3(q,a,i,j)
term(79) = term(79) + wm_interm_85_pt3(a,i,j,p) * wm_interm_98_pt3(q,a,i,j)
term(80) = term(80) + wm_interm_102_pt3(q,a,i,j) * wm_interm_84_pt3(a,i,j,p)
term(81) = term(81) + wm_interm_102_pt3(q,a,i,j) * wm_interm_85_pt3(a,i,j,p)
term(82) = term(82) + wm_interm_102_pt3(q,a,i,j) * wm_interm_88_pt3(a,i,j,p)
term(83) = term(83) + wm_interm_102_pt3(q,a,i,j) * wm_interm_88_pt3(a,i,p,j)
term(84) = term(84) + wm_interm_102_pt3(q,a,i,j) * wm_interm_84_pt3(a,i,p,j)
term(85) = term(85) + wm_interm_102_pt3(q,a,i,j) * wm_interm_85_pt3(a,i,p,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-1.9999999999999998d+0) 
term(3) = term(3) * (3.9999999999999996d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-8.0d+0) 
term(6) = term(6) * (-1.9999999999999998d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (-1.9999999999999998d+0) 
term(9) = term(9) * (3.9999999999999996d+0) 
term(10) = term(10) * (3.9999999999999996d+0) 
term(11) = term(11) * (-8.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (-0.3333333333333333d+0) 
term(19) = term(19) * (1.3333333333333333d+0) 
term(20) = term(20) * (1.3333333333333333d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-0.6666666666666666d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-0.6666666666666666d+0) 
term(25) = term(25) * (0.6666666666666666d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (1.3333333333333333d+0) 
term(28) = term(28) * (0.6666666666666666d+0) 
term(29) = term(29) * (-0.6666666666666666d+0) 
term(30) = term(30) * (0.6666666666666666d+0) 
term(31) = term(31) * (-0.3333333333333333d+0) 
term(32) = term(32) * (-0.3333333333333333d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (1.3333333333333333d+0) 
term(35) = term(35) * (-0.3333333333333333d+0) 
term(36) = term(36) * (0.6666666666666666d+0) 
term(37) = term(37) * (-0.6666666666666666d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (1.9999999999999998d+0) 
term(41) = term(41) * (-3.9999999999999996d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (1.9999999999999998d+0) 
term(45) = term(45) * (-3.9999999999999996d+0) 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * (8.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (8.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (8.0d+0) 
term(54) = term(54) * (1.9999999999999998d+0) 
term(55) = term(55) * (-3.9999999999999996d+0) 
term(56) = term(56) * (-3.9999999999999996d+0) 
term(57) = term(57) * (7.999999999999999d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (8.0d+0) 
term(60) = term(60) * (8.0d+0) 
term(61) = term(61) * (-16.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (8.0d+0) 
term(64) = term(64) * (-4.0d+0) 
term(65) = term(65) * (8.0d+0) 
term(66) = term(66) * (7.999999999999999d+0) 
term(67) = term(67) * (-15.999999999999998d+0) 
term(68) = term(68) * (2.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (-3.9999999999999996d+0) 
term(73) = term(73) * (7.999999999999999d+0) 
term(74) = term(74) * (1.9999999999999998d+0) 
term(75) = term(75) * (-3.9999999999999996d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (8.0d+0) 
term(80) = term(80) * (2.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * (-3.9999999999999996d+0) 
term(85) = term(85) * (7.999999999999999d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(86) = term(86) + wm_interm_19_pt3(a,q,p,i) * wm_interm_20_pt3(a,i)
term(87) = term(87) + wm_interm_19_pt3(a,q,p,i) * wm_interm_21_pt3(a,i)
term(88) = term(88) + wm_interm_19_pt3(a,q,p,i) * wm_interm_22_pt3(a,i)
term(89) = term(89) + wm_interm_19_pt3(a,q,p,i) * wm_interm_23_pt3(a,i)
term(90) = term(90) + wm_interm_66_pt3(a,i) * wm_interm_71_pt3(a,q,p,i)
term(91) = term(91) + wm_interm_68_pt3(a,i) * wm_interm_71_pt3(a,q,p,i)
term(92) = term(92) + wm_interm_69_pt3(a,i) * wm_interm_71_pt3(a,q,p,i)
term(93) = term(93) + wm_interm_70_pt3(a,i) * wm_interm_71_pt3(a,q,p,i)
end do 
end do 

term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (-2.0d+0) 
term(90) = term(90) * (3.9999999999999996d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-1.9999999999999998d+0) 
term(93) = term(93) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(94) = term(94) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_80_pt3(c,a,i,p)
term(95) = term(95) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_81_pt3(c,a,i,p)
term(96) = term(96) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_78_pt3(c,a,i,p)
term(97) = term(97) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_77_pt3(c,a,i,p)
term(98) = term(98) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_109_pt3(b,c,p,k)
term(99) = term(99) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_110_pt3(b,c,p,k)
term(100) = term(100) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_109_pt3(b,c,p,k)
term(101) = term(101) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_110_pt3(b,c,p,k)
term(102) = term(102) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_109_pt3(a,c,p,k)
term(103) = term(103) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_110_pt3(a,c,p,k)
term(104) = term(104) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_109_pt3(a,c,p,k)
term(105) = term(105) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_110_pt3(a,c,p,k)
term(106) = term(106) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_110_pt3(b,c,p,i)
term(107) = term(107) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_109_pt3(b,c,p,i)
term(108) = term(108) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_110_pt3(a,c,p,i)
term(109) = term(109) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_109_pt3(a,c,p,i)
term(110) = term(110) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_111_pt3(b,c,p,i)
term(111) = term(111) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_112_pt3(b,c,p,i)
term(112) = term(112) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_111_pt3(b,c,p,k)
term(113) = term(113) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_112_pt3(b,c,p,k)
term(114) = term(114) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_111_pt3(b,c,p,k)
term(115) = term(115) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_112_pt3(b,c,p,k)
term(116) = term(116) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_111_pt3(a,c,p,i)
term(117) = term(117) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_112_pt3(a,c,p,i)
term(118) = term(118) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_111_pt3(a,c,p,k)
term(119) = term(119) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_112_pt3(a,c,p,k)
term(120) = term(120) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_111_pt3(a,c,p,k)
term(121) = term(121) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_112_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (-4.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * (4.0d+0) 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * (4.0d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (4.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (-1.9999999999999998d+0) 
term(111) = term(111) * (3.9999999999999996d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (4.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(117) = term(117) * (-1.9999999999999998d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(122) = term(122) + wm_interm_41_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,p,i)
term(123) = term(123) + wm_interm_41_pt3(q,i,j,k) * wm_interm_79_pt3(k,j,p,i)
term(124) = term(124) + wm_interm_40_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,p,i)
term(125) = term(125) + wm_interm_38_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,p,i)
term(126) = term(126) + wm_interm_47_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,p,i)
term(127) = term(127) + wm_interm_47_pt3(q,i,j,k) * wm_interm_79_pt3(k,j,p,i)
end do 
end do 
end do 

term(123) = term(123) * (-2.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(126) = term(126) * (-1.9999999999999998d+0) 
term(127) = term(127) * (3.9999999999999996d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(128) = term(128) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_77_pt3(c,a,k,p)
term(129) = term(129) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_78_pt3(c,a,k,p)
term(130) = term(130) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_80_pt3(c,a,k,p)
term(131) = term(131) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_81_pt3(c,a,k,p)
term(132) = term(132) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_77_pt3(c,a,k,p)
term(133) = term(133) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_78_pt3(c,a,k,p)
term(134) = term(134) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_80_pt3(c,a,k,p)
term(135) = term(135) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_81_pt3(c,a,k,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (8.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (8.0d+0) 
term(135) = term(135) * (-16.0d+0) 

do i = 1, nocc 
term(136) = term(136) + wm_interm_35_pt3(q,i) * wm_interm_4_pt3(i,p)
term(137) = term(137) + wm_interm_34_pt3(q,i) * wm_interm_4_pt3(i,p)
term(138) = term(138) + wm_interm_39_pt3(q,i) * wm_interm_4_pt3(i,p)
term(139) = term(139) + wm_interm_37_pt3(q,i) * wm_interm_4_pt3(i,p)
term(140) = term(140) + wm_interm_59_pt3(i,p) * wm_interm_66_pt3(q,i)
term(141) = term(141) + wm_interm_59_pt3(i,p) * wm_interm_68_pt3(q,i)
term(142) = term(142) + wm_interm_59_pt3(i,p) * wm_interm_69_pt3(q,i)
term(143) = term(143) + wm_interm_59_pt3(i,p) * wm_interm_70_pt3(q,i)
term(144) = term(144) + wm_interm_35_pt3(q,i) * wm_interm_82_pt3(i,p)
term(145) = term(145) + wm_interm_35_pt3(q,i) * wm_interm_83_pt3(i,p)
term(146) = term(146) + wm_interm_34_pt3(q,i) * wm_interm_82_pt3(i,p)
term(147) = term(147) + wm_interm_34_pt3(q,i) * wm_interm_83_pt3(i,p)
term(148) = term(148) + wm_interm_39_pt3(q,i) * wm_interm_82_pt3(i,p)
term(149) = term(149) + wm_interm_39_pt3(q,i) * wm_interm_83_pt3(i,p)
term(150) = term(150) + wm_interm_37_pt3(q,i) * wm_interm_82_pt3(i,p)
term(151) = term(151) + wm_interm_37_pt3(q,i) * wm_interm_83_pt3(i,p)
term(152) = term(152) + wm_interm_115_pt3(q,i) * wm_interm_126_pt3(p,i)
term(153) = term(153) + wm_interm_116_pt3(q,i) * wm_interm_126_pt3(p,i)
term(154) = term(154) + wm_interm_115_pt3(q,i) * wm_interm_127_pt3(p,i)
term(155) = term(155) + wm_interm_116_pt3(q,i) * wm_interm_127_pt3(p,i)
term(156) = term(156) + wm_interm_113_pt3(q,i) * wm_interm_126_pt3(p,i)
term(157) = term(157) + wm_interm_108_pt3(q,i) * wm_interm_126_pt3(p,i)
term(158) = term(158) + wm_interm_113_pt3(q,i) * wm_interm_127_pt3(p,i)
term(159) = term(159) + wm_interm_108_pt3(q,i) * wm_interm_127_pt3(p,i)
end do 

term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * (1.9999999999999998d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (-3.9999999999999996d+0) 
term(140) = term(140) * (3.9999999999999996d+0) 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (-1.9999999999999998d+0) 
term(143) = term(143) * (2.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (-1.9999999999999998d+0) 
term(147) = term(147) * (3.9999999999999996d+0) 
term(148) = term(148) * (-4.0d+0) 
term(149) = term(149) * (8.0d+0) 
term(150) = term(150) * (3.9999999999999996d+0) 
term(151) = term(151) * (-7.999999999999999d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (-1.9999999999999998d+0) 
term(157) = term(157) * (3.9999999999999996d+0) 
term(158) = term(158) * (3.9999999999999996d+0) 
term(159) = term(159) * (-7.999999999999999d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(160) = term(160) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,i,j,k,p,l)
term(161) = term(161) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,i,j,p,k,l)
term(162) = term(162) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,i,j,k,l,p)
term(163) = term(163) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,j,i,p,k,l)
term(164) = term(164) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,j,i,k,p,l)
term(165) = term(165) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,j,i,k,l,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(161) = term(161) * (-2.0d+0) 
term(162) = term(162) * (-1.9999999999999998d+0) 
term(164) = term(164) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(166) = term(166) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_26_pt3(a,b,i,j)
term(167) = term(167) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_27_pt3(a,b,i,j)
term(168) = term(168) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_28_pt3(a,b,i,j)
term(169) = term(169) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_29_pt3(a,b,i,j)
term(170) = term(170) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_72_pt3(a,b,i,j)
term(171) = term(171) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_73_pt3(a,b,i,j)
term(172) = term(172) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_75_pt3(a,b,i,j)
term(173) = term(173) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_74_pt3(a,b,i,j)
term(174) = term(174) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_76_pt3(a,b,i,j)
end do 
end do 
end do 
end do 

term(166) = term(166) * (8.0d+0) 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * (-2.0d+0) 
term(170) = term(170) * (-4.0d+0) 
term(171) = term(171) * (1.3333333333333333d+0) 
term(172) = term(172) * (-0.3333333333333333d+0) 
term(173) = term(173) * (1.3333333333333333d+0) 
term(174) = term(174) * (-0.3333333333333333d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(175) = term(175) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,i,j,l,k,p)
term(176) = term(176) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,i,j,l,p,k)
term(177) = term(177) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,i,j,p,l,k)
term(178) = term(178) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,j,i,l,k,p)
term(179) = term(179) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,j,i,p,l,k)
term(180) = term(180) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_86_pt3(b,j,i,l,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (-1.9999999999999998d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(181) = term(181) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_72_pt3(b,a,i,j)
term(182) = term(182) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_73_pt3(b,a,i,j)
term(183) = term(183) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_74_pt3(b,a,i,j)
term(184) = term(184) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_75_pt3(b,a,i,j)
term(185) = term(185) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_76_pt3(b,a,i,j)
end do 
end do 
end do 
end do 

term(181) = term(181) * (2.0d+0) 
term(182) = term(182) * (-0.6666666666666666d+0) 
term(183) = term(183) * (-0.6666666666666666d+0) 
term(184) = term(184) * (0.6666666666666666d+0) 
term(185) = term(185) * (0.6666666666666666d+0) 

do a = nocc + 1, nactive 
term(186) = term(186) + wm_interm_20_pt3(a,p) * wm_interm_30_pt3(a,q)
term(187) = term(187) + wm_interm_21_pt3(a,p) * wm_interm_30_pt3(a,q)
term(188) = term(188) + wm_interm_22_pt3(a,p) * wm_interm_30_pt3(a,q)
term(189) = term(189) + wm_interm_23_pt3(a,p) * wm_interm_30_pt3(a,q)
term(190) = term(190) + wm_interm_89_pt3(a,p) * wm_interm_99_pt3(q,a)
term(191) = term(191) + wm_interm_90_pt3(a,p) * wm_interm_99_pt3(q,a)
term(192) = term(192) + wm_interm_91_pt3(a,p) * wm_interm_99_pt3(q,a)
term(193) = term(193) + wm_interm_92_pt3(a,p) * wm_interm_99_pt3(q,a)
term(194) = term(194) + wm_interm_93_pt3(a,p) * wm_interm_99_pt3(q,a)
term(195) = term(195) + wm_interm_94_pt3(a,p) * wm_interm_99_pt3(q,a)
term(196) = term(196) + wm_interm_100_pt3(q,a) * wm_interm_89_pt3(a,p)
term(197) = term(197) + wm_interm_100_pt3(q,a) * wm_interm_90_pt3(a,p)
term(198) = term(198) + wm_interm_100_pt3(q,a) * wm_interm_91_pt3(a,p)
term(199) = term(199) + wm_interm_100_pt3(q,a) * wm_interm_92_pt3(a,p)
term(200) = term(200) + wm_interm_100_pt3(q,a) * wm_interm_93_pt3(a,p)
term(201) = term(201) + wm_interm_100_pt3(q,a) * wm_interm_94_pt3(a,p)
end do 

term(186) = term(186) * (-4.0d+0) 
term(187) = term(187) * (4.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-2.0d+0) 
term(190) = term(190) * (-1.9999999999999998d+0) 
term(191) = term(191) * (3.9999999999999996d+0) 
term(192) = term(192) * (-1.9999999999999998d+0) 
term(193) = term(193) * (3.9999999999999996d+0) 
term(194) = term(194) * (3.9999999999999996d+0) 
term(195) = term(195) * (-7.999999999999999d+0) 
term(197) = term(197) * (-1.9999999999999998d+0) 
term(199) = term(199) * (-1.9999999999999998d+0) 
term(200) = term(200) * (-1.9999999999999998d+0) 
term(201) = term(201) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(202) = term(202) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_80_pt3(c,a,k,p)
term(203) = term(203) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_81_pt3(c,a,k,p)
term(204) = term(204) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_78_pt3(c,a,k,p)
term(205) = term(205) + s2(a,b,j,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_77_pt3(c,a,k,p)
term(206) = term(206) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_109_pt3(b,c,p,k)
term(207) = term(207) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_110_pt3(b,c,p,k)
term(208) = term(208) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_110_pt3(a,c,p,k)
term(209) = term(209) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_109_pt3(a,c,p,k)
term(210) = term(210) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_80_pt3(c,a,i,p)
term(211) = term(211) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_81_pt3(c,a,i,p)
term(212) = term(212) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_78_pt3(c,a,i,p)
term(213) = term(213) + s2(a,b,j,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_77_pt3(c,a,i,p)
term(214) = term(214) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_110_pt3(b,c,p,i)
term(215) = term(215) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_109_pt3(b,c,p,i)
term(216) = term(216) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_110_pt3(a,c,p,i)
term(217) = term(217) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_109_pt3(a,c,p,i)
term(218) = term(218) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_111_pt3(b,c,p,i)
term(219) = term(219) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_112_pt3(b,c,p,i)
term(220) = term(220) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_111_pt3(b,c,p,k)
term(221) = term(221) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_112_pt3(b,c,p,k)
term(222) = term(222) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_111_pt3(a,c,p,k)
term(223) = term(223) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_112_pt3(a,c,p,k)
term(224) = term(224) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_111_pt3(a,c,p,i)
term(225) = term(225) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_112_pt3(a,c,p,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(202) = term(202) * (2.0d+0) 
term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * (2.0d+0) 
term(205) = term(205) * (-4.0d+0) 
term(207) = term(207) * (-2.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (8.0d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (8.0d+0) 
term(215) = term(215) * (-2.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(219) = term(219) * (-1.9999999999999998d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(223) = term(223) * (-2.0d+0) 
term(224) = term(224) * (-1.9999999999999998d+0) 
term(225) = term(225) * (3.9999999999999996d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(226) = term(226) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_72_pt3(b,a,j,i)
term(227) = term(227) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_73_pt3(b,a,j,i)
term(228) = term(228) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_74_pt3(b,a,j,i)
term(229) = term(229) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_75_pt3(b,a,j,i)
term(230) = term(230) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_76_pt3(b,a,j,i)
end do 
end do 
end do 
end do 

term(226) = term(226) * (-4.0d+0) 
term(227) = term(227) * (1.3333333333333333d+0) 
term(228) = term(228) * (1.3333333333333333d+0) 
term(229) = term(229) * (-0.3333333333333333d+0) 
term(230) = term(230) * (-0.3333333333333333d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(231) = term(231) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,k,l,p,i,j)
term(232) = term(232) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,l,k,p,i,j)
term(233) = term(233) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,p,k,l,i,j)
term(234) = term(234) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,k,p,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(232) = term(232) * (-2.0d+0) 
term(234) = term(234) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(235) = term(235) + wm_interm_41_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,i,p)
term(236) = term(236) + wm_interm_41_pt3(q,i,j,k) * wm_interm_79_pt3(k,j,i,p)
term(237) = term(237) + wm_interm_40_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,i,p)
term(238) = term(238) + wm_interm_38_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,i,p)
term(239) = term(239) + wm_interm_47_pt3(q,i,j,k) * wm_interm_79_pt3(j,k,i,p)
term(240) = term(240) + wm_interm_47_pt3(q,i,j,k) * wm_interm_79_pt3(k,j,i,p)
term(241) = term(241) + wm_interm_117_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,j,k)
term(242) = term(242) + wm_interm_117_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,k,j)
term(243) = term(243) + wm_interm_114_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,k,j)
term(244) = term(244) + wm_interm_114_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,j,k)
term(245) = term(245) + wm_interm_106_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,k,j)
term(246) = term(246) + wm_interm_107_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,k,j)
term(247) = term(247) + wm_interm_106_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,j,k)
term(248) = term(248) + wm_interm_107_pt3(q,i,j,k) * wm_interm_125_pt3(i,p,j,k)
end do 
end do 
end do 

term(235) = term(235) * (-2.0d+0) 
term(238) = term(238) * (-2.0d+0) 
term(239) = term(239) * (3.9999999999999996d+0) 
term(240) = term(240) * (-1.9999999999999998d+0) 
term(242) = term(242) * (-1.9999999999999998d+0) 
term(244) = term(244) * (-1.9999999999999998d+0) 
term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(249) = term(249) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_26_pt3(a,b,j,i)
term(250) = term(250) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_27_pt3(a,b,j,i)
term(251) = term(251) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_28_pt3(a,b,j,i)
term(252) = term(252) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_29_pt3(a,b,j,i)
term(253) = term(253) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_72_pt3(a,b,j,i)
term(254) = term(254) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_73_pt3(a,b,j,i)
term(255) = term(255) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_75_pt3(a,b,j,i)
term(256) = term(256) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_74_pt3(a,b,j,i)
term(257) = term(257) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_76_pt3(a,b,j,i)
end do 
end do 
end do 
end do 

term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (-1.9999999999999998d+0) 
term(251) = term(251) * (2.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (-0.6666666666666666d+0) 
term(255) = term(255) * (0.6666666666666666d+0) 
term(256) = term(256) * (-0.6666666666666666d+0) 
term(257) = term(257) * (0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(258) = term(258) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,p,l,k,i,j)
term(259) = term(259) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,l,p,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(258) = term(258) * (-2.0d+0) 
term(259) = term(259) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(260) = term(260) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_110_pt3(b,c,p,k)
term(261) = term(261) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_109_pt3(b,c,p,k)
term(262) = term(262) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_109_pt3(a,c,p,k)
term(263) = term(263) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_110_pt3(a,c,p,k)
term(264) = term(264) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_80_pt3(c,a,k,p)
term(265) = term(265) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_81_pt3(c,a,k,p)
term(266) = term(266) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_77_pt3(c,a,k,p)
term(267) = term(267) + s2(a,b,i,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_78_pt3(c,a,k,p)
term(268) = term(268) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_111_pt3(b,c,p,k)
term(269) = term(269) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_112_pt3(b,c,p,k)
term(270) = term(270) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_111_pt3(a,c,p,k)
term(271) = term(271) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_112_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(261) = term(261) * (-2.0d+0) 
term(263) = term(263) * (-2.0d+0) 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * (8.0d+0) 
term(266) = term(266) * (2.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(269) = term(269) * (-2.0d+0) 
term(270) = term(270) * (-2.0d+0) 
term(271) = term(271) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(272) = term(272) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,k,p,l,j,i)
term(273) = term(273) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,p,k,l,j,i)
term(274) = term(274) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,k,l,p,j,i)
term(275) = term(275) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,l,k,p,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(273) = term(273) * (-2.0d+0) 
term(274) = term(274) * (-2.0d+0) 
term(275) = term(275) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(276) = term(276) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,l,p,k,j,i)
term(277) = term(277) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,l) * wm_interm_101_pt3(b,p,l,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(276) = term(276) * (-2.0d+0) 


    calc_D_ov_wm_cc3_pt3 = zero
    do s = 0, 277
    calc_D_ov_wm_cc3_pt3 = calc_D_ov_wm_cc3_pt3 + term(s)
    end do

    end function calc_D_ov_wm_cc3_pt3
    
    function calc_D_vo_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b, c, l 
    real(F64), dimension(0:847) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_5_pt3(a,q,i,j) * wm_interm_6_pt3(p,a,j,i)
term(1) = term(1) + wm_interm_5_pt3(a,q,i,j) * wm_interm_7_pt3(p,a,j,i)
term(2) = term(2) + wm_interm_5_pt3(a,q,i,j) * wm_interm_8_pt3(p,a,i,j)
term(3) = term(3) + wm_interm_5_pt3(a,q,i,j) * wm_interm_8_pt3(p,a,j,i)
term(4) = term(4) + wm_interm_5_pt3(a,q,i,j) * wm_interm_6_pt3(p,a,i,j)
term(5) = term(5) + wm_interm_5_pt3(a,q,i,j) * wm_interm_7_pt3(p,a,i,j)
term(6) = term(6) + wm_interm_17_pt3(a,i,j,q) * wm_interm_19_pt3(p,a,i,j)
term(7) = term(7) + wm_interm_18_pt3(a,i,j,q) * wm_interm_19_pt3(p,a,i,j)
term(8) = term(8) + wm_interm_13_pt3(a,i,j,q) * wm_interm_19_pt3(p,a,i,j)
term(9) = term(9) + wm_interm_16_pt3(a,i,j,q) * wm_interm_19_pt3(p,a,i,j)
term(10) = term(10) + wm_interm_15_pt3(a,i,j,q) * wm_interm_19_pt3(p,a,i,j)
term(11) = term(11) + wm_interm_14_pt3(a,i,j,q) * wm_interm_19_pt3(p,a,i,j)
term(12) = term(12) + wm_interm_13_pt3(a,i,j,q) * wm_interm_19_pt3(a,p,i,j)
term(13) = term(13) + wm_interm_16_pt3(a,i,j,q) * wm_interm_19_pt3(a,p,i,j)
term(14) = term(14) + wm_interm_15_pt3(a,i,j,q) * wm_interm_19_pt3(a,p,i,j)
term(15) = term(15) + wm_interm_14_pt3(a,i,j,q) * wm_interm_19_pt3(a,p,i,j)
term(16) = term(16) + wm_interm_11_pt3(a,i,j,q) * wm_interm_25_pt3(p,a,i,j)
term(17) = term(17) + wm_interm_11_pt3(a,i,j,q) * wm_interm_24_pt3(p,a,i,j)
term(18) = term(18) + wm_interm_11_pt3(a,i,j,q) * wm_interm_24_pt3(a,p,i,j)
term(19) = term(19) + wm_interm_11_pt3(a,i,j,q) * wm_interm_25_pt3(a,p,i,j)
term(20) = term(20) + wm_interm_0_pt3(a,p,i,j) * wm_interm_32_pt3(a,j,q,i)
term(21) = term(21) + wm_interm_0_pt3(a,p,i,j) * wm_interm_32_pt3(a,j,i,q)
term(22) = term(22) + wm_interm_0_pt3(a,p,i,j) * wm_interm_36_pt3(a,j,q,i)
term(23) = term(23) + wm_interm_0_pt3(a,p,i,j) * wm_interm_38_pt3(a,j,i,q)
term(24) = term(24) + wm_interm_0_pt3(a,p,i,j) * wm_interm_40_pt3(a,j,i,q)
term(25) = term(25) + wm_interm_0_pt3(a,p,i,j) * wm_interm_36_pt3(a,j,i,q)
term(26) = term(26) + wm_interm_58_pt3(a,q,i,j) * wm_interm_60_pt3(a,p,i,j)
term(27) = term(27) + wm_interm_58_pt3(a,i,q,j) * wm_interm_60_pt3(a,p,i,j)
term(28) = term(28) + wm_interm_60_pt3(a,p,i,j) * wm_interm_61_pt3(a,i,q,j)
term(29) = term(29) + wm_interm_60_pt3(a,p,i,j) * wm_interm_62_pt3(a,q,i,j)
term(30) = term(30) + wm_interm_60_pt3(a,p,i,j) * wm_interm_61_pt3(a,q,i,j)
term(31) = term(31) + wm_interm_60_pt3(a,p,i,j) * wm_interm_62_pt3(a,i,q,j)
term(32) = term(32) + wm_interm_60_pt3(a,p,i,j) * wm_interm_63_pt3(a,q,i,j)
term(33) = term(33) + wm_interm_60_pt3(a,p,i,j) * wm_interm_63_pt3(a,i,q,j)
term(34) = term(34) + wm_interm_60_pt3(a,p,i,j) * wm_interm_64_pt3(a,i,q,j)
term(35) = term(35) + wm_interm_60_pt3(a,p,i,j) * wm_interm_64_pt3(a,q,i,j)
term(36) = term(36) + wm_interm_60_pt3(a,p,i,j) * wm_interm_65_pt3(a,i,q,j)
term(37) = term(37) + wm_interm_60_pt3(a,p,i,j) * wm_interm_65_pt3(a,q,i,j)
term(38) = term(38) + wm_interm_61_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,i,j)
term(39) = term(39) + wm_interm_64_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,i,j)
term(40) = term(40) + wm_interm_58_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,i,j)
term(41) = term(41) + wm_interm_65_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,i,j)
term(42) = term(42) + wm_interm_62_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,i,j)
term(43) = term(43) + wm_interm_63_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,i,j)
term(44) = term(44) + wm_interm_61_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,i,j)
term(45) = term(45) + wm_interm_58_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,i,j)
term(46) = term(46) + wm_interm_64_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,i,j)
term(47) = term(47) + wm_interm_62_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,i,j)
term(48) = term(48) + wm_interm_63_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,i,j)
term(49) = term(49) + wm_interm_65_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,i,j)
term(50) = term(50) + wm_interm_32_pt3(a,i,q,j) * wm_interm_80_pt3(a,p,j,i)
term(51) = term(51) + wm_interm_32_pt3(a,i,q,j) * wm_interm_81_pt3(a,p,j,i)
term(52) = term(52) + wm_interm_32_pt3(a,i,q,j) * wm_interm_78_pt3(a,p,j,i)
term(53) = term(53) + wm_interm_32_pt3(a,i,q,j) * wm_interm_77_pt3(a,p,j,i)
term(54) = term(54) + wm_interm_106_pt3(a,i,q,j) * wm_interm_109_pt3(p,a,i,j)
term(55) = term(55) + wm_interm_107_pt3(a,i,q,j) * wm_interm_109_pt3(p,a,i,j)
term(56) = term(56) + wm_interm_106_pt3(a,i,q,j) * wm_interm_110_pt3(p,a,i,j)
term(57) = term(57) + wm_interm_107_pt3(a,i,q,j) * wm_interm_110_pt3(p,a,i,j)
term(58) = term(58) + wm_interm_36_pt3(a,i,q,j) * wm_interm_80_pt3(a,p,j,i)
term(59) = term(59) + wm_interm_36_pt3(a,i,q,j) * wm_interm_81_pt3(a,p,j,i)
term(60) = term(60) + wm_interm_36_pt3(a,i,q,j) * wm_interm_78_pt3(a,p,j,i)
term(61) = term(61) + wm_interm_36_pt3(a,i,q,j) * wm_interm_77_pt3(a,p,j,i)
term(62) = term(62) + wm_interm_110_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,q,j)
term(63) = term(63) + wm_interm_109_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,q,j)
term(64) = term(64) + wm_interm_109_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,j,q)
term(65) = term(65) + wm_interm_110_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,j,q)
term(66) = term(66) + wm_interm_109_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,q,j)
term(67) = term(67) + wm_interm_110_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,q,j)
term(68) = term(68) + wm_interm_110_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,j,q)
term(69) = term(69) + wm_interm_109_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,j,q)
term(70) = term(70) + wm_interm_111_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,j,q)
term(71) = term(71) + wm_interm_111_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,q,j)
term(72) = term(72) + wm_interm_111_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,j,q)
term(73) = term(73) + wm_interm_106_pt3(a,i,q,j) * wm_interm_111_pt3(p,a,i,j)
term(74) = term(74) + wm_interm_107_pt3(a,i,q,j) * wm_interm_111_pt3(p,a,i,j)
term(75) = term(75) + wm_interm_111_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,q,j)
term(76) = term(76) + wm_interm_112_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,j,q)
term(77) = term(77) + wm_interm_112_pt3(p,a,i,j) * wm_interm_117_pt3(a,i,q,j)
term(78) = term(78) + wm_interm_112_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,j,q)
term(79) = term(79) + wm_interm_106_pt3(a,i,q,j) * wm_interm_112_pt3(p,a,i,j)
term(80) = term(80) + wm_interm_107_pt3(a,i,q,j) * wm_interm_112_pt3(p,a,i,j)
term(81) = term(81) + wm_interm_112_pt3(p,a,i,j) * wm_interm_114_pt3(a,i,q,j)
term(82) = term(82) + wm_interm_106_pt3(a,i,j,q) * wm_interm_111_pt3(p,a,i,j)
term(83) = term(83) + wm_interm_107_pt3(a,i,j,q) * wm_interm_111_pt3(p,a,i,j)
term(84) = term(84) + wm_interm_106_pt3(a,i,j,q) * wm_interm_112_pt3(p,a,i,j)
term(85) = term(85) + wm_interm_107_pt3(a,i,j,q) * wm_interm_112_pt3(p,a,i,j)
term(86) = term(86) + wm_interm_106_pt3(a,i,j,q) * wm_interm_110_pt3(p,a,i,j)
term(87) = term(87) + wm_interm_106_pt3(a,i,j,q) * wm_interm_109_pt3(p,a,i,j)
term(88) = term(88) + wm_interm_107_pt3(a,i,j,q) * wm_interm_110_pt3(p,a,i,j)
term(89) = term(89) + wm_interm_107_pt3(a,i,j,q) * wm_interm_109_pt3(p,a,i,j)
end do 
end do 
end do 

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-1.9999999999999998d+0) 
term(3) = term(3) * (3.9999999999999996d+0) 
term(4) = term(4) * (-8.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (8.0d+0) 
term(8) = term(8) * (1.9999999999999998d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-3.9999999999999996d+0) 
term(12) = term(12) * (-8.0d+0) 
term(13) = term(13) * (3.9999999999999996d+0) 
term(14) = term(14) * (-7.999999999999999d+0) 
term(15) = term(15) * (16.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-1.9999999999999998d+0) 
term(18) = term(18) * (3.9999999999999996d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (3.9999999999999996d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (-1.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (2.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-1.0d+0) 
term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (-3.9999999999999996d+0) 
term(59) = term(59) * (7.999999999999999d+0) 
term(60) = term(60) * (-3.9999999999999996d+0) 
term(61) = term(61) * (7.999999999999999d+0) 
term(63) = term(63) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(67) = term(67) * (-2.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (8.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-2.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (8.0d+0) 
term(80) = term(80) * (-16.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (1.9999999999999998d+0) 
term(83) = term(83) * (-3.9999999999999996d+0) 
term(84) = term(84) * (-3.9999999999999996d+0) 
term(85) = term(85) * (7.999999999999999d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(90) = term(90) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,l,p,i) * wm_interm_119_pt3(a,i,l,k,j,q)
term(91) = term(91) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,l,p,i) * wm_interm_119_pt3(a,l,i,k,j,q)
term(92) = term(92) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,l,p,i) * wm_interm_119_pt3(b,i,l,k,j,q)
term(93) = term(93) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,l,p,i) * wm_interm_119_pt3(b,l,i,k,j,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(91) = term(91) * (-2.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (4.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(94) = term(94) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_pt3(b,a)
term(95) = term(95) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_pt3(b,a)
term(96) = term(96) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_pt3(b,a)
term(97) = term(97) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_pt3(b,a)
end do 
end do 
end do 
end do 
end do 

term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * (4.0d+0) 

do a = nocc + 1, nactive 
term(98) = term(98) + wm_interm_33_pt3(a,p) * wm_interm_34_pt3(a,q)
term(99) = term(99) + wm_interm_33_pt3(a,p) * wm_interm_35_pt3(a,q)
term(100) = term(100) + wm_interm_33_pt3(a,p) * wm_interm_37_pt3(a,q)
term(101) = term(101) + wm_interm_33_pt3(a,p) * wm_interm_39_pt3(a,q)
term(102) = term(102) + wm_interm_66_pt3(a,q) * wm_interm_67_pt3(a,p)
term(103) = term(103) + wm_interm_67_pt3(a,p) * wm_interm_68_pt3(a,q)
term(104) = term(104) + wm_interm_67_pt3(a,p) * wm_interm_69_pt3(a,q)
term(105) = term(105) + wm_interm_67_pt3(a,p) * wm_interm_70_pt3(a,q)
term(106) = term(106) + wm_interm_34_pt3(a,q) * wm_interm_95_pt3(a,p)
term(107) = term(107) + wm_interm_34_pt3(a,q) * wm_interm_96_pt3(a,p)
term(108) = term(108) + wm_interm_35_pt3(a,q) * wm_interm_95_pt3(a,p)
term(109) = term(109) + wm_interm_35_pt3(a,q) * wm_interm_96_pt3(a,p)
term(110) = term(110) + wm_interm_37_pt3(a,q) * wm_interm_95_pt3(a,p)
term(111) = term(111) + wm_interm_37_pt3(a,q) * wm_interm_96_pt3(a,p)
term(112) = term(112) + wm_interm_39_pt3(a,q) * wm_interm_95_pt3(a,p)
term(113) = term(113) + wm_interm_39_pt3(a,q) * wm_interm_96_pt3(a,p)
term(114) = term(114) + wm_interm_115_pt3(a,q) * wm_interm_120_pt3(p,a)
term(115) = term(115) + wm_interm_116_pt3(a,q) * wm_interm_120_pt3(p,a)
term(116) = term(116) + wm_interm_115_pt3(a,q) * wm_interm_121_pt3(p,a)
term(117) = term(117) + wm_interm_116_pt3(a,q) * wm_interm_121_pt3(p,a)
term(118) = term(118) + wm_interm_113_pt3(a,q) * wm_interm_120_pt3(p,a)
term(119) = term(119) + wm_interm_108_pt3(a,q) * wm_interm_120_pt3(p,a)
term(120) = term(120) + wm_interm_113_pt3(a,q) * wm_interm_121_pt3(p,a)
term(121) = term(121) + wm_interm_108_pt3(a,q) * wm_interm_121_pt3(p,a)
end do 

term(98) = term(98) * (1.9999999999999998d+0) 
term(99) = term(99) * (-1.9999999999999998d+0) 
term(100) = term(100) * (-3.9999999999999996d+0) 
term(101) = term(101) * (4.0d+0) 
term(102) = term(102) * (3.9999999999999996d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-1.9999999999999998d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (-1.9999999999999998d+0) 
term(107) = term(107) * (3.9999999999999996d+0) 
term(108) = term(108) * (1.9999999999999996d+0) 
term(109) = term(109) * (-3.999999999999999d+0) 
term(110) = term(110) * (3.9999999999999996d+0) 
term(111) = term(111) * (-7.999999999999999d+0) 
term(112) = term(112) * (-3.9999999999999996d+0) 
term(113) = term(113) * (7.999999999999999d+0) 
term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (-1.9999999999999998d+0) 
term(119) = term(119) * (3.9999999999999996d+0) 
term(120) = term(120) * (3.9999999999999996d+0) 
term(121) = term(121) * (-7.999999999999999d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(122) = term(122) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,i,k,q) * wm_interm_77_pt3(c,a,k,j)
term(123) = term(123) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,i,k,q) * wm_interm_78_pt3(c,a,k,j)
term(124) = term(124) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,i,k,q) * wm_interm_80_pt3(c,a,k,j)
term(125) = term(125) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,i,k,q) * wm_interm_81_pt3(c,a,k,j)
term(126) = term(126) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_77_pt3(c,b,i,j)
term(127) = term(127) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_78_pt3(c,b,i,j)
term(128) = term(128) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_80_pt3(c,b,i,j)
term(129) = term(129) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_pt3(c,b,i,j)
term(130) = term(130) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_77_pt3(c,b,k,j)
term(131) = term(131) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_78_pt3(c,b,k,j)
term(132) = term(132) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_80_pt3(c,b,k,j)
term(133) = term(133) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_pt3(c,b,k,j)
term(134) = term(134) + s2(a,b,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_77_pt3(c,b,k,j)
term(135) = term(135) + s2(a,b,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_78_pt3(c,b,k,j)
term(136) = term(136) + s2(a,b,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_80_pt3(c,b,k,j)
term(137) = term(137) + s2(a,b,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_pt3(c,b,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * (8.0d+0) 
term(124) = term(124) * (8.0d+0) 
term(125) = term(125) * (-16.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * (8.0d+0) 
term(130) = term(130) * (2.0d+0) 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-3.9999999999999996d+0) 
term(135) = term(135) * (7.999999999999999d+0) 
term(136) = term(136) * (7.999999999999999d+0) 
term(137) = term(137) * (-15.999999999999998d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(138) = term(138) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,i,q) * wm_interm_80_pt3(c,a,k,j)
term(139) = term(139) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,i,q) * wm_interm_81_pt3(c,a,k,j)
term(140) = term(140) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,i,q) * wm_interm_78_pt3(c,a,k,j)
term(141) = term(141) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,i,q) * wm_interm_77_pt3(c,a,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * (8.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(142) = term(142) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,q) * wm_interm_77_pt3(c,a,k,i)
term(143) = term(143) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,q) * wm_interm_78_pt3(c,a,k,i)
term(144) = term(144) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,q) * wm_interm_80_pt3(c,a,k,i)
term(145) = term(145) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,q) * wm_interm_81_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(142) = term(142) * (8.0d+0) 
term(143) = term(143) * (-16.0d+0) 
term(144) = term(144) * (-16.0d+0) 
term(145) = term(145) * (32.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(146) = term(146) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,k) * wm_interm_77_pt3(c,a,q,j)
term(147) = term(147) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,k) * wm_interm_78_pt3(c,a,q,j)
term(148) = term(148) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,k) * wm_interm_78_pt3(c,a,q,i)
term(149) = term(149) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,k) * wm_interm_77_pt3(c,a,q,i)
term(150) = term(150) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,j) * wm_interm_78_pt3(c,a,q,k)
term(151) = term(151) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,j) * wm_interm_77_pt3(c,a,q,k)
term(152) = term(152) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,i) * wm_interm_77_pt3(c,a,q,k)
term(153) = term(153) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,i) * wm_interm_78_pt3(c,a,q,k)
term(154) = term(154) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,k) * wm_interm_80_pt3(c,a,q,j)
term(155) = term(155) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,k) * wm_interm_81_pt3(c,a,q,j)
term(156) = term(156) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,k) * wm_interm_80_pt3(c,a,q,i)
term(157) = term(157) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,k) * wm_interm_81_pt3(c,a,q,i)
term(158) = term(158) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,j) * wm_interm_80_pt3(c,a,q,k)
term(159) = term(159) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,i,j) * wm_interm_81_pt3(c,a,q,k)
term(160) = term(160) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,i,k) * wm_interm_80_pt3(c,a,k,j)
term(161) = term(161) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,i,k) * wm_interm_81_pt3(c,a,k,j)
term(162) = term(162) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,j,k) * wm_interm_80_pt3(c,a,k,i)
term(163) = term(163) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,j,k) * wm_interm_81_pt3(c,a,k,i)
term(164) = term(164) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,i) * wm_interm_80_pt3(c,a,q,k)
term(165) = term(165) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,j,i) * wm_interm_81_pt3(c,a,q,k)
term(166) = term(166) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,i,k) * wm_interm_77_pt3(c,a,k,j)
term(167) = term(167) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,i,k) * wm_interm_78_pt3(c,a,k,j)
term(168) = term(168) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,j,k) * wm_interm_77_pt3(c,a,k,i)
term(169) = term(169) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,j,k) * wm_interm_78_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(146) = term(146) * (2.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (1.9999999999999998d+0) 
term(151) = term(151) * (-3.9999999999999996d+0) 
term(152) = term(152) * (1.9999999999999998d+0) 
term(153) = term(153) * (-3.9999999999999996d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (1.9999999999999998d+0) 
term(159) = term(159) * (-3.9999999999999996d+0) 
term(160) = term(160) * (8.0d+0) 
term(161) = term(161) * (-16.0d+0) 
term(162) = term(162) * (-4.0d+0) 
term(163) = term(163) * (8.0d+0) 
term(164) = term(164) * (-3.9999999999999996d+0) 
term(165) = term(165) * (7.999999999999999d+0) 
term(166) = term(166) * (-4.0d+0) 
term(167) = term(167) * (8.0d+0) 
term(168) = term(168) * (2.0d+0) 
term(169) = term(169) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(170) = term(170) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,k,j) * wm_interm_5_pt3(b,i,k,j)
term(171) = term(171) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,k,j) * wm_interm_5_pt3(b,i,j,k)
term(172) = term(172) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,k,j) * wm_interm_88_pt3(b,i,k,j)
term(173) = term(173) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,k,j) * wm_interm_84_pt3(b,i,k,j)
term(174) = term(174) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,k,j) * wm_interm_85_pt3(b,i,k,j)
term(175) = term(175) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_106_pt3(b,i,k,j)
term(176) = term(176) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_107_pt3(b,i,k,j)
term(177) = term(177) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_114_pt3(b,i,k,j)
term(178) = term(178) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_117_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(170) = term(170) * (-2.0d+0) 
term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (7.999999999999999d+0) 
term(173) = term(173) * (8.0d+0) 
term(174) = term(174) * (-16.0d+0) 
term(175) = term(175) * (8.0d+0) 
term(176) = term(176) * (-16.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (-2.0d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(179) = term(179) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_5_pt3(b,i,j,k)
term(180) = term(180) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_5_pt3(b,i,k,j)
term(181) = term(181) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_44_pt3(b,i,j,k)
term(182) = term(182) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_44_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(179) = term(179) * (4.0d+0) 
term(180) = term(180) * (-8.0d+0) 
term(181) = term(181) * (4.0d+0) 
term(182) = term(182) * (-8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(183) = term(183) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,q,j) * wm_interm_5_pt3(b,i,k,j)
term(184) = term(184) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,q,j) * wm_interm_5_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(183) = term(183) * (3.9999999999999996d+0) 
term(184) = term(184) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(185) = term(185) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,k) * wm_interm_87_pt3(a,c,q,k)
term(186) = term(186) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,k) * wm_interm_97_pt3(a,c,q,k)
term(187) = term(187) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,k) * wm_interm_98_pt3(a,c,q,k)
term(188) = term(188) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,k) * wm_interm_102_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (2.0d+0) 
term(187) = term(187) * (-4.0d+0) 
term(188) = term(188) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(189) = term(189) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,k) * wm_interm_87_pt3(a,c,q,k)
term(190) = term(190) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,k) * wm_interm_97_pt3(a,c,q,k)
term(191) = term(191) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,k) * wm_interm_98_pt3(a,c,q,k)
term(192) = term(192) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,k) * wm_interm_102_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(189) = term(189) * (8.0d+0) 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (-4.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(193) = term(193) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_77_pt3(a,b,i,k)
term(194) = term(194) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_78_pt3(a,b,i,k)
term(195) = term(195) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_80_pt3(a,b,i,k)
term(196) = term(196) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_81_pt3(a,b,i,k)
term(197) = term(197) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_77_pt3(a,b,i,j)
term(198) = term(198) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_78_pt3(a,b,i,j)
term(199) = term(199) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_80_pt3(a,b,i,j)
term(200) = term(200) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_pt3(a,b,i,j)
term(201) = term(201) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_80_pt3(c,b,i,k)
term(202) = term(202) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_81_pt3(c,b,i,k)
term(203) = term(203) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_78_pt3(c,b,i,k)
term(204) = term(204) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_77_pt3(c,b,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(193) = term(193) * (2.0d+0) 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * (8.0d+0) 
term(199) = term(199) * (8.0d+0) 
term(200) = term(200) * (-16.0d+0) 
term(201) = term(201) * (2.0000000000000004d+0) 
term(202) = term(202) * (-4.000000000000001d+0) 
term(203) = term(203) * (2.0000000000000004d+0) 
term(204) = term(204) * (-4.000000000000001d+0) 

do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(205) = term(205) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,i,j) * wm_interm_87_pt3(a,c,q,k)
term(206) = term(206) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,i,j) * wm_interm_97_pt3(a,c,q,k)
term(207) = term(207) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,i,j) * wm_interm_98_pt3(a,c,q,k)
term(208) = term(208) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,i,j) * wm_interm_102_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(205) = term(205) * (-4.0d+0) 
term(206) = term(206) * (2.0d+0) 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(209) = term(209) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_87_pt3(a,c,q,k)
term(210) = term(210) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_97_pt3(a,c,q,k)
term(211) = term(211) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_98_pt3(a,c,q,k)
term(212) = term(212) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_102_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(209) = term(209) * (8.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (2.0d+0) 
term(212) = term(212) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(213) = term(213) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,q) * wm_interm_11_pt3(b,j,k,i)
term(214) = term(214) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,q) * wm_interm_11_pt3(b,k,j,i)
term(215) = term(215) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_1_pt3(b,j,k,i)
term(216) = term(216) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_1_pt3(b,k,j,i)
term(217) = term(217) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_2_pt3(b,k,j,i)
term(218) = term(218) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_2_pt3(b,j,k,i)
term(219) = term(219) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_3_pt3(b,j,k,i)
term(220) = term(220) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_3_pt3(b,k,j,i)
term(221) = term(221) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_1_pt3(b,j,k,i)
term(222) = term(222) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_1_pt3(b,k,j,i)
term(223) = term(223) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_2_pt3(b,k,j,i)
term(224) = term(224) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_2_pt3(b,j,k,i)
term(225) = term(225) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_3_pt3(b,j,k,i)
term(226) = term(226) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_3_pt3(b,k,j,i)
term(227) = term(227) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_32_pt3(b,i,j,k)
term(228) = term(228) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_36_pt3(b,i,j,k)
term(229) = term(229) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_40_pt3(b,i,j,k)
term(230) = term(230) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_38_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (-8.0d+0) 
term(215) = term(215) * (-4.0d+0) 
term(216) = term(216) * (8.0d+0) 
term(217) = term(217) * (-3.9999999999999996d+0) 
term(218) = term(218) * (7.999999999999999d+0) 
term(219) = term(219) * (8.0d+0) 
term(220) = term(220) * (-16.0d+0) 
term(221) = term(221) * (2.0d+0) 
term(222) = term(222) * (-4.0d+0) 
term(223) = term(223) * (1.9999999999999998d+0) 
term(224) = term(224) * (-3.9999999999999996d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (8.0d+0) 
term(228) = term(228) * (-2.0d+0) 
term(230) = term(230) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(231) = term(231) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_44_pt3(b,j,k,i)
term(232) = term(232) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_44_pt3(b,j,i,k)
term(233) = term(233) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_44_pt3(a,j,i,k)
term(234) = term(234) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_44_pt3(a,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(231) = term(231) * (-2.0d+0) 
term(232) = term(232) * (4.0d+0) 
term(233) = term(233) * (-2.0d+0) 
term(234) = term(234) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(235) = term(235) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_87_pt3(a,c,q,k)
term(236) = term(236) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_97_pt3(a,c,q,k)
term(237) = term(237) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_98_pt3(a,c,q,k)
term(238) = term(238) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_102_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(235) = term(235) * (-15.999999999999998d+0) 
term(236) = term(236) * (7.999999999999999d+0) 
term(237) = term(237) * (-3.9999999999999996d+0) 
term(238) = term(238) * (7.999999999999999d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(239) = term(239) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,k) * wm_interm_56_pt3(a,k,i,j)
term(240) = term(240) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,k) * wm_interm_56_pt3(a,k,j,i)
term(241) = term(241) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,k) * wm_interm_56_pt3(b,k,j,i)
term(242) = term(242) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,k) * wm_interm_56_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(239) = term(239) * (3.9999999999999996d+0) 
term(240) = term(240) * (-1.9999999999999998d+0) 
term(241) = term(241) * (3.9999999999999996d+0) 
term(242) = term(242) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(243) = term(243) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_11_pt3(a,k,i,j)
term(244) = term(244) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_11_pt3(b,k,i,j)
term(245) = term(245) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_11_pt3(b,i,k,j)
term(246) = term(246) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_11_pt3(a,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(243) = term(243) * (-2.0d+0) 
term(244) = term(244) * (4.0d+0) 
term(245) = term(245) * (-2.0d+0) 
term(246) = term(246) * (4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(247) = term(247) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,k) * wm_interm_56_pt3(a,k,i,j)
term(248) = term(248) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,k) * wm_interm_56_pt3(b,k,i,j)
term(249) = term(249) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_32_pt3(a,k,i,j)
term(250) = term(250) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_36_pt3(a,k,i,j)
term(251) = term(251) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_32_pt3(b,k,i,j)
term(252) = term(252) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_36_pt3(b,k,i,j)
term(253) = term(253) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_38_pt3(a,k,i,j)
term(254) = term(254) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_40_pt3(a,k,i,j)
term(255) = term(255) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_40_pt3(b,k,i,j)
term(256) = term(256) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_38_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (4.0d+0) 
term(250) = term(250) * (-8.0d+0) 
term(251) = term(251) * (-2.0d+0) 
term(252) = term(252) * (4.0d+0) 
term(253) = term(253) * (-2.0d+0) 
term(254) = term(254) * (4.0d+0) 
term(255) = term(255) * (-2.0d+0) 
term(256) = term(256) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(257) = term(257) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,i,k) * wm_interm_80_pt3(c,a,k,j)
term(258) = term(258) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,i,k) * wm_interm_81_pt3(c,a,k,j)
term(259) = term(259) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,i,k) * wm_interm_80_pt3(c,a,k,j)
term(260) = term(260) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,i,k) * wm_interm_81_pt3(c,a,k,j)
term(261) = term(261) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,i,k) * wm_interm_77_pt3(c,a,k,j)
term(262) = term(262) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,i,k) * wm_interm_78_pt3(c,a,k,j)
term(263) = term(263) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,i,k) * wm_interm_77_pt3(c,a,k,j)
term(264) = term(264) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,i,k) * wm_interm_78_pt3(c,a,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * (8.0d+0) 
term(259) = term(259) * (8.0d+0) 
term(260) = term(260) * (-16.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(265) = term(265) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,k) * wm_interm_56_pt3(b,k,j,i)
term(266) = term(266) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,k) * wm_interm_56_pt3(a,k,j,i)
term(267) = term(267) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,k,j) * wm_interm_88_pt3(b,i,k,j)
term(268) = term(268) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,k,j) * wm_interm_84_pt3(b,i,k,j)
term(269) = term(269) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,k,j) * wm_interm_85_pt3(b,i,k,j)
term(270) = term(270) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_106_pt3(b,i,k,j)
term(271) = term(271) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_107_pt3(b,i,k,j)
term(272) = term(272) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_32_pt3(b,k,j,i)
term(273) = term(273) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_36_pt3(b,k,j,i)
term(274) = term(274) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_32_pt3(b,i,k,j)
term(275) = term(275) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_32_pt3(a,k,j,i)
term(276) = term(276) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_36_pt3(b,i,k,j)
term(277) = term(277) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_36_pt3(a,k,j,i)
term(278) = term(278) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_114_pt3(b,i,k,j)
term(279) = term(279) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_117_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(265) = term(265) * (-4.0d+0) 
term(266) = term(266) * (2.0d+0) 
term(267) = term(267) * (-3.9999999999999996d+0) 
term(268) = term(268) * (-4.0d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (8.0d+0) 
term(272) = term(272) * (4.0d+0) 
term(273) = term(273) * (-8.0d+0) 
term(275) = term(275) * (-2.0d+0) 
term(276) = term(276) * (-2.0d+0) 
term(277) = term(277) * (4.0d+0) 
term(278) = term(278) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(280) = term(280) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,i,j) * wm_interm_95_pt3(c,a)
term(281) = term(281) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,i,j) * wm_interm_96_pt3(c,a)
term(282) = term(282) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,i,j) * wm_interm_95_pt3(c,a)
term(283) = term(283) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,i,j) * wm_interm_96_pt3(c,a)
term(284) = term(284) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_120_pt3(c,a)
term(285) = term(285) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_121_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(280) = term(280) * (2.0d+0) 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (-4.0d+0) 
term(283) = term(283) * (8.0d+0) 
term(285) = term(285) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(286) = term(286) + wm_interm_2_pt3(p,q,i,j) * wm_interm_4_pt3(j,i)
term(287) = term(287) + wm_interm_2_pt3(p,i,q,j) * wm_interm_4_pt3(j,i)
term(288) = term(288) + wm_interm_1_pt3(p,i,q,j) * wm_interm_4_pt3(j,i)
term(289) = term(289) + wm_interm_1_pt3(p,q,i,j) * wm_interm_4_pt3(j,i)
term(290) = term(290) + wm_interm_3_pt3(p,i,q,j) * wm_interm_4_pt3(j,i)
term(291) = term(291) + wm_interm_3_pt3(p,q,i,j) * wm_interm_4_pt3(j,i)
term(292) = term(292) + wm_interm_12_pt3(i,j) * wm_interm_13_pt3(p,i,q,j)
term(293) = term(293) + wm_interm_12_pt3(i,j) * wm_interm_14_pt3(p,i,q,j)
term(294) = term(294) + wm_interm_12_pt3(i,j) * wm_interm_15_pt3(p,i,q,j)
term(295) = term(295) + wm_interm_12_pt3(i,j) * wm_interm_16_pt3(p,i,q,j)
term(296) = term(296) + wm_interm_12_pt3(i,j) * wm_interm_17_pt3(p,i,q,j)
term(297) = term(297) + wm_interm_12_pt3(i,j) * wm_interm_18_pt3(p,i,q,j)
term(298) = term(298) + wm_interm_41_pt3(p,i,j,q) * wm_interm_4_pt3(j,i)
term(299) = term(299) + wm_interm_47_pt3(p,i,j,q) * wm_interm_4_pt3(j,i)
term(300) = term(300) + wm_interm_41_pt3(p,i,q,j) * wm_interm_4_pt3(j,i)
term(301) = term(301) + wm_interm_40_pt3(p,i,j,q) * wm_interm_4_pt3(j,i)
term(302) = term(302) + wm_interm_38_pt3(p,i,j,q) * wm_interm_4_pt3(j,i)
term(303) = term(303) + wm_interm_47_pt3(p,i,q,j) * wm_interm_4_pt3(j,i)
term(304) = term(304) + wm_interm_58_pt3(p,i,q,j) * wm_interm_59_pt3(i,j)
term(305) = term(305) + wm_interm_58_pt3(p,q,i,j) * wm_interm_59_pt3(i,j)
term(306) = term(306) + wm_interm_59_pt3(i,j) * wm_interm_61_pt3(p,q,i,j)
term(307) = term(307) + wm_interm_59_pt3(i,j) * wm_interm_61_pt3(p,i,q,j)
term(308) = term(308) + wm_interm_59_pt3(i,j) * wm_interm_62_pt3(p,i,q,j)
term(309) = term(309) + wm_interm_59_pt3(i,j) * wm_interm_62_pt3(p,q,i,j)
term(310) = term(310) + wm_interm_59_pt3(i,j) * wm_interm_63_pt3(p,i,q,j)
term(311) = term(311) + wm_interm_59_pt3(i,j) * wm_interm_63_pt3(p,q,i,j)
term(312) = term(312) + wm_interm_59_pt3(i,j) * wm_interm_64_pt3(p,q,i,j)
term(313) = term(313) + wm_interm_59_pt3(i,j) * wm_interm_64_pt3(p,i,q,j)
term(314) = term(314) + wm_interm_59_pt3(i,j) * wm_interm_65_pt3(p,q,i,j)
term(315) = term(315) + wm_interm_59_pt3(i,j) * wm_interm_65_pt3(p,i,q,j)
term(316) = term(316) + wm_interm_2_pt3(p,q,i,j) * wm_interm_82_pt3(j,i)
term(317) = term(317) + wm_interm_2_pt3(p,q,i,j) * wm_interm_83_pt3(j,i)
term(318) = term(318) + wm_interm_2_pt3(p,i,q,j) * wm_interm_82_pt3(j,i)
term(319) = term(319) + wm_interm_2_pt3(p,i,q,j) * wm_interm_83_pt3(j,i)
term(320) = term(320) + wm_interm_1_pt3(p,i,q,j) * wm_interm_82_pt3(j,i)
term(321) = term(321) + wm_interm_1_pt3(p,i,q,j) * wm_interm_83_pt3(j,i)
term(322) = term(322) + wm_interm_1_pt3(p,q,i,j) * wm_interm_82_pt3(j,i)
term(323) = term(323) + wm_interm_1_pt3(p,q,i,j) * wm_interm_83_pt3(j,i)
term(324) = term(324) + wm_interm_3_pt3(p,i,q,j) * wm_interm_82_pt3(j,i)
term(325) = term(325) + wm_interm_3_pt3(p,i,q,j) * wm_interm_83_pt3(j,i)
term(326) = term(326) + wm_interm_3_pt3(p,q,i,j) * wm_interm_82_pt3(j,i)
term(327) = term(327) + wm_interm_3_pt3(p,q,i,j) * wm_interm_83_pt3(j,i)
term(328) = term(328) + wm_interm_103_pt3(i,j) * wm_interm_84_pt3(p,i,q,j)
term(329) = term(329) + wm_interm_103_pt3(i,j) * wm_interm_85_pt3(p,i,q,j)
term(330) = term(330) + wm_interm_103_pt3(i,j) * wm_interm_88_pt3(p,i,q,j)
term(331) = term(331) + wm_interm_103_pt3(i,j) * wm_interm_88_pt3(p,i,j,q)
term(332) = term(332) + wm_interm_103_pt3(i,j) * wm_interm_84_pt3(p,i,j,q)
term(333) = term(333) + wm_interm_103_pt3(i,j) * wm_interm_85_pt3(p,i,j,q)
term(334) = term(334) + wm_interm_105_pt3(i,j) * wm_interm_84_pt3(p,i,q,j)
term(335) = term(335) + wm_interm_105_pt3(i,j) * wm_interm_85_pt3(p,i,q,j)
term(336) = term(336) + wm_interm_105_pt3(i,j) * wm_interm_88_pt3(p,i,q,j)
term(337) = term(337) + wm_interm_105_pt3(i,j) * wm_interm_88_pt3(p,i,j,q)
term(338) = term(338) + wm_interm_105_pt3(i,j) * wm_interm_84_pt3(p,i,j,q)
term(339) = term(339) + wm_interm_105_pt3(i,j) * wm_interm_85_pt3(p,i,j,q)
term(340) = term(340) + wm_interm_41_pt3(p,i,j,q) * wm_interm_82_pt3(j,i)
term(341) = term(341) + wm_interm_41_pt3(p,i,j,q) * wm_interm_83_pt3(j,i)
term(342) = term(342) + wm_interm_47_pt3(p,i,j,q) * wm_interm_82_pt3(j,i)
term(343) = term(343) + wm_interm_47_pt3(p,i,j,q) * wm_interm_83_pt3(j,i)
term(344) = term(344) + wm_interm_106_pt3(p,i,j,q) * wm_interm_126_pt3(i,j)
term(345) = term(345) + wm_interm_107_pt3(p,i,j,q) * wm_interm_126_pt3(i,j)
term(346) = term(346) + wm_interm_106_pt3(p,i,j,q) * wm_interm_127_pt3(i,j)
term(347) = term(347) + wm_interm_107_pt3(p,i,j,q) * wm_interm_127_pt3(i,j)
term(348) = term(348) + wm_interm_41_pt3(p,i,q,j) * wm_interm_82_pt3(j,i)
term(349) = term(349) + wm_interm_41_pt3(p,i,q,j) * wm_interm_83_pt3(j,i)
term(350) = term(350) + wm_interm_40_pt3(p,i,j,q) * wm_interm_82_pt3(j,i)
term(351) = term(351) + wm_interm_40_pt3(p,i,j,q) * wm_interm_83_pt3(j,i)
term(352) = term(352) + wm_interm_38_pt3(p,i,j,q) * wm_interm_82_pt3(j,i)
term(353) = term(353) + wm_interm_38_pt3(p,i,j,q) * wm_interm_83_pt3(j,i)
term(354) = term(354) + wm_interm_47_pt3(p,i,q,j) * wm_interm_82_pt3(j,i)
term(355) = term(355) + wm_interm_47_pt3(p,i,q,j) * wm_interm_83_pt3(j,i)
term(356) = term(356) + wm_interm_114_pt3(p,i,q,j) * wm_interm_126_pt3(i,j)
term(357) = term(357) + wm_interm_117_pt3(p,i,q,j) * wm_interm_126_pt3(i,j)
term(358) = term(358) + wm_interm_117_pt3(p,i,j,q) * wm_interm_126_pt3(i,j)
term(359) = term(359) + wm_interm_114_pt3(p,i,j,q) * wm_interm_126_pt3(i,j)
term(360) = term(360) + wm_interm_114_pt3(p,i,q,j) * wm_interm_127_pt3(i,j)
term(361) = term(361) + wm_interm_117_pt3(p,i,q,j) * wm_interm_127_pt3(i,j)
term(362) = term(362) + wm_interm_117_pt3(p,i,j,q) * wm_interm_127_pt3(i,j)
term(363) = term(363) + wm_interm_114_pt3(p,i,j,q) * wm_interm_127_pt3(i,j)
term(364) = term(364) + wm_interm_106_pt3(p,i,q,j) * wm_interm_126_pt3(i,j)
term(365) = term(365) + wm_interm_107_pt3(p,i,q,j) * wm_interm_126_pt3(i,j)
term(366) = term(366) + wm_interm_106_pt3(p,i,q,j) * wm_interm_127_pt3(i,j)
term(367) = term(367) + wm_interm_107_pt3(p,i,q,j) * wm_interm_127_pt3(i,j)
end do 
end do 

term(286) = term(286) * (-1.9999999999999998d+0) 
term(287) = term(287) * (3.9999999999999996d+0) 
term(288) = term(288) * (-2.0d+0) 
term(289) = term(289) * (4.0d+0) 
term(290) = term(290) * (4.0d+0) 
term(291) = term(291) * (-8.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * (4.0d+0) 
term(294) = term(294) * (-2.0d+0) 
term(295) = term(295) * (4.0d+0) 
term(296) = term(296) * (4.0d+0) 
term(297) = term(297) * (-8.0d+0) 
term(298) = term(298) * (4.0d+0) 
term(299) = term(299) * (-7.999999999999999d+0) 
term(300) = term(300) * (-2.0d+0) 
term(301) = term(301) * (-2.0d+0) 
term(302) = term(302) * (4.0d+0) 
term(303) = term(303) * (3.9999999999999996d+0) 
term(304) = term(304) * (-1.0d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (-1.0d+0) 
term(307) = term(307) * (2.0d+0) 
term(308) = term(308) * (2.0d+0) 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * (-1.0d+0) 
term(311) = term(311) * (2.0d+0) 
term(312) = term(312) * (2.0d+0) 
term(313) = term(313) * (-4.0d+0) 
term(314) = term(314) * (-1.0d+0) 
term(315) = term(315) * (2.0d+0) 
term(316) = term(316) * (1.9999999999999998d+0) 
term(317) = term(317) * (-3.9999999999999996d+0) 
term(318) = term(318) * (-3.9999999999999996d+0) 
term(319) = term(319) * (7.999999999999999d+0) 
term(320) = term(320) * (2.0d+0) 
term(321) = term(321) * (-4.0d+0) 
term(322) = term(322) * (-4.0d+0) 
term(323) = term(323) * (8.0d+0) 
term(324) = term(324) * (-4.0d+0) 
term(325) = term(325) * (8.0d+0) 
term(326) = term(326) * (8.0d+0) 
term(327) = term(327) * (-16.0d+0) 
term(328) = term(328) * (1.9999999999999998d+0) 
term(329) = term(329) * (-3.9999999999999996d+0) 
term(330) = term(330) * (1.9999999999999998d+0) 
term(331) = term(331) * (-3.9999999999999996d+0) 
term(332) = term(332) * (-3.9999999999999996d+0) 
term(333) = term(333) * (7.999999999999999d+0) 
term(334) = term(334) * (-3.9999999999999996d+0) 
term(335) = term(335) * (7.999999999999999d+0) 
term(336) = term(336) * (-3.9999999999999996d+0) 
term(337) = term(337) * (7.999999999999999d+0) 
term(338) = term(338) * (7.999999999999999d+0) 
term(339) = term(339) * (-15.999999999999998d+0) 
term(340) = term(340) * (-4.0d+0) 
term(341) = term(341) * (8.0d+0) 
term(342) = term(342) * (7.999999999999999d+0) 
term(343) = term(343) * (-15.999999999999998d+0) 
term(344) = term(344) * (-4.0d+0) 
term(345) = term(345) * (8.0d+0) 
term(346) = term(346) * (8.0d+0) 
term(347) = term(347) * (-16.0d+0) 
term(348) = term(348) * (2.0d+0) 
term(349) = term(349) * (-4.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (-4.0d+0) 
term(353) = term(353) * (8.0d+0) 
term(354) = term(354) * (-3.9999999999999996d+0) 
term(355) = term(355) * (7.999999999999999d+0) 
term(357) = term(357) * (-2.0d+0) 
term(359) = term(359) * (-2.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (-2.0d+0) 
term(363) = term(363) * (4.0d+0) 
term(364) = term(364) * (1.9999999999999998d+0) 
term(365) = term(365) * (-3.9999999999999996d+0) 
term(366) = term(366) * (-3.9999999999999996d+0) 
term(367) = term(367) * (7.999999999999999d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(368) = term(368) + wm_interm_101_pt3(p,i,j,q,k,l) * wm_interm_79_pt3(k,l,j,i)
term(369) = term(369) + wm_interm_124_pt3(p,i,j,k,l,q) * wm_interm_79_pt3(k,l,j,i)
term(370) = term(370) + wm_interm_124_pt3(p,i,j,k,q,l) * wm_interm_79_pt3(k,l,j,i)
term(371) = term(371) + wm_interm_124_pt3(p,i,j,k,q,l) * wm_interm_79_pt3(l,k,j,i)
end do 
end do 
end do 
end do 

term(368) = term(368) * (-2.0d+0) 
term(369) = term(369) * (-1.9999999999999998d+0) 
term(371) = term(371) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(372) = term(372) + wm_interm_8_pt3(p,a,q,i) * wm_interm_9_pt3(a,i)
term(373) = term(373) + wm_interm_6_pt3(p,a,i,q) * wm_interm_9_pt3(a,i)
term(374) = term(374) + wm_interm_7_pt3(p,a,i,q) * wm_interm_9_pt3(a,i)
term(375) = term(375) + wm_interm_8_pt3(p,a,i,q) * wm_interm_9_pt3(a,i)
term(376) = term(376) + wm_interm_7_pt3(p,a,q,i) * wm_interm_9_pt3(a,i)
term(377) = term(377) + wm_interm_6_pt3(p,a,q,i) * wm_interm_9_pt3(a,i)
term(378) = term(378) + wm_interm_10_pt3(a,i) * wm_interm_8_pt3(p,a,q,i)
term(379) = term(379) + wm_interm_10_pt3(a,i) * wm_interm_6_pt3(p,a,i,q)
term(380) = term(380) + wm_interm_10_pt3(a,i) * wm_interm_7_pt3(p,a,i,q)
term(381) = term(381) + wm_interm_10_pt3(a,i) * wm_interm_8_pt3(p,a,i,q)
term(382) = term(382) + wm_interm_10_pt3(a,i) * wm_interm_7_pt3(p,a,q,i)
term(383) = term(383) + wm_interm_10_pt3(a,i) * wm_interm_6_pt3(p,a,q,i)
term(384) = term(384) + wm_interm_19_pt3(a,p,i,q) * wm_interm_20_pt3(a,i)
term(385) = term(385) + wm_interm_19_pt3(a,p,i,q) * wm_interm_21_pt3(a,i)
term(386) = term(386) + wm_interm_19_pt3(a,p,i,q) * wm_interm_22_pt3(a,i)
term(387) = term(387) + wm_interm_19_pt3(a,p,i,q) * wm_interm_23_pt3(a,i)
term(388) = term(388) + wm_interm_19_pt3(p,a,i,q) * wm_interm_20_pt3(a,i)
term(389) = term(389) + wm_interm_19_pt3(p,a,i,q) * wm_interm_21_pt3(a,i)
term(390) = term(390) + wm_interm_19_pt3(p,a,i,q) * wm_interm_22_pt3(a,i)
term(391) = term(391) + wm_interm_19_pt3(p,a,i,q) * wm_interm_23_pt3(a,i)
term(392) = term(392) + wm_interm_20_pt3(a,i) * wm_interm_31_pt3(a,p,i,q)
term(393) = term(393) + wm_interm_21_pt3(a,i) * wm_interm_31_pt3(a,p,i,q)
term(394) = term(394) + wm_interm_22_pt3(a,i) * wm_interm_31_pt3(a,p,i,q)
term(395) = term(395) + wm_interm_23_pt3(a,i) * wm_interm_31_pt3(a,p,i,q)
term(396) = term(396) + wm_interm_45_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(397) = term(397) + wm_interm_46_pt3(a,i) * wm_interm_48_pt3(a,p,i,q)
term(398) = term(398) + wm_interm_48_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(399) = term(399) + wm_interm_45_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(400) = term(400) + wm_interm_43_pt3(a,p,q,i) * wm_interm_46_pt3(a,i)
term(401) = term(401) + wm_interm_46_pt3(a,i) * wm_interm_48_pt3(a,p,q,i)
term(402) = term(402) + wm_interm_48_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(403) = term(403) + wm_interm_43_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(404) = term(404) + wm_interm_43_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
term(405) = term(405) + wm_interm_43_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(406) = term(406) + wm_interm_45_pt3(a,p,q,i) * wm_interm_46_pt3(a,i)
term(407) = term(407) + wm_interm_45_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(408) = term(408) + wm_interm_50_pt3(p,a,q,i) * wm_interm_51_pt3(a,i)
term(409) = term(409) + wm_interm_51_pt3(a,i) * wm_interm_52_pt3(p,a,q,i)
term(410) = term(410) + wm_interm_50_pt3(p,a,i,q) * wm_interm_51_pt3(a,i)
term(411) = term(411) + wm_interm_51_pt3(a,i) * wm_interm_53_pt3(p,a,q,i)
term(412) = term(412) + wm_interm_51_pt3(a,i) * wm_interm_54_pt3(p,a,q,i)
term(413) = term(413) + wm_interm_51_pt3(a,i) * wm_interm_52_pt3(p,a,i,q)
term(414) = term(414) + wm_interm_51_pt3(a,i) * wm_interm_55_pt3(p,a,q,i)
term(415) = term(415) + wm_interm_51_pt3(a,i) * wm_interm_55_pt3(p,a,i,q)
term(416) = term(416) + wm_interm_50_pt3(a,p,q,i) * wm_interm_51_pt3(a,i)
term(417) = term(417) + wm_interm_51_pt3(a,i) * wm_interm_54_pt3(a,p,q,i)
term(418) = term(418) + wm_interm_51_pt3(a,i) * wm_interm_52_pt3(a,p,q,i)
term(419) = term(419) + wm_interm_51_pt3(a,i) * wm_interm_52_pt3(a,p,i,q)
term(420) = term(420) + wm_interm_51_pt3(a,i) * wm_interm_53_pt3(p,a,i,q)
term(421) = term(421) + wm_interm_51_pt3(a,i) * wm_interm_54_pt3(p,a,i,q)
term(422) = term(422) + wm_interm_51_pt3(a,i) * wm_interm_54_pt3(a,p,i,q)
term(423) = term(423) + wm_interm_50_pt3(a,p,i,q) * wm_interm_51_pt3(a,i)
term(424) = term(424) + wm_interm_51_pt3(a,i) * wm_interm_55_pt3(a,p,i,q)
term(425) = term(425) + wm_interm_51_pt3(a,i) * wm_interm_53_pt3(a,p,q,i)
term(426) = term(426) + wm_interm_51_pt3(a,i) * wm_interm_55_pt3(a,p,q,i)
term(427) = term(427) + wm_interm_51_pt3(a,i) * wm_interm_53_pt3(a,p,i,q)
term(428) = term(428) + wm_interm_53_pt3(a,p,q,i) * wm_interm_57_pt3(a,i)
term(429) = term(429) + wm_interm_55_pt3(a,p,q,i) * wm_interm_57_pt3(a,i)
term(430) = term(430) + wm_interm_50_pt3(a,p,q,i) * wm_interm_57_pt3(a,i)
term(431) = term(431) + wm_interm_52_pt3(a,p,q,i) * wm_interm_57_pt3(a,i)
term(432) = term(432) + wm_interm_55_pt3(p,a,q,i) * wm_interm_57_pt3(a,i)
term(433) = term(433) + wm_interm_53_pt3(p,a,q,i) * wm_interm_57_pt3(a,i)
term(434) = term(434) + wm_interm_50_pt3(p,a,q,i) * wm_interm_57_pt3(a,i)
term(435) = term(435) + wm_interm_52_pt3(p,a,q,i) * wm_interm_57_pt3(a,i)
term(436) = term(436) + wm_interm_53_pt3(p,a,i,q) * wm_interm_57_pt3(a,i)
term(437) = term(437) + wm_interm_52_pt3(p,a,i,q) * wm_interm_57_pt3(a,i)
term(438) = term(438) + wm_interm_50_pt3(p,a,i,q) * wm_interm_57_pt3(a,i)
term(439) = term(439) + wm_interm_54_pt3(p,a,i,q) * wm_interm_57_pt3(a,i)
term(440) = term(440) + wm_interm_54_pt3(p,a,q,i) * wm_interm_57_pt3(a,i)
term(441) = term(441) + wm_interm_55_pt3(a,p,i,q) * wm_interm_57_pt3(a,i)
term(442) = term(442) + wm_interm_50_pt3(a,p,i,q) * wm_interm_57_pt3(a,i)
term(443) = term(443) + wm_interm_55_pt3(p,a,i,q) * wm_interm_57_pt3(a,i)
term(444) = term(444) + wm_interm_54_pt3(a,p,q,i) * wm_interm_57_pt3(a,i)
term(445) = term(445) + wm_interm_52_pt3(a,p,i,q) * wm_interm_57_pt3(a,i)
term(446) = term(446) + wm_interm_54_pt3(a,p,i,q) * wm_interm_57_pt3(a,i)
term(447) = term(447) + wm_interm_53_pt3(a,p,i,q) * wm_interm_57_pt3(a,i)
term(448) = term(448) + wm_interm_60_pt3(a,p,i,q) * wm_interm_66_pt3(a,i)
term(449) = term(449) + wm_interm_60_pt3(a,p,i,q) * wm_interm_68_pt3(a,i)
term(450) = term(450) + wm_interm_60_pt3(a,p,i,q) * wm_interm_69_pt3(a,i)
term(451) = term(451) + wm_interm_60_pt3(a,p,i,q) * wm_interm_70_pt3(a,i)
term(452) = term(452) + wm_interm_66_pt3(a,i) * wm_interm_71_pt3(p,a,i,q)
term(453) = term(453) + wm_interm_68_pt3(a,i) * wm_interm_71_pt3(p,a,i,q)
term(454) = term(454) + wm_interm_69_pt3(a,i) * wm_interm_71_pt3(p,a,i,q)
term(455) = term(455) + wm_interm_70_pt3(a,i) * wm_interm_71_pt3(p,a,i,q)
term(456) = term(456) + wm_interm_66_pt3(a,i) * wm_interm_71_pt3(a,p,i,q)
term(457) = term(457) + wm_interm_68_pt3(a,i) * wm_interm_71_pt3(a,p,i,q)
term(458) = term(458) + wm_interm_69_pt3(a,i) * wm_interm_71_pt3(a,p,i,q)
term(459) = term(459) + wm_interm_70_pt3(a,i) * wm_interm_71_pt3(a,p,i,q)
term(460) = term(460) + wm_interm_87_pt3(p,a,q,i) * wm_interm_89_pt3(a,i)
term(461) = term(461) + wm_interm_87_pt3(p,a,q,i) * wm_interm_90_pt3(a,i)
term(462) = term(462) + wm_interm_87_pt3(p,a,q,i) * wm_interm_91_pt3(a,i)
term(463) = term(463) + wm_interm_87_pt3(p,a,q,i) * wm_interm_92_pt3(a,i)
term(464) = term(464) + wm_interm_87_pt3(p,a,q,i) * wm_interm_93_pt3(a,i)
term(465) = term(465) + wm_interm_87_pt3(p,a,q,i) * wm_interm_94_pt3(a,i)
term(466) = term(466) + wm_interm_89_pt3(a,i) * wm_interm_97_pt3(p,a,q,i)
term(467) = term(467) + wm_interm_90_pt3(a,i) * wm_interm_97_pt3(p,a,q,i)
term(468) = term(468) + wm_interm_91_pt3(a,i) * wm_interm_97_pt3(p,a,q,i)
term(469) = term(469) + wm_interm_92_pt3(a,i) * wm_interm_97_pt3(p,a,q,i)
term(470) = term(470) + wm_interm_93_pt3(a,i) * wm_interm_97_pt3(p,a,q,i)
term(471) = term(471) + wm_interm_94_pt3(a,i) * wm_interm_97_pt3(p,a,q,i)
term(472) = term(472) + wm_interm_93_pt3(a,i) * wm_interm_98_pt3(p,a,q,i)
term(473) = term(473) + wm_interm_94_pt3(a,i) * wm_interm_98_pt3(p,a,q,i)
term(474) = term(474) + wm_interm_89_pt3(a,i) * wm_interm_98_pt3(p,a,q,i)
term(475) = term(475) + wm_interm_90_pt3(a,i) * wm_interm_98_pt3(p,a,q,i)
term(476) = term(476) + wm_interm_91_pt3(a,i) * wm_interm_98_pt3(p,a,q,i)
term(477) = term(477) + wm_interm_92_pt3(a,i) * wm_interm_98_pt3(p,a,q,i)
term(478) = term(478) + wm_interm_102_pt3(p,a,q,i) * wm_interm_89_pt3(a,i)
term(479) = term(479) + wm_interm_102_pt3(p,a,q,i) * wm_interm_90_pt3(a,i)
term(480) = term(480) + wm_interm_102_pt3(p,a,q,i) * wm_interm_91_pt3(a,i)
term(481) = term(481) + wm_interm_102_pt3(p,a,q,i) * wm_interm_92_pt3(a,i)
term(482) = term(482) + wm_interm_102_pt3(p,a,q,i) * wm_interm_93_pt3(a,i)
term(483) = term(483) + wm_interm_102_pt3(p,a,q,i) * wm_interm_94_pt3(a,i)
term(484) = term(484) + wm_interm_34_pt3(a,i) * wm_interm_80_pt3(a,p,i,q)
term(485) = term(485) + wm_interm_37_pt3(a,i) * wm_interm_80_pt3(a,p,i,q)
term(486) = term(486) + wm_interm_34_pt3(a,i) * wm_interm_81_pt3(a,p,i,q)
term(487) = term(487) + wm_interm_37_pt3(a,i) * wm_interm_81_pt3(a,p,i,q)
term(488) = term(488) + wm_interm_34_pt3(a,i) * wm_interm_77_pt3(a,p,i,q)
term(489) = term(489) + wm_interm_37_pt3(a,i) * wm_interm_77_pt3(a,p,i,q)
term(490) = term(490) + wm_interm_34_pt3(a,i) * wm_interm_78_pt3(a,p,i,q)
term(491) = term(491) + wm_interm_37_pt3(a,i) * wm_interm_78_pt3(a,p,i,q)
term(492) = term(492) + wm_interm_35_pt3(a,i) * wm_interm_77_pt3(a,p,i,q)
term(493) = term(493) + wm_interm_35_pt3(a,i) * wm_interm_78_pt3(a,p,i,q)
term(494) = term(494) + wm_interm_39_pt3(a,i) * wm_interm_77_pt3(a,p,i,q)
term(495) = term(495) + wm_interm_39_pt3(a,i) * wm_interm_78_pt3(a,p,i,q)
term(496) = term(496) + wm_interm_35_pt3(a,i) * wm_interm_80_pt3(a,p,i,q)
term(497) = term(497) + wm_interm_39_pt3(a,i) * wm_interm_80_pt3(a,p,i,q)
term(498) = term(498) + wm_interm_35_pt3(a,i) * wm_interm_81_pt3(a,p,i,q)
term(499) = term(499) + wm_interm_39_pt3(a,i) * wm_interm_81_pt3(a,p,i,q)
term(500) = term(500) + wm_interm_108_pt3(a,i) * wm_interm_109_pt3(p,a,q,i)
term(501) = term(501) + wm_interm_108_pt3(a,i) * wm_interm_110_pt3(p,a,q,i)
term(502) = term(502) + wm_interm_108_pt3(a,i) * wm_interm_111_pt3(p,a,q,i)
term(503) = term(503) + wm_interm_108_pt3(a,i) * wm_interm_112_pt3(p,a,q,i)
term(504) = term(504) + wm_interm_109_pt3(p,a,q,i) * wm_interm_113_pt3(a,i)
term(505) = term(505) + wm_interm_110_pt3(p,a,q,i) * wm_interm_113_pt3(a,i)
term(506) = term(506) + wm_interm_111_pt3(p,a,q,i) * wm_interm_113_pt3(a,i)
term(507) = term(507) + wm_interm_112_pt3(p,a,q,i) * wm_interm_113_pt3(a,i)
term(508) = term(508) + wm_interm_111_pt3(p,a,q,i) * wm_interm_115_pt3(a,i)
term(509) = term(509) + wm_interm_112_pt3(p,a,q,i) * wm_interm_115_pt3(a,i)
term(510) = term(510) + wm_interm_109_pt3(p,a,q,i) * wm_interm_115_pt3(a,i)
term(511) = term(511) + wm_interm_110_pt3(p,a,q,i) * wm_interm_115_pt3(a,i)
term(512) = term(512) + wm_interm_109_pt3(p,a,q,i) * wm_interm_116_pt3(a,i)
term(513) = term(513) + wm_interm_110_pt3(p,a,q,i) * wm_interm_116_pt3(a,i)
term(514) = term(514) + wm_interm_111_pt3(p,a,q,i) * wm_interm_116_pt3(a,i)
term(515) = term(515) + wm_interm_112_pt3(p,a,q,i) * wm_interm_116_pt3(a,i)
end do 
end do 

term(372) = term(372) * (4.0d+0) 
term(373) = term(373) * (-8.0d+0) 
term(374) = term(374) * (4.0d+0) 
term(375) = term(375) * (-8.0d+0) 
term(376) = term(376) * (-7.999999999999999d+0) 
term(377) = term(377) * (15.999999999999998d+0) 
term(378) = term(378) * (-2.0d+0) 
term(379) = term(379) * (4.0d+0) 
term(380) = term(380) * (-2.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(382) = term(382) * (3.9999999999999996d+0) 
term(383) = term(383) * (-7.999999999999999d+0) 
term(384) = term(384) * (8.0d+0) 
term(385) = term(385) * (-8.0d+0) 
term(386) = term(386) * (-4.0d+0) 
term(387) = term(387) * (4.0d+0) 
term(388) = term(388) * (-4.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (2.0d+0) 
term(391) = term(391) * (-2.0d+0) 
term(392) = term(392) * (8.0d+0) 
term(393) = term(393) * (-8.0d+0) 
term(394) = term(394) * (-4.0d+0) 
term(395) = term(395) * (4.0d+0) 
term(396) = term(396) * (-8.0d+0) 
term(397) = term(397) * (4.0d+0) 
term(398) = term(398) * (-8.0d+0) 
term(399) = term(399) * (16.0d+0) 
term(400) = term(400) * (4.0d+0) 
term(401) = term(401) * (-2.0d+0) 
term(402) = term(402) * (3.9999999999999996d+0) 
term(403) = term(403) * (-8.0d+0) 
term(404) = term(404) * (4.0d+0) 
term(405) = term(405) * (-2.0000000000000004d+0) 
term(406) = term(406) * (4.0d+0) 
term(407) = term(407) * (-7.999999999999999d+0) 
term(408) = term(408) * (-2.6666666666666665d+0) 
term(409) = term(409) * (8.0d+0) 
term(410) = term(410) * (1.3333333333333333d+0) 
term(411) = term(411) * (-2.6666666666666665d+0) 
term(412) = term(412) * (0.6666666666666666d+0) 
term(413) = term(413) * (-4.0d+0) 
term(414) = term(414) * (0.6666666666666666d+0) 
term(415) = term(415) * (-1.3333333333333333d+0) 
term(416) = term(416) * (1.3333333333333333d+0) 
term(417) = term(417) * (-1.3333333333333333d+0) 
term(418) = term(418) * (-4.0d+0) 
term(419) = term(419) * (8.0d+0) 
term(420) = term(420) * (1.3333333333333333d+0) 
term(421) = term(421) * (-1.3333333333333333d+0) 
term(422) = term(422) * (0.6666666666666666d+0) 
term(423) = term(423) * (-2.6666666666666665d+0) 
term(424) = term(424) * (0.6666666666666666d+0) 
term(425) = term(425) * (1.3333333333333333d+0) 
term(426) = term(426) * (-1.3333333333333333d+0) 
term(427) = term(427) * (-2.6666666666666665d+0) 
term(428) = term(428) * (-0.6666666666666666d+0) 
term(429) = term(429) * (0.6666666666666666d+0) 
term(430) = term(430) * (-0.6666666666666666d+0) 
term(431) = term(431) * (2.0d+0) 
term(432) = term(432) * (-0.3333333333333333d+0) 
term(433) = term(433) * (1.3333333333333333d+0) 
term(434) = term(434) * (1.3333333333333333d+0) 
term(435) = term(435) * (-4.0d+0) 
term(436) = term(436) * (-0.6666666666666666d+0) 
term(437) = term(437) * (2.0d+0) 
term(438) = term(438) * (-0.6666666666666666d+0) 
term(439) = term(439) * (0.6666666666666666d+0) 
term(440) = term(440) * (-0.3333333333333333d+0) 
term(441) = term(441) * (-0.3333333333333333d+0) 
term(442) = term(442) * (1.3333333333333333d+0) 
term(443) = term(443) * (0.6666666666666666d+0) 
term(444) = term(444) * (0.6666666666666666d+0) 
term(445) = term(445) * (-4.0d+0) 
term(446) = term(446) * (-0.3333333333333333d+0) 
term(447) = term(447) * (1.3333333333333333d+0) 
term(448) = term(448) * (-7.999999999999999d+0) 
term(449) = term(449) * (8.0d+0) 
term(450) = term(450) * (3.9999999999999996d+0) 
term(451) = term(451) * (-4.0d+0) 
term(452) = term(452) * (3.9999999999999996d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (-1.9999999999999998d+0) 
term(455) = term(455) * (2.0d+0) 
term(456) = term(456) * (-7.999999999999999d+0) 
term(457) = term(457) * (8.0d+0) 
term(458) = term(458) * (3.9999999999999996d+0) 
term(459) = term(459) * (-4.0d+0) 
term(460) = term(460) * (4.0d+0) 
term(461) = term(461) * (-8.0d+0) 
term(462) = term(462) * (4.0d+0) 
term(463) = term(463) * (-8.0d+0) 
term(464) = term(464) * (-7.999999999999999d+0) 
term(465) = term(465) * (15.999999999999998d+0) 
term(466) = term(466) * (-2.0d+0) 
term(467) = term(467) * (4.0d+0) 
term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (4.0d+0) 
term(470) = term(470) * (3.9999999999999996d+0) 
term(471) = term(471) * (-7.999999999999999d+0) 
term(472) = term(472) * (-1.9999999999999998d+0) 
term(473) = term(473) * (3.9999999999999996d+0) 
term(475) = term(475) * (-2.0d+0) 
term(477) = term(477) * (-2.0d+0) 
term(478) = term(478) * (-2.0d+0) 
term(479) = term(479) * (4.0d+0) 
term(480) = term(480) * (-2.0d+0) 
term(481) = term(481) * (4.0d+0) 
term(482) = term(482) * (3.9999999999999996d+0) 
term(483) = term(483) * (-7.999999999999999d+0) 
term(484) = term(484) * (3.9999999999999996d+0) 
term(485) = term(485) * (-7.999999999999999d+0) 
term(486) = term(486) * (-7.999999999999999d+0) 
term(487) = term(487) * (15.999999999999998d+0) 
term(488) = term(488) * (-1.9999999999999998d+0) 
term(489) = term(489) * (3.9999999999999996d+0) 
term(490) = term(490) * (3.9999999999999996d+0) 
term(491) = term(491) * (-7.999999999999999d+0) 
term(492) = term(492) * (2.0d+0) 
term(493) = term(493) * (-4.0d+0) 
term(494) = term(494) * (-4.0d+0) 
term(495) = term(495) * (8.0d+0) 
term(496) = term(496) * (-4.0d+0) 
term(497) = term(497) * (8.0d+0) 
term(498) = term(498) * (8.0d+0) 
term(499) = term(499) * (-16.0d+0) 
term(500) = term(500) * (4.0d+0) 
term(501) = term(501) * (-8.0d+0) 
term(502) = term(502) * (-8.0d+0) 
term(503) = term(503) * (16.0d+0) 
term(504) = term(504) * (-2.0d+0) 
term(505) = term(505) * (4.0d+0) 
term(506) = term(506) * (4.0d+0) 
term(507) = term(507) * (-8.0d+0) 
term(508) = term(508) * (-4.0d+0) 
term(509) = term(509) * (8.0d+0) 
term(510) = term(510) * (2.0d+0) 
term(511) = term(511) * (-4.0d+0) 
term(512) = term(512) * (-4.0d+0) 
term(513) = term(513) * (8.0d+0) 
term(514) = term(514) * (8.0d+0) 
term(515) = term(515) * (-16.0d+0) 

term(516) = term(516) + wm_interm_34_pt3(p,q) * wm_interm_42_pt3
term(517) = term(517) + wm_interm_37_pt3(p,q) * wm_interm_42_pt3
term(518) = term(518) + wm_interm_35_pt3(p,q) * wm_interm_42_pt3
term(519) = term(519) + wm_interm_39_pt3(p,q) * wm_interm_42_pt3
term(520) = term(520) + wm_interm_122_pt3 * wm_interm_34_pt3(p,q)
term(521) = term(521) + wm_interm_123_pt3 * wm_interm_34_pt3(p,q)
term(522) = term(522) + wm_interm_122_pt3 * wm_interm_37_pt3(p,q)
term(523) = term(523) + wm_interm_123_pt3 * wm_interm_37_pt3(p,q)
term(524) = term(524) + wm_interm_122_pt3 * wm_interm_35_pt3(p,q)
term(525) = term(525) + wm_interm_123_pt3 * wm_interm_35_pt3(p,q)
term(526) = term(526) + wm_interm_122_pt3 * wm_interm_39_pt3(p,q)
term(527) = term(527) + wm_interm_123_pt3 * wm_interm_39_pt3(p,q)

term(516) = term(516) * (-3.9999999999999996d+0) 
term(517) = term(517) * (7.999999999999999d+0) 
term(518) = term(518) * (3.9999999999999996d+0) 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * (1.9999999999999998d+0) 
term(521) = term(521) * (-3.9999999999999996d+0) 
term(522) = term(522) * (-3.9999999999999996d+0) 
term(523) = term(523) * (7.999999999999999d+0) 
term(524) = term(524) * (-1.9999999999999998d+0) 
term(525) = term(525) * (3.9999999999999996d+0) 
term(526) = term(526) * (4.0d+0) 
term(527) = term(527) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(528) = term(528) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,k,j) * wm_interm_84_pt3(b,i,j,k)
term(529) = term(529) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,k,j) * wm_interm_85_pt3(b,i,j,k)
term(530) = term(530) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,k,j) * wm_interm_88_pt3(b,i,j,k)
term(531) = term(531) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_106_pt3(b,i,j,k)
term(532) = term(532) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_107_pt3(b,i,j,k)
term(533) = term(533) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_114_pt3(b,i,j,k)
term(534) = term(534) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_117_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(528) = term(528) * (-4.0d+0) 
term(529) = term(529) * (8.0d+0) 
term(530) = term(530) * (-3.9999999999999996d+0) 
term(531) = term(531) * (-4.0d+0) 
term(532) = term(532) * (8.0d+0) 
term(533) = term(533) * (-2.0d+0) 
term(534) = term(534) * (4.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(535) = term(535) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_80_pt3(a,b,k,j)
term(536) = term(536) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_pt3(a,b,k,j)
term(537) = term(537) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_78_pt3(a,b,k,j)
term(538) = term(538) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_77_pt3(a,b,k,j)
term(539) = term(539) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_80_pt3(a,b,k,j)
term(540) = term(540) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_pt3(a,b,k,j)
term(541) = term(541) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_77_pt3(a,b,k,j)
term(542) = term(542) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_78_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(535) = term(535) * (2.0d+0) 
term(536) = term(536) * (-4.0d+0) 
term(537) = term(537) * (2.0d+0) 
term(538) = term(538) * (-4.0d+0) 
term(539) = term(539) * (-3.9999999999999996d+0) 
term(540) = term(540) * (7.999999999999999d+0) 
term(541) = term(541) * (1.9999999999999998d+0) 
term(542) = term(542) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(543) = term(543) + wm_interm_2_pt3(p,i,j,k) * wm_interm_79_pt3(q,k,i,j)
term(544) = term(544) + wm_interm_1_pt3(p,i,j,k) * wm_interm_79_pt3(q,k,i,j)
term(545) = term(545) + wm_interm_3_pt3(p,i,j,k) * wm_interm_79_pt3(q,k,i,j)
term(546) = term(546) + wm_interm_1_pt3(p,i,j,k) * wm_interm_79_pt3(k,q,i,j)
term(547) = term(547) + wm_interm_2_pt3(p,i,j,k) * wm_interm_79_pt3(k,q,i,j)
term(548) = term(548) + wm_interm_3_pt3(p,i,j,k) * wm_interm_79_pt3(k,q,i,j)
term(549) = term(549) + wm_interm_104_pt3(q,i,j,k) * wm_interm_84_pt3(p,i,j,k)
term(550) = term(550) + wm_interm_104_pt3(q,i,j,k) * wm_interm_85_pt3(p,i,j,k)
term(551) = term(551) + wm_interm_104_pt3(i,q,j,k) * wm_interm_84_pt3(p,i,j,k)
term(552) = term(552) + wm_interm_104_pt3(i,q,j,k) * wm_interm_85_pt3(p,i,j,k)
term(553) = term(553) + wm_interm_104_pt3(i,q,j,k) * wm_interm_84_pt3(p,i,k,j)
term(554) = term(554) + wm_interm_104_pt3(i,q,j,k) * wm_interm_85_pt3(p,i,k,j)
term(555) = term(555) + wm_interm_104_pt3(i,q,j,k) * wm_interm_88_pt3(p,i,k,j)
term(556) = term(556) + wm_interm_104_pt3(i,q,j,k) * wm_interm_88_pt3(p,i,j,k)
term(557) = term(557) + wm_interm_104_pt3(q,i,j,k) * wm_interm_84_pt3(p,i,k,j)
term(558) = term(558) + wm_interm_104_pt3(q,i,j,k) * wm_interm_85_pt3(p,i,k,j)
term(559) = term(559) + wm_interm_104_pt3(q,i,j,k) * wm_interm_88_pt3(p,i,j,k)
term(560) = term(560) + wm_interm_104_pt3(q,i,j,k) * wm_interm_88_pt3(p,i,k,j)
end do 
end do 
end do 

term(544) = term(544) * (-2.0d+0) 
term(545) = term(545) * (4.0d+0) 
term(547) = term(547) * (-1.9999999999999998d+0) 
term(548) = term(548) * (-2.0d+0) 
term(550) = term(550) * (-2.0d+0) 
term(551) = term(551) * (-2.0d+0) 
term(552) = term(552) * (4.0d+0) 
term(554) = term(554) * (-2.0d+0) 
term(556) = term(556) * (-2.0d+0) 
term(557) = term(557) * (-2.0d+0) 
term(558) = term(558) * (4.0d+0) 
term(560) = term(560) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(561) = term(561) + wm_interm_2_pt3(p,i,j,k) * wm_interm_79_pt3(q,k,j,i)
term(562) = term(562) + wm_interm_1_pt3(p,i,j,k) * wm_interm_79_pt3(q,k,j,i)
term(563) = term(563) + wm_interm_3_pt3(p,i,j,k) * wm_interm_79_pt3(q,k,j,i)
term(564) = term(564) + wm_interm_1_pt3(p,i,j,k) * wm_interm_79_pt3(k,q,j,i)
term(565) = term(565) + wm_interm_2_pt3(p,i,j,k) * wm_interm_79_pt3(k,q,j,i)
term(566) = term(566) + wm_interm_3_pt3(p,i,j,k) * wm_interm_79_pt3(k,q,j,i)
end do 
end do 
end do 

term(561) = term(561) * (-1.9999999999999998d+0) 
term(563) = term(563) * (-2.0d+0) 
term(564) = term(564) * (-2.0d+0) 
term(566) = term(566) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(567) = term(567) + wm_interm_101_pt3(p,i,j,q,k,l) * wm_interm_79_pt3(k,l,i,j)
term(568) = term(568) + wm_interm_101_pt3(p,i,q,j,k,l) * wm_interm_79_pt3(k,l,j,i)
term(569) = term(569) + wm_interm_101_pt3(p,q,i,j,k,l) * wm_interm_79_pt3(k,l,j,i)
term(570) = term(570) + wm_interm_101_pt3(p,i,q,j,k,l) * wm_interm_79_pt3(k,l,i,j)
term(571) = term(571) + wm_interm_101_pt3(p,q,i,j,k,l) * wm_interm_79_pt3(k,l,i,j)
term(572) = term(572) + wm_interm_104_pt3(i,j,k,l) * wm_interm_86_pt3(p,i,j,q,k,l)
term(573) = term(573) + wm_interm_104_pt3(i,j,k,l) * wm_interm_86_pt3(p,i,j,k,q,l)
term(574) = term(574) + wm_interm_104_pt3(i,j,k,l) * wm_interm_86_pt3(p,i,j,k,l,q)
term(575) = term(575) + wm_interm_124_pt3(p,i,j,k,l,q) * wm_interm_79_pt3(k,l,i,j)
term(576) = term(576) + wm_interm_118_pt3(p,i,j,k,l,q) * wm_interm_125_pt3(i,j,l,k)
term(577) = term(577) + wm_interm_118_pt3(p,i,j,k,l,q) * wm_interm_125_pt3(i,j,k,l)
term(578) = term(578) + wm_interm_124_pt3(p,i,j,k,q,l) * wm_interm_79_pt3(k,l,i,j)
term(579) = term(579) + wm_interm_124_pt3(p,i,j,k,q,l) * wm_interm_79_pt3(l,k,i,j)
term(580) = term(580) + wm_interm_118_pt3(p,i,j,k,q,l) * wm_interm_125_pt3(i,j,l,k)
term(581) = term(581) + wm_interm_118_pt3(p,i,j,k,q,l) * wm_interm_125_pt3(i,j,k,l)
term(582) = term(582) + wm_interm_118_pt3(p,i,j,q,k,l) * wm_interm_125_pt3(i,j,k,l)
term(583) = term(583) + wm_interm_118_pt3(p,i,j,q,k,l) * wm_interm_125_pt3(i,j,l,k)
end do 
end do 
end do 
end do 

term(569) = term(569) * (-2.0d+0) 
term(570) = term(570) * (-2.0d+0) 
term(571) = term(571) * (4.0d+0) 
term(573) = term(573) * (-2.0d+0) 
term(574) = term(574) * (4.0d+0) 
term(575) = term(575) * (3.9999999999999996d+0) 
term(576) = term(576) * (-1.9999999999999998d+0) 
term(577) = term(577) * (3.9999999999999996d+0) 
term(578) = term(578) * (-2.0d+0) 
term(581) = term(581) * (-2.0d+0) 
term(583) = term(583) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(584) = term(584) + wm_interm_13_pt3(a,i,j,q) * wm_interm_19_pt3(a,p,j,i)
term(585) = term(585) + wm_interm_14_pt3(a,i,j,q) * wm_interm_19_pt3(a,p,j,i)
term(586) = term(586) + wm_interm_11_pt3(a,i,j,q) * wm_interm_24_pt3(a,p,j,i)
term(587) = term(587) + wm_interm_11_pt3(a,i,j,q) * wm_interm_24_pt3(p,a,j,i)
term(588) = term(588) + wm_interm_11_pt3(a,i,j,q) * wm_interm_25_pt3(p,a,j,i)
term(589) = term(589) + wm_interm_11_pt3(a,i,j,q) * wm_interm_25_pt3(a,p,j,i)
term(590) = term(590) + wm_interm_64_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,j,i)
term(591) = term(591) + wm_interm_61_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,j,i)
term(592) = term(592) + wm_interm_62_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,j,i)
term(593) = term(593) + wm_interm_63_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,j,i)
term(594) = term(594) + wm_interm_58_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,j,i)
term(595) = term(595) + wm_interm_65_pt3(a,i,j,q) * wm_interm_71_pt3(p,a,j,i)
term(596) = term(596) + wm_interm_61_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,j,i)
term(597) = term(597) + wm_interm_58_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,j,i)
term(598) = term(598) + wm_interm_64_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,j,i)
term(599) = term(599) + wm_interm_62_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,j,i)
term(600) = term(600) + wm_interm_63_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,j,i)
term(601) = term(601) + wm_interm_65_pt3(a,i,j,q) * wm_interm_71_pt3(a,p,j,i)
term(602) = term(602) + wm_interm_32_pt3(a,i,j,q) * wm_interm_77_pt3(a,p,j,i)
term(603) = term(603) + wm_interm_32_pt3(a,i,j,q) * wm_interm_78_pt3(a,p,j,i)
term(604) = term(604) + wm_interm_32_pt3(a,i,j,q) * wm_interm_80_pt3(a,p,j,i)
term(605) = term(605) + wm_interm_32_pt3(a,i,j,q) * wm_interm_81_pt3(a,p,j,i)
term(606) = term(606) + wm_interm_38_pt3(a,i,j,q) * wm_interm_80_pt3(a,p,j,i)
term(607) = term(607) + wm_interm_38_pt3(a,i,j,q) * wm_interm_81_pt3(a,p,j,i)
term(608) = term(608) + wm_interm_38_pt3(a,i,j,q) * wm_interm_78_pt3(a,p,j,i)
term(609) = term(609) + wm_interm_38_pt3(a,i,j,q) * wm_interm_77_pt3(a,p,j,i)
term(610) = term(610) + wm_interm_40_pt3(a,i,j,q) * wm_interm_80_pt3(a,p,j,i)
term(611) = term(611) + wm_interm_40_pt3(a,i,j,q) * wm_interm_81_pt3(a,p,j,i)
term(612) = term(612) + wm_interm_40_pt3(a,i,j,q) * wm_interm_77_pt3(a,p,j,i)
term(613) = term(613) + wm_interm_40_pt3(a,i,j,q) * wm_interm_78_pt3(a,p,j,i)
term(614) = term(614) + wm_interm_36_pt3(a,i,j,q) * wm_interm_77_pt3(a,p,j,i)
term(615) = term(615) + wm_interm_36_pt3(a,i,j,q) * wm_interm_78_pt3(a,p,j,i)
term(616) = term(616) + wm_interm_36_pt3(a,i,j,q) * wm_interm_80_pt3(a,p,j,i)
term(617) = term(617) + wm_interm_36_pt3(a,i,j,q) * wm_interm_81_pt3(a,p,j,i)
end do 
end do 
end do 

term(584) = term(584) * (3.9999999999999996d+0) 
term(585) = term(585) * (-7.999999999999999d+0) 
term(586) = term(586) * (-1.9999999999999998d+0) 
term(587) = term(587) * (3.9999999999999996d+0) 
term(588) = term(588) * (-4.0d+0) 
term(589) = term(589) * (2.0d+0) 
term(590) = term(590) * (2.0d+0) 
term(591) = term(591) * (-1.0d+0) 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * (2.0d+0) 
term(594) = term(594) * (2.0d+0) 
term(595) = term(595) * (-1.0d+0) 
term(596) = term(596) * (2.0d+0) 
term(597) = term(597) * (-1.0d+0) 
term(598) = term(598) * (-4.0d+0) 
term(599) = term(599) * (2.0d+0) 
term(600) = term(600) * (-1.0d+0) 
term(601) = term(601) * (2.0d+0) 
term(602) = term(602) * (2.0d+0) 
term(603) = term(603) * (-4.0d+0) 
term(604) = term(604) * (-4.0d+0) 
term(605) = term(605) * (8.0d+0) 
term(606) = term(606) * (2.0d+0) 
term(607) = term(607) * (-4.0d+0) 
term(608) = term(608) * (2.0d+0) 
term(609) = term(609) * (-4.0d+0) 
term(610) = term(610) * (-4.0d+0) 
term(611) = term(611) * (8.0d+0) 
term(612) = term(612) * (2.0d+0) 
term(613) = term(613) * (-4.0d+0) 
term(614) = term(614) * (-4.0d+0) 
term(615) = term(615) * (8.0d+0) 
term(616) = term(616) * (8.0d+0) 
term(617) = term(617) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(618) = term(618) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,i) * wm_interm_77_pt3(c,a,q,j)
term(619) = term(619) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,i) * wm_interm_78_pt3(c,a,q,j)
term(620) = term(620) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,j) * wm_interm_78_pt3(c,a,q,i)
term(621) = term(621) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,j) * wm_interm_77_pt3(c,a,q,i)
term(622) = term(622) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,i) * wm_interm_80_pt3(c,a,q,j)
term(623) = term(623) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,i) * wm_interm_81_pt3(c,a,q,j)
term(624) = term(624) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,j) * wm_interm_80_pt3(c,a,q,i)
term(625) = term(625) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,c,k,j) * wm_interm_81_pt3(c,a,q,i)
term(626) = term(626) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,i) * wm_interm_77_pt3(c,a,k,j)
term(627) = term(627) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,i) * wm_interm_78_pt3(c,a,k,j)
term(628) = term(628) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,j) * wm_interm_78_pt3(c,a,k,i)
term(629) = term(629) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,j) * wm_interm_77_pt3(c,a,k,i)
term(630) = term(630) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,i) * wm_interm_80_pt3(c,a,k,j)
term(631) = term(631) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,i) * wm_interm_81_pt3(c,a,k,j)
term(632) = term(632) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,j) * wm_interm_80_pt3(c,a,k,i)
term(633) = term(633) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,k,j) * wm_interm_81_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(618) = term(618) * (-4.0d+0) 
term(619) = term(619) * (8.0d+0) 
term(620) = term(620) * (-4.0d+0) 
term(621) = term(621) * (8.0d+0) 
term(622) = term(622) * (8.0d+0) 
term(623) = term(623) * (-16.0d+0) 
term(624) = term(624) * (-4.0d+0) 
term(625) = term(625) * (8.0d+0) 
term(626) = term(626) * (2.0d+0) 
term(627) = term(627) * (-4.0d+0) 
term(628) = term(628) * (2.0d+0) 
term(629) = term(629) * (-4.0d+0) 
term(630) = term(630) * (-4.0d+0) 
term(631) = term(631) * (8.0d+0) 
term(632) = term(632) * (2.0d+0) 
term(633) = term(633) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(634) = term(634) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,i,j) * wm_interm_95_pt3(c,a)
term(635) = term(635) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,i,j) * wm_interm_96_pt3(c,a)
term(636) = term(636) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,j,i) * wm_interm_95_pt3(c,a)
term(637) = term(637) + t3(nocc, nactive, a,b,p,j,q,i) * t2(b,c,j,i) * wm_interm_96_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(634) = term(634) * (-3.9999999999999996d+0) 
term(635) = term(635) * (7.999999999999999d+0) 
term(636) = term(636) * (1.9999999999999998d+0) 
term(637) = term(637) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(638) = term(638) + wm_interm_104_pt3(i,j,k,l) * wm_interm_86_pt3(p,i,j,l,q,k)
term(639) = term(639) + wm_interm_104_pt3(i,j,k,l) * wm_interm_86_pt3(p,i,j,q,l,k)
term(640) = term(640) + wm_interm_104_pt3(i,j,k,l) * wm_interm_86_pt3(p,i,j,l,k,q)
end do 
end do 
end do 
end do 

term(639) = term(639) * (-2.0d+0) 
term(640) = term(640) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(641) = term(641) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,j) * wm_interm_77_pt3(c,a,k,i)
term(642) = term(642) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,j) * wm_interm_78_pt3(c,a,k,i)
term(643) = term(643) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,j) * wm_interm_77_pt3(c,a,k,i)
term(644) = term(644) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,j) * wm_interm_78_pt3(c,a,k,i)
term(645) = term(645) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,i) * wm_interm_78_pt3(c,a,k,j)
term(646) = term(646) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,i) * wm_interm_77_pt3(c,a,k,j)
term(647) = term(647) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,i) * wm_interm_78_pt3(c,a,k,j)
term(648) = term(648) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,i) * wm_interm_77_pt3(c,a,k,j)
term(649) = term(649) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,i) * wm_interm_80_pt3(c,a,k,j)
term(650) = term(650) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,i) * wm_interm_81_pt3(c,a,k,j)
term(651) = term(651) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,i) * wm_interm_80_pt3(c,a,k,j)
term(652) = term(652) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,i) * wm_interm_81_pt3(c,a,k,j)
term(653) = term(653) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,j) * wm_interm_80_pt3(c,a,k,i)
term(654) = term(654) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,k,j) * wm_interm_81_pt3(c,a,k,i)
term(655) = term(655) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,j) * wm_interm_80_pt3(c,a,k,i)
term(656) = term(656) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,k,j) * wm_interm_81_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(641) = term(641) * (1.9999999999999998d+0) 
term(642) = term(642) * (-3.9999999999999996d+0) 
term(643) = term(643) * (-3.9999999999999996d+0) 
term(644) = term(644) * (7.999999999999999d+0) 
term(645) = term(645) * (2.0d+0) 
term(646) = term(646) * (-4.0d+0) 
term(647) = term(647) * (-4.0d+0) 
term(648) = term(648) * (8.0d+0) 
term(649) = term(649) * (2.0d+0) 
term(650) = term(650) * (-4.0d+0) 
term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (8.0d+0) 
term(653) = term(653) * (-3.9999999999999996d+0) 
term(654) = term(654) * (7.999999999999999d+0) 
term(655) = term(655) * (7.999999999999999d+0) 
term(656) = term(656) * (-15.999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(657) = term(657) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,q,j) * wm_interm_99_pt3(a,c)
term(658) = term(658) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,q,j) * wm_interm_100_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(657) = term(657) * (-3.9999999999999996d+0) 
term(658) = term(658) * (1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(659) = term(659) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,q,i) * wm_interm_99_pt3(a,c)
term(660) = term(660) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,q,i) * wm_interm_100_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(659) = term(659) * (7.999999999999999d+0) 
term(660) = term(660) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(661) = term(661) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,j,i) * wm_interm_99_pt3(a,c)
term(662) = term(662) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_99_pt3(a,c)
term(663) = term(663) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,j,i) * wm_interm_100_pt3(a,c)
term(664) = term(664) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_100_pt3(a,c)
term(665) = term(665) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_95_pt3(c,a)
term(666) = term(666) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_96_pt3(c,a)
term(667) = term(667) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,i,j,q) * wm_interm_120_pt3(b,c)
term(668) = term(668) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,i,j,q) * wm_interm_121_pt3(b,c)
term(669) = term(669) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_120_pt3(a,c)
term(670) = term(670) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_121_pt3(a,c)
term(671) = term(671) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_95_pt3(a,b)
term(672) = term(672) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_96_pt3(a,b)
term(673) = term(673) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_95_pt3(a,b)
term(674) = term(674) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_96_pt3(a,b)
term(675) = term(675) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_pt3(b,c)
term(676) = term(676) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_pt3(b,c)
term(677) = term(677) + s2(a,b,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_95_pt3(c,b)
term(678) = term(678) + s2(a,b,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_96_pt3(c,b)
end do 
end do 
end do 
end do 
end do 

term(661) = term(661) * (-3.9999999999999996d+0) 
term(662) = term(662) * (7.999999999999999d+0) 
term(663) = term(663) * (1.9999999999999998d+0) 
term(664) = term(664) * (-3.9999999999999996d+0) 
term(665) = term(665) * (-4.0d+0) 
term(666) = term(666) * (8.0d+0) 
term(667) = term(667) * (4.0d+0) 
term(668) = term(668) * (-8.0d+0) 
term(669) = term(669) * (-2.0d+0) 
term(670) = term(670) * (4.0d+0) 
term(671) = term(671) * (2.0d+0) 
term(672) = term(672) * (-4.0d+0) 
term(673) = term(673) * (-4.0d+0) 
term(674) = term(674) * (8.0d+0) 
term(675) = term(675) * (-4.0d+0) 
term(676) = term(676) * (8.0d+0) 
term(677) = term(677) * (-4.0d+0) 
term(678) = term(678) * (8.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(679) = term(679) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,i,j) * wm_interm_99_pt3(a,c)
term(680) = term(680) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_99_pt3(a,c)
term(681) = term(681) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,i,j) * wm_interm_100_pt3(a,c)
term(682) = term(682) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_100_pt3(a,c)
term(683) = term(683) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_95_pt3(c,a)
term(684) = term(684) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_96_pt3(c,a)
term(685) = term(685) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,i,q) * wm_interm_120_pt3(b,c)
term(686) = term(686) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,i,q) * wm_interm_121_pt3(b,c)
term(687) = term(687) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_120_pt3(a,c)
term(688) = term(688) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_121_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(679) = term(679) * (7.999999999999999d+0) 
term(680) = term(680) * (-15.999999999999998d+0) 
term(681) = term(681) * (-3.9999999999999996d+0) 
term(682) = term(682) * (7.999999999999999d+0) 
term(683) = term(683) * (8.0d+0) 
term(684) = term(684) * (-16.0d+0) 
term(685) = term(685) * (-2.0d+0) 
term(686) = term(686) * (4.0d+0) 
term(687) = term(687) * (4.0d+0) 
term(688) = term(688) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(689) = term(689) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,k,j) * wm_interm_87_pt3(a,c,q,k)
term(690) = term(690) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,k,j) * wm_interm_97_pt3(a,c,q,k)
term(691) = term(691) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,k,j) * wm_interm_98_pt3(a,c,q,k)
term(692) = term(692) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,k,j) * wm_interm_102_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(689) = term(689) * (7.999999999999999d+0) 
term(690) = term(690) * (-3.9999999999999996d+0) 
term(691) = term(691) * (1.9999999999999998d+0) 
term(692) = term(692) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(693) = term(693) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,j,k) * wm_interm_80_pt3(c,a,k,i)
term(694) = term(694) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,j,k) * wm_interm_81_pt3(c,a,k,i)
term(695) = term(695) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,j,k) * wm_interm_80_pt3(c,a,k,i)
term(696) = term(696) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,j,k) * wm_interm_81_pt3(c,a,k,i)
term(697) = term(697) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,j,k) * wm_interm_77_pt3(c,a,k,i)
term(698) = term(698) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,j,k) * wm_interm_78_pt3(c,a,k,i)
term(699) = term(699) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,j,k) * wm_interm_77_pt3(c,a,k,i)
term(700) = term(700) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,j,k) * wm_interm_78_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(693) = term(693) * (7.999999999999999d+0) 
term(694) = term(694) * (-15.999999999999998d+0) 
term(695) = term(695) * (-15.999999999999998d+0) 
term(696) = term(696) * (31.999999999999996d+0) 
term(697) = term(697) * (-3.9999999999999996d+0) 
term(698) = term(698) * (7.999999999999999d+0) 
term(699) = term(699) * (7.999999999999999d+0) 
term(700) = term(700) * (-15.999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(701) = term(701) + s1(a,i) * s2(b,p,q,j) * wm_interm_24_pt3(b,a,j,i)
term(702) = term(702) + s1(a,i) * s2(b,p,q,j) * wm_interm_25_pt3(b,a,j,i)
term(703) = term(703) + s1(a,i) * s2(b,p,q,j) * wm_interm_24_pt3(b,a,i,j)
term(704) = term(704) + s1(a,i) * s2(b,p,q,j) * wm_interm_25_pt3(b,a,i,j)
term(705) = term(705) + s1(a,i) * s2(b,p,j,q) * wm_interm_24_pt3(b,a,i,j)
term(706) = term(706) + s1(a,i) * s2(b,p,j,q) * wm_interm_25_pt3(b,a,i,j)
term(707) = term(707) + s1(a,q) * s2(b,p,j,i) * wm_interm_24_pt3(b,a,i,j)
term(708) = term(708) + s1(a,q) * s2(b,p,j,i) * wm_interm_25_pt3(b,a,i,j)
term(709) = term(709) + t1(a,i) * t2(b,p,j,q) * wm_interm_50_pt3(b,a,i,j)
term(710) = term(710) + t1(a,i) * t2(b,p,j,q) * wm_interm_52_pt3(b,a,i,j)
term(711) = term(711) + t1(a,i) * t2(b,p,j,q) * wm_interm_55_pt3(b,a,i,j)
term(712) = term(712) + t1(a,i) * t2(b,p,j,q) * wm_interm_53_pt3(b,a,i,j)
term(713) = term(713) + t1(a,i) * t2(b,p,j,q) * wm_interm_54_pt3(b,a,i,j)
term(714) = term(714) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_pt3(b,a,i,j)
term(715) = term(715) + t1(b,j) * t2(a,p,q,i) * wm_interm_54_pt3(b,a,i,j)
term(716) = term(716) + t1(b,j) * t2(a,p,q,i) * wm_interm_52_pt3(b,a,i,j)
term(717) = term(717) + t1(b,j) * t2(a,p,q,i) * wm_interm_53_pt3(b,a,i,j)
term(718) = term(718) + t1(b,j) * t2(a,p,q,i) * wm_interm_55_pt3(b,a,i,j)
term(719) = term(719) + t1(a,q) * t2(b,p,j,i) * wm_interm_52_pt3(b,a,i,j)
term(720) = term(720) + t1(a,q) * t2(b,p,j,i) * wm_interm_53_pt3(b,a,i,j)
term(721) = term(721) + t1(a,q) * t2(b,p,j,i) * wm_interm_55_pt3(b,a,i,j)
term(722) = term(722) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_pt3(b,a,i,j)
term(723) = term(723) + t1(a,q) * t2(b,p,j,i) * wm_interm_54_pt3(b,a,i,j)
end do 
end do 
end do 
end do 

term(701) = term(701) * (-3.9999999999999996d+0) 
term(702) = term(702) * (3.9999999999999996d+0) 
term(703) = term(703) * (2.0d+0) 
term(704) = term(704) * (-2.0d+0) 
term(705) = term(705) * (-4.0d+0) 
term(706) = term(706) * (4.0d+0) 
term(707) = term(707) * (3.9999999999999996d+0) 
term(708) = term(708) * (-4.0d+0) 
term(709) = term(709) * (1.3333333333333333d+0) 
term(710) = term(710) * (-4.0d+0) 
term(711) = term(711) * (-1.3333333333333333d+0) 
term(712) = term(712) * (1.3333333333333333d+0) 
term(713) = term(713) * (-1.3333333333333333d+0) 
term(714) = term(714) * (-0.6666666666666666d+0) 
term(715) = term(715) * (0.6666666666666666d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (-0.6666666666666666d+0) 
term(718) = term(718) * (0.6666666666666666d+0) 
term(719) = term(719) * (2.0d+0) 
term(720) = term(720) * (-0.6666666666666666d+0) 
term(721) = term(721) * (0.6666666666666666d+0) 
term(722) = term(722) * (-0.6666666666666666d+0) 
term(723) = term(723) * (0.6666666666666666d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(724) = term(724) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,j,i) * wm_interm_95_pt3(c,a)
term(725) = term(725) + t3(nocc, nactive, a,b,p,q,j,i) * t2(b,c,j,i) * wm_interm_96_pt3(c,a)
term(726) = term(726) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,j,i) * wm_interm_95_pt3(c,a)
term(727) = term(727) + t3(nocc, nactive, a,b,p,i,j,q) * t2(b,c,j,i) * wm_interm_96_pt3(c,a)
term(728) = term(728) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_120_pt3(c,a)
term(729) = term(729) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_121_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(724) = term(724) * (-4.0d+0) 
term(725) = term(725) * (8.0d+0) 
term(726) = term(726) * (8.0d+0) 
term(727) = term(727) * (-16.0d+0) 
term(728) = term(728) * (-2.0d+0) 
term(729) = term(729) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(730) = term(730) + s1(a,i) * s2(b,p,j,q) * wm_interm_24_pt3(b,a,j,i)
term(731) = term(731) + s1(a,i) * s2(b,p,j,q) * wm_interm_25_pt3(b,a,j,i)
term(732) = term(732) + s1(a,q) * s2(b,p,j,i) * wm_interm_24_pt3(b,a,j,i)
term(733) = term(733) + s1(a,q) * s2(b,p,j,i) * wm_interm_25_pt3(b,a,j,i)
term(734) = term(734) + t1(a,i) * t2(b,p,j,q) * wm_interm_50_pt3(b,a,j,i)
term(735) = term(735) + t1(a,i) * t2(b,p,j,q) * wm_interm_52_pt3(b,a,j,i)
term(736) = term(736) + t1(a,i) * t2(b,p,j,q) * wm_interm_53_pt3(b,a,j,i)
term(737) = term(737) + t1(a,i) * t2(b,p,j,q) * wm_interm_54_pt3(b,a,j,i)
term(738) = term(738) + t1(a,i) * t2(b,p,j,q) * wm_interm_55_pt3(b,a,j,i)
term(739) = term(739) + t1(b,j) * t2(a,p,q,i) * wm_interm_52_pt3(b,a,j,i)
term(740) = term(740) + t1(b,j) * t2(a,p,q,i) * wm_interm_54_pt3(b,a,j,i)
term(741) = term(741) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_pt3(b,a,j,i)
term(742) = term(742) + t1(b,j) * t2(a,p,q,i) * wm_interm_55_pt3(b,a,j,i)
term(743) = term(743) + t1(b,j) * t2(a,p,q,i) * wm_interm_53_pt3(b,a,j,i)
term(744) = term(744) + t1(a,q) * t2(b,p,j,i) * wm_interm_55_pt3(b,a,j,i)
term(745) = term(745) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_pt3(b,a,j,i)
term(746) = term(746) + t1(a,q) * t2(b,p,j,i) * wm_interm_53_pt3(b,a,j,i)
term(747) = term(747) + t1(a,q) * t2(b,p,j,i) * wm_interm_52_pt3(b,a,j,i)
term(748) = term(748) + t1(a,q) * t2(b,p,j,i) * wm_interm_54_pt3(b,a,j,i)
end do 
end do 
end do 
end do 

term(730) = term(730) * (7.999999999999999d+0) 
term(731) = term(731) * (-7.999999999999999d+0) 
term(732) = term(732) * (-7.999999999999999d+0) 
term(733) = term(733) * (8.0d+0) 
term(734) = term(734) * (-2.6666666666666665d+0) 
term(735) = term(735) * (8.0d+0) 
term(736) = term(736) * (-2.6666666666666665d+0) 
term(737) = term(737) * (0.6666666666666666d+0) 
term(738) = term(738) * (0.6666666666666666d+0) 
term(739) = term(739) * (-4.0d+0) 
term(740) = term(740) * (-0.3333333333333333d+0) 
term(741) = term(741) * (1.3333333333333333d+0) 
term(742) = term(742) * (-0.3333333333333333d+0) 
term(743) = term(743) * (1.3333333333333333d+0) 
term(744) = term(744) * (-0.3333333333333333d+0) 
term(745) = term(745) * (1.3333333333333333d+0) 
term(746) = term(746) * (1.3333333333333333d+0) 
term(747) = term(747) * (-4.0d+0) 
term(748) = term(748) * (-0.3333333333333333d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(749) = term(749) + s1(a,i) * s2(b,p,q,j) * wm_interm_24_pt3(a,b,i,j)
term(750) = term(750) + s1(a,i) * s2(b,p,q,j) * wm_interm_24_pt3(a,b,j,i)
term(751) = term(751) + s1(a,i) * s2(b,p,j,q) * wm_interm_24_pt3(a,b,i,j)
term(752) = term(752) + s1(a,i) * s2(b,p,q,j) * wm_interm_25_pt3(a,b,j,i)
term(753) = term(753) + s1(a,i) * s2(b,p,q,j) * wm_interm_25_pt3(a,b,i,j)
term(754) = term(754) + s1(a,i) * s2(b,p,j,q) * wm_interm_25_pt3(a,b,i,j)
term(755) = term(755) + s1(a,q) * s2(b,p,j,i) * wm_interm_24_pt3(a,b,i,j)
term(756) = term(756) + s1(a,q) * s2(b,p,j,i) * wm_interm_25_pt3(a,b,i,j)
term(757) = term(757) + t1(a,i) * t2(b,p,j,q) * wm_interm_52_pt3(a,b,i,j)
term(758) = term(758) + t1(a,i) * t2(b,p,j,q) * wm_interm_54_pt3(a,b,i,j)
term(759) = term(759) + t1(a,i) * t2(b,p,j,q) * wm_interm_50_pt3(a,b,i,j)
term(760) = term(760) + t1(a,i) * t2(b,p,j,q) * wm_interm_55_pt3(a,b,i,j)
term(761) = term(761) + t1(a,i) * t2(b,p,j,q) * wm_interm_53_pt3(a,b,i,j)
term(762) = term(762) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_pt3(a,b,i,j)
term(763) = term(763) + t1(b,j) * t2(a,p,q,i) * wm_interm_52_pt3(a,b,i,j)
term(764) = term(764) + t1(b,j) * t2(a,p,q,i) * wm_interm_53_pt3(a,b,i,j)
term(765) = term(765) + t1(b,j) * t2(a,p,q,i) * wm_interm_54_pt3(a,b,i,j)
term(766) = term(766) + t1(b,j) * t2(a,p,q,i) * wm_interm_55_pt3(a,b,i,j)
term(767) = term(767) + t1(a,q) * t2(b,p,j,i) * wm_interm_55_pt3(a,b,i,j)
term(768) = term(768) + t1(a,q) * t2(b,p,j,i) * wm_interm_53_pt3(a,b,i,j)
term(769) = term(769) + t1(a,q) * t2(b,p,j,i) * wm_interm_52_pt3(a,b,i,j)
term(770) = term(770) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_pt3(a,b,i,j)
term(771) = term(771) + t1(a,q) * t2(b,p,j,i) * wm_interm_54_pt3(a,b,i,j)
end do 
end do 
end do 
end do 

term(749) = term(749) * (-4.0d+0) 
term(750) = term(750) * (2.0d+0) 
term(751) = term(751) * (8.0d+0) 
term(752) = term(752) * (-2.0d+0) 
term(753) = term(753) * (4.0d+0) 
term(754) = term(754) * (-8.0d+0) 
term(755) = term(755) * (-7.999999999999999d+0) 
term(756) = term(756) * (8.0d+0) 
term(757) = term(757) * (8.0d+0) 
term(758) = term(758) * (0.6666666666666666d+0) 
term(759) = term(759) * (-2.6666666666666665d+0) 
term(760) = term(760) * (0.6666666666666666d+0) 
term(761) = term(761) * (-2.6666666666666665d+0) 
term(762) = term(762) * (1.3333333333333333d+0) 
term(763) = term(763) * (-4.0d+0) 
term(764) = term(764) * (1.3333333333333333d+0) 
term(765) = term(765) * (-0.3333333333333333d+0) 
term(766) = term(766) * (-0.3333333333333333d+0) 
term(767) = term(767) * (-0.3333333333333333d+0) 
term(768) = term(768) * (1.3333333333333333d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * (1.3333333333333333d+0) 
term(771) = term(771) * (-0.3333333333333333d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(772) = term(772) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,k,j) * wm_interm_84_pt3(b,i,j,k)
term(773) = term(773) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,k,j) * wm_interm_85_pt3(b,i,j,k)
term(774) = term(774) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,k,j) * wm_interm_88_pt3(b,i,j,k)
term(775) = term(775) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_32_pt3(b,i,j,k)
term(776) = term(776) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_36_pt3(b,i,j,k)
term(777) = term(777) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_106_pt3(b,i,j,k)
term(778) = term(778) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_107_pt3(b,i,j,k)
term(779) = term(779) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_32_pt3(b,i,k,j)
term(780) = term(780) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_36_pt3(b,i,k,j)
term(781) = term(781) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_114_pt3(b,i,j,k)
term(782) = term(782) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_38_pt3(b,i,j,k)
term(783) = term(783) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_40_pt3(b,i,j,k)
term(784) = term(784) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_117_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(772) = term(772) * (2.0d+0) 
term(773) = term(773) * (-4.0d+0) 
term(774) = term(774) * (1.9999999999999998d+0) 
term(775) = term(775) * (-2.0d+0) 
term(776) = term(776) * (4.0d+0) 
term(777) = term(777) * (2.0d+0) 
term(778) = term(778) * (-4.0d+0) 
term(779) = term(779) * (-2.0d+0) 
term(780) = term(780) * (4.0d+0) 
term(783) = term(783) * (-2.0d+0) 
term(784) = term(784) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(785) = term(785) + s1(a,i) * s2(b,p,j,q) * wm_interm_24_pt3(a,b,j,i)
term(786) = term(786) + s1(a,i) * s2(b,p,j,q) * wm_interm_25_pt3(a,b,j,i)
term(787) = term(787) + s1(a,q) * s2(b,p,j,i) * wm_interm_25_pt3(a,b,j,i)
term(788) = term(788) + s1(a,q) * s2(b,p,j,i) * wm_interm_24_pt3(a,b,j,i)
term(789) = term(789) + t1(a,i) * t2(b,p,j,q) * wm_interm_50_pt3(a,b,j,i)
term(790) = term(790) + t1(a,i) * t2(b,p,j,q) * wm_interm_54_pt3(a,b,j,i)
term(791) = term(791) + t1(a,i) * t2(b,p,j,q) * wm_interm_52_pt3(a,b,j,i)
term(792) = term(792) + t1(a,i) * t2(b,p,j,q) * wm_interm_53_pt3(a,b,j,i)
term(793) = term(793) + t1(a,i) * t2(b,p,j,q) * wm_interm_55_pt3(a,b,j,i)
term(794) = term(794) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_pt3(a,b,j,i)
term(795) = term(795) + t1(b,j) * t2(a,p,q,i) * wm_interm_52_pt3(a,b,j,i)
term(796) = term(796) + t1(b,j) * t2(a,p,q,i) * wm_interm_55_pt3(a,b,j,i)
term(797) = term(797) + t1(b,j) * t2(a,p,q,i) * wm_interm_53_pt3(a,b,j,i)
term(798) = term(798) + t1(b,j) * t2(a,p,q,i) * wm_interm_54_pt3(a,b,j,i)
term(799) = term(799) + t1(a,q) * t2(b,p,j,i) * wm_interm_53_pt3(a,b,j,i)
term(800) = term(800) + t1(a,q) * t2(b,p,j,i) * wm_interm_52_pt3(a,b,j,i)
term(801) = term(801) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_pt3(a,b,j,i)
term(802) = term(802) + t1(a,q) * t2(b,p,j,i) * wm_interm_54_pt3(a,b,j,i)
term(803) = term(803) + t1(a,q) * t2(b,p,j,i) * wm_interm_55_pt3(a,b,j,i)
end do 
end do 
end do 
end do 

term(785) = term(785) * (-4.0d+0) 
term(786) = term(786) * (4.0d+0) 
term(787) = term(787) * (-4.0d+0) 
term(788) = term(788) * (3.9999999999999996d+0) 
term(789) = term(789) * (1.3333333333333333d+0) 
term(790) = term(790) * (-1.3333333333333333d+0) 
term(791) = term(791) * (-4.0d+0) 
term(792) = term(792) * (1.3333333333333333d+0) 
term(793) = term(793) * (-1.3333333333333333d+0) 
term(794) = term(794) * (-0.6666666666666666d+0) 
term(795) = term(795) * (2.0d+0) 
term(796) = term(796) * (0.6666666666666666d+0) 
term(797) = term(797) * (-0.6666666666666666d+0) 
term(798) = term(798) * (0.6666666666666666d+0) 
term(799) = term(799) * (-0.6666666666666666d+0) 
term(800) = term(800) * (2.0d+0) 
term(801) = term(801) * (-0.6666666666666666d+0) 
term(802) = term(802) * (0.6666666666666666d+0) 
term(803) = term(803) * (0.6666666666666666d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(804) = term(804) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_pt3(b,c)
term(805) = term(805) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_pt3(b,c)
term(806) = term(806) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_95_pt3(c,b)
term(807) = term(807) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_96_pt3(c,b)
end do 
end do 
end do 
end do 
end do 

term(804) = term(804) * (2.0d+0) 
term(805) = term(805) * (-4.0d+0) 
term(806) = term(806) * (2.0d+0) 
term(807) = term(807) * (-4.0d+0) 

do i = 1, nocc 
term(808) = term(808) + wm_interm_12_pt3(i,q) * wm_interm_20_pt3(p,i)
term(809) = term(809) + wm_interm_12_pt3(i,q) * wm_interm_21_pt3(p,i)
term(810) = term(810) + wm_interm_12_pt3(i,q) * wm_interm_22_pt3(p,i)
term(811) = term(811) + wm_interm_12_pt3(i,q) * wm_interm_23_pt3(p,i)
term(812) = term(812) + wm_interm_103_pt3(q,i) * wm_interm_93_pt3(p,i)
term(813) = term(813) + wm_interm_103_pt3(q,i) * wm_interm_94_pt3(p,i)
term(814) = term(814) + wm_interm_103_pt3(q,i) * wm_interm_89_pt3(p,i)
term(815) = term(815) + wm_interm_103_pt3(q,i) * wm_interm_90_pt3(p,i)
term(816) = term(816) + wm_interm_103_pt3(q,i) * wm_interm_91_pt3(p,i)
term(817) = term(817) + wm_interm_103_pt3(q,i) * wm_interm_92_pt3(p,i)
term(818) = term(818) + wm_interm_105_pt3(q,i) * wm_interm_93_pt3(p,i)
term(819) = term(819) + wm_interm_105_pt3(q,i) * wm_interm_94_pt3(p,i)
term(820) = term(820) + wm_interm_105_pt3(q,i) * wm_interm_89_pt3(p,i)
term(821) = term(821) + wm_interm_105_pt3(q,i) * wm_interm_90_pt3(p,i)
term(822) = term(822) + wm_interm_105_pt3(q,i) * wm_interm_91_pt3(p,i)
term(823) = term(823) + wm_interm_105_pt3(q,i) * wm_interm_92_pt3(p,i)
end do 

term(808) = term(808) * (-4.0d+0) 
term(809) = term(809) * (4.0d+0) 
term(810) = term(810) * (2.0d+0) 
term(811) = term(811) * (-2.0d+0) 
term(812) = term(812) * (-1.9999999999999998d+0) 
term(813) = term(813) * (3.9999999999999996d+0) 
term(815) = term(815) * (-1.9999999999999998d+0) 
term(817) = term(817) * (-1.9999999999999998d+0) 
term(818) = term(818) * (3.9999999999999996d+0) 
term(819) = term(819) * (-7.999999999999999d+0) 
term(820) = term(820) * (-1.9999999999999998d+0) 
term(821) = term(821) * (3.9999999999999996d+0) 
term(822) = term(822) * (-1.9999999999999998d+0) 
term(823) = term(823) * (3.9999999999999996d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(824) = term(824) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,j,i,q,k,l)
term(825) = term(825) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,i,j,q,k,l)
term(826) = term(826) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,i,j,k,q,l)
term(827) = term(827) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,j,i,k,q,l)
term(828) = term(828) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,j,i,k,l,q)
term(829) = term(829) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,i,j,k,l,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(825) = term(825) * (-2.0d+0) 
term(827) = term(827) * (-2.0d+0) 
term(829) = term(829) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(830) = term(830) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,l,p,i) * wm_interm_119_pt3(a,l,i,q,j,k)
term(831) = term(831) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,l,p,i) * wm_interm_119_pt3(a,i,l,q,j,k)
term(832) = term(832) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,l,p,i) * wm_interm_119_pt3(b,i,l,q,j,k)
term(833) = term(833) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,l,p,i) * wm_interm_119_pt3(b,l,i,q,j,k)
term(834) = term(834) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,l,p,i) * wm_interm_119_pt3(a,i,l,j,k,q)
term(835) = term(835) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,l,p,i) * wm_interm_119_pt3(a,l,i,j,k,q)
term(836) = term(836) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,l,p,i) * wm_interm_119_pt3(b,i,l,j,k,q)
term(837) = term(837) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,l,p,i) * wm_interm_119_pt3(b,l,i,j,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(831) = term(831) * (-1.9999999999999998d+0) 
term(833) = term(833) * (-1.9999999999999998d+0) 
term(834) = term(834) * (-2.0d+0) 
term(835) = term(835) * (4.0d+0) 
term(837) = term(837) * (-2.0d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(838) = term(838) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,q) * wm_interm_80_pt3(c,a,k,i)
term(839) = term(839) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,q) * wm_interm_81_pt3(c,a,k,i)
term(840) = term(840) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,q) * wm_interm_77_pt3(c,a,k,i)
term(841) = term(841) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,q) * wm_interm_78_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(838) = term(838) * (8.0d+0) 
term(839) = term(839) * (-16.0d+0) 
term(840) = term(840) * (-4.0d+0) 
term(841) = term(841) * (8.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(842) = term(842) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,j,i,q,l,k)
term(843) = term(843) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,i,j,q,l,k)
term(844) = term(844) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,i,j,l,q,k)
term(845) = term(845) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,j,i,l,q,k)
term(846) = term(846) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,i,j,l,k,q)
term(847) = term(847) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_118_pt3(b,j,i,l,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(842) = term(842) * (-2.0d+0) 
term(843) = term(843) * (4.0d+0) 
term(844) = term(844) * (-2.0d+0) 
term(845) = term(845) * (4.0d+0) 
term(847) = term(847) * (-2.0d+0) 


    calc_D_vo_wm_cc3_pt3 = zero
    do s = 0, 847
    calc_D_vo_wm_cc3_pt3 = calc_D_vo_wm_cc3_pt3 + term(s)
    end do

    end function calc_D_vo_wm_cc3_pt3
    
    function calc_D_vv_wm_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, b, l, a 
    real(F64), dimension(0:507) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_128_pt3(p,i,j,k) * wm_interm_2_pt3(q,k,j,i)
term(1) = term(1) + wm_interm_128_pt3(p,i,j,k) * wm_interm_2_pt3(q,j,k,i)
term(2) = term(2) + wm_interm_128_pt3(p,i,j,k) * wm_interm_1_pt3(q,j,k,i)
term(3) = term(3) + wm_interm_128_pt3(p,i,j,k) * wm_interm_1_pt3(q,k,j,i)
term(4) = term(4) + wm_interm_128_pt3(p,i,j,k) * wm_interm_3_pt3(q,j,k,i)
term(5) = term(5) + wm_interm_128_pt3(p,i,j,k) * wm_interm_3_pt3(q,k,j,i)
term(6) = term(6) + wm_interm_11_pt3(q,i,j,k) * wm_interm_136_pt3(p,k,j,i)
term(7) = term(7) + wm_interm_11_pt3(q,i,j,k) * wm_interm_137_pt3(p,k,j,i)
term(8) = term(8) + wm_interm_11_pt3(q,i,j,k) * wm_interm_138_pt3(p,k,j,i)
term(9) = term(9) + wm_interm_11_pt3(q,i,j,k) * wm_interm_141_pt3(p,k,j,i)
term(10) = term(10) + wm_interm_146_pt3(p,i,j,k) * wm_interm_41_pt3(q,k,j,i)
term(11) = term(11) + wm_interm_143_pt3(p,i,j,k) * wm_interm_41_pt3(q,k,j,i)
term(12) = term(12) + wm_interm_146_pt3(p,i,j,k) * wm_interm_47_pt3(q,k,j,i)
term(13) = term(13) + wm_interm_143_pt3(p,i,j,k) * wm_interm_47_pt3(q,k,j,i)
term(14) = term(14) + wm_interm_161_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,j,i)
term(15) = term(15) + wm_interm_162_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,j,i)
term(16) = term(16) + wm_interm_158_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,j,i)
term(17) = term(17) + wm_interm_157_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,j,i)
term(18) = term(18) + wm_interm_153_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,j,i)
term(19) = term(19) + wm_interm_154_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * (3.9999999999999996d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-8.0d+0) 
term(6) = term(6) * (-0.9999999999999999d+0) 
term(7) = term(7) * (3.9999999999999996d+0) 
term(8) = term(8) * (-7.999999999999999d+0) 
term(9) = term(9) * (1.9999999999999998d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (-3.9999999999999996d+0) 
term(13) = term(13) * (1.9999999999999998d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,j,k) * wm_interm_9_pt3(b,k)
term(21) = term(21) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,j,k) * wm_interm_10_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_89_pt3(a,j)
term(23) = term(23) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_90_pt3(a,j)
term(24) = term(24) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_91_pt3(a,j)
term(25) = term(25) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_92_pt3(a,j)
term(26) = term(26) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_93_pt3(a,j)
term(27) = term(27) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_94_pt3(a,j)
term(28) = term(28) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,j)
term(29) = term(29) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_37_pt3(a,j)
term(30) = term(30) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,j)
term(31) = term(31) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_39_pt3(a,j)
term(32) = term(32) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_108_pt3(a,j)
term(33) = term(33) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_113_pt3(a,j)
term(34) = term(34) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_115_pt3(a,j)
term(35) = term(35) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_116_pt3(a,j)
term(36) = term(36) + s2(a,p,i,j) * t1(q,i) * wm_interm_147_pt3(a,j)
term(37) = term(37) + s2(a,p,i,j) * t1(q,i) * wm_interm_148_pt3(a,j)
term(38) = term(38) + s2(a,p,i,j) * t1(q,i) * wm_interm_149_pt3(a,j)
term(39) = term(39) + s2(a,p,i,j) * t1(q,i) * wm_interm_150_pt3(a,j)
term(40) = term(40) + s1(p,i) * t2(a,q,i,j) * wm_interm_159_pt3(a,j)
term(41) = term(41) + s1(p,i) * t2(a,q,i,j) * wm_interm_160_pt3(a,j)
term(42) = term(42) + s1(p,i) * t2(a,q,i,j) * wm_interm_155_pt3(a,j)
term(43) = term(43) + s1(p,i) * t2(a,q,i,j) * wm_interm_156_pt3(a,j)
end do 
end do 
end do 

term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (1.9999999999999998d+0) 
term(27) = term(27) * (-3.9999999999999996d+0) 
term(28) = term(28) * (1.9999999999999998d+0) 
term(29) = term(29) * (-3.9999999999999996d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-1.9999999999999998d+0) 
term(37) = term(37) * (3.9999999999999996d+0) 
term(38) = term(38) * (1.9999999999999998d+0) 
term(39) = term(39) * (-3.9999999999999996d+0) 
term(40) = term(40) * (-1.9999999999999998d+0) 
term(41) = term(41) * (3.9999999999999996d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(44) = term(44) + wm_interm_5_pt3(q,i,j,k) * wm_interm_84_pt3(p,i,k,j)
term(45) = term(45) + wm_interm_5_pt3(q,i,j,k) * wm_interm_85_pt3(p,i,k,j)
term(46) = term(46) + wm_interm_5_pt3(q,i,j,k) * wm_interm_88_pt3(p,i,k,j)
term(47) = term(47) + wm_interm_5_pt3(q,i,j,k) * wm_interm_88_pt3(p,i,j,k)
term(48) = term(48) + wm_interm_5_pt3(q,i,j,k) * wm_interm_84_pt3(p,i,j,k)
term(49) = term(49) + wm_interm_5_pt3(q,i,j,k) * wm_interm_85_pt3(p,i,j,k)
term(50) = term(50) + wm_interm_11_pt3(q,i,j,k) * wm_interm_136_pt3(p,k,i,j)
term(51) = term(51) + wm_interm_11_pt3(q,i,j,k) * wm_interm_137_pt3(p,k,i,j)
term(52) = term(52) + wm_interm_11_pt3(q,i,j,k) * wm_interm_138_pt3(p,k,i,j)
term(53) = term(53) + wm_interm_139_pt3(q,i,j,k) * wm_interm_13_pt3(p,j,i,k)
term(54) = term(54) + wm_interm_139_pt3(q,i,j,k) * wm_interm_14_pt3(p,j,i,k)
term(55) = term(55) + wm_interm_139_pt3(q,i,j,k) * wm_interm_16_pt3(p,i,j,k)
term(56) = term(56) + wm_interm_139_pt3(q,i,j,k) * wm_interm_15_pt3(p,i,j,k)
term(57) = term(57) + wm_interm_139_pt3(q,i,j,k) * wm_interm_13_pt3(p,i,j,k)
term(58) = term(58) + wm_interm_139_pt3(q,i,j,k) * wm_interm_14_pt3(p,i,j,k)
term(59) = term(59) + wm_interm_11_pt3(q,i,j,k) * wm_interm_141_pt3(p,k,i,j)
term(60) = term(60) + wm_interm_13_pt3(p,i,j,k) * wm_interm_142_pt3(q,i,j,k)
term(61) = term(61) + wm_interm_142_pt3(q,i,j,k) * wm_interm_14_pt3(p,i,j,k)
term(62) = term(62) + wm_interm_142_pt3(q,i,j,k) * wm_interm_15_pt3(p,i,j,k)
term(63) = term(63) + wm_interm_142_pt3(q,i,j,k) * wm_interm_16_pt3(p,i,j,k)
term(64) = term(64) + wm_interm_13_pt3(p,i,j,k) * wm_interm_142_pt3(q,j,i,k)
term(65) = term(65) + wm_interm_142_pt3(q,i,j,k) * wm_interm_14_pt3(p,j,i,k)
term(66) = term(66) + wm_interm_146_pt3(p,i,j,k) * wm_interm_41_pt3(q,k,i,j)
term(67) = term(67) + wm_interm_143_pt3(p,i,j,k) * wm_interm_41_pt3(q,k,i,j)
term(68) = term(68) + wm_interm_146_pt3(p,i,j,k) * wm_interm_38_pt3(q,k,i,j)
term(69) = term(69) + wm_interm_146_pt3(p,i,j,k) * wm_interm_40_pt3(q,k,i,j)
term(70) = term(70) + wm_interm_146_pt3(p,i,j,k) * wm_interm_47_pt3(q,k,i,j)
term(71) = term(71) + wm_interm_143_pt3(p,i,j,k) * wm_interm_40_pt3(q,k,i,j)
term(72) = term(72) + wm_interm_143_pt3(p,i,j,k) * wm_interm_38_pt3(q,k,i,j)
term(73) = term(73) + wm_interm_143_pt3(p,i,j,k) * wm_interm_47_pt3(q,k,i,j)
term(74) = term(74) + wm_interm_114_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,k,j)
term(75) = term(75) + wm_interm_117_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,k,j)
term(76) = term(76) + wm_interm_117_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,j,k)
term(77) = term(77) + wm_interm_114_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,j,k)
term(78) = term(78) + wm_interm_106_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,k,j)
term(79) = term(79) + wm_interm_107_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,k,j)
term(80) = term(80) + wm_interm_106_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,j,k)
term(81) = term(81) + wm_interm_107_pt3(q,i,j,k) * wm_interm_44_pt3(p,i,j,k)
term(82) = term(82) + wm_interm_166_pt3(p,i,j,k) * wm_interm_58_pt3(q,j,i,k)
term(83) = term(83) + wm_interm_166_pt3(p,i,j,k) * wm_interm_58_pt3(q,i,j,k)
term(84) = term(84) + wm_interm_166_pt3(p,i,j,k) * wm_interm_61_pt3(q,i,j,k)
term(85) = term(85) + wm_interm_166_pt3(p,i,j,k) * wm_interm_61_pt3(q,j,i,k)
term(86) = term(86) + wm_interm_166_pt3(p,i,j,k) * wm_interm_62_pt3(q,j,i,k)
term(87) = term(87) + wm_interm_166_pt3(p,i,j,k) * wm_interm_62_pt3(q,i,j,k)
term(88) = term(88) + wm_interm_166_pt3(p,i,j,k) * wm_interm_63_pt3(q,j,i,k)
term(89) = term(89) + wm_interm_166_pt3(p,i,j,k) * wm_interm_63_pt3(q,i,j,k)
term(90) = term(90) + wm_interm_166_pt3(p,i,j,k) * wm_interm_64_pt3(q,i,j,k)
term(91) = term(91) + wm_interm_166_pt3(p,i,j,k) * wm_interm_64_pt3(q,j,i,k)
term(92) = term(92) + wm_interm_166_pt3(p,i,j,k) * wm_interm_65_pt3(q,i,j,k)
term(93) = term(93) + wm_interm_166_pt3(p,i,j,k) * wm_interm_65_pt3(q,j,i,k)
term(94) = term(94) + wm_interm_161_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,i,j)
term(95) = term(95) + wm_interm_162_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,i,j)
term(96) = term(96) + wm_interm_157_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,i,j)
term(97) = term(97) + wm_interm_158_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,i,j)
term(98) = term(98) + wm_interm_153_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,i,j)
term(99) = term(99) + wm_interm_154_pt3(p,i,j,k) * wm_interm_170_pt3(q,k,i,j)
end do 
end do 
end do 

term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-1.9999999999999998d+0) 
term(47) = term(47) * (3.9999999999999996d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * (1.9999999999999998d+0) 
term(51) = term(51) * (-1.9999999999999998d+0) 
term(52) = term(52) * (3.9999999999999996d+0) 
term(53) = term(53) * (-0.9999999999999999d+0) 
term(54) = term(54) * (1.9999999999999998d+0) 
term(55) = term(55) * (-0.9999999999999999d+0) 
term(56) = term(56) * (1.9999999999999998d+0) 
term(57) = term(57) * (1.9999999999999998d+0) 
term(58) = term(58) * (-3.9999999999999996d+0) 
term(59) = term(59) * (-0.9999999999999999d+0) 
term(60) = term(60) * (-0.9999999999999999d+0) 
term(61) = term(61) * (1.9999999999999998d+0) 
term(62) = term(62) * (-0.9999999999999999d+0) 
term(63) = term(63) * (1.9999999999999998d+0) 
term(64) = term(64) * (1.9999999999999998d+0) 
term(65) = term(65) * (-3.9999999999999996d+0) 
term(66) = term(66) * (-1.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (1.9999999999999998d+0) 
term(71) = term(71) * (-1.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-3.9999999999999996d+0) 
term(74) = term(74) * (-1.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-1.0d+0) 
term(77) = term(77) * (2.0d+0) 
term(78) = term(78) * (-1.9999999999999998d+0) 
term(79) = term(79) * (3.9999999999999996d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * (-1.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-1.0d+0) 
term(93) = term(93) * (2.0d+0) 
term(94) = term(94) * (-1.0d+0) 
term(95) = term(95) * (2.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(100) = term(100) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_9_pt3(b,k)
term(101) = term(101) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_10_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * (4.0d+0) 

do i = 1, nocc 
term(102) = term(102) + wm_interm_10_pt3(q,i) * wm_interm_93_pt3(p,i)
term(103) = term(103) + wm_interm_10_pt3(q,i) * wm_interm_94_pt3(p,i)
term(104) = term(104) + wm_interm_10_pt3(q,i) * wm_interm_89_pt3(p,i)
term(105) = term(105) + wm_interm_10_pt3(q,i) * wm_interm_90_pt3(p,i)
term(106) = term(106) + wm_interm_10_pt3(q,i) * wm_interm_91_pt3(p,i)
term(107) = term(107) + wm_interm_10_pt3(q,i) * wm_interm_92_pt3(p,i)
term(108) = term(108) + wm_interm_89_pt3(p,i) * wm_interm_9_pt3(q,i)
term(109) = term(109) + wm_interm_90_pt3(p,i) * wm_interm_9_pt3(q,i)
term(110) = term(110) + wm_interm_91_pt3(p,i) * wm_interm_9_pt3(q,i)
term(111) = term(111) + wm_interm_92_pt3(p,i) * wm_interm_9_pt3(q,i)
term(112) = term(112) + wm_interm_93_pt3(p,i) * wm_interm_9_pt3(q,i)
term(113) = term(113) + wm_interm_94_pt3(p,i) * wm_interm_9_pt3(q,i)
term(114) = term(114) + wm_interm_133_pt3(q,i) * wm_interm_20_pt3(p,i)
term(115) = term(115) + wm_interm_133_pt3(q,i) * wm_interm_21_pt3(p,i)
term(116) = term(116) + wm_interm_133_pt3(q,i) * wm_interm_22_pt3(p,i)
term(117) = term(117) + wm_interm_133_pt3(q,i) * wm_interm_23_pt3(p,i)
term(118) = term(118) + wm_interm_134_pt3(q,i) * wm_interm_20_pt3(p,i)
term(119) = term(119) + wm_interm_134_pt3(q,i) * wm_interm_21_pt3(p,i)
term(120) = term(120) + wm_interm_134_pt3(q,i) * wm_interm_22_pt3(p,i)
term(121) = term(121) + wm_interm_134_pt3(q,i) * wm_interm_23_pt3(p,i)
term(122) = term(122) + wm_interm_144_pt3(p,i) * wm_interm_35_pt3(q,i)
term(123) = term(123) + wm_interm_145_pt3(p,i) * wm_interm_35_pt3(q,i)
term(124) = term(124) + wm_interm_145_pt3(p,i) * wm_interm_34_pt3(q,i)
term(125) = term(125) + wm_interm_144_pt3(p,i) * wm_interm_34_pt3(q,i)
term(126) = term(126) + wm_interm_144_pt3(p,i) * wm_interm_39_pt3(q,i)
term(127) = term(127) + wm_interm_145_pt3(p,i) * wm_interm_39_pt3(q,i)
term(128) = term(128) + wm_interm_145_pt3(p,i) * wm_interm_37_pt3(q,i)
term(129) = term(129) + wm_interm_144_pt3(p,i) * wm_interm_37_pt3(q,i)
term(130) = term(130) + wm_interm_115_pt3(q,i) * wm_interm_49_pt3(p,i)
term(131) = term(131) + wm_interm_116_pt3(q,i) * wm_interm_49_pt3(p,i)
term(132) = term(132) + wm_interm_115_pt3(q,i) * wm_interm_46_pt3(p,i)
term(133) = term(133) + wm_interm_116_pt3(q,i) * wm_interm_46_pt3(p,i)
term(134) = term(134) + wm_interm_113_pt3(q,i) * wm_interm_46_pt3(p,i)
term(135) = term(135) + wm_interm_108_pt3(q,i) * wm_interm_46_pt3(p,i)
term(136) = term(136) + wm_interm_113_pt3(q,i) * wm_interm_49_pt3(p,i)
term(137) = term(137) + wm_interm_108_pt3(q,i) * wm_interm_49_pt3(p,i)
term(138) = term(138) + wm_interm_151_pt3(p,i) * wm_interm_66_pt3(q,i)
term(139) = term(139) + wm_interm_151_pt3(p,i) * wm_interm_68_pt3(q,i)
term(140) = term(140) + wm_interm_152_pt3(p,i) * wm_interm_66_pt3(q,i)
term(141) = term(141) + wm_interm_152_pt3(p,i) * wm_interm_68_pt3(q,i)
term(142) = term(142) + wm_interm_151_pt3(p,i) * wm_interm_69_pt3(q,i)
term(143) = term(143) + wm_interm_151_pt3(p,i) * wm_interm_70_pt3(q,i)
term(144) = term(144) + wm_interm_152_pt3(p,i) * wm_interm_69_pt3(q,i)
term(145) = term(145) + wm_interm_152_pt3(p,i) * wm_interm_70_pt3(q,i)
term(146) = term(146) + wm_interm_155_pt3(p,i) * wm_interm_57_pt3(q,i)
term(147) = term(147) + wm_interm_156_pt3(p,i) * wm_interm_57_pt3(q,i)
term(148) = term(148) + wm_interm_159_pt3(p,i) * wm_interm_57_pt3(q,i)
term(149) = term(149) + wm_interm_160_pt3(p,i) * wm_interm_57_pt3(q,i)
term(150) = term(150) + wm_interm_159_pt3(p,i) * wm_interm_51_pt3(q,i)
term(151) = term(151) + wm_interm_160_pt3(p,i) * wm_interm_51_pt3(q,i)
term(152) = term(152) + wm_interm_155_pt3(p,i) * wm_interm_51_pt3(q,i)
term(153) = term(153) + wm_interm_156_pt3(p,i) * wm_interm_51_pt3(q,i)
end do 

term(102) = term(102) * (1.9999999999999998d+0) 
term(103) = term(103) * (-3.9999999999999996d+0) 
term(104) = term(104) * (-1.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (2.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-4.0d+0) 
term(112) = term(112) * (-3.9999999999999996d+0) 
term(113) = term(113) * (7.999999999999999d+0) 
term(114) = term(114) * (8.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (4.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-2.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-2.0d+0) 
term(124) = term(124) * (1.9999999999999998d+0) 
term(125) = term(125) * (-3.9999999999999996d+0) 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (4.0d+0) 
term(128) = term(128) * (-3.9999999999999996d+0) 
term(129) = term(129) * (7.999999999999999d+0) 
term(130) = term(130) * (3.9999999999999996d+0) 
term(131) = term(131) * (-7.999999999999999d+0) 
term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * (4.0d+0) 
term(134) = term(134) * (1.9999999999999998d+0) 
term(135) = term(135) * (-3.9999999999999996d+0) 
term(136) = term(136) * (-3.9999999999999996d+0) 
term(137) = term(137) * (7.999999999999999d+0) 
term(138) = term(138) * (7.999999999999999d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * (-3.9999999999999996d+0) 
term(141) = term(141) * (4.0d+0) 
term(142) = term(142) * (-3.9999999999999996d+0) 
term(143) = term(143) * (4.0d+0) 
term(144) = term(144) * (1.9999999999999998d+0) 
term(145) = term(145) * (-2.0d+0) 
term(146) = term(146) * (2.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (-1.9999999999999998d+0) 
term(149) = term(149) * (3.9999999999999996d+0) 
term(150) = term(150) * (3.9999999999999996d+0) 
term(151) = term(151) * (-7.999999999999999d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(154) = term(154) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_9_pt3(b,k)
term(155) = term(155) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_10_pt3(b,k)
term(156) = term(156) + t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,j,i) * wm_interm_130_pt3(a,k)
term(157) = term(157) + t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,j,i) * wm_interm_131_pt3(a,k)
term(158) = term(158) + t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,k,i) * wm_interm_130_pt3(a,j)
term(159) = term(159) + t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,k,i) * wm_interm_131_pt3(a,j)
term(160) = term(160) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_46_pt3(b,k)
term(161) = term(161) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_46_pt3(b,j)
term(162) = term(162) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_49_pt3(b,k)
term(163) = term(163) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_49_pt3(b,j)
end do 
end do 
end do 
end do 
end do 

term(154) = term(154) * (4.0d+0) 
term(155) = term(155) * (-2.0d+0) 
term(156) = term(156) * (4.0d+0) 
term(157) = term(157) * (-8.0d+0) 
term(158) = term(158) * (-1.9999999999999998d+0) 
term(159) = term(159) * (3.9999999999999996d+0) 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (4.0d+0) 
term(163) = term(163) * (-7.999999999999999d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(164) = term(164) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,l,k) * wm_interm_5_pt3(b,j,l,k)
term(165) = term(165) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,l,k) * wm_interm_5_pt3(b,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(164) = term(164) * (-2.0d+0) 
term(165) = term(165) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(166) = term(166) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(166) = term(166) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(167) = term(167) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,i,l,j) * wm_interm_44_pt3(b,k,l,j)
term(168) = term(168) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,i,l,j) * wm_interm_44_pt3(b,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(169) = term(169) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_pt3(b,j,k,l)
term(170) = term(170) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_pt3(b,j,l,k)
term(171) = term(171) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_44_pt3(b,j,k,l)
term(172) = term(172) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,j,i) * wm_interm_146_pt3(b,l,i,k)
term(173) = term(173) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,j,i) * wm_interm_143_pt3(b,l,i,k)
term(174) = term(174) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,j,i) * wm_interm_146_pt3(b,i,l,k)
term(175) = term(175) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,j,i) * wm_interm_143_pt3(b,i,l,k)
term(176) = term(176) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_44_pt3(b,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(169) = term(169) * (4.0d+0) 
term(170) = term(170) * (-8.0d+0) 
term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (-1.0d+0) 
term(173) = term(173) * (2.0d+0) 
term(174) = term(174) * (2.0d+0) 
term(175) = term(175) * (-1.0d+0) 
term(176) = term(176) * (-8.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(177) = term(177) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_pt3(b,i,l,k)
term(178) = term(178) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_pt3(b,j,l,k)
term(179) = term(179) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_pt3(b,j,k,l)
term(180) = term(180) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(177) = term(177) * (-1.9999999999999998d+0) 
term(178) = term(178) * (3.9999999999999996d+0) 
term(179) = term(179) * (-1.9999999999999998d+0) 
term(180) = term(180) * (3.9999999999999996d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(181) = term(181) + t3(nocc, nactive, a,b,p,k,l,j) * t2(b,q,j,i) * wm_interm_128_pt3(a,i,k,l)
term(182) = term(182) + t3(nocc, nactive, a,b,p,k,l,j) * t2(b,q,k,i) * wm_interm_128_pt3(a,i,j,l)
term(183) = term(183) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,k,l,j) * wm_interm_44_pt3(b,i,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * (3.9999999999999996d+0) 
term(183) = term(183) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(184) = term(184) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_89_pt3(a,j)
term(185) = term(185) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_90_pt3(a,j)
term(186) = term(186) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_91_pt3(a,j)
term(187) = term(187) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_92_pt3(a,j)
term(188) = term(188) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_93_pt3(a,j)
term(189) = term(189) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_94_pt3(a,j)
term(190) = term(190) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,j)
term(191) = term(191) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_37_pt3(a,j)
term(192) = term(192) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,j)
term(193) = term(193) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_39_pt3(a,j)
term(194) = term(194) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_108_pt3(a,j)
term(195) = term(195) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_113_pt3(a,j)
term(196) = term(196) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_115_pt3(a,j)
term(197) = term(197) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_116_pt3(a,j)
term(198) = term(198) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_20_pt3(a,i)
term(199) = term(199) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_21_pt3(a,i)
term(200) = term(200) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_20_pt3(a,j)
term(201) = term(201) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_21_pt3(a,j)
term(202) = term(202) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_22_pt3(a,i)
term(203) = term(203) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_22_pt3(a,j)
term(204) = term(204) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_23_pt3(a,i)
term(205) = term(205) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_23_pt3(a,j)
term(206) = term(206) + s2(a,p,j,i) * t1(q,i) * wm_interm_147_pt3(a,j)
term(207) = term(207) + s2(a,p,j,i) * t1(q,i) * wm_interm_148_pt3(a,j)
term(208) = term(208) + s2(a,p,j,i) * t1(q,i) * wm_interm_149_pt3(a,j)
term(209) = term(209) + s2(a,p,j,i) * t1(q,i) * wm_interm_150_pt3(a,j)
term(210) = term(210) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_66_pt3(a,i)
term(211) = term(211) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_66_pt3(a,j)
term(212) = term(212) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_68_pt3(a,i)
term(213) = term(213) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_68_pt3(a,j)
term(214) = term(214) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_69_pt3(a,i)
term(215) = term(215) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_69_pt3(a,j)
term(216) = term(216) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_70_pt3(a,i)
term(217) = term(217) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_70_pt3(a,j)
term(218) = term(218) + s1(p,i) * t2(a,q,j,i) * wm_interm_159_pt3(a,j)
term(219) = term(219) + s1(p,i) * t2(a,q,j,i) * wm_interm_160_pt3(a,j)
term(220) = term(220) + s1(p,i) * t2(a,q,j,i) * wm_interm_155_pt3(a,j)
term(221) = term(221) + s1(p,i) * t2(a,q,j,i) * wm_interm_156_pt3(a,j)
end do 
end do 
end do 

term(184) = term(184) * (2.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (2.0d+0) 
term(187) = term(187) * (-4.0d+0) 
term(188) = term(188) * (-3.9999999999999996d+0) 
term(189) = term(189) * (7.999999999999999d+0) 
term(190) = term(190) * (-3.9999999999999996d+0) 
term(191) = term(191) * (7.999999999999999d+0) 
term(192) = term(192) * (4.0d+0) 
term(193) = term(193) * (-8.0d+0) 
term(194) = term(194) * (8.0d+0) 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * (4.0d+0) 
term(197) = term(197) * (-8.0d+0) 
term(198) = term(198) * (-4.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (8.0d+0) 
term(201) = term(201) * (-8.0d+0) 
term(202) = term(202) * (2.0d+0) 
term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * (4.0d+0) 
term(206) = term(206) * (3.9999999999999996d+0) 
term(207) = term(207) * (-7.999999999999999d+0) 
term(208) = term(208) * (-3.9999999999999996d+0) 
term(209) = term(209) * (7.999999999999999d+0) 
term(210) = term(210) * (3.9999999999999996d+0) 
term(211) = term(211) * (-7.999999999999999d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (8.0d+0) 
term(214) = term(214) * (-1.9999999999999998d+0) 
term(215) = term(215) * (3.9999999999999996d+0) 
term(216) = term(216) * (2.0d+0) 
term(217) = term(217) * (-4.0d+0) 
term(218) = term(218) * (3.9999999999999996d+0) 
term(219) = term(219) * (-7.999999999999999d+0) 
term(220) = term(220) * (-4.0d+0) 
term(221) = term(221) * (8.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(222) = term(222) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_5_pt3(b,i,k,l)
term(223) = term(223) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_44_pt3(b,i,k,l)
term(224) = term(224) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,l,i,j) * wm_interm_44_pt3(b,k,l,j)
term(225) = term(225) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_44_pt3(b,i,l,j)
term(226) = term(226) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_44_pt3(b,i,j,l)
term(227) = term(227) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,l,i,j) * wm_interm_44_pt3(b,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * (-2.0d+0) 
term(224) = term(224) * (4.0d+0) 
term(225) = term(225) * (-2.0d+0) 
term(226) = term(226) * (4.0d+0) 
term(227) = term(227) * (-2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(228) = term(228) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_5_pt3(b,i,l,k)
term(229) = term(229) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_44_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(228) = term(228) * (4.0d+0) 
term(229) = term(229) * (4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(230) = term(230) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_146_pt3(b,l,k,j)
term(231) = term(231) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_146_pt3(b,l,k,j)
term(232) = term(232) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_146_pt3(b,l,i,j)
term(233) = term(233) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_143_pt3(b,l,i,j)
term(234) = term(234) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_143_pt3(b,l,k,j)
term(235) = term(235) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_143_pt3(b,l,k,j)
term(236) = term(236) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_146_pt3(b,i,l,j)
term(237) = term(237) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_146_pt3(b,k,l,j)
term(238) = term(238) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_146_pt3(b,k,l,j)
term(239) = term(239) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_143_pt3(b,k,l,j)
term(240) = term(240) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_143_pt3(b,k,l,j)
term(241) = term(241) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_143_pt3(b,i,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(230) = term(230) * (-0.9999999999999999d+0) 
term(231) = term(231) * (2.0d+0) 
term(232) = term(232) * (2.0d+0) 
term(233) = term(233) * (-1.0d+0) 
term(234) = term(234) * (1.9999999999999998d+0) 
term(235) = term(235) * (-4.0d+0) 
term(236) = term(236) * (-1.0d+0) 
term(237) = term(237) * (1.9999999999999998d+0) 
term(238) = term(238) * (-4.0d+0) 
term(239) = term(239) * (-0.9999999999999999d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(242) = term(242) + t3(nocc, nactive, a,b,p,k,l,j) * t2(b,q,j,i) * wm_interm_128_pt3(a,i,l,k)
term(243) = term(243) + t3(nocc, nactive, a,b,p,k,l,j) * t2(b,q,k,i) * wm_interm_128_pt3(a,i,l,j)
term(244) = term(244) + t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,l,i) * wm_interm_128_pt3(a,l,k,j)
term(245) = term(245) + t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,l,i) * wm_interm_128_pt3(a,l,j,k)
term(246) = term(246) + t3(nocc, nactive, a,b,p,k,l,j) * t2(b,q,l,i) * wm_interm_128_pt3(a,i,k,j)
term(247) = term(247) + t3(nocc, nactive, a,b,p,k,l,j) * t2(b,q,l,i) * wm_interm_128_pt3(a,i,j,k)
term(248) = term(248) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,k,l,j) * wm_interm_44_pt3(b,i,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(242) = term(242) * (4.0d+0) 
term(243) = term(243) * (-1.9999999999999998d+0) 
term(244) = term(244) * (-2.0d+0) 
term(245) = term(245) * (4.0d+0) 
term(246) = term(246) * (4.0d+0) 
term(247) = term(247) * (-8.0d+0) 
term(248) = term(248) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(249) = term(249) + t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,l,i) * wm_interm_128_pt3(a,l,j,k)
term(250) = term(250) + t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,l,i) * wm_interm_128_pt3(a,l,k,j)
term(251) = term(251) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,l,k,j)
term(252) = term(252) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(249) = term(249) * (-1.9999999999999998d+0) 
term(250) = term(250) * (3.9999999999999996d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(253) = term(253) + r1(vrdav_Rl, p,i) * t2(a,q,k,j) * wm_interm_88_pt3(a,i,j,k)
term(254) = term(254) + r1(vrdav_Rl, p,i) * t2(a,q,k,j) * wm_interm_84_pt3(a,i,j,k)
term(255) = term(255) + r1(vrdav_Rl, p,i) * t2(a,q,k,j) * wm_interm_85_pt3(a,i,j,k)
term(256) = term(256) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_32_pt3(a,i,j,k)
term(257) = term(257) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,i,j,k)
term(258) = term(258) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_106_pt3(a,i,j,k)
term(259) = term(259) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_107_pt3(a,i,j,k)
term(260) = term(260) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_114_pt3(a,i,j,k)
term(261) = term(261) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_40_pt3(a,i,j,k)
term(262) = term(262) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_38_pt3(a,i,j,k)
term(263) = term(263) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_117_pt3(a,i,j,k)
term(264) = term(264) + s2(a,p,k,j) * t1(q,i) * wm_interm_141_pt3(a,i,j,k)
term(265) = term(265) + s2(a,p,k,j) * t1(q,i) * wm_interm_137_pt3(a,i,j,k)
term(266) = term(266) + s2(a,p,k,j) * t1(q,i) * wm_interm_138_pt3(a,i,j,k)
term(267) = term(267) + s2(a,p,k,j) * t1(q,i) * wm_interm_136_pt3(a,i,j,k)
end do 
end do 
end do 
end do 

term(253) = term(253) * (4.0d+0) 
term(254) = term(254) * (4.0d+0) 
term(255) = term(255) * (-8.0d+0) 
term(256) = term(256) * (-2.0d+0) 
term(257) = term(257) * (4.0d+0) 
term(258) = term(258) * (4.0d+0) 
term(259) = term(259) * (-8.0d+0) 
term(260) = term(260) * (2.0d+0) 
term(261) = term(261) * (-1.0d+0) 
term(262) = term(262) * (2.0d+0) 
term(263) = term(263) * (-1.0d+0) 
term(264) = term(264) * (1.9999999999999998d+0) 
term(265) = term(265) * (3.9999999999999996d+0) 
term(266) = term(266) * (-7.999999999999999d+0) 
term(267) = term(267) * (-0.9999999999999999d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(268) = term(268) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,l,i) * wm_interm_128_pt3(a,l,j,k)
term(269) = term(269) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,l,i) * wm_interm_128_pt3(a,l,k,j)
term(270) = term(270) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,j,l,i) * wm_interm_146_pt3(b,i,l,k)
term(271) = term(271) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,j,l,i) * wm_interm_146_pt3(b,l,i,k)
term(272) = term(272) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,j,l,i) * wm_interm_143_pt3(b,l,i,k)
term(273) = term(273) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,j,l,i) * wm_interm_143_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(268) = term(268) * (4.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (-1.0d+0) 
term(271) = term(271) * (2.0d+0) 
term(272) = term(272) * (-1.0d+0) 
term(273) = term(273) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(274) = term(274) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,k,l,i) * wm_interm_146_pt3(b,i,l,j)
term(275) = term(275) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,k,l,i) * wm_interm_146_pt3(b,l,i,j)
term(276) = term(276) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,k,l,i) * wm_interm_143_pt3(b,l,i,j)
term(277) = term(277) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,k,l,i) * wm_interm_143_pt3(b,i,l,j)
term(278) = term(278) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,l,j,k)
term(279) = term(279) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(274) = term(274) * (2.0d+0) 
term(275) = term(275) * (-4.0d+0) 
term(276) = term(276) * (2.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (-4.0d+0) 
term(279) = term(279) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(280) = term(280) + r1(vrdav_Rl, p,i) * t2(a,q,k,j) * wm_interm_84_pt3(a,i,k,j)
term(281) = term(281) + r1(vrdav_Rl, p,i) * t2(a,q,k,j) * wm_interm_85_pt3(a,i,k,j)
term(282) = term(282) + r1(vrdav_Rl, p,i) * t2(a,q,k,j) * wm_interm_88_pt3(a,i,k,j)
term(283) = term(283) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_32_pt3(a,i,k,j)
term(284) = term(284) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,i,k,j)
term(285) = term(285) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_106_pt3(a,i,k,j)
term(286) = term(286) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_107_pt3(a,i,k,j)
term(287) = term(287) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_114_pt3(a,i,k,j)
term(288) = term(288) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_38_pt3(a,i,k,j)
term(289) = term(289) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_40_pt3(a,i,k,j)
term(290) = term(290) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_117_pt3(a,i,k,j)
term(291) = term(291) + s2(a,p,k,j) * t1(q,i) * wm_interm_137_pt3(a,i,k,j)
term(292) = term(292) + s2(a,p,k,j) * t1(q,i) * wm_interm_138_pt3(a,i,k,j)
term(293) = term(293) + s2(a,p,k,j) * t1(q,i) * wm_interm_141_pt3(a,i,k,j)
term(294) = term(294) + s2(a,p,k,j) * t1(q,i) * wm_interm_136_pt3(a,i,k,j)
end do 
end do 
end do 
end do 

term(280) = term(280) * (-2.0d+0) 
term(281) = term(281) * (4.0d+0) 
term(282) = term(282) * (-2.0d+0) 
term(283) = term(283) * (4.0d+0) 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (-2.0d+0) 
term(286) = term(286) * (4.0d+0) 
term(287) = term(287) * (-1.0d+0) 
term(288) = term(288) * (-1.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (2.0d+0) 
term(291) = term(291) * (-1.9999999999999998d+0) 
term(292) = term(292) * (3.9999999999999996d+0) 
term(293) = term(293) * (-0.9999999999999999d+0) 
term(294) = term(294) * (1.9999999999999998d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(295) = term(295) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_9_pt3(b,k)
term(296) = term(296) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_10_pt3(b,k)
term(297) = term(297) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,k) * wm_interm_133_pt3(a,j)
term(298) = term(298) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,k) * wm_interm_133_pt3(a,i)
term(299) = term(299) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,i) * wm_interm_133_pt3(a,k)
term(300) = term(300) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,i) * wm_interm_133_pt3(a,j)
term(301) = term(301) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,k) * wm_interm_134_pt3(a,j)
term(302) = term(302) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,k) * wm_interm_134_pt3(a,i)
term(303) = term(303) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,i) * wm_interm_134_pt3(a,k)
term(304) = term(304) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,i) * wm_interm_134_pt3(a,j)
term(305) = term(305) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_144_pt3(b,k)
term(306) = term(306) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_145_pt3(b,k)
term(307) = term(307) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_145_pt3(b,i)
term(308) = term(308) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_144_pt3(b,i)
term(309) = term(309) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_46_pt3(b,k)
term(310) = term(310) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_144_pt3(b,k)
term(311) = term(311) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_145_pt3(b,k)
term(312) = term(312) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_49_pt3(b,k)
term(313) = term(313) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,p,i) * wm_interm_51_pt3(b,k)
term(314) = term(314) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_51_pt3(b,j)
term(315) = term(315) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_51_pt3(a,k)
term(316) = term(316) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,p,i) * wm_interm_51_pt3(a,j)
term(317) = term(317) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_57_pt3(a,k)
term(318) = term(318) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,p,i) * wm_interm_57_pt3(b,k)
term(319) = term(319) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_57_pt3(b,j)
term(320) = term(320) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,p,i) * wm_interm_57_pt3(a,j)
term(321) = term(321) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,k,i) * wm_interm_151_pt3(b,j)
term(322) = term(322) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,j,i) * wm_interm_151_pt3(b,k)
term(323) = term(323) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,j,i) * wm_interm_151_pt3(a,k)
term(324) = term(324) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,k,i) * wm_interm_151_pt3(a,j)
term(325) = term(325) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,k,i) * wm_interm_152_pt3(b,j)
term(326) = term(326) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,j,i) * wm_interm_152_pt3(b,k)
term(327) = term(327) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,j,i) * wm_interm_152_pt3(a,k)
term(328) = term(328) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,k,i) * wm_interm_152_pt3(a,j)
end do 
end do 
end do 
end do 
end do 

term(295) = term(295) * (-8.0d+0) 
term(296) = term(296) * (4.0d+0) 
term(297) = term(297) * (-8.0d+0) 
term(298) = term(298) * (3.9999999999999996d+0) 
term(299) = term(299) * (-7.999999999999999d+0) 
term(300) = term(300) * (16.0d+0) 
term(301) = term(301) * (4.0d+0) 
term(302) = term(302) * (-1.9999999999999998d+0) 
term(303) = term(303) * (3.9999999999999996d+0) 
term(304) = term(304) * (-8.0d+0) 
term(305) = term(305) * (3.9999999999999996d+0) 
term(306) = term(306) * (-1.9999999999999998d+0) 
term(307) = term(307) * (4.0d+0) 
term(308) = term(308) * (-8.0d+0) 
term(309) = term(309) * (4.0d+0) 
term(310) = term(310) * (-8.0d+0) 
term(311) = term(311) * (4.0d+0) 
term(312) = term(312) * (-8.0d+0) 
term(313) = term(313) * (8.0d+0) 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * (-4.0d+0) 
term(316) = term(316) * (8.0d+0) 
term(317) = term(317) * (2.0d+0) 
term(318) = term(318) * (-4.0d+0) 
term(319) = term(319) * (2.0d+0) 
term(320) = term(320) * (-4.0d+0) 
term(321) = term(321) * (4.0d+0) 
term(322) = term(322) * (-8.0d+0) 
term(323) = term(323) * (4.0d+0) 
term(324) = term(324) * (-8.0d+0) 
term(325) = term(325) * (-2.0d+0) 
term(326) = term(326) * (4.0d+0) 
term(327) = term(327) * (-2.0d+0) 
term(328) = term(328) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(329) = term(329) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,j,p,i) * wm_interm_11_pt3(b,l,i,k)
term(330) = term(330) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,j,p,i) * wm_interm_11_pt3(b,i,l,k)
term(331) = term(331) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,k,p,i) * wm_interm_11_pt3(b,i,l,j)
term(332) = term(332) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,k,p,i) * wm_interm_11_pt3(b,l,i,j)
term(333) = term(333) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,l,k,j)
term(334) = term(334) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(329) = term(329) * (-1.9999999999999998d+0) 
term(330) = term(330) * (3.9999999999999996d+0) 
term(331) = term(331) * (-1.9999999999999998d+0) 
term(332) = term(332) * (3.9999999999999996d+0) 
term(333) = term(333) * (-4.0d+0) 
term(334) = term(334) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(335) = term(335) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,j,b,l,p,i) * wm_interm_11_pt3(b,l,i,k)
term(336) = term(336) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,j,b,l,p,i) * wm_interm_11_pt3(b,i,l,k)
term(337) = term(337) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_139_pt3(a,i,k,l)
term(338) = term(338) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_139_pt3(a,k,i,l)
term(339) = term(339) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_142_pt3(a,i,k,l)
term(340) = term(340) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_142_pt3(a,k,i,l)
term(341) = term(341) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,l,j) * wm_interm_166_pt3(b,i,k,l)
term(342) = term(342) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,l,j) * wm_interm_166_pt3(b,k,i,l)
term(343) = term(343) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,l,j) * wm_interm_166_pt3(a,k,i,l)
term(344) = term(344) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,l,j) * wm_interm_166_pt3(a,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(335) = term(335) * (4.0d+0) 
term(336) = term(336) * (-2.0d+0) 
term(337) = term(337) * (1.9999999999999998d+0) 
term(338) = term(338) * (-0.9999999999999999d+0) 
term(339) = term(339) * (-0.9999999999999999d+0) 
term(340) = term(340) * (1.9999999999999998d+0) 
term(341) = term(341) * (-1.0d+0) 
term(342) = term(342) * (2.0d+0) 
term(343) = term(343) * (-1.0d+0) 
term(344) = term(344) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(345) = term(345) + r2(vrdav_Rl, a,j,q,i) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_11_pt3(b,k,l,j)
term(346) = term(346) + r2(vrdav_Rl, a,j,q,i) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_11_pt3(b,l,k,j)
term(347) = term(347) + r2(vrdav_Rl, a,i,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_11_pt3(b,k,l,j)
term(348) = term(348) + r2(vrdav_Rl, a,i,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_11_pt3(b,l,k,j)
term(349) = term(349) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_11_pt3(b,i,l,j)
term(350) = term(350) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_11_pt3(b,l,i,j)
term(351) = term(351) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(a,q,l,j) * wm_interm_166_pt3(b,k,i,l)
term(352) = term(352) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(a,q,l,j) * wm_interm_166_pt3(b,i,k,l)
term(353) = term(353) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(b,q,l,j) * wm_interm_166_pt3(a,i,k,l)
term(354) = term(354) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(b,q,l,j) * wm_interm_166_pt3(a,k,i,l)
term(355) = term(355) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(a,q,i,j) * wm_interm_166_pt3(b,k,l,j)
term(356) = term(356) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(a,q,i,j) * wm_interm_166_pt3(b,l,k,j)
term(357) = term(357) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(a,q,l,j) * wm_interm_166_pt3(b,i,k,j)
term(358) = term(358) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(a,q,l,j) * wm_interm_166_pt3(b,k,i,j)
term(359) = term(359) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(a,q,k,j) * wm_interm_166_pt3(b,i,l,j)
term(360) = term(360) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(a,q,k,j) * wm_interm_166_pt3(b,l,i,j)
term(361) = term(361) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(b,q,i,j) * wm_interm_166_pt3(a,l,k,j)
term(362) = term(362) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(b,q,i,j) * wm_interm_166_pt3(a,k,l,j)
term(363) = term(363) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(b,q,k,j) * wm_interm_166_pt3(a,i,l,j)
term(364) = term(364) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(b,q,k,j) * wm_interm_166_pt3(a,l,i,j)
term(365) = term(365) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(b,q,l,j) * wm_interm_166_pt3(a,i,k,j)
term(366) = term(366) + r3(vrdav_Rl, a,k,b,l,p,i) * t2(b,q,l,j) * wm_interm_166_pt3(a,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(345) = term(345) * (3.9999999999999996d+0) 
term(346) = term(346) * (-7.999999999999999d+0) 
term(347) = term(347) * (-1.9999999999999998d+0) 
term(348) = term(348) * (3.9999999999999996d+0) 
term(349) = term(349) * (4.0d+0) 
term(350) = term(350) * (-8.0d+0) 
term(351) = term(351) * (-1.0d+0) 
term(352) = term(352) * (2.0d+0) 
term(353) = term(353) * (-1.0d+0) 
term(354) = term(354) * (2.0d+0) 
term(355) = term(355) * (-1.0d+0) 
term(356) = term(356) * (2.0d+0) 
term(357) = term(357) * (-1.0d+0) 
term(358) = term(358) * (2.0d+0) 
term(359) = term(359) * (2.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (-1.0d+0) 
term(362) = term(362) * (2.0d+0) 
term(363) = term(363) * (-1.0d+0) 
term(364) = term(364) * (2.0d+0) 
term(365) = term(365) * (2.0d+0) 
term(366) = term(366) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(367) = term(367) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_1_pt3(a,i,k,j)
term(368) = term(368) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_1_pt3(a,k,i,j)
term(369) = term(369) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_2_pt3(a,k,i,j)
term(370) = term(370) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_2_pt3(a,i,k,j)
term(371) = term(371) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_3_pt3(a,i,k,j)
term(372) = term(372) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_3_pt3(a,k,i,j)
term(373) = term(373) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_13_pt3(a,i,k,j)
term(374) = term(374) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_13_pt3(a,k,i,j)
term(375) = term(375) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_15_pt3(a,i,k,j)
term(376) = term(376) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_14_pt3(a,i,k,j)
term(377) = term(377) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_16_pt3(a,i,k,j)
term(378) = term(378) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_14_pt3(a,k,i,j)
term(379) = term(379) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_16_pt3(a,k,i,j)
term(380) = term(380) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * wm_interm_15_pt3(a,k,i,j)
term(381) = term(381) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_58_pt3(a,i,k,j)
term(382) = term(382) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_58_pt3(a,k,i,j)
term(383) = term(383) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_61_pt3(a,k,i,j)
term(384) = term(384) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_62_pt3(a,i,k,j)
term(385) = term(385) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_61_pt3(a,i,k,j)
term(386) = term(386) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_62_pt3(a,k,i,j)
term(387) = term(387) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_63_pt3(a,i,k,j)
term(388) = term(388) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_63_pt3(a,k,i,j)
term(389) = term(389) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_64_pt3(a,k,i,j)
term(390) = term(390) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_64_pt3(a,i,k,j)
term(391) = term(391) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_65_pt3(a,k,i,j)
term(392) = term(392) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * wm_interm_65_pt3(a,i,k,j)
term(393) = term(393) + s1(p,i) * t2(a,q,k,j) * wm_interm_161_pt3(a,j,k,i)
term(394) = term(394) + s1(p,i) * t2(a,q,k,j) * wm_interm_162_pt3(a,j,k,i)
term(395) = term(395) + s1(p,i) * t2(a,q,k,j) * wm_interm_158_pt3(a,k,j,i)
term(396) = term(396) + s1(p,i) * t2(a,q,k,j) * wm_interm_157_pt3(a,k,j,i)
term(397) = term(397) + s1(p,i) * t2(a,q,k,j) * wm_interm_157_pt3(a,j,k,i)
term(398) = term(398) + s1(p,i) * t2(a,q,k,j) * wm_interm_158_pt3(a,j,k,i)
term(399) = term(399) + s1(p,i) * t2(a,q,k,j) * wm_interm_161_pt3(a,k,j,i)
term(400) = term(400) + s1(p,i) * t2(a,q,k,j) * wm_interm_162_pt3(a,k,j,i)
term(401) = term(401) + s1(p,i) * t2(a,q,k,j) * wm_interm_153_pt3(a,k,j,i)
term(402) = term(402) + s1(p,i) * t2(a,q,k,j) * wm_interm_154_pt3(a,k,j,i)
term(403) = term(403) + s1(p,i) * t2(a,q,k,j) * wm_interm_153_pt3(a,j,k,i)
term(404) = term(404) + s1(p,i) * t2(a,q,k,j) * wm_interm_154_pt3(a,j,k,i)
end do 
end do 
end do 
end do 

term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-2.0d+0) 
term(370) = term(370) * (4.0d+0) 
term(371) = term(371) * (4.0d+0) 
term(372) = term(372) * (-8.0d+0) 
term(373) = term(373) * (-1.9999999999999998d+0) 
term(374) = term(374) * (3.9999999999999996d+0) 
term(375) = term(375) * (-0.9999999999999999d+0) 
term(376) = term(376) * (3.9999999999999996d+0) 
term(377) = term(377) * (1.9999999999999998d+0) 
term(378) = term(378) * (-7.999999999999999d+0) 
term(379) = term(379) * (-0.9999999999999999d+0) 
term(380) = term(380) * (1.9999999999999998d+0) 
term(381) = term(381) * (-1.0d+0) 
term(382) = term(382) * (2.0d+0) 
term(383) = term(383) * (-1.0d+0) 
term(384) = term(384) * (2.0d+0) 
term(385) = term(385) * (2.0d+0) 
term(386) = term(386) * (-4.0d+0) 
term(387) = term(387) * (-1.0d+0) 
term(388) = term(388) * (2.0d+0) 
term(389) = term(389) * (2.0d+0) 
term(390) = term(390) * (-4.0d+0) 
term(391) = term(391) * (-1.0d+0) 
term(392) = term(392) * (2.0d+0) 
term(393) = term(393) * (-1.0d+0) 
term(394) = term(394) * (2.0d+0) 
term(395) = term(395) * (-1.0d+0) 
term(396) = term(396) * (2.0d+0) 
term(397) = term(397) * (-1.0d+0) 
term(398) = term(398) * (2.0d+0) 
term(399) = term(399) * (2.0d+0) 
term(400) = term(400) * (-4.0d+0) 
term(401) = term(401) * (-1.0d+0) 
term(402) = term(402) * (2.0d+0) 
term(403) = term(403) * (2.0d+0) 
term(404) = term(404) * (-4.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(405) = term(405) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56_pt3(a,i,j,l)
term(406) = term(406) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56_pt3(a,i,l,j)
term(407) = term(407) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_56_pt3(a,i,k,l)
term(408) = term(408) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,i,k,j)
term(409) = term(409) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56_pt3(b,i,j,l)
term(410) = term(410) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_56_pt3(b,i,k,l)
term(411) = term(411) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,i,k,j)
term(412) = term(412) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56_pt3(b,i,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(405) = term(405) * (-1.0d+0) 
term(406) = term(406) * (2.0d+0) 
term(407) = term(407) * (2.0d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (-1.0d+0) 
term(411) = term(411) * (2.0d+0) 
term(412) = term(412) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(413) = term(413) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,l,j,k)
term(414) = term(414) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,l,k,j)
term(415) = term(415) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,l,j,k)
term(416) = term(416) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,l,k,j)
term(417) = term(417) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,l,j,k)
term(418) = term(418) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (-1.0d+0) 
term(415) = term(415) * (-1.0d+0) 
term(416) = term(416) * (2.0d+0) 
term(417) = term(417) * (-1.0d+0) 
term(418) = term(418) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(419) = term(419) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_139_pt3(a,i,k,l)
term(420) = term(420) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_139_pt3(a,i,j,l)
term(421) = term(421) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_139_pt3(a,i,j,l)
term(422) = term(422) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_139_pt3(a,k,j,l)
term(423) = term(423) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_139_pt3(a,j,k,l)
term(424) = term(424) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_139_pt3(a,k,i,l)
term(425) = term(425) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_139_pt3(a,j,i,l)
term(426) = term(426) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_139_pt3(a,j,i,l)
term(427) = term(427) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_139_pt3(a,k,j,l)
term(428) = term(428) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_139_pt3(a,j,k,l)
term(429) = term(429) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_142_pt3(a,k,i,l)
term(430) = term(430) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_142_pt3(a,j,i,l)
term(431) = term(431) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_142_pt3(a,j,k,l)
term(432) = term(432) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_142_pt3(a,k,j,l)
term(433) = term(433) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_142_pt3(a,i,k,l)
term(434) = term(434) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_142_pt3(a,i,j,l)
term(435) = term(435) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_142_pt3(a,i,j,l)
term(436) = term(436) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_142_pt3(a,j,i,l)
term(437) = term(437) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_142_pt3(a,j,k,l)
term(438) = term(438) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_142_pt3(a,k,j,l)
term(439) = term(439) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,l,i) * wm_interm_166_pt3(b,j,k,l)
term(440) = term(440) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,l,i) * wm_interm_166_pt3(b,k,j,l)
term(441) = term(441) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,l,i) * wm_interm_166_pt3(a,k,j,l)
term(442) = term(442) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,l,i) * wm_interm_166_pt3(a,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(419) = term(419) * (-0.9999999999999999d+0) 
term(420) = term(420) * (1.9999999999999998d+0) 
term(421) = term(421) * (-0.9999999999999999d+0) 
term(422) = term(422) * (-0.9999999999999999d+0) 
term(423) = term(423) * (1.9999999999999998d+0) 
term(424) = term(424) * (1.9999999999999998d+0) 
term(425) = term(425) * (-3.9999999999999996d+0) 
term(426) = term(426) * (1.9999999999999998d+0) 
term(427) = term(427) * (1.9999999999999998d+0) 
term(428) = term(428) * (-3.9999999999999996d+0) 
term(429) = term(429) * (-0.9999999999999999d+0) 
term(430) = term(430) * (1.9999999999999998d+0) 
term(431) = term(431) * (-0.9999999999999999d+0) 
term(432) = term(432) * (1.9999999999999998d+0) 
term(433) = term(433) * (1.9999999999999998d+0) 
term(434) = term(434) * (-3.9999999999999996d+0) 
term(435) = term(435) * (1.9999999999999998d+0) 
term(436) = term(436) * (-0.9999999999999999d+0) 
term(437) = term(437) * (1.9999999999999998d+0) 
term(438) = term(438) * (-3.9999999999999996d+0) 
term(439) = term(439) * (2.0d+0) 
term(440) = term(440) * (-4.0d+0) 
term(441) = term(441) * (2.0d+0) 
term(442) = term(442) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(443) = term(443) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_9_pt3(b,k)
term(444) = term(444) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_10_pt3(b,k)
term(445) = term(445) + t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,j,i) * wm_interm_130_pt3(a,k)
term(446) = term(446) + t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,j,i) * wm_interm_131_pt3(a,k)
term(447) = term(447) + t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,k,i) * wm_interm_130_pt3(a,j)
term(448) = term(448) + t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,k,i) * wm_interm_131_pt3(a,j)
term(449) = term(449) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_46_pt3(b,k)
term(450) = term(450) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_46_pt3(b,j)
term(451) = term(451) + r2(vrdav_Rl, a,k,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_49_pt3(b,j)
term(452) = term(452) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_49_pt3(b,k)
term(453) = term(453) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_51_pt3(b,k)
term(454) = term(454) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_51_pt3(b,j)
term(455) = term(455) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_51_pt3(a,k)
term(456) = term(456) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_51_pt3(a,j)
term(457) = term(457) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_57_pt3(a,j)
term(458) = term(458) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_57_pt3(b,j)
term(459) = term(459) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_57_pt3(b,k)
term(460) = term(460) + r3(vrdav_Rl, a,k,b,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_57_pt3(a,k)
end do 
end do 
end do 
end do 
end do 

term(443) = term(443) * (-7.999999999999999d+0) 
term(444) = term(444) * (3.9999999999999996d+0) 
term(445) = term(445) * (-1.9999999999999998d+0) 
term(446) = term(446) * (3.9999999999999996d+0) 
term(447) = term(447) * (3.9999999999999996d+0) 
term(448) = term(448) * (-7.999999999999999d+0) 
term(449) = term(449) * (4.0d+0) 
term(450) = term(450) * (-2.0d+0) 
term(451) = term(451) * (3.9999999999999996d+0) 
term(452) = term(452) * (-8.0d+0) 
term(453) = term(453) * (2.0d+0) 
term(454) = term(454) * (-4.0d+0) 
term(455) = term(455) * (-4.0d+0) 
term(456) = term(456) * (2.0d+0) 
term(457) = term(457) * (-1.0d+0) 
term(458) = term(458) * (2.0d+0) 
term(459) = term(459) * (-1.0d+0) 
term(460) = term(460) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(461) = term(461) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,j) * wm_interm_133_pt3(a,k)
term(462) = term(462) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,j) * wm_interm_133_pt3(a,i)
term(463) = term(463) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,j) * wm_interm_134_pt3(a,k)
term(464) = term(464) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,j) * wm_interm_134_pt3(a,i)
term(465) = term(465) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,i,j) * wm_interm_151_pt3(b,k)
term(466) = term(466) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,k,j) * wm_interm_151_pt3(b,i)
term(467) = term(467) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,i,j) * wm_interm_151_pt3(a,k)
term(468) = term(468) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,k,j) * wm_interm_151_pt3(a,i)
term(469) = term(469) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,i,j) * wm_interm_152_pt3(b,k)
term(470) = term(470) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(a,q,k,j) * wm_interm_152_pt3(b,i)
term(471) = term(471) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,i,j) * wm_interm_152_pt3(a,k)
term(472) = term(472) + r3(vrdav_Rl, a,j,b,k,p,i) * t2(b,q,k,j) * wm_interm_152_pt3(a,i)
end do 
end do 
end do 
end do 
end do 

term(461) = term(461) * (3.9999999999999996d+0) 
term(462) = term(462) * (-7.999999999999999d+0) 
term(463) = term(463) * (-1.9999999999999998d+0) 
term(464) = term(464) * (3.9999999999999996d+0) 
term(465) = term(465) * (4.0d+0) 
term(466) = term(466) * (-2.0d+0) 
term(467) = term(467) * (-2.0d+0) 
term(468) = term(468) * (4.0d+0) 
term(469) = term(469) * (-2.0d+0) 
term(472) = term(472) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(473) = term(473) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_51_pt3(b,k)
term(474) = term(474) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_51_pt3(b,j)
term(475) = term(475) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_51_pt3(a,k)
term(476) = term(476) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_51_pt3(a,j)
term(477) = term(477) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_57_pt3(a,k)
term(478) = term(478) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_57_pt3(a,j)
term(479) = term(479) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_57_pt3(b,k)
term(480) = term(480) + r3(vrdav_Rl, a,i,b,k,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_57_pt3(b,j)
end do 
end do 
end do 
end do 
end do 

term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (2.0d+0) 
term(475) = term(475) * (2.0d+0) 
term(476) = term(476) * (-4.0d+0) 
term(477) = term(477) * (-1.0d+0) 
term(478) = term(478) * (2.0d+0) 
term(479) = term(479) * (2.0d+0) 
term(480) = term(480) * (-1.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(481) = term(481) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_9_pt3(b,k)
term(482) = term(482) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_10_pt3(b,k)
term(483) = term(483) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,j,i) * wm_interm_130_pt3(a,k)
term(484) = term(484) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,j,i) * wm_interm_131_pt3(a,k)
term(485) = term(485) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,k,i) * wm_interm_130_pt3(a,j)
term(486) = term(486) + t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,k,i) * wm_interm_131_pt3(a,j)
term(487) = term(487) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_145_pt3(b,i)
term(488) = term(488) + s2(a,p,k,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_144_pt3(b,i)
term(489) = term(489) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_145_pt3(b,k)
term(490) = term(490) + s2(a,p,i,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_144_pt3(b,k)
term(491) = term(491) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_46_pt3(b,k)
term(492) = term(492) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_145_pt3(b,k)
term(493) = term(493) + s2(a,p,j,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_144_pt3(b,k)
term(494) = term(494) + r2(vrdav_Rl, a,j,p,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_49_pt3(b,k)
term(495) = term(495) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(a,q,i,j) * wm_interm_151_pt3(b,k)
term(496) = term(496) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(a,q,k,j) * wm_interm_151_pt3(b,i)
term(497) = term(497) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(b,q,i,j) * wm_interm_151_pt3(a,k)
term(498) = term(498) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(b,q,k,j) * wm_interm_151_pt3(a,i)
term(499) = term(499) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(a,q,i,j) * wm_interm_152_pt3(b,k)
term(500) = term(500) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(a,q,k,j) * wm_interm_152_pt3(b,i)
term(501) = term(501) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(b,q,i,j) * wm_interm_152_pt3(a,k)
term(502) = term(502) + r3(vrdav_Rl, a,k,b,j,p,i) * t2(b,q,k,j) * wm_interm_152_pt3(a,i)
end do 
end do 
end do 
end do 
end do 

term(481) = term(481) * (15.999999999999998d+0) 
term(482) = term(482) * (-7.999999999999999d+0) 
term(483) = term(483) * (3.9999999999999996d+0) 
term(484) = term(484) * (-7.999999999999999d+0) 
term(485) = term(485) * (-8.0d+0) 
term(486) = term(486) * (16.0d+0) 
term(487) = term(487) * (-2.0d+0) 
term(488) = term(488) * (4.0d+0) 
term(489) = term(489) * (3.9999999999999996d+0) 
term(490) = term(490) * (-7.999999999999999d+0) 
term(491) = term(491) * (-8.0d+0) 
term(492) = term(492) * (-8.0d+0) 
term(493) = term(493) * (16.0d+0) 
term(494) = term(494) * (16.0d+0) 
term(495) = term(495) * (-2.0d+0) 
term(496) = term(496) * (4.0d+0) 
term(497) = term(497) * (4.0d+0) 
term(498) = term(498) * (-2.0d+0) 
term(500) = term(500) * (-2.0d+0) 
term(501) = term(501) * (-2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(503) = term(503) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_56_pt3(b,i,l,k)
term(504) = term(504) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, b,l,p,i) * wm_interm_56_pt3(a,i,j,k)
term(505) = term(505) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_56_pt3(b,i,j,k)
term(506) = term(506) + r3(vrdav_Rl, a,k,b,l,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_56_pt3(a,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(503) = term(503) * (2.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(507) = term(507) + r2(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(507) = term(507) * (-8.0d+0) 


    calc_D_vv_wm_cc3_pt3 = zero
    do s = 0, 507
    calc_D_vv_wm_cc3_pt3 = calc_D_vv_wm_cc3_pt3 + term(s)
    end do

    end function calc_D_vv_wm_cc3_pt3
    

        

  end module ss_cc3_pt3
